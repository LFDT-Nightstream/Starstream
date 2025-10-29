use futures_channel::mpsc;
use futures_util::{StreamExt, TryFutureExt, join, sink::SinkExt, stream, stream_select};
use serde::{Deserialize, Serialize};
use starstream_language_server::Server;
use std::future;
use tower_lsp_server::{
    LspService,
    jsonrpc::{Error, Id, Request, Response},
};
use tower_service::Service;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::spawn_local;
use web_sys::{
    DedicatedWorkerGlobalScope, MessageEvent,
    js_sys::{self, Function},
};

// ----------------------------------------------------------------------------
// postMessage and onMessage communication

/// An incoming or outgoing JSON-RPC message.
#[derive(Deserialize, Serialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde(untagged)]
enum Message {
    /// A response message.
    Response(Response),
    /// A request or notification message.
    Request(Request),
}

/// Write output to `postMessage`.
fn output(message: Message) {
    js_sys::global()
        .dyn_into::<DedicatedWorkerGlobalScope>()
        .unwrap()
        .post_message(&serde_wasm_bindgen::to_value(&message).unwrap())
        .unwrap();
}

/// Set `this.onmessage` to give inputs on this channel.
fn set_up_input() -> mpsc::Receiver<Result<Message, serde_wasm_bindgen::Error>> {
    let (tx, rx) = mpsc::channel(0);

    let onmessage: Closure<dyn Fn(MessageEvent)> = Closure::new(move |event: MessageEvent| {
        let message = serde_wasm_bindgen::from_value::<Message>(event.data());
        let mut tx = tx.clone();
        spawn_local(async move {
            tx.send(message).await.unwrap();
        });
    });

    let onmessage = onmessage.into_js_value().dyn_into::<Function>().unwrap();

    js_sys::global()
        .dyn_into::<DedicatedWorkerGlobalScope>()
        .unwrap()
        .set_onmessage(Some(&onmessage));

    rx
}

macro_rules! error {
    ($($rest:tt)*) => {
        web_sys::console::error_1(&JsValue::from_str(&format!($($rest)*)))
    };
}

// ----------------------------------------------------------------------------
// Language server main

const MAX_CONCURRENCY: usize = 4;
const MESSAGE_QUEUE_SIZE: usize = 100;

#[wasm_bindgen(start)]
pub async fn main() {
    let mut input_rx = set_up_input();

    // Based on tower-lsp-server 0.22.1 src/transport.rs
    // but without LanguageServerCodec framing.

    let (mut service, loopback) = Server::new();
    let (client_requests, mut client_responses) = loopback.split();

    let (client_requests, client_abort) = stream::abortable(client_requests);
    let (mut responses_tx, responses_rx) = mpsc::channel(0);
    let (mut server_tasks_tx, server_tasks_rx) = mpsc::channel(MESSAGE_QUEUE_SIZE);

    let process_server_tasks = server_tasks_rx
        .buffer_unordered(MAX_CONCURRENCY)
        .filter_map(future::ready)
        .map(|res| Ok(Message::Response(res)))
        .forward(responses_tx.clone());

    let print_output = stream_select!(responses_rx, client_requests.map(Message::Request))
        .for_each(async |x| output(x));

    let read_input = async {
        while let Some(msg) = input_rx.next().await {
            match msg {
                Ok(Message::Request(req)) => {
                    if let Err(err) = future::poll_fn(|cx| service.poll_ready(cx)).await {
                        // error!("{}", display_sources(err.into().as_ref()));
                        error!("{:?}", err);
                        return;
                    }

                    // per specifications, the server must exit immediately after the exit notification
                    // some clients will not close stdin and thus keep framed_stdin waiting forever
                    // we break early here so that control can be yielded back immediately
                    let will_exit = req.method() == "exit";

                    let fut = service.call(req).unwrap_or_else(|err| {
                        // error!("{}", display_sources(err.into().as_ref()));
                        error!("{:?}", err);
                        None
                    });

                    let _ = server_tasks_tx.send(fut).await;

                    if will_exit {
                        break;
                    }
                }
                Ok(Message::Response(res)) => {
                    if let Err(err) = client_responses.send(res).await {
                        // error!("{}", display_sources(&err));
                        error!("{:?}", err);
                        return;
                    }
                }
                Err(err) => {
                    error!("failed to decode message: {}", err);
                    let res = Response::from_error(Id::Null, Error::parse_error());
                    let _ = responses_tx.send(Message::Response(res)).await;
                }
            }
        }

        server_tasks_tx.disconnect();
        responses_tx.disconnect();
        client_abort.abort();
    };

    let _ = join!(process_server_tasks, print_output, read_input);
}

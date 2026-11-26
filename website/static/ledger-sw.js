// Serves the in-browser ledger of an open ledger terminal page at
// `/ledger-node/`, forwarding each request to that page.

const LEDGER_PATH = "/ledger-node/";
const PAGE_PATH = "/ledger-client";
const PAGE_CHECK_INTERVAL = 1000;

self.addEventListener("install", () => self.skipWaiting());

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if (
    url.origin === self.location.origin &&
    url.pathname.startsWith(LEDGER_PATH)
  ) {
    event.respondWith(forward(event.request));
  }
});

async function forward(request) {
  const windows = await self.clients.matchAll({
    type: "window",
    includeUncontrolled: true,
  });
  const page = windows.find(({ url }) =>
    new URL(url).pathname.startsWith(PAGE_PATH),
  );
  if (!page) {
    return new Response("no ledger terminal page is open", { status: 503 });
  }
  const headers = [];
  request.headers.forEach((value, name) => headers.push([name, value]));
  const body =
    request.method === "GET" || request.method === "HEAD"
      ? undefined
      : await request.arrayBuffer();
  const { port1, port2 } = new MessageChannel();
  return new Promise((resolve) => {
    let acked = false;
    const timer = setInterval(async () => {
      if (!acked) {
        clearInterval(timer);
        resolve(
          new Response("the ledger terminal page did not answer", {
            status: 503,
          }),
        );
      } else if (!(await self.clients.get(page.id))) {
        clearInterval(timer);
        resolve(
          new Response("the ledger terminal page was closed", { status: 503 }),
        );
      }
    }, PAGE_CHECK_INTERVAL);
    port1.onmessage = ({ data }) => {
      if (data.ack) {
        acked = true;
        return;
      }
      clearInterval(timer);
      resolve(
        new Response(data.body.byteLength > 0 ? data.body : null, {
          status: data.status,
          headers: data.headers,
        }),
      );
    };
    page.postMessage(
      {
        type: "ledger-request",
        request: { method: request.method, url: request.url, headers, body },
      },
      body ? [port2, body] : [port2],
    );
  });
}

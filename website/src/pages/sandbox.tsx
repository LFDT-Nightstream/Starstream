import Layout from "@theme/Layout";
import { AnsiHtml } from "fancy-ansi/react";
import {
  ComponentProps,
  Dispatch,
  ReactNode,
  SetStateAction,
  useCallback,
  useEffect,
  useRef,
  useState,
} from "react";
import type {
  SandboxWorkerRequest,
  SandboxWorkerResponse,
} from "../sandbox.worker";
import { useBlobUrl } from "../hooks";

function useSandboxWorker(onResponse: (r: SandboxWorkerResponse) => void): {
  request(r: SandboxWorkerRequest): void;
  restart(): void;
} {
  const worker = useRef<Worker>(null);
  useEffect(() => {
    worker.current = new Worker(new URL("../sandbox.worker", import.meta.url));
    return () => worker.current!.terminate();
  }, []);
  useEffect(() => {
    function onMessage({ data }: { data: SandboxWorkerResponse }) {
      onResponse(data);
    }
    worker.current!.addEventListener("message", onMessage);
    return () => worker.current!.removeEventListener("message", onMessage);
  }, [onResponse]);
  return {
    request(r) {
      worker.current!.postMessage(r);
    },
    restart() {
      worker.current!.terminate();
      worker.current = new Worker(
        new URL("../sandbox.worker", import.meta.url),
      );
    },
  };
}

// Wrapper to load `../editor.tsx` only in the browser.
function Editor(props: ComponentProps<typeof import("../editor").Editor>) {
  const [editorModule, setEditorModule] =
    useState<typeof import("../editor")>();
  useEffect(() => {
    import("../editor").then(setEditorModule);
  }, []);
  return editorModule ? <editorModule.Editor {...props} /> : null;
}

function Tabs(props: {
  current: string;
  setCurrent: Dispatch<SetStateAction<string>>;
  className?: string;
  style?: React.CSSProperties;
  tabs: { key: string; title?: ReactNode; body?: ReactNode }[];
  after?: ReactNode;
  right?: ReactNode;
}) {
  const { current, setCurrent } = props;
  useEffect(() => {
    setCurrent((current) => {
      if (!props.tabs.some((tab) => current === tab.key)) {
        current = props.tabs[0].key;
      }
      return current;
    });
  }, [current, JSON.stringify(props.tabs.map((tab) => tab.key))]);
  return (
    <div
      className={`flex--grow flex--column ${props.className}`}
      style={props.style}
    >
      <div className="sandbox-tabs__list">
        {props.tabs.map((tab) => (
          <button
            key={tab.key}
            type="button"
            onClick={() => setCurrent(tab.key)}
            className={`sandbox-tabs__button ${
              current === tab.key ? "sandbox-tabs__button--current" : ""
            }`}
          >
            {tab.title ?? tab.key}
          </button>
        ))}
        <div className="flex--grow">{props.after}</div>
        <div>{props.right}</div>
      </div>
      {/* Put all tabs in the DOM upfront to avoid unmounting/remounting the Monaco editor constantly. */}
      {props.tabs.map((tab) => (
        <div
          className="sandbox-tabs__area"
          key={tab.key}
          hidden={tab.key !== current}
        >
          {tab.body}
        </div>
      ))}
    </div>
  );
}

export function Sandbox() {
  const [inputTab, setInputTab] = useState("");

  const [outputTab, setOutputTab] = useState("");
  const [busy, setBusy] = useState(false);
  const [wat, setWat] = useState("");

  const [coreWasm, setCoreWasm] = useState<Uint8Array<ArrayBuffer>>();
  const [componentWasm, setComponentWasm] = useState<Uint8Array<ArrayBuffer>>();

  const coreWasmUrl = useBlobUrl(
    coreWasm,
    "starstream_sandbox.core.wasm",
    "application/wasm",
  );
  const componentWasmUrl = useBlobUrl(
    componentWasm,
    "starstream_sandbox.component.wasm",
    "application/wasm",
  );

  const request_id = useRef(0);
  const worker = useSandboxWorker((response) => {
    if (response.request_id !== request_id.current) {
      console.log("discarding response for old request", response);
      return;
    }

    if (response.type == "idle") {
      setBusy(false);
    } else if (response.type == "log") {
      // TODO: Show in UI
      console.log(
        ["", "Error", "Warn", "Info", "Debug", "Trace"][response.level],
        `[${response.target}]`,
        response.body,
      );
    } else if (response.type == "wat") {
      setWat(response.wat);
    } else if (response.type == "core_wasm") {
      setCoreWasm(response.bytes);
      setComponentWasm(undefined); // In case of error.
    } else if (response.type == "component_wasm") {
      setComponentWasm(response.bytes);
    } else {
      response satisfies never;
    }
  });

  const onTextChanged = useCallback((code: string) => {
    worker.request({ request_id: ++request_id.current, code });
  }, []);

  return (
    <div className="flex--grow row">
      <div className="col col--6 flex--column">
        <Tabs
          current={inputTab}
          setCurrent={setInputTab}
          tabs={[
            {
              key: "Editor",
              body: <Editor onTextChanged={onTextChanged} />,
            },
          ]}
        />
      </div>
      <div className="col col--6 flex--column">
        <Tabs
          current={outputTab}
          setCurrent={setOutputTab}
          tabs={[
            {
              key: "About",
              body: (
                <div className="margin--sm">
                  <h1>Starstream Sandbox</h1>
                  <p>Tabs:</p>
                  <ul>
                    <li>
                      Wasm: The output of the Starstream compiler targeting
                      WebAssembly. Updates live.
                    </li>
                  </ul>
                  <p>Keyboard shortcuts:</p>
                  <ul>
                    <li>Ctrl+S to format</li>
                  </ul>
                </div>
              ),
            },
            {
              key: "Wasm",
              body: (
                <div className="margin--sm">
                  <div>
                    {[
                      ...(coreWasmUrl
                        ? [
                            <a href={coreWasmUrl} download>
                              Download core .wasm
                            </a>,
                          ]
                        : []),
                      ...(componentWasmUrl
                        ? [
                            <a href={componentWasmUrl} download>
                              Download component .wasm
                            </a>,
                          ]
                        : []),
                    ].reduce(
                      (prev, cur) => (prev ? [prev, " - ", cur] : cur),
                      null,
                    )}
                  </div>
                  <pre>
                    <AnsiHtml text={wat} />
                  </pre>
                </div>
              ),
            },
          ]}
        />
      </div>
    </div>
  );
}

export default function SandboxPage() {
  return (
    <Layout title="Sandbox">
      <div className="flex--grow margin-vert--md margin-horiz--md flex--column">
        <Sandbox />
      </div>
    </Layout>
  );
}

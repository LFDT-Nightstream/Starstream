import Layout from "@theme/Layout";
import {
  ComponentProps,
  Dispatch,
  ReactNode,
  SetStateAction,
  useEffect,
  useRef,
  useState,
} from "react";

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
  const [inputLedgerState, setInputLedgerState] = useState({});

  const [outputTab, setOutputTab] = useState("");
  const [busy, setBusy] = useState(false);
  const [compilerLog, setCompilerLog] = useState({
    log: "",
    warnings: 0,
    errors: 0,
  });

  const startTime = useRef(0);
  const request_id = useRef(0);

  return (
    <div className="flex--grow row">
      <div className="col col--6 flex--column">
        <Tabs
          current={inputTab}
          setCurrent={setInputTab}
          tabs={[
            {
              key: "Editor",
              body: <Editor onTextChanged={console.log} />,
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
                  <p>Keyboard shortcuts:</p>
                  <ul>
                    <li>Ctrl+S to format</li>
                  </ul>
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

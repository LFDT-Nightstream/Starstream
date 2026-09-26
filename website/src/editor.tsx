import { MonacoEditorReactComp } from "@typefox/monaco-editor-react";
import * as monaco from "monaco-editor";
import { EditorAppConfig } from "monaco-languageclient/editorApp";
import type { MonacoVscodeApiConfig } from "monaco-languageclient/vscodeApiWrapper";
import { configureDefaultWorkerFactory } from "monaco-languageclient/workerFactory";
import { useEffect, useRef } from "react";
import "./starstream.vsix";

const code = `\
abi Score {
    fn plus_chips(chips: u64);
    fn plus_mult(mult: u64);
    fn mult_mult(mult_pct: u64);
    fn finish();
    event Finish(total: u64);
}

utxo ScoreProgress {
    storage {
        let mut chips: u64;
        let mut mult: u64;
    }

    main fn new() {
        yield(Score);
        emit Finish(chips * mult);
    }

    impl Score {
        fn plus_chips(pub chips2: u64) {
            chips = chips + chips2;
        }
        fn plus_mult(pub mult2: u64) {
            mult = mult + mult2;
        }
        fn mult_mult(pub mult_pct: u64) {
            mult = mult * mult_pct / 100;
        }
        fn finish() {
            resume;
        }
    }
}
`;

// Doesn't seem to need to be global, but changes don't take effect, so it might as well be.
const vscodeApiConfig: MonacoVscodeApiConfig = {
  $type: "extended",
  viewsConfig: {
    $type: "EditorService",
  },
  userConfiguration: {
    json: JSON.stringify({
      "workbench.colorTheme": theme(),
      "editor.wordBasedSuggestions": "off",
      "editor.formatOnSave": true,
    }),
  },
  monacoWorkerFactory: configureDefaultWorkerFactory,
};

// MUST be global to avoid MonacoEditorReactComp double-initializing.
const editorAppConfig: EditorAppConfig = {
  codeResources: {
    modified: {
      text: code,
      uri: "/workspace/sandbox.star",
    },
  },
};

export function Editor(props: {
  onTextChanged?: (text: string) => void;
  onMarkersChange?: (markers: monaco.editor.IMarker[]) => void;
  onMount?: (editor: monaco.editor.ICodeEditor) => void;
}) {
  const cleanupRef = useRef<(() => void) | null>(null);

  useEffect(() => {
    return () => {
      cleanupRef.current?.();
    };
  }, []);

  return (
    <MonacoEditorReactComp
      style={{ width: "100%", height: "100%" }}
      vscodeApiConfig={vscodeApiConfig}
      editorAppConfig={editorAppConfig}
      onTextChanged={(contents) =>
        props.onTextChanged?.(contents.modified ?? "")
      }
      onVscodeApiInitDone={() => {
        const disconnectTheme = startWatchingTheme();

        let disposeMarkers: monaco.IDisposable | undefined;
        if (props.onMarkersChange) {
          const updateMarkers = () => {
            const markers = monaco.editor.getModelMarkers({});
            props.onMarkersChange?.(markers);
          };
          updateMarkers();
          disposeMarkers = monaco.editor.onDidChangeMarkers(updateMarkers);
        }

        let timeoutId: number | undefined;
        if (props.onMount) {
          timeoutId = window.setTimeout(() => {
            const editors = monaco.editor.getEditors();
            if (editors.length > 0) {
              props.onMount?.(editors[0]);
            }
          }, 100);
        }

        cleanupRef.current = () => {
          disconnectTheme();
          disposeMarkers?.dispose();
          if (timeoutId !== undefined) {
            window.clearTimeout(timeoutId);
          }
        };
      }}
    />
  );
}

function startWatchingTheme(): () => void {
  const mo = new MutationObserver((records) => {
    for (const each of records) {
      if (
        each.target === document.documentElement &&
        each.type === "attributes" &&
        each.attributeName === "data-theme"
      ) {
        monaco.editor.setTheme(theme());
      }
    }
  });
  mo.observe(document.documentElement, {
    attributes: true,
    attributeFilter: ["data-theme"],
  });
  monaco.editor.setTheme(theme());
  return () => mo.disconnect();
}

function theme() {
  return document.documentElement.getAttribute("data-theme") === "dark"
    ? "Default Dark Modern"
    : "Default Light Modern";
}

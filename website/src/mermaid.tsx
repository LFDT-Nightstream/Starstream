import mermaid from 'mermaid';
import { useId } from 'react';

export function MermaidDiagram(props: { text: string }) {
  const id = useId();
  function setDiv(div: HTMLDivElement) {
    const text = props.text;
    if (div && text) {
      (async () => {
        const { svg, bindFunctions } = await mermaid.render(id, text);
        div.innerHTML = svg;
        bindFunctions?.(div);
      })();
    }
  }
  return <div ref={setDiv} className="sandbox-mermaid"></div>;
}

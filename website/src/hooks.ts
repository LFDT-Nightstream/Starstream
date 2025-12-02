import { Ref, useEffect, useState, useSyncExternalStore } from "react";

export function setRef<T>(ref: Ref<T> | undefined, value: T) {
  if (ref === null || ref === undefined) {
    // Nothing to do.
  } else if ("current" in ref) {
    ref.current = value;
  } else if (ref instanceof Function) {
    ref(value);
  }
}

export function useDocusaurusTheme(): string {
  return useSyncExternalStore<string>(
    (onChange) => {
      const mo = new MutationObserver((records) => {
        for (const each of records) {
          if (
            each.target === document.documentElement &&
            each.type === "attributes" &&
            each.attributeName === "data-theme"
          ) {
            onChange();
          }
        }
      });
      mo.observe(document.documentElement, { attributes: true });
      return () => mo.disconnect();
    },
    () => document.documentElement.getAttribute("data-theme") ?? "light",
    () => "light",
  );
}

export function useBlobUrl(
  array: Uint8Array | undefined,
  filename: string,
  type: string,
): string | undefined {
  const [url, setUrl] = useState<string>();
  useEffect(() => {
    if (array) {
      const url = URL.createObjectURL(new File([array], filename, { type }));
      setUrl(url);
      return () => URL.revokeObjectURL(url);
    } else {
      setUrl(undefined);
    }
  }, [array, filename, type]);
  return url;
}

import { Ref, useSyncExternalStore } from "react";

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
    () => "light"
  );
}

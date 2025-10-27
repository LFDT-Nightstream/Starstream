import { useSyncExternalStore } from "react";

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

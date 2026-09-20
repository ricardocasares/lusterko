/// <reference types="vite/client" />

declare module "*.elm" {
  export const Elm: {
    Main: { init(options: { node: HTMLElement | null }): void };
  };
}

// Warry/elm-ffi 1.0.0 polyfill, BSD-3-Clause.
const globalScope = globalThis as typeof globalThis & Record<string, unknown>;
const nativeSetTimeout = setTimeout;
const AsyncFunction = Object.getPrototypeOf(async function () {})
  .constructor as FunctionConstructor;
const secret = -Math.random();
let promiseSlot: (() => Promise<unknown>) | undefined;

Object.defineProperty(Object.prototype, "_elm_ffi_read_", {
  get() {
    return (this as { value: unknown }).value;
  },
  set(code: string) {
    try {
      (this as { value: unknown }).value = Function(code)();
    } catch (error) {
      console.error(error);
    }
  },
});

Object.defineProperty(Object.prototype, "_elm_ffi_create_", {
  get() {
    return (this as { value: unknown }).value;
  },
  set({ args, code }: { args: string[]; code: string }) {
    (this as { value: unknown }).value = AsyncFunction(...args, code);
  },
});

Object.defineProperty(Object.prototype, "_elm_ffi_apply_", {
  get() {
    return (this as { value: unknown }).value;
  },
  set({
    holder,
    params,
  }: {
    holder: Record<string, (...values: unknown[]) => Promise<unknown>>;
    params: unknown[];
  }) {
    const target = this as { value: unknown };
    target.value = { AW: secret };
    promiseSlot = () =>
      holder
        ._elm_ffi_create_(...params)
        .then((value) => {
          target.value = { OK: value };
        })
        .catch((error) => {
          target.value = { ER: error };
        });
  },
});

globalScope.setTimeout = ((
  callback: TimerHandler,
  time?: number,
  ...args: unknown[]
) =>
  time === secret && promiseSlot
    ? promiseSlot().finally(() => (callback as () => void)())
    : nativeSetTimeout(callback, time, ...args)) as typeof setTimeout;

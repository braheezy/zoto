### Checkpoint – Web/WASM refactor

- Dedicated **Enable Audio** button added (`examples/wasm_player/index.html`) so user gestures are explicit and controllable.
- Browser glue moved to `examples/wasm_player/app.js`; now:
  - Manages JS handle interop, WASI shims, and all zoto exports.
  - Provides deterministic initialization with logging around `zoto_init`, `zoto_create_context`, and `zoto_setup_after_user_interaction`.
  - Button state reflects load/ready/error states; initialization work now runs via microtask/timeout to avoid blocking event handlers.
- Zig WASM driver (`src/driver_js.zig`) updated earlier to:
  - Use JS number helper imports and array/object constructors.
  - Post audio buffers via `postMessage` with transfer lists.
  - Release all transient JS handles to prevent leaks.
- Current issue under investigation: browser tab freeze right after logging `[zoto] initAudioOnInteraction: scheduling initialization`. Latest change defers work with `setTimeout` and adds more logging to isolate the stall.

Next validation steps:
1. `zig build wasm-player`
2. Serve `examples/wasm_player/` (e.g., `python3 -m http.server`).
3. Load `index.html`, click **Enable Audio**, observe console logs to see how far initialization proceeds.

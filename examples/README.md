# Nex examples

## Available examples

- `hello`: Language basics — factorial, FizzBuzz, string concatenation.

- `console_showcase`: A small console-style app showing imports, interfaces, classes,
  base qualification with `::`, `if/else`, `using`, `try/catch/finally`, assignment,
  and expression precedence.

- `game_showcase`: 3D game engine demonstration — window creation, camera, keyboard
  input, colored geometry, frame timing, and UI overlay (HUD) on top of the 3D scene.
  Requires `nex3d_native.dll` and `nex_ui_native.dll` in the runtime library path.

- `ui_declarative`: A desktop GUI application demonstrating the `nex_ui` library and
  `.nexui` declarative markup — layout containers (Column, Row, Grid), widgets (Text,
  Button, TextInput, Checkbox, Slider), canvas drawing, event callbacks, dynamic
  updates, and styling.
  Requires `nex_ui_native.dll` in the runtime library path.

- `gpt2_transformer`: A GPT-2-style language model built on the `torch` library.
  Demonstrates tensor operations, neural network layers, and training loops.
  Requires `nex_torch_native.dll` and libtorch at runtime.

## Running

From any example directory:

```
nex run
```

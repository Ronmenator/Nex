# Nex Project Layout

Workspace root:

- `project.toml`
- `src/*.nex` source modules

## Library Dependencies

External libraries are declared in `project.toml` under `[libs]`:

```toml
[libs]
nex3d = { path = "../libs/nex3d" }
```

- The library name becomes the import namespace prefix: `import nex3d.math`, `import nex3d.engine`
- Libraries are Nex projects with their own `project.toml` and `src/` directory
- Library sources are compiled alongside the main project
- v1: local paths only; flat resolution (no transitive dependencies)

### nex3d — Game Engine Library

The `nex3d` library provides 2D and 3D game development:

- **nex3d.math**: Vec2, Vec3, Vec4 and math helpers (pure Nex, uses std.math)
- **nex3d.engine**: Window creation and main loop (requires `--features engine`)

Requires: `cargo build --features engine -p nex`

Build artifacts:

- `*.nexmeta` metadata JSON
- generated object files and linked runtime artifacts (implementation-defined in v1)

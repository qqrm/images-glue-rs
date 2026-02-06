# images-glue-rs

A tiny browser tool to glue images into a single horizontal strip (row). Paste images from the clipboard,
reorder by dragging, resize with optional aspect ratio lock, then export as JPEG or PNG.

Tech: Rust + WASM + Leptos (CSR) + Canvas2D. Deploy: GitHub Pages.

## Local dev

Prereqs:
- Rust (stable)
- WASM target
- Trunk

```bash
rustup target add wasm32-unknown-unknown
cargo install trunk
```

Run:
```bash
just dev
```

Build release:
```bash
just build
```

## Controls

- Paste images: Ctrl/Cmd+V (each paste appends)
- Reorder: drag image by its body
- Resize: drag circular handles (corners/sides)
- Delete: hover an image and click “×” (top-right)
- Keep aspect ratio: checkbox (default ON)
- Global size normalization: slider (largest ↔ smallest)
- Export: Ctrl/Cmd+S or click Save
  - JPEG: white background, quality 0.9
  - PNG: transparent background

## Spec
See `SPEC.md`.

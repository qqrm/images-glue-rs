https://qqrm.github.io/images-glue-rs

**# images-glue-rs

A tiny browser tool to glue images into a single horizontal strip (row). Paste images from the clipboard,
reorder by dragging, resize with optional aspect ratio lock, then export as JPEG or PNG.

Tech: Rust + WASM + Leptos (CSR) + Canvas2D. Delivery: GitHub Pages.

## Local dev

List available Just recipes:

```bash
just --list
```

One-time bootstrap:

```bash
just bootstrap
```

Start dev server:

```bash
just dev
```

This runs Trunk on `http://127.0.0.1:8080/`.

Build release locally (optional):

```bash
just build
```

## Controls

- Add images: file picker (multiple) or paste with Ctrl/Cmd+V
- Reorder: drag an image; crossing centers swaps order
- Resize: drag circular handles
  - Keep aspect ratio: checkbox (default ON)
  - When OFF: independent X/Y scaling
- Delete: hover an image and click the “×” (top-right)
- Global size normalization: slider (largest ↔ smallest)
- Export: Ctrl/Cmd+S or click Save
  - JPEG: white background, quality 0.9
  - PNG: transparent background

## Deployment (GitHub Pages)

The repo contains a workflow that builds and deploys automatically on every push to `main` or `master`.

Initial one-time repo setup in GitHub UI:
- Settings → Pages
- Build and deployment → Source: GitHub Actions

After that: `git push` triggers the deploy.

## Spec

See `SPEC.md`.

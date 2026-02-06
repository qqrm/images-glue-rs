# Image Glue (Rust + WASM) — Specification v1.5

## Goal
A lightweight, browser-based tool to quickly glue images into a single tiled canvas (auto-wrapped rows).
Primary workflow: paste multiple images from the clipboard, reorder them by dragging, optionally resize
(individual images) with an aspect-ratio lock, and export the composed result as JPEG or PNG.

The app is fully client-side (no backend) using Rust + WASM + Leptos (CSR) + Canvas2D.
Delivery is GitHub Pages via GitHub Actions.

## In Scope (MVP)
1. Unlimited images (N ≥ 1).
2. Import:
   - Paste (Ctrl/Cmd+V): each pasted image is appended to the end of the layout order.
   - File picker: select one or more image files; all are appended in order.
   - Drag & drop: drop one or more image files; all are appended in order.
3. Layout:
   - Images are arranged as tiles in reading order (left → right, top → bottom) with automatic wrapping.
   - The app maintains an explicit order; on any change, the tiled layout is *packed* (no gaps).
4. Reorder:
   - Drag an image by its body to reorder tiles.
   - Drop position is computed by nearest-tile placement from the dragged image center; the item is inserted at the corresponding index.
   - (Optional but recommended) show an insertion marker while dragging.
5. Delete:
   - On hover, show an “×” button in the top-right of the image.
   - Clicking “×” removes the image from the layout.
6. Resize:
   - Show circular handles on hover/selection (corners + sides).
   - Global checkbox “Keep aspect ratio” (default ON).
     - ON: any resize preserves the image’s aspect ratio.
     - OFF: free scaling (corners change width & height independently; side handles change one axis).
   - After resizing, the tiled layout is repacked.
7. Global size normalization slider:
   - Let `h_min` be the minimum natural height among images, `h_max` the maximum natural height.
   - Slider `S ∈ [0..1]`:
     - `S = 0`: normalize to the largest image height → `H_target = h_max`
     - `S = 1`: normalize to the smallest image height → `H_target = h_min`
     - Default `S = 0.5` (linear interpolation):
       - `H_target = h_max*(1-S) + h_min*S`
   - Base size for each image `i`:
     - `h_base_i = H_target`
     - `w_base_i = H_target * (nat_w_i / nat_h_i)`
   - Manual resize is preserved as a multiplier on top of base size:
     - If Keep aspect ON: per-image `k_i` (uniform scale).
       - `w_i = w_base_i * k_i`, `h_i = h_base_i * k_i`
     - If Keep aspect OFF: per-image `sx_i`, `sy_i` (non-uniform scale).
       - `w_i = w_base_i * sx_i`, `h_i = h_base_i * sy_i`
8. Export:
   - Format selector: JPEG (default) / PNG.
   - JPEG background is always white; PNG background is transparent.
   - Ctrl/Cmd+S triggers export (and prevents the browser “Save page” default).
   - Output size is the bounding box of the packed tiled layout.
   - JPEG quality default: 0.9.

## Out of Scope (MVP)
- Overlay/compositing modes, opacity/blend modes
- Rotation
- Crop frame (can be added later)
- Undo/redo
- Project saving/loading

## UX / Shortcuts
- Paste: Ctrl/Cmd+V (append images)
- Save: Ctrl/Cmd+S (export)
- Pointer:
  - Drag image body: reorder
  - Drag handles: resize
  - Hover “×”: delete image

## Non-Goals
This is intentionally not a full image editor; it is a fast glue tool optimized for clipboard-driven workflows.

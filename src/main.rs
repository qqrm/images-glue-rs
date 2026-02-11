use js_sys::Reflect;
use leptos::prelude::*;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_futures::JsFuture;
use web_sys::{
    Blob, ClipboardEvent, DragEvent, File, FileList, HtmlAnchorElement, HtmlCanvasElement,
    ImageBitmap, PointerEvent, Url, Window,
};

use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExportFormat {
    Jpeg,
    Png,
}

impl ExportFormat {
    fn mime(self) -> &'static str {
        match self {
            ExportFormat::Jpeg => "image/jpeg",
            ExportFormat::Png => "image/png",
        }
    }
    fn ext(self) -> &'static str {
        match self {
            ExportFormat::Jpeg => "jpg",
            ExportFormat::Png => "png",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Handle {
    N,
    S,
    E,
    W,
    NE,
    NW,
    SE,
    SW,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LayoutMode {
    Line,
    Vertical,
    FreeFlow,
}

impl LayoutMode {
    fn as_str(self) -> &'static str {
        match self {
            LayoutMode::Line => "line",
            LayoutMode::Vertical => "vertical",
            LayoutMode::FreeFlow => "free",
        }
    }

    fn label(self) -> &'static str {
        match self {
            LayoutMode::Line => "Line",
            LayoutMode::Vertical => "Vertical",
            LayoutMode::FreeFlow => "Free flow",
        }
    }

    fn from_str(s: &str) -> Self {
        match s {
            "line" => LayoutMode::Line,
            "vertical" => LayoutMode::Vertical,
            "free" => LayoutMode::FreeFlow,
            _ => LayoutMode::Line,
        }
    }
}

#[derive(Clone, Debug)]
struct LayoutTransition {
    from_positions: HashMap<u64, (f64, f64)>,
    start_ms: f64,
    duration_ms: f64,
}

#[derive(Clone, Debug)]
struct ImageItem {
    id: u64,
    _name: String,
    bitmap: ImageBitmap,
    nat_w: f64,
    nat_h: f64,

    // manual scaling on top of base size
    k: f64,  // used when keep_aspect = true
    sx: f64, // used when keep_aspect = false
    sy: f64, // used when keep_aspect = false

    // workspace position (top-left) in logical CSS px
    x: f64,
    y: f64,
}

#[derive(Clone, Debug)]
struct LayoutItem {
    id: u64,
    x: f64,
    y: f64,
    w: f64,
    h: f64,
    delete_btn: (f64, f64, f64, f64), // x,y,w,h
}

#[derive(Clone, Debug, Default)]
struct Layout {
    items: Vec<LayoutItem>,
    out_w: f64,
    out_h: f64,
    view_w: f64,
    view_h: f64,
    warn_too_large: bool,
}

#[derive(Clone, Debug)]
struct AppState {
    images: Vec<ImageItem>,
    keep_aspect: bool,
    slider: f64, // 0..1
    export_format: ExportFormat,
    layout_mode: LayoutMode,
    selected: Option<u64>,
    hovered: Option<u64>,
    proximity_hovered: Option<u64>,

    layout: Layout,
    drag: Option<DragState>,
    transition: Option<LayoutTransition>,
}

#[derive(Clone, Debug)]
enum DragState {
    Move {
        id: u64,
        grab_dx: f64, // pointer_x - item_x at drag start
        grab_dy: f64, // pointer_y - item_y at drag start
        draw_x: f64,
        draw_y: f64,
    },
    Reorder {
        id: u64,
        insertion_index: usize,
        pointer_x: f64,
        pointer_y: f64,
        pointer_dx: f64,
        pointer_dy: f64,
    },
    Resize {
        id: u64,
        handle: Handle,
        start_pointer_x: f64,
        start_pointer_y: f64,
        start_k: f64,
        start_sx: f64,
        start_sy: f64,
        // base sizes at drag start for stable math
        base_w: f64,
        base_h: f64,
        // current box at start
        start_x: f64,
        start_y: f64,
        start_w: f64,
        start_h: f64,
    },
}

const MIN_IMAGE_SIDE_PX: f64 = 48.0;
const PREVIEW_PADDING_PX: f64 = 12.0;

impl Default for AppState {
    fn default() -> Self {
        Self {
            images: Vec::new(),
            keep_aspect: true,
            slider: 0.5,
            export_format: ExportFormat::Jpeg,
            layout_mode: LayoutMode::Line,
            selected: None,
            hovered: None,
            proximity_hovered: None,
            layout: Layout::default(),
            drag: None,
            transition: None,
        }
    }
}

fn window() -> Window {
    web_sys::window().expect("window")
}

async fn create_image_bitmap_from_file(file: &File) -> Result<ImageBitmap, JsValue> {
    let blob: Blob = file.clone().dyn_into::<Blob>()?;
    create_image_bitmap_from_blob(&blob).await
}

async fn create_image_bitmap_from_blob(blob: &Blob) -> Result<ImageBitmap, JsValue> {
    let promise = window().create_image_bitmap_with_blob(blob)?;
    let js = JsFuture::from(promise).await?;
    js.dyn_into::<ImageBitmap>()
}

fn file_list_to_vec(files: &FileList) -> Vec<File> {
    let mut out = Vec::new();
    for i in 0..files.length() {
        if let Some(f) = files.get(i) {
            out.push(f);
        }
    }
    out
}

fn now_ts() -> String {
    // ISO-ish without punctuation for filenames
    let d = js_sys::Date::new_0();
    let y = d.get_full_year();
    let m = d.get_month() + 1;
    let day = d.get_date();
    let hh = d.get_hours();
    let mm = d.get_minutes();
    let ss = d.get_seconds();
    format!(
        "{:04}{:02}{:02}-{:02}{:02}{:02}",
        y as i32, m as i32, day as i32, hh as i32, mm as i32, ss as i32
    )
}

impl AppState {
    fn compute_targets(&self) -> (f64, f64) {
        let mut h_min = f64::INFINITY;
        let mut h_max: f64 = 0.0;
        let mut w_min = f64::INFINITY;
        let mut w_max: f64 = 0.0;

        for img in &self.images {
            h_min = h_min.min(img.nat_h);
            h_max = h_max.max(img.nat_h);
            w_min = w_min.min(img.nat_w);
            w_max = w_max.max(img.nat_w);
        }

        if !h_min.is_finite() {
            h_min = 1.0;
        }
        if !w_min.is_finite() {
            w_min = 1.0;
        }

        let s = self.slider.clamp(0.0, 1.0);
        let h_target = h_max * (1.0 - s) + h_min * s;
        let w_target = w_max * (1.0 - s) + w_min * s;
        (h_target, w_target)
    }

    fn base_size_for(&self, img: &ImageItem, h_target: f64, w_target: f64) -> (f64, f64) {
        let aspect = img.nat_w / img.nat_h;
        match self.layout_mode {
            LayoutMode::Line | LayoutMode::FreeFlow => {
                let base_h = h_target;
                let base_w = h_target * aspect;
                (base_w, base_h)
            }
            LayoutMode::Vertical => {
                let base_w = w_target;
                let base_h = w_target / aspect;
                (base_w, base_h)
            }
        }
    }

    fn final_size_for(&self, img: &ImageItem, base_w: f64, base_h: f64) -> (f64, f64) {
        if self.keep_aspect {
            let k = img.k.max(0.01);
            (base_w * k, base_h * k)
        } else {
            let sx = img.sx.max(0.01);
            let sy = img.sy.max(0.01);
            (base_w * sx, base_h * sy)
        }
    }

    fn recompute_layout(&mut self) {
        let mut layout = Layout::default();

        if self.images.is_empty() {
            self.layout = layout;
            return;
        }

        let (h_target, w_target) = self.compute_targets();

        if self.layout_mode == LayoutMode::FreeFlow {
            for img in &mut self.images {
                if !img.x.is_finite() {
                    img.x = 0.0;
                }
                if !img.y.is_finite() {
                    img.y = 0.0;
                }
            }

            let min_x = self
                .images
                .iter()
                .map(|i| i.x)
                .fold(f64::INFINITY, f64::min);
            let min_y = self
                .images
                .iter()
                .map(|i| i.y)
                .fold(f64::INFINITY, f64::min);

            let shift_x = if min_x < 0.0 { -min_x } else { 0.0 };
            let shift_y = if min_y < 0.0 { -min_y } else { 0.0 };
            if shift_x != 0.0 || shift_y != 0.0 {
                for img in &mut self.images {
                    img.x += shift_x;
                    img.y += shift_y;
                }
            }
        }

        let mut out_w: f64 = 0.0;
        let mut out_h: f64 = 0.0;

        match self.layout_mode {
            LayoutMode::Line => {
                let mut x: f64 = 0.0;
                for img in &self.images {
                    let (base_w, base_h) = self.base_size_for(img, h_target, w_target);
                    let (w, h) = self.final_size_for(img, base_w, base_h);
                    let item_x = x;
                    let item_y = 0.0;
                    x += w;

                    let btn = {
                        let pad = 4.0;
                        let sz = 18.0;
                        (item_x + w - sz - pad, item_y + pad, sz, sz)
                    };

                    layout.items.push(LayoutItem {
                        id: img.id,
                        x: item_x,
                        y: item_y,
                        w,
                        h,
                        delete_btn: btn,
                    });

                    out_w = x;
                    out_h = out_h.max(h);
                }
            }
            LayoutMode::Vertical => {
                let mut y: f64 = 0.0;
                for img in &self.images {
                    let (base_w, base_h) = self.base_size_for(img, h_target, w_target);
                    let (w, h) = self.final_size_for(img, base_w, base_h);
                    let item_x = 0.0;
                    let item_y = y;
                    y += h;

                    let btn = {
                        let pad = 4.0;
                        let sz = 18.0;
                        (item_x + w - sz - pad, item_y + pad, sz, sz)
                    };

                    layout.items.push(LayoutItem {
                        id: img.id,
                        x: item_x,
                        y: item_y,
                        w,
                        h,
                        delete_btn: btn,
                    });

                    out_h = y;
                    out_w = out_w.max(w);
                }
            }
            LayoutMode::FreeFlow => {
                for img in &self.images {
                    let (base_w, base_h) = self.base_size_for(img, h_target, w_target);
                    let (w, h) = self.final_size_for(img, base_w, base_h);
                    let item_x = img.x;
                    let item_y = img.y;

                    let btn = {
                        let pad = 4.0;
                        let sz = 18.0;
                        (item_x + w - sz - pad, item_y + pad, sz, sz)
                    };

                    layout.items.push(LayoutItem {
                        id: img.id,
                        x: item_x,
                        y: item_y,
                        w,
                        h,
                        delete_btn: btn,
                    });

                    out_w = out_w.max(item_x + w);
                    out_h = out_h.max(item_y + h);
                }
            }
        }

        layout.out_w = out_w;
        layout.out_h = out_h;
        layout.view_w = (layout.out_w + 1200.0).max(1600.0);
        layout.view_h = (layout.out_h + 900.0).max(900.0);
        layout.warn_too_large = layout.out_w > 16384.0 || layout.out_h > 16384.0;

        self.layout = layout;

        let ids: std::collections::HashSet<u64> = self.images.iter().map(|i| i.id).collect();
        if let Some(id) = self.selected
            && !ids.contains(&id)
        {
            self.selected = None;
        }
        if let Some(id) = self.hovered
            && !ids.contains(&id)
        {
            self.hovered = None;
        }
        if let Some(id) = self.proximity_hovered
            && !ids.contains(&id)
        {
            self.proximity_hovered = None;
        }
    }

    fn hit_test_item(&self, px: f64, py: f64) -> Option<u64> {
        for it in &self.layout.items {
            if px >= it.x && px <= it.x + it.w && py >= it.y && py <= it.y + it.h {
                return Some(it.id);
            }
        }
        None
    }

    fn hit_test_item_with_margin(&self, px: f64, py: f64, margin: f64) -> Option<u64> {
        let mut best: Option<(u64, f64)> = None;
        for it in &self.layout.items {
            let dx = if px < it.x {
                it.x - px
            } else if px > it.x + it.w {
                px - (it.x + it.w)
            } else {
                0.0
            };
            let dy = if py < it.y {
                it.y - py
            } else if py > it.y + it.h {
                py - (it.y + it.h)
            } else {
                0.0
            };

            let dist2 = dx * dx + dy * dy;
            if dist2 <= margin * margin {
                match best {
                    Some((_, best_d2)) if dist2 >= best_d2 => {}
                    _ => best = Some((it.id, dist2)),
                }
            }
        }
        best.map(|(id, _)| id)
    }

    fn hit_test_delete(&self, id: u64, px: f64, py: f64) -> bool {
        if let Some(it) = self.layout.items.iter().find(|x| x.id == id) {
            let (bx, by, bw, bh) = it.delete_btn;
            px >= bx && px <= bx + bw && py >= by && py <= by + bh
        } else {
            false
        }
    }

    fn handle_points(it: &LayoutItem) -> [(Handle, f64, f64); 8] {
        let x = it.x;
        let y = it.y;
        let w = it.w;
        let h = it.h;
        [
            (Handle::NW, x, y),
            (Handle::N, x + w / 2.0, y),
            (Handle::NE, x + w, y),
            (Handle::E, x + w, y + h / 2.0),
            (Handle::SE, x + w, y + h),
            (Handle::S, x + w / 2.0, y + h),
            (Handle::SW, x, y + h),
            (Handle::W, x, y + h / 2.0),
        ]
    }

    fn hit_test_handle(&self, id: u64, px: f64, py: f64) -> Option<Handle> {
        let it = self.layout.items.iter().find(|x| x.id == id)?;
        let r = 8.0;
        for (h, hx, hy) in Self::handle_points(it) {
            let dx = px - hx;
            let dy = py - hy;
            if dx * dx + dy * dy <= r * r {
                return Some(h);
            }
        }
        None
    }

    fn remove_image(&mut self, id: u64) {
        self.images.retain(|i| i.id != id);
        if self.selected == Some(id) {
            self.selected = None;
        }
        if self.hovered == Some(id) {
            self.hovered = None;
        }
        if self.proximity_hovered == Some(id) {
            self.proximity_hovered = None;
        }
        self.drag = None;
        self.recompute_layout();
    }

    fn reorder_by_insertion(&mut self, id: u64, insertion_index: usize) {
        let Some(from) = self.images.iter().position(|i| i.id == id) else {
            return;
        };
        let item = self.images.remove(from);
        let to = insertion_index.min(self.images.len());
        self.images.insert(to, item);
        self.recompute_layout();
    }

    fn compute_insertion_index(
        &self,
        dragged_id: u64,
        dragged_center: f64,
        axis_is_vertical: bool,
    ) -> usize {
        let mut centers: Vec<f64> = self
            .layout
            .items
            .iter()
            .filter(|it| it.id != dragged_id)
            .map(|it| {
                if axis_is_vertical {
                    it.y + it.h / 2.0
                } else {
                    it.x + it.w / 2.0
                }
            })
            .collect();
        centers.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let mut idx = 0usize;
        for c in centers {
            if dragged_center > c {
                idx += 1;
            } else {
                break;
            }
        }
        idx
    }
}

fn schedule_animation_tick(state: RwSignal<AppState, leptos::prelude::LocalStorage>) {
    set_timeout(
        move || {
            let should_continue = state.with(|s| {
                if let Some(t) = &s.transition {
                    js_sys::Date::now() < t.start_ms + t.duration_ms
                } else {
                    false
                }
            });

            if should_continue {
                state.update(|_| {});
                schedule_animation_tick(state);
            } else {
                state.update(|s| s.transition = None);
            }
        },
        std::time::Duration::from_millis(16),
    );
}

fn is_shortcut_key(ev: &web_sys::KeyboardEvent, code: &str, key_ascii: char) -> bool {
    ev.code() == code || ev.key().eq_ignore_ascii_case(&key_ascii.to_string())
}

fn set_canvas_size(
    canvas: &HtmlCanvasElement,
    css_w: f64,
    css_h: f64,
) -> Option<(web_sys::CanvasRenderingContext2d, f64)> {
    let dpr = window().device_pixel_ratio();
    canvas
        .set_attribute(
            "style",
            &format!("width:{}px;height:{}px;", css_w.max(1.0), css_h.max(1.0)),
        )
        .ok()?;

    canvas.set_width((css_w.max(1.0) * dpr).round() as u32);
    canvas.set_height((css_h.max(1.0) * dpr).round() as u32);

    let ctx = canvas
        .get_context("2d")
        .ok()??
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .ok()?;

    ctx.set_transform(dpr, 0.0, 0.0, dpr, 0.0, 0.0).ok()?;
    Some((ctx, dpr))
}

fn render(state: &AppState, canvas: &HtmlCanvasElement) {
    let preview_pad = PREVIEW_PADDING_PX;
    let w = (state.layout.view_w + preview_pad * 2.0).max(1.0);
    let h = (state.layout.view_h + preview_pad * 2.0).max(1.0);

    let Some((ctx, _dpr)) = set_canvas_size(canvas, w, h) else {
        return;
    };

    ctx.clear_rect(0.0, 0.0, w, h);

    let reorder_preview = if let Some(DragState::Reorder {
        id,
        insertion_index,
        pointer_x,
        pointer_y,
        ..
    }) = &state.drag
    {
        let axis_is_vertical = state.layout_mode == LayoutMode::Vertical;
        let mut packed = 0.0;
        let mut idx = 0usize;
        let mut preview_positions = HashMap::new();
        let dragged_layout = state.layout.items.iter().find(|it| it.id == *id).cloned();

        if let Some(dragged_layout) = dragged_layout {
            for it in &state.layout.items {
                if it.id == *id {
                    continue;
                }
                if idx == *insertion_index {
                    packed += if axis_is_vertical {
                        dragged_layout.h
                    } else {
                        dragged_layout.w
                    };
                }
                preview_positions.insert(it.id, packed);
                packed += if axis_is_vertical { it.h } else { it.w };
                idx += 1;
            }

            Some((
                *id,
                axis_is_vertical,
                preview_positions,
                *pointer_x,
                *pointer_y,
                dragged_layout,
            ))
        } else {
            None
        }
    } else {
        None
    };

    for it in &state.layout.items {
        let is_dragged = state
            .drag
            .as_ref()
            .and_then(|drag| match drag {
                DragState::Move { id, .. } | DragState::Reorder { id, .. } => Some(*id),
                _ => None,
            })
            .map(|dragged_id| dragged_id == it.id)
            .unwrap_or(false);

        if is_dragged {
            continue;
        }

        let draw_pos = if let Some((x, y)) =
            reorder_preview
                .as_ref()
                .and_then(|(_, axis_is_vertical, map, _, _, _)| {
                    map.get(&it.id).map(|packed| {
                        if *axis_is_vertical {
                            (0.0, *packed)
                        } else {
                            (*packed, 0.0)
                        }
                    })
                }) {
            (x, y)
        } else if let Some(t) = &state.transition {
            let target = (it.x, it.y);
            let from = t.from_positions.get(&it.id).copied().unwrap_or(target);
            let p = ((js_sys::Date::now() - t.start_ms) / t.duration_ms).clamp(0.0, 1.0);
            let e = 1.0 - (1.0 - p).powi(3);
            (
                from.0 + (target.0 - from.0) * e,
                from.1 + (target.1 - from.1) * e,
            )
        } else {
            (it.x, it.y)
        };

        let draw_x = draw_pos.0 + preview_pad;
        let draw_y = draw_pos.1 + preview_pad;

        if let Some(img) = state.images.iter().find(|i| i.id == it.id) {
            let _ = ctx.draw_image_with_image_bitmap_and_dw_and_dh(
                &img.bitmap,
                draw_x,
                draw_y,
                it.w,
                it.h,
            );
        }

        let is_sel = state.selected == Some(it.id);
        let is_hov = state.hovered == Some(it.id);
        if is_sel || is_hov {
            ctx.save();
            ctx.set_line_width(if is_sel { 2.0 } else { 1.0 });

            Reflect::set(
                ctx.as_ref(),
                &JsValue::from_str("strokeStyle"),
                &JsValue::from_str(if is_sel {
                    "#66aaff"
                } else {
                    "rgba(255,255,255,0.6)"
                }),
            )
            .unwrap();

            ctx.stroke_rect(draw_x + 0.5, draw_y + 0.5, it.w - 1.0, it.h - 1.0);
            ctx.restore();
        }

        if is_hov {
            let (_bx, _by, bw, bh) = it.delete_btn;
            let bx = draw_x + it.w - bw - 4.0;
            let by = draw_y + 4.0;
            ctx.save();

            Reflect::set(
                ctx.as_ref(),
                &JsValue::from_str("fillStyle"),
                &JsValue::from_str("rgba(0,0,0,0.55)"),
            )
            .unwrap();
            ctx.fill_rect(bx, by, bw, bh);

            Reflect::set(
                ctx.as_ref(),
                &JsValue::from_str("strokeStyle"),
                &JsValue::from_str("rgba(255,255,255,0.8)"),
            )
            .unwrap();
            ctx.stroke_rect(bx + 0.5, by + 0.5, bw - 1.0, bh - 1.0);

            Reflect::set(
                ctx.as_ref(),
                &JsValue::from_str("fillStyle"),
                &JsValue::from_str("rgba(255,255,255,0.9)"),
            )
            .unwrap();

            ctx.set_font("16px system-ui");
            ctx.set_text_align("center");
            ctx.set_text_baseline("middle");
            let _ = ctx.fill_text("×", bx + bw / 2.0, by + bh / 2.0 + 0.5);
            ctx.restore();
        }
    }

    let dragging_non_resize = matches!(
        state.drag,
        Some(DragState::Move { .. } | DragState::Reorder { .. })
    );
    let handle_owner = if dragging_non_resize {
        None
    } else {
        state.selected.or(state.hovered).or(state.proximity_hovered)
    };
    if let Some(id) = handle_owner
        && let Some(it) = state.layout.items.iter().find(|x| x.id == id)
    {
        ctx.save();

        Reflect::set(
            ctx.as_ref(),
            &JsValue::from_str("fillStyle"),
            &JsValue::from_str("rgba(255,255,255,0.9)"),
        )
        .unwrap();

        Reflect::set(
            ctx.as_ref(),
            &JsValue::from_str("strokeStyle"),
            &JsValue::from_str("rgba(0,0,0,0.6)"),
        )
        .unwrap();

        ctx.set_line_width(1.0);

        let r = 5.5;
        for (_h, hx, hy) in AppState::handle_points(it) {
            ctx.begin_path();
            let _ = ctx.arc(
                hx + preview_pad,
                hy + preview_pad,
                r,
                0.0,
                std::f64::consts::TAU,
            );
            ctx.close_path();
            ctx.fill();
            ctx.stroke();
        }

        ctx.restore();
    }

    if let Some(DragState::Move {
        id, draw_x, draw_y, ..
    }) = &state.drag
        && let Some(it) = state.layout.items.iter().find(|it| it.id == *id)
        && let Some(dragged) = state.images.iter().find(|img| img.id == *id)
    {
        let cx = *draw_x + preview_pad;
        let cy = *draw_y + preview_pad;
        ctx.save();
        let _ = ctx.draw_image_with_image_bitmap_and_dw_and_dh(&dragged.bitmap, cx, cy, it.w, it.h);
        Reflect::set(
            ctx.as_ref(),
            &JsValue::from_str("strokeStyle"),
            &JsValue::from_str("rgba(255,255,255,0.9)"),
        )
        .unwrap();
        ctx.set_line_width(1.5);
        ctx.stroke_rect(cx + 0.5, cy + 0.5, it.w - 1.0, it.h - 1.0);
        ctx.restore();
    }

    if let Some((id, _, _, pointer_x, pointer_y, it)) = reorder_preview
        && let Some(dragged) = state.images.iter().find(|img| img.id == id)
    {
        let cx = pointer_x - it.w / 2.0 + preview_pad;
        let cy = pointer_y - it.h / 2.0 + preview_pad;
        ctx.save();
        let _ = ctx.draw_image_with_image_bitmap_and_dw_and_dh(&dragged.bitmap, cx, cy, it.w, it.h);
        Reflect::set(
            ctx.as_ref(),
            &JsValue::from_str("strokeStyle"),
            &JsValue::from_str("rgba(255,255,255,0.9)"),
        )
        .unwrap();
        ctx.set_line_width(1.5);
        ctx.stroke_rect(cx + 0.5, cy + 0.5, it.w - 1.0, it.h - 1.0);
        ctx.restore();
    }
}

async fn append_files(state: impl Update<Value = AppState> + Copy, files: Vec<File>) {
    for f in files {
        let mime = f.type_();
        if !mime.starts_with("image/") {
            continue;
        }
        let bmp = match create_image_bitmap_from_file(&f).await {
            Ok(b) => b,
            Err(_) => continue,
        };
        let nat_w = bmp.width() as f64;
        let nat_h = bmp.height() as f64;
        let name = f.name();

        state.update(|s| {
            let id = js_sys::Math::random().to_bits();
            s.images.push(ImageItem {
                id,
                _name: name,
                bitmap: bmp.clone(),
                nat_w,
                nat_h,
                k: 1.0,
                sx: 1.0,
                sy: 1.0,
                x: 0.0,
                y: s.layout.out_h,
            });
            s.selected = Some(id);
            s.recompute_layout();
        });
    }
}

fn render_output_canvas(state: &AppState, format: ExportFormat) -> Option<HtmlCanvasElement> {
    if state.images.is_empty() {
        return None;
    }

    let out_w = state.layout.out_w.round().max(1.0) as u32;
    let out_h = state.layout.out_h.round().max(1.0) as u32;

    let doc = window().document().expect("document");
    let canvas: HtmlCanvasElement = doc
        .create_element("canvas")
        .expect("create canvas")
        .dyn_into()
        .expect("canvas");
    canvas.set_width(out_w);
    canvas.set_height(out_h);

    let ctx: web_sys::CanvasRenderingContext2d = canvas
        .get_context("2d")
        .expect("ctx")
        .unwrap()
        .dyn_into()
        .unwrap();

    if format == ExportFormat::Jpeg {
        Reflect::set(
            ctx.as_ref(),
            &JsValue::from_str("fillStyle"),
            &JsValue::from_str("#ffffff"),
        )
        .unwrap();
        ctx.fill_rect(0.0, 0.0, out_w as f64, out_h as f64);
    } else {
        ctx.clear_rect(0.0, 0.0, out_w as f64, out_h as f64);
    }

    for it in &state.layout.items {
        if let Some(img) = state.images.iter().find(|i| i.id == it.id) {
            let _ =
                ctx.draw_image_with_image_bitmap_and_dw_and_dh(&img.bitmap, it.x, it.y, it.w, it.h);
        }
    }

    Some(canvas)
}

fn copy_current_to_clipboard(state: &AppState) {
    let Some(canvas) = render_output_canvas(state, ExportFormat::Png) else {
        return;
    };

    let cb = Closure::<dyn FnMut(JsValue)>::new(move |blob_js: JsValue| {
        if blob_js.is_null() || blob_js.is_undefined() {
            return;
        }
        let blob: Blob = blob_js.dyn_into().unwrap();

        let f = js_sys::Function::new_with_args(
            "blob",
            "if (!navigator.clipboard || !window.ClipboardItem) return Promise.reject('clipboard unavailable');\n             return navigator.clipboard.write([new ClipboardItem({'image/png': blob})]);",
        );
        let _ = f.call1(&JsValue::NULL, &blob.into());
    });

    let _ = canvas.to_blob_with_type(
        cb.as_ref()
            .dyn_ref::<js_sys::Function>()
            .expect("toBlob cb"),
        ExportFormat::Png.mime(),
    );
    cb.forget();
}

fn export_current(state: &AppState) {
    let Some(canvas) = render_output_canvas(state, state.export_format) else {
        return;
    };

    let mime = state.export_format.mime();
    let ext = state.export_format.ext();
    let filename = format!("glue-{}.{}", now_ts(), ext);

    let cb = Closure::<dyn FnMut(JsValue)>::new(move |blob_js: JsValue| {
        if blob_js.is_null() || blob_js.is_undefined() {
            return;
        }
        let blob: Blob = blob_js.dyn_into().unwrap();

        let url = Url::create_object_url_with_blob(&blob).unwrap();
        let doc = window().document().unwrap();

        let a: HtmlAnchorElement = doc
            .create_element("a")
            .unwrap()
            .dyn_into::<HtmlAnchorElement>()
            .unwrap();
        a.set_href(&url);
        a.set_download(&filename);
        let _ = a.set_attribute("style", "display:none;");

        doc.body().unwrap().append_child(&a).ok();
        a.click();
        a.remove();

        Url::revoke_object_url(&url).ok();
    });

    if state.export_format == ExportFormat::Jpeg {
        let to_blob = js_sys::Reflect::get(canvas.as_ref(), &JsValue::from_str("toBlob"))
            .ok()
            .and_then(|v| v.dyn_into::<js_sys::Function>().ok());

        if let Some(f) = to_blob {
            let this = JsValue::from(canvas.clone());
            let args = js_sys::Array::new();
            args.push(cb.as_ref());
            args.push(&JsValue::from_str(mime));
            args.push(&JsValue::from_f64(0.9));
            let _ = f.apply(&this, &args);
        } else {
            let _ = canvas.to_blob_with_type(
                cb.as_ref()
                    .dyn_ref::<js_sys::Function>()
                    .expect("toBlob cb"),
                mime,
            );
        }
    } else {
        let _ = canvas.to_blob_with_type(
            cb.as_ref()
                .dyn_ref::<js_sys::Function>()
                .expect("toBlob cb"),
            mime,
        );
    }
    cb.forget();
}

#[component]
fn App() -> impl IntoView {
    let canvas_ref = NodeRef::<leptos::html::Canvas>::new();
    let file_ref = NodeRef::<leptos::html::Input>::new();

    let state = RwSignal::new_local(AppState::default());
    let copy_feedback = RwSignal::new_local(false);

    // render effect
    Effect::new({
        move |_| {
            state.with(|s| {
                if let Some(canvas) = canvas_ref.get() {
                    render(s, &canvas);
                }
            });
        }
    });

    // global paste handler
    {
        let closure = Closure::<dyn FnMut(ClipboardEvent)>::new(move |ev: ClipboardEvent| {
            let Some(data) = ev.clipboard_data() else {
                return;
            };
            let items = data.items();
            for i in 0..items.length() {
                if let Some(it) = items.get(i) {
                    let mime = it.type_();
                    if !mime.starts_with("image/") {
                        continue;
                    }
                    if let Ok(Some(file)) = it.get_as_file() {
                        wasm_bindgen_futures::spawn_local(async move {
                            append_files(state, vec![file]).await;
                        });
                        // accept the first image from this paste event
                        break;
                    }
                }
            }
        });
        window()
            .add_event_listener_with_callback(
                "paste",
                closure
                    .as_ref()
                    .dyn_ref::<js_sys::Function>()
                    .expect("paste fn"),
            )
            .expect("paste listener");
        closure.forget();
    }

    // global keydown: save/copy shortcuts
    {
        let closure =
            Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(move |ev: web_sys::KeyboardEvent| {
                if ev.ctrl_key() || ev.meta_key() {
                    if is_shortcut_key(&ev, "KeyS", 's') {
                        ev.prevent_default();
                        state.with(export_current);
                    } else if is_shortcut_key(&ev, "KeyC", 'c') {
                        ev.prevent_default();
                        state.with(copy_current_to_clipboard);
                        copy_feedback.set(true);
                        set_timeout(
                            move || copy_feedback.set(false),
                            std::time::Duration::from_millis(700),
                        );
                    }
                }
            });
        window()
            .add_event_listener_with_callback(
                "keydown",
                closure
                    .as_ref()
                    .dyn_ref::<js_sys::Function>()
                    .expect("keydown fn"),
            )
            .expect("keydown listener");
        closure.forget();
    }

    // file input onchange
    let on_files = {
        move |_| {
            let Some(input) = file_ref.get() else { return };
            let Some(files) = input.files() else { return };
            let files = file_list_to_vec(&files);
            // reset input so selecting same file again triggers change
            input.set_value("");
            wasm_bindgen_futures::spawn_local(async move {
                append_files(state, files).await;
            });
        }
    };

    // drag & drop
    let on_drag_over = move |ev: DragEvent| {
        ev.prevent_default();
    };
    let on_drop = {
        move |ev: DragEvent| {
            ev.prevent_default();
            let Some(dt) = ev.data_transfer() else { return };
            let files = dt.files();
            let Some(files) = files else { return };
            let files = file_list_to_vec(&files);
            wasm_bindgen_futures::spawn_local(async move {
                append_files(state, files).await;
            });
        }
    };

    // pointer handlers on canvas
    let on_pointer_move = {
        move |ev: PointerEvent| {
            let Some(canvas) = canvas_ref.get() else {
                return;
            };
            let px = ev.offset_x() as f64 - PREVIEW_PADDING_PX;
            let py = ev.offset_y() as f64 - PREVIEW_PADDING_PX;

            state.update(|s| {
                s.hovered = s.hit_test_item(px, py);
                s.proximity_hovered = if s.hovered.is_none() {
                    s.hit_test_item_with_margin(px, py, 20.0)
                } else {
                    None
                };

                if let Some(d) = s.drag.clone() {
                    match d {
                        DragState::Move {
                            id,
                            grab_dx,
                            grab_dy,
                            ..
                        } => {
                            s.hovered = None;
                            s.proximity_hovered = None;

                            s.drag = Some(DragState::Move {
                                id,
                                grab_dx,
                                grab_dy,
                                draw_x: px - grab_dx,
                                draw_y: py - grab_dy,
                            });
                        }
                        DragState::Reorder {
                            id,
                            insertion_index: _,
                            pointer_x: _,
                            pointer_y: _,
                            pointer_dx,
                            pointer_dy,
                        } => {
                            s.hovered = None;
                            s.proximity_hovered = None;

                            let dragged_center_x = px - pointer_dx;
                            let dragged_center_y = py - pointer_dy;
                            let axis_is_vertical = s.layout_mode == LayoutMode::Vertical;
                            let dragged_center = if axis_is_vertical {
                                dragged_center_y
                            } else {
                                dragged_center_x
                            };
                            let insertion_index =
                                s.compute_insertion_index(id, dragged_center, axis_is_vertical);

                            s.drag = Some(DragState::Reorder {
                                id,
                                insertion_index,
                                pointer_x: px,
                                pointer_y: py,
                                pointer_dx,
                                pointer_dy,
                            });
                        }
                        DragState::Resize {
                            id,
                            handle,
                            start_pointer_x,
                            start_pointer_y,
                            start_k,
                            start_sx,
                            start_sy,
                            base_w,
                            base_h,
                            start_x,
                            start_y,
                            start_w,
                            start_h,
                        } => {
                            let dx = px - start_pointer_x;
                            let dy = py - start_pointer_y;

                            let mut target_w = start_w;
                            let mut target_h = start_h;

                            match handle {
                                Handle::E => target_w = (start_w + dx).max(MIN_IMAGE_SIDE_PX),
                                Handle::W => target_w = (start_w - dx).max(MIN_IMAGE_SIDE_PX),
                                Handle::S => target_h = (start_h + dy).max(MIN_IMAGE_SIDE_PX),
                                Handle::N => target_h = (start_h - dy).max(MIN_IMAGE_SIDE_PX),
                                Handle::SE => {
                                    target_w = (start_w + dx).max(MIN_IMAGE_SIDE_PX);
                                    target_h = (start_h + dy).max(MIN_IMAGE_SIDE_PX);
                                }
                                Handle::SW => {
                                    target_w = (start_w - dx).max(MIN_IMAGE_SIDE_PX);
                                    target_h = (start_h + dy).max(MIN_IMAGE_SIDE_PX);
                                }
                                Handle::NE => {
                                    target_w = (start_w + dx).max(MIN_IMAGE_SIDE_PX);
                                    target_h = (start_h - dy).max(MIN_IMAGE_SIDE_PX);
                                }
                                Handle::NW => {
                                    target_w = (start_w - dx).max(MIN_IMAGE_SIDE_PX);
                                    target_h = (start_h - dy).max(MIN_IMAGE_SIDE_PX);
                                }
                            }

                            if s.keep_aspect {
                                let scale_w = target_w / base_w.max(1.0);
                                let scale_h = target_h / base_h.max(1.0);

                                let new_k = match handle {
                                    Handle::E | Handle::W => scale_w,
                                    Handle::N | Handle::S => scale_h,
                                    _ => {
                                        if dx.abs() >= dy.abs() {
                                            scale_w
                                        } else {
                                            scale_h
                                        }
                                    }
                                }
                                .clamp(0.01, 100.0);

                                if let Some(img) = s.images.iter_mut().find(|i| i.id == id) {
                                    img.k = new_k;
                                }
                            } else {
                                let new_sx = (target_w / base_w.max(1.0)).clamp(0.01, 100.0);
                                let new_sy = (target_h / base_h.max(1.0)).clamp(0.01, 100.0);

                                if let Some(img) = s.images.iter_mut().find(|i| i.id == id) {
                                    img.sx = new_sx;
                                    img.sy = new_sy;
                                    let _ = (start_k, start_sx, start_sy, start_x, start_y);
                                }
                            }

                            s.recompute_layout();
                            s.drag = Some(DragState::Resize {
                                id,
                                handle,
                                start_pointer_x,
                                start_pointer_y,
                                start_k,
                                start_sx,
                                start_sy,
                                base_w,
                                base_h,
                                start_x,
                                start_y,
                                start_w,
                                start_h,
                            });
                        }
                    }
                }
            });

            let _ = canvas;
        }
    };

    let on_pointer_down = {
        move |ev: PointerEvent| {
            let px = ev.offset_x() as f64 - PREVIEW_PADDING_PX;
            let py = ev.offset_y() as f64 - PREVIEW_PADDING_PX;

            state.update(|s| {
                s.recompute_layout();

                let Some(id) = s.hit_test_item(px, py) else {
                    s.selected = None;
                    s.drag = None;
                    return;
                };

                if s.hit_test_delete(id, px, py) {
                    s.remove_image(id);
                    return;
                }

                s.selected = Some(id);

                if let Some(h) = s.hit_test_handle(id, px, py) {
                    let (h_target, w_target) = s.compute_targets();
                    let img = s.images.iter().find(|i| i.id == id).expect("image");
                    let (base_w, base_h) = s.base_size_for(img, h_target, w_target);
                    let it = s
                        .layout
                        .items
                        .iter()
                        .find(|x| x.id == id)
                        .expect("layout item");

                    s.drag = Some(DragState::Resize {
                        id,
                        handle: h,
                        start_pointer_x: px,
                        start_pointer_y: py,
                        start_k: img.k,
                        start_sx: img.sx,
                        start_sy: img.sy,
                        base_w,
                        base_h,
                        start_x: it.x,
                        start_y: it.y,
                        start_w: it.w,
                        start_h: it.h,
                    });
                    return;
                }

                let it = s
                    .layout
                    .items
                    .iter()
                    .find(|x| x.id == id)
                    .expect("layout item");
                if s.layout_mode == LayoutMode::FreeFlow {
                    s.drag = Some(DragState::Move {
                        id,
                        grab_dx: px - it.x,
                        grab_dy: py - it.y,
                        draw_x: it.x,
                        draw_y: it.y,
                    });
                } else {
                    let center_x = it.x + it.w / 2.0;
                    let center_y = it.y + it.h / 2.0;
                    let pointer_dx = px - center_x;
                    let pointer_dy = py - center_y;
                    let axis_is_vertical = s.layout_mode == LayoutMode::Vertical;
                    let dragged_center = if axis_is_vertical { center_y } else { center_x };
                    let insertion_index =
                        s.compute_insertion_index(id, dragged_center, axis_is_vertical);
                    s.drag = Some(DragState::Reorder {
                        id,
                        insertion_index,
                        pointer_x: px,
                        pointer_y: py,
                        pointer_dx,
                        pointer_dy,
                    });
                }
            });
        }
    };

    let on_pointer_up = {
        move |_ev: PointerEvent| {
            let mut should_schedule_tick = false;
            state.update(|s| {
                match s.drag.clone() {
                    Some(DragState::Move {
                        id, draw_x, draw_y, ..
                    }) => {
                        if let Some(img) = s.images.iter_mut().find(|i| i.id == id) {
                            img.x = draw_x;
                            img.y = draw_y;
                        }
                        s.recompute_layout();
                    }
                    Some(DragState::Reorder {
                        id,
                        insertion_index,
                        ..
                    }) => {
                        let from_positions = s
                            .layout
                            .items
                            .iter()
                            .map(|it| (it.id, (it.x, it.y)))
                            .collect::<HashMap<_, _>>();
                        s.reorder_by_insertion(id, insertion_index);
                        s.transition = Some(LayoutTransition {
                            from_positions,
                            start_ms: js_sys::Date::now(),
                            duration_ms: 180.0,
                        });
                        should_schedule_tick = true;
                    }
                    Some(DragState::Resize { .. }) | None => {}
                }
                s.drag = None;
            });
            if should_schedule_tick {
                schedule_animation_tick(state);
            }
        }
    };

    // controls
    let on_toggle_aspect = {
        move |ev| {
            let checked = event_target_checked(&ev);
            state.update(|s| {
                s.keep_aspect = checked;
                // When toggling OFF -> keep existing k but also reflect into sx/sy uniformly for smoothness.
                if !s.keep_aspect {
                    for img in &mut s.images {
                        img.sx = img.k;
                        img.sy = img.k;
                    }
                } else {
                    // toggling ON: take current sx as uniform k (use average)
                    for img in &mut s.images {
                        img.k = ((img.sx + img.sy) / 2.0).max(0.01);
                    }
                }
                s.recompute_layout();
            });
        }
    };

    let on_slider = {
        move |ev| {
            let v = event_target_value(&ev).parse::<f64>().unwrap_or(0.5);
            state.update(|s| {
                s.slider = v.clamp(0.0, 1.0);
                s.recompute_layout();
            });
        }
    };

    let on_format = {
        move |ev| {
            let v = event_target_value(&ev);
            state.update(|s| {
                s.export_format = if v == "png" {
                    ExportFormat::Png
                } else {
                    ExportFormat::Jpeg
                };
            });
        }
    };

    let on_layout_mode = {
        move |ev| {
            let v = event_target_value(&ev);
            let mut should_schedule_tick = false;
            state.update(|s| {
                let new_mode = LayoutMode::from_str(&v);
                if s.layout_mode == new_mode {
                    return;
                }
                let from_positions = s
                    .layout
                    .items
                    .iter()
                    .map(|it| (it.id, (it.x, it.y)))
                    .collect::<HashMap<_, _>>();
                s.layout_mode = new_mode;
                s.recompute_layout();
                s.transition = Some(LayoutTransition {
                    from_positions,
                    start_ms: js_sys::Date::now(),
                    duration_ms: 180.0,
                });
                should_schedule_tick = true;
            });
            if should_schedule_tick {
                schedule_animation_tick(state);
            }
        }
    };

    let on_save_click = {
        move |_| {
            state.with(export_current);
        }
    };

    let on_copy_click = {
        move |_| {
            state.with(copy_current_to_clipboard);
            copy_feedback.set(true);
            set_timeout(
                move || copy_feedback.set(false),
                std::time::Duration::from_millis(700),
            );
        }
    };

    // Derived strings
    let warn = Memo::new(move |_| state.with(|s| s.layout.warn_too_large));
    let slider_label = Memo::new(move |_| {
        state.with(|s| match s.layout_mode {
            LayoutMode::Vertical => "Size (width): largest → smallest",
            LayoutMode::Line | LayoutMode::FreeFlow => "Size (height): largest → smallest",
        })
    });
    let drag_hint = Memo::new(move |_| {
        state.with(|s| match s.layout_mode {
            LayoutMode::FreeFlow => "Move: drag",
            LayoutMode::Line | LayoutMode::Vertical => "Reorder: drag",
        })
    });

    view! {
        <div class="main" on:dragover=on_drag_over on:drop=on_drop>
            <div class="topbar">
                <label class="file-picker">
                    <span class="file-picker-label">"Add images"</span>
                    <input node_ref=file_ref type="file" accept="image/*" multiple
                        on:change=on_files
                    />
                    <span class="file-picker-status">
                        {move || {
                            let count = state.with(|s| s.images.len());
                            if count == 0 { "No images".to_string() } else { format!("{} image(s)", count) }
                        }}
                    </span>
                </label>

                <label style="display:flex;align-items:center;gap:8px;">
                    <input type="checkbox"
                        prop:checked=move || state.with(|s| s.keep_aspect)
                        on:change=on_toggle_aspect
                    />
                    <span>"Keep aspect ratio"</span>
                </label>

                <label style="display:flex;flex-direction:column;gap:4px;">
                    <span class="panel-label">{move || slider_label.get()}</span>
                    <input type="range" min="0" max="1" step="0.01"
                        prop:value=move || state.with(|s| s.slider.to_string())
                        on:input=on_slider
                    />
                </label>

                <label style="display:flex;align-items:center;gap:8px;">
                    <span class="panel-label">"Layout"</span>
                    <select
                        prop:value=move || state.with(|s| s.layout_mode.as_str().to_string())
                        on:change=on_layout_mode
                    >
                        <option value="line">{LayoutMode::Line.label()}</option>
                        <option value="vertical">{LayoutMode::Vertical.label()}</option>
                        <option value="free">{LayoutMode::FreeFlow.label()}</option>
                    </select>
                </label>

                <label style="display:flex;align-items:center;gap:8px;">
                    <span class="panel-label">"Export"</span>
                    <select
                        prop:value=move || state.with(|s| if s.export_format == ExportFormat::Png { "png".to_string() } else { "jpg".to_string() })
                        on:change=on_format
                    >
                        <option value="jpg">"JPEG"</option>
                        <option value="png">"PNG"</option>
                    </select>
                </label>

                <button on:click=on_save_click>"Save"</button>
                <button class=move || if copy_feedback.get() { "copy-btn copy-btn-ok" } else { "copy-btn" } on:click=on_copy_click>
                    {move || if copy_feedback.get() { "Copied" } else { "Copy" }}
                </button>

                {move || warn.get().then(|| view! { <div class="warn">"Warning: output may exceed typical canvas limits (16384px)."</div> })}
            </div>

            <div class="canvas-wrap">
                <canvas
                    node_ref=canvas_ref
                    on:pointermove=on_pointer_move
                    on:pointerdown=on_pointer_down
                    on:pointerup=on_pointer_up
                    style="background: rgba(255,255,255,0.02); border: 1px solid rgba(128,128,128,0.35); border-radius: 10px;"
                ></canvas>
                <div class="floating-hints">
                    <div>"Paste: Ctrl/Cmd+V"</div>
                    <div>{move || drag_hint.get()}</div>
                    <div>"Resize: drag corner/side handle"</div>
                    <div>"Delete: hover image, click ×"</div>
                    <div>"Copy: Ctrl/Cmd+C"</div>
                    <div>"Save: Ctrl/Cmd+S"</div>
                </div>
            </div>
        </div>
    }
}

fn main() {
    console_error_panic_hook::set_once();
    mount_to_body(|| view! { <App /> });
}

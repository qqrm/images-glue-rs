use js_sys::Reflect;
use leptos::prelude::*;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_futures::JsFuture;
use web_sys::{
    Blob, ClipboardEvent, DragEvent, File, FileList, HtmlAnchorElement, HtmlCanvasElement,
    ImageBitmap, PointerEvent, Url, Window,
};

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

    selected: Option<u64>,
    hovered: Option<u64>,

    layout: Layout,
    drag: Option<DragState>,
}

#[derive(Clone, Debug)]
enum DragState {
    Reorder {
        id: u64,
        pointer_dx: f64, // pointer_x - center_x
        insertion_index: usize,
        pointer_x: f64,
        pointer_y: f64,
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

impl Default for AppState {
    fn default() -> Self {
        Self {
            images: Vec::new(),
            keep_aspect: true,
            slider: 0.5,
            export_format: ExportFormat::Jpeg,
            selected: None,
            hovered: None,
            layout: Layout::default(),
            drag: None,
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
    fn recompute_layout(&mut self) {
        let mut layout = Layout::default();

        if self.images.is_empty() {
            self.layout = layout;
            return;
        }

        // compute h_min/h_max from natural heights
        let mut h_min = f64::INFINITY;
        let mut h_max: f64 = 0.0;
        for img in &self.images {
            h_min = h_min.min(img.nat_h);
            h_max = h_max.max(img.nat_h);
        }
        if !h_min.is_finite() {
            h_min = 1.0;
        }

        // slider S: 0 -> max, 1 -> min
        let s = self.slider.clamp(0.0, 1.0);
        let h_target = h_max * (1.0 - s) + h_min * s;

        // sizes
        let mut x: f64 = 0.0;
        let mut out_h: f64 = 0.0;
        for img in &self.images {
            let aspect = img.nat_w / img.nat_h;
            let base_w = h_target * aspect;
            let base_h = h_target;

            let (w, h) = if self.keep_aspect {
                let k = img.k.max(0.01);
                (base_w * k, base_h * k)
            } else {
                let sx = img.sx.max(0.01);
                let sy = img.sy.max(0.01);
                (base_w * sx, base_h * sy)
            };

            let btn = {
                let pad = 4.0;
                let sz = 18.0;
                (x + w - sz - pad, 0.0 + pad, sz, sz)
            };

            layout.items.push(LayoutItem {
                id: img.id,
                x,
                y: 0.0,
                w,
                h,
                delete_btn: btn,
            });

            x += w; // gap=0
            out_h = out_h.max(h);
        }

        layout.out_w = x;
        layout.out_h = out_h;

        // Workspace should stay larger than glued content so images can be stretched to the right/bottom.
        layout.view_w = (layout.out_w + 1200.0).max(1600.0);
        layout.view_h = (layout.out_h + 900.0).max(900.0);

        // warning: typical max canvas dimension (varies by browser/GPU)
        layout.warn_too_large = layout.out_w > 16384.0 || layout.out_h > 16384.0;

        self.layout = layout;

        // clamp hovered/selected if items removed
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
    }

    fn hit_test_item(&self, px: f64, py: f64) -> Option<u64> {
        for it in &self.layout.items {
            if px >= it.x && px <= it.x + it.w && py >= it.y && py <= it.y + it.h {
                return Some(it.id);
            }
        }
        None
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
        self.drag = None;
        self.recompute_layout();
    }

    fn reorder_by_insertion(&mut self, id: u64, insertion_index: usize) {
        let idx = self.images.iter().position(|i| i.id == id);
        let Some(from) = idx else { return };
        let item = self.images.remove(from);
        let mut to = insertion_index.min(self.images.len());
        if from < to {
            // after removal, indices shift left
            to = to.saturating_sub(1);
        }
        self.images.insert(to, item);
        self.recompute_layout();
    }

    fn compute_insertion_index(&self, dragged_id: u64, dragged_center_x: f64) -> usize {
        // Compare against centers of other items in current packed layout.
        // Insertion index is the count of items whose center is < dragged_center_x.
        let mut centers: Vec<(u64, f64)> = self
            .layout
            .items
            .iter()
            .filter(|it| it.id != dragged_id)
            .map(|it| (it.id, it.x + it.w / 2.0))
            .collect();
        centers.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

        let mut idx = 0usize;
        for (_, cx) in centers {
            if dragged_center_x > cx {
                idx += 1;
            } else {
                break;
            }
        }
        idx
    }
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
    // Preview canvas is intentionally larger than content bounds.
    let w = state.layout.view_w.max(1.0);
    let h = state.layout.view_h.max(1.0);

    let Some((ctx, _dpr)) = set_canvas_size(canvas, w, h) else {
        return;
    };

    // Background (for preview): dark behind; canvas is actual output area.
    // For JPEG export we will draw white, but preview remains neutral.
    ctx.clear_rect(0.0, 0.0, w, h);

    // draw images
    for it in &state.layout.items {
        if let Some(img) = state.images.iter().find(|i| i.id == it.id) {
            // draw the bitmap
            let _ =
                ctx.draw_image_with_image_bitmap_and_dw_and_dh(&img.bitmap, it.x, it.y, it.w, it.h);
        }
        // outline hover/selected
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

            ctx.stroke_rect(it.x + 0.5, it.y + 0.5, it.w - 1.0, it.h - 1.0);
            ctx.restore();
        }

        // delete button on hover
        if is_hov {
            let (bx, by, bw, bh) = it.delete_btn;
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

    // draw handles for selected (or hovered if nothing selected)
    let handle_owner = state.selected.or(state.hovered);
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
            let _ = ctx.arc(hx, hy, r, 0.0, std::f64::consts::TAU);
            ctx.close_path();
            ctx.fill();
            ctx.stroke();
        }

        ctx.restore();
    }

    // insertion marker while reordering
    if let Some(DragState::Reorder {
        id,
        insertion_index,
        pointer_x,
        pointer_y,
        ..
    }) = &state.drag
    {
        if let Some(it) = state.layout.items.iter().find(|x| x.id == *id)
            && let Some(dragged) = state.images.iter().find(|img| img.id == *id)
        {
            let cx = *pointer_x - it.w / 2.0;
            let cy = *pointer_y - it.h / 2.0;
            ctx.save();
            Reflect::set(
                ctx.as_ref(),
                &JsValue::from_str("globalAlpha"),
                &JsValue::from_f64(0.65),
            )
            .unwrap();
            let _ = ctx.draw_image_with_image_bitmap_and_dw_and_dh(
                &dragged.bitmap,
                cx,
                cy,
                it.w,
                it.h,
            );
            ctx.restore();

            ctx.save();
            Reflect::set(
                ctx.as_ref(),
                &JsValue::from_str("strokeStyle"),
                &JsValue::from_str("rgba(255,255,255,0.75)"),
            )
            .unwrap();
            ctx.set_line_width(1.0);
            ctx.stroke_rect(cx + 0.5, cy + 0.5, it.w - 1.0, it.h - 1.0);
            ctx.begin_path();
            ctx.move_to(*pointer_x - 6.0, *pointer_y);
            ctx.line_to(*pointer_x + 6.0, *pointer_y);
            ctx.move_to(*pointer_x, *pointer_y - 6.0);
            ctx.line_to(*pointer_x, *pointer_y + 6.0);
            ctx.stroke();
            ctx.restore();
        }

        let mut x: f64 = 0.0;
        let mut idx = 0usize;
        for it in &state.layout.items {
            if it.id == *id {
                continue;
            }
            if idx == *insertion_index {
                break;
            }
            x = it.x + it.w;
            idx += 1;
        }
        ctx.save();

        Reflect::set(
            ctx.as_ref(),
            &JsValue::from_str("strokeStyle"),
            &JsValue::from_str("rgba(255,255,255,0.9)"),
        )
        .unwrap();

        ctx.set_line_width(2.0);
        ctx.begin_path();
        ctx.move_to(x + 0.5, 0.0);
        ctx.line_to(x + 0.5, h);
        ctx.stroke();
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
            // offsetX/Y in CSS px == logical units (we render in logical units).
            let px = ev.offset_x() as f64;
            let py = ev.offset_y() as f64;

            state.update(|s| {
                // update hover
                s.hovered = s.hit_test_item(px, py);

                // update drag
                if let Some(d) = s.drag.clone() {
                    match d {
                        DragState::Reorder { id, pointer_dx, .. } => {
                            // desired center X follows pointer
                            let center_x = px - pointer_dx;
                            let insertion = s.compute_insertion_index(id, center_x);
                            s.drag = Some(DragState::Reorder {
                                id,
                                pointer_dx,
                                insertion_index: insertion,
                                pointer_x: px,
                                pointer_y: py,
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

                            // derive target w/h from handle movement, using simple rules.
                            // We keep layout packed; resizing only changes multipliers.
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

                                // choose dominant axis for corners, axis-specific for sides
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
                                    // keep stored for completeness
                                    let _ = (start_sx, start_sy, start_x, start_y);
                                }
                            }

                            s.recompute_layout();
                            // keep drag state live (with updated start box is not necessary for MVP)
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

            // request redraw by touching state; effect above will run.
            let _ = canvas; // keep owned
        }
    };

    let on_pointer_down = {
        move |ev: PointerEvent| {
            let px = ev.offset_x() as f64;
            let py = ev.offset_y() as f64;

            state.update(|s| {
                s.recompute_layout();

                let Some(id) = s.hit_test_item(px, py) else {
                    s.selected = None;
                    s.drag = None;
                    return;
                };

                // delete click
                if s.hit_test_delete(id, px, py) {
                    s.remove_image(id);
                    return;
                }

                s.selected = Some(id);

                // resize handle?
                if let Some(h) = s.hit_test_handle(id, px, py) {
                    // compute base sizes for this id at current slider
                    let mut h_min = f64::INFINITY;
                    let mut h_max: f64 = 0.0;
                    for img in &s.images {
                        h_min = h_min.min(img.nat_h);
                        h_max = h_max.max(img.nat_h);
                    }
                    if !h_min.is_finite() {
                        h_min = 1.0;
                    }
                    let ss = s.slider.clamp(0.0, 1.0);
                    let h_target = h_max * (1.0 - ss) + h_min * ss;

                    let img = s.images.iter().find(|i| i.id == id).unwrap();
                    let aspect = img.nat_w / img.nat_h;
                    let base_w = h_target * aspect;
                    let base_h = h_target;

                    let it = s.layout.items.iter().find(|x| x.id == id).unwrap();

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

                // reorder drag: keep center under pointer with offset
                let it = s.layout.items.iter().find(|x| x.id == id).unwrap();
                let center_x = it.x + it.w / 2.0;
                let pointer_dx = px - center_x;
                let insertion = s.compute_insertion_index(id, center_x);
                s.drag = Some(DragState::Reorder {
                    id,
                    pointer_dx,
                    insertion_index: insertion,
                    pointer_x: px,
                    pointer_y: py,
                });
            });
        }
    };

    let on_pointer_up = {
        move |_ev: PointerEvent| {
            state.update(|s| {
                if let Some(DragState::Reorder {
                    id,
                    insertion_index,
                    ..
                }) = s.drag.clone()
                {
                    s.reorder_by_insertion(id, insertion_index);
                }
                s.drag = None;
            });
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

    let on_save_click = {
        move |_| {
            state.with(export_current);
        }
    };

    let on_copy_click = {
        move |_| {
            state.with(copy_current_to_clipboard);
        }
    };

    // Derived strings
    let warn = Memo::new(move |_| state.with(|s| s.layout.warn_too_large));

    view! {
        <div class="main" on:dragover=on_drag_over on:drop=on_drop>
            <div class="topbar">
                <label>
                    <span class="panel-label">"Add"</span>
                    <input node_ref=file_ref type="file" accept="image/*" multiple
                        on:change=on_files
                        style="margin-left:8px;"
                    />
                </label>

                <label style="display:flex;align-items:center;gap:8px;">
                    <input type="checkbox"
                        prop:checked=move || state.with(|s| s.keep_aspect)
                        on:change=on_toggle_aspect
                    />
                    <span>"Keep aspect ratio"</span>
                </label>

                <label style="display:flex;flex-direction:column;gap:4px;">
                    <span class="panel-label">"Size: largest → smallest"</span>
                    <input type="range" min="0" max="1" step="0.01"
                        prop:value=move || state.with(|s| s.slider.to_string())
                        on:input=on_slider
                    />
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

                <button on:click=on_save_click>"Save (Ctrl+S / Ctrl+Shift+S)"</button>
                <button on:click=on_copy_click>"Copy image (Ctrl+C)"</button>

                <div class="hint">"Paste with Ctrl/Cmd+V. Drag to reorder. Use corner handles to stretch right/down. Hover × to delete."</div>
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
            </div>
        </div>
    }
}

fn main() {
    console_error_panic_hook::set_once();
    mount_to_body(|| view! { <App /> });
}

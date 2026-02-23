pub mod style;
pub mod event;
pub mod widget;
pub mod renderer;
pub mod layout;
pub mod text_render;
pub mod wgpu_renderer;
pub mod terminal_renderer;
pub mod canvas;
pub mod app;
pub mod binding;

use std::cell::RefCell;
use std::io::Write;
use std::os::raw::c_char;

use app::{AppConfig, Backend, UiState};
use event::{CallbackEntry, CallbackFn, CallbackKind};
use style::{Alignment, Color, Edges, Justification};
use widget::{CanvasCommand, Widget, WidgetKind};

thread_local! {
    static STATE: RefCell<Option<UiState>> = RefCell::new(None);
    static BINDINGS: RefCell<binding::BindingRegistry> = RefCell::new(binding::BindingRegistry::new());
}

fn with_state<R>(f: impl FnOnce(&mut UiState) -> R) -> R {
    STATE.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let state = borrow.as_mut().expect("UI not initialized; call ui_app_create first");
        f(state)
    })
}

unsafe fn cstr_to_string(s: *const c_char) -> String {
    if s.is_null() {
        return String::new();
    }
    std::ffi::CStr::from_ptr(s)
        .to_str()
        .unwrap_or("")
        .to_string()
}

fn string_to_cstr(s: &str) -> *mut c_char {
    let bytes = s.as_bytes();
    unsafe {
        let ptr = libc::malloc(bytes.len() + 1) as *mut c_char;
        if !ptr.is_null() {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr as *mut u8, bytes.len());
            *ptr.add(bytes.len()) = 0;
        }
        ptr
    }
}

fn ui_debug_log(location: &str, message: &str, data: &str) {
    let path = std::env::var("NEX_UI_DEBUG_LOG").unwrap_or_else(|_| "debug-ui.log".to_string());
    if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(path) {
        let _ = writeln!(f, "[ui][{location}] {message} | {data}");
    }
}

// ---------------------------------------------------------------------------
// Application lifecycle
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_app_create(
    title: *const c_char,
    width: i64,
    height: i64,
) -> i64 {
    let config = AppConfig {
        title: cstr_to_string(title),
        width: width.max(1) as u32,
        height: height.max(1) as u32,
        backend: Backend::Wgpu,
    };
    STATE.with(|cell| {
        *cell.borrow_mut() = Some(UiState::new(config));
    });
    1
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_app_set_backend(app: i64, backend: i64) {
    let _ = app;
    with_state(|s| {
        s.config.backend = Backend::from_i64(backend);
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_app_set_root(app: i64, root: i64) {
    let _ = app;
    with_state(|s| {
        s.root = Some(root);
        s.mark_dirty();
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_app_run(app: i64) {
    let _ = app;
    let state = STATE.with(|cell| cell.borrow_mut().take());
    let Some(state) = state else { return };

    match state.config.backend {
        Backend::Wgpu => {
            app::run_app_blocking(state);
        }
        Backend::Terminal => {
            let mut state = state;
            app::run_terminal_app(&mut state);
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_app_quit(app: i64) {
    let _ = app;
    with_state(|s| {
        s.running = false;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_app_destroy(app: i64) {
    let _ = app;
    STATE.with(|cell| {
        *cell.borrow_mut() = None;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_app_is_running(app: i64) -> i64 {
    let _ = app;
    STATE.with(|cell| {
        cell.borrow()
            .as_ref()
            .map(|s| if s.running { 1i64 } else { 0 })
            .unwrap_or(0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_app_render(app: i64) {
    let _ = app;
    with_state(|s| {
        s.render_frame();
    });
}

// ---------------------------------------------------------------------------
// Event polling
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_poll_event(app: i64) -> i64 {
    let _ = app;
    with_state(|s| {
        let evt = s.events.poll();
        evt.event_type as i64
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_event_type(event: i64) -> i64 {
    event
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_event_widget(_event: i64) -> i64 {
    with_state(|s| {
        let evt = s.events.poll();
        evt.widget_id
    })
}

// ---------------------------------------------------------------------------
// Widget creation
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_text(text: *const c_char) -> i64 {
    let txt = cstr_to_string(text);
    with_state(|s| {
        let w = Widget::new(WidgetKind::Text).with_text(&txt);
        s.widgets.alloc(w)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_button(label: *const c_char) -> i64 {
    let txt = cstr_to_string(label);
    with_state(|s| {
        let w = Widget::new(WidgetKind::Button).with_text(&txt);
        s.widgets.alloc(w)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_text_input(placeholder: *const c_char) -> i64 {
    let ph = cstr_to_string(placeholder);
    with_state(|s| {
        let mut w = Widget::new(WidgetKind::TextInput);
        w.placeholder = ph;
        s.widgets.alloc(w)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_image(path: *const c_char) -> i64 {
    let p = cstr_to_string(path);
    with_state(|s| {
        let mut w = Widget::new(WidgetKind::Image);
        w.image_path = p;
        s.widgets.alloc(w)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_checkbox(label: *const c_char) -> i64 {
    let txt = cstr_to_string(label);
    with_state(|s| {
        let w = Widget::new(WidgetKind::Checkbox).with_text(&txt);
        s.widgets.alloc(w)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_slider(min_bits: i64, max_bits: i64) -> i64 {
    let min_val = f64::from_bits(min_bits as u64) as f32;
    let max_val = f64::from_bits(max_bits as u64) as f32;
    with_state(|s| {
        let mut w = Widget::new(WidgetKind::Slider);
        w.slider_min = min_val;
        w.slider_max = max_val;
        w.slider_value = min_val;
        s.widgets.alloc(w)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_row() -> i64 {
    with_state(|s| s.widgets.alloc(Widget::new(WidgetKind::Row)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_column() -> i64 {
    with_state(|s| s.widgets.alloc(Widget::new(WidgetKind::Column)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_stack() -> i64 {
    with_state(|s| s.widgets.alloc(Widget::new(WidgetKind::Stack)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_scroll() -> i64 {
    with_state(|s| s.widgets.alloc(Widget::new(WidgetKind::Scroll)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_grid(cols: i64) -> i64 {
    with_state(|s| {
        let mut w = Widget::new(WidgetKind::Grid);
        w.grid_cols = cols.max(1) as i32;
        s.widgets.alloc(w)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_canvas(width: i64, height: i64) -> i64 {
    with_state(|s| {
        let mut w = Widget::new(WidgetKind::Canvas);
        w.style.width = Some(width as f32);
        w.style.height = Some(height as f32);
        s.widgets.alloc(w)
    })
}

// ---------------------------------------------------------------------------
// Widget tree
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_add_child(parent: i64, child: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(parent) {
            if !w.children.contains(&child) {
                w.children.push(child);
            }
        }
        if let Some(c) = s.widgets.get_mut(child) {
            c.parent = Some(parent);
        }
        s.mark_dirty();
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_remove_child(parent: i64, child: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(parent) {
            w.children.retain(|&c| c != child);
        }
        if let Some(c) = s.widgets.get_mut(child) {
            c.parent = None;
        }
        s.mark_dirty();
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_id(widget: i64, id: *const c_char) {
    let id_str = cstr_to_string(id);
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.id_str = id_str;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_get_id(widget: i64) -> *mut c_char {
    with_state(|s| {
        s.widgets
            .get(widget)
            .map(|w| string_to_cstr(&w.id_str))
            .unwrap_or(string_to_cstr(""))
    })
}

// ---------------------------------------------------------------------------
// Widget properties
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_text(widget: i64, text: *const c_char) {
    let txt = cstr_to_string(text);
    // #region agent log
    { use std::io::Write; let path = std::path::Path::new(r"d:\Development\Personal\Nex\debug-8a9f05.log"); if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(path) { let _ = writeln!(f, r#"{{"sessionId":"8a9f05","hypothesisId":"H2","location":"nex_ui_set_text","message":"setting","data":{{"widget":{},"text":"{}","len":{}}}}}"#, widget, txt.replace('"',"\\\"").chars().take(60).collect::<String>(), txt.len()); } }
    // #endregion
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.text = txt;
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_get_text(widget: i64) -> *mut c_char {
    let (text, ptr) = with_state(|s| {
        let t = s.widgets.get(widget).map(|w| w.text.clone()).unwrap_or_default();
        let p = string_to_cstr(&t);
        (t, p)
    });
    // #region agent log
    { use std::io::Write; let path = std::path::Path::new(r"d:\Development\Personal\Nex\debug-8a9f05.log"); if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(path) { let _ = writeln!(f, r#"{{"sessionId":"8a9f05","hypothesisId":"H1","location":"nex_ui_get_text","message":"returning","data":{{"widget":{},"text":"{}"}}}}"#, widget, text.replace('"',"\\\"").chars().take(60).collect::<String>()); } }
    // #endregion
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_visible(widget: i64, visible: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.visible = visible != 0;
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_enabled(widget: i64, enabled: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.enabled = enabled != 0;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_get_value_float(widget: i64) -> i64 {
    with_state(|s| {
        s.widgets.get(widget).map(|w| {
            let val = match w.kind {
                WidgetKind::Slider => w.slider_value as f64,
                WidgetKind::Checkbox => if w.checked { 1.0 } else { 0.0 },
                _ => 0.0,
            };
            val.to_bits() as i64
        }).unwrap_or(0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_value_float(widget: i64, val_bits: i64) {
    let val = f64::from_bits(val_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            match w.kind {
                WidgetKind::Slider => {
                    w.slider_value = val.clamp(w.slider_min, w.slider_max);
                }
                WidgetKind::Checkbox => {
                    w.checked = val > 0.5;
                }
                _ => {}
            }
        }
    });
}

// ---------------------------------------------------------------------------
// Styling
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_width(widget: i64, width_bits: i64) {
    let w = f64::from_bits(width_bits as u64) as f32;
    with_state(|s| {
        if let Some(wid) = s.widgets.get_mut(widget) {
            wid.style.width = Some(w);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_height(widget: i64, height_bits: i64) {
    let h = f64::from_bits(height_bits as u64) as f32;
    with_state(|s| {
        if let Some(wid) = s.widgets.get_mut(widget) {
            wid.style.height = Some(h);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_min_width(widget: i64, val_bits: i64) {
    let v = f64::from_bits(val_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.min_width = Some(v);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_min_height(widget: i64, val_bits: i64) {
    let v = f64::from_bits(val_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.min_height = Some(v);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_padding(widget: i64, packed: i64) {
    let edges = Edges::from_packed(packed);
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.padding = edges;
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_padding_all(widget: i64, value_bits: i64) {
    let v = f64::from_bits(value_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.padding = Edges::all(v);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_margin(widget: i64, packed: i64) {
    let edges = Edges::from_packed(packed);
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.margin = edges;
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_bg_color(widget: i64, rgba: i64) {
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.bg_color = color;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_fg_color(widget: i64, rgba: i64) {
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.fg_color = color;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_font_size(widget: i64, size_bits: i64) {
    let size = f64::from_bits(size_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.font_size = size;
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_border(widget: i64, width_bits: i64, rgba: i64) {
    let width = f64::from_bits(width_bits as u64) as f32;
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.border_width = width;
            w.style.border_color = color;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_border_radius(widget: i64, radius_bits: i64) {
    let r = f64::from_bits(radius_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.border_radius = r;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_flex_grow(widget: i64, grow_bits: i64) {
    let g = f64::from_bits(grow_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.flex_grow = g;
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_align_self(widget: i64, align: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.align_self = Some(Alignment::from_i64(align));
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_h_align(widget: i64, align: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.h_align = Some(Alignment::from_i64(align));
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_v_align(widget: i64, align: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.v_align = Some(Alignment::from_i64(align));
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_justify_content(widget: i64, justify: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.justify_content = Justification::from_i64(justify);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_align_items(widget: i64, align: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.align_items = Alignment::from_i64(align);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_gap(widget: i64, gap_bits: i64) {
    let g = f64::from_bits(gap_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.gap = g;
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_max_width(widget: i64, val_bits: i64) {
    let v = f64::from_bits(val_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.max_width = Some(v);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_max_height(widget: i64, val_bits: i64) {
    let v = f64::from_bits(val_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.max_height = Some(v);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_flex_shrink(widget: i64, val_bits: i64) {
    let v = f64::from_bits(val_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.flex_shrink = v;
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_margin_all(widget: i64, value_bits: i64) {
    let v = f64::from_bits(value_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.margin = Edges::all(v);
            s.needs_layout = true;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_checked(widget: i64, checked: i64) {
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.checked = checked != 0;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_border_width(widget: i64, width_bits: i64) {
    let width = f64::from_bits(width_bits as u64) as f32;
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.border_width = width;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_set_border_color(widget: i64, rgba: i64) {
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(w) = s.widgets.get_mut(widget) {
            w.style.border_color = color;
        }
    });
}

// ---------------------------------------------------------------------------
// Event callbacks
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_on_click(widget: i64, callback: i64) {
    let func_ptr: CallbackFn = std::mem::transmute(callback);
    ui_debug_log(
        "nex_ui_on_click",
        "register callback",
        &format!("widget={}, callback=0x{:x}", widget, callback as usize),
    );
    with_state(|s| {
        s.callbacks.push(CallbackEntry {
            kind: CallbackKind::Click,
            widget_id: widget,
            func_ptr,
        });
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_on_change(widget: i64, callback: i64) {
    let func_ptr: CallbackFn = std::mem::transmute(callback);
    ui_debug_log(
        "nex_ui_on_change",
        "register callback",
        &format!("widget={}, callback=0x{:x}", widget, callback as usize),
    );
    with_state(|s| {
        s.callbacks.push(CallbackEntry {
            kind: CallbackKind::Change,
            widget_id: widget,
            func_ptr,
        });
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_on_hover(widget: i64, callback: i64) {
    let func_ptr: CallbackFn = std::mem::transmute(callback);
    ui_debug_log(
        "nex_ui_on_hover",
        "register callback",
        &format!("widget={}, callback=0x{:x}", widget, callback as usize),
    );
    with_state(|s| {
        s.callbacks.push(CallbackEntry {
            kind: CallbackKind::Hover,
            widget_id: widget,
            func_ptr,
        });
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_on_key(widget: i64, callback: i64) {
    let func_ptr: CallbackFn = std::mem::transmute(callback);
    ui_debug_log(
        "nex_ui_on_key",
        "register callback",
        &format!("widget={}, callback=0x{:x}", widget, callback as usize),
    );
    with_state(|s| {
        s.callbacks.push(CallbackEntry {
            kind: CallbackKind::Key,
            widget_id: widget,
            func_ptr,
        });
    });
}

// ---------------------------------------------------------------------------
// Canvas drawing
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_canvas_fill_rect(
    widget: i64, x_bits: i64, y_bits: i64, w_bits: i64, h_bits: i64, rgba: i64,
) {
    let x = f64::from_bits(x_bits as u64) as f32;
    let y = f64::from_bits(y_bits as u64) as f32;
    let w = f64::from_bits(w_bits as u64) as f32;
    let h = f64::from_bits(h_bits as u64) as f32;
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(wid) = s.widgets.get_mut(widget) {
            wid.canvas_commands.push(CanvasCommand::FillRect { x, y, w, h, color });
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_canvas_stroke_rect(
    widget: i64, x_bits: i64, y_bits: i64, w_bits: i64, h_bits: i64, lw_bits: i64, rgba: i64,
) {
    let x = f64::from_bits(x_bits as u64) as f32;
    let y = f64::from_bits(y_bits as u64) as f32;
    let w = f64::from_bits(w_bits as u64) as f32;
    let h = f64::from_bits(h_bits as u64) as f32;
    let lw = f64::from_bits(lw_bits as u64) as f32;
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(wid) = s.widgets.get_mut(widget) {
            wid.canvas_commands.push(CanvasCommand::StrokeRect { x, y, w, h, line_width: lw, color });
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_canvas_fill_circle(
    widget: i64, cx_bits: i64, cy_bits: i64, r_bits: i64, rgba: i64,
) {
    let cx = f64::from_bits(cx_bits as u64) as f32;
    let cy = f64::from_bits(cy_bits as u64) as f32;
    let radius = f64::from_bits(r_bits as u64) as f32;
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(wid) = s.widgets.get_mut(widget) {
            wid.canvas_commands.push(CanvasCommand::FillCircle { cx, cy, radius, color });
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_canvas_draw_line(
    widget: i64, x1_bits: i64, y1_bits: i64, x2_bits: i64, y2_bits: i64, lw_bits: i64, rgba: i64,
) {
    let x1 = f64::from_bits(x1_bits as u64) as f32;
    let y1 = f64::from_bits(y1_bits as u64) as f32;
    let x2 = f64::from_bits(x2_bits as u64) as f32;
    let y2 = f64::from_bits(y2_bits as u64) as f32;
    let lw = f64::from_bits(lw_bits as u64) as f32;
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(wid) = s.widgets.get_mut(widget) {
            wid.canvas_commands.push(CanvasCommand::DrawLine { x1, y1, x2, y2, line_width: lw, color });
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_canvas_draw_text(
    widget: i64, text: *const c_char, x_bits: i64, y_bits: i64, size_bits: i64, rgba: i64,
) {
    let txt = cstr_to_string(text);
    let x = f64::from_bits(x_bits as u64) as f32;
    let y = f64::from_bits(y_bits as u64) as f32;
    let size = f64::from_bits(size_bits as u64) as f32;
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(wid) = s.widgets.get_mut(widget) {
            wid.canvas_commands.push(CanvasCommand::DrawText { text: txt, x, y, size, color });
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_canvas_clear(widget: i64, rgba: i64) {
    let color = Color::from_packed(rgba);
    with_state(|s| {
        if let Some(wid) = s.widgets.get_mut(widget) {
            wid.canvas_commands.clear();
            wid.canvas_commands.push(CanvasCommand::Clear { color });
        }
    });
}

// ---------------------------------------------------------------------------
// Dialogs (simple implementations using println for now)
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_dialog_message(title: *const c_char, message: *const c_char) {
    let t = cstr_to_string(title);
    let m = cstr_to_string(message);
    eprintln!("[Dialog: {}] {}", t, m);
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_dialog_confirm(title: *const c_char, message: *const c_char) -> i64 {
    let t = cstr_to_string(title);
    let m = cstr_to_string(message);
    eprintln!("[Confirm: {}] {} (returning true)", t, m);
    1
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_dialog_open_file(title: *const c_char, _filter: *const c_char) -> *mut c_char {
    let t = cstr_to_string(title);
    eprintln!("[OpenFile: {}] (returning empty)", t);
    string_to_cstr("")
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_dialog_save_file(title: *const c_char, _filter: *const c_char) -> *mut c_char {
    let t = cstr_to_string(title);
    eprintln!("[SaveFile: {}] (returning empty)", t);
    string_to_cstr("")
}

// ---------------------------------------------------------------------------
// MVVM Binding Engine FFI
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_ui_bind(property: *const c_char, callback: binding::BindingCallback) {
    let prop = cstr_to_string(property);
    BINDINGS.with(|cell| {
        cell.borrow_mut().bind(&prop, callback);
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_unbind(property: *const c_char) {
    let prop = cstr_to_string(property);
    BINDINGS.with(|cell| {
        cell.borrow_mut().unbind(&prop);
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_notify_changed(property: *const c_char) {
    let prop = cstr_to_string(property);
    BINDINGS.with(|cell| {
        cell.borrow().notify(&prop);
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_ui_bindings_clear() {
    BINDINGS.with(|cell| {
        cell.borrow_mut().clear();
    });
}

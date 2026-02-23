use std::sync::Arc;
use std::{io::Write, panic::AssertUnwindSafe};

use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::{ElementState, MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowAttributes, WindowId};

use super::canvas;
use super::event::{CallbackEntry, CallbackFn, CallbackKind, EventQueue, EventType, UiEvent};
use super::layout::LayoutEngine;
use super::renderer::{Rect, Renderer};
use super::style::Color;
use super::terminal_renderer::TerminalRenderer;
use super::wgpu_renderer::WgpuRenderer;
use super::widget::{WidgetArena, WidgetKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Backend {
    Wgpu = 0,
    Terminal = 1,
}

impl Backend {
    pub fn from_i64(v: i64) -> Self {
        match v {
            1 => Self::Terminal,
            _ => Self::Wgpu,
        }
    }
}

pub struct AppConfig {
    pub title: String,
    pub width: u32,
    pub height: u32,
    pub backend: Backend,
}

enum ActiveRenderer {
    Wgpu(WgpuRenderer),
    Terminal(TerminalRenderer),
    None,
}

pub struct UiState {
    pub config: AppConfig,
    pub widgets: WidgetArena,
    pub layout: LayoutEngine,
    pub events: EventQueue,
    pub callbacks: Vec<CallbackEntry>,
    pub root: Option<i64>,
    pub running: bool,
    pub needs_layout: bool,
    renderer: ActiveRenderer,
    window: Option<Arc<Window>>,
    mouse_x: f32,
    mouse_y: f32,
    hovered_widget: Option<i64>,
}

struct PendingCallback {
    widget_id: i64,
    kind: CallbackKind,
    func_ptr: CallbackFn,
}

fn ui_debug_log(location: &str, message: &str, data: &str) {
    let path = std::env::var("NEX_UI_DEBUG_LOG").unwrap_or_else(|_| "debug-ui.log".to_string());
    if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(path) {
        let _ = writeln!(f, "[ui][{location}] {message} | {data}");
    }
}

impl UiState {
    pub fn new(config: AppConfig) -> Self {
        Self {
            config,
            widgets: WidgetArena::new(),
            layout: LayoutEngine::new(),
            events: EventQueue::new(),
            callbacks: Vec::new(),
            root: None,
            running: true,
            needs_layout: true,
            renderer: ActiveRenderer::None,
            window: None,
            mouse_x: 0.0,
            mouse_y: 0.0,
            hovered_widget: None,
        }
    }

    pub fn mark_dirty(&mut self) {
        self.needs_layout = true;
    }

    pub fn recompute_layout(&mut self) {
        if let Some(root) = self.root {
            let (w, h) = match &self.window {
                Some(win) => {
                    let size = win.inner_size();
                    (size.width as f32, size.height as f32)
                }
                None => (self.config.width as f32, self.config.height as f32),
            };
            self.layout.compute(root, &self.widgets, w, h);
            self.needs_layout = false;
        }
    }

    pub fn render_frame(&mut self) {
        if self.needs_layout {
            self.recompute_layout();
        }

        let (width, height) = match &self.window {
            Some(win) => {
                let s = win.inner_size();
                (s.width, s.height)
            }
            None => (self.config.width, self.config.height),
        };

        if width == 0 || height == 0 {
            return;
        }

        match &mut self.renderer {
            ActiveRenderer::Wgpu(r) => {
                r.begin_frame(width, height);
                r.clear(Color::from_rgba(30, 30, 46, 255));
                if let Some(root) = self.root {
                    Self::render_widget_gpu(root, &self.widgets, &self.layout, r);
                }
                r.end_frame();
                r.present();
            }
            ActiveRenderer::Terminal(r) => {
                if let Some(root) = self.root {
                    r.render_widget_tree(root, &self.widgets, &self.layout);
                }
            }
            ActiveRenderer::None => {}
        }
    }

    fn render_widget_gpu(
        handle: i64,
        arena: &WidgetArena,
        layout: &LayoutEngine,
        renderer: &mut WgpuRenderer,
    ) {
        let Some(widget) = arena.get(handle) else { return };
        if !widget.style.visible {
            return;
        }
        let Some(rect) = layout.get_rect(handle) else { return };
        let s = &widget.style;

        if s.bg_color.a > 0 {
            let bg = if widget.kind == WidgetKind::Button && widget.pressed {
                Color::PRESSED_BLUE
            } else if widget.kind == WidgetKind::Button && widget.hovered {
                Color::HOVER_BLUE
            } else {
                s.bg_color
            };
            renderer.fill_rect(rect, bg, s.border_radius);
        }

        if s.border_width > 0.0 && s.border_color.a > 0 {
            let border_color = if widget.focused {
                Color::BLUE
            } else {
                s.border_color
            };
            renderer.stroke_rect(rect, border_color, s.border_width, s.border_radius);
        }

        match &widget.kind {
            WidgetKind::Text => {
                let text_x = rect.x + s.padding.left;
                let text_y = rect.y + s.padding.top;
                renderer.draw_text(&widget.text, text_x, text_y, s.font_size, s.fg_color);
            }
            WidgetKind::Button => {
                let text_x = rect.x + s.padding.left;
                let text_y = rect.y + s.padding.top;
                renderer.draw_text(&widget.text, text_x, text_y, s.font_size, s.fg_color);
            }
            WidgetKind::TextInput => {
                let display = if widget.text.is_empty() {
                    &widget.placeholder
                } else {
                    &widget.text
                };
                let color = if widget.text.is_empty() {
                    Color::GRAY
                } else {
                    s.fg_color
                };
                let text_x = rect.x + s.padding.left;
                let text_y = rect.y + s.padding.top;
                renderer.draw_text(display, text_x, text_y, s.font_size, color);

                if widget.focused {
                    let blink = (std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap_or_default()
                        .as_millis() % 1060) < 530;
                    if blink {
                        let caret_x = text_x + renderer.measure_text_width(&widget.text, s.font_size);
                        let caret_y = text_y;
                        let caret_h = s.font_size;
                        let caret_rect = Rect::new(caret_x, caret_y, 2.0, caret_h);
                        renderer.fill_rect(caret_rect, s.fg_color, 0.0);
                    }
                }
            }
            WidgetKind::Checkbox => {
                let box_size: f32 = 20.0;
                let box_y = rect.y + (rect.h - box_size) / 2.0;
                let box_rect = Rect::new(rect.x, box_y, box_size, box_size);
                renderer.stroke_rect(box_rect, s.border_color, s.border_width, s.border_radius);
                if widget.checked {
                    let inset = box_size * 0.25;
                    let check_rect = Rect::new(
                        rect.x + inset,
                        box_y + inset,
                        box_size - inset * 2.0,
                        box_size - inset * 2.0,
                    );
                    renderer.fill_rect(check_rect, Color::BLUE, 2.0);
                }
                if !widget.text.is_empty() {
                    renderer.draw_text(
                        &widget.text,
                        rect.x + box_size + 6.0,
                        rect.y + (rect.h - s.font_size) / 2.0,
                        s.font_size,
                        s.fg_color,
                    );
                }
            }
            WidgetKind::Slider => {
                let track_h = 4.0;
                let track_y = rect.y + (rect.h - track_h) / 2.0;
                renderer.fill_rect(
                    Rect::new(rect.x, track_y, rect.w, track_h),
                    Color::GRAY,
                    2.0,
                );
                let ratio = if widget.slider_max > widget.slider_min {
                    ((widget.slider_value - widget.slider_min) / (widget.slider_max - widget.slider_min)).clamp(0.0, 1.0)
                } else {
                    0.0
                };
                let filled_w = rect.w * ratio;
                renderer.fill_rect(
                    Rect::new(rect.x, track_y, filled_w, track_h),
                    Color::BLUE,
                    2.0,
                );
                let thumb_r = 8.0;
                let thumb_x = rect.x + filled_w;
                let thumb_y = rect.y + rect.h / 2.0;
                renderer.fill_circle(thumb_x, thumb_y, thumb_r, Color::WHITE);
            }
            WidgetKind::Canvas => {
                canvas::execute_canvas_commands(&widget.canvas_commands, rect, renderer);
            }
            _ => {}
        }

        let children = widget.children.clone();
        for child_handle in children {
            Self::render_widget_gpu(child_handle, arena, layout, renderer);
        }
    }

    fn handle_mouse_move(&mut self, x: f32, y: f32) {
        self.mouse_x = x;
        self.mouse_y = y;

        let hit = self.layout.hit_test(x, y, &self.widgets);

        if hit != self.hovered_widget {
            if let Some(prev) = self.hovered_widget {
                if let Some(w) = self.widgets.get_mut(prev) {
                    w.hovered = false;
                }
                self.events.push(UiEvent {
                    event_type: EventType::HoverEnd,
                    widget_id: prev,
                    x,
                    y,
                    ..Default::default()
                });
            }
            if let Some(next) = hit {
                if let Some(w) = self.widgets.get_mut(next) {
                    w.hovered = true;
                }
                self.events.push(UiEvent::hover(next, x, y));
            }
            self.hovered_widget = hit;
        }
    }

    fn handle_mouse_down(&mut self, x: f32, y: f32) {
        let hit = self.layout.hit_test(x, y, &self.widgets);
        if let Some(id) = hit {
            if let Some(w) = self.widgets.get_mut(id) {
                w.pressed = true;
            }
            self.events.push(UiEvent {
                event_type: EventType::MouseDown,
                widget_id: id,
                x,
                y,
                ..Default::default()
            });
        }
    }

    fn handle_mouse_up(&mut self, x: f32, y: f32) -> Vec<PendingCallback> {
        let hit = self.layout.hit_test(x, y, &self.widgets);

        let pressed_widgets: Vec<i64> = self.widgets.iter()
            .filter(|(_, w)| w.pressed)
            .map(|(h, _)| h)
            .collect();

        for h in &pressed_widgets {
            if let Some(w) = self.widgets.get_mut(*h) {
                w.pressed = false;
            }
        }

        let mut pending = Vec::new();

        let focused_handles: Vec<i64> = self.widgets.iter()
            .filter(|(_, w)| w.focused)
            .map(|(h, _)| h)
            .collect();
        for h in focused_handles {
            if let Some(w) = self.widgets.get_mut(h) {
                w.focused = false;
            }
        }

        if let Some(id) = hit {
            self.events.push(UiEvent::click(id, x, y));

            if let Some(w) = self.widgets.get_mut(id) {
                match w.kind {
                    WidgetKind::Checkbox => {
                        w.checked = !w.checked;
                        let checked = w.checked;
                        self.events.push(UiEvent {
                            event_type: EventType::ValueChange,
                            widget_id: id,
                            x: if checked { 1.0 } else { 0.0 },
                            ..Default::default()
                        });
                    }
                    WidgetKind::TextInput => {
                        w.focused = true;
                    }
                    _ => {}
                }
            }

            self.collect_callbacks(CallbackKind::Click, id, &mut pending);
        }

        pending
    }

    fn handle_text_input(&mut self, text: &str) -> Vec<PendingCallback> {
        let mut pending = Vec::new();
        let focused: Option<i64> = self
            .widgets
            .iter()
            .find(|(_, w)| w.focused && w.kind == WidgetKind::TextInput)
            .map(|(h, _)| h);

        ui_debug_log(
            "handle_text_input",
            "received text input",
            &format!("text={:?}, focused={:?}", text, focused),
        );

        if let Some(id) = focused {
            if let Some(w) = self.widgets.get_mut(id) {
                let before = w.text.clone();
                w.text.push_str(text);
                self.needs_layout = true;
                ui_debug_log(
                    "handle_text_input",
                    "updated text widget content",
                    &format!("widget={}, before={:?}, after={:?}", id, before, w.text),
                );
            }
            self.events.push(UiEvent::text_input(id, text));
            self.collect_callbacks(CallbackKind::Change, id, &mut pending);
            ui_debug_log(
                "handle_text_input",
                "queued callbacks",
                &format!("widget={}, callback_count={}", id, pending.len()),
            );
        }

        pending
    }

    fn handle_key_press(&mut self, key: &str) -> Vec<PendingCallback> {
        let mut pending = Vec::new();
        if key == "Backspace" {
            let focused: Option<i64> = self
                .widgets
                .iter()
                .find(|(_, w)| w.focused && w.kind == WidgetKind::TextInput)
                .map(|(h, _)| h);
            if let Some(id) = focused {
                if let Some(w) = self.widgets.get_mut(id) {
                    let before = w.text.clone();
                    w.text.pop();
                    self.needs_layout = true;
                    ui_debug_log(
                        "handle_key_press",
                        "backspace applied",
                        &format!("widget={}, before={:?}, after={:?}", id, before, w.text),
                    );
                }
                self.collect_callbacks(CallbackKind::Change, id, &mut pending);
            }
        }
        self.events.push(UiEvent::key_press(key));
        pending
    }

    fn collect_callbacks(&self, kind: CallbackKind, widget_id: i64, out: &mut Vec<PendingCallback>) {
        for cb in &self.callbacks {
            if cb.kind == kind && cb.widget_id == widget_id {
                out.push(PendingCallback {
                    widget_id,
                    kind,
                    func_ptr: cb.func_ptr,
                });
            }
        }
    }
}

pub struct WinitApp;

impl WinitApp {
    fn request_redraw(&self) {
        super::STATE.with(|cell| {
            let borrow = cell.borrow();
            if let Some(state) = borrow.as_ref() {
                if let Some(win) = &state.window {
                    win.request_redraw();
                }
            }
        });
    }
}

fn fire_pending(pending: Vec<PendingCallback>) {
    for cb in pending {
        ui_debug_log(
            "fire_pending",
            "invoking callback",
            &format!(
                "widget={}, kind={:?}, fn_ptr=0x{:x}",
                cb.widget_id, cb.kind, cb.func_ptr as usize
            ),
        );
        let result = std::panic::catch_unwind(AssertUnwindSafe(|| unsafe {
            (cb.func_ptr)(cb.widget_id, cb.kind as i64);
        }));
        match result {
            Ok(()) => ui_debug_log(
                "fire_pending",
                "callback returned",
                &format!("widget={}, kind={:?}", cb.widget_id, cb.kind),
            ),
            Err(_) => ui_debug_log(
                "fire_pending",
                "callback panicked",
                &format!("widget={}, kind={:?}", cb.widget_id, cb.kind),
            ),
        }
    }
}

fn with_state_mut<R>(f: impl FnOnce(&mut UiState) -> R) -> R {
    super::STATE.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let state = borrow.as_mut().expect("UI not initialized");
        f(state)
    })
}

impl ApplicationHandler for WinitApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        with_state_mut(|state| {
            if state.window.is_some() {
                return;
            }

            let attrs = WindowAttributes::default()
                .with_title(&state.config.title)
                .with_inner_size(LogicalSize::new(
                    state.config.width,
                    state.config.height,
                ));

            let window = Arc::new(event_loop.create_window(attrs).expect("create window"));
            let renderer = WgpuRenderer::new(window.clone());
            state.renderer = ActiveRenderer::Wgpu(renderer);
            state.window = Some(window);
            state.needs_layout = true;
        });
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                with_state_mut(|state| {
                    state.running = false;
                    state.events.push(UiEvent::window_close());
                });
                event_loop.exit();
            }
            WindowEvent::Resized(size) => {
                with_state_mut(|state| {
                    if let ActiveRenderer::Wgpu(r) = &mut state.renderer {
                        r.resize(size.width, size.height);
                    }
                    state.needs_layout = true;
                    state.events.push(UiEvent::window_resize(
                        size.width as f32,
                        size.height as f32,
                    ));
                });
                self.request_redraw();
            }
            WindowEvent::RedrawRequested => {
                with_state_mut(|state| state.render_frame());
            }
            WindowEvent::CursorMoved { position, .. } => {
                with_state_mut(|state| {
                    state.handle_mouse_move(position.x as f32, position.y as f32);
                });
                self.request_redraw();
            }
            WindowEvent::MouseInput { state: elem_state, button: MouseButton::Left, .. } => {
                let pending = with_state_mut(|state| {
                    match elem_state {
                        ElementState::Pressed => {
                            let mx = state.mouse_x;
                            let my = state.mouse_y;
                            state.handle_mouse_down(mx, my);
                            Vec::new()
                        }
                        ElementState::Released => {
                            let mx = state.mouse_x;
                            let my = state.mouse_y;
                            state.handle_mouse_up(mx, my)
                        }
                    }
                });
                fire_pending(pending);
                self.request_redraw();
            }
            WindowEvent::KeyboardInput { event: key_event, .. } => {
                if key_event.state == ElementState::Pressed {
                    use winit::keyboard::Key;
                    let pending = match &key_event.logical_key {
                        Key::Character(s) => {
                            let text = s.to_string();
                            with_state_mut(|state| state.handle_text_input(&text))
                        }
                        Key::Named(named) => {
                            let key_name = format!("{:?}", named);
                            with_state_mut(|state| state.handle_key_press(&key_name))
                        }
                        _ => Vec::new(),
                    };
                    fire_pending(pending);
                    self.request_redraw();
                }
            }
            _ => {}
        }
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        self.request_redraw();
    }
}

pub fn run_app_blocking(state: UiState) {
    super::STATE.with(|cell| {
        *cell.borrow_mut() = Some(state);
    });
    let event_loop = EventLoop::new().expect("create event loop");
    let mut app = WinitApp;
    let _ = event_loop.run_app(&mut app);
}

pub fn run_terminal_app(state: &mut UiState) {
    let mut term = TerminalRenderer::new();
    term.init();
    state.renderer = ActiveRenderer::Terminal(TerminalRenderer::new());

    while state.running && term.is_running() {
        term.poll_events(&mut state.events);

        if state.needs_layout {
            let w = term.width() as f32;
            let h = term.height() as f32;
            if let Some(root) = state.root {
                state.layout.compute(root, &state.widgets, w, h);
                state.needs_layout = false;
            }
        }

        if let Some(root) = state.root {
            term.render_widget_tree(root, &state.widgets, &state.layout);
        }

        std::thread::sleep(std::time::Duration::from_millis(16));
    }

    term.shutdown();
}

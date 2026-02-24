use std::collections::VecDeque;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(i64)]
pub enum EventType {
    None = 0,
    Click = 1,
    Hover = 2,
    HoverEnd = 3,
    KeyPress = 4,
    KeyRelease = 5,
    TextInput = 6,
    ValueChange = 7,
    FocusIn = 8,
    FocusOut = 9,
    WindowClose = 10,
    WindowResize = 11,
    MouseDown = 12,
    MouseUp = 13,
    MouseMove = 14,
    Scroll = 15,
}

#[derive(Clone, Debug)]
pub struct UiEvent {
    pub event_type: EventType,
    pub widget_id: i64,
    pub key: String,
    pub text: String,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

impl Default for UiEvent {
    fn default() -> Self {
        Self {
            event_type: EventType::None,
            widget_id: -1,
            key: String::new(),
            text: String::new(),
            x: 0.0,
            y: 0.0,
            width: 0.0,
            height: 0.0,
        }
    }
}

impl UiEvent {
    pub fn click(widget_id: i64, x: f32, y: f32) -> Self {
        Self { event_type: EventType::Click, widget_id, x, y, ..Default::default() }
    }

    pub fn hover(widget_id: i64, x: f32, y: f32) -> Self {
        Self { event_type: EventType::Hover, widget_id, x, y, ..Default::default() }
    }

    pub fn key_press(key: &str) -> Self {
        Self { event_type: EventType::KeyPress, key: key.to_string(), ..Default::default() }
    }

    pub fn text_input(widget_id: i64, text: &str) -> Self {
        Self { event_type: EventType::TextInput, widget_id, text: text.to_string(), ..Default::default() }
    }

    pub fn window_close() -> Self {
        Self { event_type: EventType::WindowClose, ..Default::default() }
    }

    pub fn window_resize(width: f32, height: f32) -> Self {
        Self { event_type: EventType::WindowResize, width, height, ..Default::default() }
    }
}

pub struct EventQueue {
    events: VecDeque<UiEvent>,
    pool: Vec<UiEvent>,
}

impl EventQueue {
    pub fn new() -> Self {
        Self {
            events: VecDeque::with_capacity(64),
            pool: Vec::with_capacity(64),
        }
    }

    pub fn push(&mut self, event: UiEvent) {
        self.events.push_back(event);
    }

    pub fn poll(&mut self) -> UiEvent {
        self.events.pop_front().unwrap_or_default()
    }

    pub fn has_events(&self) -> bool {
        !self.events.is_empty()
    }

    pub fn drain_to_pool(&mut self) {
        self.pool.extend(self.events.drain(..));
    }

    pub fn clear(&mut self) {
        self.events.clear();
    }
}

pub type CallbackFn = unsafe extern "C" fn(i64, i64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CallbackKind {
    Click,
    Change,
    Hover,
    Key,
}

pub struct CallbackEntry {
    pub kind: CallbackKind,
    pub widget_id: i64,
    pub func_ptr: CallbackFn,
}

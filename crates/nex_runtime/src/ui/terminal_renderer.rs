use std::io::Write;

use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEventKind},
    style,
    terminal,
    ExecutableCommand, QueueableCommand,
};

use super::event::{EventQueue, UiEvent};
use super::layout::LayoutEngine;
use super::renderer::{Rect, Renderer};
use super::style::Color;
use super::widget::{WidgetArena, WidgetKind};

pub struct TerminalRenderer {
    stdout: std::io::Stdout,
    width: u16,
    height: u16,
    buffer: Vec<TermCell>,
    running: bool,
}

#[derive(Clone, Copy)]
struct TermCell {
    ch: char,
    fg: Color,
    bg: Color,
}

impl Default for TermCell {
    fn default() -> Self {
        Self { ch: ' ', fg: Color::WHITE, bg: Color::BLACK }
    }
}

impl TerminalRenderer {
    pub fn new() -> Self {
        let (w, h) = terminal::size().unwrap_or((80, 24));
        Self {
            stdout: std::io::stdout(),
            width: w,
            height: h,
            buffer: vec![TermCell::default(); (w as usize) * (h as usize)],
            running: true,
        }
    }

    pub fn init(&mut self) {
        let _ = terminal::enable_raw_mode();
        let _ = self.stdout.execute(terminal::EnterAlternateScreen);
        let _ = self.stdout.execute(cursor::Hide);
    }

    pub fn shutdown(&mut self) {
        let _ = self.stdout.execute(cursor::Show);
        let _ = self.stdout.execute(terminal::LeaveAlternateScreen);
        let _ = terminal::disable_raw_mode();
    }

    pub fn is_running(&self) -> bool {
        self.running
    }

    pub fn width(&self) -> u16 {
        self.width
    }

    pub fn height(&self) -> u16 {
        self.height
    }

    pub fn poll_events(&mut self, event_queue: &mut EventQueue) {
        while event::poll(std::time::Duration::from_millis(0)).unwrap_or(false) {
            if let Ok(evt) = event::read() {
                match evt {
                    Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                        match key_event.code {
                            KeyCode::Esc => {
                                self.running = false;
                                event_queue.push(UiEvent::window_close());
                            }
                            KeyCode::Char(c) => {
                                let mut key_name = String::new();
                                key_name.push(c);
                                event_queue.push(UiEvent::key_press(&key_name));
                            }
                            KeyCode::Enter => {
                                event_queue.push(UiEvent::key_press("Enter"));
                            }
                            KeyCode::Tab => {
                                event_queue.push(UiEvent::key_press("Tab"));
                            }
                            _ => {}
                        }
                    }
                    Event::Resize(w, h) => {
                        self.width = w;
                        self.height = h;
                        self.buffer = vec![TermCell::default(); (w as usize) * (h as usize)];
                        event_queue.push(UiEvent::window_resize(w as f32, h as f32));
                    }
                    _ => {}
                }
            }
        }
    }

    pub fn render_widget_tree(
        &mut self,
        root: i64,
        arena: &WidgetArena,
        layout: &LayoutEngine,
    ) {
        self.buffer.fill(TermCell::default());
        self.render_widget(root, arena, layout);
        self.flush();
    }

    fn render_widget(&mut self, handle: i64, arena: &WidgetArena, layout: &LayoutEngine) {
        let Some(widget) = arena.get(handle) else { return };
        if !widget.style.visible {
            return;
        }
        let Some(rect) = layout.get_rect(handle) else { return };

        let col = (rect.x / 1.0) as u16;
        let row = (rect.y / 1.0) as u16;
        let w = (rect.w / 1.0) as u16;
        let _h = (rect.h / 1.0) as u16;

        match widget.kind {
            WidgetKind::Text => {
                self.put_text(col, row, &widget.text, widget.style.fg_color, widget.style.bg_color);
            }
            WidgetKind::Button => {
                self.fill_rect_term(col, row, w.max(widget.text.len() as u16 + 4), 3, widget.style.bg_color);
                self.put_text(col + 2, row + 1, &widget.text, widget.style.fg_color, widget.style.bg_color);
            }
            WidgetKind::TextInput => {
                let display = if widget.text.is_empty() { &widget.placeholder } else { &widget.text };
                self.fill_rect_term(col, row, w.max(20), 3, Color::WHITE);
                self.draw_border_term(col, row, w.max(20), 3, Color::GRAY);
                self.put_text(col + 1, row + 1, display, widget.style.fg_color, Color::WHITE);
            }
            WidgetKind::Checkbox => {
                let mark = if widget.checked { "[x]" } else { "[ ]" };
                let label = format!("{} {}", mark, widget.text);
                self.put_text(col, row, &label, widget.style.fg_color, widget.style.bg_color);
            }
            WidgetKind::Slider => {
                let bar_w = w.max(20) as usize;
                let ratio = if widget.slider_max > widget.slider_min {
                    ((widget.slider_value - widget.slider_min) / (widget.slider_max - widget.slider_min)).clamp(0.0, 1.0)
                } else {
                    0.0
                };
                let filled = (ratio * bar_w as f32) as usize;
                let bar: String = "█".repeat(filled) + &"░".repeat(bar_w - filled);
                self.put_text(col, row, &bar, Color::BLUE, Color::DARK_GRAY);
            }
            _ => {}
        }

        for &child in &widget.children.clone() {
            self.render_widget(child, arena, layout);
        }
    }

    fn put_text(&mut self, col: u16, row: u16, text: &str, fg: Color, bg: Color) {
        for (i, ch) in text.chars().enumerate() {
            let x = col + i as u16;
            if x < self.width && row < self.height {
                let idx = (row as usize) * (self.width as usize) + (x as usize);
                if idx < self.buffer.len() {
                    self.buffer[idx] = TermCell { ch, fg, bg };
                }
            }
        }
    }

    fn fill_rect_term(&mut self, col: u16, row: u16, w: u16, h: u16, color: Color) {
        for dy in 0..h {
            for dx in 0..w {
                let x = col + dx;
                let y = row + dy;
                if x < self.width && y < self.height {
                    let idx = (y as usize) * (self.width as usize) + (x as usize);
                    if idx < self.buffer.len() {
                        self.buffer[idx] = TermCell { ch: ' ', fg: Color::WHITE, bg: color };
                    }
                }
            }
        }
    }

    fn draw_border_term(&mut self, col: u16, row: u16, w: u16, h: u16, color: Color) {
        if w < 2 || h < 2 {
            return;
        }
        self.put_char(col, row, '┌', color);
        self.put_char(col + w - 1, row, '┐', color);
        self.put_char(col, row + h - 1, '└', color);
        self.put_char(col + w - 1, row + h - 1, '┘', color);
        for x in 1..w - 1 {
            self.put_char(col + x, row, '─', color);
            self.put_char(col + x, row + h - 1, '─', color);
        }
        for y in 1..h - 1 {
            self.put_char(col, row + y, '│', color);
            self.put_char(col + w - 1, row + y, '│', color);
        }
    }

    fn put_char(&mut self, col: u16, row: u16, ch: char, fg: Color) {
        if col < self.width && row < self.height {
            let idx = (row as usize) * (self.width as usize) + (col as usize);
            if idx < self.buffer.len() {
                self.buffer[idx].ch = ch;
                self.buffer[idx].fg = fg;
            }
        }
    }

    fn flush(&mut self) {
        let mut prev_fg = Color::WHITE;
        let mut prev_bg = Color::BLACK;

        let _ = self.stdout.queue(cursor::MoveTo(0, 0));
        let _ = self.stdout.queue(style::SetForegroundColor(to_crossterm_color(prev_fg)));
        let _ = self.stdout.queue(style::SetBackgroundColor(to_crossterm_color(prev_bg)));

        for y in 0..self.height {
            let _ = self.stdout.queue(cursor::MoveTo(0, y));
            for x in 0..self.width {
                let idx = (y as usize) * (self.width as usize) + (x as usize);
                let cell = if idx < self.buffer.len() {
                    self.buffer[idx]
                } else {
                    TermCell::default()
                };
                if cell.fg.r != prev_fg.r || cell.fg.g != prev_fg.g || cell.fg.b != prev_fg.b {
                    let _ = self.stdout.queue(style::SetForegroundColor(to_crossterm_color(cell.fg)));
                    prev_fg = cell.fg;
                }
                if cell.bg.r != prev_bg.r || cell.bg.g != prev_bg.g || cell.bg.b != prev_bg.b {
                    let _ = self.stdout.queue(style::SetBackgroundColor(to_crossterm_color(cell.bg)));
                    prev_bg = cell.bg;
                }
                let _ = self.stdout.queue(style::Print(cell.ch));
            }
        }
        let _ = self.stdout.flush();
    }
}

impl Renderer for TerminalRenderer {
    fn begin_frame(&mut self, _width: u32, _height: u32) {
        self.buffer.fill(TermCell::default());
    }
    fn clear(&mut self, _color: Color) {
        self.buffer.fill(TermCell::default());
    }
    fn fill_rect(&mut self, rect: Rect, color: Color, _border_radius: f32) {
        let col = rect.x as u16;
        let row = rect.y as u16;
        let w = rect.w as u16;
        let h = rect.h as u16;
        self.fill_rect_term(col, row, w, h, color);
    }
    fn stroke_rect(&mut self, rect: Rect, color: Color, _line_width: f32, _border_radius: f32) {
        self.draw_border_term(rect.x as u16, rect.y as u16, rect.w as u16, rect.h as u16, color);
    }
    fn draw_text(&mut self, text: &str, x: f32, y: f32, _size: f32, color: Color) {
        self.put_text(x as u16, y as u16, text, color, Color::BLACK);
    }
    fn measure_text_width(&mut self, text: &str, _size: f32) -> f32 {
        text.len() as f32
    }
    fn fill_circle(&mut self, cx: f32, cy: f32, _radius: f32, _color: Color) {
        self.put_char(cx as u16, cy as u16, '●', _color);
    }
    fn draw_line(&mut self, x1: f32, y1: f32, x2: f32, _y2: f32, _line_width: f32, color: Color) {
        let len = ((x2 - x1).abs().max((y1 - _y2).abs())) as u16;
        for i in 0..=len {
            let t = if len == 0 { 0.0 } else { i as f32 / len as f32 };
            let x = x1 + (x2 - x1) * t;
            let y = y1 + (_y2 - y1) * t;
            self.put_char(x as u16, y as u16, '·', color);
        }
    }
    fn end_frame(&mut self) {}
    fn present(&mut self) {
        self.flush();
    }
}

fn to_crossterm_color(c: Color) -> style::Color {
    style::Color::Rgb { r: c.r, g: c.g, b: c.b }
}

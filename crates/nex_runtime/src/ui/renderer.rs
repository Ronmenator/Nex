use super::style::Color;

#[derive(Clone, Copy, Debug)]
pub struct Rect {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
}

impl Rect {
    pub fn new(x: f32, y: f32, w: f32, h: f32) -> Self {
        Self { x, y, w, h }
    }

    pub fn contains(&self, px: f32, py: f32) -> bool {
        px >= self.x && px < self.x + self.w && py >= self.y && py < self.y + self.h
    }
}

pub trait Renderer {
    fn begin_frame(&mut self, width: u32, height: u32);
    fn clear(&mut self, color: Color);
    fn fill_rect(&mut self, rect: Rect, color: Color, border_radius: f32);
    fn stroke_rect(&mut self, rect: Rect, color: Color, line_width: f32, border_radius: f32);
    fn draw_text(&mut self, text: &str, x: f32, y: f32, size: f32, color: Color);
    fn measure_text_width(&mut self, text: &str, size: f32) -> f32;
    fn fill_circle(&mut self, cx: f32, cy: f32, radius: f32, color: Color);
    fn draw_line(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, line_width: f32, color: Color);
    fn end_frame(&mut self);
    fn present(&mut self);
}

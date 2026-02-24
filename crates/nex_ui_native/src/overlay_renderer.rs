use super::renderer::{Rect, Renderer};
use super::style::Color;
use super::text_render::TextRenderer;

/// CPU-only overlay renderer for compositing UI on top of a 3D engine scene.
/// Renders to a `Vec<u32>` framebuffer (ARGB pixels) that the engine uploads
/// to a GPU texture and blits with alpha blending.
pub struct OverlayRenderer {
    framebuffer: Vec<u32>,
    fb_width: u32,
    fb_height: u32,
    text_renderer: TextRenderer,
}

impl OverlayRenderer {
    pub fn new(width: u32, height: u32) -> Self {
        let w = width.max(1);
        let h = height.max(1);
        Self {
            framebuffer: vec![0x00000000; (w * h) as usize],
            fb_width: w,
            fb_height: h,
            text_renderer: TextRenderer::new(),
        }
    }

    pub fn framebuffer(&self) -> &[u32] {
        &self.framebuffer
    }

    pub fn dimensions(&self) -> (u32, u32) {
        (self.fb_width, self.fb_height)
    }

    fn ensure_fb(&mut self, width: u32, height: u32) {
        let w = width.max(1);
        let h = height.max(1);
        if self.fb_width != w || self.fb_height != h {
            self.fb_width = w;
            self.fb_height = h;
            self.framebuffer = vec![0x00000000; (w * h) as usize];
        }
    }

    fn fb_fill_rect(&mut self, rect: Rect, color: Color) {
        let x0 = (rect.x as u32).min(self.fb_width);
        let y0 = (rect.y as u32).min(self.fb_height);
        let x1 = ((rect.x + rect.w) as u32).min(self.fb_width);
        let y1 = ((rect.y + rect.h) as u32).min(self.fb_height);
        let pixel = color_to_pixel(color);
        let alpha = color.a as u32;

        if alpha == 0 {
            return;
        }

        for y in y0..y1 {
            for x in x0..x1 {
                let idx = (y * self.fb_width + x) as usize;
                if idx < self.framebuffer.len() {
                    if alpha == 255 {
                        self.framebuffer[idx] = pixel;
                    } else {
                        self.framebuffer[idx] = blend_pixel(self.framebuffer[idx], pixel, alpha);
                    }
                }
            }
        }
    }
}

impl Renderer for OverlayRenderer {
    fn begin_frame(&mut self, width: u32, height: u32) {
        self.ensure_fb(width, height);
    }

    fn clear(&mut self, _color: Color) {
        // Clear to fully transparent so only UI widgets are visible over the 3D scene
        self.framebuffer.fill(0x00000000);
    }

    fn fill_rect(&mut self, rect: Rect, color: Color, _border_radius: f32) {
        self.fb_fill_rect(rect, color);
    }

    fn stroke_rect(&mut self, rect: Rect, color: Color, line_width: f32, _border_radius: f32) {
        let lw = line_width.max(1.0);
        // Top
        self.fb_fill_rect(Rect::new(rect.x, rect.y, rect.w, lw), color);
        // Bottom
        self.fb_fill_rect(Rect::new(rect.x, rect.y + rect.h - lw, rect.w, lw), color);
        // Left
        self.fb_fill_rect(Rect::new(rect.x, rect.y, lw, rect.h), color);
        // Right
        self.fb_fill_rect(Rect::new(rect.x + rect.w - lw, rect.y, lw, rect.h), color);
    }

    fn draw_text(&mut self, text: &str, x: f32, y: f32, size: f32, color: Color) {
        let fb_w = self.fb_width;
        let fb_h = self.fb_height;
        let fb = &mut self.framebuffer;
        self.text_renderer.draw_text(text, x, y, size, color, 0.0, fb, fb_w, fb_h);
    }

    fn measure_text_width(&mut self, text: &str, size: f32) -> f32 {
        self.text_renderer.measure_width(text, size)
    }

    fn fill_circle(&mut self, cx: f32, cy: f32, radius: f32, color: Color) {
        let r2 = radius * radius;
        let x0 = ((cx - radius) as u32).min(self.fb_width);
        let y0 = ((cy - radius) as u32).min(self.fb_height);
        let x1 = ((cx + radius + 1.0) as u32).min(self.fb_width);
        let y1 = ((cy + radius + 1.0) as u32).min(self.fb_height);
        let pixel = color_to_pixel(color);
        let alpha = color.a as u32;

        for y in y0..y1 {
            for x in x0..x1 {
                let dx = x as f32 - cx;
                let dy = y as f32 - cy;
                if dx * dx + dy * dy <= r2 {
                    let idx = (y * self.fb_width + x) as usize;
                    if idx < self.framebuffer.len() {
                        if alpha == 255 {
                            self.framebuffer[idx] = pixel;
                        } else {
                            self.framebuffer[idx] = blend_pixel(self.framebuffer[idx], pixel, alpha);
                        }
                    }
                }
            }
        }
    }

    fn draw_line(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, line_width: f32, color: Color) {
        let dx = x2 - x1;
        let dy = y2 - y1;
        let len = (dx * dx + dy * dy).sqrt();
        if len < 0.001 {
            return;
        }
        let steps = len.ceil() as u32;
        let half_w = (line_width / 2.0).max(0.5);
        let pixel = color_to_pixel(color);

        for i in 0..=steps {
            let t = i as f32 / steps as f32;
            let cx = x1 + dx * t;
            let cy = y1 + dy * t;
            let px0 = ((cx - half_w) as u32).min(self.fb_width);
            let py0 = ((cy - half_w) as u32).min(self.fb_height);
            let px1 = ((cx + half_w + 1.0) as u32).min(self.fb_width);
            let py1 = ((cy + half_w + 1.0) as u32).min(self.fb_height);
            for py in py0..py1 {
                for px in px0..px1 {
                    let idx = (py * self.fb_width + px) as usize;
                    if idx < self.framebuffer.len() {
                        self.framebuffer[idx] = pixel;
                    }
                }
            }
        }
    }

    fn end_frame(&mut self) {
        // No-op: the engine handles GPU upload
    }

    fn present(&mut self) {
        // No-op: the engine handles presentation
    }
}

fn color_to_pixel(c: Color) -> u32 {
    ((c.a as u32) << 24) | ((c.r as u32) << 16) | ((c.g as u32) << 8) | (c.b as u32)
}

fn blend_pixel(dst: u32, src: u32, src_alpha: u32) -> u32 {
    let inv = 255 - src_alpha;
    let sr = (src >> 16) & 0xFF;
    let sg = (src >> 8) & 0xFF;
    let sb = src & 0xFF;
    let sa = src_alpha;
    let dr = (dst >> 16) & 0xFF;
    let dg = (dst >> 8) & 0xFF;
    let db = dst & 0xFF;
    let da = (dst >> 24) & 0xFF;
    let r = (sr * sa + dr * inv) / 255;
    let g = (sg * sa + dg * inv) / 255;
    let b = (sb * sa + db * inv) / 255;
    // Composited alpha: src_alpha + dst_alpha * (1 - src_alpha / 255)
    let a = sa + (da * inv) / 255;
    (a.min(255) << 24) | (r << 16) | (g << 8) | b
}

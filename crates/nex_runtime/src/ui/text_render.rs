use cosmic_text::{Attrs, Buffer, FontSystem, Metrics, Shaping, SwashCache};

use super::style::Color;

pub struct TextRenderer {
    pub font_system: FontSystem,
    pub swash_cache: SwashCache,
}

impl TextRenderer {
    pub fn new() -> Self {
        Self {
            font_system: FontSystem::new(),
            swash_cache: SwashCache::new(),
        }
    }

    pub fn draw_text(
        &mut self,
        text: &str,
        x: f32,
        y: f32,
        size: f32,
        color: Color,
        max_width: f32,
        framebuffer: &mut [u32],
        fb_width: u32,
        fb_height: u32,
    ) {
        if text.is_empty() {
            return;
        }

        let line_height = size * 1.3;
        let metrics = Metrics::new(size, line_height);
        let mut buffer = Buffer::new(&mut self.font_system, metrics);

        let buf_width = if max_width > 0.0 { max_width } else { fb_width as f32 };
        buffer.set_size(&mut self.font_system, Some(buf_width), Some(fb_height as f32));
        buffer.set_text(&mut self.font_system, text, Attrs::new(), Shaping::Advanced);
        buffer.shape_until_scroll(&mut self.font_system, false);

        let text_color = cosmic_text::Color::rgba(color.r, color.g, color.b, color.a);

        buffer.draw(&mut self.font_system, &mut self.swash_cache, text_color, |px, py, _w, _h, drawn_color| {
            let screen_x = (x as i32) + px;
            let screen_y = (y as i32) + py;
            if screen_x < 0 || screen_y < 0 {
                return;
            }
            let sx = screen_x as u32;
            let sy = screen_y as u32;
            if sx >= fb_width || sy >= fb_height {
                return;
            }
            let alpha = drawn_color.a();
            if alpha == 0 {
                return;
            }
            let idx = (sy * fb_width + sx) as usize;
            if idx >= framebuffer.len() {
                return;
            }
            let src_r = drawn_color.r() as u32;
            let src_g = drawn_color.g() as u32;
            let src_b = drawn_color.b() as u32;
            let src_a = alpha as u32;

            if src_a == 255 {
                framebuffer[idx] = (src_r << 16) | (src_g << 8) | src_b | 0xFF000000;
            } else {
                let dst = framebuffer[idx];
                let dst_r = (dst >> 16) & 0xFF;
                let dst_g = (dst >> 8) & 0xFF;
                let dst_b = dst & 0xFF;
                let inv_a = 255 - src_a;
                let r = (src_r * src_a + dst_r * inv_a) / 255;
                let g = (src_g * src_a + dst_g * inv_a) / 255;
                let b = (src_b * src_a + dst_b * inv_a) / 255;
                framebuffer[idx] = (r << 16) | (g << 8) | b | 0xFF000000;
            }
        });
    }

    pub fn measure_text(&mut self, text: &str, size: f32, max_width: f32) -> (f32, f32) {
        if text.is_empty() {
            return (0.0, size * 1.3);
        }

        let line_height = size * 1.3;
        let metrics = Metrics::new(size, line_height);
        let mut buffer = Buffer::new(&mut self.font_system, metrics);
        let buf_width = if max_width > 0.0 { max_width } else { 10000.0 };
        buffer.set_size(&mut self.font_system, Some(buf_width), Some(10000.0));
        buffer.set_text(&mut self.font_system, text, Attrs::new(), Shaping::Advanced);
        buffer.shape_until_scroll(&mut self.font_system, false);

        let mut width: f32 = 0.0;
        let mut height: f32 = 0.0;
        for run in buffer.layout_runs() {
            width = width.max(run.line_w);
            height = height.max(run.line_y + line_height);
        }
        (width, height)
    }

    pub fn measure_width(&mut self, text: &str, size: f32) -> f32 {
        self.measure_text(text, size, 0.0).0
    }
}

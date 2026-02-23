use super::renderer::{Rect, Renderer};
use super::widget::CanvasCommand;

pub fn execute_canvas_commands(
    commands: &[CanvasCommand],
    rect: Rect,
    renderer: &mut dyn Renderer,
) {
    for cmd in commands {
        match cmd {
            CanvasCommand::FillRect { x, y, w, h, color } => {
                renderer.fill_rect(
                    Rect::new(rect.x + x, rect.y + y, *w, *h),
                    *color,
                    0.0,
                );
            }
            CanvasCommand::StrokeRect { x, y, w, h, line_width, color } => {
                renderer.stroke_rect(
                    Rect::new(rect.x + x, rect.y + y, *w, *h),
                    *color,
                    *line_width,
                    0.0,
                );
            }
            CanvasCommand::FillCircle { cx, cy, radius, color } => {
                renderer.fill_circle(rect.x + cx, rect.y + cy, *radius, *color);
            }
            CanvasCommand::DrawLine { x1, y1, x2, y2, line_width, color } => {
                renderer.draw_line(
                    rect.x + x1, rect.y + y1,
                    rect.x + x2, rect.y + y2,
                    *line_width, *color,
                );
            }
            CanvasCommand::DrawText { text, x, y, size, color } => {
                renderer.draw_text(text, rect.x + x, rect.y + y, *size, *color);
            }
            CanvasCommand::Clear { color } => {
                renderer.fill_rect(rect, *color, 0.0);
            }
        }
    }
}

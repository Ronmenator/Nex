use std::collections::HashMap;

use taffy::prelude::*;
use taffy::{MinMax, TaffyTree};

use super::renderer::Rect;
use super::style::Alignment;
use super::widget::{Widget, WidgetArena, WidgetKind};

pub struct LayoutEngine {
    tree: TaffyTree,
    widget_to_node: HashMap<i64, NodeId>,
    computed_rects: HashMap<i64, Rect>,
}

impl LayoutEngine {
    pub fn new() -> Self {
        Self {
            tree: TaffyTree::new(),
            widget_to_node: HashMap::new(),
            computed_rects: HashMap::new(),
        }
    }

    pub fn compute(&mut self, root: i64, arena: &WidgetArena, width: f32, height: f32) {
        self.tree = TaffyTree::new();
        self.widget_to_node.clear();
        self.computed_rects.clear();

        if let Some(root_node) = self.build_node(root, arena) {
            let _ = self.tree.compute_layout(
                root_node,
                Size {
                    width: AvailableSpace::Definite(width),
                    height: AvailableSpace::Definite(height),
                },
            );
            self.extract_rects(root, 0.0, 0.0);
        }
    }

    fn build_node(&mut self, handle: i64, arena: &WidgetArena) -> Option<NodeId> {
        let widget = arena.get(handle)?;

        if !widget.style.visible {
            return None;
        }

        let style = self.widget_to_taffy_style(widget);

        let child_nodes: Vec<NodeId> = widget
            .children
            .iter()
            .filter_map(|&child_handle| self.build_node(child_handle, arena))
            .collect();

        let node = if child_nodes.is_empty() {
            self.tree.new_leaf(style).ok()?
        } else {
            self.tree.new_with_children(style, &child_nodes).ok()?
        };

        self.widget_to_node.insert(handle, node);
        Some(node)
    }

    fn widget_to_taffy_style(&self, widget: &Widget) -> Style {
        let s = &widget.style;
        let mut style = Style::default();

        style.size.width = match s.width {
            Some(w) => Dimension::Length(w),
            None => match widget.kind {
                WidgetKind::Row | WidgetKind::Column | WidgetKind::Stack
                | WidgetKind::Scroll | WidgetKind::Grid => Dimension::Percent(1.0),
                _ => Dimension::Auto,
            },
        };
        style.size.height = match s.height {
            Some(h) => Dimension::Length(h),
            None => Dimension::Auto,
        };

        if let Some(w) = s.min_width {
            style.min_size.width = Dimension::Length(w);
        }
        if let Some(h) = s.min_height {
            style.min_size.height = Dimension::Length(h);
        }
        if let Some(w) = s.max_width {
            style.max_size.width = Dimension::Length(w);
        }
        if let Some(h) = s.max_height {
            style.max_size.height = Dimension::Length(h);
        }

        self.apply_intrinsic_size(widget, &mut style);

        style.padding = taffy::Rect {
            top: LengthPercentage::Length(s.padding.top),
            right: LengthPercentage::Length(s.padding.right),
            bottom: LengthPercentage::Length(s.padding.bottom),
            left: LengthPercentage::Length(s.padding.left),
        };
        style.margin = taffy::Rect {
            top: LengthPercentageAuto::Length(s.margin.top),
            right: LengthPercentageAuto::Length(s.margin.right),
            bottom: LengthPercentageAuto::Length(s.margin.bottom),
            left: LengthPercentageAuto::Length(s.margin.left),
        };

        style.flex_grow = s.flex_grow;
        style.flex_shrink = s.flex_shrink;
        style.gap = Size {
            width: LengthPercentage::Length(s.gap),
            height: LengthPercentage::Length(s.gap),
        };

        match widget.kind {
            WidgetKind::Row => {
                style.display = Display::Flex;
                style.flex_direction = FlexDirection::Row;
            }
            WidgetKind::Column | WidgetKind::Scroll => {
                style.display = Display::Flex;
                style.flex_direction = FlexDirection::Column;
            }
            WidgetKind::Stack => {
                style.display = Display::Flex;
                style.flex_direction = FlexDirection::Column;
            }
            WidgetKind::Grid => {
                style.display = Display::Grid;
                let cols = widget.grid_cols.max(1) as usize;
                style.grid_template_columns = vec![TrackSizingFunction::from(MinMax {
                    min: MinTrackSizingFunction::Auto,
                    max: MaxTrackSizingFunction::Fraction(1.0),
                }); cols];
            }
            _ => {
                style.display = Display::Flex;
            }
        }

        style.align_items = match widget.align_items {
            Alignment::Start => Some(AlignItems::FlexStart),
            Alignment::Center => Some(AlignItems::Center),
            Alignment::End => Some(AlignItems::FlexEnd),
            Alignment::Stretch => Some(AlignItems::Stretch),
        };

        style.justify_content = match widget.justify_content {
            super::style::Justification::Start => Some(JustifyContent::FlexStart),
            super::style::Justification::Center => Some(JustifyContent::Center),
            super::style::Justification::End => Some(JustifyContent::FlexEnd),
            super::style::Justification::SpaceBetween => Some(JustifyContent::SpaceBetween),
            super::style::Justification::SpaceAround => Some(JustifyContent::SpaceAround),
            super::style::Justification::SpaceEvenly => Some(JustifyContent::SpaceEvenly),
        };

        style
    }

    fn apply_intrinsic_size(&self, widget: &Widget, style: &mut Style) {
        let s = &widget.style;
        let pad_h = s.padding.top + s.padding.bottom;
        let pad_w = s.padding.left + s.padding.right;
        let line_h = s.font_size * 1.3;

        match widget.kind {
            WidgetKind::Text => {
                let text_w = estimate_text_width(&widget.text, s.font_size);
                if s.min_height.is_none() && s.height.is_none() {
                    style.min_size.height = Dimension::Length(line_h + pad_h);
                }
                if s.min_width.is_none() && s.width.is_none() {
                    style.min_size.width = Dimension::Length(text_w + pad_w);
                }
            }
            WidgetKind::Button => {
                let text_w = estimate_text_width(&widget.text, s.font_size);
                if s.min_height.is_none() && s.height.is_none() {
                    style.min_size.height = Dimension::Length(line_h + pad_h);
                }
                if s.min_width.is_none() && s.width.is_none() {
                    style.min_size.width = Dimension::Length(text_w + pad_w);
                }
            }
            WidgetKind::TextInput => {
                let placeholder_w = estimate_text_width(&widget.placeholder, s.font_size);
                if s.min_height.is_none() && s.height.is_none() {
                    style.min_size.height = Dimension::Length(line_h + pad_h);
                }
                if s.min_width.is_none() && s.width.is_none() {
                    style.min_size.width = Dimension::Length(placeholder_w.min(120.0) + pad_w);
                }
            }
            WidgetKind::Checkbox => {
                let box_size: f32 = 20.0;
                let label_w = estimate_text_width(&widget.text, s.font_size);
                let total_w = box_size + 6.0 + label_w;
                let h = box_size.max(line_h);
                style.size.width = Dimension::Auto;
                style.size.height = Dimension::Auto;
                if s.min_height.is_none() {
                    style.min_size.height = Dimension::Length(h);
                }
                if s.min_width.is_none() {
                    style.min_size.width = Dimension::Length(total_w);
                }
            }
            WidgetKind::Slider => {
                if s.min_width.is_none() && s.width.is_none() {
                    style.min_size.width = Dimension::Length(100.0);
                }
            }
            _ => {}
        }
    }

    fn extract_rects(&mut self, handle: i64, offset_x: f32, offset_y: f32) {
        let Some(&node) = self.widget_to_node.get(&handle) else { return };
        let Ok(layout) = self.tree.layout(node) else { return };

        let x = offset_x + layout.location.x;
        let y = offset_y + layout.location.y;
        let w = layout.size.width;
        let h = layout.size.height;

        self.computed_rects.insert(handle, Rect::new(x, y, w, h));

        let children: Vec<i64> = self
            .widget_to_node
            .keys()
            .copied()
            .collect::<Vec<_>>()
            .into_iter()
            .filter(|&k| {
                if let Some(child_node) = self.widget_to_node.get(&k) {
                    self.tree
                        .children(node)
                        .map_or(false, |c| c.contains(child_node))
                } else {
                    false
                }
            })
            .collect();

        for child in children {
            self.extract_rects(child, x, y);
        }
    }

    pub fn get_rect(&self, handle: i64) -> Option<Rect> {
        self.computed_rects.get(&handle).copied()
    }

    pub fn hit_test(&self, x: f32, y: f32, arena: &WidgetArena) -> Option<i64> {
        let mut best: Option<(i64, f32)> = None;
        for (&handle, &rect) in &self.computed_rects {
            if rect.contains(x, y) {
                if let Some(widget) = arena.get(handle) {
                    if !widget.kind.is_container() && widget.style.enabled {
                        let area = rect.w * rect.h;
                        if best.map_or(true, |(_, a)| area < a) {
                            best = Some((handle, area));
                        }
                    }
                }
            }
        }
        best.map(|(h, _)| h)
    }

    pub fn all_rects(&self) -> &HashMap<i64, Rect> {
        &self.computed_rects
    }
}

fn estimate_text_width(text: &str, font_size: f32) -> f32 {
    if text.is_empty() {
        return 0.0;
    }
    text.len() as f32 * font_size * 0.55
}

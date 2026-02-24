use super::style::{Alignment, Color, Justification, WidgetStyle};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WidgetKind {
    Text,
    Button,
    TextInput,
    Image,
    Checkbox,
    Slider,
    Row,
    Column,
    Stack,
    Scroll,
    Grid,
    Canvas,
}

impl WidgetKind {
    pub fn is_container(&self) -> bool {
        matches!(self, Self::Row | Self::Column | Self::Stack | Self::Scroll | Self::Grid)
    }
}

#[derive(Clone, Debug)]
pub struct Widget {
    pub kind: WidgetKind,
    pub style: WidgetStyle,
    pub text: String,
    pub id_str: String,
    pub children: Vec<i64>,
    pub parent: Option<i64>,

    pub checked: bool,
    pub slider_value: f32,
    pub slider_min: f32,
    pub slider_max: f32,
    pub placeholder: String,
    pub image_path: String,
    pub grid_cols: i32,
    pub align_items: Alignment,
    pub justify_content: Justification,
    pub align_self: Option<Alignment>,
    pub h_align: Option<Alignment>,
    pub v_align: Option<Alignment>,

    pub hovered: bool,
    pub pressed: bool,
    pub focused: bool,

    pub canvas_commands: Vec<CanvasCommand>,
}

impl Widget {
    pub fn new(kind: WidgetKind) -> Self {
        let default_style = match &kind {
            WidgetKind::Button => {
                let mut s = WidgetStyle::default();
                s.bg_color = Color::BLUE;
                s.fg_color = Color::WHITE;
                s.padding = super::style::Edges { top: 8.0, right: 16.0, bottom: 8.0, left: 16.0 };
                s.border_radius = 4.0;
                s
            }
            WidgetKind::TextInput => {
                let mut s = WidgetStyle::default();
                s.bg_color = Color::WHITE;
                s.fg_color = Color::BLACK;
                s.border_width = 1.0;
                s.border_color = Color::GRAY;
                s.padding = super::style::Edges { top: 6.0, right: 8.0, bottom: 6.0, left: 8.0 };
                s.border_radius = 4.0;
                s
            }
            WidgetKind::Checkbox => {
                let mut s = WidgetStyle::default();
                s.border_width = 2.0;
                s.border_color = Color::GRAY;
                s.border_radius = 3.0;
                s
            }
            WidgetKind::Slider => {
                let mut s = WidgetStyle::default();
                s.height = Some(24.0);
                s
            }
            _ => WidgetStyle::default(),
        };

        Self {
            kind,
            style: default_style,
            text: String::new(),
            id_str: String::new(),
            children: Vec::new(),
            parent: None,
            checked: false,
            slider_value: 0.0,
            slider_min: 0.0,
            slider_max: 1.0,
            placeholder: String::new(),
            image_path: String::new(),
            grid_cols: 2,
            align_items: Alignment::Start,
            justify_content: Justification::Start,
            align_self: None,
            h_align: None,
            v_align: None,
            hovered: false,
            pressed: false,
            focused: false,
            canvas_commands: Vec::new(),
        }
    }

    pub fn with_text(mut self, text: &str) -> Self {
        self.text = text.to_string();
        self
    }
}

#[derive(Clone, Debug)]
pub enum CanvasCommand {
    FillRect { x: f32, y: f32, w: f32, h: f32, color: Color },
    StrokeRect { x: f32, y: f32, w: f32, h: f32, line_width: f32, color: Color },
    FillCircle { cx: f32, cy: f32, radius: f32, color: Color },
    DrawLine { x1: f32, y1: f32, x2: f32, y2: f32, line_width: f32, color: Color },
    DrawText { text: String, x: f32, y: f32, size: f32, color: Color },
    Clear { color: Color },
}

pub struct WidgetArena {
    widgets: Vec<Option<Widget>>,
    generations: Vec<u32>,
    free_list: Vec<usize>,
}

impl WidgetArena {
    pub fn new() -> Self {
        Self {
            widgets: Vec::with_capacity(256),
            generations: Vec::with_capacity(256),
            free_list: Vec::new(),
        }
    }

    pub fn alloc(&mut self, widget: Widget) -> i64 {
        let (index, gen) = if let Some(idx) = self.free_list.pop() {
            self.widgets[idx] = Some(widget);
            self.generations[idx] += 1;
            (idx, self.generations[idx])
        } else {
            let idx = self.widgets.len();
            self.widgets.push(Some(widget));
            self.generations.push(0);
            (idx, 0)
        };
        pack_handle(index as u32, gen)
    }

    pub fn get(&self, handle: i64) -> Option<&Widget> {
        let (index, gen) = unpack_handle(handle);
        let idx = index as usize;
        if idx < self.widgets.len() && self.generations[idx] == gen {
            self.widgets[idx].as_ref()
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, handle: i64) -> Option<&mut Widget> {
        let (index, gen) = unpack_handle(handle);
        let idx = index as usize;
        if idx < self.widgets.len() && self.generations[idx] == gen {
            self.widgets[idx].as_mut()
        } else {
            None
        }
    }

    pub fn free(&mut self, handle: i64) {
        let (index, gen) = unpack_handle(handle);
        let idx = index as usize;
        if idx < self.widgets.len() && self.generations[idx] == gen {
            self.widgets[idx] = None;
            self.free_list.push(idx);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (i64, &Widget)> {
        self.widgets.iter().enumerate().filter_map(move |(i, opt)| {
            opt.as_ref().map(|w| (pack_handle(i as u32, self.generations[i]), w))
        })
    }
}

fn pack_handle(index: u32, generation: u32) -> i64 {
    ((generation as i64) << 32) | (index as i64)
}

fn unpack_handle(handle: i64) -> (u32, u32) {
    let index = handle as u32;
    let generation = (handle >> 32) as u32;
    (index, generation)
}

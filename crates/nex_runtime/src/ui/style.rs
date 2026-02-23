#[derive(Clone, Copy, Debug)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    pub const TRANSPARENT: Self = Self { r: 0, g: 0, b: 0, a: 0 };
    pub const WHITE: Self = Self { r: 255, g: 255, b: 255, a: 255 };
    pub const BLACK: Self = Self { r: 0, g: 0, b: 0, a: 255 };
    pub const GRAY: Self = Self { r: 200, g: 200, b: 200, a: 255 };
    pub const DARK_GRAY: Self = Self { r: 60, g: 60, b: 60, a: 255 };
    pub const LIGHT_GRAY: Self = Self { r: 230, g: 230, b: 230, a: 255 };
    pub const BLUE: Self = Self { r: 66, g: 133, b: 244, a: 255 };
    pub const HOVER_BLUE: Self = Self { r: 53, g: 122, b: 232, a: 255 };
    pub const PRESSED_BLUE: Self = Self { r: 40, g: 100, b: 200, a: 255 };

    pub fn from_rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    pub fn from_packed(packed: i64) -> Self {
        Self {
            r: ((packed >> 24) & 0xFF) as u8,
            g: ((packed >> 16) & 0xFF) as u8,
            b: ((packed >> 8) & 0xFF) as u8,
            a: (packed & 0xFF) as u8,
        }
    }

    pub fn to_packed(self) -> i64 {
        ((self.r as i64) << 24) | ((self.g as i64) << 16) | ((self.b as i64) << 8) | (self.a as i64)
    }

    pub fn to_f32_array(self) -> [f32; 4] {
        [
            self.r as f32 / 255.0,
            self.g as f32 / 255.0,
            self.b as f32 / 255.0,
            self.a as f32 / 255.0,
        ]
    }
}

impl Default for Color {
    fn default() -> Self {
        Self::TRANSPARENT
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Edges {
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
    pub left: f32,
}

impl Edges {
    pub fn all(v: f32) -> Self {
        Self { top: v, right: v, bottom: v, left: v }
    }

    pub fn from_packed(packed: i64) -> Self {
        let bytes = packed.to_le_bytes();
        Self {
            top: i16::from_le_bytes([bytes[0], bytes[1]]) as f32,
            right: i16::from_le_bytes([bytes[2], bytes[3]]) as f32,
            bottom: i16::from_le_bytes([bytes[4], bytes[5]]) as f32,
            left: i16::from_le_bytes([bytes[6], bytes[7]]) as f32,
        }
    }
}

#[derive(Clone, Debug)]
pub struct WidgetStyle {
    pub width: Option<f32>,
    pub height: Option<f32>,
    pub min_width: Option<f32>,
    pub min_height: Option<f32>,
    pub max_width: Option<f32>,
    pub max_height: Option<f32>,
    pub padding: Edges,
    pub margin: Edges,
    pub bg_color: Color,
    pub fg_color: Color,
    pub font_size: f32,
    pub border_width: f32,
    pub border_color: Color,
    pub border_radius: f32,
    pub flex_grow: f32,
    pub flex_shrink: f32,
    pub gap: f32,
    pub visible: bool,
    pub enabled: bool,
}

impl Default for WidgetStyle {
    fn default() -> Self {
        Self {
            width: None,
            height: None,
            min_width: None,
            min_height: None,
            max_width: None,
            max_height: None,
            padding: Edges::default(),
            margin: Edges::default(),
            bg_color: Color::TRANSPARENT,
            fg_color: Color::WHITE,
            font_size: 16.0,
            border_width: 0.0,
            border_color: Color::TRANSPARENT,
            border_radius: 0.0,
            flex_grow: 0.0,
            flex_shrink: 1.0,
            gap: 0.0,
            visible: true,
            enabled: true,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Alignment {
    Start,
    Center,
    End,
    Stretch,
}

impl Alignment {
    pub fn from_i64(v: i64) -> Self {
        match v {
            1 => Self::Center,
            2 => Self::End,
            3 => Self::Stretch,
            _ => Self::Start,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Justification {
    Start,
    Center,
    End,
    SpaceBetween,
    SpaceAround,
    SpaceEvenly,
}

impl Justification {
    pub fn from_i64(v: i64) -> Self {
        match v {
            1 => Self::Center,
            2 => Self::End,
            3 => Self::SpaceBetween,
            4 => Self::SpaceAround,
            5 => Self::SpaceEvenly,
            _ => Self::Start,
        }
    }
}

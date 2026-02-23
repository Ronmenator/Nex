//! `.nexui` markup parser — lightweight XML subset for Nex declarative UI.
//!
//! Produces a [`UIDocument`] AST that downstream passes lower into imperative
//! `std.ui` calls.

use std::path::PathBuf;
use nexc_diag::{Diagnostic, DiagnosticSink, Severity, Span};

// ---------------------------------------------------------------------------
// AST types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct UIDocument {
    pub namespace: String,
    pub class_name: String,
    pub root: UIElement,
    pub file_path: String,
    pub styles: Vec<UIStyle>,
    /// External stylesheet imports from `<Window.Resources>` / `<Widget.Resources>` blocks.
    pub resource_imports: Vec<UIResourceImport>,
}

/// A reference to an external `.nexuistyle` file declared inside `<Window.Resources>`.
#[derive(Debug, Clone)]
pub struct UIResourceImport {
    /// Path to the `.nexuistyle` file, relative to the importing `.nexui` file.
    pub source: String,
}

/// A named style definition from a `<Window.Styles>` / `<Widget.Styles>` block.
#[derive(Debug, Clone)]
pub struct UIStyle {
    pub name: String,
    /// Optional `TargetType` attribute — informational only, not enforced at compile time.
    pub target_type: Option<String>,
    pub setters: Vec<UIStyleSetter>,
}

/// A single `<Setter Property="..." Value="..."/>` inside a `<Style>`.
#[derive(Debug, Clone)]
pub struct UIStyleSetter {
    pub property: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct UIElement {
    pub tag: String,
    pub attributes: Vec<UIAttribute>,
    pub children: Vec<UIElement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UIAttribute {
    pub name: String,
    pub value: UIAttrValue,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UIAttrValue {
    Literal(String),
    Binding(UIBinding),
}

#[derive(Debug, Clone)]
pub struct UIBinding {
    pub path: String,
    pub mode: BindingMode,
    pub converter: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingMode {
    OneWay,
    TwoWay,
}

// ---------------------------------------------------------------------------
// Known element → widget mapping
// ---------------------------------------------------------------------------

pub fn tag_to_widget_kind(tag: &str) -> Option<&'static str> {
    match tag {
        "Window" => Some("window"),
        // Non-window widget root — compiles to _create_container / _build_widget
        "Widget" => Some("widget_root"),
        // Shows one child widget at a time; children must be <Include Widget="…" />
        "WidgetSwitcher" => Some("widget_switcher"),
        // Reference to a Widget class inside a WidgetSwitcher
        "Include" => Some("include_widget"),
        "Text" => Some("text"),
        "Button" => Some("button"),
        "TextInput" => Some("text_input"),
        "Image" => Some("image"),
        "Checkbox" => Some("checkbox"),
        "Slider" => Some("slider"),
        "Row" => Some("row"),
        "Column" => Some("column"),
        "Stack" => Some("stack"),
        "Scroll" => Some("scroll"),
        "Grid" => Some("grid"),
        "Canvas" => Some("canvas"),
        _ => None,
    }
}

pub fn is_event_attribute(name: &str) -> bool {
    matches!(name, "Click" | "Change" | "Hover" | "Key")
}

pub fn is_layout_attribute(name: &str) -> bool {
    matches!(
        name,
        "Width"
            | "Height"
            | "MinWidth"
            | "MinHeight"
            | "MaxWidth"
            | "MaxHeight"
            | "Padding"
            | "Margin"
            | "FlexGrow"
            | "FlexShrink"
            | "Gap"
            | "AlignItems"
            | "JustifyContent"
            | "AlignSelf"
    )
}

pub fn is_style_attribute(name: &str) -> bool {
    matches!(
        name,
        "BgColor"
            | "FgColor"
            | "FontSize"
            | "BorderWidth"
            | "BorderColor"
            | "BorderRadius"
            | "Visible"
            | "Enabled"
    )
}

pub fn is_content_attribute(name: &str) -> bool {
    matches!(
        name,
        "Text" | "Label" | "Placeholder" | "Source" | "Title" | "Columns"
            | "Min" | "Max" | "Value" | "Checked"
            // Widget / WidgetSwitcher / Include specific attributes
            | "Extends" | "Widget" | "Active"
            // Style reference
            | "Style"
    )
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

struct XmlParser<'a> {
    src: &'a str,
    pos: usize,
    sink: &'a mut DiagnosticSink,
    file_path: String,
}

impl<'a> XmlParser<'a> {
    fn new(src: &'a str, file_path: &str, sink: &'a mut DiagnosticSink) -> Self {
        Self {
            src,
            pos: 0,
            sink,
            file_path: file_path.to_string(),
        }
    }

    fn remaining(&self) -> &str {
        &self.src[self.pos..]
    }

    fn skip_ws(&mut self) {
        while self.pos < self.src.len()
            && self.src.as_bytes()[self.pos].is_ascii_whitespace()
        {
            self.pos += 1;
        }
    }

    fn peek(&self) -> Option<u8> {
        self.src.as_bytes().get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<u8> {
        let b = self.src.as_bytes().get(self.pos).copied();
        if b.is_some() {
            self.pos += 1;
        }
        b
    }

    fn expect(&mut self, ch: u8) -> bool {
        if self.peek() == Some(ch) {
            self.pos += 1;
            true
        } else {
            self.err(format!("expected '{}', got {:?}", ch as char, self.peek().map(|b| b as char)));
            false
        }
    }

    fn err(&mut self, msg: String) {
        self.sink.push(Diagnostic {
            id: "nexui_parse".into(),
            severity: Severity::Error,
            span: Some(Span { lo: self.pos, hi: self.pos + 1 }),
            file: Some(PathBuf::from(&self.file_path)),
            message: msg,
            notes: Vec::new(),
            suggestions: Vec::new(),
        });
    }

    fn skip_xml_decl_and_comments(&mut self) {
        loop {
            self.skip_ws();
            if self.remaining().starts_with("<?") {
                if let Some(end) = self.remaining().find("?>") {
                    self.pos += end + 2;
                } else {
                    self.pos = self.src.len();
                }
            } else if self.remaining().starts_with("<!--") {
                if let Some(end) = self.remaining().find("-->") {
                    self.pos += end + 3;
                } else {
                    self.pos = self.src.len();
                }
            } else {
                break;
            }
        }
    }

    fn read_name(&mut self) -> String {
        let start = self.pos;
        while self.pos < self.src.len() {
            let b = self.src.as_bytes()[self.pos];
            if b.is_ascii_alphanumeric() || b == b'_' || b == b'-' || b == b':' || b == b'.' {
                self.pos += 1;
            } else {
                break;
            }
        }
        self.src[start..self.pos].to_string()
    }

    fn read_quoted_string(&mut self) -> String {
        let quote = match self.advance() {
            Some(b'"') => b'"',
            Some(b'\'') => b'\'',
            _ => {
                self.err("expected quoted string".into());
                return String::new();
            }
        };
        let start = self.pos;
        while self.pos < self.src.len() && self.src.as_bytes()[self.pos] != quote {
            self.pos += 1;
        }
        let s = self.src[start..self.pos].to_string();
        if self.pos < self.src.len() {
            self.pos += 1; // closing quote
        }
        s
    }

    fn parse_attr_value(&self, raw: &str, _span: Span) -> UIAttrValue {
        let trimmed = raw.trim();
        if trimmed.starts_with("{Binding") && trimmed.ends_with('}') {
            let inner = trimmed["{Binding".len()..trimmed.len() - 1].trim();
            let parts: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();
            let path = parts[0].to_string();
            let mut mode = BindingMode::OneWay;
            let mut converter = None;
            for part in &parts[1..] {
                if let Some(val) = part.strip_prefix("Mode=") {
                    if val.trim() == "TwoWay" {
                        mode = BindingMode::TwoWay;
                    }
                } else if let Some(val) = part.strip_prefix("Converter=") {
                    converter = Some(val.trim().to_string());
                }
            }
            UIAttrValue::Binding(UIBinding { path, mode, converter })
        } else {
            UIAttrValue::Literal(raw.to_string())
        }
    }

    fn parse_element(&mut self) -> Option<UIElement> {
        self.skip_ws();
        let start = self.pos;
        if !self.expect(b'<') {
            return None;
        }
        let tag = self.read_name();
        if tag.is_empty() {
            self.err("expected element tag name".into());
            return None;
        }

        let mut attributes = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some(b'/') => {
                    self.pos += 1;
                    self.expect(b'>');
                    return Some(UIElement {
                        tag,
                        attributes,
                        children: Vec::new(),
                        span: Span { lo: start, hi: self.pos },
                    });
                }
                Some(b'>') => {
                    self.pos += 1;
                    break;
                }
                Some(b) if b.is_ascii_alphabetic() || b == b'_' => {
                    let attr_start = self.pos;
                    let name = self.read_name();
                    self.skip_ws();
                    self.expect(b'=');
                    self.skip_ws();
                    let raw_val = self.read_quoted_string();
                    let attr_span = Span { lo: attr_start, hi: self.pos };
                    let value = self.parse_attr_value(&raw_val, attr_span);
                    attributes.push(UIAttribute { name, value, span: attr_span });
                }
                _ => {
                    self.err(format!("unexpected byte in element: {:?}", self.peek()));
                    self.pos += 1;
                    if self.pos >= self.src.len() {
                        return None;
                    }
                }
            }
        }

        // Parse children and closing tag.
        let mut children = Vec::new();
        loop {
            self.skip_ws();
            if self.remaining().starts_with("</") {
                self.pos += 2;
                let close_name = self.read_name();
                if close_name != tag {
                    self.err(format!("mismatched closing tag: expected </{tag}>, got </{close_name}>"));
                }
                self.skip_ws();
                self.expect(b'>');
                break;
            } else if self.remaining().starts_with("<!--") {
                if let Some(end) = self.remaining().find("-->") {
                    self.pos += end + 3;
                } else {
                    self.pos = self.src.len();
                    break;
                }
            } else if self.peek() == Some(b'<') {
                if let Some(child) = self.parse_element() {
                    children.push(child);
                }
            } else if self.pos >= self.src.len() {
                self.err(format!("unexpected end of file in element <{tag}>"));
                break;
            } else {
                self.pos += 1;
            }
        }

        Some(UIElement {
            tag,
            attributes,
            children,
            span: Span { lo: start, hi: self.pos },
        })
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a `.nexui` file into a [`UIDocument`].
pub fn parse_nexui(src: &str, file_path: &str, sink: &mut DiagnosticSink) -> Option<UIDocument> {
    let mut parser = XmlParser::new(src, file_path, sink);
    parser.skip_xml_decl_and_comments();

    let root = parser.parse_element()?;

    // Extract xmlns and derive class name from file stem.
    let namespace = root
        .attributes
        .iter()
        .find(|a| a.name == "xmlns")
        .and_then(|a| match &a.value {
            UIAttrValue::Literal(s) => Some(s.clone()),
            _ => None,
        })
        .unwrap_or_else(|| "std.ui".to_string());

    let class_name = std::path::Path::new(file_path)
        .file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| "UnnamedWindow".to_string());

    // A `<ResourceDictionary>` root means this is a pure `.nexuistyle` stylesheet file.
    // Parse all its children as styles and return a widget-free document.
    if root.tag == "ResourceDictionary" {
        let styles = parse_styles_block(&root);
        return Some(UIDocument {
            namespace,
            class_name,
            root: UIElement {
                tag: "ResourceDictionary".into(),
                attributes: Vec::new(),
                children: Vec::new(),
                span: root.span,
            },
            file_path: file_path.to_string(),
            styles,
            resource_imports: Vec::new(),
        });
    }

    // Extract `<Window.Styles>` / `<Widget.Styles>` and `<Window.Resources>` / `<Widget.Resources>`
    // blocks from the root's children. These are not real widgets and are removed from the tree.
    let mut styles: Vec<UIStyle> = Vec::new();
    let mut resource_imports: Vec<UIResourceImport> = Vec::new();
    let mut widget_children: Vec<UIElement> = Vec::new();
    for child in root.children {
        if child.tag.ends_with(".Styles") {
            styles.extend(parse_styles_block(&child));
        } else if child.tag.ends_with(".Resources") {
            resource_imports.extend(parse_resources_block(&child));
        } else {
            widget_children.push(child);
        }
    }

    let root = UIElement {
        tag: root.tag,
        attributes: root.attributes,
        children: widget_children,
        span: root.span,
    };

    Some(UIDocument {
        namespace,
        class_name,
        root,
        file_path: file_path.to_string(),
        styles,
        resource_imports,
    })
}

/// Extract `UIStyle` definitions from a `<Window.Styles>` element.
fn parse_styles_block(styles_elem: &UIElement) -> Vec<UIStyle> {
    let mut styles = Vec::new();
    for style_elem in &styles_elem.children {
        if style_elem.tag != "Style" {
            continue;
        }
        let name = match find_literal_attr_in_elem(style_elem, "Name") {
            Some(n) => n,
            None => continue, // Style without a Name is ignored.
        };
        let target_type = find_literal_attr_in_elem(style_elem, "TargetType");
        let mut setters = Vec::new();
        for setter in &style_elem.children {
            if setter.tag != "Setter" {
                continue;
            }
            let property = match find_literal_attr_in_elem(setter, "Property") {
                Some(p) => p,
                None => continue,
            };
            let value = match find_literal_attr_in_elem(setter, "Value") {
                Some(v) => v,
                None => continue,
            };
            setters.push(UIStyleSetter { property, value });
        }
        styles.push(UIStyle { name, target_type, setters });
    }
    styles
}

/// Extract `UIResourceImport` entries from a `<Window.Resources>` element.
fn parse_resources_block(resources_elem: &UIElement) -> Vec<UIResourceImport> {
    let mut imports = Vec::new();
    for child in &resources_elem.children {
        if child.tag != "ResourceDictionary" {
            continue;
        }
        if let Some(source) = find_literal_attr_in_elem(child, "Source") {
            imports.push(UIResourceImport { source });
        }
    }
    imports
}

fn find_literal_attr_in_elem(elem: &UIElement, name: &str) -> Option<String> {
    elem.attributes.iter().find(|a| a.name == name).and_then(|a| {
        if let UIAttrValue::Literal(s) = &a.value {
            Some(s.clone())
        } else {
            None
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_window() {
        let src = r#"<Window xmlns="std.ui" Title="Test" Width="800" Height="600">
  <Column>
    <Text Text="Hello" />
    <Button Label="Click" Click="OnClick" />
  </Column>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = parse_nexui(src, "TestWindow.nexui", &mut sink).unwrap();
        assert!(!sink.has_errors());
        assert_eq!(doc.class_name, "TestWindow");
        assert_eq!(doc.root.tag, "Window");
        assert_eq!(doc.root.children.len(), 1);
        let col = &doc.root.children[0];
        assert_eq!(col.tag, "Column");
        assert_eq!(col.children.len(), 2);
    }

    #[test]
    fn parse_styles_block_extracted() {
        let src = r#"<Window xmlns="std.ui" Title="App" Width="800" Height="600">
  <Window.Styles>
    <Style Name="Primary" TargetType="Button">
      <Setter Property="BgColor" Value="0x3B82F6FF"/>
      <Setter Property="FgColor" Value="0xFFFFFFFF"/>
      <Setter Property="BorderRadius" Value="6"/>
    </Style>
    <Style Name="Header">
      <Setter Property="FontSize" Value="28"/>
    </Style>
  </Window.Styles>
  <Column>
    <Button Style="Primary" Label="Go" />
  </Column>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = parse_nexui(src, "App.nexui", &mut sink).unwrap();
        assert!(!sink.has_errors());
        // Styles block must NOT appear as a child of the root element.
        assert!(
            doc.root.children.iter().all(|c| !c.tag.ends_with(".Styles")),
            "<Window.Styles> should be removed from the widget tree"
        );
        // Two styles parsed.
        assert_eq!(doc.styles.len(), 2);
        let primary = doc.styles.iter().find(|s| s.name == "Primary").unwrap();
        assert_eq!(primary.target_type.as_deref(), Some("Button"));
        assert_eq!(primary.setters.len(), 3);
        assert_eq!(primary.setters[0].property, "BgColor");
        assert_eq!(primary.setters[0].value, "0x3B82F6FF");
        let header = doc.styles.iter().find(|s| s.name == "Header").unwrap();
        assert!(header.target_type.is_none());
        assert_eq!(header.setters.len(), 1);
    }

    #[test]
    fn parse_resources_block_extracted() {
        let src = r#"<Window xmlns="std.ui" Title="App">
  <Window.Resources>
    <ResourceDictionary Source="Styles/App.nexuistyle"/>
    <ResourceDictionary Source="Styles/Theme.nexuistyle"/>
  </Window.Resources>
  <Column/>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = parse_nexui(src, "App.nexui", &mut sink).unwrap();
        assert!(!sink.has_errors());
        // Resources block must NOT appear as a child of the root element.
        assert!(
            doc.root.children.iter().all(|c| !c.tag.ends_with(".Resources")),
            "<Window.Resources> should be removed from the widget tree"
        );
        assert_eq!(doc.resource_imports.len(), 2);
        assert_eq!(doc.resource_imports[0].source, "Styles/App.nexuistyle");
        assert_eq!(doc.resource_imports[1].source, "Styles/Theme.nexuistyle");
    }

    #[test]
    fn resource_dictionary_root_yields_styles() {
        let src = r#"<ResourceDictionary xmlns="std.ui">
  <Style Name="Primary" TargetType="Button">
    <Setter Property="BgColor" Value="0x3B82F6FF"/>
    <Setter Property="BorderRadius" Value="6"/>
  </Style>
  <Style Name="Muted" TargetType="Text">
    <Setter Property="FgColor" Value="0x6B7280FF"/>
  </Style>
</ResourceDictionary>"#;
        let mut sink = DiagnosticSink::new();
        let doc = parse_nexui(src, "AppStyles.nexuistyle", &mut sink).unwrap();
        assert!(!sink.has_errors());
        assert_eq!(doc.root.tag, "ResourceDictionary");
        assert!(doc.root.children.is_empty(), "no widget children for stylesheet files");
        assert_eq!(doc.styles.len(), 2);
        assert_eq!(doc.styles[0].name, "Primary");
        assert_eq!(doc.styles[0].setters.len(), 2);
        assert_eq!(doc.styles[1].name, "Muted");
        assert!(doc.resource_imports.is_empty());
    }

    #[test]
    fn parse_binding_expressions() {
        let src = r#"<Window xmlns="std.ui">
  <Text Text="{Binding Counter}" />
  <TextInput Text="{Binding Name, Mode=TwoWay}" />
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = parse_nexui(src, "Bind.nexui", &mut sink).unwrap();
        assert!(!sink.has_errors());
        let text = &doc.root.children[0];
        match &text.attributes[0].value {
            UIAttrValue::Binding(b) => {
                assert_eq!(b.path, "Counter");
                assert_eq!(b.mode, BindingMode::OneWay);
            }
            _ => panic!("expected binding"),
        }
        let input = &doc.root.children[1];
        match &input.attributes[0].value {
            UIAttrValue::Binding(b) => {
                assert_eq!(b.path, "Name");
                assert_eq!(b.mode, BindingMode::TwoWay);
            }
            _ => panic!("expected binding"),
        }
    }
}

//! `.nexui` lowering — transforms a [`UIDocument`] into a partial Nex `ClassDecl`
//! containing imperative `std.ui` widget construction, event wiring, and binding
//! setup code.
//!
//! ## Root element kinds
//!
//! | Root tag         | Generated methods                                                     |
//! |------------------|-----------------------------------------------------------------------|
//! | `Window`         | `_init_ui`, `_setup_bindings`, `_run_app`, `_update_*`               |
//! | `Widget`         | `_create_container`, virtual `_build_widget`, `_init_bindings`       |
//! | `Widget Extends` | override `_build_widget`, `_init_bindings`; `base_specs` populated   |

use nexc_ast::*;
use nexc_diag::{Diagnostic, DiagnosticSink, Severity, Span};
use nexui_parse::*;
use std::collections::{HashMap, HashSet};

const S: Span = Span::new(0, 0);

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Lower a parsed `.nexui` document into a partial `ClassDecl`.
///
/// The generated class contains:
/// - Fields for every widget handle (e.g. `_text_0`, `_btn_1`)
/// - An `_init_ui()` method that builds the widget tree (Window root)
/// - A `_create_container() -> Int64` method (Widget root)
/// - A `_build_widget() -> Int64` method (Widget root, virtual or override)
/// - A `_setup_bindings()` method (Window) / `_init_bindings()` method (Widget) that registers property-change listeners
/// - Per-binding `_update_PropertyName()` methods
pub fn lower_document(doc: &UIDocument, sink: &mut DiagnosticSink) -> ClassDecl {
    let mut ctx = LowerCtx::new(&doc.class_name);

    // Populate named styles from the document's style block.
    for style in &doc.styles {
        ctx.styles.insert(style.name.clone(), style.setters.clone());
    }

    let is_widget = doc.root.tag == "Widget";
    if is_widget {
        ctx.is_widget_root = true;
        ctx.widget_extends = find_literal_attr(&doc.root, "Extends");
    }

    ctx.lower_element(&doc.root, None);

    // Generate the deferred WidgetSwitcher Active binding updater (if any).
    ctx.apply_switcher_binding();

    // Emit warnings for any Style references that had no matching definition.
    let mut seen: HashSet<&str> = HashSet::new();
    for name in &ctx.unresolved_styles {
        if seen.insert(name.as_str()) {
            sink.push(Diagnostic {
                id: "nexui_lower".into(),
                severity: Severity::Warning,
                span: None,
                file: Some(std::path::PathBuf::from(&doc.file_path)),
                message: format!("style '{}' is not defined in this document", name),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }
    }

    let base_specs = if let Some(ref base) = ctx.widget_extends.clone() {
        vec![BaseSpec {
            name: base.clone(),
            shared: false,
            ctor_args: Vec::new(),
            span: S,
        }]
    } else {
        Vec::new()
    };

    let mut methods = Vec::new();
    if is_widget {
        if ctx.widget_extends.is_none() {
            // Base widget
            methods.push(ctx.build_create_container());
            methods.push(ctx.build_virtual_build_widget());
        } else {
            // Derived widget
            methods.push(ctx.build_override_build_widget());
        }
        methods.push(ctx.build_widget_bindings());
    } else {
        // Window
        methods.push(ctx.build_init_ui());
        methods.push(ctx.build_setup_bindings());
        methods.push(ctx.build_run_app());
    }
    methods.extend(ctx.build_update_methods());

    ClassDecl {
        name: doc.class_name.clone(),
        is_partial: true,
        visibility: Visibility::Public,
        type_params: Vec::new(),
        base_specs,
        fields: ctx.fields,
        methods,
        attributes: Vec::new(),
        span: S,
    }
}

// ---------------------------------------------------------------------------
// Lowering context
// ---------------------------------------------------------------------------

struct SwitcherItem {
    index: usize,
    handle_field: String,
}

struct LowerCtx {
    /// Lowercased/sanitized class-name prefix for generated private symbols.
    class_tag: String,
    counter: u32,
    fields: Vec<FieldDecl>,
    /// Statements for `_init_ui` (Window) or `_create_container` (base Widget).
    init_stmts: Vec<Stmt>,
    /// Extra statements for derived Widget's `override _build_widget`.
    derived_stmts: Vec<Stmt>,
    binding_stmts: Vec<Stmt>,
    update_methods: Vec<FunctionDecl>,
    app_field: Option<String>,
    root_field: Option<String>,
    window_title: Option<String>,
    window_width: Option<i64>,
    window_height: Option<i64>,
    // Widget-specific
    is_widget_root: bool,
    widget_extends: Option<String>,
    widget_root_field: Option<String>,
    // WidgetSwitcher-specific
    switcher_items: Vec<SwitcherItem>,
    switcher_active_binding: Option<String>, // property name for the Active binding
    /// Named styles from `<Window.Styles>` / `<Widget.Styles>` blocks.
    styles: HashMap<String, Vec<UIStyleSetter>>,
    /// Style names referenced by elements but not defined — collected for diagnostics.
    unresolved_styles: Vec<String>,
}

impl LowerCtx {
    fn new(class_name: &str) -> Self {
        let class_tag = sanitize_symbol_part(class_name);
        Self {
            class_tag,
            counter: 0,
            fields: Vec::new(),
            init_stmts: Vec::new(),
            derived_stmts: Vec::new(),
            binding_stmts: Vec::new(),
            update_methods: Vec::new(),
            app_field: None,
            root_field: None,
            window_title: None,
            window_width: Some(800),
            window_height: Some(600),
            is_widget_root: false,
            widget_extends: None,
            widget_root_field: None,
            switcher_items: Vec::new(),
            switcher_active_binding: None,
            styles: HashMap::new(),
            unresolved_styles: Vec::new(),
        }
    }

    fn fresh_name(&mut self, prefix: &str) -> String {
        self.counter += 1;
        let p = sanitize_symbol_part(prefix);
        format!("_{}_{}_{}", self.class_tag, p, self.counter)
    }

    fn lower_element(&mut self, elem: &UIElement, parent: Option<&str>) -> String {
        // Root <Widget> element is not a real widget; process its children instead.
        if elem.tag == "Widget" && parent.is_none() {
            return self.lower_widget_root(elem);
        }

        // <WidgetSwitcher> gets special treatment.
        if elem.tag == "WidgetSwitcher" {
            return self.lower_widget_switcher(elem, parent);
        }

        let is_window = elem.tag == "Window";
        let field_name = self.fresh_name(&elem.tag.to_lowercase());
        let create_call = self.widget_create_call(elem);

        self.fields.push(FieldDecl {
            name: field_name.clone(),
            ty: None,
            initializer: None,
            visibility: Visibility::Internal,
            span: S,
        });

        self.init_stmts.push(assign(&field_name, create_call));
        self.apply_attributes(elem, &field_name);

        if is_window && parent.is_none() {
            self.app_field = Some(field_name.clone());
            let children: Vec<UIElement> = elem.children.clone();
            for child in &children {
                let child_field = self.lower_element(child, None);
                if self.root_field.is_none() {
                    self.root_field = Some(child_field);
                }
            }
        } else {
            if let Some(p) = parent {
                self.init_stmts.push(stmt_call(
                    "ui_add_child",
                    vec![ident(p), ident(&field_name)],
                ));
            }
            let children: Vec<UIElement> = elem.children.clone();
            for child in &children {
                self.lower_element(child, Some(&field_name));
            }
        }

        field_name
    }

    /// Handle the root `<Widget>` element (base or derived).
    fn lower_widget_root(&mut self, elem: &UIElement) -> String {
        let is_derived = self.widget_extends.is_some();
        if is_derived {
            // Derived widget: children are appended to the inherited container.
            for child in &elem.children.clone() {
                self.lower_derived_child(child, "_inherited_container");
            }
        } else {
            // Base widget: children form the widget tree normally.
            let children: Vec<UIElement> = elem.children.clone();
            for child in &children {
                let child_field = self.lower_element(child, None);
                if self.widget_root_field.is_none() {
                    self.widget_root_field = Some(child_field);
                }
            }
        }
        String::new()
    }

    /// Lower an element into `derived_stmts` and attach it to `parent`.
    fn lower_derived_child(&mut self, elem: &UIElement, parent: &str) {
        let field_name = self.fresh_name(&elem.tag.to_lowercase());
        let create_call = self.widget_create_call(elem);

        self.fields.push(FieldDecl {
            name: field_name.clone(),
            ty: None,
            initializer: None,
            visibility: Visibility::Internal,
            span: S,
        });

        // Redirect to derived_stmts.
        let before = self.init_stmts.len();
        self.init_stmts.push(assign(&field_name, create_call));
        self.apply_attributes(elem, &field_name);
        self.init_stmts.push(stmt_call(
            "ui_add_child",
            vec![ident(parent), ident(&field_name)],
        ));
        let extra: Vec<Stmt> = self.init_stmts.drain(before..).collect();
        self.derived_stmts.extend(extra);

        // Recurse into children.
        for child in &elem.children.clone() {
            self.lower_derived_child(child, &field_name);
        }
    }

    /// Handle `<WidgetSwitcher Active="{Binding Prop}">` with `<Include Widget="Cls"/>` children.
    fn lower_widget_switcher(&mut self, elem: &UIElement, parent: Option<&str>) -> String {
        let field_name = self.fresh_name("switcher");

        self.fields.push(FieldDecl {
            name: field_name.clone(),
            ty: None,
            initializer: None,
            visibility: Visibility::Internal,
            span: S,
        });

        self.init_stmts
            .push(assign(&field_name, call("ui_stack", vec![])));

        if let Some(p) = parent {
            self.init_stmts.push(stmt_call(
                "ui_add_child",
                vec![ident(p), ident(&field_name)],
            ));
        }

        // Apply layout/style attributes to the switcher container itself.
        self.apply_attributes(elem, &field_name);

        // Capture Active binding property name for deferred updater generation.
        for attr in &elem.attributes {
            if attr.name == "Active" {
                if let UIAttrValue::Binding(b) = &attr.value {
                    self.switcher_active_binding = Some(b.path.clone());
                }
            }
        }

        // Process <Include> children.
        let mut include_idx: usize = 0;
        for child in &elem.children.clone() {
            if child.tag == "Include" {
                if let Some(class_name) = find_literal_attr(child, "Widget") {
                    let inst_field = self.fresh_name("sw_inst");
                    let handle_field = self.fresh_name("sw_handle");

                    self.fields.push(FieldDecl {
                        name: inst_field.clone(),
                        ty: None,
                        initializer: None,
                        visibility: Visibility::Internal,
                        span: S,
                    });
                    self.fields.push(FieldDecl {
                        name: handle_field.clone(),
                        ty: None,
                        initializer: None,
                        visibility: Visibility::Internal,
                        span: S,
                    });

                    // _sw_inst_N = ClassName()
                    self.init_stmts
                        .push(assign(&inst_field, call(&class_name, vec![])));

                    // _sw_inst_N._init_bindings()
                    self.init_stmts
                        .push(stmt_call(&format!("{class_name}::_init_bindings"), vec![]));

                    // _sw_handle_N = ClassName::_build_widget()
                    self.init_stmts.push(assign(
                        &handle_field,
                        call(&format!("{class_name}::_build_widget"), vec![]),
                    ));

                    // ui_add_child(switcher, handle)
                    self.init_stmts.push(stmt_call(
                        "ui_add_child",
                        vec![ident(&field_name), ident(&handle_field)],
                    ));

                    // First child is visible; rest are hidden.
                    let visible: i64 = if include_idx == 0 { 1 } else { 0 };
                    self.init_stmts.push(stmt_call(
                        "ui_set_visible",
                        vec![ident(&handle_field), int_lit(visible)],
                    ));

                    self.switcher_items.push(SwitcherItem {
                        index: include_idx,
                        handle_field: handle_field.clone(),
                    });

                    include_idx += 1;
                }
            }
        }

        field_name
    }

    /// Generate the `_update_<Prop>` method for a WidgetSwitcher's Active binding.
    fn apply_switcher_binding(&mut self) {
        let prop = match self.switcher_active_binding.clone() {
            Some(p) => p,
            None => return,
        };

        let updater_name = format!("_update_{prop}");
        let mut stmts: Vec<Stmt> = Vec::new();

        for item in &self.switcher_items {
            let cmp = Expr::Binary {
                op: BinaryOp::EqEq,
                lhs: Box::new(call(&prop, vec![])),
                rhs: Box::new(int_lit(item.index as i64)),
                span: S,
            };
            stmts.push(stmt_call(
                "ui_set_visible",
                vec![ident(&item.handle_field), cmp],
            ));
        }
        stmts.push(Stmt::Return(None, S));

        self.update_methods.push(FunctionDecl {
            name: updater_name.clone(),
            type_params: Vec::new(),
            params: Vec::new(),
            return_type: None,
            is_public: false,
            is_virtual: false,
            is_override: false,
            is_static: false,
            is_async: false,
            operator: None,
            body: Some(Expr::Block(Block {
                statements: stmts,
                span: S,
            })),
            span: S,
            attributes: Vec::new(),
        });

        self.binding_stmts.push(stmt_call(
            "nex_ui_bind",
            vec![str_lit(&prop), ident(&updater_name)],
        ));
    }

    fn widget_create_call(&mut self, elem: &UIElement) -> Expr {
        match elem.tag.as_str() {
            "Window" => {
                for attr in &elem.attributes {
                    if let UIAttrValue::Literal(v) = &attr.value {
                        match attr.name.as_str() {
                            "Title" => self.window_title = Some(v.clone()),
                            "Width" => self.window_width = v.parse().ok(),
                            "Height" => self.window_height = v.parse().ok(),
                            _ => {}
                        }
                    }
                }
                let title = self
                    .window_title
                    .clone()
                    .unwrap_or_else(|| "Nex App".into());
                let w = self.window_width.unwrap_or(800);
                let h = self.window_height.unwrap_or(600);
                call(
                    "ui_app_create",
                    vec![str_lit(&title), int_lit(w), int_lit(h)],
                )
            }
            "Text" => {
                let text = find_literal_attr(elem, "Text").unwrap_or_default();
                call("ui_text", vec![str_lit(&text)])
            }
            "Button" => {
                let label = find_literal_attr(elem, "Label").unwrap_or_default();
                call("ui_button", vec![str_lit(&label)])
            }
            "TextInput" => {
                let ph = find_literal_attr(elem, "Placeholder").unwrap_or_default();
                call("ui_text_input", vec![str_lit(&ph)])
            }
            "Image" => {
                let src = find_literal_attr(elem, "Source").unwrap_or_default();
                call("ui_image", vec![str_lit(&src)])
            }
            "Checkbox" => {
                let label = find_literal_attr(elem, "Label").unwrap_or_default();
                call("ui_checkbox", vec![str_lit(&label)])
            }
            "Slider" => {
                let min: f64 = find_literal_attr(elem, "Min")
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(0.0);
                let max: f64 = find_literal_attr(elem, "Max")
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(100.0);
                call("ui_slider", vec![float_lit(min), float_lit(max)])
            }
            "Row" => call("ui_row", vec![]),
            "Column" => call("ui_column", vec![]),
            "Stack" => call("ui_stack", vec![]),
            "Scroll" => call("ui_scroll", vec![]),
            "Grid" => {
                let cols = find_literal_attr(elem, "Columns")
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(2);
                call("ui_grid", vec![int_lit(cols)])
            }
            "Canvas" => {
                let w = find_literal_attr(elem, "Width")
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(200);
                let h = find_literal_attr(elem, "Height")
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(200);
                call("ui_canvas", vec![int_lit(w), int_lit(h)])
            }
            _ => call("ui_column", vec![]),
        }
    }

    fn apply_attributes(&mut self, elem: &UIElement, field: &str) {
        // Apply named style setters first so that inline attributes override them.
        if let Some(style_name) = find_literal_attr(elem, "Style") {
            let setters: Vec<UIStyleSetter> = self
                .styles
                .get(&style_name)
                .cloned()
                .unwrap_or_else(|| {
                    self.unresolved_styles.push(style_name.clone());
                    Vec::new()
                });
            for setter in &setters {
                self.apply_literal_attr(field, &setter.property, &setter.value, elem);
            }
        }

        for attr in &elem.attributes {
            // Skip meta / structural attributes.
            match attr.name.as_str() {
                "xmlns" | "Extends" | "Widget" | "Active" | "Style" => continue,
                "Title" | "Width" | "Height" if elem.tag == "Window" => continue,
                _ => {}
            }
            match &attr.value {
                UIAttrValue::Literal(val) => self.apply_literal_attr(field, &attr.name, val, elem),
                UIAttrValue::Binding(binding) => {
                    self.apply_binding(field, &attr.name, binding, elem)
                }
            }
        }
    }

    fn apply_literal_attr(&mut self, field: &str, name: &str, val: &str, elem: &UIElement) {
        match name {
            // Already baked into creation call — skip.
            "Text" | "Label" | "Placeholder" | "Source" | "Min" | "Max" | "Columns"
                if elem.tag != "Window" => {}
            "Click" => {
                self.init_stmts
                    .push(stmt_call("ui_on_click", vec![ident(field), ident(val)]));
            }
            "Change" => {
                self.init_stmts
                    .push(stmt_call("ui_on_change", vec![ident(field), ident(val)]));
            }
            "Hover" => {
                self.init_stmts
                    .push(stmt_call("ui_on_hover", vec![ident(field), ident(val)]));
            }
            "Key" => {
                self.init_stmts
                    .push(stmt_call("ui_on_key", vec![ident(field), ident(val)]));
            }
            "BgColor" => {
                if let Some(v) = parse_color(val) {
                    self.init_stmts
                        .push(stmt_call("ui_set_bg_color", vec![ident(field), int_lit(v)]));
                }
            }
            "FgColor" => {
                if let Some(v) = parse_color(val) {
                    self.init_stmts
                        .push(stmt_call("ui_set_fg_color", vec![ident(field), int_lit(v)]));
                }
            }
            "FontSize" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_font_size",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "Gap" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts
                        .push(stmt_call("ui_set_gap", vec![ident(field), float_lit(v)]));
                }
            }
            "Padding" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_padding_all",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "FlexGrow" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_flex_grow",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "Visible" => {
                let b: i64 = if val == "true" { 1 } else { 0 };
                self.init_stmts
                    .push(stmt_call("ui_set_visible", vec![ident(field), int_lit(b)]));
            }
            "Enabled" => {
                let b: i64 = if val == "true" { 1 } else { 0 };
                self.init_stmts
                    .push(stmt_call("ui_set_enabled", vec![ident(field), int_lit(b)]));
            }
            "Checked" => {
                let b: i64 = if val == "true" { 1 } else { 0 };
                self.init_stmts
                    .push(stmt_call("ui_set_checked", vec![ident(field), int_lit(b)]));
            }
            "Value" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_value_float",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "BorderRadius" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_border_radius",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "BorderWidth" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_border_width",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "BorderColor" => {
                if let Some(v) = parse_color(val) {
                    self.init_stmts.push(stmt_call(
                        "ui_set_border_color",
                        vec![ident(field), int_lit(v)],
                    ));
                }
            }
            "Margin" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_margin_all",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "Width" if elem.tag != "Window" && elem.tag != "Canvas" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_width",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "Height" if elem.tag != "Window" && elem.tag != "Canvas" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_height",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "MinWidth" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_min_width",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "MinHeight" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_min_height",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "MaxWidth" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_max_width",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "MaxHeight" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_max_height",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "FlexShrink" => {
                if let Ok(v) = val.parse::<f64>() {
                    self.init_stmts.push(stmt_call(
                        "ui_set_flex_shrink",
                        vec![ident(field), float_lit(v)],
                    ));
                }
            }
            "AlignItems" => {
                self.init_stmts.push(stmt_call(
                    "ui_set_align_items",
                    vec![ident(field), int_lit(alignment_val(val))],
                ));
            }
            "JustifyContent" => {
                self.init_stmts.push(stmt_call(
                    "ui_set_justify_content",
                    vec![ident(field), int_lit(justification_val(val))],
                ));
            }
            "AlignSelf" => {
                self.init_stmts.push(stmt_call(
                    "ui_set_align_self",
                    vec![ident(field), int_lit(alignment_val(val))],
                ));
            }
            "HorizontalAlignment" => {
                self.init_stmts.push(stmt_call(
                    "ui_set_h_align",
                    vec![ident(field), int_lit(alignment_val(val))],
                ));
            }
            "VerticalAlignment" => {
                self.init_stmts.push(stmt_call(
                    "ui_set_v_align",
                    vec![ident(field), int_lit(alignment_val(val))],
                ));
            }
            _ => {}
        }
    }

    fn apply_binding(
        &mut self,
        field: &str,
        attr_name: &str,
        binding: &UIBinding,
        _elem: &UIElement,
    ) {
        let updater_name = format!("_update_{}", binding.path);
        let updater_body = match attr_name {
            "Text" | "Label" => {
                let value_expr = if binding.mode == BindingMode::TwoWay && attr_name == "Text" {
                    ident(&binding.path)
                } else {
                    call(&binding.path, vec![])
                };
                vec![stmt_call("ui_set_text", vec![ident(field), value_expr])]
            }
            "FgColor" => {
                vec![stmt_call(
                    "ui_set_fg_color",
                    vec![ident(field), call(&binding.path, vec![])],
                )]
            }
            "BgColor" => {
                vec![stmt_call(
                    "ui_set_bg_color",
                    vec![ident(field), call(&binding.path, vec![])],
                )]
            }
            "Visible" => {
                vec![stmt_call(
                    "ui_set_visible",
                    vec![ident(field), call(&binding.path, vec![])],
                )]
            }
            "Enabled" => {
                vec![stmt_call(
                    "ui_set_enabled",
                    vec![ident(field), call(&binding.path, vec![])],
                )]
            }
            "Value" => {
                vec![stmt_call(
                    "ui_set_value_float",
                    vec![ident(field), call(&binding.path, vec![])],
                )]
            }
            _ => vec![],
        };

        if !updater_body.is_empty() {
            // Avoid duplicate update methods for the same property.
            if !self.update_methods.iter().any(|m| m.name == updater_name) {
                let mut body_stmts = updater_body;
                body_stmts.push(Stmt::Return(None, S));
                self.update_methods.push(FunctionDecl {
                    name: updater_name.clone(),
                    type_params: Vec::new(),
                    params: Vec::new(),
                    return_type: None,
                    is_public: false,
                    is_virtual: false,
                    is_override: false,
                    is_static: false,
                    is_async: false,
                    operator: None,
                    body: Some(Expr::Block(Block {
                        statements: body_stmts,
                        span: S,
                    })),
                    span: S,
                    attributes: Vec::new(),
                });
            }

            self.binding_stmts.push(stmt_call(
                "nex_ui_bind",
                vec![str_lit(&binding.path), ident(&updater_name)],
            ));
        }

        if binding.mode == BindingMode::TwoWay && attr_name == "Text" {
            let setter_name = format!("_twoway_{}", binding.path);
            let get_text = call("ui_get_text", vec![ident("widget_id")]);
            let copy_via_concat = Expr::Binary {
                op: nexc_ast::BinaryOp::Add,
                lhs: Box::new(Expr::Literal {
                    value: Literal::String(String::new()),
                    span: S,
                }),
                rhs: Box::new(get_text),
                span: S,
            };
            let setter_body = vec![
                Stmt::Expr(Expr::Assign {
                    target: Box::new(ident(&binding.path)),
                    value: Box::new(copy_via_concat),
                    op: AssignOp::Assign,
                    span: S,
                }),
                Stmt::Return(None, S),
            ];
            if !self.update_methods.iter().any(|m| m.name == setter_name) {
                self.update_methods.push(FunctionDecl {
                    name: setter_name.clone(),
                    type_params: Vec::new(),
                    params: vec![
                        ParamDecl {
                            name: "widget_id".into(),
                            type_hint: None,
                            default_value: None,
                            span: S,
                        },
                        ParamDecl {
                            name: "event_kind".into(),
                            type_hint: None,
                            default_value: None,
                            span: S,
                        },
                    ],
                    return_type: None,
                    is_public: false,
                    is_virtual: false,
                    is_override: false,
                    is_static: false,
                    is_async: false,
                    operator: None,
                    body: Some(Expr::Block(Block {
                        statements: setter_body,
                        span: S,
                    })),
                    span: S,
                    attributes: Vec::new(),
                });
            }
            self.init_stmts.push(stmt_call(
                "ui_on_change",
                vec![ident(field), ident(&setter_name)],
            ));
        }
    }

    // -----------------------------------------------------------------------
    // Method builders — Window
    // -----------------------------------------------------------------------

    fn build_init_ui(&self) -> FunctionDecl {
        let mut stmts = self.init_stmts.clone();
        if let (Some(app), Some(root)) = (&self.app_field, &self.root_field) {
            stmts.push(stmt_call("ui_app_set_root", vec![ident(app), ident(root)]));
        }
        stmts.push(Stmt::Return(None, S));
        func("_init_ui", stmts, true, false, false)
    }

    fn build_setup_bindings(&self) -> FunctionDecl {
        let mut stmts = self.binding_stmts.clone();
        stmts.push(Stmt::Return(None, S));
        func("_setup_bindings", stmts, true, false, false)
    }

    /// Widget-class variant: named `_init_bindings` to avoid colliding with the
    /// Window-class `_setup_bindings` during unqualified JIT symbol resolution.
    fn build_widget_bindings(&self) -> FunctionDecl {
        let mut stmts = self.binding_stmts.clone();
        stmts.push(Stmt::Return(None, S));
        func("_init_bindings", stmts, true, false, false)
    }

    fn build_run_app(&self) -> FunctionDecl {
        let mut stmts = Vec::new();
        if let Some(app) = &self.app_field {
            stmts.push(stmt_call("ui_app_run", vec![ident(app)]));
            stmts.push(stmt_call("ui_app_destroy", vec![ident(app)]));
        }
        stmts.push(Stmt::Return(None, S));
        func("_run_app", stmts, true, false, false)
    }

    // -----------------------------------------------------------------------
    // Method builders — Widget
    // -----------------------------------------------------------------------

    /// `public def _create_container() -> Int64` — builds the widget tree and returns its root.
    fn build_create_container(&self) -> FunctionDecl {
        let mut stmts = self.init_stmts.clone();
        let fallback_root = format!("_{}_column_1", self.class_tag);
        let ret = self.widget_root_field.as_deref().unwrap_or(&fallback_root);
        stmts.push(Stmt::Return(Some(ident(ret)), S));
        func_ret("_create_container", stmts, true, false, false)
    }

    /// `public virtual def _build_widget() -> Int64` — delegates to `_create_container`.
    fn build_virtual_build_widget(&self) -> FunctionDecl {
        let stmts = vec![Stmt::Return(Some(call("_create_container", vec![])), S)];
        func_ret("_build_widget", stmts, true, true, false)
    }

    /// `public override def _build_widget() -> Int64` — calls inherited `_create_container`,
    /// then appends derived children, and returns the container.
    fn build_override_build_widget(&self) -> FunctionDecl {
        // Declare a local variable for the inherited container.
        let mut stmts: Vec<Stmt> = vec![Stmt::VarDecl(VarDecl {
            name: "_inherited_container".into(),
            inferred_type: None,
            explicit_type: None,
            initializer: Some(call("_create_container", vec![])),
            is_dynamic: false,
            visibility: Visibility::Internal,
            attributes: Vec::new(),
            span: S,
        })];
        stmts.extend(self.derived_stmts.clone());
        stmts.push(Stmt::Return(Some(ident("_inherited_container")), S));
        func_ret("_build_widget", stmts, true, false, true)
    }

    fn build_update_methods(&self) -> Vec<FunctionDecl> {
        self.update_methods.clone()
    }
}

// ---------------------------------------------------------------------------
// AST construction helpers
// ---------------------------------------------------------------------------

fn sanitize_symbol_part(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        "x".to_string()
    } else {
        out
    }
}

fn find_literal_attr(elem: &UIElement, name: &str) -> Option<String> {
    elem.attributes
        .iter()
        .find(|a| a.name == name)
        .and_then(|a| {
            if let UIAttrValue::Literal(s) = &a.value {
                Some(s.clone())
            } else {
                None
            }
        })
}

fn ident(name: &str) -> Expr {
    Expr::Identifier {
        name: name.to_string(),
        span: S,
    }
}

fn str_lit(s: &str) -> Expr {
    Expr::Literal {
        value: Literal::String(s.to_string()),
        span: S,
    }
}

fn int_lit(v: i64) -> Expr {
    Expr::Literal {
        value: Literal::Int(v),
        span: S,
    }
}

fn float_lit(v: f64) -> Expr {
    Expr::Literal {
        value: Literal::Float(v),
        span: S,
    }
}

fn call(name: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        callee: Box::new(ident(name)),
        type_args: vec![],
        args,
        span: S,
    }
}

fn stmt_call(name: &str, args: Vec<Expr>) -> Stmt {
    Stmt::Expr(call(name, args))
}

fn assign(dst: &str, value: Expr) -> Stmt {
    Stmt::Expr(Expr::Assign {
        target: Box::new(ident(dst)),
        value: Box::new(value),
        op: AssignOp::Assign,
        span: S,
    })
}

/// Build a `FunctionDecl` returning nothing.
fn func(
    name: &str,
    stmts: Vec<Stmt>,
    is_public: bool,
    is_virtual: bool,
    is_override: bool,
) -> FunctionDecl {
    FunctionDecl {
        name: name.into(),
        type_params: Vec::new(),
        params: Vec::new(),
        return_type: None,
        is_public,
        is_virtual,
        is_override,
        is_static: false,
        is_async: false,
        operator: None,
        body: Some(Expr::Block(Block {
            statements: stmts,
            span: S,
        })),
        span: S,
        attributes: Vec::new(),
    }
}

/// Build a `FunctionDecl` returning `Int64`.
fn func_ret(
    name: &str,
    stmts: Vec<Stmt>,
    is_public: bool,
    is_virtual: bool,
    is_override: bool,
) -> FunctionDecl {
    FunctionDecl {
        name: name.into(),
        type_params: Vec::new(),
        params: Vec::new(),
        return_type: Some(TypeExpr {
            span: S,
            kind: TypeExprKind::Named("Int64".into()),
        }),
        is_public,
        is_virtual,
        is_override,
        is_static: false,
        is_async: false,
        operator: None,
        body: Some(Expr::Block(Block {
            statements: stmts,
            span: S,
        })),
        span: S,
        attributes: Vec::new(),
    }
}

/// Convert an alignment string to its integer encoding.
/// Matches `Alignment::from_i64` in the runtime: 0=Start, 1=Center, 2=End, 3=Stretch.
fn alignment_val(val: &str) -> i64 {
    match val.to_ascii_lowercase().as_str() {
        "center" => 1,
        "end" | "right" | "bottom" => 2,
        "stretch" => 3,
        _ => 0, // start / left / top
    }
}

/// Convert a justification string to its integer encoding.
/// Matches `Justification::from_i64` in the runtime.
fn justification_val(val: &str) -> i64 {
    match val.to_ascii_lowercase().as_str() {
        "center" => 1,
        "end" => 2,
        "spacebetween" | "space-between" => 3,
        "spacearound" | "space-around" => 4,
        "spaceevenly" | "space-evenly" => 5,
        _ => 0, // start
    }
}

/// Parse a color value from either `0xRRGGBBAA` hex or plain decimal.
fn parse_color(val: &str) -> Option<i64> {
    let trimmed = val.trim();
    if let Some(hex) = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
    {
        i64::from_str_radix(hex, 16).ok()
    } else {
        trimmed.parse::<i64>().ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lower_simple_window() {
        let src = r#"<Window xmlns="std.ui" Title="Test" Width="800" Height="600">
  <Column>
    <Text Text="Hello" />
    <Button Label="Click" Click="on_click" />
  </Column>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "Test.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert_eq!(cls.name, "Test");
        assert!(cls.is_partial);
        assert!(cls.methods.iter().any(|m| m.name == "_init_ui"));
        assert!(cls.methods.iter().any(|m| m.name == "_setup_bindings"));
        assert!(cls.methods.iter().any(|m| m.name == "_run_app"));
    }

    #[test]
    fn lower_binding_generates_updater() {
        let src = r#"<Window xmlns="std.ui">
  <Text Text="{Binding CounterText}" />
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "Bind.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert!(cls.methods.iter().any(|m| m.name == "_update_CounterText"));
    }

    #[test]
    fn twoway_text_setter_does_not_notify_changed() {
        let src = r#"<Window xmlns="std.ui">
  <TextInput Text="{Binding name_value, Mode=TwoWay}" />
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "BindTwoWay.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        let setter = cls
            .methods
            .iter()
            .find(|m| m.name == "_twoway_name_value")
            .expect("two-way setter should be generated");
        let body = setter.body.as_ref().expect("setter body should exist");
        let Expr::Block(block) = body else {
            panic!("setter body should be block")
        };
        let has_notify = block.statements.iter().any(|stmt| match stmt {
            Stmt::Expr(Expr::Call { callee, .. }) => {
                matches!(callee.as_ref(), Expr::Identifier { name, .. } if name == "notify_changed")
            }
            _ => false,
        });
        assert!(!has_notify, "two-way setter must not call notify_changed");
    }

    #[test]
    fn lower_base_widget() {
        let src = r#"<Widget xmlns="std.ui">
  <Column Padding="16" Gap="12">
    <Text Text="Hello from Widget" />
  </Column>
</Widget>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "BaseWidget.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert_eq!(cls.name, "BaseWidget");
        assert!(cls.is_partial);
        assert!(cls.base_specs.is_empty());
        assert!(cls.methods.iter().any(|m| m.name == "_create_container"));
        let build = cls
            .methods
            .iter()
            .find(|m| m.name == "_build_widget")
            .unwrap();
        assert!(build.is_virtual);
        assert!(!build.is_override);
    }

    #[test]
    fn lower_derived_widget() {
        let src = r#"<Widget xmlns="std.ui" Extends="BaseWidget">
  <Button Label="Extra" Click="on_extra" />
</Widget>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "DerivedWidget.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert_eq!(cls.name, "DerivedWidget");
        assert_eq!(cls.base_specs.len(), 1);
        assert_eq!(cls.base_specs[0].name, "BaseWidget");
        assert!(
            !cls.methods.iter().any(|m| m.name == "_create_container"),
            "_create_container must not be generated for derived widgets"
        );
        let build = cls
            .methods
            .iter()
            .find(|m| m.name == "_build_widget")
            .unwrap();
        assert!(!build.is_virtual);
        assert!(build.is_override);
    }

    #[test]
    fn lower_widget_switcher_generates_active_updater() {
        let src = r#"<Window xmlns="std.ui" Title="App" Width="800" Height="600">
  <WidgetSwitcher Active="{Binding ActiveScreen}">
    <Include Widget="HomeWidget" />
    <Include Widget="CounterWidget" />
  </WidgetSwitcher>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "Main.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert!(
            cls.methods.iter().any(|m| m.name == "_update_ActiveScreen"),
            "should generate _update_ActiveScreen for WidgetSwitcher Active binding"
        );
    }

    #[test]
    fn named_style_emits_setters_before_inline() {
        let src = r#"<Window xmlns="std.ui" Title="App">
  <Window.Styles>
    <Style Name="Accent" TargetType="Button">
      <Setter Property="BgColor" Value="0x3B82F6FF"/>
      <Setter Property="BorderRadius" Value="6"/>
    </Style>
  </Window.Styles>
  <Column>
    <Button Style="Accent" Label="OK" BgColor="0xEF4444FF" Click="on_ok"/>
  </Column>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "App.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert!(!sink.has_errors());

        let init = cls.methods.iter().find(|m| m.name == "_init_ui").unwrap();
        let Expr::Block(block) = init.body.as_ref().unwrap() else {
            panic!("_init_ui must be a block");
        };

        // Collect all ui_set_bg_color call positions.
        let bg_positions: Vec<usize> = block
            .statements
            .iter()
            .enumerate()
            .filter_map(|(i, stmt)| match stmt {
                Stmt::Expr(Expr::Call { callee, .. }) => {
                    if matches!(callee.as_ref(),
                        Expr::Identifier { name, .. } if name == "ui_set_bg_color") {
                        Some(i)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect();
        // Both style and inline emit ui_set_bg_color; inline must come last.
        assert_eq!(bg_positions.len(), 2, "should have two ui_set_bg_color calls");
        assert!(
            bg_positions[0] < bg_positions[1],
            "style setter must precede inline attribute"
        );

        // BorderRadius from the style should be emitted.
        let has_border_radius = block.statements.iter().any(|stmt| match stmt {
            Stmt::Expr(Expr::Call { callee, .. }) => {
                matches!(callee.as_ref(),
                    Expr::Identifier { name, .. } if name == "ui_set_border_radius")
            }
            _ => false,
        });
        assert!(has_border_radius, "style setter for BorderRadius should be lowered");
    }

    #[test]
    fn unresolved_style_emits_warning_not_error() {
        let src = r#"<Window xmlns="std.ui">
  <Column>
    <Button Style="Nonexistent" Label="X" />
  </Column>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "App.nexui", &mut sink).unwrap();
        let _cls = lower_document(&doc, &mut sink);
        assert!(!sink.has_errors(), "unresolved style should not be a hard error");
        assert!(
            sink.diagnostics()
                .iter()
                .any(|d| d.severity == Severity::Warning),
            "unresolved style should emit a warning"
        );
    }

    #[test]
    fn missing_layout_attrs_are_lowered() {
        let src = r#"<Window xmlns="std.ui">
  <Column MaxWidth="400" MinHeight="200" FlexShrink="0" AlignItems="center">
    <Button Margin="8" BorderRadius="4" BorderWidth="1" BorderColor="0xFF0000FF"
            MaxHeight="48" AlignSelf="stretch" />
  </Column>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "Attrs.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert!(!sink.has_errors());

        let init = cls.methods.iter().find(|m| m.name == "_init_ui").unwrap();
        let Expr::Block(block) = init.body.as_ref().unwrap() else {
            panic!("must be block");
        };
        let calls: Vec<&str> = block
            .statements
            .iter()
            .filter_map(|s| match s {
                Stmt::Expr(Expr::Call { callee, .. }) => {
                    if let Expr::Identifier { name, .. } = callee.as_ref() {
                        Some(name.as_str())
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect();
        for expected in &[
            "ui_set_max_width",
            "ui_set_min_height",
            "ui_set_flex_shrink",
            "ui_set_align_items",
            "ui_set_margin_all",
            "ui_set_border_radius",
            "ui_set_border_width",
            "ui_set_border_color",
            "ui_set_max_height",
            "ui_set_align_self",
        ] {
            assert!(
                calls.contains(expected),
                "expected call to {expected} but it was not generated"
            );
        }
    }

    #[test]
    fn horizontal_and_vertical_alignment_are_lowered() {
        let src = r#"<Window xmlns="std.ui">
  <Column>
    <Row HorizontalAlignment="Right" VerticalAlignment="Center" />
  </Column>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "Align.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert!(!sink.has_errors());

        let init = cls.methods.iter().find(|m| m.name == "_init_ui").unwrap();
        let Expr::Block(block) = init.body.as_ref().unwrap() else {
            panic!("must be block");
        };
        let calls: Vec<(&str, i64)> = block
            .statements
            .iter()
            .filter_map(|s| match s {
                Stmt::Expr(Expr::Call { callee, args, .. }) => {
                    if let Expr::Identifier { name, .. } = callee.as_ref() {
                        if (name == "ui_set_h_align" || name == "ui_set_v_align") && args.len() == 2 {
                            if let Expr::Literal { value: Literal::Int(v), .. } = &args[1] {
                                return Some((name.as_str(), *v));
                            }
                        }
                    }
                    None
                }
                _ => None,
            })
            .collect();

        assert!(calls.iter().any(|&(n, v)| n == "ui_set_h_align" && v == 2), // 2 = End/Right
            "HorizontalAlignment=\"Right\" should emit ui_set_h_align with value 2");
        assert!(calls.iter().any(|&(n, v)| n == "ui_set_v_align" && v == 1), // 1 = Center
            "VerticalAlignment=\"Center\" should emit ui_set_v_align with value 1");
    }

    #[test]
    fn generated_widget_fields_are_class_scoped() {
        let src = r#"<Window xmlns="std.ui"><Column><Text Text="Hi" /></Column></Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "MainWindow.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        assert!(
            cls.fields
                .iter()
                .all(|f| f.name.starts_with("_mainwindow_")),
            "all generated fields should be prefixed with the class tag"
        );
    }

    #[test]
    fn switcher_include_calls_are_class_qualified() {
        let src = r#"<Window xmlns="std.ui">
  <WidgetSwitcher Active="{Binding ActiveScreen}">
    <Include Widget="HomeWidget" />
  </WidgetSwitcher>
</Window>"#;
        let mut sink = DiagnosticSink::new();
        let doc = nexui_parse::parse_nexui(src, "Main.nexui", &mut sink).unwrap();
        let cls = lower_document(&doc, &mut sink);
        let init = cls
            .methods
            .iter()
            .find(|m| m.name == "_init_ui")
            .expect("expected _init_ui");
        let body = init.body.as_ref().expect("expected body");
        let Expr::Block(block) = body else {
            panic!("_init_ui must be a block");
        };

        let has_qualified_setup = block.statements.iter().any(|stmt| match stmt {
            Stmt::Expr(Expr::Call { callee, .. }) => {
                matches!(callee.as_ref(), Expr::Identifier { name, .. } if name == "HomeWidget::_init_bindings")
            }
            _ => false,
        });
        assert!(
            has_qualified_setup,
            "include lowering should call HomeWidget::_init_bindings()"
        );

        let has_qualified_build = block.statements.iter().any(|stmt| match stmt {
            Stmt::Expr(Expr::Assign { value, .. }) => {
                matches!(value.as_ref(),
                    Expr::Call { callee, .. }
                    if matches!(callee.as_ref(), Expr::Identifier { name, .. } if name == "HomeWidget::_build_widget")
                )
            }
            _ => false,
        });
        assert!(
            has_qualified_build,
            "include lowering should call HomeWidget::_build_widget()"
        );
    }
}

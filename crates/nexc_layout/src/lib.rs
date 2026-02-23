use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use nexc_ast::{ClassDecl, Item};
use nexc_diag::DiagnosticSink;

#[derive(Debug, Clone)]
pub struct ClassLayout {
    pub class_name: String,
    pub size: usize,
    pub align: usize,
    pub pointer_map: Vec<usize>,
    pub base_offsets: HashMap<String, usize>,
    pub vtable_slots: Vec<String>,
}

impl ClassLayout {
    pub fn hash(&self) -> String {
        let mut hasher = DefaultHasher::new();
        self.class_name.hash(&mut hasher);
        self.size.hash(&mut hasher);
        self.align.hash(&mut hasher);
        self.pointer_map.hash(&mut hasher);
        self.vtable_slots.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }
}

pub fn compute_layouts(module: &nexc_type::TypedModule) -> Vec<ClassLayout> {
    let mut out = Vec::new();
    for item in &module.file.items {
        if let Item::Class(class) = item {
            out.push(layout_class(class));
        }
    }
    out
}

pub fn layout_class(class: &ClassDecl) -> ClassLayout {
    let mut base_offsets = HashMap::new();
    let mut cursor = 0usize;
    for base in &class.base_specs {
        if base.shared && base_offsets.contains_key(&base.name) {
            continue;
        }
        base_offsets.insert(base.name.clone(), cursor);
        cursor += 16;
    }
    ClassLayout {
        class_name: class.name.clone(),
        size: cursor + class.fields.len() * 8 + class.methods.len() * 8,
        align: 8,
        pointer_map: Vec::new(),
        base_offsets,
        vtable_slots: class.methods.iter().map(|m| m.name.clone()).collect(),
    }
}

pub fn validate_layout_for_diamond(_classes: &[ClassDecl], _sink: &mut DiagnosticSink) {}

#[cfg(test)]
mod tests {
    use super::*;
    use nexc_ast::{BaseSpec, Visibility};
    use nexc_diag::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn empty_class(name: &str, bases: Vec<BaseSpec>) -> ClassDecl {
        ClassDecl {
            name: name.to_string(),
            is_partial: false,
            visibility: Visibility::Internal,
            type_params: Vec::new(),
            base_specs: bases,
            fields: Vec::new(),
            methods: Vec::new(),
            span: span(),
        }
    }

    #[test]
    fn shared_base_collapses_duplicate_layout_slot() {
        let class = empty_class(
            "D",
            vec![
                BaseSpec {
                    name: "A".to_string(),
                    shared: true,
                    ctor_args: Vec::new(),
                    span: span(),
                },
                BaseSpec {
                    name: "A".to_string(),
                    shared: true,
                    ctor_args: Vec::new(),
                    span: span(),
                },
            ],
        );
        let layout = layout_class(&class);
        assert_eq!(layout.base_offsets.len(), 1);
        assert!(layout.base_offsets.contains_key("A"));
    }
}

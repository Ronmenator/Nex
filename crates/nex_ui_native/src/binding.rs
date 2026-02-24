//! MVVM binding engine — maps property names to update callbacks.
//!
//! When `nex_ui_notify_changed("Foo")` is called, all callbacks registered for
//! property `"Foo"` are invoked, refreshing the bound UI widgets.

use std::collections::HashMap;

pub type BindingCallback = unsafe extern "C" fn();

#[derive(Default)]
pub struct BindingRegistry {
    bindings: HashMap<String, Vec<BindingCallback>>,
}

impl BindingRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn bind(&mut self, property: &str, callback: BindingCallback) {
        self.bindings
            .entry(property.to_string())
            .or_default()
            .push(callback);
    }

    pub fn unbind(&mut self, property: &str) {
        self.bindings.remove(property);
    }

    pub fn notify(&self, property: &str) {
        // #region agent log
        { use std::io::Write; let path = std::path::Path::new(r"d:\Development\Personal\Nex\debug-8a9f05.log"); if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(path) { let _ = writeln!(f, r#"{{"sessionId":"8a9f05","hypothesisId":"H3","location":"binding::notify","message":"invoking","data":{{"property":"{}","count":{}}}}}"#, property, self.bindings.get(property).map(|v| v.len()).unwrap_or(0)); } }
        // #endregion
        if let Some(callbacks) = self.bindings.get(property) {
            for cb in callbacks {
                unsafe { cb() };
            }
        }
    }

    pub fn clear(&mut self) {
        self.bindings.clear();
    }
}

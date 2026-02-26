use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::sync::Mutex;

// ---------------------------------------------------------------------------
// Metadata types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub type_name: String,
    pub is_public: bool,
    /// Address of the global variable backing this field (set after JIT).
    pub addr: usize,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub param_types: Vec<String>,
    pub return_type: String,
    pub is_static: bool,
    pub is_virtual: bool,
    pub func_ptr: usize,
}

#[derive(Debug, Clone)]
pub struct VariantInfo {
    pub name: String,
    pub ordinal: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i64)]
pub enum TypeKind {
    Class = 0,
    Struct = 1,
    Enum = 2,
    Interface = 3,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub name: String,
    pub module: String,
    pub kind: TypeKind,
    pub is_reflectable: bool,
    pub base_classes: Vec<String>,
    pub interfaces: Vec<String>,
    pub fields: Vec<FieldInfo>,
    pub methods: Vec<MethodInfo>,
    pub variants: Vec<VariantInfo>,
}

// ---------------------------------------------------------------------------
// Global registry
// ---------------------------------------------------------------------------

struct ReflectRegistry {
    types: Vec<TypeInfo>,
    name_to_id: HashMap<String, usize>,
}

impl ReflectRegistry {
    fn new() -> Self {
        Self {
            types: Vec::new(),
            name_to_id: HashMap::new(),
        }
    }
}

static REGISTRY: Mutex<Option<ReflectRegistry>> = Mutex::new(None);

/// Pre-built map of global variable names → addresses, populated by codegen
/// after JIT finalization.  Used for lazy field address resolution.
static GLOBAL_ADDRS: Mutex<Option<HashMap<String, usize>>> = Mutex::new(None);

/// Called from codegen after JIT finalization to store the global address map.
/// Field addresses are resolved lazily when first accessed.
pub fn set_global_addrs(addrs: HashMap<String, usize>) {
    let mut guard = GLOBAL_ADDRS.lock().unwrap();
    *guard = Some(addrs);
}

fn with_registry<F, R>(f: F) -> R
where
    F: FnOnce(&mut ReflectRegistry) -> R,
{
    let mut guard = REGISTRY.lock().unwrap();
    if guard.is_none() {
        *guard = Some(ReflectRegistry::new());
    }
    f(guard.as_mut().unwrap())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

unsafe fn cstr_to_string(p: *const c_char) -> String {
    if p.is_null() {
        return String::new();
    }
    CStr::from_ptr(p).to_string_lossy().into_owned()
}

fn leak_cstring(s: &str) -> *mut c_char {
    CString::new(s).unwrap_or_default().into_raw()
}

// ---------------------------------------------------------------------------
// Registration functions (called from generated module-init code)
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_register_type(
    name: *const c_char,
    module: *const c_char,
    kind: i64,
    is_reflectable: i64,
) -> i64 {
    let name_s = cstr_to_string(name);
    let module_s = cstr_to_string(module);
    let type_kind = match kind {
        0 => TypeKind::Class,
        1 => TypeKind::Struct,
        2 => TypeKind::Enum,
        3 => TypeKind::Interface,
        _ => TypeKind::Class,
    };
    with_registry(|reg| {
        if let Some(&id) = reg.name_to_id.get(&name_s) {
            return id as i64;
        }
        let id = reg.types.len();
        reg.name_to_id.insert(name_s.clone(), id);
        reg.types.push(TypeInfo {
            name: name_s,
            module: module_s,
            kind: type_kind,
            is_reflectable: is_reflectable != 0,
            base_classes: Vec::new(),
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            variants: Vec::new(),
        });
        id as i64
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_add_base(type_id: i64, base_name: *const c_char) {
    let base = cstr_to_string(base_name);
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            info.base_classes.push(base);
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_add_interface(type_id: i64, iface_name: *const c_char) {
    let iface = cstr_to_string(iface_name);
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            info.interfaces.push(iface);
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_add_field(
    type_id: i64,
    name: *const c_char,
    type_name: *const c_char,
    is_public: i64,
) {
    let n = cstr_to_string(name);
    let t = cstr_to_string(type_name);
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            info.fields.push(FieldInfo {
                name: n,
                type_name: t,
                is_public: is_public != 0,
                addr: 0,
            });
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_add_method(
    type_id: i64,
    name: *const c_char,
    return_type: *const c_char,
    param_count: i64,
    is_static: i64,
    is_virtual: i64,
) {
    let n = cstr_to_string(name);
    let rt = cstr_to_string(return_type);
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            info.methods.push(MethodInfo {
                name: n,
                param_types: Vec::new(),
                return_type: rt,
                is_static: is_static != 0,
                is_virtual: is_virtual != 0,
                func_ptr: 0,
            });
        }
    });
    let _ = param_count; // param_types populated later if needed
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_add_variant(
    type_id: i64,
    name: *const c_char,
    ordinal: i64,
) {
    let n = cstr_to_string(name);
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            info.variants.push(VariantInfo {
                name: n,
                ordinal,
            });
        }
    });
}

// ---------------------------------------------------------------------------
// Query functions (called from user Nex code via Reflect.*)
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_find_type(name: *const c_char) -> i64 {
    let name_s = cstr_to_string(name);
    with_registry(|reg| {
        reg.name_to_id.get(&name_s).map(|&id| id as i64).unwrap_or(-1)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_name(type_id: i64) -> *mut c_char {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .map(|t| leak_cstring(&t.name))
            .unwrap_or(leak_cstring(""))
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_module(type_id: i64) -> *mut c_char {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .map(|t| leak_cstring(&t.module))
            .unwrap_or(leak_cstring(""))
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_kind(type_id: i64) -> i64 {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .map(|t| t.kind as i64)
            .unwrap_or(-1)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_field_count(type_id: i64) -> i64 {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .map(|t| {
                if t.is_reflectable {
                    t.fields.len() as i64
                } else {
                    0
                }
            })
            .unwrap_or(0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_field_name(type_id: i64, index: i64) -> *mut c_char {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .and_then(|t| {
                if t.is_reflectable {
                    t.fields.get(index as usize)
                } else {
                    None
                }
            })
            .map(|f| leak_cstring(&f.name))
            .unwrap_or(leak_cstring(""))
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_field_type(type_id: i64, index: i64) -> *mut c_char {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .and_then(|t| {
                if t.is_reflectable {
                    t.fields.get(index as usize)
                } else {
                    None
                }
            })
            .map(|f| leak_cstring(&f.type_name))
            .unwrap_or(leak_cstring(""))
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_method_count(type_id: i64) -> i64 {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .map(|t| {
                if t.is_reflectable {
                    t.methods.len() as i64
                } else {
                    0
                }
            })
            .unwrap_or(0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_method_name(type_id: i64, index: i64) -> *mut c_char {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .and_then(|t| {
                if t.is_reflectable {
                    t.methods.get(index as usize)
                } else {
                    None
                }
            })
            .map(|m| leak_cstring(&m.name))
            .unwrap_or(leak_cstring(""))
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_method_return_type(
    type_id: i64,
    index: i64,
) -> *mut c_char {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .and_then(|t| {
                if t.is_reflectable {
                    t.methods.get(index as usize)
                } else {
                    None
                }
            })
            .map(|m| leak_cstring(&m.return_type))
            .unwrap_or(leak_cstring(""))
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_implements(
    type_id: i64,
    iface_name: *const c_char,
) -> i64 {
    let iface = cstr_to_string(iface_name);
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .map(|t| {
                if t.interfaces.iter().any(|i| i == &iface) {
                    1i64
                } else if t.base_classes.iter().any(|b| b == &iface) {
                    1i64
                } else {
                    0i64
                }
            })
            .unwrap_or(0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_is_reflectable(type_id: i64) -> i64 {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .map(|t| if t.is_reflectable { 1i64 } else { 0i64 })
            .unwrap_or(0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_count() -> i64 {
    with_registry(|reg| reg.types.len() as i64)
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_name_at(index: i64) -> *mut c_char {
    with_registry(|reg| {
        reg.types
            .get(index as usize)
            .map(|t| leak_cstring(&t.name))
            .unwrap_or(leak_cstring(""))
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_type_interfaces(type_id: i64) -> *mut c_char {
    with_registry(|reg| {
        reg.types
            .get(type_id as usize)
            .map(|t| leak_cstring(&t.interfaces.join(",")))
            .unwrap_or(leak_cstring(""))
    })
}

// ---------------------------------------------------------------------------
// Dynamic invocation (Phase 4)
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_set_method_ptr(
    type_id: i64,
    method_index: i64,
    func_ptr: i64,
) {
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            if let Some(method) = info.methods.get_mut(method_index as usize) {
                method.func_ptr = func_ptr as usize;
            }
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_invoke(
    type_id: i64,
    method_name: *const c_char,
    args: *const i64,
    arg_count: i64,
) -> i64 {
    let mname = cstr_to_string(method_name);
    let fptr = with_registry(|reg| {
        reg.types.get(type_id as usize).and_then(|t| {
            t.methods
                .iter()
                .find(|m| m.name == mname)
                .map(|m| m.func_ptr)
        })
    });
    let Some(fp) = fptr else { return 0 };
    if fp == 0 {
        return 0;
    }
    match arg_count {
        0 => {
            let f: extern "C" fn() -> i64 = std::mem::transmute(fp);
            f()
        }
        1 => {
            let f: extern "C" fn(i64) -> i64 = std::mem::transmute(fp);
            f(*args)
        }
        2 => {
            let f: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(fp);
            f(*args, *args.add(1))
        }
        3 => {
            let f: extern "C" fn(i64, i64, i64) -> i64 = std::mem::transmute(fp);
            f(*args, *args.add(1), *args.add(2))
        }
        4 => {
            let f: extern "C" fn(i64, i64, i64, i64) -> i64 = std::mem::transmute(fp);
            f(*args, *args.add(1), *args.add(2), *args.add(3))
        }
        5 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(fp);
            f(*args, *args.add(1), *args.add(2), *args.add(3), *args.add(4))
        }
        6 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(fp);
            f(
                *args,
                *args.add(1),
                *args.add(2),
                *args.add(3),
                *args.add(4),
                *args.add(5),
            )
        }
        7 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                std::mem::transmute(fp);
            f(
                *args,
                *args.add(1),
                *args.add(2),
                *args.add(3),
                *args.add(4),
                *args.add(5),
                *args.add(6),
            )
        }
        8 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                std::mem::transmute(fp);
            f(
                *args,
                *args.add(1),
                *args.add(2),
                *args.add(3),
                *args.add(4),
                *args.add(5),
                *args.add(6),
                *args.add(7),
            )
        }
        _ => 0,
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_create_instance(
    type_id: i64,
    args: *const i64,
    arg_count: i64,
) -> i64 {
    // Find the ::init method and call it
    let fptr = with_registry(|reg| {
        reg.types.get(type_id as usize).and_then(|t| {
            t.methods
                .iter()
                .find(|m| m.name == "init")
                .map(|m| m.func_ptr)
        })
    });
    let Some(fp) = fptr else { return 0 };
    if fp == 0 {
        return 0;
    }
    // Reuse the same trampoline pattern
    nex_reflect_invoke_raw(fp, args, arg_count)
}

unsafe fn nex_reflect_invoke_raw(fp: usize, args: *const i64, arg_count: i64) -> i64 {
    match arg_count {
        0 => {
            let f: extern "C" fn() -> i64 = std::mem::transmute(fp);
            f()
        }
        1 => {
            let f: extern "C" fn(i64) -> i64 = std::mem::transmute(fp);
            f(*args)
        }
        2 => {
            let f: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(fp);
            f(*args, *args.add(1))
        }
        3 => {
            let f: extern "C" fn(i64, i64, i64) -> i64 = std::mem::transmute(fp);
            f(*args, *args.add(1), *args.add(2))
        }
        4 => {
            let f: extern "C" fn(i64, i64, i64, i64) -> i64 = std::mem::transmute(fp);
            f(*args, *args.add(1), *args.add(2), *args.add(3))
        }
        5 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(fp);
            f(*args, *args.add(1), *args.add(2), *args.add(3), *args.add(4))
        }
        6 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(fp);
            f(
                *args,
                *args.add(1),
                *args.add(2),
                *args.add(3),
                *args.add(4),
                *args.add(5),
            )
        }
        7 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                std::mem::transmute(fp);
            f(
                *args,
                *args.add(1),
                *args.add(2),
                *args.add(3),
                *args.add(4),
                *args.add(5),
                *args.add(6),
            )
        }
        8 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                std::mem::transmute(fp);
            f(
                *args,
                *args.add(1),
                *args.add(2),
                *args.add(3),
                *args.add(4),
                *args.add(5),
                *args.add(6),
                *args.add(7),
            )
        }
        _ => 0,
    }
}

// ---------------------------------------------------------------------------
// Post-JIT method pointer patching
// ---------------------------------------------------------------------------

/// Called from codegen after JIT finalization to patch method function pointers.
/// `lookup` takes a function name (e.g. "Animal::speak") and returns the raw
/// code pointer if the function was compiled, or None.
pub fn patch_method_pointers(lookup: &dyn Fn(&str) -> Option<*const u8>) {
    with_registry(|reg| {
        for (_type_idx, info) in reg.types.iter_mut().enumerate() {
            let type_name = info.name.clone();
            for (_method_idx, method) in info.methods.iter_mut().enumerate() {
                // Try ClassName::methodName pattern (standard)
                let func_name = format!("{}::{}", type_name, method.name);
                if let Some(ptr) = lookup(&func_name) {
                    method.func_ptr = ptr as usize;
                    continue;
                }
                // Try just the method name (for top-level or init functions)
                if let Some(ptr) = lookup(&method.name) {
                    method.func_ptr = ptr as usize;
                }
            }
        }
    });
}

// ---------------------------------------------------------------------------
// Lazy field address resolution
// ---------------------------------------------------------------------------

/// Resolve a field's address from the global address map (if not yet resolved).
/// Returns the address, or 0 if not found.
fn resolve_field_addr(type_name: &str, field: &mut FieldInfo) -> usize {
    if field.addr != 0 {
        return field.addr;
    }
    let global_name = format!("%{}.{}", type_name, field.name);
    let guard = GLOBAL_ADDRS.lock().unwrap();
    if let Some(addrs) = guard.as_ref() {
        if let Some(&addr) = addrs.get(&global_name) {
            field.addr = addr;
            return addr;
        }
    }
    0
}

// ---------------------------------------------------------------------------
// Field value access (read/write field globals via reflection)
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_getFieldString(type_id: i64, index: i64) -> *mut c_char {
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            let type_name = info.name.clone();
            if let Some(field) = info.fields.get_mut(index as usize) {
                let addr = resolve_field_addr(&type_name, field);
                if addr != 0 {
                    let ptr = *(addr as *const *const c_char);
                    if !ptr.is_null() {
                        let s = CStr::from_ptr(ptr).to_string_lossy();
                        return leak_cstring(&s);
                    }
                }
            }
        }
        leak_cstring("")
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_getFieldInt(type_id: i64, index: i64) -> i64 {
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            let type_name = info.name.clone();
            if let Some(field) = info.fields.get_mut(index as usize) {
                let addr = resolve_field_addr(&type_name, field);
                if addr != 0 {
                    return *(addr as *const i64);
                }
            }
        }
        0
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_getFieldFloat(type_id: i64, index: i64) -> f64 {
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            let type_name = info.name.clone();
            if let Some(field) = info.fields.get_mut(index as usize) {
                let addr = resolve_field_addr(&type_name, field);
                if addr != 0 {
                    return *(addr as *const f64);
                }
            }
        }
        0.0
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_getFieldBool(type_id: i64, index: i64) -> i64 {
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            let type_name = info.name.clone();
            if let Some(field) = info.fields.get_mut(index as usize) {
                let addr = resolve_field_addr(&type_name, field);
                if addr != 0 {
                    return *(addr as *const i64);
                }
            }
        }
        0
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_setFieldString(
    type_id: i64,
    index: i64,
    value: *const c_char,
) {
    let s = cstr_to_string(value);
    let new_ptr = CString::new(s).unwrap_or_default().into_raw();
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            let type_name = info.name.clone();
            if let Some(field) = info.fields.get_mut(index as usize) {
                let addr = resolve_field_addr(&type_name, field);
                if addr != 0 {
                    *(addr as *mut *mut c_char) = new_ptr;
                }
            }
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_setFieldInt(type_id: i64, index: i64, value: i64) {
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            let type_name = info.name.clone();
            if let Some(field) = info.fields.get_mut(index as usize) {
                let addr = resolve_field_addr(&type_name, field);
                if addr != 0 {
                    *(addr as *mut i64) = value;
                }
            }
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_setFieldFloat(type_id: i64, index: i64, value: f64) {
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            let type_name = info.name.clone();
            if let Some(field) = info.fields.get_mut(index as usize) {
                let addr = resolve_field_addr(&type_name, field);
                if addr != 0 {
                    *(addr as *mut f64) = value;
                }
            }
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_setFieldBool(type_id: i64, index: i64, value: i64) {
    with_registry(|reg| {
        if let Some(info) = reg.types.get_mut(type_id as usize) {
            let type_name = info.name.clone();
            if let Some(field) = info.fields.get_mut(index as usize) {
                let addr = resolve_field_addr(&type_name, field);
                if addr != 0 {
                    *(addr as *mut i64) = value;
                }
            }
        }
    });
}

// ---------------------------------------------------------------------------
// Reset (for test isolation)
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_reflect_reset() {
    let mut guard = REGISTRY.lock().unwrap();
    *guard = None;
}

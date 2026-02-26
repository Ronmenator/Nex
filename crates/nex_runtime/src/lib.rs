use std::alloc::{alloc, dealloc, Layout};
use std::os::raw::c_char;
use std::ptr;
use std::sync::Mutex;

pub mod math;
pub mod string;
pub mod convert;
pub mod env;
pub mod time;
pub mod collections_ext;
pub mod io_ext;
pub mod path;
pub mod json;
pub mod process;
pub mod net;
pub mod threading;
pub mod async_runtime;
pub mod closure_runtime;
pub mod logging;
pub mod testing;

pub use math::*;
pub use string::*;
pub use convert::*;
pub use env::*;
pub use time::*;
pub use collections_ext::*;
pub use io_ext::*;
pub use path::*;
pub use json::*;
pub use process::*;
pub use net::*;
pub use threading::*;
pub use async_runtime::*;
pub use closure_runtime::*;
pub use logging::*;
pub use testing::*;

#[repr(C)]
pub struct NexType {
    pub size: u32,
    pub align: u32,
    pub pointer_map: *const u8,
    pub name: *const c_char,
}

unsafe impl Send for NexType {}
unsafe impl Sync for NexType {}

#[repr(C)]
pub struct NexObj {
    pub gc_mark: u8,
    pub type_desc: *const NexType,
}

const TAG_INT: u8 = 1;
const TAG_FLOAT: u8 = 2;
const TAG_BOOL: u8 = 3;
const _TAG_STRING: u8 = 4;
const TAG_OBJ: u8 = 5;

#[repr(C)]
pub struct NexVar {
    pub tag: u8,
    pub int_val: i64,
    pub float_val: f64,
    pub obj_val: *mut NexObj,
}

struct GcState {
    objects: Vec<*mut NexObj>,
    roots: Vec<*mut NexObj>,
}

unsafe impl Send for GcState {}

static GC: Mutex<Option<GcState>> = Mutex::new(None);

fn gc_init() {
    let mut gc = GC.lock().unwrap();
    if gc.is_none() {
        *gc = Some(GcState {
            objects: Vec::new(),
            roots: Vec::new(),
        });
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_gc_alloc(type_desc: *const NexType, size: u32) -> *mut NexObj {
    gc_init();
    let total = (size as usize).max(std::mem::size_of::<NexObj>());
    let align = if type_desc.is_null() { 8 } else { (*type_desc).align as usize }.max(8);
    let layout = Layout::from_size_align(total, align).unwrap_or(Layout::new::<NexObj>());
    let ptr = alloc(layout) as *mut NexObj;
    if ptr.is_null() {
        std::process::abort();
    }
    ptr::write(ptr, NexObj {
        gc_mark: 0,
        type_desc,
    });
    let mut gc = GC.lock().unwrap();
    if let Some(state) = gc.as_mut() {
        state.objects.push(ptr);
    }
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_gc_collect() {
    gc_init();
    let mut gc = GC.lock().unwrap();
    let Some(state) = gc.as_mut() else { return };

    for obj in &state.objects {
        (**obj).gc_mark = 0;
    }

    for root in &state.roots {
        mark(*root);
    }

    let mut survivors = Vec::new();
    for obj in state.objects.drain(..) {
        if (*obj).gc_mark != 0 {
            survivors.push(obj);
        } else {
            let size = if (*obj).type_desc.is_null() {
                std::mem::size_of::<NexObj>()
            } else {
                (*(*obj).type_desc).size as usize
            };
            let layout = Layout::from_size_align(size.max(std::mem::size_of::<NexObj>()), 8)
                .unwrap_or(Layout::new::<NexObj>());
            dealloc(obj as *mut u8, layout);
        }
    }
    state.objects = survivors;
}

unsafe fn mark(obj: *mut NexObj) {
    if obj.is_null() || (*obj).gc_mark != 0 {
        return;
    }
    (*obj).gc_mark = 1;
    if !(*obj).type_desc.is_null() {
        let type_desc = &*(*obj).type_desc;
        if !type_desc.pointer_map.is_null() {
            let field_count = type_desc.size as usize / 8;
            let base = (obj as *const u8).add(std::mem::size_of::<NexObj>());
            for i in 0..field_count {
                let map_byte = *type_desc.pointer_map.add(i / 8);
                if map_byte & (1 << (i % 8)) != 0 {
                    let field_ptr = *(base.add(i * 8) as *const *mut NexObj);
                    mark(field_ptr);
                }
            }
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_gc_safepoint() {
    // v1: no-op; future versions may trigger collection based on allocation pressure
}

#[no_mangle]
pub unsafe extern "C" fn nex_gc_write_barrier(_obj: *mut NexObj) {
    // v1 stub: required for generational GC in v2
}

#[no_mangle]
pub unsafe extern "C" fn nex_throw(ex: *mut NexObj) -> ! {
    let _msg = if ex.is_null() {
        "null exception".to_string()
    } else {
        format!("Nex exception at {:p}", ex)
    };
    eprintln!("nex_throw: {}", _msg);
    std::process::abort();
}

#[no_mangle]
pub unsafe extern "C" fn nex_new_exception(message: *const c_char) -> *mut NexObj {
    let obj = nex_gc_alloc(ptr::null(), 64);
    if !message.is_null() {
        let _msg = std::ffi::CStr::from_ptr(message);
    }
    obj
}

#[no_mangle]
pub unsafe extern "C" fn nex_var_from_i64(v: i64) -> *mut NexVar {
    let layout = Layout::new::<NexVar>();
    let ptr = alloc(layout) as *mut NexVar;
    ptr::write(ptr, NexVar {
        tag: TAG_INT,
        int_val: v,
        float_val: 0.0,
        obj_val: ptr::null_mut(),
    });
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_var_from_f64(v: f64) -> *mut NexVar {
    let layout = Layout::new::<NexVar>();
    let ptr = alloc(layout) as *mut NexVar;
    ptr::write(ptr, NexVar {
        tag: TAG_FLOAT,
        int_val: 0,
        float_val: v,
        obj_val: ptr::null_mut(),
    });
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_var_from_obj(o: *mut NexObj) -> *mut NexVar {
    let layout = Layout::new::<NexVar>();
    let ptr = alloc(layout) as *mut NexVar;
    ptr::write(ptr, NexVar {
        tag: TAG_OBJ,
        int_val: 0,
        float_val: 0.0,
        obj_val: o,
    });
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_var_add(a: *mut NexVar, b: *mut NexVar) -> *mut NexVar {
    if a.is_null() || b.is_null() {
        return nex_var_from_i64(0);
    }
    let a = &*a;
    let b = &*b;
    match (a.tag, b.tag) {
        (TAG_INT, TAG_INT) => nex_var_from_i64(a.int_val.wrapping_add(b.int_val)),
        (TAG_FLOAT, TAG_FLOAT) => nex_var_from_f64(a.float_val + b.float_val),
        (TAG_INT, TAG_FLOAT) => nex_var_from_f64(a.int_val as f64 + b.float_val),
        (TAG_FLOAT, TAG_INT) => nex_var_from_f64(a.float_val + b.int_val as f64),
        _ => nex_var_from_i64(0),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_var_eq(a: *mut NexVar, b: *mut NexVar) -> *mut NexVar {
    if a.is_null() || b.is_null() {
        return nex_var_from_i64(if a == b { 1 } else { 0 });
    }
    let a = &*a;
    let b = &*b;
    let eq = match (a.tag, b.tag) {
        (TAG_INT, TAG_INT) => a.int_val == b.int_val,
        (TAG_FLOAT, TAG_FLOAT) => a.float_val == b.float_val,
        (TAG_BOOL, TAG_BOOL) => a.int_val == b.int_val,
        _ => false,
    };
    nex_var_from_i64(if eq { 1 } else { 0 })
}

#[no_mangle]
pub unsafe extern "C" fn nex_var_invoke_member(
    _recv: *mut NexVar,
    _name: *mut NexObj,
    _args: *mut *mut NexVar,
    _argc: u32,
) -> *mut NexVar {
    nex_var_from_i64(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_var_typeof(_v: *mut NexObj) -> *const NexType {
    ptr::null()
}

// ---- Print ----

#[no_mangle]
pub unsafe extern "C" fn nex_print_str(s: *const c_char) {
    if !s.is_null() {
        let cs = std::ffi::CStr::from_ptr(s);
        if let Ok(rs) = cs.to_str() {
            use std::io::Write;
            let _ = write!(std::io::stdout(), "{rs}");
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_println_str(s: *const c_char) {
    if !s.is_null() {
        let cs = std::ffi::CStr::from_ptr(s);
        if let Ok(rs) = cs.to_str() {
            use std::io::Write;
            let _ = writeln!(std::io::stdout(), "{rs}");
        }
    } else {
        use std::io::Write;
        let _ = writeln!(std::io::stdout());
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_print_int(v: i64) {
    use std::io::Write;
    let _ = write!(std::io::stdout(), "{v}");
}

#[no_mangle]
pub unsafe extern "C" fn nex_println_int(v: i64) {
    use std::io::Write;
    let _ = writeln!(std::io::stdout(), "{v}");
}

#[no_mangle]
pub unsafe extern "C" fn nex_print_double(v: f64) {
    use std::io::Write;
    let _ = write!(std::io::stdout(), "{v}");
}

#[no_mangle]
pub unsafe extern "C" fn nex_println_double(v: f64) {
    use std::io::Write;
    let _ = writeln!(std::io::stdout(), "{v}");
}

#[no_mangle]
pub unsafe extern "C" fn nex_print_bool(v: i32) {
    use std::io::Write;
    let _ = write!(std::io::stdout(), "{}", if v != 0 { "true" } else { "false" });
}

#[no_mangle]
pub unsafe extern "C" fn nex_println_bool(v: i32) {
    use std::io::Write;
    let _ = writeln!(std::io::stdout(), "{}", if v != 0 { "true" } else { "false" });
}

#[no_mangle]
pub unsafe extern "C" fn nex_print_char(c: i32) {
    use std::io::Write;
    if let Some(ch) = char::from_u32(c as u32) {
        let _ = write!(std::io::stdout(), "{ch}");
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_println_char(c: i32) {
    use std::io::Write;
    if let Some(ch) = char::from_u32(c as u32) {
        let _ = writeln!(std::io::stdout(), "{ch}");
    }
}

// ---- Strings ----

#[no_mangle]
pub unsafe extern "C" fn nex_str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    let la = if a.is_null() { 0 } else { libc::strlen(a) };
    let lb = if b.is_null() { 0 } else { libc::strlen(b) };
    let r = libc::malloc(la + lb + 1) as *mut c_char;
    if r.is_null() {
        eprintln!("out of memory");
        std::process::abort();
    }
    if !a.is_null() {
        ptr::copy_nonoverlapping(a as *const u8, r as *mut u8, la);
    }
    if !b.is_null() {
        ptr::copy_nonoverlapping(b as *const u8, (r as *mut u8).add(la), lb);
    }
    *r.add(la + lb) = 0;
    r
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_eq(a: *const c_char, b: *const c_char) -> i64 {
    if a.is_null() && b.is_null() {
        return 1;
    }
    if a.is_null() || b.is_null() {
        return 0;
    }
    if libc::strcmp(a, b) == 0 { 1 } else { 0 }
}

#[no_mangle]
pub unsafe extern "C" fn nex_int_to_str(v: i64) -> *mut c_char {
    let s = format!("{v}\0");
    let ptr = libc::malloc(s.len()) as *mut c_char;
    if ptr.is_null() {
        std::process::abort();
    }
    ptr::copy_nonoverlapping(s.as_ptr(), ptr as *mut u8, s.len());
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_double_to_str(v: f64) -> *mut c_char {
    let s = format!("{v}\0");
    let ptr = libc::malloc(s.len()) as *mut c_char;
    if ptr.is_null() {
        std::process::abort();
    }
    ptr::copy_nonoverlapping(s.as_ptr(), ptr as *mut u8, s.len());
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_bool_to_str(v: i32) -> *mut c_char {
    let s = if v != 0 { "true\0" } else { "false\0" };
    let ptr = libc::malloc(s.len()) as *mut c_char;
    if ptr.is_null() {
        std::process::abort();
    }
    ptr::copy_nonoverlapping(s.as_ptr(), ptr as *mut u8, s.len());
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_length(s: *const c_char) -> i32 {
    if s.is_null() { 0 } else { libc::strlen(s) as i32 }
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_substring(s: *const c_char, start: i32, len: i32) -> *mut c_char {
    if s.is_null() || start < 0 || len <= 0 {
        let e = libc::malloc(1) as *mut c_char;
        if !e.is_null() { *e = 0; }
        return e;
    }
    let slen = libc::strlen(s) as i32;
    if start >= slen {
        let e = libc::malloc(1) as *mut c_char;
        if !e.is_null() { *e = 0; }
        return e;
    }
    let actual_len = len.min(slen - start) as usize;
    let r = libc::malloc(actual_len + 1) as *mut c_char;
    if r.is_null() {
        std::process::abort();
    }
    ptr::copy_nonoverlapping((s as *const u8).add(start as usize), r as *mut u8, actual_len);
    *r.add(actual_len) = 0;
    r
}

// ---- Disposable ----

#[no_mangle]
pub unsafe extern "C" fn nex_dispose(_resource: *mut u8) {
    // v1: no-op
}

// ---- List<T> (type-erased as Vec<i64>) ----

#[no_mangle]
pub unsafe extern "C" fn nex_list_new() -> *mut Vec<i64> {
    Box::into_raw(Box::new(Vec::<i64>::new()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_add(list: *mut Vec<i64>, item: i64) {
    if !list.is_null() {
        (*list).push(item);
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_get(list: *mut Vec<i64>, index: i64) -> i64 {
    if list.is_null() { return 0; }
    let v = &*list;
    let i = index as usize;
    if i < v.len() { v[i] } else { 0 }
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_set(list: *mut Vec<i64>, index: i64, value: i64) {
    if list.is_null() { return; }
    let v = &mut *list;
    let i = index as usize;
    if i < v.len() { v[i] = value; }
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_length(list: *mut Vec<i64>) -> i64 {
    if list.is_null() { 0 } else { (*list).len() as i64 }
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_remove(list: *mut Vec<i64>, index: i64) -> i64 {
    if list.is_null() { return 0; }
    let v = &mut *list;
    let i = index as usize;
    if i < v.len() { v.remove(i) } else { 0 }
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_free(list: *mut Vec<i64>) {
    if !list.is_null() {
        drop(Box::from_raw(list));
    }
}

// ---- GC root management (Rust API) ----

pub fn gc_add_root(obj: *mut NexObj) {
    gc_init();
    let mut gc = GC.lock().unwrap();
    if let Some(state) = gc.as_mut() {
        state.roots.push(obj);
    }
}

pub fn gc_remove_root(obj: *mut NexObj) {
    let mut gc = GC.lock().unwrap();
    if let Some(state) = gc.as_mut() {
        state.roots.retain(|r| *r != obj);
    }
}

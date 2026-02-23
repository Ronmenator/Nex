use std::os::raw::c_char;
use std::ptr;

unsafe fn cstr_to_str<'a>(s: *const c_char) -> &'a str {
    if s.is_null() { return ""; }
    std::ffi::CStr::from_ptr(s).to_str().unwrap_or("")
}

unsafe fn str_to_cstr(s: &str) -> *mut c_char {
    let len = s.len();
    let ptr = libc::malloc(len + 1) as *mut c_char;
    if ptr.is_null() { std::process::abort(); }
    ptr::copy_nonoverlapping(s.as_ptr(), ptr as *mut u8, len);
    *ptr.add(len) = 0;
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_parse(s: *const c_char) -> *mut serde_json::Value {
    let text = cstr_to_str(s);
    match serde_json::from_str::<serde_json::Value>(text) {
        Ok(val) => Box::into_raw(Box::new(val)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_stringify(handle: *mut serde_json::Value) -> *mut c_char {
    if handle.is_null() { return str_to_cstr("null"); }
    let val = &*handle;
    str_to_cstr(&val.to_string())
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_get_string(handle: *mut serde_json::Value, key: *const c_char) -> *mut c_char {
    if handle.is_null() { return str_to_cstr(""); }
    let val = &*handle;
    let k = cstr_to_str(key);
    val.get(k)
        .and_then(|v| v.as_str())
        .map(|s| str_to_cstr(s))
        .unwrap_or_else(|| str_to_cstr(""))
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_get_int(handle: *mut serde_json::Value, key: *const c_char) -> i64 {
    if handle.is_null() { return 0; }
    let val = &*handle;
    val.get(cstr_to_str(key)).and_then(|v| v.as_i64()).unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_get_float(handle: *mut serde_json::Value, key: *const c_char) -> f64 {
    if handle.is_null() { return 0.0; }
    let val = &*handle;
    val.get(cstr_to_str(key)).and_then(|v| v.as_f64()).unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_get_bool(handle: *mut serde_json::Value, key: *const c_char) -> i32 {
    if handle.is_null() { return 0; }
    let val = &*handle;
    val.get(cstr_to_str(key)).and_then(|v| v.as_bool()).map(|b| b as i32).unwrap_or(0)
}

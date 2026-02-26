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

// ---------------------------------------------------------------------------
// JSON object builder (for serialization via reflection)
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_json_new_object() -> *mut serde_json::Value {
    Box::into_raw(Box::new(serde_json::Value::Object(serde_json::Map::new())))
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_set_string(
    handle: *mut serde_json::Value,
    key: *const c_char,
    value: *const c_char,
) {
    if handle.is_null() { return; }
    let obj = &mut *handle;
    if let serde_json::Value::Object(map) = obj {
        map.insert(
            cstr_to_str(key).to_string(),
            serde_json::Value::String(cstr_to_str(value).to_string()),
        );
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_set_int(
    handle: *mut serde_json::Value,
    key: *const c_char,
    value: i64,
) {
    if handle.is_null() { return; }
    let obj = &mut *handle;
    if let serde_json::Value::Object(map) = obj {
        map.insert(
            cstr_to_str(key).to_string(),
            serde_json::Value::Number(serde_json::Number::from(value)),
        );
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_set_float(
    handle: *mut serde_json::Value,
    key: *const c_char,
    value: f64,
) {
    if handle.is_null() { return; }
    let obj = &mut *handle;
    if let serde_json::Value::Object(map) = obj {
        if let Some(n) = serde_json::Number::from_f64(value) {
            map.insert(cstr_to_str(key).to_string(), serde_json::Value::Number(n));
        } else {
            map.insert(cstr_to_str(key).to_string(), serde_json::Value::Null);
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_set_bool(
    handle: *mut serde_json::Value,
    key: *const c_char,
    value: i32,
) {
    if handle.is_null() { return; }
    let obj = &mut *handle;
    if let serde_json::Value::Object(map) = obj {
        map.insert(
            cstr_to_str(key).to_string(),
            serde_json::Value::Bool(value != 0),
        );
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_set_null(
    handle: *mut serde_json::Value,
    key: *const c_char,
) {
    if handle.is_null() { return; }
    let obj = &mut *handle;
    if let serde_json::Value::Object(map) = obj {
        map.insert(cstr_to_str(key).to_string(), serde_json::Value::Null);
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_stringify_pretty(handle: *mut serde_json::Value) -> *mut c_char {
    if handle.is_null() { return str_to_cstr("null"); }
    let val = &*handle;
    match serde_json::to_string_pretty(val) {
        Ok(s) => str_to_cstr(&s),
        Err(_) => str_to_cstr("null"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_json_free(handle: *mut serde_json::Value) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

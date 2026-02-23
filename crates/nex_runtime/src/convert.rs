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
pub unsafe extern "C" fn nex_parse_int(s: *const c_char) -> i64 {
    cstr_to_str(s).trim().parse::<i64>().unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_parse_float(s: *const c_char) -> f64 {
    cstr_to_str(s).trim().parse::<f64>().unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_parse_bool(s: *const c_char) -> i32 {
    match cstr_to_str(s).trim().to_lowercase().as_str() {
        "true" | "1" | "yes" => 1,
        _ => 0,
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_char_to_str(c: i32) -> *mut c_char {
    let ch = char::from_u32(c as u32).unwrap_or('\0');
    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    str_to_cstr(s)
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_to_chars(s: *const c_char) -> *mut i32 {
    let src = cstr_to_str(s);
    let chars: Vec<i32> = src.chars().map(|c| c as i32).collect();
    let len = chars.len();
    let total = (len + 1) * std::mem::size_of::<i32>();
    let ptr = libc::malloc(total) as *mut i32;
    if ptr.is_null() { std::process::abort(); }
    *ptr = len as i32;
    ptr::copy_nonoverlapping(chars.as_ptr(), ptr.add(1), len);
    ptr
}

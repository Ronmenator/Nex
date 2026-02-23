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
pub unsafe extern "C" fn nex_env_get(name: *const c_char) -> *mut c_char {
    let key = cstr_to_str(name);
    let val = std::env::var(key).unwrap_or_default();
    str_to_cstr(&val)
}

#[no_mangle]
pub unsafe extern "C" fn nex_env_set(name: *const c_char, value: *const c_char) {
    let key = cstr_to_str(name);
    let val = cstr_to_str(value);
    std::env::set_var(key, val);
}

#[no_mangle]
pub unsafe extern "C" fn nex_env_has(name: *const c_char) -> i32 {
    std::env::var(cstr_to_str(name)).is_ok() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_env_args_count() -> i32 {
    std::env::args().count() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_env_args_get(index: i32) -> *mut c_char {
    let val = std::env::args().nth(index as usize).unwrap_or_default();
    str_to_cstr(&val)
}

#[no_mangle]
pub unsafe extern "C" fn nex_env_cwd() -> *mut c_char {
    let cwd = std::env::current_dir()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();
    str_to_cstr(&cwd)
}

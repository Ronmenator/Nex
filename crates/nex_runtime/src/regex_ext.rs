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
pub unsafe extern "C" fn nex_regex_new(pattern: *const c_char) -> *mut regex::Regex {
    match regex::Regex::new(cstr_to_str(pattern)) {
        Ok(re) => Box::into_raw(Box::new(re)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_regex_is_match(handle: *mut regex::Regex, s: *const c_char) -> i32 {
    if handle.is_null() { return 0; }
    let re = &*handle;
    re.is_match(cstr_to_str(s)) as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_regex_find(handle: *mut regex::Regex, s: *const c_char) -> *mut c_char {
    if handle.is_null() { return str_to_cstr(""); }
    let re = &*handle;
    re.find(cstr_to_str(s))
        .map(|m| str_to_cstr(m.as_str()))
        .unwrap_or_else(|| str_to_cstr(""))
}

#[no_mangle]
pub unsafe extern "C" fn nex_regex_replace(handle: *mut regex::Regex, s: *const c_char, rep: *const c_char) -> *mut c_char {
    if handle.is_null() { return str_to_cstr(""); }
    let re = &*handle;
    let result = re.replace_all(cstr_to_str(s), cstr_to_str(rep));
    str_to_cstr(&result)
}

#[no_mangle]
pub unsafe extern "C" fn nex_regex_free(handle: *mut regex::Regex) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

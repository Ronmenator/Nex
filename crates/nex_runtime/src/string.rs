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
pub unsafe extern "C" fn nex_str_split(s: *const c_char, delim: *const c_char) -> *mut c_char {
    let src = cstr_to_str(s);
    let d = cstr_to_str(delim);
    let joined = src.split(d).collect::<Vec<_>>().join("\0");
    str_to_cstr(&joined)
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_trim(s: *const c_char) -> *mut c_char {
    str_to_cstr(cstr_to_str(s).trim())
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_trim_start(s: *const c_char) -> *mut c_char {
    str_to_cstr(cstr_to_str(s).trim_start())
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_trim_end(s: *const c_char) -> *mut c_char {
    str_to_cstr(cstr_to_str(s).trim_end())
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_starts_with(s: *const c_char, prefix: *const c_char) -> i64 {
    cstr_to_str(s).starts_with(cstr_to_str(prefix)) as i64
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_ends_with(s: *const c_char, suffix: *const c_char) -> i64 {
    cstr_to_str(s).ends_with(cstr_to_str(suffix)) as i64
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_contains(s: *const c_char, needle: *const c_char) -> i64 {
    cstr_to_str(s).contains(cstr_to_str(needle)) as i64
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_index_of(s: *const c_char, needle: *const c_char) -> i64 {
    match cstr_to_str(s).find(cstr_to_str(needle)) {
        Some(i) => i as i64,
        None => -1,
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_replace(s: *const c_char, old: *const c_char, new: *const c_char) -> *mut c_char {
    let result = cstr_to_str(s).replace(cstr_to_str(old), cstr_to_str(new));
    str_to_cstr(&result)
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_to_upper(s: *const c_char) -> *mut c_char {
    str_to_cstr(&cstr_to_str(s).to_uppercase())
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_to_lower(s: *const c_char) -> *mut c_char {
    str_to_cstr(&cstr_to_str(s).to_lowercase())
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_repeat(s: *const c_char, count: i32) -> *mut c_char {
    let repeated = cstr_to_str(s).repeat(count.max(0) as usize);
    str_to_cstr(&repeated)
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_char_at(s: *const c_char, index: i64) -> i64 {
    let src = cstr_to_str(s);
    src.chars().nth(index.max(0) as usize).map(|c| c as i64).unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_reverse(s: *const c_char) -> *mut c_char {
    let reversed: String = cstr_to_str(s).chars().rev().collect();
    str_to_cstr(&reversed)
}

#[no_mangle]
pub unsafe extern "C" fn nex_str_truncate(s: *const c_char, max_len: i64) -> *mut c_char {
    let src = cstr_to_str(s);
    let n = max_len.max(0) as usize;
    let truncated: String = src.chars().take(n).collect();
    str_to_cstr(&truncated)
}

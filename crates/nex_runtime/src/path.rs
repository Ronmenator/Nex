use std::os::raw::c_char;
use std::path::Path;
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
pub unsafe extern "C" fn nex_path_join(a: *const c_char, b: *const c_char) -> *mut c_char {
    let joined = Path::new(cstr_to_str(a)).join(cstr_to_str(b));
    str_to_cstr(&joined.to_string_lossy())
}

#[no_mangle]
pub unsafe extern "C" fn nex_path_parent(p: *const c_char) -> *mut c_char {
    let parent = Path::new(cstr_to_str(p))
        .parent()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();
    str_to_cstr(&parent)
}

#[no_mangle]
pub unsafe extern "C" fn nex_path_file_name(p: *const c_char) -> *mut c_char {
    let name = Path::new(cstr_to_str(p))
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    str_to_cstr(&name)
}

#[no_mangle]
pub unsafe extern "C" fn nex_path_extension(p: *const c_char) -> *mut c_char {
    let ext = Path::new(cstr_to_str(p))
        .extension()
        .map(|e| e.to_string_lossy().into_owned())
        .unwrap_or_default();
    str_to_cstr(&ext)
}

#[no_mangle]
pub unsafe extern "C" fn nex_path_stem(p: *const c_char) -> *mut c_char {
    let stem = Path::new(cstr_to_str(p))
        .file_stem()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_default();
    str_to_cstr(&stem)
}

#[no_mangle]
pub unsafe extern "C" fn nex_path_is_absolute(p: *const c_char) -> i32 {
    Path::new(cstr_to_str(p)).is_absolute() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_path_normalize(p: *const c_char) -> *mut c_char {
    let path = Path::new(cstr_to_str(p));
    let normalized = path.canonicalize()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| path.to_string_lossy().into_owned());
    str_to_cstr(&normalized)
}

#[no_mangle]
pub unsafe extern "C" fn nex_path_separator() -> *mut c_char {
    str_to_cstr(std::path::MAIN_SEPARATOR_STR)
}

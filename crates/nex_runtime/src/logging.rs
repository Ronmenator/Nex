use std::os::raw::c_char;
use std::sync::atomic::{AtomicI32, Ordering};

static LOG_LEVEL: AtomicI32 = AtomicI32::new(0); // 0=debug, 1=info, 2=warn, 3=error

unsafe fn cstr_to_str<'a>(s: *const c_char) -> &'a str {
    if s.is_null() { return ""; }
    std::ffi::CStr::from_ptr(s).to_str().unwrap_or("")
}

fn log_at(level: i32, prefix: &str, msg: &str) {
    if level >= LOG_LEVEL.load(Ordering::Relaxed) {
        eprintln!("[{prefix}] {msg}");
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_log_debug(s: *const c_char) {
    log_at(0, "DEBUG", cstr_to_str(s));
}

#[no_mangle]
pub unsafe extern "C" fn nex_log_info(s: *const c_char) {
    log_at(1, "INFO", cstr_to_str(s));
}

#[no_mangle]
pub unsafe extern "C" fn nex_log_warn(s: *const c_char) {
    log_at(2, "WARN", cstr_to_str(s));
}

#[no_mangle]
pub unsafe extern "C" fn nex_log_error(s: *const c_char) {
    log_at(3, "ERROR", cstr_to_str(s));
}

#[no_mangle]
pub unsafe extern "C" fn nex_log_set_level(level: i32) {
    LOG_LEVEL.store(level.clamp(0, 3), Ordering::Relaxed);
}

#[no_mangle]
pub unsafe extern "C" fn nex_log_with_tag(tag: *const c_char, msg: *const c_char) {
    let t = cstr_to_str(tag);
    let m = cstr_to_str(msg);
    eprintln!("[{t}] {m}");
}

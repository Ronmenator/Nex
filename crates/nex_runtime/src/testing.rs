use std::os::raw::c_char;

unsafe fn cstr_to_str<'a>(s: *const c_char) -> &'a str {
    if s.is_null() { return ""; }
    std::ffi::CStr::from_ptr(s).to_str().unwrap_or("")
}

fn fail(msg: &str) {
    eprintln!("ASSERTION FAILED: {msg}");
    std::process::abort();
}

#[no_mangle]
pub unsafe extern "C" fn nex_assert(cond: i32, msg: *const c_char) {
    if cond == 0 { fail(cstr_to_str(msg)); }
}

#[no_mangle]
pub unsafe extern "C" fn nex_assert_eq_int(a: i64, b: i64, msg: *const c_char) {
    if a != b { fail(&format!("{}: expected {} == {}", cstr_to_str(msg), a, b)); }
}

#[no_mangle]
pub unsafe extern "C" fn nex_assert_eq_str(a: *const c_char, b: *const c_char, msg: *const c_char) {
    if cstr_to_str(a) != cstr_to_str(b) {
        fail(&format!("{}: expected \"{}\" == \"{}\"", cstr_to_str(msg), cstr_to_str(a), cstr_to_str(b)));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_assert_eq_float(a: f64, b: f64, msg: *const c_char) {
    if (a - b).abs() > f64::EPSILON {
        fail(&format!("{}: expected {} == {}", cstr_to_str(msg), a, b));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_assert_eq_bool(a: i32, b: i32, msg: *const c_char) {
    if (a != 0) != (b != 0) {
        fail(&format!("{}: expected {} == {}", cstr_to_str(msg), a != 0, b != 0));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_assert_ne_int(a: i64, b: i64, msg: *const c_char) {
    if a == b { fail(&format!("{}: expected {} != {}", cstr_to_str(msg), a, b)); }
}

#[no_mangle]
pub unsafe extern "C" fn nex_assert_ne_str(a: *const c_char, b: *const c_char, msg: *const c_char) {
    if cstr_to_str(a) == cstr_to_str(b) {
        fail(&format!("{}: expected \"{}\" != \"{}\"", cstr_to_str(msg), cstr_to_str(a), cstr_to_str(b)));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_assert_true(cond: i32, msg: *const c_char) {
    if cond == 0 { fail(cstr_to_str(msg)); }
}

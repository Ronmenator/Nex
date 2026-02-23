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

fn shell_cmd() -> (&'static str, &'static str) {
    if cfg!(windows) { ("cmd", "/C") } else { ("sh", "-c") }
}

#[no_mangle]
pub unsafe extern "C" fn nex_process_exec(cmd: *const c_char) -> i32 {
    let (shell, flag) = shell_cmd();
    std::process::Command::new(shell)
        .args([flag, cstr_to_str(cmd)])
        .status()
        .map(|s| s.code().unwrap_or(-1))
        .unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn nex_process_exec_output(cmd: *const c_char) -> *mut c_char {
    let (shell, flag) = shell_cmd();
    let output = std::process::Command::new(shell)
        .args([flag, cstr_to_str(cmd)])
        .output();
    match output {
        Ok(o) => str_to_cstr(&String::from_utf8_lossy(&o.stdout)),
        Err(_) => str_to_cstr(""),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_process_exit(code: i32) {
    std::process::exit(code);
}

#[no_mangle]
pub unsafe extern "C" fn nex_process_pid() -> i64 {
    std::process::id() as i64
}

#[no_mangle]
pub unsafe extern "C" fn nex_process_spawn(cmd: *const c_char) -> *mut std::process::Child {
    let (shell, flag) = shell_cmd();
    match std::process::Command::new(shell)
        .args([flag, cstr_to_str(cmd)])
        .spawn()
    {
        Ok(child) => Box::into_raw(Box::new(child)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_process_wait(handle: *mut std::process::Child) -> i32 {
    if handle.is_null() { return -1; }
    let mut child = Box::from_raw(handle);
    child.wait().map(|s| s.code().unwrap_or(-1)).unwrap_or(-1)
}

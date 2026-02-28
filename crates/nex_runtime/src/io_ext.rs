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
pub unsafe extern "C" fn nex_io_read_line() -> *mut c_char {
    let mut line = String::new();
    let _ = std::io::stdin().read_line(&mut line);
    if line.ends_with('\n') { line.pop(); }
    if line.ends_with('\r') { line.pop(); }
    str_to_cstr(&line)
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_file_exists(path: *const c_char) -> i32 {
    std::path::Path::new(cstr_to_str(path)).exists() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_file_delete(path: *const c_char) -> i32 {
    std::fs::remove_file(cstr_to_str(path)).is_ok() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_file_rename(from: *const c_char, to: *const c_char) -> i32 {
    std::fs::rename(cstr_to_str(from), cstr_to_str(to)).is_ok() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_file_copy(from: *const c_char, to: *const c_char) -> i32 {
    std::fs::copy(cstr_to_str(from), cstr_to_str(to)).is_ok() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_file_size(path: *const c_char) -> i64 {
    std::fs::metadata(cstr_to_str(path))
        .map(|m| m.len() as i64)
        .unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_file_read_bytes(path: *const c_char, buf: *mut u8, max_len: i64) -> i64 {
    use std::io::Read;
    let Ok(mut f) = std::fs::File::open(cstr_to_str(path)) else { return -1 };
    let slice = std::slice::from_raw_parts_mut(buf, max_len as usize);
    f.read(slice).map(|n| n as i64).unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_file_write_bytes(path: *const c_char, buf: *const u8, len: i64) -> i32 {
    use std::io::Write;
    let Ok(mut f) = std::fs::File::create(cstr_to_str(path)) else { return 0 };
    let slice = std::slice::from_raw_parts(buf, len as usize);
    f.write_all(slice).is_ok() as i32
}

/// File.write_text(path, content) — create/overwrite file with text content.
#[no_mangle]
pub unsafe extern "C" fn nex_io_file_write_text(path: *const c_char, data: *const c_char) -> i32 {
    use std::io::Write;
    let Ok(mut f) = std::fs::File::create(cstr_to_str(path)) else { return 0 };
    f.write_all(cstr_to_str(data).as_bytes()).is_ok() as i32
}

/// File.read_all(path) — read entire file as string.
#[no_mangle]
pub unsafe extern "C" fn nex_io_file_read_all(path: *const c_char) -> *mut c_char {
    let content = std::fs::read_to_string(cstr_to_str(path)).unwrap_or_default();
    str_to_cstr(&content)
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_file_append(path: *const c_char, data: *const c_char) -> i32 {
    use std::io::Write;
    let Ok(mut f) = std::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(cstr_to_str(path))
    else {
        return 0;
    };
    f.write_all(cstr_to_str(data).as_bytes()).is_ok() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_mkdir(path: *const c_char) -> i32 {
    std::fs::create_dir_all(cstr_to_str(path)).is_ok() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_io_list_dir(path: *const c_char) -> *mut c_char {
    let entries: Vec<String> = std::fs::read_dir(cstr_to_str(path))
        .into_iter()
        .flatten()
        .filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .collect();
    str_to_cstr(&entries.join("\n"))
}

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

pub struct HttpResponse {
    pub status: i32,
    pub body: String,
    pub headers: Vec<(String, String)>,
}

fn do_response(resp: Result<minreq::Response, minreq::Error>) -> *mut HttpResponse {
    match resp {
        Ok(r) => {
            let headers = r.headers.iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Box::into_raw(Box::new(HttpResponse {
                status: r.status_code as i32,
                body: r.as_str().unwrap_or("").to_string(),
                headers,
            }))
        }
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_get(url: *const c_char) -> *mut HttpResponse {
    do_response(minreq::get(cstr_to_str(url)).send())
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_post(url: *const c_char, body: *const c_char, content_type: *const c_char) -> *mut HttpResponse {
    do_response(
        minreq::post(cstr_to_str(url))
            .with_header("Content-Type", cstr_to_str(content_type))
            .with_body(cstr_to_str(body))
            .send()
    )
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_response_status(handle: *mut HttpResponse) -> i32 {
    if handle.is_null() { return 0; }
    (*handle).status
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_response_body(handle: *mut HttpResponse) -> *mut c_char {
    if handle.is_null() { return str_to_cstr(""); }
    str_to_cstr(&(*handle).body)
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_response_header(handle: *mut HttpResponse, name: *const c_char) -> *mut c_char {
    if handle.is_null() { return str_to_cstr(""); }
    let key = cstr_to_str(name).to_lowercase();
    let val = (*handle).headers.iter()
        .find(|(k, _)| k.to_lowercase() == key)
        .map(|(_, v)| v.as_str())
        .unwrap_or("");
    str_to_cstr(val)
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_response_free(handle: *mut HttpResponse) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

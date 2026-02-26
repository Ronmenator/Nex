use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
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

pub struct HttpServer {
    listener: TcpListener,
}

pub struct HttpConn {
    stream: TcpStream,
    method: String,
    path: String,
    headers: Vec<(String, String)>,
    body: String,
}

fn parse_request(stream: &mut TcpStream) -> Option<HttpConn> {
    let peer_stream = stream.try_clone().ok()?;
    let mut reader = BufReader::new(peer_stream);

    // Read request line
    let mut request_line = String::new();
    reader.read_line(&mut request_line).ok()?;
    let parts: Vec<&str> = request_line.trim().splitn(3, ' ').collect();
    if parts.len() < 2 { return None; }
    let method = parts[0].to_string();
    let path = parts[1].to_string();

    // Read headers
    let mut headers = Vec::new();
    let mut content_length: usize = 0;
    loop {
        let mut line = String::new();
        reader.read_line(&mut line).ok()?;
        let trimmed = line.trim();
        if trimmed.is_empty() { break; }
        if let Some((key, value)) = trimmed.split_once(':') {
            let key = key.trim().to_string();
            let value = value.trim().to_string();
            if key.eq_ignore_ascii_case("content-length") {
                content_length = value.parse().unwrap_or(0);
            }
            headers.push((key, value));
        }
    }

    // Read body if Content-Length > 0
    let body = if content_length > 0 {
        let mut buf = vec![0u8; content_length];
        reader.read_exact(&mut buf).ok()?;
        String::from_utf8_lossy(&buf).to_string()
    } else {
        String::new()
    };

    Some(HttpConn {
        stream: stream.try_clone().ok()?,
        method,
        path,
        headers,
        body,
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_server_new(port: i64) -> *mut HttpServer {
    let addr = format!("0.0.0.0:{}", port);
    match TcpListener::bind(&addr) {
        Ok(listener) => Box::into_raw(Box::new(HttpServer { listener })),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_server_accept(server: *mut HttpServer) -> *mut HttpConn {
    if server.is_null() { return ptr::null_mut(); }
    let srv = &*server;
    match srv.listener.accept() {
        Ok((mut stream, _)) => {
            match parse_request(&mut stream) {
                Some(conn) => Box::into_raw(Box::new(conn)),
                None => ptr::null_mut(),
            }
        }
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_conn_method(conn: *mut HttpConn) -> *mut c_char {
    if conn.is_null() { return str_to_cstr(""); }
    str_to_cstr(&(*conn).method)
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_conn_path(conn: *mut HttpConn) -> *mut c_char {
    if conn.is_null() { return str_to_cstr(""); }
    str_to_cstr(&(*conn).path)
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_conn_header(conn: *mut HttpConn, name: *const c_char) -> *mut c_char {
    if conn.is_null() { return str_to_cstr(""); }
    let target = cstr_to_str(name);
    for (key, value) in &(*conn).headers {
        if key.eq_ignore_ascii_case(target) {
            return str_to_cstr(value);
        }
    }
    str_to_cstr("")
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_conn_body(conn: *mut HttpConn) -> *mut c_char {
    if conn.is_null() { return str_to_cstr(""); }
    str_to_cstr(&(*conn).body)
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_conn_respond(
    conn: *mut HttpConn,
    status: i64,
    body: *const c_char,
    content_type: *const c_char,
) {
    if conn.is_null() { return; }
    let body_str = cstr_to_str(body);
    let ct = cstr_to_str(content_type);
    let ct = if ct.is_empty() { "text/plain" } else { ct };
    let status_text = match status {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        405 => "Method Not Allowed",
        500 => "Internal Server Error",
        _ => "OK",
    };
    let response = format!(
        "HTTP/1.1 {} {}\r\nContent-Type: {}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
        status, status_text, ct, body_str.len(), body_str
    );
    let _ = (*conn).stream.write_all(response.as_bytes());
    let _ = (*conn).stream.flush();
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_conn_close(conn: *mut HttpConn) {
    if !conn.is_null() {
        drop(Box::from_raw(conn));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_http_server_close(server: *mut HttpServer) {
    if !server.is_null() {
        drop(Box::from_raw(server));
    }
}

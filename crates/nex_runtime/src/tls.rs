use std::io::{Read, Write};
use std::net::TcpStream;
use std::os::raw::c_char;
use std::ptr;
use std::sync::Arc;
use rustls::{ClientConfig, ClientConnection, StreamOwned};
use rustls_pki_types::ServerName;

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

pub struct NexTlsStream {
    stream: StreamOwned<ClientConnection, TcpStream>,
}

/// Connect to a TLS server. Returns a TLS stream handle, or null on failure.
#[no_mangle]
pub unsafe extern "C" fn nex_tls_connect(host: *const c_char, port: i64) -> *mut NexTlsStream {
    let host_str = cstr_to_str(host);
    let addr = format!("{}:{}", host_str, port);

    let root_store = rustls::RootCertStore::from_iter(
        webpki_roots::TLS_SERVER_ROOTS.iter().cloned()
    );

    let config = ClientConfig::builder()
        .with_root_certificates(root_store)
        .with_no_client_auth();

    let server_name = match ServerName::try_from(host_str.to_string()) {
        Ok(name) => name,
        Err(_) => return ptr::null_mut(),
    };

    let conn = match ClientConnection::new(Arc::new(config), server_name) {
        Ok(c) => c,
        Err(_) => return ptr::null_mut(),
    };

    let tcp_stream = match TcpStream::connect(&addr) {
        Ok(s) => s,
        Err(_) => return ptr::null_mut(),
    };

    let tls_stream = StreamOwned::new(conn, tcp_stream);
    Box::into_raw(Box::new(NexTlsStream { stream: tls_stream }))
}

/// Send data over a TLS connection. Returns bytes sent, or -1 on failure.
#[no_mangle]
pub unsafe extern "C" fn nex_tls_send(handle: *mut NexTlsStream, data: *const c_char) -> i64 {
    if handle.is_null() || data.is_null() { return -1; }
    let s = cstr_to_str(data);
    match (*handle).stream.write(s.as_bytes()) {
        Ok(n) => {
            let _ = (*handle).stream.flush();
            n as i64
        }
        Err(_) => -1,
    }
}

/// Send raw bytes over a TLS connection. Returns bytes sent, or -1 on failure.
#[no_mangle]
pub unsafe extern "C" fn nex_tls_send_bytes(handle: *mut NexTlsStream, data: *const u8, len: i64) -> i64 {
    if handle.is_null() || data.is_null() || len <= 0 { return -1; }
    let slice = std::slice::from_raw_parts(data, len as usize);
    match (*handle).stream.write(slice) {
        Ok(n) => {
            let _ = (*handle).stream.flush();
            n as i64
        }
        Err(_) => -1,
    }
}

/// Receive data from a TLS connection as a string. Returns the received string.
#[no_mangle]
pub unsafe extern "C" fn nex_tls_recv(handle: *mut NexTlsStream, max_len: i64) -> *mut c_char {
    if handle.is_null() || max_len <= 0 { return str_to_cstr(""); }
    let mut buf = vec![0u8; max_len as usize];
    match (*handle).stream.read(&mut buf) {
        Ok(n) => str_to_cstr(&String::from_utf8_lossy(&buf[..n])),
        Err(_) => str_to_cstr(""),
    }
}

/// Receive raw bytes from a TLS connection. Returns bytes read, or -1 on failure.
#[no_mangle]
pub unsafe extern "C" fn nex_tls_recv_bytes(handle: *mut NexTlsStream, buf: *mut u8, max_len: i64) -> i64 {
    if handle.is_null() || buf.is_null() || max_len <= 0 { return -1; }
    let slice = std::slice::from_raw_parts_mut(buf, max_len as usize);
    match (*handle).stream.read(slice) {
        Ok(n) => n as i64,
        Err(_) => -1,
    }
}

/// Close a TLS connection.
#[no_mangle]
pub unsafe extern "C" fn nex_tls_close(handle: *mut NexTlsStream) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

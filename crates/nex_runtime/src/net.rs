use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream, UdpSocket};
use std::os::raw::c_char;
use std::ptr;

unsafe fn cstr_to_str<'a>(s: *const c_char) -> &'a str {
    if s.is_null() { return ""; }
    std::ffi::CStr::from_ptr(s).to_str().unwrap_or("")
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_tcp_connect(host: *const c_char, port: i32) -> *mut TcpStream {
    let addr = format!("{}:{}", cstr_to_str(host), port);
    match TcpStream::connect(&addr) {
        Ok(stream) => Box::into_raw(Box::new(stream)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_tcp_close(handle: *mut TcpStream) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_tcp_send(handle: *mut TcpStream, data: *const u8, len: i32) -> i32 {
    if handle.is_null() || data.is_null() { return -1; }
    let stream = &mut *handle;
    let slice = std::slice::from_raw_parts(data, len as usize);
    stream.write(slice).map(|n| n as i32).unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_tcp_recv(handle: *mut TcpStream, buf: *mut u8, max_len: i32) -> i32 {
    if handle.is_null() || buf.is_null() { return -1; }
    let stream = &mut *handle;
    let slice = std::slice::from_raw_parts_mut(buf, max_len as usize);
    stream.read(slice).map(|n| n as i32).unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_tcp_listen(host: *const c_char, port: i32) -> *mut TcpListener {
    let addr = format!("{}:{}", cstr_to_str(host), port);
    match TcpListener::bind(&addr) {
        Ok(listener) => Box::into_raw(Box::new(listener)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_tcp_accept(handle: *mut TcpListener) -> *mut TcpStream {
    if handle.is_null() { return ptr::null_mut(); }
    let listener = &*handle;
    match listener.accept() {
        Ok((stream, _)) => Box::into_raw(Box::new(stream)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_udp_bind(host: *const c_char, port: i32) -> *mut UdpSocket {
    let addr = format!("{}:{}", cstr_to_str(host), port);
    match UdpSocket::bind(&addr) {
        Ok(socket) => Box::into_raw(Box::new(socket)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_udp_close(handle: *mut UdpSocket) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_udp_send(handle: *mut UdpSocket, data: *const u8, len: i32, host: *const c_char, port: i32) -> i32 {
    if handle.is_null() || data.is_null() { return -1; }
    let socket = &*handle;
    let addr = format!("{}:{}", cstr_to_str(host), port);
    let slice = std::slice::from_raw_parts(data, len as usize);
    socket.send_to(slice, &addr).map(|n| n as i32).unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn nex_net_udp_recv(handle: *mut UdpSocket, buf: *mut u8, max_len: i32) -> i32 {
    if handle.is_null() || buf.is_null() { return -1; }
    let socket = &*handle;
    let slice = std::slice::from_raw_parts_mut(buf, max_len as usize);
    socket.recv(slice).map(|n| n as i32).unwrap_or(-1)
}

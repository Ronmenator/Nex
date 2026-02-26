use std::io::{Read, Write};
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

// ---------------------------------------------------------------------------
// WebSocket Server
// ---------------------------------------------------------------------------

pub struct WsServer {
    listener: TcpListener,
}

pub struct WsConn {
    stream: TcpStream,
}

fn sha1_digest(input: &[u8]) -> [u8; 20] {
    use sha1::Sha1;
    use sha1::Digest;
    let mut hasher = Sha1::new();
    hasher.update(input);
    let result = hasher.finalize();
    let mut out = [0u8; 20];
    out.copy_from_slice(&result);
    out
}

fn base64_encode(data: &[u8]) -> String {
    use base64::Engine;
    base64::engine::general_purpose::STANDARD.encode(data)
}

const WS_MAGIC: &str = "258EAFA5-E914-47DA-95CA-5AB4A11B4DB5";

fn perform_handshake(stream: &mut TcpStream) -> bool {
    let mut buf = [0u8; 4096];
    let n = match stream.read(&mut buf) {
        Ok(n) if n > 0 => n,
        _ => return false,
    };
    let request = String::from_utf8_lossy(&buf[..n]);

    // Extract Sec-WebSocket-Key
    let key = request.lines()
        .find(|l| l.to_lowercase().starts_with("sec-websocket-key:"))
        .and_then(|l| l.split_once(':'))
        .map(|(_, v)| v.trim().to_string());

    let key = match key {
        Some(k) => k,
        None => return false,
    };

    // Compute accept key
    let accept_input = format!("{}{}", key, WS_MAGIC);
    let hash = sha1_digest(accept_input.as_bytes());
    let accept = base64_encode(&hash);

    let response = format!(
        "HTTP/1.1 101 Switching Protocols\r\n\
         Upgrade: websocket\r\n\
         Connection: Upgrade\r\n\
         Sec-WebSocket-Accept: {}\r\n\r\n",
        accept
    );

    stream.write_all(response.as_bytes()).is_ok()
}

#[no_mangle]
pub unsafe extern "C" fn nex_ws_server_new(port: i64) -> *mut WsServer {
    let addr = format!("0.0.0.0:{}", port);
    match TcpListener::bind(&addr) {
        Ok(listener) => Box::into_raw(Box::new(WsServer { listener })),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_ws_accept(server: *mut WsServer) -> *mut WsConn {
    if server.is_null() { return ptr::null_mut(); }
    let srv = &*server;
    match srv.listener.accept() {
        Ok((mut stream, _)) => {
            if perform_handshake(&mut stream) {
                Box::into_raw(Box::new(WsConn { stream }))
            } else {
                ptr::null_mut()
            }
        }
        Err(_) => ptr::null_mut(),
    }
}

/// Send a text message over a WebSocket connection.
#[no_mangle]
pub unsafe extern "C" fn nex_ws_send(conn: *mut WsConn, msg: *const c_char) -> i64 {
    if conn.is_null() { return -1; }
    let text = cstr_to_str(msg);
    let payload = text.as_bytes();
    let frame = encode_frame(0x01, payload); // 0x01 = text frame
    match (*conn).stream.write_all(&frame) {
        Ok(_) => {
            let _ = (*conn).stream.flush();
            payload.len() as i64
        }
        Err(_) => -1,
    }
}

/// Receive a text message from a WebSocket connection.
#[no_mangle]
pub unsafe extern "C" fn nex_ws_recv(conn: *mut WsConn) -> *mut c_char {
    if conn.is_null() { return str_to_cstr(""); }
    match decode_frame(&mut (*conn).stream) {
        Some((opcode, data)) => {
            if opcode == 0x08 {
                // Close frame
                return str_to_cstr("");
            }
            if opcode == 0x09 {
                // Ping — send pong
                let pong = encode_frame(0x0A, &data);
                let _ = (*conn).stream.write_all(&pong);
                // Try to read the next frame
                match decode_frame(&mut (*conn).stream) {
                    Some((_, data)) => str_to_cstr(&String::from_utf8_lossy(&data)),
                    None => str_to_cstr(""),
                }
            } else {
                str_to_cstr(&String::from_utf8_lossy(&data))
            }
        }
        None => str_to_cstr(""),
    }
}

/// Close a WebSocket connection.
#[no_mangle]
pub unsafe extern "C" fn nex_ws_close(conn: *mut WsConn) {
    if conn.is_null() { return; }
    // Send close frame
    let close_frame = encode_frame(0x08, &[]);
    let c = &mut *conn;
    let _ = c.stream.write_all(&close_frame);
    let _ = c.stream.flush();
    drop(Box::from_raw(conn));
}

/// Close a WebSocket server.
#[no_mangle]
pub unsafe extern "C" fn nex_ws_server_close(server: *mut WsServer) {
    if !server.is_null() {
        drop(Box::from_raw(server));
    }
}

// ---------------------------------------------------------------------------
// WebSocket frame encoding/decoding (RFC 6455)
// ---------------------------------------------------------------------------

fn encode_frame(opcode: u8, payload: &[u8]) -> Vec<u8> {
    let mut frame = Vec::new();
    frame.push(0x80 | opcode); // FIN bit + opcode

    let len = payload.len();
    if len < 126 {
        frame.push(len as u8);
    } else if len < 65536 {
        frame.push(126);
        frame.push((len >> 8) as u8);
        frame.push((len & 0xFF) as u8);
    } else {
        frame.push(127);
        for i in (0..8).rev() {
            frame.push(((len >> (i * 8)) & 0xFF) as u8);
        }
    }

    frame.extend_from_slice(payload);
    frame
}

fn decode_frame(stream: &mut TcpStream) -> Option<(u8, Vec<u8>)> {
    let mut header = [0u8; 2];
    stream.read_exact(&mut header).ok()?;

    let opcode = header[0] & 0x0F;
    let masked = (header[1] & 0x80) != 0;
    let mut payload_len = (header[1] & 0x7F) as u64;

    if payload_len == 126 {
        let mut ext = [0u8; 2];
        stream.read_exact(&mut ext).ok()?;
        payload_len = u16::from_be_bytes(ext) as u64;
    } else if payload_len == 127 {
        let mut ext = [0u8; 8];
        stream.read_exact(&mut ext).ok()?;
        payload_len = u64::from_be_bytes(ext);
    }

    let mask_key = if masked {
        let mut mk = [0u8; 4];
        stream.read_exact(&mut mk).ok()?;
        Some(mk)
    } else {
        None
    };

    let mut payload = vec![0u8; payload_len as usize];
    stream.read_exact(&mut payload).ok()?;

    if let Some(mk) = mask_key {
        for i in 0..payload.len() {
            payload[i] ^= mk[i % 4];
        }
    }

    Some((opcode, payload))
}

use std::os::raw::c_char;
use std::ptr;
use sha2::{Sha256, Sha512, Digest};
use md5::Md5;
use hmac::{Hmac, Mac};
use base64::Engine;
use rand::RngCore;

type HmacSha256 = Hmac<Sha256>;

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

fn hex_encode(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_sha256(s: *const c_char) -> *mut c_char {
    let hash = Sha256::digest(cstr_to_str(s).as_bytes());
    str_to_cstr(&hex_encode(&hash))
}

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_sha512(s: *const c_char) -> *mut c_char {
    let hash = Sha512::digest(cstr_to_str(s).as_bytes());
    str_to_cstr(&hex_encode(&hash))
}

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_md5(s: *const c_char) -> *mut c_char {
    let hash = Md5::digest(cstr_to_str(s).as_bytes());
    str_to_cstr(&hex_encode(&hash))
}

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_random_bytes(buf: *mut u8, len: i32) {
    if buf.is_null() || len <= 0 { return; }
    let slice = std::slice::from_raw_parts_mut(buf, len as usize);
    rand::thread_rng().fill_bytes(slice);
}

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_base64_encode(s: *const c_char) -> *mut c_char {
    let encoded = base64::engine::general_purpose::STANDARD.encode(cstr_to_str(s).as_bytes());
    str_to_cstr(&encoded)
}

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_base64_decode(s: *const c_char) -> *mut c_char {
    match base64::engine::general_purpose::STANDARD.decode(cstr_to_str(s).as_bytes()) {
        Ok(bytes) => str_to_cstr(&String::from_utf8_lossy(&bytes)),
        Err(_) => str_to_cstr(""),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_hmac_sha256(key: *const c_char, msg: *const c_char) -> *mut c_char {
    let mut mac = HmacSha256::new_from_slice(cstr_to_str(key).as_bytes())
        .unwrap_or_else(|_| HmacSha256::new_from_slice(b"").unwrap());
    mac.update(cstr_to_str(msg).as_bytes());
    let result = mac.finalize().into_bytes();
    str_to_cstr(&hex_encode(&result))
}

use std::os::raw::c_char;
use std::ptr;
use sha2::{Sha256, Sha512, Digest};
use sha1::Sha1;
use md5::Md5;
use hmac::{Hmac, Mac};
use base64::Engine;
use rand::RngCore;
use aes_gcm::{Aes256Gcm, KeyInit, aead::Aead};
use aes_gcm::aead::generic_array::GenericArray;
use ed25519_dalek::{SigningKey, Signer, Verifier, VerifyingKey};
use argon2::Argon2;

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

fn hex_decode(s: &str) -> Option<Vec<u8>> {
    if s.len() % 2 != 0 { return None; }
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).ok())
        .collect()
}

// ---------------------------------------------------------------------------
// Hash functions
// ---------------------------------------------------------------------------

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
pub unsafe extern "C" fn nex_crypto_sha1(s: *const c_char) -> *mut c_char {
    let hash = Sha1::digest(cstr_to_str(s).as_bytes());
    str_to_cstr(&hex_encode(&hash))
}

// ---------------------------------------------------------------------------
// Random
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_random_bytes(buf: *mut u8, len: i32) {
    if buf.is_null() || len <= 0 { return; }
    let slice = std::slice::from_raw_parts_mut(buf, len as usize);
    rand::thread_rng().fill_bytes(slice);
}

// ---------------------------------------------------------------------------
// Base64
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// HMAC
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_crypto_hmac_sha256(key: *const c_char, msg: *const c_char) -> *mut c_char {
    let mut mac = <HmacSha256 as Mac>::new_from_slice(cstr_to_str(key).as_bytes())
        .unwrap_or_else(|_| <HmacSha256 as Mac>::new_from_slice(b"").unwrap());
    mac.update(cstr_to_str(msg).as_bytes());
    let result = mac.finalize().into_bytes();
    str_to_cstr(&hex_encode(&result))
}

// ---------------------------------------------------------------------------
// AES-256-GCM
// ---------------------------------------------------------------------------

/// Encrypts plaintext with AES-256-GCM.
/// key_hex: 64-char hex string (32 bytes)
/// plaintext: string to encrypt
/// Returns: base64(nonce || ciphertext || tag)  — nonce is 12 bytes prepended
#[no_mangle]
pub unsafe extern "C" fn nex_crypto_aes_encrypt(key_hex: *const c_char, plaintext: *const c_char) -> *mut c_char {
    let key_bytes = match hex_decode(cstr_to_str(key_hex)) {
        Some(k) if k.len() == 32 => k,
        _ => return str_to_cstr(""),
    };
    let key = GenericArray::from_slice(&key_bytes);
    let cipher = Aes256Gcm::new(key);

    let mut nonce_bytes = [0u8; 12];
    rand::thread_rng().fill_bytes(&mut nonce_bytes);
    let nonce = GenericArray::from_slice(&nonce_bytes);

    match cipher.encrypt(nonce, cstr_to_str(plaintext).as_bytes()) {
        Ok(ciphertext) => {
            let mut combined = nonce_bytes.to_vec();
            combined.extend_from_slice(&ciphertext);
            let encoded = base64::engine::general_purpose::STANDARD.encode(&combined);
            str_to_cstr(&encoded)
        }
        Err(_) => str_to_cstr(""),
    }
}

/// Decrypts AES-256-GCM ciphertext.
/// key_hex: 64-char hex string (32 bytes)
/// encrypted_b64: base64(nonce || ciphertext || tag)
/// Returns: decrypted plaintext string, or "" on failure
#[no_mangle]
pub unsafe extern "C" fn nex_crypto_aes_decrypt(key_hex: *const c_char, encrypted_b64: *const c_char) -> *mut c_char {
    let key_bytes = match hex_decode(cstr_to_str(key_hex)) {
        Some(k) if k.len() == 32 => k,
        _ => return str_to_cstr(""),
    };
    let key = GenericArray::from_slice(&key_bytes);
    let cipher = Aes256Gcm::new(key);

    let combined = match base64::engine::general_purpose::STANDARD.decode(cstr_to_str(encrypted_b64).as_bytes()) {
        Ok(b) if b.len() > 12 => b,
        _ => return str_to_cstr(""),
    };

    let nonce = GenericArray::from_slice(&combined[..12]);
    match cipher.decrypt(nonce, &combined[12..]) {
        Ok(plaintext) => str_to_cstr(&String::from_utf8_lossy(&plaintext)),
        Err(_) => str_to_cstr(""),
    }
}

/// Generates a random 32-byte AES-256 key, returned as 64-char hex string.
#[no_mangle]
pub unsafe extern "C" fn nex_crypto_aes_keygen() -> *mut c_char {
    let mut key = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut key);
    str_to_cstr(&hex_encode(&key))
}

// ---------------------------------------------------------------------------
// Ed25519 signatures
// ---------------------------------------------------------------------------

/// Generates an Ed25519 keypair. Returns "pubkey_hex:privkey_hex".
#[no_mangle]
pub unsafe extern "C" fn nex_crypto_ed25519_keygen() -> *mut c_char {
    let mut csprng = rand::thread_rng();
    let signing_key = SigningKey::generate(&mut csprng);
    let verifying_key = signing_key.verifying_key();
    let result = format!("{}:{}", hex_encode(verifying_key.as_bytes()), hex_encode(signing_key.as_bytes()));
    str_to_cstr(&result)
}

/// Signs a message with an Ed25519 private key.
/// privkey_hex: 64-char hex string (32 bytes)
/// msg: message string
/// Returns: 128-char hex string (64-byte signature)
#[no_mangle]
pub unsafe extern "C" fn nex_crypto_ed25519_sign(privkey_hex: *const c_char, msg: *const c_char) -> *mut c_char {
    let key_bytes = match hex_decode(cstr_to_str(privkey_hex)) {
        Some(k) if k.len() == 32 => k,
        _ => return str_to_cstr(""),
    };
    let mut key_arr = [0u8; 32];
    key_arr.copy_from_slice(&key_bytes);
    let signing_key = SigningKey::from_bytes(&key_arr);
    let signature = signing_key.sign(cstr_to_str(msg).as_bytes());
    str_to_cstr(&hex_encode(&signature.to_bytes()))
}

/// Verifies an Ed25519 signature.
/// pubkey_hex: 64-char hex string (32 bytes)
/// msg: message string
/// sig_hex: 128-char hex string (64-byte signature)
/// Returns: 1 if valid, 0 if invalid
#[no_mangle]
pub unsafe extern "C" fn nex_crypto_ed25519_verify(pubkey_hex: *const c_char, msg: *const c_char, sig_hex: *const c_char) -> i64 {
    let pk_str = cstr_to_str(pubkey_hex);
    let msg_str = cstr_to_str(msg);
    let sig_str = cstr_to_str(sig_hex);
    let pub_bytes = match hex_decode(pk_str) {
        Some(k) if k.len() == 32 => k,
        _ => return 0,
    };
    let sig_bytes = match hex_decode(sig_str) {
        Some(s) if s.len() == 64 => s,
        _ => return 0,
    };
    let mut pub_arr = [0u8; 32];
    pub_arr.copy_from_slice(&pub_bytes);
    let verifying_key = match VerifyingKey::from_bytes(&pub_arr) {
        Ok(k) => k,
        Err(_) => return 0,
    };
    let mut sig_arr = [0u8; 64];
    sig_arr.copy_from_slice(&sig_bytes);
    let signature = ed25519_dalek::Signature::from_bytes(&sig_arr);
    match verifying_key.verify(msg_str.as_bytes(), &signature) {
        Ok(_) => 1,
        Err(_) => 0,
    }
}

// ---------------------------------------------------------------------------
// Key Derivation — PBKDF2 and Argon2
// ---------------------------------------------------------------------------

/// Derives a key using PBKDF2-HMAC-SHA256.
/// password, salt: input strings
/// iterations: number of iterations (cast from i64)
/// Returns: 64-char hex string (32-byte derived key)
#[no_mangle]
pub unsafe extern "C" fn nex_crypto_pbkdf2(password: *const c_char, salt: *const c_char, iterations: i64) -> *mut c_char {
    let mut output = [0u8; 32];
    pbkdf2::pbkdf2_hmac::<Sha256>(
        cstr_to_str(password).as_bytes(),
        cstr_to_str(salt).as_bytes(),
        iterations as u32,
        &mut output,
    );
    str_to_cstr(&hex_encode(&output))
}

/// Derives a key using Argon2id (default parameters).
/// password, salt: input strings
/// Returns: 64-char hex string (32-byte derived key)
#[no_mangle]
pub unsafe extern "C" fn nex_crypto_argon2(password: *const c_char, salt: *const c_char) -> *mut c_char {
    let mut output = [0u8; 32];
    let argon2 = Argon2::default();
    match argon2.hash_password_into(
        cstr_to_str(password).as_bytes(),
        cstr_to_str(salt).as_bytes(),
        &mut output,
    ) {
        Ok(_) => str_to_cstr(&hex_encode(&output)),
        Err(_) => str_to_cstr(""),
    }
}

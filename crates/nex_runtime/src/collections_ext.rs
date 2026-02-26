use std::collections::{HashMap, HashSet};
use std::os::raw::c_char;

pub struct IntList {
    pub inner: Vec<i64>,
}

pub struct IntSet {
    pub inner: HashSet<i64>,
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_sort_int(handle: *mut IntList) {
    if handle.is_null() { return; }
    (*handle).inner.sort();
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_reverse(handle: *mut IntList) {
    if handle.is_null() { return; }
    (*handle).inner.reverse();
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_clear(handle: *mut IntList) {
    if handle.is_null() { return; }
    (*handle).inner.clear();
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_contains_int(handle: *mut IntList, value: i64) -> i32 {
    if handle.is_null() { return 0; }
    (*handle).inner.contains(&value) as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_list_index_of_int(handle: *mut IntList, value: i64) -> i32 {
    if handle.is_null() { return -1; }
    (*handle).inner.iter().position(|&v| v == value).map(|i| i as i32).unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn nex_set_new() -> *mut IntSet {
    Box::into_raw(Box::new(IntSet { inner: HashSet::new() }))
}

#[no_mangle]
pub unsafe extern "C" fn nex_set_add(handle: *mut IntSet, value: i64) {
    if handle.is_null() { return; }
    (*handle).inner.insert(value);
}

#[no_mangle]
pub unsafe extern "C" fn nex_set_contains(handle: *mut IntSet, value: i64) -> i32 {
    if handle.is_null() { return 0; }
    (*handle).inner.contains(&value) as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_set_remove(handle: *mut IntSet, value: i64) {
    if handle.is_null() { return; }
    (*handle).inner.remove(&value);
}

#[no_mangle]
pub unsafe extern "C" fn nex_set_size(handle: *mut IntSet) -> i32 {
    if handle.is_null() { return 0; }
    (*handle).inner.len() as i32
}

// ── Map[String, String] runtime ─────────────────────────────────────────

pub struct StrMap {
    pub inner: HashMap<String, String>,
}

unsafe fn cstr_to_string(s: *const c_char) -> String {
    if s.is_null() { return String::new(); }
    std::ffi::CStr::from_ptr(s).to_str().unwrap_or("").to_owned()
}

unsafe fn str_to_cstr(s: &str) -> *mut c_char {
    let len = s.len();
    let ptr = libc::malloc(len + 1) as *mut c_char;
    if ptr.is_null() { std::process::abort(); }
    std::ptr::copy_nonoverlapping(s.as_ptr(), ptr as *mut u8, len);
    *ptr.add(len) = 0;
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn nex_map_new() -> *mut StrMap {
    Box::into_raw(Box::new(StrMap { inner: HashMap::new() }))
}

#[no_mangle]
pub unsafe extern "C" fn nex_map_put(handle: *mut StrMap, key: *const c_char, value: *const c_char) {
    if handle.is_null() { return; }
    (*handle).inner.insert(cstr_to_string(key), cstr_to_string(value));
}

#[no_mangle]
pub unsafe extern "C" fn nex_map_get(handle: *mut StrMap, key: *const c_char) -> *mut c_char {
    if handle.is_null() { return str_to_cstr(""); }
    let k = cstr_to_string(key);
    match (*handle).inner.get(&k) {
        Some(v) => str_to_cstr(v),
        None => str_to_cstr(""),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_map_contains(handle: *mut StrMap, key: *const c_char) -> i32 {
    if handle.is_null() { return 0; }
    (*handle).inner.contains_key(&cstr_to_string(key)) as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_map_remove(handle: *mut StrMap, key: *const c_char) {
    if handle.is_null() { return; }
    (*handle).inner.remove(&cstr_to_string(key));
}

#[no_mangle]
pub unsafe extern "C" fn nex_map_size(handle: *mut StrMap) -> i64 {
    if handle.is_null() { return 0; }
    (*handle).inner.len() as i64
}

/// Returns a List[String] (IntList handle) containing all keys.
#[no_mangle]
pub unsafe extern "C" fn nex_map_keys(handle: *mut StrMap) -> *mut IntList {
    if handle.is_null() {
        return Box::into_raw(Box::new(IntList { inner: vec![] }));
    }
    let mut list = IntList { inner: Vec::with_capacity((*handle).inner.len()) };
    for key in (*handle).inner.keys() {
        list.inner.push(str_to_cstr(key) as i64);
    }
    Box::into_raw(Box::new(list))
}

/// Returns a List[String] (IntList handle) containing all values.
#[no_mangle]
pub unsafe extern "C" fn nex_map_values(handle: *mut StrMap) -> *mut IntList {
    if handle.is_null() {
        return Box::into_raw(Box::new(IntList { inner: vec![] }));
    }
    let mut list = IntList { inner: Vec::with_capacity((*handle).inner.len()) };
    for val in (*handle).inner.values() {
        list.inner.push(str_to_cstr(val) as i64);
    }
    Box::into_raw(Box::new(list))
}

#[no_mangle]
pub unsafe extern "C" fn nex_map_free(handle: *mut StrMap) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

// ── Generic List methods (filter, map, forEach, contains_str) ───────────
// NOTE: Core list API uses *mut Vec<i64> directly (not IntList wrapper).
// These functions must match that convention.

/// Filter a list using a closure callback.
/// The closure is called as `fn(env: i64, element: i64) -> i64`.
/// Elements where the callback returns non-zero are kept.
#[no_mangle]
pub unsafe extern "C" fn nex_list_filter(handle: *mut Vec<i64>, closure_ptr: i64) -> *mut Vec<i64> {
    if handle.is_null() || closure_ptr == 0 {
        return Box::into_raw(Box::new(Vec::<i64>::new()));
    }
    let env = closure_ptr as *const i64;
    let func_ptr = *env;
    let callback: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(func_ptr);
    let mut result = Vec::new();
    for &elem in &(*handle) {
        if callback(closure_ptr, elem) != 0 {
            result.push(elem);
        }
    }
    Box::into_raw(Box::new(result))
}

/// Map a list using a closure callback.
/// The closure is called as `fn(env: i64, element: i64) -> i64`.
/// Returns a new list with the transformed elements.
#[no_mangle]
pub unsafe extern "C" fn nex_list_map(handle: *mut Vec<i64>, closure_ptr: i64) -> *mut Vec<i64> {
    if handle.is_null() || closure_ptr == 0 {
        return Box::into_raw(Box::new(Vec::<i64>::new()));
    }
    let env = closure_ptr as *const i64;
    let func_ptr = *env;
    let callback: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(func_ptr);
    let mut result = Vec::with_capacity((*handle).len());
    for &elem in &(*handle) {
        result.push(callback(closure_ptr, elem));
    }
    Box::into_raw(Box::new(result))
}

/// Iterate over a list calling a closure for each element.
/// The closure is called as `fn(env: i64, element: i64) -> i64`.
#[no_mangle]
pub unsafe extern "C" fn nex_list_foreach(handle: *mut Vec<i64>, closure_ptr: i64) {
    if handle.is_null() || closure_ptr == 0 { return; }
    let env = closure_ptr as *const i64;
    let func_ptr = *env;
    let callback: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(func_ptr);
    for &elem in &(*handle) {
        callback(closure_ptr, elem);
    }
}

/// Check if a list contains a given string value.
/// Compares by string content (not pointer equality).
#[no_mangle]
pub unsafe extern "C" fn nex_list_contains_str(handle: *mut Vec<i64>, value: *const c_char) -> i32 {
    if handle.is_null() || value.is_null() { return 0; }
    let target = std::ffi::CStr::from_ptr(value).to_str().unwrap_or("");
    for &elem in &(*handle) {
        let elem_ptr = elem as *const c_char;
        if !elem_ptr.is_null() {
            let elem_str = std::ffi::CStr::from_ptr(elem_ptr).to_str().unwrap_or("");
            if elem_str == target {
                return 1;
            }
        }
    }
    0
}

use std::collections::HashSet;

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

#[no_mangle]
pub unsafe extern "C" fn nex_map_keys(_handle: *mut u8) -> *mut u8 {
    std::ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn nex_map_values(_handle: *mut u8) -> *mut u8 {
    std::ptr::null_mut()
}

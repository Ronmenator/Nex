use std::alloc::{alloc, Layout};

/// Closure layout (flat i64 array):
///   [0] = func_ptr   (pointer to the lifted function)
///   [1] = n_captures  (number of captured values)
///   [2..] = captured values (cap_0, cap_1, ..., cap_N-1)
///
/// The lifted function always takes `__env: i64` (the closure pointer) as its
/// first parameter, followed by the normal parameters.

/// Allocate a closure with room for `n_captures` captured values.
/// Stores `func_ptr` at slot 0 and `n_captures` at slot 1.
/// Capture slots are zero-initialized.
#[no_mangle]
pub unsafe extern "C" fn nex_closure_alloc(func_ptr: i64, n_captures: i64) -> i64 {
    let total = 2 + n_captures as usize;
    let layout = Layout::array::<i64>(total).unwrap();
    let ptr = alloc(layout) as *mut i64;
    *ptr = func_ptr;
    *ptr.add(1) = n_captures;
    for i in 0..n_captures as usize {
        *ptr.add(2 + i) = 0;
    }
    ptr as i64
}

/// Store a captured value into the closure at the given index.
#[no_mangle]
pub unsafe extern "C" fn nex_closure_set_cap(closure_ptr: i64, index: i64, value: i64) {
    let ptr = closure_ptr as *mut i64;
    *ptr.add(2 + index as usize) = value;
}

/// Load a captured value from the closure at the given index.
#[no_mangle]
pub unsafe extern "C" fn nex_closure_get_cap(closure_ptr: i64, index: i64) -> i64 {
    let ptr = closure_ptr as *const i64;
    *ptr.add(2 + index as usize)
}

/// Extract the function pointer from a closure.
#[no_mangle]
pub unsafe extern "C" fn nex_closure_get_fn(closure_ptr: i64) -> i64 {
    let ptr = closure_ptr as *const i64;
    *ptr
}

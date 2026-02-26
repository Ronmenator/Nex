use std::sync::{Arc, Condvar, Mutex};
use std::thread;

/// Opaque task handle. The async function runs on a worker thread.
/// The result (an i64) is stored when the task completes.
/// `await` blocks on the condvar until done.
struct TaskHandle {
    result: Mutex<Option<i64>>,
    done: Condvar,
}

impl TaskHandle {
    fn new() -> Arc<Self> {
        Arc::new(TaskHandle {
            result: Mutex::new(None),
            done: Condvar::new(),
        })
    }
}

/// Spawn an async task. `func_ptr` is a function pointer that takes
/// no arguments and returns i64. Returns a TaskHandle pointer as i64.
#[no_mangle]
pub unsafe extern "C" fn nex_task_spawn(func_ptr: i64) -> i64 {
    let fp: extern "C" fn() -> i64 = std::mem::transmute(func_ptr);
    let handle = TaskHandle::new();
    let handle_clone = handle.clone();

    thread::spawn(move || {
        let result = fp();
        let mut lock = handle_clone.result.lock().unwrap();
        *lock = Some(result);
        handle_clone.done.notify_all();
    });

    Arc::into_raw(handle) as i64
}

/// Block until the task completes and return its result.
#[no_mangle]
pub unsafe extern "C" fn nex_task_await(handle_ptr: i64) -> i64 {
    if handle_ptr == 0 {
        return 0;
    }
    let handle = Arc::from_raw(handle_ptr as *const TaskHandle);
    let mut lock = handle.result.lock().unwrap();
    while lock.is_none() {
        lock = handle.done.wait(lock).unwrap();
    }
    lock.unwrap()
}

/// Check if a task has completed without blocking.
/// Returns 1 if done, 0 if still running.
#[no_mangle]
pub unsafe extern "C" fn nex_task_is_done(handle_ptr: i64) -> i64 {
    if handle_ptr == 0 {
        return 1;
    }
    let handle = &*(handle_ptr as *const TaskHandle);
    let lock = handle.result.lock().unwrap();
    if lock.is_some() { 1 } else { 0 }
}

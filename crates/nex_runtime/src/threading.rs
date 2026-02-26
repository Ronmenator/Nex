use std::sync::Mutex;

pub struct ThreadHandle {
    handle: Option<std::thread::JoinHandle<()>>,
}

#[no_mangle]
pub unsafe extern "C" fn nex_thread_spawn(func_ptr: extern "C" fn()) -> *mut ThreadHandle {
    let handle = std::thread::spawn(move || {
        func_ptr();
    });
    Box::into_raw(Box::new(ThreadHandle {
        handle: Some(handle),
    }))
}

#[no_mangle]
pub unsafe extern "C" fn nex_thread_join(th: *mut ThreadHandle) -> i32 {
    if th.is_null() { return -1; }
    let mut t = Box::from_raw(th);
    if let Some(h) = t.handle.take() {
        h.join().is_ok() as i32
    } else {
        0
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_thread_sleep(ms: i64) {
    if ms > 0 {
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_thread_current_id() -> i64 {
    let id = std::thread::current().id();
    let s = format!("{:?}", id);
    s.chars().filter(|c| c.is_ascii_digit()).collect::<String>().parse::<i64>().unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_mutex_new() -> *mut Mutex<()> {
    Box::into_raw(Box::new(Mutex::new(())))
}

#[no_mangle]
pub unsafe extern "C" fn nex_mutex_lock(handle: *mut Mutex<()>) {
    if handle.is_null() { return; }
    let mtx = &*handle;
    let _guard = mtx.lock().unwrap();
    std::mem::forget(_guard);
}

#[no_mangle]
pub unsafe extern "C" fn nex_mutex_unlock(_handle: *mut Mutex<()>) {
    // In this simplified model, lock/unlock are advisory.
    // A full implementation would track the MutexGuard.
}

#[no_mangle]
pub unsafe extern "C" fn nex_mutex_free(handle: *mut Mutex<()>) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

// ---------------------------------------------------------------------------
// Thread Pool — channel-based fixed-size worker pool
// ---------------------------------------------------------------------------

use std::sync::Arc;

pub struct NexThreadPool {
    workers: Vec<std::thread::JoinHandle<()>>,
    sender: Option<std::sync::mpsc::Sender<Box<dyn FnOnce() + Send>>>,
}

#[no_mangle]
pub unsafe extern "C" fn nex_threadpool_new(size: i64) -> *mut NexThreadPool {
    let size = size.max(1) as usize;
    let (sender, receiver) = std::sync::mpsc::channel::<Box<dyn FnOnce() + Send>>();
    let receiver = Arc::new(Mutex::new(receiver));
    let mut workers = Vec::with_capacity(size);
    for _ in 0..size {
        let rx = Arc::clone(&receiver);
        let handle = std::thread::spawn(move || {
            loop {
                let task = {
                    let lock = rx.lock().unwrap();
                    lock.recv()
                };
                match task {
                    Ok(job) => job(),
                    Err(_) => break,
                }
            }
        });
        workers.push(handle);
    }
    Box::into_raw(Box::new(NexThreadPool {
        workers,
        sender: Some(sender),
    }))
}

#[no_mangle]
pub unsafe extern "C" fn nex_threadpool_submit(pool: *mut NexThreadPool, func_ptr: i64) {
    if pool.is_null() { return; }
    let func: extern "C" fn() = std::mem::transmute(func_ptr);
    if let Some(ref sender) = (*pool).sender {
        let _ = sender.send(Box::new(move || func()));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_threadpool_shutdown(pool: *mut NexThreadPool) {
    if pool.is_null() { return; }
    let mut pool = Box::from_raw(pool);
    // Drop sender to signal workers to stop
    pool.sender.take();
    for worker in pool.workers.drain(..) {
        let _ = worker.join();
    }
}

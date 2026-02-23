use std::time::{SystemTime, UNIX_EPOCH, Duration};

#[no_mangle]
pub unsafe extern "C" fn nex_time_now_millis() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_time_now_nanos() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as i64)
        .unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_time_sleep_millis(ms: i64) {
    if ms > 0 {
        std::thread::sleep(Duration::from_millis(ms as u64));
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_time_elapsed_millis(start: i64) -> i64 {
    let now = nex_time_now_millis();
    now - start
}

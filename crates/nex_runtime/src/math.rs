use rand::Rng;

#[no_mangle]
pub unsafe extern "C" fn nex_math_abs_int(v: i64) -> i64 {
    v.wrapping_abs()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_abs_float(v: f64) -> f64 {
    v.abs()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_min_int(a: i64, b: i64) -> i64 {
    a.min(b)
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_max_int(a: i64, b: i64) -> i64 {
    a.max(b)
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_min_float(a: f64, b: f64) -> f64 {
    a.min(b)
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_max_float(a: f64, b: f64) -> f64 {
    a.max(b)
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_clamp_int(v: i64, lo: i64, hi: i64) -> i64 {
    v.max(lo).min(hi)
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_clamp_float(v: f64, lo: f64, hi: f64) -> f64 {
    v.max(lo).min(hi)
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_floor(v: f64) -> f64 {
    v.floor()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_ceil(v: f64) -> f64 {
    v.ceil()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_round(v: f64) -> f64 {
    v.round()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_sqrt(v: f64) -> f64 {
    v.sqrt()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_pow(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_sin(v: f64) -> f64 {
    v.sin()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_cos(v: f64) -> f64 {
    v.cos()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_tan(v: f64) -> f64 {
    v.tan()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_log(v: f64) -> f64 {
    v.ln()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_log2(v: f64) -> f64 {
    v.log2()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_log10(v: f64) -> f64 {
    v.log10()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_exp(v: f64) -> f64 {
    v.exp()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_random() -> f64 {
    rand::thread_rng().gen::<f64>()
}

#[no_mangle]
pub unsafe extern "C" fn nex_math_random_range(lo: i64, hi: i64) -> i64 {
    if lo >= hi {
        return lo;
    }
    rand::thread_rng().gen_range(lo..hi)
}

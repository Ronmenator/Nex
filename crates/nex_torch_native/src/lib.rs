use std::os::raw::c_char;
use std::ptr;
use tch::{Device, Kind, Tensor, nn};
use tch::nn::{Module, OptimizerConfig};

unsafe fn cstr_to_str<'a>(s: *const c_char) -> &'a str {
    if s.is_null() { return ""; }
    std::ffi::CStr::from_ptr(s).to_str().unwrap_or("")
}

unsafe fn str_to_cstr(s: &str) -> *mut c_char {
    let len = s.len();
    let p = libc::malloc(len + 1) as *mut c_char;
    if p.is_null() { std::process::abort(); }
    ptr::copy_nonoverlapping(s.as_ptr(), p as *mut u8, len);
    *p.add(len) = 0;
    p
}

unsafe fn read_shape(shape_ptr: *const i64, ndims: i64) -> Vec<i64> {
    if shape_ptr.is_null() || ndims <= 0 { return vec![]; }
    std::slice::from_raw_parts(shape_ptr, ndims as usize).to_vec()
}

fn parse_device(s: &str) -> Device {
    match s {
        "cuda" | "cuda:0" => Device::Cuda(0),
        s if s.starts_with("cuda:") => {
            let idx = s[5..].parse::<usize>().unwrap_or(0);
            Device::Cuda(idx)
        }
        _ => Device::Cpu,
    }
}

// ---------------------------------------------------------------------------
// Tensor creation
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_zeros(shape_ptr: *const i64, ndims: i64) -> *mut Tensor {
    let shape = read_shape(shape_ptr, ndims);
    Box::into_raw(Box::new(Tensor::zeros(&shape, (Kind::Float, Device::Cpu))))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_ones(shape_ptr: *const i64, ndims: i64) -> *mut Tensor {
    let shape = read_shape(shape_ptr, ndims);
    Box::into_raw(Box::new(Tensor::ones(&shape, (Kind::Float, Device::Cpu))))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_rand(shape_ptr: *const i64, ndims: i64) -> *mut Tensor {
    let shape = read_shape(shape_ptr, ndims);
    Box::into_raw(Box::new(Tensor::rand(&shape, (Kind::Float, Device::Cpu))))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_randn(shape_ptr: *const i64, ndims: i64) -> *mut Tensor {
    let shape = read_shape(shape_ptr, ndims);
    Box::into_raw(Box::new(Tensor::randn(&shape, (Kind::Float, Device::Cpu))))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_from_float_data(
    data_ptr: *const f64, shape_ptr: *const i64, ndims: i64,
) -> *mut Tensor {
    let shape = read_shape(shape_ptr, ndims);
    let numel: i64 = shape.iter().product();
    let data = std::slice::from_raw_parts(data_ptr, numel as usize);
    let t = Tensor::from_slice(data).reshape(&shape);
    Box::into_raw(Box::new(t))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_arange(start: f64, end_val: f64, step: f64) -> *mut Tensor {
    let t = Tensor::arange_start_step(start, end_val, step, (Kind::Float, Device::Cpu));
    Box::into_raw(Box::new(t))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_eye(n: i64) -> *mut Tensor {
    Box::into_raw(Box::new(Tensor::eye(n, (Kind::Float, Device::Cpu))))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_free(t: *mut Tensor) {
    if !t.is_null() { drop(Box::from_raw(t)); }
}

// ---------------------------------------------------------------------------
// Tensor operations
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_add(a: *mut Tensor, b: *mut Tensor) -> *mut Tensor {
    if a.is_null() || b.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new(&*a + &*b))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_sub(a: *mut Tensor, b: *mut Tensor) -> *mut Tensor {
    if a.is_null() || b.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new(&*a - &*b))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_mul(a: *mut Tensor, b: *mut Tensor) -> *mut Tensor {
    if a.is_null() || b.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new(&*a * &*b))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_div(a: *mut Tensor, b: *mut Tensor) -> *mut Tensor {
    if a.is_null() || b.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*a).f_div(&*b).unwrap_or_else(|_| Tensor::zeros(&[], (Kind::Float, Device::Cpu)))))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_matmul(a: *mut Tensor, b: *mut Tensor) -> *mut Tensor {
    if a.is_null() || b.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*a).matmul(&*b)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_neg(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).neg()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_exp(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).exp()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_log(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).log()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_sum(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).sum(Kind::Float)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_mean(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).mean(Kind::Float)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_reshape(t: *mut Tensor, shape_ptr: *const i64, ndims: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    let shape = read_shape(shape_ptr, ndims);
    Box::into_raw(Box::new((*t).reshape(&shape)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_transpose(t: *mut Tensor, dim0: i64, dim1: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).transpose(dim0, dim1)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_squeeze(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).squeeze()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_unsqueeze(t: *mut Tensor, dim: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).unsqueeze(dim)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_print(t: *mut Tensor) {
    if t.is_null() { println!("null tensor"); return; }
    (*t).print();
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_shape_dim(t: *mut Tensor, dim: i64) -> i64 {
    if t.is_null() { return 0; }
    (*t).size()[dim as usize]
}

// ---------------------------------------------------------------------------
// Tensor data access
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_get_float(t: *mut Tensor, index: i64) -> f64 {
    if t.is_null() { return 0.0; }
    (*t).double_value(&[index])
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_item_float(t: *mut Tensor) -> f64 {
    if t.is_null() { return 0.0; }
    f64::try_from(&*t).unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_ndim(t: *mut Tensor) -> i64 {
    if t.is_null() { return 0; }
    (*t).dim() as i64
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_numel(t: *mut Tensor) -> i64 {
    if t.is_null() { return 0; }
    (*t).numel() as i64
}

// ---------------------------------------------------------------------------
// Device management
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_cuda_is_available() -> i32 {
    tch::Cuda::is_available() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_cuda_device_count() -> i32 {
    tch::Cuda::device_count() as i32
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_to_device(t: *mut Tensor, device_str: *const c_char) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    let device = parse_device(cstr_to_str(device_str));
    Box::into_raw(Box::new((*t).to_device(device)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_set_num_threads(n: i64) {
    tch::set_num_threads(n as i32);
}

// ---------------------------------------------------------------------------
// Autograd
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_requires_grad(t: *mut Tensor, requires: i32) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    let out = (*t).set_requires_grad(requires != 0);
    Box::into_raw(Box::new(out))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_backward(t: *mut Tensor) {
    if t.is_null() { return; }
    (*t).backward();
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_grad(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).grad()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_no_grad(flag: i32) {
    if flag != 0 {
        tch::no_grad_guard();
    }
}

// ---------------------------------------------------------------------------
// Neural network layers
// ---------------------------------------------------------------------------

pub struct NexModule {
    pub vs: nn::VarStore,
    pub seq: nn::Sequential,
    next_layer: usize,
}

impl NexModule {
    fn new() -> Self {
        let vs = nn::VarStore::new(Device::Cpu);
        let seq = nn::seq();
        Self { vs, seq, next_layer: 0 }
    }

    fn next_layer_name(&mut self) -> String {
        let name = format!("layer_{}", self.next_layer);
        self.next_layer += 1;
        name
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_sequential_new() -> *mut NexModule {
    Box::into_raw(Box::new(NexModule::new()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_linear(module: *mut NexModule, in_features: i64, out_features: i64) {
    if module.is_null() { return; }
    let m = &mut *module;
    let name = m.next_layer_name();
    let path = m.vs.root() / &name;
    let layer = nn::linear(&path, in_features, out_features, Default::default());
    m.seq = std::mem::replace(&mut m.seq, nn::seq()).add(layer);
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_conv2d(module: *mut NexModule, in_ch: i64, out_ch: i64, kernel_size: i64) {
    if module.is_null() { return; }
    let m = &mut *module;
    let name = m.next_layer_name();
    let path = m.vs.root() / &name;
    let config = nn::ConvConfig { stride: 1, padding: 0, ..Default::default() };
    let layer = nn::conv2d(&path, in_ch, out_ch, kernel_size, config);
    m.seq = std::mem::replace(&mut m.seq, nn::seq()).add(layer);
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_relu(module: *mut NexModule) {
    if module.is_null() { return; }
    let m = &mut *module;
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(|t| t.relu());
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_sigmoid(module: *mut NexModule) {
    if module.is_null() { return; }
    let m = &mut *module;
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(|t| t.sigmoid());
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_tanh(module: *mut NexModule) {
    if module.is_null() { return; }
    let m = &mut *module;
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(|t| t.tanh());
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_softmax(module: *mut NexModule, dim: i64) {
    if module.is_null() { return; }
    let m = &mut *module;
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(move |t| t.softmax(dim, Kind::Float));
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_dropout(module: *mut NexModule, p: f64) {
    if module.is_null() { return; }
    let m = &mut *module;
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(move |t| t.dropout(p, true));
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_batch_norm(module: *mut NexModule, features: i64) {
    if module.is_null() { return; }
    let m = &mut *module;
    let name = m.next_layer_name();
    let path = m.vs.root() / &name;
    let bn = nn::batch_norm1d(&path, features, Default::default());
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(move |t| t.apply_t(&bn, false));
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_to_device(module: *mut NexModule, device_str: *const c_char) {
    if module.is_null() { return; }
    let m = &mut *module;
    let device = parse_device(cstr_to_str(device_str));
    m.vs.set_device(device);
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_forward(module: *mut NexModule, input: *mut Tensor) -> *mut Tensor {
    if module.is_null() || input.is_null() { return ptr::null_mut(); }
    let m = &*module;
    let out = m.seq.forward(&*input);
    Box::into_raw(Box::new(out))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_free(module: *mut NexModule) {
    if !module.is_null() { drop(Box::from_raw(module)); }
}

// ---------------------------------------------------------------------------
// Loss functions
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_loss_mse(pred: *mut Tensor, target: *mut Tensor) -> *mut Tensor {
    if pred.is_null() || target.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*pred).mse_loss(&*target, tch::Reduction::Mean)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_loss_cross_entropy(pred: *mut Tensor, target: *mut Tensor) -> *mut Tensor {
    if pred.is_null() || target.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*pred).cross_entropy_for_logits(&*target)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_loss_bce(pred: *mut Tensor, target: *mut Tensor) -> *mut Tensor {
    if pred.is_null() || target.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*pred).binary_cross_entropy_with_logits::<Tensor>(
        &*target, None, None, tch::Reduction::Mean,
    )))
}

// ---------------------------------------------------------------------------
// Optimizers
// ---------------------------------------------------------------------------

pub struct NexOptimizer {
    inner: nn::Optimizer,
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_optim_sgd(module: *mut NexModule, lr: f64) -> *mut NexOptimizer {
    if module.is_null() { return ptr::null_mut(); }
    let m = &*module;
    let opt = nn::Sgd::default().build(&m.vs, lr).unwrap();
    Box::into_raw(Box::new(NexOptimizer { inner: opt }))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_optim_adam(module: *mut NexModule, lr: f64) -> *mut NexOptimizer {
    if module.is_null() { return ptr::null_mut(); }
    let m = &*module;
    let opt = nn::Adam::default().build(&m.vs, lr).unwrap();
    Box::into_raw(Box::new(NexOptimizer { inner: opt }))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_optim_step(opt: *mut NexOptimizer) {
    if opt.is_null() { return; }
    (*opt).inner.step();
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_optim_zero_grad(opt: *mut NexOptimizer) {
    if opt.is_null() { return; }
    (*opt).inner.zero_grad();
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_optim_free(opt: *mut NexOptimizer) {
    if !opt.is_null() { drop(Box::from_raw(opt)); }
}

// ---------------------------------------------------------------------------
// Model I/O
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_model_save(module: *mut NexModule, path_str: *const c_char) {
    if module.is_null() { return; }
    let m = &*module;
    let _ = m.vs.save(cstr_to_str(path_str));
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_model_load(module: *mut NexModule, path_str: *const c_char) {
    if module.is_null() { return; }
    let m = &mut *module;
    let _ = m.vs.load(cstr_to_str(path_str));
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_jit_load(path_str: *const c_char) -> *mut tch::CModule {
    match tch::CModule::load(cstr_to_str(path_str)) {
        Ok(m) => Box::into_raw(Box::new(m)),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_jit_forward(module: *mut tch::CModule, input: *mut Tensor) -> *mut Tensor {
    if module.is_null() || input.is_null() { return ptr::null_mut(); }
    let m = &*module;
    match m.forward_ts(&[&*input]) {
        Ok(t) => Box::into_raw(Box::new(t)),
        Err(_) => ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Extended tensor operations — scalar arithmetic
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_add_scalar(t: *mut Tensor, scalar: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new(&*t + scalar))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_mul_scalar(t: *mut Tensor, scalar: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new(&*t * scalar))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_div_scalar(t: *mut Tensor, scalar: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).f_div_scalar(scalar).unwrap_or_else(|_| Tensor::zeros(&[], (Kind::Float, Device::Cpu)))))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_pow_scalar(t: *mut Tensor, exponent: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).pow_tensor_scalar(exponent)))
}

// ---------------------------------------------------------------------------
// Extended tensor operations — element-wise math
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_sqrt(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).sqrt()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_abs(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).abs()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_clamp(t: *mut Tensor, min_val: f64, max_val: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).clamp(min_val, max_val)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_softmax(t: *mut Tensor, dim: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).softmax(dim, Kind::Float)))
}

// ---------------------------------------------------------------------------
// Extended tensor operations — comparison (return bool/uint8 tensors)
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_eq_scalar(t: *mut Tensor, value: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).eq(value)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_gt_scalar(t: *mut Tensor, value: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).gt(value)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_lt_scalar(t: *mut Tensor, value: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).lt(value)))
}

// ---------------------------------------------------------------------------
// Extended tensor operations — masking and triangular
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_tril(t: *mut Tensor, diagonal: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).tril(diagonal)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_triu(t: *mut Tensor, diagonal: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).triu(diagonal)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_masked_fill(t: *mut Tensor, mask: *mut Tensor, value: f64) -> *mut Tensor {
    if t.is_null() || mask.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).f_masked_fill(&*mask, value).unwrap_or_else(|_| (*t).shallow_clone())))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_where_self(condition: *mut Tensor, x: *mut Tensor, y: *mut Tensor) -> *mut Tensor {
    if condition.is_null() || x.is_null() || y.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*x).f_where_self(&*condition, &*y).unwrap_or_else(|_| (*x).shallow_clone())))
}

// ---------------------------------------------------------------------------
// Extended tensor operations — reduction with dimension
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_sum_dim(t: *mut Tensor, dim: i64, keepdim: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).sum_dim_intlist(&[dim][..], keepdim != 0, Kind::Float)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_mean_dim(t: *mut Tensor, dim: i64, keepdim: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).mean_dim(&[dim][..], keepdim != 0, Kind::Float)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_argmax(t: *mut Tensor, dim: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).argmax(dim, false)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_max_dim(t: *mut Tensor, dim: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    let (values, _indices) = (*t).max_dim(dim, false);
    Box::into_raw(Box::new(values))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_min_dim(t: *mut Tensor, dim: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    let (values, _indices) = (*t).min_dim(dim, false);
    Box::into_raw(Box::new(values))
}

// ---------------------------------------------------------------------------
// Extended tensor operations — shape and indexing
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_cat(a: *mut Tensor, b: *mut Tensor, dim: i64) -> *mut Tensor {
    if a.is_null() || b.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new(Tensor::cat(&[&*a, &*b], dim)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_narrow(t: *mut Tensor, dim: i64, start: i64, length: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).narrow(dim, start, length)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_index_select(t: *mut Tensor, dim: i64, index: *mut Tensor) -> *mut Tensor {
    if t.is_null() || index.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).index_select(dim, &*index)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_flatten(t: *mut Tensor, start_dim: i64, end_dim: i64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).flatten(start_dim, end_dim)))
}

// ---------------------------------------------------------------------------
// Extended tensor operations — creation helpers
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_ones_like(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).ones_like()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_zeros_like(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).zeros_like()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_full_like(t: *mut Tensor, value: f64) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).full_like(value)))
}

// ---------------------------------------------------------------------------
// Extended tensor operations — utility
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_clone(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    let out = (*t).zeros_like();
    Box::into_raw(Box::new(out + &*t))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_detach(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).detach()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_contiguous(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).contiguous()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_to_dtype_float(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).to_kind(Kind::Float)))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_to_dtype_long(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).to_kind(Kind::Int64)))
}

// ---------------------------------------------------------------------------
// Optimizer — learning rate scheduling
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_optim_set_lr(opt: *mut NexOptimizer, lr: f64) {
    if opt.is_null() { return; }
    (*opt).inner.set_lr(lr);
}

// ---------------------------------------------------------------------------
// Gradient clipping
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_clip_grad_norm(module: *mut NexModule, max_norm: f64) {
    if module.is_null() { return; }
    let m = &*module;
    let params: Vec<Tensor> = m.vs.trainable_variables();
    if params.is_empty() { return; }
    // Compute total gradient norm
    let mut total_norm_sq = 0.0f64;
    for p in &params {
        let g = p.grad();
        if g.defined() {
            let norm: f64 = f64::try_from(&g.norm()).unwrap_or(0.0);
            total_norm_sq += norm * norm;
        }
    }
    let total_norm = total_norm_sq.sqrt();
    let clip_coef = max_norm / (total_norm + 1e-6);
    if clip_coef < 1.0 {
        let _guard = tch::no_grad_guard();
        for p in &params {
            let mut g = p.grad();
            if g.defined() {
                let _ = g.f_mul_scalar_(clip_coef);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Weight initialization
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_init_normal(module: *mut NexModule, std: f64) {
    if module.is_null() { return; }
    let m = &*module;
    let _guard = tch::no_grad_guard();
    for mut p in m.vs.trainable_variables() {
        let _ = p.init(nn::Init::Randn { mean: 0.0, stdev: std });
    }
}

// ---------------------------------------------------------------------------
// Trigonometric tensor operations
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_sin(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).sin()))
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_cos(t: *mut Tensor) -> *mut Tensor {
    if t.is_null() { return ptr::null_mut(); }
    Box::into_raw(Box::new((*t).cos()))
}

// ---------------------------------------------------------------------------
// Extended NN layers
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_layer_norm(module: *mut NexModule, normalized_shape: i64) {
    if module.is_null() { return; }
    let m = &mut *module;
    let name = m.next_layer_name();
    let path = m.vs.root() / &name;
    let ln = nn::layer_norm(&path, vec![normalized_shape], Default::default());
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(move |t| t.apply(&ln));
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_gelu(module: *mut NexModule) {
    if module.is_null() { return; }
    let m = &mut *module;
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(|t| t.gelu("none"));
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_linear_no_bias(module: *mut NexModule, in_features: i64, out_features: i64) {
    if module.is_null() { return; }
    let m = &mut *module;
    let name = m.next_layer_name();
    let path = m.vs.root() / &name;
    let config = nn::LinearConfig { bias: false, ..Default::default() };
    let layer = nn::linear(&path, in_features, out_features, config);
    m.seq = std::mem::replace(&mut m.seq, nn::seq()).add(layer);
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_rms_norm(module: *mut NexModule, dim: i64) {
    if module.is_null() { return; }
    let m = &mut *module;
    let name = m.next_layer_name();
    let path = m.vs.root() / &name;
    let scale = path.var("scale", &[dim], nn::Init::Const(1.0));
    let eps = 1e-6f64;
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(move |t| {
            let variance = t.pow_tensor_scalar(2.0).mean_dim(&[-1][..], true, Kind::Float);
            let normed = t * (variance + eps).rsqrt();
            normed * &scale
        });
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_nn_embedding(module: *mut NexModule, num_embeddings: i64, embedding_dim: i64) {
    if module.is_null() { return; }
    let m = &mut *module;
    let name = m.next_layer_name();
    let path = m.vs.root() / &name;
    let emb = nn::embedding(&path, num_embeddings, embedding_dim, Default::default());
    m.seq = std::mem::replace(&mut m.seq, nn::seq())
        .add_fn(move |t| {
            let indices = t.to_kind(Kind::Int64);
            emb.forward(&indices)
        });
}

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nex_torch_manual_seed(seed: i64) {
    tch::manual_seed(seed);
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_version() -> *mut c_char {
    str_to_cstr("libtorch (via tch-rs)")
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_tensor_to_string(t: *mut Tensor) -> *mut c_char {
    if t.is_null() { return str_to_cstr("null"); }
    str_to_cstr(&format!("{:?}", *t))
}

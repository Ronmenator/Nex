//! nex3d native engine — Game window, input, timing, camera, and 3D rendering.
//! Compiled as a cdylib (.dll / .so) and loaded dynamically by the Nex runtime.

use std::cell::RefCell;
use std::collections::HashSet;
use std::os::raw::c_char;
use std::sync::Arc;
use std::time::Instant;

// -----------------------------------------------------------------------
// UI overlay bridge: look up UI DLL symbols at runtime so the engine can
// render the UI overlay framebuffer and forward mouse events without a
// compile-time dependency on nex_ui_native.
// -----------------------------------------------------------------------

mod ui_bridge {
    use std::sync::OnceLock;

    type OverlayRenderFn = unsafe extern "C" fn(i64, i64) -> *const u32;
    type OverlayMouseMoveFn = unsafe extern "C" fn(i64, i64);
    type OverlayMouseBtnFn = unsafe extern "C" fn(i64, i64);

    struct UiBridge {
        overlay_render: Option<OverlayRenderFn>,
        overlay_mouse_move: Option<OverlayMouseMoveFn>,
        overlay_mouse_down: Option<OverlayMouseBtnFn>,
        overlay_mouse_up: Option<OverlayMouseBtnFn>,
    }

    static BRIDGE: OnceLock<UiBridge> = OnceLock::new();

    unsafe fn lookup_symbol(name: &[u8]) -> *const () {
        #[cfg(windows)]
        {
            extern "system" {
                fn GetModuleHandleA(name: *const u8) -> *mut core::ffi::c_void;
                fn GetProcAddress(
                    module: *mut core::ffi::c_void,
                    name: *const u8,
                ) -> *const ();
            }
            // Try the already-loaded UI DLL
            let module = GetModuleHandleA(b"nex_ui_native.dll\0".as_ptr());
            if module.is_null() {
                return std::ptr::null();
            }
            let ptr = GetProcAddress(module, name.as_ptr());
            if ptr.is_null() { std::ptr::null() } else { ptr }
        }
        #[cfg(not(windows))]
        {
            extern "C" {
                fn dlsym(
                    handle: *mut core::ffi::c_void,
                    symbol: *const u8,
                ) -> *const ();
            }
            // RTLD_DEFAULT (0 on Linux/macOS) searches all loaded shared objects
            let ptr = dlsym(std::ptr::null_mut(), name.as_ptr());
            if ptr.is_null() { std::ptr::null() } else { ptr }
        }
    }

    fn get_bridge() -> &'static UiBridge {
        BRIDGE.get_or_init(|| unsafe {
            UiBridge {
                overlay_render: {
                    let p = lookup_symbol(b"nex_ui_overlay_render\0");
                    if p.is_null() { None } else { Some(std::mem::transmute(p)) }
                },
                overlay_mouse_move: {
                    let p = lookup_symbol(b"nex_ui_overlay_mouse_move\0");
                    if p.is_null() { None } else { Some(std::mem::transmute(p)) }
                },
                overlay_mouse_down: {
                    let p = lookup_symbol(b"nex_ui_overlay_mouse_down\0");
                    if p.is_null() { None } else { Some(std::mem::transmute(p)) }
                },
                overlay_mouse_up: {
                    let p = lookup_symbol(b"nex_ui_overlay_mouse_up\0");
                    if p.is_null() { None } else { Some(std::mem::transmute(p)) }
                },
            }
        })
    }

    /// Render the UI overlay and return the framebuffer pixels (ARGB u32), width, height.
    pub fn render_overlay(w: u32, h: u32) -> Option<(Vec<u32>, u32, u32)> {
        let bridge = get_bridge();
        let render_fn = bridge.overlay_render?;
        unsafe {
            let ptr = render_fn(w as i64, h as i64);
            if ptr.is_null() {
                return None;
            }
            let total = (w as usize) * (h as usize);
            let pixels = std::slice::from_raw_parts(ptr, total).to_vec();
            Some((pixels, w, h))
        }
    }

    pub fn mouse_move(x: i64, y: i64) {
        if let Some(f) = get_bridge().overlay_mouse_move {
            unsafe { f(x, y) }
        }
    }

    pub fn mouse_down(x: i64, y: i64) {
        if let Some(f) = get_bridge().overlay_mouse_down {
            unsafe { f(x, y) }
        }
    }

    pub fn mouse_up(x: i64, y: i64) {
        if let Some(f) = get_bridge().overlay_mouse_up {
            unsafe { f(x, y) }
        }
    }
}

use pollster::FutureExt;
use wgpu::util::DeviceExt;
use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::{ElementState, MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::window::{Window, WindowAttributes};

// -----------------------------------------------------------------------
// Vertex format: position (xyz) + color (rgb), 6 floats = 24 bytes
// -----------------------------------------------------------------------

#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct Vertex {
    position: [f32; 3],
    color: [f32; 3],
}

impl Vertex {
    fn layout() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Vertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[
                wgpu::VertexAttribute {
                    offset: 0,
                    shader_location: 0,
                    format: wgpu::VertexFormat::Float32x3,
                },
                wgpu::VertexAttribute {
                    offset: 12,
                    shader_location: 1,
                    format: wgpu::VertexFormat::Float32x3,
                },
            ],
        }
    }
}

// -----------------------------------------------------------------------
// Embedded WGSL shader
// -----------------------------------------------------------------------

const SHADER_SRC: &str = r#"
struct Uniforms {
view_proj: mat4x4<f32>,
};
@group(0) @binding(0) var<uniform> uniforms: Uniforms;

struct VertexInput {
@location(0) position: vec3<f32>,
@location(1) color: vec3<f32>,
};

struct VertexOutput {
@builtin(position) clip_position: vec4<f32>,
@location(0) color: vec3<f32>,
@location(1) world_pos: vec3<f32>,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
var out: VertexOutput;
out.clip_position = uniforms.view_proj * vec4<f32>(in.position, 1.0);
out.color = in.color;
out.world_pos = in.position;
return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
// Compute flat face normal from screen-space derivatives of world position
let dx = dpdx(in.world_pos);
let dy = dpdy(in.world_pos);
let normal = normalize(cross(dx, dy));

// Directional light from upper-right-front
let light_dir = normalize(vec3<f32>(0.4, 0.8, 0.6));
let ndotl = max(dot(normal, light_dir), 0.0);

let ambient = 0.25;
let diffuse = 0.75 * ndotl;
let lit = in.color * (ambient + diffuse);

return vec4<f32>(lit, 1.0);
}
"#;

// -----------------------------------------------------------------------
// Key code mapping: winit KeyCode -> our integer code
// -----------------------------------------------------------------------

fn keycode_to_int(key: KeyCode) -> Option<u32> {
    match key {
        KeyCode::KeyA => Some(0),
        KeyCode::KeyB => Some(1),
        KeyCode::KeyC => Some(2),
        KeyCode::KeyD => Some(3),
        KeyCode::KeyE => Some(4),
        KeyCode::KeyF => Some(5),
        KeyCode::KeyG => Some(6),
        KeyCode::KeyH => Some(7),
        KeyCode::KeyI => Some(8),
        KeyCode::KeyJ => Some(9),
        KeyCode::KeyK => Some(10),
        KeyCode::KeyL => Some(11),
        KeyCode::KeyM => Some(12),
        KeyCode::KeyN => Some(13),
        KeyCode::KeyO => Some(14),
        KeyCode::KeyP => Some(15),
        KeyCode::KeyQ => Some(16),
        KeyCode::KeyR => Some(17),
        KeyCode::KeyS => Some(18),
        KeyCode::KeyT => Some(19),
        KeyCode::KeyU => Some(20),
        KeyCode::KeyV => Some(21),
        KeyCode::KeyW => Some(22),
        KeyCode::KeyX => Some(23),
        KeyCode::KeyY => Some(24),
        KeyCode::KeyZ => Some(25),
        KeyCode::Digit0 => Some(26),
        KeyCode::Digit1 => Some(27),
        KeyCode::Digit2 => Some(28),
        KeyCode::Digit3 => Some(29),
        KeyCode::Digit4 => Some(30),
        KeyCode::Digit5 => Some(31),
        KeyCode::Digit6 => Some(32),
        KeyCode::Digit7 => Some(33),
        KeyCode::Digit8 => Some(34),
        KeyCode::Digit9 => Some(35),
        KeyCode::Escape => Some(36),
        KeyCode::Space => Some(37),
        KeyCode::Enter => Some(38),
        KeyCode::Tab => Some(39),
        KeyCode::Backspace => Some(40),
        KeyCode::ArrowLeft => Some(41),
        KeyCode::ArrowRight => Some(42),
        KeyCode::ArrowUp => Some(43),
        KeyCode::ArrowDown => Some(44),
        KeyCode::ShiftLeft => Some(45),
        KeyCode::ControlLeft => Some(46),
        KeyCode::AltLeft => Some(47),
        KeyCode::F1 => Some(48),
        KeyCode::F2 => Some(49),
        KeyCode::F3 => Some(50),
        KeyCode::F4 => Some(51),
        KeyCode::F5 => Some(52),
        KeyCode::F6 => Some(53),
        KeyCode::F7 => Some(54),
        KeyCode::F8 => Some(55),
        KeyCode::F9 => Some(56),
        KeyCode::F10 => Some(57),
        KeyCode::F11 => Some(58),
        KeyCode::F12 => Some(59),
        _ => None,
    }
}

fn mouse_button_to_int(btn: MouseButton) -> Option<u32> {
    match btn {
        MouseButton::Left => Some(0),
        MouseButton::Right => Some(1),
        MouseButton::Middle => Some(2),
        _ => None,
    }
}

// -----------------------------------------------------------------------
// 4x4 matrix math (for view/projection computation on the Rust side)
// -----------------------------------------------------------------------

type Mat4 = [f32; 16];

#[allow(dead_code)]
fn mat4_identity() -> Mat4 {
    [
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0,
    ]
}

fn mat4_multiply(a: &Mat4, b: &Mat4) -> Mat4 {
    let mut r = [0.0f32; 16];
    for row in 0..4 {
        for col in 0..4 {
            let mut sum = 0.0f32;
            for k in 0..4 {
                sum += a[row * 4 + k] * b[k * 4 + col];
            }
            r[row * 4 + col] = sum;
        }
    }
    r
}

fn mat4_look_at(eye: [f32; 3], target: [f32; 3], up: [f32; 3]) -> Mat4 {
    let f = normalize([
        target[0] - eye[0],
        target[1] - eye[1],
        target[2] - eye[2],
    ]);
    let s = normalize(cross(f, up));
    let u = cross(s, f);
    [
        s[0],  s[1],  s[2],  -dot(s, eye),
        u[0],  u[1],  u[2],  -dot(u, eye),
        -f[0], -f[1], -f[2],  dot(f, eye),
        0.0,   0.0,   0.0,   1.0,
    ]
}

fn mat4_perspective(fov_y: f32, aspect: f32, near: f32, far: f32) -> Mat4 {
    let half_tan = (fov_y / 2.0).tan();
    [
        1.0 / (aspect * half_tan), 0.0, 0.0, 0.0,
        0.0, 1.0 / half_tan, 0.0, 0.0,
        0.0, 0.0, -(far + near) / (far - near), -(2.0 * far * near) / (far - near),
        0.0, 0.0, -1.0, 0.0,
    ]
}

fn cross(a: [f32; 3], b: [f32; 3]) -> [f32; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

fn dot(a: [f32; 3], b: [f32; 3]) -> f32 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn normalize(v: [f32; 3]) -> [f32; 3] {
    let len = (v[0] * v[0] + v[1] * v[1] + v[2] * v[2]).sqrt();
    if len < 1e-6 {
        return [0.0, 0.0, 0.0];
    }
    [v[0] / len, v[1] / len, v[2] / len]
}

// -----------------------------------------------------------------------
// UI overlay blit shader (fullscreen quad, alpha blending)
// -----------------------------------------------------------------------

const UI_BLIT_SHADER: &str = r#"
struct VertexOutput {
@builtin(position) pos: vec4<f32>,
@location(0) uv: vec2<f32>,
};

@vertex
fn vs_main(@builtin(vertex_index) idx: u32) -> VertexOutput {
var positions = array<vec2<f32>, 6>(
    vec2(-1.0, 1.0),
    vec2(-1.0, -1.0),
    vec2(1.0, -1.0),
    vec2(-1.0, 1.0),
    vec2(1.0, -1.0),
    vec2(1.0, 1.0),
);
var uvs = array<vec2<f32>, 6>(
    vec2(0.0, 0.0),
    vec2(0.0, 1.0),
    vec2(1.0, 1.0),
    vec2(0.0, 0.0),
    vec2(1.0, 1.0),
    vec2(1.0, 0.0),
);
var out: VertexOutput;
out.pos = vec4(positions[idx], 0.0, 1.0);
out.uv = uvs[idx];
return out;
}

@group(0) @binding(0) var fb_texture: texture_2d<f32>;
@group(0) @binding(1) var fb_sampler: sampler;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
return textureSample(fb_texture, fb_sampler, in.uv);
}
"#;

// -----------------------------------------------------------------------
// Engine state
// -----------------------------------------------------------------------

struct EngineState {
    // Window
    title: String,
    width: u32,
    height: u32,
    running: bool,
    window: Option<Arc<Window>>,

    // WGPU core
    surface: Option<wgpu::Surface<'static>>,
    device: Option<wgpu::Device>,
    queue: Option<wgpu::Queue>,
    config: Option<wgpu::SurfaceConfiguration>,

    // Render pipeline
    render_pipeline: Option<wgpu::RenderPipeline>,
    uniform_buffer: Option<wgpu::Buffer>,
    bind_group: Option<wgpu::BindGroup>,
    depth_view: Option<wgpu::TextureView>,

    // Clear color
    clear_color: [f64; 4],

    // User callback
    update_fn: Option<extern "C" fn()>,

    // Vertices accumulated this frame
    vertices: Vec<Vertex>,

    // Camera
    camera_pos: [f64; 3],
    camera_target: [f64; 3],
    camera_up: [f64; 3],
    fov: f64,
    aspect: f64,
    near_plane: f64,
    far_plane: f64,

    // Input: keyboard
    keys_down: HashSet<u32>,
    keys_pressed: HashSet<u32>,
    keys_released: HashSet<u32>,

    // Input: mouse
    mouse_x: f64,
    mouse_y: f64,
    mouse_dx: f64,
    mouse_dy: f64,
    prev_mouse_x: f64,
    prev_mouse_y: f64,
    mouse_buttons_down: HashSet<u32>,
    mouse_buttons_pressed: HashSet<u32>,

    // Timing
    start_time: Instant,
    last_frame_time: Instant,
    delta_time: f64,
    elapsed_time: f64,
    frame_count: u64,

    // UI overlay
    ui_overlay_enabled: bool,
    ui_blit_pipeline: Option<wgpu::RenderPipeline>,
    ui_blit_bind_group_layout: Option<wgpu::BindGroupLayout>,
    ui_blit_sampler: Option<wgpu::Sampler>,
    ui_fb_texture: Option<wgpu::Texture>,
    ui_bind_group: Option<wgpu::BindGroup>,
    ui_fb_width: u32,
    ui_fb_height: u32,
}

impl EngineState {
    fn new(title: String, width: u32, height: u32) -> Self {
        let now = Instant::now();
        Self {
            title,
            width,
            height,
            running: true,
            window: None,
            surface: None,
            device: None,
            queue: None,
            config: None,
            render_pipeline: None,
            uniform_buffer: None,
            bind_group: None,
            depth_view: None,
            clear_color: [0.1, 0.1, 0.15, 1.0],
            update_fn: None,
            vertices: Vec::with_capacity(4096),
            camera_pos: [0.0, 2.0, 5.0],
            camera_target: [0.0, 0.0, 0.0],
            camera_up: [0.0, 1.0, 0.0],
            fov: std::f64::consts::FRAC_PI_3,
            aspect: width as f64 / height.max(1) as f64,
            near_plane: 0.1,
            far_plane: 1000.0,
            keys_down: HashSet::new(),
            keys_pressed: HashSet::new(),
            keys_released: HashSet::new(),
            mouse_x: 0.0,
            mouse_y: 0.0,
            mouse_dx: 0.0,
            mouse_dy: 0.0,
            prev_mouse_x: 0.0,
            prev_mouse_y: 0.0,
            mouse_buttons_down: HashSet::new(),
            mouse_buttons_pressed: HashSet::new(),
            start_time: now,
            last_frame_time: now,
            delta_time: 0.0,
            elapsed_time: 0.0,
            frame_count: 0,
            ui_overlay_enabled: false,
            ui_blit_pipeline: None,
            ui_blit_bind_group_layout: None,
            ui_blit_sampler: None,
            ui_fb_texture: None,
            ui_bind_group: None,
            ui_fb_width: 0,
            ui_fb_height: 0,
        }
    }

    fn create_depth_texture(device: &wgpu::Device, width: u32, height: u32) -> wgpu::TextureView {
        let depth_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("depth_texture"),
            size: wgpu::Extent3d {
                width: width.max(1),
                height: height.max(1),
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Depth32Float,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        });
        depth_texture.create_view(&Default::default())
    }

}

thread_local! {
    static STATE: RefCell<Option<EngineState>> = RefCell::new(None);
}

fn with_state<R>(f: impl FnOnce(&mut EngineState) -> R) -> R {
    STATE.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let state = borrow
            .as_mut()
            .expect("Engine not initialized; call engine_window_create first");
        f(state)
    })
}

fn try_with_state<R>(f: impl FnOnce(&mut EngineState) -> R) -> Option<R> {
    STATE.with(|cell| {
        let mut borrow = cell.borrow_mut();
        borrow.as_mut().map(|state| f(state))
    })
}

unsafe fn cstr_to_string(s: *const c_char) -> String {
    if s.is_null() {
        return String::new();
    }
    std::ffi::CStr::from_ptr(s)
        .to_str()
        .unwrap_or("")
        .to_string()
}

// -----------------------------------------------------------------------
// Application handler
// -----------------------------------------------------------------------

pub struct EngineApp;

impl EngineApp {
    fn get_ui_framebuffer() -> Option<(Vec<u32>, u32, u32)> {
        let (w, h) = with_state(|s| (s.width, s.height));
        ui_bridge::render_overlay(w, h)
    }
}

impl ApplicationHandler for EngineApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        with_state(|state| {
            if state.window.is_some() {
                return;
            }

            // Create window
            let attrs = WindowAttributes::default()
                .with_title(&state.title)
                .with_inner_size(LogicalSize::new(state.width as f64, state.height as f64));
            let window =
                Arc::new(event_loop.create_window(attrs).expect("create window"));

            // Initialize WGPU
            let instance = wgpu::Instance::new(wgpu::InstanceDescriptor::default());
            let surface = instance
                .create_surface(window.clone())
                .expect("create surface");
            let adapter = instance
                .request_adapter(&wgpu::RequestAdapterOptions {
                    power_preference: wgpu::PowerPreference::HighPerformance,
                    compatible_surface: Some(&surface),
                    force_fallback_adapter: false,
                })
                .block_on()
                .expect("request adapter");

            let (device, queue) = adapter
                .request_device(&wgpu::DeviceDescriptor::default(), None)
                .block_on()
                .expect("request device");

            let caps = surface.get_capabilities(&adapter);
            let format = caps
                .formats
                .iter()
                .find(|f| f.is_srgb())
                .copied()
                .unwrap_or(caps.formats[0]);
            let config = wgpu::SurfaceConfiguration {
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                format,
                width: state.width.max(1),
                height: state.height.max(1),
                present_mode: wgpu::PresentMode::AutoVsync,
                alpha_mode: caps.alpha_modes[0],
                view_formats: vec![],
                desired_maximum_frame_latency: 2,
            };
            surface.configure(&device, &config);

            // Create shader module
            let shader =
                device.create_shader_module(wgpu::ShaderModuleDescriptor {
                    label: Some("engine_shader"),
                    source: wgpu::ShaderSource::Wgsl(SHADER_SRC.into()),
                });

            // Uniform buffer (4x4 matrix = 64 bytes)
            let uniform_buffer =
                device.create_buffer(&wgpu::BufferDescriptor {
                    label: Some("uniform_buffer"),
                    size: 64,
                    usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                    mapped_at_creation: false,
                });

            // Bind group layout & bind group
            let bind_group_layout =
                device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("bind_group_layout"),
                    entries: &[wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    }],
                });

            let bind_group =
                device.create_bind_group(&wgpu::BindGroupDescriptor {
                    label: Some("bind_group"),
                    layout: &bind_group_layout,
                    entries: &[wgpu::BindGroupEntry {
                        binding: 0,
                        resource: uniform_buffer.as_entire_binding(),
                    }],
                });

            // Pipeline layout
            let pipeline_layout =
                device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("pipeline_layout"),
                    bind_group_layouts: &[&bind_group_layout],
                    push_constant_ranges: &[],
                });

            // Render pipeline
            let render_pipeline =
                device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: Some("render_pipeline"),
                    layout: Some(&pipeline_layout),
                    vertex: wgpu::VertexState {
                        module: &shader,
                        entry_point: Some("vs_main"),
                        buffers: &[Vertex::layout()],
                        compilation_options: Default::default(),
                    },
                    fragment: Some(wgpu::FragmentState {
                        module: &shader,
                        entry_point: Some("fs_main"),
                        targets: &[Some(wgpu::ColorTargetState {
                            format,
                            blend: Some(wgpu::BlendState::REPLACE),
                            write_mask: wgpu::ColorWrites::ALL,
                        })],
                        compilation_options: Default::default(),
                    }),
                    primitive: wgpu::PrimitiveState {
                        topology: wgpu::PrimitiveTopology::TriangleList,
                        strip_index_format: None,
                        front_face: wgpu::FrontFace::Ccw,
                        cull_mode: None, // No culling for now so both sides are visible
                        polygon_mode: wgpu::PolygonMode::Fill,
                        unclipped_depth: false,
                        conservative: false,
                    },
                    depth_stencil: Some(wgpu::DepthStencilState {
                        format: wgpu::TextureFormat::Depth32Float,
                        depth_write_enabled: true,
                        depth_compare: wgpu::CompareFunction::Less,
                        stencil: wgpu::StencilState::default(),
                        bias: wgpu::DepthBiasState::default(),
                    }),
                    multisample: wgpu::MultisampleState::default(),
                    multiview: None,
                    cache: None,
                });

            // Depth texture
            let depth_view = EngineState::create_depth_texture(
                &device,
                state.width,
                state.height,
            );

            // UI overlay blit pipeline (alpha blended fullscreen quad)
            let ui_blit_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("ui_blit_shader"),
                source: wgpu::ShaderSource::Wgsl(UI_BLIT_SHADER.into()),
            });

            let ui_blit_bind_group_layout =
                device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("ui_blit_bgl"),
                    entries: &[
                        wgpu::BindGroupLayoutEntry {
                            binding: 0,
                            visibility: wgpu::ShaderStages::FRAGMENT,
                            ty: wgpu::BindingType::Texture {
                                sample_type: wgpu::TextureSampleType::Float { filterable: true },
                                view_dimension: wgpu::TextureViewDimension::D2,
                                multisampled: false,
                            },
                            count: None,
                        },
                        wgpu::BindGroupLayoutEntry {
                            binding: 1,
                            visibility: wgpu::ShaderStages::FRAGMENT,
                            ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                            count: None,
                        },
                    ],
                });

            let ui_blit_pipeline_layout =
                device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("ui_blit_pipeline_layout"),
                    bind_group_layouts: &[&ui_blit_bind_group_layout],
                    push_constant_ranges: &[],
                });

            let ui_blit_pipeline =
                device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: Some("ui_blit_pipeline"),
                    layout: Some(&ui_blit_pipeline_layout),
                    vertex: wgpu::VertexState {
                        module: &ui_blit_shader,
                        entry_point: Some("vs_main"),
                        buffers: &[],
                        compilation_options: Default::default(),
                    },
                    fragment: Some(wgpu::FragmentState {
                        module: &ui_blit_shader,
                        entry_point: Some("fs_main"),
                        targets: &[Some(wgpu::ColorTargetState {
                            format,
                            blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                            write_mask: wgpu::ColorWrites::ALL,
                        })],
                        compilation_options: Default::default(),
                    }),
                    primitive: wgpu::PrimitiveState {
                        topology: wgpu::PrimitiveTopology::TriangleList,
                        ..Default::default()
                    },
                    depth_stencil: None,
                    multisample: wgpu::MultisampleState::default(),
                    multiview: None,
                    cache: None,
                });

            let ui_blit_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
                label: Some("ui_blit_sampler"),
                mag_filter: wgpu::FilterMode::Nearest,
                min_filter: wgpu::FilterMode::Nearest,
                ..Default::default()
            });

            state.window = Some(window);
            state.surface =
                Some(unsafe { std::mem::transmute::<_, wgpu::Surface<'static>>(surface) });
            state.device = Some(device);
            state.queue = Some(queue);
            state.config = Some(config);
            state.render_pipeline = Some(render_pipeline);
            state.uniform_buffer = Some(uniform_buffer);
            state.bind_group = Some(bind_group);
            state.depth_view = Some(depth_view);
            state.ui_blit_pipeline = Some(ui_blit_pipeline);
            state.ui_blit_bind_group_layout = Some(ui_blit_bind_group_layout);
            state.ui_blit_sampler = Some(ui_blit_sampler);
        });
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _id: winit::window::WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                with_state(|s| s.running = false);
                event_loop.exit();
            }

            WindowEvent::Resized(size) => {
                with_state(|state| {
                    state.width = size.width.max(1);
                    state.height = size.height.max(1);
                    state.aspect = state.width as f64 / state.height as f64;
                    if let (Some(ref surface), Some(ref device), Some(ref mut config)) =
                        (&state.surface, &state.device, &mut state.config)
                    {
                        config.width = state.width;
                        config.height = state.height;
                        surface.configure(device, config);
                        state.depth_view = Some(EngineState::create_depth_texture(
                            device,
                            state.width,
                            state.height,
                        ));
                    }
                });
            }

            WindowEvent::KeyboardInput { event, .. } => {
                if let PhysicalKey::Code(code) = event.physical_key {
                    if let Some(key_id) = keycode_to_int(code) {
                        with_state(|state| match event.state {
                            ElementState::Pressed => {
                                if !state.keys_down.contains(&key_id) {
                                    state.keys_pressed.insert(key_id);
                                }
                                state.keys_down.insert(key_id);
                            }
                            ElementState::Released => {
                                state.keys_down.remove(&key_id);
                                state.keys_released.insert(key_id);
                            }
                        });
                    }
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                with_state(|state| {
                    state.mouse_x = position.x;
                    state.mouse_y = position.y;
                });
                // Forward to UI overlay
                if with_state(|s| s.ui_overlay_enabled) {
                    ui_bridge::mouse_move(position.x as i64, position.y as i64);
                }
            }

            WindowEvent::MouseInput { state: btn_state, button, .. } => {
                let ui_captured = false;

                // Forward to UI overlay
                if with_state(|s| s.ui_overlay_enabled) {
                    let (mx, my) = with_state(|s| (s.mouse_x as i64, s.mouse_y as i64));
                    match btn_state {
                        ElementState::Pressed => ui_bridge::mouse_down(mx, my),
                        ElementState::Released => ui_bridge::mouse_up(mx, my),
                    }
                }

                // Only update engine input if UI didn't capture the click
                if !ui_captured {
                    if let Some(btn_id) = mouse_button_to_int(button) {
                        with_state(|state| match btn_state {
                            ElementState::Pressed => {
                                if !state.mouse_buttons_down.contains(&btn_id) {
                                    state.mouse_buttons_pressed.insert(btn_id);
                                }
                                state.mouse_buttons_down.insert(btn_id);
                            }
                            ElementState::Released => {
                                state.mouse_buttons_down.remove(&btn_id);
                            }
                        });
                    }
                }
            }

            WindowEvent::RedrawRequested => {
                with_state(|state| {
                    // -- Timing --
                    let now = Instant::now();
                    state.delta_time = now.duration_since(state.last_frame_time).as_secs_f64();
                    state.elapsed_time = now.duration_since(state.start_time).as_secs_f64();
                    state.last_frame_time = now;
                    state.frame_count += 1;

                    // -- Mouse delta --
                    state.mouse_dx = state.mouse_x - state.prev_mouse_x;
                    state.mouse_dy = state.mouse_y - state.prev_mouse_y;
                    state.prev_mouse_x = state.mouse_x;
                    state.prev_mouse_y = state.mouse_y;
                });

                // -- Call user update function --
                let update_fn = with_state(|state| state.update_fn);
                if let Some(f) = update_fn {
                    f();
                }

                // -- Render UI overlay framebuffer (outside main borrow) --
                let ui_fb_data: Option<(Vec<u32>, u32, u32)> = {
                    let enabled = with_state(|s| s.ui_overlay_enabled);
                    if enabled {
                        Self::get_ui_framebuffer()
                    } else {
                        None
                    }
                };

                with_state(|state| {
                    let (
                        Some(ref surface),
                        Some(ref device),
                        Some(ref queue),
                        Some(ref pipeline),
                        Some(ref uniform_buf),
                        Some(ref bind_group),
                        Some(ref depth_view),
                    ) = (
                        &state.surface,
                        &state.device,
                        &state.queue,
                        &state.render_pipeline,
                        &state.uniform_buffer,
                        &state.bind_group,
                        &state.depth_view,
                    ) else {
                        return;
                    };

                    // -- Compute view-projection matrix --
                    let eye = [
                        state.camera_pos[0] as f32,
                        state.camera_pos[1] as f32,
                        state.camera_pos[2] as f32,
                    ];
                    let target = [
                        state.camera_target[0] as f32,
                        state.camera_target[1] as f32,
                        state.camera_target[2] as f32,
                    ];
                    let up = [
                        state.camera_up[0] as f32,
                        state.camera_up[1] as f32,
                        state.camera_up[2] as f32,
                    ];
                    let view = mat4_look_at(eye, target, up);
                    let proj = mat4_perspective(
                        state.fov as f32,
                        state.aspect as f32,
                        state.near_plane as f32,
                        state.far_plane as f32,
                    );
                    let view_proj = mat4_multiply(&proj, &view);

                    // Upload uniform
                    queue.write_buffer(
                        uniform_buf,
                        0,
                        bytemuck::cast_slice(&view_proj),
                    );

                    // -- Create vertex buffer from accumulated vertices --
                    let vertex_buffer = if !state.vertices.is_empty() {
                        Some(device.create_buffer_init(
                            &wgpu::util::BufferInitDescriptor {
                                label: Some("vertex_buffer"),
                                contents: bytemuck::cast_slice(&state.vertices),
                                usage: wgpu::BufferUsages::VERTEX,
                            },
                        ))
                    } else {
                        None
                    };
                    let vertex_count = state.vertices.len() as u32;

                    // -- Render --
                    if let Ok(frame) = surface.get_current_texture() {
                        let view = frame.texture.create_view(&Default::default());
                        let mut encoder =
                            device.create_command_encoder(&Default::default());

                        // -- 3D render pass --
                        {
                            let mut rpass =
                                encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                                    label: Some("engine_render_pass"),
                                    color_attachments: &[Some(
                                        wgpu::RenderPassColorAttachment {
                                            view: &view,
                                            resolve_target: None,
                                            ops: wgpu::Operations {
                                                load: wgpu::LoadOp::Clear(wgpu::Color {
                                                    r: state.clear_color[0],
                                                    g: state.clear_color[1],
                                                    b: state.clear_color[2],
                                                    a: state.clear_color[3],
                                                }),
                                                store: wgpu::StoreOp::Store,
                                            },
                                        },
                                    )],
                                    depth_stencil_attachment: Some(
                                        wgpu::RenderPassDepthStencilAttachment {
                                            view: depth_view,
                                            depth_ops: Some(wgpu::Operations {
                                                load: wgpu::LoadOp::Clear(1.0),
                                                store: wgpu::StoreOp::Store,
                                            }),
                                            stencil_ops: None,
                                        },
                                    ),
                                    occlusion_query_set: None,
                                    timestamp_writes: None,
                                });

                            if let Some(ref vb) = vertex_buffer {
                                rpass.set_pipeline(pipeline);
                                rpass.set_bind_group(0, bind_group, &[]);
                                rpass.set_vertex_buffer(0, vb.slice(..));
                                rpass.draw(0..vertex_count, 0..1);
                            }
                        }

                        // -- UI overlay blit pass --
                        if let Some((ref fb_pixels, fb_w, fb_h)) = ui_fb_data {
                            // Recreate GPU texture if size changed
                            if state.ui_fb_width != fb_w || state.ui_fb_height != fb_h {
                                state.ui_fb_width = fb_w;
                                state.ui_fb_height = fb_h;

                                let tex = device.create_texture(&wgpu::TextureDescriptor {
                                    label: Some("ui_overlay_fb"),
                                    size: wgpu::Extent3d {
                                        width: fb_w,
                                        height: fb_h,
                                        depth_or_array_layers: 1,
                                    },
                                    mip_level_count: 1,
                                    sample_count: 1,
                                    dimension: wgpu::TextureDimension::D2,
                                    format: wgpu::TextureFormat::Rgba8UnormSrgb,
                                    usage: wgpu::TextureUsages::TEXTURE_BINDING
                                        | wgpu::TextureUsages::COPY_DST,
                                    view_formats: &[],
                                });

                                let tex_view = tex.create_view(&Default::default());
                                if let (Some(ref layout), Some(ref sampler)) =
                                    (&state.ui_blit_bind_group_layout, &state.ui_blit_sampler)
                                {
                                    state.ui_bind_group = Some(device.create_bind_group(
                                        &wgpu::BindGroupDescriptor {
                                            label: Some("ui_overlay_bg"),
                                            layout,
                                            entries: &[
                                                wgpu::BindGroupEntry {
                                                    binding: 0,
                                                    resource: wgpu::BindingResource::TextureView(
                                                        &tex_view,
                                                    ),
                                                },
                                                wgpu::BindGroupEntry {
                                                    binding: 1,
                                                    resource: wgpu::BindingResource::Sampler(
                                                        sampler,
                                                    ),
                                                },
                                            ],
                                        },
                                    ));
                                }
                                state.ui_fb_texture = Some(tex);
                            }

                            // Upload framebuffer pixels (ARGB u32 -> RGBA u8)
                            if let Some(ref ui_tex) = state.ui_fb_texture {
                                let rgba_data: Vec<u8> = fb_pixels
                                    .iter()
                                    .flat_map(|&pixel| {
                                        let r = ((pixel >> 16) & 0xFF) as u8;
                                        let g = ((pixel >> 8) & 0xFF) as u8;
                                        let b = (pixel & 0xFF) as u8;
                                        let a = ((pixel >> 24) & 0xFF) as u8;
                                        [r, g, b, a]
                                    })
                                    .collect();
                                queue.write_texture(
                                    wgpu::ImageCopyTexture {
                                        texture: ui_tex,
                                        mip_level: 0,
                                        origin: wgpu::Origin3d::ZERO,
                                        aspect: wgpu::TextureAspect::All,
                                    },
                                    &rgba_data,
                                    wgpu::ImageDataLayout {
                                        offset: 0,
                                        bytes_per_row: Some(4 * fb_w),
                                        rows_per_image: Some(fb_h),
                                    },
                                    wgpu::Extent3d {
                                        width: fb_w,
                                        height: fb_h,
                                        depth_or_array_layers: 1,
                                    },
                                );
                            }

                            // Blit UI onto 3D scene
                            if let (Some(ref blit_pipeline), Some(ref bg)) =
                                (&state.ui_blit_pipeline, &state.ui_bind_group)
                            {
                                let mut rpass = encoder.begin_render_pass(
                                    &wgpu::RenderPassDescriptor {
                                        label: Some("ui_overlay_pass"),
                                        color_attachments: &[Some(
                                            wgpu::RenderPassColorAttachment {
                                                view: &view,
                                                resolve_target: None,
                                                ops: wgpu::Operations {
                                                    load: wgpu::LoadOp::Load, // preserve 3D
                                                    store: wgpu::StoreOp::Store,
                                                },
                                            },
                                        )],
                                        depth_stencil_attachment: None,
                                        occlusion_query_set: None,
                                        timestamp_writes: None,
                                    },
                                );
                                rpass.set_pipeline(blit_pipeline);
                                rpass.set_bind_group(0, bg, &[]);
                                rpass.draw(0..6, 0..1);
                            }
                        }

                        queue.submit(std::iter::once(encoder.finish()));
                        frame.present();
                    }

                    // -- Clear vertices for next frame --
                    state.vertices.clear();

                    // -- Clear per-frame input state --
                    state.keys_pressed.clear();
                    state.keys_released.clear();
                    state.mouse_buttons_pressed.clear();
                });
            }

            _ => {}
        }
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        STATE.with(|cell| {
            if let Some(ref state) = *cell.borrow() {
                if let Some(ref win) = state.window {
                    win.request_redraw();
                }
            }
        });
    }
}

// =======================================================================
// FFI: Engine lifecycle
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_window_create(
    title: *const c_char,
    width: i64,
    height: i64,
) -> i64 {
    let state = EngineState::new(
        cstr_to_string(title),
        width.max(1) as u32,
        height.max(1) as u32,
    );
    STATE.with(|cell| {
        *cell.borrow_mut() = Some(state);
    });
    1
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_window_run(handle: i64) {
    let _ = handle;
    let mut app = EngineApp;
    let event_loop = EventLoop::new().expect("create event loop");
    let _ = event_loop.run_app(&mut app);
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_window_quit(handle: i64) {
    let _ = handle;
    with_state(|s| s.running = false);
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_window_destroy(handle: i64) {
    let _ = handle;
    STATE.with(|cell| {
        *cell.borrow_mut() = None;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_window_is_running(handle: i64) -> i64 {
    let _ = handle;
    STATE.with(|cell| {
        cell.borrow()
            .as_ref()
            .map(|s| if s.running { 1i64 } else { 0i64 })
            .unwrap_or(0)
    })
}

// =======================================================================
// FFI: Configuration
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_clear_color(r: f64, g: f64, b: f64, a: f64) {
    try_with_state(|s| {
        s.clear_color = [r, g, b, a];
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_update_fn(fn_ptr: i64) {
    try_with_state(|s| {
        if fn_ptr == 0 {
            s.update_fn = None;
        } else {
            s.update_fn = Some(std::mem::transmute::<i64, extern "C" fn()>(fn_ptr));
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_window_width() -> i64 {
    try_with_state(|s| s.width as i64).unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_window_height() -> i64 {
    try_with_state(|s| s.height as i64).unwrap_or(0)
}

// =======================================================================
// FFI: Input — Keyboard
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_key_down(key: i64) -> i64 {
    try_with_state(|s| if s.keys_down.contains(&(key as u32)) { 1i64 } else { 0 })
        .unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_key_pressed(key: i64) -> i64 {
    try_with_state(|s| if s.keys_pressed.contains(&(key as u32)) { 1i64 } else { 0 })
        .unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_key_released(key: i64) -> i64 {
    try_with_state(|s| if s.keys_released.contains(&(key as u32)) { 1i64 } else { 0 })
        .unwrap_or(0)
}

// =======================================================================
// FFI: Input — Mouse
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_mouse_x() -> f64 {
    try_with_state(|s| s.mouse_x).unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_mouse_y() -> f64 {
    try_with_state(|s| s.mouse_y).unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_mouse_delta_x() -> f64 {
    try_with_state(|s| s.mouse_dx).unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_mouse_delta_y() -> f64 {
    try_with_state(|s| s.mouse_dy).unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_mouse_button_down(button: i64) -> i64 {
    try_with_state(|s| {
        if s.mouse_buttons_down.contains(&(button as u32)) { 1i64 } else { 0 }
    })
    .unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_mouse_button_pressed(button: i64) -> i64 {
    try_with_state(|s| {
        if s.mouse_buttons_pressed.contains(&(button as u32)) { 1i64 } else { 0 }
    })
    .unwrap_or(0)
}

// =======================================================================
// FFI: Timing
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_delta_time() -> f64 {
    try_with_state(|s| s.delta_time).unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_elapsed_time() -> f64 {
    try_with_state(|s| s.elapsed_time).unwrap_or(0.0)
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_frame_count() -> i64 {
    try_with_state(|s| s.frame_count as i64).unwrap_or(0)
}

// =======================================================================
// FFI: Camera
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_camera_pos(x: f64, y: f64, z: f64) {
    try_with_state(|s| {
        s.camera_pos = [x, y, z];
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_camera_target(x: f64, y: f64, z: f64) {
    try_with_state(|s| {
        s.camera_target = [x, y, z];
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_camera_up(x: f64, y: f64, z: f64) {
    try_with_state(|s| {
        s.camera_up = [x, y, z];
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_perspective(fov: f64, aspect: f64, near: f64, far: f64) {
    try_with_state(|s| {
        s.fov = fov;
        if aspect > 0.0 {
            s.aspect = aspect;
        }
        s.near_plane = near;
        s.far_plane = far;
    });
}

// =======================================================================
// FFI: Drawing
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_push_vertex(
    x: f64, y: f64, z: f64,
    r: f64, g: f64, b: f64,
) {
    try_with_state(|s| {
        s.vertices.push(Vertex {
            position: [x as f32, y as f32, z as f32],
            color: [r as f32, g as f32, b as f32],
        });
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_draw_triangles() {
    // Vertices are drawn during RedrawRequested; this is a no-op marker.
    // The vertices are already accumulated via push_vertex and will be
    // rendered on the next frame.
}

// =======================================================================
// FFI: UI Overlay
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_enable_ui_overlay() {
    try_with_state(|s| {
        s.ui_overlay_enabled = true;
    });
}


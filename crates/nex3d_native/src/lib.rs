//! nex3d native engine — Game window, input, timing, camera, and 3D rendering.
//! Compiled as a cdylib (.dll / .so) and loaded dynamically by the Nex runtime.

use std::cell::RefCell;
use std::collections::HashSet;
use std::io::Cursor;
use std::os::raw::c_char;
use std::sync::Arc;
use std::time::Instant;

use gilrs::{Gilrs, Button as GilrsButton, Axis as GilrsAxis};
use rodio::{Decoder, OutputStream, OutputStreamHandle, Sink, Source};

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
// Vertex format: position (xyz) + normal (xyz) + uv (xy) + color (rgb) = 44 bytes
// -----------------------------------------------------------------------

#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct Vertex {
    position: [f32; 3],
    normal: [f32; 3],
    uv: [f32; 2],
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
                wgpu::VertexAttribute {
                    offset: 24,
                    shader_location: 2,
                    format: wgpu::VertexFormat::Float32x2,
                },
                wgpu::VertexAttribute {
                    offset: 32,
                    shader_location: 3,
                    format: wgpu::VertexFormat::Float32x3,
                },
            ],
        }
    }
}

// -----------------------------------------------------------------------
// Skinned vertex (for skeletal animation)
// -----------------------------------------------------------------------

const MAX_BONES: usize = 128;

#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct SkinnedVertex {
    position: [f32; 3],
    normal: [f32; 3],
    uv: [f32; 2],
    color: [f32; 3],
    joint_indices: [u32; 4],
    bone_weights: [f32; 4],
}

impl SkinnedVertex {
    fn layout() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<SkinnedVertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[
                wgpu::VertexAttribute { offset: 0,  shader_location: 0, format: wgpu::VertexFormat::Float32x3 },
                wgpu::VertexAttribute { offset: 12, shader_location: 1, format: wgpu::VertexFormat::Float32x3 },
                wgpu::VertexAttribute { offset: 24, shader_location: 2, format: wgpu::VertexFormat::Float32x2 },
                wgpu::VertexAttribute { offset: 32, shader_location: 3, format: wgpu::VertexFormat::Float32x3 },
                wgpu::VertexAttribute { offset: 44, shader_location: 4, format: wgpu::VertexFormat::Uint32x4 },
                wgpu::VertexAttribute { offset: 60, shader_location: 5, format: wgpu::VertexFormat::Float32x4 },
            ],
        }
    }
}

// -----------------------------------------------------------------------
// Skeletal animation data structures
// -----------------------------------------------------------------------

#[derive(Clone)]
struct Joint {
    parent: i32,                // -1 for root
    inv_bind: [[f32; 4]; 4],   // inverse bind matrix (column-major)
    local_translation: [f32; 3],
    local_rotation: [f32; 4],   // quaternion (x,y,z,w)
    local_scale: [f32; 3],
}

#[derive(Clone)]
struct AnimKeyframe<T: Clone> {
    time: f32,
    value: T,
}

#[derive(Clone)]
struct AnimChannel {
    joint_index: usize,
    translations: Vec<AnimKeyframe<[f32; 3]>>,
    rotations: Vec<AnimKeyframe<[f32; 4]>>,
    scales: Vec<AnimKeyframe<[f32; 3]>>,
}

#[derive(Clone)]
struct AnimClip {
    #[allow(dead_code)]
    name: String,
    duration: f32,
    channels: Vec<AnimChannel>,
}

struct AnimState {
    clip_index: usize,
    time: f32,
    speed: f32,
    looping: bool,
    playing: bool,
}

struct AnimatedModel {
    skinned_vertices: Vec<SkinnedVertex>,
    joints: Vec<Joint>,
    animations: Vec<AnimClip>,
    anim_state: AnimState,
    bone_buffer: wgpu::Buffer,
    bone_bind_group: wgpu::BindGroup,
}

// -----------------------------------------------------------------------
// Math helpers for skeletal animation
// -----------------------------------------------------------------------

fn quat_slerp(a: [f32; 4], b: [f32; 4], t: f32) -> [f32; 4] {
    let mut dot = a[0]*b[0] + a[1]*b[1] + a[2]*b[2] + a[3]*b[3];
    let mut b2 = b;
    if dot < 0.0 {
        dot = -dot;
        b2 = [-b[0], -b[1], -b[2], -b[3]];
    }
    if dot > 0.9995 {
        // Near-parallel: normalized lerp
        let r = [
            a[0] + (b2[0] - a[0]) * t,
            a[1] + (b2[1] - a[1]) * t,
            a[2] + (b2[2] - a[2]) * t,
            a[3] + (b2[3] - a[3]) * t,
        ];
        let len = (r[0]*r[0] + r[1]*r[1] + r[2]*r[2] + r[3]*r[3]).sqrt();
        return [r[0]/len, r[1]/len, r[2]/len, r[3]/len];
    }
    let theta = dot.acos();
    let sin_theta = theta.sin();
    let wa = ((1.0 - t) * theta).sin() / sin_theta;
    let wb = (t * theta).sin() / sin_theta;
    [
        a[0]*wa + b2[0]*wb,
        a[1]*wa + b2[1]*wb,
        a[2]*wa + b2[2]*wb,
        a[3]*wa + b2[3]*wb,
    ]
}

fn quat_to_mat4(q: [f32; 4]) -> [[f32; 4]; 4] {
    let (x, y, z, w) = (q[0], q[1], q[2], q[3]);
    let x2 = x+x; let y2 = y+y; let z2 = z+z;
    let xx = x*x2; let xy = x*y2; let xz = x*z2;
    let yy = y*y2; let yz = y*z2; let zz = z*z2;
    let wx = w*x2; let wy = w*y2; let wz = w*z2;
    [
        [1.0-yy-zz, xy+wz,     xz-wy,     0.0],
        [xy-wz,     1.0-xx-zz, yz+wx,     0.0],
        [xz+wy,     yz-wx,     1.0-xx-yy, 0.0],
        [0.0,       0.0,       0.0,       1.0],
    ]
}

fn mat4x4_identity() -> [[f32; 4]; 4] {
    [
        [1.0, 0.0, 0.0, 0.0],
        [0.0, 1.0, 0.0, 0.0],
        [0.0, 0.0, 1.0, 0.0],
        [0.0, 0.0, 0.0, 1.0],
    ]
}

fn mat4_mul(a: &[[f32; 4]; 4], b: &[[f32; 4]; 4]) -> [[f32; 4]; 4] {
    let mut r = [[0.0f32; 4]; 4];
    for i in 0..4 {
        for j in 0..4 {
            r[i][j] = a[i][0]*b[0][j] + a[i][1]*b[1][j] + a[i][2]*b[2][j] + a[i][3]*b[3][j];
        }
    }
    r
}

fn compose_trs(t: [f32; 3], r: [f32; 4], s: [f32; 3]) -> [[f32; 4]; 4] {
    let mut m = quat_to_mat4(r);
    // Apply scale
    m[0][0] *= s[0]; m[0][1] *= s[0]; m[0][2] *= s[0];
    m[1][0] *= s[1]; m[1][1] *= s[1]; m[1][2] *= s[1];
    m[2][0] *= s[2]; m[2][1] *= s[2]; m[2][2] *= s[2];
    // Apply translation
    m[3][0] = t[0]; m[3][1] = t[1]; m[3][2] = t[2];
    m
}

fn lerp3(a: [f32; 3], b: [f32; 3], t: f32) -> [f32; 3] {
    [a[0]+(b[0]-a[0])*t, a[1]+(b[1]-a[1])*t, a[2]+(b[2]-a[2])*t]
}

fn sample_keyframes_vec3(keys: &[AnimKeyframe<[f32; 3]>], time: f32) -> [f32; 3] {
    if keys.is_empty() { return [0.0, 0.0, 0.0]; }
    if keys.len() == 1 || time <= keys[0].time { return keys[0].value; }
    if time >= keys.last().unwrap().time { return keys.last().unwrap().value; }
    for i in 0..keys.len()-1 {
        if time >= keys[i].time && time < keys[i+1].time {
            let t = (time - keys[i].time) / (keys[i+1].time - keys[i].time);
            return lerp3(keys[i].value, keys[i+1].value, t);
        }
    }
    keys.last().unwrap().value
}

fn sample_keyframes_quat(keys: &[AnimKeyframe<[f32; 4]>], time: f32) -> [f32; 4] {
    if keys.is_empty() { return [0.0, 0.0, 0.0, 1.0]; }
    if keys.len() == 1 || time <= keys[0].time { return keys[0].value; }
    if time >= keys.last().unwrap().time { return keys.last().unwrap().value; }
    for i in 0..keys.len()-1 {
        if time >= keys[i].time && time < keys[i+1].time {
            let t = (time - keys[i].time) / (keys[i+1].time - keys[i].time);
            return quat_slerp(keys[i].value, keys[i+1].value, t);
        }
    }
    keys.last().unwrap().value
}

/// Compute final bone matrices for the current animation frame.
fn compute_bone_matrices(model: &AnimatedModel) -> Vec<[[f32; 4]; 4]> {
    let num_joints = model.joints.len();
    let mut local_transforms: Vec<[[f32; 4]; 4]> = Vec::with_capacity(num_joints);

    // Start with bind pose
    for joint in &model.joints {
        local_transforms.push(compose_trs(
            joint.local_translation,
            joint.local_rotation,
            joint.local_scale,
        ));
    }

    // Override with animation data
    let state = &model.anim_state;
    if state.clip_index < model.animations.len() {
        let clip = &model.animations[state.clip_index];
        for channel in &clip.channels {
            let ji = channel.joint_index;
            if ji >= num_joints { continue; }

            let t = if !channel.translations.is_empty() {
                sample_keyframes_vec3(&channel.translations, state.time)
            } else {
                model.joints[ji].local_translation
            };
            let r = if !channel.rotations.is_empty() {
                sample_keyframes_quat(&channel.rotations, state.time)
            } else {
                model.joints[ji].local_rotation
            };
            let s = if !channel.scales.is_empty() {
                sample_keyframes_vec3(&channel.scales, state.time)
            } else {
                model.joints[ji].local_scale
            };
            local_transforms[ji] = compose_trs(t, r, s);
        }
    }

    // Compose global transforms (parent chain)
    let mut global_transforms = vec![mat4x4_identity(); num_joints];
    for i in 0..num_joints {
        let parent = model.joints[i].parent;
        if parent >= 0 && (parent as usize) < num_joints {
            global_transforms[i] = mat4_mul(&global_transforms[parent as usize], &local_transforms[i]);
        } else {
            global_transforms[i] = local_transforms[i];
        }
    }

    // Apply inverse bind matrices → final bone matrices
    let mut final_matrices = vec![mat4x4_identity(); MAX_BONES];
    for i in 0..num_joints.min(MAX_BONES) {
        final_matrices[i] = mat4_mul(&global_transforms[i], &model.joints[i].inv_bind);
    }

    final_matrices
}

// -----------------------------------------------------------------------
// Light data (CPU side)
// -----------------------------------------------------------------------

const MAX_LIGHTS: usize = 8;

#[derive(Copy, Clone)]
struct LightData {
    light_type: u32,        // 0=off, 1=directional, 2=point, 3=spot
    enabled: bool,
    position: [f32; 3],
    direction: [f32; 3],
    color: [f32; 3],
    intensity: f32,
    range: f32,
    inner_cone_cos: f32,
    outer_cone_cos: f32,
}

impl LightData {
    fn new() -> Self {
        Self {
            light_type: 0,
            enabled: false,
            position: [0.0, 0.0, 0.0],
            direction: [0.0, -1.0, 0.0],
            color: [1.0, 1.0, 1.0],
            intensity: 1.0,
            range: 10.0,
            inner_cone_cos: 0.9063, // ~25 degrees
            outer_cone_cos: 0.8192, // ~35 degrees
        }
    }
}

// GPU-side uniform layout for a single light (64 bytes, WGSL-aligned)
#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct GpuLight {
    position_and_type: [f32; 4],    // xyz=position, w=light_type
    direction_and_intensity: [f32; 4], // xyz=direction, w=intensity
    color_and_enabled: [f32; 4],    // rgb=color, a=enabled (1.0/0.0)
    params: [f32; 4],              // x=range, y=inner_cone_cos, z=outer_cone_cos, w=0
}

// Full uniform buffer layout (608 bytes)
#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct Uniforms {
    view_proj: [f32; 16],          // 64 bytes
    camera_pos: [f32; 4],          // 16 bytes (w=unused)
    ambient_and_count: [f32; 4],   // 16 bytes (rgb=ambient, w=num_lights)
    lights: [GpuLight; MAX_LIGHTS], // 512 bytes
}

// -----------------------------------------------------------------------
// Embedded WGSL shader
// -----------------------------------------------------------------------

const SHADER_SRC: &str = r#"
struct Light {
    position_and_type: vec4<f32>,
    direction_and_intensity: vec4<f32>,
    color_and_enabled: vec4<f32>,
    params: vec4<f32>,
};

struct Uniforms {
    view_proj: mat4x4<f32>,
    camera_pos: vec4<f32>,
    ambient_and_count: vec4<f32>,
    lights: array<Light, 8>,
};
@group(0) @binding(0) var<uniform> uniforms: Uniforms;

// Texture bind group (1x1 white default when no texture is bound)
@group(1) @binding(0) var diffuse_texture: texture_2d<f32>;
@group(1) @binding(1) var diffuse_sampler: sampler;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) uv: vec2<f32>,
    @location(3) color: vec3<f32>,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec3<f32>,
    @location(1) world_pos: vec3<f32>,
    @location(2) world_normal: vec3<f32>,
    @location(3) uv: vec2<f32>,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = uniforms.view_proj * vec4<f32>(in.position, 1.0);
    out.color = in.color;
    out.world_pos = in.position;
    out.world_normal = in.normal;
    out.uv = in.uv;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    // Sample texture (1x1 white if no texture bound → just vertex color)
    let tex_color = textureSample(diffuse_texture, diffuse_sampler, in.uv);
    let base_color = in.color * tex_color.rgb;
    let base_alpha = tex_color.a;

    // Determine normal: use vertex normal if provided, else compute from derivatives
    var normal: vec3<f32>;
    let nlen = dot(in.world_normal, in.world_normal);
    if (nlen < 0.001) {
        let dx = dpdx(in.world_pos);
        let dy = dpdy(in.world_pos);
        normal = normalize(cross(dx, dy));
    } else {
        normal = normalize(in.world_normal);
    }

    let num_lights = i32(uniforms.ambient_and_count.w);
    let ambient_color = uniforms.ambient_and_count.xyz;
    let camera_pos = uniforms.camera_pos.xyz;
    let view_dir = normalize(camera_pos - in.world_pos);

    var total_light = ambient_color;

    // If no lights are active, use legacy fixed lighting
    if (num_lights == 0) {
        let light_dir = normalize(vec3<f32>(0.4, 0.8, 0.6));
        let ndotl = max(dot(normal, light_dir), 0.0);
        let legacy_ambient = 0.25;
        let legacy_diffuse = 0.75 * ndotl;
        return vec4<f32>(base_color * (legacy_ambient + legacy_diffuse), base_alpha);
    }

    let specular_power = 32.0;

    for (var i = 0; i < num_lights; i = i + 1) {
        let light = uniforms.lights[i];
        let light_type = i32(light.position_and_type.w);
        let enabled = light.color_and_enabled.w;

        if (light_type == 0 || enabled < 0.5) {
            continue;
        }

        let light_color = light.color_and_enabled.xyz;
        let intensity = light.direction_and_intensity.w;
        let light_pos = light.position_and_type.xyz;
        let light_dir_raw = light.direction_and_intensity.xyz;
        let light_range = light.params.x;
        let inner_cos = light.params.y;
        let outer_cos = light.params.z;

        var light_dir: vec3<f32>;
        var attenuation = 1.0;

        if (light_type == 1) {
            light_dir = normalize(-light_dir_raw);
        } else {
            let to_light = light_pos - in.world_pos;
            let dist = length(to_light);
            light_dir = to_light / max(dist, 0.0001);
            let ratio = dist / max(light_range, 0.0001);
            attenuation = max(1.0 - ratio * ratio, 0.0);
            attenuation = attenuation * attenuation;

            if (light_type == 3) {
                let spot_dir = normalize(light_dir_raw);
                let cos_angle = dot(-light_dir, spot_dir);
                let spot_factor = clamp(
                    (cos_angle - outer_cos) / max(inner_cos - outer_cos, 0.0001),
                    0.0, 1.0
                );
                attenuation = attenuation * spot_factor;
            }
        }

        let ndotl = max(dot(normal, light_dir), 0.0);
        let diffuse = light_color * intensity * ndotl * attenuation;

        let half_vec = normalize(light_dir + view_dir);
        let ndoth = max(dot(normal, half_vec), 0.0);
        let spec = pow(ndoth, specular_power) * attenuation * intensity;
        let specular = light_color * spec * step(0.001, ndotl);

        total_light = total_light + diffuse + specular;
    }

    let lit = base_color * total_light;
    return vec4<f32>(lit, base_alpha);
}
"#;

// -----------------------------------------------------------------------
// Skinned WGSL shader (extends main shader with bone matrices)
// -----------------------------------------------------------------------

const SKINNED_SHADER_SRC: &str = r#"
struct Light {
    position_and_type: vec4<f32>,
    direction_and_intensity: vec4<f32>,
    color_and_enabled: vec4<f32>,
    params: vec4<f32>,
};

struct Uniforms {
    view_proj: mat4x4<f32>,
    camera_pos: vec4<f32>,
    ambient_and_count: vec4<f32>,
    lights: array<Light, 8>,
};
@group(0) @binding(0) var<uniform> uniforms: Uniforms;

@group(1) @binding(0) var diffuse_texture: texture_2d<f32>;
@group(1) @binding(1) var diffuse_sampler: sampler;

@group(2) @binding(0) var<storage, read> bone_matrices: array<mat4x4<f32>, 128>;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) uv: vec2<f32>,
    @location(3) color: vec3<f32>,
    @location(4) joint_indices: vec4<u32>,
    @location(5) bone_weights: vec4<f32>,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec3<f32>,
    @location(1) world_pos: vec3<f32>,
    @location(2) world_normal: vec3<f32>,
    @location(3) uv: vec2<f32>,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var skinned_pos = vec3<f32>(0.0, 0.0, 0.0);
    var skinned_normal = vec3<f32>(0.0, 0.0, 0.0);

    for (var i = 0u; i < 4u; i = i + 1u) {
        let w = in.bone_weights[i];
        if (w > 0.0) {
            let bone = bone_matrices[in.joint_indices[i]];
            skinned_pos = skinned_pos + (bone * vec4<f32>(in.position, 1.0)).xyz * w;
            skinned_normal = skinned_normal + (bone * vec4<f32>(in.normal, 0.0)).xyz * w;
        }
    }

    var out: VertexOutput;
    out.clip_position = uniforms.view_proj * vec4<f32>(skinned_pos, 1.0);
    out.color = in.color;
    out.world_pos = skinned_pos;
    out.world_normal = skinned_normal;
    out.uv = in.uv;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let tex_color = textureSample(diffuse_texture, diffuse_sampler, in.uv);
    let base_color = in.color * tex_color.rgb;
    let base_alpha = tex_color.a;

    var normal: vec3<f32>;
    let nlen = dot(in.world_normal, in.world_normal);
    if (nlen < 0.001) {
        let dx = dpdx(in.world_pos);
        let dy = dpdy(in.world_pos);
        normal = normalize(cross(dx, dy));
    } else {
        normal = normalize(in.world_normal);
    }

    let num_lights = i32(uniforms.ambient_and_count.w);
    let ambient_color = uniforms.ambient_and_count.xyz;
    let camera_pos = uniforms.camera_pos.xyz;
    let view_dir = normalize(camera_pos - in.world_pos);

    var total_light = ambient_color;

    if (num_lights == 0) {
        let light_dir = normalize(vec3<f32>(0.4, 0.8, 0.6));
        let ndotl = max(dot(normal, light_dir), 0.0);
        let legacy_ambient = 0.25;
        let legacy_diffuse = 0.75 * ndotl;
        return vec4<f32>(base_color * (legacy_ambient + legacy_diffuse), base_alpha);
    }

    let specular_power = 32.0;

    for (var i = 0; i < num_lights; i = i + 1) {
        let light = uniforms.lights[i];
        let light_type = i32(light.position_and_type.w);
        let enabled = light.color_and_enabled.w;
        if (light_type == 0 || enabled < 0.5) { continue; }

        let light_color = light.color_and_enabled.xyz;
        let intensity = light.direction_and_intensity.w;
        let light_pos = light.position_and_type.xyz;
        let light_dir_raw = light.direction_and_intensity.xyz;
        let light_range = light.params.x;
        let inner_cos = light.params.y;
        let outer_cos = light.params.z;

        var light_dir: vec3<f32>;
        var attenuation = 1.0;

        if (light_type == 1) {
            light_dir = normalize(-light_dir_raw);
        } else {
            let to_light = light_pos - in.world_pos;
            let dist = length(to_light);
            light_dir = to_light / max(dist, 0.0001);
            let ratio = dist / max(light_range, 0.0001);
            attenuation = max(1.0 - ratio * ratio, 0.0);
            attenuation = attenuation * attenuation;
            if (light_type == 3) {
                let spot_dir = normalize(light_dir_raw);
                let cos_angle = dot(-light_dir, spot_dir);
                let spot_factor = clamp(
                    (cos_angle - outer_cos) / max(inner_cos - outer_cos, 0.0001),
                    0.0, 1.0
                );
                attenuation = attenuation * spot_factor;
            }
        }

        let ndotl = max(dot(normal, light_dir), 0.0);
        let diffuse = light_color * intensity * ndotl * attenuation;

        let half_vec = normalize(light_dir + view_dir);
        let ndoth = max(dot(normal, half_vec), 0.0);
        let spec = pow(ndoth, specular_power) * attenuation * intensity;
        let specular = light_color * spec * step(0.001, ndotl);

        total_light = total_light + diffuse + specular;
    }

    let lit = base_color * total_light;
    return vec4<f32>(lit, base_alpha);
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

// Loaded texture data
struct LoadedTexture {
    #[allow(dead_code)]
    texture: wgpu::Texture,
    bind_group: wgpu::BindGroup,
    width: u32,
    height: u32,
}

struct LoadedModel {
    vertices: Vec<Vertex>,
}

#[allow(dead_code)]
struct RenderTargetData {
    color_texture: wgpu::Texture,
    color_view: wgpu::TextureView,
    depth_view: wgpu::TextureView,
    bind_group: wgpu::BindGroup,  // for sampling as texture
    width: u32,
    height: u32,
}

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

    // Texture system
    texture_bind_group_layout: Option<wgpu::BindGroupLayout>,
    default_texture_bind_group: Option<wgpu::BindGroup>,
    textures: Vec<LoadedTexture>,
    active_texture: Option<usize>,

    // Clear color
    clear_color: [f64; 4],

    // User callback
    update_fn: Option<extern "C" fn()>,

    // Vertices accumulated this frame
    vertices: Vec<Vertex>,

    // Lighting
    lights: [LightData; MAX_LIGHTS],
    ambient_color: [f32; 3],

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
    mouse_scroll_delta: f64,

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

    // Audio (rodio)
    audio_stream: Option<(OutputStream, OutputStreamHandle)>,
    audio_buffers: Vec<Arc<Vec<u8>>>,   // loaded audio file bytes
    audio_sinks: Vec<Option<Sink>>,     // playback sinks (1 per loaded sound)

    // Gamepad (gilrs)
    gilrs: Option<Gilrs>,

    // Render targets
    render_targets: Vec<RenderTargetData>,
    active_render_target: Option<usize>,

    // Loaded OBJ models
    models: Vec<LoadedModel>,

    // Skeletal animation
    skinned_pipeline: Option<wgpu::RenderPipeline>,
    bone_bind_group_layout: Option<wgpu::BindGroupLayout>,
    anim_models: Vec<AnimatedModel>,
    skinned_draw_calls: Vec<usize>,  // model indices to draw this frame
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
            texture_bind_group_layout: None,
            default_texture_bind_group: None,
            textures: Vec::new(),
            active_texture: None,
            clear_color: [0.1, 0.1, 0.15, 1.0],
            update_fn: None,
            vertices: Vec::with_capacity(4096),
            lights: [LightData::new(); MAX_LIGHTS],
            ambient_color: [0.1, 0.1, 0.1],
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
            mouse_scroll_delta: 0.0,
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

            audio_stream: None,
            audio_buffers: Vec::new(),
            audio_sinks: Vec::new(),

            gilrs: None,

            render_targets: Vec::new(),
            active_render_target: None,

            models: Vec::new(),

            skinned_pipeline: None,
            bone_bind_group_layout: None,
            anim_models: Vec::new(),
            skinned_draw_calls: Vec::new(),
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

            // Uniform buffer: view_proj(64) + camera_pos(16) + ambient(16) + lights(512) = 608 bytes
            let uniform_buffer =
                device.create_buffer(&wgpu::BufferDescriptor {
                    label: Some("uniform_buffer"),
                    size: std::mem::size_of::<Uniforms>() as u64,
                    usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                    mapped_at_creation: false,
                });

            // Bind group layout (group 0: uniforms)
            let bind_group_layout =
                device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("bind_group_layout"),
                    entries: &[wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX_FRAGMENT,
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

            // Texture bind group layout (group 1: texture + sampler)
            let texture_bind_group_layout =
                device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("texture_bind_group_layout"),
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

            // Default 1x1 white texture (used when no texture is bound)
            let default_tex = device.create_texture(&wgpu::TextureDescriptor {
                label: Some("default_white_texture"),
                size: wgpu::Extent3d { width: 1, height: 1, depth_or_array_layers: 1 },
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: wgpu::TextureFormat::Rgba8UnormSrgb,
                usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                view_formats: &[],
            });
            queue.write_texture(
                wgpu::ImageCopyTexture {
                    texture: &default_tex,
                    mip_level: 0,
                    origin: wgpu::Origin3d::ZERO,
                    aspect: wgpu::TextureAspect::All,
                },
                &[255u8, 255, 255, 255], // solid white
                wgpu::ImageDataLayout { offset: 0, bytes_per_row: Some(4), rows_per_image: Some(1) },
                wgpu::Extent3d { width: 1, height: 1, depth_or_array_layers: 1 },
            );
            let default_tex_view = default_tex.create_view(&Default::default());
            let default_tex_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
                label: Some("default_sampler"),
                mag_filter: wgpu::FilterMode::Linear,
                min_filter: wgpu::FilterMode::Linear,
                ..Default::default()
            });
            let default_texture_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("default_texture_bg"),
                layout: &texture_bind_group_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(&default_tex_view),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(&default_tex_sampler),
                    },
                ],
            });

            // Pipeline layout (group 0: uniforms, group 1: texture)
            let pipeline_layout =
                device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("pipeline_layout"),
                    bind_group_layouts: &[&bind_group_layout, &texture_bind_group_layout],
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

            // ---- Skinned pipeline for skeletal animation ----
            let bone_bind_group_layout =
                device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("bone_bind_group_layout"),
                    entries: &[wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Storage { read_only: true },
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    }],
                });

            let skinned_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("skinned_shader"),
                source: wgpu::ShaderSource::Wgsl(SKINNED_SHADER_SRC.into()),
            });

            let skinned_pipeline_layout =
                device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("skinned_pipeline_layout"),
                    bind_group_layouts: &[
                        &bind_group_layout,
                        &texture_bind_group_layout,
                        &bone_bind_group_layout,
                    ],
                    push_constant_ranges: &[],
                });

            let skinned_pipeline =
                device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: Some("skinned_render_pipeline"),
                    layout: Some(&skinned_pipeline_layout),
                    vertex: wgpu::VertexState {
                        module: &skinned_shader,
                        entry_point: Some("vs_main"),
                        buffers: &[SkinnedVertex::layout()],
                        compilation_options: Default::default(),
                    },
                    fragment: Some(wgpu::FragmentState {
                        module: &skinned_shader,
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
                        strip_index_format: None,
                        front_face: wgpu::FrontFace::Ccw,
                        cull_mode: None,
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
            state.texture_bind_group_layout = Some(texture_bind_group_layout);
            state.default_texture_bind_group = Some(default_texture_bind_group);
            state.ui_blit_pipeline = Some(ui_blit_pipeline);
            state.ui_blit_bind_group_layout = Some(ui_blit_bind_group_layout);
            state.ui_blit_sampler = Some(ui_blit_sampler);
            state.skinned_pipeline = Some(skinned_pipeline);
            state.bone_bind_group_layout = Some(bone_bind_group_layout);
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

            WindowEvent::MouseWheel { delta, .. } => {
                let dy = match delta {
                    winit::event::MouseScrollDelta::LineDelta(_, y) => y as f64,
                    winit::event::MouseScrollDelta::PixelDelta(pos) => pos.y / 30.0,
                };
                with_state(|s| s.mouse_scroll_delta += dy);
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

                    // -- Build GPU light array --
                    let mut gpu_lights = [GpuLight {
                        position_and_type: [0.0; 4],
                        direction_and_intensity: [0.0; 4],
                        color_and_enabled: [0.0; 4],
                        params: [0.0; 4],
                    }; MAX_LIGHTS];
                    let mut num_lights: u32 = 0;
                    for i in 0..MAX_LIGHTS {
                        let l = &state.lights[i];
                        if l.light_type != 0 && l.enabled {
                            gpu_lights[i] = GpuLight {
                                position_and_type: [l.position[0], l.position[1], l.position[2], l.light_type as f32],
                                direction_and_intensity: [l.direction[0], l.direction[1], l.direction[2], l.intensity],
                                color_and_enabled: [l.color[0], l.color[1], l.color[2], 1.0],
                                params: [l.range, l.inner_cone_cos, l.outer_cone_cos, 0.0],
                            };
                            num_lights = (i + 1) as u32;
                        }
                    }

                    // -- Build and upload uniforms --
                    let uniforms = Uniforms {
                        view_proj,
                        camera_pos: [eye[0], eye[1], eye[2], 0.0],
                        ambient_and_count: [
                            state.ambient_color[0],
                            state.ambient_color[1],
                            state.ambient_color[2],
                            num_lights as f32,
                        ],
                        lights: gpu_lights,
                    };
                    queue.write_buffer(
                        uniform_buf,
                        0,
                        bytemuck::bytes_of(&uniforms),
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
                                // Bind texture (active or default white)
                                let tex_bg = if let Some(idx) = state.active_texture {
                                    state.textures.get(idx).map(|t| &t.bind_group)
                                } else {
                                    None
                                };
                                let tex_bg = tex_bg.or(state.default_texture_bind_group.as_ref());
                                if let Some(tbg) = tex_bg {
                                    rpass.set_bind_group(1, tbg, &[]);
                                }
                                rpass.set_vertex_buffer(0, vb.slice(..));
                                rpass.draw(0..vertex_count, 0..1);
                            }

                            // -- Skinned mesh rendering --
                            if let Some(ref skinned_pl) = state.skinned_pipeline {
                                let draw_calls: Vec<usize> = state.skinned_draw_calls.clone();
                                if !draw_calls.is_empty() {
                                    rpass.set_pipeline(skinned_pl);
                                    rpass.set_bind_group(0, bind_group, &[]);

                                    let default_tex = state.default_texture_bind_group.as_ref();
                                    if let Some(tbg) = default_tex {
                                        rpass.set_bind_group(1, tbg, &[]);
                                    }

                                    for &model_idx in &draw_calls {
                                        if model_idx >= state.anim_models.len() { continue; }
                                        let model = &state.anim_models[model_idx];
                                        if model.skinned_vertices.is_empty() { continue; }

                                        // Upload bone matrices
                                        let bone_mats = compute_bone_matrices(model);
                                        let bone_data: Vec<f32> = bone_mats.iter()
                                            .flat_map(|m| m.iter().flat_map(|r| r.iter().copied()))
                                            .collect();
                                        queue.write_buffer(
                                            &model.bone_buffer,
                                            0,
                                            bytemuck::cast_slice(&bone_data),
                                        );

                                        // Create skinned vertex buffer
                                        let svb = device.create_buffer_init(
                                            &wgpu::util::BufferInitDescriptor {
                                                label: Some("skinned_vb"),
                                                contents: bytemuck::cast_slice(&model.skinned_vertices),
                                                usage: wgpu::BufferUsages::VERTEX,
                                            },
                                        );

                                        rpass.set_bind_group(2, &model.bone_bind_group, &[]);
                                        rpass.set_vertex_buffer(0, svb.slice(..));
                                        rpass.draw(0..model.skinned_vertices.len() as u32, 0..1);
                                    }
                                }
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
                    state.mouse_scroll_delta = 0.0;
                    state.skinned_draw_calls.clear();
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
            normal: [0.0, 0.0, 0.0],
            uv: [0.0, 0.0],
            color: [r as f32, g as f32, b as f32],
        });
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_push_vertex_lit(
    x: f64, y: f64, z: f64,
    nx: f64, ny: f64, nz: f64,
    r: f64, g: f64, b: f64,
) {
    try_with_state(|s| {
        s.vertices.push(Vertex {
            position: [x as f32, y as f32, z as f32],
            normal: [nx as f32, ny as f32, nz as f32],
            uv: [0.0, 0.0],
            color: [r as f32, g as f32, b as f32],
        });
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_push_vertex_uv(
    x: f64, y: f64, z: f64,
    nx: f64, ny: f64, nz: f64,
    u: f64, v: f64,
    r: f64, g: f64, b: f64,
) {
    try_with_state(|s| {
        s.vertices.push(Vertex {
            position: [x as f32, y as f32, z as f32],
            normal: [nx as f32, ny as f32, nz as f32],
            uv: [u as f32, v as f32],
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
// FFI: Lighting
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_ambient_color(r: f64, g: f64, b: f64) {
    try_with_state(|s| {
        s.ambient_color = [r as f32, g as f32, b as f32];
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_light_set_type(slot: i64, light_type: i64) {
    try_with_state(|s| {
        let idx = slot as usize;
        if idx < MAX_LIGHTS {
            s.lights[idx].light_type = light_type as u32;
            if light_type != 0 {
                s.lights[idx].enabled = true;
            }
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_light_set_enabled(slot: i64, enabled: i64) {
    try_with_state(|s| {
        let idx = slot as usize;
        if idx < MAX_LIGHTS {
            s.lights[idx].enabled = enabled != 0;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_light_set_position(slot: i64, x: f64, y: f64, z: f64) {
    try_with_state(|s| {
        let idx = slot as usize;
        if idx < MAX_LIGHTS {
            s.lights[idx].position = [x as f32, y as f32, z as f32];
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_light_set_direction(slot: i64, dx: f64, dy: f64, dz: f64) {
    try_with_state(|s| {
        let idx = slot as usize;
        if idx < MAX_LIGHTS {
            s.lights[idx].direction = [dx as f32, dy as f32, dz as f32];
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_light_set_color(slot: i64, r: f64, g: f64, b: f64) {
    try_with_state(|s| {
        let idx = slot as usize;
        if idx < MAX_LIGHTS {
            s.lights[idx].color = [r as f32, g as f32, b as f32];
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_light_set_intensity(slot: i64, intensity: f64) {
    try_with_state(|s| {
        let idx = slot as usize;
        if idx < MAX_LIGHTS {
            s.lights[idx].intensity = intensity as f32;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_light_set_range(slot: i64, range: f64) {
    try_with_state(|s| {
        let idx = slot as usize;
        if idx < MAX_LIGHTS {
            s.lights[idx].range = range as f32;
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_light_set_spot_angles(slot: i64, inner_deg: f64, outer_deg: f64) {
    try_with_state(|s| {
        let idx = slot as usize;
        if idx < MAX_LIGHTS {
            s.lights[idx].inner_cone_cos = (inner_deg as f32).to_radians().cos();
            s.lights[idx].outer_cone_cos = (outer_deg as f32).to_radians().cos();
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_clear_lights() {
    try_with_state(|s| {
        for i in 0..MAX_LIGHTS {
            s.lights[i] = LightData::new();
        }
    });
}

// =======================================================================
// FFI: Texture2D
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_texture_load(path_ptr: *const c_char) -> i64 {
    let path = cstr_to_string(path_ptr);
    let img = match image::open(&path) {
        Ok(img) => img.to_rgba8(),
        Err(_) => return -1,
    };
    let (w, h) = img.dimensions();
    let data = img.into_raw();

    with_state(|s| {
        let (Some(ref device), Some(ref queue), Some(ref layout)) =
            (&s.device, &s.queue, &s.texture_bind_group_layout)
        else {
            return -1;
        };

        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("loaded_texture"),
            size: wgpu::Extent3d { width: w, height: h, depth_or_array_layers: 1 },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        queue.write_texture(
            wgpu::ImageCopyTexture {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            &data,
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(4 * w),
                rows_per_image: Some(h),
            },
            wgpu::Extent3d { width: w, height: h, depth_or_array_layers: 1 },
        );

        let view = texture.create_view(&Default::default());
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("texture_sampler"),
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("texture_bg"),
            layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });

        let handle = s.textures.len() as i64;
        s.textures.push(LoadedTexture { texture, bind_group, width: w, height: h });
        handle
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_texture_bind(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx < s.textures.len() {
            s.active_texture = Some(idx);
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_texture_unbind() {
    try_with_state(|s| {
        s.active_texture = None;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_texture_width(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        s.textures.get(idx).map(|t| t.width as i64).unwrap_or(0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_texture_height(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        s.textures.get(idx).map(|t| t.height as i64).unwrap_or(0)
    })
}

// =======================================================================
// FFI: SpriteBatch (2D textured quads using orthographic projection)
// =======================================================================

// SpriteBatch mode: saves/restores 3D camera state and renders with ortho projection
#[no_mangle]
pub unsafe extern "C" fn nex_engine_spritebatch_begin() {
    // No-op: batching is implicit. The Nex side will save camera state.
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_spritebatch_end() {
    // No-op: batching is implicit. The Nex side will restore camera state.
}

/// Draw a textured quad: tex_handle, dest_x, dest_y, dest_w, dest_h, r, g, b, a
#[no_mangle]
pub unsafe extern "C" fn nex_engine_spritebatch_draw(
    tex: i64, dx: f64, dy: f64, dw: f64, dh: f64,
    r: f64, g: f64, b: f64, _a: f64,
) {
    try_with_state(|s| {
        // Bind this texture for the quad
        let idx = tex as usize;
        if idx < s.textures.len() {
            s.active_texture = Some(idx);
        }
        let x0 = dx as f32;
        let y0 = dy as f32;
        let x1 = (dx + dw) as f32;
        let y1 = (dy + dh) as f32;
        let rf = r as f32;
        let gf = g as f32;
        let bf = b as f32;
        // Two triangles forming a quad, UV mapped [0,1]
        s.vertices.push(Vertex { position: [x0, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [0.0, 0.0], color: [rf, gf, bf] });
        s.vertices.push(Vertex { position: [x1, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [1.0, 0.0], color: [rf, gf, bf] });
        s.vertices.push(Vertex { position: [x1, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [1.0, 1.0], color: [rf, gf, bf] });
        s.vertices.push(Vertex { position: [x0, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [0.0, 0.0], color: [rf, gf, bf] });
        s.vertices.push(Vertex { position: [x1, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [1.0, 1.0], color: [rf, gf, bf] });
        s.vertices.push(Vertex { position: [x0, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [0.0, 1.0], color: [rf, gf, bf] });
    });
}

/// Draw with source rectangle: tex_handle, src_x, src_y, src_w, src_h, dest_x, dest_y, dest_w, dest_h
#[no_mangle]
pub unsafe extern "C" fn nex_engine_spritebatch_draw_src(
    tex: i64,
    sx: f64, sy: f64, sw: f64, sh: f64,
    dx: f64, dy: f64, dw: f64, dh: f64,
) {
    try_with_state(|s| {
        let idx = tex as usize;
        if idx >= s.textures.len() { return; }
        let tw = s.textures[idx].width as f32;
        let th = s.textures[idx].height as f32;
        s.active_texture = Some(idx);
        // Compute UV from source rect
        let u0 = sx as f32 / tw;
        let v0 = sy as f32 / th;
        let u1 = (sx as f32 + sw as f32) / tw;
        let v1 = (sy as f32 + sh as f32) / th;
        let x0 = dx as f32;
        let y0 = dy as f32;
        let x1 = (dx + dw) as f32;
        let y1 = (dy + dh) as f32;
        s.vertices.push(Vertex { position: [x0, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [u0, v0], color: [1.0, 1.0, 1.0] });
        s.vertices.push(Vertex { position: [x1, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [u1, v0], color: [1.0, 1.0, 1.0] });
        s.vertices.push(Vertex { position: [x1, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [u1, v1], color: [1.0, 1.0, 1.0] });
        s.vertices.push(Vertex { position: [x0, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [u0, v0], color: [1.0, 1.0, 1.0] });
        s.vertices.push(Vertex { position: [x1, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [u1, v1], color: [1.0, 1.0, 1.0] });
        s.vertices.push(Vertex { position: [x0, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [u0, v1], color: [1.0, 1.0, 1.0] });
    });
}

// =======================================================================
// FFI: SpriteFont (built-in 8x8 bitmap font)
// =======================================================================

// Font texture handle (-1 = not yet created)
static FONT_TEX: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(-1);

fn ensure_font_texture() {
    use std::sync::atomic::Ordering;
    if FONT_TEX.load(Ordering::Relaxed) >= 0 { return; }

    // Generate a 128x64 texture with ASCII chars 32-126 arranged in a 16x6 grid.
    // Each cell is 8x8 pixels. We generate a simple recognizable font procedurally.
    let tw: u32 = 128;
    let th: u32 = 64;
    let mut pixels = vec![0u8; (tw * th * 4) as usize];

    // Use a simple procedural font: each ASCII char mapped to a basic pattern
    // For now, create white rectangles with character shapes
    for ch in 32u8..127 {
        let idx = (ch - 32) as u32;
        let cx = (idx % 16) * 8;
        let cy = (idx / 16) * 8;

        // Simple: draw the character outline — a minimal recognizable pattern
        // For a real font, this would be a proper bitmap. This gives something visible.
        let pattern = simple_char_pattern(ch);
        for row in 0..8u32 {
            let byte = pattern[row as usize];
            for col in 0..8u32 {
                if (byte >> (7 - col)) & 1 != 0 {
                    let px = cx + col;
                    let py = cy + row;
                    let off = ((py * tw + px) * 4) as usize;
                    pixels[off] = 255;
                    pixels[off + 1] = 255;
                    pixels[off + 2] = 255;
                    pixels[off + 3] = 255;
                }
            }
        }
    }

    // Upload to GPU
    with_state(|s| {
        let (Some(ref device), Some(ref queue), Some(ref layout)) =
            (&s.device, &s.queue, &s.texture_bind_group_layout)
        else { return; };

        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("font_texture"),
            size: wgpu::Extent3d { width: tw, height: th, depth_or_array_layers: 1 },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        queue.write_texture(
            wgpu::ImageCopyTexture { texture: &texture, mip_level: 0, origin: wgpu::Origin3d::ZERO, aspect: wgpu::TextureAspect::All },
            &pixels,
            wgpu::ImageDataLayout { offset: 0, bytes_per_row: Some(4 * tw), rows_per_image: Some(th) },
            wgpu::Extent3d { width: tw, height: th, depth_or_array_layers: 1 },
        );
        let view = texture.create_view(&Default::default());
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("font_sampler"),
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("font_bg"),
            layout,
            entries: &[
                wgpu::BindGroupEntry { binding: 0, resource: wgpu::BindingResource::TextureView(&view) },
                wgpu::BindGroupEntry { binding: 1, resource: wgpu::BindingResource::Sampler(&sampler) },
            ],
        });
        let handle = s.textures.len() as i64;
        s.textures.push(LoadedTexture { texture, bind_group, width: tw, height: th });
        FONT_TEX.store(handle, Ordering::Relaxed);
    });
}

/// Return an 8-byte pattern for a printable ASCII character.
fn simple_char_pattern(ch: u8) -> [u8; 8] {
    // Minimal built-in 5x7 font patterns for common chars
    match ch {
        b' ' => [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        b'!' => [0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x10, 0x00],
        b'"' => [0x28, 0x28, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        b'#' => [0x28, 0x7C, 0x28, 0x28, 0x7C, 0x28, 0x00, 0x00],
        b'$' => [0x10, 0x3C, 0x50, 0x38, 0x14, 0x78, 0x10, 0x00],
        b'%' => [0x44, 0x08, 0x10, 0x20, 0x44, 0x00, 0x00, 0x00],
        b'&' => [0x30, 0x48, 0x30, 0x50, 0x4C, 0x34, 0x00, 0x00],
        b'\'' => [0x10, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        b'(' => [0x08, 0x10, 0x20, 0x20, 0x20, 0x10, 0x08, 0x00],
        b')' => [0x20, 0x10, 0x08, 0x08, 0x08, 0x10, 0x20, 0x00],
        b'*' => [0x00, 0x28, 0x10, 0x7C, 0x10, 0x28, 0x00, 0x00],
        b'+' => [0x00, 0x10, 0x10, 0x7C, 0x10, 0x10, 0x00, 0x00],
        b',' => [0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x10, 0x20],
        b'-' => [0x00, 0x00, 0x00, 0x7C, 0x00, 0x00, 0x00, 0x00],
        b'.' => [0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x18, 0x00],
        b'/' => [0x04, 0x08, 0x10, 0x20, 0x40, 0x00, 0x00, 0x00],
        b'0' => [0x38, 0x44, 0x4C, 0x54, 0x64, 0x44, 0x38, 0x00],
        b'1' => [0x10, 0x30, 0x10, 0x10, 0x10, 0x10, 0x38, 0x00],
        b'2' => [0x38, 0x44, 0x04, 0x18, 0x20, 0x40, 0x7C, 0x00],
        b'3' => [0x38, 0x44, 0x04, 0x18, 0x04, 0x44, 0x38, 0x00],
        b'4' => [0x08, 0x18, 0x28, 0x48, 0x7C, 0x08, 0x08, 0x00],
        b'5' => [0x7C, 0x40, 0x78, 0x04, 0x04, 0x44, 0x38, 0x00],
        b'6' => [0x18, 0x20, 0x40, 0x78, 0x44, 0x44, 0x38, 0x00],
        b'7' => [0x7C, 0x04, 0x08, 0x10, 0x20, 0x20, 0x20, 0x00],
        b'8' => [0x38, 0x44, 0x44, 0x38, 0x44, 0x44, 0x38, 0x00],
        b'9' => [0x38, 0x44, 0x44, 0x3C, 0x04, 0x08, 0x30, 0x00],
        b':' => [0x00, 0x18, 0x18, 0x00, 0x18, 0x18, 0x00, 0x00],
        b';' => [0x00, 0x18, 0x18, 0x00, 0x18, 0x08, 0x10, 0x00],
        b'<' => [0x04, 0x08, 0x10, 0x20, 0x10, 0x08, 0x04, 0x00],
        b'=' => [0x00, 0x00, 0x7C, 0x00, 0x7C, 0x00, 0x00, 0x00],
        b'>' => [0x40, 0x20, 0x10, 0x08, 0x10, 0x20, 0x40, 0x00],
        b'?' => [0x38, 0x44, 0x04, 0x08, 0x10, 0x00, 0x10, 0x00],
        b'@' => [0x38, 0x44, 0x5C, 0x54, 0x5C, 0x40, 0x3C, 0x00],
        b'A' => [0x38, 0x44, 0x44, 0x7C, 0x44, 0x44, 0x44, 0x00],
        b'B' => [0x78, 0x44, 0x44, 0x78, 0x44, 0x44, 0x78, 0x00],
        b'C' => [0x38, 0x44, 0x40, 0x40, 0x40, 0x44, 0x38, 0x00],
        b'D' => [0x78, 0x44, 0x44, 0x44, 0x44, 0x44, 0x78, 0x00],
        b'E' => [0x7C, 0x40, 0x40, 0x78, 0x40, 0x40, 0x7C, 0x00],
        b'F' => [0x7C, 0x40, 0x40, 0x78, 0x40, 0x40, 0x40, 0x00],
        b'G' => [0x38, 0x44, 0x40, 0x5C, 0x44, 0x44, 0x3C, 0x00],
        b'H' => [0x44, 0x44, 0x44, 0x7C, 0x44, 0x44, 0x44, 0x00],
        b'I' => [0x38, 0x10, 0x10, 0x10, 0x10, 0x10, 0x38, 0x00],
        b'J' => [0x1C, 0x08, 0x08, 0x08, 0x08, 0x48, 0x30, 0x00],
        b'K' => [0x44, 0x48, 0x50, 0x60, 0x50, 0x48, 0x44, 0x00],
        b'L' => [0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x7C, 0x00],
        b'M' => [0x44, 0x6C, 0x54, 0x44, 0x44, 0x44, 0x44, 0x00],
        b'N' => [0x44, 0x64, 0x54, 0x4C, 0x44, 0x44, 0x44, 0x00],
        b'O' => [0x38, 0x44, 0x44, 0x44, 0x44, 0x44, 0x38, 0x00],
        b'P' => [0x78, 0x44, 0x44, 0x78, 0x40, 0x40, 0x40, 0x00],
        b'Q' => [0x38, 0x44, 0x44, 0x44, 0x54, 0x48, 0x34, 0x00],
        b'R' => [0x78, 0x44, 0x44, 0x78, 0x50, 0x48, 0x44, 0x00],
        b'S' => [0x38, 0x44, 0x40, 0x38, 0x04, 0x44, 0x38, 0x00],
        b'T' => [0x7C, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00],
        b'U' => [0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x38, 0x00],
        b'V' => [0x44, 0x44, 0x44, 0x44, 0x28, 0x28, 0x10, 0x00],
        b'W' => [0x44, 0x44, 0x44, 0x54, 0x54, 0x6C, 0x44, 0x00],
        b'X' => [0x44, 0x44, 0x28, 0x10, 0x28, 0x44, 0x44, 0x00],
        b'Y' => [0x44, 0x44, 0x28, 0x10, 0x10, 0x10, 0x10, 0x00],
        b'Z' => [0x7C, 0x04, 0x08, 0x10, 0x20, 0x40, 0x7C, 0x00],
        b'[' => [0x38, 0x20, 0x20, 0x20, 0x20, 0x20, 0x38, 0x00],
        b'\\' => [0x40, 0x20, 0x10, 0x08, 0x04, 0x00, 0x00, 0x00],
        b']' => [0x38, 0x08, 0x08, 0x08, 0x08, 0x08, 0x38, 0x00],
        b'^' => [0x10, 0x28, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00],
        b'_' => [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7C, 0x00],
        b'`' => [0x20, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        b'a' => [0x00, 0x00, 0x38, 0x04, 0x3C, 0x44, 0x3C, 0x00],
        b'b' => [0x40, 0x40, 0x78, 0x44, 0x44, 0x44, 0x78, 0x00],
        b'c' => [0x00, 0x00, 0x38, 0x44, 0x40, 0x44, 0x38, 0x00],
        b'd' => [0x04, 0x04, 0x3C, 0x44, 0x44, 0x44, 0x3C, 0x00],
        b'e' => [0x00, 0x00, 0x38, 0x44, 0x7C, 0x40, 0x38, 0x00],
        b'f' => [0x18, 0x24, 0x20, 0x70, 0x20, 0x20, 0x20, 0x00],
        b'g' => [0x00, 0x00, 0x3C, 0x44, 0x44, 0x3C, 0x04, 0x38],
        b'h' => [0x40, 0x40, 0x78, 0x44, 0x44, 0x44, 0x44, 0x00],
        b'i' => [0x10, 0x00, 0x30, 0x10, 0x10, 0x10, 0x38, 0x00],
        b'j' => [0x08, 0x00, 0x18, 0x08, 0x08, 0x08, 0x48, 0x30],
        b'k' => [0x40, 0x40, 0x48, 0x50, 0x60, 0x50, 0x48, 0x00],
        b'l' => [0x30, 0x10, 0x10, 0x10, 0x10, 0x10, 0x38, 0x00],
        b'm' => [0x00, 0x00, 0x68, 0x54, 0x54, 0x44, 0x44, 0x00],
        b'n' => [0x00, 0x00, 0x78, 0x44, 0x44, 0x44, 0x44, 0x00],
        b'o' => [0x00, 0x00, 0x38, 0x44, 0x44, 0x44, 0x38, 0x00],
        b'p' => [0x00, 0x00, 0x78, 0x44, 0x44, 0x78, 0x40, 0x40],
        b'q' => [0x00, 0x00, 0x3C, 0x44, 0x44, 0x3C, 0x04, 0x04],
        b'r' => [0x00, 0x00, 0x58, 0x64, 0x40, 0x40, 0x40, 0x00],
        b's' => [0x00, 0x00, 0x3C, 0x40, 0x38, 0x04, 0x78, 0x00],
        b't' => [0x20, 0x20, 0x70, 0x20, 0x20, 0x24, 0x18, 0x00],
        b'u' => [0x00, 0x00, 0x44, 0x44, 0x44, 0x44, 0x3C, 0x00],
        b'v' => [0x00, 0x00, 0x44, 0x44, 0x44, 0x28, 0x10, 0x00],
        b'w' => [0x00, 0x00, 0x44, 0x44, 0x54, 0x54, 0x28, 0x00],
        b'x' => [0x00, 0x00, 0x44, 0x28, 0x10, 0x28, 0x44, 0x00],
        b'y' => [0x00, 0x00, 0x44, 0x44, 0x44, 0x3C, 0x04, 0x38],
        b'z' => [0x00, 0x00, 0x7C, 0x08, 0x10, 0x20, 0x7C, 0x00],
        b'{' => [0x0C, 0x10, 0x10, 0x60, 0x10, 0x10, 0x0C, 0x00],
        b'|' => [0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00],
        b'}' => [0x60, 0x10, 0x10, 0x0C, 0x10, 0x10, 0x60, 0x00],
        b'~' => [0x00, 0x24, 0x54, 0x48, 0x00, 0x00, 0x00, 0x00],
        _ => [0x7C, 0x44, 0x44, 0x44, 0x44, 0x44, 0x7C, 0x00], // box for unknown
    }
}

/// Draw text: text_ptr, text_len, x, y, scale, r, g, b
#[no_mangle]
pub unsafe extern "C" fn nex_engine_font_draw_text(
    text_ptr: *const c_char,
    x: f64, y: f64, scale: f64,
    r: f64, g: f64, b: f64,
) {
    ensure_font_texture();
    let font_handle = FONT_TEX.load(std::sync::atomic::Ordering::Relaxed);
    if font_handle < 0 { return; }

    let text = cstr_to_string(text_ptr);
    let char_w = 8.0 * scale;
    let char_h = 8.0 * scale;
    let tex_w = 128.0_f32;
    let tex_h = 64.0_f32;

    try_with_state(|s| {
        let idx = font_handle as usize;
        if idx >= s.textures.len() { return; }
        s.active_texture = Some(idx);
        let rf = r as f32;
        let gf = g as f32;
        let bf = b as f32;

        for (i, ch) in text.bytes().enumerate() {
            if ch < 32 || ch > 126 { continue; }
            let ci = (ch - 32) as u32;
            let cu = (ci % 16) as f32 * 8.0 / tex_w;
            let cv = (ci / 16) as f32 * 8.0 / tex_h;
            let cu1 = cu + 8.0 / tex_w;
            let cv1 = cv + 8.0 / tex_h;
            let x0 = (x + i as f64 * char_w) as f32;
            let y0 = y as f32;
            let x1 = x0 + char_w as f32;
            let y1 = y0 + char_h as f32;

            s.vertices.push(Vertex { position: [x0, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [cu, cv], color: [rf, gf, bf] });
            s.vertices.push(Vertex { position: [x1, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [cu1, cv], color: [rf, gf, bf] });
            s.vertices.push(Vertex { position: [x1, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [cu1, cv1], color: [rf, gf, bf] });
            s.vertices.push(Vertex { position: [x0, y0, 0.0], normal: [0.0, 0.0, 1.0], uv: [cu, cv], color: [rf, gf, bf] });
            s.vertices.push(Vertex { position: [x1, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [cu1, cv1], color: [rf, gf, bf] });
            s.vertices.push(Vertex { position: [x0, y1, 0.0], normal: [0.0, 0.0, 1.0], uv: [cu, cv1], color: [rf, gf, bf] });
        }
    });
}

/// Measure text width: text_ptr → pixel width at scale
#[no_mangle]
pub unsafe extern "C" fn nex_engine_font_measure_text(text_ptr: *const c_char, scale: f64) -> f64 {
    let text = cstr_to_string(text_ptr);
    let count = text.bytes().filter(|&b| b >= 32 && b <= 126).count();
    count as f64 * 8.0 * scale
}

// =======================================================================
// FFI: Skeletal Animation (glTF)
// =======================================================================

/// Load a glTF/GLB model with skeleton and animations. Returns handle.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_model_load(path_ptr: *const c_char) -> i64 {
    let path = cstr_to_string(path_ptr);
    let (document, buffers, _images) = match gltf::import(&path) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("[nex3d] anim: failed to load '{}': {}", path, e);
            return -1;
        }
    };

    let mut skinned_vertices: Vec<SkinnedVertex> = Vec::new();
    let mut joints: Vec<Joint> = Vec::new();
    let mut animations: Vec<AnimClip> = Vec::new();

    // Build node index → joint index mapping
    let skin = document.skins().next(); // use first skin

    // Collect joint nodes and inverse bind matrices
    if let Some(ref skin) = skin {
        let joint_nodes: Vec<usize> = skin.joints().map(|j| j.index()).collect();
        let reader = skin.reader(|buf| Some(&buffers[buf.index()]));

        let inv_bind_mats: Vec<[[f32; 4]; 4]> = reader
            .read_inverse_bind_matrices()
            .map(|iter| iter.collect())
            .unwrap_or_else(|| vec![mat4x4_identity(); joint_nodes.len()]);

        // Build joint hierarchy
        for (ji, &node_idx) in joint_nodes.iter().enumerate() {
            let node = document.nodes().nth(node_idx).unwrap();
            let (t, r, s) = node.transform().decomposed();

            // Find parent joint index
            let parent = find_parent_joint(&document, node_idx, &joint_nodes);

            joints.push(Joint {
                parent,
                inv_bind: inv_bind_mats.get(ji).copied().unwrap_or(mat4x4_identity()),
                local_translation: t,
                local_rotation: r,
                local_scale: s,
            });
        }

        // Extract mesh data with joint attributes
        for mesh in document.meshes() {
            for primitive in mesh.primitives() {
                let reader = primitive.reader(|buf| Some(&buffers[buf.index()]));

                let positions: Vec<[f32; 3]> = reader
                    .read_positions()
                    .map(|iter| iter.collect())
                    .unwrap_or_default();

                let normals: Vec<[f32; 3]> = reader
                    .read_normals()
                    .map(|iter| iter.collect())
                    .unwrap_or_else(|| vec![[0.0, 1.0, 0.0]; positions.len()]);

                let tex_coords: Vec<[f32; 2]> = reader
                    .read_tex_coords(0)
                    .map(|iter| iter.into_f32().collect())
                    .unwrap_or_else(|| vec![[0.0, 0.0]; positions.len()]);

                let joint_indices: Vec<[u16; 4]> = reader
                    .read_joints(0)
                    .map(|iter| iter.into_u16().collect())
                    .unwrap_or_else(|| vec![[0, 0, 0, 0]; positions.len()]);

                let weights: Vec<[f32; 4]> = reader
                    .read_weights(0)
                    .map(|iter| iter.into_f32().collect())
                    .unwrap_or_else(|| vec![[1.0, 0.0, 0.0, 0.0]; positions.len()]);

                // Build vertex list
                let mut verts: Vec<SkinnedVertex> = Vec::with_capacity(positions.len());
                for i in 0..positions.len() {
                    verts.push(SkinnedVertex {
                        position: positions[i],
                        normal: normals.get(i).copied().unwrap_or([0.0, 1.0, 0.0]),
                        uv: tex_coords.get(i).copied().unwrap_or([0.0, 0.0]),
                        color: [1.0, 1.0, 1.0],
                        joint_indices: [
                            joint_indices[i][0] as u32,
                            joint_indices[i][1] as u32,
                            joint_indices[i][2] as u32,
                            joint_indices[i][3] as u32,
                        ],
                        bone_weights: weights.get(i).copied().unwrap_or([1.0, 0.0, 0.0, 0.0]),
                    });
                }

                // Apply indices if present
                if let Some(indices) = reader.read_indices() {
                    let indices: Vec<u32> = indices.into_u32().collect();
                    let indexed: Vec<SkinnedVertex> = indices.iter().map(|&idx| {
                        verts.get(idx as usize).copied().unwrap_or(SkinnedVertex {
                            position: [0.0, 0.0, 0.0],
                            normal: [0.0, 1.0, 0.0],
                            uv: [0.0, 0.0],
                            color: [1.0, 1.0, 1.0],
                            joint_indices: [0, 0, 0, 0],
                            bone_weights: [1.0, 0.0, 0.0, 0.0],
                        })
                    }).collect();
                    skinned_vertices.extend_from_slice(&indexed);
                } else {
                    skinned_vertices.extend_from_slice(&verts);
                }
            }
        }

        // Extract animations
        let joint_nodes: Vec<usize> = skin.joints().map(|j| j.index()).collect();
        for anim in document.animations() {
            let mut channels: Vec<AnimChannel> = Vec::new();
            let mut duration: f32 = 0.0;

            for channel in anim.channels() {
                let target_node = channel.target().node().index();
                // Map node index to joint index
                let joint_idx = match joint_nodes.iter().position(|&n| n == target_node) {
                    Some(idx) => idx,
                    None => continue,
                };

                let reader = channel.reader(|buf| Some(&buffers[buf.index()]));
                let times: Vec<f32> = reader.read_inputs().map(|iter| iter.collect()).unwrap_or_default();
                if let Some(&last) = times.last() {
                    duration = duration.max(last);
                }

                // Find or create channel for this joint
                let ch_idx = channels.iter().position(|c| c.joint_index == joint_idx)
                    .unwrap_or_else(|| {
                        channels.push(AnimChannel {
                            joint_index: joint_idx,
                            translations: Vec::new(),
                            rotations: Vec::new(),
                            scales: Vec::new(),
                        });
                        channels.len() - 1
                    });

                match reader.read_outputs() {
                    Some(gltf::animation::util::ReadOutputs::Translations(iter)) => {
                        let values: Vec<[f32; 3]> = iter.collect();
                        for (i, &t) in times.iter().enumerate() {
                            if let Some(&v) = values.get(i) {
                                channels[ch_idx].translations.push(AnimKeyframe { time: t, value: v });
                            }
                        }
                    }
                    Some(gltf::animation::util::ReadOutputs::Rotations(iter)) => {
                        let values: Vec<[f32; 4]> = iter.into_f32().collect();
                        for (i, &t) in times.iter().enumerate() {
                            if let Some(&v) = values.get(i) {
                                channels[ch_idx].rotations.push(AnimKeyframe { time: t, value: v });
                            }
                        }
                    }
                    Some(gltf::animation::util::ReadOutputs::Scales(iter)) => {
                        let values: Vec<[f32; 3]> = iter.collect();
                        for (i, &t) in times.iter().enumerate() {
                            if let Some(&v) = values.get(i) {
                                channels[ch_idx].scales.push(AnimKeyframe { time: t, value: v });
                            }
                        }
                    }
                    _ => {}
                }
            }

            animations.push(AnimClip {
                name: anim.name().unwrap_or("unnamed").to_string(),
                duration,
                channels,
            });
        }
    }

    // Create GPU resources
    with_state(|s| {
        let device = match s.device.as_ref() { Some(d) => d, None => return -1 };
        let layout = match s.bone_bind_group_layout.as_ref() { Some(l) => l, None => return -1 };

        let bone_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("bone_buffer"),
            size: (MAX_BONES * 16 * 4) as u64, // 128 * mat4x4<f32>
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let bone_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("bone_bind_group"),
            layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: bone_buffer.as_entire_binding(),
            }],
        });

        let handle = s.anim_models.len() as i64;
        s.anim_models.push(AnimatedModel {
            skinned_vertices,
            joints,
            animations,
            anim_state: AnimState {
                clip_index: 0,
                time: 0.0,
                speed: 1.0,
                looping: true,
                playing: false,
            },
            bone_buffer,
            bone_bind_group,
        });
        handle
    })
}

/// Helper: find parent joint index for a given node
fn find_parent_joint(document: &gltf::Document, node_idx: usize, joint_nodes: &[usize]) -> i32 {
    for node in document.nodes() {
        for child in node.children() {
            if child.index() == node_idx {
                if let Some(pos) = joint_nodes.iter().position(|&n| n == node.index()) {
                    return pos as i32;
                }
                // Parent not a joint — recurse
                return find_parent_joint(document, node.index(), joint_nodes);
            }
        }
    }
    -1 // root
}

/// Draw animated model: update animation + queue for skinned rendering.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_model_draw(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return; }

        // Advance animation time
        let dt = s.delta_time as f32;
        let model = &mut s.anim_models[idx];
        if model.anim_state.playing {
            model.anim_state.time += dt * model.anim_state.speed;
            if model.anim_state.clip_index < model.animations.len() {
                let duration = model.animations[model.anim_state.clip_index].duration;
                if duration > 0.0 {
                    if model.anim_state.looping {
                        model.anim_state.time %= duration;
                    } else if model.anim_state.time > duration {
                        model.anim_state.time = duration;
                        model.anim_state.playing = false;
                    }
                }
            }
        }

        s.skinned_draw_calls.push(idx);
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_play(handle: i64, clip: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return; }
        s.anim_models[idx].anim_state.clip_index = clip as usize;
        s.anim_models[idx].anim_state.time = 0.0;
        s.anim_models[idx].anim_state.playing = true;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_stop(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return; }
        s.anim_models[idx].anim_state.playing = false;
        s.anim_models[idx].anim_state.time = 0.0;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_pause(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return; }
        s.anim_models[idx].anim_state.playing = false;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_set_speed(handle: i64, speed_bits: i64) {
    let speed = f64::from_bits(speed_bits as u64) as f32;
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return; }
        s.anim_models[idx].anim_state.speed = speed;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_set_looping(handle: i64, flag: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return; }
        s.anim_models[idx].anim_state.looping = flag != 0;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_set_time(handle: i64, time_bits: i64) {
    let time = f64::from_bits(time_bits as u64) as f32;
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return; }
        s.anim_models[idx].anim_state.time = time;
    });
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_get_time(handle: i64) -> f64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return 0.0; }
        s.anim_models[idx].anim_state.time as f64
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_clip_count(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return 0; }
        s.anim_models[idx].animations.len() as i64
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_clip_duration(handle: i64, clip: i64) -> f64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return 0.0; }
        let ci = clip as usize;
        if ci >= s.anim_models[idx].animations.len() { return 0.0; }
        s.anim_models[idx].animations[ci].duration as f64
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_joint_count(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return 0; }
        s.anim_models[idx].joints.len() as i64
    })
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_anim_model_free(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.anim_models.len() { return; }
        s.anim_models[idx].skinned_vertices.clear();
        s.anim_models[idx].joints.clear();
        s.anim_models[idx].animations.clear();
    });
}

// =======================================================================
// FFI: Render States
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_blend_mode(_mode: i64) {
    // 0=opaque, 1=alpha, 2=additive, 3=multiply
    // Pipeline recreation would be needed for a real impl.
    // For now this is a stub that will be enhanced when pipeline caching is added.
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_cull_mode(_mode: i64) {
    // 0=none, 1=back, 2=front
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_fill_mode(_mode: i64) {
    // 0=solid, 1=wireframe
}

#[no_mangle]
pub unsafe extern "C" fn nex_engine_set_depth_enabled(_enabled: i64) {
    // 0=disabled, 1=enabled
}

// =======================================================================
// FFI: Mouse scroll
// =======================================================================

#[no_mangle]
pub unsafe extern "C" fn nex_engine_mouse_scroll_delta() -> f64 {
    with_state(|s| s.mouse_scroll_delta)
}

// =======================================================================
// FFI: Audio (rodio)
// =======================================================================

/// Ensure the audio output stream is initialized.
fn ensure_audio_stream(s: &mut EngineState) {
    if s.audio_stream.is_none() {
        match OutputStream::try_default() {
            Ok((stream, handle)) => {
                s.audio_stream = Some((stream, handle));
            }
            Err(e) => {
                eprintln!("[nex3d] audio: failed to open output stream: {}", e);
            }
        }
    }
}

/// Load an audio file (wav, mp3, ogg, flac) from disk. Returns handle (index).
#[no_mangle]
pub unsafe extern "C" fn nex_engine_audio_load(path_ptr: *const c_char) -> i64 {
    let path = cstr_to_string(path_ptr);
    match std::fs::read(&path) {
        Ok(bytes) => {
            // Verify the data is decodable
            if Decoder::new(Cursor::new(bytes.clone())).is_err() {
                eprintln!("[nex3d] audio: failed to decode '{}'", path);
                return -1;
            }
            with_state(|s| {
                ensure_audio_stream(s);
                let handle = s.audio_buffers.len() as i64;
                s.audio_buffers.push(Arc::new(bytes));
                s.audio_sinks.push(None);
                handle
            })
        }
        Err(e) => {
            eprintln!("[nex3d] audio: failed to load '{}': {}", path, e);
            -1
        }
    }
}

/// Play the sound (non-looping). Stops any previous playback on this handle.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_audio_play(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.audio_buffers.len() { return; }
        let (_, stream_handle) = match s.audio_stream.as_ref() {
            Some(sh) => sh,
            None => return,
        };
        let data: Vec<u8> = s.audio_buffers[idx].as_ref().clone();
        if let Ok(source) = Decoder::new(Cursor::new(data)) {
            let sink = match Sink::try_new(stream_handle) {
                Ok(sink) => sink,
                Err(_) => return,
            };
            sink.append(source);
            s.audio_sinks[idx] = Some(sink);
        }
    });
}

/// Play the sound looping. Stops any previous playback on this handle.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_audio_play_looped(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.audio_buffers.len() { return; }
        let (_, stream_handle) = match s.audio_stream.as_ref() {
            Some(sh) => sh,
            None => return,
        };
        let data: Vec<u8> = s.audio_buffers[idx].as_ref().clone();
        if let Ok(source) = Decoder::new(Cursor::new(data)) {
            let sink = match Sink::try_new(stream_handle) {
                Ok(sink) => sink,
                Err(_) => return,
            };
            sink.append(source.repeat_infinite());
            s.audio_sinks[idx] = Some(sink);
        }
    });
}

/// Stop playback for the given handle.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_audio_stop(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.audio_sinks.len() { return; }
        if let Some(sink) = s.audio_sinks[idx].take() {
            sink.stop();
        }
    });
}

/// Set volume for the given handle (0.0 to 1.0, passed as f64 bits).
#[no_mangle]
pub unsafe extern "C" fn nex_engine_audio_set_volume(handle: i64, volume_bits: i64) {
    let volume = f64::from_bits(volume_bits as u64);
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.audio_sinks.len() { return; }
        if let Some(ref sink) = s.audio_sinks[idx] {
            sink.set_volume(volume as f32);
        }
    });
}

/// Returns 1 if the sound is currently playing, 0 otherwise.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_audio_is_playing(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.audio_sinks.len() { return 0; }
        match &s.audio_sinks[idx] {
            Some(sink) => if sink.empty() { 0 } else { 1 },
            None => 0,
        }
    })
}

/// Free the audio resource for the given handle.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_audio_free(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.audio_sinks.len() { return; }
        if let Some(sink) = s.audio_sinks[idx].take() {
            sink.stop();
        }
    });
}

// =======================================================================
// FFI: OBJ Model Loading (tobj)
// =======================================================================

/// Load an OBJ model from disk. Returns handle (index).
#[no_mangle]
pub unsafe extern "C" fn nex_engine_model_load(path_ptr: *const c_char) -> i64 {
    let path = cstr_to_string(path_ptr);
    let load_options = tobj::LoadOptions {
        triangulate: true,
        single_index: true,
        ..Default::default()
    };
    let (models, _materials) = match tobj::load_obj(&path, &load_options) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("[nex3d] model: failed to load '{}': {}", path, e);
            return -1;
        }
    };

    let mut vertices = Vec::new();
    for model in &models {
        let mesh = &model.mesh;
        let num_vertices = mesh.positions.len() / 3;
        for i in 0..num_vertices {
            let px = mesh.positions[i * 3] as f32;
            let py = mesh.positions[i * 3 + 1] as f32;
            let pz = mesh.positions[i * 3 + 2] as f32;

            let (nx, ny, nz) = if !mesh.normals.is_empty() {
                (
                    mesh.normals[i * 3] as f32,
                    mesh.normals[i * 3 + 1] as f32,
                    mesh.normals[i * 3 + 2] as f32,
                )
            } else {
                (0.0, 1.0, 0.0)
            };

            let (u, v) = if !mesh.texcoords.is_empty() {
                (
                    mesh.texcoords[i * 2] as f32,
                    1.0 - mesh.texcoords[i * 2 + 1] as f32, // flip V
                )
            } else {
                (0.0, 0.0)
            };

            vertices.push(Vertex {
                position: [px, py, pz],
                normal: [nx, ny, nz],
                uv: [u, v],
                color: [1.0, 1.0, 1.0],
            });
        }

        // Re-index using the face indices
        if !mesh.indices.is_empty() {
            let indexed_verts: Vec<Vertex> = mesh.indices.iter().map(|&idx| {
                let i = idx as usize;
                if i < vertices.len() {
                    vertices[i]
                } else {
                    Vertex {
                        position: [0.0, 0.0, 0.0],
                        normal: [0.0, 1.0, 0.0],
                        uv: [0.0, 0.0],
                        color: [1.0, 1.0, 1.0],
                    }
                }
            }).collect();
            vertices = indexed_verts;
        }
    }

    with_state(|s| {
        let handle = s.models.len() as i64;
        s.models.push(LoadedModel { vertices });
        handle
    })
}

/// Draw the model by pushing all its vertices into the current frame's vertex buffer.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_model_draw(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.models.len() { return; }
        let model = &s.models[idx];
        s.vertices.extend_from_slice(&model.vertices);
    });
}

/// Get vertex count for a loaded model.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_model_vertex_count(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.models.len() { return 0; }
        s.models[idx].vertices.len() as i64
    })
}

/// Free a loaded model.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_model_free(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx >= s.models.len() { return; }
        s.models[idx].vertices.clear();
    });
}

// =======================================================================
// FFI: RenderTarget2D
// =======================================================================

/// Create an off-screen render target with the given dimensions. Returns handle.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_rendertarget_create(w: i64, h: i64) -> i64 {
    with_state(|s| {
        let device = match s.device.as_ref() { Some(d) => d, None => return -1 };
        let width = w.max(1) as u32;
        let height = h.max(1) as u32;

        // Color texture (render target + texture binding for sampling)
        let color_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("rendertarget_color"),
            size: wgpu::Extent3d { width, height, depth_or_array_layers: 1 },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Bgra8UnormSrgb,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        });
        let color_view = color_texture.create_view(&wgpu::TextureViewDescriptor::default());

        // Depth texture
        let depth_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("rendertarget_depth"),
            size: wgpu::Extent3d { width, height, depth_or_array_layers: 1 },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Depth32Float,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        });
        let depth_view = depth_texture.create_view(&wgpu::TextureViewDescriptor::default());

        // Bind group for sampling this RT as a texture (uses same layout as textures)
        let layout = match s.texture_bind_group_layout.as_ref() {
            Some(l) => l,
            None => return -1,
        };
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("rendertarget_bind_group"),
            layout,
            entries: &[
                wgpu::BindGroupEntry { binding: 0, resource: wgpu::BindingResource::TextureView(&color_view) },
                wgpu::BindGroupEntry { binding: 1, resource: wgpu::BindingResource::Sampler(&sampler) },
            ],
        });

        let handle = s.render_targets.len() as i64;
        s.render_targets.push(RenderTargetData {
            color_texture,
            color_view,
            depth_view,
            bind_group,
            width,
            height,
        });
        handle
    })
}

/// Bind a render target so subsequent draws go to it instead of the screen.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_rendertarget_bind(handle: i64) {
    try_with_state(|s| {
        let idx = handle as usize;
        if idx < s.render_targets.len() {
            s.active_render_target = Some(idx);
        }
    });
}

/// Unbind the current render target (go back to drawing to the screen).
#[no_mangle]
pub unsafe extern "C" fn nex_engine_rendertarget_unbind() {
    try_with_state(|s| {
        s.active_render_target = None;
    });
}

/// Get the render target as a texture handle for drawing.
/// This registers it into the textures array and returns the texture handle.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_rendertarget_as_texture(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.render_targets.len() { return -1; }
        let _rt = &s.render_targets[idx];
        // Return a negative handle that encodes the RT index (offset by -1000)
        // The texture_bind function can check for this
        -(idx as i64) - 1000
    })
}

/// Get render target width.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_rendertarget_width(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.render_targets.len() { return 0; }
        s.render_targets[idx].width as i64
    })
}

/// Get render target height.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_rendertarget_height(handle: i64) -> i64 {
    with_state(|s| {
        let idx = handle as usize;
        if idx >= s.render_targets.len() { return 0; }
        s.render_targets[idx].height as i64
    })
}

/// Free a render target.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_rendertarget_free(_handle: i64) {
    // Render targets are stored in a Vec; freeing just drops the GPU resources
    // when the EngineState is dropped. For a real impl we'd use a slot allocator.
}

// =======================================================================
// FFI: Gamepad (gilrs)
// =======================================================================

/// Ensure gilrs is initialized.
fn ensure_gilrs(s: &mut EngineState) {
    if s.gilrs.is_none() {
        match Gilrs::new() {
            Ok(g) => { s.gilrs = Some(g); }
            Err(e) => { eprintln!("[nex3d] gamepad: gilrs init failed: {}", e); }
        }
    }
}

/// Map integer button code to gilrs Button enum.
fn map_button(code: i64) -> Option<GilrsButton> {
    match code {
        0 => Some(GilrsButton::South),        // A / Cross
        1 => Some(GilrsButton::East),         // B / Circle
        2 => Some(GilrsButton::West),         // X / Square
        3 => Some(GilrsButton::North),        // Y / Triangle
        4 => Some(GilrsButton::LeftTrigger),   // LB
        5 => Some(GilrsButton::RightTrigger),  // RB
        6 => Some(GilrsButton::LeftTrigger2),  // LT (digital)
        7 => Some(GilrsButton::RightTrigger2), // RT (digital)
        8 => Some(GilrsButton::Select),        // Back / Select
        9 => Some(GilrsButton::Start),
        10 => Some(GilrsButton::LeftThumb),
        11 => Some(GilrsButton::RightThumb),
        12 => Some(GilrsButton::DPadUp),
        13 => Some(GilrsButton::DPadDown),
        14 => Some(GilrsButton::DPadLeft),
        15 => Some(GilrsButton::DPadRight),
        _ => None,
    }
}

/// Map integer axis code to gilrs Axis enum.
fn map_axis(code: i64) -> Option<GilrsAxis> {
    match code {
        0 => Some(GilrsAxis::LeftStickX),
        1 => Some(GilrsAxis::LeftStickY),
        2 => Some(GilrsAxis::RightStickX),
        3 => Some(GilrsAxis::RightStickY),
        _ => None,
    }
}

/// Returns 1 if gamepad for `player` (0-based) is connected.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_gamepad_connected(player: i64) -> i64 {
    with_state(|s| {
        ensure_gilrs(s);
        let gilrs = match s.gilrs.as_mut() { Some(g) => g, None => return 0 };
        // Drain events so state is up-to-date
        while gilrs.next_event().is_some() {}
        let mut idx = 0i64;
        for (_id, gp) in gilrs.gamepads() {
            if idx == player && gp.is_connected() { return 1; }
            idx += 1;
        }
        0
    })
}

/// Returns 1 if the given button is currently pressed on gamepad `player`.
#[no_mangle]
pub unsafe extern "C" fn nex_engine_gamepad_button(player: i64, button: i64) -> i64 {
    with_state(|s| {
        ensure_gilrs(s);
        let gilrs = match s.gilrs.as_mut() { Some(g) => g, None => return 0 };
        while gilrs.next_event().is_some() {}
        let btn = match map_button(button) { Some(b) => b, None => return 0 };
        let mut idx = 0i64;
        for (_id, gp) in gilrs.gamepads() {
            if idx == player {
                return if gp.is_pressed(btn) { 1 } else { 0 };
            }
            idx += 1;
        }
        0
    })
}

/// Returns axis value (-1.0 to 1.0) for gamepad `player`, axis code.
/// Axis codes: 0=LeftX, 1=LeftY, 2=RightX, 3=RightY
#[no_mangle]
pub unsafe extern "C" fn nex_engine_gamepad_axis(player: i64, axis: i64) -> f64 {
    with_state(|s| {
        ensure_gilrs(s);
        let gilrs = match s.gilrs.as_mut() { Some(g) => g, None => return 0.0 };
        while gilrs.next_event().is_some() {}
        let ax = match map_axis(axis) { Some(a) => a, None => return 0.0 };
        let mut idx = 0i64;
        for (_id, gp) in gilrs.gamepads() {
            if idx == player {
                return gp.value(ax) as f64;
            }
            idx += 1;
        }
        0.0
    })
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


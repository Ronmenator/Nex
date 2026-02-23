use std::sync::Arc;

use super::renderer::{Rect, Renderer};
use super::style::Color;
use super::text_render::TextRenderer;

const BLIT_SHADER: &str = r#"
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

pub struct WgpuRenderer {
    device: wgpu::Device,
    queue: wgpu::Queue,
    surface: wgpu::Surface<'static>,
    surface_config: wgpu::SurfaceConfiguration,
    pipeline: wgpu::RenderPipeline,
    bind_group_layout: wgpu::BindGroupLayout,
    sampler: wgpu::Sampler,
    fb_texture: Option<wgpu::Texture>,
    bind_group: Option<wgpu::BindGroup>,

    framebuffer: Vec<u32>,
    fb_width: u32,
    fb_height: u32,

    text_renderer: TextRenderer,
}

impl WgpuRenderer {
    pub fn new(window: Arc<winit::window::Window>) -> Self {
        let size = window.inner_size();
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });

        let surface = instance.create_surface(window).expect("create surface");

        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .expect("request adapter");

        let (device, queue) = pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                label: Some("nex_ui"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::default(),
                memory_hints: wgpu::MemoryHints::default(),
            },
            None,
        ))
        .expect("request device");

        let surface_caps = surface.get_capabilities(&adapter);
        let surface_format = surface_caps
            .formats
            .iter()
            .find(|f| f.is_srgb())
            .copied()
            .unwrap_or(surface_caps.formats[0]);

        let surface_config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width.max(1),
            height: size.height.max(1),
            present_mode: wgpu::PresentMode::AutoVsync,
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &surface_config);

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("blit_shader"),
            source: wgpu::ShaderSource::Wgsl(BLIT_SHADER.into()),
        });

        let bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("blit_bgl"),
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

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("blit_pipeline_layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("blit_pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: surface_format,
                    blend: Some(wgpu::BlendState::REPLACE),
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

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("blit_sampler"),
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        Self {
            device,
            queue,
            surface,
            surface_config,
            pipeline,
            bind_group_layout,
            sampler,
            fb_texture: None,
            bind_group: None,
            framebuffer: Vec::new(),
            fb_width: 0,
            fb_height: 0,
            text_renderer: TextRenderer::new(),
        }
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        let w = width.max(1);
        let h = height.max(1);
        self.surface_config.width = w;
        self.surface_config.height = h;
        self.surface.configure(&self.device, &self.surface_config);
    }

    pub fn text_renderer_mut(&mut self) -> &mut TextRenderer {
        &mut self.text_renderer
    }

    fn ensure_fb(&mut self, width: u32, height: u32) {
        if self.fb_width != width || self.fb_height != height {
            self.fb_width = width;
            self.fb_height = height;
            self.framebuffer = vec![0xFF1E1E2E; (width * height) as usize];

            let texture = self.device.create_texture(&wgpu::TextureDescriptor {
                label: Some("framebuffer"),
                size: wgpu::Extent3d { width, height, depth_or_array_layers: 1 },
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: wgpu::TextureFormat::Rgba8UnormSrgb,
                usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                view_formats: &[],
            });

            let view = texture.create_view(&Default::default());
            self.bind_group = Some(self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("blit_bg"),
                layout: &self.bind_group_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(&view),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(&self.sampler),
                    },
                ],
            }));
            self.fb_texture = Some(texture);
        }
    }

    fn fb_fill_rect(&mut self, rect: Rect, color: Color) {
        let x0 = (rect.x as u32).min(self.fb_width);
        let y0 = (rect.y as u32).min(self.fb_height);
        let x1 = ((rect.x + rect.w) as u32).min(self.fb_width);
        let y1 = ((rect.y + rect.h) as u32).min(self.fb_height);
        let pixel = color_to_pixel(color);
        let alpha = color.a as u32;

        if alpha == 0 {
            return;
        }

        for y in y0..y1 {
            for x in x0..x1 {
                let idx = (y * self.fb_width + x) as usize;
                if idx < self.framebuffer.len() {
                    if alpha == 255 {
                        self.framebuffer[idx] = pixel;
                    } else {
                        self.framebuffer[idx] = blend_pixel(self.framebuffer[idx], pixel, alpha);
                    }
                }
            }
        }
    }
}

impl Renderer for WgpuRenderer {
    fn begin_frame(&mut self, width: u32, height: u32) {
        self.ensure_fb(width, height);
    }

    fn clear(&mut self, color: Color) {
        let pixel = color_to_pixel(color);
        self.framebuffer.fill(pixel);
    }

    fn fill_rect(&mut self, rect: Rect, color: Color, _border_radius: f32) {
        self.fb_fill_rect(rect, color);
    }

    fn stroke_rect(&mut self, rect: Rect, color: Color, line_width: f32, _border_radius: f32) {
        let lw = line_width.max(1.0);
        // Top
        self.fb_fill_rect(Rect::new(rect.x, rect.y, rect.w, lw), color);
        // Bottom
        self.fb_fill_rect(Rect::new(rect.x, rect.y + rect.h - lw, rect.w, lw), color);
        // Left
        self.fb_fill_rect(Rect::new(rect.x, rect.y, lw, rect.h), color);
        // Right
        self.fb_fill_rect(Rect::new(rect.x + rect.w - lw, rect.y, lw, rect.h), color);
    }

    fn draw_text(&mut self, text: &str, x: f32, y: f32, size: f32, color: Color) {
        let fb_w = self.fb_width;
        let fb_h = self.fb_height;
        let fb = &mut self.framebuffer;
        self.text_renderer.draw_text(text, x, y, size, color, 0.0, fb, fb_w, fb_h);
    }

    fn measure_text_width(&mut self, text: &str, size: f32) -> f32 {
        self.text_renderer.measure_width(text, size)
    }

    fn fill_circle(&mut self, cx: f32, cy: f32, radius: f32, color: Color) {
        let r2 = radius * radius;
        let x0 = ((cx - radius) as u32).min(self.fb_width);
        let y0 = ((cy - radius) as u32).min(self.fb_height);
        let x1 = ((cx + radius + 1.0) as u32).min(self.fb_width);
        let y1 = ((cy + radius + 1.0) as u32).min(self.fb_height);
        let pixel = color_to_pixel(color);
        let alpha = color.a as u32;

        for y in y0..y1 {
            for x in x0..x1 {
                let dx = x as f32 - cx;
                let dy = y as f32 - cy;
                if dx * dx + dy * dy <= r2 {
                    let idx = (y * self.fb_width + x) as usize;
                    if idx < self.framebuffer.len() {
                        if alpha == 255 {
                            self.framebuffer[idx] = pixel;
                        } else {
                            self.framebuffer[idx] = blend_pixel(self.framebuffer[idx], pixel, alpha);
                        }
                    }
                }
            }
        }
    }

    fn draw_line(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, line_width: f32, color: Color) {
        let dx = x2 - x1;
        let dy = y2 - y1;
        let len = (dx * dx + dy * dy).sqrt();
        if len < 0.001 {
            return;
        }
        let steps = len.ceil() as u32;
        let half_w = (line_width / 2.0).max(0.5);
        let pixel = color_to_pixel(color);

        for i in 0..=steps {
            let t = i as f32 / steps as f32;
            let cx = x1 + dx * t;
            let cy = y1 + dy * t;
            let px0 = ((cx - half_w) as u32).min(self.fb_width);
            let py0 = ((cy - half_w) as u32).min(self.fb_height);
            let px1 = ((cx + half_w + 1.0) as u32).min(self.fb_width);
            let py1 = ((cy + half_w + 1.0) as u32).min(self.fb_height);
            for py in py0..py1 {
                for px in px0..px1 {
                    let idx = (py * self.fb_width + px) as usize;
                    if idx < self.framebuffer.len() {
                        self.framebuffer[idx] = pixel;
                    }
                }
            }
        }
    }

    fn end_frame(&mut self) {
        if let Some(ref texture) = self.fb_texture {
            let rgba_data: Vec<u8> = self
                .framebuffer
                .iter()
                .flat_map(|&pixel| {
                    let r = ((pixel >> 16) & 0xFF) as u8;
                    let g = ((pixel >> 8) & 0xFF) as u8;
                    let b = (pixel & 0xFF) as u8;
                    let a = ((pixel >> 24) & 0xFF) as u8;
                    [r, g, b, a]
                })
                .collect();

            self.queue.write_texture(
                wgpu::ImageCopyTexture {
                    texture,
                    mip_level: 0,
                    origin: wgpu::Origin3d::ZERO,
                    aspect: wgpu::TextureAspect::All,
                },
                &rgba_data,
                wgpu::ImageDataLayout {
                    offset: 0,
                    bytes_per_row: Some(4 * self.fb_width),
                    rows_per_image: Some(self.fb_height),
                },
                wgpu::Extent3d {
                    width: self.fb_width,
                    height: self.fb_height,
                    depth_or_array_layers: 1,
                },
            );
        }
    }

    fn present(&mut self) {
        let Ok(output) = self.surface.get_current_texture() else { return };
        let view = output.texture.create_view(&Default::default());

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: Some("blit") });

        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("blit_pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            pass.set_pipeline(&self.pipeline);
            if let Some(ref bg) = self.bind_group {
                pass.set_bind_group(0, bg, &[]);
            }
            pass.draw(0..6, 0..1);
        }

        self.queue.submit(std::iter::once(encoder.finish()));
        output.present();
    }
}

fn color_to_pixel(c: Color) -> u32 {
    ((c.a as u32) << 24) | ((c.r as u32) << 16) | ((c.g as u32) << 8) | (c.b as u32)
}

fn blend_pixel(dst: u32, src: u32, src_alpha: u32) -> u32 {
    let inv = 255 - src_alpha;
    let sr = (src >> 16) & 0xFF;
    let sg = (src >> 8) & 0xFF;
    let sb = src & 0xFF;
    let dr = (dst >> 16) & 0xFF;
    let dg = (dst >> 8) & 0xFF;
    let db = dst & 0xFF;
    let r = (sr * src_alpha + dr * inv) / 255;
    let g = (sg * src_alpha + dg * inv) / 255;
    let b = (sb * src_alpha + db * inv) / 255;
    0xFF000000 | (r << 16) | (g << 8) | b
}

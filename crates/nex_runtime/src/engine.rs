//! nex3d engine — Game window and main loop for 2D/3D games.
//! Uses winit + wgpu for cross-platform rendering.

#[cfg(feature = "engine")]
mod impl_engine {
    use std::cell::RefCell;
    use std::os::raw::c_char;
    use std::sync::Arc;

    use pollster::FutureExt;
    use winit::application::ApplicationHandler;
    use winit::dpi::LogicalSize;
    use winit::event::WindowEvent;
    use winit::event_loop::{ActiveEventLoop, EventLoop};
    use winit::window::{Window, WindowAttributes};

    struct EngineState {
        title: String,
        width: u32,
        height: u32,
        running: bool,
        window: Option<Arc<Window>>,
        surface: Option<wgpu::Surface<'static>>,
        device: Option<wgpu::Device>,
        queue: Option<wgpu::Queue>,
        config: Option<wgpu::SurfaceConfiguration>,
    }

    thread_local! {
        static STATE: RefCell<Option<EngineState>> = RefCell::new(None);
    }

    fn with_state<R>(f: impl FnOnce(&mut EngineState) -> R) -> R {
        STATE.with(|cell| {
            let mut borrow = cell.borrow_mut();
            let state = borrow.as_mut().expect("Engine not initialized; call engine_window_create first");
            f(state)
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

    pub struct EngineApp;

    impl ApplicationHandler for EngineApp {
        fn resumed(&mut self, event_loop: &ActiveEventLoop) {
            with_state(|state| {
                if state.window.is_some() {
                    return;
                }
                let attrs = WindowAttributes::default()
                    .with_title(&state.title)
                    .with_inner_size(LogicalSize::new(state.width as f64, state.height as f64));
                let window = Arc::new(event_loop.create_window(attrs).expect("create window"));

                let instance = wgpu::Instance::new(wgpu::InstanceDescriptor::default());
                let surface = instance.create_surface(window.clone()).expect("create surface");
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
                let format = caps.formats.iter().find(|f| f.is_srgb()).copied().unwrap_or(caps.formats[0]);
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

                state.window = Some(window);
                state.surface = Some(unsafe { std::mem::transmute::<_, wgpu::Surface<'static>>(surface) });
                state.device = Some(device);
                state.queue = Some(queue);
                state.config = Some(config);
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
                WindowEvent::RedrawRequested => {
                    with_state(|state| {
                        if let (Some(ref surface), Some(ref device), Some(ref queue)) =
                            (&state.surface, &state.device, &state.queue)
                        {
                            if let Ok(frame) = surface.get_current_texture() {
                                let view = frame.texture.create_view(&Default::default());
                                let mut encoder = device.create_command_encoder(&Default::default());
                                {
                                    let _rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                                        label: Some("engine clear"),
                                        occlusion_query_set: None,
                                        color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                                            view: &view,
                                            resolve_target: None,
                                            ops: wgpu::Operations {
                                                load: wgpu::LoadOp::Clear(wgpu::Color {
                                                    r: 0.1,
                                                    g: 0.1,
                                                    b: 0.15,
                                                    a: 1.0,
                                                }),
                                                store: wgpu::StoreOp::Store,
                                            },
                                        })],
                                        depth_stencil_attachment: None,
                                        timestamp_writes: None,
                                    });
                                }
                                queue.submit(std::iter::once(encoder.finish()));
                                frame.present();
                            }
                        }
                    });
                }
                WindowEvent::Resized(size) => {
                    with_state(|state| {
                        if let (Some(ref surface), Some(ref device), Some(ref mut config)) =
                            (&state.surface, &state.device, &mut state.config)
                        {
                            config.width = size.width.max(1);
                            config.height = size.height.max(1);
                            surface.configure(device, config);
                        }
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

    #[no_mangle]
    pub unsafe extern "C" fn nex_engine_window_create(
        title: *const c_char,
        width: i64,
        height: i64,
    ) -> i64 {
        let config = EngineState {
            title: cstr_to_string(title),
            width: width.max(1) as u32,
            height: height.max(1) as u32,
            running: true,
            window: None,
            surface: None,
            device: None,
            queue: None,
            config: None,
        };
        STATE.with(|cell| {
            *cell.borrow_mut() = Some(config);
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
}

#[cfg(not(feature = "engine"))]
mod impl_engine {
    use std::os::raw::c_char;

    #[no_mangle]
    pub unsafe extern "C" fn nex_engine_window_create(
        _title: *const c_char,
        _width: i64,
        _height: i64,
    ) -> i64 {
        eprintln!("nex3d engine: build with --features engine to enable");
        0
    }

    #[no_mangle]
    pub unsafe extern "C" fn nex_engine_window_run(_handle: i64) {}

    #[no_mangle]
    pub unsafe extern "C" fn nex_engine_window_quit(_handle: i64) {}

    #[no_mangle]
    pub unsafe extern "C" fn nex_engine_window_destroy(_handle: i64) {}

    #[no_mangle]
    pub unsafe extern "C" fn nex_engine_window_is_running(_handle: i64) -> i64 {
        0
    }
}

pub use impl_engine::*;

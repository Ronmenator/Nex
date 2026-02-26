use std::collections::HashMap;
#[cfg(target_os = "windows")]
use std::os::windows::ffi::OsStrExt;

use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{
    AbiParam, Function, FuncRef, InstBuilder, MemFlags, Signature, UserFuncName,
};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use nexc_ir::{IrFunction, IrInstruction, IrModule, IrValue};
use nexc_type::Type;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// AOT compilation: IrModule -> object file bytes (.o / .obj).
pub fn generate_object(ir: &IrModule) -> Result<Vec<u8>, String> {
    generate_object_impl(ir, false)
}

/// AOT compilation for shared libraries: IrModule -> PIC object file bytes.
pub fn generate_shared_object(ir: &IrModule) -> Result<Vec<u8>, String> {
    generate_object_impl(ir, true)
}

fn generate_object_impl(ir: &IrModule, pic: bool) -> Result<Vec<u8>, String> {
    let flag_builder = build_settings(pic);
    let isa_builder = cranelift_codegen::isa::lookup(target_lexicon::Triple::host())
        .map_err(|e| format!("ISA lookup: {e}"))?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|e| format!("ISA build: {e}"))?;

    let obj_builder =
        ObjectBuilder::new(isa, ir.name.as_bytes().to_vec(), cranelift_module::default_libcall_names())
            .map_err(|e| format!("ObjectBuilder: {e}"))?;
    let mut module = ObjectModule::new(obj_builder);
    let mut ctx = module.make_context();

    let (_func_ids, _global_data) = translate_module(&mut module, ir, &mut ctx)?;

    let product = module.finish();
    let bytes = product.emit().map_err(|e| format!("emit object: {e}"))?;
    Ok(bytes)
}

/// JIT execution: compile IrModule in memory and call its `main` function.
/// Returns the process exit code.
///
/// `native_libs` is a list of paths to native dynamic libraries (.dll / .so)
/// whose exported symbols should be made available to JIT-compiled code.
pub fn jit_execute(
    ir: &IrModule,
    _args: &[String],
    native_libs: &[std::path::PathBuf],
) -> Result<i32, String> {
    use cranelift_jit::{JITBuilder, JITModule};

    let flag_builder = build_settings(false);
    let isa_builder = cranelift_codegen::isa::lookup(target_lexicon::Triple::host())
        .map_err(|e| format!("ISA lookup: {e}"))?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|e| format!("ISA build: {e}"))?;

    let mut jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    register_runtime_symbols(&mut jit_builder);
    register_native_libs(&mut jit_builder, native_libs)?;

    let mut module = JITModule::new(jit_builder);
    let mut ctx = module.make_context();

    let (func_ids, global_data) = translate_module(&mut module, ir, &mut ctx)?;

    module.finalize_definitions().map_err(|e| format!("JIT finalize: {e}"))?;

    // Patch reflection method pointers now that JIT code is finalized
    nex_runtime::reflect::patch_method_pointers(&|name| {
        func_ids
            .get(name)
            .map(|id| module.get_finalized_function(*id))
    });

    // Build global address map for lazy field resolution via reflection.
    // Types are registered at runtime (during main's init preamble), so we
    // cannot patch addresses eagerly.  Instead we store the full map and
    // resolve on first field access.
    {
        let mut addrs = std::collections::HashMap::new();
        for (name, &id) in &global_data {
            let (ptr, _) = module.get_finalized_data(id);
            addrs.insert(name.clone(), ptr as usize);
        }
        nex_runtime::reflect::set_global_addrs(addrs);
    }

    let main_name = if func_ids.contains_key("main") {
        "main"
    } else if func_ids.contains_key("nex_main") {
        "nex_main"
    } else {
        return Err("no main function found".into());
    };
    let main_id = func_ids[main_name];

    let code_ptr = module.get_finalized_function(main_id);

    // Check if main returns a value or void by looking at the IR.
    let main_ir = ir.functions.iter().find(|f| f.name == "main");
    let returns_value = main_ir
        .map(|f| !matches!(f.return_type, Type::Unit))
        .unwrap_or(false);

    if returns_value {
        let main_fn: extern "C" fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
        Ok(main_fn() as i32)
    } else {
        let main_fn: extern "C" fn() = unsafe { std::mem::transmute(code_ptr) };
        main_fn();
        Ok(0)
    }
}

// ---------------------------------------------------------------------------
// Settings
// ---------------------------------------------------------------------------

fn build_settings(pic: bool) -> settings::Builder {
    let mut b = settings::builder();
    b.set("opt_level", "speed").unwrap();
    b.set("is_pic", if pic { "true" } else { "false" }).unwrap();
    b
}

fn register_runtime_symbols(builder: &mut cranelift_jit::JITBuilder) {
    macro_rules! sym {
        ($name:ident) => {
            builder.symbol(stringify!($name), nex_runtime::$name as *const u8);
        };
    }
    sym!(nex_gc_alloc);
    sym!(nex_gc_collect);
    sym!(nex_gc_safepoint);
    sym!(nex_gc_write_barrier);
    sym!(nex_throw);
    sym!(nex_new_exception);

    sym!(nex_print_str);
    sym!(nex_println_str);
    sym!(nex_print_int);
    sym!(nex_println_int);
    sym!(nex_print_double);
    sym!(nex_println_double);
    sym!(nex_print_bool);
    sym!(nex_println_bool);
    sym!(nex_print_char);
    sym!(nex_println_char);

    sym!(nex_str_concat);
    sym!(nex_str_eq);
    sym!(nex_int_to_str);
    sym!(nex_double_to_str);
    sym!(nex_bool_to_str);
    sym!(nex_str_length);
    sym!(nex_str_substring);
    sym!(nex_dispose);

    // std.math
    sym!(nex_math_abs_int);
    sym!(nex_math_abs_float);
    sym!(nex_math_min_int);
    sym!(nex_math_max_int);
    sym!(nex_math_min_float);
    sym!(nex_math_max_float);
    sym!(nex_math_clamp_int);
    sym!(nex_math_clamp_float);
    sym!(nex_math_floor);
    sym!(nex_math_ceil);
    sym!(nex_math_round);
    sym!(nex_math_sqrt);
    sym!(nex_math_pow);
    sym!(nex_math_sin);
    sym!(nex_math_cos);
    sym!(nex_math_tan);
    sym!(nex_math_log);
    sym!(nex_math_log2);
    sym!(nex_math_log10);
    sym!(nex_math_exp);
    sym!(nex_math_random);
    sym!(nex_math_random_range);

    // std.string
    sym!(nex_str_split);
    sym!(nex_str_trim);
    sym!(nex_str_trim_start);
    sym!(nex_str_trim_end);
    sym!(nex_str_starts_with);
    sym!(nex_str_ends_with);
    sym!(nex_str_contains);
    sym!(nex_str_index_of);
    sym!(nex_str_replace);
    sym!(nex_str_to_upper);
    sym!(nex_str_to_lower);
    sym!(nex_str_repeat);
    sym!(nex_str_char_at);
    sym!(nex_str_reverse);
    sym!(nex_str_truncate);

    // std.convert
    sym!(nex_parse_int);
    sym!(nex_parse_float);
    sym!(nex_parse_bool);
    sym!(nex_char_to_str);
    sym!(nex_str_to_chars);

    // std.env
    sym!(nex_env_get);
    sym!(nex_env_set);
    sym!(nex_env_has);
    sym!(nex_env_args_count);
    sym!(nex_env_args_get);
    sym!(nex_env_cwd);

    // std.time
    sym!(nex_time_now_millis);
    sym!(nex_time_now_nanos);
    sym!(nex_time_sleep_millis);
    sym!(nex_time_elapsed_millis);

    // std.collections
    sym!(nex_list_new);
    sym!(nex_list_add);
    sym!(nex_list_get);
    sym!(nex_list_set);
    sym!(nex_list_length);
    sym!(nex_list_remove);
    sym!(nex_list_free);
    sym!(nex_list_sort_int);
    sym!(nex_list_reverse);
    sym!(nex_list_clear);
    sym!(nex_list_contains_int);
    sym!(nex_list_index_of_int);
    sym!(nex_list_filter);
    sym!(nex_list_map);
    sym!(nex_list_foreach);
    sym!(nex_list_contains_str);
    sym!(nex_set_new);
    sym!(nex_set_add);
    sym!(nex_set_contains);
    sym!(nex_set_remove);
    sym!(nex_set_size);
    sym!(nex_map_new);
    sym!(nex_map_put);
    sym!(nex_map_get);
    sym!(nex_map_contains);
    sym!(nex_map_remove);
    sym!(nex_map_size);
    sym!(nex_map_keys);
    sym!(nex_map_values);
    sym!(nex_map_free);

    // std.io
    sym!(nex_io_read_line);
    sym!(nex_io_file_exists);
    sym!(nex_io_file_delete);
    sym!(nex_io_file_rename);
    sym!(nex_io_file_copy);
    sym!(nex_io_file_size);
    sym!(nex_io_file_read_bytes);
    sym!(nex_io_file_write_bytes);
    sym!(nex_io_file_append);
    sym!(nex_io_mkdir);
    sym!(nex_io_list_dir);

    // std.path
    sym!(nex_path_join);
    sym!(nex_path_parent);
    sym!(nex_path_file_name);
    sym!(nex_path_extension);
    sym!(nex_path_stem);
    sym!(nex_path_is_absolute);
    sym!(nex_path_normalize);
    sym!(nex_path_separator);

    // std.json
    sym!(nex_json_parse);
    sym!(nex_json_stringify);
    sym!(nex_json_get_string);
    sym!(nex_json_get_int);
    sym!(nex_json_get_float);
    sym!(nex_json_get_bool);
    sym!(nex_json_new_object);
    sym!(nex_json_set_string);
    sym!(nex_json_set_int);
    sym!(nex_json_set_float);
    sym!(nex_json_set_bool);
    sym!(nex_json_set_null);
    sym!(nex_json_stringify_pretty);
    sym!(nex_json_free);

    // std.process
    sym!(nex_process_exec);
    sym!(nex_process_exec_output);
    sym!(nex_process_exit);
    sym!(nex_process_pid);
    sym!(nex_process_spawn);
    sym!(nex_process_wait);

    // std.net
    sym!(nex_net_tcp_connect);
    sym!(nex_net_tcp_close);
    sym!(nex_net_tcp_send);
    sym!(nex_net_tcp_recv);
    sym!(nex_net_tcp_listen);
    sym!(nex_net_tcp_accept);
    sym!(nex_net_udp_bind);
    sym!(nex_net_udp_close);
    sym!(nex_net_udp_send);
    sym!(nex_net_udp_recv);

    // std.threading
    sym!(nex_thread_spawn);
    sym!(nex_thread_join);
    sym!(nex_thread_sleep);
    sym!(nex_thread_current_id);
    sym!(nex_mutex_new);
    sym!(nex_mutex_lock);
    sym!(nex_mutex_unlock);
    sym!(nex_mutex_free);

    // std.async
    sym!(nex_task_spawn);
    sym!(nex_task_await);
    sym!(nex_task_is_done);

    // std.closure
    sym!(nex_closure_alloc);
    sym!(nex_closure_set_cap);
    sym!(nex_closure_get_cap);
    sym!(nex_closure_get_fn);

    // std.logging
    sym!(nex_log_debug);
    sym!(nex_log_info);
    sym!(nex_log_warn);
    sym!(nex_log_error);
    sym!(nex_log_set_level);
    sym!(nex_log_with_tag);

    // std.testing
    sym!(nex_assert);
    sym!(nex_assert_eq_int);
    sym!(nex_assert_eq_str);
    sym!(nex_assert_eq_float);
    sym!(nex_assert_eq_bool);
    sym!(nex_assert_ne_int);
    sym!(nex_assert_ne_str);
    sym!(nex_assert_true);

    // std.reflect
    sym!(nex_reflect_register_type);
    sym!(nex_reflect_add_base);
    sym!(nex_reflect_add_interface);
    sym!(nex_reflect_add_field);
    sym!(nex_reflect_add_method);
    sym!(nex_reflect_add_variant);
    sym!(nex_reflect_find_type);
    sym!(nex_reflect_type_name);
    sym!(nex_reflect_type_module);
    sym!(nex_reflect_type_kind);
    sym!(nex_reflect_type_field_count);
    sym!(nex_reflect_type_field_name);
    sym!(nex_reflect_type_field_type);
    sym!(nex_reflect_type_method_count);
    sym!(nex_reflect_type_method_name);
    sym!(nex_reflect_type_method_return_type);
    sym!(nex_reflect_type_implements);
    sym!(nex_reflect_type_is_reflectable);
    sym!(nex_reflect_type_count);
    sym!(nex_reflect_type_name_at);
    sym!(nex_reflect_type_interfaces);
    sym!(nex_reflect_set_method_ptr);
    sym!(nex_reflect_invoke);
    sym!(nex_reflect_create_instance);
    sym!(nex_reflect_reset);
    sym!(nex_reflect_getFieldString);
    sym!(nex_reflect_getFieldInt);
    sym!(nex_reflect_getFieldFloat);
    sym!(nex_reflect_getFieldBool);
    sym!(nex_reflect_setFieldString);
    sym!(nex_reflect_setFieldInt);
    sym!(nex_reflect_setFieldFloat);
    sym!(nex_reflect_setFieldBool);

    // NOTE: torch, crypto, http, regex symbols are loaded dynamically via
    // register_native_libs() from their respective native DLLs.
}

// ---------------------------------------------------------------------------
// Dynamic native library loading
// ---------------------------------------------------------------------------

#[cfg(target_os = "windows")]
unsafe extern "system" {
    fn LoadLibraryW(lpLibFileName: *const u16) -> *mut std::ffi::c_void;
}

/// Load native dynamic libraries (.dll / .so) and register all their exported
/// `nex_*` symbols with the JIT builder so Nex code can call them.
///
/// Each library is kept alive for the lifetime of the process (leaked via
/// `std::mem::forget`) because the JIT may reference function pointers at any
/// time during execution.
fn register_native_libs(
    builder: &mut cranelift_jit::JITBuilder,
    native_libs: &[std::path::PathBuf],
) -> Result<(), String> {
    // On Windows, prepend libtorch/lib to PATH so that nex_torch_native.dll
    // can find its libtorch dependencies (torch_cpu.dll, c10.dll,
    // torch_cuda.dll, c10_cuda.dll, etc.) regardless of the user's PATH.
    #[cfg(target_os = "windows")]
    {
        let libtorch_dir = std::env::var("LIBTORCH").ok()
            .map(std::path::PathBuf::from)
            .or_else(|| {
                // Auto-detect common libtorch locations.
                for p in &["D:\\libtorch", "C:\\libtorch"] {
                    let path = std::path::Path::new(p);
                    if path.join("lib").is_dir() {
                        return Some(path.to_path_buf());
                    }
                }
                None
            });
        if let Some(libtorch) = libtorch_dir {
            let lib_dir = libtorch.join("lib");
            if lib_dir.is_dir() {
                let current = std::env::var("PATH").unwrap_or_default();
                let lib_str = lib_dir.to_string_lossy();
                if !current.contains(&*lib_str) {
                    std::env::set_var("PATH", format!("{};{}", lib_str, current));
                }

                // Eagerly load torch_cuda.dll BEFORE any other libtorch
                // function is called.  If manual_seed() or any other torch
                // op runs first, libtorch initialises in CPU-only mode and
                // the CUDA hooks never register.
                let cuda_dll = lib_dir.join("torch_cuda.dll");
                if cuda_dll.exists() {
                    let wide: Vec<u16> = cuda_dll.as_os_str()
                        .encode_wide()
                        .chain(std::iter::once(0))
                        .collect();
                    unsafe { LoadLibraryW(wide.as_ptr()); }
                }
            }
        }
    }

    for lib_path in native_libs {
        // Strip \\?\ prefix that Windows canonicalize adds — it prevents
        // LoadLibraryExW from searching the DLL's directory for dependencies.
        let load_path = {
            let s = lib_path.to_string_lossy();
            if s.starts_with(r"\\?\") {
                std::path::PathBuf::from(&s[4..])
            } else {
                lib_path.clone()
            }
        };
        unsafe {
            let lib = libloading::Library::new(&load_path)
                .map_err(|e| format!("failed to load native library {}: {e}", load_path.display()))?;

            // Probe for all known exported symbol names.
            // The DLLs export `nex_*` extern "C" functions.  We iterate a
            // comprehensive list of symbol names and register any that the
            // library actually exports.
            for &name in NATIVE_SYMBOL_NAMES {
                if let Ok(sym) = lib.get::<*const u8>(name.as_bytes()) {
                    builder.symbol(name, *sym);
                }
            }

            // Keep the library loaded for the duration of the process.
            std::mem::forget(lib);
        }
    }
    Ok(())
}

/// All possible native symbol names that DLLs may export.
/// This list is probed at load time — missing symbols are silently skipped.
static NATIVE_SYMBOL_NAMES: &[&str] = &[
    // nex3d engine
    "nex_engine_window_create",
    "nex_engine_window_run",
    "nex_engine_window_quit",
    "nex_engine_window_destroy",
    "nex_engine_window_is_running",
    "nex_engine_clear_color",
    "nex_engine_set_update_fn",
    "nex_engine_window_width",
    "nex_engine_window_height",
    "nex_engine_key_down",
    "nex_engine_key_pressed",
    "nex_engine_key_released",
    "nex_engine_mouse_x",
    "nex_engine_mouse_y",
    "nex_engine_mouse_delta_x",
    "nex_engine_mouse_delta_y",
    "nex_engine_mouse_button_down",
    "nex_engine_mouse_button_pressed",
    "nex_engine_delta_time",
    "nex_engine_elapsed_time",
    "nex_engine_frame_count",
    "nex_engine_set_camera_pos",
    "nex_engine_set_camera_target",
    "nex_engine_set_camera_up",
    "nex_engine_set_perspective",
    "nex_engine_push_vertex",
    "nex_engine_push_vertex_lit",
    "nex_engine_draw_triangles",
    "nex_engine_set_ambient_color",
    "nex_engine_light_set_type",
    "nex_engine_light_set_enabled",
    "nex_engine_light_set_position",
    "nex_engine_light_set_direction",
    "nex_engine_light_set_color",
    "nex_engine_light_set_intensity",
    "nex_engine_light_set_range",
    "nex_engine_light_set_spot_angles",
    "nex_engine_clear_lights",
    "nex_engine_push_vertex_uv",
    "nex_engine_texture_load",
    "nex_engine_texture_bind",
    "nex_engine_texture_unbind",
    "nex_engine_texture_width",
    "nex_engine_texture_height",
    "nex_engine_spritebatch_begin",
    "nex_engine_spritebatch_end",
    "nex_engine_spritebatch_draw",
    "nex_engine_spritebatch_draw_src",
    "nex_engine_font_draw_text",
    "nex_engine_font_measure_text",
    "nex_engine_set_blend_mode",
    "nex_engine_set_cull_mode",
    "nex_engine_set_fill_mode",
    "nex_engine_set_depth_enabled",
    "nex_engine_mouse_scroll_delta",
    "nex_engine_audio_load",
    "nex_engine_audio_play",
    "nex_engine_audio_play_looped",
    "nex_engine_audio_stop",
    "nex_engine_audio_set_volume",
    "nex_engine_audio_is_playing",
    "nex_engine_audio_free",
    "nex_engine_model_load",
    "nex_engine_model_draw",
    "nex_engine_model_vertex_count",
    "nex_engine_model_free",
    "nex_engine_anim_model_load",
    "nex_engine_anim_model_draw",
    "nex_engine_anim_play",
    "nex_engine_anim_stop",
    "nex_engine_anim_pause",
    "nex_engine_anim_set_speed",
    "nex_engine_anim_set_looping",
    "nex_engine_anim_set_time",
    "nex_engine_anim_get_time",
    "nex_engine_anim_clip_count",
    "nex_engine_anim_clip_duration",
    "nex_engine_anim_joint_count",
    "nex_engine_anim_model_free",
    "nex_engine_rendertarget_create",
    "nex_engine_rendertarget_bind",
    "nex_engine_rendertarget_unbind",
    "nex_engine_rendertarget_as_texture",
    "nex_engine_rendertarget_width",
    "nex_engine_rendertarget_height",
    "nex_engine_rendertarget_free",
    "nex_engine_gamepad_connected",
    "nex_engine_gamepad_button",
    "nex_engine_gamepad_axis",
    "nex_engine_enable_ui_overlay",
    // nex UI
    "nex_ui_app_create",
    "nex_ui_app_set_backend",
    "nex_ui_app_set_root",
    "nex_ui_app_run",
    "nex_ui_app_quit",
    "nex_ui_app_destroy",
    "nex_ui_app_is_running",
    "nex_ui_app_render",
    "nex_ui_poll_event",
    "nex_ui_event_type",
    "nex_ui_event_widget",
    "nex_ui_text",
    "nex_ui_button",
    "nex_ui_text_input",
    "nex_ui_image",
    "nex_ui_checkbox",
    "nex_ui_slider",
    "nex_ui_row",
    "nex_ui_column",
    "nex_ui_stack",
    "nex_ui_scroll",
    "nex_ui_grid",
    "nex_ui_canvas",
    "nex_ui_add_child",
    "nex_ui_remove_child",
    "nex_ui_set_id",
    "nex_ui_get_id",
    "nex_ui_set_text",
    "nex_ui_get_text",
    "nex_ui_set_visible",
    "nex_ui_set_enabled",
    "nex_ui_get_value_float",
    "nex_ui_set_value_float",
    "nex_ui_set_width",
    "nex_ui_set_height",
    "nex_ui_set_min_width",
    "nex_ui_set_min_height",
    "nex_ui_set_padding",
    "nex_ui_set_padding_all",
    "nex_ui_set_margin",
    "nex_ui_set_bg_color",
    "nex_ui_set_fg_color",
    "nex_ui_set_font_size",
    "nex_ui_set_border",
    "nex_ui_set_border_radius",
    "nex_ui_set_flex_grow",
    "nex_ui_set_align_self",
    "nex_ui_set_justify_content",
    "nex_ui_set_align_items",
    "nex_ui_set_gap",
    "nex_ui_on_click",
    "nex_ui_on_change",
    "nex_ui_on_hover",
    "nex_ui_on_key",
    "nex_ui_canvas_fill_rect",
    "nex_ui_canvas_stroke_rect",
    "nex_ui_canvas_fill_circle",
    "nex_ui_canvas_draw_line",
    "nex_ui_canvas_draw_text",
    "nex_ui_canvas_clear",
    "nex_ui_dialog_message",
    "nex_ui_dialog_confirm",
    "nex_ui_dialog_open_file",
    "nex_ui_dialog_save_file",
    "nex_ui_bind",
    "nex_ui_unbind",
    "nex_ui_notify_changed",
    "nex_ui_bindings_clear",
    "nex_ui_overlay_init",
    "nex_ui_overlay_render",
    "nex_ui_overlay_mouse_move",
    "nex_ui_overlay_mouse_down",
    "nex_ui_overlay_mouse_up",
    "nex_ui_overlay_key_char",
    "nex_ui_overlay_key_name",
    "nex_ui_overlay_hit_test",
    "nex_ui_set_h_align",
    "nex_ui_set_v_align",
    "nex_ui_set_max_width",
    "nex_ui_set_max_height",
    "nex_ui_set_flex_shrink",
    "nex_ui_set_margin_all",
    "nex_ui_set_checked",
    "nex_ui_set_border_width",
    "nex_ui_set_border_color",
    // torch
    "nex_torch_tensor_zeros",
    "nex_torch_tensor_ones",
    "nex_torch_tensor_rand",
    "nex_torch_tensor_randn",
    "nex_torch_tensor_from_float_data",
    "nex_torch_tensor_arange",
    "nex_torch_tensor_eye",
    "nex_torch_tensor_free",
    "nex_torch_tensor_add",
    "nex_torch_tensor_sub",
    "nex_torch_tensor_mul",
    "nex_torch_tensor_div",
    "nex_torch_tensor_matmul",
    "nex_torch_tensor_neg",
    "nex_torch_tensor_exp",
    "nex_torch_tensor_log",
    "nex_torch_tensor_sum",
    "nex_torch_tensor_mean",
    "nex_torch_tensor_reshape",
    "nex_torch_tensor_transpose",
    "nex_torch_tensor_squeeze",
    "nex_torch_tensor_unsqueeze",
    "nex_torch_tensor_print",
    "nex_torch_tensor_shape_dim",
    "nex_torch_tensor_get_float",
    "nex_torch_tensor_item_float",
    "nex_torch_tensor_ndim",
    "nex_torch_tensor_numel",
    "nex_torch_cuda_is_available",
    "nex_torch_cuda_device_count",
    "nex_torch_tensor_to_device",
    "nex_torch_set_num_threads",
    "nex_torch_tensor_requires_grad",
    "nex_torch_tensor_backward",
    "nex_torch_tensor_grad",
    "nex_torch_no_grad",
    "nex_torch_nn_sequential_new",
    "nex_torch_nn_linear",
    "nex_torch_nn_conv2d",
    "nex_torch_nn_relu",
    "nex_torch_nn_sigmoid",
    "nex_torch_nn_tanh",
    "nex_torch_nn_softmax",
    "nex_torch_nn_dropout",
    "nex_torch_nn_batch_norm",
    "nex_torch_nn_to_device",
    "nex_torch_nn_forward",
    "nex_torch_nn_free",
    "nex_torch_loss_mse",
    "nex_torch_loss_cross_entropy",
    "nex_torch_loss_bce",
    "nex_torch_optim_sgd",
    "nex_torch_optim_adam",
    "nex_torch_optim_step",
    "nex_torch_optim_zero_grad",
    "nex_torch_optim_free",
    "nex_torch_model_save",
    "nex_torch_model_load",
    "nex_torch_jit_load",
    "nex_torch_jit_forward",
    "nex_torch_manual_seed",
    "nex_torch_version",
    "nex_torch_tensor_to_string",
    // torch — extended tensor ops
    "nex_torch_tensor_add_scalar",
    "nex_torch_tensor_mul_scalar",
    "nex_torch_tensor_div_scalar",
    "nex_torch_tensor_pow_scalar",
    "nex_torch_tensor_sqrt",
    "nex_torch_tensor_abs",
    "nex_torch_tensor_clamp",
    "nex_torch_tensor_softmax",
    "nex_torch_tensor_eq_scalar",
    "nex_torch_tensor_gt_scalar",
    "nex_torch_tensor_lt_scalar",
    "nex_torch_tensor_tril",
    "nex_torch_tensor_triu",
    "nex_torch_tensor_masked_fill",
    "nex_torch_tensor_where_self",
    "nex_torch_tensor_sum_dim",
    "nex_torch_tensor_mean_dim",
    "nex_torch_tensor_argmax",
    "nex_torch_tensor_max_dim",
    "nex_torch_tensor_min_dim",
    "nex_torch_tensor_cat",
    "nex_torch_tensor_narrow",
    "nex_torch_tensor_index_select",
    "nex_torch_tensor_flatten",
    "nex_torch_tensor_ones_like",
    "nex_torch_tensor_zeros_like",
    "nex_torch_tensor_full_like",
    "nex_torch_tensor_clone",
    "nex_torch_tensor_detach",
    "nex_torch_tensor_contiguous",
    "nex_torch_tensor_to_dtype_float",
    "nex_torch_tensor_to_dtype_long",
    // torch — extended NN layers
    "nex_torch_nn_layer_norm",
    "nex_torch_nn_gelu",
    "nex_torch_nn_embedding",
    // crypto
    "nex_crypto_sha256",
    "nex_crypto_sha512",
    "nex_crypto_md5",
    "nex_crypto_random_bytes",
    "nex_crypto_base64_encode",
    "nex_crypto_base64_decode",
    "nex_crypto_hmac_sha256",
    // http
    "nex_http_get",
    "nex_http_post",
    "nex_http_response_status",
    "nex_http_response_body",
    "nex_http_response_header",
    "nex_http_response_free",
    // regex
    "nex_regex_new",
    "nex_regex_is_match",
    "nex_regex_find",
    "nex_regex_replace",
    "nex_regex_free",
];

// ---------------------------------------------------------------------------
// Module-level translation (shared by AOT and JIT)
// ---------------------------------------------------------------------------

fn translate_module<M: Module>(
    module: &mut M,
    ir: &IrModule,
    ctx: &mut Context,
) -> Result<(HashMap<String, FuncId>, HashMap<String, cranelift_module::DataId>), String> {
    let mut func_ids: HashMap<String, FuncId> = HashMap::new();
    let mut string_data = StringPool::new();

    // Collect all string constants so we can emit data objects.
    for func in &ir.functions {
        collect_strings(func, &mut string_data.strings);
    }
    string_data.define_all(module)?;

    // Create mutable data objects for module-level global variables.
    let mut global_data: HashMap<String, cranelift_module::DataId> = HashMap::new();
    for gname in &ir.globals {
        let data_name = format!(".global.{}", gname.trim_start_matches('%'));
        let data_id = module
            .declare_data(&data_name, Linkage::Local, true, false)
            .map_err(|e| format!("declare global {data_name}: {e}"))?;
        let mut desc = DataDescription::new();
        desc.define_zeroinit(8);
        module
            .define_data(data_id, &desc)
            .map_err(|e| format!("define global {data_name}: {e}"))?;
        global_data.insert(gname.clone(), data_id);
    }

    // Forward-declare every Nex function so they can call each other.
    for func in &ir.functions {
        let sig = build_signature(module, func);
        let name = mangle_name(&func.name);
        let id = module
            .declare_function(&name, Linkage::Export, &sig)
            .map_err(|e| format!("declare {name}: {e}"))?;
        func_ids.insert(func.name.clone(), id);
    }

    // Declare external runtime functions used by Print / Call instructions.
    // Only declare symbols actually referenced by the IR to avoid unresolved
    // import errors when linking shared libraries on MSVC.
    let needed = collect_needed_imports(ir);
    declare_runtime_imports(module, &mut func_ids, Some(&needed))?;

    // Translate each function body.
    for func in &ir.functions {
        let fid = func_ids[&func.name];
        module.clear_context(ctx);
        ctx.func.signature = build_signature(module, func);
        ctx.func.name = UserFuncName::user(0, fid.as_u32());
        translate_function(
            module,
            &mut ctx.func,
            func,
            &func_ids,
            &string_data,
            &ir.functions,
            &ir.types,
            &global_data,
        )?;
        module
            .define_function(fid, ctx)
            .map_err(|e| format!("define {}: {e}", func.name))?;
    }

    Ok((func_ids, global_data))
}

// ---------------------------------------------------------------------------
// Register type tracking for print dispatch
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq)]
enum RegType {
    Int,
    Float,
    Bool,
    String,
    Unknown,
}

/// Runtime functions that return String (pointer); codegen must not coerce
/// their result with nex_int_to_str when used in string context.
fn runtime_func_return_type(name: &str) -> Option<RegType> {
    if name == "ui_get_text"
        || name == "nex_ui_get_text"
        || name.ends_with("::ui_get_text")
        || name.ends_with("::nex_ui_get_text")
        || name.ends_with(".ui_get_text")
        || name.ends_with(".nex_ui_get_text")
        || name.contains("ui_get_text")
    {
        return Some(RegType::String);
    }
    // Math / torch functions that return f64.
    match name {
        "abs_float" | "min_float" | "max_float" | "clamp_float"
        | "floor" | "ceil" | "round" | "sqrt" | "pow"
        | "sin" | "cos" | "tan" | "log" | "log2" | "log10" | "exp"
        | "math_random"
        | "parse_float"
        | "tensor_item_float" | "tensor_get_float" => return Some(RegType::Float),
        _ => {}
    }
    // Map functions
    match name {
        "nex_map_get" => return Some(RegType::String),
        _ => {}
    }
    // JSON functions
    match name {
        "nex_json_get_string" | "nex_json_stringify" | "nex_json_stringify_pretty" => {
            return Some(RegType::String);
        }
        "nex_json_get_float" => {
            return Some(RegType::Float);
        }
        _ => {}
    }
    // Reflection query functions
    match name {
        "nex_reflect_type_name" | "nex_reflect_type_module"
        | "nex_reflect_type_field_name" | "nex_reflect_type_field_type"
        | "nex_reflect_type_method_name" | "nex_reflect_type_method_return_type"
        | "nex_reflect_type_name_at" | "nex_reflect_type_interfaces"
        | "nex_reflect_getFieldString" => {
            return Some(RegType::String);
        }
        "nex_reflect_getFieldFloat" => {
            return Some(RegType::Float);
        }
        "nex_reflect_register_type" | "nex_reflect_find_type"
        | "nex_reflect_type_kind" | "nex_reflect_type_field_count"
        | "nex_reflect_type_method_count" | "nex_reflect_type_implements"
        | "nex_reflect_type_is_reflectable" | "nex_reflect_type_count"
        | "nex_reflect_invoke" | "nex_reflect_create_instance"
        | "nex_reflect_getFieldInt" | "nex_reflect_getFieldBool" => {
            return Some(RegType::Int);
        }
        _ => {}
    }
    None
}

/// Build a map from register names to their inferred type category.
/// This is used to select the correct runtime print function.
fn build_reg_type_map(
    ir_func: &IrFunction,
    ir_module_funcs: &[IrFunction],
    module_types: &HashMap<String, Type>,
    global_data: &HashMap<String, cranelift_module::DataId>,
) -> HashMap<String, RegType> {
    let func_return_types: HashMap<&str, RegType> = ir_module_funcs
        .iter()
        .map(|f| {
            let rt = match &f.return_type {
                Type::String => RegType::String,
                Type::Int | Type::Int64 | Type::Byte => RegType::Int,
                Type::Float | Type::Double => RegType::Float,
                Type::Bool => RegType::Bool,
                _ => RegType::Unknown,
            };
            (f.name.as_str(), rt)
        })
        .collect();

    let mut map = HashMap::new();
    // Seed with known module/global symbol types so globals like `%name_value`
    // keep their string-ness across functions that only read them.
    for (name, ty) in module_types {
        let reg_ty = match ty {
            Type::String => RegType::String,
            Type::Int | Type::Int64 | Type::Byte => RegType::Int,
            Type::Float | Type::Double => RegType::Float,
            Type::Bool => RegType::Bool,
            _ => RegType::Unknown,
        };
        map.insert(format!("%{name}"), reg_ty);
    }
    // Seed with function parameter types so that e.g. `println(msg)` inside a
    // user-defined function correctly dispatches to nex_print_str when msg: String.
    for (name, ty) in &ir_func.params {
        let reg_ty = match ty {
            Type::String => RegType::String,
            Type::Int | Type::Int64 | Type::Byte => RegType::Int,
            Type::Float | Type::Double => RegType::Float,
            Type::Bool => RegType::Bool,
            _ => RegType::Unknown,
        };
        map.insert(format!("%param.{name}"), reg_ty);
        map.insert(format!("%{name}"), reg_ty);
    }
    // Cross-function global type inference: scan ALL functions for globals stored
    // from string constants (e.g. class field initializers like `name_value = ""`
    // prepended into main). This lets functions that only READ a string global
    // (e.g. on_greet reading `name_value`) correctly type it as RegType::String
    // instead of defaulting to RegType::Int and calling nex_int_to_str on the pointer.
    for func in ir_module_funcs {
        for block in &func.blocks {
            for inst in &block.instructions {
                if let IrInstruction::Store { dst, src: IrValue::StringConst(_) } = inst {
                    if global_data.contains_key(dst.as_str()) {
                        map.insert(dst.clone(), RegType::String);
                    }
                }
            }
        }
    }
    for block in &ir_func.blocks {
        for inst in &block.instructions {
            match inst {
                IrInstruction::Store { dst, src } => {
                    let ty = match src {
                        IrValue::StringConst(_) => RegType::String,
                        IrValue::IntConst(_) => RegType::Int,
                        IrValue::FloatConst(_) => RegType::Float,
                        IrValue::BoolConst(_) => RegType::Bool,
                        IrValue::Register(r) => map.get(r).copied().unwrap_or(RegType::Unknown),
                        IrValue::NullConst => RegType::Unknown,
                    };
                    map.insert(dst.clone(), ty);
                }
                IrInstruction::Call { dst: Some(dst), target, .. } => {
                    let rt = func_return_types.get(target.as_str()).copied()
                        .or_else(|| {
                            func_return_types.iter()
                                .find(|(k, _)| k.ends_with(&format!("::{target}")))
                                .map(|(_, &v)| v)
                        })
                        .or_else(|| runtime_func_return_type(target.as_str()));
                    let rt = rt.unwrap_or(RegType::Unknown);
                    map.insert(dst.clone(), rt);
                }
                IrInstruction::BinOp { dst, op, lhs, rhs, .. } => {
                    let ty = match op.as_str() {
                        "bitand" | "bitor" | "bitxor" | "shl" | "shr" => RegType::Int,
                        "add" | "sub" | "mul" | "div" | "mod" => {
                            let lty = match lhs {
                                IrValue::FloatConst(_) => RegType::Float,
                                IrValue::StringConst(_) => RegType::String,
                                IrValue::Register(r) => map.get(r).copied().unwrap_or(RegType::Int),
                                _ => RegType::Int,
                            };
                            if op == "add" && lty == RegType::String {
                                RegType::String
                            } else if op == "add" {
                                let rty = match rhs {
                                    IrValue::StringConst(_) => RegType::String,
                                    IrValue::Register(r) => map.get(r).copied().unwrap_or(RegType::Int),
                                    _ => RegType::Int,
                                };
                                if rty == RegType::String { RegType::String } else { lty }
                            } else {
                                lty
                            }
                        }
                        _ => RegType::Bool,
                    };
                    map.insert(dst.clone(), ty);
                }
                IrInstruction::Load { dst, src } => {
                    // Propagate type from the source (e.g. %Node.name) to dst.
                    if let Some(&ty) = map.get(src) {
                        map.insert(dst.clone(), ty);
                    }
                }
                _ => {}
            }
        }
    }
    map
}

// ---------------------------------------------------------------------------
// Function-level translation
// ---------------------------------------------------------------------------

fn translate_function<M: Module>(
    module: &mut M,
    func: &mut Function,
    ir_func: &IrFunction,
    func_ids: &HashMap<String, FuncId>,
    strings: &StringPool,
    all_ir_funcs: &[IrFunction],
    module_types: &HashMap<String, Type>,
    global_data: &HashMap<String, cranelift_module::DataId>,
) -> Result<(), String> {
    let mut fb_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(func, &mut fb_ctx);

    // Pre-create a Cranelift block for every IrBlock.
    let mut block_map: HashMap<String, cranelift_codegen::ir::Block> = HashMap::new();
    let mut ordered_blocks: Vec<cranelift_codegen::ir::Block> = Vec::new();
    for ir_block in &ir_func.blocks {
        let blk = builder.create_block();
        block_map.insert(ir_block.label.clone(), blk);
        ordered_blocks.push(blk);
    }

    // Scan for jump/branch targets that reference labels without a corresponding
    // IrBlock (e.g. merge labels). Create extra blocks for them.
    let mut phantom_targets: Vec<String> = Vec::new();
    for ir_block in &ir_func.blocks {
        for inst in &ir_block.instructions {
            match inst {
                IrInstruction::Branch {
                    then_label,
                    else_label,
                    ..
                } => {
                    for label in [then_label, else_label] {
                        if !block_map.contains_key(label) && !phantom_targets.contains(label) {
                            phantom_targets.push(label.clone());
                        }
                    }
                }
                IrInstruction::Jump { target } => {
                    if !block_map.contains_key(target) && !phantom_targets.contains(target) {
                        phantom_targets.push(target.clone());
                    }
                }
                _ => {}
            }
        }
    }
    for label in &phantom_targets {
        let blk = builder.create_block();
        block_map.insert(label.clone(), blk);
        ordered_blocks.push(blk);
    }

    // Collect every register / variable name referenced in the function and
    // assign each a unique Cranelift Variable (skip globals — they use data objects).
    let var_names = collect_variable_names(ir_func);
    let mut var_map: HashMap<String, Variable> = HashMap::new();
    for name in &var_names {
        if global_data.contains_key(name) {
            continue;
        }
        let var = builder.declare_var(types::I64);
        var_map.insert(name.clone(), var);
    }

    // Entry block: append params.
    let entry = ordered_blocks[0];
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);

    // Initialize all variables to zero so Cranelift's SSA builder never sees
    // an undefined use. This must happen BEFORE parameter binding.
    let zero = builder.ins().iconst(types::I64, 0);
    for (_, &var) in &var_map {
        builder.def_var(var, zero);
    }

    // Bind function parameters to variables (overwrites the zero defaults).
    for (i, (name, _)) in ir_func.params.iter().enumerate() {
        let var_name = format!("%param.{name}");
        if let Some(&var) = var_map.get(&var_name) {
            let val = builder.block_params(entry)[i];
            builder.def_var(var, val);
        }
        let plain = format!("%{name}");
        if let Some(&var) = var_map.get(&plain) {
            let val = builder.block_params(entry)[i];
            builder.def_var(var, val);
        }
    }

    // Build register type map for print dispatch.
    let reg_types = build_reg_type_map(ir_func, all_ir_funcs, module_types, global_data);

    // Translate each IrBlock.
    let has_return_type = !matches!(ir_func.return_type, Type::Unit);

    for (blk_idx, ir_block) in ir_func.blocks.iter().enumerate() {
        let cl_block = block_map[&ir_block.label];

        if blk_idx != 0 {
            builder.switch_to_block(cl_block);
        }

        let mut terminated = false;
        for inst in &ir_block.instructions {
            if terminated {
                break;
            }
            terminated = emit_instruction(
                &mut builder,
                module,
                inst,
                &var_map,
                &block_map,
                func_ids,
                strings,
                has_return_type,
                &reg_types,
                global_data,
            );
        }

        if !terminated {
            if blk_idx + 1 < ir_func.blocks.len() {
                let next = block_map[&ir_func.blocks[blk_idx + 1].label];
                builder.ins().jump(next, &[]);
            } else if has_return_type {
                let z = builder.ins().iconst(types::I64, 0);
                builder.ins().return_(&[z]);
            } else {
                builder.ins().return_(&[]);
            }
        }
    }

    // Fill phantom blocks (merge/exit labels that don't have a corresponding
    // IrBlock).
    for label in &phantom_targets {
        let cl_block = block_map[label];
        builder.switch_to_block(cl_block);

        let target = find_fallthrough_for_phantom(label, ir_func, &block_map, &ordered_blocks);
        match target {
            Some(t) => {
                builder.ins().jump(t, &[]);
            }
            None => {
                if has_return_type {
                    let z = builder.ins().iconst(types::I64, 0);
                    builder.ins().return_(&[z]);
                } else {
                    builder.ins().return_(&[]);
                }
            }
        }
    }

    // Seal all blocks at once — this is safe because all predecessor edges are
    // now established.
    builder.seal_all_blocks();
    builder.finalize();
    Ok(())
}

/// For a phantom label (merge/exit label referenced but not defined as its own
/// IrBlock), find the block that should logically follow. The merge point is
/// the block right after the LAST IrBlock that jumps to this label.
fn find_fallthrough_for_phantom(
    label: &str,
    ir_func: &IrFunction,
    block_map: &HashMap<String, cranelift_codegen::ir::Block>,
    _ordered_blocks: &[cranelift_codegen::ir::Block],
) -> Option<cranelift_codegen::ir::Block> {
    let mut last_ref_idx: Option<usize> = None;
    for (i, ir_block) in ir_func.blocks.iter().enumerate() {
        let references_label = ir_block.instructions.iter().any(|inst| match inst {
            IrInstruction::Branch {
                then_label,
                else_label,
                ..
            } => then_label == label || else_label == label,
            IrInstruction::Jump { target } => target == label,
            _ => false,
        });
        if references_label {
            last_ref_idx = Some(i);
        }
    }
    if let Some(idx) = last_ref_idx {
        if idx + 1 < ir_func.blocks.len() {
            return Some(block_map[&ir_func.blocks[idx + 1].label]);
        }
    }
    // Fallback: the very last real block.
    ir_func.blocks.last().map(|b| block_map[&b.label])
}

// ---------------------------------------------------------------------------
// Instruction emission – returns true when the instruction is a terminator.
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
fn emit_instruction<M: Module>(
    builder: &mut FunctionBuilder,
    module: &mut M,
    inst: &IrInstruction,
    vars: &HashMap<String, Variable>,
    blocks: &HashMap<String, cranelift_codegen::ir::Block>,
    func_ids: &HashMap<String, FuncId>,
    strings: &StringPool,
    has_return_type: bool,
    reg_types: &HashMap<String, RegType>,
    global_data: &HashMap<String, cranelift_module::DataId>,
) -> bool {
    match inst {
        IrInstruction::Nop | IrInstruction::EmitDiag { .. } => false,

        IrInstruction::Return(None) => {
            if has_return_type {
                let zero = builder.ins().iconst(types::I64, 0);
                builder.ins().return_(&[zero]);
            } else {
                builder.ins().return_(&[]);
            }
            true
        }
        IrInstruction::Return(Some(val)) => {
            let v = resolve_value(builder, module, val, vars, strings, func_ids, global_data);
            builder.ins().return_(&[v]);
            true
        }

        IrInstruction::Allocate { dst, .. } => {
            let _ = dst;
            false
        }

        IrInstruction::Store { dst, src } => {
            let v = resolve_value(builder, module, src, vars, strings, func_ids, global_data);
            set_var(builder, module, dst, v, vars, global_data);
            false
        }

        IrInstruction::Load { dst, src } => {
            let v = resolve_value(builder, module, &IrValue::Register(src.clone()), vars, strings, func_ids, global_data);
            set_var(builder, module, dst, v, vars, global_data);
            false
        }

        IrInstruction::BinOp { dst, op, lhs, rhs } => {
            let lhs_type = irvalue_reg_type(lhs, reg_types);
            let rhs_type = irvalue_reg_type(rhs, reg_types);
            let l = resolve_value(builder, module, lhs, vars, strings, func_ids, global_data);
            let r = resolve_value(builder, module, rhs, vars, strings, func_ids, global_data);
            let result = match op.as_str() {
                "add" if lhs_type == RegType::String || rhs_type == RegType::String => {
                    let lstr = coerce_to_str(builder, module, l, lhs_type, func_ids);
                    let rstr = coerce_to_str(builder, module, r, rhs_type, func_ids);
                    call_runtime2(builder, module, func_ids, "nex_str_concat", lstr, rstr)
                }
                "add" => builder.ins().iadd(l, r),
                "sub" => builder.ins().isub(l, r),
                "mul" => builder.ins().imul(l, r),
                "div" => builder.ins().sdiv(l, r),
                "mod" => builder.ins().srem(l, r),
                "eq" if lhs_type == RegType::String || rhs_type == RegType::String => {
                    call_runtime2(builder, module, func_ids, "nex_str_eq", l, r)
                }
                "eq" => {

                    let c = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::Equal, l, r);
                    builder.ins().uextend(types::I64, c)
                }
                "ne" if lhs_type == RegType::String || rhs_type == RegType::String => {
                    let eq = call_runtime2(builder, module, func_ids, "nex_str_eq", l, r);
                    let one = builder.ins().iconst(types::I64, 1);
                    builder.ins().bxor(eq, one)
                }
                "ne" => {
                    let c = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::NotEqual, l, r);
                    builder.ins().uextend(types::I64, c)
                }
                "lt" => {
                    let c = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::SignedLessThan, l, r);
                    builder.ins().uextend(types::I64, c)
                }
                "le" => {
                    let c = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::SignedLessThanOrEqual, l, r);
                    builder.ins().uextend(types::I64, c)
                }
                "gt" => {
                    let c = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::SignedGreaterThan, l, r);
                    builder.ins().uextend(types::I64, c)
                }
                "ge" => {
                    let c = builder.ins().icmp(cranelift_codegen::ir::condcodes::IntCC::SignedGreaterThanOrEqual, l, r);
                    builder.ins().uextend(types::I64, c)
                }
                "and" => builder.ins().band(l, r),
                "or" => builder.ins().bor(l, r),
                "bitand" => builder.ins().band(l, r),
                "bitor" => builder.ins().bor(l, r),
                "bitxor" => builder.ins().bxor(l, r),
                "shl" => builder.ins().ishl(l, r),
                "shr" => builder.ins().sshr(l, r),
                _ => builder.ins().iadd(l, r),
            };
            set_var(builder, module, dst, result, vars, global_data);
            false
        }

        IrInstruction::UnaryOp { dst, op, operand } => {
            let v = resolve_value(builder, module, operand, vars, strings, func_ids, global_data);
            let result = match op.as_str() {
                "neg" => builder.ins().ineg(v),
                "not" => {
                    let one = builder.ins().iconst(types::I64, 1);
                    builder.ins().bxor(v, one)
                }
                "bitnot" => builder.ins().bnot(v),
                _ => v,
            };
            set_var(builder, module, dst, result, vars, global_data);
            false
        }

        IrInstruction::Branch {
            cond,
            then_label,
            else_label,
        } => {
            let cv = resolve_value(builder, module, cond, vars, strings, func_ids, global_data);
            let then_blk = blocks[then_label];
            let else_blk = blocks[else_label];
            builder.ins().brif(cv, then_blk, &[], else_blk, &[]);
            true
        }

        IrInstruction::Jump { target } => {
            let blk = blocks[target];
            builder.ins().jump(blk, &[]);
            true
        }

        IrInstruction::Call { dst, target, args } => {
            // Intercept print/println and dispatch to typed runtime functions.
            if target == "print" || target == "println" {
                let is_ln = target == "println";
                if args.is_empty() {
                    // print() → no-op; println() → newline
                    if is_ln {
                        if let Some(&fid) = func_ids.get("nex_println_str") {
                            let func_ref = module.declare_func_in_func(fid, builder.func);
                            let null = builder.ins().iconst(types::I64, 0);
                            builder.ins().call(func_ref, &[null]);
                        }
                    }
                } else if args.len() == 1 {
                    // Single arg: dispatch to typed runtime (int, float, bool, str)
                    let rt_name = pick_print_runtime(target, &args[0], reg_types);
                    let arg_val = resolve_value(builder, module, &args[0], vars, strings, func_ids, global_data);
                    if let Some(&fid) = func_ids.get(rt_name) {
                        let func_ref = module.declare_func_in_func(fid, builder.func);
                        let coerced = coerce_call_args(builder, func_ref, &[arg_val]);
                        builder.ins().call(func_ref, &coerced);
                    }
                } else {
                    // Multi-arg: coerce each to string, join with " ", print result
                    let space = resolve_value(builder, module, &IrValue::StringConst(" ".into()), vars, strings, func_ids, global_data);
                    let mut accum = {
                        let v = resolve_value(builder, module, &args[0], vars, strings, func_ids, global_data);
                        let ty = irvalue_reg_type(&args[0], reg_types);
                        coerce_to_str(builder, module, v, ty, func_ids)
                    };
                    for arg in &args[1..] {
                        // append space
                        accum = call_runtime2(builder, module, func_ids, "nex_str_concat", accum, space);
                        // append next arg as string
                        let v = resolve_value(builder, module, arg, vars, strings, func_ids, global_data);
                        let ty = irvalue_reg_type(arg, reg_types);
                        let s = coerce_to_str(builder, module, v, ty, func_ids);
                        accum = call_runtime2(builder, module, func_ids, "nex_str_concat", accum, s);
                    }
                    let rt_name = if is_ln { "nex_println_str" } else { "nex_print_str" };
                    if let Some(&fid) = func_ids.get(rt_name) {
                        let func_ref = module.declare_func_in_func(fid, builder.func);
                        builder.ins().call(func_ref, &[accum]);
                    }
                }
                if let Some(d) = dst {
                    let z = builder.ins().iconst(types::I64, 0);
                    set_var(builder, module, d, z, vars, global_data);
                }
                return false;
            }

            // Route stdlib function calls to their runtime symbols.
            if let Some(rt_name) = stdlib_function_name(target) {
                let arg_vals: Vec<cranelift_codegen::ir::Value> = args
                    .iter()
                    .map(|a| resolve_value(builder, module, a, vars, strings, func_ids, global_data))
                    .collect();
                if let Some(&fid) = func_ids.get(rt_name) {
                    let func_ref = module.declare_func_in_func(fid, builder.func);
                    let coerced = coerce_call_args(builder, func_ref, &arg_vals);
                    let call = builder.ins().call(func_ref, &coerced);
                    if let Some(d) = dst {
                        let rv = coerce_return_value(builder, call);
                        set_var(builder, module, d, rv, vars, global_data);
                    }
                }
                return false;
            }

            // Handle __func_addr__<name>: get function pointer as i64
            if target.starts_with("__func_addr__") {
                let real_name = &target["__func_addr__".len()..];
                let resolved_fid = func_ids.get(real_name).copied().or_else(|| {
                    func_ids.iter()
                        .find(|(k, _)| k.ends_with(&format!("::{real_name}")))
                        .map(|(_, &id)| id)
                });
                if let Some(fid) = resolved_fid {
                    let func_ref = module.declare_func_in_func(fid, builder.func);
                    let addr = builder.ins().func_addr(types::I64, func_ref);
                    if let Some(d) = dst {
                        set_var(builder, module, d, addr, vars, global_data);
                    }
                } else if let Some(d) = dst {
                    let z = builder.ins().iconst(types::I64, 0);
                    set_var(builder, module, d, z, vars, global_data);
                }
                return false;
            }

            let arg_vals: Vec<cranelift_codegen::ir::Value> = args
                .iter()
                .map(|a| resolve_value(builder, module, a, vars, strings, func_ids, global_data))
                .collect();

            let resolved_fid = func_ids.get(target).copied().or_else(|| {
                func_ids.iter()
                    .find(|(k, _)| k.ends_with(&format!("::{target}")))
                    .map(|(_, &id)| id)
            });

            if let Some(fid) = resolved_fid {
                let func_ref = module.declare_func_in_func(fid, builder.func);
                let coerced = coerce_call_args(builder, func_ref, &arg_vals);
                let call = builder.ins().call(func_ref, &coerced);
                if let Some(d) = dst {
                    let rv = coerce_return_value(builder, call);
                    set_var(builder, module, d, rv, vars, global_data);
                }
            } else {
                // Unknown function — check if it's a variable holding a closure
                // (lambda/closure call via call_indirect).
                // Closure layout: [func_ptr, n_captures, cap0, ...]
                // The lifted function takes __env (closure ptr) as first arg.
                let var_name = format!("%{target}");
                if vars.contains_key(&var_name) {
                    let closure_ptr = resolve_value(builder, module, &IrValue::Register(var_name), vars, strings, func_ids, global_data);
                    // Load func_ptr from closure[0]
                    let func_ptr = builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), closure_ptr, 0);
                    // Build a call signature: __env (i64) + all original args, returns i64
                    let mut sig = module.make_signature();
                    sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // __env
                    for _ in &arg_vals {
                        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
                    }
                    sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I64));
                    let sig_ref = builder.import_signature(sig);
                    // Prepend closure_ptr as __env argument
                    let mut all_args = vec![closure_ptr];
                    all_args.extend(arg_vals);
                    let call = builder.ins().call_indirect(sig_ref, func_ptr, &all_args);
                    if let Some(d) = dst {
                        let results = builder.inst_results(call);
                        let rv = if !results.is_empty() {
                            results[0]
                        } else {
                            builder.ins().iconst(types::I64, 0)
                        };
                        set_var(builder, module, d, rv, vars, global_data);
                    }
                } else if let Some(d) = dst {
                    let z = builder.ins().iconst(types::I64, 0);
                    set_var(builder, module, d, z, vars, global_data);
                }
            }
            false
        }

        IrInstruction::VCall {
            dst,
            slot: _,
            this_ptr: _,
            args: _,
        } => {
            if let Some(d) = dst {
                let z = builder.ins().iconst(types::I64, 0);
                set_var(builder, module, d, z, vars, global_data);
            }
            false
        }

        IrInstruction::Print { value } => {
            let v = resolve_value(builder, module, value, vars, strings, func_ids, global_data);
            let ty = irvalue_reg_type(value, reg_types);
            let callee = match ty {
                RegType::String => "nex_println_str",
                RegType::Float => "nex_println_double",
                RegType::Bool => "nex_println_bool",
                _ => "nex_println_int",
            };
            if let Some(&fid) = func_ids.get(callee) {
                let func_ref = module.declare_func_in_func(fid, builder.func);
                let coerced = coerce_call_args(builder, func_ref, &[v]);
                builder.ins().call(func_ref, &coerced);
            }
            false
        }

        IrInstruction::MemberAccess {
            dst,
            receiver,
            field: _,
        } => {
            let v = resolve_value(builder, module, receiver, vars, strings, func_ids, global_data);
            set_var(builder, module, dst, v, vars, global_data);
            false
        }
    }
}

// ---------------------------------------------------------------------------
// Variable set helper – stores to global data object or local SSA variable.
// ---------------------------------------------------------------------------

fn set_var<M: Module>(
    builder: &mut FunctionBuilder,
    module: &mut M,
    name: &str,
    value: cranelift_codegen::ir::Value,
    vars: &HashMap<String, Variable>,
    global_data: &HashMap<String, cranelift_module::DataId>,
) {
    if let Some(&data_id) = global_data.get(name) {
        let gv = module.declare_data_in_func(data_id, builder.func);
        let addr = builder.ins().global_value(types::I64, gv);
        builder.ins().store(cranelift_codegen::ir::MemFlags::new(), value, addr, 0);
    } else if let Some(&var) = vars.get(name) {
        builder.def_var(var, value);
    }
}

// ---------------------------------------------------------------------------
// Value resolution
// ---------------------------------------------------------------------------

fn resolve_value<M: Module>(
    builder: &mut FunctionBuilder,
    module: &mut M,
    val: &IrValue,
    vars: &HashMap<String, Variable>,
    strings: &StringPool,
    func_ids: &HashMap<String, FuncId>,
    global_data: &HashMap<String, cranelift_module::DataId>,
) -> cranelift_codegen::ir::Value {
    match val {
        IrValue::IntConst(v) => builder.ins().iconst(types::I64, *v),
        IrValue::FloatConst(v) => {
            let bits = (*v).to_bits() as i64;
            builder.ins().iconst(types::I64, bits)
        }
        IrValue::BoolConst(v) => builder.ins().iconst(types::I64, if *v { 1 } else { 0 }),
        IrValue::NullConst => builder.ins().iconst(types::I64, 0),
        IrValue::StringConst(s) => {
            if let Some(&data_id) = strings.map.get(s) {
                let gv = module.declare_data_in_func(data_id, builder.func);
                builder.ins().global_value(types::I64, gv)
            } else {
                builder.ins().iconst(types::I64, 0)
            }
        }
        IrValue::Register(name) => {
            if let Some(&data_id) = global_data.get(name) {
                let gv = module.declare_data_in_func(data_id, builder.func);
                let addr = builder.ins().global_value(types::I64, gv);
                builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::new(), addr, 0)
            } else if let Some(&var) = vars.get(name) {
                builder.use_var(var)
            } else {
                let plain = name.strip_prefix('%').unwrap_or(name);
                let fid_lookup = func_ids.get(plain).copied().or_else(|| {
                    func_ids.iter()
                        .find(|(k, _)| k.ends_with(&format!("::{plain}")))
                        .map(|(_, &id)| id)
                });
                if let Some(fid) = fid_lookup {
                    let func_ref = module.declare_func_in_func(fid, builder.func);
                    builder.ins().func_addr(types::I64, func_ref)
                } else {
                    builder.ins().iconst(types::I64, 0)
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// String pool – data objects for string constants
// ---------------------------------------------------------------------------

struct StringPool {
    strings: Vec<String>,
    map: HashMap<String, cranelift_module::DataId>,
}

impl StringPool {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn define_all<M: Module>(&mut self, module: &mut M) -> Result<(), String> {
        for (i, s) in self.strings.iter().enumerate() {
            let name = format!(".str.{i}");
            let data_id = module
                .declare_data(&name, Linkage::Local, false, false)
                .map_err(|e| format!("declare data {name}: {e}"))?;
            let mut desc = DataDescription::new();
            let mut bytes = s.as_bytes().to_vec();
            bytes.push(0); // null terminator
            desc.define(bytes.into_boxed_slice());
            module
                .define_data(data_id, &desc)
                .map_err(|e| format!("define data {name}: {e}"))?;
            self.map.insert(s.clone(), data_id);
        }
        Ok(())
    }
}

fn collect_strings(func: &IrFunction, pool: &mut Vec<String>) {
    for block in &func.blocks {
        for inst in &block.instructions {
            let mut push = |s: &String| {
                if !pool.contains(s) {
                    pool.push(s.clone());
                }
            };
            match inst {
                IrInstruction::Store {
                    src: IrValue::StringConst(s),
                    ..
                }
                | IrInstruction::Print {
                    value: IrValue::StringConst(s),
                } => push(s),
                IrInstruction::Call { target, args, .. } => {
                    // Multi-arg print/println needs a " " separator string.
                    if (target == "print" || target == "println") && args.len() > 1 {
                        push(&" ".to_string());
                    }
                    for a in args {
                        if let IrValue::StringConst(s) = a {
                            push(s);
                        }
                    }
                }
                IrInstruction::Return(Some(IrValue::StringConst(s))) => push(s),
                IrInstruction::BinOp { lhs, rhs, .. } => {
                    if let IrValue::StringConst(s) = lhs {
                        push(s);
                    }
                    if let IrValue::StringConst(s) = rhs {
                        push(s);
                    }
                }
                _ => {}
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Infer the RegType of an IrValue using the reg_types map.
fn irvalue_reg_type(val: &IrValue, reg_types: &HashMap<String, RegType>) -> RegType {
    match val {
        IrValue::StringConst(_) => RegType::String,
        IrValue::IntConst(_) => RegType::Int,
        IrValue::FloatConst(_) => RegType::Float,
        IrValue::BoolConst(_) => RegType::Bool,
        IrValue::NullConst => RegType::Int,
        IrValue::Register(r) => reg_types.get(r).copied().unwrap_or(RegType::Int),
    }
}

/// Convert a Cranelift value to a string pointer by calling the appropriate
/// runtime conversion function. If the value is already a string, return it
/// unchanged.
fn coerce_to_str<M: Module>(
    builder: &mut FunctionBuilder,
    module: &mut M,
    val: cranelift_codegen::ir::Value,
    ty: RegType,
    func_ids: &HashMap<String, FuncId>,
) -> cranelift_codegen::ir::Value {
    let rt_name = match ty {
        RegType::String => return val,
        RegType::Float => "nex_double_to_str",
        RegType::Bool => "nex_bool_to_str",
        _ => "nex_int_to_str",
    };
    call_runtime1(builder, module, func_ids, rt_name, val)
}

/// Call a single-argument runtime function that returns a value.
fn call_runtime1<M: Module>(
    builder: &mut FunctionBuilder,
    module: &mut M,
    func_ids: &HashMap<String, FuncId>,
    name: &str,
    arg: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    if let Some(&fid) = func_ids.get(name) {
        let func_ref = module.declare_func_in_func(fid, builder.func);
        let coerced = coerce_call_args(builder, func_ref, &[arg]);
        let call = builder.ins().call(func_ref, &coerced);
        coerce_return_value(builder, call)
    } else {
        builder.ins().iconst(types::I64, 0)
    }
}

/// Call a two-argument runtime function that returns a value.
fn call_runtime2<M: Module>(
    builder: &mut FunctionBuilder,
    module: &mut M,
    func_ids: &HashMap<String, FuncId>,
    name: &str,
    a: cranelift_codegen::ir::Value,
    b: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    if let Some(&fid) = func_ids.get(name) {
        let func_ref = module.declare_func_in_func(fid, builder.func);
        let coerced = coerce_call_args(builder, func_ref, &[a, b]);
        let call = builder.ins().call(func_ref, &coerced);
        coerce_return_value(builder, call)
    } else {
        builder.ins().iconst(types::I64, 0)
    }
}

/// Bitcast call arguments from I64 to F64 where the function signature expects
/// F64. This is needed because the Nex IR represents all values as I64 bit
/// patterns, but the C ABI on Windows x64 passes floats in xmm registers.
fn coerce_call_args(
    builder: &mut FunctionBuilder,
    func_ref: FuncRef,
    args: &[cranelift_codegen::ir::Value],
) -> Vec<cranelift_codegen::ir::Value> {
    let sig = builder.func.dfg.ext_funcs[func_ref].signature;
    // Collect param types up front to avoid borrowing builder.func across the loop.
    let param_types: Vec<cranelift_codegen::ir::Type> = builder.func.dfg.signatures[sig]
        .params
        .iter()
        .map(|p| p.value_type)
        .collect();
    args.iter()
        .enumerate()
        .map(|(i, &v)| {
            if i < param_types.len()
                && param_types[i] == types::F64
                && builder.func.dfg.value_type(v) == types::I64
            {
                builder.ins().bitcast(types::F64, MemFlags::new(), v)
            } else {
                v
            }
        })
        .collect()
}

/// If a call returns F64, bitcast the result back to I64 for the Nex IR.
fn coerce_return_value(
    builder: &mut FunctionBuilder,
    call_inst: cranelift_codegen::ir::Inst,
) -> cranelift_codegen::ir::Value {
    let results = builder.inst_results(call_inst);
    if results.is_empty() {
        return builder.ins().iconst(types::I64, 0);
    }
    let ret = results[0];
    if builder.func.dfg.value_type(ret) == types::F64 {
        builder.ins().bitcast(types::I64, MemFlags::new(), ret)
    } else {
        ret
    }
}

/// Map a print/println call to the correct typed runtime function.
fn pick_print_runtime<'a>(
    func_name: &str,
    arg: &IrValue,
    reg_types: &HashMap<String, RegType>,
) -> &'a str {
    let is_ln = func_name == "println";
    let ty = match arg {
        IrValue::StringConst(_) => RegType::String,
        IrValue::IntConst(_) => RegType::Int,
        IrValue::FloatConst(_) => RegType::Float,
        IrValue::BoolConst(_) => RegType::Bool,
        IrValue::Register(r) => reg_types.get(r).copied().unwrap_or(RegType::Int),
        IrValue::NullConst => RegType::Int,
    };
    match (ty, is_ln) {
        (RegType::String, false) => "nex_print_str",
        (RegType::String, true) => "nex_println_str",
        (RegType::Float, false) => "nex_print_double",
        (RegType::Float, true) => "nex_println_double",
        (RegType::Bool, false) => "nex_print_bool",
        (RegType::Bool, true) => "nex_println_bool",
        (_, false) => "nex_print_int",
        (_, true) => "nex_println_int",
    }
}

/// Map an Nex stdlib function name to its C ABI runtime symbol.
fn stdlib_function_name(name: &str) -> Option<&'static str> {
    match name {
        // std.math
        "abs_int" => Some("nex_math_abs_int"),
        "abs_float" => Some("nex_math_abs_float"),
        "min_int" => Some("nex_math_min_int"),
        "max_int" => Some("nex_math_max_int"),
        "min_float" => Some("nex_math_min_float"),
        "max_float" => Some("nex_math_max_float"),
        "clamp_int" => Some("nex_math_clamp_int"),
        "clamp_float" => Some("nex_math_clamp_float"),
        "floor" => Some("nex_math_floor"),
        "ceil" => Some("nex_math_ceil"),
        "round" => Some("nex_math_round"),
        "sqrt" => Some("nex_math_sqrt"),
        "pow" => Some("nex_math_pow"),
        "sin" => Some("nex_math_sin"),
        "cos" => Some("nex_math_cos"),
        "tan" => Some("nex_math_tan"),
        "log" => Some("nex_math_log"),
        "log2" => Some("nex_math_log2"),
        "log10" => Some("nex_math_log10"),
        "exp" => Some("nex_math_exp"),
        "random" => Some("nex_math_random"),
        "random_range" => Some("nex_math_random_range"),
        // std.string
        "str_length" => Some("nex_str_length"),
        "str_substring" => Some("nex_str_substring"),
        "str_split" => Some("nex_str_split"),
        "str_trim" => Some("nex_str_trim"),
        "str_trim_start" => Some("nex_str_trim_start"),
        "str_trim_end" => Some("nex_str_trim_end"),
        "starts_with" => Some("nex_str_starts_with"),
        "ends_with" => Some("nex_str_ends_with"),
        "contains" => Some("nex_str_contains"),
        "index_of" => Some("nex_str_index_of"),
        "str_replace" => Some("nex_str_replace"),
        "to_upper" => Some("nex_str_to_upper"),
        "to_lower" => Some("nex_str_to_lower"),
        "str_repeat" => Some("nex_str_repeat"),
        "char_at" => Some("nex_str_char_at"),
        "str_reverse" => Some("nex_str_reverse"),
        "str_truncate" => Some("nex_str_truncate"),
        // std.convert
        "parse_int" => Some("nex_parse_int"),
        "parse_float" => Some("nex_parse_float"),
        "parse_bool" => Some("nex_parse_bool"),
        "char_to_str" => Some("nex_char_to_str"),
        "str_to_chars" => Some("nex_str_to_chars"),
        // std.env
        "env_get" => Some("nex_env_get"),
        "env_set" => Some("nex_env_set"),
        "env_has" => Some("nex_env_has"),
        "env_args_count" => Some("nex_env_args_count"),
        "env_args_get" => Some("nex_env_args_get"),
        "env_cwd" => Some("nex_env_cwd"),
        // std.time
        "now_millis" => Some("nex_time_now_millis"),
        "now_nanos" => Some("nex_time_now_nanos"),
        "sleep_millis" => Some("nex_time_sleep_millis"),
        "elapsed_millis" => Some("nex_time_elapsed_millis"),
        // std.collections
        "list_sort_int" => Some("nex_list_sort_int"),
        "list_reverse" => Some("nex_list_reverse"),
        "list_clear" => Some("nex_list_clear"),
        "list_contains_int" => Some("nex_list_contains_int"),
        "list_index_of_int" => Some("nex_list_index_of_int"),
        "list_filter" => Some("nex_list_filter"),
        "list_map" => Some("nex_list_map"),
        "list_foreach" => Some("nex_list_foreach"),
        "list_contains_str" => Some("nex_list_contains_str"),
        "set_new" => Some("nex_set_new"),
        "set_add" => Some("nex_set_add"),
        "set_contains" => Some("nex_set_contains"),
        "set_remove" => Some("nex_set_remove"),
        "set_size" => Some("nex_set_size"),
        "map_new" => Some("nex_map_new"),
        "map_put" => Some("nex_map_put"),
        "map_get" => Some("nex_map_get"),
        "map_contains" => Some("nex_map_contains"),
        "map_remove" => Some("nex_map_remove"),
        "map_size" => Some("nex_map_size"),
        "map_keys" => Some("nex_map_keys"),
        "map_values" => Some("nex_map_values"),
        "map_free" => Some("nex_map_free"),
        // std.io
        "read_line" => Some("nex_io_read_line"),
        "file_exists" => Some("nex_io_file_exists"),
        "file_delete" => Some("nex_io_file_delete"),
        "file_rename" => Some("nex_io_file_rename"),
        "file_copy" => Some("nex_io_file_copy"),
        "file_size" => Some("nex_io_file_size"),
        "file_read_bytes" => Some("nex_io_file_read_bytes"),
        "file_write_bytes" => Some("nex_io_file_write_bytes"),
        "file_append" => Some("nex_io_file_append"),
        "mkdir" => Some("nex_io_mkdir"),
        "list_dir" => Some("nex_io_list_dir"),
        // std.path
        "path_join" => Some("nex_path_join"),
        "path_parent" => Some("nex_path_parent"),
        "path_file_name" => Some("nex_path_file_name"),
        "path_extension" => Some("nex_path_extension"),
        "path_stem" => Some("nex_path_stem"),
        "path_is_absolute" => Some("nex_path_is_absolute"),
        "path_normalize" => Some("nex_path_normalize"),
        "path_separator" => Some("nex_path_separator"),
        // std.json
        "json_parse" => Some("nex_json_parse"),
        "json_stringify" => Some("nex_json_stringify"),
        "json_get_string" => Some("nex_json_get_string"),
        "json_get_int" => Some("nex_json_get_int"),
        "json_get_float" => Some("nex_json_get_float"),
        "json_get_bool" => Some("nex_json_get_bool"),
        "json_new_object" => Some("nex_json_new_object"),
        "json_set_string" => Some("nex_json_set_string"),
        "json_set_int" => Some("nex_json_set_int"),
        "json_set_float" => Some("nex_json_set_float"),
        "json_set_bool" => Some("nex_json_set_bool"),
        "json_set_null" => Some("nex_json_set_null"),
        "json_stringify_pretty" => Some("nex_json_stringify_pretty"),
        "json_free" => Some("nex_json_free"),
        // std.regex
        "regex_new" => Some("nex_regex_new"),
        "regex_is_match" => Some("nex_regex_is_match"),
        "regex_find" => Some("nex_regex_find"),
        "regex_replace" => Some("nex_regex_replace"),
        "regex_free" => Some("nex_regex_free"),
        // std.process
        "process_exec" => Some("nex_process_exec"),
        "process_exec_output" => Some("nex_process_exec_output"),
        "process_exit" => Some("nex_process_exit"),
        "process_pid" => Some("nex_process_pid"),
        "process_spawn" => Some("nex_process_spawn"),
        "process_wait" => Some("nex_process_wait"),
        // std.net
        "tcp_connect" => Some("nex_net_tcp_connect"),
        "tcp_close" => Some("nex_net_tcp_close"),
        "tcp_send" => Some("nex_net_tcp_send"),
        "tcp_recv" => Some("nex_net_tcp_recv"),
        "tcp_listen" => Some("nex_net_tcp_listen"),
        "tcp_accept" => Some("nex_net_tcp_accept"),
        "udp_bind" => Some("nex_net_udp_bind"),
        "udp_close" => Some("nex_net_udp_close"),
        "udp_send" => Some("nex_net_udp_send"),
        "udp_recv" => Some("nex_net_udp_recv"),
        // std.http
        "http_get" => Some("nex_http_get"),
        "http_post" => Some("nex_http_post"),
        "http_response_status" => Some("nex_http_response_status"),
        "http_response_body" => Some("nex_http_response_body"),
        "http_response_header" => Some("nex_http_response_header"),
        "http_response_free" => Some("nex_http_response_free"),
        // std.threading
        "thread_spawn" => Some("nex_thread_spawn"),
        "thread_join" => Some("nex_thread_join"),
        "thread_sleep" => Some("nex_thread_sleep"),
        "thread_current_id" => Some("nex_thread_current_id"),
        "mutex_new" => Some("nex_mutex_new"),
        "mutex_lock" => Some("nex_mutex_lock"),
        "mutex_unlock" => Some("nex_mutex_unlock"),
        "mutex_free" => Some("nex_mutex_free"),
        // std.crypto
        "sha256" => Some("nex_crypto_sha256"),
        "sha512" => Some("nex_crypto_sha512"),
        "md5" => Some("nex_crypto_md5"),
        "random_bytes" => Some("nex_crypto_random_bytes"),
        "base64_encode" => Some("nex_crypto_base64_encode"),
        "base64_decode" => Some("nex_crypto_base64_decode"),
        "hmac_sha256" => Some("nex_crypto_hmac_sha256"),
        // std.logging
        "log_debug" => Some("nex_log_debug"),
        "log_info" => Some("nex_log_info"),
        "log_warn" => Some("nex_log_warn"),
        "log_error" => Some("nex_log_error"),
        "log_set_level" => Some("nex_log_set_level"),
        "log_with_tag" => Some("nex_log_with_tag"),
        // std.testing
        "assert" => Some("nex_assert"),
        "assert_eq_int" => Some("nex_assert_eq_int"),
        "assert_eq_str" => Some("nex_assert_eq_str"),
        "assert_eq_float" => Some("nex_assert_eq_float"),
        "assert_eq_bool" => Some("nex_assert_eq_bool"),
        "assert_ne_int" => Some("nex_assert_ne_int"),
        "assert_ne_str" => Some("nex_assert_ne_str"),
        "assert_true" => Some("nex_assert_true"),
        _ => {
            if let Some(n) = ui_function_name(name) {
                return Some(n);
            }
            if let Some(n) = engine_function_name(name) {
                return Some(n);
            }
            if let Some(n) = torch_function_name(name) {
                return Some(n);
            }
            if let Some(n) = crypto_function_name(name) {
                return Some(n);
            }
            if let Some(n) = http_function_name(name) {
                return Some(n);
            }
            if let Some(n) = regex_function_name(name) {
                return Some(n);
            }
            None
        }
    }
}

fn ui_function_name(name: &str) -> Option<&'static str> {
    match name {
        // Application
        "ui_app_create" => Some("nex_ui_app_create"),
        "ui_app_set_backend" => Some("nex_ui_app_set_backend"),
        "ui_app_set_root" => Some("nex_ui_app_set_root"),
        "ui_app_run" => Some("nex_ui_app_run"),
        "ui_app_quit" => Some("nex_ui_app_quit"),
        "ui_app_destroy" => Some("nex_ui_app_destroy"),
        "ui_is_running" => Some("nex_ui_app_is_running"),
        "ui_app_render" | "ui_render" => Some("nex_ui_app_render"),
        // Events
        "ui_poll_event" => Some("nex_ui_poll_event"),
        "ui_event_type" => Some("nex_ui_event_type"),
        "ui_event_widget" => Some("nex_ui_event_widget"),
        // Widget creation
        "ui_text" => Some("nex_ui_text"),
        "ui_button" => Some("nex_ui_button"),
        "ui_text_input" => Some("nex_ui_text_input"),
        "ui_image" => Some("nex_ui_image"),
        "ui_checkbox" => Some("nex_ui_checkbox"),
        "ui_slider" => Some("nex_ui_slider"),
        "ui_row" => Some("nex_ui_row"),
        "ui_column" => Some("nex_ui_column"),
        "ui_stack" => Some("nex_ui_stack"),
        "ui_scroll" => Some("nex_ui_scroll"),
        "ui_grid" => Some("nex_ui_grid"),
        "ui_canvas" => Some("nex_ui_canvas"),
        // Widget tree
        "ui_add_child" => Some("nex_ui_add_child"),
        "ui_remove_child" => Some("nex_ui_remove_child"),
        "ui_set_id" => Some("nex_ui_set_id"),
        "ui_get_id" => Some("nex_ui_get_id"),
        // Widget properties
        "ui_set_text" => Some("nex_ui_set_text"),
        "ui_get_text" => Some("nex_ui_get_text"),
        "ui_set_visible" => Some("nex_ui_set_visible"),
        "ui_set_enabled" => Some("nex_ui_set_enabled"),
        "ui_get_value_float" => Some("nex_ui_get_value_float"),
        "ui_set_value_float" => Some("nex_ui_set_value_float"),
        // Styling
        "ui_set_width" => Some("nex_ui_set_width"),
        "ui_set_height" => Some("nex_ui_set_height"),
        "ui_set_min_width" => Some("nex_ui_set_min_width"),
        "ui_set_min_height" => Some("nex_ui_set_min_height"),
        "ui_set_padding" => Some("nex_ui_set_padding"),
        "ui_set_padding_all" => Some("nex_ui_set_padding_all"),
        "ui_set_margin" => Some("nex_ui_set_margin"),
        "ui_set_bg_color" => Some("nex_ui_set_bg_color"),
        "ui_set_fg_color" => Some("nex_ui_set_fg_color"),
        "ui_set_font_size" => Some("nex_ui_set_font_size"),
        "ui_set_border" => Some("nex_ui_set_border"),
        "ui_set_border_radius" => Some("nex_ui_set_border_radius"),
        "ui_set_flex_grow" => Some("nex_ui_set_flex_grow"),
        "ui_set_align_self" => Some("nex_ui_set_align_self"),
        "ui_set_justify_content" => Some("nex_ui_set_justify_content"),
        "ui_set_align_items" => Some("nex_ui_set_align_items"),
        "ui_set_gap" => Some("nex_ui_set_gap"),
        // Callbacks
        "ui_on_click" => Some("nex_ui_on_click"),
        "ui_on_change" => Some("nex_ui_on_change"),
        "ui_on_hover" => Some("nex_ui_on_hover"),
        "ui_on_key" => Some("nex_ui_on_key"),
        // Canvas
        "ui_canvas_fill_rect" => Some("nex_ui_canvas_fill_rect"),
        "ui_canvas_stroke_rect" => Some("nex_ui_canvas_stroke_rect"),
        "ui_canvas_fill_circle" => Some("nex_ui_canvas_fill_circle"),
        "ui_canvas_draw_line" => Some("nex_ui_canvas_draw_line"),
        "ui_canvas_draw_text" => Some("nex_ui_canvas_draw_text"),
        "ui_canvas_clear" => Some("nex_ui_canvas_clear"),
        // Dialogs
        "ui_dialog_message" => Some("nex_ui_dialog_message"),
        "ui_dialog_confirm" => Some("nex_ui_dialog_confirm"),
        "ui_dialog_open_file" => Some("nex_ui_dialog_open_file"),
        "ui_dialog_save_file" => Some("nex_ui_dialog_save_file"),
        // Binding engine
        "nex_ui_bind" => Some("nex_ui_bind"),
        "nex_ui_unbind" => Some("nex_ui_unbind"),
        "notify_changed" | "nex_ui_notify_changed" => Some("nex_ui_notify_changed"),
        "nex_ui_bindings_clear" => Some("nex_ui_bindings_clear"),
        // Overlay
        "ui_overlay_init" => Some("nex_ui_overlay_init"),
        "ui_overlay_render" => Some("nex_ui_overlay_render"),
        "ui_overlay_mouse_move" => Some("nex_ui_overlay_mouse_move"),
        "ui_overlay_mouse_down" => Some("nex_ui_overlay_mouse_down"),
        "ui_overlay_mouse_up" => Some("nex_ui_overlay_mouse_up"),
        "ui_overlay_key_char" => Some("nex_ui_overlay_key_char"),
        "ui_overlay_key_name" => Some("nex_ui_overlay_key_name"),
        "ui_overlay_hit_test" => Some("nex_ui_overlay_hit_test"),
        _ => None,
    }
}

fn engine_function_name(name: &str) -> Option<&'static str> {
    match name {
        // Lifecycle
        "engine_window_create" => Some("nex_engine_window_create"),
        "engine_window_run" => Some("nex_engine_window_run"),
        "engine_window_quit" => Some("nex_engine_window_quit"),
        "engine_window_destroy" => Some("nex_engine_window_destroy"),
        "engine_window_is_running" => Some("nex_engine_window_is_running"),
        // Config
        "engine_clear_color" => Some("nex_engine_clear_color"),
        "engine_set_update_fn" => Some("nex_engine_set_update_fn"),
        "engine_window_width" => Some("nex_engine_window_width"),
        "engine_window_height" => Some("nex_engine_window_height"),
        // Input — keyboard
        "engine_key_down" => Some("nex_engine_key_down"),
        "engine_key_pressed" => Some("nex_engine_key_pressed"),
        "engine_key_released" => Some("nex_engine_key_released"),
        // Input — mouse
        "engine_mouse_x" => Some("nex_engine_mouse_x"),
        "engine_mouse_y" => Some("nex_engine_mouse_y"),
        "engine_mouse_delta_x" => Some("nex_engine_mouse_delta_x"),
        "engine_mouse_delta_y" => Some("nex_engine_mouse_delta_y"),
        "engine_mouse_button_down" => Some("nex_engine_mouse_button_down"),
        "engine_mouse_button_pressed" => Some("nex_engine_mouse_button_pressed"),
        // Timing
        "engine_delta_time" => Some("nex_engine_delta_time"),
        "engine_elapsed_time" => Some("nex_engine_elapsed_time"),
        "engine_frame_count" => Some("nex_engine_frame_count"),
        // Camera
        "engine_set_camera_pos" => Some("nex_engine_set_camera_pos"),
        "engine_set_camera_target" => Some("nex_engine_set_camera_target"),
        "engine_set_camera_up" => Some("nex_engine_set_camera_up"),
        "engine_set_perspective" => Some("nex_engine_set_perspective"),
        // Drawing
        "engine_push_vertex" => Some("nex_engine_push_vertex"),
        "engine_push_vertex_lit" => Some("nex_engine_push_vertex_lit"),
        "engine_draw_triangles" => Some("nex_engine_draw_triangles"),
        // Lighting
        "engine_set_ambient_color" => Some("nex_engine_set_ambient_color"),
        "engine_light_set_type" => Some("nex_engine_light_set_type"),
        "engine_light_set_enabled" => Some("nex_engine_light_set_enabled"),
        "engine_light_set_position" => Some("nex_engine_light_set_position"),
        "engine_light_set_direction" => Some("nex_engine_light_set_direction"),
        "engine_light_set_color" => Some("nex_engine_light_set_color"),
        "engine_light_set_intensity" => Some("nex_engine_light_set_intensity"),
        "engine_light_set_range" => Some("nex_engine_light_set_range"),
        "engine_light_set_spot_angles" => Some("nex_engine_light_set_spot_angles"),
        "engine_clear_lights" => Some("nex_engine_clear_lights"),
        // Texture
        "engine_push_vertex_uv" => Some("nex_engine_push_vertex_uv"),
        "engine_texture_load" => Some("nex_engine_texture_load"),
        "engine_texture_bind" => Some("nex_engine_texture_bind"),
        "engine_texture_unbind" => Some("nex_engine_texture_unbind"),
        "engine_texture_width" => Some("nex_engine_texture_width"),
        "engine_texture_height" => Some("nex_engine_texture_height"),
        // SpriteBatch
        "engine_spritebatch_begin" => Some("nex_engine_spritebatch_begin"),
        "engine_spritebatch_end" => Some("nex_engine_spritebatch_end"),
        "engine_spritebatch_draw" => Some("nex_engine_spritebatch_draw"),
        "engine_spritebatch_draw_src" => Some("nex_engine_spritebatch_draw_src"),
        // Font
        "engine_font_draw_text" => Some("nex_engine_font_draw_text"),
        "engine_font_measure_text" => Some("nex_engine_font_measure_text"),
        // Render States
        "engine_set_blend_mode" => Some("nex_engine_set_blend_mode"),
        "engine_set_cull_mode" => Some("nex_engine_set_cull_mode"),
        "engine_set_fill_mode" => Some("nex_engine_set_fill_mode"),
        "engine_set_depth_enabled" => Some("nex_engine_set_depth_enabled"),
        // Mouse scroll
        "engine_mouse_scroll_delta" => Some("nex_engine_mouse_scroll_delta"),
        // Audio
        "engine_audio_load" => Some("nex_engine_audio_load"),
        "engine_audio_play" => Some("nex_engine_audio_play"),
        "engine_audio_play_looped" => Some("nex_engine_audio_play_looped"),
        "engine_audio_stop" => Some("nex_engine_audio_stop"),
        "engine_audio_set_volume" => Some("nex_engine_audio_set_volume"),
        "engine_audio_is_playing" => Some("nex_engine_audio_is_playing"),
        "engine_audio_free" => Some("nex_engine_audio_free"),
        // Model loading
        "engine_model_load" => Some("nex_engine_model_load"),
        "engine_model_draw" => Some("nex_engine_model_draw"),
        "engine_model_vertex_count" => Some("nex_engine_model_vertex_count"),
        "engine_model_free" => Some("nex_engine_model_free"),
        // Skeletal Animation
        "engine_anim_model_load" => Some("nex_engine_anim_model_load"),
        "engine_anim_model_draw" => Some("nex_engine_anim_model_draw"),
        "engine_anim_play" => Some("nex_engine_anim_play"),
        "engine_anim_stop" => Some("nex_engine_anim_stop"),
        "engine_anim_pause" => Some("nex_engine_anim_pause"),
        "engine_anim_set_speed" => Some("nex_engine_anim_set_speed"),
        "engine_anim_set_looping" => Some("nex_engine_anim_set_looping"),
        "engine_anim_set_time" => Some("nex_engine_anim_set_time"),
        "engine_anim_get_time" => Some("nex_engine_anim_get_time"),
        "engine_anim_clip_count" => Some("nex_engine_anim_clip_count"),
        "engine_anim_clip_duration" => Some("nex_engine_anim_clip_duration"),
        "engine_anim_joint_count" => Some("nex_engine_anim_joint_count"),
        "engine_anim_model_free" => Some("nex_engine_anim_model_free"),
        // RenderTarget
        "engine_rendertarget_create" => Some("nex_engine_rendertarget_create"),
        "engine_rendertarget_bind" => Some("nex_engine_rendertarget_bind"),
        "engine_rendertarget_unbind" => Some("nex_engine_rendertarget_unbind"),
        "engine_rendertarget_as_texture" => Some("nex_engine_rendertarget_as_texture"),
        "engine_rendertarget_width" => Some("nex_engine_rendertarget_width"),
        "engine_rendertarget_height" => Some("nex_engine_rendertarget_height"),
        "engine_rendertarget_free" => Some("nex_engine_rendertarget_free"),
        // Gamepad
        "engine_gamepad_connected" => Some("nex_engine_gamepad_connected"),
        "engine_gamepad_button" => Some("nex_engine_gamepad_button"),
        "engine_gamepad_axis" => Some("nex_engine_gamepad_axis"),
        // UI Overlay
        "engine_enable_ui_overlay" => Some("nex_engine_enable_ui_overlay"),
        _ => None,
    }
}

fn torch_function_name(name: &str) -> Option<&'static str> {
    match name {
        // Tensor creation
        "tensor_zeros" => Some("nex_torch_tensor_zeros"),
        "tensor_ones" => Some("nex_torch_tensor_ones"),
        "tensor_rand" => Some("nex_torch_tensor_rand"),
        "tensor_randn" => Some("nex_torch_tensor_randn"),
        "tensor_from_float_data" => Some("nex_torch_tensor_from_float_data"),
        "tensor_arange" => Some("nex_torch_tensor_arange"),
        "tensor_eye" => Some("nex_torch_tensor_eye"),
        "tensor_free" => Some("nex_torch_tensor_free"),
        // Tensor operations
        "tensor_add" => Some("nex_torch_tensor_add"),
        "tensor_sub" => Some("nex_torch_tensor_sub"),
        "tensor_mul" => Some("nex_torch_tensor_mul"),
        "tensor_div" => Some("nex_torch_tensor_div"),
        "tensor_matmul" => Some("nex_torch_tensor_matmul"),
        "tensor_neg" => Some("nex_torch_tensor_neg"),
        "tensor_exp" => Some("nex_torch_tensor_exp"),
        "tensor_log" => Some("nex_torch_tensor_log"),
        "tensor_sum" => Some("nex_torch_tensor_sum"),
        "tensor_mean" => Some("nex_torch_tensor_mean"),
        "tensor_reshape" => Some("nex_torch_tensor_reshape"),
        "tensor_transpose" => Some("nex_torch_tensor_transpose"),
        "tensor_squeeze" => Some("nex_torch_tensor_squeeze"),
        "tensor_unsqueeze" => Some("nex_torch_tensor_unsqueeze"),
        "tensor_print" => Some("nex_torch_tensor_print"),
        "tensor_shape_dim" => Some("nex_torch_tensor_shape_dim"),
        // Tensor data access
        "tensor_get_float" => Some("nex_torch_tensor_get_float"),
        "tensor_item_float" => Some("nex_torch_tensor_item_float"),
        "tensor_ndim" => Some("nex_torch_tensor_ndim"),
        "tensor_numel" => Some("nex_torch_tensor_numel"),
        // Device management
        "cuda_is_available" => Some("nex_torch_cuda_is_available"),
        "cuda_device_count" => Some("nex_torch_cuda_device_count"),
        "tensor_to_device" => Some("nex_torch_tensor_to_device"),
        "set_num_threads" => Some("nex_torch_set_num_threads"),
        // Autograd
        "tensor_requires_grad" => Some("nex_torch_tensor_requires_grad"),
        "tensor_backward" => Some("nex_torch_tensor_backward"),
        "tensor_grad" => Some("nex_torch_tensor_grad"),
        "torch_no_grad" => Some("nex_torch_no_grad"),
        // NN layers
        "nn_sequential_new" => Some("nex_torch_nn_sequential_new"),
        "nn_linear" => Some("nex_torch_nn_linear"),
        "nn_conv2d" => Some("nex_torch_nn_conv2d"),
        "nn_relu" => Some("nex_torch_nn_relu"),
        "nn_sigmoid" => Some("nex_torch_nn_sigmoid"),
        "nn_tanh" => Some("nex_torch_nn_tanh"),
        "nn_softmax" => Some("nex_torch_nn_softmax"),
        "nn_dropout" => Some("nex_torch_nn_dropout"),
        "nn_batch_norm" => Some("nex_torch_nn_batch_norm"),
        "nn_to_device" => Some("nex_torch_nn_to_device"),
        "nn_forward" => Some("nex_torch_nn_forward"),
        "nn_free" => Some("nex_torch_nn_free"),
        // Loss functions
        "loss_mse" => Some("nex_torch_loss_mse"),
        "loss_cross_entropy" => Some("nex_torch_loss_cross_entropy"),
        "loss_bce" => Some("nex_torch_loss_bce"),
        // Optimizers
        "optim_sgd" => Some("nex_torch_optim_sgd"),
        "optim_adam" => Some("nex_torch_optim_adam"),
        "optim_step" => Some("nex_torch_optim_step"),
        "optim_zero_grad" => Some("nex_torch_optim_zero_grad"),
        "optim_free" => Some("nex_torch_optim_free"),
        // Model I/O
        "model_save" => Some("nex_torch_model_save"),
        "model_load" => Some("nex_torch_model_load"),
        "jit_load" => Some("nex_torch_jit_load"),
        "jit_forward" => Some("nex_torch_jit_forward"),
        // Extended tensor ops — scalar arithmetic
        "tensor_add_scalar" => Some("nex_torch_tensor_add_scalar"),
        "tensor_mul_scalar" => Some("nex_torch_tensor_mul_scalar"),
        "tensor_div_scalar" => Some("nex_torch_tensor_div_scalar"),
        "tensor_pow_scalar" => Some("nex_torch_tensor_pow_scalar"),
        // Extended tensor ops — element-wise math
        "tensor_sqrt" => Some("nex_torch_tensor_sqrt"),
        "tensor_abs" => Some("nex_torch_tensor_abs"),
        "tensor_clamp" => Some("nex_torch_tensor_clamp"),
        "tensor_softmax" => Some("nex_torch_tensor_softmax"),
        // Extended tensor ops — comparison
        "tensor_eq_scalar" => Some("nex_torch_tensor_eq_scalar"),
        "tensor_gt_scalar" => Some("nex_torch_tensor_gt_scalar"),
        "tensor_lt_scalar" => Some("nex_torch_tensor_lt_scalar"),
        // Extended tensor ops — masking and triangular
        "tensor_tril" => Some("nex_torch_tensor_tril"),
        "tensor_triu" => Some("nex_torch_tensor_triu"),
        "tensor_masked_fill" => Some("nex_torch_tensor_masked_fill"),
        "tensor_where_self" => Some("nex_torch_tensor_where_self"),
        // Extended tensor ops — reduction with dimension
        "tensor_sum_dim" => Some("nex_torch_tensor_sum_dim"),
        "tensor_mean_dim" => Some("nex_torch_tensor_mean_dim"),
        "tensor_argmax" => Some("nex_torch_tensor_argmax"),
        "tensor_max_dim" => Some("nex_torch_tensor_max_dim"),
        "tensor_min_dim" => Some("nex_torch_tensor_min_dim"),
        // Extended tensor ops — shape and indexing
        "tensor_cat" => Some("nex_torch_tensor_cat"),
        "tensor_narrow" => Some("nex_torch_tensor_narrow"),
        "tensor_index_select" => Some("nex_torch_tensor_index_select"),
        "tensor_flatten" => Some("nex_torch_tensor_flatten"),
        // Extended tensor ops — creation helpers
        "tensor_ones_like" => Some("nex_torch_tensor_ones_like"),
        "tensor_zeros_like" => Some("nex_torch_tensor_zeros_like"),
        "tensor_full_like" => Some("nex_torch_tensor_full_like"),
        // Extended tensor ops — utility
        "tensor_clone" => Some("nex_torch_tensor_clone"),
        "tensor_detach" => Some("nex_torch_tensor_detach"),
        "tensor_contiguous" => Some("nex_torch_tensor_contiguous"),
        "tensor_to_dtype_float" => Some("nex_torch_tensor_to_dtype_float"),
        "tensor_to_dtype_long" => Some("nex_torch_tensor_to_dtype_long"),
        // Extended NN layers
        "nn_layer_norm" => Some("nex_torch_nn_layer_norm"),
        "nn_gelu" => Some("nex_torch_nn_gelu"),
        "nn_embedding" => Some("nex_torch_nn_embedding"),
        // Utility
        "torch_manual_seed" => Some("nex_torch_manual_seed"),
        "torch_version" => Some("nex_torch_version"),
        "tensor_to_string" => Some("nex_torch_tensor_to_string"),
        _ => None,
    }
}

fn crypto_function_name(name: &str) -> Option<&'static str> {
    match name {
        "crypto_sha256" => Some("nex_crypto_sha256"),
        "crypto_sha512" => Some("nex_crypto_sha512"),
        "crypto_md5" => Some("nex_crypto_md5"),
        "crypto_random_bytes" => Some("nex_crypto_random_bytes"),
        "crypto_base64_encode" => Some("nex_crypto_base64_encode"),
        "crypto_base64_decode" => Some("nex_crypto_base64_decode"),
        "crypto_hmac_sha256" => Some("nex_crypto_hmac_sha256"),
        _ => None,
    }
}

fn http_function_name(name: &str) -> Option<&'static str> {
    match name {
        "http_get" => Some("nex_http_get"),
        "http_post" => Some("nex_http_post"),
        "http_response_status" => Some("nex_http_response_status"),
        "http_response_body" => Some("nex_http_response_body"),
        "http_response_header" => Some("nex_http_response_header"),
        "http_response_free" => Some("nex_http_response_free"),
        _ => None,
    }
}

fn regex_function_name(name: &str) -> Option<&'static str> {
    match name {
        "regex_new" => Some("nex_regex_new"),
        "regex_is_match" => Some("nex_regex_is_match"),
        "regex_find" => Some("nex_regex_find"),
        "regex_replace" => Some("nex_regex_replace"),
        "regex_free" => Some("nex_regex_free"),
        _ => None,
    }
}

fn build_signature<M: Module>(module: &M, func: &IrFunction) -> Signature {
    let call_conv = module.isa().default_call_conv();
    let mut sig = Signature::new(call_conv);

    for _ in &func.params {
        sig.params.push(AbiParam::new(types::I64));
    }

    match &func.return_type {
        Type::Unit => {}
        _ => {
            sig.returns.push(AbiParam::new(types::I64));
        }
    }
    sig
}

fn mangle_name(name: &str) -> String {
    name.replace("::", ".")
}

/// Walk every instruction to discover all register / variable names so we
/// can pre-declare Cranelift Variables.
fn collect_variable_names(func: &IrFunction) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    let mut add = |n: &str| {
        if !names.contains(&n.to_string()) {
            names.push(n.to_string());
        }
    };

    for (pname, _) in &func.params {
        add(&format!("%param.{pname}"));
        add(&format!("%{pname}"));
    }

    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                IrInstruction::Allocate { dst, .. } => add(dst),
                IrInstruction::Store { dst, src } => {
                    add(dst);
                    if let IrValue::Register(r) = src {
                        add(r);
                    }
                }
                IrInstruction::Load { dst, src } => {
                    add(dst);
                    add(src);
                }
                IrInstruction::BinOp { dst, lhs, rhs, .. } => {
                    add(dst);
                    if let IrValue::Register(r) = lhs {
                        add(r);
                    }
                    if let IrValue::Register(r) = rhs {
                        add(r);
                    }
                }
                IrInstruction::UnaryOp { dst, operand, .. } => {
                    add(dst);
                    if let IrValue::Register(r) = operand {
                        add(r);
                    }
                }
                IrInstruction::Call { dst, args: _, .. } => {
                    if let Some(d) = dst {
                        add(d);
                    }
                }
                IrInstruction::VCall { dst, this_ptr, args: _, .. } => {
                    if let Some(d) = dst {
                        add(d);
                    }
                    add(&format!("%{this_ptr}"));
                }
                IrInstruction::MemberAccess { dst, receiver, .. } => {
                    add(dst);
                    if let IrValue::Register(r) = receiver {
                        add(r);
                    }
                }
                IrInstruction::Branch { cond, .. } => {
                    if let IrValue::Register(r) = cond {
                        add(r);
                    }
                }
                IrInstruction::Return(Some(IrValue::Register(r))) => add(r),
                IrInstruction::Print {
                    value: IrValue::Register(r),
                } => add(r),
                _ => {}
            }
        }
    }

    names
}

/// Scan the IR module and collect the set of runtime symbol names that are
/// actually referenced.  This allows `declare_runtime_imports` to skip
/// declaring hundreds of unused imports — critical for MSVC DLL linking which
/// requires all imports to be resolved at link time.
fn collect_needed_imports(ir: &IrModule) -> std::collections::HashSet<String> {
    let mut needed: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Core runtime symbols — always needed by any module.
    for s in &[
        "nex_gc_alloc",
        "nex_gc_collect",
        "nex_gc_safepoint",
        "nex_throw",
        "nex_dispose",
        "puts",
    ] {
        needed.insert(s.to_string());
    }

    for func in &ir.functions {
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    IrInstruction::Print { .. } => {
                        // Print can dispatch to any typed print function at
                        // codegen time, so include all variants.
                        for s in &[
                            "nex_print_str", "nex_println_str",
                            "nex_print_int", "nex_println_int",
                            "nex_print_double", "nex_println_double",
                            "nex_print_bool", "nex_println_bool",
                            "nex_print_char", "nex_println_char",
                        ] {
                            needed.insert(s.to_string());
                        }
                    }
                    IrInstruction::Call { target, args, .. } => {
                        // print/println route through pick_print_runtime.
                        if target == "print" || target == "println" {
                            for s in &[
                                "nex_print_str", "nex_println_str",
                                "nex_print_int", "nex_println_int",
                                "nex_print_double", "nex_println_double",
                                "nex_print_bool", "nex_println_bool",
                                "nex_print_char", "nex_println_char",
                            ] {
                                needed.insert(s.to_string());
                            }
                            // Multi-arg print needs string coercion + concat.
                            if args.len() > 1 {
                                for s in &[
                                    "nex_str_concat",
                                    "nex_int_to_str",
                                    "nex_double_to_str",
                                    "nex_bool_to_str",
                                ] {
                                    needed.insert(s.to_string());
                                }
                            }
                        } else if let Some(rt) = stdlib_function_name(target) {
                            needed.insert(rt.to_string());
                        } else if target.starts_with("nex_") {
                            // IR targets already using full runtime names
                            // (e.g. nex_list_add from builtin dispatch)
                            needed.insert(target.clone());
                        }
                    }
                    IrInstruction::BinOp { op, .. } if op == "add" => {
                        // String concat and coerce_to_str may be used.
                        for s in &[
                            "nex_str_concat",
                            "nex_int_to_str",
                            "nex_double_to_str",
                            "nex_bool_to_str",
                        ] {
                            needed.insert(s.to_string());
                        }
                    }
                    IrInstruction::BinOp { op, .. } if op == "eq" || op == "ne" => {
                        // String equality via nex_str_eq may be dispatched at codegen time.
                        needed.insert("nex_str_eq".to_string());
                    }
                    _ => {}
                }
            }
        }
    }

    needed
}

/// Declare common runtime functions as imports so generated code can call them.
/// When `filter` is `Some`, only symbols in the set are declared (used for
/// shared library builds to avoid unresolved MSVC linker errors).
fn declare_runtime_imports<M: Module>(
    module: &mut M,
    func_ids: &mut HashMap<String, FuncId>,
    filter: Option<&std::collections::HashSet<String>>,
) -> Result<(), String> {
    let cc = module.isa().default_call_conv();

    // void(i64) – single pointer/int arg, no return
    let mut sig_void_ptr = Signature::new(cc);
    sig_void_ptr.params.push(AbiParam::new(types::I64));

    // void(i64, i64) – two args
    let mut sig_void_ptr2 = Signature::new(cc);
    sig_void_ptr2.params.push(AbiParam::new(types::I64));
    sig_void_ptr2.params.push(AbiParam::new(types::I64));

    // i64(i64, i64) – two args, one return
    let mut sig_ptr_ptr2 = Signature::new(cc);
    sig_ptr_ptr2.params.push(AbiParam::new(types::I64));
    sig_ptr_ptr2.params.push(AbiParam::new(types::I64));
    sig_ptr_ptr2.returns.push(AbiParam::new(types::I64));

    // i64(i64) – one arg, one return
    let mut sig_ptr_ptr = Signature::new(cc);
    sig_ptr_ptr.params.push(AbiParam::new(types::I64));
    sig_ptr_ptr.returns.push(AbiParam::new(types::I64));

    // i64(i64, i32) – gc_alloc(type_desc, size)
    let mut sig_gc_alloc = Signature::new(cc);
    sig_gc_alloc.params.push(AbiParam::new(types::I64));
    sig_gc_alloc.params.push(AbiParam::new(types::I32));
    sig_gc_alloc.returns.push(AbiParam::new(types::I64));

    // void() – no args
    let sig_void = Signature::new(cc);

    // i64(i64, i32, i32) – substring(ptr, start, len)
    let mut sig_substring = Signature::new(cc);
    sig_substring.params.push(AbiParam::new(types::I64));
    sig_substring.params.push(AbiParam::new(types::I32));
    sig_substring.params.push(AbiParam::new(types::I32));
    sig_substring.returns.push(AbiParam::new(types::I64));

    // i64() – no args, one return
    let mut sig_ret_ptr = Signature::new(cc);
    sig_ret_ptr.returns.push(AbiParam::new(types::I64));

    // i64(i64, i64, i64) – three args, one return
    let mut sig_ptr_ptr3 = Signature::new(cc);
    sig_ptr_ptr3.params.push(AbiParam::new(types::I64));
    sig_ptr_ptr3.params.push(AbiParam::new(types::I64));
    sig_ptr_ptr3.params.push(AbiParam::new(types::I64));
    sig_ptr_ptr3.returns.push(AbiParam::new(types::I64));

    // void(i64, i64, i64) – three args, no return
    let mut sig_void_ptr3 = Signature::new(cc);
    sig_void_ptr3.params.push(AbiParam::new(types::I64));
    sig_void_ptr3.params.push(AbiParam::new(types::I64));
    sig_void_ptr3.params.push(AbiParam::new(types::I64));

    // i64(i64, i64, i64, i64, i64) – five args, one return (udp_send)
    let mut sig_ptr_ptr5 = Signature::new(cc);
    for _ in 0..5 { sig_ptr_ptr5.params.push(AbiParam::new(types::I64)); }
    sig_ptr_ptr5.returns.push(AbiParam::new(types::I64));

    // --- F64-aware signatures for correct Windows x64 calling convention ---

    // void(f64) – print_double, println_double
    let mut sig_void_f64 = Signature::new(cc);
    sig_void_f64.params.push(AbiParam::new(types::F64));

    // f64(f64) – abs_float, floor, ceil, round, sqrt, sin, cos, tan, log, ...
    let mut sig_f64_ret_f64 = Signature::new(cc);
    sig_f64_ret_f64.params.push(AbiParam::new(types::F64));
    sig_f64_ret_f64.returns.push(AbiParam::new(types::F64));

    // f64(f64, f64) – min_float, max_float, pow
    let mut sig_f64x2_ret_f64 = Signature::new(cc);
    sig_f64x2_ret_f64.params.push(AbiParam::new(types::F64));
    sig_f64x2_ret_f64.params.push(AbiParam::new(types::F64));
    sig_f64x2_ret_f64.returns.push(AbiParam::new(types::F64));

    // f64(f64, f64, f64) – clamp_float
    let mut sig_f64x3_ret_f64 = Signature::new(cc);
    sig_f64x3_ret_f64.params.push(AbiParam::new(types::F64));
    sig_f64x3_ret_f64.params.push(AbiParam::new(types::F64));
    sig_f64x3_ret_f64.params.push(AbiParam::new(types::F64));
    sig_f64x3_ret_f64.returns.push(AbiParam::new(types::F64));

    // i64(f64, f64, f64) – tensor_arange
    let mut sig_f64x3_ret_i64 = Signature::new(cc);
    sig_f64x3_ret_i64.params.push(AbiParam::new(types::F64));
    sig_f64x3_ret_i64.params.push(AbiParam::new(types::F64));
    sig_f64x3_ret_i64.params.push(AbiParam::new(types::F64));
    sig_f64x3_ret_i64.returns.push(AbiParam::new(types::I64));

    // i64(f64) – double_to_str
    let mut sig_f64_ret_i64 = Signature::new(cc);
    sig_f64_ret_i64.params.push(AbiParam::new(types::F64));
    sig_f64_ret_i64.returns.push(AbiParam::new(types::I64));

    // f64() – math_random
    let mut sig_ret_f64 = Signature::new(cc);
    sig_ret_f64.returns.push(AbiParam::new(types::F64));

    // f64(i64, i64) – reflect getFieldFloat(type_id, index) -> f64
    let mut sig_i64x2_ret_f64 = Signature::new(cc);
    sig_i64x2_ret_f64.params.push(AbiParam::new(types::I64));
    sig_i64x2_ret_f64.params.push(AbiParam::new(types::I64));
    sig_i64x2_ret_f64.returns.push(AbiParam::new(types::F64));

    // void(i64, i64, f64) – reflect setFieldFloat / json_set_float
    let mut sig_void_i64x2_f64 = Signature::new(cc);
    sig_void_i64x2_f64.params.push(AbiParam::new(types::I64));
    sig_void_i64x2_f64.params.push(AbiParam::new(types::I64));
    sig_void_i64x2_f64.params.push(AbiParam::new(types::F64));

    // f64(i64) – tensor_item_float, parse_float
    let mut sig_i64_ret_f64 = Signature::new(cc);
    sig_i64_ret_f64.params.push(AbiParam::new(types::I64));
    sig_i64_ret_f64.returns.push(AbiParam::new(types::F64));

    // f64(i64, i64) – tensor_get_float
    let mut sig_i64x2_ret_f64 = Signature::new(cc);
    sig_i64x2_ret_f64.params.push(AbiParam::new(types::I64));
    sig_i64x2_ret_f64.params.push(AbiParam::new(types::I64));
    sig_i64x2_ret_f64.returns.push(AbiParam::new(types::F64));

    // i64(i64, f64) – optim_sgd, optim_adam, add_scalar, mul_scalar, ...
    let mut sig_i64_f64_ret_i64 = Signature::new(cc);
    sig_i64_f64_ret_i64.params.push(AbiParam::new(types::I64));
    sig_i64_f64_ret_i64.params.push(AbiParam::new(types::F64));
    sig_i64_f64_ret_i64.returns.push(AbiParam::new(types::I64));

    // i64(i64, f64, f64) – tensor_clamp
    let mut sig_i64_f64x2_ret_i64 = Signature::new(cc);
    sig_i64_f64x2_ret_i64.params.push(AbiParam::new(types::I64));
    sig_i64_f64x2_ret_i64.params.push(AbiParam::new(types::F64));
    sig_i64_f64x2_ret_i64.params.push(AbiParam::new(types::F64));
    sig_i64_f64x2_ret_i64.returns.push(AbiParam::new(types::I64));

    // i64(i64, i64, f64) – tensor_masked_fill
    let mut sig_i64x2_f64_ret_i64 = Signature::new(cc);
    sig_i64x2_f64_ret_i64.params.push(AbiParam::new(types::I64));
    sig_i64x2_f64_ret_i64.params.push(AbiParam::new(types::I64));
    sig_i64x2_f64_ret_i64.params.push(AbiParam::new(types::F64));
    sig_i64x2_f64_ret_i64.returns.push(AbiParam::new(types::I64));

    // void(i64, f64) – nn_dropout
    let mut sig_void_i64_f64 = Signature::new(cc);
    sig_void_i64_f64.params.push(AbiParam::new(types::I64));
    sig_void_i64_f64.params.push(AbiParam::new(types::F64));

    let imports: Vec<(&str, &Signature)> = vec![
        ("nex_gc_alloc", &sig_gc_alloc),
        ("nex_gc_collect", &sig_void),
        ("nex_gc_safepoint", &sig_void),
        ("nex_throw", &sig_void_ptr),
        ("nex_print_str", &sig_void_ptr),
        ("nex_println_str", &sig_void_ptr),
        ("nex_print_int", &sig_void_ptr),
        ("nex_println_int", &sig_void_ptr),
        ("nex_print_double", &sig_void_f64),
        ("nex_println_double", &sig_void_f64),
        ("nex_print_bool", &sig_void_ptr),
        ("nex_println_bool", &sig_void_ptr),
        ("nex_print_char", &sig_void_ptr),
        ("nex_println_char", &sig_void_ptr),
        ("nex_str_concat", &sig_ptr_ptr2),
        ("nex_str_eq", &sig_ptr_ptr2),
        ("nex_int_to_str", &sig_ptr_ptr),
        ("nex_double_to_str", &sig_f64_ret_i64),
        ("nex_bool_to_str", &sig_ptr_ptr),
        ("nex_str_length", &sig_ptr_ptr),
        ("nex_str_substring", &sig_substring),
        ("nex_dispose", &sig_void_ptr),
        ("puts", &sig_void_ptr),
        // std.math
        ("nex_math_abs_int", &sig_ptr_ptr),
        ("nex_math_abs_float", &sig_f64_ret_f64),
        ("nex_math_min_int", &sig_ptr_ptr2),
        ("nex_math_max_int", &sig_ptr_ptr2),
        ("nex_math_min_float", &sig_f64x2_ret_f64),
        ("nex_math_max_float", &sig_f64x2_ret_f64),
        ("nex_math_clamp_int", &sig_ptr_ptr3),
        ("nex_math_clamp_float", &sig_f64x3_ret_f64),
        ("nex_math_floor", &sig_f64_ret_f64),
        ("nex_math_ceil", &sig_f64_ret_f64),
        ("nex_math_round", &sig_f64_ret_f64),
        ("nex_math_sqrt", &sig_f64_ret_f64),
        ("nex_math_pow", &sig_f64x2_ret_f64),
        ("nex_math_sin", &sig_f64_ret_f64),
        ("nex_math_cos", &sig_f64_ret_f64),
        ("nex_math_tan", &sig_f64_ret_f64),
        ("nex_math_log", &sig_f64_ret_f64),
        ("nex_math_log2", &sig_f64_ret_f64),
        ("nex_math_log10", &sig_f64_ret_f64),
        ("nex_math_exp", &sig_f64_ret_f64),
        ("nex_math_random", &sig_ret_f64),
        ("nex_math_random_range", &sig_ptr_ptr2),
        // std.string
        ("nex_str_split", &sig_ptr_ptr2),
        ("nex_str_trim", &sig_ptr_ptr),
        ("nex_str_trim_start", &sig_ptr_ptr),
        ("nex_str_trim_end", &sig_ptr_ptr),
        ("nex_str_starts_with", &sig_ptr_ptr2),
        ("nex_str_ends_with", &sig_ptr_ptr2),
        ("nex_str_contains", &sig_ptr_ptr2),
        ("nex_str_index_of", &sig_ptr_ptr2),
        ("nex_str_replace", &sig_ptr_ptr3),
        ("nex_str_to_upper", &sig_ptr_ptr),
        ("nex_str_to_lower", &sig_ptr_ptr),
        ("nex_str_repeat", &sig_ptr_ptr2),
        ("nex_str_char_at", &sig_ptr_ptr2),
        ("nex_str_reverse", &sig_ptr_ptr),
        ("nex_str_truncate", &sig_ptr_ptr2),
        // std.convert
        ("nex_parse_int", &sig_ptr_ptr),
        ("nex_parse_float", &sig_i64_ret_f64),
        ("nex_parse_bool", &sig_ptr_ptr),
        ("nex_char_to_str", &sig_ptr_ptr),
        ("nex_str_to_chars", &sig_ptr_ptr),
        // std.env
        ("nex_env_get", &sig_ptr_ptr),
        ("nex_env_set", &sig_void_ptr2),
        ("nex_env_has", &sig_ptr_ptr),
        ("nex_env_args_count", &sig_ret_ptr),
        ("nex_env_args_get", &sig_ptr_ptr),
        ("nex_env_cwd", &sig_ret_ptr),
        // std.time
        ("nex_time_now_millis", &sig_ret_ptr),
        ("nex_time_now_nanos", &sig_ret_ptr),
        ("nex_time_sleep_millis", &sig_void_ptr),
        ("nex_time_elapsed_millis", &sig_ptr_ptr),
        // std.collections — core List ops
        ("nex_list_new", &sig_ret_ptr),
        ("nex_list_add", &sig_void_ptr2),
        ("nex_list_get", &sig_ptr_ptr2),
        ("nex_list_set", &sig_void_ptr3),
        ("nex_list_length", &sig_ptr_ptr),
        ("nex_list_remove", &sig_ptr_ptr2),
        ("nex_list_free", &sig_void_ptr),
        // std.collections — extended
        ("nex_list_sort_int", &sig_void_ptr),
        ("nex_list_reverse", &sig_void_ptr),
        ("nex_list_clear", &sig_void_ptr),
        ("nex_list_contains_int", &sig_ptr_ptr2),
        ("nex_list_index_of_int", &sig_ptr_ptr2),
        ("nex_list_filter", &sig_ptr_ptr2),
        ("nex_list_map", &sig_ptr_ptr2),
        ("nex_list_foreach", &sig_void_ptr2),
        ("nex_list_contains_str", &sig_ptr_ptr2),
        ("nex_set_new", &sig_ret_ptr),
        ("nex_set_add", &sig_void_ptr2),
        ("nex_set_contains", &sig_ptr_ptr2),
        ("nex_set_remove", &sig_void_ptr2),
        ("nex_set_size", &sig_ptr_ptr),
        ("nex_map_new", &sig_ret_ptr),
        ("nex_map_put", &sig_void_ptr3),
        ("nex_map_get", &sig_ptr_ptr2),
        ("nex_map_contains", &sig_ptr_ptr2),
        ("nex_map_remove", &sig_void_ptr2),
        ("nex_map_size", &sig_ptr_ptr),
        ("nex_map_keys", &sig_ptr_ptr),
        ("nex_map_values", &sig_ptr_ptr),
        ("nex_map_free", &sig_void_ptr),
        // std.io
        ("nex_io_read_line", &sig_ret_ptr),
        ("nex_io_file_exists", &sig_ptr_ptr),
        ("nex_io_file_delete", &sig_ptr_ptr),
        ("nex_io_file_rename", &sig_ptr_ptr2),
        ("nex_io_file_copy", &sig_ptr_ptr2),
        ("nex_io_file_size", &sig_ptr_ptr),
        ("nex_io_file_read_bytes", &sig_ptr_ptr3),
        ("nex_io_file_write_bytes", &sig_ptr_ptr3),
        ("nex_io_file_append", &sig_ptr_ptr2),
        ("nex_io_mkdir", &sig_ptr_ptr),
        ("nex_io_list_dir", &sig_ptr_ptr),
        // std.path
        ("nex_path_join", &sig_ptr_ptr2),
        ("nex_path_parent", &sig_ptr_ptr),
        ("nex_path_file_name", &sig_ptr_ptr),
        ("nex_path_extension", &sig_ptr_ptr),
        ("nex_path_stem", &sig_ptr_ptr),
        ("nex_path_is_absolute", &sig_ptr_ptr),
        ("nex_path_normalize", &sig_ptr_ptr),
        ("nex_path_separator", &sig_ret_ptr),
        // std.json
        ("nex_json_parse", &sig_ptr_ptr),
        ("nex_json_stringify", &sig_ptr_ptr),
        ("nex_json_get_string", &sig_ptr_ptr2),
        ("nex_json_get_int", &sig_ptr_ptr2),
        ("nex_json_get_float", &sig_ptr_ptr2),
        ("nex_json_get_bool", &sig_ptr_ptr2),
        ("nex_json_new_object", &sig_ret_ptr),
        ("nex_json_set_string", &sig_void_ptr3),
        ("nex_json_set_int", &sig_void_ptr3),
        ("nex_json_set_float", &sig_void_i64x2_f64),
        ("nex_json_set_bool", &sig_void_ptr3),
        ("nex_json_set_null", &sig_void_ptr2),
        ("nex_json_stringify_pretty", &sig_ptr_ptr),
        ("nex_json_free", &sig_void_ptr),
        // std.regex
        ("nex_regex_new", &sig_ptr_ptr),
        ("nex_regex_is_match", &sig_ptr_ptr2),
        ("nex_regex_find", &sig_ptr_ptr2),
        ("nex_regex_replace", &sig_ptr_ptr3),
        ("nex_regex_free", &sig_void_ptr),
        // std.process
        ("nex_process_exec", &sig_ptr_ptr),
        ("nex_process_exec_output", &sig_ptr_ptr),
        ("nex_process_exit", &sig_void_ptr),
        ("nex_process_pid", &sig_ret_ptr),
        ("nex_process_spawn", &sig_ptr_ptr),
        ("nex_process_wait", &sig_ptr_ptr),
        // std.net
        ("nex_net_tcp_connect", &sig_ptr_ptr2),
        ("nex_net_tcp_close", &sig_void_ptr),
        ("nex_net_tcp_send", &sig_ptr_ptr3),
        ("nex_net_tcp_recv", &sig_ptr_ptr3),
        ("nex_net_tcp_listen", &sig_ptr_ptr2),
        ("nex_net_tcp_accept", &sig_ptr_ptr),
        ("nex_net_udp_bind", &sig_ptr_ptr2),
        ("nex_net_udp_close", &sig_void_ptr),
        ("nex_net_udp_send", &sig_ptr_ptr5),
        ("nex_net_udp_recv", &sig_ptr_ptr3),
        // std.http
        ("nex_http_get", &sig_ptr_ptr),
        ("nex_http_post", &sig_ptr_ptr3),
        ("nex_http_response_status", &sig_ptr_ptr),
        ("nex_http_response_body", &sig_ptr_ptr),
        ("nex_http_response_header", &sig_ptr_ptr2),
        ("nex_http_response_free", &sig_void_ptr),
        // std.threading
        ("nex_thread_spawn", &sig_ptr_ptr),
        ("nex_thread_join", &sig_ptr_ptr),
        ("nex_thread_sleep", &sig_void_ptr),
        ("nex_thread_current_id", &sig_ret_ptr),
        ("nex_mutex_new", &sig_ret_ptr),
        ("nex_mutex_lock", &sig_void_ptr),
        ("nex_mutex_unlock", &sig_void_ptr),
        ("nex_mutex_free", &sig_void_ptr),
        // std.async
        ("nex_task_spawn", &sig_ptr_ptr),
        ("nex_task_await", &sig_ptr_ptr),
        ("nex_task_is_done", &sig_ptr_ptr),
        // std.closure
        ("nex_closure_alloc", &sig_ptr_ptr2),
        ("nex_closure_set_cap", &sig_void_ptr3),
        ("nex_closure_get_cap", &sig_ptr_ptr2),
        ("nex_closure_get_fn", &sig_ptr_ptr),
        // std.crypto
        ("nex_crypto_sha256", &sig_ptr_ptr),
        ("nex_crypto_sha512", &sig_ptr_ptr),
        ("nex_crypto_md5", &sig_ptr_ptr),
        ("nex_crypto_random_bytes", &sig_void_ptr2),
        ("nex_crypto_base64_encode", &sig_ptr_ptr),
        ("nex_crypto_base64_decode", &sig_ptr_ptr),
        ("nex_crypto_hmac_sha256", &sig_ptr_ptr2),
        // std.logging
        ("nex_log_debug", &sig_void_ptr),
        ("nex_log_info", &sig_void_ptr),
        ("nex_log_warn", &sig_void_ptr),
        ("nex_log_error", &sig_void_ptr),
        ("nex_log_set_level", &sig_void_ptr),
        ("nex_log_with_tag", &sig_void_ptr2),
        // std.testing
        ("nex_assert", &sig_void_ptr2),
        ("nex_assert_eq_int", &sig_void_ptr3),
        ("nex_assert_eq_str", &sig_void_ptr3),
        ("nex_assert_eq_float", &sig_void_ptr3),
        ("nex_assert_eq_bool", &sig_void_ptr3),
        ("nex_assert_ne_int", &sig_void_ptr3),
        ("nex_assert_ne_str", &sig_void_ptr3),
        ("nex_assert_true", &sig_void_ptr2),
    ];

    // void(i64, i64, i64, i64) – four args, no return
    let mut sig_void_ptr4 = Signature::new(cc);
    for _ in 0..4 { sig_void_ptr4.params.push(AbiParam::new(types::I64)); }

    // void(i64, i64, i64, i64, i64, i64) – six args, no return
    let mut sig_void_ptr6 = Signature::new(cc);
    for _ in 0..6 { sig_void_ptr6.params.push(AbiParam::new(types::I64)); }

    // i64(i64, i64, i64, i64, i64, i64) – six args, one return
    let mut sig_ptr_ptr6 = Signature::new(cc);
    for _ in 0..6 { sig_ptr_ptr6.params.push(AbiParam::new(types::I64)); }
    sig_ptr_ptr6.returns.push(AbiParam::new(types::I64));

    // void(i64, i64, i64, i64, i64, i64, i64) – seven args, no return
    let mut sig_void_ptr7 = Signature::new(cc);
    for _ in 0..7 { sig_void_ptr7.params.push(AbiParam::new(types::I64)); }

    // void(i64 x 9) – nine args, no return (push_vertex_lit)
    let mut sig_void_ptr9 = Signature::new(cc);
    for _ in 0..9 { sig_void_ptr9.params.push(AbiParam::new(types::I64)); }

    // void(i64 x 11) – eleven args, no return (push_vertex_uv)
    let mut sig_void_ptr11 = Signature::new(cc);
    for _ in 0..11 { sig_void_ptr11.params.push(AbiParam::new(types::I64)); }

    // i64(i64, i64, i64, i64) – four args, one return
    let mut sig_ptr_ptr4 = Signature::new(cc);
    for _ in 0..4 { sig_ptr_ptr4.params.push(AbiParam::new(types::I64)); }
    sig_ptr_ptr4.returns.push(AbiParam::new(types::I64));

    let reflect_imports: Vec<(&str, &Signature)> = vec![
        // Registration
        ("nex_reflect_register_type", &sig_ptr_ptr4),    // (name, module, kind, is_reflectable) -> type_id
        ("nex_reflect_add_base", &sig_void_ptr2),         // (type_id, base_name)
        ("nex_reflect_add_interface", &sig_void_ptr2),    // (type_id, iface_name)
        ("nex_reflect_add_field", &sig_void_ptr4),        // (type_id, name, type_name, is_public)
        ("nex_reflect_add_method", &sig_void_ptr6),       // (type_id, name, ret_type, param_count, is_static, is_virtual)
        ("nex_reflect_add_variant", &sig_void_ptr3),      // (type_id, name, ordinal)
        // Query — single arg returning i64/ptr
        ("nex_reflect_find_type", &sig_ptr_ptr),          // (name) -> type_id
        ("nex_reflect_type_name", &sig_ptr_ptr),          // (type_id) -> *c_char
        ("nex_reflect_type_module", &sig_ptr_ptr),        // (type_id) -> *c_char
        ("nex_reflect_type_kind", &sig_ptr_ptr),          // (type_id) -> i64
        ("nex_reflect_type_field_count", &sig_ptr_ptr),   // (type_id) -> i64
        ("nex_reflect_type_method_count", &sig_ptr_ptr),  // (type_id) -> i64
        ("nex_reflect_type_is_reflectable", &sig_ptr_ptr),// (type_id) -> i64
        ("nex_reflect_type_interfaces", &sig_ptr_ptr),    // (type_id) -> *c_char
        // Query — two args returning i64/ptr
        ("nex_reflect_type_field_name", &sig_ptr_ptr2),   // (type_id, index) -> *c_char
        ("nex_reflect_type_field_type", &sig_ptr_ptr2),   // (type_id, index) -> *c_char
        ("nex_reflect_type_method_name", &sig_ptr_ptr2),  // (type_id, index) -> *c_char
        ("nex_reflect_type_method_return_type", &sig_ptr_ptr2), // (type_id, index) -> *c_char
        ("nex_reflect_type_implements", &sig_ptr_ptr2),   // (type_id, iface_name) -> i64
        // Enumeration
        ("nex_reflect_type_count", &sig_ret_ptr),         // () -> i64
        ("nex_reflect_type_name_at", &sig_ptr_ptr),       // (index) -> *c_char
        // Dynamic invocation
        ("nex_reflect_set_method_ptr", &sig_void_ptr3),   // (type_id, method_index, func_ptr)
        ("nex_reflect_invoke", &sig_ptr_ptr4),            // (type_id, method_name, args_ptr, arg_count) -> i64
        ("nex_reflect_create_instance", &sig_ptr_ptr3),   // (type_id, args_ptr, arg_count) -> i64
        ("nex_reflect_reset", &sig_void),                 // ()
        // Field value access
        ("nex_reflect_getFieldString", &sig_ptr_ptr2),    // (type_id, index) -> *c_char
        ("nex_reflect_getFieldInt", &sig_ptr_ptr2),       // (type_id, index) -> i64
        ("nex_reflect_getFieldFloat", &sig_i64x2_ret_f64),// (type_id, index) -> f64
        ("nex_reflect_getFieldBool", &sig_ptr_ptr2),      // (type_id, index) -> i64
        ("nex_reflect_setFieldString", &sig_void_ptr3),   // (type_id, index, value)
        ("nex_reflect_setFieldInt", &sig_void_ptr3),      // (type_id, index, value)
        ("nex_reflect_setFieldFloat", &sig_void_i64x2_f64),// (type_id, index, value: f64)
        ("nex_reflect_setFieldBool", &sig_void_ptr3),     // (type_id, index, value)
    ];

    let ui_imports: Vec<(&str, &Signature)> = vec![
        // Application
        ("nex_ui_app_create", &sig_ptr_ptr3),
        ("nex_ui_app_set_backend", &sig_void_ptr2),
        ("nex_ui_app_set_root", &sig_void_ptr2),
        ("nex_ui_app_run", &sig_void_ptr),
        ("nex_ui_app_quit", &sig_void_ptr),
        ("nex_ui_app_destroy", &sig_void_ptr),
        ("nex_ui_app_is_running", &sig_ptr_ptr),
        ("nex_ui_app_render", &sig_void_ptr),
        // Events
        ("nex_ui_poll_event", &sig_ptr_ptr),
        ("nex_ui_event_type", &sig_ptr_ptr),
        ("nex_ui_event_widget", &sig_ptr_ptr),
        // Widget creation
        ("nex_ui_text", &sig_ptr_ptr),
        ("nex_ui_button", &sig_ptr_ptr),
        ("nex_ui_text_input", &sig_ptr_ptr),
        ("nex_ui_image", &sig_ptr_ptr),
        ("nex_ui_checkbox", &sig_ptr_ptr),
        ("nex_ui_slider", &sig_ptr_ptr2),
        ("nex_ui_row", &sig_ret_ptr),
        ("nex_ui_column", &sig_ret_ptr),
        ("nex_ui_stack", &sig_ret_ptr),
        ("nex_ui_scroll", &sig_ret_ptr),
        ("nex_ui_grid", &sig_ptr_ptr),
        ("nex_ui_canvas", &sig_ptr_ptr2),
        // Widget tree
        ("nex_ui_add_child", &sig_void_ptr2),
        ("nex_ui_remove_child", &sig_void_ptr2),
        ("nex_ui_set_id", &sig_void_ptr2),
        ("nex_ui_get_id", &sig_ptr_ptr),
        // Widget properties
        ("nex_ui_set_text", &sig_void_ptr2),
        ("nex_ui_get_text", &sig_ptr_ptr),
        ("nex_ui_set_visible", &sig_void_ptr2),
        ("nex_ui_set_enabled", &sig_void_ptr2),
        ("nex_ui_get_value_float", &sig_ptr_ptr),
        ("nex_ui_set_value_float", &sig_void_ptr2),
        // Styling
        ("nex_ui_set_width", &sig_void_ptr2),
        ("nex_ui_set_height", &sig_void_ptr2),
        ("nex_ui_set_min_width", &sig_void_ptr2),
        ("nex_ui_set_min_height", &sig_void_ptr2),
        ("nex_ui_set_padding", &sig_void_ptr2),
        ("nex_ui_set_padding_all", &sig_void_ptr2),
        ("nex_ui_set_margin", &sig_void_ptr2),
        ("nex_ui_set_bg_color", &sig_void_ptr2),
        ("nex_ui_set_fg_color", &sig_void_ptr2),
        ("nex_ui_set_font_size", &sig_void_ptr2),
        ("nex_ui_set_border", &sig_void_ptr3),
        ("nex_ui_set_border_radius", &sig_void_ptr2),
        ("nex_ui_set_flex_grow", &sig_void_ptr2),
        ("nex_ui_set_align_self", &sig_void_ptr2),
        ("nex_ui_set_justify_content", &sig_void_ptr2),
        ("nex_ui_set_align_items", &sig_void_ptr2),
        ("nex_ui_set_gap", &sig_void_ptr2),
        // Callbacks
        ("nex_ui_on_click", &sig_void_ptr2),
        ("nex_ui_on_change", &sig_void_ptr2),
        ("nex_ui_on_hover", &sig_void_ptr2),
        ("nex_ui_on_key", &sig_void_ptr2),
        // Canvas
        ("nex_ui_canvas_fill_rect", &sig_void_ptr6),
        ("nex_ui_canvas_stroke_rect", &sig_void_ptr7),
        ("nex_ui_canvas_fill_circle", &sig_ptr_ptr5),
        ("nex_ui_canvas_draw_line", &sig_void_ptr7),
        ("nex_ui_canvas_draw_text", &sig_void_ptr6),
        ("nex_ui_canvas_clear", &sig_void_ptr2),
        // Dialogs
        ("nex_ui_dialog_message", &sig_void_ptr2),
        ("nex_ui_dialog_confirm", &sig_ptr_ptr2),
        ("nex_ui_dialog_open_file", &sig_ptr_ptr2),
        ("nex_ui_dialog_save_file", &sig_ptr_ptr2),
        // Binding engine
        ("nex_ui_bind", &sig_void_ptr2),
        ("nex_ui_unbind", &sig_void_ptr),
        ("nex_ui_notify_changed", &sig_void_ptr),
        ("nex_ui_bindings_clear", &sig_void),
        // Overlay
        ("nex_ui_overlay_init", &sig_void_ptr2),
        ("nex_ui_overlay_render", &sig_ptr_ptr2),
        ("nex_ui_overlay_mouse_move", &sig_void_ptr2),
        ("nex_ui_overlay_mouse_down", &sig_void_ptr2),
        ("nex_ui_overlay_mouse_up", &sig_void_ptr2),
        ("nex_ui_overlay_key_char", &sig_void_ptr),
        ("nex_ui_overlay_key_name", &sig_void_ptr),
        ("nex_ui_overlay_hit_test", &sig_ptr_ptr2),
    ];

    let engine_imports: Vec<(&str, &Signature)> = vec![
        // Lifecycle
        ("nex_engine_window_create", &sig_ptr_ptr3),
        ("nex_engine_window_run", &sig_void_ptr),
        ("nex_engine_window_quit", &sig_void_ptr),
        ("nex_engine_window_destroy", &sig_void_ptr),
        ("nex_engine_window_is_running", &sig_ptr_ptr),
        // Config
        ("nex_engine_clear_color", &sig_void_ptr4),
        ("nex_engine_set_update_fn", &sig_void_ptr),
        ("nex_engine_window_width", &sig_ret_ptr),
        ("nex_engine_window_height", &sig_ret_ptr),
        // Input — keyboard
        ("nex_engine_key_down", &sig_ptr_ptr),
        ("nex_engine_key_pressed", &sig_ptr_ptr),
        ("nex_engine_key_released", &sig_ptr_ptr),
        // Input — mouse
        ("nex_engine_mouse_x", &sig_ret_ptr),
        ("nex_engine_mouse_y", &sig_ret_ptr),
        ("nex_engine_mouse_delta_x", &sig_ret_ptr),
        ("nex_engine_mouse_delta_y", &sig_ret_ptr),
        ("nex_engine_mouse_button_down", &sig_ptr_ptr),
        ("nex_engine_mouse_button_pressed", &sig_ptr_ptr),
        // Timing
        ("nex_engine_delta_time", &sig_ret_ptr),
        ("nex_engine_elapsed_time", &sig_ret_ptr),
        ("nex_engine_frame_count", &sig_ret_ptr),
        // Camera
        ("nex_engine_set_camera_pos", &sig_void_ptr3),
        ("nex_engine_set_camera_target", &sig_void_ptr3),
        ("nex_engine_set_camera_up", &sig_void_ptr3),
        ("nex_engine_set_perspective", &sig_void_ptr4),
        // Drawing
        ("nex_engine_push_vertex", &sig_void_ptr6),
        ("nex_engine_push_vertex_lit", &sig_void_ptr9),
        ("nex_engine_draw_triangles", &sig_void),
        // Lighting
        ("nex_engine_set_ambient_color", &sig_void_ptr3),
        ("nex_engine_light_set_type", &sig_void_ptr2),
        ("nex_engine_light_set_enabled", &sig_void_ptr2),
        ("nex_engine_light_set_position", &sig_void_ptr4),
        ("nex_engine_light_set_direction", &sig_void_ptr4),
        ("nex_engine_light_set_color", &sig_void_ptr4),
        ("nex_engine_light_set_intensity", &sig_void_ptr2),
        ("nex_engine_light_set_range", &sig_void_ptr2),
        ("nex_engine_light_set_spot_angles", &sig_void_ptr3),
        ("nex_engine_clear_lights", &sig_void),
        // Texture
        ("nex_engine_push_vertex_uv", &sig_void_ptr11),
        ("nex_engine_texture_load", &sig_ptr_ptr),
        ("nex_engine_texture_bind", &sig_void_ptr),
        ("nex_engine_texture_unbind", &sig_void),
        ("nex_engine_texture_width", &sig_ptr_ptr),
        ("nex_engine_texture_height", &sig_ptr_ptr),
        // SpriteBatch
        ("nex_engine_spritebatch_begin", &sig_void),
        ("nex_engine_spritebatch_end", &sig_void),
        ("nex_engine_spritebatch_draw", &sig_void_ptr9),
        ("nex_engine_spritebatch_draw_src", &sig_void_ptr9),
        // Font
        ("nex_engine_font_draw_text", &sig_void_ptr7),
        ("nex_engine_font_measure_text", &sig_ptr_ptr2),
        // Render States
        ("nex_engine_set_blend_mode", &sig_void_ptr),
        ("nex_engine_set_cull_mode", &sig_void_ptr),
        ("nex_engine_set_fill_mode", &sig_void_ptr),
        ("nex_engine_set_depth_enabled", &sig_void_ptr),
        // Mouse scroll
        ("nex_engine_mouse_scroll_delta", &sig_ret_ptr),
        // Audio
        ("nex_engine_audio_load", &sig_ptr_ptr),          // path_ptr → handle
        ("nex_engine_audio_play", &sig_void_ptr),          // handle
        ("nex_engine_audio_play_looped", &sig_void_ptr),   // handle
        ("nex_engine_audio_stop", &sig_void_ptr),          // handle
        ("nex_engine_audio_set_volume", &sig_void_ptr2),   // handle, volume_bits
        ("nex_engine_audio_is_playing", &sig_ptr_ptr),     // handle → 0/1
        ("nex_engine_audio_free", &sig_void_ptr),          // handle
        // Model loading
        ("nex_engine_model_load", &sig_ptr_ptr),           // path_ptr → handle
        ("nex_engine_model_draw", &sig_void_ptr),          // handle
        ("nex_engine_model_vertex_count", &sig_ptr_ptr),   // handle → count
        ("nex_engine_model_free", &sig_void_ptr),          // handle
        // Skeletal Animation
        ("nex_engine_anim_model_load", &sig_ptr_ptr),      // path_ptr → handle
        ("nex_engine_anim_model_draw", &sig_void_ptr),     // handle
        ("nex_engine_anim_play", &sig_void_ptr2),          // handle, clip
        ("nex_engine_anim_stop", &sig_void_ptr),           // handle
        ("nex_engine_anim_pause", &sig_void_ptr),          // handle
        ("nex_engine_anim_set_speed", &sig_void_ptr2),     // handle, speed_bits
        ("nex_engine_anim_set_looping", &sig_void_ptr2),   // handle, flag
        ("nex_engine_anim_set_time", &sig_void_ptr2),      // handle, time_bits
        ("nex_engine_anim_get_time", &sig_ptr_ptr),        // handle → time
        ("nex_engine_anim_clip_count", &sig_ptr_ptr),      // handle → count
        ("nex_engine_anim_clip_duration", &sig_ptr_ptr2),  // handle, clip → duration
        ("nex_engine_anim_joint_count", &sig_ptr_ptr),     // handle → count
        ("nex_engine_anim_model_free", &sig_void_ptr),     // handle
        // RenderTarget
        ("nex_engine_rendertarget_create", &sig_ptr_ptr2), // w, h → handle
        ("nex_engine_rendertarget_bind", &sig_void_ptr),   // handle
        ("nex_engine_rendertarget_unbind", &sig_void),
        ("nex_engine_rendertarget_as_texture", &sig_ptr_ptr), // handle → tex_handle
        ("nex_engine_rendertarget_width", &sig_ptr_ptr),   // handle → width
        ("nex_engine_rendertarget_height", &sig_ptr_ptr),  // handle → height
        ("nex_engine_rendertarget_free", &sig_void_ptr),   // handle
        // Gamepad
        ("nex_engine_gamepad_connected", &sig_ptr_ptr),    // player → 0/1
        ("nex_engine_gamepad_button", &sig_ptr_ptr2),      // player, button → 0/1
        ("nex_engine_gamepad_axis", &sig_ptr_ptr2),        // player, axis → f64 (as bits)
        // UI Overlay
        ("nex_engine_enable_ui_overlay", &sig_void),
    ];

    let torch_imports: Vec<(&str, &Signature)> = vec![
        // Tensor creation
        ("nex_torch_tensor_zeros", &sig_ptr_ptr2),
        ("nex_torch_tensor_ones", &sig_ptr_ptr2),
        ("nex_torch_tensor_rand", &sig_ptr_ptr2),
        ("nex_torch_tensor_randn", &sig_ptr_ptr2),
        ("nex_torch_tensor_from_float_data", &sig_ptr_ptr3),
        ("nex_torch_tensor_arange", &sig_f64x3_ret_i64),
        ("nex_torch_tensor_eye", &sig_ptr_ptr),
        ("nex_torch_tensor_free", &sig_void_ptr),
        // Tensor operations
        ("nex_torch_tensor_add", &sig_ptr_ptr2),
        ("nex_torch_tensor_sub", &sig_ptr_ptr2),
        ("nex_torch_tensor_mul", &sig_ptr_ptr2),
        ("nex_torch_tensor_div", &sig_ptr_ptr2),
        ("nex_torch_tensor_matmul", &sig_ptr_ptr2),
        ("nex_torch_tensor_neg", &sig_ptr_ptr),
        ("nex_torch_tensor_exp", &sig_ptr_ptr),
        ("nex_torch_tensor_log", &sig_ptr_ptr),
        ("nex_torch_tensor_sum", &sig_ptr_ptr),
        ("nex_torch_tensor_mean", &sig_ptr_ptr),
        ("nex_torch_tensor_reshape", &sig_ptr_ptr3),
        ("nex_torch_tensor_transpose", &sig_ptr_ptr3),
        ("nex_torch_tensor_squeeze", &sig_ptr_ptr),
        ("nex_torch_tensor_unsqueeze", &sig_ptr_ptr2),
        ("nex_torch_tensor_print", &sig_void_ptr),
        ("nex_torch_tensor_shape_dim", &sig_ptr_ptr2),
        // Tensor data access
        ("nex_torch_tensor_get_float", &sig_i64x2_ret_f64),
        ("nex_torch_tensor_item_float", &sig_i64_ret_f64),
        ("nex_torch_tensor_ndim", &sig_ptr_ptr),
        ("nex_torch_tensor_numel", &sig_ptr_ptr),
        // Device management
        ("nex_torch_cuda_is_available", &sig_ret_ptr),
        ("nex_torch_cuda_device_count", &sig_ret_ptr),
        ("nex_torch_tensor_to_device", &sig_ptr_ptr2),
        ("nex_torch_set_num_threads", &sig_void_ptr),
        // Autograd
        ("nex_torch_tensor_requires_grad", &sig_ptr_ptr2),
        ("nex_torch_tensor_backward", &sig_void_ptr),
        ("nex_torch_tensor_grad", &sig_ptr_ptr),
        ("nex_torch_no_grad", &sig_void_ptr),
        // NN layers
        ("nex_torch_nn_sequential_new", &sig_ret_ptr),
        ("nex_torch_nn_linear", &sig_void_ptr3),
        ("nex_torch_nn_conv2d", &sig_void_ptr4),
        ("nex_torch_nn_relu", &sig_void_ptr),
        ("nex_torch_nn_sigmoid", &sig_void_ptr),
        ("nex_torch_nn_tanh", &sig_void_ptr),
        ("nex_torch_nn_softmax", &sig_void_ptr2),
        ("nex_torch_nn_dropout", &sig_void_i64_f64),
        ("nex_torch_nn_batch_norm", &sig_void_ptr2),
        ("nex_torch_nn_to_device", &sig_void_ptr2),
        ("nex_torch_nn_forward", &sig_ptr_ptr2),
        ("nex_torch_nn_free", &sig_void_ptr),
        // Loss functions
        ("nex_torch_loss_mse", &sig_ptr_ptr2),
        ("nex_torch_loss_cross_entropy", &sig_ptr_ptr2),
        ("nex_torch_loss_bce", &sig_ptr_ptr2),
        // Optimizers
        ("nex_torch_optim_sgd", &sig_i64_f64_ret_i64),
        ("nex_torch_optim_adam", &sig_i64_f64_ret_i64),
        ("nex_torch_optim_step", &sig_void_ptr),
        ("nex_torch_optim_zero_grad", &sig_void_ptr),
        ("nex_torch_optim_free", &sig_void_ptr),
        // Model I/O
        ("nex_torch_model_save", &sig_void_ptr2),
        ("nex_torch_model_load", &sig_void_ptr2),
        ("nex_torch_jit_load", &sig_ptr_ptr),
        ("nex_torch_jit_forward", &sig_ptr_ptr2),
        // Extended tensor ops — scalar arithmetic
        ("nex_torch_tensor_add_scalar", &sig_i64_f64_ret_i64),
        ("nex_torch_tensor_mul_scalar", &sig_i64_f64_ret_i64),
        ("nex_torch_tensor_div_scalar", &sig_i64_f64_ret_i64),
        ("nex_torch_tensor_pow_scalar", &sig_i64_f64_ret_i64),
        // Extended tensor ops — element-wise math
        ("nex_torch_tensor_sqrt", &sig_ptr_ptr),
        ("nex_torch_tensor_abs", &sig_ptr_ptr),
        ("nex_torch_tensor_clamp", &sig_i64_f64x2_ret_i64),
        ("nex_torch_tensor_softmax", &sig_ptr_ptr2),
        // Extended tensor ops — comparison
        ("nex_torch_tensor_eq_scalar", &sig_i64_f64_ret_i64),
        ("nex_torch_tensor_gt_scalar", &sig_i64_f64_ret_i64),
        ("nex_torch_tensor_lt_scalar", &sig_i64_f64_ret_i64),
        // Extended tensor ops — masking and triangular
        ("nex_torch_tensor_tril", &sig_ptr_ptr2),
        ("nex_torch_tensor_triu", &sig_ptr_ptr2),
        ("nex_torch_tensor_masked_fill", &sig_i64x2_f64_ret_i64),
        ("nex_torch_tensor_where_self", &sig_ptr_ptr3),
        // Extended tensor ops — reduction with dimension
        ("nex_torch_tensor_sum_dim", &sig_ptr_ptr3),
        ("nex_torch_tensor_mean_dim", &sig_ptr_ptr3),
        ("nex_torch_tensor_argmax", &sig_ptr_ptr2),
        ("nex_torch_tensor_max_dim", &sig_ptr_ptr2),
        ("nex_torch_tensor_min_dim", &sig_ptr_ptr2),
        // Extended tensor ops — shape and indexing
        ("nex_torch_tensor_cat", &sig_ptr_ptr3),
        ("nex_torch_tensor_narrow", &sig_ptr_ptr4),
        ("nex_torch_tensor_index_select", &sig_ptr_ptr3),
        ("nex_torch_tensor_flatten", &sig_ptr_ptr3),
        // Extended tensor ops — creation helpers
        ("nex_torch_tensor_ones_like", &sig_ptr_ptr),
        ("nex_torch_tensor_zeros_like", &sig_ptr_ptr),
        ("nex_torch_tensor_full_like", &sig_i64_f64_ret_i64),
        // Extended tensor ops — utility
        ("nex_torch_tensor_clone", &sig_ptr_ptr),
        ("nex_torch_tensor_detach", &sig_ptr_ptr),
        ("nex_torch_tensor_contiguous", &sig_ptr_ptr),
        ("nex_torch_tensor_to_dtype_float", &sig_ptr_ptr),
        ("nex_torch_tensor_to_dtype_long", &sig_ptr_ptr),
        // Extended NN layers
        ("nex_torch_nn_layer_norm", &sig_void_ptr2),
        ("nex_torch_nn_gelu", &sig_void_ptr),
        ("nex_torch_nn_embedding", &sig_void_ptr3),
        // Utility
        ("nex_torch_manual_seed", &sig_void_ptr),
        ("nex_torch_version", &sig_ret_ptr),
        ("nex_torch_tensor_to_string", &sig_ptr_ptr),
    ];

    // Helper: declare an import only if not already declared, and if it passes
    // the optional filter (when building shared libs).
    let mut declare = |name: &str, sig: &Signature| -> Result<(), String> {
        if func_ids.contains_key(name) {
            return Ok(());
        }
        if let Some(f) = filter {
            if !f.contains(name) {
                return Ok(());
            }
        }
        let id = module
            .declare_function(name, Linkage::Import, sig)
            .map_err(|e| format!("declare import {name}: {e}"))?;
        func_ids.insert(name.to_string(), id);
        Ok(())
    };

    for (name, sig) in imports {
        declare(name, sig)?;
    }
    for (name, sig) in reflect_imports {
        declare(name, sig)?;
    }
    for (name, sig) in ui_imports {
        declare(name, sig)?;
    }
    for (name, sig) in engine_imports {
        declare(name, sig)?;
    }
    for (name, sig) in torch_imports {
        declare(name, sig)?;
    }

    let crypto_imports: Vec<(&str, &Signature)> = vec![
        ("nex_crypto_sha256", &sig_ptr_ptr),
        ("nex_crypto_sha512", &sig_ptr_ptr),
        ("nex_crypto_md5", &sig_ptr_ptr),
        ("nex_crypto_random_bytes", &sig_void_ptr2),
        ("nex_crypto_base64_encode", &sig_ptr_ptr),
        ("nex_crypto_base64_decode", &sig_ptr_ptr),
        ("nex_crypto_hmac_sha256", &sig_ptr_ptr2),
    ];

    let http_imports: Vec<(&str, &Signature)> = vec![
        ("nex_http_get", &sig_ptr_ptr),
        ("nex_http_post", &sig_ptr_ptr3),
        ("nex_http_response_status", &sig_ptr_ptr),
        ("nex_http_response_body", &sig_ptr_ptr),
        ("nex_http_response_header", &sig_ptr_ptr2),
        ("nex_http_response_free", &sig_void_ptr),
    ];

    let regex_imports: Vec<(&str, &Signature)> = vec![
        ("nex_regex_new", &sig_ptr_ptr),
        ("nex_regex_is_match", &sig_ptr_ptr2),
        ("nex_regex_find", &sig_ptr_ptr2),
        ("nex_regex_replace", &sig_ptr_ptr3),
        ("nex_regex_free", &sig_void_ptr),
    ];

    for imports_list in [crypto_imports, http_imports, regex_imports] {
        for (name, sig) in imports_list {
            declare(name, sig)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{runtime_func_return_type, stdlib_function_name, RegType};

    #[test]
    fn maps_string_length_and_substring_runtime_symbols() {
        assert_eq!(stdlib_function_name("str_length"), Some("nex_str_length"));
        assert_eq!(stdlib_function_name("str_substring"), Some("nex_str_substring"));
    }

    #[test]
    fn treats_qualified_ui_get_text_as_string_return() {
        assert_eq!(runtime_func_return_type("ui_get_text"), Some(RegType::String));
        assert_eq!(runtime_func_return_type("nex_ui_get_text"), Some(RegType::String));
        assert_eq!(runtime_func_return_type("MainWindow::ui_get_text"), Some(RegType::String));
        assert_eq!(runtime_func_return_type("MainWindow::nex_ui_get_text"), Some(RegType::String));
        assert_eq!(runtime_func_return_type("std.ui.ui_get_text"), Some(RegType::String));
        assert_eq!(runtime_func_return_type("std.ui.nex_ui_get_text"), Some(RegType::String));
    }
}

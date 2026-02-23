use std::fmt::Display;

pub mod core {
    use super::*;

    pub fn print<T: Display>(value: T) {
        print!("{value}");
    }

    pub fn println<T: Display>(value: T) {
        std::println!("{value}");
    }
}

pub mod runtime {
    pub use nex_runtime::*;

    pub trait Disposable {
        fn dispose(&mut self);
    }

    pub fn gc_collect() {
        unsafe { nex_runtime::nex_gc_collect(); }
    }
}

pub mod collections {
    use std::collections::HashMap;
    use std::hash::Hash;

    pub struct List<T> {
        inner: Vec<T>,
    }

    impl<T> List<T> {
        pub fn new() -> Self {
            Self { inner: Vec::new() }
        }

        pub fn add(&mut self, item: T) {
            self.inner.push(item);
        }

        pub fn get(&self, index: usize) -> Option<&T> {
            self.inner.get(index)
        }

        pub fn set(&mut self, index: usize, value: T) {
            if index < self.inner.len() {
                self.inner[index] = value;
            }
        }

        pub fn length(&self) -> usize {
            self.inner.len()
        }

        pub fn remove(&mut self, index: usize) -> Option<T> {
            if index < self.inner.len() {
                Some(self.inner.remove(index))
            } else {
                None
            }
        }
    }

    impl<T> Default for List<T> {
        fn default() -> Self {
            Self::new()
        }
    }

    pub struct Map<K, V> {
        inner: HashMap<K, V>,
    }

    impl<K: Eq + Hash, V> Map<K, V> {
        pub fn new() -> Self {
            Self { inner: HashMap::new() }
        }

        pub fn put(&mut self, key: K, value: V) {
            self.inner.insert(key, value);
        }

        pub fn get(&self, key: &K) -> Option<&V> {
            self.inner.get(key)
        }

        pub fn try_get(&self, key: &K) -> (bool, Option<&V>) {
            match self.inner.get(key) {
                Some(v) => (true, Some(v)),
                None => (false, None),
            }
        }

        pub fn contains(&self, key: &K) -> bool {
            self.inner.contains_key(key)
        }

        pub fn remove(&mut self, key: &K) -> Option<V> {
            self.inner.remove(key)
        }

        pub fn size(&self) -> usize {
            self.inner.len()
        }
    }

    impl<K: Eq + Hash, V> Default for Map<K, V> {
        fn default() -> Self {
            Self::new()
        }
    }
}

pub mod io {
    use super::runtime::Disposable;
    use std::fs;
    use std::io::Write;

    pub struct File {
        path: String,
        handle: Option<fs::File>,
    }

    impl File {
        pub fn open(path: &str) -> Result<Self, String> {
            let handle = fs::OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .open(path)
                .map_err(|e| e.to_string())?;
            Ok(Self {
                path: path.to_string(),
                handle: Some(handle),
            })
        }

        pub fn read_all(&self) -> Result<String, String> {
            fs::read_to_string(&self.path).map_err(|e| e.to_string())
        }

        pub fn write_text(&mut self, text: &str) -> Result<(), String> {
            if let Some(handle) = &mut self.handle {
                handle.write_all(text.as_bytes()).map_err(|e| e.to_string())
            } else {
                Err("file is closed".into())
            }
        }
    }

    impl Disposable for File {
        fn dispose(&mut self) {
            self.handle.take();
        }
    }

    impl Drop for File {
        fn drop(&mut self) {
            self.dispose();
        }
    }
}

pub mod math {
    pub use nex_runtime::math::*;
}

pub mod string {
    pub use nex_runtime::string::*;
}

pub mod convert {
    pub use nex_runtime::convert::*;
}

pub mod env {
    pub use nex_runtime::env::*;
}

pub mod time {
    pub use nex_runtime::time::*;
}

pub mod path {
    pub use nex_runtime::path::*;
}

pub mod json {
    pub use nex_runtime::json::*;
}

pub mod regex {
    pub use nex_runtime::regex_ext::*;
}

pub mod process {
    pub use nex_runtime::process::*;
}

pub mod net {
    pub use nex_runtime::net::*;
}

pub mod http {
    pub use nex_runtime::http::*;
}

pub mod threading {
    pub use nex_runtime::threading::*;
}

pub mod crypto {
    pub use nex_runtime::crypto::*;
}

pub mod logging {
    pub use nex_runtime::logging::*;
}

pub mod testing {
    pub use nex_runtime::testing::*;
}

#[cfg(feature = "torch")]
pub mod torch {
    pub use nex_runtime::torch::*;
}

#[cfg(feature = "ui")]
pub mod ui {
    pub use nex_runtime::ui::*;
}

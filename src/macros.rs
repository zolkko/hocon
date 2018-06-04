
#[macro_export]
macro_rules! object {
    ($($k: expr => $v: expr),*) => {{
        let mut obj = $crate::config::Object::new();
        $( obj.insert($k, $v); )*
        obj
    }}
}

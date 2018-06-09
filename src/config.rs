use std::collections::BTreeMap;


trait Path {
}


trait Config<'a> {

    /// Get values as a boolean
    fn get_boolean<P: Path>(&self, path: &P) -> Option<bool>;

    /// Gets a list value with boolean elements.
    fn get_boolean_list<P: Path>(&self, path: &P) -> Option<&'a [bool]>;

    /// Gets a value as a size in bytes (parses special strings like "128M").
    fn get_bytes<P: Path>(&self, path: &P) -> Option<u64>;

    /// Gets a list value with elements representing a size in bytes.
    fn get_bytes_list<P: Path>(&self, path: &P) -> Option<&'a [u64]>;

    fn get_config<P: Path>(&self, path: &P) -> Option<&'a Self>;
}


pub struct IncludeError {
}

pub type IncludeResult = Result<ConfigValue, IncludeError>;


pub struct ParseError {
}

pub type ParseResult = Result<ConfigValue, ParseError>;


/// Representation of a Hocon value.
#[derive(PartialEq, Clone, Debug)]
pub enum ConfigValue {
    /// Represents a Hocon value
    Value(String),
    /// Represents a Hocon array
    Array(Array),
    /// Represents a Hocon object
    Object(Object),
}

/// Type representing a Hocon array, payload of the `Config::Array` variant
pub type Array = Vec<ConfigValue>;

/// Type representing a Hocon table, payload of the `Config::Object` variant
pub type Object = BTreeMap<String, ConfigValue>;


fn merge_value(dest: &mut Object, key: &str, value: &ConfigValue) {
    let insert = match dest.get_mut(key) {
        Some(dest_value) => match dest_value {
            ConfigValue::Object(ref mut dest_obj) => match value {
                ConfigValue::Object(ref src_obj) => {
                    merge_objects(dest_obj, src_obj);
                    false
                }
                _ => true,
            },
            ConfigValue::Array(ref mut dest_array) => match value {
                ConfigValue::Array(ref src_array) => {
                    merge_arrays(dest_array, src_array);
                    false
                }
                _ => true,
            },
            ConfigValue::Value(_) => true,
        },
        None => true,
    };

    if insert {
        dest.insert(key.to_string(), value.clone());
    }
}

fn merge_arrays(first: &mut Array, second: &Array) {
    first.extend(second.iter().map(|x| x.clone()));
}

/// The function recursively merges two objects together.
/// Source overrides destination keys if their types differ.
pub(crate) fn merge_objects(dest: &mut Object, src: &Object) {
    for (key, value) in src.iter() {
        merge_value(dest, key, value);
    }
}

pub(crate) fn put_value<'a>(dest: &mut Object, path: &[&'a str], value: &ConfigValue) {

    if path.len() == 0 {
        ()
    } else if path.len() == 1 {
        merge_value(dest, path[0], value)
    } else {
        let (k, rest) = path.split_first().unwrap();
        let (last_k, rest) = rest.split_last().unwrap();

        let mut obj = Object::new();
        obj.insert(last_k.to_string(), value.clone());

        for key in rest.iter().rev() {
            let mut next_obj = Object::new();
            next_obj.insert(key.to_string(), ConfigValue::Object(obj));

            obj = next_obj;
        }

        if !dest.contains_key(&k.to_string()) {
            dest.insert(k.to_string(), ConfigValue::Object(obj));
        } else {
            merge_value(dest, k, value);
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_put_value() {
        let mut dest = Object::new();
        let value = ConfigValue::Value("a value".into());
        put_value(&mut dest, &["key"], &value);

        assert_eq!(dest.get("key".into()), Some(&ConfigValue::Value("a value".into())));

        let mut dest = Object::new();
        let value = ConfigValue::Value("a value".into());
        put_value(&mut dest, &["key", "sub"], &value);

        assert_eq!(dest.get("key".into()), Some(&ConfigValue::Object(
            object![
                "sub".into() => ConfigValue::Value("a value".into())
            ]
        )))
    }

    #[test]
    fn test_merge_value() {
        let mut dest = Object::new();
        merge_value(&mut dest, &"key", &ConfigValue::Value("a value".to_string()));
        assert_eq!(dest.get(&"key".to_string()), Some(&ConfigValue::Value("a value".to_string())));

        let mut dest = object!["key".to_string() => ConfigValue::Value("old value".to_string())];
        merge_value(&mut dest, &"key".to_string(), &ConfigValue::Value("new value".to_string()));
        assert_eq!(dest.get(&"key".to_string()), Some(&ConfigValue::Value("new value".to_string())));

        let mut dest = object!["key".to_string() => ConfigValue::Array(vec![ConfigValue::Value("old value".to_string())])];
        merge_value(&mut dest, &"key", &ConfigValue::Array(vec![ConfigValue::Value("new value".to_string())]));
        assert_eq!(
            dest.get(&"key".to_string()),
            Some(&ConfigValue::Array(vec![ConfigValue::Value("old value".to_string()), ConfigValue::Value("new value".to_string())]))
        );

        let mut dest = object![
            "key".to_string() => ConfigValue::Array(vec![ConfigValue::Value("old value".to_string())])
        ];
        merge_value(&mut dest, &"key", &ConfigValue::Value("new value".to_string()));
        assert_eq!(dest.get(&"key".to_string()), Some(&ConfigValue::Value("new value".to_string())));

        let mut dest = object![
            "key".to_string() => ConfigValue::Object(object![
                "key1".to_string() => ConfigValue::Value("old sub value".to_string()),
                "key3".to_string() => ConfigValue::Array(vec![ConfigValue::Value("array-value-1".to_string())]),
                "key4".to_string() => ConfigValue::Value("old value 4".to_string())
            ])
        ];

        merge_value(&mut dest, &"key".to_string(), &ConfigValue::Object(object![
            "key1".to_string() => ConfigValue::Value("new sub value".to_string()),
            "key2".to_string() => ConfigValue::Value("value2".to_string()),
            "key3".to_string() => ConfigValue::Array(vec![ConfigValue::Value("array-value-2".to_string())])
        ]));

        assert_eq!(
            dest,
            object![
                "key".to_string() => ConfigValue::Object(object![
                    "key1".to_string() => ConfigValue::Value("new sub value".to_string()),
                    "key2".to_string() => ConfigValue::Value("value2".to_string()),
                    "key3".to_string() => ConfigValue::Array(vec![
                        ConfigValue::Value("array-value-1".to_string()),
                        ConfigValue::Value("array-value-2".to_string())
                    ]),
                    "key4".to_string() => ConfigValue::Value("old value 4".to_string())
                ])
            ],
        );
    }

}
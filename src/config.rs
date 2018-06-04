use std::collections::BTreeMap;

pub enum Include {
    Any(String),
    Url(String),
    Classpath(String),
    File(String)
}

pub struct IncludeError {
}

pub type IncludeResult = Result<Config, IncludeError>;


pub struct ParseError {
}

pub type ParseResult = Result<Config, ParseError>;


/// Representation of a Hocon value.
#[derive(PartialEq, Clone, Debug)]
pub enum Config {
    /// Represents a Hocon value
    Value(String),
    /// Represents a Hocon array
    Array(Array),
    /// Represents a Hocon object
    Object(Object),
}

/// Type representing a Hocon array, payload of the `Config::Array` variant
pub type Array = Vec<Config>;

/// Type representing a Hocon table, payload of the `Config::Object` variant
pub type Object = BTreeMap<String, Config>;


fn merge_value(dest: &mut Object, key: &str, value: &Config) {
    let insert = match dest.get_mut(key) {
        Some(dest_value) => match dest_value {
            Config::Object(ref mut dest_obj) => match value {
                Config::Object(ref src_obj) => {
                    merge_objects(dest_obj, src_obj);
                    false
                }
                _ => true,
            },
            Config::Array(ref mut dest_array) => match value {
                Config::Array(ref src_array) => {
                    merge_arrays(dest_array, src_array);
                    false
                }
                _ => true,
            },
            Config::Value(_) => true,
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

pub(crate) fn put_value<'a>(dest: &mut Object, path: &[&'a str], value: &Config) {
    match path {
        [] => (),
        [k] => merge_value(dest, k, value),
        [k, rest.., last_k] => {
            let mut obj = Object::new();
            obj.insert(last_k.to_string(), value.clone());

            for key in rest.iter().rev() {
                let mut next_obj = Object::new();
                next_obj.insert(key.to_string(), Config::Object(obj));

                obj = next_obj;
            }

            if !dest.contains_key(&k.to_string()) {
                dest.insert(k.to_string(), Config::Object(obj));
            } else {
                merge_value(dest, k, value);
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_merge_value() {
        let mut dest = Object::new();
        merge_value(&mut dest, &"key", &Config::Value("a value".to_string()));
        assert_eq!(dest.get(&"key".to_string()), Some(&Config::Value("a value".to_string())));

        let mut dest = object!["key".to_string() => Config::Value("old value".to_string())];
        merge_value(&mut dest, &"key".to_string(), &Config::Value("new value".to_string()));
        assert_eq!(dest.get(&"key".to_string()), Some(&Config::Value("new value".to_string())));

        let mut dest = object!["key".to_string() => Config::Array(vec![Config::Value("old value".to_string())])];
        merge_value(&mut dest, &"key", &Config::Array(vec![Config::Value("new value".to_string())]));
        assert_eq!(
            dest.get(&"key".to_string()),
            Some(&Config::Array(vec![Config::Value("old value".to_string()), Config::Value("new value".to_string())]))
        );

        let mut dest = object![
            "key".to_string() => Config::Array(vec![Config::Value("old value".to_string())])
        ];
        merge_value(&mut dest, &"key", &Config::Value("new value".to_string()));
        assert_eq!(dest.get(&"key".to_string()), Some(&Config::Value("new value".to_string())));

        let mut dest = object![
            "key".to_string() => Config::Object(object![
                "key1".to_string() => Config::Value("old sub value".to_string()),
                "key3".to_string() => Config::Array(vec![Config::Value("array-value-1".to_string())]),
                "key4".to_string() => Config::Value("old value 4".to_string())
            ])
        ];

        merge_value(&mut dest, &"key".to_string(), &Config::Object(object![
            "key1".to_string() => Config::Value("new sub value".to_string()),
            "key2".to_string() => Config::Value("value2".to_string()),
            "key3".to_string() => Config::Array(vec![Config::Value("array-value-2".to_string())])
        ]));

        assert_eq!(
            dest,
            object![
                "key".to_string() => Config::Object(object![
                    "key1".to_string() => Config::Value("new sub value".to_string()),
                    "key2".to_string() => Config::Value("value2".to_string()),
                    "key3".to_string() => Config::Array(vec![
                        Config::Value("array-value-1".to_string()),
                        Config::Value("array-value-2".to_string())
                    ]),
                    "key4".to_string() => Config::Value("old value 4".to_string())
                ])
            ],
        );
    }

}
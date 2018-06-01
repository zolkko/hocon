use std::collections::{BTreeMap, HashMap};


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

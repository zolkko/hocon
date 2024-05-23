use crate::error::HoconError;
use crate::value::{Array, Object, Value};
use serde::de;
use serde::forward_to_deserialize_any;
use std::fmt;

/// Interpret a `hocon::Value` as an instance of type `T`.
///
/// This conversion can fail if the structure of the Value does not match the
/// structure expected by `T`, for example if `T` is a struct type but the Value
/// contains something other than a Hocon object. It can also fail if the structure
/// is correct but `T`'s implementation of `Deserialize` decides that something
/// is wrong with the data, for example required struct fields are missing from
/// the Hocon object or some number is too big to fit in the expected primitive
/// type.
pub fn from_value<T: de::DeserializeOwned>(value: Value) -> Result<T, HoconError> {
    de::Deserialize::deserialize(value)
}

struct ValueVisitor;

impl<'de> de::Visitor<'de> for ValueVisitor {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any HOCON value")
    }

    fn visit_bool<E: de::Error>(self, b: bool) -> Result<Value, E> {
        Ok(Value::Bool(b))
    }

    fn visit_i64<E: de::Error>(self, i: i64) -> Result<Value, E> {
        Ok(Value::Integer(i as isize))
    }

    fn visit_u64<E: de::Error>(self, u: u64) -> Result<Value, E> {
        Ok(Value::Integer(u as isize))
    }

    fn visit_f64<E: de::Error>(self, f: f64) -> Result<Value, E> {
        Ok(Value::Float(f))
    }

    fn visit_str<E: de::Error>(self, s: &str) -> Result<Value, E> {
        Ok(Value::String(s.to_owned()))
    }

    fn visit_string<E: de::Error>(self, s: String) -> Result<Value, E> {
        Ok(Value::String(s))
    }

    fn visit_none<E: de::Error>(self) -> Result<Value, E> {
        Ok(Value::Null)
    }

    fn visit_some<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<Value, D::Error> {
        de::Deserialize::deserialize(deserializer)
    }

    fn visit_unit<E: de::Error>(self) -> Result<Value, E> {
        Ok(Value::Null)
    }

    fn visit_seq<V: de::SeqAccess<'de>>(self, mut visitor: V) -> Result<Value, V::Error> {
        let mut vec = Array::default();

        while let Some(element) = visitor.next_element()? {
            vec.push(element);
        }

        Ok(Value::Array(vec))
    }

    fn visit_map<V: de::MapAccess<'de>>(self, mut visitor: V) -> Result<Value, V::Error> {
        let mut values = Object::default();

        while let Some((key, value)) = visitor.next_entry()? {
            values.insert(key, value);
        }

        Ok(Value::Object(values))
    }
}

impl<'de> de::Deserialize<'de> for Value {
    fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_any(ValueVisitor)
    }
}

impl<'de> de::Deserializer<'de> for Value {
    type Error = HoconError;

    fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Null => visitor.visit_unit(),
            Value::Bool(v) => visitor.visit_bool(v),
            Value::Integer(v) => {
                if v < std::u64::MIN as isize {
                    visitor.visit_i64(v as i64)
                } else {
                    visitor.visit_u64(v as u64)
                }
            }
            Value::Float(v) => visitor.visit_f64(v),
            Value::String(v) => visitor.visit_string(v),
            Value::Array(v) => visit_array(v, visitor),
            Value::Object(v) => visit_object(v, visitor),
        }
    }

    fn deserialize_bool<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Bool(v) => visitor.visit_bool(v),
            Value::Integer(v) => {
                if v == 0 {
                    visitor.visit_bool(false)
                } else {
                    visitor.visit_bool(true)
                }
            }
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_i8<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::i8::MAX as isize && v >= std::i8::MIN as isize => visitor.visit_i8(v as i8),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_i16<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::i16::MAX as isize && v >= std::i16::MIN as isize => visitor.visit_i16(v as i16),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_i32<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::i32::MAX as isize && v >= std::i32::MIN as isize => visitor.visit_i32(v as i32),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_i64<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) => visitor.visit_i64(v as i64),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_u8<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::u8::MAX as isize && v >= std::u8::MIN as isize => visitor.visit_u8(v as u8),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_u16<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::u16::MAX as isize && v >= std::u16::MIN as isize => visitor.visit_u16(v as u16),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_u32<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::u32::MAX as isize && v >= std::u32::MIN as isize => visitor.visit_u32(v as u32),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_u64<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v >= std::u64::MIN as isize => visitor.visit_u32(v as u32),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_f32<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Float(v) => visitor.visit_f32(v as f32),
            Value::Integer(v) => visitor.visit_f32(v as f32),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_f64<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Float(v) => visitor.visit_f64(v),
            Value::Integer(v) => visitor.visit_f64(v as f64),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_char<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_string(visitor)
    }

    fn deserialize_str<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_string(visitor)
    }

    fn deserialize_string<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::String(s) => visitor.visit_string(s),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_bytes<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_byte_buf(visitor)
    }

    fn deserialize_byte_buf<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::String(v) => visitor.visit_string(v),
            Value::Array(v) => visit_array(v, visitor),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_option<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Null => visitor.visit_unit(),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_unit_struct<V: de::Visitor<'de>>(self, _name: &'static str, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V: de::Visitor<'de>>(self, _name: &'static str, _visitor: V) -> Result<V::Value, Self::Error> {
        unimplemented!()
    }

    fn deserialize_seq<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Array(v) => visit_array(v, visitor),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_tuple<V: de::Visitor<'de>>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V: de::Visitor<'de>>(self, _name: &'static str, len: usize, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Object(o) => visit_object(o, visitor),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_struct<V: de::Visitor<'de>>(self, _name: &'static str, _fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Array(v) => visit_array(v, visitor),
            Value::Object(v) => visit_object(v, visitor),
            _ => Err(self.invalid_type(&visitor)),
        }
    }

    fn deserialize_enum<V: de::Visitor<'de>>(self, _name: &'static str, _variants: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> {
        visit_enum(self, visitor)
    }

    fn deserialize_identifier<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_string(visitor)
    }

    fn deserialize_ignored_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        drop(self);
        visitor.visit_unit()
    }
}

impl Value {
    #[cold]
    fn invalid_type<E: de::Error>(&self, exp: &dyn de::Expected) -> E {
        de::Error::invalid_type(self.unexpected(), exp)
    }

    #[cold]
    fn unexpected(&self) -> de::Unexpected {
        match *self {
            Value::Null => de::Unexpected::Unit,
            Value::Bool(b) => de::Unexpected::Bool(b),
            Value::Integer(n) => {
                if n < -1 {
                    de::Unexpected::Signed(n as i64)
                } else {
                    de::Unexpected::Unsigned(n as u64)
                }
            }
            Value::Float(n) => de::Unexpected::Float(n),
            Value::String(ref s) => de::Unexpected::Str(s),
            Value::Array(_) => de::Unexpected::Seq,
            Value::Object(_) => de::Unexpected::Map,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

fn visit_array<'de, V: de::Visitor<'de>>(array: Array, visitor: V) -> Result<V::Value, HoconError> {
    let mut deserializer = ArrayDeserializer::new(array);
    let seq = visitor.visit_seq(&mut deserializer)?;
    let remaining = deserializer.iter.len();
    if remaining == 0 {
        Ok(seq)
    } else {
        Err(HoconError::message("fewer elements in array"))
    }
}

struct ArrayDeserializer {
    iter: std::vec::IntoIter<Value>,
}

impl ArrayDeserializer {
    fn new(array: Array) -> Self {
        ArrayDeserializer { iter: array.into_iter() }
    }
}

impl<'de> de::Deserializer<'de> for ArrayDeserializer {
    type Error = HoconError;

    #[inline]
    fn deserialize_any<V: de::Visitor<'de>>(mut self, visitor: V) -> Result<V::Value, Self::Error> {
        let len = self.iter.len();
        if len == 0 {
            visitor.visit_unit()
        } else {
            let ret = visitor.visit_seq(&mut self)?;
            let remaining = self.iter.len();
            if remaining == 0 {
                Ok(ret)
            } else {
                Err(HoconError::message("fewer elements in array"))
            }
        }
    }

    fn deserialize_ignored_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        drop(self);
        visitor.visit_unit()
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes
        byte_buf option unit unit_struct newtype_struct seq tuple tuple_struct
        map struct enum identifier
    }
}

impl<'de> de::SeqAccess<'de> for ArrayDeserializer {
    type Error = HoconError;

    fn next_element_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error> {
        match self.iter.next() {
            Some(value) => seed.deserialize(value).map(Some),
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

fn visit_object<'de, V: de::Visitor<'de>>(object: Object, visitor: V) -> Result<V::Value, HoconError> {
    let mut deserializer = ObjectDeserializer::new(object);
    let map = visitor.visit_map(&mut deserializer)?;
    let remaining = deserializer.iter.len();
    if remaining == 0 {
        Ok(map)
    } else {
        Err(HoconError::message("fewer elements in map"))
    }
}

struct ObjectDeserializer {
    iter: <Object as IntoIterator>::IntoIter,
    value: Option<Value>,
}

impl ObjectDeserializer {
    fn new(object: Object) -> Self {
        ObjectDeserializer {
            iter: object.into_iter(),
            value: None,
        }
    }
}

impl<'de> de::MapAccess<'de> for ObjectDeserializer {
    type Error = HoconError;

    fn next_key_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error> {
        match self.iter.next() {
            Some((key, value)) => {
                self.value = Some(value);
                seed.deserialize(Value::String(key)).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<T::Value, Self::Error> {
        match self.value.take() {
            Some(value) => seed.deserialize(value),
            None => panic!("visit_value called before visit_key"),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

impl<'de> de::Deserializer<'de> for ObjectDeserializer {
    type Error = HoconError;

    #[inline]
    fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_map(self)
    }

    fn deserialize_ignored_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        drop(self);
        visitor.visit_unit()
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes
        byte_buf option unit unit_struct newtype_struct seq tuple tuple_struct
        map struct enum identifier
    }
}

////////////////////////////////////////////////////////////////////////////////

fn visit_enum<'de, V: de::Visitor<'de>>(v: Value, visitor: V) -> Result<V::Value, HoconError> {
    let (variant, value) = match v {
        Value::Object(value) => {
            let mut iter = value.into_iter();
            let (variant, value) = match iter.next() {
                Some(v) => v,
                None => {
                    return Err(HoconError::message("map with a single key"));
                }
            };

            if iter.next().is_some() {
                return Err(HoconError::message("map with a single key"));
            }

            (Value::String(variant), Some(value))
        }
        Value::String(variant) => (Value::String(variant), None),
        _other => {
            return Err(HoconError::message("string or map"));
        }
    };

    visitor.visit_enum(EnumDeserializer { variant, value })
}

struct EnumDeserializer {
    variant: Value,
    value: Option<Value>,
}

impl<'de> de::EnumAccess<'de> for EnumDeserializer {
    type Error = HoconError;

    type Variant = VariantDeserializer;

    fn variant_seed<V: de::DeserializeSeed<'de>>(self, seed: V) -> Result<(V::Value, VariantDeserializer), Self::Error> {
        let visitor = VariantDeserializer { value: self.value };
        seed.deserialize(self.variant).map(|v| (v, visitor))
    }
}

struct VariantDeserializer {
    value: Option<Value>,
}

impl<'de> de::VariantAccess<'de> for VariantDeserializer {
    type Error = HoconError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match self.value {
            Some(value) => de::Deserialize::deserialize(value),
            None => Ok(()),
        }
    }

    fn newtype_variant_seed<T: de::DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value, Self::Error> {
        match self.value {
            Some(value) => seed.deserialize(value),
            None => Err(HoconError::message("newtype variant")),
        }
    }

    fn tuple_variant<V: de::Visitor<'de>>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error> {
        match self.value {
            Some(Value::Array(v)) => de::Deserializer::deserialize_any(ArrayDeserializer::new(v), visitor),
            _ => Err(HoconError::message("tuple variant")),
        }
    }

    fn struct_variant<V: de::Visitor<'de>>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> {
        match self.value {
            Some(Value::Object(v)) => de::Deserializer::deserialize_any(ObjectDeserializer::new(v), visitor),
            _ => Err(HoconError::message("struct variant")),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

    use super::*;
    use std::collections::HashMap;

    #[test]
    fn value_bool() {
        let v: bool = from_value(Value::Bool(true)).expect("must deserialize hocon::Value::Bool into bool");
        assert_eq!(v, true);

        let v: bool = from_value(Value::Integer(1)).expect("must deserialize hocon::Value::Integer into bool");
        assert_eq!(v, true);

        let v: bool = from_value(Value::Integer(0)).expect("must deserialize hocon::Value::Integer into bool");
        assert_eq!(v, false);

        let v: Result<bool, _> = from_value(Value::String("test".to_owned()));
        assert!(v.is_err(), "must not convert string into bool");
    }

    #[test]
    fn value_string() {
        let s: String = from_value(Value::String("string value".to_owned())).expect("must deserialize hocon::Value::String into String");
        assert_eq!("string value", s);

        let v: Result<String, _> = from_value(Value::Integer(123));
        assert!(v.is_err(), "must not convert hocon::Value::Integer to string");
    }

    #[test]
    fn value_char() {
        let s: char = from_value(Value::String("S".to_owned())).expect("must deserialize hocon::Value::String into char");
        assert_eq!('S', s);

        let v: Result<char, _> = from_value(Value::Integer(123));
        assert!(v.is_err(), "must not convert hocon::Value::Integer to char");
    }

    #[test]
    fn value_i8() {
        let v: i8 = from_value(Value::Integer(12)).expect("must deserialize hocon::Value::Integer into i8");
        assert_eq!(v, 12);

        let v: Result<i8, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null to i8");

        let v: Result<i8, _> = from_value(Value::Integer(std::i8::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i8 if the value is too big");

        let v: Result<i8, _> = from_value(Value::Integer(std::i8::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i8 if the value is too small");
    }

    #[test]
    fn value_i16() {
        let v: i16 = from_value(Value::Integer(12)).expect("must deserialize hocon::Value::Integer into i16");
        assert_eq!(v, 12);

        let v: Result<i16, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null to i16");

        let v: Result<i16, _> = from_value(Value::Integer(std::i16::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i16 if the value is too big");

        let v: Result<i16, _> = from_value(Value::Integer(std::i16::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i16 if the value is too small");
    }

    #[test]
    fn value_i32() {
        let v: i32 = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into i32");
        assert_eq!(v, 123);

        let v: Result<i32, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Integer to i32");

        let v: Result<i32, _> = from_value(Value::Integer(std::i32::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i32 if the value is too big");

        let v: Result<i32, _> = from_value(Value::Integer(std::i32::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i32 if the value is too small");
    }

    #[test]
    fn value_i64() {
        let v: i64 = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into i64");
        assert_eq!(v, 123);

        let v: Result<i64, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null to i64");
    }

    #[test]
    fn value_u8() {
        let v: u8 = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into u8");
        assert_eq!(v, 123);

        let v: Result<u8, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null into u8");

        let v: Result<u8, _> = from_value(Value::Integer(std::u8::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into u8 if the value is too big");

        let v: Result<u8, _> = from_value(Value::Integer(std::u8::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into u8 if the value is too small");
    }

    #[test]
    fn value_u16() {
        let v: u16 = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into u16");
        assert_eq!(v, 123);

        let v: Result<u16, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null into u16");

        let v: Result<u16, _> = from_value(Value::Integer(std::u16::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into u16 if the value is too big");

        let v: Result<u16, _> = from_value(Value::Integer(std::u16::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into u16 if the value is too small");
    }

    #[test]
    fn value_u32() {
        let v: u32 = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into u32");
        assert_eq!(v, 123);

        let v: Result<u32, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null into u32");

        let v: Result<u32, _> = from_value(Value::Integer(std::u32::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into u32 if the value is too big");

        let v: Result<u32, _> = from_value(Value::Integer(std::u32::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into u32 if the value is too small");
    }

    #[test]
    fn value_u64() {
        let v: u64 = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into u64");
        assert_eq!(v, 123);

        let v: Result<u64, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null into u32");

        let v: Result<u64, _> = from_value(Value::Integer(std::u64::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into u64 if the value is too small");
    }

    #[test]
    fn value_f32() {
        let v: f32 = from_value(Value::Float(123.0)).expect("must deserialize hocon::Value::Float into f32");
        assert_eq!(v, 123.0);

        let v: f32 = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into f32");
        assert_eq!(v, 123.0);

        let v: Result<f32, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null into f32");
    }

    #[test]
    fn value_f64() {
        let v: f64 = from_value(Value::Float(123.0)).expect("must deserialize hocon::Value::Float into f64");
        assert_eq!(v, 123.0);

        let v: f64 = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into f64");
        assert_eq!(v, 123.0);

        let v: Result<f64, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert hocon::Value::Null into f64");
    }

    #[test]
    fn value_array() {
        let value = Value::Array(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
        let v: Vec<i32> = from_value(value).expect("must deserialize hocon::Value::Array into vector of i32");
        assert_eq!(&v, &[1, 2, 3]);

        let value = Value::Array(vec![Value::Integer(3), Value::Integer(2), Value::Integer(1)]);
        let v: Vec<u32> = from_value(value).expect("must deserialize hocon::Value::Array into vector of u32");
        assert_eq!(&v, &[3, 2, 1]);
    }

    #[test]
    fn value_object() {
        let value = Value::Object({
            let mut obj = Object::default();
            obj.insert("field1".to_owned(), Value::Integer(1));
            obj.insert("field2".to_owned(), Value::Integer(2));
            obj.insert("field3".to_owned(), Value::Integer(3));
            obj
        });
        let v: HashMap<String, i32> = from_value(value).expect("must deserialize hocon::Value::Object into map of i32");
        let expected = {
            let mut map = HashMap::default();
            map.insert("field1".to_owned(), 1);
            map.insert("field2".to_owned(), 2);
            map.insert("field3".to_owned(), 3);
            map
        };
        assert_eq!(v, expected);
    }

    #[test]
    fn value_optional() {
        let v: Option<i32> = from_value(Value::Integer(123)).expect("must deserialize hocon::Value::Integer into optional i32");
        assert_eq!(v, Some(123));

        let v: Option<i32> = from_value(Value::Null).expect("must deserialize hocon::Value::Integer into optional i32");
        assert_eq!(v, None);
    }

    #[test]
    fn value_unit() {
        let v: () = from_value(Value::Null).expect("must deserialize hocon::Value::Null into unit");
        assert_eq!(v, ());
    }

    #[test]
    fn value_tuple() {
        let v: (i32, i32) = from_value(Value::Array(vec![Value::Integer(1), Value::Integer(2)])).expect("must deserialize hocon::Value::Array into tuple of i32");
        assert_eq!(v, (1, 2));

        let v: Result<(i32, i32), _> = from_value(Value::Array(vec![Value::Integer(1)]));
        assert!(v.is_err(), "expected a tuple of size 2");

        let v: Result<(i32, i32), _> = from_value(Value::Array(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]));
        assert!(v.is_err(), "expected a tuple of size 2");

        let v: Result<(i32, i32), _> = from_value(Value::Null);
        assert!(v.is_err(), "expected an array");
    }

    #[test]
    fn value_tuple_struct() {
        use serde_derive::Deserialize;

        #[derive(Deserialize, Debug, PartialEq)]
        pub struct TupleStruct(pub i32, pub i32);

        let v: TupleStruct = from_value(Value::Array(vec![Value::Integer(1), Value::Integer(2)])).expect("must deserialize hocon::Value::Array into tuple of i32");
        assert_eq!(v, TupleStruct(1, 2));
    }

    #[test]
    fn value_unit_struct() {
        use serde_derive::Deserialize;

        #[derive(Debug, PartialEq, Deserialize)]
        pub struct UnitStruct;

        let v: UnitStruct = from_value(Value::Null).expect("must deserialize hocon::Value::Null into unit struct");
        assert_eq!(v, UnitStruct);
    }

    #[test]
    fn value_struct() {
        use serde_derive::Deserialize;

        #[derive(Deserialize, Debug, PartialEq)]
        pub struct TestStruct {
            pub field1: i32,
            pub field2: String,
        }

        let v: TestStruct = from_value(Value::Object({
            let mut obj = Object::default();
            obj.insert("field1".to_owned(), Value::Integer(1));
            obj.insert("field2".to_owned(), Value::String("2".to_owned()));
            obj
        }))
        .expect("must deserialize hocon::Value::Object into TestStruct");
        assert_eq!(
            v,
            TestStruct {
                field1: 1,
                field2: "2".to_owned(),
            }
        );

        let v: TestStruct = from_value(Value::Array(vec![Value::Integer(1), Value::String("2".to_owned())])).expect("must deserialize hocon::Value::Object into TestStruct");
        assert_eq!(
            v,
            TestStruct {
                field1: 1,
                field2: "2".to_owned(),
            }
        );

        let v: Result<TestStruct, _> = from_value(Value::Object({
            let mut obj = Object::default();
            obj.insert("field2".to_owned(), Value::String("2".to_owned()));
            obj
        }));
        assert!(v.is_err(), "missing field field1");

        let v: Result<TestStruct, _> = from_value(Value::Array(vec![Value::Integer(1)]));
        assert!(v.is_err(), "not enough elements in the array");
    }

    #[test]
    fn value_enum() {
        use serde_derive::Deserialize;

        #[derive(Deserialize, Debug, PartialEq)]
        pub enum TestUnit {
            One,
            Two,
        }

        let v: TestUnit = from_value(Value::String("One".to_string())).expect("must deserialize hocon::Value::String into enum");
        assert_eq!(v, TestUnit::One);

        let v: TestUnit = from_value(Value::String("Two".to_string())).expect("must deserialize hocon::Value::String into enum");
        assert_eq!(v, TestUnit::Two);

        #[derive(Deserialize, Debug, PartialEq)]
        pub enum TestStruct {
            One { field1: i32, field2: String },
            Two { field1: String, field2: i32 },
        }

        let v: TestStruct = from_value(Value::Object({
            let mut fields = Object::default();
            fields.insert("field1".to_owned(), Value::Integer(1));
            fields.insert("field2".to_owned(), Value::String("2".to_owned()));

            let mut obj = Object::default();
            obj.insert("One".to_owned(), Value::Object(fields));
            obj
        }))
        .expect("must deserialize hocon::Value::String into enum");
        assert_eq!(
            v,
            TestStruct::One {
                field1: 1,
                field2: "2".to_owned(),
            }
        );

        let v: TestStruct = from_value(Value::Object({
            let mut fields = Object::default();
            fields.insert("field1".to_owned(), Value::String("1".to_owned()));
            fields.insert("field2".to_owned(), Value::Integer(2));

            let mut obj = Object::default();
            obj.insert("Two".to_owned(), Value::Object(fields));
            obj
        }))
        .expect("must deserialize hocon::Value::String into enum");
        assert_eq!(
            v,
            TestStruct::Two {
                field1: "1".to_owned(),
                field2: 2,
            }
        );
    }
}

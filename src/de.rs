use std::fmt;

use serde::de;

use super::*;


/*
/// ```rust
/// # use hocon::Value;
/// let val = Value::String("foo".to_owned());
/// let s: String = hocon::from_value(val).unwrap();
/// assert_eq!("foo", s);
/// ```
*/


/// Interpret a `hocon::Value` as an instance of type `T`.
///
/// This conversion can fail if the structure of the Value does not match the
/// structure expected by `T`, for example if `T` is a struct type but the Value
/// contains something other than a Hocon object. It can also fail if the structure
/// is correct but `T`'s implementation of `Deserialize` decides that something
/// is wrong with the data, for example required struct fields are missing from
/// the Hocon object or some number is too big to fit in the expected primitive
/// type.
pub fn from_value<T: de::DeserializeOwned>(value: Value) -> Result<T, HoconError<Rule>> {
    de::Deserialize::deserialize(value)
}

////////////////////////////////////////////////////////////////////////////////

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
        Ok(Value::Float(f.into()))
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

    type Error = HoconError<Rule>;

    fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Null => visitor.visit_unit(),
            Value::Bool(v) => visitor.visit_bool(v),
            Value::Integer(v) => {
                if v < 0 {
                    visitor.visit_i64(v as i64)
                } else {
                    visitor.visit_u64(v as u64)
                }
            },
            Value::Float(v) => visitor.visit_f64(v),
            Value::String(v) => visitor.visit_string(v),
            Value::Array(v) => unimplemented!(),
            Value::Object(v) => unimplemented!(),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_i8<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::i8::MAX as isize && v >= std::i8::MIN as isize => visitor.visit_i8(v as i8),
            _ => Err(self.invalid_type(&visitor))
        }
    }

    fn deserialize_i16<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::i16::MAX as isize && v >= std::i16::MIN as isize => visitor.visit_i16(v as i16),
            _ => Err(self.invalid_type(&visitor))
        }
    }

    fn deserialize_i32<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) if v <= std::i32::MAX as isize && v >= std::i32::MIN as isize => visitor.visit_i32(v as i32),
            _ => Err(self.invalid_type(&visitor))
        }
    }

    fn deserialize_i64<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        match self {
            Value::Integer(v) => visitor.visit_i64(v as i64),
            _ => Err(self.invalid_type(&visitor))
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de> {
        unimplemented!()
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de> {
        unimplemented!()
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de> {
        unimplemented!()
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de> {
        unimplemented!()
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
            _ => Err(self.invalid_type(&visitor))
        }
    }




    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_newtype_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_tuple_struct<V>(self, name: &'static str, len: usize, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_struct<V>(self, name: &'static str, fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_enum<V>(self, name: &'static str, variants: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor<'de>
    {
        unimplemented!()
    }
}

impl Value {
    #[cold]
    fn invalid_type<E: de::Error>(&self, exp: &de::Expected) -> E {
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
            },
            Value::Float(n) => de::Unexpected::Float(n),
            Value::String(ref s) => de::Unexpected::Str(s),
            Value::Array(_) => de::Unexpected::Seq,
            Value::Object(_) => de::Unexpected::Map,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;


    #[test]
    fn value_string() {
        let s: String = from_value(Value::String("string value".to_owned()))
            .expect("must deserialize hocon::Value::String into String");
        assert_eq!("string value", s);
    }

    #[test]
    fn value_string_failure() {
        let v: Result<String, _> = from_value(Value::Integer(123));
        assert!(v.is_err(), "must not convert isize to string");
    }

    #[test]
    fn value_char() {
        let s: char = from_value(Value::String("S".to_owned()))
            .expect("must deserialize hocon::Value::String into char");
        assert_eq!('S', s);
    }

    #[test]
    fn value_char_failure() {
        let v: Result<char, _> = from_value(Value::Integer(123));
        assert!(v.is_err(), "must not convert isize to char");
    }

    #[test]
    fn value_isize() {
        let v: isize = from_value(Value::Integer(123))
            .expect("must deserialize hocon::Value::Integer into isize");
        assert_eq!(v, 123);
    }

    #[test]
    fn value_isize_failure() {
        let v: Result<isize, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert null to isize");
    }

    #[test]
    fn value_i64() {
        let v: i64 = from_value(Value::Integer(123))
            .expect("must deserialize hocon::Value::Integer into i64");
        assert_eq!(v, 123);
    }

    #[test]
    fn value_i64_failure() {
        let v: Result<i64, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert null to i64");
    }

    #[test]
    fn value_i32() {
        let v: i32 = from_value(Value::Integer(123))
            .expect("must deserialize hocon::Value::Integer into i32");
        assert_eq!(v, 123);
    }

    #[test]
    fn value_i32_failure() {
        let v: Result<i32, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert null to i32");

        let v: Result<i32, _> = from_value(Value::Integer(std::i32::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i32 if it is too big");

        let v: Result<i32, _> = from_value(Value::Integer(std::i32::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i32 if it is too small");
    }

    #[test]
    fn value_i16() {
        let v: i16 = from_value(Value::Integer(12))
            .expect("must deserialize hocon::Value::Integer into i16");
        assert_eq!(v, 12);
    }

    #[test]
    fn value_i16_failure() {
        let v: Result<i16, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert null to i16");

        let v: Result<i16, _> = from_value(Value::Integer(std::i16::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i16 if it is too big");

        let v: Result<i16, _> = from_value(Value::Integer(std::i16::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i16 if it is too small");
    }

    #[test]
    fn value_i8() {
        let v: i8 = from_value(Value::Integer(12))
            .expect("must deserialize hocon::Value::Integer into i16");
        assert_eq!(v, 12);
    }

    #[test]
    fn value_i8_failure() {
        let v: Result<i8, _> = from_value(Value::Null);
        assert!(v.is_err(), "must not convert null to i8");

        let v: Result<i8, _> = from_value(Value::Integer(std::i8::MAX as isize + 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i8 if it is too big");

        let v: Result<i8, _> = from_value(Value::Integer(std::i8::MIN as isize - 1));
        assert!(v.is_err(), "must not convert hocon::Value::Integer into i8 if it is too small");
    }
}

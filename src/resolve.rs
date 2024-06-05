use crate::ast;
use crate::ast::{FieldOp, FieldOrInclude, PathRef, Span};
use crate::error::Error;
use crate::value::{Array, Object, Position, Value, ValueKind};

pub(crate) fn resolve(input: ast::Value) -> Result<Value, Error> {
    let span = input.span;
    let column = span.get_column();
    let line = span.location_line() as usize;
    match input.kind {
        ast::ValueKind::Object(object) => {
            let root_object = convert_object(object)?;
            let position = Position::new(line, column);
            Ok(Value::new(ValueKind::Object(root_object), position))
        }
        _ => Err(Error::string(format!("top level entity defined at line {line}, column {column} must be an object"))),
    }
}

fn convert_object(from: ast::Object<ast::Span>) -> Result<Object, Error> {
    let mut object = Object::new();

    for item in from.fields {
        match item {
            FieldOrInclude::Field(field) => match field.op {
                FieldOp::Assign(values) => {
                    let value = fold_values(values)?;
                    object_assign(&mut object, &field.path, value)?;
                }
                FieldOp::Append(values) => {
                    let value = fold_values(values)?;
                    object_append(&mut object, &field.path, value)?;
                }
                FieldOp::Object(o) => {
                    let span = Span::new("FIXME");
                    let value = convert_value(ast::Value::new(ast::ValueKind::Object(o), span))?;
                    object_assign(&mut object, &field.path, value)?;
                }
            },
            FieldOrInclude::Include(_) => {
                // unimplemented!()
            }
        }
    }

    Ok(object)
}

fn object_append(out: &mut Object, path: PathRef, value: Value) -> Result<(), Error> {
    match path {
        [] => Ok(()),
        [name] => {
            match out.get_mut(*name) {
                Some(Value(ValueKind::Array(ref mut array), _)) => {
                    array.push(value);
                    Ok(())
                }
                None => {
                    let pos = value.1.clone();
                    out.insert(name.to_string(), Value(ValueKind::Array(vec![value]), pos));
                    Ok(())
                }
                _ => {
                    // Shall we allow appending to objects, string and numbers?
                    Err(Error::string(format!("cannot append value to the [{name}] key")))
                }
            }
        }
        [name, tail @ ..] => {
            if !out.contains_key(*name) {
                // Maybe it is better to inset parent's position?
                out.insert(name.to_string(), Value::new(ValueKind::Object(Object::default()), value.1.clone()));
            }
            let maybe_child_object = out.get_mut(*name);
            if let Some(Value(ValueKind::Object(ref mut child_object), _)) = maybe_child_object {
                object_append(child_object, tail, value)
            } else {
                // Shall I allow addressing array elements?
                Err(Error::message("cannot address non objects"))
            }
        }
    }
}

fn object_assign(out: &mut Object, path: PathRef, value: Value) -> Result<(), Error> {
    match path {
        [] => Ok(()),
        [name] => {
            out.insert(name.to_string(), value);
            Ok(())
        }
        [name, tail @ ..] => {
            if !out.contains_key(*name) {
                out.insert(name.to_string(), Value::new(ValueKind::Object(Object::default()), value.1.clone()));
            }
            let maybe_child_object = out.get_mut(*name);
            if let Some(Value(ValueKind::Object(ref mut child_object), _)) = maybe_child_object {
                object_assign(child_object, tail, value)
            } else {
                // Shall I allow addressing array elements?
                Err(Error::message("cannot address non objects"))
            }
        }
    }
}

fn fold_values(mut values: Vec<ast::Value>) -> Result<Value, Error> {
    if values.len() > 1 {
        let values = convert_values(values)?;
        let position = values.first().map(|Value(_, p)| p.clone()).unwrap_or(Position::new(0, 0));
        if all_arrays(&values) {
            let array_values = cast_values_to_arrays(values);
            let merged_array = merge_arrays(array_values);
            Ok(Value::new(ValueKind::Array(merged_array), position))
        } else if all_objects(&values) {
            let object_values = cast_values_to_objects(values);
            let merged_object = merge_objects(object_values);
            Ok(Value::new(ValueKind::Object(merged_object), position))
        } else {
            let merged_string = values.into_iter().map(value_to_string).collect::<Result<String, _>>()?;
            Ok(Value::new(ValueKind::String(merged_string), position))
        }
    } else if let Some(value) = values.pop() {
        convert_value(value)
    } else {
        // TODO: pass default position from the previous token.
        Ok(Value::new(ValueKind::Null, Position::new(0, 0)))
    }
}

fn convert_values(values: Vec<ast::Value>) -> Result<Vec<Value>, Error> {
    values.into_iter().map(convert_value).collect()
}

fn convert_value(value: ast::Value) -> Result<Value, Error> {
    let span = value.span;
    let line = span.location_line();
    let column = span.get_column();
    let position = Position::new(line as usize, column);

    let kind = match value.kind {
        ast::ValueKind::Null => ValueKind::Null,
        ast::ValueKind::Boolean(val) => ValueKind::Boolean(val),
        ast::ValueKind::Integer(val) => ValueKind::Integer(val),
        ast::ValueKind::Real(val) => ValueKind::Real(val),
        ast::ValueKind::String(val) => ValueKind::String(val.to_string()),
        ast::ValueKind::Array(val) => {
            let array = val.items.into_iter().map(fold_values).collect::<Result<Array, Error>>()?;
            ValueKind::Array(array)
        }
        ast::ValueKind::Object(val) => {
            let obj = convert_object(val)?;
            ValueKind::Object(obj)
        }
        ast::ValueKind::Substitution(_) => {
            // TODO(zolkko): substitutions are not yet implemented
            // unimplemented!()
            ValueKind::Null
        }
    };

    Ok(Value::new(kind, position))
}

fn value_to_string(Value(value, _): Value) -> Result<String, Error> {
    // Use a function instead of ToString/Display trait
    // because it may require context to render Substitution.

    Ok(match value {
        ValueKind::Null => "".to_string(),
        ValueKind::Boolean(val) => val.to_string(),
        ValueKind::Integer(val) => val.to_string(),
        ValueKind::Real(val) => val.to_string(),
        ValueKind::String(val) => val,
        ValueKind::Array(array) => array_to_string(array)?,
        ValueKind::Object(object) => object_to_string(object)?,
        ValueKind::BadValue(err) => err.to_string(),
    })
}

fn object_to_string(obj: Object) -> Result<String, Error> {
    let mut res: String = "{".to_string();
    let mut obj_iter = obj.into_iter();
    if let Some((k, v)) = obj_iter.next() {
        res += &k;
        res += ":";
        res += &value_to_string(v)?;

        for (k, v) in obj_iter {
            res += ",";
            res += &k;
            res += ":";
            res += &value_to_string(v)?;
        }
    }
    res += "}";
    Ok(res)
}

fn array_to_string(vals: Array) -> Result<String, Error> {
    let mut res: String = "[".to_string();
    let mut vals_iter = vals.into_iter();
    if let Some(x) = vals_iter.next() {
        res += &value_to_string(x)?;
        for val in vals_iter {
            res += ",";
            res += &value_to_string(val)?;
        }
    }
    res += "]";
    Ok(res)
}

fn cast_values_to_objects(values: Vec<Value>) -> impl IntoIterator<Item = Object> {
    values
        .into_iter()
        .filter_map(|x| if let Value(ValueKind::Object(object), _) = x { Some(object) } else { None })
}

fn merge_objects(objects: impl IntoIterator<Item = Object>) -> Object {
    objects.into_iter().flat_map(|x| x.into_iter()).collect()
}

fn cast_values_to_arrays(values: Vec<Value>) -> impl IntoIterator<Item = Array> {
    values
        .into_iter()
        .filter_map(|x| if let Value(ValueKind::Array(array), _) = x { Some(array) } else { None })
}

fn merge_arrays(arrays: impl IntoIterator<Item = Array>) -> Array {
    arrays.into_iter().flatten().collect()
}

fn all_objects(values: &[Value]) -> bool {
    values.iter().all(|x| matches!(x, Value(ValueKind::Object(_), _)))
}

fn all_arrays(values: &[Value]) -> bool {
    values.iter().all(|x| matches!(x, Value(ValueKind::Array(_), _)))
}

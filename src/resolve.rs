use crate::ast;
use crate::ast::{FieldOp, FieldOrInclude, PathRef, Value as AstValue};
use crate::error::Error;
use crate::value::{Array, Object, Value};

pub(crate) fn resolve(input: AstValue) -> Result<Value, Error> {
    todo!()
    /*
    match input {
        AstValue::Object(object) => {
            let root_object = convert_object(object)?;
            Ok(Value::Object(root_object))
        }
        _ => Err(Error::message("top level entity must be an object")),
    }*/
}
/*
fn convert_object(from: ast::Object) -> Result<Object, Error> {
    let mut object = Object::default();

    for item in from {
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
                    let value = convert_value(ast::Value::Object(o))?;
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
                Some(Value::Array(ref mut array)) => {
                    array.push(value);
                    Ok(())
                }
                None => {
                    out.insert(name.to_string(), Value::Array(vec![value]));
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
                out.insert(name.to_string(), Value::Object(Object::default()));
            }
            let maybe_child_object = out.get_mut(*name);
            if let Some(Value::Object(ref mut child_object)) = maybe_child_object {
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
                out.insert(name.to_string(), Value::Object(Object::default()));
            }
            let maybe_child_object = out.get_mut(*name);
            if let Some(Value::Object(ref mut child_object)) = maybe_child_object {
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
        if all_arrays(&values) {
            let array_values = cast_values_to_arrays(values);
            let merged_array = merge_arrays(array_values);
            Ok(Value::Array(merged_array))
        } else if all_objects(&values) {
            let object_values = cast_values_to_objects(values);
            let merged_object = merge_objects(object_values);
            Ok(Value::Object(merged_object))
        } else {
            let merged_string = values.into_iter().map(value_to_string).collect::<Result<String, _>>()?;
            Ok(Value::String(merged_string))
        }
    } else if let Some(value) = values.pop() {
        convert_value(value)
    } else {
        Ok(Value::Null)
    }
}

fn convert_values(values: Vec<ast::Value>) -> Result<Vec<Value>, Error> {
    values.into_iter().map(convert_value).collect()
}

fn convert_value(value: ast::Value) -> Result<Value, Error> {
    let res = match value {
        ast::Value::Null => Value::Null,
        ast::Value::Boolean(val) => Value::Boolean(val),
        ast::Value::Integer(val) => Value::Integer(val),
        ast::Value::Real(val) => Value::Real(val),
        ast::Value::String(val) => Value::String(val.to_string()),
        ast::Value::Array(val) => {
            let array = val.into_iter().map(fold_values).collect::<Result<Array, Error>>()?;
            Value::Array(array)
        }
        ast::Value::Object(val) => {
            let obj = convert_object(val)?;
            Value::Object(obj)
        }
        ast::Value::Substitution(_) => {
            // TODO(zolkko): substitutions are not yet implemented
            // unimplemented!()
            Value::Null
        }
    };

    Ok(res)
}

fn value_to_string(value: Value) -> Result<String, Error> {
    // Use a function instead of ToString/Display trait
    // because it may require context to render Substitution.

    Ok(match value {
        Value::Null => "".to_string(),
        Value::Boolean(val) => val.to_string(),
        Value::Integer(val) => val.to_string(),
        Value::Real(val) => val.to_string(),
        Value::String(val) => val,
        Value::Array(array) => array_to_string(array)?,
        Value::Object(object) => object_to_string(object)?,
        Value::BadValue(err) => err.to_string(),
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
    values.into_iter().filter_map(|x| if let Value::Object(object) = x { Some(object) } else { None })
}

fn merge_objects(objects: impl IntoIterator<Item = Object>) -> Object {
    objects.into_iter().flat_map(|x| x.into_iter()).collect()
}

fn cast_values_to_arrays(values: Vec<Value>) -> impl IntoIterator<Item = Array> {
    values.into_iter().filter_map(|x| if let Value::Array(array) = x { Some(array) } else { None })
}

fn merge_arrays(arrays: impl IntoIterator<Item = Array>) -> Array {
    arrays.into_iter().flatten().collect()
}

fn all_objects(values: &[Value]) -> bool {
    values.iter().all(|x| matches!(x, Value::Object(_)))
}

fn all_arrays(values: &[Value]) -> bool {
    values.iter().all(|x| matches!(x, Value::Array(_)))
}
*/
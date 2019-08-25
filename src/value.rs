#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Integer(i64),
    Double(f64),
    Boolean(bool),
    String(&'a str),
    Array(&'a [Value<'a>]),
}

impl<'a> Value<'a> {
    pub fn type_of(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Array(_) => "array",
        }
    }
}

impl<'a> From<i64> for Value<'a> {
    fn from(int: i64) -> Value<'a> {
        Value::Integer(int)
    }
}

impl<'a> From<f64> for Value<'a> {
    fn from(num: f64) -> Value<'a> {
        Value::Double(num)
    }
}

impl<'a> From<bool> for Value<'a> {
    fn from(b: bool) -> Value<'a> {
        Value::Boolean(b)
    }
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(s: &'a str) -> Value<'a> {
        Value::String(s)
    }
}

impl<'a> From<&'a Vec<Value<'a>>> for Value<'a> {
    fn from(vs: &'a Vec<Value<'a>>) -> Value<'a> {
        Value::Array(vs.as_slice())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_int() {
        assert_eq!(Value::from(123), Value::Integer(123));
    }

    #[test]
    fn test_from_float() {
        assert_eq!(Value::from(1.23), Value::Double(1.23));
    }

    #[test]
    fn test_from_bool() {
        assert_eq!(Value::from(true), Value::Boolean(true));
    }

    #[test]
    fn test_from_str() {
        let s = "hello world";
        assert_eq!(Value::from(s), Value::String(s));
    }

    #[test]
    fn test_from_vec() {
        let vs = vec![
            Value::from(123),
            Value::from(true),
            Value::from(3.21),
            Value::from("hwhwhwh"),
        ];

        let v1 = Value::from(&vs);
        let v2 = Value::Array(vs.as_slice());

        assert_eq!(v1, v2);
    }
}

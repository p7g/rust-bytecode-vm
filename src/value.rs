use crate::interpreter::Interpreter;

type BuiltinFunction<'a> = fn(&mut Interpreter, &'a [Value<'a>]) -> Value<'a>;

#[derive(Debug, PartialEq)]
pub enum Upvalue<'a> {
    Open(&'a Value<'a>),
    Closed(Value<'a>),
}

pub enum FunctionValue<'a> {
    Builtin {
        name: Option<usize>,
        arity: usize,
        function: BuiltinFunction<'a>,
    },
    User {
        name: Option<usize>,
        arity: usize,
        address: usize,
        upvalues: &'a [Upvalue<'a>],
    },
}

impl<'a> std::fmt::Debug for FunctionValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionValue::Builtin {
                name,
                arity,
                function,
            } => write!(
                f,
                "FunctionValue::Builtin(name: {:?}, arity: {:?}, function: {:p})",
                name, arity, function,
            ),
            FunctionValue::User {
                name,
                arity,
                address,
                ..
            } => write!(
                f,
                "FunctionValue::User(name: {:?}, arity: {:?}, address: {:?})",
                name, arity, address,
            ),
        }
    }
}

impl<'a> PartialEq for FunctionValue<'a> {
    fn eq(&self, other: &FunctionValue<'a>) -> bool {
        match self {
            FunctionValue::Builtin {
                name,
                arity,
                function,
            } => {
                if let FunctionValue::Builtin {
                    name: other_name,
                    arity: other_arity,
                    function: other_function,
                } = other
                {
                    name == other_name
                        && arity == other_arity
                        && function as *const BuiltinFunction
                            == other_function as *const BuiltinFunction
                } else {
                    false
                }
            }
            FunctionValue::User {
                name,
                arity,
                address,
                upvalues,
            } => {
                if let FunctionValue::User {
                    name: other_name,
                    arity: other_arity,
                    address: other_address,
                    upvalues: other_upvalues,
                } = other
                {
                    name == other_name
                        && arity == other_arity
                        && address == other_address
                        && upvalues == other_upvalues
                } else {
                    false
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Integer(i64),
    Double(f64),
    Boolean(bool),
    Null,
    String(&'a str),
    Array(Box<[Value<'a>]>),
    Function(&'a FunctionValue<'a>),
}

impl<'a> Value<'a> {
    pub fn type_of(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::Boolean(_) => "boolean",
            Value::Null => "null",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Function(_) => "function",
        }
    }
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Value<'a>) -> bool {
        match self {
            Value::Integer(a) => {
                if let Value::Integer(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Value::Double(a) => {
                if let Value::Double(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Value::Boolean(a) => {
                if let Value::Boolean(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Value::Null => {
                if let Value::Null = other {
                    true
                } else {
                    false
                }
            }
            Value::String(a) => {
                if let Value::String(b) = other {
                    if a.len() != b.len() {
                        false
                    } else {
                        a.chars().zip(b.chars()).all(|(a, b)| a == b)
                    }
                } else {
                    false
                }
            }
            Value::Array(a) => {
                if let Value::Array(b) = other {
                    if a.len() != b.len() {
                        false
                    } else {
                        a.iter().zip(b.iter()).all(|(a, b)| a == b)
                    }
                } else {
                    false
                }
            }
            Value::Function(a) => {
                if let Value::Function(b) = other {
                    a == b // see trait PartialEq forFunctionValue
                } else {
                    false
                }
            }
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

impl<'a> From<Vec<Value<'a>>> for Value<'a> {
    fn from(vs: Vec<Value<'a>>) -> Value<'a> {
        Value::Array(vs.into_boxed_slice())
    }
}

impl<'a> From<&'a FunctionValue<'a>> for Value<'a> {
    fn from(f: &'a FunctionValue<'a>) -> Value<'a> {
        Value::Function(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

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

        let v1 = Value::from(vs.clone());
        let v2 = Value::Array(vs.into_boxed_slice());

        assert_eq!(v1, v2);
    }

    fn builtin_function<'a>(_: &mut Interpreter, _: &'a [Value<'a>]) -> Value<'a> {
        Value::Null
    }

    #[test]
    fn test_functionvalue_equality() {
        let a = FunctionValue::Builtin {
            name: Some(123),
            arity: 1,
            function: builtin_function,
        };
        let b = FunctionValue::User {
            name: Some(123),
            arity: 1,
            address: 123,
            upvalues: &[],
        };
        let d = FunctionValue::User {
            name: Some(123),
            arity: 1,
            address: 123,
            upvalues: &[],
        };

        let av = Value::from(&a);
        let bv = Value::from(&b);
        let dv = Value::from(&d);

        assert_ne!(a, b);
        // might not be equal, function pointers can be different even when they point
        // to the same function
        // assert_eq!(a, c);
        assert_eq!(b, d);

        assert_ne!(av, bv);
        assert_eq!(bv, dv);
    }

    #[test]
    fn test_int_equality() {
        let a = Value::from(123123);
        let b = Value::from(123123);
        let c = Value::from(432432);

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_double_equality() {
        let a = Value::from(1.23);
        let b = Value::from(1.23);
        let c = Value::from(2.34);

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_boolean_equality() {
        let a = Value::from(true);
        let b = Value::from(true);
        let c = Value::from(false);

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_null_equality() {
        let a = Value::Null;
        let b = Value::Null;
        let c = Value::Boolean(false);

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_string_equality() {
        let a = Value::String("hello");
        let b = Value::String("hello");
        let c = Value::String("world");

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_array_equality() {
        let a = Value::from(vec![Value::from(true), Value::Null, Value::String("abc")]);
        let b = Value::from(vec![Value::from(true), Value::Null, Value::String("abc")]);
        let c = Value::from(vec![Value::from(123)]);

        assert_eq!(a, b);
        assert_ne!(a, c);
    }
}

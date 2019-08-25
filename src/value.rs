use crate::code_object::CodeObject;
use crate::interpreter::Interpreter;

type BuiltinFunction<'a> = fn(&mut Interpreter, &'a [Value<'a>]) -> Value<'a>;

pub enum FunctionValue<'a> {
    Builtin {
        name: Option<usize>,
        arity: usize,
        function: BuiltinFunction<'a>,
    },
    User {
        name: Option<usize>,
        arity: usize,
        code_object: CodeObject,
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
                code_object,
            } => write!(
                f,
                "FunctionValue::User(name: {:?}, arity: {:?}, code_object: {:?})",
                name, arity, code_object,
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
                code_object,
            } => {
                if let FunctionValue::User {
                    name: other_name,
                    arity: other_arity,
                    code_object: other_code_object,
                } = other {
                    name == other_name &&
                        arity == other_arity &&
                        code_object == other_code_object
                } else {
                    false
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Integer(i64),
    Double(f64),
    Boolean(bool),
    Null,
    String(&'a str),
    Array(&'a [Value<'a>]),
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

impl<'a> From<&'a FunctionValue<'a>> for Value<'a> {
    fn from(f: &'a FunctionValue<'a>) -> Value<'a> {
        Value::Function(f)
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
            code_object: CodeObject::new(Vec::new()),
        };
        let c = FunctionValue::Builtin {
            name: Some(123),
            arity: 1,
            function: builtin_function,
        };
        let d = FunctionValue::User {
            name: Some(123),
            arity: 1,
            code_object: CodeObject::new(Vec::new()),
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
}

use crate::interpreter::Interpreter;
use std::cell::RefCell;
use std::rc::Rc;

type BuiltinFunction = fn(&mut Interpreter, Vec<Value>) -> Value;

#[derive(Debug, PartialEq)]
enum UpvalueValue {
    Open(usize), // index of stack
    Closed(Value),
}

#[derive(Debug, PartialEq)]
pub struct Upvalue {
    value: UpvalueValue,
}

impl Upvalue {
    pub fn new(value: usize) -> Upvalue {
        Upvalue {
            value: UpvalueValue::Open(value),
        }
    }

    pub fn is_open(&self) -> bool {
        if let UpvalueValue::Open(_) = self.value {
            true
        } else {
            false
        }
    }

    pub fn close(&mut self, value: Value) {
        if let UpvalueValue::Open(_) = &self.value {
            self.value = UpvalueValue::Closed(value);
        } else {
            panic!("Closing closed upvalue");
        }
    }

    pub fn get_value(&self) -> Value {
        if let UpvalueValue::Closed(value) = &self.value {
            value.clone()
        } else {
            panic!("Getting value of open upvalue");
        }
    }

    pub fn set_value(&mut self, value: Value) {
        if let UpvalueValue::Closed(_) = &self.value {
            self.value = UpvalueValue::Closed(value); // FIXME: make sure this doesn't leak
        } else {
            panic!("Setting value of open upvalue");
        }
    }

    pub fn stack_index(&self) -> usize {
        if let UpvalueValue::Open(index) = self.value {
            index
        } else {
            panic!("Getting index of closed upvalue");
        }
    }
}

#[derive(Clone)]
pub enum FunctionValue {
    Builtin {
        name: Option<usize>,
        arity: usize,
        function: BuiltinFunction,
    },
    User {
        name: Option<usize>,
        arity: usize,
        address: usize,
        upvalues: Vec<Rc<RefCell<Upvalue>>>,
    },
}

impl std::fmt::Debug for FunctionValue {
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

impl PartialEq for FunctionValue {
    fn eq(&self, other: &FunctionValue) -> bool {
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
pub enum Value {
    Integer(i64),
    Double(f64),
    Boolean(bool),
    Null,
    String(String),
    Array(Rc<RefCell<Box<[Value]>>>),
    Function(FunctionValue),
}

impl Value {
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

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(n) => *n != 0,
            Value::Double(n) => *n != 0f64,
            Value::Boolean(b) => *b,
            Value::String(s) => !s.is_empty(),
            Value::Array(vs) => !vs.borrow().is_empty(),
            Value::Function(_) => true,
            Value::Null => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Integer(n) => write!(f, "{}", n),
            Value::Double(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::Array(vs) => {
                write!(f, "[")?;
                for val in vs.borrow().iter() {
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            Value::Function(func) => write!(f, "{:?}", func),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
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
                    if a.borrow().len() != b.borrow().len() {
                        false
                    } else {
                        a.borrow()
                            .iter()
                            .zip(b.borrow().iter())
                            .all(|(a, b)| a == b)
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

impl From<i64> for Value {
    fn from(int: i64) -> Value {
        Value::Integer(int)
    }
}

impl From<f64> for Value {
    fn from(num: f64) -> Value {
        Value::Double(num)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Value {
        Value::String(s.to_string())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::String(s)
    }
}

impl From<Vec<Value>> for Value {
    fn from(vs: Vec<Value>) -> Value {
        Value::Array(Rc::new(RefCell::new(vs.into_boxed_slice())))
    }
}

impl From<FunctionValue> for Value {
    fn from(f: FunctionValue) -> Value {
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
        assert_eq!(Value::from(s), Value::String(s.to_string()));
    }

    #[test]
    fn test_from_vec() {
        let vs = vec![
            Value::from(123),
            Value::from(true),
            Value::from(3.21),
            Value::from("hwhwhwh"),
        ];

        let v1 = Value::from(vec![
            Value::from(123),
            Value::from(true),
            Value::from(3.21),
            Value::from("hwhwhwh"),
        ]);
        let v2 = Value::Array(Rc::new(RefCell::new(vs.into_boxed_slice())));

        assert_eq!(v1, v2);
    }

    fn builtin_function(_: &mut Interpreter, _: Vec<Value>) -> Value {
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
            upvalues: Vec::new(),
        };
        let d = FunctionValue::User {
            name: Some(123),
            arity: 1,
            address: 123,
            upvalues: Vec::new(),
        };

        assert_ne!(a, b);
        // might not be equal, function pointers can be different even when they point
        // to the same function
        // assert_eq!(a, c);
        assert_eq!(b, d);

        let av = Value::from(a);
        let bv = Value::from(b);
        let dv = Value::from(d);

        assert_ne!(av, bv);
        assert_eq!(bv, dv);
    }

    #[test]
    fn test_int_equality() {
        let a = Value::from(123_123);
        let b = Value::from(123_123);
        let c = Value::from(432_432);

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
        let a = Value::from("hello");
        let b = Value::from("hello");
        let c = Value::from("world");

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_array_equality() {
        let a = Value::from(vec![Value::from(true), Value::Null, Value::from("abc")]);
        let b = Value::from(vec![Value::from(true), Value::Null, Value::from("abc")]);
        let c = Value::from(vec![Value::from(123)]);

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_integer_truthiness() {
        let a = Value::from(1);
        let b = Value::from(0);

        assert!(a.is_truthy());
        assert!(!b.is_truthy());
    }

    #[test]
    fn test_double_truthiness() {
        let a = Value::from(1.1);
        let b = Value::from(0f64);

        assert!(a.is_truthy());
        assert!(!b.is_truthy());
    }

    #[test]
    fn test_boolean_truthiness() {
        let a = Value::from(true);
        let b = Value::from(false);

        assert!(a.is_truthy());
        assert!(!b.is_truthy());
    }

    #[test]
    fn test_null_truthiness() {
        assert!(!Value::Null.is_truthy());
    }

    #[test]
    fn test_string_truthiness() {
        let a = Value::from("hello");
        let b = Value::from("");

        assert!(a.is_truthy());
        assert!(!b.is_truthy());
    }

    #[test]
    fn test_array_truthiness() {
        let a = Value::from(vec![Value::Null]);
        let b = Value::from(Vec::new());

        assert!(a.is_truthy());
        assert!(!b.is_truthy());
    }

    #[test]
    fn test_function_truthiness() {
        let a = Value::from(FunctionValue::Builtin {
            name: Some(123),
            arity: 3,
            function: builtin_function,
        });

        assert!(a.is_truthy());
    }
}

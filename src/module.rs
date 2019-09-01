use std::collections::HashMap;

use crate::value::Value;

pub struct Module<'a> {
    pub name: &'a str,
    exports: HashMap<usize, Value>,
}

impl<'a> Module<'a> {
    pub fn new(name: &'a str) -> Module<'a> {
        Module {
            name,
            exports: HashMap::new(),
        }
    }

    pub fn export(&mut self, name: usize, value: Value) {
        self.exports.insert(name, value);
    }

    pub fn get(&self, idx: usize) -> Option<&Value> {
        self.exports.get(&idx)
    }
}

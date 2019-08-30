use std::collections::HashMap;

use crate::agent::Agent;
use crate::value::Value;

pub struct Module<'a> {
    pub name: &'a str,
    agent: Agent<'a>,
    exports: HashMap<usize, Value>,
}

impl<'a> Module<'a> {
    pub fn new(agent: Agent<'a>, name: &'a str) -> Module<'a> {
        Module {
            name,
            agent,
            exports: HashMap::new(),
        }
    }

    pub fn export(&mut self, name: &'a str, value: Value) {
        let id = self.agent.intern_string(name);
        self.exports.insert(id, value);
    }

    pub fn get(&self, idx: usize) -> Option<&Value> {
        self.exports.get(&idx)
    }
}

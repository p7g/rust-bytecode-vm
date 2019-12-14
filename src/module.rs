use crate::agent::Agent;
use crate::value::Value;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct ModuleSpec {
    pub name: usize,
    exports: HashSet<usize>,
}

impl ModuleSpec {
    pub fn new(name: usize) -> Self {
        Self {
            name,
            exports: HashSet::new(),
        }
    }

    pub fn add_export(&mut self, name: usize) {
        self.exports.insert(name);
    }
}

#[derive(Debug)]
pub struct Module {
    pub global_scope: HashMap<usize, Value>,
    spec: ModuleSpec,
}

impl Module {
    pub fn new(spec: ModuleSpec, global_scope: HashMap<usize, Value>) -> Self {
        Self { global_scope, spec }
    }

    pub fn name(&self) -> usize {
        self.spec.name
    }

    pub fn resolve_export(&self, agent: &Agent, name: usize) -> Result<Value, String> {
        if let Some(val) = self.global_scope.get(&name).cloned() {
            Ok(val)
        } else {
            Err(format!(
                "Module {} has no export {}",
                agent.string_table[self.spec.name], agent.string_table[name]
            ))
        }
    }
}

use crate::module::ModuleSpec;
use crate::value::Upvalue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Agent {
    pub string_table: Vec<String>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
    modules: Vec<ModuleSpec>,
    module_indices: HashMap<usize, usize>,
    next_module_id: usize,
}

impl Agent {
    pub fn new() -> Agent {
        Agent {
            string_table: Vec::new(),
            upvalues: Vec::new(),
            modules: Vec::new(),
            module_indices: HashMap::new(),
            next_module_id: 0,
        }
    }

    pub fn intern_string(&mut self, s: &str) -> usize {
        if let Some(idx) = self
            .string_table
            .iter()
            .position(|ref interned| *interned == s)
        {
            idx
        } else {
            let idx = self.string_table.len();
            self.string_table.push(String::from(s));
            idx
        }
    }

    pub fn num_modules(&self) -> usize {
        self.modules.len()
    }

    pub fn add_module(&mut self, name: usize, mut spec: ModuleSpec) {
        spec.finalize(self.next_module_id);
        self.module_indices.insert(name, self.next_module_id);
        self.modules.push(spec);
        self.next_module_id += 1;
    }

    pub fn get_module(&mut self, idx: usize) -> &ModuleSpec {
        &self.modules[idx]
    }

    pub fn get_module_by_name(&mut self, name: usize) -> Option<&ModuleSpec> {
        if let Some(idx) = self.module_indices.get(&name) {
            Some(&self.modules[*idx])
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_interning() {
        let mut agent = Agent::new();

        let a = agent.intern_string("hello");
        let b = agent.intern_string("world");
        let c = agent.intern_string("hello");

        assert_eq!(a, c);
        assert_ne!(b, c);
    }
}

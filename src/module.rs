use crate::value::Value;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ModuleSpec {
    id: Option<usize>,
    pub name: usize,
    exports: HashMap<usize, usize>,
    export_names: HashMap<usize, usize>,
    next: usize,
}

impl ModuleSpec {
    pub fn new(name: usize) -> Self {
        Self {
            id: None,
            name,
            next: 0,
            exports: HashMap::new(),
            export_names: HashMap::new(),
        }
    }

    pub fn finalize(&mut self, id: usize) {
        debug_assert!(self.id.is_none());
        self.id = Some(id);
    }

    pub fn id(&self) -> usize {
        if let Some(id) = self.id {
            id
        } else {
            unreachable!("Getting id of module that hasn't been finalized");
        }
    }

    pub fn add_export(&mut self, name: usize) {
        self.exports.insert(name, self.next);
        self.export_names.insert(self.next, name);
        self.next += 1;
    }

    pub fn has_export(&self, name: usize) -> bool {
        self.exports.contains_key(&name)
    }

    pub fn export_id(&self, name: usize) -> usize {
        self.exports[&name]
    }

    pub fn num_exports(&self) -> usize {
        self.next
    }
}

#[derive(Debug)]
pub struct Module {
    pub global_scope: HashMap<usize, Value>,
    exports: Vec<Value>,
    pub spec: ModuleSpec,
}

impl Module {
    pub fn new(spec: ModuleSpec, global_scope: HashMap<usize, Value>) -> Self {
        let mut exports = Vec::with_capacity(spec.num_exports());
        exports.resize_with(spec.num_exports(), Default::default);

        Self {
            global_scope,
            exports,
            spec,
        }
    }

    pub fn name(&self) -> usize {
        self.spec.name
    }

    pub fn id(&self) -> usize {
        self.spec.id()
    }

    pub fn get_export(&self, id: usize) -> Value {
        self.exports[id].clone()
    }

    pub fn set_export(&mut self, id: usize, value: Value) {
        self.exports[id] = value;
    }

    pub fn finalize(&mut self) {
        // FIXME: Copying the values into the exports list from the global scope
        // means that changing an exported value will not affect the value
        // imported from elsewhere.
        for i in 0..self.spec.num_exports() {
            let name = self.spec.export_names[&i];
            self.exports.push(self.global_scope[&name].clone());
        }
    }
}

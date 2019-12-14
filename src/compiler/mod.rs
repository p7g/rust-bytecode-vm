pub(crate) mod artifact;
pub(crate) mod bytecode;
pub(crate) mod codegen;
pub(crate) mod parser;
pub(crate) mod value;

use crate::agent::Agent;
use bytecode::Bytecode;
use std::collections::HashSet;
use std::convert::AsRef;
use std::fs;
use std::path::Path;

type Result = std::result::Result<(), Box<dyn std::error::Error>>;

struct Module {}

pub(crate) struct Compiler<'a> {
    agent: &'a mut Agent,
    bytecode: Bytecode,
    modules: Vec<Module>,
    compiled_modules: HashSet<String>,
}

impl<'a> Compiler<'a> {
    pub(crate) fn new(agent: &'a mut Agent) -> Self {
        Self {
            agent,
            bytecode: Bytecode::new(),
            modules: Vec::new(),
            compiled_modules: HashSet::new(),
        }
    }

    pub(crate) fn compile_file<T>(&mut self, path: T) -> Result
    where
        T: AsRef<Path>,
    {
        let text = fs::read_to_string(path)?;

        let name = path.as_ref().canonicalize()?.to_string_lossy();

        self.compile(name.into_owned(), text)
    }

    pub(crate) fn compile<T>(&mut self, name: String, text: T) -> Result
    where
        T: AsRef<str>,
    {
        if self.compiled_modules.contains(&name) {
            return Ok(());
        }

        let text = text.as_ref();

        let lexer = parser::Lexer::new(text);
        let parser = parser::Parser::new(self.agent, lexer);

        for import in parser.imports() {
            self.compile_file(import)?;
        }

        let mut gen = codegen::CodeGen::new(self.bytecode);

        Ok(())
    }
}

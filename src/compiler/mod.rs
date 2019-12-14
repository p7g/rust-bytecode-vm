pub(crate) mod artifact;
#[macro_use]
pub(crate) mod bytecode;
pub(crate) mod codegen;
pub(crate) mod disassemble;
pub(crate) mod parser;

use std::collections::HashSet;
use std::convert::AsRef;
use std::fs;
use std::path::Path;

use crate::agent::Agent;
use crate::module::ModuleSpec;

use bytecode::Bytecode;

type Result = std::result::Result<(), Box<dyn std::error::Error>>;

pub(crate) struct Compiler<'a> {
    agent: &'a mut Agent,
    bytecode: Option<Bytecode>,
    compiled_modules: HashSet<String>,
}

impl<'a> Compiler<'a> {
    pub(crate) fn new(agent: &'a mut Agent) -> Self {
        Self {
            agent,
            bytecode: Some(Bytecode::new()),
            compiled_modules: HashSet::new(),
        }
    }

    pub(crate) fn code(self) -> Option<Vec<u8>> {
        self.bytecode.map(Bytecode::into)
    }

    pub(crate) fn compile_file<T>(&mut self, path: T) -> Result
    where
        T: AsRef<Path>,
    {
        let path = path.as_ref();
        let name = path.canonicalize()?.to_string_lossy().into_owned();

        let text = fs::read_to_string(&name)?;

        self.compile(name, text)
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
        let mut parser = parser::Parser::new(self.agent, lexer);
        let parsed_module = parser.parse()?;

        for import in parsed_module.imports {
            self.compile_file(import)?;
        }

        let gen = codegen::CodeGen::with_bytecode(self.agent, self.bytecode.take().unwrap());

        self.bytecode.replace(gen.compile(
            ModuleSpec::new(parsed_module.name),
            parsed_module.statements.iter(),
        )?);

        Ok(())
    }
}

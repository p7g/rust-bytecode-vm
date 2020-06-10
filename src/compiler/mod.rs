pub(crate) mod artifact;
#[macro_use]
pub(crate) mod bytecode;
pub(crate) mod codegen;
pub(crate) mod disassemble;
pub(crate) mod parser;
pub(crate) mod opt;

use std::collections::HashSet;
use std::convert::AsRef;
use std::fs;
use std::path::Path;

use crate::agent::Agent;
use crate::debuginfo::DebugInfo;

use bytecode::Bytecode;

type Result = std::result::Result<(), Box<dyn std::error::Error>>;

pub(crate) struct Compiler<'a> {
    agent: &'a mut Agent,
    bytecode: Option<Bytecode>,
    compiled_modules: HashSet<String>,
    pub(crate) debuginfo: DebugInfo,
}

impl<'a> Compiler<'a> {
    pub(crate) fn new(agent: &'a mut Agent) -> Self {
        Self {
            agent,
            bytecode: Some(Bytecode::new()),
            compiled_modules: HashSet::new(),
            debuginfo: DebugInfo::new(),
        }
    }

    pub(crate) fn end(self) -> (Option<Vec<u8>>, DebugInfo) {
        (self.bytecode.map(Bytecode::into), self.debuginfo)
    }

    pub(crate) fn compile_file<T, U>(&mut self, pwd: U, path: T) -> Result
    where
        T: AsRef<Path>,
        U: AsRef<Path>,
    {
        let mut path = path.as_ref();
        let pwd = pwd.as_ref();
        let joined = pwd.join(path);

        if !path.is_absolute() {
            path = joined.as_ref();
        }

        let path = path.canonicalize()?;
        let pwd = path.parent().unwrap();
        let name = path.to_string_lossy().into_owned();

        let text = fs::read_to_string(&name)?;

        self.compile(pwd, name, text)
    }

    pub(crate) fn compile<T, P>(&mut self, pwd: P, name: String, text: T) -> Result
    where
        T: AsRef<str>,
        P: AsRef<Path>,
    {
        if self.compiled_modules.contains(&name) {
            return Ok(());
        }

        let text = text.as_ref();

        let lexer = parser::Lexer::new(&name, text);
        let mut parser = parser::Parser::new(&name, self.agent, lexer);
        let parsed_module = parser.parse()?;

        for import in parsed_module.imports {
            self.compile_file(&pwd, import)?;
        }

        self.agent
            .modules
            .insert(parsed_module.spec.name, parsed_module.spec.clone());
        let gen = codegen::CodeGen::with_bytecode(
            self.agent,
            &mut self.debuginfo,
            self.bytecode.take().unwrap(),
        );

        self.bytecode
            .replace(gen.compile(parsed_module.spec, parsed_module.statements.iter())?);

        self.compiled_modules.insert(name);

        Ok(())
    }
}

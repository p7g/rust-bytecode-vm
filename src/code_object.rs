pub struct CodeObject {
    pub instructions: Vec<u8>,
}

impl CodeObject {
    pub fn new(instructions: Vec<u8>) -> CodeObject {
        CodeObject {
            instructions,
        }
    }
}

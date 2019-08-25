pub struct Agent<'a> {
    pub string_table: Vec<&'a str>,
}

impl<'a> Agent<'a> {
    pub fn new() -> Agent<'a> {
        Agent {
            string_table: Vec::new(),
        }
    }

    pub fn intern_string(&mut self, s: &'a str) -> usize {
        if let Some(idx) = self
            .string_table
            .iter()
            .position(|ref interned| *interned == &s)
        {
            idx
        } else {
            let idx = self.string_table.len();
            self.string_table.push(s);
            idx
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

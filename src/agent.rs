pub struct Agent<'a> {
    pub string_table: Vec<&'a str>,
}

impl<'a> Agent<'a> {
    pub fn new() -> Agent<'a> {
        Agent {
            string_table: Vec::new(),
        }
    }

    pub fn intern_string(&mut self, s: &'a str) -> &'a str {
        if let Some(ptr) = self
            .string_table
            .iter()
            .find(|ref interned| **interned == &s)
        {
            ptr
        } else {
            self.string_table.push(s);
            s
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

        assert_eq!(a.as_ptr(), c.as_ptr());
        assert_ne!(b.as_ptr(), c.as_ptr());
    }
}

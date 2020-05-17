use std::collections::HashMap;
use std::iter::Iterator;

use crate::compiler::parser::Position;

#[derive(Debug, PartialEq)]
pub(crate) struct Context {
    pub(crate) position: Position,
}

#[derive(Debug)]
enum InfoEntry {
    Context(Context),
    Forward(usize),
}

#[derive(Debug)]
pub(crate) struct DebugInfo(HashMap<usize, InfoEntry>);

impl DebugInfo {
    pub(crate) fn new() -> Self {
        DebugInfo(HashMap::new())
    }

    pub(crate) fn insert<T>(&mut self, mut positions: T, ctx: Context)
    where
        T: Iterator<Item = usize>,
    {
        if let Some(original) = positions.next() {
            self.0.insert(original, InfoEntry::Context(ctx));

            for i in positions {
                self.0.entry(i).or_insert(InfoEntry::Forward(original));
            }
        }
    }

    pub(crate) fn get(&self, offset: usize) -> Option<&Context> {
        let mut entry = self.0.get(&offset);

        while let Some(InfoEntry::Forward(i)) = entry {
            entry = self.0.get(i);
        }

        entry.map(|x| match x {
            InfoEntry::Context(c) => c,
            _ => unreachable!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::Position;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_getting_context() {
        let mut dbginfo = DebugInfo::new();
        let pos = Position {
            line: 10,
            column: 11,
        };

        dbginfo.insert(
            vec![1, 2, 3].into_iter(),
            Context {
                position: pos.clone(),
            },
        );

        for i in 1..=3 {
            assert_eq!(
                dbginfo.get(i),
                Some(Context {
                    position: pos.clone()
                })
                .as_ref()
            );
        }
    }
}

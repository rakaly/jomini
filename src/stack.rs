const MAX_DEPTH: usize = 30;

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum StackType {
    Object(usize),
    Array(usize),
}

impl Default for StackType {
    fn default() -> Self {
        StackType::Object(0)
    }
}

/// A pseudo fixed-length vector with a fallible push method. The core binary parser shouldn't
/// allocate so using a Vec was out of the question and outsourcing to an external crate (eg:
/// arrayvec) seemed overkill. Having a fixed length depth that a parser can reach has the nice
/// property that it prevents any sort of exhaustion or overflow.
#[derive(Debug, PartialEq, Clone, Default)]
pub(crate) struct Stack {
    depth: usize,
    values: [StackType; MAX_DEPTH],
}

impl Stack {
    pub fn push(&mut self, val: StackType) -> bool {
        if self.depth >= self.values.len() {
            false
        } else {
            self.values[self.depth] = val;
            self.depth += 1;
            true
        }
    }

    pub fn pop(&mut self) -> Option<StackType> {
        if self.depth > 0 {
            self.depth -= 1;
            let res = self.values[self.depth];
            Some(res)
        } else {
            None
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.depth == 0
    }
}

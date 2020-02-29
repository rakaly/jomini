const MAX_DEPTH: usize = 16;

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub(crate) enum DepthType {
    Object = 1,
    Array,
}

/// A pseudo fixed-length vector with a fallible push method. The core binary parser shouldn't
/// allocate so using a Vec was out of the question and outsourcing to an external crate (eg:
/// arrayvec) seemed overkill. Having a fixed length depth that a parser can reach has the nice
/// property that it prevents any sort of exhaustion or overflow.
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Depth {
    depth: usize,
    values: [DepthType; MAX_DEPTH],
}

impl Depth {
    pub fn new() -> Self {
        Depth {
            depth: 0,
            values: [DepthType::Object; MAX_DEPTH],
        }
    }

    pub fn last(&self) -> Option<DepthType> {
        if self.depth > 0 {
            Some(self.values[self.depth - 1])
        } else {
            None
        }
    }

    pub fn push(&mut self, val: DepthType) -> bool {
        if self.depth >= self.values.len() {
            false
        } else {
            self.values[self.depth] = val;
            self.depth += 1;
            true
        }
    }

    pub fn len(&self) -> usize {
        self.depth
    }

    pub fn pop(&mut self) -> Option<DepthType> {
        if self.depth > 0 {
            self.depth -= 1;
            let res = self.values[self.depth];
            Some(res)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_depth_arr() {
        let mut depths = Depth::new();
        assert_eq!(depths.pop(), None);
        assert_eq!(depths.last(), None);

        let push = depths.push(DepthType::Object);
        assert!(push);
        assert_eq!(depths.last(), Some(DepthType::Object));
        assert_eq!(depths.pop(), Some(DepthType::Object));
        assert_eq!(depths.pop(), None);
        assert_eq!(depths.last(), None);

        for _ in 0..MAX_DEPTH {
            assert!(depths.push(DepthType::Array));
        }

        assert!(!depths.push(DepthType::Array));
        assert_eq!(depths.pop(), Some(DepthType::Array));
    }
}

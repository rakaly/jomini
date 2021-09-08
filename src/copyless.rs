///! Trimmed down version of copyless v0.1.5: https://github.com/kvark/copyless

/// A typesafe helper that separates new value construction from
/// vector growing, allowing LLVM to ideally construct the element in place.
pub struct VecAllocation<'a, T: 'a> {
    vec: &'a mut Vec<T>,
    index: usize,
}

impl<'a, T> VecAllocation<'a, T> {
    /// Consumes self and writes the given value into the allocation.
    // writing is safe because alloc() ensured enough capacity
    // and `Allocation` holds a mutable borrow to prevent anyone else
    // from breaking this invariant.
    #[inline(always)]
    pub fn init(self, value: T) -> usize {
        unsafe {
            std::ptr::write(self.vec.as_mut_ptr().add(self.index), value);
            self.vec.set_len(self.index + 1);
        }
        self.index
    }
}

/// Helper trait for a `Vec` type that allocates up-front.
pub trait VecHelper<T> {
    /// Grows the vector by a single entry, returning the allocation.
    fn alloc(&mut self) -> VecAllocation<T>;
}

impl<T> VecHelper<T> for Vec<T> {
    fn alloc(&mut self) -> VecAllocation<T> {
        let index = self.len();
        if self.capacity() == index {
            self.reserve(1);
        }
        VecAllocation { vec: self, index }
    }
}

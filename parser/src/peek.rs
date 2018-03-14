pub struct Peek2<I: Iterator> {
    iter: I,
    items: (Option<I::Item>, Option<I::Item>),
}

impl<I: Iterator> Peek2<I> {
    pub fn new(iter: I) -> Self {
        Peek2 {
            iter,
            items: (None, None),
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.fill();
        self.items.0.as_ref()
    }

    pub fn peek_next(&mut self) -> Option<&I::Item> {
        self.fill();
        self.items.1.as_ref()
    }

    fn fill(&mut self) {
        if self.items.0.is_none() {
            self.items = (self.iter.next(), self.iter.next());
        }
    }
}

impl<I> Iterator for Peek2<I>
    where I: Iterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.fill();
        let next = self.items.0.take();
        self.items.0 = self.items.1.take();
        self.items.1 = self.iter.next();
        next
    }
}

#[cfg(test)]
mod test {
    use super::Peek2;

    #[test]
    fn peek() {
        let vec = vec![1, 2, 3];
        let mut items = Peek2::new(vec.iter());

        assert_eq!(Some(&&1), items.peek());
        assert_eq!(Some(&&2), items.peek_next());

        assert_eq!(Some(&1), items.next());

        assert_eq!(Some(&&2), items.peek());
        assert_eq!(Some(&&3), items.peek_next());

        assert_eq!(Some(&2), items.next());

        assert_eq!(Some(&&3), items.peek());
        assert_eq!(None, items.peek_next());

        assert_eq!(Some(&3), items.next());

        assert_eq!(None, items.peek());
        assert_eq!(None, items.peek_next());
        assert_eq!(None, items.next());
    }
}

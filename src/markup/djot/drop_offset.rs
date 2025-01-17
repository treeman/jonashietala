use jotdown::Event;
use std::ops::Range;

pub struct DropOffset<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> {
    parent: I,
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> DropOffset<'a, I> {
    pub fn new(parent: I) -> Self {
        Self { parent }
    }
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> Iterator for DropOffset<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parent.next().map(|x| x.0)
    }
}

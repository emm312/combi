use std::{collections::HashSet, hash::Hash};

pub struct Dedup_<I>
where
    I: Iterator,
{
    iter: I,
    set: HashSet<I::Item>,
}

impl<I> Iterator for Dedup_<I>
where
    I: Iterator,
    I::Item: Eq + Hash + Clone,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let mut out: I::Item = self.iter.next()?;

        while self.set.contains(&out) {
            out = self.iter.next()?;
        }

        self.set.insert(out.clone());
        Some(out)
    }
}

pub trait Dedup: Iterator {
    fn dedup(self) -> Dedup_<Self>
    where
        Self: Sized,
    {
        Dedup_ {
            iter: self,
            set: HashSet::new(),
        }
    }
}

impl<I: Iterator> Dedup for I {}

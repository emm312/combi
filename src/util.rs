use std::{collections::HashSet, hash::Hash};

pub struct Dedup<I>
where
    I: Iterator,
{
    iter: I,
    set: HashSet<I::Item>,
}

impl<I> Iterator for Dedup<I>
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

pub trait DedupI: Iterator {
    fn dedup(self) -> Dedup<Self>
    where
        Self: Sized,
    {
        Dedup {
            iter: self,
            set: HashSet::new(),
        }
    }
}

impl<I: Iterator> DedupI for I {}

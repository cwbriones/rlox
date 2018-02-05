pub trait Partition<T, U> {
    fn partition<A, B>(self) -> (A, B)
        where
            A: Default + Extend<T>,
            B: Default + Extend<U>;
}

impl<I, T, U> Partition<T, U> for I
    where I: Iterator<Item=Result<T, U>>
{
    fn partition<A, B>(self) -> (A, B)
        where
            A: Default + Extend<T>,
            B: Default + Extend<U>,
    {
        let mut oks: A = Default::default();
        let mut errs: B = Default::default();

        for res in self {
            match res {
                Ok(ok) => oks.extend(Some(ok)),
                Err(err) => errs.extend(Some(err)),
            }
        }
        (oks, errs)
    }
}

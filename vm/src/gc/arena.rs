use std::ops::Deref;

struct Arena<T> {
    storage: Vec<Entry<T>>,
    next: usize,
}

enum Entry<T> {
    Occupied(OccupiedEntry<T>),
    Vacant(usize),
}

struct OccupiedEntry<T> {
    item: T,
    mark: bool,
}

impl<T> Entry<T> {
    fn insert(&mut self, t: T) -> Option<usize> {
        if let Entry::Vacant(index) = *self {
            *self = Entry::Occupied(OccupiedEntry{item: t, mark: false});
            return Some(index);
        }
        None
    }

    fn is_vacant(&self) -> bool {
        if let Entry::Vacant(_) = *self {
            true
        } else {
            false
        }
    }

    fn should_collect(&self) -> bool {
        match *self {
            Entry::Occupied(ref o) => !o.mark,
            _ => false,
        }
    }
}

impl<T> Arena<T> {
    fn new(capacity: usize) -> Self {
        let mut storage = Vec::with_capacity(capacity);
        for i in 0..(capacity - 1) {
            storage.push(Entry::Vacant(i + 1));
        }
        // Treat this as NULL
        storage.push(Entry::Vacant(::std::usize::MAX));

        Arena {
            storage,
            next: 0,
        }
    }

    fn allocate(&mut self, item: T) -> ArenaPtr<T>
    {
        if self.is_full() {
            panic!("OOM");
        }
        let index = self.next;
        self.next = self.storage[self.next]
            .insert(item)
            .unwrap();

        if let Entry::Occupied(ref mut o) = self.storage[index] {
            return ArenaPtr::new(o as *mut _)
        }
        unreachable!()
    }

    fn sweep(&mut self) {
        for (i, entry) in self.storage.iter_mut().enumerate() {
            if entry.should_collect() {
                *entry = Entry::Vacant(self.next);
                self.next = i;
            }
        }
    }

    fn capacity(&self) -> usize {
        self.storage.capacity()
    }

    fn is_full(&self) -> bool {
        self.storage.capacity() == self.storage.len()
    }

    fn is_empty(&self) -> bool {
        self.storage.is_empty()
    }
}

const CAPACITY: usize = 8192;

pub struct ArenaSet<T> {
    arenas: Vec<Box<Arena<T>>>,
    capacity: usize,
}

impl<T> ArenaSet<T> {
    pub fn new(arena_capacity: usize) -> Self {
        let mut arenas = Vec::new();
        arenas.push(Box::new(Arena::new(arena_capacity)));

        ArenaSet {
            arenas,
            capacity: arena_capacity,
        }
    }

    pub fn sweep(&mut self) {
        for a in &mut self.arenas {
            a.sweep();
        }
        self.arenas.retain(|a| !a.is_empty());
    }

    pub fn allocate(&mut self, t: T) -> ArenaPtr<T> {
        for arena in &mut self.arenas {
            if !arena.is_full() {
                return arena.allocate(t);
            }
        }

        let mut new_arena = Box::new(Arena::new(self.capacity));
        let ptr = new_arena.allocate(t);
        self.arenas.push(new_arena);
        ptr
    }

    pub fn total_capacity(&self) -> usize {
        self.arenas.len() * self.capacity
    }
}

// TODO: We can make this the size of one pointer if use unsafe
pub struct ArenaPtr<T> {
    entry: *mut OccupiedEntry<T>
}

impl<T> Clone for ArenaPtr<T> {
    fn clone(&self) -> Self {
        ArenaPtr::new(self.entry)
    }
}

impl<T> ::std::marker::Copy for ArenaPtr<T> {}

impl<T> ArenaPtr<T> {
    fn new(entry: *mut OccupiedEntry<T>) -> Self {
        ArenaPtr { entry }
    }

    fn mark(&mut self) {
        unsafe {
            (*self.entry).mark = true;
        }
    }
}

impl<T> Deref for ArenaPtr<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe {
            &(*self.entry).item
        }
    }
}

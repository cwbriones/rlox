use self::arena::ArenaSet;
use self::object::Object;
use self::object::ObjectHandle;
use self::value::Value;

pub mod value;
pub mod object;

mod arena;

// Things to do for GC:
//
// Add a root bit.
// Add a marked bit.
//
// Move the free list info into the arena, so that we can hand out the raw arena pointers.

// I like the ideas from the multiple-arena implementation used by oxischeme.
//
// But I want the pointer to encapsulate all state. We don't have to worry about rooting / unrooting
// ourselves, because the only roots will ever be the following:
//
// local variables currently in scope
// global variables (static)
//
// So unlike oxischeme, we don't have to track this between evaluation of operations.
// The vm will root/unroot objects as they come into scope.
//
// We will have the following GC object types:
//
// * NativeFunction
// * LoxFunction
// * LoxInstance
// * LoxClass
// * String
//
// We can keep the arenas (for now, at least) and reuse them for each of these.

const ARENASET_CAPACITY: usize = 4096;
const INITIAL_NEXT_GC: usize = 8192;
const GC_GROW_FACTOR: usize = 2;

pub struct Gc {
    objects: ArenaSet<Object>,
    allocations: usize,
    next_gc: usize,
}

impl Gc {
    pub fn new() -> Self {
        Gc {
            objects: ArenaSet::new(ARENASET_CAPACITY),
            allocations: 0,
            next_gc: INITIAL_NEXT_GC,
        }
    }

    pub fn allocate<F, I>(&mut self, obj: Object, roots_fn: F) -> ObjectHandle
        where
            F: FnOnce() -> I,
            I: Iterator<Item=Value>,
    {
        self.on_allocation(roots_fn);
        ObjectHandle::new(self.objects.allocate(obj))
    }

    pub fn allocate_string<F, I>(&mut self, s: String, roots_fn: F) -> ObjectHandle 
        where
            F: FnOnce() -> I,
            I: Iterator<Item=Value>,
    {
        self.on_allocation(roots_fn);
        let obj = Object::string(s);
        ObjectHandle::new(self.objects.allocate(obj))
    }

    fn on_allocation<F, I>(&mut self, roots: F)
        where
            F: FnOnce() -> I,
            I: Iterator<Item=Value>
    {
        self.allocations += 1;
        if self.allocations > self.next_gc {
            self.gc_collect(roots);
        }
    }

    fn gc_collect<F, I>(&mut self, roots: F) 
        where
            F: FnOnce() -> I,
            I: Iterator<Item=Value>
    {
        // Mark
        roots().filter_map(|v| v.as_object()).for_each(|mut o| o.trace());

        // Sweep
        self.objects.sweep();

        // Reset
        self.allocations = 0;
        self.next_gc *= GC_GROW_FACTOR;
    }
}

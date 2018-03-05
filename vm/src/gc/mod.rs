use self::arena::ArenaSet;
use self::object::Object;
use self::object::ObjectHandle;

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
    roots: Vec<ObjectHandle>,
}

impl Gc {
    pub fn new() -> Self {
        Gc {
            objects: ArenaSet::new(ARENASET_CAPACITY),
            allocations: 0,
            next_gc: INITIAL_NEXT_GC,
            roots: Vec::new(),
        }
    }

    pub fn allocate_string(&mut self, s: String) -> ObjectHandle {
        self.on_allocation();
        let obj = Object::string(s);
        ObjectHandle::new(self.objects.allocate(obj))
    }

    pub fn root(&mut self, obj: ObjectHandle) {
        self.roots.push(obj);
    }

    fn on_allocation(&mut self) {
        self.allocations += 1;
        if self.allocations > self.next_gc {
            self.gc_collect();
        }
    }

    fn gc_collect(&mut self) {
        // Mark
        for root in &mut self.roots {
            if root.is_marked() {
                root.mark();
            }
        }

        // Sweep
        self.objects.sweep();

        // Reset
        self.allocations = 0;
        self.next_gc *= GC_GROW_FACTOR;
    }
}


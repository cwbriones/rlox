use super::arena::ArenaPtr;

pub enum Object {
    String(ArenaPtr<String>)
}

impl Object {
}

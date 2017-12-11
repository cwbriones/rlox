use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::Value;

#[derive(Debug, Clone)]
pub struct LoxInstance {
    fields: Rc<RefCell<HashMap<String, Value>>>,
}

impl LoxInstance {
    fn new() -> Self {
        let fields = Rc::new(RefCell::new(HashMap::new()));
        LoxInstance {
            fields,
        }
    }

    pub fn get(&self, field: &str) -> Option<Value> {
        self.fields.borrow().get(field).map(Clone::clone)
    }

    pub fn set(&self, field: &str, value: Value) {
        let mut fields = self.fields.borrow_mut();
        fields.insert(field.into(), value);
    }

    fn this(&self) -> Self {
        self.clone()
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, other: &LoxInstance) -> bool {
        &*self.fields as *const _ == &*other.fields as *const _
    }
}

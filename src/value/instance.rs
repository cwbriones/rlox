use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::Access;
use super::Value;
use super::LoxClass;

#[derive(Clone)]
pub struct LoxInstance {
    class: Rc<LoxClass>,
    fields: Rc<RefCell<HashMap<String, Value>>>,
}

impl LoxInstance {
    pub fn new(class: Rc<LoxClass>) -> Self {
        let fields = Rc::new(RefCell::new(HashMap::new()));
        LoxInstance {
            class,
            fields,
        }
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, other: &LoxInstance) -> bool {
        &*self.fields as *const _ == &*other.fields as *const _
    }
}

impl ::std::fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "'{}' instance", self.class.name())
    }
}

impl Access for LoxInstance {
    fn get(&self, field: &str) -> Option<Value> {
        self.class
            .method(field)
            .map(|m| {
                let this = Value::Instance(self.clone());
                m.bind(this).into()
            })
            .or_else(|| self.fields.borrow().get(field).map(Clone::clone))
    }

    fn set(&mut self, field: &str, value: Value) {
        let mut fields = self.fields.borrow_mut();
        fields.insert(field.into(), value);
    }
}

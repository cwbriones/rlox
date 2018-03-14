use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::Value;
use super::callable::Callable;
use super::callable::LoxClass;

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

    pub fn get(&self, field: &str) -> Option<Value> {
        self.fields.borrow()
            .get(field)
            .map(Clone::clone)
            .or_else(|| self.get_method(field))
    }

    fn get_method(&self, method: &str) -> Option<Value> {
        self.class.method(method)
            .map(|m| {
                let this = Value::Instance(self.clone());
                let bound = m.bind(this);
                Value::Callable(Callable::Function(bound))
            })
    }

    pub fn set(&self, field: &str, value: Value) {
        let mut fields = self.fields.borrow_mut();
        fields.insert(field.into(), value);
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, other: &LoxInstance) -> bool {
        &*self.fields as *const _ == &*other.fields as *const _
    }
}

impl ::std::fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{} instance", self.class.name())
    }
}

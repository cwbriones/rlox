use std::collections::BTreeMap;
use value::Value;

use std::rc::Rc;
use std::cell::RefCell;

struct EnvNode {
    map: BTreeMap<String, Rc<Value>>,
    parent: Option<Rc<RefCell<EnvNode>>>,
}

impl EnvNode {
    fn new() -> Self {
        EnvNode {
            map: BTreeMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: Rc<RefCell<EnvNode>>) -> Self {
        EnvNode {
            map: BTreeMap::new(),
            parent: Some(parent),
        }
    }

    fn get(&self, key: &str) -> Option<Rc<Value>> {
        self.map.get(key).map(Clone::clone)
    }

    fn set(&mut self, key: &str, value: Value) -> bool {
        self.map.insert(key.to_owned(), Rc::new(value)).is_some()
    }
}

// TODO: Add a wrapper environment that knows about globals for the interpreter
// so that get/sets are uniform wrt the Variable type.
#[derive(Clone)]
pub struct Environment {
    node: Rc<RefCell<EnvNode>>,
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        let ptr = &*self.node as *const _;
        let other_ptr = &*other.node as *const _;
        ptr == other_ptr
    }
}

#[derive(PartialEq, Debug)]
pub struct Variable {
    name: String,
    depth: Option<usize>,
}

impl Variable {
    pub fn new_global(name: &str) -> Self {
        Variable {
            name: name.to_owned(),
            depth: None,
        }
    }

    pub fn new_local(name: &str) -> Self {
        Variable {
            name: name.to_owned(),
            depth: Some(0),
        }
    }

    pub fn resolve(&mut self, depth: usize) {
        self.depth = Some(depth);
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn depth(&self) -> Option<usize> {
        self.depth
    }
}

impl Environment {
    pub fn new() -> Self {
        let node = Rc::new(RefCell::new(EnvNode::new()));
        Environment { node }
    }

    pub fn get_at(&self, key: &str, depth: usize) -> Option<Rc<Value>> {
        self.ancestor(depth)
            .and_then(|ancestor| ancestor.borrow().get(key).clone())
    }

    pub fn set(&mut self, var: &Variable, value: Value) -> bool {
        self.set_at(var.name(), value, 0)
    }

    pub fn set_at(&mut self, key: &str, value: Value, depth: usize) -> bool {
        self.ancestor(depth)
            .map(|ancestor| ancestor.borrow_mut().set(key, value))
            .unwrap_or(false)
    }

    pub fn extend(&self) -> Self {
        let node = Rc::new(RefCell::new(
            EnvNode::with_parent(self.node.clone())
        ));
        Environment { node }
    }

    fn ancestor(&self, depth: usize) -> Option<Rc<RefCell<EnvNode>>> {
        let mut node = Some(self.node.clone());

        for _ in 0..depth {
            // Temporary introduced to get around borrowck
            let mut next = None;
            if let Some(ref node) = node {
                let bn = node.borrow();
                next = bn.parent.clone();
            }
            if next.is_none() {
                return None
            }
            node = next;
        }
        node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_environment() {
        let mut env = Environment::new();
        assert_eq!(None, env.get_at("a", 0));
        env.set_at("a", Value::True, 0);
        assert_eq!(Some(Rc::new(Value::True)), env.get_at("a", 0));
        assert_eq!(None, env.get_at("b", 0));
    }

    #[test]
    fn get_at() {
        let mut outer = Environment::new();
        let mut inner = outer.extend();
        let mut inner2 = inner.extend();

        outer.set_at("a", Value::Nil, 0);
        inner.set_at("a", Value::True, 0);
        inner2.set_at("a", Value::False, 0);

        assert_eq!(Some(Rc::new(Value::False)), inner2.get_at("a", 0));
        assert_eq!(Some(Rc::new(Value::True)), inner2.get_at("a", 1));
        assert_eq!(Some(Rc::new(Value::Nil)), inner2.get_at("a", 2));
        assert_eq!(None, inner2.get_at("a", 3));

        assert_eq!(Some(Rc::new(Value::True)), inner.get_at("a", 0));
        assert_eq!(Some(Rc::new(Value::Nil)), inner.get_at("a", 1));
        assert_eq!(None, inner.get_at("a", 2));

        assert_eq!(Some(Rc::new(Value::Nil)), outer.get_at("a", 0));
    }

    #[test]
    fn set_at() {
        let mut outer = Environment::new();
        let mut inner = outer.extend();
        let mut inner2 = inner.extend();

        outer.set_at("a", Value::Nil, 0);
        inner.set_at("a", Value::True, 0);
        inner2.set_at("a", Value::False, 0);

        inner2.set_at("a", Value::Number(1.0), 2);
        assert_eq!(Some(Rc::new(Value::Number(1.0))), outer.get_at("a", 0));

        inner.set_at("a", Value::Number(2.0), 1);
        assert_eq!(Some(Rc::new(Value::Number(2.0))), outer.get_at("a", 0));

        inner2.set_at("b", Value::Number(3.0), 1);
        assert_eq!(Some(Rc::new(Value::Number(3.0))), inner.get_at("b", 0));
    }
}

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

    fn lookup(&self, key: &str) -> Option<Rc<Value>> {
        self.map.get(key).map(Clone::clone)
    }

    fn parent(&self) -> Option<Rc<RefCell<EnvNode>>> {
        self.parent.clone()
    }
}

#[derive(Clone)]
pub struct Environment {
    node: Rc<RefCell<EnvNode>>,
}

impl Environment {
    pub fn new() -> Self {
        let node = Rc::new(RefCell::new(EnvNode::new()));
        Environment { node }
    }

    pub fn lookup(&self, key: &str) -> Option<Rc<Value>> {
        let mut node = Some(self.node.clone());

        loop {
            let mut next = None;
            if let Some(ref n) = node {
                let bn = n.borrow();
                match bn.lookup(key) {
                    s@Some(_) => return s.clone(),
                    None      => next = bn.parent()
                }
            }
            if next.is_none() {
                return None;
            }
            node = next;
        }
    }

    pub fn define(&mut self, key: &str) {
        let mut inner = self.node.borrow_mut();
        inner.map.insert(key.into(), Rc::new(Value::Nil));
    }

    pub fn bind(&mut self, key: &str, value: Value) {
        let mut inner = self.node.borrow_mut();
        inner.map.insert(key.into(), Rc::new(value));
    }

    pub fn extend(&self) -> Self {
        let node = Rc::new(RefCell::new(
            EnvNode::with_parent(self.node.clone())
        ));
        Environment { node }
    }

    pub fn parent(&self) -> Option<Self> {
        let node = self.node.borrow();
        node.parent().map(|rc| Environment { node: rc })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let mut env = Environment::new();
        assert_eq!(None, env.lookup("a"));
        env.bind("a", Value::True);
        assert_eq!(Some(Rc::new(Value::True)), env.lookup("a"));
        assert_eq!(None, env.lookup("b"));
    }

    #[test]
    fn scope() {
        let mut outer = Environment::new();
        outer.bind("b", Value::Number(10.0));
        outer.bind("c", Value::Number(3.0));

        let mut inner = outer.extend();
        inner.bind("a", Value::Number(1.0));
        inner.bind("b", Value::Number(2.0));

        // inner scope only
        assert_eq!(Some(Rc::new(Value::Number(1.0))), inner.lookup("a"));
        // outer scope only
        assert_eq!(Some(Rc::new(Value::Number(3.0))), inner.lookup("c"));
        // shadowed var
        assert_eq!(Some(Rc::new(Value::Number(2.0))), inner.lookup("b"));
        // neither
        assert_eq!(None, inner.lookup("d"));
    }

    #[test]
    fn define() {
        let mut env = Environment::new();
        env.define("a");
        assert!(env.lookup("a").is_some());
        assert_eq!(Some(Rc::new(Value::Nil)), env.lookup("a"));
    }
}

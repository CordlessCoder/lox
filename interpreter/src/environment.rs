use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{eval::EvalError, types::Value};

type Scope<'s> = HashMap<&'s str, Value<'s>>;
#[derive(Debug, Clone)]
struct Definition<'s> {
    name: &'s str,
    old_value: Option<Value<'s>>,
}

#[derive(Debug, Default)]
pub struct Environment<'s>(Rc<RefCell<EnvState<'s>>>);

impl Clone for Environment<'_> {
    fn clone(&self) -> Self {
        Self(Rc::new(RefCell::new(self.0.borrow().clone())))
    }
}

#[derive(Debug, Clone, Default)]
struct EnvState<'s> {
    // PERF: Keep only one for the current state of the scope for O(1) indexing.
    scope: Scope<'s>,
    /// Tracks definitions/shadows created by local scopes, to be reversed
    layers: Vec<Vec<Definition<'s>>>,
}

impl<'s> EnvState<'s> {
    pub fn push_scope(&mut self) {
        self.layers.push(Vec::new());
    }
    pub fn pop_scope(&mut self) {
        let shadows = self
            .layers
            .pop()
            .expect("Can't pop a scope without pushing one first.");
        for Definition { name, old_value } in shadows.into_iter().rev() {
            self.scope.insert(name, old_value.unwrap_or(Value::Nil));
        }
    }
    pub fn clear(&mut self) {
        self.layers.clear();
        self.scope.clear();
    }
    pub fn define(&mut self, name: &'s str, value: Value<'s>) {
        let old_value = self.scope.insert(name, value);
        if let Some(level) = self.layers.last_mut() {
            level.push(Definition { name, old_value });
        }
    }
    pub fn assign(&mut self, name: &'s str, value: Value<'s>) -> Result<(), EvalError<'s>> {
        use std::collections::hash_map::Entry;
        match self.scope.entry(name) {
            Entry::Vacant(_) => return Err(EvalError::UndefinedIdent(name.to_string())),
            Entry::Occupied(mut o) => o.insert(value),
        };
        Ok(())
    }
    pub fn get(&self, name: &str) -> Option<Value<'s>> {
        self.scope.get(name).cloned()
    }
}

pub struct EnvScopeGuard<'s> {
    env: Rc<RefCell<EnvState<'s>>>,
}

impl Drop for EnvScopeGuard<'_> {
    fn drop(&mut self) {
        self.env.borrow_mut().pop_scope();
    }
}

impl<'s> Environment<'s> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn clear(&mut self) {
        self.0.borrow_mut().clear();
    }
    pub fn define(&mut self, name: &'s str, value: Value<'s>) {
        self.0.borrow_mut().define(name, value);
    }
    pub fn assign(&mut self, name: &'s str, value: Value<'s>) -> Result<(), EvalError<'s>> {
        self.0.borrow_mut().assign(name, value)
    }
    pub fn get(&self, name: &str) -> Option<Value<'s>> {
        self.0.borrow().get(name)
    }
    pub fn scope_guard(&mut self) -> EnvScopeGuard<'s> {
        self.0.borrow_mut().push_scope();
        EnvScopeGuard {
            env: Rc::clone(&self.0),
        }
    }
}

use std::collections::HashMap;

use vm_types::Value;

use crate::eval::EvalError;

type Scope<'s> = HashMap<&'s str, Value>;
#[derive(Debug, Clone)]
struct Definition<'s> {
    name: &'s str,
    old_value: Option<Value>,
}

#[derive(Debug, Clone, Default)]
pub struct Environment<'s> {
    // PERF: Keep only one for the current state of the scope for O(1) indexing.
    scope: Scope<'s>,
    /// Tracks definitions/shadows created by local scopes, to be reversed
    layers: Vec<Vec<Definition<'s>>>,
}

impl<'s> Environment<'s> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn define(&mut self, name: &'s str, value: Value) {
        let old_value = self.scope.insert(name, value);
        if let Some(level) = self.layers.last_mut() {
            level.push(Definition { name, old_value });
        }
    }
    pub fn assign(&mut self, name: &'s str, value: Value) -> Result<(), EvalError> {
        use std::collections::hash_map::Entry;
        match self.scope.entry(name) {
            Entry::Vacant(_) => return Err(EvalError::UndefinedIdent(name.to_string())),
            Entry::Occupied(mut o) => o.insert(value),
        };
        Ok(())
    }
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.scope.get(name)
    }
    pub fn push_scope(&mut self) {
        self.layers.push(Vec::new());
    }
    pub fn pop_scope(&mut self) {
        let shadows = self
            .layers
            .pop()
            .expect("Can't pop a scope without pushing one first.");
        for Definition { name, old_value } in shadows {
            self.scope.insert(name, old_value.unwrap_or(Value::Nil));
        }
    }
}

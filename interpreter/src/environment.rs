use std::collections::HashMap;

use vm_types::Value;

use crate::eval::EvalError;

type Scope<'s> = HashMap<&'s str, Value>;

#[derive(Debug, Clone, Default)]
pub struct Environment<'s> {
    global_variables: Scope<'s>,
    scopes: Vec<Scope<'s>>,
}

impl<'s> Environment<'s> {
    fn top_scope(&mut self) -> &mut Scope<'s> {
        if let Some(last) = self.scopes.last_mut() {
            return last;
        }
        &mut self.global_variables
    }
    fn all_scopes(&self) -> impl Iterator<Item = &Scope<'s>> {
        self.scopes
            .iter()
            .rev()
            .chain(core::iter::once(&self.global_variables))
    }
    fn all_scopes_mut(&mut self) -> impl Iterator<Item = &mut Scope<'s>> {
        let Self {
            global_variables,
            scopes,
        } = self;
        scopes
            .iter_mut()
            .rev()
            .chain(core::iter::once(global_variables))
    }
    pub fn define(&mut self, name: &'s str, value: Value) {
        self.top_scope().insert(name, value);
    }
    pub fn assign(&mut self, name: &'s str, value: Value) -> Result<(), EvalError> {
        use std::collections::hash_map::Entry;
        let Some(mut var) = self
            .all_scopes_mut()
            .flat_map(|scope| match scope.entry(name) {
                Entry::Vacant(_) => None,
                Entry::Occupied(o) => Some(o),
            })
            .next()
        else {
            return Err(EvalError::UndefinedIdent(name.to_string()));
        };
        var.insert(value);
        Ok(())
    }
    pub fn get(&mut self, name: &str) -> Option<&Value> {
        self.all_scopes().flat_map(|s| s.get(name)).next()
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }
    pub fn pop_scope(&mut self) {
        self.scopes
            .pop()
            .expect("Attempted to pop a scope without pushing one.");
    }
}

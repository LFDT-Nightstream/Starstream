use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Multiplicity<S> {
    Unused,
    Linear { witness: S },
    Affine { witness: S },
    Many { witness: ManyWitness<S> },
}

impl<S: Clone> Multiplicity<S> {
    pub fn witness(&self) -> Option<S> {
        match self {
            Multiplicity::Unused => None,
            Multiplicity::Linear { witness } => Some(witness.clone()),
            Multiplicity::Affine { witness } => Some(witness.clone()),
            Multiplicity::Many {
                witness: ManyWitness::UsedInLoop { span },
            } => Some(span.clone()),
            Multiplicity::Many {
                witness: ManyWitness::UsedTwice { first, then: _ },
            } => Some(first.clone()),
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ManyWitness<S> {
    UsedInLoop { span: S },
    UsedTwice { first: S, then: S },
}

pub struct Scope<V, S> {
    declarations: HashSet<V>,
    multiplicities: HashMap<V, Multiplicity<S>>,
    is_loop_scope: bool,
}

pub struct ResourceTracker<K, S> {
    stack: Vec<Scope<K, S>>,
}

impl<V: Hash + Eq + Copy + std::fmt::Debug, S: Clone> ResourceTracker<V, S> {
    pub fn new() -> Self {
        let mut res = Self { stack: vec![] };

        res.push_branch();

        res
    }

    pub fn finish(&mut self) -> HashMap<V, Multiplicity<S>> {
        let res = self.stack.pop().unwrap().multiplicities;

        assert!(self.stack.is_empty());

        res
    }

    pub fn push_branch(&mut self) {
        self.stack.push(Scope {
            declarations: HashSet::new(),
            is_loop_scope: false,
            multiplicities: HashMap::new(),
        });
    }

    pub fn push_loop_scope(&mut self) {
        self.stack.push(Scope {
            is_loop_scope: true,
            declarations: HashSet::new(),
            multiplicities: HashMap::new(),
        });
    }

    pub fn pop_branches(&mut self, n_branches: u32) {
        for _ in 0..n_branches - 1 {
            let branch = self.stack.pop().unwrap();
            let merged = self.stack.last_mut().unwrap();

            let mut new_multiplicities = HashMap::new();

            for var in merged
                .multiplicities
                .keys()
                .chain(branch.multiplicities.keys())
            {
                let a = merged.multiplicities.get(var).cloned();
                let b = branch.multiplicities.get(var).cloned();

                // if the varible is declared in one of the branches, then
                // it can't appear on the others, so we don't change it's
                // classification
                if merged.declarations.contains(var) {
                    new_multiplicities.insert(*var, a.unwrap());
                } else if branch.declarations.contains(var) {
                    new_multiplicities.insert(*var, b.unwrap());
                } else {
                    // if not, we are considering a variable defined outside the branches
                    //
                    // we need to account for this by making linears affine in this case:
                    //
                    // let x = ...
                    // if (cond) {
                    //  f(x)
                    // }
                    // else {}
                    //
                    // or the variable unrestricted in cases like this:
                    //
                    // let x = ...
                    // if (cond) {
                    //   f(x);
                    //   f(x);
                    // }
                    // else {
                    //   f(x);
                    // }
                    let merged_multiplicity = match (
                        a.unwrap_or(Multiplicity::Unused),
                        b.unwrap_or(Multiplicity::Unused),
                    ) {
                        (Multiplicity::Many { witness }, Multiplicity::Many { witness: _ }) => {
                            Multiplicity::Many { witness }
                        }
                        (Multiplicity::Many { witness }, _)
                        | (_, Multiplicity::Many { witness }) => Multiplicity::Many { witness },
                        (Multiplicity::Linear { witness }, Multiplicity::Linear { witness: _ }) => {
                            Multiplicity::Linear { witness }
                        }
                        (Multiplicity::Unused, Multiplicity::Unused) => Multiplicity::Unused,
                        (prev_branch, new_branch) => Multiplicity::Affine {
                            // we can unwrap since we handled the (Unused, Unused) case already
                            witness: prev_branch.witness().or(new_branch.witness()).unwrap(),
                        },
                    };

                    new_multiplicities.insert(*var, merged_multiplicity);
                }
            }

            merged.multiplicities = new_multiplicities;

            for var in branch.declarations {
                merged.declarations.insert(var);
            }
        }

        self.pop_loop();
    }

    pub fn pop_loop(&mut self) {
        let scope = self.stack.pop().unwrap();
        let prev = self.stack.last_mut().unwrap();

        for (var, new_usage) in scope.multiplicities {
            let prev_usage = prev
                .multiplicities
                .entry(var)
                .or_insert(Multiplicity::Unused);

            let new = match (prev_usage.clone(), new_usage) {
                (Multiplicity::Unused, x) | (x, Multiplicity::Unused) => x,
                (Multiplicity::Many { witness }, _) => Multiplicity::Many { witness },
                (prev, new) => Multiplicity::Many {
                    witness: ManyWitness::UsedTwice {
                        first: prev.witness().unwrap(),
                        then: new.witness().unwrap(),
                    },
                },
            };

            *prev_usage = new;
        }
    }

    pub fn declare_variable(&mut self, var: V) {
        let scope = self.stack.last_mut().unwrap();
        scope.declarations.insert(var);
    }

    pub fn consume(&mut self, var: V, span: S) {
        let scope = self.stack.last_mut().unwrap();

        let entry = scope
            .multiplicities
            .entry(var)
            .or_insert(Multiplicity::Unused);

        if let Multiplicity::Linear { witness } = entry {
            *entry = Multiplicity::Many {
                witness: ManyWitness::UsedTwice {
                    first: witness.clone(),
                    then: span,
                },
            }
        } else if scope.is_loop_scope && !scope.declarations.contains(&var) {
            // here we only need to handle loops:
            //
            // let x = ...
            // while (cond) {
            //   consume(x);
            // }
            //
            // x gets a usage of Unrestricted, regardless of how many times it loops
            //
            // but we don't apply this rule to variables declared inside the loop:
            //
            // while (cond) {
            //   let x = ...;
            //   consume(x);
            // }
            *entry = Multiplicity::Many {
                witness: ManyWitness::UsedInLoop { span },
            }
        } else {
            *entry = Multiplicity::Linear { witness: span }
        };
    }
}

#[cfg(test)]
mod tests {
    use super::ResourceTracker;
    use crate::typechecking::linear::{ManyWitness, Multiplicity};

    #[test]
    fn merge_seq() {
        let mut tracker = ResourceTracker::new();

        tracker.consume(0, 0);
        tracker.consume(1, 1);

        tracker.consume(0, 2);

        let res = tracker.finish();

        assert_eq!(
            res[&0],
            Multiplicity::Many {
                witness: ManyWitness::UsedTwice { first: 0, then: 2 }
            }
        );
        assert_eq!(res[&1], Multiplicity::Linear { witness: 1 });
    }

    #[test]
    fn merge_loop() {
        let mut tracker = ResourceTracker::new();

        tracker.declare_variable(0);

        tracker.push_loop_scope();

        tracker.consume(0, 0);

        tracker.declare_variable(1);
        tracker.consume(1, 1);

        tracker.pop_loop();

        let res = tracker.finish();

        assert_eq!(
            res[&0],
            Multiplicity::Many {
                witness: ManyWitness::UsedInLoop { span: 0 }
            }
        );
        assert_eq!(res[&1], Multiplicity::Linear { witness: 1 });
    }

    #[test]
    fn merge_branches() {
        let mut tracker = ResourceTracker::new();

        // let x0 = ...
        // let x1 = ...
        // let x2 = ...
        tracker.declare_variable(0);
        tracker.declare_variable(1);
        tracker.declare_variable(2);

        // if (cond1) {
        //   consume(x0);
        //   consume(x0);
        //   consume(x2);
        // }
        tracker.push_branch();
        tracker.consume(0, 0);
        tracker.consume(0, 1);
        tracker.consume(2, 2);

        // else if (cond2) {
        //   consume(x1);
        //   consume(x2);
        // }
        tracker.push_branch();
        tracker.consume(1, 3);
        tracker.consume(2, 4);

        // else {
        //   consume(x2);
        //   let x3 = ...;
        //   consume(x3);
        // }
        tracker.push_branch();
        tracker.consume(2, 5);
        tracker.declare_variable(3);
        tracker.consume(3, 6);

        // merge the 3 branches to compute resulting multiplicity
        tracker.pop_branches(3);

        let res = tracker.finish();

        assert_eq!(
            res[&0],
            Multiplicity::Many {
                witness: crate::typechecking::linear::ManyWitness::UsedTwice { first: 0, then: 1 }
            }
        );
        assert_eq!(res[&1], Multiplicity::Affine { witness: 3 });
        assert_eq!(res[&2], Multiplicity::Linear { witness: 2 });
        assert_eq!(res[&3], Multiplicity::Linear { witness: 6 });
    }
}

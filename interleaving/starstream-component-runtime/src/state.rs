use starstream_interleaving_spec::{Hash, ProcessId, Ref, Value, WasmModule};
use std::collections::HashMap;

pub type ProgramHash = Hash<WasmModule>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProcessKind {
    Utxo,
    Coord,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcessDefinition {
    pub kind: ProcessKind,
    pub program_hash: ProgramHash,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcessSlot<Resource> {
    pub pid: ProcessId,
    pub definition: ProcessDefinition,
    pub exported_resource: Option<Resource>,
}

#[derive(Clone, Debug)]
pub struct ResourceTable<Resource> {
    by_resource: HashMap<Resource, ProcessId>,
}

impl<Resource> ResourceTable<Resource>
where
    Resource: Copy + Eq + std::hash::Hash,
{
    pub fn insert(&mut self, resource: Resource, pid: ProcessId) {
        self.by_resource.insert(resource, pid);
    }

    pub fn resolve(&self, resource: Resource) -> Option<ProcessId> {
        self.by_resource.get(&resource).copied()
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RefArena {
    next_ref: u64,
    store: HashMap<Ref, Vec<Value>>,
}

impl RefArena {
    pub fn alloc_words(&mut self, size_words: u32) -> Ref {
        let reff = Ref(self.next_ref);
        self.next_ref += 1;
        self.store
            .insert(reff, vec![Value(0); size_words as usize * 4usize]);
        reff
    }

    pub fn read_lanes(&self, reff: Ref, offset_words: u32) -> Option<[Value; 4]> {
        let vals = self.store.get(&reff)?;
        let base = offset_words as usize * 4usize;
        Some([
            vals.get(base).copied().unwrap_or(Value(0)),
            vals.get(base + 1).copied().unwrap_or(Value(0)),
            vals.get(base + 2).copied().unwrap_or(Value(0)),
            vals.get(base + 3).copied().unwrap_or(Value(0)),
        ])
    }

    pub fn write_lanes(&mut self, reff: Ref, offset_words: u32, lanes: [Value; 4]) -> bool {
        let Some(vals) = self.store.get_mut(&reff) else {
            return false;
        };
        let base = offset_words as usize * 4usize;
        for (idx, lane) in lanes.into_iter().enumerate() {
            if let Some(slot) = vals.get_mut(base + idx) {
                *slot = lane;
            }
        }
        true
    }
}

#[derive(Clone, Debug)]
pub struct StarstreamState<Resource> {
    pub processes: Vec<ProcessSlot<Resource>>,
    pub resources: ResourceTable<Resource>,
    pub refs: RefArena,
}

impl<Resource> Default for StarstreamState<Resource> {
    fn default() -> Self {
        Self {
            processes: Vec::new(),
            resources: ResourceTable {
                by_resource: HashMap::new(),
            },
            refs: RefArena::default(),
        }
    }
}

impl<Resource> StarstreamState<Resource>
where
    Resource: Copy + Eq + std::hash::Hash,
{
    pub fn push_process(
        &mut self,
        definition: ProcessDefinition,
        exported_resource: Option<Resource>,
    ) -> ProcessId {
        let pid = ProcessId(self.processes.len());
        if let Some(resource) = exported_resource {
            self.resources.insert(resource, pid);
        }
        self.processes.push(ProcessSlot {
            pid,
            definition,
            exported_resource,
        });
        pid
    }

    pub fn resolve_resource(&self, resource: Resource) -> Option<ProcessId> {
        self.resources.resolve(resource)
    }
}

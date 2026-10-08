//! Ledger wRPC bindings.

use bytes::Bytes;

use crate::runtime::{DataState, GlobalValue, ModuleState};
use crate::{DigestParseError, Transaction, TransactionEvent, TransactionInput, TransactionOutput};

wit_bindgen_wrpc::generate!();

use starstream::ledger::types;

impl From<DigestParseError> for types::GetError {
    fn from(err: DigestParseError) -> Self {
        Self::InvalidDigest(err.to_string())
    }
}

impl From<types::GlobalValue> for GlobalValue {
    fn from(v: types::GlobalValue) -> Self {
        match v {
            types::GlobalValue::I32(v) => Self::I32(v),
            types::GlobalValue::I64(v) => Self::I64(v),
            types::GlobalValue::F32(v) => Self::F32(v),
            types::GlobalValue::F64(v) => Self::F64(v),
            types::GlobalValue::V128((lo, hi)) => {
                let v = u128::from(hi) << 64 | u128::from(lo);
                Self::V128(v.to_le_bytes())
            }
        }
    }
}

impl From<GlobalValue> for types::GlobalValue {
    fn from(v: GlobalValue) -> Self {
        match v {
            GlobalValue::I32(v) => Self::I32(v),
            GlobalValue::I64(v) => Self::I64(v),
            GlobalValue::F32(v) => Self::F32(v),
            GlobalValue::F64(v) => Self::F64(v),
            GlobalValue::V128(v) => {
                let v = u128::from_le_bytes(v);
                Self::V128((v as u64, (v >> 64) as u64))
            }
        }
    }
}

impl From<types::DataState> for DataState {
    fn from(v: types::DataState) -> Self {
        Self {
            memory_index: v.memory_index,
            offset: v.offset,
            data: v.data.into(),
        }
    }
}

impl From<DataState> for types::DataState {
    fn from(v: DataState) -> Self {
        Self {
            memory_index: v.memory_index,
            offset: v.offset,
            data: v.data.into(),
        }
    }
}

impl From<types::ModuleState> for ModuleState {
    fn from(v: types::ModuleState) -> Self {
        Self {
            memories: v.memories,
            globals: v.globals.into_iter().map(Into::into).collect(),
            data: v.data.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<ModuleState> for types::ModuleState {
    fn from(v: ModuleState) -> Self {
        Self {
            memories: v.memories,
            globals: v.globals.into_iter().map(Into::into).collect(),
            data: v.data.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<types::TransactionEvent> for TransactionEvent {
    fn from(v: types::TransactionEvent) -> Self {
        Self {
            abi_name: v.abi_name.into(),
            name: v.name.into(),
            params: Box::from(&*v.params),
        }
    }
}

impl From<TransactionEvent> for types::TransactionEvent {
    fn from(v: TransactionEvent) -> Self {
        Self {
            abi_name: v.abi_name.into(),
            name: v.name.into(),
            params: Bytes::from(v.params),
        }
    }
}

impl From<types::TransactionInput> for TransactionInput {
    fn from(v: types::TransactionInput) -> Self {
        Self {
            transaction: v.transaction.into(),
            index: v.index,
        }
    }
}

impl From<TransactionInput> for types::TransactionInput {
    fn from(v: TransactionInput) -> Self {
        Self {
            transaction: v.transaction.into(),
            index: v.index,
        }
    }
}

impl From<types::TransactionOutput> for TransactionOutput {
    fn from(v: types::TransactionOutput) -> Self {
        Self {
            contract: v.contract.into(),
            instance: v.instance.into(),
            methods: v.methods.into_iter().collect(),
            storage: Box::from(&*v.storage),
            state: v.state.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<TransactionOutput> for types::TransactionOutput {
    fn from(v: TransactionOutput) -> Self {
        Self {
            contract: v.contract.into(),
            instance: v.instance.into(),
            methods: v.methods.into_iter().collect(),
            storage: Bytes::from(v.storage),
            state: v.state.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<types::Transaction> for Transaction {
    fn from(v: types::Transaction) -> Self {
        Self {
            inputs: v.inputs.into_iter().map(Into::into).collect(),
            outputs: v.outputs.into_iter().map(Into::into).collect(),
            events: v.events.into_iter().map(Into::into).collect(),
            proof: Box::from(&*v.proof),
        }
    }
}

impl From<Transaction> for types::Transaction {
    fn from(v: Transaction) -> Self {
        Self {
            inputs: v.inputs.into_iter().map(Into::into).collect(),
            outputs: v.outputs.into_iter().map(Into::into).collect(),
            events: v.events.into_iter().map(Into::into).collect(),
            proof: Bytes::from(v.proof),
        }
    }
}

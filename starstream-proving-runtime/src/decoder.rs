use starstream_interleaving_spec::events::EventKind;
use starstream_interleaving_spec::{BLOCK_SIZE, ResourceHandle, Trace};

/// One committed event block.
pub type AbsorbedBlock = [u64; BLOCK_SIZE];

/// Decode a self-describing tagged event stream.
///
/// Function attribution is checked by the Wasm relation. The caller supplies
/// the coordinator-visible handles for `SetStorage`, which are intentionally
/// absent from the committed event blocks.
pub fn decode_tagged_blocks(
    blocks: &[AbsorbedBlock],
    coordinator_handles: &mut impl Iterator<Item = ResourceHandle>,
) -> Result<Trace, BlockCodecError> {
    let mut steps = Vec::new();
    let mut block_index = 0;
    while block_index < blocks.len() {
        let tag = blocks[block_index][0];
        let Some(kind) = EventKind::from_tag(tag) else {
            return Err(BlockCodecError::UnknownTag {
                block: block_index,
                tag,
            });
        };
        let count = kind.blocks().len();
        let available = blocks.len() - block_index;
        if available < count {
            return Err(BlockCodecError::TruncatedTaggedEvent {
                block: block_index,
                tag,
                expected_blocks: count,
                available_blocks: available,
            });
        }
        let words = &blocks[block_index..block_index + count];
        let handle = (kind == EventKind::SetStorage)
            .then(|| coordinator_handles.next())
            .flatten();
        steps.push(kind.decode(words, None, handle)?);
        block_index += count;
    }
    Ok(Trace::new(steps))
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum BlockCodecError {
    #[error("block {block}: unknown tagged event {tag}")]
    UnknownTag { block: usize, tag: u64 },
    #[error(
        "block {block}: tagged event {tag} requires {expected_blocks} blocks, but only {available_blocks} remain"
    )]
    TruncatedTaggedEvent {
        block: usize,
        tag: u64,
        expected_blocks: usize,
        available_blocks: usize,
    },
    #[error("single event block decode error")]
    SingleEventError(#[from] starstream_interleaving_spec::events::EventCodecError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use starstream_interleaving_spec::{MethodHash, Out, StarstreamValue, Step, events};

    #[test]
    fn decodes_consecutive_events_and_rejects_truncation() {
        let enter = Step::EnterMethod {
            method: MethodHash([1, 2, 3, 4, 5, 6, 7, 8]),
            arguments: StarstreamValue([11, 12, 13, 14]),
        };
        let ret = Step::Return {
            result: Out(StarstreamValue::UNIT_VALUE),
        };
        let mut blocks = events::encode(&enter);
        assert!(matches!(
            decode_tagged_blocks(&blocks[..1], &mut std::iter::empty()),
            Err(BlockCodecError::TruncatedTaggedEvent { .. })
        ));
        blocks.extend(events::encode(&ret));
        assert_eq!(
            decode_tagged_blocks(&blocks, &mut std::iter::empty()).unwrap(),
            Trace::new([enter, ret])
        );
        blocks[0][0] = 999;
        assert!(matches!(
            decode_tagged_blocks(&blocks, &mut std::iter::empty()),
            Err(BlockCodecError::UnknownTag { .. })
        ));
    }

    #[test]
    fn storage_load_requires_host_handle() {
        let step = Step::SetStorage {
            storage: StarstreamValue::UNIT_VALUE,
            coordinator_handle: ResourceHandle(17),
        };
        let blocks = events::encode(&step);
        assert!(matches!(
            decode_tagged_blocks(&blocks, &mut std::iter::empty()),
            Err(BlockCodecError::SingleEventError(
                events::EventCodecError::MissingCoordinatorHandle
            ))
        ));
        assert_eq!(
            decode_tagged_blocks(&blocks, &mut [ResourceHandle(17)].into_iter()).unwrap(),
            Trace::new([step])
        );
    }
}

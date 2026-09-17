use starstream_interleaving_spec::{MethodHash, Step, events};

#[test]
fn method_identity_preserves_text_and_event_order() {
    // Distinct high/low halves catch pair reversal and whole-hash reversal.
    let hash = MethodHash([
        0x01234567, 0x89abcdef, 0xfedcba98, 0x76543210, 0x0badcafe, 0xdeadbeef, 0xffffffff,
        0x80000000,
    ]);
    assert_eq!(
        hash.to_hex(),
        "89abcdef0123456776543210fedcba98deadbeef0badcafe80000000ffffffff"
    );
    assert_eq!(
        events::encode(&Step::RegisterMethod { method: hash }),
        vec![
            [
                4, 0x01234567, 0x89abcdef, 0xfedcba98, 0x76543210, 0x0badcafe, 0xdeadbeef,
                0xffffffff
            ],
            [0x80000000, 0, 0, 0, 0, 0, 0, 0],
        ]
    );
    let json = serde_json::to_value(hash).unwrap();
    assert_eq!(json, serde_json::json!(hash.0));
    assert_eq!(serde_json::from_value::<MethodHash>(json).unwrap(), hash);
    assert!(serde_json::from_str::<MethodHash>("[1,2,3,4]").is_err());
}

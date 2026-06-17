abi Score {
    fn plus_chips(chips: u64);
    fn plus_mult(mult: u64);
    fn mult_mult(mult_pct: u64);
    fn finish();
    event Finish(total: u64);
}

utxo ScoreProgress {
    storage {
        let mut chips: u64;
        let mut mult: u64;
    }

    main fn new() {
        yield(Score);
        emit Finish(chips * mult);
    }

    impl Score {
        fn plus_chips(pub chips2: u64) {
            chips = chips + chips2;
        }
        fn plus_mult(pub mult2: u64) {
            mult = mult + mult2;
        }
        fn mult_mult(pub mult_pct: u64) {
            mult = mult * mult_pct / 100;
        }
        fn finish() {
            resume;
        }
    }
}

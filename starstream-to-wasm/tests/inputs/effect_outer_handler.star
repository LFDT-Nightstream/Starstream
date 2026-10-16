abi Effects {
    effect Hello();
    event Log(point: u32);
}

abi Trigger {
    fn trigger();
}

utxo TriggerUtxo {
    main fn new() {
        yield(Trigger);
    }

    impl Trigger {
        fn trigger() {
            raise Hello();
        }
    }
}

script fn main() {
    let trigger = TriggerUtxo::new();
    try {
        trigger.trigger();
    } with Hello() {
        emit Log(1);
    }
}

test "Effect search reaches outer instance handler" {
    main()
}

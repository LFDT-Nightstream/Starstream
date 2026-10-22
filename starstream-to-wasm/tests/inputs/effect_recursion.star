abi Effects {
    effect Hello();
    event Log(point: u32);
}

test {
    emit Log(1);
    try {
        try {
            emit Log(2);
            raise Hello();
            emit Log(6);
        } with Hello() {
            emit Log(3);
            raise Hello();
            emit Log(5);
        }
    } with Hello() {
        emit Log(4);
    }
    emit Log(7);
}

script fn main() -> i64 {
    let hex: i64 = 0xFF;
    let octal: u8 = 0o17;
    let binary: u16 = 0b1010;
    let masked = 0x10 + 0b1 + 0o7 + 9;
    hex + masked
}

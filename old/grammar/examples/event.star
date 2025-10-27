abi Example {
  event MyEvent(u32);
}

script {
  fn main() {
    Example::MyEvent(17);
  }
}

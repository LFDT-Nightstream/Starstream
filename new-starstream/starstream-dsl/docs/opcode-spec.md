This document is a scratch pad and should be ignored for now

```rust
match (context, stack, tape) {
    (c, [...stack, a, b], [Add(a, b), ...tape]) => (c, [...stack, a + b], tape),
    // (c, [Sub(a, b), ...rest]) => (c, [a - b, ...rest]),
    (c, [...stack, value], [Declare(name), ...tape]) => (c.declare(name, value), stack, tape),
    (c, [...stack, value], [Store(name), ...tape]) => (c.store(name, value), stack, tape),
    (c, stack, [Load(name), ...tape]) => (c, [...stack, c.load(name)], tape),
}
```

| Op      | Read    | Write         | Desc    |
| ------- | ------- | ------------- | ------- |
| `add`     | Stack 2 | Stack 1       |  |
| `let`   | Stack 1 | Current scope |         |
| `load`  | Scopes  | Stack 1       |         |
| `store` | Stack 1 | Scopes        |         |

module utils {
  type Option[t] = Some(t) | None
  pure def last(items: List[a]): a = items[items.length() - 1]

  pure def pop_last(items: List[a]): List[a] =
    items.slice(0, items.length() - 1)

  pure def to_set(items: List[a]): Set[a] =
    items.foldl(Set(), (result, elem) => result.union(Set(elem)))

  pure def is_some(opt: Option[a]): bool =
    match opt {
      | Some(_) => true
      | None => false
    }

  pure def insert(self: Set[a], a: a): Set[a] =
    self.union(Set(a))

  pure def is_empty_set(self: Set[a]): bool =
    self.size() == 0
}

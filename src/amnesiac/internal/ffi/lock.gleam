import amnesiac/internal/opaque_types.{type Key, type Table}
import gleam/erlang/node.{type Node}

pub type LockType {
  Read
  Write
  StickyWrite
}

pub type LockItem(a) {
  Record(Table, Key)
  Table(Table)
  Global(a, List(Node))
}

@external(erlang, "lock_ffi", "lock")
pub fn lock(item: LockItem(a), with lock_type: LockType) -> Nil

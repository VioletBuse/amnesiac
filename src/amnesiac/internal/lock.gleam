import amnesiac/internal/table.{type Table}
import gleam/erlang/node.{type Node}

pub type LockType {
  Read
  Write
  StickyWrite
}

pub type LockItem(a) {
  Record(Table, a)
  Table(Table)
  Global(a, List(Node))
}

@external(erlang, "lock_ffi", "lock")
pub fn lock(item: LockItem(a), with lock_type: LockType) -> Nil

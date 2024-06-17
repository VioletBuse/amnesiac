import amnesiac/internal/opaque_types.{type Key, type Record, type Table}

pub type WriteLock {
  Write
  StickyWrite
}

@external(erlang, "write_ffi", "write")
pub fn write(table: Table, record value: Record, with lock: WriteLock) -> Nil

@external(erlang, "write_ffi", "delete")
pub fn delete(table: Table, key key: Key, with lock: WriteLock) -> Nil

@external(erlang, "write_ffi", "delete_object")
pub fn delete_object(
  table: Table,
  record value: Record,
  with lock: WriteLock,
) -> Nil

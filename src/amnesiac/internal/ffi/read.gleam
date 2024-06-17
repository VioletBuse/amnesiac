import amnesiac/internal/ffi/lock.{type LockType}
import amnesiac/internal/opaque_types.{type Key, type Record, type Table}
import gleam/option.{type Option}

@external(erlang, "read_ffi", "all_keys")
pub fn all_keys(table: Table) -> List(Key)

@external(erlang, "read_ffi", "first_key")
pub fn first_key(table: Table) -> Option(Key)

@external(erlang, "read_ffi", "last_key")
pub fn last_key(table: Table) -> Option(Key)

@external(erlang, "read_ffi", "next_key")
pub fn next_key(table: Table, cursor key: Key) -> Option(Key)

@external(erlang, "read_ffi", "prev_key")
pub fn prev_key(table: Table, cursor key: Key) -> Option(Key)

@external(erlang, "read_ffi", "read")
pub fn read(table: Table, key: Key, with lock: LockType) -> List(Record)

@external(erlang, "read_ffi", "match_object")
pub fn match_object(
  table: Table,
  match pattern: pattern,
  with lock: LockType,
) -> List(Record)

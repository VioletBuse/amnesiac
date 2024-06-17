import amnesiac/internal/opaque_types.{
  type ActivityId, type CheckpointName, type Key, type Record, type Table,
}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/node.{type Node}
import gleam/erlang/process.{type Selector}

@external(erlang, "subscribe_ffi", "subscribe_system")
pub fn subscribe_system() -> Result(Node, Dynamic)

@external(erlang, "subscribe_ffi", "subscribe_activity")
pub fn subscribe_activity() -> Result(Node, Dynamic)

@external(erlang, "subscribe_ffi", "subscribe_table_simple")
pub fn subscribe_table_simple(table: Table) -> Result(Node, Dynamic)

@external(erlang, "subscribe_ffi", "subscribe_table_detailed")
pub fn subscribe_table_detailed(table: Table) -> Result(Node, Dynamic)

// mnesia_system_event
pub type SystemEvent {
  MnesiaUp(Node)
  MnesiaDown(Node)
  MnesiaCheckpointActivated(CheckpointName)
  MnesiaCheckpointDeactivated(CheckpointName)
  MnesiaOverload(Dynamic)
  InconsistentDatabase(Dynamic, Node)
  MnesiaFatal(Dynamic, List(Dynamic))
  MnesiaInfo(Dynamic, List(Dynamic))
  MnesiaError(Dynamic, List(Dynamic))
  MnesiaUser(Dynamic)
}

// mnesia_activity_event
pub type ActivityEvent {
  Complete(id: ActivityId)
}

// mnesia_table_event
pub type SimpleTableEvent {
  SimpleWrite(Record, ActivityId)
  SimpleDeleteObject(Record, ActivityId)
  SimpleDelete(#(Table, Dynamic), ActivityId)
}

// mnesia_table_event
pub type DetailedTableEvent {
  DetailedWrite(Table, Record, List(Record), ActivityId)
  DetailedDeleteRecord(Table, Record, List(Record), ActivityId)
  DetailedDeleteWithKey(Table, #(Table, Key), List(Record), ActivityId)
}

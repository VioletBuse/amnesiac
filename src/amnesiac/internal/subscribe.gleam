import amnesiac/internal/table.{type Table}
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
}

// mnesia_activity_event
pub type ActivityEvent

// mnesia_table_event
pub type TableEvent

pub fn selecting_system(
  selector: Selector(a),
  mapping transform: fn(SystemEvent) -> a,
) -> Selector(a) {
  process.selecting_record2(
    selector,
    atom.create_from_string("mnesia_system_event"),
  )
}

import amnesiac/internal/opaque_types.{type Attribute, type Record, type Table}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node.{type Node}

@external(erlang, "table_ffi", "table_name")
pub fn table_name(name: Atom) -> Table

pub fn name_from_string(name: String) -> Table {
  atom.create_from_string(name)
  |> table_name
}

pub type AccessMode {
  ReadWrite
  ReadOnly
}

pub type TableType {
  Set
  OrderedSet
  Bag
}

pub type CreateOption {
  AccessMode(AccessMode)
  Attributes(List(Atom))
  DiscCopies(List(Node))
  DiscOnlyCopies(List(Node))
  RamCopies(List(Node))
  Index(List(Atom))
  LoadOrder(Int)
  Majority(Bool)
  RecordName(Atom)
  Type(TableType)
  LocalContent(Bool)
}

@external(erlang, "table_ffi", "create_table")
pub fn create_table(
  name: Table,
  create_option: List(CreateOption),
) -> Result(Nil, Dynamic)

@external(erlang, "table_ffi", "del_table_copy")
pub fn del_table_copy(table: Table, node: Node) -> Result(Nil, Dynamic)

@external(erlang, "table_ffi", "del_table_index")
pub fn del_table_index(
  table: Table,
  index attribute: Attribute,
) -> Result(Nil, Dynamic)

@external(erlang, "table_ffi", "delete_table")
pub fn delete_table(table: Table) -> Result(Nil, Dynamic)

@external(erlang, "table_ffi", "move_table_copy")
pub fn move_table_copy(
  table: Table,
  from node_1: Node,
  to node_2: Node,
) -> Result(Nil, Dynamic)

@external(erlang, "table_ffi", "transform_table")
pub fn transform_table(
  table: Table,
  transformer fxn: fn(Record) -> Record,
  new_attributes attrs: List(Attribute),
  name record_name: Atom,
) -> Result(Nil, Dynamic)

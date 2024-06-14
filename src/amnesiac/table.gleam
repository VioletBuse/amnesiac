import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node.{type Node}

pub type Table

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
  index attribute: Atom,
) -> Result(Nil, Dynamic)

@external(erlang, "table_ffi", "delete_table")
pub fn delete_table(table: Table) -> Result(Nil, Dynamic)

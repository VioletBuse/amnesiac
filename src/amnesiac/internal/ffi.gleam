import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node.{type Node}

@external(erlang, "amnesiac_ffi", "abort")
pub fn abort(reason: a) -> Nil

pub type Table

pub type CheckpointArgs(a) {
  Name(a)
  Max(List(Table))
  Min(List(Table))
  AllowRemote(Bool)
  RamOverwritesDump(Bool)
}

@external(erlang, "amnesiac_ffi", "activate_checkpoint")
pub fn activate_checkpoint(
  args: List(CheckpointArgs(a)),
) -> Result(#(a, List(Node)), Dynamic)

pub type ActivityContext

@external(erlang, "amnesiac_ffi", "activity")
pub fn activity(
  context context: ActivityContext,
  function fun: fn() -> a,
) -> Result(a, Dynamic)

pub type StorageType {
  DiscCopyStorage
  DiscOnlyCopyStorage
  RamCopyStorage
}

@external(erlang, "amnesiac_ffi", "add_table_copy")
pub fn add_table_copy(
  table to_copy: Table,
  remote node: Node,
  storage st: StorageType,
) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "add_table_index")
pub fn add_table_index(
  table table: Table,
  attribute attribute: Atom,
) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "all_keys")
pub fn all_keys(table table: Table) -> List(a)

@external(erlang, "amnesiac_ffi", "async_dirty")
pub fn async_dirty(fun fun: fn() -> a) -> a

@external(erlang, "amnesiac_ffi", "backup")
pub fn backup(destination: a) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "backup_checkpoint")
pub fn backup_checkpoint(name: a, destination: b) -> Result(Nil, Dynamic)

pub type ConfigKey {
  ExtraDbNodes
  DcDumpLimit
}

pub type ConfigValue {
  NodeList(List(Node))
  Float(Float)
  Integer(Int)
}

@external(erlang, "amnesiac_ffi", "change_config")
pub fn change_config(
  key: ConfigKey,
  value: ConfigValue,
) -> Result(ConfigValue, Dynamic)

pub type TableAccessMode {
  ReadOnly
  ReadWrite
}

@external(erlang, "amnesiac_ffi", "change_table_access_mode")
pub fn change_table_access_mode(
  table: Table,
  mode: TableAccessMode,
) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "change_table_copy_type")
pub fn change_table_copy_type(
  table: Table,
  node: Node,
  storage_type: StorageType,
) -> Result(Nil, Dynamic)

pub type NodePool {
  NodePool(List(Node))
}

pub type FragProp {
  Activate(NodePool)
  Deactivate
  AddFrag(List(Node))
  DelFrag
  AddNode(Node)
  DelNode(Node)
}

@external(erlang, "amnesiac_ffi", "change_table_frag")
pub fn change_table_frag(
  table: Table,
  frag_prop: FragProp,
) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "change_table_load_order")
pub fn change_table_load_order(table: Table, order: Int) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "change_table_majority")
pub fn change_table_majority(
  table: Table,
  majority: Bool,
) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "clear_table")
pub fn clear_table(table: Table) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "create_schema")
pub fn create_schema(nodes: List(Node)) -> Result(Nil, Dynamic)

pub type CreateType {
  Set
  OrderedSet
  Bag
}

pub type CreateOption {
  AccessMode(TableAccessMode)
  Attributes(List(Atom))
  DiscCopies(List(Node))
  DiscOnlyCopies(List(Node))
  Index(List(Atom))
  LoadOrder(Int)
  Majority(Bool)
  RamCopies(List(Node))
  RecordName(Atom)
  Type(CreateType)
  LocalContent(Bool)
}

@external(erlang, "amnesiac_ffi", "create_table")
pub fn create_table(
  name: Table,
  options: List(CreateOption),
) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "deactivate_checkpoint")
pub fn deactivate_checkpoint(name: a) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "del_table_copy")
pub fn del_table_copy(table: Table, node: Node) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "del_table_index")
pub fn del_table_index(table: Table, index: Atom) -> Result(Nil, Dynamic)

pub type LockKind {
  Write
  StickyWrite
}

@external(erlang, "amnesiac_ffi", "delete")
pub fn delete(table: Table, key: a, with lock_kind: LockKind) -> Nil

@external(erlang, "amnesiac_ffi", "delete_object")
pub fn delete_object(table: Table, record: a, with lock_kind: LockKind) -> Nil

@external(erlang, "amnesiac_ffi", "delete_schema")
pub fn delete_schema(nodes: List(Node)) -> Result(Nil, Dynamic)

@external(erlang, "amnesiac_ffi", "delete_table")
pub fn delete_table(table: Table) -> Result(Nil, Dynamic)

import gleam/dynamic.{type Dynamic}
import gleam/erlang/node.{type Node}

@external(erlang, "schema_ffi", "create_schema")
pub fn create_schema(on nodes: List(Node)) -> Result(Nil, Dynamic)

@external(erlang, "schema_ffi", "delete_schema")
pub fn delete_schema(on nodes: List(Node)) -> Result(Nil, Dynamic)

@external(erlang, "schema_ffi", "print_schema")
pub fn print_schema() -> Nil

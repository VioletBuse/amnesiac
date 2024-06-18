import amnesiac/internal/ffi/main
import amnesiac/internal/ffi/schema
import amnesiac/internal/ffi/table
import gleam/erlang/node.{type Node}

pub fn start() {
  main.start()
}

pub fn stop() {
  main.stop()
}

pub fn create_schema(nodes: List(Node)) {
  schema.create_schema(nodes)
}

pub fn delete_schema(nodes: List(Node)) {
  schema.delete_schema(nodes)
}

import gleam/dynamic
import gleam/erlang/node

pub type LockType {
  Read
  Write
  StickyWrite
}

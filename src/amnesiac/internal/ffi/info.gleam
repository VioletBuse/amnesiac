import amnesiac/internal/opaque_types.{type Table}

@external(erlang, "info_ffi", "get_tables")
pub fn get_tables() -> List(Table)

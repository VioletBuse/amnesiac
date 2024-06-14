import gleam/dynamic.{type Dynamic}

@external(erlang, "main_ffi", "start")
pub fn start() -> Result(Nil, Dynamic)

@external(erlang, "main_ffi", "stop")
pub fn stop() -> Result(Nil, Dynamic)

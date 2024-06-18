-module(info_ffi).

-export([get_tables/0]).

get_tables() ->
    mnesia:system_info(tables).

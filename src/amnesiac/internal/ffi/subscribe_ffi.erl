-module(subscribe_ffi).

-export([subscribe_system/0, subscribe_activity/0,
    subscribe_table_simple/1, subscribe_table_detailed/1]).

subscribe_system() ->
    mnesia:subscribe(system).

subscribe_activity() ->
    mnesia:subscribe(activity).

subscribe_table_simple(Table) ->
    mnesia:subscribe({table, Table, simple}).

subscribe_table_detailed(Table) ->
    mnesia:subscribe({table, Table, detailed}).

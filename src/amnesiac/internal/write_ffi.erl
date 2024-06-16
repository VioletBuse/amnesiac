-module(write_ffi).

-export([write/3, delete/3, delete_object/3]).

write(Table, Record, Lock) ->
    mnesia:write(Table, Record, Lock),
    nil.

delete(Table, Key, Lock) ->
    mnesia:delete(Table, Key, Lock),
    nil.

delete_object(Table, Record, Lock) ->
    mnesia:delete_object(Table, Record, Lock),
    nil.

-module(read_ffi).

-export([all_keys/1, first_key/1, last_key/1, next_key/2, prev_key/2,
    read/3, match_object/3]).

all_keys(Table) ->
    mnesia:keys(Table)

first_key(Table) ->
    case mnesia:first(Table) of
        '$end_of_table' -> none;
        Key -> {some, Key}
    end.

last_key(Table) ->
    case mnesia:last(Table) of
        '$end_of_table' -> none;
        Key -> {some, Key}
    end.

next_key(Table, Cursor) ->
    case mnesia:next(Table, Cursor) of
        '$end_of_table' -> none;
        Key -> {some, Key}
    end.

prev_key(Table, Cursor) ->
    case mnesia:prev(Table, Cursor) of
        '$end_of_table' -> none;
        Key -> {some, Key}
    end.

read(Table, Key, Lock) ->
    mnesia:read(Table, Key, Lock).

match_object(Table, Pattern, Lock) ->
    mnesia:match_object(Table, Pattern, Lock).

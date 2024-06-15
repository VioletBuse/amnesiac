-module(table_ffi)

-export([table_name/1, create_table/2, del_table_copy/2, del_table_index/2,
    delete_table/1, move_table_copy/3, transform_table/4]).

table_name(Table) ->
    Table.

create_table(Table, Options) ->
    case mnesia:create_table(Table, Options) of
        {atomic, ok} -> {ok, nil};
        {aborted, Reason} -> {error, Reason}
    end.

del_table_copy(Table, Node) ->
    case mnesia:del_table_copy(Table, Node) of
        {atomic, ok} -> {ok, nil};
        {aborted, Reason} -> {error, Reason}
    end.

del_table_index(Table, Index) ->
    case mnesia:del_table_index(Table, Index) of
        {atomic, ok} -> {ok, nil};
        {aborted, Reason} -> {error, Reason}
    end.

delete_table(Table) ->
    case mnesia:delete_table(Table) of
        {atomic, ok} -> {ok, nil};
        {aborted, Reason} -> {error, Reason}
    end.

move_table_copy(Table, From, To) ->
    case mnesia:move_table_copy(Table, From, To) of
        {atomic, ok} -> {ok, nil};
        {aborted, Reason} -> {error, Reason}
    end.

transform_table(Table, Fxn, NewAttributes, NewRecordName) ->
    case mnesia:transform_table(Table, Fxn, NewAttributes, NewRecordName) of
        {atomic, ok} -> {ok, nil};
        {aborted, Reason} -> {error, Reason}
    end.

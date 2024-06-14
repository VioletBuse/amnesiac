-module(amnesiac_ffi).

-export([abort/1, activate_checkpoint/1, activity/2, add_table_copy/3, add_table_index/2,
    all_keys/1, async_dirty/1, backup/1, backup_checkpoint/2, change_config/2,
    change_table_access_mode/2, change_table_copy_type/3, change_table_frag/2,
    change_table_load_order/2, change_table_majority/2, clear_table/1,
    create_schema/1, create_table/2]).

%% utility functions

storage_type_conversion(StorageType) ->
    case StorageType of
        disc_copy_storage -> disc_copies;
        disc_only_copy_storage -> disc_only_copies;
        ram_copy_storage -> ram_copies
    end.

t_result_to_gleam_result(Result) ->
    case Result of
        {atomic, Atomic} -> {ok, Atomic};
        {aborted, Aborted} -> {error, Aborted}
    end.

%% mnesia ports

abort(Reason) ->
    mnesia:abort(Reason).

activate_checkpoint(Args) ->
    case mnesia:activate_checkpoint(Args) of
        {ok, Name, Nodes} ->
            {ok, {Name, Nodes}};
        {error, Reason} ->
            {error, Reason}
    end.

activity(Ctx, Fun) ->
    case mnesia:activity(Ctx, Fun) of
        {atomic, Res} -> {ok, Res};
        {aborted, Reason} -> {error, Reason};
        Res -> {ok, Res}
    end.

add_table_copy(Table, Node, StorageType) ->
    ST = storage_type_conversion(StorageType)
    case mnesia:add_table_copy(Table, Node, ST) of
        {atomic, ok} -> {ok, nil};
        {aborted, Reason} -> {error, Reason}
    end.

add_table_index(Table, IndexAttr) ->
    case mnesia:add_table_index(Table, IndexAttr) of
        {atomic, ok} -> {ok, nil};
        {aborted, Reason} -> {error, Reason}
    end.

all_keys(Table) ->
    mnesia:all_keys(Table).

async_dirty(Fun) ->
    mnesia:async_dirty(Fun).

backup(Dest) ->
    case mnesia:backup(Dest) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

backup_checkpoint(Name, Dest) ->
    case mnesia:backup_checkpoint(Name, Dest) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

change_config(Key, Value) ->
    Val = case Value of
        {node_list, List} -> List;
        {integer, IntVal} -> IntVal;
        {float, FloatVal} -> FloatVal
    end.
    case mnesia:change_config(Key, Val) of
        {ok, ResValue} when is_list(ResValue) -> {ok, {node_list, ResValue}};
        {ok, ResValue} when is_integer(ResValue) -> {ok, {integer, ResValue}};
        {ok, ResValue} -> {ok, {float, ResValue}};
        {error, Reason} -> {error, Reason}
    end.

change_table_access_mode(Table, Mode) ->
    case mnesia:change_table_access_mode(Table, Mode) of
        {atomic, ok} -> {ok, nil};
        {abort, Reason} -> {error, Reason}
    end.

change_table_copy_type(Table, Node, Type) ->
    CT = storage_type_conversion(Type)
    case mnesia:change_table_copy_type(Table, Node, CT) of
        {atomic, ok} -> {ok, nil};
        {abort, Reason} -> {error, Reason}
    end.

change_table_frag(Table, FragProp) ->
    case mnesia:change_table_frag(Table, FragProp) of
        {atomic, ok} -> {ok, nil};
        {abort, Reason} -> {error, Reason}
    end.

change_table_load_order(Table, Order) ->
    case mnesia:change_table_load_order(Table, Order) of
        {atomic, ok} -> {ok, nil};
        {abort, Reason} -> {error, Reason}
    end.

change_table_majority(Table, M) ->
    case mnesia:change_table_majority(Table, M) of
        {atomic, ok} -> {ok, nil};
        {abort, Reason} -> {error, Reason}
    end.

clear_table(Table) ->
    case mnesia:clear_table(Table) of
        {atomic, ok} -> {ok, nil};
        {abort, Reason} -> {error, Reason}
    end.

create_schema(Nodes) ->
    case mnesia:create_schema(Nodes) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

create_table(Name, Opts) ->
    case mnesia:create_table(Name, Opts) of
        {atomic, ok} -> {ok, nil};
        {abort, Reason} -> {error, Reason}
    end.

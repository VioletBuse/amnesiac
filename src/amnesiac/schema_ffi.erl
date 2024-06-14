-module(schema_ffi).

-export([create_schema/1, delete_schema/1, print_schema/0]).

create_schema(Nodes) ->
    case mnesia:create_schema(Nodes) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

delete_schema(Nodes) ->
    case mnesia:delete_schema(Nodes) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

print_schema() ->
    mnesia:schema().

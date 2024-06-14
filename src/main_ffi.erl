-module(main_ffi).

-export([start/0, stop/0]).

start() ->
    case mnesia:start() of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

stop() ->
    case mnesia:stop() of
        stopped -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

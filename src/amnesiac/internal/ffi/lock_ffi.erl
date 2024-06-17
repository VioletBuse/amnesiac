-module(lock_ffi).

-export([lock/2]).

lock(Item, Kind) ->
    mnesia:lock(Item, Kind),
    nil.

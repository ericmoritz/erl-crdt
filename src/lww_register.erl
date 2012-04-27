-module(lww_register).
-export([new/1, new/2, merge/2, value/1]).
-import(crdt_misc, [sorted_id/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(lwwregister, {value, ts}).

new(Value) ->
    new(Value, sorted_id()).

new(Value, TS) ->
    #lwwregister{value=Value, ts=TS}.

value(#lwwregister{value=Value}) ->
    Value.

merge(V1=#lwwregister{ts=TS1}, #lwwregister{ts=TS2}) when TS1 > TS2 ->
    V1;
merge(_, V2) ->
    V2.



-ifdef(TEST).

value_test() ->
    R = new("hello"),
    ?assertEqual("hello", value(R)).

merge1_test() ->
    R1 = new("hello"),
    timer:sleep(1),
    R2 = new("goodbye"),

    R3 = merge(R1, R2),
    
    ?assertEqual("goodbye", value(R3)).

merge2_test() ->
    R2 = new("goodbye", 2),
    R1 = new("hello", 1),

    R3 = merge(R1, R2),

    ?assertEqual("goodbye", value(R3)).

-endif.

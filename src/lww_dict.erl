-module(lww_dict).
-export([new/0, store/3, erase/2, find/2, is_key/2, merge/2, to_list/1, from_list/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new() ->
    dict:new().

store(Key, Value, Dict) ->
    dict:store(Key, lww_register:new(Value), Dict).

erase(Key, Dict) ->
    dict:store(Key, lww_register:new(undefined), Dict).

find(Key, Dict) ->
    case dict:find(Key, Dict) of
        error ->
            error;
        {ok, Reg} ->
            case lww_register:value(Reg) of
                undefined ->
                    error;
                Value ->
                    {ok, Value}
            end
    end.

is_key(Key, Dict) ->
    find(Key, Dict) =/= error.

merge(D1, D2) ->
    dict:merge(fun(_Key, V1, V2) ->
                       lww_register:merge(V1, V2) end,
               D1, D2).
to_list(D) ->
    [{Key, lww_register:value(R)} || {Key, R} <- dict:to_list(D)].

from_list(L) ->
    lists:foldl(fun({K,V}, Acc) ->
                  store(K, V, Acc) end,
          new(),
          L).

-ifdef(TEST).

store_test() ->
    D = store(foo, "Foo", new()),
    ?assertEqual({ok, "Foo"}, find(foo, D)).

erase_test() ->    
    D = erase(foo, store(foo, "Foo", new())),

    ?assertEqual(error, find(foo, D)).

merge_test() ->
    D = new(),
    D1 = store(foo, "Foo", D),
    D2 = store(foo, "foo", D),
    
    D3 = merge(D1, D2),
    
    ?assertEqual({ok, "foo"}, find(foo, D3)).

merge_erase_test() ->
    D = new(),
    D1 = store(foo, "Foo", D),
    D2 = erase(foo, D),
    
    D3 = merge(D1, D2),
    
    ?assertEqual(error, find(foo, D3)).
-endif.

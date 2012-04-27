-module(zset).
-export([new/0, add_element/3, del_element/2, is_element/2, merge/2,
        from_list/1, to_list/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(zset, {scores,set}).

new() ->
    #zset{scores=lww_dict:new(), set=or_set:new(orddict)}.

add_element(Element, Score, ZSet=#zset{scores=Scores, set=Set0}) ->
    ScoreKey = {Score, Element},

    % Update the ScoreKey stored in the ordset
    Set1 = case lww_dict:find(Element, Scores) of
               {ok, OldScoreKey} ->
                   or_set:del_element(OldScoreKey, Set0);
               error ->
                   Set0
           end,

    ZSet#zset{scores=lww_dict:store(Element, ScoreKey, Scores),
              set=or_set:add_element(ScoreKey, Set1)}.

del_element(Element, ZSet=#zset{scores=Scores, set=Set}) ->
    case lww_dict:find(Element, Scores) of
        {ok, ScoreKey} ->
            ZSet#zset{scores=lww_dict:erase(Element, Scores),
                      set=or_set:del_element(ScoreKey, Set)};
        error ->
            ZSet
    end.

is_element(Element, #zset{scores=Scores}) ->
    lww_dict:is_key(Element, Scores).

to_list(#zset{set=Set}) ->
    [Element || {_Score, Element} <- or_set:to_list(Set)].

from_list(L) ->
    lists:foldl(fun({Element, Score}, Acc) ->
                  add_element(Element, Score, Acc) end,
          new(), L).
                  
merge(#zset{scores=Scores1}, #zset{scores=Scores2}) ->
    Scores3 = lww_dict:merge(Scores1, Scores2),
    Set = or_set:from_list([Value || {_, Value} <- lww_dict:to_list(Scores3)]),
    #zset{scores=Scores3, set=Set}.

-ifdef(TEST).

add_element_test() ->
    ZS1 = add_element(eric, 0, new()),

    ?assert(is_element(eric, ZS1)).

del_element_test() ->
    ZS1 = del_element(eric, add_element(eric, 0, new())),

    ?assert(not is_element(eric, ZS1)).

to_list_test() ->
    ZS1 = add_element("b", 0,
                      add_element("a", 1, new())),

    io:format("~w", [ZS1]),

    ?assertEqual(["b", "a"], to_list(ZS1)).

from_list_test() ->
    ZS = from_list([{"a", 1}, {"b", 0}]),
    ?assertEqual(["b", "a"], to_list(ZS)).

merge_test() ->    
    ZS = new(),
    
    ZS1 = add_element(a, 1, ZS),
    ZS2 = add_element(a, 0,
                      add_element(b, 0, ZS)),

    io:format("~w", [ZS1]),
    ZS3 = merge(ZS1, ZS2),


    ?assertEqual([a, b], to_list(ZS3)).

-endif.


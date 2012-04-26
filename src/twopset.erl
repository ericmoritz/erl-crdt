-module(twopset).
-export([new/0, add_element/2, del_element/2, is_element/2, merge/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new() ->
    {sets:new(), sets:new()}.

merge({A1, R1}, {A2, R2}) ->
    A3 = sets:union(A1, A2),
    R3 = sets:union(R1, R2),
    {A3, R3}.

is_element(Element, {ASet, RSet}) ->
    sets:is_element(Element, ASet) and not sets:is_element(Element, RSet).

add_element(Element, {ASet, RSet}) ->
    {sets:add_element(Element, ASet), RSet}.

del_element(Element, {ASet, RSet}) ->
    case is_element(Element, {ASet, RSet}) of
        true ->
            {ASet, sets:add_element(Element, RSet)};
        false ->
            {ASet, RSet}
    end.

from_list(L) ->
    lists:foldl(fun add_element/2, new(), L).

-ifdef(TEST).

merge_test() ->
    S = from_list([eric, glenn, shawn]),
    
    % diverge from S
    S1 = del_element(glenn, S),
    S2 = del_element(shawn, add_element(shawn, S)),

    S3 = merge(S1, S2),

    ?assert(is_element(eric, S3)),
    ?assert(not is_element(shawn, S3)),
    ?assert(not is_element(glenn, S3)),
    
    % removed items from a twopset can not be added back
    S4 = add_element(shawn, S3),
    ?assert(not is_element(shawn, S4)).

-endif.

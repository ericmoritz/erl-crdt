-module(orset).
-export([new/0, add_element/2, del_element/2, is_element/2, merge/2,
        from_list/1, to_list/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

sorted_id() ->
    {now(), node(), make_ref()}. % can refs() be serialized?

get_element_set(Element, Dict) ->
    case dict:find(Element, Dict) of
        {ok, Value} ->
            Value;
        error ->
            sets:new()
    end.

new() ->
    {dict:new(), dict:new()}.

add_element(Element, {A, R}) ->
    Id = sorted_id(),
    ESet = sets:add_element(Id, get_element_set(Element, A)),
    {dict:store(Element, ESet, A), R}.

del_element(Element, {ADict, RDict}) ->
    case is_element(Element, {ADict, RDict}) of
        true->
            ESet = sets:union(get_element_set(Element, ADict),
                              get_element_set(Element, RDict)),

            {dict:erase(Element, ADict),
             dict:store(Element, ESet, RDict)};
        false ->
            {ADict, RDict}
    end.

merge({A1, R1},{A2, R2}) ->
    MergeEDict = fun(D1, D2) ->
                         dict:merge(fun(_K, V1, V2) -> 
                                            sets:union(V1, V2) end,
                                    D1, D2) end,
    {MergeEDict(A1, A2), MergeEDict(R1, R2)}.

is_element(Element, {A, R}) ->
    AESet = get_element_set(Element, A),
    RESet = get_element_set(Element, R),
    Items = sets:subtract(AESet, RESet),

    (sets:size(Items) > 0).
    
from_list(L) ->
    lists:foldl(fun add_element/2, new(), L).

to_list({A, R}) ->
    [Key || Key <- dict:fetch_keys(A), is_element(Key, {A, R})].

-ifdef(TEST).

merge_test() ->
    S = from_list([eric, glenn, shawn]),
    
    % diverge from S
    S1 = del_element(glenn, S),
    S2 = del_element(shawn, add_element(shawn, S)),

    S3 = merge(S1, S2),

    ?assert(is_element(eric, S3)),
    ?assert(not is_element(shawn, S3)),
    ?assert(not is_element(glenn, S3)).

to_list_test() ->
    S = from_list([eric, glenn, shawn]),
    
    % diverge from S
    S1 = del_element(glenn, S),
    S2 = del_element(shawn, add_element(shawn, S)),

    S3 = merge(S1, S2),
    
    ?assert([eric] =:= to_list(S3)).

-endif.

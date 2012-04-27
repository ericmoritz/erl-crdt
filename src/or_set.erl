-module(or_set).
-export([new/0, new/1, add_element/2, del_element/2, is_element/2, merge/2,
        from_list/2, from_list/1, to_list/1]).
-import(crdt_misc, [sorted_id/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(orset, {dictmod, a, r}).


get_element_set(Element, Dict, #orset{dictmod=DictMod}) ->
    case DictMod:find(Element, Dict) of
        {ok, Value} ->
            Value;
        error ->
            sets:new()
    end.

new() ->
    new(ordered).

new(ordered) ->
    new(orddict);
new(unordered) ->
    new(dict);
new(DictModule) ->
    #orset{dictmod=DictModule, a=DictModule:new(), r=DictModule:new()}.

add_element(Element, ORSet=#orset{dictmod=DictMod, a=A, r=R}) ->
    Id = sorted_id(),
    ESet = sets:add_element(Id, get_element_set(Element, A, ORSet)),

    ORSet#orset{a=DictMod:store(Element, ESet, A), r=R}.

del_element(Element, ORSet=#orset{dictmod=DictMod, a=A, r=R}) ->
    case is_element(Element, ORSet) of
        true->
            ESet = sets:union(get_element_set(Element, A, ORSet),
                              get_element_set(Element, R, ORSet)),

            ORSet#orset{a=DictMod:erase(Element, A),
                        r=DictMod:store(Element, ESet, R)};
        false ->
            ORSet
    end.

merge(ORSet=#orset{a=A1, r=R1}, #orset{dictmod=DictMod, a=A2, r=R2}) ->
    MergeEDict = fun(D1, D2) ->
                         DictMod:merge(fun(_K, V1, V2) -> 
                                               sets:union(V1, V2) end,
                                    D1, D2) end,
    ORSet#orset{a=MergeEDict(A1, A2), r=MergeEDict(R1, R2)}.

is_element(Element, ORSet=#orset{a=A, r=R}) ->
    AESet = get_element_set(Element, A, ORSet),
    RESet = get_element_set(Element, R, ORSet),
    Items = sets:subtract(AESet, RESet),

    (sets:size(Items) > 0).
    
from_list(L) ->
    from_list(ordered, L).

from_list(ordered, L) ->
    from_list(orddict, L);
from_list(unordered, L) ->
    from_list(dict, L);
from_list(DictMod, L) ->
    lists:foldl(fun add_element/2, new(DictMod), L).


to_list(ORSet=#orset{dictmod=DictMod, a=A}) ->
    [Key || Key <- DictMod:fetch_keys(A), is_element(Key, ORSet)].

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

sets_merge_test() ->
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
    
    ?assertEqual([eric], to_list(S3)).

to_ordered_list_test() ->
    S = from_list(ordered, [glenn, eric, shawn]),
    ?assertEqual([eric, glenn, shawn], to_list(S)).
    

-endif.

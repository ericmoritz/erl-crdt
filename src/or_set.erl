-module(or_set).
-export([new/0, new/1, add_element/2, del_element/2, is_element/2, merge/2,
        from_list/1, to_list/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(orset, {mod, a, r}).


sorted_id() ->
    {now(), node(), make_ref()}. % can refs() be serialized?

get_element_set(Element, Dict, #orset{mod=Mod}) ->
    case dict:find(Element, Dict) of
        {ok, Value} ->
            Value;
        error ->
            Mod:new()
    end.

new() ->
    new(ordsets).

new(SetModule) ->
    #orset{mod=SetModule, a=dict:new(), r=dict:new()}.

add_element(Element, ORSet=#orset{mod=Mod, a=A, r=R}) ->
    Id = sorted_id(),
    ESet = Mod:add_element(Id, get_element_set(Element, A, ORSet)),

    ORSet#orset{a=dict:store(Element, ESet, A), r=R}.

del_element(Element, ORSet=#orset{mod=Mod, a=A, r=R}) ->
    case is_element(Element, ORSet) of
        true->
            ESet = Mod:union(get_element_set(Element, A, ORSet),
                             get_element_set(Element, R, ORSet)),

            ORSet#orset{a=dict:erase(Element, A),
                   r=dict:store(Element, ESet, R)};
        false ->
            ORSet
    end.

merge(ORSet=#orset{a=A1, r=R1}, #orset{mod=Mod, a=A2, r=R2}) ->
    MergeEDict = fun(D1, D2) ->
                         dict:merge(fun(_K, V1, V2) -> 
                                            Mod:union(V1, V2) end,
                                    D1, D2) end,
    ORSet#orset{a=MergeEDict(A1, A2), r=MergeEDict(R1, R2)}.

is_element(Element, ORSet=#orset{mod=Mod, a=A, r=R}) ->
    AESet = get_element_set(Element, A, ORSet),
    RESet = get_element_set(Element, R, ORSet),
    Items = Mod:subtract(AESet, RESet),

    (Mod:size(Items) > 0).
    
from_list(L) ->
    from_list(ordsets, L).

from_list(SetModule, L) ->
    lists:foldl(fun add_element/2, new(SetModule), L).

to_list(ORSet=#orset{a=A}) ->
    [Key || Key <- dict:fetch_keys(A), is_element(Key, ORSet)].

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
    
    ?assert([eric] =:= to_list(S3)).

-endif.

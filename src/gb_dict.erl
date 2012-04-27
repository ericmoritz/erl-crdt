-module(gb_dict).

-export([new/0, find/2, store/3, erase/2, merge/3, fetch_keys/1]).
-import(lists, [foldl/3]).

% A minimal dict-like interface for use with or_set.

new() ->
    gb_trees:empty().

store(Key, Value, Tree) ->
    case gb_trees:is_defined(Key, Tree) of
        true ->
            gb_trees:update(Key, Value, Tree);
        false ->
            gb_trees:insert(Key, Value, Tree)
    end.

erase(Key, Tree) ->
    gb_trees:delete_any(Key, Tree).

find(Key, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Value} ->
            {ok, Value};
        none ->
            error
    end.
            

merge(Fun, Tree1, Tree2) ->
    {Keys1, Keys2} = {sets:from_list(fetch_keys(Tree1)),
                      sets:from_list(fetch_keys(Tree2))},
    CommonKeys = sets:intersection(Keys1, Keys2),

    FoldKeys = fun(Keys, Tree, Initial) ->
                       foldl(fun(Key, Acc) ->
                                     {ok, Value} = find(Key, Tree),
                                     store(Key, Value, Acc) end,
                             Initial,
                             sets:to_list(sets:subtract(Keys, CommonKeys)))
               end,
    NewTree1 = FoldKeys(Keys1, Tree1, new()),
    NewTree2 = FoldKeys(Keys2, Tree2, NewTree1),
    foldl(fun(Key, Acc) ->
                  {ok, Value1} = find(Key, Tree1),
                  {ok, Value2} = find(Key, Tree2),
                  Value3 = Fun(Key, Value1, Value2),
                  store(Key, Value3, Acc) end,
          NewTree2, sets:to_list(CommonKeys)).
    
    
fetch_keys(Tree) ->
    gb_trees:keys(Tree).

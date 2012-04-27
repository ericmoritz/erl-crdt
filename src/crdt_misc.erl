-module(crdt_misc).

-export([sorted_id/0]).

sorted_id() ->
    {now(), node(), make_ref()}. % can refs() be serialized?


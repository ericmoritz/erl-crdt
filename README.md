# Erlang CRDT

An experimental port of my [Python
CRDT](https://github.com/ericmoritz/crdt) project


## Usage

Currently the erl-crdt app provides an Observed-Remove set which
enables elements to be removed and added concurrently.

A Concurrent add and removes favors add.

    S = or_set:from_list([glenn, eric]),

    % diverge
    S1 = or_set:del_element(glenn, S),
    S2 = or_set:add_element(shawn, S),
    
    % merge
    [eric, shawn] = or_set:to_list(or_set:merge(S1, S2)).
    



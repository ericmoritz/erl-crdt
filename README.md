# Erlang CRDT

An experimental port of my [Python
CRDT](https://github.com/ericmoritz/crdt) project


## Usage

Currently the erl-crdt app provides an Observed-Remove set which
enables elements to be removed and added concurrently.

A Concurrent add and removes favors add.

    S = orset:from_list([glenn, eric]),

    % diverge
    S1 = orset:del_element(glenn, S),
    S2 = orset:add_element(shawn, S),
    
    % merge
    [eric, shawn] = orset:to_list(orset:merge(S1, S2)).
    

In most applications, the orset is the preferred module to use.  



# Erlang CRDT

An experimental port of my [Python
CRDT](https://github.com/ericmoritz/crdt) project


## Usage

There is currently two sets defined at the moment.  An Observed-Remove
Set (`orset`) and Two Phase Set (`twopset`).

Both have the same interface.  The difference between the `orset` and
the `twopset` is that elements in a twopset can not be added again
after they have been removed.

    S = orset:new(),

    % diverge
    S1 = orset:add_element(a, S),
    S2 = orset:add_element(b, S),
    
    % merge
    S3 = orset:merge(S1, S2)
    
    % inspect
    true = orset:is_element(a, S3),
    true = orset:is_element(b, S3),

    % diverge further
    S4 = orset:del_element(b, S3),
    S5 = orset:add_element(c, S3),

    S6 = orset:merge(S4, S5),
    
    true = orset:is_element(c, S6),
    false = orset:is_element(b, S6).



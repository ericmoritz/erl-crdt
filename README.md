# Erlang CRDT

An experimental port of my [Python
CRDT](https://github.com/ericmoritz/crdt) project

If you don't know what a CRDT is, watch
[this](http://research.microsoft.com/apps/video/dl.aspx?id=153540)

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
    


## References
* [Conﬂict-free Replicated Data Types](http://hal.inria.fr/docs/00/60/93/99/PDF/RR-7687.pdf)
* [A comprehensive study of Convergent and Commutative Replicated Data Types](http://hal.archives-ouvertes.fr/docs/00/55/55/88/PDF/techreport.pdf)
* [Marc Shapiro's talk @ Microsoft](http://research.microsoft.com/apps/video/dl.aspx?id=153540)
* [Logoot](https://gforge.inria.fr/docman/view.php/1646/6393/weiss09.pdf)- CRDT for a distributed peer-to-peer Document editing

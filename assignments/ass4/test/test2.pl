
/*  del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */ 
del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.

/* same_set(S1, S2) -- verifies set equality among the sets S1 and S2*/
same_set([], []).
same_set([H|T], List) :- 
	mem(H, List), del(H, List, Rest),
    same_set(T, Rest).

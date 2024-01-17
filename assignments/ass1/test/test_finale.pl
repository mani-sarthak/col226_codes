/* To check membership of X in a given list */
mem(X, []) :- false.
mem(X, [X|_]).
mem(X, [_|T]) :- mem(X, T).



/* Part A.1 reflexive transitive closure over a relation R amd a set S*/

/* To check membership of a ralation (a, b) in the reflexive_transitive_closure of R and S*/
/* An observation to avoid infinite loops is that one relation wont be used twice while finding the
    transitive closure (if exists then simply use from the last time it is used) */
mem((X, X), refTransClosureHelper(R, S), _) :- mem(X, S), !.
mem((X, Y), refTransClosureHelper(R, S), Visited) :- 
    mem(X, S), 
    mem(Y, S), 
    mem((X, Y), R), 
    \+ mem((X, Y), Visited), write(([0],X, Y)), !.
mem((X, Z), refTransClosureHelper(R, S), Visited) :- 
    mem(X, S), 
    mem(Z, S),
    mem((X, Y), R), 
    \+ mem((X, Y), Visited), write((X, Y)),
    mem((Y, Z), refTransClosureHelper(R, S), [(X, Y)|Visited]), !.
    
mem((X, Y), reflexive_transitive_closure(R, S)) :- mem((X, Y), refTransClosureHelper(R, S), []).


/* Part A.2 reflexive symetric transitive closure over a relation R amd a set S*/

/* To check membership of a relation (a, b) in equivalence_closure of R and S */
mem((X, Y), reflexive_symetric_transitive_closure(R, S)) :- 
    mem((X, Y), reflexive_transitive_closure(R, S));
    mem((Y, X), reflexive_transitive_closure(R, S)).




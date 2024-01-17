mem(X, []) :- false.
mem(X, [X|_]).
mem(X, [_|T]) :- mem(X, T).


mem((X, X), refTransClosureHelper(R, S), _) :- mem(X, S), !.
mem((X, Y), refTransClosureHelper(R, S), Visited) :- 
    mem(X, S), 
    mem(Y, S), 
    mem((X, Y), R), 
    \+ mem((X, Y), Visited), write(([0],X, Y)).
mem((X, Z), refTransClosureHelper(R, S), Visited) :- 
    mem(X, S), 
    mem(Z, S),
    mem((X, Y), R), 
    \+ mem((X, Y), Visited), write((X, Y)),
    mem((Y, Z), refTransClosureHelper(R, S), [(X, Y)|Visited]).
    
mem((X, Y), reflexive_transitive_closure(R, S)) :- mem((X, Y), refTransClosureHelper(R, S), []).
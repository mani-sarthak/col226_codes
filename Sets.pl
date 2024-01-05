% We are specifuing here and not implememting !

mem(X, []) :- false.
mem(X, [X|_]) :- !. % as soon as it finds out element returns !
mem(X, [_|T]) :- mem(X, T).




subset([], S) :- !.
subset([M|T], S) :- mem(M, S), subset(T, S).


equal(S1, S2) :- subset(S1, S2), subset(S2, S1).



% Two ways to implement union
% 1. Using 'OR'
% inefficient !!
% mem(X, union(S1, S2)) :- mem(X, S1); mem(X, S2).


% 2. Using one function and then the other

mem(X, union(S1, S2)) :- mem(X, S1).
mem(X, union(S1, S2)) :- mem(X, S2).

% Efficiency:

% The second set of rules might be more efficient 
% in some cases. Once mem(X, S1) succeeds, it doesn't
% explore mem(X, S2). This can be important in cases 
% where finding membership in S1 is less expensive 
% than finding membership in S2.



% Logical Implications:

% The first set of rules expresses a logical OR 
% condition. If X is a member of S1 or S2, the goal 
% succeeds.
% The second set of rules expresses a more exclusive
%  condition. If X is a member of S1, it succeeds.
% If not, it checks if X is a member of S2.



mem(X, inter(S1, S2)) :- mem(X, S1), mem(X, S2).


mem(X, diff(S1, S2)) :- mem(X, S1), \+ mem(X, S2).

mem(X, compl(S)) :- \+ mem(X, S).


mem(X, sym_diff(S1, S2)) :- mem(X, union(S1, S2)), \+ mem(X, inter(S1, S2)).

mem(X, pow(S)) :- subset(X, S).

mem((X, Y), cartesian(S1, S2)) :- mem(X, S1), mem(Y, S2).



mem((X, X), id(S)) :- mem(X, S).

mem((X, Z), composition(R1, R2)) :- mem((X, Y), R1), mem((Y, Z), R2).

mem((Y, X), inv(R)) :- mem((X, Y), R).

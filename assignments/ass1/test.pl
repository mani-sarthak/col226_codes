% We are specifuing here and not implememting !

mem(X, []) :- false.
mem(X, [X|_]) :- !. % as soon as it finds out element returns !
mem(X, [_|T]) :- mem(X, T).



/*  del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */ 

del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.



/*  remdups(L, L1) remove duplicates from a list L to get L1 */

remdups([ ], [ ]) :- !.
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).



/* Assuming no duplicates in S1, S2

 here is an implementation of union of S1, S2 */

unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).

  

/* append(L1, L2, L3) -- append lis  L1 to list L2 to get list  L3 */

append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).



/* mapcons(X,L1, L2) --  cons the element X to each list in L1 to get L2 */

mapcons(X, [ ], [ ]) :- !.
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).

/* powerI( S, P1): Here is an implementation of powerset of S */

powerI([ ], [ [ ] ]) :- !.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).



/* cartesianl(S1, S2, S3) -- returns the cartesian product of sets S1 and S2 in S3 */

/*      S x 0 = 0 = 0 x S, where 0 is empty set {}      */
cartesianl(S, [], []) :- !.
cartesianl([], S, []) :- !.

/*   Using the first element in the first list  with the second list get the members of the cartesian product in S4 using mapcons(X, S2, S4). 

Recursively compute the rest members of the cartesian product apart from the elements from the first list for which already computed and then append the result in S3. */


cartesianl([X|S1], S2, S3) :- mapcons(X, S2, S4), cartesianl(S1, S2, S5), append(S4, S5, S3).


/* diffl(S1, S2, S3) -- computes the set differnce of S2 from S1 and puts the result in S3  */


/* Set difference for empty set is the set itself (base case).
Since the sets contain distinct elements we iterate for every element in the second set then delete it from the first set (if exists) using del(X, L1, L2) */
diffl(L, [], L) :- !.
diffl(L1, [X|S], L2) :- diffl(L1, S, L3),  del(X, L3, L2). 



/* inter(S1, S2, S3) -- computes the intersection of the sets S1 and S2 and puts the result in S3 */

/* Intersection with empty set results in an empty set. */
inter(S, [], []) :- !.
inter([], S, []) :- !.

/* If there is a common member between the sets S1 and S2 then add that to the result S3 */
inter(S1, [X|S2], [X|S3]) :- mem(X, S1), inter(S1, S2, S3).

/* If the top element X of second set is not in the first set then it is not a common member among the sets and hence we dont consider it in our final answer */
inter(S1, [X|S2], S3) :- \+ mem(X, S1), inter(S1, S2, S3).


/*Part A*/

/* Reflexive - Transitive Closure */

mem((X, X), refTransClose(R, S)) :- mem(X, S), !.
mem((X, Y), refTransClose(R, S)) :- mem((X, Y), R), !.
mem((X, Z), refTransClose(R, S)) :- mem((X, Y), R), mem((Y, Z), refTransClose(R, S)), !.

/* Reflexive - Transitive - Symetric Closure or Equivalence Closure 

Equivalence Closure -- Union of Reflexive Transitive Closure and its inverse. */

mem((X, Y), equiClose(R, S)) :- mem((X, Y), refTransClose(R, S)); mem((Y, X), refTransClose(R, S)).

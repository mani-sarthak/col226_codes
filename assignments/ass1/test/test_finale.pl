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





/* Part B*/

/*

B.1 


Since we dont have set implemented and rather have only Lists 
we have to ensure that List doesnt have duplicates !!


mapcons(X, [ ], [ ]) :- !.
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).

append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).

->  Easily can see that it works as expected apart from duplicate elements

powerI([ ], [ [ ] ]) :- !.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).
->  Implementation only using mapcons(_, _, _)  and append(_, _, _). 
    So is working correct except for duplicate elements.


examples for powerI 
powerI([], L).                          L = [[]]
powerI([2], L).                         L = [[2], []]
powerI([2, []], L)                      L = [[2, []], [2], [[]], []]
powerI([2, 3], L)                       L = [[2, 3], [2], [3], []]
powerI([2, 3, a], L)                    L = [[2, 3, a], [2, 3], [2, a], [2], [3, a], [3], [a], []]



del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.

->  Since whenever the head element of the second list matches X it is not added in Z,
    we can say that we have not taken any of X in the second list and hence delete works
    as expected.


unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).



examples for unionI (assuming no duplicates in the input lists)
unionI([], [], L)                               L = []
unionI([], [1, 2, 3], L)                        L = [1, 2, 3] 
unionI([1, 2, 3], [], L)                        L = [1, 2, 3]
unionI([1, 2, 3], [a, b], L)                    L = [1, 2, 3, a, b]
unionI([1, 2, 3], [2, 3, 4], L)                 L = [1, 2, 3, 4]
unionI([1, 2, 3], [3, 2, 4], L)                 L = [1, 2, 3, 4]
unionI([1, 3, 2], [2, 3, 4], L)                 L = [1, 3, 2, 4]
unionI([1, 3, 2], [3, 2, 4], L)                 L = [1, 3, 2, 4]
unionI([1, 3, 2], [2, 2, 4], L)                 L = [1, 3, 2, 4]
unionI([1, 2, 3, 2, 4, 2], [2, 3, 2], L)        L = [1, 2, 3, 2, 4, 2] ERROR (duplicate elements)
unionI([1, 2, 3, 2], [4, 4, 5, 4], L)           L = [1, 2, 3, 2, 4, 4, 5, 4] ERROR (duplicate elements)

From the examples above we can observe that unionI works only when the input lists dont have duplicates.
Apart from that it is taking the union of the sets (without guarantee on the duplicate elements !)

B.2 

From above we see that unionI(_, _, _) HAVE DUPLICATES !!
*/

/*
B.3 interI(S1, S2, S3) -- computes the intersection of the sets S1 and S2 and puts the result in S3.
*/


/* Intersection with empty set results in an empty set. */
interI(S, [], []) :- !.
interI([], S, []) :- !.

/* If there is a common member between the sets S1 and S2 then add that to the result S3 */
interI(S1, [X|S2], [X|S3]) :- mem(X, S1), interI(S1, S2, S3).

/* If the top element X of second set is not in the first set then it is not a common member among the sets and hence we dont consider it in our final answer */
interI(S1, [X|S2], S3) :- \+ mem(X, S1), interI(S1, S2, S3).



/*
B.4 diffI(S1, S2, S3) -- computes the set differnce of S2 from S1 and puts the result in S3 
*/


/* Set difference for empty set is the set itself (base case).
Since the sets contain distinct elements we iterate for every element in the second set then
delete it from the first set (if exists) using del(X, L1, L2) */


del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.

diffI(L, [], L) :- !.
diffI(L1, [X|S], L2) :- diffI(L1, S, L3),  del(X, L3, L2). 




/*
B.5 cartesianI(S1, S2, S3) -- returns the cartesian product of sets S1 and S2 in S3
*/


/* mapcons(X,L1, L2) --  cons the element X to each list in L1 to get L2 */
mapcons(X, [ ], [ ]) :- !.
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).


/* append(L1, L2, L3) -- append lis  L1 to list L2 to get list  L3 */
append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).


/*      S x 0 = 0 = 0 x S, where 0 is empty set {}      */
cartesianl(S, [], []) :- !.
cartesianl([], S, []) :- !.

/*   Using the first element in the first list  with the second 
list get the members of the cartesian product in S4 using mapcons(X, S2, S4). 

Recursively compute the rest members of the cartesian product apart 
from the elements from the first list for which already computed and then append the result in S3. */

cartesianl([X|S1], S2, S3) :- mapcons(X, S2, S4), cartesianl(S1, S2, S5), append(S4, S5, S3).


/*
B.6 Testcases for validation 


*/


/*

B.7 Given a code to generate the powerSet of a list, and we have to check that powerSet of different 
implementaions of a set (same numbers, ordered diffrenetly and no duplicates) are the same.

We first sort the sets (lists) since we have no duplicates  in both of them, after sorting 
the two lists will be ordered, now by 1-1 mapping from the powerSet generator function, we can check that
both of the lists will give the same output IF AND ONLY IF they have the same elements in them (they are equal). 
*/



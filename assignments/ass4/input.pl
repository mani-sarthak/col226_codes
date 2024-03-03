
/* Intersection with empty set results in an empty set. */
interHelper(S, [], []) :- !.
interHelper([], S, []) :- !.

/* If there is a common member between the sets S1 and S2 then add that to the result S3 */
interHelper(S1, [X|S2], [X|S3]) :- mem(X, S1), interHelper(S1, S2, S3).

/* If the top element X of second set is not in the first set then it is not a common member among the sets and hence we dont consider it in our final answer */
interHelper(S1, [X|S2], S3) :- mem(X, S1), interHelper(S1, S2, S3).

interI(S1, S2, S3) :- interHelper(S1, S2, Intersection), same_set(Intersection, S3).

/*
B.4 diffHelper(S1, S2, S3) -- computes the set differnce of S2 from S1 and puts the result in S3 
*/


/* Set difference for empty set is the set itself (base case).
Since the sets contain distinct elements we iterate for every element in the second set then
delete it from the first set (if exists) using del(X, L1, L2) */



diffHelper(L, [], L) :- !.
diffHelper(L1, [X|S], L2) :- diffHelper(L1, S, L3),  del(X, L3, L2). 
diffI(S1, S2, S3) :- diffHelper(S1, S2, SetDiff), same_set(SetDiff, S3).



/*
B.5 cartesianGenerate(S1, S2, S3) -- returns the cartesian product of sets S1 and S2 in S3
*/

cartesianGenerateHelper(X, [ ], [ ]) :- !.
cartesianGenerateHelper(X, [Y|R], [ [X, Y] | Z ]) :- cartesianGenerateHelper(X, R, Z).


/*      S x 0 = 0 = 0 x S, where 0 is empty set {}      */
cartesianGenerate(S, [], []) :- !.
cartesianGenerate([], S, []) :- !.

/*   Using the first element in the first list  with the second 
list get the members of the cartesian product in S4 using cartesianHelper(X, S2, S4). 

Recursively compute the rest members of the cartesian product apart 
from the elements from the first list for which already computed and then append the result in S3. */

cartesianGenerate([X|S1], S2, S3) :- cartesianGenerateHelper(X, S2, S4), cartesianGenerate(S1, S2, S5), append(S4, S5, S3).
cartesianI(S1, S2, S3) :- cartesianGenerate(S1, S2, Cartesian), same_set(Cartesian, S3).

/* HELPER FUNCTIONS */

/* To check membership of X in a given list */
mem(X, []) :- false.
mem(X, [X|_]).
mem(X, [_|T]) :- mem(X, T).


/* append(L1, L2, L3) -- append lis  L1 to list L2 to get list  L3 */
append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).


/*  del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */ 
del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.



/* Part A.1 reflexive transitive closure over a relation R amd a set S*/
/*  
    These have been checked extensively using the scripts which can be found here
    (https://drive.google.com/drive/folders/1Zk78tj0CF4eMyR_tFOe1m8z5TD_wIs2k?usp=sharing)
*/
/* To check membership of a ralation (a, b) in the reflexive_transitive_closure of R and S*/
/* An observation to avoid infinite loops is that one relation wont be used twice while finding the
    transitive closure (if exists then simply use from the last time it is used) */
mem((X, X), refTransClosureHelper(R, S), _) :- mem(X, S), !.
mem((X, Y), refTransClosureHelper(R, S), Visited) :- 
    mem(X, S), 
    mem(Y, S), 
    mem((X, Y), R), 
    \+ mem((X, Y), Visited), !.
mem((X, Z), refTransClosureHelper(R, S), Visited) :- 
    mem(X, S), 
    mem(Z, S),
    mem((X, Y), R), 
    \+ mem((X, Y), Visited),
    mem((Y, Z), refTransClosureHelper(R, S), [(X, Y)|Visited]), !.
    
mem((X, Y), reflexive_transitive_closure(R, S)) :- mem((X, Y), refTransClosureHelper(R, S), []).


/* Part A.2 reflexive symetric transitive closure over a relation R amd a set S*/


/* generate the Symmetric closure of a set (without duplicates) */
generateInverse([], []).
generateInverse([(X, X)|R], S) :- generateInverse(R, S).
generateInverse([(X, Y)|R], [(Y, X)|S]) :- generateInverse(R, S).

generateSymetricClosure(R, S) :-  generateInverse(R, L), append(R, L, S).


/* To check membership of a relation (a, b) in equivalence_closure of R and S
    find the symmetric closure of R and say it SymR. Then search for the member in the 
    reflexive transitive close of the 
 */

mem((X, Y), reflexive_symetric_transitive_closure(R, S)) :- 
    generateSymetricClosure(R, SymR),
    mem((X, Y), reflexive_transitive_closure(SymR, S)).



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

From above we see that unionI(_, _, _) CAN HAVE DUPLICATES (if the input lists have duplicates)!!
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



diffI(L, [], L) :- !.
diffI(L1, [X|S], L2) :- diffI(L1, S, L3),  del(X, L3, L2). 




/*
B.5 cartesianI(S1, S2, S3) -- returns the cartesian product of sets S1 and S2 in S3
*/

cartesianHelper(X, [ ], [ ]) :- !.
cartesianHelper(X, [Y|R], [ (X, Y) | Z ]) :- cartesianHelper(X, R, Z).


/*      S x 0 = 0 = 0 x S, where 0 is empty set {}      */
cartesianI(S, [], []) :- !.
cartesianI([], S, []) :- !.

/*   Using the first element in the first list  with the second 
list get the members of the cartesian product in S4 using cartesianHelper(X, S2, S4). 

Recursively compute the rest members of the cartesian product apart 
from the elements from the first list for which already computed and then append the result in S3. */

cartesianI([X|S1], S2, S3) :- cartesianHelper(X, S2, S4), cartesianI(S1, S2, S5), append(S4, S5, S3).


/*
B.6 Testcases for validation 


interI([], [1, 2], L).              L = []
interI([a, b], [], L).              L = []
interI([3, 4], [4, 5], L).          L = [4]
interI([3,4,7,8],[4,5,6,7,3],L).    L = [4,7,3]


diffI([],[4,5,6,7,3],L).            L = []
diffI([8, 7, 1], [], L).            L = [8,7,1]
diffI([3],[4,5,6,7,3],L).           L = []
diffI([3,4,7,8,6],[4,5,6,7,3],L).   L = [8]
diffI([8],[4,5,6,7,3],L).           L = [8]
diffI([8,7,1],[1,7],L).             L = [8]


cartesianI([1, 2, 3], [], L).       L = []
cartesianI([],[a, b, c], L).        L = []
cartesianI([1], [a, b, c], L).      L = [(1,a),(1,b),(1,c)]
cartesianI([1,2],[a,b,c],L).        L = [(1,a),(1,b),(1,c),(2,a),(2,b),(2,c)]
cartesianI([1,2,3],[a,b,c],L).      L = [(1,a),(1,b),(1,c),(2,a),(2,b),(2,c),(3,a),(3,b),(3,c)]
cartesianI([1, 2, 3], [[]], L).     L = [(1,[]),(2,[]),(3,[])]
cartesianI([[]], [a, b, c]).        L = [([],a),([],b),([],c)]

*/


/*

B.7 Given a code to generate the powerSet of a list, and we have to check that powerSet of different 
implementaions of a set (same elements, ordered diffrenetly and no duplicates) are the same.


ASSUMPTION : This method only works on simple sets with unordered elements and not on set of sets !


IDEA : Check membership of lists to check for equality rather than directly matching the lists. 
(This will get rid of the problems caused by ordering)
Once that is done now search element by element of one powerset say P1 into P2 using the membership above.
This will claim that P1 is a subset of P2.
Similarly check for each member of P2 search its membership in P1. This will claim P2 is a subset of P1.

Hence we can show that P1 = P2.


EXAMPLE:
powerI( [2, 1, 3], P1).          P1 = [[2,1,3],[2,1],[2,3],[2],[1,3],[1],[3],[]]
powerI( [3, 2, 1], P2).          P2 = [[3,2,1],[3,2],[3,1],[3],[2,1],[2],[1],[]]

We observe that [2, 1, 3] != [3, 2, 1] by directly matching them, but when we look for membership we 
can say that they are exactly the same. Doing that for each member in P1 first and then for P2 gives P1 = p2.



The following code shows the implementation.

mem(X, []) :- false.
mem(X, [X|_]).
mem(X, [_|T]) :- mem(X, T).


del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.


same_set([], []).
same_set([H|T], List) :- 
	mem(H, List), del(H, List, Rest),
    same_set(T, Rest).
    
belongs_to_powerset(_, []) :- fail.
belongs_to_powerset([], _).
belongs_to_powerset(H, [Head|Tail]) :- 
    same_set(H, Head); belongs_to_powerset(H, Tail).

    
subset_of_powerset([], _).
subset_of_powerset([H|T], Powerset) :-
    belongs_to_powerset(H, Powerset),
    subset_of_powerset(T, Powerset).
    
powerset_equal(Powerset1, Powerset2) :-
    subset_of_powerset(Powerset1, Powerset2),
    subset_of_powerset(Powerset2, Powerset1).

*/



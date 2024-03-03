
/* generate the Symmetric closure of a set (without duplicates) */
generateInverse([], []).
generateInverse([[X, X]|R], S) :- generateInverse(R, S).
generateInverse([[X, Y]|R], [[Y, X]|S]) :- generateInverse(R, S).

generateSymetricClosure(R, S) :-  generateInverse(R, L), append(R, L, S).


/* To check membership of a relation (a, b) in equivalence_closure of R and S
    find the symmetric closure of R and say it SymR. Then search for the member in the 
    reflexive transitive close of the 
 */

mem([X, Y], reflexive_symetric_transitive_closure(R, S)) :- 
    generateSymetricClosure(R, SymR),
    mem([X, Y], reflexive_transitive_closure(SymR, S)).



/* Part B*/

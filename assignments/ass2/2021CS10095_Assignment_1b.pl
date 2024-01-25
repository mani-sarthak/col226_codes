/*

Assignment 1.b


Consider the language of expressions with variables

E ∈ Exp ::= N ∣ T ∣ F ∣ x ∣ E1 + E2 ∣ E1 * E2 ∣ E1 ∧ E2 ∣ E1 ∨ E2 ∣ ¬E1 ∣ E1 = E2 ∣ E1 > E2 

Extend this language with other arithmetic, boolean and comparison operators and any other kind of expressions.  
Assume types intT and boolT.  You may add other types such as unitT.  

Encode the type-checking relation in PROLOG as a predicate hastype(G,E,T). 
*/



/*

E ∈ Exp ::= N ∣ true ∣ false ∣ x ∣                                                                          (basic types numeric, bool and variable)
            E1 + E2 ∣ E1 * E2 ∣ E1 - E2 | E1 / E2 | E1 mod E2 |                                             (arithmetic operations)
            E1 and_op E2 ∣ E1 or_op E2 ∣ not_op E1 ∣ E1 xor_op E2 | E1 imply_op E2 |                        (boolean operations)
            E1 eq_op E2 ∣ E1 gt_op E2 | E1 lt_op E2 | E1 geq_op E2 | E1 gle_op E2 | E1 neq_op E2 |          (comparison operations)
            if (E1, E2, E3)                                                                                 (complex expressios like -> if E1 then E2 else E3) 

Note: I have used the following operators for the above operations
    and_op, or_op, not_op, xor_op, imply_op, neq_op, eq_op, geq_op, leq_op, lt_op, gt_op, mod


*/


% Extended language of expressions with variables

% Base types
hastype(_, N, intT) :- integer(N).
hastype(_, true, boolT).
hastype(_, false, boolT).

% Variable lookup in context
hastype(G, X, T) :- member((X, T), G).

% Arithmetic operations +, -, *, /, mod
:- op(700, xfy, mod).

hastype(G, E1 + E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 - E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 * E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 / E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 mod E2, intT) :- hastype(G, E1, intT), hastype(G, E2, intT).

% Boolean operations %    &, | , ^ , -> , ~
:- op(700, xfy, and_op).
:- op(700, xfy, or_op).
:- op(700, xfy, xor_op).
:- op(700, xfy, imply_op).
:- op(700, fy, not_op).


hastype(G, E1 and_op E2, boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, E1 or_op E2, boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, E1 xor_op E2, boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, E1 imply_op E2, boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, not_op E1, boolT) :- hastype(G, E1, boolT).

% Comparison operations =, >, <, >=, <=, !=
:- op(700, xfy, neq_op).
:- op(700, xfy, eq_op).
:- op(700, xfy, geq_op).
:- op(700, xfy, leq_op).
:- op(700, xfy, lt_op).
:- op(700, xfy, gt_op).

hastype(G, E1 eq_op E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 gt_op E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 lt_op E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 geq_op E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 leq_op E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, E1 neq_op E2, boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).

% If-then-else
hastype(G, if(E1, E2, E3), T) :- 
    hastype(G, E1, boolT),
    hastype(G, E2, T),
    hastype(G, E3, T).




% Test cases
:- begin_tests(type_checking).

test(integer_arithmetic) :-
    hastype([], 5 + 3, intT).

test(boolean_expression) :-
    hastype([], T and T, boolT).

test(variable_lookup) :-
    G = [(x, intT), (y, boolT)],
    hastype(G, x + 4, intT).

test(comparison_operation) :-
    hastype([], 7 > 3, boolT).

test(if_then_else) :-
    G = [(x, boolT)],
    hastype(G, if(x, 10, 20), intT).

:- end_tests(type_checking).

% Run tests
:- run_tests.

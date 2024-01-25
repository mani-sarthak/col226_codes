/*

E ∈ Exp ::= N ∣ true ∣ false ∣ x ∣ string                                                                         (basic types numeric, bool and variable)
            E1 + E2 ∣ E1 * E2 ∣ E1 - E2 | E1 / E2 | E1 mod E2 |                                             (arithmetic operations)
            E1 and_op E2 ∣ E1 or_op E2 ∣ not_op E1 ∣ E1 xor_op E2 | E1 imply_op E2 |                        (boolean operations)
            E1 eq_op E2 ∣ E1 gt_op E2 | E1 lt_op E2 | E1 geq_op E2 | E1 gle_op E2 | E1 neq_op E2 |          (comparison operations)
            if (E1, E2, E3)                                                                                 (complex expressios like -> if E1 then E2 else E3) 
            concat(E1, E2) ∣ length(E1) ∣ substring(E1, E2)                                                 (string operations)

Note: I have used the following operators for the above operations
    and_op, or_op, not_op, xor_op, imply_op, neq_op, eq_op, geq_op, leq_op, lt_op, gt_op, mod

Note: uintT is a new type defined by me for unsigned integers (non-negative integers)
        and stringT is a new type defined by me for strings

*/




% Base types
hastype(_, N, intT) :- integer(N).
hastype(_, N, uintT) :- integer(N), N >= 0.
hastype(_, true, boolT).
hastype(_, false, boolT).
hastype(_, Str, stringT) :- string(Str).

% Variable lookup in context
hastype(G, X, T) :- member((X, T), G).


% String operations
hastype(G, concat(E1, E2), stringT) :-
    hastype(G, E1, stringT), hastype(G, E2, stringT).

% Length of a string, returning an integer
hastype(G, length(Str), intT) :-
    hastype(G, Str, stringT).

% Substring check, returning a boolean
hastype(G, substring(S1, S2), boolT) :-
    hastype(G, S1, stringT), hastype(G, S2, stringT).





% Arithmetic operations for uintT
% subtraction and division are not defined for uintT as they can result in negative numbers
hastype(G, E1 + E2, uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).
hastype(G, E1 * E2, uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).

% Arithmetic operations for intT
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



/*


% Test cases
hastype([(x, boolT)], false, boolT). true
hastype([(x, boolT)], fals, boolT). false
hastype([(x, boolT)], 1, boolT). false
hastype([(x, boolT)], 1, intT). true
hastype([(x, boolT)], -3, intT). true
hastype([(x, boolT)], -3, unitT). false
hastype([], 'Hello', stringT). true
hastype([], 'Hello World\n', stringT).
hastype([(x, boolT)], x, boolT). true
hastype([(x, boolT)], x, intT). false


hastype([], 3 + 4, intT). true
hastype([], 3 - 2, intT). true
hastype([], concat('Hello', 'World'), stringT). true
hastype([], length('Hello'), intT). true
hastype([], substring('Hello', 'World'), boolT). true
hastype([], true and_op false, boolT). true
hastype([], not_op true, boolT). true

hastype([(x, intT), (y, boolT), (s, stringT)], x + 4, intT). true
hastype([], if(true, 5, 10), intT). true


*/
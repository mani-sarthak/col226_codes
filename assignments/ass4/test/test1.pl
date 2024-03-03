
% base cases boolT, intT, uintT are types, all working independently among themselves only
% all things inside intT(), uintT(), boolT() are atomic ie intT(intT()) not valid.
bool(true).
bool(false).
zero(0).
intT(X) :-  integer(X).
uintT(X) :- intT(X), X > 0.
hastype(G, intT(X), intT).
hastype(G, boolT(X), boolT).
hastype(G, uintT(X), uintT).
hastype(G, X, boolT) :- bool(X).
hastype(G, X, intT) :- integer(X).
hastype(G, X, uintT) :- uintT(X).




% integer operators
hastype(G, add(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, sub(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, mul(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT). 
hastype(G, mod(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).


% unsigned operators
hastype(G, addU(E1, E2), uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).
hastype(G, mulU(E1, E2), uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).
hastype(G, divU(E1, E2), uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).
hastype(G, modU(E1, E2), uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).


% comparison operators
hastype(G, eq(E1, E2), boolT) :- hastype(G, E1, T), hastype(G, E2, T).
hastype(G, neq(E1, E2), boolT) :- hastype(G, E1, T), hastype(G, E2, T).
hastype(G, lt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, gt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, le(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, ge(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).

% boolean operators
hastype(G, not(E), boolT) :- hastype(G, E, boolT).
hastype(G, and(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, or(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, xor(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, imply(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).



% integer operators
hastype(G, add(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, sub(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, mul(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT). 
hastype(G, mod(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).


% unsigned operators
hastype(G, addU(E1, E2), uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).
hastype(G, mulU(E1, E2), uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).
hastype(G, divU(E1, E2), uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).
hastype(G, modU(E1, E2), uintT) :- hastype(G, E1, uintT), hastype(G, E2, uintT).


% comparison operators
hastype(G, eq(E1, E2), boolT) :- hastype(G, E1, T), hastype(G, E2, T).
hastype(G, neq(E1, E2), boolT) :- hastype(G, E1, T), hastype(G, E2, T).
hastype(G, lt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, gt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, le(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, ge(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).

% boolean operators
hastype(G, not(E), boolT) :- hastype(G, E, boolT).
hastype(G, and(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, or(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, xor(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, imply(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).



% The output of the loop have the same output type
hastype(G, ifThenElse(E1, E2, E3), T) :- hastype(G, E1, boolT), hastype(G, E2, T), hastype(G, E3, T).



/*

hastype([(a, intT), (b,intT)], add(mul(varT(a), intT(5)),mul(varT(b), intT(3))), boolT).
hastype([(a, intT), (b,intT)],add(mul(varT(a), intT(5)),mul(varT(b), intT(3))), intT).

hastype([(a, intT), (b, intT)], not(gt(add(mul(varT(a), intT(5)), mul(varT(b), intT(3))), intT(0))), boolT).
hastype([(a, intT), (b,intT)], not(gt(add(mul(varT(a), intT(5)),mul(varT(b), intT(3)))), intT(0)), intT).

hastype([(c,boolT)], varT(c), intT).
hastype([(c,boolT)], varT(c), boolT).

hastype([(a, boolT), (b, boolT)], and(or(false, varT(a)), and(varT(b), true)), boolT).
hastype([(a, boolT), (b, boolT)], and(or(false, varT(a)), and(varT(b), true)), intT).

hastype([(a, boolT)], not(varT(a)), boolT).
hastype([(a, intT), (b, intT)], eq(add(varT(a), intT(5)), mul(varT(b),intT(3))), boolT).


*/




/*

% Test cases for base type definitions
:- begin_tests(base_types).
test(bool_true) :- hastype([], boolT(true), boolT).
test(bool_false) :- hastype([], boolT(false), boolT).
test(int_positive) :- hastype([], intT(5), intT).
test(int_negative) :- hastype([], intT(-3), intT).
test(uint_positive) :- hastype([], uintT(7), uintT).
test(uint_zero) :- hastype([], uintT(0), uintT).
test(varT) :- hastype([(x, intT)], varT(x), intT).
:- end_tests(base_types).

% Test cases for integer operators
:- begin_tests(integer_operators).
test(add_int) :- hastype([], add(intT(3), intT(2)), intT).
test(sub_int) :- hastype([], sub(intT(5), intT(3)), intT).
test(mul_int) :- hastype([], mul(intT(4), intT(2)), intT).
test(div_int) :- hastype([], div(intT(8), intT(2)), intT).
test(mod_int) :- hastype([], mod(intT(9), intT(2)), intT).
:- end_tests(integer_operators).

% Test cases for boolean operators
:- begin_tests(boolean_operators).
test(not_bool) :- hastype([], not(boolT(true)), boolT).
test(and_bool) :- hastype([], and(boolT(true), boolT(false)), boolT).
test(or_bool) :- hastype([], or(boolT(false), boolT(true)), boolT).
test(xor_bool) :- hastype([], xor(boolT(true), boolT(true)), boolT).
test(imply_bool) :- hastype([], imply(boolT(false), boolT(true)), boolT).
:- end_tests(boolean_operators).

% Run all tests
:- run_tests.

*/

% Hello World program in Prolog
:- initialization hello_world.


% Define a rule that prints "Hello, World!"
hello_world :-
    write('Hello, World!'), nl.  % nl adds a newline



mem(X, []) :- fail.
mem(X, [X|_]) :- !.
mem(X, [_|R]) :- mem(X, R).



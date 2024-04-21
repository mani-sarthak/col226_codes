fact(0, 1).
fact(X, Y) :-
        X > 0,
        Z = X - 1,
        fact(Z, W),
        Y =  W * X.

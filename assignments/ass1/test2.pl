mem(X, []) :- false.
mem(X, [X|_]) :- !. % as soon as it finds out element returns !
mem(X, [_|T]) :- mem(X, T).


insert(X,L,L) :- mem(X,L).
insert(X,L,[X|L]) :- \+mem(X,L).

% union([],L,L) :- !.
% union([X|L1],L2,L) :- union(L1,L2,L), (mem(X,L1); mem(X,L2)).
% union([X|L1],L2,[X|L]) :- union(L1,L2,L).


del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.


union([ ], S2, S2) :- !.
union(S1, [ ], S1) :- !.
union([X|R], S2, [X|Z]) :- del(X, S2, S3),  union(R, S3, Z).

  



compose((A,B),[],[]) :- !.
compose((A,B),[(B,C)|L],[(A,C)|X]) :- compose((A,B),L,X).
compose((A,B),[X|R],Ans) :- compose((A,B),R,Ans).
% compose((A,B),[(B,C)|L],[(A,C)|R]) :- compose((A,B),L,R).

composeRelation([],B,[]) :- !.
composeRelation([X|R1],R2,R) :- composeRelation(R1,R2,R3), compose(X,R2,R4), union(R3,R4,R).


composeRelationMultipleTimes(R,0,[]).
composeRelationMultipleTimes(R,1,R).
composeRelationMultipleTimes(R1,X,R) :- Y is X-1, composeRelationMultipleTimes(R1,Y,Rint), composeRelation(Rint,R1,R).

% S is a set, R is the reflexive relation on S
createReflexive([],[]) :- !.
createReflexive([X|S],[(X,X)|R]) :- createReflexive(S,R). 

composeRelationWithUnion(R,S,0,L) :- createReflexive(S,L).
composeRelationWithUnion(R,S,X,L) :- Y is X - 1, composeRelationWithUnion(R,S,Y,L1), composeRelationMultipleTimes(R,X,L2), union(L1,L2,L).

rtc(R,S,L) :- length(S,X), composeRelationWithUnion(R,S,X,L).

rtcMembership(R,S,(A,B)) :- rtc(R,S,X), write(X), mem((A,B),X).
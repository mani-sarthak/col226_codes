/* Part A.1 reflexive transitive closure over a relation R amd a set S*/
/*  
    These have been checked extensively using the scripts which can be found here
    (https://drive.google.com/drive/folders/1Zk78tj0CF4eMyR_tFOe1m8z5TD_wIs2k?usp=sharing)
*/
/* To check membership of a ralation (a, b) in the reflexive_transitive_closure of R and S*/
/* An observation to avoid infinite loops is that one relation wont be used twice while finding the
    transitive closure (if exists then simply use from the last time it is used)
*/
mem([X, Y], refTransClosureHelper(R, S), Visited) :- 
    mem(X, S), 
    mem(Y, S), 
    mem([X, Y], R), 
    mem([X, Y], Visited), !.
mem([X, Z], refTransClosureHelper(R, S), Visited) :- 
    mem(X, S), 
    mem(Z, S),
    mem([X, Y], R), 
    mem([X, Y], Visited),
    mem([Y, Z], refTransClosureHelper(R, S), [[X, Y]|Visited]), !.
    
mem([X, Y], reflexive_transitive_closure(R, S)) :- mem([X, Y], refTransClosureHelper(R, S), []).




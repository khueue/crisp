check(Rel, X, Y) :-
    Goal =.. [Rel,X,Y],
    call(Goal).

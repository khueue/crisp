test(check/3, Goals) :-
    Goals = [ true
    , check(<, 1, 2)
    , check(=<, 1, 2)
    , check(=<, 1, 1)
    , check(@>, abb, aba)
    , check(@>=, aba, aba)
    , \+check(@>, aba, aba)
    , check(is, 3, 1+2)
    , (check(is, X, 1+2), X = 3)
    ].

check(Rel, X, Y) :-
    Goal =.. [Rel,X,Y],
    call(Goal).

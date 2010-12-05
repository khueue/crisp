% Utilities used by sorting routines.

%% check(+RelationPredicate, ?X, ?Y)
%
% True if RelationPredicate(X, Y) is true.

test(check/3, Goals) :-
    Goals = [ true
    , check(<, 1, 2)
    , check(=<, 1, 2)
    , check(=<, 1, 1)
    , \+ check(=<, 1, 0)
    , check(@>, abb, aba)
    , check(@>=, aba, aba)
    , \+ check(@>, aba, aba)
    , check(is, 3, 1+2)
    , (check(is, X, 1+2), X = 3)
    , (check(is, Y, 5-1), Y \== 3)
    ].

check(Rel, X, Y) :-
    Goal =.. [Rel,X,Y],
    call(Goal).

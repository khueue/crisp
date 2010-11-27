:- [check].

quick_sort(L, Rel, S) :-
    quick_sort(L, Rel, [], S).

quick_sort([], _, Sorted, Sorted).
quick_sort([P|Xs], Rel, Sorted0, Sorted) :-
    partition(Xs, Rel, P, Left, Right),
    quick_sort(Right, Rel, Sorted0, Right1),
    quick_sort(Left, Rel, [P|Right1], Sorted).

partition([], _, _, [], []).
partition([X|Xs], Rel, P, [X|Smalls], Bigs) :-
    check(Rel, X, P),
    partition(Xs, Rel, P, Smalls, Bigs).
partition([X|Xs], Rel, P, Smalls, [X|Bigs]) :-
    \+check(Rel, X, P),
    partition(Xs, Rel, P, Smalls, Bigs).

/*

% Without tail recursion:

quick_sort([], _, []).
quick_sort([P|Xs], Rel, Sorted) :-
    divide(Xs, Rel, P, Left, Right),
    quick_sort(Right, Rel, Right1),
    quick_sort(Left, Rel, Left1),
    concatenate(Left1, [P|Right1], Sorted).

concatenate([], L2, L2).
concatenate([X|L1], L2, [X|L3]) :-
    concatenate(L1, L2, L3).

*/

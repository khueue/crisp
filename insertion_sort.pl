:- [check].

insertion_sort(L, Rel, S) :-
    insertion_sort(L, Rel, [], S).

insertion_sort([], _, Sorted, Sorted).
insertion_sort([X|Xs], Rel, Sorted0, Sorted) :-
    ordered_insert(Sorted0, X, Rel, Sorted1),
    insertion_sort(Xs, Rel, Sorted1, Sorted).

ordered_insert([], X, _, [X]).
ordered_insert([Y|Ys], X, Rel, [X,Y|Ys]) :-
    check(Rel, X, Y).
ordered_insert([Y|Ys], X, Rel, [Y|L]) :-
    \+check(Rel, X, Y),
    ordered_insert(Ys, X, Rel, L).

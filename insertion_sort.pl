:- [sort_utils].

test(insertion_sort/3, Goals) :-
    Goals = [ true
    , insertion_sort([], _, [])
    , insertion_sort([1,3,-2], <, [-2,1,3])
    , insertion_sort([1,2,3], <, [1,2,3])
    , insertion_sort([1,2,3], =<, [1,2,3])
    , insertion_sort([a,c,b], @>=, [c,b,a])
    , insertion_sort([1,2,3], >, [3,2,1])
    ].

insertion_sort(L, Rel, S) :-
    insertion_sort(L, Rel, [], S).

insertion_sort([], _, Sorted, Sorted).
insertion_sort([X|Xs], Rel, Sorted0, Sorted) :-
    ordered_insert(Sorted0, X, Rel, Sorted1),
    insertion_sort(Xs, Rel, Sorted1, Sorted).

test(ordered_insert/3, Goals) :-
    Goals = [ true
    , ordered_insert([],    1, _, [1])
    , ordered_insert([1,3], 2, <, [1,2,3])
    , ordered_insert([1],   2, <, [1,2])
    , ordered_insert([1],   2, >, [2,1])
    ].

ordered_insert([], X, _, [X]).
ordered_insert([Y|Ys], X, Rel, [X,Y|Ys]) :-
    check(Rel, X, Y),
    !.
ordered_insert([Y|Ys], X, Rel, [Y|L]) :-
    % \+check(Rel, X, Y),
    ordered_insert(Ys, X, Rel, L).

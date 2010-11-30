:- [sort_utils].

%%  insertion_sort(+List, +Relation, ?SortedList)
%
%   True if SortedList is List ordered according to the
%   binary relation given by Relation.

insertion_sort(L, Rel, S) :-
    insertion_sort(L, Rel, [], S).

test(insertion_sort/3, Goals) :-
    Goals = [ true
    , insertion_sort([], _, [])
    , insertion_sort([1,3,-2], <, [-2,1,3])
    , insertion_sort([1,2,3], <, [1,2,3])
    , insertion_sort([1,2,3], =<, [1,2,3])
    , insertion_sort([a,c,b], @>=, [c,b,a])
    , insertion_sort([1,2,3], >, [3,2,1])
    ].

%%  XXX
%
%   XXXXXX

insertion_sort([], _, Sorted, Sorted).
insertion_sort([X|Xs], Rel, Sorted0, Sorted) :-
    ordered_insert(Sorted0, X, Rel, Sorted1),
    insertion_sort(Xs, Rel, Sorted1, Sorted).

test(insertion_sort/4, Goals) :-
    Goals = [ true
    , true
    ].

%%  XXX
%
%   XXXXXX

ordered_insert([], X, _, [X]).
ordered_insert([Y|Ys], X, Rel, [X,Y|Ys]) :-
    check(Rel, X, Y),
    !.
ordered_insert([Y|Ys], X, Rel, [Y|L]) :-
    % \+check(Rel, X, Y),
    ordered_insert(Ys, X, Rel, L).

test(ordered_insert/4, Goals) :-
    Goals = [ true
    , ordered_insert([], 1, _, [1])
    , ordered_insert([1,3], 2, <, [1,2,3,4])
    , ordered_insert([1], 2, <, [1,2])
    , ordered_insert([1], 2, >, [2,1])
    , (ordered_insert([1], 2, >, S), S = [2,1])
    ].

test(mem, Goals) :-
    Goals = [ true
    , member(a, [1,2,3])
    ].

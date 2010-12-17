:- module(insertion_sort, [insertion_sort/3]).

:- include('../lib/crisp_includes').

:- use_module('sort_utils').

%%  insertion_sort(+List, +Relation, ?SortedList)
%
%   True if SortedList is a permutation of List such that Relation(A, B)
%   is true for any two consecutive elements A and B in SortedList.

test(insertion_sort/3,
    [ true
    , insertion_sort([], _, [])
    , insertion_sort([1,3,-2], <, [-2,1,3])
    , insertion_sort([1,2,3], <, [1,2,3])
    , insertion_sort([1,2,3], =<, [1,2,3])
    , insertion_sort([a,c,b], @>=, [c,b,a])
    , insertion_sort([1,2,3], >, [3,2,1])
    ]).

insertion_sort(L, Rel, S) :-
    insertion_sort(L, Rel, [], S).

%   insertion_sort(+List, +Relation, +SortedList0, ?SortedList)
%
%   Uses SortedList0 to accumulate the sorted elements so far.

test(insertion_sort/4,
    [ true
    , insertion_sort([], <, [], [])
    , insertion_sort([3,2,1,4], <, [], [1,2,3,4])
    , (insertion_sort([3,2,1], <, [4,5], S1), S1 = [1,2,3,4,5])
    , (insertion_sort([3,2,1], <, [5,0], S2), S2 = [1,2,3,5,0])
    ]).

insertion_sort([], _, Sorted, Sorted).
insertion_sort([X|Xs], Rel, Sorted0, Sorted) :-
    ordered_insert(Sorted0, X, Rel, Sorted1),
    insertion_sort(Xs, Rel, Sorted1, Sorted).

%   ordered_insert(+OrderedList, +X, +Relation, ?OrderedListWithX)

test(ordered_insert/4,
    [ true
    , ordered_insert([], 1, _, [1])
    , ordered_insert([1,3], 2, <, [1,2,3])
    , ordered_insert([1], 2, <, [1,2])
    , ordered_insert([1], 2, >, [2,1])
    , (ordered_insert([1], 2, >, S), S = [2,1])
    ]).

ordered_insert([], X, _, [X]).
ordered_insert([Y|Ys], X, Rel, [X,Y|Ys]) :-
    check(Rel, X, Y),
    !.
ordered_insert([Y|Ys], X, Rel, [Y|L]) :-
    % \+ check(Rel, X, Y),
    ordered_insert(Ys, X, Rel, L).

:- module(quick_sort, [quick_sort/3]).

:- use_module('sort_utils').

:- include('../lib/crisp_includes').

%%  quick_sort(+List, +Relation, ?SortedList)
%
%   True if SortedList is a permutation of List such that Relation(A, B)
%   is true for any two consecutive elements A and B in SortedList.

test(quick_sort/3,
    [ true
    , quick_sort([], _, [])
    , quick_sort([1], _, [1])
    , quick_sort([1,2,3,4,5], <, [1,2,3,4,5])
    , quick_sort([5,4,3,2,1], <, [1,2,3,4,5])
    , quick_sort([1,1,1,1,1], <, [1,1,1,1,1])
    , quick_sort([1,2,3,1,2,3], >=, [3,3,2,2,1,1])
    , quick_sort([3,2,1,3,2,1], >=, [3,3,2,2,1,1])
    , (quick_sort([3,2,1,3,2,1], >=, S1), S1 == [3,3,2,2,1,1])
    , one:quick_sort([3,2,1,3,2,1], >=, _)
    ]).

quick_sort(L, Rel, S) :-
    quick_sort(L, Rel, [], S).

%   quick_sort(+List, +Relation, ?SortedList0, ?SortedList)
%
%   Uses SortedList0 as an accumulator that holds sorted elements that
%   should be inserted at the end of the final result.

test(quick_sort/4,
    [ true
    , quick_sort([], <, [], [])
    , quick_sort([1], <, [], [1])
    , quick_sort([], <, [1], [1])
    , quick_sort([4,9,2,4], <, [5,6,7], [2,4,4,9,5,6,7])
    , quick_sort([3,1,2,4,5], @>, [c,a,b], [5,4,3,2,1,c,a,b])
    ]).

quick_sort([], _, Sorted, Sorted).
quick_sort([P|Xs], Rel, Sorted0, Sorted) :-
    partition(Xs, Rel, P, Left, Right),
    quick_sort(Left, Rel, [P|Right1], Sorted),
    quick_sort(Right, Rel, Sorted0, Right1).

%   partition(+List, +Relation, +Pivot, ?Left, ?Right)
%
%   Partitions List into Left and Right such that Relation(X, Pivot)
%   is true for all elements X in Left.

test(partition/5,
    [ true
    , partition([], _, _, [], [])
    , partition([1,2,3,4,5], <, 3, [1,2], [3,4,5])
    , partition([1,2,3,4,5], =<, 3, [1,2,3], [4,5])
    , partition([1,2,3,4,5], <, 6, [1,2,3,4,5], [])
    , partition([1,2,3,4,5], >, 6, [], [1,2,3,4,5])
    , partition([1,1,1], =<, 1, [1,1,1], [])
    , partition([1,1,1], >=, 1, [1,1,1], [])
    , partition([1,1,1], >, 1, [], [1,1,1])
    , one:partition([1,1,1], >, 1, _, _)
    ]).

partition([], _, _, [], []).
partition([X|Xs], Rel, P, [X|Smalls], Bigs) :-
    check(Rel, X, P),
    !,
    partition(Xs, Rel, P, Smalls, Bigs).
partition([X|Xs], Rel, P, Smalls, [X|Bigs]) :-
    % \+ check(Rel, X, P),
    partition(Xs, Rel, P, Smalls, Bigs).

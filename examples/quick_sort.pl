:- module(quick_sort, [quick_sort/3, quick_sort_dcg/4]).

:- use_module('sort_utils').

:- include('../lib/crisp_includes').

%%  quick_sort(+List, +Relation, ?SortedList)
%
%   True if SortedList is a permutation of List such that Relation(A, B)
%   is true for any two consecutive elements A and B in SortedList.

describe(quick_sort/3,
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
    , onedet:quick_sort([3,2,1,3,2,1], >=, _)
    ]).

quick_sort(L, Rel, S) :-
    quick_sort(L, Rel, [], S).

%   quick_sort(+List, +Relation, ?SortedList0, ?SortedList)
%
%   Uses SortedList0 as an accumulator that holds sorted elements that
%   should be inserted at the end of the final result.

describe(quick_sort/4,
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

describe(partition/5,
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
    , onedet:partition([1,1,1], >, 1, _, _)
    ]).

partition([], _, _, [], []).
partition([X|Xs], Rel, P, [X|Smalls], Bigs) :-
    check(Rel, X, P),
    !,
    partition(Xs, Rel, P, Smalls, Bigs).
partition([X|Xs], Rel, P, Smalls, [X|Bigs]) :-
    % \+ check(Rel, X, P),
    partition(Xs, Rel, P, Smalls, Bigs).

%%  quick_sort_dcg(+List, +Relation, ?SortedListWithEnd, ?End)
%
%   Same specification as quick_sort/4 except for this:
%
%   This predicate is implemented using Definite Clause Grammar (DCG),
%   so the last two arguments are added upon compile-time and represent
%   difference lists: End is bound to the end of SortedListWithEnd.

describe(quick_sort_dcg/4,
    [ true
    , quick_sort_dcg([], <, [], [])
    , quick_sort_dcg([1], <, [1|E1], E1)
    , quick_sort_dcg([4,9,2,4,5,6,7], <, [2,4,4,5,6,7,9|E2], E2)
    , quick_sort_dcg([3,1,2,4,5,c,a,b], @>, [c,b,a,5,4,3,2,1|E3], E3)
    , one:quick_sort_dcg([3,1,2,4,5,c,a,b], @>, _, _)
    , onedet:quick_sort_dcg([3,1,2,4,5,c,a,b], @>, _, _)
    ]).

quick_sort_dcg([], _) --> [].
quick_sort_dcg([X|Xs], Rel) -->
    { partition(Xs, Rel, X, Left, Right) },
    quick_sort_dcg(Left, Rel),
    [X],
    quick_sort_dcg(Right, Rel).

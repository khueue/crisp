:- module(permutation_sort, [permutation_sort/3]).

:- ensure_loaded('../crisp_utils').
:- use_module('sort_utils').

%%  permutation_sort(+List, +Relation, ?SortedList)
%
%   True if SortedList is a permutation of List such that Relation(A, B)
%   is true for any two consecutive elements A and B in SortedList.

test(permutation_sort/3, Goals) :-
    Goals = [ true
    , permutation_sort([], _, [])
    , permutation_sort([1], _, [1])
    , permutation_sort([1,2,3,4,5], <, [1,2,3,4,5])
    , permutation_sort([5,4,3,2,1], <, [1,2,3,4,5])
    , fail:permutation_sort([1,1,1,1,1], <, [1,1,1,1,1]) % Fails is_sorted!
    , permutation_sort([1,2,3,1,2,3], >=, [3,3,2,2,1,1])
    , permutation_sort([3,2,1,3,2,1], >=, [3,3,2,2,1,1])
    , (permutation_sort([3,2,1,3,2,1], >=, S1), S1 = [3,3,2,2,1,1])
    , one:permutation_sort([3,2,1,3,2,1], >=, _)
    ].

permutation_sort(List, Rel, Sorted) :-
    permutation(List, Sorted),
    is_sorted(Sorted, Rel),
    !.

%   permutation(?List, ?Permutation)
%
%   True if Permutation is a permutation of List.

test(permutation/2, Goals) :-
    Goals = [ true
    , permutation([], [])
    , permutation([1], [1])
    , permutation([1,2], [1,2])
    , permutation([1,2], [2,1])
    , permutation([1,2,3], [1,2,3])
    , permutation([1,2,3], [1,3,2])
    , permutation([1,2,3], [2,1,3])
    , permutation([1,2,3], [2,3,1])
    , permutation([1,2,3], [3,1,2])
    , permutation([1,2,3], [3,2,1])
    ].

permutation([], []).
permutation([X|Xs], Permutation) :-
    permutation(Xs, Xs1),
    insert(Xs1, X, Permutation).

%   insert(+List, +Elem, ?ListWithElem)
%   insert(?List, +Elem, +ListWithElem)
%
%   True if ListWithElem is List with Elem inserted somewhere.

test(insert/3, Goals) :-
    Goals = [ true
    , insert([], 1, [1])
    , insert([2], 1, [1,2])
    , insert([2], 1, [2,1])
    , insert([3,2], 1, [1,3,2])
    , insert([3,2], 1, [3,1,2])
    , insert([3,2], 1, [3,2,1])
    ].

insert(L, Elem, [Elem|L]).
insert([X|L1], Elem, [X|L2]) :-
    insert(L1, Elem, L2).

%   is_sorted(+List, +Relation)
%
%   True if Relation(A, B) is true for any two consecutive elements A and B
%   in List.

test(is_sorted/2, Goals) :-
    Goals = [ true
    , is_sorted([], _)
    , is_sorted([1], _)
    , is_sorted([1,2], <)
    , is_sorted([1,2,3], <)
    , is_sorted([1,2,3,3], =<)
    , one:is_sorted([1,2,3,3,3], =<)
    , fail:is_sorted([1,2,3,3,2], =<)
    ].

is_sorted([], _).
is_sorted([_], _) :- !.
is_sorted([X,Y|Xs], Rel) :-
    check(Rel, X, Y),
    is_sorted([Y|Xs], Rel).

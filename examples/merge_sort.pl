:- module(merge_sort, [merge_sort/3]).

:- ensure_loaded('../crisp_utils').
:- use_module('sort_utils').

%%  merge_sort(+List, +Relation, ?SortedList)
%
%   True if SortedList is a permutation of List such that Relation(A, B)
%   is true for any two consecutive elements A and B in SortedList.

test(merge_sort/3, Goals) :-
    Goals = [ true
    , merge_sort([], <, [])
    , (merge_sort([4,2,3,1,-1], <, S1), S1 = [-1,1,2,3,4])
    , (merge_sort([4,2,3,1,-1], >=, S2), S2 = [4,3,2,1,-1])
    , one/merge_sort([4,2,3,1,-1], >=, _)
    ].

merge_sort([], _, []) :- !.
merge_sort([X], _, [X]) :- !.
merge_sort([X,Y|Xs], Rel, Sorted) :-
    split([X,Y|Xs], L1, L2),
    merge_sort(L1, Rel, S1),
    merge_sort(L2, Rel, S2),
    merge(S1, S2, Rel, Sorted).

%   split(+List, ?PartList1, ?PartList2)
%   split(?List, +PartList1, +PartList2)
%
%   Splits a list into two lists of roughly equal length.

test(split/3, Goals) :-
    Goals = [ true
    , split([], [], [])
    , split([1,2,3], [1,3], [2])
    , split([1,2,3,4], [1,3], [2,4])
    , (split([1,2,3,4], L1, L2), L1 = [1,3], L2 = [2,4])
    , (split(L, [1,3], [2,4]), L = [1,2,3,4])
    , one/split([1,2,3,4], _, _)
    , one/split([1,2,3,4,5], _, _)
    ].

split([], [], []) :- !.
split([X], [X], []) :- !.
split([X,Y|Xs], [X|L1], [Y|L2]) :-
    split(Xs, L1, L2).

%   merge(+SortedList1, +SortedList2, +Relation, ?SortedList)
%
%   Merges two sorted lists into one, maintaining sort order.

test(merge/4, Goals) :-
    Goals = [ true
    , merge([], [], <, [])
    , merge([1,3,5], [2,4], <, [1,2,3,4,5])
    ].

merge([], Ys, _, Ys) :- !.
merge(Xs, [], _, Xs) :- !.
merge([X|Xs], [Y|Ys], Rel, [X|L]) :-
    check(Rel, X, Y),
    !,
    merge(Xs, [Y|Ys], Rel, L).
merge([X|Xs], [Y|Ys], Rel, [Y|L]) :-
    % \+ check(Rel, X, Y),
    merge([X|Xs], Ys, Rel, L).

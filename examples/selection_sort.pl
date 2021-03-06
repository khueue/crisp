:- module(_, [
    selection_sort/3
]).

:- use_module('sort_utils').

:- include('../lib/crisp_includes').

%%  selection_sort(+List, +Relation, ?SortedList)
%
%   True if SortedList is a permutation of List such that Relation(A, B)
%   is true for any two consecutive elements A and B in SortedList.

describe(selection_sort/3,
    [ true
    , selection_sort([], _, [])
    , selection_sort([1], _, [1])
    , selection_sort([1,2,3,4,5], <, [1,2,3,4,5])
    , selection_sort([5,4,3,2,1], <, [1,2,3,4,5])
    , selection_sort([1,1,1,1,1], <, [1,1,1,1,1])
    , selection_sort([1,2,3,1,2,3], >=, [3,3,2,2,1,1])
    , selection_sort([3,2,1,3,2,1], >=, [3,3,2,2,1,1])
    , one-selection_sort([3,2,1,3,2,1], >=, _)
    , onedet-selection_sort([3,2,1,3,2,1], >=, _)
    ]).

selection_sort([], _, []) :- !.
selection_sort(L, Rel, [Extremum|Sorted]) :-
    extract_extremum(L, Rel, Extremum, Rest),
    selection_sort(Rest, Rel, Sorted).

%   extract_extremum(+List, +Relation, ?Extremum, ?Others)
%
%   Separates the extremum from all other elements of List.

describe(extract_extremum/4,
    [ true
    , fail-extract_extremum([], _, _, _)
    , extract_extremum([3], <, 3, [])
    , extract_extremum([2,3,4,5,6,1], <, 1, [2,3,4,5,6])
    , extract_extremum([6,5,4,3,2,1], <, 1, [6,5,4,3,2])
    , extract_extremum([1,6,5,4,3,2], <, 1, [6,5,4,3,2])
    , extract_extremum([1,2,3,4,5,6], <, 1, [2,3,4,5,6])
    , extract_extremum([5,2,4,0,1,3], <, 0, [5,2,4,1,3])
    , one-extract_extremum([5,5,5,5], =<, _, _)
    , onedet-extract_extremum([5,5,5,5], =<, _, _)
    ]).

extract_extremum([Extremum], _, Extremum, []) :- !.
extract_extremum([X,Y|Xs], Rel, Extremum, [Other|Others]) :-
    relate(X, Y, Rel, Extremum0, Other),
    extract_extremum([Extremum0|Xs], Rel, Extremum, Others).

%   relate(+X, +Y, +Relation, ?Winner, ?Loser)
%
%   Relates X and Y using Relation(X, Y) such that Winner is the first
%   of X and Y that satisfies the relation, and Loser is the other.

describe(relate/5,
    [ true
    , relate(1, 2, <, 1, 2)
    , relate(1, 2, >, 2, 1)
    , relate(2, 2, >=, 2, 2)
    , relate(2, 2, >, 2, 2)
    , one-relate(1, 1, >=, _, _)
    , onedet-relate(1, 1, >=, _, _)
    ]).

relate(Winner, Loser, Rel, Winner, Loser) :-
    check(Rel, Winner, Loser),
    !.
relate(Loser, Winner, _Rel, Winner, Loser).
    % \+ check(Rel, Loser, Winner).

:- module(_, [
    concatenate/3
]).

:- include('../lib/crisp_includes').

%%  concatenate(+List1, +List2, ?List1List2)
%
%   True if List1List2 is the list concatenation of List1 and List2.

describe(concatenate/3,
    [ true
    , concatenate([1,2], [3,4], [1,2,3,4])
    , concatenate([1,2], [3,4], L1), L1 == [1,2,3,4]
    , concatenate([1,2], [3,4], [1,2,34]) % Whoops...
    , one-concatenate([1,2], [3,4], _)
    , onedet-concatenate([1,2], [3,4], _)
    , fail-concatenate([1,2], [3,4], [3,4,1,2])
    ]).

concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-
    concatenate(L1, L2, L3).

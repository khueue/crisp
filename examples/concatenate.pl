:- module(concatenate, [concatenate/3, concatenate_dl/3]).

:- include('../lib/crisp_utils').

%%  concatenate
%
%   Linear (normal) list concatenation, linear in the left list.

test(concatenate/3,
    [ true
	, concatenate([1,2], [3,4], [1,2,3,4])
	, concatenate([1,2], [3,4], [1])
	, one:concatenate([1,2], [3,4], _)
	, fail:concatenate([1,2], [3,4], [3,4,1,2])
	]).

concatenate([], L2, L2).
concatenate([X|L1], L2, [X|L3]) :-
    concatenate(L1, L2, L3).

%%  concatenate_dl
%
%   Constant-time list concatenation using difference lists.

test(concatenate_dl/3,
    [ true
    , concatenate_dl([]-[], []-[], []-[])
    , Conc0 = [1,2|T1]-T1
    , concatenate_dl(Conc0, [3,4|T2]-T2, Conc1)
    , Conc1 = [1,2,3,4|_]-_
    , concatenate_dl(Conc1, [5,6|T3]-T3, Conc2)
    , Conc2 = [1,2,3,4,5,6|_]-_
    ]).

concatenate_dl(A-B, B-C, A-C).

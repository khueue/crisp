:- module(concatenate, [concatenate/3]).
:- ensure_loaded('../crisp_utils').

test(concatenate/3, Goals) :-
	Goals = [ true
	, concatenate([1,2], [3,4], [1,2,3,4])
	, concatenate([1,2], [3,4], [1])
	, one:concatenate([1,2], [3,4], _)
	, fail:concatenate([1,2], [3,4], [3,4,1,2])
	].

concatenate([], L2, L2).
concatenate([X|L1], L2, [X|L3]) :-
    concatenate(L1, L2, L3).

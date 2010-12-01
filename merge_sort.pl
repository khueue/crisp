:- [sort_utils].

%% XXX
%
% XXXXXX

merge_sort([], _, []) :- !.
merge_sort([X], _, [X]) :- !.
merge_sort([X,Y|Xs], Rel, Sorted) :-
    split([X,Y|Xs], L1, L2),
    merge_sort(L1, Rel, S1),
    merge_sort(L2, Rel, S2),
    merge(S1, S2, Rel, Sorted).

%% XXX
%
% XXXXXX

split([], [], []) :- !.
split([X], [X], []) :- !.
split([X,Y|Xs], [X|L1], [Y|L2]) :-
    split(Xs, L1, L2).

%% XXX
%
% XXXXXX

merge([], Ys, _, Ys) :- !.
merge(Xs, [], _, Xs) :- !.
merge([X|Xs], [Y|Ys], Rel, [X|L]) :-
    check(Rel, X, Y),
    !,
    merge(Xs, [Y|Ys], Rel, L).
merge([X|Xs], [Y|Ys], Rel, [Y|L]) :-
    % \+check(Rel, X, Y),
    merge([X|Xs], Ys, Rel, L).

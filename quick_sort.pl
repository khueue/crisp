:- [sort_utils].

%% XXX
%
% XXXXXX

quick_sort(L, Rel, S) :-
    quick_sort(L, Rel, [], S).

test(quick_sort/3, Goals) :-
    Goals = [ true
    , fail
    ].

%% XXX
%
% XXXXXX

quick_sort([], _, Sorted, Sorted).
quick_sort([P|Xs], Rel, Sorted0, Sorted) :-
    partition(Xs, Rel, P, Left, Right),
    quick_sort(Right, Rel, Sorted0, Right1),
    quick_sort(Left, Rel, [P|Right1], Sorted).

test(quick_sort/4, Goals) :-
    Goals = [ true
    , fail
    ].

%% XXX
%
% XXXXXX

partition([], _, _, [], []).
partition([X|Xs], Rel, P, [X|Smalls], Bigs) :-
    check(Rel, X, P),
    !,
    partition(Xs, Rel, P, Smalls, Bigs).
partition([X|Xs], Rel, P, Smalls, [X|Bigs]) :-
    % \+ check(Rel, X, P),
    partition(Xs, Rel, P, Smalls, Bigs).

test(partition/5, Goals) :-
    Goals = [ true
    , fail
    ].

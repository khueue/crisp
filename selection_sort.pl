:- [sort_utils].

%% XXX
%
% XXXXXX

selection_sort([], _, []) :- !.
selection_sort(L, Rel, [Extremum|Sorted]) :-
    extract_extremum(L, Rel, Extremum, Rest),
    selection_sort(Rest, Rel, Sorted).

test(selection_sort/3, Goals) :-
    Goals = [ true
    , fail
    ].

% extract_extremum(+List, +Relation, ?Extremum, ?Others) XXX
%
% Extracts the extremum value and all other elements from a list.

extract_extremum([Extremum], _, Extremum, []) :- !.
extract_extremum([X,Y|Xs], Rel, Extremum, [Other|Others]) :-
    relate(X, Y, Rel, Extremum0, Other),
    extract_extremum([Extremum0|Xs], Rel, Extremum, Others).

test(extract_extremum/4, Goals) :-
    Goals = [ true
    , extract_extremum([3], <, 3, [])
    , extract_extremum([2,3,1], <, 1, [2,3])
    ].

% XXX
%
% XXXXXX

relate(Winner, Loser, Rel, Winner, Loser) :-
    check(Rel, Winner, Loser),
    !.
relate(Loser, Winner, _, Winner, Loser).
    % \+ check(Rel, Loser, Winner).

test(relate/5, Goals) :-
    Goals = [ true
    , fail
    ].

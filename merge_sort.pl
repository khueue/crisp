% Needs tail recursion.
merge_sort([], []).
merge_sort([X], [X]).
merge_sort([X,Y|Xs], Sorted) :-
  split([X,Y|Xs], L1, L2),
  merge_sort(L1, S1),
  merge_sort(L2, S2),
  merge(S1, S2, Sorted).

split([], [], []).
split([X], [X], []).
split([X,Y|Xs], [X|L1], [Y|L2]) :-
  split(Xs, L1, L2).

merge([], Ys, Ys).
merge(Xs, [], Xs).
merge([X|Xs], [Y|Ys], [X|L]) :-
  X @< Y,
  !,
  merge(Xs, [Y|Ys], L).
merge([X|Xs], [Y|Ys], [Y|L]) :-
  % X @>= Y,
  merge([X|Xs], Ys, L).

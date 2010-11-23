insertion_sort(L, S) :-
  insertion_sort(L, [], S).

insertion_sort([], Sorted, Sorted).
insertion_sort([X|Xs], Sorted0, Sorted) :-
  ordered_insert(Sorted0, X, Sorted1),
  insertion_sort(Xs, Sorted1, Sorted).

ordered_insert([], X, [X]).
ordered_insert([Y|Ys], X, [X,Y|Ys]) :-
  X @< Y,
  !.
ordered_insert([Y|Ys], X, [Y|L]) :-
  % X @>= Y,
  ordered_insert(Ys, X, L).

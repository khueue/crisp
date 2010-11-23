selection_sort([], []).
selection_sort(L, [Min|Sorted]) :-
  extract_min(L, Min, Rest),
  selection_sort(Rest, Sorted).

extract_min([Min], Min, []).
extract_min([X,Y|Xs], Min, [Big|Rest]) :-
  min_max(X, Y, Small, Big),
  extract_min([Small|Xs], Min, Rest).

min_max(Min, Max, Min, Max) :-
  Min @< Max,
  !.
min_max(Max, Min, Min, Max).
  % Max @>= Min.

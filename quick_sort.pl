quick_sort(L, S) :-
  quick_sort(L, [], S).

quick_sort([], Sorted, Sorted).
quick_sort([P|Xs], Sorted0, Sorted) :-
  divide(Xs, P, Smaller, Bigger),
  quick_sort(Bigger, Sorted0, Bigger1),
  quick_sort(Smaller, [P|Bigger1], Sorted).

divide([], _, [], []).
divide([X|Xs], P, [X|Smalls], Bigs) :-
  X @< P,
  !,
  divide(Xs, P, Smalls, Bigs).
divide([X|Xs], P, Smalls, [X|Bigs]) :-
  % X @>= P,
  divide(Xs, P, Smalls, Bigs).

/*

% Without tail recursion:

quick_sort([], []).
quick_sort([P|Xs], Sorted) :-
  divide(Xs, P, Smaller, Bigger),
  quick_sort(Bigger, Bigger1),
  quick_sort(Smaller, Smaller1),
  concatenate(Smaller1, [P|Bigger1], Sorted).

concatenate([], L2, L2).
concatenate([X|L1], L2, [X|L3]) :-
  concatenate(L1, L2, L3).

*/

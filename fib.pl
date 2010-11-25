fib(N, Fib) :-
  fib(N, 0, 1, Fib).

fib(0, Fib, _, Fib) :-
  !.
fib(N, F0, F1, Fib) :-
  N > 0,
  !,
  N1 is N - 1,
  F2 is F0 + F1,
  fib(N1, F1, F2, Fib).

/*

% Naive yet beautiful version:

fib(0, 0).
fib(1, 1).
fib(N, Fib) :-
  N > 1,
  N1 is N - 1,
  N2 is N - 2,
  fib(N1, Fib1),
  fib(N2, Fib2),
  Fib is Fib1 + Fib2.

*/

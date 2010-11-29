:- [pest].

%%  XXX
%
%   XXXXXX

fib(N, Fib) :-
    fib(N, 0, 1, Fib).

%%  XXX
%
%   XXXXXX

fib(0, Fib, _, Fib).
fib(N, F0, F1, Fib) :-
    N > 0,
    N1 is N - 1,
    F2 is F0 + F1,
    fib(N1, F1, F2, Fib).

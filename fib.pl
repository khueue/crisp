%% fib(+Nth, +NthFib)
%
% True if NthFib is the Nth Fibonacci number, starting with (0, 0) and (1, 1).

fib(N, Fib) :-
    fib(N, 0, 1, Fib).

test(fib/2, Goals) :-
    Goals = [ true
    , fail:fib(-1, _)
    , fib(0, 0)
    , fib(1, 1)
    , fib(2, 1)
    , fib(3, 2)
    , fib(4, 3)
    , fib(5, 5)
    , fib(6, 8)
    ].

% fib(+Nth, +ZerothFib, +FirstFib, ?NthFib)

fib(0, Fib, _, Fib) :- !.
fib(N, F0, F1, Fib) :-
    N > 0,
    N1 is N - 1,
    F2 is F0 + F1,
    fib(N1, F1, F2, Fib).

test(fib/4, Goals) :-
    Goals = [ true
    , fail:fib(-1, _, _, _)
    , fib(0, 0, 1, 0)
    , fib(1, 0, 1, 1)
    , fib(2, 0, 1, 1)
    , fib(3, 0, 1, 2)
    , fib(4, 0, 1, 3)
    , fib(5, 0, 1, 5)
    , fib(6, 0, 1, 8)
    ].

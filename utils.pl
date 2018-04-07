:- module(_, [
    seq/3
]).

seq(Min, Max, Seq) :-
    findall(N, between(Min,Max,N), Seq).

Crisp - Crazy simple unit testing in Prolog

Crisp should be compatible with:
- SWI-Prolog 5.10.2
- SICStus 3.12.5

Crisp is NOT compatible with:
- GNU Prolog (due to its lack of a proper module system)

Make sure the directives in crisp_utils are loaded by each file that
needs testing, and then sprinkle your code with test/2 predicates:

:- ensure_loaded('path/to/crisp_utils').

member(X, [X|_]).
member(X, [_|T]) :-
    member(X, T).

test(member/2, Goals) :-
	Goals = [ true
	, member(1, [1,2,3])
	, fail:member(1, [])
	].

See the examples folder for more example usage.

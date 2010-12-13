Crisp - Crazy simple unit testing in Prolog

Crisp should be compatible with:
- SWI-Prolog 5.10.2
- SICStus 3.12.5

Crisp is NOT compatible with:
- GNU Prolog (due to its lack of a proper module system)

Make sure the directives in crisp_utils are loaded by each file that
needs testing, and then sprinkle your code with test/2 predicates:

	:- module(conc, [conc/3]).
	:- ensure_loaded('path/to/crisp_utils').

	test(conc/3, Goals) :-
		Goals = [ true
		, conc([1,2], [3,4], [1,2,3,4])
		, one:conc([1,2], [3,4], _)
		, fail:conc([1,2], [3,4], [3,4,1,2])
		].

	conc([], L2, L2).
	conc([X|L1], L2, [X|L3]) :-
	    conc(L1, L2, L3).

The test case 'true' is simply ignored. It's just a trick to make the
remaining test cases line up nicely with the commas. Do what you want
with it.

See the examples folder for more examples.

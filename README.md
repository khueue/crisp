# Crisp - Crazy Simple Unit Testing in Prolog

## Compatibility

Crisp should be compatible with:

 * SWI-Prolog 5.10.2

 * SICStus 3.12.5

Crisp is NOT compatible with:

 * GNU Prolog (due to its lack of a proper module system)

## Usage

Make sure the directives in crisp_utils are loaded by each file (module or not) that needs testing, and then sprinkle your code with test/2 predicates:

	:- module(concatenate, [concatenate/3]).
	:- ensure_loaded('path/to/crisp_utils').

	test(concatenate/3, Goals) :-
		Goals = [ true
		, concatenate([1,2], [3,4], [1,2,3,4])
		, concatenate([1,2], [3,4], [1])
		, one:concatenate([1,2], [3,4], _)
		, fail:concatenate([1,2], [3,4], [3,4,1,2])
		].

	concatenate([], L2, L2).
	concatenate([X|L1], L2, [X|L3]) :-
	    concatenate(L1, L2, L3).

Then, simply issue a call to crisp/0:

	?- crisp.
	Crisp 0.0.1

	Module: concatenate
	- concatenate/3
	  !!! fail: concatenate([1,2],[3,4],[1])
	  => 1/4 fail, 3/4 pass

	Summary: 1/4 fail, 3/4 pass

	true.

Special forms of goals:

 * true - Simply ignored. It's just a trick to make the remaining test cases line up nicely with the commas.

 * one:Goal - Fails if Goal has more than one answer (basically just does a findall and checks the number of answers).

 * fail:Goal - Succeeds if Goal fails. Basically the same as \\+Goal.

See the examples folder for more examples.

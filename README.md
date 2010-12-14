# Crisp - Crazy Simple Unit Testing in Prolog


## Compatibility

Crisp should be compatible with:

 * SWI-Prolog 5.10.2

 * SICStus 3.12.5

Crisp is NOT compatible with:

 * GNU Prolog (due to its lack of a proper module system)


## Usage

Make sure the directives in crisp_utils are loaded by each file that needs testing, and then sprinkle your code with test/2 predicates. Files under test need not be modules, and predicates under test need not be exported. Example with a module:

	:- module(concatenate, [concatenate/3]).
	:- ensure_loaded('../crisp_utils').

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

Then, simply call crisp/0 to run all tests:

	?- crisp.
	Crisp 0.0.1

	Module: concatenate
	- concatenate/3
	  !!! FAIL: concatenate([1,2],[3,4],[1])
	  => 1/4 fail, 3/4 pass

	Summary: 1/4 fail, 3/4 pass

	true.

Special forms of test goals:

 * true - Simply ignored. It's just a trick to make the remaining test cases line up nicely with the commas.

 * one:Goal - Fails if Goal has more than one answer. Basically just does a findall and checks the number of answers.

 * fail:Goal - Succeeds if Goal fails. Basically the same as \\+Goal.

See the examples folder for more examples.

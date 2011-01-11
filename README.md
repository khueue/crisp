# Crisp - Crazy Simple Unit Testing in Prolog

Crisp is a tiny unit testing tool designed for ease of use and simplicity. It is basically just a wrapper around a very basic idea: keeping small and simple tests close to the code at hand.

## Compatibility

Crisp should be compatible with at least:

 * SWI-Prolog 5.10.2
 * SICStus Prolog 3.12.5
 * YAP Prolog 6.2.0

Crisp is NOT compatible with:

 * GNU Prolog (due to its lack of a proper module system)

## Quick Start

For a full demonstration of Crisp running all the tests in the examples folder:

 1. Clone the repository.
 2. Start SWI-Prolog in the project root.
 3. `?- ['lib/crisp', 'examples/*'].`
 4. `?- crisp.`

## Usage

First make sure the directives in lib/crisp_includes.pl are included in each file that needs testing (adequately solved by a call to include/1). These included directives let us define test/2 predicates in several places and files:

    :- multifile test/2.
    :- discontiguous test/2.

Then sprinkle your code with test/2 predicates, where the first argument is a label for the test (name and arity of the tested predicate might be a good candidate), and the second argument is a list of goals that are supposed to succeed. Files with tests need not be modules, and predicates with tests need not be exported -- Crisp traverses all loaded modules, including the top "user" level. Example using a module (examples/concatenate.pl):

    :- module(concatenate, [concatenate/3, concatenate_dl/3]).

    :- include('../lib/crisp_includes').

    %%  concatenate
    %
    %   Linear (normal) list concatenation, linear in the left list.

    test(concatenate/3,
        [ true
        , concatenate([1,2], [3,4], [1,2,3,4])
        , concatenate([1,2], [3,4], [1])
        , one:concatenate([1,2], [3,4], _)
        , fail:concatenate([1,2], [3,4], [3,4,1,2])
        ]).

    concatenate([], L2, L2).
    concatenate([X|L1], L2, [X|L3]) :-
        concatenate(L1, L2, L3).

    %%  concatenate_dl
    %
    %   Constant-time list concatenation using difference lists.

    test(concatenate_dl/3,
        [ true
        , concatenate_dl([]-[], []-[], []-[])
        , Conc0 = [1,2|T1]-T1
        , concatenate_dl(Conc0, [3,4|T2]-T2, Conc1)
        , Conc1 = [1,2,3,4|_]-_
        , concatenate_dl(Conc1, [5,6|T3]-T3, Conc2)
        , Conc2 = [1,2,3,4,5,6|_]-_
        ]).

    concatenate_dl(A-B, B-C, A-C).

Simply call crisp/0 to run all tests:

    ?- crisp.
    Crisp 0.0.1

    Module: concatenate
    - concatenate/3
      !!! FAIL: concatenate([1,2],[3,4],[1])
      => 1/4 fail, 3/4 pass
    - concatenate_dl/3 => all/6 pass

    Summary: 1/10 fail, 9/10 pass
    true.

## Miscellaneous

### Special Forms of Goals

A test goal can be anything (supposed to evaluate to true), but Crisp provides the following syntactic sugar for convenience and readability:

 * `true` - Ignored. It's just a (foul) trick to make the remaining test cases line up nicely with the commas (see the examples).
 * `one:Goal` - Succeeds if Goal has exactly one solution. Basically just does a findall and checks the number of solutions.
 * `fail:Goal` - Succeeds if Goal fails. Basically the same as \\+Goal.

### Hints for Constructing Tests

_Determinism._ If a predicate is supposed to generate only one solution, make sure to include a goal using `one:` described above. Using underscores for output is fine, and encouraged, because it does not draw our attention to what the actual result is (other goals should check that), only that we get exactly one solution. Example:

    ...
    , one:quick_sort([3,2,1,3,2,1], >=, _)
    ...

_Output Arguments._ When output arguments may be instantiated beforehand (often prefixed with '?' in documentation), make sure to check both situations: instantiated beforehand and only validated by the predicate, and uninstantiated and then supplied by the predicate. Be sure to use exact comparisons, such as `==`, when checking the generated result: if you use simple equals, `=`, and the predicate (mistakenly) leaves the output unbound, your check will succeed always. Example:

    ...
    , quick_sort([3,2,1,3,2,1], >=, [3,3,2,2,1,1])
    , (quick_sort([3,2,1,3,2,1], >=, S1), S1 == [3,3,2,2,1,1])
    ...

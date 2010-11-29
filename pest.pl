/***************************************************************************
 * Let t/0 be defined by several files instead of each file overwriting old
 * definitions. This is important and must be added to all files that have
 * t/0 clauses!
 */

:- multifile t/0.

/***************************************************************************
 * Ignore warnings about t/0 (and any other predicates) not being defined
 * together (they are split over many files).
 */

:- set_prolog_flag(discontiguous_warnings, off).

/***************************************************************************
 * ?- test.
 *
 * Runs all tests. Always succeeds.
 */

test :-
    write('Started testing.'), nl, nl,
    findall(t(TestName,Goals), t(TestName,Goals), Tests),
    run_unit_tests(Tests), nl,
    write('Finished testing.'), nl.

run_unit_tests([]).
run_unit_tests([t(Label,Goals)|Tests]) :-
    write('--- Testing: '), write(Label), nl,
    run_tests(Goals),
    run_unit_tests(Tests).

run_tests([]).
run_tests([Goal|Goals]) :-
    run_test(Goal),
    run_tests(Goals).

/***************************************************************************
 * ?- t(+Goal).
 *
 * Calls Goal and does nothing if it succeeds.
 * Prints a message if Goal fails.
 */

run_test(Goal) :-
    Goal,
    !.
run_test(Goal) :-
    write('    >>> FAILED: '), write(Goal), nl.

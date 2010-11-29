/***************************************************************************
 * Let t/0 be defined by several files instead of each file overwriting old
 * definitions. This is important and must be added to all files that have
 * t/0 clauses!
 */

:- multifile test/2.

/***************************************************************************
 * Ignore warnings about t/0 (and any other predicates) not being defined
 * together (they are split over many files).
 */

:- set_prolog_flag(discontiguous_warnings, off).

%%  XXX
%
%   XXXXXX

test :-
    write('Started testing.'), nl, nl,
    collect_and_run, nl,
    write('Finished testing.'), nl.

collect_and_run :-
    findall(test(Name,Goals), test(Name,Goals), Tests),
    run_all_tests(Tests).

run_all_tests([]).
run_all_tests([test(Name,Goals)|Tests]) :-
    write('--- '), write(Name), nl,
    run_tests(Goals),
    run_all_tests(Tests).

run_tests([]).
run_tests([Goal|Goals]) :-
    run_test(Goal),
    run_tests(Goals).

run_test(Goal) :-
    call(Goal),
    !.
run_test(Goal) :-
    write('    >>> FAIL: '), write(Goal), nl.

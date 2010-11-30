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
    write('Started testing.'), nl,
    nl, collect_and_run, nl,
    write('Finished testing.'), nl.

collect_and_run :-
    findall(test(Name,Goals), test(Name,Goals), Tests),
    run_all_tests(Tests).

run_all_tests([]).
run_all_tests([test(Name,Goals)|Tests]) :-
    write('--- '), write(Name), nl,
    run_tests(Goals, 0, Pass, 0, Fail),
    Total is Pass + Fail,
    write('pass: '), write(Pass), write('/'), write(Total), nl,
    run_all_tests(Tests).

run_tests([], Pass, Pass, Fail, Fail).
run_tests([Goal|Goals], Pass0, Pass, Fail0, Fail) :-
    run_test(Goal, Pass0, Pass1, Fail0, Fail1),
    run_tests(Goals, Pass1, Pass, Fail1, Fail).

run_test(Goal, Pass0, Pass, Fail, Fail) :-
    call(Goal),
    !,
    Pass is Pass0 + 1.
run_test(Goal, Pass, Pass, Fail0, Fail) :-
    write('    >>> FAIL: '), write(Goal), nl,
    Fail is Fail0 + 1.

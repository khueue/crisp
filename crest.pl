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
    run_tests(Goals, stats(0,0,0), stats(Pass,Fail,Total)),
    write('    Fail: '), write(Fail/Total),
    write(', Pass: '), write(Pass/Total), nl,
    run_all_tests(Tests).

run_tests([], Stats, Stats).
run_tests([Goal|Goals], Stats0, Stats) :-
    run_test(Goal, Result),
    update_stats(Result, Stats0, Stats1),
    run_tests(Goals, Stats1, Stats).

run_test(Goal, pass) :-
    call(Goal),
    !.
run_test(Goal, fail) :-
    % \+call(Goal),
    write('    >>> FAIL: '), write(Goal), nl.

update_stats(pass, stats(Pass0,Fail,Total0), stats(Pass,Fail,Total)) :-
    Pass is Pass0 + 1,
    Total is Total0 + 1.
update_stats(fail, stats(Pass,Fail0,Total0), stats(Pass,Fail,Total)) :-
    Fail is Fail0 + 1,
    Total is Total0 + 1.

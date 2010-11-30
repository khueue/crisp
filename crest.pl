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
    test(Name, Goals),
    run_all_tests(Name, Goals),
    fail.
collect_and_run.
    % No more tests.

run_all_tests(Name, Goals) :-
    write(Name), write(':'), nl,
    run_tests(Goals, stats(0,0,0), Stats),
    write_stats(Stats).

write_stats(Stats) :-
    write('    '), write_fail(Stats),
    write(', '), write_pass(Stats),
    nl.

write_fail(stats(_,Fail,Total)) :-
    write('Fail: '), write(Fail/Total).

write_pass(stats(Pass,_,Total)) :-
    write('Pass: '), write(Pass/Total).

run_tests([], Stats, Stats).
/*run_tests(['true'|Goals], Stats, Stats) :-
    !,
    run_tests(Goals, Stats, Stats).*/
run_tests([Goal|Goals], Stats0, Stats) :-
    run_test(Goal, Result),
    write_result(Goal, Result),
    update_stats(Result, Stats0, Stats1),
    run_tests(Goals, Stats1, Stats).

run_test(Goal, pass) :-
    call(Goal),
    !.
run_test(Goal, fail).
    % Goal failed.

write_result(_, pass) :- !.
write_result(Goal, fail) :-
    write('    >>> FAIL: '), write(Goal), nl.

update_stats(pass, stats(Pass0,Fail,Total0), stats(Pass,Fail,Total)) :-
    Pass is Pass0 + 1,
    Total is Total0 + 1.
update_stats(fail, stats(Pass,Fail0,Total0), stats(Pass,Fail,Total)) :-
    Fail is Fail0 + 1,
    Total is Total0 + 1.

% Crisp - Crazy simple unit testing in Prolog

crisp_version([0,0,1]).

% Let test/2 be defined by several files instead of each file overwriting old
% definitions. With SICStus, I believe this must be added to all files that
% have test/2 clauses.

:- multifile test/2.

% Ignore warnings about test/2 (and any other predicates!) not being defined
% together (split over many files).

:- set_prolog_flag(discontiguous_warnings, off).

%% XXX
%
% XXXXXX

test :-
    crisp_version(Version),
    write('Crisp '), write_version(Version), nl,
    write('Started testing.'), nl,
    nl, collect_and_run, nl,
    write('Finished testing.'), nl.

write_version([X]) :-
    !,
    write(X).
write_version([X|Xs]) :-
    write(X), write('.'),
    write_version(Xs).

collect_and_run :-
    findall(test(Name,Goals), test(Name,Goals), Tests),
    run_all_tests(Tests, stats(Pass,Fail)),
    nl,
    write('Summary: '),
    Total is Pass + Fail,
    write(Fail/Total), write(' fail, '),
    write(Pass/Total), write(' pass'),
    nl.

run_all_tests(Tests, GlobalStats) :-
    run_all_tests(Tests, stats(0,0), GlobalStats).

run_all_tests([], GlobalStats, GlobalStats).
run_all_tests([test(Name,Goals)|Tests], GlobalStats0, GlobalStats) :-
    write_test_name(Name),
    run_goals(Goals, TestStats),
    write_stats(TestStats),
    update_global_stats(GlobalStats0, TestStats, GlobalStats1),
    run_all_tests(Tests, GlobalStats1, GlobalStats).

write_test_name(Pred/Arity) :-
    !,
    write('- '), write(Pred/Arity).
write_test_name(Name) :-
    write('- \''), write(Name), write('\'').

run_goals(Goals, TestStats) :-
    run_goals(Goals, stats(0,0), TestStats).

update_global_stats(stats(Pass0,Fail0), stats(TestPass,TestFail), stats(Pass,Fail)) :-
    Pass is Pass0 + TestPass,
    Fail is Fail0 + TestFail.

write_stats(stats(Pass,0)) :-
    !,
    write(' => '), write_pass(stats(Pass,0)), nl.
write_stats(Stats) :-
    nl,
    write('    => '), write_fail(Stats),
    write(', '), write_pass(Stats), nl.

write_pass(stats(Pass,Fail)) :-
    Total is Pass + Fail,
    write(Pass/Total), write(' pass').

write_fail(stats(Pass,Fail)) :-
    Total is Pass + Fail,
    write(Fail/Total), write(' fail').

run_goals([], Stats, Stats).
run_goals([true|Goals], Stats0, Stats) :-
    !,
    run_goals(Goals, Stats0, Stats).
run_goals([Goal|Goals], Stats0, Stats) :-
    % Goal \== true,
    execute_test(Goal, Result),
    write_result(Goal, Result),
    update_stats(Result, Stats0, Stats1),
    run_goals(Goals, Stats1, Stats).

execute_test(Goal, pass) :-
    call(Goal),
    !.
execute_test(_Goal, fail).
    % \+ call(Goal).

write_result(_Goal, pass) :- !.
write_result(Goal, fail) :-
    nl,
    write('    >>> FAIL: '), write(Goal).

update_stats(pass, stats(Pass0,Fail), stats(Pass,Fail)) :-
    Pass is Pass0 + 1.
update_stats(fail, stats(Pass,Fail0), stats(Pass,Fail)) :-
    Fail is Fail0 + 1.

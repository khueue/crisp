%   Crisp - Crazy simple unit testing in Prolog

:- module(crisp, [crisp/0, crisp_version/1]).

crisp_version([0,0,1]).

%%  crisp
%
%   Runs all test/2 predicates defined in any module, except those that
%   seem to be loaded by the environment, and then prints a test report.

crisp :-
    write_prologue,
    collect_and_run,
    write_epilogue.

collect_and_run :-
    findall(Module, current_module(Module), Modules),
    run_modules(Modules, stats(0,0), GlobalStats),
    %module(user),
    write_summary(GlobalStats).

write_summary(stats(Pass,Fail)) :-
    nl,
    write('Summary: '),
    Total is Pass + Fail,
    write_ratio(Fail/Total), write(' fail, '),
    write_ratio(Pass/Total), write(' pass'),
    nl.

run_modules([], GlobalStats, GlobalStats).
run_modules([Module|Modules], GlobalStats0, GlobalStats) :-
    \+ ignored_module(Module),
    current_predicate(Module:test/2),
    !,
    write('### Module: '), write(Module), nl,
    findall(test(Name,Goals), Module:test(Name,Goals), Tests),
    run_all_tests(Tests, Module, GlobalStats0, GlobalStats1),
    run_modules(Modules, GlobalStats1, GlobalStats).
run_modules([_|Modules], GlobalStats0, GlobalStats) :-
    run_modules(Modules, GlobalStats0, GlobalStats).

%   This program.
ignored_module(crisp).

%   SWI-Prolog 5.10.2.
ignored_module(error).
ignored_module(license).
ignored_module(link_xpce).
ignored_module(lists).
ignored_module(make).
ignored_module(pce_swi_hooks).
ignored_module(prolog).
ignored_module(swi_option).
ignored_module(swi_system_utilities).
ignored_module(system).
ignored_module(toplevel_variables).

%   SICStus 3.12.5.
ignored_module(clpfd).
ignored_module(prolog).
ignored_module('SU_messages').

run_all_tests([], Module, GlobalStats, GlobalStats) :-
    !,
    write('No test(Name, Goals) predicates found.'), nl,
    run_all_tests_aux([], Module, GlobalStats, GlobalStats).
run_all_tests(Tests, Module, GlobalStats0, GlobalStats) :-
    run_all_tests_aux(Tests, Module, GlobalStats0, GlobalStats).

run_all_tests_aux([], _Module, GlobalStats, GlobalStats).
run_all_tests_aux([test(Name,Goals)|Tests], Module, GlobalStats0, GlobalStats) :-
    write_test_name(Name),
    run_goals(Goals, Module, TestStats),
    write_stats(TestStats),
    update_global_stats(GlobalStats0, TestStats, GlobalStats1),
    run_all_tests_aux(Tests, Module, GlobalStats1, GlobalStats).

write_test_name(Pred/Arity) :-
    !,
    write('- '), write(Pred/Arity).
write_test_name(Name) :-
    write('- \''), write(Name), write('\'').

run_goals(Goals, Module, TestStats) :-
    run_goals(Goals, Module, stats(0,0), TestStats).

update_global_stats(
        stats(Pass0,Fail0), stats(TestPass,TestFail), stats(Pass,Fail)) :-
    Pass is Pass0 + TestPass,
    Fail is Fail0 + TestFail.

write_stats(stats(Pass,0)) :-
    !,
    write(' => '), write_pass(stats(Pass,0)), nl.
write_stats(Stats) :-
    nl,
    write('        => '), write_fail(Stats),
    write(', '), write_pass(Stats), nl.

write_pass(stats(Pass,Fail)) :-
    Total is Pass + Fail,
    write_ratio(Pass/Total), write(' pass').

write_fail(stats(Pass,Fail)) :-
    Total is Pass + Fail,
    write_ratio(Fail/Total), write(' fail').

write_ratio(0/All) :-
    !,
    write(none/All).
write_ratio(All/All) :-
    !,
    write(all/All).
write_ratio(Some/All) :-
    !,
    write(Some/All).

run_goals([], _Module, Stats, Stats).
run_goals([true|Goals], Module, Stats0, Stats) :-
    !,
    run_goals(Goals, Module, Stats0, Stats).
run_goals([one:Goal|Goals], Module, Stats0, Stats) :-
    !,
    execute_det_test(Goal, Module, Result),
    write_result(one:Goal, Result),
    update_stats(Result, Stats0, Stats1),
    run_goals(Goals, Module, Stats1, Stats).
run_goals([fail:Goal|Goals], Module, Stats0, Stats) :-
    !,
    execute_test(\+(Goal), Module, Result),
    write_result(fail:Goal, Result),
    update_stats(Result, Stats0, Stats1),
    run_goals(Goals, Module, Stats1, Stats).
run_goals([Goal|Goals], Module, Stats0, Stats) :-
    % Goal has no special form.
    execute_test(Goal, Module, Result),
    write_result(Goal, Result),
    update_stats(Result, Stats0, Stats1),
    run_goals(Goals, Module, Stats1, Stats).

execute_det_test(Goal, Module, pass) :-
    findall(_, Module:Goal, [_OnlyOne]),
    !.
execute_det_test(_Goal, _Module, fail).

execute_test(Goal, Module, pass) :-
    Module:call(Goal),
    !.
execute_test(_Goal, _Module, fail).
    % \+ Module:call(Goal).

write_result(_Goal, pass) :- !.
write_result(Goal, fail) :-
    nl,
    write('    >>> FAIL: '), write(Goal).

update_stats(pass, stats(Pass0,Fail), stats(Pass,Fail)) :-
    Pass is Pass0 + 1.
update_stats(fail, stats(Pass,Fail0), stats(Pass,Fail)) :-
    Fail is Fail0 + 1.

write_prologue :-
    crisp_version(Version),
    write('Crisp '), write_version_list(Version), nl,
    nl.

write_version_list([X]) :-
    !,
    write(X).
write_version_list([X|Xs]) :-
    write(X), write('.'),
    write_version_list(Xs).

write_epilogue :-
    nl.

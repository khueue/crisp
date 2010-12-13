% Crisp
%
% MIT License (http://www.opensource.org/licenses/mit-license.php)
% Copyright (c) 2009, khueue.

:- module(crisp, [crisp/0, version/1]).

%%  version(?VersionAsList)
%
%   True if VersionAsList is [Major,Minor,Patch].

version([0,0,1]).

%%  crisp
%
%   Processes all modules (except those that seem to belong to the
%   environment, see ignored_module/1) in alphabetical order, and runs
%   all test/2 predicates it finds. Prints test results on the fly.

crisp :-
    write_prologue,
    collect_and_run,
    write_epilogue.

collect_and_run :-
    setof(Module, current_module(Module), Modules),
    run_all_modules(Modules, stats(0,0), GlobalStats),
    write_summary(GlobalStats).

write_summary(stats(Pass,Fail)) :-
    nl,
    write('Summary: '),
    Total is Pass + Fail,
    write_ratio(Fail/Total), write(' fail, '),
    write_ratio(Pass/Total), write(' pass'),
    nl.

run_all_modules([], GlobalStats, GlobalStats).
run_all_modules([Module|Modules], GlobalStats0, GlobalStats) :-
    suitable_module(Module),
    !,
    run_module(Module, GlobalStats0, GlobalStats1),
    run_all_modules(Modules, GlobalStats1, GlobalStats).
run_all_modules([_Module|Modules], GlobalStats0, GlobalStats) :-
    % \+ suitable_module(Module),
    run_all_modules(Modules, GlobalStats0, GlobalStats).

suitable_module(Module) :-
    \+ ignored_module(Module),
    current_predicate(Module:test/2).

run_module(Module, Stats0, Stats) :-
    write('### Module: '), write(Module), nl,
    findall(test(Name,Goals), Module:test(Name,Goals), Tests),
    run_all_tests(Tests, Module, Stats0, Stats).

% This module.
ignored_module(crisp).
% SWI-Prolog 5.10.2.
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
% SICStus 3.12.5.
ignored_module(clpfd).
ignored_module(prolog).
ignored_module('SU_messages').

run_all_tests([], _Module, GlobalStats, GlobalStats).
run_all_tests([test(Name,Goals)|Tests], Module, GlobalStats0, GlobalStats) :-
    run_test(test(Name,Goals), Module, TestStats),
    update_global_stats(GlobalStats0, TestStats, GlobalStats1),
    run_all_tests(Tests, Module, GlobalStats1, GlobalStats).

run_test(test(Name,Goals), Module, TestStats) :-
    write_test_name(Name),
    run_goals(Goals, Module, TestStats),
    write_stats(TestStats).

write_test_name(Pred/Arity) :-
    !,
    write('- '), write(Pred/Arity).
write_test_name(Name) :-
    write('- \''), write(Name), write('\'').

run_goals(Goals, Module, TestStats) :-
    run_goals(Goals, Module, stats(0,0), TestStats).

update_global_stats(stats(P0,F0), stats(P1,F1), stats(Pass,Fail)) :-
    Pass is P0 + P1,
    Fail is F0 + F1.

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
    execute_deterministic_test(Goal, Module, Result),
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

execute_deterministic_test(Goal, Module, pass) :-
    findall(_, Module:Goal, [_ExactlyOneSolution]),
    !.
execute_deterministic_test(_Goal, _Module, fail).
    % Not exactly one solution.

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
    version(Version),
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

%   Crisp - Crazy Simple Unit Testing in Prolog
%   https://github.com/khueue/
%
%   Todo:
%   - Numbers for failing tests? In lack of line number indication.
%   - Look into copy_term/2 for solving the problem of tying up all
%     variables in a goal list. Or is that good behavior?
%   - Redisplay failing tests at end of report.
%   - Run only certain tests.

:- module(crisp, [crisp/0, crisp_version/1]).

%%  crisp_version(?VersionAsList)
%
%   True if VersionAsList is [Major,Minor,Patch].

crisp_version([0,0,1]).

%%  crisp
%
%   Processes all modules in alphabetical order (except those that seem
%   to belong to the environment, see ignored_module/1), and runs all
%   describe/2 predicates it finds. Prints test results on the fly.

crisp :-
    write_prologue,
    collect_and_run,
    write_epilogue.

collect_and_run :-
    all_modules(Modules),
    run_all_modules(Modules, GlobalStats),
    write_summary(GlobalStats).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modules.

all_modules(Modules) :-
    setof(Module, current_module(Module), Modules).

run_all_modules(Modules, GlobalStats) :-
    run_all_modules(Modules, stats(0,0), GlobalStats).

run_all_modules([], Stats, Stats).
run_all_modules([Module|Modules], Stats0, Stats) :-
    suitable_module(Module),
    !,
    before_module,
    run_module(Module, Stats0, Stats1),
    after_module,
    run_all_modules(Modules, Stats1, Stats).
run_all_modules([_Module|Modules], Stats0, Stats) :-
    % \+ suitable_module(Module),
    run_all_modules(Modules, Stats0, Stats).

suitable_module(Module) :-
    \+ ignored_module(Module),
    current_predicate(Module:describe/2). % Some describe/2 exists.

before_module.

after_module.

run_module(Module, Stats0, Stats) :-
    write_module_header(Module),
    all_tests_in_module(Module, Tests),
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
% SICStus Prolog 3.12.5.
ignored_module(clpfd).
ignored_module(prolog).
ignored_module('SU_messages').
% YAP Prolog 6.2.0.
ignored_module(dialect).
ignored_module(autoloader).
ignored_module(lists).
ignored_module(swi).
ignored_module(operating_system_support).
ignored_module(nb).
ignored_module(arg).
ignored_module(yap_hacks).
ignored_module(readutil).
ignored_module(system).
ignored_module(terms).
ignored_module(charsio).
ignored_module(attributes).
ignored_module(idb).
ignored_module(prolog).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests within module.

all_tests_in_module(Module, Tests) :-
    findall(describe(Name,Goals), Module:describe(Name,Goals), Tests).

run_all_tests([], _Module, Stats, Stats) :-
    !,
    write('- No describe/2 found.'), nl.
run_all_tests(Tests, Module, Stats0, Stats) :-
    run_all_tests_aux(Tests, Module, Stats0, Stats).

run_all_tests_aux([], _Module, Stats, Stats).
run_all_tests_aux([describe(Name,Goals)|Tests], Module, Stats0, Stats) :-
    before_test,
    run_test(describe(Name,Goals), Module, TestStats),
    add_stats(Stats0, TestStats, Stats1),
    after_test,
    run_all_tests_aux(Tests, Module, Stats1, Stats).

run_test(describe(Name,Goals), Module, TestStats) :-
    write_test_name(Name),
    run_all_goals(Goals, Module, TestStats),
    write_stats(TestStats).

add_stats(stats(P0,F0), stats(P1,F1), stats(Pass,Fail)) :-
    Pass is P0 + P1,
    Fail is F0 + F1.

before_test.

after_test.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Goals within test.

run_all_goals(Goals, Module, Stats) :-
    run_all_goals(Goals, Module, stats(0,0), Stats).

run_all_goals([], _Module, Stats, Stats).
run_all_goals([true|Goals], Module, Stats0, Stats) :-
    !,
    run_all_goals(Goals, Module, Stats0, Stats).
run_all_goals([one:Goal|Goals], Module, Stats0, Stats) :-
    !,
    execute_deterministic_goal(Goal, Module, Result),
    write_result(Result, one:Goal),
    update_stats(Result, Stats0, Stats1),
    run_all_goals(Goals, Module, Stats1, Stats).
run_all_goals([fail:Goal|Goals], Module, Stats0, Stats) :-
    !,
    execute_goal(\+((Goal)), Module, Result),
    write_result(Result, fail:Goal),
    update_stats(Result, Stats0, Stats1),
    run_all_goals(Goals, Module, Stats1, Stats).
run_all_goals([Goal|Goals], Module, Stats0, Stats) :-
    % Goal has no special form.
    execute_goal(Goal, Module, Result),
    write_result(Result, Goal),
    update_stats(Result, Stats0, Stats1),
    run_all_goals(Goals, Module, Stats1, Stats).

execute_deterministic_goal(Goal, Module, pass) :-
    findall(_, Module:call(Goal), [_ExactlyOneSolution]),
    executes_deterministically(Module:Goal),
    !.
execute_deterministic_goal(_Goal, _Module, fail).
    % Not exactly one solution.

executes_deterministically(Module:Goal) :-
    call_cleanup(Module:call(Goal), NoChoicePointsLeft = true),
    !, % So that it does not backtrack into something deterministic.
    NoChoicePointsLeft == true.

execute_goal(Goal, Module, pass) :-
    Module:call(Goal),
    !.
execute_goal(_Goal, _Module, fail).
    % \+ Module:call(Goal).

update_stats(pass, stats(Pass0,F), stats(Pass,F)) :-
    Pass is Pass0 + 1.
update_stats(fail, stats(P,Fail0), stats(P,Fail)) :-
    Fail is Fail0 + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printing routines.

write_module_header(Module) :-
    nl,
    write('Module: '), write(Module), nl.

write_test_name(Pred/Arity) :-
    !,
    write('- '), write(Pred/Arity).
write_test_name(Name) :-
    write('- \''), write(Name), write('\'').

write_stats(stats(Pass,0)) :-
    !,
    write(' => '), write_pass(stats(Pass,0)), nl.
write_stats(Stats) :-
    % At least one test failed.
    nl,
    write('  => '), write_fail(Stats),
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
    write(Some/All).

write_result(pass, _Goal).
write_result(fail, Goal) :-
    nl,
    write('  !!! FAIL: '), write(Goal).

write_summary(stats(Pass,Fail)) :-
    nl,
    write('Summary: '),
    Total is Pass + Fail,
    write_ratio(Fail/Total), write(' fail, '),
    write_ratio(Pass/Total), write(' pass'),
    nl.

write_prologue :-
    crisp_version(Version),
    write('Crisp '), write_version_list(Version), nl.

write_version_list([X]) :-
    !,
    write(X).
write_version_list([X|Xs]) :-
    write(X), write('.'),
    write_version_list(Xs).

write_epilogue.

/*

% Think long and hard about this:

tuple_to_list(Tuple, [Head|Elements]) :-
    Tuple =.. [',',Head,Tail],
    !,
    tuple_to_list(Tail, Elements).
tuple_to_list(Element, [Element]).
    % Element is not a tuple.

*/
/*

all_tests_in_module(Module, Tests) :-
    findall(
        describe(Name,Goals),
        all_goals_in_test(Module, Name, Goals),
        Tests).

all_goals_in_test(Module, Name, Goals) :-
    clause(Module:describe(Name), Tuple),
    tuple_to_list(Tuple, Goals).

*/

/** <module> 
% A MUD testing API is defined here
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(moo_testing,
	[run_mud_tests/0,
        run_mud_test/2,
        test_name/1,
        test_true/1,
        test_false/1,
        last_test_name/1,
        run_mud_test_code/1]).

:- dynamic(was_test_name/1).
:- meta_predicate run_mud_test_code(0).
:- meta_predicate run_mud_test(*,0).
:- meta_predicate test_true(0).
:- meta_predicate test_false(0).


:- include(logicmoo(vworld/moo_header)).
:- moodb:register_module_type(utility).

% do some sanity testing (expects the startrek world is loaded)
run_mud_tests:-
  forall(moo:mud_test(Name,Test),run_mud_test(Name,Test)).

moo:action_help(tests,"run run_mud_tests/0").

moo:agent_call_command(_Agent,tests) :- scan_updates, run_mud_tests.


moo:action_help(test(term),"run tests containing term").

moo:agent_call_command(_Gent,test(Obj)):- term_test(Obj).

test_name(String):-dmsg(moo_test(named(String))),retractall(was_test_name(_)),asserta(was_test_name(String)).
last_test_name(String):-was_test_name(String),!.
last_test_name(unknown).
test_true(SomeGoal):- SomeGoal; (last_test_name(String),dmsg(moo_test(failed(String:SomeGoal)))).
test_false(SomeGoal):- not(SomeGoal); (last_test_name(String),dmsg(moo_test(failed(String:SomeGoal)))).

term_test(Obj):-
   doall((
   moo:mud_test(H,B),
   use_term_listing(Obj,moo:mud_test(H,B)),
   moo:mud_test(H,B),
   fail)).

run_mud_test(Name,Test):-
   dmsg(tests(run_mud_test(Name))),
   once(catch((run_mud_test_code(Test),dmsg(tests(passed_mud_test(Name)))),E,dmsg(tests(fail_mud_test(E, Name))));dmsg(tests(fail_mud_test(Name)))).

% TODO (not use call/1)
run_mud_test_code(Test):-call(Test).




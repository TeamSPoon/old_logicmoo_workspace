/** <module> 
% A MUD testing API is defined here
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(moo_testing,
	[run_mud_tests/0,
        run_mud_test/2,
        test_name/1,
        test_true/1,
        term_test/1,
        test_false/1,
        last_test_name/1,
        test_call/1]).

:- thread_local was_test_name/1.

:- meta_predicate_transparent test_call(^).
:- meta_predicate_transparent run_mud_test(?,^).
:- meta_predicate_transparent test_true(^).
:- meta_predicate_transparent test_false(^).
:- meta_predicate_transparent term_test(^).
:- meta_predicate_transparent run_mud_tests/0.


:- include(logicmoo(vworld/moo_header)).
:- moo:register_module_type(utility).

% do some sanity testing (expects the startrek world is loaded)
run_mud_tests:-
  forall(moo:mud_test(Name,Test),run_mud_test(Name,Test)).

moo:action_info(tests,"run run_mud_tests/0").

moo:agent_call_command(_Agent,tests) :- scan_updates, run_mud_tests.


moo:action_info(test(term),"run tests containing term").

moo:agent_call_command(Agent,test(Obj)):-foc_current_player(Agent),term_test(Obj).


test_name(String):-fmt(start_moo_test(named(String))),asserta(was_test_name(String)).
last_test_name(String):- was_test_name(String),!.
last_test_name(unknown).

test_result(Result):-test_result(Result,true).
test_result(Result,SomeGoal):- last_test_name(String),fmt(Result:test_mini_result(Result:String,SomeGoal)).

from_here(_:SomeGoal):-!,functor(SomeGoal,F,_),atom_concat('test',_,F).
from_here(SomeGoal):-!,functor(SomeGoal,F,_),atom_concat('test',_,F).



test_call(X):- var(X),!, throw(var(test_call(X))).
test_call(meta_callable(String,test_name(String))):-!,string(String).
test_call(meta_call(X)):- !,test_call0(X).
test_call(Goal):- meta_interp(test_call,Goal).

test_call0(SomeGoal):- from_here(SomeGoal),!,call_expanded(SomeGoal).
test_call0(SomeGoal):- dmsg(call_expanded(SomeGoal)), catch(SomeGoal,E,(test_result(error(E),SomeGoal),!,fail)).

test_true(SomeGoal):-  once(((test_call(SomeGoal),!,test_result(passed,SomeGoal));test_result(failed,SomeGoal))).
test_false(SomeGoal):- test_true(not(SomeGoal)).

term_test(Obj):-
   doall((
   moo:mud_test(H,B),
   once(use_term_listing(Obj,((H,B)),true)),
   once(run_mud_test(H,B)),
   fail)).

run_mud_test(Name,Test):-
   fmt(begin_mud_test(Name)),
   once(ccatch((test_call(Test),fmt(completed_mud_test(Name))),E,fmt(error_mud_test(E, Name)));fmt(tests(incomplet_mud_test(Name)))).

%:- module_predicates_are_exported.

% :- module_meta_predicates_are_transparent(moo_testing).


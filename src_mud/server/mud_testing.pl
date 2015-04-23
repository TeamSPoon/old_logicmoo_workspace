/** <module> 
% A MUD testing API is defined here
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:-swi_module(moo_testing,
	[run_mud_tests/0,
        run_mud_test/2,
        test_name/1,
        test_true/1,
        run_mud_test/1,
        test_false/1,
        last_test_name/1,
        test_call/1]).

:- thread_local was_test_name/1.

:- meta_predicate_transparent test_call(+).
:- meta_predicate_transparent run_mud_test(+,+).
:- meta_predicate_transparent test_true(+).
:- meta_predicate_transparent test_true_req(+).
:- meta_predicate_transparent test_false(+).
:- meta_predicate_transparent run_mud_test(+).

% :- trace,leash(+all),meta_predicate_transparent run_mud_tests().

:- include(prologmud(mud_header)).
% :- register_module_type (utility).

% do some sanity testing (expects the startrek world is loaded)
run_mud_tests:-
  forall(mud_test(Name,Test),run_mud_test(Name,Test)).

user:action_info(actTests,"run run_mud_tests").

user:agent_call_command(_Agent,actTests) :- scan_updates, run_mud_tests.


user:action_info(actTest(ftTerm),"run tests containing term").

user:agent_call_command(Agent,actTest(Obj)):-foc_current_player(Agent),run_mud_test(Obj).


test_name(String):-fmt(start_moo_test(mudNamed(String))),asserta(was_test_name(String)).
last_test_name(String):- was_test_name(String),!.
last_test_name(unknown).

test_result(Result):-test_result(Result,true).

test_result(Result,SomeGoal):- last_test_name(String),fmt(Result:test_mini_result(Result:String,SomeGoal)).

from_here(_:SomeGoal):-!,functor(SomeGoal,F,_),atom_concat(actTest,_,F).
from_here(SomeGoal):-!,functor(SomeGoal,F,_),atom_concat(actTest,_,F).

test_call(X):- var(X),!, throw(var(test_call(X))).
test_call(meta_callable(String,test_name(String))):-!,string(String).
test_call(meta_call(X)):- show_call(test_call0(X)).
test_call(Goal):-  meta_interp(test_call,Goal).

test_call0(SomeGoal):- from_here(SomeGoal),!,req(SomeGoal).
test_call0(SomeGoal):- dmsg(req(SomeGoal)), catch(SomeGoal,E,(test_result(error(E),SomeGoal),!,fail)).

test_true_req(Req):- test_true(req(Req)).
test_true(SomeGoal):- test_call(SomeGoal) *-> test_result(passed,SomeGoal) ;  test_result(failed,SomeGoal).
test_false(SomeGoal):- test_true(not(SomeGoal)).

run_mud_test(Filter):-
   doall((
   member(F/A,[mud_test/0,mud_test/1,mud_test/2]),   
   current_predicate(M:F/A),
   functor(H,F,A),
   not(predicate_property(M:H,imported_from(_))),
   clause(M:H,B),
   use_term_listing(Filter,M:H,B),
   once(run_mud_test_clause(M:H,B)),
   fail)).

run_mud_test_clause(M:mud_test,B):- must((contains_term(test_name(Name),nonvar(Name)))),!, M:run_mud_test(Name,B).
run_mud_test_clause(M:mud_test(Name),B):- !, M:run_mud_test(Name,B).
run_mud_test_clause(M:mud_test(Name,Test),B):- forall(B,M:run_mud_test(Name,Test)).

run_mud_test(Name,Test):-
   fmt(begin_mud_test(Name)),
   once(ccatch((test_call(Test),fmt(completed_mud_test(Name))),E,fmt(error_mud_test(E, Name)));fmt(actTests(incomplet_mud_test(Name)))).

:- module_predicates_are_exported.

:- module_meta_predicates_are_transparent(moo_testing).

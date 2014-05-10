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
        run_mud_test_code/1]).

:- meta_predicate moo_testing:run_mud_test_code(0).
:- meta_predicate moo_testing:run_mud_test(*,0).


% do some sanity testing (expects the startrek world is loaded)
run_mud_tests:-
  forall(moo:decl_mud_test(Name,Test),run_mud_test(Name,Test)).

moo:decl_action(tests,"run run_mud_tests/0").

moo:agent_call_command(_Agent,tests) :- make, run_mud_tests.


moo:decl_action(test(term),"run tests containing term").

moo:agent_call_command(_Gent,test(Obj)):- term_test(Obj).

term_test(Obj):-
   doall((
   moo:decl_mud_test(H,B),
   use_term_listing(Obj,moo:decl_mud_test(H,B)),
   moo:decl_mud_test(H,B),
   fail)).

run_mud_test(Name,Test):-
   dmsg(tests(run_mud_test(Name))),
   once(catch((run_mud_test_code(Test),dmsg(tests(passed_mud_test(Name)))),E,dmsg(tests(fail_mud_test(E, Name))));dmsg(tests(fail_mud_test(Name)))).

% TODO (not use call/1)
run_mud_test_code(Test):-call(Test).




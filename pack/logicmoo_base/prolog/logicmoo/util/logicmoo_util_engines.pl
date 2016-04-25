% ===================================================================
% File 'logicmoo_util_engines.pl'
% Purpose: An Implementation in SWI-Prolog of Unwindable context frames
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_ctx_frame.pl' 1.1.1
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2035/12/16 01:57:18 $
% ===================================================================
% ===================================================================
%  
%   Run goals separately 
%

:- module(logicmoo_util_engines,
  [ result_check/3,
    on_diff_fail/2,
    on_diff_throw/2,
    call_diff/3,
    shared_vars/3,
    next_solution/0,    
    call_goal_in_thread/1
  ]).

result_check(Call,N+NVs,O+OVs):-
   NVs=@=OVs -> true; Call.


/*

  % I'd like these two to run at the same time (Did I really 
  need to start a thread with send_messages)
 
 ?- on_diff_fail(member(X,[1,2,3]),member(X,[1,2,4])). 
   X = 1 ;
   X = 2 ;
   No.
*/

on_diff_fail(N,O):- 
  call_diff(result_check(fail),N,O).

on_diff_throw(N,O):- 
  call_diff(result_check(trace_or_throw(different(N,O))),N,O).

call_diff(Check, N,O):- 
   shared_vars(N,O,Vs),
 % O must be able to diverge
   copy_term(O+Vs,CO+CVs),
 % Later on we''ll allow different result orders
   NOL = saved_in([],[]), 
   collecting_list(call(CO),CVs,1,NOL),
   collecting_list(call(N),Vs,2,NOL),
   call(Check,N+Vs,O+CVs).

collecting_list(G,Vs,At,S):- 
   call(G),copy_term(Vs,CVs),
   arg(At,S,Was),nb_setarg(At,S,[CVs|Was]).


:- nb_setval(query_result,sol(0,1,false,false)).

shared_vars(S9,G9,SVG):-notrace(( term_variables(S9,Vs1),term_variables(G9,Vs2),intersect_eq0(Vs2,Vs1,SVG))).

% sol(number,G,successfull,done)
next_solution:- notrace(next_solution(How)),call(How).

next_solution(throw(no_query_result)) :- \+ nb_current(query_result,_),!.
next_solution(request_next0) :- nb_getval(query_result,sol(_,_,true,false)),!.
next_solution(nop(last(G))) :- nb_getval(query_result,sol(_,G,true,true)),!.
next_solution(request_next0) :- nb_getval(query_result,sol(_,_,_,false)),!.
next_solution(nop(unknown(QR))) :- nb_getval(query_result,QR),!.

request_next0 :- 
  notrace((thread_send_message(ask1,please(next_sol)), !, 
  thread_get_message(answer1,M),w(rn(M)),nb_setval(query_result,M))),!.

call_goal_in_thread(G):- 
  once((nb_setval(in,v(_,G,_)), 
    start_goal_saved1)),fail.
call_goal_in_thread(G):- next_solution, get_sol(G).


get_sol(G):-
  repeat,
  next_solution,
  nb_getval(query_result,M),
  react_message(M,G,(ReactA,ReactB)),
  wdmsg(M),
  (ReactA == ! -> ! ; ReactA),
   ReactB.

react_message(sol(_,_,unknown,false),_,(true,fail)):-!.
react_message(sol(_,_,false,_),_,(!,fail)):- !.
react_message(sol(_,G,true,false),G,(true,true)):-!.
react_message(sol(_,G,true,true),G,(!,true)):- !.


call_goal_in_thread_saved_nd:- start_goal_saved1, fail.
call_goal_in_thread_saved_nd:- next_solution.

memberchk_eq0(X, [Y|Ys]) :- X==Y;memberchk_eq0(X,Ys).

intersect_eq0([], _, []).
intersect_eq0([X|Xs], Ys, L) :-
 	(   memberchk_eq0(X, Ys)
 	->  L = [X|T],
 	    intersect_eq0(Xs, Ys, T)
 	;   intersect_eq0(Xs, Ys, L)
 	).



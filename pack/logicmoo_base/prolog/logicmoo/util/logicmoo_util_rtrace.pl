/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_rtrace.pl
:- module(logicmoo_util_rtrace,
   [
      gftrace/0,
      ggtrace/0,
      grtrace/0,
      hotrace/0,
      nortrace/0,
      pop_tracer/0,
      push_tracer/0,
      push_tracer_and_notrace/0,
      restore_guitracer/0,
      rtrace/0,
      stop_rtrace/0,
      start_rtrace/0,
      save_guitracer/0,
      reset_tracer/0,
      cnotrace/1,
      cnotrace/0,
      fixhotrace/1,
      gftrace/1,
      grtrace/1,
      ggtrace/1,
      rtrace/1,  % dtrace why choice points are left over
      ftrace/1, % tells why a call didn't succeed once
      restore_trace/1,
      on_x_rtrace/1,
      hotrace/1,
      thread_leash/1      
    ]).

:- meta_predicate
	restore_trace(0),
        on_x_rtrace(0),
	cnotrace(0),
        fixhotrace(0),
        ftrace(0),        
        gftrace(0),
        ggtrace(0),
        grtrace(0),
	rtrace(0),
	hotrace(0).

:- module_transparent
      hotrace/0,
      hotrace/1,
      nortrace/0,
      pop_tracer/0,
      push_tracer/0,
      push_tracer_and_notrace/0,
      reset_tracer/0,
      restore_guitracer/0,
      rtrace/0,      
      save_guitracer/0.

:- include('logicmoo_util_header.pi').



%= 	 	 

%% ggtrace is semidet.
%
% Gg Trace.
%
ggtrace:- default_dumptrace(DDT), ggtrace(DDT).

%= 	 	 

%% ggtrace( :GoalTrace) is semidet.
%
% Gg Trace.
%
ggtrace(Trace):- 
   thread_leash(-call),((visible(+all),visible(-unify),visible(+exception),
   thread_leash(-all),thread_leash(+exception),
   thread_leash(+call))),Trace,hotrace(thread_leash(-call)).


%= 	 	 

%% gftrace is semidet.
%
% Gf Trace.
%
gftrace:- default_dumptrace(DDT), gftrace(DDT).

%= 	 	 

%% gftrace( :GoalTrace) is semidet.
%
% Gf Trace.
%
gftrace(Trace):- 
   thread_leash(-call),((visible(-all), visible(+fail),visible(+call),visible(+exception),
   thread_leash(-all),thread_leash(+exception),
   thread_leash(+call))),Trace,thread_leash(-call).


%= 	 	 

%% grtrace is semidet.
%
% Gr Trace.
%
grtrace:- default_dumptrace(DDT), grtrace(DDT).

%= 	 	 

%% grtrace( :GoalTrace) is semidet.
%
% Gr Trace.
%
grtrace(Trace):- hotrace(( visible(+all),thread_leash(+all))), Trace.



%= 	 	 

%% cnotrace( :GoalGoal) is semidet.
%
% Cno Trace.
%
cnotrace(Goal):- once(Goal).
cnotrace:- notrace.
%:- mpred_trace_less(hotrace/1).
%:- '$set_predicate_attribute'(hotrace(_), hide_childs, 0).
%:- '$set_predicate_attribute'(hotrace(_), trace, 1).




%% hotrace is nondet.
%
% Unlike notrace/1, it allows traceing when excpetions are raised during Goal.
%
hotrace:-!.
hotrace:-notrace.
%:- export(hotrace/1).
%% = :- meta_predicate(hotrace(0)).



%% thread_leash( +Flags) is det.
%
% Only leashs the main thread
%
%thread_leash(-Some):- thread_self(main)->leash(-Some);true.
%thread_leash(+Some):- thread_self(main)->leash(+Some);true.
thread_leash(Some):- thread_self(main)->leash(Some);true.

:- meta_predicate hotrace(0).

hotrace(Goal):-!,call(Goal).
hotrace(Goal):-
   ((tracing,notrace )-> Tracing = trace ;   Tracing = true),
   '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   (Undo =   notrace(((notrace,'$leash'(_, OldL),'$visible'(_, OldV), Tracing)))),
   (RTRACE = notrace((visible(-all),visible(+exception),thread_leash(-all),thread_leash(+exception)))),!,
   setup_call_cleanup_each(RTRACE,(notrace,Goal),Undo).


% :- trace(hotrace/1, -all).       
% hotrace(Goal):- get_hotrace(Goal,Y),Y.
%:- mpred_trace_less(hotrace/1).
%:- '$set_predicate_attribute'(hotrace(_), hide_childs, 1).
%:- '$set_predicate_attribute'(hotrace(_), trace, 0).


:- thread_local(tlbugger:rtracing/0).


% =========================================================================

 
:-thread_local(t_l:wasguitracer/1).
:-thread_local(t_l:wastracer/1).

%= 	 	 

%% save_guitracer is semidet.
%
% Save Guitracer.
%
save_guitracer:- ignore(((current_prolog_flag(gui_tracer, GWas);GWas=false),asserta(t_l:wasguitracer(GWas)))).

%= 	 	 

%% restore_guitracer is semidet.
%
% Restore Guitracer.
%
restore_guitracer:- ignore((retract(t_l:wasguitracer(GWas)),set_prolog_flag(gui_tracer, GWas))).


%= 	 	 

%% rtrace is semidet.
%
% R Trace.
%
rtrace:- notrace,start_rtrace,trace. % save_guitracer,noguitracer

start_rtrace:- assert_if_new(tlbugger:rtracing),visible(+all),visible(+exception),thread_leash(-all),thread_leash(+exception).



%% nortrace is semidet.
%
% Nor Trace.
%
nortrace:- notrace((\+ tlbugger:rtracing)),!.
nortrace:- notrace,stop_rtrace.

stop_rtrace:- retractall(tlbugger:rtracing),visible(+all),visible(+exception),thread_leash(+all),thread_leash(+exception).

push_tracer_and_notrace:- notrace((push_tracer,notrace)).
   
%= 	 	 

%% push_tracer is semidet.
%
% Push Tracer.
%
push_tracer:- hotrace((get_tracer(Reset),asserta(t_l:wastracer(Reset)))),!.

%= 	 	 

%% pop_tracer is semidet.
%
% Pop Tracer.
%
pop_tracer:- hotrace((retract(t_l:wastracer(Reset)),!,Reset)),!.

%= 	 	 

%% reset_tracer is semidet.
%
% Reset Tracer.
%
reset_tracer:- hotrace((ignore(((t_l:wastracer(Reset),Reset))))).
 

%= 	 	 

%% get_tracer( ?Reset) is semidet.
%
% Get Tracer.
%
get_tracer(Reset):- 
    Reset =  ((set_prolog_flag(debug,WasDebug),CC,CC2,'$leash'(_, OldL),'$visible'(_, OldV))),
    '$leash'(OldL, OldL),'$visible'(OldV, OldV),
     (current_prolog_flag(debug,true)->WasDebug=true;WasDebug=false),
     (tlbugger:rtracing->CC2=rtrace;CC2= nortrace),
     (tracing -> CC = dtrace ; CC = hotrace),!.

    

%= :- meta_predicate  restore_trace(0).

%= 	 	 

%% restore_trace( :GoalGoal) is semidet.
%
% restore  Trace.
%
restore_trace(Goal):- 
  setup_call_cleanup_each(push_tracer,Goal,pop_tracer).


%= :- meta_predicate  rtrace(0).


%= 	 	 

%% rtrace( :GoalGoal) is semidet.
%
% R Trace.
%
% rtrace(Goal):- hotrace(tlbugger:rtracing),!, Goal.

% rtrace(Goal):- wdmsg(rtrace(Goal)),!, restore_trace(setup_call_cleanup_each(rtrace,(trace,Goal),nortrace)).

% rtrace(Goal):- notrace(tlbugger:rtracing),!,call(Goal).
rtrace(Goal):- !,setup_call_cleanup(start_rtrace,call((rtrace,Goal)),stop_rtrace).
rtrace(Goal):- tracing,!,setup_call_cleanup(start_rtrace,call(Goal),stop_rtrace).
rtrace(Goal):- \+ tracing,start_rtrace,!,setup_call_cleanup(trace,call(Goal),(notrace,stop_rtrace)).
rtrace(Goal):- 
  ((tracing,notrace )-> Tracing = trace ;   Tracing = true),
   '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   wdmsg(rtrace(Goal)),
   (Undo =   (((notrace,ignore(retract(tlbugger:rtracing)),'$leash'(_, OldL),'$visible'(_, OldV), Tracing)))),
   (RTRACE = ((notrace,asserta(tlbugger:rtracing),visible(+all),thread_leash(-all),thread_leash(+exception),trace))),!,
   setup_call_cleanup_each(RTRACE,(trace,Goal),Undo).
/*
:- '$set_predicate_attribute'(system:call_cleanup(_,_), trace, 0).
:- '$set_predicate_attribute'(system:call_cleanup(_,_), hide_childs, 1).
:- '$set_predicate_attribute'(system:setup_call_cleanup(_,_,_), trace, 0).
:- '$set_predicate_attribute'(system:setup_call_cleanup(_,_,_), hide_childs, 1).
:- '$set_predicate_attribute'(system:setup_call_catcher_cleanup(_,_,_,_), trace, 0).
:- '$set_predicate_attribute'(system:setup_call_catcher_cleanup(_,_,_,_), hide_childs, 1).
*/
:- '$set_predicate_attribute'(hotrace(_), trace, 0).
:- '$set_predicate_attribute'(hotrace(_), hide_childs, 1).
:- '$set_predicate_attribute'(rtrace(_), trace, 1).
:- '$set_predicate_attribute'(rtrace(_), hide_childs, 0).
:- '$set_predicate_attribute'(rtrace, trace, 0).
:- '$set_predicate_attribute'(rtrace, hide_childs, 1).
:- '$set_predicate_attribute'(nortrace, trace, 0).
:- '$set_predicate_attribute'(nortrace, hide_childs, 1).
:- '$set_predicate_attribute'(pop_tracer, trace, 0).
:- '$set_predicate_attribute'(pop_tracer, hide_childs, 1).
:- '$set_predicate_attribute'(tlbugger:rtracing, trace, 0).
:- '$set_predicate_attribute'(system:tracing, trace, 0).
:- '$set_predicate_attribute'(system:hotrace, trace, 0).
:- '$set_predicate_attribute'(system:trace, trace, 0).
%= :- meta_predicate  ftrace(0).

%= 	 	 

%% ftrace( :GoalGoal) is semidet.
%
% Functor Trace.
%
ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   thread_leash(-all),thread_leash(+exception),trace,Goal)).



%= 	 	 

% :- mpred_trace_less(rtrace/0).
% :- mpred_trace_less(nortrace/0).
% :- mpred_trace_less(nortrace/0).
% :- mpred_trace_less(rtrace/0).

:- unlock_predicate(system:hotrace/1).
% :- mpred_trace_less(system:hotrace/1).
:- '$set_predicate_attribute'(hotrace(_), trace, 0).
%:- '$set_predicate_attribute'(hotrace(_), hide_childs, 1).
% :- if_may_hide('$set_predicate_attribute'(hotrace(_), trace, 0)).
% :- if_may_hide('$set_predicate_attribute'(system:hotrace(_), hide_childs, 1)).
% :- if_may_hide('$set_predicate_attribute'(system:hotrace(_), trace, 0)).
:- lock_predicate(system:hotrace/1).

% :- mpred_trace_childs(hotrace/1).

% :- mpred_trace_none(system:trace/0).
% :- mpred_trace_none(system:hotrace/0).

% :- mpred_trace_none(system:tracing/0).

%:- ( listing(hotrace/1),redefine_system_predicate(system:hotrace(_)), mpred_trace_none(hotrace(0)) ).
% :- if_may_hide('$set_predicate_attribute'(hotrace(_), trace, 0)).
% :- if_may_hide('$set_predicate_attribute'(hotrace(_), hide_childs, 1)).

:- export(fixhotrace/1).

%= 	 	 

%% fixhotrace( :GoalX) is semidet.
%
% Fixho Trace.
%
fixhotrace(X):- tracing -> setup_call_cleanup_each(hotrace,X,hotrace(dtrace)) ; call(X).
% :- mpred_trace_none(fixhotrace(0)).


:- export(on_x_rtrace/1).

%= 	 	 

%% on_x_rtrace( :GoalC) is semidet.
%
% If there If Is A an exception in  :Goal Class then r Trace.
%
on_x_rtrace(C):- 
 hotrace(skipWrapper;tracing;(tlbugger:rtracing))-> C;
   catchv(C,E,
     (wdmsg(on_x_rtrace(E)),catchv(rtrace(with_skip_bugger(C)),E,wdmsg(E)),dtrace(C))).


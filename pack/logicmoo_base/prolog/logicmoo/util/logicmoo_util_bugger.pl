/** <module> Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_bugger.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
/*
unused_bugger:-nop((module(bugger,[
     was_module/2,
     with_dmsg/2,
     evil_term/3,
     all_module_predicates_are_transparent/1,
     kill_term_expansion/0,
     stack_depth/1,
     stack_check/1,
     stack_check/2,
     stack_check_else/2,
         module_meta_predicates_are_transparent/1,
         module_predicates_are_exported/1,
         module_predicates_are_exported/0,
         all_module_predicates_are_transparent/1,
         
         %is_loop_checked/1,
         %is_module_loop_checked/2,
         snumbervars/1,
         snumbervars/1,
         snumbervars/2,
         must_det_l/1,
         one_must_det/2,
         is_deterministic/1,
     programmer_error/1,
     forall_member/3,
     debugOnError0/1,
     global_pathname/2,
     printAll/2,
     moo_hide_childs/1,
     moo_hide_childs/2,
     debugOnFailure/1,
     
     user_use_module/1,
     dumpST/1,
     dtrace/0,
     trace_or_throw/1,
     trace_or/1,
     os_to_prolog_filename/2,
     debugOnFailure0/1,   
     ifThen/2,     
     module_predicate/3,
     to_m_f_arity_pi/5,
     % test_call/1,
     printAll/1,
     dynamic_load_pl/1,
     term_to_message_string/2,
     isDebugging/1,
     ggtrace/0,
     gftrace/0,
     grtrace/0,
     has_auto_trace/1,
     one_must/2,
%     read_line_with_nl/3,
	 unnumbervars/2,
         renumbervars/2,

     flush_output_safe/0,
     flush_output_safe/1,

     loop_check/1,
     loop_check/2,
     loop_check_term/3,     
     loop_check_term_key/3,
     no_loop_check_term_key/3,
         % loop_check_throw/1,
         %loop_check_fail/1,

     %debugCall/1,
     debugCallWhy/2,
     %debugCallF/1,
   
      dumpST/0,
      
      dtrace/1,
      export_all_preds/0,
     debugOnError/1, % Throws unless [Fail or Debug]
     logOnError/1, % Succeeds unless no error and failure occured

     ignoreOnError/1, % same
        debugOnErrorIgnore/1,

        must_det/1, % must leave no coice points behind 
     throwOnFailure/1, % Throws unless [Fail or Debug]

     % cant ignore - Throws but can be set to [Throw, Fail or Ignore or Debug]
     must/1, % must succeed at least once
     ftrace/1, % tells me why a call didn't succeed once
     sanity/1, % doesnt run on release
     gmust/2, % like must/1 but arg2 must be ground at exit
    
     rtrace/1,  % trace why choice points are left over
     must_each/1,  % list block must succeed once .. it smartly only debugs to the last failures
     logOnErrorIgnore/1,
     debugOnFailure/1, % Succeeds but can be set to [Fail or Debug]
     logOnFailure/1,  % Fails unless [+Ignore]
          % can ignore
     failOnError/1, % for wrapping code may throw to indicate failure
   must_not_repeat/1,  % predicate must never bind the same arguments the same way twice

   */
/*

debug(+Topic, +Format, +Arguments)
Prints a message using format(Format, Arguments) if Topic unies with a topic
enabled with debug/1.
debug/nodebug(+Topic [>le])
Enables/disables messages for which Topic unies. If >le is added, the debug
messages are appended to the given le.
assertion(:Goal)
Assumes that Goal is true. Prints a stack-dump and traps to the debugger otherwise.
This facility is derived from the assert() macro as used in C, renamed
for obvious reasons.
*/

/*
     prolog_must/1,
     prolog_must_l/1,


        with_output_to_stream/2,
     dmsg/1,
     dmsg/2,
     dfmt/1,
     dfmt/2,
     dmsg/3,
     %%logLevel/2,
     setLogLevel/2,
     fresh_line/1,
     fresh_line/0,
     logOnFailureIgnore/1,
	 writeFailureLog/2,
	% debugOnFailure/2,
     debugOnFailureEach/1,
    moo_hide_childs/1,

     fmt/1,fmt0/1,
     fmt/2,fmt0/2,
     fmt/3,fmt0/3,

     unlistify/2,
     listify/2,

     tlbugger:ifCanTrace/0,
     ctrace/0,
     isConsole/0,

     must_assign/1,
     must_assign/2,
     hotrace/1,
     cnotrace/1,
     set_bugger_flag/2,
     bugger_flag/2,
     buggeroo/0,
      join_path/3,

    ((dynamic_multifile_exported)/1),
      
      op(1150,fx,(dynamic_multifile_exported)),
      export_all_preds/0,
     export_all_preds/1 ]))).
*/

:- meta_predicate nodebugx(0).
:- meta_predicate oncely(0).
:- meta_predicate with_preds(?,?,?,?,?,0).
:- meta_predicate with_search_filters(0).
:- meta_predicate with_skip_bugger(0).

:- meta_predicate one_must(0,0,0).

:- swi_export(nop/1).
nop(_).
%set_prolog_flag(N,V):-!,nop(set_prolog_flag(N,V)).


% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- set_prolog_flag(generate_debug_info, true).
% have to load this module here so we dont take ownership of prolog_exception_hook/4.

% :- user:ensure_loaded(library(backcomp)).
:- user:ensure_loaded(library(ansi_term)).
:- user:ensure_loaded(library(check)).
:- user:ensure_loaded(library(debug)).
:- user:ensure_loaded(library(lists)).
:- user:ensure_loaded(library(make)).
:- user:ensure_loaded(library(prolog_stack)).
:- user:ensure_loaded(library(system)).
:- user:ensure_loaded(library(apply)).

:- ensure_loaded(logicmoo_util_dmsg).
:- ensure_loaded(logicmoo_util_bugger_catch).

:- ensure_loaded(logicmoo_util_database).
:- ensure_loaded(logicmoo_util_with_assertions).
:- ensure_loaded(logicmoo_util_prolog_streams).



:-module_transparent(if_may_hide/1).
%if_may_hide(_G):-!.
if_may_hide(G):-G.

:-thread_local(thlocal:session_id/1).
:-multifile(thlocal:session_id/1).

user:moo_hide(M):-if_may_hide(b_moo_hide(M)).
user:moo_hide(M,F,A):-if_may_hide(b_moo_hide(M,F,A)).

b_moo_hide(M,F,A):-
    functor(Pred,F,A),   
   '$set_predicate_attribute'(M:Pred, noprofile, 1),
   (A==0 -> '$set_predicate_attribute'(M:Pred, hide_childs, 1);'$set_predicate_attribute'(M:Pred, hide_childs, 1)),
   (A==0 -> '$set_predicate_attribute'(M:Pred, trace, 0);'$set_predicate_attribute'(M:Pred, trace, 1)).



b_moo_hide(M:F/A):-!,'$find_predicate'(F/A,List),forall(lists:member(M:F/A,List),moo_hide(M,F,A)).
b_moo_hide(P):-!,'$find_predicate'(P,List),forall(lists:member(M:F/A,List),moo_hide(M,F,A)).
b_moo_hide(F/A):-!,'$find_predicate'(F/A,List),forall(lists:member(M:F/A,List),moo_hide(M,F,A)).

:- meta_predicate(cnotrace(0)).
cnotrace(G):- once(G).
:-moo_hide(cnotrace/1).
:-if_may_hide('$set_predicate_attribute'(cnotrace(_), hide_childs, 1)).
:-if_may_hide('$set_predicate_attribute'(cnotrace(_), trace, 0)).


hotrace:-notrace.
:- swi_export(hotrace/1).
:- meta_predicate(hotrace(0)).
% Unlike notrace/1, it allows traceing when excpetions are raised during Goal.


thread_leash(+Some):-!, (thread_self(main)->leash(+Some);thread_leash(-Some)).
thread_leash(-Some):-!, (thread_self(main)->leash(-Some);thread_leash(-Some)).
thread_leash(Some):-!, (thread_self(main)->leash(+Some);thread_leash(-Some)).

% ((tracing, notrace) -> CC=trace;CC=true), '$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,(('$leash'(_, Old),'$visible'(_, OldV),CC),CC)).
get_hotrace(X,Y):- tracing,!,'$visible'(OldV, OldV),notrace,visible(-all),visible(+exception),Y = call_cleanup(show_call(X),notrace(('$visible'(_, OldV),trace))).
get_hotrace(X,Y):- !,'$visible'(OldV, OldV),notrace,visible(-all),visible(+exception),Y = call_cleanup(X,('$visible'(_, OldV))).

hotrace(X):- notrace(get_hotrace(X,Y)),Y.
:-moo_hide(hotrace/1).
:-if_may_hide('$set_predicate_attribute'(hotrace(_), hide_childs, 1)).
:-if_may_hide('$set_predicate_attribute'(hotrace(_), trace, 0)).

%get_hotrace(X):-  visible(+exception),thread_leash(+exception),restore_trace((notrace,X)).
% hotrace(C):-current_predicate(logicmoo_bugger_loaded/0)->catchvv((C),E,((writeq(E=C),rtrace(C),trace_or_throw(E=C))));C.


:-
      op(1150,fx,(user:dynamic_multifile_exported)),
      op(1150,fx,user:meta_predicate),
      op(1150,fx,user:thread_local).

      writeSTDERR0(A):-dmsg(A).
      debugFmt(A):-dmsg(A).

:-module_transparent((
     was_module/2,
     with_dmsg/2,
     dtrace/1,
     dtrace/2,
     dtrace/0,
     evil_term/3,
     kill_term_expansion/0,
     stack_depth/1,
     stack_check/1,
     stack_check/2,
     stack_check_else/2,
         module_meta_predicates_are_transparent/1,
         module_predicates_are_exported/1,
         module_predicates_are_exported/0,
         all_module_predicates_are_transparent/1,
         
         is_loop_checked/1,
         is_module_loop_checked/2,
         snumbervars/1,
         snumbervars/1,
         snumbervars/2,
         must_det_l/1,
         one_must_det/2,
         is_deterministic/1,
     programmer_error/1,
     forall_member/3,
     debugOnError0/1,
     global_pathname/2,
     printAll/2,
     moo_hide_childs/1,
     moo_hide_childs/2,
     debugOnFailure/1,
     
     user_use_module/1,
     dumpST/1,
     dtrace/0,
     trace_or_throw/1,
     trace_or/1,
     os_to_prolog_filename/2,
     debugOnFailure0/1,
     ifThen/2,     
     module_predicate/3,
     to_m_f_arity_pi/5,
     % test_call/1,
     printAll/1,
     dynamic_load_pl/1,
     term_to_message_string/2,
     isDebugging/1,
     ggtrace/0,
     gftrace/0,
     grtrace/0,
     ggtrace/1,
     gftrace/1,
     grtrace/1,
     has_auto_trace/1,
     one_must/2,
%     read_line_with_nl/3,
	 unnumbervars/2,
         renumbervars/2,

     flush_output_safe/0,
     flush_output_safe/1,

     loop_check/1,
     loop_check/2,
     loop_check_term/3,
         % loop_check_throw/1,
         %loop_check_fail/1,

     %debugCall/1,
     debugCallWhy/2,
     %debugCallF/1,
   
      dumpST/0,
      
      dtrace/1,
      export_all_preds/0,
     debugOnError/1, % Throws unless [Fail or Debug]
     logOnError/1, % Succeeds unless no error and failure occured

     ignoreOnError/1, % same
        debugOnErrorIgnore/1,

        must_det/1, % must leave no coice points behind 
     throwOnFailure/1, % Throws unless [Fail or Debug]

     % cant ignore - Throws but can be set to [Throw, Fail or Ignore or Debug]
     must/1, % must succeed at least once
     ftrace/1, % tells me why a call didn't succeed once
     sanity/1, % doesnt run on release
     gmust/2, % like must/1 but arg2 must be ground at exit
    
     rtrace/1,  % trace why choice points are left over
     must_each/1,  % list block must succeed once .. it smartly only debugs to the last failures
     logOnErrorIgnore/1,
     logOnError/1,
     logOnError0/1,
     debugOnFailure/1, % Succeeds but can be set to [Fail or Debug]
     logOnFailure/1,  % Fails unless [+Ignore]
          % can ignore
     failOnError/1, % for wrapping code may throw to indicate failure
   must_not_repeat/1,  % predicate must never bind the same arguments the same way twice



     prolog_must/1,
     prolog_must_l/1,


        with_output_to_stream/2,
     dmsg/1,
     dmsg/2,
     dfmt/1,
     dfmt/2,
     dmsg/3,
     %%logLevel/2,
     setLogLevel/2,
     fresh_line/1,
     fresh_line/0,
     logOnFailureIgnore/1,
	 writeFailureLog/2,
	% debugOnFailure/2,
     debugOnFailureEach/1,
    moo_hide_childs/1,

     fmt/1,fmt0/1,
     fmt/2,fmt0/2,
     fmt/3,fmt0/3,

     unlistify/2,
     listify/2,

     tlbugger:ifCanTrace/0,
     ctrace/0,
     isConsole/0,

     must_assign/1,
     must_assign/2,
     hotrace/1,
     set_bugger_flag/2,
     bugger_flag/2,
     buggeroo/0,
      join_path/3,

    ((dynamic_multifile_exported)/1),
      export_all_preds/0,
     export_all_preds/1 )).



:- thread_local(tlbugger:rtracing/0).
:- thread_local(tlbugger:no_colors/0).
% =========================================================================

:- thread_local( tlbugger:old_no_repeats/0).
:- thread_local( tlbugger:skip_bugger/0).
:- thread_local( tlbugger:dont_skip_bugger/0).
%:-thread_local( tlbugger:bugger_prolog_flag/2).
:- thread_local( tlbugger:skipMust/0).

%MAIN tlbugger:skip_bugger.
%MAIN tlbugger:skipMust.


% :- autoload.



:-meta_predicate restore_trace(0).
restore_trace(Goal):-  ((tracing, notrace) -> CC=trace;CC=true), 
   '$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,(('$leash'(_, Old),'$visible'(_, OldV),CC),CC)).




nortrace:-stop_rtrace,notrace.
rnotrace:-stop_rtrace,trace.
rtrace:-notrace((visible(+all),visible(+unify),visible(+exception),thread_leash(-all),thread_leash(+exception),assert(tlbugger:rtracing))).
start_rtrace:-rtrace,trace.
stop_rtrace:-notrace((visible(+all),visible(+unify),visible(+exception),thread_leash(+all),thread_leash(+exception),notrace,ignore(retract(tlbugger:rtracing)))).
:-moo_hide(nortrace/0).
:-moo_hide(rnotrace/0).
:-moo_hide(rtrace/0).
:-moo_hide(start_rtrace/0).
:-moo_hide(stop_rtrace/0).

:-moo_hide(true/0).
:-unlock_predicate(system:notrace/1).
:-moo_hide(system:notrace/1).
:-if_may_hide('$set_predicate_attribute'(notrace(_), hide_childs, 1)).
:-if_may_hide('$set_predicate_attribute'(notrace(_), trace, 0)).
:-if_may_hide('$set_predicate_attribute'(system:notrace(_), hide_childs, 1)).
:-if_may_hide('$set_predicate_attribute'(system:notrace(_), trace, 0)).
:-lock_predicate(system:notrace/1).


get_rtrace(X,Y):- tlbugger:rtracing,!,X=Y.
get_rtrace(X,Y):- tracing,!,'$leash'(Old, Old),'$visible'(OldV, OldV),rtrace, Y= call_cleanup((X),notrace(('$leash'(_, Old),'$visible'(_, OldV),stop_rtrace))).
get_rtrace(X,Y):- '$leash'(Old, Old),'$visible'(OldV, OldV), (Y= call_cleanup((X),(notrace,'$leash'(_, Old),'$visible'(_, OldV)))).

:-meta_predicate rtrace(0).
rtrace(Goal):- (tracing -> (notrace(get_rtrace(Goal,C)),C) ; (trace,notrace(get_rtrace(Goal,C)),C,notrace)).

:-moo_hide(rtrace/1).

:-meta_predicate ftrace(0).
ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   thread_leash(-all),thread_leash(+exception),trace,Goal)).

non_user_console:-thread_self(Self),Self\=main,current_input(In),stream_property(In,tty(TF)),TF\==true,!,set_stream(In,close_on_exec(true)).
trace_or_throw(E):-non_user_console,thread_self(Self),wdmsg(thread_trace_or_throw(Self+E)),
   current_input(In),   logOnError(attach_console),set_stream(In,close_on_exec(true)),throw(abort),thread_exit(trace_or_throw(E)).
trace_or_throw(E):- trace_or(throw(E)).

:- ensure_loaded(logicmoo_util_filestreams).
:- ensure_loaded(logicmoo_util_preddefs).
:- ensure_loaded(logicmoo_util_loop_check).
:- ensure_loaded(logicmoo_util_no_repeats).
:- ensure_loaded(logicmoo_util_varnames).

:-swi_export(errx/0).
errx:-debugOnError((assert_if_new(tlbugger:dont_skip_bugger),do_gc,dumpST(10))),!.

% false = use this wrapper, true = code is good and avoid using this wrapper
:- swi_export(skipWrapper/0).
%skipWrapper:-!,fail.
skipWrapper:- tracing,\+tlbugger:rtracing.
skipWrapper:- tlbugger:dont_skip_bugger,!,fail.
% skipWrapper:- 0 is random(5),!.
% skipWrapper:- tlbugger:skipMust,!.
skipWrapper:- tlbugger:skip_bugger,!.

:-moo_hide(skipWrapper/0).
:-moo_hide(tracing/0).
:-if_may_hide('$set_predicate_attribute'(tlbugger:skipMust,hide_childs,1)).
:-if_may_hide('$set_predicate_attribute'(tlbugger:skipWrapper,hide_childs,1)).


% false = hide this wrapper
showHiddens:-true.

% -- CODEBLOCK


logOnError(C):-prolog_ecall(0,logOnError0,C).
:-swi_export(logOnError0/1).
logOnError0(C):- catchvv(C,E,dmsg(logOnError(E,C))).
logOnErrorEach(C):-prolog_ecall(1,logOnError,C).
logOnErrorIgnore(C):-prolog:ignore(logOnError0(C)).


:- meta_predicate(one_must(0,0)).
one_must(MCall,OnFail):- skipWrapper,!, (MCall *->  true ;    OnFail).
one_must(MCall,OnFail):- strip_module(MCall,M,Call), '@'(( Call *->  true ;    OnFail ),M).


must_det(C):- must(C),!.

one_must_det(Call,_OnFail):-Call,!.
one_must_det(_Call,OnFail):-OnFail,!.

must_det(Call,OnFail):- trace_or_throw(deprecated(must_det(Call,OnFail))),Call,!.
must_det(_Call,OnFail):-OnFail.

:-module_transparent(must_det_l/1).
must_det_l(MC):- tlbugger:skipMust,!, strip_module(MC,M,C),!, '@'(det_lm(M,C),M).
must_det_l(MC):- strip_module(MC,M,C),!, '@'(must_det_lm(M,C),M).

:-module_transparent(must_det_lm/2).
must_det_lm(_,C):-var(C),trace_or_throw(var_must_det_l(C)),!.
must_det_lm(_,[]):-!.
must_det_lm(M,[C|List]):-!,must(M:C),!,must_det_lm(M,List).
must_det_lm(M,(C,List)):-!,must(M:C),!,must_det_lm(M,List).
must_det_lm(M,C):- must(M:C),!.

:-module_transparent(det_lm/2).
det_lm(M,(C,List)):- !,C,!,det_lm(M,List).
det_lm(M,C):-M:C,!.

:-module_transparent(must_l/1).
must_l(C):-var(C),trace_or_throw(var_must_l(C)),!.
must_l((A,!,B)):-!,must(A),!,must_l(B).
must_l((A,B)):-!,must((A,(deterministic(true)->(!,must_l(B));B))).
must_l(C):- must(C).


:-thread_local tlbugger:skip_use_slow_sanity/0.
tlbugger:skip_use_slow_sanity.

% thread locals should defaults to false  tlbugger:skip_use_slow_sanity.

slow_sanity(C):- ( tlbugger:skip_use_slow_sanity ; sanity(C)),!.


% -- CODEBLOCK
:- swi_export(sanity/1).
:-meta_predicate(sanity(0)).

% sanity is used for type checking (is not required)
% sanity(Call):-!.
% sanity(_):-!.
% sanity(_):-skipWrapper,!.
sanity(Call):-bugger_flag(release,true),!,assertion(Call).
sanity(G):- tlbugger:show_must_go_on,!,ignore(show_call_failure(G)).
sanity(G):- ignore(must(show_call_failure(G))).


:-swi_export(is_release/0).
is_release :- \+ not_is_release.
:-swi_export(not_is_release/0).
not_is_release :- 1 is random(4).


:-thread_local tlbugger:show_must_go_on/0.
badfood(MCall):- numbervars(MCall,0,_,[functor_name('VAR_______________________x0BADF00D'),attvar(bind),singletons(false)]),dumpST.

% -- CODEBLOCK
:- swi_export(without_must/1).
:-meta_predicate(without_must(0)).

without_must(G):- with_assertions(tlbugger:skipMust,G).

% -- CODEBLOCK
:- swi_export(y_must/2).
:-meta_predicate (y_must(?,0)).
y_must(Y,C):- catchvv(C,E,(wdmsg(E:must_xI__xI__xI__xI__xI_(Y,C)),fail)) *-> true ; dtrace(y_must(Y,C)).

% -- CODEBLOCK
:- swi_export(must/1).
:-meta_predicate (must(0)).
:-set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(200), attributes(portray)]).
:-set_prolog_flag(debugger_show_context,true).

must(C):- notrace(get_must(C,CALL)),CALL.

get_must(C,C):- fail,is_release,!.
get_must(C,DO):-  tlbugger:skipMust,!,DO = C.
get_must(Call,DO):- skipWrapper,!, DO = (Call *-> true ; ((dmsg(failed(must(Call))),trace,Call))).
get_must(Call,DO):- tlbugger:show_must_go_on,!,
 DO = ((catchvv(Call,E,
     cnotrace(((dumpST,dmsg(error,sHOW_MUST_go_on_xI__xI__xI__xI__xI_(E,Call))),badfood(Call))))
            *-> true ; cnotrace((dumpST,dmsg(error,sHOW_MUST_go_on_failed_F__A__I__L_(Call)),badfood(Call))))).

get_must(Call,DO):-    
   (DO = (catchvv(Call,E,
     (dumpST,dmsg(error,must_xI__xI__xI__xI__xI_(E,Call)),
         set_prolog_flag(debug_on_error,true),
         ignore_each((rtrace(Call),stop_rtrace,trace,dtrace(Call),badfood(Call)))))
         *-> true ; (dumpST,ignore_each(((trace,dtrace(must_failed_F__A__I__L_(Call),Call),badfood(Call))))))).

:-swi_export(ignore_each/1).
:-meta_predicate(ignore_each(1)).
ignore_each((A,B)):-ignore_each(A),ignore_each(B),!.
ignore_each(A):-ignore(A).

:- meta_predicate
	must_maplist(1, ?),
	must_maplist(2, ?, ?),
        must_maplist(3, ?, ?, ?).

%%	must_maplist(:Goal, ?List)
%
%	True if Goal can successfully  be   applied  on  all elements of
%	List. Arguments are reordered to gain  performance as well as to
%	make the predicate deterministic under normal circumstances.

must_maplist(_, []).
must_maplist(Goal, [Elem|Tail]) :-
	must(call(Goal, Elem)),
	must_maplist(Goal, Tail).

%%	must_maplist(:Goal, ?List1, ?List2)
%
%	As must_maplist/2, operating on pairs of elements from two lists.
must_maplist(_, [], []).
must_maplist( Goal, [Elem1|Tail1], [Elem2|Tail2]) :-
	must(call(Goal, Elem1, Elem2)),
	must_maplist( Goal, Tail1, Tail2).

must_maplist(_, [], [],[]).
must_maplist( Goal, [Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3]) :-
	must(call(Goal, Elem1, Elem2, Elem3)),
	must_maplist( Goal, Tail1, Tail2, Tail3).



:- meta_predicate(motrace(0)).
motrace(O):-one_must(hotrace(must(O)),(trace,O)).

throw_safe(Exc):-trace_or_throw(Exc).

:- thread_local( thlocal:testing_for_release/1).

test_for_release(File):-  source_file(File), \+ make:modified_file(File), !.
test_for_release(File):-  
 G = test_for_release(File),
 setup_call_cleanup(dmsg('~N~nPress Ctrl-D to begin ~n~n  :- ~q. ~n~n',[G]),
  if_interactive(prolog),
   setup_call_cleanup(dmsg('~N~nStarting ~q...~n',[G]),
      with_assertions(thlocal:testing_for_release(File),user:ensure_loaded(File)),
      test_for_release_problems(File))).

test_for_release_problems(_):-!.
test_for_release_problems(File):-  
      dmsg('~N~nListing problems after ~q...~n',[File]),
      list_undefined,
      nop(at_start(if_defined(gxref,true))),!.

:- meta_predicate if_interactive(0).

if_interactive(Goal):-ignore(if_interactive0(Goal)),!.
if_interactive0(Goal):- 
   thread_self(main),
   current_input(In),
   stream_property(In,input),
   stream_property(In,tty(true)),
   read_pending_input(In,_,_),!,
   dmsg('~n(waiting ... ~n',[]),!,
   wait_for_input([In],RL,5),!,
   ( RL ==[] -> dmsg('...moving on)~n',[]) ; (dmsg('... starting goal)~n',[]),Goal)),
   !.



:- dynamic(buggerDir/1).
:- abolish(buggerDir/1),prolog_load_context(directory,D),asserta(buggerDir(D)).
:- dynamic(buggerFile/1).
:- abolish(buggerFile/1),prolog_load_context(source,D),asserta(buggerFile(D)).

:- module_transparent(user:library_directory/1).

% hasLibrarySupport :- absolute_file_name('logicmoo_util_library.pl',File),exists_file(File).

throwNoLib:- trace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(user:library_directory), trace_or_throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

addLibraryDir :- buggerDir(Here),atom_concat(Here,'/..',UpOne), absolute_file_name(UpOne,AUpOne),asserta(user:library_directory(AUpOne)).

% if not has library suport, add this direcotry as a library directory
% :-not(hasLibrarySupport) -> addLibraryDir ; true .

% :-hasLibrarySupport->true;throwNoLib.

:-must((context_module(X),!,X==user)).
:-must((  '$set_source_module'(X,X),!,X==user)).

:- swi_export(dumpST/0).



ib_multi_transparent33(MT):-multifile(MT),module_transparent(MT),dynamic_safe(MT).

dif_safe(Agent,Obj):- (var(Agent);var(Obj)),!.
dif_safe(Agent,Obj):- Agent\==Obj.

% hide Pred from tracing
to_m_f_arity_pi(M:Plain,M,F,A,PI):-!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(Term,M,F,A,PI):- strip_module(Term,M,Plain),Plain\==Term,!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(F/A,_M,F,A,PI):-functor_safe(PI,F,A),!.
to_m_f_arity_pi(PI,_M,F,A,PI):-functor_safe(PI,F,A).

with_preds((X,Y),M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call),with_preds(Y,M,F,A,PI,Call).
with_preds([X],M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call).
with_preds([X|Y],M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call),with_preds(Y,M,F,A,PI,Call).
with_preds(M:X,_M,F,A,PI,Call):-!, with_preds(X,M,F,A,PI,Call).
with_preds(X,M,F,A,PI,Call):-forall(to_m_f_arity_pi(X,M,F,A,PI),Call).

% moo_show_childs(_):-showHiddens,!.

:- swi_export(moo_show_childs/1).
:-module_transparent(moo_show_childs/1).
moo_show_childs(X):- with_preds(X,_M,F,A,_PI,'$hide'(F/A)).

moo_show_childs(M,F,A):-functor_safe(MPred,F,A),moo_show_childs(M,F,A,MPred).
moo_show_childs(M,_,_, MPred):- not(predicate_property(_:MPred,imported_from(M))).
moo_show_childs(M,F,A,_MPred):- moo_trace_hidechilds(M,F,A,0,0).



% ===================================================================
% Substitution based on ==
% ===================================================================
% Usage: dbgsubst(+Fml,+X,+Sk,?FmlSk)

:- swi_export(dbgsubst/4).
dbgsubst(A,B,C,A):- B==C,!.
dbgsubst(A,B,C,D):-var(A),!,dmsg(dbgsubst(A,B,C,D)),dumpST,dtrace,dbgsubst0(A,B,C,D).
dbgsubst(A,B,C,D):-dbgsubst0(A,B,C,D).

dbgsubst0(A,B,C,D):- 
      catchvv(hotrace(nd_dbgsubst(A,B,C,D)),E,(dumpST,dmsg(E:nd_dbgsubst(A,B,C,D)),fail)),!.
dbgsubst0(A,_B,_C,A).

nd_dbgsubst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_dbgsubst(  P, X,Sk, P1 ) :- functor_safe(P,_,N),nd_dbgsubst1( X, Sk, P, N, P1 ).

nd_dbgsubst1( _,  _, P, 0, P  ).
nd_dbgsubst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_dbgsubst2( X, Sk, Args, ArgS ),
            nd_dbgsubst2( X, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_dbgsubst2( _,  _, [], [] ).
nd_dbgsubst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( X, Sk, [A|As], [Ap|AS] ) :- nd_dbgsubst( A,X,Sk,Ap ),nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( _X, _Sk, L, L ).



:- meta_predicate(moo_hide_all(0)).
:- swi_export(moo_hide_all/1).
moo_hide_all(_:X) :-
	var(X),
        throw(error(instantiation_error, _)).
moo_hide_all(_:[]) :- !.
moo_hide_all(M:(H,T)) :- !,
	moo_hide_all(M:H),
	moo_hide_all(M:T).
moo_hide_all(M:[H|T]) :- !,
	moo_hide_all(M:H),
	moo_hide_all(M:T).
moo_hide_all(W):-
   (W =  M:F/A -> functor_safe(P,F,A) ; true),
   (W =  M:P -> functor_safe(P,F,A) ; true),
   M:
     (
     (atom(P) -> M:module_transparent(F/A) ;  true),
    ((A>0,arg(1,P,NV),nonvar(NV)) -> M:meta_predicate(P) ; true),
   user:moo_hide(F/A),noprofile(M:F/A),
   nospy(M:P)),!.
moo_hide_all(W):- throw(error(moo_hide_all(W), _)).

:- swi_export(moo_hide_all/1).



%=========================================
% Module Utils
%=========================================
:- swi_export(moo_hide_childs/1).
:-module_transparent(moo_hide_childs/1).
:- meta_predicate(moo_hide_childs(0)).

% module_functor(PredImpl,Module,Pred,Arity).

module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).
strip_arity(Pred/Arity,Pred,Arity).
strip_arity(PredImpl,Pred,Arity):-functor_safe(PredImpl,Pred,Arity).
% moo_hide_childs(+Pred).
:- swi_export(moo_hide_childs/1).

moo_hide_childs(Preds):- with_preds(Preds,M,F,A,_PI,moo_hide_childs(M,F/A)).

moo_hide_childs(Preds):- with_preds(Preds,M,_F,_A,PI,moo_hide_childs_0(M,PI)).

moo_hide_childs(M,Preds):- with_preds(Preds,_M,_F,_A,PI,moo_hide_childs_0(M,PI)).

% predicate_property(do_pending_db_ops,PP)
% predicate_property_m(Pred,imported_from(M)):- predicate_property(Pred,imported_from(M)).
% predicate_property_m(Pred,imported_from(M)):-predicate_property(M:Pred,M).

moo_hide_childs_0(M,Pred):-
% predicate_property_m(Pred,M),
'$set_predicate_attribute'(M:Pred, trace, 1),
'$set_predicate_attribute'(M:Pred, noprofile, 1),
'$set_predicate_attribute'(M:Pred, hide_childs, 1),!.

moo_hide_childs_0(N,MPred):- predicate_property(MPred,imported_from(M)), writeq(wont_hide(M,N,MPred)),nl.


moo_hide_childs(M,F,A):-moo_trace_hidechilds(M,F,A,1,1).

moo_only_childs(M,F,A):-moo_trace_hidechilds(M,F,A,0,0).


moo_trace_hidechilds(M,F,A,Trace,HideChilds):-
  current_predicate(M:F/A),functor(P,F,A),
  set_trace_np_hc(M:P,Trace,1,HideChilds).

set_trace_np_hc(MP,Trace,NP,HideChilds):-
   '$set_predicate_attribute'(MP, trace, Trace),
   '$set_predicate_attribute'(MP, noprofile, NP),
   '$set_predicate_attribute'(MP, hide_childs, HideChilds),!.

:- create_prolog_flag(bugger_debug,filter,[type(term),keep(true)]).
:- create_prolog_flag(opt_debug,filter,[type(term),keep(true)]).
:- create_prolog_flag(dmsg_color,true,[type(boolean),keep(false)]).

:- moo_trace_hidechilds(_,catchvv,3,0,0).
:- moo_trace_hidechilds(system,catch,3,0,0).


%:-multifile( tlbugger:bugger_prolog_flag/2).
%:- swi_export( tlbugger:bugger_prolog_flag/2).


:-meta_predicate(callsc(0)).
callsc(G):-G.
:-moo_hide(callsc/1).
:- current_predicate(M:callsc/1),moo_trace_hidechilds(M,callsc,1,0,0).

:- ensure_loaded(logicmoo_util_prolog_frames).


/*
current_prolog_flag(N,VV):-
   (( tlbugger:bugger_prolog_flag(N,V),
   ignore(current_prolog_flag(N,VO)),!,(VO=@=V -> true; ddmsg_call(set_prolog_flag(N,VO))));(current_prolog_flag(N,V),asserta( tlbugger:bugger_prolog_flag(N,V)))),!, V=VV.

set_prolog_flag(N,V):- current_prolog_flag(N,VV),!,
        (V==VV ->  true ; (asserta( tlbugger:bugger_prolog_flag(N,V)),set_prolog_flag(N,V))).
*/


:- set_prolog_flag(opt_debug,filter).
% :- set_prolog_flag(dmsg_color,false).

:- dynamic(double_quotes_was/1).
:- multifile(double_quotes_was/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).

define_if_missing(M:F/A,List):-current_predicate(M:F/A)->true;((forall(member(C,List),M:assertz(C)),swi_export(M:F/A))).

define_if_missing(system:atomics_to_string/3, [
  ( system:atomics_to_string(List, Separator, String):- new_a2s(List, Separator, String) ) ]).

define_if_missing(system:atomics_to_string/2, [
  ( system:atomics_to_string(List, String):- new_a2s(List, '', String) ) ]).

new_a2s(List, Separator, String):-catchvv(new_a2s0(List, Separator, String),_,((trace,new_a2s0(List, Separator, String)))).
new_a2s0(List, Separator, String):- debug,
 (atomic(String) -> (string_to_atom(String,Atom),concat_atom(List, Separator, Atom));
     (concat_atom(List, Separator, Atom),string_to_atom(String,Atom))).



:- swi_export(bad_idea/0).
bad_idea:-fail.

/*
:- swi_export(static_predicate/1).
:- meta_predicate(static_predicate(:)).
:- module_transparent(static_predicate/1).
static_predicate(FA):-once(predicate_property(FA,_)),not(predicate_property(FA,dynamic)).
*/

:-meta_predicate(show_call_entry(0)).
show_call_entry(Call):-wdmsg(show_call_entry(Call)),show_call(Call).

% ===================================================================
% Bugger Term Expansions
% ===================================================================

:-moo_show_childs(must(0)).

:- swi_export(wdmsg/1).
wdmsg(X):- cnotrace(with_all_dmsg(dmsg(X))).
wdmsg(F,X):- with_all_dmsg(dmsg(F,X)).

prolog_call(Call):-call(Call).
:-moo_show_childs(prolog_call(0)).

:-moo_hide_all(system:trace/0).
:-moo_hide_all(system:notrace/0).

:-moo_hide_all(system:tracing/0).

%:-user: ( listing(notrace/1),redefine_system_predicate(system:hotrace(_)), user:moo_hide_all(hotrace(0)) ).
:-if_may_hide('$set_predicate_attribute'(hotrace(_), trace, 0)).
:-if_may_hide('$set_predicate_attribute'(hotrace(_), hide_childs, 1)).

:- swi_export(fixhotrace/1).
fixhotrace(X):- tracing -> (call(X),trace) ; call(X).
:-moo_hide_all(fixhotrace(0)).

:- swi_export(hidetrace/1).
hidetrace(X):- X.
:-moo_hide_all(hidetrace(0)).

:- swi_export( tlbugger:use_bugger_expansion/0).
:-dynamic( tlbugger:use_bugger_expansion/0).
:- retractall( tlbugger:use_bugger_expansion).
%:- asserta( tlbugger:use_bugger_expansion).

functor_h0(P,F,A):-var(P),!,throw(functor_h_var(P,F,A)).
functor_h0(_:P,F,A):-nonvar(P),!,functor_h0(P,F,A).
functor_h0((P :- _),F,A):-nonvar(P),!,functor_h0(P,F,A).
functor_h0(P,_:F,A):-atom(F),compound(P),compound_name_arity(P,F,A),!.
functor_h0(P,F,A):-compound(P),compound_name_arity(P,F,A),!.
functor_h0(F,F,1):-!.

:-meta_predicate(bugger_t_expansion(+,+,-)).
bugger_t_expansion(_,T,T):-var(T),!.
bugger_t_expansion(CM,(H:-B),(H:-BB)):-!,bugger_t_expansion(CM,B,BB).
bugger_t_expansion(_,T,T):-not(compound(T)),!.
% bugger_t_expansion(_,C =.. List,compound_name_arguments(C,F,ARGS)):-List =@= [F|ARGS],!.
bugger_t_expansion(_,prolog_call(T),T):-!.
bugger_t_expansion(_,dynamic(T),dynamic(T)):-!.
bugger_t_expansion(_,format(F,A),format_safe(F,A)):-!.
bugger_t_expansion(CM,hotrace(T),fixhotrace(TT)):-!,bugger_t_expansion(CM,(T),(TT)).
bugger_t_expansion(_,F/A,F/A):-!.
bugger_t_expansion(_,M:F/A,M:F/A):-!.
bugger_t_expansion(CM,[F0|ARGS0],[F1|ARGS1]):- !,bugger_t_expansion(CM,F0,F1),bugger_t_expansion(CM,ARGS0,ARGS1).
% bugger_t_expansion(CM,T,AA):-  tlbugger:use_bugger_expansion,compound_name_arguments(T,F,[A]),unwrap_for_debug(F),!,bugger_t_expansion(CM,A,AA),
%  ddmsg(bugger_term_expansion((T->AA))),!.
bugger_t_expansion(_,use_module(T),use_module(T)):-!.
bugger_t_expansion(_,module(A,B),module(A,B)):-!.
bugger_t_expansion(_,listing(A),listing(A)):-!.
bugger_t_expansion(CM,M:T,M:TT):-!,bugger_t_expansion(CM,T,TT),!.
bugger_t_expansion(_,test_is(A),test_is_safe(A)):-!.
bugger_t_expansion(_,delete(A,B,C),delete(A,B,C)):-!.
bugger_t_expansion(CM,T,TT):-  
     compound_name_arguments(T,F,A),hotrace((bugger_t_expansion(CM,A,AA),
     functor_h0(T,FH,AH))),
    ( (fail,bugger_atom_change(CM,T,F,FH,AH,FF))-> true; FF=F ),
    compound_name_arguments(TT,FF,AA),!,
    ((true;T =@= TT)-> true;  ddmsg(bugger_term_expansion(CM,(T->TT)))),!.

:-meta_predicate(bugger_atom_change(:,0,+,+,-,-)).
bugger_atom_change(CM,T,F,FH,FA,FF):- tlbugger:use_bugger_expansion, bugger_atom_change0(CM,T,F,FH,FA,FF).
bugger_atom_change0(_CM,T,_F,FH,FA,FF):- current_predicate_module(T,M1),atom_concat(FH,'_safe',FF),functor_safe(FFT,FF,FA),current_predicate_module(FFT,M2),differnt_modules(M1,M2).

:-meta_predicate(bugger_atom_change(:,(-))).
bugger_atom_change(CM:T,TT):-
     functor_h0(T,FH,AH),
     F = CM:T,
    (bugger_atom_change(CM,T,F,FH,AH,FF)->true;FF=F),!,
    compound_name_arity(TT,FF,AH).


differnt_modules(User2,User1):- (User1==user;User2==user),!.
differnt_modules(User2,User1):- User1 \== User2.


:-dynamic(unwrap_for_debug/1).
% unwrap_for_debug(F):-member(F,[notrace,hotrace]).
% unwrap_for_debug(F):-member(F,[traceok,must,must_det,hotrace]).
%unwrap_for_debug(F):-member(F,['debugOnError',debugOnError0]),!,fail.
%unwrap_for_debug(F):-member(FF,['OnError','OnFailure','LeastOne','Ignore','must']),atom_concat(_,FF,F),!.

:-meta_predicate(bugger_goal_expansion(:,-)).
bugger_goal_expansion(CM:T,TT):-  tlbugger:use_bugger_expansion,!,bugger_goal_expansion(CM,T,TT).
:-meta_predicate(bugger_goal_expansion(+,+,-)).
bugger_goal_expansion(CM,T,T3):- once(bugger_t_expansion(CM,T,T2)),T\==T2,!,failOnError(expand_term(T2,T3)).

:-meta_predicate(bugger_expand_goal(0,-)).
bugger_expand_goal(T,_):- fail,ddmsg(bugger_expand_goal(T)),fail.

:-meta_predicate(bugger_expand_term(0,-)).
bugger_expand_term(T,_):- fail, ddmsg(bugger_expand_term(T)),fail.

:- swi_export(format_safe/2).
format_safe(A,B):-catchvv(format(A,B),E,(dumpST,dtrace(E:format(A,B)))).

:-meta_predicate(bugger_term_expansion(:,-)).
bugger_term_expansion(CM:T,TT):- compound(T),  tlbugger:use_bugger_expansion,!,bugger_term_expansion(CM,T,TT).
:-meta_predicate(bugger_term_expansion(+,+,-)).
bugger_term_expansion(CM,T,T3):- once(bugger_t_expansion(CM,T,T2)),T\==T2,!,nop(ddmsg(T\==T2)),catchvv(expand_term(T2,T3),_,fail).

% user:     expand_goal(G,G2):- compound(G),bugger_expand_goal(G,G2),!.


% user:goal_expansion(G,G2):- compound(G),bugger_goal_expansion(G,G2).

% user:expand_term(G,G2):- compound(G),bugger_expand_term(G,G2),!.


:- swi_export(traceok/1).
:- multifile current_directory_search/1.
:- module_transparent current_directory_search/1.
:- meta_predicate(hotrace(0)).
:- meta_predicate(traceok(0)).



thread_local_leaks:-!.



trace_or(E):- dumpST,dmsg(E),trace,dtrace,!.
trace_or(E):- trace,E.

has_gui_debug :- current_prolog_flag(windows,true),!.
has_gui_debug :- ( \+ current_prolog_flag(gui,true) ),!,fail.
has_gui_debug :- getenv('DISPLAY',NV),NV\==''.

:- swi_export(nodebugx/1).
:- module_transparent(nodebugx/1).
nodebugx(X):- 
  with_no_assertions(tlbugger:ifCanTrace,
   with_assertions(tlbugger:ifWontTrace,
    with_assertions(tlbugger:show_must_go_on,
       with_assertions(tlbugger:ifHideTrace,hotrace(X))))).

:-multifile term_to_message_string/2.
:-dynamic term_to_message_string/2.

:- dynamic isDebugging/1.

:-multifile was_module/2.
:-dynamic was_module/2.
:-module_transparent was_module/2.
:-multifile evil_term/3.
:-dynamic evil_term/3.

:- thread_local(has_auto_trace/1).

%user:term_expansion(G,G2):- loop_check(bugger_term_expansion(G,G2)).
%user:goal_expansion(G,G2):- loop_check(bugger_goal_expansion(G,G2)).



shrink_clause(P,Body,Prop):- (Body==true-> Prop=P ; (Prop= (P:-Body))).


shrink_clause( (H:-true),H):-!.
shrink_clause( HB,HB).


% - 	list_difference_eq(+List, -Subtract, -Rest)
%
%	Delete all elements of Subtract from List and unify the result
%	with Rest. Element comparision is done using ==/2.
list_difference_eq([],_,[]).
list_difference_eq([X|Xs],Ys,L) :-
 	(  list_difference_eq_memberchk_eq(X,Ys)
 	-> list_difference_eq(Xs,Ys,L)
 	;  L = [X|T],
 	  list_difference_eq(Xs,Ys,T)
 	).
list_difference_eq_memberchk_eq(X, [Y|Ys]) :- (  X == Y -> true ;  list_difference_eq_memberchk_eq(X, Ys) ).


:-use_module(library(ansi_term)).

:- dynamic(user:mpred_prop/2).
:- multifile(user:mpred_prop/2).



:- meta_predicate meta_interp(:,+).

meta_interp_signal(meta_call(V)):-!,nonvar(V).
meta_interp_signal(meta_callable(_,_)).
meta_interp_signal(_:meta_call(V)):-!,nonvar(V).
meta_interp_signal(_:meta_callable(_,_)).

:- swi_export(meta_interp/2).
meta_interp(CE,A):- hotrace((var(A);not(stack_check))),!, throw(meta_interp(CE,A)).
meta_interp(_CE,A):- thread_leash(+all),meta_interp_signal(A),!,fail.
meta_interp(CE,M:X):- atom(M),!,meta_interp(CE,X).
meta_interp(_,true):-!.
meta_interp(CE,A):- call(CE, meta_callable(A,NewA)),!,NewA.
meta_interp(CE,not(A)):-!,not(meta_interp(CE,A)).
meta_interp(CE,once(A)):-!,once(meta_interp(CE,A)).
meta_interp(CE,(A;B)):-!,meta_interp(CE,A);meta_interp(CE,B).
meta_interp(CE,(A->B)):-!,meta_interp(CE,A)->meta_interp(CE,B).
meta_interp(CE,(A->B;C)):-!,(meta_interp(CE,A)->meta_interp(CE,B);meta_interp(CE,C)).
meta_interp(CE,(A*->B;C)):-!,(meta_interp(CE,A)*->meta_interp(CE,B);meta_interp(CE,C)).
meta_interp(CE,(A,!)):-!,meta_interp(CE,A),!.
meta_interp(CE,(A,B)):-!,meta_interp(CE,A),meta_interp(CE,B).
%meta_interp(_CE,!):- !, cut_block(!).
meta_interp(CE,A):- show_call(call(CE,meta_call(A))).


% was_module(Mod,Exports) :- nop(was_module(Mod,Exports)).

bugger_flag(F=V):-bugger_flag(F,V).
bugger_flag(F,V):-current_prolog_flag(F,V).

set_bugger_flag(F,V):-current_prolog_flag(F,_Old),!,set_prolog_flag(F,V).
set_bugger_flag(F,V):-create_prolog_flag(F,V,[keep(true),tCol(ftTerm)]),!.



:- meta_predicate sanity(0).
:- meta_predicate gmust(0,0).
:- meta_predicate must(0).
:- meta_predicate rmust_det(0).
:- meta_predicate must_det(0).
:- meta_predicate must_det_l(0).
:- meta_predicate must_each(0).
:- meta_predicate one_must(0,0).
:- meta_predicate one_must_det(0,0).
:- meta_predicate prolog_must(0).
:- meta_predicate prolog_must_l(?).
:- meta_predicate prolog_must_not(0).
:- meta_predicate slow_sanity(0).

:- meta_predicate logOnFailure0(0).
:- meta_predicate debugOnError0(0).
:- meta_predicate will_debug_else_throw(0,0).
:- meta_predicate printAll(0,*).
:- meta_predicate load_dirrective(0,*).
:- meta_predicate debugOnFailure0(0).
:- meta_predicate cli_ntrace(0).
:- meta_predicate printPredCount(*,0,*).
:- meta_predicate ignoreOnError(0).
:- meta_predicate traceIf(0).
:- meta_predicate ifThen(0,0).
:- meta_predicate prolog_ecall_fa(*,1,*,*,0).
:- meta_predicate tryCatchIgnore(0).
:- meta_predicate logOnError0(0).
:- meta_predicate failOnError(0).
% :- meta_predicate test_call(0).

%:- meta_predicate debugCall(0).
:- meta_predicate prolog_ecall(*,1,?).
%:- meta_predicate traceafter_call(0).
:- meta_predicate if_prolog(*,0).
:- meta_predicate rtrace(0).
:- meta_predicate rtraceOnError(0).
:- meta_predicate debugOnError(0).
:- meta_predicate debugOnError0(0).
:- meta_predicate debugOnErrorIgnore(0).
:- meta_predicate debugOnFailure0(0).
:- meta_predicate forall_member(*,*,0).
:- meta_predicate throwOnFailure(0).
:- meta_predicate hotrace(0).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (0.:16 sec)

:- meta_predicate printAll(0).
:- meta_predicate showProfilerStatistics(0).
%:- meta_predicate debugCallF(0).

:- swi_export((user_ensure_loaded/1)).
:- module_transparent user_ensure_loaded/1.
user_ensure_loaded(What):- !, '@'(ensure_loaded(What),'user').

:- module_transparent user_use_module/1.
% user_use_module(logicmoo(What)):- !, '@'(use_module(logicmoo(What)),'user').
% user_use_module(library(What)):- !, use_module(library(What)).
user_use_module(What):- '@'(use_module(What),'user').

% :- '@'(ensure_loaded(logicmoo_util_library), 'user').



:- module_transparent(must_det_l/1).

:- module_transparent(loop_check_term_key/3).
:- module_transparent(no_loop_check_term_key/3).
% :- swi_export(no_loop_check/1).
%:- module_transparent(loop_check_fail/1).
%:- module_transparent(loop_check_throw/1).
%:- module_transparent(loop_check_term/3).
%:- meta_predicate((loop_check_throw(0))).
%:- meta_predicate((loop_check_fail(0))).


:-use_module(logicmoo_util_coroutining_was).


% ===================================================

:-meta_predicate(once_if_ground(0)).
once_if_ground(Call):-not(ground(Call)),!,Call.
once_if_ground(Call):- once(Call).

:-meta_predicate(once_if_ground(0,-)).
once_if_ground(Call,T):-not(ground(Call)),!,Call,deterministic(D),(D=yes -> T= (!) ; T = true).
once_if_ground(Call,!):-once(Call).

% ===================================================

to_list_of(_,[Rest],Rest):-!.
to_list_of(RL,[R|Rest],LList):-
      to_list_of(RL,R,L),
      to_list_of(RL,Rest,List),
      LList=..[RL,L,List],!.

% ===================================================

call_or_list([Rest]):-!,call(Rest).
call_or_list(Rest):-to_list_of(';',Rest,List),!,call(List).

call_skipping_n_clauses(N,H):-
   findall(B,clause_safe(H,B),L),length(L,LL),!,LL>N,length(Skip,N),append(Skip,Rest,L),!,call_or_list(Rest).

% =========================================================================

:- thread_local( tlbugger:wastracing/0).
:- moo_hide_all( tlbugger:wastracing/0).

% =========================================================================
% cli_ntrace(+Call) is nondet.
% use call/1 with trace turned off
cli_ntrace(X):- tracing -> with_assertions( tlbugger:wastracing,call_cleanup((notrace,call(X)),trace)) ; call(X).
traceok(X):-  tlbugger:wastracing -> call_cleanup((trace,call(X)),notrace) ; call(X).

:- moo_hide_all(tlbugger:skip_bugger).
:- moo_hide_all(skipWrapper).


% =========================================================================


% ==========================================================
% can/will Tracer.
% ==========================================================

:-thread_local(tlbugger:ifCanTrace/0).
tlbugger:ifCanTrace.
:-moo_hide(tlbugger:ifCanTrace/0).
% thread locals should defaults to false: tlbugger:ifCanTrace.
%MAIN 

:- swi_export(tlbugger:ifWontTrace/0).
:-thread_local(tlbugger:ifWontTrace/0).
:-moo_hide(tlbugger:ifWontTrace/0).

:- ensure_loaded(logicmoo_util_first).

:-moo_hide(tlbugger:ifHideTrace/0).

%:-meta_predicate(set_no_debug).
:- swi_export(set_no_debug/0).

:-dynamic(is_set_no_debug/0).

set_no_debug:- 
  must_det_l((
   asserta(is_set_no_debug),
   set_prolog_flag(generate_debug_info, false),
   retractall(tlbugger:ifCanTrace),
   retractall(tlbugger:ifWontTrace),
   asserta(tlbugger:ifWontTrace),   
   set_prolog_flag(report_error,false),   
   set_prolog_flag(debug_on_error,false),
   set_prolog_flag(debug, false),   
   set_prolog_flag(query_debug_settings, debug(false, false)),
   set_gui_debug(fail),
   thread_leash(-all),
   thread_leash(+exception),
   visible(-cut_call),!,
   notrace, nodebug)),!.

:- swi_export(set_no_debug_thread/0).
set_no_debug_thread:- 
  must_det_l((
   retractall(tlbugger:ifCanTrace),
   retractall(tlbugger:ifWontTrace),
   asserta(tlbugger:ifWontTrace))),!.

:-if(exists_source(library(gui_tracer))).
:- meta_predicate set_gui_debug(0).
set_gui_debug(TF):- current_prolog_flag(gui,true),!,
   ((TF, has_gui_debug,set_yes_debug, ignore((use_module(library(gui_tracer)),catchvv(guitracer,_,true)))) 
     -> set_prolog_flag(gui_tracer, true) ;
        set_prolog_flag(gui_tracer, false)).
:-endif.
set_gui_debug(false):-!.
set_gui_debug(true):- dmsg("Warning: no GUI").

:- module_transparent(set_yes_debug/0).
:- swi_export(set_yes_debug/0).
set_yes_debug:- 
  must_det_l([
   set_prolog_flag(generate_debug_info, true),
   set_prolog_flag(report_error,true),   
   set_prolog_flag(debug_on_error,true),
   set_prolog_flag(debug, true),   
   set_prolog_flag(query_debug_settings, debug(true, true)),
   % set_gui_debug(true),
   thread_leash(+all),
   thread_leash(+exception),
   visible(+cut_call),
   notrace, debug]),!.

set_yes_debug_thread:-
  set_yes_debug,
   (tlbugger:ifCanTrace->true;assert(tlbugger:ifCanTrace)),
   retractall(tlbugger:ifWontTrace).

:- tlbugger:use_bugger_expansion->true;assert(tlbugger:use_bugger_expansion).

% :- set_yes_debug.


isConsole :- telling(user).
isConsole :- current_output(X),!,stream_property(X,alias(user_output)).


willTrace:-tlbugger:ifWontTrace,!,fail.
willTrace:-not(isConsole),!,fail.
willTrace:-tlbugger:ifCanTrace.

hideTrace:-
  hideTrace([hotrace/1], -all),
  %%hideTrace(computeInnerEach/4, -all),

  hideTrace(
   [maplist_safe/2,
       maplist_safe/3], -all),


  hideTrace([hideTrace/0,
     tlbugger:ifCanTrace/0,
     ctrace/0,
     willTrace/0], -all),

  hideTrace([traceafter_call/1], -all),
  % hideTrace([notrace_call/1], -all),

  hideTrace(user:[
   call/1,
   call/2,
   apply/2,
   '$bags':findall/3,
   '$bags':findall/4,
   once/1,
   ','/2,
   catch/3,
   catchvv/3,
   member/2], -all),

  hideTrace(user:setup_call_catcher_cleanup/4,-all),

  hideTrace(system:throw/1, +all),
  %%hideTrace(system:dmsg/2, +all),
  hideTrace(user:message_hook/3 , +all),
  hideTrace(system:message_to_string/2, +all),
  !,hideRest,!.
  %%findall(File-F/A,(functor_source_file(M,P,F,A,File),M==user),List),sort(List,Sort),dmsg(Sort),!.

hideRest:- fail, buggerDir(BuggerDir),
   functor_source_file(M,_P,F,A,File),atom_concat(BuggerDir,_,File),hideTraceMFA(M,F,A,-all),
   fail.
hideRest:- functor_source_file(system,_P,F,A,_File),hideTraceMFA(system,F,A,-all), fail.
hideRest.

:- meta_predicate(hideTrace(:,-)).

functor_source_file(M,P,F,A,File):-functor_source_file0(M,P,F,A,File). % sanity(ground((M,F,A,File))),must(user:nonvar(P)).
functor_source_file0(M,P,F,A,File):-current_predicate(F/A),functor_safe(P,F,A),source_file(P,File),predicate_module(P,M).

predicate_module(P,M):- var(P),!,trace_or_throw(var_predicate_module(P,M)).
predicate_module(P,M):- predicate_property(P,imported_from(M)),!.
predicate_module(F/A,M):- atom(F),integer(A),functor(P,F,A),P\==F/A,predicate_property(P,imported_from(M)),!.
predicate_module(Ctx:P,M):- Ctx:predicate_property(P,imported_from(M)),!.
predicate_module(Ctx:F/A,M):- Ctx:((atom(F),integer(A),functor(P,F,A),P\==F/A,predicate_property(P,imported_from(M)))),!.
predicate_module(M:_,M):-!. %strip_module(P,M,_F),!.
predicate_module(_P,user):-!. %strip_module(P,M,_F),!.
%%predicate_module(P,M):- strip_module(P,M,_F),!.

hideTrace(_:A, _) :-
    var(A), !, trace, fail,
    throw(error(instantiation_error, _)).
hideTrace(_:[], _) :- !.
hideTrace(A:[B|D], C) :- !,
    hideTrace(A:B, C),
    hideTrace(A:D, C),!.

hideTrace(M:A,T):-!,hideTraceMP(M,A,T),!.
hideTrace(MA,T):-hideTraceMP(_,MA,T),!.

hideTraceMP(M,F/A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMP(M,P,T):-functor_safe(P,F,0),trace,hideTraceMFA(M,F,_A,T),!.
hideTraceMP(M,P,T):-functor_safe(P,F,A),hideTraceMFA(M,F,A,T),!.

tryCatchIgnore(MFA):- catchvv(MFA,_E,true). %%dmsg(tryCatchIgnoreError(MFA:E))),!.
tryCatchIgnore(_MFA):- !. %%dmsg(tryCatchIgnoreFailed(MFA)).

% tryHide(_MFA):-showHiddens,!.
tryHide(MFA):- tryCatchIgnore(moo_hide(MFA)).

hideTraceMFA(_,M:F,A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMFA(M,F,A,T):-user:nonvar(A),functor_safe(P,F,A),predicate_property(P,imported_from(IM)),IM \== M,!,nop(dmsg(doHideTrace(IM,F,A,T))),hideTraceMFA(IM,F,A,T),!.
hideTraceMFA(M,F,A,T):-hideTraceMFAT(M,F,A,T),!.

hideTraceMFAT(M,F,A,T):-doHideTrace(M,F,A,T),!.

doHideTrace(_M,_F,_A,[]):-!.
doHideTrace(M,F,A,[hide|T]):- tryHide(M:F/A),!,doHideTrace(M,F,A,T),!.
doHideTrace(M,F,A,[-all]):- moo_hide(M:F/A),fail.
doHideTrace(M,F,A,ATTRIB):- (\+ is_list(ATTRIB)),!,doHideTrace(M,F,A,[ATTRIB]).
doHideTrace(M,F,A,ATTRIB):- tryHide(M:F/A),!,
  tryCatchIgnore(trace(M:F/A,ATTRIB)),!.


ctrace:-willTrace->trace;notrace.

buggeroo:-hideTrace,traceAll,atom_concat(guit,racer,TRACER), catchvv(call(TRACER),_,true),debug,list_undefined.

singletons(_).

/*
 Stop turning GC on/off
:- set_prolog_flag(backtrace_depth,   200).
:- set_prolog_flag(backtrace_goal_depth, 2000).
:- set_prolog_flag(backtrace_show_lines, true).
:-set_prolog_flag(debugger_show_context,true).
:-set_prolog_flag(trace_gc,true).
:-set_prolog_flag(debug,true).
:-set_prolog_flag(gc,true).
*/
set_optimize(_):- !.
set_optimize(TF):- set_prolog_flag(gc,TF),set_prolog_flag(last_call_optimisation,TF),set_prolog_flag(optimise,TF).

do_gc:- !.
do_gc:- do_gc0.

do_gc0:- current_prolog_flag(gc,true),!,do_gc0.
do_gc0:- set_prolog_flag(gc,true), do_gc1, set_prolog_flag(gc,false).
do_gc1:- cnotrace((garbage_collect, garbage_collect_atoms /*garbage_collect_clauses*/ /*, statistics*/
                    )).


failOnError(Call):-notrace(catchvv(Call,_,fail)).

fresh_line:-current_output(Strm),fresh_line(Strm),!.
fresh_line(Strm):-is_prolog_stream(Strm),failOnError(format(Strm,'~n',[])),!.
fresh_line(Strm):-failOnError(format(Strm,'~N',[])),!.
fresh_line(Strm):-failOnError((stream_property(Strm,position('$stream_position'(_,_,POS,_))),(POS>0->nl(Strm);true))),!.
fresh_line(Strm):-failOnError(nl(Strm)),!.
fresh_line(_).

ifThen(When,Do):-When->Do;true.

% :- current_predicate(F/N),trace(F/N, -all),fail.
/*
traceAll:- current_predicate(user:F/N),
  functor_safe(P,F,N),
  local_predicate(P,F/N),
  trace(F/N, +fail),fail.
traceAll:- not((predicate_property(clearCateStack/1,_))),!.
traceAll:-findall(_,(member(F,[member/2,dmsg/1,takeout/3,findall/3,clearCateStack/1]),trace(F, -all)),_).
*/
traceAll:-!.


forall_member(C,[C],Call):-!,once(Call).
forall_member(C,C1,Call):-forall(member(C,C1),once(Call)).

must_assign(From=To):-must_assign(From,To).
must_assign(From,To):-To=From,!.
must_assign(From,To):- tlbugger:skipMust,!,ignore(To=From),!.
must_assign(From,To):-dmsg(From),dmsg(=),dmsg(From),dmsg(must_assign),!,trace,To=From.


prolog_must(Call):-must(Call).


% gmust is must with sanity
gmust(True,Call):-catchvv((Call,(True->true;throw(retry(gmust(True,Call))))),retry(gmust(True,_)),(trace,Call,True)).

% must is used declaring the predicate must suceeed

throwOnFailure(Call):-one_must(Call,throw(throwOnFailure(Call))).
ignoreOnError(CX):-ignore(catchvv(CX,_,true)).

% pause_trace(_):- hotrace(((debug,visible(+all),thread_leash(+exception),thread_leash(+call)))),trace.

%debugCall(C):-hotrace,dmsg(debugCall(C)),dumpST, pause_trace(errored(C)),ggtrace,C.
%debugCallF(C):-hotrace,dmsg(debugCallF(C)),dumpST, pause_trace(failed(C)),gftrace,C.

debugCallWhy(Why, C):- hotrace((notrace,wdmsg(Why))),dtrace(C).

:- swi_export(rtraceOnError/1).
rtraceOnError(C):-
  catchvv(
  with_skip_bugger( C ),E,(dmsg(rtraceOnError(E=C)),dtrace,thread_leash(+call),trace,thread_leash(+exception),thread_leash(+all),rtrace(with_skip_bugger( C )),dmsg(E=C),thread_leash(+call),dtrace)).


with_skip_bugger(C):-setup_call_cleanup(asserta( tlbugger:skip_bugger),C,retract( tlbugger:skip_bugger)).

%debugOnError(C):- tlbugger:rtracing,!, catchvv(C,E,call_cleanup(debugCallWhy(thrown(E),C),throw(E))).
%debugOnError(C):- skipWrapper,!,C.
debugOnError(C):- !,debugOnError0(C).
debugOnError(C):-prolog_ecall(0,debugOnError0,C).
debugOnError0(C):- tlbugger:rtracing,!,C.
debugOnError0(C):- catchvv(C,E,call_cleanup(debugCallWhy(thrown(E),C),throw(E))).
debugOnErrorEach(C):-prolog_ecall(1,debugOnError0,C).
debugOnErrorIgnore(C):-ignore(debugOnError0(C)).

debugOnFailure(C):-prolog_ecall(0,debugOnFailure0,C).
debugOnFailure0(C):- one_must(rtraceOnError(C),debugCallWhy(failed(debugOnFailure0(C)),C)).
debugOnFailureEach(C):-prolog_ecall(1,debugOnFailure,C).
debugOnFailureIgnore(C):-ignore(debugOnFailure(C)).

logOnFailure0(C):- one_must(C,dmsg(logOnFailure(C))).
logOnFailureEach(C):-prolog_ecall(1,logOnFailure,C).
logOnFailureIgnore(C):-ignore(logOnFailure0(logOnError0(C))).


%debugOnFailure0(X):-ctrace,X.
%debugOnFailure0(X):-catchvv(X,E,(writeFailureLog(E,X),throw(E))).
%throwOnFailure/1 is like Java/C's assert/1
%debugOnFailure1(Module,CALL):-trace,debugOnFailure(Module:CALL),!.
%debugOnFailure1(arg_domains,CALL):-!,logOnFailure(CALL),!.

beenCaught(must(Call)):- !, beenCaught(Call).
beenCaught((A,B)):- !,beenCaught(A),beenCaught(B).
beenCaught(Call):- fail, predicate_property(Call,number_of_clauses(_Count)), clause(Call,(_A,_B)),!,clause(Call,Body),beenCaught(Body).
beenCaught(Call):- catchvv(once(Call),E,(dmsg(caugth(Call,E)),beenCaught(Call))),!.
beenCaught(Call):- traceAll,dmsg(tracing(Call)),debug,trace,Call.


:-meta_predicate(with_no_term_expansions(0)).
with_no_term_expansions(Call):-
  with_no_assertions(user:term_expansion(_,_),
    with_no_assertions(term_expansion(_,_),
    with_no_assertions(user:goal_expansion(_,_),
      with_no_assertions(goal_expansion(_,_),Call)))).

kill_term_expansion:-
   abolish(user:term_expansion,2),
   abolish(user:goal_expansion,2),
   dynamic(user:term_expansion/2),
   dynamic(user:goal_expansion/2),
   multifile(user:term_expansion/2),
   multifile(user:goal_expansion/2).

local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-real_builtin_predicate(P),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
%local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains666(F,'aiml_'),!.
local_predicate(P,F/N):-functor_safe(P,F,N),!,fail.

%atom_contains666(F,C):- hotrace((atom(F),atom(C),sub_atom(F,_,_,_,C))).

:-meta_predicate(real_builtin_predicate(0)).


real_builtin_predicate(G):- predicate_property(G,foreign),!.
real_builtin_predicate(G):- strip_module(G,_,GS),(predicate_property(prolog:GS,built_in);predicate_property(system:GS,built_in)),!.
real_builtin_predicate(G):- (predicate_property(G,built_in),(functor(G,F,_),not(user:mpred_prop(F,prologHybrid)))).

will_debug_else_throw(E,Goal):- dmsg(bugger(will_debug_else_throw(E,Goal))),grtrace,Goal.

show_goal_rethrow(E,Goal):-
   dmsg(bugger(show_goal_rethrow(E,Goal))),
   throw(E).

on_prolog_ecall(F,A,Var,Value):-
  bin_ecall(F,A,Var,Value),!.
on_prolog_ecall(F,A,Var,Value):-
  default_ecall(IfTrue,Var,Value),
  on_prolog_ecall(F,A,IfTrue,true),!.


default_ecall(asis,call,call).
default_ecall(asis,fake_failure,fail).
default_ecall(asis,error,nocatch).

default_ecall(neverfail,call,call).
default_ecall(neverfail,fail,fake_bindings).
default_ecall(neverfail,error,show_goal_rethrow).

default_ecall(onfailure,call,none).
default_ecall(onfailure,fail,reuse).
default_ecall(onfailure,error,none).

default_ecall(onerror,call,none).
default_ecall(onerror,fail,none).
default_ecall(onerror,error,reuse).


on_prolog_ecall_override(F,A,Var,_SentValue, Value):- on_prolog_ecall(F,A,Var,Value), Value \== reuse,!.
on_prolog_ecall_override(_F,_A,_Var, Value, Value).

bin_ecall(F,A,unwrap,true):-member(F/A,[(';')/2,(',')/2,('->')/2,('call')/1]).
bin_ecall(F,A,fail,
 throw(never_fail(F/A))):-
   member(F/A,
    [(retractall)/1]).
bin_ecall(F,A,asis,true):-member(F/A,[('must')/1]).


:-moo_show_childs(prolog_ecall/2).
:-moo_show_childs(prolog_ecall/5).


prolog_ecall(_,_,Call):-var(Call),!,trace,randomVars(Call).
% prolog_ecall(BDepth,OnCall,M:Call):- fail,!, '@'( prolog_ecall(BDepth,OnCall,Call), M).

prolog_ecall(_,_,Call):-skipWrapper,!,Call.
prolog_ecall(BDepth,OnCall, (X->Y;Z)):-!,(prolog_ecall(BDepth,OnCall,X) -> prolog_ecall(BDepth,OnCall,Y) ; prolog_ecall(BDepth,OnCall,Z)).
prolog_ecall(BDepth,OnCall,Call):-functor_safe(Call,F,A),prolog_ecall_fa(BDepth,OnCall,F,A,Call).

% fake = true
prolog_ecall_fa(_,_,F,A,Call):-
  on_prolog_ecall(F,A,fake,true),!,
  atom_concat(F,'_FaKe_Binding',FAKE),
  snumbervars(Call,FAKE,0),
  dmsg(error(fake(succeed,Call))),!.

% A=0 , (unwrap = true ; asis = true)
prolog_ecall_fa(_,_,F,0,Call):-
  (on_prolog_ecall(F,0,unwrap,true);on_prolog_ecall(F,0,asis,true)),!,
  call(Call).

% A=1 , (unwrap = true )
prolog_ecall_fa(BDepth,OnCall,F,1,Call):-
  on_prolog_ecall(F,1,unwrap,true),
  arg(1,Call,Arg),!,
  prolog_ecall(BDepth,OnCall,Arg).

% A>1 , (unwrap = true )
prolog_ecall_fa(BDepth,OnCall,F,A,Call):-
  on_prolog_ecall(F,A,unwrap,true),!,
  Call=..[F|OArgs],
  functor_safe(Copy,F,A),
  Copy=..[F|NArgs],
  replace_elements(OArgs,E,prolog_ecall(BDepth,OnCall,E),NArgs),
  call(Copy).

% A>1 , (asis = true )
prolog_ecall_fa(_,_,F,A,Call):-
  on_prolog_ecall(F,A,asis,true),!,
  call(Call).

% each = true
prolog_ecall_fa(BDepth,OnCall,F,A,Call):-
  (on_prolog_ecall(F,A,each,true);BDepth>0),!,
  BDepth1 is BDepth-1,
  predicate_property(Call,number_of_clauses(_Count)),
  % any with bodies
  clause(Call,NT),NT \== true,!,
  clause(Call,Body),
   prolog_ecall(BDepth1,OnCall,Body).

prolog_ecall_fa(_,OnCall,_F,_A,Call):-
  call(OnCall,Call).

replace_elements([],_,_,[]):-!.
replace_elements([A|ListA],A,B,[B|ListB]):-replace_elements(ListA,A,B,ListB).

prolog_must_l(T):-T==[],!.
prolog_must_l([H|T]):-!,must(H), prolog_must_l(T).
prolog_must_l((H,T)):-!,prolog_must_l(H),prolog_must_l(T).
prolog_must_l(H):-must(H).

programmer_error(E):-trace, randomVars(E),dmsg('~q~n',[error(E)]),trace,randomVars(E),!,throw(E).



:-moo_show_childs(must/1).

% must(C):- ( 1 is random(4)) -> rmust_det(C) ; C.

rmust_det(C):- C *-> true ; dtrace(C).
% rmust_det(C)-  catchvv((C *-> true ; debugCallWhy(failed(must(C)),C)),E,debugCallWhy(thrown(E),C)).

must_each(List):-var(List),trace_or_throw(var_must_each(List)).
must_each([List]):-!,must(List).
must_each([E|List]):-!,must(E),must_each0(List).
must_each0(List):-var(List),trace_or_throw(var_must_each(List)).
must_each0([]):-!.
must_each0([E|List]):-E,must_each0(List).

:-moo_show_childs(one_must/2).
one_must(C1,C2,C3):-one_must(C1,one_must(C2,C3)).

is_deterministic(once(V)):-var(V),trace_or_throw(is_deterministic(var_once(V))).
is_deterministic(M:G):-atom(M),!,is_deterministic(G).
is_deterministic(Atomic):-atomic(Atomic),!.
is_deterministic(Ground):-ground(Ground),!.
is_deterministic((_,Cut)):-Cut==!.
is_deterministic(_ = _).
is_deterministic(_ =@= _).
is_deterministic(_ =.. _).
is_deterministic(_ == _).
is_deterministic(_ \== _).
is_deterministic(_ \== _).
is_deterministic(atom(_)).
is_deterministic(compound(_)).
is_deterministic(findall(_,_,_)).
is_deterministic(functor_safe(_,_,_)).
is_deterministic(functor_safe(_,_,_)).
is_deterministic(ground(_)).
is_deterministic(nonvar(_)).
is_deterministic(not(_)).
is_deterministic(once(_)).
is_deterministic(var(_)).
%is_deterministic(Call):-predicate_property(Call,nodebug),!.
%is_deterministic(Call):-predicate_property(Call,foreign),!.



randomVars(Term):- random(R), StartR is round('*'(R,1000000)), !,
 ignore(Start=StartR),
 snumbervars(Term, Start, _).

prolog_must_not(Call):-Call,!,trace,!,programmer_error(prolog_must_not(Call)).
prolog_must_not(_Call):-!.

%%%retractall(E):- retractall(E),functor_safe(E,File,A),dynamic(File/A),!.


% =================================================================================
% Utils
% =================================================================================

printPredCount(Msg,Pred,N1):- compound(Pred), debugOnFailureEach((arg(_,Pred,NG))),user:nonvar(NG),!,
  findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),dmsg(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor_safe(Pred,File,A),functor_safe(FA,File,A), predicate_property(FA,number_of_clauses(N1)),dmsg(num_clauses(Msg,File/A,N1)),!.



showProfilerStatistics(FileMatch):-
  statistics(global,Mem), MU is (Mem / 1024 / 1024),
  printPredCount('showProfilerStatistics: '(MU),FileMatch,_N1).



% ===============================================================================================
% UTILS
% ===============================================================================================

alldiscontiguous:-!.


if_prolog(swi,G):-call(G). % Run B-Prolog Specifics
if_prolog(_,_):-!. % Dont run SWI Specificd or others


:-meta_predicate(time_call(0)).
time_call(Call):-
  statistics(runtime,[MSecStart,_]),   
  ignore(show_call_failure(Call)),
  statistics(runtime,[MSecEnd,_]),
   MSec is (MSecEnd-MSecStart),
   Time is MSec/1000,
   ignore((Time > 0.5 , dmsg('Time'(Time)=Call))).

:- dynamic logger_property/2.
logger_property(todo,once,true).


:-debug(todo).


:-dynamic hook:dmsg_hook/1.
:-multifile hook:dmsg_hook/1.

contains_atom(V,A):-sub_term(VV,V),nonvar(VV),functor_safe(VV,A,_).

:- swi_export(matches_term/2).
matches_term(Filter,_):- var(Filter),!.
matches_term(Filter,Term):- var(Term),!,Filter=var.
matches_term(Filter,Term):- ( \+ \+ (matches_term0(Filter,Term))),!.
matches_term0(Filter,Term):- Term = Filter.
matches_term0(Filter,Term):- atomic(Filter),!,contains_atom(Term,Filter).
matches_term0(F/A,Term):- (var(A)->member(A,[0,1,2,3,4]);true), functor_safe(Filter,F,A), matches_term0(Filter,Term).
matches_term0(Filter,Term):- sub_term(STerm,Term),nonvar(STerm),matches_term0(Filter,STerm),!.


:-meta_predicate(gripe_time(+,0)).
:-swi_export(gripe_time/2).
gripe_time(TooLong,Goal):-statistics(cputime,Start),
  (Goal*->Success=true;Success=fail),
  once((statistics(cputime,End),
   Elapse is End-Start,
(Elapse>TooLong -> dmsg(gripe_time(warn(Elapse>TooLong),Goal)); true))),!,
   Success.





:- meta_predicate show_call0(0).
show_call0(C):- C. % debugOnError0(C). % dmsg(show_call(C)),C.      

:- meta_predicate show_call(:).
show_call(M:add(A)):-!, show_call0(M:add(A)),!.
% show_call(M:must(C)):- !, M:must(C).
show_call(C):-one_must((show_call0(C),dmsg(succeed(C))),((dmsg(failed_show_call(C)),garbage_collect_atoms,!,fail))).

:- meta_predicate show_call_failure(0).
show_call_failure(C):-one_must((show_call0(C)),((dmsg(failed_show_call(C)),dmsg(failed_show_call(C)),garbage_collect_atoms,!,fail))).

:- meta_predicate show_call_success(0).
show_call_success(C):- show_call0(C),dmsg(show_call_success(C)).

:- meta_predicate logOnFailure(0).
:- swi_export(logOnFailure/1).
logOnFailure(C):-one_must(C,notrace((dmsg(failed_show_call(C)),garbage_collect_atoms,!,fail))).


:-dynamic(user:logLevel/2).
:-module_transparent(user:logLevel/2).
:-multifile(user:logLevel/2).

setLogLevel(M,L):-retractall(user:logLevel(M,_)),(user:nonvar(L)->asserta(user:logLevel(M,L));true).

user:logLevel(debug,ERR):-thread_current_error_stream(ERR).
user:logLevel(error,ERR):-thread_current_error_stream(ERR).
user:logLevel(private,none).
user:logLevel(S,Z):-current_stream(_X,write,Z),trace,stream_property(Z,alias(S)).

loggerReFmt(L,LRR):-user:logLevel(L,LR),L \==LR,!,loggerReFmt(LR,LRR),!.
loggerReFmt(L,L).

loggerFmtReal(none,_F,_A):-!.
loggerFmtReal(S,F,A):-
  current_stream(_,write,S),
    fmt(S,F,A),
    flush_output_safe(S),!.



%=========================================
% Module Utils
%=========================================


:-swi_export(loading_module/1).
:-module_transparent(loading_module/1).
:-swi_export(loading_module/2).
:-module_transparent(loading_module/2).
:-swi_export(show_module/1).
:-module_transparent(show_module/1).

loading_module(M,use_module(U)):- parent_goal(_:catch(M:use_module(U),_,_),_).
loading_module(M,use_module(U)):- parent_goal(_:catch(M:ensure_loaded(U),_,_),_).
loading_module(M,consult(F)):- parent_goal(_:'$consult_file_2'(F,M,_,_,_),_).
loading_module(M,source_location(F)):- source_location(F,_),source_file_property(F,module(M)).
loading_module(M,file(F)):- prolog_load_context(file,F),source_file_property(F,module(M)).
loading_module(M,source(F)):- prolog_load_context(source,F),source_file_property(F,module(M)).
loading_module(M,prolog_load_context):- prolog_load_context(module,M).
loading_module(M,stream_property(F)):- stream_property(_X,file_name(F)),source_file_property(F,module(M)).
loading_module(M,context_module):- context_module(M).


user:prolog_current_frames(Each):- prolog_current_frame(Frame),prolog_current_frame_or_parent(Frame,Each).
prolog_current_frame_or_parent(Frame,Each):- Each=Frame; 
  (prolog_frame_attribute(Frame,parent,Parent),prolog_current_frame_or_parent(Parent,Each)).

:-module_transparent(user:caller_module(-)).
user:caller_module(Module):-user:caller_module(Module,v(function_expansion,func,user,'$toplevel','$apply','$expand')).
user:caller_module(Module,Skipped):- module_stack(Module,_), \+ arg(_,Skipped,Module).

:-module_transparent(user:module_stack(-,-)).
user:module_stack(M,prolog_load_context):- prolog_load_context(module, M).
user:module_stack(M,'$module'):- '$module'(M,M).
user:module_stack(M,'$set_source_module'):- '$set_source_module'(M,M).
user:module_stack(M,of):- predicate_property(M:of(_,_),imported_from(func)).
user:module_stack(M,frame):- prolog_current_frames(Each), prolog_frame_attribute(Each,context_module,M).


loading_module(M):- ((loading_module(M,_),M\=user));M=user.

show_module(W):-dmsg('<!--':W),ignore((show_call((loading_module(_,_))),fail)),dmsg(W:'-->').


module_predicate(ModuleName,P,F,A):-current_predicate(ModuleName:F/A),functor_catch(P,F,A), not((( predicate_property(ModuleName:P,imported_from(IM)),IM\==ModuleName ))).

:- meta_predicate if_defined(?).
:-swi_export(if_defined/1).
if_defined(Call):-current_predicate(_,Call)->Call.
:- meta_predicate if_defined(?,?).
:-swi_export(if_defined/2).
if_defined(Call,Else):-current_predicate(_,Call)->(Call*->true;fail);Else.
:- meta_predicate when_defined(?).
:-swi_export(when_defined/1).
when_defined(Call):-if_defined(Call,true).



% ========================================================================================
% Some prologs have a printf() tCol predicate.. so I made up fmtString/fmt in the Cyc code that calls the per-prolog mechaism
% in SWI it''s formzat/N and sformat/N
% ========================================================================================
:- dynamic(isConsoleOverwritten_bugger/0).
:- dynamic(formatter_hook/4).


fmtString(X,Y,Z):-sformat(X,Y,Z).
fmtString(Y,Z):-sformat(Y,Z).

saveUserInput:-retractall(isConsoleOverwritten_bugger),flush_output.
writeSavedPrompt:-not(isConsoleOverwritten_bugger),!.
writeSavedPrompt:-flush_output.
writeOverwritten:-isConsoleOverwritten_bugger,!.
writeOverwritten:-assert(isConsoleOverwritten_bugger).

writeErrMsg(Out,E):- message_to_string(E,S),fmt(Out,'<cycml:error>~s</cycml:error>\n',[S]),!.
writeErrMsg(Out,E,Goal):- message_to_string(E,S),fmt(Out,'<cycml:error>goal "~q" ~s</cycml:error>\n',[Goal,S]),!.
writeFileToStream(Dest,Filename):-
    catchvv((
    open(Filename,'r',Input),
    repeat,
        get_code(Input,Char),
        put(Dest,Char),
    at_end_of_stream(Input),
    close(Input)),E,
    fmt('<cycml:error goal="~q">~w</cycml:error>\n',[writeFileToStream(Dest,Filename),E])).




% =================================================================================
% Utils
% =================================================================================
% test_call(G):-writeln(G),ignore(once(catchvv(G,E,writeln(E)))).

debugFmtList(ListI):-hotrace((copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),dmsg(List0))),!.
debugFmtList0([],[]):-!.
debugFmtList0([A|ListA],[B|ListB]):-debugFmtList1(A,B),!,debugFmtList0(ListA,ListB),!.

debugFmtList1(Value,Value):-var(Value),!.
debugFmtList1(Name=Number,Name=Number):-number(Number).
debugFmtList1(Name=Value,Name=Value):-var(Value),!.
debugFmtList1(Name=Value,Name=(len:Len)):-copy_term(Value,ValueO),append(ValueO,[],ValueO),is_list(ValueO),length(ValueO,Len),!.
debugFmtList1(Name=Value,Name=(F:A)):-functor_safe(Value,F,A).
debugFmtList1(Value,shown(Value)).

% ===============================================================================================
% unlistify / listify
% ===============================================================================================

unlistify([L],O):-user:nonvar(L),unlistify(L,O),!.
unlistify(L,L).

listify(OUT,OUT):-not(not(is_list(OUT))),!.
listify(OUT,[OUT]).


traceIf(_Call):-!.
traceIf(Call):-ignore((Call,trace)).

traceafter_call(X):- call_cleanup(restore_trace((thread_leash(-all),visible(-all),X)),(thread_leash(+call), trace)).

%getWordTokens(WORDS,TOKENS):-concat_atom(TOKENS,' ',WORDS).
%is_string(S):- string(S).




%%:-user:(forall(current_predicate(user:FA),user:moo_hide_childs(user:FA))).
% hide this module from tracing
%%:-user:(forall(current_predicate(logicmoo_util_strings:FA),user:moo_hide_childs(logicmoo_util_strings:FA))).

module_hotrace(M):- forall(predicate_property(P,imported_from(M)),user:moo_hide_childs(M:P)).



:-meta_predicate(test_tl(1,+)).
test_tl(Pred,Term):-call(Pred,Term),!.
test_tl(Pred,Term):-compound(Term),functor_safe(Term,F,_),call(Pred,F),!.

:-meta_predicate(test_tl(+)).
test_tl(M:C):-!,call(M:C).
test_tl(C):-functor(C,F,A),test_tl(C,F,A).

:-meta_predicate(test_tl(+,+,+)).
test_tl(C,F,A):-current_predicate(thglobal:F/A),call(thglobal:C).
test_tl(C,F,A):-current_predicate(thlocal:F/A),call(thlocal:C).
test_tl(C,F,A):-current_predicate(thlocal_global:F/A),call(thlocal_global:C).


% asserta_if_ground(_):- !.
asserta_if_ground(G):- ground(G),asserta(G),!.
asserta_if_ground(_).


% =====================================================================================================================
:- module_hotrace(user).
% =====================================================================================================================
%:- module_hotrace(logicmoo_util_strings).
% =====================================================================================================================

:-ignore((source_location(File,_Line),module_property(M,file(File)),!,forall(current_predicate(M:F/A),moo_show_childs(M,F,A)))).

:-moo_show_childs(user,prolog_ecall_fa,5).
:-moo_show_childs(user,prolog_ecall,3).
:-moo_show_childs(user,must,1).
:-moo_show_childs(user,must,2).
:-moo_show_childs(user,must_flag,3).
:-moo_show_childs(user,traceok,1).
:-moo_show_childs(user,hotrace,1).

% though maybe dumptrace
default_dumptrace(trace).

ggtrace:- default_dumptrace(DDT), ggtrace(DDT).
ggtrace(Trace):- 
   thread_leash(-call),((visible(+all),visible(-unify),visible(+exception),
   thread_leash(-all),thread_leash(+exception),
   thread_leash(+call))),Trace,thread_leash(-call).

gftrace:- default_dumptrace(DDT), gftrace(DDT).
gftrace(Trace):- 
   thread_leash(-call),((visible(-all), visible(+fail),visible(+call),visible(+exception),
   thread_leash(-all),thread_leash(+exception),
   thread_leash(+call))),Trace,thread_leash(-call).

grtrace:- default_dumptrace(DDT), grtrace(DDT).
grtrace(Trace):- hotrace(( visible(+all),thread_leash(+all))), Trace.

:- thread_local(is_pushed_def/3).

:- meta_predicate(push_def(:)).
push_def(Pred):-must((get_functor(Pred,F,A),prolog_load_context(file,CurrentFile),
   functor_safe(Proto,F,A))),must(forall(clause(Proto,Body),is_pushed_def(CurrentFile,Proto,Body))),!.

: -meta_predicate(pop_def(:)).
pop_def(Pred):-must((get_functor(Pred,F,A),prolog_load_context(file,CurrentFile),
   functor_safe(Proto,F,A))),forall(retract(is_pushed_def(CurrentFile,Proto,Body)),assertz((Proto:-Body))),!.


show_and_do(C):-wdmsg(show_and_do(C)),!,trace,C.



%dtrace:- skipWrapper,!,dmsg(dtrace_skipWrapper).
dtrace:- notrace(dumpST0),notrace(dumpST9),gtrace,trace.


% esa Michele Murer 360-750-7500 ext_135

:-meta_predicate(dtrace(+,0)).
dtrace(MSG,G):-wdmsg(error,MSG),dtrace(G).

:-meta_predicate(dtrace(0)).
dtrace(G):- notrace(get_dtrace(G,DT)),!,call(DT).

get_dtrace(G,call(C,G)):-has_auto_trace(C),wdmsg(has_auto_trace(C,G)),!.
get_dtrace(G,true):- \+ tlbugger:ifCanTrace,!,hotrace((wdmsg((not(tlbugger:ifCanTrace(G)))))),!,badfood(G),!,hotrace(dumpST).
get_dtrace(G,call_cleanup((dumptrace(G)*->trace;trace),trace)):-tracing,notrace,!,wdmsg(tracing_dtrace(G)),!.
get_dtrace(G,dumptrace(G)):-current_predicate(logicmoo_bugger_loaded/0),!.
get_dtrace(G,G).

:-meta_predicate(dumptrace(0)).
%dumptrace(G):- tracing,!,thread_leash(+call),wdmsg(tracing_dumptrace(G)),with_all_dmsg(G).

dumptrace(G):- ignore((debug,
 % catchvv(attach_console,_,true),
 thread_leash(+exception),visible(+exception))),fresh_line,
 repeat, hotrace((fmt(in_dumptrace(G)),
  show_call_failure(get_single_char(C)))),
  with_all_dmsg(dumptrace(G,C)).

:-meta_predicate(dumptrace(0,+)).
dumptrace(_,0'g):-hotrace(dumpST2(_,500000000)),!,fail.
dumptrace(_,0'G):-hotrace(dumpST(_,500000000)),!,fail.
dumptrace(G,0'l):-hotrace(ggtrace),!,G.
dumptrace(G,0's):-hotrace(ggtrace),!,(hotrace(G)*->true;true).
dumptrace(G,0'i):-hotrace(ggtrace),!,ignore(G).
dumptrace(_,0'b):-debug,prolog,!,fail.
dumptrace(_,0'a):-abort,!,fail.
dumptrace(_,0'x):-must((lex,ex)),!,fail.
dumptrace(_,0'e):-halt(1),!.
dumptrace(G,0'l):-visible(+all),show_and_do(rtrace(G)).
dumptrace(G,0'c):-!, show_and_do((G))*->true;true.
dumptrace(G,0'r):-show_and_do(rtrace(G)),!,fail.
dumptrace(G,0'f):-show_and_do(ftrace(G)),!,fail.
dumptrace(G,0't):-visible(+all),thread_leash(+all),trace,!,G.
dumptrace(G,10):-!,dumptrace_ret(G).
dumptrace(G,13):-!,dumptrace_ret(G).
dumptrace(_,C):-fmt(unused_keypress(C)),!,fail.
% )))))))))))))) %
dumptrace_ret(G):-thread_leash(+all),visible(+all),visible(+unify),trace,G.


:-module_transparent(nth_pi/2).
:-module_transparent(nth_goal/2).
:-module_transparent(nth_frame/3).
:-module_transparent(nth_frame_attribute/5).
nth_pi(Nth, Value):- prolog_current_frame(Frame), nth_frame_attribute(Nth,-1, Frame, predicate_indicator, Value).
nth_goal(Nth, Value):- prolog_current_frame(Frame), nth_frame_attribute(Nth,-1, Frame, goal, Value).
nth_frame(Nth, Key, Value):- prolog_current_frame(Frame), nth_frame_attribute(Nth,-1, Frame, Key, Value).
nth_frame_attribute(Nth,NthIn, Frame, Key, Value):-  
 hotrace((
   (NthIn>=0,Nth=NthIn,prolog_frame_attribute(Frame, Key, Value));
   ((prolog_frame_attribute(Frame, parent, ParentFrame),
     NthNext is NthIn + 1, nth_frame_attribute(Nth,NthNext, ParentFrame, Key, Value))))).

in_file_expansion :- nth_pi(LF,_:'$load_file'/_),nth_pi(TL,'$toplevel':_/0),!,LF<TL, 
  (nth_pi(ED,_:'$execute_directive_3'/_)-> (LF<ED) ; true).

in_file_directive :- nth_pi(LF,_:'$load_file'/_),nth_pi(TL,'$toplevel':_/0),!,LF<TL, 
  (nth_pi(ED,_:'$execute_directive_3'/_)-> (LF>ED) ; false).

in_toplevel :- nth_pi(LF,_:'$load_file'/_),nth_pi(TL,'$toplevel':_/0),!,LF>TL, 
  (nth_pi(ED,_:'$execute_directive_3'/_)-> (ED>TL) ; true).


:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),swi_export(F/A),module_transparent(F/A))).

:-dynamic(did_ref_job/1).
do_ref_job(_Body,Ref):-did_ref_job(Ref),!.
do_ref_job(Body ,Ref):-asserta(did_ref_job(Ref)),!,show_call(Body).


% bugger_prolog_exception_hook(error(syntax_error(operator_expected),_),_,_,_).
bugger_prolog_exception_hook(Info,_,_,_):- bugger_error_info(Info),!, dumpST,dmsg(prolog_exception_hook(Info)), dtrace.

bugger_error_info(C):-contains_var(type_error,C).
bugger_error_info(C):-contains_var(instantiation_error,C).
bugger_error_info(C):-contains_var(existence_error(procedure,_/_),C).



% Installs exception reporter.
:- multifile(user:prolog_exception_hook/4).
:- dynamic(user:prolog_exception_hook/4).
% Writes exceptions with stacktrace into stderr.
% Fail/0 call at the end allows the exception to be
% processed by other hooks too.
disabled_this:- asserta((user:prolog_exception_hook(Exception, Exception, Frame, _):- 
 thread_current_error_stream(ERR),
    (   Exception = error(Term)
    ;   Exception = error(Term, _)),
    Term \= type_error(number,_), 
    Term \= type_error(character_code,_), 
    Term \= type_error(character,_), 
    Term \= type_error(text,_), 
    Term \= syntax_error(_), 
    Term \= existence_error(procedure,iCrackers1),
    prolog_frame_attribute(Frame,parent,PFrame),
    prolog_frame_attribute(PFrame,goal,Goal),
    format_to_error( 'Error ST-Begin: ~p', [Term]), nl(ERR),
    ignore((thread_current_input(main,In),see(In))),
    dumpST(Frame,20),

    dtrace(Goal),
    format_to_error( 'Error ST-End: ~p', [Term]), nl(ERR),
    nl(ERR), fail)).


% show the warnings origins
:-multifile(user:message_hook/3). 
:-dynamic(user:message_hook/3).
:- asserta((user:message_hook(Term, Kind, Lines):-  if_defined(buggery_ok), (Kind= warning;Kind= error),Term\=syntax_error(_), 
 current_predicate(logicmoo_bugger_loaded/0), no_buggery,
  dmsg(user:message_hook(Term, Kind, Lines)),hotrace(dumpST(20)),dmsg(user:message_hook(Term, Kind, Lines)),

  % (repeat,get_single_char(C),dumptrace(true,C),!),

   fail)).

% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- user_use_module(library(prolog_stack)).

user:prolog_exception_hook(A,B,C,D):- fail,
   once(copy_term(A,AA)),catchvv(( once(bugger_prolog_exception_hook(AA,B,C,D))),_,fail),fail.

:-moo_hide('$syspreds':thread_leash/1).
%:-moo_hide(thread_leash/1).
:-moo_hide('$syspreds':visible/1).
%:-moo_hide(visible/1).
:-moo_hide(notrace/0).
:-moo_hide(hotrace/1).
:-moo_hide(trace/0).
% :-'$set_predicate_attribute'(!, trace, 1).

:-forall(current_predicate(tlbugger:X),moo_hide(tlbugger:X)).

% :-hideTrace.

%:-module(user).
%:-prolog.

:-retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:-moo_hide_all(with_assertions/2).
:-'$set_predicate_attribute'(with_assertions(_,_), hide_childs, 0).


:- '$find_predicate'(tlbugger:A/0,O),forall(member(M:F/A,O),moo_hide(M,F,A)).

:-moo_hide(dmsg/1).
%:-moo_hide(system:notrace/1). 


:- all_module_predicates_are_transparent(user).
:- module_predicates_are_exported.
:- module_meta_predicates_are_transparent(user).
:- all_module_predicates_are_transparent(logicmoo_util_bugger_catch).

:-module_property(user, exports(List)),moo_show_childs(List).

logicmoo_bugger_loaded.




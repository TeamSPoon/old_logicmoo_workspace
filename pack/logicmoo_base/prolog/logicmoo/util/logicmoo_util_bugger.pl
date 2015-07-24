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
unused_bugger:-module(bugger,[
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
         safe_numbervars/1,
         safe_numbervars/2,
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
     export_all_preds/1 ]).


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



b_moo_hide(M:F/A):-!,'$find_predicate'(F/A,List),forall(member(M:F/A,List),moo_hide(M,F,A)).
b_moo_hide(P):-!,'$find_predicate'(P,List),forall(member(M:F/A,List),moo_hide(M,F,A)).
b_moo_hide(F/A):-!,'$find_predicate'(F/A,List),forall(member(M:F/A,List),moo_hide(M,F,A)).

:- meta_predicate(cnotrace(0)).
cnotrace(G):- notrace(G).
:-moo_hide(cnotrace/1).
:-if_may_hide('$set_predicate_attribute'(cnotrace(_), hide_childs, 1)).
:-if_may_hide('$set_predicate_attribute'(cnotrace(_), trace, 0)).


hotrace:-notrace.
:- export(hotrace/1).
:- meta_predicate(hotrace(0)).
% Unlike notrace/1, it allows traceing when excpetions are raised during Goal.


thread_leash(+Some):-!, (thread_self(main)->leash(+Some);thread_leash(-Some)).
thread_leash(-Some):-!, (thread_self(main)->leash(-Some);thread_leash(-Some)).
thread_leash(Some):-!, (thread_self(main)->leash(+Some);thread_leash(-Some)).

% ((tracing, notrace) -> CC=trace;CC=true), '$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,(('$leash'(_, Old),'$visible'(_, OldV),CC),CC)).
get_hotrace(X,Y):- tracing,!,'$visible'(OldV, OldV),notrace,visible(-all),visible(+exception),Y = call_cleanup(X,('$visible'(_, OldV),trace)).
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
         safe_numbervars/1,
         safe_numbervars/2,
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

:- set_prolog_flag(generate_debug_info, true).

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
rtrace(Goal):- (tracing -> (notrace(get_rtrace(Goal,C)),C) ; (trace,cnotrace(get_rtrace(Goal,C)),C,notrace)).

:-moo_hide(rtrace/1).

:-meta_predicate ftrace(0).
ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   thread_leash(-all),thread_leash(+exception),trace,Goal)).



trace_or_throw(E):- trace_or(throw(E)).

:-export(errx/0).
errx:-debugOnError((assert_if_new(tlbugger:dont_skip_bugger),do_gc,dumpST(10))),!.

% false = use this wrapper, true = code is good and avoid using this wrapper
:- export(skipWrapper/0).
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
:-export(is_ftVar/1).
user:is_ftVar(V):-var(V),!.
user:is_ftVar('$VAR'(_)).
:-export(not_ftVar/1).
not_ftVar(V):-not(is_ftVar(V)).

logOnError(C):-prolog_ecall(0,logOnError0,C).
:-export(logOnError0/1).
logOnError0(C):- catchvv(C,E,dmsg(logOnError(E,C))).
logOnErrorEach(C):-prolog_ecall(1,logOnError,C).
logOnErrorIgnore(C):-prolog:ignore(logOnError0(C)).


:- meta_predicate(one_must(0,0)).
one_must(MCall,OnFail):- skipWrapper,!, (MCall *->  true ;    OnFail).
one_must(MCall,OnFail):- strip_module(MCall,M,Call), '@'(( Call *->  true ;    OnFail ),M).


:-export(transitive/3).
:-meta_predicate(transitive(2,+,-)).
:-meta_predicate(transitive_lc(2,+,-)).
:-meta_predicate(transitive_except(+,2,+,-)).

transitive(X,A,B):- once(debugOnError(call(X,A,R)) -> ( R\=@=A -> transitive_lc(X,R,B) ; B=R); B=A),!.

transitive_lc(X,A,B):-transitive_except([],X,A,B).

transitive_except(NotIn,X,A,B):- memberchk_same(A,NotIn)-> (B=A,!) ;((once(debugOnError(call(X,A,R)) -> ( R\=@=A -> transitive_except([A|NotIn],X,R,B) ; B=R); B=A))),!.


must_det(C):- must(C),!.

one_must_det(Call,_OnFail):-Call,!.
one_must_det(_Call,OnFail):-OnFail,!.

must_det(Call,_OnFail):-Call,!.
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


:-thread_local  tlbugger:skip_use_slow_sanity/0.

% thread locals should defaults to false  tlbugger:skip_use_slow_sanity.

slow_sanity(C):-  sanity(C),!. %  ( tlbugger:skip_use_slow_sanity ; must_det(C)),!.


% -- CODEBLOCK
:- export(sanity/1).
:-meta_predicate(sanity(0)).

% sanity is used for type checking (is not required)
% sanity(Call):-!.
% sanity(_):-!.
% sanity(_):-skipWrapper,!.
sanity(Call):-bugger_flag(release,true),!,assertion(Call).
sanity(G):- tlbugger:show_must_go_on,!,ignore(show_call_failure(G)).
sanity(G):- ignore(must(show_call_failure(G))).


:-export(is_release/0).
is_release :- \+ not_is_release.
:-export(not_is_release/0).
not_is_release :- 1 is random(4).


:-thread_local tlbugger:show_must_go_on/0.
badfood(MCall):- numbervars(MCall,0,_,[functor_name('VAR_______________________x0BADF00D'),attvar(bind),singletons(false)]),dumpST.

% -- CODEBLOCK
:- export(without_must/1).
:-meta_predicate(without_must(0)).

without_must(G):- with_assertions(tlbugger:skipMust,G).

% -- CODEBLOCK
:- export(y_must/2).
:-meta_predicate (y_must(?,0)).
y_must(Y,C):- catchvv(C,E,(wdmsg(E:must_xI__xI__xI__xI__xI_(Y,C)),fail)) *-> true ; dtrace(y_must(Y,C)).

% -- CODEBLOCK
:- export(must/1).
:-meta_predicate (must(0)).
:-set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(200), attributes(portray)]).
:-set_prolog_flag(debugger_show_context,true).

must(C):-cnotrace(get_must(C,CALL)),CALL.

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

:-export(ignore_each/1).
:-meta_predicate(ignore_each(1)).
ignore_each((A,B)):-ignore_each(A),ignore_each(B),!.
ignore_each(A):-ignore(A).

:- meta_predicate
	must_maplist(1, ?),
	must_maplist(2, ?, ?).

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



:- meta_predicate(motrace(0)).
motrace(O):-one_must(hotrace(must(O)),(trace,O)).

:- ensure_loaded(logicmoo_util_bugger_catch).
throw_safe(Exc):-trace_or_throw(Exc).

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


:- export(nop/1).
nop(_).
:- export(dumpST/0).

% functor_safe(P,F,A):- catchvv(compound_name_arity(P,F,A),_,functor(P,F,A)).

erase_safe(Which,REF):- hotrace((var(REF)-> ddmsg(var_erase_safe(Which,REF)) ; erase(REF))).

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

:- export(moo_show_childs/1).
:-module_transparent(moo_show_childs/1).
moo_show_childs(X):- with_preds(X,_M,F,A,_PI,'$hide'(F/A)).

moo_show_childs(M,F,A):-functor_safe(MPred,F,A),moo_show_childs(M,F,A,MPred).
moo_show_childs(M,_,_, MPred):- not(predicate_property(_:MPred,imported_from(M))).
moo_show_childs(M,F,A,_MPred):- moo_trace_hidechilds(M,F,A,0,0).



% ===================================================================
% Substitution based on ==
% ===================================================================
% Usage: dbgsubst(+Fml,+X,+Sk,?FmlSk)

:- export(dbgsubst/4).
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


% ----------
:- export(static_predicate/3).
:- meta_predicate(static_predicate(+,+,+)).
static_predicate(M,F,A):- functor_safe(FA,F,A),  once(M:predicate_property(FA,_)),not(M:predicate_property(FA,dynamic)),not((M:predicate_property(FA,imported_from(Where)),Where \== M)).

static_predicate(A):-atom(F),!,current_predicate(F/A),!,functor(FA,F,A),static_predicate(FA).
static_predicate(F/A):-!,atom(F),current_predicate(F/A),!,functor(FA,F,A),static_predicate(FA).
static_predicate(FA):-predicate_property(FA,built_in),!.
static_predicate(FA):-once(predicate_property(FA,_)),not(predicate_property(FA,dynamic)).

:- export((((dynamic_safe)/1))).
:- meta_predicate(dynamic_safe(+)).
:- module_transparent((((dynamic_safe)/1))).
dynamic_safe(MFA):- with_mfa(MFA,dynamic_safe).

:- export((((dynamic_safe)/3))).
:- meta_predicate(dynamic_safe(+,+,+)).
:- module_transparent((((dynamic_safe)/3))).

convert_to_dynamic(M:FA):- !, get_functor(FA,F,A),convert_to_dynamic(M,F,A).
convert_to_dynamic(FA):- get_functor(FA,F,A), convert_to_dynamic(user,F,A).

convert_to_dynamic(M,F,A):-  functor(C,F,A), M:predicate_property(C,dynamic),!.
convert_to_dynamic(M,F,A):-  M:functor(C,F,A),\+ predicate_property(C,_),!,M:((dynamic(F/A),multifile(F/A),export(F/A))),!.
convert_to_dynamic(M,F,A):-  functor(C,F,A),predicate_property(C,number_of_clauses(_)),\+ predicate_property(C,dynamic),
  findall((C:-B),clause(C,B),List),M:abolish(F,A),dynamic(M:F/A),multifile(M:F/A),export(F/A),maplist(assertz,List),!.
convert_to_dynamic(M,F,A):-  functor(C,F,A),findall((C:-B),clause(C,B),List),abolish(F,A),dynamic(M:F/A),multifile(M:F/A),export(F/A),maplist(assertz,List),!.

dynamic_safe(M,F,A):- functor(C,F,A),predicate_property(C,imported_from(system)),!,dmsg(warn(predicate_property(M:C,imported_from(system)))).
dynamic_safe(M,F,A):- (static_predicate(M,F,A) -> show_call(convert_to_dynamic(M,F,A)) ; logOnErrorIgnore((dynamic(M:F/A),multifile(M:F/A)))). % , warn_module_dupes(M,F,A).
:-op(1150,fx,user:dynamic_safe).


% pred_prop(Spec,DO,TEST,DONT)
pred_prop((M:F/A),DO,TEST,true):-pred_prop(M:F/A,DO,TEST).
pred_prop(M:F/A,(lock_predicate(M:F/A)),(built_in),unlock_predicate(M:F/A)).
pred_prop(M:F/A, (dynamic(M:F/A)) ,(dynamic), show_call(compile_predicates([F/A]))).

pred_prop(_,(meta_predicate Spec),(meta_predicate Spec)).
pred_prop(M:F/A,multifile(M:F/A)	       ,(multifile)).
pred_prop(M:F/A,module_transparent(M:F/A) ,(transparent)).
pred_prop(M:F/A,discontiguous(M:F/A) ,(discontiguous)).
pred_prop(M:F/A,volatile(M:F/A)	  ,(volatile)).
pred_prop(M:F/A,public(M:F/A)     ,(public)).
pred_prop(M:F/A,thread_local(M:F/A),(thread_local)).
pred_prop(M:F/A,noprofile(M:F/A)	    , (noprofile)).
pred_prop(M:F/A,'$iso'(M:F/A) ,(iso)).


:-dynamic(mpred_impl/2).
:-multifile(mpred_impl/2).

%%  rebuild_pred_into(OMC,NMC,AssertZ,[+dynamic,-built_in,+volatile, etc]).

:-meta_predicate(rebuild_pred_into(0,1,?)).
rebuild_pred_into(C,AssertZ,OtherTraits):-rebuild_pred_into(C,C,AssertZ,OtherTraits).

:-meta_predicate(rebuild_pred_into(0,0,1,?)).
rebuild_pred_into(_,NMC,AssertZ,_):-mpred_impl(NMC,AssertZ),!.
rebuild_pred_into(OMC,NMC,AssertZ,OtherTraits):-
  lsting(OMC),
  asserta(mpred_impl(NMC,AssertZ)),
  show_call((predicate_property(OMC,number_of_clauses(_)))),
  strip_module(OMC, OM, OC),
  strip_module(NMC, NM, NC),
   must_det_l((
      '$set_source_module'(Before, OM),  
      functor(NC,NF,A), functor(OC,OF,A), 
      (show_call(predicate_property(OMC,number_of_clauses(_)))),
      must(show_call_failure(predicate_property(OMC,number_of_clauses(_)))),
      forall(predicate_property(OC,PP),asserta(pp_temp(NC,PP))),
      findall((OC:-B),((clause(OC,B),assertz(pp_clauses((OC:-B))))),List),
      '$set_source_module'(_, NM),
      forall(member(-PP,OtherTraits),retractall(pp_temp(NC,PP))),
      forall(member(+PP,OtherTraits),asserta(pp_temp(NC,PP))),      
      once(pp_temp(NC,(built_in))->(redefine_system_predicate(NF/A),unlock_predicate(NF/A));true),      
      show_call(must(abolish(NF/A))),
      show_call(must(abolish(NF/A))),
      garbage_collect_clauses,
      ignore(convert_to_dynamic(NM,NF,A)),
      garbage_collect_clauses,
      %must(\+ predicate_property(NMC,_)),
      %once(memberchk(CC,List)->true;(CC=((NC:-fail,1234)))),
      %convert_to_dynamic(NM,NF,A),
      %ignore(logOnError(pp_temp(NC,(dynamic))->dynamic(NF/A);true)),
      %ignore(once(pp_temp(NC,(multifile))->multifile(NF/A);true)),
      must(((pp_temp(NC,file(File)),pp_temp(NC,line_count(_Line))))
        -> 
            must(('$compile_aux_clauses'(CC, File),retractall(CC)));
            must(dmsg(noFileFor(NC)))),
      forall(pred_prop(NM:NF/A,TODO,PP,ELSE),(pp_temp(NC,PP)->must(TODO);must(ELSE))),
      (pp_temp(NC,meta_predicate(NC))->meta_predicate(NC);true),
      dbgsubst(List,OF,NF,ListO),maplist(AssertZ,ListO),!,
      
      retractall(mpred_impl(NMC,_)),
      asserta(mpred_impl(NMC,AssertZ)),
      '$set_source_module'(_, Before),
      lsting(NMC),      
      retractall(pp_temp(NC,_))
      )).


% ----------
:- export(with_pi/2).
:- module_transparent(with_pi/2).
:- meta_predicate(with_pi(0,4)).
with_pi(_:[],_):-!.
with_pi(M:(P1,P2),Pred3):-!,'@'(( with_pi(M:P1,Pred3),with_pi(M:P2,Pred3) ),M).
with_pi(M:P,Pred3):-context_module(CM),!,with_pi_selected(CM,M,P,Pred3),!.
with_pi([],_):-!.
with_pi((P1,P2),Pred3):-!, with_pi(P1,Pred3),with_pi(P2,Pred3).
with_pi(P,Pred3):-context_module(CM),with_pi_selected(CM,CM,P,Pred3),!.

:- export(with_pi_selected/4).
:- meta_predicate(with_pi_selected(+,+,+,+)).
with_pi_selected(CM, M,[P|L],Pred3):-!,with_pi_selected(CM,M,P,  Pred3),with_pi_selected(CM,M,L,Pred3).
with_pi_selected(CM,M,(P,L),Pred3):-!,with_pi_selected(CM,M,P,  Pred3),with_pi_selected(CM,M,L,Pred3).
with_pi_selected(_CM,_M,[]  ,_Pred3):-!.
with_pi_selected(CM,M,[P]  ,Pred3):-!,with_pi_selected(CM,M,P,  Pred3).
with_pi_selected(CM,_, M:F//A,Pred3):-Ap2 is A+2, !,with_pi_selected(CM,M,F/Ap2,Pred3).
with_pi_selected(CM,_, M:F/A,Pred3):-!,with_pi_selected(CM,M,F/A,Pred3).
with_pi_selected(CM,_, M:P ,Pred3):-!,with_pi_selected(CM,M,P,  Pred3).
with_pi_selected(CM,M, F//A ,Pred3):-Ap2 is A+2,!,functor_safe(P,F,A),  with_pi_stub(CM,M,P,F/Ap2,Pred3).
with_pi_selected(CM,M, F/A ,Pred3):-!,functor_safe(P,F,A),  with_pi_stub(CM,M,P,F/A,Pred3).
with_pi_selected(CM,M, P ,Pred3):-  functor_safe(P,F,A), with_pi_stub(CM,M,P,F/A,Pred3).


% ----------



:- export(with_pi_stub/5).
:- meta_predicate(with_pi_stub(+,+,+,+,3)).
with_pi_stub(CM, M,P, F//A , PM:Pred3):- ((integer(A),atom(M),atom(F),Ap2 is A+2, functor_safe(P,F,Ap2))),
  must(PM:call(Pred3,CM, M,P,F/Ap2)),!.
with_pi_stub(CM, M,P, F/A , PM:Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),
  must(PM:call(Pred3,CM, M,P,F/A)),!.
with_pi_stub(CM, M,P, F//A , Pred3):- ((integer(A),atom(M),atom(F),Ap2 is A+2,functor_safe(P,F,Ap2))),
   must(call(Pred3,CM, M,P,F/Ap2)),!.
with_pi_stub(CM, M,P, F/A , Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),
   must(call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A ,_: Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),  must(call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A ,CP: Pred3):-!, must(CP:call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A , Pred3):-!, must(call(Pred3,CM, M,P,F/A)),!.

with_pi_stub(CM, M,P,FA,Pred3):- trace_or_throw(invalide_args(CM, M,P,FA,Pred3)).
% ----------

:- export(with_mfa/2).
:- module_transparent(with_mfa/2).
:- meta_predicate(with_mfa(0,3)).
with_mfa(P  ,Pred3):- with_pi(P,with_mfa_of(Pred3)).

:- module_transparent(with_mfa_of/5).
:- meta_predicate(with_mfa_of(3,+,+,+,+)).
with_mfa_of(Pred3,_CM,M,_P,F//A):- Ap2 is A+2, M:call(Pred3,M,F,Ap2).
with_mfa_of(Pred3,_CM,M,_P,F/A):-M:call(Pred3,M,F,A).

% ----------


:-module_transparent(make_transparent/4).
:- export(make_transparent/4).

make_transparent(_CM,M,_PI,F/0):-!, compound_name_arity(C,F,0), M:meta_predicate(C).
make_transparent(CM,_,PI,M:F/A):-!,make_transparent(CM,M,PI,F/A).
make_transparent(_CM,M,PI,F/A):-
   motrace(((var(PI)->functor_safe(PI,F,A);true),
   M:module_transparent(F/A),
   fill_args(PI,('?')),!,
   dbgsubst(PI, (^),(^),PI1),
   dbgsubst(PI1,(0),(0),PI2),
   dbgsubst(PI2,(:),(:),PI3),
   (compound(PI3) -> M:meta_predicate(PI3) ; true))).

% ----------

:- export((dynamic_multifile_exported)/1).
:- export((dynamic_multifile_exported)/4).
:- meta_predicate(( dynamic_multifile_exported(:), dynamic_multifile_exported(+,+,+,+))).
:- module_transparent((dynamic_multifile_exported)/1).
dynamic_multifile_exported( (M:F)/A ):- !,context_module(CM),
'@'((dynamic_safe(M,F,A), 
   (static_predicate(M,F,A) -> true ; ((M:multifile(F/A),CM:multifile(F/A)))),
   % make_transparent(CM,M,PI,F/A),
   M:export(F/A)),M). %,dmsg(dynamic_multifile_exported(CALL)).
dynamic_multifile_exported( FA ):- with_pi(FA,(dynamic_multifile_exported)).
dynamic_multifile_exported(CM, M, _PI, F/A):-
'@'((dynamic_safe(M,F,A), 
   (static_predicate(M,F,A) -> true ; ((M:multifile(F/A),CM:multifile(F/A)))),
   % make_transparent(CM,M,PI,F/A),
   M:export(F/A)),M). %,dmsg(dynamic_multifile_exported(CALL)).

% ----------


:- export(fill_args/2).
fill_args([Arg|More],With):-!,ignore(With=Arg),fill_args(More,With).
fill_args([],_).
fill_args(PI,With):-compound_name_arguments(PI,_,ARGS),fill_args(ARGS,With).

:- meta_predicate(meta_predicate(0)).


:- export(def_meta_predicate/3).
:- meta_predicate((def_meta_predicate(0,+,+))).

def_meta_predicate(M:F,S,E):-!,M:doall(((between(S,E,N),make_list('?',N,List),compound_name_arguments(CALL,F,List),'@'(meta_predicate(CALL),M)))).
def_meta_predicate(F,S,E):- trace_or_throw(def_meta_predicate(F,S,E)).



:- meta_predicate(moo_hide_all(0)).
:- export(moo_hide_all/1).
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

:- export(moo_hide_all/1).



%=========================================
% Module Utils
%=========================================
:- export(moo_hide_childs/1).
:-module_transparent(moo_hide_childs/1).
:- meta_predicate(moo_hide_childs(0)).

% module_functor(PredImpl,Module,Pred,Arity).

module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).
strip_arity(Pred/Arity,Pred,Arity).
strip_arity(PredImpl,Pred,Arity):-functor_safe(PredImpl,Pred,Arity).
% moo_hide_childs(+Pred).
:- export(moo_hide_childs/1).

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

:- moo_trace_hidechilds(logicmoo_util_bugger_catch,catchvv,3,0,0).
:- moo_trace_hidechilds(system,catch,3,0,0).


%:-multifile( tlbugger:bugger_prolog_flag/2).
%:- export( tlbugger:bugger_prolog_flag/2).


:-meta_predicate(callsc(0)).
callsc(G):-G.
:-moo_hide(callsc/1).
:- current_predicate(M:callsc/1),moo_trace_hidechilds(M,callsc,1,0,0).



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

define_if_missing(M:F/A,List):-current_predicate(M:F/A)->true;((forall(member(C,List),M:assertz(C)),export(M:F/A))).

define_if_missing(system:atomics_to_string/3, [
  ( system:atomics_to_string(List, Separator, String):- new_a2s(List, Separator, String) ) ]).

define_if_missing(system:atomics_to_string/2, [
  ( system:atomics_to_string(List, String):- new_a2s(List, '', String) ) ]).

new_a2s(List, Separator, String):-catchvv(new_a2s0(List, Separator, String),_,((trace,new_a2s0(List, Separator, String)))).
new_a2s0(List, Separator, String):- debug,
 (atomic(String) -> (string_to_atom(String,Atom),concat_atom(List, Separator, Atom));
     (concat_atom(List, Separator, Atom),string_to_atom(String,Atom))).



:- export(bad_idea/0).
bad_idea:-fail.

/*
:- export(static_predicate/1).
:- meta_predicate(static_predicate(:)).
:- module_transparent(static_predicate/1).
static_predicate(FA):-once(predicate_property(FA,_)),not(predicate_property(FA,dynamic)).
*/

% ===================================================================
% Safely number vars
% ===================================================================
bugger_numbervars_with_names(Term):-
   term_variables(Term,Vars),bugger_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!.

bugger_name_variables([]).
bugger_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   bugger_name_variables(Vars).


numbervars_impl(Term,Start,List):- integer(Start),!,numbervars_impl(Term,'$VAR',Start,List).
numbervars_impl(Term,Functor,Start):- integer(Start),atom(Functor),!,numbervars_impl(Term,Functor,Start,_End).
numbervars_impl(Term,Functor,List):- is_list(List),atom(Functor),!, term_variables(Term,Vars),bugger_name_variables(Vars),!,must(( numbervars(Term,0,_End,[functor_name(Functor)|List]))).

numbervars_impl(Term,Start,End,List):-number(Start),is_list(List),!,must(( numbervars(Term,Start,End,List) )).
numbervars_impl(Term,Functor,Start,List):- is_list(List),sanity(integer(Start)),!,must(( numbervars(Term,Start,_End,[functor_name(Functor)|List]))).
numbervars_impl(Term,Functor,Start,End):- !,debugOnError((numbervars(Term,Start,End,[attvar(skip),functor_name(Functor),singletons(true)]))).
numbervars_impl(Term,Functor,Start,End):- sanity((must(var(End);integer(End)),numbervars(Term,Start,End,[attvar(skip),functor_name(Functor),singletons(true)]))).

numbervars_impl(Term,Functor,Start,End,List):-must(( must(var(End);number(End)),numbervars(Term,Start,End,[functor_name(Functor)|List]))).


:- export(snumbervars/3).
snumbervars(Term,Start,End):- integer(Start),!,numbervars_impl(Term,'$VAR',Start,End).
:- export(snumbervars/4).
snumbervars(Term,Functor,Start,List):-numbervars_impl(Term,Functor,Start,List).
:- export(snumbervars/1).
snumbervars(Term):-numbervars_impl(Term,0,_).

% ===================================================================
% Loop checking
% ===================================================================
:- thread_local tlbugger:ilc/1.

:-meta_predicate(call_t(0)).
% call_t(C0):-reduce_make_key(C0,C),!,table(C),!,query(C).
% call_t(C0):-query(C).
call_t(C):- call(C).

reduce_make_key(call(C),O):-!,reduce_make_key(C,O).
reduce_make_key(call_u(C),O):-!,reduce_make_key(C,O).
reduce_make_key(req(C),O):-!,reduce_make_key(C,O).
reduce_make_key(pfc_call(C),O):-!,reduce_make_key(C,O).
reduce_make_key(must(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats_old(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats_old(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(call_tabled(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_loop_check_unsafe(C),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check(C),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check(C,_),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check_term_key(C,_,_),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check_term(C,_,_),O):-!,reduce_make_key(C,O).
reduce_make_key(fact_loop_checked(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(V+C,V+O):-!,reduce_make_key(C,O).
reduce_make_key(M:C,O):-atom(M),!,reduce_make_key(C,O).
reduce_make_key(O,O).


cc_key(CC,Key):- cyclic_term(CC),copy_term(CC,CKey,_),numbervars(CKey,0,_),format(atom(Key),'~w',[CKey]),!.
cc_key(CC,Key):- copy_term(CC,Key,_),numbervars(Key,0,_),!.

make_key(M:CC,Key):- atom(M),!,hotrace((ground(CC)->Key=CC ; cc_key(CC,Key))).
make_key(CC,Key):- hotrace((ground(CC)->Key=CC ; cc_key(CC,Key))).


%is_loop_checked(Call):-  make_key(Call,Key),!,tlbugger:ilc(Key).



loop_check(Call):- parent_goal(Term,2)->loop_check_term_key(Call,Term+Call,fail).
loop_check(Call, TODO):- parent_goal(Term,2)->loop_check_term_key(Call,Term+Call, TODO).
loop_check_term_key(Call,KeyIn,TODO):- make_key(KeyIn,Key) -> loop_check_term(Call,Key,TODO).


no_loop_check(Call):- parent_goal(Term,2)->no_loop_check_term_key(Call,Term+Call,fail).
no_loop_check(Call, TODO):- parent_goal(Term,2)->no_loop_check_term_key(Call,Term+Call, TODO).
no_loop_check_term_key(Call,KeyIn,TODO):- make_key(KeyIn,Key) -> with_no_assertions(tlbugger:ilc(_),loop_check_term(Call,Key,TODO)).


loop_check_term(Call,Key,TODO):- TT = tlbugger:ilc(Key),
 ( \+(TT) -> (setup_call_cleanup(asserta(TT,REF), Call, 
   erase_safe(TT:loop_check_term(Call,Key,TODO),REF))) ; 
   ((can_fail(TODO)->retract_can_table;true),call(TODO)) ).

can_fail(G):-not(G=true),not(G=must(_)).

% get_where(When)
get_where(B:L):-get_where0(F:L),file_base_name(F,B).
get_where0(F:L):-source_location(file,F),current_input(S),line_position(S,L),!.
get_where0(F:L):-source_location(F,L),!.
get_where0(A:0):-current_input(S),stream_property(S,alias(A)),!.
get_where0(M:0):-context_module(M),!.
get_where0(user:0):-!.

lco_goal_expansion(V,V):- \+ compound(V),!.
lco_goal_expansion(loop_check(G),O):-!,lco_goal_expansion(loop_check(G,fail),O).
lco_goal_expansion(loop_check(G,TODO),loop_check_term_key(G,G:W,TODO)):-must(get_where(W)).
lco_goal_expansion(no_loop_check(G),O):-!,lco_goal_expansion(no_loop_check(G,fail),O).
lco_goal_expansion(no_loop_check(G,TODO),no_loop_check_term_key(G,G:W,TODO)):-must(get_where(W)).
lco_goal_expansion(B,A):- compound_name_arguments(B,F,ARGS),maplist(lco_goal_expansion,ARGS,AARGS),compound_name_arguments(A,F,AARGS).

:-meta_predicate(show_call_entry(0)).
show_call_entry(Call):-wdmsg(show_call_entry(Call)),show_call(Call).

% ===================================================================
% Bugger Term Expansions
% ===================================================================
always_show_dmsg:-thread_self(main).

is_hiding_dmsgs:- \+always_show_dmsg, current_prolog_flag(opt_debug,false),!.
is_hiding_dmsgs:- \+always_show_dmsg, tlbugger:ifHideTrace,!.

% bugger_debug=false turns off just debugging about the debugger
% opt_debug=false turns off all the rest of debugging
% ddmsg(_):-current_prolog_flag(bugger_debug,false),!.
ddmsg(D):-format(user_error,'~Ndmsg: ~q~n',[D]).
ddmsg_call(D):- ( (ddmsg(ddmsg_call(D)),call(D),ddmsg(ddmsg_exit(D))) *-> true ; ddmsg(ddmsg_failed(D))).


:-moo_show_childs(must(0)).

:- export(wdmsg/1).
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


:- export(remove_pred/3).
remove_pred(_,_,_):-!.
remove_pred(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred(M,F,A):- functor(P,F,A),
  (current_predicate(M:F/A) -> ignore((catchvv(redefine_system_predicate(M:P),_,true),abolish(M:F,A)));true),
  M:asserta((P:-user:wdmsg(error(P)),throw(permission_error(M:F/A)))).

:- export(fixhotrace/1).
fixhotrace(X):- tracing -> (call(X),trace) ; call(X).
:-moo_hide_all(fixhotrace(0)).

:- export(hidetrace/1).
hidetrace(X):- X.
:-moo_hide_all(hidetrace(0)).

:- export( tlbugger:use_bugger_expansion/0).
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


:- meta_predicate(call_if_defined(0)).
:- export(call_if_defined/1).
call_if_defined(G):-current_predicate(_,G),G.


:-module_transparent(p_predicate_property/2).
p_predicate_property(P,PP):-predicate_property(P,PP),!.
p_predicate_property(_:P,PP):-predicate_property(P,PP).
%current_bugger_predicate(M:FF/FA):-nonvar(FF),!,current_predicate(M:FF,FA).
%current_bugger_predicate(FF/FA):-nonvar(FF),!,!,current_predicate(FF/FA).
:-module_transparent(current_predicate_module/2).
current_predicate_module(P,M):-var(P),!,current_predicate(F/A),functor_safe(P,F,A),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(OM:F/A,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(OM:P,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(F/A,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(P,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).

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
bugger_goal_expansion(CM,T,T3):- once(bugger_t_expansion(CM,T,T2)),T\==T2,!,catchvv(expand_term(T2,T3),_,fail).

:-meta_predicate(bugger_expand_goal(0,-)).
bugger_expand_goal(T,_):- fail,ddmsg(bugger_expand_goal(T)),fail.

:-meta_predicate(bugger_expand_term(0,-)).
bugger_expand_term(T,_):- fail, ddmsg(bugger_expand_term(T)),fail.

:- export(format_safe/2).
format_safe(A,B):-catchvv(format(A,B),E,(dumpST,dtrace(E:format(A,B)))).

:-meta_predicate(bugger_term_expansion(:,-)).
bugger_term_expansion(CM:T,TT):- compound(T),  tlbugger:use_bugger_expansion,!,bugger_term_expansion(CM,T,TT).
:-meta_predicate(bugger_term_expansion(+,+,-)).
bugger_term_expansion(CM,T,T3):- once(bugger_t_expansion(CM,T,T2)),T\==T2,!,nop(ddmsg(T\==T2)),catchvv(expand_term(T2,T3),_,fail).

% user:     expand_goal(G,G2):- compound(G),bugger_expand_goal(G,G2),!.


% user:goal_expansion(G,G2):- compound(G),bugger_goal_expansion(G,G2).

% user:expand_term(G,G2):- compound(G),bugger_expand_term(G,G2),!.


:- export(traceok/1).
:- multifile current_directory_search/1.
:- module_transparent current_directory_search/1.
:- meta_predicate(hotrace(0)).
:- meta_predicate(traceok(0)).

%:- meta_predicate((loop_check(0,0))).
%:- meta_predicate((no_loop_check(0,0))).
%:- meta_predicate((no_loop_check(0))).
%:- meta_predicate((no_loop_check_unsafe(0))).
:- meta_predicate((loop_check_term(0,?,0))).
:- meta_predicate((loop_check_term_key(0,?,0))).
%:- meta_predicate((loop_check(0,0))).
%:- meta_predicate((loop_check(0))).

:- export(mstatistics/0).
mstatistics:-
  garbage_collect,
  garbage_collect_atoms,
  statistics,
  statistics(stack,Om),O is Om/1000000,
  statistics(clauses,C),
  statistics(memory,[Tm,_]),T is Tm/1000000,
  statistics(atoms,[A,Mm,0]),AM is Mm/1000000,
  OdC is O/C,
  OdA is (A/1000)*39,
  PerAtom is (Mm/A),
  b_i(L,NA),
  save_atoms,
  fmt((stack/clauses/mem/new + O/C/T-Tm/NA = c(OdC)/a(OdA-AM-PerAtom))),!,
  (NA<1000->fmt(L);true).

current_atom_or_blob(X,atom):-current_atom(X).
current_atom_or_blob(X,blob(T)):-current_blob(X,T).
current_atom_or_blob(X,functor_safe(Y)):-current_functor(X,Y).
current_atom_or_blob(X,key):-current_key(X).
current_atom_or_blob(X,flag):-current_key(X).

blob_info(A,atom,blob(A,text)).
blob_info(A,blob(text),blob(A,text)).
blob_info(A,functor_safe(Y),functor_safe(A,Y)).
blob_info(A,key,key(A,Y)):-findall(V,recorded(A,V),Y).
blob_info(A,flag,flag(A,Y)):-flag(A,Y,Y).
blob_info(A,blob(clause),blob(A,T,Y,H,B)):-T=clause, findall(V,clause_property(A,V),Y),(clause(H,B,A)->true;H=dead).
blob_info(A,blob(record),blob(A,T,Y)):-T=record,with_output_to(string(Y),print_record_properties(A, current_output)).
% blob_info(A,blob(T),blob(A,T,Y)):-with_output_to(string(Y),prolog_term_view:emit_term(A, [])).
blob_info(A,blob(T),blob(A,T)).

:- export(saved_current_atom/2).
:-dynamic(saved_current_atom/2).
:- export(new_atoms/2).
new_atoms(X,Type):-current_atom_or_blob(X,Type),not(saved_current_atom(X,Type)).
:- export(save_atoms/0).
save_atoms:-forall(new_atoms(X,Type),assert(saved_current_atom(X,Type))).
:- export(b_i/2).
b_i(L,NA):-findall(W,(new_atoms(X,Type),once(blob_info(X,Type,W))),LL),list_to_set(LL,L),length(L,NA).
print_record_properties(Record, Out) :-
	format(Out, 'Record reference ~w~n', [Record]),
	(   recorded(Key, Value, Record)
	->  format(Out, ' Key:   ~p~n', [Key]),
	    format(Out, ' Value: ~p~n', [Value])
	;   format(Out, ' <erased>~n', [])
	).

print_clause_properties(REF, Out) :-
	format(Out, 'Clause reference ~w~n', [REF]),
	(   clause(Head, Body, REF)
	->  nl(Out),
	    portray_clause(Out, (Head:-Body))
	;   format(Out, '\t<erased>~n', [])
	).




thread_local_leaks:-!.

in_pengines:- relative_frame(context_module,pengines,_).

% ?- relative_frame(context_module,X,Y).
:- export(relative_frame/3).
relative_frame(Attrib,Term,Nth):- parent_frame_attribute(Attrib,Term,Nth,_RealNth,_FrameNum).

:- export(parent_goal/2).
parent_goal(Term,Nth):- parent_frame_attribute(goal,Term,Nth,_RealNth,_FrameNum).
:- export(parent_frame_attribute/5).
parent_frame_attribute(Attrib,Term,Nth,RealNth,FrameNum):-hotrace((ignore(Attrib=goal),prolog_current_frame(Frame),
                                                current_frames(Frame,Attrib,5,NextList))),!,nth1(Nth,NextList,RealNth-FrameNum-Term).


prolog_frame_match(Frame,goal,Term):-!,prolog_frame_attribute(Frame,goal,TermO),!,Term=TermO.
prolog_frame_match(Frame,parent_goal,Term):-nonvar(Term),!,prolog_frame_attribute(Frame,parent_goal,Term).
prolog_frame_match(Frame,not(Attrib),Term):-!,nonvar(Attrib),not(prolog_frame_attribute(Frame,Attrib,Term)).
prolog_frame_match(_,[],X):-!,X=[].
prolog_frame_match(Frame,[I|IL],[O|OL]):-!,prolog_frame_match(Frame,I,O),!,prolog_frame_match(Frame,IL,OL),!.
prolog_frame_match(Frame,Attrib,Term):-prolog_frame_attribute(Frame,Attrib,Term).

current_frames(Frame,Attrib,N,NextList):- N>0, N2 is N-1,prolog_frame_attribute(Frame,parent,ParentFrame),!,current_frames(ParentFrame,Attrib,N2,NextList).
current_frames(Frame,Attrib,0,NextList):- current_next_frames(Attrib,1,Frame,NextList).

current_next_frames(Attrib,Nth,Frame,[Nth-Frame-Term|NextList]):- prolog_frame_match(Frame,Attrib,Term), !,
   (prolog_frame_attribute(Frame,parent,ParentFrame) -> 
    ( Nth2 is Nth+1, current_next_frames(Attrib,Nth2, ParentFrame,NextList));
         NextList=[]).
current_next_frames(Attrib,Nth,Frame,NextList):- 
   (prolog_frame_attribute(Frame,parent,ParentFrame) -> 
    ( Nth2 is Nth+1, current_next_frames(Attrib,Nth2, ParentFrame,NextList));
         NextList=[]).
current_next_frames(_,_,_,[]).


trace_or(E):- dumpST,dmsg(E),trace,dtrace,!.
trace_or(E):- trace,E.

has_gui_debug :- getenv('DISPLAY',NV),NV\==''.

:- export(nodebugx/1).
:- module_transparent(nodebugx/1).
nodebugx(X):- 
  with_no_assertions(tlbugger:ifCanTrace,
   with_assertions(tlbugger:ifWontTrace,
    with_assertions(tlbugger:show_must_go_on,
       with_assertions(tlbugger:ifHideTrace,hotrace(X))))).


% :- use_module(library(prolog_stack)).

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

:- export(meta_interp/2).
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

:- export((user_ensure_loaded/1)).
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
% :- export(no_loop_check/1).
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

% ===================================================================

:-thread_local  tlbugger:attributedVars.

%  tlbugger:attributedVars.

:- export(must_not_repeat/1).
:-meta_predicate(must_not_repeat(0)).
must_not_repeat(C):-call(C).

% ===================================================
% 
% no_repeats(:Call) 
%
% Like call/1 but ony succeeds only unique variabes
% 
% logicmoo_mud:  ?- no_repeats(member(X,[3,1,1,1,3,2])).
% X = 3 ;
% X = 1 ;
% X = 2.
% ===================================================




no_repeats_av:-tlbugger:attributedVars.

:- export(no_repeats/1).
:- meta_predicate no_repeats(0).

no_repeats(Call):- tlbugger:old_no_repeats,!, no_repeats_old(Call).
no_repeats(Call):- no_repeats_av,!,no_repeats_av(Call).
no_repeats(Call):- no_repeats_old(Call).


:- export(no_repeats/2).
:- meta_predicate no_repeats(+,0).
no_repeats(Vs,Call):- tlbugger:old_no_repeats,!,no_repeats_old(Vs,Call).
no_repeats(Vs,Call):- no_repeats_av,!,no_repeats_av(Vs,Call).
no_repeats(Vs,Call):- no_repeats_old(Vs,Call).

/*
no_repeats_dif(Vs,Call):- dif(Vs,_), get_attr(Vs,dif,vardif(CONS,_)),!,
  Call,copy_term_nat(Vs,C),nb_setarg(2, CONS, [_-C|CONS]).

*/

% ===================================================
% 
% no_repeats_old([+Vars,]:Call) 
% 
% Like call/1 but ony succeeds on unique free variabes
% 
% logicmoo_mud:  ?- no_repeats( X , member(X-Y,[3-2,1-4,1-5,2-1])).
% X = 3, Y = 2 ;
% X = 1, Y = 4 ;
% X = 2, Y = 1.
% ===================================================
:- export(no_repeats_old/1).
:- meta_predicate no_repeats_old(0).
no_repeats_old(Call):- no_repeats_old(Call,Call).

:- export(no_repeats_old/2).
:- meta_predicate no_repeats_old(+,0).

:-use_module(library(logicmoo/util/rec_lambda)).

memberchk_same(X, [Y|Ys]) :- (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),memberchk_same(X, Ys) )). 

memberchk_pred(Pred, X, [Y|Ys]) :- (   call(Pred,X,Y) -> true ;   (nonvar(Ys),memberchk_pred(Pred, X, Ys) )). 
memberchk_pred_rev(Pred, X, [Y|Ys]) :- (   call(Pred,Y,X) -> true ;   (nonvar(Ys),memberchk_pred_rev(Pred,X, Ys) )). 


no_repeats_old(Vs,Call):- CONS = [_], (Call), hotrace(( \+ memberchk_same(Vs,CONS), copy_term(Vs,CVs), CONS=[_|T], nb_setarg(2, CONS, [CVs|T]))).

mcs_t2(A,B) :- call(lambda(X, [Y|Ys], (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),reenter_lambda(X, Ys) ))),A,B). 
mcs_t(A,B) :- call(lambda(X, [Y|Ys], (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),reenter_lambda(X, Ys) ))),A,B). 


no_repeats_t(Vs,Call):- CONS = [_], (Call), 
  (( \+ call(lambda(X, [Y|Ys], (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),reenter_lambda(X, Ys) ))),Vs,CONS),
       copy_term(Vs,CVs), CONS=[_|T], nb_linkarg(2, CONS, [CVs|T]))).

% 

:- export(no_repeats_u/2).
:- meta_predicate no_repeats_u(+,0).
no_repeats_u(Vs,Call):- CONS = [_], (Call), hotrace((  CONS=[_|T],
    \+ memberchk_pred_rev(subsumes_term,Vs,T), copy_term(Vs,CVs), nb_setarg(2, CONS, [CVs|T]))).



% for dont-care vars
:- export(no_repeats_dc/2).
:- meta_predicate no_repeats_dc(+,0).
no_repeats_dc(Vs,Call):- term_variables(Call,CV),term_variables(Vs,VsL),subtract_eq(CV,VsL,NewVs),no_repeats(NewVs,Call).

subtract_eq([], _, []) :- !.
subtract_eq([A|C], B, D) :-
        memberchk_same(A, B), !,
        subtract_eq(C, B, D).
subtract_eq([A|B], C, [A|D]) :-
        subtract_eq(B, C, D).


% ===================================================
%
%  no_repeats_av/1 - Filter repeats using coroutining
%
% Same as no_repeats(:Call) (so same as call/1 but fitered)
%
% (everytime we see new value.. we add it to was/2 in an attributed variable that we have a refernce in a compound)
% Cehcked via ?- was(AVar,Foo), get_attrs(AVar,ATTRS1), get_attrs(AVar,ATTRS2), ATTRS1==ATTRS2.
%
%  So the variable binding gerts rejected several frames below your code? ( are we nipping away futile bindings?)
% 
% however ..
%     does that mess with anything in code that we are calling?
%  Could some peice of code been required to see some binding to make a side effect come about?
%  
% logicmoo_mud:  ?- no_repeats_av(member(X,[3,1,1,1,3,2])).
% X = 3 ;
% X = 1 ;
% X = 2.
%
% attributed variable verson of getting filtered bindings
% ===================================================
:- export(no_repeats_av/1).
:-meta_predicate(no_repeats_av(0)).
:- export(no_repeats_av/2).
:-meta_predicate(no_repeats_av(+,0)).
:- export(no_repeats_av_l/2).
:-meta_predicate(no_repeats_av_l(+,0)).

no_repeats_av(AVar,Call):- var(AVar),!,
      setup_call_cleanup(
       (was(AVar,iNEVER),asserta(tlbugger:cannot_save_table,Ref),get_attr(AVar,was,varwas(CONS,_))),
        (Call,copy_term_nat(AVar,C),nb_linkarg(2, CONS, [_-C|CONS])),
        (del_attr(AVar,was),erase(Ref))).
no_repeats_av(List,Call):- is_list(List),!,no_repeats_av_l(List,Call).
no_repeats_av(Term,Call):-term_variables(Term,List),!,no_repeats_av_l(List,Call).

no_repeats_av(Call):-term_variables(Call,Vs),!,no_repeats_av_l(Vs,Call).

no_repeats_av_l([],Call):-!,Call,!.
no_repeats_av_l([AVar],Call):-!,
   no_repeats_av(AVar,Call).
no_repeats_av_l([AVar|List],Call):-
   no_repeats_av(AVar,no_repeats_av_l(List,Call)).



% =========================================================================
:- meta_predicate succeeds_n_times(0, -).
% =========================================================================

succeeds_n_times(Goal, Times) :-
        Counter = counter(0),
        (   Goal,
            arg(1, Counter, N0),
            N is N0 + 1,
            nb_setarg(1, Counter, N),
            fail
        ;   arg(1, Counter, Times)
        ).

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

:- export(tlbugger:ifWontTrace/0).
:-thread_local(tlbugger:ifWontTrace/0).
:-moo_hide(tlbugger:ifWontTrace/0).


:- export(tlbugger:ifHideTrace/0).
:-thread_local(tlbugger:ifHideTrace/0).
:-moo_hide(tlbugger:ifHideTrace/0).

%:-meta_predicate(set_no_debug).
:- export(set_no_debug/0).

set_no_debug:- 
  must_det_l((
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

:- export(set_no_debug_thread/0).
set_no_debug_thread:- 
  must_det_l((
   retractall(tlbugger:ifCanTrace),
   retractall(tlbugger:ifWontTrace),
   asserta(tlbugger:ifWontTrace))),!.

set_gui_debug(TF):-ignore((catchvv(guitracer,_,true))),
   ((TF,has_gui_debug)-> set_prolog_flag(gui_tracer, true) ; set_prolog_flag(gui_tracer, false)).


:-meta_predicate(set_yes_debug()).
:- export(set_yes_debug/0).
set_yes_debug:- 
  must_det_l([
   set_prolog_flag(generate_debug_info, true),
   (tlbugger:ifCanTrace->true;assert(tlbugger:ifCanTrace)),
   retractall(tlbugger:ifWontTrace),   
   (tlbugger:ifWontTrace->true;assert(tlbugger:ifWontTrace)),   
   set_prolog_flag(report_error,true),   
   set_prolog_flag(debug_on_error,true),
   set_prolog_flag(debug, true),   
   set_prolog_flag(query_debug_settings, debug(true, true)),
   % set_gui_debug(true),
   thread_leash(+all),
   thread_leash(+exception),
   visible(+cut_call),
   notrace, debug]),!.


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
:- meta_predicate with_output_to_stream(?,0).

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

do_gc:- current_prolog_flag(gc,true),!,do_gc0.
do_gc:- set_prolog_flag(gc,true), do_gc0, set_prolog_flag(gc,false).
do_gc0:- cnotrace((garbage_collect, garbage_collect_atoms, garbage_collect_clauses /*, statistics*/
                    )).


failOnError(Call):-catchvv(Call,_,fail).

fresh_line:-current_output(Strm),fresh_line(Strm),!.
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

:- export(rtraceOnError/1).
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



dynamic_if_missing(F/A):-functor_safe(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):-dynamic([F/A]).

get_pi(PI,PI):-var(PI),!.
get_pi(F/A,PI):-!,functor(PI,F,A).
get_pi(PI,PI):- atomic(PI),!.
get_pi(PI,PI):- compound(PI),!.
get_pi(Mask,PI):-get_functor(Mask,F,A),functor(PI,F,A),!.

%%%retractall(E):- retractall(E),functor_safe(E,File,A),dynamic(File/A),!.

pp_listing(Pred):- functor_safe(Pred,File,A),functor_safe(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

:- meta_predicate(with_assertions(?,0)).
with_assertions( [],Call):- !,Call.
with_assertions( [With|MORE],Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With,MORE),Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With;MORE),Call):- !,with_assertions(With,Call);with_assertions(MORE,Call).
with_assertions( -TL:With,Call):- !,with_no_assertions(TL:With,Call).
with_assertions( +TL:With,Call):- !,with_assertions(TL:With,Call).
with_assertions( not(With),Call):- !,with_no_assertions(With,Call).
with_assertions( -With,Call):- !,with_no_assertions(With,Call).
with_assertions( +With,Call):- !,with_assertions(With,Call).

with_assertions(op(N,XFY,OP),MCall):-!,
     (current_op(PN,XFY,OP);PN=0),!,
     strip_module(MCall,M,Call),
     (PN==N -> Call ; setup_call_cleanup(op(N,XFY,OP),'@'(Call,M),op(PN,XFY,OP))).

with_assertions(set_prolog_flag(N,XFY),MCall):- !,
     (current_prolog_flag(N,WAS);WAS=unUSED),
     strip_module(MCall,M,Call),!,
     (XFY==WAS -> Call ; 
     (setup_call_cleanup(set_prolog_flag(N,XFY),'@'(Call,M),(WAS=unUSED->true;set_prolog_flag(N,WAS))))).

with_assertions(before_after(Before,After),Call):-
     (Before -> setup_call_cleanup(Call,After);Call).

with_assertions(THead,Call):- 
 cnotrace(( 
     to_thread_head(THead,M,_Head,HAssert) -> true ; throw(failed(to_thread_head(THead,M,_,HAssert))))),
     setup_call_cleanup(hotrace(asserta(M:HAssert,REF)),Call,erase_safe(asserta(M:HAssert,REF),REF)).

:-meta_predicate(with_no_assertions(+,0)).
with_no_assertions(UHead,Call):- 
  hotrace((THead = (UHead:- (!,fail)),
   (to_thread_head(THead,M,Head,HAssert) -> true; throw(to_thread_head(THead,M,Head,HAssert))))),
       setup_call_cleanup(hotrace(M:asserta(HAssert,REF)),Call,erase_safe(asserta(M:HAssert,REF),REF)).

/*
old version
:-meta_predicate(with_no_assertions(+,0)).
with_no_assertions(THead,Call):-
 must_det(to_thread_head((THead:- (!,fail)),M,Head,H)),
   copy_term(H,  WithA), !, setup_call_cleanup(M:asserta(WithA,REF),Call,must_det(M:retract(Head))).
*/

to_thread_head((H:-B),TL,HO,(HH:-B)):-!,to_thread_head(H,TL,HO,HH),!.
to_thread_head(thglobal:Head,thglobal,thglobal:Head,Head):- !.
to_thread_head(TL:Head,TL,TL:Head,Head):-!, check_thread_local(TL:Head).
% to_thread_head(Head,Module,Module:Head,Head):-Head \= (_:_), predicate_module(Head,Module),!.
to_thread_head(user:Head,user,user:Head,Head):- !.
to_thread_head(Head,thlocal,thlocal:Head,Head):-!,check_thread_local(thlocal:Head).
to_thread_head(Head,tlbugger,tlbugger:Head,Head):-check_thread_local(tlbugger:Head).

check_thread_local(thlocal:_):-!.
check_thread_local(tlbugger:_):-!.
check_thread_local(_):-!.
check_thread_local(user:_):-!.
check_thread_local(TL:Head):-slow_sanity(( predicate_property(TL:Head,(dynamic)),must_det(predicate_property(TL:Head,(thread_local))))).

:-thread_local( tlbugger: tlbugger:dmsg_match/2).
:-meta_predicate(with_all_dmsg(0)).
:-meta_predicate(with_show_dmsg(*,0)).

with_all_dmsg(Call):- always_show_dmsg,!,Call.
with_all_dmsg(Call):-
     with_assertions(set_prolog_flag(opt_debug,true),
       with_assertions( tlbugger:dmsg_match(show,_),Call)).
with_all_dmsg(Call):- always_show_dmsg,!,Call.
with_show_dmsg(TypeShown,Call):-
  with_assertions(set_prolog_flag(opt_debug,filter),
     with_assertions( tlbugger:dmsg_match(showing,TypeShown),Call)).

:-meta_predicate(with_no_dmsg(0)).
with_no_dmsg(Call):-with_assertions(set_prolog_flag(opt_debug,false),Call).
with_no_dmsg(TypeUnShown,Call):-with_assertions(set_prolog_flag(opt_debug,filter),
  with_assertions( tlbugger:dmsg_match(hidden,TypeUnShown),Call)).

% dmsg_hides_message(_):- !,fail.
dmsg_hides_message(_):- current_prolog_flag(opt_debug,false),!.
dmsg_hides_message(_):- current_prolog_flag(opt_debug,true),!,fail.
dmsg_hides_message(C):-  tlbugger:dmsg_match(HideShow,Matcher),matches_term(Matcher,C),!,HideShow=hidden.

dmsg_hide(isValueMissing):-!,set_prolog_flag(opt_debug,false).
dmsg_hide(Term):-set_prolog_flag(opt_debug,filter),must(nonvar(Term)),asserta_new( tlbugger:dmsg_match(hidden,Term)),retractall( tlbugger:dmsg_match(showing,Term)),nodebug(Term).
dmsg_show(isValueMissing):-!,set_prolog_flag(opt_debug,true).
dmsg_show(Term):-set_prolog_flag(opt_debug,filter),asserta_new( tlbugger:dmsg_match(showing,Term)),ignore(retractall( tlbugger:dmsg_match(hidden,Term))),debug(Term).
dmsg_showall(Term):-ignore(retractall( tlbugger:dmsg_match(hidden,Term))).

% =================================================================================
% Utils
% =================================================================================

printPredCount(Msg,Pred,N1):- compound(Pred), debugOnFailureEach((arg(_,Pred,NG))),user:nonvar(NG),!,
  findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),dmsg(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor_safe(Pred,File,A),functor_safe(FA,File,A), predicate_property(FA,number_of_clauses(N1)),dmsg(num_clauses(Msg,File/A,N1)),!.



% =================================================================================
% Loader Utils
% =================================================================================


dynamic_load_pl(PLNAME):-consult(PLNAME),!.

dynamic_load_pl(PLNAME):- % unload_file(PLNAME),
   open(PLNAME, read, In, []),
   repeat,
   line_count(In,Lineno),
   % double_quotes(_DQBool)
   Options = [variables(_Vars),variable_names(_VarNames),singletons(_Singletons),comment(_Comment)],
   catchvv((read_term(In,Term,[syntax_errors(error)|Options])),E,(dmsg(E),fail)),
   load_term(Term,[line_count(Lineno),file(PLNAME),stream(In)|Options]),
   Term==end_of_file,
   close(In).

load_term(E,_Options):- E == end_of_file, !.
load_term(Term,Options):-catchvv(load_term2(Term,Options),E,(dmsg(error(load_term(Term,Options,E))),throw_safe(E))).

load_term2(':-'(Term),Options):-!,load_dirrective(Term,Options),!.
load_term2(:-(H,B),Options):-!,load_assert(H,B,Options).
load_term2(Fact,Options):-!,load_assert(Fact,true,Options).

load_assert(H,B,_Options):-assert((H:-B)),!.

load_dirrective(include(PLNAME),_Options):- (atom_concat_safe(Key,'.pl',PLNAME) ; Key=PLNAME),!, dynamic_load_pl(Key).
load_dirrective(CALL,_Options):- CALL=..[module,M,_Preds],!,module(M),call(CALL).
load_dirrective(Term,_Options):-!,Term.

showProfilerStatistics(FileMatch):-
  statistics(global,Mem), MU is (Mem / 1024 / 1024),
  printPredCount('showProfilerStatistics: '(MU),FileMatch,_N1).



% ===============================================================================================
% UTILS
% ===============================================================================================


:- meta_predicate user:unify_listing(0).
unify_listing(FileMatch):-functor_safe(FileMatch,F,A),unify_listing(FileMatch,F,A),!.
unify_listing_header(FileMatch):-functor_safe(FileMatch,F,A),unify_listing_header(FileMatch,F,A),!.


:- meta_predicate user:unify_listing(0,*,*).
unify_listing_header(FileMatch,F,A):- (fmt('~n/* Prediate: ~q / ~q ~n',[F,A,FileMatch])),fail.
unify_listing_header(FileMatch,_F,_A):- printAll(predicate_property(FileMatch,PP),PP),fail.
unify_listing_header(FileMatch,_F,_A):- (fmt('~n ~q. ~n */ ~n',[FileMatch])),fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,dynamic),(fmt(':-dynamic(~q).~n',[F/A])),fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,multifile),(fmt(':-multifile(~q).~n',[F/A])),fail.
unify_listing_header(_FileMatch,_F,_A).

unify_listing(FileMatch,F,A):- unify_listing_header(FileMatch,F,A), printAll(FileMatch).

printAll(FileMatch):-printAll(FileMatch,FileMatch).
printAll(Call,Print):- flag(printAll,_,0), forall((Call,flag(printAll,N,N+1)),(fmt('~q.~n',[Print]))),fail.
printAll(_Call,Print):- flag(printAll,PA,0),(fmt('~n /* found ~q for ~q. ~n */ ~n',[PA,Print])).


/*
contains_term_unifiable(SearchThis,Find):-Find=SearchThis,!.
contains_term_unifiable(SearchThis,Find):-compound(SearchThis),functor_safe(SearchThis,Func,_),(Func==Find;arg(_,SearchThis,Arg),contains_term_unifiable(Arg,Find)).
*/
% =================================================================================
% Utils
% =================================================================================

global_pathname(B,A):-absolute_file_name(B,A),!.
global_pathname(B,A):-relative_pathname(B,A).

relative_pathname(Path,Relative):-absolute_file_name(Path,[relative_to('./')],Absolute),member(Rel,['./','../','../../']),absolute_file_name(Rel,Clip),
  canonical_pathname(Absolute,AbsoluteA),
  canonical_pathname(Clip,ClipA),
  atom_concat_safe(ClipA,RelativeA,AbsoluteA),!,atom_concat_safe(Rel,RelativeA,Relative),!.
relative_pathname(Path,Relative):-canonical_pathname(Path,Relative),!.

canonical_pathname(Absolute,AbsoluteB):-prolog_to_os_filename(AbsoluteA,Absolute),expand_file_name(AbsoluteA,[AbsoluteB]),!.

alldiscontiguous:-!.


% ==========================================================
% Sending Notes
% ==========================================================

writeModePush(_Push):-!.
writeModePop(_Pop):-!.


if_prolog(swi,G):-call(G). % Run B-Prolog Specifics
if_prolog(_,_):-!. % Dont run SWI Specificd or others

indent_e(0):-!.
indent_e(X):- X > 20, XX is X-20,!,indent_e(XX).
indent_e(X):- catchvv((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).

% ===================================================================
% Lowlevel printng
% ===================================================================


fmt0(X,Y,Z):-catchvv((format(X,Y,Z),flush_output_safe(X)),E,dmsg(E)).
fmt0(X,Y):-catchvv((format(X,Y),flush_output_safe),E,dmsg(E)).
fmt0(X):-catchvv(text_to_string(X,S),_,fail),'format'('~w',[S]),!.
fmt0(X):- (atom(X) -> catchvv((format(X,[]),flush_output_safe),E,dmsg(E)) ; (term_to_message_string(X,M) -> 'format'('~q~n',[M]);fmt_or_pp(X))).
fmt(X):-fresh_line,fmt_ansi(fmt0(X)).
fmt(X,Y):- fresh_line,fmt_ansi(fmt0(X,Y)),!.
fmt(X,Y,Z):- fmt_ansi(fmt0(X,Y,Z)),!.

% :-reexport(library(ansi_term)).

tst_fmt:- make,
 findall(R,(clause(ansi_term:sgr_code(R, _),_),ground(R)),List),
 ignore((
        ansi_term:ansi_color(FC, _),
        member(FG,[hfg(FC),fg(FC)]),
        % ansi_term:ansi_term:ansi_color(Key, _),
        member(BG,[hbg(default),bg(default)]),
        member(R,List),
        % random_member(R1,List),
    C=[reset,R,FG,BG],
  fresh_line,
  ansi_term:ansi_format(C,' ~q ~n',[C]),fail)).


fmt_ansi(Call):-ansicall([reset,bold,hfg(white),bg(black)],Call).

fmt_portray_clause(X):- unnumbervars(X,Y),!,snumbervars(Y), portray_clause(Y).
fmt_or_pp(portray((X:-Y))):-!,fmt_portray_clause((X:-Y)),!.
fmt_or_pp(portray(X)):-!,functor_safe(X,F,A),fmt_portray_clause((pp(F,A):-X)),!.
fmt_or_pp(X):-format('~q~n',[X]).

dfmt(X):- with_output_to_stream(user_error,fmt(X)).
dfmt(X,Y):- with_output_to_stream(user_error,fmt(X,Y)).

with_output_to_stream(Stream,Goal):-
    current_output(Saved),
   setup_call_cleanup(set_output(Stream),
         Goal,
         set_output(Saved)).




:-dynamic dmsg_log/3.

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

:- thread_local is_with_dmsg/1.

with_dmsg(Functor,Goal):-
   with_assertions(is_with_dmsg(Functor),Goal).

:-dynamic hook:dmsg_hook/1.
:-multifile hook:dmsg_hook/1.

contains_atom(V,A):-sub_term(VV,V),nonvar(VV),functor_safe(VV,A,_).

:- export(matches_term/2).
matches_term(Filter,_):- var(Filter),!.
matches_term(Filter,Term):- var(Term),!,Filter=var.
matches_term(Filter,Term):- ( \+ \+ (matches_term0(Filter,Term))),!.
matches_term0(Filter,Term):- Term = Filter.
matches_term0(Filter,Term):- atomic(Filter),!,contains_atom(Term,Filter).
matches_term0(F/A,Term):- (var(A)->member(A,[0,1,2,3,4]);true), functor_safe(Filter,F,A), matches_term0(Filter,Term).
matches_term0(Filter,Term):- sub_term(STerm,Term),nonvar(STerm),matches_term0(Filter,STerm),!.

show_source_location :- once((ignore((prolog_load_context(file,F),prolog_load_context(stream,S),line_count(S,L),format('~N% ~w~n',[F:L]))))).

:-thread_local(tlbugger:skipDmsg/0).

dmsginfo(V):-dmsg(info(V)).
dmsg(V):- cnotrace((dmsg0(V))).
:-moo_hide(dmsg/1).
%:-moo_hide(system:notrace/1). 

:-export(dmsg0/1).
:-export(dmsg1/1).
dmsg0(V):- is_with_dmsg(FP),!,FP=..FPL,append(FPL,[V],VVL),VV=..VVL,once(dmsg0(VV)).
dmsg0(_):- \+ always_show_dmsg, is_hiding_dmsgs,!.

dmsg0(V):- var(V),!,dmsg0(dmsg_var(V)).
dmsg0(NC):- cyclic_term(NC),!,fresh_line,write(dmsg_cyclic_term(NC)),fresh_line.
dmsg0(NC):- tlbugger:skipDmsg,!,fresh_line,write(dmsg_cyclic_term(NC)),fresh_line.

dmsg0(V):- once(dmsg1(V)), ignore((hook:dmsg_hook(V),fail)).
%dmsg1(trace_or_throw(V)):- dumpST(350),dmsg(warning,V),fail.
%dmsg1(error(V)):- dumpST(250),dmsg(warning,V),fail.
%dmsg1(warn(V)):- dumpST(150),dmsg(warning,V),fail.
dmsg1(NC):- cyclic_term(NC),!,fresh_line,write(dmsg_cyclic_term(NC)),fresh_line.
dmsg1(skip_dmsg(_)):-!.
%dmsg1(C):- \+ always_show_dmsg, dmsg_hides_message(C),!.
dmsg1(ansi(Ctrl,Msg)):- ansicall(Ctrl,dmsg1(Msg)).
dmsg1(C):-functor_safe(C,Topic,_),debugging(Topic,_True_or_False),logger_property(Topic,once,true),!,
      (dmsg_log(Topic,_Time,C) -> true ; ((get_time(Time),asserta(dmsg_log(todo,Time,C)),!,dmsg2(C)))).


dmsg1(_):- show_source_location,fail.
dmsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall(Ctrl, dmsg2(Msg)).

dmsg2(C):-not(ground(C)),copy_term(C,Stuff), snumbervars(Stuff),!,dmsg3(Stuff).
dmsg2(Msg):-dmsg3(Msg).

into_comments(S,AC,O):-atomics_to_string(Lines,'\n',S),!,atom_concat('\n',AC,Sep),atomics_to_string(Lines,Sep,O).

dmsg3(Msg):- 
         sformat(Str,Msg,[],[]),
         string_to_atom(AStr,Str),concat_atom(Lines,'\n',AStr),
         mesg_color(Msg,Ctrl),!,
         with_output_to_stream(user_error,  
                     forall(member(E,Lines), dmsg5(Ctrl,E))).

get_indent_level(Depth-25) :- if_prolog(swi,((prolog_current_frame(Frame),prolog_frame_attribute(Frame,level,Depth)))),!.
get_indent_level(2):-!.

if_color_debug:-current_prolog_flag(dmsg_color,true).
if_color_debug(Call):- if_color_debug(Call, true).
if_color_debug(Call,UnColor):- if_color_debug->Call;UnColor.

dmsg5(_,color(Ctrl,E)):-!,dmsg5(Ctrl,E).
dmsg5(Ctrl,E):- get_indent_level(Indent), 
 (fresh_line,if_color_debug(write('\e[0m')),write('%'),indent_e(Indent),!,
  ansicall(Ctrl,
   if_color_debug(format('~w\e[0m',[E]),format('~w',[E]))),fresh_line),!.

/*dmsg5([]):-!.
dmsg5(LF):-functor_safe(LF,F,_),loggerReFmt(F,LR), ((LR==F,is_stream(F))->loggerFmtReal(F,LF,[]);dmsg(LR,LF,[])),!.
dmsg5([A|L]):-!,dmsg('% ~q~n',[[A|L]]).
dmsg5(Comp):-bugger:evil_term(_,Comp,Comp2),!,dmsg('% ~q~n',[Comp2]).
dmsg5(Stuff):-!,dmsg('% ~q~n',[Stuff]).
*/
dmsg(_,F):-F==[-1];F==[[-1]].
dmsg(F,A):-is_list(A),!,nl(user_error),fmt0(user_error,F,A),nl(user_error),flush_output_safe(user_error),!.
dmsg(_,_):- is_hiding_dmsgs,!.
dmsg(C,T):-!, ansicall(C,dmsg(msg(C,T))).
dmsg(C,T):-!, (( fmt('<font size=+1 color=~w>',[C]), fmt(T), fmt('</font>',[]))),!.

dmsg(L,F,A):-loggerReFmt(L,LR),loggerFmtReal(LR,F,A).

vdmsg(L,F):-loggerReFmt(L,LR),loggerFmtReal(LR,F,[]).


/*
ansifmt(+Attributes, +Format, +Args) is det
Format text with ANSI attributes. This predicate behaves as format/2 using Format and Args, but if the current_output is a terminal, it adds ANSI escape sequences according to Attributes. For example, to print a text in bold cyan, do
?- ansifmt([bold,fg(cyan)], 'Hello ~w', [world]).
Attributes is either a single attribute or a list thereof. The attribute names are derived from the ANSI specification. See the source for sgr_code/2 for details. Some commonly used attributes are:

bold
underline
fg(Color), bg(Color), hfg(Color), hbg(Color)
Defined color constants are below. default can be used to access the default color of the terminal.

black, red, green, yellow, blue, magenta, cyan, white
ANSI sequences are sent if and only if

The current_output has the property tty(true) (see stream_property/2).
The Prolog flag color_term is true.

ansifmt(Ctrl, Format, Args) :- ansifmt(current_output, Ctrl, Format, Args).

ansifmt(Stream, Ctrl, Format, Args) :-
     % we can "assume"
        % ignore(((stream_property(Stream, tty(true)),current_prolog_flag(color_term, true)))), !,
	(   is_list(Ctrl)
	->  maplist(ansi_term:sgr_code_ex, Ctrl, Codes),
	    atomic_list_concat(Codes, (';'), OnCode)
	;   ansi_term:sgr_code_ex(Ctrl, OnCode)
	),
	'format'(string(Fmt), '\e[~~wm~w\e[0m', [Format]),
        retractall(last_used_color(Ctrl)),asserta(last_used_color(Ctrl)),
	'format'(Stream, Fmt, [OnCode|Args]),
	flush_output,!.
ansifmt(Stream, _Attr, Format, Args) :- 'format'(Stream, Format, Args).

*/

:-use_module(library(ansi_term)).

:- export(ansifmt/2).
ansifmt(Ctrl,Fmt):- colormsg(Ctrl,Fmt).
:- export(ansifmt/3).
ansifmt(Ctrl,F,A):- colormsg(Ctrl,(format(F,A))).



:- export(colormsg/2).



colormsg(d,Msg):- mesg_color(Msg,Ctrl),!,colormsg(Ctrl,Msg).
colormsg(Ctrl,Msg):- fresh_line,ansicall(Ctrl,fmt0(Msg)),fresh_line.

:- export(ansicall/2).
ansicall(Ctrl,Call):- hotrace((current_output(Out), ansicall(Out,Ctrl,Call))).

ansi_control_conv([],[]):-!.
ansi_control_conv([H|T],HT):-!,ansi_control_conv(H,HH),!,ansi_control_conv(T,TT),!,flatten([HH,TT],HT),!.
ansi_control_conv(warn,Ctrl):- !, ansi_control_conv(warning,Ctrl),!.
ansi_control_conv(Level,Ctrl):- ansi_term:level_attrs(Level,Ansi),Level\=Ansi,!,ansi_control_conv(Ansi,Ctrl).
ansi_control_conv(Color,Ctrl):- ansi_term:ansi_color(Color,_),!,ansi_control_conv(fg(Color),Ctrl).
ansi_control_conv(Ctrl,CtrlO):-flatten([Ctrl],CtrlO),!.

is_tty(Out):- not(tlbugger:no_colors), is_stream(Out),stream_property(Out,tty(true)).

ansicall(Out,_,Call):- \+ is_tty(Out),!,Call.
ansicall(Out,_,Call):- tlbugger:skipDmsg,!,Call.

ansicall(Out,CtrlIn,Call):- once(ansi_control_conv(CtrlIn,Ctrl)),  CtrlIn\=Ctrl,!,ansicall(Out,Ctrl,Call).
ansicall(_,_,Call):- in_pengines,!,Call.
ansicall(Out,Ctrl,Call):-
   retractall(last_used_color(_)),asserta(last_used_color(Ctrl)),ansicall0(Out,Ctrl,Call),!.

ansicall0(Out,[Ctrl|Set],Call):-!, ansicall0(Out,Ctrl,ansicall0(Out,Set,Call)).
ansicall0(_,[],Call):-!,Call.
ansicall0(Out,Ctrl,Call):-if_color_debug(ansicall1(Out,Ctrl,Call),keep_line_pos(Out, Call)).

ansicall1(Out,Ctrl,Call):-
   must(sgr_code_on_off(Ctrl, OnCode, OffCode)),!,
     keep_line_pos(Out, (format(Out, '\e[~wm', [OnCode]))),
	call_cleanup(Call,
           keep_line_pos(Out, (format(Out, '\e[~wm', [OffCode])))).
/*
ansicall(S,Set,Call):-
     call_cleanup((
         stream_property(S, tty(true)), current_prolog_flag(color_term, true), !,
	(is_list(Ctrl) ->  maplist(sgr_code_on_off, Ctrl, Codes, OffCodes), 
          atomic_list_concat(Codes, (';'), OnCode) atomic_list_concat(OffCodes, (';'), OffCode) ;   sgr_code_on_off(Ctrl, OnCode, OffCode)),
        keep_line_pos(S, (format(S,'\e[~wm', [OnCode])))),
	call_cleanup(Call,keep_line_pos(S, (format(S, '\e[~wm', [OffCode]))))).


*/




keep_line_pos(S, G) :-
       (stream_property(S, position(Pos)) -> 
	(stream_position_data(line_position, Pos, LPos),
        call_cleanup(G, set_stream(S, line_position(LPos)))) ; G).


:-dynamic(term_color0/2).


term_color0(retract,magenta).
term_color0(retractall,magenta).
term_color0(assertz,hfg(green)).
term_color0(assertz_new,hfg(green)).
term_color0(asserta_new,hfg(green)).
term_color0(db_op,hfg(blue)).


mesg_color(T,C):-cyclic_term(T),!,C=reset.
mesg_color(T,C):-var(T),!,dumpST,!,C=[blink(slow),fg(red),hbg(black)],!.
mesg_color(T,C):- string(T),!,must(f_word(T,F)),!,functor_color(F,C).
mesg_color(T,C):-not(compound(T)),catchvv(text_to_string(T,S),_,fail),!,mesg_color(S,C).
mesg_color(T,C):-not(compound(T)),term_to_atom(T,A),!,mesg_color(A,C).
mesg_color(succeed(T),C):-nonvar(T),mesg_color(T,C).
mesg_color(=(T,_),C):-nonvar(T),mesg_color(T,C).
mesg_color(debug(T),C):-nonvar(T),mesg_color(T,C).
mesg_color(_:T,C):-nonvar(T),!,mesg_color(T,C).
mesg_color(T,C):-functor_safe(T,F,_),member(F,[color,ansi]),compound(T),arg(1,T,C),nonvar(C).
mesg_color(T,C):-predef_functor_color(F,C),mesg_arg1(T,F).
mesg_color(T,C):-nonvar(T),defined_message_color(F,C),matches_term(F,T),!.
mesg_color(T,C):-functor_h0(T,F,_),!,functor_color(F,C),!.

f_word(T,A):-concat_atom([A|_],' ',T),!.
f_word(T,A):-concat_atom([A|_],'_',T),!.
f_word(T,A):- string_to_atom(T,A),!.

mesg_arg1(T,_TT):-var(T),!,fail.
mesg_arg1(_:T,C):-nonvar(T),!,mesg_arg1(T,C).
mesg_arg1(T,TT):-not(compound(T)),!,T=TT.
mesg_arg1(T,F):-functor_h0(T,F,_).
mesg_arg1(T,C):-compound(T),arg(1,T,F),!,nonvar(F),mesg_arg1(F,C).


:- export(defined_message_color/2).
:-dynamic(defined_message_color/2).

defined_message_color(todo,[fg(red),bg(black),underline]).
%defined_message_color(error,[fg(red),hbg(black),bold]).
defined_message_color(warn,[fg(black),hbg(red),bold]).
defined_message_color(A,B):-term_color0(A,B).


predef_functor_color(F,C):- defined_message_color(F,C),!.
predef_functor_color(F,C):- defined_message_color(F/_,C),!.
predef_functor_color(F,C):- term_color0(F,C),!.

functor_color(F,C):- predef_functor_color(F,C),!.
functor_color(F,C):- next_color(C),assertz(term_color0(F,C)),!.


:-dynamic(last_used_color/1).

last_used_color(pink).

last_used_fg_color(LFG):-last_used_color(LU),fg_color(LU,LFG),!.
last_used_fg_color(default).

good_next_color(C):-var(C),!,trace_or_throw(var_good_next_color(C)),!.
good_next_color(C):- last_used_fg_color(LFG),fg_color(C,FG),FG\=LFG,!.
good_next_color(C):- not(unliked_ctrl(C)).

unliked_ctrl(fg(blue)).
unliked_ctrl(fg(black)).
unliked_ctrl(fg(red)).
unliked_ctrl(bg(white)).
unliked_ctrl(hbg(white)).
unliked_ctrl(X):-is_list(X),member(E,X),nonvar(E),unliked_ctrl(E).

fg_color(LU,FG):-member(fg(FG),LU),FG\=white,!.
fg_color(LU,FG):-member(hfg(FG),LU),FG\=white,!.
fg_color(_,default).

:- export(random_color/1).
random_color([reset,M,FG,BG,font(Font)]):-Font is random(8),
  findall(Cr,ansi_term:ansi_color(Cr, _),L),
  random_member(E,L),
  random_member(FG,[hfg(E),fg(E)]),not(unliked_ctrl(FG)),
  contrasting_color(FG,BG), not(unliked_ctrl(BG)),
  random_member(M,[bold,faint,reset,bold,faint,reset,bold,faint,reset]),!. % underline,negative


:- export(tst_color/0).
tst_color:- make, ignore((( between(1,20,_),random_member(Call,[colormsg(C,cm(C)),dmsg(color(C,dm(C))),ansifmt(C,C)]),next_color(C),Call,fail))).
:- export(tst_color/1).
tst_color(C):- make,colormsg(C,C).

:- export(next_color/1).
next_color(C):- between(1,10,_), random_color(C), good_next_color(C),!.
next_color([underline|C]):- random_color(C),!.

:- export(contrasting_color/2).
contrasting_color(white,black).
contrasting_color(A,default):-atom(A),A \= black.
contrasting_color(fg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(hfg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(black,white).
contrasting_color(default,default).
contrasting_color(_,default).

:-thread_local(ansi_prop/2).

sgr_on_code(Ctrl,OnCode):- ansi_term:sgr_code(Ctrl,OnCode),!.
sgr_on_code(blink, 6).
sgr_on_code(-Ctrl,OffCode):-  nonvar(Ctrl), sgr_off_code(Ctrl,OffCode),!.
sgr_on_code(Foo,7):- format(user_error,'~nMISSING: n~q~n',[sgr_on_code(Foo,7)]),!,dtrace(sgr_on_code(Foo,7)).

sgr_off_code(Ctrl,OnCode):-ansi_term:off_code(Ctrl,OnCode),!.
sgr_off_code(- Ctrl,OnCode):- nonvar(Ctrl), sgr_on_code(Ctrl,OnCode),!.
sgr_off_code(fg(_), CurFG):- (ansi_prop(fg,CurFG)->true;CurFG=39),!.
sgr_off_code(bg(_), CurBG):- (ansi_prop(ng,CurBG)->true;CurBG=49),!.
sgr_off_code(bold, 21).
sgr_off_code(italic_and_franktur, 23).
sgr_off_code(franktur, 23).
sgr_off_code(italic, 23).
sgr_off_code(underline, 24).
sgr_off_code(blink, 25).
sgr_off_code(blink(_), 25).
sgr_off_code(negative, 27).
sgr_off_code(conceal, 28).
sgr_off_code(crossed_out, 29).
sgr_off_code(framed, 54).
sgr_off_code(overlined, 55).
sgr_off_code(_,0).


sgr_code_on_off(Ctrl,OnCode,OffCode):-sgr_on_code(Ctrl,OnCode),sgr_off_code(Ctrl,OffCode),!.
sgr_code_on_off(Ctrl,OnCode,OffCode):-sgr_on_code(Ctrl,OnCode),sgr_off_code(Ctrl,OffCode),!.


% ansicall(Ctrl,Msg):- msg_to_string(Msg,S),fresh_line,catchvv(ansifmt(Ctrl,'~w~n',[S]),_,catchvv(ansifmt(fg(Ctrl),'~w',[S]),_,'format'('~q (~w)~n',[ansicall(Ctrl,Msg),S]))),fresh_line.
/*
dmsg(Color,Term):- current_prolog_flag(tty_control, true),!,  tell(user),fresh_line,to_petty_color(Color,Type),
   call_cleanup(((sformat(Str,Term,[],[]),dmsg(Type,if_tty([Str-[]])))),told).
*/

msg_to_string(Var,Str):-var(Var),!,sformat(Str,'~q',[Var]),!.
msg_to_string(portray(Msg),Str):- with_output_to(string(Str),portray_clause_w_vars(user_output,Msg,[],[])),!.
msg_to_string(pp(Msg),Str):- sformat(Str,Msg,[],[]),!.
msg_to_string(fmt(F,A),Str):-sformat(Str,F,A),!.
msg_to_string(format(F,A),Str):-sformat(Str,F,A),!.
msg_to_string(Msg,Str):-atomic(Msg),!,sformat(Str,'~w',[Msg]).
msg_to_string(m2s(Msg),Str):-message_to_string(Msg,Str),!.
msg_to_string(Msg,Str):-sformat(Str,Msg,[],[]),!.

:-use_module(library(listing)).
sformat(Str,Msg,Vs,Opts):- nonvar(Msg),functor_safe(Msg,':-',_),!,with_output_to(string(Str),portray_clause_w_vars(user_output,Msg,Vs,Opts)).
sformat(Str,Msg,Vs,Opts):- with_output_to(chars(Codes),(current_output(CO),portray_clause_w_vars(CO,':-'(Msg),Vs,Opts))),append([_,_,_],PrintCodes,Codes),'sformat'(Str,'   ~s',[PrintCodes]),!.
portray_clause_w_vars(Out,Msg,Vs,Options):- \+ \+ ((prolog_listing:do_portray_clause(Out,Msg,[variable_names(Vs),numbervars(true),character_escapes(true),quoted(true)|Options]))),!.

:-meta_predicate(gripe_time(+,0)).
:-export(gripe_time/2).
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
:- export(logOnFailure/1).
logOnFailure(C):-one_must(C,(dmsg(failed_show_call(C)),garbage_collect_atoms,!,fail)).


:-dynamic(user:logLevel/2).
:-module_transparent(user:logLevel/2).
:-multifile(user:logLevel/2).

setLogLevel(M,L):-retractall(user:logLevel(M,_)),(user:nonvar(L)->asserta(user:logLevel(M,L));true).

user:logLevel(debug,user_error).
user:logLevel(error,user_error).
user:logLevel(private,none).
user:logLevel(S,Z):-current_stream(_X,write,Z),trace,stream_property(Z,alias(S)).

loggerReFmt(L,LRR):-user:logLevel(L,LR),L \==LR,!,loggerReFmt(LR,LRR),!.
loggerReFmt(L,L).

loggerFmtReal(none,_F,_A):-!.
loggerFmtReal(S,F,A):-
  current_stream(_,write,S),
    fmt(S,F,A),
    flush_output_safe(S),!.


dumpSTC0:- MaxDepth=200,integer(MaxDepth),Term = dumpST(MaxDepth),
   (var(Frame)->prolog_current_frame(Frame);true),
   ignore(( get_prolog_backtrace(MaxDepth, Trace,[frame(Frame),goal_depth(5)]),
    format(user_error, '% dumpST ~p', [Term]), nl(user_error),
    print_prolog_backtrace(user_error, Trace,[subgoal_positions(true)]), nl(user_error), fail)),!.


 
dump_st:- must_det_l((prolog_current_frame(Frame),get_prolog_backtrace(1000, Trace,[frame(Frame),goal_depth(10-0)]),
   dmsg(Trace),
   print_prolog_backtrace(user_error, Trace,[subgoal_positions(true)]))).

:-moo_hide_childs(stack_depth/1).
:-moo_hide_childs(stack_check/0).
:-moo_hide_childs(stack_check/1).
:-moo_hide_childs(stack_check/2).
stack_depth(Level):-hotrace((prolog_current_frame(Frame),prolog_frame_attribute(Frame,level,Level))).

stack_check:-!.
stack_check:-stack_check(3000).
stack_check(BreakIfOver):- stack_check_else(BreakIfOver, trace_or_throw(stack_check(BreakIfOver))).
stack_check(BreakIfOver,Error):- stack_check_else(BreakIfOver, trace_or_throw(stack_check(BreakIfOver,Error))).
stack_check_else(BreakIfOver,Call):- stack_depth(Level) ,  ( Level < BreakIfOver -> true ; (dbgsubst(Call,stack_lvl,Level,NewCall),NewCall)).


% dumpstack_arguments.
dumpST:- tlbugger:skipDmsg-> dumpSTC ; cnotrace(dumpST0).
dumpSTC:-
   with_assertions(tlbugger:skipDmsg,dumpST0).

dumpST0:- tlbugger:ifHideTrace,!.
dumpST0:- hotrace((prolog_current_frame(Frame),dumpST2(Frame,5000))).
% dumpST(_):-is_hiding_dmsgs,!.

dumpST(_):- tlbugger:ifHideTrace,!.
dumpST(Opts):- dumpST(_,Opts).

dumpST(_,_):- tlbugger:ifHideTrace,!.
dumpST(Frame,Opts):-var(Opts),!,dumpST(Frame,5000).
dumpST(Frame,MaxDepth):-integer(MaxDepth),Term = dumpST(MaxDepth),
   (var(Frame)->prolog_current_frame(Frame);true),
   ignore(( get_prolog_backtrace(MaxDepth, Trace,[frame(Frame),goal_depth(100)]),
    format(user_error, '% dumpST ~p', [Term]), nl(user_error),
    print_prolog_backtrace(user_error, Trace,[subgoal_positions(true)]), nl(user_error), fail)),!.
dumpST(Frame,MaxDepth):- integer(MaxDepth),(var(Frame)->prolog_current_frame(Frame);true),dumpST2(Frame,MaxDepth).
dumpST(Frame,Opts):-is_list(Opts),!,dumpST(1,Frame,Opts).
dumpST(Frame,Opts):-show_call(dumpST(1,Frame,[Opts])).

dumpST2(_,_):- tlbugger:ifHideTrace,!.
dumpST2(Frame,From-MaxDepth):-integer(MaxDepth),!,dumpST(Frame,[skip_depth(From),max_depth(MaxDepth),numbervars(safe),show([has_alternatives,level,context_module,goal,clause])]).
dumpST2(Frame,MaxDepth):-integer(MaxDepth),!,dumpST(Frame,[max_depth(MaxDepth),numbervars(safe),show([has_alternatives,level,context_module,goal,clause])]).

get_m_opt(Opts,Max_depth,D100,RetVal):-E=..[Max_depth,V],(((member(E,Opts),nonvar(V)))->RetVal=V;RetVal=D100).

dumpST(_,_,_):- tlbugger:ifHideTrace,!.
dumpST(N,Frame,Opts):-
  ignore(prolog_current_frame(Frame)),
  must(( dumpST4(N,Frame,Opts,Out))),
   must((get_m_opt(Opts,numbervars,-1,Start),
   neg1_numbervars(Out,Start,ROut),
   reverse(ROut,RROut),
   ignore((forall(member(E,RROut),fdmsg(E)))))).

neg1_numbervars(T,-1,T):-!.
neg1_numbervars(Out,Start,ROut):-copy_term(Out,ROut),integer(Start),!,snumbervars(ROut,Start,_).
neg1_numbervars(Out,safe,ROut):-copy_term(Out,ROut),safe_numbervars(ROut).

fdmsg1(txt(S)):-'format'(S,[]),!.
fdmsg1(level=L):-'format'('(~q)',[L]),!.
fdmsg1(context_module=G):-!,format('[~w] ',[G]),!.
fdmsg1(has_alternatives=G):- (G==false->true;'format'('*',[G])),!.
fdmsg1(goal=G):-mesg_color(G,Ctrl),simplify_goal_printed(G,GG),!,ansicall(Ctrl,format(' ~q. ',[GG])),!.
fdmsg1(clause=[F,L]):- directory_file_path(_,FF,F),'format'('  %  ~w:~w: ',[FF,L]),!.
fdmsg1(clause=[F,L]):- fresh_line,'format'('%  ~w:~w: ',[F,L]),!.
fdmsg1(clause=[]):-'format'(' /*DYN*/ ',[]),!.
fdmsg1(G):-mesg_color(G,Ctrl),ansicall(Ctrl,format(' ~q ',[G])),!.
fdmsg1(M):-dmsg(failed_fdmsg1(M)).

fdmsg(fr(List)):-is_list(List),!,fresh_line,ignore(forall(member(E,List),fdmsg1(E))),nl.
fdmsg(M):-dmsg(M).

simplify_goal_printed(G,O):- must(transitive(simplify_goal_printed0,G,O)),!.
simplify_goal_printed(O,O):-!.

:-export(simplify_goal_printed0/2).
simplify_goal_printed0(Var,Var):- \+ compound(Var),!.
simplify_goal_printed0(user:G,G).
simplify_goal_printed0(system:G,G).
simplify_goal_printed0(catchvv(G,_,_),G).
simplify_goal_printed0(call(G),G).
simplify_goal_printed0(G,O):- G=..[F|A],maplist(simplify_goal_printed,A,AA),!,A\==AA,O=..[F|AA].

dumpST4(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,max_depth,100,MD),N>=MD,!.
%dumpST4(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,skip_depth,100,SD),N=<SD,!,fail.
dumpST4(N,Frame,Opts,[fr(Goal)|MORE]):- get_m_opt(Opts,show,goal,Ctrl),getPFA(Frame,Ctrl,Goal),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST4(N,Frame,Opts,[nf(no(Ctrl),N,Frame,Opts)|MORE]):- get_m_opt(Opts,show,goal,Ctrl),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST4(N,Frame,Opts,[nf(noFrame(N,Frame,Opts))]).

dumpST_Parent(N,Frame,Opts,More):- prolog_frame_attribute(Frame,parent,ParentFrame), NN is N +1,dumpST4(NN,ParentFrame,Opts,More),!.
dumpST_Parent(N,Frame,Opts,[nf(noParent(N,Frame,Opts))]).



getPFA(Frame,[L|List],Goal):- !,findall(R, (member(A,[L|List]),getPFA1(Frame,A,R)) ,Goal).
getPFA(Frame,Ctrl,Goal):-getPFA1(Frame,Ctrl,Goal).

getPFA1(_Frame,txt(Txt),txt(Txt)):-!.
getPFA1(Frame,clause,Goal):-getPFA2(Frame,clause,ClRef),clauseST(ClRef,Goal),!.
getPFA1(Frame,Ctrl,Ctrl=Goal):-getPFA2(Frame,Ctrl,Goal),!.
getPFA1(_,Ctrl,no(Ctrl)).

getPFA2(Frame,Ctrl,Goal):- catchvv((prolog_frame_attribute(Frame,Ctrl,Goal)),E,Goal=[error(Ctrl,E)]),!.

clauseST(ClRef,clause=Goal):- findall(V,(member(Prop,[file(V),line_count(V)]),clause_property(ClRef,Prop)),Goal).

clauseST(ClRef,Goal = HB):- ignore(((clause(Head, Body, ClRef),copy_term(((Head :- Body)),HB)))),
   snumbervars(HB,0,_),
   findall(Prop,(member(Prop,[source(_),line_count(_),file(_),fact,erased]),clause_property(ClRef,Prop)),Goal).


% ========================================================================================
% safe_numbervars/1 (just simpler safe_numbervars.. will use a rand9ome start point so if a partially numbered getPrologVars wont get dup getPrologVars)
% Each prolog has a specific way it could unnumber the result of a safe_numbervars
% ========================================================================================
% 7676767
safe_numbervars(E,EE):-duplicate_term(E,EE),
  get_gtime(G),numbervars(EE,G,End,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  term_variables(EE,AttVars),
  numbervars(EE,End,_,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  forall(member(V,AttVars),(copy_term(V,VC,Gs),V='$VAR'(VC=Gs))).
   

get_gtime(G):- get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G).

safe_numbervars(EE):-get_gtime(G),numbervars(EE,G,_End,[attvar(skip),functor_name('$VAR'),singletons(true)]).

%safe_numbervars(Copy,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).
%safe_numbervars(Copy,_,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).

check_varnames(Vs):-var(Vs),!.
check_varnames([]):-!.
check_varnames([N=V|Vs]):-atom(N),var(V),check_varnames(Vs).


%=========================================
% unnumbervars
%=========================================

% source_module(M):-!,M=u.
source_module(M):-nonvar(M),source_module(M0),!,M0=M.
source_module(M):-loading_module(M),!.
source_module(M):-'$set_source_module'(M,   M),!.


:-thread_local(thlocal:last_source_file/1).
loading_file(FIn):- ((source_file0(F) *-> (retractall(thlocal:last_source_file(_)),asserta(thlocal:last_source_file(F))) ; (fail,thlocal:last_source_file(F)))),!,F=FIn.
source_file0(F):-source_location(F,_).
source_file0(F):-prolog_load_context(file, F).
source_file0(F):-prolog_load_context(source, F).
source_file0(F):-seeing(X),is_stream(X),stream_property(X,file_name(F)),exists_file(F).
source_file0(F):-prolog_load_context(stream, S),stream_property(S,file_name(F)),exists_file(F).
source_file0(F):-findall(E,(stream_property( S,mode(read)),stream_property(S,file_name(E)),exists_file(E),line_count(S,C),C>1),L),last(L,F).


source_variables_l(AllS):-
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (b_getval('$variable_names', Vs2);Vs2=[]),
  (parent_goal('$toplevel':'$execute_goal2'(_, Vs3),_);Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  nb_linkval('$variable_names', AllS).


source_variables(Vs):- (((prolog_load_context(variable_names,Vs),Vs\==[]);
   (b_getval('$variable_names', Vs),Vs\==[]))),!.
source_variables(Vars):-var(Vars),parent_goal('$toplevel':'$execute_goal2'(_, Vars),_),!.
source_variables([]).

/*
b_implode_varnames(_):-!.
b_implode_varnames0([]):-!.
b_implode_varnames0([N=V|Vs]):- ignore((V='$VAR'(N);V=N)),b_implode_varnames0(Vs),!.

imploded_copyvars(C,CT):-must((source_variables(Vs),copy_term(C-Vs,CT-VVs),b_implode_varnames(VVs))),!.
*/

:-export(unnumbervars/2).


unnumbervars(X,YY):- unnumbervars0(X,Y),!,must(Y=YY).

/*
unnumbervars(In,Out):- contains_term('$VAR'(I),In),atomic(I),!,must(( term_string(In,String,[numbervars(true)]),term_string(Out,String))),!.
unnumbervars(In,Out):- copy_term(In,Out),!.
unnumbervars(In,Out):- =(In,Out),!.
unnumbervars(X,YY):- fail,
 source_variables(Vs),copy_term(X-Vs,CXVs-CVs),b_implode_varnames(CVs),
     unnumbervars1(CXVs,YY),!.


unnumbervars(X,YY):-unnumbervars0(X,Y),!,must(Y=YY).
*/


unnumbervars0(X,YY):- 
   must_det_l((with_output_to(string(A),write_term(X,[numbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,_NewVars),!,must(YY=Y))).

/*

unnumbervars_and_save(X,YO):-
 term_variables(X,TV),
 must((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   must(atom_to_term(A,Y,NewVars)),
   (NewVars==[]-> YO=X ; (length(TV,TVL),length(NewVars,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVars),Y=X)))).

unnumbervars_and_copy(X,YO):-
 term_variables(X,TV),
 must((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   must(atom_to_term(A,Y,NewVars)),
   (NewVars==[]-> YO=X ; (length(TV,TVL),length(NewVars,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVars),Y=X)))).
*/

%add_newvars(_):-!.
add_newvars(Vs):- (var(Vs);Vs=[]),!.
add_newvars([N=V|Vs]):- add_newvar(N,V),!,add_newvars(Vs).


add_newvar(_,V):-nonvar(V),!.
add_newvar(N,_):-var(N),!.
add_newvar('A',_):-!.
add_newvar('B',_):-!.
add_newvar(N,_):- atom(N),atom_concat('_',_,N),!.
add_newvar(N,V):- 
  b_getval('$variable_names', V0s),
  remove_grounds(V0s,Vs),
 once((member(NN=Was,Vs),N==NN,var(Was),var(V),(Was=V))-> (V0s==Vs->true;nb_linkval('$variable_names',Vs)); nb_linkval('$variable_names',[N=V|Vs])).

remove_grounds(Vs,Vs):-var(Vs),!.
remove_grounds([],[]):-!.
remove_grounds([N=V|NewCNamedVarsS],NewCNamedVarsSG):-
   (N==V;ground(V)),remove_grounds(NewCNamedVarsS,NewCNamedVarsSG).
remove_grounds([N=V|V0s],[N=NV|Vs]):-
   (var(V) -> NV=V ; NV=_ ),
   remove_grounds(V0s,Vs).

% renumbervars(X,X):-ground(X),!.
renumbervars(X,Z):-unnumbervars(X,Y),safe_numbervars(Y,Z),!.
renumbervars(Y,Z):-safe_numbervars(Y,Z),!.

withFormatter(Lang,From,Vars,SForm):-formatter_hook(Lang,From,Vars,SForm),!.
withFormatter(_Lang,From,_Vars,SForm):-sformat(SForm,'~w',[From]).

flush_output_safe:-ignore(catchvv(flush_output,_,true)).
flush_output_safe(X):-ignore(catchvv(flush_output(X),_,true)).

writeFailureLog(E,X):-
		fmt(user_error,'\n% error: ~q ~q\n',[E,X]),flush_output_safe(user_error),!,
		%,true.
		fmt('\n% error: ~q ~q\n',[E,X]),!,flush_output. %,fmt([E,X]).

%unknown(Old, autoload).


%=========================================
% Module Utils
%=========================================


:-export(loading_module/1).
:-module_transparent(loading_module/1).
:-export(loading_module/2).
:-module_transparent(loading_module/2).
:-export(show_module/1).
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
:-export(if_defined/1).
if_defined(Call):-current_predicate(_,Call)->Call.
:- meta_predicate if_defined(?,?).
:-export(if_defined/2).
if_defined(Call,Else):-current_predicate(_,Call)->(Call*->true;fail);Else.
:- meta_predicate when_defined(?).
:-export(when_defined/1).
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




% ===============================================================================================
% join_path(CurrentDir,Filename,Name)
% ===============================================================================================



join_path(CurrentDir,Filename,Name):-
     atom_ensure_endswtih(CurrentDir,'/',Out),atom_ensure_endswtih('./',Right,Filename),
     atom_concat(Out,Right,Name),!.

atom_ensure_endswtih(A,E,A):-atom(E),atom_concat(_Left,E,A),!.
atom_ensure_endswtih(A,E,O):-atom(A),atom(E),atom_concat(A,E,O),!.
atom_ensure_endswtih(A,E,O):-atom(A),atom(O),atom_concat(A,E,O),!.
atom_ensure_endswtih(A,O,O):-atom(A),atom(O),!.

os_to_prolog_filename(OS,_PL):-sanity(atom(OS)),fail.
os_to_prolog_filename(_OS,PL):-sanity(var(PL)),fail.
os_to_prolog_filename(OS,PL):-exists_file_safe(OS),!,PL=OS.
os_to_prolog_filename(OS,PL):-exists_directory_safe(OS),!,PL=OS.
os_to_prolog_filename(OS,PL):-current_directory_search(CurrentDir),join_path(CurrentDir,OS,PL),exists_file_safe(PL),!.
os_to_prolog_filename(OS,PL):-current_directory_search(CurrentDir),join_path(CurrentDir,OS,PL),exists_directory_safe(PL),!.

os_to_prolog_filename(OS,PL):-atom(OS),atomic_list_concat([X,Y|Z],'\\',OS),atomic_list_concat([X,Y|Z],'/',OPS),!,os_to_prolog_filename(OPS,PL).
os_to_prolog_filename(OS,PL):-atom_concat_safe(BeforeSlash,'/',OS),os_to_prolog_filename(BeforeSlash,PL).
os_to_prolog_filename(OS,PL):-absolute_file_name(OS,OSP),OS \== OSP,!,os_to_prolog_filename(OSP,PL).


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

 %:-interactor.

export_all_preds:-source_location(File,_Line),module_property(M,file(File)),!,export_all_preds(M).

export_all_preds(ModuleName):-forall(current_predicate(ModuleName:F/A),
                   ((export(F/A),functor_safe(P,F,A),moo_hide_childs(ModuleName:P)))).







%%:-user:(forall(current_predicate(user:FA),user:moo_hide_childs(user:FA))).
% hide this module from tracing
%%:-user:(forall(current_predicate(logicmoo_util_strings:FA),user:moo_hide_childs(logicmoo_util_strings:FA))).

module_hotrace(M):- forall(predicate_property(P,imported_from(M)),user:moo_hide_childs(M:P)).

:-export(doall/1).
:- meta_predicate doall(0).
doall(M:C):-!, M:ignore(M:(C,fail)).
doall(C):-ignore((C,fail)).


:-meta_predicate clause_safe(?, ?).
:-module_transparent clause_safe/2.
:- export(clause_safe/2).

:- export(clause_safe_m3/3).

clause_safe(M:H,B):-!,debugOnError(clause(M:H,B)).
clause_safe(H,B):-!,debugOnError(clause(H,B)).

clause_safe(M:H,B):-!,clause_safe_m3(M,H,B).
clause_safe(H,B):-!,clause_safe_m3(_,H,B).
%clause_safe(M,string(S),B):- trace_or_throw(clause_safe(M,string(S),B)).
clause_safe_m3(M,H,B):-  (nonvar(H)->true;(trace,current_predicate(M:F/A),functor(H,F,A))),predicate_property(M:H,number_of_clauses(_)),clause(H,B).



% =====================================================================================================================
:- export((call_no_cuts/1)).
% =====================================================================================================================
:- meta_predicate call_no_cuts(0).
:- module_transparent call_no_cuts/1.
call_no_cuts((A,B)):-!,(call_no_cuts(A),call_no_cuts(B)).
call_no_cuts((A;B)):-!,(call_no_cuts(A);call_no_cuts(B)).
call_no_cuts((A->B)):-!,(call_no_cuts(A)->call_no_cuts(B)).
call_no_cuts((A->B;C)):-!,(call_no_cuts(A)->call_no_cuts(B);call_no_cuts(C)).
call_no_cuts(M:CALL):-atom(M),!,functor(CALL,F,A),functor(C,F,A),must(once(not(not(clause_safe(C,_))))),!,clause_safe(CALL,TEST),debugOnError(TEST).
call_no_cuts(CALL):-functor(CALL,F,A),functor(C,F,A),must(once(not(not(clause_safe(C,_))))),!,clause_safe(CALL,TEST),debugOnError(TEST).

/*
:- meta_predicate call_no_cuts_loop_checked(0).
:- module_transparent call_no_cuts_loop_checked/1.
:- meta_predicate call_no_cuts_loop_checked(0,0).
:- module_transparent call_no_cuts_loop_checked/2.
call_no_cuts_loop_checked(Call):-call_no_cuts_loop_checked(Call, fail).
call_no_cuts_loop_checked(Call, TODO):-!, loop_check(Call,TODO).
call_no_cuts_loop_checked(Call, TODO):- clause(Call,Body),make_key(Body,Key),loop_check_term(Body,Key,TODO).
*/

% =====================================================================================================================
:- export((call_tabled/1)).
:- export((cannot_table_call/1)).
:- export((cannot_use_tables/1)).
:- export((skipped_table_call/1)).
% =====================================================================================================================
:- meta_predicate call_tabled(0).
:- module_transparent call_tabled/1.

:- meta_predicate call_vars_tabled(?,0).
:- module_transparent call_vars_tabled/2.

:- meta_predicate((cannot_table_call(0))).
:- meta_predicate((cannot_use_tables(0))).
:- meta_predicate((skipped_table_call(0))).



:- meta_predicate call_setof_tabled(?,0,-).
:- meta_predicate findall_nodupes(?,0,-).
:- module_transparent call_setof_tabled/3.

:- dynamic(table_bugger:call_tabled_cached_results/2).
:- dynamic(table_bugger:call_tabled_perm/2).
:- thread_local table_bugger:maybe_table_key/1.

retract_can_table :- retractall(maybe_table_key(_)).

:- meta_predicate(make_key(?,-)).

:-module_transparent(ex/0).
lex:-listing(tlbugger:ilc(_)),forall(current_predicate(table_bugger:F/A),listing(table_bugger:F/A)),catchvv(listing(user:already_added_this_round),_,true).
ex:-expire_tabled_list(_),retractall(tlbugger:ilc(_)),dmsg_showall(_),forall(current_predicate(table_bugger:F/A),(functor(RA,F,A),retractall(RA))),catchvv(expire_dont_add,_,true).

expire_tabled_list(V):-var(V),!,retractall(table_bugger:call_tabled_cached_results(_,_)).
expire_tabled_list(_):-!,retractall(table_bugger:call_tabled_cached_results(_,_)).
expire_tabled_list(T):- atoms_of(T,A1), CT= table_bugger:call_tabled_cached_results(Key,List),ignore(((CT,once(any_term_overlap_atoms_of(A1,List);(not(member(Key,List)),
  any_term_overlap_atoms_of(A1,Key))),retractall(CT)),fail)).

any_term_overlap_atoms_of(A1,T2):-atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

any_term_overlap(T1,T2):- atoms_of(T1,A1),atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

:-meta_predicate(make_tabled_perm(0)).

make_tabled_perm(Call):- must(really_can_table),must(outside_of_loop_check),
  term_variables(Call,Vars),!,make_key(Vars+Call,LKey),reduce_make_key(LKey,Key),
  findall(Vars,with_no_assertions(tlbugger:cannot_save_table,no_loop_check_unsafe(no_repeats_old(Call))),KList),
  must(KList=[_|_]),!,
  asserta(table_bugger:call_tabled_perm(Key,KList)),!,
  member(Vars,KList).




:-thread_local tlbugger:cannot_save_table/0.
:-thread_local tlbugger:cannot_use_any_tables/0.

skipped_table_call(Call):- cannot_use_tables(cannot_table_call(Call)).
cannot_table_call(Call):- with_assertions( tlbugger:cannot_save_table,Call).
cannot_use_tables(Call):- with_assertions( tlbugger:cannot_use_any_tables,Call).

call_tabled(setof(Vars,C,List)):- !,call_setof_tabled(Vars,C,List).
call_tabled(findall(Vars,C,List)):- !,call_setof_tabled(Vars,C,List).
call_tabled(C):- sanity(nonvar(C)), term_variables(C,Vars),!,call_vars_tabled(Vars,C).

call_vars_tabled(Vars,C):- call_setof_tabled(Vars,C,Set),!,member(Vars,Set).

call_setof_tabled(Vars,C,List):- make_key(Vars+C,Key),call_tabled0(Key,Vars,C,List).

findall_nodupes(Vs,C,List):- ground(Vs),!,(C->List=[Vs];List=[]),!.
findall_nodupes(Vs,C,L):- findall(Vs,no_repeats_old(Vs,call_t(C)),L).
%findall_nodupes(Vs,C,L):- setof(Vs,no_repeats_old(Vs,C),L).


:-meta_predicate(call_tabled0(?,?,?,?)).
:-meta_predicate(call_tabled1(?,?,?,?)).
%call_tabled0(Key,Vars,C,List):- table_bugger:maybe_table_key(Key),dmsg(looped_findall_nodupes(Vars,C,List)),fail.
call_tabled0( Key,_,_,List):- reduce_make_key(Key,RKey),table_bugger:call_tabled_perm(RKey,List),!.
call_tabled0( Key,_,_,List):- \+ (tlbugger:cannot_use_any_tables), table_bugger:call_tabled_cached_results(Key,List),!.
call_tabled0(_Key,Vars,C,List):- tlbugger:cannot_save_table,!,findall_nodupes(Vars,C,List).

call_tabled0(Key,Vars,C,List):- outside_of_loop_check,!, findall_nodupes(Vars,C,List),
  ignore((really_can_table,!,asserta_if_ground(table_bugger:call_tabled_cached_results(Key,List)))),!.

%call_tabled0(Key,Vars,C,List):- table_bugger:maybe_table_key(Key),!,findall_nodupes(Vars,C,List).

call_tabled0(Key,Vars,C,List):-call_tabled1(Key,Vars,C,List).

call_tabled1(Key,Vars,C,List):- asserta(table_bugger:maybe_table_key(Key)), findall_nodupes(Vars,C,List),
  ignore((really_can_table,!,
  % if table_bugger:maybe_table_key(Key) is now missing that meant a loop_checker had limited some results
  show_call_failure(retract(table_bugger:maybe_table_key(Key))),!,
  asserta_if_ground(table_bugger:call_tabled_cached_results(Key,List)))),!.

really_can_table:- not(test_tl(tlbugger:cannot_save_table)),!.

outside_of_loop_check:- (clause(tlbugger:ilc(_),B)->B=(!,fail);true).


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




:- export(no_repeats_findall5/5).
:- meta_predicate no_repeats_findall5(+,0,-,-,-).
no_repeats_findall5(Vs,Call,ExitDET,USE,NEW):- 
   (((HOLDER = fa([]),
   Call,arg(1,HOLDER,CONS),
   ((
   ((\+ memberchk_same(Vs,CONS), 
   copy_term(Vs,CVs), 
   append(CONS,[CVs],NEW),
    nb_setarg(1, HOLDER, NEW)))
      ->
       USE=true;
       ((USE=false,CONS=NEW))
       )),
   deterministic(ExitDET))) 
    *-> true;
     (NEW=[],ExitDET=true,USE=false)).

:- export(no_repeats_save/4).
:- meta_predicate no_repeats_save(+,0,-,-).
no_repeats_save(Vs,Call,Saved,USE):-
 SavedHolder = saved(_),
  no_repeats_findall5(Vs,Call,ExitDET,USE,NEW),
  ( ExitDET==true -> (nb_linkarg(1,SavedHolder,NEW),!) ; true),  
  arg(1,SavedHolder,Saved).

:- export(no_repeats_save/2).
:- meta_predicate no_repeats_save(+,0).
no_repeats_save(Vs,Call):-  
  call_cleanup(
   (( no_repeats_save(Vs,Call,SavedList,USE),
      (USE==true -> true ; fail))), 
   (is_list(SavedList) -> writeln(saving(SavedList)) ; writeln(givingup_on(Call)))).
  

:- export(no_repeats_findall_r/5).
:- meta_predicate no_repeats_findall_r(+,0,-,-,-).
no_repeats_findall_r(Vs,Call,CONS,ExitDET,List):- 
   CONS = [ExitDET],
   (Call,once((\+ memberchk_same(Vs,CONS), copy_term(Vs,CVs), CONS=[_|T],List=[CVs|T], nb_linkarg(2, CONS, List)))),
   deterministic(ExitDET).

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
dtrace:- dtrace(ignore(show_call_failure(trace))).


% esa Michele Murer 360-750-7500 ext_135

:-meta_predicate(dtrace(+,0)).
dtrace(MSG,G):-wdmsg(error,MSG),dtrace(G).

:-meta_predicate(dtrace(0)).
dtrace(G):- \+ tlbugger:ifCanTrace,!,hotrace((wdmsg((not(tlbugger:ifCanTrace(G)))))),!,badfood(G),!,hotrace(dumpST).
dtrace(G):-has_auto_trace(C),wdmsg(has_auto_trace(C,G)),!,call(C,G).
dtrace(G):-tracing,notrace,!,wdmsg(tracing_dtrace(G)),(dumptrace(G)*->trace;trace).
dtrace(G):-current_predicate(logicmoo_bugger_loaded/0),!,dumptrace(G).
dtrace(G):-G.

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


module_predicate(ModuleName,F,A):-current_predicate(ModuleName:F/A),functor_safe(P,F,A),
   not((( predicate_property(ModuleName:P,imported_from(IM)),IM\==ModuleName ))).

:-module_transparent(module_predicates_are_exported/0).
:-module_transparent(module_predicates_are_exported/1).
:-module_transparent(module_predicates_are_exported0/1).

module_predicates_are_exported:- context_module(CM),module_predicates_are_exported(CM).

module_predicates_are_exported(user):-!,context_module(CM),module_predicates_are_exported0(CM).
module_predicates_are_exported(Ctx):- show_call(module_predicates_are_exported0(Ctx)).

module_predicates_are_exported0(user):- !. % dmsg(warn(module_predicates_are_exported(user))).
module_predicates_are_exported0(ModuleName):-
   module_property(ModuleName, exports(List)),
    findall(F/A,
    (module_predicate(ModuleName,F,A),
      not(member(F/A,List))), Private),
   module_predicates_are_not_exported_list(ModuleName,Private).

:-export(export_if_noconflict/2).
:-module_transparent(export_if_noconflict/2).
export_if_noconflict(M,F/A):- current_module(M2),M2\=M,module_property(M2,exports(X)),member(F/A,X),dmsg(skipping_export(M2=M:F/A)),!.
export_if_noconflict(M,F/A):-M:export(F/A).

% module_predicates_are_not_exported_list(ModuleName,Private):- once((length(Private,Len),dmsg(module_predicates_are_not_exported_list(ModuleName,Len)))),fail.
module_predicates_are_not_exported_list(ModuleName,Private):- forall(member(F/A,Private),export_if_noconflict(ModuleName,F/A)).





arg_is_transparent(Arg):- member(Arg,[':','0']).
arg_is_transparent(0).
arg_is_transparent(Arg):- number(Arg).

% make meta_predicate's module_transparent
module_meta_predicates_are_transparent(_):-!.
module_meta_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore(((predicate_property(ModuleName:P,(meta_predicate( P ))),
            not(predicate_property(ModuleName:P,(transparent))), (compound(P),arg(_,P,Arg),arg_is_transparent(Arg))),
                   (dmsg(todo(module_transparent(ModuleName:F/A))),
                   (module_transparent(ModuleName:F/A)))))).

:- export(all_module_predicates_are_transparent/1).
% all_module_predicates_are_transparent(_):-!.
all_module_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore((
            not(predicate_property(ModuleName:P,(transparent))),
                   ( dmsg(todo(module_transparent(ModuleName:F/A)))),
                   (module_transparent(ModuleName:F/A))))).

quiet_all_module_predicates_are_transparent(_):-!.
quiet_all_module_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore((
            not(predicate_property(ModuleName:P,(transparent))),
                   nop(dmsg(todo(module_transparent(ModuleName:F/A)))),
                   (module_transparent(ModuleName:F/A))))).


:- multifile user:listing_mpred_hook/1.
:- dynamic user:listing_mpred_hook/1.

:-export((term_listing/1)).
term_listing([]):-!.
term_listing(Match):- 
  '@'(ignore((catchvv(listing(Match),E,wdmsg(E)))),'user'),
  term_non_listing(Match),!.

user:listing_mpred_hook(_):-fail.
user:term_mpred_listing(Match):-
 current_predicate(user:listing_mpred_hook/1), 
 %format('/* listing_mpred_hook(~q) => ~n',[Match]),!,
 once(debugOnError(doall(call_no_cuts(user:listing_mpred_hook(Match))))),
 !. %format(' <= listing_mpred_hook(~q) */ ~n',[Match]).

:-export(term_non_listing/1).
term_non_listing(Match):- 
   format('/* term_non_listing(~q) => ~n',[Match]),
      term_listing_inner(portray_hbr,Match),
   format(' <= term_non_listing(~q) */ ~n',[Match]).

term_listing_inner(Pred,Match):-
   '@'(ignore((doall((
      synth_clause_for(H,B,Ref),
      once(ok_show(H)),
      once(slow_term_matches_hb(Match,H,B)),
      once(call(Pred,H,B,Ref)),
      fail)))),'user').

:- multifile user:prolog_list_goal/1.
% user:prolog_list_goal(Goal):- writeq(hello(prolog_list_goal(Goal))),nl.

:- dynamic(user:no_buggery/0).
user:buggery_ok :- \+ compiling, current_predicate(logicmoo_bugger_loaded/0), \+ user:no_buggery.

:- multifile prolog:locate_clauses/2.
prolog:locate_clauses(A, _) :- current_predicate(logicmoo_bugger_loaded/0),buggery_ok ,current_predicate(user:term_mpred_listing/1),call_no_cuts(user:term_mpred_listing(A)),fail.



:-multifile((synth_clause_for/3)).
:-export((synth_clause_for/3)).
synth_clause_for(H,B,Ref):- user:((cur_predicate(_,H),synth_clause_ref(H,B,Ref))).


:-export((synth_clause_ref/3)).
synth_clause_ref(M:H,B,Ref):-M==user,!,synth_clause_ref(H,B,Ref).
synth_clause_ref(H,(fail,synth_clause_info(Props)),0):- (H\=(_:-_)),once(pred_info(H,Props)).
synth_clause_ref(H,B,Ref):- predicate_property(M:H,number_of_clauses(_)),!,clause(M:H,B,Ref).

:-export((term_matches_hb/3)).
term_matches_hb([],_,_):-!.
term_matches_hb([F1],H,B):-!,term_matches_hb(F1,H,B),!.
term_matches_hb([F1|FS],H,B):-!,term_matches_hb((F1;FS),H,B).
term_matches_hb([F1+FS],H,B):-!,term_matches_hb((F1,FS),H,B).
term_matches_hb((F1,FS),H,B):-!,term_matches_hb(F1,H,B),term_matches_hb(FS,H,B).
term_matches_hb((F1;FS),H,B):-!, (term_matches_hb(F1,H,B);term_matches_hb(FS,H,B)).

term_matches_hb(arity(A),H,_):-!,functor(H,_,A).
term_matches_hb(functor(F),H,_):-!,functor(H,F,_).
term_matches_hb(not(C),H,B):-nonvar(C),!,term_matches_hb(-(C),H,B).

term_matches_hb(-(C),H,B):-nonvar(C),!,not(term_matches_hb(C,H,B)).
term_matches_hb(+(C),H,B):-nonvar(C),!,(term_matches_hb(C,H,B)).
term_matches_hb(module(M),H,_):-!,predicate_property(H,imported_from(M)).
term_matches_hb(F/A,H,_):-atom(F),integer(A),!,functor(H,F,A).

term_matches_hb(h(P),H,_):-!,term_matches_hb(P,H,666666).
term_matches_hb(b(P),_,B):-!,term_matches_hb(P,666666,B).
term_matches_hb(HO,H,B):- string(HO),!, term_matches_hb_2(contains,HO,H,B).
term_matches_hb(string(HO),H,B):- !, term_matches_hb_2(contains,HO,H,B).
term_matches_hb(contains(HO),H,B):-!, term_matches_hb_2(contains,HO,H,B).


:-export(slow_term_matches_hb/3).
slow_term_matches_hb(noinfo,_,info(_)):-!,fail. 
slow_term_matches_hb(HO,H,B):- atom(HO),!, term_matches_hb_2(exact,HO,H,B).
slow_term_matches_hb(HO,H,B):-term_matches_hb(HO,H,B).
slow_term_matches_hb(M:HO,H,B):-!,term_matches_hb(module(M),H,B),!,term_matches_hb(h(HO),H,B).
slow_term_matches_hb(HO,H,B):- !,term_matches_hb_2(exact,HO,H,B).

:-export(fast_term_matches_hb/3).
fast_term_matches_hb(HO,H,B):-contains_var(HO,(H:-B)).
fast_term_matches_hb(HO,H,B):-term_matches_hb(HO,H,B).

:-export(term_matches_hb_2/4).
term_matches_hb_2(contains,HO,H,B):- any_to_string(HO,HS),!, with_output_to(string(H1B1),write_canonical((H:-B))), (sub_atom_icasechk(HS,_,H1B1);sub_atom_icasechk(H1B1,_,HS)),!.
term_matches_hb_2(exact,HO,H,B):- contains_var(HO,(H:-B)).

% match_term_listing(HO,H,B):-!, synth_clause_ref(H,B,_Ref), term_matches_hb(HO,H,B).

nonvar_search(V):-var(V),!,fail.
nonvar_search(M:_/A):-nonvar(M),!,nonvar(A).
nonvar_search(M:P):-nonvar(M),nonvar(P).
nonvar_search(F/A):-!,nonvar(F),nonvar(A).
%nonvar_search(F):-atom(F).
%nonvar_search(P):-compound(F).

:-dynamic(cur_predicates/1).
:-export((cur_predicate)/2).
cur_predicate(M:F/A,M:P):-
   !,
   current_predicate(M:F/A),functor(P,F,A),not(predicate_property(user:P,imported_from(_))).
/*
cur_predicate(M:F/A,MP):-atom(M),var(F),!,cur_predicate(M,F/A,MP).
cur_predicate(MFA,M:P):-atom(M),var(P),!,cur_predicate(M,MFA,P).
cur_predicate(M:FA,MP):-atom(M),!,cur_predicate(M,FA,MP).
cur_predicate(MFA,MP):-cur_predicate(_,MFA,MP).
cur_predicate(M,MFA,MP):-nonvar(M),
 M:call(cur_predicate(MFA,MP)).

cur_predicate(SM,MFA,MP):-
   (

 %nonvar_search(MFA) -> SEARCH=MFA ; 
 %nonvar_search(MP) -> SEARCH=MP ;
    
  nonvar(MFA) ->    (current_predicate(MFA),SEARCH=MFA);
    
  nonvar(MP) ->     (current_predicate(_,MP),SEARCH=MP);
   
   nonvar(SM) ->  (SM:current_predicate(SF/SA),SEARCH=M:SF/SA);
                      
    current_predicate(SEARCH)),
  no_repeats(M:F/A,('$find_predicate'(SEARCH,MFAs),
   
  member(M:F/A,MFAs))),
   debug,
   must_det_l((
  once(two_mfa(MFA, M,F,A)),

       functor(P,F,A),
  once(to_mp(MP,M,P)))).
   ignore(SM=M).


% match_mfa(MFA1,MFA2):-must_det_l((to_mfa(MFA1,M1,F1,A1),to_mfa(MFA2,M2,F2,A2))),A1=A2,F1=F2,!,ignore(M1=M2).


two_mfa(M1:F/A,M2,F,A):-!,ignore(M1=M2).
two_mfa(M1:F,M2,F,_):-atom(F),!,ignore(M1=M2).
two_mfa(M1:FA,M2,F,A):- nonvar(FA),!, get_functor(FA,F,A),ignore(M1=M2).

two_mfa(F,_,F,_):- atom(F),!.
two_mfa(F/A,_,F,A):- !.
to_mp(M1:P,M2,P):-ignore(M1=M2).
to_mp(P,_,P):-nonvar(P),!.
*/

:-export(ok_show/1).
ok_show(F/A):-!,functor(P,F,A),ok_show(P),!.
ok_show(P):-not(bad_pred(P)).



% when we import new and awefull code base (the previous )this can be helpfull
% we redfine list_undefined/1 .. this is the old version
:- export(scansrc_list_undefined/1).
scansrc_list_undefined(_):-!.
scansrc_list_undefined(A):- real_list_undefined(A).


:- export(real_list_undefined/1).
real_list_undefined(A):- 
 merge_options(A, [module_class([user])], B),
        prolog_walk_code([undefined(trace), on_trace(found_undef)|B]),
        findall(C-D, retract(undef(C, D)), E),
        (   E==[]
        ->  true
        ;   print_message(warning, check(undefined_predicates)),
            keysort(E, F),
            group_pairs_by_key(F, G),
            maplist(check:report_undefined, G)
        ).

:-export(mmake/0).
mmake:- update_changed_files , if_defined(load_mpred_files).
:-export(update_changed_files/0).
update_changed_files :-
        set_prolog_flag(verbose_load,true),
        ensure_loaded(library(make)),
	findall(File, make:modified_file(File), Reload0),
	list_to_set(Reload0, Reload),
	(   prolog:make_hook(before, Reload)
	->  true
	;   true
	),
	print_message(silent, make(reload(Reload))),
	maplist(make:reload_file, Reload),
	print_message(silent, make(done(Reload))),
	(   prolog:make_hook(after, Reload)
	->  true
	;   
           true %list_undefined,list_void_declarations
	).

:- export(remove_undef_search/0).
% check:list_undefined:-real_list_undefined([]).
remove_undef_search:- ((
 '@'(use_module(library(check)),'user'),
 redefine_system_predicate(check:list_undefined(_)),
 abolish(check:list_undefined/1),
 assert((check:list_undefined(A):- not(thread_self(main)),!, ignore(A=[]))),
 assert((check:list_undefined(A):- reload_library_index,  update_changed_files,call(thread_self(main)),!, ignore(A=[]))),
 assert((check:list_undefined(A):- ignore(A=[]),scansrc_list_undefined(A))))).

:- remove_undef_search.


mp(M,P,MP):-atom(M),!,(MP=M:P ; MP=P).
mp(_,P,MP):-MP=P.


:-export(bad_pred/1).
bad_pred(M:P):-!,atom(M),bad_pred(P). 
bad_pred(P):-functor(P,F,A),arg(_,v(cur_predicates/_,db_op/_,db_op00/_,db_op0/_,db_op_loop/_,do_expand_args_l/3),F/A).
bad_pred(P):-predicate_property(P,autoloaded(_)).
bad_pred(P):-not(predicate_property(P,number_of_clauses(_))).
bad_pred(P):-predicate_property(P,imported_from(_)),predicate_property(P,static).
bad_pred(P):-predicate_property(P,foreign).

:-export(pred_info/2).
pred_info(H,Props):- get_functor(H,F,_),findall(PP,user:mpred_prop(F,PP),Props).

:-export(portray_hbr/3).
portray_hbr(H,B,_):- B==true, !, portray_one_line(H).
portray_hbr(H,B,_):- portray_one_line((H:-B)).

:-export(portray_one_line/1).
:-thread_local(portray_one_line_hook/1).
portray_one_line(H):- portray_one_line_hook(H),!.
portray_one_line(H):- current_predicate(wdmsg/1),wdmsg(H),!.
portray_one_line(H):- not(not((snumbervars(H),writeq(H),write('.'),nl))),!.
portray_one_line(H):- writeq(H),write('.'),nl,!.





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



:-export((portray_clause_w_vars/4,dmsg5/2,ansicall/3,ansi_control_conv/2)).

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

:-dynamic(did_ref_job/1).
do_ref_job(_Body,Ref):-did_ref_job(Ref),!.
do_ref_job(Body ,Ref):-asserta(did_ref_job(Ref)),!,show_call(Body).


:- all_module_predicates_are_transparent(user).
:- module_predicates_are_exported.
:- module_meta_predicates_are_transparent(user).
:- all_module_predicates_are_transparent(logicmoo_util_bugger_catch).

:-module_property(user, exports(List)),moo_show_childs(List).

% bugger_prolog_exception_hook(error(syntax_error(operator_expected),_),_,_,_).
bugger_prolog_exception_hook(Info,_,_,_):- bugger_error_info(Info),!, dumpST,dmsg(prolog_exception_hook(Info)), dtrace.

bugger_error_info(C):-contains_var(type_error,C).
bugger_error_info(C):-contains_var(instantiation_error,C).
bugger_error_info(C):-contains_var(existence_error(procedure,_/_),C).

:-thread_self(ID),current_input(In),asserta(thread_current_input(ID,In)).

:-listing(thread_current_input/2).

% Installs exception reporter.
:- multifile(user:prolog_exception_hook/4).
:- dynamic(user:prolog_exception_hook/4).
% Writes exceptions with stacktrace into stderr.
% Fail/0 call at the end allows the exception to be
% processed by other hooks too.
disabled_this:- asserta((user:prolog_exception_hook(Exception, Exception, Frame, _):- 
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
    format(user_error, 'Error ST-Begin: ~p', [Term]), nl(user_error),
    ignore((thread_current_input(main,In),see(In))),
    dumpST(Frame,20),

    dtrace(Goal),
    format(user_error, 'Error ST-End: ~p', [Term]), nl(user_error),
    nl(user_error), fail)).


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

%system:goal_expansion(LC,LCOO):-nonvar(LC),transitive(lco_goal_expansion,LC,LCO),LC\=@=LCO,must(LCO=LCOO),!.
%system:term_expansion(LC,LCOO):-nonvar(LC),transitive(lco_goal_expansion,LC,LCO),LC\=@=LCO,must(LCO=LCOO),!.
% user:term_expansion(LC,LCOO):-nonvar(LC),(LC=(H:-B)),lco_goal_expansion(B,BE),B\=@=BE,((H:-BE)=LCOO).
user:goal_expansion(LC,LCOO):- current_predicate(logicmoo_bugger_loaded/0),once(lco_goal_expansion(LC,LCOO)),LC\=@=LCOO.

:- '$find_predicate'(tlbugger:A/0,O),forall(member(M:F/A,O),moo_hide(M,F,A)).

logicmoo_bugger_loaded.


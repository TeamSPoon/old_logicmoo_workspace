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
:-module(bugger,[
     was_module/2,
     with_dmsg/2,
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
         loop_check_clauses/3,
         loop_check_clauses/2,
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
     module_notrace/1,
     user_use_module/1,
     dumpST/1,
     dtrace/0,
     trace_or_throw/1,
     trace_or/1,
     os_to_prolog_filename/2,
     debugOnFailure0/1,
   ((meta_predicate_transparent) /1),      
      op(1150,fx,meta_predicate_transparent),
      op(1150,fx,decl_thlocal),
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
     cmust/1, % doesnt run on release
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
	 sendNote/1,
	 sendNote/4,
	 writeFailureLog/2,
	% debugOnFailure/2,
     debugOnFailureEach/1,
    moo_hide_childs/1,

     fmt/1,fmt0/1,
     fmt/2,fmt0/2,
     fmt/3,fmt0/3,

     unlistify/2,
     listify/2,

     ifCanTrace/0,
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
      
      op(1150,fx,(dynamic_multifile_exported)),
      export_all_preds/0,
     export_all_preds/1 ]).

:- set_prolog_flag(generate_debug_info, true).

:- use_module(logicmoo_util_bugger_catch).
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


:-export(nop/1).
nop(_).
:-export(dumpST/0).

% functor_safe(P,F,A):- catch(compound_name_arity(P,F,A),_,functor(P,F,A)).

erase_safe(Which,REF):-var(REF)-> ddmsg(var_erase_safe(Which,REF)) ; erase(REF).

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

:-export(moo_show_childs/1).
:-module_transparent(moo_show_childs/1).
moo_show_childs(X):- with_preds(X,_M,F,A,_PI,'$hide'(F/A)).

moo_show_childs(M,F,A):-functor_safe(MPred,F,A),moo_show_childs(M,F,A,MPred).
moo_show_childs(M,_,_, MPred):- not(predicate_property(_:MPred,imported_from(M))).
moo_show_childs(M,F,A,_MPred):- moo_trace_hidechilds(M,F,A,0,0).




% ----------
:- export(static_predicate/3).
:- meta_predicate(static_predicate(+,+,+)).
static_predicate(M,F,A):- functor_safe(FA,F,A),  once(M:predicate_property(FA,_)),not(M:predicate_property(FA,dynamic)),not((M:predicate_property(FA,imported_from(Where)),Where \== M)).


:- export((((dynamic_safe)/1))).
:- meta_predicate(dynamic_safe(0)).
:- module_transparent((((dynamic_safe)/1))).
dynamic_safe(MFA):- with_mfa(MFA,dynamic_safe).

:- export((((dynamic_safe)/3))).
:- meta_predicate(dynamic_safe(+,+,+)).
:- module_transparent((((dynamic_safe)/3))).
dynamic_safe(M,F,A):- (static_predicate(M,F,A) -> true ; M:dynamic(M:F/A)). % , warn_module_dupes(M,F,A).
:-op(1150,fx,dynamic_safe).



% ----------
:-export(with_pi/2).
:- module_transparent(with_pi/2).
:- meta_predicate(with_pi(:,4)).
with_pi([],_):-!.
with_pi((P1,P2),Pred3):-!, with_pi(P1,Pred3),with_pi(P2,Pred3).
with_pi(P  ,Pred3):- context_module(M),with_pi_selected(M,M,P,Pred3),!.

:- export(with_pi_selected/4).
:- meta_predicate(with_pi_selected(+,+,+,+)).
with_pi_selected(CM, M,[P|L],Pred3):-!,with_pi_selected(CM,M,P,  Pred3),with_pi_selected(CM,M,L,Pred3).
with_pi_selected(CM,M,(P,L),Pred3):-!,with_pi_selected(CM,M,P,  Pred3),with_pi_selected(CM,M,L,Pred3).
with_pi_selected(_CM,_M,[]  ,_Pred3):-!.
with_pi_selected(CM,M,[P]  ,Pred3):-!,with_pi_selected(CM,M,P,  Pred3).
with_pi_selected(CM,_, M:F/A,Pred3):-!,with_pi_selected(CM,M,F/A,Pred3).
with_pi_selected(CM,_, M:P ,Pred3):-!,with_pi_selected(CM,M,P,  Pred3).
with_pi_selected(CM,M, F/A ,Pred3):-!,functor_safe(P,F,A),  with_pi_stub(CM,M,P,F/A,Pred3).
with_pi_selected(CM,M, P ,Pred3):-  functor_safe(P,F,A), with_pi_stub(CM,M,P,F/A,Pred3).


% ----------
:- export(within_module/2).
:- meta_predicate(within_module(0,?)).
within_module(Call,M):- '@'(Call,M).
:- meta_predicate(within_module_broken(0,?)).
within_module_broken(Call,M):- context_module(CM), module(M), call_cleanup(Call, module(CM)).
% system:@(A, B) :- call(@(A, B)).


:- export(with_pi_stub/5).
with_pi_stub(CM, M,P, F/A , CM:Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),
   (within_module(M:call(Pred3,CM, M,P,F/A),CM)),!.
with_pi_stub(CM, M,P, F/A , Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),
   (within_module(M:call(Pred3,CM, M,P,F/A),CM)),!.
with_pi_stub(CM, M,P,FA,Pred3):- throw(invalide_args(CM, M,P,FA,Pred3)).
% ----------

:- export(with_mfa/2).
:- module_transparent(with_mfa/2).
:- meta_predicate(with_mfa(:,3)).
with_mfa(P  ,Pred3):- with_pi(P,with_mfa_of(Pred3)).

:- module_transparent(with_mfa_of/5).
:- meta_predicate(with_mfa_of(3,+,+,+,+)).
with_mfa_of(Pred3,_CM,M,_P,F/A):-M:call(Pred3,M,F,A).

% ----------

% ===================================================================
% Substitution based on ==
% ===================================================================
% Usage: dbgsubst(+Fml,+X,+Sk,?FmlSk)

:-export(dbgsubst/4).
dbgsubst(A,B,C,D):-var(A),!,dmsg(dbgsubst(A,B,C,D)),dumpST,dtrace,dbgsubst0(A,B,C,D).
dbgsubst(A,B,C,D):-dbgsubst0(A,B,C,D).

dbgsubst0(A,B,C,D):- 
      catchv(notrace(nd_dbgsubst(A,B,C,D)),E,(dumpST,dmsg(E:nd_dbgsubst(A,B,C,D)),fail)),!.
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


:-module_transparent(make_transparent/4).
:-export(make_transparent/4).

make_transparent(_CM,M,_PI,F/0):-!, M:meta_predicate(F).
make_transparent(_CM,M,PI,F/A):-
   notrace(((var(PI)->functor_safe(PI,F,A);true),
   M:module_transparent(F/A),
   fill_args(PI,('?')),!,
   dbgsubst(PI, (0),(0),PI1),
   dbgsubst(PI1,(0),(0),PI2),
   dbgsubst(PI2,(:),(:),PI3),
   (compound(PI3) -> M:meta_predicate(PI3) ; true))).

% ----------

:- export((dynamic_multifile_exported)/1).
:- export((dynamic_multifile_exported)/4).
:- meta_predicate(( dynamic_multifile_exported(:), dynamic_multifile_exported(+,+,+,+))).
:- module_transparent((dynamic_multifile_exported)/1).
dynamic_multifile_exported( FA ):- with_pi(FA,(dynamic_multifile_exported)).
dynamic_multifile_exported(CM, M, _PI, F/A):-'@'((dynamic_safe(M,F,A), 
   M:multifile(F/A), 
   CM:multifile(F/A),
   % make_transparent(CM,M,PI,F/A),
   M:export(F/A)),M). %,dmsg(dynamic_multifile_exported(CALL)).

% ----------

:- export(((meta_predicate_transparent)/1)).
:- meta_predicate(( meta_predicate_transparent(:),  meta_predicate_transparent(+,+,+,+))).
meta_predicate_transparent(CM:M:F/A):- functor_safe(PI,F,A), !, 
   meta_predicate_transparent(CM,M,PI,F/A).
meta_predicate_transparent(CM:F/A):- functor_safe(PI,F,A), !, 
   meta_predicate_transparent(CM,CM,PI,F/A).
meta_predicate_transparent(MP):-with_pi(MP,(meta_predicate_transparent)).

meta_predicate_transparent(CM,M,PI,F/A):-
   M:multifile(F/A),
   CM:multifile(F/A),   
   M:export(F/A),
   make_transparent(CM,M,PI,F/A),!. 
   % dynamic_multifile_exported(CM, M,PI,F/A).

:-export(fill_args/2).
fill_args([Arg|More],With):-!,ignore(With=Arg),fill_args(More,With).
fill_args([],_).
fill_args(PI,With):-compound_name_arguments(PI,_,ARGS),fill_args(ARGS,With).

:- meta_predicate_transparent(meta_predicate_transparent(0)).


:- export(def_meta_predicate/3).
:- meta_predicate_transparent((def_meta_predicate(0,+,+))).

def_meta_predicate(M:F,S,E):-!,doall(((between(S,E,N),make_list('?',N,List),compound_name_arguments(CALL,F,List),'@'(meta_predicate_transparent(CALL),M)))).
def_meta_predicate(F,S,E):- trace_or_throw(def_meta_predicate(F,S,E)).



:- meta_predicate(moo_hide_all(:)).
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
   '$syspreds':'$hide'(F/A),noprofile(M:F/A),
   nospy(M:P)),!.
moo_hide_all(W):- throw(error(moo_hide_all(W), _)).

:-export(moo_hide_all/1).



%=========================================
% Module Utils
%=========================================
:-export(moo_hide_childs/1).
:-module_transparent(moo_hide_childs/1).
:- meta_predicate(moo_hide_childs(0)).

% module_functor(PredImpl,Module,Pred,Arity).

module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).
strip_arity(Pred/Arity,Pred,Arity).
strip_arity(PredImpl,Pred,Arity):-functor_safe(PredImpl,Pred,Arity).
% moo_hide_childs(+Pred).
:-export(moo_hide_childs/1).

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

moo_hide_childs_0(N,MPred):-
predicate_property(MPred,imported_from(M)),
writeq(wont_hide(M,N,MPred)),nl.

moo_hide_childs(M,F,A):-moo_trace_hidechilds(M,F,A,1,1).



moo_trace_hidechilds(M,F,A,Trace,HideChilds):-
'$set_predicate_attribute'(M:F/A, trace, Trace),
'$set_predicate_attribute'(M:F/A, noprofile, 1),
'$set_predicate_attribute'(M:F/A, hide_childs, HideChilds),!.

:- create_prolog_flag(bugger_debug,filter,[type(term),keep(true)]).
:- create_prolog_flag(opt_debug,filter,[type(term),keep(true)]).
:- create_prolog_flag(dmsg_color,true,[type(boolean),keep(false)]).

%:-thread_local( /*tlbugger:*/ bugger_prolog_flag/2).
:-thread_local( /*tlbugger:*/ skipMust/0).

%:-multifile( /*tlbugger:*/ bugger_prolog_flag/2).
%:-export( /*tlbugger:*/ bugger_prolog_flag/2).



must(C):-  /*tlbugger:*/ skipMust,!,catch(C,E,(wdmsg(E:C),fail)).
must(C):- catch(C,E,(wdmsg(E:C),fail)) *-> true ; (wdmsg(failed_must(C)),dtrace(C)).

/*
current_prolog_flag(N,VV):-
   (( /*tlbugger:*/ bugger_prolog_flag(N,V),
   ignore(current_prolog_flag(N,VO)),!,(VO=@=V -> true; ddmsg_call(set_prolog_flag(N,VO))));(current_prolog_flag(N,V),asserta( /*tlbugger:*/ bugger_prolog_flag(N,V)))),!, V=VV.

set_prolog_flag(N,V):- current_prolog_flag(N,VV),!,
        (V==VV ->  true ; (asserta( /*tlbugger:*/ bugger_prolog_flag(N,V)),set_prolog_flag(N,V))).
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

new_a2s(List, Separator, String):-catch(new_a2s0(List, Separator, String),_,((trace,new_a2s0(List, Separator, String)))).
new_a2s0(List, Separator, String):- debug,
 (atomic(String) -> (string_to_atom(String,Atom),concat_atom(List, Separator, Atom));
     (concat_atom(List, Separator, Atom),string_to_atom(String,Atom))).



:-export(bad_idea/0).
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

numbervars_impl(Term,Functor,Start,End):- numbervars(Term,Start,End,[attvar(bind),functor_name(Functor),singletons(true)]).

:-export(snumbervars/3).
snumbervars(Term,Functor,End):- atom(Functor),!,numbervars_impl(Term,Functor,0,End).
snumbervars(Term,Start,End):- integer(Start),!,numbervars_impl(Term,'$VAR',Start,End).
:-export(snumbervars/4).
snumbervars(Term,Start,End,_):-snumbervars(Term,Start,End).
:-export(snumbervars/1).
snumbervars(Term):-numbervars(Term,0,_End).

% ===================================================================
% Loop checking
% ===================================================================
:- thread_local ilc/1.
:- dynamic(ilc/1).

:- thread_local ilc_local/2.
:- dynamic(ilc_local/2).

make_key(CC,Key):- notrace(ground(CC)->Key=CC ; (copy_term(CC,Key,_),numbervars(Key,0,_))).

is_loop_checked(Call):-  make_key(Call,Key),!,ilc(Key).
is_module_loop_checked(Module, Call):- (var(Call)->true;make_key(Call,Key)),!,ilc_local(Module,Key).

no_loop_check_unsafe(Call):- with_no_assertions(ilc(_),Call).

no_loop_check(Call):- no_loop_check(Call,trace_or_throw(loop_to_no_loop_check(Call))).
no_loop_check(Call, TODO):-  with_no_assertions(ilc(_),loop_check_local(Call,TODO)).

no_loop_check_module( Module, Call, TODO):-  with_no_assertions(ilc_local(Module,_),loop_check_local(Call,TODO)).

loop_check(Call):- loop_check(Call,fail).
loop_check(Call, TODO):- make_key(Call,Key),!, loop_check_term(Call,Key,TODO).

loop_check_local(Call):- loop_check_local(Call,trace_or_throw(syntax_loop_check_local(Call))).
loop_check_local(Call,TODO):- loop_check_module(current,Call,TODO).

% loop_check_local(Call):- loop_check_local(Call,trace_or_throw(syntax_loop_check_local(Call))).
loop_check_module(Module,Call):-loop_check_module(Module,Call,fail).
loop_check_module(Module,Call,TODO):- make_key(Call,Key), LC = ilc_local(Module,Key),  ( \+(LC) -> (setup_call_cleanup(asserta(LC,REF),Call,erase_safe(asserta(LC,REF),REF))); call(TODO) ).

loop_check_term(Call,Key,TODO):- TT = ilc(Key),
   ( \+(TT) -> (setup_call_cleanup(asserta(TT,REF), Call, erase_safe(loop_check_term(Call,Key,TODO),REF))) ; call(TODO) ).

% ===================================================================
% Bugger Term Expansions
% ===================================================================

is_hiding_dmsgs:-bugger_flag(opt_debug,false),!.

% bugger_debug=false turns off just debugging about the debugger
% opt_debug=false turns off all the rest of debugging
% ddmsg(_):-current_prolog_flag(bugger_debug,false),!.
ddmsg(D):-format(user_error,'dmsg: ~q~n',[D]).
ddmsg_call(D):- ( (ddmsg(ddmsg_call(D)),call(D),ddmsg(ddmsg_exit(D))) *-> true ; ddmsg(ddmsg_failed(D))).


:-moo_show_childs(must(0)).

:-export(wdmsg/1).
wdmsg(X):- with_all_dmsg(dmsg(X)).

prolog_call(Call):-call(Call).
:-moo_show_childs(prolog_call(0)).

:-moo_hide_all(system:trace/0).
:-moo_hide_all(system:tracing/0).

% :-user: ( listing(notrace/1),redefine_system_predicate(system:notrace(_)), bugger:moo_hide_all(notrace(0)) ).

%:-'$set_predicate_attribute'(system:notrace(_), trace, 0).
%:-'$set_predicate_attribute'(system:notrace(_), hide_childs, 1).


:-export(remove_pred/3).
remove_pred(_,_,_):-!.
remove_pred(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred(M,F,A):- functor(P,F,A),
  (current_predicate(M:F/A) -> ignore((catch(redefine_system_predicate(M:P),_,true),abolish(M:F,A)));true),
  M:asserta((P:-bugger:wdmsg(error(P)),throw(permission_error(M:F/A)))).

:-export(fixnotrace/1).
fixnotrace(X):- tracing -> (call(X),trace) ; call(X).
:-moo_hide_all(fixnotrace(0)).

:-export(hidetrace/1).
hidetrace(X):- X.
:-moo_hide_all(hidetrace(0)).

:-export( /*tlbugger:*/ use_bugger_expansion/0).
:-dynamic( /*tlbugger:*/ use_bugger_expansion/0).
:- retractall( /*tlbugger:*/ use_bugger_expansion).
%:- asserta( /*tlbugger:*/ use_bugger_expansion).

functor_h0(P,F,A):-var(P),!,throw(functor_h_var(P,F,A)).
functor_h0(_:P,F,A):-nonvar(P),!,functor_h0(P,F,A).
functor_h0((P :- _B),F,A):-nonvar(P),!,functor_h0(P,F,A).
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
bugger_t_expansion(CM,notrace(T),fixnotrace(TT)):-!,bugger_t_expansion(CM,(T),(TT)).
bugger_t_expansion(_,F/A,F/A):-!.
bugger_t_expansion(_,M:F/A,M:F/A):-!.
bugger_t_expansion(CM,[F0|ARGS0],[F1|ARGS1]):- !,bugger_t_expansion(CM,F0,F1),bugger_t_expansion(CM,ARGS0,ARGS1).
% bugger_t_expansion(CM,T,AA):-  /*tlbugger:*/ use_bugger_expansion,compound_name_arguments(T,F,[A]),unwrap_for_debug(F),!,bugger_t_expansion(CM,A,AA),
%  ddmsg(bugger_term_expansion((T->AA))),!.
bugger_t_expansion(_,use_module(T),use_module(T)):-!.
bugger_t_expansion(_,module(A,B),module(A,B)):-!.
bugger_t_expansion(_,listing(A),listing(A)):-!.
bugger_t_expansion(CM,M:T,M:TT):-!,bugger_t_expansion(CM,T,TT),!.
bugger_t_expansion(_,test_is(A),test_is_safe(A)):-!.
bugger_t_expansion(_,delete(A,B,C),delete(A,B,C)):-!.
bugger_t_expansion(CM,T,TT):-  
     compound_name_arguments(T,F,A),notrace((bugger_t_expansion(CM,A,AA),
     functor_h0(T,FH,AH))),
    ( (fail,bugger_atom_change(CM,T,F,FH,AH,FF))-> true; FF=F ),
    compound_name_arguments(TT,FF,AA),!,
    ((true;T =@= TT)-> true;  ddmsg(bugger_term_expansion(CM,(T->TT)))),!.



:-module_transparent(p_predicate_property/2).
p_predicate_property(P,PP):-predicate_property(P,PP),!.
p_predicate_property(_:P,PP):-predicate_property(P,PP).
%current_bugger_predicate(M:FF/FA):-nonvar(FF),!,current_predicate(M:FF,FA).
%current_bugger_predicate(FF/FA):-nonvar(FF),!,!,current_predicate(FF/FA).
:-module_transparent(current_predicate_module/2).
current_predicate_module(OM:P,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(P,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).

:-meta_predicate(bugger_atom_change(:,0,+,+,-,-)).
bugger_atom_change(CM,T,F,FH,FA,FF):- /*tlbugger:*/ use_bugger_expansion, bugger_atom_change0(CM,T,F,FH,FA,FF).
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
bugger_goal_expansion(CM:T,TT):-  /*tlbugger:*/ use_bugger_expansion,!,bugger_goal_expansion(CM,T,TT).
:-meta_predicate(bugger_goal_expansion(+,+,-)).
bugger_goal_expansion(CM,T,T3):- once(bugger_t_expansion(CM,T,T2)),T\==T2,!,catch(expand_term(T2,T3),_,fail).

:-meta_predicate(bugger_expand_goal(0,-)).
bugger_expand_goal(T,_):- fail,ddmsg(bugger_expand_goal(T)),fail.

:-meta_predicate(bugger_expand_term(0,-)).
bugger_expand_term(T,_):- fail, ddmsg(bugger_expand_term(T)),fail.

:-export(format_safe/2).
format_safe(A,B):-catch(format(A,B),E,(dumpST,dtrace(E:format(A,B)))).

:-meta_predicate(bugger_term_expansion(:,-)).
bugger_term_expansion(CM:T,TT):- compound(T),  /*tlbugger:*/ use_bugger_expansion,!,bugger_term_expansion(CM,T,TT).
:-meta_predicate(bugger_term_expansion(+,+,-)).
bugger_term_expansion(CM,T,T3):- once(bugger_t_expansion(CM,T,T2)),T\==T2,!,nop(ddmsg(T\==T2)),catch(expand_term(T2,T3),_,fail).

% user:     expand_goal(G,G2):- compound(G),bugger_expand_goal(G,G2),!.


% user:goal_expansion(G,G2):- compound(G),bugger_goal_expansion(G,G2).

% user:expand_term(G,G2):- compound(G),bugger_expand_term(G,G2),!.


:- export(traceok/1).
:- multifile current_directory_search/1.
:- module_transparent current_directory_search/1.
:- meta_predicate(hotrace(0)).
:- meta_predicate(traceok(0)).

:- meta_predicate_transparent((loop_check_module(?,0))).
:- meta_predicate_transparent((loop_check_module(?,0,0))).
:- meta_predicate_transparent((no_loop_check_module(0,?,0))).

:- meta_predicate_transparent((loop_check_local(0,0))).
:- meta_predicate_transparent((no_loop_check(0,0))).
:- meta_predicate_transparent((no_loop_check(0))).
:- meta_predicate_transparent((no_loop_check_unsafe(0))).
:- meta_predicate_transparent((loop_check_term(0,?,0))).
:- meta_predicate_transparent((loop_check(0,0))).
:- meta_predicate_transparent((loop_check(0))).

:-export(mstatistics/0).
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

:-export(saved_current_atom/2).
:-dynamic(saved_current_atom/2).
:-export(new_atoms/2).
new_atoms(X,Type):-current_atom_or_blob(X,Type),not(saved_current_atom(X,Type)).
:-export(save_atoms/0).
save_atoms:-forall(new_atoms(X,Type),assert(saved_current_atom(X,Type))).
:-export(b_i/2).
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

:- export(((decl_thlocal)/1)).
:- meta_predicate(( decl_thlocal(:),  decl_thlocal(+,+,+,+))).
decl_thlocal(M:P):-!,with_pi(M:P,(decl_thlocal)).
decl_thlocal(P):-with_pi(thlocal:P,(decl_thlocal)).

decl_thlocal(CM,M,PI,F/A):-
   (var(PI)->functor_safe(PI,F,A);true),
   % (thread_local_leaks->dynamic(M:F/A);thread_local(M:F/A)),
   thread_local(M:F/A),
   % make_transparent(CM,M,PI,F/A),
   dynamic_multifile_exported(CM, M,PI,F/A).



:-export(parent_goal/2).
parent_goal(Term,Nth):- parent_frame_attribute(goal,Term,Nth,_RealNth,_FrameNum).
:-export(parent_frame_attribute/5).
parent_frame_attribute(Attrib,Term,Nth,RealNth,FrameNum):-notrace((ignore(Attrib=goal),prolog_current_frame(Frame),
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
nodebugx(X):- set_no_debug,notrace(X).

cnotrace:-notrace.

:- meta_predicate_transparent(cnotrace(0)).
:- module_transparent(cnotrace/1).
:- export(cnotrace/1).
cnotrace(C):-catchv(hotrace(C),E,((dmsg(E=C),rtrace(C),trace_or_throw(E=C)))).
:-'$syspreds':'$hide'(cnotrace/1).

trace_or_throw(E):- trace_or(throw(E)).

% :- use_module(library(prolog_stack)).

:-multifile term_to_message_string/2.
:-dynamic term_to_message_string/2.

:- dynamic isDebugging/1.

:-multifile was_module/2.
:-dynamic was_module/2.
:-module_transparent was_module/2.
:-multifile evil_term/3.
:-dynamic evil_term/3.

:- decl_thlocal(has_auto_trace/1).

%user:term_expansion(G,G2):- loop_check(bugger_term_expansion(G,G2)).
%user:goal_expansion(G,G2):- loop_check(bugger_goal_expansion(G,G2)).


:- meta_predicate_transparent(tlocal/2).
tlocal(M,ON):- forall(current_predicate(M:F/A), tlocal(M,F,A,ON)).

:- meta_predicate_transparent(tlocal/4).
tlocal(M,F,A,ON):- functor_safe(P,F,A), not(predicate_property(M:P,imported_from(_))), once((tlocal_0(M,P,ON,TF),must_det(tlocal_show(M,F,A,P,ON,TF)))).


:- meta_predicate_transparent(tlocal_0/4).
tlocal_0(M,P,ON,TF):- ccatch(tlocal_1(M,P,ON,TF),ERROR,(dmsg(ERROR),
       ((contains_var(error,ON);contains_var(all,ON)), 
         TF=' .???. '(M:P,ERROR)))),!.     

:- meta_predicate_transparent(tlocal_1/4).
tlocal_1(M,P,ON,TF):- '@'(M:call(M:P),M),!,
        (contains_var(on,ON),contains_var(true,ON);contains_var(all,ON)),!,
         TF=' .XXX. '(M:P),!.
tlocal_1(M,P,ON,TF):-  predicate_property(M:P,number_of_clauses(N)),N>0,!, 
        (contains_var(off,ON);contains_var(all,ON)),!,
         nth_clause(P, 1, REF),clause(Head, Body, REF),shrink_clause(Head,Body,FCL),
         TF=' .OFF. '(M:FCL),!.

:- meta_predicate_transparent(tlocal_2/4).
tlocal_2(M,P,ON,TF):- tlocal_1(M,P,ON,TF),!.
tlocal_2(M,P,ON,TF):-
        (contains_var(false,ON);contains_var(all,ON)),!,
         TF=' . - . '(M:P),!.

:- meta_predicate_transparent(shrink_clause/3).
shrink_clause(P,Body,Prop):- (Body==true-> Prop=P ; (Prop= (P:-Body))).

:-use_module(library(ansi_term)).

:-dynamic(user:mpred_prop/2).
:-multifile(user:mpred_prop/2).
:- meta_predicate_transparent(tlocal_show/6).
tlocal_show(M,F,A,P,_ON,TF):-
   copy_term(P,PL),
   must_det((predicate_property(M:P,number_of_clauses(_)) -> findall(Prop,(clause(M:PL,Body),shrink_clause(PL,Body,Prop)),Props1);Props1=[no_clause_Access])),
   findall(' ++'(Prop),call(user:mpred_prop,F,Prop),Props2),
   findall(' -'(yes(Prop)),(predicate_property(M:P,Prop),not(member(Prop,[number_of_rules(0),number_of_clauses(0),/*thread_local,*/volatile,dynamic,visible,interpreted]))),Props3),
   findall(' -'(not(Prop)),(member(Prop,[number_of_clauses(_),thread_local,volatile,dynamic,visible,exported,interpreted]),not(predicate_property(M:P,Prop))),Props4),   
   flatten([[Props1],[Props2],[Props3],[Props4],[TF/A]],PropsR),
   numbervars(PropsR,0,_,[singletons(true),attvars(skip)]),
   reverse(PropsR,Props),
   fmt(Props),!.


:- export(tlocals/0).
:- module_transparent(tlocals/0).
tlocals:- !.
tlocals:- 
   tlocals(false),
   tlocals(all).

tlocals(SHOW):- 
   doall((current_module(M),M\==thglobal,M\==thlocal,current_predicate(M:F/A),functor_safe(P,F,A),predicate_property(M:P,thread_local),tlocal(M,F,A,SHOW))),
   doall(tlocal(thglobal,SHOW)),
   doall(tlocal(thlocal,SHOW)),!.


:- meta_predicate_transparent meta_interp(:,+).

meta_interp_signal(meta_call(V)):-!,nonvar(V).
meta_interp_signal(meta_callable(_,_)).
meta_interp_signal(_:meta_call(V)):-!,nonvar(V).
meta_interp_signal(_:meta_callable(_,_)).

:-export(meta_interp/2).
meta_interp(CE,A):- notrace((var(A);not(stack_check))),!, throw(meta_interp(CE,A)).
meta_interp(_CE,A):- leash(+all),meta_interp_signal(A),!,fail.
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
set_bugger_flag(F,V):-create_prolog_flag(F,V,[keep(true),type(term)]),!.



:- meta_predicate_transparent cmust(0).
:- meta_predicate_transparent gmust(0,0).
:- meta_predicate_transparent must(0).
:- meta_predicate_transparent rmust_det(0).
:- meta_predicate_transparent must_det(0).
:- meta_predicate_transparent must_det_l(?).
:- meta_predicate_transparent must_each(0).
:- meta_predicate_transparent one_must(0,0).
:- meta_predicate_transparent one_must_det(0,0).
:- meta_predicate_transparent prolog_must(0).
:- meta_predicate_transparent prolog_must_l(?).
:- meta_predicate_transparent prolog_must_not(0).
:- meta_predicate_transparent slow_sanity(0).

:- meta_predicate_transparent logOnFailure0(0).
:- meta_predicate_transparent debugOnError0(0).
:- meta_predicate_transparent will_debug_else_throw(0,0).
:- meta_predicate_transparent printAll(0,*).
:- meta_predicate_transparent load_dirrective(0,*).
:- meta_predicate_transparent debugOnFailure0(0).
:- meta_predicate_transparent cli_notrace(0).
:- meta_predicate_transparent printPredCount(*,0,*).
:- meta_predicate_transparent ignoreOnError(0).
:- meta_predicate_transparent traceIf(0).
:- meta_predicate_transparent ifThen(0,0).
:- meta_predicate_transparent prolog_ecall_fa(*,1,*,*,0).
:- meta_predicate_transparent tryCatchIgnore(0).
:- meta_predicate_transparent logOnError0(0).
:- meta_predicate_transparent failOnError(0).
% :- meta_predicate_transparent test_call(0).

%:- meta_predicate_transparent debugCall(0).
:- meta_predicate_transparent prolog_ecall(*,1,?).
%:- meta_predicate_transparent traceafter_call(0).
:- meta_predicate_transparent if_prolog(*,0).
:- meta_predicate_transparent loop_check_clauses(0,?,0).
:- meta_predicate_transparent loop_check_clauses(0,0).
:- meta_predicate_transparent rtrace(0).
:- meta_predicate_transparent rtraceOnError(0).
:- meta_predicate_transparent debugOnError(0).
:- meta_predicate_transparent debugOnError0(0).
:- meta_predicate_transparent debugOnErrorIgnore(0).
:- meta_predicate_transparent debugOnFailure0(0).
:- meta_predicate_transparent forall_member(*,*,0).
:- meta_predicate_transparent throwOnFailure(0).
:- meta_predicate_transparent hotrace(0).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (0.:16 sec)

:- meta_predicate_transparent printAll(0).
:- meta_predicate_transparent showProfilerStatistics(0).
%:- meta_predicate_transparent debugCallF(0).

:- export((user_ensure_loaded/1)).
:- module_transparent user_ensure_loaded/1.
user_ensure_loaded(What):- !, within_module(ensure_loaded(What),'user').

:- module_transparent user_use_module/1.
% user_use_module(logicmoo(What)):- !, '@'(use_module(logicmoo(What)),'user').
% user_use_module(library(What)):- !, use_module(library(What)).
user_use_module(What):- within_module(use_module(What),'user').

:- within_module(use_module(logicmoo_util_library), 'user').



:- module_transparent(must_det_l/1).

:- module_transparent(loop_check/2).
:- module_transparent(no_loop_check/1).
:- export(no_loop_check/1).
%:- module_transparent(loop_check_fail/1).
%:- module_transparent(loop_check_throw/1).
%:- module_transparent(loop_check_term/3).
%:- meta_predicate_transparent((loop_check_throw(0))).
%:- meta_predicate_transparent((loop_check_fail(0))).


:-decl_thlocal  /*tlbugger:*/ can_table/0.
:-decl_thlocal  /*tlbugger:*/ cannot_table/0.
% thread locals should defaults to false:  /*tlbugger:*/ can_table.

cannot_table_call(Call):- with_assertions( /*tlbugger:*/ cannot_table,Call).

:-use_module(logicmoo_util_coroutining_was).


% ===================================================

:-meta_predicate_transparent(once_if_ground(0)).
once_if_ground(Call):-not(ground(Call)),!,Call.
once_if_ground(Call):- once(Call).

:-meta_predicate_transparent(once_if_ground(0,-)).
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

:-decl_thlocal  /*tlbugger:*/ attributedVars.

%  /*tlbugger:*/ attributedVars.

:-export(must_not_repeat/1).
:-meta_predicate(must_not_repeat(0)).
must_not_repeat(C):-call(C).

% ===================================================
% 
% no_repeats(:Call) 
%  (uses newval_or_fail/2)
%
% Like call/1 but ony succeeds only unique variabes
% 
% logicmoo_mud:  ?- no_repeats(member(X,[3,1,1,1,3,2])).
% X = 3 ;
% X = 1 ;
% X = 2.
% ===================================================


memberchk_same(X, [Y|Ys]) :- (   X =@= Y ->  (var(X) -> X==Y ; true) ;   memberchk_same(X, Ys) ).

:- export(no_repeats/1).
:- meta_predicate no_repeats(0).
% no_repeats(Call):-  /*tlbugger:*/ attributedVars,!,no_repeats_av(Call).

no_repeats(Call):- no_repeats(Call,Call).

% ===================================================
% 
% no_repeats(+Vars,:Call) 
%  (uses newval_or_fail/2)
% 
% Like call/1 but ony succeeds on unique free variabes
% 
% logicmoo_mud:  ?- no_repeats( X , member(X-Y,[3-2,1-4,1-5,2-1])).
% X = 3, Y = 2 ;
% X = 1, Y = 4 ;
% X = 2, Y = 1.
% ===================================================
:- export(no_repeats/2).
:- meta_predicate no_repeats(+,0).
% no_repeats(Vs,Call):-  /*tlbugger:*/ attributedVars,!,no_repeats_av(Vs,Call).

no_repeats(Vs,Call):- hotrace((ground(Vs) -> ((traceok(Call),!)) ;  no_repeats0(Vs,Call))).

:- export(no_repeats0/2).
:- meta_predicate no_repeats0(+,0).
no_repeats0(Vs,Call):- CONS = [_], traceok(Call), notrace(( \+ memberchk_same(Vs,CONS), copy_term(Vs,CVs), CONS=[_|T], nb_setarg(2, CONS, [CVs|T]))).

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


:-export(newval_or_fail/2).
:-meta_predicate(newval_or_fail(+,+)).
newval_or_fail(CONS,VAL):- CONS = [CAR|CDR], VAL \== CAR,  ( CDR==[] ->  nb_setarg(2, CONS, [VAL]) ; newval_or_fail(CDR,VAL)). 

% ==========================================================
%    is newval_or_fail/2  - a little term db to track if we've seen a term or not
% 
% written out for understanding  
% 
% newval_or_fail(cons(Vars,_),Vars):-!,fail.
% newval_or_fail(CONS,Vars):- CONS = '.'(_,[]), !,nb_setarg(2, CONS, '.'(Vars,[])).
% newval_or_fail(CONS,Vars):- CONS = '.'(Var,_),var(Var), !,nb_setarg(1, CONS, Vars).   % should we even bother to look here?
% newval_or_fail(cons(_,Nxt),Vars):- newval_or_fail_2(Nxt,Vars).
%
% (combined into one rule for immplementing)
% ==========================================================
:- export(newval_or_fail/2).
:- meta_predicate newval_or_fail(?,?).
% What may I do to simplify or speed up the below fo combine to the caller?
%  i skip checking the car each time
% what can be done to speed up?
%  perhapes starting ourt with an array? vv(_,_,_,_,_).
% and grow with a.. vv(_,_,_,_,vv(_,_,_,_,vv(_,_,_,_,_))) ? 




:- meta_predicate
        succeeds_n_times(0, -).

succeeds_n_times(Goal, Times) :-
        Counter = counter(0),
        (   Goal,
            arg(1, Counter, N0),
            N is N0 + 1,
            nb_setarg(1, Counter, N),
            fail
        ;   arg(1, Counter, Times)
        ).

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
%  (uses newval_or_fail/2)
%
% logicmoo_mud:  ?- no_repeats_av(member(X,[3,1,1,1,3,2])).
% X = 3 ;
% X = 1 ;
% X = 2.
%
% attributed variable verson of getting filtered bindings
% ===================================================
filter_repeats:-fail.

:-export(no_repeats_av/1).
:-meta_predicate(no_repeats_av(0)).
no_repeats_av(Call):-  term_variables(Call,VarList), flag(oddeven,X,X+1),
  ((VarList=[] ; 1 is X mod 3) -> Call ; 
   ((1 is X mod 2 ->  no_repeats_av_prox(VarList,Call); (CONS = [_],  call(Call), newval_or_fail(CONS,VarList))))).



:-export(no_repeats_av/2).
:-meta_predicate(no_repeats_av(+,0)).

no_repeats_av(Var,Call):- var(Var),!,no_repeats_avar(Var,Call).
no_repeats_av(VarList,Call):- no_repeats_avl(VarList,Call).


:-export(no_repeats_avl/2).
:-meta_predicate(no_repeats_avl(+,0)).
no_repeats_avl([],Call):-!,Call,!.
no_repeats_avl([Var],Call):- !,no_repeats_avar(Var,Call).
no_repeats_avl(VarList,Call):-no_repeats_av_prox(VarList,Call).

:-export(no_repeats_avar/2).
:-meta_predicate(no_repeats_avar(+,0)).
no_repeats_avar(VarList,Call):- filter_repeats, !, filter_repeats(VarList,Call).
no_repeats_avar(AVar,Call):- get_attr(AVar,was,VARWAS),!,work_with_attvar(AVar,Call,VARWAS,true).
no_repeats_avar(AVar,Call):-  /*tlbugger:*/ attributedVars,!, create_varwas(AVar,VARWAS), !,work_with_attvar(AVar,Call,VARWAS,del_attr(AVar,was)).
no_repeats_avar(Var,Call):- no_repeats_av_prox(Var,Call).

:-export(no_repeats_av_prox/2).
:-meta_predicate(no_repeats_av_prox(+,0)).
no_repeats_av_prox(VarList,Call):- filter_repeats,!, filter_repeats(VarList,Call).
no_repeats_av_prox(VarList,Call):- create_varwas(AVar,VARWAS), !,VarList=AVar, !,work_with_attvar(AVar,Call,VARWAS,del_attr(AVar,was)).

:-export(create_varwas/2).
:-meta_predicate(create_varwas(+,+)).
create_varwas(AVar,VARWAS):- was(AVar,_Comquatz),get_attr(AVar,was,VARWAS),!.

:-export(work_with_attvar/4).
:-meta_predicate(work_with_attvar(+,0,+,0)).
work_with_attvar(AVar,Call,VARWAS,ExitHook):- VARWAS = varwas(CONS,[]),  call_cleanup((  ( call(Call), newval_or_fail_attrib_varwas_cons(VARWAS, CONS, (was(-)-AVar)))),ExitHook).


:-export(newval_or_fail_attrib_varwas_cons/3).
:-meta_predicate(newval_or_fail_attrib_varwas_cons(+,+,+)).
newval_or_fail_attrib_varwas_cons(VARWAS,CONS,VAL):- CONS = [CAR|CDR], VAL \== CAR,  ( CDR==[] ->  ((arg(1, VARWAS, FIRSTCONS),nb_setarg(1, VARWAS, [VAL|FIRSTCONS]))) ; newval_or_fail_attrib_varwas_cons(VARWAS,CDR,VAL)). 



:-export(filter_repeats/2).
:-meta_predicate(filter_repeats(+,0)).
filter_repeats(AVar,Call):- 
      % 1 =  make globals storage compounds
      LVAL= lastResult(_),
      LATTR = lastWasHolderState([]),

      % 2a = create intial varwas 
      was(AVar,_),

      % 2b = refernce it
      get_attr(AVar,was,WASHOLDER),

      % 3 = WASHOLDER now looks like "varwas([_G190299-comquatz], [])"
      %  store it's arg1  into lastDifHolderState
      arg(1,WASHOLDER,SAVE), nb_setarg(1,LATTR,SAVE),
     
       % would a cut go here? 
      !,
      call_cleanup((( 
        
          % 4 = get the last saved attributes and set them from line 3 or 8
          arg(1,LATTR,LAST_WASHOLDER_STATE),  nb_setarg(1, WASHOLDER, LAST_WASHOLDER_STATE),
         
          % 5 = get the last result and add it to the was (saved from line 7)
          arg(1,LVAL,LR), was(AVar,LR),

          % 6 = call 
          call(Call), 

          % 7 = save our new val
          nb_setarg(1,LVAL,AVar),

          % 8 = save the new washolder state into lastWasHolderState
          arg(1,WASHOLDER, NEW_WASHOLDER_STATE), nb_setarg(1,LATTR, NEW_WASHOLDER_STATE)

          % 9 = REDO TO Line 4

          )),del_attr(AVar,was)).  % 10  = clean up for Line 2 needed?

% =========================================================================

:- decl_thlocal( /*tlbugger:*/ wastracing/0).
:- moo_hide_all( /*tlbugger:*/ wastracing/0).

% =========================================================================
% cli_notrace(+Call) is nondet.
% use call/1 with trace turned off
cli_notrace(X):- tracing -> with_assertions( /*tlbugger:*/ wastracing,call_cleanup((notrace,call(X)),trace)) ; call(X).
traceok(X):-  /*tlbugger:*/ wastracing -> call_cleanup((trace,call(X)),notrace) ; call(X).


% =========================================================================

:- decl_thlocal( /*tlbugger:*/ skip_bugger/0).

% false = use this wrapper, true = code is good and avoid using this wrapper
skipWrapper:- /*tlbugger:*/ skip_bugger.
skipWrapper:-tracing.
% false = hide this wrapper
showHiddens:-true.

:-moo_hide_all( /*tlbugger:*/ skip_bugger).
:-moo_hide_all(skipWrapper).



:- set_prolog_flag(backtrace_depth,   200).
:- set_prolog_flag(backtrace_goal_depth, 20).
:- set_prolog_flag(backtrace_show_lines, true).
% =========================================================================


% ==========================================================
% can/will Tracer.
% ==========================================================

:-decl_thlocal(ifCanTrace/0).
% thread locals should defaults to false: ifCanTrace.
ifCanTrace.


:-export(ifWontTrace/0).
:-decl_thlocal(ifWontTrace/0).

:-meta_predicate(set_no_debug()).
:-export(set_no_debug/0).
set_no_debug:- 
  must_det_l([
   set_prolog_flag(generate_debug_info, false),
   retractall(ifCanTrace),
   retractall(ifWontTrace),
   asserta(ifWontTrace),   
   set_prolog_flag(report_error,false),   
   set_prolog_flag(debug_on_error,false),
   set_prolog_flag(debug, false),   
   set_prolog_flag(query_debug_settings, debug(false, false)),
   set_gui_debug(fail),
   leash(-all),
   leash(+exception),
   visible(-cut_call),!,
   notrace, nodebug]),!.

set_gui_debug(TF):-ignore((catch(guitracer,_,true))),
   ((TF,has_gui_debug)-> set_prolog_flag(gui_tracer, true) ; set_prolog_flag(gui_tracer, false)).


:-meta_predicate(set_yes_debug()).
:-export(set_yes_debug/0).
set_yes_debug:- 
  must_det_l([
   set_prolog_flag(generate_debug_info, true),
   (ifCanTrace->true;assert(ifCanTrace)),
   retractall(ifWontTrace),   
   (ifWontTrace->true;assert(ifWontTrace)),   
   set_prolog_flag(report_error,true),   
   set_prolog_flag(debug_on_error,true),
   set_prolog_flag(debug, true),   
   set_prolog_flag(query_debug_settings, debug(true, true)),
   set_gui_debug(true),
   leash(+all),
   leash(+exception),
   visible(+cut_call),!,
   notrace, debug]),!.


:- assert_if_new( /*tlbugger:*/ use_bugger_expansion).

% :- set_yes_debug.


isConsole :- telling(user).
isConsole :- current_output(X),!,stream_property(X,alias(user_output)).


willTrace:-ifWontTrace,!,fail.
willTrace:-not(isConsole),!,fail.
willTrace:-ifCanTrace.

hideTrace:-
  hideTrace([hotrace/1], -all),
  %%hideTrace(computeInnerEach/4, -all),

  hideTrace(
   [maplist_safe/2,
       maplist_safe/3], -all),


  hideTrace([hideTrace/0,
     ifCanTrace/0,
     ctrace/0,
     willTrace/0], -all),

  hideTrace([
     traceafter_call/1,

     notrace_call/1], -all),

  hideTrace(user:[
   call/1,
   call/2,
   apply/2,
   '$bags':findall/3,
   '$bags':findall/4,
   once/1,
   ','/2,
   catchv/3,
   catch/3,
   catchv/3,
   member/2], -all),

  hideTrace(user:setup_call_catcher_cleanup/4,-all),

  hideTrace(system:throw/1, +all),
  %%hideTrace(system:print_message/2, +all),
  hideTrace(user:message_hook/3 , +all),
  hideTrace(system:message_to_string/2, +all),
  !,hideRest,!.
  %%findall(File-F/A,(functor_source_file(M,P,F,A,File),M==user),List),sort(List,Sort),dmsg(Sort),!.

hideRest:- fail, logicmoo_util_library:buggerDir(BuggerDir),
   functor_source_file(M,_P,F,A,File),atom_concat(BuggerDir,_,File),hideTraceMFA(M,F,A,-all),
   fail.
hideRest:- functor_source_file(system,_P,F,A,_File),hideTraceMFA(system,F,A,-all), fail.
hideRest.

:- meta_predicate_transparent(hideTrace(:,-)).
:- meta_predicate_transparent with_output_to_stream(?,0).

functor_source_file(M,P,F,A,File):-functor_source_file0(M,P,F,A,File). % must(ground((M,F,A,File))),must(user:nonvar(P)).
functor_source_file0(M,P,F,A,File):-current_predicate(F/A),functor_safe(P,F,A),source_file(P,File),predicate_module(P,M).

predicate_module(P,M):- predicate_property(P,imported_from(M)),!.
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

tryCatchIgnore(MFA):- catchv(MFA,_E,true). %%dmsg(tryCatchIgnoreError(MFA:E))),!.
tryCatchIgnore(_MFA):- !. %%dmsg(tryCatchIgnoreFailed(MFA)).

tryHide(_MFA):-showHiddens,!.
tryHide(MFA):- tryCatchIgnore('$hide'(MFA)).

hideTraceMFA(_,M:F,A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMFA(M,F,A,T):-user:nonvar(A),functor_safe(P,F,A),predicate_property(P,imported_from(IM)),IM \== M,!,nop(dmsg(doHideTrace(IM,F,A,T))),hideTraceMFA(IM,F,A,T),!.
hideTraceMFA(M,F,A,T):-hideTraceMFAT(M,F,A,T),!.

hideTraceMFAT(M,F,A,T):-doHideTrace(M,F,A,T),!.

doHideTrace(_M,_F,_A,[]):-!.
doHideTrace(M,F,A,[hide|T]):- tryHide(M:F/A),!,doHideTrace(M,F,A,T),!.
doHideTrace(M,F,A,ATTRIB):- tryHide(M:F/A),!,
  tryCatchIgnore(trace(M:F/A,ATTRIB)),!.


ctrace:-willTrace->trace;notrace.

buggeroo:-hideTrace,traceAll,atom_concat(guit,racer,TRACER), catchv(call(TRACER),_,true),debug,list_undefined.

singletons(_).

:-set_prolog_flag(debugger_show_context,true).
% :-set_prolog_flag(trace_gc,true).
:-set_prolog_flag(debug,true).
:-set_prolog_flag(gc,true).
set_mem_opt(TF):- set_prolog_flag(gc,TF),set_prolog_flag(last_call_optimisation,TF),set_prolog_flag(optimise,TF).

do_gc:- statistics,do_gc0,do_gc0,statistics.
do_gc0:- current_prolog_flag(gc,GCWAS),set_prolog_flag(gc,true), garbage_collect, garbage_collect_atoms, set_prolog_flag(gc,GCWAS).


failOnError(Call):-catchv(Call,_,fail).

fresh_line:-current_output(Strm),fresh_line(Strm),!.
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
must_assign(From,To):- /*tlbugger:*/ skipMust,!,ignore(To=From),!.
must_assign(From,To):-dmsg(From),dmsg(=),dmsg(From),dmsg(must_assign),!,trace,To=From.


prolog_must(Call):-must(Call).


% cmust is only used for type checking
cmust(_):-bugger_flag(release,true),!.
cmust(Call):-one_must(Call,will_debug_else_throw(cmust(Call),Call)).

% gmust is must with cmust
gmust(True,Call):-catchv((Call,(True->true;throw(retry(gmust(True,Call))))),retry(gmust(True,_)),(trace,Call,True)).

% must is used declaring the predicate must suceeed

throwOnFailure(Call):-one_must(Call,throw(throwOnFailure(Call))).
ignoreOnError(CX):-ignore(catchv(CX,_,true)).

% pause_trace(_):- cnotrace(((debug,visible(+all),leash(+exception),leash(+call)))),trace.

%debugCall(C):-cnotrace,dmsg(debugCall(C)),dumpST, pause_trace(errored(C)),ggtrace,C.
%debugCallF(C):-cnotrace,dmsg(debugCallF(C)),dumpST, pause_trace(failed(C)),gftrace,C.

debugCallWhy(Why, C):- notrace((notrace,dmsg(Why))),dtrace(C).

:-export(rtraceOnError/1).
rtraceOnError(C):-
  catchv(
  with_skip_bugger( C ),E,(dmsg(rtraceOnError(E=C)),dtrace,leash(+call),trace,leash(+exception),leash(+all),rtrace(with_skip_bugger( C )),dmsg(E=C),leash(+call),dtrace)).


with_skip_bugger(C):-setup_call_cleanup(asserta( /*tlbugger:*/ skip_bugger),C,retract( /*tlbugger:*/ skip_bugger)).

debugOnError(C):- skipWrapper,!,C.
debugOnError(C):- !,debugOnError0(C).
debugOnError(C):-prolog_ecall(0,debugOnError0,C).
debugOnError0(C):- skipWrapper,!,C.
debugOnError0(C):- catchv(C,E,call_cleanup(debugCallWhy(thrown(E),C),throw(E))).
debugOnErrorEach(C):-prolog_ecall(1,debugOnError0,C).
debugOnErrorIgnore(C):-ignore(debugOnError0(C)).

debugOnFailure(C):-prolog_ecall(0,debugOnFailure0,C).
debugOnFailure0(C):- one_must(rtraceOnError(C),debugCallWhy(failed(debugOnFailure0(C)),C)).
debugOnFailureEach(C):-prolog_ecall(1,debugOnFailure,C).
debugOnFailureIgnore(C):-ignore(debugOnFailure(C)).

logOnError(C):-prolog_ecall(0,logOnError0,C).
logOnError0(C):- catchv(C,E,dmsg(logOnError(E,C))).
logOnErrorEach(C):-prolog_ecall(1,logOnError,C).
logOnErrorIgnore(C):-ignore(logOnError(C)).

logOnFailure0(C):- one_must(C,dmsg(logOnFailure(C))).
logOnFailureEach(C):-prolog_ecall(1,logOnFailure,C).
logOnFailureIgnore(C):-ignore(logOnFailure(C)).


%debugOnFailure0(X):-ctrace,X.
%debugOnFailure0(X):-catchv(X,E,(writeFailureLog(E,X),throw(E))).
%throwOnFailure/1 is like Java/C's assert/1
%debugOnFailure1(Module,CALL):-trace,debugOnFailure(Module:CALL),!.
%debugOnFailure1(arg_domains,CALL):-!,logOnFailure(CALL),!.

beenCaught(must(Call)):- !, beenCaught(Call).
beenCaught((A,B)):- !,beenCaught(A),beenCaught(B).
beenCaught(Call):- fail, predicate_property(Call,number_of_clauses(_Count)), clause(Call,(_A,_B)),!,clause(Call,Body),beenCaught(Body).
beenCaught(Call):- catchv(once(Call),E,(dmsg(caugth(Call,E)),beenCaught(Call))),!.
beenCaught(Call):- traceAll,dmsg(tracing(Call)),debug,trace,Call.

:-meta_predicate_transparent(with_no_term_expansions(0)).
with_no_term_expansions(Call):-
  with_no_assertions(user:term_expansion(_,_),
    with_no_assertions(user:goal_expansion(_,_),Call)).

kill_term_expansion:-
   abolish(user:term_expansion,2),
   abolish(user:goal_expansion,2),
   dynamic(user:term_expansion/2),
   dynamic(user:goal_expansion/2),
   multifile(user:term_expansion/2),
   multifile(user:goal_expansion/2).

local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-predicate_property(P,built_in),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
%local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains666(F,'aiml_'),!.
local_predicate(P,F/N):-functor_safe(P,F,N),!,fail.

%atom_contains666(F,C):- hotrace((atom(F),atom(C),sub_atom(F,_,_,_,C))).



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
% rmust_det(C)-  catchv((C *-> true ; debugCallWhy(failed(must(C)),C)),E,debugCallWhy(thrown(E),C)).

must_each(List):-var(List),trace_or_throw(var_must_each(List)).
must_each([List]):-!,must(List).
must_each([E|List]):-!,must(E),must_each0(List).
must_each0(List):-var(List),trace_or_throw(var_must_each(List)).
must_each0([]):-!.
must_each0([E|List]):-E,must_each0(List).

:-moo_show_childs(one_must/2).
one_must(C1,C2,C3):-one_must(C1,one_must(C2,C3)).
one_must(Call,OnFail):- ( Call *->  true ;    OnFail ).

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

must_det_l([]):-!.
must_det_l([C|List]):-!,rmust_det(C),!,must_det_l(List).
must_det_l((C,List)):-!,rmust_det(C),!,must_det_l(List).
must_det_l(C):- !,must_det(C).

:-decl_thlocal  /*tlbugger:*/ skip_use_slow_sanity/0.
% thread locals should defaults to false  /*tlbugger:*/ skip_use_slow_sanity.

slow_sanity(C):-  must(C),!. %  ( /*tlbugger:*/ skip_use_slow_sanity ; must_det(C)),!.
must_det(C):- must(C),!.

one_must_det(Call,_OnFail):-Call,!.
one_must_det(_Call,OnFail):-OnFail,!.


randomVars(Term):- random(R), StartR is round('*'(R,1000000)), !,
 ignore(Start=StartR),
 snumbervars(Term, Start, _).

prolog_must_not(Call):-Call,!,trace,!,programmer_error(prolog_must_not(Call)).
prolog_must_not(_Call):-!.



dynamic_if_missing(F/A):-functor_safe(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):-dynamic([F/A]).



%%%retractall(E):- retractall(E),functor_safe(E,File,A),dynamic(File/A),!.

pp_listing(Pred):- functor_safe(Pred,File,A),functor_safe(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

:- meta_predicate_transparent(with_assertions(+,0)).
with_assertions( [],Call):- !,Call.
with_assertions( [With|MORE],Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With,MORE),Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With;MORE),Call):- !,with_assertions(With,Call);with_assertions(MORE,Call).
with_assertions( -TL:With,Call):- !,with_no_assertions(TL:With,Call).
with_assertions( +TL:With,Call):- !,with_assertions(TL:With,Call).
with_assertions( not(With),Call):- !,with_no_assertions(With,Call).
with_assertions( -With,Call):- !,with_no_assertions(With,Call).
with_assertions( +With,Call):- !,with_assertions(With,Call).

with_assertions(op(N,XFY,OP),Call):-!,
     (current_op(PN,XFY,OP);PN=0),!,
     (PN==N -> Call ; setup_call_cleanup(op(N,XFY,OP),Call,op(PN,XFY,OP))).

with_assertions(set_prolog_flag(N,XFY),Call):-!,
     (current_prolog_flag(N,WAS);WAS=unUSED),!,     
     (XFY==WAS -> Call ; 
     (
       setup_call_cleanup(set_prolog_flag(N,XFY),Call,(WAS=unUSED->true;set_prolog_flag(N,WAS))))).

with_assertions(before_after(Before,After),Call):-!,
     (Before -> setup_call_cleanup(true,Call,After);Call).

% with_assertions(THead,Call):- functor_safe(THead,F,_),b_setval(F,THead).

/*
with_assertions(THead,Call):- ground(THead),!,
 must_det(to_thread_head(THead,M,_Head,WithA)),
   ( M:WithA -> Call ; setup_call_cleanup(M:asserta(WithA),Call,must_det(M:retract(WithA)))).
*/

with_assertions(THead,Call):- to_thread_head(THead,M,_Head,HAssert), !, 
     setup_call_cleanup(asserta(M:HAssert,REF),Call,erase_safe(asserta(M:HAssert,REF),REF)).

/*

with_assertions(THead,Call):- !,
 must_det(to_thread_head(THead,M,_Head,H)),
   copy_term(H,  WithA), !,
   setup_call_cleanup(M:asserta(WithA),Call,must_det(M:retract(WithA))).


with_assertions(THead,Call):- 
 must_det(to_thread_head(THead,M,_Head,H)),
   copy_term(H,  WithA), !,
   with_assertions(M,WithA,Call).
   
:- meta_predicate_transparent(with_assertions(+,+,0)).
with_assertions(M,WithA,Call):- M:WithA,!,Call.
with_assertions(M,WithA,Call):-
   setup_call_cleanup(M:asserta(WithA),Call,must_det(M:retract(WithA))).

*/

:-meta_predicate_transparent(with_no_assertions(+,0)).
with_no_assertions(THead,Call):-
 must_det(to_thread_head((THead:- (!,fail)),M,_HEAD,H)),
   copy_term(H, WithA), !,
    setup_call_cleanup(M:asserta(WithA,REF),Call,erase_safe(M:asserta(WithA,REF),REF)).

/*
old version
:-meta_predicate_transparent(with_no_assertions(+,0)).
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
to_thread_head(Head,tlbugger, /*tlbugger:*/ Head,Head):- check_thread_local( /*tlbugger:*/ Head).

check_thread_local(thlocal:_):-!.
check_thread_local(_):-!.
check_thread_local(user:_):-!.
check_thread_local(TL:Head):-slow_sanity(( predicate_property(TL:Head,(dynamic)),must_det(predicate_property(TL:Head,(thread_local))))).

:-decl_thlocal( /*tlbugger:*/  /*tlbugger:*/ dmsg_match/2).
:-meta_predicate_transparent(with_all_dmsg(0)).
:-meta_predicate_transparent(with_all_dmsg(0)).

with_all_dmsg(Call):-
     with_assertions(set_prolog_flag(opt_debug,true),
       with_no_assertions( /*tlbugger:*/ dmsg_match(hidden,_),Call)).
with_show_dmsg(TypeShown,Call):-
  with_assertions(set_prolog_flag(opt_debug,filter),
     with_assertions( /*tlbugger:*/ dmsg_match(showing,TypeShown),Call)).

:-meta_predicate_transparent(with_no_dmsg(0)).
with_no_dmsg(Call):-with_assertions(set_prolog_flag(opt_debug,false),Call).
with_no_dmsg(TypeUnShown,Call):-with_assertions(set_prolog_flag(opt_debug,filter),
  with_assertions( /*tlbugger:*/ dmsg_match(hidden,TypeUnShown),Call)).

dmsg_hides_message(_):- current_prolog_flag(opt_debug,false),!.
dmsg_hides_message(_):- current_prolog_flag(opt_debug,true),!,fail.
dmsg_hides_message(C):-  /*tlbugger:*/ dmsg_match(HideShow,Matcher),matches_term(Matcher,C),!,HideShow=hidden.

dmsg_hide(Term):-must(nonvar(Term)),asserta_new( /*tlbugger:*/ dmsg_match(hidden,Term)),retractall( /*tlbugger:*/ dmsg_match(showing,Term)),nodebug(Term).
dmsg_show(Term):-asserta_new( /*tlbugger:*/ dmsg_match(showing,Term)),ignore(retractall( /*tlbugger:*/ dmsg_match(hidden,Term))),debug(Term).
dmsg_showall(Term):-ignore(retractall( /*tlbugger:*/ dmsg_match(hidden,Term))).

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
   catchv((read_term(In,Term,[syntax_errors(error)|Options])),E,(dmsg(E),fail)),
   load_term(Term,[line_count(Lineno),file(PLNAME),stream(In)|Options]),
   Term==end_of_file,
   close(In).

load_term(E,_Options):- E == end_of_file, !.
load_term(Term,Options):-catchv(load_term2(Term,Options),E,(dmsg(error(load_term(Term,Options,E))),throw_safe(E))).

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


:- meta_predicate_transparent bugger:unify_listing(0).
unify_listing(FileMatch):-functor_safe(FileMatch,F,A),unify_listing(FileMatch,F,A),!.
unify_listing_header(FileMatch):-functor_safe(FileMatch,F,A),unify_listing_header(FileMatch,F,A),!.


:- meta_predicate_transparent bugger:unify_listing(0,*,*).
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
indent_e(X):- catchv((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).

% ===================================================================
% Lowlevel printng
% ===================================================================


fmt0(X,Y,Z):-catchv((format(X,Y,Z),flush_output_safe(X)),E,dmsg(E)).
fmt0(X,Y):-catchv((format(X,Y),flush_output_safe),E,dmsg(E)).
fmt0(X):-catchv(text_to_string(X,S),_,fail),'format'('~w',[S]),!.
fmt0(X):- (atom(X) -> catchv((format(X,[]),flush_output_safe),E,dmsg(E)) ; (term_to_message_string(X,M) -> 'format'('~q~n',[M]);fmt_or_pp(X))).
fmt(X):-fresh_line,fmt_ansi(fmt0(X)).
fmt(X,Y):- fresh_line,fmt_ansi(fmt0(X,Y)),!.
fmt(X,Y,Z):- fmt_ansi(fmt0(X,Y,Z)),!.

% :-reexport(library(ansi_term)).

tst_fmt:- make,
 findall(R,(clause(ansi_term:sgr_code(R, _),_),ground(R)),List),
 doall((
        ansi_term:ansi_color(FC, _),
        member(FG,[hfg(FC),fg(FC)]),
        % ansi_term:ansi_term:ansi_color(Key, _),
        member(BG,[hbg(default),bg(default)]),
        member(R,List),
        % random_member(R1,List),
    C=[reset,R,FG,BG],
  fresh_line,
  ansi_term:ansi_format(C,' ~q ~n',[C]))).


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

:-meta_predicate_transparent(time_call(:)).
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

:- decl_thlocal is_with_dmsg/1.

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

dmsginfo(V):-dmsg(info(V)).
dmsg(V):- notrace(once(dmsg0(V))).
:-'$syspreds':'$hide'(dmsg/1).

dmsg0(V):- is_with_dmsg(FP),!,FP=..FPL,append(FPL,[V],VVL),VV=..VVL,once(dmsg0(VV)).
%DMILES 
dmsg0(_):- is_hiding_dmsgs,!.
dmsg0(V):- var(V),!,dmsg0(dmsg_var(V)).
dmsg0(V):- (doall((once(dmsg1(V)),hook:dmsg_hook(V)))),!.
/*
dmsg1(trace_or_throw(V)):- dumpST(15),print_message(warning,V),fail.
dmsg1(error(V)):- print_message(warning,V),fail.
dmsg1(warn(V)):- dumpST(6),print_message(warning,V),fail.
*/
dmsg1(skip_dmsg(_)):-!.
%DMILES  
dmsg1(C):-dmsg_hides_message(C),!.
dmsg1(ansi(Ctrl,Msg)):- ansicall(Ctrl,dmsg1(Msg)).
dmsg1(C):-functor_safe(C,Topic,_),debugging(Topic,_True_or_False),!,logger_property(Topic,once,true),!,
      (dmsg_log(Topic,_Time,C) -> true ; ((get_time(Time),asserta(dmsg_log(todo,Time,C)),!,dmsg2(C)))).

dmsg1(ansi(Ctrl,Msg)):- !, ansicall(Ctrl,dmsg2(Msg)).
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
dmsg(C,T):-!, (( fmt('<font size=+1 color=~w>',[C]), fmt(T), fmt('</font>',[]))),!.

dmsg(L,F,A):-loggerReFmt(L,LR),loggerFmtReal(LR,F,A).

%dmsg(C,T):- is_hiding_dmsgs,!.
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

:-export(ansifmt/2).
ansifmt(Ctrl,Fmt):- colormsg(Ctrl,Fmt).
:-export(ansifmt/3).
ansifmt(Ctrl,F,A):- colormsg(Ctrl,(format(F,A))).


:-export(colormsg/2).
colormsg(d,Msg):- mesg_color(Msg,Ctrl),!,colormsg(Ctrl,Msg).
colormsg(Ctrl,Msg):- fresh_line,ansicall(Ctrl,fmt0(Msg)),fresh_line.

:-export(ansicall/2).
ansicall(Ctrl,Call):- current_output(Out), ansicall(Out,Ctrl,Call).

ansi_control_conv([],[]):-!.
ansi_control_conv([H|T],HT):-!,ansi_control_conv(H,HH),!,ansi_control_conv(T,TT),!,flatten([HH,TT],HT),!.
ansi_control_conv(warn,Ctrl):- !, ansi_control_conv(warning,Ctrl),!.
ansi_control_conv(Level,Ctrl):- ansi_term:level_attrs(Level,Ansi),Level\=Ansi,!,ansi_control_conv(Ansi,Ctrl).
ansi_control_conv(Color,Ctrl):- ansi_term:ansi_color(Color,_),!,ansi_control_conv(fg(Color),Ctrl).
ansi_control_conv(Ctrl,CtrlO):-flatten([Ctrl],CtrlO),!.

is_tty(Out):- is_stream(Out),stream_property(Out,tty(true)).

ansicall(Out,_,Call):- \+ is_tty(Out),!,Call.
ansicall(Out,CtrlIn,Call):- once(ansi_control_conv(CtrlIn,Ctrl)),  CtrlIn\=Ctrl,!,ansicall(Out,Ctrl,Call).
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

mesg_color(T,C):-var(T),!,dumpST,!,C=[blink(slow),fg(red),hbg(black)],!.
mesg_color(T,C):-string(T),!,must(f_word(T,F)),!,functor_color(F,C).
mesg_color(T,C):-not(compound(T)),text_to_string(T,S),!,mesg_color(S,C).
mesg_color(_:T,C):-nonvar(T),!,mesg_color(T,C).
mesg_color(T,C):-functor_safe(T,F,_),member(F,[color,ansi]),compound(T),arg(1,T,C),nonvar(C).
mesg_color(T,C):-predef_functor_color(F,C),mesg_arg1(T,F).
mesg_color(T,C):-nonvar(T),defined_message_color(F,C),matches_term(F,T),!.
mesg_color(T,C):-functor_h0(T,F,_),!,functor_color(F,C),!.

f_word(T,A):-concat_atom([A|_],' ',T),!.
f_word(T,A):-concat_atom([A|_],'_',T),!.
f_word(T,A):-string_to_atom(T,A),!.

mesg_arg1(T,_TT):-var(T),!,fail.
mesg_arg1(_:T,C):-nonvar(T),!,mesg_arg1(T,C).
mesg_arg1(T,TT):-not(compound(T)),!,T=TT.
mesg_arg1(T,F):-functor_h0(T,F,_).
mesg_arg1(T,C):-compound(T),arg(1,T,F),!,nonvar(F),mesg_arg1(F,C).


:-export(defined_message_color/2).
:-dynamic(defined_message_color/2).

defined_message_color(todo,[fg(red),bg(black),underline]).
defined_message_color(error,[fg(red),hbg(black),bold]).
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

:-export(random_color/1).
random_color([reset,M,FG,BG,font(Font)]):-Font is random(8),
  findall(Cr,ansi_term:ansi_color(Cr, _),L),
  random_member(E,L),
  random_member(FG,[hfg(E),fg(E)]),not(unliked_ctrl(FG)),
  contrasting_color(FG,BG), not(unliked_ctrl(BG)),
  random_member(M,[bold,faint,reset,bold,faint,reset,bold,faint,reset]),!. % underline,negative


:-export(tst_color/0).
tst_color:- make, doall(( between(1,20,_),random_member(Call,[colormsg(C,cm(C)),dmsg(color(C,dm(C))),ansifmt(C,C)]),next_color(C),Call)).
:-export(tst_color/1).
tst_color(C):- make,colormsg(C,C).

:-export(next_color/1).
next_color(C):- between(1,10,_), random_color(C), good_next_color(C),!.
next_color([underline|C]):- random_color(C),!.

:-export(contrasting_color/2).
contrasting_color(white,black).
contrasting_color(A,default):-atom(A),A \= black.
contrasting_color(fg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(hfg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(black,white).
contrasting_color(default,default).
contrasting_color(_,default).

:-decl_thlocal(ansi_prop/2).

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


% ansicall(Ctrl,Msg):- msg_to_string(Msg,S),fresh_line,catchv(ansifmt(Ctrl,'~w~n',[S]),_,catchv(ansifmt(fg(Ctrl),'~w',[S]),_,'format'('~q (~w)~n',[ansicall(Ctrl,Msg),S]))),fresh_line.
/*
dmsg(Color,Term):- current_prolog_flag(tty_control, true),!,  tell(user),fresh_line,to_petty_color(Color,Type),
   call_cleanup(((sformat(Str,Term,[],[]),print_message(Type,if_tty([Str-[]])))),told).
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


:- meta_predicate_transparent show_call0(0).
show_call0(C):-C. % debugOnError0(C). % dmsg(show_call(C)),C.      

:- meta_predicate_transparent show_call(0).
show_call(M:add(A)):-!, show_call0(M:add(A)),!.
% show_call(M:must(C)):- !, M:must(C).
show_call(C):-one_must((show_call0(C),dmsg(succeed(C))),((dmsg(failed_show_call(C)),!,fail))).

:- meta_predicate_transparent show_call_failure(0).
show_call_failure(C):-one_must((show_call0(C)),((dmsg(failed_show_call(C)),!,fail))).

:- meta_predicate_transparent logOnFailure(0).
:- export(logOnFailure/1).
logOnFailure(C):-one_must(C,(dmsg(failed_show_call(C)),!,fail)).


:-dynamic(user:logLevel/2).
:-module_transparent(user:logLevel/2).
:-multifile(user:user:logLevel/2).

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


 

:-moo_hide_childs(stack_depth/1).
:-moo_hide_childs(stack_check/0).
:-moo_hide_childs(stack_check/1).
:-moo_hide_childs(stack_check/2).
stack_depth(Level):-notrace((prolog_current_frame(Frame),prolog_frame_attribute(Frame,level,Level))).

stack_check:-!.
stack_check:-stack_check(3000).
stack_check(BreakIfOver):- stack_check_else(BreakIfOver, trace_or_throw(stack_check(BreakIfOver))).
stack_check(BreakIfOver,Error):- stack_check_else(BreakIfOver, trace_or_throw(stack_check(BreakIfOver,Error))).
stack_check_else(BreakIfOver,Call):- stack_depth(Level) ,  ( Level < BreakIfOver -> true ; (dbgsubst(Call,stack_lvl,Level,NewCall),NewCall)).

% dumpstack_arguments.
dumpST:-prolog_call(notrace(dumpST(5000))).

% dumpST(_):-is_hiding_dmsgs,!.
dumpST(Opts):- prolog_current_frame(Frame),dumpST(Frame,Opts).

dumpST(Frame,MaxDepth):-integer(MaxDepth),!,dumpST(Frame,[max_depth(MaxDepth),numbervars(safe),show([level,goal,clause])]).
dumpST(Frame,Opts):-var(Opts),!,dumpST(Frame,5000).
dumpST(Frame,Opts):-is_list(Opts),!,dumpST(1,Frame,Opts).
dumpST(Frame,Opts):-show_call(dumpST(1,Frame,[Opts])).

get_m_opt(Opts,Max_depth,D100,RetVal):-E=..[Max_depth,V],(((member(E,Opts),nonvar(V)))->RetVal=V;RetVal=D100).

dumpST(N,Frame,Opts):-
  must(( dumpST(N,Frame,Opts,Out))),
   must((get_m_opt(Opts,numbervars,-1,Start),
   neg1_numbervars(Out,Start,ROut),
   reverse(ROut,RROut),
   ignore((forall(member(E,RROut),fdmsg(E)))))).

neg1_numbervars(T,-1,T):-!.
neg1_numbervars(Out,Start,ROut):-copy_term(Out,ROut),integer(Start),!,safe_numbervars(ROut,Start,_).
neg1_numbervars(Out,safe,ROut):-copy_term(Out,ROut),safe_numbervars(ROut).

fdmsg1(txt(S)):-'format'(S,[]),!.
fdmsg1(level=L):-'format'('(~q)',[L]),!.
fdmsg1(goal=G):-'format'(' ~q. ',[G]),!.
fdmsg1(clause=[F,L]):- directory_file_path(_,FF,F),'format'('  %  ~w:~w: ',[FF,L]),!.
fdmsg1(clause=[F,L]):- fresh_line,'format'('%  ~w:~w: ',[F,L]),!.
fdmsg1(clause=[]):-'format'(' /*DYN*/ ',[]),!.
fdmsg1(E):- 'format'(' ~q ',[E]).

fdmsg(fr(List)):-is_list(List),!,fresh_line,ignore(forall(member(E,List),fdmsg1(E))),nl.
fdmsg(M):-dmsg(M).

dumpST(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,max_depth,100,MD),N>=MD,!.
dumpST(N,Frame,Opts,[fr(Goal)|MORE]):- get_m_opt(Opts,show,goal,Ctrl),getPFA(Frame,Ctrl,Goal),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST(N,Frame,Opts,[nf(no(Ctrl),N,Frame,Opts)|MORE]):- get_m_opt(Opts,show,goal,Ctrl),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST(N,Frame,Opts,[nf(noFrame(N,Frame,Opts))]).

dumpST_Parent(N,Frame,Opts,More):- prolog_frame_attribute(Frame,parent,ParentFrame), NN is N +1,dumpST(NN,ParentFrame,Opts,More),!.
dumpST_Parent(N,Frame,Opts,[nf(noParent(N,Frame,Opts))]).



getPFA(Frame,[L|List],Goal):- !,findall(R, (member(A,[L|List]),getPFA1(Frame,A,R)) ,Goal).
getPFA(Frame,Ctrl,Goal):-getPFA1(Frame,Ctrl,Goal).

getPFA1(_Frame,txt(Txt),txt(Txt)):-!.
getPFA1(Frame,clause,Goal):-getPFA2(Frame,clause,ClRef),clauseST(ClRef,Goal),!.
getPFA1(Frame,Ctrl,Ctrl=Goal):-getPFA2(Frame,Ctrl,Goal),!.
getPFA1(_,Ctrl,no(Ctrl)).

getPFA2(Frame,Ctrl,Goal):- catchv((prolog_frame_attribute(Frame,Ctrl,Goal)),E,Goal=[error(Ctrl,E)]),!.

clauseST(ClRef,clause=Goal):- findall(V,(member(Prop,[file(V),line_count(V)]),clause_property(ClRef,Prop)),Goal).

clauseST(ClRef,Goal = HB):- ignore(((clause(Head, Body, ClRef),copy_term(((Head :- Body)),HB)))),
   snumbervars(HB,0,_),
   findall(Prop,(member(Prop,[source(_),line_count(_),file(_),fact,erased]),clause_property(ClRef,Prop)),Goal).


:-ib_multi_transparent33(seenNote/1).

sendNote(X):-var(X),!.
sendNote(X):-seenNote(X),!.
sendNote(X):-!,assert(seenNote(X)).
sendNote(_).

sendNote(To,From,Subj,Message):-sendNote(To,From,Subj,Message,_).

sendNote(To,From,Subj,Message,Vars):-
	not(not((safe_numbervars((To,From,Subj,Message,Vars)),
	dmsg(sendNote(To,From,Subj,Message,Vars))))).

% ========================================================================================
% safe_numbervars/1 (just simpler safe_numbervars.. will use a rand9ome start point so if a partially numbered getPrologVars wont get dup getPrologVars)
% Each prolog has a specific way it could unnumber the result of a safe_numbervars
% ========================================================================================

safe_numbervars(E,EE):-duplicate_term(E,EE),safe_numbervars(EE),get_gtime(G),snumbervars(EE,'$VAR',G,[attvar(bind)]),
   term_variables(EE,AttVars),forall(member(V,AttVars),(copy_term(V,VC,Gs),V='$VAR'(VC=Gs))).
   

get_gtime(G):- get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G).

safe_numbervars(X):-get_gtime(G),snumbervars(X,G,_).

safe_numbervars(Copy,X,Z):-snumbervars(Copy,X,Z,[attvar(bind)]).
safe_numbervars(Copy,_,X,Z):-snumbervars(Copy,X,Z,[attvar(bind)]).

unnumbervars(X,Y):-with_output_to(atom(A),write_term(X,[numbervars(true),quoted(true)])),atom_to_term(A,Y,_),!.

renumbervars(X,X):-ground(X),!.
renumbervars(X,Z):-unnumbervars(X,Y),safe_numbervars(Y,Z).
renumbervars(Y,Z):-safe_numbervars(Y,Z).

withFormatter(Lang,From,Vars,SForm):-formatter_hook(Lang,From,Vars,SForm),!.
withFormatter(_Lang,From,_Vars,SForm):-sformat(SForm,'~w',[From]).

flush_output_safe:-ignore(catchv(flush_output,_,true)).
flush_output_safe(X):-ignore(catchv(flush_output(X),_,true)).

writeFailureLog(E,X):-
		fmt(user_error,'\n% error: ~q ~q\n',[E,X]),flush_output_safe(user_error),!,
		%,true.
		fmt('\n% error: ~q ~q\n',[E,X]),!,flush_output. %,fmt([E,X]).

%unknown(Old, autoload).




% ========================================================================================
% Some prologs have a printf() type predicate.. so I made up fmtString/fmt in the Cyc code that calls the per-prolog mechaism
% in SWI it''s formzat/N and sformat/N
% ========================================================================================
:-ib_multi_transparent33(isConsoleOverwritten/0).
:- ib_multi_transparent33(formatter_hook/4).


fmtString(X,Y,Z):-sformat(X,Y,Z).
fmtString(Y,Z):-sformat(Y,Z).

saveUserInput:-retractall(isConsoleOverwritten),flush_output.
writeSavedPrompt:-not(isConsoleOverwritten),!.
writeSavedPrompt:-flush_output.
writeOverwritten:-isConsoleOverwritten,!.
writeOverwritten:-assert(isConsoleOverwritten).

writeErrMsg(Out,E):- message_to_string(E,S),fmt(Out,'<cycml:error>~s</cycml:error>\n',[S]),!.
writeErrMsg(Out,E,Goal):- message_to_string(E,S),fmt(Out,'<cycml:error>goal "~q" ~s</cycml:error>\n',[Goal,S]),!.
writeFileToStream(Dest,Filename):-
    catchv((
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

os_to_prolog_filename(OS,_PL):-must(atom(OS)),fail.
os_to_prolog_filename(_OS,PL):-must(var(PL)),fail.
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
% test_call(G):-writeln(G),ignore(once(catchv(G,E,writeln(E)))).

debugFmtList(ListI):-cnotrace((copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),dmsg(List0))),!.
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

% hotrace(Goal).
% Like cnotrace/1 it still skips over debugging Goal.
% Unlike cnotrace/1, it allows traceing when excpetions are raised during Goal.
%hotrace(C):- skipWrapper,!,notrace(C).
:-moo_hide_childs(hotrace/1).
hotrace(X):- ( tracing -> with_assertions( /*tlbugger:*/ wastracing,call_cleanup((nop(notrace),call(X)),trace)) ; call(X) ).


:-'$syspreds':'$hide'(hotrace/1).

traceafter_call(X):- call_cleanup(restore_trace((leash(-all),visible(-all),X)),(leash(+call), trace)).

/*
% :- meta_predicate_transparent notrace_call(0).

notrace_call(X):-cnotrace,catchv(traceafter_call(X),E,(dmsg(E-X),trace,throw(E))).
traceafter_call(X):-X,trace.
traceafter_call(_):-tracing,fail.
traceafter_call(_):-trace,fail.
*/



%getWordTokens(WORDS,TOKENS):-concat_atom(TOKENS,' ',WORDS).
%is_string(S):-string(S).



 %:-interactor.

export_all_preds:-source_location(File,_Line),module_property(M,file(File)),!,export_all_preds(M).

export_all_preds(ModuleName):-forall(current_predicate(ModuleName:F/A),
                   ((export(F/A),functor_safe(P,F,A),moo_hide_childs(ModuleName:P)))).








%%:-user:(forall(current_predicate(bugger:FA),bugger:moo_hide_childs(bugger:FA))).
% hide this module from tracing
%%:-user:(forall(current_predicate(logicmoo_util_strings:FA),bugger:moo_hide_childs(logicmoo_util_strings:FA))).

module_notrace(M):- forall(predicate_property(P,imported_from(M)),bugger:moo_hide_childs(M:P)).


% =====================================================================================================================
:-export((call_no_cuts/1)).
% =====================================================================================================================
:- module_transparent call_no_cuts/1.
:- meta_predicate_transparent call_no_cuts(0).
call_no_cuts(CALL):-clause_safe(CALL,TEST),call(TEST).

call_no_cuts_0(true):-!.
call_no_cuts_0((!)):-!.
call_no_cuts_0((A,B)):-!,call_no_cuts_0(A),call_no_cuts_0(B).
call_no_cuts_0((A;B)):-!,call_no_cuts_0(A);call_no_cuts_0(B).
call_no_cuts_0(C):-call(C).

% =====================================================================================================================
:-export((call_tabled/1)).
% =====================================================================================================================
:- meta_predicate_transparent call_tabled(0).
:- module_transparent call_tabled/1.

:- meta_predicate_transparent call_vars_tabled(?,0).
:- module_transparent call_vars_tabled/2.


:- meta_predicate_transparent call_setof_tabled(?,0,-).
:- meta_predicate_transparent findall_nodupes(?,0,-).
:- module_transparent call_setof_tabled/3.

:- dynamic(call_tabled_list/2).

:- meta_predicate_transparent(make_key(?,-)).

expire_tabled_list(all):-!,retractall(call_tabled_list(_,_)).
expire_tabled_list(_):-!,retractall(call_tabled_list(_,_)).
expire_tabled_list(T):- atoms_of(T,A1), CT= call_tabled_list(Key,List),doall(((CT,once(any_term_overlap_atoms_of(A1,List);(not(member(Key,List)),any_term_overlap_atoms_of(A1,Key))),retractall(CT)))).

any_term_overlap_atoms_of(A1,T2):-atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

any_term_overlap(T1,T2):- atoms_of(T1,A1),atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

:-meta_predicate_transparent(call_tabled_can(0)).

call_tabled_can(Call):-with_assertions( /*tlbugger:*/ can_table,with_no_assertions( /*tlbugger:*/ cannot_table,call_tabled(Call))).

call_tabled(setof(Vars,C,List)):- !,call_setof_tabled(Vars,C,List).
call_tabled(findall(Vars,C,List)):- !,call_setof_tabled(Vars,C,List).
call_tabled(C):- slow_sanity(nonvar(C)), term_variables(C,Vars),!,call_vars_tabled(Vars,C).

call_vars_tabled(Vars,C):- call_setof_tabled(Vars,C,Set),!,member(Vars,Set).

call_setof_tabled(Vars,C,List):- make_key(Vars+C,Key),call_tabled0(Key,Vars,C,List).

findall_nodupes(Vs,C,List):- ground(Vs),!,(C->List=[Vs];List=[]),!.
findall_nodupes(Vs,C,L):- setof(Vs,C,L).

call_tabled0(Key,_,_,List):- call_tabled_list(Key,List),!.
call_tabled0(Key,Vars,C,List):- really_can_table,!,findall_nodupes(Vars,C,List),!,asserta_if_ground(call_tabled_list(Key,List)),!.
call_tabled0(_,Vars,C,List):- findall_nodupes(Vars,C,List),!.

really_can_table:-not(test_tl( /*tlbugger:*/ cannot_table)),test_tl( /*tlbugger:*/ can_table).


:-meta_predicate_transparent(test_tl(1,+)).
test_tl(Pred,Term):-call(Pred,Term),!.
test_tl(Pred,Term):-compound(Term),functor_safe(Term,F,_),call(Pred,F),!.

:-meta_predicate_transparent(test_tl(+)).
test_tl(M:C):-!,call(M:C).
test_tl(C):-functor(C,F,A),test_tl(C,F,A).

:-meta_predicate_transparent(test_tl(+,+,+)).
test_tl(C,F,A):-current_predicate(thglobal:F/A),call(thglobal:C).
test_tl(C,F,A):-current_predicate(thlocal:F/A),call(thlocal:C).
test_tl(C,F,A):-current_predicate(thlocal_global:F/A),call(thlocal_global:C).


% asserta_if_ground(_):- !.
asserta_if_ground(G):- ground(G),asserta(G),!.
asserta_if_ground(_).


% =====================================================================================================================
:- module_notrace(bugger).
% =====================================================================================================================
%:- module_notrace(logicmoo_util_strings).
% =====================================================================================================================

:-source_location(File,_Line),module_property(M,file(File)),!,forall(current_predicate(M:F/A),moo_show_childs(M,F,A)).

:-moo_show_childs(bugger,prolog_ecall_fa,5).
:-moo_show_childs(bugger,prolog_ecall,3).
:-moo_show_childs(bugger,must,1).
:-moo_show_childs(bugger,must,2).
:-moo_show_childs(bugger,must_flag,3).
:-moo_show_childs(bugger,traceok,1).
:-moo_show_childs(bugger,hotrace,1).

% though maybe dumptrace
default_dumptrace(trace).

ggtrace:- default_dumptrace(DDT), ggtrace(DDT).
ggtrace(Trace):- 
   leash(-call),((visible(+all),visible(-unify),visible(+exception),
   leash(-all),leash(+exception),
   leash(+call))),Trace,leash(-call).

gftrace:- default_dumptrace(DDT), gftrace(DDT).
gftrace(Trace):- 
   leash(-call),((visible(-all), visible(+fail),visible(+call),visible(+exception),
   leash(-all),leash(+exception),
   leash(+call))),Trace,leash(-call).

grtrace:- default_dumptrace(DDT), grtrace(DDT).
grtrace(Trace):- notrace(( visible(+all),leash(+all))), Trace.



show_and_do(C):-dmsg(C),!,traceok(C).

dtrace:-dtrace(trace).


% esa Michele Murer 360-750-7500 ext_135

%dtrace:- skipWrapper,!,dmsg(dtrace_skipWrapper).
dtrace(G):-tracing,!,write(dtrace(G)),nl,notrace,leash(+call),dtrace(G).
dtrace(G):-write(dtrace(G)),nl,has_auto_trace(C),!,C.
dtrace(G):-repeat,debug,dumptrace(G),!.

dumptrace(_):-tracing,!,leash(+call).
dumptrace(G):- not(ifCanTrace),!,notrace((fmt((not(ifCanTrace(G)))))),!,snumbervars(G),!,notrace(dumpST).
dumptrace(G):- fmt(in_dumptrace(G)),leash(+exception),show_call(get_single_char(C)),with_all_dmsg(dumptrace(G,C)).
dumptrace(_,0'g):-notrace(dumpST),!,fail.
dumptrace(G,0'l):-notrace(ggtrace),!,G.
dumptrace(_,0'b):-debug,prolog,!,fail.
dumptrace(_,0'a):-abort.
dumptrace(_,0'e):-halt(1).
dumptrace(G,0'l):-show_and_do(rtrace(G)).
dumptrace(G,0'c):-show_and_do((G)).
dumptrace(G,0'r):-show_and_do(ftrace(G)).
dumptrace(_,0't):-visible(+all),leash(+all),trace,!.
dumptrace(G,0't):-show_and_do(grtrace(G)).
dumptrace(_,10):-dumptrace_ret,!.
dumptrace(_,13):-dumptrace_ret,!.
dumptrace(_,C):-fmt(unused_keypress(C)),!,fail.
% )))))))))) %
dumptrace_ret:-leash(+all),visible(+all),visible(+unify),trace.

restore_trace(Goal):-  tracing, notrace,!,'$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,(('$leash'(_, Old),'$visible'(_, OldV),trace),trace)).
restore_trace(Goal):-  '$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,((notrace,'$leash'(_, Old),'$visible'(_, OldV)))).

rtrace(Goal):- do_gc, restore_trace((notrace((visible(+all),visible(+unify),visible(+exception),leash(-all),leash(+exception))),trace,Goal)).

ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   leash(-all),leash(+exception),trace,Goal)).



module_predicate(ModuleName,F,A):-current_predicate(ModuleName:F/A),functor_safe(P,F,A),
   not((( predicate_property(ModuleName:P,imported_from(IM)),IM\==ModuleName ))).

:-module_transparent(module_predicates_are_exported/0).
:-module_transparent(module_predicates_are_exported/1).
:-module_transparent(module_predicates_are_exported0/1).

module_predicates_are_exported:- context_module(CM),module_predicates_are_exported(CM).

module_predicates_are_exported(user):-!,context_module(CM),module_predicates_are_exported0(CM).
module_predicates_are_exported(Ctx):-module_predicates_are_exported0(Ctx).

module_predicates_are_exported0(user):- !. % dmsg(warn(module_predicates_are_exported(user))).
module_predicates_are_exported0(ModuleName):-
   module_property(ModuleName, exports(List)),
    findall(F/A,
    (module_predicate(ModuleName,F,A),
      not(member(F/A,List))), Private),
   module_predicates_are_not_exported_list(ModuleName,Private).

module_predicates_are_not_exported_list(ModuleName,Private):- once((length(Private,Len),dmsg(module_predicates_are_not_exported_list(ModuleName,Len)))),fail.
module_predicates_are_not_exported_list(ModuleName,Private):- forall(member(F/A,Private),
     (( % dmsg(export(ModuleName:F/A)),
                   ModuleName:export(F/A)))).

arg_is_transparent(Arg):- member(Arg,[':','0']).
arg_is_transparent(0).
arg_is_transparent(Arg):- number(Arg).

% make meta_predicate_transparent's module_transparent
module_meta_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore(((predicate_property(ModuleName:P,(meta_predicate( P ))),
            not(predicate_property(ModuleName:P,(transparent))), (compound(P),arg(_,P,Arg),arg_is_transparent(Arg))),
                   (dmsg(todo(module_transparent(ModuleName:F/A))),
                   (module_transparent(ModuleName:F/A)))))).

all_module_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore((
            not(predicate_property(ModuleName:P,(transparent))),
                   (dmsg(todo(module_transparent(ModuleName:F/A)))),
                   (module_transparent(ModuleName:F/A))))).

quiet_all_module_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore((
            not(predicate_property(ModuleName:P,(transparent))),
                   nop(dmsg(todo(module_transparent(ModuleName:F/A)))),
                   (module_transparent(ModuleName:F/A))))).


:- module_predicates_are_exported(bugger).
:- module_meta_predicates_are_transparent(bugger).

:-module_property(bugger, exports(List)),moo_show_childs(List).

% bugger_prolog_exception_hook(error(syntax_error(operator_expected),_),_,_,_).
bugger_prolog_exception_hook(Info,_,_,_):- bugger_error_info(Info),!, dumpST,dmsg(prolog_exception_hook(Info)), dtrace.

bugger_error_info(C):-contains_var(type_error,C).
bugger_error_info(C):-contains_var(instantiation_error,C).
bugger_error_info(C):-contains_var(existence_error(procedure,_/_),C).


% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- user_use_module(library(prolog_stack)).

user:prolog_exception_hook(A,B,C,D):- fail,
   once(copy_term(A,AA)),catchv(( once(bugger_prolog_exception_hook(AA,B,C,D))),_,fail),fail.

:-'$syspreds':'$hide'('$syspreds':leash/1).
:-'$syspreds':'$hide'(leash/1).
:-'$syspreds':'$hide'('$syspreds':visible/1).
:-'$syspreds':'$hide'(visible/1).
% :-'$set_predicate_attribute'(!, trace, 1).
% :-hideTrace.

%:-module(bugger).
%:-prolog.

:-retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).

end_of_file.

% current_predicate

:-export(test_is_safe/1).
test_is_safe(Arg):-dmsg(color(green,test_is_safe(Arg))).

:- use_module(logicmoo_util_bugger_test).
:- listing(test1a).
:- listing(test1b).

:-test1a.
:-test1b.


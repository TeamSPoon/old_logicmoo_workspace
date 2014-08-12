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
         tlbugger:inside_loop_check/1,
         is_loop_checked/1,
         rtrace/1,
         snumbervars/1,
         safe_numbervars/1,
         safe_numbervars/2,
         loop_check_clauses/3,
         loop_check_clauses/2,
         must_det/1,
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
     dumpST/0,
     dumpST/1,
     dtrace/0,
     dtrace/1,
     trace_or_throw/1,
     trace_or/1,
     os_to_prolog_filename/2,
     debugOnFailure0/1,
      op(1150,fx,meta_predicate_transparent),
      meta_predicate_transparent/1,      
      op(1150,fx,decl_thlocal),
     ifThen/2,
     nop/1,
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

     logOnErrorIgnore/1,
     debugOnErrorIgnore/1,
     %debugCall/1,
     debugCallWhy/2,
     %debugCallF/1,

     % can ignore
     failOnError/1, % for wrapping code may throw to indicate failure

     ignoreOnError/1, % same

     debugOnError/1, % Throws unless [Fail or Debug]
     logOnError/1, % Succeeds unless no error and failure occured


     debugOnFailure/1, % Succeeds but can be set to [Fail or Debug]
     logOnFailure/1,  % Fails unless [+Ignore]

     throwOnFailure/1, % Throws unless [Fail or Debug]

     must_each/1,  % list block must succeed once .. it smartly only debugs to the last failures

     % cant ignore - Throws but can be set to [Throw, Fail or Ignore or Debug]
     must/1, % must succeed at least once
     cmust/1, % doesnt run on release
     gmust/2, % like must/1 but arg2 must be ground at exit
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
    moo_hide/1,

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

         op(1150,fx,(dynamic_multifile_exported)),
         %dynamic_multifile_exported/3,
         %dynamic_multifile_exported/2,
         dynamic_multifile_exported/1,
     export_all_preds/0,
     export_all_preds/1
	 ]).

:-export((bad_idea/0)).
bad_idea:-fail.

/*
:- export(static_predicate/1).
:- meta_predicate(static_predicate(:)).
:- module_transparent(static_predicate/1).
static_predicate(FA):-once(predicate_property(FA,_)),not(predicate_property(FA,dynamic)).
*/

% ===================================================================
% Substitution based on ==
% ===================================================================

% Usage: dbgsubst(+Fml,+X,+Sk,?FmlSk)

dbgsubst(A,B,C,D):-var(A),!,dmsg(dbgsubst(A,B,C,D)),dumpST,dtrace,dbgsubst0(A,B,C,D).
dbgsubst(A,B,C,D):-dbgsubst0(A,B,C,D).

dbgsubst0(A,B,C,D):- 
      ccatch(notrace(nd_dbgsubst(A,B,C,D)),E,(dumpST,dmsg(E:nd_dbgsubst(A,B,C,D)),fail)),!.
dbgsubst0(A,_B,_C,A).

nd_dbgsubst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_dbgsubst(  P, X,Sk, P1 ) :- functor_catch(P,_,N),nd_dbgsubst1( X, Sk, P, N, P1 ).

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
static_predicate(M,F,A):- functor(FA,F,A),  once(M:predicate_property(FA,_)),not(M:predicate_property(FA,dynamic)),not((M:predicate_property(FA,imported_from(Where)),Where \= M)).


:- export(dynamic_safe/1).
:- meta_predicate(dynamic_safe(:)).
:- module_transparent(dynamic_safe/1).
dynamic_safe(MFA):- with_mfa(MFA,dynamic_safe).

:- export(dynamic_safe/3).
:- meta_predicate(dynamic_safe(+,+,+)).
:- module_transparent(dynamic_safe/3).
dynamic_safe(M,F,A):- (static_predicate(M,F,A) -> true ; M:dynamic(M:F/A)). % , warn_module_dupes(M,F,A).

:-op(1150,fx,dynamic_safe).

% ----------
:-export(with_pi/2).
:- module_transparent(with_pi/2).
:- meta_predicate(with_pi(:,3)).
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
with_pi_selected(CM,M, F/A ,Pred3):-!,functor(P,F,A),  with_pi_stub(CM,M,P,F/A,Pred3).
with_pi_selected(CM,M, P ,Pred3):-  functor(P,F,A), with_pi_stub(CM,M,P,F/A,Pred3).

:- export(with_pi_stub/5).
with_pi_stub(CM, M,P, F/A , CM:Pred3):- ((integer(A),atom(M),atom(F),functor(P,F,A))),
   ('@'(M:call(Pred3,CM, M,P,F/A),CM)),!.
with_pi_stub(CM, M,P, F/A , Pred3):- ((integer(A),atom(M),atom(F),functor(P,F,A))),
   ('@'(M:call(Pred3,CM, M,P,F/A),CM)),!.
with_pi_stub(CM, M,P,FA,Pred3):- throw(invalide_args(CM, M,P,FA,Pred3)).
% ----------

:- export(with_mfa/2).
:- module_transparent(with_mfa/2).
:- meta_predicate(with_mfa(:,4)).
with_mfa(P  ,Pred3):- with_pi(P,with_mfa_of(Pred3)).

:- module_transparent(with_mfa_of/5).
:- meta_predicate(with_mfa_of(3,+,+,+)).
with_mfa_of(Pred3,_CM,M,_P,F/A):-M:call(Pred3,M,F,A).


% ----------

:- export((dynamic_multifile_exported)/1).
:- export((dynamic_multifile_exported)/4).
:- meta_predicate(( dynamic_multifile_exported(:), dynamic_multifile_exported(+,+,+,+))).
:- module_transparent((dynamic_multifile_exported)/1).
dynamic_multifile_exported( FA ):- with_pi(FA,(dynamic_multifile_exported)).
dynamic_multifile_exported(CM, M, _PI, F/A):- CALL='@'((dynamic_safe(M,F,A), 
   M:multifile(F/A), 
   CM:multifile(F/A),
   M:export(F/A)),M),CALL. %,dmsg(dynamic_multifile_exported(CALL)).

% ----------

:- export(((meta_predicate_transparent)/1)).
:- meta_predicate(( meta_predicate_transparent(:),  meta_predicate_transparent(+,+,+,+))).
meta_predicate_transparent(MP):-with_pi(MP,(meta_predicate_transparent)).

meta_predicate_transparent(CM,M,PI,F/A):-
   M:module_transparent(F/A),
   notrace((fill_args(PI,('?')),!,
   dbgsubst(PI, (0),(0),PI1),
   dbgsubst(PI1,(^),(0),PI2),
   dbgsubst(PI2,(:),(:),PI3))),
   M:meta_predicate(PI3),
   dynamic_multifile_exported(CM, M,PI3,F/A).

fill_args([Arg|More],With):-!,ignore(With=Arg),fill_args(More,With).
fill_args([],_).
fill_args(PI,With):-PI=..[_|ARGS],fill_args(ARGS,With).

% use ccatch/3 to replace catch/3 works around SWI specific bugs when using $abort/0 and block/3
% (ccatch/3 allows you to have these exceptions buggle past your block handlers)
:- meta_predicate((ccatch(0, ?, 0))).
bubbled_ex(block(_,_)).
bubbled_ex('$aborted').
bubbled_ex_check(E):-bubbled_ex(E),throw(E).
bubbled_ex_check(_).
ccatch(Goal,E,Recovery):- nonvar(E),!,catch(Goal,E,Recovery). % normal mode
ccatch(Goal,E,Recovery):- catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode

:-export(functor_catch/3).
functor_catch(P,F,A):-ccatch(functor(P,F,A),E,(dumpST,dmsg(E:functor(P,F,A)),dtrace)).

:- meta_predicate_transparent(meta_predicate_transparent(0)).


:- export(def_meta_predicate/3).
:- meta_predicate_transparent((def_meta_predicate(0,+,+))).

def_meta_predicate(M:F,S,E):-!,doall(((between(S,E,N),make_list('?',N,List),CALL=..[F|List],'@'(meta_predicate_transparent(CALL),M)))).
def_meta_predicate(F,S,E):- trace_or_throw(def_meta_predicate(F,S,E)).


:- meta_predicate_transparent((loop_check_term(0,?,0))).
:- meta_predicate_transparent((loop_check(^,^))).


:- export(((decl_thlocal)/1)).
:- meta_predicate(( decl_thlocal(+),  decl_thlocal(+,+,+,+))).
decl_thlocal(M:P):-!,with_pi(M:P,(decl_thlocal)).
decl_thlocal(P):-with_pi(thlocal:P,(decl_thlocal)).

decl_thlocal(CM,M,PI,F/A):-
   thread_local(M:F/A),
   M:module_transparent(F/A),
   notrace((fill_args(PI,('?')),!,
   dbgsubst(PI, (0),(0),PI1),
   dbgsubst(PI1,(^),(0),PI2),
   dbgsubst(PI2,(:),(:),PI3))),
   M:meta_predicate(PI3),
   dynamic_multifile_exported(CM, M,PI3,F/A).


   

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


trace_or(E):- dumpST,dmsg(E),dtrace,!.
trace_or(E):- E.




cnotrace:-notrace.

:- meta_predicate_transparent(cnotrace(0)).
:- module_transparent(cnotrace/1).
:- export(cnotrace/1).
cnotrace(C):-ccatch(hotrace(C),E,((dmsg(E=C),rtrace(C),trace_or_throw(E=C)))).


trace_or_throw(E):-trace_or(throw(E)).

% :- use_module(library(prolog_stack)).

:-multifile term_to_message_string/2.
:-dynamic term_to_message_string/2.

:- dynamic isDebugging/1.

:-multifile was_module/2.
:-dynamic was_module/2.
:-module_transparent was_module/2.
:-multifile evil_term/3.
:-dynamic evil_term/3.

:- thread_local has_auto_trace/1.



:- meta_predicate_transparent meta_interp(-,-).

meta_interp_signal(meta_call(V)):-!,nonvar(V).
meta_interp_signal(meta_callable(_,_)).
meta_interp_signal(_:meta_call(V)):-!,nonvar(V).
meta_interp_signal(_:meta_callable(_,_)).

:-export(meta_interp/2).

meta_interp(_,_):-stack_check(300),fail.

meta_interp(CE,A):- var(A),!, throw(meta_interp(CE,A)).
meta_interp(M:X,A):- atom(M),!,meta_interp(X,A).
meta_interp(CE,M:X):- atom(M),!,meta_interp(CE,X).

meta_interp(_CE,A):- leash(+all),meta_interp_signal(A),!,fail.
meta_interp(_,_:true):-!.
%right thing i thought 
%meta_interp(CE,M:A):-!, '@'(meta_interp(CE,A),M).
meta_interp(CE,A):- call(CE, meta_callable(A,NewA)),!,NewA.
meta_interp(CE,not(A)):-!,not(meta_interp(CE,A)).
meta_interp(CE,once(A)):-!,once(meta_interp(CE,A)).
meta_interp(CE,(A;B)):-!,meta_interp(CE,A);meta_interp(CE,B).
meta_interp(CE,(A->B)):-!,meta_interp(CE,A)->meta_interp(CE,B).
meta_interp(CE,(A->B;C)):-!,(meta_interp(CE,A)->meta_interp(CE,B);meta_interp(CE,C)).
meta_interp(CE,(A,!)):-!,meta_interp(CE,A),!.
meta_interp(CE,(A,B)):-!,meta_interp(CE,A),meta_interp(CE,B).
meta_interp(CE,[A]):-!,meta_interp(CE,A).
meta_interp(CE,[A|B]):-!,meta_interp(CE,A),meta_interp(CE,B).
%meta_interp(_CE,!):- !, cut_block(!).
meta_interp(CE,_:A):- !, call(CE,meta_call(A)).
meta_interp(CE,_:A):-!, meta_interp(CE,A).
meta_interp(CE,A):- call(CE,meta_call(A)).


% was_module(Mod,Exports) :- nop(was_module(Mod,Exports)).

bugger_flag(F=V):-bugger_flag(F,V).
bugger_flag(F,V):-current_prolog_flag(F,V).
set_bugger_flag(F,V):-current_prolog_flag(F,_Old),set_prolog_flag(F,V),!.
set_bugger_flag(F,V):-create_prolog_flag(F,V,[term]).


:- meta_predicate_transparent moo_hide_childs(0).
:- module_transparent moo_hide_childs/0.

:- meta_predicate_transparent one_must(^,^).
:- meta_predicate_transparent must(^).
:- meta_predicate_transparent prolog_must_not(^).
:- meta_predicate_transparent gmust(^,^).
:- meta_predicate_transparent logOnFailure0(^).
:- meta_predicate_transparent debugOnError0(^).
:- meta_predicate_transparent will_debug_else_throw(^,^).
:- meta_predicate_transparent printAll(^,*).
:- meta_predicate_transparent load_dirrective(^,*).
:- meta_predicate_transparent debugOnFailure0(^).
:- meta_predicate_transparent cli_notrace(^).
:- meta_predicate_transparent printPredCount(*,^,*).
:- meta_predicate_transparent ignoreOnError(^).
:- meta_predicate_transparent traceIf(^).
:- meta_predicate_transparent ifThen(^,^).
:- meta_predicate_transparent prolog_ecall_fa(*,1,*,*,^).
:- meta_predicate_transparent tryCatchIgnore(^).
:- meta_predicate_transparent logOnError0(^).
:- meta_predicate_transparent failOnError(^).
% :- meta_predicate_transparent test_call(^).
:- meta_predicate_transparent cmust(^).
%:- meta_predicate_transparent debugCall(^).
:- meta_predicate_transparent prolog_ecall(*,1,?).
%:- meta_predicate_transparent traceafter_call(^).
:- meta_predicate_transparent if_prolog(*,^).
:- meta_predicate_transparent must(^).
:- meta_predicate_transparent must_each(^).
:- meta_predicate_transparent must_det(^).
:- meta_predicate_transparent loop_check_clauses(^,?,^).
:- meta_predicate_transparent loop_check_clauses(^,^).
:- meta_predicate_transparent must_det_l(?).
:- meta_predicate_transparent rtrace(^).
:- meta_predicate_transparent one_must_det(+,^).
:- meta_predicate_transparent rtraceOnError(^).
:- meta_predicate_transparent debugOnError(^).
:- meta_predicate_transparent debugOnError0(^).
:- meta_predicate_transparent debugOnErrorIgnore(^).
:- meta_predicate_transparent debugOnFailure0(^).
:- meta_predicate_transparent forall_member(*,*,^).
:- meta_predicate_transparent throwOnFailure(^).
:- meta_predicate_transparent hotrace(^).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (^.:16 sec)
:- meta_predicate_transparent prolog_must_l(^).
:- meta_predicate_transparent printAll(^).
:- meta_predicate_transparent prolog_must(^).
:- meta_predicate_transparent showProfilerStatistics(^).
%:- meta_predicate_transparent debugCallF(^).

:- module_transparent user_use_module/1.
% user_use_module(What):-!, ensure_loaded(What).
% user_use_module(logicmoo(What)):- !, '@'(use_module(logicmoo(What)),'user').
% user_use_module(library(What)):- !, use_module(library(What)).
user_use_module(What):- '@'(use_module(What),'user').

:- '@'(use_module(logicmoo_util_library), 'user').


:- thread_local tlbugger:inside_loop_check/1.
:- dynamic_multifile_exported(tlbugger:inside_loop_check/1).



:- module_transparent(must_det_l/1).

:- module_transparent(loop_check/2).
:- module_transparent(no_loop_check/1).
:- export(no_loop_check/1).
%:- module_transparent(loop_check_fail/1).
%:- module_transparent(loop_check_throw/1).
%:- module_transparent(loop_check_term/3).
%:- meta_predicate_transparent((loop_check_throw(0))).
%:- meta_predicate_transparent((loop_check_fail(0))).


:-thread_local thlocal:can_table/0.
:-thread_local thlocal:cannot_table/0.
thlocal:can_table.

snumbervars(BC):-numbervars(BC,0,_,[singletons(true),attvar(skip)]).

:-meta_predicate_transparent(once_if_ground(0)).
once_if_ground(Call):-not(ground(Call)),!,Call.
once_if_ground(Call):- once(Call).

:-meta_predicate_transparent(once_if_ground(0,-)).
once_if_ground(Call,T):-not(ground(Call)),!,Call,deterministic(D),(D=yes -> T= (!) ; T = true).
once_if_ground(Call,!):-once(Call).


to_list_of(_,[Rest],Rest):-!.
to_list_of(RL,[R|Rest],LList):-
      to_list_of(RL,R,L),
      to_list_of(RL,Rest,List),
      LList=..[RL,L,List],!.

call_or_list([Rest]):-!,call(Rest).
call_or_list(Rest):-to_list_of(';',Rest,List),!,call(List).

call_skipping_n_clauses(N,H):-
   findall(B,clause_safe(H,B),L),length(L,LL),!,LL>N,length(Skip,N),append(Skip,Rest,L),!,call_or_list(Rest).

cannot_table_call(Call):- with_assertions(cannot_table,Call).

% =========================================================================

is_loop_checked(B):- 
       make_key(B,BC),!,tlbugger:inside_loop_check(BC).



no_loop_check(Call):- with_no_assertions(tlbugger:inside_loop_check(_),Call).

loop_check_clauses(B,TODO):-  loop_check_clauses(B,1,TODO).
loop_check_clauses(B,N,TODO):- make_key(B,BC), loop_check_term(call_skipping_n_clauses(N,B),BC,TODO).

% loop_check_throw(B):- loop_check(B,((retractall(tlbugger:inside_loop_check(B)),debugCallWhy(loop_check_throw(B),loop_check_throw(B))))).
loop_check(B):- loop_check(B,fail).
loop_check(B, TODO):- make_key(B,BC),!, loop_check_term(B,BC,TODO).

loop_check_term(_B, BC, TODO):- tlbugger:inside_loop_check(BC),!,call(TODO).
loop_check_term( B, BC,_TODO):- not(bad_idea),!,
    setup_call_cleanup(asserta(tlbugger:inside_loop_check(BC)),B,
     (ignore(retract((tlbugger:inside_loop_check(BC)))))).
loop_check_term( B, BC,_TODO):- with_assertions(tlbugger:inside_loop_check(BC),B).


% =========================================================================
% cli_notrace(+Call) is nondet.
% use call/1 with trace turned off
cli_notrace(Call):-tracing,cnotrace,!,call_cleanup(call(Call),trace).
cli_notrace(Call):-call(Call).

% =========================================================================

:- dynamic(skip_bugger/0).

% false = use this wrapper, true = code is good and avoid using this wrapper
skipWrapper:-skip_bugger.
skipWrapper:-tracing.
% false = hide this wrapper
showHiddens:-true.

:- set_prolog_flag(backtrace_depth,   200).
:- set_prolog_flag(backtrace_goal_depth, 20).
:- set_prolog_flag(backtrace_show_lines, true).
% =========================================================================



ib_multi_transparent33(MT):-multifile(MT),module_transparent(MT),dynamic_safe(MT).

:- multifile current_directory_search/1.
:- module_transparent current_directory_search/1.
:- ib_multi_transparent33(formatter_hook/4).
:- module_transparent(hotrace/1).


% :-ccatch(guitracer,E,(writeq(E),nl)).



% hide Pred from tracing
to_m_f_arity_pi(M:Plain,M,F,A,PI):-!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(Term,M,F,A,PI):- strip_module(Term,M,Plain),Plain\=Term,!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(F/A,_M,F,A,PI):-functor_catch(PI,F,A),!.
to_m_f_arity_pi(PI,_M,F,A,PI):-functor_catch(PI,F,A).

with_preds((X,Y),M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call),with_preds(Y,M,F,A,PI,Call).
with_preds([X],M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call).
with_preds([X|Y],M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call),with_preds(Y,M,F,A,PI,Call).
with_preds(M:X,_M,F,A,PI,Call):-!, with_preds(X,M,F,A,PI,Call).
with_preds(X,M,F,A,PI,Call):-forall(to_m_f_arity_pi(X,M,F,A,PI),Call).

% moo_hide_show_childs(_):-showHiddens,!.
moo_hide_show_childs(X):- with_preds(X,_M,F,A,_PI,'$hide'(F/A)).


 %=========================================
 % Module Utils
 %=========================================

 % module_functor(PredImpl,Module,Pred,Arity).

 module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).
 strip_arity(Pred/Arity,Pred,Arity).
 strip_arity(PredImpl,Pred,Arity):-functor_catch(PredImpl,Pred,Arity).
% moo_hide(+Pred).
:-export(moo_hide/1).

moo_hide(Preds):- with_preds(Preds,M,F,A,_PI,moo_hide_childs(M,F/A)).

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



moo_hide_show_childs(M,F,A):-functor_catch(MPred,F,A),moo_hide_show_childs(M,F,A,MPred).

moo_hide_show_childs(M,_,_,MPred):-
   not(predicate_property(_:MPred,imported_from(M))).

moo_hide_show_childs(M,F,A,_MPred):-
 % dmsg(moo_hide_show_childs(M,F,A,MPred)),
 '$set_predicate_attribute'(M:F/A, trace, 0),
 '$set_predicate_attribute'(M:F/A, noprofile, 1),
 '$set_predicate_attribute'(M:F/A, hide_childs, 0),!.


% ==========================================================
% can/will Tracer.
% ==========================================================

:-dynamic(ifCanTrace/0).
ifCanTrace.

isConsole :- telling(user).
isConsole :- current_output(X),!,stream_property(X,alias(user_output)).

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
   ccatch/3,
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
:- meta_predicate_transparent with_output_to_stream(*,0).

functor_source_file(M,P,F,A,File):-functor_source_file0(M,P,F,A,File). % must(ground((M,F,A,File))),must(user:nonvar(P)).
functor_source_file0(M,P,F,A,File):-current_predicate(F/A),functor_catch(P,F,A),source_file(P,File),predicate_module(P,M).

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
hideTraceMP(M,P,T):-functor_catch(P,F,0),trace,hideTraceMFA(M,F,_A,T),!.
hideTraceMP(M,P,T):-functor_catch(P,F,A),hideTraceMFA(M,F,A,T),!.

tryCatchIgnore(MFA):- ccatch(MFA,_E,true). %%dmsg(tryCatchIgnoreError(MFA:E))),!.
tryCatchIgnore(_MFA):- !. %%dmsg(tryCatchIgnoreFailed(MFA)).

tryHide(_MFA):-showHiddens,!.
tryHide(MFA):- tryCatchIgnore('$hide'(MFA)).

hideTraceMFA(_,M:F,A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMFA(M,F,A,T):-user:nonvar(A),functor_catch(P,F,A),predicate_property(P,imported_from(IM)),IM \== M,!,nop(dmsg(doHideTrace(IM,F,A,T))),hideTraceMFA(IM,F,A,T),!.
hideTraceMFA(M,F,A,T):-hideTraceMFAT(M,F,A,T),!.

hideTraceMFAT(M,F,A,T):-doHideTrace(M,F,A,T),!.

doHideTrace(_M,_F,_A,[]):-!.
doHideTrace(M,F,A,[hide|T]):- tryHide(M:F/A),!,doHideTrace(M,F,A,T),!.
doHideTrace(M,F,A,ATTRIB):- tryHide(M:F/A),!,
  tryCatchIgnore(trace(M:F/A,ATTRIB)),!.


ctrace:-willTrace->trace;notrace.

buggeroo:-hideTrace,traceAll,atom_concat(guit,racer,TRACER), ccatch(call(TRACER),_,true),debug,list_undefined.

singletons(_).

:-set_prolog_flag(debugger_show_context,true).
% :-set_prolog_flag(trace_gc,true).
:-set_prolog_flag(debug,true).
:-set_prolog_flag(gc,false).


failOnError(Call):-ccatch(Call,_,fail).

fresh_line:-current_output(Strm),fresh_line(Strm),!.
fresh_line(Strm):-failOnError((stream_property(Strm,position('$stream_position'(_,_,POS,_))),ifThen(POS>0,nl(Strm)))),!.
fresh_line(Strm):-failOnError(nl(Strm)),!.
fresh_line(_).

ifThen(When,Do):-When->Do;true.

% :- current_predicate(F/N),trace(F/N, -all),fail.
/*
traceAll:- current_predicate(user:F/N),
  functor_catch(P,F,N),
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
must_assign(From,To):-dmsg(From),dmsg(=),dmsg(From),dmsg(must_assign),!,trace,To=From.




prolog_must(Call):-must(Call).


% cmust is only used for type checking
cmust(_):-bugger_flag(release,true),!.
cmust(Call):-one_must(Call,will_debug_else_throw(cmust(Call),Call)).

% gmust is must with cmust
gmust(True,Call):-ccatch((Call,(True->true;throw(retry(gmust(True,Call))))),retry(gmust(True,_)),(trace,Call,True)).

% must is used declaring the predicate must suceeed

throwOnFailure(Call):-one_must(Call,throw(throwOnFailure(Call))).
ignoreOnError(CX):-ignore(ccatch(CX,_,true)).

% pause_trace(_):- cnotrace(((debug,visible(+all),leash(+exception),leash(+call)))),trace.

%debugCall(C):-cnotrace,dmsg(debugCall(C)),dumpST, pause_trace(errored(C)),ggtrace,C.
%debugCallF(C):-cnotrace,dmsg(debugCallF(C)),dumpST, pause_trace(failed(C)),gftrace,C.

debugCallWhy(Why, C):-cnotrace,dmsg(Why),debugCallWhy2(Why, C).
debugCallWhy2(failed(_Why), C):- gftrace,grtrace,trace,leash(+all),dtrace(C).
debugCallWhy2(thrown(_Why), C):- ggtrace,trace,leash(+all),dtrace(C).
debugCallWhy2(_Why, C):- grtrace,trace,leash(+all),dtrace(C).

:-export(rtraceOnError/1).
rtraceOnError(C):-ccatch(
  with_skip_bugger( C ),E,(dmsg(E=C),leash(+call),trace,leash(+exception),leash(+all),rtrace(with_skip_bugger( C )),dmsg(E=C),leash(+call),dtrace)).


with_skip_bugger(C):-setup_call_cleanup(asserta(skip_bugger),C,retract(skip_bugger)).

debugOnError(C):- skipWrapper,!,C.
debugOnError(C):- !,debugOnError0(C).
debugOnError(C):-prolog_ecall(0,debugOnError0,C).
debugOnError0(C):- skipWrapper,!,C.
debugOnError0(C):- ccatch(C,E,call_cleanup(debugCallWhy(thrown(E),C),throw(E))).
debugOnErrorEach(C):-prolog_ecall(1,debugOnError0,C).
debugOnErrorIgnore(C):-ignore(debugOnError0(C)).

debugOnFailure(C):-prolog_ecall(0,debugOnFailure0,C).
debugOnFailure0(C):- one_must(rtraceOnError(C),debugCallWhy(failed(debugOnFailure0(C)),C)).
debugOnFailureEach(C):-prolog_ecall(1,debugOnFailure,C).
debugOnFailureIgnore(C):-ignore(debugOnFailure(C)).

logOnError(C):-prolog_ecall(0,logOnError0,C).
logOnError0(C):- ccatch(C,E,dmsg(logOnError(E,C))).
logOnErrorEach(C):-prolog_ecall(1,logOnError,C).
logOnErrorIgnore(C):-ignore(logOnError(C)).

logOnFailure0(C):- one_must(C,dmsg(logOnFailure(C))).
logOnFailureEach(C):-prolog_ecall(1,logOnFailure,C).
logOnFailureIgnore(C):-ignore(logOnFailure(C)).


%debugOnFailure0(X):-ctrace,X.
%debugOnFailure0(X):-ccatch(X,E,(writeFailureLog(E,X),throw(E))).
%throwOnFailure/1 is like Java/C's assert/1
%debugOnFailure1(Module,CALL):-trace,debugOnFailure(Module:CALL),!.
%debugOnFailure1(arg_domains,CALL):-!,logOnFailure(CALL),!.

beenCaught(must(Call)):- !, beenCaught(Call).
beenCaught((A,B)):- !,beenCaught(A),beenCaught(B).
beenCaught(Call):- fail, predicate_property(Call,number_of_clauses(_Count)), clause(Call,(_A,_B)),!,clause(Call,Body),beenCaught(Body).
beenCaught(Call):- ccatch(once(Call),E,(dmsg(caugth(Call,E)),beenCaught(Call))),!.
beenCaught(Call):- traceAll,dmsg(tracing(Call)),debug,trace,Call.

kill_term_expansion:-
   abolish(term_expansion,2),
   abolish(goal_expansion,2),
   dynamic(term_expansion/2),
   dynamic(goal_expansion/2).

local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-predicate_property(P,built_in),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
%local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains666(F,'aiml_'),!.
local_predicate(P,F/N):-functor_catch(P,F,N),!,fail.

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


on_prolog_ecall_override(F,A,Var,_SentValue, Value):- on_prolog_ecall(F,A,Var,Value), Value \= reuse,!.
on_prolog_ecall_override(_F,_A,_Var, Value, Value).

bin_ecall(F,A,unwrap,true):-member(F/A,[(';')/2,(',')/2,('->')/2,('call')/1]).
bin_ecall(F,A,fail,throw(never_fail(F/A))):-member(F/A,[(retractall)/1]).
bin_ecall(F,A,asis,true):-member(F/A,[('must')/1]).


:-moo_hide_show_childs(prolog_ecall/2).
:-moo_hide_show_childs(prolog_ecall/5).


prolog_ecall(_,_,Call):-var(Call),!,trace,randomVars(Call).
% prolog_ecall(BDepth,OnCall,M:Call):- fail,!, '@'( prolog_ecall(BDepth,OnCall,Call), M).

prolog_ecall(_,_,Call):-skipWrapper,!,Call.
prolog_ecall(BDepth,OnCall, (X->Y;Z)):-!,(prolog_ecall(BDepth,OnCall,X) -> prolog_ecall(BDepth,OnCall,Y) ; prolog_ecall(BDepth,OnCall,Z)).
prolog_ecall(BDepth,OnCall,Call):-functor_catch(Call,F,A),prolog_ecall_fa(BDepth,OnCall,F,A,Call).

% fake = true
prolog_ecall_fa(_,_,F,A,Call):-
  on_prolog_ecall(F,A,fake,true),!,
  atom_concat(F,'_FaKe_Binding',FAKE),
  numbervars(Call,FAKE,0,_),
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
  functor_catch(Copy,F,A),
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


:-moo_hide_show_childs(hotrace/1).

:-moo_hide_show_childs(must/1).
must(C):- not(not(is_deterministic(C))),!,must_det(C).
must(C):- debugOnError(one_must(C,(rtrace(C),debugCallWhy(failed(must(C)),C)))).

must_each(List):-var(List),trace_or_throw(var_must_each(List)).
must_each([List]):-!,must(List).
must_each([E|List]):-!,must(E),must_each0(List).
must_each0(List):-var(List),trace_or_throw(var_must_each(List)).
must_each0([]):-!.
must_each0([E|List]):-E,must_each0(List).

:-moo_hide_show_childs(one_must/2).
% now using gensym counter instead of findall (since findall can make tracing difficult)

one_must(C1,C2,C3):-one_must(C1,one_must(C2,C3)).

one_must(Call,OnFail):- is_deterministic(Call),!,one_must_det(Call,OnFail).

% better version I think but makes more tracing
one_must(Call,OnFail):- cnotrace((gensym(mustCounter,Sym),flag(Sym,_,0),!)), must_flag(Sym,Call,OnFail).

must_flag(Sym,Call,_NFail):-call(Call),flag(Sym,C,C+1).
must_flag(Sym,_All,OnFail):-flag(Sym,Old,0),!, Old==0, % if old > 0 we want to fail 
         call(OnFail).

%old findall version
must_findall(OneA,_Else):-copy_term(OneA,One),findall(One,call(One),OneL),[_|_]=OneL,!,member(OneA,OneL).
must_findall(_OneA,Else):-!,Else.

is_deterministic(once(V)):-var(V),trace_or_throw(is_deterministic(var_once(V))).
is_deterministic(M:G):-atom(M),!,is_deterministic(G).
is_deterministic(Atomic):-atomic(Atomic),!.
is_deterministic(Ground):-ground(Ground),!.
is_deterministic((_,Cut)):-Cut==!.
is_deterministic(_ = _).
is_deterministic(_ =@= _).
is_deterministic(_ =.. _).
is_deterministic(_ == _).
is_deterministic(_ \= _).
is_deterministic(_ \== _).
is_deterministic(atom(_)).
is_deterministic(compound(_)).
is_deterministic(findall(_,_,_)).
is_deterministic(functor(_,_,_)).
is_deterministic(functor_catch(_,_,_)).
is_deterministic(ground(_)).
is_deterministic(nonvar(_)).
is_deterministic(not(_)).
is_deterministic(once(_)).
is_deterministic(var(_)).
%is_deterministic(Call):-predicate_property(Call,nodebug),!.
%is_deterministic(Call):-predicate_property(Call,foreign),!.

must_det_l([]):-!.
must_det_l([C|List]):-!,must_det(C),!,must_det_l(List).
must_det_l(C):- !,must_det(C).

must_det(C):- one_must_det(C,debugCallWhy(failed(must_det(C)),C)).

one_must_det(Call,_OnFail):-Call,!.
one_must_det(_Call,OnFail):-OnFail,!.


randomVars(Term):- random(R), StartR is round('*'(R,1000000)), !,
 ignore(Start=StartR),
 numbervars(Term, Start, _End, [attvar(skip),functor_name('$VAR')]).

prolog_must_not(Call):-Call,!,trace,!,programmer_error(prolog_must_not(Call)).
prolog_must_not(_Call):-!.



dynamic_if_missing(F/A):-functor_catch(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):-dynamic([F/A]).



%%%retractall(E):- retractall(E),functor_catch(E,File,A),dynamic(File/A),!.

pp_listing(Pred):- functor_catch(Pred,File,A),functor_catch(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.


:- meta_predicate_transparent(with_assertions(+,^)).
with_assertions( [],Call):- !,Call.
with_assertions( [With|MORE],Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With,MORE),Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With;MORE),Call):- !,with_assertions(With,Call);with_assertions(MORE,Call).
with_assertions( -TL:With,Call):- !,with_no_assertions(TL:With,Call).
with_assertions( +TL:With,Call):- !,with_assertions(TL:With,Call).
with_assertions( -With,Call):- !,with_no_assertions(With,Call).
with_assertions( +With,Call):- !,with_assertions(With,Call).

with_assertions(THead,Call):- 
 must_det(to_thread_head(THead,M,_Head,H)),
   copy_term(H,  WithA), !, setup_call_cleanup(M:asserta(WithA),Call,logOnFailure(M:retract(WithA))).

:-meta_predicate_transparent(with_no_assertions(+,^)).
with_no_assertions(THead,Call):- !, with_assertions((THead:-!,fail),Call).
with_no_assertions(THead,Call):-
   must_det(to_thread_head(THead,M,Head,H)), 
   copy_term((Head :- (!,fail)),WithA), 
   setup_call_cleanup((asserta(WithA),must_det(clause(Head,TBODY))),Call,must_det(logOnFailure(retract((Head:-TBODY)));(functor(H,F,A),abolish(M:F/A),thread_local(M:F/A)))).

to_thread_head((H:-B),TL,(HO:-B),(HH:-B)):-to_thread_head(H,TL,HO,HH),!.
to_thread_head(thglobal:Head,thglobal,thglobal:Head,Head):- must_det((predicate_property(thglobal:Head,(dynamic)),not(predicate_property(thglobal:Head,(thread_local))))).
to_thread_head(TL:Head,TL,TL:Head,Head):-!, must_det(( predicate_property(TL:Head,(dynamic)),must_det(predicate_property(TL:Head,(thread_local))))),!.
to_thread_head(Head,thlocal,thlocal:Head,Head):- must_det(( predicate_property(thlocal:Head,(dynamic)),predicate_property(thlocal:Head,(thread_local)))),!.

:-thread_local(thlocal:dmsg_hidden/1).
:-meta_predicate_transparent(with_all_dmsg(?)).
with_all_dmsg(Call):-with_no_assertions(thlocal:dmsg_hidden(_),Call).
:-meta_predicate_transparent(with_no_dmsg(?)).
with_no_dmsg(Call):-with_assertions(thlocal:dmsg_hidden(_),Call).
with_no_dmsg(Shown,Call):-with_assertions(thlocal:dmsg_hidden(Shown),Call).

dmsg_hide(Term):-asserta(thlocal:dmsg_hidden(Term)).
dmsg_show(Term):-ignore(retract(thlocal:dmsg_hidden(Term))).
dmsg_showall(Term):-ignore(retractall(thlocal:dmsg_hidden(Term))).

% =================================================================================
% Utils
% =================================================================================

printPredCount(Msg,Pred,N1):- compound(Pred), debugOnFailureEach((arg(_,Pred,NG))),user:nonvar(NG),!,
  findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),dmsg(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor_catch(Pred,File,A),functor_catch(FA,File,A), predicate_property(FA,number_of_clauses(N1)),dmsg(num_clauses(Msg,File/A,N1)),!.



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
   ccatch((read_term(In,Term,[syntax_errors(error)|Options])),E,(dmsg(E),fail)),
   load_term(Term,[line_count(Lineno),file(PLNAME),stream(In)|Options]),
   Term==end_of_file,
   close(In).

load_term(E,_Options):- E == end_of_file, !.
load_term(Term,Options):-ccatch(load_term2(Term,Options),E,(dmsg(error(load_term(Term,Options,E))),throw_safe(E))).

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
unify_listing(FileMatch):-functor_catch(FileMatch,F,A),unify_listing(FileMatch,F,A),!.
unify_listing_header(FileMatch):-functor_catch(FileMatch,F,A),unify_listing_header(FileMatch,F,A),!.


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

contains_term(SearchThis,Find):-Find==SearchThis,!.
contains_term(SearchThis,Find):-compound(SearchThis),functor_catch(SearchThis,Func,_),(Func==Find;arg(_,SearchThis,Arg),contains_term(Arg,Find)).

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
indent_e(X):- ccatch((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).

% ===================================================================
% Lowlevel printng
% ===================================================================
fmt0(X,Y,Z):-ccatch((format(X,Y,Z),flush_output_safe(X)),E,dmsg(E)).
fmt0(X,Y):-ccatch((format(X,Y),flush_output_safe),E,dmsg(E)).
fmt0(X):-ccatch(text_to_string(X,S),_,fail),'format'('~w',[S]),!.
fmt0(X):- (atom(X) -> ccatch((format(X,[]),flush_output_safe),E,dmsg(E)) ; (term_to_message_string(X,M) -> 'format'('~q~n',[M]);fmt_or_pp(X))).
fmt(X):-fresh_line,fmt0(X).
fmt(X,Y):-fresh_line,fmt0(X,Y),!.
fmt(X,Y,Z):- fmt0(X,Y,Z),!.


fmt_portray_clause(X):- unnumbervars(X,Y),!,numbervars(Y), portray_clause(Y).
fmt_or_pp(pp((X:-Y))):-!,fmt_portray_clause((X:-Y)),!.
fmt_or_pp(pp(X)):-!,functor_catch(X,F,A),fmt_portray_clause((pp(F,A):-X)),!.
fmt_or_pp(X):-'format'('~q~n',[X]).

dfmt(X):- with_output_to_stream(user_error,fmt(X)).
dfmt(X,Y):- with_output_to_stream(user_error,fmt(X,Y)).

with_output_to_stream(Stream,Goal):-
    current_output(Saved),
   setup_call_cleanup(set_output(Stream),
         Goal,
         set_output(Saved)).

:-dynamic dmsg_log/3.

logger_property(todo,once,true).


:-debug(todo).

:- thread_local is_with_dmsg/1.

with_dmsg(Functor,Goal):-
   with_assertions(is_with_dmsg(Functor),Goal).

:-dynamic hook:dmsg_hook/1.
:-multifile hook:dmsg_hook/1.

dmsg(V):- is_with_dmsg(FP),!,FP=..FPL,append(FPL,[V],VVL),VV=..VVL,once(dmsg0(VV)).
dmsg(V):- once(dmsg0(V)).
dmsg0(_):- bugger_flag(opt_debug=off),!.
dmsg0(V):-var(V),!,dmsg0(dmsg_var(V)).

dmsg0(V):- cnotrace(dmsg1(V)),!, hotrace(doall(( hook:dmsg_hook(V)))).

dmsg1(warn(V)):- print_message(warning,V).
dmsg1(skip_dmsg(_)):-!.
dmsg1(C):-thlocal:dmsg_hidden(C),!.
dmsg1(C):-functor_catch(C,Topic,_),thlocal:dmsg_hidden(Topic),!.
dmsg1(C):-functor_catch(C,Topic,_),debugging(Topic,_True_or_False),!,logger_property(Topic,once,true),!,
      (dmsg_log(Topic,_Time,C) -> true ; ((get_time(Time),asserta(dmsg_log(todo,Time,C)),!,dmsg2(C)))).
dmsg1(C):-((copy_term(C,Stuff), snumbervars(Stuff),!,dmsg2(Stuff))).

dmsg2(T):-!,
	((
	if_prolog(swi,
		(prolog_current_frame(Frame),
		prolog_frame_attribute(Frame,level,Depth),!,
		Depth2 = (Depth-25))),
        with_output_to_stream(user_error,
        ((
	fmt('%',[]),!,
	indent_e(Depth2),!,
	fmt0(T)))))).

% never gets here due to cut above 
dmsg2([]):-!.
dmsg2(LF):-functor_catch(LF,F,_),loggerReFmt(F,LR), ((LR==F,is_stream(F))->loggerFmtReal(F,LF,[]);dmsg(LR,LF,[])),!.
dmsg2([A|L]):-!,dmsg('% ~q~n',[[A|L]]).
dmsg2(Comp):-bugger:evil_term(_,Comp,Comp2),!,dmsg('% ~q~n',[Comp2]).
dmsg2(Stuff):-!,dmsg('% ~q~n',[Stuff]).


dmsg(L,F,A):-loggerReFmt(L,LR),loggerFmtReal(LR,F,A).

dmsg(F,A):-is_list(A),!,
    nl(user_error),
    fmt(user_error,F,A),
    nl(user_error),
    flush_output_safe(user_error),!.

dmsg(C,T):-!,
	((
	fmt('<font size=+1 color=~w>',[C]),
	fmt(T),
    fmt('</font>',[]))),!.


%dmsg(C,T):- isDebugOption(opt_debug=off),!.
dmsg(L,F):-loggerReFmt(L,LR),loggerFmtReal(LR,F,[]).
dmsg(_,F):-F==[-1];F==[[-1]].


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
:-moo_hide_childs(stack_check/1).
:-moo_hide_childs(stack_check/2).
stack_depth(Level):-once((prolog_current_frame(Frame),prolog_frame_attribute(Frame,level,Level))).
stack_check(BreakIfOver):- stack_check_else(BreakIfOver, trace_or_throw(stack_check(BreakIfOver))).
stack_check(BreakIfOver,Error):- stack_check_else(BreakIfOver, trace_or_throw(stack_check(BreakIfOver,Error))).
stack_check_else(BreakIfOver,Call):- stack_depth(Level) ,  ( Level < BreakIfOver -> true ; (dbgsubst(Call,stack_lvl,Level,NewCall),NewCall)).

% dumpstack_arguments.
dumpST:-cnotrace(dumpST([max_depth(5000),numbervars(safe),show([level,goal,clause])])).

dumpST(_):-bugger_flag(opt_debug=off),!.
dumpST(Opts):- prolog_current_frame(Frame),dumpST(Frame,Opts).

dumpST(Frame,MaxDepth):-integer(MaxDepth),!,dumpST(Frame,[max_depth(MaxDepth)]).
dumpST(Frame,Opts):-var(Opts),!,dumpST(Frame,5000).
dumpST(Frame,Opts):-is_list(Opts),!,dumpST(1,Frame,Opts).
dumpST(Frame,Opts):-dumpST(1,Frame,[Opts]).

get_m_opt(Opts,Max_depth,D100,RetVal):-E=..[Max_depth,V],(((member(E,Opts),nonvar(V)))->RetVal=V;RetVal=D100).

dumpST(N,Frame,Opts):-
   dumpST(N,Frame,Opts,Out),
   get_m_opt(Opts,numbervars,-1,Start),
   neg1_numbervars(Out,Start,ROut),
   reverse(ROut,RROut),
   ignore((forall(member(E,RROut),fdmsg(E)))).

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
dumpST(N,Frame,Opts,[fr(Goal)|MORE]):- get_m_opt(Opts,show,goal,Attr),getPFA(Frame,Attr,Goal),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST(N,Frame,Opts,[nf(no(Attr),N,Frame,Opts)|MORE]):- get_m_opt(Opts,show,goal,Attr),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST(N,Frame,Opts,[nf(noFrame(N,Frame,Opts))]).

dumpST_Parent(N,Frame,Opts,More):- prolog_frame_attribute(Frame,parent,ParentFrame), NN is N +1,dumpST(NN,ParentFrame,Opts,More),!.
dumpST_Parent(N,Frame,Opts,[nf(noParent(N,Frame,Opts))]).



getPFA(Frame,[L|List],Goal):- !,findall(R, (member(A,[L|List]),getPFA1(Frame,A,R)) ,Goal).
getPFA(Frame,Attr,Goal):-getPFA1(Frame,Attr,Goal).

getPFA1(_Frame,txt(Txt),txt(Txt)):-!.
getPFA1(Frame,clause,Goal):-getPFA2(Frame,clause,ClRef),clauseST(ClRef,Goal),!.
getPFA1(Frame,Attr,Attr=Goal):-getPFA2(Frame,Attr,Goal),!.
getPFA1(_,Attr,no(Attr)).

getPFA2(Frame,Attr,Goal):- ccatch((prolog_frame_attribute(Frame,Attr,Goal)),E,Goal=[error(Attr,E)]),!.

clauseST(ClRef,clause=Goal):- findall(V,(member(Prop,[file(V),line_count(V)]),clause_property(ClRef,Prop)),Goal).

clauseST(ClRef,Goal = HB):- ignore(((clause(Head, Body, ClRef),copy_term(((Head :- Body)),HB)))),
   numbervars(HB,0,_),
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

safe_numbervars(E,EE):-duplicate_term(E,EE),safe_numbervars(EE),get_gtime(G),numbervars(EE,'$VAR',G,[attvar(skip)]),
   term_variables(EE,AttVars),forall(member(V,AttVars),(copy_term(V,VC,Gs),V='$VAR'(VC=Gs))).
   

get_gtime(G):- get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G).

safe_numbervars(X):-get_gtime(G),safe_numbervars(X,'$VAR',G,_).

safe_numbervars(Copy,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).
safe_numbervars(Copy,_,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).

unnumbervars(X,Y):-with_output_to(atom(A),write_term(X,[numbervars(true),quoted(true)])),atom_to_term(A,Y,_),!.

renumbervars(X,X):-ground(X),!.
renumbervars(X,Z):-unnumbervars(X,Y),safe_numbervars(Y,Z).
renumbervars(Y,Z):-safe_numbervars(Y,Z).

withFormatter(Lang,From,Vars,SForm):-formatter_hook(Lang,From,Vars,SForm),!.
withFormatter(_Lang,From,_Vars,SForm):-sformat(SForm,'~w',[From]).

flush_output_safe:-ignore(ccatch(flush_output,_,true)).
flush_output_safe(X):-ignore(ccatch(flush_output(X),_,true)).

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
    ccatch((
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
os_to_prolog_filename(OS,PL):-absolute_file_name(OS,OSP),OS \= OSP,!,os_to_prolog_filename(OSP,PL).


% =================================================================================
% Utils
% =================================================================================
% test_call(G):-writeln(G),ignore(once(ccatch(G,E,writeln(E)))).

nop(_).

debugFmtList(ListI):-cnotrace((copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),dmsg(List0))),!.
debugFmtList0([],[]):-!.
debugFmtList0([A|ListA],[B|ListB]):-debugFmtList1(A,B),!,debugFmtList0(ListA,ListB),!.

debugFmtList1(Value,Value):-var(Value),!.
debugFmtList1(Name=Number,Name=Number):-number(Number).
debugFmtList1(Name=Value,Name=Value):-var(Value),!.
debugFmtList1(Name=Value,Name=(len:Len)):-copy_term(Value,ValueO),append(ValueO,[],ValueO),is_list(ValueO),length(ValueO,Len),!.
debugFmtList1(Name=Value,Name=(F:A)):-functor_catch(Value,F,A).
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
hotrace(X):- tracing,!,notrace,call_cleanup(X,trace).
hotrace(X):- call(X).

traceafter_call(X):- call_cleanup(restore_trace((leash(-all),visible(-all),X)),(leash(+call), trace)).

/*
% :- meta_predicate_transparent notrace_call(^).

notrace_call(X):-cnotrace,ccatch(traceafter_call(X),E,(dmsg(E-X),trace,throw(E))).
traceafter_call(X):-X,trace.
traceafter_call(_):-tracing,fail.
traceafter_call(_):-trace,fail.
*/



%getWordTokens(WORDS,TOKENS):-concat_atom(TOKENS,' ',WORDS).
%is_string(S):-string(S).



 %:-interactor.

export_all_preds:-source_location(File,_Line),module_property(M,file(File)),!,export_all_preds(M).

export_all_preds(ModuleName):-forall(current_predicate(ModuleName:F/A),
                   ((export(F/A),functor_catch(P,F,A),moo_hide(ModuleName:P)))).








%%:-user:(forall(current_predicate(bugger:FA),bugger:moo_hide(bugger:FA))).
% hide this module from tracing
%%:-user:(forall(current_predicate(logicmoo_util_strings:FA),bugger:moo_hide(logicmoo_util_strings:FA))).

module_notrace(M):- forall(predicate_property(P,imported_from(M)),bugger:moo_hide(M:P)).


% =====================================================================================================================
:-export((call_no_cuts/1)).
% =====================================================================================================================
:- module_transparent call_no_cuts/1.
:- meta_predicate_transparent call_no_cuts(0).
call_no_cuts(CALL):-clause_safe(CALL,TEST),call_no_cuts_0(TEST).

call_no_cuts_0(true):-!.
call_no_cuts_0((!)):-!.
call_no_cuts_0((A,B)):-!,call_no_cuts_0(A),call_no_cuts_0(B).
call_no_cuts_0((A;B)):-!,call_no_cuts_0(A);call_no_cuts_0(B).
call_no_cuts_0(C):-call(C).

% =====================================================================================================================
:-export((call_tabled/1)).
% =====================================================================================================================
:- meta_predicate_transparent call_tabled(^).
:- module_transparent call_tabled/1.

:- meta_predicate_transparent call_vars_tabled(?,^).
:- module_transparent call_vars_tabled/2.

:- meta_predicate_transparent call_setof_tabled(?,^,-).
:- module_transparent call_setof_tabled/3.

:- dynamic(call_tabled_list/2).

:- meta_predicate_transparent(make_key(*,-)).
make_key(CC,CC):- notrace(ground(CC)->Key=CC ;(copy_term(CC,Key),numbervars(Key,'$VAR',0,_))),!.

expire_tabled_list(T):- atoms_of(T,A1), CT= call_tabled_list(Key,List),doall(((CT,once(any_term_overlap_atoms_of(A1,List);(not(member(Key,List)),any_term_overlap_atoms_of(A1,Key))),retractall(CT)))).

any_term_overlap_atoms_of(A1,T2):-atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

any_term_overlap(T1,T2):- atoms_of(T1,A1),atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

call_tabled(setof(Vars,C,List)):- !,call_setof_tabled(Vars,C,List).
call_tabled(findall(Vars,C,List)):- !,call_setof_tabled(Vars,C,List).
call_tabled(C):- must_det(nonvar(C)), term_variables(C,Vars),!,call_vars_tabled(Vars,C).

call_vars_tabled(Vars,C):- call_setof_tabled(Vars,C,Set),!,member(Vars,Set).

call_setof_tabled(Vars,C,List):- make_key(Vars+C,Key),call_tabled0(Key,Vars,C,List).

call_tabled0(Key,_,_,List):- call_tabled_list(Key,List),!.
call_tabled0(Key,Vars,C,List):- really_can_table,!, no_loop_check(setof(Vars,C,List)),!,asserta_if_ground(call_tabled_list(Key,List)),!.
call_tabled0(Key,Vars,C,List):- no_loop_check(setof(Vars,C,List)),!,asserta_if_ground(call_tabled_list(Key,List)),!.

really_can_table:-not(thlocal:cannot_table),thlocal:can_table.




% asserta_if_ground(_):- !.
asserta_if_ground(G):- ground(G),asserta(G),!.
asserta_if_ground(_).


% =====================================================================================================================
:- module_notrace(bugger).
% =====================================================================================================================
:- module_notrace(logicmoo_util_strings).
% =====================================================================================================================

:-source_location(File,_Line),module_property(M,file(File)),!,forall(current_predicate(M:F/A),moo_hide_show_childs(M,F,A)).

:-moo_hide_show_childs(bugger,prolog_ecall_fa,5).
:-moo_hide_show_childs(bugger,prolog_ecall,3).
:-moo_hide_show_childs(bugger,must,1).
:-moo_hide_show_childs(bugger,must,2).
:-moo_hide_show_childs(bugger,must_flag,3).


% bugger_debug=off turns off just debugging about the debugger
% opt_debug=off turns off all the rest of debugging
bdmsg(_):-bugger_flag(bugger_debug=off),!.
bdmsg(_):-!.
bdmsg(D):-once(dmsg(D)).

bugger_term_expansion(_,_):-!,fail.
bugger_term_expansion(T,T3):- compound(T), once(bugger_t_expansion(T,T2)),T\=T2,!,ccatch(expand_term(T2,T3),_,fail).

% though maybe dumptrace
default_dumptrace(notrace(trace)).

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



show_and_do(C):-dmsg(C),!,C.
dtrace(C):-dtrace,C.

%dtrace:- skipWrapper,!,dmsg(dtrace_skipWrapper).
dtrace:-tracing,!,write(dtrace),nl,notrace,leash(+call),dtrace.
dtrace:-write(dtrace),nl,has_auto_trace(C),!,C.
dtrace:-repeat,dumptrace,!.

dumptrace:-tracing,!,leash(+call).
dumptrace:- notrace((fmt(in_dumptrace),leash(+exception),get_single_char(C))),dumptrace(C).
dumptrace(0'g):-notrace(dumpST),!,fail.
dumptrace(0'l):-notrace((ggtrace,!,trace)).
dumptrace(0'b):-prolog,!,fail.
dumptrace(0't):-trace,!.
dumptrace(0't):-show_and_do(grtrace(trace)).
dumptrace(0'f):-show_and_do(gftrace(true)).
dumptrace(10):-dumptrace_ret,!.
dumptrace(13):-dumptrace_ret,!.
dumptrace(C):-dmsg(unused_keypress(C)),!,fail.

dumptrace_ret:-leash(+call),trace.

restore_trace(Goal):-  tracing, notrace,!,'$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,(('$leash'(_, Old),'$visible'(_, OldV),trace),trace)).
restore_trace(Goal):-  '$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,((notrace,'$leash'(_, Old),'$visible'(_, OldV)))).

rtrace(Goal):- restore_trace((
   cnotrace((visible(+all),visible(+unify),visible(+exception),
   leash(-all),leash(+exception))),trace,Goal)).

ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   leash(-all),leash(+exception),trace,Goal)).

bugger_t_expansion(T,T):-not(compound(T)),!.
bugger_t_expansion(T,AA):- T=..[F,A],unwrappabe(F),bdmsg(bugger_term_expansion(T)),bugger_t_expansion(A,AA),!.
bugger_t_expansion([F0|ARGS0],[F1|ARGS1]):-bugger_t_expansion(F0,F1),bugger_t_expansion(ARGS0,ARGS1).
bugger_t_expansion(T,TT):- T=..[F|ARGS0],bugger_t_expansion(ARGS0,ARGS1), TT=..[F|ARGS1].

unwrappabe(F):-member(F,['debugOnError',debugOnError0]),!,fail.
unwrappabe(F):-member(FF,['OnError','OnFailure','LeastOne','Ignore','must']),atom_concat(_,FF,F),!.



bugger_goal_expansion(T,_):- bdmsg(bugger_goal_expansion(T)),fail.

bugger_expand_goal(T,_):- bdmsg(bugger_expand_goal(T)),fail.

bugger_expand_term(T,_):- bdmsg(bugger_expand_term(T)),fail.



% user:     expand_goal(G,G2):- compound(G),bugger_expand_goal(G,G2),!.


% user:goal_expansion(G,G2):- compound(G),bugger_goal_expansion(G,G2).

% user:expand_term(G,G2):- compound(G),bugger_expand_term(G,G2),!.

% user:term_expansion((H:-G),(H:-G2)):- bugger_term_expansion(G,G2).

module_predicate(ModuleName,F,A):-current_predicate(ModuleName:F/A),functor_catch(P,F,A),
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

arg_is_transparent(Arg):- member(Arg,[':','^']).
arg_is_transparent(0).
arg_is_transparent(Arg):- number(Arg).

% make meta_predicate_transparent's module_transparent
module_meta_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_catch(P,F,A)), 
      ignore(((predicate_property(ModuleName:P,(meta_predicate( P ))),
            not(predicate_property(ModuleName:P,(transparent))), arg(_,P,Arg),arg_is_transparent(Arg)),
                   (dmsg(todo(module_transparent(ModuleName:F/A))),
                   (module_transparent(ModuleName:F/A)))))).


:- module_predicates_are_exported(bugger).
:- module_meta_predicates_are_transparent(bugger).

:-module_property(bugger, exports(List)),moo_hide_show_childs(List).

% bugger_prolog_exception_hook(error(syntax_error(operator_expected),_),_,_,_).
bugger_prolog_exception_hook(Info,_,_,_):- bugger_error_info(Info),!, dumpST,dmsg(prolog_exception_hook(Info)), dtrace.

bugger_error_info(C):-contains_var(instantiation_error,C).
bugger_error_info(C):-contains_var(existence_error(procedure,s/0),C).


% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- user_use_module(library(prolog_stack)).

user:prolog_exception_hook(A,B,C,D):-once(copy_term(A,AA)),ccatch(( once(bugger_prolog_exception_hook(AA,B,C,D))),_,fail),fail.

:-'$syspreds':'$hide'('$syspreds':leash/1).
:-'$syspreds':'$hide'(leash/1).
:-'$syspreds':'$hide'('$syspreds':visible/1).
:-'$syspreds':'$hide'(visible/1).
% :-'$set_predicate_attribute'(!, trace, 1).

/** <module> dbase_i_mpred_pfc
% Provides a prolog database replacent that uses PFC
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure
%
*/

:- include(dbase_i_header).

:- ignore((current_predicate(not/1),abolish(system:not/1),abolish(not/1),dynamic(not/1),assert(((not(P):- nonvar(P), \+ P))),meta_predicate(not(0)))).
:- op(0,fx,'decl_mpred_pfc').
decl_mpred_pfc(F/A):-!,export(F/A),dynamic(F/A),asserta_if_new(user:mpred_prop(prologOnly)),asserta_if_new(prolog_arity(F,A)),asserta_if_new(user:mpred_prop(F,isPfcMeta)).
decl_mpred_pfc(F):-atom(F),!,decl_mpred_pfc(F/0).
:- op(1150,fx,'decl_mpred_pfc').

has_numvars(P):-term_variables(P,PV),unnumbervars(P,UP),term_variables(UP,UPV),!,UPV\==[],PV\=@=UPV.

must_numvars(P):- must(ground(P);has_numvars(P)),!.
must_no_numvars(P):-must(not(has_numvars(P))),!.

user:decl_database_hook(Op,Hook):- loop_check(pfc_provide_mpred_storage_op(Op,Hook)).

is_retract_first(one).
is_retract_first(a).

pfc_provide_mpred_storage_op(Op,(I1,I2)):-!,pfc_provide_mpred_storage_op(Op,I1),pfc_provide_mpred_storage_op(Op,I2).
pfc_provide_mpred_storage_op(Op,(=>(P))):-!,pfc_provide_mpred_storage_op(Op,P).
pfc_provide_mpred_storage_op(change(assert,_AorZ),Fact):- loop_check(pfcAddPreTermExpansion(Fact)).
% pfcRem to just get the first
pfc_provide_mpred_storage_op(change(retract,OneOrA),FactOrRule):- is_retract_first(OneOrA),!,loop_check(pfcRem(FactOrRule)),ignore((ground(FactOrRule),pfcRem2(FactOrRule))). 
% pfcRem2 should be forcefull enough
pfc_provide_mpred_storage_op(change(retract,all),FactOrRule):- loop_check(pfcRem2(FactOrRule)),!.
pfc_provide_mpred_storage_op(is_asserted,FactOrRule):- nonvar(FactOrRule),!,loop_check(pfc_clause(FactOrRule)).

pfc_clause_is_asserted(H,B):- var(H),nonvar(B),!,fail.
pfc_clause_is_asserted(H,B):- one_must(pfc_clause_db_unify(H,B),pfc_clause_is_asserted_hb_nonunify(H,B)).
pfc_clause_is_asserted(H,B,Ref):-pfc_clause_db_ref(H,B,Ref).

pfc_clause_is_asserted_hb_nonunify(G, T   ):- T==true,notrace(pfcRuleOutcomeHeadBody(G,H,B)),G\=@=H,!,pfc_clause_is_asserted(H,B).
pfc_clause_is_asserted_hb_nonunify(H,(T,B)):- T==true,!,pfc_clause_is_asserted_hb_nonunify(H,B).
pfc_clause_is_asserted_hb_nonunify(H,(B,T)):- T==true,!,pfc_clause_is_asserted_hb_nonunify(H,B).
pfc_clause_is_asserted_hb_nonunify(H,B):- pfc_clause_db_unify( =>( B , H) , true).
pfc_clause_is_asserted_hb_nonunify(H,B):- pfc_clause_db_unify( <=( H , B) , true).
pfc_clause_is_asserted_hb_nonunify(H,B):-pfc_mpred_storage_clauses(_,H,B,_).


pfcDatabaseGoal(G):-compound(G),functor(G,F,A),pfcDatabaseTerm(F/A).

user:provide_mpred_storage_clauses(Type,H,B,Proof):-pfc_mpred_storage_clauses(Type,H,B,Proof).

pfc_mpred_storage_clauses(forward(Type),('=>'(H)),B,Proof):- nonvar(H),!, user:provide_mpred_storage_clauses(Type,H,B,Proof).
pfc_mpred_storage_clauses(forward,H,B, ftProofFn(R)):- R=(=>(B,H)),clause(R,true).
pfc_mpred_storage_clauses(backward,H,B,ftProofFn(R)):- R=(<=(H,B)),clause(R,true).
pfc_mpred_storage_clauses(equiv,H,B,   ftProofFn(R)):- R=(<=>(LS,RS)),clause(R,true),(((LS=H,RS=B));((LS=B,RS=H))).
% pfc_mpred_storage_clauses(Type,H,true, ftProofFn(R)):-nonvar(H),!,pfcDatabaseTerm(F/A),functor(R,F,A),pfcRuleOutcomeHead(R,H),clause(R,true),pfcType(R,Type),Type\=rule.
% pfc_mpred_storage_clauses(Type,H,true, ftProofFn(R)):-pfcDatabaseTerm(F/A),functor(R,F,A),pfcType(R,Type),Type\=rule,clause(R,true),once(pfcRuleOutcomeHead(R,H)).

:-dynamic(pfcExpectedClauseCount_db/3).
pfcGetExpectedClauseCount(F,A,C):- (pfcExpectedClauseCount_db(F,A,C);C=0).
pfcGetActualClauseCount(F,A,C):-functor(P,F,A),predicate_property(P,number_of_clauses(C)).
pfcIsClauseCountWrong(F,A):-pfcGetExpectedClauseCount(F,A,E),pfcGetActualClauseCount(F,A,C),!,C\=E.
pfcCountsClauses(F,A):- mpred_arity(F,A),pfcWatches(F/A).

pfcCheckClauseCounts :- forall(pfcCountsClauses(F,A),pfcUpdateClauses(F,A)).

pfcUpdateClauses(F,A):-not(pfcIsClauseCountWrong(F,A)),!.
pfcUpdateClauses(F,A):-functor(P,F,A),forall((clause(P,T),is_true(T)),hooked_asserta(P)),retractall(pfcExpectedClauseCount_db(F,A,_)),
   predicate_property(P,number_of_clauses(C)),
   asserta(pfcExpectedClauseCount_db(F,A,C)).

:-onEachLoad(pfcCheckClauseCounts).

% :-asserta(thlocal:pfcExpansion).

:- thread_local thlocal:pfcExpansion/0.
:- dynamic thlocal:pfcExpansionWas.

maybe_hybrid(F/_):-user:mpred_prop(F,prologOnly),!.
maybe_hybrid(F/_):-user:mpred_prop(F,prologHybrid),!.
maybe_hybrid(F/_):-user:mpred_prop(F,isPfcMeta),!.
maybe_hybrid(F/_):-user:mpred_prop(F,X),atom(X),!.
maybe_hybrid(F/A):-atom(F),debugOnError(current_predicate(F/A)),!.
maybe_hybrid(_/A):-A=1,!.
% maybe_hybrid(C/1):-ignore((nonvar(C)->decl_mpred_hybrid(C/1);ignore(decl_mpred_hybrid(isa/2))))
maybe_hybrid(F/A):- current_predicate((decl_mpred_hybrid)/1), ignore(must((atom(F),decl_mpred_hybrid(F/A)))).

pfcDoConjs(Pred,List):-pfcDoConjs(Pred,List,[]).

pfcLambda([A1],Body,A1):-Body.
pfcLambda([A1,A2],Body,A1,A2):-Body.
pfcLambda([A1,A2,A3],Body,A1,A2,A3):-Body.
pfcLambda([A1,A2,A3,A4],Body,A1,A2,A3,A4):-Body.

:-call(pfcLambda([E],writeln(E)),hello_lambda).

pfcDoConjs(_,[],_) :- !.
pfcDoConjs(Pred,H,S):-var(H),!,apply(Pred,[H|S]).
pfcDoConjs(Pred,[H|T],S):-!, apply(Pred,[H|S]), pfcDoConjs(Pred,T,S).
pfcDoConjs(Pred,(H,T),S):-!, apply(Pred,[H|S]), pfcDoConjs(Pred,T,S).
pfcDoConjs(Pred,H,S):-apply(Pred,[H|S]).

deny_pfc_Permission_to_remove(pfcInternal,_,_):-!,fail. %allow
deny_pfc_Permission_to_remove(_,P,not(pfcControlled)):-get_functor(P,F,A), not(pfc_local(P,F,A);mpred_prop(F,pfcControlled)).

pfc_pre_expansion_each(X,X):-as_is_term(X),!.
pfc_pre_expansion_each(X,X):-not(compound((X))),!.
pfc_pre_expansion_each(X,isa(I,C)):- current_predicate(was_isa/3),was_isa(X,I,C),!,( \+ \+ maybe_hybrid(C/1)).
pfc_pre_expansion_each(Sent,OUT):-Sent=..[And|C12],current_predicate(is_logical_functor/1),is_logical_functor(And),!,maplist(pfc_pre_expansion_each,C12,O12),OUT=..[And|O12],!.
pfc_pre_expansion_each(C12,OUT):-is_list(C12),!,maplist(pfc_pre_expansion_each,C12,OUT),!.
pfc_pre_expansion_each(X,X):- \+ \+ ((get_functor(X,F,A),must(maybe_hybrid(F/A)))),!.

% {G}:-mpred_call(G).
user:mpred_arity(F,A):-pfcDatabaseTerm(F/A).
user:mpred_prop(F,argIsa(_,ftAskable)):-pfcDatabaseTerm(F/_).

:-dynamic p.
:-dynamic x.
:-decl_mpred_pfc q.
:-dynamic fly/1.
:-decl_mpred_pfc not/1.

:- decl_mpred_pfc pfc_local/1.
pfc_local(G):-get_functor(G,F,A),pfc_local(G,F,A).

pfc_local(G,_,_):-thglobal:pfcManageHybrids,!,pfcMarkC(G),!.
pfc_local(G,_,_):- not(current_predicate(hooked_assertz/1)),!,pfcMarkC(G).
pfc_local(_,F,A):-pfcDatabaseTerm(F/A),!.
pfc_local(_,F,_):-user:mpred_prop(F,isPfcMeta),!.
pfc_local(_,F,_):-user:mpred_prop(F,prologOnly).
pfc_local(_,F,_):-user:mpred_prop(F,prologHybrid),!,fail.


% pfc_local(_).

pfcControlled(G):-notrace(pfcControlled0(G)).
pfcWatches(G):-notrace(pfcWatched0(G)).

pfcControlled0(G):- get_functor(G,F), (user:mpred_prop(F,pfcControlled);user:mpred_prop(F,pfcMustFC)).
pfcWatched0(G):- get_functor(G,F), (user:mpred_prop(F,pfcWatched);user:mpred_prop(F,pfcControlled);user:mpred_prop(F,pfcMustFC)).

:-thread_local(thlocal:pfc_no_mark/0).



pfcMarkW(G):-pfcMarkAs(G,pfcWatched).
pfcMarkC(G):-pfcMarkAs(G,pfcControlled).
pfcMarkF(G):-pfcMarkAs(G,pfcMustFC).
pfcMarkB(G):-pfcMarkAs(G,pfcPreferBC).

pfcMarkAs(_, _):-thlocal:pfc_no_mark,!.
pfcMarkAs(G,As):-must(pfcDoConjs(pfcMarkAs1,G,[As])),!.

pfcMarkAs1(F,As):-atom(F),clause(user:mpred_prop(F,prologOnly),true),!,dmsg(todo(warn(wont_pfcMarkAs1(F,As)))).
pfcMarkAs1(F,As):-atom(F),!,assert_if_new(user:mpred_prop(F,As)).
pfcMarkAs1(G,_):-not(compound(G)),!.
pfcMarkAs1((G1,G2),As):-!,pfcMarkAs1(G1,As),pfcMarkAs1(G2,As).
pfcMarkAs1(G,As):-pfcDatabaseGoal(G),!,forall(pfcRuleOutcomeHeadBody(G,H,B),(pfcMarkAs(H,As),pfcMarkAs(B,pfcWatched))),!.
pfcMarkAs1(G,As):-get_functor(G,F,A),pfcMarkAs2(F,A,As).

pfcMarkAs2(F,A,As):-As==pfcControlled,dynamic_safe(F/A),fail.
pfcMarkAs2(F,A,As):-As==pfcMustFC,dynamic(F/A),fail.
pfcMarkAs2(F,A,As):-assert_if_new(user:mpred_arity(F,A)),pfcMarkAs1(F,As).

show_call_success_local(G):- call(G).

throw_on_bad_fact(assert(a,G)):-!,dtrace,pfc_asserta0(G).
throw_on_bad_fact(assert(z,G)):-!,dtrace,pfc_assertz0(G).
throw_on_bad_fact(assert(_,G)):-!,dtrace,pfc_assert0(G).
% by returning true we veto the assertion  (fail accepts assertion)
throw_on_bad_fact(G):-why_throw_on_bad_fact(G,Why), dmsg(error(trace_or_throw(throw_on_bad_fact(Why,G)))),!,fail.

why_throw_on_bad_fact(G,singletons(HS)):-   
  head_singletons_g(G,HS),get_functor(HS,F),!,not(mpred_prop(F,predCanHaveSingletons)).

head_singletons_g(G,HS):- pfcRuleOutcomeHeadBody(G,H,B), head_singletons_hb(H,B,HS),!.
  
head_singletons_hb(HN,BN,H):- unnumbervars(HN:BN,H:B),
  term_variables(H,HV),
  numbervars((H:B),66,_,[singletons(true)]),!,
    member('$VAR'('_'),HV).


pfc_retractall(Why,P) :- deny_pfc_Permission_to_remove(Why,P,Because),!, pfcWarn("Denying ~w retractall access to ~w because ~w",[Why,P,Because]),!.
pfc_retractall(_,G):-pfc_local(G),!,ignore((retract(G),fail)).
pfc_retractall(_,G):-hooked_retractall(G).

pfc_retract(Why,P) :- deny_pfc_Permission_to_remove(Why,P,Because),!, pfcWarn("Denying ~w retract access to ~w because ~w",[Why,P,Because]),!.
pfc_retract(_,pfcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).
pfc_retract(_,pfcPT(Key,Head,Body)) :-  
  % undo a positive trigger.
  %
  !,
  (retract(pfcPT(Key,Head,Body))
    -> unFc(pfcPT(Head,Body))
     ; pfcWarn("Trigger not found to pfc_retract: ~w",[pfcPT(Head,Body)])).
pfc_retract(_,G):- pfc_local(G),!,retract(G).
pfc_retract(_,G):- hooked_retract(G).

pfc_assertz(G):- unnumbervars(G,GG),pfc_assertz0(GG).
pfc_assertz0(G):- throw_on_bad_fact(G),!.
pfc_assertz0(G):- pfc_local(G),!,assertz(G).
pfc_assertz0(G):- must(not(pfc_local(G))), hooked_assertz(G),pfcMarkC(G).

pfc_asserta(G):- unnumbervars(G,GG),pfc_asserta0(GG).
pfc_asserta0(G):- throw_on_bad_fact(G),!.
pfc_asserta0(G):- pfc_local(G),!,asserta(G).
pfc_asserta0(G):- hooked_asserta(G),pfcMarkC(G).

pfc_assert(G):- unnumbervars(G,GG),pfc_assert0(GG).
pfc_assert0(G):- throw_on_bad_fact(G),!.
pfc_assert0(G):- pfc_local(G),!,assert(G).
pfc_assert0(G):- add(G),pfcMarkC(G).


pfc_clause_db_unify(H,B):- must(pfc_local(H)),
   (current_predicate(_,H) -> (predicate_property(H,number_of_clauses(_)) -> clause(H,B) ; B = call(H)); % simulates a body for system predicates
                                             B = mpred_call(H)).
pfc_clause_db_check(H,B):- copy_term(H:B,HH:BB), clause(HH,BB,Ref),clause(CH,CB,Ref),H:B=@=CH:CB,!.
pfc_clause_db_ref(H,B,Ref):-must(pfc_local(H)),!,pfc_clause_local_db_ref(H,B,Ref).

pfc_clause_local_db_ref(H,B,Ref):- copy_term(H:B,HH:BB),clause(HH,BB,Ref),clause(CH,CB,Ref),H:B=@=CH:CB,!.


pfc_call(G):- pfc_call(nonPFC,G).
pfc_call(_,G):- pfc_local(G),!,must(predicate_property(G,_)),!, debugOnError(call(G)).
pfc_call(Why,X):-dbase_op(call(Why),X).

:-thread_local ntd_max_depth/2.

% not_too_deep(_,G):-!,G.
not_too_deep(Key,G):-stack_depth(CD),
  (ntd_max_depth(Key,MD)->
      ( (CD > MD) -> (!,fail) ; G) ; 
    (MD is CD+200,call_cleanup(asserta(ntd_max_depth(Key,MD),REF),G,erase(REF)))).

% :- set_prolog_flag(unknown,fail).
:- decl_mpred_pfc(go/0).

pfcVersion(1.2).

% pfcFile('pfcsyntax').	% operator declarations.

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.



:- op(500,fx,'~').
:- op(1075,xfx,('=>')).
:- op(1075,xfx,'<=>').
:- op(1075,xfx,('<=')).
:- op(1100,fx,('=>')).
:- op(1150,xfx,('::::')).

:- op(1200,fx,(user:disabled)).
:- op(1200,fx,(user:enabled)).
:- user:op(1199,fx,(:-)).
:- user:op(1199,xfx,(:-)).



fwc:-true.
bwc:-true.

is_fc_body(P):- (fwc==P ; (compound(P),arg(_,P,E),is_fc_body(E))),!.
is_bc_body(P):- (bwc==P ; (compound(P),arg(_,P,E),is_bc_body(E))),!.


pfc_file_expansion(A,B) :- not(thlocal:into_form_code), pfc_file_expansion_each(A,B),
  (thlocal:pfcExpansion -> true; 
    (dmsg(warn_PfcWantedToExpand(A)),dmsg(warn_into(B)),!,fail)).
:-export(pfc_file_expansion/2).

pfc_file_expansion_each(':-'(_),_):-!,fail.
pfc_file_expansion_each((P=>Q),(:- pfcMarkF(Q),pfcAdd((P=>Q)))).
pfc_file_expansion_each(implies(P,Q),PQ):- !, pfc_file_expansion_each((P=>Q),PQ).
%pfc_file_expansion_each((P=>Q),(:- pfcAdd(('<='(Q,P))))).  % DO NOT USE speed-up attempt
pfc_file_expansion_each(('<='(P,Q)),(:- pfcMarkB(P),pfcAdd(('<='(P,Q))))).
pfc_file_expansion_each((P<=>Q),(:- pfcMarkC(P),pfcMarkC(Q),pfcAdd((P<=>Q)))).
pfc_file_expansion_each((RuleName :::: Rule),(:- pfcAdd((RuleName :::: Rule)))).
pfc_file_expansion_each((=>P),(:- pfcMarkF(P),pfcAdd((=>P)))):-nonvar(P).
pfc_file_expansion_each('fwc'((Q)),(:- pfcMarkF(Q),pfcAdd(=>Q))):-nonvar(Q).

pfc_file_expansion_each((disabled(Q):-P),(:- pfcRem(Q))):-P==true, nonvar(Q), (not(thlocal:pfcExpansion);pfcMarkC(Q)),!.
pfc_file_expansion_each((enabled(Q):-P),(:- pfcAdd(Q))):-P==true, nonvar(Q), (not(thlocal:pfcExpansion);pfcMarkC(Q)),!.

pfc_file_expansion_each((disabled(Q):-P),(disabled(Q):-P)):- nonvar(P),P\==true,nonvar(Q),(not(thlocal:pfcExpansion);pfcMarkC(Q)),!.
pfc_file_expansion_each((enabled(Q):-P), (:-(pfcMarkC(Q),pfcAdd(Q<=P)))):- nonvar(P),P\==true,nonvar(Q).

pfc_file_expansion_each(((Q:-P)),(:- (pfcMarkF(Q),pfcMarkC(P=>Q),pfcAdd(P=>Q)))):- pfcMustFC(Q),!.

pfc_file_expansion_each(((Q:-P)),(:- (pfcMarkF(Q),pfcMarkC(P=>Q),pfcAdd(P=>Q)))):- nonvar(P),P\==true,nonvar(Q),is_fc_body(P),!.
pfc_file_expansion_each(((Q:-P)),(:- (pfcMarkB(Q),pfcMarkC(Q),pfcAdd(Q<=P)))):- nonvar(P),P\==true,nonvar(Q),is_bc_body(P),!.

pfc_file_expansion_each(((Q:-P)),(:- pfcMarkB(Q),pfcAdd(Q<=P))):- nonvar(P),nonvar(Q),P\==true,not(is_fc_body(P)),pfcControlled(Q),!.
pfc_file_expansion_each(P,(:- pfcAdd(P))):- pfcMustFC(P),!.
pfc_file_expansion_each(P,(:- pfcAdd(P))):-pfcControlled(P),!.
pfc_file_expansion_each(((Q:-P)),(:- (pfcMarkB(Q),pfcMarkC(Q),pfcAdd((Q:-P))))):- nonvar(P),P\==true,nonvar(Q),pfcUseAllBC((Q:-P)).
pfc_file_expansion_each((Q,(:- (pfcMarkC(Q),pfcAdd(Q))))):- nonvar(Q),pfcUseAllFact(Q).

pfcMustUseFC(G):- once(pfcRuleOutcomeHeadBody(G,H,_)),H\=@=G,!,pfcMustUseFC(H).
pfcMustUseFC(G):- get_functor(G,F),not(user:mpred_prop(F,prologOnly)),user:mpred_prop(F,pfcMustFC).

pfcUseAllBC(((Q:-P))):-may_use_head(Q),no_head_singletons_hb(Q,P).
pfcUseAllFact(Q):-may_use_head(Q),no_head_singletons_hb(Q,true).

no_head_singletons_hb(Q,P):-not(((head_singletons_hb(Q,P,_),get_functor(Q,F,A),decl_mpred_prolog(F/A)))).

callBC(G):-loop_check(mpred_call(G)).

may_never_deduce_bc_change.

may_use_head(_):-may_never_deduce_bc_change,!,fail.
may_use_head(Q):-var(Q),!,fail.
may_use_head(_:_):-!,fail.
may_use_head(Q):-Q \= (F/A),!, get_functor(Q,F,A),!,may_use_head(F/A).
may_use_head(F/_):- atom_contains(F,'_'),!,fail.
may_use_head(F/_):- user:mpred_prop(F,prologOnly),!,fail.
may_use_head(F/_):- current_predicate(F/A),functor(G,F,A),real_builtin_predicate(G),!,fail.
may_use_head(F/A):- functor(G,F,A),real_builtin_predicate(G),!,fail.
may_use_head(_/1):-!,fail.
may_use_head(_/2).
% may_use_head(_/_).


pfcAddPreTermExpansion((I1,I2)):-!,pfcAddPreTermExpansion(I1),pfcAddPreTermExpansion(I2).
pfcAddPreTermExpansion(Info):-pfc_file_expansion_each(Info,What),!,What=(:-Call),show_call(must(call(Call))).


% pfcFile('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

:- use_module(library(lists)).

:- decl_mpred_pfc ('=>')/2.
:- decl_mpred_pfc ('::::')/2.
:- decl_mpred_pfc '<=>'/2.
:- decl_mpred_pfc '<='/2.
:- decl_mpred_pfc 'pfcPT'/2.
:- decl_mpred_pfc 'pfcNT'/3.
:- decl_mpred_pfc 'pfcBT'/2.
:- decl_mpred_pfc pfcUndoMethod/2.
:- decl_mpred_pfc pfcAction/2.

:- decl_mpred_pfc pfcSelect/1.
:- decl_mpred_pfc pfcDatabaseTerm/1.


%:- decl_mpred_pfc pfcTmsMode/1.
:- decl_mpred_pfc pfcQueue/1.
:- decl_mpred_pfc pfcDefault/1.

:- decl_mpred_pfc pfcDatabase/1.
:- decl_mpred_pfc pfcHaltSignal/0.
%:- decl_mpred_pfc pfcDebugging/0.
%:- decl_mpred_pfc pfcSearch/1.

%%= initialization of global assertons 

%= pfcDefaultSetting/1 initialized a global assertion.
%=  pfcDefaultSetting(P,Q) - if there is any fact unifying with P, then do 
%=  nothing, else pfc_assert Q.

pfcDefaultSetting(GeneralTerm,Default) :-
  clause(GeneralTerm,true) -> true ; assert(Default).

%= pfcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfcDefaultSetting(pfc_settings(tmsMode,_), pfc_settings(tmsMode,cycles)).

% Pfc Search strategy. pfc_settings(searchMode,X) where X is one of {direct,depth,breadth}
:- pfcDefaultSetting(pfc_settings(searchMode,_), pfc_settings(searchMode,direct)).

user:mpred_prop(isa,pfcMustFC).

pfcMustFC(H):-get_functor(H,F),mpred_prop(F,pfcMustFC).
pfcPreferBC(H):-get_functor(H,F,A),mpred_prop(F,pfcPreferBC),dynamic(F/A),functor(PHead,F,A),asserta_if_new((PHead:-callBC(PHead))).

pfcPreferedDir(H,B,(B=>H)):-pfcMustFC(H).
pfcPreferedDir(H,B,(H<=B)):-pfcPreferBC(H).


% 

%= add/2 and pfcPost/2 are the main ways to assert new clauses into the
%= database and have forward reasoning done.

%= pfcAdd(P,S) asserts P into the dataBase with support from S.

pfcAdd(isa(I,Not)):-Not==not,!,pfcAdd(not(I)),!.
pfcAdd(P) :- pfcAdd(P,(pcfUser,pcfUser)).


pfcAdd(P,S):-pfcDoConjs(pfcAdd0,P,[S]).

pfcAdd0(P,S):-cyc_to_pfc_expansion_entry(P,PE),!,pfcAdd1(PE,S),!.
pfcAdd0(P,S):-pfcAdd1(P,S),!.

pfcAdd1(P,S):-fully_expand(change(assert,add),P,PE)->(PE\=@=P,!,pfcAdd(PE,S)),!.
pfcAdd1(M:P0,S0) :- atom(M),!,pfcAdd0(P0,S0).
pfcAdd1(P:-B,S) :-is_true(B),!,pfcAdd0(P,S).
pfcAdd1(B=>P,S) :-is_true(B),!,pfcAdd0(P,S).
pfcAdd1(P<=B,S) :-is_true(B),!,pfcAdd0(P,S).
pfcAdd1((=>P),S) :- pfcAdd0(P,S).

pfcAdd1((H:-B),S) :-head_singletons_hb(H,B,_),!,pfcAdd2((H<=B),S).
pfcAdd1((B=>H),S) :-head_singletons_hb(H,B,_),!,pfcAdd2((H<=B),S).
pfcAdd1((H:-B),S) :-pfcPreferedDir(H,B,G),!,pfcAdd2(G,S).
pfcAdd1(P,S) :- pfcAdd2(P,S).

pfcAdd2(P0,S0) :- copy_term(P0:S0,P1:S1),
 must((pfc_pre_expansion_each(P1,P),pfc_pre_expansion_each(S1,S))),
  must(copy_term(P-S,P2-S2)),
  (\+ \+ pfcPost(P2,S2)),
  % must(variant(P:S,P2:S2)),
  pfcRun.

%pfcAdd2(_,_).
pfcAdd2(P,S) :- pfcWarn("pfcAdd(~w,~w) failed",[P,S]),!,fail.


% pfcPost(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) pfcPost1 is called. It always succeeds.

pfcPost(Each,S) :- pfcDoConjs(pfcPost1,Each,[S]).

% pfcPost1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

pfcPost1(P,S) :- 
  %= db pfcAddDbToHead(P,P2),
  % pfcRemoveOldVersion(P),
  copy_term(P,PC),
  must((
  \+ \+ pfcAddSupport(P,S),
  sanity(PC=@=P))),
  \+ \+ pfcUnique(P),
  sanity(PC=@=P),
  \+ \+ must(pfcAssertIfUnknown(P)), % was simply pfc_assert(P),
   (\+ \+ pfcTraceAdd(P,S)),
   !,
   \+ \+ pfcEnqueue(P,S),
   !.

pfcPost1(P,S) :- (\+ \+ pfcUnique(P)),pfcWarn("(maybe ERROR?!) pfcAdd(~w,~w) failed",[P,S]),!.
pfcPost1(_,_).
%%pfcPost1(P,S) :-  pfcWarn("pfcAdd(~w,~w) failed",[P,S]).


pfcRepropagate(P) :-
  forall(must(pfcGetSupport(P,S)), pfcRepropagate(P,S)).

pfcRepropagate(P,S) :-
  must(pfcAssertIfUnknown(P)), % was simply pfc_assert(P),
  pfcTraceAdd(P,S),
  !,
  pfcEnqueue(P,S),
  !.



%%
%= pfcAddDbToHead(+P,-NewP) talkes a fact P or a conditioned fact
%= (P:-C) and adds the Db context.
%%

pfcAddDbToHead(P,NewP) :-
  pfcCurrentDb(Db),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).


% pfcUnique(X) is true if there is no assertion X in the prolog db.

pfcUnique((Head:-Tail)) :- 
  !, 
  \+ pfc_clause_db_unify(Head,Tail).

pfcUnique(P) :-
  !,
  \+ pfc_clause_db_unify(P,true).


pfcEnqueue(P,S) :-
  pfc_settings(searchMode,Mode) 
    -> (Mode=direct  -> pfcFwd(P) ;
	Mode=depth   -> pfcAsserta(pfcQueue(P),S) ;
	Mode=breadth -> pfcAssert(pfcQueue(P),S) ;
	else         -> pfcWarn("Unrecognized pfcSearch mode: ~w", Mode))
     ; pfcWarn("No pfcSearch mode").


% if there is a rule of the form Identifier ::: Rule then delete it.

pfcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  var(Identifier),
  !,
  pfcWarn("variable used as an  rule name in ~w :::: ~w",
          [Identifier,Body]).

  
pfcRemoveOldVersion((Identifier::::Body)) :-
  nonvar(Identifier),
  pfc_clause_db_unify((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfcRem((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion(_).



% 

% pfcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfcQueue mechanism.

pfcRun :-
  (\+ pfc_settings(searchMode,direct)),
  pfcStep,
  pfcRun.
pfcRun.


% pfcStep removes one entry from the pfcQueue and reasons from it.


pfcStep :-  
  % if pfcHaltSignal is true, reset it and fail, thereby stopping inferencing.
  pfcRetractInternal(pfcHaltSignal),
  !, 
  fail.

pfcStep :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  pfcdo(pfcFwd(P)),
  !.

get_next_fact(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P) :- 
  pfcRetractInternal(pfcQueue(P)),
  pfcRemoveSupportsQuietly(pfcQueue(P)),
  !.
remove_selection(P) :-
  brake(format("~Npfc:get_next_fact - selected fact not on Queue: ~w",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the pcfUser defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(P) :- 
  pfcSelect(P),
  !.  
select_next_fact(P) :- 
  defaultpfcSelect(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultpfcSelect(P) :- pfcQueue(P),!.

% pfcHalt stops the forward chaining.
pfcHalt :-  pfcHalt("",[]).

pfcHalt(Format) :- pfcHalt(Format,[]).

pfcHalt(Format,Args) :- 
  format(Format,Args),
  pfcHaltSignal -> 
       pfcWarn("pfcHalt finds pfcHaltSignal already set")
     ; pfc_assert(pfcHaltSignal).


%%
%%
%= predicates for manipulating triggers
%%


pfcAddTrigger(pfcPT(Trigger,Body),Support) :-
  !,
  pfc_trace_msg('~n      Adding positive trigger ~q~n',
		[pfcPT(Trigger,Body)]),
  pfcAssert(pfcPT(Trigger,Body),Support),
  copy_term(pfcPT(Trigger,Body),Tcopy),
  pfcBC(Trigger),
  pfcEvalLHS(Body,(Trigger,Tcopy)),
  fail.


pfcAddTrigger(pfcNT(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('~n      Adding negative trigger: ~q~n       test: ~q~n       body: ~q~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfcAssert(pfcNT(TriggerCopy,Test,Body),Support),
  \+Test,
  pfcEvalLHS(Body,((\+Trigger),pfcNT(TriggerCopy,Test,Body))).

pfcAddTrigger(pfcBT(Trigger,Body),Support) :-
  !,
  pfcAssert(pfcBT(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body).

pfcAddTrigger(X,_Support) :-
  pfcWarn("Unrecognized trigger to pfcAddtrigger: ~w",[X]).


pfcBtPtCombine(Head,Body,Support) :- 
  %= a backward trigger (pfcBT) was just added with head and Body and support Support
  %= find any pfcPT's with unifying heads and add the instantied pfcBT body.
  pfcGetTriggerQuick(pfcPT(Head,_PtBody)),
  pfcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  pfc_clause_db_unify(Trigger,true).

pfcGetTrigger(Trigger):-pfcGetTriggerQuick(Trigger).

%%
%%
%= predicates for manipulating action traces.
%%

pfcAddActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  pfcAddSupport(pfcAction(Action),Support).

pfcRemActionTrace(pfcAction(A)) :-
  pfcUndoMethod(A,M),
  M,
  !.


%%
%= predicates to remove pfc facts, triggers, action traces, and queue items
%= from the database.
%%

pfcRetractInternal(X) :- 
  %= pfc_retract an arbitrary thing.
  pfcType(X,Type),
  pfcRetractTypeInternal(Type,X),
  !.

pfcRetractTypeInternal(fact,X) :-   
  %= db pfcAddDbToHead(X,X2), pfc_retract(pfcInternal,X2). 
  pfc_retract(pfcInternal,X).

pfcRetractTypeInternal(rule,X) :- 
  %= db  pfcAddDbToHead(X,X2),  pfc_retract(pfcInternal,X2).
  pfc_retract(pfcInternal,X).

pfcRetractTypeInternal(trigger,X) :- 
  pfc_retract(pfcInternal,X)
    -> unFc(X)
     ; pfcWarn("Trigger not found to pfc_retract: ~w",[X]).

pfcRetractTypeInternal(action,X) :- pfcRemActionTrace(X).
  

%= pfcAddSome(X) adds item X to some database

pfcAddSome(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  % pfc_call the appropriate predicate.
  pfcAddType(Type,X).

pfcAddType(fact,X) :- 
  pfcUnique(X), 
  pfc_assert(X),!.
pfcAddType(rule,X) :- 
  pfcUnique(X), 
  pfc_assert(X),!.
pfcAddType(trigger,X) :- 
  pfc_assert(X).
pfcAddType(action,_Action) :- !.


  

%= pfcRem(P,S) removes support S from P and checks to see if P is still supported.
%= If it is not, then the fact is retreactred from the database and any support
%= relationships it participated in removed.
pfcRem(P) :- 
  % pfcRem/1 is the pcfUser's interface - it withdraws pcfUser support for P.
  pfcDoConjs(pfcLambda([E],pfcRem(E,(pcfUser,pcfUser))),P).

pfcRem(P,S) :-
  % pfcDebug(format("~Nremoving support ~w from ~w",[S,P])),
  pfc_trace_msg('~n    Removing support: ~q from ~q~n',[S,P]),
  pfcRemSupport(P,S)
     -> pcfRemoveIfUnsupported(P)
      ; pfcWarn("pfcRem/2 Could not find support ~w to remove from fact ~w", [S,P]).

%%
%= pfcRem2 is like pfcRem, but if P is still in the DB after removing the
%= pcfUser's support, it is retracted by more forceful means (e.g. removeLiteralSupportsAndDependants/1).
%%

pfcRem2(P) :- 
  % pfcRem2/1 is the pcfUser's interface - it withdraws pcfUser support for P.
  pfcDoConjs(pfcLambda([E],pfcRem2(E,(pcfUser,pcfUser))),P).

pfcRem2(P,S) :-
  pfcRem(P,S),
  pfcBC(P)
     -> removeLiteralSupportsAndDependants(P) 
      ; true.

%%
%= removeLiteralSupportsAndDependants(+F) retracts fact F from the DB and removes any dependent facts */
%%

removeLiteralSupportsAndDependants(F) :- 
  pfcRemoveSupports(F),
  pfcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :- 
  pfcRemSupport(F,S),
  pfcWarn("~w was still supported by ~w",[F,S]),
  fail.
pfcRemoveSupports(_).

pfcRemoveSupportsQuietly(F) :- 
  pfcRemSupport(F,_),
  fail.
pfcRemoveSupportsQuietly(_).

% pfcUndo(X) undoes X.



pfcUndo(pfcNT(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (pfc_retract(pfcInternal,pfcNT(Head,Condition,Body))
    -> unFc(pfcNT(Head,Condition,Body))
     ; pfcWarn("Trigger not found to pfc_retract: ~w",[pfcNT(Head,Condition,Body)])).

pfcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  pfc_retract(pfcUndo,Fact),
  pfcTraceRem(Fact),
  unFc1(Fact).
  


%= unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
%= been removed from the database, so remove all support relations it
%= participates in and check the things that they support to see if they
%= should stayu in the database or should also be removed.


unFc(F) :- 
  pfcRetractSupportRelations(F),
  unFc1(F).

unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  pfcRun.


pfcUnFcCheckTriggers(F) :-
  pfcType(F,fact),
  copy_term(F,Fcopy),
  pfcNT(Fcopy,Condition,Action),
  (\+ Condition),
  pfcEvalLHS(Action,((\+F),pfcNT(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractSupportRelations(Fact) :-
  pfcType(Fact,Type),
  (Type=trigger -> pfcRemSupport(P,(_,Fact))
                ; pfcRemSupport(P,(Fact,_))),
  pcfRemoveIfUnsupported(P),
  fail.
pfcRetractSupportRelations(_).



%= pcfRemoveIfUnsupported(+P) checks to see if P is supported and removes
%= it from the DB if it is not.

pcfRemoveIfUnsupported(P) :- 
   pfcSupported(P) -> true ;  pfcUndo(P).


%= pfcSupported(+P) succeeds if P is "supported". What this means
%= depends on the TMS mode selected.

pfcSupported(P) :- 
  pfc_settings(tmsMode,Mode),
  pfcSupported(Mode,P).

pfcSupported(local,P) :- !, pfcGetSupport(P,_).
pfcSupported(cycles,P) :-  !, wellFounded(P).
pfcSupported(full,P) :-  !, wellFounded(P).
pfcSupported(_,_P) :- true.


%%
%= a fact is well founded if it is supported by the pcfUser
%= or by a set of facts and a rules, all of which are well founded.
%%

wellFounded(Fact) :- pfcWFF(Fact,[]).

pfcWFF(F,_) :-
  % supported by pcfUser (pfcAxiom) or an "absent" fact (assumption).
  (pfcAxiom(F) ; pfcAssumptionBase(F)),
  !.

pfcWFF(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supportsForWhy(F,Supporters),
  % all of whose members are well founded.
  pfcWFF_L(Supporters,[F|Descendants]),
  !.

%= pfcWFF_L(L) simply maps pfcWFF over the list.

pfcWFF_L([],_).
pfcWFF_L([X|Rest],L) :-
  pfcWFF(X,L),
  pfcWFF_L(Rest,L).


% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a pcfUser-defined fact are: [pcfUser].

supportsForWhy(F,[Fact|MoreFacts]) :-
  pfcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(pcfUser,[]) :- !.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).


%%
%%
%= pfcFwd(X) forward chains from a fact or a list of facts X.
%%
pfcFwd(X):-
  with_assertions(thlocal:pfc_no_mark,pfcDoConjs(pfcFwd1,X)).

%%
%= pfcFwd1(+P) forward chains for a single fact.
%%
pfcFwd1(Fact) :-
   pfcRuleOutcomeHead(Fact,Outcome),!,
    loop_check_term(pfcFwd1_newoutcome(Fact),Outcome,
      loop_check_term(pfcFwd1_newoutcome(Fact),Outcome,
       dmsg(looped_pfcRuleOutcomeHead(Fact,Outcome)))).

pfcRuleOutcomeHead(Outcome,OutcomeO):-var(Outcome),!,OutcomeO=Outcome.
pfcRuleOutcomeHead(_=>Outcome,OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(Outcome<=_,OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(Outcome<=>_,OutcomeO):-pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(_<=>Outcome,OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(_::::Outcome,OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcBT(Outcome,_),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcNT(_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcNT(_,_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcPT(_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcPT(_,_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(support1(Outcome,_,_),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(support2(_,_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(support3(_,Outcome,_),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcQueue(Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcDefault(Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(Outcome:-_,Outcome):-!.
pfcRuleOutcomeHead(Outcome,Outcome).

pfcRuleOutcomeHeadBody(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
pfcRuleOutcomeHeadBody(Ante1=>Outcome,OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(Outcome<=Ante1,OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(Outcome<=>Ante1,OutcomeO,(Ante1,Ante2)):-pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(Ante1<=>Outcome,OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(_::::Outcome,OutcomeO,Ante2):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(pfcBT(Outcome,Ante1),OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(pfcPT(Ante1,Outcome),OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(pfcPT(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(pfcNT(Ante1,Outcome),OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(pfcNT(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(support1(Outcome,Ante1a,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(support2(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(support3(Ante1a,Outcome,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(pfcQueue(Outcome),OutcomeO,Ante2):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody(pfcDefault(Outcome),OutcomeO,Ante2):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody((Outcome:-Ante),Outcome,Ante):-!.
pfcRuleOutcomeHeadBody(Outcome,Outcome,true).

pfcFwd1_newoutcome(Fact) :-
  fc_rule_check(Fact),
  copy_term(Fact,F),
  % check positive triggers
  pfcRunPT(Fact,F),
  % check negative triggers
  pfcRunNT(Fact,F).


%%
%= fc_rule_check(P) does some special, built in forward chaining if P is 
%= a rule.
%= 

fc_rule_check((P=>Q)) :-  
  !,  
  pfcProcessRule(P,Q,(P=>Q)).
fc_rule_check((Name::::P=>Q)) :- 
  !,  
  pfcProcessRule(P,Q,(Name::::P=>Q)).
fc_rule_check((P<=>Q)) :- 
  !, 
  pfcProcessRule(P,Q,(P<=>Q)), 
  pfcProcessRule(Q,P,(P<=>Q)).
fc_rule_check((Name :::: P <=> Q)) :- 
  !, 
  pfcProcessRule(P,Q,((Name::::P<=>Q))), 
  pfcProcessRule(Q,P,((Name::::P<=>Q))).

fc_rule_check(('<='(P,Q))) :-
  !,
  pfcDefineBcRule(P,Q,('<='(P,Q))).

fc_rule_check(_).


pfcRunPT(Fact,F) :- 
  pfcGetTriggerQuick(pfcPT(F,Body)),
  pfc_trace_msg('~n      Found positive trigger: ~q~n       body: ~q~n',
		[F,Body]),
  not_too_deep(pfcRunPT, pfcEvalLHS(Body,(Fact,pfcPT(F,Body)))),
  fail.

%pfcRunPT(Fact,F) :- 
%  pfcGetTriggerQuick(pfcPT(presently(F),Body)),
%  pfcEvalLHS(Body,(presently(Fact),pfcPT(presently(F),Body))),
%  fail.

pfcRunPT(_,_).

pfcRunNT(_Fact,F) :-
  support3(pfcNT(F,Condition,Body),X,_),
  Condition,
  pfcRem(X,(_,pfcNT(F,Condition,Body))),
  fail.
pfcRunNT(_,_).


%%
%= pfcDefineBcRule(+Head,+Body,+ParentRule) - defines a backeard
%= chaining rule and adds the corresponding pfcBT triggers to the database.
%%

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfcLiteral(Head)),
  pfcWarn("Malformed backward chaining rule.  ~w not atomic.",[Head]),
  pfcWarn("rule: ~w",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfcBuildRhs(Head,Rhs),
  pfcForEach(pfc_nf(Body,Lhs),
          (pfcBuildTrigger(Lhs,rhs(Rhs),Trigger),
           pfcAdd(pfcBT(Head,Trigger),(ParentRuleCopy,pcfUser)))).
 


%%
%%
%= eval something on the LHS of a rule.
%%

 
pfcEvalLHS((Test->Body),Support) :-  
  !, 
  (pfc_call(pfcTest,Test) -> pfcEvalLHS(Body,Support)),
  !.

pfcEvalLHS(rhs(X),Support) :-
  !,
  pfc_eval_rhs(X,Support),
  !.

pfcEvalLHS(X,Support) :-
  pfcType(X,trigger),
  !,
  pfcAddTrigger(X,Support),
  !.

%pfcEvalLHS(snip(X),Support) :- 
%  snip(Support),
%  pfcEvalLHS(X,Support).

pfcEvalLHS(X,_) :-
  pfcWarn("Unrecognized item found in trigger body, namely ~w.",[X]).


%%
%= eval something on the RHS of a rule.
%%

pfc_eval_rhs([],_) :- !.
pfc_eval_rhs([Head|Tail],Support) :- 
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs(Tail,Support).


pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 pfcEvalAction(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfcNegatedLiteral(P),
 !,
 pfcRem(P).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs([X|Xrest],Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 pfcPost1(Assertion,Support).


pfc_eval_rhs1(X,_) :-
  pfcWarn("Malformed rhs of a rule: ~w",[X]).


%%
%= evaluate an action found on the rhs of a rule.
%%

pfcEvalAction(Action,Support) :-
  pfc_call(pfcEval,Action), 
  (pfcUndoable(Action) 
     -> pfcAddActionTrace(Action,Support) 
      ; true).


%%
%= 
%%

pfc_trigger_the_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
pfc_trigger_the_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfcBC(Trigger),
%  pfcEvalLHS(Body,(presently(Trigger),pfcPT(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfcBC(Trigger),
  pfcEvalLHS(Body,(Trigger,pfcPT(TriggerCopy,Body))),
  fail.


%%
%= pfcBC(F) is true iff F is a fact available for forward (backward?) chaining.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%%

pfcBC(P) :-
  % trigger any bc rules.
  pfcBT(P,Trigger),
  pfcGetSupport(pfcBT(P,Trigger),S),
  pfcEvalLHS(Trigger,S),
  fail.

pfcBC(F) :-
  %= this is probably not advisable due to extreme inefficiency.
  var(F)    ->  pfcFact(F) ;
  ( \+ current_predicate(_,F)) -> mpred_call(F) ;
  % check for system predicates as well.
  not(predicate_property(F,number_of_clauses(_))) -> pfc_call(systemPred,F) ; 
  otherwise ->  pfc_clause_db_unify(F,Condition),
    with_assertions(thlocal:noDBaseHOOKS(_),pfc_call(neck(F),Condition)).



% an action is pfcUndoable if there exists a method for undoing it.
pfcUndoable(A) :- pfcUndoMethod(A,_).



%%
%%
%= defining fc rules 
%%

%= pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form 
%= Out.  It also does certain optimizations.  Backtracking into this
%= predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  pfc_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


%= pfc_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%= Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P / Condition form is no longer used anywhere.

pfc_nf1(P / Cond,[(\+P) / Cond]) :- pfcNegatedLiteral(P), !.

pfc_nf1(P / Cond,[P / Cond]) :-  pfcLiteral(P), !.

%= handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_negation(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

%= disjunction.

pfc_nf1((P;Q),NF) :- 
  !,
  (pfc_nf1(P,NF) ;   pfc_nf1(Q,NF)).


%= conjunction.

pfc_nf1((P,Q),NF) :-
  !,
  pfc_nf1(P,NF1),
  pfc_nf1(Q,NF2),
  append(NF1,NF2,NF).

%= handle a random atom.

pfc_nf1(P,[P]) :- 
  pfcLiteral(P), 
  !.

%%= shouln't we have something to catch the rest as errors?
pfc_nf1(Term,[Term]) :-
  pfcWarn("pfc_nf doesn't know how to normalize ~w",[Term]).


%= pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
pfc_nf1_negation((P / Cond),[(\+(P)) / Cond]) :- !.

pfc_nf1_negation((P;Q),NF) :-
  !,
  pfc_nf1_negation(P,NFp),
  pfc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

pfc_nf1_negation((P,Q),NF) :- 
  % this code is not correct! twf.
  !,
  pfc_nf1_negation(P,NF) 
  ;
  (pfc_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

pfc_nf1_negation(P,[\+P]).




%= pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%= changing ~{...} to {\+...}
%%= ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations(X,X) :- var(X),!.

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

pfc_nf_negation(Form,{PLNeg}) :-  
  nonvar(Form),
  correct_negations(callable,Form,PLNeg),
  !.
pfc_nf_negation(X,X).


%%
%= pfcBuildRhs(+Conjunction,-Rhs)
%%

pfcBuildRhs(X,[X]) :- 
  var(X),
  !.

pfcBuildRhs((A,B),[A2|Rest]) :- 
  !, 
  pfcCompileRhsTerm(A,A2),
  pfcBuildRhs(B,Rest).

pfcBuildRhs(X,[X2]) :-
   pfcCompileRhsTerm(X,X2).

pfcCompileRhsTerm((P/C),((P:-C))) :- !.
pfcCompileRhsTerm(P,P).


%= pfc_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

pfc_negation((~P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).
pfc_negation((naf(P)),P).
% pfc_negation(not(P)),P).

pfcNegatedLiteral(P) :- 
  pfc_negation(P,Q),
  pfcPositiveLiteral(Q).

pfcLiteral(X) :- pfcNegatedLiteral(X).
pfcLiteral(X) :- pfcPositiveLiteral(X).

pfcPositiveLiteral(X) :-  
  functor(X,F,_), 
  \+ pfcConnective(F).

pfcConnective(';').
pfcConnective(',').
pfcConnective('/').
pfcConnective('|').
pfcConnective(('=>')).
pfcConnective(('<=')).
pfcConnective('<=>').

pfcConnective('-').
pfcConnective('~').
pfcConnective(('\\+')).

pfcProcessRule(Lhs,Rhs,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfcBuildRhs(Rhs,Rhs2),
  pfcForEach(pfc_nf(Lhs,Lhs2), 
          pfcBuild1Rule(Lhs2,rhs(Rhs2),(ParentRuleCopy,pcfUser))).

pfcBuild1Rule(Lhs,Rhs,Support) :-
  pfcBuildTrigger(Lhs,Rhs,Trigger),
  pfcEvalLHS(Trigger,Support).

pfcBuildTrigger([],Consequent,Consequent).

pfcBuildTrigger([V|Triggers],Consequent,pfcPT(V,X)) :-
  var(V),
  !, 
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([(T1/Test)|Triggers],Consequent,pfcNT(T2,Test2,X)) :- 
  pfc_negation(T1,T2),
  !,
  pfcBuildNtTest(T2,Test,Test2),
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([(T1)|Triggers],Consequent,pfcNT(T2,Test,X)) :- 
  pfc_negation(T1,T2),
  !,
  pfcBuildNtTest(T2,true,Test),
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([T/Test|Triggers],Consequent,pfcPT(T,X)) :-
  !, 
  pfcBuildTest(Test,Test2),
  pfcBuildTrigger([{Test2}|Triggers],Consequent,X).


%pfcBuildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([T|Triggers],Consequent,pfcPT(T,X)) :-
  !, 
  pfcBuildTrigger(Triggers,Consequent,X).

%%
%= pfcBuildNtTest(+,+,-).
%%
%= builds the test used in a negative trigger (pfcNT/3).  This test is a
%= conjunction of the check than no matching facts are in the db and any
%= additional test specified in the rule attached to this ~ term.
%%

pfcBuildNtTest(T,Testin,Testout) :-
  pfcBuildTest(Testin,Testmid),
  pfcConjoin((pfcBC(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

pfcBuildTest({Test},Test) :- !,pfcMarkW(Test).
pfcBuildTest(Test,Test):-pfcMarkW(Test).

%%

mpred_listing(F/_):-!,term_listing(F).
mpred_listing(Pred):-
  (get_functor(Pred,F,AUsed),((AUsed==0)->ignore(mpred_arity(F,A));A=AUsed)),
  mpred_listing(F/A).

%= simple typeing for pfc objects

pfcType(('=>'(_,_)),Type) :- !, Type=rule.
pfcType(('<=>'(_,_)),Type) :- !, Type=rule.
pfcType(('<='(_,_)),Type) :- !, Type=rule.
pfcType(pfcPT(_,_,_),Type) :- !, Type=trigger.
pfcType(pfcPT(_,_),Type) :- !, Type=trigger.
pfcType(pfcNT(_,_,_),Type) :- !,  Type=trigger.
pfcType(pfcBT(_,_),Type) :- !,  Type=trigger.
pfcType(pfcAction(_),Type) :- !, Type=action.
pfcType((('::::'(_,X))),Type) :- !, pfcType(X,Type).
pfcType(_,fact) :-
  %= if it's not one of the above, it must be a fact!
  !.

pfcAssertIfUnknown(P):- clause_safe(P,true),!.
pfcAssertIfUnknown(P):- is_asserted_eq(P),!.
pfcAssertIfUnknown(P):- show_call(pfc_assert(P)),!,sanity(is_asserted_eq(P)).

pfcAssert(P,Support) :- 
  (pfc_clause(P) ; pfc_assert(P)),
  !,
  pfcAddSupport(P,Support).

pfcAsserta(P,Support) :-
  (pfc_clause(P) ; pfc_asserta(P)),
  !,
  pfcAddSupport(P,Support).

pfcAssertz(P,Support) :-
  (pfc_clause(P) ; pfc_assertz(P)),
  !,
  pfcAddSupport(P,Support).

pfc_clause((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  pfc_clause_db_unify(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_clause(Head) :-
  % find a unit clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  pfc_clause_db_unify(Head_copy,true),
  variant(Head,Head_copy).


pfcForEach(Binder,Body) :- Binder,pfcdo(Body),fail.
pfcForEach(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- X,!.
pfcdo(_).


%= pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
%= L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-  
  pfcUnion(Tail,L,Tail2).


%= pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
%= arg3 is a simplified expression representing the conjunction of
%= args 1 and 2.

pfcConjoin(true,X,X) :- !.
pfcConjoin(X,true,X) :- !.
pfcConjoin(C1,C2,(C1,C2)).

% pfcFile('pfcsupport').	% support maintenance

%%
%%
%= predicates for manipulating support relationships
%%

:-decl_mpred_pfc(support2/3).
:-decl_mpred_pfc(support1/3).
:-decl_mpred_pfc(support3/3).

%= pfcAddSupport(+Fact,+Support)

pfcAddSupport(P,(Fact,Trigger)) :-
  pfc_assert(support1(P,Fact,Trigger)),
  pfc_assert(support2(Fact,Trigger,P)),
  pfc_assert(support3(Trigger,P,Fact)).

pfcGetSupport(P,(Fact,Trigger)) :-
   nonvar(P)         -> support1(P,Fact,Trigger) 
   ; nonvar(Fact)    -> support2(Fact,Trigger,P) 
   ; nonvar(Trigger) -> support3(Trigger,P,Fact) 
   ; otherwise       -> support1(P,Fact,Trigger).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(P),
  !,
  pfcRetractOrWarn(pfcRemoveSupport,support1(P,Fact,Trigger)),
  pfcRetractOrWarn(pfcRemoveSupport,support2(Fact,Trigger,P)),
  pfcRetractOrWarn(pfcRemoveSupport,support3(Trigger,P,Fact)).


pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(Fact),
  !,
  pfcRetractOrWarn(pfcRemoveSupport,support2(Fact,Trigger,P)),
  pfcRetractOrWarn(pfcRemoveSupport,support1(P,Fact,Trigger)),
  pfcRetractOrWarn(pfcRemoveSupport,support3(Trigger,P,Fact)).

pfcRemSupport(P,(Fact,Trigger)) :-
  pfcRetractOrWarn(pfcRemoveSupport,support3(Trigger,P,Fact)),
  pfcRetractOrWarn(pfcRemoveSupport,support1(P,Fact,Trigger)),
  pfcRetractOrWarn(pfcRemoveSupport,support2(Fact,Trigger,P)).


pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  support1(P,F,T).

pfc_make_supports((P,S1,S2)) :- 
  pfcAddSupport(P,(S1,S2),_),
  (pfcAddSome(P); true),
  !.

%= pfcTriggerKey(+Trigger,-Key) 
%%
%= Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey(pfcPT(Key,_),Key).
pfcTriggerKey(pfcPT(Key,_,_),Key).
pfcTriggerKey(pfcNT(Key,_,_),Key).
pfcTriggerKey(Key,Key).


%%^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger pfcBase1 pfc_clause that stores the trigger.
%%

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).



% pfcFile('thlocal').	% predicates to manipulate database.


%   File   : thlocal.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%%	restore, reset, etc.0 )

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm(support1/3).
pfcDatabaseTerm(support2/3).
pfcDatabaseTerm(support3/3).
pfcDatabaseTerm(pfcPT/3).
pfcDatabaseTerm(pfcNT/3).
pfcDatabaseTerm(pfcPT/2).
pfcDatabaseTerm(pfcNT/2).
pfcDatabaseTerm(pfcBT/2).
pfcDatabaseTerm('=>'/2).
pfcDatabaseTerm('<=>'/2).
pfcDatabaseTerm('<='/2).
pfcDatabaseTerm(pfcQueue/1).
pfcDatabaseTerm(pfcDefault/1).

% removes all forward chaining rules and pfcJustification_L from db.

pfcReset :-
  pfc_clause_db_unify(support1(P,F,Trigger),true),
  pfcRetractOrWarn(pfcReset,P),
  pfcRetractOrWarn(pfcReset,support1(P,F,Trigger)),
  pfcRetractOrWarn(pfcReset,support2(F,Trigger,P)),
  pfcRetractOrWarn(pfcReset,support3(Trigger,P,F)),
  fail.
pfcReset :-
  pfcDatabaseItem(T),
  pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]).
pfcReset.

% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  pfc_clause_db_unify(Term,_).

pfcRetractOrWarn(Why,X) :-  pfc_retract(Why,X), !.
pfcRetractOrWarn(Why,X) :- 
  pfcWarn("~w couldn't pfc_retract ~p.",[Why,X]).



% pfcFile('pfcdebug').	% debugging aids (e.g. tracing).


%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- decl_mpred_pfc pfc_settings/2.
:- decl_mpred_pfc pfc_settings/3.

:- pfcDefaultSetting(pfc_settings(warnings,_), pfc_settings(warnings,true)).

%= predicates to examine the state of pfc

pfcQueue :- listing(pfcQueue/1).

pfcPrintDB :- current_predicate(must_det_l/1),!,
  must_det_l([
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
   pfcPrintSupports,
   pfcQueue]),!.

pfcPrintDB :-
   pfcPrintFacts,
   pfcPrintRules,
   pfcPrintTriggers,
  pfcPrintSupports,
  pfcQueue,!.

%= pfcPrintFacts ..

pfcPrintFacts :- pfcPrintFacts(_,true).

pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-
  pfcFacts(P,C,L),
  pfcClassifyFacts(L,User,Pfc,_Rule),
  format("~n~nUser added facts:",[]),
  pfcPrintitems(User),
  format("~n~nPfc added facts:",[]),
  pfcPrintitems(Pfc).


%= printitems clobbers it's arguments - beware!

pfcPrintitems(H):-pfcDoConjs(pfcLambda([E],(numbervars(E,0,_),format("~n  ~w",[E]))),H).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,(pcfUser,pcfUser)),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).


printHeadItems(Head):-ignore((bagof(Head,pfc_clause_db_unify(Head,true),R1),pfcPrintitems(R1))).
printHeadCallItems(Head):-ignore((bagof(Head,pfc_clause_db_unify(Head,true),R1),pfcPrintitems(R1))).

pfcPrintRules :-
  printHeadItems((P=>Q)),printHeadItems((P<=>Q)),printHeadItems((P<=Q)).

pfcPrintTriggers :-
  format("Positive triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcPT(T,B))),
  format("Negative triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcNT(T,B))),
  format("Goal triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcBT(T,B))),!.

pfcPrintSupports :- 
  % temporary hack.
  setof((S > P), pfcGetSupport(P,S),L),
  pfcPrintitems(L).

%= pfcFact(P) is true if fact P was asserted into the database via add.

pfcFact(P) :- no_repeats(pfcFact(P,true)).

%= pfcFact(P,C) is true if fact P was asserted into the database via
%= add and contdition C is satisfied.  For example, we might do:
%= 
%=  pfcFact(X,pfcUserFact(X))
%%

pfcFact(P,C) :- no_repeats(pfcFact0(P,C)).

pfcFact(P,C) :- 
  pfcGetSupport(P,_),
  pfcType(P,fact),
  pfc_call(fact,C).

%= pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

%= pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof(P,pfcFact(P,C),L).

brake(X) :-  X, break.

%%
%%
%= predicates providing a simple tracing facility
%%

pfcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(o,o)).

pfcTraceAdd(pfcPT(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfcTraceAdd(pfcNT(_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfcTraceAdd(P,S) :-
   pfcTraceAddPrint(P,S),
   pfcTraceBreak(P,S).

crazy_bad_fact(isa(not,tCol)).
crazy_bad_fact(isa(_,not)).
crazy_bad_fact(mpred_prop(_,predArgTypes)).

pfcTraceAddPrint(P,S) :- crazy_bad_fact(P),retractall(tlbugger:show_must_go_on),!,trace_or_throw(crazy_pfcTraceAddPrint(P,S)).

pfcTraceAddPrint(P,S) :-
  \+ \+ pfc_settings(traced,P),
  !,
  copy_term(P,Pcopy),
  numbervars(Pcopy,0,_),
  (S=(pcfUser,pcfUser)
       -> format("~nAdding (u) ~w",[Pcopy])
        ; format("~nAdding (g) ~w",[Pcopy])).

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  pfc_settings(spied,P,add) -> 
   (copy_term(P,Pcopy),
    numbervars(Pcopy,0,_),
    format("~nBreaking on pfcAdd(~w)",[Pcopy]),
    break)
   ; true.

pfcTraceRem(pfcPT(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem(pfcNT(_,_)) :-
  % hack for now - never trace triggers.
  !.

pfcTraceRem(P) :-
  (pfc_settings(traced,P) 
     -> format('~nRemoving ~w.',[P])
      ; true),
  (pfc_settings(spied,P,pfcRem)
   -> (format("~nBreaking on pfcRem(~w)",[P]),
       break)
   ; true).


pfcTrace :- pfcTrace(_).

pfcTrace(Form) :-
  asserta(pfc_settings(traced,Form)).

pfcTrace(Form,Condition) :- 
  pfc_assert((pfc_settings(traced,Form) :- Condition)).

pfcSpy(Form) :- pfcSpy(Form,[add,pfcRem],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[add,pfcRem],Condition) :-
  !,
  pfcSpy1(Form,add,Condition),
  pfcSpy1(Form,pfcRem,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  pfc_assert((pfc_settings(spied,Form,Mode) :- Condition)).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :- 
  pfc_clause_db_ref(pfc_settings(spied,Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- pfc_retractall(pfcInternal,pfc_settings(traced,Form)).

% needed:  pfcTraceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :-
    pfc_settings(trace_exec,true),
    !,
    format(user_output, Msg, Args).
pfc_trace_msg(_Msg,_Args).

pfcWatch :- pfc_assert(pfc_settings(trace_exec,true)).

pfcNoWatch :-  pfc_retractall(pfcInternal,pfc_settings(trace_exec,true)).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :- 
  format("~nERROR/Pfc: ",[]),
  format(Msg,Args).


%%
%= These control whether or not warnings are printed at all.
%=   pfcWarn.
%=   nopfcWarn.
%%
%= These print a warning message if the flag pfcWarnings is set.
%=   pfcWarn(+Message)
%=   pfcWarn(+Message,+ListOfArguments)
%%

pfcWarn :- 
  pfc_retractall(pfcInternal,pfc_settings(warnings,_)),
  pfc_assert(pfc_settings(warnings,true)).

nopfcWarn :-
  pfc_retractall(pfcInternal,pfc_settings(warnings,_)),
  pfc_assert(pfc_settings(warnings,false)).
 
pfcWarn(Msg) :-  pfcWarn(Msg,[]).

pfcWarn(Msg,Args) :- 
  pfc_settings(warnings,true),
  !,
  sformat(S,Msg,Args),
  dmsg("~nWARNING/Pfc: ~s",[S]), % dtrace(S),
  !. 

pfcWarn(_,_).

%%
%= pfcWarnings/0 sets flag to cause pfc warning messages to print.
%= pfcNoWarnings/0 sets flag to cause pfc warning messages not to print.
%%

pfcWarnings :- 
  pfc_retractall(pfcInternal,pfc_settings(warnings,_)),
  pfc_assert(pfc_settings(warnings,true)).

pfcNoWarnings :- 
  pfc_retractall(pfcInternal,pfc_settings(warnings,_)).



% pfcFile('pfcjust').	% predicates to manipulate pfcJustification_L.


%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc Justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

pfcJustificationDB(F,J) :- justSupports(F,J).

pfcJustification_L(F,Js) :- bagof(J,pfcJustificationDB(F,J),Js).

justSupports(F,J):- loop_check(pfcGetSupport(F,J)).


%= pfcBase1(P,L) - is true iff L is a list of "pfcBase1" facts which, taken
%= together, allows us to deduce P.  A pfcBase1 fact is an pfcAxiom (a fact 
%= added by the pcfUser or a raw Prolog fact (i.e. one w/o any support))
%= or an assumption.

pfcBase1(F,[F]) :- (pfcAxiom(F) ; pfcAssumptionBase(F)),!.

pfcBase1(F,L) :-
  % i.e. (reduce 'append (map 'pfcBase1 (justification f)))
  pfcJustificationDB(F,Js),
  pfcBases(Js,L).


%= pfcBases(L1,L2) is true if list L2 represents the union of all of the 
%= facts on which some conclusion in list L1 is based.

pfcBases([],[]).
pfcBases([X|Rest],L) :-
  pfcBase1(X,Bx),
  pfcBases(Rest,Br),
  pfcUnion(Bx,Br,L).
	
pfcAxiom(F) :- 
  pfcGetSupport(F,(pcfUser,pcfUser)); 
  pfcGetSupport(F,(pfcGod,pfcGod)).

%= an pfcAssumptionBase/1''s G was a failed goal, i.e. were assuming that our failure to 
%= prove P is a proof of not(P)

pfcAssumptionBase(P) :- pfc_negation(P,_).
   
%= pfcAssumptionsSet(X,As) if As is a set of assumptions which underly X.

pfcAssumptionsSet(X,[X]) :- pfcAssumptionBase(X).
pfcAssumptionsSet(X,[]) :- pfcAxiom(X).
pfcAssumptionsSet(X,L) :-
  pfcJustificationDB(X,Js),
  pfcAssumption1(Js,L).

pfcAssumption1([],[]).
pfcAssumption1([X|Rest],L) :-
  pfcAssumptionsSet(X,Bx),
  pfcAssumption1(Rest,Br),
  pfcUnion(Bx,Br,L).  


%= pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%= of the form
%%
%=     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%=          ^                         and has the form of
%=          [J11, J12,... J1n]      a list of proof trees.


% pfcChild(P,Q) is true iff P is an immediate justifier for Q.
% mode: pfcChild(+,?)

pfcChild(P,Q) :-
  pfcGetSupport(Q,(P,_)).

pfcChild(P,Q) :-
  pfcGetSupport(Q,(_,Trig)),
  pfcType(Trig,trigger),
  pfcChild(P,Trig).

pfcChildren(P,L) :- bagof(C,pfcChild(P,C),L).

% pfcDescendant(P,Q) is true iff P is a justifier for Q.

pfcDescendant(P,Q) :- 
   pfcDescendant1(P,Q,[]).

pfcDescendant1(P,Q,Seen) :-
  pfcChild(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfcDescendant1(P,X,[X|Seen])).
  
pfcDescendants(P,L) :- 
  bagof(Q,pfcDescendant1(P,Q,[]),L).



% pfcFile('pfcwhy').	% interactive exploration of pfcJustification_L.



%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc pfcJustification_L.

% ***** predicates for brousing pfcJustification_L *****

:- use_module(library(lists)).

pfcWhy :- 
  pfcWhyMemory1(P,_),
  pfcWhy(P).

pfcWhy(N) :-
  number(N),
  !,
  pfcWhyMemory1(P,Js),
  pfcWhyCommand(N,P,Js).

pfcWhy(P) :-
  pfcJustification_L(P,Js),
  pfc_retractall(pfcInternal,pfcWhyMemory1(_,_)),
  pfc_assert(pfcWhyMemory1(P,Js)),
  pfcWhyBrouse(P,Js).

pfcWhy1(P) :-
  pfcJustification_L(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhyBrouse(P,Js) :-
  pfcShowJustifications(P,Js),
  pfcAskUser(' >> ',Answer),
  pfcWhyCommand(Answer,P,Js).

pfcWhyCommand(q,_,_) :- !.
pfcWhyCommand(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",[]).

pfcWhyCommand(N,_P,Js) :-
  float(N),
  !,
  pfcSelectJustificationNode(Js,N,Node),
  pfcWhy1(Node).

pfcWhyCommand(u,_,_) :-
  % u=up
  !.

pfcCommand(N,_,_) :-
  integer(N),
  !,
  format("~n~w is a yet unimplemented command.",[N]),
  fail.

pfcCommand(X,_,_) :-
 format("~n~w is an unrecognized command, enter h. for help.",[X]),
 fail.
  
pfcShowJustifications(P,Js) :-
  format("~nJustifications for ~w:",[P]),
  pfcShowJustification1(Js,1).

pfcShowJustification1([],_).

pfcShowJustification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  pfcShowJustifications2(J,N,1),
  N2 is N+1,
  pfcShowJustification1(Js,N2).

pfcShowJustifications2([],_,_).

pfcShowJustifications2([C|Rest],JustNo,StepNo) :- 
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  format("~n    ~w.~w ~w",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  pfcShowJustifications2(Rest,JustNo,StepNext).

pfcAskUser(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth(StepNo,Justification,Step).


:- if_startup_script(with_assertions(thlocal:pfcExpansion,ensure_loaded(dbase_i_mpred_pfc_testing))).

:- if_startup_script(prolog).



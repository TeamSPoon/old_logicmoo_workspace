/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
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
:- if(current_predicate(logicmoo_utils:combine_logicmoo_utils/0)).
:- module(logicmoo_util_bb_env,
    [  % when the predciates are not being moved from file to file the exports will be moved here
      ]).

:- include('logicmoo_util_header.pi').

:- multifile(lmconf:mpred_user_kb/1).
:- dynamic(lmconf:mpred_user_kb/1).

:- multifile(lmconf:mpred_system_kb/1).
:- dynamic(lmconf:mpred_system_kb/1).

:- thread_local(t_l:push_env_ctx).
:- dynamic(bb:'$env_info'/1).
:- multifile(bb:'$env_info'/1).

:- meta_predicate(env_call(+)).

:- dynamic(env_push_args/4).
:- multifile(env_push_args/4).
:- thread_local(t_l:db_spy/0).

:- thread_local((canDoTermExp/0)).
:- retractall(canDoTermExp).


% :- discontiguous((env_call/1,env_assert/1,env_asserta/1,env_retract/1,env_retractall/1)).

% 401,809,449 inferences, 52.592 CPU in 53.088 seconds (99% CPU, 7640071 Lips)

env_call(P):- call(ocl:P),ppi(P).
env_assert(P):- call(assert,ocl:P),!.
env_asserta(P):- call(asserta,ocl:P),!.
env_retract(P):-  call(retract,ocl:P).
env_retractall(F/A):- functor(P,F,A),!,retractall(ocl:P),!.
env_retractall(P):- call(retractall,ocl:P),!.

/*
env_call(P):- env_mpred_op(call,P),ppi(P).
env_assert(P):- must_det(env_mpred_op(assert,P)),!.
env_asserta(P):- must_det(env_mpred_op(asserta,P)),!.
env_retract(P):- env_mpred_op(retract,P).
env_retractall(F/A):- functor(P,F,A),!,env_retractall(P),!.
env_retractall(P):- must(env_mpred_op(retractall,P)),!.
*/

env_clear(kb(Dom)):-nonvar(Dom),!,env_clear(Dom).
env_clear(Dom):- forall(prop_mpred(Dom,F,A),env_mpred_op(retractall(F/A))).
env_mpred_op(OP_P):- OP_P=..[OP,P],env_mpred_op(OP,P).

:- module_transparent(env_mpred_op/2).
:- meta_predicate env_mpred_op(1,:).
env_mpred_op(OP,P):- var(OP),!,P.
%TODO env_mpred_op(OP,P):- prop_mpred(P,_,_),!,forall(prop_mpred(P,F,A),(nop(trace),env_mpred_op(OP,F/A) )).
%TODO env_mpred_op(OP,F):- prop_mpred(_,F,_),!,forall(prop_mpred(_,F,A),(nop(trace),env_mpred_op(OP,F/A) )).
env_mpred_op(OP,F/A):-integer(A),atom(F),!,functor(P,F,A),!,env_mpred_op(OP,P).
env_mpred_op(OP,P):- t_l:push_env_ctx, do_prefix_arg(P, ZZ, PP, _Type),P\==PP,!,get_env_ctx(ZZ),call(OP,/*ocluser*/ocl:PP).
env_mpred_op(OP,P):- functor_h(P,F,A),must(get_mpred_stubType(F,A,ENV)),!,env_mpred_op(ENV,OP,P).
env_mpred_op(OP,P):- append_term(OP,P,CALL),current_predicate(_,CALL),!,show_call(/*ocluser*/ocl:CALL).
env_mpred_op(OP,P):- trace,trace_or_throw(unk_env_mpred_op(OP,P)).

env_shadow(OP,P):-lmconf:call(OP,P).

:- dynamic( in_dyn/2).
in_dyn(_DB,Call):- var(Call),!,get_mp_arity(F,A),functor(Call,F,A),( predicate_property(Call,_) -> loop_check(Call)).
in_dyn(_DB,Call):- functor(Call,F,A), get_mp_arity(F,A), predicate_property(Call,_), !, loop_check(Call).
in_dyn_pred(_DB,Call):- var(Call),!,get_mp_arity(F,A),functor(Call,F,A),( predicate_property(Call,_) -> loop_check(Call)).
in_dyn_pred(_DB,Call):- functor(Call,F,A), get_mp_arity(F,A), predicate_property(Call,_), !, loop_check(Call).


get_mp_arity(F,A):- lmconf:mpred_user_kb(M),M:arity(F,A).
get_mp_arity(F,A):- lmconf:mpred_system_kb(M),M:mpred_arity(F,A).

prop_mpred(Prop,F,A):- lmconf:mpred_system_kb(M),M:kb:mpred_isa(F,Prop),get_mp_arity(F,A).

get_mpred_stubType(_,_,dyn):-!.
get_mpred_stubType(F,A,StubOut):-    
   prop_mpred(stubType(Stub),F,A),!,must(StubIn=Stub),
   % PREVENTS FAILURE
   nop(StubIn==dyn->true;dmsg(get_mpred_stubType(F,A,StubIn))),
   StubOut=dyn.

get_mpred_stubType(F,A,dyn):-prop_mpred(dyn,F,A).
get_mpred_stubType(_,_,dyn).

:- ain(isa_kb:mpred_isa(l)).
:- ain(isa_kb:mpred_isa(g)).
:- ain(isa_kb:mpred_isa(dyn)).
:- nb_setval(disabled_env_learn_pred,false).

:- thread_local(t_l:env_ctx/2).

:- export(decl_env_mepred/2).
%:-module_transparent(decl_env_mepred/2).

decl_env_mepred(_,[]):-!.
decl_env_mepred([],_):-!.
decl_env_mepred(Props,[H|T]):-!,decl_env_mepred(Props,H),decl_env_mepred(Props,T).
decl_env_mepred(Props,(H,T)):-!,decl_env_mepred(Props,H),decl_env_mepred(Props,T).


decl_env_mepred([H],Pred):-!,decl_env_mepred(H,Pred).
decl_env_mepred([H|T],Pred):-!,decl_env_mepred(H,Pred),decl_env_mepred(T,Pred).
decl_env_mepred((H,T),Pred):-!,decl_env_mepred(H,Pred),decl_env_mepred(T,Pred).

decl_env_mepred(kb(KB),_):- ain(isa_mpred_user_kb(KB)),fail.
decl_env_mepred(stubType(dyn),Pred):-!, decl_env_mepred(dyn,Pred).

decl_env_mepred(CMPD,Pred):-  fail, compound(CMPD),CMPD=..[_|CMPDL],
   decl_env_mepred(CMPDL,Pred),
   get_functor(Pred,F,A),
   decl_env_mepred_fa(CMPD,Pred,F,A),!.

decl_env_mepred(Prop,Pred):- get_functor(Pred,F,A),
   decl_env_mepred_fa(Prop,Pred,F,A).

decl_env_mepred_dom(Props,Preds):-
 must(w_tl(t_l:env_ctx(dom,_CurrentDomain), 
    decl_env_mepred([dom|Props],Preds))).
decl_env_mepred_task(Props,Preds):-
 must(w_tl(t_l:env_ctx(task,_CurrentTask), 
    decl_env_mepred([task|Props],Preds))).


abolish_and_make_static(F,A):-
  must_det_l((abolish(F,A),
  retractall(get_mp_arity(F,A)),
   retractall(arity(F,A)),
   retractall(prop_mpred(_,F,A)),
  functor(H,F,A),asserta((H:-trace_or_throw(H))),compile_predicates([F/A]),lock_predicate(H))).

decl_env_mepred_fa(Prop,_Pred,F,A):- t_l:push_env_ctx,    
   t_l:env_ctx(Type,Prefix),A1 is A+1,!,
   must_det_l((functor(Pred1,F,A1),functor(Pred,F,A1),
   add_push_prefix_arg(Pred,Type,Prefix,Pred1),
   decl_env_mepred_real(Prop,Pred1,F,A1),!,
   abolish_and_make_static(F,A),!,
   if_defined(lmconf:arity(F,AA)),
   must(arity(F,A1)==arity(F,AA)))).
decl_env_mepred_fa(Prop,Pred,F,A):-
   decl_env_mepred_real(Prop,Pred,F,A).

decl_env_mepred_real(Prop,Pred,F,A):- 
  (Prop==task->(thread_local(/*ocluser*/ocl:F/A));true),
  (Prop==dyn->(dynamic(/*ocluser*/ocl:F/A));true),
  (Prop==cache->'$set_pattr'(ocl:Pred, pred, (volatile));true),
  (Prop==dom->(multifile(/*ocluser*/ocl:F/A));true),
  lmconf:export(/*ocluser*/ocl:F/A),
  if_defined(decl_mpred(Pred,Prop),ain(kb:mpred_isa(F,Prop))),
  ain(isa_kb:mpred_isa(Prop)), ain(get_mp_arity(F,A)),ain(arity(F,A)),!,
  trace,ain(prop_mpred(Prop,F,A)).


env_learn_pred(_,_):-nb_getval(disabled_env_learn_pred,true),!.
env_learn_pred(ENV,P):-lmconf:decl_env_mepred(ENV,P).

env_recorded(call,Val) :- recorded(Val,Val).
env_recorded(assert, Val) :- recordz(Val,Val).
env_recorded(asserta, Val) :- recorda(Val,Val).
env_recorded(retract, Val) :- recorded(Val,Val,Ref), erase_safe(recorded(Val,Val,Ref),Ref).
env_recorded(retractall, Val) :- foreach( recorded(Val,Val,Ref), erase_safe(recorded(Val,Val,Ref),Ref) ).

lg_op2(rec_db,OP,env_recorded(OP)).
lg_op2(g,OP,OP).

lg_op2(_,OP,OP).

:- meta_predicate env_mpred_op(?,1,:).
:- meta_predicate env_mpred_op_1(?,1,:).
:- meta_predicate env_shadow(1,?).

env_mpred_op(_,_,[]):-!.
env_mpred_op(ENV,OP,F/A):- trace,var(A),!, forall(prop_mpred(ENV,F,A),((functor(P,F,A),env_mpred_op(ENV,OP,P)))).
env_mpred_op(ENV,retractall,F/A):-functor(P,F,A),!,env_mpred_op(ENV,retractall,P).
% env_mpred_op(ENV,OP,Dom):- isa_kb:mpred_isa(Dom),!,forall(prop_mpred(Dom,F,A),env_mpred_op(ENV,OP,F/A)).
% env_mpred_op(ENV,OP,F/A):-!, functor(P,F,A), (((get_mpred_stubType(F,A,LG2),LG2\==ENV)  -> env_mpred_op(LG2,OP,P) ; env_mpred_op(ENV,OP,P) )).
% env_mpred_op(_,retractall,P):-functor_h(P,F,A),must(get_mpred_stubType(F,A,_)),fail.
% env_mpred_op(ENV,OP,P):- functor_h(P,F,A),  (((get_mpred_stubType(F,A,LG2),LG2\==ENV)  -> env_mpred_op(LG2,OP,P) ; fail )).
env_mpred_op(ENV,OP,P):- functor_h(P,F,A),  (((get_mpred_stubType(F,A,LG2),LG2\==ENV)  -> env_mpred_op_1(LG2,OP,P) ; env_mpred_op_1(ENV,OP,P) )).


env_mpred_op_1(dyn,OP,P):- !,call(OP,/*ocluser*/ocl:P).
env_mpred_op_1(ENV,OP,(A,B)):-!, env_mpred_op(OP,A), env_mpred_op(ENV,OP,B).
env_mpred_op_1(ENV,OP,[A|B]):-!, env_mpred_op(ENV,OP,A), env_mpred_op(ENV,OP,B).

env_mpred_op_1(in_dyn(DB),OP,P):- !, call(OP,in_dyn(DB,P)).
env_mpred_op_1(in_pred(DB),OP,P):-!, DBPRED=..[DB,P], call(OP,DBPRED).
env_mpred_op_1(with_pred(Pred),OP,P):-!, call(Pred,OP,P).
% env_mpred_op_1(ENV,OP,P):- dmsg(env_mpred_op_1(ENV,OP,P)),fail.
env_mpred_op_1(ENV,OP,P):- lg_op2(ENV,OP,OP2),!,call(OP2,P).
% env_mpred_op_1(ENV,OP,P):- throw(trace),simplest(ENV),!,call(OP,P).
env_mpred_op_1(stubType(ENV),OP,P):-!,env_mpred_op(ENV,OP,P).
% !,env_mpred_op_1(in_dyn(DB),OP,P).
env_mpred_op_1(_,OP,P):-!,env_mpred_op_1(in_dyn(db),OP,P).
env_mpred_op_1(_,_,_):-trace,fail.
env_mpred_op_1(l,OP,P):-!,call(OP,/*ocluser*/ocl:P).
env_mpred_op_1(g,OP,P):-!,call(OP,/*ocluser*/ocl:P).
env_mpred_op_1(l,OP,P):-!,env_mpred_op_1(dyn,OP,P).
env_mpred_op_1(l,OP,P):-!,env_mpred_op_1(in_dyn(db),OP,P).
env_mpred_op_1(l,OP,P):-!,env_mpred_op_1(rec_db,OP,P).
env_mpred_op_1(g,asserta,P):-retractall(/*ocluser*/ocl:P),asserta(/*ocluser*/ocl:P).
env_mpred_op_1(g,assert,P):-ain(P).
env_mpred_op_1(g,retract,P):-env_mpred_op_1(g,call,P),retract(/*ocluser*/ocl:P).
env_mpred_op_1(g,retractall,P):-foreach(env_mpred_op_1(g,call,P),retractall(P)).
env_mpred_op_1(ENV,OP,P):-env_learn_pred(ENV,P),lg_op2(ENV,OP,OP2),!,call(OP2,P).
env_mpred_op_1(_,OP,P):-call(OP,P).

%ppi(P):-functor(P,tp_node,_),!.
%ppi(P):-predicate_property(P,number_of_clauses(NC)),!,(NC<2000->true;(dmsg((number_of_clauses(NC):-P)))),!.
ppi(_).


env_info(O):- forall(env_info(O,Info),portray_clause(env_info(O):-Info)).

env_info(Type,Infos):- isa_kb:mpred_isa(Type),atom(Type),!,findall(Info,env_1_info(Type,Info),Infos),!.
env_info(Pred,Infos):- (nonvar(Pred)-> env_predinfo(Pred,Infos) ; (get_mp_arity(F,A),Pred=F/A,env_predinfo(Pred,Infos))),!.


harvest_preds(Type,Functors):-
 findall(functor(P,F,A),((get_mp_arity(F,A),(prop_mpred(Type,F,A);Type=F),functor(P,F,A))),Functors).

env_1_info(Type,[predcount(NC)|Infos]):- 
 gensym(env_1_info,Sym),flag(Sym,_,0),
   harvest_preds(Type,PFAs),
    findall(F/A - PredInf,
      (member(functor(P,F,A),PFAs),
        predicate_property(P,number_of_clauses(NC)),
        env_predinfo(P,PredInf),
        flag(Sym,X,X+NC)),
    Infos),flag(Sym,NC,0).

env_predinfo(PIn,Infos):- functor_h(PIn,F,A),get_mp_arity(F,A),functor(P,F,A),findall(Info,pred_1_info(P,F,A,Info),Infos).

pred_1_info(P,_,_,Info):- member(Info:Prop,[count(NC):number_of_clauses(NC),mf:multifile,dyn:dynamic,vol:volitile,local:local]),predicate_property(P,Prop).
pred_1_info(_,F,A,Info):- prop_mpred(Info,F,A).
pred_1_info(_,F,A,F/A).


:- meta_predicate(env_consult(:)).
env_consult(M:File):- \+ exists_file(File),!,forall(filematch(File,FM),env_consult(M:FM)).
env_consult(M:File):- ain(env_source_file(File)),
   w_tl((M:term_expansion(A,B):-env_term_expansion(A,B)),M:consult(File)).


env_set(Call):-Call=..[P,V],!,gvar_put(P,V).
env_get(Call):-Call=..[P,V],!,gvar_get(P,V).


env_meta_term(t(env_call,env_assert,env_asserta,env_retract,env_retractall)).


env_push_argsA(Pred, Type,Prefix,Pred1 ):-t_l:push_env_ctx, env_push_args(Pred, Type,Prefix,Pred1 ).

do_prefix_arg(Pred,Prefix,Pred ,Type ) :-env_push_argsA(_,  Type,Prefix,Pred ),!.
do_prefix_arg(Pred,Prefix,Pred1 ,Type ) :-env_push_argsA(Pred, Type,Prefix,Pred1 ),!.
do_prefix_arg(M:Pred,Prefix,M:Pred1, Type):-do_prefix_arg(Pred,Prefix,Pred1,Type),!.

push_prefix_arg(Pred,Type,Prefix,Pred ):-env_push_argsA(_,Type,Prefix,Pred),!,must(compound(Pred)).
push_prefix_arg(Pred,Type,Prefix,Pred1):-env_push_argsA(Pred,Type,Prefix,Pred1),!.

add_push_prefix_arg((F/A),Type,Prefix,Pred1):-must(integer(A)),!,functor(Pred,F,A),add_push_prefix_arg(Pred,Type,Prefix,Pred1).
add_push_prefix_arg(Pred,Type,Prefix,Pred1):- must(atom(Type)),Pred=..[F|ARGS],Pred1=..[F,Prefix|ARGS],ain(env_push_args(Pred,Type,Prefix,Pred1)),!.


term_expansion_add_context(_NeedIt,_Ctx,_,B,B):- var(B),!.
term_expansion_add_context( NeedIt, Ctx,Outter,B,BB):- Outter \== (/), do_prefix_arg(B,Ctx,BB,_Type),!,ignore(NeedIt=Outter).
term_expansion_add_context(_NeedIt,_Ctx,_,B,B):- \+compound(B),!.
term_expansion_add_context(_NeedIt,_Ctx,_,DECL,DECLN):- 
    DECL  =..[DF,(F/A)],number(A),atom(F),functor(H,F,A),
    term_expansion_add_context(_Dont,_Ctx2, decl ,H,HH),
    DECLN =..[DF,(F/B)],functor(HH,F,B).
term_expansion_add_context( NeedIt, Ctx,_,B,BB):- B=..[F|A], maplist(term_expansion_add_context(NeedIt,Ctx,F),A,AA),BB=..[F|AA],!.

:- dynamic(env_source_file/1).

hb_to_clause(H,B,HB):- hb_to_clause0(H,B,HB0),!,HB=HB0.
hb_to_clause0(H,T,H):- T==true.
hb_to_clause0(T,B,(:-B)):- T==true.
hb_to_clause0(H,B,(H:-B)).


clause_to_hb(HB,H,B):-clause_to_hb0(HB,H0,B0),!,H=H0,B=B0.
clause_to_hb0((C:-T),H,B):- T==true,clause_to_hb0(C,H,B).
clause_to_hb0((H:-B),H,B).
clause_to_hb0((:-B),true,B).
clause_to_hb0((H),H,true).

:- export(env_term_expansion/2).
env_term_expansion(HB,OUT):- t_l:push_env_ctx,!,
  must_det_l((
   clause_to_hb(HB,H,B),
   term_expansion_add_context(BNeedIt,Ctx,(:-),B,BB),
   term_expansion_add_context(HNeedIt,Ctx,(:-),H,HH),
   ((var(BNeedIt),functor(HH,F,A),\+functor(H,F,A)) -> ((get_env_ctx(Ctx),nonvar(Ctx))) ; true),
   (((nonvar(HNeedIt);nonvar(BNeedIt)),var(Ctx)) -> BBB = (get_env_ctx(Ctx),BB) ; BBB = BB))),
   (BBB\==B ; H\==HH),
   ((dmsg((old(H):-old,(B))),dmsg((HH:-BBB)))),
   hb_to_clause(HH,BBB,OUT),!.


env_term_expansion(HB,bb:'$env_info'(OUT)):-  HB\=(:-_), end_of_file\==HB, clause_to_hb(HB,H,B),
  is_ocl_expanded_file,must(ain((H:-B))),
  hb_to_clause(H,B,OUT),!.


get_env_expected_ctx(Current):- 
   (prolog_load_context(source,File) -> Current = loading(File) ; (env_source_file(_) -> Current = memory ; Current = bb  )).

get_env_source_ctx(A,Active):-
    clause(domain_name(A),true,Ref),
    (clause_property(Ref,file(From)) -> (env_source_file(From) -> Active = loaded(From) ;  Active = loading(From)) ; Active = memory).

get_env_ctx(A):- gvar_get(domain_name,A),!.
get_env_ctx(A):- 
    get_env_expected_ctx(Current),
    get_env_source_ctx(A, Active),
    ( Current=Active -> true ; fail).
get_env_ctx(_ChameleonWorld).
% get_env_ctx(chameleonWorld):-!.

:- add_push_prefix_arg(get_tasks/3,dom,_,_).
:- add_push_prefix_arg(domain_name/1,dom,_,_).

is_env_expanded_file:- loading_file(File),!,once(file_name_extension(_,ocl,File);env_source_file(File)),!.

is_ocl_expanded_file:- loading_file(File),file_name_extension(_,ocl,File).
   
/*
user:term_expansion(A,B):- nonvar(A), A\==end_of_file, is_env_expanded_file,
  env_term_expansion(A,B),
  must(nonvar(B)),A\=@=B.
*/



/*
env_mpred_op(ENV,OP_P):- throw(trace),simplest(ENV),!,OP_P.
env_mpred_op(ENV,call(P)):-env_mpred_op(ENV,call,P).
env_mpred_op(ENV,assert(P)):-env_mpred_op(ENV,assert,P).
env_mpred_op(ENV,asserta(P)):-env_mpred_op(ENV,asserta,P).
env_mpred_op(ENV,retract(P)):-env_mpred_op(ENV,retract,P).
env_mpred_op(ENV,retractall(F/A)):-functor(P,F,A),!,env_mpred_op(ENV,retractall,P).
env_mpred_op(ENV,retractall(P)):-env_mpred_op(ENV,retractall,P).
env_mpred_op(ENV,OP_P):- OP_P=..[OP,P], env_mpred_op(ENV,OP,P).


env_call(P):- env_mpred_op(call,P),ppi(P).
env_assert(P):- env_mpred_op(assert,P).
env_asserta(P):- env_mpred_op(asserta,P).
env_retract(P):- env_mpred_op(retract,P).
env_retractall(P):-env_mpred_op(retractall,P).


env_call(F):-!,call(F).
env_asserta(F):-!,maybe_show_env_mpred_op(asserta(F)).
env_assert(F):-!,maybe_show_env_mpred_op(assert(F)).
env_retract(F):-!,maybe_show_env_mpred_op(retract(F)).
env_retractall(F):-!,maybe_show_env_mpred_op(retractall(F)).
maybe_show_env_mpred_op(G):- !,G.
maybe_show_env_mpred_op(G):- t_l:db_spy -> show_call(G); G.

:- meta_predicate(maybe_show_env_mpred_op(0)).

*/
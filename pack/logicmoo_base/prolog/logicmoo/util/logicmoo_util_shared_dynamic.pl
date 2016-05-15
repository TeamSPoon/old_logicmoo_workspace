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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_structs.pl
:- module(logicmoo_util_shared_dynamic,
          [
          decl_shared/1,
          decl_shared/2,
          system_goal_expansion_safe_wrap/2,
          ereq/1,
          dbreq/1,
          nb_current_or_nil/2,
          get_named_value_goal/2,
          is_fbe/3,
          warn_if_static/2,
          is_user_module/0]).


:- module_transparent((decl_shared/1,
          system_goal_expansion_safe_wrap/2,
          ereq/1,
          dbreq/1,
          warn_if_static/2,
          is_user_module/0)).

:- meta_predicate decl_shared(1,+).
:- meta_predicate decl_shared(+).

:- dynamic(wsh_w:wrap_shared/3).

:- multifile(system:goal_expansion/4).
:- dynamic(system:goal_expansion/4).
:- multifile(system:term_expansion/4).
:- dynamic(system:term_expansion/4).

:- use_module(logicmoo_util_dmsg).
:- use_module(logicmoo_util_rtrace).

:- meta_predicate get_named_value_goal(0,*).


:- multifile((system:clause_expansion/2,
              system:directive_expansion/2,
              system:body_expansion/2,
              system:sub_body_expansion/2,
              system:call_expansion/2,
              system:sub_call_expansion/2)).
:- dynamic((system:clause_expansion/2,
              system:directive_expansion/2,
              system:body_expansion/2,
              system:sub_body_expansion/2,
              system:call_expansion/2,
              system:sub_call_expansion/2)).



:- dynamic(ereq/1).
:- module_transparent(ereq/1).
ereq(C):- find_and_call(C).

:- dynamic(dbreq/1).
:- module_transparent(dbreq/1).
dbreq(C):- ereq(C).

%% wsh_w:wrap_shared( ?VALUE1, :Wrapper, ?VALUE3) is semidet.
%
% Wrap Shared.
%

% wsh_w:wrap_shared(_,_,_):-!,fail.
wsh_w:wrap_shared(isa,2,ereq).
wsh_w:wrap_shared(t,_,ereq).
%wsh_w:wrap_shared(call,_,ereq).
%wsh_w:wrap_shared(apply,_,ereq).

wsh_w:wrap_shared(assert,1,dbreq):- is_user_module.
wsh_w:wrap_shared(assert,2,dbreq):- is_user_module.
wsh_w:wrap_shared(asserta,1,dbreq):- is_user_module.
wsh_w:wrap_shared(asserta,2,dbreq):- is_user_module.
wsh_w:wrap_shared(assertz,1,dbreq):- is_user_module.
wsh_w:wrap_shared(assertz,2,dbreq):- is_user_module.
wsh_w:wrap_shared(clause,2,dbreq):- is_user_module.
wsh_w:wrap_shared(clause,3,dbreq):- is_user_module.
wsh_w:wrap_shared(retract,1,dbreq):- is_user_module.
wsh_w:wrap_shared(retractall,1,dbreq):- is_user_module.

wsh_w:wrap_shared(argQuotedIsa,3,ereq).
wsh_w:wrap_shared(arity,2,ereq).
wsh_w:wrap_shared(call_OnEachLoad,1,ereq).
wsh_w:wrap_shared(completeExtentEnumerable,1,ereq).
wsh_w:wrap_shared(completelyAssertedCollection,1,ereq).
wsh_w:wrap_shared(constrain_args_pttp,2,ereq).
wsh_w:wrap_shared(cycPlus2,2,ereq).
wsh_w:wrap_shared(cycPred,2,ereq).
wsh_w:wrap_shared(decided_not_was_isa,2,ereq).
wsh_w:wrap_shared(predicateConventionMt,2,ereq).
wsh_w:wrap_shared(functorDeclares,1,ereq).
wsh_w:wrap_shared(genls,2,ereq).
wsh_w:wrap_shared(isa,2,ereq).
wsh_w:wrap_shared(lambda,5,ereq).
%wsh_w:wrap_shared(lmcache:loaded_external_kbs,0,ereq).
wsh_w:wrap_shared(meta_argtypes,1,ereq).
wsh_w:wrap_shared(mpred_f,4,ereq).
wsh_w:wrap_shared(mpred_f,5,ereq).
wsh_w:wrap_shared(mpred_f,6,ereq).
wsh_w:wrap_shared(mpred_f,7,ereq).
wsh_w:wrap_shared(mpred_mark,3,ereq).
wsh_w:wrap_shared(predicateConventionMt,2,ereq).
wsh_w:wrap_shared(mudKeyword,2,ereq).
wsh_w:wrap_shared(pfcControlled,1,ereq).
wsh_w:wrap_shared(pfcRHS,1,ereq).
wsh_w:wrap_shared(prologBuiltin,1,ereq).
wsh_w:wrap_shared(prologDynamic,1,ereq).
wsh_w:wrap_shared(prologHybrid,1,ereq).
wsh_w:wrap_shared(prologKIF,1,ereq).
wsh_w:wrap_shared(prologMacroHead,1,ereq).
wsh_w:wrap_shared(prologPTTP,1,ereq).
wsh_w:wrap_shared(prologSideEffects,1,ereq).
wsh_w:wrap_shared(props,2,ereq).
wsh_w:wrap_shared(resultIsa,2,ereq).
wsh_w:wrap_shared(singleValuedInArg,2,ereq).
wsh_w:wrap_shared(spft,3,ereq).
wsh_w:wrap_shared(support_hilog,2,ereq).
wsh_w:wrap_shared(t,3,ereq).
wsh_w:wrap_shared(tCol,1,ereq).
wsh_w:wrap_shared(tNotForUnboundPredicates,1,ereq).
wsh_w:wrap_shared(tPred,1,ereq).
wsh_w:wrap_shared(tRelation,1,ereq).
wsh_w:wrap_shared(tSet,1,ereq).
wsh_w:wrap_shared(ttExpressionType,1,ereq).
wsh_w:wrap_shared(ttPredType,1,ereq).
wsh_w:wrap_shared(ttTemporalType,1,ereq).
wsh_w:wrap_shared(use_ideep_swi,0,ereq).

is_user_module :- prolog_load_context(source,F), lmconf:mpred_is_impl_file(_,F),!,fail.
is_user_module :- prolog_load_context(module,user). 
is_user_module :- prolog_load_context(module,M), module_property(M,class(L)),L=library,!,fail.


%% system_goal_expansion_safe_wrap( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%
system_goal_expansion_safe_wrap(MT:Goal,call_u(Goal)):-MT==abox,!.
system_goal_expansion_safe_wrap(MT:Goal,(call_u(genlMt(abox,GMt)),with_umt(GMt,Goal))):- MT==tbox.
system_goal_expansion_safe_wrap(T,_):- \+ compound(T),!,fail.
system_goal_expansion_safe_wrap(M:T,M:I):-!,compound(T),functor(T,F,A),wsh_w:wrap_shared(F,A,How),safe_wrap(T,How,I).
system_goal_expansion_safe_wrap(T,I):- functor(T,F,A),wsh_w:wrap_shared(F,A,How),safe_wrap(T,How,I).


system:sub_call_expansion(T,I):-nonvar(T),system_goal_expansion_safe_wrap(T,I).
system:sub_call_expansion(T,(mpred_at_box:defaultAssertMt(NewVar),NewT)):- compound(T),subst(T,abox,NewVar,NewT),NewT\=@=T.

system:sub_body_expansion(T,I):-nonvar(T),system_goal_expansion_safe_wrap(T,I).



%% safe_wrap( Term, +How, -Wrapped) is semidet.
%
% Safely Paying Attention To Corner Cases Wrap.
%
safe_wrap(I,_,if_defined(I)):- current_prolog_flag(xref,true),!,fail.
safe_wrap(I,How,call(How,I)).



warn_if_static(F,A):- 
 ignore((F\={},
  functor(P,F,A),
  is_static_predicate(F/A),
  listing(P),
  trace_or_throw(warn(pfcPosTrigger,P,static)))).


decl_shared(Var):- decl_shared(nop,Var),!.

%% decl_shared_plus(Plus, :TermM) is semidet.
%
% Declare Shared.
%
decl_shared(Plus,Var):-var(Var),!,trace_or_throw(var_decl_shared(Plus,Var)).
decl_shared(Plus,baseKB:FA):-!,decl_shared(Plus,FA),!.
decl_shared(Plus,abox:FA):-!,decl_shared(Plus,FA),!.
decl_shared(Plus,(A,B)):-!,decl_shared(Plus,A),!,decl_shared(Plus,B),!.
decl_shared(Plus,[A]):-!,decl_shared(Plus,A),!.
decl_shared(Plus,[A|B]):-!,decl_shared(Plus,A),!,decl_shared(Plus,B),!.
decl_shared(Plus,M:(A,B)):-!,decl_shared(Plus,M:A),!,decl_shared(Plus,M:B),!.
decl_shared(Plus,M:[A]):-!,decl_shared(Plus,M:A),!.
decl_shared(Plus,M:[A|B]):-!,decl_shared(Plus,M:A),!,decl_shared(Plus,M:B),!.
decl_shared(Plus,_:M:A):-!,decl_shared(Plus,M:A),!.
decl_shared(Plus,F):-atom(F),!,decl_shared(Plus,F/_).
decl_shared(Plus,M:F):-atom(F),!,decl_shared(Plus,M:F/_).

decl_shared(Plus,F/A):-atom(F),!,
 asserta_if_new(wsh_w:wrap_shared(F,A,ereq)),
 ain(baseKB:prologHybrid(F)),
 ignore((integer(A),
   ain(baseKB:arity(F,A)),
   baseKB:multifile(F/A),
   functor(P,F,A),
   call(Plus,P),
   baseKB:discontiguous(F/A))).


decl_shared(Plus,M:F/A):-atom(F),!,
 asserta_if_new(wsh_w:wrap_shared(F,A,M:ereq)),
 ain(baseKB:prologHybrid(F)),
 ignore((integer(A),
   ain(baseKB:arity(F,A)),
   baseKB:multifile(F/A),
   functor(P,F,A),
   call(Plus,M:P),
   baseKB:discontiguous(F/A))).


decl_shared(Plus,M:P):-compound(P),!,functor(P,F,A),F\==(/),F\==(//),!,decl_shared(Plus,M:F/A).
decl_shared(Plus,P):-compound(P),!,functor(P,F,A),F\==(/),F\==(//),!,decl_shared(Plus,F/A).


% loading_module 
%:- decl_shared(Plus,arity/2).
%:- decl_shared(Plus,t).
%:- decl_shared(Plus,meta_argtypes/1).



% :- use_module(library(logicmoo_utils)).

% wdmsg(P):- format(user_error,' ~p ~n',[P]).

get_named_value_goal(G,N=V):- functor(G,N,_), ((\+ \+ G )-> V=true; V=false).


is_fbe(term,I,PosI):-!,
   compound(PosI),nonvar(I),
   nb_current('$term',Was), Was==I,
   nb_current('$term_position', Pos),
   get_pos_at(Pos,PosAt),
   get_pos_at(PosI,At),!,
   PosAt>0,!,At>=PosAt.

is_fbe(goal,I,PosI):-!,
   compound(PosI),nonvar(I),
   b_getval('$term',Was), Was==[],
   nb_current('$term_position', Pos),
   get_pos_at(Pos,PosAt),
   get_pos_at(PosI,At),!,
   PosAt>0,!,At>=PosAt.


%get_pos_at('$stream_position'(PosAt,_,_,_),PosAt) :- !.
get_pos_at(Pos,At) :- arg(1,Pos,At),!.


system_term_expansion(I,_P,_O,_P2):- var(I),!,fail.
system_term_expansion(I,P,_O,_P2):- \+ is_fbe(term,I,P),!,fail.
system_term_expansion((:- B),P,O,P2):- system:directive_expansion((:- B),O),P=P2.
system_term_expansion((:- B),P,O,P2):-!, system:call_expansion(( B),O),P=P2.
system_term_expansion(I,_P,_O,_P2):- nb_setval('$term_e',I),fail.
system_term_expansion((H ),P,O,P2):-  system:clause_expansion((H ),O),P=P2.
system_term_expansion((H :- I),P,(H :- O),P2):- system:body_expansion(I,O),P=P2.

% system_goal_expansion(I,P,O,P2):- var(I),!,fail.
sub_positional(P):- compound(P),functor(P,F,A),arg(A,P,[L|_]),compound(L),functor(L,F,_).

positional_range(_-_).
positional_seg(term_position(G2787,_,G2787,_,[_-_])).

nb_current_or_nil(N,V):-nb_current(N,V)->true;V=[].

system_goal_expansion(I,P,O,P2):- b_getval('$term',Was),
  get_named_value_goal(is_fbe(term,I,P),L1),
  get_named_value_goal(Was=@=I,L2),
  get_named_value_goal(sub_positional(P),L3),
  get_named_value_goal(positional_seg(P),L4),
  nb_current_or_nil('$term_e',TermWas), 
  get_named_value_goal(\+ (TermWas =@= Was), L5),
  do_ge([L1,L2,L3,L4,L5],I,O),ignore(I=O),!,P2=P.

 do_ge([is_fbe=false,(=@=)=false,sub_positional=false,positional_seg=true,(\+)=true], I,O):- ignore(system:sub_call_expansion(I,O)).
 do_ge(_,I,O):- ignore(system:sub_body_expansion(I,O)).
 do_ge(Why,I,O):- wdmsg(do_ge(Why,I,O)).


system:clause_expansion(I,O):- current_prolog_flag(show_expanders,true), wdmsg(system:clause_expansion(I,O)),fail.
system:directive_expansion(I,O):-  current_prolog_flag(show_expanders,true),wdmsg(system:directive_expansion(I,O)),fail.
system:body_expansion(I,O):- current_prolog_flag(show_expanders,true),wdmsg(system:body_expansion(I,O)),fail.
system:sub_body_expansion(I,O):- current_prolog_flag(show_expanders,true),wdmsg(system:sub_body_expansion(I,O)),fail.
system:call_expansion(I,O):- current_prolog_flag(show_expanders,true),wdmsg(system:call_expansion(I,O)),fail.
system:sub_call_expansion(I,O):- current_prolog_flag(show_expanders,true),wdmsg(system:sub_call_expansion(I,O)),fail.



% system:term_expansion(I,P,O,P2):- fail, get_named_value_goal(is_fbe(term,I,P)),wdmsg(system:te4(I,P,O,P2)),fail.
system:term_expansion(I,P,O,P2):- system_term_expansion(I,P,O,P2).
system:goal_expansion(I,P,O,P2):- system_goal_expansion(I,P,O,P2).



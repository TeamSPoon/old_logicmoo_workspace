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
          swc/0,
          clause_b/1,
          really_safe_wrap/3,
          warn_if_static/2]).



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
:- multifile((system:clause_expansion/4,
              system:directive_expansion/4,
              system:body_expansion/4,
              system:sub_body_expansion/4,
              system:call_expansion/4,
              system:sub_call_expansion/4)).
:- dynamic((system:clause_expansion/4,
              system:directive_expansion/4,
              system:body_expansion/4,
              system:sub_body_expansion/4,
              system:call_expansion/4,
              system:sub_call_expansion/4)).

:- module_transparent((decl_shared/1,
          system_goal_expansion_safe_wrap/2,
          ereq/1,
          dbreq/1,
          warn_if_static/2)).

:- meta_predicate decl_shared(1,+).
:- meta_predicate decl_shared(+).

:- dynamic(lmconf:wrap_shared/3).


:- use_module(logicmoo_util_clause_expansion).
:- use_module(logicmoo_util_dmsg).
:- use_module(logicmoo_util_rtrace).


:- dynamic(ereq/1).
:- module_transparent(ereq/1).
ereq(C):- find_and_call(C).

:- dynamic(dbreq/1).
:- module_transparent(dbreq/1).
dbreq(C):- ereq(C).

%% lmconf:wrap_shared( ?VALUE1, :Wrapper, ?VALUE3) is semidet.
%
% Wrap Shared.
%

% lmconf:wrap_shared(_,_,_):-!,fail.
lmconf:wrap_shared(isa,2,ereq).
lmconf:wrap_shared(t,_,ereq).
%lmconf:wrap_shared(call,_,ereq).
%lmconf:wrap_shared(apply,_,ereq).

lmconf:wrap_shared(assert,1,dbreq):- is_user_module.
lmconf:wrap_shared(assert,2,dbreq):- is_user_module.
lmconf:wrap_shared(asserta,1,dbreq):- is_user_module.
lmconf:wrap_shared(asserta,2,dbreq):- is_user_module.
lmconf:wrap_shared(assertz,1,dbreq):- is_user_module.
lmconf:wrap_shared(assertz,2,dbreq):- is_user_module.
lmconf:wrap_shared(clause,2,dbreq):- is_user_module.
lmconf:wrap_shared(clause,3,dbreq):- is_user_module.
lmconf:wrap_shared(retract,1,dbreq):- is_user_module.
lmconf:wrap_shared(retractall,1,dbreq):- is_user_module.


lmconf:wrap_shared(mtCore,1,ereq).
lmconf:wrap_shared(mtProlog,1,ereq).
lmconf:wrap_shared(tAgent,1,ereq).
lmconf:wrap_shared(mtCycL,1,ereq).
lmconf:wrap_shared(mtExact,1,ereq).
lmconf:wrap_shared(mtGlobal,1,ereq).
lmconf:wrap_shared(nameStrings,1,ereq).


%lmconf:wrap_shared(lmcache:loaded_external_kbs,1,ereq).
lmconf:wrap_shared(argQuotedIsa,3,ereq).

lmconf:wrap_shared(arity,2,ereq).
lmconf:wrap_shared(functorDeclares,1,ereq).

lmconf:wrap_shared(call_OnEachLoad,1,ereq).
lmconf:wrap_shared(completeExtentEnumerable,1,ereq).
lmconf:wrap_shared(completelyAssertedCollection,1,ereq).
lmconf:wrap_shared(constrain_args_pttp,2,ereq).
lmconf:wrap_shared(cycPlus2,2,ereq).
lmconf:wrap_shared(cycPred,2,ereq).
lmconf:wrap_shared(decided_not_was_isa,2,ereq).
lmconf:wrap_shared(genls,2,ereq).
lmconf:wrap_shared(isa,2,ereq).
lmconf:wrap_shared(lambda,5,ereq).

lmconf:wrap_shared(meta_argtypes,1,ereq).
lmconf:wrap_shared(mpred_f,4,ereq).
lmconf:wrap_shared(mpred_f,5,ereq).
lmconf:wrap_shared(mpred_f,6,ereq).
lmconf:wrap_shared(mpred_f,7,ereq).
lmconf:wrap_shared(props,2,ereq).

lmconf:wrap_shared(F,A,ereq):-
   functor(P,F,A),
   member(MVis,[baseKB,lmcache,lmconf]),
   predicate_property(MVis:P,defined),
   \+ predicate_property(MVis:P,imported_from(_)),!.

lmconf:wrap_shared(mpred_mark,3,ereq).
lmconf:wrap_shared(mudKeyword,2,ereq).
lmconf:wrap_shared(pfcControlled,1,ereq).
lmconf:wrap_shared(pfcRHS,1,ereq).

lmconf:wrap_shared(predicateConventionMt,2,ereq).
lmconf:wrap_shared(prologBuiltin,1,ereq).
lmconf:wrap_shared(prologDynamic,1,ereq).
lmconf:wrap_shared(prologHybrid,1,ereq).
lmconf:wrap_shared(prologKIF,1,ereq).
lmconf:wrap_shared(prologMacroHead,1,ereq).
lmconf:wrap_shared(prologPTTP,1,ereq).
lmconf:wrap_shared(prologSideEffects,1,ereq).

lmconf:wrap_shared(resultIsa,2,ereq).
lmconf:wrap_shared(singleValuedInArg,2,ereq).
lmconf:wrap_shared(spft,3,ereq).
lmconf:wrap_shared(support_hilog,2,ereq).
lmconf:wrap_shared(t,3,ereq).
lmconf:wrap_shared(tCol,1,ereq).
lmconf:wrap_shared(tNotForUnboundPredicates,1,ereq).
lmconf:wrap_shared(tPred,1,ereq).
lmconf:wrap_shared(tRelation,1,ereq).
lmconf:wrap_shared(tCol,1,ereq).

lmconf:wrap_shared(ttExpressionType,1,ereq).
lmconf:wrap_shared(ttPredType,1,ereq).
lmconf:wrap_shared(ttTemporalType,1,ereq).
lmconf:wrap_shared(use_ideep_swi,0,ereq).


clause_b(G):- clause(baseKB:G,Body),call(Body).


%% system_goal_expansion_safe_wrap( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%

%system_goal_expansion_safe_wrap(Goal,_):- functor(Goal,F,_),arg(_,v(call_u,call,(/),(',')),F),!,fail.
%system_goal_expansion_safe_wrap(MT:Goal,(call_u(genlMt(abox,GMt)),with_umt(GMt,Goal))):- MT==tbox.

system_goal_expansion_safe_wrap(T,_):- \+ callable(T),!,fail.
system_goal_expansion_safe_wrap(MT:Goal,call_u(Goal)):-compound(Goal),MT==abox,!.
system_goal_expansion_safe_wrap(M:T,O):- callable(T),!,functor(T,F,A),lmconf:wrap_shared(F,A,How),!,safe_wrap(M:T,How,O).
system_goal_expansion_safe_wrap(T,I):- functor(T,F,A),lmconf:wrap_shared(F,A,How),!,safe_wrap(T,How,I).

really_safe_wrap(Type,I,O):- callable(I),
   prolog_load_context(module,M),
   \+ clause_b(mtCycL(M)),
   system_goal_expansion_safe_wrap(I,O)->I\=@=O,dmsg(really_safe_wrap(Type,I,O)).


%% safe_wrap( Term, +How, -Wrapped) is semidet.
%
% Safely Paying Attention To Corner Cases Wrap.
%

safe_wrap(M:I,M:How,call(How,M:I)):-!.
safe_wrap(M:I,How,call(How,M:I)):-!.
safe_wrap(I,M:How,call(How,M:I)):-!.
safe_wrap(I,How,call(How,I)):-!.

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

decl_shared(Plus,M:F/A):- M==baseKB,!,decl_shared(Plus,F/A).

decl_shared(Plus,M:F/A):- check_never_decl_shared(Plus,M,F,A),fail.
decl_shared(Plus,F/A):- check_never_decl_shared(Plus,baseKB,F,A),fail.

decl_shared(Plus,M:F/A):-must(atom(F)),!,
 asserta_if_new(lmconf:wrap_shared(F,A,M:ereq)),
 ignore((integer(A),
   baseKB:multifile(M:F/A),
   baseKB:dynamic(M:F/A),
   baseKB:discontiguous(M:F/A),
   baseKB:public(M:F/A),
   on_f_throw( (M:F/A)\== (baseKB:loaded_external_kbs/1)),
   once((M==baseKB->true;ain(baseKB:predicateConventionMt(F,M)))),
   functor(P,F,A),
      %once(on_f_throw( (M:F/A)\== (lmconf:loaded_external_kbs/1))),
      %once(on_f_throw( (M:F/A)\== (mpred_online:semweb_startup/0))),
      %once(on_f_throw( (M:F/A)\== (lmconf:irc_user_plays/3))),
   call(Plus,M:P),
   % (find_and_call(mtCycL(M))->ain(baseKB:prologHybrid(F));true),
   ain(baseKB:arity(F,A)) )).


decl_shared(Plus,F/A):-atom(F),!,
 asserta_if_new(lmconf:wrap_shared(F,A,ereq)),
 ignore((integer(A),
   baseKB:multifile(F/A),
   baseKB:dynamic(F/A),
   baseKB:discontiguous(F/A),
   baseKB:public(F/A),
     % once(on_f_throw( (F/A)\== (loaded_external_kbs/1))),
     % once(on_f_throw( (F/A)\== (semweb_startup/0))),
     % once(on_f_throw( (F/A)\== (irc_user_plays/3))),
   
   ain(baseKB:arity(F,A)),
   functor(P,F,A),
   call(Plus,P))),!.
 


decl_shared(Plus,M:P):-compound(P),!,functor(P,F,A),F\==(/),F\==(//),!,decl_shared(Plus,M:F/A).
decl_shared(Plus,P):-compound(P),!,functor(P,F,A),F\==(/),F\==(//),!,decl_shared(Plus,F/A).

check_never_decl_shared(_Plus,baseKB,mudComfort,1).


% loading_module 
%:- decl_shared(Plus,arity/2).
%:- decl_shared(Plus,t).
%:- decl_shared(Plus,meta_argtypes/1).


:- dynamic system:goal_expansion/4.
:- multifile system:goal_expansion/4.
:- dynamic system:sub_call_expansion/2.
:- multifile system:sub_call_expansion/2.
:- dynamic system:sub_body_expansion/2.
:- multifile system:sub_body_expansion/2.

swc.

system:sub_call_expansion(I,O):-really_safe_wrap(ce,I,O).
%system:sub_body_expansion(I,O):-really_safe_wrap(be,I,O).
system:body_expansion(I,O):- O\== true, O\=(swc,_),really_safe_wrap(be,I,O).
%system:goal_expansion(I,P,O,P):-really_safe_wrap(ge,I,O).


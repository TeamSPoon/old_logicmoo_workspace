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
          virtualize_source/3,
          virtualize_code/3,
          virtualize_code_fa/5,
          ereq/1,
          dbreq/1,
          swc/0,
          clause_b/1,
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
          virtualize_code/2,
          ereq/1,
          dbreq/1,
          warn_if_static/2)).

:- meta_predicate decl_shared(1,+).
:- meta_predicate decl_shared(+).

:- dynamic(baseKB:safe_wrap/3).



:- multifile(baseKB:col_as_isa/1).
:- multifile(baseKB:col_as_unary/1).
:- multifile(baseKB:col_as_static/1).
:- dynamic(baseKB:col_as_isa/1).
:- dynamic(baseKB:col_as_unary/1).
:- dynamic(baseKB:col_as_static/1).

:- ensure_loaded(logicmoo_util_clause_expansion).
:- ensure_loaded(logicmoo_util_dmsg).
:- ensure_loaded(logicmoo_util_rtrace).


:- dynamic(ereq/1).
:- module_transparent(ereq/1).
ereq(C):- find_and_call(C).

:- dynamic(dbreq/1).
:- module_transparent(dbreq/1).
dbreq(C):- ereq(C).


%% baseKB:safe_wrap( ?VALUE1, :Wrapper, ?VALUE3) is semidet.
%
% Virtualizer Shared Code.
%

get_virtualizer_mode(ge,F,A,HowIn):- baseKB:safe_wrap(F,A,HowOut),!,must(HowIn=HowOut).

% baseKB:safe_wrap(_,_,_):-!,fail.
baseKB:safe_wrap(O,_,_):- bad_functor_check(O),!,fail.
baseKB:safe_wrap(F,A,T):- virtualize_safety(F,A0),A==A0,arg(_,v(on_x_rtrace,on_x_debug),T),current_predicate(T/1).
baseKB:safe_wrap(F,_,ereq):- never_virtualize(F),!,fail.
baseKB:safe_wrap(F,A,dbreq):- is_user_module, virtualize_dbreq(F,A).
baseKB:safe_wrap(F,A,ereq):- virtualize_ereq(F,A).
baseKB:safe_wrap(COL,_,ereq):- clause_b(col_as_isa(COL)).
%baseKB:safe_wrap(F,A,on_x_log_throw):- virtualize_safety(F,A0),A==A0.
baseKB:safe_wrap(F,_,_):- clause_b(prologBuiltin(F)),!,fail.
baseKB:safe_wrap(F,A,call_u):- find_and_call(hybrid_support(F,A)).
baseKB:safe_wrap(COL,_,ereq):- clause_b(col_as_unary(COL)).

baseKB:safe_wrap(F,A,ereq):- atom(F),integer(A),
   functor(Goal,F,A),
   % member(M,[baseKB,lmcache,lmconf]),
   baseKB = M,
   predicate_property(M:Goal,defined),
   \+ predicate_property(M:Goal,static),
   \+ predicate_property(M:Goal,imported_from(_)),!.

% Preds that we''d like to know a little more than "instanciation exception"s


bad_functor_check(O):-var(O).
bad_functor_check(:):- !,dumpST,dtrace.
%bad_functor_check(/):- !,dumpST,dtrace.
%bad_functor_check(//):- !,dumpST,dtrace.


virtualize_safety(O):- bad_functor_check(O),!,fail.
virtualize_safety((=..),2).
virtualize_safety(functor,3).
virtualize_safety(arg,3).
virtualize_safety(is,2).

% Preds that we assume indicate we''d already passed over it


never_virtualize(O):- bad_functor_check(O),!,fail.
never_virtualize(ereq).
never_virtualize(call_u).
never_virtualize(clause_u).
never_virtualize(lookup_u).
never_virtualize(dbreq).
never_virtualize(clause_b).
never_virtualize(('.')).
never_virtualize(('[]')).
never_virtualize(('[|]')).
never_virtualize((/)).
never_virtualize((//)).
never_virtualize(add).
never_virtualize(del).
never_virtualize(clr).
never_virtualize(ain).
never_virtualize(props).
never_virtualize(on_x_rtrace).
never_virtualize(on_x_debug).
never_virtualize(aina).
never_virtualize(decl_shared).

never_virtualize(ainz).
never_virtualize(apply).
never_virtualize(call).
never_virtualize(F):- ereq(mpred_mark(pfcBuiltin,F,_)).

% operations to transactionalize
virtualize_dbreq(O,_):- bad_functor_check(O),!,fail.
virtualize_dbreq(assert,1).
virtualize_dbreq(assert,2).
virtualize_dbreq(asserta,1).
virtualize_dbreq(asserta,2).
virtualize_dbreq(assertz,1).
virtualize_dbreq(assertz,2).
virtualize_dbreq(clause,2).
virtualize_dbreq(clause,3).
virtualize_dbreq(retract,1).
virtualize_dbreq(retractall,1).

virtualize_dbreq(recorda,_).
virtualize_dbreq(recordz,_).
virtualize_dbreq(recorded,_).
virtualize_dbreq(erase,1).




virtualize_ereq(O,_):- bad_functor_check(O),!,fail.

%virtualize_ereq(lmcache:loaded_external_kbs,1).


virtualize_ereq(isa,2).
virtualize_ereq(genls,2).
virtualize_ereq(t,_).
virtualize_ereq(t,3).

virtualize_ereq(functorDeclares,1).

virtualize_ereq(mtCore,1).
virtualize_ereq(mtProlog,1).
virtualize_ereq(mtCycL,1).
virtualize_ereq(mtExact,1).
virtualize_ereq(mtGlobal,1).
virtualize_ereq(nameStrings,1).


virtualize_ereq(arity,2).
virtualize_ereq(argIsa,3).
virtualize_ereq(argQuotedIsa,3).



virtualize_ereq(call_OnEachLoad,1).
virtualize_ereq(completeExtentEnumerable,1).
virtualize_ereq(completelyAssertedCollection,1).
virtualize_ereq(constrain_args_pttp,2).
virtualize_ereq(cycPlus2,2).
virtualize_ereq(cycPred,2).
virtualize_ereq(decided_not_was_isa,2).
virtualize_ereq(lambda,5).

virtualize_ereq(meta_argtypes,1).
virtualize_ereq(mpred_f,4).
virtualize_ereq(mpred_f,5).
virtualize_ereq(mpred_f,6).
virtualize_ereq(mpred_f,7).
virtualize_ereq(props,2).

virtualize_ereq(mpred_mark,3).
virtualize_ereq(mudKeyword,2).

virtualize_ereq(pfcControlled,1).
virtualize_ereq(pfcRHS,1).
virtualize_ereq(predicateConventionMt,2).
virtualize_ereq(prologBuiltin,1).
virtualize_ereq(prologDynamic,1).
virtualize_ereq(prologHybrid,1).
virtualize_ereq(prologKIF,1).
virtualize_ereq(prologMacroHead,1).
virtualize_ereq(prologPTTP,1).
virtualize_ereq(prologSideEffects,1).

virtualize_ereq(resultIsa,2).
virtualize_ereq(singleValuedInArg,2).
virtualize_ereq(spft,3).
virtualize_ereq(support_hilog,2).
virtualize_ereq(tCol,1).
virtualize_ereq(tNotForUnboundPredicates,1).
virtualize_ereq(tPred,1).
virtualize_ereq(tRelation,1).
virtualize_ereq(tAgent,1).
virtualize_ereq(tCol,1).

virtualize_ereq(ttExpressionType,1).
virtualize_ereq(ttPredType,1).
virtualize_ereq(ttTemporalType,1).
virtualize_ereq(use_ideep_swi,0).
virtualize_ereq(==>,_).
virtualize_ereq(<==>,_).
virtualize_ereq((<--),2).
virtualize_ereq(agent_text_command,_).
virtualize_ereq(agent_command,_).
virtualize_ereq(coerce_hook,_).

:- dynamic(baseKB:t/2).

%% clause_b( ?C) is semidet.
%
% Clause User Microtheory.
%
%clause_b(M:Goal):-  !,clause(M:Goal,true).
%clause_b(Goal):-  !,clause(baseKB:Goal,true).

% lookup_u/cheaply_u/call_u/clause_b
clause_b(Goal):-  baseKB:call(call,Goal).
% clause_b(Goal):-  baseKB:clause(Goal,B),baseKB:call(B).

%clause_b(Goal):-  baseKB:clause(Goal,B)*->call(B);clause_b0(Goal).
%clause_b(Goal):-  baseKB:clause(Goal,true)*->true;clause_b0(Goal).

% clause_b0(Goal):- if_defined(to_was_isa(clause_b,Goal,P0),fail),!,Goal\=P0,baseKB:clause(P0,true).

%clause_b(M:C):-!,clause(M:C,true).
%clause_b(C):- call_u(clause(C,true)).
%clause_b(C):-!,clause(_:C,true).
%clause_b(Goal):-  Goal=..[C,PART],!,baseKB:t(C,PART).
%clause_b(Goal):-  current_predicate(_,baseKB:Goal),!,loop_check(baseKB:Goal).
% clause_b(Goal):- clause(baseKB:Goal,Body),(Body==true->true;call_u(Body)).


%% virtualize_code(X, :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.f$
%

%virtualize_code(X,Goal,_):- functor(Goal,F,_),arg(_,v(call_u,call,(/),(',')),F),!,fail.
%virtualize_code(X,M:Goal,(call_u(genlMt(abox,GMt)),with_umt(GMt,Goal))):- M==tbox.

virtualize_args_as(Goal,Args):- sanity((arg(1,Goal,Var),var(Var))), predicate_property(Goal,meta_predicate(Args)).
virtualize_args_as(Goal,_):-predicate_property(Goal,built_in),!,fail.
virtualize_args_as(Goal,Goal):-predicate_property(Goal,transparent),!.
virtualize_args_as(Which,Args):- descend_ge(Which),Args=Which.

descend_ge(':-'((-),0)).
descend_ge(( :- 0)).
descend_ge('{}'(0)).
descend_ge('==>'(-,-)).
descend_ge('==>'(-)).
descend_ge('<--'(-,-)).
descend_ge(z(if)).
descend_ge(z(_)):-!,fail.
descend_ge(Which):-functor(Which,F,_),!,descend_ge(z(F)),!.


cannot_decend_expansion(_,In):- \+ compound(In),!.
cannot_decend_expansion(X,_:In):-!,cannot_decend_expansion(X,In).
cannot_decend_expansion(ge,In):- functor(In,F,_),never_virtualize(F).


virtualize_code(X,In,_):- cannot_decend_expansion(X,In),!,fail.
virtualize_code(ge,M:In,ereq(In)):- M==abox,!.
virtualize_code(ge,M:In,M:In):- atom(M),callable(In),(predicate_property(M:In,volatile);predicate_property(M:In,thread_local)),!.
virtualize_code(ge,(SWC,ALL),ereq((SWC,ALL))):- SWC==swc,!.
virtualize_code(X,(G1,G2),(O1,O2)):-!,virtualize_code(X,G1,O1),!,virtualize_code(X,G2,O2),!.
virtualize_code(X,M:In,Out):- functor(In,F,A),virtualize_code_fa(X,M:In,F,A,Out).
virtualize_code(X,M:In,M:Out):-!, virtualize_code(X,In,Out).
virtualize_code(X,In,PART):- functor(In,F,A),virtualize_code_fa(X,In,F,A,PART).

virtualize_code_fa(X,In,F,A,PART):- get_virtualizer_mode(X,F,A,How),!,safe_virtualize(In,How,PART).
virtualize_code_fa(X,M:In,F,A,M:PART):-!,virtualize_code_fa(X,In,F,A,PART).
virtualize_code_fa(X,In,F,A,PART):- X==ge, functor(ArgModes,F,A),
  Args=ArgModes,
  virtualize_args_as(Args,ArgModes),!, 
  map_compound_args(virtualize_code_each(X),ArgModes,In,PART),!.

% virtualize_code(X,In,Out):- compound(In), virtualize_special_outside(X,In),!,Out=ereq(In).

virtualize_special_outside(X,In):- functor(In,F,A),get_virtualizer_mode(X,F,A,_How),!.
virtualize_special_outside(X,In):- arg(_,In,Arg), \+cannot_decend_expansion(X,Arg),virtualize_special_outside(X,In).

virtualize_code_each(X,Arg,In,Out):- var(Arg),!,virtualize_code_each(X,(+),In,Out).
virtualize_code_each(X,Arg,In,Out):- (integer(Arg); Arg == + ) -> virtualize_code(X,In,Out),!.
virtualize_code_each(X,-,In,Out):- if_defined(fully_expand_head(X,In,Out)),!.
virtualize_code_each(_,_,In,Out):- must(Out=In).

map_compound_args(Pred,Args,In,Out):- must(( compound(Args), compound(In), Args=..[_|ArgsL],In=..[F|InL],maplist(Pred,ArgsL,InL,OutL),Out=..[F|OutL])).

could_safe_virtualize:- prolog_load_context(module,M),\+ clause_b(mtCycL(M)),
     \+ ((current_prolog_flag(dialect_pfc,true); 
       (source_location(F,_W),( atom_concat(_,'.pfc.pl',F);atom_concat(_,'.plmoo',F);atom_concat(_,'.pfc',F))))).

virtualize_source(X,In,Out):-  current_prolog_flag(unsafe_speedups,true),!, 
   (virtualize_code(X,In,Out)->In\=@=Out,dmsg(virtualize_source(X,(In))-->Out)).

virtualize_source(X,In,Out):- callable(In),
 term_variables(In,List),
  (List==[] -> (virtualize_code(X,In,Out)->In\=@=Out,dmsg(virtualize_source(X,(In))-->Out));
 setup_call_cleanup(maplist(lock_vars,List),
   (virtualize_code(X,In,Out)->In\=@=Out,dmsg(virtualize_source(X,In)-->Out)),
   maplist(unlock_vars,List))).


%% safe_virtualize( Term, +How, -Wrapped) is semidet.
%
% Safely Paying Attention To Corner Cases Wrap.
%

safe_virtualize(Goal,How,Out):- must(safe_virtualize_0(Goal,How,call(MHow,MGoal))),!, Out=..[MHow,MGoal].

safe_virtualize_0(M:Goal,M:How,call(How,M:Goal)).
safe_virtualize_0(M:Goal,How,call(How,M:Goal)).
safe_virtualize_0(Goal,M:How,call(How,M:Goal)).
safe_virtualize_0(Goal,How,call(How,Goal)).

warn_if_static(F,A):- 
 ignore((F\={},
  functor(Goal,F,A),
  is_static_predicate(F/A),
  listing(Goal),
  trace_or_throw(warn(pfcPosTrigger,Goal,static)))).


decl_shared(Expr):- decl_shared(nop,Expr),!.

%% decl_shared_plus(Cl, TermM) is semidet.
%
% Declare Shared.
%
decl_shared(Cl,Var):-var(Var),!,trace_or_throw(var_decl_shared(Cl,Var)).
decl_shared(Cl,baseKB:FA):-!,decl_shared(Cl,FA),!.
decl_shared(Cl,abox:FA):-!,decl_shared(Cl,FA),!.

decl_shared(Cl,(G1,G2)):-!,decl_shared(Cl,G1),!,decl_shared(Cl,G2),!.
decl_shared(Cl,[G1]):-!,decl_shared(Cl,G1),!.
decl_shared(Cl,[G1|G2]):-!,decl_shared(Cl,G1),!,decl_shared(Cl,G2),!.
decl_shared(Cl,M:(G1,G2)):-!,decl_shared(Cl,M:G1),!,decl_shared(Cl,M:G2),!.
decl_shared(Cl,M:[G1]):-!,decl_shared(Cl,M:G1),!.
decl_shared(Cl,M:[G1|G2]):-!,decl_shared(Cl,M:G1),!,decl_shared(Cl,M:G2),!.
decl_shared(Cl,_:M:G1):-!,decl_shared(Cl,M:G1),!.
decl_shared(Cl,F):-atom(F),!,decl_shared(Cl,F/_).
decl_shared(Cl,M:F):-atom(F),!,decl_shared(Cl,M:F/_).

decl_shared(Cl,M:F/G1):- M==baseKB,!,decl_shared(Cl,F/G1).

decl_shared(Cl,M:F/G1):- check_never_decl_shared(Cl,M,F,G1),fail.
decl_shared(Cl,F/G1):- check_never_decl_shared(Cl,baseKB,F,G1),fail.

decl_shared(Cl,M:F/A):-must(atom(F)),!,
 asserta_if_new(baseKB:safe_wrap(F,A,M:ereq)),
 ignore((integer(A),
   baseKB:multifile(M:F/A),
   baseKB:dynamic(M:F/A),
   baseKB:discontiguous(M:F/A),
   baseKB:public(M:F/A),
   % on_f_throw( (M:F/A)\== (lmcache:loaded_external_kbs/1)),
   once((M==baseKB->true;ain(baseKB:predicateConventionMt(F,M)))),
   functor(Goal,F,A),
      %once(on_f_throw( (M:F/A)\== (lmcache:loaded_external_kbs/1))),
      %once(on_f_throw( (M:F/A)\== (mpred_online:semweb_startup/0))),
      %once(on_f_throw( (M:F/A)\== (baseKB:irc_user_plays/3))),
   call(Cl,M:Goal),
   % (find_and_call(mtCycL(M))->ain(baseKB:prologHybrid(F));true),
   ain(baseKB:arity(F,A)) )).


decl_shared(Cl,F/A):-atom(F),!,
 asserta_if_new(baseKB:safe_wrap(F,A,ereq)),
 ignore((integer(A),
   baseKB:multifile(F/A),
   baseKB:dynamic(F/A),
   baseKB:discontiguous(F/A),
   baseKB:public(F/A),
     % once(on_f_throw( (F/A)\== (loaded_external_kbs/1))),
     % once(on_f_throw( (F/A)\== (semweb_startup/0))),
     % once(on_f_throw( (F/A)\== (irc_user_plays/3))),
   
   ain(baseKB:arity(F,A)),
   functor(Goal,F,A),
   call(Cl,Goal))),!.
 


decl_shared(Cl,M:Goal):-compound(Goal),!,functor(Goal,F,A),F\==(/),F\==(//),!,decl_shared(Cl,M:F/A).
decl_shared(Cl,Goal):-compound(Goal),!,functor(Goal,F,A),F\==(/),F\==(//),!,decl_shared(Cl,F/A).

check_never_decl_shared(_Plus,baseKB,mudComfort,1).


% loading_module 
%:- decl_shared(Cl,arity/2).
%:- decl_shared(Cl,t).
%:- decl_shared(Cl,meta_argtypes/1).


:- dynamic system:goal_expansion/4.
:- multifile system:goal_expansion/4.
:- dynamic system:sub_call_expansion/2.
:- multifile system:sub_call_expansion/2.
:- dynamic system:sub_body_expansion/2.
:- multifile system:sub_body_expansion/2.

swc.


%system:sub_body_expansion(In,Out):-virtualize_source(be,In,Out).
%system:sub_body_expansion(In,Out):- Out\== true, Out\=(cwc,_),could_safe_virtualize,virtualize_source(be,In,Out).
%system:sub_call_expansion(In,Out):-virtualize_source(ce,In,Out).
system:goal_expansion(In,Goal,Out,Goal):- callable(Goal), \+ current_prolog_flag(xref,true), In\== true, In\=(cwc,_),current_prolog_flag(lm_expanders,true), virtualize_source(ge,In,Out)-> In\=Out.
%system:term_expansion(In,Goal,Out,Goal):- In\== true, In\=(cwc,_),current_prolog_flag(lm_expanders,true), virtualize_source(te,In,Out)-> In\=Out.


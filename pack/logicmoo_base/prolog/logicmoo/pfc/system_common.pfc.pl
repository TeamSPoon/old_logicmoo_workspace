/* <module>
% =============================================
% File 'system_common.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% pain(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm).
%
%                      ANTECEEDANT                                   CONSEQUENT
%
%         P =         test nesc true                         assert(P),retract(~P) , enable(P).
%       ~ P =         test nesc false                        assert(~P),retract(P), disable(P)
%
%   ~ ~(P) =         test possible (via not impossible)      retract( ~(P)), enable(P).
%  \+ ~(P) =         test impossiblity is unknown            retract( ~(P))
%   ~ \+(P) =        same as P                               same as P
%     \+(P) =        test naf(P)                             retract(P)
%
% Dec 13, 2035
% Douglas Miles
*/

% :- require('system_base.pfc').

:- begin_pfc.

:- prolog_load_context(module,Mod),sanity(Mod==baseKB),writeq(prolog_load_context(module,Mod)),nl.


arity(genlPreds,2).

:- dynamic(mpred_undo_sys/3).
pfcControlled(mpred_undo_sys(ftAssertion, ftCallable, ftCallable)).
mpred_undo_sys(P, WhenAdded, WhenRemoved) ==> (P ==> {WhenAdded}), mpred_do_and_undo_method(WhenAdded,WhenRemoved).

% DONT mpred_undo_sys(added(P),ain(P),mpred_retract(P)).
% mpred_undo_sys(asserted(P),assert_eq_quitely(PE),retract_eq_quitely(PE)):-expand_goal(P,PE).

% 
comment(isa,"Instance of").

~(tCol(genlPreds)).

~(singleValuedInArg(arity,2)).
~(prologSingleValued(arity)).
~(prologSingleValued(support_hilog)).

~(arity(argIsa,1)).
arity(pddlObjects,2).

prologHybrid(genls/2).


/*
:- 
 With = kb_dynamic, % [multifile,kb_dynamic,discontiguous],
 with_pfa(With,((logical_functor_pttp/1, pfcControlled/1, pfcRHS/1,  conflict/1,   argsQuoted/1,     add_args/15,argIsa_known/3,call_mt_t/11))),

% with_pfa(With,((( call_which_t/9,constrain_args_pttp/2,contract_output_proof/2,get_clause_vars_for_print/2,holds_f_p2/2,input_to_forms/2,is_wrapper_pred/1,lambda/5,mpred_f/1,
%          pp_i2tml_now/1,pp_item_html/2,pttp1a_wid/3,pttp_builtin/2,pttp_nnf_pre_clean_functor/3,quasiQuote/1,relax_term/6,retractall_wid/1,ruleRewrite/2,search/7,support_hilog/2,svar_fixvarname/2)))),

% with_pfa(With,((pfcControlled/1,pfcRHS/1,logical_functor_pttp/1,          add_args/15,argIsa_known/3,call_mt_t/11,call_which_t/9,constrain_args_pttp/2,contract_output_proof/2,get_clause_vars_for_print/2,holds_f_p2/2,input_to_forms/2,is_wrapper_pred/1,lambda/5,mpred_f/1,pp_i2tml_now/1,pp_item_html/2,pttp1a_wid/3,pttp_builtin/2,pttp_nnf_pre_clean_functor/3,
%          quasiQuote/1,relax_term/6,retractall_wid/1,ruleRewrite/2,search/7,support_hilog/2,svar_fixvarname/2,rtNotForUnboundPredicates/1))),
 with_pfa(With,(((bt/2),(nt/3),(pk/3),(pt/2),(spft/3),(tms/1),(hs/1),(que/1),(pm/1),
          (('==>')/1),(('::::')/2),(('<-')/2),(('<==>')/2),(('==>')/2),(('~')/1),(('nesc')/1),((mpred_action)/1),
          (mpred_do_and_undo_method/2),
	  prologMultiValued/1,prologOrdered/1,prologNegByFailure/1,prologPTTP/1,prologKIF/1,pfcControlled/1,ttRelationType/1,
           prologHybrid/1,predCanHaveSingletons/1,prologDynamic/1,prologBuiltin/1,prologMacroHead/1,prologListValued/1,prologSingleValued/1,
          (hs/1),(pfcControlled/1),(prologDynamic/2),(prologSideEffects/1),(prologSingleValued/1),(singleValuedInArg/2),(prologSideEffects/1,
          prologMacroHead/1,pfcControlled/1,
           resolveConflict/1,resolverConflict_robot/1)))),
 with_pfa(With,((mpred_isa/2,arity/2,predicateConventionMt/2))),
 with_pfa(With,((vtColor/1))).
 */

:-  multifile((disjointWith/2,genls/2,isa/2,argIsa/3)).
:-  dynamic((disjointWith/2,genls/2,isa/2,argIsa/3)).
:- discontiguous((disjointWith/2,genls/2,isa/2,argIsa/3,typeGenls/2)).

% :- autoload.



/*
 %  % :- debug_logicmoo(_).
:- nodebug_logicmoo(http(_)).
:- debug_logicmoo(logicmoo(_)).
:- mpred_trace_exec.
:- begin_pfc.

*/

% remove conflicts early 
% (~(P)/mpred_non_neg_literal(P) ==> ( {mpred_remove(P)}, (\+P ))).

:- ain((never_retract_u(~(X),is_ftVar(X)):- cwc,is_ftVar(X))).

/*
Unneeded yet

% These next 2 might be best as builtins?
((~(P)/(mpred_non_neg_literal(P),copy_term(P,PP))) ==> \+ PP ).
% (P/mpred_non_neg_literal(P) ==> (\+ ~(P))).
% ((P/(mpred_non_neg_literal(P),copy_term(P,PP))) ==> (~ ~ PP )).

% :- nortrace,cnotrace.
% a pretty basic conflict (disabled for now)
%(~(P)/mpred_non_neg_literal(P), P) ==> conflict(~(P)).
(~(P)/mpred_non_neg_literal(P), P) ==> conflict(~(P)).
%(P/mpred_non_neg_literal(P), ~(P)) ==> conflict(P).

*/

%:- rtrace,dtrace.
==>(prologBuiltin(mpred_select/2)).
% :- nortrace,cnotrace.

:- kb_dynamic(conflict/1).
% a conflict triggers a Prolog action to resolve it.
conflict(C) ==> {must(with_mpred_trace_exec((resolveConflict(C),\+conflict(C))))}.

% :- kb_dynamic(ttTypeType/1).

% meta rules to schedule inferencing.
% resolve conflicts asap
% mpred_select(conflict(X),W) :- que(conflict(X),W).

:- dynamic(rtUnaryPredicate/1).
:- dynamic(ttExpressionType/1).

{type_prefix(_Prefix,Type)}==>tCol(Type).
{type_suffix(_Suffix,Type)}==>tCol(Type).
tSet(completelyAssertedCollection).


%% prologBuiltin( ?ARG1, ?ARG2) is semidet.
%
% Prolog Builtin.
%
%WRONG prologBuiltin(resolveConflict/1,predicateConventionMt(baseKB)).
%WRONG prologBuiltin(mpred_select/2,predicateConventionMt(baseKB)).
%:-rtrace.
prologBuiltin(agent_text_command/4,prologDynamic).
%tPred(t,prologDynamic).

% tPred(member/2,prologBuiltin).

tSet(rtNotForUnboundPredicates).


==>tCol(vtVerb).

:- sanity(fileAssertMt(baseKB)).
:- sanity(defaultAssertMt(baseKB)).

ttRelationType(rtNotForUnboundPredicates).
rtNotForUnboundPredicates(member).

%and(A,B):- cwc, call_u((A,B)).
%or(A,B):- cwc, call_u((A;B)).


:- fully_expand(pkif("
(==> 
    (and 
      (typeGenls ?COLTYPE1 ?COL1) 
      (typeGenls ?COLTYPE2 ?COL2) 
      (disjointWith ?COL1 ?COL2)) 
    (disjointWith ?COLTYPE1 ?COLTYPE2))"
),O),dmsg(O).

:-must(ain(pkif("
(==> 
    (and 
      (typeGenls ?COLTYPE1 ?COL1) 
      (typeGenls ?COLTYPE2 ?COL2) 
      (disjointWith ?COL1 ?COL2)) 
    (disjointWith ?COLTYPE1 ?COLTYPE2))"
))).


/*
?- isa(iExplorer2,W),
*/

% ===================================================================
% Type checker system / Never Assert / Retraction checks
% ===================================================================

(sometimesTypeCheck, typeCheckDecl(Each,Must), Each, {\+ Must}) ==> failed_typeCheckDecl(Each,Must).

failed_typeCheckDecl(Each,Must)==>{trace_or_throw(failed_typeCheckDecl(Each,Must))}.

never_assert_u(vtVerb(BAD),vtVerbError):-fail,BAD=='[|]'.
never_assert_u(prologSingleValued(BAD),var_prologSingleValued(BAD)):-is_ftVar(BAD).

never_assert_u(baseKB:mtProlog(baseKB),must(mtCycL(baseKB))).

never_assert_u(A,test_sanity(A)):-never_assert_u(A).

never_retract_u(A,test_sanity(A)):-never_retract_u(A).

never_retract_u(X,is_ftVar(X)):-is_ftVar(X).
never_retract_u(human(trudy),sanity_test).
never_retract_u(tHumanHair(skRelationAllExistsFn(mudSubPart, skRelationAllExistsFn(mudSubPart, skRelationAllExistsFn(mudSubPart, iExplorer1, tHumanBody), tHumanHead), tHumanHair)),sanity_test).
never_retract_u((father(skArg1ofFatherFn(trudy), trudy)),sanity_test).
never_retract_u(argQuotedIsa(thereExistAtLeast, 1, ftPositiveInteger),sanity_test).

% P/never_assert_u(P,Why) ==> conflict(never_assert_u(P,Why))

prologHybrid(arity/2).
prologDynamic(is_never_type/1).
prologDynamic(term_expansion/2).
prologBuiltin(var/1).

completelyAssertedCollection(completelyAssertedCollection).
completelyAssertedCollection(C)==>tCol(C).

% A Type Specification
completelyAssertedCollection(tCol).  % a type is a type
completelyDecidableCollection(ftSpec). % A specification is sort of a type

:- multifile ftSpec/1.
:- dynamic ftSpec/1.
:- discontiguous ftSpec/1.
tCol(ftSpec).
ttExpressionType(ftSpec).
~meta_argtypes(ftSpec(ftString)). % A string may not be considered a legal specification
functorForFtSpec(prologEquality).

resultIsa(_F,C)/ground(C)==>ftSpec(C).
argsQuoted(argsQuoted).
argsQuoted(ftSpec).
ftSpec(vtActionTemplate).
argsQuoted(vtActionTemplate).

% ftSpec(ftSpec).
meta_argtypes(ftSpec(meta_argtypes(ftSpec))).  % An argtype specification is considered to be a legal specification

genls(ttExpressionType, ftSpec).

genls(tCol, ftSpec).
meta_argtypes(ftSpec(tCol)).  % A typeclass is considered to be a legal specification

~tIndividual(tIndividual).

pfcControlled(P),arity(P,A)==>hybrid_support(P,A).

ttRelationType(X)==>tCol(X).
ttRelationType(X)/atom(X)==>(arity(X,1),pfcControlled(X)).

tSet(ttExpressionType).


% tCols are either syntactic or existential
completelyAssertedCollection(ttExpressionType).  % syntactic
completelyAssertedCollection(tSet). % existential

%ttExpressionType(T)==>completelyDecidableCollection(T).

% relations are predsor functions
completelyAssertedCollection(tRelation).
completelyAssertedCollection(tPred).
completelyAssertedCollection(tFunction).

completelyAssertedCollection(prologMacroHead).  % Items read from a file might be a special Macro Head
completelyAssertedCollection(ttRelationType).  % Or they might be a predciate declarer
completelyAssertedCollection(functorDeclares).  % or they might declare other things
% completelyAssertedCollection(prologMacroHead).  % or they might declare other things

:- ((ain((completelyAssertedCollection(isEach(tPred,prologMultiValued,prologOrdered,predIsFlag,tAvoidForwardChain,
 prologNegByFailure,meta_argtypes,prologHybrid,prologPTTP,prologDynamic,prologKIF,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued)))))).


completelyDecidableCollection(ftTerm).
completelyAssertedCollection(meta_argtypes).
completelyAssertedCollection(pfcControlled).
completelyAssertedCollection(predCanHaveSingletons).
completelyAssertedCollection(prologHybrid).
completelyAssertedCollection(tTemporalThing).
completelyAssertedCollection(prologMultiValued).
completelyAssertedCollection(prologDynamic).
completelyAssertedCollection(prologSideEffects).
completelyAssertedCollection(prologNegByFailure).
completelyAssertedCollection(prologSingleValued).
completelyAssertedCollection(tInferInstanceFromArgType).
completelyAssertedCollection(ttNotTemporalType).
completelyAssertedCollection(ttSpatialType).
completelyAssertedCollection(ttTemporalType).
completelyAssertedCollection(ttTypeType).
completelyAssertedCollection(ttUnverifiableType).

prologNegByFailure(quotedIsa).
~prologNegByFailure(isa).

ttRelationType(pfcDatabaseTerm).
ttRelationType(pfcControlled).
ttRelationType(prologSingleValued).


ttRelationType(pfcWatched).
ttRelationType(pfcCreates).
:- dynamic(pfcNegTrigger/1).
ttRelationType(pfcNegTrigger).
ttRelationType(pfcPosTrigger).
ttRelationType(pfcBcTrigger).
ttRelationType(pfcRHS).

ttRelationType(pfcMustFC).

((ttRelationType(X)/atom(X)) ==>support_hilog(X,1)).


ttRelationType(P)==>(tCol(P),completelyAssertedCollection(P)).

:- dynamic(baseKB:ttTypeType/1).

ttTypeType(C)==>completelyAssertedCollection(C).

%% mpred_univ( ?C, ?I, ?Head) is semidet.
%
% Managed Predicate Univ.
%
% TODO decide if still needed 
mpred_univ(C,I,Head):- cwc,atom(C),!,Head=..[C,I],predicate_property(Head,number_of_clauses(_)).

(someTimesBuggy2ndOrder,genlPreds(C1,C2),arity(C1,2)) ==>
  {P1 =.. [C1,X,Y],
    P2 =.. [C2,X,Y]},
  clif(P1 => P2).

(someTimesBuggy2ndOrder,genlPreds(C1,C2),arity(C1,3)) ==>
  {P1 =.. [C1,X,Y,Z],
    P2 =.. [C2,X,Y,Z]},
  clif(P1 => P2).

tCol(tCol).  % = isa(tCol,tCol).
tCol(tSet).

(genls(C,SC)==>(tCol(C),tCol(SC))).

% tSet(C)/(atom(C),TCI=..[C,I]) ==> (arity(C,1),mpred_univ(C,I,TCI),


% :- prolog.
% tPred
ttRelationType(isEach(pfcDatabaseTerm,pfcControlled,pfcWatched,pfcMustFC,predIsFlag,prologMultiValued,
 pfcBcTrigger,
 prologSingleValued,prologMacroHead,notAssertable,prologBuiltin,prologDynamic,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,prologEquality,prologPTTP,
 prologSideEffects,prologHybrid,prologListValued)).

completelyAssertedCollection(isEach(tCol,tPred,pfcControlled)).
ttRelationType(C)==>completelyAssertedCollection(C).

% genls(meta_argtypes,ftSpec).

:- dynamic(baseKB:mudWielding/2).

prologMultiValued(P)==> \+ prologSingleValued(P).
prologSingleValued(P)==> \+ prologMultiValued(P).

~(ttExpressionType(prologEquality)).
ttRelationType(prologEquality).
prologEquality(mudEquals).
prologEquality(('=')).
prologEquality(('==')).

~(isa((','), prologEquality)).

tSet(isEach(tCol,tPred,pfcControlled)).
tSet(meta_argtypes).
tSet(prologMacroHead).
tSet(functorDeclares).
ttRelationType(predIsFlag).
ttRelationType(prologDynamic).
ttRelationType(prologHybrid).
ttRelationType(pfcControlled).
ttRelationType(prologKIF).
ttRelationType(prologBuiltin).
ttRelationType(prologPTTP).

pfcControlled(genlPreds).
pfcControlled(isa).
pfcControlled(argIsa).


%tCol(X) ==> isa(X,tCol).
%tCol(X) ==> ruleRewrite(t(X,I),isa(I,X)).


%typeProps(tCoffeeCup,[mudColor(vBlack),mudColor(isEach(vBrown,vBlack)),mudSize(vSmall),mudShape(vCuplike),mudMaterial(vGlass),mudTexture(vSmooth)]).
%props(iCoffeeCup7,[mudColor(vBlack),mudColor(isEach(vBrown,vBlack)),mudSize(vSmall),mudShape(vCuplike),mudMaterial(vGlass),mudTexture(vSmooth)]).

:- sanity(get_lang(pfc)).

% tCol(C)/(\+ never_isa_syntax(C))==>{decl_as_isa(C)}.

%underkill - Though it is making bad things happen 
ttExpressionType(C)==> \+ completelyAssertedCollection(C).

tSet(rtNotForUnboundPredicates).
tSet(tPred).
tSet(prologBuiltin).
tSet(tFunction).
tSet(tRelation).
tSet(ttTemporalType).
tSet(prologMacroHead).


ttModule(tSourceCode,mudToCyc('ComputerCode'),comment("Source code files containing callable features")).
ttModule(tSourceData,mudToCyc('PropositionalInformationThing'),comment("Source data files containing world state information")).

prologHybrid(isLoadedType(ttModule),pfcControlled).
prologHybrid(isLoaded(tMicrotheory),pfcControlled).

isLoaded(Thing),isa(Thing,ModType), \+ ttExpressionType(ModType) ==> isLoadedType(ModType).

pfcControlled(prologArity(tRelation,ftInt)).
pfcControlled(isa(ftTerm,tCol)).

tSet(tSet).
tSet(tCol).
tSet(ttModule).
tFixedArityRelation(tSet).
tFixedArityRelation(tCol).
ttRelationType(prologHybrid).

:- check_clause_counts.

%:- rtrace,trace.
%:- notrace, nortrace.

prologHybrid(mudToCyc(ftTerm,ftTerm)).

:- must(arity(mudToCyc,2)).

% col_as_isa(X)==>tFixedArityRelation(X),arity(X,1).
col_as_unary(X)==>tFixedArityRelation(X),arity(X,1).

tSet(ttExpressionType).
tSet(completelyAssertedCollection).


ttExpressionType(C) ==> ( \+ completelyAssertedCollection(C), ~ tSet(C), tCol(C)).

((tCol(C)/( \+ ttExpressionType(C))) ==> tSet(C)).

:- sanity(get_lang(pfc)).
%WEIRD ~(tCol(C))/completelyAssertedCollection(C)==> \+ completelyAssertedCollection(C).
% EASIER
% ~tCol(C) ==> ~completelyAssertedCollection(C).
% (tCol(C),\+ ttExpressionType(C)) ==> tSet(C).
% ((tCol(P), \+ ttExpressionType(P)) <==> tSet(P)).

tSet(C) ==>
({ignore((
  % kb_dynamic(C/1),
  % dynamic(C/1),
  % wdmsg(c_tSet(C)),
  atom(C),
  ( \+ is_static_predicate(C/1)),
  functor(Head,C,1),
  call(BHead=baseKB:Head),
  ( \+(predicate_property(BHead,_))-> kb_dynamic(C/1); true),
  nop(predicate_property(BHead,dynamic)->true;show_pred_info(BHead))))},
  functorDeclares(C),
  pfcControlled(C),
  \+ ttExpressionType(C),
  tCol(C),
  arity(C,1)).

/*
tSet(C)==>
 ( {atom(C), functor(Head,C,1), call(BHead=baseKB:Head),
   ( \+(predicate_property(BHead,_))-> kb_dynamic(C/1); true),
    nop(predicate_property(BHead,dynamic)->true;show_pred_info(BHead))},
   functorDeclares(C),
   pfcControlled(C),
   arity(C,1)).
*/


/*
tSet(C)/(atom(C),TCI=..[C,I]) ==> (arity(C,1),
 % mpred_univ(C,I,TCI),
 {call_u((decl_type(C), 
  ignore((
   \+ is_static_predicate(C/1),
   kb_dynamic(C/1),
   \+ completelyAssertedCollection(C),
   call_u(ain((
   ((TCI :- 
    ((cwc, call_u((
      predicate_property(TCI,number_of_rules(1)),
    lazy(( \+ call_u(~(TCI)))),
    isa_backchaing(I,C))))))))))))))}).


*/
/*
ttExpressionType(P) ==> 
 {get_functor(P,F), functor(Head,F,1), call(BHead=baseKB:Head),
  call((\+ predicate_property(BHead,defined) -> kb_dynamic(F/1); true)),
  call((predicate_property(BHead,dynamic)->(ain(Head==>{ignore(retract(Head))}));show_pred_info(BHead)))},
  ~prologMacroHead(F),
  ~functorDeclares(F),
  ~tSet(F),
  notAssertibleCollection(F),
  tCol(F),
  completelyDecidableCollection(F),
  arity(F,1).
*/

%:- mpred_trace_exec.

tSet(tKnownID).
% :- xlisting(tKnownID).
%?- isa(tKnownID,W).
%:- break.

:- mpred_notrace_exec.

% (tInferInstanceFromArgType(Col),tCol(Col)/i_name('',Col,ColName),tPred(Prop)/i_name('',Prop,PropName),{ColName=PropName}==> tInferInstanceFromArgType(Prop)).

% (tInferInstanceFromArgType(Prop),tPred(Prop),arity(Prop,N)/(N>1) ==> ({i_name('vt',Prop,FinalType)},tCol(FinalType),tInferInstanceFromArgType(FinalType),argIsa(Prop,N,FinalType))).

ttRelationType(predCanHaveSingletons).
ttRelationType(prologSideEffects).
prologSideEffects(write/1).
prologSideEffects(resolveConflict/1).



/*
((hybrid_support(F,A)/(is_ftNameArity(F,A), \+ prologDynamic(F),\+ is_static_predicate(F/A))) ==>
  ({    
    functor(G,F,A),
     (var(M)->must(defaultAssertMt(M));true),
     (var(M)->ignore(( current_predicate(F,M:G), \+ predicate_property(M:G,imported_from(_))));true),
     (var(M)->predicate_property(M:G,exported);true),
     % must(rebuild_pred_into(G,G,ain,[+dynamic,+multifile,+discontiguous])),         
     % (predicate_property(M:G,dynamic)->true;must(convert_to_dynamic(M,F,A))),
     kb_dynamic(M:F/A),
     show_failure(hybrid_support, \+ is_static_predicate(F/A))}),
     prologHybrid(F),
    arity(F,A)).
*/


arity(prologMacroHead,1).
ttRelationType(isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologHybrid,
  predCanHaveSingletons,prologDynamic,tPred,prologMacroHead,prologListValued,prologSingleValued)).
prologMacroHead(prologMacroHead).
ttRelationType(X)==>functorDeclares(X).
% tCol(X)==>functorDeclares(X).
% functorDeclares(X)==>tCol(X).
% prologMacroHead(X)==>functorDeclares(X).
prologMacroHead(pddlSomethingIsa/2).
tPred(pddlSomethingIsa(ftTerm,ftListFn(tCol))).

/*
prologBuiltin(A) :- cwc,head_to_functor_name(A, B),prologBuiltin(B).
prologBuiltin(P) :- cwc,is_ftCompound(P),!,get_functor(P,F,A),functor(C,F,A),(predicate_property(C,built_in)). % predicate_property(P,static)).
ttRelationType(PT)==> {atom(PT),H=..[PT,I]}, (H:-cwc,head_to_functor_name(I,F),call_u(call(PT,F))).
*/


tCol(iExplorer4)==>{trace_or_throw(never_tCol(iExplorer4))}.

isa(pddlSomethingIsa/2, prologHybrid).

arity(argIsa,3).

% prologHybrid(F/A)/(atom(F),number(A)) ==> arity(F,A),{must(dynamic_safe(F/A))}.

%:-mpred_trace_exec.

% Functions
tFunction(ArgTypes)/is_declarations(ArgTypes) ==> meta_argtypes(ArgTypes),{get_functor(ArgTypes,F)},tFunction(F).
% FormatTypes
ttExpressionType(ArgTypes)/is_declarations(ArgTypes) ==> meta_argtypes(ArgTypes).

argIsa(completeExtentAsserted,1,tPred).
((meta_argtypes(ArgTypes)/is_ftCompound(ArgTypes)) ==> ({get_functor(ArgTypes,F,A)},arity(F,A),{arg(N,ArgTypes,Type)},argIsa(F,N,Type))).

:-mpred_run.
:- mpred_notrace_exec.


meta_argtypes(predicateConventionMt(tPred,tMicrotheory)).
meta_argtypes(argIsa(tRelation,ftInt,tCol)).
:- must(argIsa(predicateConventionMt,1,tPred)).
:- must(argIsa(predicateConventionMt,2,tMicrotheory)).

prologMacroHead(tCol).


completelyAssertedCollection(prologSingleValued).
completelyAssertedCollection(tCol).
completelyAssertedCollection(ttExpressionType).
completelyAssertedCollection(ttValueType).
completelyAssertedCollection(ttTemporalType).
completelyAssertedCollection(tRelation).
completelyAssertedCollection(tPred).

completelyAssertedCollection(C)==>completeExtentAsserted(C).

completelyAssertedCollection(completeExtentAsserted).
completeExtentAsserted(completelyAssertedCollection).
completeExtentAsserted(functorDeclares).
completeExtentAsserted(completeExtentAsserted).
arity(completeExtentAsserted,1).

% tSet(completeExtentAsserted).
argIsa(completeExtentAsserted,1,tPred).
meta_argtypes(genlPreds(tPred,tPred)).
:- must_det(argIsa(genlPreds,2,_)).
completeExtentAsserted(defnSufficient).


:- kb_dynamic(ttNotTemporalType/1).
ttNotTemporalType(ftInt).
%ttNotTemporalType(ftTerm).
ttNotTemporalType(tCol).
ttNotTemporalType(ttExpressionType).
ttNotTemporalType(ttValueType).

==>ttNotTemporalType(tCol).
ttNotTemporalType(T)==>tCol(T).
==>ttTemporalType(tTemporalThing).
ttTemporalType(T)==>tCol(T).

arity(argQuoted,1).





% (isa(Inst,Type), tCol(Inst)) ==> isa(Type,ttTypeType).


(ttExpressionType(FT),{is_ftCompound(FT)})==>meta_argtypes(FT).

tSet(vtDirection).

:- sanity(get_lang(pfc)).

disjointWith(ttTemporalType,ttAbstractType).
disjointWith(Sub, Super) ==> disjointWith( Super, Sub).

(rtSymmetric(Pred) ==> ({atom(Pred),G1=..[Pred,X,Y],G2=..[Pred,Y,X]}, (G1==>G2), (~(G1)==> ~(G2)))).


tSet(rtNotForUnboundPredicates).

prologSideEffects(P)==>rtNotForUnboundPredicates(P).

isa(tRelation,ttAbstractType).




:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).

:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'neg_sanity.pfc')).


:- endif. % load_time_sanity


%P/(is_ftNonvar(P),get_functor(P,F)),afterAdding(F,Do)/is_ftNonvar(Do)==>{call(Do,P)}.
%~P/(is_ftNonvar(P),get_functor(P,F)),afterRemoving(F,Do)/is_ftNonvar(Do)==>{call(Do,P)}.




%:-rtrace.
% (tCol(Inst), {isa_from_morphology(Inst,Type)}) ==> (isa(Inst,Type)).

% HOW TO MAKE THIS FAST?  isa(Inst,Type) <= {isa_from_morphology(Inst,Type)}.

%((disjointWith(P1,P2) , genls(C1,P1), {dif:dif(C1,P1)}) ==>    disjointWith(C1,P2)).
% (disjointWith(C1,P2) <= (genls(C1,P1), {dif:dif(C1,P1)}, disjointWith(P1,P2))).

tSet(completelyAssertedCollection).
tSet(completeIsaAsserted).
% genls(completeIsaAsserted,tTemporalThing).
genls(completelyAssertedCollection,tCol).
completelyAssertedCollection(completelyAssertedCollection).
completelyAssertedCollection(tPred).
completelyAssertedCollection(tRelation).
completelyAssertedCollection(ttExpressionType).
completelyAssertedCollection(tCol).
completelyAssertedCollection(prologMacroHead).
% completelyAssertedCollection(functorDeclares).
completelyAssertedCollection(ttRelationType).
completelyAssertedCollection(completelyAssertedCollection).

% dividesBetween(S,C1,C2) ==> (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

% disjointWith(P1,P2) ==> ((~(isa(C,P1))) <==> isa(C,P2)).

% isa(Col1, ttObjectType) ==> ~(isa(Col1, ttExpressionType)).


% tCol(ArgsIsa):-ttRelationType(ArgsIsa).
% TODO decide if OK
%tCol(F):-t(functorDeclares,F).
tSet(ttExpressionType).


:- ensure_loaded('system_genls.pfc').

genls(ttSpatialType,ttTemporalType).
genls(tSpatialThing,tTemporalThing).




% remove conflicts early 
% (~(P)/mpred_non_neg_literal(P) ==> ( {mpred_remove(P)}, (\+P ))).

tSet(ttTypeFacet).
tCol(tAvoidForwardChain, comment("tAvoidForwardChain means that backchain is required for subclasses to gain membership TODO: Give example ")).
% genls(ttExpressionType,tAvoidForwardChain).
isa('tThing',tAvoidForwardChain).
%isa('CycLTerm',tAvoidForwardChain).
==>prologHybrid(quotedIsa(ftTerm,ttExpressionType)).
:- kb_dynamic(quotedIsa/2).

/*
  ftSpec
  tCol
 ttFormatType | tCol
*/

% isa(I,C):- cwc, is_ftNonvar(C),ttExpressionType(C),!,quotedIsa(I,C).
%isa(I,C):- cwc, tCol(C),(ttExpressionType(C)*->quotedIsa(I,C);loop_check(isa_backchaing(I,C))).
%isa(I,C):- cwc, tSet(C),(ttExpressionType(C)*->quotedIsa(I,C)).
% isa(I,C):- cwc, when(?=(I,C),\+ clause_b(isa(I,C))), (loop_check(visit_pred(I,C))*->true;loop_check(no_repeats(isa_backchaing(I,C)))).
%isa(I,C):- cwc, loop_check(visit_pred(I,C)).
%isa(I,C):- cwc, loop_check(visit_isa(I,C)).

isa([tIndividual(tSack)],C):-C==ftListFn(ftAskable),!.
isa(iExplorer2,C):- C==argsQuoted,!,fail.
isa(I,C):- ground(I:C),not_isa(I,C),!,fail.
isa(I,C):- cwc, no_repeats(loop_check(isa_backchaing(I,C))).
% isa(_,C):- nonvar(C),\+ tSet(C),!,fail.

quotedIsa(_,C):- nonvar(C), tSet(C),!,fail.
quotedIsa(I,C):- cwc, loop_check(term_is_ft(I,C)).

dif_in_arg(P,N,Q):- cwc, ground(P),P=..[F|ARGS],arg(N,P,B),Q=..[F|ARGS],nb_setarg(N,Q,A),dif(A,B).

tSet(ttSpatialType).
tSet(tSpatialThing).
completelyAssertedCollection(ttTypeType).
completelyAssertedCollection(tCol).



:- kb_dynamic(isa/2).

ttRelationType(Prop)==>tCol(Prop).



%:-baseKB:agenda_slow_op_enqueue(ain(((arity(Pred,2),argIsa(Pred,1,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tCol(Col)), \+prologSideEffects(Pred), t(Pred,Arg,_)/is_ftNonvar(Arg)) ==> t(Col,Arg)))).
%:-baseKB:agenda_slow_op_enqueue(ain(((arity(Pred,2),argIsa(Pred,2,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tCol(Col)), \+prologSideEffects(Pred), t(Pred,_,Arg)/is_ftNonvar(Arg)) ==> t(Col,Arg)))).
%:-add_slow(((arity(Pred,2),argIsa(Pred,2,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tCol(Col)),t(Pred,_,Arg)/is_ftNonvar(Arg)) ==> t(Col,Arg))).
%(((P/(has_functor(P),get_functor(P,F,A),A\=2,\+prologSideEffects(F),mpred_literal(P)) ==> {baseKB:agenda_slow_op_enqueue(deduceEachArgType(P))}))).

% :-rtrace.

%((((P/(nonvar(P),is_ftNonvar(P),functor(P,F,A), \+ mpred_connective(F), A>1) ==> 
%   {baseKB:agenda_slow_op_enqueue(must(ignore(deduceEachArgType(P))))})))).


% tCol(Col) <==> isa(Col,tCol).


%(disjointWith(P1,P2) , genls(C1,P1)) ==>    disjointWith(C1,P2).
disjointWith(Sub, Super) ==> disjointWith( Super, Sub).
disjointWith(ttTemporalType,ttAbstractType).

prologHybrid(typeGenls/2).
:- ain(meta_argtypes(typeGenls(ttTypeType,tCol))).
%(isa(COLTYPEINST,COLTYPE) , typeGenls(COLTYPE,COL)) ==> genls(COLTYPEINST,COL).

typeGenls(ttRelationType,tPred).


prologHybrid(argIsa/3).

:- asserta(t_l:pfcExpansion).





%% argIsa( ?F, ?N, ?Type) is semidet.
%
% asserted Argument  (isa/2) known.
%
argIsa(F/_,N,Type):- nonvar(F),!,argIsa(F,N,Type).
argIsa(F,N,Type):- var(F),!,tRelation(F),argIsa(F,N,Type).
argIsa(F,N,Type):- var(N),arity_no_bc(F,A),!,system_between(1,A,N),argIsa(F,N,Type).
argIsa(F,1,F):- tCol(F), arity_no_bc(F,1),!.
% Managed Arity Predicates.
argIsa(Pred,N,ftVoprop) :- number(N),arity_no_bc(Pred,A),N>A,!.
argIsa(isEach(arity,arityMax,arityMin),2,ftInt).
/*
argIsa(F,_,ftTerm):-member(F/_, [argIsa/3,predProxyAssert/2,negate_wrapper0/2,mudFacing/_,registered_module_type/2,       
                                ruleBackward/2,formatted_resultIsa/2, pt/_,rhs/_,nt/_,bt/_,bracket/3]),!.
argIsa(Prop,N1,Type):- is_2nd_order_holds(Prop),dmsg(todo(define(argIsa(Prop,N1,'Second_Order_TYPE')))),dumpST,dtrace,Type=argIsaFn(Prop,N1),!.
*/


%= 	 	 

%% argQuotedIsa( ?F, ?N, ?FTO) is semidet.
%
% Argument  (isa/2) Format Type.
%
argQuotedIsa(F/_,N,Type):-nonvar(F),!,argQuotedIsa(F,N,Type).
argQuotedIsa(F,N,FTO):- argIsa(F,N,FT), must(to_format_type(FT,FTO)),!.


:- was_export(argIsa/3).

%= 	 	 

%% argIsa( ?F, ?N, ?Type) is semidet.
%
% Argument  (isa/2) call  Primary Helper.
%
argIsa(argIsa,1,tRelation).
argIsa(argIsa,2,ftInt).
argIsa(argIsa,3,tCol).  
argIsa(comment,2,ftString).
argIsa(isKappaFn,1,ftVar).
argIsa(isKappaFn,2,ftAskable).
%argIsa(isInstFn,1,tCol).


argIsa(quotedDefnIff,1,tSpec).
argIsa(quotedDefnIff,2,ftCallable).
argIsa(meta_argtypes,1,tSpec).


argIsa(isa,2,tCol).
argIsa(mpred_isa,1,tPred).
argIsa(mpred_isa,2,ftVoprop).
% argIsa(mpred_isa,3,ftVoprop).

argIsa(formatted_resultIsa,1,ttExpressionType).
argIsa(formatted_resultIsa,2,tCol).

argIsa(predicates,1,ftListFn(ftTerm)).
argIsa(resultIsa,2,tCol).

argIsa(predTypeMax,1,tPred).
argIsa(predTypeMax,2,tCol).
argIsa(predTypeMax,3,ftInt).

argIsa(predInstMax,1,tObj).
argIsa(predInstMax,2,tPred).
argIsa(predInstMax,3,ftInt).

argIsa(props,1,ftID).
argIsa(props,N,ftVoprop):- integer(N), system_between(2,31,N).

argIsa(apathFn,1,tRegion).
argIsa(apathFn,2,vtDirection).
argIsa(localityOfObject,1,tObj).
argIsa(localityOfObject,2,tSpatialThing).

argIsa(typeProps,1,tCol).
argIsa(typeProps,N,ftVoprop):-system_between(2,31,N).

argIsa(instTypeProps,1,ftProlog).
argIsa(instTypeProps,2,tCol).
argIsa(instTypeProps,N,ftVoprop):-system_between(3,31,N).


argIsa(must,1,ftCallable).

argsIsa(F,Type),arity(F,A),{system_between(1,A,N)}==>argIsa(F,N,Type).

argIsa(predicateConventionMt,1,tPred).
argIsa(predicateConventionMt,2,ftAtom).
% argIsa(baseKB:agent_text_command,_,ftTerm).
argIsa('<=>',_,ftTerm).
argIsa(class_template,N,Type):- (N=1 -> Type=tCol;Type=ftVoprop).
argIsa(isEach(descriptionHere,mudDescription,nameString,mudKeyword),2,ftString).

argQuotedIsa(F,N,Type)==>argIsa(F,N,Type).

argQuotedIsa(F,N,Type):- functorDeclares(F),(N=1 -> Type=F ; Type=ftVoprop).
%argIsa(F,N,Type):- t(tCol,F),!,(N=1 -> Type=F ; Type=ftVoprop).

/*
{source_file(M:P,_),functor(P,F,A),
  \+ predicate_property(M:P,imported_from(_))} 
   ==> functor_module(M,F,A).
:- show_count(functor_module/3).
*/

:- dynamic(functor_module/3).

({current_module(M),
 (predicate_property(functor_module(_,_,_),
   clause_count(N)),N<1000),current_predicate(M:F/A),functor(P,F,A), 
  \+ predicate_property(M:P,imported_from(_))}) 
   ==> functor_module(M,F,A).

functor_module(_,F,A)==> arity(F,A).


:- show_count(arity/2).

(functor_module(M,F,A),
  {functor(P,F,A),predicate_property(M:P,static)})==>
  (predicateConventionMt(F,M),mpred_prop(F,A,prologBuiltin)).

(functor_module(M,F,A),
  {functor(P,F,A), predicate_property(M:P,meta_predicate(P)), 
  system_between(1,A,N),arg(N,P,Number),number(Number)}) 
       ==> argIsa(F,N,ftAskable).

(functor_module(M,F,A),
  {functor(P,F,A), predicate_property(M:P,meta_predicate(P)), 
  system_between(1,A,N),arg(N,P,0)}) 
       ==> argIsa(F,N,ftAskable).



%= 	 	 

%% argsIsa( ?WP, ?VALUE2) is semidet.
%
% Argument  (isa/2) call Helper number 3..
%
argsIsa(isEach(predProxyRetract,predProxyAssert,predProxyQuery,genlInverse),tPred).
argsIsa(disjointWith,tCol).
argsIsa(ftFormFn,ftTerm).
argsIsa(mudTermAnglify,ftTerm).
argsIsa(genls,tCol).
argsIsa(subFormat,ttExpressionType).












/*
:- ain(((vtActionTemplate(ArgTypes)/is_declarations(ArgTypes) ==> vtActionTemplate(ArgTypes)))).
:- ain(((baseKB:action_info(ArgTypes,_)/is_declarations(ArgTypes) ==> vtActionTemplate(ArgTypes)))).
:- ain(((prologMacroHead(Compound)/compound_functor(Compound,F)) ==> functorDeclares(F))).
(ttExpressionType(FT)/is_declarations(FT))==>meta_argtypes(FT).


*/

arity(F,A)/(atom(F),\+ is_sentence_functor(F),number(A),A<10,functor(P,F,A),\+ rtLogicalConnective(F)), 
  \+ meta_argtypes_guessed(P),   
   argIsa(F,A,_),
   argIsa(F,1,_),
 {generateArgVars(P, argIsa(F), '='(_))}
==> meta_argtypes_guessed(P).

meta_argtypes_guessed(P)==>meta_argtypes(P).
   
:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).


% :- if_startup_script(w_tl(t_l:pfcExpansion,ensure_loaded(mpred_i_mpred_mpred_testing))).

% :-asserta(baseKB:isa_pred_now_locked).


% :-loadTinyAssertions1.

%:-prolog_repl.
%:-showTinyAssertions.
%:-prolog_repl.
%:-loadTinyAssertions2.


:- endif.

:- meta_predicate(~(0)).
:- kb_dynamic(~(0)).

:- kb_dynamic(predicateConventionMt/2).
:- decl_mpred(predicateConventionMt/2).

meta_argtypes(predicateConventionMt(tPred,tMicrotheory)).


prologHybrid(predicateConventionMt, 2).
prologMultiValued(predicateConventionMt(tRelation,ftAtom)).

prologHybrid(Compound)/get_arity(Compound,F,A)==>{kb_dynamic(F/A)}.

% pddlObjects(Type,EList)==>  isa(isEach(EList),Type).
% pddlSorts(Type,EList)==> genls(isEach(EList),Type).


:- kb_dynamic(argIsa/3).

:- decl_mpred(argIsa/3).

prologBuiltin(col_arity/2).
col_arity(Spec,A):-arity(Spec,A),!.
col_arity(Spec,A):-atom(Spec),!,A=1.
col_arity(Spec,A):-compound(Spec),functor(Spec,_,A).


isa(Spec,tCol)/col_arity(Spec,A) ==> arity(Spec,A).

% :-ain((mpred_isa(I,C)==>{ain((isa(I,tPred),mpred_isa(I,C),props(I,[C])))})).
% :-ain((t(C,I)==>{ /* retract(hasInstance_dyn(C,I)), */ ain((isa(I,C))) , ain((props(I,C)))})).


% :-include('mpred_header.pi').
tSet(tPred).

:- must(assert_argIsa(tPred,1,tPred)).


/*
% reflexive equality
equal(A,B) ==> equal(B,A).
equal(A,B),{ \+ (A=B}),equal(B,C),{ \+ (A=C)} ==> equal(A,C).

notequal(A,B) ==> notequal(B,A).
equal(A,C),notequal(A,B) ==> notequal(C,B).
*/

:- dynamic(either/2).
% is this how to define constraints?
% either(P,Q) ==> (~(P) ==> Q), (~(Q) ==> P).
(either(P,Q) ==> ((~(P) <==> Q), (~(Q) <==> P))).
% ((P,Q ==> false) ==> (P ==> ~(Q)), (Q ==> ~(P))).


:-  /**/ export(member/2).
:-  /**/ export(arg/3).
%:-  /**/ export(call_u/1).
% prologDynamic(cycAssert/2).
:-  /**/ export(integer/1).
% :-  /**/ export(makeConstant/1).
% :-  /**/ export(naf/1).
:-  /**/ export(number/1).
:-  /**/ export(string/1).
:-  /**/ export(var/1).

tSet(completeExtentAsserted).
tSet(ttExpressionType).
tSet(prologMacroHead).
tSet(functorDeclares).

%((prologHybrid(C),{get_functor(C,F,A),C\=F}) ==> arity(F,A)).
prologHybrid(typeProps/2).
arity(typeProps,2).

% :- decl_mpred_pfc ~/1.
prologHybrid(isEach( tCol/1, disjointWith/2, genls/2,genlPreds/2, meta_argtypes/1)).

:- ignore(show_failure(why,arity(typeProps,2))).
:- must(call_u(arity(typeProps,2))).
:- ain_expanded((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,predCanHaveSingletons,prologDynamic,prologMacroHead,prologListValued,prologSingleValued),1,tPred))).
:- ain_expanded((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologHybrid,prologPTTP,prologDynamic,prologMacroHead,prologListValued,prologSingleValued),2,ftListFn(ftVoprop)))).
:- ain_expanded((isa(isEach(prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologPTTP,prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued),functorDeclares))).
:- ain_expanded((genls(isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,prologDynamic,prologBuiltin,prologKIF,prologMacroHead,prologListValued,prologSingleValued),tPred))).
:- assert_hasInstance(tCol,tCol).
:- file_begin(pfc).

%TODO FIX :- decl_mpred(tDeleted(ftID),[predIsFlag]).
prologHybrid(isEach( ttNotTemporalType/1,ttTemporalType/1 )).
prologHybrid(isEach(genlInverse/2,genlPreds/2)).
prologHybrid(argIsa/3).
prologHybrid(disjointWith/2).
prologHybrid(instTypeProps/3).
prologHybrid(predProxyAssert,2).
prologHybrid(predProxyQuery, 2).
prologHybrid(predProxyRetract, 2).
prologHybrid(predTypeMax/3).
prologHybrid(prologSingleValued/1).
prologHybrid(prologSideEffects).
prologHybrid(resultIsa/2).
prologHybrid(genls/2).
prologHybrid(isa/2).
prologHybrid(genls/2).
prologDynamic(arg/3).
tSet(meta_argtypes).
tSet(prologMultiValued).
tSet(prologSingleValued).
tSet(tCol).
tSet(tFunction).
tSet(tInferInstanceFromArgType).
tSet(tPred).
tSet(tRelation).
tSet(ttTemporalType).
tSet(ttTypeType).
% tCol(tPathway).
genls(tFunction,tRelation).

tSet(ttValueType).

ttExpressionType(ftString).
ttExpressionType(ftVar).


ttExpressionType(ftCallable).
ttExpressionType(ftPercent).

:- dynamic(vtColor/1).
isa(vRed,vtColor).

completelyAssertedCollection(vtValue).


:- system:op(1199,fx,('==>')).
:- system:op(1190,xfx,('::::')).
:- system:op(1180,xfx,('==>')).
:- system:op(1170,xfx,('<==>')).
:- system:op(1160,xfx,('<-')).

:- system:op(1150,xfx,('=>')).
:- system:op(1140,xfx,('<=')).
:- system:op(1130,xfx,('<=>')).


:-  system:op(600,yfx,('&')).
:-  system:op(600,yfx,('v')).
:-  system:op(350,xfx,('xor')).
:-  system:op(300,fx,('~')).
:-  system:op(300,fx,('-')).

isa(vtColor,ttValueType).

isa(X,ttValueType) ==> genls(X,vtValue).
isa(X,ttValueType) ==> completelyAssertedCollection(X).

isa(vtValue,ttValueType).

typeGenls(ttValueType,vtValue).

% :- must((vtColor(vRed))).


argIsa(Prop,N,Type) :- cwc,number(N),loop_check(argIsa_known(Prop,N,Type)),must(ground(argIsa(Prop,N,Type))).
%argIsa(Prop,N,Type) <- {cwc,number(N),argIsa_known(Prop,N,Type),must(ground(argIsa(Prop,N,Type)))}.

ttExpressionType(Type) ==> (argIsa(Prop,N,Type),{number(N)} ==> argQuotedIsa(Prop,N,Type)).

:- discontiguous(prologSingleValued/1).


:- do_gc.

:- kb_dynamic(mudLabelTypeProps/3).
:- shared_multifile(mudLabelTypeProps/3).
:- forall(ttRelationType(F),must((decl_type(F),ain(functorDeclares(F)),ain(genls(F,tPred))))).
% :-  /**/ export(mtForPred/2).

/*
:- rtrace.
:- debug,dtrace,(kb_dynamic((argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, 
    isa/2,  genls/2, pddlSomethingIsa/2, ttSpatialType/1, 
    resultIsa/2, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, 
    ttExpressionType/1, typeProps/2))).

:- prolog. 
*/
:- kb_dynamic((argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, 
    isa/2,  genls/2, pddlSomethingIsa/2, 
    resultIsa/2, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, 
    ttExpressionType/1, typeProps/2)).

prologHybrid(isEach(argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, isa/2, 
   genls/2, pddlSomethingIsa/2, resultIsa/2, subFormat/2, tCol/1, tRegion/1, 
   completelyAssertedCollection/1, ttExpressionType/1, typeProps/2)).

:- ain(isa(ttExpressionType,ttAbstractType)).
:- discontiguous(subFormat/2).
:- kb_dynamic(tChannel/1).
:- shared_multifile(tChannel/1).

% ain((I/(mpred_literal(I),fully_expand(_,I,O),I \=@=O )==> ({format('~q~n',[fully_expand(I->O)])},O))).

subFormat(ftDeplictsFn(tCol),ftSpec).
subFormat(ftDeplictsFn(meta_argtypes),ftSpec).
subFormat(ftVoprop,ftSpec).

tFunction(isEach(ftRest(ftTerm))).
tFunction(isRandom(tCol)).
tFunction(isAnd(ftRest(ftSpec))).
tFunction(isMost(ftRest(ftSpec))).
tFunction(isOneOf(ftRest(ftSpec))).
tFunction(isNot(ftSpec)).
tFunction(isOptional(ftSpec,ftTerm)).
tFunction(isOptionalStr(ftString)).
tFunction(exactStr(ftString)).

resultIsa(ftDeplictsFn,ftSpec).

tPred(quotedDefnIff/2,prologHybrid).


isa(argIsa,prologHybrid).
isa(determinerString, prologMultiValued).
isa(quotedDefnIff, completeExtentAsserted).
isa(ftInt,ttExpressionType).
isa(ftNumber,ttExpressionType).
isa(ftString,ttExpressionType).
isa(isInstFn,tFunction).
isa(isKappaFn,tFunction).
isa(prologMultiValued, tCol).
arity(ftListFn,1).
arity(isLikeFn,2).
arity(ftDeplictsFn,1).

arity(tFunction,1).
tFunction(ftDiceFn(ftInt,ftInt,ftInt)).
tFunction(ftListFn(tCol)).
tFunction(ftDeplictsFn(tCol)).

completelyAssertedCollection(tAvoidForwardChain).
completelyAssertedCollection('SententialOperator').


tSet(tAvoidForwardChain).
tSet('SententialOperator').
%TODO tAvoidForwardChain('$VAR'('FUNC')).

tAvoidForwardChain(isEach('FunctionToArg',holds,equals,different,evaluate,trueSentence,'TINYKB-ASSERTION',termOfUnit)).
genls('rtSententialRelation','rtSententialOperator').
genls('rtSententialOperator',tAvoidForwardChain).
genls('rtVariableArityRelation',tAvoidForwardChain).
genls('rtCommutativeRelation',tAvoidForwardChain).
genls('tFunction',tAvoidForwardChain).
genls('rtEvaluatableRelation',tAvoidForwardChain).

tSet('rtCommutativeRelation').
tSet('rtEvaluatableRelation').
tSet('rtSententialRelation').
tSet('rtVariableArityRelation').


tSet(completeIsaAsserted).
%completelyAssertedCollection(Ext):- fwc, arg(_,vv(tCol,vtDirection,ttExpressionType,tRegion,ftString, genlPreds),Ext).
completeExtentAsserted(formatted_resultIsa).
completeExtentAsserted(quotedDefnIff).
completelyAssertedCollection(completelyAssertedCollection).

ttExpressionType(ftVar).
ttExpressionType(ftVoprop).


ttStringType('CharacterString').
ttStringType('SubLString').
ttStringType('ControlCharacterFreeString').
ttStringType('SubLListOfStrings').
ttStringType(['ListOfTypeFn', X]):-atom(X),ttStringType(X).


% resultIsa(F,C)==>(ftSpec(C),'tFunction'(F)).
% ( meta_argtypes(FT)/dif(FT,COL), genls(FT, COL),tCol(COL),{\+ (isa(COL,ttExpressionType))}) ==> formatted_resultIsa(FT,COL).

%:- mpred_trace.
%:- pfcWatch.
%:- mpred_warn.
% next_test :- sleep(1),pfcReset.


% :- kb_dynamic((disjointWith/2,genls/2)).

argsQuoted(mudAreaConnected).

prologHybrid(argQuotedIsa(tRelation,ftInt,ttExpressionType)).
prologHybrid(argIsa(tRelation,ftInt,tCol)).
prologHybrid(formatted_resultIsa(ttExpressionType,tCol)).

:- sanity(argIsa(genlPreds,2,_)).
:- must(tCol(vtVerb)).
:- must(t(tCol,vtVerb)).
:- must(isa(vtVerb,tCol)).


prologHybrid(quotedDefnIff(ttExpressionType,ftTerm)).
prologHybrid(defnNecessary(ttExpressionType,ftTerm)).
prologHybrid(quotedDefnIff(ttExpressionType,ftTerm)).


tFuncton(isLikeFn(tPred,tCol)).
tRelation('==>'(ftAskable,ftAssertable)).
prologHybrid(subFormat(ttExpressionType,ttExpressionType)).
prologMultiValued(comment(ftTerm,ftString)).
prologMultiValued(genlInverse(tPred,tPred)).
prologMultiValued(genlPreds(tPred,tPred)).
prologMultiValued(predProxyAssert(prologMultiValued,ftTerm)).
prologMultiValued(predProxyQuery(prologMultiValued,ftTerm)).

:- if(true).
prologHybrid(instTypeProps(ftID,tCol,ftRest(ftVoprop))).
prologMacroHead(macroSomethingDescription(ftTerm,ftListFn(ftString))).
prologMacroHead(pddlObjects(tCol,ftListFn(ftID))).
prologMacroHead(pddlDescription(ftID,ftListFn(ftString))).
prologMacroHead(pddlPredicates(ftListFn(ftVoprop))).
prologMacroHead(pddlSorts(tCol,ftListFn(tCol))).
prologMacroHead(pddlTypes(ftListFn(tCol))).
:- endif.


% prologMultiValued('<==>'(ftTerm,ftTerm)).
prologMultiValued('<-'(ftAssertable,ftAskable)).
prologMultiValued('==>'(ftAskable,ftAssertable)).
prologNegByFailure(predArgMulti(prologMultiValued,ftInt)).
prologNegByFailure(tDeleted(ftID)).

%= 	 	 

%% prologSingleValued( ?ARG1, ?ARG2) is semidet.
%
% Prolog Single Valued.
%
prologSingleValued(predInstMax(ftID,prologSingleValued,ftInt),prologHybrid).
prologSingleValued(predTypeMax(prologSingleValued,tCol,ftInt),prologHybrid).
resultIsa(txtFormatFn,ftText).
%'<==>'(prologMultiValued(CallSig,[predProxyAssert(aina),predProxyRetract(del),predProxyQuery(call)]),prologDynamic(CallSig)).
%'<==>'(prologMultiValued(CallSig,[predProxyAssert(pttp_tell),predProxyRetract(pttp_retract),predProxyQuery(pttp_ask)]),prologPTTP(CallSig)).
subFormat(ftAtom,ftTerm).
subFormat(ftCallable,ftProlog).
resultIsa(ftDiceFn,ftInt).
subFormat(ftID,ftTerm).
subFormat(ftInt,ftNumber).
subFormat(ftInteger,ftNumber).
subFormat(ftNumber,ftPercent).
subFormat(ftPercent,ftNumber).
subFormat(ftString,ftTerm).
subFormat(ftString,ftText).
subFormat(ftTerm,ftProlog).
subFormat(ftText,ftTerm).
subFormat(ftVar,ftProlog).
subFormat(ftVoprop,ftRest(ftVoprop)).
subFormat(ftVoprop,ftTerm).

subFormat(COL1,COL2)/atom(COL1)==>(ttExpressionType(COL1),ttExpressionType(COL2)).

tCol(W)==>{guess_supertypes(W)}.


tSet(tNewlyCreated).
tSet(ttTypeFacet).

:- dynamic(tNewlyCreated/1).
tNewlyCreated(W)==>{guess_types(W)}.

ttTypeFacet(ttTypeFacet).
ttTypeFacet(ttUnverifiableType).


%typeGenls(tPred,ttRelationType).
typeGenls(ttExpressionTypeType,ttExpressionType).
typeGenls(ttTypeFacet,tCol).
typeGenls(ttTypeType,tCol).


(typeGenls(TT,ST) ==> 
 (isa(Inst,TT) ==> genls(Inst,ST))).


:-kb_dynamic(rtUnaryPredicate/1).
:-kb_dynamic(ttSpatialType/1).


ttTypeFacet(ttUnverifiableType).
ttUnverifiableType(ftID).
% ttUnverifiableType(ftListFn(ftTerm)).
ttUnverifiableType(ftListFn).
% ttUnverifiableType(ftDiceFn(ftInt,ftInt,ftInt)).
ttUnverifiableType(ftDice).
ttUnverifiableType(ftString).
ttUnverifiableType(ftTerm).
ttUnverifiableType(ftText).
ttUnverifiableType(ftVoprop).
ttUnverifiableType(tCol).
ttUnverifiableType(tFunction).
ttUnverifiableType(tPred).
ttUnverifiableType(ttExpressionType).
ttUnverifiableType(vtDirection).


%ttRelationType(ArgsIsa)==>tPred(ArgsIsa).
%TODO isa(_,ArgsIsa)==>tCol(ArgsIsa).

:- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true),set_prolog_flag(debug, true).


/*
disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):- disjointWithT(A,B).
disjointWith(A,B):- disjointWithT(AS,BS),transitive_subclass_or_same(A,AS),transitive_subclass_or_same(B,BS).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.
*/
disjointWith(Sub, Super) ==> disjointWith( Super, Sub).


disjointWith(ttTemporalType,ttAbstractType).

prologHybrid(dividesBetween(tCol,tCol,tCol)).

quotedDefnIff(X,_)==>ttExpressionType(X).

quotedDefnIff(ftInt,integer).
quotedDefnIff(ftFloat,float).
quotedDefnIff(ftAtom,atom).
quotedDefnIff(ftString,is_ftString).
quotedDefnIff(ftSimpleString,string).
quotedDefnIff(ftCallable,is_callable).
quotedDefnIff(ftCompound,is_ftCompound).
quotedDefnIff(ftGround,ground).
quotedDefnIff(ftID,is_id).
quotedDefnIff(ftTerm,is_ftNonvar).
quotedDefnIff(ftVar,is_ftVar).
quotedDefnIff(ftNonvar,is_ftNonvar).
quotedDefnIff(ftNumber,number).
quotedDefnIff(ftList,is_list).
% quotedDefnIff(ftRest,is_rest).
quotedDefnIff(ftBoolean,is_boolean).
quotedDefnIff(ftText,is_ftText).
quotedDefnIff(ftRest(Type),is_rest_of(Type)):- cwc, is_ftNonvar(Type).
quotedDefnIff(ftListFn(Type),is_list_of(Type)):- cwc, is_ftNonvar(Type).
quotedDefnIff(ftCodeIs(SomeCode),SomeCode):- cwc, is_ftNonvar(SomeCode).

(ttExpressionType(FT)/append_term(FT,Arg,Head)==> ((Head:- !, term_is_ft(Arg,FT)))).

% tCol(Type),(rtBinaryPredicate(Pred)/(functor(G,Pred,2),G=..[Pred,isInstFn(Type),Value])), G ==> relationMostInstance(Pred,Type,Value).


%((genlPreds(Col1,Col2),(arity(Col1,1);arity(Col2,1)))==>genls(Col1,Col2)).
%((genls(Col1,Col2),(tPred(Col1);tPred(Col2)))==>genlPreds(Col1,Col2)).

tSet(rtBinaryPredicate).
ttRelationType(rtBinaryPredicate).

isa(arity,rtBinaryPredicate).

% (arity(Pred,2),tPred(Pred)) <==> isa(Pred,rtBinaryPredicate).

ttRelationType('rtUnaryPredicate').

isa(arity,rtBinaryPredicate).



specialFunctor('\\+').
specialFunctor('/').


:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).
/*
:- must((expand_props(_,props(iCrackers666,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]),O),ain(mdefault(O)))).

:- must((fully_expand(_,props(iCrackers666,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]),O),mpred_why(mdefault(O)))).
*/
:- endif.

arity(Pred,2),tPred(Pred) <==> rtBinaryPredicate(Pred).

% if arity is ever greater than 1 it can never become 1
% arity(F,A)/(number(A),A>1) ==> ~(arity(F,1)).

completelyAssertedCollection(rtBinaryPredicate).


% TODO ADD THIS 
%(tCol(Super),completelyAssertedCollection(Super),genls(Sub, Super), isa(I,Sub), {ground(I:Sub:Super),\==(Sub, Super)}) ==> isa(I,Super).

:- mpred_trace_exec.
((genlPreds(P,equals),argIsa(P,1,Col)) ==>  {trace},(t(P,A,B):- (nonvar(A),A==B,isa(A,Col)))).
genlPreds(genls,equals).
:- mpred_notrace_exec.
rtReflexiveBinaryPredicate(TB)==>genlPreds(TB,equals).

% (isa(TypeType,ttTypeType) , isa(Inst,TypeType), genls(SubInst,Inst)) ==> isa(SubInst,TypeType).



tSet(tPred).
prologHybrid(isa/2).

%mpred_online:semweb_startup:- with_no_term_expansions(if_file_exists(use_module(logicmoo(dbase/mpred_i_rdf_store)))).

% :- with_no_term_expansions(if_file_exists(use_module(logicmoo(mobs/planner/mpred_i_hyhtn)))).
tSet(predIsFlag).
tSet(prologDynamic).
prologHybrid(formatted_resultIsa/2).

:-sanity(argIsa(genlPreds,2,_)).
:- must(tCol(vtVerb)).
:- must(t(tCol,vtVerb)).
:- must(isa(vtVerb,tCol)).


isa(iPlato,'tPhilosopher').
%:- mpred_test(\+ isa(iPlato,ftAtom)).
%:- mpred_test(~isa(iPlato,ftAtom)).
:- mpred_test(~quotedIsa(iPlato,'tPhilosopher')).
:- mpred_test(quotedIsa(iPlato,ftAtom)).

ttBarrierStr(A)/(atomic_list_concat([A,"Type"],AType0),
  atomic_list_concat([A,''],Type0),
  do_renames(Type0,Type),
  do_renames(AType0,TypeType)) ==> barrierSpindle(TypeType,Type).



barrierSpindle(TypeType,Type)==> isa(TypeType,ttBarrierType),isa(Type,ttBarrier),typeGenls(TypeType,Type).

ttBarrier(C)==>tSet(C).
ttBarrierType(C)==>tSet(C),ttTypeType(C).

ttBarrier(C)==>(isa(I,C)==>mainClass(I,C)).


ttBarrier(A)/dif(A,B),ttBarrier(B)==> disjointWith(A,B).
% ttBarrierType(A)/dif(A,B),ttBarrierType(B)==> disjointWith(A,B).

tCol(ttAbstractType).
disjointWith(C,D)==> tCol(C),tCol(D).
not_isa(I,C):- cwc, mainClass(I,MC),disjointWith(MC,DC),genls(C,DC).

cycBetween(A,B,N):-
  number(A),number(B),!,system_between(A,B,N).

genlsFwd(tItem,'Artifact').
genlsFwd(tRegion,'Place').
ttBarrierStr("Action").
ttBarrierStr("Agent").
ttBarrierStr("Artifact").
barrierSpindle('SpecifiedPartTypeCollection','PhysicalPartOfObject').
ttBarrierStr("Capability").
ttBarrierStr("Event").
ttBarrierStr("FormulaTemplate").
ttBarrierStr("Goal").
ttBarrierStr("Group").
ttBarrierStr("LinguisticObject").
ttBarrierStr("Microtheory").
ttBarrierStr("PersonTypeByActivity").
ttBarrierStr("Place").
ttBarrierStr("Quantity").
ttBarrierStr("Relation").
ttBarrierStr("ScalarInterval").
ttBarrierStr("Situation").
ttBarrierStr("ExpressionType").
ttBarrierStr("TimeParameter").
ttBarrierStr("Topic").
% ttBarrierStr("Collection").

:- listing(disjointWith/2).


% ttBarrier(X)==> (isa(I,_),mpred_truth_value(isa(I,X),TF,Why)==>mudTF(isa(I,X),TF,Why)).
% :- listing(mudTF/3).


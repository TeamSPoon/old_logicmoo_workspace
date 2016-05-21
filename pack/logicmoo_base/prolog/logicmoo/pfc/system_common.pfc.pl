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

:- set_fileAssertMt(baseKB).
:- set_defaultAssertMt(baseKB).

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
%          quasiQuote/1,relax_term/6,retractall_wid/1,ruleRewrite/2,search/7,support_hilog/2,svar_fixvarname/2,tNotForUnboundPredicates/1))),
 with_pfa(With,(((bt/2),(nt/3),(pk/3),(pt/2),(spft/3),(tms/1),(hs/1),(que/1),(pm/1),
          (('==>')/1),(('::::')/2),(('<-')/2),(('<==>')/2),(('==>')/2),(('~')/1),(('nesc')/1),((mpred_action)/1),
          (mpred_do_and_undo_method/2),
	  prologMultiValued/1,prologOrdered/1,prologNegByFailure/1,prologPTTP/1,prologKIF/1,pfcControlled/1,ttPredType/1,
           prologHybrid/1,predCanHaveSingletons/1,prologDynamic/1,prologBuiltin/1,prologMacroHead/1,prologListValued/1,prologSingleValued/1,
          (hs/1),(pfcControlled/1),(prologDynamic/2),(prologSideEffects/1),(prologSingleValued/1),(singleValuedInArg/2),(prologSideEffects/1,prologMacroHead/1,pfcControlled/1,
           resolveConflict/1,resolverConflict_robot/1)))),
 with_pfa(With,((mpred_isa/2,arity/2,predicateConventionMt/2))),
 with_pfa(With,((vtColor/1))).
 */

% :-  dynamic((disjointWith/2,genls/2,isa/2,argIsa/3)).
% :- discontiguous((disjointWith/2,genls/2,isa/2,argIsa/3,typeGenls/2)).

% :- autoload.



/*
 %  % :- debug(_).
:- nodebug(http(_)).
:- debug(mpred).
:- mpred_trace_exec.
:- begin_pfc.

*/

% remove conflicts early 
% (~(P)/mpred_non_neg_literal(P) ==> ( {mpred_remove(P)}, (\+P ))).

:- ain((never_retract_u(~(X),is_ftVar(X)):- cwc,is_ftVar(X))).


% These next 2 might be best as builtins?
((~(P)/(mpred_non_neg_literal(P),copy_term(P,PP))) ==> \+ PP ).
(P/mpred_non_neg_literal(P) ==> { remove_negative_version(P) } ).
% (P/mpred_non_neg_literal(P) ==> (\+ ~(P))).
% ((P/(mpred_non_neg_literal(P),copy_term(P,PP))) ==> (~ ~ PP )).

% :- nortrace,cnotrace.
% a pretty basic conflict (disabled for now)
%(~(P)/mpred_non_neg_literal(P), P) ==> conflict(~(P)).
%(P/mpred_non_neg_literal(P), ~(P)) ==> conflict(P).

%:- rtrace,trace.
prologBuiltin(mpred_select/2).
% :- nortrace,cnotrace.

:- kb_dynamic(conflict/1).
% a conflict triggers a Prolog action to resolve it.
conflict(C) ==> {must(with_mpred_trace_exec(resolveConflict(C),\+conflict(C)))}.

% :- kb_dynamic(ttTypeType/1).

% meta rules to schedule inferencing.
% resolve conflicts asap
% mpred_select(conflict(X),W) :- que(conflict(X),W).


{type_prefix(_Prefix,Type)}==>tCol(Type).
{type_suffix(_Suffix,Type)}==>tCol(Type).
tCol(completelyAssertedCollection).

%WEIRD ~(tCol(C))/completelyAssertedCollection(C)==> \+ completelyAssertedCollection(C).
% EASIER
~tCol(C) ==> ~completelyAssertedCollection(C).

(tCol(P),~ttExpressionType(P)) ==> tSet(P).

%= 	 	 

%% prologBuiltin( ?ARG1, ?ARG2) is semidet.
%
% Prolog Builtin.
%
%WRONG prologBuiltin(resolveConflict/1,predicateConventionMt(baseKB)).
%WRONG prologBuiltin(mpred_select/2,predicateConventionMt(lmconf)).
%:-rtrace.
prologBuiltin(agent_text_command/4,prologDynamic).
%tPred(t,prologDynamic).

% tPred(member/2,prologBuiltin).

tCol(tNotForUnboundPredicates).


:- sanity(fileAssertMt(baseKB)).
:- sanity(defaultAssertMt(baseKB)).


tNotForUnboundPredicates(member).

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


tPred(arity/2,prologHybrid).
tPred(is_never_type/1,prologDynamic).
tPred(term_expansion/2,prologDynamic).
tPred(var/1,prologBuiltin).

completelyAssertedCollection(completelyAssertedCollection).
completelyAssertedCollection(C)==>tCol(C).

% A Type Specification
completelyAssertedCollection(tCol).  % a type is a type
completelyAssertedCollection(tSpec). % A specification is sort of a type

:- discontiguous tSpec/1.
tSpec(tCol).  % A specification may be a type
tSpec(meta_argtypes).  % A specification may be a syntactic description



arity(apathFn,2).
arity(isKappaFn,2).
arity(isInstFn,1).
arity(ftListFn,1).
arity(xyzFn,4).
arity(arity,2).
arity(is_never_type,1).
arity(argIsa, 3).
arity(Prop,1):-ttPredType(Prop).
arity(meta_argtypes,1).
arity(arity,2).
arity(is_never_type,1).
arity(prologSingleValued,1).
arity('<=>',2).
arity(F,A):- is_ftNameArity(F,A), current_predicate(F/A),A>1.
arity(F,1):- is_ftNameArity(F,1), current_predicate(F/1),\+((dif:dif(Z,1), arity(F,Z))).


pfcControlled(P),arity(P,A)==>hybrid_support(P,A).

ttPredType(X)==>tCol(X).
ttPredType(X)/atom(X)==>(arity(X,1),pfcControlled(X)).

tSet(ttExpressionType).


% Cols are either syntactic or existential
completelyAssertedCollection(ttExpressionType).  % syntactic
completelyAssertedCollection(tSet). % existential

% relations are predsor functions
completelyAssertedCollection(tRelation).
completelyAssertedCollection(tPred).
completelyAssertedCollection(tFunction).

completelyAssertedCollection(prologMacroHead).  % Items read from a file might be a special Macro Head
completelyAssertedCollection(ttPredType).  % Or they might be a predciate declarer
completelyAssertedCollection(functorDeclares).  % or they might declare other things

:- ((ain((completelyAssertedCollection(isEach(tPred,prologMultiValued,prologOrdered,predIsFlag,ttNonGenled,
 prologNegByFailure,meta_argtypes,prologHybrid,prologPTTP,prologDynamic,prologKIF,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued)))))).

completelyAssertedCollection(ftTerm).
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

ttPredType(pfcDatabaseTerm).
ttPredType(pfcControlled).
ttPredType(prologSingleValued).


ttPredType(pfcWatched).
ttPredType(pfcCreates).
:- dynamic(pfcNegTrigger/1).
ttPredType(pfcNegTrigger).
ttPredType(pfcPosTrigger).
ttPredType(pfcBcTrigger).
ttPredType(pfcRHS).

ttPredType(pfcMustFC).

((ttPredType(X)/atom(X)) ==>support_hilog(X,1)).


ttPredType(P)==>(tSet(P),completelyAssertedCollection(P)).

:- dynamic(baseKB:ttTypeType/1).

ttTypeType(C)==>completelyAssertedCollection(C).

%overkill
tSet(C)==>completelyAssertedCollection(C).

%underkill - Though it is making bad things happen 
% ttExpressionType(C)==> ~completelyAssertedCollection(C).

tCol(C)/(atom(C),TCI=..[C,I]) ==> ({decl_type(C)},arity(C,1),mpred_univ(C,I,TCI)).

(tCol(C)/(atom(C), \+ is_static_predicate(C/1) )) ==> {kb_dynamic(C/1)}.

(tCol(C)/(atom(C),TCI=..[C,I],\+ is_static_predicate(C/1), \+ completelyAssertedCollection(C))) 
  ==> ((TCI:- 
    ((cwc,
    lazy(( \+ ~(TCI))),
    isa_backchaing(I,C))))).

% (tInferInstanceFromArgType(Col),tCol(Col)/i_name('',Col,ColName),tPred(Prop)/i_name('',Prop,PropName),{ColName=PropName}==> tInferInstanceFromArgType(Prop)).

% (tInferInstanceFromArgType(Prop),tPred(Prop),arity(Prop,N)/(N>1) ==> ({i_name('vt',Prop,FinalType)},tCol(FinalType),tInferInstanceFromArgType(FinalType),argIsa(Prop,N,FinalType))).

ttPredType(predCanHaveSingletons).
ttPredType(prologSideEffects).
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

% :- prolog.
% tPred
ttPredType(isEach(meta_argtypes,pfcDatabaseTerm,pfcControlled,pfcWatched,pfcMustFC,predIsFlag,prologMultiValued,
 pfcBcTrigger,
 prologSingleValued,prologMacroHead,notAssertable,prologBuiltin,prologDynamic,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,prologEquality,prologPTTP,
 prologSideEffects,prologHybrid,prologListValued)).

completelyAssertedCollection(isEach(tCol,tPred,pfcControlled)).
ttPredType(C)==>completelyAssertedCollection(C).


~(ttExpressionType(prologEquality)).
ttPredType(prologEquality).
tSpec(prologEquality).
prologEquality(mudEquals).
prologEquality(('=')).
prologEquality(('==')).

~(isa((','), prologEquality)).

tCol(isEach(tCol,tPred,pfcControlled)).
tCol(meta_argtypes).
tCol(functorDeclares).
ttPredType(predIsFlag).
ttPredType(prologDynamic).
ttPredType(prologHybrid).
ttPredType(pfcControlled).
ttPredType(prologKIF).
ttPredType(prologBuiltin).
ttPredType(prologPTTP).

pfcControlled(genlPreds).
pfcControlled(isa).
pfcControlled(argIsa).


%tCol(X) ==> isa(X,tCol).
%tCol(X) ==> ruleRewrite(t(X,I),isa(I,X)).


%typeProps(tCoffeeCup,[mudColor(vBlack),mudColor(isEach(vBrown,vBlack)),mudSize(vSmall),mudShape(vCuplike),mudMaterial(vGlass),mudTexture(vSmooth)]).
%props(iCoffeeCup7,[mudColor(vBlack),mudColor(isEach(vBrown,vBlack)),mudSize(vSmall),mudShape(vCuplike),mudMaterial(vGlass),mudTexture(vSmooth)]).

==>tSet(tSet).


:- must(ain((tSet(C)==>
 ( {atom(C), functor(Head,C,1),
  ( \+(predicate_property(Head,S1))-> kb_dynamic(C/1); true),
  Head=..[C,S2],nop((S1:S2)),
 (predicate_property(Head,dynamic)->true;show_pred_info(Head))},
   functorDeclares(C),
   pfcControlled(C),
   arity(C,1),
   % (isa(I,C)/ground(I:C)==>Head),
   tCol(C))))).


ttExpressionType(P) ==> {get_functor(P,C), functor(Head,C,1),
  (\+(predicate_property(Head,S1))-> (kb_dynamic(C/1));true),  
  Head=..[C,I],
  nop((S1)),
 (predicate_property(Head,dynamic)->true;show_pred_info(Head))},
   ~(functorDeclares(C)),
   % isa(C,prologDynamic),
   arity(C,1),
   ((Head)/predicate_property(Head,dynamic)==>{ignore(retract(Head))}),
   ((isa(I,C))==>{ignore(retract(isa(I,C)))}).

arity(prologMacroHead,1).



ttPredType(isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologHybrid,
  predCanHaveSingletons,prologDynamic,tPred,prologMacroHead,prologListValued,prologSingleValued)).
prologMacroHead(prologMacroHead).
ttPredType(X)==>functorDeclares(X).
functorDeclares(X)==>tSet(X).
functorDeclares(X)==>tCol(X).
% prologMacroHead(X)==>functorDeclares(X).
% prologMacroHead(pddlSomethingIsa/2).
tPred(pddlSomethingIsa(ftTerm,ftListFn(tCol))).

prologBuiltin(A) :- cwc,head_to_functor_name(A, B),prologBuiltin(B).
prologBuiltin(P) :- cwc,is_ftCompound(P),!,get_functor(P,F,A),functor(C,F,A),(predicate_property(C,built_in)). % predicate_property(P,static)).
ttPredType(PT)==> {atom(PT),H=..[PT,I]}, (H:-cwc,head_to_functor_name(I,F),call(PT,F)).






isa(pddlSomethingIsa/2, prologHybrid).

arity(argIsa,3).

% prologHybrid(F/A)/(atom(F),number(A)) ==> arity(F,A),{must(dynamic_safe(F/A))}.

% Functions
tFunction(ArgTypes)/is_declarations(ArgTypes) ==> meta_argtypes(ArgTypes).
% FormatTypes
ttExpressionType(ArgTypes)/is_declarations(ArgTypes) ==> meta_argtypes(ArgTypes).


meta_argtypes(ArgTypes)/is_ftCompound(ArgTypes) ==> {get_functor(ArgTypes,F,A)},arity(F,A).


prologMacroHead(tCol).


completelyAssertedCollection(prologSingleValued).
completelyAssertedCollection(tCol).
completelyAssertedCollection(ttExpressionType).
completelyAssertedCollection(ttValueType).
completelyAssertedCollection(ttTemporalType).
completelyAssertedCollection(tRelation).
completelyAssertedCollection(tPred).

completelyAssertedCollection(C)==>completeExtentAsserted(C).

completeExtentAsserted(genlPreds).
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




((isa(Inst,ttTemporalType), tCol(Inst)) ==> genls(Inst,tTemporalThing)).

% (isa(Inst,Type), tCol(Inst)) ==> isa(Type,ttTypeType).


(ttExpressionType(FT),{is_ftCompound(FT)})==>meta_argtypes(FT).

tCol(vtDirection).

disjointWith(ttTemporalType,ttAbstractType).
disjointWith(Sub, Super) ==> disjointWith( Super, Sub).

(ptSymmetric(Pred) ==> ({atom(Pred),G1=..[Pred,X,Y],G2=..[Pred,Y,X]}, (G1==>G2), (~(G1)==> ~(G2)))).


tCol(tNotForUnboundPredicates).

prologSideEffects(P)==>tNotForUnboundPredicates(P).

isa(tRelation,ttAbstractType).




:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'neg_sanity.pfc')).


:- endif. % load_time_sanity


%P/(is_ftNonvar(P),get_functor(P,F)),afterAdding(F,Do)/is_ftNonvar(Do)==>{call(Do,P)}.
%~P/(is_ftNonvar(P),get_functor(P,F)),afterRemoving(F,Do)/is_ftNonvar(Do)==>{call(Do,P)}.




%:-rtrace.
(tCol(Inst), {isa_from_morphology(Inst,Type)}) ==> (isa(Inst,Type)).

% HOW TO MAKE THIS FAST?  isa(Inst,Type) <= {isa_from_morphology(Inst,Type)}.

%((disjointWith(P1,P2) , genls(C1,P1), {dif:dif(C1,P1)}) ==>    disjointWith(C1,P2)).
% (disjointWith(C1,P2) <= (genls(C1,P1), {dif:dif(C1,P1)}, disjointWith(P1,P2))).

tCol(completelyAssertedCollection).
tCol(completeIsaAsserted).
% genls(completeIsaAsserted,tTemporalThing).
genls(completelyAssertedCollection,tCol).
completelyAssertedCollection(completelyAssertedCollection).
completelyAssertedCollection(tPred).
completelyAssertedCollection(tRelation).
completelyAssertedCollection(tFormatType).
completelyAssertedCollection(tSet).
completelyAssertedCollection(functorDeclares).
completelyAssertedCollection(ttPredType).
completelyAssertedCollection(completelyAssertedCollection).

% dividesBetween(S,C1,C2) ==> (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

% disjointWith(P1,P2) ==> ((~(isa(C,P1))) <==> isa(C,P2)).

% isa(Col1, ttObjectType) ==> ~(isa(Col1, ttExpressionType)).

tCol(tCol).
tCol(tPred).
tCol(tFunction).
tCol(tRelation).
tCol(ttTemporalType).
tCol(ttExpressionType).
tCol(functorDeclares).
% tCol(ArgsIsa):-ttPredType(ArgsIsa).
% TODO decide if OK
%tCol(F):-t(functorDeclares,F).
tCol(ttExpressionType).


:- ensure_loaded('system_genls.pfc').

genls(ttSpatialType,ttTemporalType).
genls(tSpatialThing,tTemporalThing).




% remove conflicts early 
% (~(P)/mpred_non_neg_literal(P) ==> ( {mpred_remove(P)}, (\+P ))).

tCol(ttNonGenled).
% genls(ttExpressionType,ttNonGenled).
isa('Thing',ttNonGenled).
isa('CycLTerm',ttNonGenled).
==>prologHybrid(quotedIsa(ftTerm,ttExpressionType)).
:- kb_dynamic(quotedIsa/2).


isa(I,C):- cwc, mpred_univ(C,I,CI),atom(C),(current_predicate(C,M:CI),\+ predicate_property(M:CI,imported_from(_))), call_u(call(M:CI)).
isa(I,C):- isa_backchaing(I,C).
isa(I,C):- cwc, is_ftNonvar(C),ttExpressionType(C),!,quotedIsa(I,C).

quotedIsa(I,C):- cwc, term_is_ft(I,C).

dif_in_arg(P,N,Q):- cwc, ground(P),P=..[F|ARGS],arg(N,P,B),Q=..[F|ARGS],nb_setarg(N,Q,A),dif(A,B).

tCol(ttSpatialType).
tCol(tSpatialThing).
completelyAssertedCollection(ttTypeType).
completelyAssertedCollection(tCol).



:- kb_dynamic(isa/2).

ttPredType(Prop)==>tCol(Prop).



%:-lmconf:agenda_slow_op_enqueue(ain(((arity(Pred,2),argIsa(Pred,1,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tSet(Col)), \+prologSideEffects(Pred), t(Pred,Arg,_)/is_ftNonvar(Arg)) ==> t(Col,Arg)))).
%:-lmconf:agenda_slow_op_enqueue(ain(((arity(Pred,2),argIsa(Pred,2,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tSet(Col)), \+prologSideEffects(Pred), t(Pred,_,Arg)/is_ftNonvar(Arg)) ==> t(Col,Arg)))).
%:-add_slow(((arity(Pred,2),argIsa(Pred,2,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tSet(Col)),t(Pred,_,Arg)/is_ftNonvar(Arg)) ==> t(Col,Arg))).
%(((P/(has_functor(P),get_functor(P,F,A),A\=2,\+prologSideEffects(F),mpred_literal(P)) ==> {lmconf:agenda_slow_op_enqueue(deduceEachArgType(P))}))).

% :-rtrace.

((((P/(nonvar(P),is_ftNonvar(P),functor(P,F,A), \+ mpred_connective(F), A>1) ==> 
   {lmconf:agenda_slow_op_enqueue(must(ignore(deduceEachArgType(P))))})))).


% tCol(Col) <==> isa(Col,tCol).


%(disjointWith(P1,P2) , genls(C1,P1)) ==>    disjointWith(C1,P2).
disjointWith(Sub, Super) ==> disjointWith( Super, Sub).
disjointWith(ttTemporalType,ttAbstractType).

prologHybrid(typeGenls/2).
:- ain(meta_argtypes(typeGenls(ttTypeType,tCol))).
%(isa(COLTYPEINST,COLTYPE) , typeGenls(COLTYPE,COL)) ==> genls(COLTYPEINST,COL).

typeGenls(ttPredType,tPred).


prologHybrid(argIsa/3).

:- asserta(t_l:pfcExpansion).


/*
:- ain(((vtActionTemplate(ArgTypes)/is_declarations(ArgTypes) ==> vtActionTemplate(ArgTypes)))).
:- ain(((lmconf:action_info(ArgTypes,_)/is_declarations(ArgTypes) ==> vtActionTemplate(ArgTypes)))).
:- ain(((isa(Compound,prologMacroHead)/compound_functor(Compound,F)) ==> functorDeclares(F))).
(ttExpressionType(FT)/is_declarations(FT))==>meta_argtypes(FT).


*/

:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).


% :- if_startup_script(w_tl(t_l:pfcExpansion,ensure_loaded(mpred_i_mpred_mpred_testing))).

% :-asserta(lmconf:isa_pred_now_locked).


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

meta_argtypes(predicateConventionMt(tPred,tPrologModule)).


prologHybrid(predicateConventionMt, 2).
prologMultiValued(predicateConventionMt(tRelation,ftAtom)).


% pddlObjects(Type,EList)==>  isa(isEach(EList),Type).
% pddlSorts(Type,EList)==> genls(isEach(EList),Type).


:- kb_dynamic(argIsa/3).

:- decl_mpred(argIsa/3).

isa(Spec,tCol) ==> arity(Spec,1).

% :-ain((mpred_isa(I,C)==>{ain((isa(I,tPred),mpred_isa(I,C),props(I,[C])))})).
% :-ain((t(C,I)==>{ /* retract(hasInstance_dyn(C,I)), */ ain((isa(I,C))) , ain((props(I,C)))})).


% :-include('mpred_header.pi').
tCol(tPred).

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

tCol(completeExtentAsserted).
tCol(ttExpressionType).
tCol(functorDeclares).

((prologHybrid(C),{get_functor(C,F,A),C\=F}) ==> arity(F,A)).
prologHybrid(typeProps/2).
arity(typeProps,2).

% :- decl_mpred_pfc ~/1.
prologHybrid(isEach( tCol/1, disjointWith/2, genls/2,genlPreds/2, meta_argtypes/1)).

:- ignore(show_failure(why,arity(typeProps,2))).
:- must(call_u(arity(typeProps,2))).
:- ain((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,predCanHaveSingletons,prologDynamic,prologMacroHead,prologListValued,prologSingleValued),1,tPred))).
:- ain((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologHybrid,prologPTTP,prologDynamic,prologMacroHead,prologListValued,prologSingleValued),2,ftListFn(ftVoprop)))).
:- ain((isa(isEach(prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologPTTP,prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued),functorDeclares))).
:- ain((genls(isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,prologDynamic,prologBuiltin,prologKIF,prologMacroHead,prologListValued,prologSingleValued),tPred))).
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
tCol(meta_argtypes).
tCol(prologMultiValued).
tCol(prologSingleValued).
tCol(tCol).
tCol(tFunction).
tCol(tInferInstanceFromArgType).
tCol(tPred).
tCol(tRelation).
tCol(meta_argtypes).
tCol(ttTemporalType).
tCol(ttTypeType).
% tCol(tPathway).

tCol(ttValueType).
tSpec(vtActionTemplate).
ttExpressionType(ftString).
ttExpressionType(ftVar).
ttExpressionType(ftSpec).

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


:- assertz_if_new((argIsa(Prop,N,Type) :- cwc,number(N),argIsa_known(Prop,N,Type),must(ground(argIsa(Prop,N,Type))))).

argIsa(Prop,N,Type),{number(N)},ttExpressionType(Type) ==> argQuotedIsa(Prop,N,Type).

:- discontiguous(prologSingleValued/1).


:- do_gc.

:- kb_dynamic(mudLabelTypeProps/3).
:- shared_multifile(mudLabelTypeProps/3).
:- forall(ttPredType(F),must((decl_type(F),ain(isa(F,functorDeclares)),ain(genls(F,tPred))))).
:-  /**/ export(mtForPred/2).

/*
:- rtrace.
:- debug,trace,(kb_dynamic((argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, 
    isa/2,  genls/2, pddlSomethingIsa/2, 
    resultIsa/2, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, 
    ttExpressionType/1, typeProps/2))).

:- prolog. 
*/

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
tFunction(ftDice(ftInt,ftInt,ftInt)).
tFunction(ftListFn(ftRest)).
tFunction(ftDeplictsFn(tCol)).

completelyAssertedCollection(tAvoidForwardChain).
completelyAssertedCollection('SententialOperator').


tCol(tAvoidForwardChain).
tCol('SententialOperator').
%TODO tAvoidForwardChain('$VAR'('FUNC')).

tAvoidForwardChain(isEach('FunctionToArg',holds,equals,different,evaluate,trueSentence,'TINYKB-ASSERTION',termOfUnit)).
genls('SententialRelation','SententialOperator').
genls('SententialOperator',tAvoidForwardChain).
genls('VariableArityRelation',tAvoidForwardChain).
genls('CommutativeRelation',tAvoidForwardChain).
genls('tFunction',tAvoidForwardChain).
genls('EvaluatableRelation',tAvoidForwardChain).


tCol(completeIsaAsserted).
%completelyAssertedCollection(Ext):- fwc, arg(_,vv(tCol,vtDirection,ttExpressionType,tRegion,ftString, genlPreds),Ext).
completeExtentAsserted(formatted_resultIsa).
completeExtentAsserted(quotedDefnIff).
completelyAssertedCollection(completelyAssertedCollection).

ttExpressionType(ftVar).
ttExpressionType(ftVoprop).

tCol(ftSpec).
:- asserta(ftSpec(ftSpec)).

resultIsa(_F,C)/ground(C)==>ftSpec(C).

% resultIsa(F,C)==>(ftSpec(C),'tFunction'(F)).
% % ( meta_argtypes(FT)/dif(FT,COL), genls(FT, COL),tCol(COL),{not(isa(COL,ttExpressionType))}) ==> formatted_resultIsa(FT,COL).

%:- mpred_trace.
%:- pfcWatch.
%:- mpred_warn.
% next_test :- sleep(1),pfcReset.


% :- kb_dynamic((disjointWith/2,genls/2)).

argsQuoted(bordersOn).

prologHybrid(argQuotedIsa(tRelation,ftInt,ttExpressionType)).
prologHybrid(argIsa(tRelation,ftInt,tCol)).
prologHybrid(formatted_resultIsa(ttExpressionType,tCol)).



prologHybrid(quotedDefnIff(ttExpressionType,ftTerm)).
prologHybrid(defnNecessary(ttExpressionType,ftTerm)).
prologHybrid(quotedDefnIff(ttExpressionType,ftTerm)).


tFuncton(isLikeFn(tPred,tCol)).
tRelation('==>'(ftAskable,ftAssertable)).
prologHybrid(instTypeProps(ftID,tCol,ftRest(ftVoprop))).
prologHybrid(subFormat(ttExpressionType,ttExpressionType)).
prologMacroHead(macroSomethingDescription(ftTerm,ftListFn(ftString))).
prologMacroHead(pddlObjects(tCol,ftListFn(ftID))).
prologMacroHead(pddlPredicates(ftListFn(ftVoprop))).
prologMacroHead(pddlSorts(tCol,ftListFn(tCol))).
prologMacroHead(pddlTypes(ftListFn(tCol))).
prologMultiValued(comment(ftTerm,ftString)).
prologMultiValued(genlInverse(tPred,tPred)).
prologMultiValued(genlPreds(tPred,tPred)).
prologMultiValued(predProxyAssert(prologMultiValued,ftTerm)).
prologMultiValued(predProxyQuery(prologMultiValued,ftTerm)).



% prologMultiValued('<==>'(ftTerm,ftTerm)).
prologMultiValued('<-'(ftAssertable,ftAskable)).
prologMultiValued('==>'(ftAskable,ftAssertable)).
prologNegByFailure(predArgMulti(prologMultiValued,ftInt)).
prologNegByFailure(tDeleted(ftID)).

genls(ttExpressionType, ftSpec).
%= 	 	 

%% prologSingleValued( ?ARG1, ?ARG2) is semidet.
%
% Prolog Single Valued.
%
prologSingleValued(predInstMax(ftID,prologSingleValued,ftInt),prologHybrid).
prologSingleValued(predTypeMax(prologSingleValued,tCol,ftInt),prologHybrid).
resultIsa(txtFormatFn,ftText).
%'<==>'(prologMultiValued(CallSig,[predProxyAssert(hooked_asserta),predProxyRetract(hooked_retract),predProxyQuery(call)]),prologDynamic(CallSig)).
%'<==>'(prologMultiValued(CallSig,[predProxyAssert(pttp_tell),predProxyRetract(pttp_retract),predProxyQuery(pttp_ask)]),prologPTTP(CallSig)).
subFormat(ftAtom,ftTerm).
subFormat(ftCallable,ftProlog).
resultIsa(ftDice,ftInt).
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


tCol(W)==>{guess_supertypes(W)}.


tCol(tNewlyCreated).
tCol(ttTypeFacet).

:- dynamic(tNewlyCreated/1).
tNewlyCreated(W)==>{guess_types(W)}.

ttTypeFacet(tNewlyCreated).
ttTypeFacet(ttTypeFacet).
ttTypeFacet(ttUnverifiableType).


%typeGenls(tPred,ttPredType).
typeGenls(ttExpressionTypeType,ttExpressionType).
typeGenls(ttTemporalType,tTemporalThing).
typeGenls(ttTypeFacet,tCol).
typeGenls(ttTypeType,tCol).



ttTypeFacet(ttUnverifiableType).
ttUnverifiableType(ftDice).
ttUnverifiableType(ftID).
ttUnverifiableType(ftListFn(ftTerm)).
ttUnverifiableType(ftString).
ttUnverifiableType(ftTerm).
ttUnverifiableType(ftText).
ttUnverifiableType(ftVoprop).
ttUnverifiableType(tCol).
ttUnverifiableType(tFunction).
ttUnverifiableType(tPred).
ttUnverifiableType(ttExpressionType).
ttUnverifiableType(vtDirection).


%ttPredType(ArgsIsa)==>tPred(ArgsIsa).
%TODO isa(_,ArgsIsa)==>tCol(ArgsIsa).



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
quotedDefnIff(ftString,string).
quotedDefnIff(ftCallable,is_callable).
quotedDefnIff(ftCompound,is_ftCompound).
quotedDefnIff(ftGround,ground).
quotedDefnIff(ftID,is_id).
quotedDefnIff(ftTerm,is_ftNonvar).
quotedDefnIff(ftVar,is_ftVar).
quotedDefnIff(ftNonvar,is_ftNonvar).
quotedDefnIff(ftNumber,number).
quotedDefnIff(ftList,is_list).
quotedDefnIff(ftRest,is_rest).
quotedDefnIff(ftBoolean,is_boolean).
quotedDefnIff(ftText,is_string).
quotedDefnIff(ftRest(Type),is_rest_of(Type)):- cwc, is_ftNonvar(Type).
quotedDefnIff(ftListFn(Type),is_list_of(Type)):- cwc, is_ftNonvar(Type).
quotedDefnIff(ftCodeIs(SomeCode),SomeCode):- cwc, is_ftNonvar(SomeCode).



% tCol(Type),(ptBinaryPredicate(Pred)/(functor(G,Pred,2),G=..[Pred,isInstFn(Type),Value])), G ==> relationMostInstance(Pred,Type,Value).



%((genlPreds(Col1,Col2),(arity(Col1,1);arity(Col2,1)))==>genls(Col1,Col2)).
%((genls(Col1,Col2),(tPred(Col1);tPred(Col2)))==>genlPreds(Col1,Col2)).

tCol(ptBinaryPredicate).
ttPredType(ptBinaryPredicate).

isa(arity,ptBinaryPredicate).

(arity(Pred,2),tPred(Pred)) <==> isa(Pred,ptBinaryPredicate).

:- must(ain(tCol('ptUnaryPredicate'))).

isa(arity,ptBinaryPredicate).



specialFunctor('\\+').
specialFunctor('/').


:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).
/*
:- must((expand_props(_,props(iCrackers666,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]),O),ain(mdefault(O)))).

:- must((fully_expand(_,props(iCrackers666,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]),O),mpred_why(mdefault(O)))).
*/
:- endif.

arity(Pred,2),tPred(Pred) <==> ptBinaryPredicate(Pred).

% if arity is ever greater than 1 it can never become 1
% arity(F,A)/(number(A),A>1) ==> ~(arity(F,1)).

completelyAssertedCollection(ptBinaryPredicate).


% TODO ADD THIS 
%(tSet(Super),completelyAssertedCollection(Super),genls(Sub, Super), isa(I,Sub), {ground(I:Sub:Super),\==(Sub, Super)}) ==> isa(I,Super).

% genlPreds(genls,equals).
% genls(A, B):- tCol(A),{A=B}.

% (isa(TypeType,ttTypeType) , isa(Inst,TypeType), genls(SubInst,Inst)) ==> isa(SubInst,TypeType).



tCol(tPred).
prologHybrid(isa/2).

%mpred_online:semweb_startup:- with_no_term_expansions(if_file_exists(use_module(logicmoo(dbase/mpred_i_rdf_store)))).

% :- with_no_term_expansions(if_file_exists(use_module(logicmoo(mobs/planner/mpred_i_hyhtn)))).
tCol(predIsFlag).
tCol(prologDynamic).
prologHybrid(formatted_resultIsa/2).



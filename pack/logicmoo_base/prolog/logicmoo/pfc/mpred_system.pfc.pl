/** <module>
% =============================================
% File 'mpred_system.pfc'
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

:- include(logicmoo(mpred/'mpred_header.pi')).

/*
:- 
 With = kb_dynamic, % [multifile,kb_dynamic,discontiguous],
 with_pfa(With,((logical_functor_pttp/1, pfcControlled/1, pfcRHS/1,  conflict/1,   baseKB:argsQuoted/1,     add_args/15,argIsa_known/3,call_mt_t/11))),

% with_pfa(With,((( call_which_t/9,constrain_args_pttp/2,contract_output_proof/2,get_clause_vars_for_print/2,holds_f_p2/2,input_to_forms/2,is_wrapper_pred/1,lambda/5,mpred_f/1,
%          pp_i2tml_now/1,pp_item_html/2,pttp1a_wid/3,pttp_builtin/2,pttp_nnf_pre_clean_functor/3,quasiQuote/1,relax_term/6,retractall_wid/1,ruleRewrite/2,search/7,support_hilog/2,svar_fixvarname/2)))),

% with_pfa(With,((pfcControlled/1,pfcRHS/1,logical_functor_pttp/1,          add_args/15,argIsa_known/3,call_mt_t/11,call_which_t/9,constrain_args_pttp/2,contract_output_proof/2,get_clause_vars_for_print/2,holds_f_p2/2,input_to_forms/2,is_wrapper_pred/1,lambda/5,mpred_f/1,pp_i2tml_now/1,pp_item_html/2,pttp1a_wid/3,pttp_builtin/2,pttp_nnf_pre_clean_functor/3,
%          quasiQuote/1,relax_term/6,retractall_wid/1,ruleRewrite/2,search/7,support_hilog/2,svar_fixvarname/2,tNotForUnboundPredicates/1))),
 with_pfa(With,(((basePFC:bt/3),(basePFC:nt/4),(basePFC:pk/4),(basePFC:pt/3),(basePFC:spft/5),(basePFC:tms/1),(basePFC:hs/1),(basePFC:qu/3),(basePFC:sm/1),
          (('==>')/1),(('::::')/2),(('<-')/2),(('<==>')/2),(('==>')/2),(('~')/1),(('nesc')/1),((mpred_action)/1),
          (mpred_do_and_undo_method/2),
	  prologMultiValued/1,prologOrdered/1,prologNegByFailure/1,prologPTTP/1,prologKIF/1,pfcControlled/1,tPredType/1,
           prologHybrid/1,predCanHaveSingletons/1,prologDynamic/1,prologBuiltin/1,prologMacroHead/1,prologListValued/1,prologSingleValued/1,
          (basePFC:hs/2),(pfcControlled/1),(prologDynamic/2),(prologSideEffects/1),(prologSingleValued/1),(singleValuedInArg/2),(prologSideEffects/1,prologMacroHead/1,pfcControlled/1,
           resolveConflict/1,resolverConflict_robot/1)))),
 with_pfa(With,((mpred_isa/2,arity/2,mpred_module/2))),
 with_pfa(With,((baseKB:vtColor/1))).
 */

:- file_begin(pfc).
% :-  dynamic((disjointWith/2,genls/2,isa/2,argIsa/3)).
% :- baseKB:discontiguous((disjointWith/2,genls/2,isa/2,argIsa/3,typeGenls/2)).

:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,fx,('<-')).
:- op(1050,xfx,'<==>').
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).



:- kb_dynamic(tCol/1).
:- kb_dynamic(subFormat/2).
:- kb_dynamic(singleValuedInArg/2).
:- kb_dynamic(ptReformulatorDirectivePredicate/1).
:- kb_dynamic(support_hilog/2).
:- kb_dynamic(mpred_undo_sys/3).

:- dynamic(arity/2).
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
arity(F,A):- atom(F), integer(A),current_predicate(F/A),A>1.
arity(F,1):- atom(F), current_predicate(F/1),\+((dif:dif(Z,1), arity(F,Z))).


:- sanity(mpred_is_silient).
:- sanity(\+mpred_is_tracing_exec).


prologHybrid(arity/2).


% this mean to leave terms at EL:  foo('QuoteFn'([cant,touch,me])).

quasiQuote('QuoteFn').

argsQuoted('loop_check_term').
argsQuoted('loop_check_term_key').
argsQuoted('QuoteFn').
argsQuoted('$VAR').
baseKB:arity('$VAR',1).

argsQuoted(ain).
argsQuoted(meta_argtypes).
argsQuoted(ttFormated).
argsQuoted(ruleRewrite).
argsQuoted(bordersOn).
argsQuoted(mpred_action).
argsQuoted(ain).
argsQuoted(mpred_rem).
argsQuoted(added).
argsQuoted(call).
argsQuoted(call_u).
argsQuoted(member).
argsQuoted(=..).
argsQuoted({}).
argsQuoted(second_order).
% argsQuoted((':-')).


% ~(tCol({})).


prologBuiltin(F),arity(F,A)==>{make_builtin(F/A)}.

prologBuiltin(mpred_select/2).

:- kb_dynamic(conflict/1).
% a conflict triggers a Prolog action to resolve it.
conflict(C) ==> {must(with_mpred_trace_exec(resolveConflict(C),\+conflict(C)))}.

:- kb_dynamic(ttTypeType/1).

% meta rules to schedule inferencing.
% resolve conflicts asap
% mpred_select(conflict(X),W) :- basePFC:qu(umt,conflict(X),W).



{type_prefix(_Prefix,Type)}==>tCol(Type).
{type_prefix(_Suffix,Type)}==>tCol(Type).


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

meta_argtypes(support_hilog(tRelation,ftInt)).

% remove conflicts early 
% (~(P)/mpred_non_neg_literal(P) ==> ( {mpred_rem(P)}, (\+P ))).
(~(P)/mpred_non_neg_literal(P) ==> \+P ).

%:- rtrace.
(P/mpred_non_neg_literal(P) ==> (\+ ~(P))).
% a pretty basic conflict.
%(~(P)/mpred_non_neg_literal(P), P) ==> conflict(~(P)).
%(P/mpred_non_neg_literal(P), ~(P)) ==> conflict(P).

prologHybrid(genls/2).

((tPred(F),arity(F,A)/(integer(A),A>1), ~prologBuiltin(F)) ==> (~(tCol(F)),support_hilog(F,A))).


~(tCol(C))/completelyAssertedCollection(C)==> \+ completelyAssertedCollection(C).

:- kb_dynamic(support_hilog/2).

(((support_hilog(F,A)/(F\='$VAR',atom(F),integer(A),\+ static_predicate(F/A), \+ prologDynamic(F)))) ==>
   (hybrid_support(F,A), 
    {% functor(Head,F,A) ,Head=..[F|TTs], TT=..[t,F|TTs],
    %  (CL = (Head :- cwc, call(second_order(TT,CuttedCall)), ((CuttedCall=(C1,!,C2)) -> (C1,!,C2);CuttedCall)))
    CL = arity(F,A)
    },
   (CL))).


%:- kb_dynamic(hybrid_support/2).
%prologBuiltin(resolveConflict/1).

((hybrid_support(F,A)/(F\='$VAR',atom(F),integer(A), \+ prologDynamic(F),\+ static_predicate(F/A))) ==>
  ({    
    functor(G,F,A),
     (var(M)->t_l:user_abox(M);true),
     (var(M)->predicate_property(M:G,exported);true),
     (var(M)->ignore(( current_predicate(F,M:G), \+ predicate_property(M:G,imported_from(_))));true),
     % mpred_test(rebuild_pred_into(G,G,ain,[+dynamic,+multifile,+discontiguous])),         
     (predicate_property(M:G,dynamic)->true;must(convert_to_dynamic(M,F,A))),
     kb_dynamic(M:F/A),
     discontiguous(M:F/A),
     show_failure(hybrid_support, \+ static_predicate(F/A))}),
     prologHybrid(F),
    arity(F,A)).

((prologHybrid(F),arity(F,A))<==>hybrid_support(F,A)).

:- ensure_loaded('mpred_markers.pfc').

arity(genlPreds,2).


prologBuiltin(resolveConflict/1,mpred_module(baseKB)).
prologBuiltin(mpred_select/2,mpred_module(lmconf)).
%:-rtrace.
prologBuiltin(agent_text_command/4,prologDynamic).
%tPred(t,prologDynamic).

% tPred(member/2,prologBuiltin).

tCol(tNotForUnboundPredicates).

tNotForUnboundPredicates(member).

never_assert_u(vtVerb(BAD),vtVerbError):-fail,BAD=='[|]'.
never_assert_u(prologSingleValued(BAD),var_prologSingleValued(BAD)):-is_ftVar(BAD).

never_retract_u(X,is_ftVar(X)):-is_ftVar(X).
never_retract_u(~(X),is_ftVar(X)):-is_ftVar(X).
never_retract_u(human(trudy),sanity_test).
never_retract_u(tHumanHair(skRelationAllExistsFn(mudSubPart, skRelationAllExistsFn(mudSubPart, skRelationAllExistsFn(mudSubPart, iExplorer1, tHumanBody), tHumanHead), tHumanHair)),sanity_test).
never_retract_u((father(skArg1ofFatherFn(trudy), trudy)),sanity_test).
never_retract_u(argQuotedIsa(thereExistAtLeast, 1, ftPositiveInteger),sanity_test).


tPred(arity/2,prologHybrid).
tPred(is_never_type/1,prologDynamic).
tPred(term_expansion/2,prologDynamic).
tPred(var/1,prologBuiltin).

tCol(completelyAssertedCollection).
completelyAssertedCollection(completelyAssertedCollection).
completelyAssertedCollection(C)==>tCol(C).

% A Type Specification
completelyAssertedCollection(tCol).  % a type is a type
completelyAssertedCollection(tSpec). % A specification is sort of a type

:- discontiguous baseKB:tSpec/1.
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
arity(F,A):- atom(F), integer(A),current_predicate(F/A),A>1.
arity(F,1):- atom(F), current_predicate(F/1),\+((dif:dif(Z,1), arity(F,Z))).


pfcControlled(P),arity(P,A)==>hybrid_support(P,A).

ttPredType(X)==>tCol(X).
ttPredType(X)/atom(X)==>(arity(X,1),pfcControlled(X)).

tSet(ttFormatType).


% Cols are either syntactic or existential
completelyAssertedCollection(ttFormatType).  % syntactic
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
ttPredType(pfcNegTrigger).
ttPredType(pfcPosTrigger).
ttPredType(pfcBcTrigger).
ttPredType(pfcRHS).

ttPredType(pfcMustFC).

((ttPredType(X)/atom(X)) ==>support_hilog(X,1)).

ttPredType(P)==>(tSet(P),completelyAssertedCollection(P)).
ttTypeType(C)==>completelyAssertedCollection(C).

%overkill
tSet(C)==>completelyAssertedCollection(C).

%underkill
ttFormatType(C)==> ~completelyAssertedCollection(C).

tCol(C)/(atom(C),TCI=..[C,I]) ==> {decl_type(C)},arity(C,1),mpred_univ(C,I,TCI).
(tCol(C)/(atom(C), \+ static_predicate(C/1) )) ==> {kb_dynamic(C/1)}.
(tCol(C)/(atom(C),TCI=..[C,I],\+ static_predicate(C/1), \+completelyAssertedCollection(C))) 
  ==> ((TCI:-cwc,
    ( \+ ~(TCI)),
    isa_backchaing(I,C))).

% (tInferInstanceFromArgType(Col),tCol(Col)/i_name('',Col,ColName),tPred(Prop)/i_name('',Prop,PropName),{ColName=PropName}==> tInferInstanceFromArgType(Prop)).

% (tInferInstanceFromArgType(Prop),tPred(Prop),arity(Prop,N)/(N>1) ==> ({i_name('vt',Prop,FinalType)},tCol(FinalType),tInferInstanceFromArgType(FinalType),argIsa(Prop,N,FinalType))).

ttPredType(predCanHaveSingletons).
ttPredType(prologSideEffects).
prologSideEffects(write/1).
prologSideEffects(resolveConflict/1).


% :- prolog.

ttPredType(isEach(meta_argtypes,pfcDatabaseTerm,pfcControlled,pfcWatched,pfcMustFC,predIsFlag,tPred,prologMultiValued,pfcBcTrigger,
 prologSingleValued,prologMacroHead,notAssertable,prologBuiltin,prologDynamic,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,prologEquality,prologPTTP,
 prologSideEffects,prologHybrid,prologListValued)).

completelyAssertedCollection(isEach(tCol,tPred,pfcControlled)).
ttPredType(C)==>completelyAssertedCollection(C).


~(ttFormatType(prologEquality)).
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


tSet(C)==>
 ( {atom(C), functor(Head,C,1),
  ( \+(predicate_property(Head,S1))-> kb_dynamic(C/1); true),
  Head=..[C,S2],nop((S1:S2)),
 (predicate_property(Head,dynamic)->true;show_pred_info(Head))},
   functorDeclares(C),
   pfcControlled(C),
   arity(C,1),
   % (isa(I,C)/ground(I:C)==>Head),
   tCol(C)).


ttFormatType(P) ==> {get_functor(P,C), functor(Head,C,1),
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

prologBuiltin(A) :- cwc,is_ftCompound(A),get_functor(A, B),call(prologBuiltin, B).
prologBuiltin(P) :- cwc,is_ftCompound(P),!,get_functor(P,F,A),functor(C,F,A),(predicate_property(C,built_in)). % predicate_property(P,static)).
ttPredType(PT)==> {atom(PT),H=..[PT,I]}, (H:-cwc,is_ftCompound(I),get_functor(I,F),call(PT,F)).

isa(pddlSomethingIsa/2, prologHybrid).

arity(argIsa,3).

% prologHybrid(F/A)/(atom(F),number(A)) ==> arity(F,A),{must(dynamic_safe(F/A))}.

% Functions
tFunction(ArgTypes)/is_declarations(ArgTypes) ==> meta_argtypes(ArgTypes).
% FormatTypes
ttFormatType(ArgTypes)/is_declarations(ArgTypes) ==> meta_argtypes(ArgTypes).


meta_argtypes(ArgTypes)/is_ftCompound(ArgTypes) ==> {get_functor(ArgTypes,F,A)},arity(F,A).


prologMacroHead(tCol).


completelyAssertedCollection(prologSingleValued).
completelyAssertedCollection(tCol).
completelyAssertedCollection(ttFormatType).
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
ttNotTemporalType(ttFormatType).
ttNotTemporalType(ttValueType).

==>ttNotTemporalType(tCol).
ttNotTemporalType(T)==>tCol(T).
==>ttTemporalType(tTemporalThing).
ttTemporalType(T)==>tCol(T).

arity(argQuoted,1).


((isa(Inst,ttTemporalType), tCol(Inst)) ==> genls(Inst,tTemporalThing)).

% (isa(Inst,Type), tCol(Inst)) ==> isa(Type,ttTypeType).


(ttFormatType(FT),{is_ftCompound(FT)})==>meta_argtypes(FT).

tCol(vtDirection).

disjointWith(Sub, Super) ==> disjointWith( Super, Sub).
disjointWith(ttTemporalType,ttAbstractType).

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

% isa(Col1, ttObjectType) ==> ~(isa(Col1, ttFormatType)).

tCol(tCol).
tCol(tPred).
tCol(tFunction).
tCol(tRelation).
tCol(ttTemporalType).
tCol(ttFormatType).
tCol(functorDeclares).
% tCol(ArgsIsa):-ttPredType(ArgsIsa).
% TODO decide if OK
%tCol(F):-t(functorDeclares,F).
tCol(ttFormatType).


:- ensure_loaded('genls.pfc').

genls(ttSpatialType,ttTemporalType).
genls(tSpatialThing,tTemporalThing).


tCol(ttNonGenled).
% genls(ttFormatType,ttNonGenled).
isa('Thing',ttNonGenled).
isa('CycLTerm',ttNonGenled).
==>prologHybrid(quotedIsa(ftTerm,ttFormatType)).
:- kb_dynamic(quotedIsa/2).

isa(I,C):- cwc, mpred_univ(C,I,CI),atom(C),current_predicate(C,M:CI)->call(M:CI).
isa(I,C):- cwc, is_asserted(ttFormatType(C)),!, quotedIsa(I,C).
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

((P/is_ftNonvar(P),{functor(P,F,A),\+ mpred_connective(F), A>1}) ==> {lmconf:agenda_slow_op_enqueue(must(ignore(deduceEachArgType(P))))}).
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
(ttFormatType(FT)/is_declarations(FT))==>meta_argtypes(FT).


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

:- kb_dynamic(mpred_module/2).
:- decl_mpred(mpred_module/2).

meta_argtypes(mpred_module(tPred,tPrologModule)).
:- debug.

prologHybrid(mpred_module, 2).
prologMultiValued(mpred_module(tRelation,ftAtom)).


% pddlObjects(Type,EList)==>  isa(isEach(EList),Type).
% pddlSorts(Type,EList)==> genls(isEach(EList),Type).


:- kb_dynamic(argIsa/3).

:- decl_mpred(argIsa/3).

isa(Spec,tCol) ==> arity(Spec,1).

% :-ain((mpred_isa(I,C)==>{ain((isa(I,tPred),mpred_isa(I,C),props(I,[C])))})).
% :-ain((t(C,I)==>{ /* retract(hasInstance_dyn(C,I)), */ ain((isa(I,C))) , ain((props(I,C)))})).


% :-include('mpred_header.pi').
tCol(tPred).
:- mpred_test(assert_argIsa(tPred,1,tPred)).


/*
% reflexive equality
equal(A,B) ==> equal(B,A).
equal(A,B),{ \+ (A=B}),equal(B,C),{ \+ (A=C)} ==> equal(A,C).

notequal(A,B) ==> notequal(B,A).
equal(A,C),notequal(A,B) ==> notequal(C,B).
*/

% is this how to define constraints?
% either(P,Q) ==> (~(P) ==> Q), (~(Q) ==> P).
(either(P,Q) ==> ((~(P) <==> Q), (~(Q) <==> P))).
% ((P,Q ==> false) ==> (P ==> ~(Q)), (Q ==> ~(P))).


:- was_export(member/2).
:- was_export(arg/3).
%:- was_export(req/1).
% prologDynamic(cycAssert/2).
:- was_export(integer/1).
% :- was_export(makeConstant/1).
% :- was_export(naf/1).
:- was_export(number/1).
:- was_export(string/1).
:- was_export(var/1).

tCol(completeExtentAsserted).
tCol(ttFormatType).
tCol(functorDeclares).


((prologHybrid(C),{get_functor(C,F,A),C\=F}) ==> arity(F,A)).
prologHybrid(typeProps/2).
arity(typeProps,2).



% :- decl_mpred_pfc ~/1.
prologHybrid(isEach( tCol/1, disjointWith/2, genls/2,genlPreds/2, meta_argtypes/1)).

:- ignore(show_failure(why,arity(typeProps,2))).
:- mpred_test(req(arity(typeProps,2))).
:- ain((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,predCanHaveSingletons,prologDynamic,prologMacroHead,prologListValued,prologSingleValued),1,tPred))).
:- ain((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologHybrid,prologPTTP,prologDynamic,prologMacroHead,prologListValued,prologSingleValued),2,ftListFn(ftVoprop)))).
:- ain((isa(isEach(prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologPTTP,prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued),functorDeclares))).
:- ain((genls(isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,prologDynamic,prologBuiltin,prologKIF,prologMacroHead,prologListValued,prologSingleValued),tPred))).
:- assert_hasInstance(tCol,tCol).
:- file_begin(pfc).
:- debug.
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
ttFormatType(ftString).
ttFormatType(ftVar).
ttFormatType(ftSpec).

ttFormatType(ftCallable).
ttFormatType(ftPercent).

isa(vRed,vtColor).

completelyAssertedCollection(vtValue).



isa(vtColor,ttValueType).
isa(X,ttValueType)==> (genls(X,vtValue),completelyAssertedCollection(X)).

isa(vtValue,ttValueType).

typeGenls(ttValueType,vtValue).


:- mpred_test(baseKB:vtColor(vRed)).


:- assertz_if_new((argIsa(Prop,N,Type) :- cwc,number(N),argIsa_known(Prop,N,Type),must(ground(argIsa(Prop,N,Type))))).

argIsa(Prop,N,Type),{number(N)},ttFormatType(Type) ==> argQuotedIsa(Prop,N,Type).

:- discontiguous(prologSingleValued/1).
:- do_gc.

:- kb_dynamic(mudLabelTypeProps/3).
:- was_shared_multifile(mudLabelTypeProps/3).
:- forall(ttPredType(F),must((decl_type(F),ain(isa(F,functorDeclares)),ain(genls(F,tPred))))).
:- was_export(mtForPred/2).

/*
:- rtrace.
:- debug,trace,baseKB:(kb_dynamic(baseKB:(argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, 
    isa/2,  genls/2, pddlSomethingIsa/2, 
    resultIsa/2, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, 
    ttFormatType/1, typeProps/2))).

:- prolog. 
*/

prologHybrid(isEach(argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, isa/2, 
   genls/2, pddlSomethingIsa/2, resultIsa/2, subFormat/2, tCol/1, tRegion/1, 
   completelyAssertedCollection/1, ttFormatType/1, typeProps/2)).

:- ain(isa(ttFormatType,ttAbstractType)).
:- discontiguous(subFormat/2).
:- kb_dynamic(tChannel/1).
:- was_shared_multifile(tChannel/1).

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

resultIsa(F,C)==>(isa(F,'tFunction'),isa(C,ftSpec)).
resultIsa(ftDeplictsFn,ftSpec).

tPred(quotedDefnIff/2,prologHybrid).

isa(argIsa,prologHybrid).
isa(determinerString, prologMultiValued).
isa(quotedDefnIff, completeExtentAsserted).
isa(ftInt,ttFormatType).
isa(ftNumber,ttFormatType).
isa(ftString,ttFormatType).
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
%completelyAssertedCollection(Ext):- fwc, arg(_,vv(tCol,vtDirection,ttFormatType,tRegion,ftString,genlPreds),Ext).
completeExtentAsserted(formatted_resultIsa).
completeExtentAsserted(quotedDefnIff).
completelyAssertedCollection(completelyAssertedCollection).
ttFormatType(ftString).
ttFormatType(ftVar).
ttFormatType(ftVoprop).



%:- mpred_trace.
%:- pfcWatch.
%:- mpred_warn.
% next_test :- sleep(1),pfcReset.


% :- kb_dynamic((disjointWith/2,genls/2)).


prologHybrid(argQuotedIsa(tRelation,ftInt,ttFormatType)).
prologHybrid(argIsa(tRelation,ftInt,tCol)).
prologHybrid(formatted_resultIsa(ttFormatType,tCol)).



prologHybrid(quotedDefnIff(ttFormatType,ftTerm)).
prologHybrid(defnNecessary(ttFormatType,ftTerm)).
prologHybrid(defnIff(ttFormatType,ftTerm)).
prologHybrid(quotedDefnIff(ttFormatType,ftTerm)).


tFuncton(isLikeFn(tPred,tCol)).
tRelation('==>'(ftAskable,ftAssertable)).
prologHybrid(instTypeProps(ftID,tCol,ftRest(ftVoprop))).
prologHybrid(subFormat(ttFormatType,ttFormatType)).
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

tNewlyCreated(W)==>{guess_types(W)}.

ttTypeFacet(tNewlyCreated).
ttTypeFacet(ttTypeFacet).
ttTypeFacet(ttUnverifiableType).


%typeGenls(tPred,ttPredType).
typeGenls(ttFormatTypeType,ttFormatType).
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
ttUnverifiableType(ttFormatType).
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

quotedDefnIff(X,_)==>ttFormatType(X).

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

:- mpred_test(ain(tCol('ptUnaryPredicate'))).

isa(arity,ptBinaryPredicate).


((pfcControlled(C)/(get_arity(C,F,A),arity(F,A))) ==> support_hilog(F,A)).

pfcControlled(C)/has_functor(C)==>({kb_dynamic(C),get_functor(C,F,A)},arity(F,A),pfcControlled(F)).
isa(F,pfcMustFC) ==> pfcControlled(F).

(tCol(P),~ttFormatType(P)) ==> tSet(P).

prologHybrid(X)/has_functor(X)==>{kb_dynamic(X)}.
prologDynamic(X)/has_functor(X)==>{decl_mpred_prolog(X)}.
prologBuiltin(X)/has_functor(X)==>{decl_mpred_prolog(X)}.
pfcControlled(X)/is_ftCompound(X)==>{once(X=(F/A);get_functor(X,F,A)),kb_dynamic(F/A)}.


pfcControlled(C)==>prologHybrid(C).



specialFunctor('\\+').
specialFunctor('/').


:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).
/*
:- mpred_test((expand_props(_,props(iCrackers666,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]),O),ain(mpred_default(O)))).

:- mpred_test((fully_expand(_,props(iCrackers666,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]),O),mpred_why(mpred_default(O)))).
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
prologHybrid(resultIsa/2).


:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

:- mpred_test((fully_expand_goal(_,:- was_shared_multifile lmconf:create_random_fact/1,O),show_failure(why,O=(:- was_shared_multifile lmconf:create_random_fact/1)))).

% :- sanity(test_expand_units(tCol(_A))).

% :- sanity(test_expand_units(number(_A))).

% :- sanity((writeq(tCol(_A)),nl)).


tCol(vtTestType).

:- mpred_test(must_compile_special_clause(vtTestType(vTest1))).

vtTestType(vTest1).
vtTestType(vTest2).

%:-mpred_test(not(tPred(prologHybrid))).
% prologHybrid(function_corisponding_predicate(tFunction,tPred)).

:- sanity(tCol(tCol)).

:- mpred_test(agenda_rescan_for_module_ready).

:- mpred_test(must_compile_special_clause(tCol(tCol))).

:- mpred_test(must_compile_special_clause(isa(_,_))).
:- mpred_test(must_compile_special_clause(not(_))).

:- mpred_test(source_location(_,_)).

:- mpred_test(in_file_expansion;in_file_directive).

:- endif.

notAssertable(isFact/1).
prologHybrid(isFact/1).
% :- kb_dynamic(added/1).
:-asserta((added(Added):-basePFC:spft(umt,Added,U,U,_))).
isFact(A):- cwc, is_ftNonvar(A), ( added(A) ; clause_asserted(A)),not((arg(_,A,V),var(V))).



% mpred_default(((argIsa(Pred,N,FT),ttFormatType(FT)/(isFact(argIsa(Pred,N,FT)),ground(argIsa(Pred,N,FT))))==>argQuotedIsa(Pred,N,FT))).
mpred_default(((genlPreds(Child,Parent),argIsa(Parent,N,FT))==>argIsa(Child,N,FT))).
mpred_default(((genlPreds(Child,Parent),argQuotedIsa(Parent,N,FT)/ground(argIsa(Parent,N,FT)))==>argQuotedIsa(Child,N,FT))).


makeArgConstraint(I,TCol)==>{
     concat_atom([result,I],'',ResultIsa),ain(argIsa(ResultIsa,1,tFunction)),ain(argIsa(ResultIsa,2,TCol)),
     concat_atom([arg,I],'',ArgIsa),ain(argIsa(ArgIsa,1,tRelation)),ain(argIsa(ArgIsa,2,ftInt)),ain(argIsa(ArgIsa,3,TCol)),
     doall((between(1,6,N),concat_atom([arg,N,I],'',ArgNIsa),
     ain(argIsa(ArgNIsa,1,tRelation)),ain(argIsa(ArgNIsa,2,TCol)),  
     CArgNIsa =.. [ArgNIsa,Pred,Col],
     CArgIsa =.. [ArgIsa,Pred,N,Col],
     %ain((CArgNIsa<==>CArgIsa)),
     ain_fast(ruleRewrite(CArgNIsa,CArgIsa))
     ))}.

makeArgConstraint('Isa',tCol).
makeArgConstraint('Genl',tCol).
makeArgConstraint('QuotedIsa',ttFormatType).
makeArgConstraint('Format',ftTerm).
makeArgConstraint('SometimesIsa',tCol).

argFormat(arity,2,vSetTheFormat).

% {Arity=2},arity(Pred,Arity),(argIsa(Pred,Arity,ftInt)/(A=ftInt;A=ftPercent))==>singleValuedInArg(Pred,Arity).
mpred_default((arity(Pred,2),argIsa(Pred,2,ftInt))==>singleValuedInArg(Pred,2)).

argFormat(P,S,vSingleEntry)<==>singleValuedInArg(P,S).
argFormat(P,S,vSetTheFormat)<==> ~singleValuedInArg(P,S).

((arity(Pred,2),argIsa(Pred,2,ftPercent))==>singleValuedInArg(Pred,2)).


argSingleValueDefault(F, N, _)==>singleValuedInArg(F,N).


((singleValuedInArg(F,N),arity(F,A),{atom(F),integer(N),integer(A),functor(P,F,A),\+ is_ftEquality(P)}) ==> 
  (made_update_single_valued_arg(P,N),
   (P ==> {update_single_valued_arg(P,N)}))).


:- mpred_run.

:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).


% :- rescan_pfc.

:- endif.




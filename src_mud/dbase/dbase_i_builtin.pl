/** <module>
% ===================================================================
% File 'dbase_i_builtin.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt)) == holds(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt)) == padd(height,Obj,ObjHt,...) == moo(QueryForm)
% kretract[all](Obj,height(ObjHt)) == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

:- include(dbase_i_header).

% :- (do_term_expansions->true;throw(not_term_expansions)).
% :- decl_mpred_hybrid(mudNeedsLook,2).
% :- decl_mpred_prolog(createableSubclassType/2).
% :- decl_mpred_prolog(member/2).
% :- decl_mpred_prolog(mudMoveDist/2,[predArgTypes(mudMoveDist(tAgentGeneric,ftInt)),prologSingleValued,predModule(user),query(call),argSingleValueDefault(2,1)]).
% :- dynamic((weight/2)).
% :- dynamic(subclass/2).
% :- must((argIsa_call_0(comment,2,W), W\=term)).
% :- style_check(-discontiguous).
% ================================================
% =================================================================================================
% BEGIN world database
% BEGIN world English
% builtin = ftCallable native
% col =
% col(predArgTypes).
% cyc =
% database backing impls
% db_prop_prolog(world,isa(obj,col)).
% db_prop_prolog(world,same(ftID,ftID)).
% dbase_t = a dbase_t/N
% dbase_t(action_info,What,text("command is: ",What)):- holds_t(action_type,What).
% dynamic = ftCallable dynamic assert/call
% flags
% ftCallable code
% genlPreds(tPred,tPred).
% live another day to fight (meaning repl_to_string/1 for now is in ftCallable)
% subclass(dir,value).
% multivalued
% pddlSomethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).
% predModule(isa(obj,col),user).
% prologMultiValued
% prologMultiValued(gridValue(tRegion,ftInt,ftInt,tObj)).
% prologSingleValued(mudEnergy(tChargeAble,ftInt(500))).
% prologSingleValued(mudFacing(tObj,vtDirection(vNorth))).
% prologSingleValued(repl_to_string(tAgentGeneric,term),[prologSingleValued,argSingleValueDefault(2,default_repl_obj_to_string)]).
% prologSingleValued(repl_writer(tAgentGeneric,term),argSingleValueDefault(2,default_repl_writer)).
% prologSingleValued(spawn_rate(isPropFn(subclass(tObj)),ftInt)).
% pttp = pttp _int compiled
% we need a way to call this: maxCapacity
% we need a way to call this: typeMaxCapacity
%:- decl_mpred_hybrid(repl_to_string(tAgentGeneric,term),[prologSingleValued,argSingleValueDefault(2,default_repl_obj_to_string)]).
%:- decl_mpred_hybrid(repl_writer(tAgentGeneric,term),[prologSingleValued,argSingleValueDefault(2,default_repl_writer)]).
%:- forall(is_pred_declarer(F),dynamic(F/1)).
%localityOfObject(fo_T__T_T_T_TTTT_________TT__To,fo_T__T_T_T_TTTT_________TT__To_R).
%mpred(ArgTypes,[prologSingleValued]):- prologSingleValued(ArgTypes).
%mpred(ArgTypes,PropTypes):- decl_mpred_prop(ArgTypes,PropTypes).
%mpred(CallSig,[external(M)]):- prologOnly(M:CallSig).
%mpred(G,[predArgMulti(AT)|LIST]):- prologMultiValued(G,AT,LIST).
%isa(AT,ttAgentType):- subclass(AT,ttAgentGeneric).
%subclass(AT,ttAgentGeneric):- isa(AT,ttAgentType).
%prologMultiValued(G,AT,[prologOrdered|LIST]):- prologMultiValued(G,LIST),functor_catch(G,_,AT).
%prologSingleValued(mudMoveDist(tAgentGeneric,ftNumber)).
%subFormat(ftTextType,ftText).
%typeProps(tAgentGeneric,[mudMoveDist(1)]).
:- begin_transform_moo_preds.


:- discontiguous(prologSingleValued/1).
:- do_gc.
:- doall((is_pred_declarer(F),decl_type(F),add(isa(F,macroDeclarer)),add(subclass(F,tRelation)))).
:- dynamic_multifile_exported mtForPred/2.
:- user:decl_mpred_hybrid((argIsa/3, formatted_resultIsa/2, mudFtInfo/2, subFormat/2, isa/2, mudLabelTypeProps/3, subclass/2, pddlSomethingIsa/2, resultIsa/2, subFormat/2, tCol/1, tRegion/1, ttCompleteExtentAsserted/1, ttFormatType/1, typeProps/2)).


:-decl_mpred_hybrid(typeGenls/2).
:-decl_mpred_prolog(arg/3).


:- forall(is_pred_declarer(F),assert_hasInstance(macroDeclarer,F)).
:- forall(is_pred_declarer(F),must(decl_type(F))).
:- forall(is_pred_declarer(Prop),(decl_type(Prop),assert_if_new(dbase_t(subclass,Prop,tPred)))).
:- add((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,prologOnly,prologMacroHead,prologListValued,prologSingleValued),1,tPred))).
:- add((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,prologOnly,prologMacroHead,prologListValued,prologSingleValued),2,ftListFn(ftVoprop)))).
:- add((isa(isEach(prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologPTTP,prologHybrid,prologOnly,prologOnly,prologMacroHead,prologListValued,prologSingleValued),macroDeclarer))).
:- add((subclass(isEach(prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,prologOnly,prologMacroHead,prologListValued,prologSingleValued),tPred))).
:- must(mpred_arity(typeProps,2)).
makeConstant(X):- trace_or_throw(makeConstant(X)).
mpred_arity(isLikeFn,2).
mpred_prop(mudFtInfo, ttCompleteExtentAsserted).
mpred_prop(mudFtInfo/2,prologHybrid).
mpred_prop(subclass, ttCompleteExtentAsserted).
mpred_prop(subclass/2, prologHybrid).
ttFormatType(ftVoprop).
ttFormatType(ftVar).
ttFormatType(ftString).
subFormat(ftVoprop,ftTerm).
subFormat(ftVar,ftProlog).
subFormat(ftText,ftTerm).
subFormat(ftTerm,ftProlog).
subFormat(ftString,ftText).
subFormat(ftString,ftTerm).
subFormat(ftPercent,ftNumber).
subFormat(ftNumber,ftPercent).
subFormat(ftInteger,ftNumber).
subFormat(ftInt,ftNumber).
subFormat(ftID,ftTerm).
subFormat(ftDice,ftInt).
subFormat(ftCallable,ftProlog).
subFormat(ftAtom,ftTerm).
ttCompleteExtentAsserted(ttCompleteExtentAsserted).
ttCompleteExtentAsserted(mudFtInfo).
formatted_resultIsa(ftDice(ftInt,ftInt,ftInt),ftInt).
subclass(discoverableType,tCol).
isa(vtDirection,ttValueType).
isa(ftText,ttFormatType).
mudFtInfo(ftVar,prologCall(var(isSelf))).
mudFtInfo(ftText,prologCall(is_string(isSelf))).
mudFtInfo(ftTerm,prologCall(nonvar(isSelf))).
mudFtInfo(ftString,prologCall(string(isSelf))).
mudFtInfo(ftRest,prologCall(true)).
mudFtInfo(ftProlog,prologCall(predicate_property(isSelf,visible))).
mudFtInfo(ftNumber,prologCall(number(isSelf))).
mudFtInfo(ftID,prologCall((atom(isSelf),compound(isSelf)))).
mudFtInfo(ftCallable,prologCall(predicate_property(isSelf,visible))).
mudFtInfo(ftBoolean,prologCall(member(isSelf,[vTrue,vFalse]))).
mudFtInfo(ftAtom,prologCall(atom(isSelf))).
mudFormatted(ftListFn(tCol)).
mudFormatted(ftDice(ftInt,ftInt,ftInt)).
resultIsa(txtFormatFn,ftString).
prologHybrid(typeProps(tCol,ftVoprop)).
prologMultiValued(genlPreds(tPred,tPred)).
prologMultiValued(genlInverse(tPred,tPred)).
prologMacroHead(pddlTypes(ftListFn(tCol))).
prologMacroHead(pddlSorts(tCol,ftListFn(tCol))).
prologMacroHead(pddlSomethingIsa(ftTerm,ftListFn(tCol))).
prologMacroHead(pddlPredicates(ftListFn(ftVoprop))).
prologMacroHead(pddlObjects(tCol,ftListFn(ftID))).
prologMacroHead(macroSomethingDescription(ftTerm,ftListFn(ftString))).
prologHybrid(subFormat(ttFormatType,ttFormatType)).
predArgTypes(ruleForward(ftTerm,ftTerm)).
predArgTypes(pddlTypes(ftListFn(tCol))).
predArgTypes(pddlSorts(tCol,ftListFn(tCol))).
predArgTypes(pddlSomethingIsa(ftTerm,ftListFn(tCol))).
predArgTypes(pddlPredicates(ftListFn(ftVoprop))).
predArgTypes(pddlObjects(tCol,ftListFn(ftID))).
predArgTypes(mudFtInfo(ttFormatType,ftTerm)).
predArgTypes(macroSomethingDescription(ftTerm,ftListFn(ftString))).
predArgTypes(isLikeFn(tPred,tCol)).
predArgTypes(formatted_resultIsa(ttFormatType,tCol)).
predArgTypes(argIsa(tRelation,ftInt,tCol)).
predArgTypes(argFormat(tRelation,ftInt,ttFormatType)).
predArgTypes(argSingleValueDefault(prologSingleValued,ftInt,ftTerm)).
ruleEquiv(prologMultiValued(CallSig,[predProxyAssert(pttp_tell),predProxyRetract(pttp_retract),predProxyQuery(pttp_ask)]),prologPTTP(CallSig)).
ruleEquiv(prologMultiValued(CallSig,[predProxyAssert(hooked_asserta),predProxyRetract(hooked_retract),predProxyQuery(call)]),prologOnly(CallSig)).

prologSingleValued(predTypeMax(prologSingleValued,tCol,ftInt)).
prologSingleValued(predInstMax(ftID,prologSingleValued,ftInt)).
prologMultiValued(ruleForward(ftTerm,ftTerm)).
prologMultiValued(ruleEquiv(ftTerm,ftTerm)).
prologMultiValued(ruleBackward(ftTerm,ftTerm)).
prologNegByFailure(tDeleted(ftID)).
prologNegByFailure(predArgMulti(prologMultiValued,ftInt)).
prologMultiValued(predProxyQuery(prologMultiValued,ftTerm)).
prologMultiValued(predProxyAssert(prologMultiValued,ftTerm)).
prologMultiValued(predModule(tRelation,ftAtom)).


prologMultiValued(comment(ftTerm,ftString)).
prologHybrid(instTypeProps(ftID,tCol,ftVoprop)).


% isa(Inst,tHasAction):- nonvar(Inst),isa(Inst,Type),tCol(Type),isa(Type,ttTypeByAction).
cycAssert(A,B):- trace_or_throw(cycAssert(A,B)).

disjointWith(tObj,tRegion).
disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):- disjointWithT(A,B).
disjointWith(A,B):- disjointWithT(AS,BS),transitive_subclass_or_same(A,AS),transitive_subclass_or_same(B,BS).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.

tChannel(iGossupChannel).
tChannel(A):- tRegion(A).
tChannel(A):- tAgentGeneric(A).

user:hook_coerce(Text,tPred,Pred):- mpred_prop(Pred,predArity(_)),name_text(Pred,Text).

:- must_det(argIsa_call(genlPreds,2,_Type)).

% =================================================================================================
% END world database
% =================================================================================================

:- end_transform_moo_preds.

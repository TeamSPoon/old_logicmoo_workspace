/** <module>
% ===================================================================
% File 'logicmoo_i_builtin.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% ===================================================================
%
% PFC is a language extension for prolog.. there is so much that can be done in the language for exmaple.. 
%
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
end_of_file.

:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<=>').
:- op(1050,xfx,('<=')).
:- op(1100,fx,('=>')).
:- op(1150,xfx,('::::')).
:- dynamic(tCol/1).

:- dynamic(subFormat/2).
:- dynamic(pfc_undo_sys/3).

:- include((logicmoo_i_header)).


sane_transitivity(_PredInfo,I,Sub,Super):-  
% (I=Super->trace_or_throw(sane_transitivity(PredInfo,I,Sub,Super));true),
%  sanity(I=Sub->trace_or_throw(sane_transitivity(PredInfo,I,Sub,Super));true),
%  sanity(Sub=Super->trace_or_throw(sane_transitivity(PredInfo,I,Sub,Super));true),
  \=(I,Super),\=(Sub,Super),\=(I,Sub),
  \+(isa(Super,ttNonGenled)),
  \+(isa(I,ttNonGenled)).

(genls(I,Sub),genls(Sub, Super),{sane_transitivity((genls(I,Sub),genls(Sub, Super)),I,Sub,Super)}) => genls(I,Super).










tCol(tFly).

(mpred_prop(_,meta_argtypes(ArgTypes)),{is_declarations(ArgTypes)}) =>  meta_argtypes(ArgTypes).



a=>b.
b=>c.
c=>a.


jj=>jj.


% completeExtentAsserted(genls)



implies(and(resultIsaArg('$VAR'('FUNC'), 5), arg5Genl('$VAR'('FUNC'), '$VAR'('COL'))), resultIsa('$VAR'('FUNC'), '$VAR'('COL'))).




dividesBetween(tTemporalThing,tMassfull,tMassless).
dividesBetween(tObj,tMassfull,tMassless).

((dividesBetween(S,C1,C2),{ground(S:C1:C2)}) => ({ground(S:C1:C2)},(disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)))).

isa(Col1, ttObjectType) => ~isa(Col1, ttFormatType).

(neg(isa(I,Super)) <= (disjointWith(Sub, Super),isa(I,Sub))).
% disjointWith(P1,P2) => {\+(isa(P1,ttNonGenled)),\+(isa(P2,ttNonGenled))},(neg(isa(C,P1)) <=> isa(C,P2)).

tCol(tCol).
tCol(tPred).
tCol(tFunction).
tCol(tRelation).
tCol(ttTemporalType).
tCol(ttFormatType).
/*

% this isn't written yet.
resolveConflict(C) :- dtrace,
  format("~NHalting with conflict ~w", [C]),
  pfc_halt.


% a conflict triggers a Prolog action to resolve it.
conflict(C) => {resolveConflict(C)}.

% meta rules to schedule inferencing.

% resolve conflicts asap
pfc_select(conflict(X),S) :- pfc_queue(conflict(X),S).
  
% a pretty basic conflict.
((neg(P), P ) => conflict(P)).

*/

tCol(tFly).
tCol(tCanary).
tCol(tPenguin).
tCol(tBird).
genls(tCanary,tBird).
genls(tPenguin,tBird).
%(isa(A, tBird) =>isa(A, tFly)).
%(isa(A, tBird), ~neg(isa(A, tFly))=>isa(A, tFly)).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% % % These next two have been comnbined with the two following % % %
(((pfc_default(P)/pfc_literal(P))  =>  (~neg(P) => P))).
((pfc_default((P => Q))/pfc_literal(Q) => (P, ~neg(Q) => Q))).


%((pfc_default(P)/pfc_literal(P), {pfcVerifyMissing(P,F)})) =>  ((F, ~neg(P)) => P).
%((pfc_default((P => Q))/pfc_literal(Q), {pfcVerifyMissing(Q,F)})) => ((P, F, ~neg(Q)) => Q).
% % % 
(pfc_default((Q <= P))/pfc_literal(Q)) => (Q <=(P, ~neg(Q))).
%(pfc_default((P => Q))/pfc_literal(Q)) => (Q <=(P, ~neg(Q))).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

neg(P) <= {pfcVerifyMissing(P,F,Test)},Test,{F\=P}.
isMissing(P) <= {pfcVerifyMissing(P,F,Test)},Test,{F\=P}.

% is this how to define constraints?
either(P,Q) => (neg(P) => Q), (neg(Q) => P).
% (P,Q => false) => (P => neg(Q)), (Q => neg(P)).

% rembmer the tCol rule points to isa/2
% tCol(C)=>{atom(C),P=..[C,I],assertz_if_new((P:-infoF(isa(I,C)))),assertz_if_new((P:-isa(I,C)))}.

tCol(C)=> {atom(C),P=..[C,I],assertz_if_new((P:-infoF(isa(I,C)))),assertz_if_new((isa(I,C):-no_repeats(P)))}.
% isa(I,C):- tCol(C),{append_term(C,I,G)},(G/current_predicate(_,G)).
% isa(I,C):- no_repeats((no_repeats(tCol(C)),append_term(C,I,G),current_predicate(_,G),G)).

prologHybrid((zDefault/1,tPenguin/1,tFly/1,tBird/1,tCanary/1)).

'UnaryPredicate'(Pred)<=>(arity(Pred,1),tPred(Pred)).

'UnaryPredicate'(zDefault).
((zDefault(P/pfc_literal(P)))  =>  (~neg(P) => P)).
zDefault((P => Q))/pfc_literal(Q) => ((P, ~neg(Q) => Q)).
%zDefault((P => Q))/pfc_literal(Q) => (Q <=(P, ~neg(Q))).
%zDefault((Q <= P))/pfc_literal(Q) => (Q <=(P, ~neg(Q))).

% birds fly by default.
zDefault((tBird(X) => tFly(X))).

% here's one way to do an isa hierarchy.
% genlPreds = subclass.

/*
(genlPreds(C1,C2),arity(C1,2)) =>
  {P1 =.. [C1,X,Y],
    P2 =.. [C2,X,Y]},
  (P1 => P2).

(genlPreds(C1,C2),arity(C1,3)) =>
  {P1 =.. [C1,X,Y,Z],
    P2 =.. [C2,X,Y,Z]},
  (P1 => P2).
*/



(zenlPreds(C1,C2)) =>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 => P2).


zenlPreds(tCanary,tBird).
zenlPreds(tPenguin,tBird).

% penguins do neg fly.
tPenguin(X) => neg(tFly(X)).

% chilly is a penguin.
tPenguin(iChilly).

% tweety is a canary.
tCanary(iTweety).


% birds fly by default.
(pfc_default(( tBird(X) => tFly(X)))).


tCol(tFly).
tCol(tCanary).
tCol(tPenguin).
tCol(tBird).

% => genls(tBird,tFly).


% penguins do neg tFly.
tPenguin(X) => neg(tFly(X)).

% iChilly is a tPenguin.
((=> tPenguin(iChilly))).

% iTweety is a tCanary.
((=> tCanary(iTweety))).

:-must(in_file_expansion;in_file_directive).

:-listing(tBird).
:-listing(tFly).



end_of_file.


:-dynamic((fly/1,bird/1,penguin/1,canary/1)).


% birds fly by default.
(pfc_default((bird(X) => fly(X)))).

% heres one way to do an subclass hierarchy.

(((genls_test(C1,C2) =>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 => P2)))).

(genls_test(canary,bird)).
(genls_test(penguin,bird)).

% penguins do neg fly.
(penguin(X) => neg(fly(X))).

% chilly is a penguin.
(penguin(chilly)).

% tweety is a canary.
(canary(tweety)).

:-listing([fly/1,bird/1,penguin/1,canary/1]).

:-ignore(show_call_failure(fly(tweety))).

/*

 the CycL language extends Prolog's first order logic capabilities with some higher order logics.  
 It also extrends prolog to show proofs.. one issue is the CycL language never signed up for cuts or other execution  orders.    
 PrologMUD extends the CycL language to allow preset program flow (unless a predicate is declared to not honor order of execution 
  (this is usually best!)).  PrologMUD  implements a new design of the cyc canonicalizer..   

 usually in Cyc the rules "(implies (and Axy Byz) (and Cwxyz Dwx))" are converted to DNF (Correct me if I am wrong.. 
 since i have heard it uses ConjectiveNormalForm as well) ... the DNF generates Clausal forms..  The forms choosen 



?-  kif_to_boxlog(((parent('$VAR'('G'),'$VAR'('P')) & parent('$VAR'('P'),'$VAR'('C'))) => grandparent('$VAR'('G'),'$VAR'('C'))),O). 

O = [ (-parent(G, P):- -grandparent(G, C), parent(P, C)), 
      (-parent(P, C):- -grandparent(G, C), parent(G, P)), 
      (grandparent(G, C):-parent(G, P), parent(P, C))].


?- kif_to_boxlog( (grandparent('$VAR'('G'),'$VAR'('C')) => exists('$VAR'('P'), (parent('$VAR'('G'),'$VAR'('P')) & parent('$VAR'('P'),'$VAR'('C'))))),O).

    (-grandparent(G, C):- mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)), (-parent(G, P) ; -parent(P, C))),   % You have proven G is not the grandparent of C when you have proven tha G has no children or that C has no parents
    (-mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)):- grandparent(G, C), (-parent(G, P) ; -parent(P, C))), 
    (parent(G, P):-grandparent(G, C), mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G))), % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P
    (parent(P, C):-grandparent(G, C), mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G))),
    (mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)):- grandparent(G, C),  \+ (parent(G, P) , parent(P, C)))]  % We failed to find a true P


O = [ 
      (-grandparent(G, P):- -parent(G, _P) ; -parent(_P, P)),    
      parent(G, P):- grandparent(G, C), parent(P,C),   % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P
      parent(P, C):- grandparent(G, C), parent(G,P))].   % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P

*/



/*

genls(_Sub, Super) => tCol(Super).
genls(Sub, _Super) => tCol(Sub).
% use backchain instead (isa(I,Sub), disjointWith(Sub, Super)) => neg(isa(I,Super)).

*/

:- must_det(argIsa(genlPreds,2,_Type)).


% Hook a predicate up!
/*
(tinyInstallToAssertedEL(F,A) => ({current_predicate(make_el_stub/4),functor(H,F,A),make_el_stub(H,_,_,B)},(H<=B))).

((tinyPredTypeActive(PREDTYPE),isa(F,PREDTYPE)) => tinyPredActive(F)).

((tinyPredActive(F),arity(F,A))=>tinyInstallToAssertedEL(F,A)).

=>tinyPredActive(isa).
=>tinyPredActive(genls).
=>tinyPredActive(arity).
=>tinyInstallToAssertedEL(afterAdding,2).

=>tinyPredTypeActive('WFFConstraintSatisfactionPredicate').
=>tinyPredTypeActive('WFFConstraintPredicate').
=>tinyPredTypeActive('WFFSupportedPredicate').

=>tinyPredActive(afterRemoving).
=>tinyPredActive(afterAdding).

% argQuotedIsa(Pred, N, FT) <=  (argIsa(Pred, N, Type),genls(Type,FT),ttFormatType(FT)).

% consistent(CycL) <= cycLToMpred(neg(CycL),Consequent),not(pfcCall(Consequent)). 

=>tinyPredTypeActive('expansion').
=>tinyPredTypeActive('collectionExpansion').
(isa(F,tPred),arity(F,A),{once((tinyAssertion0(DB,MT,STR),arg(1,DB,F),atom(F)))})=>tinyInstallToAssertedEL(F,A).
*/

argQuotedIsa(sentenceTruth,1,'CycLSentence-Assertible').
:- pfc_add(((collectionExpansion(COL,RULE),isa(PRED,COL),{subst(RULE,':ARG1',PRED,NEWRULE)}) => trueSentence(NEWRULE))).
:- pfc_add(((expansion(F,RULE),arity(F,A),{make_kw_functor(F,A,CYCL),kw_to_vars((CYCL<=>RULE),NEWRULE)}) => trueSentence(NEWRULE))).

nearestIsa(I,C) <= is_user_supported(isa(I,C)).
nearestGenls(I,C) <= is_user_supported(genls(I,C)).


% meta_argtypes(ArgTypes)/is_declarations(ArgTypes) => {wdmsg(in_loop(meta_argtypes))},meta_argtypes(ArgTypes).
tRelation(ArgTypes)/is_declarations(ArgTypes) => meta_argtypes(ArgTypes).
:- pfc_add(((isa(Compound,prologMacroHead)/compound_functor(Compound,F)) => functorDeclares(F))).
(ttFormatType(FT)/is_declarations(FT))=>meta_argtypes(FT).

:- show_call(source_location(_,_)).

:- must(in_file_expansion;in_file_directive).


end_of_file.

%(exactlyAssertedSentence(CycL), is_simple_gaf(CycL)) => ({cycAdd(CycL)},CycL).
%(exactlyAssertedSentence(CycL), needs_indexing(CycL)) => assertedSentence(CycL).
%(exactlyAssertedSentence(CycL), needs_canoncalization(CycL)) => trueSentence(CycL).
%:- prolog.
assertedSentence(CycL) => ({ sent_to_conseq(CycL,Consequent) }, Consequent).
trueSentence(CycL) => ({ sent_to_conseq(CycL,Consequent) }, Consequent).


% :- pfc_add(((isa(Compound,prologMacroHead)/compound_functor(Compound,F)) => functorDeclares(F))).
% :- rtrace,pfc_add((isa(_,ArgsIsa)=>tCol(ArgsIsa))).




% (typeProps(Type,Props)=>(tSet(Type),((isa(I,Type)=>props(I,Props))))).


% props(I,Props)/(ground(I:Props),as_list(Props,PList))=>pfc_assert(props(I,isEach(PList))).

))).

.


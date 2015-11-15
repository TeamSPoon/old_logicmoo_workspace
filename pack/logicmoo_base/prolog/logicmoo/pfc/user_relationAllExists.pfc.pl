
:- file_begin(pfc).



:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).
:- kb_dynamic(tCol/1).
:- kb_dynamic(tHominid/1).

:- kb_dynamic(relationAllOnly/3).
:- kb_dynamic(ptTransitiveBinaryPredicate/1).

predInterArgIsa(mudSubPart(tBodyPart,tBodyPart)).
relationAllOnly(mudSubPart,tHumanBody,tBodyPart).

:- mpred_trace_exec.
tSet(tHumanBody).

((relationAllOnly(Pred,Col1,Col2)/(G=..[Pred,VAL,Value])) ==> 
   (isa(VAL,Col1) ==> (( G ==> isa(Value,Col2))))).

cycl('(implies 
       (and 
           (isa ?BPRED SymmetricBinaryPredicate) 
           (transitiveViaArg ?PRED ?BPRED ?N)) 
       (transitiveViaArgInverse ?PRED ?BPRED ?N))').



ptTransitiveBinaryPredicate(genls).
ptTransitiveBinaryPredicate(mudSubPart).
(ptTransitiveBinaryPredicate(P)/ground(P)) ==>
    ((t(P,A,B),t(P,B,C)) ==> t(P,A,C)).

((t(isa,A,B),t(genls,B,C)) ==> t(isa,A,C)).

((transitiveViaArg(PRED,BPRED,2),arity(PRED,2)) /ground(PRED:BPRED)) ==> clif((t(PRED,A,B) , t(BPRED,B,C)) => t(PRED,A,C)).
((transitiveViaArgInverse(PRED,BPRED,2),arity(PRED,2))/ground(PRED:BPRED)) ==> clif((t(PRED,A,B) & t(BPRED,C,B)) => t(PRED,A,C)).

((transitiveViaArg(PRED,BPRED,2),arity(PRED,3)) /ground(PRED:BPRED)) ==> clif((t(PRED,A,B,Z) , t(BPRED,B,C)) => t(PRED,A,C,Z)).
((transitiveViaArgInverse(PRED,BPRED,2),arity(PRED,3))/ground(PRED:BPRED)) ==> clif((t(PRED,A,B,Z) & t(BPRED,C,B)) => t(PRED,A,C,Z)).

((transitiveViaArg(PRED,BPRED,3),arity(PRED,3)) /ground(PRED:BPRED)) ==> clif((t(PRED,Z,A,B) , t(BPRED,B,C)) => t(PRED,Z,A,C)).
((transitiveViaArgInverse(PRED,BPRED,3),arity(PRED,3))/ground(PRED:BPRED)) ==> clif((t(PRED,Z,A,B) , t(BPRED,C,B)) => t(PRED,Z,A,C)).

(relationAllExists(Pred,Col1,Col2) ==> (ptBinaryPredicate(Pred),tCol(Col1),tCol(Col2))).

% version that works but not safe
/*
relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value]) ==> 
  (isa(VAL,Col1) ==> 
    ((( {ignore(cnstrn(Value,isa(Value,Col2))},(~ (G/isa(Value,Col2)))) ==> ({Value=skRelationInstanceExistsFn(Pred,VAL,Col2)},isa(Value,Col2), G))))).
*/

/*
relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value]) ==> 
  (isa(VAL,Col1) ==> 
    ((~ (G/isa(Value,Col2))) ==> ({Value=skRelationInstanceExistsFn(Pred,VAL,Col2)},isa(Value,Col2), G))).


relationAllExists(Pred,Col1,Col2) ==>
 ({G1=..[Pred,VAL,Value1],G2=..[Pred,VAL,Value2],Value2=skRelationInstanceExistsFn(Pred,VAL,Col2)},
  (isa(VAL,Col1) ==> (
    ((((~ (G1/(isa(Value1,Col2))))) ==> (isa(Value2,Col2), G2)))))).

*/


relationInstanceExists(Pred,VAL,D_COL) ==>
 ({SK= skRelationInstanceExistsFn(Pred,VAL,D_COL), G1=..[Pred,VAL,Missing],G2=..[Pred,VAL,SK],ISA=..[D_COL,SK]},
  (( ~ (G1/(isa(Missing,D_COL),is_non_skolem(Missing)))) ==> (G2,ISA))).


(relationAllExists(Pred,I_COL,D_COL)==> 
  clif(all(I,exists(D,  t(Pred,I,D) & isa(I,I_COL) & isa(D,D_COL))))).

(relationAllExists(Pred,I_COL,D_COL)==>
 ({SK= skRelationAllExistsFn(Pred,VAL,D_COL), G1=..[Pred,VAL,Missing],G2=..[Pred,VAL,SK],ISA=..[D_COL,SK]},
  (isa(VAL,I_COL) ==> 
    ((((~ (G1/(isa(Missing,D_COL),is_non_skolem(Missing))))) ==> (G2,ISA)))))).



relationExistsAll(Pred,D_COL,I_COL) ==>
 ({SK= skRelationExistsAllFn(VAL,Pred,D_COL,I_COL), G1=..[Pred,Missing,VAL],G2=..[Pred,SK,VAL],ISA=..[D_COL,SK]},
   (isa(VAL,I_COL) ==>   
  (( ~ (G1/(isa(Missing,D_COL),is_non_skolem(Missing)))) ==> (G2,ISA)))).

relationExistsInstance(Pred,D_COL,VAL) ==>
 ({SK= skRelationExistsInstanceFn(Pred,D_COL,VAL), G1=..[Pred,Missing,VAL],G2=..[Pred,SK,VAL],ISA=..[D_COL,SK]},
  (( ~ (G1/(isa(Missing,D_COL),is_non_skolem(Missing)))) ==> (G2,ISA))).


prologHybrid(relationMostInstance(ptBinaryPredicate,tCol,vtValue)).
relationMostInstance(BP,_,_)==>ptBinaryPredicate(BP).
(relationMostInstance(Pred,_,Value),{\+number(Value)},argIsa(Pred,2,Type))==> isa(Value,Type).
%((relationMostInstance(Pred,Type,Value),{G=..[Pred,Inst,Value],GI=..[Pred,Inst,_]})) ==> (({GI=..[Pred,Inst,_]},isa(Inst,Type), ~GI) ==> G ).
relationMostInstance(Pred,Type,Value) ==> mdefault(isa(Inst,Type) ==> t(Pred,Inst,Value)).
% relationMostInstance(Pred,Type,Value) ==> mdefault( isa(Inst,Type) ==> ?Pred(Inst,Value) ).



prologHybrid(relationAllInstance(ptBinaryPredicate,tCol,vtValue)).
relationAllInstance(BP,_,_)==>ptBinaryPredicate(BP).
relationAllInstance(Pred,_,Value),{\+number(Value)},argIsa(Pred,2,Type)==>(isa(Value,Type),isa(Pred,ptRolePredicate)).

relationAllInstance(Pred,I_COL,VAL) ==>
 ({G1=..[Pred,INST,_Missing],G2=..[Pred,INST,VAL]},
  (isa(INST,I_COL) ==> ( ~ G1 ==> G2))).

relationInstanceAll(Pred,VAL,I_COL) ==>
 ({G2=..[Pred,VAL,INST]},
  (isa(INST,I_COL) ==>  G2 )).

/*
(relationAllExists(Pred,Col1,Col2)/(Value = skRelationInstanceExistsFn(Pred,VAL,Col2),G=..[Pred,VAL,Value]))
  ==> 
  ((isa(VAL,Col1), G) ==> G).
*/

% version that works best but wrong
/*
(relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value])) ==> 
  ((isa(VAL,Col1),G,isa(Value,Col2)) ==> unneeded(relationAllExists(Pred,VAL,Col2))).

((relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value]))==>
   (isa(VAL,Col1) ==> ((~G , ~unneeded(relationAllExists(Pred,VAL,Col2))) ==> ({Value=skRelationInstanceExistsFn(Pred,VAL,Col2)},isa(Value,Col2), G)))).
*/


/*
relationAllExists(Pred,Col1,Col2)/(G=..[Pred,VAL,Value]) ==> 
  (isa(VAL,Col1) ==> 
    ((( ~((G,isa(Value,Col2)))) ==> ({Value=skRelationInstanceExistsFn(Pred,VAL,Col2)},isa(Value,Col2), G)))).
*/


:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

isa(iExplorer1,tHumanPlayer).
isa(iExplorer2,tHumanPlayer).
genls(tHumanPlayer,tHominid).
genls(tHumanPlayer,tPlayer).
genls(tPlayer,tAgent).

isa(iExplorer1,tHominid).


%mudSubPart(iExplorer2, iBody2).
%mudSubPart(iExplorer1, iHumanBody1).

%isa(iHumanBody1,tHumanBody).


:- endif.

tCol(tHumanBody).
genls(tHumanBody,tBodyPart).

relationAllExists(mudSubPart,tHominid,tHumanBody).
relationAllExists(mudSubPart,tHumanBody,isEach(tHumanHead,tHumanNeck,tHumanUpperTorso,tHumanLowerTorso,tHumanPelvis,tHumanArms,tHumanLegs)).
relationAllExists(mudSubPart,tHumanHead,isEach(tHumanFace,tHumanHair)).


:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

% :- mpred_spy_all.

:- endif.

/*
have to confirm how *most* works
==>
'(==>
  (relationInstanceMost ?BINPRED ?THING ?COL2)
     (=> 
       (isa ?THING ?COL1)        
       (relationExistsMost ?BINPRED ?COL1 ?COL2)))'.
*/


isa(skRelationAllExistsFn(P,A,C),C):- nonvar(P),nonvar(A),nonvar(C).

:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

?- listing(mudSubPart).
%:-rtrace((isa(Inst,tHumanNeck),mudSubPart(iExplorer1,Inst))).
%:-rtrace((mudSubPart(iExplorer1,Inst),isa(Inst,tHumanNeck))).
%:- must((isa(Inst,tHumanHair),mudSubPart(iExplorer1,Inst))).
%:- must((mudSubPart(iExplorer1,Inst),isa(Inst,tHumanNeck))).
%:- must((mudSubPart(iExplorer1,Inst),isa(Inst,tHumanHair))).

?- listing(mudSubPart).


:- endif.





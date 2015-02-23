/** <module> 
% ===================================================================
% File 'dbase_c_term_expansion'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == holds(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/


reduce_clause(Op,C,HB):-demodulize(Op,C,CB),CB\=@=C,!,reduce_clause(Op,CB,HB).
reduce_clause(Op,clause(C, B),HB):-!,reduce_clause(Op,(C :- B),HB).
reduce_clause(_,(C:- B),HB):- is_true(B),!,reduce_clause(Op,C,HB).
reduce_clause(_,C,C).

dbase_head_expansion(_,V,V ):-var(V),!.
dbase_head_expansion(Op,H,GG):-correct_negations(Op,H,GG),!.
dbase_head_expansion(_,V,V).

% ================================================
% fully_expand_maplist/3
% ================================================

any_op_to_call_op(_,call(conjecture)).

fully_expand_maplist(FE,[E],E,G,O):- !,call(FE,Op,G,O).
fully_expand_maplist(FE,[E|List],T,G,O):- copy_term(T+G,CT+CG),E=CT,!,call(FE,CG,O1),fully_expand_maplist(FE,List,T,G,O2),conjoin(O1,O2,O).
fully_expand_maplist(FE,List,T,G,O):-findall(M, (member(T,List),call(FE,Op,G,M)), ML),list_to_conjuncts(ML,O).


% ================================================
% fully_expand/3
%   SIMPLISTIC REWRITE (this is not the PRECANONICALIZER)
% ================================================

as_is_term(M:NC):-atom(M),!,compound(NC),functor(NC,Op,2),infix_op(Op,_).
as_is_term(NC):-var(NC),!.
as_is_term('$VAR'(_)):-!.
as_is_term(NC):-compound(NC),functor(NC,Op,2),infix_op(Op,_).

:-moo_hide_childs(fully_expand/3).


fully_expand_goal(_,A,B):-thlocal:infSkipFullExpand,!,A=B.
fully_expand_goal(Op,I,O):-fully_expand_each(Op,I,O).

fully_expand(_,A,B):-thlocal:infSkipFullExpand,!,A=B.
fully_expand(Op,A,B):- ((must(fully_expand0(Op,A,BO)),!,must(B=BO),((A\=@=B,B\=isa(_,_), (A \=@= user:B)) ->dmsg(fully_expand(Op,(A->B)));true))),!.

fully_expand0(_,Sent,SentO):-not(compound(Sent)),!,must(SentO=Sent).
fully_expand0(Op,':-'(Sent),':-'(OUT)):-!,fully_expand_goal(Op,Sent,OUT),!.
fully_expand0(Op,Sent,SentO):-var(Op),!,fully_expand0(is_asserted,Sent,SentO),!.
fully_expand0(Op,':-'(Sent,B),':-'(OUT,B)):- is_true(B), !,fully_expand0(Op,Sent,OUT),!.
fully_expand0(Op,SentI,SentO):- loop_check(fully_expand_each(Op,SentI,SentO),SentI=SentO),!.

fully_expand_each(Op,SI,SentO):- with_assertions(thlocal:into_form_code,transitive(fully_expand_each0(Op),SI,SentO)).
fully_expand_each0(_ ,NC,NC):-not(compound(NC)),!.
fully_expand_each0(_ ,NC,NC):-as_is_term(NC),!.
fully_expand_each0(_ ,NC,OUT):-pfc_expand(NC,OUT),!.
fully_expand_each0(Op,SI,SentO):-
       transitive(fully_expand_z(Op),SI,S1),!,
       transitive(fully_expand_1(Op),S1,S2),!,transitive(fully_expand_2(Op),S2,S3),!,
       transitive(fully_expand_3(Op),S3,S4),!,transitive(fully_expand_4(Op),S4,S5),!,
       transitive(fully_expand_5(Op),S5,SentO).

pfc_expand(PfcRule,Out):-compound(PfcRule),functor(PfcRule,F,A),pfcDatabaseTerm(F/A),
   PfcRule=[F|Args],maplist(fully_expand_each(Op),Args,ArgsO),!,Out=..[F|ArgsO].
 
fully_expand_z(A,B,C):-loop_check(fully_expand_0(A,B,C),B=C).
fully_expand_0(_ ,NC,NC):-not(compound(NC)),!.
fully_expand_0(_ ,NC,NC):-as_is_term(NC),!.
fully_expand_0(Op,=>(G),(GG)):-!,fully_expand_0(Op,(G),(GG)).
fully_expand_0(Op,(G,B),(GG,BB)):-!,fully_expand_0(Op,G,GG),fully_expand_0(Op,B,BB).
fully_expand_0(Op,(G;B),(GG;BB)):-!,fully_expand_0(Op,G,GG),fully_expand_0(Op,B,BB).
fully_expand_0(Op,(G:-B),(GG:-BB)):-!,fully_expand_0(Op,G,GG),fully_expand_goal(Op,B,BB).
fully_expand_0(_,Term,CL):- expands_on(isEach,Term),!,findall(O,do_expand_args(isEach,Term,O),L),!,list_to_conjuncts(L,CL).
%fully_expand_0(Op,mpred_prop(F,Props),O):- Props==tCol, !,fully_expand_0(Op,isa(F,tCol),O).
fully_expand_0(Op,pddlSomethingIsa(A,List),O):- !,fully_expand_maplist(fully_expand(Op),List,E,isa(A,E),O).
fully_expand_0(Op,pddlDescription(A,List),O):- !,fully_expand_maplist(fully_expand(Op),List,E,mudDescription(A,E),O).
fully_expand_0(Op,pddlObjects(Type,List),O):- !,fully_expand_maplist(fully_expand(Op),List,I,isa(I,Type),O).
fully_expand_0(Op,pddlSorts(Type,List),O):- !,fully_expand_maplist(fully_expand(Op),List,I,subclass(I,Type),O).
fully_expand_0(Op,pddlTypes(List),O):- !,fully_expand_maplist(fully_expand(Op),List,I,isa(I,tCol),O).
fully_expand_0(Op,pddlPredicates(List),O):- !,fully_expand_maplist(fully_expand(Op),List,I,isa(I,tPred),O).
fully_expand_0(Op,EACH,O):- EACH=..[each|List],fully_expand_maplist(fully_expand(Op),List,T,T,O).

fully_expand_0(Op,mpred_prop(I, C),mpred_prop(I, C)):-atom(C),!.
fully_expand_0(Op,predArgTypes(F,Args),isa(Args,predArgTypes)):-atom(F),!,functor(Args,Pred,A),assert_arity(Pred,A).
fully_expand_0(Op,predArgTypes(Args),isa(Args,predArgTypes)):-!,functor(Args,Pred,A),assert_arity(Pred,A).
fully_expand_0(Op,PredDEclaration,O):-PredDEclaration=..[F,Pred/A|Args],is_pred_declarer(F),
  integer(A),
  assert_arity(Pred,A),
  expand_props(Op,props(Pred,[mpred_arity(A),F,tPred|Args]),O),!.
fully_expand_0(Op,PredDEclaration,O):-PredDEclaration=..[F,PredDecl|Args],is_pred_declarer(F),
  compound(PredDecl),functor(PredDecl,Pred,A), (F==predArgTypes -> ISA = tPred ; ISA=F),
  assert_arity(Pred,A),
  expand_props(Op,props(Pred,[mpred_arity(A),predArgTypes(PredDecl),tPred,ISA|Args]),O),!.
fully_expand_0(Op,PredDEclaration,O):-PredDEclaration=..[F,Pred|Args],is_pred_declarer(F),
  atom(Pred),
  expand_props(Op,props(Pred,[tPred,F|Args]),O),!.

% fully_expand_0(Op,decl_mpred(F,Props),O):-!,fully_expand_0(Op,mpred_prop(F,Props),O).

fully_expand_0(_,mpred_prop(F,Props),props(F,[tPred|Props])):-is_list(Props),!,
fully_expand_0(_,mpred_prop(F,Props),props(F,[tPred,Props])).
% fully_expand_0(_,mpred_arity(F,A),tPred(F,[mpred_arity(A)])).
fully_expand_0(_,mpred_arity(F,A),mpred_arity(F,A)):-atom(F),!.

/*
fully_expand_0(Op,MT:Term,MT:O):- is_kb_module(MT),!,with_assertions(thlocal:caller_module(kb,MT),fully_expand_0(Op,Term,O)).
fully_expand_0(Op,DB:Term,DB:O):- dbase_mod(DB),!,with_assertions(thlocal:caller_module(db,DB),fully_expand_0(Op,Term,O)).
fully_expand_0(Op,KB:Term,KB:O):- atom(KB),!,with_assertions(thlocal:caller_module(prolog,KB),fully_expand_0(Op,Term,O)).
*/
fully_expand_0(Op,(:-(CALL)),(:-(CALLO))):-with_assert_op_override(Op,fully_expand_0(Op,CALL,CALLO)).
fully_expand_0(Op,(mpred_call(CALL)),(mpred_call(CALLO))):-with_assert_op_override(Op,fully_expand_0(Op,CALL,CALLO)).
fully_expand_0(_ ,include(CALL),(load_data_file_now(CALL))):-!.
fully_expand_0(_ ,HB,HB).

% fully_expand_0(query(HLDS,Must),props(Obj,Props)):- nonvar(Obj),var(Props),!,gather_props_for(query(HLDS,Must),Obj,Props).

demodulize(Op,H,HHH):-once(strip_module(H,_,HH)),H\==HH,!,demodulize(Op,HH,HHH).
demodulize(Op,H,HH):-compound(H),H=..[F|HL],!,maplist(demodulize(Op),HL,HHL),HH=..[F|HHL],!.
demodulize(_ ,HB,HB).


fully_expand_1(_,V,_):-var(V),!,fail.
fully_expand_1(Op,Sent,SentO):-predicate_property(Sent,meta_predicate(_)),Sent=..[F|List],!,maplist(fully_expand_1(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO].
fully_expand_1(Op,Sent,SentO):-Sent=..[F|List],is_logical_functor(F),maplist(fully_expand_each(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO],!.
fully_expand_1(change(_,_),Sent,SentO):-user:ruleRewrite(Sent,SentO),!.
% fully_expand_1(_,Sent,SentO):-loop_check(expand_term(Sent,SentO),Sent=SentO),!.

fully_expand_2(_,V,_):-var(V),!,fail.
fully_expand_2(Op,Sent,SentO):-predicate_property(Sent,meta_predicate(_)),Sent=..[F|List],!,maplist(fully_expand_2(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO].
fully_expand_2(Op,Sent,SentO):-Sent=..[F|List],is_logical_functor(F),!,maplist(fully_expand_each(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO].
fully_expand_2(Op,Sent,SentO):-transitive(expand_props(Op),Sent,SentO),!.
fully_expand_2(_Op,Sent,SentO):-current_hilog(Functor),transform_holds(Functor,Sent,SentO).

fully_expand_3(_,V,_):-var(V),!,fail.
fully_expand_3(Op,Sent,SentO):-predicate_property(Sent,meta_predicate(_)),Sent=..[F|List],!,maplist(fully_expand_3(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO].
fully_expand_3(Op,Sent,SentO):-Sent=..[F|List],is_logical_functor(F),!,maplist(fully_expand_each(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO].
fully_expand_3(Op,RDF,OUT):- RDF=..[SVO,S,V,O],is_svo_functor(SVO),!,must_det(into_expand_mpred_form(Op,[dbase_t,V,S,O],OUT)).
fully_expand_3(_Op,Sent,SentO):-into_mpred_form(Sent,SentO).

fully_expand_4(_,A,B):-thglobal:pfcManageHybrids,!,A=B.
fully_expand_4(_,V,_):-var(V),!,fail.
fully_expand_4(Op,Sent,SentO):-db_quf(Op,Sent,Pretest,Template),(Pretest==true-> SentO = Template ; SentO = (Pretest,Template)),!.

fully_expand_5(_ ,NC,NC):- as_is_term(NC),!.
fully_expand_5(Op,{Sent},{Sent}):-!.
fully_expand_5(Op,Sent,SentO):-predicate_property(Sent,meta_predicate(_)),Sent=..[F|List],!,maplist(fully_expand_5(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO].
fully_expand_5(Op,Sent,SentO):-Sent=..[F|List],is_logical_functor(F),!,maplist(fully_expand_5(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO].
fully_expand_5(_,A,B):-thglobal:pfcManageHybrids,!,A=B.
fully_expand_5(Op,Sent,SentO):-must(correctArgsIsa(Op,Sent,SentO)),!.

instTypePropsToType(instTypeProps,ttSpatialType).
typePropsToType(typeProps,tCol).
%typePropsToType(mpred_prop,tPred).
%typePropsToType(props,ftTerm).

into_expand_mpred_form(Op,prop(Obj,Prop),Out):-!,transitive(expand_props(Op),prop(Obj,Prop),Out).
into_expand_mpred_form(_,In,Out):-transitive(into_mpred_form,In,Out).

expand_props(_,Sent,OUT):-not(compound(Sent)),!,OUT=Sent.
expand_props(Op,(True,Term),OUT):- True==true,!,expand_props(Op,(Term),OUT).
%expand_props(Op,Term,OUT):- stack_check,(var(Op);var(Term)),!,trace_or_throw(var_expand_units(Op,Term,OUT)).
expand_props(Op,Sent,OUT):-Sent=..[And|C12],is_logical_functor(And),!,maplist(expand_props(Op),C12,O12),OUT=..[And|O12].
expand_props(Op,props(Obj,Open),OUT):- var(Open),!,trace_or_throw(expand_props(Op,props(Obj,Open)),OUT).
expand_props(Op,props(Obj,List),nop(expand_props(Op,props(Obj,[])))):- List==[],!.
expand_props(Op,props(Obj,[P]),OUT):- nonvar(P),!,expand_props(Op,props(Obj,P),OUT).
expand_props(Op,props(Obj,[P|ROPS]),OUT):- !,expand_props(Op,props(Obj,P),OUT1),expand_props(Op,props(Obj,ROPS),OUT2),conjoin(OUT1,OUT2,OUT).
expand_props(Op,props(Obj,PropVal),OUT):- atom(PropVal),!,Call=..[PropVal,Obj],!,into_expand_mpred_form(Op,Call,OUT).
expand_props(Op,props(Obj,PropVal),OUT):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,into_expand_mpred_form(Op,[dbase_t,Prop,Obj|Val],OUT).
expand_props(Op,props(Obj,PropVal),OUT):- PropVal=..[Op,Pred|Val],comparitiveOp(Op),
   not(comparitiveOp(Pred)),!,OPVAL=..[Op|Val],PropVal2=..[Pred,OPVAL],
    expand_props(Op,props(Obj,PropVal2),OUT).
expand_props(Op,props(Obj,PropVal),OUT):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,into_expand_mpred_form(Op,[dbase_t,Prop,Obj|Val],OUT).
expand_props(Op,props(Obj,PropVal),OUT):- PropVal=..[Prop|Val],!,trace_or_throw(dtrace),into_expand_mpred_form(Op,[dbase_t,Prop,Obj|Val],OUT).

expand_props(Op,ClassTemplate,OUT):- ClassTemplate=..[TypeProps,Type|Props],
  typePropsToType(TypeProps,TypePropsIsa),
  TypeProps\=props,
  assert_isa(Type,TypePropsIsa),
  create_the_inst_fn(_ALL,Type,TypePropsIsa,NewType),
  NewClassTemplate=..[props,NewType|Props],
  expand_props(Op,NewClassTemplate,OUT),!.

expand_props(Op,ClassTemplate,OUT):- ClassTemplate=..[TypeProps,X,Type|Props],
  instTypePropsToType(TypeProps,TypePropsIsa),
  TypeProps\=props,
  assert_isa(Type,TypePropsIsa),
  create_the_inst_fn(X,Type,TypePropsIsa,NewType),
  NewClassTemplate=..[props,NewType|Props],
  expand_props(Op,NewClassTemplate,OUT),!.


expand_props(Op,ClassTemplate,OUT):- get_functor(ClassTemplate,F,A),not(A==2),
   ClassTemplate=..[TypeProps,Type|Props],
   typePropsToType(TypeProps,TypePropsIsa),F==TypeProps,!,   
   assert_isa(Type,TypePropsIsa),
   flatten(Props,AllProps),
   MID=..[TypeProps,Type,AllProps],
   expand_props(Op,MID,OUT),dmsg(warn(used_type_props(MID))),!.

expand_props(_,Sent,Sent).

create_the_inst_fn(_,Type,PropsIsa,NewType):-PropsIsa==tCol, NewType = isInstFn(Type),!.
% create_the_inst_fn(_,Type,PropsIsa,NewType):-!, NewType = isInstFn(Type),!.
create_the_inst_fn(X,Type,PropsIsa,NewType):- NewType = isKappaFn(X,and(isa(X,Type),isa(Type,PropsIsa))),!.

:-export(conjoin/3).
conjoin_op(Op,A,B,C) :-  C =.. [Op,A,B].


db_quf_l(Op,And,[C],D2,D4):- !, db_quf(Op,C,D2,D3),!,D4=..[And,D3].
db_quf_l(Op,And,C12,Pre2,Templ2):-db_quf_l_0(Op,And,C12,Pre2,Templ2).

db_quf_l_0(Op,_And,[C],D2,D3):- db_quf(Op,C,D2,D3),!.
db_quf_l_0(Op, And,[C|C12],PreO,TemplO):-
  db_quf(Op,C,Pre,Next),
  db_quf_l_0(Op,And,C12,Pre2,Templ2),
  conjoin(Pre,Pre2,PreO),
  conjoin_op(And,Next,Templ2,TemplO).

:-export(db_quf/4).
db_quf(Op,C,Pretest,Template):-not(compound(C)),!,must(Pretest=true),must(Template=C).
db_quf(Op,C,Pretest,Template):-var(C),!,trace_or_throw(var_db_quf(Op,C,Pretest,Template)).
db_quf(Op,C,Pretest,Template):-as_is_term(C),!,must(Pretest=true),must(Template=C),!.

db_quf(Op,M:C,Pretest,M:Template):-atom(M),!,must(db_quf(Op,C,Pretest,Template)).

db_quf(Op,Sent,D2,D3):- Sent=..[And|C12],C12=[_|_],is_logical_functor(And),!, db_quf_l(Op,And,C12,D2,D3).

db_quf(Op,':-'(C,D),':-'(C2,D2),':-'(C3,D3)):-!,db_quf(Op,C,C2,C3),db_quf(Op,D,D2,D3).
db_quf(Op,','(C,D),','(C2,D2),','(C3,D3)):-!,db_quf(Op,C,C2,C3),db_quf(Op,D,D2,D3).

db_quf(Op,C,Pretest,Template):- C=..[Holds,OBJ|ARGS],is_holds_true(Holds),atom(OBJ),!,C1=..[OBJ|ARGS],must(db_quf(Op,C1,Pretest,Template)).
db_quf(_Op,C,true,C):- C=..[Holds,OBJ|_],is_holds_true(Holds),var(OBJ),!.

db_quf(Op,C,Pretest,Template):- C=..[Prop,OBJ|ARGS],
      functor(C,Prop,A),
      show_call_failure(translate_args(Op,Prop,A,OBJ,2,ARGS,NEWARGS,true,Pretest)),
      Template =.. [Prop,OBJ|NEWARGS],!.
db_quf(_Op,C,true,C).

translate_args(_O,_Prop,_A,_OBJ,_N,[],[],GIN,GIN).
translate_args(Op,Prop,A,OBJ,N1,[ARG|S],[NEW|ARGS],GIN,GOALS):-
   must(argIsa_call(Op,Prop,N1,Type)),
   translateOneArg(Op,Prop,OBJ,Type,ARG,NEW,GIN,GMID),
   N2 is N1 +1,
   translate_args(Op,Prop,A,OBJ,N2,S,ARGS,GMID,GOALS).


% ftVar
translateOneArg(_Op,_Prop,_Obj,_Type,VAR,VAR,G,G):-var(VAR),!.

% not an expression
translateOneArg(_O,_Prop,_Obj,_Type,ATOMIC,ATOMIC,G,G):-atomic(ATOMIC),!.
% translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,same_arg(col(Type),ATOMIC,ATOMICUSE))):-atomic(ATOMIC),!.

% translateOneArg(_O,_Prop,_Obj,Type,VAR,VAR,G,G):-ignore(isa(VAR,Type)),!.

% props(Obj,size < 2).
translateOneArg(_O,Prop,Obj,Type,ARG,OLD,G,(GETTER,COMPARE,G)):-
       functor(ARG,F,2), comparitiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,Obj,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,isOneOf(Sz,[size+1,2])).
translateOneArg(Op,Prop,O,Type,isOneOf(VAL,LIST),VAL,G,(GO,G)):-
   translateListOps(Op,Prop,O,Type,VAL,LIST,G,GO).

% db_op(Op, Obj,size + 2).
translateOneArg(_O,Prop,Obj,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       functor(ARG,F,2), additiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,Obj,OLD],
       STORE= update_value(OLD,VAL,NEW),!.

translateOneArg(_O,_Prop,_Obj,_Type,NART,NART,G,G):-!.
translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,ignore(same_arg(tCol(Type),ATOMIC,ATOMICUSE)))).

translateListOps(_O,_Prop,_Obj,_Type,_VAL,[],G,G).
translateListOps(Op,Prop,Obj,Type,VAL,[L|LIST],G,GO2):-
   translateOneArg(Op,Prop,Obj,Type,L,VAL,G,GO),
   translateListOps(Op,Prop,Obj,Type,VAL,LIST,GO,GO2).

compare_op(Type,F,OLD,VAL):-nop(Type),show_call((call(F,OLD,VAL))),!.

% start of database
% These will all be deleted at start of run

:-export(inverse_args/2).
inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

% =======================================================
% correctArgsIsa/3
% =======================================================

:-export(same_vars/2).
same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.


:-moo_hide_childs(replace_arg/4).
replace_arg(C,A,OLD,CC):- 
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),!,
   CC=..FARGO.

:-moo_hide_childs(replace_nth/4).
replace_nth([],_,_,[]):- !.
replace_nth([_|ARGO],0,OLD,[OLD|ARGO]):- !.
replace_nth([T|FARGS],A,OLD,[T|FARGO]):- 
    A2 is A-1,replace_nth(FARGS,A2,OLD,FARGO).


replace_nth([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw(missed_the_boat).
replace_nth([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth(ARGS,Which1,OldVar,NewVar,NEWARGS),!.


:-moo_hide_childs(update_value/4).
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):-compute_value_no_dice(NEW,NEWV),!.


list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.

compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).

compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).

insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):- argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).

verb_after_arg(_,_,1).


% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.

% ========================================
% expanded_different compares fact terms to see if they are different
% ========================================

:-'$hide'(expanded_different/2).
:-export(expanded_different/2).

expanded_different(G0,G1):-call(expanded_different_ic(G0,G1)).

expanded_different_ic(G0,G1):-G0==G1,!,fail.
expanded_different_ic(G0,G1):-expanded_different_1(G0,G1),!.
expanded_different_ic(G0,G1):- G0\==G1.

expanded_different_1(NV:G0,G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,NV:G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,G1):- (var(G0);var(G1)),!,trace_or_throw(expanded_different(G0,G1)).
expanded_different_1(G0,G1):- G0 \= G1,!.


% ========================================
% into_assertable_form/2 (use mpred_prop to decide what functor if any)
% ========================================

into_assertable_form(M:H,G):-atom(M),!,into_assertable_form(H,G).
into_assertable_form(H,G):- get_functor(H,F,A),into_assertable_form4(H,F,A,G).

into_assertable_form4(G,F,_,Dbase):-mpred_prop(F,prologHybridFunctor(Tdbase_t)),!,into_functor_form(Tdbase_t,G,Dbase).
into_assertable_form4(G,F,_,GG):- mpred_prop(F,prologOnly),!,into_mpred_form(G,GG).
into_assertable_form4(G,F,_,GG):- mpred_prop(F,prologPTTP),!,into_mpred_form(G,GG).
into_assertable_form4(G,F,_,GG):- mpred_prop(F,prologSNARK),!,into_mpred_form(G,GG).
into_assertable_form4(G,F,_,Dbase):-mpred_prop(F,prologHybrid),!,into_functor_form(dbase_t,G,Dbase).


% ========================================
% into_functor_form/3 (adds a second order functor onto most predicates)
% ========================================
:-export(into_assertable_form/3).
into_functor_form(HFDS,M:X,O):- atom(M),!,into_functor_form(HFDS,X,O),!.
into_functor_form(HFDS,X,O):-call((( X=..[F|A],into_functor_form(HFDS, X,F,A,O)))),!.

% TODO finish negations
into_functor_form(Dbase_t,X,Dbase_t,_A,X):-!.
into_functor_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_functor_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_functor_form(Dbase_t,_X,HFDS,A,Call):- is_holds_true(HFDS), Call=..[Dbase_t|A].
into_functor_form(Dbase_t,_X,F,A,Call):-Call=..[Dbase_t,F|A].

% ========================================
% into_mpred_form/2 (removes a second order functors until the common mpred form is left)
% ========================================
:-moo_hide_childs(into_mpred_form/2).
:-export(into_mpred_form/2).
into_mpred_form(V,VO):-not(compound(V)),!,VO=V.
into_mpred_form(M:X,M:O):- atom(M),!,into_mpred_form(X,O),!.
into_mpred_form((H:-B),(HH:-BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H:-B),(HH:-BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H,B),(HH,BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H;B),(HH;BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form(WAS,isa(I,C)):-was_isa(WAS,I,C),!.
into_mpred_form(dbase_t(P,A,B),O):-atomic(P),!,O=..[P,A,B].
into_mpred_form(dbase_t(P,A,B,C),O):-atomic(P),!,O=..[P,A,B,C].
into_mpred_form(Var,MPRED):- var(Var), trace_or_throw(var_into_mpred_form(Var,MPRED)).
into_mpred_form(I,O):-loop_check(into_mpred_form_lc(I,O),O=I). % trace_or_throw(into_mpred_form(I,O))).

into_mpred_form_lc([F|Fist],O):-!,G=..[dbase_t|[F|Fist]], into_mpred_form(G,O).
into_mpred_form_lc(G,O):- functor(G,F,A),G=..[F,P|ARGS],!,into_mpred_form6(G,F,P,A,ARGS,O),!.

% TODO confirm negations

into_mpred_form6(H,_,_,_,_,GO):- once(with_assertions(thlocal:into_form_code,(expand_term( (H :- true) , C ), reduce_clause(is_asserted,C,G)))),expanded_different(H,G),!,into_mpred_form(G,GO),!.
into_mpred_form6(_,F,_,1,[C],O):-alt_calls(F),!,into_mpred_form(C,O),!.
into_mpred_form6(_,':-',C,1,_,':-'(O)):-!,into_mpred_form_lc(C,O).
into_mpred_form6(_,not,C,1,_,not(O)):-into_mpred_form(C,O),!.
into_mpred_form6(C,isa,_,2,_,C):-!.
into_mpred_form6(C,_,_,_,_,isa(I,T)):-was_isa(C,I,T),!.
into_mpred_form6(_X,dbase_t,P,_N,A,O):-!,(atom(P)->O=..[P|A];O=..[dbase_t,P|A]).
into_mpred_form6(G,_,_,1,_,G):-predicate_property(G,number_of_rules(N)),N >0, !.
into_mpred_form6(G,F,C,1,_,O):-real_builtin_predicate(G),!,into_mpred_form(C,OO),O=..[F,OO].
into_mpred_form6(_X,H,P,_N,A,O):-is_holds_false(H),(atom(P)->(G=..[P|A],O=not(G));O=..[holds_f,P|A]).
into_mpred_form6(_X,H,P,_N,A,O):-is_holds_true(H),(atom(P)->O=..[P|A];O=..[dbase_t,P|A]).
into_mpred_form6(G,F,_,_,_,G):-mpred_prop(F,prologHybrid),!.
into_mpred_form6(G,F,_,_,_,G):-mpred_prop(F,prologOnly),!.
into_mpred_form6(G,F,_,_,_,G):-nop(dmsg(warn(unknown_mpred_type(F,G)))).

% ========================================
% acceptable_xform/2 (when the form is a isa/2, do a validity check)
% ========================================
acceptable_xform(From,To):- From \=@= To,  (To = isa(I,C) -> was_isa(From,I,C); true).

% ========================================
% transform_holds(Functor,In,Out)
% ========================================
transform_holds(H,In,Out):- once(transform_holds_3(H,In,Out)),!,ignore((In\=Out,fail,dmsg(transform_holds(H,In,Out)))).


% foreach_arg/7 
%  is a maping predicate
foreach_arg(ARGS,_N,_ArgIn,_ArgN,_ArgOut,_Call,ARGS):-not(compound(ARGS)),!.
foreach_arg([ArgIn1|ARGS],ArgN1,ArgIn,ArgN,ArgOut,Call1,[ArgOut1|ARGSO]):-
     copy_term( a(ArgIn1,ArgOut1,ArgN1,Call1), a(ArgIn,ArgOut,ArgN,Call) ),
      call(Call),
      ArgN2 is ArgN + 1,
      foreach_arg(ARGS,ArgN2,ArgIn,ArgN,ArgOut,Call,ARGSO).

transform_functor_holds(_,F,ArgInOut,N,ArgInOut):- once(argIsa_ft(F,N,FT)),FT=ftTerm,!.
transform_functor_holds(Op,_,ArgIn,_,ArgOut):- transform_holds(Op,ArgIn,ArgOut),!.

transform_holds_3(_,A,A):-not(compound(A)),!.
transform_holds_3(_,props(Obj,Props),props(Obj,Props)):-!.
%transform_holds_3(Op,Sent,OUT):-Sent=..[And|C12],is_logical_functor(And),!,maplist(transform_holds_3(Op),C12,O12),OUT=..[And|O12].
transform_holds_3(_,A,A):-functor_catch(A,F,N), predicate_property(A,_),mpred_prop(F,mpred_arity(N)),!.
transform_holds_3(HFDS,M:Term,OUT):-atom(M),!,transform_holds_3(HFDS,Term,OUT).
transform_holds_3(HFDS,[P,A|ARGS],DBASE):- var(P),!,DBASE=..[HFDS,P,A|ARGS].
transform_holds_3(HFDS, ['[|]'|ARGS],DBASE):- trace_or_throw(list_transform_holds_3(HFDS,['[|]'|ARGS],DBASE)).
transform_holds_3(Op,[SVOFunctor,Obj,Prop|ARGS],OUT):- is_svo_functor(SVOFunctor),!,transform_holds_3(Op,[Prop,Obj|ARGS],OUT).
transform_holds_3(_,[P|ARGS],[P|ARGS]):- not(atom(P)),!,dmsg(transform_holds_3),trace_or_throw(dtrace).
transform_holds_3(HFDS,[HOFDS,P,A|ARGS],OUT):- is_holds_true(HOFDS),!,transform_holds_3(HFDS,[P,A|ARGS],OUT).
transform_holds_3(HFDS,[HOFDS,P,A|ARGS],OUT):- HFDS==HOFDS, !, transform_holds_3(HFDS,[P,A|ARGS],OUT).
transform_holds_3(_,HOFDS,isa(I,C)):- was_isa(HOFDS,I,C),!.
transform_holds_3(_,[Type,Inst],isa(Inst,Type)):-nonvar(Type),isa(Type,tCol),!.
transform_holds_3(_,HOFDS,isa(I,C)):- holds_args(HOFDS,[ISA,I,C]),ISA==isa,!.

transform_holds_3(Op,[Fogical|ARGS],OUT):-  
         call(call,is_logical_functor(Fogical)),!,sanity(not(is_svo_functor(Fogical))),
         must_det(foreach_arg(ARGS,1,ArgIn,ArgN,ArgOut,transform_functor_holds(Op,Fogical,ArgIn,ArgN,ArgOut),FARGS)),
         OUT=..[Fogical|FARGS].

transform_holds_3(_,[props,Obj,Props],props(Obj,Props)).
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[isa(Type)|PROPS])):- 
                  nonvar(Inst), not(Type=props), mpred_call(tCol(Type)), must_det(not(is_never_type(Type))),!.
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[isa(Type)|PROPS])):- 
                  nonvar(Inst), not(Type=props), hasInstance(functorDeclares,Type), must_det(not(is_never_type(Type))),!.

transform_holds_3(_,[P,A|ARGS],DBASE):- atom(P),!,DBASE=..[P,A|ARGS].
transform_holds_3(_,[P,A|ARGS],DBASE):- !, nonvar(P),dumpST,trace_or_throw(dtrace), DBASE=..[P,A|ARGS].
transform_holds_3(Op,DBASE_T,OUT):- DBASE_T=..[P,A|ARGS],!,transform_holds_3(Op,[P,A|ARGS],OUT).


holds_args([H|FIST],FISTO):- !, is_holds_true(H),!,FIST=FISTO.
holds_args(HOFDS,FIST):- compound(HOFDS),HOFDS=..[H|FIST],is_holds_true(H),!.


% ================================================
%  expand_goal_correct_argIsa/2
% ================================================
expands_on(EachOf,Term):-subst(Term,EachOf,foooz,Term2),!,Term2\=Term,not((do_expand_args(EachOf,Term,O),O = Term)).
if_expands_on(EachOf,Term,Call):- expands_on(EachOf,Term),subst(Call,Term,O,OCall),!, forall(do_expand_args(EachOf,Term,O),OCall).

/*
%db_reop(WhatNot,Call) :- into_mpred_form(Call,NewCall),NewCall\=@=Call,!,db_reop(WhatNot,NewCall).
db_reop(Op,Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),db_reop_l(Op,O)).
db_reop(Op,Term):-db_reop_l(Op,Term).

db_reop_l(query(_HLDS,Must),Call) :- !,preq(Must,Call).
db_reop_l(Op,DATA):-no_loop_check(db_op0(Op,DATA)).

dmsg_hook(transform_holds(dbase_t,_What,props(ttSpatialType,[isa(isa),isa]))):-trace_or_throw(dtrace).

*/


% expand_goal_correct_argIsa(A,A):-simple_code,!.
expand_goal_correct_argIsa(A,B):- expand_goal(A,B).

% db_op_simpler(query(HLDS,_),MODULE:C0,req(call,MODULE:C0)):- atom(MODULE), nonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor_catch(C0,F,A), dmsg(todo(unmodulize(F/A))), %trace_or_throw(module_form(MODULE:C0)), %   db_op(Op,C0).
db_op_simpler(_,TypeTerm,props(Inst,[isa(Type)|PROPS])):- TypeTerm=..[Type,Inst|PROPS],nonvar(Inst),hasInstance(functorDeclares,Type),!.


db_op_sentence(_Op,Prop,ARGS,C0):- atom(Prop),!, C0=..[Prop|ARGS].
db_op_sentence(_Op,Prop,ARGS,C0):- C0=..[dbase_t,Prop|ARGS].


:-export(simply_functors/3).
simply_functors(Db_pred,query(HLDS,Must),Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,query(HLDS,Must),Simpler).
simply_functors(Db_pred,Op,Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,Op,Simpler).


% -  dmsg_hook(db_op(query(HLDS,call),holds_t(ft_info,col,'$VAR'(_)))):-trace_or_throw(dtrace).

% ================================================
% add_from_file/2
% ================================================
add_from_file(B,_):- contains_singletons(B),trace_or_throw(dtrace),dmsg(todo(add_from_file_contains_singletons(B))),!,fail.
add_from_file(B,B):- add(B). % db_op(change(assert,_OldV),B),!.

univ_left(Comp,[M:P|List]):- nonvar(M),univ_left0(M, Comp, [P|List]),!.
univ_left(Comp,[H,M:P|List]):- nonvar(M),univ_left0(M,Comp,[H,P|List]),!.
univ_left(Comp,[P|List]):-dbase_mod(DBASE), univ_left0(DBASE,Comp,[P|List]),!.
univ_left0(M,M:Comp,List):- Comp=..List,!.



:-export((force_expand_head/2,force_head_expansion/2)).
:-export((force_expand_goal/2)).
force_expand_head(G,GH) :- force_head_expansion(G,GH),!.
force_expand_goal(A, B) :- force_expand(expand_goal(A, B)).

:-thread_local inside_clause_expansion/1.
   
set_list_len(List,A,NewList):-length(List,LL),A=LL,!,NewList=List.
set_list_len(List,A,NewList):-length(List,LL),A>LL,length(NewList,A),append(List,_,NewList),!.
set_list_len(List,A,NewList):-length(NewList,A),append(NewList,_,List),!.


is_mpred_prolog(F,_):-get_mpred_prop(F,prologOnly).

declare_as_code(F,A):-findall(n(File,Line),source_location(File,Line),SL),ignore(inside_clause_expansion(CE)),decl_mpred(F,prologOnly),decl_mpred(F,info(discoveredInCode(F/A,SL,CE))),!.
if_mud_asserted(F,A2,_,_Why):-is_mpred_prolog(F,A2),!,fail.
if_mud_asserted(F,A2,A,Why):-using_holds_db(F,A2,A,Why).


is_kb_module(Moo):-atom(Moo),member(Moo,[add,dyn,abox,tbox,kb,opencyc]).
is_kb_mt_module(Moo):-atom(Moo),member(Moo,[moomt,kbmt,mt]).

:-export(if_use_holds_db/4).
if_use_holds_db(F,A2,_,_):- is_mpred_prolog(F,A2),!,fail.
if_use_holds_db(F,A,_,_):-  never_use_holds_db(F,A,_Why),!,fail.
if_use_holds_db(F,A2,A,Why):- using_holds_db(F,A2,A,Why),!.
if_use_holds_db(F,A,_,_):- declare_as_code(F,A),fail.

never_use_holds_db(F,N,Why):-trace_or_throw(todo(find_impl,never_use_holds_db(F,N,Why))).

isCycPredArity_Check(F,A):-get_mpred_prop(F,cycPred(A)).

using_holds_db(F,A,_,_):- never_use_holds_db(F,A,_),!,fail.
using_holds_db(F,A2,A,m2(F,A2,isCycPredArity_Check)):- integer(A2), A is A2-2, A>0, isCycPredArity_Check(F,A),!.
using_holds_db(F,A,A,tCol(F/A)):- integer(A), tCol(F),!, must(A>0).
using_holds_db(F,A,A,isCycPredArity_Check):- isCycPredArity_Check(F,A).
using_holds_db(F,A,A,W):-integer(A),!,fail,trace_or_throw(wont(using_holds_db(F,A,A,W))).

ensure_moo_pred(F,A,_,_):- never_use_holds_db(F,A,Why),!,trace_or_throw(never_use_holds_db(F,A,Why)).
ensure_moo_pred(F,A,A,is_mpred_prolog):- is_mpred_prolog(F,A),!.
ensure_moo_pred(F,A,NewA,Why):- using_holds_db(F,A,NewA,Why),!.
ensure_moo_pred(F,A,A,Why):- dmsg(once(ensure(Why):decl_mpred(F,A))),decl_mpred(F,A).

prepend_module(_:C,M,M:C):-!.
prepend_module(C,M,M:C).

negate_wrapper(P,N):-var(P),trace_or_throw(call(negate_wrapper(P,N))).
negate_wrapper(Dbase_t,Dbase_f):-negate_wrapper0(Dbase_t,Dbase_f).
negate_wrapper(Dbase_f,Dbase_t):-negate_wrapper0(Dbase_t,Dbase_f).
negate_wrapper(P,N):-trace_or_throw(unkown(negate_wrapper(P,N))).

negate_wrapper0(holds_t,holds_f).
negate_wrapper0(dbase_t,dbase_f).
negate_wrapper0(int_firstOrder,int_not_firstOrder).
negate_wrapper0(firstOrder,not_firstOrder).
negate_wrapper0(asserted_dbase_t,asserted_dbase_f).
negate_wrapper0(Dbase_t,Dbase_f):- atom_concat(Dbase,'_t',Dbase_t),atom_concat(Dbase,'_f',Dbase_f).

:-thread_local hga_wrapper/3.
hga_wrapper(dbase_t,holds_t,dbase_t).

get_goal_wrappers(if_use_holds_db, Holds_t , N):- hga_wrapper(_,Holds_t,_),!,negate_wrapper(Holds_t,N),!.
get_goal_wrappers(if_use_holds_db, holds_t , holds_f).

get_head_wrappers(if_mud_asserted, Holds_t , N):- hga_wrapper(Holds_t,_,_),!,negate_wrapper(Holds_t,N),!.
get_head_wrappers(if_mud_asserted, dbase_t , dbase_f).

get_asserted_wrappers(if_mud_asserted, Holds_t , N):-  hga_wrapper(_,_,Holds_t),!,negate_wrapper(Holds_t,N),!.
get_asserted_wrappers(if_mud_asserted, dbase_t , dbase_t).

try_mud_body_expansion(G0,G2):- ((mud_goal_expansion_0(G0,G1),!,expanded_different(G0, G1),!,dbase_mod(DBASE))),prepend_module(G1,DBASE,G2).
mud_goal_expansion_0(G1,G2):- ((get_goal_wrappers(If_use_holds_db, Holds_t , Holds_f),!,Holds_t\=nil ,  mud_pred_expansion(If_use_holds_db, Holds_t - Holds_f,G1,G2))).

try_mud_head_expansion(G0,G2):- ((mud_head_expansion_0(G0,G1),!,expanded_different(G0, G1),!,dbase_mod(DBASE))),prepend_module(G1,DBASE,G2).
mud_head_expansion_0(G1,G2):- ((get_head_wrappers(If_mud_asserted, Dbase_t , Dbase_f),!,Dbase_t\=nil, mud_pred_expansion(If_mud_asserted, Dbase_t - Dbase_f,G1,G2))),!.

try_mud_asserted_expansion(G0,G2):-  must(is_compiling_sourcecode),    
  mud_asserted_expansion_0(G0,G1),!,
   expanded_different(G0, G1),
   while_capturing_changes(add_from_file(G1,G2),Changes),!,ignore((Changes\==[],dmsg(add(todo(Changes-G2))))).
mud_asserted_expansion_0(G1,G2):- ((get_asserted_wrappers(If_mud_asserted, Asserted_dbase_t , Asserted_dbase_f),!,Asserted_dbase_t\=nil,mud_pred_expansion(If_mud_asserted, Asserted_dbase_t - Asserted_dbase_f,G1,G2))),!.

:-export(force_clause_expansion/2).

attempt_clause_expansion(B,BR):- compound(B), copy_term(B,BC),snumbervars(BC),!, attempt_clause_expansion(B,BC,BR).
attempt_clause_expansion(_,BC,_):-inside_clause_expansion(BC),!,fail.
attempt_clause_expansion(B,BC,BR):- 
    setup_call_cleanup(asserta(inside_clause_expansion(BC)),
    force_clause_expansion(B,BR),
    ignore(retract(inside_clause_expansion(BC)))).

force_clause_expansion(':-'(B),':-'(BR)):- !, with_assertions(is_compiling_clause,user:expand_goal(B,BR)).
force_clause_expansion(B,BR):- with_assertions(is_compiling_clause,force_expand(force_clause_expansion0(B,BR))).

force_clause_expansion0(M:((H:-B)),R):- !, mud_rule_expansion(M:H,M:B,R),!.
force_clause_expansion0(((M:H:-B)),R):- !, mud_rule_expansion(M:H,B,R),!.
force_clause_expansion0(((H:-B)),R):- mud_rule_expansion(H,B,R),!.
force_clause_expansion0(H,HR):- try_mud_asserted_expansion(H,HR),!.
force_clause_expansion0(B,BR):- force_head_expansion(B,BR).

force_expand(Goal):-thread_self(ID),with_assertions(always_expand_on_thread(ID),Goal),!.


force_head_expansion(H,HR):- try_mud_head_expansion(H,HR),!.
force_head_expansion(H,HR):- force_expand(expand_term(H,HR)).

mud_rule_expansion(H,True,HR):-is_true(True),!,force_clause_expansion(H,HR).  
mud_rule_expansion(H,B,HB):- pttp_expansions(H,B),pttp_term_expansion((H:-B),HB).
mud_rule_expansion(H,B,((HR:-BR))):-force_head_expansion(H,HR),force_expand_goal(B,BR),!.

is_term_head(H):- (( \+ \+ (inside_clause_expansion(H0),!,H=H0))),!.
%is_term_head(_):- inside_clause_expansion(_),!,fail.
%is_term_head(H):-H=_, is_our_sources(H).


is_our_dir(LM):- user:file_search_path(logicmoo,LM0),absolute_file_name(LM0,LM).
current_loading_file_path(Path):- prolog_load_context(module,M),!,module_property(M,file(Path)).
current_loading_file_path(Dir):- prolog_load_context(directory,Dir0),!,absolute_file_name(Dir0,Dir).

is_our_sources(_):- current_loading_file_path(Dir),is_our_dir(LM),atom_concat(LM,_,Dir),!.
is_our_sources(_):- prolog_load_context(module,user),!,not(prolog_load_context(directory,_)).


holds_form(G1,HOLDS,G2):-
      functor_check_univ(G1,F,List),
      must_det(holds_form0(F,List,HOLDS,G2L)),
      univ_left(G2,G2L).

holds_form0(F,[P|List],HOLDS,G2L):-
      (is_holds_true(F) -> (correct_args_length([P|List],NEWARGS),G2L = [HOLDS|NEWARGS]) ;
      (is_holds_false(F) -> (correct_args_length([P|List],NEWARGS),negate_wrapper(HOLDS,NHOLDS),G2L = [NHOLDS|NEWARGS]) ;
      correct_args_length([F,P|List],NEWARGS), G2L= [HOLDS|NEWARGS])).

correct_args_length([F|List],[F|NewList]):-
   length(List,A),
   must_det(ensure_moo_pred(F,A,NewA,_)),!,            
      set_list_len(List,NewA,NewList).


xcall_form(G1,G2):-must_det(xcall_form0(G1,G2)).
xcall_form0(G1,G2):-
      functor_check_univ(G1,F,List),
      correct_args_length([F|List],NewList),
      univ_left(G2,NewList),!.

:- meta_predicate mud_pred_expansion(-,-,-,+).


mud_pred_expansion(_Prd,_HNH,G1,_):-not(compound(G1)),!,fail.
mud_pred_expansion(_Prd,_HNH,_:G1,_):-var(G1),!,fail.
mud_pred_expansion(_Prd,_HNH,_/_,_):-!,fail.
mud_pred_expansion(_Prd,_HNH,(_,_),_):-!,fail.
mud_pred_expansion(_Prd,_HNH,_:_/_,_):-!,fail.
mud_pred_expansion(Prd,HNH,_:M:G1,G2):- atom(M),!,mud_pred_expansion(Prd,HNH,M:G1,G2).
mud_pred_expansion(_Prd,_HNH,G1,G2):- functor_safe(G1,F,_),xcall_t==F,!,G2 = (G1),!.
mud_pred_expansion(Pred,NHOLDS - HOLDS, not(G1) ,G2):-!,mud_pred_expansion(Pred,HOLDS - NHOLDS,G1,G2).
mud_pred_expansion(Pred,NHOLDS - HOLDS, \+(G1) ,G2):-!,mud_pred_expansion(Pred,HOLDS - NHOLDS,G1,G2).

mud_pred_expansion(Pred, HNH, G0 ,G2):-
 functor_safe(G0,F,1),G1=..[F,MP],
 predicate_property(G0, meta_predicate(G1)),
 member(MP,[:,0,1,2,3,4,5,6,7,8,9]),!,
 G0=..[F,Term],
 mud_pred_expansion(Pred, HNH, Term ,Term2),
  G2=..[F,Term2],!.


mud_pred_expansion(Pred,HNH, Moo:G0,G3):- nonvar(Moo),is_kb_module(Moo),
   xcall_form(G0,G1),
   functor_safe(G1,F,A),
   ensure_moo_pred(F,A,_,_Why),
   mud_pred_expansion_0(Pred,HNH,G1,G2),!,G2=G3.

mud_pred_expansion(Pred,HNH, Moo:G1,G3):-  nonvar(Moo),!, mud_pred_expansion_0(Pred,HNH,Moo:G1,G2),!,G2=G3.
mud_pred_expansion(Pred,HNH,G1,G3):- mud_pred_expansion_0(Pred,HNH,G1,G2),!,G2=G3.

mud_pred_expansion_0(Pred,HNH,_:G1,G2):-!,compound(G1),
   mud_pred_expansion_1(Pred,HNH,G1,G2),!.
mud_pred_expansion_0(Pred,HNH,G1,G2):-!,compound(G1),
   mud_pred_expansion_1(Pred,HNH,G1,G2),!.

mud_pred_expansion_1(Pred,HNH,G1,G2):-G1=..[F|ArgList],functor_safe(G1,F,A),mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2).

mud_pred_expansion_2(_,Holds,_,HoldsT-HoldsF,_,_):-member(Holds,[HoldsT,HoldsF]),!,fail.
mud_pred_expansion_2(_,Holds,_,_,_,_):-member(Holds,[',',';']),!,fail.

mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2):-member(F,[':','.']),!,trace_or_throw(mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2)).
mud_pred_expansion_2(Pred,F,_,HNH,ArgList,G2):- is_holds_true(F),holds_form_l(Pred,ArgList,HNH,G2).
mud_pred_expansion_2(Pred,F,_,HOLDS - NHOLDS,ArgList,G2):- is_holds_false(F),holds_form_l(Pred,ArgList,NHOLDS - HOLDS,G2).
% mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2):-is_2nd_order_holds(F),!,trace_or_throw(mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2)).
mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2):- call(Pred,F,A,_,_),holds_form_l(Pred,[F|ArgList],HNH,G2).

holds_form_l(Pred,[G1],HNH,G2):-
   compound(G1),not(is_list(G1)),!,
   mud_pred_expansion(Pred,HNH,G1,G2).

holds_form_l(_,G1,HNH,G2):-do_holds_form(G1,HNH,G2).

do_holds_form([F|List],HOLDS - _NHOLDS,G2):-
   atom(F),
   G1=..[F|List],
   holds_form(G1,HOLDS,G2).

do_holds_form([F|List],HOLDS - _NHOLDS,G2):- G2=..[HOLDS,F|List].



differnt_assert(G1,G2):- notrace(differnt_assert1(G1,G2)),dmsg(differnt_assert(G1,G2)),dtrace.

differnt_assert1(M:G1,G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,M:G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G1,M1)),G1\=M1,!, differnt_assert1(M1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G2,M2)),G2\=M2,!, differnt_assert1(G1,M2).
differnt_assert1(G1,G2):- not((G1 =@= G2)).




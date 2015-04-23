/** <module> logicmoo_i_pfc
% Provides a prolog database replacent that uses PFC
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure
%
*/

:-ensure_loaded(library(logicmoo/mpred/pfc)).

:-pfc_untrace.

end_of_file.

:- include(logicmoo_i_header).

pmsg(S):-wdmsg(pfc(S)).
pmsg(S,Args):- sformat(SF,S,Args),pmsg(SF).

% :- asserta(tlbugger:no_colors).

/*
:-meta_predicate(if_defined(0)).
if_defined(Call):-current_predicate(_,Call),!,Call.
:-meta_predicate(if_defined(0,0)).
if_defined(Call,Else):-current_predicate(_,Call)->Call;Else.
*/
:-meta_predicate(pfc_is_asserted(?)).
pfc_is_asserted(U):- if_defined(is_asserted_1(U),clause_asserted(U)).
:-meta_predicate(pfc_system_call(0)).
pfc_system_call(C):- if_defined(mpred_call(C),if_defined(C)).
% is_true(G):-G==true.

:- op(500,fx,'~').
:- op(1075,xfx,('=>')).
:- op(1075,xfx,'<=>').
:- op(1075,xfx,('<=')).
:- op(1100,fx,('=>')).
:- op(1150,xfx,('::::')).
:- op(1200,xfx,'-->>').
:- op(1200,xfx,'--*>>').
% :- op(1200,xfx,'<<--').
:- op(400,yfx,'\\\\').

:-thread_local pfc_slow_search/0.
:-thread_local thlocal:no_side_effects/0.

:- dynamic(neg/1).
:- multifile(neg/1).
:- meta_predicate(neg(1)).

:- op(1200,fx,(user:disabled)).
:- op(1200,fx,(user:enabled)).
:- user:op(1199,fx,(:-)).
:- user:op(1199,xfx,(:-)).

% convert prolog neck per predicate
:-dynamic(pfc_settings/3).

pfc_settings(neck,default,(:-)).
pfc_settings(neck,forward,(=>)).
pfc_settings(neck,backward,(<=)).
pfc_settings(neck,prolog,(:-)).
pfc_settings(neck,member/2,(prolog)).
pfc_settings(neck,rule,default).
pfc_settings(neck,fact,default).

pfc_settings(add,(:-)/1,pfc_system_call).
pfc_settings(add,(=>)/2,pfc).
pfc_settings(add,(<=)/2,pfc).
pfc_settings(add,(:-)/2,rule).
pfc_settings(add,default,prolog).
pfc_settings(add,forward,pfc).
pfc_settings(add,backward,pfc).
pfc_settings(add,pfc,pfc_add).
pfc_settings(add,member/2,prolog).
%pfc_settings(add,prolog,assertz).
pfc_settings(add,rule,default).
pfc_settings(add,fact,default).


pfc_transform_neck(HB,O):-pfc_get_setting(neck,HB,Type),pfc_transform_neck(HB,Type,O).

pfc_transform_neck(HB,Pred,HB):- atom(Pred),HB=..[Pred|_],!.
pfc_transform_neck((H:-B),(:-),(H:-B)).
pfc_transform_neck((H:-B),(=>),(B=>H)).
pfc_transform_neck((H:-B),(<=),(H<=B)).
pfc_transform_neck((HB),(<=),(HB<=true)).
pfc_transform_neck(HB,TYPE,O):-pfc_settings(neck,TYPE,Type),loop_check(pfc_transform_neck(HB,Type,O)).
pfc_transform_neck((H:-B),Pred,O):- atom(Pred),!,O=..[Pred,H,B].
pfc_transform_neck(HB,Pred,O):- atom(Pred),!,O=..[Pred,HB].
pfc_transform_neck(HB,_,HB).


% H = parentOf(_,_), pfc_clause(H,B,R).

pfc_get_neck(H,FB):-pfc_get_neck_fb(H,FB)*->true;pfc_get_setting_pred(neck,H,FB).
pfc_get_neck_fb(H,(<=)):- once(clause(H,callBC(H));clause(H,infoF(H <= _));clause(H <= _,true)).
pfc_get_neck_fb(H,(=>)):- once(clause(H,infoF(_ => H));clause(_ => H,true)).
pfc_get_neck_fb(H,(:-)):- once(clause(H,infoF(H:-_));(clause(H,B),\+is_true(B),\+is_meta_info(B))).

pfc_is_neck_other(H,Type):-pfc_get_neck(H,T),T\=Type,!.

pfc_set_neck(H,Type):- pfc_make_fa(H,F,A), \+ current_predicate(F/A), pfc_setting_change(neck,H,Type).
pfc_set_neck(H,Type):- \+ pfc_is_neck_other(H,Type), !, pfc_setting_change(neck,H,Type).
pfc_set_neck(H,Type):- 
  findall((H:-B),(pfc_clause(H,B,_),B\==fail),ToAssert),
  forall(member((H:-B),ToAssert),pfc_rem2(H)),
  pfc_setting_change(neck,H,Type),
  forall(member(HB,ToAssert),pfc_compile(HB)).

  
%pfc_compile(HB):-pfc_transform_neck(HB,AS),pfc_get_setting(add,HB,How), How==prolog->assertz(How);call(How,AS).
pfc_compile(HB):-pfc_file_expansion(HB,AS),pfc_compile(HB,AS).
pfc_compile(_HB,(:-Call)):-!,call(Call).
pfc_compile(_HB,(H:-B)):-!,assertz((H:-B)).
pfc_compile(_HB,(HB)):-!,assertz((HB)).


pfcRemAll(H):-must((findall(pfcRem13(H,Why),pfc_clause(H,_,Why),L),
  ignore(forall(member(pfcRem13(H,Why),L),ignore(pfcRem13(H,Why)))),
  ignore(pfcRem13(H)))).


pfcRem13(H,asserted(R,CL)):-pfcRem13(H),ignore((clause_property(R,_),erase(R))),!.
pfcRem13(H,deduced(R,CL)):-pfcRem13(H),ignore((clause_property(R,_),erase(R))),!.
pfcRem13(H):-    
  doall((CL=(_=>HH),support_shape(CL),sub_term_v(H,HH),pfcRem123(HH,CL))),
  doall((CL=(_=>H),support_shape(CL),pfcRem123(H,CL))),
  doall((CL=(HH<=_),support_shape(CL),sub_term_v(H,HH),pfcRem123(HH,CL))),
  doall((CL=(HH:-_),support_shape(CL),sub_term_v(H,HH),pfcRem123(HH,CL))),
  doall((CL=(_<=>_),support_shape(CL),sub_term_v(H,CL),pfcRem123a(CL))),
  doall((CL=(pfcPT(_,_)),support_shape(CL),sub_term_v(rhs([H]),CL),pfcRem123a(CL))),
  doall((CL=(pfcBT(_,_)),support_shape(CL),sub_term_v(rhs([H]),CL),pfcRem123a(CL))),
  doall((CL=(pfcNT(_,_)),support_shape(CL),sub_term_v(rhs([H]),CL),pfcRem123a(CL))),
  doall((CL=(spft(_,_,_)),support_shape(CL),sub_term_v(rhs([H]),CL),pfcRem123a(CL))),
  doall((CL=(support2(_,_,_)),support_shape(CL),sub_term_v(rhs([H]),CL),pfcRem123a(CL))),
  doall((CL=(support3(_,_,_)),support_shape(CL),sub_term_v(rhs([H]),CL),pfcRem123a(CL))),
  doall((CL=(H),support_shape(CL),pfcRem123a(CL))),
  doall((clause(H,_,Ref),pfcRem123a(H),erase(Ref))),!.

support_shape(CL):-clause(CL,true).
support_shape(CL):-spft(CL,_,_).
support_shape(CL):-support2(_,CL,_).
support_shape(CL):-support3(_,_,CL).

pfcRem123(HH,CL):- pfcRem123a(HH),pfcRem123a(CL).
pfcRem123a(HH):- doall(pfcRem1(HH,_)),doall(pfcRem3(HH)).


make_functor(PO,M:F,A):-must(ground(F/A)),!,functor(P,F,A),(P=PO;P=M:PO).
make_functor(PO,F,A):-must(ground(F/A)),!,functor(P,F,A),(P=PO;P=_:PO).

pfc_mpred_prop(F,T):-clause(user:mpred_prop(F,T),true).

pfc_setting_change(Prop,Pred,Type):-ignore(retractall(pfc_settings(Prop,Pred,_))),asserta(pfc_settings(Prop,Pred,Type)).


reduce_hb((C:- B),HB):- is_true(B),!,reduce_hb(C,HB).
reduce_hb((C<= B),HB):- is_true(B),!,reduce_hb(C,HB).
reduce_hb((B=> C),HB):- is_true(B),!,reduce_hb(C,HB).
reduce_hb(C,C).

pfc_get_setting(Prop,H,Type):-transitive(pfc_get_setting_pred(Prop),H,Type),H\=@=Type,!.
pfc_get_setting(Prop,HB,Type):-reduce_hb(HB,RC),pfc_get_setting_fr(Prop,RC,Type),Type\==rule,Type\==fact,Type\==default,!.
pfc_get_setting(Prop,_,Type):-transitive(pfc_get_setting_pred(Prop),default,Type),Type\==default,!.



pfc_get_setting_fr(Prop,H,Type):-functor(H,F,A),(pfc_settings(Prop,F/A,CT);pfc_settings(Prop,F,CT)),
   transitive(pfc_get_setting_pred(Prop),CT,Type),Type\==rule,Type\==fact,Type\==default.
pfc_get_setting_fr(Prop,(_:-_),Type):-!,transitive(pfc_get_setting_pred(Prop),rule,Type).
pfc_get_setting_fr(Prop, _ ,Type):- transitive(pfc_get_setting_pred(Prop),fact,Type).

pfc_get_setting_pred(Prop,H,Type):-pfc_settings(Prop,H,Type).
pfc_get_setting_pred(Prop,H,Type):-pfc_make_fa(H,F,A),reduce_fa(F,A,R),pfc_settings(Prop,R,Type).

reduce_fa(F,A,H):-A>0,make_functor(H,F,A).
reduce_fa(F,A,F/A).
reduce_fa(F,_,F).

pfc_make_fa(default,default,0):-!.
pfc_make_fa(rule,rule,0):-!.
pfc_make_fa(fact,fact,0):-!.
pfc_make_fa(prolog,prolog,0):-!.
pfc_make_fa(direct,direct,0):-!.
pfc_make_fa((:-),(:-),0):-!.
pfc_make_fa(F,F,A):-atom(F),guess_arity(F,A).
pfc_make_fa(F/A,F,A):-atom(F),ground(F/A),!.
pfc_make_fa(F/A,F,A):-atom(F),!,guess_arity(F,A).
pfc_make_fa(PI,F,A):-get_functor(PI,F,A).

pfc_fully_expand(Type,B,A):-if_defined(fully_expand(Type,B,A)).
pfc_fully_expand(_Type,B,A):-loop_check(expand_goal(B,A)),!.
% maybe comment out
% pfc_fully_expand_warn(_,B,A):- 0 is random(2),!,B=A.
pfc_fully_expand_warn(Type,B,A):-pfc_fully_expand(Type,B,A),(B\=@=A->wdmsg(error(pfc_fully_expand_warn(Type,B,A)));true),!.

% :- ignore((current_predicate(not/1),abolish(system:not/1),abolish(not/1),dynamic(not/1),assert(((not(P):- nonvar(P), \+ P))),meta_predicate(not(0)))).
:- op(0,fx,'decl_mpred_pfc').
user:isa(pfcMetaPred,tCol).
user:isa(pfcMustFC,tCol).
decl_mpred_pfc(F/A):-!,export(F/A),dynamic(F/A),asserta_if_new(user:mpred_prop(F,prologOnly)),asserta_if_new(arity(F,A)),asserta_if_new(user:mpred_prop(F,pfcMetaPred)),
  asserta_if_new(user:mpred_prop(F,arity(A))).
decl_mpred_pfc(F):-atom(F),!,decl_mpred_pfc(F/0).
:- op(1150,fx,'decl_mpred_pfc').

has_numvars(P):-term_variables(P,PV),unnumbervars(P,UP),term_variables(UP,UPV),!,UPV\==[],PV\=@=UPV.

must_numvars(P):- must(ground(P);has_numvars(P)),!.
must_no_numvars(P):-must(\+(has_numvars(P))),!.

:-meta_predicate(loop_check_nr(0)).
loop_check_nr(G):-no_repeats(loop_check(G)).

user:decl_database_hook(Op,Hook):- loop_check_nr(pfc_provide_mpred_storage_op(Op,Hook)).

is_retract_first(one).
is_retract_first(a).

pfc_provide_mpred_storage_op(Op,(I1,I2)):-!,pfc_provide_mpred_storage_op(Op,I1),pfc_provide_mpred_storage_op(Op,I2).
pfc_provide_mpred_storage_op(Op,(=>(P))):-!,pfc_provide_mpred_storage_op(Op,P).
%pfc_provide_mpred_storage_op(change(assert,_AorZ),Fact):- loop_check_nr(pfc_addPreTermExpansion(Fact)).
% pfcRem1 to just get the first
pfc_provide_mpred_storage_op(change(retract,OneOrA),FactOrRule):- is_retract_first(OneOrA),!,loop_check_nr(pfcRem1(FactOrRule)),ignore((ground(FactOrRule),pfc_rem2(FactOrRule))). 
% pfc_rem2 should be forcefull enough
pfc_provide_mpred_storage_op(change(retract,all),FactOrRule):- loop_check_nr(pfc_rem2(FactOrRule)),!.
% pfc_provide_mpred_storage_op(is_asserted,FactOrRule):- nonvar(FactOrRule),!,loop_check_nr(pfcClauseInt(FactOrRule)).

pfc_clause_is_asserted(H,B):- var(H),nonvar(B),!,fail.
pfc_clause_is_asserted(H,B):- one_must(pfc_clause_db_unify(H,B),pfc_clause_is_asserted_hb_nonunify(H,B)).
pfc_clause_is_asserted(H,B,Ref):-pfc_clause_db_ref(H,B,Ref).

pfc_clause_is_asserted_hb_nonunify(H,B):- pfc_clause_db_unify( =>( B , H) , true).
pfc_clause_is_asserted_hb_nonunify(H,B):- pfc_clause_db_unify( <=( H , B) , true).
pfc_clause_is_asserted_hb_nonunify(_,_):-!,fail.
pfc_clause_is_asserted_hb_nonunify(G, T   ):- T==true,!,notrace(pfcRuleOutcomeHeadBody(G,H,B)),G\=@=H,!,pfc_clause_is_asserted(H,B).
pfc_clause_is_asserted_hb_nonunify(H,(T,B)):- T==true,!,pfc_clause_is_asserted_hb_nonunify(H,B).
pfc_clause_is_asserted_hb_nonunify(H,(B,T)):- T==true,!,pfc_clause_is_asserted_hb_nonunify(H,B).
pfc_clause_is_asserted_hb_nonunify(H,B):- pfc_clause_db_unify( <=( H , B) , true).
pfc_clause_is_asserted_hb_nonunify(H,B):-pfc_clause(H,B,_).


pfcDatabaseGoal(G):-compound(G),get_functor(G,F,A),pfcDatabaseTerm(F/A).

user:provide_mpred_storage_clauses(Type,H,B,Proof):-pfc_clause(Type,H,B,Proof).

%pfc_clause('=>'(H),B,forward(Proof)):- nonvar(H),!, user:provide_mpred_storage_clauses(H,B,Proof).
%pfc_clause(H,B,forward(R)):- R=(=>(B,H)),clause(R,true).
pfc_clause(H,B,Why):-predicate_property(H,number_of_clauses(_)),clause(H,CL,R),pfc_pbody(H,CL,R,B,Why).
% pfc_clause(H,B,backward(R)):- R=(<=(H,B)),clause(R,true).
% pfc_clause(H,B,equiv(R)):- R=(<=>(LS,RS)),clause(R,true),(((LS=H,RS=B));((LS=B,RS=H))).
% pfc_clause(H,true, pfcTypeFull(R,Type)):-nonvar(H),!,pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcRuleOutcomeHead(R,H),clause(R,true),pfcTypeFull(R,Type),Type\=rule.
% pfc_clause(H,true, pfcTypeFull(R)):-pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcTypeFull(R,Type),Type\=rule,clause(R,true),once(pfcRuleOutcomeHead(R,H)).

pfc_pbody(H,callBC(BC),R,fail,deduced(backchains)):-!.
pfc_pbody(H,infoF(INFO),R,B,Why):-!,pfc_pbody_f(H,INFO,R,B,Why).
pfc_pbody(H,B,R,BIn,WHY):- is_true(B),!,BIn=B,get_why(H,H,R,WHY).
pfc_pbody(H,B,R,B,asserted(R,(H:-B))).

pfc_pbody_f(H,CL,R,B,WHY):- CL=(B=>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
pfc_pbody_f(H,CL,R,B,WHY):- CL=(HH<=B),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
pfc_pbody_f(H,CL,R,B,WHY):- CL=(HH<=>B),sub_term_eq(H,HH),get_why(H,CL,R,WHY).
pfc_pbody_f(H,CL,R,B,WHY):- CL=(B<=>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
pfc_pbody_f(H,CL,R,fail,infoF(CL)):- trace_or_throw(pfc_pbody_f(H,CL,R,B,WHY)).

sub_term_eq(H,HH):-H==HH,!.
sub_term_eq(H,HH):-each_subterm(HH,ST),ST==H,!.
sub_term_v(H,HH):-H=@=HH,!.
sub_term_v(H,HH):-each_subterm(HH,ST),ST=@=H,!.

get_why(H,CL,R,asserted(R,CL)):- clause(spft(CL, u, u),true),!.
get_why(H,CL,R,deduced(R,WHY)):-pfcGetSupport1(H,WH)*->WHY=(H=WH);(pfcGetSupport1(CL,WH),WHY=(CL=WH)).


:-dynamic(pfcExpectedClauseCount_db/3).
pfcGetExpectedClauseCount(F,A,C):- (pfcExpectedClauseCount_db(F,A,C);C=0),!.
pfcGetActualClauseCount(F,A,C):-make_functor(P,F,A),predicate_property(P,number_of_clauses(C)).
pfcIsClauseCountWrong(F,A):-pfcGetExpectedClauseCount(F,A,E),pfcGetActualClauseCount(F,A,C),!,C\=E.
pfcCountsClauses(F,A):- arity(F,A),pfcWatches(F/A).

pfcCheckClauseCounts :- forall(pfcExpectedClauseCount_db(F,A,_),pfcUpdateClauses(F,A)).
% pfcCheckClauseCounts :- forall(pfcCountsClauses(F,A),pfcUpdateClauses(F,A)).

pfcUpdateClauses(F,A):-not(pfcIsClauseCountWrong(F,A)),!.
pfcUpdateClauses(F,A):-make_functor(P,F,A),forall((clause(P,T),is_true(T)),hooked_asserta(P)),retractall(pfcExpectedClauseCount_db(F,A,_)),
   predicate_property(P,number_of_clauses(C)),
   asserta(pfcExpectedClauseCount_db(F,A,C)).

:-if(current_predicate(onEachLoad/1)).
:-onEachLoad(pfcCheckClauseCounts).
:-endif.

% :-asserta(thlocal:pfcExpansion).

:- thread_local thlocal:pfcExpansion/0.
:- dynamic thlocal:pfcExpansionWas.

maybe_hybrid(F/_):-pfc_mpred_prop(F,prologOnly),!.
maybe_hybrid(F/_):-pfc_mpred_prop(F,prologHybrid),!.
maybe_hybrid(F/_):-pfc_mpred_prop(F,pfcMetaPred),!.
maybe_hybrid(F/_):-pfc_mpred_prop(F,X),atom(X),!.
maybe_hybrid(F/A):-atom(F),debugOnError(current_predicate(F/A)),!.
% maybe_hybrid(C/1):-ignore((nonvar(C)->decl_mpred_hybrid(C/1);ignore(decl_mpred_hybrid(isa/2))))
maybe_hybrid(F/A):- current_predicate((decl_mpred_hybrid)/1), ignore(must((atom(F),decl_mpred_hybrid(F/A)))).
% maybe_hybrid(_/A):-A=1,!.
maybe_hybrid(F/A):-dynamic(F/A).

pfc_maptree(Pred,List):-pfc_maptree(Pred,List,[]).

pfc_lambda([A1],Body,A1):-Body.
pfc_lambda([A1,A2],Body,A1,A2):-Body.
pfc_lambda([A1,A2,A3],Body,A1,A2,A3):-Body.
pfc_lambda([A1,A2,A3,A4],Body,A1,A2,A3,A4):-Body.

% :-call(pfc_lambda([E],writeln(E)),hello_lambda).

:-dynamic(pfcMetaPred/1).
:-dynamic(pfcControlled/1).
:-dynamic(pfcWatched/1).
:-dynamic(pfcMustFC/1).

pfcPreds:-
  listing(pfcMetaPred/1),
  listing(pfcControlled/1),
  listing(pfcWatched/1),
  listing(pfcMustFC/1),
  listing(pfcPreferBC/1).


pfc_maptree(_,[],_) :- !.
pfc_maptree(Pred,H,S):-var(H),!,apply(Pred,[H|S]).
pfc_maptree(Pred,(H,T),S):-!, apply(Pred,[H|S]), pfc_maptree(Pred,T,S).
pfc_maptree(Pred,[H|T],S):-!, apply(Pred,[H|S]), pfc_maptree(Pred,T,S).
pfc_maptree(Pred,H,S):-apply(Pred,[H|S]).

deny_pfc_Permission_to_remove(pfcInternal,_,_):-!,fail. %allow
deny_pfc_Permission_to_remove(_,P,not(pfcControlled)):-get_functor(P,F,A), \+(pfc_local(P,F,A);pfc_mpred_prop(F,pfcControlled)).

pfc_pre_expansion_each(X,X):- \+ compound(X),!.
pfc_pre_expansion_each(X,X):-if_defined(as_is_term(X)),!.
pfc_pre_expansion_each(X,isa(I,C)):- if_defined(was_isa(X,I,C)),!,( \+ \+ maybe_hybrid(C/1)).
pfc_pre_expansion_each(Sent,OUT):-Sent=..[And|C12],if_defined(is_logical_functor(And)),!,maplist(pfc_pre_expansion_each,C12,O12),OUT=..[And|O12],!.
pfc_pre_expansion_each(C12,OUT):-is_list(C12),!,maplist(pfc_pre_expansion_each,C12,OUT),!.
pfc_pre_expansion_each(X,X):-!.
pfc_pre_expansion_each(X,X):- \+ \+ ((get_functor(X,F,A),must(maybe_hybrid(F/A);pfc_local(X)))),!.

% {G}:-pfc_system_call(G).
user:arity(F,A):-pfcDatabaseTerm(F/A).
user:mpred_prop(F,argIsa(_,ftAskable)):-pfcDatabaseTerm(F/_).

user:mpred_prop(isa,2,pfcMustFC).

pfcMustFC(H):-get_functor(H,F),pfc_mpred_prop(F,pfcMustFC).
pfcPreferBC(H):-get_functor(H,F,A),pfc_mpred_prop(F,pfcPreferBC),dynamic(F/A),functor(PHead,F,A),assertz_if_new(((PHead:-callBC(PHead)))).

:-asserta_if_new(thglobal:pfcManageHybrids).
pfc_manage_hybrids:-!.
pfc_manage_hybrids :- if_defined(thglobal:pfcManageHybrids),!.

:- decl_mpred_pfc pfc_local/1.
pfc_local(G):-get_functor(G,F,A),pfc_local(G,F,A).

pfc_local(_,F,A):-pfcDatabaseTerm(F/A),!.
pfc_local(_,F,_):-pfc_mpred_prop(F,pfcMetaPred),!.
pfc_local(_,F,_):-pfc_mpred_prop(F,prologOnly).
pfc_local(G,_,_):-pfc_manage_hybrids,!,pfc_mark_C(G),!.
pfc_local(G,_,_):-pfc_manage_hybrids,!,pfc_mark_W(G),!.
pfc_local(G,_,_):- not(current_predicate(hooked_assertz/1)),!,pfc_mark_C(G).
pfc_local(_,F,_):-pfc_mpred_prop(F,prologHybrid),!,fail.
pfc_local(_,_,_).

% pfc_local(_).

pfcControlled(G):-notrace(pfcControlled0(G)).
pfcWatches(G):-notrace(pfcWatched0(G)).

pfcControlled0(Var):-is_ftVar(Var),!.
pfcControlled0((_:F)/A):-atom(M),!,pfcControlled0(F/A).
pfcControlled0(G):- get_functor(G,F), (pfc_mpred_prop(F,pfcControlled);pfc_mpred_prop(F,pfcMustFC)).

pfcWatched0(Var):-is_ftVar(Var),!.
pfcWatched0((M:F)/A):-atom(M),!,pfcWatched0(F/A).
pfcWatched0(G):- get_functor(G,F), (pfc_mpred_prop(F,pfcWatched);pfc_mpred_prop(F,pfcControlled);pfc_mpred_prop(F,pfcMustFC)).

:-thread_local(thlocal:pfc_no_mark/0).



pfc_mark_W(G):-pfc_mark_As(G,pfcWatched).
pfc_mark_C(G):-pfc_mark_As(G,pfcControlled).
pfc_mark_F(G):-pfc_mark_As(G,pfcMustFC).
pfc_mark_B(_):-!.
pfc_mark_B(G):-pfc_mark_As(G,pfcPreferBC).

pfc_mark_As(_, _):-thlocal:pfc_no_mark,!.
pfc_mark_As(G,_):-is_true(G),!.
pfc_mark_As(G,As):-must(pfc_maptree(pfc_mark_As1,G,[As])),!.

pfc_mark_As1(F,As):-atom(F),clause(pfc_mpred_prop(F,prologOnly),true),!,pmsg(todo(warn(wont_pfc_mark_As1(F,As)))).
pfc_mark_As1(F,As):-atom(F),!,assert_if_new(user:mpred_prop(F,As)).
pfc_mark_As1(G,_):- \+(compound(G)),!.
pfc_mark_As1(u,_):-!.
pfc_mark_As1(pfcGod,_):-!.
pfc_mark_As1(u,_):-!.
pfc_mark_As1(pfcGod,_):-!.
pfc_mark_As1((G1,G2),As):-!,pfc_mark_As1(G1,As),pfc_mark_As1(G2,As).
pfc_mark_As1(forall(G1,G2),As):-!,pfc_mark_As1(G1,As),pfc_mark_As1(G2,As).
pfc_mark_As1(G,As):-pfcDatabaseGoal(G),must((forall(pfcRuleOutcomeHeadBody(G,H,B),must((pfc_mark_As(H,As),pfc_mark_As(B,pfcWatched)))))),!.
pfc_mark_As1(G,As):-predicate_property(G,meta_predicate(_)),G=..[_|LIST],!,pfc_mark_As(LIST,As).
pfc_mark_As1(G,As):-get_functor(G,F,A),pfc_mark_As2(F,A,As).

pfc_mark_As2(F,A,As):-As==pfcControlled,dynamic_safe(F/A),fail.
pfc_mark_As2(F,A,As):-As==pfcMustFC,dynamic_safe(F/A),fail.
pfc_mark_As2(F,A,As):-assert_if_new(user:arity(F,A)),must(pfc_mark_As1(F,As)).


% by returning true we veto the assertion  (fail accepts assertion)
throw_on_bad_fact(G):-why_throw_on_bad_fact(G,Why), pmsg(((throw_on_bad_fact(Why,G)))),!,fail.

why_throw_on_bad_fact(G,singletons(HS)):-   
  head_singletons_g(G,HS),get_functor(HS,F),!,not(pfc_mpred_prop(F,predCanHaveSingletons)).

head_singletons_g(G,HS):- pfcRuleOutcomeHeadBody(G,H,B), head_singletons_hb(H,B,HS),!.
  
head_singletons_hb(HN,BN,H):- unnumbervars(HN:BN,H:B),
  term_variables(H,HV),
  numbervars((H:B),66,_,[singletons(true)]),!,
    member('$VAR'('_'),HV).

pfc_settings_retractall(P,S,O):-retractall(pfc_settings(P,S,O)).


pfc_retract(Why,P) :- deny_pfc_Permission_to_remove(Why,P,Because),!, pfc_warn("Denying ~w retract access to ~w because ~w",[Why,P,Because]),!.
pfc_retract(_,pfcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).
pfc_retract(_,pfcPT3(Key,Head,Body)) :-  
  % undo a positive trigger.
  %
  !,
  (retract(pfcPT3(Key,Head,Body))
    -> unFc(pfcPT(Head,Body))
     ; pfc_warn("Trigger not found to pfc_retract: ~w",[pfcPT(Head,Body)])).
pfc_retract(pfcInternal(_),G):- must(pfc_local(G)),!,retract(G).
pfc_retract(pfcInternal,G):- must(pfc_local(G)),!,retract(G).
pfc_retract(_,G):- pfc_local(G),!,retract(G),loop_check(if_defined(run_database_hooks_depth_1(change(retract,a),G),true),true).
pfc_retract(_,G):- hooked_retract(G).


pfc_ignored(argIsa(F, A, argIsaFn(F, A))).
pfc_ignored(genls(A,A)).
pfc_ignored(isa(tCol,tCol)).
pfc_ignored(isa(W,tCol)):-if_defined(user:hasInstance_dyn(tCol,W)).
pfc_ignored(isa(W,_)):-compound(W),isa(W,pred_argtypes).
 
pfc_ignored(isa(_,Atom)):-atom(Atom),atom_concat(ft,_,Atom),!.
pfc_ignored(isa(_,argIsaFn(_, _))).

pfc_assert(G):- pfc_mpred_transform(G,GG),must(pfc_assert0(GG)),!.

pfc_assert0(G):- \+(must(\+(pfc_ignored(G)))).
pfc_assert0(G):- must(\+(throw_on_bad_fact(G))),fail.
pfc_assert0(G):- must((pfc_manage_hybrids)),!,must((pfc_local(G),!,assertz_if_new(G),add_meta_facts(assertz_if_new,G))).
pfc_assert0(G):- pfc_local(G),!,
  must(( ( \+ predicate_property(G,dynamic) -> must(G) ; (assertz_if_new(G),if_defined(hooked_assertz(G),true))),
   add_meta_facts(assertz,G))).

pfc_assert0(G):- trace,add(G),pfc_mark_C(G).

user:mpred_prop(F,prologOnly):-user:mpred_prop(F,pfcMetaPred).

add_meta_facts(G):-add_meta_facts(assertz_if_new,G).
add_meta_facts(How,(H:-True)):-is_true(True),must(nonvar(H)),!,add_meta_facts(How,H).
add_meta_facts(How,(H<=B)):- !,add_meta_facts(How,(H:-infoF(H<=B))),!,add_meta_facts(How,(H:-callBC(H))).
add_meta_facts(How,(B=>H)):- !,add_meta_facts(How,(H:-infoF(B=>H))),!.
add_meta_facts(How,(B<=>H)):- !,add_meta_facts(How,(H:-infoF(B<=>H))),!,add_meta_facts(How,(B:-infoF(B<=>H))),!.
add_meta_facts(How,((A,B):-INFOC)):-is_meta_info(INFOC),(nonvar(A);nonvar(B)),!,add_meta_facts(How,((A):-INFOC)),add_meta_facts(How,((B):-INFOC)),!.
add_meta_facts(How,((A;B):-INFOC)):-is_meta_info(INFOC),(nonvar(A);nonvar(B)),!,add_meta_facts(How,((A):-INFOC)),add_meta_facts(How,((B):-INFOC)),!.
add_meta_facts(How,(~(A):-infoF(C))):-nonvar(C),nonvar(A),!,add_meta_facts(How,((A):-infoF(~(C)))). % call(How,(~(A):-infoF(C))).
add_meta_facts(How,(A:-INFOC)):-is_meta_info(INFOC),!,rewrap_h(A,AA),call(How,(AA:-INFOC)),!.
add_meta_facts(How,pfcBT(H,_)):-!,add_meta_facts(How,(H:-callBC(H))).
%add_meta_facts(How,G):-dmsg(skipped_add_meta_facts(How,G)).
add_meta_facts(_,_).


is_meta_info(callBC(C)):-nonvar(C),!.
is_meta_info(infoF(C)):-nonvar(C),!.

is_static_pred(A):-current_predicate(_,A), \+ predicate_property(A,dynamic).
:-dynamic(not_not/1).
rewrap_h(A,A):-nonvar(A),\+ is_static_pred(A).
rewrap_h(A,F):- functor(A,F,_),\+ is_static_pred(F),!.
rewrap_h(A,not_not(A)):-!.

pfc_mpred_transform(G,GGG):-must((pfc_fully_expand_warn(pfc_mpred_transform,G,GG))),!,unnumbervars(GG,GGG).


pfc_clause_db_unify(H,B):- must(pfc_local(H)),
   (current_predicate(_,H) -> (predicate_property(H,number_of_clauses(_)) -> clause(H,B) ; B = call(H)); % simulates a body for system predicates
                                             B = pfc_system_call(H)).
pfc_clause_db_check(H,B):- copy_term(H:B,HH:BB), clause(HH,BB,Ref),clause(CH,CB,Ref),H:B=@=CH:CB,!.
pfc_clause_db_ref(H,B,Ref):-must(pfc_local(H)),!,pfc_clause_local_db_ref(H,B,Ref).

pfc_clause_local_db_ref(H,B,Ref):- copy_term(H:B,HH:BB),clause(HH,BB,Ref),clause(CH,CB,Ref),H:B=@=CH:CB,!.

% pfc_call_prolog_native(G):- pfc_call_prolog_native(nonPFC,G).
pfc_call_prolog_native(_,true):-!.
pfc_call_prolog_native(_,G):- pfc_local(G),!,show_call_failure(predicate_property(G,_)),!, debugOnError(call(G)).
pfc_call_prolog_native(Why,X):-mpred_op(call(Why),X).

:-thread_local ntd_max_depth/2.

not_too_deep(_,G):- skipWrapper,!,G.
not_too_deep(Key,G):-stack_depth(CD),
  (ntd_max_depth(Key,MD)->
      ( (CD > MD) -> (!,fail) ; G) ; 
    (MD is CD+200,call_cleanup(asserta(ntd_max_depth(Key,MD),REF),G,erase(REF)))).

% :- set_prolog_flag(unknown,fail).
:- decl_mpred_pfc(go/0).

pfcRuleOutcomeHead(Outcome,OutcomeO):-var(Outcome),!,OutcomeO=Outcome.
pfcRuleOutcomeHead((Outcome1,Outcome2),OutcomeO):-!,pfcRuleOutcomeHead(Outcome1,Outcome1O),pfcRuleOutcomeHead(Outcome2,Outcome2O),pfcConjoin(Outcome1O,Outcome2O,OutcomeO).
pfcRuleOutcomeHead(_=>Outcome,OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(Outcome<=_,OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(Outcome<=>_,OutcomeO):-pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(_<=>Outcome,OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(_::::Outcome,OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcBT(Outcome,_),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcNT(_,_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcPT(_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfcPT3(_,_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(spft(Outcome,_,_),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(support3(_,_,Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(support2(_,Outcome,_),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(pfc_queue(Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
% pfcRuleOutcomeHead(pfc Default(Outcome),OutcomeO):-!,pfcRuleOutcomeHead(Outcome,OutcomeO).
pfcRuleOutcomeHead(Outcome:-_,Outcome):-!.
pfcRuleOutcomeHead(Outcome,Outcome).


pfcRuleOutcomeHeadBody(Outcome,OutcomeO,AnteO):-pfcRuleOutcomeHeadBody_0(Outcome,OutcomeO,Ante),pfcRuleOutcomeHead(Ante,AnteO).

pfcRuleOutcomeHeadBody_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
pfcRuleOutcomeHeadBody_0((Outcome1,Outcome2),OutcomeO,AnteO):-!,pfcRuleOutcomeHeadBody(Outcome1,Outcome1O,Ante1),pfcRuleOutcomeHeadBody(Outcome2,Outcome2O,Ante2),
                   pfcConjoin(Outcome1O,Outcome2O,OutcomeO),
                   pfcConjoin(Ante1,Ante2,AnteO).
pfcRuleOutcomeHeadBody_0(Ante1=>Outcome,OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(Outcome<=Ante1,OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(Outcome<=>Ante1,OutcomeO,(Ante1,Ante2)):-pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(Ante1<=>Outcome,OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(_::::Outcome,OutcomeO,Ante2):-!,pfcRuleOutcomeHeadBody_0(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(pfcBT(Outcome,Ante1),OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(pfcPT(Ante1,Outcome),OutcomeO,(Ante1,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(pfcPT3(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(pfcNT(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(spft(Outcome,Ante1a,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(support3(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(support2(Ante1a,Outcome,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0(pfc_queue(Outcome),OutcomeO,Ante2):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
% pfcRuleOutcomeHeadBody_0(pfc Default(Outcome),OutcomeO,Ante2):-!,pfcRuleOutcomeHeadBody(Outcome,OutcomeO,Ante2).
pfcRuleOutcomeHeadBody_0((Outcome:-Ante),Outcome,Ante):-!.
pfcRuleOutcomeHeadBody_0(Outcome,Outcome,true).


pfcVersion(1.2).

% pfcFile('pfcsyntax').	% operator declarations.

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.


pfcPreferedDir(H,B,(B=>H)):-pfcMustFC(H).
pfcPreferedDir(H,B,(H<=B)):-pfcPreferBC(H).



fwc:-true.
bwc:-true.

is_fc_body(P):- (fwc==P ; (compound(P),arg(_,P,E),is_fc_body(E))),!.
is_bc_body(P):- (bwc==P ; (compound(P),arg(_,P,E),is_bc_body(E))),!.


:-module_transparent(pfc_file_expansion/2).
:-module_transparent(pfc_file_expansion_lc/2).
pfc_file_expansion(A,B):- A\=(:-(_)), compound(A), loop_check(pfc_file_expansion_lc(A,B)).
pfc_file_expansion_lc(A,BO) :- A\=(:-(_)),not(thlocal:into_form_code), pfc_file_expansion_each(A,B),
   ((B=(:-(CALL))) -> (must(CALL),BO='$was_imported_kb_content$'(A,CALL)) ;
    ((((B=(:-(CALL)));pfc_may_expand)) -> (must(CALL),dmsg(expanded(A->B)),BO=CALL);  (pmsg(warn_PfcWantedToExpand(A)),pmsg(warn_into(B)),trace,pfc_may_expand,!,fail))).
:-export(pfc_file_expansion/2).


/*
pfc_file_expansion_each_pttp((P,Q), :- pttp_tell( (P,Q) )):- !.
pfc_file_expansion_each_pttp((P;Q), :- pttp_tell( (P;Q) )):- !.
pfc_file_expansion_each_pttp((P:-Is_pttp,Q), :- pttp_tell((P:-Q))):- Is_pttp==is_pttp.

pfc_file_expansion_each_pttp(all(Q,P), :-snark_tell(all(Q,P))):- !.
pfc_file_expansion_each_pttp(exists(Q,P), :-snark_tell(exists(Q,P))):- !.
pfc_file_expansion_each_pttp(~(P), :- snark_tell( (P) )):- !.
pfc_file_expansion_each_pttp(implies(P,Q), :- snark_tell( =>(P,Q) )):- !.
*/

pfc_file_expansion_each((P -->> Q),(:- pfc_add(Rule))) :- dtrace,
  pfc_translate_rule((P -->> Q), Rule).
pfc_file_expansion_each((P --*>> Q),(:- pfc_add(Rule))) :- dtrace,
  pfc_translate_rule((P --*>> Q), Rule).
pfc_file_expansion_each(':-'(_),_):-!,fail.
pfc_file_expansion_each((P=>Q),(:- pfc_mark_F(Q),pfc_add((P=>Q)))).
%pfc_file_expansion_each((P=>Q),(:- pfc_add(('<='(Q,P))))).  % DO NOT USE speed-up attempt
pfc_file_expansion_each(('<='(P,Q)),(:- pfc_mark_B(P),pfc_add(('<='(P,Q))))).
pfc_file_expansion_each((P<=>Q),(:- pfc_mark_C(P),pfc_mark_C(Q),pfc_add((P<=>Q)))).
pfc_file_expansion_each((RuleName :::: Rule),(:- pfc_add((RuleName :::: Rule)))).
pfc_file_expansion_each((=>P),(:- pfc_mark_F(P),pfc_add((=>P)))):-nonvar(P).
pfc_file_expansion_each('fwc'((Q)),(:- pfc_mark_F(Q),pfc_add(=>Q))):-nonvar(Q).

pfc_file_expansion_each((disabled(Q):-P),(:- pfcRem1(Q))):-P==true, nonvar(Q), (not(thlocal:pfcExpansion);pfc_mark_C(Q)),!.
pfc_file_expansion_each((enabled(Q):-P),(:- pfc_add(Q))):-P==true, nonvar(Q), (not(thlocal:pfcExpansion);pfc_mark_C(Q)),!.

pfc_file_expansion_each((disabled(Q):-P),(disabled(Q):-P)):- nonvar(P),P\==true,nonvar(Q),(not(thlocal:pfcExpansion);pfc_mark_C(Q)),!.
pfc_file_expansion_each((enabled(Q):-P), (:-(pfc_mark_C(Q),pfc_add(Q<=P)))):- nonvar(P),P\==true,nonvar(Q).

pfc_file_expansion_each(((Q:-P)),(:- (pfc_mark_F(Q),pfc_mark_C(P=>Q),pfc_add(P=>Q)))):- pfcMustFC(Q),!.

pfc_file_expansion_each(((Q:-P)),(:- (pfc_mark_F(Q),pfc_mark_C(P=>Q),pfc_add(P=>Q)))):- nonvar(P),P\==true,nonvar(Q),is_fc_body(P),!.
pfc_file_expansion_each(((Q:-P)),(:- (pfc_mark_B(Q),pfc_mark_C(Q),pfc_add(Q<=P)))):- nonvar(P),P\==true,nonvar(Q),is_bc_body(P),!.

%pfc_file_expansion_each(((Q:-P)),(:- pfc_mark_B(Q),pfc_add(Q<=P))):- nonvar(P),nonvar(Q),P\==true,not(is_fc_body(P)),pfcControlled(Q),!.
pfc_file_expansion_each(P,(:- pfc_add(P))):- pfcMustFC(P),!.
pfc_file_expansion_each(P,(:- pfc_add(P))):-pfcControlled(P),!.
%pfc_file_expansion_each(((Q:-P)),(:- (pfc_mark_B(Q),pfc_mark_C(Q),pfc_add((Q:-P))))):- nonvar(P),P\==true,nonvar(Q),pfcUseAllBC((Q:-P)).
%pfc_file_expansion_each((Q,(:- (pfc_mark_C(Q),pfc_add(Q))))):- nonvar(Q),pfcUseAllFact(Q).
pfc_file_expansion_each(P,O):- pfc_transform_neck(P,H), H \=@= P, pfc_file_expansion_each(H,O).
pfc_file_expansion_each(P,(:- CALL)):- pfc_get_setting(add,P,How),!,How\==prolog,CALL=..[How,P].

pfcMustUseFC(G):- once(pfcRuleOutcomeHeadBody(G,H,_)),H\=@=G,!,pfcMustUseFC(H).
pfcMustUseFC(G):- get_functor(G,F),not(pfc_mpred_prop(F,prologOnly)),pfc_mpred_prop(F,pfcMustFC).

pfcUseAllBC(((Q:-P))):-may_use_head(Q),no_head_singletons_hb(Q,P).
pfcUseAllFact(Q):-may_use_head(Q),no_head_singletons_hb(Q,true).

no_head_singletons_hb(Q,P):-not(((head_singletons_hb(Q,P,_),get_functor(Q,F,A),decl_mpred_prolog(F/A)))).

callBC(isa(_,_)):-!,fail.
callBC(G):-pfc_negation(G,Pos),!,show_call(not(callBC(Pos))),!.
callBC(G):- loop_check_nr(pfcBC_NoFacts(G)).

may_never_deduce_bc_change.

may_use_head(_):-may_never_deduce_bc_change,!,fail.
may_use_head(Q):-var(Q),!,fail.
may_use_head(_:_):-!,fail.
may_use_head(Q):-Q \= (F/A),!, get_functor(Q,F,A),!,may_use_head(F/A).
may_use_head(F/_):- atom_contains(F,'_'),!,fail.
may_use_head(F/_):- pfc_mpred_prop(F,prologOnly),!,fail.
may_use_head(F/_):- current_predicate(F/A),make_functor(G,F,A),real_builtin_predicate(G),!,fail.
may_use_head(F/A):- make_functor(G,F,A),real_builtin_predicate(G),!,fail.
may_use_head(_/1):-!,fail.
may_use_head(_/2).
% may_use_head(_/_).


pfc_addPreTermExpansion((I1,I2)):-!,pfc_addPreTermExpansion(I1),pfc_addPreTermExpansion(I2).
pfc_addPreTermExpansion(Info):-pfc_file_expansion_each(Info,What),!,What=(:-Call),show_call(must(call(Call))).


% pfcFile('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

:- use_module(library(lists)).

:- decl_mpred_pfc ('=>')/2.
:- decl_mpred_pfc ('::::')/2.
:- decl_mpred_pfc '<=>'/2.
:- decl_mpred_pfc '<='/2.
:- decl_mpred_pfc 'pfcPT'/2.
:- decl_mpred_pfc 'pfcNT'/3.
:- decl_mpred_pfc 'pfcBT'/2.
:- decl_mpred_pfc pfcUndoMethod/2.
:- decl_mpred_pfc pfcAction/2.

:- decl_mpred_pfc pfc_select/1.
:- decl_mpred_pfc pfcDatabaseTerm/1.


%:- decl_mpred_pfc pfcTmsMode/1.
:- decl_mpred_pfc pfc_queue/1.
%:- decl_mpred_pfc pfc Default/1.

:- decl_mpred_pfc pfcDatabase/1.
:- decl_mpred_pfc pfcHaltSignal/0.
%:- decl_mpred_pfc pfcDebugging/0.
%:- decl_mpred_pfc pfcSearch/1.

:- decl_mpred_pfc pfc_settings/3.

%%= initialization of global assertons 

%= pfc_setting_default/3 initialized a global assertion.
pfc_setting_default(Prop,Pred,Type):- pfc_settings(Prop,Pred,_)->true;asserta(pfc_settings(Prop,Pred,Type)).

%= pfcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfc_setting_default(tmsMode,default,cycles).

% Pfc Search strategy. pfc_settings(searchMode,default,X) where X is one of {direct,depth,breadth}
:- pfc_setting_default(searchMode,default,direct).



% pfc_add(/*to_exp*/((:-export number/1))):-trace_or_throw(crazy_pfc_add(/*to_exp*/((:-export number/1)))).


% EOF

%= add/2 and pfcPost/2 are the main ways to assert new clauses into the
%= database and have forward reasoning done.

%= pfc_add(P,S) asserts P into the dataBase with support from S.

pfc_add(P) :- current_predicate(add/1),!,add(P).
pfc_add(P) :- pfc_add_fast(P).

pfc_add_fast(P) :- get_user_support_for_add(P,S),pfc_add(P,S).

pfc_add(P,S):-pfc_maptree(pfc_add1,P,[S]).

pfc_overrided_add(S,(=>(P)),P):- nonvar(P).
pfc_overrided_add(S,((P<= TRUE)),P):- nonvar(P),is_true(TRUE).
pfc_overrided_add(S,P,PO):- pfc_slow_search, pfc_fully_expand_warn(change(assert,pfc_overrided_add),P,PO), PO\=@=P.
pfc_overrided_add(S,P,P):- is_wrong(P,S,_),!.
%pfc_overrided_add(_,(H:-B),(H<=B)) :- head_singletons_hb(H,B,_),!,wdmsg("adding pfcBC instead of Neck ~q",[pfc_add1(H<=B)]).
%pfc_overrided_add(_,(B=>H),(H<=B)) :-head_singletons_hb(H,B,_),!,wdmsg("adding pfcBC instead of pfcFWC ~q",[pfc_add1(H<=B)]).
pfc_overrided_add(_,(H:-B),(G)) :-pfcPreferedDir(H,B,G),!,wdmsg(error("adding ~q",[pfcPreferedDir(H,B,G)])).

is_wrong(P,S,Why):-pfc_negate_for_add(P,N),P\=@=N,pfcGetSupport(N,Why),!,pmsg(warn(want(P,S),but,N -> Why)),!,dtrace.

pfc_add1(P,S):-transitive(pfc_overrided_add(S),P,PO), P\=@=PO ,!, pfc_warn("pfc_add1 Changed ~q",[P->PO]),pfc_add1(PO,S).
% pfc_add1(P,_):-pfcTypeFull(P,T),T==support,!,assert_if_new(P),!.
% pfc_add1(P,S) :- is_wrong(P,S,_),!.
pfc_add1(P,_):-pfc_ignored(P),!.
%pfc_add1(PO,S0) :- is_deduced_by_god(S0),pfcGetSupport(PO,Why),!,trace_or_throw(not_needed(Why,pfc_add1(PO,S0))).
pfc_add1(PO,S0) :- is_deduced_by_god(S0),pfcGetSupport(PO,Why),!,pmsg(not_needed(Why,pfc_add1(PO,S0))).
pfc_add1(PO,USER) :- get_user_support_for_lookup(_,USER),pfcGetSupport(PO,Why),Why==USER,!,pmsg(already(Why,pfc_add1(PO,S0))).
pfc_add1(PO,USER) :- get_user_support_for_lookup(_,USER),pfcGetSupport(PO,Why),Why\==USER,!,pmsg(user_needed(Why,pfc_add1(PO,USER))),!,pfc_add2(PO,USER).

pfc_add1(PO,S0):-pfc_add2(PO,S0).

pfc_add2(PO,S0) :- copy_term(PO:S0,P1:S1),
 must((pfc_pre_expansion_each(P1,P),pfc_pre_expansion_each(S1,S))),
  must(copy_term(P-S,P2-S2)),
  must(pfcPost(P2,S2)),
  sanity(variant(P:S,P2:S2)),
  pfcRun,!.

%pfc_add1(_,_).
pfc_add2(P,S) :- pfcError("pfc_add(~w,~w) failed",[P,S]),!,fail.


% OLD VERSION
pfc_add_BTRule(P,S):-!, must(pfcPost(P,S)), pfcRun.

% USE UNTIL IT DONT WORK
pfc_add_BTRule(P,S):- !,
  pfc_addSupport(P,S),
  assertz_if_new(P).

% MAYBE VERSION
pfc_add_BTRule(P,S):-
  %= db pfc_addDbToHead(P,P2),
  % pfcRemoveOldVersion(P),
  sanity(copy_term(P,PC)),
  must((pfc_addSupport(P,S),sanity(PC=@=P))),
  pfcUnique(P),sanity(PC=@=P),
  assertz_if_new(P),
  % pfcTraceAdd(P,S),
   !,
  % would never be ran? 
  pfcEnqueue(P,S,breadth),!. 

pfc_add_BTRule(P,S) :- pfcError("pfc_add(~w,~w) failed",[P,S]),!,fail.



% pfcPost(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) pfcPost1 is called. It always succeeds.

pfcPost(Each,S) :- pfc_maptree(pfcPost1,Each,[S]).

% pfcPost1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

pfcPost1(P,_):-pfc_ignored(P),!.
pfcPost1(P,S) :- 
  %= db pfc_addDbToHead(P,P2),
  % pfcRemoveOldVersion(P),
  copy_term(P,PC),
  must((pfc_addSupport(P,S),sanity(PC=@=P))),
  pfcUnique(P),sanity(PC=@=P),
  must(pfcAssertIfUnknown(P)), % was simply pfc_assert(P),
   pfcTraceAdd(P,S),
   !,
   pfcEnqueue(P,S),
   !.

pfcPost1(P,S) :- (\+ \+ pfcUnique(P)),pfcError("(maybe ERROR?!) pfc_add(~w,~w) failed",[P,S]),!.
pfcPost1(_,_).
pfcPost1(P,S) :-  pfcError("pfc_add(~w,~w) failed",[P,S]).


pfcRepropagate(P) :-
  forall(must(pfcGetSupport(P,S)), pfcRepropagate(P,S)).

pfcRepropagate(P,S) :-
  (\+ \+ must(pfcAssertIfUnknown(P))), % was simply pfcAssertS(P),
  pfcTraceAdd(P,S),
  !,
  pfcEnqueue(P,S),
  !.



%%
%= pfc_addDbToHead(+P,-NewP) talkes a fact P or a conditioned fact
%= (P:-C) and adds the Db context.
%%
/*
pfc_addDbToHead(P,NewP) :-
  pfcCurrentDb(Db),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).
*/

% pfcUnique(X) is true if there is no assertion X in the prolog db.

pfcUnique((Head:-Tail)) :- 
  !, 
  \+ pfc_clause_db_unify(Head,Tail).

pfcUnique(P) :-
  !,
  \+ pfc_clause_db_unify(P,true).


pfcEnqueue(P,S):-
 pfc_get_setting(searchMode,P,Mode) -> pfcEnqueue(P,S,Mode) ; pfc_warn("No pfcSearch mode").

pfcEnqueue(P,S,Mode) :-
     (Mode=direct  -> pfcFwd(P) ;
	Mode=depth   -> pfcAssertAInt(pfc_queue(P),S) ;
	Mode=breadth -> pfcAssertInt(pfc_queue(P),S) ;
	otherwise    -> pfcError("Unrecognized pfcSearch mode: ~w", Mode)).
    


% if there is a rule of the form Identifier ::: Rule then delete it.

pfcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  var(Identifier),
  !,
  pfcError("variable used as an  rule name in ~w :::: ~w",
          [Identifier,Body]).

  
pfcRemoveOldVersion((Identifier::::Body)) :-
  nonvar(Identifier),
  pfc_clause_db_unify((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfcRem1((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion(_).



% EOF

% pfcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfc_queue mechanism.

pfcRun :-
  (\+ pfc_settings(searchMode,default,direct)),
  pfcRunSteps.

pfcRun :- pfcRunQueueNoHalt.


pfcRunSteps:- 
  pfcStep,
  pfcRunSteps.


pfcRunQueueNoHalt:- doall(( pfc_queue(Q),\+ pfcHaltSignal,retract(pfc_queue(Q)),pfcdo(pfcFwd(P)))).
  

pfcStep :- \+ pfcHaltSignal,
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  pfcdo(pfcFwd(P)),
  !.
% pfcStep removes one entry from the pfc_queue and reasons from it.
pfcStep :-  
  % if pfcHaltSignal is true, reset it and fail, thereby stopping inferencing.
  retract(pfcHaltSignal),
  !, 
  fail.

get_next_fact(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P) :- 
  pfcRetractInternal(pfc_queue(P)),
  pfcRemoveSupportsQuietly(pfc_queue(P)),
  !.
remove_selection(P) :-
  brake(pmsg("pfc:get_next_fact - selected fact not on Queue: ~w", [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(P) :- 
  pfc_select(P),
  !.  
select_next_fact(P) :- 
  defaultpfcSelect(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultpfcSelect(P) :- pfc_queue(P),!.

% pfc_halt stops the forward chaining.
pfc_halt :-  pfc_halt("",[]).

pfc_halt(Format) :- pfc_halt(Format,[]).

pfc_halt(Format,Args) :- 
  pmsg(Format,Args),
  pfcHaltSignal -> 
       pfc_warn("pfc_halt finds pfcHaltSignal already set")
     ; assert(pfcHaltSignal).


% EOF
%%
%= predicates for manipulating triggers
%%


pfc_addTrigger(pfcPT(Trigger,Body),Support) :-
  !,
  pfc_trace_msg('      Adding positive trigger ~q~n',
		[pfcPT(Trigger,Body)]),
  pfcAssertInt(pfcPT(Trigger,Body),Support),
  copy_term(pfcPT(Trigger,Body),Tcopy),
  pfcTriggerCall(Trigger),
  pfcEvalLHS(Body,(Trigger,Tcopy)),
  fail.


pfc_addTrigger(pfcNT(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('      Adding negative trigger: ~q~n       test: ~q~n       body: ~q~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfcAssertInt(pfcNT(TriggerCopy,Test,Body),Support),
  \+Test,
  pfcEvalLHS(Body,((\+Trigger),pfcNT(TriggerCopy,Test,Body))).

pfc_addTrigger(pfcBT(Trigger,Body),Support) :-
  !,
  pfcAssertInt(pfcBT(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body).

pfc_addTrigger(X,Support) :-
  pfcError("Unrecognized trigger to pfc_addtrigger: ~w",[trigger(X,Support)]).


pfcBtPtCombine(Head,Body,Support) :- 
  %= a backward trigger (pfcBT) was just added with head and Body and support Support
  %= find any pfcPT's with unifying heads and add the instantied pfcBT body.
  pfcGetTriggerQuick(pfcPT(Head,_PtBody)),
  pfcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  pfc_clause_db_unify(Trigger,true).

pfcGetTrigger(Trigger):-pfcGetTriggerQuick(Trigger).

% EOF
%%
%= predicates for manipulating action traces.
%%

pfc_addActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  pfc_addSupport(pfcAction(Action),Support).

pfcRemActionTrace(pfcAction(A)) :-
  pfcUndoMethod(A,M),
  M,
  !.


% EOF
%= predicates to remove pfc facts, triggers, action traces, and queue items
%= from the database.
%%

pfcRetractInternal(X) :- 
  %= pfc_retract an arbitrary thing.
  pfc_db_type(X,Type),
  pfcRetractTypeInternal(Type,X),
  !.

pfcRetractTypeInternal(rule,X) :- 
  %= db  pfc_addDbToHead(X,X2),  pfc_retract(pfcInternal(rule),X2).
  pfc_retract(pfcInternal(rule),X).

pfcRetractTypeInternal(trigger,X) :- 
  pfc_retract(pfcInternal(trigger),X)
    -> unFc(X)
     ; pfc_warn("Trigger not found to pfc_retract: ~w",[X]).

pfcRetractTypeInternal(action,X) :- pfcRemActionTrace(X).

pfcRetractTypeInternal(support,X) :-  pfc_retract(pfcInternal(support),X).

pfcRetractTypeInternal(fact,X) :-   
  %= db pfc_addDbToHead(X,X2), pfc_retract(pfcInternal,X2). 
  pfc_retract(pfcInternal(fact),X).

%= pfc_addSome(X) adds item X to some database

pfc_addSome(X) :-
  % what type of X do we have?
  pfc_db_type(X,Type),
  % pfc_call_prolog_native the appropriate predicate.
  pfc_addType(Type,X).

pfc_addType(support,X) :- 
  pfcAssertS(X),!.
pfc_addType(rule,X) :- 
  pfcUnique(X), 
  pfcAssertS(X),!.
pfc_addType(trigger,X) :- 
  pfcAssertS(X).
pfc_addType(action,_Action) :- !.
pfc_addType(fact,X) :- 
  must(pfcUnique(X)), 
  pfc_assert(X),!,
  run_database_hooks(change(assert,z),X).



  

%= pfcRem1(P,S) removes support S from P and checks to see if P is still supported.
%= If it is not, then the fact is retreactred from the database and any support
%= relationships it participated in removed.
pfcRem1(P) :- 
  % pfcRem1/1 is the user's interface - it withdraws user support for P.
  pfc_maptree(pfc_lambda([E],pfcRem_user(E)),P).

pfcRem_user(E):- get_user_support_for_remove(E,S),!,pfcRem1(E,S).


pfcRem1(P,S) :-
  % pfcDebug(pmsg("removing support ~w from ~w",[S,P])),
  pfc_trace_msg('Removing support: ~q from ~q~n',[S,P]),  
  pfcRemSupport(P,S)
     -> pcfRemoveIfUnsupported(P)
      ; pfc_warn("pfcRem1/2 Could not find support ~w to remove from fact ~w", [S,P]).

%%
%= pfc_rem2 is like pfcRem1, but if P is still in the DB after removing the
%= user's support, it is retracted by more forceful means (e.g. pfcRem3/1).
%%

pfc_rem2(P) :- 
  % pfc_rem2/1 is the user's interface - it withdraws user support for P.
  pfc_maptree(pfc_lambda([E],pfcRem2_user(E)),P).

pfcRem2_user(E):- get_user_support_for_remove(E,S), pfc_rem2(E,S).


pfc_rem2(P,S) :-
  pfcRem1(P,S),
  pfcCall(P)
     -> pfcRem3(P) 
      ; true.

%%
%= pfcRem3(+F) retracts fact F from the DB and removes any dependent facts */
%%

pfcRem3(F) :- 
  pfcRemoveSupports(F),
  pfcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :- 
  pfcRemSupport(F,S),
  pfc_warn("~w was still supported by ~w",[F,S]),
  fail.
pfcRemoveSupports(_).

pfcRemoveSupportsQuietly(F) :- 
  pfcRemSupport(F,_),
  fail.
pfcRemoveSupportsQuietly(_).

% pfcUndo(X) undoes X.



pfcUndo(pfcNT(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (pfc_retract(pfcInternal(pfcNT),pfcNT(Head,Condition,Body))
    -> unFc(pfcNT(Head,Condition,Body))
     ; pfcError("Trigger not found to pfc_retract: ~w",[pfcNT(Head,Condition,Body)])).

pfcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  pfc_retract(pfcUndo,Fact),!,
  pfcTraceRem(Fact),
  unFc1(Fact).

pfcUndo(Fact) :- pmsg(no_pfcUndo(Fact)),sanity((functor(Fact,F,_),not((atom_concat(_,'Fn',F),dtrace)))).


%= unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
%= been removed from the database, so remove all support relations it
%= participates in and check the things that they support to see if they
%= should stayu in the database or should also be removed.


unFc(F) :- 
  pfcRetractSupportRelations(F),
  unFc1(F).

unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  pfcRun.


pfcUnFcCheckTriggers(F) :-
  pfc_db_type(F,fact),
  copy_term(F,Fcopy),
  pfcNT(Fcopy,Condition,Action),
  (\+ Condition),
  pfcEvalLHS(Action,((\+F),pfcNT(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractSupportRelations(Fact) :-
  pfc_db_type(Fact,Type),
  (Type=trigger -> pfcRemSupport(P,(_,Fact))
                ; pfcRemSupport(P,(Fact,_))),
  pcfRemoveIfUnsupported(P),
  fail.
pfcRetractSupportRelations(_).



%= pcfRemoveIfUnsupported(+P) checks to see if P is supported and removes
%= it from the DB if it is not.

pcfRemoveIfUnsupported(P) :- 
   pfcSupported(P) -> true ;  pfcUndo(P).


%= pfcSupported(+P) succeeds if P is "supported". What this means
%= depends on the TMS mode selected.

pfcSupported(P) :- 
  pfc_settings(tmsMode,P,Mode),
  pfc_tms_supported(Mode,P).

pfc_tms_supported(local,P) :- !, pfcGetSupport(P,A),!,A\=fail.
pfc_tms_supported(cycles,P) :-  !, wellFounded(P).
pfc_tms_supported(full,P) :-  !, wellFounded(P).
pfc_tms_supported(_,_P) :- true.


% EOF
%= a fact is well founded if it is supported by the user
%= or by a set of facts and a rules, all of which are well founded.
%%

wellFounded(Fact) :- pfcWFF(Fact,[]).

pfcWFF(F,_) :-
  % supported by user (pfcAxiom) or an "absent" fact (assumption/assumable).
  (pfcAxiom(F) ; pfcAssumptionBase(F)),
  !.

pfcWFF(F,Descendants):- 
   pfcWFF_Descendants(F,Descendants).

pfcWFF_Descendants(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supportsForWhy(F,Supporters),
  % all of whose members are well founded.
  pfcWFF_L(Supporters,[F|Descendants]),
  !.

%= pfcWFF_L(L) simply maps pfcWFF over the list.

pfcWFF_L([],_).
pfcWFF_L([X|Rest],L) :-
  pfcWFF(X,L),
  pfcWFF_L(Rest,L).


% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [u/*(Original)*/].

supportsForWhy(F,[Fact|MoreFacts]) :-
  pfcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(u,[]) :- !.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).


% EOF
%%
%= pfcFwd(X) forward chains from a fact or a list of facts X.
%%
pfcFwd(X):-
  with_assertions(thlocal:pfc_no_mark,pfc_maptree(pfcFwd1,X)).

%%
%= pfcFwd1(+P) forward chains for a single fact.
%%
pfcFwd1(Fact) :-
   pfcRuleOutcomeHead(Fact,Outcome),!,
    loop_check_term(pfcFwd1_newoutcome(Fact),Outcome,
       (pmsg(looped_pfcRuleOutcomeHead(Fact,Outcome)))),!.

pfcFwd1_newoutcome(Fact) :-
  fc_rule_check(Fact),
  copy_term(Fact,F),
  % check positive triggers
  pfcRunPT(Fact,F),
  % check negative triggers
  pfcRunNT(Fact,F).


%%
%= fc_rule_check(P) does some special, built in forward chaining if P is 
%= a rule.
%= 

fc_rule_check((P=>Q)) :-  
  !,  
  pfcProcessRule(P,Q,(P=>Q)).
fc_rule_check((Name::::P=>Q)) :- 
  !,  
  pfcProcessRule(P,Q,(Name::::P=>Q)).
fc_rule_check((P<=>Q)) :- 
  !, 
  pfcProcessRule(P,Q,(P<=>Q)), 
  pfcProcessRule(Q,P,(P<=>Q)).
fc_rule_check((Name :::: P <=> Q)) :- 
  !, 
  pfcProcessRule(P,Q,((Name::::P<=>Q))), 
  pfcProcessRule(Q,P,((Name::::P<=>Q))).

fc_rule_check(('<='(P,Q))) :-
  !,
  pfcDefineBcRule(P,Q,('<='(P,Q))).

fc_rule_check(_).


pfcRunPT(Fact,F) :- 
  pfcGetTriggerQuick(pfcPT(F,Body)),
  pfc_trace_msg('      Found positive trigger: ~q~n       body: ~q~n',
		[F,Body]),
  not_too_deep(pfcRunPT, pfcEvalLHS(Body,(Fact,pfcPT(F,Body)))),
  fail.

%pfcRunPT(Fact,F) :- 
%  pfcGetTriggerQuick(pfcPT(presently(F),Body)),
%  pfcEvalLHS(Body,(presently(Fact),pfcPT(presently(F),Body))),
%  fail.

pfcRunPT(_,_).

pfcRunNT(_Fact,F) :-
  support2(pfcNT(F,Condition,Body),X,_),
  Condition,
  pfcRem1(X,(_,pfcNT(F,Condition,Body))),
  fail.
pfcRunNT(_,_).


%%
%= pfcDefineBcRule(+Head,+Body,+ParentRule) - defines a backeard
%= chaining rule and adds the corresponding pfcBT triggers to the database.
%%

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfc_literal(Head)),
  pfc_warn("Malformed backward chaining rule.  ~w not atomic.",[Head]),
  pfc_warn("rule: ~w",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfcBuildRhs(Head,Rhs),
  pfcForEach(pfc_nf(Body,Lhs),
          (pfcBuildTrigger(Lhs,rhs(Rhs),Trigger),
           pfc_add_BTRule(pfcBT(Head,Trigger),(ParentRuleCopy,u)))).
 


% EOF
%%
%= eval something on the LHS of a rule.
%%

 
pfcEvalLHS((Test->Body),Support) :-  
  !, 
  (pfc_call_prolog_native(pfcTest,Test) -> pfcEvalLHS(Body,Support)),
  !.

pfcEvalLHS(rhs(X),Support) :-
  !,
  pfc_eval_rhs(X,Support),
  !.

pfcEvalLHS(X,Support) :-
  pfc_db_type(X,trigger),
  !,
  pfc_addTrigger(X,Support),
  !.

%pfcEvalLHS(snip(X),Support) :- 
%  snip(Support),
%  pfcEvalLHS(X,Support).

pfcEvalLHS(X,_) :-
  pfcError("Unrecognized item found in trigger body, namely ~w.",[X]).


%%
%= eval something on the RHS of a rule.
%%

pfc_eval_rhs([],_) :- !.
pfc_eval_rhs([Head|Tail],Support) :- 
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs(Tail,Support).



pfc_eval_rhs1(XXrest,Support) :- is_list(XXrest),
 % embedded sublist.
 !, pfc_eval_rhs(XXrest,Support).


pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 pfcEvalAction(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfcNegatedLiteral(P),
 !,
 pfcRem1(P).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 pfcPost1(Assertion,Support).

pfc_eval_rhs1(X,_) :-
  pfcError("Malformed rhs of a rule: ~w",[X]).


%%
%= evaluate an action found on the rhs of a rule.
%%

pfcEvalAction(Action,Support) :-
  (pfc_call_prolog_native(pfcEvalAction,Action)), 
  (pfcUndoable(Action) 
     -> pfc_addActionTrace(Action,Support) 
      ; true).


%%
%= 
%%

pfc_trigger_the_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
pfc_trigger_the_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfcCall(Trigger),
%  pfcEvalLHS(Body,(presently(Trigger),pfcPT(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfcTriggerCall(Trigger),
  pfcEvalLHS(Body,(Trigger,pfcPT(TriggerCopy,Body))),
  fail.


%%
%= The predicate pfc/1 is the proper way to access terms in the Pfc database. pfc(P) succeeds if P is a term
%= in the current pfc database after invoking any backward chaining rules or is provable by Prolog.
%= pfcCall(F) is true iff F is a fact available for forward (backward?) chaining.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%%
pfcCall(F):-loop_check_nr(pfcBC_Cache(F)).

pfcTriggerCall(F):-
   with_assertions(thlocal:no_side_effects,loop_check_nr(pfc_user_fact(F);pfcBC_Cache(F))).



pfcBC_Cache(P) :-
  % trigger any bc rules.
  pfcBT(P,Trigger),
  pfcGetSupport(pfcBT(P,Trigger),S),
  pfcEvalLHS(Trigger,S),
  maybeSupport(P,S),
  fail.


pfcBC_Cache(F) :-
  %= this is probably not advisable due to extreme inefficiency.
  var(F)    ->  pfcFact(F) ;
  ( \+ current_predicate(_,F)) -> pfc_system_call(F) ;
  % check for system predicates as well.
  \+(predicate_property(F,number_of_clauses(_))) -> pfc_call_prolog_native(systemPred,F) ; 
  otherwise ->  (pfc_clause_db_unify(F,Condition), 
    pfc_call_prolog_native(neck(F),Condition), 
       ignore((ground(F),(\+(pfc_is_asserted(F)), maybeSupport(F,(pfcGod,pfcGod)))))).


maybeSupport(P,_):-pfc_ignored(P),!.
maybeSupport(P,S):-( \+ ground(P)-> true;
  (predicate_property(P,dynamic)->pfc_add(P,S);true)).


%%
%= pfcBC_NoFacts(F) is true iff F is a fact available for backward chaining ONLY.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%= this Predicate should hide Facts from callBC/1
%%
pfcBC_NoFacts(F):- pfcBC_NoFacts_TRY(F)*-> true ; (pfc_slow_search,pfcBC_Cache(F)).

pfcBC_NoFacts_TRY(F) :- nonvar(F),
 (
  %= this is probably not advisable due to extreme inefficiency.
  var(F)    ->  pfcFact(F) ;
  ( \+ current_predicate(_,F)) -> pfc_system_call(F) ;
  % check for system predicates as well.
  \+(predicate_property(F,number_of_clauses(_))) -> pfc_call_prolog_native(systemPred,F) ; 
  otherwise -> pfcBC_NoFacts_TRY2(F)).

ruleBackward(F,Condition):-ruleBackward0(F,Condition),Condition\=call(F).
ruleBackward0(F,Condition):-pfc_clause_db_unify(F,Condition),not(is_true(Condition);is_meta_info(Condition)).
ruleBackward0(F,Condition):-'<='(F,Condition),not(is_true(Condition);is_meta_info(Condition)).

pfcBC_NoFacts_TRY2(F) :- no_repeats(ruleBackward(F,Condition)),
  pfc_call_prolog_native(neck(F),Condition),\+ clause(F,true),
  maybeSupport(F,(pfcGod,pfcGod)).




% an action is pfcUndoable if there exists a method for undoing it.
pfcUndoable(A) :- pfcUndoMethod(A,_).



% EOF
%%
%= defining fc rules 
%%

%= pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form 
%= Out.  It also does certain optimizations.  Backtracking into this
%= predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  pfc_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


%= pfc_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%= Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P / Condition form is no longer used anywhere.

pfc_nf1(P / Cond,[(\+P) / Cond]) :- pfcNegatedLiteral(P), !.

pfc_nf1(P / Cond,[P / Cond]) :-  pfcLiteralOrNeg(P), !.

%= handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_negation(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

%= disjunction.

pfc_nf1((P;Q),NF) :- 
  !,
  (pfc_nf1(P,NF) ;   pfc_nf1(Q,NF)).


%= conjunction.

pfc_nf1((P,Q),NF) :-
  !,
  pfc_nf1(P,NF1),
  pfc_nf1(Q,NF2),
  append(NF1,NF2,NF).

%= handle a random atom.

pfc_nf1(P,[P]) :- 
  pfcLiteralOrNeg(P),  %= DMILES TODO unaddNeg
  !.


%%= shouln't we have something to catch the rest as errors?
pfc_nf1(Term,[Term]) :-
  pfcError("pfc_nf doesn't know how to normalize ~w",[Term]).


%= pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
pfc_nf1_negation((P / Cond),[(\+(P)) / Cond]) :- !.

pfc_nf1_negation((P;Q),NF) :-
  !,
  pfc_nf1_negation(P,NFp),
  pfc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

pfc_nf1_negation((P,Q),NF) :- 
  % this code is not correct! twf.
  !,
  pfc_nf1_negation(P,NF) 
  ;
  (pfc_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

pfc_nf1_negation(P,[\+P]).




%= pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%= changing ~{...} to {\+...}
%%= ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations(X,X) :- var(X),!.

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

pfc_nf_negation(Form,{PLNeg}) :-  
  nonvar(Form),
  correct_negations(callable,Form,PLNeg),
  !.
pfc_nf_negation(X,X).


%%
%= pfcBuildRhs(+Conjunction,-Rhs)
%%

pfcBuildRhs(X,[X]) :- 
  var(X),
  !.

pfcBuildRhs((A,B),[A2|Rest]) :- 
  !, 
  pfcCompileRhsTerm(A,A2),
  pfcBuildRhs(B,Rest).

pfcBuildRhs(X,[X2]) :-
   pfcCompileRhsTerm(X,X2).

pfcCompileRhsTerm((P/C),((P:-C))) :- !.
pfcCompileRhsTerm(P,P).

pfc_negate_for_add(NQ,NQ):-is_ftVar(NQ),!.
pfc_negate_for_add(','(_,Q),NQ):-!,pfc_negate_for_add(Q,NQ).
pfc_negate_for_add('<=>'(P,Q),'<=>'(P,NQ)):-!,pfc_negate_for_add(Q,NQ).
pfc_negate_for_add(In,Out):-pfc_negate(pfc_negate_for_add,In,Out).

pfc_negate(C,'=>'(P,Q),'=>'(P,NQ)):-!,call(C,Q,NQ).
pfc_negate(C,'<='(Q,P),'<='(NQ,P)):-!,call(C,Q,NQ).
pfc_negate(C,':-'(Q,P),':-'(NQ,P)):-!,call(C,Q,NQ).
pfc_negate(_,N,P):-pfcNegatedLiteral(N),!,pfc_negation(N,P).
pfc_negate(_,P,N):-pfcPositiveLiteral(P),!,N=not(P).

%= pfc_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

pfc_negation((~P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).
pfc_negation((naf(P)),P).
% pfc_negation(not(P)),P).
% pfc_negation(NP,PP):-loop_check_nr(pfc_negation0(NP,PP)).
pfc_negation0(NP,PP):- compound(NP), NP=..[NF,A|RGS],negated_functor(NF,PF),!,PP=..[PF,A|RGS].

pfcNegatedLiteral(P) :- 
  pfc_negation(P,Q),
  pfcPositiveLiteral(Q),!.

pfcLiteralOrNeg(X):-pfc_literal(X),!.
pfcLiteralOrNeg(V):-is_ftVar(V),!.
pfcLiteralOrNeg(neg(X)):-pfc_literal(X),!.
pfcLiteralOrNeg(neg(V)):-!.

pfc_literal(X) :- pfcNegatedLiteral(X),!.
pfc_literal(X) :- pfcPositiveLiteral(X),!.

pfcPositiveLiteral(X) :- nonvar(X), 
  functor(X,F,_), 
  \+ pfcConnective(F).

pfcConnective(';').
pfcConnective(',').
pfcConnective('/').
pfcConnective('|').
pfcConnective(('=>')).
pfcConnective(('<=')).
pfcConnective('<=>').

pfcConnective('-').
pfcConnective('~').
pfcConnective(('\\+')).
pfcConnective(neg):-!,fail.
pfcConnective(F):- loop_check(if_defined(is_logical_functor(F))).

:-dynamic((~)/1).
:-dynamic((neg)/1).
~(F):- \+ pfcCall(F).



pfcProcessRule(Lhs,Rhs,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfcBuildRhs(Rhs,Rhs2),
  pfcForEach(pfc_nf(Lhs,Lhs2), 
          pfcBuild1Rule(Lhs2,rhs(Rhs2),(ParentRuleCopy,u))).

pfcBuild1Rule(Lhs,Rhs,Support) :-
  pfcBuildTrigger(Lhs,Rhs,Trigger),
  pfcEvalLHS(Trigger,Support).

pfcBuildTrigger([],Consequent,Consequent).

pfcBuildTrigger([V|Triggers],Consequent,pfcPT(V,X)) :-
  var(V),
  !, 
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([(T1/Test)|Triggers],Consequent,pfcNT(T2,Test2,X)) :- 
  pfc_negation(T1,T2),
  !,
  pfcBuildNtTest(T2,Test,Test2),
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([(T1)|Triggers],Consequent,pfcNT(T2,Test,X)) :- 
  pfc_negation(T1,T2),
  !,
  pfcBuildNtTest(T2,true,Test),
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([T/Test|Triggers],Consequent,pfcPT(T,X)) :-
  !, 
  pfcBuildTest(Test,Test2),
  pfcBuildTrigger([{Test2}|Triggers],Consequent,X).


%pfcBuildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([T|Triggers],Consequent,pfcPT(T,X)) :-
  !, 
  pfcBuildTrigger(Triggers,Consequent,X).

%%
%= pfcBuildNtTest(+,+,-).
%%
%= builds the test used in a negative trigger (pfcNT/3).  This test is a
%= conjunction of the check than no matching facts are in the db and any
%= additional test specified in the rule attached to this ~ term.
%%

pfcBuildNtTest(T,Testin,Testout) :-
  pfcBuildTest(Testin,Testmid),
  pfcConjoin((pfcCall(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

pfcBuildTest({Test},Test) :- !,pfc_mark_W(Test).
pfcBuildTest(Test,Test):-pfc_mark_W(Test).

% EOF

mpred_listing(F/_):-!,term_listing(F).
mpred_listing(Pred):-
  (get_functor(Pred,F,AUsed),((AUsed==0)->ignore(arity(F,A));A=AUsed)),
  mpred_listing(F/A).


% predicate_property(P,meta_predicate(P)),arg(_,P,N),number(N)
user:listing_mpred_hook(Match):- debugOnError(loop_check_nr(pfc_listing_mpred_hook(Match))).

guess_arity(F,A):- no_repeats(F/A,(current_predicate(F/A);arity(F,A))).

pfc_listing_mpred_hook(Match):- must(nonvar(Match)),fail.
pfc_listing_mpred_hook(M:P):- atom(M),!,pfc_listing_mpred_hook(P).
pfc_listing_mpred_hook(F/Unk):- is_ftVar(Unk),!,forall(guess_arity(F,A),pfc_listing_mpred_hook(F/A)).
pfc_listing_mpred_hook(_/0):- !.
pfc_listing_mpred_hook(F/A):-!,ground(F/A),make_functor(Match,F,A),!,pfc_listing_mpred_hook(Match).
pfc_listing_mpred_hook(F):- atom(F),!,forall(guess_arity(F,A),pfc_listing_mpred_hook(F/A)).
pfc_listing_mpred_hook(Match):- once((must((pfc_fully_expand(pfc_listing_mpred_hook,Match,New))),
                                Match\=@=New)),
                                current_predicate(_,New),
                                listing(New),fail.
pfc_listing_mpred_hook(MFA):- pmsg(pfc_listing_mpred_hook(MFA)),fail.
pfc_listing_mpred_hook(Match):- not(not(pfcLiteralOrNeg(Match))),pfc_listing_mpred_hook_2nd(Match).


match_clauses(H,H,B):-clause(H,B).

nonvar_contains_term(V,A):-atomic(A),!,sub_term(VV,V),nonvar(VV),functor_safe(VV,A,_).
nonvar_contains_term(V,A):-sub_term(VV,V),nonvar(VV),A=@=VV.
% nonvar_contains_term(V,C):-functor(C,A,_),!,nonvar_contains_term(V,A).

head_search_for_listing(H):-member(H,[neg(_),(_=>_),(_<=_),(_<=>_),isa(_,_),argIsa(_,_,_)]).
head_search_for_listing(H):-  pfcDatabaseTerm(F/A),functor(H,F,A).

pfc_listing_mpred_hook_2nd(Match):- 
 no_repeats(CL,(( no_repeats(head_search_for_listing(F)),predicate_property(F,number_of_clauses(_)),match_clauses(F,H,B), CL=(H:-B),
   not(is_meta_info(B)),
   once(nonvar_contains_term(CL,Match))))),
   portray_clause(CL),fail.


pfcTypeFull(G,Type) :- pfc_db_type(G,Type),Type\=fact,!.
pfcTypeFull(spft(_,_,_),support).
pfcTypeFull(support2(_,_,_),support).
pfcTypeFull(support3(_,_,_),support).
pfcTypeFull((H:-B),Type):-is_true(B),!,pfcTypeFull(H,Type).
pfcTypeFull(_,fact) :-
  %= if it's not one of the above, it must be a fact!
  !.

%= simple typeing for pfc objects
pfc_db_type(('=>'(_,_)),Type) :- !, Type=rule.
pfc_db_type(('<=>'(_,_)),Type) :- !, Type=rule.
pfc_db_type(('<='(_,_)),Type) :- !, Type=rule.
pfc_db_type(pfcPT3(_,_,_),Type) :- !, Type=trigger.
pfc_db_type(pfcPT(_,_),Type) :- !, Type=trigger.
pfc_db_type(pfcNT(_,_,_),Type) :- !,  Type=trigger.
pfc_db_type(pfcBT(_,_),Type) :- !,  Type=trigger.
pfc_db_type(pfcAction(_),Type) :- !, Type=action.
pfc_db_type((('::::'(_,X))),Type) :- !, pfc_db_type(X,Type).
pfc_db_type(_,fact) :-
  %= if it's not one of the above, it must be a fact!
  !.


pfcAssertIfUnknown(G):-if_defined(into_mpred_form(G,P),G=P),unnumbervars(P,U),pfcAssertIfUnknown_nv(P,U).

pfcAssertIfUnknown_nv(P,U):- ( \+ \+ pfc_is_asserted(U)) -> true ; (pfc_assert(P),!,nop(no_loop_check(sanity(show_call_failure(pfc_is_asserted(U)))))).

pfcAssertS(P):-assert_if_new(P).
pfcAssertInt(P,Support) :- 
  (pfcClauseInt(P) ; pfcAssertS(P)),
  !,
  pfc_addSupport(P,Support).

pfcAssertAInt(P,Support) :-
  (pfcClauseInt(P) ; asserta_new(P)),
  !,
  pfc_addSupport(P,Support).


pfcClauseInt((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  pfc_clause_db_unify(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfcClauseInt(Head) :-
  % find a unit clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  pfc_clause_db_unify(Head_copy,true),
  variant(Head,Head_copy).


pfcForEach(Binder,Body) :- Binder,pfcdo(Body),fail.
pfcForEach(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- X,!.
pfcdo(_).


%= pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
%= L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-  
  pfcUnion(Tail,L,Tail2).


%= pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
%= arg3 is a simplified expression representing the conjunction of
%= args 1 and 2.

pfcConjoin(TRUE,X,X) :- is_true(TRUE),!.
pfcConjoin(X,TRUE,X) :- is_true(TRUE),!.
pfcConjoin(C1,C2,(C1,C2)).

% pfcFile('pfcsupport').	% support maintenance

% EOF
%%
%= predicates for manipulating support relationships
%%

:-decl_mpred_pfc(support3/3).
:-decl_mpred_pfc(spft/3).
:-decl_mpred_pfc(support2/3).

%= pfc_addSupport(+Fact,+Support)

pfc_addSupport(P,(Fact,Trigger)) :-
  pfcAssertS(spft(P,Fact,Trigger)),
  pfcAssertS(support3(Fact,Trigger,P)),
  pfcAssertS(support2(Trigger,P,Fact)).

pfcGetSupportORNil(P,Support):- (pfcGetSupport(P,Support) *-> true ; Support = (fail)).
% for a litteral
pfcGetSupport1(P,(Fact,Trigger)) :-
   nonvar(P)         -> spft(P,Fact,Trigger) 
   ; nonvar(Fact)    -> support3(Fact,Trigger,P) 
   ; nonvar(Trigger) -> support2(Trigger,P,Fact) 
   ; otherwise       -> spft(P,Fact,Trigger).

pfcGetSupport(P,More):- pfc_fully_expand_warn(is_asserted,P,PS),P \=@= PS,!,pfcGetSupport(PS,More).
pfcGetSupport((P1,P2),((F1,F2),(T1,T2))):-nonvar(P1),!,pfcGetSupport(P1,(F1,T1)),pfcGetSupport(P2,(F2,T2)).
pfcGetSupport(P,More):- pfcGetSupport1(P,More).
% TODO pack the T1 into T2 return value is still a (Fact,Trigger) pair

pfcWhy(G,Proof):-pfcGetSupport(G,S),(S=(G,G)->Proof=asserted;Proof=S).

pfcRemoveSupportItems(P,Types):-
  pfcGetSupport(P,Support),
  show_call(pfcFilterSupports(Support,Types,Results)),
  pfc_maptree(pfcRem1,Results).

pfcTypeFilter_l(ResultsO,Filter,ResultsO):-pfcTypeFilter(ResultsO,Filter).
pfcTypeFilter_l((Body,More),Filter,ResultsO):-!,pfcTypeFilter_l(Body,Filter,BodyO),pfcTypeFilter_l((More),Filter,(ResultsM)),conjoin(BodyO,ResultsM,ResultsO).
pfcTypeFilter_l(_,_Filter,!).

pfcFilterSupports(Support,Filter,ResultsO):- 
  pfcRuleOutcomeHeadBody(Support,_,Body),
  pfcTypeFilter_l(Body,Filter,ResultsO),!.

pfcFilterSupports(Support,Filter,ResultsO):-
  findall(Term, ((sub_term(Term,Support),pfcLiteralOrNeg(Term),compound(Term),pfcTypeFilter(Term,Filter))),Results),
  list_to_set(Results,ResultsO).

pfcTypeFilter(Term,Filter):- not(is_list(Filter)),!,pfcTypeFilter(Term,[Filter]).
pfcTypeFilter(Term,FilterS):- pfcTypeFull(Term,Type),memberchk(Type,FilterS),!.
pfcTypeFilter(Term,FilterS):- member(Filter,FilterS),append_term(Filter,Term,Call),current_predicate(_,Call),debugOnError(Call),!.


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

% pfcGetSupport2(P,(P,Trigger)) :- pfcPT(isa(A, tObj), pfcNT(mudPossess(B, A), pfcCall(mudPossess(B, A)), rhs([spatialInRegion(A)])))

is_support(E):-compound(E),functor(E,F,_),memberchk(F,[spft,support2,support3]).


get_support_for(P,PS):-thlocal:current_why(PS,P),!.
get_support_for(_,PS):-thlocal:current_why(PS,_),!.
get_support_for(P,PS):-copy_term(P,PS).

pfc_user_fact(V):-spft(V,User,User).

is_user_supported(V):-pfcGetSupport1(V,(u,u)).


get_user_support_for_lookup(_,(u,u)).
get_user_support_for_remove(_,(u,u)).
get_user_support_for_add(P,(u,u)):-!. % get_support_for(P,PS).

get_god_support_for_lookup(_,(pfcGod,pfcGod)).
is_deduced_by_god((pfcGod,pfcGod)).
%get_god_support_for_add(P,(pfcGod,pfcGod)):-!. % get_support_for(P,PS).


pfcRemSupport(_,(P,_)) :- is_support(P),!,ignore(pfcRetractOrWarn(pfcRemoveSupport,P)).
pfcRemSupport(P,_) :-is_support(P),!,ignore(pfcRetractOrWarn(pfcRemoveSupport,P)).
pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(P),
  !,
  pfcRetractOrWarn(pfcRemoveSupport,spft(P,Fact,Trigger)),
  pfcRetractOrWarn(pfcRemoveSupport,support3(Fact,Trigger,P)),
  pfcRetractOrWarn(pfcRemoveSupport,support2(Trigger,P,Fact)).


pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(Fact),
  !,
  pfcRetractOrWarn(pfcRemoveSupport,support3(Fact,Trigger,P)),
  pfcRetractOrWarn(pfcRemoveSupport,spft(P,Fact,Trigger)),
  pfcRetractOrWarn(pfcRemoveSupport,support2(Trigger,P,Fact)).

pfcRemSupport(P,(Fact,Trigger)) :-
  pfcRetractOrWarn(pfcRemoveSupport,support2(Trigger,P,Fact)),
  pfcRetractOrWarn(pfcRemoveSupport,spft(P,Fact,Trigger)),
  pfcRetractOrWarn(pfcRemoveSupport,support3(Fact,Trigger,P)).


pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  spft(P,F,T).

pfc_make_supports((P,S1,S2)) :- 
  pfc_addSupport(P,(S1,S2)),
  (pfc_addSome(P); true),
  !.

%= pfcTriggerKey(+Trigger,-Key) 
%%
%= Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey(pfcPT(Key,_),Key).
pfcTriggerKey(pfcPT3(Key,_,_),Key).
pfcTriggerKey(pfcNT(Key,_,_),Key).
pfcTriggerKey(Key,Key).


%%^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger pfcBase1 pfcClauseInt that stores the trigger.
%%

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).



% pfcFile('thlocal').	% predicates to manipulate database.


%   File   : thlocal.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%%	restore, reset, etc.0 )

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm(spft/3).
pfcDatabaseTerm(support3/3).
pfcDatabaseTerm(support2/3).
pfcDatabaseTerm(pfcPT3/3).
pfcDatabaseTerm(pfcNT/3).
pfcDatabaseTerm(pfcPT/2).
pfcDatabaseTerm(pfcBT/2).
pfcDatabaseTerm('=>'/2).
pfcDatabaseTerm('<=>'/2).
pfcDatabaseTerm('<='/2).
pfcDatabaseTerm(pfc_queue/1).
% CANT BE HERE OR IT DISABLED FWD CHAINING pfc DatabaseTerm(pfc Default/1).

% removes all forward chaining rules and pfcJustification_L from db.

pfcReset :-
  pfc_clause_db_unify(spft(P,F,Trigger),true),
  pfcRetractOrWarn(pfcReset,P),
  pfcRetractOrWarn(pfcReset,spft(P,F,Trigger)),
  pfcRetractOrWarn(pfcReset,support3(F,Trigger,P)),
  pfcRetractOrWarn(pfcReset,support2(Trigger,P,F)),
  fail.
pfcReset :-
  pfcDatabaseItem(T),
  pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]).
pfcReset.

% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  pfc_clause_db_unify(Term,_).

pfcRetractOrWarn(Why,X) :-  pfc_retract(Why,X), !.
pfcRetractOrWarn(Why,X) :- fail,compound(X),arg(_,X,E),compound(E),functor(E,F,_),member(F,[spft,support2,support3]),!,
  pfcError("~w couldn't pfc_retract ~p.",[Why,X]),dtrace.

pfcRetractOrWarn(Why,X) :- 
  pfc_warn("~w couldn't pfc_retract ~p.",[Why,X]).



% pfcFile('pfcdebug').	% debugging aids (e.g. tracing).


%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- decl_mpred_pfc pfc_settings/3.

:- pfc_setting_default(warnings,default,true).

%= predicates to examine the state of pfc

pfc_queue :- listing(pfc_queue/1).

pfcPrintDB :- current_predicate(must_det_l/1),!,
  must_det_l([
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
   pfcPrintSupports,
   pfc_queue]),!.

pfcPrintDB :-
   pfcPrintFacts,
   pfcPrintRules,
   pfcPrintTriggers,
  pfcPrintSupports,
  pfc_queue,!.

%= pfcPrintFacts ..

pfcPrintFacts :- pfcPrintFacts(_,true).

pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-
  pfcFacts(P,C,L),
  must(pfcClassifyFacts(L,User,Pfc,_Rule)),!,
  fmt("User added facts:",[]),
  must(pfcPrintitems(User)),
  fmt("Pfc added facts:",[]),
  must(pfcPrintitems(Pfc)).


%= printitems clobbers it's arguments - beware (fixed .. it no longer clobers)!

pfcPrintitems(HIn):-copy_term(HIn,H),pfc_maptree(pfc_lambda([E],(numbervars(E,0,_),fmt(" ~q.~n",[E]))),H).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfc_db_type(H,rule),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  get_user_support_for_lookup(H,US),
  pfcGetSupport(H,US),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).


printHeadItems(Head):-ignore((bagof(Head,pfc_clause_db_unify(Head,true),R1),pfcPrintitems(R1))).
printHeadCallItems(Head):-ignore((bagof(Head,pfc_clause_db_unify(Head,true),R1),pfcPrintitems(R1))).

pfcPrintRules :-
  printHeadItems((P=>Q)),printHeadItems((P<=>Q)),printHeadItems((P<=Q)).

pfcPrintTriggers :-
  fmt("% Positive triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcPT(_,_))),
  fmt("% Negative triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcNT(_,_,_))),
  fmt("% Goal triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcBT(_,_))),!.

pfcPrintSupports :- 
  % temporary hack.
  setof((S > P), pfcGetSupport(P,S),L),
  pfcPrintitems(L).

pfcVerifyMissing(isa(I,D), isa(I,C), ((isa(I,C), {D==C});~isa(I,C))). 
pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});~mudColor(I,C))). 

pfcVerifyMissing(GC, GO, ((GO, {D==C});~GO) ):- 
       GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

pfcFreeLastArg(isa(I,C),neg(isa(I,C))):-nonvar(C),!.
pfcFreeLastArg(isa(I,C),(isa(I,F),C\=F)):-!.
pfcFreeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
pfcFreeLastArg(_G,false).

%= pfcFact(P) is true if fact P was asserted into the database via add.
pfcFact(P) :- no_repeats(pfcFact(P,true)).

%= pfcFact(P,C) is true if fact P was asserted into the database via
%= add and condition C is satisfied.  For example, we might do:
%= 
%=  pfcFact(X,pfc_user_fact(X))
%%

pfcFact(P,C) :- no_repeats(pfcFact0(P,C)).

pfcFact0(P,C) :- 
  pfcGetSupport(P,_),
  pfc_db_type(P,fact),
  pfc_call_prolog_native(fact,C).

%= pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

%= pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof(P,pfcFact(P,C),L).

brake(X) :-  X, break.

% EOF
%%
%= predicates providing a simple tracing facility
%%

pfcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(o,o)).

pfcTraceAdd(pfcPT(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfcTraceAdd(pfcNT(_,_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfcTraceAdd(P,S) :-
   pfcTraceAddPrint(P,S),
   pfcTraceBreak(P,S).

crazy_bad_fact(isa(not,tCol)).
crazy_bad_fact(isa(_,not)).
crazy_bad_fact(user:mpred_prop(_,pred_argtypes)).

pfcTraceAddPrint(P,S) :- crazy_bad_fact(P),retractall(tlbugger:show_must_go_on),!,trace_or_throw(crazy_pfcTraceAddPrint(P,S)).

pfcTraceAddPrint(P,S) :-
  \+ \+ pfc_get_setting(mpredTracing,P,true),
  !,
  copy_term(P,Pcopy),
  numbervars(Pcopy,0,_),
  get_user_support_for_lookup(P,PS),
  (S = PS
       -> pmsg("Adding (u) ~q",[Pcopy])
        ; pmsg("Adding (g) ~q",[Pcopy])).

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  pfc_settings(mpredSpying,P,add) -> 
   (copy_term(P,Pcopy),
    numbervars(Pcopy,0,_),
    wdmsg("Breaking on pfc_add(~w)",[Pcopy]),
    break)
   ; true.

pfcTraceRem(pfcPT(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem(pfcNT(_,_)) :-
  % hack for now - never trace triggers.
  !.

pfcTraceRem(P) :-
  (pfc_settings(mpredTracing,P,true) 
     -> pmsg('Removing ~w.',[P])
      ; true),
  (pfc_settings(mpredSpying,P,rem)
   -> (pmsg("Breaking on pfcRem1(~w)",[P]),
       break)
   ; true).


pfc_trace :- pfc_trace(_).

pfc_trace(Form) :-
  asserta(pfc_settings(mpredTracing,Form,true)).

pfc_trace(Form,Condition) :- 
  pfcAssertS((pfc_settings(mpredTracing,Form,true) :- Condition)).

pfcSpy(Form) :- pfcSpy(Form,[add,rem],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[add,rem],Condition) :-
  !,
  pfcSpy1(Form,add,Condition),
  pfcSpy1(Form,rem,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  pfcAssertS((pfc_settings(mpredSpying,Form,Mode) :- Condition)).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :- 
  pfc_clause_db_ref(pfc_settings(mpredSpying,Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- pfc_settings_retractall(mpredTracing,Form,true).

% needed:  pfcTraceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :- pfc_settings(trace_exec,default,true), !,pmsg(Msg, Args).
pfc_trace_msg(_Msg,_Args).

pfcWatch :- pfc_setting_change(trace_exec,default,true).

pfcNoWatch :- pfc_setting_change(trace_exec,default,false).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :- 
  sformat(S,Msg,Args),
  pmsg("ERROR/Pfc: ~s",[S]), % dtrace(S),
  !,
  trace_or_throw(S),!.


%%
%= These control whether or not warnings are printed at all.
%=   pfc_warn.
%=   nopfc_warn.
%%
%= These print a warning message if the flag pfc_warnings is set.
%=   pfc_warn(+Message)
%=   pfc_warn(+Message,+ListOfArguments)
%%

pfc_warn :- pfc_setting_change(warnings,default,true).

nopfc_warn :- pfc_setting_change(warnings,default,false).
 
pfc_warn(Msg) :-  pfc_warn(Msg,[]).

pfc_warn(_,_):- !.
pfc_warn(Msg,Args) :- 
  pfc_settings(warnings,default,true),
  !,
  sformat(S,Msg,Args),
  pmsg("WARNING/Pfc: ~s",[S]), % dtrace(S),
  !. 

pfc_warn(_,_).

%%
%= pfc_warnings/0 sets flag to cause pfc warning messages to print.
%= pfcNoWarnings/0 sets flag to cause pfc warning messages not to print.
%%

pfc_warnings :- pfc_setting_change(warnings,default,true).

pfcNoWarnings :- pfc_settings_retractall(warnings,default,_).



% pfcFile('pfcjust').	% predicates to manipulate pfcJustification_L.


%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc Justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

pfcJustificationDB(F,J) :- justSupports(F,J).

pfcJustification_L(F,Js) :- bagof(J,pfcJustificationDB(F,J),Js).

justSupports(F,J):- loop_check_nr(pfcGetSupport(F,J)).


%= pfcBase1(P,L) - is true iff L is a list of "base" facts which, taken
%= together, allows us to deduce P.  A pfcBase1 fact is an pfcAxiom (a fact 
%= added by the user or a raw Prolog fact (i.e. one w/o any support))
%= or an assumption.

pfcBase1(F,[F]) :- (pfcAxiom(F) ; pfcAssumptionBase(F)),!.

pfcBase1(F,L) :-
  % i.e. (reduce 'append (map 'pfcBase1 (justification f)))
  pfcJustificationDB(F,Js),
  pfcBases(Js,L).


%= pfcBases(L1,L2) is true if list L2 represents the union of all of the 
%= facts on which some conclusion in list L1 is based.

pfcBases([],[]).
pfcBases([X|Rest],L) :-
  pfcBase1(X,Bx),
  pfcBases(Rest,Br),
  pfcUnion(Bx,Br,L).
	
pfcAxiom(F) :- 
 (get_user_support_for_lookup(F,US), pfcGetSupport(F,US)); 
 (get_god_support_for_lookup(F,GS),pfcGetSupport(F,GS)).

%= an pfcAssumptionBase/1''s G was a failed goal, i.e. were assuming that our failure to 
%= prove P is a proof of not(P)

pfcAssumptionBase(P) :- pfc_negation(P,_).
   
%= pfcAssumptionsSet(X,As) if As is a set of assumptions which underly X.

pfcAssumptionsSet(X,[X]) :- pfcAssumptionBase(X).
pfcAssumptionsSet(X,[]) :- pfcAxiom(X).
pfcAssumptionsSet(X,L) :-
  pfcJustificationDB(X,Js),
  pfcAssumption1(Js,L).

pfcAssumption1([],[]).
pfcAssumption1([X|Rest],L) :-
  pfcAssumptionsSet(X,Bx),
  pfcAssumption1(Rest,Br),
  pfcUnion(Bx,Br,L).  


%= pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%= of the form
%%
%=     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%=          ^                         and has the form of
%=          [J11, J12,... J1n]      a list of proof trees.


% pfcChild(P,Q) is true iff P is an immediate justifier for Q.
% mode: pfcChild(+,?)

pfcChild(P,Q) :-
  pfcGetSupport(Q,(P,_)).

pfcChild(P,Q) :-
  pfcGetSupport(Q,(_,Trig)),
  pfc_db_type(Trig,trigger),
  pfcChild(P,Trig).

pfcChildren(P,L) :- bagof(C,pfcChild(P,C),L).

% pfcDescendant(P,Q) is true iff P is a justifier for Q.

pfcDescendant(P,Q) :- 
   pfcDescendant1(P,Q,[]).

pfcDescendant1(P,Q,Seen) :-
  pfcChild(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfcDescendant1(P,X,[X|Seen])).
  
pfcDescendants(P,L) :- 
  bagof(Q,pfcDescendant1(P,Q,[]),L).



% pfcFile('pfcwhy').	% interactive exploration of pfcJustification_L.



%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc pfcJustification_L.

% ***** predicates for brousing pfcJustification_L *****

:- use_module(library(lists)).

:-dynamic(pfcWhyMemory1/2).

pfcWhy :- 
  pfcWhyMemory1(P,_),
  pfcWhy(P).

pfcWhy(N) :-
  number(N),
  !,
  pfcWhyMemory1(P,Js),
  pfcWhyCommand(N,P,Js).

pfcWhy(P) :-
  pfcJustification_L(P,Js),
  retractall(pfcWhyMemory1(_,_)),
  assert(pfcWhyMemory1(P,Js)),
  pfcWhyBrouse(P,Js).

pfcWhy1(P) :-
  pfcJustification_L(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhyBrouse(P,Js) :-
  pfcShowJustifications(P,Js),
  pfcAskUser(' >> ',Answer),
  pfcWhyCommand(Answer,P,Js).

pfcWhyCommand(q,_,_) :- !.
pfcWhyCommand(h,_,_) :- 
  !,
  fmt("
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",[]).

pfcWhyCommand(N,_P,Js) :-
  float(N),
  !,
  pfcSelectJustificationNode(Js,N,Node),
  pfcWhy1(Node).

pfcWhyCommand(u,_,_) :-
  % u=up
  !.

pfcCommand(N,_,_) :-
  integer(N),
  !,
  fmt("~w is a yet unimplemented command.",[N]),
  fail.

pfcCommand(X,_,_) :-
 fmt("~w is an unrecognized command, enter h. for help.",[X]),
 fail.
  
pfcShowJustifications(P,Js) :-
  fmt("Justifications for ~w:",[P]),
  pfcShowJustification1(Js,1).

pfcShowJustification1([],_).

pfcShowJustification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  pfcShowJustifications2(J,N,1),
  N2 is N+1,
  pfcShowJustification1(Js,N2).

pfcShowJustifications2([],_,_).

pfcShowJustifications2([C|Rest],JustNo,StepNo) :- 
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  pmsg("    ~w.~w ~w",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  pfcShowJustifications2(Rest,JustNo,StepNext).

pfcAskUser(Msg,Ans) :-
  fmt0(Msg),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth(StepNo,Justification,Step).



% dcg_pfc: translation of dcg-like grammar rules into pfc rules.

:- dynamic(pfc_term_expansion_ok/0).
% :- use_module(library(strings)), use_module(library(lists)).

/*
pfc_file_expansion((P=>Q),(:- pfc_add((P=>Q)))).
%pfc_file_expansion((P=>Q),(:- pfc_add(('<='(Q,P))))).  % speed-up attempt
pfc_file_expansion(('<='(P,Q)),(:- pfc_add(('<='(P,Q))))).
pfc_file_expansion((P<=>Q),(:- pfc_add((P<=>Q)))).
pfc_file_expansion((RuleName :::: Rule),(:- pfc_add((RuleName :::: Rule)))).
pfc_file_expansion((=>P),(:- pfc_add(P))).
pfc_file_expansion((P -->> Q),(:- pfc_add(Rule))) :-
  pfc_translate_rule((P -->> Q), Rule).
pfc_file_expansion((P --*>> Q),(:- pfc_add(Rule))) :-
  pfc_translate_rule((P --*>> Q), Rule).
*/

:- multifile('term_expansion'/2).
% term_expansion(I,O):-pfc_file_expansion(I,O),I\=@=O, (pfc_term_expansion_ok -> true ; print_message(warning,pfc_file_expansion(I,O))).



pfc_translate_rule((LP-->>[]),H) :- !, pfc_t_lp(LP,_Id,S,S,H).

pfc_translate_rule((LP-->>RP),(H <= B)):-
   pfc_t_lp(LP,Id,S,SR,H),
   pfc_t_rp(RP,Id,S,SR,B1),
   pfc_tidy(B1,B).


pfc_translate_rule((LP--*>>[]),H) :- !, pfc_t_lp(LP,_Id,S,S,H).
pfc_translate_rule((LP--*>>RP),(B => H)):-
   pfc_t_lp(LP,Id,S,SR,H),
   pfc_t_rp(RP,Id,S,SR,B1),
   pfc_tidy(B1,B).

pfc_t_lp(X,Id,S,SR,ss(X,Id,(S \\ SR))) :- var(X),!.

pfc_t_lp((LP,List),Id,S,SR,ss(LP,Id,(S \\ List2))):- 
   !,
   append(List,SR,List2).

pfc_t_lp(LP,Id,S,SR,ss(LP,Id,(S \\ SR))).

pfc_t_rp(!,_Id,S,S,!) :- !.
pfc_t_rp([],_Id,S,S1,S=S1) :- !.
pfc_t_rp([X],Id,S,SR,ss(word(X),Id,(S \\ SR))) :- !.
pfc_t_rp([X|R],Id,S,SR,(ss(word(X),Id,(S \\ SR1)),RB)) :- 
  !, 
  pfc_t_rp(R,Id,SR1,SR,RB).
pfc_t_rp({T},_Id,S,S,{T}) :- !.
pfc_t_rp((T,R),Id,S,SR,(Tt,Rt)) :- !,
   pfc_t_rp(T,Id,S,SR1,Tt),
   pfc_t_rp(R,Id,SR1,SR,Rt).
pfc_t_rp((T;R),Id,S,SR,(Tt;Rt)) :- !,
   pfc_t_or(T,Id,S,SR,Tt),
   pfc_t_or(R,Id,S,SR,Rt).
pfc_t_rp(T,Id,S,SR,ss(T,Id,(S \\ SR))).

pfc_t_or(X,Id,S0,S,P) :-
   pfc_t_rp(X,Id,S0a,S,Pa),
 ( var(S0a), (\==(S0a,S)), !, S0=S0a, P=Pa;
   P=(S0=S0a,Pa) ).

pfc_tidy((P1;P2),(Q1;Q2)) :-
   !,
   pfc_tidy(P1,Q1),
   pfc_tidy(P2,Q2).
pfc_tidy(((P1,P2),P3),Q) :- 
   pfc_tidy((P1,(P2,P3)),Q).
pfc_tidy((P1,P2),(Q1,Q2)) :- 
   !,
   pfc_tidy(P1,Q1),
   pfc_tidy(P2,Q2).
pfc_tidy(A,A) :- !.

compile_pfcg :-
  ((retract((L -->> R)), pfc_translate_rule((L -->> R), PfcRule));
    (retract((L --*>> R)), pfc_translate_rule((L --*>> R), PfcRule))),
  pfc_add(PfcRule),
  fail.
compile_pfcg.

parse(Words) :- 
  parse(Words,Id),
  format(" sentence id = ~w",Id),
  show(Id,sentence(_X)).


parse(Words,Id) :- 
  gen_s_tag(Id),
  parse1(Words,Id),
  pfc_add(sentence(Id,Words)).

parse1([],_) :- !.
parse1([H|T],Id) :-
 do(pfc_add(ss(word(H),Id,([H|T] \\ T)))),
 parse1(T,Id).


showSentences(Id) :- showSentences(Id,_).

showSentences(Id,Words) :-
  sentence(Id,Words),
  pfc(ss(s(S),Id,(Words \\ []))),
  nl,write(S),
  fail.
showSentences(_,_).

do(X) :- call(X) -> true;true.

show(Id,C) :-
  pfc(ss(C,Id,A \\ B)),
  append(Words,B,A),
  format("~n ~w    :   ~w",[C,Words]),
  fail.


gen_s_tag(s(N2)) :-
  % var(_V),
  (retract(s_tag(N)); N=0),
  N2 is N+1,
  assert(s_tag(N2)).

make_term(ss(Constituent,Id,String),Term) :-
   Constituent =.. [Name|Args],
   name(Name,Name_string),
   name(Name2,[36|Name_string]),
   append([Name2|Args],[Id,String],Term_string),
   Term =.. Term_string.
% append([],X,X). append([H|T],L2,[H|L3]) :- append(T,L2,L3).


% user:term_expansion(A,B):- loop_check(pfc_file_expansion_lc(A,B)),A\=@=B.

user:term_expansion(A,B):- current_predicate(_,pfcExpansion_loaded),loop_check(pfc_file_expansion(A,B)),A\=@=B.

pfcExpansion_loaded.
% system:goal_expansion(P,O):- (\+ current_predicate(_,P)),O= pfcCall(P).
:- if_startup_script(with_assertions(thlocal:pfcExpansion,ensure_loaded(mpred_i_mpred_pfc_testing))).

:- if_startup_script(prolog).

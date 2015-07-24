% :-module(pfc,[pfc_assert/1,pfc_assert_fast/1,pfc_add/1,pfc_call/1,pfc_fwd/1,pfc_assert_fast/1,remove/1,pfc_rem1/1,pfc_rem2/1,pfc_file_expansion/2]).
%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure


% ======================= pfc_file('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                            bug in the recorded database.
%   Purpose: core Pfc predicates.


/* UNUSED TODAY

:- use_module(library(mavis)).
:- use_module(library(type_check)).
:- use_module(library(typedef)).
*/

:- use_module(library(lists)).

/*
:- use_module(library('pldoc/doc_process')).
:- use_module(library('pldoc/doc_colour')).
:- use_module(library('pldoc/doc_html')).
:- use_module(library('pldoc/doc_index')).
:- use_module(library('pldoc/doc_search')).
:- use_module(library('pldoc/doc_modes')).
:- use_module(library('pldoc/doc_man')).
:- use_module(library('pldoc/doc_wiki')).
:- use_module(library('pldoc/doc_util')).
:- use_module(library('pldoc/doc_modes')).
:- use_module(library('pldoc/doc_htmlsrc')).
:- use_module(library('pldoc/doc_access')).
:- use_module(library('pldoc/doc_pack')).
:- use_module(library('pldoc/doc_register')).
:- use_module(library('pldoc/doc_library')).

*/
% :- user:ensure_loaded(library(dra/tabling3/swi_toplevel)).

second_order(_,_):-fail.


:-multifile(user:rescan_pfc_hook/0).
:-dynamic(user:rescan_pfc_hook/0).

:- discontiguous(pfc_file_expansion_0/2).
/*
compiled(F/A):- dynamic(F/A),compile_predicates([F/A]).
:- compiled(('=>')/1).
:- compiled(('neg')/1).
:- compiled(('=>')/2).
:- compiled(('<=')/2).
:- compiled(('::::')/2).
:- compiled(('<=>')/2).
*/

has_functor(_):-!,fail.
has_functor(F/A):-!,atom(F),integer(A),!.
has_functor(C):- (\+ compound(C)),!,fail.
has_functor(C):-compound(C),\+is_list(C).

pfc_each_literal(P,E):-nonvar(P),P=(P1,P2),!,(pfc_each_literal(P1,E);pfc_each_literal(P2,E)).
pfc_each_literal(P,P). %:-conjuncts_to_list(P,List),member(E,List).

to_addable_form_wte(P0,P):-
    once(notrace(to_addable_form(P0,P));must(to_addable_form_wte(P0,P))),
    (P0\=@=P->pfc_debug_trace(to_addable_form(P0,P));true).

retract_eq_quitely((H:-B)):-ignore((clause(H,B,Ref),clause(HH,BB,Ref),H=@=HH,B=@=BB,!,erase(Ref))).
retract_eq_quitely((H)):-ignore((clause(H,true,Ref),clause(HH,BB,Ref),H=@=HH,BB==true,!,erase(Ref))).
assert_eq_quitely(H):-assert_if_new(H).

reduce_clause_from_fwd(H,H):- (\+compound(H)),!.
reduce_clause_from_fwd((H:-B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((B=>H),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H<=B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((B<=>H),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H<=>B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H,B),(HH,BB)):-!,reduce_clause_from_fwd(H,HH),reduce_clause_from_fwd(B,BB).
reduce_clause_from_fwd(H,H).

to_addable_form(I,I):-var(I),!.
to_addable_form(I,OOO):-is_list(I),!,maplist(to_addable_form,I,O),flatten(O,OO),!,reduce_clause_from_fwd(OO,OOO).
to_addable_form(I,OO):- current_predicate(logicmoo_i_term_expansion_file/0),once(fully_expand(_,I,II)),!,
 once((into_mpred_form(II,M),to_predicate_isas_each(M,O))),!,reduce_clause_from_fwd(O,OO).
to_addable_form(I,O):- findall(M,do_expand_args(isEach,I,M),IM),list_to_conjuncts(IM,M),to_predicate_isas_each(M,O),!.

to_predicate_isas_each(I,O):-to_predicate_isas(I,O).

to_predicate_isas(V,V):- (\+compound(V)),!.
to_predicate_isas([H|T],[HH|TT]):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
to_predicate_isas((H,T),(HH,TT)):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
%to_predicate_isas(I,I):-contains_term(S,I),nonvar(S),exact_args(S),!.
to_predicate_isas(I,O):-must(to_predicate_isas0(I,O)),!.

append_as_first_arg(C,I,V):-C=..[F|ARGS],V=..[F,I|ARGS].

to_predicate_isas0(V,V):- (\+compound(V)),!.
to_predicate_isas0({V},{V}):-!.
to_predicate_isas0(eXact(V),V):-!.
to_predicate_isas0(t(C,I),V):-atom(C)->V=..[C,I];(var(C)->V=t(C,I);append_as_first_arg(C,I,V)).
to_predicate_isas0(isa(I,C),V):-!,atom(C)->V=..[C,I];(var(C)->V=isa(I,C);append_as_first_arg(C,I,V)).
to_predicate_isas0(C,C):-exact_args(C),!.
to_predicate_isas0([H|T],[HH|TT]):-!,to_predicate_isas0(H,HH),to_predicate_isas0(T,TT),!.
to_predicate_isas0(C,CO):-C=..[F|CL],maplist(to_predicate_isas0,CL,CLO),!,CO=..[F|CLO].

:-source_location(F,_),asserta(absolute_source_location_pfc(F)).
exact_args(_):-!,fail.
exact_args(Q):-argsQuoted(Q).
exact_args(asserted(_)).
exact_args(retract_eq_quitely(_)).
exact_args(asserts_eq_quitely(_)).
exact_args(assertz_if_new(_)).
exact_args((_:-_)).
exact_args((_ =.. _)).
exact_args((:-( _))).
exact_args((A/B)):- (var(A);var(B)).
exact_args(pfc_add(_)).
exact_args(dynamic(_)).
exact_args(cwc).
exact_args(true).
% exact_args(C):-source_file(C,I),absolute_source_location_pfc(I).

pfc_is_tautology(Var):-var(Var).
pfc_is_tautology(V):- \+ \+ (copy_term_nat(V,VC),numbervars(VC),pfc_is_taut(VC)).

pfc_is_taut(A:-B):-!,pfc_is_taut(B=>A).
pfc_is_taut(A<=B):-!,pfc_is_taut(B=>A).
pfc_is_taut(A<=>B):-!,(pfc_is_taut(B=>A);pfc_is_taut(A=>B)).
pfc_is_taut(B=>A):- A==B,!.
pfc_is_taut((B,_)=>A):-nonvar(B),pfc_is_taut(B=>A),!.
pfc_is_taut((_,B)=>A):-nonvar(B),pfc_is_taut(B=>A),!.
pfc_is_taut(B=>(A,_)):-nonvar(A),pfc_is_taut(B=>A),!.
pfc_is_taut(B=>(_,A)):-nonvar(A),pfc_is_taut(B=>A),!.

loop_check_nr(CL):- loop_check(no_repeats(CL)).

user:decl_database_hook(Op,Hook):- loop_check_nr(pfc_provide_mpred_storage_op(Op,Hook)).

is_retract_first(one).
is_retract_first(a).

pfc_provide_mpred_storage_op(Op,(I1,I2)):-!,pfc_provide_mpred_storage_op(Op,I1),pfc_provide_mpred_storage_op(Op,I2).
pfc_provide_mpred_storage_op(Op,(=>(P))):-!,pfc_provide_mpred_storage_op(Op,P).
%pfc_provide_mpred_storage_op(change(assert,_AorZ),Fact):- loop_check_nr(pfc_addPreTermExpansion(Fact)).
% pfcRem1 to just get the first
pfc_provide_mpred_storage_op(change(retract,OneOrA),FactOrRule):- is_retract_first(OneOrA),!,
            loop_check_nr(pfc_rem1(FactOrRule)),
  ignore((ground(FactOrRule),pfc_rem2(FactOrRule))).
% pfc_rem2 should be forcefull enough
pfc_provide_mpred_storage_op(change(retract,all),FactOrRule):- loop_check_nr(pfc_rem2(FactOrRule)),!.
% pfc_provide_mpred_storage_op(is_asserted,FactOrRule):- nonvar(FactOrRule),!,loop_check_nr(clause_i(FactOrRule)).

pfc_clause_is_asserted_hb_nonunify(H,B):- clause( =>( B , H) , true).
pfc_clause_is_asserted_hb_nonunify(H,B):- clause( <=( H , B) , true).
pfc_clause_is_asserted_hb_nonunify(_,_):-!,fail.
pfc_clause_is_asserted_hb_nonunify(G, T   ):- T==true,!,hotrace(pfc_rule_hb(G,H,B)),G\=@=H,!,pfc_clause_is_asserted(H,B).
pfc_clause_is_asserted_hb_nonunify(H,(T,B)):- T==true,!,pfc_clause_is_asserted_hb_nonunify(H,B).
pfc_clause_is_asserted_hb_nonunify(H,(B,T)):- T==true,!,pfc_clause_is_asserted_hb_nonunify(H,B).
pfc_clause_is_asserted_hb_nonunify(H,B):- clause_u( <=( H , B) , true).
pfc_clause_is_asserted_hb_nonunify(H,B):- pfc_clause_is_asserted(H,B,_).

pfc_clause_is_asserted(H,B):- var(H),nonvar(B),!,fail.
pfc_clause_is_asserted(H,B):- has_cl(H) -> clause(H,B) ; pfc_clause_is_asserted_hb_nonunify(H,B).
%pfc_clause_is_asserted(H,B,Ref):- clause_u(H,B,Ref).


% pfcDatabaseGoal(G):-compound(G),get_functor(G,F,A),pfcDatabaseTerm(F/A).

user:provide_mpred_storage_clauses(pfc,H,B,Proof):-pfc_clause(H,B,Proof).

%pfc_clause('=>'(H),B,forward(Proof)):- nonvar(H),!, user:provide_mpred_storage_clauses(H,B,Proof).
%pfc_clause(H,B,forward(R)):- R=(=>(B,H)),clause(R,true).
pfc_clause(H,B,Why):-has_cl(H),clause(H,CL,R),pfc_pbody(H,CL,R,B,Why).
%pfc_clause(H,B,backward(R)):- R=(<=(H,B)),clause(R,true).
%pfc_clause(H,B,equiv(R)):- R=(<=>(LS,RS)),clause(R,true),(((LS=H,RS=B));((LS=B,RS=H))).
% pfc_clause(H,true, pfcTypeFull(R,Type)):-nonvar(H),!,pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcRuleOutcomeHead(R,H),clause(R,true),pfcTypeFull(R,Type),Type\=rule.
% pfc_clause(H,true, pfcTypeFull(R)):-pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcTypeFull(R,Type),Type\=rule,clause(R,true),once(pfcRuleOutcomeHead(R,H)).

pfc_pbody(_H,pfc_bc_only(_BC),_R,fail,deduced(backchains)):-!.
pfc_pbody(H,infoF(INFO),R,B,Why):-!,pfc_pbody_f(H,INFO,R,B,Why).
pfc_pbody(H,B,R,BIn,WHY):- is_true(B),!,BIn=B,get_why(H,H,R,WHY).
pfc_pbody(H,B,R,B,asserted(R,(H:-B))).

get_why(_,CL,R,asserted(R,CL)):- clause(spft(CL, U, U),true),!.
get_why(H,CL,R,deduced(R,WHY)):-pfc_get_support(H,WH)*->WHY=(H=WH);(pfc_get_support(CL,WH),WHY=(CL=WH)).


pfc_pbody_f(H,CL,R,B,WHY):- CL=(B=>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
pfc_pbody_f(H,CL,R,B,WHY):- CL=(HH<=B),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
pfc_pbody_f(H,CL,R,B,WHY):- CL=(HH<=>B),sub_term_eq(H,HH),get_why(H,CL,R,WHY).
pfc_pbody_f(H,CL,R,B,WHY):- CL=(B<=>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
pfc_pbody_f(H,CL,R,fail,infoF(CL)):- trace_or_throw(pfc_pbody_f(H,CL,R)).

sub_term_eq(H,HH):-H==HH,!.
sub_term_eq(H,HH):-each_subterm(HH,ST),ST==H,!.
sub_term_v(H,HH):-H=@=HH,!.
sub_term_v(H,HH):-each_subterm(HH,ST),ST=@=H,!.


pfc_rule_hb(Outcome,OutcomeO,AnteO):-pfc_rule_hb_0(Outcome,OutcomeO,Ante),pfc_rule_hb_0(Ante,AnteO,_).

pfc_rule_hb_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
pfc_rule_hb_0((Outcome1,Outcome2),OutcomeO,AnteO):-!,pfc_rule_hb(Outcome1,Outcome1O,Ante1),pfc_rule_hb(Outcome2,Outcome2O,Ante2),
                   conjoin(Outcome1O,Outcome2O,OutcomeO),
                   conjoin(Ante1,Ante2,AnteO).
pfc_rule_hb_0(Ante1=>Outcome,OutcomeO,(Ante1,Ante2)):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(Outcome<=Ante1,OutcomeO,(Ante1,Ante2)):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(Outcome<=>Ante1,OutcomeO,(Ante1,Ante2)):-pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(Ante1<=>Outcome,OutcomeO,(Ante1,Ante2)):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(_::::Outcome,OutcomeO,Ante2):-!,pfc_rule_hb_0(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(bt(Outcome,Ante1),OutcomeO,(Ante1,Ante2)):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(pt(Ante1,Outcome),OutcomeO,(Ante1,Ante2)):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(pk(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(nt(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(spft(Outcome,Ante1a,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(pfc_queue(Outcome,_),OutcomeO,Ante2):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
% pfc_rule_hb_0(pfc Default(Outcome),OutcomeO,Ante2):-!,pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0((Outcome:-Ante),Outcome,Ante):-!.
pfc_rule_hb_0(Outcome,Outcome,true).

pfc_add_minfo(G):-pfc_add_minfo(assertz_if_new,G).
pfc_add_minfo(How,(H:-True)):-is_true(True),must(nonvar(H)),!,pfc_add_minfo(How,H).
pfc_add_minfo(How,(H<=B)):- !,pfc_add_minfo(How,(H:-infoF(H<=B))),!,pfc_add_minfo(How,(H:-pfc_bc_only(H))),pfc_add_minfo_2(How,(B:-infoF(H<=B))).
pfc_add_minfo(How,(B=>H)):- !,pfc_add_minfo(How,(H:-infoF(B=>H))),!,pfc_add_minfo_2(How,(B:-infoF(B=>H))).
pfc_add_minfo(How,(B<=>H)):- !,pfc_add_minfo(How,(H:-infoF(B<=>H))),!,pfc_add_minfo(How,(B:-infoF(B<=>H))),!.
pfc_add_minfo(How,((A,B):-INFOC)):-pfc_is_info(INFOC),(nonvar(A);nonvar(B)),!,pfc_add_minfo(How,((A):-INFOC)),pfc_add_minfo(How,((B):-INFOC)),!.
pfc_add_minfo(How,((A;B):-INFOC)):-pfc_is_info(INFOC),(nonvar(A);nonvar(B)),!,pfc_add_minfo(How,((A):-INFOC)),pfc_add_minfo(How,((B):-INFOC)),!.
pfc_add_minfo(How,(~(A):-infoF(C))):-nonvar(C),nonvar(A),!,pfc_add_minfo(How,((A):-infoF((C)))). % call(How,(~(A):-infoF(C))).
pfc_add_minfo(How,(neg(A):-infoF(C))):-nonvar(C),nonvar(A),!,pfc_add_minfo(How,((A):-infoF((C)))). % call(How,(~(A):-infoF(C))).
pfc_add_minfo(How,(A:-INFOC)):-nonvar(INFOC),INFOC= pfc_bc_only(A),!,call(How,(A:-INFOC)),!.
pfc_add_minfo(How,bt(H,_)):-!,call(How,(H:-pfc_bc_only(H))).
pfc_add_minfo(How,nt(H,Test,Body)):-!,call(How,(H:-fail,nt(H,Test,Body))).
pfc_add_minfo(How,pt(H,Body)):-!,call(How,(H:-fail,pt(H,Body))).
pfc_add_minfo(How,(A0:-INFOC0)):- pfc_is_info(INFOC0), copy_term((A0:-INFOC0),(A:-INFOC)),!,must((pfc_rewrap_h(A,AA),imploded_copyvars((AA:-INFOC),ALLINFO), call(How,(ALLINFO)))),!.
%pfc_add_minfo(How,G):-dmsg(skipped_add_meta_facts(How,G)).
pfc_add_minfo(_,_).

:-export(pfc_add_minfo_2/2).
pfc_add_minfo_2(How,G):-pfc_add_minfo(How,G).

pfc_is_info(pfc_bc_only(C)):-nonvar(C),!.
pfc_is_info(infoF(C)):-nonvar(C),!.



:-dynamic(not_not/1).
pfc_rewrap_h(A,A):-nonvar(A),\+ is_static_pred(A).
pfc_rewrap_h(A,F):- functor(A,F,_),\+ is_static_pred(F),!.
pfc_rewrap_h(A,not_not(A)):-!.

fwc:-true.
bwc:-true.
wac:-true.
is_fc_body(P):-cwc, notrace(fwc==P ; (compound(P),arg(1,P,E),is_fc_body(E))),!.
is_bc_body(P):-cwc, notrace(bwc==P ; (compound(P),arg(1,P,E),is_bc_body(E))),!.
is_action_body(P):-cwc, notrace(wac==P ; (compound(P),arg(1,P,E),is_action_body(E))),!.


:-dynamic(use_presently/0).
:-multifile(pfc_default/1).
:-dynamic(pfc_default/1).

:-thread_local(pfc_debug_local/0).
:-dynamic(pfc_silient/0).
pfc_silient :- \+ pfc_debug_local.

pfc_debug_trace(A):-pfc_debug_trace('~q.~n',A).
pfc_debug_trace(_,_):-pfc_silient,!.
pfc_debug_trace(F,A):-wdmsg(F,A),!.

% user''s program''s database
assert_u(arity(prologHybrid,0)):-trace_or_throw(assert_u(arity(prologHybrid,0))).
assert_u(X):- show_call_success(clause_asserted(X)),!.
assert_u(X):- /*(X\=(_:-_)->true;dynamic(X)),*/ assert(X).
asserta_u(X):-asserta(X).
assertz_u(X):-assertz(X).
retract_u(X):-retract(X).
clause_u(H,B):- must(H\==true),clause(H,B).
clause_u(H,B,Ref):-must(H\==true),clause(H,B,Ref).


call_u(X):- pfc_call(X).

retractall_u(X):-retractall(X).


% prolog system database
assert_prologsys(X):-assert(X).
asserta_prologsys(X):-asserta(X).
assertz_prologsys(X):-assertz(X).
clause_prologsys(H,B):-clause(H,B).
clause_prologsys(H,B,Ref):-clause(H,B,Ref).
call_prologsys(X):-call(X).

% internal bookkeeping
assert_i(X):-assert_if_new(X).
asserta_i(X):-asserta_if_new(X).
assertz_i(X):-assertz_if_new(X).
retract_i(X):-retract(X).
clause_i(H,B):-clause(H,B).
clause_i(H,B,Ref):-clause(H,B,Ref).
call_i(X):-call(X).
retractall_i(X):-retractall(X).

map_literals(P,G):-map_literals(P,G,[]).

map_literals(_,H,_):-var(H),!. % skip over it
map_literals(_,[],_) :- !.
map_literals(Pred,(H,T),S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- pfc_literal(H),must(apply(Pred,[H|S])),!.
map_literals(_Pred,H,_S):- \+ compound(H),!. % skip over it
map_literals(Pred,H,S):-H=..List,!,map_literals(Pred,List,S),!.


map_unless(Test,Pred,H,S):- call(Test,H),ignore(apply(Pred,[H|S])),!.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ compound(H),!. % skip over it
map_unless(Test,Pred,(H,T),S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):-H=..List,!,map_unless(Test,Pred,List,S),!.

pfc_maptree(Pred,List):-pfc_maptree(Pred,List,[]).
pfc_maptree(Pred,H,S):-var(H),!,apply(Pred,[H|S]).
pfc_maptree(_,[],_) :- !.
pfc_maptree(Pred,(H,T),S):-!, pfc_maptree(Pred,H,S), pfc_maptree(Pred,T,S).
pfc_maptree(Pred,(H;T),S):-!, pfc_maptree(Pred,H,S) ; pfc_maptree(Pred,T,S).
pfc_maptree(Pred,[H|T],S):-!, apply(Pred,[H|S]), pfc_maptree(Pred,T,S).
pfc_maptree(Pred,H,S):-apply(Pred,[H|S]). 

:-use_module(library(logicmoo/util/rec_lambda)).


% used to annotate a predciate to indicate PFC support
:-multifile(infoF/1).
:-dynamic(infoF/1).
:-export(infoF/1).

%example pfcVerifyMissing(mpred_prop(I,D), mpred_prop(I,C), ((mpred_prop(I,C), {D==C});~mpred_prop(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});~mudColor(I,C))). 

pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example pfc_freeLastArg(mpred_prop(I,C),neg(mpred_prop(I,C))):-nonvar(C),!.
%example pfc_freeLastArg(mpred_prop(I,C),(mpred_prop(I,F),C\=F)):-!.
pfc_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
pfc_freeLastArg(_G,false).


pfcVersion(1.2).

:- meta_predicate pfcl_do(0).
:- meta_predicate pfc_fact(*,0).
:- meta_predicate foreachl_do(0,0).
:- meta_predicate brake(0).
:- meta_predicate fc_eval_action(0,*).
:- meta_predicate call_prologsys(0).
:- meta_predicate call_i(0).
:- meta_predicate call_u(0).
% :- meta_predicate prove_by_contradiction(0).
% :- meta_predicate time(0,*).

% ======================= pfc_file('pfcsyntax').	% operator declarations.

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<=>').
:- op(1050,xfx,('<=')).
:- op(1100,fx,('=>')).
:- op(1150,xfx,('::::')).


%=% initialization of global assertons

%= pfc_init_i/1 initialized a global assertion.
%=  pfc_init_i(P,Q) - if there is any fact unifying with P, then do
%=  nothing, else assert_db Q.

pfc_init_i(GeneralTerm,Default) :-
  clause_i(GeneralTerm,true) -> true ; assert_i(Default).

%= fcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfc_init_i(fcTmsMode(_), fcTmsMode(cycles)).

% Pfc Search strategy. pfc_search(X) where X is one of {direct,depth,breadth}
:- pfc_init_i(pfc_search(_), pfc_search(direct)).

% aliases
pfc_add(P):-pfc_assert(P).
pfc_add(P,S):-pfc_assert(P,S).
pfc_add_fast('$was_imported_kb_content$'(_, _)<=_):-!.
pfc_add_fast(P):-pfc_assert_fast(P).

%= pfc_assert/2 and pfc_post/2 are the main ways to assert_db new clauses into the
%= database and have forward reasoning done.

%= pfc_assert(P,S) asserts P into the user''s dataBase with support from S.
pfc_assert(P) :- 
  pfc_assert_fast(P).

pfc_assert(P,S) :- 
  pfc_assert_fast(P,S).


pfc_assert_fast(P0):-pfc_assert_fast(P0,(u,u)).

pfc_assert_fast((=>P),S) :- nonvar(P),!,
  pfc_assert_fast(P,S).

pfc_assert_fast(P0,S):- gripe_time(0.6,pfc_assert_fast_0(P0,S)).

pfc_assert_fast_0(P0,S):-
  must(to_addable_form_wte(P0,P)),
      (is_list(P)
        ->maplist(pfc_assert_fast_sp(S),P);
       pfc_assert_fast_sp(S,P)).

pfc_assert_fast_sp(S,P) :- must(to_addable_form_wte(P,PC)),must(nonvar(PC)),PC\=@=P,!,pfc_assert_fast_sp0(S,PC),!.
pfc_assert_fast_sp(S,P) :- pfc_assert_fast_sp0(S,P),!.
%pfc_assert_fast_sp(_,_).
pfc_assert_fast_sp(P,S) :- pfc_error("pfc_assert_fast(~w,~w) failed",[P,S]).


pfc_assert_fast_sp0(S,P) :-
   pfc_rule_hb(P,OutcomeO,_),
     loop_check_term((pfc_post1_sp(S,P),pfc_run_maybe),
     pfc_asserting(OutcomeO),
     (pfc_post1_sp(S,P),pfc_debug_trace(looped_outcome((P))))),!.



% pfc_post(+Ps,+S) tries to assert a fact or set of fact to the database.  For
% each fact (or the singelton) pfc_post1 is called. It always succeeds.

pfc_post([H|T],S) :-
  !,
  pfc_post1(H,S),
  pfc_post(T,S).
pfc_post([],_) :- !.
pfc_post(P,S) :-   
  pfc_post1(P,S).

% pfc_post1(+P,+S) tries to assert a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.
pfc_post1(pfc_assert(P0),S):-!,pfc_post1(P0,S).
pfc_post1(PI,SI):-copy_term(pfc_post1(PI,SI),pfc_post1(P0,S)),
  to_addable_form_wte(P0,P),
      (is_list(P)
        ->maplist(pfc_post1_sp(S),P);
       pfc_post1_sp(S,P)).

pfc_post1_sp(S,(P1,P2)) :- !,pfc_post1_sp(S,(P1)),pfc_post1_sp(S,(P2)).
pfc_post1_sp(S,[P1]) :- !,pfc_post1_sp(S,(P1)).
pfc_post1_sp(S,[P1|P2]) :- !,pfc_post1_sp(S,(P1)),pfc_post1_sp(S,(P2)).
pfc_post1_sp(_S,P) :- once((pfc_is_tautology(P),dumpST,dmsg(trace_or_throw(todo(error(pfc_is_tautology(P))))))),show_load_context,prolog,fail.

pfc_post1_sp(S,P) :- must(loop_check(pfc_post1_sp_0(S,P),true)),!.
% pfc_post1_sp(_,_).
pfc_post1_sp(S,P) :-  pfc_warn("pfc_post1(~w,~w) failed",[P,S]).

pfc_post1_sp_0(S,P) :-
  %= db pfc_add_db_to_head(P,P2),
  % pfc_remove_old_version(P),
  must(pfc_assert_fast_i(P,S)).

pfc_assert_fast_i(P,S):- loop_check(pfc_assert_fast_i_0(P,S),true).

% TOO UNSAFE 
%pfc_assert_fast_i_0(P,_):- functor(P,F,_),if_defined(singleValueInArg(F,SV)),arg(SV,P,V),is_relative(V),!,show_call(db_assert_sv(P)).
%pfc_assert_fast_i_0(P,_):- functor(P,_,SV),arg(SV,P,V),is_relative(V),!,show_call(db_assert_sv(P)).
pfc_assert_fast_i_0(P,S):-
   must(loop_check(pfc_post1_sp_3(S,P),true)),!.


is_already_supported(P,(S,T),(S,T)):- clause_asserted(spft(P,S,T)),!.
is_already_supported(P,_S,(u,u)):- clause_asserted(spft(P,u,u)),!.

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term(P,PC),spft(PC,_,_),P=@=PC,!.


maybe_did_support.


different_literal(Q,N,R):- 
 nonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A),
  (singleValuedInArg(F,N) -> 
    (arg(N,Q,Was),dif(Was,NEW),replace_arg(Q,N,NEW,R));
    ((arg(N,Q,Was),nonvar(Q)) -> (dif(Was,NEW),replace_arg(Q,N,NEW,R));
        (N=A,arg(N,Q,Was),dif(Was,NEW),replace_arg(Q,N,NEW,R)))).


pfc_unpost1_sp_3(S,P):- doall(pfc_rem2a(P,S)),!,pfc_unfwc(P).


pfc_post1_sp_3(ST, \+ P) :-!, pfc_unpost1_sp_3(ST,P).
pfc_post1_sp_3(ST, ~ P) :-!, pfc_unpost1_sp_3(ST,P).

pfc_post1_sp_3(ST,P):- maybe_did_support,is_already_supported(P,ST,_HOW),!.
pfc_post1_sp_3(S,P):- \+ pfc_unique_u(P),!,must(pfc_add_support(P,S)),!.
pfc_post1_sp_3(S,P):-
  must(assert_u(P)),
  must(pfc_add_support(P,S)),
  must(pfc_trace_add(P,S)),
  !,
  must(pfc_enqueue(P,S)),
  !.



% was nothing  pfc_current_db/1.
pfc_current_u(pfc_current).
pfc_current.


%=
%= pfc_add_db_to_head(+P,-NewP) talkes a fact P or a conditioned fact
%= (P:-C) and adds the Db context.
%=

pfc_add_db_to_head(P,NewP) :-
  pfc_current_u(Db),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).


% pfc_unique_u(X) is true if there is no assertion X in the prolog db.

pfc_unique_u((Head:-Tail)) :-
  !,
  \+ clause_u(Head,Tail).
pfc_unique_u(P) :-
  !,
  \+ clause_u(P,true).


pfc_unique_i((Head:-Tail)) :-
  !,
  \+ clause_i(Head,Tail).
pfc_unique_i(P) :-
  !,
  \+ clause_i(P,true).


pfc_enqueue(P,S) :-
  must(pfc_search(Mode))
    -> (Mode=direct  -> must(pfc_fwd(P,S)) ;
	Mode=depth   -> pfc_asserta_i(pfc_queue(P,S),S) ;
	Mode=breadth -> pfc_assertz_i(pfc_queue(P,S),S) ;
	% else
          otherwise           -> pfc_warn("Unrecognized pfc_search mode: ~w", Mode))
     ; pfc_warn("No pfc_search mode").


% if there is a rule of the form Identifier ::: Rule then delete it.

pfc_remove_old_version((Identifier::::Body)) :-
  % this should never happen.
  var(identifier),
  !,
  pfc_warn("variable used as an  rule name in ~w :::: ~w",
          [Identifier,Body]).


pfc_remove_old_version((Identifier::::Body)) :-
  nonvar(Identifier),
  clause_u((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfc_rem1((Identifier::::OldBody)),
  !.
pfc_remove_old_version(_).

% pfc_run compute the deductive closure of the current database.
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfc_queue mechanism.

pfc_run_maybe:-!, pfc_run.
pfc_run_maybe :- (X is random(5)),X<4,!.
pfc_run_maybe :-
  (\+ pfc_search(direct)),
  pfc_step,
  pfc_run_maybe.
pfc_run_maybe.

pfc_run:-loop_check(pfc_run0,true).

pfc_run0 :-
  (\+ pfc_search(direct)),
  pfc_step,
  pfc_run0.
pfc_run0.


pfc_run_queued:- repeat,sleep(1.0),pfc_run,fail.
:-thread_property(X,alias(pfc_running_queue))-> true ; thread_create(pfc_run_queued,_,[alias(pfc_running_queue)]).


% pfc_step removes one entry from the pfc_queue and reasons from it.

pfc_step:-loop_check(pfc_step0,true).

pfc_step0 :-
  % if pfc_halt_signal(Signal) is true, reset it and fail, thereby stopping inferencing.
  pfc_retract_db_type(pfc_halt_signal(Signal)),
  !,
  pfc_warn("~N% Stopping on signal ~w",[Signal]),
  fail.

pfc_step0 :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P,S),
  pfcl_do(pfc_fwd(P,S)),
  !.

get_next_fact(P,WS) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P,WS),
  remove_selection(P,WS).

remove_selection(P,S) :-
  pfc_retract_db_type(pfc_queue(P,S)),
  pfc_remove_supports_quietly(pfc_queue(P,S)),
  !.
remove_selection(P,S) :-
  brake(fmt("~N% pfc:get_next_fact - selected fact not on Queue: ~w (~w)",
               [P,S])).


% select_next_fact(P) identifies the next fact to reason from.
% It tries the user defined predicate first and, failing that,
%  the pfc_default mechanism.

select_next_fact(P,S) :-
  pfc_select(P,S),
  !.
select_next_fact(P,S) :-
  defaultpfc_select(P,S),
  !.

% the pfc_default selection predicate takes the item at the froint of the queue.
defaultpfc_select(P,S) :- pfc_queue(P,S),!.

% pfc_halt stops the forward chaining.
pfc_halt :-  pfc_halt("",[]).

pfc_halt(Format) :- pfc_halt(Format,[]).

pfc_halt(Format,Args) :-
  sformat(S,Format,Args),
  !,
  fmt('~N% ~w~n',[S]),
  (pfc_halt_signal(Signal) ->
       pfc_warn("pfc_halt finds pfc_halt_signal(Signal) already set to ~w",[Signal])
     ; assert_i(pfc_halt_signal(S))).


%=
%=
%= predicates for manipulating triggers
%=


pfc_add_trigger(pt(Trigger,Body),Support) :-
  !,
  pfc_trace_msg('~N%       Adding p-trigger ~q~n', 
    [pt(Trigger,Body)]),
  (clause_asserted(pt(Trigger,Body)) -> true ;
   ( \+ \+ pfc_assert_i(pt(Trigger,Body),Support),
     must(pfc_mark_as(Support,p,Trigger,pfcPosTrigger)),
     copy_term(pt(Trigger,Body),Tcopy), !,
     call_u(Trigger),
     debugOnError(pfc_eval_lhs(Body,(Trigger,Tcopy))),
     fail)).



pfc_add_trigger(nt(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('~N%       Adding n-trigger: ~q~n       test: ~q~n%body: ~q~n',
		[Trigger,Test,Body]),
   pfc_mark_as(Support,n,Trigger,pfcNegTrigger),
  copy_term(Trigger,TriggerCopy),
  pfc_assert_i(nt(TriggerCopy,Test,Body),Support),
  \+ call_u(Test),
  pfc_eval_lhs(Body,((\+Trigger),nt(TriggerCopy,Test,Body))).


pfc_add_trigger(bt(Trigger,Body),Support) :-
  !,
   must(pfc_assert_i(bt(Trigger,Body),Support)),
   pfc_trace_msg('~N%       Adding b-trigger ~q~n',   [bt(Trigger,Body)]),  
   assertz_if_new((Trigger:-pfc_bc_only(Trigger))),
   must(pfc_mark_as(Support,b,Trigger,pfcBcTrigger)),
  % WAS pfc_bt_pt_combine(Trigger,Body).
  pfc_bt_pt_combine(Sup,Trigger,Body,Support).

pfc_add_trigger(X,Support) :-
  pfc_warn("Unrecognized trigger to pfc_addtrigger: ~w",[X:pfc_add_trigger(X,Support)]).


pfc_bt_pt_combine(_Sup,Head,Body,Support) :-
  %= a backward trigger (bt) was just added with head and Body and support Support
  %= find any pt''s with unifying heads and assert the instantied bt body.
  pfc_get_trigger_quick(pt(Head,_PtBody)),
  pfc_eval_lhs(Body,Support),
  fail.
pfc_bt_pt_combine(_Sup,_,_,_) :- !.


pfc_get_trigger(Trigger) :-  clause_i(Trigger,true).

pfc_get_trigger_quick(Trigger) :-  clause_i(Trigger,true).

%=
%=
%= predicates for manipulating action traces.
%=

pfc_add_actiontrace(Action,Support) :-
  % adds an action trace and it''s support.
  pfc_add_support(pfc_action(Action),Support).

pfc_rem_actiontrace(pfc_action(A)) :-
  pfc_undo_method(A,M),
  M,
  !.


%=
%= predicates to remove pfc facts, triggers, action traces, and queue items
%= from the database.
%=
%= was simply:  pfc_retract
pfc_retract_db_type(X) :-
  %= retract an arbitrary thing.
  pfc_db_type(X,Type),
  pfc_retract_db_type(Type,X),
  !.


pfc_retract_db_type(_,pfc_queue(P,S)) :-
  doall(retract_u(pfc_queue(P,_))),
  ignore(pfc_unfwc(pfc_queue(P,S))).


pfc_retract_db_type(fact,X) :-
  %= db pfc_add_db_to_head(X,X2), retract(X2).
  retract_u(X),
  ignore(pfc_unfwc(X)).

pfc_retract_db_type(rule,X) :-
  %= db  pfc_add_db_to_head(X,X2),  retract(X2).
  retract_u(X).

pfc_retract_db_type(trigger,X) :-
  retract_i(X)
    -> pfc_unfwc(X)
     ; pfc_warn("Trigger not found to retract: ~w",[X]).

pfc_retract_db_type(action,X) :- pfc_rem_actiontrace(X).


%= pfc_add_db_type(X) adds item X to some database
%= was simply:  pfc_Add
pfc_add_db_type(X) :-
  % what type of X do we have?
  pfc_db_type(X,Type),
  % call the appropriate predicate.
  pfc_add_db_type(Type,X).

pfc_add_db_type(fact,X) :-
  pfc_unique_u(X),
  assert_u(X),!.
pfc_add_db_type(rule,X) :-
  pfc_unique_i(X),
  assert_i(X),!.
pfc_add_db_type(trigger,X) :-
  assert_i(X).
pfc_add_db_type(action,_Action) :- !.


%= pfc_rem1(P,S) removes support S from P and checks to see if P is still supported.
%= If it is not, then the fact is retracted from the database and any support
%= relationships it participated in removed.

pfc_rem1(List) :-
  % iterate down the list of facts to be pfc_rem1'ed.
  nonvar(List),
  List=[_|_],
  rem_list(List).

pfc_rem1(P) :-
  % pfc_rem1/1 is the user''s interface - it withdraws user support for P.
  pfc_rem1(P,(u,u)).

rem_list([H|T]) :-
  % pfc_rem1 each element in the list.
  pfc_rem1(H,(u,u)),
  rem_list(T).

pfc_rem1(P,S) :- pfc_run,
  % pfc_debug_trace(fmt("~N% removing support ~w from ~w",[S,P])),
  pfc_trace_msg('~N%     Removing support: ~q from ~q~n',[S,P]),
  pfc_rem_support(P,S)
     -> remove_if_unsupported(P)
      ; pfc_warn("pfc_rem1/2 Could not find support ~w to remove from fact ~w",
                [S,P]).

%=
%= pfc_rem2 is like pfc_rem1, but if P is still in the DB after removing the
%= user''s support, it is retracted by more forceful means (e.g. remove).
%=

pfc_rem2a(P) :- pfc_run,
  % pfc_rem2/1 is the user''s interface - it withdraws user support for P.
  pfc_rem2a(P,(u,u)).

pfc_rem2a(P,S) :-
  pfc_rem1(P,S),
  % used to say pfc_call(P) but that meant it was 
  % was no_repeats(( pfc_call_with_triggers(P);pfc_call_with_no_triggers(P)))
  (( pfc_call(P) )  
     -> (pfc_remove3(P))
      ; true).

% prev way
% pfc_rem2(P):-!,pfc_rem2a(P).
% new way
pfc_rem2(P):-pfc_rem2a(P),pfc_unfwc(P).

% help us choose
pfc_rem(P) :-pfc_rem2a(P),pfc_unfwc(P).


%=
%= pfc_remove3(+F) retracts fact F from the DB and removes any dependent facts */
%=

pfc_remove3(F) :-
  show_call(pfc_remove_supports_f_l(F)),
  pfc_undo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfc_remove_supports_f_l(F) :-
  pfc_rem_support(F,S),
  (S=(z,z)->true;pfc_warn("~w was still supported by ~w",[F,S])),
  fail.
pfc_remove_supports_f_l(_).

pfc_remove_supports_quietly(F) :-
  pfc_rem_support(F,_),
  fail.
pfc_remove_supports_quietly(_).

% pfc_undo(X) undoes X.


pfc_undo(pfc_action(A)) :-
  % undo an action by finding a method and successfully executing it.
  !,
  pfc_rem_actiontrace(pfc_action(A)).

pfc_undo(pk(Key,Head,Body)) :-
  % undo a positive trigger.
  %
  !,
  (retract_i(pk(Key,Head,Body))
    -> pfc_unfwc(pt(Head,Body))
     ; pfc_warn("Trigger not found to retract: ~w",[pt(Head,Body)])).

pfc_undo(nt(Head,Condition,Body)) :-
  % undo a negative trigger.
  !,
  (retract_i(nt(Head,Condition,Body))
    -> pfc_unfwc(nt(Head,Condition,Body))
     ; pfc_warn("Trigger not found to retract: ~w",[nt(Head,Condition,Body)])).

pfc_undo(Fact):- pfc_undo_u(Fact)*->true;pfc_undo_e(Fact).

pfc_undo_u(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract_u(Fact),
     must(pfc_trace_rem(Fact)),
     pfc_unfwc1(Fact).

pfc_undo_e(Fact) :- 
     pfc_debug_trace("Fact not found in user db: ~w",[Fact]),
     pfc_trace_rem(Fact),
     pfc_unfwc(Fact).


%= pfc_unfwc(P) "un-forward-chains" from fact f.  That is, fact F has just
%= been removed from the database, so remove all support relations it
%= participates in and check the things that they support to see if they
%= should stay in the database or should also be removed.

pfc_unfwc(F) :-
  pfc_retract_support_relations(F),
  pfc_unfwc1(F).

pfc_unfwc1(F) :-
  pfc_unfwc_check_triggers(_Sup,F),
  % is this really the right place for pfc_run<?
  pfc_run_maybe.

pfc_unfwc_check_triggers(_Sup,F) :-
  pfc_db_type(F,fact),
  copy_term(F,Fcopy),
  nt(Fcopy,Condition,Action),
  (\+ Condition),
  G = pfc_eval_lhs(Action,((\+F),nt(F,Condition,Action))),
  loop_check(G,wdmsg(unfwc_caught_loop(G))),
  fail.
pfc_unfwc_check_triggers(_Sup,_).

pfc_retract_support_relations(Fact) :-
  pfc_db_type(Fact,Type),
  (Type=trigger -> pfc_rem_support(P,(_,Fact))
                ; pfc_rem_support(P,(Fact,_))),
  remove_if_unsupported(P),
  fail.
pfc_retract_support_relations(_).

%= remove_if_unsupported(+P) checks to see if P is supported and removes
%= it from the DB if it is not.

remove_if_unsupported(P) :-
   pfc_tms_supported(P) -> true ;  pfc_undo(P).


%= pfc_tms_supported(+P) succeeds if P is "supported". What this means
%= depends on the TMS mode selected.

pfc_tms_supported(P) :-
  fcTmsMode(Mode),
  pfc_tms_supported(Mode,P).

pfc_tms_supported(deep,P) :- !, pfc_deep_support(P).
pfc_tms_supported(local,P) :- !, pfc_get_support(P,_). % ,sanity(pfc_deep_support(S)).
pfc_tms_supported(cycles,P) :-  !, wellFounded(P).
pfc_tms_supported(_,_P) :- true.

% user:hook_one_minute_timer_tick:- statistics.


pfc_scan_tms(P):-pfc_get_support(P,(S,SS)),(S\=SS),once((pfc_deep_support(P)->true;dmsg(warn(now_maybe_unsupported(pfc_get_support(P,(S,SS)),fail))))).

user_atom(u).
user_atom(g).
user_atom(m).
user_atom(d).

pfc_deep_support(unbound):-!,fail.
pfc_deep_support(M):-loop_check(pfc_deep_support0(M),fail).

pfc_deep_support0((U,U)):-user_atom(U),!.
pfc_deep_support0((A=>_)):-!,pfc_deep_support(A).
pfc_deep_support0(pt(A,B)):-!,pfc_deep_support(A),pfc_deep_support(B).
pfc_deep_support0((A->B)):-!,pfc_deep_support(A),pfc_deep_support(B).
pfc_deep_support0((A/B)):-!,pfc_deep_support(A),pfc_deep_support(B).
pfc_deep_support0((A,B)):-!,pfc_deep_support(A),pfc_deep_support(B).
pfc_deep_support0(rhs(P)):-!,maplist(pfc_deep_support,P).
pfc_deep_support0(\+ call_u(P)):-!,pfc_call(\+ P).
pfc_deep_support0(call_u(P)):-!,pfc_call(P).
pfc_deep_support0({P}):-!,pfc_call(P).
pfc_deep_support0(P):-pfc_get_support(P,S),pfc_deep_support(S),!.
pfc_deep_support0(\+(P)):-!, pfc_call(\+(P)).
pfc_deep_support0(P):-user_atom(P),!.
pfc_deep_support0(P):-pfc_call(P).


%=
%= a fact is well founded if it is supported by the user
%= or by a set of facts and a rules, all of which are well founded.
%=

wellFounded(Fact) :- pfc_wff(Fact,[]).

pfc_wff(F,_) :-
  % supported by user (axiom) or an "absent" fact (assumption).
  (axiom(F) ; assumption(F)),
  !.

pfc_wff(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supports_f_l(F,Supporters),
  % all of whose members are well founded.
  pfc_wfflist(Supporters,[F|Descendants]),
  !.

%= pfc_wfflist(L) simply maps pfc_wff over the list.

pfc_wfflist([],_).
pfc_wfflist([X|Rest],L) :-
  pfc_wff(X,L),
  pfc_wfflist(Rest,L).

% supports_f_l(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [u].

supports_f_l(F,[Fact|MoreFacts]) :-
  pfc_get_support_precanonical_plus_more(F,(Fact,Trigger)),
  trigger_supports_f_l(Trigger,MoreFacts).

pfc_get_support_precanonical_plus_more(P,Sup):-pfc_get_support_one(P,Sup)*->true;((to_addable_form_wte(P,PE),pfc_get_support_one(PE,Sup))).
pfc_get_support_one(P,Sup):- pfc_get_support(P,Sup)*->true;(pfc_get_support_via_clause_db(P,Sup)*->true;pfc_get_support_via_sentence(P,Sup)).

pfc_get_support_via_sentence(Var,_):-var(Var),!,fail.
pfc_get_support_via_sentence((A,B),(FC,TC)):-!, pfc_get_support_precanonical_plus_more(A,(FA,TA)),pfc_get_support_precanonical_plus_more(B,(FB,TB)),conjoin(FA,FB,FC),conjoin(TA,TB,TC).
pfc_get_support_via_sentence(true,g):-!.

pfc_get_support_via_clause_db(\+ P,OUT):- pfc_get_support_via_clause_db(neg(P),OUT).
pfc_get_support_via_clause_db(\+ P,(naf(g),g)):- !, predicate_property(P,number_of_clauses(_)),\+ clause(P,Body).
pfc_get_support_via_clause_db(P,OUT):- (( predicate_property(P,number_of_clauses(N)),N>0,
   clause(P,Body),(Body==true->Sup=(g);(support_ok_via_clause(P),pfc_get_support_precanonical_plus_more(Body,Sup))))),
   OUT=(Sup,g).


support_ok_via_clause(H):- get_functor(H,F,A),support_ok_via_clause(H,F,A).
support_ok_via_clause(_,(\+),1):-!,fail.
support_ok_via_clause(H,F,A):- prologSideEffects(F),!,fail.
support_ok_via_clause(H,F,A):- \+ predicate_property(H,number_of_clauses(_)),!,fail.
support_ok_via_clause(H,F,A):- pfcMark(pfcRHS,_,F,A),!,fail.
support_ok_via_clause(H,F,A):- pfcMark(pfcMustFC,_,F,A),!,fail.
support_ok_via_clause(H,F,A):- prologDynamic(F),!.
support_ok_via_clause(H,F,A):- argsQuoted(F),!,fail.
support_ok_via_clause(H,F,A):- \+ pfcControlled(F),!.


pfc_get_support_precanonical(F,Sup):-to_addable_form_wte(F,P),pfc_get_support(P,Sup).
spft_precanonical(F,SF,ST):-to_addable_form_wte(F,P),!,spft(P,SF,ST).

trigger_supports_f_l(u,[]) :- !.
trigger_supports_f_l(Trigger,[Fact|MoreFacts]) :-
  pfc_get_support_precanonical_plus_more(Trigger,(Fact,AnotherTrigger)),
  trigger_supports_f_l(AnotherTrigger,MoreFacts).


%=
%=
%= pfc_fwd(X) forward chains from a fact or a list of facts X.
%=

pfc_fwd(P):-pfc_fwd(P,(u,u)).
pfc_fwd([H|T],S) :- !, pfc_fwd1(H,S), pfc_fwd(T,S).
pfc_fwd([],_) :- !.
pfc_fwd(P,S) :- pfc_fwd1(P,S).

% pfc_fwd1(+P) forward chains for a single fact.



pfc_fwd1(Fact,Sup) :- gripe_time(0.50,pfc_fwd2(Fact,Sup)).

pfc_fwd2(Fact,Sup) :- cyclic_term(Fact;Sup),writeq(pfc_fwd2_cyclic_term(Fact;Sup)),!.
pfc_fwd2(Fact,Sup) :-
  must(pfc_add_rule(Sup,Fact)),
  copy_term(Fact,F),
  % check positive triggers
  must(fcpt(Fact,F)),
  % check negative triggers
  must(fcnt(Fact,F)),!.


%=
%= pfc_add_rule(Sup,P) does some special, built in forward chaining if P is
%= a rule.
%=
pfc_add_rule(Sup,Fact):- 
  copy_term(Sup+Fact,SupC+FactC),
  cyclic_break((SupC+FactC)),
  pfc_add_rule0(SupC,FactC).

pfc_add_rule0(Sup,(P=>Q)) :-
  !,
  process_rule(Sup,P,Q,(P=>Q)).

pfc_add_rule0(Sup,(Name::::P=>Q)) :-
  !,
  process_rule(Sup,P,Q,(Name::::P=>Q)).

pfc_add_rule0(Sup,(P<=>Q)) :-
  !,
  process_rule(Sup,P,Q,(P<=>Q)),
  process_rule(Sup,Q,P,(P<=>Q)).

pfc_add_rule0(Sup,(Name::::P<=>Q)) :-
  !,
  process_rule(Sup,P,Q,((Name::::P<=>Q))),
  process_rule(Sup,Q,P,((Name::::P<=>Q))).

pfc_add_rule0(Sup,('<='(P,Q))) :-
  !,
  pfc_define_bc_rule(Sup,P,Q,('<='(P,Q))).

pfc_add_rule0(_Sup,_).


fcpt(Fact,F) :-
  pfc_get_trigger_quick(pt(F,Body)),
  (pfc_trace_exec->pp_item('Found ',pt(F,Body));true),
  pfc_eval_lhs(Body,(Fact,pt(F,Body))),
  fail.

fcpt(Fact,F) :- use_presently,
  pfc_get_trigger_quick(pt(presently(F),Body)),
  pp_item('Found presently ',pt(F,Body)),
  pfc_eval_lhs(Body,(presently(Fact),pt(presently(F),Body))),
  fail.

fcpt(_,_).

fcnt(_Fact,F) :-
  spft(X,_,nt(F,Condition,Body)),
  call_u(Condition),
  pfc_rem1(X,(_,nt(F,Condition,Body))),
  fail.
fcnt(_,_).


%=
%= pfc_define_bc_rule(Sup,+Head,+Body,+Parent_rule) - defines a backward
%= chaining rule and adds the corresponding bt triggers to the database.
%=

pfc_define_bc_rule(Sup,Head,Body,Parent_rule) :-
  (\+ pfc_literal(Head)),
  pfc_warn("~w Malformed backward chaining rule.  ~w not atomic.",[Sup,(Head:-Body)]),
  pfc_warn("rule: ~w",[Parent_rule]),
 % !,
  dtrace(pfc_define_bc_rule(Sup,Head,Body,Parent_rule)),
  fail.

pfc_define_bc_rule(Sup,Head,Body,Parent_rule) :- 
  copy_term(Parent_rule,Parent_ruleCopy),
  assert_if_new(Head:-pfc_bc_only(Head)),
  build_rhs(Sup,Head,Rhs),
  foreachl_do(pfc_nf(Body,Lhs),
       (build_trigger(Support,Lhs,rhs(Rhs),Trigger),
        pfc_assert_fast(bt(Head,Trigger),(Parent_ruleCopy,u)))).

%=
%=
%= eval something on the LHS of a rule.
%=

pfc_eval_lhs(P,S):-loop_check(pfc_eval_lhs0(P,S)).

pfc_eval_lhs0((Test->Body),Support) :-
  !,
  (call_prologsys(Test) -> pfc_eval_lhs(Body,Support)),
  !.

pfc_eval_lhs0(rhs(X),Support) :-
   cyclic_break((X)),

  !,
  debugOnError(pfc_eval_rhs_0(X,Support)),
  !.

pfc_eval_lhs0(X,Support) :-
  cyclic_break((X)),
  debugOnError(pfc_db_type(X,trigger)),
  !,
  debugOnError(pfc_add_trigger(X,Support)),
  !.

%pfc_eval_lhs0(snip(X),Support) :-
%  snip(Support),
%  pfc_eval_lhs(X,Support).

pfc_eval_lhs0(_Sup,X,_) :-
  pfc_warn("Unrecognized item found in trigger body, namely ~w.",[X]).


%=
%= eval something on the RHS of a rule.
%=

pfc_eval_rhs_0([],_) :- !.
pfc_eval_rhs_0([Head|Tail],Support) :-
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs_0(Tail,Support).


pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 fc_eval_action(Action,Support).

pfc_eval_rhs1(pfc_action(Action),Support) :-
 % evaluable Prolog code.
 !,
 fc_eval_action(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfc_negation(P,N),
 !,
 pfc_rem1(N).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs_0([X|Xrest],Support).

pfc_eval_rhs1(added(Assertion),Support) :-
 % an assertion to be added.
 pfc_assert(Assertion,Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 pfc_post1(Assertion,Support).

pfc_eval_rhs1(X,_) :-
  pfc_warn("Malformed rhs of a rule: ~w",[X]).


%=
%= evaluate an action found on the rhs of a rule.
%=

fc_eval_action(Action,Support) :-
  call_prologsys(Action),
  (undoable(Action)
     -> pfc_add_actiontrace(Action,Support)
      ; true).


%=
%=
%=

trigger_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


trigger_trigger1(presently(Trigger),Body) :- use_presently,
  nonvar(Trigger),!,
  copy_term(Trigger,TriggerCopy),
  call_u(Trigger),
  pfc_eval_lhs(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfc_call(Trigger),
  pfc_eval_lhs(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.

%=
%= pfc_call(F) is true iff F is a fact available for forward chaining.
%= Note that this has the side effect [maybe] of catching unsupported facts and
%= assigning them support from God. (g,g)
%=
pfc_call(F):- debugOnError(no_repeats(loop_check(pfc_call_0(F),fail))). % loop_check(pfc_call_0(F)))).

lsting(L):-with_assertions(thlocal:pfc_listing_disabled,listing(L)).


pfc_call_0(Var):-var(Var),!,pfc_call_with_no_triggers(Var).
pfc_call_0(U:X):-U==user,!,pfc_call_0(X).
pfc_call_0(t(A,B)):-(atom(A)->true;no_repeats(arity(A,1))),ABC=..[A,B],pfc_call_0(ABC).
pfc_call_0(isa(B,A)):-(atom(A)->true;no_repeats(tCol(A))),ABC=..[A,B],pfc_call_0(ABC).
%pfc_call_0(t(A,B)):-!,(atom(A)->true;no_repeats(arity(A,1))),ABC=..[A,B],pfc_call_0(ABC).
pfc_call_0(t(A,B,C)):-!,(atom(A)->true;no_repeats(arity(A,2))),ABC=..[A,B,C],pfc_call_0(ABC).
pfc_call_0(t(A,B,C,D)):-!,(atom(A)->true;no_repeats(arity(A,3))),ABC=..[A,B,C,D],pfc_call_0(ABC).
pfc_call_0(t(A,B,C,D,E)):-!,(atom(A)->true;no_repeats(arity(A,4))),ABC=..[A,B,C,D,E],pfc_call_0(ABC).
pfc_call_0((C1,C2)):-!,pfc_call_0(C1),pfc_call_0(C2).
pfc_call_0(call(X)):- !, pfc_call_0(X).
pfc_call_0(\+(X)):- !, \+ pfc_call_0(X).
pfc_call_0(call_u(X)):- !, pfc_call_0(X).
pfc_call_0(G):-pfc_call_0(G,F,A).

pfc_call_0(G,F,A):-  (ground(G); \+ current_predicate(F/A) ; \+ (predicate_property(G,clause_count(CC)),CC>1)),  
                ignore((loop_check(call_with_bc_triggers(G)),maybeSupport(G,(g,g)),fail)),
                (\+ current_predicate(F/A),dynamic(F/A),multifile(F/A),fail).
pfc_call_0(G,F,A):- (pfc_call_with_no_triggers(G)).


:-thread_local thlocal:infBackChainPrevented/1.

call_with_bc_triggers(P) :- functor(P,F,A), \+thlocal:infBackChainPrevented(F/A), bt(P,Trigger), pfc_get_support(bt(P,Trigger),S),
  thlocal:no_side_effects(P),
  with_no_assertions(thlocal:infBackChainPrevented(F/A),pfc_eval_lhs(Trigger,S)).


pfc_call_with_no_triggers(U:X):-U==user,!,pfc_call_with_no_triggers(X).
pfc_call_with_no_triggers(F) :- 
  %= this (var(F)) is probably not advisable due to extreme inefficiency.
  (var(F)    ->  pfc_facts_and_universe(F) ; pfc_call_with_no_triggers_bound(F)).

pfc_call_with_no_triggers_bound(F) :- 
  show_call_failure(thlocal:no_side_effects(F)),
  (\+ current_predicate(_,F) -> fail;
  call_prologsys(F)).
  %= we check for system predicates as well.
  %has_cl(F) -> (clause_u(F,Condition),(Condition==true->true;call_u(Condition)));
  %call_prologsys(F).


pfc_bc_only(G):- pfc_negation(G,Pos),!, show_call(\+ pfc_bc_only(Pos)).
pfc_bc_only(G):- !,(pfc_call(G)).
%pfc_bc_only(G):- loop_check(no_repeats(pfcBC_NoFacts(G))).

%%
%= pfcBC_NoFacts(F) is true iff F is a fact available for backward chaining ONLY.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%= this Predicate should hide Facts from pfc_bc_only/1
%%
pfcBC_NoFacts(F):- pfcBC_NoFacts_TRY(F)*-> true ; (pfc_slow_search,pfcBC_Cache(F)).

pfc_slow_search.


ruleBackward(F,Condition):-ruleBackward0(F,Condition),Condition\=call_sysprolog(F).
%ruleBackward0(F,Condition):-clause_u(F,Condition),not(is_true(Condition);pfc_is_info(Condition)).
ruleBackward0(F,Condition):-'<='(F,Condition),not(is_true(Condition);pfc_is_info(Condition)).

:-dynamic('{}'/1).
{X}:-dmsg(legacy({X})),call_prologsys(X).


pfcBC_NoFacts_TRY(F) :- no_repeats(ruleBackward(F,Condition)),
  % neck(F),
  call_prologsys(Condition),
  maybeSupport(F,(g,g)).


pfcBC_Cache(F) :- pfc_call(F),
   ignore((ground(F),( (\+is_asserted_1(F)), maybeSupport(F,(g,g))))).


maybeSupport(P,_):-pfc_ignored(P),!.
maybeSupport(P,S):-( \+ ground(P)-> true;
  (predicate_property(P,dynamic)->pfc_add(P,S);true)).

pfc_ignored(argIsa(F, A, argIsaFn(F, A))).
pfc_ignored(genls(A,A)).
pfc_ignored(isa(tCol,tCol)).
%pfc_ignored(isa(W,tCol)):-if_defined(user:hasInstance_dyn(tCol,W)).
pfc_ignored(isa(W,_)):-compound(W),isa(W,pred_argtypes).
pfc_ignored(C):-clause_safe(C,true). 
pfc_ignored(isa(_,Atom)):-atom(Atom),atom_concat(ft,_,Atom),!.
pfc_ignored(isa(_,argIsaFn(_, _))).


has_cl(H):-predicate_property(H,number_of_clauses(_)).

% an action is undoable if there exists a method for undoing it.
undoable(A) :- pfc_undo_method(A,_).

%=
%=
%= defining fc rules
%=

%= pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form
%= Out.  It also does certain optimizations.  Backtracking into this
%= predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  must(pfc_nf1(LHS,List2)),
  pfc_nf_negations(List2,List).


%= pfc_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%= Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go
% away eventually when the P/Condition form is no longer used anywhere.

pfc_nf1(P/Cond,[(\+P)/Cond]) :- pfc_negated_literal(P), !.

pfc_nf1(P/Cond,[P/Cond]) :-  pfc_literal(P), !.

pfc_nf1({P},[{P}]) :-!.

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
  pfc_literal(P),
  !.

%=% shouln't we have something to catch the rest as errors?
pfc_nf1(Term,[Term]) :-
  pfc_warn("pfc_nf doesn't know how to normalize ~w",[Term]),!,fail.


%= pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
pfc_nf1_negation((P/Cond),[(\+(P))/Cond]) :- !.

pfc_nf1_negation((P;Q),NF) :-
  !,
  pfc_nf1_negation(P,NFp),
  pfc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

pfc_nf1_negation((P,Q),NF) :-
  % this code is not correct! tpfc_wff.
  !,
  pfc_nf1_negation(P,NF)
  ;
  (pfc_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

pfc_nf1_negation(P,[\+P]).


%= pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%= changing ~{...} to {\+...}
%=% ? is this still needed? tpfc_wff 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! tpfc_wff 3/27/90

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

pfc_nf_negation(Form,{\+ X}) :-
  nonvar(Form),
  Form=(~({X})),
  !.
pfc_nf_negation(Form,{\+ X}) :-
  nonvar(Form),
  Form=(neg({X})),
  !.
pfc_nf_negation(X,X).


%=
%= build_rhs(Sup,+Conjunction,-Rhs)
%=

build_rhs(_Sup,X,[X]) :-
  var(X),
  !.
build_rhs(_Sup,neg(X),[neg(X)]) :-
  var(X),
  !.

build_rhs(Sup,call(A),Rest) :- !, build_rhs(Sup,(A),Rest).
build_rhs(Sup,call_u(A),Rest) :- !, build_rhs(Sup,(A),Rest).

build_rhs(Sup,(A,B),[A2|Rest]) :-
  !,
  pfc_compile_rhsTerm(Sup,A,A2),
  build_rhs(Sup,B,Rest).

build_rhs(Sup,X,[X2]) :-
   pfc_compile_rhsTerm(Sup,X,X2).


pfc_compile_rhsTerm(Sup,P,P):-is_ftVar(P),!.
pfc_compile_rhsTerm(Sup,(P/C),((P0:-C0))) :- !,pfc_compile_rhsTerm(Sup,P,P0),build_code_test(Sup,C,C0).
pfc_compile_rhsTerm(Sup,I,O):-to_addable_form_wte(I,O), (\+ \+ pfc_mark_as(Sup,r,O,pfcRHS)),!.



pfc_mark_as(_,_,P,_):-var(P),!.
pfc_mark_as(Sup,_PosNeg,neg(P),Type):-pfc_mark_as(Sup,neg,P,Type).
pfc_mark_as(Sup,_PosNeg,\+(P),Type):-pfc_mark_as(Sup,neg,P,Type).
pfc_mark_as(Sup,_PosNeg,-(P),Type):-pfc_mark_as(Sup,neg,P,Type).
pfc_mark_as(Sup,_PosNeg,~(P),Type):-pfc_mark_as(Sup,neg,P,Type).
pfc_mark_as(Sup,PosNeg,P,Type):-get_functor(P,F,A),pfc_mark_fa_as(Sup,PosNeg,P,F,A,Type),!.

:- dynamic( pfcMark/4).

pfc_mark_fa_as(_Sup,_PosNeg,_P,F,A,Type):- pfcMark(Type,_,F,A),!.
pfc_mark_fa_as(_Sup,_PosNeg,_P,isa,_,_):- !.
pfc_mark_fa_as(_Sup,_PosNeg,_P,t,_,_):- !.
pfc_mark_fa_as(_Sup,_PosNeg,_P,argIsa,N,_):- !,must(N=3).
pfc_mark_fa_as(_Sup,_PosNeg,_P,arity,N,_):- !,must(N=2).
pfc_mark_fa_as(_Sup,_PosNeg,_P,pfcMark,_,_):- !,must(N=4).
pfc_mark_fa_as(_Sup,_PosNeg,_P,mpred_prop,N,_):- must(N=2).
pfc_mark_fa_as(_Sup,_PosNeg,_P,_:mpred_prop,N,_):- must(N=2).
pfc_mark_fa_as(Sup,PosNeg,_P,F,A,Type):- pfc_assert_fast_i(pfcMark(Type,PosNeg,F,A),(s(Sup),g)),!.


user:hook_one_minute_timer_tick:-pfc_cleanup.

pfc_cleanup:- forall((no_repeats(F-A,(pfcMark(pfcRHS,_,F,A),A>1))),pfc_cleanup(F,A)).

pfc_cleanup(F,A):-functor(P,F,A),predicate_property(P,dynamic)->pfc_cleanup_0(P);true.

pfc_cleanup_0(P):- findall(P-B-Ref,clause(P,B,Ref),L),forall(member(P-B-Ref,L),erase(Ref)),forall(member(P-B-Ref,L),assertz_if_new((P:-B))).

:-debug.
%isInstFn(A):-!,trace_or_throw(isInstFn(A)).

%= pfc_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

pfc_negation((~P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).

pfc_negated_literal(P) :- nonvar(P),
  pfc_negation(P,Q),
  pfc_literal(Q).

pfc_literal_nv(X):-nonvar(X),pfc_literal(X).
pfc_literal(X) :- cyclic_term(X),!,fail.
pfc_literal(X) :- pfc_negated_literal(X),!.
pfc_literal(X) :- pfc_positive_literal(X),!.
pfc_literal(X) :- var(X),!.

pfc_positive_literal(X) :- nonvar(X),
  functor(X,F,_),
  \+ pfc_connective(F).

pfc_connective(';').
pfc_connective(',').
pfc_connective('/').
pfc_connective('|').
pfc_connective(('=>')).
pfc_connective(('<=')).
pfc_connective('<=>').

pfc_connective('-').
pfc_connective('~').
pfc_connective('\\+').

cyclic_break(Cyclic):-cyclic_term(Cyclic)->(writeq(Cyclic),nl,prolog);true.

process_rule(Sup,Lhs,Rhs,Parent_rule) :- 
  copy_term(Parent_rule,Parent_ruleCopy),
  build_rhs((Parent_ruleCopy),Rhs,Rhs2),
   cyclic_break((Lhs)),
   cyclic_break((Rhs)),
  foreachl_do(pfc_nf(Lhs,Lhs2),
   build_rule(Lhs2,rhs(Rhs2),(Parent_ruleCopy,u))).

build_rule(Lhs,Rhs,Support) :-
  cyclic_break((Lhs)),
  cyclic_break((Rhs)),
  build_trigger(Support,Lhs,Rhs,Trigger),
  pfc_eval_lhs(Trigger,Support).

build_trigger(Support,[],Consequent,ConsequentO):- build_consequent(Support,Consequent,ConsequentO).

build_trigger(Support,[V|Triggers],Consequent,pt(V,X)) :-
  var(V),
  !,
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[(T1/Test)|Triggers],Consequent,nt(T2,Test2,X)) :-
  nonvar(T1),pfc_negation(T1,T2),
  !,
  build_neg_test(Support,T2,Test,Test2),
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[(T1)|Triggers],Consequent,nt(T2,Test,X)) :-
  pfc_negation(T1,T2),
  !,
  build_neg_test(Support,T2,true,Test),
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[T/Test|Triggers],Consequent,pt(T,X)) :-
  !,
  build_code_test(Support,Test,Test2),
  build_trigger(Support,[{Test2}|Triggers],Consequent,X).


%build_trigger(Support,[snip|Triggers],Consequent,snip(X)) :-
%  !,
%  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[T|Triggers],Consequent,pt(T,X)) :-
  !,
  build_trigger(Support,Triggers,Consequent,X).

%=
%= build_neg_test(Support,+,+,-).
%=
%= builds the test used in a negative trigger (nt/3).  This test is a
%= conjunction of the check than no matching facts are in the db and any
%= additional test specified in the rule attached to this ~ term.
%=

build_neg_test(Support,T,Testin,Testout) :-
  build_code_test(Support,Testin,Testmid),
  conjoin((call_u(T)),Testmid,Testout).


%= this just strips away any currly brackets.

build_code_test(Support,Test,TestO):-var(Test),!,TestO=call_u(Test).
build_code_test(Support,{Test},TestO) :- !,build_code_test(Support,Test,TestO).
build_code_test(Support,Test,TestO):- sentence_op(Test),Test=..[F|TestL],maplist(build_code_test(Support),TestL,TestLO),TestO=..[F|TestLO],!.
build_code_test(Support,Test,Test):-must(pfc_mark_as(Support,p,Test,pfcWatched)),!.
build_code_test(Support,Test,Test).

sentence_op(Var):-var(Var),!,fail.
sentence_op(rhs(_)).
sentence_op(neg(_)).
sentence_op(-(_)).
sentence_op(~(_)).
sentence_op(\+(_)).
sentence_op(call_u(_)).
sentence_op(Test):-predicate_property(Test,meta_predicate(_)),predicate_property(Test,built_in).

%=

build_consequent(Support,Test,Test):-var(Test),!.
build_consequent(Support,Test,TestO):-var(Test),!,TestO=added(Test).
build_consequent(Support,rhs(Test),rhs(TestO)) :- !,build_consequent(Support,Test,TestO).
build_consequent(Support,Test,TestO):- sentence_op(Test),Test=..[F|TestL],maplist(build_consequent(Support),TestL,TestLO),TestO=..[F|TestLO],!.
build_consequent(Support,Test,Test):-must(pfc_mark_as(Support,p,Test,pfcCreates)),!.
build_consequent(Support,Test,Test).

%= simple typeing for pfc objects

pfc_db_type(('=>'(_,_)),Type) :- !, Type=rule.
pfc_db_type(('<=>'(_,_)),Type) :- !, Type=rule.
pfc_db_type(('<='(_,_)),Type) :- !, Type=rule.
pfc_db_type((':-'(_,_)),Type) :- !, Type=rule.
pfc_db_type(pk(_,_,_),Type) :- !, Type=trigger.
pfc_db_type(pt(_,_),Type) :- !, Type=trigger.
pfc_db_type(nt(_,_,_),Type) :- !,  Type=trigger.
pfc_db_type(bt(_,_),Type) :- !,  Type=trigger.
pfc_db_type(pfc_action(_),Type) :- !, Type=action.
pfc_db_type((('::::'(_,X))),Type) :- nonvar(X),!, pfc_db_type(X,Type).
pfc_db_type(_,fact) :-
  %= if it''s not one of the above, it must be a fact!
  !.

pfc_assert_i(P0,Support0) :- 
  unnumbervars(P0+Support0,P+Support),
  (pfc_clause_i(P) ; assert_i(P)),
  !,
  pfc_add_support(P,Support).



pfc_asserta_i(P0,Support0) :- 
  unnumbervars(P0+Support0,P+Support),
  (pfc_clause_i(P) ; asserta_i(P)),
  !,
  pfc_add_support(P,Support).


pfc_assertz_i(P0,Support0) :- 
  unnumbervars(P0+Support0,P+Support),
  (pfc_clause_i(P) ; assertz_i(P)),
  !,
  pfc_add_support(P,Support).


pfc_clause_i((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  clause_i(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_clause_i(Head) :-
  % find a unit clause_db identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  clause_u(Head_copy,true),
  variant(Head,Head_copy).


foreachl_do(Binder,Body) :- Binder,pfcl_do(Body),fail.
foreachl_do(_,_).

% pfcl_do(X) executes X once and always succeeds.
pfcl_do(X) :- X,!.
pfcl_do(_).


%= pfc_union(L1,L2,L3) - true if set L3 is the result of appending sets
%= L1 and L2 where sets are represented as simple lists.

pfc_union([],L,L).
pfc_union([Head|Tail],L,Tail2) :-
  memberchk(Head,L),
  !,
  pfc_union(Tail,L,Tail2).
pfc_union([Head|Tail],L,[Head|Tail2]) :-
  pfc_union(Tail,L,Tail2).



% ======================= pfc_file('pfcsupport').	% support maintenance

%=
%=
%= predicates for manipulating support relationships
%=

%= pfc_add_support(+Fact,+Support)

%pfc_add_support(tCol(PC),(u,u)) :- PC== pathConnects, trace_or_throw(pfc_add_support(tCol(PC),(u,u))).
%pfc_add_support(P,(Fact,Trigger)) :-  clause_asserted( spft(P,Fact,Trigger)),!. % ,dmsg(dup(warn(pfc_add_support(P,(Fact,Trigger))))),!.
%pfc_add_support(P,(Fact,Trigger)) :-  assertz( spft(P,Fact,Trigger)),!. 
%pfc_add_support(P,(Fact,Trigger)) :-  once((IS= spft(P,Fact,Trigger), copy_term(IS,WAS),IS)),(IS=@=WAS->show_call_failure(clause_asserted( spft(P,Fact,Trigger)));
%     ( ignore((fail,numbervars(WAS),retract(WAS))),assertz(IS), nop(dmsg(warn([unify,pfc_add_support,WAS,IS]))))),!.
pfc_add_support(P,(Fact,Trigger)) :- 
   U=spft(P,Fact,Trigger),
   unnumbervars(U,UNNUMBEREDVARS),
   assertz_if_new(UNNUMBEREDVARS). % was assert_i



pfc_get_support(P,(Fact,Trigger)) :- spft(P,Fact,Trigger)*->true;(nonvar(P),pfc_get_support_neg(P,(Fact,Trigger))).

% dont pfc_get_support_neg(\+ neg(P),(Fact,Trigger)) :- spft((P),Fact,Trigger).
pfc_get_support_neg(\+ (P),S) :- !, nonvar(P), pfc_get_support(neg(P),S).
pfc_get_support_neg(~ (P),S) :- !, nonvar(P), pfc_get_support(neg(P),S).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfc_rem_support( N , S):- nonvar(N), N = (\+ P), pfc_rem_support(neg(P),S).

pfc_rem_support(P,(Fact,Trigger)) :-
  nonvar(P), !, pfc_retract_or_warn_i(spft(P,Fact,Trigger)).
pfc_rem_support(P,(Fact,Trigger)) :- nonvar(Fact), !, pfc_retract_or_warn_i(spft(P,Fact,Trigger)).
pfc_rem_support(P,(Fact,Trigger)) :- pfc_retract_or_warn_i(spft(P,Fact,Trigger)).


% TODO not called yet
pfc_collect_supports_f_l(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports_f_l([]).

/* UNUSED TODAY
% TODO not called yet
pfc_support_relation((P,F,T)) :- spft(P,F,T).

% TODO not called yet
pfc_make_supports_f_l((P,S1,S2)) :-
  % was pfc_add_support(P,(S1,S2),_),
  pfc_add_support(P,(S1,S2)),
  (pfc_add_db_type(P); true),
  !.
*/

is_relative(V):- (\+compound(V)),!,fail.
is_relative(+(_)).
is_relative(-(_)).
is_relative(N):-number(N),N =< 10, N >= -10.


% TODO not called yet
%= pfc_get_trigger_key(+Trigger,-Key)
%=
%= Arg1 is a trigger.  Key is the best term to index it on.

pfc_get_trigger_key(pt(Key,_),Key).
pfc_get_trigger_key(pk(Key,_,_),Key).
pfc_get_trigger_key(nt(Key,_,_),Key).
pfc_get_trigger_key(Key,Key).


%=^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger base clause that stores the trigger.
%=

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).

% ======================= pfc_file('pfcdb').	% predicates to manipulate database.

%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%=	restore, reset, etc).

% pfc_database_term(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfc_database_term(spft/3).
pfc_database_term(pk/3).
pfc_database_term(bt/2).  % was 3
pfc_database_term(nt/3). % was 4
pfc_database_term('=>'/2).
pfc_database_term('<=>'/2).
pfc_database_term('<='/2).
pfc_database_term(pfc_queue/2).


% removes all forward chaining rules and justifications from db.

pfc_reset :-
  clause_i(spft(P,F,Trigger),true),
  pfc_retract_or_warn_i(P),
  pfc_retract_or_warn_i(spft(P,F,Trigger)),
  fail.
pfc_reset :-
  pfc_database_item(T),
  pfc_error("Pfc database not empty after pfc_reset, e.g., ~p.~n",[T]).
pfc_reset.

% true if there is some pfc crud still in the database.
pfc_database_item(Term) :-
  pfc_database_term(P/A),
  functor(Term,P,A),
  clause_u(Term,_).

pfc_retract_or_warn_i(X) :-  retract_i(X), !.
pfc_retract_or_warn_i(X) :- 
  pfc_debug_trace("Couldn't retract ~p.",[X]),!.
pfc_retract_or_warn_i(_).

% ======================= pfc_file('pfcdebug').	% debugging aids (e.g. tracing).
%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh
%   for Pfc.

:- dynamic pfc_traced/1.
:- dynamic pfc_spied/2.
:- dynamic pfc_trace_exec/0.
:- dynamic pfc_warnings/1.


:- pfc_init_i(pfc_warnings(_), pfc_warnings(true)).

%= predicates to examine the state of pfc


pfc_queue :- lsting(pfc_queue/2).


pppfc :-
  pp_facts,
  pp_rules,
  pp_triggers,
  pp_supports,
  pfc_queue.

%= pp_facts ...

pp_facts :- pp_facts(_,true).

pp_facts(Pattern) :- pp_facts(Pattern,true).

pp_facts(P,C) :-
  pfc_facts(P,C,L),
  pfc_classify_facts(L,User,Pfc,_Rule),
  draw_line,
  fmt("~N% User added facts:",[]),
  pp_items(Type,User),
  draw_line,
  draw_line,
  fmt("~N% Pfc added facts:",[]),
  pp_items(Type,Pfc),
  draw_line.

draw_line:- (thlocal:print_mode(H)->true;H=unknown),fmt("~N%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]).


pp_items(Type,[]).
pp_items(Type,[H|T]) :-
  pp_item(Type,H),
  pp_items(Type,T).
pp_items(Type,H) :- pp_item(Type,H).

pp_item(M,H):- flag(show_asserions_offered,X,X+1),thlocal:print_mode(html),!, (\+ \+ pp_item_html(M,H)),!.
pp_item(M,H):- true,(\+ \+ pp_item_html(M,H)),!.
pp_item(M,(H:-true)):-pp_item(M,H).
pp_item(M,spft(W,U,U)):-!,pp_item(M,U:W).
pp_item(M,spft(W,F,U)):- atom(U),!, fmt('~N%~n',[]),pp_item(M,U:W), fmt('~N% rule: ~q~n%~n', [F]),!.
pp_item(M,spft(W,F,U)):-          !,fmt('~N% ~w~n%d:       ~q~n%format:    ~q~n', [M,W,F]),pp_item(M,U).
pp_item(M,U:W):- !,sformat(S,'~w  ~w:',[M,U]),!, pp_item(S,W).
pp_item(M,nt(Trigger,Test,Body)) :- !, fmt('~N%~sn-trigger: ~q~n%test: ~q~n%body: ~q~n', [M,Trigger,Test,Body]).
pp_item(M,pt(F,Body)):-              !,fmt('~N%~sp-trigger: ~q~n~n%body:~n', [M,F]), portray_clause((F:-Body)).
pp_item(M,bt(F,Body)):-              !,fmt('~N%~sb-trigger: ~q~n%body: ~q~n', [M,F,Body]).
pp_item(M,H):- \+ \+ ((  fmt("~N%~s ~q~n",[M,H]))).

pfc_classify_facts([],[],[],[]).

pfc_classify_facts([H|T],User,Pfc,[H|Rule]) :-
  pfc_db_type(H,rule),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],[H|User],Pfc,Rule) :-
  pfc_get_support(H,(u,u)),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],User,[H|Pfc],Rule) :-
  pfc_classify_facts(T,User,Pfc,Rule).


print_db_items(T, I):- 
    draw_line, 
    fmt("~N% ~w ...~n",[T]),
    print_db_items(I),
    draw_line.

print_db_items(F/A):-number(A),!,functor(P,F,A),!,print_db_items(P).
print_db_items(I):- bagof(I,clause_u(I,true),R1),pp_items(Type,R1),!.
print_db_items(I):- listing(I),!,nl,nl.

pp_rules :-
   print_db_items("Forward Rules",(_=>_)),
   print_db_items("Bidirectional Rules",(_<=>_)), 
   print_db_items("Backchaining Rules",(_<=_)),
   print_db_items("Forward Facts",(=>(_))).

pp_triggers :-
     print_db_items("Positive triggers",pt(_,_)),
     print_db_items("Negative triggers", nt(_,_,_)),
     print_db_items("Goal triggers",bt(_,_)).

pp_supports :-
  % temporary hack.
  draw_line,
  fmt("~N% Supports ...~n",[]),
  setof((S > P), pfc_get_support(P,S),L),
  pp_items(Type,L),
  draw_line.



get_fa(PI,F,A):-var(PI),!.
get_fa(F/A,F,A):- !.
get_fa(PI,PI,A):- atomic(PI),!.
get_fa(PI,F,A):- compound(PI),!,functor(PI,F,A).
get_fa(Mask,F,A):-get_functor(Mask,F,A).


clause_or_call(M:H,B):-var(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,_F,_A))),M:clause_or_call(H,B).
clause_or_call(isa(I,C),true):-!,req(isa_asserted(I,C)).
clause_or_call(genls(I,C),true):-!,logOnError(req(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause(H,B)*->!;fail) ; clause(H,B)).
clause_or_call(H,true):- should_call_for_facts(H),no_repeats(logOnError(H)).


% as opposed to simply using clause(H,true).
should_call_for_facts(H):- get_functor(H,F,A),should_call_for_facts(H,F,A).
should_call_for_facts(H,F,A):- prologSideEffects(F),!,fail.
should_call_for_facts(H,F,A):- \+ predicate_property(H,number_of_clauses(_)),!.
should_call_for_facts(H,F,A):- pfcMark(pfcRHS,_,F,A),!,fail.
should_call_for_facts(H,F,A):- pfcMark(pfcMustFC,_,F,A),!,fail.
should_call_for_facts(H,F,A):- prologDynamic(F),!.
should_call_for_facts(H,F,A):- \+ pfcControlled(F),!.


print_db_items(Title,Mask,What):-print_db_items(Title,Mask,Mask,What).
print_db_items(Title,Mask,SHOW,What0):-
     get_pi(Mask,H),get_pi(What0,What),
     format(atom(Showing),'~q for ~q...',[Title,What]),
     statistics(cputime,Now),Max is Now + 2,!,
       gripe_time(1.0,
         doall((once(statistics(cputime,NewNow)),NewNow<Max,clause_or_call(H,B),
             hotrace(pfc_contains_term(What,(H:-B))),
             flag(print_db_items,LI,LI+1),
             hotrace(pp_item(Showing,SHOW))))),
     pp_item(Showing,done),!.

pfc_contains_term(What,_):-is_ftVar(What),!.
pfc_contains_term(What,Inside):-compound(What),!,(\+ \+ ((copy_term_nat(Inside,Inside0),numbervars(Inside0),contains_term(What,Inside0)))),!.
pfc_contains_term(What,Inside):- (\+ \+ notrace((subst(Inside,What,foundZadooksy,Diff),Diff \=@= Inside ))),!.

% user:listing_mpred_hook(What):- debugOnError(pfc_listing(What)).

:-thread_local thlocal:pfc_listing_disabled.

pfc_listing(What):-thlocal:pfc_listing_disabled,!.
pfc_listing(What):-loop_check(pfc_listing_nlc(What)).

:-meta_predicate(pfc_listing_nlc(?)).

pfc_listing_nlc(M:What):-atom(M),!,M:pfc_listing(What).
pfc_listing_nlc(What):-loop_check(pfc_listing_0(What),true).

pfc_listing_0(What):-get_pi(What,PI),PI\=@=What,pfc_listing(PI).
pfc_listing_0(What):-nonvar(What),What=neg(Then),!, \+ \+ pfc_listing_1(Then), \+ \+ pfc_listing_1(What).
pfc_listing_0(What):- \+ \+  pfc_listing_1(neg(What)), \+ \+ pfc_listing_1(What).

pfc_listing_types('Triggers').
pfc_listing_types('Instances').
pfc_listing_types('Subclasses').
pfc_listing_types('ArgTypes').
pfc_listing_types('Arity').
pfc_listing_types('Forward').
pfc_listing_types('Bidirectional').
pfc_listing_types('Backchaining').
pfc_listing_types('Negative').
pfc_listing_types('Sources').
pfc_listing_types('Supports').
pfc_listing_types('Edits').

% print_db_items_and_neg(Title,Fact,What):-nonvar(Fact),Fact=neg(_),!,fail.
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,Fact,What).
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,neg(Fact),What).

pfc_listing_1(neg(What)):-var(What),!.
pfc_listing_1(What):-var(What),!.
pfc_listing_1(What):-
   print_db_items('Supports User',spft_precanonical(P,u,u),spft(P,u,u),What),
   print_db_items('Forward Facts',(=>(F)),F,What),
   print_db_items('Forward Rules',(_=>_),What),
 ignore((What\=neg(_),functor(What,IWhat,_),
   print_db_items_and_neg('Instance Of',isa(IWhat,_),IWhat),
   print_db_items_and_neg('Instances: ',isa(_,IWhat),IWhat),
   print_db_items_and_neg('Subclass Of',genls(IWhat,_),IWhat),
   print_db_items_and_neg('Subclasses: ',genls(_,IWhat),IWhat))),
   print_db_items('PFC Watches', pfcMark(_,_,_,_),What),
   print_db_items('Triggers Negative', nt(_,_,_),What),
   print_db_items('Triggers Goal',bt(_,_),What),
   print_db_items('Triggers Positive',pt(_,_),What),
   print_db_items('Bidirectional Rules',(_<=>_),What), 
   dif(A,B),print_db_items('Supports Deduced',spft_precanonical(P,A,B),spft(P,A,B),What),
   dif(G,u),print_db_items('Supports Nonuser',spft_precanonical(P,G,G),spft(P,G,G),What),
   print_db_items('Backchaining Rules',(_<=_),What),
   % print_db_items('Edits',is_disabled_clause(_),What),
   print_db_items('Edits',is_edited_clause(_,_,_),What),
   print_db_items('Instances',isa(_,_),What),
   print_db_items('Subclasses',genls(_,_),What),
   print_db_items('Negative Facts',neg(_),What),

   print_db_items('ArgTypes',argGenls(_,_,_),What),
   print_db_items('ArgTypes',argIsa(_,_,_),What),
   print_db_items('ArgTypes',argQuotedIsa(_,_,_),What),
   print_db_items('ArgTypes',meta_argtypes(_),What),
   print_db_items('ArgTypes',predicate_property(G,meta_predicate(G)),What),
   print_db_items('ArgTypes',resultGenls(_,_),What),
   print_db_items('ArgTypes',resultIsa(_,_),What),
   print_db_items('Arity',arity(_,_),What),
   print_db_items('Arity',current_predicate(_),What),
   print_db_items('MetaFacts Predicate',predicate_property(_,_),What),
   print_db_items('Sources',module_property(_,_),What),
   print_db_items('Sources',mpred_module(_,_),What),
   print_db_items('Sources',source_file(_,_),What),
   print_db_items('Sources',_:man_index(_,_,_,_,_),What),
   print_db_items('Sources',_:'$pldoc'(_,_,_,_),What),
   print_db_items('Sources',_:'$pred_option'(_,_,_,_),What),
   print_db_items('Sources',_:'$mode'(_,_),What),
   !.     


no_side_effects(S):- get_functor(S,F), (prologSideEffects(F)->thlocal:side_effect_ok;true).

is_disabled_clause(C):-is_edited_clause(C,_,New),memberchk((disabled),New).

%= pfc_fact(P) is true if fact P was asserted into the database via pfc_assert.

pfc_fact(P) :- pfc_fact(P,true).

%= pfc_fact(P,C) is true if fact P was asserted into the database via
%= assert and condition C is satisfied.  For example, we might do:
%=
%=  pfc_fact(X,pfc_user_fact(X))
%=

pfc_user_fact(X):-no_repeats(spft(X,U,U)).

pfc_fact(P,C) :-
  pfc_get_support(P,_),nonvar(P),
  once(pfc_db_type(P,F)),F=fact,
  call_prologsys(C).

%= pfc_facts(-ListofPfcFacts) returns a list of facts added.

pfc_facts(L) :- pfc_facts(_,true,L).

pfc_facts(P,L) :- pfc_facts(P,true,L).

%= pfc_facts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfc_facts(P,C,L) :- setof(P,pfc_fact(P,C),L).

brake(X) :-  X, break.

%=
%=
%= predicates providing a simple tracing facility
%=

pfc_trace_add(P) :-
  % this is here for upward compat. - should go away eventually.
  pfc_trace_add(P,(o,o)).

/*
pfc_trace_add(pt(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfc_trace_add(nt(_,_),_) :-
  % hack for now - never trace triggers.
  !.
*/
pfc_trace_add(P,S) :-
   pfc_trace_addPrint(P,S),
   pfc_trace_break(P,S).


pfc_trace_addPrint(P,S):- (\+ \+ pfc_trace_addPrint_0(P,S)).
pfc_trace_addPrint_0(P,S) :-
  pfc_traced(P),
  !,
  must(S=(F,T)),
  (F==T
       -> pfc_debug_trace("~N% Adding (~w) ~w ~n",[F,P])
        ; pfc_debug_trace("~N% Adding (:) ~w    <-------- (~q <=TF=> ~q)~n",[P,(T),(F)])).

pfc_trace_addPrint_0(_,_).


pfc_trace_break(P,_S) :-
  pfc_spied(P,add) ->
   \+ \+ ((copy_term(P,Pcopy),
    numbervars(Pcopy,0,_),
    fmt("~N% Breaking on pfc_assert(~w)",[Pcopy]),
    break))
   ; true.

pfc_trace_rem(pt2(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfc_trace_rem(nt2(_,_)) :-
  % hack for now - never trace triggers.
  !.

pfc_trace_rem(P) :-
  ((pfc_traced(P))
     -> (pfc_debug_trace('~N% Removing ~w.~n',[P]))
      ; true),
  (pfc_spied(P,rem)
   -> (fmt("~N% Breaking on pfc_rem1(~w)",[P]),
break)
   ; true),!.


pfc_trace :- pfc_trace(_).

pfc_trace(Form) :-
  assert_i(pfc_traced(Form)).

pfc_trace(Form,Condition) :-
  assert_i((pfc_traced(Form) :- Condition)).

pfc_spy(Form) :- pfc_spy(Form,[pfc_assert,pfc_rem1],true).

pfc_spy(Form,Modes) :- pfc_spy(Form,Modes,true).

pfc_spy(Form,[add,rem],Condition) :-
  !,
  pfc_spy1(Form,add,Condition),
  pfc_spy1(Form,rem,Condition).

pfc_spy(Form,Mode,Condition) :-
  pfc_spy1(Form,Mode,Condition).

pfc_spy1(Form,Mode,Condition) :-
  assert_i((pfc_spied(Form,Mode) :- Condition)).

pfc_no_spy :- pfc_no_spy(_,_,_).

pfc_no_spy(Form) :- pfc_no_spy(Form,_,_).

pfc_no_spy(Form,Mode,Condition) :-
  clause_i(pfc_spied(Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
pfc_no_spy(_,_,_).

pfc_no_trace :- pfc_untrace.
pfc_untrace :- pfc_untrace(_).
pfc_untrace(Form) :- retractall_i(pfc_traced(Form)).

% needed:  pfc_trace_rule(Name)  ...


% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :-
    pfc_trace_exec,
    !,
    notrace( \+ \+  fmt(user_output, Msg, Args)).
pfc_trace_msg(_Msg,_Args).

pfc_watch :- assert_i(pfc_trace_exec).

pfc_no_watch :-  retractall_i(pfc_trace_exec).

pfc_error(Msg) :-  pfc_error(Msg,[]).

pfc_error(Msg,Args) :-
  fmt("~N% ERROR/Pfc: ",[]),
  fmt(Msg,Args).


%=
%= These control whether or not warnings are printed at all.
%=   pfc_warn.
%=   nopfc_warn.
%=
%= These print a warning message if the flag pfc_warnings is set.
%=   pfc_warn(+Message)
%=   pfc_warn(+Message,+ListOfArguments)
%=

pfc_warn :-
  retractall_i(pfc_warnings(_)),
  assert_i(pfc_warnings(true)).

nopfc_warn :-
  retractall_i(pfc_warnings(_)),
  assert_i(pfc_warnings(false)).

pfc_warn(Msg) :-  pfc_warn(Msg,[]).

pfc_warn(Msg,Args) :-
 % pfc_warnings(true),
  !,
  sformat(S, Msg,Args),
  show_source_location,
  dmsg(pfc(warn(S))),!.  
pfc_warn(_,_).

%=
%= pfc_warnings/0 sets flag to cause pfc warning messages to print.
%= pfc_no_warnings/0 sets flag to cause pfc warning messages not to print.
%=

pfc_warnings :-
  retractall_i(pfc_warnings(_)),
  assert_i(pfc_warnings(true)).

pfc_no_warnings :-
  retractall_i(pfc_warnings(_)).


% ======================= pfc_file('pfcjust').	% predicates to manipulate justifications.


%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

justification(F,J) :- supports_f_l(F,J).

justifications(F,Js) :- bagof(J,justification(F,J),Js).

%= base(P,L) - is true iff L is a list of "base" facts which, taken
%= together, allows us to deduce P.  A base fact is an axiom (a fact
%= added by the user or a raw Prolog fact (i.e. one w/o any support))
%= or an assumption.

base(F,[F]) :- (axiom(F) ; assumption(F)),!.
base(F,L) :-
  % i.e. (reduce 'append (map 'base (justification f)))
  justification(F,Js),
  bases(Js,L).


%= bases(L1,L2) is true if list L2 represents the union of all of the
%= facts on which some conclusion in list L1 is based.

bases([],[]).
bases([X|Rest],L) :-
  base(X,Bx),
  bases(Rest,Br),  
  pfc_union(Bx,Br,L).
	

axiom(F) :-
  pfc_get_support(F,(u,u));
  pfc_get_support(F,(g,g));
  pfc_get_support(F,(OTHER,OTHER)).

% axiom(F) :-  pfc_get_support(F,(U,U)).

%= an assumption is a failed action, i.e. were assuming that our failure to
%= prove P is a proof of not(P)

assumption(P) :- nonvar(P),pfc_negation(P,_).

%= assumptions(X,As) if As is a set of assumptions which underly X.

assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  pfc_union(Bx,Br,L).


%= pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%= of the form
%=
%=     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%=          ^                         and has the form of
%=          [J11, J12,... J1n]      a list of proof trees.


% pfc_child(P,Q) is true iff P is an immediate justifier for Q.
% mode: pfc_child(+,?)

pfc_child(P,Q) :-
  pfc_get_support(Q,(P,_)).

pfc_child(P,Q) :-
  pfc_get_support(Q,(_,Trig)),
  pfc_db_type(Trig,trigger),
  pfc_child(P,Trig).

pfc_children(P,L) :- bagof(C,pfc_child(P,C),L).

% pfc_descendant(P,Q) is true iff P is a justifier for Q.

pfc_descendant(P,Q) :-
   pfc_descendant1(P,Q,[]).

pfc_descendant1(P,Q,Seen) :-
  pfc_child(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfc_descendant1(P,X,[X|Seen])).

pfc_descendants(P,L) :-
  bagof(Q,pfc_descendant1(P,Q,[]),L).

% ======================= pfc_file('pfcwhy').	% interactive exploration of justifications.

%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:-dynamic(whymemory/2).
:-thread_local(thlocal:pfc_interactive_why).

:- use_module(library(lists)).

pfc_interactive_why:- with_assertions(thlocal:pfc_interactive_why,pfc_why).

pfc_why :-
  whymemory(P,_),
  pfc_why(P).

user:why(N) :- pfc_why(N).

pfc_interactive_why(N):- with_assertions(thlocal:pfc_interactive_why,pfc_why(N)).

pfc_why(N) :-
  number(N),
  !,
  whymemory(P,Js),
  pfc_why_command(N,P,Js).

pfc_why(P) :-
  justifications(P,Js),
  retractall_i(whymemory(_,_)),
  assert_i(whymemory(P,Js)),
  pfc_whyBrouse(P,Js).

pfc_why1(P) :-
  justifications(P,Js),
  pfc_whyBrouse(P,Js).

pfc_whyBrouse(P,Js) :-
  pp_justifications(P,Js),
  pfc_interactive_why -> ((
  pfc_ask(' >> ',Answer),
  pfc_why_command(Answer,P,Js))); true.

pfc_why_command(q,_,_) :- !.
pfc_why_command(h,_,_) :-
  !,
  fmt("~N%
Justification Browser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",
[]).

pfc_why_command(N,_P,Js) :-
  float(N),
  !,
  pfc_select_justificationNode(Js,N,Node),
  pfc_why1(Node).

pfc_why_command(u,_,_) :-
  % u=up
  !.

pfc_command(N,_,_) :-
  integer(N),
  !,
  fmt("~N% ~w is a yet unimplemented command.",[N]),
  fail.

pfc_command(X,_,_) :-
 fmt("~N% ~w is an unrecognized command, enter h. for help.",[X]),
 fail.

pp_why(P):- is_list(P),!,maplist(pp_why,P),!.

pp_why(P):-
  no_repeats(P,justifications(P,Js)),
      pp_justifications(P,Js).

pp_justifications(P,Js) :-
  fmt("~N% Justifications for ~w:",[P]),
  must(pp_justification1(Js,1)).

pp_justification1([],_).

pp_justification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  pp_justifications2(J,N,1),
  N2 is N+1,
  pp_justification1(Js,N2).

pp_justifications2([],_,_).

pp_justifications2([C|Rest],JustNo,StepNo) :-
  copy_term(C,CCopy),
  \+ \+ (( numbervars(CCopy,0,_),
  fmt("~N%     ~w.~w ~w",[JustNo,StepNo,CCopy]))),
  StepNext is 1+StepNo,
  pp_justifications2(Rest,JustNo,StepNext).

pfc_ask(Msg,Ans) :-
  fmt("~N% ~w",[Msg]),
  read(Ans).

pfc_select_justificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth_pfc_call(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth_pfc_call(StepNo,Justification,Step).

nth_pfc_call(N,List,Ele):-N2 is N+1,lists:nth0(N2,List,Ele).



:-dynamic(prologMacroHead/1).


compute_resolve(NewerP,OlderQ,S1,S2,(pfc_remove3(OlderQ),pfc_add(NewerP,S1),pfc_rem1(conflict(NewerP)))):-wdmsg(compute_resolve(S1>S2)).

compute_resolve(NewerP,OlderQ,Resolve):-
   supports_f_l(NewerP,S1),
   supports_f_l(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).


:-multifile(resolveConflict/1).
:-dynamic(resolveConflict/1).
:-multifile(resolverConflict_robot/1).
:-dynamic(resolverConflict_robot/1).
:-export(resolverConflict_robot/1).
resolveConflict(C) :- forall(must(pfc_nf1_negation(C,N)),ignore(show_call_failure((nop(resolveConflict(C)),pp_why(N))))),ignore(show_call_failure((nop(resolveConflict(C)),pp_why(C)))), if_defined(resolverConflict_robot(C)),!.
resolveConflict(C) :- forall(must(pfc_nf1_negation(C,N)),forall(compute_resolve(C,N,TODO),debugOnError(TODO))),!.
resolveConflict(C) :- must((pfc_remove3(C),fmt("~nRem-3 with conflict ~w~n", [C]),pfc_run,sanity(\+C))),\+C,!.
resolveConflict(C) :-
  fmt("~NHalting with conflict ~w~n", [C]),   
  must(pfc_halt(conflict(C))).


pfc_prove_neg(G):-trace, \+ pfc_bc_caching(G), \+ pfc_fact(G).


show_pred_info(F/A):-integer(A),functor(H,F,A),!,show_pred_info(H).
show_pred_info(Head):-
        doall(show_call(no_repeats(isa(Head,_)))),
        functor(Head,F,_),
        doall(show_call(no_repeats(isa(F,_)))),
     (current_predicate(_,Head) -> show_pred_info_0(Head); wdmsg(cannot_show_pred_info(Head))),!.

show_pred_info_0(Head):- 
        doall(show_call(predicate_property(Head,_))),
        (has_cl(Head)->doall((show_call(clause(Head,_))));hotrace((lsting(Head)))),!.

pred_head(Type,P):- no_repeats_u(P,(call(Type,P),\+ meta_wrapper(P),compound(P))).

pred_head_all(P):- pred_head(pred_all,P).

meta_wrapper(neg(_)).
meta_wrapper(pt(_,_)).
meta_wrapper(bt(_,_)).
meta_wrapper(nt(_,_,_)).
meta_wrapper(spft(_,_,_)).
meta_wrapper(added(_)).
meta_wrapper(term_expansion(_,_)).
meta_wrapper(P):- \+ current_predicate(_,P).
meta_wrapper(P):- functor(P,F,_), 
   (prologSideEffects(F);tNotForUnboundPredicates(F)).

meta_wrapper_rule((_=>_)).
meta_wrapper_rule((_<=>_)).
meta_wrapper_rule((_=>_)).
meta_wrapper_rule((_:-_)).


pred_all(P):-pred_u0(P).
pred_all(P):-pred_t0(P).
pred_all(P):-pred_r0(P).

pred_u0(P):-pred_u1(P),has_db_clauses(P).
pred_u0(P):-pred_u2(P).
pred_u1(P):-pfcControlled(F),arity(F,A),functor(P,F,A).
pred_u1(P):-prologHybrid(F),arity(F,A),functor(P,F,A).
pred_u1(P):-prologDynamic(F),arity(F,A),functor(P,F,A).
pred_u2(P):-support_hilog(F,A),functor(P,F,A),has_db_clauses(P).
pred_u2(P):-clause(arity(F,A),true),functor(P,F,A),has_db_clauses(P).

has_db_clauses(P):- predicate_property(P,number_of_clauses(NC)),\+ predicate_property(P,number_of_rules(NC)), \+ \+ clause(P,true).

pred_t0(P):-pt(P,_).
pred_t0(P):-bt(P,_).
pred_t0(P):-nt(P,_,_).
pred_t0(P):-spft(P,_,_).
pred_t0(P):- '=>'(P).

pred_r0(P=>Q):- (P=>Q).
pred_r0(P<=>Q):- (P<=>Q).

rescan_pfc:-forall(clause(user:rescan_pfc_hook,Body),show_call_entry(Body)).

pfc_facts_and_universe(P):- (var(P)->((pred_head_all(P) /* , \+ meta_wrapper_rule(P) */ ));true),call(no_repeats(debugOnError(P))).

user:rescan_pfc_hook:- forall(pfc_facts_and_universe(P),with_assertions(pfc_debug_local,pfc_fwd(P))).
/*
user:rescan_pfc_hook:- forall(pred_head(pred_u0,P), 
                          forall(no_repeats(P,call(P)),
                            show_call(pfc_fwd(P)))).
*/


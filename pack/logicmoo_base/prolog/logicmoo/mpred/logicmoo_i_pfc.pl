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



:-multifile(user:rescan_pfc_hook/0).
:-dynamic(user:rescan_pfc_hook/0).
:-dynamic(use_presently/0).
:-dynamic(pfc_undo_method/2).
% used to annotate a predciate to indicate PFC support
:-multifile(infoF/1).
:-dynamic(infoF/1).
:-export(infoF/1).

:- set_prolog_flag(access_level,system).

is_pfc_action('$VAR'(_)):-!,fail.
is_pfc_action(remove_if_unsupported(_,_)).
is_pfc_action(P):-predicate_property(P,static).
pfc_is_builtin(P):-predicate_property(P,built_in).

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


:- ensure_loaded(library(logicmoo/plarkc/dbase_i_sexpr_reader)).

to_addable_form_wte(Why,I,O):-string(I),must_det_l((input_to_forms(string(I),Wff,Vs),b_setval('$variable_names',Vs),!,kif_sterm_to_pterm(Wff,PTerm),
  to_addable_form_wte(Why,PTerm,O))).
to_addable_form_wte(Why,I,O):-atom(I),atom_contains(I,'('),must_det_l((input_to_forms(atom(I),Wff,Vs),b_setval('$variable_names',Vs),!,sexpr_sterm_to_pterm(Wff,PTerm),
  to_addable_form_wte(Why,PTerm,O))).
to_addable_form_wte(Why,=>(I),O):-!,to_addable_form_wte(Why,I,O).
to_addable_form_wte(Why,(I),O):- subst(I,'impliesF',(=>),M),I\=@=M,!,to_addable_form_wte(Why,M,O).
to_addable_form_wte(Why,USER:I,O):-USER=user,!,to_addable_form_wte(Why,I,O).
to_addable_form_wte(_Why,neg(USER:P0),neg(P0)):-USER=user,!.
to_addable_form_wte(_Why,neg(P0),neg(P0)):-!.
to_addable_form_wte(assert,(H:-B),(H:-B)):-B\==true,!.
to_addable_form_wte(Why,(CUT,P0),(CUT,P)):-pfc_is_builtin(CUT),!,trace,!,to_addable_form_wte(Why,P0,P).
to_addable_form_wte(Why,P0,P):-
    once(notrace(to_addable_form(P0,P));must(to_addable_form(P0,P))),
    ((P0\=@=P,P0\=isa(_,_))->pfc_debug_trace(to_addable_form(Why,P0,P));true).

retract_eq_quitely((H:-B)):-ignore((clause(H,B,Ref),clause(HH,BB,Ref),H=@=HH,B=@=BB,!,erase_safe(clause(HH,BB,Ref),Ref))).
retract_eq_quitely((H)):-ignore((clause(H,true,Ref),clause(HH,BB,Ref),H=@=HH,BB==true,!,erase_safe(clause(HH,BB,Ref),Ref))).
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
to_addable_form(I,OOO):-is_list(I),!,must_maplist(to_addable_form,I,O),flatten(O,OO),!,reduce_clause_from_fwd(OO,OOO).
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
to_predicate_isas0(C,CO):-C=..[F|CL],must_maplist(to_predicate_isas0,CL,CLO),!,CO=..[F|CLO].

:-source_location(F,_),asserta(absolute_source_location_pfc(F)).
exact_args(Q):-var(Q),!,fail.
exact_args(Q):-argsQuoted(Q).
exact_args(Q):-compound(Q),functor(Q,F,_),argsQuoted(F).
exact_args(second_order(_,_)).
exact_args(call(_)).
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
pfc_is_tautology(V):- \+ \+ notrace((copy_term_nat(V,VC),numbervars(VC),pfc_is_taut(VC))).

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



%:-dynamic(not_not/1).
pfc_rewrap_h(A,A):-nonvar(A),\+ is_static_pred(A).
pfc_rewrap_h(A,F):- functor(A,F,_),\+ is_static_pred(F),!.
%pfc_rewrap_h(A,not_not(A)):-!.

fwc:-true.
bwc:-true.
wac:-true.
is_fc_body(P):-cwc, notrace(fwc==P ; (compound(P),arg(1,P,E),is_fc_body(E))),!.
is_bc_body(P):-cwc, notrace(bwc==P ; (compound(P),arg(1,P,E),is_bc_body(E))),!.
is_action_body(P):-cwc, notrace(wac==P ; (compound(P),arg(1,P,E),is_action_body(E))),!.



:-thread_local(thlocal:pfc_debug_local/0).
pfc_silient :- \+ thlocal:pfc_debug_local, \+ thlocal:pfc_trace_exec.

pfc_tracing :-  thlocal:pfc_debug_local ;  thlocal:pfc_trace_exec.

pfc_debug_trace(_):-pfc_silient,!.
pfc_debug_trace(F):-wdmsg(F),!.

pfc_debug_trace(_,_):-pfc_silient,!.
pfc_debug_trace(F,A):-wdmsg(F,A),!.

% show_if_debug(A):- !,show_call(A).
show_if_debug(A):- pfc_tracing->show_call(A);A.

% ======================= 
% user''s program''s database
% ======================= 
% assert_u(arity(prologHybrid,0)):-trace_or_throw(assert_u(arity(prologHybrid,0))).
assert_u(X):- not(compound(X)),!,asserta_u(X,X,0).
assert_u(X):- never_assert_u(Y),X=@=Y,trace_or_throw(never_assert_u(Y)).
assert_u(X):- unnumbervars(X,XO),functor(X,F,A),assert_u(XO,F,A).

assert_u(X,F,_):-if_defined(singleValueInArg(F,SV)),!,must(update_single_valued_arg(X,SV)).
assert_u(X,F,A):-prologSingleValued(F),!,must(update_single_valued_arg(X,A)).
assert_u(X,F,A):-must(isa(F,prologOrdered) -> assertz_u(X,F,A) ; asserta_u(X,F,A)).


asserta_u(X):- never_assert_u(Y),X=@=Y,trace_or_throw(never_assert_u(Y)).
asserta_u(X):- functor(X,F,A),unnumbervars(X,XO),asserta_u(XO,F,A).
asserta_u(X,_,_):- show_call_success(clause_asserted(X)),!.
asserta_u(X,_,_):- must((expire_tabled_list(X),show_if_debug(call_with_attvars(asserta,X)))).

assertz_u(X):- never_assert_u(Y),X=@=Y,trace_or_throw(never_assert_u(Y)).
assertz_u(X):- functor(X,F,A),unnumbervars(X,XO),assertz_u(XO,F,A).
assertz_u(X,_,_):- show_call_success(clause_asserted(X)),!.
assertz_u(X,_,_):- must((expire_tabled_list(X),show_if_debug(call_with_attvars(assertz,X)))).

never_assert_u(vtVerb(BAD)):-fail,BAD=='[|]'.
never_retract_u(human(trudy)).
never_retract_u((father(skArg1ofFatherFn(trudy), trudy))).

retract_u(neg(X)):-must(nonvar(X)),!,retract(neg(X)),must((expire_tabled_list(neg(X)))),must((expire_tabled_list((X)))).
retract_u(X):- never_retract_u(Y),X=@=Y,trace_or_throw(never_retract_u(Y)).
retract_u(X):-show_if_debug(retract(X)),must((expire_tabled_list(X))).
retractall_u(X):-retractall(X),must((expire_tabled_list(X))).
call_u(X):- pfc_call(X).
clause_u(H,B):- must(H\==true),clause(H,B).
clause_u(H,B,Ref):-must(H\==true),clause(H,B,Ref).


pfc_update_literal(P,N,Q,R):-
    arg(N,P,UPDATE),notrace(replace_arg(P,N,OLD,Q)),
    must(Q),update_value(OLD,UPDATE,NEW), 
    notrace(replace_arg(Q,N,NEW,R)).

update_single_valued_arg(P,N):- arg(N,P,UPDATE),notrace(replace_arg(P,N,OLD,Q)),
  must_det_l((call_with_attvars(assert_if_new,spft(P,u,u)),(P->true;(assertz_u(P))),
     doall((clause(Q,true,E),UPDATE \== OLD,erase_safe(clause(Q,true,E),E),pfc_unfwc1(Q))))).



% ======================= 
% prolog system database
% ======================= 
/*
assert_prologsys(X0):-unnumbervars(X0,X),assert(X).
asserta_prologsys(X0):-unnumbervars(X0,X),asserta(X).
assertz_prologsys(X0):-unnumbervars(X0,X),assertz(X).

clause_prologsys(H,B):-clause(H,B).
clause_prologsys(H,B,Ref):-clause(H,B,Ref).
*/
call_prologsys(X):-call(X).

% ======================= 
% internal bookkeeping
% ======================= 
assert_i(X0):-unnumbervars(X0,X),call_with_attvars(assert_if_new,X).
asserta_i(X0):-unnumbervars(X0,X),call_with_attvars(asserta_if_new,X).
assertz_i(X0):-unnumbervars(X0,X),call_with_attvars(assertz_if_new,X).
retract_i(X0):-unnumbervars(X0,X),retract(X).
clause_i(H,B):-clause(H,B).
clause_i(H,B,Ref):-clause(H,B,Ref).
call_i(X):-call(X).
retractall_i(X0):-unnumbervars(X0,X),retractall(X).


% ======================= 
% utils
% ======================= 
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



%example pfcVerifyMissing(mpred_prop(I,D), mpred_prop(I,C), ((mpred_prop(I,C), {D==C});~mpred_prop(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});~mudColor(I,C))). 

pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example pfc_freeLastArg(mpred_prop(I,C),neg(mpred_prop(I,C))):-nonvar(C),!.
%example pfc_freeLastArg(mpred_prop(I,C),(mpred_prop(I,F),C\=F)):-!.
pfc_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
pfc_freeLastArg(_G,false).

pfc_current_op_support((p,p)):-!.

pfcVersion(1.2).

:- meta_predicate pfcl_do(0).
:- meta_predicate pfc_fact(*,0).
:- meta_predicate foreachl_do(0,0).
:- meta_predicate brake(0).
:- meta_predicate fc_eval_action(0,*).
:- meta_predicate call_prologsys(0).
:- meta_predicate call_i(0).
:- meta_predicate pfc_get_trigger_quick(0).
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

correctify_support((S,T),(S,T)):-!.
correctify_support(U,(U,U)):-atom(U),!.
correctify_support([U],S):-correctify_support(U,S).

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
pfc_add_fast('$was_imported_kb_content$'(_, _)<=THIS):-nonvar(THIS),!.
pfc_add_fast(P):-pfc_assert_fast(P).


pfc_asserta(G):-pfc_add(G).
% a really common example is people want unbound predicate backchaining .. that is to query the predicates witha  varaible where the predciate is 
pfc_assertz(G):-pfc_add(G).

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

pfc_assert_fast(P0,S):- gripe_time(0.6,pfc_assert_fast_timed(P0,S)).

pfc_assert_fast_timed(P0,S):-
  must(to_addable_form_wte(assert,P0,P)),
      (is_list(P)
        ->must_maplist(pfc_assert_fast_sp(S),P);
       pfc_assert_fast_sp(S,P)).


pfc_assert_fast_sp(S,P) :-
   pfc_rule_hb(P,OutcomeO,_),
     loop_check_term((pfc_post_sp_zzz(S,P),pfc_run_maybe),
     pfc_asserting(OutcomeO),
     (pfc_post_sp_zzz(S,P),pfc_debug_trace(looped_outcome((P))))),!.
%pfc_assert_fast_sp(_,_).
pfc_assert_fast_sp(P,S) :- pfc_error("pfc_assert_fast(~q,~q) failed",[P,S]).



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
pfc_post1(pfc_assert(P0),S):- must(nonvar(P0)), !,pfc_assert(P0,S).
pfc_post1(P0,S):-
  to_addable_form_wte(assert,P0,P),
      (is_list(P)
        ->maplist(pfc_post_sp_zzz(S),P);
       pfc_post_sp_zzz(S,P)).

pfc_post_sp(S,P):-unnumbervars(S+P,SO+PO),pfc_post_sp_zzz(SO,PO).
pfc_post_sp_zzz(S,(P1,P2)) :- !,pfc_post_sp_zzz(S,(P1)),pfc_post_sp_zzz(S,(P2)).
pfc_post_sp_zzz(S,[P1]) :- !,pfc_post_sp_zzz(S,(P1)).
pfc_post_sp_zzz(S,[P1|P2]) :- !,pfc_post_sp_zzz(S,(P1)),pfc_post_sp_zzz(S,(P2)).
pfc_post_sp_zzz(S, \+ P) :-!,doall(pfc_rem2a(P,S)),!,pfc_undo((\+),P).
pfc_post_sp_zzz(S, ~  P) :-!,doall(pfc_rem2a(P,S)),!,pfc_undo((~ ),P).
pfc_post_sp_zzz(S, not( P)) :-!,pfc_post_sp_zzz(S, neg( P)).
pfc_post_sp_zzz(_S,P) :- once((pfc_is_tautology(P),dumpST,dmsg(trace_or_throw(todo(error(pfc_is_tautology(P))))))),show_load_context,prolog,fail.

% only do loop check if it's already supported

pfc_post_sp_zzz(S,P) :- compound(P), arg(SV,P,V),is_relative(V),must((pfc_update_literal(P,SV,Q,R),pfc_post_sp_zzz(S,R))),(Q=R->true;pfc_undo(update,Q)).
pfc_post_sp_zzz(S,P) :- is_already_supported(P,S,_How),must(loop_check(pfc_post1_sp_0(S,P),pfc_post1_sp_1(S,P))),!. % ,pfc_enqueue(P,S).
pfc_post_sp_zzz(S,P) :- pfc_post1_sp_0(S,P).

pfc_post1_sp_0(S,P) :-
  %= db pfc_add_db_to_head(P,P2),
  % pfc_remove_old_version(P),
  must(once(pfc_add_support(P,S))),
  pfc_post1_sp_1(S,P).

pfc_post1_sp_1(S,P):- P\==true,
  pfc_unique_u(P),
  must(assert_u(P)),!,
  must(pfc_trace_add(P,S)),
  !,
  must(pfc_enqueue(P,S)),
  !.

pfc_post1_sp_1(_,_). % arleady added
pfc_post1_sp_1(S,P) :-  pfc_warn("pfc_post1(~q,~q) failed",[P,S]).


with_pfc_trace_exec(P):- with_assertions(thlocal:pfc_trace_exec, must(show_call(P))).
pfc_test(P):- with_pfc_trace_exec(must(show_call(P))).

is_already_supported(P,(S,T),(S,T)):- clause_asserted(spft(P,S,T)),!.
is_already_supported(P,_S,(u,u)):- clause_asserted(spft(P,u,u)),!.

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term(P,PC),spft(PC,_,_),P=@=PC,!.



different_literal(Q,N,R,Test):- 
 nonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A),
  (singleValuedInArg(F,N) -> 
    (arg(N,Q,Was),Test=dif(Was,NEW),replace_arg(Q,N,NEW,R));
    ((arg(N,Q,Was),nonvar(Q)) -> (Test=dif(Was,NEW),replace_arg(Q,N,NEW,R));
        (N=A,arg(N,Q,Was),Test=dif(Was,NEW),replace_arg(Q,N,NEW,R)))).



% was nothing  pfc_current_db/1.
pfc_current_db(u).
pfc_current.


%=
%= pfc_add_db_to_head(+P,-NewP) talkes a fact P or a conditioned fact
%= (P:-C) and adds the Db context.
%=

pfc_add_db_to_head(P,NewP) :-
  pfc_current_db(Db),
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
          otherwise           -> pfc_warn("Unrecognized pfc_search mode: ~q", Mode))
     ; pfc_warn("No pfc_search mode").


% if there is a rule of the form Identifier ::: Rule then delete it.

pfc_remove_old_version((Identifier::::Body)) :-
  % this should never happen.
  var(identifier),
  !,
  pfc_warn("variable used as an  rule name in ~q :::: ~q",
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
  pfc_step,
  pfc_run0.
pfc_run0.


%pfc_run_queued:- repeat,sleep(1.0),pfc_run,fail.
%:-thread_property(_,alias(pfc_running_queue))-> true ; thread_create(pfc_run_queued,_,[alias(pfc_running_queue)]).


% pfc_step removes one entry from the pfc_queue and reasons from it.

pfc_step:-loop_check(pfc_step0,true).

pfc_step0 :-
  % if pfc_halt_signal(Signal) is true, reset it and fail, thereby stopping inferencing.
  pfc_retract_db_type(pfc_halt_signal(Signal)),
  !,
  pfc_warn("~N% Stopping on signal ~q",[Signal]),
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
  brake(fmt("~N% pfc:get_next_fact - selected fact not on Queue: ~q (~q)",
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
  fmt('~N% ~q~n',[S]),
  (pfc_halt_signal(Signal) ->
       pfc_warn("pfc_halt finds pfc_halt_signal(Signal) already set to ~q",[Signal])
     ; assert_i(pfc_halt_signal(S))).


%=
%=
%= predicates for manipulating triggers
%=

pfc_add_trigger(pt(Trigger,Body),Support) :-
  !,  
  pfc_trace_item('Adding For Later',pt(Trigger,Body)),
  (clause_asserted(pt(Trigger,Body)) -> true ;
      ( \+ \+ pfc_assert_t(pt(Trigger,Body),Support),
        must(pfc_mark_as(Support,p,Trigger,pfcPosTrigger)),
        add_reprop(Trigger),
        copy_term(pt(Trigger,Body),Tcopy), !,
        no_repeats(call_u(Trigger)),
        debugOnError(pfc_eval_lhs(Body,(Trigger,Tcopy))),
        fail)).



pfc_add_trigger(nt(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_item('Adding For Later',nt(Trigger,Test,Body)),
 %%(pfc_call_t_exact(nt(TriggerCopy,Test,Body)) -> true ;  )
   must(pfc_mark_as(Support,n,Trigger,pfcNegTrigger)),
        copy_term(Trigger,TriggerCopy),
        pfc_assert_t(nt(TriggerCopy,Test,Body),Support),
        \+ call_u(Test),
  pfc_eval_lhs(Body,((\+Trigger),nt(TriggerCopy,Test,Body))).


pfc_add_trigger(bt(Trigger,Body),Support) :-
  !,
  pfc_trace_item('Adding For Later',bt(Trigger,Body)),
   must(pfc_assert_t(bt(Trigger,Body),Support)),
      assertz_if_new((Trigger:-pfc_bc_only(Trigger))),
      must(pfc_mark_as(Support,b,Trigger,pfcBcTrigger)),
     % WAS pfc_bt_pt_combine(Trigger,Body).
  pfc_bt_pt_combine(Trigger,Body,Support).

pfc_add_trigger(X,Support) :- trace,
  pfc_warn("Unrecognized trigger to pfc_addtrigger: ~q",[X:pfc_add_trigger(X,Support)]).


pfc_bt_pt_combine(Head,Body,Support) :-
  %= a backward trigger (bt) was just added with head and Body and support Support
  %= find any pt''s with unifying heads and assert the instantied bt body.
  pfc_get_trigger_quick(pt(Head,_PtBody)),
  pfc_eval_lhs(Body,Support),
  fail.
pfc_bt_pt_combine(_,_,_) :- !.


pfc_get_trigger_quick(Trigger) :- !, no_repeats(Trigger).
pfc_get_trigger_quick(Trigger) :- clause(Trigger,true)*->true;clause(spft(Trigger,_,_),true).

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
  retract_t(X)
    -> pfc_unfwc(X)
     ; pfc_warn("Trigger not found to retract: ~q",[X]).

pfc_retract_db_type(action,X) :- pfc_rem_actiontrace(X).


/* UNUSED TODAY
%= pfc_add_db_type(X) adds item X to some database
%= was simply:  pfc_Add
pfc_add_db_type(X) :-
  % what type of X do we have?
  pfc_db_type(X,Type),
  % call the appropriate predicate.
  pfc_add_db_type(Type,X).

pfc_add_db_type(fact,X) :-
  pfc_unique_u(X),
  must(assert_u(X)),!.
pfc_add_db_type(rule,X) :-
  pfc_unique_i(X),
  assert_i(X),!.
pfc_add_db_type(trigger,X) :-
  assert_t(X).
pfc_add_db_type(action,_Action) :- !.
*/

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

pfc_rem1(P,S) :- copy_term(pfc_rem1(P,S),Why),
  pfc_trace_item('Removing support',pfc_rem1(P,S)),
  pfc_rem_support(Why,P,S)
     -> (remove_if_unsupported(Why,P))
      ; pfc_warn("pfc_rem1/2 Could not find support ~q to remove from fact ~q",
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
  show_call(pfc_remove_supports_f_l(pfc_remove3(F),F)),
  pfc_undo(pfc_remove3(F),F).


% removes any remaining supports for fact F, complaining as it goes.

pfc_remove_supports_f_l(Why,F) :-
  pfc_rem_support(Why,F,S),
  (S=(z,z)->true;pfc_warn("~q was still supported by ~q",[F,S])),
  fail.
pfc_remove_supports_f_l(_,_).

pfc_remove_supports_quietly(F) :-
  pfc_rem_support(pfc_remove_supports_quietly,F,_),
  fail.
pfc_remove_supports_quietly(_).

% pfc_undo(Why,X) undoes X.


pfc_undo(Why,pfc_action(A)) :-
  % undo an action by finding a method and successfully executing it.
  !,
  pfc_rem_actiontrace(Why,pfc_action(A)).

pfc_undo(Why,pk(Key,Head,Body)) :-
  % undo a positive trigger.
  %
  !,
  (retract_i(pk(Key,Head,Body))
    -> pfc_unfwc(pt(Head,Body))
     ; pfc_warn("for ~q \nTrigger not found to retract: ~q",[Why,pt(Head,Body)])).

pfc_undo(Why,pt(Head,Body)) :- 
  % undo a positive trigger.
  %
  !,
  (retract_i(pt(Head,Body))
    -> pfc_unfwc(pt(Head,Body))
     ; pfc_warn("for ~q:\nTrigger not found to retract: ~q",[Why,pt(Head,Body)])).

pfc_undo(Why,nt(Head,Condition,Body)) :-
  % undo a negative trigger.
  !,
  (retract_i(nt(Head,Condition,Body))
    -> pfc_unfwc(nt(Head,Condition,Body))
     ; pfc_warn("for ~q:\nTrigger not found to retract: ~q",[Why,nt(Head,Condition,Body)])).

pfc_undo(Why,Fact):- pfc_undo_u(Why,Fact)*->true;pfc_undo_e(Why,Fact).

pfc_undo_u(Why,Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract_u(Fact),
     must(pfc_trace_rem(Why,Fact)),
     pfc_unfwc1(Fact).

pfc_undo_e(Why,Fact) :- 
     (Fact\=neg(_)->pfc_debug_trace("pfc_undo_e ; Fact not found in user db: ~q",[Fact]);true),
     pfc_trace_rem(Why,Fact),
     pfc_unfwc(Fact).


%= pfc_unfwc(P) "un-forward-chains" from fact f.  That is, fact F has just
%= been removed from the database, so remove all support relations it
%= participates in and check the things that they support to see if they
%= should stay in the database or should also be removed.

pfc_unfwc(F) :-
  pfc_retract_support_relations(pfc_unfwc(F),F),
  pfc_unfwc1(F).

pfc_unfwc1(F) :-
  pfc_unfwc_check_triggers(_Sup,F),
  % is this really the right place for pfc_run<?
  pfc_run_maybe.

pfc_unfwc_check_triggers(_Sup,F) :-
  pfc_db_type(F,fact),
  copy_term(F,Fcopy),
  pfc_get_trigger_quick(nt(Fcopy,Condition,Action)),
  (\+ Condition),
  G = pfc_eval_lhs(Action,((\+F),nt(F,Condition,Action))),
  loop_check(G,wdmsg(unfwc_caught_loop(G))),
  fail.
pfc_unfwc_check_triggers(_Sup,_).

pfc_retract_support_relations(Why,Fact) :-
  pfc_db_type(Fact,Type),
  (Type=trigger -> pfc_rem_support(Why,P,(_,Fact)) ;
    % non trigger
    pfc_rem_support(Why,P,(Fact,_))),
  remove_if_unsupported(Why,P),
  fail.
pfc_retract_support_relations(_,_).

%= remove_if_unsupported(Why,+P) checks to see if P is supported and removes
%= it from the DB if it is not.


remove_if_unsupported_verbose(Why,TMS,P) :- var(P),!,trace_or_throw(warn(var_remove_if_unsupported_verbose(Why,TMS,P))).
remove_if_unsupported_verbose(Why,TMS,P) :- (\+ ground(P) -> dmsg(warn(ng_remove_if_unsupported_verbose(Why,TMS,P))) ;true),
   (((pfc_tms_supported(TMS,P,How),How\=unknown(_)) -> pfc_trace_msg(v_still_supported(How,Why,TMS,P)) ; (  pfc_undo(Why,P)))),
   pfc_run.


remove_if_unsupported(Why,P) :- var(P),!,trace_or_throw(warn(var_remove_if_unsupported(Why,P))).
remove_if_unsupported(Why,P) :- ((\+ ground(P), P \= (_:-_) , P \= neg(_) ) -> dmsg(warn(nonground_remove_if_unsupported(Why,P))) ;true),
   (((pfc_tms_supported(local,P,How),How\=unknown(_)) -> pfc_trace_msg(still_supported(How,Why,local,P)) ; (  pfc_undo(Why,P)))),
   pfc_run.


%= pfc_tms_supported(+P,-How) succeeds if P is "supported". What "How" means
%= depends on the TMS mode selected.

pfc_tms_supported(P,How) :-
  fcTmsMode(Mode),
  pfc_tms_supported0(Mode,P,How).


pfc_tms_supported(Mode,P,How) :- var(Mode),fcTmsMode(Mode),!,pfc_tms_supported0(Mode,P,How).
pfc_tms_supported(Mode,P,How) :- pfc_tms_supported0(Mode,P,How).
pfc_tms_supported(How,_P,unknown(How)).

pfc_tms_supported0(local,P,How) :-  pfc_get_support(P,How). % ,sanity(pfc_deep_support(How,S)).
pfc_tms_supported0(cycles,P,How) :-  wellFounded(P,How).
pfc_tms_supported0(deep,P,How) :- pfc_deep_support(How,P).

% user:hook_one_minute_timer_tick:- statistics.


pfc_scan_tms(P):-pfc_get_support(P,(S,SS)),
  (S==SS-> true;
   once((pfc_deep_support(How,P)->true;
     (dmsg(warn(now_maybe_unsupported(pfc_get_support(P,(S,SS)),fail))))))).

user_atom(u).
user_atom(g).
user_atom(m).
user_atom(d).

pfc_deep_support(_How,unbound):-!,fail.
pfc_deep_support(How,M):-loop_check(pfc_deep_support0(How,M),fail).

pfc_deep_support0(user_atom(U),(U,U)):-user_atom(U),!.
pfc_deep_support0(How,(A=>_)):-!,pfc_deep_support(How,A).
pfc_deep_support0(pt(HowA,HowB),pt(A,B)):-!,pfc_deep_support(HowA,A),pfc_deep_support(HowB,B).
pfc_deep_support0(HowA->HowB,(A->B)):-!,pfc_deep_support(HowA,A),pfc_deep_support(HowB,B).
pfc_deep_support0(HowA/HowB,(A/B)):-!,pfc_deep_support(HowA,A),pfc_deep_support(HowB,B).
pfc_deep_support0((HowA,HowB),(A,B)):-!,pfc_deep_support(HowA,A),pfc_deep_support(HowB,B).
pfc_deep_support0(How,rhs(P)):-!,maplist(pfc_deep_support,How,P).
pfc_deep_support0(pfc_call(\+ P),\+ call_u(P)):-!,pfc_call(\+ P).
pfc_deep_support0(pfc_call(P),call_u(P)):-!,pfc_call(P).
pfc_deep_support0(pfc_call(P),{P}):-!,pfc_call(P).
pfc_deep_support0(How=>S,P):-pfc_get_support(P,S),pfc_deep_support(How,S),!.
pfc_deep_support0(pfc_call(\+(P)),\+(P)):-!, pfc_call(\+(P)).
pfc_deep_support0(user_atom(P),P):-user_atom(P),!.
pfc_deep_support0(pfc_call((P)),P):-pfc_call(P).


%=
%= a fact is well founded if it is supported by the user
%= or by a set of facts and a rules, all of which are well founded.
%=

wellFounded(Fact,How) :- pfc_wff(Fact,[],How).

pfc_wff(F,_,How) :-
  % supported by user (axiom) or an "absent" fact (assumption).
  ((axiom(F),How =axiom(F) ); (assumption(F),How=assumption(F))),
  !.

pfc_wff(F,Descendants,wff(Supporters)) :-
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
  pfc_wff(X,L,_How),
  pfc_wfflist(Rest,L).

% supports_f_l(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [u].

supports_f_l(F,[Fact|MoreFacts]) :-
  pfc_get_support_precanonical_plus_more(F,(Fact,Trigger)),
  trigger_supports_f_l(Trigger,MoreFacts).

pfc_get_support_precanonical_plus_more(P,Sup):-pfc_get_support_one(P,Sup)*->true;((to_addable_form_wte(pfc_get_support_precanonical_plus_more,P,PE),P\=@=PE,pfc_get_support_one(PE,Sup))).
pfc_get_support_one(P,Sup):- pfc_get_support(P,Sup)*->true;
  (pfc_get_support_via_clause_db(P,Sup)*->true;
     pfc_get_support_via_sentence(P,Sup)).

pfc_get_support_via_sentence(Var,_):-var(Var),!,fail.
pfc_get_support_via_sentence((A,B),(FC,TC)):-!, pfc_get_support_precanonical_plus_more(A,(FA,TA)),pfc_get_support_precanonical_plus_more(B,(FB,TB)),conjoin(FA,FB,FC),conjoin(TA,TB,TC).
pfc_get_support_via_sentence(true,g):-!.

pfc_get_support_via_clause_db(\+ P,OUT):- pfc_get_support_via_clause_db(neg(P),OUT).
pfc_get_support_via_clause_db(\+ P,(naf(g),g)):- !, predicate_property(P,number_of_clauses(_)),\+ clause(P,_Body).
pfc_get_support_via_clause_db(P,OUT):- predicate_property(P,number_of_clauses(N)),N>0,
   clause(P,Body),(Body==true->Sup=(g);
    (support_ok_via_clause_body(P),pfc_get_support_precanonical_plus_more(Body,Sup))),
   OUT=(Sup,g).

support_ok_via_clause_body(_H):-!,fail.
support_ok_via_clause_body(H):- get_functor(H,F,A),support_ok_via_clause_body(H,F,A).
support_ok_via_clause_body(_,(\+),1):-!,fail.
support_ok_via_clause_body(_,F,_):- prologSideEffects(F),!,fail.
support_ok_via_clause_body(H,_,_):- \+ predicate_property(H,number_of_clauses(_)),!,fail.
support_ok_via_clause_body(_,F,A):- pfcMark(pfcRHS,_,F,A),!,fail.
support_ok_via_clause_body(_,F,A):- pfcMark(pfcMustFC,_,F,A),!,fail.
support_ok_via_clause_body(_,F,_):- argsQuoted(F),!,fail.
support_ok_via_clause_body(_,F,_):- prologDynamic(F),!.
support_ok_via_clause_body(_,F,_):- \+ pfcControlled(F),!.


pfc_get_support_precanonical(F,Sup):-to_addable_form_wte(pfc_get_support_precanonical,F,P),pfc_get_support(P,Sup).
spft_precanonical(F,SF,ST):-to_addable_form_wte(spft_precanonical,F,P),!,spft(P,SF,ST).

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

pfc_fwd2(Fact,_Sup) :- is_pfc_action(Fact),must(Fact),fail.
pfc_fwd2(Fact,Sup) :- cyclic_term(Fact;Sup),writeq(pfc_fwd2_cyclic_term(Fact;Sup)),!.
pfc_fwd2(Fact,_Sup) :-
  must(pfc_add_rule_if_rule(Fact)),
  copy_term(Fact,F),
  % check positive triggers
  must(fcpt(Fact,F)),
  % check negative triggers
  must(fcnt(Fact,F)),!.


%=
%= pfc_add_rule_if_rule(P) does some special, built in forward chaining if P is
%= a rule.
%=
pfc_add_rule_if_rule(Fact):-  
  cyclic_break(Fact),
  must(pfc_add_rule0(Fact)),!.

pfc_add_rule0((P=>Q)) :-
  !,
  process_rule(P,Q,(P=>Q)).

pfc_add_rule0((Name::::P=>Q)) :-
  !,
  process_rule(P,Q,(Name::::P=>Q)).

pfc_add_rule0((P<=>Q)) :-
  !,
  process_rule(P,Q,(P<=>Q)),
  process_rule(Q,P,(P<=>Q)).

pfc_add_rule0((Name::::P<=>Q)) :-
  !,
  process_rule(P,Q,((Name::::P<=>Q))),
  process_rule(Q,P,((Name::::P<=>Q))).

pfc_add_rule0(('<='(P,Q))) :-
  !,
  pfc_define_bc_rule(P,Q,('<='(P,Q))).

pfc_add_rule0(_).

fcpt(Fact,F):- fcpt0(Fact,F)*->fail;nop(pfc_trace_msg(no_pt(Fact,F))).
fcpt(_,_).

fcpt0(Fact,F) :- 
  pfc_get_trigger_quick(pt(F,Body)),
  (pfc_eval_lhs(Body,(Fact,pt(F,Body)))
    *-> pfc_trace_item('Using Trigger',pt(F,Body));
      (pfc_trace_item('Skipped Trigger',pt(F,Body)),fail)).
  

fcpt0(Fact,F) :- use_presently,
  pfc_get_trigger_quick(pt(presently(F),Body)),
  pp_item('Found presently ',pt(F,Body)),
  pfc_eval_lhs(Body,(presently(Fact),pt(presently(F),Body))).

fcnt(Fact,F):- fcnt0(Fact,F)*->fail;nop(pfc_trace_msg(no_spft_nt(Fact,F))).
fcnt(_,_).

fcnt0(_Fact,F) :- 
  spft(X,_,nt(F,Condition,Body)),
  (call_u(Condition) *-> 
   (pfc_trace_item('Using Trigger'(X),nt(F,Condition,Body)),pfc_rem1(X,(_,nt(F,Condition,Body))),fail);
      (pfc_trace_item('Unused Trigger'(X),nt(F,Condition,Body)),fail)).
  


%=
%= pfc_define_bc_rule(+Head,+Body,+Parent_rule) - defines a backward
%= chaining rule and adds the corresponding bt triggers to the database.
%=

pfc_define_bc_rule(Head,Body,Parent_rule) :-
  (\+ pfc_literal(Head)),
  pfc_warn("Malformed backward chaining rule.  ~q not atomic.",[(Head:-Body)]),
  pfc_warn("rule: ~q",[Parent_rule]),
 % !,
  dtrace(pfc_define_bc_rule(Head,Body,Parent_rule)),
  fail.

pfc_define_bc_rule(Head,Body,Parent_rule) :- 
  copy_term(Parent_rule,Parent_ruleCopy),
  call_with_attvars(assert_if_new,Head:-pfc_bc_only(Head)),
  build_rhs(Head,Head,Rhs),
  foreachl_do(pfc_nf(Body,Lhs),
       (build_trigger(Parent_ruleCopy,Lhs,rhs(Rhs),Trigger),
       % can be pfc_post_sp(bt(Head,Trigger),(Parent_ruleCopy,u))
         pfc_assert_fast(bt(Head,Trigger),(Parent_ruleCopy,u)))).
        

%=
%=
%= eval something on the LHS of a rule.
%=

pfc_eval_lhs(P,S):-no_repeats(loop_check(pfc_eval_lhs0(P,S))).

pfc_eval_lhs0((Test->Body),Support) :-
  !,
  % (call_prologsys(Test) -> pfc_eval_lhs(Body,Support)),
   ((no_repeats(call_prologsys(Test)) , no_repeats(pfc_eval_lhs(Body,Support))) *-> true ; (!,fail)).

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
  pfc_warn("Unrecognized item found in trigger body, namely ~q.",[X]).


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
  pfc_warn("Malformed rhs of a rule: ~q",[X]).


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
  no_repeats(call_u(Trigger)),
  pfc_eval_lhs(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  no_repeats(pfc_call(Trigger)),
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
pfc_call_0(G):-functor(G,F,A),pfc_call_0(G,F,A).

pfc_call_0(G,F,A):-  (ground(G); \+ current_predicate(F/A) ; \+ (predicate_property(G,clause_count(CC)),CC>1)),  
                ignore((loop_check(call_with_bc_triggers(G)),maybeSupport(G,(g,g)),fail)),
                (\+ current_predicate(F/A),dynamic(F/A),multifile(F/A),fail).
pfc_call_0(G,_,_):- (pfc_call_with_no_triggers(G)).


:-thread_local thlocal:infBackChainPrevented/1.

call_with_bc_triggers(P) :- functor(P,F,A), \+thlocal:infBackChainPrevented(F/A), 
  pfc_get_trigger_quick(bt(P,Trigger)),
  no_repeats(pfc_get_support(bt(P,Trigger),S)),
  once(no_side_effects(P)),
  with_no_assertions(thlocal:infBackChainPrevented(F/A),pfc_eval_lhs(Trigger,S)).


pfc_call_with_no_triggers(U:X):-U==user,!,pfc_call_with_no_triggers(X).
pfc_call_with_no_triggers(F) :- 
  %= this (var(F)) is probably not advisable due to extreme inefficiency.
  (var(F)    ->  pfc_facts_and_universe(F) ; pfc_call_with_no_triggers_bound(F)).

pfc_call_with_no_triggers_bound(F) :- 
  show_call_failure(no_side_effects(F)),
  (\+ current_predicate(_,F) -> fail;call_prologsys(F)).
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

%:-dynamic('{}'/1).
%{X}:-dmsg(legacy({X})),call_prologsys(X).


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

% these next three rules are here for upward compatibility and will go
% away eventually when the P/Condition form is no longer used anywhere.

pfc_nf1(P/Cond,[(\+Q)/Cond]) :- fail, pfc_negated_literal(P,Q), !.  % DMILES does not undersand why this is wron gand the next is correct here
pfc_nf1(P/Cond,[(\+P)/Cond]) :- pfc_negated_literal(P,_), !.

pfc_nf1(P/Cond,[P/Cond]) :-  pfc_literal(P), !.

pfc_nf1({P},[{P}]) :-!.

%= handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_negation(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

pfc_nf1(~((P,Q)),NF) :-
 pfc_nf1(~P,NP),
 pfc_nf1(~Q,NQ),
 !,
 pfc_nf1(((NP/Q);(NQ/P)),NF).

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
  pfc_warn("pfc_nf doesn't know how to normalize ~q",[Term]),!,fail.

pfc_negation_w_neg(neg(P),P):-nonvar(P),!.
pfc_negation_w_neg(P,NF):-pfc_nf1_negation(P,NF).

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


pfc_compile_rhsTerm(_Sup,P,P):-is_ftVar(P),!.
pfc_compile_rhsTerm(Sup,(P/C),((P0:-C0))) :- !,pfc_compile_rhsTerm(Sup,P,P0),build_code_test(Sup,C,C0),!.
pfc_compile_rhsTerm(Sup,I,O):-to_addable_form_wte(pfc_compile_rhsTerm,I,O), must(\+ \+ pfc_mark_as(Sup,r,O,pfcRHS)),!.



pfc_mark_as(_,_,P,_):-var(P),!.

pfc_mark_as(Sup,_PosNeg,neg(P),Type):-!,pfc_mark_as(Sup,neg,P,Type).
pfc_mark_as(Sup,_PosNeg,\+(P),Type):-!,pfc_mark_as(Sup,neg,P,Type).
pfc_mark_as(Sup,_PosNeg,-(P),Type):-!,pfc_mark_as(Sup,neg,P,Type).
pfc_mark_as(Sup,_PosNeg,~(P),Type):-!,pfc_mark_as(Sup,neg,P,Type).
pfc_mark_as(Sup,PosNeg,( P / _ ),Type):- !, pfc_mark_as(Sup,PosNeg,P,Type).
pfc_mark_as(_Sup,_PosNeg,( _ :- _ ),_Type):-!.
pfc_mark_as(_Sup,_PosNeg,( _ , _ ),_Type):-!.
pfc_mark_as(Sup,PosNeg,P,Type):-get_functor(P,F,A),ignore(pfc_mark_fa_as(Sup,PosNeg,P,F,A,Type)),!.

:- dynamic( pfcMark/4).

% pfc_mark_fa_as(_,_,_,'\=',2,_):- trace.
pfc_mark_fa_as(_Sup, PosNeg,_P,F,A,Type):- pfcMark(Type,PosNeg,F,A),!.
pfc_mark_fa_as(_Sup,_PosNeg,_P,isa,_,_):- !.
pfc_mark_fa_as(_Sup,_PosNeg,_P,t,_,_):- !.
pfc_mark_fa_as(_Sup,_PosNeg,_P,argIsa,N,_):- !,must(N=3).
pfc_mark_fa_as(_Sup,_PosNeg,_P,arity,N,_):- !,must(N=2).
pfc_mark_fa_as(_Sup,_PosNeg,_P,pfcMark,N,_):- !,must(N=4).
pfc_mark_fa_as(_Sup,_PosNeg,_P,mpred_prop,N,_):- must(N=2).
pfc_mark_fa_as(_Sup,_PosNeg,_P,_:mpred_prop,N,_):- must(N=2).
pfc_mark_fa_as(Sup,PosNeg,_P,F,A,Type):- pfc_post_sp_zzz((s(Sup),g),pfcMark(Type,PosNeg,F,A)),!.


user:hook_one_minute_timer_tick:-pfc_cleanup.

pfc_cleanup:- forall((no_repeats(F-A,(pfcMark(pfcRHS,_,F,A),A>1))),pfc_cleanup(F,A)).

pfc_cleanup(F,A):-functor(P,F,A),predicate_property(P,dynamic)->pfc_cleanup_0(P);true.

pfc_cleanup_0(P):- findall(P-B-Ref,clause(P,B,Ref),L),forall(member(P-B-Ref,L),erase_safe(clause(P,B,Ref),Ref)),forall(member(P-B-Ref,L),assertz_if_new((P:-B))).

:-debug.
%isInstFn(A):-!,trace_or_throw(isInstFn(A)).

%= pfc_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

pfc_negation((~P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).

pfc_negated_literal(P):-pfc_negated_literal(P,_).
pfc_negated_literal(P,Q) :- nonvar(P),
  pfc_negation(P,Q),
  pfc_literal(Q).

pfc_literal_nv(X):-nonvar(X),pfc_literal(X).
pfc_literal(X) :- cyclic_term(X),!,fail.
pfc_literal(X) :- atom(X),!.
pfc_literal(X) :- pfc_negated_literal(X),!.
pfc_literal(X) :- pfc_positive_literal(X),!.
pfc_literal(X) :- var(X),!.

pfc_non_neg_literal(X):-atom(X),!.
pfc_non_neg_literal(X):-pfc_positive_literal(X), X \= neg(_), X \= pfcMark(_,_,_,_), X \= conflict(_).

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

process_rule(Lhs,Rhs,Parent_rule) :- 
  copy_term(Parent_rule,Parent_ruleCopy),
  build_rhs((Parent_ruleCopy),Rhs,Rhs2),
   cyclic_break((Lhs,Rhs,Rhs2,Parent_ruleCopy)),
  foreachl_do(pfc_nf(Lhs,Lhs2),
   doall(build_rule(Lhs2,rhs(Rhs2),(Parent_ruleCopy,u)))).

build_rule(Lhs,Rhs,Support) :-
  build_trigger(Support,Lhs,Rhs,Trigger),
   cyclic_break((Lhs,Rhs,Support,Trigger)),
  pfc_eval_lhs(Trigger,Support).

build_trigger(Support,[],Consequent,ConsequentO):- build_consequent(Support,Consequent,ConsequentO).

build_trigger(Support,[V|Triggers],Consequent,pt(V,X)) :-
  var(V),
  !,
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[added(T)|Triggers],Consequent,pt(T,X)) :-
  !,
  build_code_test(Support,ground(T),Test2),
  build_trigger(Support,[{Test2}|Triggers],Consequent,X).



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

build_neg_test(Support,T,Testin,Testout) :- must(nonvar(T)),
  build_code_test(Support,Testin,Testmid),
  conjoin((call_u(T)),Testmid,Testout).


%= this just strips away any currly brackets.

build_code_test(_Support,Test,TestO):-var(Test),!,must(nonvar(Test)),TestO=call_u(Test).
build_code_test(Support,{Test},TestO) :- !,build_code_test(Support,Test,TestO).
build_code_test(Support,Test,TestO):- code_sentence_op(Test),Test=..[F|TestL],must_maplist(build_code_test(Support),TestL,TestLO),TestO=..[F|TestLO],!.
build_code_test(Support,Test,Test):- must(pfc_mark_as(Support,p,Test,pfcCallCode)),!.
build_code_test(_,Test,Test).

code_sentence_op(Var):-var(Var),!,fail.
code_sentence_op(rhs(_)).
code_sentence_op(neg(_)).
code_sentence_op(-(_)).
code_sentence_op(~(_)).
code_sentence_op(\+(_)).
code_sentence_op(call_u(_)).
code_sentence_op(Test):-predicate_property(Test,meta_predicate(PP)),predicate_property(Test,built_in),  \+ (( arg(_,PP,N), N\=0)).

all_closed(C):- \+compound(C)->true;(functor(C,_,A),A>1,\+((arg(_,C,Arg),var(Arg)))),!.

%=

build_consequent(_      ,Test,Test):-var(Test),!.
build_consequent(_      ,Test,TestO):-var(Test),!,TestO=added(Test).
build_consequent(Support,rhs(Test),rhs(TestO)) :- !,build_consequent(Support,Test,TestO).
build_consequent(Support,Test,TestO):- code_sentence_op(Test),Test=..[F|TestL],maplist(build_consequent(Support),TestL,TestLO),TestO=..[F|TestLO],!.
build_consequent(Support,Test,Test):-must(pfc_mark_as(Support,p,Test,pfcCreates)),!.
build_consequent(_ ,Test,Test).

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



pfc_call_t_exact(Trigger) :- copy_term(Trigger,Copy),pfc_get_trigger_quick(Trigger),Trigger=@=Copy.

retract_t(Trigger) :-  retract_i(spft(Trigger,_,_)),ignore(retract(Trigger)).


pfc_assert_t(P0,Support0) :- 
  unnumbervars(P0+Support0,P+Support),
  (pfc_clause_i(P) ; (assert_i(P),pfc_add_trigger(P,Support))),
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
  clause_i(Head_copy,true),
  variant(Head,Head_copy).


foreachl_do(Binder,Body) :- Binder,pfcl_do(Body),fail.
foreachl_do(_,_).

% pfcl_do(X) executesf X once and always succeeds.
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
   assertz_if_new(UNNUMBEREDVARS),!. % was assert_i
pfc_add_support(P,FT) :- trace_or_throw(failed_pfc_add_support(P,FT)).


pfc_get_support(not(P),(Fact,Trigger)) :- nonvar(P),!, pfc_get_support(neg(P),(Fact,Trigger)).
pfc_get_support(P,(Fact,Trigger)) :- spft(P,Fact,Trigger)*->true;(nonvar(P),pfc_get_support_neg(P,(Fact,Trigger))).

% dont pfc_get_support_neg(\+ neg(P),(Fact,Trigger)) :- spft((P),Fact,Trigger).
pfc_get_support_neg(\+ (P),S) :- !, nonvar(P), pfc_get_support(neg(P),S).
pfc_get_support_neg(~ (P),S) :- !, nonvar(P), pfc_get_support(neg(P),S).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfc_rem_support(WhyIn,P,(Fact,Trigger)) :- var(P),!,copy_term(pfc_rem_support(Why,P,(Fact,Trigger)) ,Why),
  clause(spft(P,Fact,Trigger),true,Ref),
  ((clause(SPFC,true,Ref),
     (SPFC=@=spft(P,Fact,Trigger) -> 
        erase(Ref); 
       (wdmsg(=>(Why,~SPFC)),pfc_retract_or_warn_i(spft(P,Fact,Trigger)),nop(trace))),
   (var(P)->trace_or_throw(var(P));remove_if_unsupported_verbose(WhyIn,local,P)))).
pfc_rem_support(Why,(\+ N) , S):- pfc_rem_support(Why,neg(N),S).
pfc_rem_support(_Why,P,(Fact,Trigger)):-pfc_retract_or_warn_i(spft(P,Fact,Trigger)).

/*
% TODO not called yet
pfc_collect_supports_f_l(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports_f_l([]).
*/
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
is_relative(update(_)).
is_relative(replace(_)).
is_relative(rel(_)).
is_relative(-(_)).
is_relative(*(_)).

/*
% TODO not called yet
%= pfc_get_trigger_key(+Trigger,-Key)
%=
%= Arg1 is a trigger.  Key is the best term to index it on.

pfc_get_trigger_key(pt(Key,_),Key).
pfc_get_trigger_key(pk(Key,_,_),Key).
pfc_get_trigger_key(nt(Key,_,_),Key).
pfc_get_trigger_key(Key,Key).
*/

/*
% TODO not called yet
%=^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger base clause that stores the trigger.
%=
pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).
*/
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

pfc_retract_or_warn_i(X) :- retract_i(X),pfc_debug_trace("Success retract: ~p.",[X]),!.
pfc_retract_or_warn_i(X) :- \+ \+ X =spft(neg(_),_,_),!.
pfc_retract_or_warn_i(X) :- ground(X),pfc_debug_trace("Couldn't retract ~p.",[X]),!.
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
:- thread_local thlocal:pfc_trace_exec/0.
:- dynamic pfc_warnings/1.

pfc_traced(_):- pfc_tracing.

:- pfc_init_i(pfc_warnings(_), pfc_warnings(true)).



get_fa(PI,_F,_A):-var(PI),!.
get_fa(F/A,F,A):- !.
get_fa(PI,PI,_A):- atomic(PI),!.
get_fa(PI,F,A):- compound(PI),!,functor(PI,F,A).
get_fa(Mask,F,A):-get_functor(Mask,F,A).


clause_or_call(M:H,B):-var(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
clause_or_call(isa(I,C),true):-!,req(isa_asserted(I,C)).
clause_or_call(genls(I,C),true):-!,logOnError(req(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause(H,B)*->!;fail) ; clause(H,B)).
clause_or_call(H,true):- should_call_for_facts(H),no_repeats(logOnError(H)).


% as opposed to simply using clause(H,true).
should_call_for_facts(H):- get_functor(H,F,A),should_call_for_facts(H,F,A).
should_call_for_facts(_,F,_):- prologSideEffects(F),!,fail.
should_call_for_facts(H,_,_):- \+ predicate_property(H,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- pfcMark(pfcRHS,_,F,A),!,fail.
should_call_for_facts(_,F,A):- pfcMark(pfcMustFC,_,F,A),!,fail.
should_call_for_facts(_,F,_):- prologDynamic(F),!.
should_call_for_facts(_,F,_):- \+ pfcControlled(F),!.


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
/*
NOT NEEDED ANYMORE
pfc_trace_add(P) :-
  % this is here for upward compat. - should go away eventually.
  pfc_trace_add(P,(o,o)).
*/
/*
pfc_trace_add(bt(Head,Body)) :-
  % hack for now - never trace triggers.
  !.
pfc_trace_add(pt(Head,Body)) :-
  % hack for now - never trace triggers.
  !.
pfc_trace_add(nt(Head,Condition,Body)) :-
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
       -> pfc_debug_trace("~N% Adding (~q) ~q ~n",[F,P])
        ; pfc_debug_trace("~N% Adding (:) ~q    <-------- (~q <=TF=> ~q)~n",[P,(T),(F)])).

pfc_trace_addPrint_0(_,_).


pfc_trace_break(P,_S) :-
  pfc_spied(P,add) ->
   ((\+ \+ fmt("~N% Breaking on pfc_assert(~q)",[P])),
    break)
   ; true.

/*
pfc_trace_rem(Why,bt(Head,Body)) :-
  % hack for now - never trace triggers.
  !.
pfc_trace_rem(Why,pt(Head,Body)) :-
  % hack for now - never trace triggers.
  !.
pfc_trace_rem(Why,nt(Head,Condition,Body)) :-
  % hack for now - never trace triggers.
  !.
*/

pfc_trace_rem(Why,P) :-
  ((pfc_traced(P);pfc_traced(Why))
     -> (pfc_debug_trace('~N% Removing (~q) ~q.~n',[Why,P]))
      ; true),
  ((pfc_spied(P,rem);pfc_spied(P,Why))
     -> (fmt("~N% Breaking on remove(~q,~q)",[Why,P]), break)
   ; true),!.


pfc_trace :- pfc_trace(_).

pfc_trace(Form) :-
  assert_i(pfc_traced(Form)).

pfc_trace(Form,Condition) :-
  assert_i((pfc_traced(Form) :- Condition)).

pfc_spy(Form) :- pfc_spy(Form,[add,rem],true).

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
  erase_safe(clause_i(pfc_spied(Form,Mode), Condition, Ref),Ref),
  fail.
pfc_no_spy(_,_,_).

pfc_no_trace :- pfc_untrace.
pfc_untrace :- pfc_untrace(_).
pfc_untrace(Form) :- retractall_i(pfc_traced(Form)).

% needed:  pfc_trace_rule(Name)  ...


% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg) :- pfc_trace_msg('~q.',[Msg]).
pfc_trace_msg(Msg,Args) :-
    pfc_tracing,
    !,
    notrace((fresh_line, \+ \+  fmt(user_output, Msg, Args))).
pfc_trace_msg(_Msg,_Args).

pfc_watch :- assert_i(thlocal:pfc_trace_exec).

pfc_no_watch :-  retractall_i(thlocal:pfc_trace_exec).

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

:-pfc_warn.

pfc_warn(Msg,Args) :-
 (pfc_warnings(true); \+ pfc_silient),
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


:-dynamic(prologMacroHead/1).

compute_resolve(NewerP,OlderQ,SU,SU,(pfc_remove3(OlderQ),pfc_add(NewerP,S),pfc_rem1(conflict(NewerP)))):-
  must(correctify_support(SU,S)),
  wdmsg(compute_resolve(newer(NewerP-S)>older(OlderQ-S))).
compute_resolve(NewerP,OlderQ,S1,[u],Resolve):-compute_resolve(OlderQ,NewerP,[u],S1,Resolve),!.
compute_resolve(NewerP,OlderQ,SU,S2,(pfc_remove3(OlderQ),pfc_add(NewerP,S1),pfc_rem1(conflict(NewerP)))):-
  must(correctify_support(SU,S1)),
  wdmsg(compute_resolve((NewerP-S1)>(OlderQ-S2))).


compute_resolve(NewerP,OlderQ,Resolve):-
   supports_f_l(NewerP,S1),
   supports_f_l(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).


:-multifile(resolveConflict/1).
:-dynamic(resolveConflict/1).
:-multifile(resolverConflict_robot/1).
:-dynamic(resolverConflict_robot/1).
:-export(resolverConflict_robot/1).

resolveConflict(C):- must((resolveConflict0(C),
  show_call(is_resolved(C)),pfc_rem(conflict(C)))).
resolveConflict(C) :-
  fmt("~NHalting with conflict ~q~n", [C]),   
  must(pfc_halt(conflict(C))),fail.

is_resolved(C):-pfc_call(C),\+pfc_call(neg(C)).
is_resolved(C):-pfc_call(neg(C)),\+pfc_call(C).

:-must(nop(_)).

resolveConflict0(C) :- forall(must(pfc_negation_w_neg(C,N)),ignore(show_call_failure((nop(resolveConflict(C)),pp_why(N))))),
  ignore(show_call_failure((nop(resolveConflict(C)),pp_why(C)))), 
    doall((if_defined(resolverConflict_robot(C)),\+ is_resolved(C),!)),
    is_resolved(C),!.

resolverConflict_robot(N) :- forall(must(pfc_negation_w_neg(N,C)),forall(compute_resolve(C,N,TODO),debugOnError(show_call(TODO)))).
resolverConflict_robot(C) :- must((pfc_remove3(C),fmt("~nRem-3 with conflict ~q~n", [C]),pfc_run,sanity(\+C))).


pfc_prove_neg(G):-nop(trace), \+ pfc_bc_caching(G), \+ pfc_fact(G).

pred_head(Type,P):- no_repeats_u(P,(call(Type,P),\+ nonfact_metawrapper(P),compound(P))).

pred_head_all(P):- pred_head(pred_all,P).

nonfact_metawrapper(neg(_)).
nonfact_metawrapper(pt(_,_)).
nonfact_metawrapper(bt(_,_)).
nonfact_metawrapper(nt(_,_,_)).
nonfact_metawrapper(spft(_,_,_)).
nonfact_metawrapper(added(_)).
% we use the arity 1 forms is why 
nonfact_metawrapper(term_expansion(_,_)).
nonfact_metawrapper(P):- \+ current_predicate(_,P).
nonfact_metawrapper(P):- functor(P,F,_), 
   (prologSideEffects(F);if_defined(tNotForUnboundPredicates(F))).
nonfact_metawrapper(P):-rewritten_metawrapper(P).

rewritten_metawrapper(_):-!,fail.
%rewritten_metawrapper(isa(_,_)).
rewritten_metawrapper(C):-compound(C),functor(C,t,_).

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
%pred_r0(~(P)):- if_defined(~(P)).
%pred_r0(neg(P)):- (neg(P)).

pred_r0(P=>Q):- (P=>Q).
pred_r0(P<=>Q):- (P<=>Q).
pred_r0(P<=Q):- (P<=Q).

cnstrn(X):-term_variables(X,Vs),maplist(cnstrn0(X),Vs),!.
cnstrn(V,X):-cnstrn0(X,V).
cnstrn0(X,V):-when(nonvar(V),X).

rescan_pfc:-forall(clause(user:rescan_pfc_hook,Body),show_call_entry(Body)).

pfc_facts_and_universe(P):- (var(P)->pred_head_all(P);true),(meta_wrapper_rule(P)->show_call(no_repeats(debugOnError(P))) ; (no_repeats(debugOnError(P)))).

add_reprop(Var):- var(Var), !. % trace_or_throw(add_reprop(Var)).
add_reprop(neg(_Var)):-!.
add_reprop((H:-B)):- trace_or_throw(add_reprop((H:-B))).
add_reprop(Trigger):- assertz_if_new(pfc_queue(repropagate(Trigger),(g,g))).

repropagate(P):-  meta_wrapper_rule(P)*->repropagate0(P);fail.
repropagate(P):-  \+ predicate_property(P,_),'$find_predicate'(P,PP),PP\=[],!,forall(member(M:F/A,PP),must((functor(Q,F,A),repropagate0(M:Q)))).
repropagate(F/A):- atom(F),integer(A),!,functor(P,F,A),!,repropagate(P).
repropagate(F/A):- atom(F),var(A),!,repropagate(F).
repropagate(P):-  must(repropagate0(P)).

repropagate0(USER:P):- USER==user,!,repropagate0(P).
repropagate0(P):- forall(pfc_facts_and_universe(P),with_assertions(thlocal:pfc_debug_local,(once(fwd_ok(P)),pfc_fwd(P)))).


fwd_ok(P):-ground(P),!.
fwd_ok(if_missing(_,_)).
fwd_ok(idForTest(_,_)).
fwd_ok(clif(_)).
fwd_ok(_).
fwd_ok(P):-must(ground(P)),!.

pfc_facts_only(P):- (var(P)->(pred_head_all(P),\+ meta_wrapper_rule(P));true),(no_repeats(debugOnError(P))).


user:rescan_pfc_hook:- forall(pfc_facts_and_universe(P),with_assertions(thlocal:pfc_debug_local,pfc_fwd(P))).
user:rescan_pfc_hook:- forall(pfc_facts_and_universe(P),with_assertions(thlocal:pfc_debug_local,pfc_scan_tms(P))).
/*
user:rescan_pfc_hook:- forall(pred_head(pred_u0,P), 
                          forall(no_repeats(P,call(P)),
                            show_call(pfc_fwd(P)))).
*/

:- set_prolog_flag(access_level,user).

:-multifile(pfc_default/1).
:-dynamic(pfc_default/1).


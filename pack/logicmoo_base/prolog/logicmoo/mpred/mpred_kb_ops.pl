/** <module> 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_kb_ops.pl
:- module(mpred_kb_ops,
          [ deducedSimply/1,
            how_to_op/2,
            is_callable/1,
            lookup_inverted_op/3,
            mpred_call_1/1,
            mpred_call_2/1,
            mpred_call_3/1,
            mpred_op/2,
            naf/1,
            oncely/1,
            reduce_mpred_op/2,
            second_order/2,
            whenAnd/2
          ]).
:- meta_predicate 
        deducedSimply(0),
        is_callable(0),
        mpred_op(?, ?),
        naf(0),
        oncely(0),
        whenAnd(0,0).

:- include('mpred_header.pi').

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).



% oncely later will throw an error if there where choice points left over by call
:- meta_predicate(oncely(0)).
:- was_export(oncely/1).
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,req(Call).
oncely(Call):-once(Call).
% ================================================
% mpred_op/2
% ================================================

/*
query(t, req, G):- req(G).
query(_, _, Op, G):- dtrace(req(call(Op,G))).
once(A,B,C,D):-trace_or_throw(once(A,B,C,D)).
*/

second_order(_,_):-fail.

:- meta_predicate(deducedSimply(0)).
:- was_export(deducedSimply/1).
deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),not_asserted((Call)),nonvar(Fact),Body,dmsg((deducedSimply2(Call):-Fact)),!,show_call(why,(is_asserted(Fact),ground(Call))).

% deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),nonvar(Fact),Body,ground(Call),dmsg((deducedSimply1(Call):-Fact)),show_call(why,(is_asserted(Fact),ground(Call))).

:- meta_predicate(mpred_op(?,?)).
mpred_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_call(Op,  H )).
mpred_op(is_asserted,H):-!,is_asserted(H).
mpred_op(Op,     H ):- once(fully_expand(Op,H,HH)),H\=@=HH,!,mpred_op(Op, HH).
mpred_op(neg(_,Op),  H ):- !, show_call(why,not(mpred_op(Op,  H ))).
mpred_op(change(assert,Op),H):-!,must(mpred_modify(change(assert,Op),H)),!.
mpred_op(change(retract,Op),H):-!,must(mpred_modify(change(retract,Op),H)),!.
mpred_op(query(t,Ireq),  H ):-!, mpred_op(Ireq,H).
mpred_op(query(Dbase_t,_Ireq),  H ):-!, mpred_op(Dbase_t,H).
mpred_op(call(Op),H):-!,mpred_op(Op,H).
mpred_op(Op,((include(A)))):- wno_tl(t_l:already_in_file_term_expansion,mpred_op(Op,((load_data_file(A))))),!.
mpred_op(Op, call(H)):- nonvar(H),!, mpred_op(Op,H).
mpred_op(Op,  not(H)):- nonvar(H),!, mpred_op(neg(not,Op),H).
mpred_op(Op,'\\+'(H)):- nonvar(H),!, mpred_op(neg(('\\+'),Op),H).
mpred_op(Op,    ~(H)):- nonvar(H),!, mpred_op(neg(~,Op),H).
mpred_op(Op,     {H}):- nonvar(H),!, mpred_op(Op,H).

mpred_op(must,Call):- !,must(mpred_op(req,Call)).
mpred_op(once,Call):- !,once(mpred_op(req,Call)).

mpred_op(assertedOnly,Call):- !,w_tl(t_l:infInstanceOnly(Call),mpred_op(req,Call)).
mpred_op(_ , clause(H,B) ):- !, is_asserted(H,B).
mpred_op(_ , clause(H,B,Ref) ):- !,  is_asserted(H,B,Ref).
mpred_op(_ , (H :- B) ):- !, is_asserted(H,B).
mpred_op(clauses(Op),  H):-!,mpred_op((Op),  H).
mpred_op(_,C):- req(C).

:- was_export(whenAnd/2).
whenAnd(A,B):-A,ground(B),once(B).


% =======================================
% Transforming DBASE OPs
% ========================================

reduce_mpred_op(Op,Op2):-must(hotrace(transitive(how_to_op,Op,Op2))),!.
reduce_mpred_op(A,A).

how_to_op(assert(a),asserta_new).
how_to_op(assert(z),assertz_if_new).
how_to_op(retract(one),retract).
how_to_op(retract(all),retract_all).
how_to_op(retract(all),retractall).
how_to_op(change(assert,z),assertz_if_new).
how_to_op(change(assert,_),asserta_if_new).
how_to_op(change(retract,one),retract).
how_to_op(change(retract,_),retract_all).
how_to_op(asserta,asserta_new).
how_to_op(assertz,assertz_if_new).
how_to_op(call(W),W).
how_to_op(clauses(W),W).
how_to_op(assert,assert_if_new).
how_to_op(assert(_),asserta_new).
how_to_op(retract(_),retract_all).
how_to_op(conjecture,call).
how_to_op(query(t, req),req).
how_to_op(query(t, Req),Req).
how_to_op(change(Op,HOW),O):- !, O=..[Op,HOW].
how_to_op(HowOP,HowOP).


lookup_inverted_op(retract,assert,-).
lookup_inverted_op(retractall,assert,-).
lookup_inverted_op(assert_if_new,retract,+).
lookup_inverted_op(assert_new,retract,+).
lookup_inverted_op(assertz_if_new,retract,+).
lookup_inverted_op(assertz_new,retract,+).
lookup_inverted_op(asserta_if_new,retract,+).
lookup_inverted_op(asserta_new,retract,+).


% ================================================
% is_callable/req/naf
% ================================================

%:- was_dynamic(naf/1).
:- meta_predicate(naf(0)).
:- was_export(naf/1).
naf(Goal):- (\+ req(Goal)).

:- meta_predicate(is_callable(0)).
:- was_export(is_callable/1).
is_callable(C):-current_predicate(_,C),!.

/*
:- meta_predicate(export(0)).
:- meta_predicate(req(0)).
:- meta_predicate(mpred_call_0(0)).
:- meta_predicate(mpred_call_1(0)).
:- meta_predicate(mpred_call_2(0)).
:- meta_predicate(mpred_call_3(0)).
:- was_export(req/1).
req(V):-var(V),trace_or_throw(var_mpred_call(V)).
req(call(X)):-!,req(X).
req((X,Y)):-!,req(X),req(Y).
req((X;Y)):-!,req(X);req(Y).
req(call(X,Y)):-!,append_term(X,Y,XY),!,req(XY).
req(call(X,Y,Z)):-!,append_term(X,Y,XY),append_term(XY,Z,XYZ),!,req(XYZ).
req(Call):- 
 show_success(req,(( t_l:assert_op_override(OvOp), OvOp\=change(assert, _))),fail,
   must((reduce_mpred_op(OvOp,OvOpR),lookup_inverted_op(OvOpR,_,OverridePolarity),
     must((Call=..[Was|Apply],lookup_inverted_op(Was,InvertCurrent,_WasPol))),
   ((OverridePolarity ==('-') -> on_x_rtrace(show_call(why,apply(InvertCurrent,Apply))) ; on_x_rtrace(show_call(why,apply(Was,Apply)))))))).

req(Call):- mpred_call_0(Call).

mpred_call_0(Call):-predicate_property(Call,foreign),!,on_x_rtrace(Call).
mpred_call_0(Expand):- fully_expand(query(t,req),Expand,Call), Expand\=@=Call, !, loop_check(req(Call),mpred_call_3(Call)).
mpred_call_0(Call):- one_must(mpred_call_1(Call),mpred_call_2(Call)).
*/
mpred_call_1(Call):-current_predicate(_,Call),mpred_call_3(Call).
mpred_call_1(Call):-clause(prolog_xref:process_directive(D, Q),_),nonvar(D),D=Call,!, show_call(why,(prolog_xref:process_directive(Call,Q),must(nonvar(Q)),fmt(Q))).
mpred_call_2(Call):-predicate_property(Call,dynamic),!,is_asserted(Call).
mpred_call_2(Call):-mpred_call_3(Call).

% https://gist.githubusercontent.com/vwood/662109/raw/dce9e9ce9505443a82834cdc86163773a0dccc0c/ecldemo.c



:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

mpred_call_3(Call):-on_x_rtrace(loop_check(if_defined(Call))).



/* 
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
         %    mpred_call_1/1,mpred_call_2/1,mpred_call_3/1,
            mpred_op/2,
            naf/1,
            oncely/1,
            reduce_mpred_op/2,
            second_order/2,
            whenAnd/2,
          mpred_kb_ops_file/0
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

%= 	 	 

%% oncely( :GoalCall) is semidet.
%
% Oncely.
%
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


%= 	 	 

%% second_order( ?VALUE1, ?VALUE2) is semidet.
%
% Second Order.
%
second_order(_,_):-fail.

:- meta_predicate(deducedSimply(0)).
:- was_export(deducedSimply/1).

%= 	 	 

%% deducedSimply( :GoalCall) is semidet.
%
% Deduced Simply.
%
deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),not_asserted((Call)),nonvar(Fact),Body,dmsg((deducedSimply2(Call):-Fact)),!,show_call(why,(is_asserted(Fact),ground(Call))).

% deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),nonvar(Fact),Body,ground(Call),dmsg((deducedSimply1(Call):-Fact)),show_call(why,(is_asserted(Fact),ground(Call))).

:- meta_predicate(mpred_op(?,?)).

%= 	 	 

%% mpred_op( ?Op, ?H) is semidet.
%
% Managed Predicate Oper..
%
mpred_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_call(Op,  H )).
mpred_op(is_asserted,H):-!,is_asserted(H).
mpred_op(Op,     H ):- once(fully_expand(Op,H,HH)),H\=@=HH,!,mpred_op(Op, HH).
mpred_op(~(_,Op),  H ):- !, show_call(why,not(mpred_op(Op,  H ))).
mpred_op(change(assert,Op),H):-!,must(mpred_modify(change(assert,Op),H)),!.
mpred_op(change(retract,Op),H):-!,must(mpred_modify(change(retract,Op),H)),!.
mpred_op(query(t,Ireq),  H ):-!, mpred_op(Ireq,H).
mpred_op(query(Dbase_t,_Ireq),  H ):-!, mpred_op(Dbase_t,H).
mpred_op(call(Op),H):-!,mpred_op(Op,H).
mpred_op(Op,((include(A)))):- wno_tl(t_l:already_in_file_term_expansion,mpred_op(Op,((load_data_file(A))))),!.
mpred_op(Op, call(H)):- nonvar(H),!, mpred_op(Op,H).
mpred_op(Op,  not(H)):- nonvar(H),!, mpred_op(~(not,Op),H).
mpred_op(Op,'\\+'(H)):- nonvar(H),!, mpred_op(~(('\\+'),Op),H).
mpred_op(Op,    ~(H)):- nonvar(H),!, mpred_op(~(~,Op),H).
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

%= 	 	 

%% whenAnd( :GoalA, :GoalB) is semidet.
%
% When And.
%
whenAnd(A,B):-A,ground(B),once(B).


% =======================================
% Transforming DBASE OPs
% ========================================


%= 	 	 

%% reduce_mpred_op( ?Op, ?Op2) is semidet.
%
% Reduce Managed Predicate Oper..
%
reduce_mpred_op(Op,Op2):-must(hotrace(transitive(how_to_op,Op,Op2))),!.
reduce_mpred_op(A,A).


%= 	 	 

%% how_to_op( ?HowOP, ?HowOP) is semidet.
%
% How Converted To Oper..
%
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



%= 	 	 

%% lookup_inverted_op( ?VALUE1, ?VALUE2, +OUT3) is semidet.
%
% Lookup Inverted Oper..
%
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

%= 	 	 

%% naf( :GoalGoal) is semidet.
%
% Negation-By-Faliure.
%
naf(Goal):- (\+ req(Goal)).

:- meta_predicate(is_callable(0)).
:- was_export(is_callable/1).

%= 	 	 

%% is_callable( :GoalC) is semidet.
%
% If Is A Callable.
%
is_callable(C):-current_predicate(_,C),!.

mpred_kb_ops_file.

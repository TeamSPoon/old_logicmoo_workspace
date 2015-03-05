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


:-meta_predicate_transparent(mpred_call(0)).

% oncely later will throw an error if there where choice points left over by call
:-meta_predicate(mpred_call(0)).
:-export(mpred_call/1).
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,req(Call).
oncely(Call):-once(Call).
% ================================================
% dbase_op/2
% ================================================

/*
query(dbase_t, req, G):- mpred_call(G).
query(_, _, Op, G):- dtrace, mpred_call(call(Op,G)).
once(A,B,C,D):-trace_or_throw(once(A,B,C,D)).
*/


:-meta_predicate(deducedSimply(0)).
:-export(deducedSimply/1).
deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),not(is_asserted(Call)),nonvar(Fact),Body,dmsg((deducedSimply2(Call):-Fact)),!,show_call((is_asserted(Fact),ground(Call))).

% deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),nonvar(Fact),Body,ground(Call),dmsg((deducedSimply1(Call):-Fact)),show_call((is_asserted(Fact),ground(Call))).


dbase_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_call(Op,  H )).
dbase_op(is_asserted,H):-!,is_asserted(H).
dbase_op(Op,     H ):- once(fully_expand(Op,H,HH)),H\=@=HH,!,dbase_op(Op, HH).
dbase_op(neg(_,Op),  H ):- !, show_call(not(dbase_op(Op,  H ))).
dbase_op(change(assert,Op),H):-!,must(dbase_modify(change(assert,Op),H)),!.
dbase_op(change(retract,Op),H):-!,must(dbase_modify(change(retract,Op),H)),!.
dbase_op(query(dbase_t,Ireq),  H ):-!, dbase_op(Ireq,H).
dbase_op(query(Dbase_t,_Ireq),  H ):-!, dbase_op(Dbase_t,H).
dbase_op(call(Op),H):-!,dbase_op(Op,H).
dbase_op(Op,((include(A)))):- with_no_assertions(thlocal:already_in_file_term_expansion,dbase_op(Op,((load_data_file(A))))),!.
dbase_op(Op, call(H)):- nonvar(H),!, dbase_op(Op,H).
dbase_op(Op,  not(H)):- nonvar(H),!, dbase_op(neg(not,Op),H).
dbase_op(Op,'\\+'(H)):- nonvar(H),!, dbase_op(neg(('\\+'),Op),H).
dbase_op(Op,    ~(H)):- nonvar(H),!, dbase_op(neg(~,Op),H).
dbase_op(Op,     {H}):- nonvar(H),!, dbase_op(Op,H).

dbase_op(must,Call):- !,must(dbase_op(req,Call)).
dbase_op(once,Call):- !,once(dbase_op(req,Call)).

dbase_op(assertedOnly,Call):- !,with_assertions(thlocal:infInstanceOnly(Call),dbase_op(req,Call)).
dbase_op(_ , clause(H,B) ):- !, is_asserted(H,B).
dbase_op(_ , clause(H,B,Ref) ):- !,  is_asserted(H,B,Ref).
dbase_op(_ , (H :- B) ):- !, is_asserted(H,B).
dbase_op(clauses(Op),  H):-!,dbase_op((Op),  H).
dbase_op(_,C):- mpred_call(C).

:-export(whenAnd/2).
whenAnd(A,B):-A,ground(B),once(B).


user:agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.
user:agent_call_command(_Gent,actGrep(Obj)):- term_listing(Obj).


% =======================================
% Transforming DBASE OPs
% ========================================

reduce_dbase_op(Op,Op2):-must(notrace(transitive(how_to_op,Op,Op2))),!.
reduce_dbase_op(A,A).

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
how_to_op(query(dbase_t, req),mpred_call).
how_to_op(query(dbase_t, Req),Req).
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
% is_callable/mpred_call/naf
% ================================================

%:-dynamic(naf/1).
:-meta_predicate(naf(0)).
:-export(naf/1).
naf(Goal):-not(mpred_call(Goal)).

:-meta_predicate(is_callable(0)).
:-export(is_callable/1).
is_callable(C):-current_predicate(_,C),!.

:-meta_predicate(mpred_call(0)).
:-export(mpred_call/1).
mpred_call(call(X)):-!,mpred_call(X).
mpred_call((X,Y)):-!,mpred_call(X),mpred_call(Y).
mpred_call((X;Y)):-!,mpred_call(X);mpred_call(Y).
mpred_call(call(X,Y)):-!,append_term(X,Y,XY),!,mpred_call(XY).
mpred_call(call(X,Y,Z)):-!,append_term(X,Y,XY),append_term(XY,Z,XYZ),!,mpred_call(XYZ).
mpred_call(Call):- 
 show_call_success(( thlocal:assert_op_override(OvOp), OvOp\=change(assert, _))),fail,
   must((reduce_dbase_op(OvOp,OvOpR),lookup_inverted_op(OvOpR,_,OverridePolarity),
     must((Call=..[Was|Apply],lookup_inverted_op(Was,InvertCurrent,_WasPol))),
   ((OverridePolarity ==('-') -> debugOnError(show_call(apply(InvertCurrent,Apply))) ; debugOnError(show_call(apply(Was,Apply))))))).

mpred_call(Call):-fully_expand(query(dbase_t,mpred_call),Call,Expand),!,mpred_call_0(Expand).

mpred_call_0(Call):- one_must(mpred_call_1(Call),mpred_call_2(Call)).

mpred_call_1(Call):-current_predicate(_,Call),debugOnError(loop_check(Call)).
mpred_call_1(Call):-clause(prolog_xref:process_directive(D, Q),_),nonvar(D),D=Call,!, trace,show_call(prolog_xref:process_directive(Call,Q),fmt(Q)).
mpred_call_2(Call):-current_predicate(is_asserted/1),!,is_asserted(Call).

% https://gist.githubusercontent.com/vwood/662109/raw/dce9e9ce9505443a82834cdc86163773a0dccc0c/ecldemo.c

end_of_file.


tms_after_prolog_call(_):-!.
tms_after_prolog_call(C):-functor(C,F,_),not(user:mpred_prop(F,_)),!,ignore(resolve_if_false(C)),!.
tms_after_prolog_call(C):-checkNoArgViolation(C),resolve_if_false(C),!.
tms_after_prolog_call(_).


% ================================================
% call_typelect/2,4
% ================================================
% grab_argsIsa(F,Types):- fail,deducedSimply(user:mpred_prop(F,predArgTypes(Types))).
% grab_argsIsa(F,Types):- call_typelect([flag(+firstValue),+debugOnError,+deducedSimply],user:mpred_prop(F,predArgTypes(Types))).

call_typelect(Ops,Call):-call_typelect(Ops,Ops,Call,Call).

call_typelect(Ops,Flags,Call,Vars):-ground(Call),not(member(ground,Flags)),!,call_typelect(Ops,[ground|Flags],Call,Vars).
call_typelect(Ops,Flags,Call,Vars):-is_callable(Call),not(member(defined,Flags)),!,call_typelect(Ops,[defined|Flags],Call,Vars).
call_typelect(Ops,Flags,Call,Vars):-call_typelect_op(Ops,Flags,Call,Vars,[],Results),!,member(Vars,Results).

% complete
call_typelect_op([],_Flags,_Call,_,Results,Results):-!.

% early complete
call_typelect_op(_,Flags,_Call,_Vars,ResultsIn,ResultsOut):- member(firstValue,Flags),length(ResultsIn,L),L>0, member(Minus,[-(_),?(_)]), not(member(Minus,Flags)),!,
   must_det(ResultsIn=ResultsOut),!.

% push flag
call_typelect_op([flag(+Flag)|Op2],FlagsIn,Call,Vars,ResultsIn,ResultsOut):- !,
   ord_union(FlagsIn,[Flag],NewFlags),
   call_typelect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% remove flag
call_typelect_op([flag(-Flag)|Op2],FlagsIn,Call,Vars,ResultsIn,ResultsOut):- !,
   ord_subtract(FlagsIn,[Flag],NewFlags),
   call_typelect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% replace flags
call_typelect_op([flags(NewFlags)|Op2],_OldFlags,Call,Vars,ResultsIn,ResultsOut):- !,
   call_typelect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% sequential opers
call_typelect_op([Op1|Op2],Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   call_typelect_op(Op1,[doing(Op,Call)|Flags],Call,Vars,ResultsIn,ResultsMid),   
   call_typelect_op(Op2,[done(Op,Call)|Flags],Call,Vars,ResultsMid,ResultsOut).

% with_assertions
call_typelect_op(with_assertions(Assertions,Op2),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   with_assertions(Assertions, call_typelect_op(Op2,Flags,Call,Vars,ResultsIn,ResultsOut)).

% with_assertions
call_typelect_op(with_no_assertions(Assertions,Op2),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   with_no_assertions(Assertions, call_typelect_op(Op2,Flags,Call,Vars,ResultsIn,ResultsOut)).

% add to set
call_typelect_op(+Op,Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   findall_nodupes(Vars,call_one(Op,Flags,Call,Vars),ResultsN), 
   ord_union(ResultsIn,ResultsN,ResultsOut).

% remove from set
call_typelect_op(-Op,Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   findall_nodupes(Vars,(member(Vars,ResultsIn),not(call_one(Op,Flags,Call,Vars))),ResultsOut).

% retain only
call_typelect_op('?'(Op),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   findall_nodupes(Vars,(member(Vars,ResultsIn),call_one(Op,Flags,Call,Vars)),ResultsOut).

% unknown option
call_typelect_op(Op,Flags,Call,Vars,ResultsIn,ResultsOut):- 
   dmsg(error_call_typelect(Op,Flags,Call,Vars,ResultsIn,ResultsOut)),
     ResultsIn=ResultsOut.

% ================================================
% call_one/4
% ================================================
% and
call_one((Op1,Op2),Flags,Call,Vars):-  call_typelect(Op1,Flags,Call,Vars), call_typelect(Op2,Flags,Call,Vars).
% or
call_one((Op1;Op2),Flags,Call,Vars):- call_typelect(Op1,Flags,Call,Vars), call_typelect(Op2,Flags,Call,Vars).
% basic call
call_one(call,_Flags,Call,_Vars):- call(Call).
% not
call_one(not(Op1),Flags,Call,Vars):-  not(call_typelect(Op1,Flags,Call,Vars)).
% succeed vars
call_one(v(Vars),_Flags,_Call,Vars):-!.
% succeed call
call_one(c(Call),_Flags,Call,_Vars):-!.
% deduce_facts
call_one(deducedSimply,_Flags,Call,_Vars):-!, deducedSimply(Call).
% is_asserted
call_one(asserted,_Flags,Call,_Vars):-!, is_asserted(Call).
% macro/1
call_one(macro(Macro),_Flags,Call,Vars):- subst(Macro,c,Call,Macro1),subst(Macro1,v,Vars,Macro2),call(Macro2).
% call something else
call_one(Other,_Flags,Call,_Vars):- call(Other,Call).






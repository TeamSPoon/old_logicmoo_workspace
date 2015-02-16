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


:-meta_predicate_transparent(call_mpred(0)).

% oncely later will throw an error if there where choice points left over by call
:-meta_predicate(prologCall(0)).
:-export(prologCall/1).
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,req(Call).
oncely(Call):-once(Call).
% ================================================
% database_call/2
% ================================================

:-meta_predicate(call_mpred(0)).
:-export(call_mpred/1).
call_mpred(C):-database_call(call_mpred,C).

query(dbase_t, req, G):- prologCall(G).
query(_, _, Op, G):- dtrace, prologCall(call(Op,G)).

:-meta_predicate(is_callable(0)).
:-export(is_callable/1).
is_callable(C):-current_predicate(_,C),!.


:-dynamic(naf/1).
:-meta_predicate(naf(0)).
:-export(naf/1).
naf(Goal):-not(prologCall(Goal)).

:-meta_predicate(prologCall(0)).
:-export(prologCall/1).
prologCall(call(X)):-!,prologCall(X).
prologCall((X,Y)):-!,prologCall(X),prologCall(Y).
prologCall((X;Y)):-!,prologCall(X);prologCall(Y).
prologCall(call(X,Y)):-!,append_term(X,Y,XY),!,prologCall(XY).
prologCall(call(X,Y,Z)):-!,append_term(X,Y,XY),append_term(XY,Z,XYZ),!,prologCall(XYZ).
prologCall(Call):- 
  thlocal:assert_op_override(OvOp),
   must((reduce_dbase_op(OvOp,OvOpR),lookup_inverted_op(OvOpR,_,OverridePolarity),
   Call=..[Was|Apply],lookup_inverted_op(Was,InvertCurrent,_WasPol),   
   ((OverridePolarity ==('-') -> debugOnError(show_call(apply(InvertCurrent,Apply))) ; debugOnError(show_call(apply(Was,Apply))))))).
prologCall(Call):-current_predicate(_,Call),!,debugOnError(Call).
prologCall(Call):-debugOnError(is_asserted(Call)).

lookup_inverted_op(retract,assert,-).
lookup_inverted_op(retractall,assert,-).
lookup_inverted_op(assert_if_new,retract,+).
lookup_inverted_op(assert_new,retract,+).
lookup_inverted_op(assertz_if_new,retract,+).
lookup_inverted_op(assertz_new,retract,+).
lookup_inverted_op(asserta_if_new,retract,+).
lookup_inverted_op(asserta_new,retract,+).



:-meta_predicate(deducedSimply(0)).
:-export(deducedSimply/1).
deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),not(is_asserted(Call)),nonvar(Fact),Body,dmsg((deducedSimply2(Call):-Fact)),!,show_call((is_asserted(Fact),ground(Call))).

% deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),nonvar(Fact),Body,ground(Call),dmsg((deducedSimply1(Call):-Fact)),show_call((is_asserted(Fact),ground(Call))).

database_op_int_call(Op,A):- must((expand_goal_correct_argIsa(A,AA))),database_op_int_call_0(Op, ((AA))).
database_op_int_call_0(Op,((ensure_loaded(A)))):- database_op_int_2(Op,((load_data_file(A)))),!.
database_op_int_call_0(Op,((include(A)))):- database_op_int_2(Op,((load_data_file(A)))),!.
database_op_int_call_0(Op,A):- ((predicate_property(A,_)/*;is_mpred_callable(A)*/)),!,database_call(Op,A),!.
database_op_int_call_0(Op,A):- trace_or_throw(missing_directive(Op,A)),!.

database_call(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_call(Op,  H )).
database_call(_,true):-!.
database_call(Op,     H ):- once(demodulize(H,HH)),H\=@=HH,!,database_call(Op, HH).
database_call(call(Op),H):-!,database_call(Op,H).
database_call(Op, call(H)):- nonvar(H),!, database_call(Op,H).
database_call(Op,  not(H)):- nonvar(H),!, database_call(neg(not,Op),H).
database_call(Op,'\\+'(H)):- nonvar(H),!, database_call(neg(('\\+'),Op),H).
database_call(Op,    ~(H)):- nonvar(H),!, database_call(neg(~,Op),H).
database_call(Op,     {H}):- nonvar(H),!, database_call(Op,H).
database_call(must,Call):- !,must(database_call(req,Call)).
database_call(assertedOnly,Call):- !,with_assertions(thlocal:infInstanceOnly(Call),database_call(req,Call)).
database_call(once,Call):- !,once(database_call(req,Call)).
database_call(neg(_,Op),  H ):- !,trace, not(database_call(Op,  H )).
database_call(Op, clause(H,B) ):- !, database_check(clause(Op),clause(H,B)).
database_call(Op, clause(H,B,Ref) ):- !, database_check(clause(Op),clause(H,B,Ref)).
database_call(Op, (H :- B) ):- !, show_call(database_check(clause(Op),clause(H,B))).
database_call(is_asserted,H):-!,is_asserted(H).
database_call(conjecture,H):-!,prologCall(H).
database_call(nonPFC,  H ):-!, prologCall(H).
database_call(call_mpred,  H ):-!, prologCall(H).
database_call(query(dbase_t,Ireq),  H ):-!, database_call(Ireq,H).
database_call(query(Dbase_t,_Ireq),  H ):-!, database_call(Dbase_t,H).
database_call(Atom,H):-atom(Atom),current_predicate(Atom/1),!,debugOnError(call(Atom,H)).
database_call(_,C):- is_callable(C),!,prologCall(C).
database_call(Op,  H ):- trace_or_throw(unknown_database_call(Op,  H )).

:- export(whenAnd/2).
whenAnd(A,B):-A,ground(B),once(B).


end_of_file.


check_mcall_ok(_):-!.
check_mcall_ok(C):-functor(C,F,_),not(mpred_prop(F,_)),!,ignore(resolve_if_false(C)),!.
check_mcall_ok(C):-checkNoArgViolation(C),resolve_if_false(C),!.
check_mcall_ok(_).


% ================================================
% call_typelect/2,4
% ================================================
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






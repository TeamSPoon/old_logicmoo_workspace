



% =====================================
% = body_req
% =====================================
/*
:-export(hook:body_req/4).
not_dupe(HEAD):-must_det(last_arg_ground(HEAD)),predicate_property(HEAD,number_of_clauses(N)),N>1,not(clause_safe(HEAD,true)).
*/
:-export(last_arg_ground/1).
last_arg_ground(HEAD):-compound(HEAD),functor(HEAD,F,A),last_arg_ground(F,A,HEAD),!.
last_arg_ground(mud_test,_,_).
last_arg_ground(_,A,_):-A>2,!.
last_arg_ground(_,A,HEAD):-arg(A,HEAD,Arg),!,ground(Arg).


call_body_req(HEAD):- functor(HEAD,F,A),HEAD_T=..[F|ARGS],HEAD_T=..[dbase_t,F|ARGS],hook_body_req(F,A,HEAD,HEAD_T).


% ========================================================================================
% BODY STUB
% ========================================================================================

body_req_isa(I,C):-isa_backchaing(I,C).

body_call_cyckb(HEAD_T):-HEAD_T =.. [dbase_t|PLIST],!, thglobal:use_cyc_database, kbp_t(PLIST).

hook:body_req(F,A,HEAD,HEAD_T):- hotrace( hook_body_req(F,A,HEAD,HEAD_T)).

hook_body_req(F,A,HEAD,HEAD_T):- mpred_prop(F,prologOnly),!,dmsg(warn(hook_body_req(F,A,HEAD,HEAD_T))),fail.
hook_body_req(_,_,isa(I,C),_):- !, body_req_isa(I,C).
hook_body_req(_,_,_,dbase_t(C,I)):- !, body_req_isa(I,C).
hook_body_req(_,_,_ ,HEAD_T):- thlocal:useOnlyExternalDBs,!, body_call_cyckb(HEAD_T).
hook_body_req(F,A,HEAD,HEAD_T):- loop_check(body_req_0(F,A,HEAD,HEAD_T),fail).

:-export(body_req_0/4).
body_req_0(F,A,HEAD,HEAD_T):- not(ground(HEAD)),!,no_repeats(HEAD_T,body_req_1(F,A,HEAD,HEAD_T)).
body_req_0(F,A,HEAD,HEAD_T):- body_req_1(F,A,HEAD,HEAD_T),!. 

:-export(body_req_1/4).
body_req_1(F,A,HEAD,HEAD_T):- mpred_prop(F,call_tabled),!, call_tabled(body_req_2(F,A,HEAD,HEAD_T)).
body_req_1(F,A,HEAD,HEAD_T):- body_req_2(F,A,HEAD,HEAD_T).

body_req_2(F,_,HEAD,  _):-    mpred_prop(F,external(Module)),!,call(Module:HEAD).
body_req_2(F,A,HEAD,HEAD_T):- body_req_3(F,A,HEAD,HEAD_T).

body_req_3(_,_,_  , HEAD_T):- clause(HEAD_T,true).
body_req_3(_,_,HEAD, _):- clause(HEAD,true),dmsg(warn(clause(HEAD,true))).
body_req_3(_,_,HEAD, _):-  moo:hybrid_rule(HEAD,BODY),call_mpred_body(HEAD,BODY).
body_req_3(F,_,_,dbase_t(F,Obj,LValue)):- choose_val(F,Obj,LValue).

body_req_plus_cyc(F,_,_,HEAD_T):- mpred_prop(F,cycPlus2(_)),!,with_assertions(thglobal:use_cyc_database,body_call_cyckb(HEAD_T)).


call_mpred_body(_HEAD,BODY):- debugOnError(BODY). % ,dmsg((succeed_hybrid_rule(HEAD:-BODY))).

foo_b(b1).
foo_b(b2):-!.
foo_b(b3):-!.

:-must_det((findall(R,call_no_cuts(foo_b(R)),List),length(List,3))).


%:-rescan_mpred_props.



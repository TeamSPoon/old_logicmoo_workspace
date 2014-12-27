



% =====================================
% = body_req
% =====================================
/*
:-swi_export(body_req/4).
not_dupe(HEAD):-must_det(last_arg_ground(HEAD)),predicate_property(HEAD,number_of_clauses(N)),N>1,not(clause_safe(HEAD,true)).
*/
:-swi_export(last_arg_ground/1).
last_arg_ground(HEAD):-compound(HEAD),functor(HEAD,F,A),last_arg_ground(F,A,HEAD),!.
last_arg_ground(mud_test,_,_).
last_arg_ground(_,A,_):-A>2,!.
last_arg_ground(_,A,HEAD):-arg(A,HEAD,Arg),!,ground(Arg).


call_body_req(HEAD):- functor(HEAD,F,A),HEAD_T=..[F|ARGS],HEAD_T=..[dbase_t,F|ARGS],hook_body_req(F,A,HEAD,HEAD_T).


% ========================================================================================
% BODY STUB
% ========================================================================================

body_req_isa(I,C):-isa_backchaing(I,C).

body_call_cyckb(HEAD_T):-el_holds_DISABLED_KB, HEAD_T =.. [dbase_t|PLIST], thglobal:use_cyc_database,!, no_repeats(kbp_t(PLIST)).

body_req(F,A,HEAD,HEAD_T):- hotrace(hook_body_req(F,A,HEAD,HEAD_T)).

:-swi_export(body_req_normal/4).
hook_body_req(F,A,HEAD,HEAD_T):- mpred_prop(F,prologOnly),!,dmsg(warn(hook_body_req(F,A,HEAD,HEAD_T))),fail.
hook_body_req(_,_,isa(I,C),_):- !, body_req_isa(I,C).
hook_body_req(_,_,_,dbase_t(C,I)):- !, body_req_isa(I,C).
hook_body_req(_,_,_,hasInstance(C,I)):- !, body_req_isa(I,C).
hook_body_req(_,_,_ ,HEAD_T):- thlocal:useOnlyExternalDBs,!, body_call_cyckb(HEAD_T).
% loop checking is not usefull (why the cut was added)
hook_body_req(F,A,HEAD,HEAD_T):- !, no_repeats(body_req_normal(F,A,HEAD,HEAD_T)).
/*
hook_body_req(F,A,HEAD,HEAD_T):- 
      loop_check(body_req_normal(F,A,HEAD,HEAD_T),
% this makes sense
      loop_check(body_req_normal(F,A,HEAD,HEAD_T),
      loop_check(body_req_maybe_rules(F,A,HEAD,HEAD_T),
      loop_check(body_req_maybe_rules(F,A,HEAD,HEAD_T),
% this is likely a rule calling this
      loop_check(body_req_no_rules(F,A,HEAD,HEAD_T),
      loop_check(body_req_begin_panic(F,A,HEAD,HEAD_T),
      loop_check(body_req_end_panic(F,A,HEAD,HEAD_T),fail))))))).
*/

% this is sober
body_req_maybe_rules(F,A,HEAD,HEAD_T):- ground(HEAD),body_req_with_rules(F,A,HEAD,HEAD_T),!.
body_req_maybe_rules(F,A,HEAD,HEAD_T):- body_req_no_rules(F,A,HEAD,HEAD_T).

% fact check and break
body_req_begin_panic(_,_,HEAD,HEAD_T):- copy_term(HEAD,COPY),HEAD_T,!,dmsg(looping(COPY)).

%  break
body_req_end_panic(_,_,HEAD,HEAD_T):- ignore(show_call(HEAD_T)),dmsg(failing_looping_true(HEAD)),!,fail.

:-swi_export(body_req_normal/4).
body_req_normal(F,A,HEAD,HEAD_T):- not(ground(HEAD)),!,no_repeats(HEAD_T,body_req_1(F,A,HEAD,HEAD_T)).
body_req_normal(F,A,HEAD,HEAD_T):- body_req_1(F,A,HEAD,HEAD_T),!. 

:-swi_export(body_req_1/4).
body_req_1(F,A,HEAD,HEAD_T):- mpred_prop(F,call_tabled),!, call_tabled(body_req_2(F,A,HEAD,HEAD_T)).
body_req_1(F,A,HEAD,HEAD_T):- body_req_2(F,A,HEAD,HEAD_T).

body_req_2(F,_,HEAD,  _):-    mpred_prop(F,external(Module)),!,call(Module:HEAD).
body_req_2(F,A,HEAD,HEAD_T):- body_req_with_rules(F,A,HEAD,HEAD_T).

body_req_with_rules(F,A,HEAD,HEAD_T):-body_req_no_rules(F,A,HEAD,HEAD_T).
body_req_with_rules(F,A,HEAD,HEAD_T):-body_req_only_rules(F,A,HEAD,HEAD_T).

body_req_no_rules(_,_,HEAD, _):-     clause(HEAD,  true).
body_req_no_rules(_,_,_  , HEAD_T):- clause(HEAD_T,true).
body_req_no_rules(F,_,_,HEAD_T):- body_req_plus_cyc(F,_,_,HEAD_T).

body_req_only_rules(_,_,HEAD, _):-  hybrid_rule(HEAD,BODY),call_mpred_body(HEAD,BODY).
body_req_only_rules(_,_,_,dbase_t(F,Obj,LValue)):-  choose_val(F,Obj,LValue).

body_req_plus_cyc(F,_,_,HEAD_T):-  mpred_prop(F,cycPlus2(_)),thlocal:useOnlyExternalDBs,!,with_assertions(thglobal:use_cyc_database,body_call_cyckb(HEAD_T)).

call_mpred_body(_,true):-!.
call_mpred_body(HEAD,BODY):- no_repeats(loop_check(call_mpred_body_lc(HEAD,BODY))).

call_mpred_body_lc(_HEAD,BODY):- debugOnError(BODY).

foo_b(b1).
foo_b(b2):-!.
foo_b(b3):-!.

:-must_det((findall(R,call_no_cuts(foo_b(R)),List),length(List,3))).


%:-rescan_mpred_props.







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

body_req_isa(I,C):- loop_check(body_req_isa_lc(I,C),dbase_t(C,I)).

body_req_isa_lc(I,C):-catch(isa_backchaing(I,C),_,alt_dbase_t(C,I)).

alt_dbase_t(C,I):-clause(dbase_t(C,I),true).
alt_dbase_t(C,I):-clause(isa(I,C),true).
alt_dbase_t(C,I):-atom(C),alt_dbase_atom(C,I).

alt_dbase_atom(C,I):-clause(mpred_prop(I,C),true).
alt_dbase_atom(formattype,I):- clause(ft_info(I,_),true).
alt_dbase_atom(formattype,I):- clause(subft(I,_),true).
alt_dbase_atom(C,I):- G=..[C,I],predicate_property(G,number_of_clauses(_)),!,clause(G,true).

hook:body_req(F,A,HEAD,HEAD_T):-hook_body_req(F,A,HEAD,HEAD_T).

hook_body_req(F,_A,_HEAD,_HEAD_T):- mpred_prop(F,prologOnly),!,fail.
hook_body_req(_,_,isa(I,C),_):- !, body_req_isa(I,C).
hook_body_req(_,_,_,dbase_t(C,I)):- !, body_req_isa(I,C).
% % % hook_body_req(F,A,HEAD,HEAD_T):- predicate_property(HEAD,number_of_clauses(1)),!,body_req_1(F,A,HEAD,HEAD_T).
hook_body_req(F,A,HEAD,HEAD_T):- fail, ignore((clause(HEAD,(hook:body_req(F,A,HEAD,HEAD_T)), Ref),nth_clause(_, Nth, Ref), Nth > 1, 
      asserta(':-'(HEAD,(hook:body_req(F,A,HEAD,HEAD_T)))))),fail. % ,erase(Ref))),fail.


hook_body_req(_,_,_,dbase_t(Prop,Obj,LValue)):-!,choose_val(Prop,Obj,LValue).

hook_body_req(F,A,HEAD,HEAD_T):- body_req_0(F,A,HEAD,HEAD_T).

:-export(body_req_0/4).
body_req_0(F,A,HEAD,HEAD_T):- not(ground(HEAD)),!,body_req_1(F,A,HEAD,HEAD_T).
body_req_0(F,A,HEAD,HEAD_T):- body_req_3(F,A,HEAD,HEAD_T),!. % ,ignore((not(last_arg_ground(HEAD)),rtrace(body_req_0(F,A,HEAD,HEAD_T)))),not_dupe(HEAD).

:-export(body_req_1/4).
body_req_1(F,A,HEAD,HEAD_T):- mpred_prop(F,call_tabled),!, call_tabled(body_req_2(F,A,HEAD,HEAD_T)).
body_req_1(F,A,HEAD,HEAD_T):- body_req_2(F,A,HEAD,HEAD_T).

body_req_2(F,A,HEAD,HEAD_T):- body_req_3(F,A,HEAD,HEAD_T).

body_req_3(_,_,_    ,HEAD_T):- clause(HEAD_T,true).
body_req_3(F,_,HEAD,  _):- mpred_prop(F,external(Module)),!,call(Module:HEAD).
body_req_3(_,_,HEAD,_HEAD_T):- clause(HEAD,true).
% body_req_3(_,_,HEAD,  _):- predicate_property(HEAD,number_of_clauses(N)),N>100,!,fail. % body_req is possibly unneeded now
body_req_3(_F,_,HEAD,  _):- not(save_in_dbase_t),req(HEAD).




foo_b(b1).
foo_b(b2):-!.
foo_b(b3):-!.

:-must_det((findall(R,call_no_cuts(foo_b(R)),List),length(List,3))).


:-rescan_mpred_props.



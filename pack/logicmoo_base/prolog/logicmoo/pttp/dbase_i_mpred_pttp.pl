/** <module> logicmoo_i_mpred_pttp
% Provides a prolog database replacent that uses PTTP
%
%  wid/3
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- user:ensure_loaded(logicmoo(mpred/logicmoo_i_header)).

ainz(A):-if_defined(pfc_assertz(A),assertz_new(A)).
%:-export(internal_functor/1).
%:-export(was_pttp_functor/1).
%:-dynamic(was_pttp_functor/1).
:-multifile(user:wid/3).
:-dynamic(user:wid/3).
:-export(int_query/7).
:-dynamic(int_query/7).
:-export(int_not_query/7).
:-dynamic(int_not_query/7).

% -- CODEBLOCK
:-export(pttp_ask/1).
pttp_ask(CALL):-nonegate(_KB,CALL,NNCALL),correct_pttp(NNCALL,REALCALL),apply(REALCALL,[ [], [], 100, _OneHudred, _Proof, [_In|[]]]).

%:-dynamic(was_pttp_functor/2).

/*
% -- CODEBLOCK
%=% Substitution
:-export(subst_eq/4).
% Usage: subst_eq(+Fml,+X,+Sk,?FmlSk)
subst_eq(Fml,X,Sk,FmlSkO):-pred_subst(==,Fml,X,Sk,FmlSk),!,must(FmlSkO=FmlSk),!.


% -- CODEBLOCK
% Usage: pred_subst(+Pred,+Fml,+X,+Sk,?FmlSk)
:-export(pred_subst/5).

pred_subst( Pred, P,       X,Sk,       P1    ) :- call(Pred,P,X),!,must( Sk=P1),!.
pred_subst(_Pred, P,       _,_ ,       P1    ) :- is_ftVar(P),!, must(P1=P),!.
pred_subst( Pred,[P|Args], X,Sk,    [P1|ArgS]) :- !, pred_subst(Pred,P,X,Sk,P1),!, must(pred_subst( Pred, Args,X, Sk, ArgS )),!.
pred_subst( Pred, P,       X,Sk,       P1    ) :- compound(P),!, P =..Args, pred_subst( Pred, Args,X, Sk, ArgS ),!, must(P1 =..ArgS),!.
pred_subst(_Pred ,P,       _, _,       P     ).

% -- CODEBLOCK
:-export(must/1).
:-meta_predicate(must(0)).
must(Call):-(repeat, (catch(Call,E,(dmsg(E:Call),debug,fail)) *-> true ; (ignore(ftrace(Call)),leash(+all),repeat,wdmsg(failed(Call)),trace,Call)),!).

*/

% --
:-export(pttp_call/1).
pttp_call(Goal) :- !,pttp_call(Goal,70,0,3,[],_,no).
pttp_call(Goal,Max,Min,Inc,ProofIn,ProofOut,ShowProof):-
  pttp_prove(Goal,Max,Min,Inc,ProofIn,ProofOut,ShowProof).


% -- CODEBLOCK
:-export(pttp_load_wid/1).
pttp_load_wid(Name):-must(pttp_logic(Name,Data)),!,must(pttp_load_wid(Name,Data)).
:-export(pttp_load_wid/2).
pttp_load_wid(Name,Data):- must(retractall_wid(Name)),wdmsg(pttp_load_wid(Name)),must(pttp_tell_wid(Name:0,Data)),!.
uses_logic(Name):-pttp_logic(Name,Data),pttp_load_wid(Name,Data).


% -- CODEBLOCK
:-export(pttp_assert/1).
pttp_assert(X) :- must_pttp_id(ID),pttp_tell_wid(ID,X).

% -- CODEBLOCK
:-export(pttp_tell_wid/2).
pttp_tell_wid(ID,XY):- 
    with_no_mpred_expansions(
       with_assertions(thlocal:disable_mpred_term_expansions_locally,
          with_assertions(thlocal:infSkipFullExpand,must(pttp_assert_wid(ID,pttp,XY))))),!.

:-export(pttp_assert_wid/3).
pttp_assert_wid(ID,Mode,(X,Y)):- !, pttp_assert_wid(ID,Mode,X),kb_incr(ID,ID2), pttp_assert_wid(ID2,Mode,Y).
pttp_assert_wid(ID,Mode,[X|Y]):- !, pttp_assert_wid(ID,Mode,X),kb_incr(ID,ID2), pttp_assert_wid(ID2,Mode,Y).
pttp_assert_wid(ID,Mode,(call:-CALL)):-!,pttp_assert_wid(ID,Mode,(call(CALL))).
pttp_assert_wid(_, _Mode,uses_logic(Name)):-!,must(pttp_logic(Name,Data)),!,must(pttp_load_wid(Name,Data)).
pttp_assert_wid(ID,_Mode,kif(YY)):-!, must((numbervars(YY,'$VAR',7567,_),must(pttp_assert_wid(ID,kif,YY)))).
pttp_assert_wid(ID,_Mode,call(CALL)):-!, must((save_wid(ID,call,call(CALL)),unnumbervars(CALL,RCALL),show_call_failure(must(RCALL)))).
%pttp_assert_wid(ID,Mode,(query:-B)):- must(assertz_unumbered((query:-B))),PNF =(query:-B), must( pttp_nnf(PNF,X)),!,must(must(pttp_assert_real_wid(ID,X))).
pttp_assert_wid(ID,pttp,X):- must((bugger:with_assertions(thlocal:current_why(ID,X), must(( pttp1_wid(ID,X,Y), pttp2_wid(ID,Y)) )))).
% pttp_assert_wid(ID,Mode,KIF):- must(kif_tell(ID,KIF)),!.
pttp_assert_wid(ID,kif,X):- show_call_failure(must(kif_tell(ID,X))).

pttp_assert_wid(ID,pttp_in,HB):- !,must((save_wid(ID,pttp_in,HB), pttp_assert_real_wid(ID,HB))).
pttp_assert_wid(ID,Mode,(X:-Y)):- !,must((save_wid(ID,Mode,(X:-Y)), pttp_assert_real_wid(ID,(X:-Y)))).
pttp_assert_wid(ID,_Mode,X):-  show_call_failure(must(pttp_assert_real_wid(ID,X))),!.
pttp_assert_wid(ID,_Mode,PNF):-  must( pttp_nnf(PNF,X)),!,must(must(pttp_assert_real_wid(ID,X))).

infer_by(_).

% -- CODEBLOCK
:-export(pttp_assert_real_wid/2).
pttp_assert_real_wid(ID,X):-
  must( pttp1_wid(ID,X,Y)),!, must(pttp_assert_int_wid(ID,Y)),!.


% -- CODEBLOCK
:-export(static_predicate/2).
:-meta_predicate(static_predicate(0,?)).
static_predicate(M:(Y:-_),Why):-!,static_predicate(M:Y,Why).
static_predicate((Y:-_),Why):-!,static_predicate(Y,Why).
static_predicate(_:Y,file(F)):-!,predicate_property(_:Y,file(F)),not(predicate_property(_:Y,dynamic)).
static_predicate(Y,file(F)):-predicate_property(_:Y,file(F)),not(predicate_property(_:Y,dynamic)).


% -- CODEBLOCK
:-export(pttp_assert_int_wid_for_conjuncts/3).
:-meta_predicate(pttp_assert_int_wid_for_conjuncts(+,0,+)).
pttp_assert_int_wid_for_conjuncts(ID,Y,_):- must(pttp_assert_int_wid(ID,Y)).


% -- CODEBLOCK
:-export(save_wid/3).
save_wid(IDWhy,Atom,Wff):-must(Atom\=','),to_numbered_ground(user:wid(IDWhy,Atom,Wff),Assert),show_call_failure(ainz(Assert)).

to_numbered_ground(I,O):-ground(I)->I=O;(copy_term(I,M),numbervars(M,'$VAR',766,_),O=M->true;trace_or_throw(to_numbered_ground(I,O))).

% -- CODEBLOCK
clauses_wid(ID,ID:R,F,Y,Ref):-atomic(ID),!,nonvar(ID),clause(user:wid(ID:R,F,Y),true,Ref).
clauses_wid(ID,ID,F,Y,Ref):-clause(user:wid(ID,F,Y),true,Ref).

% -- CODEBLOCK
:-export(retract_if_no_wids/1).
retract_if_no_wids(Y):- \+ user:wid(_,_,Y) -> retractall_matches(Y) ; true.

% -- CODEBLOCK
:-export(is_wid_key/2).
is_wid_key(Other,_):-compound(Other),not(not(is_wid_key2(Other))).
   is_wid_key2(C:N):-number(N),!,(compound(C);atom(C)),!.
   is_wid_key2(+N):-!,nonvar(N),!,is_wid_key2(N).
   is_wid_key2(-N):-!,nonvar(N),!,is_wid_key2(N).
   is_wid_key2(_:N):-!,nonvar(N),!,is_wid_key2(N).
   is_wid_key2(N):-number(N),!.
   is_wid_key2(N):-compound(N),!,N=..[_,A],!,number(A).


% -- CODEBLOCK
erase_safe_pttp(_,Ref):-erase(Ref).

% -- CODEBLOCK
retractall_matches(Y):-unnumbervars(Y,YY),retractall_matches_0(YY).
retractall_matches_0((Y:-B)):-!,pred_subst(is_wid_key,B,_,_,BB),forall(clause(Y,BB,Ref),erase_safe_pttp(clause(Y,BB,Ref),Ref)).
retractall_matches_0(Y):-forall(clause(Y,true,Ref),erase_safe_pttp(clause(Y,true,Ref),Ref)).

% -- CODEBLOCK
:-export(retractall_wid/1).
retractall_wid(ID):- 
 forall(clauses_wid(ID,A,B/C,Y,Ref),must(show_call_failure((erase_safe_pttp(clauses_wid(ID,A,B/C,Y,Ref),Ref),retract_if_no_wids(Y))))),
 forall(clauses_wid(ID,A,B,Y,Ref),must(erase_safe_pttp(clauses_wid(ID,A,B,Y,Ref),Ref))).

% -- CODEBLOCK
:-export(listing_wid/1).
:-export(listing_wid/0).
listing_wid:- listing_wid(_).
listing_wid(ID):- forall(((no_repeats(RID,clauses_wid(ID,RID,_,_,_)))),write_rid(RID)).

% -- CODEBLOCK
:-export(write_rid/1).
write_rid(RID):- 
((nl,write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% '),nl,write('%   '),
   write(RID),nl,
   write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n '),nl)),!,
  forall(((clauses_wid(RID,RID,Atomic,Wff,_),
          Atomic\=pttp_nnf,atomic(Atomic))),
   (write('% '),write(Atomic), write(': '), ansicall(green,portray_clause_0( Wff )),nl)),
  forall(no_repeats(FY,(clauses_wid(RID,RID,NA,FY,_),compound(NA))),
    show_wid_int(FY)).

:- style_check(+singleton).

show_wid_int(FY):-once((reassemble_intified(FY,FYI),renumbervars_a(FYI,FY2),unnumbervars(FY2,FY3), portray_clause_0( FY3 ),nl)).

reassemble_intified(H :- B, OUT ):- 
   H=..HEADL,append(HARGS,[_G, _E, _A, _M, _F, _O, _D],HEADL),
   grab_body(B,BOD), FHARGS=..HARGS,
    OUT = (FHARGS :- BOD),!.
reassemble_intified(H, FHARGS ):-compound(H),H=..HEADL,append(HARGS,[_G, _E, _A, _M, _F, _O, _D],HEADL), FHARGS=..HARGS,!.
reassemble_intified(OUT,OUT).

grab_body(call_proof(_,A),A):-!.
grab_body((A,B),AB):-grab_body(A,AA),grab_body(B,BB),conjoin_pttp(AA,BB,AB).
grab_body(_,true).

:-export(portray_clause_0/1).
portray_clause_0( (AA:-BB) ):- !, renumbervars((AA:-BB) ,(A:-B) ),portray_clause(user_output,(A:-B),[numbervars(true)]).
portray_clause_0( (A;B) ):-writeq((A;B)),nl,!.
portray_clause_0( AB ):- portray_clause(user_output,(AB),[numbervars(true)]).



% -- CODEBLOCK
:-export(clear_pttp/0).
clear_pttp:- eraseall(int_query,_),eraseall(int_not_query,_),
  forall(wid(ID,_/_,_),retractall(wid(ID,_,_))),
  forall(was_pttp_functor(internal,F,A),(abolish(F,A),dynamic(F/A))).

%eraseall(M:F,A):-!,functor(C,F,A),forall(clause(M:C,_,X),erase(X)).
%eraseall(F,A):-current_predicate(F/A),functor(C,F,A),forall(clause(C,_,X),erase(X)).                                                            


% -- CODEBLOCK
:-dynamic(pttp_test_took/3).

:-export(do_pttp_test_maybe/1).
:-discontiguous(pttp_test_fails_is_ok/1).
:-dynamic(pttp_test_fails_is_ok/1).
:-export(do_pttp_test_maybe/2).

do_pttp_test_maybe(TestName):- forall(pttp_test(TestName,Data),do_pttp_test_maybe(TestName,Data)),lsting(pttp_test_took).
do_pttp_test_maybe(TestName,_) :- pttp_test_fails_is_ok(TestName),!.
do_pttp_test_maybe(TestName,Data) :- do_pttp_test(TestName,Data).

:-export(do_pttp_test/1).
do_pttp_test(TestName):- forall(pttp_test(TestName,Data),do_pttp_test(TestName,Data)),listing(pttp_test_took).
:-export(do_pttp_test/2).
do_pttp_test(TestName,Data) :-   
           call_cleanup((     
             catch((      
               clear_pttp,
                must_det_l([
                          dmsg(do_pttp_test(TestName)),
                          retractall_wid(TestName),
                           eraseall(int_query,_),eraseall(int_not_firstOrder,_),eraseall(int_firstOrder,_),                                                           
                               pttp_tell_wid(TestName:0,Data), 
                               once((ignore(call_print_tf(pttp_test_prove(TestName,query))))),
                               sleep(1)])),E,dmsg(error(TestName:E)))),retractall_wid(TestName)).
                              

% -- CODEBLOCK
:-export(pttp_test_prove/2).
pttp_test_prove(TestName,_):- pttp_test_query(TestName,Other),!,call30timed(TestName,Other).
pttp_test_prove(TestName,A):- call30timed(TestName,pttp_prove(A)).

call30timed(TestName,CALL):-  
   statistics(cputime, D),
        (   CALL  % catch(call_with_time_limit(CALL,30),time_limit_exceeded,(wdmsg(error_time_limit_exceeded(CALL)),fail))
        ->  B=success
        ;   B=failure
        ),
        statistics(cputime, C),
        F is C-D,
        ainz(pttp_test_took(TestName,B,F)),!,
        B=success.


:-export(call_print_tf/1).
:-meta_predicate(call_print_tf(0)).
call_print_tf(G):-(G *-> dmsg(succceeded(G)) ; (dmsg(warning(error(failed_finally(G)))),sleep(5))).

:-export(do_pttp_tests/0).
do_pttp_tests :- do_pttp_test_maybe(_), 
   forall(pttp_test_took(Test, failure, _Time),gripe_pttp_failure(Test)).

gripe_pttp_failure(Test):- pttp_test_fails_is_ok(Test),!.
gripe_pttp_failure(Test):- dmsg(gripe_pttp_failure(Test)),!.
gripe_pttp_failure(Test):- ignore(pttp_test_took(Test, failure, Time)),trace_or_throw(pttp_test_took(Test, failure, Time)).

:-multifile(user:sanity_test/0).
% user:sanity_test :- do_pttp_tests.


:-export(isNegOf/2).
isNegOf(N1,N):-number(N),!,N1 is -N.
isNegOf(N,-N):-not_ftVar(N),!.
isNegOf(-N,N):-not_ftVar(N),!.
isNegOf(N1,N):-dtrace(not(isNegOf(N1,N))),isNegOf(N,N1).

:-export(kb_incr/2).
kb_incr(WffNum1 ,WffNum2):-is_ftVar(WffNum1),trace_or_throw(kb_incr(WffNum1 ,WffNum2)).
kb_incr(WffNum1 ,WffNum2):-number(WffNum1),WffNum2 is WffNum1 + 1,!.
%kb_incr(WffNum1 ,WffNum2):-atom(WffNum1),WffNum2=..[WffNum1,0],!.
kb_incr(WffNum1 ,WffNum2):-atomic(WffNum1),WffNum2 = WffNum1:0,!.
kb_incr(WffNum1 ,WffNum2):-WffNum1=..[F,P,A|REST],kb_incr(A ,AA),!,WffNum2=..[F,P,AA|REST].
kb_incr(WffNum1 ,WffNum2):-trace_or_throw(kb_incr(WffNum1 ,WffNum2)).

:-multifile was_pttp_functor/3.
:-dynamic was_pttp_functor/3.


% -- CODEBLOCK


was_pttp_functor(internal, query,7).


% -- CODEBLOCK
int_listing_wid0:-
  forall(was_pttp_functor(external,F,A),catch(prolog_list:listing(F/A),_,fail)),
  forall(was_pttp_functor(internal,F,A),catch(prolog_list:listing(F/A),_,fail)),!.

int_listing_wid:-
  forall(was_pttp_functor(external,F,A),(functor(P,F,A),forall(clause(P,B),portray_clause_0((P:-B))))),
  forall(was_pttp_functor(internal,F,A),(functor(P,F,A),forall(clause(P,B),portray_clause_0((P:-B))))).

:- thread_local(is_query_functor/1).
must_pttp_id(ID):-must(thlocal:current_why(ID,_)).
is_query_lit(Q):- functor(Q,F,_),atom_concat('quer',_,F).

get_int_query(Int_query):- is_query_functor(X),!, atom_concat('int_',X,Int_query).
get_int_query(int_query).

:-export(pttp_query/1).
pttp_query(X) :- must_pttp_id(ID),pttp_query_wid(ID,X).
:-export(pttp_query_wid/2).
pttp_query_wid(ID, Y):- trace,pttp_tell_wid(ID,(query:-Y)), pttp_test_prove(ID,query),!.

/*
 A thread local safe way to do it
 pttp_query_wid(ID, Y):- term_variables(Y,Vars),gensym(query_pttp,Q),Z=..[Q|Vars],
    atom_concat('int_',Q,Int_query),
    with_assertions(is_query_functor(Q), 
           (pttp_assert_int_wid(ID,((Z:-Y))), pttp_test_prove(ID,Int_query))).
*/

% ===============================================


renumbervars_a(In,Out):-renumbervars(In,Out),!.

:-export(assertz_unumbered/1).

assertz_unumbered(B,_):-assertz_unumbered(B).

assertz_unumbered(B):-is_ftVar(B),trace_or_throw(var_assertz_unumbered(B)).
assertz_unumbered(true):-!.
assertz_unumbered(_:true):-!.
%assertz_unumbered((H:- (infer_by(_),B))):- !,assertz_unumbered((H:-B)).
%assertz_unumbered((H:- (infer_by(_)))):- !,assertz_unumbered((H)).
assertz_unumbered(:-(B)):- !, show_call(must(B)),!.
assertz_unumbered(B):- thlocal:current_pttp_db_oper(OP),!, must((unnumbervars(B,BB),show_call_failure(call(OP,BB)))).
assertz_unumbered(B):-must((unnumbervars(B,BB),show_call_failure(ainz(BB)))).

:-export(add_functor/2).
add_functor(Ext,F/A):- must(( export(F/A),ainz(was_pttp_functor(Ext,F,A)))).


pttp_tell(Wff):- why_to_id(pttp_tell,Wff,Why),pttp_assert_int_wid(Why,Wff).


% ===============================================================================
% pttp_assert_int
% ===============================================================================
:-export(pttp_assert_int/1).
pttp_assert_int(Y):- must_pttp_id(ID),pttp_assert_int_wid(ID,Y).
:-export(pttp_assert_int_wid/2).
:-meta_predicate(pttp_assert_int_wid(+,+)).


pttp_assert_int_wid(ID,Var):-is_ftVar(Var),trace_or_throw(var_pttp_assert_int_wid(ID,Var)).
pttp_assert_int_wid(_ID,true):-!.
pttp_assert_int_wid(ID,[H|B]):-!,pttp_assert_int_wid(ID,H),!,pttp_assert_int_wid(ID,B),!.
pttp_assert_int_wid(ID,(H,B)):-!,pttp_assert_int_wid(ID,H),!,pttp_assert_int_wid(ID,B),!.
pttp_assert_int_wid(ID,_:L):-!, pttp_assert_int_wid(ID,L).
pttp_assert_int_wid(ID,YB):- must((get_functor(YB,F,A),renumbervars_a(YB,Y),pttp_assert_int_wid04(ID,Y,F,A),assertz_unumbered(Y))).



pttp_assert_int_wid04(_,Y,_,_):- static_predicate(Y,Why),must( dmsg(error(warn(static_predicate(Y,Why))))),!.
pttp_assert_int_wid04(_,_,F,A):- was_pttp_functor(external,F,A),!.
pttp_assert_int_wid04(_,Y,F,A):- not(internal_functor(F)),add_functor(external,F/A),assertz_unumbered(Y),!.
pttp_assert_int_wid04(ID,Y,F,A):- show_call_success(user:wid(ID,F/A,Y)),!.
%pttp_assert_int_wid04(ID,Y,F,A):- fail, once((must((must((renumbervars_a(Y,BB),nonvar(BB))),pred_subst(is_wid_key,BB,_,_,YCheck),nonvar(YCheck),BB \=@= YCheck)))),user:wid(_,F/A,YCheck),!,ainz(user:wid(ID,F/A,Y)),!.
pttp_assert_int_wid04(ID,Y,F,A):- user:wid(_,_,Y),!,ain(user:wid(ID,F/A,Y)),!.
pttp_assert_int_wid04(ID,Y,F,A):- ainz(user:wid(ID,F/A,Y)),add_functor(internal,F/A),!,assertz_unumbered(Y),!.
/*
pttp_assert_int_wid04(ID,Y,F,A):- 
   static_predicate(Y,Why)-> (trace,wdmsg(warn(error(static_predicate(Y,Why))))); 
    must(show_call_failure(assertz_unumbered(Y)),
    must((not(internal_functor(F))-> add_functor(external,F/A); (ainz(user:wid(ID,F/A,Y)),add_functor(internal,F/A)))))
*/

:- user:ensure_loaded(dbase_i_mpred_pttp_statics).
:- user:ensure_loaded(dbase_i_mpred_pttp_precompiled).
:- user:ensure_loaded(dbase_i_mpred_pttp_testing).

:- if_startup_script(do_pttp_tests).




/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_preddefs.pl
:- module(logicmoo_util_preddefs,
          [ call_if_defined/1,
            convert_to_dynamic/1,
            convert_to_dynamic/3,
            current_predicate_module/2,
            def_meta_predicate/3,
            dynamic_if_missing/1,
            dynamic_multifile/1,
            (was_shared_multifile)/1,            
            (dynamic_safe)/1,
            (dynamic_safe)/3,
            dynamic_transparent/1,
            fill_args/2,
            get_module_of/2,
            get_module_of_4/4,
            get_pi/2,
            only_3rd/4,
            make_transparent/4,
            multi_transparent/1,
            must_pi/1,
            p_predicate_property/2,
            to_canonical_mpi/2,
            pred_prop/3,
            pred_prop/4,
            rebuild_as_dyn/4,
            rebuild_pred_into/3,
            rebuild_pred_into/4,
            remove_pred/3,
            static_predicate/1,
            static_predicate/3,
            save_was/4,
             (was_dynamic)/1,
             (was_multifile)/1,
             (was_module_transparent)/1,
             (was_export)/1,
             (was_shared_multifile)/1,
            with_mfa/2,            
            (make_shared_multifile)/3,
            with_pfa/2,
            with_pfa/4,
            m_m_fa_to_m_p_fa/4,
            m_fa_to_m_p_fa/2,
            with_pfa_single/4,
            with_pfa_group/4,
            with_mfa_of/5,
            with_pi/2,
            with_pi_selected/4,
            with_pi_stub/5
          ]).
:- meta_predicate
        call_if_defined(0),
        def_meta_predicate(0, +, +),
        ( (with_pfa_group(3,+,+,+))),
        ( (dynamic_safe(+)) ),
         with_pfa_single(3,?,?,?),
        ( dynamic_safe(+, +, +) ),
        get_module_of(0, -),
        get_module_of_4(0, +, +, -),
        must_pi(0),
        rebuild_pred_into(0, 1, ?),
        rebuild_pred_into(0, 0, 1, ?),
        static_predicate(+, +, +),
        with_mfa(0, 3),
        with_mfa_of(3, +, +, +, +),
        with_pi(0, 4),
        with_pi_selected(+, +, +, +),
        with_pi_stub(+, +, +, +, 0),
        context_module_of_file(-),
         with_pfa(1,*,+,+),
         with_pfa(1,+),
         only_3rd(1,*,*,*).

:- module_transparent
        convert_to_dynamic/1,
        convert_to_dynamic/3,
        current_predicate_module/2,
        dynamic_if_missing/1,
        dynamic_multifile/1,
        dynamic_transparent/1,
        only_3rd/4,
        fill_args/2,
        get_pi/2,
        make_transparent/4,
        multi_transparent/1,
        p_predicate_property/2,
        pred_prop/3,
        pred_prop/4,
        rebuild_as_dyn/4,
        remove_pred/3,
        static_predicate/1.


:- include('logicmoo_util_header.pi').


% ----------
:- export(with_pi/2).
:- module_transparent(with_pi/2).
% = :- meta_predicate(with_pi(0,4)).
with_pi(_:[]):-!.
with_pi(M:(P1,P2),Pred3):-!,'@'(( with_pi(M:P1,Pred3),with_pi(M:P2,Pred3) ),M).
with_pi(M:P,Pred3):-source_context_module(CM),!,with_pi_selected(CM,M,P,Pred3),!.
with_pi([],_):-!.
with_pi((P1,P2),Pred3):-!, with_pi(P1,Pred3),with_pi(P2,Pred3).
with_pi(P,Pred3):-source_context_module(CM),with_pi_selected(CM,CM,P,Pred3),!.

:- export(with_pi_selected/4).
% = :- meta_predicate(with_pi_selected(+,+,+,+)).
with_pi_selected(CM, M,[P|L],Pred3):-!,with_pi_selected(CM,M,P,  Pred3),with_pi_selected(CM,M,L,Pred3).
with_pi_selected(CM,M,(P,L),Pred3):-!,with_pi_selected(CM,M,P,  Pred3),with_pi_selected(CM,M,L,Pred3).
with_pi_selected(_CM,_M,[]  ,_Pred3):-!.
with_pi_selected(CM,M,[P]  ,Pred3):-!,with_pi_selected(CM,M,P,  Pred3).
with_pi_selected(CM,_, M:F//A,Pred3):-Ap2 is A+2, !,with_pi_selected(CM,M,F/Ap2,Pred3).
with_pi_selected(CM,M, F//A ,Pred3):-Ap2 is A+2,!,functor_safe(P,F,A),  with_pi_stub(CM,M,P,F/Ap2,Pred3).
with_pi_selected(CM,_, M:F/A,Pred3):-!,with_pi_selected(CM,M,F/A,Pred3).
with_pi_selected(CM,_, M:P ,Pred3):-!,with_pi_selected(CM,M,P,  Pred3).
with_pi_selected(CM,M, F/A ,Pred3):-!,functor_safe(P,F,A),  with_pi_stub(CM,M,P,F/A,Pred3).
with_pi_selected(CM,M, P ,Pred3):-  functor_safe(P,F,A), with_pi_stub(CM,M,P,F/A,Pred3).



% ----------

% = :- meta_predicate(must_pi(0)).
must_pi(X):-X,!.
must_pi(X):-trace,X,!.

:- export(with_pi_stub/5).
% = :- meta_predicate(with_pi_stub(+,+,+,+,0)).
with_pi_stub(CM, M,P, F//A , PM:Pred3):- ((integer(A),atom(M),atom(F),Ap2 is A+2, functor_safe(P,F,Ap2))),must_pi(PM:call(Pred3,CM, M,P,F/Ap2)),!.
with_pi_stub(CM, M,P, F/A , PM:Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),
  must_pi(PM:call(Pred3,CM, M,P,F/A)),!.
with_pi_stub(CM, M,P, F//A , Pred3):- ((integer(A),atom(M),atom(F),Ap2 is A+2,functor_safe(P,F,Ap2))),must_pi(call(Pred3,CM, M,P,F/Ap2)),!.
with_pi_stub(CM, M,P, F/A , Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),
   must_pi(call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A ,_: Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),  must_pi(call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A ,CP: Pred3):-!, must_pi(CP:call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A , Pred3):-!, must_pi(call(Pred3,CM, M,P,F/A)),!.

with_pi_stub(CM, M,P,FA,Pred3):- trace_or_throw(invalide_args(CM, M,P,FA,Pred3)).
% ----------

:- export(with_mfa/2).
:- module_transparent(with_mfa/2).
% = :- meta_predicate(with_mfa(0,3)).
with_mfa(P  ,Pred3):- with_pi(P,with_mfa_of(Pred3)).

:- module_transparent(with_mfa_of/5).
% = :- meta_predicate(with_mfa_of(3,+,+,+,+)).
with_mfa_of(Pred3,_CM,M,_P,F//A):- Ap2 is A+2, M:call(Pred3,M,F,Ap2).
with_mfa_of(Pred3,_CM,M,_P,F/A):-M:call(Pred3,M,F,A).

% ----------


:- module_transparent(make_transparent/4).
:- export(make_transparent/4).

make_transparent(_CM,M,_PI,F/0):-!, compound_name_arity(C,F,0), M:meta_predicate(C).
make_transparent(CM,_,PI,M:F/A):-!,make_transparent(CM,M,PI,F/A).
make_transparent(_CM,M,PI,F/A):-
   motrace(((var(PI)->functor_safe(PI,F,A);true),
   M:module_transparent(F/A),
   fill_args(PI,('?')),!,
   dbgsubst(PI, (^),(^),PI1),
   dbgsubst(PI1,(0),(0),PI2),
   dbgsubst(PI2,(:),(:),PI3),
   (compound(PI3) -> M:meta_predicate(PI3) ; true))).

% ----------


:-module_transparent(context_module_of_file/1).
context_module_of_file(CM):- prolog_load_context(source,F), make_module_name(F,CM),current_module(CM),!.
context_module_of_file(CM):- '$set_source_module'(CM,CM),!.
context_module_of_file(CM):- source_context_module(CM),!.

:- op(1150,fx,lmconf:dynamic_safe).

:- export((was_shared_multifile)/1).
:- module_transparent((was_shared_multifile)/1).
:- meta_predicate((was_shared_multifile(+))).


was_shared_multifile(PI):- context_module_of_file(CM),with_pfa_group(save_was(shared_multifile),CM, kb, PI).
was_dynamic(PI):- context_module_of_file(CM),with_pfa_group(save_was(dynamic),CM, kb, PI).
was_export(PI):- context_module_of_file(CM),with_pfa_group(save_was(export),CM, kb, PI).
was_module_transparent(PI):- context_module_of_file(CM),with_pfa_group(save_was(module_transparent),CM, kb, PI).
was_multifile(PI):- context_module_of_file(CM),with_pfa_group(save_was(multifile),CM, kb, PI).

:-dynamic(was_was:was_was_once/4).
:-export(was_was:was_was_once/4).
:-multifile(was_was:was_was_once/4).
:-dynamic(was_was:skip/2).
:-export(was_was:skip/2).
:-multifile(was_was:skip/2).

save_was(_,_,_,_).
save_was(_,_, M, F/A):- was_was:skip(F/A,M),!.
save_was(export,_, M, F/A):- !,retractall(was_was:was_was_once(F/A,M,_,_)),!,assert_if_new(was_was:skip(F/A,M)),!.
save_was(module_transparent,_, _, _):- !.
save_was(_,CM, M, F/A):-  on_x_cont(M:dynamic(F/A)), on_x_cont(CM:dynamic(F/A)), on_x_cont(M:multifile(F/A)), on_x_cont(CM:multifile(F/A)),fail.
save_was(Was,CM, M, F/A):- !, once(source_location(File,_);File=CM),assert_if_new(was_was:was_was_once(F/A,M,File,Was)),!.
save_was(Was,CM, M, P):-functor(P,F,A), save_was(Was,CM, M, F/A).

:-module_transparent(make_shared_multifile/3).
:- export((make_shared_multifile)/3).
make_shared_multifile(CM, M, F/A):- 
 must_det_l((    
    dynamic_safe(M,F,A), 
   '@'(M:export(M:F/A),M),
   '@'(M:multifile(M:F/A),M),
   '@'(M:multifile(M:F/A),CM),   
    (CM\==M->CM:import(M:F/A);true))).
make_shared_multifile(CM, M, PI):- functor(PI,F,A),make_shared_multifile(CM, M, F/A).

:-module_transparent(with_pfa/2).
with_pfa(With, PI):- context_module_of_file(CM),with_pfa_group(only_3rd(With),CM, user, PI).

:-module_transparent(with_pfa/4).
with_pfa(With,CM, M, PI):- context_module_of_file(CM),with_pfa_group(only_3rd(With),CM, M, PI).

:-module_transparent(m_m_fa_to_m_p_fa/4).
m_m_fa_to_m_p_fa(Decl_mpred_hybrid,CM,M,F/A):-!,atom(F),functor(PI,F,A),CM:call(Decl_mpred_hybrid,M,PI,F/A).
m_m_fa_to_m_p_fa(Decl_mpred_hybrid,CM,M,PI):-functor(PI,F,A),CM:call(Decl_mpred_hybrid,M,PI,F/A).

:-module_transparent(m_fa_to_m_p_fa/2).
m_fa_to_m_p_fa(Decl_mpred_hybrid,M:FA):- !, m_m_fa_to_m_p_fa(Decl_mpred_hybrid,M,M,FA).
m_fa_to_m_p_fa(Decl_mpred_hybrid,FA):-  m_m_fa_to_m_p_fa(Decl_mpred_hybrid,M,M,FA).

 
:-module_transparent(only_3rd/4).
only_3rd([],_CM, _M, _PI):- !.
only_3rd([With|List],CM, M, PI):- is_list(List),!,only_3rd(With,CM, M, PI),only_3rd(List,CM, M, PI).
only_3rd(With,user, user, PI):-!, dcall(with_pi,call(With,PI)).
only_3rd(With,CM, user, PI):-!, dcall(with_pi,call(With,CM:PI)).
% only_3rd(With,user, M, PI):-!, dcall(with_pi,call(With,M:PI)).
only_3rd(With,CM, M, PI):- CM:call(With,M:PI).

:- multifile(lmconf:mpred_is_decl_called/4).
:- dynamic(lmconf:mpred_is_decl_called/4).

:- meta_predicate(with_pfa_group(3,+,+,+)).
:- module_transparent(with_pfa_group/4).
:- export((with_pfa_group)/4).

to_canonical_mpi(M:FA,MPI):-atom(M),!,to_canonical_mpi(FA,PI),add_mi(M,PI,MPI).
to_canonical_mpi((M:F)/A,MPI):- integer(A),!,functor(PI,F,A),add_mi(M,PI,MPI).
to_canonical_mpi((M:F)//A2,MPI):-integer(A),!,A is A2 + 2, functor(PI,F,A),add_mi(M,PI,MPI).
to_canonical_mpi(F/A,MPI):- functor(P,F,A), functor(P,F,A),strip_module(P,M,PI),add_mi(M,PI,MPI).
to_canonical_mpi(F//A2,MPI):- A is A2 + 2, functor(P,F,A),strip_module(P,M,PI),add_mi(M,PI,MPI).
to_canonical_mpi(P,MPI):- strip_module(P,M,PI),add_mi(M,PI,MPI).

add_mi(M,P,M:PI):-strip_module(P,_,PI).

with_pfa_group(With,CM, _, M:F/A ):- must(atom(F)), !,with_pfa_group(With,CM, M,F/A ).
with_pfa_group(With,CM, _, (M:F)/A ):- must(atom(F)), !,with_pfa_group(With,CM, M,F/A ).
with_pfa_group(With,CM, _, M:PI ):- must(nonvar(PI)),!, with_pfa_group(With,CM,M,PI).
with_pfa_group(With,CM, M, [A] ):-!,with_pfa_group(With,CM,M, A ).
with_pfa_group(With,CM, M, [A|B] ):-!,with_pfa_group(With,CM,M, A ),with_pfa_group(With,CM,M, B ).
with_pfa_group(With,CM, M, (A,B) ):-!,with_pfa_group(With,CM,M, A ),with_pfa_group(With,CM,M, B ).
with_pfa_group(With,CM, M, ([F1|FL])/A):- !,with_pfa_single(With,CM, M, F1/A),with_pfa_group(With,CM, M, FL/A).
with_pfa_group(With,CM, M, (F1,FL)/A):- !,with_pfa_single(With,CM, M, F1/A),with_pfa_group(With,CM, M, FL/A).

with_pfa_group(With,CM, M, F):- atom(F),!,must(with_pfa_single(With,CM, M, F/0)).
with_pfa_group(With,CM, M, F/A):- !,must(with_pfa_single(With,CM, M, F/A)).
with_pfa_group(With,CM, M, PI):- must(with_pfa_single(With,CM, M, PI)).

:-export(with_pfa_single/4).
:-module_transparent(with_pfa_single/4).
with_pfa_single(With,CM, M, FA):- lmconf:mpred_is_decl_called(With,CM, M, FA),!.
% with_pfa_single(With,_CM, M, FA):- to_canonical_mpi(FA,P), \+ \+ current_predicate(_,_:P), ignore(once((must((current_predicate(_,RM:P),\+ predicate_property(RM:P,imported_form(_)), M==RM))))),fail.
with_pfa_single([], _CM, _M, _FA):-!.
with_pfa_single([With|List],CM, M, FA):- is_list(List),!,with_pfa_single(With,CM, M, FA),!,with_pfa_single(List,CM, M, FA).
with_pfa_single(With,CM, M, FA):- lmconf:mpred_is_decl_called(With,CM0, M0, FA),M0\==M, dmsg(with_pfa_single(With,CM->CM0, M->M0, FA)),!,asserta(lmconf:mpred_is_decl_called(With,CM, M, FA)),!.
with_pfa_single(With,CM, M, FA):- asserta(lmconf:mpred_is_decl_called(With,CM, M, FA)), must(call(With,CM, M, FA)).


% ----------


:- export(fill_args/2).
fill_args([Arg|More],With):-!,ignore(With=Arg),fill_args(More,With).
fill_args([],_).
fill_args(PI,With):-compound_name_arguments(PI,_,ARGS),fill_args(ARGS,With).

% = :- meta_predicate(meta_predicate(0)).


:- export(def_meta_predicate/3).
% = :- meta_predicate((def_meta_predicate(0,+,+))).

def_meta_predicate(M:F,S,E):-!,M:doall(((between(S,E,N),make_list('?',N,List),compound_name_arguments(CALL,F,List),'@'(meta_predicate(CALL),M)))).
def_meta_predicate(F,S,E):- trace_or_throw(def_meta_predicate(F,S,E)).



:- export(remove_pred/3).
remove_pred(_,_,_):-!.
remove_pred(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred(M,F,A):- functor(P,F,A),
  (current_predicate(M:F/A) -> ignore((catchvv(redefine_system_predicate(M:P),_,true),abolish(M:F,A)));true),
  M:asserta((P:- wdmsg(error(P)),throw(permission_error(M:F/A)))).



% = :- meta_predicate(call_if_defined(0)).
:- export(call_if_defined/1).
call_if_defined(G):-current_predicate(_,G),G.


:- module_transparent(p_predicate_property/2).
p_predicate_property(P,PP):-predicate_property(P,PP),!.
p_predicate_property(_:P,PP):-predicate_property(P,PP).
%current_bugger_predicate(M:FF/FA):-nonvar(FF),!,current_predicate(M:FF,FA).
%current_bugger_predicate(FF/FA):-nonvar(FF),!,!,current_predicate(FF/FA).
:- module_transparent(current_predicate_module/2).
current_predicate_module(P,M):-var(P),!,current_predicate(F/A),functor_safe(P,F,A),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(OM:F/A,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(OM:P,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(F/A,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(P,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).


dynamic_multifile(Pred/N):-
   dynamic(Pred/N),
   multifile(Pred/N),
   module_transparent(Pred/N).

dynamic_transparent([]):-!.
dynamic_transparent([X]):-dynamic_transparent(X),!.
dynamic_transparent([X|Xs]):-!,dynamic_transparent(X),dynamic_transparent(Xs),!.
dynamic_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A).
dynamic_transparent(F/A):-!,multi_transparent(lmconf:F/A).
dynamic_transparent(X):-functor_catch(X,F,A),dynamic_transparent(F/A),!.

multi_transparent([]):-!.
multi_transparent([X]):-multi_transparent(X),!.
multi_transparent([X|Xs]):-!,multi_transparent(X),multi_transparent(Xs),!.
multi_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A),multifile(M:F/A).
multi_transparent(F/A):-!,multi_transparent(lmconf:F/A).
multi_transparent(X):-functor_catch(X,F,A),multi_transparent(F/A),!.



dynamic_if_missing(F/A):-functor_safe(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):-dynamic([F/A]).

get_pi(PI,PI):-var(PI),!.
get_pi(F/A,PI):-!,functor(PI,F,A).
get_pi(PI,PI):- atomic(PI),!.
get_pi(PI,PI):- compound(PI),!.
get_pi(Mask,PI):-get_functor(Mask,F,A),functor(PI,F,A),!.


:- meta_predicate get_module_of_4(0,+,+,-).
get_module_of_4(_P,F,A,ModuleName):- current_module(ModuleName),module_property(ModuleName, exports(List)),member(F/A,List),!.
get_module_of_4(_P,F,A,M):- current_predicate(M0:F0/A0),F0=F,A0=A,!,M=M0.
get_module_of_4(P,F,A,M):- trace_or_throw((get_module_of_4(P,F,A,M))).

/*
get_module_of_4(_P,F,A,M):- current_predicate(F0/A0),F0=F,A0=A,!,lmconf:mpred_user_kb(M).
get_module_of_4(_P,F,A,_M):-trace, isCycPredArity(F,A),!,fail.
get_module_of_4(P,F,A,M):- trace, debugCall(get_module_of_4(P,F,A,M)).
*/

:- meta_predicate get_module_of(0,-).
get_module_of(V,M):-var(V),!,current_module(M).
get_module_of(F/A,M):-!,functor_catch(P,F,A),!,get_module_of(P,M).
get_module_of(P,M):-predicate_property(P,imported_from(M)),!.
get_module_of(P,M):-predicate_property(_:P,imported_from(M)),!.
get_module_of(MM:_,M):-!,MM=M.
get_module_of(P,M):-functor_catch(P,F,A),get_module_of_4(P,F,A,M).



% ----------
:- export(static_predicate/3).
% = :- meta_predicate(static_predicate(+,+,+)).
static_predicate(M,F,A):- functor_safe(FA,F,A),  once(M:predicate_property(FA,_)),not(M:predicate_property(FA,dynamic)),not((M:predicate_property(FA,imported_from(Where)),Where \== M)).

static_predicate(A):-atom(F),!,current_predicate(F/A),!,functor(FA,F,A),static_predicate(FA).
static_predicate(F/A):-!,atom(F),current_predicate(F/A),!,functor(FA,F,A),static_predicate(FA).
% static_predicate(FA):-predicate_property(FA,built_in),!.
static_predicate(FA):-predicate_property(FA,static),!.
static_predicate(FA):-once(predicate_property(FA,_)),not(predicate_property(FA,dynamic)).



:- export((((dynamic_safe)/1))).
% = :- meta_predicate(dynamic_safe(+)).
:- module_transparent((((dynamic_safe)/1))).
dynamic_safe(MFA):- with_mfa(MFA,dynamic_safe).

:- export((((dynamic_safe)/3))).
% = :- meta_predicate(dynamic_safe(+,+,+)).
:- module_transparent((((dynamic_safe)/3))).

convert_to_dynamic(M:FA):- !, get_functor(FA,F,A),convert_to_dynamic(M,F,A).
convert_to_dynamic(FA):- get_functor(FA,F,A), convert_to_dynamic(user,F,A).

convert_to_dynamic(M,F,A):-  functor(C,F,A), M:predicate_property(C,dynamic),!.
convert_to_dynamic(M,F,A):-  M:functor(C,F,A),\+ predicate_property(C,_),!,M:((dynamic(F/A),multifile(F/A),export(F/A))),!.
convert_to_dynamic(M,F,A):-  functor(C,F,A),findall((C:-B),clause(C,B),List),rebuild_as_dyn(M,C,F,A),maplist(assertz,List),!.

rebuild_as_dyn(M,C,_,_):- predicate_property(M:C,dynamic),!.
rebuild_as_dyn(M,C,F,A):- redefine_system_predicate(M:C),M:abolish(F,A),dynamic(M:F/A),multifile(M:F/A),export(F/A),!.

dynamic_safe(M,F,A):- functor(C,F,A),predicate_property(C,imported_from(system)),!,dmsg(warn(predicate_property(M:C,imported_from(system)))).
dynamic_safe(M,F,A):- (static_predicate(M,F,A) -> dcall(why,convert_to_dynamic(M,F,A)) ; on_x_log_cont((dynamic(M:F/A),multifile(M:F/A)))). % , warn_module_dupes(M,F,A).
:- op(1150,fx,lmconf:dynamic_safe).


% pred_prop(Spec,DO,TEST,DONT)
pred_prop((M:F/A),DO,TEST,true):-pred_prop(M:F/A,DO,TEST).
pred_prop(M:F/A,(lock_predicate(M:F/A)),(built_in),unlock_predicate(M:F/A)).
pred_prop(M:F/A, (dynamic(M:F/A)) ,(dynamic), dcall(why,compile_predicates([F/A]))).

pred_prop(_,(meta_predicate Spec),(meta_predicate Spec)).
pred_prop(M:F/A,multifile(M:F/A)	       ,(multifile)).
pred_prop(M:F/A,module_transparent(M:F/A) ,(transparent)).
pred_prop(M:F/A,discontiguous(M:F/A) ,(discontiguous)).
pred_prop(M:F/A,volatile(M:F/A)	  ,(volatile)).
pred_prop(M:F/A,public(M:F/A)     ,(public)).
pred_prop(M:F/A,thread_local(M:F/A),(thread_local)).
pred_prop(M:F/A,noprofile(M:F/A)	    , (noprofile)).
pred_prop(M:F/A,'$iso'(M:F/A) ,(iso)).


:- thread_local(tlbugger:rbuild_pred_impl_cache_pp/2).
:- thread_local(tlbugger:rbuild_pred_impl_cache/2).

%  rebuild_pred_into(OMC,NMC,AssertZ,[+dynamic,-built_in,+volatile, etc]).

% = :- meta_predicate(rebuild_pred_into(0,1,?)).
rebuild_pred_into(C,AssertZ,OtherTraits):-rebuild_pred_into(C,C,AssertZ,OtherTraits).

% = :- meta_predicate(rebuild_pred_into(0,0,1,?)).
rebuild_pred_into(_,NMC,AssertZ,_):-tlbugger:rbuild_pred_impl_cache(NMC,AssertZ),!.
rebuild_pred_into(OMC,NMC,AssertZ,OtherTraits):-
  listing(OMC),
  asserta(tlbugger:rbuild_pred_impl_cache(NMC,AssertZ)),
  dcall(rebuild_pred_into,(predicate_property(OMC,number_of_clauses(_)))),
  strip_module(OMC, OM, OC),
  strip_module(NMC, NM, NC),
   must_det_l((
      '$set_source_module'(Before, OM),
      functor(NC,NF,A), functor(OC,OF,A),
      (dcall(why,predicate_property(OMC,number_of_clauses(_)))),
      must_pi(dcall_failure(why,predicate_property(OMC,number_of_clauses(_)))),
      forall(predicate_property(OC,PP),asserta(tlbugger:rbuild_pred_impl_cache_pp(NC,PP))),
      findall((OC:-B),((clause(OC,B),assertz(pp_clauses((OC:-B))))),List),
      '$set_source_module'(_, NM),
      forall(member(-PP,OtherTraits),retractall(tlbugger:rbuild_pred_impl_cache_pp(NC,PP))),
      forall(member(+PP,OtherTraits),asserta(tlbugger:rbuild_pred_impl_cache_pp(NC,PP))),
      once(tlbugger:rbuild_pred_impl_cache_pp(NC,(built_in))->(redefine_system_predicate(NF/A),unlock_predicate(NF/A));true),
      dcall(why,must_pi(abolish(NF/A))),
      dcall(why,must_pi(abolish(NF/A))),
      garbage_collect_clauses,
      ignore(convert_to_dynamic(NM,NF,A)),
      garbage_collect_clauses,
      %must_pi( \+ predicate_property(NMC,_)),
      %once(memberchk(CC,List)->true;(CC=((NC:-fail,1234)))),
      %convert_to_dynamic(NM,NF,A),
      %ignore(on_x_log_throw(tlbugger:rbuild_pred_impl_cache_pp(NC,(dynamic))->dynamic(NF/A);true)),
      %ignore(once(tlbugger:rbuild_pred_impl_cache_pp(NC,(multifile))->multifile(NF/A);true)),
      must_pi(((tlbugger:rbuild_pred_impl_cache_pp(NC,file(File)),tlbugger:rbuild_pred_impl_cache_pp(NC,line_count(_Line))))
        ->
            must_pi(('$compile_aux_clauses'(CC, File),retractall(CC)));
            must_pi(dmsg(noFileFor(NC)))),
      forall(pred_prop(NM:NF/A,TODO,PP,ELSE),(tlbugger:rbuild_pred_impl_cache_pp(NC,PP)->must_pi(TODO);must_pi(ELSE))),
      (tlbugger:rbuild_pred_impl_cache_pp(NC,meta_predicate(NC))->meta_predicate(NC);true),
      dbgsubst(List,OF,NF,ListO),maplist(AssertZ,ListO),!,

      retractall(tlbugger:rbuild_pred_impl_cache(NMC,_)),
      asserta(tlbugger:rbuild_pred_impl_cache(NMC,AssertZ)),
      '$set_source_module'(_, Before),
      listing(NMC),
      retractall(tlbugger:rbuild_pred_impl_cache_pp(NC,_))
      )).

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

logicmoo_util_preddefs.



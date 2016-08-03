/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_bugger.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_with_assertions.pl
:- module(logicmoo_util_with_assertions,
          [ check_thread_local_1m/1,
            to_thread_head_1m/4,
            w_tl/2,
            w_tl_e/2,
            wno_tl/2,
            wno_tl_e/2,
            key_asserta/1,
            key_erase/0,
            wtg/2,
            with_no_x/1,
            with_each_item/3]).

:- meta_predicate
        w_tl(:, :),
        wno_tl(0, 0),        
        w_tl_e(:, :),
        wno_tl_e(0, 0),        
        with_no_x(0),
        wtg(:, 0).
:- module_transparent
        check_thread_local_1m/1,
        to_thread_head_1m/4.

:- include('logicmoo_util_header.pi').

:- use_module(library(gui_tracer)).
:- meta_predicate with_each_item(:,+,+).
%% with_each_item(+P2,+EleList,+ArgList) is nondet.
%
% Call apply(P,[Ele|ArgList]) on each Ele(ment) in the EleList.
%
% EleList is a  List, a Conjuction Terms or a single element.
%
with_each_item(P,HV,S):- var(HV),!, apply(P,[HV|S]).
with_each_item(P,M:HT,S) :- !, must_be(atom,M), M:with_each_item(P,HT,S).
with_each_item(P,[H|T],S) :- !, apply(P,[H|S]), with_each_item(P,T,S).
with_each_item(P,(H,T),S) :- !, with_each_item(P,H,S), with_each_item(P,T,S).
with_each_item(P,H,S) :- apply(P,[H|S]).


%= 	 	 

%% with_no_x( :GoalG) is nondet.
%
% Using No X.
%
with_no_x(G):- getenv('DISPLAY',DISP),!,call_cleanup((unsetenv('DISPLAY'),with_no_x(G)),setenv('DISPLAY',DISP)).
with_no_x(G):- current_prolog_flag(gui,true),!,call_cleanup((set_prolog_flag(gui,false),with_no_x(G)),set_prolog_flag(gui,true)).
with_no_x(G):- current_prolog_flag(gui_tracer,true),!,call_cleanup((noguitracer,with_no_x(G)),guitracer).
with_no_x(G):- w_tl(tlbugger:show_must_go_on,call(G)).


%% wtg( ?M, :GoalCall) is nondet.
%
% maybe this one wont use thread local checking
%
:- meta_predicate(wtg(:,0)).
wtg(M:With,Call):- w_tl(M:With,Call).

% = :- meta_predicate(w_tl(:,0)).

:- use_module(system:logicmoo_util_scce).

%= 	 	 

%% w_tl( ?CALL1, ?Call) is nondet.
%
% W Thread Local.
%
w_tl(_:[],Call):- !,Call.
w_tl(M:[With|MORE],Call):- !,w_tl(M:With,w_tl(M:MORE,Call)).
w_tl(M:(With,MORE),Call):- !,w_tl(M:With,w_tl(M:MORE,Call)).
w_tl(M:(With;MORE),Call):- !,w_tl(M:With,Call);w_tl(M:MORE,Call).
w_tl(-TL:With,Call):- !,wno_tl(TL:With,Call).
w_tl(+TL:With,Call):- !,w_tl(TL:With,Call).
w_tl(M:not(With),Call):- !,wno_tl(M:With,Call).
w_tl(M:(-With),Call):- !,wno_tl(M:With,Call).
w_tl(M:(+With),Call):- !,w_tl(M:With,Call).

w_tl(OPM:op(N,XFY,OP),MCall):-!,
     (current_op(PN,XFY,OPM:OP);PN=0),!,
     strip_module(MCall,M,Call),
     (PN==N -> Call ; setup_call_cleanup(op(N,XFY,OPM:OP),'@'(Call,M),op(PN,XFY,OPM:OP))).

w_tl(FPM:current_prolog_flag(N,XFY),MCall):- !,w_tl(FPM:set_prolog_flag(N,XFY),MCall).

w_tl(_FPM:set_prolog_flag(N,XFY),MCall):- !,
     (current_prolog_flag(N,WAS);WAS=unUSED),
     strip_module(MCall,M,Call),!,
     (XFY==WAS -> Call ; 
     (setup_call_cleanup(set_prolog_flag(N,XFY),'@'(Call,M),(WAS=unUSED->true;set_prolog_flag(N,WAS))))).

w_tl(M:before_after(Before,After),Call):-
     (M:Before -> setup_call_cleanup(true,Call,M:After);Call).

w_tl(_:ensure(WM:THeadWM),CM:Call):- !,
 hotrace(( 
     to_thread_head_1m(WM:THeadWM,M,_Head,HAssert) -> copy_term(HAssert,CHAssert) ; throw(failed(to_thread_head_1m(WM:THeadWM,M,_,HAssert))))),
     ((CM:notrace((HAssert\=(_:-_),M:CHAssert,!,HAssert=@=CHAssert))) -> ( CM:Call );
            setup_call_cleanup(asserta(M:HAssert,REF),CM:Call,erase(REF))).

w_tl(_:scc(WM:THeadWM),CM:Call):- !,
   hotrace(( 
       to_thread_head_1m(WM:THeadWM,M,_Head,HAssert) -> true ; throw(failed(to_thread_head_1m(WM:THeadWM,M,_,HAssert))))),
       setup_call_cleanup(asserta(M:HAssert,REF),CM:Call,erase(REF)).

w_tl(_:scce(WM:THeadWM),CM:Call):- !,w_tl_e(WM:THeadWM,CM:Call).

w_tl(WM:THeadWM,CM:Call):-w_tl_e(WM:THeadWM,CM:Call).

w_tl_e(WM:THeadWM,CM:Call):-
 clause_asserted(WM:THeadWM)-> CM:Call;
 
 (notrace(( 
     to_thread_head_1m(WM:THeadWM,M,_Head,HAssert) -> true ; throw(failed(to_thread_head_1m(WM:THeadWM,M,_,HAssert))))),
     scce_orig2(key_asserta(M:HAssert),CM:Call,key_erase)).


key_asserta(M:HAssert):- asserta(M:HAssert,REF),(nb_current('$w_tl_e',Was)->nb_setval('$w_tl_e',[REF|Was]);nb_setval('$w_tl_e',[REF])).
key_erase:- once((nb_current('$w_tl_e',[REF|Was]),nb_setval('$w_tl_e',Was),erase(REF))).

   	 

%% wno_tl( :GoalUHead, :GoalCall) is nondet.
%
% Wno Thread Local.
%
wno_tl(UHead,Call):- \+ UHead -> Call ; w_tl((UHead :- !,fail),Call).

wno_tl_e(UHead,Call):- \+ UHead -> Call ; w_tl_e((UHead :- !,fail),Call).



%= 	 	 

%% to_thread_head_1m( ?H, ?TL, ?HO, ?HH) is nondet.
%
% Converted To Thread Head 1m.
%
to_thread_head_1m(_: (TL:Head :- BODY),TL,TL:(TL:Head :- BODY),(TL:Head :- BODY)):- nonvar(TL),
   check_thread_local_1m(TL:Head).
to_thread_head_1m((H:-B),TL,HO,(HH:-B)):-to_thread_head_1m(H,TL,HO,HH).
to_thread_head_1m(baseKB:Head,baseKB,baseKB:Head,Head).

to_thread_head_1m(TL:Head,TL,TL:Head,Head):- check_thread_local_1m(TL:Head).
% to_thread_head_1m(Head,Module,Module:Head,Head):-Head \= (_:_), predicate_module(Head,Module).
to_thread_head_1m(baseKB:Head,user,baseKB:Head,Head).
to_thread_head_1m(Head,t_l,t_l:Head,Head):-check_thread_local_1m(t_l:Head).
to_thread_head_1m(Head,tlbugger,tlbugger:Head,Head):-check_thread_local_1m(tlbugger:Head).


%= 	 	 

%% check_thread_local_1m( ?TLHead) is nondet.
%
% Check Thread Local 1m.
%
check_thread_local_1m(_):- skipWrapper,!.
check_thread_local_1m(t_l:_):-!.
check_thread_local_1m((H:-_)):-!,check_thread_local_1m(H).
check_thread_local_1m(tlbugger:_):-!.
check_thread_local_1m(lmcache:_):-!.
check_thread_local_1m(lmcache:TLHead):- slow_sanity(predicate_property(lmcache:TLHead,(volatile))).
%check_thread_local_1m(baseKB:_):-!.
check_thread_local_1m(TLHead):- slow_sanity(predicate_property(TLHead,(thread_local))).


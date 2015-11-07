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
            wno_tl/2,
            wtg/2,
            with_no_x/1
          ]).

:- meta_predicate
        w_tl(:, :),
        wno_tl(0, 0),
        with_no_x(0),
        wtg(:, 0).
:- module_transparent
        check_thread_local_1m/1,
        to_thread_head_1m/4.

:- include('logicmoo_util_header.pi').


%= 	 	 

%% with_no_x( :GoalG) is semidet.
%
% Using No X.
%
with_no_x(G):- getenv('DISPLAY',DISP),!,call_cleanup((unsetenv('DISPLAY'),with_no_x(G)),setenv('DISPLAY',DISP)).
with_no_x(G):- current_prolog_flag(gui,true),!,call_cleanup((set_prolog_flag(gui,false),with_no_x(G)),set_prolog_flag(gui,true)).
with_no_x(G):- current_prolog_flag(gui_tracer,true),!,call_cleanup((noguitracer,with_no_x(G)),guitracer).
with_no_x(G):- call(G).

% maybe this one wont use thread local checking
% = :- meta_predicate(wtg(:,0)).

%= 	 	 

%% wtg( ?M, :GoalCall) is semidet.
%
% Wtg.
%
wtg(M:With,Call):- w_tl(M:With,Call).

% = :- meta_predicate(w_tl(:,0)).


%= 	 	 

%% w_tl( ?CALL1, ?Call) is semidet.
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

w_tl(WM:THeadWM,CM:Call):- 
 notrace(( 
     to_thread_head_1m(WM:THeadWM,M,_Head,HAssert) -> copy_term(HAssert,CHAssert) ; throw(failed(to_thread_head_1m(WM:THeadWM,M,_,HAssert))))),
     ((CM:notrace((HAssert\=(_:-_),M:CHAssert,!,HAssert=@=CHAssert))) -> ( CM:Call );
            setup_call_cleanup(asserta(M:HAssert,REF),CM:Call,erase(REF))).



%= 	 	 

%% wno_tl( :GoalUHead, :GoalCall) is semidet.
%
% Wno Thread Local.
%
wno_tl(UHead,Call):- w_tl((UHead :- !,fail),Call).

/*
wno_tl(UHead,Call):- 
  hotrace((THead = (UHead:- (!,fail)),
   (to_thread_head_1m(THead,M,Head,HAssert) -> true; throw(to_thread_head_1m(THead,M,Head,HAssert))))),
       setup_call_cleanup(hotrace(M:asserta(HAssert,REF)),Call,M:erase_safe(HAssert,REF)).
*/


%= 	 	 

%% to_thread_head_1m( ?H, ?TL, ?HO, ?HH) is semidet.
%
% Converted To Thread Head 1m.
%
to_thread_head_1m((H:-B),TL,HO,(HH:-B)):-!,to_thread_head_1m(H,TL,HO,HH),!.
to_thread_head_1m(lmconf:Head,lmconf,lmconf:Head,Head):- !.
to_thread_head_1m(TL:Head,TL,TL:Head,Head):-!, check_thread_local_1m(TL:Head).
% to_thread_head_1m(Head,Module,Module:Head,Head):-Head \= (_:_), predicate_module(Head,Module),!.
to_thread_head_1m(lmconf:Head,user,lmconf:Head,Head):- !.
to_thread_head_1m(Head,t_l,t_l:Head,Head):-!,check_thread_local_1m(t_l:Head).
to_thread_head_1m(Head,tlbugger,tlbugger:Head,Head):-check_thread_local_1m(tlbugger:Head).


%= 	 	 

%% check_thread_local_1m( ?TLHead) is semidet.
%
% Check Thread Local 1m.
%
check_thread_local_1m(_):-!.
check_thread_local_1m(t_l:_):-!.
check_thread_local_1m(tlbugger:_):-!.
%check_thread_local_1m(_):-!.
%check_thread_local_1m(lmconf:_):-!.
check_thread_local_1m(TLHead):- slow_sanity(( must_det(predicate_property(TLHead,(thread_local))))),!.


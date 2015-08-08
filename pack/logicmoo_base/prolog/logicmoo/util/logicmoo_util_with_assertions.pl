/** <module> Logicmoo Debug Tools
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

:- meta_predicate(with_assertions(?,0)).

with_assertions( [],Call):- !,Call.
with_assertions( [With|MORE],Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With,MORE),Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With;MORE),Call):- !,with_assertions(With,Call);with_assertions(MORE,Call).
with_assertions( -TL:With,Call):- !,with_no_assertions(TL:With,Call).
with_assertions( +TL:With,Call):- !,with_assertions(TL:With,Call).
with_assertions( not(With),Call):- !,with_no_assertions(With,Call).
with_assertions( -With,Call):- !,with_no_assertions(With,Call).
with_assertions( +With,Call):- !,with_assertions(With,Call).

with_assertions(op(N,XFY,OP),MCall):-!,
     (current_op(PN,XFY,OP);PN=0),!,
     strip_module(MCall,M,Call),
     (PN==N -> Call ; setup_call_cleanup(op(N,XFY,OP),'@'(Call,M),op(PN,XFY,OP))).

with_assertions(set_prolog_flag(N,XFY),MCall):- !,
     (current_prolog_flag(N,WAS);WAS=unUSED),
     strip_module(MCall,M,Call),!,
     (XFY==WAS -> Call ; 
     (setup_call_cleanup(set_prolog_flag(N,XFY),'@'(Call,M),(WAS=unUSED->true;set_prolog_flag(N,WAS))))).

with_assertions(before_after(Before,After),Call):-
     (Before -> setup_call_cleanup(Call,After);Call).

with_assertions(THead,Call):- 
 notrace(( 
     to_thread_head(THead,M,_Head,HAssert) -> copy_term(HAssert,CHAssert) ; throw(failed(to_thread_head(THead,M,_,HAssert))))),
     ((HAssert\=(_:-_),once(M:CHAssert),HAssert=@=CHAssert) -> ( Call );
     (setup_call_cleanup(hotrace(asserta(M:HAssert,REF)),Call,erase_safe(M:HAssert,REF)))).


with_no_assertions(UHead,Call):- !,with_assertions((UHead:-!,fail),Call).

:-meta_predicate(with_no_assertions(+,0)).
with_no_assertions(UHead,Call):- 
  hotrace((THead = (UHead:- (!,fail)),
   (to_thread_head(THead,M,Head,HAssert) -> true; throw(to_thread_head(THead,M,Head,HAssert))))),
       setup_call_cleanup(hotrace(M:asserta(HAssert,REF)),Call,M:erase_safe(HAssert,REF)).

/*
old version
:-meta_predicate(with_no_assertions(+,0)).
with_no_assertions(THead,Call):-
 must_det(to_thread_head((THead:- (!,fail)),M,Head,H)),
   copy_term(H,  WithA), !, setup_call_cleanup(M:asserta(WithA,REF),Call,must_det(M:retract(Head))).
*/

to_thread_head((H:-B),TL,HO,(HH:-B)):-!,to_thread_head(H,TL,HO,HH),!.
to_thread_head(thglobal:Head,thglobal,thglobal:Head,Head):- !.
to_thread_head(TL:Head,TL,TL:Head,Head):-!, check_thread_local(TL:Head).
% to_thread_head(Head,Module,Module:Head,Head):-Head \= (_:_), predicate_module(Head,Module),!.
to_thread_head(user:Head,user,user:Head,Head):- !.
to_thread_head(Head,thlocal,thlocal:Head,Head):-!,check_thread_local(thlocal:Head).
to_thread_head(Head,tlbugger,tlbugger:Head,Head):-check_thread_local(tlbugger:Head).

check_thread_local(thlocal:_):-!.
check_thread_local(tlbugger:_):-!.
check_thread_local(_):-!.
check_thread_local(user:_):-!.
check_thread_local(TL:Head):-slow_sanity(( predicate_property(TL:Head,(dynamic)),must_det(predicate_property(TL:Head,(thread_local))))).


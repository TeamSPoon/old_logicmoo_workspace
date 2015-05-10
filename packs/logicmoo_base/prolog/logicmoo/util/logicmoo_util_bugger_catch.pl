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
:-module(logicmoo_util_bugger_catch,[
      ]).

%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[X,X,X],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

:-export((   maplist_safe/2,
   maplist_safe/3)).

maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST), bugger:debugOnFailure(apply(Pred,[E]))),LISTO),!, ignore(LIST=LISTO),!.
% though this should been fine %  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), debugOnFailure(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.

maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),debugOnFailure(apply(Pred,[E,EE])))), LISTO),  ignore(LIST=LISTO),!.
% though this should been fine % maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), debugOnFailureEach(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, ignore(OUT=[AA|BB]).


:- export(bad_functor/1).
bad_functor(L) :- arg(_,v('|','.',[],':','/'),L).

:- export(warn_bad_functor/1).
warn_bad_functor(L):-ignore((hotrace(bad_functor(L)),!,trace,nop(ddmsg(bad_functor(L))))).

:- export(strip_f_module/2).
strip_f_module(_:P,FA):-nonvar(P),!,strip_f_module(P,F),!,F=FA.
strip_f_module(P,PA):-atom(P),!,P=PA.

strip_f_module(P,FA):- is_list(P),catch(text_to_string(P,S),_,fail),!,atom_string(F,S),!,F=FA.
strip_f_module(P,FA):- hotrace(string(P);atomic(P)), atom_string(F,P),!,F=FA.
strip_f_module(P,P).

% use ccatch/3 to replace catch/3 works around SWI specific issues arround using $abort/0 and block/3
% (catchv/3 allows you to have these exceptions bubble up past your catch block handlers)
:- meta_predicate((catchv(0, ?, 0))).
:- meta_predicate((ccatch(0, ?, 0))).
:- export((ccatch/3,catchv/3)).
bubbled_ex(block(_,_)).
bubbled_ex('$aborted').
bubbled_ex_check(E):- (\+ bubbled_ex(E)),!.
bubbled_ex_check(E):-throw(E).
ccatch(Goal,E,Recovery):- nonvar(E) -> catch(Goal,E,Recovery); % normal mode (the user knows what they want)
                         catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode
catchv(Goal,E,Recovery):- catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode


:- export(functor_catch/3).
functor_catch(P,F,A):- catch(functor(P,F,A),_,compound_name_arity(P,F,A)).
% functor_catch(F,F,0):-atomic(F),!.
% functor_catch(P,F,A):-ccatch(compound_name_arity(P,F,A),E,(trace,ddmsg(E:functor(P,F,A)),trace)).


:- export(functor_safe/3).
functor_safe(P,F,A):- catch(functor(P,F,A),_,compound_name_arity(P,F,A)).
% functor_safe(P,F,A):- catch(compound_name_arity(P,F,A),_,functor(P,F,A)).
/*
% functor_safe(P,F,A):-var(P),A==0,compound_name_arguments(P,F,[]),!.
functor_safe(P,F,A):-var(P),A==0,!,P=F,!.
functor_safe(P,F,A):-functor_safe0(P,F,A),!.
functor_safe0(M:P,M:F,A):-var(P),atom(M),functor_catch(P,F,A),!,warn_bad_functor(F).
functor_safe0(P,F,A):-var(P),strip_f_module(F,F0),functor_catch(P,F0,A),!,warn_bad_functor(F).
functor_safe0(P,F,A):-compound(P),!,functor_safe_compound(P,F,A),warn_bad_functor(F).
functor_safe0(P,F,0):- hotrace(string(P);atomic(P)), atom_string(F,P),warn_bad_functor(F).
functor_safe_compound((_,_),',',2).
functor_safe_compound([_|_],'.',2).
functor_safe_compound(_:P,F,A):- functor_catch(P,F,A),!.
functor_safe_compound(P,F,A):- functor_catch(P,F,A).
functor_safe_compound(P,F,A):- var(F),strip_f_module(P,P0),!,functor_catch(P0,F0,A),strip_f_module(F0,F),!.
functor_safe_compound(P,F,A):- strip_f_module(P,P0),strip_f_module(F,F0),!,functor_catch(P0,F0,A).
*/

% block(test, (repeat, !(test), fail))).
:- meta_predicate block(+, :, ?). 
block(Name, Goal, Var) :- Goal, keep(Name, Var).	% avoid last-call and GC 
keep(_, _).  
set_block_exit(Name, Value) :-  prolog_current_frame(Frame),  prolog_frame_attribute(Frame, parent_goal,  mcall:block(Name, _, Value)). 
block(Name, Goal) :-  block(Name, Goal, Var),  (   Var == !  ->  !  ;   true  ). 
!(Name) :- set_block_exit(Name, !). 

:- export((block/3, 
            set_block_exit/2, 
            block/2, 
            !/1 )).

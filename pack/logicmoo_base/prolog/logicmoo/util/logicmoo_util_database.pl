% ===================================================================
% File 'logicmoo_util_database.pl'
% Purpose: To load the logicmoo libraries as needed
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_library.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
:-swi_module(logicmoo_util_database,
        [   
         asserta_new/1,
         assertz_new/1,
         as_clause/3,
         asserta_if_new/1,
         assertz_if_new/1,
         assertz_if_new_clause/1,
         assertz_if_new_clause/2,
         assert_if_new/1,
         safe_univ/2,
         clause_asserted/2,
         clause_asserted/1]).

:-meta_predicate clause_safe(?, ?).
:-module_transparent clause_safe/2.
:- swi_export(clause_safe/2).


clause_safe(M:H,B):-!,predicate_property(M:H,number_of_clauses(_)),clause(H,B).
clause_safe(H,B):-predicate_property(_:H,number_of_clauses(_)),clause(H,B).


:- swi_export(call_with_attvars/2).
:- module_transparent(call_with_attvars/2).
/*
call_with_attvars(asserta,Term):-!,really_with_attvars(asserta,Term),!.
call_with_attvars(assertz,Term):-!,really_with_attvars(assertz,Term),!.
call_with_attvars(assert,Term):-!,really_with_attvars(assert,Term),!.
*/
call_with_attvars(OP,Term):-unnumbervars(Term,Unumbered),!,call(OP,Unumbered).

:- swi_export(really_with_attvars/2).
:- module_transparent(really_with_attvars/2).
really_with_attvars(OP,Term):-unnumbervars(Term,Unumbered),!,call(OP,Unumbered).
really_with_attvars(OP,Term):-call(OP,Term).
really_with_attvars(OP,Term):-
  copy_term(Term, Copy, Gs),
  (Gs==[] -> call(OP,Term);
    show_call((as_clause(Copy,H,B),conjoin(maplist(call,Gs),B,NB),trace,call(OP,(H:-NB))))).
  

% peekAttributes/2,pushAttributes/2,pushCateElement/2.
:- module_transparent((asserta_new/1,asserta_if_new/1,assertz_new/1,assertz_if_new/1,assert_if_new/1,assertz_if_new_clause/1,assertz_if_new_clause/2,clause_asserted/2,as_clause/2,clause_asserted/1,eraseall/2)).
:- meta_predicate asserta_new(?),asserta_if_new(?),assertz_new(?),assertz_if_new(?),assert_if_new(?),assertz_if_new_clause(?),assertz_if_new_clause(?,?).
:- meta_predicate clause_asserted(-,-),as_clause(-,-,-),clause_asserted(-),eraseall(-,-).

asserta_new(NEW):-ignore((retract(NEW),fail)),asserta(NEW).
assertz_new(NEW):-ignore((retract(NEW),fail)),assertz(NEW).
% asserta_new(_Ctx,NEW):-ignore((retract(NEW),fail)),asserta(NEW).
% writeqnl(_Ctx,NEW):- fmt('~q.~n',[NEW]),!.

eraseall(M:F,A):-!,forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,B,X),erase_safe(clause(M:C,B,X),X))).
eraseall(F,A):-forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,B,X),erase_safe(clause(M:C,B,X),X))).


assert_if_new(N):-clause_asserted(N)->true;really_with_attvars(asserta,N).

assertz_if_new(N):-clause_asserted(N)->true;really_with_attvars(assertz,N).

asserta_if_new(N):-clause_asserted(N)->true;really_with_attvars(asserta,N).

assertz_if_new_clause(C):- as_clause(C,H,B),assertz_if_new_clause(H,B).

assertz_if_new_clause(H,B):- clause_asserted(H,B)->true;assertz((H:-B)).

as_clause( M:((H :- B)),M:H,B):-!.
as_clause( ((H :- B)),H,B):-!.
as_clause( H,  H,  true).

clause_asserted(C):- as_clause(C,H,B),clause_asserted(H,B).
clause_asserted(H,B):-clause_asserted(H,B,_).
clause_asserted(H,B,Ref):- predicate_property(H,number_of_clauses(N))-> 
   N>0   -> (clause(H, B, Ref), clause(Head, Body, Ref),  (B =@= Body), (H =@= Head)),!.

retract_eq(HB):-as_clause(HB,H,B),show_call_failure(predicate_property(H,number_of_clauses(_))),clause_asserted(H,B,Ref),erase(Ref).

erase_safe(_,REF):-erase(REF).
/*
erase_safe(((M:A):-B),REF):-!,erase_safe(clause(M:A,B),REF).
erase_safe(clause(U:A,B),REF):-U=user,!, erase_safe(clause(A,B),REF).
%erase_safe(clause(A,U:B),REF):-U=user,!, erase_safe(clause(A,B),REF).
%erase_safe(clause(M:A,B),REF):-!, erase_safe_now(M,clause(A,B),REF).
erase_safe(clause(A,B),REF):-!, erase_safe_now(_,clause(A,B),REF).
erase_safe(M:(A:-B),REF):-!,erase_safe(clause(M:A,B),REF).
erase_safe((A:-B),REF):-!,erase_safe(clause(A,B),REF).
erase_safe(clause(A,B,_),REF):-!,erase_safe(clause(A,B),REF).
erase_safe(asserta(A,_),REF):-!,erase_safe(clause(A,true),REF).
erase_safe(M:A,REF):-M==user,!,erase_safe(A,REF).
erase_safe(A,REF):-!,erase_safe(clause(A,true),REF).


erase_safe_now(_,clause(M:A,B),REF):-!,erase_safe_now(M,clause(A,B),REF).
erase_safe_now(M,clause(A,B),REF):-!,
   ignore((show_call_success(\+ clause(M:A,B, REF)))),
   (((var(REF);
   show_call_success(\+ nth_clause(A, _Index, REF));   
   show_call_success(clause_property(REF,erased));
   show_call_success(\+ clause_property(REF,_))))
   -> wdmsg(warn(var_erase_safe(clause(A,B),REF))) ; 
       erase(REF)).
*/

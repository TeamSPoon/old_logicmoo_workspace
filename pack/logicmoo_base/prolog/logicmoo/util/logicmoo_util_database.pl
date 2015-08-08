

:-meta_predicate clause_safe(?, ?).
:-module_transparent clause_safe/2.
:- export(clause_safe/2).

:- export(clause_safe_m3/3).

clause_safe(M:H,B):-!,debugOnError(clause(M:H,B)).
clause_safe(H,B):-!,debugOnError(clause(H,B)).

clause_safe(M:H,B):-!,clause_safe_m3(M,H,B).
clause_safe(H,B):-!,clause_safe_m3(_,H,B).
%clause_safe(M,string(S),B):- trace_or_throw(clause_safe(M,string(S),B)).
clause_safe_m3(M,H,B):-  (nonvar(H)->true;(trace,current_predicate(M:F/A),functor(H,F,A))),predicate_property(M:H,number_of_clauses(_)),clause(H,B).

call_with_attvars(asserta,Term):-really_with_attvars(asserta,Term),!.
call_with_attvars(assertz,Term):-really_with_attvars(assertz,Term),!.
call_with_attvars(assert,Term):-really_with_attvars(assert,Term),!.
call_with_attvars(OP,Term):-call(OP,Term),!.


really_with_attvars(OP,Term):-
  copy_term(Term, Copy, Gs),
  (Gs==[] -> call(OP,Term);
    show_call((as_clause(Copy,H,B),conjoin(maplist(call,Gs),B,NB),trace,call(OP,(H:-NB))))).
  

   

% peekAttributes/2,pushAttributes/2,pushCateElement/2.
:- module_transparent((asserta_new/1,asserta_if_new/1,assertz_new/1,assertz_if_new/1,assert_if_new/1,assertz_if_new_clause/1,assertz_if_new_clause/2,clause_asserted/2,as_clause/2,clause_asserted/1,eraseall/2)).
:- meta_predicate asserta_new(?),asserta_if_new(?),assertz_new(?),assertz_if_new(?),assert_if_new(?),assertz_if_new_clause(?),assertz_if_new_clause(?,?).
:- meta_predicate clause_asserted(-,-),as_clause(-,-,-),clause_asserted(-),eraseall(-,-).

asserta_new(_Ctx,NEW):-ignore((retract(NEW),fail)),asserta(NEW).
writeqnl(_Ctx,NEW):- fmt('~q.~n',[NEW]),!.

eraseall(M:F,A):-!,forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,B,X),erase_safe(clause(M:C,B,X),X))).
eraseall(F,A):-forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,B,X),erase_safe(clause(M:C,B,X),X))).

asserta_new(NEW):-ignore((retract(NEW),fail)),asserta(NEW).
assertz_new(NEW):-ignore((retract(NEW),fail)),assertz(NEW).

assert_if_new(N):-clause_asserted(N)->true;really_with_attvars(asserta,N).

assertz_if_new(N):-clause_asserted(N)->true;really_with_attvars(assertz,N).

asserta_if_new(N):-clause_asserted(N)->true;really_with_attvars(asserta,N).

assertz_if_new_clause(C):- as_clause(C,H,B),assertz_if_new_clause(H,B).

assertz_if_new_clause(H,B):-clause_asserted(H,B),!.
assertz_if_new_clause(H,B):-assertz((H:-B)).

as_clause( M:((H :- B)),M:H,B):-!.
as_clause( ((H :- B)),H,B):-!.
as_clause( H,  H,  true).

clause_asserted(C):- as_clause(C,H,B),!,clause_asserted(H,B).
% clause_asserted(H,B):- predicate_property(H,number_of_clauses(N)),N>0, \+ \+ ((numbervars(H:B),clause(H,B))).
clause_asserted(_:H,B):-!,clause_asserted(H,B).
clause_asserted(H,B):- predicate_property(H,number_of_clauses(N)),N>0,
  clause_asserted_0(H,B).

clause_asserted_0(H,B):-
  copy_term(H:B,HH:BB),!, clause(HH, BB, Ref),(clause(Head, Body, Ref)),=@=(B,Body), =@=(H,Head),!.

same_body(B,Body):-same_heads(B,Body).

same_heads(H,Head):-H=@=Head,!.
same_heads(M1:H,M2:Head):-H=@=Head,!,call(dmsg(warn(same_heads(M1:H,M2:Head)))).
same_heads(H,M2:Head):-H=@=Head,!,nop(dmsg(warn(same_heads(_M1:H,M2:Head)))).
same_heads(M1:H,Head):-H=@=Head,!,nop(dmsg(warn(same_heads(M1:H,_M2:Head)))).



% functor_safe(P,F,A):- catchvv(compound_name_arity(P,F,A),_,functor(P,F,A)).

erase_safe_now(_,clause(M:A,B),REF):-!,erase_safe_now(M,clause(A,B),REF).
erase_safe_now(M,clause(A,B),REF):-!,
   ignore((show_call_success(\+ clause(M:A,B, REF)))),
   (((var(REF);
   show_call_success(\+ nth_clause(A, _Index, REF));   
   show_call_success(clause_property(REF,erased));
   show_call_success(\+ clause_property(REF,_))))
   -> wdmsg(warn(var_erase_safe(clause(A,B),REF))) ; 
       erase(REF)).

erase_safe(_,REF):-!,erase(REF),!.
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



% ===================================================================
% File 'logicmoo_util_library.pl'
% Purpose: To load the logicmoo libraries as needed
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_library.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
:-module(logicmoo_util_library,
        [dynamic_transparent/1,
         upcase_atom_safe/2,
         get_module_of/2,
         call_n_times/2,
         % concat_atom_safe/3,
         makeArgIndexes/1,
         contains_singletons/1,
         doall/1,
         atom_concat_safe/3,
         exists_file_safe/1,
         exists_directory_safe/1,
         eraseall/2,
         time_file_safe/2,
         
         maplist_safe/2,
         maplist_safe/3,
         subst/4,
         predsubst/3,
         wsubst/4,
         remove_dupes/2,
         get_functor/2,
         get_functor/3,
         functor_h/2,
         functor_h/3,
         flatten_set/2,
         at_start/1,
         in_thread_and_join/1,
         in_thread_and_join/2,        
         asserta_new/1,
         assertz_new/1,
         as_clause/3,
         asserta_if_new/1,
         assertz_if_new/1,
         assertz_if_new_clause/1,
         assertz_if_new_clause/2,
         throw_if_true_else_fail/2,
         assert_if_new/1,
         safe_univ/2,
         clause_asserted/2,
         clause_asserted/1,         
         make_list/3,
         if_file_exists/1,
         multi_transparent/1]).


% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- dynamic(double_quotes_was_lib/1).
:- multifile(double_quotes_was_lib/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_lib(WAS)).
:- retract(double_quotes_was_lib(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_lib(WAS)).
:- set_prolog_flag(double_quotes,string).

:- meta_predicate(if_file_exists(:)).
if_file_exists(M:Call):- arg(1,Call,File),(filematch(File,_)-> must((filematch(File,X),exists_file(X),call(M:Call)));fmt(not_installing(M,Call))),!.

:-export(filematch/2).
:-export(filematch/3).
filematch(Mask,File1):-filematch('./',Mask,File1).
filematch(RelativeTo,Mask,File1):-absolute_file_name(Mask,File1,[expand(true),extensions(['',plmoo,pl,'pl.in']),file_errors(fail),solutions(all),relative_to(RelativeTo),access(read)]).


:- ensure_loaded(logicmoo_util_bugger_new).
:- ensure_loaded(logicmoo_util_bugger_catch).
:- '@'( ensure_loaded(logicmoo_util_bugger), 'user').

% :-user_use_module(logicmoo(logicmoo_util/logicmoo_util_strings)).
% :-user_use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).

lastMember2(_E,List):-var(List),!,fail.
lastMember2(E,[H|List]):-lastMember2(E,List);E=H.


safe_univ(Call,List):-hotrace(safe_univ0(Call,List)),!.

safe_univ0(M:Call,[N:L|List]):- nonvar(M),nonvar(N),!,safe_univ0(Call,[L|List]).
safe_univ0(Call,[M:L|List]):- nonvar(M),!,safe_univ(Call,[L|List]).
safe_univ0(M:Call,[L|List]):- nonvar(M),!,safe_univ(Call,[L|List]).
safe_univ0(Call,[L|List]):- not(is_list(Call)), Call =..[L|List],!,warn_bad_functor(L).
safe_univ0([L|List],[L|List]):- var(List),atomic(Call),!,grtrace,Call =.. [L|List],warn_bad_functor(L).
safe_univ0(Call,[L|List]):- ccatch(Call =.. [L|List],E,(dumpST,'format'('~q~n',[E=safe_univ(Call,List)]))),warn_bad_functor(L).

:-export(append_term/3).
append_term(T,I,HEAD):-atom(T),HEAD=..[T,I],!.
append_term(Call,E,CallE):- Call=..List, append(List,[E],ListE), CallE=..ListE.

% =================================================================================
% Utils
% =================================================================================


:-export(each_subterm/2).
each_subterm(B, A):- (compound(B), arg(_, B, C), each_subterm(C, A));A=B.

:-export(each_subterm/3).
each_subterm(A,Pred,B):- call( Pred,A,B).
each_subterm(A,Pred,O):- 
   compound(A),
   once(  A=[H|T] ;  A=..[H|T] ),
   (each_subterm(H,Pred,O);
     each_subterm(T,Pred,O)).



:-dynamic(argNumsTracked/3).
:-dynamic(argNFound/3).
% :-index(argNFound(1,1,1)).

makeArgIndexes(CateSig):-functor_catch(CateSig,F,_),makeArgIndexes(CateSig,F),!.
makeArgIndexes(CateSig,F):- argNumsTracked(F,Atom,Number),arg(Number,CateSig,Arg),user:nonvar(Arg),
     %%Number<10,user:nonvar(Arg),atom_number(Atom,Number),
     assert_if_new(argNFound(F,Atom,Arg)),fail.
makeArgIndexes(_NEW,_F).



% peekAttributes/2,pushAttributes/2,pushCateElement/2.
:- module_transparent((asserta_new/1,asserta_if_new/1,assertz_new/1,assertz_if_new/1,assert_if_new/1,assertz_if_new_clause/1,assertz_if_new_clause/2,clause_asserted/2,as_clause/2,clause_asserted/1,eraseall/2)).
:- meta_predicate asserta_new(:),asserta_if_new(:),assertz_new(:),assertz_if_new(:),assert_if_new(:),assertz_if_new_clause(:),assertz_if_new_clause(:,:).
:- meta_predicate clause_asserted(-,-),as_clause(-,-),clause_asserted(-),eraseall(-,-).

asserta_new(_Ctx,NEW):-ignore((retract(NEW),fail)),asserta(NEW).
writeqnl(_Ctx,NEW):- fmt('~q.~n',[NEW]),!.

eraseall(M:F,A):-!,forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,_,X),erase(X))).
eraseall(F,A):-forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,_,X),erase(X))).

asserta_new(NEW):-ignore((retract(NEW),fail)),asserta(NEW).
assertz_new(NEW):-ignore((retract(NEW),fail)),assertz(NEW).

assert_if_new(N):-clause_asserted(N)->true;asserta(N).

assertz_if_new(N):-clause_asserted(N)->true;assertz(N).

asserta_if_new(N):-clause_asserted(N)->true;asserta(N).

assertz_if_new_clause(C):- as_clause(C,H,B),assertz_if_new_clause(H,B).

assertz_if_new_clause(H,B):-clause_asserted(H,B),!.
assertz_if_new_clause(H,B):-assertz((H:-B)).

as_clause( M:((H :- B)),M:H,B):-!.
as_clause( ((H :- B)),H,B):-!.
as_clause( H,  H,  true).

clause_asserted(C):- as_clause(C,H,B),!,clause_asserted(H,B).
% clause_asserted(H,B):- predicate_property(H,number_of_clauses(N)),N>0, \+ \+ ((numbervars(H:B),clause(H,B))).
clause_asserted(H,B):- predicate_property(H,number_of_clauses(N)),N>0,copy_term(H:B,HH:BB),!, clause(HH, BB, Ref),must(clause(Head, Body, Ref)),
  same_body(B,Body), same_heads(H,Head).

same_body(B,Body):-same_heads(B,Body).

same_heads(H,Head):-H=@=Head,!.
same_heads(M1:H,M2:Head):-H=@=Head,!,dtrace(dmsg(warn(same_heads(M1:H,M2:Head)))).
same_heads(H,M2:Head):-H=@=Head,!,dtrace(dmsg(warn(same_heads(_M1:H,M2:Head)))).
same_heads(M1:H,Head):-H=@=Head,!,dtrace(dmsg(warn(same_heads(M1:H,_M2:Head)))).

:-meta_predicate clause_safe(:, ?).
:-module_transparent clause_safe/2.
:-export(clause_safe/2).

:-export(clause_safe_m3/3).

clause_safe(M:H,B):-!,debugOnError(clause(M:H,B)).
clause_safe(H,B):-!,debugOnError(clause(H,B)).

clause_safe(M:H,B):-!,clause_safe_m3(M,H,B).
clause_safe(H,B):-!,clause_safe_m3(_,H,B).
%clause_safe(M,string(S),B):- trace_or_throw(clause_safe(M,string(S),B)).
clause_safe_m3(M,H,B):-  (nonvar(H)->true;(trace,current_predicate(M:F/A),functor(H,F,A))),predicate_property(M:H,number_of_clauses(_)),clause(H,B).



:- meta_predicate doall(:).
doall(M:C):-!, M:ignore(M:(C,fail)).
doall(C):-ignore((C,fail)).

% :- ensure_loaded(logicmoo_util_bugger).

proccess_status(_,exited(called(Det,Goal)),called(Goal2)):- Det = true,!,must_det(Goal=Goal2).
proccess_status(ID,exited(called(Det,Goal)),called(Goal2)):- dmsg(nondet_proccess_status(ID,exited(called(Det,Goal)),called(Goal2))),!,must_det(Goal=Goal2).
proccess_status(ID,true,Want):- dmsg(proccess_status(ID,true,Want)),!.
proccess_status(ID,false,Want):- dmsg(failed_proccess_status(ID,false,Want)),!,fail.
proccess_status(ID,exception(Status),Want):- dmsg(exception(Status, ID,false,Want)),!,throw(Status).
proccess_status(ID,exited(Other),Want):-dmsg(wierd_proccess_status(ID,exited(Other),Want)),!.

:- meta_predicate in_thread_and_join(0).
in_thread_and_join(Goal):-thread_create((Goal,deterministic(Det),thread_exit(called(Det,Goal))),ID,[detatched(false)]),thread_join(ID,Status),show_call(proccess_status(ID,Status,called(Goal))).
:- meta_predicate in_thread_and_join(0,+).
in_thread_and_join(Goal,Status):-thread_create(Goal,ID,[]),thread_join(ID,Status).


% ===================================================================
% Substitution based on Pred
% ===================================================================

% Usage: predsubst(+Fml,+Pred,?FmlSk)

predsubst(A,Pred, D):- 
      ccatch(notrace(nd_predsubst(A,Pred,D)),_,fail),!.
predsubst(A,_B,A).

nd_predsubst(  Var, Pred,SUB ) :- call(Pred,Var,SUB).
nd_predsubst(  Var, _,Var ) :- var(Var),!.
nd_predsubst(  P, Pred, P1 ) :- functor_catch(P,_,N),nd_predsubst1( Pred, P, N, P1 ).

nd_predsubst1( _,  P, 0, P  ).
nd_predsubst1( Pred,  P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_predsubst2( Pred,  Args, ArgS ),
            nd_predsubst2( Pred, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_predsubst2( _, [], [] ).
nd_predsubst2( Pred, [A|As], [Sk|AS] ) :- call(Pred,A,Sk), !, nd_predsubst2( Pred, As, AS).
nd_predsubst2( Pred, [A|As], [A|AS]  ) :- var(A), !, nd_predsubst2( Pred, As, AS).
nd_predsubst2( Pred, [A|As], [Ap|AS] ) :- nd_predsubst( A,Pred,Ap ),nd_predsubst2( Pred, As, AS).
nd_predsubst2( _, L, L ).


% ===================================================================
% Substitution based on +PRED
% ===================================================================
/*
% Usage: subst(+Pred,+Fml,+X,+Sk,?FmlSk)

pred_subst(Pred,A,B,C,D):-  ccatch(hotrace(nd_pred_subst(Pred,A,B,C,D)),E,(dumpST,dmsg(E:nd_pred_subst(Pred,A,B,C,D)),fail)),!.
pred_subst(_,A,_B,_C,A).

nd_pred_subst(Pred,  Var, VarS,SUB,SUB ) :- call(Pred, Var,VarS),!.
nd_pred_subst(_Pred,  Var, _,_,Var ) :- var(Var),!.

nd_pred_subst(Pred,  P, X,Sk, P1 ) :- functor(P,_,N),nd_pred_subst1(Pred, X, Sk, P, N, P1 ).

nd_pred_subst1(_Pred, _,  _, P, 0, P  ).
nd_pred_subst1(Pred, X, Sk, P, N, P1 ) :- N > 0, univ_safe(P , [F|Args]), 
            nd_pred_subst2(Pred, X, Sk, Args, ArgS ),
            nd_pred_subst2(Pred, X, Sk, [F], [FS] ),  
            univ_safe(P1 , [FS|ArgS]).

nd_pred_subst2(_, _,  _, [], [] ).
nd_pred_subst2(Pred, X, Sk, [A|As], [Sk|AS] ) :- call(Pred, X , A), !, nd_pred_subst2(Pred, X, Sk, As, AS).
nd_pred_subst2(Pred, X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_pred_subst2(Pred, X, Sk, As, AS).
nd_pred_subst2(Pred, X, Sk, [A|As], [Ap|AS] ) :- nd_pred_subst(Pred, A,X,Sk,Ap ),nd_pred_subst2(Pred, X, Sk, As, AS).
nd_pred_subst2(_, _X, _Sk, L, L ).

*/

% -- CODEBLOCK
% Usage: pred_subst(+Pred,+Fml,+X,+Sk,?FmlSk)
:-export(pred_subst/5).

pred_subst( Pred, P,       X,Sk,       P1    ) :- call(Pred,P,X),!,must( Sk=P1),!.
pred_subst(_Pred, P,       _,_ ,       P1    ) :- is_ftVar(P),!, must(P1=P),!.
pred_subst( Pred,[P|Args], X,Sk,    [P1|ArgS]) :- !, pred_subst(Pred,P,X,Sk,P1),!, must(pred_subst( Pred, Args,X, Sk, ArgS )),!.
pred_subst( Pred, P,       X,Sk,       P1    ) :- compound(P),!, P =..Args, pred_subst( Pred, Args,X, Sk, ArgS ),!, must(P1 =..ArgS),!.
pred_subst(_Pred ,P,       _, _,       P     ).

% dcgPredicate(M,F,A,P).

univ_safe(P,[L|L1]):- nonvar(P), must_det((var(L);atom(L))),!,debugOnError(( P=..[L|L1] )).
univ_safe(P,L):- must_det(is_list(L)),debugOnError((P=..L)).

% ===================================================================
% Substitution based on ==
% ===================================================================

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

:-moo_hide_childs(subst/4).

subst(A,B,C,D):-  notrace((ccatch(notrace(nd_subst(A,B,C,D)),E,(dumpST,dmsg(E:nd_subst(A,B,C,D)),fail)))),!.
subst(A,_B,_C,A).

nd_subst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_subst(  Var, _,_,Var ) :- var(Var),!.

nd_subst(  P, X,Sk, P1 ) :- functor(P,_,N),nd_subst1( X, Sk, P, N, P1 ).

nd_subst1( _,  _, P, 0, P  ).
nd_subst1( X, Sk, P, N, P1 ) :- N > 0, univ_safe(P , [F|Args]), 
            nd_subst2( X, Sk, Args, ArgS ),
            nd_subst2( X, Sk, [F], [FS] ),  
            univ_safe(P1 , [FS|ArgS]).

nd_subst2( _,  _, [], [] ).
nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- nd_subst( A,X,Sk,Ap ),nd_subst2( X, Sk, As, AS).
nd_subst2( _X, _Sk, L, L ).


wsubst(A,B,C,D):- 
      ccatch(notrace(weak_nd_subst(A,B,C,D)),_,fail),!.
wsubst(A,_B,_C,A).

weak_nd_subst(  Var, VarS,SUB,SUB ) :- nonvar(Var),Var=VarS,!.
weak_nd_subst(        P, X,Sk,        P1 ) :- functor_catch(P,_,N),weak_nd_subst1( X, Sk, P, N, P1 ).

weak_nd_subst1( _,  _, P, 0, P  ).

weak_nd_subst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], weak_nd_subst2( X, Sk, Args, ArgS ),
            weak_nd_subst2( X, Sk, [F], [FS] ),
            P1 =.. [FS|ArgS].

weak_nd_subst2( _,  _, [], [] ).
weak_nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- nonvar(A), X = A, !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- weak_nd_subst( A,X,Sk,Ap ),weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( _X, _Sk, L, L ).


make_list(E,1,[E]):-!.
make_list(E,N,[E|List]):- M1 is N - 1, make_list(E,M1,List),!.

:- meta_predicate get_module_of_4(0,+,+,-).
get_module_of_4(_P,F,A,ModuleName):- current_module(ModuleName),module_property(ModuleName, exports(List)),member(F/A,List),!.
get_module_of_4(_P,F,A,M):- current_predicate(M0:F0/A0),F0=F,A0=A,!,M=M0.
get_module_of_4(P,F,A,M):-trace_or_throw((get_module_of_4(P,F,A,M))).

/*
get_module_of_4(_P,F,A,M):- current_predicate(F0/A0),F0=F,A0=A,!,dbase_mod(M).
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

:-export(flatten_set/2).
flatten_set(L,S):-flatten([L],F),list_to_set(F,S),!.
%flatten_set(Percepts0,Percepts):- flatten([Percepts0],Percepts1),remove_dupes(Percepts1,Percepts).

remove_dupes(In,Out):-remove_dupes(In,Out,[]).

remove_dupes([],[],_):-!.
remove_dupes([I|In],Out,Shown):-member(I,Shown),!,remove_dupes(In,Out,Shown).
remove_dupes([I|In],[I|Out],Shown):-remove_dupes(In,Out,[I|Shown]).

functor_h(Obj,F):-notrace(functor_h(Obj,F,_)).
get_functor(Obj,F):-notrace(functor_h(Obj,F,_)).
get_functor(Obj,F,A):-notrace(functor_h(Obj,F,A)).

functor_h(Obj,F,A):-var(Obj),trace_or_throw(var_functor_h(Obj,F,A)).
functor_h(Obj,F,A):-var(Obj),!,(number(A)->functor(Obj,F,A);((current_predicate(F/A);throw(var_functor_h(Obj,F,A))))).
functor_h(F/A,F,A):-number(A),!,( atom(F) ->  true ; current_predicate(F/A)).
functor_h(':'(_,Obj),F,A):-nonvar(Obj),!,functor_h(Obj,F,A).
functor_h(M:_,F,A):- atom(M),!, ( M=F -> current_predicate(F/A) ; current_predicate(M:F/A)).
functor_h(':-'(Obj),F,A):-!,functor_h(Obj,F,A).
functor_h(':-'(Obj,_),F,A):-!,functor_h(Obj,F,A).
functor_h(Obj,F,0):- string(Obj),!,must_det(atom_string(F,Obj)).
functor_h(Obj,Obj,0):-not(compound(Obj)),!.
functor_h(Obj,F,A):-functor_catch(Obj,F,A).


:- dynamic_multifile_exported((do_expand_args/3)).

do_expand_args(Exp,Term,Out):- compound(Term),!,do_expand_args_c(Exp,Term,Out).
do_expand_args(_,Term,Term).

do_expand_args_c(Exp,[L|IST],Out):- !,do_expand_args_l(Exp,[L|IST],Out).
do_expand_args_c(Exp,Term,Out):- Term=..[P|ARGS],do_expand_args_pa(Exp,P,ARGS,Out).

do_expand_args_pa(Exp,Exp,ARGS,Out):- !,member(Out,ARGS).
do_expand_args_pa(Exp,P,ARGS,Out):- do_expand_args_l(Exp,ARGS,EARGS), Out=..[P|EARGS].

do_expand_args_l(_,A,A):- var(A),!.
do_expand_args_l(_,[],[]):- !.
do_expand_args_l(Exp,[A|RGS],[E|ARGS]):- do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).



% :- moo_hide_childs(functor_safe/2).
% :- moo_hide_childs(functor_safe/3).


:- meta_predicate call_n_times(+,0).
call_n_times(0,_Goal):-!.
call_n_times(1,Goal):-!,Goal.
call_n_times(N,Goal):-doall((between(2,N,_),once(Goal))),Goal.


:- meta_predicate at_start(0).
:-dynamic(at_started/1).
at_start(Goal):-
	copy_term(Goal,Named),
	numbervars(Named,0,_,[attvar(bind),singletons(true)]),
	copy_term(Named,Named2),
        (    at_started(Named)
	->
	     true
	;
	     ccatch(
		 (assert(at_started(Named2)),debugOnFailure0((Goal))),
		 E,
		 (retractall(at_started(Named2)),trace_or_throw(E)))
	).

dynamic_multifile(Pred/N):-
   dynamic(Pred/N),
   multifile(Pred/N),
   module_transparent(Pred/N).


:-dynamic_multifile(local_directory_search/1).

local_directory_search_combined(X):-local_directory_search(X).
local_directory_search_combined(X):-local_directory_search_combined2(X).
% for now dont do the concat 3 version
local_directory_search_combined(PL):-local_directory_search_combined2(A),local_directory_search(B),join_path(A,B,PL),exists_directory_safe(PL).
local_directory_search_combined2(PL):-local_directory_search(A),local_directory_search(B),join_path(A,B,PL),exists_directory_safe(PL).



dynamic_transparent([]):-!.
dynamic_transparent([X]):-dynamic_transparent(X),!.
dynamic_transparent([X|Xs]):-!,dynamic_transparent(X),dynamic_transparent(Xs),!.
dynamic_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A).
dynamic_transparent(F/A):-!,multi_transparent(user:F/A).
dynamic_transparent(X):-functor_catch(X,F,A),dynamic_transparent(F/A),!.

multi_transparent([]):-!.
multi_transparent([X]):-multi_transparent(X),!.
multi_transparent([X|Xs]):-!,multi_transparent(X),multi_transparent(Xs),!.
multi_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A),multifile(M:F/A).
multi_transparent(F/A):-!,multi_transparent(user:F/A).
multi_transparent(X):-functor_catch(X,F,A),multi_transparent(F/A),!.


atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.
exists_file_safe(File):-bugger:must(atomic(File)),exists_file(File).
exists_directory_safe(File):-bugger:must(atomic(File)),exists_directory(File).
/*
concat_atom_safe(List,Sep,[Atom]):-atom(Atom),!,concat_atom(List,Sep,Atom),!.
concat_atom_safe(List,Sep,Atom):-atom(Atom),!,concat_atom(ListM,Sep,Atom),!,List = ListM.
concat_atom_safe(List,Sep,Atom):- concat_atom(List,Sep,Atom),!.
*/
upcase_atom_safe(A,B):-atom(A),upcase_atom(A,B),!.
time_file_safe(F,INNER_XML):-exists_file_safe(F),time_file(F,INNER_XML).

:-export(list_to_set_safe/2).
list_to_set_safe(A,A):-(var(A);atomic(A)),!.
list_to_set_safe([A|AA],BB):- (not(not(lastMember2(A,AA))) -> list_to_set_safe(AA,BB) ; (list_to_set_safe(AA,NB),BB=[A|NB])),!.


%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[X,X,X],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST), bugger:debugOnFailure(apply(Pred,[E]))),LISTO),!, ignore(LIST=LISTO),!.
% though this should been fine %  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), debugOnFailure(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.

maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),debugOnFailure(apply(Pred,[E,EE])))), LISTO),  ignore(LIST=LISTO),!.
% though this should been fine % maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), debugOnFailureEach(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, ignore(OUT=[AA|BB]).



% TODO remove this next line
% :-ensure_loaded(logicmoo_util_bugger).
% and replace with...


term_parts(A,[A]):- not(compound(A)),!.
term_parts([A|L],TERMS):-!,term_parts_l([A|L],TERMS).
term_parts(Comp,[P/A|TERMS]):- functor_catch(Comp,P,A), Comp=..[P|List],term_parts_l(List,TERMS).

term_parts_l(Var,[open(Var),Var]):-var(Var),!.
term_parts_l([],[]):-!.
term_parts_l([A|L],TERMS):-!,term_parts(A,AP),term_parts_l(L,LP),append(AP,LP,TERMS).
term_parts_l(Term,[open(Term)|TERMS]):-term_parts(Term,TERMS),!.

pred_term_parts(Pred,A,[A]):- call(Pred,A),!.
pred_term_parts(_Pred,A,[]):-not(compound(A)),!.
pred_term_parts(Pred,[A|L],TERMS):-!,pred_term_parts_l(Pred,[A|L],TERMS),!.
pred_term_parts(Pred,Comp,TERMS):-Comp=..[P,A|List],pred_term_parts_l(Pred,[P,A|List],TERMS),!.
pred_term_parts(_,_Term,[]).

pred_term_parts_l(_,NV,[]):-NV==[],!.
pred_term_parts_l(Pred,[A|L],TERMS):-!,pred_term_parts(Pred,A,AP),pred_term_parts_l(Pred,L,LP),append(AP,LP,TERMS),!.
pred_term_parts_l(Pred,Term,TERMS):-pred_term_parts(Pred,Term,TERMS),!.
pred_term_parts_l(_,_Term,[]).

throw_if_true_else_fail(T,E):- once(hotrace(T)),trace_or_throw(throw_if_true_else_fail(E:T)).

list_retain(PL,Pred,Result):- throw_if_true_else_fail(not(is_list(PL)),list_retain(PL,Pred,Result)).
list_retain([],_Pred,[]):-!.
list_retain([R|List],Pred,[R|Retained]):- call(Pred,R),!, list_retain(List,Pred,Retained).
list_retain([_|List],Pred,Retained):- list_retain(List,Pred,Retained).

:-export(identical_member/2).
identical_member(X,[Y|_])  :-
	X == Y,
	!.
identical_member(X,[_|L]) :-
	'identical_member'(X,L).

:-export(delete_eq/3).
:-export(pred_delete/4).
delete_eq(A,B,C):-pred_delete(==,A,B,C).
pred_delete(_,[], _, []).
pred_delete(Pred,[A|C], B, D) :-
        (    call(Pred,A,B)
        ->  pred_delete(Pred,C, B, D)
        ;   D=[A|E],
            pred_delete(Pred,C, B, E)
        ).


contains_singletons(Term):- not(ground(Term)),not(not((term_variables(Term,Vs),numbervars(Term,0,_,[attvar(bind),singletons(true)]),member('$VAR'('_'),Vs)))).


% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_lib(WAS)).

:- module_predicates_are_exported(logicmoo_util_library).
:- all_module_predicates_are_transparent(logicmoo_util_library).


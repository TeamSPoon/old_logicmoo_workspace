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
         concat_atom_safe/3,
         makeArgIndexes/1,
         contains_singletons/1,
         doall/1,
         atom_concat_safe/3,
         op(1150,fx,(dynamic_multifile_exported)),
         dynamic_multifile_exported/3,
         dynamic_multifile_exported/2,
         dynamic_multifile_exported/1,
         exists_file_safe/1,
         exists_directory_safe/1,
         def_meta_predicate/3,
         time_file_safe/2,
         throw_safe/1,
         maplist_safe/2,
         maplist_safe/3,
         subst/4,
         wsubst/4,
         remove_dupes/2,
         list_to_set_safe/2,
         get_functor/2,
         get_functor/3,
         functor_safe/3,
         flatten_dedupe/2,
         at_start/1,
         in_thread_and_join/1,
         in_thread_and_join/2,
         asserta_new/1,
         asserta_if_new/1,
         throw_if_true_else_fail/2,
         assert_if_new/1,
         safe_univ/2,
         bad_functor/1,
         make_list/3,
         multi_transparent/1]).

:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)), 'user').

% :-user_use_module(logicmoo(logicmoo_util/logicmoo_util_strings)).
% :-user_use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).

lastMember(_E,List):-var(List),!,fail.
lastMember(E,[H|List]):-lastMember(E,List);E=H.

bad_functor(L) :- arg(_,v('|','.',[],':','/'),L).

warn_bad_functor(L):-ignore((notrace(bad_functor(L)),!,dumpST,dtrace,trace_or_throw(bad_functor(L)))).

safe_univ(M:Call,[N:L|List]):- nonvar(M),nonvar(N),!,safe_univ(Call,[L|List]).
safe_univ(Call,[M:L|List]):- nonvar(M),!,safe_univ(Call,[L|List]).
safe_univ(M:Call,[L|List]):- nonvar(M),!,safe_univ(Call,[L|List]).
safe_univ(Call,[L|List]):- not(is_list(Call)),Call =..[L|List],!,warn_bad_functor(L).
safe_univ([L|List],[L|List]):- var(List),atomic(Call),!,grtrace,Call =.. [L|List],warn_bad_functor(L).
safe_univ(Call,[L|List]):- catch(Call =.. [L|List],E,(format('~q~n',[E=safe_univ(Call,List)]))),warn_bad_functor(L).

% =================================================================================
% Utils
% =================================================================================

:-dynamic(argNumsTracked/3).
:-dynamic(argNFound/3).
% :-index(argNFound(1,1,1)).

makeArgIndexes(CateSig):-functor(CateSig,F,_),makeArgIndexes(CateSig,F),!.
makeArgIndexes(CateSig,F):- argNumsTracked(F,Atom,Number),arg(Number,CateSig,Arg),user:nonvar(Arg),
     %%Number<10,user:nonvar(Arg),atom_number(Atom,Number),
     assert_if_new(argNFound(F,Atom,Arg)),fail.
makeArgIndexes(_NEW,_F).



% peekAttributes/2,pushAttributes/2,pushCateElement/2.
:- meta_predicate asserta_new(:),asserta_if_new(:),assertz_new(:),assertz_if_new(:),assert_if_new(:).
asserta_new(_Ctx,NEW):-ignore(retractall(NEW)),asserta(NEW).
writeqnl(_Ctx,NEW):- fmt('~q.~n',[NEW]),!.

asserta_new(NEW):-ignore(retractall(NEW)),asserta(NEW).
assertz_new(NEW):-ignore(retractall(NEW)),assertz(NEW).

assert_if_new(N):-catch(N,_,fail),!.
assert_if_new(N):-assert(N),!.

assertz_if_new(N):-catch(N,_,fail),!.
assertz_if_new(N):-assertz(N),!.

asserta_if_new(N):-catch(N,_,fail),!.
asserta_if_new(N):-asserta(N),!.

:- meta_predicate doall(0).
doall(C):-ignore((C,fail)).

:- user_use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)).

:- meta_predicate in_thread_and_join(0).
in_thread_and_join(Goal):-in_thread_and_join(Goal,_Status).
:- meta_predicate in_thread_and_join(0,+).
in_thread_and_join(Goal,Status):-thread_create(Goal,ID,[]),thread_join(ID,Status).

% ===================================================================
% Substitution based on ==
% ===================================================================

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

subst(A,B,C,D):- 
      catch(notrace(nd_subst(A,B,C,D)),_,fail),!.
subst(A,_B,_C,A).

nd_subst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_subst(  P, X,Sk, P1 ) :- functor(P,_,N),nd_subst1( X, Sk, P, N, P1 ).

nd_subst1( _,  _, P, 0, P  ).
nd_subst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_subst2( X, Sk, Args, ArgS ),
            nd_subst2( X, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_subst2( _,  _, [], [] ).
nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- nd_subst( A,X,Sk,Ap ),nd_subst2( X, Sk, As, AS).
nd_subst2( _X, _Sk, L, L ).



wsubst(A,B,C,D):- 
      catch(notrace(weak_nd_subst(A,B,C,D)),_,fail),!.
wsubst(A,_B,_C,A).

weak_nd_subst(  Var, VarS,SUB,SUB ) :- nonvar(Var),Var=VarS,!.
weak_nd_subst(        P, X,Sk,        P1 ) :- functor(P,_,N),weak_nd_subst1( X, Sk, P, N, P1 ).

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
get_module_of_4(P,F,A,M):-throw((get_module_of_4(P,F,A,M))).

/*
get_module_of_4(_P,F,A,M):- current_predicate(F0/A0),F0=F,A0=A,!,moo:dbase_mod(M).
get_module_of_4(_P,F,A,_M):-trace, dbase:isCycPredArity(F,A),!,fail.
get_module_of_4(P,F,A,M):- trace, debugCall(get_module_of_4(P,F,A,M)).
*/

:- meta_predicate get_module_of(0,-).
get_module_of(V,M):-var(V),!,current_module(M).
get_module_of(F/A,M):-!,functor(P,F,A),!,get_module_of(P,M).
get_module_of(P,M):-predicate_property(P,imported_from(M)),!.
get_module_of(P,M):-predicate_property(_:P,imported_from(M)),!.
get_module_of(MM:_,M):-!,MM=M.
get_module_of(P,M):-functor(P,F,A),get_module_of_4(P,F,A,M).


:- meta_predicate def_meta_predicate(0,+,+).

def_meta_predicate(M:F,S,E):-!,doall(((between(S,E,N),make_list('?',N,List),CALL=..[F|List],'@'(meta_predicate(CALL),M)))).
def_meta_predicate(F,S,E):-throw((def_meta_predicate(F,S,E))).

:- meta_predicate dynamic_multifile_exported(0), dynamic_multifile_exported(+,+), dynamic_multifile_exported(+,+,+).

dynamic_multifile_exported(M:FA):- !,dynamic_multifile_exported(M,FA).
dynamic_multifile_exported( FA ):- leash(+call),trace, !, current_module(M),trace,dynamic_multifile_exported(M,FA).

dynamic_multifile_exported(_,M:F/A):-!,dynamic_multifile_exported(M,F,A).
dynamic_multifile_exported(_, M:F ):-!,dynamic_multifile_exported(M,F).
dynamic_multifile_exported(M, [A] ):-!,dynamic_multifile_exported(M,A).
dynamic_multifile_exported(M,[A|L]):-!,dynamic_multifile_exported(M,A),dynamic_multifile_exported(M,L).
dynamic_multifile_exported(M,(A,L)):-!,dynamic_multifile_exported(M,A),dynamic_multifile_exported(M,L).
dynamic_multifile_exported(M, F/A ):-!,dynamic_multifile_exported(M,F,A).


dynamic_multifile_exported(M,F,A):- integer(A),atom(M),atom(F),!, '@'(( dynamic(F/A), multifile(F/A), M:export(F/A)), M).
dynamic_multifile_exported(M,F,A):-throw((dynamic_multifile_exported(M,F,A))).

flatten_dedupe(Percepts0,Percepts):-
   flatten([Percepts0],Percepts1),remove_dupes(Percepts1,Percepts).

remove_dupes(In,Out):-remove_dupes(In,Out,[]).

remove_dupes([],[],_):-!.
remove_dupes([I|In],Out,Shown):-member(I,Shown),!,remove_dupes(In,Out,Shown).
remove_dupes([I|In],[I|Out],Shown):-remove_dupes(In,Out,[I|Shown]).

get_functor(Obj,F):-get_functor(Obj,F,_).

get_functor(Obj,F,_):-var(Obj),throw(get_functor(Obj,F)).
get_functor(_:Obj,F,A):-!,get_functor(Obj,F,A).
get_functor(Obj,F,0):- string(Obj),!,atom_string(F,Obj).
get_functor(Obj,Obj,0):-not(compound(Obj)),!.
get_functor(Obj,F,A):-functor(Obj,F,A).

strip_f_module(_:P,FA):-nonvar(P),!,strip_f_module(P,F),!,F=FA.
strip_f_module(P,PA):-atom(P),!,P=PA.
strip_f_module(P,FA):- notrace(string(P);is_list(P);atomic(P)), text_to_string(P,S),!,atom_string(F,S),!,F=FA.
strip_f_module(P,P).

functor_safe(P,F,A):-functor_safe0(P,F,A),!.
functor_safe0(M:P,M:F,A):-var(P),atom(M),functor(P,F,A),!,warn_bad_functor(F).
functor_safe0(P,F,A):-var(P),strip_f_module(F,F0),functor(P,F0,A),!,warn_bad_functor(F).
functor_safe0(P,F,A):-compound(P),!,functor_safe_compound(P,F,A),warn_bad_functor(F).
functor_safe0(P,F,0):- notrace(string(P);atomic(P)), text_to_string(P,S),!,atom_string(F,S),warn_bad_functor(F).
functor_safe_compound((_,_),',',2).
functor_safe_compound([_|_],'.',2).
functor_safe_compound(_:P,F,A):- functor(P,F,A),!.
functor_safe_compound(P,F,A):- functor(P,F,A).
functor_safe_compound(P,F,A):- var(F),strip_f_module(P,P0),!,functor(P0,F0,A),strip_f_module(F0,F),!.
functor_safe_compound(P,F,A):- strip_f_module(P,P0),strip_f_module(F,F0),!,functor(P0,F0,A).

% :- moo_hide_childs(functor_safe/2).
% :- moo_hide_childs(functor_safe/3).

:- meta_predicate at_start(0).

call_n_times(0,_Goal):-!.
call_n_times(1,Goal):-!,Goal.
call_n_times(N,Goal):-between(2,N,_),Goal.
call_n_times(_,Goal):-Goal.


:-dynamic(at_started/1).
at_start(Goal):-
	copy_term(Goal,Named),
	numbervars(Named,1,_),
	copy_term(Named,Named2),
        (    at_started(Named)
	->
	     true
	;
	     catch(
		 (assert(at_started(Named2)),debugOnFailure0((Goal))),
		 E,
		 (retractall(at_started(Named2)),throw(E)))
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
dynamic_transparent(X):-functor(X,F,A),dynamic_transparent(F/A),!.

multi_transparent([]):-!.
multi_transparent([X]):-multi_transparent(X),!.
multi_transparent([X|Xs]):-!,multi_transparent(X),multi_transparent(Xs),!.
multi_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A),multifile(M:F/A).
multi_transparent(F/A):-!,multi_transparent(user:F/A).
multi_transparent(X):-functor(X,F,A),multi_transparent(F/A),!.

:- module_transparent(library_directory/1).

throw_safe(Exc):-throw(Exc).
atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.
exists_file_safe(File):-bugger:must(atomic(File)),exists_file(File).
exists_directory_safe(File):-bugger:must(atomic(File)),exists_directory(File).
concat_atom_safe(List,Sep,[Atom]):-atom(Atom),!,concat_atom(List,Sep,Atom),!.
concat_atom_safe(List,Sep,Atom):-atom(Atom),!,concat_atom(ListM,Sep,Atom),!,List = ListM.
concat_atom_safe(List,Sep,Atom):- concat_atom(List,Sep,Atom),!.
upcase_atom_safe(A,B):-atom(A),upcase_atom(A,B),!.
time_file_safe(F,INNER_XML):-exists_file_safe(F),time_file(F,INNER_XML).
list_to_set_safe(A,A):-(var(A);atomic(A)),!.
list_to_set_safe([A|AA],BB):- (not(not(lastMember(A,AA))) -> list_to_set_safe(AA,BB) ; (list_to_set_safe(AA,NB),BB=[A|NB])),!.


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


:- dynamic(buggerDir/1).
:- abolish(buggerDir/1),prolog_load_context(directory,D),asserta(buggerDir(D)).
:- dynamic(buggerFile/1).
:- abolish(buggerFile/1),prolog_load_context(source,D),asserta(buggerFile(D)).


hasLibrarySupport :- absolute_file_name(logicmoo('logicmoo_util/logicmoo_util_library.pl'),File),exists_file(File).

throwNoLib:- trace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(library_directory), throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

addLibraryDir :- buggerDir(Here),atom_concat(Here,'/..',UpOne), absolute_file_name(UpOne,AUpOne),asserta(user:library_directory(AUpOne)).

% if not has library suport, add this direcotry as a library directory
:-not(hasLibrarySupport) -> addLibraryDir ; true .

:-hasLibrarySupport->true;throwNoLib.

% TODO remove this next line
:-user_use_module(logicmoo('logicmoo_util/logicmoo_util_bugger')).
% and replace with...



term_parts(A,[A]):- not(compound(A)),!.
term_parts([A|L],TERMS):-!,term_parts_l([A|L],TERMS).
term_parts(Comp,[P/A|TERMS]):- functor(Comp,P,A), Comp=..[P|List],term_parts_l(List,TERMS).

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

throw_if_true_else_fail(T,E):- once(hotrace(T)),throw(throw_if_true_else_fail(E:T)).

list_retain(PL,Pred,Result):- throw_if_true_else_fail(not(is_list(PL)),list_retain(PL,Pred,Result)).
list_retain([],_Pred,[]):-!.
list_retain([R|List],Pred,[R|Retained]):- call(Pred,R),!, list_retain(List,Pred,Retained).
list_retain([_|List],Pred,Retained):- list_retain(List,Pred,Retained).

identical_member(X,[Y|_])  :-
	X == Y,
	!.
identical_member(X,[_|L]) :-
	'identical_member'(X,L).


contains_singletons(Term):- not(ground(Term)),not(not((term_variables(Term,Vs),numbervars(Term,0,_,[attvar(skip),singletons(true)]),member('$VAR'('_'),Vs)))).

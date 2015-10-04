/** <module> Logicmoo Debug Tools
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
:- if(\+ current_module(logicmoo_utils)).
:- module(logicmoo_util_bugger_catch,
    [  % when the predciates are not being moved from file to file the exports will be moved here
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
:- include(logicmoo_util_header).
:- endif.

:- meta_predicate clause_safe(?, ?).
:- module_transparent clause_safe/2.
:- export(clause_safe/2).


clause_safe(M:H,B):-!,predicate_property(M:H,number_of_clauses(_)),clause(H,B).
clause_safe(H,B):-predicate_property(_:H,number_of_clauses(_)),clause(H,B).


/*
:- export(call_with_attvars/2).
:- module_transparent(call_with_attvars/2).
:- meta_predicate call_with_attvars(1,?).

call_with_attvars(asserta,Term):-!,really_with_attvars(asserta,Term),!.
call_with_attvars(assertz,Term):-!,really_with_attvars(assertz,Term),!.
call_with_attvars(assert,Term):-!,really_with_attvars(assert,Term),!.
call_with_attvars(OP,Term):-unnumbervars(Term,Unumbered),!,call(OP,Unumbered).
*/

:- export(really_with_attvars/2).
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

:-export(clause_asserted/1).
clause_asserted(C):- as_clause(C,H,B),clause_asserted(H,B).

:-export(clause_asserted/2).
clause_asserted(H,B):-clause_asserted(H,B,_).
clause_asserted(H,B,Ref):- predicate_property(H,number_of_clauses(N))-> 
   N>0   -> (clause(H, B, Ref), clause(Head, Body, Ref),  (B =@= Body), (H =@= Head)),!.

:-export(retract_eq/1).
retract_eq(HB):-as_clause(HB,H,B),show_call_failure(predicate_property(H,number_of_clauses(_))),clause_asserted(H,B,Ref),erase(Ref).


:-export(safe_univ/2).
safe_univ(Call,List):-hotrace(safe_univ0(Call,List)),!.

safe_univ0(M:Call,[N:L|List]):- nonvar(M),nonvar(N),!,safe_univ0(Call,[L|List]).
safe_univ0(Call,[M:L|List]):- nonvar(M),!,safe_univ(Call,[L|List]).
safe_univ0(M:Call,[L|List]):- nonvar(M),!,safe_univ(Call,[L|List]).
safe_univ0(Call,[L|List]):- not(is_list(Call)),sanity(atom(L);compound(Call)), Call =..[L|List],!,warn_bad_functor(L).
safe_univ0([L|List],[L|List]):- var(List),atomic(Call),!,grtrace,Call =.. [L|List],warn_bad_functor(L).
safe_univ0(Call,[L|List]):- sanity(atom(L);compound(Call)),ccatch(Call =.. [L|List],E,(dumpST,'format'('~q~n',[E=safe_univ(Call,List)]))),warn_bad_functor(L).

:- export(append_term/3).
append_term(T,I,HEAD):-atom(T),HEAD=..[T,I],!.
append_term(Call,E,CallE):-var(Call), must(compound(CallE)),CallE=..ListE,append(List,[E],ListE),Call=..List.
append_term(Call,E,CallE):-must(compound(Call)), Call=..List, append(List,[E],ListE), CallE=..ListE.



:-export(erase_safe/2).
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
   -> ddmsg(warn(var_erase_safe(clause(A,B),REF))) ; 
       erase(REF)).
*/


:- meta_predicate if_defined(?).
:- export(if_defined/1).
if_defined(Call):-current_predicate(_,Call)->Call.
:- meta_predicate if_defined(?,?).
:- export(if_defined/2).
if_defined(Call,Else):-current_predicate(_,Call)->(Call*->true;fail);Else.

:- meta_predicate when_defined(?).
:- export(when_defined/1).
when_defined(Call):-if_defined(Call,true).


:- export(no_slow_io/0).
:- thread_local(no_slow_io).
% no_slow_io.


:- thread_local(tlbugger:tl_always_show_dmsg).
always_show_dmsg:- thread_self(main).
always_show_dmsg:- tlbugger:tl_always_show_dmsg.


:- export(tlbugger:ifHideTrace/0).
:- thread_local(tlbugger:ifHideTrace/0).


:- meta_predicate(catchvvnt(0,?,0)).
catchvvnt(T,E,F):-catchvv(cnotrace(T),E,F).

:- thread_self(X),assert(thread_main(X)).

is_pdt_like:-thread_property(_,alias(pdt_console_server)).
is_pdt_like:-thread_main(X),!,X \= main.

is_main_thread:-thread_main(X),!,thread_self(X).

:- thread_local(tlbugger:no_colors/0).
:- thread_local(thlocal:thread_local_current_main_error_stream/1).

:- is_pdt_like-> assert(tlbugger:no_colors); true.


:- meta_predicate(with_main_error_to_output(0)).
with_main_error_to_output(Goal):-
 current_output(Out),
  with_assertions(thlocal:thread_local_current_main_error_stream(Out),Goal).
   

thread_current_error_stream(Err):- thlocal:thread_local_current_main_error_stream(Err),!.
thread_current_error_stream(Err):-thread_self(ID),thread_current_error_stream(ID,Err).

current_main_error_stream(Err):- thlocal:thread_local_current_main_error_stream(Err),!.
current_main_error_stream(Err):-thread_main(ID),thread_current_error_stream(ID,Err).

format_to_error(F,A):-current_main_error_stream(Err),!,format(Err,F,A).
fresh_line_to_err:- cnotrace((flush_output_safe,current_main_error_stream(Err),format(Err,'~N',[]),flush_output_safe(Err))).

:- dynamic(thread_current_input/2).
:- dynamic(thread_current_error_stream/2).
:- volatile(thread_current_input/2).
:- volatile(thread_current_error_stream/2).
save_streams:- thread_self(ID), save_streams(ID).

save_streams(ID):- current_input(In),thread_current_input(ID,In),!.
save_streams(ID):-
  current_input(In),asserta(thread_current_input(ID,In)),
  current_output(Err),asserta(thread_current_error_stream(ID,Err)).

:- meta_predicate(with_main_input(0)).
with_main_input(G):-
    current_output(OutPrev),
    current_input(InPrev),
    stream_property(ErrPrev,alias(user_error)),
    thread_main(ID),thread_current_input(ID,In),thread_current_error_stream(ID,Err),
    setup_call_cleanup(set_prolog_IO(In,OutPrev,Err),G,set_prolog_IO(InPrev,OutPrev,ErrPrev)).

 with_main_io(G):-
    current_output(OutPrev),
    current_input(InPrev),
    stream_property(ErrPrev,alias(user_error)),
    thread_main(ID),thread_current_input(ID,In),thread_current_error_stream(ID,Err),
    setup_call_cleanup(set_prolog_IO(In,Err,Err),G,set_prolog_IO(InPrev,OutPrev,ErrPrev)).

:- save_streams.
:- initialization(save_streams).

:- if(current_predicate(run_sanity_tests/0)).
:- listing(thread_current_error_stream/2).
:- endif.

:- dynamic(is_hiding_dmsgs).
is_hiding_dmsgs:- \+always_show_dmsg, current_prolog_flag(opt_debug,false),!.
is_hiding_dmsgs:- \+always_show_dmsg, tlbugger:ifHideTrace,!.

% bugger_debug=false turns off just debugging about the debugger
% opt_debug=false turns off all the rest of debugging
% ddmsg(_):-current_prolog_flag(bugger_debug,false),!.
ddmsg(D):-format_to_error('~Ndmsg: ~q~n',[D]).
ddmsg(F,A):- sformat(SF,'~~Ndmsg: ~w~n',[F]),format_to_error(SF,A).
ddmsg_call(D):- ( (ddmsg(ddmsg_call(D)),call(D),ddmsg(ddmsg_exit(D))) *-> true ; ddmsg(ddmsg_failed(D))).

:- meta_predicate(to_pi(?,?)).
to_pi(P,M:P):-var(P),!,current_module(M).
to_pi(M:P,M:P):-var(P),!,current_module(M).
to_pi(Find,(M:PI)):- once(catch('$find_predicate'(Find,Found),_,fail)),Found=[_|_],!,member(M:F/A,Found),functor(PI,F,A).
to_pi(M:Find,M:PI):-!,current_module(M),to_pi0(M,Find,M:PI).
to_pi(Find,M:PI):-current_module(M),to_pi0(M,Find,M:PI).

to_pi0(M,Find,M:PI):- atom(Find),!,when(bound(PI),(nonvar(PI),functor(PI,Find,_))).
to_pi0(M,Find/A,M:PI):-var(Find),number(A),!,when(bound(PI),(nonvar(PI),functor(PI,_,A))).
to_pi0(M,Find,PI):-get_pi(Find,PI0),!,(PI0\=(_:_)->(current_module(M),PI=(M:PI0));PI=PI0).


:- thread_local(thlocal:last_src_loc/2).
input_key(K):-thread_self(K).
   
show_new_src_location(FL):-input_key(K),show_new_src_location(K,FL).

show_new_src_location(K,FL):- thlocal:last_src_loc(K,FL),!.
show_new_src_location(K,FL):- retractall(thlocal:last_src_loc(K,_)),format_to_error('~N% ~w ',[FL]),!,asserta(thlocal:last_src_loc(K,FL)).


:-export( show_source_location/0).
show_source_location:- no_slow_io,!.
show_source_location:- is_hiding_dmsgs,!.
show_source_location:- current_predicate(logicmoo_util_varnames_file/0),current_source_location(FL),!,show_new_src_location(FL),!.
show_source_location.

:-export( as_clause_no_m/3).
as_clause_no_m( MHB,  H, B):- strip_module(MHB,_M,HB), as_clause( HB,  MH, MB),strip_module(MH,_M2H,H),strip_module(MB,_M2B,B).
as_clause_w_m(MHB, M, H, B):-  as_clause_w_m(MHB, M1H, H, B, M2B), (M1H==user->M2B=M;M1H=M).
as_clause_w_m(MHB, M1H, H, B, M2B):-  as_clause( MHB,  MH, MB),strip_module(MH,M1H,H),strip_module(MB,M2B,B).

:- export(is_ftCompound/1).
is_ftCompound(C):-compound(C),C\='$VAR'(_).

:- export(is_ftVar/1).
is_ftVar(V):-var(V),!.
is_ftVar('$VAR'(_)).

:- export(is_ftNonvar/1).
is_ftNonvar(V):- \+ is_ftVar(V).


%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[X,X,X],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

:- export((   maplist_safe/2,
   maplist_safe/3)).

maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST), debugOnFailure(apply(Pred,[E]))),LISTO),!, ignore(LIST=LISTO),!.
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
% (catch/3 allows you to have these exceptions bubble up past your catch block handlers)
:- meta_predicate((catchvv(0, ?, 0))).
:- meta_predicate((ccatch(0, ?, 0))).
:- export((ccatch/3,catchvv/3)).

bubbled_ex(block(_,_)).
bubbled_ex('$aborted').
bubbled_ex_check(E):- (\+ bubbled_ex(E)),!.
bubbled_ex_check(E):-throw(E).

ccatch(Goal,E,Recovery):- nonvar(E) -> catch(Goal,E,Recovery); % normal mode (the user knows what they want)
                         catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode

catchvv(Goal,E,Recovery):- catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode


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

:- dynamic(buggerDir/1).
:- abolish(buggerDir/1),prolog_load_context(directory,D),asserta(buggerDir(D)).
:- dynamic(buggerFile/1).
:- abolish(buggerFile/1),prolog_load_context(source,D),asserta(buggerFile(D)).

:- module_transparent(library_directory/1).

% hasLibrarySupport :- absolute_file_name('logicmoo_util_library.pl',File),exists_file(File).

throwNoLib:- trace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(library_directory), trace_or_throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

addLibraryDir :- buggerDir(Here),atom_concat(Here,'/..',UpOne), absolute_file_name(UpOne,AUpOne),asserta(library_directory(AUpOne)).

% if not has library suport, add this direcotry as a library directory
% :-not(hasLibrarySupport) -> addLibraryDir ; true .

% :-hasLibrarySupport->true;throwNoLib.




ib_multi_transparent33(MT):-multifile(MT),module_transparent(MT),dynamic_safe(MT).

dif_safe(Agent,Obj):- (var(Agent);var(Obj)),!.
dif_safe(Agent,Obj):- Agent\==Obj.

% hide Pred from tracing
to_m_f_arity_pi(M:Plain,M,F,A,PI):-!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(Term,M,F,A,PI):- strip_module(Term,M,Plain),Plain\==Term,!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(F/A,_M,F,A,PI):-functor_safe(PI,F,A),!.
to_m_f_arity_pi(PI,_M,F,A,PI):-functor_safe(PI,F,A).

with_preds((X,Y),M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call),with_preds(Y,M,F,A,PI,Call).
with_preds([X],M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call).
with_preds([X|Y],M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call),with_preds(Y,M,F,A,PI,Call).
with_preds(M:X,_M,F,A,PI,Call):-!, with_preds(X,M,F,A,PI,Call).
with_preds(X,M,F,A,PI,Call):-forall(to_m_f_arity_pi(X,M,F,A,PI),Call).

% moo_show_childs(_):-showHiddens,!.

:- export(moo_show_childs/1).
:- module_transparent(moo_show_childs/1).
moo_show_childs(X):- with_preds(X,_M,F,A,_PI,'$hide'(F/A)).

moo_show_childs(M,F,A):-functor_safe(MPred,F,A),moo_show_childs(M,F,A,MPred).
moo_show_childs(M,_,_, MPred):- not(predicate_property(_:MPred,imported_from(M))).
moo_show_childs(M,F,A,_MPred):- moo_trace_hidechilds(M,F,A,0,0).



% ===================================================================
% Substitution based on ==
% ===================================================================
% Usage: dbgsubst(+Fml,+X,+Sk,?FmlSk)

:- export(dbgsubst/4).
dbgsubst(A,B,C,A):- B==C,!.
dbgsubst(A,B,C,D):-var(A),!,ddmsg(dbgsubst(A,B,C,D)),dumpST,dtrace,dbgsubst0(A,B,C,D).
dbgsubst(A,B,C,D):-dbgsubst0(A,B,C,D).

dbgsubst0(A,B,C,D):- 
      catchvv(hotrace(nd_dbgsubst(A,B,C,D)),E,(dumpST,ddmsg(E:nd_dbgsubst(A,B,C,D)),fail)),!.
dbgsubst0(A,_B,_C,A).

nd_dbgsubst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_dbgsubst(  P, X,Sk, P1 ) :- functor_safe(P,_,N),nd_dbgsubst1( X, Sk, P, N, P1 ).

nd_dbgsubst1( _,  _, P, 0, P  ).
nd_dbgsubst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_dbgsubst2( X, Sk, Args, ArgS ),
            nd_dbgsubst2( X, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_dbgsubst2( _,  _, [], [] ).
nd_dbgsubst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( X, Sk, [A|As], [Ap|AS] ) :- nd_dbgsubst( A,X,Sk,Ap ),nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( _X, _Sk, L, L ).



:- meta_predicate(moo_hide_all(:)).
:- export(moo_hide_all/1).
moo_hide_all(_:X) :-
	var(X),
        throw(error(instantiation_error, _)).
moo_hide_all(_:[]) :- !.
moo_hide_all(M:(H,T)) :- !,
	moo_hide_all(M:H),
	moo_hide_all(M:T).
moo_hide_all(M:[H|T]) :- !,
	moo_hide_all(M:H),
	moo_hide_all(M:T).
moo_hide_all(W):-
   (W =  M:F/A -> functor_safe(P,F,A) ; true),
   (W =  M:P -> functor_safe(P,F,A) ; true),   
     (
     (atom(P) -> module_transparent(M:F/A) ;  true),
    ((A>0,arg(1,P,NV),nonvar(NV)) -> M:meta_predicate(P) ; true),
   moo_hide(M:F/A),noprofile(M:F/A),
   nospy(M:P)),!.
moo_hide_all(W):- throw(error(moo_hide_all(W), _)).

:- export(moo_hide_all/1).



%=========================================
% Module Utils
%=========================================
:- export(moo_hide_childs/1).
:- module_transparent(moo_hide_childs/1).
:- meta_predicate(moo_hide_childs(0)).

% module_functor(PredImpl,Module,Pred,Arity).

module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).
strip_arity(Pred/Arity,Pred,Arity).
strip_arity(PredImpl,Pred,Arity):-functor_safe(PredImpl,Pred,Arity).
% moo_hide_childs(+Pred).
:- export(moo_hide_childs/1).

moo_hide_childs(Preds):- with_preds(Preds,M,F,A,_PI,moo_hide_childs(M,F/A)).

moo_hide_childs(Preds):- with_preds(Preds,M,_F,_A,PI,moo_hide_childs_0(M,PI)).

moo_hide_childs(M,Preds):- with_preds(Preds,_M,_F,_A,PI,moo_hide_childs_0(M,PI)).

% predicate_property(do_pending_db_ops,PP)
% predicate_property_m(Pred,imported_from(M)):- predicate_property(Pred,imported_from(M)).
% predicate_property_m(Pred,imported_from(M)):-predicate_property(M:Pred,M).

moo_hide_childs_0(M,Pred):-
% predicate_property_m(Pred,M),
'$set_predicate_attribute'(M:Pred, trace, 1),
'$set_predicate_attribute'(M:Pred, noprofile, 1),
'$set_predicate_attribute'(M:Pred, hide_childs, 1),!.

moo_hide_childs_0(N,MPred):- predicate_property(MPred,imported_from(M)), writeq(wont_hide(M,N,MPred)),nl.


moo_hide_childs(M,F,A):-moo_trace_hidechilds(M,F,A,1,1).

moo_only_childs(M,F,A):-moo_trace_hidechilds(M,F,A,0,0).


moo_trace_hidechilds(M,F,A,Trace,HideChilds):-
  current_predicate(M:F/A),functor(P,F,A),
  set_trace_np_hc(M:P,Trace,1,HideChilds).

set_trace_np_hc(MP,Trace,NP,HideChilds):-
   '$set_predicate_attribute'(MP, trace, Trace),
   '$set_predicate_attribute'(MP, noprofile, NP),
   '$set_predicate_attribute'(MP, hide_childs, HideChilds),!.


/*

debug(+Topic, +Format, +Arguments)
Prints a message using format(Format, Arguments) if Topic unies with a topic
enabled with debug/1.
debug/nodebug(+Topic [>le])
Enables/disables messages for which Topic unies. If >le is added, the debug
messages are appended to the given le.
assertion(:Goal)
Assumes that Goal is true. Prints a stack-dump and traps to the debugger otherwise.
This facility is derived from the assert() macro as used in C, renamed
for obvious reasons.
*/
:- meta_predicate nodebugx(0).
:- meta_predicate with_preds(?,?,?,?,?,0).
:- meta_predicate with_skip_bugger(0).

:- meta_predicate one_must(0,0,0).

:- export(nop/1).
nop(_).
%set_prolog_flag(N,V):-!,nop(set_prolog_flag(N,V)).


% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- set_prolog_flag(generate_debug_info, true).
% have to load this module here so we dont take ownership of prolog_exception_hook/4.

% :- ensure_loaded(library(backcomp)).
:- ensure_loaded(library(ansi_term)).
:- ensure_loaded(library(check)).
:- ensure_loaded(library(debug)).
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(make)).
:- ensure_loaded(library(prolog_stack)).
:- ensure_loaded(library(system)).
:- ensure_loaded(library(apply)).





:- module_transparent(if_may_hide/1).
:- meta_predicate(if_may_hide(0)).
if_may_hide(_G):-!.
% if_may_hide(G):-G.

:- thread_local(thlocal:session_id/1).
:- multifile(thlocal:session_id/1).

:-export(moo_hide/1).
moo_hide(M):-if_may_hide(b_moo_hide(M)).
moo_hide(M,F,A):-if_may_hide(b_moo_hide(M,F,A)).

b_moo_hide(M,F,A):-
    functor(Pred,F,A),   
   '$set_predicate_attribute'(M:Pred, noprofile, 1),
   (A==0 -> '$set_predicate_attribute'(M:Pred, hide_childs, 1);'$set_predicate_attribute'(M:Pred, hide_childs, 1)),
   (A==0 -> '$set_predicate_attribute'(M:Pred, trace, 0);'$set_predicate_attribute'(M:Pred, trace, 1)).



b_moo_hide(M:F/A):-!,'$find_predicate'(F/A,List),forall(lists:member(M:F/A,List),moo_hide(M,F,A)).
b_moo_hide(P):-!,'$find_predicate'(P,List),forall(lists:member(M:F/A,List),moo_hide(M,F,A)).
b_moo_hide(F/A):-!,'$find_predicate'(F/A,List),forall(lists:member(M:F/A,List),moo_hide(M,F,A)).

:- meta_predicate(cnotrace(0)).
cnotrace(G):- once(G).
:- moo_hide(cnotrace/1).
:- if_may_hide('$set_predicate_attribute'(cnotrace(_), hide_childs, 1)).
:- if_may_hide('$set_predicate_attribute'(cnotrace(_), trace, 0)).


hotrace:-notrace.
:- export(hotrace/1).
:- meta_predicate(hotrace(0)).
% Unlike notrace/1, it allows traceing when excpetions are raised during Goal.


thread_leash(+Some):-!, (thread_self(main)->leash(+Some);thread_leash(-Some)).
thread_leash(-Some):-!, (thread_self(main)->leash(-Some);thread_leash(-Some)).
thread_leash(Some):-!, (thread_self(main)->leash(+Some);thread_leash(-Some)).

% ((tracing, notrace) -> CC=trace;CC=true), '$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,(('$leash'(_, Old),'$visible'(_, OldV),CC),CC)).
get_hotrace(X,Y):- tracing,!,'$visible'(OldV, OldV),notrace,visible(-all),visible(+exception),Y = call_cleanup(show_call(X),notrace(('$visible'(_, OldV),trace))).
get_hotrace(X,Y):- !,'$visible'(OldV, OldV),notrace,visible(-all),visible(+exception),Y = call_cleanup(X,('$visible'(_, OldV))).

hotrace(X):- notrace(get_hotrace(X,Y)),Y.
:- moo_hide(hotrace/1).
:- if_may_hide('$set_predicate_attribute'(hotrace(_), hide_childs, 1)).
:- if_may_hide('$set_predicate_attribute'(hotrace(_), trace, 0)).

%get_hotrace(X):-  visible(+exception),thread_leash(+exception),restore_trace((notrace,X)).
% hotrace(C):-current_predicate(logicmoo_bugger_loaded/0)->catchvv((C),E,((writeq(E=C),rtrace(C),trace_or_throw(E=C))));C.



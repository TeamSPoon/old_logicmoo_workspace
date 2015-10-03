/** <module> Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_first.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/

:-thread_local(no_slow_io).
% no_slow_io.



:-dynamic(always_show_dmsg).
always_show_dmsg:- thread_self(main).

:-export(tlbugger:ifHideTrace/0).
:-thread_local(tlbugger:ifHideTrace/0).



:-dynamic(is_hiding_dmsgs).
is_hiding_dmsgs:- \+always_show_dmsg, current_prolog_flag(opt_debug,false),!.
is_hiding_dmsgs:- \+always_show_dmsg, tlbugger:ifHideTrace,!.

% bugger_debug=false turns off just debugging about the debugger
% opt_debug=false turns off all the rest of debugging
% ddmsg(_):-current_prolog_flag(bugger_debug,false),!.
ddmsg(D):-format_to_error('~Ndmsg: ~q~n',[D]).
ddmsg_call(D):- ( (ddmsg(ddmsg_call(D)),call(D),ddmsg(ddmsg_exit(D))) *-> true ; ddmsg(ddmsg_failed(D))).

:-meta_predicate(to_pi(?,?)).
to_pi(P,M:P):-var(P),!,current_module(M).
to_pi(M:P,M:P):-var(P),!,current_module(M).
to_pi(Find,(M:PI)):- once(catch('$find_predicate'(Find,Found),_,fail)),Found=[_|_],!,member(M:F/A,Found),functor(PI,F,A).
to_pi(M:Find,M:PI):-!,current_module(M),to_pi0(M,Find,M:PI).
to_pi(Find,M:PI):-current_module(M),to_pi0(M,Find,M:PI).

to_pi0(M,Find,M:PI):- atom(Find),!,when(bound(PI),(nonvar(PI),functor(PI,Find,_))).
to_pi0(M,Find/A,M:PI):-var(Find),number(A),!,when(bound(PI),(nonvar(PI),functor(PI,_,A))).
to_pi0(M,Find,PI):-get_pi(Find,PI0),!,(PI0\=(_:_)->(current_module(M),PI=(M:PI0));PI=PI0).


:-thread_local(thlocal:last_src_loc/2).
input_key(K):-thread_self(K).
   
show_new_src_location(FL):-input_key(K),show_new_src_location(K,FL).

show_new_src_location(K,FL):- thlocal:last_src_loc(K,FL),!.
show_new_src_location(K,FL):- retractall(thlocal:last_src_loc(K,_)),format_to_error('~N% ~w ',[FL]),!,asserta(thlocal:last_src_loc(K,FL)).


show_source_location:- no_slow_io,!.
show_source_location:- is_hiding_dmsgs,!.
show_source_location:- current_predicate(logicmoo_util_varnames_file/0),current_source_location(FL),!,show_new_src_location(FL),!.
show_source_location.

as_clause_no_m( MHB,  H, B):- strip_module(MHB,_M,HB), as_clause( HB,  MH, MB),strip_module(MH,_M2H,H),strip_module(MB,_M2B,B).
as_clause_w_m(MHB, M, H, B):-  as_clause( MHB,  MH, MB),strip_module(MH,M1H,H),strip_module(MB,M2B,B), (M1H==user->M2B=M;M1H=M).

:-swi_export(is_ftCompound/1).
is_ftCompound(C):-compound(C),C\='$VAR'(_).

:-swi_export(is_ftVar/1).
is_ftVar(V):-var(V),!.
is_ftVar('$VAR'(_)).

:-swi_export(is_ftNonvar/1).
is_ftNonvar(V):- \+ is_ftVar(V).


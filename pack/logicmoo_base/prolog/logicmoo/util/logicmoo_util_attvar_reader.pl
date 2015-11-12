/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
:- module(logicmoo_util_attvar_reader, [
          expand_to_attvars/2,
          serialize_attvars/2,
          read_attvars/1,read_attvars/0,
          system_term_expansion_attvars/2,
          expand_to_attvars/3]).

:-set_prolog_flag(read_attvars,false).

%:- multifile(user:goal_expansion/2).
%:- dynamic(user:goal_expansion/2).
:- multifile(system:goal_expansion/2).
:- dynamic(system:goal_expansion/2).
:- multifile(system:term_expansion/2).
:- dynamic(system:term_expansion/2).

:- use_module(logicmoo_util_dmsg).


get_name_or_fail(V):-get_attr(V,vn,_),!.
get_name_or_fail(V):-var(V),b_getval('$variable_names',Vs),member(N=NV,Vs),V==NV,!,put_attr(V,vn,N).

expand_to_attvars(_,I,O):- expand_to_attvars(I,O).

expand_to_attvars( V,V):- var(V),ignore(get_name_or_fail(V)),!.
expand_to_attvars(IO,IO):- \+ compound(IO),!.
expand_to_attvars(avar(S),V):- nonvar(S),!, show_call(put_dyn_attrs(V,S)),ignore(get_name_or_fail(V)).
expand_to_attvars(avar(V,_),V):- nonvar(V),!.
expand_to_attvars(avar(V,S),V):- nonvar(S),!, show_call(put_dyn_attrs(V,S)),ignore(get_name_or_fail(V)).
expand_to_attvars('$VAR'(N),'$VAR'(N)):- \+ atom(N),!.
expand_to_attvars('$VAR'(N),V):- b_getval('$variable_names',Vs),member(N=V,Vs),!,put_attr(V,vn,N),!.
expand_to_attvars('$VAR'(N),V):- b_getval('$variable_names',Vs),put_variable_names([N=V|Vs]),!,put_attr(V,vn,N),!.
expand_to_attvars(C,A):- C=..[F|Args],maplist(expand_to_attvars,Args,OArgs),A=..[F|OArgs].


serialize_var(V,'$VAR'(Name)):- get_attrs(V, att(vn, Name, [])),!.
serialize_var(V,avar(V,S)):- get_attrs(V, S),!.
serialize_var(V,'$VAR'(N)):- b_getval('$variable_names',Vs),member(N=V0,Vs),V==V0,!.
serialize_var(V,V).

serialize_attvars(V,S):- var(V),serialize_var(V,S),!.
serialize_attvars(IO,IO):- \+ compound(IO),!.
serialize_attvars('$VAR'(N),'$VAR'(N)):- !.
serialize_attvars(avar(N),avar(N)):-!.
serialize_attvars(avar(N,A),avar(N,A)):-!.
serialize_attvars(C,A):- C=..[F|Args],maplist(serialize_attvars,Args,OArgs),A=..[F|OArgs].



put_dyn_attrs(_,S):-must(nonvar(S)),fail.
put_dyn_attrs(V,S):- S= att(_,_,_),!, put_attrs(V,S).
put_dyn_attrs(_V,[]):- !.
put_dyn_attrs(V,[H|T]):- !, put_dyn_attrs(V,H),put_dyn_attrs(V,T).
put_dyn_attrs(V,M=AV):- ensure_attr_setup(M),!, must(put_attr(V,M,AV)).
put_dyn_attrs(_V,MAV):- must(req(MAV)),!.

ensure_attr_setup(M):- current_predicate(M:attribute_goals/3),!.
ensure_attr_setup(M):- assert((M:attribute_goals(V,[put_attr(V,M,A)|R],R):- get_attr(V, M,A))).



read_attvars:- read_attvars(true).
read_attvars(TF):- set_prolog_flag(read_attvars,TF).


%% system_term_expansion_attvars( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%
system_term_expansion_attvars(T,O):- var(T),!,must(expand_to_attvars(T,T,O)),O\=@=T,!,debugm(xform(T --> O)).
system_term_expansion_attvars(T,_):- \+ compound(T),!,fail.
system_term_expansion_attvars('varname_info'(_,_,_,_),_):-!,fail.
system_term_expansion_attvars(M:T,M:I):-!,system_term_expansion_attvars(T,I).
system_term_expansion_attvars('$was_imported_kb_content$'(I,II),O):-!,serialize_attvars('$was_imported_kb_content$'(I,II),O),!.
system_term_expansion_attvars(T,O):- must(expand_to_attvars(T,T,O)),O\=@=T,!,debugm(xform(T --> O)).
%system_term_expansion_attvars(T,T,_):- debugm(sge(T)),fail.


%% system_goal_expansion_attvars( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%
system_goal_expansion_attvars(T,O):- var(T),!,must(expand_to_attvars(T,T,O)),O\=@=T,!,debugm(xform(T --> O)).
system_goal_expansion_attvars(T,_):- \+ compound(T),!,fail.
system_goal_expansion_attvars(M:T,M:I):-!,system_goal_expansion_attvars(T,I).
system_goal_expansion_attvars('varname_info'(_,_,_,_),_):-!,fail.
system_goal_expansion_attvars(T,O):-must(expand_to_attvars(T,T,O)),O\=@=T,!,debugm(xform(T --> O)).


system:term_expansion(I,O):- current_prolog_flag(read_attvars,true), system_term_expansion_attvars(I,O) -> I\=@=O.

system:goal_expansion(I,O):- current_prolog_flag(read_attvars,true), system_goal_expansion_attvars(I,O) -> I\=@=O.



:-set_prolog_flag(read_attvars,true).

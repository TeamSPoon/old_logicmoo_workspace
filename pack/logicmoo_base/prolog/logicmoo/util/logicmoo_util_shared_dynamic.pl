/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_structs.pl
:- module(logicmoo_util_shared_dynamic,
          [ 
             expand_to_attvars/2,
             serialize_attvars/2,
             wrap_shared/3, decl_shared/1,system_goal_expansion_sd/2 ,ereq/1, dbreq/1, system_term_expansion_sd/2,expand_to_attvars/3, is_user_module/0, read_skolems/1,read_skolems/0]).

:-set_prolog_flag(read_attvars,false).

:- dynamic(wrap_shared/3).

%:- multifile(user:goal_expansion/2).
%:- dynamic(user:goal_expansion/2).
:- multifile(system:goal_expansion/2).
:- dynamic(system:goal_expansion/2).
:- multifile(system:term_expansion/2).
:- dynamic(system:term_expansion/2).

:- use_module(logicmoo_util_dmsg).



:- dynamic(ereq/1).
:- module_transparent(ereq/1).
ereq(C):- find_and_call(C).

:- dynamic(dbreq/1).
:- module_transparent(dbreq/1).
dbreq(C):- ereq(C).

%% wrap_shared( ?VALUE1, :Wrapper, ?VALUE3) is semidet.
%
% Wrap Shared.
%
wrap_shared(isa,2,ereq).
wrap_shared(t,_,ereq).
%wrap_shared(call,_,ereq).
%wrap_shared(apply,_,ereq).

wrap_shared(assert,1,dbreq):- is_user_module.
wrap_shared(assert,2,dbreq):- is_user_module.
wrap_shared(asserta,1,dbreq):- is_user_module.
wrap_shared(asserta,2,dbreq):- is_user_module.
wrap_shared(assertz,1,dbreq):- is_user_module.
wrap_shared(assertz,2,dbreq):- is_user_module.
wrap_shared(clause,2,dbreq):- is_user_module.
wrap_shared(clause,3,dbreq):- is_user_module.
wrap_shared(retract,1,dbreq):- is_user_module.
wrap_shared(retractall,1,dbreq):- is_user_module.

is_user_module :- prolog_load_context(source,F), lmconf:mpred_is_impl_file(F),!,fail.
is_user_module :- prolog_load_context(module,M), module_property(M,class(L)),L=library,!,fail.
is_user_module :- prolog_load_context(module,user). 


read_skolems:- read_skolems(true).
read_skolems(TF):- set_prolog_flag(read_attvars,TF).



%% system_goal_expansion_sd( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%
system_goal_expansion_sd(T,O):-var(T),!,current_prolog_flag(read_attvars,true),must(expand_to_attvars(T,T,O)),O\=@=T,!,debugm(xform(T --> O)).
system_goal_expansion_sd(T,_):- \+ compound(T),!,fail.
system_goal_expansion_sd('varname_info'(_,_,_,_),_):-!,fail.
system_goal_expansion_sd(M:T,M:I):-compound(T),functor(T,F,A),wrap_shared(F,A,How),safe_wrap(T,How,I).
system_goal_expansion_sd(T,I):-compound(T),functor(T,F,A),wrap_shared(F,A,How),safe_wrap(T,How,I).
system_goal_expansion_sd(T,O):-current_prolog_flag(read_attvars,true),must(expand_to_attvars(T,T,O)),O\=@=T,!,debugm(xform(T --> O)).


system_term_expansion_sd(T,O):- var(T),!,current_prolog_flag(read_attvars,true),must(expand_to_attvars(T,T,O)),O\=@=T,!,debugm(xform(T --> O)).
system_term_expansion_sd(T,_):- \+ compound(T),!,fail.
system_term_expansion_sd('varname_info'(_,_,_,_),_):-!,fail.
system_term_expansion_sd(M:T,M:I):-!,system_term_expansion_sd(T,I).
system_term_expansion_sd('$was_imported_kb_content$'(I,II),O):-!,serialize_attvars('$was_imported_kb_content$'(I,II),O),!.
system_term_expansion_sd(T,O):- current_prolog_flag(read_attvars,true),must(expand_to_attvars(T,T,O)),O\=@=T,!,debugm(xform(T --> O)).
%system_term_expansion_sd(T,T,_):- debugm(sge(T)),fail.

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

%% safe_wrap( Term, +How, -Wrapped) is semidet.
%
% Safely Paying Attention To Corner Cases Wrap.
%
safe_wrap(I,_,if_defined(I)):- current_prolog_flag(xref,true),!,fail,numbervars(I).
safe_wrap(I,How,call(How,I)).




%% decl_shared( :TermM) is semidet.
%
% Declare Shared.
%
decl_shared((A,B)):-!,decl_shared(A),!,decl_shared(B),!.
decl_shared([A|B]):-!,decl_shared(A),!,decl_shared(B),!.
decl_shared([A]):-!,decl_shared(A),!.
decl_shared(F/A):-!,asserta_if_new(logicmoo_util_shared_dynamic:wrap_shared(F,A,ereq)).
decl_shared(M:F/A):-!,asserta_if_new(logicmoo_util_shared_dynamic:wrap_shared(F,A,M:ereq)).
decl_shared(M):-atom(M),!,asserta_if_new(logicmoo_util_shared_dynamic:wrap_shared(M,_,ereq)).

:- decl_shared(arity/2).
:- decl_shared(t).
:- decl_shared(meta_argtypes/1).




%% goal_expansion( ?LC, ?LCOO) is semidet.
%
% Hook To [system:goal_expansion/2] For Module Logicmoo_util_shared_dynamic.
% Goal Expansion.
%
system:goal_expansion(I,O):- % source_location(_,_), 
     system_goal_expansion_sd(I,O).

system:term_expansion(I,O):- % source_location(_,_), 
   system_term_expansion_sd(I,O).

user:term_expansion(I,O):- % source_location(_,_), 
   system_term_expansion_sd(I,O).


%:-set_prolog_flag(read_attvars,true).

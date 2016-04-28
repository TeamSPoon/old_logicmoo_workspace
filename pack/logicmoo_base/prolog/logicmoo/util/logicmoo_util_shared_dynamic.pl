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
          decl_shared/1,
          system_goal_expansion_safe_wrap/2,
          ereq/1,
          dbreq/1,
          is_user_module/0]).


:- dynamic(wsh_w:wrap_shared/3).

%:- multifile(user:goal_expansion/2).
%:- dynamic(user:goal_expansion/2).
:- multifile(system:goal_expansion/2).
:- dynamic(system:goal_expansion/2).
:- multifile(system:term_expansion/2).
:- dynamic(system:term_expansion/2).

:- use_module(logicmoo_util_dmsg).
:- use_module(logicmoo_util_rtrace).



:- dynamic(ereq/1).
:- module_transparent(ereq/1).
ereq(C):- find_and_call(C).

:- dynamic(dbreq/1).
:- module_transparent(dbreq/1).
dbreq(C):- ereq(C).

%% wsh_w:wrap_shared( ?VALUE1, :Wrapper, ?VALUE3) is semidet.
%
% Wrap Shared.
%

% wsh_w:wrap_shared(_,_,_):-!,fail.
wsh_w:wrap_shared(isa,2,ereq).
wsh_w:wrap_shared(t,_,ereq).
%wsh_w:wrap_shared(call,_,ereq).
%wsh_w:wrap_shared(apply,_,ereq).

wsh_w:wrap_shared(assert,1,dbreq):- is_user_module.
wsh_w:wrap_shared(assert,2,dbreq):- is_user_module.
wsh_w:wrap_shared(asserta,1,dbreq):- is_user_module.
wsh_w:wrap_shared(asserta,2,dbreq):- is_user_module.
wsh_w:wrap_shared(assertz,1,dbreq):- is_user_module.
wsh_w:wrap_shared(assertz,2,dbreq):- is_user_module.
wsh_w:wrap_shared(clause,2,dbreq):- is_user_module.
wsh_w:wrap_shared(clause,3,dbreq):- is_user_module.
wsh_w:wrap_shared(retract,1,dbreq):- is_user_module.
wsh_w:wrap_shared(retractall,1,dbreq):- is_user_module.

is_user_module :- prolog_load_context(source,F), lmconf:mpred_is_impl_file(F),!,fail.
is_user_module :- prolog_load_context(module,M), module_property(M,class(L)),L=library,!,fail.
is_user_module :- prolog_load_context(module,user). 


%% system_goal_expansion_safe_wrap( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%
system_goal_expansion_safe_wrap(T,_):- \+ compound(T),!,fail.
system_goal_expansion_safe_wrap(M:T,M:I):-!,compound(T),functor(T,F,A),wsh_w:wrap_shared(F,A,How),safe_wrap(T,How,I).
system_goal_expansion_safe_wrap(T,I):- functor(T,F,A),wsh_w:wrap_shared(F,A,How),safe_wrap(T,How,I).


%% safe_wrap( Term, +How, -Wrapped) is semidet.
%
% Safely Paying Attention To Corner Cases Wrap.
%
safe_wrap(I,_,if_defined(I)):- current_prolog_flag(xref,true),!,fail.
safe_wrap(I,How,call(How,I)).



%% decl_shared( :TermM) is semidet.
%
% Declare Shared.
%
decl_shared(Var):-var(Var),!,trace_or_throw(var_decl_shared(Var)).
decl_shared((A,B)):-!,decl_shared(A),!,decl_shared(B),!.
decl_shared([A|B]):-!,decl_shared(A),!,decl_shared(B),!.
decl_shared([A]):-!,decl_shared(A),!.

decl_shared(F/A):-atom(F),!,asserta_if_new(wsh_w:wrap_shared(F,A,ereq)),(integer(A)->ain(baseKB:arity(F,A));true),ain((baseKB:prologHybrid(F))).
decl_shared(M:F/A):-atom(F),!,asserta_if_new(wsh_w:wrap_shared(F,A,M:ereq)),ain(M:prologHybrid(F)),(integer(A)->ain(baseKB:arity(F,A));true).
decl_shared(M:P):-compound(P),!,functor(P,F,A),!,decl_shared(M:F/A).
decl_shared(F:P):-atom(P),!,decl_shared(F:P/_).
decl_shared(F):-atom(F),!,asserta_if_new(wsh_w:wrap_shared(F,_,ereq)),ain(prologHybrid(F)).

% loading_module 
%:- decl_shared(arity/2).
%:- decl_shared(t).
%:- decl_shared(meta_argtypes/1).



%% goal_expansion( ?LC, ?LCOO) is semidet.
%
% Hook To [system:goal_expansion/2] For Module Logicmoo_util_shared_dynamic.
% Goal Expansion.
%
system:goal_expansion(I,O):- % source_location(_,_), 
     hotrace(system_goal_expansion_safe_wrap(I,O)).


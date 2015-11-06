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
          [ wrap_shared/3, decl_shared/1,system_goal_expansion_sd/2 ]).

:- dynamic(wrap_shared/3).

%:- multifile(user:goal_expansion/2).
%:- dynamic(user:goal_expansion/2).
%user:goal_expansion(T,_):-dmsg(uge(T)),fail.
:- multifile(system:goal_expansion/2).
:- dynamic(system:goal_expansion/2).


% 	 	 
%% wrap_shared( ?VALUE1, :PRED2VALUE2, ?VALUE3) is semidet.
%
% Wrap Shared.
%
wrap_shared(isa,2,req).
wrap_shared(t,2,req).


% 	 	 
%% system_goal_expansion_sd( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%
system_goal_expansion_sd(T,_):-var(T),!,fail.
system_goal_expansion_sd(M:T,M:I):-!,system_goal_expansion_sd(T,I).
system_goal_expansion_sd(T,I):- functor(T,F,A),wrap_shared(F,A,How),safe_wrap(T,How,I).


% 	 	 
%% safe_wrap( ?I, ?VALUE2, ?I) is semidet.
%
% Safely Paying Attention To Corner Cases Wrap.
%
safe_wrap(I,_,if_defined(I)):- current_prolog_flag(xref,true),!,numbervars(I).
safe_wrap(I,How,call(How,I)).


% 	 	 
%% decl_shared( :TermM) is semidet.
%
% Declare Shared.
%
decl_shared((A,B)):-!,decl_shared(A),!,decl_shared(B),!.
decl_shared([A|B]):-!,decl_shared(A),!,decl_shared(B),!.
decl_shared([A]):-!,decl_shared(A),!.
decl_shared(F/A):-!,asserta_if_new(logicmoo_util_shared_dynamic:wrap_shared(F,A,req)).
decl_shared(M:F/A):-!,asserta_if_new(logicmoo_util_shared_dynamic:wrap_shared(F,A,M:req)).
decl_shared(M):-atom(M),!,asserta_if_new(logicmoo_util_shared_dynamic:wrap_shared(M,_,req)).

:- decl_shared(arity).
:- decl_shared(t).
:- decl_shared(meta_argtypes/1).


% 	 	 
%% goal_expansion( ?Math, ?MathGoal) is semidet.
%
% Hook To [system:goal_expansion/2] For Module Logicmoo_util_shared_dynamic.
% Goal Expansion.
%
system:goal_expansion(I,O):- source_location(_,_), system_goal_expansion_sd(I,O).


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
:- module(logicmoo_util_clause_expansion,
          [
          nb_current_or_nil/2,
          get_named_value_goal/2,
          is_fbe/3,
          is_user_module/0,
          expand_whatnot/3,
          expand_whatnot/5,
          do_ge/5,         
          call_whatnot_expansion/6]).

/** <module> Prolog compile-time and runtime source-code transformations

 This module specifies a set of more specialized term and goal expansions

as they are read from a file before they are processed by the compiler.

The toplevel is expand_clause/2.  This uses other translators:

	* Conditional compilation
	* clause_expansion/2 rules provided by the user

Note that this ordering implies  that conditional compilation directives
cannot be generated  by  clause_expansion/2   rules:  they  must literally
appear in the source-code.

*/

:- module_transparent((is_user_module/0,system_term_expansion/4,system_goal_expansion/4)).
:- use_module(logicmoo_util_dmsg).
:- use_module(logicmoo_util_rtrace).

:- multifile(system:goal_expansion/4).
:- dynamic(system:goal_expansion/4).
:- multifile(system:term_expansion/4).
:- dynamic(system:term_expansion/4).

:- meta_predicate get_named_value_goal(0,*).

:- multifile((clause_expansion/2,
              directive_expansion/2,
              body_expansion/2,
              sub_body_expansion/2,
              call_expansion/2,
              sub_call_expansion/2)).
:- dynamic((clause_expansion/2,
              directive_expansion/2,
              body_expansion/2,
              sub_body_expansion/2,
              call_expansion/2,
              sub_call_expansion/2)).
:- multifile((clause_expansion/4,
              directive_expansion/4,
              body_expansion/4,
              sub_body_expansion/4,
              call_expansion/4,
              sub_call_expansion/4)).
:- dynamic((clause_expansion/4,
              directive_expansion/4,
              body_expansion/4,
              sub_body_expansion/4,
              call_expansion/4,
              sub_call_expansion/4)).


:- multifile((system:clause_expansion/2,
              system:directive_expansion/2,
              system:body_expansion/2,
              system:sub_body_expansion/2,
              system:call_expansion/2,
              system:sub_call_expansion/2)).
:- dynamic((system:clause_expansion/2,
              system:directive_expansion/2,
              system:body_expansion/2,
              system:sub_body_expansion/2,
              system:call_expansion/2,
              system:sub_call_expansion/2)).
:- multifile((system:clause_expansion/4,
              system:directive_expansion/4,
              system:body_expansion/4,
              system:sub_body_expansion/4,
              system:call_expansion/4,
              system:sub_call_expansion/4)).
:- dynamic((system:clause_expansion/4,
              system:directive_expansion/4,
              system:body_expansion/4,
              system:sub_body_expansion/4,
              system:call_expansion/4,
              system:sub_call_expansion/4)).



is_user_module :- prolog_load_context(source,F), lmconf:mpred_is_impl_file(_,F),!,fail.
is_user_module :- prolog_load_context(module,user). 
is_user_module :- prolog_load_context(module,M), module_property(M,class(L)),L=library,!,fail.


get_named_value_goal(G,N=V):- functor(G,N,_), ((\+ \+ G )-> V=true; V=false).

get_pos_at_c(C,Num):-compound(C),arg(1,C,Num),number(Num).


is_fbe(term,I,PosI):-!,
   compound(PosI),nonvar(I),
   nb_current_or_nil('$term',Was), Was==I,
   nb_current_or_nil('$term_position', Pos),
   get_pos_at_c(Pos,PosAt),
   get_pos_at_c(PosI,At),!,
   PosAt>0,!,At>=PosAt.

is_fbe(goal,I,PosI):-!,
   compound(PosI),nonvar(I),
   nb_current_or_nil('$term',Was), Was==[],
   nb_current_or_nil('$term_position', Pos),
   get_pos_at_c(Pos,PosAt),
   get_pos_at_c(PosI,At),!,
   PosAt>0,!,At>=PosAt.



system_term_expansion(I,_P,_O,_P2):- var(I),!,fail.
system_term_expansion(I,P,_O,_P2):- \+ is_fbe(term,I,P),!,fail.
system_term_expansion((:- B),P,O,P2):- expand_whatnot(directive_expansion,(:- B),P,O,P2).
% system_term_expansion((:- B),P,O,P2):-!, expand_whatnot(call_expansion, B,P,O,P2).
system_term_expansion(I,_P,_O,_P2):- nb_setval('$term_e',I),fail.
system_term_expansion((H ),P,O,P2):- expand_whatnot(clause_expansion,H ,P,O,P2).
system_term_expansion((H :- I),P,(H :- O),P2):- expand_whatnot(body_expansion,I,P,O,P2).

% system_goal_expansion(I,P,O,P2):- var(I),!,fail.
sub_positional(P):- compound(P),functor(P,F,A),arg(A,P,[L|_]),compound(L),functor(L,F,_).

positional_seg(term_position(G2787,_,G2787,_,[_-_])).

nb_current_or_nil(N,V):-nb_current(N,V)->true;V=[].

system_goal_expansion(I,P,O,P2):- 
  nb_current_or_nil('$term',Was),
  get_named_value_goal(is_fbe(term,I,P),L1),
  get_named_value_goal(Was=@=I,L2),
  get_named_value_goal(sub_positional(P),L3),
  get_named_value_goal(positional_seg(P),L4),
  nb_current_or_nil('$term_e',TermWas), 
  get_named_value_goal(\+ (TermWas =@= Was), L5),
  do_ge([L1,L2,L3,L4,L5],I,P,O,P2),ignore(I=O),!,ignore(P2=P).

 do_ge([is_fbe=false,(=@=)=false,sub_positional=false,positional_seg=true,(\+)=true], I,P,O,P2):-!, 
   expand_whatnot(sub_call_expansion,I,P,O,P2).
 do_ge(_, I,P,O,P2):- !, expand_whatnot(sub_body_expansion,I,P,O,P2).
 do_ge(Why, I,P,I,P):- dmsg(do_ge(Why,I)),fail.



system:clause_expansion(I,O):- current_prolog_flag(show_expanders,true), dmsg(expand_clause(I,O)),fail.
system:directive_expansion(I,O):-  current_prolog_flag(show_expanders,true),dmsg(directive_expansion(I,O)),fail.
system:body_expansion(I,O):- current_prolog_flag(show_expanders,true),dmsg(body_expansion(I,O)),fail.
system:sub_body_expansion(I,O):- current_prolog_flag(show_expanders,true),dmsg(sub_body_expansion(I,O)),fail.
system:call_expansion(I,O):- current_prolog_flag(show_expanders,true),dmsg(call_expansion(I,O)),fail.
system:sub_call_expansion(I,O):- current_prolog_flag(show_expanders,true),dmsg(sub_call_expansion(I,O)),fail.


:- meta_predicate
    expand_whatnot(:, +, -).
    expand_whatnot(:, 4, +, ?, -, -).


%%	expand_whatnot(MM:TE,+Input, -Output) is det.
%%	expand_whatnot(MM:TE,+Input, +Pos0, -Output, -Pos) is det.
%
%	This predicate is used to translate clauses  as they are read from
%	a source-file before they are added to the Prolog database.

expand_whatnot(MM:TE,Clause0, Clause) :-
	expand_whatnot(MM:TE,Clause0, _, Clause, _).

expand_whatnot(_MM_TE,Var, Pos, Expanded, Pos) :- var(Var), !, Expanded = Var.
expand_whatnot(_MM_TE,Clause, Pos0, [], Pos) :-
	'$expand':cond_compilation(Clause, X),
	X == [], !,
	'$expand':atomic_pos(Pos0, Pos).
expand_whatnot(MM:TE,Clause, Pos0, Expanded, Pos) :-
	b_setval('$clause', Clause),
	'$def_modules'([TE/4,TE/2], MList),
	call_whatnot_expansion(MM:TE,MList, Clause, Pos0, Expanded, Pos),
	b_setval('$clause', []).

call_whatnot_expansion(_MM_TE,[], Clause, Pos, Clause, Pos).
call_whatnot_expansion(MM:TE,[M-Preds|T], Clause0, Pos0, Clause, Pos) :-
	(   '$member'(Pred, Preds),
	    (	Pred == TE/2
	    ->	call(M:TE,Clause0, Clause1),
		Pos1 = Pos0
	    ;	call(M:TE,Clause0, Pos0, Clause1, Pos1)
	    )
	->  '$expand':expand_terms(call_whatnot_expansion(MM:TE,T), Clause1, Pos1, Clause, Pos)
	;   call_whatnot_expansion(MM:TE,T, Clause0, Pos0, Clause, Pos)
	).




% system:term_expansion(I,P,O,P2):- fail, get_named_value_goal(is_fbe(term,I,P)),dmsg(te4(I,P,O,P2)),fail.
system:term_expansion(I,P,O,P2):- current_prolog_flag(lm_expanders,true), system_term_expansion(I,P,O,P2)->I\=@=O.
system:goal_expansion(I,P,O,P2):- current_prolog_flag(lm_expanders,true), system_goal_expansion(I,P,O,P2)->I\=@=O.



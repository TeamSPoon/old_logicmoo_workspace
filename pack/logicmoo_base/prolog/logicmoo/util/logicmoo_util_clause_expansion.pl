/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
% ===================================================================
% File 'logicmoo_util_clause_expansion.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_clause_expansion.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2016/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_clause_expansion.pl
:- module(lmce,
          [

          swi_module/2,
          show_module_imports/0,
          show_module_imports/1,
          show_module_imports/2,
          is_file_based_expansion/5,
          push_modules/0,
          reset_modules/0,
          current_smt/2,
          pop_modules/0,
          maybe_add_import_module/3,
          maybe_add_import_module/2,
          maybe_delete_import_module/2,
          add_prolog_predicate/6,
          glean_prolog_impl_file/4,
          add_genlMt/2,
          without_lm_expanders/1,

          nb_current_or_nil/2,
          get_named_value_goal/2,
          is_fbe/3,
          is_user_module/0,
          
          system_term_expansion/5,
          system_goal_expansion/5,
          expand_whatnot/6,
          preds_visible/3,
          do_ge/6,
          call_whatnot_expansion/7]).

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
:- if( \+ current_predicate(system:setup_call_cleanup_each/3)).
:- use_module(system:library('logicmoo/util/logicmoo_util_supp.pl')).
:- endif.

:- multifile((system:clause_expansion/2,
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
:- dynamic((system:clause_expansion/2,
              system:directive_expansion/2,
              system:body_expansion/2,
              system:sub_body_expansion/2,
              system:call_expansion/2,
              system:sub_call_expansion/2)).

:- module_transparent((system:clause_expansion/4,
              system:directive_expansion/4,
              system:body_expansion/4,
              system:sub_body_expansion/4,
              system:call_expansion/4,
              system:sub_call_expansion/4)).
:- module_transparent((system:clause_expansion/2,
              system:directive_expansion/2,
              system:body_expansion/2,
              system:sub_body_expansion/2,
              system:call_expansion/2,
              system:sub_call_expansion/2)).


:- module_transparent((is_user_module/0,without_lm_expanders/1,system_term_expansion/5,system_goal_expansion/5,functor_non_colon/3)).
:- use_module(logicmoo_util_dmsg).
:- use_module(logicmoo_util_rtrace).

:- multifile(system:goal_expansion/4).
:- dynamic(system:goal_expansion/4).
:- multifile(system:term_expansion/4).
:- dynamic(system:term_expansion/4).

:- meta_predicate get_named_value_goal(0,*).



is_user_module :- prolog_load_context(source,F), lmconf:mpred_is_impl_file(_,F),!,fail.
is_user_module :- prolog_load_context(module,user). 
is_user_module :- prolog_load_context(module,M), module_property(M,class(L)),L=library,!,fail.


get_named_value_goal(G,N=V):- functor_non_colon(G,N,_), ((\+ \+ G )-> V=true; V=false).

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

functor_non_colon(G,F,A):- compound(G), functor(G,':',2),arg(2,G,GG),!,functor_non_colon(GG,F,A).
functor_non_colon(G,F,A):- functor(G,F,A).

system_term_expansion(_,I,_P,_O,_P2):-  notrace( var(I) ),!,fail.

system_term_expansion(Mod,end_of_file,P,O,P2):-  !, expand_whatnot(Mod,clause_expansion,end_of_file,P,O,P2).
system_term_expansion(_,I,P,_O,_P2):- \+ is_fbe(term,I,P),!,fail.

system_term_expansion(Mod,(:- B),P,O,P2):- !, expand_whatnot(Mod,directive_expansion,(:- B),P,O,P2).
system_term_expansion(_,I,_P,_O,_P2):- nb_setval('$term_e',I),fail.
system_term_expansion(Mod,(H ),P,O,P2):- expand_whatnot(Mod,clause_expansion,H ,P,O,P2).
% system_term_expansion(Mod,(H :- I),P,(H :- O),P2):- expand_whatnot(Mod,body_expansion,I,P,O,P2).

% system_goal_expansion(I,P,O,P2):- var(I),!,fail.
sub_positional(P):- compound(P),functor(P,F,A),arg(A,P,[L|_]),compound(L),functor(L,F,_).

positional_seg(term_position(G2787,_,G2787,_,[_-_])).

nb_current_or_nil(N,V):-nb_current(N,V)->true;V=[].

system_goal_expansion(Mod,I,P,O,P2):- 
  notrace((nb_current_or_nil('$term',Was),
  get_named_value_goal(is_fbe(term,I,P),L1),
  get_named_value_goal(Was=@=I,L2),
  get_named_value_goal(sub_positional(P),L3),
  get_named_value_goal(positional_seg(P),L4),
  nb_current_or_nil('$term_e',TermWas), 
  get_named_value_goal(\+ (TermWas =@= Was), L5))),
  do_ge(Mod,[L1,L2,L3,L4,L5],I,P,O,P2),ignore(I=O),I\==O.


do_ge(_, _, TRUE):-TRUE==true,!.
do_ge(Mod, [is_fbe=false,  (=@=)=false, sub_positional=true, positional_seg=false,  (\+)=false], I,P,O,P2):-!, 
     expand_whatnot(Mod,body_expansion,I,P,O,P2).
do_ge(Mod, [is_fbe=false,  (=@=)=false, sub_positional=false, positional_seg=false,  (\+)=false], I,P,O,P2):-!, 
     expand_whatnot(Mod,sub_body_expansion,I,P,O,P2).
do_ge(Mod, [is_fbe=false,  (=@=)=false, sub_positional=true, positional_seg=false,  (\+)=true], I,P,O,P2):-!, 
  ((expand_whatnot(Mod,body_expansion,I,ce1(P),O,P2),I\==O);
   expand_whatnot(Mod,call_expansion,I,ce1(P),O,P2)).


do_ge(Mod, [is_fbe=false,  (=@=)=false, sub_positional=false, positional_seg=true,  (\+)=true],I,P,O,P2):- nonvar(P), !, fail,
   expand_whatnot(Mod,call_expansion,I,ce2(P),O,P2).

do_ge(Mod, [is_fbe=false,  (=@=)=false, sub_positional=false, positional_seg=true,  (\+)=true], I,P,O,P2):- var(P), !, 
   expand_whatnot(Mod,sub_call_expansion,I,P,O,P2).
do_ge(_, [is_fbe=false,  (=@=)=false, sub_positional=false, positional_seg=true,  (\+)=false], I,P,_O,_P2):- nonvar(P),!,
   nop(wdmsg(repressed(I=P))), fail.

do_ge(Mod, [is_fbe=false,  (=@=)=false, sub_positional=false, positional_seg=false,  (\+)=true], I,P,O,P2):- nonvar(P), !, 
  ((expand_whatnot(Mod,body_expansion,I,ce3(P),O,P2),I\==O);
   expand_whatnot(Mod,call_expansion,I,ce3(P),O,P2)).
 
% do_ge(Mod,Why, I,P,I,P):- dmsg(do_ge(Mod,Why,I,P)),fail.



system:clause_expansion(I,O):- current_prolog_flag(show_expanders,true), dmsg(expand_clause(I,O)),fail.
system:directive_expansion(I,O):-  current_prolog_flag(show_expanders,true),dmsg(directive_expansion(I,O)),fail.
system:body_expansion(I,O):- current_prolog_flag(show_expanders,true),dmsg(body_expansion(I,O)),fail.
system:sub_body_expansion(I,O):- current_prolog_flag(show_expanders,true),dmsg(sub_body_expansion(I,O)),fail.
system:call_expansion(I,O):- current_prolog_flag(show_expanders,true),dmsg(call_expansion(I,O)),fail.
system:sub_call_expansion(I,O):- current_prolog_flag(show_expanders,true),dmsg(sub_call_expansion(I,O)),fail.


preds_visible(_Mod,[], []).
preds_visible(Mod,[F/A|List], MList):-
 findall(MVis-F/A,
  ((default_module(Mod,MVis),
  functor(P,F,A),
  predicate_property(MVis:P,defined),
  \+ predicate_property(MVis:P,imported_from(_)))),
 MEList),
 preds_visible(Mod,List, NextMEList),
 append(MEList,NextMEList,MList).






:- meta_predicate expand_whatnot(+, 4, +, +, -, -).


%%	expand_whatnot(+Mod, +TE, +Input, +Pos0, -Output, -Pos) is det.
%
%	This predicate is used to translate clauses  as they are read from
%	a source-file before they are added to the Prolog database.

expand_whatnot(Mod,MMTE,Clause, Pos0, Expanded, Pos) :-
        strip_module(MMTE,_,TE),
        Mod:'$def_modules'([TE/4,TE/2], MList),
        MList\==[],
	call_whatnot_expansion(Mod, MMTE, MList, Clause, Pos0, Expanded, Pos).

call_whatnot_expansion(_Mod,_MM_TE,[], Clause, Pos, Clause, Pos):-!.
call_whatnot_expansion(Mod,MMTE,[M-Preds|TList], Clause0, Pos0, Clause, Pos) :-
      strip_module(MMTE,_,TE),
	(   '$member'(Pred, Preds),
	    (	Pred == TE/2
	    ->	(Mod:call(M:TE,Clause0, Clause1),
		Pos1 = Pos0)
	    ;	Mod:call(M:TE,Clause0, Pos0, Clause1, Pos1)
	    )
	->  '$expand':expand_terms(call_whatnot_expansion(Mod,MMTE,TList), Clause1, Pos1, Clause, Pos)
	;   call_whatnot_expansion(Mod, MMTE, TList, Clause0, Pos0, Clause, Pos)
	).




:- system:multifile(lmconf:source_typein_modules/3),
   system:dynamic(lmconf:source_typein_modules/3).

:- multifile(lmconf:mpred_is_impl_file/2).
:- dynamic(lmconf:mpred_is_impl_file/2).


current_smt(SM,M):-
 '$current_source_module'(SM),'$current_typein_module'(M).

push_modules:- current_smt(SM,M),
  prolog_load_context(source,F),
  system:asserta(lmconf:source_typein_modules(SM,M,F)).

reset_modules:- 
  prolog_load_context(source,F),
  once(lmconf:source_typein_modules(SM,M,F)),
  '$set_source_module'(SM),'$set_typein_module'(M),!.

pop_modules:- 
  prolog_load_context(source,F),
  once(system:retract(lmconf:source_typein_modules(SM,M,F))),
  '$set_source_module'(SM),'$set_typein_module'(M),!.


maybe_add_import_module(A,B):-maybe_add_import_module(A,B,start).

%TODO
maybe_add_import_module(_From,_To,_):- !.

maybe_add_import_module(A,baseKB,C):-!,maybe_add_import_module(baseKB,A,C).
maybe_add_import_module(From,To,_):- default_module(From,To),!.
maybe_add_import_module(user,_,start):-!.
maybe_add_import_module(From,To,Start):-  
   maybe_delete_import_module(To,From),
   catch(add_import_module(From,To,Start),E,writeln(E=add_import_module(From,To,Start))).



maybe_delete_import_module(_From,To):- To = user,!.
maybe_delete_import_module(_From,To):- To = system,!.

%TODO
maybe_delete_import_module(_From,_To):- !.

maybe_delete_import_module(From,To):- To = user,!,
    catch(add_import_module(From,system,end),E,writeln(E=add_import_module(From,system,end))),
   ignore(catch(system:delete_import_module(From,user),E,writeln(E=delete_import_module(To,From)))).
   
maybe_delete_import_module(To,From):-  ignore(catch(system:delete_import_module(To,From),E,writeln(E=delete_import_module(To,From)))).


% ===========================================================================
% add_prolog_predicate/6,glean_prolog_impl_file/2,complete_prolog_impl_file/2
% ===========================================================================

:- multifile(lmconf:known_prolog_file_prop/2).
:- dynamic(lmconf:known_prolog_file_prop/2).

add_genlMt(_,_):- \+ current_prolog_flag(logicmoo_glean,true),!.
add_genlMt(From,Prop):-atom(Prop),!,add_genlMt(From,imports(Prop)).
add_genlMt(From,CTo):-arg(1,CTo,To),From==To,!.
add_genlMt(From,imports(To)):- (arg(_,v(user,system),From);arg(_,v(user,system),To)),!.
add_genlMt(From,maybe(To)):- (arg(_,v(user,system),From);arg(_,v(user,system),To)),!.
add_genlMt(baseKB,imports(logicmoo_user)):-!. % this means never will happen

add_genlMt(lmcode,imports(baseKB)):- !, add_genlMt(baseKB,imports(lmcode)).
add_genlMt(lmcode,imports(baseKB)):-!.

% add_genlMt(_From,imports(To)):-arg(_,v(baseKB,logicmoo_user),To),!.
add_genlMt(From,Prop):-lmconf:known_prolog_file_prop(From,Prop),!.
add_genlMt(From,Prop):-assertz(lmconf:known_prolog_file_prop(From,Prop)),fail.
add_genlMt(_,file(_)):-!.
add_genlMt(_,uses(_)):-!.
add_genlMt(From,Prop):-write('% '), writeln(add_genlMt(From,Prop)),fail.

add_genlMt(From,imports(To)):-
   catch(add_import_module(From,To,start),E,writeln(E=add_import_module(From,To))).


:- meta_predicate
        glean_prolog_impl_file(+,+,+,+).

:- export(glean_prolog_impl_file/4).
:- module_transparent(glean_prolog_impl_file/4).

swi_module(M,Preds):- forall(member(P,Preds),M:export(P)). % ,dmsg(swi_module(M)).

is_file_based_expansion(term,I,PosI,_O,_PosO):-!,
   compound(PosI),nonvar(I),
   nb_current('$term',Was), Was==I,
   nb_current('$term_position', Pos),
   get_pos_at(Pos,PosAt),
   get_pos_at(PosI,At),!,
   PosAt>0,!,At>=PosAt.

is_file_based_expansion(goal,I,PosI,_O,_PosO):-!,
   compound(PosI),nonvar(I),
   %nb_current('$term',Was), Was\=[],Was=(:- _),
   nb_current('$term_position', Pos),
   get_pos_at(Pos,PosAt),
   get_pos_at(PosI,At),!,
   PosAt>0,!,At>=PosAt.

get_pos_at(C,Num):-compound(C),arg(1,C,Num),number(Num).

:- dynamic(lmconf:known_complete_prolog_impl_file/3).
glean_prolog_impl_file(_,_,_,_):- current_prolog_flag(xref,true),!.
glean_prolog_impl_file(_,_,_,_):- \+ source_location(_,_),!.

glean_prolog_impl_file(end_of_file,File,SM,TypeIn):-lmconf:known_complete_prolog_impl_file(SM,File,TypeIn),!.
glean_prolog_impl_file(end_of_file,File,SM,TypeIn):- atom(File),\+ atomic_list_concat([_,_|_],'.pfc',File),!,
   assertz(lmconf:known_complete_prolog_impl_file(SM,File,TypeIn)),
   all_source_file_predicates_are_transparent,
  % add_genlMt(logicmoo_user,imports(baseKB)),
  % add_genlMt(SM,maybe(TypeIn)),
  add_genlMt(lmcode,imports(SM)),
  % add_genlMt(SM,imports(logicmoo_user)),
  % add_genlMt(SM,imports(baseKB)),
   forall(source_file(M:H,File),
       ignore((functor(H,F,A),
         (predicate_property(M:H,imported_from(Where))
           -> add_prolog_predicate(SM,Where,H,F,A,File)
          ; add_prolog_predicate(TypeIn,M,H,F,A,File))))),
         fail.

glean_prolog_impl_file((:- module(Want,_PubList)),File,SM,TypeIn):-!,
    add_genlMt(TypeIn, uses(SM)),
    add_genlMt(lmcode, uses(SM)),
    % add_genlMt(baseKB, imports(SM)),
    add_genlMt(baseKB, imports(lmcode)),
    % add_genlMt(SM,imports(lmcode)),    
    add_genlMt(lmcode, uses(Want)),
    add_genlMt(SM, uses(Want)),
    add_genlMt(Want, file(File)).

glean_prolog_impl_file(_,File,SM,_TypeIn):-
   add_genlMt(SM,  file(File)),
   add_genlMt(SM, imports(logicmoo_user)).


:- export(add_prolog_predicate/6).
:- module_transparent(add_prolog_predicate/6).
add_prolog_predicate(skip,_M,_H,_F,_A,_S):-!.
add_prolog_predicate(_ImportTo,M,H,F,A,_S):-
  ignore((
       F\=='$mode',
       F\=='$pldoc',
       F\=='$exported_op',
       ignore(((\+ atom_concat('$',_,F),export(M:F/A)))),
       \+ predicate_property(M:H,transparent),
       M:module_transparent(M:F/A))).
      

show_module_imports(M):- show_module_imports(M,_),
  ((import_module(M,user);M=user)->true;portray_clause(':-'(ignore(system:delete_import_module(M,user))))),
  show_module_imports(_,M).

show_module_imports(M,I):-var(M),!,forall(current_module(M),show_module_imports(M,I)).
show_module_imports(M,I):-
   forall(import_module(M,I),
      portray_clause(':-'(system:add_import_module(M,I)))),
   
   forall((default_module(M,I),M\==I,\+import_module(M,I)),nop(wdmsg(default_module(M,I)))).

show_module_imports:-
  forall(current_module(M),show_module_imports(M)).
  

:- module_transparent(all_source_file_predicates_are_transparent/0).

%% all_source_file_predicates_are_transparent() is det.
%
% All Module Predicates Are Transparent.
%
all_source_file_predicates_are_transparent:-
   prolog_load_context(source,File),
    dmsg(all_source_file_predicates_are_transparent(File)),
    forall((source_file(ModuleName:P,File),functor(P,F,A)),
      ignore((
            \+ (predicate_property(ModuleName:P,(transparent))),
                   % ( nop(dmsg(todo(module_transparent(ModuleName:F/A))))),
                   (module_transparent(ModuleName:F/A))))).

:- meta_predicate without_lm_expanders(0).

without_lm_expanders(Goal):- current_prolog_flag(lm_expanders,false),!,call(Goal).
without_lm_expanders(Goal):-
  setup_call_cleanup_each(set_prolog_flag(lm_expanders,false),Goal,set_prolog_flag(lm_expanders,true)).


:- all_source_file_predicates_are_transparent.

:- module_transparent(user:term_expansion/1).
% system:term_expansion(I,P,O,P2):- fail, get_named_value_goal(is_fbe(term,I,P)),dmsg(te4(I,P,O,P2)),fail.
system:goal_expansion(I,P,O,P2):- current_prolog_flag(lm_expanders,true),prolog_load_context(module,Mod),
   without_lm_expanders((system_goal_expansion(Mod,I,P,O,P2)->(ignore(I=O),I\=@=O))).

system:term_expansion(EOF,POS,O,POS2):- 
 current_prolog_flag(lm_expanders,true),
 is_file_based_expansion(term,EOF,POS,O,POS2),
 nonvar(EOF),
 (EOF=end_of_file;EOF=(:-(module(_,_)))),
 prolog_load_context(module,M),
 M\==user, 
 ignore((    
    source_location(S,_),
    '$current_typein_module'(TM),
     glean_prolog_impl_file(EOF,S,M,TM))),fail.

system:term_expansion(I,P,O,P2):- current_prolog_flag(lm_expanders,true), prolog_load_context(module,Mod), 
   without_lm_expanders((system_term_expansion(Mod,I,P,O,P2)->(ignore(I=O),I\=@=O))).


/** <module> 
% This module defines the module cols that we use:
% utility,planner,parser,action,database,effects,spawning_loading,connection
% It is the basic module system of the Logicmoo MUD
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:-swi_module(moo,[]).

:-export(((current_context_module/1,
    term_expansion_local/2,
         register_module_type/1,          
         end_module_type/1,
         register_timer_thread/3))).

:-  op(1120,fx,export),op(1120,fx,dynamic_multifile_exported).

:-dynamic_multifile_exported(hasInstance/2).
:-dynamic_multifile_exported(mudSubclass/2).

:-multifile(user:mudIsa/2).
:-dynamic(user:mudIsa/2).
/*
:- '@'(ensure_loaded('../src_lib/logicmoo_util/logicmoo_util_all'),user).
:- ensure_loaded(logicmoo('dbase/dbase_i_rdf_store.pl')).
*/

:- include(logicmoo(vworld/moo_header)).


mudIsa_motel(tCol,tCol).
mudIsa_motel(I,T):-no_repeats_av(deduce_M(mudIsa(I,T))),I\=tCol,I\==isTDisjoint(tBOT),I\==tTOP,T\==isTDisjoint(tBOT),T\==tTOP.

user:hasInstance_dyn(tCol,ttCompleteExtentAsserted).
user:hasInstance_dyn(tCol,tCol).
user:hasInstance_dyn(tCol,tChannel).
user:hasInstance_dyn(ttCompleteExtentAsserted,ttCompleteExtentAsserted).
user:hasInstance_dyn(ttCompleteExtentAsserted,prologSingleValued).
user:hasInstance_dyn(ttCompleteExtentAsserted,tCol).
user:hasInstance_dyn(ttCompleteExtentAsserted,ttFormatType).

user:hasInstance_dyn(ttCompleteExtentAsserted,ttValueType).
user:hasInstance_dyn(ttCompleteExtentAsserted,ttSpatialType).
user:hasInstance_dyn(ttCompleteExtentAsserted,tRelation).
user:hasInstance_dyn(ttCompleteExtentAsserted,tPred).

user:hasInstance_dyn(ttCompleteExtentAsserted,mudFtInfo).
user:hasInstance_dyn(ttCompleteExtentAsserted,genlPreds).

% CANT user:hasInstance_dyn(ttCompleteExtentAsserted,tRegion).
%user:hasInstance_dyn(ttNotSpatialType,ftInt).
%user:hasInstance_dyn(ttNotSpatialType,ftTerm).
user:hasInstance_dyn(ttNotSpatialType,tCol).
user:hasInstance_dyn(ttNotSpatialType,ttFormatType).
user:hasInstance_dyn(ttNotSpatialType,ttValueType).

user:hasInstance_dyn(ttSpatialType,tAgentGeneric).
user:hasInstance_dyn(ttSpatialType,tItem).
user:hasInstance_dyn(ttSpatialType,tObj).
user:hasInstance_dyn(ttSpatialType,tRegion).
user:hasInstance_dyn(ttSpatialType,tSpatialThing).


hasInstance(T,I):- not(current_predicate(deduce_M/1)),!,user:hasInstance_dyn(T,I).
hasInstance(T,I):- !, (mudIsa_motel(I,T) *-> true ; (((atom(I),must(not(user:hasInstance_dyn(T,I)))),fail))).
hasInstance(T,I):- rdf_x(I,rdf:type,T).

assert_hasInstance(T,I):- !, assert_hasInstance_real(T,I).
assert_hasInstance(T,I):- loop_check(hooked_asserta(mudIsa(I,T)),assert_hasInstance_real(T,I)).
assert_hasInstance(T,I):- rdf_assert_x(I,rdf:type,T).

assert_hasInstance_real(T,I):- user:hasInstance_dyn(T,I),!.
assert_hasInstance_real(T,I):- expire_tabled_list(all), not(current_predicate(assert_ind/2)),!,assert_if_new(user:hasInstance_dyn(T,I)),!.
assert_hasInstance_real(T,I):- assert_if_new(user:hasInstance_dyn(T,I)),!,(atom(I)->must(assert_ind(I,T));true),!.

mpred_prop(determinerString, prologMultiValued).
mpred_prop(mudActAffect, prologMultiValued).
mpred_prop(member,prologOnly).
mpred_prop(mpred_prop,prologOnly).
mpred_prop(mpred_arity,prologOnly).
mpred_prop(never_type,prologOnly).
mpred_prop(argIsa,prologHybrid).
mpred_prop(dbase_t, prologOnly).
mpred_prop(F,prologOnly):- not(mpred_prop(F,prologHybrid)),(F=is_pred_declarer;(current_predicate(F/1);not(hasInstance(F,tCol)))).
mpred_prop(term_expansion,prologOnly).
mpred_prop(agent_text_command,prologOnly).


mpred_arity(xyzFn,4).
mpred_arity(mpred_arity,2).
mpred_arity(mpred_prop,2).
mpred_arity(never_type,1).
mpred_arity(self_call,1).
mpred_arity(argIsa, 3).
mpred_arity(mudIsa, 2).

:-dynamic_multifile_exported(is_stable/0).
is_stable:-fail.

fast_mud.

xperimental:-fail.
xperimental_big_data:-fail.
:-dynamic_multifile_exported(is_release/0).
is_release :- fail,1 is random(3).
:-dynamic_multifile_exported(not_is_release/0).
not_is_release :- true. % 1 is random(3).
simple_code :- fail.
save_in_dbase_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

:- '@'((ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_bugger)),
         ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_library)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)),
         ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_strings)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_terms)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_dcg))),'user').

% ========================================
% include_moo_files(MASK)
% ========================================

include_moo_files(Mask):- expand_file_name(Mask,X),
     forall(member(E,X),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_error,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),dynamic_multifile_exported(P)).
*/
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).


do_term_expansions:- context_module(CM), notrace(do_term_expansions(CM)).

do_term_expansions(_):- thread_self(ID),always_expand_on_thread(ID),!.
do_term_expansions(_):- always_transform_heads,not(prevent_transform_moo_preds),!.
do_term_expansions(_):- is_compiling_clause.
do_term_expansions(CM):- may_moo_term_expand(CM),!, not(ended_transform_moo_preds), not(prevent_transform_moo_preds).

check_term_expansions:- not(do_term_expansions).


:- meta_predicate locate_moo_file(:,-).

locate_moo_file(I,O):-locate_moo_file0(I,O),!.
locate_moo_file(_:O,O):-!.
locate_moo_file(O,O).
locate_moo_file0(user:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(_:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(SpecPre, Path) :-
        catch((expand_file_search_path(SpecPre,Spec)),_,fail),
        catch(absolute_file_name(Spec,
                           [ file_type(prolog),
                             access(read)
                           ],
                           Path),_,fail),
        exists_file(Path),!.

:- meta_predicate ensure_moo_loaded(:).

% the once/1s here arte just for dmiles institional memory
ensure_moo_loaded(A) :-
   setup_call_cleanup(once(asserta(may_moo_term_expand(_))),
        load_moo_files(A),
        once(retract(may_moo_term_expand(asdasdasd)))).

:- meta_predicate load_moo_files(:,+).

load_moo_files(F0):-!,user_ensure_loaded(F0).
% load_moo_files(F0):-use_module(F0).

load_moo_files(M:F0,List):-!,
  locate_moo_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_error,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files(F,[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),redynamic_multifile_exported(true),must_be_module(true)|List]).   
load_moo_files(M:F0,List):-
  locate_moo_file(M:F0,F),  % scope_settings
  'format'(user_error,'% load_moo_files_M ~q.~n',[M=locate_moo_file(F0,F)]),
   load_files(F,[redefine_module(false),module(M),expand(true),if(not_loaded),redynamic_multifile_exported(true),register(false),silent(false),must_be_module(true)|List]).



register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.

hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).
:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).


term_expansion_local(X,_):-not(compound(X)),!,fail.
term_expansion_local( ((':-'(_))) , _ ):-!,fail.

term_expansion_local(_:B1,B2):-!,term_expansion_local(B1,B2),!.

term_expansion_local(((H1:-B1)),H2B2):- !,
   nonvar(B1),  current_context_module(CM),!,        
      functor_catch(H1,F,A), atom_concat(P,'_hook',F),!,atomic_list_concat_catch([_,_|_],'_',P),
      H1=..[F|ARGS], H2=..[P|ARGS],
      B2 = '@'((nop(CM), B1), CM ),
      module_transparent((P/A)),
      % copy_term(H2,META), meta_predicate(META),
      dynamic(P/A),
      dynamic_multifile_exported(P/A),
      multifile(P/A),
      ignore(H2B2 = ((H2 :- B2))),!.


term_expansion_local(X,Y):- compound(X),loading_module_h(CM),functor_catch(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),dynamic_multifile_exported(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).

term_expansion_local0(A,B):- compound(A),term_expansion_local(A,B),!.

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).



current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

% ========================================
% begin/end_transform_moo_preds
% ========================================

:-decl_thlocal is_compiling_clause/0.
is_compiling:-is_compiling_clause;compiling.
:-decl_thlocal ended_transform_moo_preds/0, always_expand_on_thread/1, prevent_transform_moo_preds/0, may_moo_term_expand/1, always_transform_heads/0.
:-module_transparent begin_transform_moo_preds/0, end_transform_moo_preds/0.
:-dynamic_multifile_exported(((begin_transform_moo_preds/0,end_transform_moo_preds/0))).
begin_transform_moo_preds:- retractall(ended_transform_moo_preds),context_module(CM),asserta(may_moo_term_expand(CM)).
end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).

% ========================================
% register_module_type/end_module_type
% ========================================
:- module_transparent register_module_type/1.
:- dynamic_multifile_exported registered_module_type/2.

register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).

:-dynamic_multifile_exported(end_module_type/2).
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).

% :- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

:- include(logicmoo('dbase/dbase.pl')).

% :- include(logicmoo('vworld/moo_footer.pl')).

/** <module> 
% This module defines the module types that we use:
% utility,planner,parser,action,database,effects,spawning_loading,connection
% It is the basic module system of the Logicmoo MUD
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- module(moodb,
        [     
        op(1150,fx,registerCycPred)
        ]).

:- op(1150,fx,(registerCycPred)).

:-export((coerce/3, 
           add_db_prop/3,
           never_use_holds_db/2,
           rem_db_prop/3,
          is_2nd_order_holds/1,
          is_holds_false/1,
          is_holds_true/1,

         current_context_module/1,         
         (registerCycPred)/1,         
         (registerCycPred)/2,
         (registerCycPred)/3,
         ensure_moo_loaded/1,
         isRegisteredCycPred/3,
         registerCycPredPlus2/1,
         term_expansion_local/2,
         run_database_hooks/2,
         moodb:register_module_type/1,
         registered_module_type/2,
         end_module_type/1,
         enter_term_anglify/2,
         % decl_dbase_pred/2,
         register_timer_thread/3,
         scan_updates/0,
          begin_transform_moo_preds/0,
           moodb:end_transform_moo_preds/0,
          loading_module_h/1,
            may_moo_term_expand/0,
            ended_transform_moo_preds/0,
            prevent_transform_moo_preds/0,
         end_module_type/2 )).

:-dynamic loading_module_h/1.

is_holds_true(Prop):-once(notrace((atom(Prop),is_holds_true0(Prop)))).
is_holds_true0(Prop):-member(Prop,[k,p,holds,holds_t,dbase_t,res,assertion_t,assertion]).

is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

% is_holds_false(Prop):-notrace((atom(Prop),once((is_holds_false0(Prop,Stem),is_holds_true0(Stem))))).
is_holds_false(Prop):-member(Prop,[not,nholds,holds_f,dbase_f,resnt,assertion_f,retraction]).

is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).


include_moo_files(Mask):- expand_file_name(Mask,X),
     forall(member(E,X),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_error,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),export(P)).
*/
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-ignore(catch(make,_,true)).

:-dynamic(isRegisteredCycPred/3).

:-dynamic ended_transform_moo_preds/0,prevent_transform_moo_preds/0, may_moo_term_expand/0.

ended_transform_moo_preds.

begin_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(may_moo_term_expand).

 moodb:end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).



:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_library)).

:- dynamic registered_module_type/2.

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

ensure_moo_loaded(A) :-
   setup_call_cleanup(asserta(may_moo_term_expand),
        load_moo_files(A),
        retract(may_moo_term_expand)).

:- meta_predicate load_moo_files(:,+).

load_moo_files(F0):-!,use_module(F0).
load_moo_files(F0):-!,ensure_loaded(F0).

load_moo_files(M:F0,List):-!,
  locate_moo_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_error,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files(F,[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),reexport(true),must_be_module(true)|List]).   
load_moo_files(M:F0,List):-
  locate_moo_file(M:F0,F),  % scope_settings
  'format'(user_error,'% load_moo_files_M ~q.~n',[M=locate_moo_file(F0,F)]),
   load_files(F,[redefine_module(false),module(M),expand(true),if(not_loaded),reexport(true),register(false),silent(false),must_be_module(true)|List]).

enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).

:- dynamic_multifile_exported db_prop/2, is_db_prop/3.

add_db_prop(_,_,Var):- var(Var),!.
add_db_prop(_,_,[]):- !.
add_db_prop(F,A,[C|L]):-!, add_db_prop(F,A,C),add_db_prop(F,A,L),!.
add_db_prop(F,A,CL):- assert_if_new(is_db_prop(F,A,CL)).

rem_db_prop(_,_,Var):- var(Var),!.
rem_db_prop(_,_,[]):- !.
rem_db_prop(F,A,[C|L]):-!, rem_db_prop(F,A,C),rem_db_prop(F,A,L),!.
rem_db_prop(F,A,CL):- retractall(is_db_prop(F,A,CL)).

% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% :- registerCycPred('BaseKB':isa/2). 
% :- registerCycPred('BaseKB':isa(_,_)). 
% :- registerCycPred(isa(_,_),'BaseKB'). 
% :- registerCycPred('BaseKB',isa,2). 
%
%  Will make calls 
% :- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% ============================================
:-dynamic(isRegisteredCycPred/3).

% :- registerCycPred('BaseKB':isa/2). 
registerCycPred(Mt:Pred/Arity):- !,
   registerCycPred(Mt,Pred,Arity).

% :- registerCycPred('BaseKB':isa(_,_)). 
registerCycPred(Mt:Term):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).

registerCycPred(M:F):-!, '@'(registerCycPred(F), M).
registerCycPred(F/A):- registerCycPred(F,A).
registerCycPred([A]):-!,registerCycPred(A).
registerCycPred([A|L]):-!,registerCycPred(A),registerCycPred(L).
registerCycPred((A,L)):-!,registerCycPred(A),registerCycPred(L).

registerCycPredPlus2(M:F):-!, '@'(registerCycPredPlus2(F), M).
registerCycPredPlus2(F/A):- A2 is A -2, registerCycPred(F/A2).
registerCycPredPlus2([A]):-!,registerCycPredPlus2(A).
registerCycPredPlus2([A|L]):-!,registerCycPredPlus2(A),registerCycPredPlus2(L).
registerCycPredPlus2((A,L)):-!,registerCycPredPlus2(A),registerCycPredPlus2(L).


get_term_fa(_:P,F,A):-nonvar(P),!,get_term_fa(P,F,A).
get_term_fa(F/A,F,A):-nonvar(F),!.
get_term_fa(P,F,A):-functor_safe(P,F,A).

% :- registerCycPred(isa,2). 
registerCycPred(Term,Arity):- integer(Arity),!,
   get_term_fa(Term,Pred,_),
   registerCycPred(_Mt,Pred,Arity).
% :- registerCycPred(isa(_,_),'BaseKB'). 
registerCycPred(Term,Mt):- !,
   get_term_fa(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).


   
% :- registerCycPred('BaseKB',isa,2). 
registerCycPred(Mt,_:Pred,Arity):-!,registerCycPred(Mt,Pred,Arity).
registerCycPred(Mt,Pred,0):-!,registerCycPred(Mt,Pred,2).
registerCycPred(Mt,Pred,Arity):-isRegisteredCycPred(Mt,Pred,Arity),!.
registerCycPred(_,Pred,Arity):-isRegisteredCycPred(_,Pred,Arity),!.
registerCycPred(Mt,Pred,Arity):-      
      M = moodb,
      checkCycPred(Pred,Arity),
      assertz(M:isRegisteredCycPred(Mt,Pred,Arity)),
      add_db_prop(Pred,Arity,isRegisteredCycPred(Mt,Pred,Arity)),!.

checkCycPreds:-isRegisteredCycPred(_,F,A),checkCycPred(F,A),fail.
checkCycPreds.

:-dynamic(never_use_holds_db/2).

never_use_holds_db(op,_).
never_use_holds_db(Builtin,Int):-integer(Int), functor(G,Builtin,Int),member(PP,[built_in, imported_from(system), foreign]),predicate_property(G,PP).
never_use_holds_db(F,_):- is_2nd_order_holds(F).

checkCycPred(:,2):-throw(checkCycPred(:,2)).
checkCycPred(F,A):-never_use_holds_db(F,A),throw(never_use_holds_db(F,A)).
checkCycPred(F,A):-copy_term(checkCycPred(F,A),CALL),catch(checkCycPred0(F,A),E,dmsg(E=CALL)).
checkCycPred0(F,A):-functor(P,F,A),compile_predicates([F/A]),get_module_of(P,_M).

:- registerCycPred(db_prop/2).
:- registerCycPred(subclass/2).

:- thread_local(thload:current_agent/2).
:- asserta(thload:current_agent(_,dead)).
:- ignore(retract(thload:current_agent(_,dead))).

current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).


%:- dynamic_multifile_exported agent_call_command/2.
:- dynamic_multifile_exported call_after_load/1.
:- dynamic_multifile_exported thlocal:current_agent/2.
:- dynamic_multifile_exported world_agent_plan/3.


:- registerCycPred(term_anglify_last/2).
:- registerCycPred(term_anglify/2).

:- registerCycPred action_info/1.
:- registerCycPred action_help/2.
:- registerCycPred type_action_help/3.
:- registerCycPred action_rules/4.
:- registerCycPred agent_text_command/4.
:- registerCycPred createableSubclassType/2.
:- registerCycPred createableType/1.
:- registerCycPred label_type_props/3.
:- registerCycPred specifier_text/2.
:- registerCycPred subclass/2.
:- registerCycPred term_anglify/2.
:- registerCycPred term_anglify_last/2.
:- registerCycPred term_anglify_np/3.
:- registerCycPred term_anglify_np_last/3.
:- registerCycPred type_default_props/3.
:- registerCycPred type_grid/3.
:- registerCycPred update_charge/2.
:- registerCycPred update_stats/2.
:- registerCycPred use_usable/4.

:- dynamic_multifile_exported(decl_coerce/3).

decl_coerce(_,_,_):-fail.
coerce(What,Type,NewThing):- decl_coerce(What,Type,NewThing),!.
coerce(What,_Type,NewThing):-NewThing = What.


:- dynamic_multifile_exported decl_database_hook/2.
:- dynamic_multifile_exported decl_mud_test/2.
:- dynamic_multifile_exported verb_alias/2.


% hooks are declared as
% decl_database_hook(assert(A_or_Z),Fact).
% decl_database_hook(retract(One_or_All),Fact).


run_database_hooks(Type,Hook):- must(doall((copy_term(Hook,HCopy), decl_database_hook(Type,HCopy)))).



:- dynamic_multifile_exported registered_module_type/2.

:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).


register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):- repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.
   
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):- retractall(registered_module_type(CM,Type)).

register_module_type(Type):- current_context_module(CM), moodb:register_module_type(CM,Type),begin_transform_moo_preds.

register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types), moodb:register_module_type(CM,T)).
register_module_type(CM,Type):-asserta(registered_module_type(CM,Type)).

registered_module_type(Type):- current_context_module(CM),registered_module_type(CM,Type).
moodb:registered_module_type(Type):- current_context_module(CM),registered_module_type(CM,Type).

:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).

term_expansion_local(X,_):-var(X),!,fail.
term_expansion_local( ((':-'(_))) , _ ):-!,fail.

term_expansion_local(_:B1,B2):-!,term_expansion_local(B1,B2),!.

term_expansion_local(((H1:-B1)),H2B2):- !,
   nonvar(B1),  current_context_module(CM),!,        
      functor(H1,F,A), atom_concat(P,'_hook',F),!,atomic_list_concat([_,_|_],'_',P),
      H1=..[F|ARGS], H2=..[P|ARGS],
      B2 = '@'((nop(CM), B1), CM ),
      module_transparent((P/A)),
      % copy_term(H2,META), meta_predicate(META),
      dynamic_multifile_exported(CM:P/A),
      export(P/A),
      ignore(H2B2 = (( H2 :- B2))),!.

term_expansion_local(X,Y):- loading_module_h(CM),functor(X,F,A),term_expand_local(CM,X,F,A,Y).


term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F/A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):- registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):- registered_module_type(CM,dynamic_multifile_exported), dynamic_multifile_exported(CM:F/A).


term_expansion_local0(A,B):-term_expansion_local(A,B),!.
term_expansion_local0(A,A).

% decl_dbase_pred(Pred,Flags):-debase:dbase_define_db_prop(Pred,Flags).

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).

:- registerCycPred(agent_call_command,2).



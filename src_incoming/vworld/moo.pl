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

:- module(moo,
        [ coerce/3,     
          op(1150,fx,((decl_mpred))),          
          op(1150,fx,((decl_not_mpred))),       
          (decl_not_mpred)/1,
          (decl_not_mpred/2),
          add_mpred_prop/2,
           not_loading_game_file/0,
           never_use_holds_db/3,
           rem_mpred_prop/2,
          is_2nd_order_holds/1,
          is_holds_false/1,
          mpred_arity/2,
          is_holds_true/1,
          include_moo_files/1,
         current_context_module/1,         
         mud_test/2,
         ensure_moo_loaded/1,
         do_all_of/1,
         call_after/2,

         isRegisteredCycPred/3,
         get_mpred_prop/2,
         get_mpred_prop/3,
         registerCycPredPlus2/1,
         term_expansion_local/2,
         run_database_hooks/2,
         register_module_type/1,
         registered_module_type/2,
         end_module_type/1,
         enter_term_anglify/2,
         register_timer_thread/3,
         is_type/1,
         define_type/1,
         scan_updates/0,
         type_action_help/3,
         action_help/2,
            loading_module_h/1,

            always_expand_on_thread/1,
             ended_transform_moo_preds/0, prevent_transform_moo_preds/0, 
             always_transform_heads/0, may_moo_term_expand/1,
             begin_transform_moo_preds/0, end_transform_moo_preds/0,
             do_term_expansions/0,check_term_expansions/0,

          dbase_mod/1,
         end_module_type/2 ]).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

:- '@'((use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_library)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_strings)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_terms)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_dcg))),'user').

% :- once(context_module(user);(trace,context_module(CM),writeq(context_module(CM)))).

:-dynamic(dbase_mod/1).
dbase_mod(dbase).


:- dynamic_multifile_exported action_info/1.
:- dynamic_multifile_exported action_help/2.
:- dynamic_multifile_exported type_action_help/3.

:- dynamic_multifile_exported mud_test/2.
:- dynamic_multifile_exported mpred_arity/2.
:- dynamic_multifile_exported mpred_prop/3.
:- dynamic_multifile_exported agent_call_command/2.
:- dynamic_multifile_exported call_after_load/1.
:- dynamic_multifile_exported moo:world_agent_plan/3.
:- dynamic_multifile_exported now_unused/1.
:- dynamic_multifile_exported hook:decl_database_hook/2.
:- dynamic_multifile_exported decl_mud_test/2.
:- dynamic_multifile_exported verb_alias/2.
:- dynamic_multifile_exported registered_module_type/2.
:- dynamic_multifile_exported (decl_coerce)/3.
:- dynamic_multifile_exported agent_text_command/4.

:- dynamic_multifile_exported((action_info/1,action_help/2,action_rules/4,type_action_help/3,term_specifier_text/2,action_verb_useable/4)).
:- dynamic_multifile_exported((term_anglify/2,term_anglify_last/2, term_anglify_np/3,term_anglify_np_last/3)).
:- dynamic_multifile_exported((update_charge/2,update_stats/2,type_default_props/3)).

:- module_transparent register_module_type/1.
:-thread_local thlocal:repl_writer/2.
:-thread_local thlocal:repl_to_string/2.
:-thread_local thlocal:current_agent/2.
:-thread_local thlocal:dbase_change/2.
:-thread_local thlocal:dbase_opcall/2.

% :-module_transparent repl_writer/2,repl_to_string/2.


:-export(( 
         (decl_mpred)/1,         
         (decl_mpred)/2,
         (decl_mpred)/3,
                   loading_game_file/1 ,  loaded_game_file/1         )).

not_loading_game_file:-not(loading_game_file(_)),loaded_game_file(_).

:-dynamic loading_module_h/1, loading_game_file/1, loaded_game_file/1.


is_holds_true(Prop):- notrace((atom(Prop),is_holds_true0(Prop))),!.

is_holds_true0(Prop):-arg(_,p(k,p,holds,holds_t,dbase_t,asserted_dbase_t,assertion_t,assertion),Prop).

is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

% is_holds_false(Prop):-notrace((atom(Prop),once((is_holds_false0(Prop,Stem),is_holds_true0(Stem))))).
is_holds_false(Prop):-member(Prop,[not,nholds,holds_f,dbase_f,aint,assertion_f,asserted_dbase_f,retraction]).

is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).


% hooks are declared as
% decl_database_hook(assert(A_or_Z),Fact).
hook:decl_database_hook(_,_):-fail.


run_database_hooks(Type,Hook):- must(doall((copy_term(Hook,HCopy), hook:decl_database_hook(Type,HCopy)))).



include_moo_files(Mask):- expand_file_name(Mask,X),
     forall(member(E,X),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_error,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),export(P)).
*/
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).

:-dynamic(isRegisteredCycPred/3).

:-thread_local ended_transform_moo_preds/0, always_expand_on_thread/1, prevent_transform_moo_preds/0, may_moo_term_expand/1, always_transform_heads/0.

%:-module_transparent begin_transform_moo_preds/0, end_transform_moo_preds/0.
begin_transform_moo_preds:- retractall(ended_transform_moo_preds),context_module(CM),asserta(may_moo_term_expand(CM)).


end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).

%:-module_transparent(do_term_expansions/0).
%:-module_transparent(do_term_expansions/1).
%:-module_transparent(check_term_expansions/0).

do_term_expansions:- context_module(CM), notrace(do_term_expansions(CM)).
do_term_expansions(_):- thread_self(ID),always_expand_on_thread(ID),!.
do_term_expansions(_):- always_transform_heads,not(prevent_transform_moo_preds),!.
do_term_expansions(CM):- may_moo_term_expand(CM),!, not(ended_transform_moo_preds), not(prevent_transform_moo_preds).

check_term_expansions:- not(do_term_expansions).

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

% the once/1s here arte just for dmiles institional memory
ensure_moo_loaded(A) :-
   setup_call_cleanup(once(asserta(may_moo_term_expand(_))),
        load_moo_files(A),
        once(retract(may_moo_term_expand(asdasdasd)))).

:- meta_predicate load_moo_files(:,+).

load_moo_files(F0):-!,user_use_module(F0).
% load_moo_files(F0):-use_module(F0).

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


member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).


fix_fa(FA0,F,A):-var(FA0),throw(fix_fa(FA0,F,A)).
fix_fa(_:FA0,F,A):-!,fix_fa(FA0,F,A).
fix_fa(F/A,F,A):- debug,nonvar(F),number(A),!.
fix_fa(FA,F,A):- debug, get_functor(FA,F),!,mpred_arity(F,A).

get_mpred_prop(F,A,Prop):- mpred_prop_plus_assserted(F,A,Prop).
get_mpred_prop(FA,Prop):- fix_fa(FA,F,A),mpred_prop_plus_assserted(F,A,Prop).

add_mpred_prop(_,Var):- var(Var),!.
add_mpred_prop(_,[]):- !.
add_mpred_prop(FA,[C|L]):-!, add_mpred_prop(FA,C),add_mpred_prop(FA,L),!.
add_mpred_prop(F0,CL):- fix_fa(F0,F,A), asserta_new(mpred_prop(F,A,CL)), run_database_hooks(assert(a),mpred_prop(F,A,CL)).

rem_mpred_prop(_,Var):- var(Var),!.
rem_mpred_prop(_,[]):- !.
rem_mpred_prop(FA,[C|L]):-!, rem_mpred_prop(FA,C),rem_mpred_prop(FA,L),!.
rem_mpred_prop(F0,CL):- fix_fa(F0,F,A), retractall(mpred_prop(F,A,CL)), run_database_hooks(retract(all),mpred_prop(F,A,CL)).

% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% :- decl_mpred('BaseKB':isa/2). 
% :- decl_mpred('BaseKB':isa(_,_)). 
% :- decl_mpred(isa(_,_),'BaseKB'). 
% :- decl_mpred('BaseKB',isa,2). 
%
%  Will make calls 
% :- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% ============================================
:-dynamic(isRegisteredCycPred/3).

% :- decl_mpred('BaseKB':isa/2). 
decl_mpred(Mt:Pred/Arity):- !,
   decl_mpred(Mt,Pred,Arity).

% :- decl_mpred('BaseKB':isa(_,_)). 
decl_mpred(Mt:Term):-
   functor(Term,Pred,Arity),
   decl_mpred(Mt,Pred,Arity),!.

decl_mpred(M):-var(M),throw(instanciation_error(decl_mpred(M))).
decl_mpred(AB):-decl_mpred0(AB),!.
decl_mpred(M):-throw(failed(decl_mpred(M))).

decl_mpred0(M:F):-!, '@'(decl_mpred(F), M).
decl_mpred0(F/A):-!, decl_mpred(F,A).
decl_mpred0([A]):-!,decl_mpred(A).
decl_mpred0([A|L]):-!,decl_mpred(A),decl_mpred(L).
decl_mpred0((A,L)):-!,decl_mpred(A),decl_mpred(L).
decl_mpred0(M):-compound(M),functor(M,F,A),decl_mpred(F,A),add_mpred_prop(M,argsIsa(M)).

:- meta_predicate call_after(+,+).


decl_not_mpred(M):-var(M),throw(instanciation_error(decl_not_mpred(M))).
decl_not_mpred(M:F):-!, '@'(decl_not_mpred(F), M).
decl_not_mpred(F/A):-!, decl_not_mpred(F,A).
decl_not_mpred([A]):-!,decl_not_mpred(A).
decl_not_mpred([A|L]):-!,decl_not_mpred(A),decl_not_mpred(L).
decl_not_mpred((A,L)):-!,decl_not_mpred(A),decl_not_mpred(L).
decl_not_mpred(M):-compound(M),functor_safe(M,F,A),decl_not_mpred(F,A).
decl_not_mpred(M):-throw(failed(decl_not_mpred(M))).

decl_not_mpred(F,A):-
   asserta_new(never_use_holds_db(F,A,decl_not_mpred(F,A))),   
   dynamic_multifile_exported(F,A),
   add_mpred_prop(F/A,[ask_module(moo),assert_with_pred(hooked_asserta)]).

:-dynamic(dbase_module_loaded/0).
dbase_module_loaded:- 
   %module_property(dbase,exports(List)),member(add/1,List),
   predicate_property(dbase:add(_),_),!,asserta((dbase_module_loaded:-!)).

call_after(When,C):- When,!,do_all_of(When),once(must(C)).
call_after(When,C):- assert_if_new(will_call_after(When,C)).

:-dynamic(doing_all_of/1).
do_all_of(When):- doing_all_of(When),!.
do_all_of(When):- with_assertions(doing_all_of(When),doall((retract(call_after_load(When,A)),once(must(A))))).

registerCycPredPlus2(M:F):-!, '@'(registerCycPredPlus2(F), M).
registerCycPredPlus2(F/A):- A2 is A -2, decl_mpred(F/A2).
registerCycPredPlus2([A]):-!,registerCycPredPlus2(A).
registerCycPredPlus2([A|L]):-!,registerCycPredPlus2(A),registerCycPredPlus2(L).
registerCycPredPlus2((A,L)):-!,registerCycPredPlus2(A),registerCycPredPlus2(L).


decl_mpred(A,B):-decl_mpred0(A,B),!.


% :- decl_mpred(isa,2). 
decl_mpred0(Term,Arity):- integer(Arity),!,
   get_functor(Term,Pred,_),
   decl_mpred(_Mt,Pred,Arity).

decl_mpred0(Term,Templ):-compound(Templ),add_mpred_prop(Term,Templ).

% :- decl_mpred(isa(_,_),'BaseKB'). 
decl_mpred0(Term,Mt):- !,
   get_functor(Term,Pred,Arity),
   decl_mpred(Mt,Pred,Arity).


decl_mpred(A,B,C):-loop_check(decl_mpred0(A,B,C),dmsg(todo(loop_check(decl_mpred0(A,B,C))))),!.

decl_mpred0(Mt,Pred,Arity):-decl_mpred_now(Mt,Pred,Arity).


% :- decl_mpred('BaseKB',isa,2). 

decl_mpred_now(Mt,M:Pred,Arity):-var(Pred),!,decl_mpred_now(Mt,M:Pred,Arity).
decl_mpred_now(Mt,_:Pred,Arity):- nonvar(Pred),!,decl_mpred_now(Mt,Pred,Arity).
decl_mpred_now(Mt,Pred,0):-!,decl_mpred_now(Mt,Pred,2).
decl_mpred_now(_,Pred,Arity):-mpred_arity(Pred,Arity),!.
decl_mpred_now(Mt,Pred,Arity):-    
  ignore((Arity==1,define_type(Pred))),
      checkCycPred(Pred,Arity),
      assertz(moo:isRegisteredCycPred(Mt,Pred,Arity)).

checkCycPreds:-mpred_arity(F,A),checkCycPred(F,A),fail.
checkCycPreds.


mpred_arity(P,A):-isRegisteredCycPred(_,P,A).

:-dynamic(never_use_holds_db/3).

never_use_holds_db(agent_call_command,2,coded(agent_call_command,2)).
never_use_holds_db(':-',1,oper(':-')):-!.
never_use_holds_db(op,_,oper(op)):-!.
never_use_holds_db(decl_mpred,_,oper(decl_mpred)):-!.

never_use_holds_db(Builtin,Int,pp(Builtin/Int,G,PP)):-integer(Int), functor(G,Builtin,Int),member(PP,[imported_from(system),foreign,built_in]),predicate_property(G,PP),!.
never_use_holds_db(F,_,is_2nd_order_holds):- is_2nd_order_holds(F),!.

checkCycPred(:,2):-!,trace, dumpST, throw(checkCycPred(:,2)).
checkCycPred(F,A):-never_use_holds_db(F,A,Why),throw(never_use_holds_db(F,A,Why)).
checkCycPred(F,A):-copy_term(checkCycPred(F,A),CALL),catch(checkCycPred0(F,A),E,dmsg(E=CALL)).
checkCycPred0(F,A):-functor(P,F,A),compile_predicates([F/A]),get_module_of(P,_M).


:- thread_local(thload:current_agent/2).
:- asserta(thload:current_agent(_,dead)).
:- ignore(retract(thload:current_agent(_,dead))).

:-module_transparent(current_context_module/1).
current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).


decl_coerce(_,_,_):-fail.
coerce(What,Type,NewThing):- decl_coerce(What,Type,NewThing),!.
coerce(What,_Type,NewThing):-NewThing = What.


:- include(logicmoo(vworld/moo_header)).

mpred_prop_plus_assserted(F,A,Prop):- mpred_prop(F,A,Prop).
mpred_prop_plus_assserted(F,A,argsIsa(Templ)):- mpred_arity(F,A), functor(Templ,F,A),!,arg(_,v(argsIsa,multiValued,singleValued,negationByFailure,formatted,mpred,listValued),V),dbase:dbase_t(V,Templ),!.
mpred_prop_plus_assserted(F,A,Prop):- mpred_arity(F,A),functor(Templ,F,A),arg(_,v(argsIsa,multiValued,singleValued,negationByFailure,formatted,mpred,listValued),Prop),dbase:dbase_t(Prop,Templ).


define_type(Var):-var(Var),!,trace_or_throw(define_type(Var)).
define_type(M:F):-!, '@'(define_type(F), M).
define_type([]):-!.
define_type([A]):-!,define_type(A).
define_type([A|L]):-!,define_type(A),define_type(L).
define_type((A,L)):-!,define_type(A),define_type(L).
define_type(Spec):-is_type(Spec),!.

define_type(Spec):- run_database_hooks(assert(z),isa(Spec,type)),assert_if_new(dbase:dbase_t(type,Spec)).

is_type(Spec):- dbase:dbase_t(type, Spec).

:- define_type((type,formattype,multiValued,singleValued,formatted,ppred,mpred,listValued)).

:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).




:- decl_mpred subclass/2.
:- decl_mpred createableSubclassType(type,type).
:- decl_mpred createableType(type).
:- decl_mpred label_type_props/3.
:- decl_mpred label_type/2.
:- decl_mpred subclass/2.
:- decl_mpred type_grid/3.




register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):- repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.
   
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):- retractall(registered_module_type(CM,Type)).

register_module_type(Type):- current_context_module(CM), register_module_type(CM,Type),begin_transform_moo_preds.

register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types), register_module_type(CM,T)).
register_module_type(CM,Type):-asserta(registered_module_type(CM,Type)).

registered_module_type(Type):- current_context_module(CM),registered_module_type(CM,Type).
registered_module_type(Type):- current_context_module(CM),registered_module_type(CM,Type).

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


%:- module_predicates_are_exported(moo).
%:- module_meta_predicates_are_transparent(moo).


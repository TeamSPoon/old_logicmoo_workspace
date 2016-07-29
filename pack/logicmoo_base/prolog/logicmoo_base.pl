/*   
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/
:- if(( system:use_module(system:library('logicmoo/util/logicmoo_util_clause_expansion.pl')), push_modules)). 
:- endif.
:- module(logicmoo_base_file,[]).
% restore entry state
:- lcme:reset_modules.


:- if( \+ current_predicate(system:setup_call_cleanup_each/3)).
:- use_module(system:library('logicmoo/util/logicmoo_util_supp.pl')).
:- endif.

:- use_module(library(logicmoo_utils)).

/*
% baseKB:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
% baseKB:startup_option(clif,sanity). %  Run datalog sanity tests while starting
:- set_prolog_flag(report_error,true).
:- set_prolog_flag(fileerrors,false).
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(debug,true).
:- set_prolog_flag(gc,false).
:- set_prolog_flag(gc,true).
:- set_prolog_flag(optimise,false).
:- set_prolog_flag(last_call_optimisation,false).
:- debug.
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
*/

:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).
:- multifile(baseKB:mpred_is_impl_file/1).
:- dynamic(baseKB:mpred_is_impl_file/1).

:- source_location(F,_),asserta(baseKB:ignore_file_mpreds(F)).

:- multifile baseKB:startup_option/2. 
:- dynamic baseKB:startup_option/2. 
:- multifile baseKB:mpred_system_status/2.
:- dynamic baseKB:mpred_system_status/2.
:- multifile(t_l:disable_px/0).
:- thread_local(t_l:disable_px/0).


baseKB:mpred_skipped_module(eggdrop).
:- forall(current_module(CM),system:assert(baseKB:mpred_skipped_module(CM))).
:- retractall(baseKB:mpred_skipped_module(pfc)).


% ================================================
% DBASE_T System
% ================================================    

:-use_module(system:library('logicmoo/mpred/mpred_at_box.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_expansion.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_loader.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_pfc.pl'),except([op(_,_,_)])).
:-use_module(system:library('logicmoo/mpred/mpred_prolog_file.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_props.pl')).

:-multifile( baseKB:predicateConventionMt/2).
:-dynamic( baseKB:predicateConventionMt/2).
:-multifile( baseKB:predicateConventionMt/2).
:-dynamic( baseKB:predicateConventionMt/2).
:-multifile( baseKB:argsQuoted/1).
:-dynamic( baseKB:argsQuoted/1).


:-use_module(system:library('logicmoo/mpred/mpred_type_isa.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_kb_ops.pl')).
:- kb_dynamic(lmcache:loaded_external_kbs/1).
:- kb_dynamic(baseKB:mpred_skipped_module/1).


:-use_module(system:library('logicmoo/mpred/mpred_agenda.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_storage.pl')).
:-ensure_loaded(system:library('logicmoo/mpred/mpred_userkb.pl')).
:- dynamic(baseKB:argsQuoted/1).
:- dynamic(baseKB:resolveConflict/1).
:- dynamic(baseKB:agent_call_command/2).
:- baseKB:import(baseKB:agent_call_command/2).

:-use_module(system:library('logicmoo/snark/common_logic_sexpr.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_listing.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_stubs.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_type_constraints.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_type_naming.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_type_wff.pl')).
:-use_module(system:library('logicmoo/mpred/mpred_type_args.pl')).

% :-use_module(system:library('logicmoo/mpred/mpred_*.pl')).

:-use_module(system:library('logicmoo/snark/common_logic_snark.pl'),except([op(_,_,_)])).
:-use_module(system:library('logicmoo/mpred/mpred_hooks.pl')).

:-use_module(system:library('logicmoo/snark/common_logic_boxlog.pl')).
:-use_module(system:library('logicmoo/snark/common_logic_skolem.pl')).
:-use_module(system:library('logicmoo/snark/common_logic_kb_hooks.pl')).
:-use_module(system:library('logicmoo/snark/common_logic_compiler.pl'),except([op(_,_,_)])). % ,arity/2,mpred_is_tracing_exec/0, (~)/1



:-use_module(system:library('logicmoo/mpred_online/mpred_www.pl')).




:- thread_local t_l:side_effect_ok/0.
:- lcme:reset_modules.
:- set_defaultAssertMt(baseKB).
:- set_fileAssertMt(baseKB).
:- enable_mpred_expansion.
% system:goal_expansion(I,P1,O,P2):- current_prolog_flag(mpred_te,true),mpred_te(goal,system,I,P1,O,P2).
%system:term_expansion(I,P1,O,P2):- current_prolog_flag(mpred_te,true),mpred_te(term,system,I,P1,O,P2).


base_clause_expansion(P1,end_of_file,O):- !, prolog_load_context(module,Module),mpred_te(term,Module,end_of_file,P1,O,_P2)->fail.
base_clause_expansion(_,I,O):- string(I),!,expand_kif_string_or_fail(pl_te,I,O),!.
base_clause_expansion(_,:-(I), O):-  !, expand_isEach_or_fail(:-(I),O),!.
base_clause_expansion(_,I,':-'(ain_expanded(I))):- get_consequent_functor(I,F,_),
   \+ (clause_b(prologBuiltin(F))),
   (in_dialect_pfc;needs_pfc(I)),!.
base_clause_expansion(_,I, O):- expand_isEach_or_fail(I,O),!.
base_clause_expansion(_,I, _):- once(maybe_builtin(I)),fail.


needs_pfc(I) :- nonvar(I),get_consequent_functor(I,F,A),
   \+ (clause_b(prologBuiltin(F))),
   (clause_b(prologMacroHead(F));clause_b(functorDeclares(F));clause_b(hybrid_support(F,_));baseKB:wrap_shared(F,A,ereq)),!.

maybe_builtin(I) :- nonvar(I),get_consequent_functor(I,F,A),
  % \+ (clause_b(prologMacroHead(F));clause_b(functorDeclares(F));clause_b(hybrid_support(F,_));baseKB:wrap_shared(F,A,ereq)),
   ain(prologBuiltin(F/A)).





/*

:- autoload. % ([verbose(false)]).

bad_thing_to_do:- doall((clause(baseKB:wrap_shared(F,A,ereq),Body),
    retract(( baseKB:wrap_shared(F,A,ereq):- Body )), 
      between(0,9,A),ain((arity(F,A),pfcControlled(F),prologHybrid(F))),fail)).

% :- doall((current_module(W),import_module(W,system),\+ import_module(W, user), W\==baseKB, add_import_module(lmcode,W,end))).

*/
%:- dmsg("Adding logicmoo/[snark|mpred[online]] to autoload path",[]).
%:- add_library_search_path('./logicmoo/snark/',[ '*.pl']).
%:- add_library_search_path('./logicmoo/mpred/',[ 'mpred_*.pl']).
%:- must(add_library_search_path('./logicmoo/mpred_online/',[ '*.pl'])).
% :- add_library_search_path('./logicmoo/',[ '*.pl']).
% :- add_library_search_path('./plarkc/',[ '*.pl']).
% :- add_library_search_path('./pttp/',[ 'dbase_i_mpred_*.pl']).

%baseKB:sanity_check:- findall(U,(current_module(U),default_module(U,baseKB)),L),must(L==[baseKB]).
baseKB:sanity_check:- doall((current_module(M),setof(U,(current_module(U),default_module(U,M),U\==M),L),
     wdmsg(imports_eache :- (L,[sees(M)])))).
baseKB:sanity_check:- doall((current_module(M),setof(U,(current_module(U),default_module(M,U),U\==M),L),wdmsg(imports(M):-L))).
baseKB:sanity_check:- doall((baseKB:mtProlog(M),
    setof(U,(current_module(U),default_module(M,U),U\==M),L),wdmsg(imports(M):-L))).


:- module_transparent(user:exception/3).
:- multifile user:exception/3.
:- dynamic user:exception/3.
:- multifile system:exception/3.
:- module_transparent system:exception/3.
:- dynamic system:exception/3.

/*
:-ignore((baseKB:source_typein_modules(O, _O, _), O\=user,O\=baseKB,O\=system,
   setup_module_ops(O), add_abox_module(O), set_defaultAssertMt(O))).
*/  

:- set_prolog_flag(retry_undefined,false).

% Enable System
system:exception(undefined_predicate,MFA, Action):- false,current_prolog_flag(retry_undefined,true),
    must(loop_check(mpred_at_box:uses_predicate(MFA, Action),true)).
user:exception(undefined_predicate,MFA, Action):- false,current_prolog_flag(retry_undefined,true),
    must(loop_check(mpred_at_box:uses_predicate(MFA, Action),true)).

:- set_prolog_flag(system:unknown,error).
:- set_prolog_flag(user:unknown,error).
:- set_prolog_flag(lmcode:unknown,error).
:- set_prolog_flag(baseKB:unknown,warning).
%:- rtrace((mpred_at_box:defaultAssertMt(G40331),rtrace(set_prolog_flag(G40331:unknown,warning)))).
%:- dbreak.
:- must(set_prolog_flag(abox:unknown,warning)).
:- w_tl(t_l:side_effect_ok,doall(call_no_cuts(baseKB:module_local_init(abox,baseKB)))).
% :- forall(baseKB:sanity_check,true).

:-module_transparent(logicmoo_util_database:ain/1).
:-module_transparent(logicmoo_util_database:aina/1).
:-module_transparent(logicmoo_util_database:ainz/1).
:-multifile(logicmoo_util_database:ain/1).
:-multifile(logicmoo_util_database:aina/1).
:-multifile(logicmoo_util_database:ainz/1).
:- asserta_new((logicmoo_util_database:ainz(G):- !, call(mpred_ainz,G))).
:- asserta_new((logicmoo_util_database:ain(G):- !, mpred_ain(G))).
:- asserta_new((logicmoo_util_database:aina(G):- !, mpred_aina(G))).

% Load boot base file
user:lmbf:- 
 w_tl( set_prolog_flag(mpred_te,true),
  w_tl( set_prolog_flag(lm_expanders,true),
   w_tl(set_prolog_flag(pfc_booted,false),
     with_umt(baseKB,
  time((ensure_mpred_file_loaded(baseKB:library(logicmoo/pfc/'system_base.pfc')))))))),
  set_prolog_flag(pfc_booted,true).



 :- meta_predicate mpred_expansion:temp_comp(*,*,2,?).
 :- meta_predicate mpred_storage:mdel(+).
 %:- meta_predicate mpred_type_isa:assert_isa_hooked_after(?,1).
 :- meta_predicate mpred_pfc:attvar_op_fully(1,?).

:- set_prolog_flag(lm_expanders,false).

% system:clause_expansion(I,PosI,O,PosI):- base_clause_expansion(PosI,I,O),!.
system:term_expansion(I,PosI,O,PosI):- current_prolog_flag(lm_expanders,true),nonvar(I), base_clause_expansion(PosI,I,O)->I\==O.

:- list_undefined.
:- gripe_time(4.0,user:lmbf).
:- set_prolog_flag(lm_expanders,false).
% 
:- set_defaultAssertMt(baseKB).
:- set_fileAssertMt(baseKB).

%:- forall((current_module(M),M\=user,M\=system,M\=baseKB,M\=abox),maybe_add_import_module(M,abox,start)).
%:- forall((current_module(M),M\=user,M\=system,M\=baseKB),maybe_add_import_module(M,baseKB,start)).

:- list_undefined.

:- set_prolog_flag(lm_expanders,false).
% :- set_prolog_flag(read_attvars,false).
:- set_prolog_flag(mpred_te,false).

:- lcme:reset_modules.

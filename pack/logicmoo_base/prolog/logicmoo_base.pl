/*   
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
:- module(logicmoo_base_file,[]).
:- '$set_source_module'(baseKB).
%:- endif.
:- load_files(library(prolog_stack), [silent(true)]).
prolog_stack:stack_guard(none).


:- ensure_loaded(logicmoo_utils).
:- if( \+ current_predicate(system:setup_call_cleanup_each/3)).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_supp.pl')).
:- endif.

% Kill YALL
:- ensure_loaded(library(yall)).
:- w_tl(set_prolog_flag(access_level,system),
   doall(( source_file(yall:lambda_functor(_),O),source_file(M:X,O),M\==yall,
   clause(M:X,_,Ref),clause_property(Ref,file(O)),on_x_fail(erase(Ref))))).

:- w_tl(set_prolog_flag(access_level,system),
   doall(( source_file(yall:lambda_functor(_),O),source_file(M:X,O),M\==yall,
   clause(M:X,B,Ref),clause_property(Ref,file(O)),wdmsg(M:X :- B),on_x_fail(erase(Ref))))).

:- abolish(yall:lambda_functor,1),dynamic(yall:lambda_functor/1).
:- abolish(yall:lambda_like,1),dynamic(yall:lambda_like/1).

/*
% baseKB:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
% baseKB:startup_option(clif,sanity). %  Run datalog sanity tests while starting
:- set_prolog_flag(fileerrors,false).
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(gc,false).
:- set_prolog_flag(gc,true).
:- set_prolog_flag(optimise,false).
:- set_prolog_flag(last_call_optimisation,false).
:- debug.
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
*/
:- set_prolog_flag(verbose_load,true).
:- set_prolog_flag(verbose_autoload, true).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(report_error,true).
:- set_prolog_flag(debug,true).
%:- guitracer.
%:- set_prolog_flag(access_level,system).



:- set_prolog_flag(logicmoo_autoload,false).


% must be xref-ing or logicmoo_autoload or used as include file
:- set_prolog_flag(logicmoo_include,lmbase:skip_module_decl).
% lmbase:skip_module_decl:- source_location(F,L),wdmsg(lmbase:skip_module_decl(F:L)),!,fail.
lmbase:skip_module_decl:-
   (current_prolog_flag(xref,true)-> false ;
    (current_prolog_flag(logicmoo_autoload,true)-> false ;
      ((prolog_load_context(file,F),  prolog_load_context(source,F))
             -> throw(error(format(":- include(~w).",[F]),ensure_loaded(F))) ; true))). 



:- include('logicmoo/mpred/mpred_header.pi').

baseKB:mpred_skipped_module(eggdrop).
:- forall(current_module(CM),system:assert(baseKB:mpred_skipped_module(CM))).
:- retractall(baseKB:mpred_skipped_module(pfc)).
:- set_prolog_flag(lm_expanders,false).
:- set_prolog_flag(mpred_te,false).

% ================================================
% DBASE_T System
% ================================================    

:- if(current_prolog_flag(logicmoo_autoload,false)).

:- include(library('logicmoo/mpred/mpred_at_box.pl')).
:- include(library('logicmoo/mpred/mpred_expansion.pl')).
:- include(library('logicmoo/mpred/mpred_loader.pl')).
:- include(library('logicmoo/mpred/mpred_pfc.pl')). % except([op(_,_,_)]).
:- include(library('logicmoo/mpred/mpred_prolog_file.pl')).
:- include(library('logicmoo/mpred/mpred_props.pl')).

% :- include(library('logicmoo/mpred/mpred_motel.pl')).
:- include(library('logicmoo/mpred/mpred_type_isa.pl')).
:- include(library('logicmoo/mpred/mpred_kb_ops.pl')).
:- include(library('logicmoo/mpred/mpred_agenda.pl')).
:- include(library('logicmoo/mpred/mpred_storage.pl')).

:- include(library('logicmoo/snark/common_logic_sexpr.pl')).
:- include(library('logicmoo/mpred/mpred_listing.pl')).
:- include(library('logicmoo/mpred/mpred_stubs.pl')).

:- include(library('logicmoo/mpred/mpred_type_constraints.pl')).
:- include(library('logicmoo/mpred/mpred_type_naming.pl')).
:- include(library('logicmoo/mpred/mpred_type_wff.pl')).
:- include(library('logicmoo/mpred/mpred_type_args.pl')).

:- include(library('logicmoo/snark/common_logic_snark.pl')). %except([op(_,_,_)]).
:- include(library('logicmoo/mpred/mpred_hooks.pl')).

:- include(library('logicmoo/snark/common_logic_boxlog.pl')).
:- include(library('logicmoo/snark/common_logic_skolem.pl')).
:- include(library('logicmoo/snark/common_logic_compiler.pl')). % ,except([op(_,_,_)])). % ,arity/2,mpred_is_tracing_exec/0, (~)/1

:- include(library('logicmoo/mpred_online/mpred_www.pl')).

:- include(library('logicmoo/snark/common_logic_kb_hooks.pl')).

:- else.

:- dmsg("Adding logicmoo/[snark|mpred[online]] to autoload path",[]).
:- add_library_search_path('./logicmoo/snark/',[ '*.pl']).
:- add_library_search_path('./logicmoo/mpred/',[ 'mpred_*.pl']).
:- must(add_library_search_path('./logicmoo/mpred_online/',[ '*.pl'])).
%:- add_library_search_path('./logicmoo/../',[ 'logicmoo_*.pl']).
%:- add_library_search_path('./logicmoo/',[ '*.pl']).

:- reexport(library('logicmoo/mpred/mpred_at_box.pl')).
:- reexport(library('logicmoo/mpred/mpred_expansion.pl')).
:- reexport(library('logicmoo/mpred/mpred_loader.pl')).
:- reexport(library('logicmoo/mpred/mpred_pfc.pl')). % except([op(_,_,_)]).
:- reexport(library('logicmoo/mpred/mpred_prolog_file.pl')).
:- reexport(library('logicmoo/mpred/mpred_props.pl')).

:- reexport(library('logicmoo/mpred/mpred_motel.pl')).
:- reexport(library('logicmoo/mpred/mpred_type_isa.pl')).
:- reexport(library('logicmoo/mpred/mpred_kb_ops.pl')).
:- reexport(library('logicmoo/mpred/mpred_agenda.pl')).
:- reexport(library('logicmoo/mpred/mpred_storage.pl')).

:- reexport(library('logicmoo/snark/common_logic_sexpr.pl')).
:- reexport(library('logicmoo/mpred/mpred_listing.pl')).
:- reexport(library('logicmoo/mpred/mpred_stubs.pl')).

:- reexport(library('logicmoo/mpred/mpred_type_constraints.pl')).
:- reexport(library('logicmoo/mpred/mpred_type_naming.pl')).
:- reexport(library('logicmoo/mpred/mpred_type_wff.pl')).
:- reexport(library('logicmoo/mpred/mpred_type_args.pl')).

:- reexport(library('logicmoo/snark/common_logic_snark.pl')). %except([op(_,_,_)]).
:- reexport(library('logicmoo/mpred/mpred_hooks.pl')).

:- reexport(library('logicmoo/snark/common_logic_boxlog.pl')).
:- reexport(library('logicmoo/snark/common_logic_skolem.pl')).
:- reexport(library('logicmoo/snark/common_logic_compiler.pl')). % ,except([op(_,_,_)])). % ,arity/2,mpred_is_tracing_exec/0, (~)/1

:- reexport(library('logicmoo/mpred_online/mpred_www.pl')).

:- reexport(library('logicmoo/snark/common_logic_kb_hooks.pl')).

%:- add_library_search_path('./logicmoo/plarkc/',[ '*.pl']).
%:- add_library_search_path('./logicmoo/pttp/',[ 'dbase_i_mpred_*.pl']).

% :- autoload.

:- endif.

%:- thread_local t_l:side_effect_ok/0.
%:- lmce:reset_modules.
%system:goal_expansion(I,P1,O,P2):- current_prolog_flag(mpred_te,true),mpred_te(goal,system,I,P1,O,P2).
%system:term_expansion(I,P1,O,P2):- current_prolog_flag(mpred_te,true),mpred_te(term,system,I,P1,O,P2).




base_clause_expansion(_,I,_):- \+ compound(I), \+ string(I), !, fail.
base_clause_expansion(_,I,_):-
  % prolog_load_context(term,TermWas),
   (b_getval('$source_term',TermWas),\+ same_terms(TermWas, I)),
   (b_getval('$term',STermWas),\+ same_terms(STermWas, I)),!,
   fail.

base_clause_expansion(_,I,O):- string(I),!,expand_kif_string_or_fail(pl_te,I,O),!.

base_clause_expansion(P1,end_of_file,O):- !, prolog_load_context(module,Module),mpred_te(term,Module,end_of_file,P1,O,_P2),!,fail.
base_clause_expansion(_,':-'(ain_expanded(I)),':-'(ain_expanded(I))):-!.
base_clause_expansion(_,:-(I), O):-  !, expand_isEach_or_fail(:-(I),O),!.
base_clause_expansion(_,I, O):- get_consequent_functor(I,F,A),base_clause_expansion_fa(I,O,F,A).
base_clause_expansion(_,I, O):- expand_isEach_or_fail(I,O),!.

base_clause_expansion_fa(_,_,F,_):- prologBuiltin(F),!,fail.
base_clause_expansion_fa(I,':-'(ain_expanded(I)),F,A):- needs_pfc(F,A),!.
base_clause_expansion_fa(I,':-'(ain_expanded(I)),F,A):- in_dialect_pfc,!,ain(hybrid_support(F,A)),ain(prologHybrid(F)).
base_clause_expansion_fa(_,_,F,_):- ain(prologBuiltin(F)),!,fail.


needs_pfc(F,A):- 
  (clause_b(prologMacroHead(F));clause_b(functorDeclares(F));clause_b(prologHybrid(F));
  clause_b(hybrid_support(F,A));clause_b(wrap_shared(F,_,ereq))),!.

/*
maybe_builtin(I) :- nonvar(I),get_consequent_functor(I,F,A),
   \+ (clause_b(prologMacroHead(F));clause_b(functorDeclares(F));clause_b(hybrid_support(F,A))),
   ain(prologBuiltin(F/A)).

*/

:- enable_mpred_expansion.

:- ensure_loaded(library('logicmoo/mpred/mpred_userkb.pl')).

:- set_defaultAssertMt(baseKB).
:- set_fileAssertMt(baseKB).

/*

:- autoload. % ([verbose(false)]).

bad_thing_to_do:- doall((clause(baseKB:wrap_shared(F,A,ereq),Body),
    retract(( baseKB:wrap_shared(F,A,ereq):- Body )), 
      between(0,9,A),ain((arity(F,A),pfcControlled(F),prologHybrid(F))),fail)).

% :- doall((current_module(W),import_module(W,system),\+ import_module(W, user), W\==baseKB, add_import_module(lmcode,W,end))).

*/

%baseKB:sanity_check:- findall(U,(current_module(U),default_module(U,baseKB)),L),must(L==[baseKB]).
baseKB:sanity_check:- doall((current_module(M),setof(U,(current_module(U),default_module(U,M),U\==M),L),
     wdmsg(imports_eache :- (L,[sees(M)])))).
baseKB:sanity_check:- doall((current_module(M),setof(U,(current_module(U),default_module(M,U),U\==M),L),wdmsg(imports(M):-L))).
baseKB:sanity_check:- doall((baseKB:mtProlog(M),
    setof(U,(current_module(U),default_module(M,U),U\==M),L),wdmsg(imports(M):-L))).


%:- rtrace((mpred_at_box:defaultAssertMt(G40331),rtrace(set_prolog_flag(G40331:unknown,warning)))).
%:- dbreak.
:- must(set_prolog_flag(abox:unknown,warning)).
%:- w_tl(t_l:side_effect_ok,doall(call_no_cuts(baseKB:module_local_init(abox,baseKB)))).
% :- forall(baseKB:sanity_check,true).

:-module_transparent(logicmoo_util_database:ain/1).
:-module_transparent(logicmoo_util_database:aina/1).
:-module_transparent(logicmoo_util_database:ainz/1).
:-multifile(logicmoo_util_database:ain/1).
:-multifile(logicmoo_util_database:aina/1).
:-multifile(logicmoo_util_database:ainz/1).
:- asserta_new((logicmoo_util_database:ainz(G):- !, baseKB:call(mpred_ainz,G))).
:- asserta_new((logicmoo_util_database:ain(G):- !, baseKB:mpred_ain(G))).
:- asserta_new((logicmoo_util_database:aina(G):- !, baseKB:mpred_aina(G))).

% Load boot base file
user:lmbf:- 
 w_tl( set_prolog_flag(mpred_te,true),
  w_tl( set_prolog_flag(lm_expanders,true),
   w_tl(set_prolog_flag(pfc_booted,false),
     with_umt(baseKB,
  time((qcompile(baseKB:library(logicmoo/pfc/'system_base.pfc')))))))),
  set_prolog_flag(pfc_booted,true).


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
system:exception(undefined_predicate,MFA, Action):- trace, current_prolog_flag(retry_undefined,true),
    must(loop_check(uses_predicate(MFA, Action),Action=error)).
user:exception(undefined_predicate,MFA, Action):- current_prolog_flag(retry_undefined,true),
    must(loop_check(uses_predicate(MFA, Action),Action=error)).

:- set_prolog_flag(system:unknown,error).
:- set_prolog_flag(user:unknown,error).
:- set_prolog_flag(lmcode:unknown,error).
:- set_prolog_flag(baseKB:unknown,warning).

% system:clause_expansion(I,PosI,O,PosI):- base_clause_expansion(PosI,I,O),!.
system:term_expansion(I,PosI,O,PosI):- current_prolog_flag(lm_expanders,true),nonvar(I), 
      base_clause_expansion(PosI,I,O)->I\==O.

:- enable_mpred_expansion.
:- set_prolog_flag(lm_expanders,true).

% Load boot base file
:- ensure_loaded(library(logicmoo/pfc/'system_base.pfc')).

:- set_prolog_flag(read_attvars,false).


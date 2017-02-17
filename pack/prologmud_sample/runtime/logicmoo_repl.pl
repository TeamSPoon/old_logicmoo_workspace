#!/usr/bin/env swipl
/*
 Logic Engine startup

*/
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

:- if(\+ current_module(baseKB)).
:- [init_cliopatria].
:- unload_file(logicmoo_repl).
:- threads.
:- set_prolog_flag(logicmoo_qsave,true).
:- else.
:- set_prolog_flag(logicmoo_qsave,true).
:- statistics.
:- endif.


:- meta_predicate(load_library_system(:)).
load_library_system(M:File):- load_library_system(M,File). 
load_library_system(M,File):- during_boot(gripe_time(40,(if_file_exists(ensure_loaded(M:File))))).


:- '$set_source_module'(baseKB).
:- '$set_typein_module'(baseKB).

:- set_prolog_flag(do_renames,restore).
:- use_module(library(gvar_syntax)).
:- use_module(library(dif)).

:- baseKB:import(dif:dif/2).
:- baseKB:export(dif:dif/2).
:- baseKB:use_module(library(prolog_predicate)).

is_lm_mod(M):-atom_concat('logicmoo_i_',_,M).
is_lm_mod(M):-atom_concat('common_logic_',_,M).
is_lm_mod(M):-atom_concat('mud_',_,M).
is_lm_mod(M):-atom_concat('mpred_',_,M).
make_exported(op(X,Y,Z),:-op(X,Y,Z)).
make_exported(Pred,:-export(Pred)).

baseKB:term_expansion(:-module(M,List),O,ExportList,O):- nonvar(O),is_lm_mod(M),maplist(make_exported,List,ExportList).
user:term_expansion(I,P,O,P):-baseKB:term_expansion(I,P,O,P).
system:term_expansion(I,P,O,P):-baseKB:term_expansion(I,P,O,P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CYC Alignment util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(do_renames,restore).
:- baseKB:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are probly loaded by other modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- ensure_loaded(library(multivar)).
%:- ensure_loaded(library(udt)).
%:- ensure_loaded(library(atts)).
%:- use_module(library(persistency)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Optionaly] Load the EXTRA Logicmoo WWW System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- baseKB:ensure_loaded(library(logicmoo/mpred_online/mpred_www)).
:- (if_file_exists(ensure_loaded(library(logicmoo/logicmoo_run_pldoc)))).
:- (if_file_exists(ensure_loaded(library(logicmoo/logicmoo_run_swish)))).

:- after_boot((system:kill_unsafe_preds)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BINA43 Code!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- during_boot(ensure_loaded(daydream)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INIT LOGICMOO (AUTOEXEC)  Load the infernce engine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- baseKB:ensure_loaded(library(logicmoo_utils)).
:- baseKB:ensure_loaded(library(logicmoo_base)).

:- ls.

:- input_to_forms_debug("(=> (disjointDecomposition ?CLASS @ROW) (forall (?ITEM1 ?ITEM2) (=> (and (inList ?ITEM1 (ListFn @ROW)) (inList ?ITEM2 (ListFn @ROW)) (not (equal ?ITEM1 ?ITEM2))) (disjoint ?ITEM1 ?ITEM2))))").
:- input_to_forms_debug("(=> (isa ?NUMBER ImaginaryNumber) (exists (?REAL) (and (isa ?REAL RealNumber) (equal ?NUMBER (MultiplicationFn ?REAL (SquareRootFn -1))))))").
:- input_to_forms_debug("(=> (isa ?PROCESS DualObjectProcess) (exists (?OBJ1 ?OBJ2) (and (patient ?PROCESS ?OBJ1) (patient ?PROCESS ?OBJ2) (not (equal ?OBJ1 ?OBJ2)))))").
:- input_to_forms_debug("(=> (contraryAttribute @ROW) (=> (inList ?ELEMENT (ListFn @ROW)) (isa ?ELEMENT Attribute)))").
:- input_to_forms_debug("(=> (and (contraryAttribute @ROW1) (identicalListItems (ListFn @ROW1) (ListFn @ROW2))) (contraryAttribute @ROW2))").
:- input_to_forms_debug("(=> (contraryAttribute @ROW) (forall (?ATTR1 ?ATTR2) (=> (and (equal ?ATTR1 (ListOrderFn (ListFn @ROW) ?NUMBER1)) (equal ?ATTR2 (ListOrderFn (ListFn @ROW) ?NUMBER2)) (not (equal ?NUMBER1 ?NUMBER2))) (=> (property ?OBJ ?ATTR1) (not (property ?OBJ ?ATTR2))))))").
:- input_to_forms_debug("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER CelsiusDegree) (MeasureFn (DivisionFn (SubtractionFn ?NUMBER 32) 1.8) FahrenheitDegree)))").
:- input_to_forms_debug("(DivisionFn (SubtractionFn ?NUMBER 32) 1.8 #C(1.2 9))").

:- baseKB:ensure_loaded(library(logicmoo_user)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETUP CYC KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- during_boot(set_prolog_flag(do_renames,restore)).
:- gripe_time(60,baseKB:qcompile(logicmoo(plarkc/'logicmoo_i_cyc_kb_tinykb.pfc'))).


iRR7_test:-
 baseKB:(
    ain(isa(iRR7,tRR)),
    ain(genls(tRR,tRRP)),
    (\+ tRRP(iRR7) -> (xlisting(iRR7),xlisting(tRRP)) ; true),
    must( isa(iRR7,tRR) ),
    must( isa(iRR7,tRRP) ),
    must( tRRP(iRR7) )).

% :- iRR7_test.

:- after_boot_sanity_test(iRR7_test).

decl_kb_dynamic_tests:-
 sanity(listing((kb_dynamic)/1)),
 kb_dynamic(baseKB:sanity_test/0),
 kb_dynamic(baseKB:regression_test/0),
 kb_dynamic(baseKB:feature_test/0),
 kb_dynamic((
        baseKB:feature_test/0,
        baseKB:mud_test/2,
        baseKB:regression_test/0,
        baseKB:sanity_test/0,
        baseKB:agent_call_command/2,
        action_info/2,
        type_action_info/3)).

:- decl_kb_dynamic_tests.



%:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- debug.
:- baseKB:qsave_lm(lm_repl).
%:- endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD LOGICMOO KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- check_clause_counts.

% :- baseKB:load_library_system(logicmoo(logicmoo_engine)).
% :- baseKB:load_library_system(logicmoo(logicmoo_plarkc)).

:- after_boot((set_prolog_flag(pfc_booted,true),flag_call(logicmoo_debug=true),set_prolog_flag(read_attvars,false))).

:- nb_setval('$oo_stack',[]).
:- b_setval('$oo_stack',[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE PRE KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{flatTrans(G)}==>{baseKB:listing(G/2)}.
:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- baseKB:qsave_lm(lm_repl).
:- endif.




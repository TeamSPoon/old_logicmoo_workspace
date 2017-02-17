#!/usr/bin/env swipl
/*  MUD server startup script in SWI-Prolog

*/
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  entry state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(\+ current_module(baseKB)).
:- [logicmoo_repl].
:- unload_file(init_mud_server).
:- threads.
:- set_prolog_flag(logicmoo_qsave,true).
:- else.
:- set_prolog_flag(logicmoo_qsave,true).
:- statistics.
:- endif.


:- '$set_source_module'(baseKB).
:- '$set_typein_module'(baseKB).



loadNewTiny:-
  baseKB:ain((tinyKB(C,_MT,_STR),{tinykb_assertion_recipe(C,CycLOut),delay_rule_eval(CycLOut,tiny_rule,NewAsserts)}
  ==> {dmsg(tiny_clif(NewAsserts))}, tiny_kb(NewAsserts))).

:- during_boot(set_prolog_flag(do_renames,restore)).


:- baseKB:ensure_loaded(logicmoo('snark/common_logic_sumo.pfc')).

:- during_boot(set_prolog_flag(do_renames,restore)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  entry state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(access_level,system).
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETUP SUMO KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumo_ain('$COMMENT'(_)):- !.
sumo_ain(D):- 
    must(kif_assertion_recipe(D,CycLOut)),
    sumo_ain1(CycLOut).

sumo_ain1(documentation(_, xtChineseLanguage,_)).
sumo_ain1(CycLOut):-
    delay_rule_eval(CycLOut,sumo_rule,NewAsserts),
    dmsg(NewAsserts),
    ain(NewAsserts).


loadSumo1:- 
   with_lisp_translation('./games/ontologyportal_sumo/Merge.kif',sumo_ain),
   with_lisp_translation('./games/ontologyportal_sumo/Mid-level-ontology.kif',sumo_ain),
   !.

loadSumo2:- 
   with_lisp_translation('./games/ontologyportal_sumo/Translations/relations-en.txt',sumo_ain),
   with_lisp_translation('./games/ontologyportal_sumo/english_format.kif',sumo_ain),
   with_lisp_translation('./games/ontologyportal_sumo/domainEnglishFormat.kif',sumo_ain),
   !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE SUMO KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- loadSumo1.
:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- baseKB:qsave_lm(lm_repl1).
:- endif.

:- loadSumo2.
:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- baseKB:qsave_lm(lm_repl2).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE CYC KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- loadNewTiny.
:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- baseKB:qsave_lm(lm_repl3).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_of_file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INIT LOGICMOO (AUTOEXEC)  Load the infernce engine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(\+ current_module(baseKB)).
:- threads.
:- set_prolog_flag(logicmoo_qsave,true).
:- [logicmoo_repl].
:- [init_mud_server].
:- else.
:- statistics.
:- endif.

:- '$set_source_module'(baseKB).
:- '$set_typein_module'(baseKB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD CYC KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- check_clause_counts.

% :-  baseKB:load_library_system(logicmoo(logicmoo_plarkc)).

:-  baseKB:load_library_system(logicmoo(plarkc/logicmoo_i_cyc_kb)).

:- after_boot((set_prolog_flag(pfc_booted,true),flag_call(logicmoo_debug=true),set_prolog_flag(read_attvars,false))).


% :- baseKB:load_library_system(logicmoo(logicmoo_engine)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Mostly Required] Load the Logicmoo Parser/Generator System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- user:load_library_system(library(parser_all)).
%:- user:load_library_system(library(parser_e2c)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Mostly Required] Load the Logicmoo Plan Generator System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- user:load_library_system(library(logicmoo/logicmoo_planner)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Required] Load the CYC Network Client and Logicmoo CycServer Emulator (currently server is disabled)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- user:load_library_system(library(logicmoo/logicmoo_u_cyc_api)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Optional] NOT YET Load the Logicmoo RDF/OWL Browser System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- user:load_library_system(logicmoo(mpred_online/mpred_rdf)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Debugging] Normarily this set as 'true' can interfere with debugging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- set_prolog_flag(gc,true).
% Yet turning it off we cant even startup without crashing
% :- set_prolog_flag(gc,false).


:- doall(printAll(current_prolog_flag(_N,_V))).
:- initialization(kill_unsafe_preds,restore).
:- if(\+ compiling).
:- initialization(kill_unsafe_preds).
:- endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Regression tests that first run whenever a person starts the MUD on the public server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- load_library_system(pack(logicmoo_base/t/examples/pfc/'sanity_col_as_unary.pfc')).
%:- load_library_system(pack(logicmoo_base/t/examples/pfc/'sanity_birdt.pfc')).
%:- load_library_system(pack(logicmoo_base/t/examples/pfc/'sanity_sv.pfc')).
%:- load_library_system(pack(logicmoo_base/t/examples/pfc/'sanity_foob.pfc')).

:- if((gethostname(ubuntu),fail)). % INFO this fail is so we can start faster
:- show_entry(gripe_time(40, doall(baseKB:regression_test))).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ============= MUD SERVER CODE LOADING =============
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- if(\+ exists_source(prologmud(mud_loader))).
:- must((absolute_file_name(logicmoo('../../../prologmud/prolog/prologmud'),Dir),
               asserta(user:file_search_path(prologmud,Dir)))),
 must(exists_source(prologmud(mud_loader))).
:- endif.

:- ensure_loaded(prologmud(mud_loader)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ============= MUD SERVER CODE LOADED =============
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- with_mpred_trace_exec(ain(isLoaded(iSourceCode7))).

:- flag_call(logicmoo_debug=true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Optional] Creates or suppliments a world
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(lmconf:eachRule_Preconditional/1).
:- dynamic(lmconf:eachFact_Preconditional/1).
:- assert_setting01(lmconf:eachRule_Preconditional(true)).
:- assert_setting01(lmconf:eachFact_Preconditional(true)).

:- if(functorDeclares(mobExplorer)).

:- sanity(functorDeclares(tSourceData)).
:- sanity(functorDeclares(mobExplorer)).


:- ain((tCol(tLivingRoom),
 tSet(tRegion),
 tSet(tLivingRoom),

 tSet(mobExplorer),
 genls(tLivingRoom,tRegion),
 genls(tOfficeRoom,tRegion),


%genlsFwd(tLivingRoom,tRegion).
%genlsFwd(tOfficeRoom,tRegion).

% create some seats
mobExplorer(iExplorer1),
mobExplorer(iExplorer2),
mobExplorer(iExplorer3),
mobExplorer(iExplorer4),
mobExplorer(iExplorer5),
mobExplorer(iExplorer6),

(tHumanBody(skRelationAllExistsFn)==>{trace_or_throw(tHumanBody(skRelationAllExistsFn))}),

genls(mobExplorer,tHominid))).

:- endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Required] isRuntime Hook
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ain(((localityOfObject(P,_),isRuntime)==>{put_in_world(P)})).
:- ain(((onStart(Idea)==> ((isLoadedType(tSourceData),isRuntime) ==> {ain_expanded(Idea)})))).
:- ain((mpred_argtypes(mudAreaConnected(tRegion,tRegion)))).
:- install_constant_renamer_until_eof.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Optional] Creates or suppliments a world
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if( \+ tRegion(_)).

:- ain((
tRegion(iLivingRoom7),
tRegion(iOfficeRoom7),

mobExplorer(iExplorer7),
wearsClothing(iExplorer7,'iBoots773'),
wearsClothing(iExplorer7,'iCommBadge774'),
wearsClothing(iExplorer7,'iGoldUniform775'),
mudStowing(iExplorer7,'iPhaser776'))).

:- mpred_type_isa:import(baseKB:tCol/1).
:- mpred_type_isa:import(baseKB:ttCoercable/1).
:- mpred_type_isa:import(baseKB:ttStringType/1).
% :- add_import_module(mpred_type_isa,baseKB,end).
:- onSpawn(localityOfObject(iExplorer7,tLivingRoom)).

:- ain((
pddlSomethingIsa('iBoots773',['tBoots','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iCommBadge774',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']),
pddlSomethingIsa('iGoldUniform775',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iPhaser776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','Device-SingleUser','tWearAble']),

mobMonster(iCommanderdata66),
mobExplorer(iCommanderdata66),
mudDescription(iCommanderdata66,txtFormatFn("Very scary looking monster named ~w",[iCommanderdata66])),
tAgent(iCommanderdata66),
tHominid(iCommanderdata66),
wearsClothing(iCommanderdata66,'iBoots673'),
wearsClothing(iCommanderdata66,'iCommBadge674'),
wearsClothing(iCommanderdata66,'iGoldUniform675'),
mudStowing(iCommanderdata66,'iPhaser676'),

pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iCommBadge674',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']),
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','Device-SingleUser','tWearAble']))).


:- onSpawn(localityOfObject(iCommanderdata66,tOfficeRoom)).
:- onSpawn(mudAreaConnected(tLivingRoom,tOfficeRoom)).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Optionaly] Start the telent server % iCommanderdata66
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_telnet:- on_x_log_cont(start_mud_telnet_4000).
:- after_boot(start_telnet).


% :- assert_setting01(lmconf:eachFact_Preconditional(isRuntime)).

% isa(starTrek,mtCycL).
lst :- ensure_loaded('../prologmud_sample/runtime/games/src_game_startrek/?*.pfc.pl').
% [Manditory] This loads the game and initializes so test can be ran
:- declare_load_dbase('../prologmud_sample/runtime/games/src_game_nani/a_nani_household.pfc.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Optional] the following game files though can be loaded separate instead
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- declare_load_dbase('../prologmud_sample/runtime/games/src_game_nani/objs_misc_household.pfc.pl').
:- declare_load_dbase('../prologmud_sample/runtime/games/src_game_nani/?*.pfc.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Optional] the following worlds are in version control in examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- add_game_dir('../prologmud_sample/runtime/games/src_game_wumpus',prolog_repl).
% :- add_game_dir('../prologmud_sample/runtime/games/src_game_sims',prolog_repl).
% :- add_game_dir('../prologmud_sample/runtime/games/src_game_nani',prolog_repl).
% :- add_game_dir('../prologmud_sample/runtime/games/src_game_startrek',prolog_repl).

%:- check_clause_counts.

:- sanity(argIsa(genlPreds,2,_)).

:- after_boot_sanity_test(argIsa(genlPreds,2,_)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sanity tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sanity_test_ifood_rez:- ignore((
     % mpred_notrace_exec,
     % flag_call(logicmoo_debug>true),
     ain(isa(iFoodRez2,tFood)),must(isa(iFoodRez2,tEatAble)))),
    must((parseIsa_Call(tEatAble,O,["food"],Rest),O=iFoodRez2,Rest=[])).

:- after_boot_sanity_test((dmsg(sanity_test_ifood_rez))).


:- after_boot_sanity_test((gripe_time(1.0,must(coerce("s",vtDirection,_))))).
:- after_boot_sanity_test((gripe_time(2.0,must( \+ coerce(l,vtDirection,_))))).
:- after_boot_sanity_test((statistics)).
:- after_boot_sanity_test(check_clause_counts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Required/Optional]  Ensures...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- after_boot(flag_call(logicmoo_debug=false)).
:- during_boot(set_prolog_flag(unsafe_speedups,false)).

:- lst,statistics,ain(tSourceData(iWorldData8)),ain(isLoaded(iWorldData8)),ain(isRuntime).

lar0 :- dmsg("Ctrl-D to start MUD"),prolog,login_and_run.
lar :- login_and_run.

:- after_boot(lar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Required/Optional]  Ensures...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initialization_after_boot:- listing(system:after_boot_goal/1),dmsg(system:after_boot_call).
:- initialization(initialization_after_boot,after_load).
:- initialization(initialization_after_boot,restore).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Required/Optional]  Ensures...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- statistics.
:- qsave_lm(lm_init_mud).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Required/Optional]  Ensures...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- initialization(after_boot_call,after_load).
:- initialization(after_boot_call,restore).



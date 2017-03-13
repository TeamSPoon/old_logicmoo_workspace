#!/usr/bin/env swipl
/*  MUD server startup script in SWI-Prolog

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETUP KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- '$set_typein_module'(baseKB).
:- '$set_source_module'(baseKB).
:- ensure_loaded(library(logicmoo_plarkc)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD CYC KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- check_clause_counts.

:- after_boot((set_prolog_flag(pfc_booted,true),flag_call(runtime_debug=true),set_prolog_flag(read_attvars,false))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Mostly Required] Load the Logicmoo Parser/Generator System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- user:load_library_system(library(parser_all)).
%:- user:load_library_system(library(parser_e2c)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Mostly Required] Load the Logicmoo Plan Generator System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- user:load_library_system(library(logicmoo_planner)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Required] Load the CYC Network Client and Logicmoo CycServer Emulator (currently server is disabled)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- user:load_library_system(library(plark/logicmoo/logicmoo_u_cyc_api)).


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

:- flag_call(runtime_debug=true).


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


genlsFwd(tLivingRoom,tRegion),
genlsFwd(tOfficeRoom,tRegion),

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
:- set_prolog_flag_until_eof(do_renames,term_expansion).



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
% :- mpred_type_isa:import(baseKB:ttStringType/1).
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


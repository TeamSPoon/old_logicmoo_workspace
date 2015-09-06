% #! swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/
% ==========================================================
% Sanity tests that first run whenever a person stats the MUD to see if there are regressions in the system
% ==========================================================
:-multifile(user:sanity_test/0).
:-multifile(user:regression_test/0).
:-multifile(user:feature_test/0).


:- user:ensure_loaded(logicmoo_repl).


% [Mostly Required] Load the Logicmoo Plan Generator System
:- with_no_mpred_expansions(if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_planner)))).


% [Required] Load the CYC Network Client and Logicmoo CycServer Emulator (currently server is disabled)
:- with_no_mpred_expansions(user:ensure_loaded(library(logicmoo/plarkc/logicmoo_i_cyc_api))).


% [Mostly Required] Load the Logicmoo Parser/Generator System
:- gripe_time(40,user:ensure_loaded(library(parser_all))).


% [Optional] NOT YET Load the Logicmoo RDF/OWL Browser System
% % :- with_no_mpred_expansions(if_file_exists(user:ensure_loaded(logicmoo(mpred_online/dbase_i_rdf_store)))).


% [Debugging] Normarily this set as 'true' can interfere with debugging
% :- set_prolog_flag(gc,true).
% Yet turning it off we cant even startup without crashing

:- doall(show_call(current_prolog_flag(_N,_V))).


% ==========================================================
% Regression tests that first run whenever a person stats the MUD on the public server
% ==========================================================

:- if((gethostname(ubuntu),fail)). % INFO this fail is so we can start faster
:- show_call_entry(gripe_time(40, doall(user:regression_test))).
:- endif.


% ==============================
% MUD SERVER CODE LOADS
% ==============================

% [Required] load the mud system
:- show_call_entry(gripe_time(40,user:ensure_loaded(prologmud(mud_loader)))).


% ==============================
% MUD SERVER CODE STARTS
% ==============================

:- file_begin(pfc).

% [Optional] Creates or suppliments a world

tCol(tRegion).
tCol(tLivingRoom).
genls(tLivingRoom,tRegion).
genls(tOfficeRoom,tRegion).

% create some seats
tExplorer(iExplorer1).
tExplorer(iExplorer2).
tExplorer(iExplorer3).
tExplorer(iExplorer4).
tExplorer(iExplorer5).
tExplorer(iExplorer6).

(tHumanBody(skRelationAllExistsFn)==>{trace_or_throw(tHumanBody(skRelationAllExistsFn))}).

genls(tExplorer,tHominid).



tExplorer(iExplorer7).
wearsClothing(iExplorer7,'iBoots773').
wearsClothing(iExplorer7,'iCommBadge774').
wearsClothing(iExplorer7,'iGoldUniform775').
mudStowing(iExplorer7,'iPhaser776').
pddlSomethingIsa('iBoots773',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iCommBadge774',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform775',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).

isa(iCommanderdata66,'tMonster').
mudDescription(iCommanderdata66,txtFormatFn("Very screy looking monster named ~w",[iCommanderdata66])).
tAgent(iCommanderdata66).
tHominid(iCommanderdata66).
isa(iCommanderdata66,'tExplorer').
wearsClothing(iCommanderdata66,'iBoots673').
wearsClothing(iCommanderdata66,'iCommBadge674').
wearsClothing(iCommanderdata66,'iGoldUniform675').
mudStowing(iCommanderdata66,'iPhaser676').
pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iCommBadge674',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).


mpred_argtypes(bordersOn(tRegion,tRegion)).
mpred_argtypes(ensure_some_pathBetween(tRegion,tRegion)).

:-onSpawn(localityOfObject(tExplorer,tLivingRoom)).
:-onSpawn(localityOfObject(iCommanderdata66,tOfficeRoom)).
:-onSpawn(bordersOn(tLivingRoom,tOfficeRoom)).

:- file_begin(pl).

% [Optionaly] Start the telent server
start_telnet:- logOnError(toploop_telnet:start_mud_telnet_4000).

:- initialization(start_telnet).

:- user:ensure_loaded(start_mud_server).

:- rl_add_history( 'user:ensure_loaded(start_mud_server).' ).



#!/usr/local/bin/swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/

:- exists_directory(runtime)->working_directory(_,runtime);(exists_directory('../runtime')->working_directory(_,'../runtime');true).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').
user:file_search_path(weblog, '/usr/lib/swi-prolog/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(cliopatria, '../externals/ClioPatria'). % :- current_prolog_flag(unix,true).
user:file_search_path(user, '../externals/ClioPatria/user/').
user:file_search_path(swish, '../externals/swish'):- current_prolog_flag(unix,true).
user:file_search_path(pack, '../packs/').
user:file_search_path(prologmud, '../prolog/prologmud/').
:- attach_packs.

% [Required] Load the Logicmioo Base System
:- user:ensure_loaded(library(logicmoo/logicmoo_base)).

% [Optionaly] Load an Eggdrop 
:- if_file_exists(ensure_loaded('../externals/MUD_ircbot/prolog/eggdrop.pl')).
:- current_predicate(egg_go/0)->egg_go;true.




% [Required] load the mud system
:- user:ensure_loaded(prologmud(mud_startup)).

% [Optional] the following worlds are in version control in examples
% :- add_game_dir('../games/src_game_wumpus',prolog_repl).       
% :- add_game_dir('../games/src_game_sims',prolog_repl).
% :- add_game_dir('../games/src_game_startrek',prolog_repl).
% :- add_game_dir('../games/src_game_nani',prolog_repl).       


% [Optional] Creates or suppliments a world

tCol(tLivingRoom).
genls(tLivingRoom,tRegion).
genls(tOfficeRoom,tRegion).

tAgent(iCommanderdata66).
isa(iCommanderdata66,'tExplorer').
wearsClothing(iCommanderdata66,'iBoots673').
wearsClothing(iCommanderdata66,'iCommBadge674').
wearsClothing(iCommanderdata66,'iGoldUniform675').
mudStowing(iCommanderdata66,'iPhaser676').
pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iCommBadge674',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).

% create some seats
tExplorer(iPlayer1).
tExplorer(iPlayer2).
tExplorer(iPlayer3).
tExplorer(iPlayer4).
tExplorer(iPlayer5).
tExplorer(iPlayer6).


tAgent(iExplorer7).
wearsClothing(iExplorer7,'iBoots773').
wearsClothing(iExplorer7,'iCommBadge774').
wearsClothing(iExplorer7,'iGoldUniform775').
isa(iExplorer7,'tExplorer').
mudStowing(iExplorer7,'iPhaser776').
pddlSomethingIsa('iBoots773',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iCommBadge774',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform775',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).

isa(iCommanderdata66,'tMonster').
mudDescription(iCommanderdata66,txtFormatFn("Very screy looking monster named ~w",[iCommanderdata66])).


mpred_argtypes(pathConnects(tRegion,tRegion)).
mpred_argtypes(ensure_some_pathBetween(tRegion,tRegion)).

:-onSpawn(localityOfObject(tExplorer,tLivingRoom)).
:-onSpawn(localityOfObject(iCommanderdata66,tOfficeRoom)).
:-onSpawn(pathConnects(tLivingRoom,tOfficeRoom)).



% [Optionaly] Start the telent server
:-at_start(toploop_telnet:start_mud_telnet(4000)).


% [Optional] the following game files though can be loaded separate instead
:- declare_load_dbase('../games/src_game_nani/a_nani_household.plmoo').
:- declare_load_dbase('../games/src_game_nani/objs_misc_household.plmoo').
:- declare_load_dbase('../games/src_game_nani/*.plmoo').


% [Manditory] This loads the game and initializes so test can be ran
:- (if_startup_script( at_start(finish_processing_world))).


% [Optionaly] Run a battery of tests
% :- if_startup_script( doall(now_run_local_tests_dbg)).


disabled_sanity_test0a:-enqueue_player_command("hide").

disabled_sanity_test0b:- enqueue_player_command(actWho).


disabled_sanity_test1:- enqueue_player_command("rez crackers"),
   enqueue_player_command("drop crackers"),
   enqueue_player_command('look'),
   enqueue_player_command("take crackers"),
   enqueue_player_command("eat crackers"),
   enqueue_player_command('look').

disabled_sanity_test2:-enqueue_player_command("rez pants"),
   enqueue_player_command("wear pants"),
   enqueue_player_command("tp to closet"),
   enqueue_player_command("take shirt"),
   enqueue_player_command("inventory").


disabled_sanity_test3:-enqueue_player_command(prolog).

% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
% :- register_timer_thread(npc_ticker,30,npc_tick).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_startup_script(at_start(run)).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).
   

#!/usr/local/bin/swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/

:- exists_directory(runtime)->working_directory(_,runtime);(exists_directory('../runtime')->working_directory(_,'../runtime');true).
:-multifile(user:file_search_path/2).
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').
user:file_search_path(weblog, '/usr/lib/swi-prolog/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(cliopatria, '../externals/ClioPatria'). % :- current_prolog_flag(unix,true).
user:file_search_path(user, '../externals/ClioPatria/user/').
user:file_search_path(swish, '../externals/swish'):- current_prolog_flag(unix,true).
user:file_search_path(pack, '../packs/').
user:file_search_path(prologmud, '../packs/prologmud/prolog/prologmud/').
:- attach_packs.

% [Required] Load the Logicmioo Base System
:- user:ensure_loaded(library(logicmoo/logicmoo_base)).

% [Optionaly] Load an Eggdrop 
:- if_file_exists(ensure_loaded('../externals/MUD_ircbot/prolog/eggdrop/eggdrop.pl')).
:- current_predicate(egg_go/0)->egg_go;true.


% [Required] load the mud system
:- user:ensure_loaded(prologmud(mud_startup)).

% [Optional] the following worlds are in version control in examples
% :- add_game_dir('../games/src_game_wumpus',prolog_repl).       
% :- add_game_dir('../games/src_game_sims',prolog_repl).
% :- add_game_dir('../games/src_game_startrek',prolog_repl).
:- add_game_dir('../games/src_game_nani',prolog_repl).       

% [Optional] the following game files though can be loaded separate instead
% :- declare_load_dbase('../games/src_game_nani/a_nani_household.plmoo').
% :- declare_load_dbase('../games/src_game_nani/objs_misc_household.plmoo').

% [Optional] Creates or suppliments a world

tCol(tLivingRoom).
genls(tLivingRoom,tRegion).
genls(tOfficeRoom,tRegion).

tAgentGeneric(iCommanderData66).
isa(iCommanderData66,'tExplorer').
wearsClothing(iCommanderData66,'iBoots673').
wearsClothing(iCommanderData66,'iComBadge674').
wearsClothing(iCommanderData66,'iGoldUniform675').
mudStowing(iCommanderData66,'iPhaser676').
pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iComBadge674',['tComBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).

tAgentGeneric(iExplorer7).
wearsClothing(iExplorer7,'iBoots773').
wearsClothing(iExplorer7,'iComBadge774').
wearsClothing(iExplorer7,'iGoldUniform775').
isa(iExplorer7,'tExplorer').
mudStowing(iExplorer7,'iPhaser776').
pddlSomethingIsa('iBoots773',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iComBadge774',['tComBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform775',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).

isa(iCommanderData66,'tMonster').
mudDescription(iCommanderData66,txtFormatFn("Very screy looking monster named ~w",[iCommanderData66])).


:-onSpawn(localityOfObject(tExplorer,tLivingRoom)).
:-onSpawn(localityOfObject(iCommanderData66,tOfficeRoom)).
:-onSpawn(pathConnects(tLivingRoom,tOfficeRoom)).


mpred_argtypes(pathConnects(tRegion,tRegion)).
mpred_argtypes(ensure_some_pathBetween(tRegion,tRegion)).

% arity(do_ensure_some_pathBetween,0).
ensure_some_pathBetween(R1,R2):- pathBetween(R1,_,R2),!.
ensure_some_pathBetween(R1,R2):- pathBetween(R2,_,R1),!.
ensure_some_pathBetween(R1,R2):- random_path_dir(Dir), not(pathBetween(R1,Dir,_)),must(reverse_dir(Dir,Rev)),not(pathBetween(R2,Rev,_)),!, 
   must((add(pathBetween(R1,Dir,R2)),add(pathBetween(R2,Rev,R1)))),!.
ensure_some_pathBetween(R1,R2):- must((add(pathBetween(R1,apathFn(R1,R2),R2)),add(pathBetween(R2,apathFn(R2,R1),R1)))),!.

% isa(user:do_ensure_some_pathBetween,prologOnly).

do_ensure_some_pathBetween:-
  must((forall(no_repeats((is_asserted(pathConnects(R1,R2)),ground(R1:R2),isa(R1,tRegion),isa(R2,tRegion),dif(R1,R2))),
    must((ensure_some_pathBetween(R1,R2),ensure_some_pathBetween(R2,R1)))))).


:-onEachLoad(must(do_ensure_some_pathBetween)).


% [Manditory] This loads the game and initializes so test can be ran
:- (if_startup_script( at_start(finish_processing_world))).

% [Manditory] But soon it will be triggerd by the next block
:- (((user:do_ensure_some_pathBetween))).

% [Optionaly] Start the telent server
:-at_start(toploop_telnet:start_mud_telnet(4000)).


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
   

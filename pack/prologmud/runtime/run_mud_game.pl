#!/usr/bin/env swipl
/** <module> MUD server startup script in SWI-Prolog

*/


:- user:ensure_loaded(init_mud_server).
started_mud_server.


% :- make.
:- add_import_module(mpred_storage,baseKB,end).
:- listing({}/1).
:- listing(mudAtLoc/2).
% ==============================
% MUD GAME CODE LOADS
% ==============================

% [Manditory] This loads the game and initializes so test can be ran
:- declare_load_dbase('../games/src_game_nani/a_nani_household.plmoo').

% [Optional] the following game files though can be loaded separate instead
:- declare_load_dbase('../games/src_game_nani/objs_misc_household.plmoo').
:- declare_load_dbase('../games/src_game_nani/?*.plmoo').

% [Optional] the following worlds are in version control in examples
% :- add_game_dir('../games/src_game_wumpus',prolog_repl).       
% :- add_game_dir('../games/src_game_sims',prolog_repl).
% :- add_game_dir('../games/src_game_nani',prolog_repl).       
:- add_game_dir('../games/src_game_startrek',prolog_repl).

% ==============================
% MUD GAME REPL 
% ==============================
% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_startup_script(at_start(login_and_run)).
:- if_startup_script(initialization(login_and_run)).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).


end_of_file.

/*
Warning: baseKB:add/1, which is referenced by
Warning:        4-th clause of baseKB:irc_mud_event_hook/3: 4-th clause of baseKB:irc_mud_event_hook/3
Warning:        1-st clause of baseKB:rez_loc_object/2: 1-st clause of baseKB:rez_loc_object/2
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:122:5: 1-st clause of baseKB:set_player_telnet_options/1
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:123:5: 1-st clause of baseKB:set_player_telnet_options/1
Warning: baseKB:add_missing_instance_defaults/1, which is referenced by
Warning:        1-st clause of baseKB:rez_loc_object/2: 1-st clause of baseKB:rez_loc_object/2
Warning: baseKB:any_to_dir/2, which is referenced by
Warning:        1-st clause of baseKB:pathBetween_call_0/3: 1-st clause of baseKB:pathBetween_call_0/3
Warning: baseKB:calc_from_center_xyz/6, which is referenced by
Warning:        1-st clause of baseKB:mudExitAtLoc/3: 1-st clause of baseKB:mudExitAtLoc/3
Warning: baseKB:current_agent/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:147:3: 1-st clause of baseKB:session_loop/2
Warning: baseKB:deliver_event/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:127:5: 1-st clause of baseKB:goodbye_player/0
Warning: baseKB:doorLocation/5, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:337:80: 1-st clause of baseKB:show_room_grid_single/3
Warning: baseKB:enqueue_agent_action/3, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:175:42: 1-st clause of baseKB:enqueue_session_action/3
Warning: baseKB:failOnError/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:236:45: 5-th clause of baseKB:code_list_to_next_command/2
Warning: baseKB:foc_current_agent/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:126:5: 1-st clause of baseKB:goodbye_player/0
Warning:        4-th clause of baseKB:irc_mud_event_hook/3: 4-th clause of baseKB:irc_mud_event_hook/3
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:105:3: 1-st clause of baseKB:player_connect_menu/4
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:106:3: 1-st clause of baseKB:player_connect_menu/4
Warning: baseKB:get_agent_sessions/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:187:25: 1-st clause of deliver_event_hooks/2
Warning: baseKB:get_session_id/1, which is referenced by
Warning:        2-nd clause of baseKB:irc_action_queue/3: 2-nd clause of baseKB:irc_action_queue/3
Warning:        4-th clause of baseKB:irc_mud_event_hook/3: 4-th clause of baseKB:irc_mud_event_hook/3
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:101:3: 1-st clause of baseKB:player_connect_menu/4
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:200:6: 1-st clause of baseKB:prompt_read_telnet/4
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:159:3: 1-st clause of baseKB:register_player_stream_local/3
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:134:14: 1-st clause of baseKB:run_session/2
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:146:2: 1-st clause of baseKB:session_loop/2
Warning: baseKB:list_agents/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:383:8: 3-th clause of baseKB:cmdShowRoomGrid/4
Warning: baseKB:look_as/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:268:57: 3-th clause of baseKB:look_brief/1
Warning: baseKB:obj_memb/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:384:8: 3-th clause of baseKB:cmdShowRoomGrid/4
Warning: baseKB:object_string/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:259:48: 2-nd clause of baseKB:telnet_repl_obj_to_string/3
Warning: baseKB:prop_memb/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:413:15: 1-st clause of baseKB:display_grid_labels/0
Warning: baseKB:random_instance/3, which is referenced by
Warning:        2-nd clause of baseKB:random_path_dir/1: 2-nd clause of baseKB:random_path_dir/1
Warning:        3-th clause of baseKB:random_path_dir/1: 3-th clause of baseKB:random_path_dir/1
Warning:        4-th clause of baseKB:random_path_dir/1: 4-th clause of baseKB:random_path_dir/1
Warning: baseKB:reverse_dir/2, which is referenced by
Warning:        2-nd clause of baseKB:ensure_some_pathBetween/2: 2-nd clause of baseKB:ensure_some_pathBetween/2
Warning: baseKB:say/2, which is referenced by
Warning:        4-th clause of baseKB:deliver_to_irc/2: 4-th clause of baseKB:deliver_to_irc/2
Warning: baseKB:start_agent_action_thread/0, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_telnet.pl:148:2: 1-st clause of baseKB:session_loop/2
Warning: baseKB:to_egg/2, which is referenced by
Warning:        1-st clause of baseKB:invite_to_mud/1: 1-st clause of baseKB:invite_to_mud/1
Warning: kellerStorage:kellerStorageTestSuite/0, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/mud_loader.pl:275:93: 1-st clause of mud_test_local/0
Warning: w_tl/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/mud_loader.pl:222:10: 1-st clause of debug_repl_w_cyc/2
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/mud_loader.pl:182:28: 2-nd clause of hard_work/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/mud_loader.pl:195:12: 1-st clause of slow_work/0
Warning: with_no_assertions/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/mud_loader.pl:231:10: 1-st clause of debug_repl_wo_cyc/2

*/

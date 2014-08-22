/** <module> 
% This file loads the world (world.pl), the map of the world, 
% the agents and their definitions.
% This file is used as a configuation file and a startup script.
%
% July 10,1996
% John Eikenberry
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


:- set_prolog_flag(verbose_load,true).

:- include(logicmoo('vworld/moo_header.pl')).

:-export(include_moo_files/1).
include_moo_files(Mask):- expand_file_name(Mask,X),
     forall(member(E,X),user_use_module(E)).

in_user_startup(Call):- '@'(user:Call,user).

:- '@'(use_module(logicmoo('vworld/moo.pl')),'user').

% standard header used in all files that all modules are loaded (therefore useful for when(?) the day comes that modules *can*only*see their explicitly imported modules)
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- use_module(library(random)).
%:- use_module(library(date)).
% This one is for use with SWI
:- use_module(library(quintus)).


% logicmoo utils shared with other systems
:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

:- '@'((use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_library)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_strings)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_terms)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_dcg)),
         use_module(logicmoo(vworld/moo))),'user').


% logicmoo vworld mud server
%:- user_use_module(logicmoo(vworld/world)).
:- user_use_module(logicmoo(vworld/toploop_npc)).
:- user_use_module(logicmoo(vworld/toploop_telnet)).
:- user_use_module(logicmoo(vworld/toploop_output)).


:- user_use_module(logicmoo(vworld/moo_testing)).

/*

 First time you run this 2 million clauses are qcompiled 
 (I've excluded 7 million more clauses that are only available with spec ial C Y C  Liciens ing)

%     /devel/logicmoo/src_data/pldata/tiny_kb.pl *qcompiled* into tiny_kb 2.40 sec, 8,481 clauses
%     /devel/logicmoo/src_data/pldata/nldata_freq_pdat.pl *qcompiled* into nldata_freq_pdat 7.88 sec, 107,704 clauses
%     /devel/logicmoo/src_data/pldata/nldata_BRN_WSJ_LEXICON.pl *qcompiled* into nldata_BRN_WSJ_LEXICON 7.65 sec, 113,863 clauses
%     /devel/logicmoo/src_data/pldata/nldata_colloc_pdat.pl *qcompiled* into nldata_colloc_pdat 6.31 sec, 64,081 clauses
%     /devel/logicmoo/src_data/pldata/nldata_cycl_pos0.pl *qcompiled* into nldata_cycl_pos0 0.20 sec, 2,488 clauses
%     /devel/logicmoo/src_data/pldata/nldata_dictionary_some01.pl *qcompiled* into nldata_dictionary_some01 0.03 sec, 293 clauses
%     /devel/logicmoo/src_data/pldata/tt0_00022_cycl.pl *qcompiled* into tt0_00022_cycl 26.86 sec, 313,234 clauses
%     /devel/logicmoo/src_data/pldata/hl_holds.pl *qcompiled* into hl_holds 175.31 sec, 1,041,317 clauses
%     /devel/logicmoo/src_data/pldata/mworld0_declpreds.pl *qcompiled* into dbase 0.05 sec, 680 clauses
%     /devel/logicmoo/src_data/pldata/mworld0.pl *qcompiled* into mworld0 60.49 sec, 483,046 clauses

  It took several minutes on my 24 core machine with 128gb ram on all SSDs as you can see.. 

  But afterwards (the results next) .. it is able to load the system from .qlf in a mater of under 3 seconds!

  No other SQL clone has been able to beat this .. Prolog uses 80% less ram and 10x times faster than
    any SQL indexing strategy I've for a large database (wtf? secret is all atoms are keys)  
   (The atom table (pointers to strings) is of no interest/use during join ops obviouslly.. 
     in which i have to do millions of join ops per semantic parse)

%     logicmoo('pldata/tiny_kb') loaded into tiny_kb 0.02 sec, 9,016 clauses
%     logicmoo('pldata/nldata_freq_pdat') loaded into nldata_freq_pdat 0.10 sec, 107,709 clauses
%     logicmoo('pldata/nldata_BRN_WSJ_LEXICON') loaded into nldata_BRN_WSJ_LEXICON 0.09 sec, 113,868 clauses
%     logicmoo('pldata/nldata_colloc_pdat') loaded into nldata_colloc_pdat 0.06 sec, 64,086 clauses
%     logicmoo('pldata/nldata_cycl_pos0') loaded into nldata_cycl_pos0 0.00 sec, 2,479 clauses
%     logicmoo('pldata/nldata_dictionary_some01') loaded into nldata_dictionary_some01 0.00 sec, 264 clauses
%     logicmoo('pldata/tt0_00022_cycl') loaded into tt0_00022_cycl 0.28 sec, 313,287 clauses
%     logicmoo('pldata/hl_holds') loaded into hl_holds 1.31 sec, 1,041,321 clauses
%     logicmoo('pldata/mworld0_declpreds') loaded into dbase 0.00 sec, 679 clauses
%     logicmoo('pldata/mworld0') loaded into mworld0 0.60 sec, 483,058 clauses

*/

% done in 'user' to avoid reloading when we reload dbase
ensure_q_loaded(File):-
    expand_file_search_path(logicmoo('pldata/mworld0_declpreds.pl'),Path),exists_file(Path),!,                                 
   '@'(load_files(File,[if(not_loaded),qcompile(auto),expand(true),derived_from(Path)]),user).

make_qlfs:-
 %ensure_q_loaded(logicmoo('pldata/tiny_kb')),
 ensure_q_loaded(logicmoo('pldata/nldata_freq_pdat')),
 ensure_q_loaded(logicmoo('pldata/nldata_BRN_WSJ_LEXICON')),
 ensure_q_loaded(logicmoo('pldata/nldata_colloc_pdat')),
 ensure_q_loaded(logicmoo('pldata/nldata_cycl_pos0')),
 ensure_q_loaded(logicmoo('pldata/nldata_dictionary_some01')),
 % ensure_q_loaded(logicmoo('pldata/tt0_00022_cycl')),
 %ensure_q_loaded(logicmoo('pldata/hl_holds')),
 %ensure_q_loaded(logicmoo('pldata/mworld0')),
 %ensure_q_loaded(logicmoo('pldata/mworld0_declpreds')),
 catch(ensure_q_loaded(logicmoo('pldata/withvars_988')),_,true).

% :- catch(logicmoo('pldata/mworld0_declpreds.qlf'),_,make_qlfs).



/*

% done in 'user' to avoid reloading when we reload dbase

:- include_moo_files('../src_data/pldata/?*.pl').

*/

% :- user_use_module(logicmoo(pldata/tiny_kb)).
:- user_use_module(logicmoo(pldata/nldata_freq_pdat)).
:- user_use_module(logicmoo(pldata/nldata_BRN_WSJ_LEXICON)).
:- user_use_module(logicmoo(pldata/nldata_colloc_pdat)).
:- user_use_module(logicmoo(pldata/nldata_cycl_pos0)).
:- user_use_module(logicmoo(pldata/nldata_dictionary_some01)).
% :- user_use_module(logicmoo(pldata/tt0_00022_cycl)).
% :- user_use_module(logicmoo(pldata/hl_holds)).
% :- user_use_module(logicmoo(pldata/mworld0)).
:- user_use_module(logicmoo(pldata/transform_dump)).
% :- catch(user_use_module(logicmoo(pldata/withvars_988)),_,true).
:- catch(user_use_module(logicmoo(pldata/el_assertions)),_,true).

:- asserta(loaded_external_kbs),show_call(kbp_to_dbase_t).


% :- user_use_module(logicmoo(dbase/dbase_formattypes)).
% :- user_use_module(logicmoo(parsing/parser_imperative)).
:- user_ensure_loaded(logicmoo(parsing/parser_chat80)). 
:- user_ensure_loaded(logicmoo(parsing/parser_e2c)).


:- user_use_module(logicmoo('vworld/moo_loader.pl')).

:- load_data_file(logicmoo('dbase/dbase_i_builtin.pl')).


% These contain the definition of the object types.
:- in_user_startup(ensure_plmoo_loaded(logicmoo('objs/objs_misc_monster.plmoo'))). 

% Load the map file (*.map.pl) appropriate for the world being used.
:- in_user_startup(ensure_plmoo_loaded(logicmoo('rooms/vacuum.map.plmoo'))).
% NPC planners
:- user_use_module(logicmoo('mobs/monster.pl')).

:- user_use_module(logicmoo('actions/look.pl')).

% :-module(user).
%:-prolog.
% end_of_file.

/*
:- user_use_module(logicmoo('mobs/predator.pl')).
:- user_use_module(logicmoo('mobs/explorer.pl')).
:- user_use_module(logicmoo('mobs/prey.pl')).
:- user_use_module(logicmoo('mobs/mobs_conf.pl')).
:- user_use_module(logicmoo('mobs/vacuum.pl')).
*/
:- include_moo_files('../src_incoming/mobs/?*.pl').


% Action/Commands implementation
/*
:- user_use_module(logicmoo('actions/drink.pl')).
:- user_use_module(logicmoo('actions/actions_db.pl')).
:- user_use_module(logicmoo('actions/attack.pl')).
:- user_use_module(logicmoo('actions/inventory.pl')).
:- user_use_module(logicmoo('actions/push.pl')).
:- user_use_module(logicmoo('actions/where.pl')).
:- user_use_module(logicmoo('actions/climb.pl')).
:- user_use_module(logicmoo('actions/move.pl')).
:- user_use_module(logicmoo('actions/any.pl')).
:- user_use_module(logicmoo('actions/drop.pl')).
:- user_use_module(logicmoo('actions/help.pl')).
:- user_use_module(logicmoo('actions/logon.pl')).
:- user_use_module(logicmoo('actions/get_set.pl')).
:- user_use_module(logicmoo('actions/sit.pl')).
:- user_use_module(logicmoo('actions/actions_conf.pl')).
:- user_use_module(logicmoo('actions/take.pl')).
:- user_use_module(logicmoo('actions/chat.pl')).
:- user_use_module(logicmoo('actions/use.pl')).
:- user_use_module(logicmoo('actions/stats.pl')).
:- user_use_module(logicmoo('actions/eat.pl')).
:- user_use_module(logicmoo('actions/teleport.pl')).
*/
:- include_moo_files('../src_incoming/actions/?*.pl').



% Define the agents traits, both for your agent and the world inhabitants. 
% agent name and stats ([] = defaults).
% Agents with numbered names (eg. prey(1)) are able to be used multiple times.
% Just copy their line and increment the number to make more of them.
/*
:-create_agent(predator(1),[str(4),stm(2),height(2),spd(3)]).
:-create_agent(prey(1),[str(0),stm(-8),spd(1),height(1)]).
:-create_agent(prey(2),[str(0),stm(-8),spd(1),height(1)]).
:-create_agent(prey(3),[str(0),stm(-8),spd(1),height(1)]).
%:-create_agent(prey(4),[str(0),stm(-8),spd(1),height(1)]).
:-create_agent(monster(1),[str(6),stm(2),height(2),spd(1)]).
:-create_agent(monster(2),[str(6),stm(2),height(2),spd(1)]).
:-create_agent(explorer(1),[str(2),spd(4),stm(3),height(2)]).
:-create_agent(vacuum(1),[]).
:-create_agent(explorer(2),[]).
*/

:- moo:begin_transform_moo_preds.

moo:agent_text_command(Agent,[run,Term], Agent,prologCall(Term)):- ignore(Term=someCode).

% [Optionaly] Start the telent server
:-at_start(toploop_telnet:start_mud_telnet(4000)).


% standard header used in all files that all modules are loaded (therefore useful for when(?) the day comes that modules *can*only*see their explicitly imported modules)
% :- user_use_module(logicmoo(vworld/moo_header)).

% These contain the definition of the object types.

:- ensure_plmoo_loaded(logicmoo('objs/objs_misc_household.plmoo')).

:- ensure_plmoo_loaded(logicmoo('objs/objs_misc_monster.plmoo')).

% Load the map file (*.map.pl) appropriate for the world being used.
:- ensure_plmoo_loaded(logicmoo('rooms/maze.map.plmoo')).
:- ensure_plmoo_loaded(logicmoo('rooms/predator.map.plmoo')).
:- ensure_plmoo_loaded(logicmoo('rooms/vacuum.map.plmoo')).

% :- ensure_plmoo_loaded(logicmoo('rooms/*.plmoo')).

% Load the map file (*.map.pl) appropriate for the mobs being used.
%:- forall(files_matching(logicmoo('mobs/?*.plmoo'), X),dmsg(X)).
%:- ensure_plmoo_loaded(logicmoo('mobs/?*.plmoo')).
%:- ensure_plmoo_loaded(logicmoo('mobs/*/?*.plmoo')).


% puts world into running state
% :- must(old_setup).

% [Optionaly] Start the telnet server


% standard footer to clean up any header defined states
:- include(logicmoo('vworld/moo_footer.pl')).
/*
% Load datalog
:- if_flag_true(fullStart, ((use_module(logicmoo('des/des.pl')),
  flush_output,
  init_des,
  display_status,
 %  des,
   !))).

*/



% GOLOG SYSTEM WITHOUT FLUX (Default Commented Out)
%:- if_flag_true(fullStart,use_module(logicmoo('indigolog/indigolog_main_swi.pl'))).

% FLUX AGENT SYSTEM WITHOUT GOLOG (Default Commented Out)
%:- if_flag_true(fullStart,use_module(logicmoo('indigolog/flux_main_swi.pl'))).

% FLUX AGENT SYSTEM WITH GOLOG
% :- if_flag_true(true,use_module(logicmoo('indigolog/indigolog_main_swi_flux.pl'))).

% LOGICMOO DATABASE LOGIC ENGINE SERVER
%:- if_flag_true(true,use_module(logicmoo('database/logicmoo.swi'))).

% when we import new and awefull code base (the previous )this can be helpfull
% we redfine list_undefined/1 .. this is the old version
lundef :- A = [],
       check:( merge_options(A, [module_class([user])], B),
        prolog_walk_code([undefined(trace), on_trace(found_undef)|B]),
        findall(C-D, retract(undef(C, D)), E),
        (   E==[]
        ->  true
        ;   print_message(warning, check(undefined_predicates)),
            keysort(E, F),
            group_pairs_by_key(F, G),
            maplist(report_undefined, G)
        )).

% :- if_flag_true(fullStart,remove_undef_search).


/*
  ==
  ?- [library(mudconsole)].
  ?- mc_start.				% opens browser

   or else http_mud_server

  ?- mc_format('Hello ~w', [world]).
  ?- mc_html(p(['Hello ', b(world)])).
  ?- mc_ask([age(Age)], [p('How old are you'), input([name(age)])]).
  Age = 24.				% type 24 <enter>
  ==

*/



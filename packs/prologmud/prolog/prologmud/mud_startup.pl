/** <module> 
% This file loads the world (world.pl), the map of the world, 
% the agents and their definitions.
% This file is used as a configuation file and a startup script.
%
% July 10,1996
% John Eikenberry
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


:- set_prolog_flag(verbose_load,true).

:- include(mud_header).

:- prolog_load_context(directory,Dir),asserta(user:file_search_path(prologmud,Dir)).


% standard header used in all files that all modules are loaded (therefore useful for when(?) the day comes that modules *can*only*see their explicitly imported modules)
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- use_module(library(random)).
%:- use_module(library(date)).
% This one is for use with SWI
:- use_module(library(quintus)).


% logicmoo utils shared with other systems
:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).
/*
:- '@'((ensure_loaded(library(logicmoo/util/logicmoo_util_bugger)),
         ensure_loaded(library(logicmoo/util/logicmoo_util_library)),
         use_module(library(logicmoo/util/logicmoo_util_ctx_frame)),
         ensure_loaded(library(logicmoo/util/logicmoo_util_strings)),
         use_module(library(logicmoo/util/logicmoo_util_terms)),
         use_module(library(logicmoo/util/logicmoo_util_dcg)),
         use_module(prologmud(server/mud))),'user').
*/


% logicmoo vworld mud server

:- user:ensure_loaded(logicmoo(logicmoo_base)).
:- user:ensure_loaded(prologmud(server/mud_telnet)).
:- user:ensure_loaded(prologmud(vworld/world)).

:- user:ensure_loaded(prologmud(server/mud_testing)).


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

:- include_moo_files('../src_asserts/pldata/?*.pl').

*/
:-export(user_ensure_nl_loaded/1).
user_ensure_nl_loaded(F):-load_files([F],[expand(true),if(changed),qcompile(auto)]).

% :- user:ensure_loaded(logicmoo(pldata/tiny_kb)).
/*
:- user_ensure_nl_loaded(logicmoo(pldata/nldata_freq_pdat)).
:- user_ensure_nl_loaded(logicmoo(pldata/nldata_BRN_WSJ_LEXICON)).
:- user_ensure_nl_loaded(logicmoo(pldata/nldata_colloc_pdat)).
:- user_ensure_nl_loaded(logicmoo(pldata/nldata_cycl_pos0)).
:- user_ensure_nl_loaded(logicmoo(pldata/nldata_dictionary_some01)).
:- user_ensure_nl_loaded(logicmoo(pldata/nldata_talk_db_pdat)).
*/
% :- user:ensure_loaded(logicmoo(pldata/tt0_00022_cycl)).
% :- user:ensure_loaded(logicmoo(pldata/hl_holds)).
% :- user:ensure_loaded(logicmoo(pldata/mworld0)).
% :- user_ensure_nl_loaded(logicmoo(pldata/transform_dump)).
% :- catch(user:ensure_loaded(logicmoo(pldata/withvars_988)),_,true).
download_and_install_el:-
  shell('wget -N http://logicmoo.org/devel/LogicmooDeveloperFramework/TEMP~/www.logicmoo.org/downloads/datafiles/PlDataBinary.zip',_),
  shell('unzip -u -d ../src_assets/pldata/ PlDataBinary.zip'),
  catch(user:ensure_loaded(logicmoo(pldata/el_assertions)),E,fmt('Cant use el_assertions',E)).

%:- xperimental_big_data->catch(user:ensure_loaded(logicmoo(pldata/el_assertions)),_,download_and_install_el);true.

% :- asserta(loaded_external_kbs),show_call(kbp_to_dbase_t).

:- user:ensure_loaded(prologmud(vworld/world_agent)).
:- user:ensure_loaded(prologmud(parsing/parser_imperative)).
:- user:ensure_loaded(prologmud(vworld/world)).

/*
:- user:ensure_loaded(logicmoo(parsing/parser_talk)). 
:- user:ensure_loaded(logicmoo(parsing/parser_e2c)). 
:- user:ensure_loaded(logicmoo(parsing/parser_CURT)). 
:- user:ensure_loaded(logicmoo(parsing/parser_chat80)). 
*/

%:- user:ensure_loaded(logicmoo(dbase/dbase_ext_lisp)).
%:- user:ensure_loaded(logicmoo(dbase/dbase_ext_chr)).




% NPC planners
:- include_moo_files(prologmud(mobs/'?*.pl')).
:- include_moo_files('../src_assets/mobs/?*.pl').
:- xperimental->include_moo_files('../external/XperiMental/src_incoming/mobs/?*.pl');true.


% Action/Commands implementation
:- include_moo_files(prologmud(actions/'?*.pl')).
:- include_moo_files('../src_assets/actions/?*.pl').
:- xperimental->include_moo_files('../external/XperiMental/src_incoming/actions/?*.pl');true.

% New Objects
:- include_moo_files(prologmud(objs/'?*.pl')).
:- include_moo_files('../src_assets/objs/?*.pl').
:- xperimental->include_moo_files('../external/XperiMental/src_incoming/actions/?*.pl');true.


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

:- begin_transform_moo_preds.

user:agent_text_command(Agent,["run",Term], Agent,actProlog(Term)):- ignore(Term=someCode).

%:-forall(make_tabled_perm(get_all_templates(TEMPL)),dmsg(TEMPL)).
%:-forall(make_tabled_perm(grab_argsIsa(F,Types)),dmsg(grab_argsIsa(F,Types))).


:- forall(filematch('*/*.plmoo', X),(dmsg(ensure_plmoo_loaded(X)),ensure_plmoo_loaded(X))).

% [Optionaly] Start the telent server
:-at_start(toploop_telnet:start_mud_telnet(4000)).


% standard header used in all files that all modules are loaded (therefore useful for when(?) the day comes that modules *can*only*see their explicitly imported modules)
% :- include(prologmud(mud_header)).

% These contain the definition of the object cols.
% Load the map file appropriate for the world being used.
% Load the mud files appropriate for the mobs being used.
:- forall(filematch(prologmud('*/?*.plmoo'), X),dmsg(X)).
:- trace,ensure_plmoo_loaded(prologmud('*/?*.plmoo')).
:- forall(filematch(prologmud('*/*/?*.plmoo'), X),dmsg(X)).
:- ensure_plmoo_loaded(prologmud('*/*/?*.plmoo')).

% puts world into running state
% :- must(old_setup).

% [Optionaly] Start the telnet server


% standard footer to clean up any header defined states
:- include(prologmud(mud_footer)).
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
  Age = 24.				% col 24 <enter>
  ==

*/

:-pfc_untrace.

/** <module> An Implementation a MUD server in SWI-Prolog

*/
:- use_module(library(settings)).

% ======================================================
% Savea location of *this* file into logicmoo_runtime_dir/1
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:-dynamic(logicmoo_runtime_dir/1).
:- source_location(File,_Line),
    file_directory_name(File, RunDir),
    retractall(logicmoo_runtime_dir(RunDir)),
    asserta(logicmoo_runtime_dir(RunDir)).

% Add the locations that the MUD source files will be picked up by the system
local_directory_search('.'). % required
local_directory_search('../src_game').  % for non uploadables
local_directory_search('../src_incoming').  % for user uploads
local_directory_search('~logicmoo-mud/cynd/startrek'). % home dir CynD world

join_path33(A,B,C):-exists_directory(B)->B=C;directory_file_path(A,B,C).

% register search path hook
user:file_search_path(logicmoo,Directory):-
	logicmoo_runtime_dir(RunDir),
	local_directory_search(Locally),
	join_path33(RunDir,Locally,Directory).

% one more case of not clear what's the good way to do this.
% Add your own path to weblog for now
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').
user:file_search_path(weblog, '/usr/local/lib/swipl-7.1.11/pack/weblog/prolog').
user:file_search_path(cliopatria, '/devel/ClioPatria').
user:file_search_path(cliopatria, 't:/devel/ClioPatria').



:-use_module(logicmoo('mudconsole/mudconsolestart')).

% [Optionaly] load and start sparql server
% if we don't start cliopatria we have to manually start
%

start_servers :-
	current_prolog_flag(version,F),
	F > 70109,
	use_module(logicmoo(launchcliopatria)).
start_servers :-
	current_prolog_flag(version,F),
	F =< 70109,
	http_mud_server.
:- start_servers.

% [Optionaly 1st run] tell where ClioPatria is located and restart for the 2nd run
%:-set_setting(cliopatria_binding:path, 't:/devel/ClioPatria'), save_settings('moo_settings.db').

% [Required] load and start mud
:- use_module(logicmoo('vworld/moo_startup')).





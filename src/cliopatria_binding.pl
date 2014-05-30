:- module(cliopatria_binding, []).
/** <module> Separate module so setting ends up in right place

% [Optionaly 1st run] tell where ClioPatria is located and restart
:-set_setting(cliopatria_binding:path, '/devel/ClioPatria'), save_settings('moo_settings.db').

*/
:- use_module(library(settings)).

:- multifile http:location/3.
:- dynamic   http:location/3.

% doesn't descend from root because that's being moved for cliopatria
http:location(cliopatria, root(cliopatria), [priority(100)]).


:- setting(path, atom, '/devel/ClioPatria', 'Path to root of cliopatria install').

% :- load_settings('moo_settings.db').

add_cliopatria_to_search_path :-
	setting(path, invalid),
	!,
	writeln('set the cliopatria path by querying set_setting(cliopatria_binding:path, \'c:/path/to/cliopatria\'), save_settings(\'moo_settings.db\').'),
	fail.
add_cliopatria_to_search_path :-
	setting(path, Path),
	asserta(user:file_search_path(cliopatria, Path)).


:- add_cliopatria_to_search_path.

:- ensure_loaded(logicmoo(vworld/dbase_rdf_entailment)).


:- module(user_page, []).
/** <module> The web page the user interacts with

*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path), []).
:- use_module(library(http/http_server_files), []).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).

% :- style_check(-atom).

:- multifile http:location/3.
:- dynamic   http:location/3.

% all URI locations to do with play are under some subtree.
% this means we can separate, for example, having apache forward via
% a rewrite rule only a subdomain to /mud and below.
%
http:location(mud, root(mud), []).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

% These should remain. They are static assets served by the core
% server (eg. the javascript to make the page go, the fallback css, etc)
user:file_search_path(js, './http/web/js').
user:file_search_path(css, './http/web/css').
user:file_search_path(icons, './http/web/icons').

% Someday these will be set up per-MUD
%
user:file_search_path(js, '../src_assets/web/js').
user:file_search_path(css, '../src_assets/web/css').
user:file_search_path(icons, '../src_assets/web/icons').

% The game page where players spend most of gtheir time
:- http_handler(mud(.),    mud_page,    [priority(10)]).

:- html_resource(jquery,
		 [ virtual(true),
		   requires(js('jquery-1.7.1.js'))
		 ]).
:- html_resource(js('jquery.form.js'),
		 [ requires(jquery)
		 ]).
:- html_resource(js('mud.js'),
		 [ requires(jquery),
		   requires(js('jquery.form.js'))
		 ]).
:- html_resource(mud,
		 [ virtual(true),
		   requires(js('mud.js'))
		 ]).

mud_page(_Request) :-
	reply_html_page(
	    title('Logic Moo'),
	    [h1('Yup this is it'),
	     \map_section,
	     \description_section,
	     \stats_section,
	     \player_prompt,
	     \input_section,
	     p('Howdy howdy')]
	).

map_section -->
	html(p('someday this will be a snazzy map')).
description_section -->
	html(p('someday this will be the description')).
stats_section -->
	html(p('someday this will be the stats')).
player_prompt -->
	html(p('someday this will be the player prompt')).
input_section -->
	html(form([input([type(text), id(nl), name(nl)], [])])).

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

http:location(mud, root(mud), []).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(js, './http/web/js').
user:file_search_path(css, './http/web/css').

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

mud_page(Request) :-
	reply_html_page(
	    title('Logic Moo'),
	    [h1('Yup this is it'),
	     p('Howdy howdy')]
	).



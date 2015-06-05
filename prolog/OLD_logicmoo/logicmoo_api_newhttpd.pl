

:- use_module(logicmoo('mudconsole/mudconsole')).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path), []).
:- use_module(library(http/http_server_files), []).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
%:- use_module(library(option)).

% :- style_check(-atom).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(mudlogic, root(mudlogic), []).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(js, './http/web/js').
user:file_search_path(css, './http/web/css').

:- http_handler(mudlogic(''),    mudlogic_handler,    [priority(-10)]).
/*
:- http_handler(mudlogic('mc_message'), mc_message, []).
:- http_handler(mudlogic('mc_reply'),   mc_reply,   []).
*/

:- html_resource(jquery,
		 [ virtual(true),
		   requires(js('jquery-1.7.1.js'))
		 ]).
:- html_resource(js('jquery.form.js'),
		 [ requires(jquery)
		 ]).
:- html_resource(js('mudconsole.js'),
		 [ requires(jquery),
		   requires(js('jquery.form.js'))
		 ]).
:- html_resource(mudconsole,
		 [ virtual(true),
		   requires(js('mudconsole.js'))
		 ]).

/*
:- html_meta
	mc_html(html),
	mc_html(+, html),
	mc_html(+, html, +).
*/

:- multifile
        user:body//2.

user:body(game, Body) -->
        html(body([ \html_requires(webfonts),
		    \html_requires('/css/mudconsole.css'),
		    div(id(top), h1('Logicmoo Data Server')),
                    div(id(content), Body)
                  ])).

% wrong way
mudlogic_handler(Request):- make,      
           writeSTDERR('REQUEST: "~q" \n',[Request]), 
           writeHTMLStdHeader('Logicmoo Data Server'),
           fmt('<pre>'),
           listing(agent/1),
           listing(atloc/2),
           fmt('</pre>'),
           writeHTMLStdFooter,!.

% correct way
mudlogic_handler(_Request) :-
	reply_html_page(game,
			title('Logicmoo Data Server'),
			\game_page).

game_page -->
	html([div(class(map_section), [
		   \id_div(map),
		   \id_div(inventory)
		  ]),
	    \id_div(output),
	    \id_div(error_area),
	    \input_area,
	    p(class(directions), 'Type into box above')
	]).

id_div(ID) -->
	html([             
	    div(id(ID), &(nbsp))
	]).

input_area -->
	html([
	    div(id(input_area),
		input([type(text), id(inarea), value('Type here')]))
	]).


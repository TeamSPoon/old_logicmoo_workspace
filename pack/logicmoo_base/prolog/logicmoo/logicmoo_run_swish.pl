#!/usr/local/bin/swipl 

% :- module(swish_with_localedit,[]).


:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- multifile(mpred_online:semweb_startup).
:- '@'(ensure_loaded(library(logicmoo/util/logicmoo_util_bugger)),user).

:- use_module(library(process)).
install_bower:- prolog_file_dir(('.'),LPWD),
   process_create(sudo,[bower,install,'--allow-root'],[cwd(LPWD),process(PID)]),
   process_wait(PID,_Status).

:- add_to_search_path(swish, '../pack/swish/').

% setup paths to load relevant packages from development environment

/*
:- asserta(user:file_search_path(foreign, '../http')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(js, 'web/js')).
*/

% Hack: auto-loading this does not work.
:- [library(charsio)].
:- [charsio:library(memfile)].

:- debug(pengine(delay)).

:- use_module(library(plunit)).
:- use_module(library(pengines)).
:- use_module(pengine_sandbox:library(pengines)).
:- use_module(library(process)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).

:- ensure_loaded('../pack/swish/swish').


:- pengine_application(swish).
:- use_module(swish:library(pengines_io)).
pengines:prepare_module(Module, swish, _Options) :- pengines_io:pengine_bind_io_to_html(Module).

:- debug(http(request)).

% :- use_module(library(pldoc)).
% :- use_module(library(pldoc/doc_http)).


:- use_module(library(settings)).

:- multifile http:location/3.
:- dynamic   http:location/3.

:- use_module(library(memfile)).
%:- use_module(server).

/*
swish_highlight:insert_memory_file(X,Y,Z):-dmsg(error(swish_highlight:insert_memory_file(X,Y,Z))).
swish_highlight:delete_memory_file(X,Y,Z):-dmsg(error(swish_highlight:delete_memory_file(X,Y,Z))).
swish_highlight:memory_file_line_position(X,Y,Z,A):-dmsg(error(swish_highlight:memory_file_line_position(X,Y,Z,A))).
swish_highlight:memory_file_substring(X,Y,Z,A,B):-dmsg(error(swish_highlight:memory_file_substring(X,Y,Z,A,B))).
swish_highlight:memory_file_to_string(X,Y):- memory_file_to_codes(X,C),string_codes(Y,C). %  dmsg(error(swish_highlight:memory_file_to_string(X,Y))).
*/


:-multifile(prolog:sandbox_allowed_clause/1).
prolog:sandbox_allowed_clause(Clause):-nonvar(Clause).
:-multifile(sandbox:safe_primitive/1).
sandbox:safe_primitive(X):-nonvar(X),!.
sandbox:safe_primitive(P):-var(P),!,current_predicate(F/A),functor(P,F,A).
sandbox:safe_primitive(M:P):-var(P),!,current_predicate(M:F/A),functor(P,F,A).


:- use_module(library(pldoc)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/mimetype)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_hook)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(www_browser)).

:- use_module(pldoc(doc_process)).
:- use_module(pldoc(doc_htmlsrc)).
:- use_module(pldoc(doc_html)).
:- use_module(pldoc(doc_index)).
:- use_module(pldoc(doc_search)).
:- use_module(pldoc(doc_man)).
:- use_module(pldoc(doc_wiki)).
:- use_module(pldoc(doc_util)).
:- use_module(pldoc(doc_access)).
:- use_module(pldoc(doc_pack)).

:- use_module(library(doc_http)).
:- abolish(pldoc_http:src_skin,5).



pldoc_http:src_skin(Request, _Show, FormatComments, header, Out) :-  
  pldoc_http:((     
     member(request_uri(ReqURI), Request),!,
	prolog_xref:negate(FormatComments, AltFormatComments),
	replace_parameters(ReqURI, [show(raw)], RawLink),
        replace_parameters(ReqURI, [], EditLink0),
         logicmoo_util_strings:atom_subst(EditLink0,'help/source/doc','swish/filesystem/',EditLink),
	replace_parameters(ReqURI, [format_comments(AltFormatComments)], CmtLink),
	phrase(html(div(class(src_formats),
			[ 'View source with ',
			  a(href(CmtLink), \alt_view(AltFormatComments)),
                        ' or as ',
                        a(href(RawLink), raw),
                        ' or EDIT ',
                        a(href(EditLink), edit)
			])), Tokens),
	html_write:print_html(Out, Tokens))).

% called through source_to_html/3.
:- public(pldoc_http:src_skin/5).
:- prolog_listing:listing(pldoc_http:src_skin/5).

edit_file_href(_Options,File0, HREF) :-
 pldoc_index:((  is_absolute_file_name(File0),
	insert_alias(File0, File),
	ensure_slash_start(File, SlashFile),
	http_location([path(SlashFile)], Escaped),
	http_location_by_id(pldoc_doc, DocRoot),
        atom_concat(DocRoot, Escaped, HREFDOC))),
        logicmoo_util_strings:atom_subst(HREFDOC,'help/source/doc','swish/filesystem/',HREF),!.
edit_file_href(_Options,HREF, HREF).

doc_file_href(_Options,File0, HREF) :-
 pldoc_index:(( is_absolute_file_name(File0),
	insert_alias(File0, File),
	ensure_slash_start(File, SlashFile),
	http_location([path(SlashFile)], Escaped),
	http_location_by_id(pldoc_doc, DocRoot),
        atom_concat(DocRoot, Escaped, HREF))).

doc_file_href(_Options,HREF, HREF).


%%	source_button(+File, +Options)// is det.
%
%	Add show-source button.
:- abolish(pldoc_html:source_button,4).
:- public(pldoc_html:source_button//2).
pldoc_html:source_button(_File, Options) -->
	{ pldoc_html:option(files(_Map), Options) }, !.	% generating files
pldoc_html:source_button(File, _Options) -->
	{show_call((doc_file_href(Options, File, HREF0),
         edit_file_href(Options, File, EDIT_HREF0)))},

	html_write:html([
         a(href(HREF0+[show(src)]),
	       img([ class(action),
		     alt('Show source cOdE'),
		     title('Show source CODE'),
		     src(location_by_id(pldoc_resource)+'source.png')
		   ])),
         a(href(EDIT_HREF0+[]),
	       img([ class(action),
		     alt('Edit source'),
		     title('Edit source'),
		     src(location_by_id(pldoc_resource)+'edit.png')
		   ]))]).


doug_debug(O):-format(user_error,'~nDOUG_DEBUG: ~q.~n',[O]),!.

% :-debug(_).

testml([]):-!. testml([M|L]):-!,testml(M),testml(L). 
testml(M):-atomic(M),!,format('~w',[M]).
testml(nl(E)):-!,ignore((between(0,E,_),nl,fail)).
testml(ML):-phrase(ML,C,[]),testml(C).

/*
:- asserta((http:location(pldoc, root('pldoc'), []))), 
   asserta((http:location(pldoc_resource, root('pldoc'), []) :- pldoc_http:http_location_by_id(pldoc_resource, root('pldoc')))),
   asserta((http:location(pldoc_resource, R, []) :- pldoc_http:http_location_by_id(pldoc_resource, R))).
*/
:- ensure_loaded('../pack/swish/swish').


:- catch(on_signal(hup, _, hup),E,warn(E:on_signal(hup, _, hup))).

hup(_Signal) :-
        thread_send_message(main, stop).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
% :- if_startup_script((http_server(http_dispatch, [ port(3050), workers(16) ]), debug(http_request(_)),debug(cm(_)),debug(swish(_)),debug(storage))).


:- export(do_semweb_startup/0).
do_semweb_startup:-
   predicate_property(mpred_online:semweb_startup,number_of_clauses(N1)),
   forall(clause(mpred_online:semweb_startup,Body,Ref),must(do_ref_job(Body,Ref))),
   predicate_property(mpred_online:semweb_startup,number_of_clauses(N2)),
   ((N2\=N1) -> do_semweb_startup ; true).

% [Optionaly] register swish server (remote file editing)
% TODO :- with_no_mpred_expansions(if_file_exists(ensure_loaded('../pack/swish/logicmoo_run_swish'))).

% [Optionaly] register/run Cliopatria sparql server (remote RDF browsing)
% TODO mpred_online:semweb_startup:-ensure_loaded('run_clio').

% [Optionaly] register/run KnowRob robot services (we use it for the ontology mainly)
% TODO mpred_online:semweb_startup :- with_no_mpred_expansions(if_file_exists(ensure_loaded('../pack/MUD_KnowRob/knowrob_addons/knowrob_mud/prolog/init.pl'))).

% [Optionaly] register/run MILO robot services (we use it for the ontology mainly)
% TODO mpred_online:semweb_startup :- register_ros_package(milo).

% [Optionaly] register/run EulerSharp robot services (we use it for the ontology mainly)
% TODO mpred_online:semweb_startup :- register_ros_package(euler).

% :- ensure_loaded(logicmoo(dbase/mpred_i_pldoc)).
% :- do_semweb_startup.


% [Optionaly] remove debug noises
% mpred_online:semweb_startup:- forall(retract(prolog_debug:debugging(http(X), true, O)),show_call(asserta(prolog_debug:debugging(http(X), false, O)))).
% mpred_online:semweb_startup:- forall(retract(prolog_debug:debugging((X), true, O)),show_call(asserta(prolog_debug:debugging((X), false, O)))).

:-multifile(pre_file_search_path/2).

% user:pre_file_search_path(_,_):-!,fail.


end_of_file.

:-shell('sudo apt-get install npm nodejs-legacy').
:-shell('sudo npm install -g bower').


:-shell('bower install').

/*
#### Download as zip

As installing node and bower is not a pleasure on all operating systems,
you can also download  the  dependencies  as   a  single  zip  file from
Unpack the zip file, maintaining the directory structure, from the swish
root directory to create the directory web/bower_components.
*/
:-shell('wget http://www.swi-prolog.org/download/swish/swish-bower-components.zip').
:-shell('unzip -o swish-bower-components.zip -d . ').
/*

### Get the latest SWI-Prolog

Install the latest  [SWI-Prolog](http://www.swi-prolog.org) _development
version_. As SWISH is very  much  in   flux  and  depends  on the recent
SWI-Prolog pengines and sandboxing libraries, it   is  quite common that
you            need            the             [nightly            build
(Windows)](http://www.swi-prolog.org/download/daily/bin/) or build   the
system    from    the     current      git     development    repository
[swipl-devel.git](https://github.com/SWI-Prolog/swipl-devel).

Nov 6, 2014: release 7.1.26 fully supports the current SWISH.


## Running SWISH

With a sufficiently recent Prolog installed, start the system by opening
`run.pl` either by running `swipl  run.pl`   (Unix)  or opening `run.pl`
from the Windows explorer.

Now direct your browser to http://localhost:3050/

If you want  to  know  what  the   latest  version  looks  like,  go  to
http://swish.swi-prolog.org/


### Running SWISH without sandbox limitations

By default, SWISH lets you only run _safe_  commands. If you want to use
SWISH for unrestricted development, load the authentication module:
*/

 ?- [lib/authenticate].

/*
Next, for first usage, you need  to   create  a user. The authentication
module defines swish_add_user/3, which updates or  creates a file called
`passwd`:
*/

  ?- swish_add_user(guru, 'top secret', []).

/*
If you now try to run a command in  SWISH, it will prompt for a user and
password. After authentication you can run any Prolog predicate.

**NOTE** Authentication uses plain HTTP   basic authentication. Only use
this on trusted networks and do not  use   a  password  that you use for
other sensitive services. If you want to  setup a public server this way
you are strongly adviced to use HTTPS.


## Design

Most of the application is realised  using client-side JavaScript, which
can be found  in  the  directory   `web/js`.  The  JavaScript  files use
[RequireJS](http://requirejs.org/)   for   dependency     tracking   and
[jQuery](http://jquery.com/) for structuring the   JavaScript  as jQuery
plugins. The accompanying CSS is in   `web/css`.  More details about the
organization of the JavaScript is in `web/js/README.md`

There are two overal pages. `web/swish.html`  provides a static page and
`lib/page.pl` provides a Prolog frontend to  generate the overal page or
parts thereof dynamically. The latter   facilitates smoothless embedding
in SWI-Prolog web applications.


## Development and debugging

No building is needed  to  run  the   system  from  sources.  For public
installations you probably want to create   the  minified JavaScript and
CSS files to reduce network traffic and startup time. You need some more
tools for that:
*/
:-shell('npm install -g jsdoc').
:-shell('npm install -g requirejs').
:-shell('npm install -g clean-css').

/*
You also need GNU make installed as   `make`  and SWI-Prolog as `swipl`.
With all that in  place,  the   following  command  creates the minified
versions:
*/

:-shell(make).

/*
The default main page (`/`)  is   generated  from `lib/page.pl`. It uses
minified    JavaScript    and    CSS      from     `web/js/swish-min.js`
`web/css/swish-min.css` when available. If the   minified  files are not
present,  the  server  automatically  includes   the  full  source.  The
generated files may be removed using

    make clean

Alternatively, use of the minified  files   can  be  disable from Prolog
using this command and reloading the page:
*/
%     ?- debug(nominified).



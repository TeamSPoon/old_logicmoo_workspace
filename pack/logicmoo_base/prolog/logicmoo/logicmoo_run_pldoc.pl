#!/usr/local/bin/swipl

:- if(if_defined(load_mud_www)).

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


hup(_Signal) :-
        thread_send_message(main, stop).


:- catch(on_signal(hup, _, hup),error(domain_error(signal, hup), context(system:'$on_signal'/4, _)),dmsg(warn(not_installing_HUP_handler))).

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

:- multifile
	sandbox:safe_primitive/1,		% Goal
	sandbox:safe_meta_predicate/1,		% Name/Arity
	sandbox:safe_meta/2,			% Goal, Calls
	sandbox:safe_global_variable/1,		% Name
	sandbox:safe_directive/1.		% Module:Goal

:-multifile(prolog:sandbox_allowed_clause/1).
prolog:sandbox_allowed_clause(Clause):-nonvar(Clause).
:- abolish(sandbox:safe_primitive,1).
:- multifile(sandbox:verify_predefined_safe_declarations).
:- asserta(sandbox:verify_predefined_safe_declarations).
:- asserta((sandbox:safe_primitive(X):-nonvar(X))),!.
:- asserta((sandbox:safe_primitive(P):-var(P),!,current_predicate(F/A),functor(P,F,A))).
:- asserta((sandbox:safe_primitive(M:P):-var(P),!,current_predicate(M:F/A),functor(P,F,A))).
sandbox:safe_meta_predicate(V):-nonvar(V).
sandbox:safe_meta(V,O):-nonvar(V),nonvar(O).
sandbox:safe_global_variable(V):-nonvar(V).
sandbox:safe_directive(V):-nonvar(V).


:-endif.

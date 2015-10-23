
:- module(logicmoo_run_pldoc,[]).

:- use_module(library(settings)).

:- was_shared_multifile http:location/3.
:- was_dynamic http:location/3.

:- use_module(library(memfile)).
:- use_module(logicmoo_base).
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

:- if(if_defined(ultra_verbose)).
:- prolog_listing:listing(pldoc_http:src_skin/5).
:- endif.

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
	{show_call(why,(doc_file_href(Options, File, HREF0),
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


:- was_export(do_semweb_startup/0).
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
% mpred_online:semweb_startup:- forall(retract(prolog_debug:debugging(http(X), true, O)),show_call(why,asserta(prolog_debug:debugging(http(X), false, O)))).
% mpred_online:semweb_startup:- forall(retract(prolog_debug:debugging((X), true, O)),show_call(why,asserta(prolog_debug:debugging((X), false, O)))).

:- was_shared_multifile(pre_file_search_path/2).

% user:pre_file_search_path(_,_):-!,fail.

:- was_shared_multifile
	sandbox:safe_primitive/1,		% Goal
	sandbox:safe_meta_predicate/1,		% Name/Arity
	sandbox:safe_meta/2,			% Goal, Calls
	sandbox:safe_global_variable/1,		% Name
	sandbox:safe_directive/1.		% Module:Goal

:- was_shared_multifile(prolog:sandbox_allowed_clause/1).

prolog:sandbox_allowed_clause(Clause):-nonvar(Clause).


/*
normal_verify_predefined_safe_declarations :-
        \+ ( clause(safe_primitive(A), _, C),
             \+ ( catch(verify_safe_declaration(A), B, true),
                  (   nonvar(B)
                  ->  clause_property(C, file(D)),
                      clause_property(C, line_count(E)),
                      print_message(error,
                                    bad_safe_declaration(A,
                                                         D,
                                                         E))
                  ;   true
                  )
                )
           ).
*/


:- abolish(sandbox:safe_primitive,1).

% must sneak around pengines security! (we make it dynamic .. but if it loads before we do we have to kill it)
:- abolish(sandbox:verify_predefined_safe_declarations,0).
:- was_shared_multifile(sandbox:verify_predefined_safe_declarations).
:- asserta(sandbox:verify_predefined_safe_declarations).
:- asserta((sandbox:safe_primitive(X):-nonvar(X))),!.
:- asserta((sandbox:safe_primitive(P):-var(P),!,current_predicate(F/A),functor(P,F,A))).
:- asserta((sandbox:safe_primitive(M:P):-var(P),!,current_predicate(M:F/A),functor(P,F,A))).
sandbox:safe_meta_predicate(V):-nonvar(V).
sandbox:safe_meta(V,O):-nonvar(V),nonvar(O).
sandbox:safe_global_variable(V):-nonvar(V).
sandbox:safe_directive(V):-nonvar(V).

:- gripe_time(40,ensure_loaded(logicmoo(mpred_online/mpred_www))),if_defined(mpred_www:ensure_webserver).


end_of_file.




%

:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).

:- was_shared_multifile lmconf:startup_option/2. 
:- was_dynamic lmconf:startup_option/2. 

:- ensure_loaded(logicmoo_utils).

lmconf:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
lmconf:startup_option(clif,sanity). %  Run datalog sanity tests while starting




/*
:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- user:prolog_load_context(directory,Dir),
   %Dir = (DirThis/planner),
   DirFor = logicmoo,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(file_search_path(pack,Y));true).
:- user:attach_packs.
:- initialization(user:attach_packs).

% [Required] Load the Logicmoo Library Utils
% lmconf:mpred_is_impl_file(logicmoo(logicmoo_utils)).

:- user:file_search_path(logicmoo,_)-> true; (user:prolog_load_context(directory,Dir),asserta_if_new(user:file_search_path(logicmoo,Dir))).

:- was_dynamic(lmconf:isa_pred_now_locked/0).
*/

% :- include(mpred/'mpred_header.pi').



/*
:- meta_predicate call_mpred_body(*,0).
:- meta_predicate decl_mpred_hybrid_ilc_0(*,*,0,*).
:- meta_predicate assert_isa_hooked(0,*).
*/
:- meta_predicate t(7,?,?,?,?,?,?,?).
:- meta_predicate t(6,?,?,?,?,?,?).
:- meta_predicate t(5,?,?,?,?,?).
:- meta_predicate t(3,?,?,?).
:- meta_predicate t(4,?,?,?,?).
:- meta_predicate t(2,?,?).


% ========================================
% get_mpred_user_kb/1
% ========================================

% TODO uncomment the next line without breaking it all!
% lmconf:use_cyc_database.

:- asserta(lmconf:pfcManageHybrids).






% ================================================
% Debugging settings
% ================================================

:- was_export(is_stable/0).

is_stable:-fail.

:- if(current_prolog_flag(optimise,true)).
is_recompile.
:- else.
is_recompile:-fail.
:- endif.

fast_mud.
xperimental:-fail.
xperimental_big_data:-fail.

simple_code :- fail.
save_in_mpred_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.
% slow_sanity(A):-nop(A).
:- meta_predicate xtreme_debug(0).
xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, sanity(P).
xtreme_debug(_).

:- meta_predicate sanity(0).
sanity(P):- \+ is_recompile, (true; is_release),!,nop(P).
sanity(P):- on_x_rtrace(hotrace(P)),!.
sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!.
:- meta_predicate(when_debugging(+,0)).
when_debugging(What,Call):- debugging(What),!,Call.
when_debugging(_,_).

% :- asserta(tlbugger:no_colors).
% :- asserta(tlbugger:show_must_go_on).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

% ================================================
% DBASE_T System
% ================================================
:- gripe_time(40,use_module(logicmoo(mpred_online/mpred_www))).
% user:term_expansion((:-module(Name,List)), :-maplist(export,List)):- atom(Name),atom_concat(mpred_,_,Name).
% user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(mpred_,_,Name).



:- asserta(t_l:disable_px).

% user:goal_expansion(ISA,G) :- compound(ISA),t_l:is_calling,use_was_isa(ISA,I,C),to_isa_out(I,C,OUT),G=no_repeats(OUT).
:- meta_predicate(mpred_expander(?,?,?,?)).
:- meta_predicate(lmbase_record_transactions_maybe(?,?)).
:- meta_predicate(mpred_file_expansion(?,?)).


% :- read_source_files.
% logicmoo_html_needs_debug.
:- if((lmconf:startup_option(www,sanity),if_defined(logicmoo_html_needs_debug))).
:- write(ready),nl,flush_output.
:- prolog.
:- endif.
:-  call(with_mfa_of( (dynamic_safe)),user,user,boxlog_to_compile(_D,_E,_F),boxlog_to_compile/3).
:- retractall(t_l:disable_px).


:- list_undefined.

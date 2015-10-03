/** <module> Logicmoo Path Setups
*/
:- if(current_prolog_flag(dialect,yap)).
swi_export(_P):-!.
:- else.
:-module_transparent(swi_export/1).
user:swi_export(P):-export(P).
:-module_transparent(swi_module/2).
user:swi_module(_,_).
:- endif.

:-swi_module(logicmoo_util_all,[if_flag_true/2]).
:- set_prolog_flag(generate_debug_info, true).
%:- set_prolog_flag(access_level,system).

:- meta_predicate agenda_slow_op_enqueue(?).
:- meta_predicate agg_all_test2(?,?,?).
:- meta_predicate asserted_t(5,?,?,?,?,?,?,?,?,?,?,?).
:- meta_predicate call30timed(?,?).
:- meta_predicate call_after(?,?).
:- meta_predicate call_after_next(?,?).

:- meta_predicate call_for_terms(?).
:- meta_predicate call_mpred_body(?,?).
:- meta_predicate call_mpred_body_ilc(?,?).
:- meta_predicate call_whichlist_t(?,?,?).
:- meta_predicate call_with_attvars(1,?).

:- meta_predicate call_proof(?,?).
:- meta_predicate callOr(?,?,?).
:- meta_predicate clr0(?).

:- meta_predicate cnstrn(?,?).
:- meta_predicate cnstrn(?).
:- meta_predicate cnstrn0(?,?).
:- meta_predicate convertAndCall(?,?).
:- meta_predicate ddmsg_call(?).
:- meta_predicate debugCallWhy(?,?).
:- meta_predicate do_all_of_when(?).
:- meta_predicate dumptrace_ret(?).

:- meta_predicate each_subterm(?,2,?).
:- meta_predicate edit1term(?).
:- meta_predicate fmt_ansi(?).
:- meta_predicate forall_setof(?,?).
:- meta_predicate gftrace(?).
:- meta_predicate ggtrace(?).
:- meta_predicate grtrace(?).
:- meta_predicate hmust(?).
:- meta_predicate hmust_l(?).

:- meta_predicate if_html(?,?).
:- meta_predicate if_main(?).
:- meta_predicate if_may_hide(?).
:- meta_predicate in_cmt(0).
:- meta_predicate is_asserted(?).
:- meta_predicate keep_line_pos(?,?).


:- meta_predicate list_retain(?,1,?).
:- meta_predicate logOnErrorFail(0).
:- meta_predicate logOnErrorIgnore(0).
:- meta_predicate logOnFailureIgnore(0).
:- meta_predicate map_subterms(?,?,?).
:- meta_predicate map_unless(?,?,?,?).
:- meta_predicate memberchk_pred(2,?,?).
:- meta_predicate memberchk_pred_rev(2,?,?).
:- meta_predicate must_det(?,?).
:- meta_predicate must_l(0).
:- meta_predicate must_op(?,?).
:- meta_predicate myDebugOnError(?).
:- meta_predicate nd_predsubst(?,2,?).
:- meta_predicate nd_predsubst1(2,?,?,?).
:- meta_predicate nd_predsubst2(2,?,?).
:- meta_predicate(onLoad((?))).
:- meta_predicate pred_delete(2,?,?,?).
:- meta_predicate pred_head(1,?).
:- meta_predicate pred_subst(2,?,?,?,?).
:- meta_predicate pred_term_parts(1,?,?).
:- meta_predicate pred_term_parts_l(1,?,?).
:- meta_predicate predsubst(?,2,?).
:- meta_predicate prepend_each_line(?,?).
:- meta_predicate pretest_call(?).
:- meta_predicate prolog_call(?).

:- meta_predicate really_with_attvars(1,?).

:- meta_predicate return_to_pos(?).
:- meta_predicate search(?,?,?,?,?,?,?).
:- meta_predicate search0(?,?,?,?,?,?,?).
:- meta_predicate search1(?,?,?,?,?,?,?).
:- meta_predicate show_and_do(?).
:- meta_predicate show_cgoal(?).
:- meta_predicate show_edit_term(?,?,?).
:- meta_predicate show_edit_term0(?,?,?).
:- meta_predicate show_edit_term1(?,?,?).
:- meta_predicate show_if_debug(?).
:- meta_predicate simply_functors(2,?,?).
:- meta_predicate term_listing_inner(3,?,?).
:- meta_predicate throw_if_true_else_fail(?,?).
:- meta_predicate to_atomic_name(?,2,?).
:- meta_predicate to_nonvars(2,?,?).
:- meta_predicate to_stderror(?).
:- meta_predicate toCase(2,?,?).
:- meta_predicate toCaseSplit(?,2,?,?).
:- meta_predicate trace_or(?).
:- meta_predicate traceafter_call(?).
:- meta_predicate whenAnd(?,?).
:- meta_predicate with_output_to_stream(?,?).

:- meta_predicate with_current_indent(0).
:- meta_predicate with_dmsg(?,0).
:- meta_predicate with_err_to_pred(?,0).
:- meta_predicate with_fail_is_asserted(?,0).
:- meta_predicate with_input_from_pred(?,0).
:- meta_predicate with_logical_functor(?,?,1).
:- meta_predicate with_main_io(0).
:- meta_predicate with_no_dmsg(?,0).
:- meta_predicate with_output_to_console(0).
:- meta_predicate with_output_to_main(0).
:- meta_predicate with_output_to_pred(?,?,0,0).
:- meta_predicate with_output_to_pred(?,0).

:- user:ensure_loaded((logicmoo_util_filestreams)).
:- user:ensure_loaded((logicmoo_util_filesystem)).
:- user:ensure_loaded((logicmoo_util_term_listing)).

:- dynamic(double_quotes_was/1).
:- multifile(double_quotes_was/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- set_prolog_flag(double_quotes,string).

% have to load this module here so we dont take ownership of prolog_exception_hook/4.
%:- set_prolog_flag(access_level,system).
%:- set_prolog_flag(verbose_autoload, true).
%:- set_prolog_flag(generate_debug_info, true).
% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- user:ensure_loaded(library(ansi_term)).
:- user:ensure_loaded(library(check)).
:- user:ensure_loaded(library(debug)).
:- user:ensure_loaded(library(listing)).
:- user:ensure_loaded(library(lists)).
:- user:ensure_loaded(library(make)).
:- user:ensure_loaded(library(prolog_stack)).
:- user:ensure_loaded(library(system)).

:- meta_predicate if_flag_true(0,0).
:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

%logicmoo_util_all:if_flag_true(Flag,Goal):- catch(Flag,_,fail)->catch(Goal,E,throw(if_flag_true(E)));true.

:-export(if_flag_true/2).
if_flag_true(Flag,Goal):- catch(Flag,E,(dmsg(E:Flag),fail)) -> must(Goal); true.

join_path33(A,B,C):-exists_directory(B)->B=C;directory_file_path(A,B,C).

:-swi_export(with_vars/2).
:-module_transparent(with_vars/2).
:- meta_predicate with_vars(*,0).
with_vars([],Stuff):- !, Stuff.
with_vars([V|Vs],Stuff):- !,
  b_getval('$variable_names', VsOrig),
  append([V|Vs],VsOrig,Temp),
  b_setval('$variable_names', Temp),!,Stuff.
with_vars(_,Stuff):- Stuff.


% this is a backwards compatablity block for SWI-Prolog 6.6.6


% ======================================================
% Save a location of *this* file into logicmoo_runtime_dir/1
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- source_location(File,_Line),
    file_directory_name(File, RunDir),
    retractall(logicmoo_runtime_dir(RunDir)),
    asserta(logicmoo_runtime_dir(RunDir)).



/*
:-
    source_location(File,_Line),
    file_directory_name(File, RunDir),
    atom_concat(RunDir,'/../library',RelDir),
    my_absolute_file_name(RelDir,A),
   'format'(' ~q. ~n',[user:file_search_path(library, A)]),
   asserta(user:file_search_path(library, A)).
*/


:- current_prolog_flag(windows,true)->
   setenv('PATH_INDIGOLOG','../../indigolog');
   setenv('PATH_INDIGOLOG','../../indigolog').



:- user:ensure_loaded((logicmoo_util_bugger_catch)).
:- user:ensure_loaded((logicmoo_util_bugger)).
:- user:ensure_loaded((logicmoo_util_strings)).
:- user:ensure_loaded((logicmoo_util_library)).
:- user:use_module((logicmoo_util_ctx_frame)).
:- user:use_module((logicmoo_util_terms)).
:- user:use_module((logicmoo_util_dcg)).
:- user:use_module((logicmoo_util_coroutining_was)).
:- user:use_module((logicmoo_util_coroutining_iz)).
:- user:ensure_loaded(logicmoo_util_prolog_streams).



/*
win_fork(G,SERVIO,PID):-atom_concat('swipl-win.exe ',G,AC),writeq(win_fork(AC,SERVIO)),nl,
      win_exec(AC,showdefault),PID = 0.

:- current_prolog_flag(windows,true)->
   asserta((fork(G):-win_fork(G)));
   use_module(library(unix)).

% fork(G):-writeq(fork(G)),nl.
*/


% :-getenv('PATH',PATH),writeq(PATH).

% :-prolog.


show_file_search_path:- % 'format'('% ~q.~n',[forall(user:file_search_path(_,_))]),
  forall(must(user:file_search_path(A,B)),'format'('% ~q.~n',[user:file_search_path(A,B)])).

:- if(if_defined(run_sanity_tests)).
:- show_file_search_path.

% :- list_undefined.


:- endif.

:- logicmoo_util_dcg:call(do_dcg_util_tests).

% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:- set_prolog_flag(access_level,user).



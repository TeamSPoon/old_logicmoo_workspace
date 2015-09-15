#! swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/
end_of_file.
:-module(swi_toplevel,[]).
% :- shell(cls).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory,H),absolute_file_name('../../../..',A,[file_type(directory),relative_to(H)]),asserta(user:file_search_path(pack,A)).

:- attach_packs.
:- initialization(attach_packs).


property_pred((table),is_tabled).
property_pred(builtin,is_builtin).
property_pred(never_table,is_never_tabled).
property_pred(old_first,is_old_first).
property_pred(coinductive0,is_coinductive0).
property_pred(coinductive1,is_coinductive1).
property_pred(topl,is_topl).
property_pred(support,is_support).
property_pred(local,is_local).
property_pred((traces),is_tracing).
property_pred(set_default_extension,default_extension).
property_pred(hilog,is_hilog).

:- forall(property_pred(D,F) ,((DG=..[D,_],asserta((DG:-execute_directive(DG)))), 
   ( \+ current_op(_,fy,D) -> op(900,fy,D) ; true),  multifile(F/1),dynamic(F/1))).


% :- multifile sandbox:safe_primitive/1.
% :-asserta((sandbox:safe_primitive(Z):-wdmsg(Z))).

%%% ON :- initialization( profiler(_,walltime) ).
%%% ON :- initialization(user:use_module(library(swi/pce_profile))).

:- user:ensure_loaded(library(ape/get_ape_results)).

:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).

% :- op(1150, fy, (dynamic)).
:- op(900, fy, (dynamic)).
:- op(900, fx, (dynamic)).

:- use_module(library(coinduction),
  	  [ (coinductive)/1,
  	    op(1150, fx, (coinductive))
  	  ]).

retract_all0(R):- ignore((retract(R),fail)).

pf(F):- must(retract_all0(topl(_))),
  to_filename(F,FC),
   must([(FC)]),!.  % ,once(ignore((run_curent_test,sleep(2)))))).

run_curent_test:- show_call_failure(if_defined(go,if_defined(test,if_defined(top)))),!.
run_curent_test:- top(_),!,forall(top(I),time((ignore(show_call((nonvar(I),query(I))))))),!.

:- dynamic(is_clause_module/2).

clause_module(_:Goal,M):-  clause_module0(Goal,M),!.
clause_module(Goal,M):-  clause_module0(Goal,M),!.
clause_module(M:_ ,M):-atom(M).

clause_module0(:- (Goal) ,M):- !, clause_module0(Goal,M).
clause_module0((Goal:- _ ),M):- !, clause_module0(Goal,M).
clause_module0((Goal , _ ),M):- !, clause_module0(Goal,M).
clause_module0(Goal,M):-is_clause_module(Goal,M),!.
clause_module0(Goal,M):-clause_module1(Goal,M),asserta(is_clause_module(Goal,M)),!.

clause_module1(Goal,M):-predicate_property(Goal,imported_from(M)),!.
clause_module1(Goal,M):-current_predicate(_,M:Goal),!.
clause_module1(Goal,M):-source_file(Goal,F), module_property(M,file(F)),!.
clause_module1(Goal,M):-predicate_property(_:Goal,imported_from(M)),!.
clause_module1(Goal,M):-current_module(M),current_predicate(_,M:Goal),!.
clause_module1(Goal,M):-clause(Goal,_,Ref),clause_propery(Ref,module(M)).
clause_module1(Goal,M):-clause(_:Goal,_,Ref),clause_propery(Ref,module(M)).


%% load( + file name ):
%% Initialise, then load a program from this file, processing directives and
%% queries.  After this is done, enter interactive mode.


:-export(load/1).
load( FileName ) :-
     must_det_l((
        setup,
        initialise,                              % provided by a metainterpreter
        process_file( FileName ),!,
        check_general_consistency,
        program_loaded)),!.                          % provided by a metainterpreter


cputime(X):- statistics(cputime,X).

%% process_file( + file name ):
%% Load a program from this file, processing directives and queries.

% :- mode process_file( + ).

do_process_file( FileName ) :-    
        open_the_file( FileName, ProgStream ),
        process_input( ProgStream ),!,
        
        sanity(at_end_of_stream(ProgStream)),        
   % atom_to_memory_file('',Null_stream),
   % file_directory_name(FileName,D),
   stream_property(ProgStream,file_name(FN)),
   load_files(FN,[derived_from(FileName),register(true),stream(ProgStream)]),
        close( ProgStream ),!.

%

current_dirs(DO):-no_repeats(DO,(current_dirs0(D),(atom_concat(DO,'/',D)->true;DO=D))).
current_dirs0(D):- prolog_load_context(directory,D).
current_dirs0(D):- working_directory(D,D).
current_dirs0(D):- current_stream(_,read,Y), stream_property(Y,file_name(FN)), file_directory_name(FN,D).
current_dirs0(D):- stream_property(_,file_name(FN)), file_directory_name(FN,D).
current_dirs0(D):- expand_file_name('*/',X),member(E,X),absolute_file_name(E,D),exists_directory(D).
current_dirs0(D):- expand_file_name('*/*/',X),member(E,X),absolute_file_name(E,D),exists_directory(D).
current_dirs0(D):- expand_file_name('*/*/*/',X),member(E,X),absolute_file_name(E,D),exists_directory(D).
current_dirs0(D):- source_file_property(FN, modified(_)), file_directory_name(FN,D).
current_dirs0('.').

to_filename( FileName, FileName ) :- atomic(FileName),exists_file(FileName),!.
to_filename( FileName, AFN ) :-
 must(default_extension( Ext );Ext='.tlp'), 
 must((current_dirs(D),
     member(TF,[false,true]),
        absolute_file_name(FileName,AFN,[solutions(all),expand(TF),access(read),relative_to(D),file_errors(fail),extensions(['',Ext,'.pl','.tlp','.clp','.P'])]),
        exists_file(AFN))),!.

:-dynamic(is_pred_metainterp/2).
pred_metainterp(Pred,M):- is_pred_metainterp(Pred,M),!.
pred_metainterp(Pred,M):-
    is_tabled(Pred)-> M = is_tabled ;
    is_coinductive1(Pred)-> M = is_coinductive1 ;
    is_support(Pred)-> M = is_support ;
    is_builtin(Pred)-> M = is_builtin ;
    is_never_tabled(Pred)-> M = is_never_tabled.
pred_metainterp(Pred,M):- source_file(Pred,File),is_file_meta(File,M),!.
pred_metainterp(Pred,M):- might_be_clause_meta(Pred)-> M = is_never_tabled. 
pred_metainterp(_   ,unknown).

add_file_meta(FileName,Type):-to_filename(FileName,File),assert_if_new(is_file_meta(File,Type)).

:-add_file_meta('compatibility_utilities_swi',is_builtin).
:-add_file_meta('swi_toplevel',is_builtin).
:-add_file_meta('dra_common',is_builtin).





%% top:
%% Interactive mode.  Each term that is not a directive or a query is treated
%% as an abbreviated query.  After displaying the results of each query read
%% characters upto the nearest newline: if the first character is ";",
%% backtrack to find alternative solutions.
%% Exit upon encountering end of file.
%% NOTE: When running on Sicstus, each term must come on a separate line: after
%%       reading the term the rest of the line is ignored, to facilitate
%%       interaction with the user when asking whether more answers are needed.




:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

user:expand_query(_Goal, _Expanded, _Bindings, _ExpandedBindings):-fail.


:- user:dynamic(expand_answer/2).
:- user:multifile(expand_answer/2).
user:expand_answer(_Goal, _Expanded):-fail.



		/********************************
		*           EXECUTION		*
		********************************/
:-meta_predicate user:dra_execute(0, ?).
:-meta_predicate user:user:residue_vars(0, ?).
:-meta_predicate user:user:dra_execute_goal22(0, ?).

user:dra_execute(Var, _) :-
	var(Var), !,
	print_message(informational, var_query(Var)),
	fail.
user:dra_execute(end_of_file, _) :- !,
	print_message(query, query(eof)).
user:dra_execute(Goal, Bindings) :-
	'$module'(TypeIn, TypeIn),
	'$dwim_correct_goal'(TypeIn:Goal, Bindings, Corrected), !,
	setup_call_cleanup('$set_source_module'(M0, TypeIn),
			   expand_goal(Corrected, Expanded),
			   '$set_source_module'(_, M0)),
	print_message(silent, toplevel_goal(Expanded, Bindings)),
	user:dra_execute_goal22(Expanded, Bindings).
user:dra_execute(_, _) :-
	notrace,
	print_message(query, query(no)),
	fail.

user:dra_execute_goal22(Goal, Bindings) :-
   	'$toplevel':restore_debug,
	user:residue_vars(Goal, Vars),
	deterministic(Det),
	(   '$toplevel':save_debug
	;   '$toplevel':restore_debug, fail
	),
	flush_output(user_output),
	'$toplevel':call_expand_answer(Bindings, NewBindings),
	(    \+ \+ '$toplevel':write_bindings(NewBindings, Vars, Det)
	->   !, fail
	).
user:dra_execute_goal22(_, _) :-
	'$toplevel':save_debug,
	print_message(query, query(no)),
	fail.

user:residue_vars(Goal, Vars) :-
	current_prolog_flag(toplevel_residue_vars, true), !,
	call_residue_vars(query(Goal), Vars).
user:residue_vars(Goal, []) :-
	query(Goal).


user:dra_prompt(Module, BrekLev, Prompt) :-
	current_prolog_flag(toplevel_prompt, PAtom),
	atom_codes(PAtom, P0),
	(    Module \== user
	->   '$toplevel':'$substitute'('~m', [Module, ': '], P0, P1)
	;    '$toplevel':'$substitute'('~m', [], P0, P1)
	),
	(    BrekLev > 0
	->   '$toplevel':'$substitute'('~l', ['[', BrekLev, '] '], P1, P2)
	;    '$toplevel':'$substitute'('~l', [], P1, P2)
	),
	current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
	(    Tracing == true
	->   '$toplevel':'$substitute'('~d', ['[traced] '], P2, P3)
	;    Debugging == true
	->   '$toplevel':'$substitute'('~d', ['[debug] '], P2, P3)
	;    '$toplevel':'$substitute'('~d', [], P2, P3)
	),
	atom_chars(Prompt, P3).

%:- call(user:rl_add_history(ls)).
%:- call(user:rl_add_history('traced,go')).
:- '$toplevel':setup_history.

user:listing_mpred_hook(What):- debugOnError(dra_listing(What)).

dra_listing(What):-get_pi(What,PI),PI\=@=What,!,dra_listing(PI).
dra_listing(Matches):- ignore(dra_listing_0(Matches)),!.

dra_listing_0(MatchesIn):- 
 forall(property_pred(DECLF,DBF),
  (ignore(( DB=..[DBF,Matches],
  clause(DB,true),
  get_functor(Matches,PI0),
  get_functor(MatchesIn,PI1),!,
  PI0==PI1,
  functor(Matches,F,A),
  Decl=..[DECLF,F/A],
  format('~N:- ~q.~n',[Decl]))))).

set_meta(Goal, Prop):- is_pred_metainterp(Goal,Prop),!.
set_meta(Goal, Prop):- functor(Goal,F,A),functor(TGoal,F,A),
  show_call(set_meta0(TGoal, Prop)),!,
  retract_all0(is_pred_metainterp(TGoal,_)),
  asserta_if_new(is_pred_metainterp(TGoal,Prop)).

set_meta0(TGoal,is_builtin):-
    retract_all0(is_tabled(TGoal)),(predicate_property(TGoal,dynamic)->retract_all0((TGoal:-!,query(TGoal)));true),
    retract_all0(is_never_tabled(TGoal)),
    asserta_if_new(is_builtin(TGoal)).

set_meta0(TGoal,is_never_tabled):-
    retract_all0(is_tabled(TGoal)),(predicate_property(TGoal,dynamic)->retract_all0((TGoal:-!,query(TGoal)));true),
    retract_all0(is_builtin(TGoal)),
    asserta_if_new(is_never_tabled(TGoal)).

set_meta0(TGoal,is_tabled):- asserta_if_new(is_tabled(TGoal)),
    retract_all0(is_never_tabled(TGoal)),
    retract_all0(is_builtin(TGoal)),
   (predicate_property(TGoal,dynamic)->asserta_new((TGoal:-!,query(TGoal)));true).
   


:-dynamic(clause_meta/1).
might_be_clause_meta( Goal ):- compound(Goal), \+ \+ (arg(_,Goal,[_|_])),!.


%legal_directive(M:P):-atom(M),M:legal_directive(P).

%legal_directive(P):-compound(P),functor(P,F,1),property_pred(F).


%:-use_module(boot('$toplevel'),[]).
% '$query_loop'/0 
(tprolog) :-   \+ lp_system( eclipse ),!,
    user:with_assertions(op(0,fy,(traced)),   
	(((   current_prolog_flag(break_level, BreakLev)
	->  true
	;   BreakLev = -1
	),
	repeat,
	    (   '$module'(TypeIn, TypeIn),
	       '$toplevel':(((   (stream_property(user_input, tty(true)),write('tprolog '))
		->  user:dra_prompt(TypeIn, BreakLev, Prompt),
		    prompt(Old, '|    ')
		;   Prompt = '', prompt(Old, '') ),
		trim_stacks,
	        '$toplevel':read_query(Prompt, Query, Bindings),
		prompt(_, Old),
		call_expand_query(Query, ExpandedQuery,
				  Bindings, ExpandedBindings)
	    ->  expand_goal(ExpandedQuery, Goal))),
	        (user:dra_execute(Goal, ExpandedBindings),fail) )))), !.



initialize_table:-must(initialise).
print_table_statistics:-print_statistics.
%load(P):-must(prog0(P)),!.

:- user:ensure_loaded(library(dra/tabling3/dra_table_assert)).
%:- user:ensure_loaded(library(dra/tabling3/dra_table_record)).
:- user:ensure_loaded(library(dra/tabling3/compatibility_utilities_swi)).
:- user:ensure_loaded(library(dra/tabling3/dra_common)).
% c + r = 7.949 seconds

/*
% :- pf(library('dra/tabling3/examples/XSB/fib.tlp') ).

:- pf(library('dra/tabling3/examples/co_t.tlp') ).


:- pf(library('dra/tabling3/examples/coind2.tlp') ).
% :- pf(library('dra/tabling3/examples/LTL/v.pl') ).
%:- pf(library('dra/tabling3/examples/mini_graph.tlp') ).
%:- pf(library('dra/tabling3/examples/mini_language.tlp') ).
:- pf(library('dra/tabling3/examples/paper_example.tlp') ).



:- pf(library('dra/tabling3/Bench/tabling3/run')).
:- pf(library('dra/tabling3/Bench/prolog/run')).
:- pf(library('dra/tabling3/Bench/clpfd/run')).
:- pf(library('dra/tabling3/Bench/aspclp/run')).
*/

t0:- time([library('dra/tabling3/examples/XSB/farmer.tlp')]).
tn:- time([library('dra/tabling3/examples/tnot1.tlp')]).
t1:- time(pf(library('dra/tabling3/examples/XSB/farmer.tlp') )),!.
t2:- time([library('dra/tabling3/examples/XSB/ham.tlp')]).
t2a:- time([library('dra/tabling3/examples/XSB/ham_auto.tlp')]).

t2b:- time(pf(library('dra/tabling3/examples/XSB/ham.tlp') )).
t3:- [(library('dra/tabling3/examples/graph.tlp') )].
t4:- pf(library('dra/tabling3/examples/module.tlp') ).
t4:- [(library('dra/tabling3/examples/paper_example.tlp') )].
t4:- pf(library('dra/tabling3/examples/conditional.clp') ).
t4:- pf(library('dra/tabling3/examples/simple1.tlp') ).
t4:- pf(library('dra/tabling3/examples/simple1_old_first.tlp') ).
t4:- pf(library('dra/tabling3/examples/conditional.clp') ).
t4:- pf(library('dra/tabling3/examples/small_comment_example.tlp') ).
t4:- pf(library('dra/tabling3/examples/coind_new.tlp') ).
t5:- consult('/devel/LogicmooDeveloperFramework/PrologMUD/packs/MUD_PDDL/prolog/dra/tabling3/Bench/tabling/tcl.pl').

% :- repeat,logOnErrorIgnore(prolog),fail.
user:term_expansion((?- G),_):- nonvar(G), format(atom(H),'~q .',[G]),user:rl_add_history(H),fail.
% user:goal_expansion(G,_):- G\=(_,_),G\=(_;_),\+predicate_property(G,_),format(atom(H),'~q .',[G]),user:rl_add_history(H),fail.


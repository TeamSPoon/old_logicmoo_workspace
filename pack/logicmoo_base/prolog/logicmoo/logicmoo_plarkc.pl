/* <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :- module(logicmoo_run_old_pttp,[]).

:- ensure_loaded(logicmoo(logicmoo_engine)).
:- asserta_new(user:file_search_path(pldata,'/opt/cyc/')).

/*
:- (current_prolog_flag(qcompile,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(qcompile,PrevValue))),
   set_prolog_flag(qcompile,large).
*/

:- baseKB:disable_mpred_expansion.
:- set_prolog_flag(lm_expanders,false).
:- wdmsg("loading current_renames").
:- load_files(pldata(current_renames),[qcompile(auto)]).
:- wdmsg("done with current_renames").
:- retractall(renames(_)).
:- baseKB:enable_mpred_expansion.
:- set_prolog_flag(lm_expanders,true).

:- ensure_loaded(logicmoo(plarkc/logicmoo_i_cyc_kb)).

:- set_prolog_stack(local, limit(32*10**9)).
:- set_prolog_stack(global, limit(32*10**9)).


:- baseKB:disable_mpred_expansion.
:- set_prolog_flag(lm_expanders,false).
:- if(exists_source(pldata('kb_7166.qlf'))).
:- wdmsg("loading kb_7166").
:- ensure_loaded(pldata('kb_7166.qlf')).
:- else.
:- wdmsg("qcompile kb_7166").
:- load_files(pldata(kb_7166),[qcompile(auto)]).
:- endif.
:- wdmsg("done loading kb_7166").
:- set_module(kb_7166:class(library)).
:- baseKB:enable_mpred_expansion.
:- set_prolog_flag(lm_expanders,true).

:- if(current_predicate(on_fin/1)).
:- forall(call(retract,on_fin(CALL)),call(CALL)).
:- endif.

:- if(current_predicate(setup7166/0)).
:- initialization(setup7166,after_load).
:- initialization(setup7166,restore).
:- endif.


end_of_file.
%
:-in_cmt(listing(cwtdl/3)).
:- dmsg("Loading tinyKB should take under a minute").
:- ltkb1.
:- must((mudSubPart(iExplorer2,Inst),isa(Inst,tHumanNeck))).
:- must((mudSubPart(iExplorer2,Inst),isa(Inst,tHumanHair))).


/*
:- transTiny(Form,(ground(Form),functor(Form,F,1),F\== ~)).

:- set_gui_debug(false).
:- set_no_debug.
:- set_no_debug_thread.

:- transfer_predicate(tinyK8(Form), ( \+ contains_term('$VAR'(_),Form)) , ain((Form))).

:- mpred_trace.

:- set_clause_compile(fwc).

load_later:- cnotrace((transfer_predicate(tinyK8(Form),writeq(Form),ignore(on_x_log_throw(cwtdl(ain(clif(Form)),500,10)))))).

:- mpred_notrace.

:- in_cmt(listing(cwtdl_failed/1)).

*/

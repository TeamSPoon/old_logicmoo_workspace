/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
% ===================================================================
% File 'logicmoo_util_clause_expansion.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_clause_expansion.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2016/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_body_file_scope.pl
:- module(lmfs,
          [
          loading_source_file/1,
          assert_until_eof/2,
          assert_until_eof/1,
          set_prolog_flag_until_eof/2,
          call_on_end_of_module/1, 
          loading_source_file0/1,
          call_on_eof/1, 
          call_on_eof/2, 
          disable_in_file/1,
          enable_in_file/1,
          is_file_enabling/1,  
          signal_eof/2,
          signal_eof/1, 
          signal_eof/0,
          check_skip_id/3,
          did/1,
          vsubst/4,
          contains_f/2,
          contains_eq/2,
          term_expansion_option/3,
          add_did_id/3,
          maybe_eom/0
          ]).

/** <module> Prolog compile-time and runtime source-code "Settings"

 This module specifies a set of settings for files

as they are read from a file before they are processed by the compiler.

The toplevel is expand_clause/2.  This uses other translators:

	* Conditional compilation
	* clause_expansion/2 rules provided by the user

Note that this ordering implies  that conditional compilation directives
cannot be generated  by  clause_expansion/2   rules:  they  must literally
appear in the source-code.


Prolog Flag Scopes:

Module,
File,
Thread.

*/
%:-must(forall(retract(at_eof_action(CALL)),must(CALL))).
% :-must((asserta((user:term_expansion(A,B):-cyc_to_clif_notify(A,B),!),CLREF),asserta(at_eof_action(erase(CLREF))))).

:- if( \+ current_predicate(system:setup_call_cleanup_each/3)).
:- use_module(system:library('logicmoo/util/logicmoo_util_supp.pl')).
:- endif.

:- meta_predicate(call_on_eof(:)).
:- meta_predicate(call_on_eof(+,:)).
:- meta_predicate(assert_until_eof(:)).
:- meta_predicate(assert_until_eof(+,:)).
:- meta_predicate(loading_source_file(-)).
:- module_transparent((
          assert_until_eof/2,
          assert_until_eof/1,
          call_on_end_of_module/1, 
          call_on_eof/1, 
          call_on_eof/2, 
          disable_in_file/1,
          enable_in_file/1,
          is_file_enabling/1,
          loading_source_file/1,
          set_prolog_flag_until_eof/2,
          signal_eof/1, 
          signal_eof/2,
          signal_eof/0,
          check_skip_id/3,
          did/1,
          vsubst/4,
          contains_f/2,
          contains_eq/2,
          term_expansion_option/3,
          add_did_id/3)).


:- thread_local(t_l:pretend_loading_file/1).

%% loading_source_file( ?File) is det.
%
% Loading Source File.
%

loading_source_file(File):- must(((loading_source_file0(File)*-> true; loading_source_file1(File)),nonvar(File))).

:- export(loading_source_file0/1).

% This is the main file to ensure we only process signal_eof directive at the end of the actual source files
loading_source_file0(File):- prolog_load_context(source,File), prolog_load_context(file,File),!.
loading_source_file0(File):- prolog_load_context(source,File),!. % maybe warn the above didnt catch it
loading_source_file0(File):- '$current_source_module'(M),module_property(M, file(File)),prolog_load_context(file, File).
loading_source_file0(File):- '$current_source_module'(M),module_property(M, file(File)),prolog_load_context(source, File).

loading_source_file1(File):- t_l:pretend_loading_file(File).
loading_source_file1(File):- prolog_load_context(file,File).
loading_source_file1(File):- prolog_load_context(source,File),\+ prolog_load_context(file,File).

loading_source_file1(File):- loading_source_file2(File).

loading_source_file2(File):- loading_file(File).
loading_source_file2(File):- '$current_source_module'(M),module_property(M, file(File)).
loading_source_file2(File):- '$current_typein_module'(M),module_property(M, file(File)).


:- dynamic(t_l:eof_hook/3).
:- thread_local(t_l:eof_hook/3).
:- export(t_l:eof_hook/3).


% trace_if_debug:- flag_call(logicmoo_debug > true) -> trace;true.

%% signal_eof() is det.
%
% Do End Of File Actions for current File.
%
signal_eof:- must(signal_eof0),!.
signal_eof0:- prolog_load_context(module,M),prolog_load_context(file,F),signal_eof(M,F),!.
signal_eof0:- prolog_load_context(file,F), signal_eof(F),!.
signal_eof0:- prolog_load_context(module,M),ignore(sanity(M\==user)), ignore(sanity(M\==baseKB)), signal_eof_m(M),!.


%% signal_eof(File) is det.
%
% Do End of file actions queued for File.
%
signal_eof(File):- must(prolog_load_context(module,M)), must(signal_eof(M,File)),!.


%% signal_eof_m(+M) is det.
%
% Do End Of File Actions for Module''s Source File.
%
signal_eof_m(M):- trace_or_throw(must(signal_eof_m0(M))).
% signal_eof_m0(M):- prolog_load_context(source,File), signal_eof(M,File),!.
signal_eof_m0(M):- prolog_load_context(file,File), signal_eof(M,File),!.
signal_eof_m0(M):- must(loading_source_file(File)),signal_eof(M,File).



%% signal_eof_m(+M,+File) is det.
%
% Do End Of File Actions for Module+File.
%
signal_eof(M,File):- module_property(M,file(File)),prolog_load_context(file,File),
    \+ \+ ((module_property(M,file(LittleFile)),LittleFile\==File)),
   must((forall(module_property(M,file(LittleFile)),must(signal_eof_mf_1(M,LittleFile))))),!.
signal_eof(M,File):- ignore(signal_eof_mf_1(M,File)),!.

signal_eof_mf_1(M,File):- must(prolog_load_context(module,M)),
  % dmsg(info(load_mpred_file_complete(M:File))),
   GETTER=t_l:eof_hook(WasM,File,TODO),
   must((forall(clause(GETTER,Body,Ref),(qdmsg(eof_hook(GETTER:-Body)),
        doall((forall(Body,  ((qdmsg(eof_hook(M:on_f_log_ignore(GETTER))),
        show_failure(signal_eof_m(M),M:on_f_log_ignore(WasM:TODO))))))),ignore(erase(Ref)))))),!.
signal_eof_mf_1(M,File):- dmsg(signal_eof_mf_1(M,File)),!.

%% call_on_eof( ?Call) is det.
%
%  Whenever at End Of File execute Call
%
call_on_eof(Call):- loading_source_file(File), call_on_eof(File,Call).
call_on_eof(File,Call):- strip_module(Call,M,P),asserta(t_l:eof_hook(M,File,P)),
   must(M\==logicmoo_util_with_assertions),
   qdmsg(eof_hook(register,M,File,P)).



call_on_end_of_module(Call):- must(prolog_load_context(source,File)),call_on_eof(File,Call).

maybe_eom:- !.

%% assert_until_eof( ?Fact) is det.
%
% Assert Until Eof.
%
assert_until_eof(Fact):- must_det_l((loading_source_file(File),assert_until_eof(File,Fact))).

assert_until_eof(File,Fact):-  
  qdmsg(eof_hook(assert_until_eof,File,Fact)),
  must_det_l((asserta(Fact,Ref),call_on_eof(File,erase(Ref)))).

set_prolog_flag_until_eof(FlagName,Value):- current_prolog_flag(FlagName,Value),!.
set_prolog_flag_until_eof(FlagName,Value):- \+ current_prolog_flag(FlagName,_),!,
       wdmsg(warn(no_previous_value(set_prolog_flag_until_eof(FlagName,Value)))),
       set_prolog_flag(FlagName,Value).

set_prolog_flag_until_eof(FlagName,Value):- 
 (current_prolog_flag(FlagName,PrevValue)->true;PrevValue=unknown_error_value),
 qdmsg(eof_hook(set_prolog_flag_until_eof,FlagName,Value)),
   call_on_eof(set_prolog_flag(FlagName,PrevValue)),
   set_prolog_flag(FlagName,Value).

:- dynamic(lmfs_data:file_option/1).

file_option_to_db(Option,DB):- 
  retractall(lmfs_data:file_option(Option)),
  asserta(lmfs_data:file_option(Option)),
  loading_source_file(File),
  DBP=..[Option,File],
  DB=t_l:DBP,
  ( predicate_property(DB,defined)->true;(thread_local(DB),volatile(DB))).

enable_in_file(Option):- file_option_to_db(Option,DB),assert_if_new(DB),set_prolog_flag_until_eof(Option,true).

disable_in_file(Option):- file_option_to_db(Option,DB),retractall(DB),set_prolog_flag_until_eof(Option,false).

is_file_enabling(Option):- file_option_to_db(Option,DB),call(DB),!, \+ current_prolog_flag(Option,false).

contains_eq(USub,Term):- sub_term(Sub,Term),USub=@=Sub.

contains_f(F,Term):- sub_term(Sub,Term),callable(Sub),functor(Sub,F,_).


check_skip_id(Option,_ ,(did(List),_)):- compound(List),!,member(Option,List).
check_skip_id(_,Head ,Body):- ( \+ compound(Body) ; \+ compound(Head)),!.

add_did_id((did(List),Body),Option,(did(List2),Body)):- compound(List),
   ( memberchk(Option,List)->true;List2=[Option|List] ).
add_did_id(NewBody,Option,(did([Option]),NewBody)).

did(_).

:-meta_predicate(term_expansion_option(:,+,-)).
term_expansion_option(Option,(:-Body),Out):- 
   \+  check_skip_id(Option,(:-Body) ,Body),
   prolog_load_context(module,M),
   M:call(Option,(:-Body),[],Body,NewBody),!,
   Body\=@=NewBody, 
   add_did_id(NewBody,Option,NewBody2),
   user:expand_term((:- NewBody2),Out),dmsg(portray(Out)).

term_expansion_option(Option,(Head:-Body),Out):- 
   \+  check_skip_id(Option,Head ,Body),
   prolog_load_context(module,M),
   M:call(Option,Head,[],Body,NewBody),!,
   Body\=@=NewBody, 
   add_did_id(NewBody,Option,NewBody2),
   user:expand_term((Head:- NewBody2),Out),dmsg(portray(Out)).

vsubst(In,B,A,Out):-var(In),!,(In==B->Out=A;Out=In).
vsubst(In,B,A,Out):-subst(In,B,A,Out).


system:term_expansion(In,Pos,Out,Pos):- nonvar(Pos),compound(In),functor(In,(:-),_),
   lmfs_data:file_option(Option),
   is_file_enabling(Option)->
   term_expansion_option(Option,In,Out),!.

system:term_expansion(EOF,Pos,_,_):- EOF==end_of_file,
  nonvar(Pos),once(signal_eof),
  once(maybe_eom),fail.




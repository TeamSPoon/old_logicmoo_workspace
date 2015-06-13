/* <module>
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% Both A Bottom Up and Top Down Chart Parser
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%      from the book "Natural Language Processing in Prolog"            %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

% :-module(parser_chart89, [chart89/0,test_chart89_regressions/0]).

% :- ensure_loaded(logicmoo(mpred/logicmoo_i_header)).
% :- user:ensure_loaded(library(logicmoo/util/logicmoo_util_bugger)).


:- op(600,xfy,--).
:- op(450,xfy,((:))).
:- op(400,xfy,((&))).
:- op(300,fx,(('`'))).
:- op(200,xfx,((--))).

:- dynamic(use_rule/0).

% :- register_module_type(utility).



/*
:- dynamic_multifile_exported((contains0/2,country/8,city/3,borders/2,in_continent/2)).
:- dynamic_multifile_exported contains/2.
:- dynamic_multifile_exported trans/9.
:- dynamic_multifile_exported det/7.
:- dynamic_multifile_exported sentence/5.
:- dynamic_multifile_exported noun/6.


:- dynamic_multifile_exported latitude/2.
:- dynamic_multifile_exported longitude/2.
:- dynamic_multifile_exported contains/2.
*/
:- style_check(+discontiguous).
:- asserta((thlocal:enable_src_loop_checking)).


:- ensure_loaded('pldata/clex_lexicon').

% ===========================================================
% CHART89 command
% ===========================================================
type_action_info(human_player,chart89(ftListFn(ftTerm)),"Development test CHART-89 Text for a human.  Usage: CHART89 Cant i see the blue backpack?").

agent_call_command(_Gent,chart89([])):- chart89.
agent_call_command(_Gent,chart89(StringM)):-nonvar(StringM), chart89(StringM).  


% ===========================================================
% CHART89 REPL
% ===========================================================
:-thread_local thlocal:chart89_interactive/0.
chart89 :- with_assertions(tracing80,
           with_assertions(thlocal:chart89_interactive,
            with_no_assertions(thlocal:useOnlyExternalDBs,
             with_no_assertions(thglobal:use_cyc_database,
              (told, repeat, prompt_read('CHART89> ',U),  
                            to_word_list(U,WL),((WL==[bye];WL==[end,'_',of,'_',file];(mmake,once(chart89(WL)),fail)))))))).

:- multifile(thlocal:into_form_code/0).
:- asserta(thlocal:into_form_code).

% :- ensure_loaded(logicmoo(parsing/chart89/dcgtrans)).	%  generator
% :- retract(thlocal:into_form_code).

:- ensure_loaded(chart89/buchart2).	% toplevel

%:-swi_export(test_chart89_regressions/0).
test_chart89_regressions:- time((test1,test2)).
:- retract(thlocal:into_form_code).

:- retractall(thlocal:enable_src_loop_checking).


mud_test(chart89_regressions,test_chart89_regressions).

% :- context_module(CM),module_predicates_are_exported(CM).
% :- context_module(CM),quiet_all_module_predicates_are_transparent(CM).
% :- context_module(CM),module_property(CM, exports(List)),moo_hide_show_childs(List).

% :- ensure_loaded(logicmoo('vworld/moo_footer.pl')).


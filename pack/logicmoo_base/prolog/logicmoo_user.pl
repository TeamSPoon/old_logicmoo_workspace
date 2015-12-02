/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- if(('$set_source_module'(CM,CM),'$module'(M,M),logicmoo_user_base:asserta(user_module_uses(M,CM)))).
:- endif.
:- module(logicmoo_user_base,
 [
 fix_ops_for/1,
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-')]).

:- meta_predicate logicmoo_user: ~0.

:- multifile(logicmoo_user_base:user_module_uses/2).
:- dynamic(logicmoo_user_base:user_module_uses/2).

% 	 	 
%% fix_ops_for( ?VALUE1) is semidet.
%
% Fix Oper.s For.
%
fix_ops_for(CM):-
 op(1199,fx,CM:('==>')), 
 op(1190,xfx,CM:('::::')),
 op(1180,xfx,CM:('==>')),
 op(1170,xfx,CM:('<==>')),  
 op(1160,xfx,CM:('<-')),
 op(1150,xfx,CM:('=>')),
 op(1140,xfx,CM:('<=')),
 op(1130,xfx,CM:('<=>')), 
 op(600,yfx,CM:('&')), 
 op(600,yfx,CM:('v')),
 op(350,xfx,CM:('xor')),
 op(300,fx,CM:('~')),
 op(300,fx,CM:('-')).

:- fix_ops_for(user).

:- prolog_ide(debug_monitor).

/*
:- set_prolog_flag(report_error,true).
:- set_prolog_flag(fileerrors,false).
% :- set_prolog_flag(access_level,system).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(debug,true).
% :- set_prolog_flag(gc,false).
:- set_prolog_flag(optimise,false).
:- set_prolog_flag(last_call_optimisation,false).
:- debug.
*/

:- ensure_loaded(library(logicmoo/mpred_online/mpred_www)).
:- system:initialization(mpred_www:ensure_webserver(3040)).
:- mpred_www:ensure_webserver(3040).

:- baseKB:use_module(baseKB:logicmoo/logicmoo_snark).

% in case something changed
:- logicmoo_user_base:user_module_uses(M,CM),!,fix_ops_for(M),fix_ops_for(CM),dmsg(user_module_uses(M,CM)).
:- system:((logicmoo_user_base:user_module_uses(M,CM)->(('$module'(_,M),'$set_source_module'(_,CM)));true)).


:- sanity( \+predicate_property(baseKB:_,exported)).



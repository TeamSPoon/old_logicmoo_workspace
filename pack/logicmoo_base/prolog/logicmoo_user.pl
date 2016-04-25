/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- if(('$set_source_module'(CM,CM),'$current_typein_module'(M),
   multifile(logicmoo_user_base:user_module_uses/2),
   dynamic(logicmoo_user_base:user_module_uses/2),
   logicmoo_user_base:asserta(user_module_uses(M,CM)))).
:- endif.
:- module(logicmoo_user_base,
 [
 fix_ops_for/1,
 user_module_uses/2,
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
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
*/

:- ensure_loaded(library(logicmoo_base)).
% :- ensure_loaded(library(logicmoo/mpred_online/mpred_www)).
:- add_library_search_path('./logicmoo/mpred_online/',[ '*.pl']).
%:- system:initialization(ensure_webserver(3040)).
% ensure_webserver:- ensure_loaded(library(logicmoo/mpred_online/mpred_www)), call(mpred_www:call,ensure_webserver(3040)).

% in case something changed
:- logicmoo_user_base:user_module_uses(M,CM),!,
   fix_ops_for(M),fix_ops_for(CM),dmsg(user_module_uses(M,CM)).

%:- autoload.

:- sanity( \+predicate_property(_,exported)).

:- do_gc.
:- time(load_snark).

:- forall(retract(wsh_w:wrap_shared(F,A,ereq)),ain((arity(F,A),pfcControlled(F),prologHybrid(F)))).

:- system:((logicmoo_user_base:user_module_uses(M,CM)->(('$set_typein_module'(M),'$set_source_module'(CM)));true)).

end_of_file.


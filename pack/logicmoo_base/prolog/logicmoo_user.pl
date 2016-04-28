/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- if(('$set_source_module'(CM,CM),'$current_typein_module'(M),
   multifile(logicmoo_user_file:user_module_uses/2),
   dynamic(logicmoo_user_file:user_module_uses/2),
   logicmoo_user_file:asserta(user_module_uses(M,CM)))).
:- endif.
:- module(logicmoo_user_file,
 [
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


/*
:- set_prolog_flag(report_error,true).
:- set_prolog_flag(fileerrors,false).
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(debug,true).
:- set_prolog_flag(gc,false).
:- set_prolog_flag(gc,true).
:- set_prolog_flag(optimise,false).
:- set_prolog_flag(last_call_optimisation,false).
:- debug.
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
*/

:- ensure_loaded(library(logicmoo_base)).
:- must(add_library_search_path('./logicmoo/mpred_online/',[ '*.pl'])).
%:- system:initialization(ensure_webserver(3040)).

% in case something changed
:- user_module_uses(M,CM),!,
   fix_ops_for(M),fix_ops_for(CM),dmsg(user_module_uses(M,CM)).

%:- autoload.

%:- do_gc.


:-  time((baseKB:ensure_mpred_file_loaded(baseKB:library(logicmoo/pfc/'autoexec.pfc')))).

% :- time(load_snark).

:- forall(retract(wsh_w:wrap_shared(F,A,ereq)),ain((arity(F,A),pfcControlled(F),prologHybrid(F)))).

:- system:((logicmoo_user_file:user_module_uses(M,CM)->(('$set_typein_module'(M),'$set_source_module'(CM)));true)).

end_of_file.


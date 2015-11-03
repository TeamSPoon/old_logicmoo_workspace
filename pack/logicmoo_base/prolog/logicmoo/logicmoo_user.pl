/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- if(('$set_source_module'(CM,CM),'$module'(M,M),asserta(logicmoo_user_base:user_module_uses(M,CM)))).
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
:- endif.

:- multifile(logicmoo_user_base:user_module_uses/2).
:- dynamic(logicmoo_user_base:user_module_uses/2).

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
:- logicmoo_user_base:user_module_uses(M,CM),!,fix_ops_for(M),fix_ops_for(CM),writeln(user_module_uses(M,CM)).

:- baseKB:use_module(baseKB:logicmoo_snark).
%:- ensure_loaded(library(logicmoo/mpred_online/mpred_www)).
%:- system:initialization(mpred_www:ensure_mpred_webserver(3020)).

% in case something changed
:- system:((logicmoo_user_base:user_module_uses(M,CM)->(('$module'(_,M),'$set_source_module'(_,CM)));true)).

:- nop((autoload,scan_source_files_for_varnames)).

:- sanity( \+predicate_property(baseKB:_,exported)).



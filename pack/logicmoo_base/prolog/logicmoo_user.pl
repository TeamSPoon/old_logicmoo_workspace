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

:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
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

:- ensure_loaded(library(logicmoo_base)).

%:- mpred_www:ensure_loaded(library(logicmoo/mpred_online/mpred_www)).
%:- system:initialization(mpred_www:ensure_webserver(3040)).
ws_0:- mpred_www:ensure_loaded(library(logicmoo/mpred_online/mpred_www)), mpred_www:ensure_webserver(3040).

:- baseKB:use_module(baseKB:logicmoo/logicmoo_snark).

% in case something changed
:- logicmoo_user_base:user_module_uses(M,CM),!,fix_ops_for(M),fix_ops_for(CM),dmsg(user_module_uses(M,CM)).
:- system:((logicmoo_user_base:user_module_uses(M,CM)->(('$module'(_,M),'$set_source_module'(_,CM)));true)).

%:- autoload.


:- sanity( \+predicate_property(baseKB:_,exported)).

:- logicmoo_snark:load_snark.

end_of_file.

% Found new meta-predicates in iteration 1 (14.395 sec)
% :- meta_predicate mpred_pfc:foreachl_do(0,*).
% :- meta_predicate mpred_pfc:pfcl_do(0).
% :- meta_predicate mpred_props:decl_mpred(0).
% :- meta_predicate mpred_props:decl_mpred_prolog(0).
% :- meta_predicate mpred_props:decl_mpred_hybrid(0).
% :- meta_predicate mpred_props:decl_mpred_pi(0).
% :- meta_predicate mpred_props:decl_mpred(?,0).
% :- meta_predicate mpred_props:decl_mpred_prolog(?,0).
% :- meta_predicate mpred_props:decl_mpred_hybrid(?,0).
% :- meta_predicate mpred_kb_ops:'__aux_maplist/2_cnstrn0+1'(*,0).
% :- meta_predicate mpred_kb_ops:neg_in_code(0).
% :- meta_predicate baseKB:t(2,?,?).
% :- meta_predicate baseKB:resolverConflict_robot(0).
% :- meta_predicate baseKB:t(4,?,?,?,?).
% :- meta_predicate baseKB:t(3,?,?,?).
% :- meta_predicate mpred_type_constraints:'__aux_maplist/3_map_subterms+1'(*,*,2).
% :- meta_predicate mpred_type_constraints:boxlog_goal_expansion(*,0).
% :- meta_predicate mpred_type_constraints:thaw(0).
% :- meta_predicate logicmoo_util_strings:'__aux_maplist/3_toCase+1'(*,*,2).
% :- meta_predicate logicmoo_util_strings:'__aux_maplist/3_toCaseSplit+2'(*,*,?,2).
% :- meta_predicate mpred_stubs:call_wdmsg(*,*,?,0).
% :- meta_predicate mpred_stubs:ensure_universal_stub_2(*,:,?,0,*).
% :- meta_predicate mpred_stubs:call_for_literal_db(?,0,:).
% :- meta_predicate mpred_stubs:call_for_literal(?,0,:).
% :- meta_predicate mpred_loader:with_umt_l(0).
% :- meta_predicate mpred_type_isa:assert_isa_hooked(0,0).
% :- meta_predicate logicmoo_util_terms:'__aux_maplist/3_maptree+1'(*,*,2).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (8.853 sec)
% :- meta_predicate mpred_props:decl_mpred(*,?,0).
% :- meta_predicate mpred_props:decl_mpred_hybrid_ilc_0(*,*,0,*).
% :- meta_predicate mpred_props:add_mpred_prop_gleaned_4(?,*,*,0).
% :- meta_predicate mpred_props:decl_mpred_0(*,0).
% :- meta_predicate logicmoo_user:t(2,?,?).
% :- meta_predicate logicmoo_user:resolverConflict_robot(0).
% :- meta_predicate logicmoo_user:t(4,?,?,?,?).
% :- meta_predicate logicmoo_user:t(3,?,?,?).
% :- meta_predicate mpred_stubs:ensure_universal_stub_1(*,:,?,0).
% :- meta_predicate mpred_stubs:ensure_universal_stub_plus_HIDE(?,0).
% :- meta_predicate mpred_type_isa:assert_isa_hooked_after(?,0).
% Restarting analysis ...
% Found new meta-predicates in iteration 3 (8.775 sec)
% :- meta_predicate mpred_props:add_mpred_prop_gleaned(?,0).
% Restarting analysis ...
Warning: The predicates below are not defined. If these are defined
Warning: at runtime using assert/1, use :- dynamic Name/Arity.
Warning:
Warning: common_logic_snark:neq/2, which is referenced by
Warning:        /root/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl:462:21: 1-st clause of common_logic_snark:not_mudEquals/2
Warning: mpred_kb_ops:mpred_METACALL/2, which is referenced by
Warning:        /root/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_kb_ops.pl:689:2: 1-st clause of mpred_kb_ops:call_s/1
Warning: mpred_pfc:mpred_provide_storage_clauses/1, which is referenced by
Warning:        /root/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:267:14: 1-st clause of mpred_pfc:mpred_provide_storage_clauses/1v
% Checking trivial failures ...
% Checking redefined system and global predicates ...
% version/1                    Redefined system predicate
% copy_file/2                  Redefined global predicate
% Checking predicates with declarations but without clauses ...
% Checking predicates that need autoloading ...
true.


/** <module> LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(logicmoo_user,
[op(500,fx,'~'),
 op(1199,fx,('==>')),
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'),
 op(1100,fx,('nesc')),
 op(300,fx,'-'),
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(1075,xfx,'<-'),
 op(350,xfx,'xor')]).


:- baseKB:use_module(logicmoo_base).
:- baseKB:thread_local(t_l:user_abox/1).
%:- (t_l:user_abox(M)->true;(source_context_module(M))),set_user_abox(M),dmsg(user_kb=M).
%:- ensure_mpred_system.

/*
:- add_import_module(logicmoo_user,basePFC,end).
:- add_import_module(logicmoo_user,baseKB,end).
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


:-export(checkKB:m1/0).
checkKB:m1:- gripe_time(40,ensure_loaded(logicmoo(mpred_online/mpred_www))),if_defined(mpred_www:ensure_webserver), make,list_undefined.

% :- hook_message_hook.
:- set_prolog_flag(verbose_autoload,false).
:- set_prolog_flag(verbose_load,true).
m9   :-asserta_if_new((user:term_expansion(I,O):- lmbase_expansion(term,user,I,O))).
%m31 :-   (F = mpred/_),foreach(must(lmconf:mpred_is_impl_file(F)),must_det_l((dmsg(list_file_preds(F)),ensure_loaded(F),export_file_preds(F),list_file_preds(F)))).
%m32:- rtrace(ensure_mpred_system).
m33:- must(filematch_ext(['',mpred,ocl,moo,plmoo,pl,plt,pro,p,'pl.in',pfc,pfct],logicmoo_user:pfc/mpred,W)),dmsg(W),!.

%:-export(m2/0).
m2:- ensure_mpred_file_loaded(logicmoo(pfc/relationAllExists)).

% :-pfc_add(((P,Q,z(_))==>(p(P),q(Q)))).
%:-export(m3/0).
m3:- b_setval('$variable_names', ['P'=P,'Q'=Q]), R = (==>((P,Q,z(_)),(p(P),q(Q)))),  renumbervars(write_functor,R,O), writeq(O).

%   b_setval('$variable_names', ['P'=P,'Q'=Q]), R = (==>((P,Q,z(_)),(p(P),q(Q)))), write_term(R,[numbervars(true),protray(_)]),renumbervars_prev(R,O).


%:-export(m4/0).
m4:- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded('../pfc/autoexec.pfc')).

%m3:- make. % w_tl(tlbugger:ifHideTrace,(ensure_mpred_file_loaded(pfc/mpred))).

%m5 :- enable_mpred_system(baseKB).

% :-trace,call((R = (==>((P,Q,z(_)),(p(P),q(Q)))))
% :- use_module(mpred/mpred_loader).

% :- checkKB:m1.

:- baseKB:w_tl(t_l:user_abox(baseKB), baseKB:( checkKB:m1 )).

:- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded(logicmoo(snark/'common_logic_clif.pfc'))).


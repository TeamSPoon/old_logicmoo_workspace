/** <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(logicmoo_snark,[]).


:- thread_local(t_l:user_abox/1).
lh:with_ukb_snark(KB,G):-w_tl(t_l:user_abox(KB),baseKB:G).

%:- use_module(logicmoo_utils).
%:- system:initialization(use_listing_vars).

:- baseKB:ensure_loaded(baseKB:mpred/mpred_loader).
:- add_import_module(baseKB,system,end).
:- initialization(add_import_module(baseKB,system,end)).
:- lh:with_ukb_snark(baseKB,baseKB:use_module(baseKB:logicmoo_base)).
% :- my_loader(ML),system:use_module(ML:logicmoo_base).
%:- (t_l:user_abox(M)->true;(source_context_module(M))),set_user_abox(M),dmsg(user_abox=M).
%:- ensure_mpred_system.

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


:-export(checkKB:m1/0).
checkKB:m1:- gripe_time(40,baseKB:ensure_loaded(baseKB:logicmoo(mpred_online/mpred_www))),if_defined(mpred_www:ensure_webserver), make,list_undefined.

% :- hook_message_hook.
% :- set_prolog_flag(verbose_autoload,false).
% :- set_prolog_flag(verbose_load,true).
% m9   :-asserta_if_new((user:term_expansion(I,O):- lmbase_expansion(term,user,I,O))).
%m31 :-   (F = mpred/_),foreach(must(lmconf:mpred_is_impl_file(F)),must_det_l((dmsg(list_file_preds(F)),ensure_loaded(F),export_file_preds(F),list_file_preds(F)))).
%m32:- rtrace(ensure_mpred_system).
% m33:- must(filematch_ext(['',mpred,ocl,moo,plmoo,pl,plt,pro,p,'pl.in',pfc,pfct],logicmoo_user:pfc/mpred,W)),dmsg(W),!.

%:-export(m2/0).
% m2:- ensure_mpred_file_loaded(logicmoo(pfc/relationAllExists)).

% :-pfc_add(((P,Q,z(_))==>(p(P),q(Q)))).
%:-export(m3/0).
% m3:- b_setval('$variable_names', ['P'=P,'Q'=Q]), R = (==>((P,Q,z(_)),(p(P),q(Q)))),  renumbervars(write_functor,R,O), writeq(O).
%   b_setval('$variable_names', ['P'=P,'Q'=Q]), R = (==>((P,Q,z(_)),(p(P),q(Q)))), write_term(R,[numbervars(true),protray(_)]),renumbervars_prev(R,O).

%:-export(m4/0).


%m5 :- enable_mpred_system(baseKB).

% :- lh:with_ukb_snark(baseKB,baseKB:w_tl(t_l:user_abox(baseKB), baseKB:( checkKB:m1 ))).

:- lh:with_ukb_snark(baseKB,baseKB:ensure_mpred_file_loaded(baseKB:logicmoo(pfc/'autoexec.pfc'))).
% :- lh:with_ukb_snark(baseKB,baseKB:ensure_mpred_file_loaded(baseKB:logicmoo(snark/'common_logic_clif.pfc'))).



/* <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(logicmoo_snark,[]).

:- thread_local(t_l:user_abox/1).

% 	 	 
%% with_ukb_snark( ?VALUE1, ?VALUE2) is semidet.
%
% Hook To [lh:with_ukb_snark/2] For Module Logicmoo_snark.
% Using Ukb Snark.
%
lh:with_ukb_snark(KB,G):-w_tl(t_l:user_abox(KB),baseKB:G).

%:- use_module(logicmoo_utils).
%:- system:initialization(use_listing_vars).

:- add_import_module(baseKB,system,end).
:- initialization(add_import_module(baseKB,system,end)).
% :- lh:with_ukb_snark(baseKB,baseKB:use_module(baseKB:logicmoo_base)).

:-export(checkKB:m1/0).

% 	 	 
%% m1 is semidet.
%
% Hook To [checkkb:m1/0] For Module Logicmoo_snark.
% Module Secondary Helper.
%
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
% m3:- put_variable_names( ['P'=P,'Q'=Q]), R = (==>((P,Q,z(_)),(p(P),q(Q)))),  renumbervars(write_functor,R,O), writeq(O).
%   put_variable_names( ['P'=P,'Q'=Q]), R = (==>((P,Q,z(_)),(p(P),q(Q)))), write_term(R,[numbervars(true),protray(_)]),renumbervars_prev(R,O).

%:-export(m4/0).


%m5 :- enable_mpred_system(baseKB).


:- lh:with_ukb_snark(baseKB,baseKB:ensure_mpred_file_loaded(baseKB:logicmoo(pfc/'autoexec.pfc'))).




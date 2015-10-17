%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 13/07/96   File: hooks_config.pl              %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 13/07/96 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog hooks_config.pl                                           %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(current_prolog_flag(logicmoo_modules,default)).
:- module(xray_hooks_config,[]).
:- endif.

:- no_body_hooks.                      % default is no body hooks

:- no_pred_hooks.                      % default is no predicate hooks


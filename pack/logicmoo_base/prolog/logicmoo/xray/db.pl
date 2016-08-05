%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 24/11/95   File: db.pl                        %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 24/11/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog db.pl                                                     %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if( (false , \+ ((current_prolog_flag(logicmoo_include,Call),Call))) ).
:- module(xray_db,[]).
:- endif.


prolog_clause((Head :- Body),(Head :- Body)) :-
	functor(Head,Pred,_),
	builtin_predicate(Pred),
	!.
prolog_clause(Fact,(Fact:-true)) :-
	functor(Fact,Pred,_),
	Pred \= ':-',
	builtin_predicate(Pred),
	!.
prolog_clause(_,true).

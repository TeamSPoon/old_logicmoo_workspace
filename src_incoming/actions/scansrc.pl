/** <module> 
% Very simple... but kept separate to maintain modularity
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
%
:- module(scansrc, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

moo:action_info(scansrc,"Scan for sourcecode modifed on filesystem and TeamSPoon").

scansrc :- include_moo_files('../src_incoming/*/?*.pl').

:- include(logicmoo(vworld/moo_footer)).

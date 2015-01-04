% any.pl
% Dec 13, 2035
% Douglas Miles
%
% This file defines the basics of dmsg/show/hide

% :-swi_module(user). 
:-swi_module(showdebug, []).

:- include(logicmoo(vworld/moo_header)).

:-decl_type(vtOnOff).

mudIsa(vOn,vtOnOff).
mudIsa(vOff,vtOnOff).

user:term_specifier_text(Text,vtOnOff,ON_OFF):-text_to_string(Text,FLAG),to_on_off(FLAG,vOn,vOff,ON_OFF).

t_f("vOn","vOff").
t_f("t","f").
t_f("true","false").
t_f("on","off").
t_f("1","0").
t_f("show","hide").
t_f("yes","no").
t_f("y","n").

to_on_off(FLAG,ON,OFF,ON_OFF):- t_f(_,FLAG) -> ON_OFF=OFF; ON_OFF=ON.


show_dmsg_values:- listing(tlbugger:dmsg_hidden(_)),current_prolog_flag(opt_debug,ON_OFF),fmt(current_prolog_flag(opt_debug,ON_OFF)).

% dmsg/show/hide
action_info(actDMsg(optional(vtOnOff,vShowValue)),"set the dmsg flag to on/off").
agent_call_command(_Agent,actDMsg(vShowValue)):- !, show_dmsg_values.
agent_call_command(_Agent,actDMsg(ON_OFF)):- !, (ON_OFF==vOff->FLAG=false;FLAG=true),  set_bugger_flag(opt_debug,ON_OFF),fmt(current_prolog_flag(opt_debug,ON_OFF)).

action_info(actShow(optional(prolog,vShowValue)),"show messages of col").
agent_call_command(_Agent,actShow(A)):-!, A==vShowValue -> show_dmsg_values ; bugger:dmsg_show(A).

action_info(actHide(optional(prolog,vShowValue)),"hide messages of col").
agent_call_command(_Agent,actHide(A)):-!, A==vShowValue -> show_dmsg_values ; bugger:dmsg_hide(A).

% debug
verb_alias(debug,dmsg).


:- include(logicmoo(vworld/moo_footer)).

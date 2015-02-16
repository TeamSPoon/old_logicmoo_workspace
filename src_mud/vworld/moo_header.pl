/** <module> 
% All modules are declared here so that this next lines dont have to be pasted into every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands, 
% we add the modules here to not waste 160^2 lines of text and having to not 
% update 160+ files whenever a new module is used
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- multifile user:term_specifier_text/2.
:- multifile user:type_action_info/3.
:- multifile user:update_charge/2.
:- multifile user:update_stats/2.
:- multifile user:use_usable/4.
:- multifile user:verb_alias/2.
:- multifile user:mud_test/0.
:- multifile user:mud_test/1.
:- multifile user:mud_test/2.
:- multifile user:mud_test_local/0.
:- multifile user:mud_test_local/1.
:- multifile user:mud_test_local/2.
:- multifile user:world_agent_plan/3.
:- multifile user:action_info/1.
:- multifile user:action_info/2.
:- multifile user:action_info/3.
:- multifile user:action_rules/4.
:- multifile user:action_verb_useable/4.
:- multifile user:agent_call_command/2.
:- multifile user:agent_text_command/4.
:- multifile user:check_permanence/4.

:- include(logicmoo(dbase/dbase_i_header)).

:-must(not(mpred_prop(dbase_t,prologHybrid))).

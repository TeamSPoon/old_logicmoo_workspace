/** <module> 
% last file loaded per file (loses at any module side effects
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
%
*/


:- (loading_module_h(CM),registered_module_type(utility,CM))->export_all_preds;true.

:- retract(loading_module_h(_)).



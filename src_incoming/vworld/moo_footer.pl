/** <module> 
% last file loaded per file (loses at any module side effects
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
%
*/


%:- moo:loading_module_h(CM), (moo:registered_module_type(utility,CM)->export_all_preds;true).


:- end_transform_moo_preds.

% :- moo:loading_module_h(CM), (context_module(CM) -> retract(moo:loading_module_h(CM)) ; true).
% :- context_module(CM),(moo:registered_module_type(utility,CM))->module_predicates_are_exported(CM);module_predicates_are_exported(CM).
% :- retract(loading_module_h(_)).


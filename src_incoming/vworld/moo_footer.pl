/** <module> 
% last file loaded per file (loses at any module side effects
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
%
*/


:- context_module(CM),(moo:loading_module_h(CM),moo:registered_module_type(utility,CM))->module_predicates_are_exported(CM);module_predicates_are_exported(CM).

:- moo:loading_module_h(CM), (context_module(CM) -> retract(moo:loading_module_h(CM)) ; true).

:- end_transform_moo_preds.





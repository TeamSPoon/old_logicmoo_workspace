/** <module> 
% last file loaded per file (loses at any module side effects
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
%
*/


%:- (moodb:loading_module_h(CM),moodb:registered_module_type(utility,CM))->export_all_preds;true.

%:- retract(moodb:loading_module_h(_)).

:-  moodb:end_transform_moo_preds.





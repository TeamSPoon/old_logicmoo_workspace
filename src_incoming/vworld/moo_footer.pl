
% last file loaded
:- (loading_module_h(CM),registered_module_type(utility,CM))->export_all_preds;true.


:- retract(loading_module_h(_)).


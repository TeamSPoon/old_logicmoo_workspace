/** <module>
% All modules are declared here so that this next lines dont have to be pasted into every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands,
% we add the modules here to not waste 160^2 lines of text and having to not
% update 160+ files whenever a new module is used
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- style_check(+singleton).
:- style_check(+discontiguous).


% :- set_prolog_flag(unknown,fail).
% :- set_prolog_flag(unknown,error).
% :- set_prolog_flag(double_quotes,atom).
:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(verbose_load,true).
% logicmoo vworld mud server


:- context_module(Ctx),
   asserta(moo:loading_module_h(Ctx)),
    'format'('% moo_header: ~q.~n',[loading_module_h(Ctx)]).

:- call((context_module(Ctx),(( may_moo_term_expand(Ctx) -> true; asserta(may_moo_term_expand(Ctx)), 'format'('% moo_header: ~q.~n',[may_moo_term_expand(Ctx)]))))).

:- do_term_expansions -> true; (context_module(Ctx),begin_transform_moo_preds,'format'('% moo_header: begin_transform_moo_preds in ~q.~n',[Ctx])).


end_of_file.



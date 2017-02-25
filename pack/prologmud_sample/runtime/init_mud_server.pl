#!/usr/bin/env swipl
/*  MUD server startup script in SWI-Prolog

*/
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  entry state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(\+ current_module(baseKB)).
:- [logicmoo_repl].
:- threads.
:- set_prolog_flag(logicmoo_qsave,true).
:- else.
:- set_prolog_flag(logicmoo_qsave,true).
:- statistics.
:- endif.


:- '$set_source_module'(baseKB).
:- '$set_typein_module'(baseKB).



loadNewTiny:-
  baseKB:ain((tinyKB(C,_MT,_STR),{tinykb_assertion_recipe(C,CycLOut),delay_rule_eval(CycLOut,tiny_rule,NewAsserts)}
  ==> {dmsg(tiny_clif(NewAsserts))}, tiny_kb(NewAsserts))).

:- set_prolog_flag(do_renames,term_expansion).

:- baseKB:ensure_loaded(logicmoo('snark/common_logic_sumo.pfc')).

:- during_boot(set_prolog_flag(do_renames,restore)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  entry state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(access_level,system).
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETUP SUMO KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumo_ain('$COMMENT'(_)):- !.
sumo_ain(D):- 
    must(kif_assertion_recipe(D,CycLOut)),
    sumo_ain1(CycLOut).

sumo_ain1(documentation(_, xtChineseLanguage,_)).
sumo_ain1(CycLOut):-
    delay_rule_eval(CycLOut,sumo_rule,NewAsserts),
    dmsg(NewAsserts),
    ain(NewAsserts).


loadSumo1:- 
   with_lisp_translation('./games/ontologyportal_sumo/Merge.kif',sumo_ain),
   with_lisp_translation('./games/ontologyportal_sumo/Mid-level-ontology.kif',sumo_ain),
   !.

loadSumo2:- 
   with_lisp_translation('./games/ontologyportal_sumo/Translations/relations-en.txt',sumo_ain),
   with_lisp_translation('./games/ontologyportal_sumo/english_format.kif',sumo_ain),
   with_lisp_translation('./games/ontologyportal_sumo/domainEnglishFormat.kif',sumo_ain),
   !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE SUMO KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- after_boot(loadSumo1).
:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- baseKB:qsave_lm(lm_repl1).
:- endif.

:- after_boot(loadSumo2).
:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- baseKB:qsave_lm(lm_repl2).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE CYC KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- after_boot(loadTiny1).
:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- baseKB:qsave_lm(lm_repl3).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_of_file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




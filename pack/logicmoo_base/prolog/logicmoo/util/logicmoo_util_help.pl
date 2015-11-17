/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_help.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
% File: '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_help.pl'
:- module(logicmoo_util_help,
          [ current_predicate_mfa/3,
            summary_ending/2,
            autodoc_case/2,
            attempt_head_modes/1,
            fixda2/4,
            read_once/5,
            attempt_head_modes_0/3,
            autodoc_output_path/2,
            great_clause/2,
            ignored_assignment/2,
            export_file_preds/0,
            fix_doc_args_for_write/2,
            export_file_preds/1,
            export_file_preds/6,
            export_module_preds/0,
            list_item_per_line/3,
            functor_compare/3,
            notes_to_better_text/2,
            notes_to_better_text/3,
            autodoc_magic/1,
            copy_until_line/2,
            fixup_doc_args/5,       
            fixda/4,
            flatten_text/3,
            helper_name/1,
            list_file_preds/0,
            list_file_preds/1,
            list_file_preds/2,
            autodoc_stream_pred/3,
            longer_sumry/2,
            make_l_summary/3,
            make_module_name/2,
            autodoc_module/1,
            autodoc_stream/3,
            autodoc_stream_data/7,
            make_l_summary_lc/3,
            for_m_make_summary/3,
            make_summary/3,
            module_meta_transparent/1,
            mpred_prolog_only_module/1,
            prefixed_module/4,
            mpred_show_doc/1,
            mpred_show_doc/2,
            mpred_source_file/2,
            merge_mode_and_varname/3,
            no_location/3,
            portray_clause_pi_LR/2,
            portray_clause_pi_UD/2,
            predicate_decl_module/2,
            autodoc_pred/2,
            autodoc_files/0,
            autodoc_file/1,
            scan_and_list_file_preds/1,
            name_to_mode/4,
            mode_to_name/4,
            some_flocation/3,
            some_location/3,
            termw_to_atom/2,
            mpred_type_module/1,
            target_module/2,
            list_item_per_line/4,
            write_modules/0,
            lmconf:sf_known/4,
            attempt_head_vnames/1,
            attempt_head_varnames_0/1,
            helper_name0/1, is_crossfile_module_0/1, make_summary0/3, mpred_source_file_0/2, skip_functor_export_0/1, to_comparable_name_arity/3, to_mfa_0/4
          ]).
:- meta_predicate fixup_doc_args(4,?,?,*,*).
:- meta_predicate notes_to_better_text(2,*,*).
:- meta_predicate list_item_per_line(0,*,*,*),
   list_item_per_line(0,?,?).
:- (multifile lmconf:sf_known/4).
:- (module_transparent current_predicate_mfa/3, summary_ending/2, export_file_preds/0, export_file_preds/1, export_file_preds/6, export_module_preds/0, functor_compare/3, helper_name/1, helper_name0/1, is_crossfile_module_0/1, list_file_preds/0,
  list_file_preds/1, list_file_preds/2, longer_sumry/2, make_l_summary/2, make_module_name/2, module_meta_transparent/1, mpred_prolog_only_module/1, mpred_source_file/2, merge_mode_and_varname/3, no_location/3, portray_clause_pi_LR/2, portray_clause_pi_UD/2, autodoc_pred/2, scan_and_list_file_preds/1, skip_functor_export_0/1, some_flocation/3, some_location/3, target_module/2, to_comparable_name_arity/3, to_mfa_0/4, write_modules/0).
:- export((helper_name0/1, is_crossfile_module_0/1, make_summary0/3, mpred_source_file_0/2, skip_functor_export_0/1, to_comparable_name_arity/3, to_mfa_0/4)).
% :- shared_multifile(lmconf:sf_known/4).

:- dynamic(mpred_prolog_only_module/1).
/*
*/

  :- use_module(library(pldoc)).
  :- use_module(library(http/thread_httpd)).
  :- use_module(library(http/http_parameters)).
  :- use_module(library(http/html_write)).
  :- use_module(library(http/mimetype)).
  :- use_module(library(dcg/basics)).
  :- use_module(library(http/http_dispatch)).
  :- use_module(library(http/http_hook)).
  :- use_module(library(http/http_path)).
  :- use_module(library(http/http_wrapper)).
  :- use_module(library(uri)).
  :- use_module(library(debug)).
  :- use_module(library(lists)).
  :- use_module(library(url)).
  :- use_module(library(socket)).
  :- use_module(library(option)).
  :- use_module(library(error)).
  :- use_module(library(www_browser)).
  :- use_module(library(pldoc/doc_process)).
  :- use_module(library(pldoc/doc_htmlsrc)).
  :- use_module(library(pldoc/doc_html)).
  :- use_module(library(pldoc/doc_index)).
  :- use_module(library(pldoc/doc_search)).
  :- use_module(library(pldoc/doc_man)).
  :- use_module(library(pldoc/doc_wiki)).
  :- use_module(library(pldoc/doc_util)).
  :- use_module(library(pldoc/doc_access)).
  :- use_module(library(pldoc/doc_pack)).

% start a unused server
:- use_module(library(doc_http)).
%:- use_module(library(doc_html)).

:- if(exists_source(library(pldoc))).
:- use_module(library(pldoc), []).
	% Must be loaded before doc_process
:- use_module(library(pldoc/doc_process)).
:- endif.
:- use_module(library(prolog_xref)).

:- doc_collect(true).



%= 	 	 

%% make_module_name( ?P, ?Module) is semidet.
%
% Make Module Name.
%
make_module_name(P,Module):- module_property(Module,file(P)),!.
make_module_name(P,O):-atom(P),!,file_base_name(P,F),file_name_extension(M,_Ext,F),(M\==F->make_module_name(M,O);O=M).
make_module_name(mpred/P,M):-nonvar(P),!,make_module_name(P,M).
make_module_name(util/P,M):-nonvar(P),!,make_module_name(P,M).
make_module_name(P,M):-must(filematch(P,F)),F\=P,!,make_module_name(F,M).

%= 	 	 

%% helper_name0( ?F) is semidet.
%
% Helper Name Primary Helper.
%
helper_name0(F):- atom_chars(F,Chars),append(_,[U,N],Chars), ( char_type(U,digit) ;  char_type(N,digit)), !.


%= 	 	 

%% helper_name( :TermF) is semidet.
%
% Helper Name.
%
helper_name(_:FA):-!,helper_name(FA).
helper_name(F/_):-!,helper_name(F).
helper_name(F):- atom(F), helper_name0(F).


% list_item_per_line(NL,PerLine,List).

%= 	 	 

%% list_item_per_line( :GoalNL, ?PerLine, ?List) is semidet.
%
% List Item Per Line.
%
list_item_per_line(NL,PerLine,List):- list_item_per_line(NL,PerLine,PerLine,List).

%= 	 	 

%% list_item_per_line( :GoalNL, ?PerLine, ?NLeft, ?List) is semidet.
%
% List Item Per Line.
%
list_item_per_line(_,_,_,[]).
list_item_per_line(NL,PerLine,NLeft,List):- NLeft<1,NL,!,list_item_per_line(NL,PerLine,PerLine,List).
list_item_per_line(_,_,_,[E]):-  writeq(E),!.
list_item_per_line(NL,PerLine,NLeft,[E|List]):- NNLeft is NLeft -1, writeq(E),format(','),list_item_per_line(NL,PerLine,NNLeft,List).


%= 	 	 

%% portray_clause_pi_LR( ?T, ?LIST0) is semidet.
%
% Portray Clause Predicate Indicator Lr.
%
portray_clause_pi_LR(_,[]):-!.
portray_clause_pi_LR(T,LIST0):- list_to_set(LIST0,LIST), 
    length(LIST,Len), (Len<8 -> (list_to_conjuncts(LIST,E),P=..[T,E], format('~N',[]), portray_clause( ( :-P )));
    (format(':- ~w (( ',[T]),list_item_per_line(format('~N  ',[]),8,LIST),format('  )).~n'))).
%portray_clause_pi_LR(T,LIST0):-list_to_set(LIST0,LIST),list_to_conjuncts(LIST,E),P=..[T,E], format('~N',[]), portray_clause( ( :-P )),!.


%= 	 	 

%% portray_clause_pi_UD( ?T, ?LIST0) is semidet.
%
% Portray Clause Predicate Indicator Ud.
%
portray_clause_pi_UD(_,[]):-!.
portray_clause_pi_UD(T,LIST0):-list_to_set(LIST0,LIST),list_to_conjuncts(LIST,E), format('~N :- ~q % ',[T]), portray_clause( ( cmt:-E )),!.


%= 	 	 

%% to_comparable_name_arity( ?P, ?VALUE2, ?A) is semidet.
%
% Converted To Comparable Name Arity.
%
to_comparable_name_arity(P,_,A):-var(P),!,integer(A).
to_comparable_name_arity(_-FA,F,A):-!,to_comparable_name_arity(FA,F,A).
to_comparable_name_arity(_:FA,F,A):-!,to_comparable_name_arity(FA,F,A).
to_comparable_name_arity(F/A,F,A):-!.
to_comparable_name_arity(P,F,A):-functor(P,F,A).


%= 	 	 

%% functor_compare( ?R, ?P1, :TermP2) is semidet.
%
% Functor Compare.
%
functor_compare(R,(_-C1),(_-C2)):- nonvar(C1),nonvar(C2),functor_compare(R,C1,C2).
functor_compare(R,M1:P1,M2:P2):- M1==M2,!,functor_compare(R,P1,P2).
functor_compare(R,M1:P1,M2:P2):- compare(FR,M1,M2),!,(FR\==(=)->R=FR;functor_compare(R,P1,P2)).
functor_compare(R,_:_,_):- R=(>).
functor_compare(R,_,_:_):- R=(<). 
functor_compare(R,P1,P2):- logicmoo_util_help:to_comparable_name_arity(P1,F1,A1),logicmoo_util_help:to_comparable_name_arity(P2,F2,A2),!,compare(FR,F1,F2),(FR\==(=)->R=FR;compare(R,A1,A2)).



%= 	 	 

%% list_file_preds is semidet.
%
% List File Predicates.
%
list_file_preds:- source_location(S,_),list_file_preds(S).



%= 	 	 

%% skip_functor_export_0( :TermARG1) is semidet.
%
% skip functor export  Primary Helper.
%
skip_functor_export_0('$load_context_module'/3).
skip_functor_export_0('$included'/_).
skip_functor_export_0('$pldoc'/_).


%= 	 	 

%% is_crossfile_module_0( ?VALUE1) is semidet.
%
% If Is A crossfile module  Primary Helper.
%
is_crossfile_module_0(lmconf).
is_crossfile_module_0(baseKB).
is_crossfile_module_0(basePFC).
is_crossfile_module_0(user).


%= 	 	 

%% to_mfa_0( ?VALUE1, ?VALUE2, :TermSM, :TermSM) is semidet.
%
% Converted To Module-Functor-Arity  Primary Helper.
%
to_mfa_0(_,user,SM:FA,SM:FA):-!.
to_mfa_0(_,_,SM:FA,SM:FA):- is_crossfile_module_0(SM),!.
to_mfa_0(_,PM,_:FA,PM:FA):- is_crossfile_module_0(PM),!.
to_mfa_0(FM,PM,SM:F/A, SM:F/A):- PM = SM,PM = FM, functor(P,F,A) , (predicate_property(SM:P,thread_local);predicate_property(SM:P,multifile);predicate_property(SM:P,dynamic)),!.
to_mfa_0(FM,PM,SM:FA, FA ):- PM = SM,PM = FM,!.
to_mfa_0(FM,PM,SM:F/A, SM:F/A):- FM == PM,PM\==SM, functor(P,F,A), predicate_property(SM:P,multifile),!.
to_mfa_0(FM,PM,SM:FA, FA):- FM == PM,PM\==SM, !.
to_mfa_0(_,_,SM:FA,SM:FA):-!.



%= 	 	 

%% scan_and_list_file_preds( ?F) is semidet.
%
% Scan And List File Predicates.
%
scan_and_list_file_preds(F):- forall(filematch(F,S),((make_module_name(S,FM),ensure_loaded(S),export_file_preds(S),list_file_preds(S,FM)))).

%= 	 	 

%% list_file_preds( ?F) is semidet.
%
% List File Predicates.
%
list_file_preds(F):- forall(filematch(F,S),((make_module_name(S,FM),ensure_loaded(S),list_file_preds(S,FM)))).

%= 	 	 

%% list_file_preds( ?S, ?FM) is semidet.
%
% List File Predicates.
%
list_file_preds(S,FM):-
 must_det_l((
   findall(((MP)-(MFA)),(mpred_source_file(M:P,S),functor(P,F,A), \+ skip_functor_export_0(F/A),predicate_module(M:P,R),to_mfa_0(FM,R,M:F/A,MFA),to_mfa_0(FM,R,M:P,MP)),List),
   predsort(functor_compare,List,Set),
   findall(MFA, (member(MP-MFA,Set), \+ helper_name(MFA)),Exports),
   findall(MFA, (member(MP-MFA,Set), helper_name(MFA)),Non_Exports),
   findall(MFA, (member(MP-MFA,Set),predicate_property(MP,multifile)),Multifile),
   format('~N~n~n% File: ~w ~n',[S]),
   findall(META,(member(MP-MFA,Set),predicate_property(MP,meta_predicate(META))),MPList),
   % findall(MFA, (member(MP-MFA,Set),predicate_property(MP,transparent),\+ predicate_property(MP,meta_predicate(_))),Transparent),
   findall(MFA, (member(MP-MFA,Set),predicate_property(MP,transparent)),Transparent),
   findall(MFA, (member(MP-MFA,Set),predicate_property(MP,thread_local)),ThreadLocal),
   findall(MFA, (member(MP-MFA,Set),predicate_property(MP,dynamic),\+ predicate_property(MP,thread_local)),Dynamic),   
   findall(MFA, (member(MP-MFA,Set),predicate_property(MP,volatile)),Volatile),
   format('~N:- module(~q, [~n',[FM]),   
   list_item_per_line(format('~N  ',[]),8,Exports),
   list_item_per_line(format('~N    ',[]),20,Non_Exports),
   format('  )).~n'),
   portray_clause_pi_UD( meta_predicate,MPList),
   portray_clause_pi_LR( multifile,Multifile),
   portray_clause_pi_LR( module_transparent,Transparent),
   portray_clause_pi_LR( thread_local,ThreadLocal),subtract(Dynamic,ThreadLocal,DynamicL),   
   portray_clause_pi_LR( export,Non_Exports),
   portray_clause_pi_LR( dynamic,Dynamic),
   portray_clause_pi_LR( shared_multifile,DynamicL),
   portray_clause_pi_LR( volatile,Volatile))),!.
   



:-export(module_meta_transparent/1).
% = :- meta_predicate(module_meta_transparent(:)).

%= 	 	 

%% module_meta_transparent( :TermM) is semidet.
%
% Module Meta Transparent.
%
module_meta_transparent(M:F/A):-must(functor(P,F,A)),!,module_meta_transparent(M:P).
module_meta_transparent(M:P):-predicate_property(M:P,meta_predicate(_)),!.
module_meta_transparent(M:P):-predicate_property(M:P,transparent),!.
module_meta_transparent(M:P):-functor(P,F,A),module_transparent(M:F/A),!. % ground(P),M:meta_predicate(P),!.
% module_meta_transparent(M:P):-P=..[_|Args],maplist('='(?),Args),module_meta_transparent(M:P).
module_meta_transparent(_).


:-multifile(lmconf:sf_known/4).
:-dynamic(lmconf:sf_known/4).
:- export(mpred_source_file_0/2).

%= 	 	 

%% mpred_source_file( :TermM, ?S) is semidet.
%
% Managed Predicate Source File.
%
mpred_source_file(M:P,S):- !, source_file(M:P,S).
mpred_source_file(M:P,S):- no_repeats(mpred_source_file_0(M:P,S)).

%= 	 	 

%% mpred_source_file_0( :TermM, ?S) is semidet.
%
% Managed Predicate source file  Primary Helper.
%
mpred_source_file_0(M:P,S):- !, source_file(M:P,S). % ,once((to_comparable_name_arity(P,F,A))),nop(assert_if_new(lmconf:sf_known(S,F,A,M))).
mpred_source_file_0(M:P,S):- current_module(M),predicate_property(M:P,file(S)),\+ predicate_property(M:P,imported_from(_)). % to_comparable_name_arity(P,F,A).
mpred_source_file_0(M:P,S):- var(P)-> (lmconf:sf_known(S,F,A,M),functor(P,F,A)) ; (functor(P,F,A),lmconf:sf_known(S,F,A,M)).

% Return the correct M for the F/A

%= 	 	 

%% current_predicate_mfa( ?M, ?F, ?A) is semidet.
%
% Current Predicate Module-functor-arity.
%
current_predicate_mfa(M,F,A):-atom(F),integer(A),!,no_repeats(M:F/A,((functor(P,F,A),current_predicate(_,M:P),\+ predicate_property(M:P,imported_from(_))))).
current_predicate_mfa(M,F,A):-no_repeats(M:F/A,((current_predicate(_,M:P),functor(P,F,A),\+ predicate_property(M:P,imported_from(_))))).


%= 	 	 

%% no_location( ?M, ?F, ?A) is semidet.
%
% No Location.
%
no_location(M,F,A):-current_predicate_mfa(M,F,A),\+ lmconf:sf_known(_S,F,A,_MN).


%= 	 	 

%% some_location( ?M, ?F, ?A) is semidet.
%
% Some Location.
%
some_location(M,F,A):-no_repeats(F/A,(( current_predicate_mfa(M,F,A); lmconf:sf_known(_S,F,A,_MN)))).

%= 	 	 

%% some_flocation( ?FM, ?F, ?A) is semidet.
%
% Some Flocation.
%
some_flocation(FM,F,A):-no_repeats(F/A,(( lmconf:sf_known(_S,F,A,FM);current_predicate_mfa(FM,F,A)))).


:- module_transparent(export_file_preds/1).
:- export(export_file_preds/0).

%= 	 	 

%% export_file_preds is semidet.
%
% Export File Predicates.
%
export_file_preds:- source_location(S,_),export_file_preds(S),!.
:- export(export_file_preds/1).

%= 	 	 

%% export_file_preds( ?FileMatch) is semidet.
%
% Export File Predicates.
%
export_file_preds(_):- current_prolog_flag(xref,true),!.
export_file_preds(FileMatch):- forall(must(filematch(FileMatch,File)),
 (source_context_module(NotUser),show_call(why,NotUser\==user),
   forall(must(mpred_source_file(M:P,File)),(functor(P,F,A),must(export_file_preds(NotUser,File,M,P,F,A)))))).


%= 	 	 

%% predicate_decl_module( ?Pred, ?RM) is semidet.
%
% Predicate Declare Module.
%
predicate_decl_module(Pred,RM):-current_predicate(_,RM:Pred),\+ predicate_property(RM:Pred,imported_from(_)),must(RM\==user).


:- style_check(-singleton).

:- dynamic(lmconf:mpred_is_impl_file/1).
:- multifile(lmconf:mpred_is_impl_file/1).
:- volatile(lmconf:mpred_is_impl_file/1).

:- if(false).
:- else.
:- endif.


%= 	 	 

%% write_modules is semidet.
%
% Write Modules.
%
write_modules:- forall(lmconf:mpred_is_impl_file(F),(export_file_preds(F),list_file_preds(F))).



%= 	 	 

%% export_file_preds( ?NotUser, ?S, ?VALUE3, ?P, ?F, ?A) is semidet.
%
% Export File Predicates.
%
export_file_preds(NotUser,S,_,P,F,A):-current_predicate(logicmoo_varnames:F/A),!.
export_file_preds(NotUser,S,system,P,F,A):-current_predicate(system:F/A),!.
export_file_preds(NotUser,S,user,P,F,A):-current_predicate(system:F/A),!.
export_file_preds(NotUser,S,M,P,F,A):- M==user,!,trace,show_call(why,export_file_preds(NotUser,S,NotUser,P,F,A)).
export_file_preds(NotUser,S,M,P,F,A):- predicate_decl_module(P,RM),RM\==M,!,export_file_preds(NotUser,S,RM,P,F,A).
%export_file_preds(NotUser,S,M,P,F,A):- \+ helper_name(F), export(M:F/A), fail.
export_file_preds(NotUser,S,M,P,F,A):- M:export(M:F/A), fail. % export anyways
export_file_preds(NotUser,S,M,P,F,A):- M:module_transparent(F/A), fail.
% export_file_preds(NotUser,S,M,P,F,A):- module_meta_transparent(M:F/A),fail.
export_file_preds(NotUser,S,M,P,F,A):- must(predicate_property(M:P,transparent)).

:- style_check(+singleton).

:- module_transparent(export_module_preds/0).
:- export(export_module_preds/0).

%= 	 	 

%% export_module_preds is semidet.
%
% Export Module Predicates.
%
export_module_preds:- current_prolog_flag(xref,true),!.
export_module_preds:- source_context_module(M),source_file_property(S,module(M)),export_file_preds(S),forall(source_file_property(S,includes(F,_)),export_file_preds(F)).


:- use_module(library(pldoc/doc_pack)).


%= 	 	 

%% merge_mode_and_varname( ?ModeAs, ?NameAs, ?ModeAs) is semidet.
%
% Merge Pred Mode And Varname.
%
merge_mode_and_varname(ModeAs,NameAs,ModeAs:NameAs).


:- dynamic(lmconf:mpred_is_impl_file/1).
:- multifile(lmconf:mpred_is_impl_file/1).
:- volatile(lmconf:mpred_is_impl_file/1).


%= 	 	 

%% target_module( ?P, ?M) is semidet.
%
% Target Module.
%
target_module(P,M):-mpred_source_file(M:P,F),lmconf:mpred_is_impl_file(F),make_module_name(F,M).

% autodoc_output_path(_,user):- !.

%= 	 	 

%% autodoc_output_path( ?File, ?PlDocFile) is semidet.
%
% Autodoc Output Path.
%
autodoc_output_path(File,PlDocFile):- 
   atom_subst(File,'/prolog','/pldoc',PlDocFile0),
   (PlDocFile0\==File-> PlDocFile=PlDocFile0 ; 
       (atom_subst(PlDocFile0,'/logicmoo_base/','/logicmoo_base_docs/',PlDocFile1),
         (PlDocFile1\==PlDocFile0->PlDocFile=PlDocFile1;PlDocFile=user))),
   (PlDocFile == user -> true ;(file_directory_name(PlDocFile,Dir),make_directory_path(Dir))),!.

% autodoc_file(library(logicmoo_user)). 
%  list_file_preds(library(logicmoo/util/logicmoo_util_bb_env)).


%= 	 	 

%% autodoc_file( :TermFile) is semidet.
%
% Autodoc File.
%
autodoc_file(File):- var(File),!,no_repeats(varname_cache:varname_info_file(File)),autodoc_file(File).
autodoc_file(*):-!, autodoc_files.
autodoc_file(module(M)):- !,autodoc_module(M).
autodoc_file(File):- ( \+ atom(File) ; \+ exists_file(File) ), !,
   forall(must(filematch(File,E)),autodoc_file(E)).



autodoc_file(File):- 
  scan_for_varnames,
  use_listing_vars,
  setup_call_cleanup(
   tell(user_error),
  (( show_call(_,autodoc_output_path(File,PlDocFile)),
   read_source_file_vars(File),
   retractall(t_l:last_predicate_help_shown(_,_,_)),
   retractall(t_l:last_source_file_help_shown(File,_,_)),
w_tl(set_prolog_flag(xref,false),
 w_tl(t_l:disable_px,
 must_det_l((
   make_module_name(File,M),
   M:convert_to_dynamic(M,'$pldoc',4),
   setup_call_cleanup(tell(PlDocFile),
    setup_call_cleanup(
        open(File,read,LineByLineStream),
	setup_call_cleanup(
	    prolog_open_source(File, In),
	    autodoc_stream(LineByLineStream,File,In),
	    prolog_close_source(In)),
        (copy_until_line(LineByLineStream,999999999999), close(LineByLineStream))),told))))))),told),!.


%= 	 	 

%% autodoc_module( ?M) is semidet.
%
% Autodoc Module.
%
autodoc_module(M):- 
    doall((      
      source_file(M:P,File), \+ predicate_property(M:P,imported_from(_)),
      ignore(predicate_property(M:P,line_count(Start))),
      must((autodoc_stream_data(_,M, File ,Start-_,P,P,_Vs))))).



read_once(In, Term, Expanded, Vs,TermPos):- (prolog_read_source_term(In, Term, Expanded, [ variable_names(Vs), syntax_errors(error) , term_position(TermPos) ])).


%% autodoc_stream( ?LineByLineStream, ?File, ?In) is semidet.
%
% Autodoc Stream.
%
autodoc_stream(LineByLineStream,File,In):-
  make_module_name(File,M),
	repeat,          
	  catch((           
            stream_property(In,position(Pos)),
            read_once(In, Term, Expanded, Vs,TermPos)),
		E,(call((nop(set_stream_position(In,Pos)),dmsg(E),notrace,trace,rtrace(read_once(In, Term, Expanded, Vs,TermPos)))),fail)),
          stream_position_data(line_count, TermPos, Start),
          line_count(In,End),
	(   Term == end_of_file
	-> (copy_until_line(LineByLineStream,999999999999), !) ; 
          (  must(autodoc_stream_data(LineByLineStream,M, File,Start-End,Term, Expanded,Vs)),
             fail)).



%= 	 	 

%% autodoc_files is semidet.
%
% Autodoc Files.
%
autodoc_files:-
      make,
      autoload,
      make,
      scan_for_varnames,!,
      doall((
       no_repeats(varname_cache:varname_info_file(File)),
       once(atom_contains(File,'logicmoo')),
       must_det_l(autodoc_file(File)))).


%= 	 	 

%% copy_until_line( ?Src, ?VALUE2) is semidet.
%
% Copy Until Line.
%
copy_until_line(Src,_):- ( \+ is_stream(Src);at_end_of_stream(Src)),!.
copy_until_line(Src,LineNo):-
   repeat,
    (at_end_of_stream(Src) -> ! ;
    (line_count(Src,CurrentLinePos),
    (CurrentLinePos>=LineNo -> ! ;
      (echo_one_line_or_skip_autodoc(Src),fail)))).


%% autodoc_magic( ?Starter) is semidet.
%
% Autodoc Magic Recogition (only the first is used in generation)
%
autodoc_magic(Starter):- Starter = "\n%= \t \t \n\n".
autodoc_magic(Starter):- Starter = "\n% \t \t \n".


%= 	 	 

%% echo_one_line_or_skip_autodoc( ?Src) is semidet.
%
% Echo One Line Or Skip Autodoc.
%
echo_one_line_or_skip_autodoc(Src):- at_end_of_stream(Src),!.
echo_one_line_or_skip_autodoc(Src):- 
  autodoc_magic(Find),string_length(Find,L),
  peek_string(Src,L,Peeked),Peeked=Find,!,
  read_line_to_codes(Src,_),!,
  repeat,
  read_line_to_codes(Src,_),
  peek_string(Src,1,Was),
  Was \== "%",!.
echo_one_line_or_skip_autodoc(Src):- read_line_to_codes(Src,Codes),format('~s~n',[Codes]).

:-thread_local(t_l:file_loc/1).


%= 	 	 

%% autodoc_stream_data( ?Src, ?M, ?File, :TermFrom_To, ?Was, ?Expanded, ?Vs) is semidet.
%
% Autodoc Stream Data.
%
autodoc_stream_data(Src,M, File,From_To,(Was :- _ ),Expanded,Vs):-  nonvar(Was),!,
           autodoc_stream_data(Src,M, File,From_To,Was,Expanded,Vs).
autodoc_stream_data(Src,M, File,From_To,(Was --> _ ),Expanded,Vs):-  nonvar(Was),!,
           autodoc_stream_data(Src,M, File,From_To,Was,Expanded,Vs).
autodoc_stream_data(_Src,_M, _File,_From_To,(:- Was ),_Expanded,_Vs):- nonvar(Was),!.
autodoc_stream_data(Src,M, File,FromLine1-_EndingLine,Term,_Expanded,_Vs):-
   FromLine is FromLine1-0,
   strip_module(Term,_MU,PI),
   get_functor(PI,F,A),
   w_tl(t_l:file_loc(File:FromLine),
     ( copy_until_line(Src,FromLine), autodoc_stream_pred(FromLine,File,M:F/A)
       )),!.

:- dynamic(t_l:last_source_file_help_shown/3).
:- dynamic(t_l:last_predicate_help_shown/3).


%= 	 	 

%% autodoc_stream_pred( ?FromLine, ?File, :TermP) is semidet.
%
% Autodoc Stream Predicate.
%
autodoc_stream_pred(_,File,M:F/A):- t_l:last_source_file_help_shown(File,M:F/A,_),!.
autodoc_stream_pred(FromLine,File,M:F/A):- !,asserta(t_l:last_source_file_help_shown(File,M:F/A,FromLine)),
                  functor(P,F,A),must(autodoc_pred(M,M:P)),!.
autodoc_stream_pred(FromLine,File,M:P):-!,get_functor(P,F,A),autodoc_stream_pred(FromLine,File,M:F/A).
autodoc_stream_pred(FromLine,File,P):-!,get_functor(P,F,A),autodoc_stream_pred(FromLine,File,_M:F/A).



%= 	 	 

%% autodoc_pred( ?M, :TermM) is semidet.
%
% Autodoc Predicate.
%
autodoc_pred(M,M:P0):- once(to_comparable_name_arity(P0,F,A)),functor(P,F,A), M\==baseKB , (predicate_property(baseKB:P,_),\+predicate_property(baseKB:P,imported_from(_))),!.
autodoc_pred(M,M:P0):- once(to_comparable_name_arity(P0,F,A)), clause(M:'$pldoc'(F/A, _, S, D),true),S\==D,!.
autodoc_pred(_,_:end_of_file):-!.
autodoc_pred(M,M:P0):- t_l:last_predicate_help_shown(M,_,P0),!.
autodoc_pred(M,M:P0):- 
 asserta(t_l:last_predicate_help_shown(M,M,P0)),
 must_det_l((
   once(to_comparable_name_arity(P0,F,A)),
   functor(NameH,F,A),NameH=..[F|NameAs],   
   functor(ModeH,F,A),ModeH=..[F|ModeAs],
   functor(NameM,F,A),NameM=..[F|NameAsM],   
   functor(ModeM,F,A),ModeM=..[F|ModeAsM],
   functor(NameO,F,A),NameO=..[F|NameAsO],   
   functor(ModeO,F,A),ModeO=..[F|ModeAsO],
   lock_vars(NameAs),
   all_different_vars(['$VAR'('_')|NameAs]),
   attempt_head_vnames(M:NameH),

   unlock_vars(NameAs),
   attempt_head_modes(M:ModeH),
   functor(P,F,A),
   functor(DocH,F,A),DocH=..[F|DocAs],
   functor(DocM,F,A),DocM=..[F|DocAsM],
   functor(DocO,F,A),DocO=..[F|DocAsO],
   maplist(merge_mode_and_varname,ModeAs,NameAs,DocAs),
   maplist(merge_mode_and_varname,ModeAsM,NameAsM,DocAsM),
   maplist(merge_mode_and_varname,ModeAsO,NameAsO,DocAsO),
   
   all_different_vars(['$VAR'('_')|NameAsM]),
   all_different_vars(['$VAR'('_')|NameAsO]),
 
   
   fixup_doc_args(fixda,P,1,DocAs,DocAsM),

   % maplist(ignored_assignment(''),ModeAs),
   
   fixup_doc_args(fixda,P,1,DocAsM,DocAsO),

   fixup_doc_args(fixda2,P,1,DocAsM,DocAsO),

   maplist(fix_doc_args_for_write,DocAsO,DocAsOW),

   

   HDocAsNew =.. [F|DocAsOW],

   ignore(some_flocation(MN,F,A)),ignore((MN=M)),
   prefixed_module(M,MN,HDocAsNew,PrefixedDocH),
   for_m_make_summary(M,PrefixedDocH,SummaryI),
   notes_to_better_text(autodoc_case,SummaryI,SummaryA),
   atom_string(SummaryA,Summary),
   t_l:file_loc(FileFromLine),
   asserta(M:'$pldoc'(F/A, FileFromLine, Summary,Summary)),
   autodoc_magic(Starter),
   format('~N~s%% ~w is semidet.\n%\n% ~w.\n%\n',[Starter,HDocAsNew, Summary]))),!.



%= 	 	 

%% autodoc_case( ?I, ?O) is semidet.
%
% Autodoc Case.
%
autodoc_case(I,O):-catch(logicmoo_util_strings:toPropercase(I,O),_,fail),!.
autodoc_case(I,I).

%= 	 	 

%% ignored_assignment( ?A, ?B) is semidet.
%
% Ignored Assignment.
%
ignored_assignment(A,B):-ignore(A=B). 


%= 	 	 

%% fix_doc_args_for_write( ?A, ?A) is semidet.
%
% Fix Document Arguments For Write.
%
fix_doc_args_for_write(Var:A,SF):- var(Var),format(atom(SF),' ?~w',[A]).
fix_doc_args_for_write((*):A,SF):-format(atom(SF),' :Term~w',[A]).
fix_doc_args_for_write((0):A,SF):-format(atom(SF),' :Goal~w',[A]).
fix_doc_args_for_write((I):A,SF):-integer(I),format(atom(SF),' :PRED~w~w',[I,A]).
fix_doc_args_for_write((?):A,SF):-format(atom(SF),' ?~w',[A]).
fix_doc_args_for_write((+):A,SF):-format(atom(SF),' +~w',[A]).
fix_doc_args_for_write((-):A,SF):-format(atom(SF),' -~w',[A]).
fix_doc_args_for_write([_|_]:A,SF):-format(atom(SF),' ?list:~w',[A]).
fix_doc_args_for_write(_:A,SF):-!,fix_doc_args_for_write('?':A,SF).
fix_doc_args_for_write(A,A).


%= 	 	 

%% attempt_head_vnames( :TermARG1) is semidet.
%
% Attempt Head Vnames.
%
attempt_head_vnames(_:NameH):-
   attempt_head_varnames_0(NameH), get_random_headvars(NameH),!.
attempt_head_vnames(_:NameH):-
    get_random_headvars(NameH),!.
attempt_head_vnames(_:NameH):-
   attempt_head_varnames_0(NameH).

%= 	 	 

%% attempt_head_varnames_0( ?NameH) is semidet.
%
% attempt head varnames  Primary Helper.
%
attempt_head_varnames_0(NameH):- \+ compound(NameH),!.
attempt_head_varnames_0(NameH):- try_get_head_vars(NameH),ground(NameH),\+ ((arg(_,NameH,'$VAR'(Named)),atom(Named),upcase_atom(Named,UNamed),atom_contains(UNamed,'VAR'))), !.
attempt_head_varnames_0(NameH):- try_get_head_vars(NameH),ground(NameH),!.
attempt_head_varnames_0(NameH):- get_clause_vars(NameH),!.




%= 	 	 

%% attempt_head_modes( :TermNotFound) is semidet.
%
% Attempt Head Pred Modes.
%
attempt_head_modes(M:ModeH):- clause(M:'$mode'(ModeH, _Det),true),!.
attempt_head_modes(M:ModeH):- predicate_property(M:ModeH,meta_predicate(ModeH)),!.
attempt_head_modes(_:ModeH):- predicate_property(_:ModeH,meta_predicate(ModeH)),!.
attempt_head_modes(M:ModeH):- predicate_property(M:ModeH,number_of_clauses(Nth)),attempt_head_modes_0(Nth,Nth,M:ModeH),!.
attempt_head_modes(_:ModeH):- predicate_property(M:ModeH,number_of_clauses(Nth)),attempt_head_modes_0(Nth,Nth,M:ModeH),!.
attempt_head_modes(NotFound):-dmsg(attempt_head_modes(NotFound)),!.


%= 	 	 

%% attempt_head_modes_0( ?VALUE1, ?VALUE2, :TermM) is semidet.
%
% attempt head Pred Modes  Primary Helper.
%
attempt_head_modes_0(0,_,_).
attempt_head_modes_0(1,_,M:ModeH):-!,clause(M:ModeH,_),!.
attempt_head_modes_0(Max,1,M:ModeH):-nth_clause(M:ModeH,Max,Ref),!,clause(M:ModeH,_,Ref),!.
attempt_head_modes_0(Max,Nth,M:ModeH):-nth_clause(M:ModeH,Nth,Ref),
   (great_clause(M:ModeH,Ref)->true;(Nth0 is Nth - 1, attempt_head_modes_0(Max,Nth0,M:ModeH))),!.
attempt_head_modes_0(_,_,M:ModeH):-!,clause(M:ModeH,_),!.
   

%= 	 	 

%% great_clause( :TermM, ?Ref) is semidet.
%
% Great Clause.
%
great_clause(M:ModeH,Ref):- term_variables(ModeH,Vs1),clause(M:ModeH,_,Ref),term_variables(ModeH,Vs2), \+ ground(Vs1), Vs1\=Vs2.
great_clause(M:ModeH,Ref):- term_variables(ModeH,Vs1),clause(M:ModeH,_,Ref),term_variables(ModeH,Vs2),  Vs1\=Vs2.


%= 	 	 

%% fixup_doc_args( ?Pred, ?P, ?N, :TermDoc, :TermODoc) is semidet.
%
% Fixup Document Arguments.
%
fixup_doc_args(_,_,_,[],[]):-!.
fixup_doc_args(Pred,P,N,[Doc|As],[ODoc|OAs]):-
  must(ignore(call(Pred,P,N,Doc,ODoc))),
  !,N2 is N+1, must(fixup_doc_args(Pred,P,N2,As,OAs)).

:- style_check(-singleton).

%= 	 	 

%% fixda( ?P, ?N, :TermMode, :TermARG4) is semidet.
%
% Fixda.
%

fixda(P,N,Mode:_,_):- sanity(Mode\==system),fail.
fixda(P,N,Mode:Name,ModeO:NameO):-compound(Mode),functor(Mode,_,A),arg(A,Mode,NMode),fixda(P,N,NMode:Name,ModeO:NameO),!.
fixda(P,N,Mode:NameV,ModeO:NameO):-compound(NameV),arg(1,NameV,Name),fixda(P,N,Mode:Name,ModeO:NameO),!.
fixda(P,N,Mode:NameV,ModeO:NameO):-atom(NameV),atom_concat('_',Name,NameV),fixda(P,N,Mode:Name,ModeO:NameO),!.
fixda(P,N,Mode:Name,Mode:Name):-ground(Mode:Name),!.
fixda(P,N,Mode:Name,Mode:Name):-var(Mode),nonvar(Name),name_to_mode(P,N,Name,Mode),!.
fixda(P,N,Mode:Name,Mode:Name):-nonvar(Mode),var(Name),once(((mode_to_name(P,N,Mode,MName),atom_concat(MName,N,VarName),must(Name='$VAR'(VarName))))),!.
fixda(P,N,Mode:Name,Mode:Name):-!.
fixda(P,N,ModeName,ModeName):-!.


%= 	 	 

%% fixda2( ?P, ?N, :TermMode, :TermModeOO) is semidet.
%
% Fixda Extended Helper.
%
fixda2(P,N,Mode:Name,ModeOO:NameOO):-ignore((atom_concat('ARG',N,NameV),'$VAR'(NameV)=NameO,fixda(P,N,Mode:NameO,ModeOO:NameOO))).


%= 	 	 

%% mode_to_name( ?P, ?N, ?Mode, ?Name) is semidet.
%
% Pred Mode Converted To Name.
%
mode_to_name(P,N,Mode,Name):-nonvar(Mode),nonvar(Name),!.
mode_to_name(P,N,Mode,Name):-var(Name),var(Mode),!.
mode_to_name(P,N,Mode,Name):-var(Mode),nonvar(Name),!,name_to_mode(P,N,Name,Mode).
mode_to_name(P,N,Mode,Name):-mode_to_name0(Mode,Name),!.
mode_to_name(P,N,Mode,Name):-nonvar(Mode),value_to_name(P,N,Mode,Name),!.

value_to_name(P,N,Comp,'VAR'):- var(Comp),!.
value_to_name(P,N,Comp,Comp):- atom(Comp),atom_length(Comp,L),L>1, \+ bad_varnamez(Comp),!.
value_to_name(P,N,Comp,NAME):- text_to_string_safe(Comp,Str),string_to_atom(Str,Atom),atom_length(Atom,L),L>1,\+ bad_varnamez(Atom),NAME=Atom.
value_to_name(P,N,Comp,'LIST'):- is_list(Comp), \+ \+ Comp=[_|_],!.
value_to_name(P,N,Comp,'ATOM'):- atom(Comp),!.
value_to_name(P,N,Comp,'STRING'):- string(Comp),!.
value_to_name(P,N,Comp,'TERM'):- compound(Comp).

%= 	 	 

%% mode_to_name0( ?VALUE1, ?VALUE2) is semidet.
%
% Pred Mode Converted To Name Primary Helper.
%
mode_to_name0(0,'GOAL').
mode_to_name0(*,'QUERY').
mode_to_name0(:,'CALL').
mode_to_name0(+,'OUT').
mode_to_name0(?,'UPARAM').
mode_to_name0(-,'IN').
mode_to_name0(I,Name):- number(I),atom_concat('PRED',I,Name).

% mode_to_name0(_,'VALUE').

%= 	 	 

%% name_to_mode( ?P, ?N, ?Name, ?Mode) is semidet.
%
% Name Converted To Pred Mode.
%
name_to_mode(P,N,Name,Mode):-var(Name),!,mode_to_name(P,N,Mode,Name).
name_to_mode(P,N,'$VAR'(Name),Mode):-!,name_to_mode(P,N,Name,Mode),!.
name_to_mode(P,N,Name,_Mode):- must( atom(Name)),!.
name_to_mode(P,N,Name,Mode):-between(1,9,Mode),mode_to_name0(Mode,Part),atom_contains(Name,Part).
name_to_mode(P,N,Name,Mode):-mode_to_name0(Mode,Part),atom_contains(Name,Part).
name_to_mode(P,N,Name,-):-atom_contains(Name,'OUT').
name_to_mode(P,N,Name,*):-atom_contains(Name,'PRED').
name_to_mode(P,N,Name,:):-atom_contains(Name,'CALL').
name_to_mode(P,N,Name,0):-atom_contains(Name,'GOAL').
name_to_mode(P,N,Name,-):-atom_contains(Name,'RESU').
name_to_mode(P,N,'I',+).
name_to_mode(P,N,'O',-).
name_to_mode(P,N,Name,?):-atom_contains(Name,'L').
name_to_mode(P,N,Name,?):-atom_contains(Name,'T').
name_to_mode(P,N,Name,+):-atom_contains(Name,'OP').
name_to_mode(P,N,Name,+):-atom_contains(Name,'IN').
name_to_mode(P,N,Name,+):-atom_concat('_',_,Name).
name_to_mode(P,N,Name,+):-atom_concat(_,'O',Name).
name_to_mode(P,N,Name,Mode):-atom(Name),upcase_atom(Name,UName),Name\==UName,!,name_to_mode(P,N,UName,Mode).
name_to_mode(P,N,_Name,?):-!.

:- style_check(+singleton).


%= 	 	 

%% prefixed_module( ?M, ?M, ?DocH, ?DocH) is semidet.
%
% Prefixed Module.
%
prefixed_module(_M,MN,P,MN:P):-predicate_property('$attvar':P,number_of_clauses(_)),\+ predicate_property('$attvar':P,imported_from(_)),!.
prefixed_module(_M,MN,P,MN:P):-predicate_property('system':P,number_of_clauses(_)),\+ predicate_property('system':P,imported_from(_)),!.
prefixed_module(M,M,DocH,DocH):-!.
prefixed_module(_M,MN,DocH,MN:DocH).


%= 	 	 

%% system_pred( :TermM2) is semidet.
%
% System Predicate.
%
system_pred(M2:P):-predicate_property(_:P,imported_from(system)),current_predicate(_,M2:P), \+ predicate_property(M2:P,imported_from(_)).



%= 	 	 

%% notes_to_better_text( ?TextM, ?Text) is semidet.
%
% Notes Converted To Better Text.
%
notes_to_better_text(TextM,Text):-flatten([TextM],TextT),maplist(termw_to_atom,TextT,AtomsM),atomic_list_concat(AtomsM,' ',Text),!.


%= 	 	 

%% notes_to_better_text( ?Pred, ?TextM, ?Text) is semidet.
%
% Notes Converted To Better Text.
%
notes_to_better_text(Pred,TextM,Text):-flatten([TextM],TextT),maplist(termw_to_atom,TextT,AtomsM),must_maplist(Pred,AtomsM,AtomsM2),atomic_list_concat(AtomsM2,' ',Text),!.


%= 	 	 

%% termw_to_atom( ?T, ?A) is semidet.
%
% Termw Converted To Atom.
%
termw_to_atom(T,A):-atom(T),!,A=T.
termw_to_atom(T,A):-format(atom(A),'~w',[T]).

%= 	 	 

%% make_summary0( ?Args, ?F, ?Text) is semidet.
%
% Make Summary Primary Helper.
%
make_summary0(Args,F,Text):- must_det_l((make_l_summary(Args,F,TextM),notes_to_better_text(TextM,Text))).


%= 	 	 

%% for_m_make_summary( ?For, ?P, ?TextOO) is semidet.
%
% For Module Make Summary.
%
for_m_make_summary(For,M:P,[Prefix, TextOO]):- must(atom(M)), must(functor(P,F,A)),format(atom(Prefix),'Hook to [~w] for module ~w.\n%',[M:F/A,For]),
   must(for_m_make_summary(For,P,TextOO)).
for_m_make_summary(_,P,TextOO):- compound(P),compound_name_arguments(P,F,_),make_summary(P,F,TextOO).
for_m_make_summary(_,F,TextOO):- make_summary(F,F,TextOO).


%= 	 	 

%% make_summary( ?Args, ?F, ?TextOO) is semidet.
%
% Make Summary.
%
make_summary(Args,F,TextOO):- must_det_l((make_summary0(Args,F,Text),make_summary0(Args,Text,TextO),make_summary0(Args,TextO,TextOO))),!.


%= 	 	 

%% make_l_summary( ?A, ?I, ?O) is semidet.
%
% Make (list Version) Summary.
%
make_l_summary(A,I,O):- must(loop_check(make_l_summary_lc(A,I,O),O=I)),!.


%= 	 	 

%% make_l_summary_lc( ?Args, :TermH, ?HL) is semidet.
%
% Make (list Version) Summary Not Loop Checked.
%
make_l_summary_lc(_Args,[],[]).
make_l_summary_lc(Args,H,[HL,'Primary Helper']):- atom(H),atom_concat(L,'0',H),make_l_summary(Args,L,HL),!.
make_l_summary_lc(Args,H,[HL,'Secondary Helper']):- atom(H),atom_concat(L,'1',H),make_l_summary(Args,L,HL),!.
make_l_summary_lc(Args,H,[HL,'Trace']):- atom(H),atom_concat(L,'trace',H),make_l_summary(Args,L,HL),!.
make_l_summary_lc(Args,H,[HL,'Types']):- atom(H),atom_concat(L,'types',H),make_l_summary(Args,L,HL),!.
make_l_summary_lc(Args,H,[HL,'(isa/2)']):- atom(H),atom_concat(L,'isa',H),make_l_summary(Args,L,HL),!.
make_l_summary_lc(Args,H,[HL,'Extended Helper']):- atom(H),atom_concat(L,'2',H),make_l_summary(Args,L,HL),!.
make_l_summary_lc(Args,H,HL):- atom(H),atom_chars(H,Chars),append(Left,[N],Chars), char_type(N,digit),atom_number(N,Num),atom_chars(H,Left),make_l_summary(Args,[Left,Num],HL),!.
make_l_summary_lc(Args,[H1,H2|T],OUT):- append(Left,[A],[H1,H2|T]),summary_ending(A,H),flatten_text(Left,[H],AGAIN),make_l_summary(Args,AGAIN,OUT),!.
make_l_summary_lc(Args,name(N),E):- !,(arg(N,Args,_:E);arg(N,Args,E)),!.
make_l_summary_lc(Args,AGAIN,OUT):- summary_start(Left,H),flatten([Left],LLeft),append(LLeft,Rest,AGAIN),!,flatten_text(H,Rest,New),make_l_summary(Args,New,OUT),!.
make_l_summary_lc(Args,[H|T],WO):-!,must_det_l((make_l_summary(Args,H,HL),make_l_summary(Args,T,TL),flatten_text(HL,TL,WO))).
make_l_summary_lc(Args,F,Text):- atom(F),atomic_list_concat(WL,'_',F),length(WL,LL),LL>1,make_l_summary(Args,WL,Text),!.
make_l_summary_lc(Args,F,Text):- atom(F),atomic_list_concat(WL,' ',F),length(WL,LL),LL>1,make_l_summary(Args,WL,Text),!.
make_l_summary_lc(Args,H,HL):- atom_chars(H,Chars),append(Left,[L,U|MORE],Chars),
  char_type(L,lower),char_type(U,upper),append(Left,[L],LL),atom_chars(LS,LL),downcase_atom(U,UL),atom_chars(RS,[UL|MORE]),make_l_summary(Args,[LS,RS],HL),!.
make_l_summary_lc(_Args,H,OUT):- longer_sumry(H,Sum),flatten([Sum],OUT),!.
make_l_summary_lc(_Args,H,[A]):- termw_to_atom(H,A).


%= 	 	 

%% summary_start( ?VALUE1, ?VALUE2) is semidet.
%
% Summary Start.
%
summary_start([on,x],['If there is an exception in',name(1),'then']).
summary_start([on],'Whenever').
summary_start([at],'When').


%= 	 	 

%% summary_ending( :PRED2A, ?H) is semidet.
%
% Summary Ending.
%
summary_ending(A,H):-atom(A),atom_number(A,N),!,summary_ending(N,H).
summary_ending(i,'For Internal Interface').
summary_ending(u,'For User Code').
summary_ending(t,'True Stucture').
summary_ending(0,'Primary Helper').
summary_ending(f,'False').
summary_ending(1,'Secondary Helper').
summary_ending(2,'Extended Helper').
summary_ending(E,H):-number(E),atomic_list_concat(['Helper number ',E,'.'],'',H).


%= 	 	 

%% flatten_text( ?L, ?R, ?O) is semidet.
%
% Flatten Text.
%
flatten_text(L,R,O):-must((sanity(var(O)),flatten([L,R],O))).



%= 	 	 

%% longer_sumry( ?S, ?W) is semidet.
%
% Longer Sumry.
%
longer_sumry('HIDE','Presently Unused').
longer_sumry(ain,'Assert If New').
longer_sumry(arg,'Argument').
longer_sumry(as,'Converted To').
longer_sumry(ask,'Complete Inference').
longer_sumry(attvar,'Attribute Variable').
longer_sumry(av,'Attributed vars').
longer_sumry(b,'Backtackable').
longer_sumry(bb,'Blackboard').
longer_sumry(bc,'Backchaining').
longer_sumry(box,'Datalog').
longer_sumry(boxlog,'Datalog').
longer_sumry(bugger,'LogicMoo Debugger').
longer_sumry(c,'Class').
longer_sumry(chk,'Checking').
longer_sumry(cl,'Clause').
longer_sumry(clif,'IEEE Standard Common Logic Interchange Format Version').
longer_sumry(clr,'Remove/Erase').
longer_sumry(cmt,'Comment').
longer_sumry(cnf,'Confunctive Normal Form').
longer_sumry(ctx,'Context').
longer_sumry(d,'(Debug)').
longer_sumry(db,'Database').
longer_sumry(dcall,'Dirrectly Call').
longer_sumry(decl,'Declare').
longer_sumry(del,'Remove/Erase').
longer_sumry(det,'Deterministic').
longer_sumry(dlog,'Datalog').
longer_sumry(dnf,'Disjunctive Normal Form').
longer_sumry(doc,'Document').
longer_sumry(dom,'Domain').
longer_sumry(eq,'Using (==/2) (or =@=/2) )').
longer_sumry(f,'Functor').
longer_sumry(fa,'Functor-Arity').
longer_sumry(fc,'Forward Chaining').
longer_sumry(fmt,'Format').
longer_sumry(fn,'Function').
longer_sumry(from,'Converted From').
longer_sumry(ft,'Format Type').
longer_sumry(fwd,'Forward Repropigated').
longer_sumry(g,'Globally').
longer_sumry(h,'Head').
longer_sumry(hb,'Head+Body').
longer_sumry(i,'Instance').
longer_sumry(ilc,'Inside Of Loop Checking').
longer_sumry(impl,'Implimentation').
longer_sumry(into,'Converted To').
longer_sumry(io,'Input/Output').
longer_sumry(is,'If Is A').
longer_sumry(just,'Justification').
longer_sumry(kb,'Knowledge Base').
longer_sumry(kbp,'Knowledge Base P-').
longer_sumry(kif,'Knowledge Interchange Format').
longer_sumry(l,'(List version)').
longer_sumry(lang,'Language').
longer_sumry(lbl,'Labeling (Residuals)').
longer_sumry(lc,'not loop checked').
longer_sumry(lhs,'Left-Hand-Side').
longer_sumry(lit,'Literal').
longer_sumry(m,'Module').
longer_sumry(mfa,'Module-Functor-Arity').
longer_sumry(mfa,m_fa).
longer_sumry(minfo,'Metainformation').
longer_sumry(mode,'p Mode').
longer_sumry(mp,m_goal).
longer_sumry(mk,'Make').
longer_sumry(mpi,m_pi).
longer_sumry(mpred,'Managed Predicate').
longer_sumry(mt,'User Microtheory').
longer_sumry(mu,'Module Unit').
longer_sumry(mud,'Application').
longer_sumry(must,'Must Be Successfull').
longer_sumry(naf,'Negation-By-Faliure').
longer_sumry(nb,'Non Backtackable').
longer_sumry(neg,'Negated').
longer_sumry(neq,'Negated Equality').
longer_sumry(nesc,'Necesity').
longer_sumry(nf,'Normal Form').
longer_sumry(nnf,'Negated Normal Form').
longer_sumry(non,'Not').
longer_sumry(nots,'Negations').
longer_sumry(o,'Output').
longer_sumry(op,'Oper.').
longer_sumry(p,'Pred').
longer_sumry(pfc,'Prolog Forward Chaining').
longer_sumry(pi,'Predicate Indicator').
longer_sumry(poss,'Possibility').
longer_sumry(pp,'Pretty Print').
longer_sumry(pred,'Predicate').
longer_sumry(pt,'Predicate Type').
longer_sumry(quick,'Incomplete, But Fast, Version').
longer_sumry(rem,'Remove/Erase').
longer_sumry(rhs,'Right-Hand-Side').
longer_sumry(safe,'Safely Paying Attention To Corner Cases').
longer_sumry(sanity,'Optional Sanity Checking').
longer_sumry(sent,'Sentence').
longer_sumry(sexpr,'S-Expression').
longer_sumry(sk,'Skolem').
longer_sumry(st,'Stacktrace').
longer_sumry(store,'Storage').
longer_sumry(t,'True Structure').
longer_sumry(tell,'Canonicalize And Store').
longer_sumry(tf,'True/False').
longer_sumry(tl,'Thread Local').
longer_sumry(tms,'Truth Maintainence/WFF').
longer_sumry(to,'Converted To').
longer_sumry(tt,'Collection Type').
longer_sumry(umt,'User Microtheory').
longer_sumry(var,'Variable').
longer_sumry(verbose,'While Being Descriptive').
longer_sumry(wff,'Well-Formed Formula').
longer_sumry(wfs,'Well-Founded Semantics Version').
longer_sumry(why,'Generation Of Proof').
longer_sumry(with,'Using').
longer_sumry(xcall,'Extended Call').
longer_sumry(S,W):-atom(S),atom_concat(S0,'msg',S),longer_sumry(S0,W0),atom_concat(W0,'Message',W).
longer_sumry(S,W):-atom(S),atom_concat(S0,'s',S),longer_sumry(S0,W0),atom_concat(W0,'s',W).



% find normal docs
% prolog:help_hook(help(_M:F/A)):- predicate(F, A, _, _From, _To),!,fail.
% else use a structured comment 
% our smarter matching system (based off listing)
% prolog:help_hook(help(What)):- match_predicates(What, Preds), Preds\==[], Preds\==[What],forall(member(M:F/A,Preds),help(M:F/A)),fail.
% prolog:help_hook(help(A)):-  logicmoo_utils:mpred_show_doc(A),fail.


%	mpred_show_doc(+Object) is det.
%
%	Searches in doc indexes for Object occurances
%
%	@see	help/1.

:-export(mpred_show_doc/1).

%= 	 	 

%% mpred_show_doc( ?What) is semidet.
%
% Managed Predicate Show Document.
%
mpred_show_doc(What):- findall(Infos,mpred_show_doc(What,Infos),LInfos),LInfos\=[],flatten(LInfos,Infos),forall(member(E,Infos),format('~N~w~n',[E])).
mpred_show_doc(What):- match_predicates(What, Preds), Preds\==[], Preds\==[What],forall(member(M:F/A,Preds),mpred_show_doc(M:F/A)).



%= 	 	 

%% mpred_show_doc( ?M, ?PI) is semidet.
%
% Managed Predicate Show Document.
%
mpred_show_doc(M:F/A,['$mode'(PI, Det)]):-functor(PI,F,A), clause(M:'$mode'(PI, Det),true).
mpred_show_doc(M:F/A,[Info,Info2]):-   clause(M:'$pldoc'(F/A,_FL,Info,Info2),true).
mpred_show_doc(Id,[Info,Info2]):- clause(_,'$pldoc'(Id,_FL,Info,Info2),true).
mpred_show_doc(M,[Title,Info,Info2]):- pldoc_process:doc_comment(M:module(Title),_FileLines,Info,Info2).


% ?- help(term_expansion/2).
% ?- help(match_regex/2).
%:- ensure_loaded(library(check)).
%:- ensure_loaded(library(make)).


%= 	 	 

%% mpred_type_module( ?A) is semidet.
%
% Managed Predicate Type Module.
%
mpred_type_module(A):- \+ atom(A),!,fail.

%= 	 	 

%% mpred_prolog_only_module( ?M) is semidet.
%
% Managed Predicate Prolog Only Module.
%
mpred_prolog_only_module(mpred_type_wff).
mpred_prolog_only_module(logicmoo_varnames).
mpred_prolog_only_module(common_logic_compiler).
mpred_prolog_only_module(common_logic_snark).
mpred_prolog_only_module(common_logic_sexpr).
mpred_prolog_only_module(M):-atom_concat(mpred_,_,M).
mpred_prolog_only_module(M):-atom_concat(logicmoo_util,_,M).
mpred_prolog_only_module(M):-atom_concat(common_logic_,_,M).
mpred_prolog_only_module(logicmoo_utils).
mpred_prolog_only_module(t_l).
mpred_prolog_only_module(tlbugger).
mpred_prolog_only_module(lmcache).
mpred_prolog_only_module(lmconf).

mpred_prolog_only_module(M):- current_module(M),atom_concat(logicmoo_utils_,_,M).
% mpred_prolog_only_module(M):- lmconf:mpred_is_impl_file(F),make_module_name(F,M).
% mpred_prolog_only_module(user). 
:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).




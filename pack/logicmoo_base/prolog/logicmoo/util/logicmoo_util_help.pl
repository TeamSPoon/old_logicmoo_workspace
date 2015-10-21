/* <module> Logicmoo Debug Tools
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
            end_l_sum/2,
            export_file_preds/0,
            export_file_preds/1,
            export_file_preds/6,
            export_module_preds/0,
            list_item_per_line/3,
            functor_compare/3,
            helper_name/1,
            list_file_preds/0,
            list_file_preds/1,
            list_file_preds/2,
            longer_sumry/2,
            make_l_summary/2,
            make_module_name/2,
            make_summary/2,
            module_meta_transparent/1,
            mpred_prolog_only_module/1,
            mpred_show_doc/1,
            mpred_show_doc/2,
            mpred_source_file/2,
            name_modes/3,
            no_location/3,
            portray_clause_pi_LR/2,
            portray_clause_pi_UD/2,
            predicate_decl_module/2,
            print_fake_doc/2,
            scan_and_list_file_preds/1,
            some_flocation/3,
            some_location/3,
            mpred_type_module/1,
            target_module/2,
            list_item_per_line/4,
            write_modules/0,
            lmconf:sf_known/4,
            helper_name0/1, is_crossfile_module_0/1, make_summary0/2, mpred_source_file_0/2, skip_functor_export_0/1, to_comparable_fa0/3, to_mfa_0/4
          ]).
:- meta_predicate logicmoo_util_help:list_item_per_line(0,*,*,*),logicmoo_util_help:list_item_per_line(0,?,?).
:- (multifile lmconf:sf_known/4).
:- (module_transparent current_predicate_mfa/3, end_l_sum/2, export_file_preds/0, export_file_preds/1, export_file_preds/6, export_module_preds/0, functor_compare/3, helper_name/1, helper_name0/1, is_crossfile_module_0/1, list_file_preds/0,
  list_file_preds/1, list_file_preds/2, longer_sumry/2, make_l_summary/2, make_module_name/2, module_meta_transparent/1, mpred_prolog_only_module/1, mpred_source_file/2, name_modes/3, no_location/3, portray_clause_pi_LR/2, portray_clause_pi_UD/2, print_fake_doc/2, scan_and_list_file_preds/1, skip_functor_export_0/1, some_flocation/3, some_location/3, target_module/2, to_comparable_fa0/3, to_mfa_0/4, write_modules/0).
:- export((helper_name0/1, is_crossfile_module_0/1, make_summary0/2, mpred_source_file_0/2, skip_functor_export_0/1, to_comparable_fa0/3, to_mfa_0/4)).
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
:- doc_collect(true).

:- if(exists_source(library(pldoc))).
:- use_module(library(pldoc), []).
	% Must be loaded before doc_process
:- use_module(library(pldoc/doc_process)).
:- endif.
:- use_module(library(prolog_xref)).


make_module_name(P,M):-atom(P),!,file_base_name(P,F),file_name_extension(M,_Ext,F).
make_module_name(mpred/P,M):-nonvar(P),!,make_module_name(P,M).
make_module_name(util/P,M):-nonvar(P),!,make_module_name(P,M).
make_module_name(P,M):-must(filematch(P,F)),F\=P,!,make_module_name(F,M).
helper_name0(F):- atom_chars(F,Chars),append(_,[U,N],Chars), ( char_type(U,digit) ;  char_type(N,digit)), !.

helper_name(_:FA):-!,helper_name(FA).
helper_name(F/_):-!,helper_name(F).
helper_name(F):- atom(F), helper_name0(F).


% list_item_per_line(NL,PerLine,List).
list_item_per_line(NL,PerLine,List):- list_item_per_line(NL,PerLine,PerLine,List).
list_item_per_line(_,_,_,[]).
list_item_per_line(NL,PerLine,NLeft,List):- NLeft<1,NL,!,list_item_per_line(NL,PerLine,PerLine,List).
list_item_per_line(_,_,_,[E]):-  writeq(E),!.
list_item_per_line(NL,PerLine,NLeft,[E|List]):- NNLeft is NLeft -1, writeq(E),format(', '),list_item_per_line(NL,PerLine,NNLeft,List).

portray_clause_pi_LR(_,[]):-!.
portray_clause_pi_LR(T,LIST0):- list_to_set(LIST0,LIST), 
    length(LIST,Len), (Len<8 -> (list_to_conjuncts(LIST,E),P=..[T,E], format('~N',[]), portray_clause( ( :-P )));
    (format(':- ~w (( ',[T]),list_item_per_line(format('~N  ',[]),8,LIST),format('  )).~n'))).
%portray_clause_pi_LR(T,LIST0):-list_to_set(LIST0,LIST),list_to_conjuncts(LIST,E),P=..[T,E], format('~N',[]), portray_clause( ( :-P )),!.

portray_clause_pi_UD(_,[]):-!.
portray_clause_pi_UD(T,LIST0):-list_to_set(LIST0,LIST),list_to_conjuncts(LIST,E), format('~N :- ~q % ',[T]), portray_clause( ( cmt:-E )),!.

to_comparable_fa0(P,_,A):-var(P),!,integer(A).
to_comparable_fa0(_-FA,F,A):-!,to_comparable_fa0(FA,F,A).
to_comparable_fa0(_:FA,F,A):-!,to_comparable_fa0(FA,F,A).
to_comparable_fa0(F/A,F,A):-!.
to_comparable_fa0(P,F,A):-functor(P,F,A).

functor_compare(R,(_-C1),(_-C2)):- nonvar(C1),nonvar(C2),functor_compare(R,C1,C2).
functor_compare(R,M1:P1,M2:P2):- M1==M2,!,functor_compare(R,P1,P2).
functor_compare(R,M1:P1,M2:P2):- compare(FR,M1,M2),!,(FR\==(=)->R=FR;functor_compare(R,P1,P2)).
functor_compare(R,_:_,_):- R=(>).
functor_compare(R,_,_:_):- R=(<). 
functor_compare(R,P1,P2):- logicmoo_util_help:to_comparable_fa0(P1,F1,A1),logicmoo_util_help:to_comparable_fa0(P2,F2,A2),!,compare(FR,F1,F2),(FR\==(=)->R=FR;compare(R,A1,A2)).


list_file_preds:- source_location(S,_),list_file_preds(S).


skip_functor_export_0('$load_context_module'/3).
skip_functor_export_0('$included'/_).
skip_functor_export_0('$pldoc'/_).

is_crossfile_module_0(lmconf).
is_crossfile_module_0(baseKB).
is_crossfile_module_0(basePFC).
is_crossfile_module_0(user).

to_mfa_0(_,user,SM:FA,SM:FA):-!.
to_mfa_0(_,_,SM:FA,SM:FA):- is_crossfile_module_0(SM),!.
to_mfa_0(_,PM,_:FA,PM:FA):- is_crossfile_module_0(PM),!.
to_mfa_0(FM,PM,SM:F/A, SM:F/A):- PM = SM,PM = FM, functor(P,F,A) , (predicate_property(SM:P,thread_local);predicate_property(SM:P,multifile);predicate_property(SM:P,dynamic)),!.
to_mfa_0(FM,PM,SM:FA, FA ):- PM = SM,PM = FM,!.
to_mfa_0(FM,PM,SM:F/A, SM:F/A):- FM == PM,PM\==SM, functor(P,F,A), predicate_property(SM:P,multifile),!.
to_mfa_0(FM,PM,SM:FA, FA):- FM == PM,PM\==SM, !.
to_mfa_0(_,_,SM:FA,SM:FA):-!.


scan_and_list_file_preds(F):- forall(filematch(F,S),((make_module_name(S,FM),ensure_loaded(S),export_file_preds(S),list_file_preds(S,FM)))).
list_file_preds(F):- forall(filematch(F,S),((make_module_name(S,FM),ensure_loaded(S),list_file_preds(S,FM)))).
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
   portray_clause_pi_LR( was_shared_multifile,DynamicL),
   portray_clause_pi_LR( volatile,Volatile))),!.
   



:-export(module_meta_transparent/1).
% = :- meta_predicate(module_meta_transparent(:)).
module_meta_transparent(M:F/A):-must(functor(P,F,A)),!,module_meta_transparent(M:P).
module_meta_transparent(M:P):-predicate_property(M:P,meta_predicate(_)),!.
module_meta_transparent(M:P):-predicate_property(M:P,transparent),!.
module_meta_transparent(M:P):-functor(P,F,A),module_transparent(M:F/A),!. % ground(P),M:meta_predicate(P),!.
% module_meta_transparent(M:P):-P=..[_|Args],maplist('='(?),Args),module_meta_transparent(M:P).
module_meta_transparent(_).


:-multifile(lmconf:sf_known/4).
:-dynamic(lmconf:sf_known/4).
:- export(mpred_source_file_0/2).
mpred_source_file(M:P,S):- no_repeats(mpred_source_file_0(M:P,S)).
mpred_source_file_0(M:P,S):- var(P)-> (lmconf:sf_known(S,F,A,M),functor(P,F,A)) ; (functor(P,F,A),lmconf:sf_known(S,F,A,M)).
mpred_source_file_0(M:P,S):-predicate_property(M:P,file(S)),once((to_comparable_fa0(P,F,A),assert_if_new(lmconf:sf_known(S,F,A,M)))).
mpred_source_file_0(M:P,S):-source_file(M:P,S),once((to_comparable_fa0(P,F,A),assert_if_new(lmconf:sf_known(S,F,A,M)))).

% Return the correct M for the F/A
current_predicate_mfa(M,F,A):-atom(F),integer(A),!,no_repeats(M:F/A,((functor(P,F,A),current_predicate(_,M:P),\+ predicate_property(M:P,imported_from(_))))).
current_predicate_mfa(M,F,A):-no_repeats(M:F/A,((current_predicate(_,M:P),functor(P,F,A),\+ predicate_property(M:P,imported_from(_))))).

no_location(M,F,A):-current_predicate_mfa(M,F,A),\+ lmconf:sf_known(_S,F,A,_MN).

some_location(M,F,A):-no_repeats(F/A,(( current_predicate_mfa(M,F,A); lmconf:sf_known(_S,F,A,_MN)))).
some_flocation(FM,F,A):-no_repeats(F/A,(( lmconf:sf_known(_S,F,A,FM);current_predicate_mfa(FM,F,A)))).


:- module_transparent(export_file_preds/1).
:- export(export_file_preds/0).
export_file_preds:- source_location(S,_),export_file_preds(S),!.
:- export(export_file_preds/1).
export_file_preds(_):- current_prolog_flag(xref,true),!.
export_file_preds(FileMatch):- forall(must(filematch(FileMatch,File)),
 (source_context_module(NotUser),show_call(why,NotUser\==user),
   forall(must(mpred_source_file(M:P,File)),(functor(P,F,A),must(export_file_preds(NotUser,File,M,P,F,A)))))).

predicate_decl_module(Pred,RM):-current_predicate(_,RM:Pred),\+ predicate_property(RM:Pred,imported_from(_)),must(RM\==user).


:- style_check(-singleton).

:- dynamic(logicmoo_util_help:mpred_is_impl_file/1).
:- multifile(logicmoo_util_help:mpred_is_impl_file/1).
:- volatile(logicmoo_util_help:mpred_is_impl_file/1).

:- if(false).
:- else.
:- endif.

write_modules:- forall(logicmoo_util_help:mpred_is_impl_file(F),(export_file_preds(F),list_file_preds(F))).



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
export_module_preds:- current_prolog_flag(xref,true),!.
export_module_preds:- source_context_module(M),source_file_property(S,module(M)),export_file_preds(S),forall(source_file_property(S,includes(F,_)),export_file_preds(F)).


:- use_module(library(pldoc/doc_pack)).

name_modes(ModeAs:NameAs,ModeAs,NameAs).


:- dynamic(logicmoo_util_help:mpred_is_impl_file/1).
:- multifile(logicmoo_util_help:mpred_is_impl_file/1).
:- volatile(logicmoo_util_help:mpred_is_impl_file/1).

target_module(P,M):-mpred_source_file(P,F),logicmoo_util_help:mpred_is_impl_file(F),make_module_name(F,M).

make_file_help(F):- \+((atom(F),exists_file(F))),!,forall(filematch(F,S),make_file_help(S)).

make_file_help(File):-
        read_source_file_vars(File),
	setup_call_cleanup(
	    prolog_open_source(File, In),
	    read_source_help(File,In),
	    prolog_close_source(In)),!.

read_source_help(File,In):-
  make_module_name(File,M),
	repeat,
	  catch(prolog_read_source_term(In, Term, Expanded, [ variable_names(Vs), syntax_errors(error) , term_position(TermPos) ]),
		E,(nop((dmsg(E),trace)),fail)),
          stream_position_data(line_count, TermPos, LineNo),
	  ignore(save_source_file_help(M, File ,LineNo,Term,Vs)),
	  (   is_list(Expanded)
	  ->  member(T, Expanded)
	  ;   T = Expanded
	  ),
	(   T == end_of_file
	->  !
	;  (  ignore(save_source_file_help(M, File ,LineNo,T,Vs))),
	    fail
	).

save_source_file_help(M, File ,LineNo,Term,_Vs):-
   strip_module(Term,_MU,PI),
   get_functor(PI,F,A),
   use_source_file_help(File:LineNo,M:F/A).

:- dynamic(last_source_file_help/2).
use_source_file_help(File:_LineNo,M:F/A):- last_source_file_help(File,M:F/A),!.
use_source_file_help(File:_LineNo,M:F/A):-
   retractall(last_source_file_help(File,_)),
   asserta(last_source_file_help(File,M:F/A)),
   functor(P,F,A),
   print_fake_doc(M,P).
   


print_fake_doc(MN,P):- 
   scan_source_files_for_varnames,
   target_module(MN,M),
   current_predicate(_,M:P),
   once(to_comparable_fa0(P,F,A)),
   mpred_prolog_only_module(M),
   functor(NameH,F,A),NameH=..[F|NameAs],
   functor(ModeH,F,A),ModeH=..[F|ModeAs],
   functor(DocH,F,A),DocH=..[F|DocAs],
   maplist(name_modes,DocAs,ModeAs,NameAs),   
   ignore(predicate_property(M:ModeH,meta_predicate(ModeH))),
   ignore(predicate_property(_:ModeH,meta_predicate(ModeH))),
   all_different_vals(NameAs),
   lock_vars(NameH),
   try_get_head_vars(NameH),
   ignore(some_flocation(MN,F,A)),
   ignore(mpred_source_file(_:DocAs,Pos)),
   make_summary(F,Summary),
   format('%% ~q is semidet.\n% ~w.\n% at ~w\n',[MN:DocH, Summary,  Pos]).

make_summary0(F,Text):- make_l_summary(F,TextM),atomic_list_concat(TextM,' ',Text).

make_summary(F,TextOO):- make_summary0(F,Text),make_summary0(Text,TextO),make_summary0(TextO,TextOO),!.

end_l_sum(A,H):-atom(A),atom_number(A,N),!,end_l_sum(N,H).
end_l_sum(i,internal_interface).
end_l_sum(u,user_interface).
end_l_sum(t,data_structur).
end_l_sum(0,prime_helper).
end_l_sum(E,H):-number(E),atomic_list_concat([helper,E,n],'',H).

make_l_summary([],[]).
make_l_summary([H1,H2|T],OUT):- append(Left,[A],[H1,H2|T]),end_l_sum(A,H),append(Left,[H],AGAIN),make_l_summary(AGAIN,OUT).
make_l_summary([H|T],WO):-!,make_l_summary(H,HL),make_l_summary(T,TL),append(HL,TL,WO).
make_l_summary(F,Text):- atom(F),atomic_list_concat(WL,'_',F),length(WL,LL),LL>1,make_l_summary(WL,Text).
make_l_summary(F,Text):- atom(F),atomic_list_concat(WL,' ',F),length(WL,LL),LL>1,make_l_summary(WL,Text).
make_l_summary(H,HL):- atom_chars(H,Chars),append(Left,[N],Chars), char_type(N,digit),atom_number(N,Num),!,atom_chars(H,Left),make_l_summary([Left,Num],HL).
make_l_summary(H,HL):- atom_chars(H,Chars),append(Left,[L,U|MORE],Chars),!,char_type(L,lower),char_type(U,upper),append(Left,[L],LL),atom_chars(LS,LL),atom_chars(RS,[U|MORE]),make_l_summary([LS,RS],HL).
make_l_summary(H,OUT):-longer_sumry(H,Sum),flatten([Sum],OUT).
make_l_summary(H,[A]):- term_to_atom(H,A).


longer_sumry(fmt,'Format').
longer_sumry(pred,'Predicate').
longer_sumry(var,'Variable').



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
mpred_show_doc(What):- findall(Infos,mpred_show_doc(What,Infos),LInfos),LInfos\=[],flatten(LInfos,Infos),forall(member(E,Infos),format('~N~w~n',[E])).
mpred_show_doc(What):- match_predicates(What, Preds), Preds\==[], Preds\==[What],forall(member(M:F/A,Preds),mpred_show_doc(M:F/A)).


mpred_show_doc(M:F/A,['$mode'(PI, Det)]):-functor(PI,F,A), clause(M:'$mode'(PI, Det),true).
mpred_show_doc(M:F/A,[Info,Info2]):-   clause(M:'$pldoc'(F/A,_FL,Info,Info2),true).
mpred_show_doc(Id,[Info,Info2]):- clause(_,'$pldoc'(Id,_FL,Info,Info2),true).
mpred_show_doc(M,[Title,Info,Info2]):- pldoc_process:doc_comment(M:module(Title),_FileLines,Info,Info2).


% ?- help(term_expansion/2).
% ?- help(match_regex/2).
%:- ensure_loaded(library(check)).
%:- ensure_loaded(library(make)).

mpred_type_module(A):- \+ atom(A),!,fail.
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
mpred_prolog_only_module(M):- logicmoo_util_help:mpred_is_impl_file(F),make_module_name(F,M).
mpred_prolog_only_module(M):- current_module(M),atom_concat(logicmoo_utils_,_,M).
% mpred_prolog_only_module(user). 
:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).


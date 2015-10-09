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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_varnames.pl
:- module(logicmoo_varnames,
          [ 
          e2h/2,
          longer_sumry/2,
          make_l_summary/2,
          make_subterm_path/3,
          make_summary/2,
          make_summary0/2,
          mpred_impl_module/1,
          name_modes/3,
          print_fake_doc/2,
          target_module/2
 ]).
:- multifile
        make_hook/2.
% :- meta_predicate.
:- module_transparent
            e2h/2,
            longer_sumry/2,
            make_l_summary/2,
            make_subterm_path/3,
            make_summary/2,
            make_summary0/2,
            name_modes/3,
            print_fake_doc/2
          target_module/2
          .

/*

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

*/

:- dynamic(lmconfig:mpred_is_impl_file/1).
:- multifile(lmconfig:mpred_is_impl_file/1).
:- volatile(lmconfig:mpred_is_impl_file/1).

target_module(P,M):-mpred_source_file(P,F),lmconfig:mpred_is_impl_file(F),make_module_name(F,M).

print_fake_doc(M,P):- 
   scan_source_files_for_varnames,
   target_module(MN,M),
   current_predicate(_,M:P),
   once(to_fa(P,F,A)),
   mpred_impl_module(M),
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

e2h(A,H):-atom(A),atom_number(A,N),!,e2h(N,H).
e2h(i,internal_interface).
e2h(u,user_interface).
e2h(t,data_structur).
e2h(0,prime_helper).
e2h(E,H):-number(E),atomic_list_concat([helper,E,n],'',H).

make_l_summary([],[]).
make_l_summary([H1,H2|T],OUT):- append(Left,[A],[H1,H2|T]),e2h(A,H),append(Left,[H],AGAIN),make_l_summary(AGAIN,OUT).
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


mpred_show_doc(M:F/A,['$mode'(PI, Det)]):-functor(PI,F,A),M:'$mode'(PI, Det).
mpred_show_doc(M:F/A,[Info,Info2]):-M:'$pldoc'(F/A,_FL,Info,Info2).
mpred_show_doc(Id,[Info,Info2]):- '$pldoc'(Id,_FL,Info,Info2).
mpred_show_doc(M,[Title,Info,Info2]):- pldoc_process:doc_comment(M:module(Title),_FileLines,Info,Info2).


% ?- help(term_expansion/2).
% ?- help(match_regex/2).
%:- ensure_loaded(library(check)).
%:- ensure_loaded(library(make)).


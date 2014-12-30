

:-swi_export((term_listing/1)).
term_listing([]):-!.
term_listing(Match):-
   ignore((catch(listing(Match),_,fail))),
   doall((
      synth_clause_for(H,B),
      once(ok_pred(H)),
      once(use_term_listing(Match,H,B)),
      show_term_listing(H,B),
      fail)).

synth_clause_for(H,B):- cur_predicate(H,_),synth_clause_db(H,B).

synth_clause_db(H,info(Props)):- once(pred_info(H,Props)).
synth_clause_db(H,B):- predicate_property(M:H,number_of_clauses(_)),!,clause(M:H,B).

:-swi_export((use_term_listing/3)).
use_term_listing(noinfo,_,info(_)):-!,fail. 
use_term_listing(HO,H,B):- atom(HO),!, use_term_listing_2(exact,HO,H,B).
use_term_listing([],_,_):-!.
use_term_listing([F1],H,B):-!,use_term_listing(F1,H,B),!.
use_term_listing([F1|FS],H,B):-!,use_term_listing(F1,H,B),!,use_term_listing(FS,H,B),!.
use_term_listing((F1,FS),H,B):-!,use_term_listing(F1,H,B),!,use_term_listing(FS,H,B),!.
use_term_listing((F1;FS),H,B):-!,use_term_listing(F1,H,B);use_term_listing(FS,H,B).
use_term_listing(arity(A),H,_):-!,functor(H,_,A).
use_term_listing(functor(F),H,_):-!,functor(H,F,_).
use_term_listing(not(C),H,B):-nonvar(C),!,not(use_term_listing(C,H,B)).
use_term_listing(-(C),H,B):-nonvar(C),!,not(use_term_listing(C,H,B)).
use_term_listing(+(C),H,B):-nonvar(C),!,(use_term_listing(C,H,B)).
use_term_listing(module(M),H,_):-!,predicate_property(H,imported_from(M)).
use_term_listing(M:HO,H,B):-!,use_term_listing(module(M),H,B),!,use_term_listing(h(HO),H,B).
use_term_listing(F/A,H,_):-atom(F),functor(H,F,A),!.
use_term_listing(h(P),H,_):-!,use_term_listing(P,H,666666).
use_term_listing(b(P),_,B):-!,use_term_listing(P,666666,B).
use_term_listing(HO,H,B):- string(HO),!, use_term_listing_2(contains,HO,H,B).
use_term_listing(contains(HO),H,B):-!, use_term_listing_2(contains,HO,H,B).
use_term_listing(HO,H,B):- !,use_term_listing_2(exact,HO,H,B).

use_term_listing_2(contains,HO,H,B):- any_to_string(HO,HS),!, with_output_to(string(H1B1),write_canonical((H:-B))), (sub_atom_icasechk(HS,_,H1B1);sub_atom_icasechk(H1B1,_,HS)),!.
use_term_listing_2(exact,HO,H,B):- contains_var(HO,(H:-B)).

use_term_listing(HO,(H:-B)):-!, synth_clause_db(H,B), use_term_listing(HO,H,B).

:-dynamic cur_predicates/1.
cur_predicate(M:P,M:F/A):-
   current_predicate(M:F/A),functor(P,F,A),not(predicate_property(user:P,imported_from(_))).


ok_pred(F/A):-!,functor(P,F,A),ok_pred(P),!.
ok_pred(P):-not(bad_pred(P)).

bad_pred(M:P):-!,atom(M),bad_pred(P). 
bad_pred(P):-functor(P,F,A),arg(_,v(cur_predicates/_,db_op/_,db_op00/_,db_op0/_,db_op_loop/_,do_expand_args_l/3),F/A).
bad_pred(P):-predicate_property(P,autoloaded(_)).
bad_pred(P):-not(predicate_property(P,number_of_clauses(_))).
bad_pred(P):-predicate_property(P,imported_from(_)),predicate_property(P,static).
bad_pred(P):-predicate_property(P,foreign).

pred_info(H,Props):- findall(PP,mpred_prop(H,PP),Props).


show_term_listing(H,true):- !, show_term_listing(H).
show_term_listing(H,B):- show_term_listing((H:-B)).

show_term_listing(H):- not(not((snumbervars(H),writeq(H),write('.'),nl))),!.




show_all(Call):-doall((show_call(Call))).

alt_calls(call).
alt_calls(call_mpred).
alt_calls(is_asserted).
alt_calls(dbase_t).
alt_calls(req).
alt_calls(mreq).
alt_calls(ireq).

showall(Call):- doall(show_call(Call)).

findallCall(Args,Functor,ICallL,ICallLL):-  findall(Args,call(Functor,Args),ICallL),findall(Functor:C,member(C,ICallL),ICallLL).

sreq(Call):-
 into_mpred_form(Call,MCall),functor_h(MCall,MF), findall(P,pred_info(MF,P),Props),dmsg(props=Props),
   dmsg(call=Call),dmsg(call=MCall),
 % some calls remember deduced fasts and we need to prevent that
 with_assertions(readOnlyDatabases,
                (
           (is_callable(Call)-> findallCall(Call,call,CallL,CallLL) ; (CallL=[];CallLL=[])),
                 findallCall(Call,call_mpred,MCallL,MCallLL),
                 findallCall(Call,dbase_t,DCallL,DCallLL),
                 findallCall(Call,is_asserted,ACallL,ACallLL),
                 findallCall(Call,req,RCallL,RCallLL),
                 findallCall(Call,ireq,ICallL,ICallLL))),
   flatten([CallL,MCallL,DCallL,ACallL,RCallL,ICallL],ALL),
   flatten([CallLL,MCallLL,DCallLL,ACallLL,RCallLL,ICallLL],WITHFUNCTOR),
   list_to_set(ALL,SET),
                 showDif(SET,call,CallL,WITHFUNCTOR),
                 showDif(SET,call_mpred,MCallL,WITHFUNCTOR),
                 showDif(SET,dbase_t,DCallL,WITHFUNCTOR),
                 showDif(SET,is_asserted,ACallL,WITHFUNCTOR),
                 showDif(SET,req,RCallL,WITHFUNCTOR),
                 showDif(SET,ireq,ICallL,WITHFUNCTOR).

showDif(SET,Named,LIST,_WITHFUNCTOR):-
      list_to_set(LIST,ULIST),
      length(SET,SL),
      length(LIST,LL),
      length(ULIST,UL),
      fmt(Named=[l(LL),s(SL),u(UL)]),
      nl,
      showListWithCounts(ULIST,LIST),nl.

showListWithCounts(ULIST,[]):- fmt(ulist=ULIST).
showListWithCounts([],ALL):- fmt(missing=ALL).
showListWithCounts(ULIST,LIST):-ULIST=LIST,fmt(same=ULIST).
showListWithCounts(ULIST,LIST):-showCounts(ULIST,LIST).
showCounts([],_).
showCounts([H|L],OTHER):- occurrences_of_term(H,OTHER,N),write_count(H,N),showCounts(L,OTHER).

write_count(H,N):- writeq(H:N),write(', ').








/*  File         : syncol.pl
    Purpose      : interface SWI-prolog syntax coloring library
    pqSource     : interfacing SWI-Prolog source files and Qt
    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013, Carlo Capelli
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

% :- module(syncol, [syncol/2, recolor/4, syncolours/0, syncol_allfile/2]).

:- use_module(library(prolog_xref)).
:- use_module(library(prolog_colour)).
:- use_module(library(prolog_source)).


%% call the main interface to get a source colourized
%
%  see http://www.swi-prolog.org/pldoc/doc_for?object=prolog_colour:prolog_colourise_stream/3
%  and http://www.swi-prolog.org/pldoc/doc_for?object=prolog_xref:xref_source/2
%
%  note C is Qt callback argument: see PREDICATE(callback, 4) in pqSource.cpp
%
syncol(F, C) :-
  load_source(F),
  xref_source(F),
  open(F, read, S),
  prolog_colourise_stream(S, F, callback(C)),
  close(S).

%% recolor(+Text, +File, +CallbackArg, -PositionsOrError)
%
recolor(Text, File, CallbackArg, ErrorPos) :-
  atom_to_memory_file(Text, H),
  open_memory_file(H, read, S, [free_on_close(true)]),
  prolog_colourise_term(S, File, callback(CallbackArg), [subterm_positions(PositionsOrError)]),
  ( PositionsOrError = error_position(_StartClause, _EndClause, ErrorPos) ; true ),
  close(S).

/*
read_clause_positions(Atom, Module, Offset, Term, Positions, Error) :-
  atom_to_memory_file(Atom, H),
  open_memory_file(H, read, S, [free_on_close(true)]),
  read_source_term_at_location(S, Term, [module(Module), offset(Offset), subterm_positions(Positions), error(Error)]),
  close(S).
*/

%% list known fragments
%
syncolours :-
    forall(syntax_colour(C,A), pqSource:class_attributes(C,A)).

%% process entire file in Prolog, get back results list
%
:- thread_local frag/4.

syncol_allfile(F, L) :-
    retractall(frag(_,_,_,_)),
    load_source(F),
    xref_source(F),
    open(F, read, S),
    prolog_colourise_stream(S, F, callback_allfile),
    close(S),
    setof(frag(A, B, C, D), frag(A, B, C, D), L).

callback_allfile(U, V, Z) :-
    syntax_colour(U, Attributes) -> assertz(frag(V, Z, U, Attributes)) ; assertz(frag(V, Z, U, [])).

%%  apply useful behaviour changes
%
load_source(F) :-
    user:load_files(F, [silent(true),redefine_module(true)]).

/*
callback_allfile(U,V,Z) :-
        (   syntax_colour(U, Attributes), Attributes \= []
        ->  assertz(frag(V, Z, U, Attributes))
        ;   true
        ).
*/

%:- text_to_stream("hi(there). % bob\n:- a.\n",S) read_term(S,Term,[comments(C),subterm_positions(STP),syntax_errors(dec10),variable_names(VN)]).

:-export(pltext/1).
pltext(T):-text_to_stream(T,S),repeat,
 read_term(S,Term,[comments(C),subterm_positions(STP),syntax_errors(dec10),variable_names(VN)]),
 fmt(read_term(S,Term,[comments(C),subterm_positions(STP),syntax_errors(dec10),variable_names(VN)])),
 Term==end_of_file.



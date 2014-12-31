
:- module(dbase_i_pldoc,
	  [ 		% +Source, +OutStream, +Options
          source_to_txt/3,
          source_to_txt/1,
          s_to_html/3
	  ]).

:-export((term_listing/1)).
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

:-export((use_term_listing/3)).
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




transform_term(TermIn,TermOut):-nohtml,get_frag_class(State),transform_term(TermIn,State,TermOut).

transform_term(Term,Same,Term):- member(Same,[nofile,functor,int]),!.
transform_term(Term,Atom,TermO):-atom(Atom),!,TermO=..[Atom,Term].
transform_term(Term,_,Term):-!.
transform_term(Term,State,nohtml(State,Term)).



/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2014, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(prolog_colour)).
:- use_module(library('pldoc/doc_colour')).
:- use_module(library('pldoc/doc_html')).
:- use_module(library('pldoc/doc_wiki')).
:- use_module(library('pldoc/doc_modes')).
:- use_module(library('pldoc/doc_process')).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(prolog_xref)).

:- meta_predicate
	s_to_html(+, +, ^).

:- meta_predicate
	source_to_txt(+, +, ^).

:- meta_predicate 
       source_to_txt(+).

source_to_txt(S):- source_to_txt(S,stream(current_output),[]).

source_to_txt(S,Out,Opts):- 
  asserta(nohtml),
  debug(htmlsrc),
   setup_call_cleanup(
     true,
     s_to_html(S,Out,[header(false)|Opts]),
     retract(nohtml)).


:-thread_local nohtml/0.

/** <module> HTML source pretty-printer

This module colourises Prolog  source  using   HTML+CSS  using  the same
cross-reference based technology as used by PceEmacs.

@tbd	Create hyper-links to documentation and definitions.
@author Jan Wielemaker
*/

:- predicate_options(s_to_html/3, 3,
		     [ format_comments(boolean),
		       header(boolean),
		       skin(callable),
		       stylesheets(list),
		       title(atom)
		     ]).


:- thread_local
	lineno/0,			% print line-no on next output
	nonl/0,				% previous tag implies nl (block level)
	id/1.				% Emitted ids

%%	s_to_html(+In:filename, +Out, :Options) is det.
%
%	Colourise Prolog source as HTML. The idea   is to first create a
%	sequence of fragments and  then  to   apply  these  to the code.
%	Options are:
%
%	  * format_comments(+Boolean)
%	  If =true= (default), use PlDoc formatting for structured
%	  comments.
%
%	Other options are passed to the following predicates:
%
%	  * print_html_head/2
%	  * print_html_footer/2.
%	  * html_fragments/6
%
%	@param In	A filename.  Can also be an abstract name,
%			which is subject to library(prolog_source)
%			abstract file handling. See
%			prolog_open_source/2.  Note that this cannot
%			be a stream as we need to read the file three
%			times: (1) xref, (2) assign colours and (3)
%			generate HTML.
%	@param Out	Term stream(Stream) or file-name specification

s_to_html(Src, stream(Out), MOptions) :- !,
	meta_options(is_meta, MOptions, Options),
	(   option(title(_), Options)
	->  HeadOptions = Options
	;   file_base_name(Src, Title),
	    HeadOptions = [title(Title)|Options]
	),
	retractall(lineno),		% play safe
	retractall(nonl),		% play safe
	retractall(id(_)),
	colour_fragments(Src, Fragments),
	setup_call_cleanup(
	    ( open_source(Src, In),
	      asserta(user:thread_message_hook(_,_,_), Ref)
	    ),
	    ( print_html_head(Out, HeadOptions),
	      html_fragments(Fragments, In, Out, [], State, Options),
	      copy_rest(In, Out, State, State1),
	      pop_state(State1, Out, In)
	    ),
	    ( erase(Ref),
	      close(In)
	    )),
	print_html_footer(Out, Options).
s_to_html(Src, FileSpec, Options) :-
	absolute_file_name(FileSpec, OutFile, [access(write)]),
	setup_call_cleanup(
	    open(OutFile, write, Out, [encoding(utf8)]),
	    s_to_html(Src, stream(Out), Options),
	    close(Out)).

open_source(Id, Stream) :-
	prolog:xref_open_source(Id, Stream), !.
open_source(File, Stream) :-
	open(File, read, Stream).

is_meta(skin).

%%	print_html_head(+Out:stream, +Options) is det.
%
%	Print the =DOCTYPE= line and HTML header.  Options:
%
%		* header(Bool)
%		Only print the header if Bool is not =false=
%
%		* title(Title)
%		Title of the HTML document
%
%		* stylesheets(List)
%		Reference to the CSS style-sheets.
%
%		* format_comments(Bool)
%		If =true= (default), format structured comments.
%
%		* skin(Closure)
%		Called using call(Closure, Where, Out), where Where
%		is one of =header= or =footer=.  These calls are made
%		just after opening =body= and before closing =body=.

print_html_head(Out, Options) :-
	option(header(true), Options, true), !,
	option(title(Title), Options, 'Prolog source'),
	http_absolute_location(pldoc_resource('pldoc.css'), PlDocCSS, []),
	http_absolute_location(pldoc_resource('pllisting.css'), PlListingCSS, []),
	option(stylesheets(Sheets), Options, [PlListingCSS, PlDocCSS]),
	format(Out, '<!DOCTYPE html', []),
	format(Out, '<html>~n', []),
	format(Out, '  <head>~n', []),
	format(Out, '    <title>~w</title>~n', [Title]),
	forall(member(Sheet, Sheets),
	       format(Out, '    <link rel="stylesheet" type="text/css" href="~w">~n', [Sheet])),
	format(Out, '  </head>~n', []),
	format(Out, '<body>~n', []),
	skin_hook(Out, header, Options).
print_html_head(Out, Options) :-
	skin_hook(Out, header, Options).

print_html_footer(Out, Options) :-
	option(header(true), Options, true), !,
	skin_hook(Out, footer, Options),
	format(Out, '~N</body>~n', []),
	format(Out, '</html>', []).
print_html_footer(Out, Options) :-
	skin_hook(Out, footer, Options).

skin_hook(Out, Where, Options) :-
	option(skin(Skin), Options),
	call(Skin, Where, Out), !.
skin_hook(_, _, _).

noformat(_,_,_):-nohtml,!.
noformat(O,F,A):-format(O,F,A).

%%	html_fragments(+Fragments, +In, +Out, +State, +Options) is det.
%
%	Copy In to Out, inserting HTML elements using Fragments.

html_fragments([], _, _, State, State, _).
html_fragments([H|T], In, Out, State0, State, Options) :-
	html_fragment_new(H, In, Out, State0, State1, Options),
	html_fragments(T, In, Out, State1, State, Options).

%%	html_fragment(+Fragment, +In, +Out,
%%		      +StateIn, -StateOut, +Options) is det.
%
%	Print from current position upto the end of Fragment.  First
%	clause deals with structured comments.

:-thread_local(thlocal:frag_class/1).

get_frag_class(FC):-thlocal:frag_class(FC),!.
get_frag_class(noFC).

html_fragment_new(H, In, Out, State0, State1, Options):-
 class_from_frag(H,Class),
  with_assertions(thlocal:frag_class(Class),
   html_fragment(H, In, Out, State0, State1, Options)).

class_from_frag(H,A):-arg(3,H,A),!.
class_from_frag(_,noClass):-!.

html_fragment(fragment(Start, End, structured_comment, []),
	      In, Out, State0, [], Options) :-
	option(format_comments(true), Options, true), !,
	copy_without_trailing_white_lines(In, Start, Out, State0, State1),
	pop_state(State1, Out, In),
	Len is End - Start,
	read_n_codes(In, Len, Comment),
	is_structured_comment(Comment, Prefix),
	indented_lines(Comment, Prefix, Lines0),
	(   section_comment_header(Lines0, Header, Lines1)
	->  wiki_lines_to_dom(Lines1, [], DOM),
	    phrase(pldoc_html:html(div(class(comment),
				       [Header|DOM])), Tokens),
	    print_html(Out, Tokens)
	;   stream_property(In, file_name(File)),
	    line_count(In, Line),
	    (	xref_module(File, Module)
	    ->	true
	    ;	Module = user
	    ),
	    process_modes(Lines0, Module, File:Line, Modes, Args, Lines1),
	    maplist(assert_seen_mode, Modes),
	    DOM = [\pred_dt(Modes, pubdef, []), dd(class=defbody, DOM1)],
	    wiki_lines_to_dom(Lines1, Args, DOM0),
	    strip_leading_par(DOM0, DOM1),
	    phrase(pldoc_html:html(DOM), Tokens),		% HACK
	    noformat(Out, '<dl class="comment">~n', [Out]),
	    print_html(Out, Tokens),
	    noformat(Out, '</dl>~n', [Out])
	).
html_fragment(fragment(Start, End, structured_comment, []),
	      In, Out, State0, State, _Options) :- !,
	copy_to(In, Start, Out, State0, State1),
	line_count(In, StartLine),
	Len is End - Start,
	read_n_codes(In, Len, Comment),
	is_structured_comment(Comment, Prefix),
	indented_lines(Comment, Prefix, Lines),
	(   section_comment_header(Lines, _Header, _RestSectionLines)
	->  true
	;   stream_property(In, file_name(File)),
	    line_count(In, Line),
	    (	xref_module(File, Module)
	    ->	true
	    ;	Module = user
	    ),
	    process_modes(Lines, Module, File:Line, Modes, _Args, _Lines1),
	    maplist(mode_anchor(Out), Modes)
	),
	start_fragment(structured_comment, In, Out, State1, State2),
	copy_codes(Comment, StartLine, Out, State2, State3),
	end_fragment(Out, In, State3, State).
html_fragment(fragment(Start, End, Class, Sub),
	      In, Out, State0, State, Options) :-
	copy_to(In, Start, Out, State0, State1),
	start_fragment(Class, In, Out, State1, State2),
	html_fragments(Sub, In, Out, State2, State3, Options),
	copy_to(In, End, Out, State3, State4),	% TBD: pop-to?
	end_fragment(Out, In, State4, State).


start_fragment(atom, In, Out, State0, State) :- !,
	(   peek_code(In, C),
	    C == 39
	->  start_fragment(quoted_atom, In, Out, State0, State)
	;   State = [nop|State0]
	).
start_fragment(Class, _, Out, State, [Push|State]) :-
	element(Class, Tag, CSSClass), !,
	Push =.. [Tag,class(CSSClass)],
	(   anchor(Class, ID)
	->  noformat(Out, '<~w id="~w" class="~w">', [Tag, ID, CSSClass])
	;   noformat(Out, '<~w class="~w">', [Tag, CSSClass])
	).
start_fragment(Class, _, Out, State, [span(class(SpanClass))|State]) :-
	functor(Class, SpanClass, _),!,
	noformat(Out, '<span class="~w">', [SpanClass]).

end_fragment(_, _, [nop|State], State) :- !.
end_fragment(Out, In, [span(class(directive))|State], State) :- !,
	copy_full_stop(In, Out),
	noformat(Out, '</span>', []),
	(   peek_code(In, 10),
	    \+ nonl
	->  assert(nonl)
	;   true
	).
end_fragment(Out, _, [Open|State], State) :-
	retractall(nonl),
	functor(Open, Element, _),
	noformat(Out, '</~w>', [Element]).

pop_state([], _, _) :- !.
pop_state(State, Out, In) :-
	end_fragment(Out, In, State, State1),
	pop_state(State1, Out, In).


%%	anchor(+Class, -Label) is semidet.
%
%	True when Label is the =id= we   must  assign to the fragment of
%	class Class. This that  the  first   definition  of  a head with
%	the id _name/arity_.

anchor(head(_, Head), Id) :-
	callable(Head),
	functor(Head, Name, Arity),
	format(atom(Id), '~w/~w', [Name, Arity]),
	(   id(Id)
	->  fail
	;   assertz(id(Id))
	).

mode_anchor(Out, Mode) :-
	mode_anchor_name(Mode, Id),
	(   id(Id)
	->  true
	;   noformat(Out, '<span id="~w"><span>', [Id]),
	    assertz(id(Id))
	).

assert_seen_mode(Mode) :-
	mode_anchor_name(Mode, Id),
	(   id(Id)
	->  true
	;   assertz(id(Id))
	).

%%	copy_to(+In:stream, +End:int, +Out:stream, +State) is det.
%
%	Copy data from In to Out   upto  character-position End. Inserts
%	HTML entities for HTML the reserved characters =|<&>|=. If State
%	does not include a =pre= environment,   create  one and skip all
%	leading blank lines.

copy_to(In, End, Out, State, State) :-
	member(pre(_), State), !,
	copy_to(In, End, Out).
copy_to(In, End, Out, State, [pre(class(listing))|State]) :-
	noformat(Out, '<pre class="listing">~n', [Out]),
	line_count(In, Line0),
	read_to(In, End, Codes0),
	delete_leading_white_lines(Codes0, Codes, Line0, Line),
	assert(lineno),
	write_codes(Codes, Line, Out).

transform_codes(Codes,CodesOut):-
  catch(read_term_from_codes(Codes,Term,[comments(Cmts),variable_names(VNs)]),_,fail),Cmts==[],
   transform_term(Term,NewTerm),
   Term \=@= NewTerm,!,
   write_term_to_codes(NewTerm,CodesOut,[variable_names(VNs)]),!.
transform_codes(Codes,Codes).

copy_codes(Codes, Line, Out, State, State):- transform_codes(Codes,CodesOut),copy_codes1(CodesOut, Line, Out, State, State).

copy_codes1(Codes, Line, Out, State, State) :-
	member(pre(_), State), !,
	write_codes(Codes, Line, Out).
copy_codes1(Codes0, Line0, Out, State, State) :-
	noformat(Out, '<pre class="listing">~n', []),
	delete_leading_white_lines(Codes0, Codes, Line0, Line),
	assert(lineno),
	write_codes(Codes, Line, Out).


%%	copy_full_stop(+In, +Out) is det.
%
%	Copy upto and including the .

copy_full_stop(In, Out) :-
	get_code(In, C0),
	copy_full_stop(C0, In, Out).

copy_full_stop(0'., _, Out) :- !,
	put_code(Out, 0'.).
copy_full_stop(C, In, Out) :-
	put_code(Out, C),
	get_code(In, C2),
	copy_full_stop(C2, In, Out).


%%	delete_leading_white_lines(+CodesIn, -CodesOut, +LineIn, -Line) is det.
%
%	Delete leading white lines. Used  after structured comments. The
%	last two arguments update the  start-line   number  of the <pre>
%	block that is normally created.

delete_leading_white_lines(Codes0, Codes, Line0, Line) :-
	append(LineCodes, [10|Rest], Codes0),
	all_spaces(LineCodes), !,
	Line1 is Line0 + 1,
	delete_leading_white_lines(Rest, Codes, Line1, Line).
delete_leading_white_lines(Codes, Codes, Line, Line).

%%	copy_without_trailing_white_lines(+In, +End, +StateIn, -StateOut) is det.
%
%	Copy input, but skip trailing white-lines. Used to copy the text
%	leading to a structured comment.

copy_without_trailing_white_lines(In, End, Out, State, State) :-
	member(pre(_), State), !,
	line_count(In, Line),
	read_to(In, End, Codes0),
	delete_trailing_white_lines(Codes0, Codes),
	write_codes(Codes, Line, Out).
copy_without_trailing_white_lines(In, End, Out, State0, State) :-
	copy_to(In, End, Out, State0, State).

delete_trailing_white_lines(Codes0, []) :-
	all_spaces(Codes0), !.
delete_trailing_white_lines(Codes0, Codes) :-
	append(Codes, Tail, [10|Rest], Codes0), !,
	delete_trailing_white_lines(Rest, Tail).
delete_trailing_white_lines(Codes, Codes).

%%	append(-First, -FirstTail, ?Rest, +List) is nondet.
%
%	Split List.  First part is the difference-list First-FirstTail.

append(T, T, L, L).
append([H|T0], Tail, L, [H|T]) :-
	append(T0, Tail, L, T).

all_spaces([]).
all_spaces([H|T]) :-
	code_type(H, space),
	all_spaces(T).

copy_to(In, End, Out) :-
	line_count(In, Line),
	read_to(In, End, Codes),
	(   debugging(htmlsrc)
	->  length(Codes, Count),
	    debug(htmlsrc, 'Copy ~D chars: ~s', [Count, Codes])
	;   true
	),
	write_codes(Codes, Line, Out).

read_to(In, End, Codes) :-
	character_count(In, Here),
	Len is End - Here,
	read_n_codes(In, Len, Codes).

%%	write_codes(+Codes, +Line, +Out) is det.
%
%	Write codes that have been read starting at Line.

write_codes(A,B,C):-transform_codes(A,AA),write_codes1(AA,B,C).

write_codes1([], _, _).
write_codes1([H|T], L0, Out) :-
	content_escape(H, Out, L0, L1),
	write_codes1(T, L1, Out).

%%	content_escape(+Code, +Out, +Line0, -Line) is det
%
%	Write Code to Out, while taking care of.
%
%		* Use HTML entities for =|<&>|=
%		* If a line-no-tag is requested, write it
%		* On \n, post a line-no request.  If nonl/0 is set,
%		  do _not_ emit a newline as it is implied by the
%		  closed environment.

content_escape(_, Out, L, _) :-
	(   lineno
	->  retractall(lineno),
	    write_line_no(L, Out),
	    fail
	;   fail
	).
content_escape(0'\n, Out, L0, L) :- !,
	L is L0 + 1,
	(   retract(nonl)
	->  true
	;   nl(Out)
	),
	assert(lineno).

content_escape(C, Out, L, L) :- nohtml,!,
	put_code(Out, C).

content_escape(0'<, Out, L, L) :- !,
	format(Out, '&lt;', []).
content_escape(0'>, Out, L, L) :- !,
	format(Out, '&gt;', []).
content_escape(0'&, Out, L, L) :- !,
	format(Out, '&amp;', []).
content_escape(C, Out, L, L) :-
	put_code(Out, C).

write_line_no(LineNo, Out) :-
	noformat(Out, '<span class="line-no">~|~t~d~4+</span>', [LineNo]).

%%	copy_rest(+In, +Out, +StateIn, -StateOut) is det.
%
%	Copy upto the end of the input In.

copy_rest(In, Out, State0, State) :-
	copy_to(In, -1, Out, State0, State).

%%	read_n_codes(+In, +N, -Codes)
%
%	Read the next N codes from In as a list of codes. If N < 0, read
%	upto the end of stream In.

read_n_codes(_, N, Codes) :-
	N =< 0, !,
	Codes = [].
read_n_codes(In, N, Codes) :-
	get_code(In, C0),
	read_n_codes(N, C0, In, Codes).

read_n_codes(_, -1, _, []) :- !.
read_n_codes(1, C, _, [C]) :- !.
read_n_codes(N, C, In, [C|T]) :-
	get_code(In, C2),
	N2 is N - 1,
	read_n_codes(N2, C2, In, T).


%%	element(+Class, -HTMLElement, -CSSClass) is nondet.
%
%	Map classified objects to an  HTML   element  and CSS class. The
%	actual  clauses  are  created   from    the   1st   argument  of
%	prolog_src_style/2.

term_expansion(element(_,_,_), Clauses) :-
	findall(C, element_clause(C), Clauses).

%element_tag(directive, div) :- !.
element_tag(_, span).

element_clause(element(Term, Tag, CSS)) :-
	span_term(Term, CSS),
	element_tag(Term, Tag).

span_term(Classification, Class) :-
	syntax_colour(Classification, _Attributes),
	css_class(Classification, Class).

css_class(Class, Class) :-
	atom(Class), !.
css_class(Term, Class) :-
	Term =.. [P1,A|_],
	(   var(A)
	->  Class = P1
	;   css_class(A, P2),
	    atomic_list_concat([P1, -, P2], Class)
	).

element(_,_,_).				% term expanded


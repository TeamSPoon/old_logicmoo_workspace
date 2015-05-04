/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2001-2014, University of Amsterdam
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

% :- module(logicmoo_i_www,[ html_print_term/2  ]).  % +Term, +Options
	  
:- include(logicmoo(mpred/logicmoo_i_header)).

:- use_module(library(option)).
:- style_check(-discontiguous). %  toMarkup/4, toMarkupFormula/4.
:- style_check(-singleton).

/** <module> Pretty Print Prolog terms

This module is a first  start  of   what  should  become a full-featured
pretty printer for Prolog  terms  with   many  options  and  parameters.
Eventually,  it  should  replace  portray_clause/1   and  various  other
special-purpose predicates.

@tbd This is just a quicky. We  need proper handling of portray/1, avoid
printing very long terms  multiple   times,  spacing (around operators),
etc.

@tbd Use a record for the option-processing.

@tbd The current ahtml_pproach is far too simple, often resulting in illegal
     terms.
*/

:- predicate_options(html_print_term/2, 2,
		     [ output(stream),
		       right_margin(integer),
		       left_margin(integer),
		       tab_width(integer),
		       indent_arguments(integer),
		       operators(boolean),
		       write_options(list)
		     ]).

%%	html_print_term(+Term, +Options) is det.
%
%	Pretty print a Prolog term. The following options are processed:
%
%	  * output(+Stream)
%	  Define the output stream.  Default is =user_output=
%	  * right_margin(+Integer)
%	  Width of a line.  Default is 72 characters.
%	  * left_margin(+Integer)
%	  Left margin for continuation lines.  Default is 0.
%	  * tab_width(+Integer)
%	  Distance between tab-stops.  Default is 8 characters.
%	  * indent_arguments(+Spec)
%	  Defines how arguments of compound terms are placed.  Defined
%	  values are:
%	    $ =false= :
%	    Simply place them left to right (no line-breaks)
%	    $ =true= :
%	    Place them vertically, aligned with the open bracket (not
%	    implemented)
%	    $ =auto= (default) :
%	    As horizontal if line-width is not exceeded, vertical
%	    otherwise.
%	    $ An integer :
%	    Place them vertically aligned, <N> spaces to the right of
%	    the beginning of the head.
%	  * operators(+Boolean)
%	  This is the inverse of the write_term/3 option =ignore_ops=.
%	  Default is to respect them.
%	  * write_options(+List)
%	  List of options passed to write_term/3 for terms that are
%	  not further processed.  Default:
%	    ==
%		[ numbervars(true),
%		  quoted(true),
%		  portray(true)
%	        ]
%	    ==

html_print_term(Term, Options) :-
	\+ \+ html_print_term_2(Term, Options).

html_print_term_2(Term, Options0) :-
	prepare_term(Term, Template, Cycles, Constraints),
	defaults(Defs),
	merge_options(Options0, Defs, Options),
	option(write_options(WrtOpts), Options),
	option(max_depth(MaxDepth), WrtOpts, infinite),
	option(left_margin(LeftMargin), Options, 0),
	Context	= ctx(LeftMargin,0,1200,MaxDepth),
	html_pp(Template, Context, Options),
	html_print_extra(Cycles, Context, 'where', Options),
	html_print_extra(Constraints, Context, 'with constraints', Options).

html_print_extra([], _, _, _) :- !.
html_print_extra(List, Context, Comment, Options) :-
	option(output(Out), Options),
	format(Out, ', % ~w', [Comment]),
	modify_context(Context, [indent=4], Context1),
	html_print_extra_2(List, Context1, Options).

html_print_extra_2([H|T], Context, Options) :-
	option(output(Out), Options),
	context(Context, indent, Indent),
	indent(Out, Indent, Options),
	html_pp(H, Context, Options),
	(   T == []
	->  true
	;   format(Out, ',', []),
	    html_print_extra_2(T, Context, Options)
	).


%%	prepare_term(+Term, -Template, -Cycles, -Constraints)
%
%	Prepare a term, possibly  holding   cycles  and  constraints for
%	printing.

prepare_term(Term, Template, Cycles, Constraints) :-
	term_attvars(Term, []), !,
	Constraints = [],
	'$factorize_term'(Term, Template, Factors),
	bind_non_cycles(Factors, 1, Cycles),
	numbervars(Template+Cycles+Constraints, 0, _,
		   [singletons(true)]).
prepare_term(Term, Template, Cycles, Constraints) :-
	copy_term(Term, Copy, Constraints), !,
	'$factorize_term'(Copy, Template, Factors),
	bind_non_cycles(Factors, 1, Cycles),
	numbervars(Template+Cycles+Constraints, 0, _,
		   [singletons(true)]).


bind_non_cycles([], _, []).
bind_non_cycles([V=Term|T], I, L) :-
	unify_with_occurs_check(V, Term), !,
	bind_non_cycles(T, I, L).
bind_non_cycles([H|T0], I, [H|T]) :-
	H = ('$VAR'(Name)=_),
	atom_concat('_S', I, Name),
	I2 is I + 1,
	bind_non_cycles(T0, I2, T).


defaults([ output(user_output),
	   right_margin(72),
	   indent_arguments(auto),
	   operators(true),
	   write_options([ quoted(true),
			   numbervars(true),
			   portray(true),
			   attributes(portray)
			 ])
	 ]).


		 /*******************************
		 *	       CONTEXT		*
		 *******************************/

context_attribute(indent,     1).
context_attribute(depth,      2).
context_attribute(precedence, 3).
context_attribute(max_depth,  4).

context(Ctx, Name, Value) :-
	context_attribute(Name, Arg),
	arg(Arg, Ctx, Value).

modify_context(Ctx0, Mahtml_pping, Ctx) :-
	functor(Ctx0, Name, Arity),
	functor(Ctx,  Name, Arity),
	modify_context(0, Arity, Ctx0, Mahtml_pping, Ctx).

modify_context(Arity, Arity, _, _, _) :- !.
modify_context(I, Arity, Ctx0, Mahtml_pping, Ctx) :-
	N is I + 1,
	(   context_attribute(Name, N),
	    memberchk(Name=Value, Mahtml_pping)
	->  true
	;   arg(N, Ctx0, Value)
	),
	arg(N, Ctx, Value),
	modify_context(N, Arity, Ctx0, Mahtml_pping, Ctx).


dec_depth(Ctx, Ctx) :-
	context(Ctx, max_depth, infinite), !.
dec_depth(ctx(I,D,P,MD0), ctx(I,D,P,MD)) :-
	MD is MD0 - 1.


		 /*******************************
		 *	        PP		*
		 *******************************/

html_pp(Primitive, Ctx, Options) :-
	(   atomic(Primitive)
	;   var(Primitive)
	), !,
	html_pprint(Primitive, Ctx, Options).
html_pp(Portray, _Ctx, Options) :-
	option(write_options(WriteOptions), Options),
	option(portray(true), WriteOptions),
	option(output(Out), Options),
	with_output_to(Out, user:portray(Portray)), !.
html_pp(List, Ctx, Options) :-
	List = [_|_], !,
	context(Ctx, indent, Indent),
	context(Ctx, depth, Depth),
	option(output(Out), Options),
	option(indent_arguments(IndentStyle), Options),
	(   (   IndentStyle == false
	    ->	true
	    ;	IndentStyle == auto,
		html_print_width(List, Width, Options),
		option(right_margin(RM), Options),
		Indent + Width < RM
	    )
	->  html_pprint(List, Ctx, Options)
	;   format(Out, '[ ', []),
	    Nindent is Indent + 2,
	    NDepth is Depth + 1,
	    modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx),
	    html_html_pp_list_elements(List, NCtx, Options),
	    indent(Out, Indent, Options),
	    format(Out, ']', [])
	).
:- if(current_predicate(is_dict/1)).
html_pp(Dict, Ctx, Options) :-
	is_dict(Dict), !,
	dict_pairs(Dict, Tag, Pairs),
	option(output(Out), Options),
	option(indent_arguments(IndentStyle), Options),
	context(Ctx, indent, Indent),
	(   IndentStyle == false ; Pairs == []
	->  html_pprint(Dict, Ctx, Options)
	;   IndentStyle == auto,
	    html_print_width(Dict, Width, Options),
	    option(right_margin(RM), Options),
	    Indent + Width < RM		% fits on a line, simply write
	->  html_pprint(Dict, Ctx, Options)
	;   format(atom(Buf2), '~q{ ', [Tag]),
	    write(Out, Buf2),
	    atom_length(Buf2, FunctorIndent),
	    (   integer(IndentStyle)
	    ->	Nindent is Indent + IndentStyle,
	        (   FunctorIndent > IndentStyle
		->  indent(Out, Nindent, Options)
		;   true
		)
	    ;   Nindent is Indent + FunctorIndent
	    ),
	    context(Ctx, depth, Depth),
	    NDepth is Depth + 1,
	    modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx0),
	    dec_depth(NCtx0, NCtx),
	    html_html_pp_dict_args(Pairs, NCtx, Options),
	    BraceIndent is Nindent - 2,		% '{ '
	    indent(Out, BraceIndent, Options),
	    write(Out, '}')
	).
:- endif.
html_pp(Term, Ctx, Options) :-		% handle operators
	functor(Term, Name, Arity),
	current_op(Prec, Type, Name),
	match_op(Type, Arity, Kind, Prec, Left, Right),
	option(operators(true), Options), !,
	option(output(Out), Options),
	context(Ctx, indent, Indent),
	context(Ctx, depth, Depth),
	context(Ctx, precedence, CPrec),
	NDepth is Depth + 1,
	modify_context(Ctx, [depth=NDepth], Ctx1),
	dec_depth(Ctx1, Ctx2),
	(   Kind == prefix
	->  arg(1, Term, Arg),
	    (   CPrec >= Prec
	    ->	format(atom(Buf), '~q ', Name),
		atom_length(Buf, AL),
		NIndent is Indent + AL,
		write(Out, Buf),
		modify_context(Ctx2, [indent=NIndent, precedence=Right], Ctx3),
		html_pp(Arg, Ctx3, Options)
	    ;	format(atom(Buf), '(~q ', Name),
		atom_length(Buf, AL),
		NIndent is Indent + AL,
		write(Out, Buf),
		modify_context(Ctx2, [indent=NIndent, precedence=Right], Ctx3),
		html_pp(Arg, Ctx3, Options),
		format(Out, ')', [])
	    )
	;   Kind == postfix
	->  arg(1, Term, Arg),
	    (   CPrec >= Prec
	    ->  modify_context(Ctx2, [precedence=Left], Ctx3),
	        html_pp(Arg, Ctx3, Options),
		format(Out, ' ~q', Name)
	    ;	format(Out, '(', []),
		NIndent is Indent + 1,
		modify_context(Ctx2, [indent=NIndent, precedence=Left], Ctx3),
		html_pp(Arg, Ctx3, Options),
		format(Out, ' ~q)', [Name])
	    )
	;   arg(1, Term, Arg1),
	    arg(2, Term, Arg2),
	    (	CPrec >= Prec
	    ->  modify_context(Ctx2, [precedence=Left], Ctx3),
		html_pp(Arg1, Ctx3, Options),
		format(Out, ' ~q ', Name),
		modify_context(Ctx2, [precedence=Right], Ctx4),
		html_pp(Arg2, Ctx4, Options)
	    ;	format(Out, '(', []),
		NIndent is Indent + 1,
		modify_context(Ctx2, [indent=NIndent, precedence=Left], Ctx3),
		html_pp(Arg1, Ctx3, Options),
		format(Out, ' ~q ', Name),
		modify_context(Ctx2, [precedence=Right], Ctx4),
		html_pp(Arg2, Ctx4, Options),
		format(Out, ')', [])
	    )
	).
html_pp(Term, Ctx, Options) :-		% compound
	option(output(Out), Options),
	option(indent_arguments(IndentStyle), Options),
	context(Ctx, indent, Indent),
	(   IndentStyle == false
	->  html_pprint(Term, Ctx, Options)
	;   IndentStyle == auto,
	    html_print_width(Term, Width, Options),
	    option(right_margin(RM), Options),
	    Indent + Width < RM		% fits on a line, simply write
	->  html_pprint(Term, Ctx, Options)
	;   Term =.. [Name|Args],
	    format(atom(Buf2), '~q(', [Name]),
	    write(Out, Buf2),
	    atom_length(Buf2, FunctorIndent),
	    (   integer(IndentStyle)
	    ->	Nindent is Indent + IndentStyle,
	        (   FunctorIndent > IndentStyle
		->  indent(Out, Nindent, Options)
		;   true
		)
	    ;   Nindent is Indent + FunctorIndent
	    ),
	    context(Ctx, depth, Depth),
	    NDepth is Depth + 1,
	    modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx0),
	    dec_depth(NCtx0, NCtx),
	    html_html_pp_compound_args(Args, NCtx, Options),
	    write(Out, ')')
	).


html_html_pp_list_elements(_, Ctx, Options) :-
	context(Ctx, max_depth, 0), !,
	option(output(Out), Options),
	write(Out, '...').
html_html_pp_list_elements([H|T], Ctx0, Options) :-
	dec_depth(Ctx0, Ctx),
	html_pp(H, Ctx, Options),
	(   T == []
	->  true
	;   nonvar(T),
	    T = [_|_]
	->  option(output(Out), Options),
	    write(Out, ','),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent, Options),
	    html_html_pp_list_elements(T, Ctx, Options)
	;   option(output(Out), Options),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent-2, Options),
	    write(Out, '| '),
	    html_pp(T, Ctx, Options)
	).


html_html_pp_compound_args([H|T], Ctx, Options) :-
	html_pp(H, Ctx, Options),
	(   T == []
	->  true
	;   T = [_|_]
	->  option(output(Out), Options),
	    write(Out, ','),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent, Options),
	    html_html_pp_compound_args(T, Ctx, Options)
	;   option(output(Out), Options),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent-2, Options),
	    write(Out, '| '),
	    html_pp(T, Ctx, Options)
	).


:- if(current_predicate(is_dict/1)).
html_html_pp_dict_args([Name-Value|T], Ctx, Options) :-
	option(output(Out), Options),
	line_position(Out, Pos0),
	html_pp(Name, Ctx, Options),
	write(Out, ':'),
	line_position(Out, Pos1),
	context(Ctx, indent, Indent),
	Indent2 is Indent + Pos1-Pos0,
	modify_context(Ctx, [indent=Indent2], Ctx2),
	html_pp(Value, Ctx2, Options),
	(   T == []
	->  true
	;   option(output(Out), Options),
	    write(Out, ','),
	    indent(Out, Indent, Options),
	    html_html_pp_dict_args(T, Ctx, Options)
	).
:- endif.

%	match_op(+Type, +Arity, +Precedence, -LeftPrec, -RightPrec)

match_op(fx,	1, prefix,  P, _, R) :- R is P - 1.
match_op(fy,	1, prefix,  P, _, P).
match_op(xf,	1, postfix, P, _, L) :- L is P - 1.
match_op(yf,	1, postfix, P, P, _).
match_op(xfx,	2, infix,   P, A, A) :- A is P - 1.
match_op(xfy,	2, infix,   P, L, P) :- L is P - 1.
match_op(yfx,	2, infix,   P, P, R) :- R is P - 1.


%%	indent(+Out, +Indent, +Options)
%
%	Newline and indent to the indicated  column. Respects the option
%	=tab_width=.  Default  is  8.  If  the  tab-width  equals  zero,
%	indentation is emitted using spaces.

indent(Out, Indent, Options) :-
	option(tab_width(TW), Options, 8),
	nl(Out),
	(   TW =:= 0
	->  tab(Out, Indent)
	;   Tabs is Indent // TW,
	    Spaces is Indent mod TW,
	    forall(between(1, Tabs, _), put(Out, 9)),
	    tab(Out, Spaces)
	).

%%	html_print_width(+Term, -W, +Options) is det.
%
%	Width required when printing `normally' left-to-right.

html_print_width(Term, W, Options) :-
	option(right_margin(RM), Options),
	(   write_length(Term, W, [max_length(RM)|Options])
	->  true
	;   W = RM
	).

%%	html_pprint(+Term, +Context, +Options)
%
%	The bottom-line print-routine.

html_pprint(Term, Ctx, Options) :-
	option(output(Out), Options),
	html_pprint(Out, Term, Ctx, Options).

html_pprint(Out, Term, Ctx, Options) :-
	option(write_options(WriteOptions), Options),
	context(Ctx, max_depth, MaxDepth),
	(   MaxDepth == infinite
	->  write_term(Out, Term, WriteOptions)
	;   MaxDepth =< 0
	->  format(Out, '...', [])
	;   write_term(Out, Term, [max_depth(MaxDepth)|WriteOptions])
	).



:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_error)).

% http_reply_from_files is here
:- use_module(library(http/http_files)).
% http_404 is in here
:- use_module(library(http/http_dispatch)).

:- multifile http:location/3.
:- dynamic   http:location/3.

:- use_module(library(http/http_parameters)).
% :- use_module(library(http/http_session)).
:- thread_property(_,alias('http@4002'))->true; http_server(http_dispatch, [port(4002)]).
:- http_handler(root(.), handler_cyclone, [chunked]).

%user:hook_one_second_timer_tick:- catch(mmake,_,true).

handler_cyclone(Request):- 
  format('Content-type: text/html~n~n',[]),
   print_request(Request),
  with_assertions(thlocal:print_mode(html),logOnError(handler_cyclone_0(Request))).
handler_cyclone_0(Request):-
   flag(matched_assertions,_,0),
   flag(show_asserions_offered,_,0),
   retractall(shown_subtype(_)),
   retractall(shown_clause(_)),
     http_parameters(Request,[ search(Q,[  optional(true) ] )]),
      make_page_for_obj(Q).

print_request([]).
print_request([H|T]) :-
        H =.. [Name, Value],
        format(user_error,'<tr><td>~w<td>~w~n', [Name, Value]),
        print_request(T).

% <meta http-equiv="refresh" content="30;http://prologmoo.com:4002/?search=~q">
% <link rel="SHORTCUT ICON" href="http://prologmoo.com:3602/cycdoc/img/cb/mini-logo.gif"><meta name="ROBOTS" content="NOINDEX, NOFOLLOW">
make_page_for_obj(Pred):- ignore(Pred=ttPredType),url_iri(PredURL,Pred), 
 format('<html><head><title>search for ~q</title></head>',[Pred]),
 format('<body class="yui-skin-sam"><strong><font face="verdana,arial,sans-serif"><font size="5">Object : </font></strong><strong><font size="5"><a href="?search=~q" target="_top">~q</a></font></strong><br/><pre>',[PredURL,Pred]),
    logOnErrorIgnore(make_page_pretext_obj(Pred)),
 format('</pre><hr><span class="copyright"><i>Copyright &copy; 1999 - 2015 <a href="http://prologmoo.com">LogicMOO/PrologMUD</a>.  All rights reserved.</i></span></body></html>~n~n',[]).

html_html_pp_ball(Color,Alt):-format(S,'<img src="http://prologmoo.com:3602/cycdoc/img/cb/~w.gif" alt="~w" align="top" border="0"></img>',[Color,Alt]).

make_page_pretext_obj(O):- ignore((catch(mmake,_,true))),
  ignore((catch(pfc_listing(O),_,true))),
  ignore((catch(pfc_lsting(O),_,true))),!.
   
:-thread_local(shown_subtype/1).
:-thread_local(shown_clause/1).

section_open(Type):-  once(shown_subtype(Type)->true;((thlocal:print_mode(html)->format('~n</pre><hr>~w<hr><pre>~n<font face="verdana,arial,sans-serif">',[Type]);(draw_line,format('% ~w~n%~n',[Type]))),asserta(shown_subtype(Type)))),!.
section_close(Type):- shown_subtype(Type)->(retractall(shown_subtype(Type)),(thlocal:print_mode(html)->format('</font>\n</pre><hr/><pre>',[]);draw_line));true.

pp_item_html(Type,done):-!,section_close(Type),!.
pp_item_html(_,H):-shown_clause(H),!.
pp_item_html(Type,H):- \+ thlocal:print_mode(html), pp_item_html_now(Type,H),!.
pp_item_html(Type,H):- ignore((flag(matched_assertions,X,X),between(0,400,X),pp_item_html_now(Type,H))).

pp_item_html_now(Type,H):-    
   flag(matched_assertions,X,X+1),!,
   pp_item_html_if_in_range(Type,H),!,
   assert(shown_clause(H)),!.

pp_item_html_if_in_range(Type,H):- section_open(Type),!,pp_ihtml(H),!,nl.


pp_ihtml(T):-isVarProlog(T),getVarAtom(T,N),format('~w',[N]).
pp_ihtml(done):-!.
pp_ihtml(T):-string(T),format('"~w"',[T]).
pp_ihtml((H:-true)):-pp_ihtml(H).
pp_ihtml(was_chain_rule(H)):- pp_ihtml(H).
pp_ihtml(is_edited_clause(H,B,A)):- pp_ihtml(proplist([(clause)=H,before=B,after=A])).
pp_ihtml(is_disabled_clause(H)):- pp_ihtml((disabled)=H).
pp_ihtml(spft(P,U,U)):- nonvar(U),!,pp_ihtml('u-deduced'=U:P).
pp_ihtml(spft(P,F,T)):- atom(F),atom(T),!, pp_ihtml('ft-deduced'=(F,T):P).
pp_ihtml(spft(P,F,T)):- atom(T),!,   pp_ihtml(proplist(['t-deduced'=T:P])),!.
pp_ihtml(spft(P,F,T)):- atom(F),!,  pp_ihtml(proplist(['f-deduced'=F:P ,  template=T])).
pp_ihtml(spft(P,F,T)):- !, pp_ihtml(proplist(['z-deduced'=P , format=F,  template=T])).
pp_ihtml(nt(Trigger,Test,Body)) :- !, pp_ihtml(proplist(['n-trigger'=Trigger , format=Test  ,  body=Body])).
pp_ihtml(pt(Trigger,Body)):-      pp_ihtml(proplist(['p-trigger'=Trigger ,  body=Body])).
pp_ihtml(bt(Trigger,Body)):-      pp_ihtml(proplist(['b-trigger'=Trigger ,  body=Body])).
pp_ihtml(proplist([N=V|Val])):- is_list(Val),!, pp_ihtml(N:-([V|Val])).
pp_ihtml(proplist(Val)):-!, pp_ihtml(:-(proplist(Val))).
pp_ihtml(proplist([])):-!.
pp_ihtml(proplist([H|T])):-format('~N'),pp_ihtml(H),format('~N'),pp_ihtml(proplist(T)).
pp_ihtml(H=T):-format('~N~w = ',[H]),pp_ihtml(T).
pp_ihtml(M:C):- (M=='';M==""),!,pp_ihtml(C),!.
pp_ihtml(M:C):- pp_ihtml(M),format(':'),pp_ihtml(C).
pp_ihtml(C):-  is_list(C),flag(indent,X,X),html_list_term(C),!,flag(indent,_,X).
pp_ihtml(C):- compound(C),flag(indent,X,X),functor(C,F,A),C=..[F|ARGSZ],html_compound(F,A,ARGSZ),!,flag(indent,_,X).
pp_ihtml(T):- atom(T),!,write_atom_link(T,T).
pp_ihtml(T):-format('~q',[T]).


% ===================================================
% Pretty Print Formula
% ===================================================


prefixed_html_term(Prefix,Pred):-format(Prefix),pp_ihtml(Pred).


% pred_href(Name/Arity, Module, HREF) :-
write_atom_link(A/_,N):-atom(A),!,write_atom_link(A,N).
write_atom_link(C,N):-compound(C),get_functor(C,F,A),!,write_atom_link(F/A,N).
write_atom_link(_,N):- \+ thlocal:print_mode(html),format('~q',[N]),!.
write_atom_link(A,N):-url_iri(URL,A),format('<a href="?search=~w">~w</a>',[URL,N]).

indent_nbsp(X):-thlocal:print_mode(html),forall(between(0,X,_),format('&nbsp;')),!.
indent_nbsp(X):-forall(between(0,X,_),format('~t',[])),!.

indent_nl:- fresh_line, flag(indent,X,X), indent_nbsp(X).


is_op_type(Functor, Type) :-
	current_op(_Pri, F, Functor),
	op_type(F, Type).

op_type(fx,  prefix).
op_type(fy,  prefix).
op_type(xf,  postfix).
op_type(yf,  postfix).
op_type(xfx, infix).
op_type(xfy, infix).
op_type(yfx, infix).
op_type(yfy, infix).

html_compound(F,A,Args):-  \+ thlocal:print_mode(html), C=..[F|Args],portray_clause(C).
html_compound(({}),1,[ARG1]):-
                write_atom_link(({})/1,'{'),  indent_nl, 
                flag(indent,X,X+1),
                pp_ihtml(ARG1),               
                format('}').


html_compound(F,2,[ARG1,ARG2|ARGS]):- is_op_type(F,infix),
                pp_ihtml(ARG1),  indent_nbsp(1), write_atom_link(F/2,F),  
                 indent_nl, pp_ihtml(ARG2), ignore(maplist(prefixed_html_term(', '), ARGS)),!.

html_compound(F,1,[ARG1|ARGS]):- is_op_type(F,prefix), 
                 write_atom_link(F/1,F),  indent_nl, 
                 pp_ihtml(ARG1), indent_nbsp(1),
                 ignore(maplist(prefixed_html_term(', '), ARGS)),!.

html_compound(F,1,[ARG1]):- is_op_type(F,postfix),
                 pp_ihtml(ARG1), write_atom_link(F/1,F).


html_compound(F,A,[ARG1|ARGS]):-                        
                write_atom_link(F/A,F),
                format('('),
                flag(indent,X,X+1),
                pp_ihtml(ARG1),               
                ignore(maplist(prefixed_html_term(', '), ARGS)),!,
                format(')').

html_list_term([ARG1|ARGS]):-
                format('['),
                flag(indent,X,X+1),
                pp_ihtml(ARG1),               
                ignore(maplist(prefixed_html_term(', '), ARGS)),!,
                format(']').
                



% ===================================================
% Make Args
% ===================================================

make_args_out(bach,[],Vars,''):-!.
make_args_out(bach,[C],Vars,ArgsOut):-
	 flag(indent,X,X),indent_nbsp(X,PreOut),!,
	 toMarkupFormula(bach,C,Vars,Chars1),
	 sformat(ArgsOut,'~w~w',[PreOut,Chars1]).

make_args_out(bach,[C|GS],Vars,ArgsOut):-
	 toMarkupFormula(bach,C,Vars,Chars1),
	 make_args_out(bach,GS,Vars,Chars2),!,
	 flag(indent,X,X),indent_nbsp(X,PreOut),!,
	 sformat(ArgsOut,'~w~w,~w',[PreOut,Chars1,Chars2]).


make_args_out(L,[],Vars,''):-!.
make_args_out(L,[C],Vars,ArgsOut):-
                toMarkupFormula(L,C,Vars,ArgsOut).
make_args_out(L,[C|GS],Vars,ArgsOut):-
                toMarkupFormula(L,C,Vars,Chars1),
                make_args_out(L,GS,Vars,Chars2),!,
                sformat(ArgsOut,'~w ~w',[Chars1,Chars2]).

indent_nbsp(0,''):-!.
indent_nbsp(1,'\n         '):-!.
indent_nbsp(X,Chars):-XX is X -1,!, indent_nbsp(XX,OutP),!,sformat(Chars,'~w   ',[OutP]),!.



writeMarkup(Term,Chars):- logOnError(toMarkup(html,Term,_,Chars)).


% ===============================================================
% toMarkup(-Markup,-Prolog,-PrologVarableList, +Output)
% example: toMarkup(html, explaination(('Military':996:subclass('IntransitiveRelation', 'BinaryRelation')^B)* ('Military':836:subclass('BinaryRelation', 'Relation')^C)*forall('IntransitiveRelation', forall(D, forall('Relation', holds(subclass, 'IntransitiveRelation', D)and holds(subclass, D, 'Relation')=>holds(subclass, 'IntransitiveRelation', 'Relation'))))*sfind(instance(subclass, 'PartialOrderingRelation'))*sfind(subclass('PartialOrderingRelation', 'TransitiveRelation'))* ('Military':2756:instance(on, 'IntransitiveRelation')^E)), ['X'=on|A],O),write_ln(O).
%
% Markup := [html,kif,pml,leml] (Expandable)
% Prolog := any prolog term
% PrologVaraibles list is the equal list as produced by read/3  [=(Name,Val)|...]
% Output is an CharicterAtom (the difference is this atom is not added the the symbol table)
% ===================================================================
% ===================================================================
% term_to_leml(-Prolog, +Output)
%
% arity 2 version (note html) is sufficient for printing values
% ===================================================================

% ===================================================================
% This File is the bootstrap for the Moo Infence engine one first calls "[belief_module]"
% So first is loads the proper files and then starts up the system
% There are no predicates defined in this file (it just uses other files' predicates)
% ===================================================================
% ===================================================================
% EXPORTS
% ===================================================================
isNonVar(Denotation):-not(isSlot(Denotation)).

% ===============================================================================================
% ===============================================================================================

isSlot(Denotation):-((isVarProlog(Denotation);isVarObject(Denotation))),!.

isSlot(Denotation,Denotation):- isVarProlog(Denotation),!.
isSlot(Denotation,PrologVar):- isVarObject(Denotation,PrologVar),!.

% ===============================================================================================
% ===============================================================================================

isHiddenSlot(Term):-fail.

% ===============================================================================================
% ===============================================================================================

isVarProlog(A):-((var(A);A='$VAR'(_))).

% ===============================================================================================
% ===============================================================================================

isVarObject(Denotation):-((
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value),!,isSlot(Value))).

isVarObject(Denotation,Value):-((
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value),!,isSlot(Value))).

% ===============================================================================================
% ===============================================================================================
	
isObject(Denotation,BaseType):-
	(((atom(BaseType) ->
		  (atom_concat('$',BaseType,F),functor(Denotation,F,2));
		  (functor(Denotation,F,2),atom_concat('$',BaseType,F))
		 ),!)).

% ===============================================================================================
% ===============================================================================================

isQualifiableAsClass(Atom):-atom(Atom),!.
isQualifiableAsClass('$Class'(Atom,_)):-atom(Atom),!.

isQualifiableAs(Denotation,BaseType,Value):-
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value).

% ===============================================================================================
% ===============================================================================================

isQualifiedAs(Denotation,_,_):-not(compound(Denotation)),!,fail.
isQualifiedAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value,SubType).
isQualifiedAs(Denotation,BaseType,Value,SubType):-
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value),
		  arg(2,Denotation,List),
		  lastImproperMember(BaseType,SubType,List).

% ===============================================================================================
% ===============================================================================================

lastImproperMember(Default,Default,List):-isVarProlog(List),!.
lastImproperMember(Default,Default,[]):-!.
lastImproperMember(Default,SubType,List):-proper_list(List),last(SubType,List).
lastImproperMember(Default,SubType,[SubType|End]):-isVarProlog(End),!.
lastImproperMember(Default,SubType,[_|Rest]):-
	lastImproperMember(Default,SubType,Rest),!.
	
% ===============================================================================================
% ===============================================================================================

isQualifiedAndKnownAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  not(isVarProlog(Value)).

% ===============================================================================================
% ===============================================================================================

isQualifiedAndVarAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  isVarProlog(Value).

% ===============================================================================================
% ===============================================================================================

isQualifiedAndVarAndUnifiable(Denotation,BaseType,NValue):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  (isVarProlog(Value);
		  (\+ \+ NValue=Value)),!.

% ===============================================================================================
% ===============================================================================================

isBodyConnective(Funct):-atom_concat(_,'_',Funct),!.
isBodyConnective(Funct):-atom_concat('t~',_,Funct),!.
isBodyConnective(Funct):-atom_concat('f~',_,Funct),!.
isBodyConnective(Funct):-member(Funct,[and,or,until,',',';',':-',unless,xor,holdsDuring]). % Other Propositional Wrahtml_ppers

isEntityref(Var,Var):-isSlot(Var),!.
isEntityref(Term,A):-Term=..[F,A,B],!,atom_concat('$',_,F),!.


% ===============================================================================================
% ===============================================================================================

isLiteralTerm(A):-isLiteralTerm_util(A),!.
isLiteralTerm(not(A)):-isLiteralTerm_util(A),!.

isLiteralTerm_util(A):-var(A),!.
isLiteralTerm_util('$VAR'(_)):-!.
isLiteralTerm_util(string(_)):-!.
isLiteralTerm_util(A):-not(compound(A)),!.
isLiteralTerm_util(A):-string(A).

% ===============================================================================================
% ===============================================================================================

isEntitySlot(Term):-isSlot(Term),!.
isEntitySlot(Term):-not(compound(Term)),!.
isEntitySlot(Term):-isEntityFunction(Term,FnT,ArgsT),!.

% ===============================================================================================
% ===============================================================================================

isEntityFunction(Term,FnT,ArgsT):-isSlot(Term),!,fail.
isEntityFunction(Term,FnT,ArgsT):-atomic(Term),!,fail.
isEntityFunction(Term,FnT,ArgsT):-Term=..[FnT|ArgsT],hlPredicateAttribute(FnT,'Function'),!.



% ===================================================================
% getPrologVars/4. 
% ===================================================================

getPrologVars(Term, Vars, Singletons, Multiples) :-
    ((getPrologVars(Term, VarList, []),
    close_list(VarList),
    keysort(VarList, KeyList),
    split_key_list(KeyList, Vars, Singletons, Multiples))).

getPrologVars(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getPrologVars(Term, V, V) :-not(compound(Term)),!.
getPrologVars(Term,  V0, V) :-
	isQualifiedAs(Term,Type,PrologVar),
	Type \= existential,!,
	(isVarProlog(PrologVar) -> V0=[PrologVar - x|V]; V0=V),!.
	
getPrologVars(Term, V0, V) :- 
	 functor(Term, F, N),
	 getPrologVars(1, N, Term, V0, V).
getPrologVars(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getPrologVars(Arg, V0, V1),
	J is I + 1,
	getPrologVars(J, N, Term, V1, V)
    ).

% ===================================================================
% getPrologVars/4. 
% ===================================================================

getAllPrologVars(Term, Vars, Singletons, Multiples) :-
    ((getAllPrologVars(Term, VarList, []),
    close_list(VarList),
    keysort(VarList, KeyList),
    split_key_list(KeyList, Vars, Singletons, Multiples))).

getAllPrologVars(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getAllPrologVars(Term, V, V) :-not(compound(Term)),!.
getAllPrologVars(Term, V0, V) :- 
	 functor(Term, F, N),
	 getAllPrologVars(1, N, Term, V0, V).
getAllPrologVars(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getAllPrologVars(Arg, V0, V1),
	J is I + 1,
	getAllPrologVars(J, N, Term, V1, V)
    ).

% ===================================================================
% getSlots/4. Returns no Existential Body Vars
% ===================================================================

getSlots(Term, Vars, Singletons, Multiples) :-
    ((getSlots(Term, VarList, []),
    close_list(VarList),
    keysort(VarList, KeyList),
    split_key_list(KeyList, Vars, Singletons, Multiples))).

getSlots(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getSlots(Term, V, V) :-not(compound(Term)),!.
getSlots(Term, V, V) :-isHiddenSlot(Term),!.
getSlots(Term,  VO, V) :-
	isQualifiedAs(Term,existential,EVar),!,
	getSlots(EVar,  VO, V).
getSlots(Term,  V0, V) :-
	isQualifiedAs(Term,Type,PrologVar),!,
	(isVarProlog(PrologVar) -> V0=[Term - x|V]; V0=V),!.
getSlots(Term, V0, V) :- 
	 functor(Term, F, N),
	 getSlots(1, N, Term, V0, V).
getSlots(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getSlots(Arg, V0, V1),
	J is I + 1,
	getSlots(J, N, Term, V1, V)
    ).


% ===================================================================
% ===================================================================

rnd(N):-number(N),X is random(100),!,X<N.

% ===================================================================
% ===================================================================
sameString(X,Y):-same_str(X,Y).

same_str(X,X):-!.
same_str(S,Y):-to_codes(S,SS),to_codes(Y,YS),!,SS=YS.

close_str(S,Y):-to_codes(S,SS),to_codes(Y,YS),!,toLowercase(SS,LSS),toLowercase(YS,LYS),!,LSS=LYS.

% ===================================================================
% ===================================================================

to_codes(A,O):-atom(A),!,atom_codes(A,O).
to_codes(S,O):-string(S),!,string_to_atom(S,A),to_codes(A,O).
to_codes(C,C).

% ===================================================================
% getConstants/4. 
% ===================================================================

getConstants(Types,Term, Vars, Singletons, Multiples) :-
    ((getConstants(Types,Term, VarList, []),
    close_list(VarList),
    keysort(VarList, KeyList),
    split_key_list(KeyList, Vars, Singletons, Multiples))).

getConstants(Types,Term, [Term - x|V], V) :- getConstants(Types,Term),!.
getConstants(Types,Term, V, V) :- var(Term),!.
getConstants(Types,Term,  FOUND, V) :-
            Term=..[L,I|ST],
            getConstants(Types,L, VL, []),
            consts_l(Types,[I|ST], FLIST),
            ahtml_ppend(V,FLIST,UND),
            ahtml_ppend(VL,UND,FOUND),!.

getConstants(Types,Term, V, V) :- !.
    
consts_l(Types,[],[]).
consts_l(Types,[L|IST], FLIST):-
         getConstants(Types,L, FOUND,[]), 
         consts_l(Types,IST, FOUNDMore), !,
         ahtml_ppend(FOUND,FOUNDMore,FLIST).

    
getConstants(_,('.')):-!,fail.
getConstants(_,'[]'):-!,fail.
getConstants(_,'$VAR'(_)):-!,fail.
getConstants(atomic,A):-atomic(A).
getConstants(atom,A):-atom(A).
getConstants(skolems,'zzskFn'(_)).
getConstants(funct,'AssignmentFn'(_,_)).



toMarkup(chat,Var,VS,Chars):-!,catch(toMarkup(bach,Var,VS,Chars),_,true),!.
toMarkup(java,Var,VS,Chars):-!,catch(toMarkup(html,Var,VS,Chars),_,true),!.

% VARIABLES
toMarkup(L,C,Vars,Chars):-isSlot(C),!,toMarkupSlot(L,C,Vars,Chars).

% COMPOUND
toMarkup(L,C,Vars,Chars):-compound(C),!,toMarkupFormula(L,C,Vars,Chars).


toMarkup(html,'$spacer',Vars,'\n<hr>\n').
toMarkup(_,'$spacer',Vars,'\n;; ------------------------------------------------------------------------------\n\n').

toMarkup(L,formula(C),Vars,Chars):-!,toMarkupFormula(L,C,Vars,Chars).

%Terminal Control
toMarkup(html,lparen,Vars,'('):-!.
toMarkup(html,rparen,Vars,')'):-!.
toMarkup(kif,lparen,Vars,'('):-!.
toMarkup(kif,rparen,Vars,')'):-!.
toMarkup(html,nl,Vars,'<br>'):-!.
toMarkup(html,tab,Vars,'<li>'):-!.
toMarkup(kif,nl,Vars,'\n'):-!.
toMarkup(kif,tab,Vars,'\t'):-!.

toMarkup(_,surf,Vars,''):-!.
toMarkup(_,end_of_file,Vars,''):-!.

toMarkup(_,',',Vars,'and'):-!.
toMarkup(_,';',Vars,'or'):-!.
toMarkup(_,'=',Vars,'equal'):-!.
toMarkup(_,'deduced',Vars,' ').


%LISTS
%toMarkup(LANG,[COMP],Vars,Atom)

toMarkup(L,[],Vars,Atom):-toMarkup(L,'NullSet',Vars,Atom).
%toMarkup(html,[Su|Bj],Vars,Chars):-toMarkupList(html,[Su|Bj],Vars,Chars1),sformat(Chars,'<div>(<ul>~w </ul>)</div>',[Chars1]).

close_varlist([]):-!.
close_varlist('$VAR'(_)):-!.
close_varlist([V|VV]):-close_varlist(VV),!.


toMarkup(html,option(Option),Vars,Chars):-sformat(Chars,'<option value="~w">~w</option>',[Option,Option]).

% Numbers
toMarkup(_,Atom,_VS,Chars):-float(Atom),!,sformat(Chars,'~f',[Atom]).
toMarkup(_,Atom,_VS,Chars):-number(Atom),!,sformat(Chars,'~w',[Atom]).

toMarkup(L,Value,Vars,Chars):-
        fail,mooCache(PredR, skolem, Value = x(Name,Expression),SKVARS,Context, Ctx, TN, Auth, State),!,
            toMarkup(kif,Name,Vars,NameQ),  prependQuestionMark(NameQ,NameQM),
            subst(x(Sk,Expression),Sk,NameQM,x(NSk,NExpression)),!,
            toMarkup(L,exists([NSk],NExpression),SKVARS,Chars).

% PRETTYNESS
toMarkup(_,';',Vars,'or ').
toMarkup(_,',',Vars,'and ').
toMarkup(_,'neg',Vars,'neg ').
%toMarkup(_,entails,Vars,'modus-ponens ').
%toMarkup(_,entails,Vars,'modus-tollens ').

%toMarkup(L,Char,Vars,Char):-L==kif.

toMarkup(html,Atom,Vars,Char):-!,
      my2_www_form_encode(Atom,E),
      sformat(Char,'<a href="browse.moo?find=~w">~w<a>',[E,Atom]).

toMarkup(bach,Atom,Vars,Chars):-atom(Atom),atom_codes(Atom,Codes),member(32,Codes),sformat(Chars,'~w',[Atom]).
%TODO toMarkup(bach,Atom,Vars,Chars):-atom(Atom),sformat(Chars,'"~w"',[Atom]).
toMarkup(bach,Atom,Vars,Chars):-atom(Atom),sformat(Chars,'~w',[Atom]).
      

toMarkup(_,Atom,Vars,Atom):-!.

my2_www_form_encode(H,E):-
	 sformat(S,'~q',[H]),
	 string_to_atom(S,A),
	 www_form_encode(A,E),!.


%TODO Number?

% ================================================
%      toMarkupList
% ================================================

toMarkupList(L,Var,VS,Chars):-isSlot(Var),!,toMarkupSlot(L,Var,VS,Chars).
toMarkupList(_,[],VS,''):-!.
toMarkupList(LANG,[H],VS,Chars):-!,
        toMarkup(LANG,H,VS,Chars).
toMarkupList(LANG,[H|T],VS,Chars):-!,
        toMarkup(LANG,H,VS,Chars1),
        toMarkupList(LANG,T,VS,Chars2),
        sformat(Chars,'~w ~w',[Chars1,Chars2]).

markUpVARLIST(L,[],Vars,''):-!.
markUpVARLIST(L,'$VAR'(_),Vars,''):-!.

markUpVARLIST(L,[VV|Varnames],Vars,Chars):-
                  VV=..[_,Name,Value],!,
                  toMarkupVarEquals(L,'$VAR'(Name),Value,Vars,Chars1),
                  markUpVARLIST(L,Varnames,Vars,Chars2),
                  sformat(Chars,'~w\n~w',[Chars1,Chars2]).

toMarkupVarEquals(_,Name,Value,Vars,Chars):-
            toMarkup(kif,Name,Vars,NameQ),
            toMarkup(L,Value,Vars,ValChars),
            sformat(Chars,'~w = ~w',[NameQ,ValChars]).

% Real Prolog Var
toMarkupSlot(L,Slot,VarList,Chars):- isVarProlog(Slot),!,
        toMarkup_makeNamePrologVar(L,VarList,Slot,Name),
        atom_concat('?',Name,Chars),!.
% Slot 'Typed'
toMarkupSlot(L,Slot,VarList,Chars):-isQualifiedAs(Slot,BaseType,Value,Subtype), !,
        toMarkup_makeName(L,VarList,Slot,Subtype,Value,Name),
        close_freeVars(VarList,NVarList),
        ahtml_ppend(NVarList,[Name=Value],NV),
        toMarkup(L,Value,NV,VChars),
        sformat(Chars,'<div title="~w">~w</div>',[Subtype,VChars]).

toMarkup_makeNamePrologVar(L,VarList,Value,Name):-member(Name=Var,VarList),Var==Value,!.
toMarkup_makeNamePrologVar(L,VarList,Value,Name):-getVarAtom(Value,NUame),atom_concat('',NUame,Name).

getVarAtom(Value,Name):-var(Value),!,term_to_atom(Value,Vname),atom_codes(Vname,[95,_|CODES]),atom_codes(Name,CODES),!.
getVarAtom('$VAR'(VNUM),Name):-sformat(Name,'~w',['$VAR'(VNUM)]),!.
getVarAtom('$VAR'(VNUM),Name):-concat_atom([VNUM],Name),!.



toMarkup_makeName(L,VarList,Slot,BaseType,Value,Name):-
        member(Name=Var,VarList),Var==Slot,!.
toMarkup_makeName(L,VarList,Slot,BaseType,Value,Name):-
        member(Name=Var,VarList),Var==Value,!.
toMarkup_makeName(L,VarList,Slot,BaseType,Value,Name):-atom_concat('?',BaseType,Name).



close_freeVars(V,V):-proper_list(V),!.
close_freeVars(V,[]):-isSlot(V),!. %Closing List if there are no free getPrologVars
close_freeVars([X|XX],[X|More]):- close_freeVars(XX,More).





toMarkup_varProlog(kif,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],number(Name),Var==VarRef,!,sformat(NameQ,'?~d',[Name]).
toMarkup_varProlog(kif,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],Var==VarRef,!,sformat(NameQ,'?~w',[Name]).

toMarkup_varProlog(html,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],number(Name),Var==VarRef,!,sformat(NameQ,'?~d',[Name]).
toMarkup_varProlog(html,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],Var==VarRef,!,sformat(NameQ,'?~w',[Name]).

toMarkup_varProlog(T,Var,[_|Rest],Name):-nonvar(Rest),toMarkup_varProlog(T,Var,Rest,Name).
toMarkup_varProlog(kif,VAR,_,VarName):-term_to_atom(VAR,AVAR),atom_codes(AVAR,[95|CODES]),!,catch(sformat(VarName,'?HYP-~s',[CODES]),_,VarName='?HYP-AVAR').
toMarkup_varProlog(kif,VAR,_,VarName):-term_to_atom(VAR,AVAR),atom_codes(AVAR,CODES),!,catch(sformat(VarName,'?HYP-~s',[CODES]),_,VarName='?HYP-AVAR').
toMarkup_varProlog(html,VAR,VS,VarName):-toMarkup_varProlog(kif,VAR,VS,VarName).

prependQuestionMark(Name,NameQ):-atom_concat('?',Name,NameQ).


% ===================================================
% Special Objects
% ===================================================
%toMarkupFormula(L,C,Vars,Chars):-       writeq( C=Vars),nl,fail.


toMarkupFormula(L,C,Vars,Chars):-isSlot(C),!,toMarkupSlot(L,C,Vars,Chars).
toMarkupFormula(L,C,Vars,Chars):-not(compound(C)),!,toMarkup(L,C,Vars,Chars).


% QUOTED STRING FORMAT
toMarkupFormula(L,string([]),Vars,'""'):-!.
toMarkupFormula(L,string(''),Vars,'""'):-!.
toMarkupFormula(L,string(Atom),Vars,Chars):-!,sformat(Chars,'"~w"',[Atom]).
toMarkupFormula(L,'$stringCodes'(Atom),Vars,Chars):-!,sformat(Chars,'"~w"',[Atom]).
toMarkupFormula(L,'$stringSplit'(List),Vars,Chars):-
      toMarkupList(L,List,Vars,Chars1),sformat(Chars,'"~w"',[Chars1]).
toMarkupFormula(L,Atom,_VS,Chars):-((isCharCodelist(Atom);string(Atom);is_string(Atom))),!,
%        catch(sformat(Chars,'"~s"',[Atom]),_,sformat(Chars,'"~w"',[Atom])).
        catch(sformat(Chars,'~s',[Atom]),_,sformat(Chars,'~w',[Atom])).

isCharCodelist([]).  isCharCodelist([A|T]):-integer(A),A>9,A<128,isCharCodelist(T).


toMarkupFormula(bach,float(Atom),_VS,Chars):-float(Atom) -> sformat(Chars,'~f',[Atom]);sformat(Chars,'~w',[Atom]).
toMarkupFormula(bach,int(Atom),_VS,Chars):-sformat(Chars,'~w',[Atom]).



toMarkupFormula(kif,'.'(Su,Bj),Vars,Chars):- 
      Bj \==[],
      atom(Su),not(is_list(Bj)),!,
      toMarkupFormula(kif,Su,Vars,Chars1),
      toMarkupFormula(kif,Bj,Vars,Chars2),!,
      sformat(Chars,'(~w . ~w)',[Chars1,Chars2]).


% SPECIAL FORMATS


toMarkupFormula(Form,saved(Data,Mt,Vars,Lits),_VS,Chars):-
      clause(saved(Data,Mt,Vars,Lits),true,ID),!,
      toMarkupFormula(Form,Data,Vars,Chars1),
      sformat(Chars,'<a href="editform.moo?id=~w" title="Edit EL ~w"><img border=0 src="http://www.cyc.com/cycdoc/img/cb/white.gif"></a>~w in ~w',[ID,ID,Chars1,Mt]),!.

toMarkupFormula(L,saved(Data,Mt,Vars,Lits),_,O):-!,toMarkupFormula(L,Data,_,O).

toMarkupFormula(L,':-'(C,true),Vars,Chars):-toMarkup(L,C,Vars,Chars).
toMarkupFormula(L,(T^V),Vars,Chars):-var_merge(Vars,V,TVars),!,toMarkup(L,T,TVars,Chars).

% No parens (nv = no vector)
toMarkupFormula(L,nv(Subj),Vars,Chars):-is_list(Subj),!,toMarkupList(L,Subj,Vars,Chars).
toMarkupFormula(L,nv(Subj),Vars,Chars):-!,toMarkup(L,Subj,Vars,Chars).

toMarkupFormula(_,writeFmt(F,A),Vars,Chars):-sformat(Chars,F,A),!.

toMarkupFormula(kif,[Su|Bj],Vars,Chars):-toMarkupList(kif,[Su|Bj],Vars,Chars1),sformat(Chars,'(~w)',[Chars1]).

toMarkupFormula(_,writeq(Term),Vars,Atom):-!,sformat(Atom,'~q',[Term]).
toMarkupFormula(kif,maillink(Title,Address,Subject),Vars,Address):-!.
toMarkupFormula(kif,weblink(Title,URL),Vars,Title):-!.
toMarkupFormula(kif,helplink(Title,URL),Vars,Title):-!.
toMarkupFormula(L,explaination(PB),Vars,Atom):-
        flag(explaination_linenumber,_,1),
        toMarkupFormula(L,PB,Vars,AtomS),!,
        sformat(Atom,'\nExplaination:\n~w\n',[AtomS]).

toMarkupFormula(LANG,krlog(COMP),Vars,Atom):-!,prolog_to_krlog(COMP,KR),toMarkup(LANG,KR,Vars,Atom).

toMarkupFormula(LANG,kif(COMP),Vars,Atom):-!,toMarkup(kif,COMP,Vars,Atom).
toMarkupFormula(LANG,html(COMP),Vars,Atom):-!,toMarkup(html,COMP,Vars,Atom).

toMarkupFormula(html,select(Name,OptionList),Vars,Chars):-toMarkup(html,options(OptionList),Vars,Options),sformat(Chars,'<select sort name="~w" id="~w" size="1">~w</select>',[Name,Name,Options]).
toMarkupFormula(html,chectheoryox(Name,on),Vars,Chars):-
                sformat(Chars,'<input type=chectheoryox name="~w" id="~w" checked>',[Name,Name]),!.
toMarkupFormula(html,chectheoryox(Name,_),Vars,Chars):-
                sformat(Chars,'<input type=chectheoryox name="~w" id="~w">',[Name,Name]),!.
toMarkupFormula(html,options([]),Vars,'').

toMarkupFormula(L,getPrologVars(Form),Vars,Chars):-markUpVARLIST(L,Form,Vars,SChars),sformat(Chars,'~w',[SChars]),!.

toMarkupFormula(L,getPrologVars(Form),Vars,Chars):-!,sformat(Chars,'; var_post_err (~q). ',[Form]).


toMarkupFormula(html,qresult(Res),Vars,Chars):-!,sformat(Chars,'Result ',[Res]).

toMarkupFormula(kif,qresult(Res),Vars,''):-!. %,sformat(Chars,'res="~w"\n',[Res]).

% Back into Standard Terms

format_o(Format,Stuff):-
        toMarkup(html,Stuff,_,Chars),writeFmt(Format,[Chars]).


toMarkupFormula(html,options([Option|List]),Vars,Chars):-
               toMarkup(html,option(Option),Vars,Chars2),
               toMarkup(html,options(List),Vars,Out3),
               atom_concat(Chars2,Out3,Chars).

toMarkupFormula(Form,entails(A,C),Vars,Chars):-!,
      toMarkupFormula(Agent,'#$codeEntailment'(C,A),Vars,Chars1),
      sformat(Chars,'<img src="http://www.cyc.com/cycdoc/img/cb/purple.gif">~w in CODE',[Chars1]),!.


toMarkupFormula(L,hidden(F,Args),Vars,''):-!.

toMarkupFormula(L,'$nartFixed'(C),Vars,O):-!,toMarkupFormula(L,(C),Vars,O),!.

toMarkupFormula(html,colourize(Color,Thing),Vars,Chars):-!,
        toMarkupFormula(html,Thing,Vars,Chars1),!,
        sformat(Chars,'<font color="~w">~w</font>\n',[Color,Chars1]).

toMarkupFormula(L,colourize(Color,Thing),Vars,Chars):-!,
        toMarkupFormula(L,Thing,Vars,Chars),!.

/*
toMarkupFormula(L,','(A,B),Vars,Chars):-!,
        prolog_to_krlog(','(A,B),KR),
        toMarkupFormula(L,KR,Vars,Chars),!.
*/


toMarkupFormula(L,write_dollar('$v',[A|Args]),Vars,Chars):-!,
                Flag=..[getPrologVars,A|Args],!,
                toMarkupFormula(L,Flag,Vars,Chars).

toMarkupFormula(L,table_(Goal,Lits),Vars,Chars):-!,
                toMarkupFormula(L,table_p(Lits,Goal),Vars,Chars).


toMarkupFormula(L,write_dollar(F,[A|Args]),Vars,Chars):-!,
        toMarkupFormula(L,A,Vars,Chars1),
        toMarkupFormula(L,hidden(F,Args),Vars,Chars2),!,
        sformat(Chars,'~w~w',[Chars1,Chars2]).

toMarkupFormula(L,'$existential'(VarName,Name,Literal),Vars,O):-!,
        toMarkupFormula(L,'existential'(VarName),Vars,O).

toMarkupFormula(L,'$eval'(Function),Vars,O):-!,
        toMarkupFormula(L,' eval'(Function),Vars,O).


toMarkupFormula(L,functional(VarName,Domains,Literal),Vars,O):-
        toMarkup(L,Literal,Vars,O),!.

close_list_var(M,[]):-isSlot(M),!.
close_list_var([[M]|Ms],[M|Ls]):-!,
        close_list_var(Ms,Ls).
close_list_var([M|Ms],[M|Ls]):-!,
        close_list_var(Ms,Ls).


      

toMarkupFormula(L,unused(C,P),Vars,O):-!,
        toMarkupFormula(L,notused(C,writeq(P)),Vars,O).

toMarkupFormula(L,ff([]),Vars,'[]'):-!.

toMarkupFormula(L,ff([Flag|Flags]),Vars,Chars):-!,
        toMarkupFormula(L,flag(Flag),Vars,Chars1),
        toMarkupFormula(L,ff(Flags),Vars,Chars2),
        sformat(Chars,'~w, ~w',[Chars1, Chars2]).

toMarkupFormula(L,domargs([]),Vars,''):-!.

toMarkupFormula(L,domargs([(P:N)]),Vars,Chars):-!,
        toMarkupFormula(L,P,Vars,Chars1),
        sformat(Chars,'~w:~w',[Chars1,N]).

toMarkupFormula(L,domargs([(P:N)|Flags]),Vars,Chars):-!,
        toMarkupFormula(L,P,Vars,Chars1),
        toMarkupFormula(L,domargs(Flags),Vars,Chars2),
        sformat(Chars,'~s:~w,~w',[Chars1,N,Chars2]).

toMarkupFormula(L,flag(Flag),Vars,Chars):-
        Flag=..[domainV,Var,DomArgs],!,
        toMarkupFormula(L,Var,Vars,VarChars),
        toMarkupFormula(L,domargs(DomArgs),Vars,ArgChars),
        sformat(Chars,'~w(~w,[~w])',[domainV,VarChars,ArgChars]).

toMarkupFormula(L,flag(Flag),Vars,Chars):-
        Flag=..[Name,Var,Args],!,
        toMarkupFormula(L,Var,Vars,VarChars),
        sformat(Chars,'~w(~w, ~q)',[Name,VarChars,Args]).
toMarkupFormula(L,flag(Flag),Vars,Chars):-!,
        toMarkupFormula(L,writeq(Flag),Vars,Chars).


toMarkupFormula(L,writeq(Atom),_VS,Chars):-!,sformat(Chars,'~q',[Atom]).

toMarkupFormula(L,[],Vars,''):-!.

toMarkupFormula(bach,':'(Bj,list([])),Vars,Chars):-!,sformat(Chars,'~w:[]',[Bj]).
toMarkupFormula(bach,':'(Bj,map([])),Vars,Chars):-!,sformat(Chars,'~w:{}',[Bj]).
toMarkupFormula(bach,':'(Bj,More),Vars,Chars):-!,
	 toMarkupFormula(bach,More,Vars,Chars2),
        sformat(Chars,'~w:~w',[Bj,Chars2]).

toMarkupFormula(bach,[Bj],Vars,Chars):-!,toMarkupFormula(bach,Bj,Vars,Chars).
toMarkupFormula(bach,[Bj|More],Vars,Chars):-!,
	 toMarkupFormula(bach,Bj,Vars,Chars1),
	 toMarkupFormula(bach,More,Vars,Chars2),
        sformat(Chars,'~w,~w',[Chars1,Chars2]).

%toMarkupFormula(L,[A | B],Vars,Chars):-proper_list([A | B]),ahtml_ppend(['('|[A | B],[')'],TRY),toMarkupList(L,[Su|Bj],Vars,Chars).
%toMarkupFormula(L,[A | B],Vars,Chars):-catch(TRY=..['',A | B],_,fail),toMarkupFormula(L,TRY,Varsr,Chars),!.
%toMarkupFormula(L,[A | B],Vars,Chars):-catch(TRY=..[A | B],_,fail),toMarkupFormula(L,TRY,Vars,Chars),!.
%toMarkupFormula(L,[A | B],Vars,Chars):-catch(TRY=..[A | B],_,fail),toMarkupFormula(L,TRY,Vars,Chars),!.
toMarkupFormula(L,[Su|Bj],Vars,Chars):-
        toMarkupList(L,[Su|Bj],Vars,Chars1),
        sformat(Chars,'(~w)',[Chars1]).



/*
toMarkupFormula(L,Term,Vars,O):-
        Term=..[holds,F|Args],isNonVar(F),not_a_function(F),!,
        NTerm=..[F|Args],
        toMarkupFormula(L,NTerm,Vars,O).
*/
toMarkupFormula(L,'$VAR'(_)* X ,Vars,Chars):-!,toMarkupFormula(L, X ,Vars,Chars).
toMarkupFormula(L, X * '$VAR'(_) ,Vars,Chars):-!,toMarkupFormula(L, X ,Vars,Chars).
toMarkupFormula(L,(A * []),Vars,Chars):-!,toMarkupFormula(L,A ,Vars,Chars).
toMarkupFormula(L,([] * A),Vars,Chars):-!,toMarkupFormula(L,A ,Vars,Chars).
toMarkupFormula(L,deduced* X ,Vars,Chars):-!,toMarkupFormula(L, X ,Vars,Chars).
toMarkupFormula(L, X * deduced ,Vars,Chars):-!,toMarkupFormula(L, X ,Vars,Chars).


toMarkupFormula(L,domainV(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' domainV'(Var,writeq(ReqsL)),Vars,Chars).
toMarkupFormula(L,domainC(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' domainC'(Var,writeq(ReqsL)),Vars,Chars).
toMarkupFormula(L,domainA(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' domainA'(Var,writeq(ReqsL)),Vars,Chars).
toMarkupFormula(L,existsC(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' existsC'(Var,writeq(ReqsL)),Vars,Chars).
toMarkupFormula(L,existsA(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' existsA'(Var,writeq(ReqsL)),Vars,Chars).

toMarkupFormula(L,(A * B),Vars,Chars):-!,
        toMarkupFormula(L,B,Vars,Chars2),
        toMarkupFormula(L,A,Vars,Chars1),
        sformat(Chars,'~w\n~w',[Chars2, Chars1]).

toMarkupFormula(L,formula(C),Vars,Chars):-!,
        toMarkupFormula(L,C,Vars,Chars).


toMarkupFormula(html,undefined_constants(UnDefinedList),_,O):-
        toMarkupFormula(kif,nv(UnDefinedList),_,I),
        sformat(O,'\n<font color=red>Warning Undefined constants: <font color=black size=+1>~w</font></font>',[I]).

toMarkupFormula(kif,undefined_constants(UnDefinedList),_,O):-
        toMarkupFormula(kif,(UnDefinedList),_,I),
        sformat(O,'\nWarning Undefined constants ~w',[I]).



toMarkupFormula(L,C,Vars,Chars):-is_list(C),!,make_args_out(L,C,Vars,Chars1),sformat(Chars,'(~w)',[Chars1]).

% ==================================================
% Unest And/Or
% ==================================================

toMarkupFormula(L,and(and(and(and(and(F,E),D),C),B),A),VS,Chars):-!, toMarkupFormula(L,and(F,E,D,C,B,A),VS,Chars).
toMarkupFormula(L,and(and(and(and(E,D),C),B),A),VS,Chars):-!, toMarkupFormula(L,and(E,D,C,B,A),VS,Chars).
toMarkupFormula(L,and(and(and(D,C),B),A),VS,Chars):-!, toMarkupFormula(L,and(D,C,B,A),VS,Chars).
toMarkupFormula(L,and(and(B,C),A),VS,Chars):-!, toMarkupFormula(L,and(C,B,A),VS,Chars).
toMarkupFormula(L,and(A,and(B,and(C,and(D,and(E,F))))),VS,Chars):-!, toMarkupFormula(L,'and'(A,B,C,D,E,F),VS,Chars).
toMarkupFormula(L,and(A,and(B,and(C,and(D,E)))),VS,Chars):-!, toMarkupFormula(L,'and'(A,B,C,D,E),VS,Chars).
toMarkupFormula(L,and(A,and(B,and(C,D))),VS,Chars):-!, toMarkupFormula(L,'and'(A,B,C,D),VS,Chars).
toMarkupFormula(L,and(A,and(B,C)),VS,Chars):-!, toMarkupFormula(L,'and'(A,B,C),VS,Chars).
toMarkupFormula(L,or(or(or(or(D,E),D),B),A),VS,Chars):-!, toMarkupFormula(L,or(E,D,C,B,A),VS,Chars).
toMarkupFormula(L,or(or(or(C,D),B),A),VS,Chars):-!, toMarkupFormula(L,or(D,C,B,A),VS,Chars).
toMarkupFormula(L,or(or(B,C),A),VS,Chars):-!, toMarkupFormula(L,or(C,B,A),VS,Chars).
toMarkupFormula(L,or(A,or(B,or(C,or(D,E)))),VS,Chars):-!, toMarkupFormula(L,'or'(A,B,C,D,E),VS,Chars).
toMarkupFormula(L,or(A,or(B,or(C,D))),VS,Chars):-!, toMarkupFormula(L,'or'(A,B,C,D),VS,Chars).
toMarkupFormula(L,or(A,or(B,C)),VS,Chars):-!, toMarkupFormula(L,'or'(A,B,C),VS,Chars).

% ==================================================
% Mark terms as implemented in code
% ==================================================

toMarkupFormula(html,incode(X),Vars,HAtom):-!,
        toMarkupFormula(L,bullet(X),Vars,Atom),
        sformat(HAtom,'<table border=0><tr><td><pre>~w</pre></td><td><pre>Implemented in code.</pre></td></tr></table>',[Atom]).

toMarkupFormula(kif,incode(X),Vars,HAtom):-!,
        toMarkupFormula(L,bullet(X),Vars,Atom),
        sformat(HAtom,'~w\nImplemented in code.\n',[Atom]).


toMarkupFormula(html,incode(X,M),Vars,HAtom):-!,
        toMarkupFormula(L,bullet(X),Vars,Atom),
        sformat(HAtom,'<table border=0><tr><td><pre>~w</pre></td><td><pre>Implemented in code.\n~w</pre></td></tr></table>',[Atom,M]).

toMarkupFormula(kif,incode(X,M),Vars,HAtom):-!,
        toMarkupFormula(L,bullet(X),Vars,Atom),
        sformat(HAtom,'~w\nImplemented in code.\n (~w)\n',[Atom,M]).




toMarkupFormula(L,cfind(entails(Pre,Post)),Vars,Chars):-
        fail,mooCache(PredR,Post,Pre,T,true,Context,Explaination),
        toMarkupFormula(L,Explaination,Vars,Chars),!.

% ==================================================
% Show explaination of cross reference optimization
% ==================================================
toMarkupFormula(L,g_h(_),Vars,''):-!.
toMarkupFormula(L,tid(_),Vars,''):-!.

toMarkupFormula(L,crossref(X,Y),Vars,Atom):-!,
        crossref_to_explaination(crossref(X,Y),P),
        toMarkupFormula(L,P,Vars,Atom).

toMarkupFormula(L,crossref(X),Vars,Atom):-!,
        crossref_to_explaination(crossref(X),P),
        toMarkupFormula(L,P,Vars,Atom).


% ==========================
% Slolem  rewriting
% ==========================

toMarkupFormula(L,(X),Vars,Chars):- nonvar(X),X=..['E',Sk|ArgS],!,
        Y=..[Sk|ArgS],!,
        toMarkupFormula(L,Y,Vars,Chars).

% =====================
% remove_nonvars
% =====================

remove_nonvars(V,V):-isSlot(V),!.
remove_nonvars([],[]):-!.
remove_nonvars([V|L],LL):-isNonVar(V),!,remove_nonvars(L,LL).
remove_nonvars([V|L],[V|LL]):-remove_nonvars(L,LL).



% =====================
% Forall
% =====================

toMarkupFormula(L,forall(V,F),Vars,Chars):-not(is_list(V)),!,
        group_forall(forall(V,F),Next),!,
        cleanQuantifierConversionForWrite_forall(Next,O),
        toMarkupFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_forall(forall(VL,F),O):-
        remove_nonvars(VL,NL),
        ((NL=[],!,O=F);(!,O=forall(NL,F))).

toMarkupFormula(L,forall(V,F),Vars,Chars):- not(is_list(V)),!,
        toMarkupFormula(L,forall([V],F),Vars,Chars).

group_forall(forall(V1,forall(V2,forall(V3,forall(V4,forall(V5,F))))),forall([V1,V2,V3,V4,V5],F)):-!.
group_forall(forall(V1,forall(V2,forall(V3,forall(V4,F)))),forall([V1,V2,V3,V4],F)):-!.
group_forall(forall(V1,forall(V2,forall(V3,F))),forall([V1,V2,V3],F)):-!.
group_forall(forall(V1,forall(V2,F)),forall([V1,V2],F)):-!.
group_forall(forall(V1,F),forall([V1],F)):-!.

% =====================
% Exists
% =====================



toMarkupFormula(L,exists(V,F),Vars,Chars):-not(is_list(V)),!,
        group_exists(exists(V,F),Next),!,
        cleanQuantifierConversionForWrite_exists(Next,O),
        toMarkupFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_exists(exists(VL,F),O):-
        remove_nonvars(VL,NL),
        ((NL=[],!,O=F);(!,O=exists(NL,F))).

toMarkupFormula(L,exists(V,F),Vars,Chars):- not(is_list(V)),!,
        toMarkupFormula(L,exists([V],F),Vars,Chars).

group_exists(exists(V1,exists(V2,exists(V3,exists(V4,exists(V5,F))))),exists([V1,V2,V3,V4,V5],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,exists(V4,F)))),exists([V1,V2,V3,V4],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,F))),exists([V1,V2,V3],F)):-!.
group_exists(exists(V1,exists(V2,F)),exists([V1,V2],F)):-!.
group_exists(exists(V1,F),exists([V1],F)):-!.
% =====================
% Exists
% =====================

toMarkupFormula(L,exists(V,F),Vars,Chars):-not(is_list(V)),!,
        group_exists(exists(V,F),Next),!,
        cleanQuantifierConversionForWrite_exists(Next,O),
        toMarkupFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_exists(exists(VL,F),O):-
        remove_nonvars(VL,NL),
        ((NL=[],!,O=F);(!,O=exists(NL,F))).

toMarkupFormula(L,exists(V,F),Vars,Chars):- not(is_list(V)),!,
        toMarkupFormula(L,exists([V],F),Vars,Chars).

group_exists(exists(V1,exists(V2,exists(V3,exists(V4,exists(V5,F))))),exists([V1,V2,V3,V4,V5],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,exists(V4,F)))),exists([V1,V2,V3,V4],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,F))),exists([V1,V2,V3],F)):-!.
group_exists(exists(V1,exists(V2,F)),exists([V1,V2],F)):-!.
group_exists(exists(V1,F),exists([V1],F)):-!.

% ==================================================
% Finds the clausification then displays the explaination
% ==================================================
toMarkupFormula(L,map(ARGS),Vars,Chars):-
                flag(indent,X,X+1),
                indent_nbsp(X,PreOut),!,
                make_args_out(L,ARGS,Vars,ArgsOut),!,
                sformat(Chars,'~w{~w}',[PreOut,ArgsOut]), !,
                flag(indent,NX,NX-1).

toMarkupFormula(L,list(ARGS),Vars,Chars):-
                flag(indent,X,X+1),
                indent_nbsp(X,PreOut),!,
                make_args_out(L,ARGS,Vars,ArgsOut),!,
                sformat(Chars,'~w[~w]',[PreOut,ArgsOut]), !,
                flag(indent,NX,NX-1).



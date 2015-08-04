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



:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = mpred_online,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
% [Required] Load the Logicmoo Library Utils
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).


% :- module(logicmoo_i_www,[ html_print_term/2  ]).  % +Term, +Options

:- user:ensure_loaded(logicmoo_i_www_listing).
% :- if(if_defined(load_mud_www)).



:- dynamic   http:location/3.
:- multifile http:location/3.

:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [append/3, member/2, select/3]).
:- use_module(library(operators), [push_op/3]).
:- use_module(library(shlib), [current_foreign_library/2]).
:- use_module(library(prolog_source)).
:- user:use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(unix)).
:- user:use_module(library(rdf_ntriples),[rdf_ntriple_part/4]).
:- user:use_module(library(tty),[menu/3]).
:- user:use_module(library(solution_sequences),[distinct/1]).

:- if(exists_source(library(pldoc))).
:- use_module(library(pldoc), []).
	% Must be loaded before doc_process
:- use_module(library(pldoc/doc_process)).
:- endif.
:- use_module(library(prolog_xref)).


:- include(logicmoo(mpred/logicmoo_i_header)).
% :- ['../logicmoo_run_swish'].
:- user:ensure_loaded('../logicmoo_run_clio').

% WANT 
:- initialization(doc_collect(true)).

%:- use_module(library(pldoc/doc_library)).
%:- doc_load_library.

:- use_module(library(option)).
%:- style_check(-discontiguous). 
%:- style_check(-singleton).


:- thread_local(thlocal:omit_full_stop).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_error)).

:- include(library(pldoc/hooks)).
% http_reply_from_files is here
:- use_module(library(http/http_files)).
% http_404 is in here
:- use_module(library(http/http_dispatch)).
:- use_module(library(pldoc/doc_process)).
:- reexport(library(pldoc/doc_html)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_search)).
:- use_module(library(pldoc/doc_util)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(library(http/html_head)).
:- use_module(library(readutil)).
:- use_module(library(url)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(doc_http)).

:- use_module(library(http/http_session)).

:-  M=pldoc_process,ignore((module_property(M,file(S)),source_file(PI,S),
   \+ ((predicate_property(M:PI,imported_from(U)),U\==M)),
   functor(PI,F,A),import(F/A),fail)).


:- portray_text(false).  % Enable portray of strings
:- use_module(library(http/http_parameters)).

% :- use_module(library(http/http_session)).
%:- thread_property(_,alias('http@3020'))->true; http_server(http_dispatch, [port(3020)]).

:- http_handler('/logicmoo/', handler_logicmoo_cyclone, [chunked,prefix]). %  % 
:- http_handler('/logicmoo_nc/', handler_logicmoo_cyclone, [prefix]). %  % 

:- meta_predicate
	handler_logicmoo_cyclone(+).


print_request([]).
print_request([H|T]) :-
        H =.. [Name, Value],
        format(user_error,'<tr><td>~w<td>~w~n', [Name, Value]),
        print_request(T).


f_to_mfa(EF,R,F,A):-get_fa(EF,F,A),
    (((current_predicate(_:F/A),functor(P,F,A),predicate_property(_M:P,imported_from(R)))*->true;
    current_predicate(_:F/A),functor(P,F,A),source_file(R:P,_SF))),
    current_predicate(R:F/A).

:- nb_setval(pldoc_options,[ prefer(manual) ]).

escape_quote(SUnq,SObj):-atom_subst(SUnq,'"','\\\"',SObj).
escape_quote(Unq,SObj):-url_iri(SObj,Unq).
escape_double_quotes(String,SObj):-format(string(SUnq),'~q',[String]),escape_quote(SUnq,SObj),!.

% 
% <link rel="SHORTCUT ICON" href="http://prologmoo.com/cycdoc/img/cb/mini-logo.gif"><meta name="ROBOTS" content="NOINDEX, NOFOLLOW">

handler_logicmoo_cyclone( Request):-   
   catch(mmake,_,true), 
   call(handler_logicmoo_cyclone_1,Request).

http_save_in_session(NV):- \+ compound(NV),!.
http_save_in_session(NV):-is_list(NV),!,maplist(http_save_in_session,NV).
http_save_in_session(NV):-NV=..[_,V],is_list(V),http_save_in_session(V),fail.
http_save_in_session(NV):-NV=..[N,V],!,http_save_in_session(N=V).
http_save_in_session(Unsaved=_):- member(Unsaved,[path_info,protocol,peer]),!.
http_save_in_session(N=V):- NV=..[N,V],!, maybe_http_set_session(NV).
http_save_in_session(NV):-  maybe_http_set_session(NV).

maybe_http_set_session(NV):-functor(NV,N,1),functor(NVR,N,1),http_in_session(S),
   retractall(session_data(S,NVR)),
   catch(http_session:http_set_session(NV),_,asserta(session_data(S,NV))).


reset_assertion_display:-
   flag(matched_assertions,_,0),
   flag(show_asserions_offered,_,0),
   retractall(shown_subtype(_)),
   retractall(shown_clause(_)).

get_nv(N,V):- must(param_default_value(N,D)),get_nv(N,V,D).


get_nv(N,V,D):- nonvar(V),!,get_nv(N,VV,D),!,param_matches(V,VV).


get_nv(L,V,D):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V,[optional(true),default(Foo)]],
  httpd_wrapper:http_current_request(B),
   http_parameters:http_parameters(B,[CALL2])->
       V \== Foo,!.
get_nv(L,V,D):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V], http_session:session_data(F, CALL2),!.
get_nv(L,V,V):- (is_list(L)-> member(N,L) ; N=L), http_save_in_session(N=V),!.

save_request_in_session(Request):- 
      http_open_session(F,[renew(true)]),
        (member(method(post), Request) -> (http_read_data(Request, Data, []),http_save_in_session(Data));true),
        http_save_in_session(Request),
        http_save_in_session(Parameters),
        http_save_in_session(Data),!,
        http_session:http_session_id(F),forall(http_session:session_data(F,D),wdmsg(D)).



handler_logicmoo_cyclone_1(Request):- 
 must_det_l((
   save_request_in_session(Request),   
   format('Content-type: text/html~n~n',[])->
   format('<html><head><base target="_parent" />',[])->
   (member(request_uri(URI),Request)->format('<meta http-equiv="refresh" content="300;~w">',[URI]);true),
   get_nv(call,Call,show_pcall),!,
   get_sobj(_String,Obj,_PredURL,_SObj),
   format('<title>~q for ~q</title></head>',[Call,Obj]),
   format('<body class="yui-skin-sam">',[]),flush_output,
   logOnError(Call),!,
   flush_output,format('</body></html>~n~n',[]),flush_output)),!.


show_pcall_footer:-
      format('<hr><span class="copyright"><i>Copyright &copy; 1999 - 2015 <a href="http://prologmoo.com">
      LogicMOO/PrologMUD</a>.All rights reserved.</i></span></body></html>~n~n',[]),
      !.

cvt_param_to_term(In,Obj):-atom(In),catch(atom_to_term(In,Obj,_),_,fail),nonvar(Obj),!.
cvt_param_to_term(In,Obj):-string(In),catch(atom_to_term(In,Obj,_),_,fail),nonvar(Obj),!.
cvt_param_to_term(Obj,Obj).

get_sobj(String,Obj,PredURL,SObj):-
         get_nv(search,String,"ttPredType"),
         cvt_param_to_term(String,Obj),
         url_iri(PredURL,String),
         escape_double_quotes(Obj,SObj),!.

:- discontiguous param_default_value/2. 

param_default_value(human_language,'EnglishLanguage').
human_language('AlbanianLanguage').
human_language('ArabicLanguage').
human_language('BasqueLanguage').
human_language('CatalanLanguage').
human_language('ChineseLanguage').
human_language('DanishLanguage').
human_language('EnglishLanguage'). 
human_language('FarsiLanguage').
human_language('FinnishLanguage').
human_language('FrenchLanguage').
human_language('GalicianLanguage').
human_language('GermanLanguage').
human_language('HebrewLanguage').
human_language('IndonesianLanguage').
human_language('ItalianLanguage').
human_language('JapaneseLanguage').
human_language('MalayLanguage').
human_language('NorwegianBokmalLanguage').
human_language('NorwegianNorskLanguage').
human_language('PolishLanguage').
human_language('PortugueseLanguage').
human_language('SpanishLanguage').
human_language('ThaiLanguage').
human_language('de').

param_default_value(logic_lang_name,'CLIF').
logic_lang_name('CLIF',"Common Logic (CLIF)").
logic_lang_name('CycL',"CycL").
logic_lang_name('Prolog',"Prolog").
logic_lang_name('CGIF',"CG-Logic (CGIF)").
logic_lang_name('SUO-KIF',"SUO-KIF").
logic_lang_name('TPTP',"TPTP (fof/cnf)").
logic_lang_name('OWL',"OWL").

param_default_value(prover_name,'proverPTTP').
prover_name("proverCyc","CycL (LogicMOO)").
prover_name("proverPFC","PFC").
prover_name("proverPTTP","PTTP (LogicMOO)").
prover_name("proverDOLCE","DOLCE (LogicMOO)").

param_matches(A,B):-A=B,!.
param_matches(VV,V):-atomic(VV),atomic(V),string_to_atom(VV,VVA),string_to_atom(V,VA),downcase_atom(VVA,VD),downcase_atom(VA,VD).
param_matches(A,B):-A=B,!.

show_select2(Name,Pred,Options):-
  
    Call=..[Pred,ID,Value],
    must(param_default_value(Name,D);param_default_value(Pred,D)),!,
    get_nv(Name,UValue,D),
    format('<select name="~w">',[Name]),
    forall(Call,
       (((member(atom_subst(Item,ItemName),Options) -> atom_subst(Value,Item,ItemName,NValue); NValue=Value),
        (((param_matches(UValue,ID);param_matches(UValue,NValue)) -> format('<option value="~w" selected="yes">~w</option>',[ID,NValue]);
                   format('<option value="~w">~w</option>',[ID,Value])))))),
    format('</select>',[]),!.


show_select1(Name,Pred):-
 Call=..[Pred,Value],
 (param_default_value(Name,D);param_default_value(Pred,D)),!,
 format('<select name="~w">',[Name]),
 forall(Call,
    (get_nv(Name,Value,D)->format('<option value="~w" selected="yes">~w</option>',[Value,Value]);
                format('<option value="~w">~w</option>',[Value,Value]))),
 format('</select>',[]),!.

show_pcall:- 
    reset_assertion_display,
   get_sobj(String,Obj,PredURL,SObj),
format('<table width="1112" cellspacing="0" cellpadding="0" height="121" id="table4">
 <!-- MSTableType="nolayout" -->
	<form action="Browse.jsp">
      <!-- MSTableType="nolayout" -->
		<tr>
          <td align="left" valign="top" width="36" rowspan="2"><img src="http://54.183.42.206:8080/sigma/pixmaps/sigmaSymbol-gray.gif"></td>
          <td></td>
          <td align="left" valign="top" width="717" rowspan="2">
          <img src="http://54.183.42.206:8080/sigma/pixmaps/logoText-gray.gif">&nbsp;&nbsp;Prover:&nbsp;',
          []),
  show_select2(prover,prover_name,[]),
 format('<table cellspacing="0" cellpadding="0" id="table5" width="658" height="97">
      <!-- MSTableType="nolayout" -->
	<tr>
          <td align="right"><b>KB Call/Find:</b></td>
          <td align="left" valign="top" colspan="2">
              <textarea rows="4" cols="50" name="search">~s</textarea>
          </td>
          <td align="left" valign="top" height="68">',
          [SObj]),action_menu_applied('action_above',"Item",""),
format('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp<br></td></tr>
        <tr><td><img src="http://54.183.42.206:8080/sigma/pixmaps/1pixel.gif" height="3"></td>
      		<td></td>
			<td></td>
			<td height="3"></td>
            </tr>
            <tr>
                  <td align="right" width="99"><b>English&nbsp;Word:&nbsp;</b></td>
                  <td align="left" valign="top" width="276">
                      <input type="text" size="27" name="word" value="~s">
                      <img src="http://54.183.42.206:8080/sigma/pixmaps/1pixel.gif" width="3"><input type="submit" value="Overlap" name="xref"></td>
                  <td align="left" valign="top" width="144">
                      <select name="POS"><option value="Noun">Noun<option value="Verb">Verb<option value="Adjective">Adjective <option value="Adverb">Adverb</select>
                      <input type="submit" value="NatLg" name="ShowEnglish"></td>
                  <td align="left" valign="top" height="26" width="139">',
                  [SObj]),
                   show_select1('humanLang',human_language),
                  format('</td>
             </tr>
            </table>
          </td>
          <td valign="bottom" width="4" rowspan="2"></td>
          <td height="96">
          <span class="navlinks">
          <b>[&nbsp;<a href="KBs.jsp">Home</a>&nbsp;|&nbsp;              
          <a href="Graph.jsp?kb=SUMO&lang=EnglishLanguage&search=~w">Grap2h</a>]</b></span><p>
          <b>Formal&nbsp;Language&nbsp;<br></b>',
                        [SObj]),
                  show_select2(flang,logic_lang_name,[]),

                  format('</td>
      </tr>
		<tr>
			<td width="4"></td>
			<td valign="top" height="25" width="351">
                        <input type="checkbox" name="sExprs" value="ON" checked>S-Exprs&nbsp;
                        <input type="checkbox" name="webDebug" value="ON" checked>Debugging
                        </td>
		</tr>
  </form></table><hr><iframe width="100%" height="80%" frameborder="0" scrolling="yes" marginheight="0" marginwidth="0"
   allowtransparency=true id="main-iframe" name="main-iframe" style="width:100%;height:100%"
  src="?call=show_pcall_right&search=~w"></iframe>',
[SObj,SObj,String]),flush_output,!.


action_menu_applied(MenuName,ItemName,Where):-
  show_select2(MenuName,action_menu_item,[atom_subst('$item',ItemName)]),
      format('&nbsp;~w&nbsp;&nbsp;<input type="submit" value="Now" name="Apply">',[Where]).

param_default_value(action_menu_item,'Find').
action_menu_item('Find',"Find $item").
action_menu_item('Forward',"Forward Direction").
action_menu_item('Backward',"Backward Direction").
action_menu_item('query',"Query $item").
action_menu_item('repropogate',"Repropogate $item (ReAssert)").
action_menu_item('remove',"Remove $item(Unassert)").   
action_menu_item('Code',"Assume Theorem (Disable $item)").
action_menu_item('prologSingleValued',"Make $item Single Valued").
action_menu_item('prologBuiltin',"Impl $item in Prolog").
action_menu_item('prologPTTP',"Impl $item in PTTP").
action_menu_item('prologDRA',"Impl $item in DRA").
action_menu_item('prologPfc',"Impl $item in PFC").
action_menu_item('Monotonic',"Treat $item Monotonic").
action_menu_item('NonMonotonic',"Treat $item NonMonotonic").   

show_pcall_right:- 
   must_det_l((get_sobj(String,Obj,PredURL,SObj),
        format('<form action="Apply.jsp">Apply ',[]),

        action_menu_applied('action_below',"Checked or Clicked","&nbsp;below&nbsp;"),
        format('<hr/><pre>',[]),
        with_assertions(thlocal:print_mode(html),catch(make_page_pretext_obj(Obj),times_up(_),true)),
        
        format('</pre></form>',[]),
        show_pcall_footer)).



% make_page_pretext_obj(Obj):- atom(Obj),atom_to_term(Obj,Term,Bindings),nonvar(Term),Term\=@=Obj,!,must(make_page_pretext_obj(Term)).
make_page_pretext_obj(Obj):- 
  % catch(mmake,_,true),
  % forall(no_repeats(M:F/A,(f_to_mfa(Pred/A,M,F,A))),ignore(logOnFailure((this_listing(M:F/A),flush_output)))),
  % forall(no_repeats(M:F/A,(f_to_mfa(Pred/A,M,F,A))),ignore(logOnFailure((reply_object_sub_page(M:F/A),flush_output)))),
  statistics(walltime,[Wall,_]),!,
  ignore(catch(term_listing_inner(i2tml_hbr(Obj,Wall),Obj),times_up(Obj),true)),!,
  pp_i2tml_saved_done(Obj),!.

  %ignore((fail,catch(pfc_listing(Pred),_,true))),!.



:- prolog_xref:assert_default_options(register_called(all)).

reply_object_sub_page(Obj) :- phrase(object_sub_page(Obj, []), HTML), print_html(HTML),!.

object_sub_page(Obj, Options) -->
	{ doc_process:doc_comment(Obj, File:_Line, _Summary, _Comment)
	}, !,
	(   { \+ ( doc_process:doc_comment(Obj, File2:_, _, _),
		   File2 \== File )
	    }
	->  html([ \object_synopsis(Obj, []),
		   \objects([Obj], Options)
		 ])
	;   html([
		   \objects([Obj], [synopsis(true)|Options])
		 ])
	).


html_html_pp_ball(Color,Alt):-format(S,'<img src="http://prologmoo.com/cycdoc/img/cb/~w.gif" alt="~w" align="top" border="0"></img>',[Color,Alt]).


%write_html(HTML):- phrase(html(HTML), Tokens), html_write:print_html(Out, Tokens))).






return_to_pos(Call):- current_line_position(LP),Call,!, must(set_line_pos(LP)).
nl_same_pos:-return_to_pos(nl).



set_line_pos(LP):-current_output(Out),set_line_pos(Out,LP).
set_line_pos(Out,LP):- 
  current_line_position(Out,CLP), 
  (CLP==LP->! ;((CLP>LP->nl(Out);put_code(Out,32)),!,set_line_pos(Out,LP))).

current_line_position(LP):-current_output(Out),current_line_position(Out,LP).
current_line_position(Out,LP):-stream_property(Out,position( Y)),stream_position_data(line_position,Y,LP),!.

tmw:- with_assertions(thlocal:print_mode(html),(print((a(LP):-b([1,2,3,4]))),nl,nl,wid(_,_,KIF),KIF=(_=>_),nl,nl,print(KIF),listing(print_request/1))),!.









:- predicate_options(print_term/2, 2,
		     [ output(stream),
		       right_margin(integer),
		       left_margin(integer),
		       tab_width(integer),
		       indent_arguments(integer),
		       operators(boolean),
		       write_options(list)
		     ]).


:- predicate_options(print_term/2, 2,
		     [ output(stream),
		       right_margin(integer),
		       left_margin(integer),
		       tab_width(integer),
		       indent_arguments(integer),
		       operators(boolean),
		       write_options(list)
		     ]).

%%	print_term(+Term, +Options) is det.
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










% ===================================================
% Pretty Print Formula
% ===================================================
:-export(write_atom_link/1).
write_atom_link(A):-must(write_atom_link(A,A)).
:-export(write_atom_link/2).
write_atom_link(L,N):-must_det_l((write_atom_link(atom(W),L,N),format('~w',[W]))),!.

% pred_href(Name/Arity, Module, HREF) :-
write_atom_link(W,A/_,N):-atom(A),!,write_atom_link(W,A,N).
write_atom_link(W,C,N):-compound(C),get_functor(C,F,A),!,write_atom_link(W,F/A,N).
write_atom_link(atom(N),_,N):- thread_self(main),!.
write_atom_link(W,_,N):- must(nonvar(W)),\+ thlocal:print_mode(html),format(W,'~q',[N]),!.
write_atom_link(W,A,N):- nonvar(W),url_iri(URL,A),format(W,'<a href="KB.jsp?search=~q" target="_top">~w</a>',[URL,N]).



/*


%   File   : WRITE.PL
%   Author : Richard A. O'Keefe.
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

:- public
	portable_display/1,
	portable_listing/0,
	portable_listing/1,
	portable_print/1,
	portable_write/1,
	portable_writeq/1,
	rok_portray_clause/1.

:- meta_predicate
	classify_name(+, -),
	classify_alpha_tail(+),
	classify_other_tail(+),
	'functor spec'(+, -, -, -),
	'list clauses'(+, +, +, +),
	'list magic'(+, +),
	'list magic'(+, +, +),
	'list magic'(+, +, +, +),
	maybe_paren(+, +, +, +, -),
	maybe_space(+, +),
	rok_portray_clause(+),
	put_string(+),
	put_string(+, +),
	write_args(+, +, +, +, +),
	write_atom(+, +, +, -),
	write_oper(+, +, +, +, -),
	write_out(+, +, +, +, -),
	write_out(+, +, +, +, +, +, -),
	write_tail(+, +),
	write_VAR(+, +, +, -),
	write_variable(?).
*/
     

/*  WARNING!
    This file was written to assist portability and to help people
    get a decent set of output routines off the ground fast.  It is
    not particularly efficient.  Information about atom names and
    properties should be precomputed and fetched as directly as
    possible, and strings should not be created as lists!

    The four output routines differ in the following respects:
    [a] display doesn't use operator information or handle {X} or
	[H|T] specially.  The others do.
    [b] print calls portray/1 to give the user a chance to do
	something different.  The others don't.
    [c] writeq puts quotes around atoms that can't be read back.
	The others don't.
    Since they have such a lot in common, we just pass around a
    single Style argument to say what to do.

    In a Prolog which supports strings;
	write(<string>) should just write the text of the string, this so
	that write("Beware bandersnatch") can be used.  The other output
	commands should quote the string.

    listing(Preds) is supposed to write the predicates out so that they
    can be read back in exactly as they are now, provided the operator
    declarations haven't changed.  So it has to use writeq.  $VAR(X)
    will write the atom X without quotes, this so that you can write
    out a clause in a readable way by binding each input variable to
    its name.
*/


portable_display(Term) :-
	write_out(Term, display, 1200, punct, _).


portable_print(Term) :-
	write_out(Term, print, 1200, punct, _).


portable_write(Term) :-
	write_out(Term, write, 1200, punct, _).


portable_writeq(Term) :-
	write_out(Term, writeq, 1200, punct, _).



%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.

maybe_paren(P, Prio, Char, _, punct) :-
	P > Prio,
	!,
	put(Char).
maybe_paren(_, _, _, C, C).



%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
	put(32).
maybe_space(quote, alpha) :- !,
	put(32).
maybe_space(_, _).



%   put_string(S)
%   writes a list of character codes.


put_string(B):-put_string0(B).
put_string0([]).
put_string0([H|T]) :-
	put(H),
	put_string0(T).


%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.

put_string(A,B):- thlocal:print_mode(html),!,
  with_output_to(atom(S),put_string0(A,B)),url_iri(URL,S),format('<a href="KB.jsp?search=~q" target="_top">~w</a>',[URL,S]).
put_string(A,B):- put_string0(A,B).

put_string0([], Q) :-
	put(Q).
put_string0([Q|T], Q) :- !,
	put(Q), put(Q),
	put_string0(T, Q).
put_string0([H|T], Q) :-
	put(H),
	put_string0(T, Q).



%   write_variable(V)
%   is system dependent.  This just uses whatever Prolog supplies.

write_variable(V) :-
	write(V).



%   write_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style (display,write,writeq,print)
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

write_out(Term, _, _, Ci, alpha) :-
	var(Term),
	!,
	maybe_space(Ci, alpha),
	write_variable(Term).
write_out('$VAR'(N), Style, _, Ci, Co) :- !,
	write_VAR(N, Style, Ci, Co).
write_out(N, _, _, Ci, alpha) :-
	integer(N),
	(   N < 0, maybe_space(Ci, other)
	;   maybe_space(Ci, alpha)
	),  !,
	name(N, String),
	put_string(String).
write_out(Term, print, _, Ci, alpha) :-
	% DMILES HSOULD BE portray(Term),
        print(Term),
	!.
write_out(Atom, Style, Prio, _, punct) :-
	atom(Atom),
	current_op(P, _, Atom),
	P > Prio,
	!,
	put(40),
	(   Style = writeq, write_atom(Atom, Style, punct, _)
	;   name(Atom, String), put_string(String)
	),  !,
	put(41).
write_out(Atom, Style, _, Ci, Co) :-
	atom(Atom),
	!,
	write_atom(Atom, Style, Ci, Co).
write_out(Term, display, _, Ci, punct) :- !,
	functor(Term, Fsymbol, Arity),
	write_atom(Fsymbol, display, Ci, _),
	write_args(0, Arity, Term, 40, display).
write_out({Term}, Style, _, _, punct) :- !,
	put(123),
	write_out(Term, Style, 1200, punct, _),
	put(125).
write_out([Head|Tail], Style, _, _, punct) :- !,
	put(91),
	write_out(Head, Style, 999, punct, _),
	write_tail(Tail, Style).
write_out((A,B), Style, Prio, Ci, Co) :- !,
	%  This clause stops writeq quoting commas.
	maybe_paren(1000, Prio, 40, Ci, C1),
	write_out(A, Style, 999, C1, _),
	put(44),
	write_out(B, Style, 1000, punct, C2),
	maybe_paren(1000, Prio, 41, C2, Co).
write_out(Term, Style, Prio, Ci, Co) :-
	functor(Term, F, N),
	write_out(N, F, Term, Style, Prio, Ci, Co).


write_out(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, fx, F), P is O-1
	;   current_op(O, fy, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	write_atom(F, Style, C1, C2),
	arg(1, Term, A),
	write_out(A, Style, P, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_out(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xf, F), P is O-1
	;   current_op(O, yf, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_out(A, Style, P, C1, C2),
	write_atom(F, Style, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_out(2, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xfy, F), P is O-1, Q = O
	;   current_op(O, xfx, F), P is O-1, Q = P
	;   current_op(O, yfx, F), Q is O-1, P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_out(A, Style, P, C1, C2),
	write_oper(F, O, Style, C2, C3),
	arg(2, Term, B),
	write_out(B, Style, Q, C3, C4),
	maybe_paren(O, Prio, 41, C4, Co).
write_out(N, F, Term, Style, Prio, Ci, punct) :-
	write_atom(F, Style, Ci, _),
	write_args(0, N, Term, 40, Style).


write_oper(Op, Prio, Style, Ci, Co) :-
	Prio < 700, !,
	write_atom(Op, Style, Ci, Co).
write_oper(Op, _, Style, Ci, punct) :-
	put(32),
	write_atom(Op, Style, punct, _),
	put(32).


write_VAR(N, Style, Ci, alpha) :-
	integer(N), N >= 0, !,
	maybe_space(Ci, alpha),
	Letter is N mod 26 + 65,
	put(Letter),
	(   N < 26
	;   Rest is N/26, name(Rest, String),
	    put_string(String)
	), !.
write_VAR(A, Style, Ci, Co) :-
	atom(A), !,
	write_atom(A, write, Ci, Co).
write_VAR(X, Style, Ci, punct) :-
	write_atom('$VAR', Style, Ci, _),
	write_args(0, 1, '$VAR'(X), 40, Style).



write_atom(('!'), _, _, punct) :- !,
	put(33).
write_atom((';'), _, _, punct) :- !,
	put(59).
write_atom([], _, _, punct) :- !,
	put(91), put(93).
write_atom({}, _, _, punct) :- !,
	put(123), put(125).
write_atom(A,B,C,D):- write_atom_link(A,A),!.
write_atom(Atom, Style, Ci, Co) :-
	name(Atom, String),
	(   classify_name(String, Co),
	    maybe_space(Ci, Co),
	    put_string(String)
	;   Style = writeq, Co = quote,
	    maybe_space(Ci, Co),
	    put(39), put_string(String, 39)
	;   Co = alpha,
	    put_string(String)
	),  !.

%   classify_name(String, Co)
%   says whether a String is an alphabetic identifier starting
%   with a lower case letter (Co=alpha) or a string of symbol characters
%   like ++/=? (Co=other).  If it is neither of these, it fails.  That
%   means that the name needs quoting.  The special atoms ! ; [] {} are
%   handled directly in write_atom.  In a basic Prolog system with no
%   way of changing the character classes this information can be
%   calculated when an atom is created, andf just looked up.  This has to
%   be as fast as you can make it.

classify_name([H|T], alpha) :-
	H >= 97, H =< 122,
	!,
	classify_alpha_tail(T).
classify_name([H|T], other) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).

classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
	(  H >= 97, H =< 122
	;  H >= 65, H =< 90
	;  H >= 48, H =< 57
	;  H =:= 95
	), !,
	classify_alpha_tail(T).

classify_other_tail([]).
classify_other_tail([H|T]) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).



%   write_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .

write_args(N, N, _, _, _) :- !,
	put(41).
write_args(I, N, Term, C, Style) :-
	put(C),
	J is I+1,
	arg(J, Term, A),
	write_out(A, Style, 999, punct, _),
	write_args(J, N, Term, 44, Style).



%   write_tail(Tail, Style)
%   writes the tail of a list of a given style.

write_tail(Var, _) :-			%  |var]
	var(Var),
	!,
	put(124),
	write_variable(Var),
	put(93).
write_tail([], _) :- !,			%  ]
	put(93).
write_tail([Head|Tail], Style) :- !,	%  ,Head tail
	put(44),
	write_out(Head, Style, 999, punct, _),
        
	write_tail(Tail, Style).
write_tail(Other, Style) :-		%  |junk]
	put(124),
	write_out(Other, Style, 999, punct, _),
	put(93).


/*  The listing/0 and listing/1 commands are based on the Dec-10
    commands, but the format they generate is based on the "pp" command.
    The idea of rok_portray_clause/1 came from PDP-11 Prolog.

    BUG: the arguments of goals are not separated by comma-space but by
    just comma.  This should be fixed, but I haven't the time right not.
    Run the output through COMMA.EM if you really care.

    An irritating fact is that we can't guess reliably which clauses
    were grammar rules, so we can't print them out in grammar rule form.

    We need a proper pretty-printer that takes the line width into
    acount, but it really isn't all that feasible in Dec-10 Prolog.
    Perhaps we could use some ideas from NIL?
*/

portable_listing :-
	current_predicate(_, Pred),
        predicate_property(Pred,number_of_clauses(_)),
	nl,
	clause(Pred, Body),
	rok_portray_clause((Pred:-Body)),
	fail.
portable_listing.


%   listing(PredSpecs)

%   Takes a predicate specifier F/N, a partial specifier F, or a
%   list of such things, and lists each current_predicate Pred
%   matching one of these specifications.

portable_listing(V) :-
	var(V), !.       % ignore variables
portable_listing([]) :- !.
portable_listing([X|Rest]) :- !,
	portable_listing(X),
	portable_listing(Rest).
portable_listing(X) :-
	'functor spec'(X, Name, Low, High),
	current_predicate(Name, Pred),
	functor(Pred, _, N),
	N >= Low, N =< High,
	nl, 
	clause(Pred, Body),
	rok_portray_clause((Pred:-Body)),
	fail.
portable_listing(_).


'functor spec'(Name/Low-High, Name, Low, High) :- !.
'functor spec'(Name/Arity, Name, Arity, Arity) :- !.
'functor spec'(Name, Name, 0, 255).


rok_portray_clause(:-(Command)) :-
	(   Command = public(Body), Key = (public)
	;   Command = mode(Body),   Key = (mode)
	;   Command = type(Body),   Key = (type)
	;   Command = pred(Body),   Key = (pred)
	;   Command = Body,	    Key = ''
	),  !,
	nl,
	% numbervars(Body, 0, _),
	\+ \+ 'list clauses'(Body, Key, 2, 8).
rok_portray_clause((Pred:-Body)) :-
	% numbervars(Pred+Body, 0, _),
	\+ \+ portable_writeq(Pred),
	\+ \+ 'list clauses'(Body, 0, 2, 8), !.
rok_portray_clause((Pred)) :-
	rok_portray_clause((Pred:-true)).


'list clauses'((A,B), L, R, D) :- !,
	'list clauses'(A, L, 1, D), !,
	'list clauses'(B, 1, R, D).
'list clauses'(true, L, 2, D) :- !,
	put(0'.), nl.
'list clauses'((A;B), L, R, D) :- !,
	'list magic'(fail, L, D),
	'list magic'((A;B), 0, 2, D),
	'list magic'(R, '.
').
'list clauses'((A->B), L, R, D) :- !,
	'list clauses'(A, L, 5, D), !,
	'list clauses'(B, 5, R, D).
'list clauses'(Goal, L, R, D) :-
	'list magic'(Goal, L, D),
	portable_writeq(Goal),
	'list magic'(R, '.
').

'list magic'(!,    0, D) :- !,
	write(' :- ').
'list magic'(!,    1, D) :- !,
	write(',  ').
'list magic'(Goal, 0, D) :- !,
	write(' :- '),
	nl, tab(D).
'list magic'(Goal, 1, D) :- !,
	put(0',),
	nl, tab(D).
'list magic'(Goal, 3, D) :- !,
	write('(   ').
'list magic'(Goal, 4, D) :- !,
	write(';   ').
'list magic'(Goal, 5, D) :- !,
	write(' ->'),
	nl, tab(D).
'list magic'(Goal, Key, D) :-
	atom(Key),
	write(':- '), write(Key),
	nl, tab(D).


'list magic'(2, C) :- !, write(C).
'list magic'(_, _).


'list magic'((A;B), L, R, D) :- !,
	'list magic'(A, L, 1, D), !,
	'list magic'(B, 1, R, D).
'list magic'(Conj,  L, R, D) :-
	E is D+8,
	M is L+3,
	'list clauses'(Conj, M, 1, E),
	nl, tab(D),
	'list magic'(R, ')').


/*	Test code for rok_portray_clause.
	If it works, test_portray_clause(File) should write out the
	contents of File in a more or less readable fashion.

test_portray_clause(File) :-
	see(File),
	repeat,
	    read(Clause, Vars),
	    (   Clause = end_of_file
	    ;   test_bind(Vars), rok_portray_clause(Clause), fail
	    ),
	!,
	seen.

test_bind([]) :- !.
test_bind([X='$VAR'(X)|L]) :-
	test_bind(L).
:-public test_portray_clause/1.
*/





%:- endif. 


:- dynamic portray/1.
:- multifile portray/1.

% '$messages':user:my_portray(X):-fail,loop_check(user:my_portray(X)).
% portray(X):-loop_check(user:my_portray(X)).

:- discontiguous my_portray/1. 
:-export(user:my_portray/1).
user:my_portray(A) :- var(A),!,fail,writeq(A).
user:my_portray(A) :-
        atom(A),
        sub_atom(A, 0, _, _, 'http://'), !,
        (   style(B)
        ->  true
        ;   B=prefix:id
        ),
        portray_url(B, A).
user:my_portray(A) :-
        atom(A),
        atom_concat('__file://', B, A),
        sub_atom(B, D, _, C, #),
        sub_atom(B, _, C, 0, G),
        sub_atom(B, 0, D, _, E),
        file_base_name(E, F),
        format('__~w#~w', [F, G]).
user:my_portray(A) :- atom(A),!,user:write_atom_link(A,A).
user:my_portray(A) :- \+compound(A),fail.
%user:my_portray(P):- must((return_to_pos(rok_portray_clause(P)),!)).





:- rok_portray_clause((
pkif :-

        [ implies,

          [ isa(F, tPred),
            isa(A, ftInt),
            poss(KB, pos([arity(F, A)])),
            poss(KB, arity(F, A))
          ],
          =>,

          [ all([F]),

            [ implies,
              [isa(F, tPred), ex([A]), isa(A, ftInt), poss(KB, arity(F, A))],
              =>,
              [ex([A]), [isa(A, ftInt), arity(F, A)]]
            ]
          ]
        ])),nl,nl,nl.



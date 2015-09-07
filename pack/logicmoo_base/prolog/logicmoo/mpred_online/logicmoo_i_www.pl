/** <module> logicmoo_i_www
% Provides /logicmoo runtime preds browsing
%
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- use_module(library(http/http_server_files)).


:- volatile(user:session_data/2).
:- volatile(system:'$loading_file'/3).
:- volatile(http_log:log_stream/2).
:- volatile(http_session:urandom_handle/1).

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

http:location(pixmaps, root(pixmaps), []).
user:file_search_path(pixmaps, logicmoo('mpred_online/pixmaps')).
:- http_handler(pixmaps(.), serve_files_in_directory(pixmaps), [prefix]).

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

make_quotable_0(SUnq,SObj):-atom_subst(SUnq,'\\','\\\\',SObj0),atom_subst(SObj0,'\n','\\n',SObj1),atom_subst(SObj1,'"','\\\"',SObj).
make_quotable(String,SObj):-string(String),format(string(SUnq),'~s',[String]),make_quotable_0(SUnq,SObj),!.
make_quotable(String,SObj):-atomic(String),format(string(SUnq),'~w',[String]),make_quotable_0(SUnq,SObj),!.
make_quotable(String,SObj):-format(string(SUnq),'~q',[String]),make_quotable_0(SUnq,SObj),!.

% 
% <link rel="SHORTCUT ICON" href="/pixmaps/mini-logo.gif"><meta name="ROBOTS" content="NOINDEX, NOFOLLOW">

handler_logicmoo_cyclone( Request):-   
   catch(mmake,_,true), 
   notrace(call(handler_logicmoo_cyclone_1,Request)).

http_save_in_session(NV):- \+ compound(NV),!.
http_save_in_session(NV):-is_list(NV),!,maplist(http_save_in_session,NV).
http_save_in_session(NV):-NV=..[_,V],is_list(V),http_save_in_session(V),fail.
http_save_in_session(NV):-NV=..[N,V],!,http_save_in_session(N=V).
http_save_in_session(Unsaved=_):- member(Unsaved,[path_info,protocol,peer]),!.
http_save_in_session(N=V):- NV=..[N,V],!, maybe_http_set_session(NV).
http_save_in_session(NV):-  maybe_http_set_session(NV).

maybe_http_set_session(NV):-functor(NV,N,1),functor(NVR,N,1),get_http_in_session(S),
   retractall(session_data(S,NVR)),
   catch(http_session:http_set_session(NV),_,asserta(session_data(S,NV))).

get_http_in_session(S):-show_call(http_in_session(S)),!.
get_http_in_session(S):-get_http_current_request(R),member(session(S),R).

reset_assertion_display:-
   flag(matched_assertions,_,0),
   flag(show_asserions_offered,_,0),
   retractall(shown_subtype(_)),
   retractall(shown_clause(_)).

get_nv(N,V):- get_nv(N,V,DDD), (V==DDD->must(param_default_value(N,V));true).

get_nv(N,V,D):- nonvar(V),!,get_nv(N,VV,D),!,param_matches(V,VV).
get_nv(N,V,D):-get_param(N,V)*->true;V=D.

:-dynamic(http_last_request/1).
get_http_current_request(B):- httpd_wrapper:http_current_request(B), !,ignore((member(peer(ip(73,37,100,94)),B),retractall(http_last_request(_)),asserta(http_last_request(B)))).
get_http_current_request(B):-http_last_request(B),!.


get_param(N,V):-nonvar(N),!,get_param0(N,V),!.
get_param(N,V):-current_form_var(N)*->get_param0(N,V)->true.

replaced_value(call,edit,edit1term):-!.
replaced_value(call,edit_term,edit1term):-!.
replaced_value(_,X,X).

get_param0(N,O):-get_param1(N,V),replaced_value(N,V,VO),!,VO=O.

get_param1(N,V):-get_param_from_req(N,V).
get_param1(N,V):-atom(N),atom_concat('request_',UN,N),!,C=..[UN,V],get_http_current_request(B),member(C,B).
get_param1('_path_file',FILE):-!, get_http_current_request(B), member(path(PATH),B),directory_file_path(_,FILE,PATH),!.
get_param1(N,V):-get_param_from_filename(N,V),!.
%get_param1(N,V):-atom(N),C=..[N,V],get_http_current_request(B),member(C,B),!.
get_param1(N,V):-atom(N),atom_concat('_',UN,N),C=..[UN,V],get_http_current_request(B),member(C,B),!.
get_param1(call,V):-get_param0('_path_file',FILE),sub_atom(FILE,N,_,_,'_'),sub_atom(FILE,0,N,_,V),!.


get_param_from_req(L,V):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V,[optional(true),default(Foo)]],
  get_http_current_request(B),
  http_parameters:http_parameters(B,[CALL2])->
       V \== Foo,!.

get_param_from_filename(PN,VO):- url_encode(PN,N),
  get_param('_path_file',FILE),
  sformat(Sub,'_n_~w_v0_',[N]),!,
  sub_atom(FILE,B,L,_,Sub),
  Start is B + L,
  sub_atom(FILE,Start,_,0,AfterAtom),
  sub_atom(AfterAtom,NB,_,_,'_vZ_n_'),
  sub_atom(AfterAtom,0,NB,_,V),!,
  url_decode(V,VO).

% get_nv(L,V,V):- (is_list(L)-> member(N,L) ; N=L), http_save_in_session(N=V),!.

get_nv_session(L,V,_):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V], (http_open_session(F,[renew(false)]),http_session:session_data(F, CALL2)),!.
get_nv_session(_,V,V):-!.


save_request_in_session(Request):- 
      http_open_session(F,[renew(false)]),
        (member(method(post), Request) -> (http_read_data(Request, Data, []),http_save_in_session(Data));true),
        http_save_in_session(Request),
        http_session:http_session_id(F),forall(http_session:session_data(F,D),wdmsg(D)).



handler_logicmoo_cyclone_1(Request):- 
 must_det_l((
   save_request_in_session(Request),
   format('Content-type: text/html~n~n',[]),!,
   member(path(PATH),Request),directory_file_path(_,FCALL,PATH),
   (current_predicate(FCALL/0)->get_nv(call,Call,FCALL);get_nv(call,Call,edit1term)),
   (current_predicate(_,Call) -> must(Call) ; must(call_404(Call))))),!.

call_404(Call):-
   write_begin_html('edit1term',Base),
   format('<pre>'),
   writeq(call_404(Call:Base)),
   format('</pre>'),
   write_end_html.

write_begin_html(B,BASE):-  
      sformat(BASE,'~w~@',[B,get_request_vars('_n_~w_v0_~w_vZ',[search,session_data,call,term])]),
      format('<html><head>',[]),            
      %get_http_current_request(Request), (member(request_uri(URI),Request)->format('<meta http-equiv="refresh" content="300;~w">',[URI]);true),
      ((BASE\='') ->format('<base href="~w" />',[BASE]);true),
      ignore(URI=''),
      ignore(BASE=''),
     format('<title>~w for ~w</title></head>',[BASE,URI]),
     format('<body class="yui-skin-sam">',[]),flush_output,!.
   

write_end_html:- flush_output,format('</body></html>~n~n',[]),flush_output,!.



show_pcall_footer:-
      format('<hr><span class="copyright"><i>Copyright &copy; 1999 - 2015 <a href="http://prologmoo.com">
      LogicMOO/PrologMUD</a>.All rights reserved.</i></span></body></html>~n
      ',
      []),!.

cvt_param_to_term(In,Obj):-atom(In),catch(atom_to_term(In,Obj,_),_,fail),nonvar(Obj),!.
cvt_param_to_term(In,Obj):-string(In),catch(atom_to_term(In,Obj,_),_,fail),nonvar(Obj),!.
cvt_param_to_term(Obj,Obj).


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
param_default_value(search,'tHumanHead').
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

param_default_value(partOfSpeech,'N').
partOfSpeech("N","Noun").
partOfSpeech("V","Verb").
partOfSpeech("J","Adjective").
partOfSpeech("Z","Adverb").

param_matches(A,B):-A=B,!.
param_matches(VV,V):-atomic(VV),atomic(V),string_to_atom(VV,VVA),string_to_atom(V,VA),downcase_atom(VVA,VD),downcase_atom(VA,VD).
param_matches(A,B):-A=B,!.

show_select2(Name,Pred,Options):-  
    Call=..[Pred,ID,Value],
    once(must(param_default_value(Name,D);param_default_value(Pred,D))),
    get_nv(Name,UValue,D),
    format('<select name="~w">',[Name]),
    forall(Call,
       (((member(atom_subst(Item,ItemName),Options) -> atom_subst(Value,Item,ItemName,NValue); NValue=Value),
        (((param_matches(UValue,ID);param_matches(UValue,NValue)) -> format('<option value="~w" selected="yes">~w</option>',[ID,NValue]);
                   format('<option value="~w">~w</option>',[ID,Value])))))),
    format('</select>',[]),!.


show_select1(Name,Pred):-
 Call=..[Pred,Value],
 must(once(param_default_value(Name,D);param_default_value(Pred,D))),
 format('<select name="~w">',[Name]),
 forall(Call,
    (get_nv(Name,Value,D)->format('<option value="~w" selected="yes">~w</option>',[Value,Value]);
                format('<option value="~w">~w</option>',[Value,Value]))),
 format('</select>',[]),!.


 

edit1term:-
 must_det_l((
   reset_assertion_display,
   get_nv(term,String,""),
   ignore(get_nv(search,Word,String)),
   % ignore(get_nv_session(search,Word,String)),
   term_to_pretty_string(Word,SWord),
   show_call(show_edit1term(String,SWord)))).

show_edit1term(String,_SWord):-read_term_from_atom(String,T,[]),compound(T),T=(H:-_),!,show_edit1term_0(String,H).
show_edit1term(String,SWord):-show_edit1term_0(String,SWord).

show_edit1term_0(String,SWord):-atom(SWord),read_term_from_atom(SWord,T,[]),nonvar(T),!,show_edit1term_1(String,T).
show_edit1term_0(String,SWord):-show_edit1term_1(String,SWord).

show_edit1term_1(String,(P=>Q)):-show_edit1term_1(String,(P;Q;(P=>Q))).
show_edit1term_1(String,SWord):-
  write_begin_html('edit1term',Base),
    get_http_current_request(Request),
    member(request_uri(URL),Request),
format('
<table width="1111" cellspacing="0" cellpadding="0" height="121" id="table4">
 <!-- MSTableType="nolayout" -->
	<form action="edit1term">
      <!-- MSTableType="nolayout" -->
		<tr>
          <td align="left" valign="top" width="36" rowspan="2"><img src="/pixmaps/sigmaSymbol-gray.gif"></td>
          <td></td>
          <td align="left" valign="top" width="711" rowspan="2">
          <img src="/pixmaps/logoText-gray.gif">&nbsp;&nbsp;Prover:&nbsp; ~@
                   <table cellspacing="0" cellpadding="0" id="table5" width="658" height="97">
      <!-- MSTableType="nolayout" -->
	<tr>
          <td align="right"><b>Fml:</b></td>
          <td align="left" valign="top" colspan="2">
              <textarea style="white-space: pre; overflow: auto; font-size: 7pt; font-weight: bold; font-family: Verdana, Arial, Helvetica, sans-serif;border: 1px solid black;"
               wrap="off" rows="10" cols="70" name="term">~w</textarea>
          </td>
          <td align="left" valign="top" height="68">~@<input type="submit" value="TELL" name="TELL"><input type="submit" value="ASK" name="ASK">
             <br><b>Microthory</b><br>~@
             <br><b>Formal Language</b><br>~@</td>
      </tr>
        <tr><td><img src="/pixmaps/1pixel.gif" height="3"></td>
      		<td></td>
			<td></td>
			<td height="3"></td>
            </tr>
            <tr>
                  <td align="right" width="99"><b>Search:&nbsp;</b></td>
                  <td align="left" valign="top" width="276"><input type="text" size="27" name="search" value="~w">&nbsp;<input type="submit" value="Overlap" name="xref">&nbsp;</td>
                  <td align="left" valign="top" width="144">~@&nbsp;<input type="submit" value="NatLg" name="ShowEnglish"></td>
                  <td align="left" valign="top" height="26" width="139">~@</td>
             </tr>
            </table>
          </td>
          <td valign="bottom" width="9" rowspan="2"></td>
          <td height="121" rowspan="2" width="163">
          <span class="navlinks">
          <b>[&nbsp;<a href="/">Home</a>&nbsp;|&nbsp;              
          <a href="~w&Graph=true">Grap2h</a>]</b></span><p>
          <b>Response&nbsp;Language&nbsp;<br></b>~@<p>
                        <input type="checkbox" name="sExprs" value="ON" checked>S-Exprs&nbsp;
                        <input type="checkbox" name="webDebug" value="ON" checked>Debugging
                        </td>
          <td height="121" rowspan="2" width="188">
          Display Start<br>
&nbsp;<input type="text" size="13" name="displayStart" value="~w"><br>
			Display Max <a href="~w&Next=true">Next</a><br>
&nbsp;<input type="text" size="13" name="displayMax" value="~w"><br>
			~@</td>
                        
      </tr>
		<tr>
			<td width="4">&nbsp;</td>
		</tr>
  </form></table><hr>
  <iframe width="100%" height="80%" frameborder="0" scrolling="yes" marginheight="0" marginwidth="0" 
   allowtransparency=true id="main" name="main" style="width:100%;height:100%" src="search4term?call=search4term&term=~w"></iframe>'
  ,[show_select2(prover,prover_name,[]),
    String,
    action_menu_applied('action_above',"Item",""),
    show_select2('context',is_context,[]),
    show_select2(flang,logic_lang_name,[]),
    SWord,
    show_select2('POS',partOfSpeech,[]),
    show_select1('humanLang',human_language),
    URL,
    show_select2(olang,logic_lang_name,[]),
    0,URL,100000,
    show_search_filters('<br>'),
    URL,Base,SWord]),!,
   write_end_html,
   dmsg(search=SWord).


show_search_filters(BR):- 
   forall(search_filter_name_comment(N,C,_),session_checkbox(N,C,BR)).

parameter_names(List,N):-is_list(List),!,member(E,List),parameter_names(E,N).
parameter_names(V,_):- var(V),!,fail.
parameter_names(N=_,N):-!,atom(N).
parameter_names(C,N):-compound(C),functor(C,N,1).

current_form_var(N):-no_repeats((current_form_var0(N))),atom(N),\+ arg(_,v(peer,idle,ip,session),N).
current_form_var0(N):- param_default_value(N,_).
current_form_var0(N):- get_http_current_request(B),member(search(Parameters),B),parameter_names(Parameters,N).
current_form_var0(N):- http_current_session(_, Parameters),parameter_names(Parameters,N).

 
param_default_value(N,D):-search_filter_name_comment(N,_,D).
search_filter_name_comment(hideMeta,'Hide Meta/BookKeeping','OFF').
search_filter_name_comment(showSystem,'Hide System','OFF').
search_filter_name_comment(hideTriggers,'Hide Triggers','OFF').
search_filter_name_comment(showAll,'Show All','OFF').
 

session_checked(Name):- get_param(Name,V),V\=='OFF'.

session_checkbox(Name,Caption,BR):-
  (session_checked(Name) -> 
     format('<input type="checkbox" name="~w" value="ON" checked/>&nbsp;~w~w',[Name,Caption,BR]);
     format('<input type="checkbox" name="~w" value="ON"/>&nbsp;~w~w',[Name,Caption,BR])).

action_menu_applied(MenuName,ItemName,Where):-
  show_select2(MenuName,action_menu_item,[atom_subst('$item',ItemName)]),
      format('&nbsp;~w&nbsp;&nbsp;<input type="submit" value="Now" name="apply">',[Where]).

param_default_value(is_context,'BaseKB').
is_context(MT,MT):-no_repeats(is_context0(MT)).
is_context0(MT):-if_defined(exactlyAssertedEL_first(isa, MT, 'Microtheory',_,_)).
is_context0('BaseKB').

param_default_value(action_menu_item,'query').
action_menu_item('Find',"Find $item").
action_menu_item('Forward',"Forward Direction").
action_menu_item('Backward',"Backward Direction").
action_menu_item('query',"Query $item").
action_menu_item('repropagate',"Repropagate $item (ReAssert)").
action_menu_item('remove',"Remove $item(Unassert)").   
action_menu_item('Code',"Assume Theorem (Disable $item)").
action_menu_item('prologSingleValued',"Make $item Single Valued").
action_menu_item('prologBuiltin',"Impl $item in Prolog").
action_menu_item('prologPTTP',"Impl $item in PTTP").
action_menu_item('prologDRA',"Impl $item in DRA").
action_menu_item('prologPfc',"Impl $item in PFC").
action_menu_item('Monotonic',"Treat $item Monotonic").
action_menu_item('NonMonotonic',"Treat $item NonMonotonic").   



get_request_vars(Format,Exclude):- ignore(Exclude=[session_data]),
   findall(N=V,(current_form_var(N),\+ member(N,Exclude),get_nv(N,V)),NVs),
   forall(member(N=V,NVs),format(Format,[N,V])).
    

search4term:- 
   must_det_l((
        get_nv(term,Term,"tSet"),
        get_nv(search,SObj,Term),
        cvt_param_to_term(SObj,Obj),
        write_begin_html('search4term',Base),
        format('<form action="edit1term?call=edit1term~@" target="_parent">Apply ',[get_request_vars('&~w=~w',[search,session_data,call,hideMeta,showSystem,hideTriggers,showAll,term])]),
        action_menu_applied('action_below',"Checked or Clicked","&nbsp;below&nbsp;"),
        format('&nbsp;&nbsp;&nbsp;search = <input type="text" name="search" value="~q"> ~@  <br/>Base = ~w <hr/><pre>',[Obj,show_search_filters('&nbsp;&nbsp;'),Base]),flush_output,
        with_assertions(thlocal:print_mode(html),
           (with_search_filters(catch(make_page_pretext_obj(Obj),E,(writeq(E),nl))))),
        format('</pre></form>',[]),flush_output,
        show_pcall_footer,
        write_end_html)).


:-thread_local(thlocal:tl_hide_data/1).


with_search_filters(C):-
    session_checked(hideTriggers), \+ thlocal:tl_hide_data(triggers),!,
    with_assertions(thlocal:tl_hide_data(triggers),with_search_filters(C)).
with_search_filters(C):-
    session_checked(hideMeta), \+ thlocal:tl_hide_data(source_meta),!,
    with_assertions(thlocal:tl_hide_data(triggers),with_search_filters(C)).
with_search_filters(C):-C.

% make_page_pretext_obj(Obj):- atom(Obj),atom_to_term(Obj,Term,Bindings),nonvar(Term),Term\=@=Obj,!,must(make_page_pretext_obj(Term)).
make_page_pretext_obj(Obj):- 
  % catch(mmake,_,true),
  % forall(no_repeats(M:F/A,(f_to_mfa(Pred/A,M,F,A))),ignore(logOnFailure((this_listing(M:F/A),flush_output)))),
  % forall(no_repeats(M:F/A,(f_to_mfa(Pred/A,M,F,A))),ignore(logOnFailure((reply_object_sub_page(M:F/A),flush_output)))),
  call_with_time_limit(300,ignore(catch(term_listing_inner(i2tml_hbr,Obj),E,writeq(E)))),
  flush_output,
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




%write_html(HTML):- phrase(html(HTML), Tokens), html_write:print_html(Out, Tokens))).






return_to_pos(Call):- current_line_position(LP),Call,!, must(set_line_pos(LP)).
nl_same_pos:-return_to_pos(nl).



set_line_pos(LP):-current_output(Out),set_line_pos(Out,LP).
set_line_pos(Out,LP):- 
  current_line_position(Out,CLP), 
  (CLP==LP->! ;((CLP>LP->nl(Out);put_code(Out,32)),!,set_line_pos(Out,LP))).

current_line_position(LP):-current_output(Out),current_line_position(Out,LP).
current_line_position(Out,LP):-stream_property(Out,position( Y)),stream_position_data(line_position,Y,LP),!.

tmw:- with_assertions(thlocal:print_mode(html),
 (rok_portray_clause(a(LP)),
  rok_portray_clause((a(LP):-b([1,2,3,4]))),
  nl,nl,wid(_,_,KIF),
  KIF=(_=>_),nl,nl,print(KIF),listing(print_request/1))),!.
tmw:- with_assertions(thlocal:print_mode(html),(print((a(_LP):-b([1,2,3,4]))),nl,nl,wid(_,_,KIF),KIF=(_=>_),nl,nl,print(KIF),listing(print_request/1))),!.




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
%write_atom_link(W,_,N):- thread_self(main),!,write_term_to_atom_one(W,N),!.
write_atom_link(W,_,N):- must(nonvar(W)),\+ thlocal:print_mode(html),write_term_to_atom_one(W,N),!.
write_atom_link(W,A,N):- nonvar(W),catch((term_to_pretty_string(A,AQ),url_encode(AQ,URL),
   format(W,'<a href="?call=search4term&search=~w">~w</a>',[URL,AQ])),_,write_term_to_atom_one(W,N)).

write_term_to_atom_one(atom(A),Term):-format(atom(A),'~q',[Term]).

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
  with_output_to(atom(S),put_string0(A,B)),url_iri(URL,S),format('<a href="?term=~w">~w</a>',[URL,S]).
put_string(A,B):- put_string0(A,B).

% :-start_rtrace.
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
write_out(Term, print, _,  _, alpha) :-
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
write_out(N, F, Term, Style, _Prio, Ci, punct) :-
	write_atom(F, Style, Ci, _),
	write_args(0, N, Term, 40, Style).


write_oper(Op, Prio, Style, Ci, Co) :-
	Prio < 700, !,
	write_atom(Op, Style, Ci, Co).
write_oper(Op, _, Style, _Ci, punct) :-
	put(32),
	write_atom(Op, Style, punct, _),
	put(32).


write_VAR(N, _Style, Ci, alpha) :-
	integer(N), N >= 0, !,
	maybe_space(Ci, alpha),
	Letter is N mod 26 + 65,
	put(Letter),
	(   N < 26
	;   Rest is N/26, name(Rest, String),
	    put_string(String)
	), !.
write_VAR(A, _Style, _Ci, _Co) :-
	atom(A), !,
	write(A).
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
write_atom(A, write, _Ci, _Co):- !,write(A),!.
write_atom(A, _Style, _Ci, _Co):- write_atom_link(A,A),!.
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

rok_portray_clause(Var):-var(Var),!,writeq(Var).
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
'list clauses'(true, _L, 2, _D) :- !,
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

'list magic'(!,    0, _D) :- !,
	write(' :- ').
'list magic'(!,    1, _D) :- !,
	write(',  ').
'list magic'(_Goal, 0, D) :- !,
	write(' :- '),
	nl, tab(D).
'list magic'(_Goal, 1, D) :- !,
	put(0',),
	nl, tab(D).
'list magic'(_Goal, 3, _D) :- !,
	write('(   ').
'list magic'(_Goal, 4, _D) :- !,
	write(';   ').
'list magic'(_Goal, 5, D) :- !,
	write(' ->'),
	nl, tab(D).
'list magic'(_Goal, Key, D) :-
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



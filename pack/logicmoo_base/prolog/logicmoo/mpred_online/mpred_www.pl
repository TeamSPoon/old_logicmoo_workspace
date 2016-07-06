/* <module> mpred_www
% Provides /logicmoo runtime preds browsing
%
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :-module(mpred_www,[ensure_webserver/0,search4term/0]).
:- module(mpred_www,
          [ action_menu_applied/3,
            action_menu_item/2,
            add_form_script/0,
            register_logicmoo_browser/0,
            as_ftVars/1,
            call_for_terms/1,
            classify_alpha_tail/1,
            classify_name/2,
            classify_other_tail/1,
            current_form_var/1,
            current_line_position/1,
            current_line_position/2,
            cvt_param_to_term/2,
            cvt_param_to_term/3,
            do_guitracer/0,
            edit1term/0,
            edit1term/1,
            ensure_webserver/1,
            
            ensure_webserver/0,
            find_cl_ref/2,
            find_ref/2,
            fmtimg/2,
            'functor spec'/4,
            functor_to_color/2,
            functor_to_color/4,
            
            get_http_current_request/1,
            get_http_session/1,
            get_nv_session/3,
            get_param_req/2,
            get_param_sess/2,
            get_param_sess/3,
            get_request_vars/1,
            handler_logicmoo_cyclone/1,
            head_functor_sort/3,
            hmust/1,
            hmust_l/1,
            human_language/1,
            i2tml_hbr/3,
            if_html/2,
            indent_nbsp/1,
            indent_nbsp/2,
            indent_nl/0,
            is_cgi_stream/0,
            is_context/2,
            is_goog_bot/0,
            last_item_offered/1,
            'list clauses'/4,
            'list magic'/2,
            'list magic'/3,
            'list magic'/4,
            logic_lang_name/2,
            make_page_pretext_obj/1,
            make_quotable/2,
            make_session/1,
            maybe_paren/5,
            maybe_space/2,
            member_open/2,
            merge_key_vals/3,
            name_the_var/5,
            nl_same_pos/0,
            numberlist_at/2,
            object_sub_page/4,
            param_default_value/2,
            param_matches/2,
            parameter_names/2,
            partOfSpeech/2,
            portable_display/1,
            portable_listing/0,
            portable_listing/1,
            portable_print/1,
            portable_write/1,
            portable_writeq/1,
            pp_i2tml/1,
            pp_i2tml_now/1,
            pp_i2tml_save_seen/1,
            pp_i2tml_saved_done/1,
            pp_i2tml_v/1,
            pp_item_html/2,
            pp_item_html_if_in_range/2,
            pp_item_html_now/2,
            pp_now/0,
            print_request/1,
            prover_name/2,
            put_string/1,
            put_string/2,
            reply_object_sub_page/1,
            reset_assertion_display/0,
            return_to_pos/1,
            rok_portray_clause/1,
            save_in_session/1,
            save_in_session/2,
            save_in_session/3,
            save_request_in_session/1,
            search4term/0,
            search_filter_name_comment/3,
            section_close/1,
            section_open/1,
            sensical_nonvar/1,
            session_checkbox/3,
            session_checked/1,
            set_line_pos/1,
            set_line_pos/2,
            show_clause_ref/1,
            show_clause_ref_now/1,
            show_edit_term/3,
            show_http_session/0,
            show_iframe/1,
            show_iframe/3,
            show_pcall_footer/0,
            show_search_filters/1,
            show_search_filtersTop/1,
            term_to_pretty_string/2,
            this_listing/1,
            tmw/0,
            tovl/3,
            url_decode/2,
            url_decode_term/2,
            url_encode/2,
            url_encode_term/3,
            with_search_filters/1,
            write_VAR/4,
            write_args/5,
            write_as_url_encoded/2,
            write_atom/4,
            write_atom_link/1,
            write_atom_link/2,
            write_atom_link/3,
            write_begin_html/3,
            write_end_html/0,
            write_oper/5,
            write_out/5,
            write_out/7,
            write_tail/2,
            write_term_to_atom_one/2,
            write_variable/1,
          lmconf:shared_hide_data/1,
          mpred_www:http_last_request/1,
          mpred_www:last_item_offered/1,
          mpred_www_file/0
            /*
            http:location/3,
            http_dispatch:handler/4,
            http_log:log_stream/2,
            http_session:session_data/2,
            http_session:urandom_handle/1,
            system:'$init_goal'/3,
            user:file_search_path/2
            */
          ]).
 :- meta_predicate % cmt :-
        edit1term(0),
        handler_logicmoo_cyclone(+),
        hmust(0),
        hmust_l(0),
        if_html(?, 0),
        return_to_pos(0),
        show_edit_term(0, ?, ?),
        show_edit_term0(0, ?, ?),
        show_edit_term1(0, ?, ?),
        with_search_filters(0).
:- (multifile http:location/3, http_dispatch:handler/4, http_log:log_stream/2, http_session:session_data/2, http_session:urandom_handle/1, lmconf:shared_hide_data/1, system:'$init_goal'/3, user:file_search_path/2).
:- (module_transparent edit1term/1, hmust/1, hmust_l/1, if_html/2, return_to_pos/1, show_edit_term/3, show_edit_term0/3, show_edit_term1/3, with_search_filters/1).
:- (volatile http_log:log_stream/2, http_session:session_data/2, http_session:urandom_handle/1).
:- export((current_form_var0/1, get_http_session0/1,  is_context0/1, make_quotable_0/2, pp_i2tml_0/1, pp_i2tml_1/1, put_string0/1, put_string0/2, sanity_test_000/0, show_edit_term0/3, show_edit_term1/3, show_select1/2, show_select2/3)).
:- multifile((last_item_offered/1, http:location/3, http_dispatch:handler/4, http_session:session_data/2, http_session:urandom_handle/1,
   mpred_www:foobar/1, mpred_www:http_last_request/1, mpred_www:last_item_offered/1, system:'$init_goal'/3, user:file_search_path/2)).

%:- include(logicmoo(mpred/'mpred_header.pi')).
:- system:use_module(library(logicmoo_utils)).
:- system:use_module(library(logicmoo_swilib)).


:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- 
 user:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'))).


:- nb_setval(pldoc_options,[ prefer(manual) ]).

:- meta_predicate hmust(0).
:- meta_predicate hmust_l(0).
:- meta_predicate with_search_filters(0).
:- meta_predicate return_to_pos(0).
:- meta_predicate show_edit_term1(0,*,*).
:- meta_predicate show_edit_term0(0,*,*).
:- meta_predicate show_edit_term(0,*,*).
:- meta_predicate edit1term(0).

:- meta_predicate with_main_wwwerror_to_output(0).

with_main_wwwerror_to_output(G):-call(G).

%% ensure_webserver( ?ARG1) is det.
%
% Ensure Webserver.
%
ensure_webserver(Port) :- format(atom(A),'httpd@~w_1',[Port]),thread_property(_,alias(A)),!.
ensure_webserver(Port) :- on_x_rtrace(catch((http_server(http_dispatch,[ port(Port), workers(16) ])),E,wdmsg(E))).



%% ensure_webserver is det.
%
% Ensure Webserver.
%
ensure_webserver:- ensure_webserver(3020).

:- multifile(http_session:session_data/2).
:- multifile(system:'$loading_file'/3).
:- multifile(http_log:log_stream/2).
:- multifile(http_session:urandom_handle/1).

:- volatile(http_session:session_data/2).
:- volatile(system:'$loading_file'/3).
:- volatile(http_log:log_stream/2).
:- volatile(http_session:urandom_handle/1).




%% hmust( :GoalARG1) is det.
%
% Hmust.
%
hmust(G):-G *-> true ; throw(failed_hmust(G)).



%% hmust_l( :GoalARG1) is det.
%
% Hmust (list Version).
%
hmust_l(G):-is_list(G),!,maplist(hmust,G).
hmust_l((G1,G2)):- !,hmust_l(G1),hmust_l(G2).
hmust_l(G):-hmust(G).


:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = mpred_online,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
% [Required] Load the Logicmoo Library Utils

 






% WANT 
:- initialization(doc_collect(true)).


:- portray_text(false).  % or Enable portray of strings


:- thread_local(t_l:omit_full_stop).

%:- thread_property(_,alias('http@3020'))->true; http_server(http_dispatch, [port(3020)]).

register_logicmoo_browser:- http_handler('/logicmoo/', handler_logicmoo_cyclone, [chunked,prefix]),
  http_handler('/logicmoo_nc/', handler_logicmoo_cyclone, [prefix]).


:- initialization(register_logicmoo_browser). %  % 
:- initialization(register_logicmoo_browser,restore). %  % 
:- register_logicmoo_browser.


%% location( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Hook To [http:location/3] For Module Mpred_www.
% Location.
%
http:location(pixmaps, root(pixmaps), []).



%% user:file_search_path( ?ARG1, ?ARG2) is det.
%
% Hook To [user:file_search_path/2] For Module Mpred_www.
% File Search Path.
%
user:file_search_path(pixmaps, logicmoo('mpred_online/pixmaps')).
:- initialization(http_handler(pixmaps(.), serve_files_in_directory(pixmaps), [prefix])).

:- meta_predicate
	handler_logicmoo_cyclone(+).




%% print_request( :TermARG1) is det.
%
% Print Request.
%
print_request([]).
print_request([H|T]) :-
        H =.. [Name, Value],
        format(user_error,'<tr><td>~w<td>~w~n', [Name, Value]),
        print_request(T).






%% make_quotable_0( ?ARG1, ?ARG2) is det.
%
% make quotable  Primary Helper.
%
make_quotable_0(SUnq,SObj):-atom_subst(SUnq,'\\','\\\\',SObj0),atom_subst(SObj0,'\n','\\n',SObj1),atom_subst(SObj1,'"','\\\"',SObj).



%% make_quotable( ?ARG1, ?ARG2) is det.
%
% Make Quotable.
%
make_quotable(String,SObj):-string(String),format(string(SUnq),'~s',[String]),make_quotable_0(SUnq,SObj),!.
make_quotable(String,SObj):-atomic(String),format(string(SUnq),'~w',[String]),make_quotable_0(SUnq,SObj),!.
make_quotable(String,SObj):-format(string(SUnq),'~q',[String]),make_quotable_0(SUnq,SObj),!.

% 
% <link rel="SHORTCUT ICON" href="/pixmaps/mini-logo.gif"><meta name="ROBOTS" content="NOINDEX, NOFOLLOW">

% :- set_yes_debug.

:- export(save_in_session/1).



%% save_in_session( :TermARG1) is det.
%
% Save In Session.
%
save_in_session(NV):- \+ compound(NV),!.
save_in_session(NV):-is_list(NV),!,must_maplist(save_in_session,NV),!.
save_in_session(search([X=Y|R])):-nonvar(Y),is_list([X=Y|R]),once(save_in_session([X=Y|R])),!.
save_in_session(NV):-NV=..[N,V],!,hmust(save_in_session(N,V)),!.
save_in_session(N=V):- hmust(save_in_session(N,V)),!.
save_in_session(NV):- dmsg(not_save_in_session(NV)),!.

:- export(save_in_session/2).



%% save_in_session( ?ARG1, ?ARG2) is det.
%
% Save In Session.
%
save_in_session(Unsaved,_):- member(Unsaved,[session_data,request_uri,search,pool,path,input,session]),!.
save_in_session(_,V):- sub_term(Sub,V),nonvar(Sub),is_stream(Sub),!.
save_in_session(N,V):- get_http_session(S), save_in_session(S, N,V),!.

% save_in_session(S,N,V):- \+ param_default_value(N,_),!.



%% save_in_session( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Save In Session.
%
save_in_session(S,N,V):- atom(N), NV=..[N,V],functor(NVR,N,1),
   retractall(http_session:session_data(S,NVR)),
   asserta(http_session:session_data(S,NV)),!.
save_in_session(S,N,V):- dmsg(not_save_in_session(S,N,V)),!.





%% show_http_session is det.
%
% Show Http Session.
%
show_http_session:-hmust(get_http_session(S)),listing(http_session:session_data(S,_NV)).
  




%% make_session( ?ARG1) is det.
%
% Make Session.
%
make_session(S):- ignore((is_cgi_stream,http_session:http_open_session(S,[renew(false)]))),!.

:- export(get_http_session/1).



%% get_http_session( ?ARG1) is det.
%
% Get Http Session.
%
get_http_session(S):- catch(get_http_session0(S),_,fail),nonvar(S),!, make_session(S).
get_http_session(main).

% on_x_log_fail(G):- catch(G,E,(dmsg(E:G),fail)).

:- export(get_http_session0/1).



%% get_http_session0( ?ARG1) is det.
%
% Get Http Session Primary Helper.
%
get_http_session0(S):- on_x_log_fail((http_session:http_in_session(S))),!.
get_http_session0(S):- on_x_log_fail((is_cgi_stream,http_session:http_open_session(S,[renew(false)]))),!.
get_http_session0(S):- on_x_log_fail((get_http_current_request(R),member(session(S),R))),!.
get_http_session0(S):- on_x_log_fail((get_http_current_request(R),member(cookie([swipl_session=S]),R))),!.




%% is_cgi_stream is det.
%
% If Is A Cgi Stream.
%
is_cgi_stream:-current_output(X),http_stream:is_cgi_stream(X).




%% reset_assertion_display is det.
%
% Reset Assertion Display.
%
reset_assertion_display:-
   flag(matched_assertions,_,0),
   flag(show_asserions_offered,_,0),
   retractall(shown_subtype(_)),
   retractall(shown_clause(_)).




%% get_param_sess( ?ARG1, ?ARG2) is det.
%
% Get Param Sess.
%
get_param_sess(N,V):- must(param_default_value(N,D)),!,get_param_sess(N,V,D),!.

:- dynamic(http_last_request/1).
:- volatile(mpred_www:http_last_request/1).
:- volatile(mpred_www:last_item_offered/1).



%% get_http_current_request( ?ARG1) is det.
%
% Get Http Current Request.
%
get_http_current_request(B):- httpd_wrapper:http_current_request(B), !,ignore((retractall(http_last_request(_)),asserta(http_last_request(B)))).
get_http_current_request(B):- http_last_request(B),!.
get_http_current_request([]).




%% get_param_sess( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Get Param Sess.
%
get_param_sess(N,V,D):- nonvar(V),!,get_param_sess(N,VV,D),!,param_matches(V,VV).
get_param_sess(L,V,D):-get_nv_session(L,V,D).




%% get_param_req( ?ARG1, ?ARG2) is det.
%
% Get Param Req.
%
get_param_req(L,V):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V,[optional(true),default(Foo)]],
  get_http_current_request(B),
   http_parameters:http_parameters(B,[CALL2])->
       V \== Foo,!.

% get_param_sess(L,V,V):- (is_list(L)-> member(N,L) ; N=L), save_in_session(N=V),!.




%% get_nv_session( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Get Nv Session.
%
get_nv_session(L,V,_):- (is_list(L)-> member(N,L) ; N=L),
     CALL2 =.. [N,V], (get_http_session(F),http_session:session_data(F, CALL2)),!.
get_nv_session(_,V,V):-!.





%% save_request_in_session( ?ARG1) is det.
%
% Save Request In Session.
%
save_request_in_session(Request):- 
        (member(method(post), Request) -> (http_read_data(Request, Data, []),save_in_session(Data));true),
        save_in_session(Request).
        % http_session:http_session_id(F),forall(http_session:session_data(F,D),wdmsg(D)).




:- dynamic(lmcache:current_ioet/4).


%% handler_logicmoo_cyclone( +Request) is det.
%
% Handler Logicmoo Cyclone.
%
handler_logicmoo_cyclone(Request):- fail, notrace(((is_goog_bot,!,
  format('Content-type: text/html~n~n',[]),
  format('<!DOCTYPE html><html><head></head><body><pre>~q</pre></body></html>~n~n',[Request]),flush_output))),!.
handler_logicmoo_cyclone(Request):-
 ignore((
 /*nodebugx*/once((
 /*with_no_x*/once((
 /*on_x_log_fail*/once((
   must_run_each((
   current_input(In),current_output(Out),current_error(Err),
   thread_self(ID),
   asserta(lmcahce:current_ioet(In,Out,Err,ID)),
   format('Content-type: text/html~n~n',[]),
   format('<!DOCTYPE html>',[]),
   flush_output,
      must(save_request_in_session(Request)),
    % member(request_uri(URI),Request),
      member(path(PATH),Request),
    directory_file_path(_,FCALL,PATH),
   once(get_param_req(call,Call);(current_predicate(FCALL/0),Call=FCALL);get_param_sess(call,Call,edit1term)),
   must_run_each(Call))))))))))),!.
   




%% write_begin_html( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Write Begin Html.
%
write_begin_html(B,BASE,URI):-  
  hmust_l((
      % sformat(BASE,'~w~@',[B,get_request_vars('_n_~w_v0_~w_vZ')]),
      BASE = B,
      format('<html><head><style type="text/css">
   element.style {
    position: relative;
    min-height: 100%;
    top: 0px;
}
html, body {
    font-family: Verdana,sans-serif;
    font-size: 10px;
    line-height: 1.5;
}
body {
    margin: 1;
}
        input[type="checkbox"] {width:10px; height:10px; }</style>',
        []),            
      hmust((get_http_current_request(Request))),
      hmust_l(member(request_uri(URI),Request)->true;URI=''),
      % ((URI\==''->format('<meta http-equiv="refresh" content="300;~w">',[URI]);true)),
      % hmust_l((BASE\='' -> format('<base href="~w" target="_parent"/>',[BASE]);true)),
      ignore(URI=''),
      ignore(BASE=''),
     format('<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>',[]),
     format('<title>~w for ~w</title></head>',[BASE,URI]),
     format('<body class="yui-skin-sam">',[]),flush_output)),!.
   




%% write_end_html is det.
%
% Write End Html.
%
write_end_html:- flush_output,format('</body></html>~n~n',[]),flush_output,!.

% logicmoo_html_needs_debug.




%% add_form_script is det.
%
% Add Form Script.
%
add_form_script:-format("
<script type=\"text/javascript\">
$('form').submit(function() {
  $(this).find('input[type=checkbox]').each(function (i, el) {
    if(!el.checked) {
      var hidden_el = $(el).clone();
      hidden_el[0].checked = true;
      hidden_el[0].value = '0';
      hidden_el[0].type = 'hidden';
      hidden_el.insertAfter($(el));
    }    
  })
 // alert($(this));
});

var handled = false;

function callback(e) {
    var e = window.e || e;

    var targ = e.target;
    if (targ.tagName !== 'A')
        return;
    if(!handled) {     
      handled = true;
     // alert('hi ' +  targ.target);
      if (targ.target !== '') {
       return;
      }
      e.preventDefault();
      e.stopPropagation();
      $('form').action = targ.href;
      document.getElementById('find').value = targ.innerText;
     // alert('hi ' +  targ.innerText);
      $('form').submit();
    } else {
      handled = false;           
    }
}

if (document.addEventListener)
    document.addEventListener('click', callback, false);
else
    document.attachEvent('onclick', callback);



</script>
"
).





%% show_pcall_footer is det.
%
% Show Pcall Footer.
%
show_pcall_footer:- format('<hr><a href="http://prologmoo.com">LogicMOO/PrologMUD</a>',[]),!.




%% sensical_nonvar( ?ARG1) is det.
%
% Sensical Nonvar.
%
sensical_nonvar(O):-nonvar(O), O \= (_ - _).




%% cvt_param_to_term( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Cvt Param Converted To Term.
%
cvt_param_to_term(In,Obj,Vs):-atom(In),on_x_fail(atom_to_term(In,Obj,Vs)),sensical_nonvar(Obj),!.
cvt_param_to_term(In,Obj,Vs):-string(In),on_x_fail(atom_to_term(In,Obj,Vs)),sensical_nonvar(Obj),!.



%% cvt_param_to_term( ?ARG1, ?ARG2) is det.
%
% Cvt Param Converted To Term.
%
cvt_param_to_term('~w',""):-!.
cvt_param_to_term(In,Obj):-cvt_param_to_term(In,Obj,_Vs),!.
cvt_param_to_term(Obj,Obj).


:- discontiguous param_default_value/2. 




%% param_default_value( ?ARG1, ?ARG2) is det.
%
% Param Default Value.
%
param_default_value(human_language,'EnglishLanguage').



%% human_language( ?ARG1) is det.
%
% Human Language.
%
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

param_default_value(call,edit1term).

param_default_value(N,V):-
  member(N=V,['prover'='proverPTTP','apply'='find','term'='',action_below=query,'action_above'='query','context'='BaseKB','flang'='CLIF','find'='tHumanHead','xref'='Overlap','POS'='N','humanLang'='EnglishLanguage','olang'='CLIF','sExprs'='1','webDebug'='1','displayStart'='0','displayMax'='100000']).

param_default_value(request_uri,'/logicmoo/').
param_default_value(logic_lang_name,'CLIF').
param_default_value(olang,'CLIF').
param_default_value(find,'tHumanHead').



%% logic_lang_name( ?ARG1, ?ARG2) is det.
%
% Logic Language Name.
%
logic_lang_name('CLIF',"Common Logic (CLIF)").
logic_lang_name('CycL',"CycL").
logic_lang_name('Prolog',"Prolog").
logic_lang_name('CGIF',"CG-Logic (CGIF)").
logic_lang_name('SUO-KIF',"SUO-KIF").
logic_lang_name('TPTP',"TPTP (fof/cnf)").
logic_lang_name('OWL',"OWL").

param_default_value(prover_name,'proverPTTP').



%% prover_name( ?ARG1, ?ARG2) is det.
%
% Prover Name.
%
prover_name("proverCyc","CycL (LogicMOO)").
prover_name("proverPFC","PFC").
prover_name("proverPTTP","PTTP (LogicMOO)").
prover_name("proverDOLCE","DOLCE (LogicMOO)").

param_default_value(partOfSpeech,'N').



%% partOfSpeech( ?ARG1, ?ARG2) is det.
%
% Part Of Speech.
%
partOfSpeech("N","Noun").
partOfSpeech("V","Verb").
partOfSpeech("J","Adjective").
partOfSpeech("Z","Adverb").




%% param_matches( ?ARG1, ?ARG2) is det.
%
% Param Matches.
%
param_matches(A,B):-A=B,!.
param_matches(VV,V):-atomic(VV),atomic(V),string_to_atom(VV,VVA),string_to_atom(V,VA),downcase_atom(VVA,VD),downcase_atom(VA,VD).
param_matches(A,B):-A=B,!.




%% show_select2( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Show Select Extended Helper.
%
show_select2(Name,Pred,Options):-
  
    Call=..[Pred,ID,Value],
    must(param_default_value(Name,D);param_default_value(Pred,D)),!,
    get_param_sess(Name,UValue,D),
    format('<select name="~w">',[Name]),
    forall(Call,
       (((member(atom_subst(Item,ItemName),Options) -> atom_subst(Value,Item,ItemName,NValue); NValue=Value),
        (((param_matches(UValue,ID);param_matches(UValue,NValue)) -> format('<option value="~w" selected="yes">~w</option>',[ID,NValue]);
                   format('<option value="~w">~w</option>',[ID,Value])))))),
    format('</select>',[]),!.





%% show_select1( ?ARG1, ?ARG2) is det.
%
% Show Select Secondary Helper.
%
show_select1(Name,Pred):-
 Call=..[Pred,Value],
 (param_default_value(Name,D);param_default_value(Pred,D)),!,
 format('<select name="~w">',[Name]),
 forall(Call,
    (get_param_sess(Name,Value,D)->format('<option value="~w" selected="yes">~w</option>',[Value,Value]);
                format('<option value="~w">~w</option>',[Value,Value]))),
 format('</select>',[]),!.





%% as_ftVars( :TermARG1) is det.
%
% Converted To Format Type Variables.
%
as_ftVars(N='$VAR'(N)):-atomic(N),!.
as_ftVars(_N=_V).
as_ftVars(_).

% :- system:use_module(library(logicmoo/util/logicmoo_util_varnames)).

% :- use_listing_vars.



%% search4term is det.
%
% Search4term.
%
search4term:- must_run_each((
  maybe_scan_for_varnames,
  get_param_sess(term,Term,"tHumanHead"),
  get_param_sess(find,SObj,Term),
  cvt_param_to_term(SObj,Obj),
  call_for_terms(make_page_pretext_obj(Obj)))),!.




%% edit1term is det.
%
% Edit1term.
%
edit1term:-  
  get_param_req('ASK','ASK'),!,
  with_main_wwwerror_to_output(
   must_run_each((
   get_param_sess(term,String,""),
   cvt_param_to_term(String,Term,VNs),
   save_in_session(find,Term),
   % call_for_terms
   edit1term(forall(Term,pp_item_html('Answer',':-'(VNs,Term))))))),!.
  
edit1term:- 
  get_param_req('TELL','TELL'),!,
  with_main_wwwerror_to_output(
   must_run_each((
   get_param_sess(term,String,""),
   cvt_param_to_term(String,Term,VNs),
   save_in_session(find,Term),
   maplist(as_ftVars,VNs),
   call_for_terms(forall(ain(Term),pp_item_html('Assert',':-'(VNs,Term))))))),!.
  
edit1term:- 
  get_param_req('RETRACT','RETRACT'),!,
  with_main_wwwerror_to_output(
   must_run_each((
   get_param_sess(term,String,""),
   cvt_param_to_term(String,Term,VNs),
   save_in_session(find,Term),
   maplist(as_ftVars,VNs),
   call_for_terms(forall(mpred_withdraw(Term),pp_item_html('Retract',':-'(VNs,Term))))))),!.
  
edit1term:- 
 must_run_each((
             reset_assertion_display,
             get_param_sess(term,String,""),get_param_sess(find,Word,""),term_to_pretty_string(Word,SWord),
                save_in_session(find,Word),
   show_edit_term(true,String,SWord))),!,
 show_iframe(search4term,find,SWord).




%% edit1term( :GoalARG1) is det.
%
% Edit1term.
%
edit1term(Call):-
 must_run_each((
             reset_assertion_display,
             get_param_sess(term,String,""),get_param_sess(find,Word,""),term_to_pretty_string(Word,SWord),save_in_session(find,Word),
   show_edit_term(Call,String,SWord))),!.





%% show_edit_term( :GoalARG1, ?ARG2, ?ARG3) is det.
%
% Show Edit Term.
%
show_edit_term(Call,String,_SWord):- cvt_param_to_term(String,T),compound(T),T=(H:-_),!,show_edit_term0(Call,String,H).
show_edit_term(Call,String,SWord):- show_edit_term0(Call,String,SWord),!.




%% show_edit_term0( :GoalARG1, ?ARG2, ?ARG3) is det.
%
% Show Edit Term Primary Helper.
%
show_edit_term0(Call,String,SWord):-atomic(SWord),cvt_param_to_term(SWord,T),nonvar(T),!,show_edit_term1(Call,String,T).
show_edit_term0(Call,String,SWord):-show_edit_term1(Call,String,SWord).




%% do_guitracer is det.
%
% Do Guitracer.
%
do_guitracer:- guitracer,trace.




%% show_edit_term1( :GoalARG1, ?ARG2, ?ARG3) is det.
%
% Show Edit Term Secondary Helper.
%
show_edit_term1(Call,String,(P=>Q)):-!,show_edit_term1(Call,String,(P;Q;(P=>Q))),!.
show_edit_term1(Call,String,SWord):- 
 write_begin_html('edit1term',_BASE,URL),!,
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
          <td align="left" valign="top" height="68">~@
             <br><b>Microthory</b><br>~@<br/><input type="submit" value="ASK" name="ASK"><input type="submit" value="TELL" name="TELL"><input type="submit" value="RETRACT" name="RETRACT">
             <br><b>Formal Language</b><br>~@</td>
      </tr>
        <tr><td><img src="/pixmaps/1pixel.gif" height="3"></td>
      		<td></td>
			<td></td>
			<td height="3"></td>
            </tr>
            <tr>
                  <td align="right" width="99"><b>Search:&nbsp;</b></td>
                  <td align="left" valign="top" width="276"><input type="text" size="27" name="find" value="~w">&nbsp;<input type="submit" value="Overlap" name="xref">&nbsp;</td>
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
                        <input type="checkbox" name="sExprs" value="1" checked>S-Exprs&nbsp;
                        <input type="checkbox" name="webDebug" value="1" checked>Debugging
                        </td>
          <td height="121" rowspan="2" width="188"></td>
      </tr>
		<tr>
			<td width="4">&nbsp;</td>
		</tr>
  </form></table><hr>'
  ,[show_select2(prover,prover_name,[]),
    String,
    action_menu_applied('action_above',"Item",""),
    show_select2('context',is_context,[]),
    show_select2(flang,logic_lang_name,[]),
    SWord,
    show_select2('POS',partOfSpeech,[]),
    show_select1('humanLang',human_language),
    URL,
    show_select2(olang,logic_lang_name,[])]),!,   
   format('<pre>',[]),
    on_x_rtrace(Call),!,
   format('</pre>',[]),
   write_end_html,!.




%% show_iframe( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Show Iframe.
%
show_iframe(URL,Name,Value):- format('<iframe width="100%" height="800" frameborder="0" scrolling="yes" marginheight="0" marginwidth="0" allowtransparency=true id="main" name="main" style="width:100%;height:800" src="~w?~w= ~w"></iframe>',[URL,Name,Value]).



%% show_iframe( ?ARG1) is det.
%
% Show Iframe.
%
show_iframe(URL):- format('<iframe width="100%" height="800" frameborder="0" scrolling="yes" marginheight="0" marginwidth="0" allowtransparency=true id="main" name="main" style="width:100%;height:800" src="search4term?find= ~w"></iframe>',[URL]).
  



%% show_search_filtersTop( ?ARG1) is det.
%
% Show Search Filters Top.
%
show_search_filtersTop(BR):- write(BR).




%% show_search_filters( ?ARG1) is det.
%
% Show Search Filters.
%
show_search_filters(BR):- 
   forall(search_filter_name_comment(N,C,_),session_checkbox(N,C,BR)).




%% parameter_names( ?ARG1, ?ARG2) is det.
%
% Parameter Names.
%
parameter_names(List,N):-is_list(List),!,member(E,List),parameter_names(E,N).
parameter_names(V,_):- var(V),!,fail.
parameter_names(N=_,N):-!,atom(N).
parameter_names(C,N):-compound(C),functor(C,N,1).




%% current_form_var( ?ARG1) is det.
%
% Current Form Variable.
%
current_form_var(N):-no_repeats((current_form_var0(N))),atom(N),\+ arg(_,v(peer,idle,ip,session),N).



%% current_form_var0( ?ARG1) is det.
%
% Current Form Variable Primary Helper.
%
current_form_var0(N):- param_default_value(N,_).
%current_form_var0(N):- get_http_current_request(B),member(search(Parameters),B),parameter_names(Parameters,N).
%current_form_var0(N):- http_current_session(_, Parameters),parameter_names(Parameters,N).




%% is_goog_bot is det.
%
% If Is A Goog Bot.
%
is_goog_bot:- get_http_current_request(B),member(user_agent(UA),B),!,atom_contains(UA,'Googlebot').
 
param_default_value(N,D):-search_filter_name_comment(N,_,D).



%% search_filter_name_comment( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Search Filter Name Comment.
%
search_filter_name_comment(hideMeta,'Hide Meta/BookKeeping','1').
search_filter_name_comment(hideSystem,'Skip System','0').
search_filter_name_comment(hideTriggers,'Hide Triggers','1').
search_filter_name_comment(skipLarge,'No Large','1').
search_filter_name_comment(showHyperlink,'Hyperlink','1').
search_filter_name_comment(showFilenames,'Filenames','0').
search_filter_name_comment(wholePreds,'Whole Preds','0').
search_filter_name_comment(skipVarnames,'Skip Varnames','0').
search_filter_name_comment(hideClauseInfo,'Skip ClauseInfo','1').
search_filter_name_comment(showAll,'Show All','0').
  




%% session_checked( ?ARG1) is det.
%
% Session Checked.
%
session_checked(Name):- get_param_sess(Name,V),V\=='0',V\==0,V\=="0".




%% session_checkbox( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Session Checkbox.
%
session_checkbox(Name,Caption,BR):-
 (session_checked(Name)-> CHECKED='CHECKED';CHECKED=''),
 format('<font size="-3"><input type="checkbox" name="~w" value="1" ~w />~w</font>~w',[Name,CHECKED,Caption,BR]).
 % format('<font size="-3"><label><input type="checkbox" name="~w" value="1" ~w/>~w</label></font>~w',[Name,CHECKED,Caption,BR]).




%% action_menu_applied( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Action Menu Applied.
%
action_menu_applied(MenuName,ItemName,Where):-
  format('<label>',[]),show_select2(MenuName,action_menu_item,[atom_subst('$item',ItemName)]),
      format('&nbsp;~w&nbsp;&nbsp;<input type="submit" value="Now" name="Apply">',[Where]),
      format('</label>',[]).

param_default_value(is_context,'BaseKB').



%% is_context( ?ARG1, ?ARG2) is det.
%
% If Is A Context.
%
is_context(MT,MT):-no_repeats(is_context0(MT)).



%% is_context0( ?ARG1) is det.
%
% If Is A Context Primary Helper.
%
is_context0(MT):- fail, if_defined(exactlyAssertedEL_first(isa, MT, 'Microtheory',_,_)).
is_context0('BaseKB').

param_default_value(action_menu_item,'query').



%% action_menu_item( ?ARG1, ?ARG2) is det.
%
% Action Menu Item.
%
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






%% get_request_vars( ?ARG1) is det.
%
% Get Request Variables.
%
get_request_vars(Format):- ignore(Exclude=[term,find,session_data,call,user_agent,referer,session,request_uri,accept]),
   findall(N=V,(current_form_var(N),\+ member(N,Exclude),once(get_param_sess(N,V))),NVs),
   forall(member(N=V,NVs),format(Format,[N,V])).


must_run_each((A,B)):-!, must_run_each(A),!,must_run_each(B),!.
must_run_each(List):- is_list(List),!,must_maplist(must_run_each,List),!.
must_run_each(G):- flush_output,must(G),flush_output,!.


%% call_for_terms( ?ARG1) is det.
%
% Call For Terms.
%
call_for_terms(Call):- 
   must_run_each(
    [
      get_param_sess(term,Term,"tHumanHead"),
      get_param_sess(find,SObj,Term),
      cvt_param_to_term(SObj,Obj),
        write_begin_html('search4term',Base,_),
        show_search_form(Obj,Base),
        format('<pre>',[]),
        w_tl(t_l:print_mode(html),with_search_filters(catch(ignore(Call),E,dmsg(E)))),
        format('</pre>',[]),flush_output,
        show_pcall_footer,
        write_end_html]),!.

:- thread_local(t_l:tl_hide_data/1).

show_search_form(Obj,Base):-
   must_run_each(
    [
        format('<form action="search4term" target="_self"><font size="-3"> Apply ',[]),
        action_menu_applied('action_below',"Checked or Clicked","&nbsp;below&nbsp;"),
        format('&nbsp;&nbsp;&nbsp;find = <input id="find" type="text" name="find" value="~q">~@  Base = ~w</font> <a href="edit1term" target="_top">edit1term</a> <hr/></form>~n~@',
            [Obj,show_search_filters('&nbsp;&nbsp;'),Base,add_form_script])]),  !.


%% with_search_filters( :GoalARG1) is det.
%
% Using Search Filters.
%
with_search_filters(C):-
   search_filter_name_comment(FILTER,_,_),
   session_checked(FILTER), 
   \+ t_l:tl_hide_data(FILTER),!,
    w_tl(t_l:tl_hide_data(FILTER),with_search_filters(C)).
with_search_filters(C):-call(C).





%% make_page_pretext_obj( ?ARG1) is det.
%
% Make Page Pretext Obj.
%

% make_page_pretext_obj(Obj):- atom(Obj),atom_to_term(Obj,Term,Bindings),nonvar(Term),Term\=@=Obj,!,hmust(make_page_pretext_obj(Term)).

make_page_pretext_obj(Obj):- 
 must_run_each((
  % catch(mmake,_,true),
  % forall(no_repeats(M:F/A,(f_to_mfa(Pred/A,M,F,A))),ignore(logOnFailure((this_listing(M:F/A),flush_output)))),
  % forall(no_repeats(M:F/A,(f_to_mfa(Pred/A,M,F,A))),ignore(logOnFailure((reply_object_sub_page(M:F/A),flush_output)))),
  % ignore((fail,catch(mpred_listing(Pred),_,true))),
  call_with_time_limit(300,ignore(catch(xlisting_inner(i2tml_hbr,Obj,[]),E,wdmsg(E)))),
  pp_i2tml_saved_done(Obj))),!.

make_page_pretext_obj(Obj):- writeq(make_page_pretext_obj(Obj)),!.



:- prolog_xref:assert_default_options(register_called(all)).




%% reply_object_sub_page( ?ARG1) is det.
%
% Reply Object Sub Page.
%
reply_object_sub_page(Obj) :- phrase(object_sub_page(Obj, []), HTML), html_write:print_html(HTML),!.


%%  object_sub_page(+ Obj, + Options)// is det.
%
% -->.
%
object_sub_page(Obj, Options) -->
	{ pldoc_process:doc_comment(Obj, File:_Line, _Summary, _Comment)
	}, !,
	(   { \+ ( pldoc_process:doc_comment(Obj, File2:_, _, _),
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









%% return_to_pos( :GoalARG1) is det.
%
% Return Converted To Pos.
%
return_to_pos(Call):- current_line_position(LP),Call,!, must(set_line_pos(LP)).



%% nl_same_pos is det.
%
% Nl Same Pos.
%
nl_same_pos:-return_to_pos(nl).






%% set_line_pos( ?ARG1) is det.
%
% Set Line Pos.
%
set_line_pos(LP):-current_output(Out),set_line_pos(Out,LP).



%% set_line_pos( ?ARG1, ?ARG2) is det.
%
% Set Line Pos.
%
set_line_pos(_,_):-!.
set_line_pos(Out,LP):- 
  current_line_position(Out,CLP), 
  (CLP==LP->! ;((CLP>LP->nl(Out);put_code(Out,32)),!,set_line_pos(Out,LP))).




%% current_line_position( ?ARG1) is det.
%
% Current Line Position.
%
current_line_position(LP):-current_output(Out),current_line_position(Out,LP).



%% current_line_position( ?ARG1, ?ARG2) is det.
%
% Current Line Position.
%
current_line_position(Out,LP):-stream_property(Out,position( Y)),stream_position_data(line_position,Y,LP),!.




%% tmw is det.
%
% Tmw.
%
tmw:- w_tl(t_l:print_mode(html),
 (rok_portray_clause(a(LP)),
  rok_portray_clause((a(LP):-b([1,2,3,4]))),
  nl,nl,call_u(wid(_,_,KIF)),
  KIF=(_=>_),nl,nl,print(KIF),listing(print_request/1))),!.
tmw:- w_tl(t_l:print_mode(html),(print((a(_LP):-b([1,2,3,4]))),nl,nl,wid(_,_,KIF),KIF=(_=>_),nl,nl,print(KIF),listing(print_request/1))),!.



% II = 56+TTT, ((show_call(why,(url_encode(II,EE),var_property(TTT,name(NNN)),url_decode(EE,OO))))),writeq(OO).




%% url_encode( ?ARG1, ?ARG2) is det.
%
% Url Encode.
%
url_encode(B,A):- \+ atom(B),!,term_variables(B,Vars),url_encode_term(B,Vars,O),O=A.
url_encode(B,A):- atom_concat('\n',BT,B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(BT,'\n',B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(' ',BT,B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(BT,' ',B),!,url_encode(BT,A).
url_encode(B,A):- url_iri(A,B).





%% url_encode_term( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Url Encode Term.
%
url_encode_term(B,[],O):- !, term_to_atom('#$'(B:[]),BB),!,url_iri(O,BB).
url_encode_term(InTerm,_VsIn,URL):- fail, with_output_to(atom(IRI),portray_clause('#$'((InTerm:_)))),
  url_iri(URL,IRI),nb_linkval(URL,InTerm),!.

url_encode_term(InTerm,VsIn,URL):-
  get_varname_list(Prev),
  name_the_var(40,Prev,VsIn,_NewVs,Added),
  % (NewVs\==Prev ->  show_call(why,put_variable_names(NewVs)) ; true),
  with_output_to(atom(IRI),write_term('#$'(InTerm:Added),[quoted(true),variable_names(Added),quoted,priority(9)])),
  url_iri(URL,IRI),!.




%% member_open( ?ARG1, :TermARG2) is det.
%
% Member Open.
%
member_open(C, [B|A]) :-  (nonvar(B),B=C) ; (nonvar(A),member_open(C, A)).




%% name_the_var( ?ARG1, ?ARG2, :TermARG3, :TermARG4, :TermARG5) is det.
%
% Name The Variable.
%
name_the_var(_Num,Vs,[],Vs,[]).

name_the_var(Num,Vs,[VIn|More],VsOut,[N=V|Added]):- member_open(N=V,Vs),VIn==V,!,name_the_var(Num,Vs,More,VsOut,Added).
% name_the_var(Num,Vs,[VIn|More],VsOut,[N=VIn|Added]):- \+ is_list(Vs), append(Vs,[N=VIn],NewVs),!, name_the_var(Num,NewVs,More,VsOut,Added).
name_the_var(Num,Vs,[VIn|More],[N=VIn|VsOut],[N=VIn|Added]):- Num2 is Num +1, NV = '$VAR'(Num),
  with_output_to(atom(N),write_term(NV,[portrayed(true),quoted,priority(9)])),
  name_the_var(Num2,Vs,More,VsOut,Added).



%  II = 56+TTT, rtrace((url_encode(II,EE),url_decode(EE,OO))),writeq(OO),OO=II.



% url_decode(B,A):- \+ atom(B),!,term_to_atom(B,BB),!,url_encode(BB,O),!,A=O.



%% url_decode( ?ARG1, ?ARG2) is det.
%
% Url Decode.
%
url_decode(B,A):- \+ atom(B),A=B.
url_decode(A,B):- atom_concat('#%24%28',_,A) , url_decode_term(A,T),!,T=B.
url_decode(A,B):- url_iri(A,C),!,B=C.




%% url_decode_term( ?ARG1, ?ARG2) is det.
%
% Url Decode Term.
%
url_decode_term(A,T):- nb_current(A,T),nb_delete(A),!.
url_decode_term(A,T):- url_iri(A,B),
    read_term_from_atom(B,'#$'(T:Vs2),[variable_names(Vs3)]),
    ignore(Vs2=Vs3),!, ignore(Vs2=[]),!.

url_decode_term(A,T):-
    url_iri(A,B),
    read_term_from_atom(B,'#$'(T:Vs2),[variable_names(Vs3)]),
    ignore(Vs2=[]),ignore(Vs2=Vs3),
    merge_key_vals(B,Vs2,Merge),
    get_varname_list(Env),
    merge_key_vals(Env,Merge,New),
    put_variable_names(New),!.






%% tovl( :TermARG1, :TermARG2, :TermARG3) is det.
%
% Tovl.
%
tovl([],[],[]).
tovl([K|KL],[V|VL],[K=V|KVL]) :- tovl(KL, VL, KVL).




%% merge_key_vals( :TermARG1, ?ARG2, ?ARG3) is det.
%
% Merge Key Vals.
%
merge_key_vals(Prev,Pairs,NewSave):-var(Prev),!,NewSave=Pairs.
merge_key_vals([],Pairs,NewSave):-!,NewSave=Pairs.
merge_key_vals([K=V1|Prev],Pairs,NewSave):-
   member_open(K=V2,Pairs),
   V1==V2, merge_key_vals(Prev,Pairs,NewSave).
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   member_open(K2=V2,Pairs),
   K1==K2, V1=V2, merge_key_vals(Prev,Pairs,NewSave).
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   merge_key_vals(Prev,[K1=V1|Pairs],NewSave).





% x(Z+B)

%   b_setval(URL,InTerm).




%% write_as_url_encoded( ?ARG1, ?ARG2) is det.
%
% Write Converted To Url Encoded.
%
write_as_url_encoded(_Arg, D):- url_encode(D,U),!,writeq(U).
:- format_predicate('u',write_as_url_encoded(_Arg,_Time)).




%% term_to_pretty_string( ?ARG1, ?ARG2) is det.
%
% Term Converted To Pretty String.
%
term_to_pretty_string(H,HS):-atomic(H),!,with_output_to(atom(HS),writeq(H)).
term_to_pretty_string(H,HS):-
   % igno re(source_variables(X))->ignore(X=[])->
   % numb ervars(HC,0,_)->
  with_output_to(atom(HS),portray_clause(H)).




%% fmtimg( ?ARG1, ?ARG2) is det.
%
% Fmtimg.
%
fmtimg(N,Alt):- t_l:print_mode(html),!,
 make_quotable(Alt,AltQ),
 url_encode(Alt,AltS),
 format('~N<a href="?call=edit1term&term= ~w" target="_parent"><img src="/pixmaps/~w.gif" alt="~w" title="~w"><a>',[AltS,N,AltQ,AltQ]).
fmtimg(_,_).





%% indent_nbsp( ?ARG1) is det.
%
% Indent Nbsp.
%
indent_nbsp(X):-t_l:print_mode(html),forall(between(0,X,_),format('&nbsp;')),!.
indent_nbsp(X):-forall(between(0,X,_),format('~t',[])),!.




%% indent_nl is det.
%
% Indent Nl.
%
indent_nl:- fresh_line, flag(indent,X,X), indent_nbsp(X).





%% indent_nbsp( :PRED1ARG1, ?ARG2) is det.
%
% Indent Nbsp.
%
indent_nbsp(0,''):-!.
indent_nbsp(1,'\n         '):-!.
indent_nbsp(X,Chars):-XX is X -1,!, indent_nbsp(XX,OutP),!,sformat(Chars,'~w   ',[OutP]),!.




:- multifile lmconf:shared_hide_data/1.




%% shared_hide_data( :PRED4ARG1) is det.
%
% Hook To [logicmoo_util_term_listing:shared_hide_data/1] For Module Mpred_www.
% Shared Hide Data.
%
lmconf:shared_hide_data('$si$':'$was_imported_kb_content$'/2):- !,listing_filter(hideMeta).
lmconf:shared_hide_data(spft/3):- !,listing_filter(hideTriggers).
lmconf:shared_hide_data(spft/3):- !,listing_filter(hideTriggers).
lmconf:shared_hide_data(nt/3):- !,listing_filter(hideTriggers).
lmconf:shared_hide_data(pt/2):- !, listing_filter(hideTriggers).
lmconf:shared_hide_data(bt/2):- !, listing_filter(hideTriggers).
lmconf:shared_hide_data((_:-
 cwc,
        second_order(_,G19865),
        (   _G19865 = (G19867,!,G19871) ->
                call(G19867),  !,
                call(G19871)
        ;   CALL
        ))):- CALL=@=call(G19865).

lmconf:shared_hide_data(mpred_mark/3):- !,listing_filter(hideMeta).





%% pp_now is det.
%
% Pretty Print Now.
%
pp_now.




%% this_listing( :TermARG1) is det.
%
% This Listing.
%
this_listing(M:F/A):-functor(H,F,A),predicate_property(M:H,number_of_causes(_)),!, forall(clause(M:H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-functor(H,F,A),predicate_property(H,number_of_causes(_)),!, forall(clause(H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-listing(M:F/A),!.
this_listing(MFA):-listing(MFA).

:- thread_local(sortme_buffer/2).


% i2tml_save(Obj,H):- \+ is_list(H),cyc:pterm_to_sterm(H,S),H\=@=S,!,i2tml_save(Obj,S).




%% pp_i2tml_saved_done( ?ARG1) is det.
%
% Pretty Print I2tml Saved Done.
%
pp_i2tml_saved_done(_Obj):-pp_now,!,flush_output.
pp_i2tml_saved_done(Obj):-
  findall(H,retract(sortme_buffer(Obj,H)),List),predsort(head_functor_sort,List,Set),
  forall(member(S,Set),pp_i2tml(S)),!.




%% find_cl_ref( :TermARG1, ?ARG2) is det.
%
% Find Clause Ref.
%
find_cl_ref(_,none):- t_l:tl_hide_data(hideClauseInfo),!.
find_cl_ref(clause(_,_,Ref),Ref):-!.
find_cl_ref(clause(H,B),Ref):- clause(H,B,Ref),!.
find_cl_ref((H:-B),Ref):-!, clause(H,B,Ref),clause(HH,BB,Ref),H=@=HH,B=@=BB,!.
find_cl_ref(H,Ref):- clause(H,true,Ref),clause(HH,true,Ref),H=@=HH,!.




%% find_ref( :TermARG1, ?ARG2) is det.
%
% Find Ref.
%
find_ref(_,none):- t_l:tl_hide_data(hideClauseInfo),!.
find_ref(H,Ref):- find_cl_ref(H,Ref),!.
find_ref(This,Ref):- '$si$':'$was_imported_kb_content$'(A,CALL),arg(1,CALL,This),clause('$si$':'$was_imported_kb_content$'(A,CALL),true,Ref),!.
find_ref(M:This,Ref):- atom(M),!,find_ref(This,Ref).




%% head_functor_sort( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Head Functor Sort.
%
head_functor_sort(Result,H1,H2):- (var(H1);var(H2)),compare(Result,H1,H2),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,A1),get_functor(H2,F2,A2))),F1==F2,A1>0,A2>0,arg(1,H1,E1),arg(1,H2,E2),compare(Result,E1,E2),Result \== (=),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,_),get_functor(H2,F2,_))),F1\==F2,compare(Result,F1,F2),Result \== (=),!.
head_functor_sort(Result,H1,H2):-compare(Result,H1,H2),!.




%% i2tml_hbr( ?ARG1, ?ARG2, ?ARG3) is det.
%
% I2tml Hbr.
%
i2tml_hbr(H,B,Ref):- nonvar(Ref),!,pp_i2tml_save_seen(clause(H,B,Ref)).
i2tml_hbr(H,B,_):- B==true,!, pp_i2tml_save_seen(H).
i2tml_hbr(H,B,_):- !,pp_i2tml_save_seen((H:-B)).




%% pp_i2tml_save_seen( ?ARG1) is det.
%
% Pretty Print I2tml Save Seen.
%
pp_i2tml_save_seen(HB):- pp_now, !,must(pp_i2tml(HB)),!.
pp_i2tml_save_seen(HB):- assertz_if_new(sortme_buffer(_Obj,HB)),!.


:- thread_local(t_l:pp_i2tml_hook/1).

:- thread_local(t_l:tl_hide_data/1).
   
:- thread_local(shown_subtype/1).
:- thread_local(shown_clause/1).
:- meta_predicate if_html(*,0).






%% section_open( ?ARG1) is det.
%
% Section Open.
%
section_open(Type):-  once(shown_subtype(Type)->true;((t_l:print_mode(html)->format('~n</pre><hr>~w<hr><pre>~n<font face="verdana,arial,sans-serif">',[Type]);(draw_line,format('% ~w~n~n',[Type]))),asserta(shown_subtype(Type)))),!.



%% section_close( ?ARG1) is det.
%
% Section Close.
%
section_close(Type):- shown_subtype(Type)->(retractall(shown_subtype(Type)),(t_l:print_mode(html)->format('</font>\n</pre><hr/><pre>',[]);draw_line));true.





%% pp_item_html( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item Html.
%
pp_item_html(_Type,H):-var(H),!.
pp_item_html(Type,done):-!,section_close(Type),!.
pp_item_html(_,H):-shown_clause(H),!.
pp_item_html(_,P):- (listing_filter(P); (compound(P),functor(P,F,A),(listing_filter(F/A);listing_filter(F)))),!.

pp_item_html(Type,H):- \+ t_l:print_mode(html), pp_item_html_now(Type,H),!.
pp_item_html(Type,H):- ignore((flag(matched_assertions,X,X),between(0,5000,X),pp_item_html_now(Type,H))).

:- dynamic(last_item_offered/1).



%% last_item_offered( ?ARG1) is det.
%
% Last Item Offered.
%
last_item_offered(unknonw).





%% pp_item_html_now( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item Html Now.
%
pp_item_html_now(Type,H):-    
   flag(matched_assertions,X,X+1),!,
   pp_item_html_if_in_range(Type,H),!,
   assert(shown_clause(H)),!.





%% pp_item_html_if_in_range( ?ARG1, ?ARG2) is det.
%
% Pretty Print Item Html If In Range.
%
pp_item_html_if_in_range(Type,H):- section_open(Type),!,pp_i2tml(H),!,nl.

:- thread_local(t_l:last_show_clause_ref/1).
:- thread_local(t_l:current_clause_ref/1).





%% show_clause_ref( ?ARG1) is det.
%
% Show Clause Ref.
%
show_clause_ref(Ref):- Ref == none,!.
show_clause_ref(Ref):- t_l:last_show_clause_ref(Ref),!.
show_clause_ref(Ref):- retractall(t_l:last_show_clause_ref(_)),asserta(t_l:last_show_clause_ref(Ref)),on_x_rtrace(show_clause_ref_now(Ref)),!.




%% show_clause_ref_now( :GoalARG1) is det.
%
% Show Clause Ref Now.
%
show_clause_ref_now(V):-var(V),!.
show_clause_ref_now(0):-!.
show_clause_ref_now(_Ref):- listing_filter(hideClauseRef),!.
show_clause_ref_now(Ref):- listing_filter(showFilenames), \+ clause_property(Ref,predicate(_)),format('~N~p~N',[clref(Ref)]),!.
% write_html(div(class(src_formats),a(href(EditLink), edit)])).
show_clause_ref_now(Ref):- listing_filter(showFilenames),clause_property(Ref,file(File)),ignore(clause_property(Ref,line_count(Line))),
  ignore(clause_property(Ref,module(Module))),
    format('<a href="/swish/filesystem/~w#L~w">@file:~w:~w</a>(~w)~N',[File,Line,File,Line,Module]),
    fail. 
show_clause_ref_now(Ref):- clause_property(Ref,erased),
  ignore(clause_property(Ref,module(Module))),
    format('erased(~w) (~w)~N',[Ref,Module]),!.




%% pp_i2tml( :TermARG1) is det.
%
% Pretty Print I2tml.
%
pp_i2tml(Done):-Done==done,!.
pp_i2tml(T):-var(T),format('~w',[T]),!.
pp_i2tml(T):-string(T),format('"~w"',[T]).
pp_i2tml(clause(H,B,Ref)):- !, w_tl(t_l:current_clause_ref(Ref),pp_i2tml_v((H:-B))).
pp_i2tml(HB):- find_ref(HB,Ref),!, must(w_tl(t_l:current_clause_ref(Ref),pp_i2tml_v((HB)))).
pp_i2tml(HB):- w_tl(t_l:current_clause_ref(none),must(pp_i2tml_v((HB)))).




%% numberlist_at( ?ARG1, :TermARG2) is det.
%
% Numberlist When.
%
numberlist_at(_,[]).
numberlist_at(_,[N|More]):- number(N),!,N2 is N+1,numberlist_at(N2,More),!.
numberlist_at(Was,[N|More]):-var(N),  N is Was+1, N2 is N+1,  numberlist_at(N2,More),!.
numberlist_at(Was,[_|More]):- N2 is Was+2, numberlist_at(N2,More),!.





%% pp_i2tml_v( ?ARG1) is det.
%
% Pretty Print I2tml V.
%
pp_i2tml_v(HB):- ignore(catch(( \+ \+ ((get_clause_vars_for_print(HB,HB2),pp_i2tml_0(HB2)))),_,true)),!.




%% pp_i2tml_0( :TermARG1) is det.
%
% Pretty Print i2tml  Primary Helper.
%
pp_i2tml_0(Var):-var(Var),!.
pp_i2tml_0(USER:HB):-USER==user,!,pp_i2tml_0(HB),!.
pp_i2tml_0((H :- B)):-B==true,!,pp_i2tml_0((H)),!.
pp_i2tml_0(((USER:H) :- B)):-USER==user,!,pp_i2tml_0((H:-B)),!.
pp_i2tml_0((H:-B)):-B==true, !, pp_i2tml_0(H).

pp_i2tml_0(P):- listing_filter(P),!.
pp_i2tml_0(was_chain_rule(H)):- pp_i2tml_0(H).
pp_i2tml_0(M:(H)):-M==user, pp_i2tml_0(H).
pp_i2tml_0(is_edited_clause(H,B,A)):- pp_i2tml_0(proplst([(clause)=H,before=B,after=A])).
pp_i2tml_0(is_disabled_clause(H)):- pp_i2tml_0((disabled)=H).


% pp_i2tml_0(FET):-fully_expand(change(assert,html_gen),FET,NEWFET),FET\=@=NEWFET,!,pp_i2tml_0(NEWFET).

pp_i2tml_0(spft(P,F,T,W)):-!,
   w_tl(t_l:current_why_source(W),pp_i2tml_0(spft(P,F,T))).

pp_i2tml_0(spft(P,U,U)):- nonvar(U),!, pp_i2tml_1(P:-asserted_by(U)).
pp_i2tml_0(spft(P,F,T)):- atom(F),atom(T),!, pp_i2tml_1(P:-asserted_in(F:T)).
pp_i2tml_0(spft(P,F,T)):- atom(T),!,  pp_i2tml_1(((P):-  T:'t-deduced',F)). 
pp_i2tml_0(spft(P,F,T)):- atom(F),!,  pp_i2tml_1(((P):-  F:'f-deduced',T)). 
pp_i2tml_0(spft(P,F,T)):- !, pp_i2tml_1((P:- ( 'deduced-from'=F,  (rule_why = T)))).
pp_i2tml_0(nt(_,Trigger,Test,Body)) :- !, pp_i2tml_1(proplst(['n-trigger'=Trigger , format=Test  ,  (body = (Body))])).
pp_i2tml_0(pt(_,Trigger,Body)):-      pp_i2tml_1(proplst(['p-trigger'=Trigger , ( body = Body)])).
pp_i2tml_0(bt(_,Trigger,Body)):-      pp_i2tml_1(proplst(['b-trigger'=Trigger ,  ( body = Body)])).

pp_i2tml_0(proplst([N=V|Val])):- is_list(Val),!, pp_i2tml_1(N:-([clause=V|Val])).
pp_i2tml_0(proplst(Val)):-!, pp_i2tml_1(:-(proplst(Val))).


pp_i2tml_0(M:H):- M==user,!,pp_i2tml_1(H).
pp_i2tml_0((M:H:-B)):- M==user,!,pp_i2tml_1((H:-B)).
pp_i2tml_0(HB):-pp_i2tml_1(HB).




%% if_html( ?ARG1, :GoalARG2) is det.
%
% If Html.
%
if_html(F,A):-t_l:print_mode(html),!,format(F,[A]).
if_html(_,A):-A.




%% pp_i2tml_1( ?ARG1) is det.
%
% Pretty Print i2tml  Secondary Helper.
%
pp_i2tml_1(H):- 
 once(((last_item_offered(Was);Was=foobar),get_functor(Was,F1,_A1),get_functor(H,F2,_A2),
   retractall(last_item_offered(Was)),asserta(last_item_offered(H)),
    ((F1 \== F2 -> if_html('~N<hr/>',true);true)))),flush_output,fail.

pp_i2tml_1(_H):- t_l:current_clause_ref(Ref),
    if_html('<font size="1">~@</font>',show_clause_ref(Ref)),fail.

pp_i2tml_1(H):- t_l:print_mode(html), 
  term_to_pretty_string(H,ALT)->
   functor_to_color(H,FC)->fmtimg(FC,ALT)->
    format('<input type="checkbox" name="assertion[]" value="~w">',[ALT]),fail.

pp_i2tml_1(H):- \+ \+ pp_i2tml_now(H).




%% pp_i2tml_now( ?ARG1) is det.
%
% Pretty Print I2tml Now.
%
pp_i2tml_now(C):- t_l:pp_i2tml_hook(C),!.
pp_i2tml_now(C):- if_html('<font size="3">~@</font>~N',if_defined(rok_portray_clause(C),portray_clause(C))).







%% functor_to_color( ?ARG1, ?ARG2) is det.
%
% Functor Converted To Color.
%
functor_to_color(wid(_,_,G),C):-!,functor_to_color(G,C).
functor_to_color(G,C):-compound(G),functor(G,F,A),functor_to_color(G,F,A,C).
functor_to_color(_G,green):-!.





%% functor_to_color( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Functor Converted To Color.
%
functor_to_color(_G,isa,_,bug_btn_s).

functor_to_color(_G,genls,1,'plus-green').
functor_to_color(_G,arity,_,'white').
functor_to_color(_G,argIsa,_,'white').
functor_to_color(_G,argGenls,_,'white').

functor_to_color(_,_,1,yellow).

functor_to_color(G:-_,_,_,C):-nonvar(G),!,functor_to_color(G,C).



functor_to_color(_,(<==>),_,'plus-purple').
functor_to_color(_,(<-),_,purple).
functor_to_color(_,(<=),_,'cyc-right-triangle-violet').
functor_to_color(_,(==>),_,'cyc-right-triangle-violet').
functor_to_color(_,(:-),_,red_diam).


functor_to_color(_,-,_,red).
functor_to_color(_,not,_,red).
functor_to_color(_,~,_,red).
functor_to_color(_,~,_,red).

functor_to_color(_,(if),_,cy_menu).
functor_to_color(_,(iff),_,cyan).
functor_to_color(_,(all),_,cyan).
functor_to_color(_,(exists),_,blue).

functor_to_color(_,(mudEquals),_,pink).
functor_to_color(_,(skolem),_,pink).
functor_to_color(_,(wid),_,green_yellow).

functor_to_color(G,_,_,'lightgrey'):-predicate_property(G,foreign).
functor_to_color(G,_,_,'cyc-logo-3-t'):-predicate_property(G,built_in).

% :- (thread_property(ID,status(running)),ID=reloader30) -> true; thread_create(((repeat,sleep(30),mmake,fail)),_,[alias(reloader30),detached(true)]).
% ===================================================
% Pretty Print Formula
% ===================================================
:- export(write_atom_link/1).



%% write_atom_link( ?ARG1) is det.
%
% Write Atom Link.
%
write_atom_link(A):-must(write_atom_link(A,A)).
:- export(write_atom_link/2).



%% write_atom_link( ?ARG1, ?ARG2) is det.
%
% Write Atom Link.
%
write_atom_link(L,N):-must_run_each((write_atom_link(atom(W),L,N),format('~w',[W]))),!.

% pred_href(Name/Arity, Module, HREF) :-



%% write_atom_link( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Write Atom Link.
%
write_atom_link(W,A/_,N):-atom(A),!,write_atom_link(W,A,N).
write_atom_link(W,C,N):-compound(C),get_functor(C,F,A),!,write_atom_link(W,F/A,N).
%write_atom_link(W,_,N):- thread_self(main),!,write_term_to_atom_one(W,N),!.
write_atom_link(W,_,N):- must(nonvar(W)),\+ t_l:print_mode(html),write_term_to_atom_one(W,N),!.
write_atom_link(W,A,N):- nonvar(W),catch((term_to_pretty_string(A,AQ),
   url_encode(AQ,URL),
   format(W,'<a href="?f= ~w">~w</a>',[URL,AQ])),_,write_term_to_atom_one(W,N)).




%% write_term_to_atom_one( :TermARG1, ?ARG2) is det.
%
% Write Term Converted To Atom One.
%
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





%% portable_display( ?ARG1) is det.
%
% Portable Display.
%
portable_display(Term) :-
	write_out(Term, display, 1200, punct, _).





%% portable_print( ?ARG1) is det.
%
% Portable Print.
%
portable_print(Term) :-
	write_out(Term, print, 1200, punct, _).





%% portable_write( ?ARG1) is det.
%
% Portable Write.
%
portable_write(Term) :-
	write_out(Term, write, 1200, punct, _).





%% portable_writeq( ?ARG1) is det.
%
% Portable Writeq.
%
portable_writeq(Term) :-
	write_out(Term, writeq, 1200, punct, _).



%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.




%% maybe_paren( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Maybe Paren.
%
maybe_paren(P, Prio, Char, _, punct) :-
	P > Prio,
	!,
	put(Char).
maybe_paren(_, _, _, C, C).



%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.




%% maybe_space( ?ARG1, ?ARG2) is det.
%
% Maybe Space.
%
maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
	put(32).
maybe_space(quote, alpha) :- !,
	put(32).
maybe_space(_, _).



%   put_string(S)
%   writes a list of character codes.





%% put_string( ?ARG1) is det.
%
% Put String.
%
put_string(B):-put_string0(B).



%% put_string0( :TermARG1) is det.
%
% Put String Primary Helper.
%
put_string0([]).
put_string0([H|T]) :-
	put(H),
	put_string0(T).


%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.




%% put_string( ?ARG1, ?ARG2) is det.
%
% Put String.
%
put_string(A,B):- t_l:print_mode(html),!,
  with_output_to(atom(S),put_string0(A,B)),
  url_iri(URL,S),format('<a href="?f= ~w">~w</a>',[URL,S]).

put_string(A,B):- put_string0(A,B).

% :-rtrace.



%% put_string0( :TermARG1, ?ARG2) is det.
%
% Put String Primary Helper.
%
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




%% write_variable( ?ARG1) is det.
%
% Write Variable.
%
write_variable(V) :-
	write(V).



%   write_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style (display,write,writeq,print)
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.




%% write_out( :TermARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Write Out.
%
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





%% write_out( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5, ?ARG6, ?ARG7) is det.
%
% Write Out.
%
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





%% write_oper( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Write Oper.
%
write_oper(Op, Prio, Style, Ci, Co) :-
	Prio < 700, !,
	write_atom(Op, Style, Ci, Co).
write_oper(Op, _, Style, _Ci, punct) :-
	put(32),
	write_atom(Op, Style, punct, _),
	put(32).




%% write_VAR( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Write Var.
%
write_VAR(A, _Style, _Ci, _Co) :- atom(A), !,write(A).
write_VAR(N, writeq, _Ci, alpha):- writeq('$VAR'(N)),!.
write_VAR(X, Style, Ci, punct) :-
	write_atom('$VAR', Style, Ci, _),
	write_args(0, 1, '$VAR'(X), 40, Style).





%% write_atom( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% Write Atom.
%
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




%% classify_name( :TermARG1, ?ARG2) is det.
%
% Classify Name.
%
classify_name([H|T], alpha) :-
	H >= 97, H =< 122,
	!,
	classify_alpha_tail(T).
classify_name([H|T], other) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).




%% classify_alpha_tail( :TermARG1) is det.
%
% Classify Alpha Tail.
%
classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
	(  H >= 97, H =< 122
	;  H >= 65, H =< 90
	;  H >= 48, H =< 57
	;  H =:= 95
	), !,
	classify_alpha_tail(T).




%% classify_other_tail( :TermARG1) is det.
%
% Classify Other Tail.
%
classify_other_tail([]).
classify_other_tail([H|T]) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).



%   write_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .




%% write_args( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is det.
%
% Write Arguments.
%
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




%% write_tail( :TermARG1, ?ARG2) is det.
%
% Write Tail.
%
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




%% portable_listing is det.
%
% Portable Listing.
%
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




%% portable_listing( :TermARG1) is det.
%
% Portable Listing.
%
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





%% functor spec( ?ARG1, ?ARG2, :GoalARG3, :PRED255ARG4) is det.
%
% Functor Spec.
%
'functor spec'(Name/Low-High, Name, Low, High) :- !.
'functor spec'(Name/Arity, Name, Arity, Arity) :- !.
'functor spec'(Name, Name, 0, 255).




%% rok_portray_clause( :TermARG1) is det.
%
% Rok Portray Clause.
%
rok_portray_clause(Var):-var(Var),!,writeq(Var).
rok_portray_clause(:-(Command)) :-
	(   Command = public(Body), Key = (public)
	;   Command = mode(Body),   Key = (mode)
	;   Command = type(Body),   Key = (type)
	;   Command = pred(Body),   Key = (pred)
	;   Command = Body,	    Key = ''
	),  !,
	nl,
	% nu mbervars(Body, 0, _),
	\+ \+ 'list clauses'(Body, Key, 2, 8).
rok_portray_clause((Pred:-Body)) :-
	% nu mbervars(Pred+Body, 0, _),
	\+ \+ portable_writeq(Pred),
	\+ \+ 'list clauses'(Body, 0, 2, 8), !.
rok_portray_clause((Pred)) :-
	rok_portray_clause((Pred:-true)).





%% list clauses( :TermARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% List Clauses.
%
'list clauses'((A,B), L, R, D) :- !,
	'list clauses'(A, L, 1, D), !,
	'list clauses'(B, 1, R, D).
'list clauses'(true, _L, 2, _D) :- !,
	put(0'.
        ), nl.
        
'list clauses'((A;B), L, R, D) :- !,
	'list magic'(fail, L, D),
	'list magic'((A;B), 0, 2, D),
	'list magic'(R, '.
'
).

'list clauses'((A->B), L, R, D) :- !,
	'list clauses'(A, L, 5, D), !,
	'list clauses'(B, 5, R, D).
'list clauses'(Goal, L, R, D) :-
	'list magic'(Goal, L, D),
	portable_writeq(Goal),
	'list magic'(R, '.
'
).




%% list magic( ?ARG1, :PRED5ARG2, ?ARG3) is det.
%
% List Magic.
%
'list magic'(!,    0, _D) :- !,
	write(' :- ').
'list magic'(!,    1, _D) :- !,
	write(',  ').
'list magic'(_Goal, 0, D) :- !,
	write(' :- '),
	nl, tab(D).
'list magic'(_Goal, 1, D) :- !,
	put(0',
        ),
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





%% list magic( ?ARG1, ?ARG2) is det.
%
% List Magic.
%
'list magic'(2, C) :- !, write(C).
'list magic'(_, _).





%% list magic( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is det.
%
% List Magic.
%
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
:- public test_portray_clause/1.
*/





%:- endif. 


:- dynamic user:portray/1.
:- multifile user:portray/1.

% '$messages':lmconf:my_portray(X):-fail,loop_check(lmconf:my_portray(X)).
% user:portray(X):-loop_check(lmconf:my_portray(X)).
/*
:- discontiguous my_portray/1. 
:- export(lmconf:my_portray/1).
lmconf:my_portray(A) :- var(A),!,fail,writeq(A).
lmconf:my_portray(A) :-
        atom(A),
        sub_atom(A, 0, _, _, 'http://'), !,
        (   style(B)
        ->  true
        ;   B=prefix:id
        ),
        portray_url(B, A).
lmconf:my_portray(A) :-
        atom(A),
        atom_concat('__file://', B, A),
        sub_atom(B, D, _, C, #),
        sub_atom(B, _, C, 0, G),
        sub_atom(B, 0, D, _, E),
        file_base_name(E, F),
        format('__~w#~w', [F, G]).
lmconf:my_portray(A) :- atom(A),!,lmconf:write_atom_link(A,A).
lmconf:my_portray(A) :- \+compound(A),fail.
%lmconf:my_portray(P):- hmust((return_to_pos(rok_portray_clause(P)),!)).
*/







%% sanity_test_000 is det.
%
% Optional Sanity Checking test  Primary Helper Primary Helper Primary Helper.
%
sanity_test_000:- rok_portray_clause((
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



mpred_www_file.
% :- ensure_webserver(6767).

t123:- w_tl(t_l:print_mode(html),xlisting_inner(i2tml_hbr,end_of_file,[])).


/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_dmsg.pl
:- module(logicmoo_util_dmsg,
          [ ansi_control_conv/2,
            ansicall/2,
            ansicall/3,
            ansicall0/3,
            ansicall1/3,
            ansifmt/2,
            ansifmt/3,
            clauseST/2,
            colormsg/2,
            contrasting_color/2,
            defined_message_color/2,
            dfmt/1,
            dfmt/2,
            dmsg/3,
          dmsg/1,
          dmsg/2,
          cls/0,
            dmsg0/1,
            dmsg0/2,
            dmsg1/1,
            dmsg2/1,
            dmsg3/1,
            dmsg4/1,
            dmsg5/1,
            dmsg5/2,
            dmsg_hide/1,
            dmsg_hides_message/1,
            dmsg_show/1,
            dmsg_showall/1,
            dmsg_text_to_string_safe/2,
            dmsginfo/1,
            dumpST/0,
            dumpST/1,
            dumpST/2,
            dumpST/3,
            dumpST0/0,
            dumpST00/0,
            dumpST2_broke/2,
            dumpST4/4,
            dumpST9/0,
            dumpST90/0,
            dumpST_Parent/4,
            f_word/2,
            fdmsg/1,
            fdmsg1/1,
            fg_color/2,
            flush_output_safe/0,
            flush_output_safe/1,
            fmt/1,
            fmt/2,
            fmt/3,
            fmt0/1,
            fmt0/2,
            fmt0/3,
            fmt9/1,
            fmt_ansi/1,
            fmt_or_pp/1,
            fmt_portray_clause/1,
            functor_color/2,
            getPFA/3,
            getPFA1/3,
            getPFA2/3,
            get_indent_level/1,
            get_m_opt/4,
            good_next_color/1,
            if_color_debug/0,
            if_color_debug/1,
            if_color_debug/2,
            in_cmt/1,
            indent_e/1,
            indent_to_spaces/2,
            is_sgr_on_code/1,
            is_tty/1,
            keep_line_pos/2,
            last_used_fg_color/1,
            mesg_arg1/2,
            mesg_color/2,
            msg_to_string/2,
            neg1_numbervars/3,
            next_color/1,
            no_trace_dump/1,
            portray_clause_w_vars/1,
            portray_clause_w_vars/2,
            portray_clause_w_vars/3,
            portray_clause_w_vars/4,
            predef_functor_color/2,
            prepend_each_line/2,
            print_prepended/2,
            print_prepended_lines/2,
            random_color/1,
            sformat/4,
            sgr_code_on_off/3,
            sgr_off_code/2,
            sgr_on_code/2,
            sgr_on_code0/2,
            simplify_goal_printed/2,
            simplify_goal_printed0/2,
            to_stderror/1,
            tst_color/0,
            tst_color/1,
            tst_fmt/0,
            unliked_ctrl/1,
            vdmsg/2,
            withFormatter/4,
            with_all_dmsg/1,
            with_current_indent/1,
            with_dmsg/2,
            with_no_dmsg/1,
            with_no_dmsg/2,
            with_output_to_console/1,
            with_output_to_main/1,
            with_output_to_stream/2,
            with_show_dmsg/2,
            writeFailureLog/2
          ]).
:- multifile
        term_color0/2.
:- meta_predicate
        ansicall(?, 0),
        ansicall(?, ?, 0),
        ansicall0(?, ?, 0),
        ansicall1(?, ?, 0),
        fmt_ansi(0),
        if_color_debug(0),
        if_color_debug(0, 0),
        in_cmt(0),
        keep_line_pos(?, 0),
        neg1_numbervars(?, ?, 0),
        no_trace_dump(0),
        prepend_each_line(?, 0),
        to_stderror(0),
        with_all_dmsg(0),
        with_current_indent(0),
        with_dmsg(?, 0),
        with_no_dmsg(0),
        with_no_dmsg(?, 0),
        with_output_to_console(0),
        with_output_to_main(0),
        with_output_to_stream(?, 0),
        with_show_dmsg(?, 0).
:- module_transparent
        ansi_control_conv/2,
        ansifmt/2,
        ansifmt/3,
        clauseST/2,
        colormsg/2,
        contrasting_color/2,
        defined_message_color/2,
        dfmt/1,
        dfmt/2,
        dmsg/3,
        dmsg0/1,
        dmsg0/2,
        dmsg1/1,
        dmsg2/1,
        dmsg3/1,
        dmsg4/1,
        dmsg5/1,
        dmsg5/2,
        dmsg_hide/1,
        dmsg_hides_message/1,
        dmsg_show/1,
        dmsg_showall/1,
        dmsg_text_to_string_safe/2,
        dmsginfo/1,
        dumpST/0,
        dumpST/1,
        dumpST/2,
        dumpST/3,
        dumpST0/0,
        dumpST00/0,
        dumpST2_broke/2,
        dumpST4/4,
        dumpST9/0,
        dumpST90/0,
        dumpST_Parent/4,
        f_word/2,
        fdmsg/1,
        fdmsg1/1,
        fg_color/2,
        flush_output_safe/0,
        flush_output_safe/1,
        fmt/1,
        fmt/2,
        fmt/3,
        fmt0/1,
        fmt0/2,
        fmt0/3,
        fmt9/1,
        fmt_or_pp/1,
        fmt_portray_clause/1,
        functor_color/2,
        getPFA/3,
        getPFA1/3,
        getPFA2/3,
        get_indent_level/1,
        get_m_opt/4,
        good_next_color/1,
        if_color_debug/0,
        indent_e/1,
        indent_to_spaces/2,
        is_sgr_on_code/1,
        is_tty/1,
        last_used_fg_color/1,
        mesg_arg1/2,
        mesg_color/2,
        msg_to_string/2,
        next_color/1,
        portray_clause_w_vars/1,
        portray_clause_w_vars/2,
        portray_clause_w_vars/3,
        portray_clause_w_vars/4,
        predef_functor_color/2,
        print_prepended/2,
        print_prepended_lines/2,
        random_color/1,
        sformat/4,
        sgr_code_on_off/3,
        sgr_off_code/2,
        sgr_on_code/2,
        sgr_on_code0/2,
        simplify_goal_printed/2,
        simplify_goal_printed0/2,
        tst_color/0,
        tst_color/1,
        tst_fmt/0,
        unliked_ctrl/1,
        vdmsg/2,
        withFormatter/4,
        writeFailureLog/2.
:- dynamic
        defined_message_color/2,
        term_color0/2.



:- if(current_predicate(logicmoo_utils:combine_logicmoo_utils/0)).
:- module(logicmoo_util_dmsg,
[  % when the predciates are not being moved from file to file the exports will be moved here
       ]).

:- else.
:- include('logicmoo_util_header.pi').
:- endif.




% ==========================================================
% Sending Notes
% ==========================================================
:- thread_local( tlbugger:tlbugger:dmsg_match/2).
% = :- meta_predicate(with_all_dmsg(0)).
% = :- meta_predicate(with_show_dmsg(*,0)).


with_all_dmsg(Call):-
   w_tl(tlbugger:tl_always_show_dmsg,
     w_tl(set_prolog_flag(opt_debug,true),
       w_tl( tlbugger:dmsg_match(show,_),Call))).


with_show_dmsg(TypeShown,Call):-
  w_tl(set_prolog_flag(opt_debug,filter),
     w_tl( tlbugger:dmsg_match(showing,TypeShown),Call)).

% = :- meta_predicate(with_no_dmsg(0)).
with_no_dmsg(Call):- always_show_dmsg,!,Call.
with_no_dmsg(Call):-w_tl(set_prolog_flag(opt_debug,false),Call).
with_no_dmsg(TypeUnShown,Call):-w_tl(set_prolog_flag(opt_debug,filter),
  w_tl( tlbugger:dmsg_match(hidden,TypeUnShown),Call)).

% dmsg_hides_message(_):- !,fail.
dmsg_hides_message(_):- current_prolog_flag(opt_debug,false),!.
dmsg_hides_message(_):- current_prolog_flag(opt_debug,true),!,fail.
dmsg_hides_message(C):-  tlbugger:dmsg_match(HideShow,Matcher),matches_term(Matcher,C),!,HideShow=hidden.

dmsg_hide(isValueMissing):-!,set_prolog_flag(opt_debug,false).
dmsg_hide(Term):-set_prolog_flag(opt_debug,filter),must(nonvar(Term)),aina( tlbugger:dmsg_match(hidden,Term)),retractall( tlbugger:dmsg_match(showing,Term)),nodebug(Term).
dmsg_show(isValueMissing):-!,set_prolog_flag(opt_debug,true).
dmsg_show(Term):-set_prolog_flag(opt_debug,filter),aina( tlbugger:dmsg_match(showing,Term)),ignore(retractall( tlbugger:dmsg_match(hidden,Term))),debug(Term).
dmsg_showall(Term):-ignore(retractall( tlbugger:dmsg_match(hidden,Term))).


indent_e(0):-!.
indent_e(X):- X > 20, XX is X-20,!,indent_e(XX).
indent_e(X):- catchvvnt((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).

dmsg_text_to_string_safe(Expr,Forms):-on_x_fail(text_to_string(Expr,Forms)).

% ===================================================================
% Lowlevel printng
% ===================================================================
:- multifile term_to_message_string/2.
:- dynamic term_to_message_string/2.

fmt0(user_error,F,A):-!,current_main_error_stream(Err),!,format(Err,F,A).
fmt0(current_error,F,A):-!,thread_current_error_stream(Err),!,format(Err,F,A).
fmt0(X,Y,Z):-catchvvnt((format(X,Y,Z),flush_output_safe(X)),E,dfmt(E:format(X,Y))).
fmt0(X,Y):-catchvvnt((format(X,Y),flush_output_safe),E,dfmt(E:format(X,Y))).
fmt0(X):- (atomic(X);is_list(X)), dmsg_text_to_string_safe(X,S),!,format('~w',[S]),!.
fmt0(X):- (atom(X) -> catchvvnt((format(X,[]),flush_output_safe),E,dmsg(E)) ; (term_to_message_string(X,M) -> 'format'('~q~N',[M]);fmt_or_pp(X))).
fmt(X):-fresh_line,fmt_ansi(fmt0(X)).
fmt(X,Y):- fresh_line,fmt_ansi(fmt0(X,Y)),!.
fmt(X,Y,Z):- fmt_ansi(fmt0(X,Y,Z)),!.

fmt9(fmt0(F,A)):-on_x_fail(fmt0(F,A)),!.
fmt9(Msg):- if_defined_else(portray_clause_w_vars(Msg),print(Msg)).

% :-reexport(library(ansi_term)).
:- use_module(library(ansi_term)).

tst_fmt:- make,
 findall(R,(clause(ansi_term:sgr_code(R, _),_),ground(R)),List),
 ignore((
        ansi_term:ansi_color(FC, _),
        member(FG,[hfg(FC),fg(FC)]),
        % ansi_term:ansi_term:ansi_color(Key, _),
        member(BG,[hbg(default),bg(default)]),
        member(R,List),
        % random_member(R1,List),
    C=[reset,R,FG,BG],
  fresh_line,
  ansi_term:ansi_format(C,' ~q ~n',[C]),fail)).


fmt_ansi(Call):-ansicall([reset,bold,hfg(white),bg(black)],Call).

fmt_portray_clause(X):- renumbervars(X,Y),!, portray_clause(Y).

fmt_or_pp(portray((X:-Y))):-!,fmt_portray_clause((X:-Y)),!.
fmt_or_pp(portray(X)):-!,functor_safe(X,F,A),fmt_portray_clause((pp(F,A):-X)),!.
fmt_or_pp(X):-format('~q~N',[X]).

with_output_to_console(X):- current_main_error_stream(Err),with_output_to_stream(Err,X).
with_output_to_main(X):- current_main_error_stream(Err),with_output_to_stream(Err,X).

dfmt(X):- thread_current_error_stream(Err),with_output_to_stream(Err,fmt(X)).
dfmt(X,Y):- thread_current_error_stream(Err), with_output_to_stream(Err,fmt(X,Y)).

with_output_to_stream(Stream,Goal):-
    current_output(Saved),
   setup_call_cleanup(set_output(Stream),
         Goal,
         set_output(Saved)).

to_stderror(Call):- thread_current_error_stream(Err), with_output_to_stream(Err,Call).



:- dynamic dmsg_log/3.



:- thread_local tlbugger:is_with_dmsg/1.

with_dmsg(Functor,Goal):-
   w_tl(t_l:is_with_dmsg(Functor),Goal).


:- use_module(library(listing)).
sformat(Str,Msg,Vs,Opts):- nonvar(Msg),functor_safe(Msg,':-',_),!,with_output_to(string(Str),portray_clause_w_vars(user_output,Msg,Vs,Opts)).
sformat(Str,Msg,Vs,Opts):- with_output_to(chars(Codes),(current_output(CO),portray_clause_w_vars(CO,':-'(Msg),Vs,Opts))),append([_,_,_],PrintCodes,Codes),'sformat'(Str,'   ~s',[PrintCodes]),!.


portray_clause_w_vars(Out,Msg,Vs,Options):- \+ \+ ((prolog_listing:do_portray_clause(Out,Msg,[variable_names(Vs),numbervars(true),character_escapes(true),quoted(true)|Options]))),!.
portray_clause_w_vars(Msg,Vs,Options):- portray_clause_w_vars(current_output,Msg,Vs,Options).
portray_clause_w_vars(Msg,Options):- source_variables_lwv(Vs),portray_clause_w_vars(current_output,Msg,Vs,Options).


source_variables_lwv(AllS):-
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (nb_current('$variable_names', Vs2);Vs2=[]),
  %notrace(catch((parent_goal('$toplevel':'$execute_goal2'(_, Vs3),_);Vs3=[]),E,(writeq(E),Vs3=[]))),
  ignore(Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  nb_linkval('$variable_names', AllS).

export(portray_clause_w_vars/1).
portray_clause_w_vars(Msg):- portray_clause_w_vars(Msg,[]),!.
:- 'mpred_trace_none'(portray_clause_w_vars(_)).

print_prepended(Pre,S):-atom_concat(L,' ',S),!,print_prepended(Pre,L).
print_prepended(Pre,S):-atom_concat(L,'\n',S),!,print_prepended(Pre,L).
print_prepended(Pre,S):-atom_concat('\n',L,S),!,print_prepended(Pre,L).
print_prepended(Pre,S):-atomics_to_string(L,'\n',S),print_prepended_lines(Pre,L).
print_prepended_lines(_Pre,[]):- format('~N',[]).
print_prepended_lines(Pre,[H|T]):-format('~N~w~w',[Pre,H]),print_prepended_lines(Pre,T).


% in_cmt(Call):- tlbugger:no_slow_io,!,format('~N/*~n',[]),call_cleanup(Call,format('~N*/~n',[])).
in_cmt(Call):- call_cleanup(prepend_each_line('% ',Call),format('~N',[])).

with_current_indent(Call):- get_indent_level(Indent), indent_to_spaces(Indent,Space),prepend_each_line(Space,Call).

indent_to_spaces(1,' '):-!.
indent_to_spaces(0,''):-!.
indent_to_spaces(2,'  '):-!.
indent_to_spaces(3,'   '):-!.
indent_to_spaces(N,Out):- 1 is N rem 2,!, N1 is N-1, indent_to_spaces(N1,Spaces),atom_concat(' ',Spaces,Out).
indent_to_spaces(N,Out):- N2 is N div 2, indent_to_spaces(N2,Spaces),atom_concat(Spaces,Spaces,Out).

prepend_each_line(Pre,Call):-with_output_to(string(Str),Call)*->once(print_prepended(Pre,Str)).

:- meta_predicate if_color_debug(0).
:- meta_predicate if_color_debug(0,0).
if_color_debug:-current_prolog_flag(dmsg_color,true).
if_color_debug(Call):- if_color_debug(Call, true).
if_color_debug(Call,UnColor):- if_color_debug->Call;UnColor.



% % = :- export((portray_clause_w_vars/4,ansicall/3,ansi_control_conv/2)).

:- thread_local(tlbugger:skipDumpST9/0).
:- thread_local(tlbugger:skipDMsg/0).

% @(dmsg0(succeed(S_1)),[S_1=logic])


:- thread_local(tlbugger:no_slow_io/0).
:- multifile(tlbugger:no_slow_io/0).
%:- asserta(tlbugger:no_slow_io).

dmsg(C):- notrace((tlbugger:no_slow_io,!,writeln(dmsg(C)))).
dmsg(V):- if_defined_else(dmsg0(V),ddmsg(V)).
%dmsg(F,A):- notrace((tlbugger:no_slow_io,on_x_fail(format(atom(S),F,A))->writeln(dmsg(S));writeln(dmsg_fail(F,A)))),!.
dmsg(F,A):- if_defined_else(dmsg0(F,A),ddmsg(F,A)).


dmsginfo(V):-dmsg(info(V)).
dmsg0(_,_):- is_hiding_dmsgs,!.
dmsg0(F,A):- is_sgr_on_code(F),!,dmsg(ansi(F,A)).
dmsg0(F,A):- dmsg(fmt0(F,A)).
vdmsg(L,F):-loggerReFmt(L,LR),loggerFmtReal(LR,F,[]).
dmsg(L,F,A):-loggerReFmt(L,LR),loggerFmtReal(LR,F,A).

:- thread_local(tlbugger:in_dmsg/1).
:- dynamic tlbugger:dmsg_hook/1.
:- multifile tlbugger:dmsg_hook/1.

dmsg0(V):-notrace(ignore(dmsg00(V))).
dmsg00(V):- notrace(make_key(V,K)),
   (tlbugger:in_dmsg(K)-> dmsg5(V);  % format_to_error('~N% ~q~n',[dmsg0(V)]) ;
      asserta(tlbugger:in_dmsg(K),Ref),call_cleanup(dmsg1(V),erase(Ref))).

% = :- export(dmsg1/1).
dmsg1(V):- tlbugger:is_with_dmsg(FP),!,FP=..FPL,append(FPL,[V],VVL),VV=..VVL,once(dmsg1(VV)).
dmsg1(_):- \+ always_show_dmsg, is_hiding_dmsgs,!.
dmsg1(V):- var(V),!,dmsg1(warn(dmsg_var(V))).
dmsg1(NC):- cyclic_term(NC),!,trace,format_to_error('~N% ~q~n',[dmsg_cyclic_term_1]).
dmsg1(NC):- tlbugger:skipDMsg,!,loop_check_early(dmsg2(NC),format_to_error('~N% ~q~n',[skipDMsg])).
dmsg1(V):- w_tl(tlbugger:skipDMsg,((once(dmsg2(V)), ignore((tlbugger:dmsg_hook(V),fail))))).

% = :- export(dmsg2/1).
dmsg2(NC):- cyclic_term(NC),!,format_to_error('~N% ~q~n',[dmsg_cyclic_term_2]).
dmsg2(skip_dmsg(_)):-!.
%dmsg2(C):- \+ always_show_dmsg, dmsg_hides_message(C),!.
%dmsg2(trace_or_throw(V)):- dumpST(350),dmsg(warning,V),fail.
%dmsg2(error(V)):- dumpST(250),dmsg(warning,V),fail.
%dmsg2(warn(V)):- dumpST(150),dmsg(warning,V),fail.
dmsg2(Msg):-notrace((tlbugger:no_slow_io,!,dmsg3(Msg))),!.
dmsg2(ansi(Ctrl,Msg)):- !, ansicall(Ctrl,dmsg3(Msg)).
dmsg2(color(Ctrl,Msg)):- !, ansicall(Ctrl,dmsg3(Msg)).
dmsg2(Msg):- mesg_color(Msg,Ctrl),ansicall(Ctrl,dmsg3(Msg)).

dmsg3(C):- tlbugger:no_slow_io,!,writeln(dmsg3(C)).
dmsg3(C):-
  ((functor_safe(C,Topic,_),debugging(Topic,_True_or_False),logger_property(Topic,once,true),!,
      (dmsg_log(Topic,_Time,C) -> true ; ((get_time(Time),asserta(dmsg_log(todo,Time,C)),!,dmsg4(C)))))),!.

dmsg3(C):-dmsg4(C),!.

dmsg4(_):- notrace(show_source_location),fail.
% dmsg4(C):- not(ground(C)),copy_term(C,Stuff), snumbervars(Stuff),!,dmsg5(Stuff).
dmsg4(Msg):-dmsg5(Msg).

dmsg5(Msg):- to_stderror(in_cmt(fmt9(Msg))).
dmsg5(Msg,Args):- dmsg5(fmt0(Msg,Args)).


get_indent_level(Max) :- if_prolog(swi,((prolog_current_frame(Frame),prolog_frame_attribute(Frame,level,FD)))),Depth is FD div 5,Max is min(Depth,40),!.
get_indent_level(2):-!.


/*
ansifmt(+Attributes, +Format, +Args) is det
Format text with ANSI attributes. This predicate behaves as format/2 using Format and Args, but if the current_output is a terminal, it adds ANSI escape sequences according to Attributes. For example, to print a text in bold cyan, do
?- ansifmt([bold,fg(cyan)], 'Hello ~w', [world]).
Attributes is either a single attribute or a list thereof. The attribute names are derived from the ANSI specification. See the source for sgr_code/2 for details. Some commonly used attributes are:

bold
underline
fg(Color), bg(Color), hfg(Color), hbg(Color)
Defined color constants are below. default can be used to access the default color of the terminal.

black, red, green, yellow, blue, magenta, cyan, white
ANSI sequences are sent if and only if

The current_output has the property tty(true) (see stream_property/2).
The Prolog flag color_term is true.

ansifmt(Ctrl, Format, Args) :- ansifmt(current_output, Ctrl, Format, Args).

ansifmt(Stream, Ctrl, Format, Args) :-
     % we can "assume"
        % ignore(((stream_property(Stream, tty(true)),current_prolog_flag(color_term, true)))), !,
	(   is_list(Ctrl)
	->  maplist(ansi_term:sgr_code_ex, Ctrl, Codes),
	    atomic_list_concat(Codes, (';'), OnCode)
	;   ansi_term:sgr_code_ex(Ctrl, OnCode)
	),
	'format'(string(Fmt), '\e[~~wm~w\e[0m', [Format]),
        retractall(tlbugger:last_used_color(Ctrl)),asserta(tlbugger:last_used_color(Ctrl)),
	'format'(Stream, Fmt, [OnCode|Args]),
	flush_output,!.
ansifmt(Stream, _Attr, Format, Args) :- 'format'(Stream, Format, Args).

*/

:- use_module(library(ansi_term)).

% = :- export(ansifmt/2).
ansifmt(Ctrl,Fmt):- colormsg(Ctrl,Fmt).
% = :- export(ansifmt/3).
ansifmt(Ctrl,F,A):- colormsg(Ctrl,(format(F,A))).



% = :- export(colormsg/2).



colormsg(d,Msg):- mesg_color(Msg,Ctrl),!,colormsg(Ctrl,Msg).
colormsg(Ctrl,Msg):- ansicall(Ctrl,fmt0(Msg)).

% = :- export(ansicall/2).
ansicall(Ctrl,Call):- hotrace((current_output(Out), ansicall(Out,Ctrl,Call))).

ansi_control_conv(Ctrl,CtrlO):-tlbugger:no_slow_io,!,flatten([Ctrl],CtrlO),!.
ansi_control_conv([],[]):-!.
ansi_control_conv([H|T],HT):-!,ansi_control_conv(H,HH),!,ansi_control_conv(T,TT),!,flatten([HH,TT],HT),!.
ansi_control_conv(warn,Ctrl):- !, ansi_control_conv(warning,Ctrl),!.
ansi_control_conv(Level,Ctrl):- ansi_term:level_attrs(Level,Ansi),Level\=Ansi,!,ansi_control_conv(Ansi,Ctrl).
ansi_control_conv(Color,Ctrl):- ansi_term:ansi_color(Color,_),!,ansi_control_conv(fg(Color),Ctrl).
ansi_control_conv(Ctrl,CtrlO):-flatten([Ctrl],CtrlO),!.


is_tty(Out):- not(tlbugger:no_colors), \+ tlbugger:no_slow_io, is_stream(Out),stream_property(Out,tty(true)).

ansicall(Out,_,Call):- \+ is_tty(Out),!,Call.
ansicall(_Out,_,Call):- tlbugger:skipDumpST9,!,Call.

% in_pengines:- if_defined(relative_frame(context_module,pengines,_)).

ansicall(_,_,Call):-tlbugger:no_slow_io,!,Call.
ansicall(Out,CtrlIn,Call):- once(ansi_control_conv(CtrlIn,Ctrl)),  CtrlIn\=Ctrl,!,ansicall(Out,Ctrl,Call).
ansicall(_,_,Call):- if_defined_else(in_pengines,fail),!,Call.
ansicall(Out,Ctrl,Call):-
   retractall(tlbugger:last_used_color(_)),asserta(tlbugger:last_used_color(Ctrl)),ansicall0(Out,Ctrl,Call),!.

ansicall0(Out,[Ctrl|Set],Call):-!, ansicall0(Out,Ctrl,ansicall0(Out,Set,Call)).
ansicall0(_,[],Call):-!,Call.
ansicall0(Out,Ctrl,Call):-if_color_debug(ansicall1(Out,Ctrl,Call),keep_line_pos(Out, Call)).

ansicall1(Out,Ctrl,Call):-
   must(sgr_code_on_off(Ctrl, OnCode, OffCode)),!,
     keep_line_pos(Out, (format(Out, '\e[~wm', [OnCode]))),
	call_cleanup(Call,
           keep_line_pos(Out, (format(Out, '\e[~wm', [OffCode])))).
/*
ansicall(S,Set,Call):-
     call_cleanup((
         stream_property(S, tty(true)), current_prolog_flag(color_term, true), !,
	(is_list(Ctrl) ->  maplist(sgr_code_on_off, Ctrl, Codes, OffCodes),
          atomic_list_concat(Codes, (';'), OnCode) atomic_list_concat(OffCodes, (';'), OffCode) ;   sgr_code_on_off(Ctrl, OnCode, OffCode)),
        keep_line_pos(S, (format(S,'\e[~wm', [OnCode])))),
	call_cleanup(Call,keep_line_pos(S, (format(S, '\e[~wm', [OffCode]))))).


*/




keep_line_pos(S, G) :-
       (stream_property(S, position(Pos)) ->
	(stream_position_data(line_position, Pos, LPos),
        call_cleanup(G, set_stream(S, line_position(LPos)))) ; G).


:- multifile(tlbugger:term_color0/2).
:- dynamic(tlbugger:term_color0/2).


%tlbugger:term_color0(retract,magenta).
%tlbugger:term_color0(retractall,magenta).
tlbugger:term_color0(assertz,hfg(green)).
tlbugger:term_color0(ainz,hfg(green)).
tlbugger:term_color0(aina,hfg(green)).
tlbugger:term_color0(mpred_op,hfg(blue)).

mesg_color(_,[reset]):-tlbugger:no_slow_io,!.
mesg_color(T,C):-var(T),!,nop(dumpST),!,C=[blink(slow),fg(red),hbg(black)],!.
mesg_color(T,C):-is_sgr_on_code(T),!,C=T.
mesg_color(T,C):-cyclic_term(T),!,C=reset.
mesg_color("",C):- !,C=[blink(slow),fg(red),hbg(black)],!.
mesg_color(T,C):- string(T),!,must(f_word(T,F)),!,functor_color(F,C).
mesg_color([_,_,_T|_],C):-atom(T),mesg_color(T,C).
mesg_color([T|_],C):-atom(T),mesg_color(T,C).
mesg_color(T,C):-(atomic(T);is_list(T)), dmsg_text_to_string_safe(T,S),!,mesg_color(S,C).
mesg_color(T,C):-not(compound(T)),term_to_atom(T,A),!,mesg_color(A,C).
mesg_color(succeed(T),C):-nonvar(T),mesg_color(T,C).
mesg_color((T),C):- \+ \+ ((predicate_property(T,meta_predicate(_)))),arg(_,T,E),compound(E),!,mesg_color(E,C).
mesg_color(=(T,_),C):-nonvar(T),mesg_color(T,C).
mesg_color(debug(T),C):-nonvar(T),mesg_color(T,C).
mesg_color(_:T,C):-nonvar(T),!,mesg_color(T,C).
mesg_color(T,C):-functor_safe(T,F,_),member(F,[color,ansi]),compound(T),arg(1,T,C),nonvar(C).
mesg_color(T,C):-functor_safe(T,F,_),member(F,[succeed,must,mpred_op_prolog]),compound(T),arg(1,T,E),nonvar(E),!,mesg_color(E,C).
mesg_color(T,C):-functor_safe(T,F,_),member(F,[fmt0,msg]),compound(T),arg(2,T,E),nonvar(E),!,mesg_color(E,C).
mesg_color(T,C):-predef_functor_color(F,C),mesg_arg1(T,F).
mesg_color(T,C):-nonvar(T),defined_message_color(F,C),matches_term(F,T),!.
mesg_color(T,C):-functor_h0(T,F,_),!,functor_color(F,C),!.


f_word("",""):-!.
f_word(T,A):-concat_atom(List,' ',T),member(A,List),atom_length(A,L),L>0,!.
f_word(T,A):-concat_atom(List,'_',T),member(A,List),atom_length(A,L),L>0,!.
f_word(T,A):- string_to_atom(T,P),sub_atom(P,0,10,_,A),A\==P,!.
f_word(T,A):- string_to_atom(T,A),!.

mesg_arg1(T,_TT):-var(T),!,fail.
mesg_arg1(_:T,C):-nonvar(T),!,mesg_arg1(T,C).
mesg_arg1(T,TT):-not(compound(T)),!,T=TT.
mesg_arg1(T,F):-functor_h0(T,F,_).
mesg_arg1(T,C):-compound(T),arg(1,T,F),!,nonvar(F),mesg_arg1(F,C).


% = :- export(defined_message_color/2).
:- dynamic(defined_message_color/2).

defined_message_color(todo,[fg(red),bg(black),underline]).
%defined_message_color(error,[fg(red),hbg(black),bold]).
defined_message_color(warn,[fg(black),hbg(red),bold]).
defined_message_color(A,B):-tlbugger:term_color0(A,B).


predef_functor_color(F,C):- defined_message_color(F,C),!.
predef_functor_color(F,C):- defined_message_color(F/_,C),!.
predef_functor_color(F,C):- tlbugger:term_color0(F,C),!.

functor_color(F,C):- predef_functor_color(F,C),!.
functor_color(F,C):- next_color(C),ignore(on_x_fail(assertz(tlbugger:term_color0(F,C)))),!.


:- thread_local(tlbugger:last_used_color/1).

% tlbugger:last_used_color(pink).

last_used_fg_color(LFG):-tlbugger:last_used_color(LU),fg_color(LU,LFG),!.
last_used_fg_color(default).

good_next_color(C):-var(C),!,trace_or_throw(var_good_next_color(C)),!.
good_next_color(C):- last_used_fg_color(LFG),fg_color(C,FG),FG\=LFG,!.
good_next_color(C):- not(unliked_ctrl(C)).

unliked_ctrl(fg(blue)).
unliked_ctrl(fg(black)).
unliked_ctrl(fg(red)).
unliked_ctrl(bg(white)).
unliked_ctrl(hbg(white)).
unliked_ctrl(X):-is_list(X),member(E,X),nonvar(E),unliked_ctrl(E).

fg_color(LU,FG):-member(fg(FG),LU),FG\=white,!.
fg_color(LU,FG):-member(hfg(FG),LU),FG\=white,!.
fg_color(_,default).

% = :- export(random_color/1).
random_color([reset,M,FG,BG,font(Font)]):-Font is random(8),
  findall(Cr,ansi_term:ansi_color(Cr, _),L),
  random_member(E,L),
  random_member(FG,[hfg(E),fg(E)]),not(unliked_ctrl(FG)),
  contrasting_color(FG,BG), not(unliked_ctrl(BG)),
  random_member(M,[bold,faint,reset,bold,faint,reset,bold,faint,reset]),!. % underline,negative


% = :- export(tst_color/0).
tst_color:- make, ignore((( between(1,20,_),random_member(Call,[colormsg(C,cm(C)),dmsg(color(C,dm(C))),ansifmt(C,C)]),next_color(C),Call,fail))).
% = :- export(tst_color/1).
tst_color(C):- make,colormsg(C,C).

% = :- export(next_color/1).
next_color(C):- between(1,10,_), random_color(C), good_next_color(C),!.
next_color([underline|C]):- random_color(C),!.

% = :- export(contrasting_color/2).
contrasting_color(white,black).
contrasting_color(A,default):-atom(A),A \= black.
contrasting_color(fg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(hfg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(black,white).
contrasting_color(default,default).
contrasting_color(_,default).

:- thread_local(ansi_prop/2).

sgr_on_code(Ctrl,OnCode):-sgr_on_code0(Ctrl,OnCode),!.
sgr_on_code(Foo,7):- notrace((format_to_error('~NMISSING: ~q~n',[sgr_on_code(Foo,7)]))),!. % ,dtrace(sgr_on_code(Foo,7)))).

is_sgr_on_code(Ctrl):-sgr_on_code0(Ctrl,_),!.

sgr_on_code0(Ctrl,OnCode):- ansi_term:sgr_code(Ctrl,OnCode).
sgr_on_code0(blink, 6).
sgr_on_code0(-Ctrl,OffCode):-  nonvar(Ctrl), sgr_off_code(Ctrl,OffCode).

sgr_off_code(Ctrl,OnCode):-ansi_term:off_code(Ctrl,OnCode),!.
sgr_off_code(- Ctrl,OnCode):- nonvar(Ctrl), sgr_on_code(Ctrl,OnCode),!.
sgr_off_code(fg(_), CurFG):- (ansi_prop(fg,CurFG)->true;CurFG=39),!.
sgr_off_code(bg(_), CurBG):- (ansi_prop(ng,CurBG)->true;CurBG=49),!.
sgr_off_code(bold, 21).
sgr_off_code(italic_and_franktur, 23).
sgr_off_code(franktur, 23).
sgr_off_code(italic, 23).
sgr_off_code(underline, 24).
sgr_off_code(blink, 25).
sgr_off_code(blink(_), 25).
sgr_off_code(negative, 27).
sgr_off_code(conceal, 28).
sgr_off_code(crossed_out, 29).
sgr_off_code(framed, 54).
sgr_off_code(overlined, 55).
sgr_off_code(_,0).


sgr_code_on_off(Ctrl,OnCode,OffCode):-sgr_on_code(Ctrl,OnCode),sgr_off_code(Ctrl,OffCode),!.
sgr_code_on_off(Ctrl,OnCode,OffCode):-sgr_on_code(Ctrl,OnCode),sgr_off_code(Ctrl,OffCode),!.


msg_to_string(Var,Str):-var(Var),!,sformat(Str,'~q',[Var]),!.
msg_to_string(portray(Msg),Str):- with_output_to(string(Str),portray_clause_w_vars(user_output,Msg,[],[])),!.
msg_to_string(pp(Msg),Str):- sformat(Str,Msg,[],[]),!.
msg_to_string(fmt(F,A),Str):-sformat(Str,F,A),!.
msg_to_string(format(F,A),Str):-sformat(Str,F,A),!.
msg_to_string(Msg,Str):-atomic(Msg),!,sformat(Str,'~w',[Msg]).
msg_to_string(m2s(Msg),Str):-message_to_string(Msg,Str),!.
msg_to_string(Msg,Str):-sformat(Str,Msg,[],[]),!.











% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% dumpstack_arguments.
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================

dumpST:- is_hiding_dmsgs,!.
dumpST:- tlbugger:no_slow_io,!,dumpST0,!.
dumpST:- loop_check_early(dumpST99,dumpST0).
% dumpST:- ignore(catch(( w_tl(tlbugger:skipDumpST9,dumpST9)),E,writeq(E))),!.
 
dumpST0:-loop_check(logicmoo_util_bugger_catch:dumpST00,true),!.
dumpST00:-dumpST00(800),!.
dumpST00(MaxDepth):-
 notrace((catch(( 
  thread_current_error_stream(ERR),
   integer(MaxDepth),Term = dumpST(MaxDepth),format_to_error( '% dumpST ~p~n', [Term]),
    prolog_current_frame(Frame),ignore(( get_prolog_backtrace(MaxDepth, Trace,[frame(Frame),goal_depth(800),subgoal_positions(true)]),    
    print_prolog_backtrace(ERR, Trace,[subgoal_positions(true)]), nl(ERR), fail))),
   E,writeq(E)))),!.

:- set_prolog_flag(backtrace_depth,      200).
:- set_prolog_flag(backtrace_goal_depth, 20).
:- set_prolog_flag(backtrace_show_lines, true).

dumpST9:- loop_check_early(dumpST90,dumpST0).
dumpST90:- prolog_current_frame(Frame),dumpST2_broke(Frame,5000),!.

dumpST(_):- is_hiding_dmsgs,!.
dumpST(Num):-integer(Num),prolog_current_frame(Frame),dumpST2_broke(Frame,Num),!.
dumpST(Opts):- dumpST(_,Opts),!.

dumpST(Frame,Opts):-var(Opts),!,dumpST(Frame,5000).
dumpST(Frame,MaxDepth):-
   thread_current_error_stream(ERR),
   integer(MaxDepth),Term = dumpST(MaxDepth),
   (var(Frame)->prolog_current_frame(Frame);true),
   ignore(( get_prolog_backtrace(MaxDepth, Trace,[frame(Frame),goal_depth(100)]),
    format_to_error( '% dumpST ~p ~n', [Term]),
    print_prolog_backtrace(ERR, Trace,[subgoal_positions(true)]), nl(ERR), fail)),!.
dumpST(Frame,MaxDepth):- integer(MaxDepth),(var(Frame)->prolog_current_frame(Frame);true),dumpST2_broke(Frame,MaxDepth).
dumpST(Frame,Opts):-is_list(Opts),!,dumpST(1,Frame,Opts).
dumpST(Frame,Opts):-show_call(dumpST(1,Frame,[Opts])).


dumpST2_broke(Frame,From-MaxDepth):-integer(MaxDepth),!,dumpST(Frame,[skip_depth(From),max_depth(MaxDepth),numbervars(safe),show([has_alternatives,level,context_module,goal,clause])]),!.
dumpST2_broke(Frame,MaxDepth):-integer(MaxDepth),!,dumpST(Frame,[max_depth(MaxDepth),numbervars(safe),show([has_alternatives,level,context_module,goal,clause])]),!.
dumpST2_broke(_,_):- ddmsg(failure(dumpST2_broke/2)),!.

get_m_opt(Opts,Max_depth,D100,RetVal):-E=..[Max_depth,V],(((member(E,Opts),nonvar(V)))->RetVal=V;RetVal=D100).

:- meta_predicate no_trace_dump(0).

no_trace_dump(G):-notrace(G).
dumpST(N,Frame,Opts):-
  ignore(prolog_current_frame(Frame)),
  no_trace_dump(( dumpST4(N,Frame,Opts,Out))),
   no_trace_dump((get_m_opt(Opts,numbervars,88,Start),
   neg1_numbervars(Out,Start,ROut),
   reverse(ROut,RROut),
   ignore((forall(member(E,RROut),fdmsg(E)))))),!.
dumpST(_N,_Frame,_Opts):- dmsg(failed(dumpST/3)),!,notrace(dumpST0),!.


neg1_numbervars(Out,_,Out):-!.
neg1_numbervars(T,-1,T):-!.
neg1_numbervars(Out,Start,ROut):-copy_term(Out,ROut),integer(Start),snumbervars(ROut,Start,_),!.
neg1_numbervars(Out,safe,ROut):-copy_term(Out,ROut),snumbervars(ROut),!.


fdmsg1(txt(S)):-format_to_error(S,[]),!.
fdmsg1(level=L):-format_to_error('~n(~q)',[L]),!.
fdmsg1(context_module=G):-!,format_to_error('[~w] ',[G]),!.
fdmsg1(has_alternatives=G):- (G==false->true;format_to_error('~N*',[G])),!.
fdmsg1(goal=G):-mesg_color(G,Ctrl),simplify_goal_printed(G,GG),!,ansicall(Ctrl,format_to_error(' ~q. ',[GG])),!.
fdmsg1(clause=[F,L]):- directory_file_path(_,FF,F),format_to_error('  %  ~w:~w: ',[FF,L]),!.
fdmsg1(clause=[F,L]):- fresh_line_to_err,format_to_error('%  ~w:~w: ',[F,L]),!.
fdmsg1(clause=[]):-format_to_error(' /*DYN*/ ',[]),!.
fdmsg1(G):-mesg_color(G,Ctrl),ansicall(Ctrl,format_to_error(' ~q ',[G])),!.
fdmsg1(_M):-dmsg(failed_fdmsg1). % (M)).

fdmsg(fr(List)):-is_list(List),!,fresh_line_to_err,ignore(forall(member(E,List),fdmsg1(E))).
fdmsg(M):-dmsg(M).

simplify_goal_printed(O,O):-!.
simplify_goal_printed(G,O):- must(transitive(simplify_goal_printed0,G,O)),!.


% = :- export(simplify_goal_printed0/2).
simplify_goal_printed0(Var,Var):- \+ compound(Var),!.
simplify_goal_printed0(lmconf:G,G).
simplify_goal_printed0(system:G,G).
simplify_goal_printed0(catchvv(G,_,_),G).
simplify_goal_printed0(call(G),G).
simplify_goal_printed0(G,O):- G=..[F|A],must_maplist(simplify_goal_printed,A,AA),!,A\==AA,O=..[F|AA].
simplify_goal_printed0(G,O):- G=..[F|A],must_maplist(simplify_goal_printed,A,AA),!,A\==AA,O=..[F|AA].

dumpST4(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,max_depth,100,MD),N>=MD,!.
%dumpST4(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,skip_depth,100,SD),N=<SD,!,fail.
dumpST4(N,Frame,Opts,[fr(Goal)|MORE]):- get_m_opt(Opts,show,goal,Ctrl),getPFA(Frame,Ctrl,Goal),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST4(N,Frame,Opts,[nf(no(Ctrl),N,Frame,Opts)|MORE]):- get_m_opt(Opts,show,goal,Ctrl),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST4(N,Frame,Opts,[nf(noFrame(N,Frame,Opts))]).

dumpST_Parent(N,Frame,Opts,More):- prolog_frame_attribute(Frame,parent,ParentFrame), NN is N +1,dumpST4(NN,ParentFrame,Opts,More),!.
dumpST_Parent(N,Frame,Opts,[nf(noParent(N,Frame,Opts))]).



getPFA(Frame,[L|List],Goal):- !,findall(R, (member(A,[L|List]),getPFA1(Frame,A,R)) ,Goal).
getPFA(Frame,Ctrl,Goal):-getPFA1(Frame,Ctrl,Goal).

getPFA1(_Frame,txt(Txt),txt(Txt)):-!.
getPFA1(Frame,clause,Goal):-getPFA2(Frame,clause,ClRef),clauseST(ClRef,Goal),!.
getPFA1(Frame,Ctrl,Ctrl=Goal):-getPFA2(Frame,Ctrl,Goal),!.
getPFA1(_,Ctrl,no(Ctrl)).

getPFA2(Frame,Ctrl,Goal):- catchvv((prolog_frame_attribute(Frame,Ctrl,Goal)),E,Goal=[error(Ctrl,E)]),!.

clauseST(ClRef,clause=Goal):- findall(V,(member(Prop,[file(V),line_count(V)]),clause_property(ClRef,Prop)),Goal).

clauseST(ClRef,Goal = HB):- ignore(((clause(Head, Body, ClRef),copy_term(((Head :- Body)),HB)))),
   snumbervars(HB,0,_),
   findall(Prop,(member(Prop,[source(_),line_count(_),file(_),fact,erased]),clause_property(ClRef,Prop)),Goal).

:- dynamic(formatter_hook/4).


withFormatter(Lang,From,Vars,SForm):-formatter_hook(Lang,From,Vars,SForm),!.
withFormatter(_Lang,From,_Vars,SForm):-sformat(SForm,'~w',[From]).

flush_output_safe:-ignore(catchvv(flush_output,_,true)).
flush_output_safe(X):-ignore(catchvv(flush_output(X),_,true)).

writeFailureLog(E,X):-
  thread_current_error_stream(ERR),
		(fmt(ERR,'\n% error: ~q ~q\n',[E,X]),flush_output_safe(ERR),!,
		%,true.
		fmt('\n% error: ~q ~q\n',[E,X]),!,flush_output).

%unknown(Old, autoload).

cls:- shell(cls).

:- 'mpred_trace_none'(fmt(_)).
:- 'mpred_trace_none'(fmt(_,_)).
:- 'mpred_trace_none'(dfmt(_)).
:- 'mpred_trace_none'(dfmt(_,_)).
:- 'mpred_trace_none'(dmsg(_)).
:- 'mpred_trace_none'(dmsg(_,_)).


 



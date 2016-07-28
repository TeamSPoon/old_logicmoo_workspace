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
            always_show_dmsg/0,
            with_output_to_each/2,
            ansicall/2,
            ansicall/3,
            ansicall0/3,
            ansicall1/3,
            ansifmt/2,
            ansifmt/3,
          is_hiding_dmsgs/0,
            colormsg/2,
            contains_atom/2,
            contrasting_color/2,
            defined_message_color/2,

            format_to_message/3, 
            dfmt/1,dfmt/2,
            debugm/1,debugm/2,
            dmsg/1,dmsg/2,dmsg/3,

          setLogLevel/2,
          logLevel/2,
               loggerFmtReal/3,
               loggerReFmt/2,
               logger_property/3,

            cls/0,
            dmsg0/1,dmsg0/2,dmsg00/1,
            dmsg1/1,
            dmsg2/1,
            dmsg3/1,
            dmsg4/1,
            dmsg5/1,dmsg5/2,
            dmsg_hide/1,
            dmsg_hides_message/1,
            dmsg_show/1,
            dmsg_showall/1,
            dmsg_text_to_string_safe/2,
            dmsginfo/1,
            f_word/2,
            fg_color/2,
            flush_output_safe/0,
            flush_output_safe/1,
            fmt/1,fmt/2,fmt/3,
            fmt0/1,fmt0/2,fmt0/3,
            fmt9/1,
            fmt_ansi/1,
            fmt_or_pp/1,
            fmt_portray_clause/1,
            functor_color/2,
          get_indent_level/1,
            good_next_color/1,
            if_color_debug/0,
            if_color_debug/1,
            if_color_debug/2,
            in_cmt/1,
            indent_e/1,
            indent_to_spaces/2,
            is_sgr_on_code/1,
            is_tty/1,
            keep_line_pos_w_w/2,
            last_used_fg_color/1,
          matches_term/2,
          matches_term0/2,
            mesg_arg1/2,
            mesg_color/2,
            msg_to_string/2,
            next_color/1,
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


source_variables_lwv/1,
term_color0/2,
term_to_message_string/2,
ansi_prop/2,
dmsg_log/3,
dmsg000/1,

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
        keep_line_pos_w_w(?, 0),        
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

        with_output_to_each/2,
        f_word/2,
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
        get_indent_level/1,
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


:- if(current_predicate(lmcode:combine_logicmoo_utils/0)).
:- module(logicmoo_util_dmsg,
[  % when the predciates are not being moved from file to file the exports will be moved here
       ]).

:- else.
:- include('logicmoo_util_header.pi').
:- endif.

:- use_module(system:library(memfile)).
:- system:use_module(logicmoo_util_first).
:- system:use_module(logicmoo_util_with_assertions).
:- system:use_module(logicmoo_util_loop_check).



:- meta_predicate with_output_to_each(+,0).

with_output_to_each(Output,Goal):- Output= atom(A),!,
   current_output(Was),
   nb_setarg(1,Output,""),
   new_memory_file(Handle),
   open_memory_file(Handle,write,Stream,[free_on_close(true)]),
     scce_orig(set_output(Stream),
      setup_call_cleanup(true,Goal,
        (close(Stream),memory_file_to_atom(Handle,Atom),nb_setarg(1,Output,Atom),ignore(A=Atom))),
      (set_output(Was))).

with_output_to_each(Output,Goal):- Output= string(A),!,
   current_output(Was),
   nb_setarg(1,Output,""),
   new_memory_file(Handle),
   open_memory_file(Handle,write,Stream,[free_on_close(true)]),
     scce_orig(set_output(Stream),
      setup_call_cleanup(true,Goal,
        (close(Stream),memory_file_to_string(Handle,Atom),nb_setarg(1,Output,Atom),ignore(A=Atom))),
      (set_output(Was))).

with_output_to_each(Output,Goal):- 
   current_output(Was), scce_orig(set_output(Output),Goal,set_output(Was)).
    

% ==========================================================
% Sending Notes
% ==========================================================
:- thread_local( tlbugger:tlbugger:dmsg_match/2).
% = :- meta_predicate(with_all_dmsg(0)).
% = :- meta_predicate(with_show_dmsg(*,0)).



%= 	 	 

%% with_all_dmsg( :GoalCall) is semidet.
%
% Using All (debug)message.
%
with_all_dmsg(Call):-
   w_tl(tlbugger:tl_always_show_dmsg,
     w_tl(set_prolog_flag(opt_debug,true),
       w_tl( tlbugger:dmsg_match(show,_),Call))).



%= 	 	 

%% with_show_dmsg( ?TypeShown, :GoalCall) is semidet.
%
% Using Show (debug)message.
%
with_show_dmsg(TypeShown,Call):-
  w_tl(set_prolog_flag(opt_debug,filter),
     w_tl( tlbugger:dmsg_match(showing,TypeShown),Call)).

% = :- meta_predicate(with_no_dmsg(0)).

%= 	 	 

%% with_no_dmsg( :GoalCall) is semidet.
%
% Using No (debug)message.
%
with_no_dmsg(Call):- always_show_dmsg,!,Call.
with_no_dmsg(Call):-w_tl(set_prolog_flag(opt_debug,false),Call).

%= 	 	 

%% with_no_dmsg( ?TypeUnShown, :GoalCall) is semidet.
%
% Using No (debug)message.
%
with_no_dmsg(TypeUnShown,Call):-w_tl(set_prolog_flag(opt_debug,filter),
  w_tl( tlbugger:dmsg_match(hidden,TypeUnShown),Call)).

% dmsg_hides_message(_):- !,fail.

%= 	 	 

%% dmsg_hides_message( ?C) is semidet.
%
% (debug)message Hides Message.
%
dmsg_hides_message(_):- current_prolog_flag(opt_debug,false),!.
dmsg_hides_message(_):- current_prolog_flag(opt_debug,true),!,fail.
dmsg_hides_message(C):-  tlbugger:dmsg_match(HideShow,Matcher),matches_term(Matcher,C),!,HideShow=hidden.


:- export(matches_term/2).

%% matches_term( ?Filter, ?VALUE2) is semidet.
%
% Matches Term.
%
matches_term(Filter,_):- var(Filter),!.
matches_term(Filter,Term):- var(Term),!,Filter=var.
matches_term(Filter,Term):- ( \+ \+ (matches_term0(Filter,Term))),!.

%% contains_atom( ?V, ?A) is semidet.
%
% Contains Atom.
%
contains_atom(V,A):-sub_term(VV,V),nonvar(VV),functor_safe(VV,A,_).

%% matches_term0( :TermFilter, ?Term) is semidet.
%
% Matches Term Primary Helper.
%
matches_term0(Filter,Term):- Term = Filter.
matches_term0(Filter,Term):- atomic(Filter),!,contains_atom(Term,Filter).
matches_term0(F/A,Term):- (var(A)->member(A,[0,1,2,3,4]);true), functor_safe(Filter,F,A), matches_term0(Filter,Term).
matches_term0(Filter,Term):- sub_term(STerm,Term),nonvar(STerm),matches_term0(Filter,STerm),!.


%= 	 	 

%% dmsg_hide( ?Term) is semidet.
%
% (debug)message Hide.
%
dmsg_hide(isValueMissing):-!,set_prolog_flag(opt_debug,false).
dmsg_hide(Term):-set_prolog_flag(opt_debug,filter),sanity(nonvar(Term)),aina( tlbugger:dmsg_match(hidden,Term)),retractall( tlbugger:dmsg_match(showing,Term)),nodebug(Term).

%= 	 	 

%% dmsg_show( ?Term) is semidet.
%
% (debug)message Show.
%
dmsg_show(isValueMissing):-!,set_prolog_flag(opt_debug,true).
dmsg_show(Term):-set_prolog_flag(opt_debug,filter),aina( tlbugger:dmsg_match(showing,Term)),ignore(retractall( tlbugger:dmsg_match(hidden,Term))),debug(Term).

%= 	 	 

%% dmsg_showall( ?Term) is semidet.
%
% (debug)message Showall.
%
dmsg_showall(Term):-ignore(retractall( tlbugger:dmsg_match(hidden,Term))).


%= 	 	 

%% indent_e( ?X) is semidet.
%
% Indent E.
%
indent_e(0):-!.
indent_e(X):- X > 20, XX is X-20,!,indent_e(XX).
indent_e(X):- catchvvnt((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).


%= 	 	 

%% dmsg_text_to_string_safe( ?Expr, ?Forms) is semidet.
%
% (debug)message Text Converted To String Safely Paying Attention To Corner Cases.
%
dmsg_text_to_string_safe(Expr,Forms):-on_x_fail(text_to_string(Expr,Forms)).

% ===================================================================
% Lowlevel printng
% ===================================================================
:- multifile term_to_message_string/2.
:- dynamic term_to_message_string/2.


%= 	 	 

%% fmt0( ?X, ?Y, ?Z) is semidet.
%
% Format Primary Helper.
%
%fmt0(user_error,F,A):-!,get_main_error_stream(Err),!,format(Err,F,A).
%fmt0(current_error,F,A):-!,get_thread_current_error(Err),!,format(Err,F,A).
fmt0(X,Y,Z):-catchvvnt((format(X,Y,Z),flush_output_safe(X)),E,dfmt(E:format(X,Y))).

%= 	 	 

%% fmt0( ?X, ?Y) is semidet.
%
% Format Primary Helper.
%
fmt0(X,Y):-catchvvnt((format(X,Y),flush_output_safe),E,dfmt(E:format(X,Y))).

%= 	 	 

%% fmt0( ?X) is semidet.
%
% Format Primary Helper.
%
fmt0(X):- (atomic(X);is_list(X)), dmsg_text_to_string_safe(X,S),!,format('~w',[S]),!.
fmt0(X):- (atom(X) -> catchvvnt((format(X,[]),flush_output_safe),E,dmsg(E)) ; (term_to_message_string(X,M) -> 'format'('~q~N',[M]);fmt_or_pp(X))).

%= 	 	 

%% fmt( ?X) is semidet.
%
% Format.
%
fmt(X):-fresh_line,fmt_ansi(fmt0(X)).

%= 	 	 

%% fmt( ?X, ?Y) is semidet.
%
% Format.
%
fmt(X,Y):- fresh_line,fmt_ansi(fmt0(X,Y)),!.

%= 	 	 

%% fmt( ?X, ?Y, ?Z) is semidet.
%
% Format.
%
fmt(X,Y,Z):- fmt_ansi(fmt0(X,Y,Z)),!.



:- module_transparent((format_to_message)/3).

format_to_message(Format,Args,Info):- 
  icatch(((( sanity(is_list(Args))-> 
     format(string(Info),Format,Args);
     (format(string(Info),'~N~n~p +++++++++++++++++ ~p~n',[Format,Args])))))).

%= 	 	 

%% fmt9( ?Msg) is semidet.
%
% Fmt9.
%
fmt9(fmt0(F,A)):-on_x_fail(fmt0(F,A)),!.
fmt9(Msg):- catch(((string(Msg);atom(Msg)),format(Msg,[x1,x2,x3])),_,fail),!.
fmt9(Msg):- if_defined(portray_clause_w_vars(Msg),print(Msg)).

% :-reexport(library(ansi_term)).
:- use_module(library(ansi_term)).


%= 	 	 

%% tst_fmt is semidet.
%
% Tst Format.
%
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



%= 	 	 

%% fmt_ansi( :GoalCall) is semidet.
%
% Format Ansi.
%
fmt_ansi(Call):-ansicall([reset,bold,hfg(white),bg(black)],Call).


%= 	 	 

%% fmt_portray_clause( ?X) is semidet.
%
% Format Portray Clause.
%
fmt_portray_clause(X):- renumbervars_prev(X,Y),!, portray_clause(Y).


%= 	 	 

%% fmt_or_pp( ?X) is semidet.
%
% Format Or Pretty Print.
%
fmt_or_pp(portray((X:-Y))):-!,fmt_portray_clause((X:-Y)),!.
fmt_or_pp(portray(X)):-!,functor_safe(X,F,A),fmt_portray_clause((pp(F,A):-X)),!.
fmt_or_pp(X):-format('~q~N',[X]).


%= 	 	 

%% with_output_to_console( :GoalX) is semidet.
%
% Using Output Converted To Console.
%
with_output_to_console(X):- get_main_error_stream(Err),!,with_output_to_stream(Err,X).

%= 	 	 

%% with_output_to_main( :GoalX) is semidet.
%
% Using Output Converted To Main.
%
with_output_to_main(X):- get_main_error_stream(Err),!,with_output_to_stream(Err,X).


%= 	 	 

%% dfmt( ?X) is semidet.
%
% Dfmt.
%
dfmt(X):- get_thread_current_error(Err),!,with_output_to_stream(Err,fmt(X)).

%= 	 	 

%% dfmt( ?X, ?Y) is semidet.
%
% Dfmt.
%
dfmt(X,Y):- get_thread_current_error(Err), with_output_to_stream(Err,fmt(X,Y)).


%= 	 	 

%% with_output_to_stream( ?Stream, :GoalGoal) is semidet.
%
% Using Output Converted To Stream.
%
with_output_to_stream(Stream,Goal):-
   current_output(Saved),
   scce_orig(set_output(Stream),
         Goal,
         set_output(Saved)).


%= 	 	 

%% to_stderror( :GoalCall) is semidet.
%
% Converted To Stderror.
%
to_stderror(Call):- get_thread_current_error(Err), with_output_to_stream(Err,Call).



:- dynamic dmsg_log/3.


:- dynamic(logLevel/2).
:- module_transparent(logLevel/2).
:- multifile(logLevel/2).


:- dynamic logger_property/2.

%= 	 	 

%% logger_property( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Logger Property.
%
logger_property(todo,once,true).



%= 	 	 

%% setLogLevel( ?M, ?L) is semidet.
%
% Set Log Level.
%
setLogLevel(M,L):-retractall(logLevel(M,_)),(nonvar(L)->asserta(logLevel(M,L));true).


%= 	 	 

%% logLevel( ?S, ?Z) is semidet.
%
% Log Level.
%
logLevel(debug,ERR):-get_thread_current_error(ERR).
logLevel(error,ERR):-get_thread_current_error(ERR).
logLevel(private,none).
logLevel(S,Z):-current_stream(_X,write,Z),dtrace,stream_property(Z,alias(S)).


%= 	 	 

%% loggerReFmt( ?L, ?LRR) is semidet.
%
% Logger Re Format.
%
loggerReFmt(L,LRR):-logLevel(L,LR),L \==LR,!,loggerReFmt(LR,LRR),!.
loggerReFmt(L,L).


%= 	 	 

%% loggerFmtReal( ?S, ?F, ?A) is semidet.
%
% Logger Format Real.
%
loggerFmtReal(none,_F,_A):-!.
loggerFmtReal(S,F,A):-
  current_stream(_,write,S),
    fmt(S,F,A),
    flush_output_safe(S),!.



:- thread_local tlbugger:is_with_dmsg/1.


%= 	 	 

%% with_dmsg( ?Functor, :GoalGoal) is semidet.
%
% Using (debug)message.
%
with_dmsg(Functor,Goal):-
   w_tl(t_l:is_with_dmsg(Functor),Goal).


:- use_module(library(listing)).

%= 	 	 

%% sformat( ?Str, ?Msg, ?Vs, ?Opts) is semidet.
%
% Sformat.
%
sformat(Str,Msg,Vs,Opts):- nonvar(Msg),functor_safe(Msg,':-',_),!,with_output_to_each(string(Str),
   (current_output(CO),portray_clause_w_vars(CO,Msg,Vs,Opts))).
sformat(Str,Msg,Vs,Opts):- with_output_to_each(chars(Codes),(current_output(CO),portray_clause_w_vars(CO,':-'(Msg),Vs,Opts))),append([_,_,_],PrintCodes,Codes),'sformat'(Str,'   ~s',[PrintCodes]),!.



%= 	 	 

%% portray_clause_w_vars( ?Out, ?Msg, ?Vs, ?Options) is semidet.
%
% Portray Clause W Variables.
%
portray_clause_w_vars(Out,Msg,Vs,Options):- \+ \+ ((prolog_listing:do_portray_clause(Out,Msg,[variable_names(Vs),numbervars(true),attributes(portray),character_escapes(true),quoted(true)|Options]))),!.

%= 	 	 

%% portray_clause_w_vars( ?Msg, ?Vs, ?Options) is semidet.
%
% Portray Clause W Variables.
%
portray_clause_w_vars(Msg,Vs,Options):- portray_clause_w_vars(current_output,Msg,Vs,Options).

%= 	 	 

%% portray_clause_w_vars( ?Msg, ?Options) is semidet.
%
% Portray Clause W Variables.
%
portray_clause_w_vars(Msg,Options):- source_variables_lwv(Vs),portray_clause_w_vars(current_output,Msg,Vs,Options).



%= 	 	 

%% source_variables_lwv( ?AllS) is semidet.
%
% Source Variables Lwv.
%
source_variables_lwv(AllS):-
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (get_varname_list(Vs2);Vs2=[]),
  %notrace(catch((parent_goal('$toplevel':'$execute_goal2'(_, Vs3),_);Vs3=[]),E,(writeq(E),Vs3=[]))),
  ignore(Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  set_varname_list( AllS).



:- export(portray_clause_w_vars/1).

%= 	 	 

%% portray_clause_w_vars( ?Msg) is semidet.
%
% Portray Clause W Variables.
%
portray_clause_w_vars(Msg):- portray_clause_w_vars(Msg,[]),!.


%= 	 	 

%% print_prepended( ?Pre, ?S) is semidet.
%
% Print Prepended.
%
print_prepended(Pre,S):-atom_concat(L,' ',S),!,print_prepended(Pre,L).
print_prepended(Pre,S):-atom_concat(L,'\n',S),!,print_prepended(Pre,L).
print_prepended(Pre,S):-atom_concat('\n',L,S),!,print_prepended(Pre,L).
print_prepended(Pre,S):-atomics_to_string(L,'\n',S),print_prepended_lines(Pre,L).

%= 	 	 

%% print_prepended_lines( ?Pre, :TermARG2) is semidet.
%
% Print Prepended Lines.
%
print_prepended_lines(_Pre,[]):- format('~N',[]).
print_prepended_lines(Pre,[H|T]):-format('~N~w~w',[Pre,H]),print_prepended_lines(Pre,T).



%= 	 	 

%% in_cmt( :GoalCall) is semidet.
%
% In Comment.
%

% in_cmt(Call):- tlbugger:no_slow_io,!,format('~N/*~n',[]),call_cleanup(Call,format('~N*/~n',[])).
in_cmt(Call):- call_cleanup(prepend_each_line('% ',Call),format('~N',[])).


%= 	 	 

%% with_current_indent( :GoalCall) is semidet.
%
% Using Current Indent.
%
with_current_indent(Call):- 
   get_indent_level(Indent), 
   indent_to_spaces(Indent,Space),
   prepend_each_line(Space,Call).


%= 	 	 

%% indent_to_spaces( :PRED3N, ?Out) is semidet.
%
% Indent Converted To Spaces.
%
indent_to_spaces(1,' '):-!.
indent_to_spaces(0,''):-!.
indent_to_spaces(2,'  '):-!.
indent_to_spaces(3,'   '):-!.
indent_to_spaces(N,Out):- 1 is N rem 2,!, N1 is N-1, indent_to_spaces(N1,Spaces),atom_concat(' ',Spaces,Out).
indent_to_spaces(N,Out):- N2 is N div 2, indent_to_spaces(N2,Spaces),atom_concat(Spaces,Spaces,Out).


%= 	 	 

%% prepend_each_line( ?Pre, :GoalCall) is semidet.
%
% Prepend Each Line.
%
prepend_each_line(Pre,Call):-
  with_output_to_each(string(Str),Call)*->once(print_prepended(Pre,Str)).

:- meta_predicate if_color_debug(0).
:- meta_predicate if_color_debug(0,0).

%= 	 	 

%% if_color_debug is semidet.
%
% If Color Debug.
%
if_color_debug:-current_prolog_flag(dmsg_color,true).

%= 	 	 

%% if_color_debug( :GoalCall) is semidet.
%
% If Color Debug.
%
if_color_debug(Call):- if_color_debug(Call, true).

%= 	 	 

%% if_color_debug( :GoalCall, :GoalUnColor) is semidet.
%
% If Color Debug.
%
if_color_debug(Call,UnColor):- if_color_debug->Call;UnColor.



% % = :- export((portray_clause_w_vars/4,ansicall/3,ansi_control_conv/2)).

:- thread_local(tlbugger:skipDumpST9/0).
:- thread_local(tlbugger:skipDMsg/0).

% @(dmsg0(succeed(S_1)),[S_1=logic])


:- thread_local(tlbugger:no_slow_io/0).
:- multifile(tlbugger:no_slow_io/0).
%:- asserta(tlbugger:no_slow_io).


%= 	 	 

%% dmsg( ?C) is semidet.
%
% (debug)message.
%
dmsg(C):- notrace((tlbugger:no_slow_io,!,writeln(dmsg(C)))).
dmsg(V):- w_tl(set_prolog_flag(retry_undefined,false), if_defined(dmsg0(V),logicmoo_util_catch:ddmsg(V))).
%dmsg(F,A):- notrace((tlbugger:no_slow_io,on_x_fail(format(atom(S),F,A))->writeln(dmsg(S));writeln(dmsg_fail(F,A)))),!.

%= 	 	 

%% dmsg( ?F, ?A) is semidet.
%
% (debug)message.
%
dmsg(F,A):- w_tl(set_prolog_flag(retry_undefined, false),if_defined(dmsg0(F,A),logicmoo_util_catch:ddmsg(F,A))).



%= 	 	 

%% dmsginfo( ?V) is semidet.
%
% Dmsginfo.
%
dmsginfo(V):-dmsg(info(V)).

%= 	 	 

%% dmsg0( ?F, ?A) is semidet.
%
% (debug)message Primary Helper.
%
dmsg0(_,_):- is_hiding_dmsgs,!.
dmsg0(F,A):- is_sgr_on_code(F),!,dmsg(ansi(F,A)).
dmsg0(F,A):- dmsg(fmt0(F,A)).

%= 	 	 

%% vdmsg( ?L, ?F) is semidet.
%
% Vdmsg.
%
vdmsg(L,F):-loggerReFmt(L,LR),loggerFmtReal(LR,F,[]).

%= 	 	 

%% dmsg( ?L, ?F, ?A) is semidet.
%
% (debug)message.
%
dmsg(L,F,A):-loggerReFmt(L,LR),loggerFmtReal(LR,F,A).

:- thread_local(tlbugger:in_dmsg/1).
:- dynamic tlbugger:dmsg_hook/1.
:- multifile tlbugger:dmsg_hook/1.


%= 	 	 

%% dmsg0( ?V) is semidet.
%
% (debug)message Primary Helper.
%
dmsg0(V):-notrace(ignore(dmsg00(V))).

%= 	 	 

%% dmsg00( ?V) is semidet.
%
% (debug)message Primary Helper Primary Helper.
%
dmsg00(V):-cyclic_term(V),!,writeln(cyclic_term),flush_output,writeln(V),!.
dmsg00(V):- catch(logicmoo_util_dumpst:simplify_goal_printed(V,VV),_,fail),!,dmsg000(VV),!.
dmsg00(V):- dmsg000(V),!.

%% always_show_dmsg is semidet.
%
% Always Show (debug)message.
%
always_show_dmsg:- thread_self(main).
always_show_dmsg:- tlbugger:tl_always_show_dmsg.


%% dmsg000( ?V) is semidet.
%
% (debug)message Primary Helper Primary Helper Primary Helper.
%
dmsg000(V):-
   notrace(format(string(K),'~p',V)),
   (tlbugger:in_dmsg(K)-> dmsg5(V);  % format_to_error('~N% ~q~n',[dmsg0(V)]) ;
      asserta(tlbugger:in_dmsg(K),Ref),call_cleanup(dmsg1(V),erase(Ref))).

% = :- export(dmsg1/1).

%= 	 	 

%% dmsg1( ?V) is semidet.
%
% (debug)message Secondary Helper.
%
dmsg1(V):- tlbugger:is_with_dmsg(FP),!,FP=..FPL,append(FPL,[V],VVL),VV=..VVL,once(dmsg1(VV)).
dmsg1(_):- \+ always_show_dmsg, is_hiding_dmsgs,!.
dmsg1(V):- var(V),!,dmsg1(warn(dmsg_var(V))).
dmsg1(NC):- cyclic_term(NC),!,dtrace,format_to_error('~N% ~q~n',[dmsg_cyclic_term_1]).
dmsg1(NC):- tlbugger:skipDMsg,!,loop_check_early(dmsg2(NC),format_to_error('~N% ~q~n',[skipDMsg])).
dmsg1(V):- w_tl(tlbugger:skipDMsg,((once(dmsg2(V)), ignore((tlbugger:dmsg_hook(V),fail))))).

% = :- export(dmsg2/1).

%= 	 	 

%% dmsg2( :TermNC) is semidet.
%
% (debug)message Extended Helper.
%
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


%= 	 	 

%% dmsg3( ?C) is semidet.
%
% Dmsg3.
%
dmsg3(C):- tlbugger:no_slow_io,!,writeln(dmsg3(C)).
dmsg3(C):-
  ((functor_safe(C,Topic,_),debugging(Topic,_True_or_False),logger_property(Topic,once,true),!,
      (dmsg_log(Topic,_Time,C) -> true ; ((get_time(Time),asserta(dmsg_log(todo,Time,C)),!,dmsg4(C)))))),!.

dmsg3(C):-dmsg4(C),!.


%= 	 	 

%% dmsg4( ?Msg) is semidet.
%
% Dmsg4.
%
dmsg4(_):- notrace(show_source_location),fail.
dmsg4(Msg):-dmsg5(Msg).


%= 	 	 

%% dmsg5( ?Msg) is semidet.
%
% Dmsg5.
%
dmsg5(Msg):- to_stderror(in_cmt(fmt9(Msg))).

%= 	 	 

%% dmsg5( ?Msg, ?Args) is semidet.
%
% Dmsg5.
%
dmsg5(Msg,Args):- dmsg5(fmt0(Msg,Args)).



%= 	 	 

%% get_indent_level( :PRED2Max) is semidet.
%
% Get Indent Level.
%
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

%= 	 	 

%% ansifmt( ?Ctrl, ?Fmt) is semidet.
%
% Ansifmt.
%
ansifmt(Ctrl,Fmt):- colormsg(Ctrl,Fmt).
% = :- export(ansifmt/3).

%= 	 	 

%% ansifmt( ?Ctrl, ?F, ?A) is semidet.
%
% Ansifmt.
%
ansifmt(Ctrl,F,A):- colormsg(Ctrl,(format(F,A))).



%= 	 	 

%% debugm( ?X) is semidet.
%
% Debugm.
%
debugm(X):-notrace((compound(X),functor(X,F,_),!,debugm(F,X))),!.
debugm(X):-notrace((debugm(X,X))).

%= 	 	 

%% debugm( ?Why, ?Msg) is semidet.
%
% Debugm.
%
debugm(Why,Msg):- notrace(( \+ debugging(mpred), \+ debugging(Why), \+ debugging(mpred(Why)),!, debug(Why,'~N~p~n',[Msg]))),!.
debugm(Why,Msg):- notrace(( debug(Why,'~N~p~n',[Msg]))),!.


% = :- export(colormsg/2).

:- dynamic(is_hiding_dmsgs).

%= 	 	 

%% is_hiding_dmsgs is semidet.
%
% If Is A Hiding (debug)messages.
%
is_hiding_dmsgs:- \+always_show_dmsg, current_prolog_flag(opt_debug,false),!.
is_hiding_dmsgs:- \+always_show_dmsg, tlbugger:ifHideTrace,!.

%% colormsg( ?Ctrl, ?Msg) is semidet.
%
% Colormsg.
%
colormsg(d,Msg):- mesg_color(Msg,Ctrl),!,colormsg(Ctrl,Msg).
colormsg(Ctrl,Msg):- ansicall(Ctrl,fmt0(Msg)).

% = :- export(ansicall/2).

%= 	 	 

%% ansicall( ?Ctrl, :GoalCall) is semidet.
%
% Ansicall.
%
ansicall(Ctrl,Call):- notrace((current_output(Out), ansicall(Out,Ctrl,Call))).


%= 	 	 

%% ansi_control_conv( ?Ctrl, ?CtrlO) is semidet.
%
% Ansi Control Conv.
%
ansi_control_conv(Ctrl,CtrlO):-tlbugger:no_slow_io,!,flatten([Ctrl],CtrlO),!.
ansi_control_conv([],[]):-!.
ansi_control_conv([H|T],HT):-!,ansi_control_conv(H,HH),!,ansi_control_conv(T,TT),!,flatten([HH,TT],HT),!.
ansi_control_conv(warn,Ctrl):- !, ansi_control_conv(warning,Ctrl),!.
ansi_control_conv(Level,Ctrl):- ansi_term:level_attrs(Level,Ansi),Level\=Ansi,!,ansi_control_conv(Ansi,Ctrl).
ansi_control_conv(Color,Ctrl):- ansi_term:ansi_color(Color,_),!,ansi_control_conv(fg(Color),Ctrl).
ansi_control_conv(Ctrl,CtrlO):-flatten([Ctrl],CtrlO),!.



%= 	 	 

%% is_tty( ?Out) is semidet.
%
% If Is A Tty.
%
is_tty(Out):- not(tlbugger:no_colors), \+ tlbugger:no_slow_io, is_stream(Out),stream_property(Out,tty(true)).


%= 	 	 

%% ansicall( ?Out, ?UPARAM2, :GoalCall) is semidet.
%
% Ansicall.
%
ansicall(Out,_,Call):- \+ is_tty(Out),!,Call.
ansicall(_Out,_,Call):- tlbugger:skipDumpST9,!,Call.

% in_pengines:- if_defined(relative_frame(source_context_module,pengines,_)).

ansicall(_,_,Call):-tlbugger:no_slow_io,!,Call.
ansicall(Out,CtrlIn,Call):- once(ansi_control_conv(CtrlIn,Ctrl)),  CtrlIn\=Ctrl,!,ansicall(Out,Ctrl,Call).
ansicall(_,_,Call):- if_defined(in_pengines,fail),!,Call.
ansicall(Out,Ctrl,Call):-
   retractall(tlbugger:last_used_color(_)),asserta(tlbugger:last_used_color(Ctrl)),ansicall0(Out,Ctrl,Call),!.


%= 	 	 

%% ansicall0( ?Out, ?Ctrl, :GoalCall) is semidet.
%
% Ansicall Primary Helper.
%
ansicall0(Out,[Ctrl|Set],Call):-!, ansicall0(Out,Ctrl,ansicall0(Out,Set,Call)).
ansicall0(_,[],Call):-!,Call.
ansicall0(Out,Ctrl,Call):-if_color_debug(ansicall1(Out,Ctrl,Call),keep_line_pos_w_w(Out, Call)).


%= 	 	 

%% ansicall1( ?Out, ?Ctrl, :GoalCall) is semidet.
%
% Ansicall Secondary Helper.
%
ansicall1(Out,Ctrl,Call):-
   must(sgr_code_on_off(Ctrl, OnCode, OffCode)),!,
     keep_line_pos_w_w(Out, (format(Out, '\e[~wm', [OnCode]))),
	call_cleanup(Call,
           keep_line_pos_w_w(Out, (format(Out, '\e[~wm', [OffCode])))).
/*
ansicall(S,Set,Call):-
     call_cleanup((
         stream_property(S, tty(true)), current_prolog_flag(color_term, true), !,
	(is_list(Ctrl) ->  maplist(sgr_code_on_off, Ctrl, Codes, OffCodes),
          atomic_list_concat(Codes, (';'), OnCode) atomic_list_concat(OffCodes, (';'), OffCode) ;   sgr_code_on_off(Ctrl, OnCode, OffCode)),
        keep_line_pos_w_w(S, (format(S,'\e[~wm', [OnCode])))),
	call_cleanup(Call,keep_line_pos_w_w(S, (format(S, '\e[~wm', [OffCode]))))).


*/





%= 	 	 

%% keep_line_pos_w_w( ?S, :GoalG) is semidet.
%
% Hook To [ansi_term:keep_line_pos_w_w/2] For Module Logicmoo_util_dmsg.
% Keep Line Pos.
%
keep_line_pos_w_w(S, G) :-
       (stream_property(S, position(Pos)) ->
	(stream_position_data(line_position, Pos, LPos),
        call_cleanup(G, set_stream(S, line_position(LPos)))) ; G).


:- multifile(tlbugger:term_color0/2).
:- dynamic(tlbugger:term_color0/2).


%tlbugger:term_color0(retract,magenta).
%tlbugger:term_color0(retractall,magenta).

%= 	 	 

%% term_color0( ?VALUE1, ?VALUE2) is semidet.
%
% Hook To [tlbugger:term_color0/2] For Module Logicmoo_util_dmsg.
% Term Color Primary Helper.
%
tlbugger:term_color0(assertz,hfg(green)).
tlbugger:term_color0(ainz,hfg(green)).
tlbugger:term_color0(aina,hfg(green)).
tlbugger:term_color0(mpred_op,hfg(blue)).


%= 	 	 

%% mesg_color( :TermT, ?C) is semidet.
%
% Mesg Color.
%
mesg_color(_,[reset]):-tlbugger:no_slow_io,!.
mesg_color(T,C):-var(T),!,C=[blink(slow),fg(red),hbg(black)],!.
mesg_color(T,C):-is_sgr_on_code(T),!,C=T.
mesg_color(T,C):-cyclic_term(T),!,C=reset.
mesg_color("",C):- !,C=[blink(slow),fg(red),hbg(black)],!.
mesg_color(T,C):- string(T),!,must(f_word(T,F)),!,functor_color(F,C).
mesg_color([_,_,_T|_],C):-atom(T),mesg_color(T,C).
mesg_color([T|_],C):-atom(T),mesg_color(T,C).
mesg_color(T,C):-(atomic(T);is_list(T)), dmsg_text_to_string_safe(T,S),!,mesg_color(S,C).
mesg_color(T,C):-not(compound(T)),term_to_atom(T,A),!,mesg_color(A,C).
mesg_color(succeed(T),C):-nonvar(T),mesg_color(T,C).
% mesg_color((T),C):- \+ \+ ((predicate_property(T,meta_predicate(_)))),arg(_,T,E),compound(E),!,mesg_color(E,C).
mesg_color(=(T,_),C):-nonvar(T),mesg_color(T,C).
mesg_color(debug(T),C):-nonvar(T),mesg_color(T,C).
mesg_color(_:T,C):-nonvar(T),!,mesg_color(T,C).
mesg_color(T,C):-functor_safe(T,F,_),member(F,[color,ansi]),compound(T),arg(1,T,C),nonvar(C).
mesg_color(T,C):-functor_safe(T,F,_),member(F,[succeed,must,mpred_op_prolog]),compound(T),arg(1,T,E),nonvar(E),!,mesg_color(E,C).
mesg_color(T,C):-functor_safe(T,F,_),member(F,[fmt0,msg]),compound(T),arg(2,T,E),nonvar(E),!,mesg_color(E,C).
mesg_color(T,C):-predef_functor_color(F,C),mesg_arg1(T,F).
mesg_color(T,C):-nonvar(T),defined_message_color(F,C),matches_term(F,T),!.
mesg_color(T,C):-functor(T,F,_),!,functor_color(F,C),!.



%= 	 	 

%% f_word( ?T, ?A) is semidet.
%
% Functor Word.
%
f_word("",""):-!.
f_word(T,A):-concat_atom(List,' ',T),member(A,List),atom(A),atom_length(A,L),L>0,!.
f_word(T,A):-concat_atom(List,'_',T),member(A,List),atom(A),atom_length(A,L),L>0,!.
f_word(T,A):- string_to_atom(T,P),sub_atom(P,0,10,_,A),A\==P,!.
f_word(T,A):- string_to_atom(T,A),!.


%= 	 	 

%% mesg_arg1( :TermT, ?TT) is semidet.
%
% Mesg Argument Secondary Helper.
%
mesg_arg1(T,_TT):-var(T),!,fail.
mesg_arg1(_:T,C):-nonvar(T),!,mesg_arg1(T,C).
mesg_arg1(T,TT):-not(compound(T)),!,T=TT.
mesg_arg1(T,C):-compound(T),arg(1,T,F),nonvar(F),!,mesg_arg1(F,C).
mesg_arg1(T,F):-functor(T,F,_).


% = :- export(defined_message_color/2).
:- dynamic(defined_message_color/2).


%= 	 	 

%% defined_message_color( ?A, ?B) is semidet.
%
% Defined Message Color.
%
defined_message_color(todo,[fg(red),bg(black),underline]).
%defined_message_color(error,[fg(red),hbg(black),bold]).
defined_message_color(warn,[fg(black),hbg(red),bold]).
defined_message_color(A,B):-tlbugger:term_color0(A,B).



%= 	 	 

%% predef_functor_color( ?F, ?C) is semidet.
%
% Predef Functor Color.
%
predef_functor_color(F,C):- defined_message_color(F,C),!.
predef_functor_color(F,C):- defined_message_color(F/_,C),!.
predef_functor_color(F,C):- tlbugger:term_color0(F,C),!.


%= 	 	 

%% functor_color( ?F, ?C) is semidet.
%
% Functor Color.
%
functor_color(F,C):- predef_functor_color(F,C),!.
functor_color(F,C):- next_color(C),ignore(on_x_fail(assertz(tlbugger:term_color0(F,C)))),!.


:- thread_local(tlbugger:last_used_color/1).

% tlbugger:last_used_color(pink).


%= 	 	 

%% last_used_fg_color( ?LFG) is semidet.
%
% Last Used Fg Color.
%
last_used_fg_color(LFG):-tlbugger:last_used_color(LU),fg_color(LU,LFG),!.
last_used_fg_color(default).


%= 	 	 

%% good_next_color( ?C) is semidet.
%
% Good Next Color.
%
good_next_color(C):-var(C),!,trace_or_throw(var_good_next_color(C)),!.
good_next_color(C):- last_used_fg_color(LFG),fg_color(C,FG),FG\=LFG,!.
good_next_color(C):- not(unliked_ctrl(C)).


%= 	 	 

%% unliked_ctrl( ?X) is semidet.
%
% Unliked Ctrl.
%
unliked_ctrl(fg(blue)).
unliked_ctrl(fg(black)).
unliked_ctrl(fg(red)).
unliked_ctrl(bg(white)).
unliked_ctrl(hbg(white)).
unliked_ctrl(X):-is_list(X),member(E,X),nonvar(E),unliked_ctrl(E).


%= 	 	 

%% fg_color( ?LU, ?FG) is semidet.
%
% Fg Color.
%
fg_color(LU,FG):-member(fg(FG),LU),FG\=white,!.
fg_color(LU,FG):-member(hfg(FG),LU),FG\=white,!.
fg_color(_,default).

% = :- export(random_color/1).

%= 	 	 

%% random_color( ?M) is semidet.
%
% Random Color.
%
random_color([reset,M,FG,BG,font(Font)]):-Font is random(8),
  findall(Cr,ansi_term:ansi_color(Cr, _),L),
  random_member(E,L),
  random_member(FG,[hfg(E),fg(E)]),not(unliked_ctrl(FG)),
  contrasting_color(FG,BG), not(unliked_ctrl(BG)),
  random_member(M,[bold,faint,reset,bold,faint,reset,bold,faint,reset]),!. % underline,negative


% = :- export(tst_color/0).

%= 	 	 

%% tst_color is semidet.
%
% Tst Color.
%
tst_color:- make, ignore((( between(1,20,_),random_member(Call,[colormsg(C,cm(C)),dmsg(color(C,dm(C))),ansifmt(C,C)]),next_color(C),Call,fail))).
% = :- export(tst_color/1).

%= 	 	 

%% tst_color( ?C) is semidet.
%
% Tst Color.
%
tst_color(C):- make,colormsg(C,C).

% = :- export(next_color/1).

%= 	 	 

%% next_color( :TermC) is semidet.
%
% Next Color.
%
next_color(C):- between(1,10,_), random_color(C), good_next_color(C),!.
next_color([underline|C]):- random_color(C),!.

% = :- export(contrasting_color/2).

%= 	 	 

%% contrasting_color( ?A, ?VALUE2) is semidet.
%
% Contrasting Color.
%
contrasting_color(white,black).
contrasting_color(A,default):-atom(A),A \= black.
contrasting_color(fg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(hfg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(black,white).
contrasting_color(default,default).
contrasting_color(_,default).

:- thread_local(ansi_prop/2).


%= 	 	 

%% sgr_on_code( ?Ctrl, :PRED7OnCode) is semidet.
%
% Sgr Whenever Code.
%
sgr_on_code(Ctrl,OnCode):- sgr_on_code0(Ctrl,OnCode),!.
sgr_on_code(_Foo,7):-!. %  notrace((format_to_error('~NMISSING: ~q~n',[bad_sgr_on_code(Foo)]))),!.


%= 	 	 

%% is_sgr_on_code( ?Ctrl) is semidet.
%
% If Is A Sgr Whenever Code.
%
is_sgr_on_code(Ctrl):-notrace(sgr_on_code0(Ctrl,_)),!.


%= 	 	 

%% sgr_on_code0( ?Ctrl, :PRED6OnCode) is semidet.
%
% Sgr Whenever Code Primary Helper.
%
sgr_on_code0(Ctrl,OnCode):- ansi_term:sgr_code(Ctrl,OnCode).
sgr_on_code0(blink, 6).
sgr_on_code0(-Ctrl,OffCode):-  nonvar(Ctrl), sgr_off_code(Ctrl,OffCode).


%= 	 	 

%% sgr_off_code( ?Ctrl, :GoalOnCode) is semidet.
%
% Sgr Off Code.
%
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



%= 	 	 

%% sgr_code_on_off( ?Ctrl, ?OnCode, ?OffCode) is semidet.
%
% Sgr Code Whenever Off.
%
sgr_code_on_off(Ctrl,OnCode,OffCode):-sgr_on_code(Ctrl,OnCode),sgr_off_code(Ctrl,OffCode),!.
sgr_code_on_off(Ctrl,OnCode,OffCode):-sgr_on_code(Ctrl,OnCode),sgr_off_code(Ctrl,OffCode),!.
sgr_code_on_off(_Ctrl,_OnCode,[default]):-!.



%= 	 	 

%% msg_to_string( :TermVar, ?Str) is semidet.
%
% Msg Converted To String.
%
msg_to_string(Var,Str):-var(Var),!,sformat(Str,'~q',[Var]),!.
msg_to_string(portray(Msg),Str):- with_output_to_each(string(Str),(current_output(Out),portray_clause_w_vars(Out,Msg,[],[]))),!.
msg_to_string(pp(Msg),Str):- sformat(Str,Msg,[],[]),!.
msg_to_string(fmt(F,A),Str):-sformat(Str,F,A),!.
msg_to_string(format(F,A),Str):-sformat(Str,F,A),!.
msg_to_string(Msg,Str):-atomic(Msg),!,sformat(Str,'~w',[Msg]).
msg_to_string(m2s(Msg),Str):-message_to_string(Msg,Str),!.
msg_to_string(Msg,Str):-sformat(Str,Msg,[],[]),!.


:- thread_local t_l:formatter_hook/4.


%= 	 	 

%% withFormatter( ?Lang, ?From, ?Vars, ?SForm) is semidet.
%
% Using Formatter.
%
withFormatter(Lang,From,Vars,SForm):- t_l:formatter_hook(Lang,From,Vars,SForm),!.
withFormatter(_Lang,From,_Vars,SForm):-sformat(SForm,'~w',[From]).


%= 	 	 

%% flush_output_safe is semidet.
%
% Flush Output Safely Paying Attention To Corner Cases.
%
flush_output_safe:-ignore(catchv(flush_output,_,true)).

%= 	 	 

%% flush_output_safe( ?X) is semidet.
%
% Flush Output Safely Paying Attention To Corner Cases.
%
flush_output_safe(X):-ignore(catchv(flush_output(X),_,true)).


%= 	 	 

%% writeFailureLog( ?E, ?X) is semidet.
%
% Write Failure Log.
%
writeFailureLog(E,X):-
  get_thread_current_error(ERR),
		(fmt(ERR,'\n% error: ~q ~q\n',[E,X]),flush_output_safe(ERR),!,
		%,true.
		fmt('\n% error: ~q ~q\n',[E,X]),!,flush_output).

%unknown(Old, autoload).


%= 	 	 

%% cls is semidet.
%
% Clauses.
%
cls:- shell(cls).

:- use_module(library(random)).
:- use_module(logicmoo_util_varnames).
:- use_module(logicmoo_util_catch).
% :- autoload([verbose(false)]).

/*
:- 'mpred_trace_none'(fmt(_)).
:- 'mpred_trace_none'(fmt(_,_)).
:- 'mpred_trace_none'(dfmt(_)).
:- 'mpred_trace_none'(dfmt(_,_)).
:- 'mpred_trace_none'(dmsg(_)).
:- 'mpred_trace_none'(dmsg(_,_)).
:- 'mpred_trace_none'(portray_clause_w_vars(_)).
*/

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

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_catch.pl
:- module(logicmoo_util_catch,
          [ !/1,
            addLibraryDir/0,
            source_variables_l/1,
            as_clause_no_m/3,
            as_clause_w_m/4,
            as_clause_w_m/5,
            source_module/1,
            bad_functor/1,
            current_why/1,
            badfood/1,
            block/2,
            block/3,
            bubbled_ex/1,
            bubbled_ex_check/1,
            catchv/3,
            catchvvnt/3,
            current_source_file/1,current_source_location0/1,
            current_main_error_stream/1,
            dbgsubst/4,            
            dbgsubst0/4,
            ddmsg/1,
            ddmsg/2,
            ddmsg_call/1,            
            det_lm/2,
            dif_safe/2,
            errx/0,
            on_x_fail/1,
            format_to_error/2,
            fresh_line_to_err/0,
            functor_catch/3,
            functor_safe/3,
            get_must/2,
            ib_multi_transparent33/1,
            if_defined/1,
            if_defined/2,
            if_defined_else/2,
            input_key/1,
            is_ftCompound/1,
            is_ftNonvar/1,
            is_ftVar/1,
            is_hiding_dmsgs/0,
            is_main_thread/0,
            is_pdt_like/0,
            is_release/0,
            keep/2,
            loading_file/1,
            on_x_log_throw/1,
            %on_x_log_throwEach/1,
            on_x_log_cont/1,
            on_x_log_fail/1,
            maplist_safe/2,
            maplist_safe/3,            
            module_functor/4,

            trace_or_throw/1,
            trace_or_throw/1,

            must/1,
            must_det/1,
            must_det/2,
            must_det_l/1,
            must_det_lm/2,
            must_l/1,
            nd_dbgsubst/4,
            nd_dbgsubst1/5,
            nd_dbgsubst2/4,
            nop/1,
            not_is_release/0,
            one_must/2,
            one_must_det/2,
            sanity/1,
            save_streams/0,
            save_streams/1,
            set_block_exit/2,
            showHiddens/0,
            show_new_src_location/1,
            show_new_src_location/2,
            show_source_location/0,
            skipWrapper/0,
            slow_sanity/1,
            strip_arity/3,
            strip_f_module/2,
            thread_current_error_stream/1,
            throwNoLib/0,
            to_m_f_arity_pi/5,
            to_pi/2,
            to_pi0/3,
            warn_bad_functor/1,
            when_defined/1,
            with_main_error_to_output/1,
            with_main_input/1,
            with_main_io/1,
            with_preds/6,
            without_must/1,
            y_must/2
          ]).
:- meta_predicate
		block(+, :, ?),
		catchv(0, ?, 0),
		catchvvnt(0, ?, 0),
		catchv(0, ?, 0),
		
		if_defined(:),
		if_defined(:, 0),
		if_defined_else(:, 0),
		ddmsg_call(0),
		on_x_fail(0),
		on_x_log_throw(0),
		
		
		on_x_log_cont(0),
		on_x_log_fail(0),
		if_defined_else(:, 0),        

        must(0),
        must_det(0),
        must_det(0, 0),
        must_det_l(0),
        must_l(0),
        one_must(0, 0),
        one_must_det(0, 0),
        sanity(0),
        slow_sanity(0),
        to_pi(?, ?),
        when_defined(:),
        with_main_error_to_output(0),
        with_main_input(0),
        with_main_io(0),
        with_preds(?, ?, ?, ?, ?, 0),
        without_must(0),
        %on_x_log_throwEach(0),
        y_must(?, 0). 
:- module_transparent
        !/1,
        addLibraryDir/0,
        as_clause_no_m/3,
        as_clause_w_m/4,
        as_clause_w_m/5,
        bad_functor/1,        
        badfood/1,
        block/2,
        bubbled_ex/1,
        bubbled_ex_check/1,
        current_source_file/1,
        current_main_error_stream/1,
        dbgsubst/4,
        dbgsubst0/4,
        ddmsg/1,
        ddmsg/2,
        det_lm/2,
        dif_safe/2,
        errx/0,
        format_to_error/2,
        fresh_line_to_err/0,
        functor_catch/3,
        functor_safe/3,
        get_must/2,
        ib_multi_transparent33/1,
        input_key/1,
        is_ftCompound/1,
        is_ftNonvar/1,
        is_ftVar/1,
        is_hiding_dmsgs/0,
        is_main_thread/0,
        is_pdt_like/0,
        is_release/0,
        keep/2,
        loading_file/1,
        %on_x_log_throwEach/1,
        maplist_safe/2,
        maplist_safe/3,
        module_functor/4,

        must_det_lm/2,
        nd_dbgsubst/4,
        nd_dbgsubst1/5,
        nd_dbgsubst2/4,
        nop/1,
        not_is_release/0,
        save_streams/0,
        save_streams/1,
        set_block_exit/2,
        showHiddens/0,
        show_new_src_location/1,
        show_new_src_location/2,
        show_source_location/0,
        skipWrapper/0,
        strip_arity/3,
        strip_f_module/2,
        thread_current_error_stream/1,
        throwNoLib/0,
        to_m_f_arity_pi/5,
        to_pi0/3,
        warn_bad_functor/1.
:- dynamic
        is_hiding_dmsgs/0.

:- include('logicmoo_util_header.pi').

% = :- meta_predicate(catchvvnt(0,?,0)).

%= 	 	 

%% catchvvnt( :GoalT, ?E, :GoalF) is semidet.
%
% Catchvvnt.
%
catchvvnt(T,E,F):-catchv(cnotrace(T),E,F).

:- thread_self(Goal),assert(lmcache:thread_main(user,Goal)).


%= 	 	 

%% is_pdt_like is semidet.
%
% If Is A Pdt Like.
%
is_pdt_like:-thread_property(_,alias(pdt_console_server)).
is_pdt_like:-lmcache:thread_main(user,Goal),!,Goal \= main.


%= 	 	 

%% is_main_thread is semidet.
%
% If Is A Main Thread.
%
is_main_thread:-lmcache:thread_main(user,Goal),!,thread_self(Goal).

:- thread_local(tlbugger:no_colors/0).
:- thread_local(t_l:thread_local_current_main_error_stream/1).

:- is_pdt_like-> assert(tlbugger:no_colors); true.


% = :- meta_predicate(with_main_error_to_output(0)).

%= 	 	 

%% with_main_error_to_output( :GoalGoal) is semidet.
%
% Using Main Error Converted To Output.
%
with_main_error_to_output(Goal):-
 current_output(Out),
  w_tl(t_l:thread_local_current_main_error_stream(Out),Goal).
   


%= 	 	 

%% thread_current_error_stream( ?Err) is semidet.
%
% Thread Current Error Stream.
%
thread_current_error_stream(Err):- t_l:thread_local_current_main_error_stream(Err),!.
thread_current_error_stream(Err):- thread_self(ID),thread_current_error_stream(ID,Err),!.
thread_current_error_stream(Err):- stream_property(Err,alias(current_error)),!.
thread_current_error_stream(Err):- stream_property(Err,alias(user_error)),!.
thread_current_error_stream(Err):- current_main_error_stream(Err),!.

%= 	 	 

%% current_main_error_stream( ?Err) is semidet.
%
% Current Main Error Stream.
%
current_main_error_stream(Err):- t_l:thread_local_current_main_error_stream(Err),!.
current_main_error_stream(Err):- lmcache:thread_main(user,ID),thread_current_error_stream(ID,Err).
current_main_error_stream(Err):- thread_current_error_stream(main,Err).
current_main_error_stream(Err):- stream_property(Err,alias(user_error)),!.
current_main_error_stream(Err):- stream_property(Err,alias(current_error)),!.

%= 	 	 

%% format_to_error( ?F, ?A) is semidet.
%
% Format Converted To Error.
%
format_to_error(F,A):-current_main_error_stream(Err),!,format(Err,F,A).

%= 	 	 

%% fresh_line_to_err is semidet.
%
% Fresh Line Converted To Err.
%
fresh_line_to_err:- cnotrace((flush_output_safe,current_main_error_stream(Err),format(Err,'~N',[]),flush_output_safe(Err))).

:- dynamic(thread_current_input/2).
:- dynamic(thread_current_error_stream/2).
:- volatile(thread_current_input/2).
:- volatile(thread_current_error_stream/2).

%= 	 	 

%% save_streams is semidet.
%
% Save Streams.
%
save_streams:- thread_self(ID), save_streams(ID).


%= 	 	 

%% save_streams( ?ID) is semidet.
%
% Save Streams.
%
save_streams(ID):- current_input(In),thread_current_input(ID,In),!.
save_streams(ID):-
  current_input(In),asserta(thread_current_input(ID,In)),
  current_output(Err),asserta(thread_current_error_stream(ID,Err)).

% = :- meta_predicate(with_main_input(0)).

%= 	 	 

%% with_main_input( :GoalGoal) is semidet.
%
% Using Main Input.
%
with_main_input(Goal):-
    current_output(OutPrev),
    current_input(InPrev),
    stream_property(ErrPrev,alias(user_error)),
    lmcache:thread_main(user,ID),thread_current_input(ID,In),thread_current_error_stream(ID,Err),
    setup_call_cleanup(set_prolog_IO(In,OutPrev,Err),Goal,set_prolog_IO(InPrev,OutPrev,ErrPrev)).


%= 	 	 

%% with_main_io( :GoalGoal) is semidet.
%
% Using Main Input/output.
%
 with_main_io(Goal):-
    current_output(OutPrev),
    current_input(InPrev),
    stream_property(ErrPrev,alias(user_error)),
    lmcache:thread_main(user,ID),thread_current_input(ID,In),thread_current_error_stream(ID,Err),
    setup_call_cleanup(set_prolog_IO(In,Err,Err),Goal,set_prolog_IO(InPrev,OutPrev,ErrPrev)).

:- save_streams.
:- initialization(save_streams).

:- dynamic(is_hiding_dmsgs).

%= 	 	 

%% is_hiding_dmsgs is semidet.
%
% If Is A Hiding (debug)messages.
%
is_hiding_dmsgs:- \+always_show_dmsg, current_prolog_flag(opt_debug,false),!.
is_hiding_dmsgs:- \+always_show_dmsg, tlbugger:ifHideTrace,!.

% bugger_debug=false turns off just debugging about the debugger
% opt_debug=false turns off all the rest of debugging
% ddmsg(_):-current_prolog_flag(bugger_debug,false),!.
% ddmsg(D):- current_predicate(_:wdmsg/1),wdmsg(D),!.

%= 	 	 

%% ddmsg( ?D) is semidet.
%
% Ddmsg.
%
ddmsg(D):- ddmsg('~q',[D]).
%ddmsg(F,A):- current_predicate(_:wdmsg/2),wdmsg(F,A),!.

%= 	 	 

%% ddmsg( ?F, ?A) is semidet.
%
% Ddmsg.
%
ddmsg(F,A):- format_to_error(F,A),!.

%= 	 	 

%% ddmsg_call( :GoalD) is semidet.
%
% Ddmsg Call.
%
ddmsg_call(D):- ( (ddmsg(ddmsg_call(D)),call(D),ddmsg(ddmsg_exit(D))) *-> true ; ddmsg(ddmsg_failed(D))).



:- meta_predicate if_defined(:).
:- export(if_defined/1).

%= 	 	 

%% if_defined( ?G) is semidet.
%
% If Defined.
%
if_defined(C:G):-current_predicate(_,C:G),!,on_x_fail(C:G).
if_defined(_:G):-current_predicate(_,R:G),!,on_x_fail(R:G).
if_defined(G):-current_predicate(_,R:G),!,on_x_fail(R:G).
if_defined(G):-current_predicate(_,G),!,on_x_fail(G).
if_defined(Goal):- tlbugger:show_must_go_on,!,if_defined(Goal,((dmsg(warn_undefined(Goal))),!,fail)).
if_defined(Goal):- !, if_defined(Goal,(dmsg(warn_undefined(Goal)),trace)).

:- meta_predicate if_defined(:,0).
:- export(if_defined/2).

%= 	 	 

%% if_defined( ?Goal, :GoalElse) is semidet.
%
% If Defined.
%
if_defined(Goal,_Else):-current_predicate(_,Goal),!,Goal.
if_defined(_:Goal,Else):-current_predicate(_,OM:Goal)->OM:Goal;(writeln(warn_undefined(Goal)),Else).

:- meta_predicate if_defined_else(:,0).
:- export(if_defined_else/2).

%= 	 	 

%% if_defined_else( ?Goal, :GoalElse) is semidet.
%
% If Defined Else.
%
if_defined_else(Goal,_Else):-current_predicate(_,Goal),!,Goal.
if_defined_else(_:Goal,Else):-current_predicate(_,OM:Goal)->OM:Goal;Else.

:- meta_predicate when_defined(:).
:- export(when_defined/1).

%= 	 	 

%% when_defined( ?Goal) is semidet.
%
% When Defined.
%
when_defined(Goal):-if_defined(Goal,true).

:- if(current_predicate(run_sanity_tests/0)).
:- listing(thread_current_error_stream/2).
:- endif.

% = :- meta_predicate(to_pi(?,?)).

%= 	 	 

%% to_pi( ?P, ?M) is semidet.
%
% Converted To Predicate Indicator.
%
to_pi(P,M:P):-var(P),!,current_module(M).
to_pi(M:P,M:P):-var(P),!,current_module(M).
to_pi(Find,(M:PI)):- once(catch(match_predicates(Find,Found),_,fail)),Found=[_|_],!,member(M:F/A,Found),functor(PI,F,A).
to_pi(M:Find,M:PI):-!,current_module(M),to_pi0(M,Find,M:PI).
to_pi(Find,M:PI):-current_module(M),to_pi0(M,Find,M:PI).


%= 	 	 

%% to_pi0( ?M, :TermFind, :TermPI) is semidet.
%
% Converted To Predicate Indicator Primary Helper.
%
to_pi0(M,Find,M:PI):- atom(Find),!,when(nonvar(PI),(nonvar(PI),functor(PI,Find,_))).
to_pi0(M,Find/A,M:PI):-var(Find),number(A),!,when(nonvar(PI),(nonvar(PI),functor(PI,_,A))).
to_pi0(M,Find,PI):-get_pi(Find,PI0),!,(PI0\=(_:_)->(current_module(M),PI=(M:PI0));PI=PI0).


:- thread_local(t_l:last_src_loc/2).

%= 	 	 

%% input_key( ?K) is semidet.
%
% Input Key.
%
input_key(K):-thread_self(K).
   

%= 	 	 

%% show_new_src_location( ?FL) is semidet.
%
% Show New Src Location.
%
show_new_src_location(FL):-input_key(K),show_new_src_location(K,FL).


%= 	 	 

%% show_new_src_location( ?K, ?FL) is semidet.
%
% Show New Src Location.
%
show_new_src_location(K,FL):- t_l:last_src_loc(K,FL),!.
show_new_src_location(K,FL):- retractall(t_l:last_src_loc(K,_)),format_to_error('~N% ~w ',[FL]),!,asserta(t_l:last_src_loc(K,FL)).


:- thread_local(t_l:current_local_why/2).
:- thread_local(t_l:current_why_source/1).


%= 	 	 

%% sl_to_filename( ?W, ?W) is semidet.
%
% Sl Converted To Filename.
%
sl_to_filename(W,W):-atom(W),!.
sl_to_filename(_:W,W):-atom(W),!.
sl_to_filename(W,W).
sl_to_filename(W,To):-nonvar(To),To=(W:_),atom(W),!.



%= 	 	 

%% current_source_file( ?F) is semidet.
%
% Current Source Location.
%
current_source_file(F):- clause(current_source_location0(W),Body),catchv(Body,_,fail),
 sl_to_filename(W,F),!.
current_source_file(F):- F = unknown.


%= 	 	 

%% current_source_location0( :TermF) is semidet.
%
% Current Source Location Primary Helper.
%
current_source_location0(F):- t_l:current_why_source(F).
current_source_location0(F:L):-source_location(F,L),!.
current_source_location0(F:L):-prolog_load_context(file,F),current_input(S),line_position(S,L),!.
current_source_location0(F):-loading_file(F).
current_source_location0(F:L):- current_filesource(F),ignore((prolog_load_context(stream,S),!,line_count(S,L))),!.
current_source_location0(F:L):- prolog_load_context(file,F),!,ignore((prolog_load_context(stream,S),!,line_count(S,L))),!.
current_source_location0(module(M)):-source_module(M),!.
current_source_location0(When):-current_input(S),findall(NV,stream_property(S,NV),When),!.
current_source_location0(module(M)):- '$module'(M,M).

:-export(current_why/1).
:-module_transparent(current_why/1).

%= 	 	 

%% current_why( ?Why) is semidet.
%
% Current Generation Of Proof.
%
current_why(Why):- t_l:current_local_why(Why,_),!.
current_why(mfl(M,F,L)):- source_module(M), current_source_file(F:L).



% source_module(M):-!,M=u.
:-export(source_module/1).

%= 	 	 

%% source_module( ?M) is semidet.
%
% Source Module.
%
source_module(M):-nonvar(M),source_module(M0),!,M0=M.
source_module(M):-'$set_source_module'(M,   M),!.
source_module(M):-loading_module(M),!.

:- thread_local(t_l:last_source_file/1).
:- export(loading_file/1).

%= 	 	 

%% loading_file( ?FIn) is semidet.
%
% Loading File.
%
loading_file(FIn):- ((source_file0(F) *-> (retractall(t_l:last_source_file(_)),asserta(t_l:last_source_file(F))) ; (fail,t_l:last_source_file(F)))),!,F=FIn.

%= 	 	 

%% source_file0( ?F) is semidet.
%
% Source File Primary Helper.
%
source_file0(F):-source_location(F,_).
source_file0(F):-prolog_load_context(file, F).
source_file0(F):-prolog_load_context(source, F).
source_file0(F):-seeing(Goal),is_stream(Goal),stream_property(Goal,file_name(F)),exists_file(F).
source_file0(F):-prolog_load_context(stream, S),stream_property(S,file_name(F)),exists_file(F).
source_file0(F):-findall(E,catch((stream_property( S,mode(read)),stream_property(S,file_name(E)),exists_file(E),
  line_count(S,Goal),Goal>0),_,fail),L),last(L,F).


:-export(source_variables_l/1).

%= 	 	 

%% source_variables_l( ?AllS) is semidet.
%
% Source Variables (list Version).
%
source_variables_l(AllS):-
 notrace((
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (nb_current('$variable_names', Vs2);Vs2=[]),
  notrace(catch((parent_goal('$toplevel':'$execute_goal2'(_, Vs3),_);Vs3=[]),E,(writeq(E),Vs3=[]))),
  ignore(Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  nb_linkval('$variable_names', AllS))).



%= 	 	 

%% source_variables( ?Vs) is semidet.
%
% Source Variables.
%
source_variables(Vs):- (((prolog_load_context(variable_names,Vs),Vs\==[]);
   (nb_current('$variable_names', Vs),Vs\==[]))),!.
source_variables(Vars):-var(Vars),parent_goal('$toplevel':'$execute_goal2'(_, Vars),_),!.
source_variables([]).


:-export( show_source_location/0).
%show_source_location:- notrace((tlbugger:no_slow_io)),!.
%show_source_location:- is_hiding_dmsgs,!.

%= 	 	 

%% show_source_location is semidet.
%
% Show Source Location.
%
show_source_location:- source_location(F,L),!,show_new_src_location(F:L),!.
show_source_location:- current_source_file(FL),!,show_new_src_location(FL),!.
show_source_location.


% % :- use_module(logicmoo_util_database).

:-export( as_clause_no_m/3).

%= 	 	 

%% as_clause_no_m( ?MHB, ?H, ?B) is semidet.
%
% Converted To Clause No Module.
%
as_clause_no_m( MHB,  H, B):- strip_module(MHB,_M,HB), expand_to_hb( HB,  MH, MB),strip_module(MH,_M2H,H),strip_module(MB,_M2B,B).

%= 	 	 

%% as_clause_w_m( ?MHB, ?M, ?H, ?B) is semidet.
%
% Converted To Clause W Module.
%
as_clause_w_m(MHB, M, H, B):-  as_clause_w_m(MHB, M1H, H, B, M2B), (M1H==user->M2B=M;M1H=M).

%= 	 	 

%% as_clause_w_m( ?MHB, ?M1H, ?H, ?B, ?M2B) is semidet.
%
% Converted To Clause W Module.
%
as_clause_w_m(MHB, M1H, H, B, M2B):-  expand_to_hb( MHB,  MH, MB),strip_module(MH,M1H,H),strip_module(MB,M2B,B).

:- export(is_ftCompound/1).

%= 	 	 

%% is_ftCompound( ?Goal) is semidet.
%
% If Is A Format Type Compound.
%
is_ftCompound(Goal):-compound(Goal),Goal\='$VAR'(_).

:- export(is_ftVar/1).

%= 	 	 

%% is_ftVar( :TermV) is semidet.
%
% If Is A Format Type Variable.
%
is_ftVar(V):-var(V),!.
is_ftVar('$VAR'(_)).

:- export(is_ftNonvar/1).

%= 	 	 

%% is_ftNonvar( ?V) is semidet.
%
% If Is A Format Type Nonvar.
%
is_ftNonvar(V):- \+ is_ftVar(V).


%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[Goal,Goal,Goal],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

:- export((   maplist_safe/2,
   maplist_safe/3)).


%= 	 	 

%% maplist_safe( ?Pred, ?LIST) is semidet.
%
% Maplist Safely Paying Attention To Corner Cases.
%
maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST), on_f_debug(apply(Pred,[E]))),LISTO),!, ignore(LIST=LISTO),!.
% though this should been fine %  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), on_f_debug(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.


%= 	 	 

%% maplist_safe( ?Pred, ?LISTIN, ?LIST) is semidet.
%
% Maplist Safely Paying Attention To Corner Cases.
%
maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),on_f_debug(apply(Pred,[E,EE])))), LISTO),  ignore(LIST=LISTO),!.
% though this should been fine % maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), debugOnFailureEach(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, ignore(OUT=[AA|BB]).



:- export(bad_functor/1).

%= 	 	 

%% bad_functor( ?L) is semidet.
%
% Bad Functor.
%
bad_functor(L) :- arg(_,v('|','.',[],':','/'),L).

:- export(warn_bad_functor/1).

%= 	 	 

%% warn_bad_functor( ?L) is semidet.
%
% Warn Bad Functor.
%
warn_bad_functor(L):-ignore((hotrace(bad_functor(L)),!,trace,nop(ddmsg(bad_functor(L))))).

:- export(strip_f_module/2).

%= 	 	 

%% strip_f_module( ?P, ?PA) is semidet.
%
% Strip Functor Module.
%
strip_f_module(_:P,FA):-nonvar(P),!,strip_f_module(P,F),!,F=FA.
strip_f_module(P,PA):-atom(P),!,P=PA.

strip_f_module(P,FA):- is_list(P),catch(text_to_string(P,S),_,fail),!,atom_string(F,S),!,F=FA.
strip_f_module(P,FA):- hotrace(string(P);atomic(P)), atom_string(F,P),!,F=FA.
strip_f_module(P,P).

% use catchv/3 to replace catch/3 works around SWI specific issues arround using $abort/0 and block/3
% (catch/3 allows you to have these exceptions bubble up past your catch block handlers)
% = :- meta_predicate((catchv(0, ?, 0))).
% = :- meta_predicate((catchv(0, ?, 0))).
:- export((catchv/3,catchv/3)).


%= 	 	 

%% bubbled_ex( ?VALUE1) is semidet.
%
% Bubbled Ex.
%
bubbled_ex(block(_,_)).
bubbled_ex('$aborted').

%= 	 	 

%% bubbled_ex_check( ?E) is semidet.
%
% Bubbled Ex Check.
%
bubbled_ex_check(E):- ( \+ bubbled_ex(E)),!.
bubbled_ex_check(E):-throw(E).


%= 	 	 

%% catchv( :GoalGoal, ?E, :GoalRecovery) is semidet.
%
% Catchv.
%
catchv(Goal,E,Recovery):- nonvar(E) -> catch(Goal,E,Recovery); % normal mode (the user knows what they want)
                         catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode

% catchv(Goal,E,Recovery):- catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode


%:- mpred_trace_nochilds(_,catchv,3,0,0).


:- export(functor_catch/3).

%= 	 	 

%% functor_catch( ?P, ?F, ?A) is semidet.
%
% Functor Catch.
%
functor_catch(P,F,A):- catchv(functor(P,F,A),_,compound_name_arity(P,F,A)).
% functor_catch(F,F,0):-atomic(F),!.
% functor_catch(P,F,A):-catchv(compound_name_arity(P,F,A),E,(trace,ddmsg(E:functor(P,F,A)),trace)).


:- export(functor_safe/3).

%= 	 	 

%% functor_safe( ?P, ?F, ?A) is semidet.
%
% Functor Safely Paying Attention To Corner Cases.
%
functor_safe(P,F,A):- catchv(functor(P,F,A),_,compound_name_arity(P,F,A)).
% functor_safe(P,F,A):- catchv(compound_name_arity(P,F,A),_,functor(P,F,A)).
/*
% functor_safe(P,F,A):-var(P),A==0,compound_name_arguments(P,F,[]),!.
functor_safe(P,F,A):-var(P),A==0,!,P=F,!.
functor_safe(P,F,A):-functor_safe0(P,F,A),!.
functor_safe0(M:P,M:F,A):-var(P),atom(M),functor_catch(P,F,A),!,warn_bad_functor(F).
functor_safe0(P,F,A):-var(P),strip_f_module(F,F0),functor_catch(P,F0,A),!,warn_bad_functor(F).
functor_safe0(P,F,A):-compound(P),!,functor_safe_compound(P,F,A),warn_bad_functor(F).
functor_safe0(P,F,0):- hotrace(string(P);atomic(P)), atom_string(F,P),warn_bad_functor(F).
functor_safe_compound((_,_),',',2).
functor_safe_compound([_|_],'.',2).
functor_safe_compound(_:P,F,A):- functor_catch(P,F,A),!.
functor_safe_compound(P,F,A):- functor_catch(P,F,A).
functor_safe_compound(P,F,A):- var(F),strip_f_module(P,P0),!,functor_catch(P0,F0,A),strip_f_module(F0,F),!.
functor_safe_compound(P,F,A):- strip_f_module(P,P0),strip_f_module(F,F0),!,functor_catch(P0,F0,A).
*/

% block(test, (repeat, !(test), fail))).
:- meta_predicate block(+, :, ?). 

%= 	 	 

%% block( +Name, ?Goal, ?Var) is semidet.
%
% Block.
%
block(Name, Goal, Var) :- Goal, keep(Name, Var).	% avoid last-call and GC 

%= 	 	 

%% keep( ?VALUE1, ?VALUE2) is semidet.
%
% Keep.
%
keep(_, _).  

%= 	 	 

%% set_block_exit( ?Name, ?Value) is semidet.
%
% Set Block Exit.
%
set_block_exit(Name, Value) :-  prolog_current_frame(Frame),  prolog_frame_attribute(Frame, parent_goal,  mcall:block(Name, _, Value)). 

%= 	 	 

%% block( ?Name, ?Goal) is semidet.
%
% Block.
%
block(Name, Goal) :-  block(Name, Goal, Var),  (   Var == !  ->  !  ;   true  ). 

%= 	 	 

%% !( ?Name) is semidet.
%
% !.
%
!(Name) :- set_block_exit(Name, !). 

:- export((block/3, 
            set_block_exit/2, 
            block/2, 
            !/1 )).

:- dynamic(buggerFile/1).
:- abolish(buggerFile/1),prolog_load_context(source,D),asserta(buggerFile(D)).


% hasLibrarySupport :- absolute_file_name('logicmoo_util_library.pl',File),exists_file(File).


%= 	 	 

%% throwNoLib is semidet.
%
% Throw No Lib.
%
throwNoLib:- trace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(user:library_directory), trace_or_throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

:- dynamic(buggerDir/1).
:- abolish(buggerDir/1),prolog_load_context(directory,D),asserta(buggerDir(D)).


%= 	 	 

%% addLibraryDir is semidet.
%
% Add Library Dir.
%
addLibraryDir :- buggerDir(Here),atom_concat(Here,'/..',UpOne), absolute_file_name(UpOne,AUpOne),asserta(user:library_directory(AUpOne)).

% if not has library suport, add this direcotry as a library directory
% :-not(hasLibrarySupport) -> addLibraryDir ; true .

% :-hasLibrarySupport->true;throwNoLib.





%= 	 	 

%% ib_multi_transparent33( ?MT) is semidet.
%
% Ib Multi Transparent33.
%
ib_multi_transparent33(MT):-multifile(MT),module_transparent(MT),dynamic_safe(MT).


%= 	 	 

%% dif_safe( ?Agent, ?Obj) is semidet.
%
% Dif Safely Paying Attention To Corner Cases.
%
dif_safe(Agent,Obj):- (var(Agent);var(Obj)),!.
dif_safe(Agent,Obj):- Agent\==Obj.

% hide Pred from tracing

%= 	 	 

%% to_m_f_arity_pi( ?Term, ?M, ?F, ?A, ?PI) is semidet.
%
% Converted To Module Functor Arity Predicate Indicator.
%
to_m_f_arity_pi(M:Plain,M,F,A,PI):-!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(Term,M,F,A,PI):- strip_module(Term,M,Plain),Plain\==Term,!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(F/A,_M,F,A,PI):-functor_safe(PI,F,A),!.
to_m_f_arity_pi(PI,_M,F,A,PI):-functor_safe(PI,F,A).


%= 	 	 

%% with_preds( ?H, ?M, ?F, ?A, ?PI, :GoalGoal) is semidet.
%
% Using Predicates.
%
with_preds((H,Y),M,F,A,PI,Goal):-!,with_preds(H,M,F,A,PI,Goal),with_preds(Y,M,F,A,PI,Goal).
with_preds([H],M,F,A,PI,Goal):-!,with_preds(H,M,F,A,PI,Goal).
with_preds([H|Y],M,F,A,PI,Goal):-!,with_preds(H,M,F,A,PI,Goal),with_preds(Y,M,F,A,PI,Goal).
with_preds(M:H,_M,F,A,PI,Goal):-!, with_preds(H,M,F,A,PI,Goal).
with_preds(H,M,F,A,PI,Goal):-forall(to_m_f_arity_pi(H,M,F,A,PI),Goal).



% ===================================================================
% Substitution based on ==
% ===================================================================
% Usage: dbgsubst(+Fml,+Goal,+Sk,?FmlSk)

:- export(dbgsubst/4).

%= 	 	 

%% dbgsubst( ?A, ?B, ?Goal, ?A) is semidet.
%
% Dbgsubst.
%
dbgsubst(A,B,Goal,A):- B==Goal,!.
dbgsubst(A,B,Goal,D):-var(A),!,ddmsg(dbgsubst(A,B,Goal,D)),dumpST,dtrace(dbgsubst0(A,B,Goal,D)).
dbgsubst(A,B,Goal,D):-dbgsubst0(A,B,Goal,D).


%= 	 	 

%% dbgsubst0( ?A, ?B, ?Goal, ?D) is semidet.
%
% Dbgsubst Primary Helper.
%
dbgsubst0(A,B,Goal,D):- 
      catchv(hotrace(nd_dbgsubst(A,B,Goal,D)),E,(dumpST,ddmsg(E:nd_dbgsubst(A,B,Goal,D)),fail)),!.
dbgsubst0(A,_B,_C,A).


%= 	 	 

%% nd_dbgsubst( ?Var, ?VarS, ?SUB, ?SUB) is semidet.
%
% Nd Dbgsubst.
%
nd_dbgsubst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_dbgsubst(  P, Goal,Sk, P1 ) :- functor_safe(P,_,N),nd_dbgsubst1( Goal, Sk, P, N, P1 ).


%= 	 	 

%% nd_dbgsubst1( ?Goal, ?Sk, ?P, ?N, ?P1) is semidet.
%
% Nd Dbgsubst Secondary Helper.
%
nd_dbgsubst1( _,  _, P, 0, P  ).
nd_dbgsubst1( Goal, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_dbgsubst2( Goal, Sk, Args, ArgS ),
            nd_dbgsubst2( Goal, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].


%= 	 	 

%% nd_dbgsubst2( ?X, ?Sk, ?L, ?L) is semidet.
%
% Nd Dbgsubst Extended Helper.
%
nd_dbgsubst2( _,  _, [], [] ).
nd_dbgsubst2( Goal, Sk, [A|As], [Sk|AS] ) :- Goal == A, !, nd_dbgsubst2( Goal, Sk, As, AS).
nd_dbgsubst2( Goal, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_dbgsubst2( Goal, Sk, As, AS).
nd_dbgsubst2( Goal, Sk, [A|As], [Ap|AS] ) :- nd_dbgsubst( A,Goal,Sk,Ap ),nd_dbgsubst2( Goal, Sk, As, AS).
nd_dbgsubst2( _X, _Sk, L, L ).



%=========================================
% Module Utils
%=========================================

%= 	 	 

%% module_functor( ?PredImpl, ?Module, ?Pred, ?Arity) is semidet.
%
% Module Functor.
%
module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).


%= 	 	 

%% strip_arity( ?PredImpl, ?Pred, ?Arity) is semidet.
%
% Strip Arity.
%
strip_arity(Pred/Arity,Pred,Arity).
strip_arity(PredImpl,Pred,Arity):-functor_safe(PredImpl,Pred,Arity).

/*

debug(+Topic, +Format, +Arguments)
Prints a message using format(Format, Arguments) if Topic unies with a topic
enabled with debug/1.
debug/nodebug(+Topic [>le])
Enables/disables messages for which Topic unies. If >le is added, the debug
messages are appended to the given le.
assertion(:Goal)
Assumes that Goal is true. Prints a stack-dump and traps to the debugger otherwise.
This facility is derived from the assert() macro as used in Goal, renamed
for obvious reasons.
*/
:- meta_predicate with_preds(?,?,?,?,?,0).


:- export(nop/1).

%= 	 	 

%% nop( ?VALUE1) is semidet.
%
% Nop.
%
nop(_).
%set_prolog_flag(N,V):-!,nop(set_prolog_flag(N,V)).


% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- set_prolog_flag(generate_debug_info, true).
% have to load this module here so we dont take ownership of prolog_exception_hook/4.

% :- ensure_loaded(library(backcomp)).
:- ensure_loaded(library(ansi_term)).
:- ensure_loaded(library(check)).
:- ensure_loaded(library(debug)).
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(make)).
:- ensure_loaded(library(prolog_stack)).
:- ensure_loaded(library(system)).
:- ensure_loaded(library(apply)).

:- thread_local(t_l:session_id/1).
:- multifile(t_l:session_id/1).

:- thread_local(tlbugger:no_colors/0).


% =========================================================================


%= 	 	 

%% trace_or_throw( ?E) is semidet.
%
%  Trace or throw.
%
trace_or_throw(E):- non_user_console,notrace((thread_self(Self),wdmsg(thread_trace_or_throw(Self+E)),!,throw(abort),thread_exit(trace_or_throw(E)))).
trace_or_throw(E):- wdmsg(E),dtrace_msg(throw:-E).

 %:-interactor.



:-export(on_x_fail/1).

%= 	 	 

%% on_x_fail( :GoalGoal) is semidet.
%
% If there If Is A an exception in  :Goal goal then fail.
%
on_x_fail(Goal):- catchv(Goal,_,fail).


% false = hide this wrapper

%= 	 	 

%% showHiddens is semidet.
%
% Show Hiddens.
%
showHiddens:-true.

:- meta_predicate on_x_log_fail(0).
:- export(on_x_log_fail/1).

%= 	 	 

%% on_x_log_fail( :GoalGoal) is semidet.
%
% If there If Is A an exception in  :Goal goal then log fail.
%
on_x_log_fail(Goal):- catchv(Goal,E,(dmsg(E:Goal),fail)).


% -- CODEBLOCK

:- export(on_x_log_throw/1).
:- export(on_x_log_cont/1).

%= 	 	 

%% on_x_log_throw( :GoalGoal) is semidet.
%
% If there If Is A an exception in  :Goal goal then log throw.
%
on_x_log_throw(Goal):- catchv(Goal,E,(ddmsg(on_x_log_throw(E,Goal)),throw(E))).
%on_x_log_throwEach(Goal):-with_each(1,on_x_log_throw,Goal).

%= 	 	 

%% on_x_log_cont( :GoalGoal) is semidet.
%
% If there If Is A an exception in  :Goal goal then log cont.
%
on_x_log_cont(Goal):- catchv((Goal*->true;ddmsg(failed_on_x_log_cont(Goal))),E,ddmsg(E:Goal)).

:- thread_local( tlbugger:skipMust/0).
%MAIN tlbugger:skipMust.


:- export(errx/0).

%= 	 	 

%% errx is semidet.
%
% Errx.
%
errx:-on_x_debug((ain(tlbugger:dont_skip_bugger),do_gc,dumpST(10))),!.

% false = use this wrapper, true = code is good and avoid using this wrapper
:- export(skipWrapper/0).

:- thread_local(tlbugger:rtracing/0).

%skipWrapper:-!,fail.

%= 	 	 

%% skipWrapper is semidet.
%
% Skip Wrapper.
%
skipWrapper:- tracing, \+ tlbugger:rtracing,!.
skipWrapper:- tlbugger:dont_skip_bugger,!,fail.
skipWrapper:- tlbugger:skip_bugger,!.
%skipWrapper:- 0 is random(5),!.
%skipWrapper:- tlbugger:skipMust,!.


:- thread_local( tlbugger:old_no_repeats/0).
:- thread_local( tlbugger:skip_bugger/0).
:- thread_local( tlbugger:dont_skip_bugger/0).
%:-thread_local( tlbugger:bugger_prolog_flag/2).

%MAIN tlbugger:skip_bugger.


% = :- meta_predicate(one_must(0,0)).

%= 	 	 

%% one_must( :GoalMCall, :GoalOnFail) is semidet.
%
% One Must Be Successfull.
%
one_must(MCall,OnFail):-  MCall *->  true ; OnFail.



%= 	 	 

%% must_det( :GoalGoal) is semidet.
%
% Must Be Successfull Deterministic.
%
must_det(Goal):- must(Goal),!.


%= 	 	 

%% one_must_det( :GoalGoal, :GoalOnFail) is semidet.
%
% One Must Be Successfull Deterministic.
%
one_must_det(Goal,_OnFail):-Goal,!.
one_must_det(_Call,OnFail):-OnFail,!.


%= 	 	 

%% must_det( :GoalGoal, :GoalOnFail) is semidet.
%
% Must Be Successfull Deterministic.
%
must_det(Goal,OnFail):- trace_or_throw(deprecated(must_det(Goal,OnFail))),Goal,!.
must_det(_Call,OnFail):-OnFail.

:- module_transparent(must_det_l/1).

%= 	 	 

%% must_det_l( :GoalMGoal) is semidet.
%
% Must Be Successfull Deterministic (list Version).
%
must_det_l(MGoal):- strip_module(MGoal,M,Goal),!, must_det_lm(M,Goal).

:- module_transparent(must_det_lm/2).

%= 	 	 

%% must_det_lm( ?M, :TermGoal) is semidet.
%
% Must Be Successfull Deterministic Lm.
%
must_det_lm(M,Goal):-var(Goal),trace,trace_or_throw(var_must_det_l(M:Goal)),!.
must_det_lm(M,[Goal]):-!,must_det_lm(M,Goal).
must_det_lm(M,[Goal|List]):-!,must(M:Goal),!,must_det_lm(M,List).
must_det_lm(M,Goal):-tlbugger:skip_bugger,!,M:Goal.
must_det_lm(M,(Goal,List)):-!,must(M:Goal),!,must_det_lm(M,List).
must_det_lm(M,Goal):- must(M:Goal),!.
must_det_lm(_,[]):-!.
must_det_lm(M,[Goal|List]):-!,must(M:Goal),!,must_det_lm(M,List).

:- module_transparent(det_lm/2).

%= 	 	 

%% det_lm( ?M, ?Goal) is semidet.
%
% Deterministic Lm.
%
det_lm(M,(Goal,List)):- !,Goal,!,det_lm(M,List).
det_lm(M,Goal):-M:Goal,!.

:- module_transparent(must_l/1).

%= 	 	 

%% must_l( :GoalGoal) is semidet.
%
% Must Be Successfull (list Version).
%
must_l(Goal):-var(Goal),trace_or_throw(var_must_l(Goal)),!.
must_l((A,!,B)):-!,must(A),!,must_l(B).
must_l((A,B)):-!,must((A,(deterministic(true)->(!,must_l(B));B))).
must_l(Goal):- must(Goal).


:- thread_local tlbugger:skip_use_slow_sanity/0.
:- asserta((tlbugger:skip_use_slow_sanity:-!)).

% thread locals should defaults to false  tlbugger:skip_use_slow_sanity.


%= 	 	 

%% slow_sanity( :GoalGoal) is semidet.
%
% Slow Optional Sanity Checking.
%
slow_sanity(Goal):- ( tlbugger:skip_use_slow_sanity ; sanity(Goal)),!.


% -- CODEBLOCK
:- export(sanity/1).
% = :- meta_predicate(sanity(0)).

% sanity is used for type checking (is not required)
% sanity(Goal):-!.

% sanity(_):-skipWrapper,!.

%= 	 	 

%% sanity( :GoalGoal) is semidet.
%
% Optional Sanity Checking.
%
sanity(_):- is_release,!.
sanity(Goal):- bugger_flag(release,true),!,assertion(Goal).
sanity(Goal):- tlbugger:show_must_go_on,!,ignore(show_failure(why,Goal)).
sanity(Goal):- ignore(must(show_failure(why,Goal))).


:- export(is_release/0).

%= 	 	 

%% is_release is semidet.
%
% If Is A Release.
%
is_release:- !,fail.
is_release :- \+ not_is_release.
:- export(not_is_release/0).

%= 	 	 

%% not_is_release is semidet.
%
% Not If Is A Release.
%
not_is_release:- 1 is random(4).
not_is_release:- \+ is_release.



:- thread_local tlbugger:show_must_go_on/0.

%= 	 	 

%% badfood( ?MCall) is semidet.
%
% Badfood.
%
badfood(MCall):- numbervars(MCall,0,_,[functor_name('VAR_______________________x0BADF00D'),attvar(bind),singletons(false)]),dumpST.

% -- CODEBLOCK
:- export(without_must/1).
% = :- meta_predicate(without_must(0)).


%= 	 	 

%% without_must( :GoalGoal) is semidet.
%
% Without Must Be Successfull.
%
without_must(Goal):- w_tl(tlbugger:skipMust,Goal).

% -- CODEBLOCK
:- export(y_must/2).
:- meta_predicate (y_must(?,0)).

%= 	 	 

%% y_must( ?Y, :GoalGoal) is semidet.
%
% Y Must Be Successfull.
%
y_must(Y,Goal):- catchv(Goal,E,(wdmsg(E:must_xI__xI__xI__xI__xI_(Y,Goal)),fail)) *-> true ; dtrace(y_must(Y,Goal)).

% -- CODEBLOCK
:- export(must/1).
:- meta_predicate (must(0)).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(200), attributes(portray)]).
:- set_prolog_flag(debugger_show_context,true).

:- meta_predicate(must(0)).
%must(Call):-(repeat, (catchv(Call,E,(dmsg(E:Call),debug,fail)) *-> true ; (ignore(ftrace(Call)),leash(+all),repeat,wdmsg(failed(Call)),trace,Call)),!).

%= 	 	 

%% must( :GoalGoal) is semidet.
%
% Must Be Successfull.
%
must(Goal):-  notrace((must_be(nonvar,Goal),get_must(Goal,MGoal))),!,MGoal.


%= 	 	 

%% get_must( ?Goal, ?CGoal) is semidet.
%
% Get Must Be Successfull.
%
get_must(notrace(Goal),CGoal):- !,get_must(Goal,CGoal).
get_must(M:notrace(Goal),CGoal):- !,get_must(M:Goal,CGoal).
% get_must(notrace(Goal),CGoal):- !,get_must((notrace(Goal)*->true;Goal),CGoal).
get_must(Goal,CGoal):-  (is_release;tlbugger:skipMust),!,CGoal = Goal.
get_must(Goal,CGoal):- skipWrapper,!, CGoal = (Goal *-> true ; ((ddmsg(failed_FFFFFFF(must(Goal))),dumpST,trace,Goal))).
get_must(Goal,CGoal):- tlbugger:show_must_go_on,!,
 CGoal = ((catchv(Goal,E,
     notrace(((dumpST,ddmsg(error,sHOW_MUST_go_on_xI__xI__xI__xI__xI_(E,Goal))),badfood(Goal))))
            *-> true ; notrace((dumpST,wdmsg(error,sHOW_MUST_go_on_failed_F__A__I__L_(Goal)),badfood(Goal))))).

get_must(Goal,CGoal):- !, (CGoal = (on_x_rtrace(Goal) *-> true; debugCallWhy(failed(on_f_debug(Goal)),Goal))).
get_must(Goal,CGoal):- !, CGoal = (catchv(Goal,E,(notrace,ddmsg(eXXX(E,must(Goal))),rtrace(Goal),trace,!,throw(E))) *-> true ; ((ddmsg(failed(must(Goal))),trace,Goal))).
get_must(Goal,CGoal):-    
   (CGoal = (catchv(Goal,E,
     (dumpST,ddmsg(error,must_xI_(E,Goal)),set_prolog_flag(debug_on_error,true),
         ignore_each((rtrace(Goal),nortrace,trace,dtrace(Goal),badfood(Goal)))))
         *-> true ; (dumpST,ignore_each(((trace,dtrace(must_failed_F__A__I__L_(Goal),Goal),badfood(Goal))))))).

:- 'mpred_trace_none'(ddmsg(_)).
:- 'mpred_trace_none'(ddmsg(_,_)).

:- source_location(S,_),prolog_load_context(module,M),forall(source_file(M:H,S),(functor(H,F,A),M:module_transparent(M:F/A),M:export(M:F/A))).


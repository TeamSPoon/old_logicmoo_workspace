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
            get_main_error_stream/1,
            get_thread_current_error/1,
            source_variables_l/1,
            as_clause_no_m/3,
            as_clause_w_m/4,
            as_clause_w_m/5,
            source_module/1,
            bad_functor/1,
            current_why/1,
            badfood/1,
            unsafe_safe/2,
            quietly/1,
            doall_and_fail/1,
            quietly_must/1,
            on_x_f/3,
            hide_trace/1,
            block/2,
            block/3,
            with_current_why/2,
            bubbled_ex/1,
            bubbled_ex_check/1,
            catchv/3,
            catchvvnt/3,
            current_source_file/1,current_source_location0/1,
            lmcache:current_main_error_stream/1,
            lmcache:thread_current_input/2,
            dbgsubst/4,            
            dbgsubst0/4,
            ddmsg/1,
            ddmsg/2,
            ddmsg_call/1,            
            det_lm/2,
            dif_safe/2,
            dumpST_error/1,
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
            input_key/1,
            is_ftCompound/1,
            not_ftCompound/1,
            is_ftNameArity/2,
            is_ftNonvar/1,
            is_ftVar/1,
            
            is_main_thread/0,
            is_pdt_like/0,
            is_release/0,
            need_speed/0,
            allow_unsafe_code/0,
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
            get_thread_current_error/1,
            throwNoLib/0,
            to_m_f_arity_pi/5,
            to_pi/2,
            to_pi0/3,
            warn_bad_functor/1,
            when_defined/1,
            with_main_error_to_output/1,
            with_current_io/1,
            with_error_to_main/1,
            with_dmsg_to_main/1,
            with_main_input/1,
            with_main_io/1,
            with_preds/6,
            without_must/1,
            non_user_console/0,
            y_must/2


          ]).




%% non_user_console is semidet.
%
% Not User Console.
%
non_user_console:-thread_self(main),!,fail.
non_user_console:-current_input(In),stream_property(In,tty(true)),!,fail.
non_user_console:-current_prolog_flag(debug_threads,true),!,fail.
non_user_console:-current_input(In),stream_property(In, close_on_abort(true)).
non_user_console:-current_input(In),stream_property(In, close_on_exec(true)).



:- if(\+ current_predicate(system:nop/1)).
:- system:ensure_loaded(system:logicmoo_util_supp).
:- endif.



:- meta_predicate


		block(+, :, ?),
		catchv(0, ?, 0),
		catchvvnt(0, ?, 0),
		catchv(0, ?, 0),
		
		if_defined(+),
		if_defined(+, 0),
		ddmsg_call(0),
		on_x_fail(0),
		on_x_log_throw(0),
		
		
		on_x_log_cont(0),
		on_x_log_fail(0),
		if_defined(+, 0),        

        must(0),
        must_det(0),
        must_det(0, 0),
        must_det_l(0),
        must_l(0),
        one_must(0, 0),
        one_must_det(0, 0),
        unsafe_safe(0,0),
        sanity(0),
        slow_sanity(0),
        to_pi(?, ?),
        when_defined(:),
        with_main_error_to_output(0),
        with_current_io(0),
        with_dmsg_to_main(0),
        with_error_to_main(0),
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
        lmcache:current_main_error_stream/1,
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
        with_current_why/2,
        get_must/2,
        ib_multi_transparent33/1,
        input_key/1,
        is_ftCompound/1,
        not_ftCompound/1,
        is_ftNameArity/2,
        is_ftNonvar/1,
        is_ftVar/1,
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
        not_is_release/0,
        save_streams/0,
        save_streams/1,
        set_block_exit/2,
        showHiddens/0,
        show_new_src_location/1,
        show_new_src_location/2,
        show_source_location/0,
        skipWrapper/0,
        skipWrapper0/0,
        strip_arity/3,
        strip_f_module/2,
        get_thread_current_error/1,
        throwNoLib/0,
        to_m_f_arity_pi/5,
        to_pi0/3,
        warn_bad_functor/1.

:- meta_predicate
   doall_and_fail(0),
   quietly_must(0).


:- include('logicmoo_util_header.pi').

/** <module> logicmoo_util_catch - catch-like bocks

   Tracer modes:

   quietly/1 - turn off tracer if already on but still dtrace on failure
   must/1 - dtrace on failure
   rtrace/1 - non interactive debug
   sanity/1 - run in quietly/1 when problems were detected previously otherwise skippable slow_sanity/1+hide_trace/1
   assertion/1 - throw on failure
   hide_trace/1 - hide dtrace temporarily
   slow_sanity/1 - skip unless in developer mode

*/

:- thread_local( tlbugger:old_no_repeats/0).
:- thread_local( tlbugger:skip_bugger/0).
:- thread_local( tlbugger:dont_skip_bugger/0).

% = :- meta_predicate(catchvvnt(0,?,0)).

%= 	 	 

%% catchvvnt( :GoalT, ?E, :GoalF) is semidet.
%
% Catchvvnt.
%
catchvvnt(T,E,F):-catchv(hotrace(T),E,F).

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
:- thread_local(t_l:thread_local_error_stream/1).
:- volatile(t_l:thread_local_error_stream/1).

:- is_pdt_like-> assert(tlbugger:no_colors); true.


% = :- meta_predicate(with_main_error_to_output(0)).

%= 	 	 

%% with_main_error_to_output( :GoalGoal) is semidet.
%
% Using Main Error Converted To Output.
%
with_main_error_to_output(Goal):-
 current_output(Out),
  w_tl(t_l:thread_local_error_stream(Out),Goal).
   

with_current_io(Goal):-
  current_input(IN),current_output(OUT),get_thread_current_error(Err),  
  setup_call_cleanup_each(set_prolog_IO(IN,OUT,Err),Goal,set_prolog_IO(IN,OUT,Err)).


with_dmsg_to_main(Goal):-
  get_main_error_stream(Err),current_error(ErrWas),Err=ErrWas,!,Goal.
with_dmsg_to_main(Goal):-
  get_main_error_stream(Err),current_error(ErrWas),
  current_input(IN),current_output(OUT),
   w_tl(t_l:thread_local_error_stream(Err),
   setup_call_cleanup_each(set_prolog_IO(IN,OUT,Err),Goal,set_prolog_IO(IN,OUT,ErrWas))).
   
with_error_to_main(Goal):-
  get_main_error_stream(Err),current_error(ErrWas),Err=ErrWas,!,Goal.
with_error_to_main(Goal):-
  get_main_error_stream(Err),get_thread_current_error(ErrWas),
  current_input(IN),current_output(OUT),
   w_tl(t_l:thread_local_error_stream(Err),
   setup_call_cleanup_each(set_prolog_IO(IN,OUT,Err),Goal,set_prolog_IO(IN,OUT,ErrWas))).
   
   



%% get_thread_current_error( ?Err) is det.
%
% Thread Current Error Stream.
%
get_thread_current_error(Err):- t_l:thread_local_error_stream(Err),!.
get_thread_current_error(Err):- thread_self(ID),lmcache:thread_current_error_stream(ID,Err),!.
get_thread_current_error(Err):- stream_property(Err,alias(user_error)),!.
get_thread_current_error(Err):- get_main_error_stream(Err),!.

%% get_main_error_stream( ?Err) is det.
%
% Current Main Error Stream.
%
get_main_error_stream(Err):- stream_property(Err,alias(main_error)),!.
get_main_error_stream(Err):- lmcache:thread_main(user,ID),lmcache:thread_current_error_stream(ID,Err).
get_main_error_stream(Err):- t_l:thread_local_error_stream(Err),!.
get_main_error_stream(Err):- stream_property(Err,alias(user_error)),!.


%= 	 	 

%% format_to_error( ?F, ?A) is semidet.
%
% Format Converted To Error.
%
format_to_error(F,A):-get_main_error_stream(Err),!,format(Err,F,A).

%= 	 	 

%% fresh_line_to_err is semidet.
%
% Fresh Line Converted To Err.
%
fresh_line_to_err:- notrace((flush_output_safe,get_main_error_stream(Err),format(Err,'~N',[]),flush_output_safe(Err))).

:- dynamic(lmcache:thread_current_input/2).
:- volatile(lmcache:thread_current_input/2).

:- dynamic(lmcache:thread_current_error_stream/2).
:- volatile(lmcache:thread_current_error_stream/2).

%= 	 	 

%% save_streams is semidet.
%
% Save Streams.
%
save_streams:- thread_self(ID), save_streams(ID).


set_main_error:- (thread_self(main)->((stream_property(Err, alias(user_error)),set_stream(Err,alias(main_error))),
       (stream_property(In, alias(user_input)),set_stream(In,alias(main_input))),
       (stream_property(Out, alias(user_output)),set_stream(Out,alias(main_output))),
                                    set_stream(Err,alias(current_error)));true).

:- initialization(set_main_error).
:- initialization(set_main_error,restore).

%= 	 	 

%% save_streams( ?ID) is semidet.
%
% Save Streams.
%
save_streams(ID):- 
  retractall((lmcache:thread_current_input(ID,_))),
  retractall((lmcache:thread_current_error_stream(ID,_))),
  current_input(In),asserta(lmcache:thread_current_input(ID,In)),
  thread_at_exit(retractall((lmcache:thread_current_input(ID,_)))),
  thread_at_exit(retractall((lmcache:thread_current_error_stream(ID,_)))),
  (stream_property(Err, alias(user_error));current_error(Err)),
              asserta(lmcache:thread_current_error_stream(ID,Err)).


:- meta_predicate(with_main_input(0)).

%% with_main_input( :GoalGoal) is semidet.
%
% Using Main Input.
%
with_main_input(Goal):-
    current_output(OutPrev),current_input(InPrev),stream_property(ErrPrev,alias(user_error)),
    lmcache:thread_main(user,ID),lmcache:thread_current_input(ID,In),lmcache:thread_current_error_stream(ID,Err),
    setup_call_cleanup_each(set_prolog_IO(In,OutPrev,Err),Goal,set_prolog_IO(InPrev,OutPrev,ErrPrev)).


%= 	 	 

%% with_main_io( :GoalGoal) is semidet.
%
% Using Main Input/output.
%
 with_main_io(Goal):-
    current_output(OutPrev),
    current_input(InPrev),
    stream_property(ErrPrev,alias(user_error)),
    lmcache:thread_main(user,ID),
     lmcache:thread_current_input(ID,In),
       lmcache:thread_current_error_stream(ID,Err),
    setup_call_cleanup_each(set_prolog_IO(In,Err,Err),Goal,set_prolog_IO(InPrev,OutPrev,ErrPrev)).


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



%% doall_and_fail( :GoalCall) is semidet.
%
% Doall And Fail.
%
doall_and_fail(Call):- time_call(once(doall(Call))),fail.

quietly_must(G):- /*no_trace*/(must(G)).


:- meta_predicate if_defined(+).
:- export(if_defined/1).

%% if_defined( ?G) is semidet.
%
% If Defined.
%
if_defined(Goal):- if_defined(Goal,((dmsg(warn_undefined(Goal))),!,fail)).

:- meta_predicate if_defined(+,0).
:- export(if_defined/2).

%% if_defined( ?Goal, :GoalElse) is semidet.
%
% If Defined Else.
%
if_defined(Goal,_Else):- current_predicate(_,Goal),!,Goal.
if_defined(_:Goal,Else):- !, current_predicate(_,OM:Goal)->OM:Goal;Else.
if_defined(Goal,  Else):- current_predicate(_,OM:Goal)->OM:Goal;Else.





:- meta_predicate when_defined(:).
:- export(when_defined/1).

%= 	 	 

%% when_defined( ?Goal) is semidet.
%
% When Defined.
%
when_defined(Goal):-if_defined(Goal,true).

:- if(current_predicate(run_sanity_tests/0)).
:- listing(lmcache:thread_current_error_stream/2).
:- endif.

% = :- meta_predicate(to_pi(?,?)).

%= 	 	 

%% to_pi( ?P, ?M) is semidet.
%
% Converted To Predicate Indicator.
%
to_pi(P,M:P):-var(P),!,current_module(M).
to_pi(M:P,M:P):-var(P),!,current_module(M).
to_pi(Find,(M:PI)):- 
 w_tl(set_prolog_flag(retry_undefined,false),
   (once(catch(match_predicates(Find,Found),_,fail)),Found=[_|_],!,member(M:F/A,Found),functor(PI,F,A))).
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
sl_to_filename(W,W):-atom(W),exists_file(W),!.
sl_to_filename(W,W):-atom(W),!.
sl_to_filename(_:W,W):-atom(W),!.
sl_to_filename(mfl(_,F,_),F):-atom(F),!.
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
current_source_location0(module(M)):- '$current_typein_module'(M).

:-export(current_why/1).
:-module_transparent(current_why/1).

%= 	 	 

%% current_why( ?Why) is semidet.
%
% Current Generation Of Proof.
%
current_why(Why):- t_l:current_local_why(Why,_),!.
current_why(mfl(M,F,L)):- current_source_file(F:L),var(L),F= module(M),!.
current_why(mfl(M,F,L)):- source_module(M),call(ereq,mtCycL(M)),current_source_file(F:L),!.
current_why(mfl(M,F,L)):- call(ereq,defaultAssertMt(M)),current_source_file(F:L),!.


%% with_current_why( +Why, +:Prolog) is semidet.
%
% Save Well-founded Semantics Reason while executing code.
%
with_current_why(Why,Prolog):- w_tl(t_l:current_local_why(Why,Prolog),Prolog).


% source_module(M):-!,M=u.
:-export(source_module/1).

%= 	 	 

%% source_module( ?M) is semidet.
%
% Source Module.
%
source_module(M):-nonvar(M),!,source_module(M0),!,must(M0=M).
source_module(M):-'$current_source_module'(M),!.
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
source_file0(F):-seeing(S),is_stream(S),stream_property(S,file_name(F)),exists_file(F).
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
 hotrace((
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (get_varname_list(Vs2);Vs2=[]),
  hotrace(catch((parent_goal('$toplevel':'$execute_goal2'(_, Vs3),_);Vs3=[]),E,(writeq(E),Vs3=[]))),
  ignore(Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  set_varname_list( AllS))).



%= 	 	 


:-export( show_source_location/0).
%show_source_location:- hotrace((tlbugger:no_slow_io)),!.
%show_source_location:- is_hiding_dmsgs,!.

%= 	 	 

%% show_source_location is semidet.
%
% Show Source Location.
%
show_source_location:- source_location(F,L),!,show_new_src_location(F:L),!.
show_source_location:- current_source_file(FL),sanity(nonvar(FL)),!,show_new_src_location(FL),!.
show_source_location:- dumpST,dtrace.


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

%% is_ftNameArity(+F,+A) is semidet.
%
% If Is A Format Type of a Compound specifier
%
is_ftNameArity(F,A):-integer(A), atom(F), (F \= (/)),A>=0.      

%% is_ftCompound( ?Goal) is semidet.
%
% If Is A Format Type Compound.
%
is_ftCompound(Goal):-compound(Goal),\+ is_ftVar(Goal).

%% not_ftCompound( ?InOut) is semidet.
%
% Not Compound.
%
not_ftCompound(A):- is_ftVar(A) -> true ; \+ is_ftCompound(A).

:- export(is_ftVar/1).

%= 	 	 

%% is_ftVar( :TermV) is semidet.
%
% If Is A Format Type Variable.
%
is_ftVar(V):-var(V),!.
is_ftVar('$VAR'(_)).
is_ftVar('$VAR'(_,_)).
is_ftVar('avar'(_)).
is_ftVar('avar'(_,_)).
%:- mpred_trace_nochilds(is_ftVar/1).


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
warn_bad_functor(L):-ignore((hotrace(bad_functor(L)),!,dtrace,nop(ddmsg(bad_functor(L))))).

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
% functor_catch(P,F,A):-catchv(compound_name_arity(P,F,A),E,(ddmsg(E:functor(P,F,A)),dtrace)).


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
throwNoLib:- dtrace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(user:library_directory), trace_or_throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

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
trace_or_throw(E):- non_user_console,hotrace((thread_self(Self),wdmsg(thread_trace_or_throw(Self+E)),!,throw(abort),
                    thread_exit(trace_or_throw(E)))).
trace_or_throw(E):- wdmsg(E),dtrace((dtrace,throw(E))).

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
on_x_log_cont(Goal):- catchv( (Goal*->true;ddmsg(failed_on_x_log_cont(Goal))),E,ddmsg(E:Goal)).

:- thread_local( tlbugger:skipMust/0).
%MAIN tlbugger:skipMust.


:- export(errx/0).

%= 	 	 

%% errx is semidet.
%
% Errx.
%
errx:-on_x_debug((ain(tlbugger:dont_skip_bugger),do_gc,dumpST(10))),!.

:- thread_local(tlbugger:rtracing/0).

%skipWrapper:-!,fail.

%= 	 	 

%% skipWrapper is semidet.
%
% Skip Wrapper.
%

% false = use this wrapper, true = code is good and avoid using this wrapper
:- export(skipWrapper/0).

skipWrapper:- notrace((skipWrapper0)).
:- export(skipWrapper0/0).
% skipWrapper:- tracing,!.
skipWrapper0:- tracing, \+ tlbugger:rtracing,!.
skipWrapper0:- tlbugger:dont_skip_bugger,!,fail.
skipWrapper0:- current_prolog_flag(logicmoo_debug,true),!,fail.
skipWrapper0:- current_prolog_flag(unsafe_speedups,true),!.
skipWrapper0:- tlbugger:skip_bugger,!.
skipWrapper0:- is_release,!.
%skipWrapper0:- 1 is random(5),!.
%skipWrapper0:- tlbugger:skipMust,!.

:- '$hide'(skipWrapper/0).

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
must_det_lm(M,Goal):-var(Goal),trace_or_throw(var_must_det_l(M:Goal)),!.
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
must_l(Goal):- skipWrapper,!,call(Goal).
must_l(Goal):-var(Goal),trace_or_throw(var_must_l(Goal)),!.
must_l((A,!,B)):-!,must(A),!,must_l(B).
must_l((A,B)):-!,must((A,deterministic(Det),true,(Det==true->(!,must_l(B));B))).
must_l(Goal):- must(Goal).


:- thread_local tlbugger:skip_use_slow_sanity/0.
:- asserta((tlbugger:skip_use_slow_sanity:-!)).

% thread locals should defaults to false  tlbugger:skip_use_slow_sanity.


%= 	 	 

%% slow_sanity( :GoalGoal) is semidet.
%
% Slow Optional Sanity Checking.
%
slow_sanity(Goal):- ( tlbugger:skip_use_slow_sanity ; must(Goal)),!.


:- meta_predicate(hide_trace(0)).

hide_trace(G):- \+ tracing,!,call(G).
hide_trace(G):- !,call(G).
hide_trace(G):- skipWrapper,!,call(G).
hide_trace(G):- 
 restore_trace((
   hotrace(
      ignore((tracing,
      visible(-all),
      visible(-unify),
      visible(+exception),
      thread_leash(-all),
      thread_leash(+exception)))),G)).

:- meta_predicate(on_x_f(0,0,0)).
on_x_f(G,X,F):-catchv(G,E,(dumpST,wdmsg(E),X)) *-> true ; F .

:- meta_predicate quietly(0).
quietly(G):- !, G.
quietly(G):- \+ tracing,!,call(G).
quietly(G):- skipWrapper,!,call(G).
% quietly(G):- !, on_x_f((G),setup_call_cleanup(wdmsg(begin_eRRor_in(G)),rtrace(G),wdmsg(end_eRRor_in(G))),fail).
quietly(G):- on_x_f(hide_trace(G),
                     setup_call_cleanup(wdmsg(begin_eRRor_in(G)),rtrace(G),wdmsg(end_eRRor_in(G))),
                     fail).

:- if(current_prolog_flag(optimise,true)).
is_recompile:-fail.
:- else.
is_recompile:-fail.
:- endif.

% -- CODEBLOCK
:- export(sanity/1).
% = :- meta_predicate(sanity(0)).

% sanity is used for type checking (is not required)
% sanity(Goal):-!.
%% sanity( :GoalGoal) is det.
%
% Optional Sanity Checking.
%

sanity(Goal):- \+ current_prolog_flag(logicmoo_debug,true), current_prolog_flag(unsafe_speedups,true), \+ tracing,!, (1 is random(10)-> must(Goal) ; true).
sanity(_):- \+ current_prolog_flag(logicmoo_debug,true), notrace((is_release, \+ is_recompile)),!.
% sanity(Goal):- bugger_flag(release,true),!,assertion(Goal),!.
sanity(Goal):- quietly(Goal),!.
sanity(Goal):- tlbugger:show_must_go_on,!,dmsg(show_failure(sanity,Goal)).
sanity(Goal):- setup_call_cleanup(wdmsg(begin_FAIL_in(Goal)),rtrace(Goal),wdmsg(end_FAIL_in(Goal))),!,dtrace(system:dbreak).

compare_results(N+NVs,O+OVs):-
   NVs=@=OVs -> true; trace_or_throw(compare_results(N,O)).

allow_unsafe_code :- fail.

unsafe_safe(_,O):- \+ allow_unsafe_code, !, call(O).
unsafe_safe(N,O):- on_diff_throw(N,O).

:- export(need_speed/0).
need_speed:-current_prolog_flag(unsafe_speedups,true).

:- export(is_release/0).
%% is_release is semidet.
%
% If Is A Release.

is_release:- current_prolog_flag(unsafe_speedups,true),!.
is_release:- notrace((\+ current_prolog_flag(logicmoo_debug,true), \+ (1 is random(4)))).



%% not_is_release is semidet.
%
% Not If Is A Release.
%
:- export(not_is_release/0).
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
%must(Call):-(repeat, (catchv(Call,E,(dmsg(E:Call),debug,fail)) *-> true ; (ignore(ftrace(Call)),leash(+all),repeat,wdmsg(failed(Call)),dtrace,Call)),!).

%= 	 	 

%% must( :GoalGoal) is semidet.
%
% Must Be Successfull.
%
must(Goal):- skipWrapper,!, (Goal *-> true;trace_or_throw(failed_must(Goal))).
must(Goal):-  notrace((get_must(Goal,MGoal),!)),call(MGoal).

dumpST_error(Msg):- notrace((ddmsg(error,Msg),dumpST,ddmsg(error,Msg))).

%= 	 	 

%% get_must( ?Goal, ?CGoal) is semidet.
%
% Get Must Be Successfull.
%
%get_must(hotrace(Goal),CGoal):-  fail, !,get_must(Goal,CGoal).
get_must(M:Goal,M:CGoal):- must_be(nonvar,Goal), !,get_must(Goal,CGoal).
get_must(hotrace(Goal),CGoal):- !,get_must((hotrace(Goal)*->true;Goal),CGoal).
get_must(Goal,CGoal):-  (tlbugger:show_must_go_on; non_user_console),!,
 CGoal = ((catchv(Goal,E,
     notrace(((dumpST_error(sHOW_MUST_go_on_xI__xI__xI__xI__xI_(E,Goal)),ignore(rtrace(Goal)),badfood(Goal)))))
            *-> true ; 
              notrace(dumpST_error(sHOW_MUST_go_on_failed_F__A__I__L_(Goal))),ignore(rtrace(Goal)),badfood(Goal))).

get_must(Goal,CGoal):-  (tlbugger:skipMust;skipWrapper),!,CGoal = Goal.
get_must(Goal,CGoal):- !, (CGoal = (on_x_rtrace(Goal) *-> true; debugCallWhy(failed(on_f_debug(Goal)),Goal))).
% get_must(Goal,CGoal):- !, CGoal = (Goal *-> true ; ((dumpST_error(failed_FFFFFFF(must(Goal))),dtrace(Goal)))).

%get_must(Goal,CGoal):- !, CGoal = (catchv(Goal,E,(notrace,ddmsg(eXXX(E,must(Goal))),rtrace(Goal),dtrace,!,throw(E))) *-> true ; ((ddmsg(failed(must(Goal))),dtrace,Goal))).
get_must(Goal,CGoal):-    
   (CGoal = (catchv(Goal,E,
     ignore_each(((dumpST_error(must_xI_(E,Goal)), %set_prolog_flag(debug_on_error,true),
         rtrace(Goal),nortrace,dtrace(Goal),badfood(Goal)))))
         *-> true ; (dumpST,ignore_each(((dtrace(must_failed_F__A__I__L_(Goal),Goal),badfood(Goal))))))).

:- save_streams.
:- initialization(save_streams).
:- initialization(save_streams,restore).

%:- 'mpred_trace_none'(ddmsg(_)).
%:- 'mpred_trace_none'(ddmsg(_,_)).



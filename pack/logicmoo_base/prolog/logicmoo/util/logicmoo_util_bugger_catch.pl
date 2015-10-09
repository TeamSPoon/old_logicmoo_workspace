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

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_bugger_catch.pl
:- module(logicmoo_util_bugger_catch,
          [ !/1,
            addLibraryDir/0,
            as_clause_no_m/3,
            as_clause_w_m/4,
            as_clause_w_m/5,
            bad_functor/1,
            badfood/1,
            block/2,
            block/3,
            bubbled_ex/1,
            bubbled_ex_check/1,
            catchvv/3,
            catchvvnt/3,
            ccatch/3,
            cnotrace/1,
            current_source_location/1,
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
            get_hotrace/2,
            get_must/2,
            hotrace/0,
            hotrace/1,
            ib_multi_transparent33/1,
            if_defined/1,
            if_defined/2,
            if_defined_else/2,
            if_may_hide/1,
            input_key/1,
            is_ftCompound/1,
            is_ftNonvar/1,
            is_ftVar/1,
            is_hiding_dmsgs/0,
            is_main_thread/0,
            is_pdt_like/0,
            is_release/0,
            keep/2,
            on_x_log_throw/1,
            on_x_log_throw0/1,
            on_x_log_throwEach/1,
            on_x_log_cont/1,
            maplist_safe/2,
            maplist_safe/3,            
            module_functor/4,

          match_predicates/2,
          match_predicates/5,
          mpred_trace_less/1,
          mpred_trace_none/1,
          mpred_trace_nochilds/1,
          mpred_trace_childs/1,

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
            thread_leash/1,
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
        catchvv(0, ?, 0),
        catchvvnt(0, ?, 0),
        ccatch(0, ?, 0),
        cnotrace(0),
        ddmsg_call(0),
        on_x_fail(0),
        hotrace(0),
        if_defined(:),
        if_defined(:, 0),
        if_defined_else(:, 0),
        if_may_hide(0),
        on_x_log_throw(0),
        on_x_log_throw0(0),
        on_x_log_cont(0),
   match_predicates(:, -),
   match_predicates(:,-,-,-,-),
   mpred_trace_none(:),
   mpred_trace_less(:),
   mpred_trace_childs(:),
   mpred_trace_nochilds(:),
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
        current_source_location/1,
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
        get_hotrace/2,
        get_must/2,
        hotrace/0,
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
        on_x_log_throwEach/1,
        maplist_safe/2,
        maplist_safe/3,
        module_functor/4,

   match_predicates/2,
   match_predicates/5,
   mpred_trace_less/1,
   mpred_trace_none/1,
   mpred_trace_nochilds/1,
   mpred_trace_childs/1,

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
        thread_leash/1,
        throwNoLib/0,
        to_m_f_arity_pi/5,
        to_pi0/3,
        warn_bad_functor/1.
:- dynamic
        is_hiding_dmsgs/0.




:- if(current_predicate(logicmoo_utils:combine_logicmoo_utils/0)).
:- module(logicmoo_util_bugger_catch,
    [  % when the predciates are not being moved from file to file the exports will be moved here
      aina/1,
         ainz/1,
         as_clause/3,
         ain/1,
         ainz/1,
         ainz_clause/1,
         ainz_clause/2,
         ain/1,
         safe_univ/2,
         clause_asserted/2,
         clause_asserted/1]).  

:- endif.

:- meta_predicate logicmoo_util_bugger_catch:with_unlocked_pred(:,0).

% = :- meta_predicate(catchvvnt(0,?,0)).
catchvvnt(T,E,F):-catchvv(cnotrace(T),E,F).

:- thread_self(X),assert(lmcache:thread_main(user,X)).

is_pdt_like:-thread_property(_,alias(pdt_console_server)).
is_pdt_like:-lmcache:thread_main(user,X),!,X \= main.

is_main_thread:-lmcache:thread_main(user,X),!,thread_self(X).

:- thread_local(tlbugger:no_colors/0).
:- thread_local(t_l:thread_local_current_main_error_stream/1).

:- is_pdt_like-> assert(tlbugger:no_colors); true.


% = :- meta_predicate(with_main_error_to_output(0)).
with_main_error_to_output(Goal):-
 current_output(Out),
  w_tl(t_l:thread_local_current_main_error_stream(Out),Goal).
   

thread_current_error_stream(Err):- t_l:thread_local_current_main_error_stream(Err),!.
thread_current_error_stream(Err):-thread_self(ID),thread_current_error_stream(ID,Err).

current_main_error_stream(Err):- t_l:thread_local_current_main_error_stream(Err),!.
current_main_error_stream(Err):-lmcache:thread_main(user,ID),thread_current_error_stream(ID,Err).

format_to_error(F,A):-current_main_error_stream(Err),!,format(Err,F,A).
fresh_line_to_err:- cnotrace((flush_output_safe,current_main_error_stream(Err),format(Err,'~N',[]),flush_output_safe(Err))).

:- dynamic(thread_current_input/2).
:- dynamic(thread_current_error_stream/2).
:- volatile(thread_current_input/2).
:- volatile(thread_current_error_stream/2).
save_streams:- thread_self(ID), save_streams(ID).

save_streams(ID):- current_input(In),thread_current_input(ID,In),!.
save_streams(ID):-
  current_input(In),asserta(thread_current_input(ID,In)),
  current_output(Err),asserta(thread_current_error_stream(ID,Err)).

% = :- meta_predicate(with_main_input(0)).
with_main_input(G):-
    current_output(OutPrev),
    current_input(InPrev),
    stream_property(ErrPrev,alias(user_error)),
    lmcache:thread_main(user,ID),thread_current_input(ID,In),thread_current_error_stream(ID,Err),
    setup_call_cleanup(set_prolog_IO(In,OutPrev,Err),G,set_prolog_IO(InPrev,OutPrev,ErrPrev)).

 with_main_io(G):-
    current_output(OutPrev),
    current_input(InPrev),
    stream_property(ErrPrev,alias(user_error)),
    lmcache:thread_main(user,ID),thread_current_input(ID,In),thread_current_error_stream(ID,Err),
    setup_call_cleanup(set_prolog_IO(In,Err,Err),G,set_prolog_IO(InPrev,OutPrev,ErrPrev)).

:- save_streams.
:- initialization(save_streams).

:- dynamic(is_hiding_dmsgs).
is_hiding_dmsgs:- \+always_show_dmsg, current_prolog_flag(opt_debug,false),!.
is_hiding_dmsgs:- \+always_show_dmsg, tlbugger:ifHideTrace,!.

% bugger_debug=false turns off just debugging about the debugger
% opt_debug=false turns off all the rest of debugging
% ddmsg(_):-current_prolog_flag(bugger_debug,false),!.
% ddmsg(D):- current_predicate(wdmsg/1),wdmsg(D),!.
ddmsg(D):- ddmsg('~q',[D]).
%ddmsg(F,A):- current_predicate(wdmsg/2),wdmsg(F,A),!.
ddmsg(F,A):- sformat(SF,'~~Ndmsg: ~w~n',[F]),format_to_error(SF,A),!.
ddmsg_call(D):- ( (ddmsg(ddmsg_call(D)),call(D),ddmsg(ddmsg_exit(D))) *-> true ; ddmsg(ddmsg_failed(D))).



:- meta_predicate if_defined(:).
:- export(if_defined/1).
if_defined(Call):- tlbugger:show_must_go_on,!,if_defined(Call,((writeln(warn_undefined(Call))),!,fail)).
if_defined(Call):- !, if_defined(Call,(ddmsg(warn_undefined(Call)),trace)).

:- meta_predicate if_defined(:,0).
:- export(if_defined/2).
if_defined(Call,_Else):-current_predicate(_,Call),!,Call.
if_defined(_:Call,Else):-current_predicate(_,OM:Call)->OM:Call;(writeln(warn_undefined(Call)),Else).

:- meta_predicate if_defined_else(:,0).
:- export(if_defined_else/2).
if_defined_else(Call,_Else):-current_predicate(_,Call),!,Call.
if_defined_else(_:Call,Else):-current_predicate(_,OM:Call)->OM:Call;Else.

:- meta_predicate when_defined(:).
:- export(when_defined/1).
when_defined(Call):-if_defined(Call,true).

:- if(current_predicate(run_sanity_tests/0)).
:- listing(thread_current_error_stream/2).
:- endif.

% = :- meta_predicate(to_pi(?,?)).
to_pi(P,M:P):-var(P),!,current_module(M).
to_pi(M:P,M:P):-var(P),!,current_module(M).
to_pi(Find,(M:PI)):- once(catch(match_predicates(Find,Found),_,fail)),Found=[_|_],!,member(M:F/A,Found),functor(PI,F,A).
to_pi(M:Find,M:PI):-!,current_module(M),to_pi0(M,Find,M:PI).
to_pi(Find,M:PI):-current_module(M),to_pi0(M,Find,M:PI).

to_pi0(M,Find,M:PI):- atom(Find),!,when(nonvar(PI),(nonvar(PI),functor(PI,Find,_))).
to_pi0(M,Find/A,M:PI):-var(Find),number(A),!,when(nonvar(PI),(nonvar(PI),functor(PI,_,A))).
to_pi0(M,Find,PI):-get_pi(Find,PI0),!,(PI0\=(_:_)->(current_module(M),PI=(M:PI0));PI=PI0).


:- thread_local(t_l:last_src_loc/2).
input_key(K):-thread_self(K).
   
show_new_src_location(FL):-input_key(K),show_new_src_location(K,FL).

show_new_src_location(K,FL):- t_l:last_src_loc(K,FL),!.
show_new_src_location(K,FL):- retractall(t_l:last_src_loc(K,_)),format_to_error('~N% ~w ',[FL]),!,asserta(t_l:last_src_loc(K,FL)).


:- thread_local(t_l:current_local_why/2).
:- thread_local(t_l:current_why_source/1).

sl_to_filename(W,W):-atom(W),!.
sl_to_filename(_:W,W):-atom(W),!.
sl_to_filename(W,W).



current_source_location(F):- clause(current_source_location0(W),Body),on_x_log_fail(Body),sl_to_filename(W,F),!.
current_source_location(F):- F = unknown.

current_source_location0(F:L):-source_location(F,L),!.
current_source_location0(F:L):-prolog_load_context(file,F),current_input(S),line_position(S,L),!.
current_source_location0(When):-loading_file(When).
current_source_location0(F:L):- current_filesource(F),ignore((prolog_load_context(stream,S),!,line_count(S,L))),!.
current_source_location0(F:L):- prolog_load_context(file,F),!,ignore((prolog_load_context(stream,S),!,line_count(S,L))),!.
current_source_location0(module(M)):-source_module(M),!.
current_source_location0(When):-current_input(S),findall(NV,stream_property(S,NV),When),!.
current_source_location0(module(M)):- '$module'(M,M).

:-export(current_why/1).
:-module_transparent(current_why/1).
current_why(F):- t_l:current_why_source(F).
current_why(F):- t_l:current_local_why(F,_).
current_why(F):- current_source_location(F).


% source_module(M):-!,M=u.
:-export(source_module/1).
source_module(M):-nonvar(M),source_module(M0),!,M0=M.
source_module(M):-loading_module(M),!.
source_module(M):-'$set_source_module'(M,   M),!.

:- thread_local(t_l:last_source_file/1).
:- export(loading_file/1).
loading_file(FIn):- ((source_file0(F) *-> (retractall(t_l:last_source_file(_)),asserta(t_l:last_source_file(F))) ; (fail,t_l:last_source_file(F)))),!,F=FIn.
source_file0(F):-source_location(F,_).
source_file0(F):-prolog_load_context(file, F).
source_file0(F):-prolog_load_context(source, F).
source_file0(F):-seeing(X),is_stream(X),stream_property(X,file_name(F)),exists_file(F).
source_file0(F):-prolog_load_context(stream, S),stream_property(S,file_name(F)),exists_file(F).
source_file0(F):-findall(E,catch((stream_property( S,mode(read)),stream_property(S,file_name(E)),exists_file(E),
  line_count(S,C),C>0),_,fail),L),last(L,F).


:-export(source_variables_l/1).
source_variables_l(AllS):-
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (nb_current('$variable_names', Vs2);Vs2=[]),
  % notrace(catch((parent_goal('$toplevel':'$execute_goal2'(_, Vs3),_);Vs3=[]),E,(writeq(E),Vs3=[]))),
  ignore(Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  nb_linkval('$variable_names', AllS).


source_variables(Vs):- (((prolog_load_context(variable_names,Vs),Vs\==[]);
   (b_getval('$variable_names', Vs),Vs\==[]))),!.
source_variables(Vars):-var(Vars),parent_goal('$toplevel':'$execute_goal2'(_, Vars),_),!.
source_variables([]).


:-export( show_source_location/0).
show_source_location:- notrace((tlbugger:no_slow_io)),!.
show_source_location:- is_hiding_dmsgs,!.
show_source_location:- current_source_location(FL),!,show_new_src_location(FL),!.
show_source_location.


:- use_module(logicmoo_util_database).

:-export( as_clause_no_m/3).
as_clause_no_m( MHB,  H, B):- strip_module(MHB,_M,HB), as_clause( HB,  MH, MB),strip_module(MH,_M2H,H),strip_module(MB,_M2B,B).
as_clause_w_m(MHB, M, H, B):-  as_clause_w_m(MHB, M1H, H, B, M2B), (M1H==user->M2B=M;M1H=M).
as_clause_w_m(MHB, M1H, H, B, M2B):-  as_clause( MHB,  MH, MB),strip_module(MH,M1H,H),strip_module(MB,M2B,B).

:- export(is_ftCompound/1).
is_ftCompound(C):-compound(C),C\='$VAR'(_).

:- export(is_ftVar/1).
is_ftVar(V):-var(V),!.
is_ftVar('$VAR'(_)).

:- export(is_ftNonvar/1).
is_ftNonvar(V):- \+ is_ftVar(V).


%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[X,X,X],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

:- export((   maplist_safe/2,
   maplist_safe/3)).

maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST), on_f_debug(apply(Pred,[E]))),LISTO),!, ignore(LIST=LISTO),!.
% though this should been fine %  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), on_f_debug(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.

maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),on_f_debug(apply(Pred,[E,EE])))), LISTO),  ignore(LIST=LISTO),!.
% though this should been fine % maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), debugOnFailureEach(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, ignore(OUT=[AA|BB]).





%================================================================
% pred tracing 
%================================================================

% = :- meta_predicate('match_predicates'(:,-)).

match_predicates(M:Spec,Preds):- catch('$find_predicate'(M:Spec, Preds),_,catch('$find_predicate'(Spec, Preds),_,catch('$find_predicate'(user:Spec, Preds),_,fail))),!.
match_predicates(MSpec,MatchesO):- catch('$dwim':'$find_predicate'(MSpec,Matches),_,Matches=[]),!,MatchesO=Matches.

match_predicates(_:[],_M,_P,_F,_A):-!,fail.
match_predicates(IM:(ASpec,BSpec),M,P,F,A):-!, (match_predicates(IM:(ASpec),M,P,F,A);match_predicates(IM:(BSpec),M,P,F,A)).
match_predicates(IM:[ASpec|BSpec],M,P,F,A):-!, (match_predicates(IM:(ASpec),M,P,F,A);match_predicates(IM:(BSpec),M,P,F,A)).
match_predicates(IM:IF/IA,M,P,F,A):- '$find_predicate'(IM:P,Matches),member(CM:F/A,Matches),functor(P,F,A),(predicate_property(CM:P,imported_from(M))->true;CM=M),IF=F,IA=A.
match_predicates(Spec,M,P,F,A):- '$find_predicate'(Spec,Matches),member(CM:F/A,Matches),functor(P,F,A),(predicate_property(CM:P,imported_from(M))->true;CM=M).

:- module_transparent(if_may_hide/1).
% = :- meta_predicate(if_may_hide(0)).
if_may_hide(_G):-!.
% if_may_hide(G):-G.

with_unlocked_pred(Pred,Goal):-
   (predicate_property(Pred,foreign)-> true ;
  (
 ('$get_predicate_attribute'(Pred, system, 0) -> Goal ;
 ('$set_predicate_attribute'(Pred, system, 0),
   catch(Goal,_,true),'$set_predicate_attribute'(Pred, system, 1))))).

:- export(mpred_trace_less/1).
mpred_trace_less(W):- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
  '$set_predicate_attribute'(M:Pred, noprofile, 1),
  (A==0 -> '$set_predicate_attribute'(M:Pred, hide_childs, 1);'$set_predicate_attribute'(M:Pred, hide_childs, 1)),
  (A==0 -> '$set_predicate_attribute'(M:Pred, trace, 0);'$set_predicate_attribute'(M:Pred, trace, 1))))))).

:- export(mpred_trace_none/1).
mpred_trace_none(W):- (forall(match_predicates(W,M,Pred,F,A),
with_unlocked_pred(M:Pred,(
('$hide'(M:F/A),'$set_predicate_attribute'(M:Pred, hide_childs, 1),noprofile(M:F/A),nospy(M:Pred)))))).

:- export(mpred_trace_nochilds/1).
mpred_trace_nochilds(W):- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
'$set_predicate_attribute'(M:Pred, trace, 1),
'$set_predicate_attribute'(M:Pred, noprofile, 1),
'$set_predicate_attribute'(M:Pred, hide_childs, 1)))))).

:- export(mpred_trace_childs/1).
mpred_trace_childs(W) :- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
'$set_predicate_attribute'(M:Pred, trace, 0),
'$set_predicate_attribute'(M:Pred, noprofile, 1),
'$set_predicate_attribute'(M:Pred, hide_childs, 0)))))).   

mpred_trace_all(W) :- forall(match_predicates(W,M,Pred,_,A),( 
 with_unlocked_pred(M:Pred,(
 (A==0 -> '$set_predicate_attribute'(M:Pred, trace, 0);'$set_predicate_attribute'(M:Pred, trace, 1)),
 '$set_predicate_attribute'(M:Pred, noprofile, 0),
'$set_predicate_attribute'(M:Pred, hide_childs, 0))))).

%:-mpred_trace_all(prolog:_).
%:-mpred_trace_all('$apply':_).
%:-mpred_trace_all(system:_).



:- export(bad_functor/1).
bad_functor(L) :- arg(_,v('|','.',[],':','/'),L).

:- export(warn_bad_functor/1).
warn_bad_functor(L):-ignore((hotrace(bad_functor(L)),!,trace,nop(ddmsg(bad_functor(L))))).

:- export(strip_f_module/2).
strip_f_module(_:P,FA):-nonvar(P),!,strip_f_module(P,F),!,F=FA.
strip_f_module(P,PA):-atom(P),!,P=PA.

strip_f_module(P,FA):- is_list(P),catch(text_to_string(P,S),_,fail),!,atom_string(F,S),!,F=FA.
strip_f_module(P,FA):- hotrace(string(P);atomic(P)), atom_string(F,P),!,F=FA.
strip_f_module(P,P).

% use ccatch/3 to replace catch/3 works around SWI specific issues arround using $abort/0 and block/3
% (catch/3 allows you to have these exceptions bubble up past your catch block handlers)
% = :- meta_predicate((catchvv(0, ?, 0))).
% = :- meta_predicate((ccatch(0, ?, 0))).
:- export((ccatch/3,catchvv/3)).

bubbled_ex(block(_,_)).
bubbled_ex('$aborted').
bubbled_ex_check(E):- ( \+ bubbled_ex(E)),!.
bubbled_ex_check(E):-throw(E).

ccatch(Goal,E,Recovery):- nonvar(E) -> catch(Goal,E,Recovery); % normal mode (the user knows what they want)
                         catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode

catchvv(Goal,E,Recovery):- catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode


%:- mpred_trace_nochilds(_,catchvv,3,0,0).


:- export(functor_catch/3).
functor_catch(P,F,A):- catch(functor(P,F,A),_,compound_name_arity(P,F,A)).
% functor_catch(F,F,0):-atomic(F),!.
% functor_catch(P,F,A):-ccatch(compound_name_arity(P,F,A),E,(trace,ddmsg(E:functor(P,F,A)),trace)).


:- export(functor_safe/3).
functor_safe(P,F,A):- catch(functor(P,F,A),_,compound_name_arity(P,F,A)).
% functor_safe(P,F,A):- catch(compound_name_arity(P,F,A),_,functor(P,F,A)).
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
block(Name, Goal, Var) :- Goal, keep(Name, Var).	% avoid last-call and GC 
keep(_, _).  
set_block_exit(Name, Value) :-  prolog_current_frame(Frame),  prolog_frame_attribute(Frame, parent_goal,  mcall:block(Name, _, Value)). 
block(Name, Goal) :-  block(Name, Goal, Var),  (   Var == !  ->  !  ;   true  ). 
!(Name) :- set_block_exit(Name, !). 

:- export((block/3, 
            set_block_exit/2, 
            block/2, 
            !/1 )).

:- dynamic(buggerFile/1).
:- abolish(buggerFile/1),prolog_load_context(source,D),asserta(buggerFile(D)).

:- module_transparent(library_directory/1).

% hasLibrarySupport :- absolute_file_name('logicmoo_util_library.pl',File),exists_file(File).

throwNoLib:- trace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(library_directory), trace_or_throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

addLibraryDir :- buggerDir(Here),atom_concat(Here,'/..',UpOne), absolute_file_name(UpOne,AUpOne),asserta(library_directory(AUpOne)).

% if not has library suport, add this direcotry as a library directory
% :-not(hasLibrarySupport) -> addLibraryDir ; true .

% :-hasLibrarySupport->true;throwNoLib.




ib_multi_transparent33(MT):-multifile(MT),module_transparent(MT),dynamic_safe(MT).

dif_safe(Agent,Obj):- (var(Agent);var(Obj)),!.
dif_safe(Agent,Obj):- Agent\==Obj.

% hide Pred from tracing
to_m_f_arity_pi(M:Plain,M,F,A,PI):-!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(Term,M,F,A,PI):- strip_module(Term,M,Plain),Plain\==Term,!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(F/A,_M,F,A,PI):-functor_safe(PI,F,A),!.
to_m_f_arity_pi(PI,_M,F,A,PI):-functor_safe(PI,F,A).

with_preds((X,Y),M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call),with_preds(Y,M,F,A,PI,Call).
with_preds([X],M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call).
with_preds([X|Y],M,F,A,PI,Call):-!,with_preds(X,M,F,A,PI,Call),with_preds(Y,M,F,A,PI,Call).
with_preds(M:X,_M,F,A,PI,Call):-!, with_preds(X,M,F,A,PI,Call).
with_preds(X,M,F,A,PI,Call):-forall(to_m_f_arity_pi(X,M,F,A,PI),Call).



% ===================================================================
% Substitution based on ==
% ===================================================================
% Usage: dbgsubst(+Fml,+X,+Sk,?FmlSk)

:- export(dbgsubst/4).
dbgsubst(A,B,C,A):- B==C,!.
dbgsubst(A,B,C,D):-var(A),!,ddmsg(dbgsubst(A,B,C,D)),dumpST,dtrace,dbgsubst0(A,B,C,D).
dbgsubst(A,B,C,D):-dbgsubst0(A,B,C,D).

dbgsubst0(A,B,C,D):- 
      catchvv(hotrace(nd_dbgsubst(A,B,C,D)),E,(dumpST,ddmsg(E:nd_dbgsubst(A,B,C,D)),fail)),!.
dbgsubst0(A,_B,_C,A).

nd_dbgsubst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_dbgsubst(  P, X,Sk, P1 ) :- functor_safe(P,_,N),nd_dbgsubst1( X, Sk, P, N, P1 ).

nd_dbgsubst1( _,  _, P, 0, P  ).
nd_dbgsubst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_dbgsubst2( X, Sk, Args, ArgS ),
            nd_dbgsubst2( X, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_dbgsubst2( _,  _, [], [] ).
nd_dbgsubst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( X, Sk, [A|As], [Ap|AS] ) :- nd_dbgsubst( A,X,Sk,Ap ),nd_dbgsubst2( X, Sk, As, AS).
nd_dbgsubst2( _X, _Sk, L, L ).



%=========================================
% Module Utils
%=========================================
module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).

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
This facility is derived from the assert() macro as used in C, renamed
for obvious reasons.
*/
:- meta_predicate nodebugx(0).
:- meta_predicate with_preds(?,?,?,?,?,0).
:- meta_predicate with_skip_bugger(0).

:- meta_predicate one_must(0,0,0).

:- export(nop/1).
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


% = :- meta_predicate(cnotrace(0)).
cnotrace(G):- call(G).
:- mpred_trace_less(notrace/1).
:- '$set_predicate_attribute'(cnotrace(_), hide_childs, 1).
:- '$set_predicate_attribute'(cnotrace(_), trace, 0).


hotrace:-notrace.
%:- export(hotrace/1).
%% = :- meta_predicate(hotrace(0)).
% Unlike notrace/1, it allows traceing when excpetions are raised during Goal.


thread_leash(+Some):-!, (thread_self(main)->leash(+Some);thread_leash(-Some)).
thread_leash(-Some):-!, (thread_self(main)->leash(-Some);thread_leash(-Some)).
thread_leash(Some):-!, (thread_self(main)->leash(+Some);thread_leash(-Some)).

% ((tracing, notrace) -> CC=trace;CC=true), '$leash'(Old, Old),'$visible'(OldV, OldV),call_cleanup(Goal,(('$leash'(_, Old),'$visible'(_, OldV),CC),CC)).
get_hotrace(X,Y):- tracing,!,'$visible'(OldV, OldV),notrace,visible(-all),visible(+exception),Y = call_cleanup(show_call(X),cnotrace(('$visible'(_, OldV),trace))).
get_hotrace(X,Y):- !,'$visible'(OldV, OldV),notrace,visible(-all),visible(+exception),Y = call_cleanup(X,('$visible'(_, OldV))).

hotrace(X):-  X.
% hotrace(X):- get_hotrace(X,Y),Y.
:- mpred_trace_less(hotrace/1).
:- '$set_predicate_attribute'(hotrace(_), hide_childs, 1).
:- '$set_predicate_attribute'(hotrace(_), trace, 0).


%get_hotrace(X):-  visible(+exception),thread_leash(+exception),restore_trace((notrace,X)).
% hotrace(C):-current_predicate(_:logicmoo_bugger_loaded/0)->catchvv((C),E,((writeq(E=C),rtrace(C),trace_or_throw(E=C))));C.

:- if(false).
:- else.
:- include(logicmoo_util_header).
:- endif.

:-export(on_x_fail/1).
on_x_fail(Call):- notrace(catchvv(Call,_,fail)).


% false = hide this wrapper
showHiddens:-true.

:- meta_predicate on_x_log_fail(0).
:- export(on_x_log_fail/1).
on_x_log_fail(G):- catch(G,E,(dmsg(E:G),fail)).


% -- CODEBLOCK

:- export(on_x_log_throw/1).
on_x_log_throw(C):-prolog_ecall(0,on_x_log_throw0,C).
:- export(on_x_log_throw0/1).
on_x_log_throw0(C):- catchvv(C,E,ddmsg(on_x_log_throw(E,C))).
on_x_log_throwEach(C):-prolog_ecall(1,on_x_log_throw,C).
on_x_log_cont(C):-ignore(on_x_log_throw0(C)).

:- thread_local( tlbugger:skipMust/0).
%MAIN tlbugger:skipMust.


:- export(errx/0).
errx:-on_x_debug((ain(tlbugger:dont_skip_bugger),do_gc,dumpST(10))),!.

% false = use this wrapper, true = code is good and avoid using this wrapper
:- export(skipWrapper/0).
%skipWrapper:-!,fail.
skipWrapper:- tracing,\+tlbugger:rtracing.
skipWrapper:- tlbugger:dont_skip_bugger,!,fail.
% skipWrapper:- 0 is random(5),!.
% skipWrapper:- tlbugger:skipMust,!.
skipWrapper:- tlbugger:skip_bugger,!.

:- thread_local( tlbugger:old_no_repeats/0).
:- thread_local( tlbugger:skip_bugger/0).
:- thread_local( tlbugger:dont_skip_bugger/0).
%:-thread_local( tlbugger:bugger_prolog_flag/2).

%MAIN tlbugger:skip_bugger.


% = :- meta_predicate(one_must(0,0)).
one_must(MCall,OnFail):- skipWrapper,!, (MCall *->  true ;    OnFail).
one_must(MCall,OnFail):- strip_module(MCall,M,Call), '@'(( Call *->  true ;    OnFail ),M).


must_det(C):- must(C),!.

one_must_det(Call,_OnFail):-Call,!.
one_must_det(_Call,OnFail):-OnFail,!.

must_det(Call,OnFail):- trace_or_throw(deprecated(must_det(Call,OnFail))),Call,!.
must_det(_Call,OnFail):-OnFail.

:- module_transparent(must_det_l/1).
must_det_l(MC):- tlbugger:skipMust,!, strip_module(MC,M,C),!, '@'(det_lm(M,C),M).
must_det_l(MC):- strip_module(MC,M,C),!, '@'(must_det_lm(M,C),M).

:- module_transparent(must_det_lm/2).
must_det_lm(_,C):-var(C),trace_or_throw(var_must_det_l(C)),!.
must_det_lm(_,[]):-!.
must_det_lm(M,[C|List]):-!,must(M:C),!,must_det_lm(M,List).
must_det_lm(M,(C,List)):-!,must(M:C),!,must_det_lm(M,List).
must_det_lm(M,C):- must(M:C),!.

:- module_transparent(det_lm/2).
det_lm(M,(C,List)):- !,C,!,det_lm(M,List).
det_lm(M,C):-M:C,!.

:- module_transparent(must_l/1).
must_l(C):-var(C),trace_or_throw(var_must_l(C)),!.
must_l((A,!,B)):-!,must(A),!,must_l(B).
must_l((A,B)):-!,must((A,(deterministic(true)->(!,must_l(B));B))).
must_l(C):- must(C).


:- thread_local tlbugger:skip_use_slow_sanity/0.
:- asserta((tlbugger:skip_use_slow_sanity:-!)).

% thread locals should defaults to false  tlbugger:skip_use_slow_sanity.

slow_sanity(C):- ( tlbugger:skip_use_slow_sanity ; sanity(C)),!.


% -- CODEBLOCK
:- export(sanity/1).
% = :- meta_predicate(sanity(0)).

% sanity is used for type checking (is not required)
% sanity(Call):-!.
% sanity(_):-!.
% sanity(_):-skipWrapper,!.
sanity(Call):-bugger_flag(release,true),!,assertion(Call).
sanity(G):- tlbugger:show_must_go_on,!,ignore(show_call_failure(G)).
sanity(G):- ignore(must(show_call_failure(G))).


:- export(is_release/0).
is_release :- \+ not_is_release.
:- export(not_is_release/0).
not_is_release :- 1 is random(4).


:- thread_local tlbugger:show_must_go_on/0.
badfood(MCall):- numbervars(MCall,0,_,[functor_name('VAR_______________________x0BADF00D'),attvar(bind),singletons(false)]),dumpST.

% -- CODEBLOCK
:- export(without_must/1).
% = :- meta_predicate(without_must(0)).

without_must(G):- w_tl(tlbugger:skipMust,G).

% -- CODEBLOCK
:- export(y_must/2).
:- meta_predicate (y_must(?,0)).
y_must(Y,C):- catchvv(C,E,(wdmsg(E:must_xI__xI__xI__xI__xI_(Y,C)),fail)) *-> true ; dtrace(y_must(Y,C)).

% -- CODEBLOCK
:- export(must/1).
:- meta_predicate (must(0)).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(200), attributes(portray)]).
:- set_prolog_flag(debugger_show_context,true).

must(M:C):- notrace(get_must(M:C,CALL)),CALL.

get_must(C,C):- fail,is_release,!.
get_must(C,DO):-  tlbugger:skipMust,!,DO = C.
get_must(Call,DO):- skipWrapper,!, DO = (Call *-> true ; ((ddmsg(failed(must(Call))),trace,Call))).
get_must(Call,DO):- tlbugger:show_must_go_on,!,
 DO = ((catchvv(Call,E,
     notrace(((dumpST,ddmsg(error,sHOW_MUST_go_on_xI__xI__xI__xI__xI_(E,Call))),badfood(Call))))
            *-> true ; notrace((dumpST,ddmsg(error,sHOW_MUST_go_on_failed_F__A__I__L_(Call)),badfood(Call))))).

% get_must(Call,DO):- !, DO = on_f_debug(on_x_debug(Call)).
get_must(Call,DO):- !, DO = (catch(Call,E,(ddmsg(eRRR(E,must(Call))),rtrace(Call),trace,!,fail)) *-> true ; ((ddmsg(failed(must(Call))),trace,Call))).

get_must(Call,DO):-    
   (DO = (catchvv(Call,E,
     (dumpST,ddmsg(error,must_xI_(E,Call)),set_prolog_flag(debug_on_error,true),
         ignore_each((rtrace(Call),stop_rtrace,trace,dtrace(Call),badfood(Call)))))
         *-> true ; (dumpST,ignore_each(((trace,dtrace(must_failed_F__A__I__L_(Call),Call),badfood(Call))))))).

:- 'mpred_trace_none'(ddmsg(_)).
:- 'mpred_trace_none'(ddmsg(_,_)).


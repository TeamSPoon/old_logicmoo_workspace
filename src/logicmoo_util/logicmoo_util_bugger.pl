/** <module> Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_bugger.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
:-module(bugger,[
     was_module/2,
     evil_term/3,
         module_meta_predicates_are_transparent/1,
         module_predicates_are_exported/1,
         module_predicates_are_exported/0,
         inside_loop_check/1,
         is_loop_checked/1,
     programmer_error/1,
     forall_member/3,
     debugOnError0/1,
     global_pathname/2,
     printAll/2,
     moo_hide_childs/1,
     debugOnFailure/1,
     module_notrace/1,
     user_use_module/1,
     dumpST/0,
     dumpST/1, 
     os_to_prolog_filename/2,
     debugOnFailure0/1,
      op(1150,fx,meta_predicate_transparent),
      meta_predicate_transparent/1,
      meta_predicate_transparent/2,
     ifThen/2,
     nop/1,
     module_predicate/3,
     to_m_f_arity_pi/5,
     test_call/1,
     printAll/1,
     dynamic_load_pl/1,
     term_to_message_string/2,
     atLeastOne/1,
     ggtrace/0,
     gftrace/0,
     grtrace/0,
     atLeastOne0/2,
%     read_line_with_nl/3,
	 unnumbervars/2,

     flush_output_safe/0,
     flush_output_safe/1,

     loop_check/2,
         loop_check_throw/1,
         loop_check_fail/1,

     logOnErrorIgnore/1,
     debugOnErrorIgnore/1,
     debugCall/1,
     debugCallWhy/2,
     debugCallF/1,

     % can ignore
     failOnError/1, % for wrapping code may throw to indicate failure

     ignoreOnError/1, % same

     debugOnError/1, % Throws unless [Fail or Debug]
     logOnError/1, % Succeeds unless no error and failure occured


     debugOnFailure/1, % Succeeds but can be set to [Fail or Debug]
     logOnFailure/1,  % Fails unless [+Ignore]

     throwOnFailure/1, % Throws unless [Fail or Debug]

     % cant ignore - Throws but can be set to [Throw, Fail or Ignore or Debug]
     must/1, % must succeed at least once
     cmust/1, % doesnt run on release
     gmust/2, % like must/1 but arg2 must be ground at exit
     prolog_must/1,
     prolog_must_l/1,


        with_output_to_stream/2,
     dmsg/1,
     dmsg/2,
     dfmt/1,
     dfmt/2,
     dmsg/3,
     %%logLevel/2,
     setLogLevel/2,
     fresh_line/1,
     fresh_line/0,
     logOnFailureIgnore/1,
	 sendNote/1,
	 sendNote/4,
	 writeFailureLog/2,
	% debugOnFailure/2,
     debugOnFailureEach/1,
    moo_hide/1,

     fmt/1,fmt0/1,
     fmt/2,fmt0/2,
     fmt/3,fmt0/3,

     unlistify/2,
     listify/2,

     ifCanTrace/0,
     ctrace/0,
     isConsole/0,

     must_assign/1,
     must_assign/2,
     hotrace/1,
     set_bugger_flag/2,
     bugger_flag/2,
     buggeroo/0,
     join_path/3,
     export_all_preds/0,
     export_all_preds/1
	 ]).


% :- use_module(library(prolog_stack)).

:-multifile term_to_message_string/2.
:-dynamic term_to_message_string/2.

:-multifile was_module/2.
:-dynamic was_module/2.
:-module_transparent was_module/2.
:-multifile evil_term/3.
:-dynamic evil_term/3.

% was_module(Mod,Exports) :- nop(was_module(Mod,Exports)).

bugger_flag(F=V):-bugger_flag(F,V).
bugger_flag(F,V):-current_prolog_flag(F,V).
set_bugger_flag(F,V):-current_prolog_flag(F,_Old),set_prolog_flag(F,V),!.
set_bugger_flag(F,V):-create_prolog_flag(F,V,[term]).


:- meta_predicate moo_hide_childs(:).
:- module_transparent moo_hide_childs/0.

:- meta_predicate atLeastOne3(+,^,^).
:- meta_predicate atLeastOne0(^,^).
:- meta_predicate atLeastOne(^).
:- meta_predicate prolog_must_not(^).
:- meta_predicate gmust(^,^).
:- meta_predicate logOnFailure0(^).
:- meta_predicate debugOnError0(^).
:- meta_predicate will_debug_else_throw(^,^).
:- meta_predicate printAll(^,*).
:- meta_predicate load_dirrective(^,*).
:- meta_predicate debugOnFailure0(^).
:- meta_predicate cli_notrace(^).
:- meta_predicate printPredCount(*,^,*).
:- meta_predicate ignoreOnError(^).
:- meta_predicate traceIf(^).
:- meta_predicate ifThen(^,^).
:- meta_predicate prolog_ecall_fa(*,1,*,*,^).
:- meta_predicate tryCatchIgnore(^).
:- meta_predicate logOnError0(^).
:- meta_predicate failOnError(^).
:- meta_predicate test_call(^).
:- meta_predicate cmust(^).
:- meta_predicate debugCall(^).
:- meta_predicate prolog_ecall(*,1,?).
:- meta_predicate traceafter_call(^).
:- meta_predicate if_prolog(*,^).
:- meta_predicate must(^).
:- meta_predicate debugOnError(^).
:- meta_predicate debugOnError0(^).
:- meta_predicate debugOnErrorIgnore(^).
:- meta_predicate debugOnFailure0(^).
:- meta_predicate forall_member(*,*,^).
:- meta_predicate throwOnFailure(^).
:- meta_predicate hotrace(^).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (^.:16 sec)
:- meta_predicate prolog_must_l(^).
:- meta_predicate printAll(^).
:- meta_predicate prolog_must(^).
:- meta_predicate showProfilerStatistics(^).
:- meta_predicate notrace_call(^).
:- meta_predicate debugCallF(^).

:- module_transparent user_use_module/1.
% user_use_module(What):-!, ensure_loaded(What).
% user_use_module(logicmoo(What)):- !, '@'(use_module(logicmoo(What)),'user').
% user_use_module(library(What)):- !, use_module(library(What)).
user_use_module(What):- '@'(use_module(What),'user').

:- '@'(use_module(logicmoo_util_library), 'user').


:-dynamic inside_loop_check/1.



:- module_transparent(loop_check_fail/1).
:- module_transparent(loop_check_throw/1).
:- module_transparent(loop_check/2).
:- meta_predicate((loop_check_throw(0))).
:- meta_predicate((loop_check(0,0))).


loop_check_throw(B):- loop_check(B,((retractall(inside_loop_check(BC)),debugCallWhy(loop_check_throw(BC,B),loop_check_throw(B))))).
loop_check_fail(B):- loop_check(B,(dmsg(once(loop_check_fail(B))),fail)).

is_loop_checked(B):- copy_term(B,BC),numbervars(BC,0,_),!,inside_loop_check(BC).

loop_check(B,TODO):- copy_term(B,BC),numbervars(BC,0,_),!, loop_check(BC,B,TODO).
loop_check(BC,_B,TODO):-inside_loop_check(BC),!,call(TODO).
loop_check(BC,B,_TODO):- 
    setup_call_cleanup(asserta(inside_loop_check(BC)),
    call(B),
    ignore(retract(inside_loop_check(BC)))).


 % cli_notrace(+Call) is nondet.
 % use call/1 with trace turned off
 cli_notrace(Call):-tracing,notrace,!,call_cleanup(call(Call),trace).
 cli_notrace(Call):-call(Call).

% =========================================================================
 % false = use this wrapper, true = code is good and avoid using this wrapper
 skipWrapper:-false.
 % false = hide this wrapper
 showHiddens:-true.

:- set_prolog_flag(backtrace_depth,   20).
:- set_prolog_flag(backtrace_goal_depth, 10).
:- set_prolog_flag(backtrace_show_lines, true).
% =========================================================================


ib_multi_transparent(MT):-multifile(MT),module_transparent(MT),dynamic(MT).

:- multifile current_directory_search/1.
:- module_transparent current_directory_search/1.
:- ib_multi_transparent(formatter_hook/4).
:- module_transparent(hotrace/1).


% :-catch(guitracer,E,(writeq(E),nl)).

dhideTrace(_):-showHiddens,!.
dhideTrace(X):-'$hide'(X),!.

% ==========================================================
% can/will Tracer.
% ==========================================================

:-dynamic(ifCanTrace/0).
ifCanTrace.

isConsole :- telling(user).
isConsole :- current_output(X),!,stream_property(X,alias(user_output)).

willTrace:-not(isConsole),!,fail.
willTrace:-ifCanTrace.

hideTrace:-
  hideTrace([hotrace/1], -all),
  %%hideTrace(computeInnerEach/4, -all),

  hideTrace(
   [maplist_safe/2,
       maplist_safe/3], -all),


  hideTrace([hideTrace/0,
     ifCanTrace/0,
     ctrace/0,
     willTrace/0], -all),

  hideTrace([
     traceafter_call/1,

     notrace_call/1], -all),

  hideTrace(user:[
   call/1,
   call/2,
   apply/2,
   '$bags':findall/3,
   '$bags':findall/4,
   once/1,
   ','/2,
   catch/3,
   member/2], -all),

  hideTrace(user:setup_call_catcher_cleanup/4,-all),

  hideTrace(system:throw/1, +all),
  %%hideTrace(system:print_message/2, +all),
  hideTrace(user:message_hook/3 , +all),
  hideTrace(system:message_to_string/2, +all),
  !,hideRest,!.
  %%findall(File-F/A,(functor_source_file(M,P,F,A,File),M==user),List),sort(List,Sort),dmsg(Sort),!.

hideRest:- fail, logicmoo_util_library:buggerDir(BuggerDir),
   functor_source_file(M,_P,F,A,File),atom_concat(BuggerDir,_,File),hideTraceMFA(M,F,A,-all),
   fail.
hideRest:- functor_source_file(system,_P,F,A,_File),hideTraceMFA(system,F,A,-all), fail.
hideRest.

:- meta_predicate(hideTrace(:,+)).
:- meta_predicate with_output_to_stream(*,0).

functor_source_file(M,P,F,A,File):-functor_source_file0(M,P,F,A,File). % must(ground((M,F,A,File))),must(user:nonvar(P)).
functor_source_file0(M,P,F,A,File):-current_predicate(F/A),functor(P,F,A),source_file(P,File),predicate_module(P,M).

predicate_module(P,M):- predicate_property(P,imported_from(M)),!.
predicate_module(M:_,M):-!. %strip_module(P,M,_F),!.
predicate_module(_P,user):-!. %strip_module(P,M,_F),!.
%%predicate_module(P,M):- strip_module(P,M,_F),!.

hideTrace(_:A, _) :-
    var(A), !, trace, fail,
    throw(error(instantiation_error, _)).
hideTrace(_:[], _) :- !.
hideTrace(A:[B|D], C) :- !,
    hideTrace(A:B, C),
    hideTrace(A:D, C),!.

hideTrace(M:A,T):-!,hideTraceMP(M,A,T),!.
hideTrace(MA,T):-hideTraceMP(_,MA,T),!.

hideTraceMP(M,F/A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMP(M,P,T):-functor(P,F,0),trace,hideTraceMFA(M,F,_A,T),!.
hideTraceMP(M,P,T):-functor(P,F,A),hideTraceMFA(M,F,A,T),!.

tryCatchIgnore(MFA):- catch(MFA,_E,true). %%dmsg(tryCatchIgnoreError(MFA:E))),!.
tryCatchIgnore(_MFA):- !. %%dmsg(tryCatchIgnoreFailed(MFA)).

tryHide(_MFA):-showHiddens,!.
tryHide(MFA):- tryCatchIgnore('$hide'(MFA)).

hideTraceMFA(_,M:F,A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMFA(M,F,A,T):-user:nonvar(A),functor(P,F,A),predicate_property(P,imported_from(IM)),IM \== M,!,nop(dmsg(doHideTrace(IM,F,A,T))),hideTraceMFA(IM,F,A,T),!.
hideTraceMFA(M,F,A,T):-hideTraceMFAT(M,F,A,T),!.

hideTraceMFAT(M,F,A,T):-doHideTrace(M,F,A,T),!.

doHideTrace(_M,_F,_A,[]):-!.
doHideTrace(M,F,A,[hide|T]):- tryHide(M:F/A),!,doHideTrace(M,F,A,T),!.
doHideTrace(M,F,A,ATTRIB):- tryHide(M:F/A),!,
  tryCatchIgnore(trace(M:F/A,ATTRIB)),!.


ctrace:-willTrace->trace;notrace.

buggeroo:-hideTrace,traceAll,atom_concat(guit,racer,TRACER), catch(call(TRACER),_,true),debug,list_undefined.

singletons(_).

:-set_prolog_flag(debugger_show_context,true).
% :-set_prolog_flag(trace_gc,true).
:-set_prolog_flag(debug,true).
:-set_prolog_flag(gc,false).


failOnError(Call):-catch(Call,_,fail).

fresh_line:-current_output(Strm),fresh_line(Strm),!.
fresh_line(Strm):-failOnError((stream_property(Strm,position('$stream_position'(_,_,POS,_))),ifThen(POS>0,nl(Strm)))),!.
fresh_line(Strm):-failOnError(nl(Strm)),!.
fresh_line(_).

ifThen(When,Do):-When->Do;true.

% :- current_predicate(F/N),trace(F/N, -all),fail.
/*
traceAll:- current_predicate(user:F/N),
  functor(P,F,N),
  local_predicate(P,F/N),
  trace(F/N, +fail),fail.
traceAll:- not((predicate_property(clearCateStack/1,_))),!.
traceAll:-findall(_,(member(F,[member/2,dmsg/1,takeout/3,findall/3,clearCateStack/1]),trace(F, -all)),_).
*/
traceAll:-!.


meta_predicate_transparent(X):-strip_module(X,M,F),!, meta_predicate_transparent(M,F).
meta_predicate_transparent(M,(X,Y)):-!,meta_predicate_transparent(M,X),meta_predicate_transparent(M,Y),!.
meta_predicate_transparent(_M,X):-atom(X),!.
meta_predicate_transparent(_M,X):-
  debugOnFailureEach((
  arg(1,X,A),functor(X,F,_),
  FA=F/A,
  dynamic_if_missing(FA),
  %module_transparent(FA),
  %%meta_predicate(X),
  %trace(FA, -all),
  %%dhideTrace(FA),
  !)).

forall_member(C,[C],Call):-!,once(Call).
forall_member(C,C1,Call):-forall(member(C,C1),once(Call)).

must_assign(From=To):-must_assign(From,To).
must_assign(From,To):-To=From,!.
must_assign(From,To):-dmsg(From),dmsg(=),dmsg(From),dmsg(must_assign),!,trace,To=From.




prolog_must(Call):-must(Call).

% cmust is only used for type checking
cmust(_):-bugger_flag(release,true),!.
cmust(Call):-atLeastOne0(will_debug_else_throw(cmust(Call),Call),Call).

% gmust is must with cmust
gmust(True,Call):-catch((Call,(True->true;throw(retry(gmust(True,Call))))),retry(gmust(True,_)),(trace,Call,True)).

% must is used declaring the predicate must suceeed
must(C):-atLeastOne0(debugCallWhy(must(C),C),C).

throwOnFailure(Call):-atLeastOne0(throw(throwOnFailure(Call)),Call).
ignoreOnError(CX):-ignore(catch(CX,_,true)).

pause_trace(_):- notrace(((debug,visible(+all),leash(+exception),leash(+call)))),trace.

debugCall(C):-notrace,dmsg(debugCall(C)),dumpST, pause_trace(errored(C)),ggtrace,C.
debugCallWhy(Why, C):-notrace,dmsg(error(debugCallWhy(Why, C))),dumpST, pause_trace(debugCallWhy(Why, C)),grtrace,C.
debugCallF(C):-notrace,dmsg(debugCallF(C)),dumpST, pause_trace(failed(C)),gftrace,C.

debugOnError(C):- !, C.
debugOnError(C):-prolog_ecall(0,debugOnError0,C).
debugOnError0(C):- !, C.
debugOnError0(C):- catch(C,E,debugCallWhy(E,C)).
debugOnErrorEach(C):-prolog_ecall(1,debugOnError,C).
debugOnErrorIgnore(C):-ignore(debugOnError(C)).

debugOnFailure(C):-prolog_ecall(0,debugOnFailure0,C).
debugOnFailure0(C):- atLeastOne0((dmsg(debugOnFailure(C)),debugCallF(C)),C).
debugOnFailureEach(C):-prolog_ecall(1,debugOnFailure,C).
debugOnFailureIgnore(C):-ignore(debugOnFailure(C)).

logOnError(C):-prolog_ecall(0,logOnError0,C).
logOnError0(C):- catch(C,E,dmsg(logOnError(E,C))).
logOnErrorEach(C):-prolog_ecall(1,logOnError,C).
logOnErrorIgnore(C):-ignore(logOnError(C)).

logOnFailure(C):-prolog_ecall(0,logOnFailure0,C).
logOnFailure0(C):- atLeastOne0(dmsg(logOnFailure(C)),C).
logOnFailureEach(C):-prolog_ecall(1,logOnFailure,C).
logOnFailureIgnore(C):-ignore(logOnFailure(C)).


%debugOnFailure0(X):-ctrace,X.
%debugOnFailure0(X):-catch(X,E,(writeFailureLog(E,X),throw(E))).
%throwOnFailure/1 is like Java/C's assert/1
%debugOnFailure1(Module,CALL):-trace,debugOnFailure(Module:CALL),!.
%debugOnFailure1(arg_domains,CALL):-!,logOnFailure(CALL),!.

beenCaught(must(Call)):- !, beenCaught(Call).
beenCaught((A,B)):- !,beenCaught(A),beenCaught(B).
beenCaught(Call):- fail, predicate_property(Call,number_of_clauses(_Count)), clause(Call,(_A,_B)),!,clause(Call,Body),beenCaught(Body).
beenCaught(Call):- catch(once(Call),E,(dmsg(caugth(Call,E)),beenCaught(Call))),!.
beenCaught(Call):- traceAll,dmsg(tracing(Call)),debug,trace,Call.


local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-predicate_property(P,built_in),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
%local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains666(F,'aiml_'),!.
local_predicate(P,F/N):-functor(P,F,N),!,fail.

%atom_contains666(F,C):- hotrace((atom(F),atom(C),sub_atom(F,_,_,_,C))).



will_debug_else_throw(E,Goal):- dmsg(bugger(will_debug_else_throw(E,Goal))),grtrace,Goal.

show_goal_rethrow(E,Goal):-
   dmsg(bugger(show_goal_rethrow(E,Goal))),
   throw(E).

on_prolog_ecall(F,A,Var,Value):-
  bin_ecall(F,A,Var,Value),!.
on_prolog_ecall(F,A,Var,Value):-
  default_ecall(IfTrue,Var,Value),
  on_prolog_ecall(F,A,IfTrue,true),!.



default_ecall(asis,call,call).
default_ecall(asis,fake_failure,fail).
default_ecall(asis,error,nocatch).

default_ecall(neverfail,call,call).
default_ecall(neverfail,fail,fake_bindings).
default_ecall(neverfail,error,show_goal_rethrow).

default_ecall(onfailure,call,none).
default_ecall(onfailure,fail,reuse).
default_ecall(onfailure,error,none).

default_ecall(onerror,call,none).
default_ecall(onerror,fail,none).
default_ecall(onerror,error,reuse).


on_prolog_ecall_override(F,A,Var,_SentValue, Value):- on_prolog_ecall(F,A,Var,Value), Value \= reuse,!.
on_prolog_ecall_override(_F,_A,_Var, Value, Value).

bin_ecall(F,A,unwrap,true):-member(F/A,[(';')/2,(',')/2,('->')/2,('call')/1]).
bin_ecall(F,A,fail,throw(never_fail(F/A))):-member(F/A,[(retractall)/1]).
bin_ecall(F,A,asis,true):-member(F/A,[('must')/1]).


:-dhideTrace(prolog_ecall/2).
:-dhideTrace(prolog_ecall/5).


prolog_ecall(_,_,Call):-var(Call),!,trace,randomVars(Call).
% prolog_ecall(BDepth,OnCall,M:Call):- fail,!, '@'( prolog_ecall(BDepth,OnCall,Call), M).

prolog_ecall(_,_,Call):-skipWrapper,!,Call.
prolog_ecall(BDepth,OnCall, (X->Y;Z)):-!,(prolog_ecall(BDepth,OnCall,X) -> prolog_ecall(BDepth,OnCall,Y) ; prolog_ecall(BDepth,OnCall,Z)).
prolog_ecall(BDepth,OnCall,Call):-functor(Call,F,A),prolog_ecall_fa(BDepth,OnCall,F,A,Call).

% fake = true
prolog_ecall_fa(_,_,F,A,Call):-
  on_prolog_ecall(F,A,fake,true),!,
  atom_concat(F,'_FaKe_Binding',FAKE),
  numbervars(Call,FAKE,0,_),
  dmsg(error(fake(succeed,Call))),!.

% A=0 , (unwrap = true ; asis = true)
prolog_ecall_fa(_,_,F,0,Call):-
  (on_prolog_ecall(F,0,unwrap,true);on_prolog_ecall(F,0,asis,true)),!,
  call(Call).

% A=1 , (unwrap = true )
prolog_ecall_fa(BDepth,OnCall,F,1,Call):-
  on_prolog_ecall(F,1,unwrap,true),
  arg(1,Call,Arg),!,
  prolog_ecall(BDepth,OnCall,Arg).

% A>1 , (unwrap = true )
prolog_ecall_fa(BDepth,OnCall,F,A,Call):-
  on_prolog_ecall(F,A,unwrap,true),!,
  Call=..[F|OArgs],
  functor(Copy,F,A),
  Copy=..[F|NArgs],
  replace_elements(OArgs,E,prolog_ecall(BDepth,OnCall,E),NArgs),
  call(Copy).

% A>1 , (asis = true )
prolog_ecall_fa(_,_,F,A,Call):-
  on_prolog_ecall(F,A,asis,true),!,
  call(Call).

% each = true
prolog_ecall_fa(BDepth,OnCall,F,A,Call):-
  (on_prolog_ecall(F,A,each,true);BDepth>0),!,
  BDepth1 is BDepth-1,
  predicate_property(Call,number_of_clauses(_Count)),
  % any with bodies
  clause(Call,NT),NT \== true,!,
  clause(Call,Body),
   prolog_ecall(BDepth1,OnCall,Body).

prolog_ecall_fa(_,OnCall,_F,_A,Call):-
  call(OnCall,Call).

replace_elements([],_,_,[]):-!.
replace_elements([A|ListA],A,B,[B|ListB]):-replace_elements(ListA,A,B,ListB).

prolog_must_l(T):-T==[],!.
prolog_must_l([H|T]):-!,must(H), prolog_must_l(T).
prolog_must_l((H,T)):-!,prolog_must_l(H),prolog_must_l(T).
prolog_must_l(H):-must(H).

programmer_error(E):-trace, randomVars(E),dmsg('~q~n',[error(E)]),trace,randomVars(E),!,throw(E).


:-dhideTrace(atLeastOne/1).
atLeastOne(C):-atLeastOne0(debugCallWhy(atLeastOne(C),C),C).

:-dhideTrace(atLeastOne0/2).
% now using gensym counter instead of findall (since findall can make tracing difficult)

%old findall version
atLeastOne0(_Else,OneA):-copy_term(OneA,One),findall(One,call(One),OneL),[_|_]=OneL,!,member(OneA,OneL).
atLeastOne0(Else,_OneA):-!,Else.

% better version I think but makes more tracing
atLeastOne0(OnFail,Call):- gensym(atLeastOneCounter,Sym),flag(Sym,_,0),!, atLeastOne3(Sym,OnFail,Call).
atLeastOne3(Sym,_NFail,Call):-call(Call),flag(Sym,C,C+1).
atLeastOne3(Sym,OnFail,_All):-flag(Sym,Old,0),!, Old==0, % if old > 0 we want to fail 
         call(OnFail).




randomVars(Term):- random(R), StartR is round('*'(R,1000000)), !,
 ignore(Start=StartR),
 numbervars(Term, Start, _End, [attvar(skip),functor_name('$VAR')]).

prolog_must_not(Call):-Call,!,trace,!,programmer_error(prolog_must_not(Call)).
prolog_must_not(_Call):-!.



dynamic_if_missing(F/A):-functor(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):- dynamic([F/A]).


%%%retractall(E):- retractall(E),functor(E,File,A),dynamic(File/A),!.

pp_listing(Pred):- functor(Pred,File,A),functor(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

% =================================================================================
% Utils
% =================================================================================

printPredCount(Msg,Pred,N1):- compound(Pred), debugOnFailureEach((arg(_,Pred,NG))),user:nonvar(NG),!,
  findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),dmsg(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor(Pred,File,A),functor(FA,File,A), predicate_property(FA,number_of_clauses(N1)),dmsg(num_clauses(Msg,File/A,N1)),!.



% =================================================================================
% Loader Utils
% =================================================================================


dynamic_load_pl(PLNAME):-consult(PLNAME),!.

dynamic_load_pl(PLNAME):- % unload_file(PLNAME),
   open(PLNAME, read, In, []),
   repeat,
   line_count(In,Lineno),
   % double_quotes(_DQBool)
   Options = [variables(_Vars),variable_names(_VarNames),singletons(_Singletons),comment(_Comment)],
   catch((read_term(In,Term,[syntax_errors(error)|Options])),E,(dmsg(E),fail)),
   load_term(Term,[line_count(Lineno),file(PLNAME),stream(In)|Options]),
   Term==end_of_file,
   close(In).

load_term(E,_Options):- E == end_of_file, !.
load_term(Term,Options):-catch(load_term2(Term,Options),E,(dmsg(error(load_term(Term,Options,E))),throw_safe(E))).

load_term2(':-'(Term),Options):-!,load_dirrective(Term,Options),!.
load_term2(:-(H,B),Options):-!,load_assert(H,B,Options).
load_term2(Fact,Options):-!,load_assert(Fact,true,Options).

load_assert(H,B,_Options):-assert((H:-B)),!.

load_dirrective(include(PLNAME),_Options):- (atom_concat_safe(Key,'.pl',PLNAME) ; Key=PLNAME),!, dynamic_load_pl(Key).
load_dirrective(CALL,_Options):- CALL=..[module,M,_Preds],!,module(M),call(CALL).
load_dirrective(Term,_Options):-!,Term.

showProfilerStatistics(FileMatch):-
  statistics(global,Mem), MU is (Mem / 1024 / 1024),
  printPredCount('showProfilerStatistics: '(MU),FileMatch,_N1).



% ===============================================================================================
% UTILS
% ===============================================================================================


:- meta_predicate bugger:unify_listing(0).
unify_listing(FileMatch):-functor(FileMatch,F,A),unify_listing(FileMatch,F,A),!.
unify_listing_header(FileMatch):-functor(FileMatch,F,A),unify_listing_header(FileMatch,F,A),!.


:- meta_predicate bugger:unify_listing(0,*,*).
unify_listing_header(FileMatch,F,A):- (fmt('~n/* Prediate: ~q / ~q ~n',[F,A,FileMatch])),fail.
unify_listing_header(FileMatch,_F,_A):- printAll(predicate_property(FileMatch,PP),PP),fail.
unify_listing_header(FileMatch,_F,_A):- (fmt('~n ~q. ~n */ ~n',[FileMatch])),fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,dynamic),(fmt(':-dynamic(~q).~n',[F/A])),fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,multifile),(fmt(':-multifile(~q).~n',[F/A])),fail.
unify_listing_header(_FileMatch,_F,_A).

unify_listing(FileMatch,F,A):- unify_listing_header(FileMatch,F,A), printAll(FileMatch).

printAll(FileMatch):-printAll(FileMatch,FileMatch).
printAll(Call,Print):- flag(printAll,_,0), forall((Call,flag(printAll,N,N+1)),(fmt('~q.~n',[Print]))),fail.
printAll(_Call,Print):- flag(printAll,PA,0),(fmt('~n /* found ~q for ~q. ~n */ ~n',[PA,Print])).

contains_term(SearchThis,Find):-Find==SearchThis,!.
contains_term(SearchThis,Find):-compound(SearchThis),functor(SearchThis,Func,_),(Func==Find;arg(_,SearchThis,Arg),contains_term(Arg,Find)).

% =================================================================================
% Utils
% =================================================================================

global_pathname(B,A):-absolute_file_name(B,A),!.
global_pathname(B,A):-relative_pathname(B,A).

relative_pathname(Path,Relative):-absolute_file_name(Path,[relative_to('./')],Absolute),member(Rel,['./','../','../../']),absolute_file_name(Rel,Clip),
  canonical_pathname(Absolute,AbsoluteA),
  canonical_pathname(Clip,ClipA),
  atom_concat_safe(ClipA,RelativeA,AbsoluteA),!,atom_concat_safe(Rel,RelativeA,Relative),!.
relative_pathname(Path,Relative):-canonical_pathname(Path,Relative),!.

canonical_pathname(Absolute,AbsoluteB):-prolog_to_os_filename(AbsoluteA,Absolute),expand_file_name(AbsoluteA,[AbsoluteB]),!.

alldiscontiguous:-!.


% ===================================================================
% Lowlevel printng
% ===================================================================
unnumbervars(STUFF,UN):-sformat(S,'~W',[STUFF,[quoted(true),character_escapes(true),module(user),numbervars(true),portray(false),double_quotes(true)]]),string_to_atom(S,Atom),atom_to_term(Atom,UN,_).

open_list(V,V):-var(V).
open_list(A,B):-append(A,_,B).

unnumbervars_nil(X,Y):-!,unnumbervars(X,Y).

:-dynamic(numbered_var(_,_)).
collect_temp_vars(VARS):-!,(setof(=(Name,Number),numbered_var(Name,Number),VARS);VARS=[]).

% ==========================================================
% Sending Notes
% ==========================================================

writeModePush(_Push):-!.
writeModePop(_Pop):-!.


if_prolog(swi,G):-call(G). % Run B-Prolog Specifics
if_prolog(_,_):-!. % Dont run SWI Specificd or others

indent_e(0):-!.
indent_e(X):- X > 20, XX is X-20,!,indent_e(XX).
indent_e(X):- catch((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).

% ===================================================================
% Lowlevel printng
% ===================================================================
fmt0(X,Y,Z):-catch((format(X,Y,Z),flush_output_safe(X)),E,dmsg(E)).
fmt0(X,Y):-catch((format(X,Y),flush_output_safe),E,dmsg(E)).
fmt0(X):-catch(text_to_string(X,S),_,fail),'format'('~w',[S]),!.
fmt0(X):- (atom(X) -> catch((format(X,[]),flush_output_safe),E,dmsg(E)) ; (term_to_message_string(X,M) -> 'format'('~q~n',[M]);'format'('~q~n',[X]))).
fmt(X):-fresh_line,fmt0(X).
fmt(X,Y):-fresh_line,fmt0(X,Y),!.
fmt(X,Y,Z):- fmt0(X,Y,Z),!.

dfmt(X):- with_output_to_stream(user_error,fmt(X)).
dfmt(X,Y):- with_output_to_stream(user_error,fmt(X,Y)).

with_output_to_stream(Stream,Goal):-
    current_output(Saved),
   setup_call_cleanup(set_output(Stream),
         Goal,
         set_output(Saved)).

:-dynamic dmsg_log/3.

logger_property(todo,once,true).


:-debug(todo).

dmsg(V):- (dmsg0(V)).
dmsg0(_):- bugger_flag(opt_debug=off),!.
dmsg0(V):-var(V),!,ggtrace,throw(dmsg(V)).
dmsg0(warn(V)):- print_message(warning,V).
dmsg0(C):-functor(C,Topic,_),debugging(Topic,_True_or_False),!,logger_property(Topic,once,true),!,
      (dmsg_log(Topic,_Time,C) -> true ; ((get_time(Time),asserta(dmsg_log(todo,Time,C)),!,dmsg2(C)))).
dmsg0(C):-((copy_term(C,Stuff), randomVars(Stuff),!,dmsg2(Stuff))).

dmsg2(T):-!,
	((
	if_prolog(swi,
		(prolog_current_frame(Frame),
		prolog_frame_attribute(Frame,level,Depth),!,
		Depth2 = (Depth-25))),
        with_output_to_stream(user_error,
        ((
	fmt('%',[]),!,
	indent_e(Depth2),!,
	fmt0(T)))))).

% never gets here due to cut above 
dmsg2([]):-!.
dmsg2(LF):-functor(LF,F,_),loggerReFmt(F,LR), ((LR==F,is_stream(F))->loggerFmtReal(F,LF,[]);dmsg(LR,LF,[])),!.
dmsg2([A|L]):-!,dmsg('% ~q~n',[[A|L]]).
dmsg2(Comp):-bugger:evil_term(_,Comp,Comp2),!,dmsg('% ~q~n',[Comp2]).
dmsg2(Stuff):-!,dmsg('% ~q~n',[Stuff]).


dmsg(L,F,A):-loggerReFmt(L,LR),loggerFmtReal(LR,F,A).

dmsg(F,A):-is_list(A),!,
    nl(user_error),
    fmt(user_error,F,A),
    nl(user_error),
    flush_output_safe(user_error),!.

dmsg(C,T):-!,
	((
	fmt('<font size=+1 color=~w>',[C]),
	fmt(T),
    fmt('</font>',[]))),!.


%dmsg(C,T):- isDebugOption(opt_debug=off),!.
dmsg(L,F):-loggerReFmt(L,LR),loggerFmtReal(LR,F,[]).
dmsg(_,F):-F==[-1];F==[[-1]].


:-dynamic(user:logLevel/2).
:-module_transparent(user:logLevel/2).
:-multifile(user:user:logLevel/2).

setLogLevel(M,L):-retractall(user:logLevel(M,_)),(user:nonvar(L)->asserta(user:logLevel(M,L));true).

user:logLevel(debug,user_error).
user:logLevel(error,user_error).
user:logLevel(private,none).
user:logLevel(S,Z):-current_stream(_X,write,Z),trace,stream_property(Z,alias(S)).

loggerReFmt(L,LRR):-user:logLevel(L,LR),L \==LR,!,loggerReFmt(LR,LRR),!.
loggerReFmt(L,L).

loggerFmtReal(none,_F,_A):-!.
loggerFmtReal(S,F,A):-
  current_stream(_,write,S),
    fmt(S,F,A),
    flush_output_safe(S),!.

% dumpstack_arguments.
dumpST:-dumpST([max_depth(5000),numbervars(safe),show([level,goal,clause])]).

dumpST(_):-bugger_flag(opt_debug=off),!.
dumpST(Opts):- prolog_current_frame(Frame),dumpST(Frame,Opts).

dumpST(Frame,MaxDepth):-integer(MaxDepth),!,dumpST(Frame,[max_depth(MaxDepth)]).
dumpST(Frame,Opts):-var(Opts),!,dumpST(Frame,5000).
dumpST(Frame,Opts):-is_list(Opts),!,dumpST(1,Frame,Opts).
dumpST(Frame,Opts):-dumpST(1,Frame,[Opts]).

get_m_opt(Opts,Max_depth,D100,RetVal):-E=..[Max_depth,V],(((member(E,Opts),nonvar(V)))->RetVal=V;RetVal=D100).

dumpST(N,Frame,Opts):-
   dumpST(N,Frame,Opts,Out),
   get_m_opt(Opts,numbervars,-1,Start),
   neg1_numbervars(Out,Start,ROut),
   reverse(ROut,RROut),
   ignore((forall(member(E,RROut),fdmsg(E)))).

neg1_numbervars(T,-1,T):-!.
neg1_numbervars(Out,Start,ROut):-copy_term(Out,ROut),integer(Start),!,safe_numbervars(ROut,Start,_).
neg1_numbervars(Out,safe,ROut):-copy_term(Out,ROut),safe_numbervars(ROut).

fdmsg1(txt(S)):-'format'(S,[]),!.
fdmsg1(level=L):-'format'('(~q)',[L]),!.
fdmsg1(goal=G):-'format'(' ~q. ',[G]),!.
fdmsg1(clause=[F,L]):- fresh_line,'format'('%  ~w:~w: ',[F,L]),!.
fdmsg1(clause=[]):-'format'(' /*DYN*/ ',[]),!.
fdmsg1(E):- 'format'(' ~q ',[E]).

fdmsg(fr(List)):-is_list(List),!,fresh_line,ignore(forall(member(E,List),fdmsg1(E))),nl.
fdmsg(M):-dmsg(M).

dumpST(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,max_depth,100,MD),N>=MD,!.
dumpST(N,Frame,Opts,[fr(Goal)|MORE]):- get_m_opt(Opts,show,goal,Attr),getPFA(Frame,Attr,Goal),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST(N,Frame,Opts,[nf(no(Attr),N,Frame,Opts)|MORE]):- get_m_opt(Opts,show,goal,Attr),!,dumpST_Parent(N,Frame,Opts,MORE).
dumpST(N,Frame,Opts,[nf(noFrame(N,Frame,Opts))]).

dumpST_Parent(N,Frame,Opts,More):- prolog_frame_attribute(Frame,parent,ParentFrame), NN is N +1,dumpST(NN,ParentFrame,Opts,More),!.
dumpST_Parent(N,Frame,Opts,[nf(noParent(N,Frame,Opts))]).



getPFA(Frame,[L|List],Goal):- !,findall(R, (member(A,[L|List]),getPFA1(Frame,A,R)) ,Goal).
getPFA(Frame,Attr,Goal):-getPFA1(Frame,Attr,Goal).

getPFA1(_Frame,txt(Txt),txt(Txt)):-!.
getPFA1(Frame,clause,Goal):-getPFA2(Frame,clause,ClRef),clauseST(ClRef,Goal),!.
getPFA1(Frame,Attr,Attr=Goal):-getPFA2(Frame,Attr,Goal),!.
getPFA1(_,Attr,no(Attr)).

getPFA2(Frame,Attr,Goal):- catch((prolog_frame_attribute(Frame,Attr,Goal)),E,Goal=[error(Attr,E)]),!.

clauseST(ClRef,clause=Goal):- findall(V,(member(Prop,[source(V),line_count(V)]),clause_property(ClRef,Prop)),Goal).

clauseST(ClRef,Goal = HB):- ignore(((clause(Head, Body, ClRef),copy_term(((Head :- Body)),HB)))),
   numbervars(HB,0,_),
   findall(Prop,(member(Prop,[source(_),line_count(_),file(_),fact,erased]),clause_property(ClRef,Prop)),Goal).


:-ib_multi_transparent(seenNote/1).

sendNote(X):-var(X),!.
sendNote(X):-seenNote(X),!.
sendNote(X):-!,assert(seenNote(X)).
sendNote(_).

sendNote(To,From,Subj,Message):-sendNote(To,From,Subj,Message,_).

sendNote(To,From,Subj,Message,Vars):-
	not(not((safe_numbervars((To,From,Subj,Message,Vars)),
	dmsg(sendNote(To,From,Subj,Message,Vars))))).

% ========================================================================================
% safe_numbervars/1 (just simpler safe_numbervars.. will use a rand9ome start point so if a partially numbered getPrologVars wont get dup getPrologVars
% Each prolog has a specific way it could unnumber the result of a safe_numbervars
% ========================================================================================

safe_numbervars(X):-get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G),!,safe_numbervars(X,'$VAR',G,_).
safe_numbervars(Copy,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).
safe_numbervars(Copy,_,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).
%unnumbervars(X,Y):-term_to_atom(X,A),atom_to_term(A,Y,_).


withFormatter(Lang,From,Vars,SForm):-formatter_hook(Lang,From,Vars,SForm),!.
withFormatter(_Lang,From,_Vars,SForm):-sformat(SForm,'~w',[From]).

flush_output_safe:-ignore(catch(flush_output,_,true)).
flush_output_safe(X):-ignore(catch(flush_output(X),_,true)).

writeFailureLog(E,X):-
		fmt(user_error,'\n% error: ~q ~q\n',[E,X]),flush_output_safe(user_error),!,
		%,true.
		fmt('\n% error: ~q ~q\n',[E,X]),!,flush_output. %,fmt([E,X]).

%unknown(Old, autoload).




% ========================================================================================
% Some prologs have a printf() type predicate.. so I made up fmtString/fmt in the Cyc code that calls the per-prolog mechaism
% in SWI it''s formzat/N and sformat/N
% ========================================================================================
:-ib_multi_transparent(isConsoleOverwritten/0).


fmtString(X,Y,Z):-sformat(X,Y,Z).
fmtString(Y,Z):-sformat(Y,Z).

saveUserInput:-retractall(isConsoleOverwritten),flush_output.
writeSavedPrompt:-not(isConsoleOverwritten),!.
writeSavedPrompt:-flush_output.
writeOverwritten:-isConsoleOverwritten,!.
writeOverwritten:-assert(isConsoleOverwritten).

writeErrMsg(Out,E):- message_to_string(E,S),fmt(Out,'<cycml:error>~s</cycml:error>\n',[S]),!.
writeErrMsg(Out,E,Goal):- message_to_string(E,S),fmt(Out,'<cycml:error>goal "~q" ~s</cycml:error>\n',[Goal,S]),!.
writeFileToStream(Dest,Filename):-
    catch((
    open(Filename,'r',Input),
    repeat,
        get_code(Input,Char),
        put(Dest,Char),
    at_end_of_stream(Input),
    close(Input)),E,
    fmt('<cycml:error goal="~q">~w</cycml:error>\n',[writeFileToStream(Dest,Filename),E])).




% ===============================================================================================
% join_path(CurrentDir,Filename,Name)
% ===============================================================================================



join_path(CurrentDir,Filename,Name):-
     atom_ensure_endswtih(CurrentDir,'/',Out),atom_ensure_endswtih('./',Right,Filename),
     atom_concat(Out,Right,Name),!.

atom_ensure_endswtih(A,E,A):-atom(E),atom_concat(_Left,E,A),!.
atom_ensure_endswtih(A,E,O):-atom(A),atom(E),atom_concat(A,E,O),!.
atom_ensure_endswtih(A,E,O):-atom(A),atom(O),atom_concat(A,E,O),!.
atom_ensure_endswtih(A,O,O):-atom(A),atom(O),!.

os_to_prolog_filename(OS,_PL):-must(atom(OS)),fail.
os_to_prolog_filename(_OS,PL):-must(var(PL)),fail.
os_to_prolog_filename(OS,PL):-exists_file_safe(OS),!,PL=OS.
os_to_prolog_filename(OS,PL):-exists_directory_safe(OS),!,PL=OS.
os_to_prolog_filename(OS,PL):-current_directory_search(CurrentDir),join_path(CurrentDir,OS,PL),exists_file_safe(PL),!.
os_to_prolog_filename(OS,PL):-current_directory_search(CurrentDir),join_path(CurrentDir,OS,PL),exists_directory_safe(PL),!.

os_to_prolog_filename(OS,PL):-atom(OS),atomic_list_concat([X,Y|Z],'\\',OS),atomic_list_concat([X,Y|Z],'/',OPS),!,os_to_prolog_filename(OPS,PL).
os_to_prolog_filename(OS,PL):-atom_concat_safe(BeforeSlash,'/',OS),os_to_prolog_filename(BeforeSlash,PL).
os_to_prolog_filename(OS,PL):-absolute_file_name(OS,OSP),OS \= OSP,!,os_to_prolog_filename(OSP,PL).



% =================================================================================
% Utils
% =================================================================================
test_call(G):-writeln(G),ignore(once(catch(G,E,writeln(E)))).

nop(_).

debugFmtList(ListI):-notrace((copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),dmsg(List0))),!.
debugFmtList0([],[]):-!.
debugFmtList0([A|ListA],[B|ListB]):-debugFmtList1(A,B),!,debugFmtList0(ListA,ListB),!.

debugFmtList1(Value,Value):-var(Value),!.
debugFmtList1(Name=Number,Name=Number):-number(Number).
debugFmtList1(Name=Value,Name=Value):-var(Value),!.
debugFmtList1(Name=Value,Name=(len:Len)):-copy_term(Value,ValueO),append(ValueO,[],ValueO),is_list(ValueO),length(ValueO,Len),!.
debugFmtList1(Name=Value,Name=(F:A)):-functor(Value,F,A).
debugFmtList1(Value,shown(Value)).

% ===============================================================================================
% unlistify / listify
% ===============================================================================================

unlistify([L],O):-user:nonvar(L),unlistify(L,O),!.
unlistify(L,L).

listify(OUT,OUT):-not(not(is_list(OUT))),!.
listify(OUT,[OUT]).





traceIf(_Call):-!.
traceIf(Call):-ignore((Call,trace)).

% hotrace(Goal).
% Like notrace/1 it still skips over debugging Goal.
% Unlike notrace/1, it allows traceing when excpetions are raised during Goal.
hotrace(X):-!, tracing -> notrace(X) ; catch(notrace(X),E,(dmsg(E-X),grtrace,X)).
% more accepable.. less usefull
hotrace(X):- tracing -> notrace(X) ; X.



notrace_call(X):-notrace,catch(traceafter_call(X),E,(dmsg(E-X),trace,throw(E))).
traceafter_call(X):-X,trace.
traceafter_call(_):-tracing,fail.
traceafter_call(_):-trace,fail.




%getWordTokens(WORDS,TOKENS):-concat_atom(TOKENS,' ',WORDS).
%is_string(S):-string(S).



 %=========================================
 % Module Utils
 %=========================================

 % module_functor(PredImpl,Module,Pred,Arity).

 module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).
 strip_arity(Pred/Arity,Pred,Arity).
 strip_arity(PredImpl,Pred,Arity):-functor(PredImpl,Pred,Arity).


 %:-interactor.

export_all_preds:-source_location(File,_Line),module_property(M,file(File)),!,export_all_preds(M).

export_all_preds(ModuleName):-forall(current_predicate(ModuleName:F/A),
                   ((export(F/A),functor(P,F,A),moo_hide(ModuleName:P)))).



% hide Pred from tracing
to_m_f_arity_pi(Term,M,F,A,PI):- strip_module(Term,M,Plain),Plain\=Term,!,
to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(M:Plain,M,F,A,PI):-!,
to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(F/A,_M,F,A,PI):-functor(PI,F,A),!.
to_m_f_arity_pi(PI,_M,F,A,PI):-!,functor(PI,F,A).


% moo_hide(+Pred).
:-export(moo_hide/1).

moo_hide(_PIn):-showHiddens,!.
moo_hide(PIn):-
 must(to_m_f_arity_pi(PIn,M,_F,_A,MPred)),
 moo_hide_childs(M,MPred).


moo_hide_childs(M:Pred):-
  moo_hide_childs(M,Pred).

moo_hide_childs(M,Pred):-
  predicate_property(Pred,imported_from(M)),
 '$set_predicate_attribute'(M:Pred, trace, 0),
 '$set_predicate_attribute'(M:Pred, noprofile, 1),
 '$set_predicate_attribute'(M:Pred, hide_childs, 1),!.

moo_hide_childs(N,MPred):-
 predicate_property(MPred,imported_from(M)),
  writeq(wont_hide(M,N,MPred)),nl.


%%:-user:(forall(current_predicate(bugger:FA),bugger:moo_hide(bugger:FA))).
% hide this module from tracing
%%:-user:(forall(current_predicate(logicmoo_util_strings:FA),bugger:moo_hide(logicmoo_util_strings:FA))).

module_notrace(M):- forall(predicate_property(P,imported_from(M)),bugger:moo_hide(M:P)).


moo_hide_showChilds(M,F,A):-functor(MPred,F,A),moo_hide_showChilds(M,F,A,MPred).

moo_hide_showChilds(M,_,_,MPred):-
   not(predicate_property(_:MPred,imported_from(M))).

moo_hide_showChilds(M,F,A,_MPred):-
 % dmsg(moo_hide_showChilds(M,F,A,MPred)),
 '$set_predicate_attribute'(M:F/A, trace, 0),
 '$set_predicate_attribute'(M:F/A, noprofile, 1),
 '$set_predicate_attribute'(M:F/A, hide_childs, 0),!.

:- module_notrace(bugger).
:- module_notrace(logicmoo_util_strings).


:-source_location(File,_Line),module_property(M,file(File)),!,forall(current_predicate(M:F/A),moo_hide_showChilds(M,F,A)).

:-moo_hide_showChilds(bugger,prolog_ecall_fa,5).
:-moo_hide_showChilds(bugger,prolog_ecall,3).
:-moo_hide_showChilds(bugger,atLeastOne,1).
:-moo_hide_showChilds(bugger,atLeastOne1,2).
:-moo_hide_showChilds(bugger,atLeastOne3,3).


% bugger_debug=off turns off just debugging about the debugger
% opt_debug=off turns off all the rest of debugging
bdmsg(_):-bugger_flag(bugger_debug=off),!.
bdmsg(_):-!.
bdmsg(D):-once(dmsg(D)).

bugger_term_expansion(_,_):-!,fail.
bugger_term_expansion(T,T3):- compound(T), once(bugger_t_expansion(T,T2)),T\=T2,!,catch(expand_term(T2,T3),_,fail).

ggtrace:- 
   notrace((visible(+all),visible(-unify),visible(+exception),
   leash(-all),leash(+exception),
   leash(+call))),trace,leash(-call).

gftrace:- 
   notrace((visible(-all), visible(+fail),visible(+call),visible(+exception),
   leash(-all),leash(+exception),
   leash(+call))),trace,leash(-call).

grtrace:- notrace(( visible(+all),leash(+all))), trace.


bugger_t_expansion(T,T):-not(compound(T)),!.
bugger_t_expansion(T,AA):- T=..[F,A],unwrappabe(F),bdmsg(bugger_term_expansion(T)),bugger_t_expansion(A,AA),!.
bugger_t_expansion([F0|ARGS0],[F1|ARGS1]):-bugger_t_expansion(F0,F1),bugger_t_expansion(ARGS0,ARGS1).
bugger_t_expansion(T,TT):- T=..[F|ARGS0],bugger_t_expansion(ARGS0,ARGS1), TT=..[F|ARGS1].

unwrappabe(F):-member(F,['debugOnError',debugOnError0]),!,fail.
unwrappabe(F):-member(FF,['OnError','OnFailure','LeastOne','Ignore','must']),atom_concat(_,FF,F),!.



bugger_goal_expansion(T,_):- bdmsg(bugger_goal_expansion(T)),fail.

bugger_expand_goal(T,_):- bdmsg(bugger_expand_goal(T)),fail.

bugger_expand_term(T,_):- bdmsg(bugger_expand_term(T)),fail.



% user:expand_goal(G,G2):- compound(G),bugger_expand_goal(G,G2),!.


% user:goal_expansion(G,G2):- compound(G),bugger_goal_expansion(G,G2).

% user:expand_term(G,G2):- compound(G),bugger_expand_term(G,G2),!.

% user:term_expansion((H:-G),(H:-G2)):- bugger_term_expansion(G,G2).

module_predicate(ModuleName,F,A):-current_predicate(ModuleName:F/A),functor(P,F,A),
   not((( predicate_property(ModuleName:P,imported_from(IM)),IM\==ModuleName ))).

:-module_transparent(module_predicates_are_exported/0).
:-module_transparent(module_predicates_are_exported/1).
:-module_transparent(module_predicates_are_exported0/1).

module_predicates_are_exported:- context_module(CM),module_predicates_are_exported(CM).

module_predicates_are_exported(user):-!,context_module(CM),module_predicates_are_exported0(CM).
module_predicates_are_exported(Ctx):-module_predicates_are_exported0(Ctx).

module_predicates_are_exported0(user):- !. % dmsg(warn(module_predicates_are_exported(user))).
module_predicates_are_exported0(ModuleName):-
   module_property(ModuleName, exports(List)),
    findall(F/A,
    (module_predicate(ModuleName,F,A),
      not(member(F/A,List))), Private),
   module_predicates_are_not_exported_list(ModuleName,Private).

module_predicates_are_not_exported_list(ModuleName,Private):-length(Private,Len),dmsg(module_predicates_are_not_exported_list(ModuleName,Len)),!.
module_predicates_are_not_exported_list(ModuleName,Private):- forall(member(F/A,Private),
     ((dmsg(export(ModuleName:F/A)),
                   ModuleName:export(F/A)))).

% make meta_predicate's module_transparent
module_meta_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor(P,F,A), once((predicate_property(ModuleName:P,(meta_predicate( P ))),
            not(predicate_property(ModuleName:P,(transparent))),P=..[F|Args], memberchk(':',Args) ))),
                   (dmsg(todo(module_transparent(ModuleName:F/A))),
                   (module_transparent(ModuleName:F/A)))).


%:- module_meta_predicates_are_transparent(bugger).


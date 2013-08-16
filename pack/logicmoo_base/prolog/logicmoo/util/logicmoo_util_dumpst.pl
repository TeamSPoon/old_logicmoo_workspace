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
:- module(logicmoo_util_dumpst,[
          getPFA/3,getPFA1/3,getPFA2/3,get_m_opt/4,fdmsg/1,fdmsg1/1,
          neg1_numbervars/3,clauseST/2,
          dtrace/0,dtrace/1,dtrace/2,
          dumptrace/1,dumptrace/2,dumptrace0/1,
          dumptrace_ret/1,
          drain_framelist/1,
          drain_framelist_ele/1,
          printable_variable_name/2,
          v_name1/2,
          v_name2/2,
          dump_st/0,
          with_source_module/1,
          to_wmsg/2,
          fmsg_rout/1,
          simplify_goal_printed/2,
          dumpST/0,dumpST/1,dumpST1/0,
          dumpST0/0,dumpST0/1,dumpST0/2,
          dumpST9/0,dumpST9/1,dumpST9/2,dumpST_now/2,printFrame/3,frame_to_fmsg/4

   ]).

:-  meta_predicate dumptrace_ret(?),
  neg1_numbervars(?, ?, 0),
  with_source_module(0),
  dumptrace_ret(0),
  dumptrace0(0),
  dumptrace(0),
  dtrace(*,0).


:- set_prolog_flag(backtrace_depth,      200).
:- set_prolog_flag(backtrace_goal_depth, 20).
:- set_prolog_flag(backtrace_show_lines, true).

:- module_transparent
          getPFA/3,getPFA1/3,getPFA2/3,get_m_opt/4,fdmsg/1,fdmsg1/1,
          neg1_numbervars/3,clauseST/2,
          dtrace/0,dtrace/1,dtrace/2,
          dumptrace/1,dumptrace/2,
          dumptrace_ret/1,
          dump_st/0,
          dumpST/0,dumpST/1,
          dumpST0/0,dumpST0/1,dumpST0/2,
          dumpST9/0,dumpST9/1,dumpST9/2.


:- ensure_loaded(library(debug)).


%= 	 	 

%% dump_st is semidet.
%
% Dump Stack Trace.
%
dump_st:- prolog_current_frame(Frame),dumpST0(Frame,10).


%= 	 	 

%% dumpST0 is semidet.
%
% Dump S True Stucture Primary Helper.
%
dumpST0:- dbreak, 
   prolog_current_frame(Frame),(tracing->notrace((CU=dtrace,notrace));CU=true),dumpST0(Frame,800),!,CU.

%= 	 	 

%% dumpST0( ?Opts) is semidet.
%
% Dump S True Stucture Primary Helper.
%
dumpST0(Opts):- once(nb_current('$dump_frame',Frame);prolog_current_frame(Frame)),dumpST0(Frame,Opts).

%= 	 	 

%% dumpST0( ?Frame, ?MaxDepth) is semidet.
%
% Dump S True Stucture Primary Helper.
%
dumpST0(_,_):- tlbugger:ifHideTrace,!.
dumpST0(Frame,MaxDepth):- ignore(MaxDepth=5000),Term = dumpST(MaxDepth),
   (var(Frame)->once(nb_current('$dump_frame',Frame);prolog_current_frame(Frame));true),
   ignore(( get_prolog_backtrace(MaxDepth, Trace,[frame(Frame),goal_depth(13)]),
    format(user_error, '% dumpST ~p', [Term]), nl(user_error),
    attach_console,dtrace,
    dbreak,

    print_prolog_backtrace(user_error, Trace,[subgoal_positions(true)]), nl(user_error), fail)),!.



% dumpstack_arguments.

%= 	 	 

%% dumpST is semidet.
%
% Dump S True Stucture.
%
dumpST:- notrace((prolog_current_frame(Frame),b_setval('$dump_frame',Frame),dumpST1)).

%= 	 	 

%% dumpST1 is semidet.
%
% Dump S True Stucture Secondary Helper.
%
dumpST1:- is_hiding_dmsgs,!.
dumpST1:- tlbugger:no_slow_io,!,dumpST0,!.
dumpST1:- tlbugger:ifHideTrace,!.
dumpST1:- loop_check_early(dumpST9,dumpST0).

%= 	 	 

%% dumpST( ?Depth) is semidet.
%
% Dump S True Stucture.
%
dumpST(Depth):- notrace((prolog_current_frame(Frame),b_setval('$dump_frame',Frame))),
   loop_check_early(logicmoo_util_dumpst:dumpST9(Depth),dumpST0(Depth)).


%= 	 	 

%% get_m_opt( ?Opts, ?Max_depth, ?D100, ?RetVal) is semidet.
%
% Get Module Opt.
%
get_m_opt(Opts,Max_depth,D100,RetVal):-E=..[Max_depth,V],(((member(E,Opts),nonvar(V)))->RetVal=V;RetVal=D100).



%= 	 	 

%% dumpST9 is semidet.
%
% Dump S T9.
%
dumpST9:- notrace((once(nb_current('$dump_frame',Frame);prolog_current_frame(Frame)), dumpST9(Frame,5000))).

%= 	 	 

%% dumpST9( ?Depth) is semidet.
%
% Dump S T9.
%
dumpST9(Depth):- once(nb_current('$dump_frame',Frame);prolog_current_frame(Frame)), dumpST9(Frame,Depth).


%= 	 	 

%% dumpST9( ?Frame, :TermMaxDepth) is semidet.
%
% Dump S T9.
%
dumpST9(_,_):- tlbugger:ifHideTrace,!.
dumpST9(Frame,MaxDepth):- integer(MaxDepth),!,dumpST_now(Frame,[max_depth(MaxDepth),numbervars(true),show([level,has_alternatives,hidden,context_module,goal,clause])]).
dumpST9(Frame,From-MaxDepth):- integer(MaxDepth),!,dumpST_now(Frame,[skip_depth(From),max_depth(MaxDepth),numbervars(true),show([level,has_alternatives,hidden,context_module,goal,clause])]).
dumpST9(Frame,List):- is_list(List),dumpST_now(Frame,[show([level,has_alternatives,hidden,context_module,goal,clause])|List]).



%= 	 	 

%% drain_framelist( ?Opts) is semidet.
%
% Drain Framelist.
%
drain_framelist(Opts):- repeat, \+ drain_framelist_ele(Opts).


%= 	 	 

%% drain_framelist_ele( ?Opts) is semidet.
%
% Drain Framelist Ele.
%
drain_framelist_ele(Opts):- 
    nb_getval('$current_stack_frame_list',[N-Frame|Next]),
    nb_setval('$current_stack_frame_list',Next),!,
    printFrame(N,Frame,Opts),!.
    
        


%= 	 	 

%% dumpST_now( ?FrameIn, ?Opts) is semidet.
%
% Dump S True Stucture Now.
%
dumpST_now(FrameIn,Opts):-
  once(number(FrameIn);prolog_current_frame(FrameIn)),
   nb_setval('$hide_rest_frames',false),
   b_setval('$current_stack_frame_depth',0),
   b_setval('$current_stack_frame_list',[]),
   get_m_opt(Opts,max_depth,100,MD),
   b_setval('$current_stack_frame_handle',FrameIn),
  (repeat,  
     nb_getval('$current_stack_frame_depth',N),
     nb_getval('$current_stack_frame_handle',Frame),
    ((pushFrame(N,Frame,Opts),MD>N)-> 
     ((prolog_frame_attribute(Frame,parent,ParentFrame)->
       (nb_setval('$current_stack_frame_handle',ParentFrame),
       NN is N +1,nb_setval('$current_stack_frame_depth',NN),fail); !));
     (!))),
   drain_framelist(Opts),!.



%% pushFrame( ?N, ?Frame, ?Opts) is semidet.
%
% Push Frame.
%
pushFrame(N,Frame,_Opts):- nb_getval('$current_stack_frame_list',Current),nb_setval('$current_stack_frame_list',[N-Frame|Current]).


%= 	 	 

%% printFrame( ?N, ?Frame, ?Opts) is semidet.
%
% Print Frame.
%
printFrame(_,_,_):- nb_current('$hide_rest_frames',true),!.
printFrame(N,Frame,Opts):-
  ignore(((frame_to_fmsg(N,Frame,Opts,Out)),must(fmsg_rout(Out)))),!.


%= 	 	 

%% frame_to_fmsg( ?N, ?Frame, ?Opts, ?N) is semidet.
%
% Frame Converted To Functor Message.
%
frame_to_fmsg(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,max_depth,100,MD),N>=MD,!,fail.
%  dumpST9(N,Frame,Opts,[nf(max_depth,N,Frame,Opts)]):-get_m_opt(Opts,skip_depth,100,SD),N=<SD,!.
frame_to_fmsg(_,Frame,Opts,[fr(Goal)]):- get_m_opt(Opts,show,goal,Ctrl),getPFA(Frame,Ctrl,Goal),!.
frame_to_fmsg(N,Frame,Opts,[nf(no(Ctrl),N,Frame,Opts)]):- get_m_opt(Opts,show,goal,Ctrl),!.
frame_to_fmsg(N,Frame,Opts,[nf(noFrame(N,Frame,Opts))]).

 


%= 	 	 

%% fmsg_rout( :TermRROut) is semidet.
%
% Functor Message Rout.
%
fmsg_rout([]):-!.
fmsg_rout([fr(E)|_]):- member(goal=GG,E),end_dump(GG),!,ignore(fdmsg(fr(E))),!.
fmsg_rout([fr(E)|_]):- member(goal=GG,E),end_dump(GG),!,ignore(fdmsg(fr(E))),!.
fmsg_rout([E|RROut]):- ignore(fdmsg(E)),!,fmsg_rout(RROut).
fmsg_rout(RROut):- show_call(why,forall(member(E,RROut),fdmsg(E))),!.


%= 	 	 

%% neg1_numbervars( ?Out, ?Start, :GoalROut) is semidet.
%
% Negated Secondary Helper Numbervars.
%
neg1_numbervars(T,-1,T):-!.
neg1_numbervars(Out,false,Out):-!.
neg1_numbervars(Out,true,ROut):-copy_term(Out,ROut),!,snumbervars(ROut,777,_).
neg1_numbervars(Out,Start,ROut):-copy_term(Out,ROut),integer(Start),!,snumbervars(ROut,Start,_).
neg1_numbervars(Out,safe,ROut):-copy_term(Out,ROut),safe_numbervars(ROut).


%= 	 	 

%% fdmsg1( ?G) is semidet.
%
% Fdmsg Secondary Helper.
%
fdmsg1(txt(S)):-'format'(S,[]),!.
fdmsg1(level=L):-'format'('(~q)',[L]),!.
fdmsg1(context_module=G):- simplify_m(G,M),!,mesg_color(G,Ctrl),ansicall(Ctrl,format('[~w]',[M])),!.
fdmsg1(has_alternatives=G):- (G==false->true;'format'('*',[G])),!.
fdmsg1(hidden=G):- (G==false->true;'format'('$',[G])),!.
fdmsg1(goal=G):-simplify_goal_printed(G,GG),!,mesg_color(GG,Ctrl),ansicall(Ctrl,format(' ~q. ',[GG])),!.
fdmsg1(clause=[F,L]):- directory_file_path(_,FF,F),'format'('  %  ~w:~w: ',[FF,L]),!.
fdmsg1(clause=[F,L]):- fresh_line,'format'('%  ~w:~w: ',[F,L]),!.
fdmsg1(clause=[]):-'format'(' /*DYN*/ ',[]),!.
fdmsg1(G):-mesg_color(G,Ctrl),ansicall(Ctrl,format(' ~q ',[G])),!.
fdmsg1(M):-dmsg(failed_fdmsg1(M)).



%= 	 	 

%% simplify_m( ?G, ?M) is semidet.
%
% Simplify Module.
%
simplify_m(G,M):-atom(G),sub_atom(G,_,6,0,M),!.
simplify_m(G,G).

%= 	 	 

%% fdmsg( ?M) is semidet.
%
% Fdmsg.
%
fdmsg(fr(List)):-is_list(List),!,must((fresh_line,ignore(forall(member(E,List),fdmsg1(E))),nl)).
fdmsg(M):- logicmoo_util_catch:ddmsg(failed_fdmsg(M)).

:- thread_local(tlbugger:plain_attvars/0).

:-export(simplify_goal_printed/2).

%= 	 	 
printable_variable_name(Var, Name) :- nonvar(Name),!,must(printable_variable_name(Var, NameO)),!,Name=NameO.
printable_variable_name(Var, Name) :- nonvar(Var),Var='$VAR'(_),format(atom(Name),"~w_VAR",Var).
printable_variable_name(Var, Name) :- nonvar(Var),format(atom(Name),"(_~q_)",Var).
printable_variable_name(Var,Name):- (get_attr(Var, vn, Name1);
  get_attr(Var, varnames, Name1)),
 (var_property(Var,name(Name2))-> 
   (Name1==Name2-> atom_concat(Name1,'_VN',Name) ; Name=(Name1:Name2)); 
    (atom(Name1)->atom_concat('?',Name1,Name);
   format(atom(Name),"'$VaR'(~q)",Var))),!.
printable_variable_name(Var,Name):- v_name1(Var,Name),!.
printable_variable_name(Var,Name):- v_name2(Var,Name),!. % ,atom_concat(Name1,'_TL',Name).

v_name1(Var,Name):- var_property(Var,name(Name)),!.
v_name1(Var,Name):- get_varname_list(Vs),member(Name=V,Vs),atomic(Name),V==Var,!.
v_name1(Var,Name):- nb_current('$old_variable_names', Vs),member(Name=V,Vs),atomic(Name),V==Var,!.
v_name2(Var,Name):- get_varname_list(Vs),format(atom(Name),'~W',[Var, [variable_names(Vs)]]).
 

%attrs_to_list(att(sk,_,ATTRS),[sk|List]):-!,attrs_to_list(ATTRS,List).
attrs_to_list(att(vn,_,ATTRS),List):-!,attrs_to_list(ATTRS,List).
attrs_to_list(att(M,V,ATTRS),[M=VV|List]):- w_tl(tlbugger:plain_attvars,simplify_goal_printed(V,VV)),!,attrs_to_list(ATTRS,List).
attrs_to_list([],[]).
attrs_to_list(_ATTRS,[]).

%% simplify_goal_printed( :TermVar, :TermVar) is semidet.
%
% Simplify Goal Printed.
%

simplify_var_printed(Var,'$avar'('$VAR'(Name))):- tlbugger:plain_attvars,must(printable_variable_name(Var,Name)),!.
simplify_var_printed(Var,'$VAR'(Name)):- get_attrs(Var,att(vn, _, [])),printable_variable_name(Var, Name),!.
simplify_var_printed(Var,'$avar'('$VAR'(Name))):- tlbugger:plain_attvars,must(printable_variable_name(Var,Name)),!.
simplify_var_printed(Var,'$avar'(Dict)):- get_attrs(Var,ATTRS),must(printable_variable_name(Var,Name)),attrs_to_list(ATTRS,List),
                         dict_create(Dict,'$VAR'(Name),List).
simplify_var_printed(Var,'$VAR'(Name)):- is_ftVar(Var),!,printable_variable_name(Var, Name).

simplify_goal_printed(Var,Var):-var(Var),!.
simplify_goal_printed(Var,Name):-cyclic_term(Var),!,Name=Var.
simplify_goal_printed(Var,Name):-is_ftVar(Var),\+ current_prolog_flag(variable_names_bad,true),simplify_var_printed(Var,Name),!.
simplify_goal_printed(Var,Var):-var(Var),!.
simplify_goal_printed(setup_call_catcher_cleanup,sccc).
simplify_goal_printed(existence_error(X,Y),existence_error(X,Y)):-nl,writeq(existence_error(X,Y)),nl,fail.
simplify_goal_printed(setup_call_cleanup,scc).
simplify_goal_printed(existence_error,'existence_error_XXXXXXXXX__\e[0m\e[1;34m%-6s\e[m\'This is text\e[0mRED__existence_error_existence_error').
simplify_goal_printed(setup_call_cleanup_each,scce).
simplify_goal_printed(call_cleanup,cc).
simplify_goal_printed(call_term_expansion(_,A,_,B,_),O):- !, simplify_goal_printed(call_term_expansion_5('...',A,'...',B,'...'),O).
simplify_goal_printed(A,'...'(SA)):- atom(A),atom_concat('/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/',SA,A),!.
simplify_goal_printed(A,'...'(SA)):- atom(A),atom_concat('/home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/',SA,A),!.
simplify_goal_printed(A,'...'(SA)):- atom(A),atom_concat('/home/dmiles/lib/swipl/pack/logicmoo_base/t/',SA,A),!.
% simplify_goal_printed(A,'...'(SA)):- atom(A),atom_concat('/',_,A),!,directory_file_path(_,SA,A),!.
simplify_goal_printed(GOAL=A,AS):- goal==GOAL,!,simplify_goal_printed(A,AS).
simplify_goal_printed(Var,Var):- \+ compound(Var),!.
simplify_goal_printed(term_position(_,_,_,_,_),'$..term_position/4..$').
%simplify_goal_printed(user:G,GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(system:G,GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(catchv(G,_,_),GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(catch(G,_,_),GS):-!,simplify_goal_printed(G,GS).
%simplify_goal_printed(skolem(V,N,_F),GS):-!,simplify_goal_printed(skeq(V,N,'..'),GS).

simplify_goal_printed('<meta-call>'(G),GS):-!,simplify_goal_printed(G,GS).
simplify_goal_printed(must_det_lm(M,G),GS):-!,simplify_goal_printed(M:must_det_l(G),GS).
simplify_goal_printed(call(G),GS):-!,simplify_goal_printed(G,GS).
simplify_goal_printed(M:G,MS:GS):-atom(M), simplify_m(M,MS),!,simplify_goal_printed(G,GS).
simplify_goal_printed([F|A],[FS|AS]):- !,simplify_goal_printed(F,FS),simplify_goal_printed(A,AS).
simplify_goal_printed(G,GS):- G=..[F|A],maplist(simplify_goal_printed,[F|A],AA),GS=..AA.




%= 	 	 

%% getPFA( ?Frame, ?Ctrl, ?Goal) is semidet.
%
% Get Pred Functor A.
%
getPFA(Frame,[L|List],Goal):- !,findall(R, (member(A,[L|List]),getPFA1(Frame,A,R)) ,Goal).
getPFA(Frame,Ctrl,Goal):-getPFA1(Frame,Ctrl,Goal).


%= 	 	 

%% getPFA1( ?Frame, ?Txt, ?Txt) is semidet.
%
% Get Pred Functor A Secondary Helper.
%
getPFA1(_Frame,txt(Txt),txt(Txt)):-!.
getPFA1(Frame,clause,Goal):-getPFA2(Frame,clause,ClRef),clauseST(ClRef,Goal),!.
getPFA1(Frame,Ctrl,Ctrl=Goal):-getPFA2(Frame,Ctrl,Goal),!.
getPFA1(_,Ctrl,no(Ctrl)).


%= 	 	 

%% getPFA2( ?Frame, ?Ctrl, ?Goal) is semidet.
%
% Get Pred Functor A Extended Helper.
%
getPFA2(Frame,Ctrl,Goal):- catchv((prolog_frame_attribute(Frame,Ctrl,Goal)),E,Goal=[error(Ctrl,E)]),!.


%= 	 	 

%% clauseST( ?ClRef, :TermGoal) is semidet.
%
% Clause S True Stucture.
%
clauseST(ClRef,clause=Goal):- findall(V,(member(Prop,[file(V),line_count(V)]),clause_property(ClRef,Prop)),Goal).

clauseST(ClRef,Goal = HB):- ignore(((clause(Head, Body, ClRef),copy_term(((Head :- Body)),HB)))),
   snumbervars(HB,0,_),
   findall(Prop,(member(Prop,[source(_),line_count(_),file(_),fact,erased]),clause_property(ClRef,Prop)),Goal).


:- thread_local(tlbugger:ifCanTrace/0).


%= 	 	 

%% end_dump( :TermGG) is semidet.
%
% End Dump.
%
end_dump(true):-!,fail.
end_dump(_:GG):-!,end_dump(GG).
end_dump(GG):-compound(GG),functor(GG,F,_),atom_concat(dump,_,F),nb_setval('$hide_rest_frames',true).

% =====================
% dtrace/0/1/2
% =====================

system:dtrace:- wdmsg("DUMP_TRACE/0"), (thread_self(main)->dtrace(system:trace);(dumpST(30),abort)).
%= 	 	 

%% dtrace is semidet.
%
% (debug) Trace.
%
system:dbreak:- wdmsg("DUMP_BREAK/0"), (thread_self(main)->dtrace(system:break);true).

:- thread_local(tlbugger:has_auto_trace/1).
:-meta_predicate(dtrace(0)).

%= 	 	 

%% dtrace( :GoalG) is semidet.
%
% (debug) Trace.
%

dtrace(G):- strip_module(G,_,dbreak),\+ thread_self(main),!.
dtrace(G):- tlbugger:has_auto_trace(C),wdmsg(has_auto_trace(C,G)),!,call(C,G). 
dtrace(G):- notrace((tracing,notrace)),!,wdmsg(tracing_dtrace(G)),scce_orig(notrace,restore_trace((leash(+all),dumptrace(G))),trace).

dtrace(G):- notrace((once(((G=dmsg(GG);G=_:dmsg(GG);G=GG),nonvar(GG))),wdmsg(GG),fail)).
%dtrace(G):- \+ tlbugger:ifCanTrace,!,hotrace((wdmsg((not(tlbugger:ifCanTrace(G)))))),!,badfood(G),!,dumpST.
%dtrace(G):- \+ tlbugger:ifCanTrace,!,hotrace((wdmsg((not(tlbugger:ifCanTrace(G)))))),!,badfood(G),!,dumpST.
dtrace(G):- dumptrace(G).


% :-meta_predicate(dtrace(+,?)).

%= 	 	 

%% dtrace( +MSG, ?G) is semidet.
%
% (debug) Trace.
%
dtrace(MSG,G):-wdmsg(MSG),dtrace(G).


%= 	 	 

%% to_wmsg( :TermG, :TermWG) is semidet.
%
% Converted To Wmsg.
%
to_wmsg(G,WG):- \+ compound(G),!,WG=G.
to_wmsg(M:G,M:WG):-atom(M), to_wmsg(G,WG).
to_wmsg(dmsg(G),WG):-!, to_wmsg(G,WG).
to_wmsg(wdmsg(G),WG):-!, to_wmsg(G,WG).
to_wmsg(G,WG):- (G=WG).


with_source_module(G):-
  '$current_source_module'(M),
  '$current_typein_module'(WM),
  scce_orig('$set_typein_module'(M),G,'$set_typein_module'(WM)).
   


% =====================
% dumptrace/1/2
% =====================
% :-meta_predicate(dumptrace(?)).

%= 	 	 

%% dumptrace( ?G) is semidet.
%
% Dump Trace.
%
dumptrace(G):- non_user_console,!,dumpST_error(non_user_console+dumptrace(G)),abort,fail.
dumptrace(G):-
  w_tl(set_prolog_flag(gui_tracer, false),
   w_tl(set_prolog_flag(gui, false),
    w_tl(set_prolog_flag(retry_undefined, false),
     dumptrace0(G)))).

dumptrace0(G):- notrace((tracing,notrace,wdmsg(tracing_dumptrace(G)))),!, catch(((dumptrace0(G) *-> dtrace ; (dtrace,fail))),_,true).
dumptrace0(G):-   
  catch(attach_console,_,true),
    repeat, 
    (tracing -> (!,fail) ; true),
    to_wmsg(G,WG),
    fmt(in_dumptrace(G)),
    wdmsg(WG),
    (get_single_char(C)->with_all_dmsg(dumptrace(G,C));throw(cant_get_single_char(!))).

:-meta_predicate(dumptrace(0,+)).

%= 	 	 

%% dumptrace( :GoalG, +C) is semidet.
%
% Dump Trace.
%
dumptrace(_,0'h):- listing(dumptrace/2),!,fail.
dumptrace(_,0'g):-!,dumpST,!,fail.
dumptrace(_,0'G):-!,notrace(dumpST0(500000)),!,fail.
dumptrace(_,0'D):-!,prolog_stack:backtrace(8000),!,fail.
dumptrace(_,0'd):-!,prolog_stack:backtrace(800),!,fail.

dumptrace(G,0'l):-!, 
  restore_trace(( notrace(ggtrace),G)),!,notrace.
dumptrace(G,0's):-!,hotrace(ggtrace),!,(hotrace(G)*->true;true).
dumptrace(G,0'S):-!, wdmsg(skipping(G)),!.
dumptrace(G,0'x):-!, wdmsg(skipping(G)),!.
dumptrace(G,0'i):-!,hotrace(ggtrace),!,ignore(G).
dumptrace(_,0'b):-!,debug,break,!,fail.
dumptrace(_,0'a):-!,abort,!,fail.
dumptrace(_,0'x):-!,must((lex,ex)),!,fail.
dumptrace(_,0'e):-!,halt(1),!.
dumptrace(_,0'm):-!,make,fail.
dumptrace(G,0'L):-!,xlisting(G),!,fail.
dumptrace(G,0'l):-!,visible(+all),show_and_do(rtrace(G)).
dumptrace(G,0'c):-!, show_and_do((G))*->true;true.
dumptrace(G,0'r):-!, notrace,(rtrace((G,notrace))),!,fail.
dumptrace(G,0'f):-!, notrace,(ftrace((G,notrace))),!,fail.
dumptrace(G,0't):-!,visible(+all),leash(+all),trace,!,G.
dumptrace(G,10):-!,dumptrace_ret(G).
dumptrace(G,13):-!,dumptrace_ret(G).
dumptrace(G,Code):- number(Code),char_code(Char,Code),!,dumptrace(G,Char).
dumptrace(_G,'p'):- in_cmt(if_defined(pp_DB,fail)),!,fail.


dumptrace(_,C):-fmt(unused_keypress(C)),!,fail.
% )))))))))))))) %

%= 	 	 

%% dumptrace_ret( ?G) is semidet.
%
% Dump Trace Ret.
%
dumptrace_ret(G):- notrace((leash(+all),visible(+all),visible(+unify),trace)),G.



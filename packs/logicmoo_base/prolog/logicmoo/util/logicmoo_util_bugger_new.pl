/*

:- module(zxz, 
          [ 
          ]). 
*/

end_of_file.

:-multifile(system:goal_expansion/2).
:- export(system:goal_expansion/2).
:-multifile(tlbugger:bugger_prolog_flag/2).
:-multifile(arity/2).
:-meta_predicate(lmdmsg_call(0)).
:-dynamic(arity/2).
:-thread_local(inxp/0).
:- meta_predicate wno(:,:).
:- meta_predicate wyes(:,:).
:-visible(+all).
:-leash(+all).


menvext(_Goal,F,top(A,B,C,D),top([F|A],B,C,D)).
menv(top([],[],[],[])).


lmdmsg(D):-format(user_error,'dmsg: ~q~n',[D]).
lmdmsg_call(D):- ( (lmdmsg(lmdmsg_call(D)),call(D),lmdmsg(lmdmsg_exit(D))) *-> true ; lmdmsg(lmdmsg_failed(D))).

:-use_module(library(apply)).
:-meta_predicate pcall(:).
:- export(pcall/1).
:-meta_predicate(out_mcall(:)).
:- export(out_mcall/1).
:-meta_predicate(in_mcall(:)).
:- export(in_mcall/1).
:-meta_predicate(mcall(:)).
:- export(mcall/1).
:-meta_predicate(mcall(?,?)).
:- export(mcall/2).
:-meta_predicate(mcall_expansion(:,?)).

mcall(Goal):- menv(Env),mcall(Goal, Env).

:- export(mcall/2).
% mcall(M:Goal,Env):-atomic(M),M=zxz,!,mcall(Goal,Env).
mcall(V, _):- var(V), !, throw(error(instantiation_error, _)).
mcall(true, _Env):-!, true.
mcall(fail, _Env):-!, fail.
mcall(A is B, _Env):-!, A is B.
mcall(\+ A, Env):-!, \+ mcall(A, Env).
mcall(!, _Env):-!, ( true ; throw(cut(_)) ).
mcall(tt, _Env):-!,  throw(tt).

mcall((Goal1, Goal2), Env):- !,mcall(Goal1, Env),mcall(Goal2, Env).
mcall((Goal1; Goal2), Env):- !,mcall(Goal1, Env);mcall(Goal2, Env).
mcall((Goal1 -> Goal2 ; Goal3), Env):- !, (mcall(Goal1, Env) -> mcall(Goal2, Env) ; mcall(Goal3, Env)).
% mcall(M:Goal,Env):-atomic(M),!,mcall(Goal,Env).
mcall(Goal,Env):-predicate_property(Goal,meta_predicate(MA)),MA=..[F|MARGS],Goal=..[F|GARGS],maplist(do_meta_arg(Env),MARGS,GARGS,NARGS),!,NG=..[F|NARGS],mcall2(NG,Env).
mcall(Goal,Env):-mcall2(Goal,Env).

do_meta_arg(_Env,(+),Goal,Goal).
do_meta_arg(_Env,(-),Goal,Goal).
do_meta_arg(_Env,(?),Goal,Goal).
do_meta_arg(Env, _,Goal,mcall(Goal,Env)).


:- export(mcall2/2).
mcall2(M:Goal,Env):-!,mcall3(M,Goal,Env).
mcall2(Goal,Env):-mcall3(user,Goal,Env).

mcall3(_M,Goal,Env):- 
    functor(Goal,F, _),
    nod('Call: ', Goal, Env),    
    catch( ( redo_clause(Env, Goal, F, BodyOut),call(BodyOut) ), cut(F), fail),
    nod('Exit: ', Goal, Env).
mcall3(_M,Goal, Env):-
    nod('Fail: ', Goal, Env),
    fail.

redo_clause(Env, Goal, F, mcall(BBody, Env1)) :- 
  predicate_property(Goal,number_of_clauses(NC)),!,NC>0,NC<100000,
  menvext(Goal,F,Env,Env1),
    (findall(Goal-Body, clause(Goal, Body), [First|Rest]),
    ( Goal-Body = First
    ; length(Rest, RL), length(RestC, RL),
      member(Goal-Body,RestC),
      nod('Redo: ', Goal, Env),
      Rest = RestC
    )),
    better_body(Body,BBody).

redo_clause(_Env, Goal, _F, pcall(Goal)).

better_body(Body,Body).

nod(Message, Goal, Env):- nop(nod(Message, Goal, Env)),!.
nod(Message, Goal, Env):- arg(1,Env,List),length(List,Tab),tab(Tab),tab(Tab),write(Env), write(': '), write(Message),write(Goal), nl.

:- meta_predicate trace_query(:,*,0).

trace_query(In, Out, Query):-
    consult(In),
    tell(Out),
    call_with_depth_limit(findall(Query, mcall(Query), Solutions), 40, XMessage),
    writeln(XMessage),
    writeln(Solutions),
    told,
    unload_file(In),
    true.


new_bugger_expansion(Goal,GO) :- \+ inxp, wyes(inxp,mcall_expansion(Goal,GO)),!.
mcall_expansion(M:Goal,GO):-atomic(M),nonvar(Goal),!, functor(Goal,F,A),mcall_expansion(Goal,M,F,A,GO),!.
mcall_expansion(Goal,GO):-nonvar(Goal),functor(Goal,F,A),mcall_expansion(Goal,user,F,A,GO),!.

mcall_expansion(_,_M,mcall,_,_):-!,fail.
% mcall_expansion(Goal,_M,_,_,Goal):-predicate_property(zxz:Goal,line_count(_)),!,fail.
mcall_expansion(_,_M,'[|]',_,_):-!,fail.
mcall_expansion(_,_M,'[]',_,_):-!,fail.
mcall_expansion(_,_M,module,_,_):-!,fail.
mcall_expansion(_,_M,(:-),1,_):-!,fail.
mcall_expansion(_,_M,pcall,_,_):-!,fail.
mcall_expansion(_,_M,call,_,_):-!,fail.
mcall_expansion(Goal,_M,_Call,_,Goal):-!.
mcall_expansion(Goal,M,F,_,zxz:mcall(M:Goal)):- atom_length(F,L),L<4.
mcall_expansion(Goal,M,F,A,zxz:mcall(M:Goal)):- \+ current_predicate(F/A).
mcall_expansion(Goal,M,F,A,zxz:mcall(M:Goal)):- \+ \+ arity(F,A).

wno(A,Goal):-setup_call_cleanup(asserta((A:-!,fail),REF),Goal,erase(REF)).
wyes(A,Goal):-setup_call_cleanup(asserta(A,REF),Goal,erase(REF)).

pcall(P):-out_mcall(call(P)).
out_mcall(Goal):-lmdmsg(out_mcall(Goal)), wno(inxp,Goal).
in_mcall(Goal):- % lmdmsg(in_mcall(Goal)), 
            wyes(inxp,Goal).


%user:goal_expansion(Goal,GO):- new_bugger_expansion(Goal,GO), Goal \=@= GO,!.
%system:goal_expansion(Goal,GO):-  new_bugger_expansion(Goal,GO), Goal \=@= GO,!.
%user:term_expansion(Goal,GO):- new_bugger_expansion(Goal,GO).
%system:term_expansion(Goal,GO):- new_bugger_expansion(Goal,GO).
%:-listing(system:goal_expansion/2).



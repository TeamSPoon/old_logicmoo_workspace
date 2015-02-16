/** <module> dbase_i_mpred_pfc
% Provides a prolog database replacent that uses PFC
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- include(dbase_i_header).

:- ignore((current_predicate(not/1),abolish(system:not/1),abolish(not/1),dynamic(not/1),assert(((not(P):- nonvar(P), \+ P))),meta_predicate(not(0)))).
:- op(0,fx,'decl_mpred_pfc').
decl_mpred_pfc(F/A):-!,export(F/A),dynamic(F/A),asserta_if_new(user:mpred_prop(prologOnly)),asserta_if_new(prolog_arity(F,A)),asserta_if_new(user:mpred_prop(F,isPfc)).
decl_mpred_pfc(F):-atom(F),!,decl_mpred_pfc(F/0).
:- op(1150,fx,'decl_mpred_pfc').

:-decl_mpred_hybrid(isa/2).

:- dynamic thlocal:pfcExpansion/0.
:- dynamic thlocal:pfcExpansionWas.

maybe_hybrid(F/_):-user:mpred_prop(F,prologOnly),!.
maybe_hybrid(F/_):-user:mpred_prop(F,prologHybrid),!.
maybe_hybrid(F/_):-user:mpred_prop(F,isPfc),!.
maybe_hybrid(F/_):-user:mpred_prop(F,X),atom(X),!.
maybe_hybrid(F/A):-current_predicate(F/A),!.
maybe_hybrid(_/A):-A=1,!.
% maybe_hybrid(C/1):-ignore((nonvar(C)->decl_mpred_hybrid(C/1);ignore(decl_mpred_hybrid(isa/2))))
maybe_hybrid(F/A):-ignore(must(decl_mpred_hybrid(F/A))).


grant_pfc_Permission_to_remove(P):-get_functor(P,F,A),
 (local_clause(P,F,A);mpred_prop(F,pfcControlled)).

pre_expand_term(X,X):-not(compound(X)),!.
pre_expand_term(X,isa(I,C)):-was_isa(X,I,C),!,maybe_hybrid(C/1).
pre_expand_term(Sent,OUT):-Sent=..[And|C12],is_logical_functor(And),!,maplist(pre_expand_term,C12,O12),OUT=..[And|O12],!.
pre_expand_term(C12,OUT):-is_list(C12),!,maplist(pre_expand_term,C12,OUT),!.
pre_expand_term(X,X):-get_functor(X,F,A),must(maybe_hybrid(F/A)),!.

:-dynamic p.
:-dynamic x.
:-decl_mpred_pfc q.
:-decl_mpred_pfc fly/1.

:- decl_mpred_pfc local_clause/1.
local_clause(_):- not(current_predicate(database_modify/2)),!.
local_clause(G):-get_functor(G,F,A),local_clause(G,F,A).

local_clause(_,F,_):-user:mpred_prop(F,isPfc).
local_clause(_,F,_):-user:mpred_prop(F,prologHybrid),!,fail.
local_clause(_,F,A):-pfcDatabaseTerm(F/A).

% local_clause(_).

pfcControls(G):- get_functor(G,F), user:mpred_prop(F,pfcControlled).
pfcMark(G):-get_functor(G,F,A),assert_if_new(user:mpred_prop(F,pfcControlled)),assert_if_new(user:mpred_arity(F,A)).

show_call_success_local(G):-call(G).

pfc_retractall(G):-show_call_success_local(local_clause(G)),!,retractall(G).
pfc_retractall(G):-database_modify(retract(all),G).
pfc_retract(G):- show_call_success_local(local_clause(G)),!,retract(G).
pfc_retract(G):- database_modify(retract(one),G).
pfc_assertz(G):- show_call_success_local(local_clause(G)),!,assertz(G),pfcMark(G).
pfc_assertz(G):- database_modify(assert(z),G),pfcMark(G).
pfc_asserta(G):- show_call_success_local(local_clause(G)),!,asserta(G),pfcMark(G).
pfc_asserta(G):- database_modify(assert(a),G),pfcMark(G).
pfc_assert(G):- show_call_success_local(local_clause(G)),!,assert(G),pfcMark(G).
pfc_assert(G):- database_modify(assert(z),G),pfcMark(G).
pfc_clause_ref(H,B,Ref):-must(local_clause(H)),!,clause(H,B,Ref).
pfc_clause(H,B):-show_call_success_local(local_clause(H)),!,clause(H,B).
pfc_clause(H,B):-database_check(clauses(is_asserted),clause(H,B)).
pfc_call(G):- pfc_call(nonPFC,G).
pfc_call(_,G):- show_call_success_local(local_clause(G)),!,predicate_property(G,_),!, debugOnError(call(G)).
pfc_call(What,X):-database_call(call(What),X).

:-thread_local ntd_max_depth/2.

% not_too_deep(_,G):-!,G.
not_too_deep(Key,G):-stack_depth(CD),
  (ntd_max_depth(Key,MD)->
      ( (CD > MD) -> (!,fail) ; G) ; 
    (MD is CD+200,call_cleanup(asserta(ntd_max_depth(Key,MD),REF),G,erase(REF)))).

% :- set_prolog_flag(unknown,fail).
:- decl_mpred_pfc(go/0).

pfcVersion(1.2).

% pfcFile('pfcsyntax').	% operator declarations.

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.



:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<=>').
:- op(1050,xfx,('<=')).
:- op(1100,fx,('=>')).
:- op(1150,xfx,('::::')).

:- op(1200,fx,(user:'disabled')).
:- op(1200,fx,(user:'enabled')).
:- op(1199,fx,(user:':-')).
:- op(1199,xfx,(user:':-')).


is_fc_body(P):- fwc==P ; (compound(P),arg(_,P,E),is_fc_body(E)).

pfc_term_expansion(':-'(_),_):-!,fail.
pfc_term_expansion((P=>Q),(:- pfcAdd((P=>Q)))).
%pfc_term_expansion((P=>Q),(:- pfcAdd(('<='(Q,P))))).  % DO NOT USE speed-up attempt
pfc_term_expansion(('<='(P,Q)),(:- pfcAdd(('<='(P,Q))))).
pfc_term_expansion((P<=>Q),(:- pfcAdd((P<=>Q)))).
pfc_term_expansion((RuleName :::: Rule),(:- pfcAdd((RuleName :::: Rule)))).
pfc_term_expansion((=>P),(:- pfcAdd(P))):-nonvar(P).
pfc_term_expansion('fwc'((Q)),(:- pfcAdd(=>Q))):-nonvar(Q).
pfc_term_expansion((disabled(Q):-P),(:- pfcRem(Q))):-P==true, nonvar(Q), pfcMark(Q),!.
pfc_term_expansion((enabled(Q):-P),(:- pfcAdd(Q))):-P==true,!,nonvar(Q), pfcMark(Q),!.
pfc_term_expansion((disabled(Q):-P),(disabled(Q):-P)):- nonvar(P),nonvar(Q),pfcMark(Q),!.
pfc_term_expansion((enabled(Q):-P),(Q<=P)):- nonvar(P),nonvar(Q),pfcMark(Q),!,pfcAdd(Q<=P).
pfc_term_expansion(((Q:-P)),(:- pfcAdd(P=>Q))):- nonvar(P),nonvar(Q),is_fc_body(P),!,pfcMark(Q).
pfc_term_expansion(((Q:-P)),(:- pfcAdd(P<=Q))):- nonvar(P),nonvar(Q),not(is_fc_body(P)),pfcControls(Q).


:-asserta(thlocal:pfcExpansion).

% pfcFile('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

:- use_module(library(lists)).

:- decl_mpred_pfc ('=>')/2.
:- decl_mpred_pfc ('::::')/2.
:- decl_mpred_pfc '<=>'/2.
:- decl_mpred_pfc '<='/2.
:- decl_mpred_pfc 'pfcPT'/2.
:- decl_mpred_pfc 'pfcNT'/3.
:- decl_mpred_pfc 'pfcBT'/2.
:- decl_mpred_pfc pfcUndoMethod/2.
:- decl_mpred_pfc pfcAction/2.

:- decl_mpred_pfc pfcSelect/1.
:- decl_mpred_pfc pfcDatabaseTerm/1.


%:- decl_mpred_pfc pfcTmsMode/1.
:- decl_mpred_pfc pfcQueue/1.
:- decl_mpred_pfc pfcDatabase/1.
:- decl_mpred_pfc pfcHaltSignal/0.
%:- decl_mpred_pfc pfcDebugging/0.
%:- decl_mpred_pfc pfcSearch/1.

%%= initialization of global assertons 

%= pfcDefault/1 initialized a global assertion.
%=  pfcDefault(P,Q) - if there is any fact unifying with P, then do 
%=  nothing, else pfc_assert Q.

pfcDefault(GeneralTerm,Default) :-
  pfc_clause(GeneralTerm,true) -> true ; pfc_assert(Default).

%= pfcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfcDefault(pfc_settings(tmsMode,_), pfc_settings(tmsMode,cycles)).

% Pfc Search strategy. pfc_settings(searchMode,X) where X is one of {direct,depth,breadth}
:- pfcDefault(pfc_settings(searchMode,_), pfc_settings(searchMode,direct)).


% 

%= add/2 and pfcPost/2 are the main ways to pfc_assert new clauses into the
%= database and have forward reasoning done.

%= pfcAdd(P,S) asserts P into the dataBase with support from S.

pfcAdd(P) :- pfcAdd(P,(pcfUser,pcfUser)).

pfcAdd((=>P),S) :- pfcAdd(P,S).

pfcAdd(P0,S0) :- pre_expand_term(P0,P),pre_expand_term(S0,S),
  pfcPost(P,S),
  pfcRun.

%pfcAdd(_,_).
%pfcAdd(P,S) :- pfcWarn("pfcAdd(~w,~w) failed",[P,S]).


% pfcPost(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) pfcPost1 is called. It always succeeds.

pfcPost([H|T],S) :-
  !,
  pfcPost1(H,S),
  pfcPost(T,S).
pfcPost([],_) :- !.
pfcPost(P,S) :- pfcPost1(P,S).


% pfcPost1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

pfcPost1(P,S) :- 
  %= db pfcAddDbToHead(P,P2),
  % pfcRemoveOldVersion(P),
  pfcAddSupport(P,S),
  pfcUnique(P),
  must(pfcAssertIfUnknown(P)), % was simply pfc_assert(P),
  pfcTraceAdd(P,S),
  !,
  pfcEnqueue(P,S),
  !.

pfcPost1(_,_).
%%pfcPost1(P,S) :-  pfcWarn("pfcAdd(~w,~w) failed",[P,S]).

%%
%= pfcAddDbToHead(+P,-NewP) talkes a fact P or a conditioned fact
%= (P:-C) and adds the Db context.
%%

pfcAddDbToHead(P,NewP) :-
  pfcCurrentDb(Db),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).


% pfcUnique(X) is true if there is no assertion X in the prolog db.

pfcUnique((Head:-Tail)) :- 
  !, 
  \+ pfc_clause(Head,Tail).
pfcUnique(P) :-
  !,
  \+ pfc_clause(P,true).


pfcEnqueue(P,S) :-
  pfc_settings(searchMode,Mode) 
    -> (Mode=direct  -> pfcFwd(P) ;
	Mode=depth   -> pfcAsserta(pfcQueue(P),S) ;
	Mode=breadth -> pfcAssert(pfcQueue(P),S) ;
	else         -> pfcWarn("Unrecognized pfcSearch mode: ~w", Mode))
     ; pfcWarn("No pfcSearch mode").


% if there is a rule of the form Identifier ::: Rule then delete it.

pfcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  var(Identifier),
  !,
  pfcWarn("variable used as an  rule name in ~w :::: ~w",
          [Identifier,Body]).

  
pfcRemoveOldVersion((Identifier::::Body)) :-
  nonvar(Identifier),
  pfc_clause((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfcRem((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion(_).



% 

% pfcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfcQueue mechanism.

pfcRun :-
  (\+ pfc_settings(searchMode,direct)),
  pfcStep,
  pfcRun.
pfcRun.


% pfcStep removes one entry from the pfcQueue and reasons from it.


pfcStep :-  
  % if pfcHaltSignal is true, reset it and fail, thereby stopping inferencing.
  pfcRetract(pfcHaltSignal),
  !, 
  fail.

pfcStep :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  pfcdo(pfcFwd(P)),
  !.

get_next_fact(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P) :- 
  pfcRetract(pfcQueue(P)),
  pfcRemoveSupportsQuietly(pfcQueue(P)),
  !.
remove_selection(P) :-
  brake(format("~Npfc:get_next_fact - selected fact not on Queue: ~w",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the pcfUser defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(P) :- 
  pfcSelect(P),
  !.  
select_next_fact(P) :- 
  defaultpfcSelect(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultpfcSelect(P) :- pfcQueue(P),!.

% pfcHalt stops the forward chaining.
pfcHalt :-  pfcHalt("",[]).

pfcHalt(Format) :- pfcHalt(Format,[]).

pfcHalt(Format,Args) :- 
  format(Format,Args),
  pfcHaltSignal -> 
       pfcWarn("pfcHalt finds pfcHaltSignal already set")
     ; pfc_assert(pfcHaltSignal).


%%
%%
%= predicates for manipulating triggers
%%


pfcAddTrigger(pfcPT(Trigger,Body),Support) :-
  !,
  pfc_trace_msg('~n      Adding positive trigger ~q~n',
		[pfcPT(Trigger,Body)]),
  pfcAssert(pfcPT(Trigger,Body),Support),
  copy_term(pfcPT(Trigger,Body),Tcopy),
  pfcBC(Trigger),
  pfcEvalLHS(Body,(Trigger,Tcopy)),
  fail.


pfcAddTrigger(pfcNT(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('~n      Adding negative trigger: ~q~n       test: ~q~n       body: ~q~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfcAssert(pfcNT(TriggerCopy,Test,Body),Support),
  \+Test,
  pfcEvalLHS(Body,((\+Trigger),pfcNT(TriggerCopy,Test,Body))).

pfcAddTrigger(pfcBT(Trigger,Body),Support) :-
  !,
  pfcAssert(pfcBT(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body).

pfcAddTrigger(X,_Support) :-
  pfcWarn("Unrecognized trigger to pfcAddtrigger: ~w",[X]).


pfcBtPtCombine(Head,Body,Support) :- 
  %= a backward trigger (pfcBT) was just added with head and Body and support Support
  %= find any pfcPT's with unifying heads and add the instantied pfcBT body.
  pfcGetTriggerQuick(pfcPT(Head,_PtBody)),
  pfcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  pfc_clause(Trigger,true).

pfcGetTrigger(Trigger):-pfcGetTriggerQuick(Trigger).

%%
%%
%= predicates for manipulating action traces.
%%

pfcAddActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  pfcAddSupport(pfcAction(Action),Support).

pfcRemActionTrace(pfcAction(A)) :-
  pfcUndoMethod(A,M),
  M,
  !.


%%
%= predicates to remove pfc facts, triggers, action traces, and queue items
%= from the database.
%%

pfcRetract(X) :- 
  %= pfc_retract an arbitrary thing.
  pfcType(X,Type),
  pfcRetractType(Type,X),
  !.

pfcRetractType(fact,X) :-   
  %= db pfcAddDbToHead(X,X2), pfc_retract(X2). 
  pfc_retract(X).

pfcRetractType(rule,X) :- 
  %= db  pfcAddDbToHead(X,X2),  pfc_retract(X2).
  pfc_retract(X).
pfcRetractType(trigger,X) :- 
  pfc_retract(X)
    -> unFc(X)
     ; pfcWarn("Trigger not found to pfc_retract: ~w",[X]).

pfcRetractType(action,X) :- pfcRemActionTrace(X).
  

%= pfcAddSome(X) adds item X to some database

pfcAddSome(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  % pfc_call the appropriate predicate.
  pfcAddType(Type,X).

pfcAddType(fact,X) :- 
  pfcUnique(X), 
  pfc_assert(X),!.
pfcAddType(rule,X) :- 
  pfcUnique(X), 
  pfc_assert(X),!.
pfcAddType(trigger,X) :- 
  pfc_assert(X).
pfcAddType(action,_Action) :- !.


  

%= pfcRem(P,S) removes support S from P and checks to see if P is still supported.
%= If it is not, then the fact is retreactred from the database and any support
%= relationships it participated in removed.

pfcRem(List) :- 
  % iterate down the list of facts to be pfcRem'ed.
  nonvar(List),
  List=[_|_],
  pfcRem_L(List).
  
pfcRem(P) :- 
  % pfcRem/1 is the pcfUser's interface - it withdraws pcfUser support for P.
  pfcRem(P,(pcfUser,pcfUser)).

pfcRem_L([H|T]) :-
  % pfcRem each element in the list.
  pfcRem(H,(pcfUser,pcfUser)),
  pfcRem_L(T).

pfcRem(P,S) :-
  % pfcDebug(format("~Nremoving support ~w from ~w",[S,P])),
  pfc_trace_msg('~n    Removing support: ~q from ~q~n',[S,P]),
  pfcRemSupport(P,S)
     -> pcfRemoveIfUnsupported(P)
      ; pfcWarn("pfcRem/2 Could not find support ~w to remove from fact ~w",
                [S,P]).

%%
%= pfcRem2 is like pfcRem, but if P is still in the DB after removing the
%= pcfUser's support, it is retracted by more forceful means (e.g. remove).
%%

pfcRem2(P) :- 
  % pfcRem2/1 is the pcfUser's interface - it withdraws pcfUser support for P.
  pfcRem2(P,(pcfUser,pcfUser)).

pfcRem2(P,S) :-
  pfcRem(P,S),
  pfcBC(P)
     -> remove(P) 
      ; true.

%%
%= remove(+F) retracts fact F from the DB and removes any dependent facts */
%%

remove(F) :- 
  pfcRemoveSupports(F),
  pfcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :- 
  pfcRemSupport(F,S),
  pfcWarn("~w was still supported by ~w",[F,S]),
  fail.
pfcRemoveSupports(_).

pfcRemoveSupportsQuietly(F) :- 
  pfcRemSupport(F,_),
  fail.
pfcRemoveSupportsQuietly(_).

% pfcUndo(X) undoes X.


pfcUndo(pfcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).

pfcUndo(pfcPT(Key,Head,Body)) :-  
  % undo a positive trigger.
  %
  !,
  (pfc_retract(pfcPT(Key,Head,Body))
    -> unFc(pfcPT(Head,Body))
     ; pfcWarn("Trigger not found to pfc_retract: ~w",[pfcPT(Head,Body)])).

pfcUndo(pfcNT(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (pfc_retract(pfcNT(Head,Condition,Body))
    -> unFc(pfcNT(Head,Condition,Body))
     ; pfcWarn("Trigger not found to pfc_retract: ~w",[pfcNT(Head,Condition,Body)])).

pfcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  pfc_retract(Fact),
  pfcTraceRem(Fact),
  unFc1(Fact).
  


%= unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
%= been removed from the database, so remove all support relations it
%= participates in and check the things that they support to see if they
%= should stayu in the database or should also be removed.


unFc(F) :- 
  pfcRetractSupportRelations(F),
  unFc1(F).

unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  pfcRun.


pfcUnFcCheckTriggers(F) :-
  pfcType(F,fact),
  copy_term(F,Fcopy),
  pfcNT(Fcopy,Condition,Action),
  (\+ Condition),
  pfcEvalLHS(Action,((\+F),pfcNT(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractSupportRelations(Fact) :-
  pfcType(Fact,Type),
  (Type=trigger -> pfcRemSupport(P,(_,Fact))
                ; pfcRemSupport(P,(Fact,_))),
  pcfRemoveIfUnsupported(P),
  fail.
pfcRetractSupportRelations(_).



%= pcfRemoveIfUnsupported(+P) checks to see if P is supported and removes
%= it from the DB if it is not.

pcfRemoveIfUnsupported(P) :- not(grant_pfc_Permission_to_remove(P)),!,pfcWarn("Denying pcfRemoveIfUnsupported access to ~w ",[P]).
pcfRemoveIfUnsupported(P) :- 
   pfcSupported(P) -> true ;  pfcUndo(P).


%= pfcSupported(+P) succeeds if P is "supported". What this means
%= depends on the TMS mode selected.

pfcSupported(P) :- 
  pfc_settings(tmsMode,Mode),
  pfcSupported(Mode,P).

pfcSupported(local,P) :- !, pfcGetSupport(P,_).
pfcSupported(cycles,P) :-  !, wellFounded(P).
pfcSupported(_,_P) :- true.


%%
%= a fact is well founded if it is supported by the pcfUser
%= or by a set of facts and a rules, all of which are well founded.
%%

wellFounded(Fact) :- pfcWFF(Fact,[]).

pfcWFF(F,_) :-
  % supported by pcfUser (pfcAxiom) or an "absent" fact (assumption).
  (pfcAxiom(F) ; pfcAssumptionBase(F)),
  !.

pfcWFF(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a pfcJustificationDB.
  supportsForWhy(F,Supporters),
  % all of whose members are well founded.
  pfcWFF_L(Supporters,[F|Descendants]),
  !.

%= pfcWFF_L(L) simply maps pfcWFF over the list.

pfcWFF_L([],_).
pfcWFF_L([X|Rest],L) :-
  pfcWFF(X,L),
  pfcWFF_L(Rest,L).



% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one pfcJustificationDB for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a pcfUser-defined fact are: [pcfUser].

supportsForWhy(F,[Fact|MoreFacts]) :-
  pfcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(pcfUser,[]) :- !.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).


%%
%%
%= pfcFwd(X) forward chains from a fact or a list of facts X.
%%


pfcFwd([H|T]) :- !, pfcFwd1(H), pfcFwd(T).
pfcFwd([]) :- !.
pfcFwd(P) :- pfcFwd1(P).

% pfcFwd1(+P) forward chains for a single fact.

% pfcFwd1(Fact) :- map_if_list(pfcFwd1,List),!.
pfcFwd1(Fact) :-
   pfcRuleOutcome(Fact,Outcome),!,
   loop_check_term(pfcFwd1_newoutcome(Fact),Outcome,true).

pfcRuleOutcome(_=>Outcome,Outcome).
pfcRuleOutcome(Outcome<=_,Outcome).
pfcRuleOutcome(Outcome:-_,Outcome).
pfcRuleOutcome(Outcome,Outcome).

pfcFwd1_newoutcome(Fact) :-
  fc_rule_check(Fact),
  copy_term(Fact,F),
  % check positive triggers
  pfcRunPT(Fact,F),
  % check negative triggers
  pfcRunNT(Fact,F).


%%
%= fc_rule_check(P) does some special, built in forward chaining if P is 
%= a rule.
%= 

fc_rule_check((P=>Q)) :-  
  !,  
  pfcProcessRule(P,Q,(P=>Q)).
fc_rule_check((Name::::P=>Q)) :- 
  !,  
  pfcProcessRule(P,Q,(Name::::P=>Q)).
fc_rule_check((P<=>Q)) :- 
  !, 
  pfcProcessRule(P,Q,(P<=>Q)), 
  pfcProcessRule(Q,P,(P<=>Q)).
fc_rule_check((Name :::: P <=> Q)) :- 
  !, 
  pfcProcessRule(P,Q,((Name::::P<=>Q))), 
  pfcProcessRule(Q,P,((Name::::P<=>Q))).

fc_rule_check(('<='(P,Q))) :-
  !,
  pfcDefineBcRule(P,Q,('<='(P,Q))).

fc_rule_check(_).


pfcRunPT(Fact,F) :- 
  pfcGetTriggerQuick(pfcPT(F,Body)),
  pfc_trace_msg('~n      Found positive trigger: ~q~n       body: ~q~n',
		[F,Body]),
  not_too_deep(pfcRunPT, pfcEvalLHS(Body,(Fact,pfcPT(F,Body)))),
  fail.

%pfcRunPT(Fact,F) :- 
%  pfcGetTriggerQuick(pfcPT(presently(F),Body)),
%  pfcEvalLHS(Body,(presently(Fact),pfcPT(presently(F),Body))),
%  fail.

pfcRunPT(_,_).

pfcRunNT(_Fact,F) :-
  support3(pfcNT(F,Condition,Body),X,_),
  Condition,
  pfcRem(X,(_,pfcNT(F,Condition,Body))),
  fail.
pfcRunNT(_,_).


%%
%= pfcDefineBcRule(+Head,+Body,+ParentRule) - defines a backeard
%= chaining rule and adds the corresponding pfcBT triggers to the database.
%%

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfcAtom(Head)),
  pfcWarn("Malformed backward chaining rule.  ~w not atomic.",[Head]),
  pfcWarn("rule: ~w",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfcBuildRhs(Head,Rhs),
  pfcForEach(pfc_nf(Body,Lhs),
          (pfcBuildTrigger(Lhs,rhs(Rhs),Trigger),
           pfcAdd(pfcBT(Head,Trigger),(ParentRuleCopy,pcfUser)))).
 


%%
%%
%= eval something on the LHS of a rule.
%%

 
pfcEvalLHS((Test->Body),Support) :-  
  !, 
  (pfc_call(nonPFC,Test) -> pfcEvalLHS(Body,Support)),
  !.

pfcEvalLHS(rhs(X),Support) :-
  !,
  pfc_eval_rhs(X,Support),
  !.

pfcEvalLHS(X,Support) :-
  pfcType(X,trigger),
  !,
  pfcAddTrigger(X,Support),
  !.

%pfcEvalLHS(snip(X),Support) :- 
%  snip(Support),
%  pfcEvalLHS(X,Support).

pfcEvalLHS(X,_) :-
  pfcWarn("Unrecognized item found in trigger body, namely ~w.",[X]).


%%
%= eval something on the RHS of a rule.
%%

pfc_eval_rhs([],_) :- !.
pfc_eval_rhs([Head|Tail],Support) :- 
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs(Tail,Support).


pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 pfcEvalAction(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfcNegatedLiteral(P),
 !,
 pfcRem(P).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs([X|Xrest],Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 pfcPost1(Assertion,Support).


pfc_eval_rhs1(X,_) :-
  pfcWarn("Malformed rhs of a rule: ~w",[X]).


%%
%= evaluate an action found on the rhs of a rule.
%%

pfcEvalAction(Action,Support) :-
  pfc_call(nonPFC,Action), 
  (pfcUndoable(Action) 
     -> pfcAddActionTrace(Action,Support) 
      ; true).


%%
%= 
%%

pfc_trigger_the_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
pfc_trigger_the_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfcBC(Trigger),
%  pfcEvalLHS(Body,(presently(Trigger),pfcPT(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfcBC(Trigger),
  pfcEvalLHS(Body,(Trigger,pfcPT(TriggerCopy,Body))),
  fail.



%%
%= pfcBC(F) is true iff F is a fact available for forward chaining.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%%

pfcBC(P) :-
  % trigger any bc rules.
  pfcBT(P,Trigger),
  pfcGetSupport(pfcBT(P,Trigger),S),
  pfcEvalLHS(Trigger,S),
  fail.

pfcBC(F) :-
  %= this is probably not advisable due to extreme inefficiency.
  var(F)    ->  pfcFact(F) ;
  otherwise ->  pfc_clause(F,Condition),pfc_call(nonPFC,Condition).

%%pfcBC(F) :- 
%=  %= we really need to check for system predicates as well.
%=  % current_predicate(_,F) -> pfc_call(nonPFC,F).
%=  pfc_clause(F,Condition),pfc_call(nonPFC,Condition).


% an action is pfcUndoable if there exists a method for undoing it.
pfcUndoable(A) :- pfcUndoMethod(A,_).



%%
%%
%= defining fc rules 
%%

%= pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form 
%= Out.  It also does certain optimizations.  Backtracking into this
%= predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  pfc_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


%= pfc_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%= Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

pfc_nf1(P/Cond,[(\+P)/Cond]) :- pfcNegatedLiteral(P), !.

pfc_nf1(P/Cond,[P/Cond]) :-  pfcAtom(P), !.

%= handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_negation(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

%= disjunction.

pfc_nf1((P;Q),NF) :- 
  !,
  (pfc_nf1(P,NF) ;   pfc_nf1(Q,NF)).


%= conjunction.

pfc_nf1((P,Q),NF) :-
  !,
  pfc_nf1(P,NF1),
  pfc_nf1(Q,NF2),
  append(NF1,NF2,NF).

%= handle a random atom.

pfc_nf1(P,[P]) :- 
  pfcAtom(P), 
  !.

%%= shouln't we have something to catch the rest as errors?
pfc_nf1(Term,[Term]) :-
  pfcWarn("pfc_nf doesn't know how to normalize ~w",[Term]).


%= pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
pfc_nf1_negation((P/Cond),[(\+(P))/Cond]) :- !.

pfc_nf1_negation((P;Q),NF) :-
  !,
  pfc_nf1_negation(P,NFp),
  pfc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

pfc_nf1_negation((P,Q),NF) :- 
  % this code is not correct! twf.
  !,
  pfc_nf1_negation(P,NF) 
  ;
  (pfc_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

pfc_nf1_negation(P,[\+P]).




%= pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%= changing ~{...} to {\+...}
%%= ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations(X,X) :- var(X),!.

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

pfc_nf_negation(Form,{PLNeg}) :-  
  nonvar(Form),
  correct_negations(callable,Form,PLNeg),
  !.
pfc_nf_negation(X,X).


%%
%= pfcBuildRhs(+Conjunction,-Rhs)
%%

pfcBuildRhs(X,[X]) :- 
  var(X),
  !.

pfcBuildRhs((A,B),[A2|Rest]) :- 
  !, 
  pfcCompileRhsTerm(A,A2),
  pfcBuildRhs(B,Rest).

pfcBuildRhs(X,[X2]) :-
   pfcCompileRhsTerm(X,X2).

pfcCompileRhsTerm((P/C),((P:-C))) :- !.
pfcCompileRhsTerm(P,P).


%= pfc_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

pfc_negation((~P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).
pfc_negation((naf(P)),P).
% pfc_negation(not(P)),P).

pfcNegatedLiteral(P) :- 
  pfc_negation(P,Q),
  pfcPositiveAtom(Q).

pfcAtom(X) :- pfcNegatedLiteral(X).
pfcAtom(X) :- pfcPositiveAtom(X).

pfcPositiveAtom(X) :-  
  functor(X,F,_), 
  \+ pfcConnective(F).

pfcConnective(';').
pfcConnective(',').
pfcConnective('/').
pfcConnective('|').
pfcConnective(('=>')).
pfcConnective(('<=')).
pfcConnective('<=>').

pfcConnective('-').
pfcConnective('~').
pfcConnective(('\\+')).

pfcProcessRule(Lhs,Rhs,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfcBuildRhs(Rhs,Rhs2),
  pfcForEach(pfc_nf(Lhs,Lhs2), 
          pfcBuild1Rule(Lhs2,rhs(Rhs2),(ParentRuleCopy,pcfUser))).

pfcBuild1Rule(Lhs,Rhs,Support) :-
  pfcBuildTrigger(Lhs,Rhs,Trigger),
  pfcEvalLHS(Trigger,Support).

pfcBuildTrigger([],Consequent,Consequent).

pfcBuildTrigger([V|Triggers],Consequent,pfcPT(V,X)) :-
  var(V),
  !, 
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([(T1/Test)|Triggers],Consequent,pfcNT(T2,Test2,X)) :-
  pfc_negation(T1,T2),
  !, 
  pfcBuildNtTest(T2,Test,Test2),
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([(T1)|Triggers],Consequent,pfcNT(T2,Test,X)) :-
  pfc_negation(T1,T2),
  !,
  pfcBuildNtTest(T2,true,Test),
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([T/Test|Triggers],Consequent,pfcPT(T,X)) :-
  !, 
  pfcBuildTest(Test,Test2),
  pfcBuildTrigger([{Test2}|Triggers],Consequent,X).


%pfcBuildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  pfcBuildTrigger(Triggers,Consequent,X).

pfcBuildTrigger([T|Triggers],Consequent,pfcPT(T,X)) :-
  !, 
  pfcBuildTrigger(Triggers,Consequent,X).

%%
%= pfcBuildNtTest(+,+,-).
%%
%= builds the test used in a negative trigger (pfcNT/3).  This test is a
%= conjunction of the check than no matching facts are in the db and any
%= additional test specified in the rule attached to this ~ term.
%%

pfcBuildNtTest(T,Testin,Testout) :-
  pfcBuildTest(Testin,Testmid),
  pfcConjoin((pfcBC(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

pfcBuildTest({Test},Test) :- !.
pfcBuildTest(Test,Test).

%%

mpred_listing(F/_):-!,term_listing(F).
mpred_listing(Pred):-
  (get_functor(Pred,F,A0),A0==0->ignore(mpred_arity(F,A));A=A0),
  mpred_listing(F/A).

%= simple typeing for pfc objects

pfcType(('=>'(_,_)),Type) :- !, Type=rule.
pfcType(('<=>'(_,_)),Type) :- !, Type=rule.
pfcType(('<='(_,_)),Type) :- !, Type=rule.
pfcType(pfcPT(_,_,_),Type) :- !, Type=trigger.
pfcType(pfcPT(_,_),Type) :- !, Type=trigger.
pfcType(pfcNT(_,_,_),Type) :- !,  Type=trigger.
pfcType(pfcBT(_,_),Type) :- !,  Type=trigger.
pfcType(pfcAction(_),Type) :- !, Type=action.
pfcType((('::::'(_,X))),Type) :- !, pfcType(X,Type).
pfcType(_,fact) :-
  %= if it's not one of the above, it must be a fact!
  !.

pfcAssertIfUnknown(P):- (show_call_success(is_asserted(P));(fail,show_call(prologCall(P)))),!.
pfcAssertIfUnknown(P):-show_call(pfc_assert(P)),!,sanity(is_asserted(P)).

pfcAssert(P,Support) :- 
  (pfc_clause(P) ; pfc_assert(P)),
  !,
  pfcAddSupport(P,Support).

pfcAsserta(P,Support) :-
  (pfc_clause(P) ; pfc_asserta(P)),
  !,
  pfcAddSupport(P,Support).

pfcAssertz(P,Support) :-
  (pfc_clause(P) ; pfc_assertz(P)),
  !,
  pfcAddSupport(P,Support).

pfc_clause((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  pfc_clause(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_clause(Head) :-
  % find a unit pfc_clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  pfc_clause(Head_copy,true),
  variant(Head,Head_copy).

pfcForEach(Binder,Body) :- Binder,pfcdo(Body),fail.
pfcForEach(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- X,!.
pfcdo(_).


%= pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
%= L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-  
  pfcUnion(Tail,L,Tail2).


%= pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
%= arg3 is a simplified expression representing the conjunction of
%= args 1 and 2.

pfcConjoin(true,X,X) :- !.
pfcConjoin(X,true,X) :- !.
pfcConjoin(C1,C2,(C1,C2)).

% pfcFile('pfcsupport').	% support maintenance

%%
%%
%= predicates for manipulating support relationships
%%

:-decl_mpred_pfc(support2/3).
:-decl_mpred_pfc(support1/3).
:-decl_mpred_pfc(support3/3).

%= pfcAddSupport(+Fact,+Support)

pfcAddSupport(P,(Fact,Trigger)) :-
  pfc_assert(support1(P,Fact,Trigger)),
  pfc_assert(support2(Fact,Trigger,P)),
  pfc_assert(support3(Trigger,P,Fact)).

pfcGetSupport(P,(Fact,Trigger)) :-
   nonvar(P)         -> support1(P,Fact,Trigger) 
   ; nonvar(Fact)    -> support2(Fact,Trigger,P) 
   ; nonvar(Trigger) -> support3(Trigger,P,Fact) 
   ; otherwise       -> support1(P,Fact,Trigger).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(P),
  !,
  pfcRetractOrWarn(support1(P,Fact,Trigger)),
  pfcRetractOrWarn(support2(Fact,Trigger,P)),
  pfcRetractOrWarn(support3(Trigger,P,Fact)).


pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(Fact),
  !,
  pfcRetractOrWarn(support2(Fact,Trigger,P)),
  pfcRetractOrWarn(support1(P,Fact,Trigger)),
  pfcRetractOrWarn(support3(Trigger,P,Fact)).

pfcRemSupport(P,(Fact,Trigger)) :-
  pfcRetractOrWarn(support3(Trigger,P,Fact)),
  pfcRetractOrWarn(support1(P,Fact,Trigger)),
  pfcRetractOrWarn(support2(Fact,Trigger,P)).


pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  support1(P,F,T).

pfc_make_supports((P,S1,S2)) :- 
  pfcAddSupport(P,(S1,S2),_),
  (pfcAddSome(P); true),
  !.

%= pfcTriggerKey(+Trigger,-Key) 
%%
%= Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey(pfcPT(Key,_),Key).
pfcTriggerKey(pfcPT(Key,_,_),Key).
pfcTriggerKey(pfcNT(Key,_,_),Key).
pfcTriggerKey(Key,Key).


%%^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger pfcBase1 pfc_clause that stores the trigger.
%%

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).



% pfcFile('thlocal').	% predicates to manipulate database.


%   File   : thlocal.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%%	restore, reset, etc.0 )

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm(support1/3).
pfcDatabaseTerm(support2/3).
pfcDatabaseTerm(support3/3).
pfcDatabaseTerm(pfcPT/3).
pfcDatabaseTerm(pfcBT/3).
pfcDatabaseTerm(pfcNT/4).
pfcDatabaseTerm('=>'/2).
pfcDatabaseTerm('<=>'/2).
pfcDatabaseTerm('<='/2).
pfcDatabaseTerm(pfcQueue/1).

% removes all forward chaining rules and pfcJustification_L from db.

pfcReset :-
  pfc_clause(support1(P,F,Trigger),true),
  pfcRetractOrWarn(P),
  pfcRetractOrWarn(support1(P,F,Trigger)),
  pfcRetractOrWarn(support2(F,Trigger,P)),
  pfcRetractOrWarn(support3(Trigger,P,F)),
  fail.
pfcReset :-
  pfcDatabaseItem(T),
  pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]).
pfcReset.

% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  pfc_clause(Term,_).

pfcRetractOrWarn(X) :-  pfc_retract(X), !.
pfcRetractOrWarn(X) :- 
  pfcWarn("Couldn't pfc_retract ~p.",[X]).



% pfcFile('pfcdebug').	% debugging aids (e.g. tracing).


%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- decl_mpred_pfc pfc_settings/2.
:- decl_mpred_pfc pfc_settings/3.

:- pfcDefault(pfc_settings(warnings,_), pfc_settings(warnings,true)).

%= predicates to examine the state of pfc

pfcQueue :- listing(pfcQueue/1).

pfcPrintDB :- current_predicate(must_det_l/1),!,
  must_det_l([
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
   pfcPrintSupports,
   pfcQueue]),!.

pfcPrintDB :-
   pfcPrintFacts,
   pfcPrintRules,
   pfcPrintTriggers,
  pfcPrintSupports,
  pfcQueue,!.

%= pfcPrintFacts ..

pfcPrintFacts :- pfcPrintFacts(_,true).

pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-
  pfcFacts(P,C,L),
  pfcClassifyFacts(L,User,Pfc,_Rule),
  format("~n~nUser added facts:",[]),
  pfcPrintitems(User),
  format("~n~nPfc added facts:",[]),
  pfcPrintitems(Pfc).


%= printitems clobbers it's arguments - beware!

pfcPrintitems([]).
pfcPrintitems([H|T]) :-
  numbervars(H,0,_),
  format("~n  ~w",[H]),
  pfcPrintitems(T).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,(pcfUser,pcfUser)),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).


printHeadItems(Head):-ignore((bagof(Head,pfc_clause(Head,true),R1),pfcPrintitems(R1))).
printHeadCallItems(Head):-ignore((bagof(Head,pfc_clause(Head,true),R1),pfcPrintitems(R1))).

pfcPrintRules :-
  printHeadItems((P=>Q)),printHeadItems((P<=>Q)),printHeadItems((P<=Q)).

pfcPrintTriggers :-
  format("Positive triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcPT(T,B))),
  format("Negative triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcNT(T,B))),
  format("Goal triggers...~n",[]),
     printHeadCallItems(pfcGetTrigger(pfcBT(T,B))),!.

pfcPrintSupports :- 
  % temporary hack.
  setof((S > P), pfcGetSupport(P,S),L),
  pfcPrintitems(L).

%= pfcFact(P) is true if fact P was asserted into the database via add.

pfcFact(P) :- pfcFact(P,true).

%= pfcFact(P,C) is true if fact P was asserted into the database via
%= add and contdition C is satisfied.  For example, we might do:
%= 
%=  pfcFact(X,pfcUserFact(X))
%%

pfcFact(P,C) :- 
  pfcGetSupport(P,_),
  pfcType(P,fact),
  pfc_call(nonPFC,C).

%= pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

%= pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof(P,pfcFact(P,C),L).

brake(X) :-  X, break.

%%
%%
%= predicates providing a simple tracing facility
%%

pfcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(o,o)).

pfcTraceAdd(pfcPT(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfcTraceAdd(pfcNT(_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfcTraceAdd(P,S) :-
   pfcTraceAddPrint(P,S),
   pfcTraceBreak(P,S).
   

pfcTraceAddPrint(P,S) :-
  pfc_settings(traced,P),
  !,
  copy_term(P,Pcopy),
  numbervars(Pcopy,0,_),
  (S=(pcfUser,pcfUser)
       -> format("~nAdding (u) ~w",[Pcopy])
        ; format("~nAdding (g) ~w",[Pcopy])).

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  pfc_settings(spied,P,add) -> 
   (copy_term(P,Pcopy),
    numbervars(Pcopy,0,_),
    format("~nBreaking on pfcAdd(~w)",[Pcopy]),
    break)
   ; true.

pfcTraceRem(pfcPT(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem(pfcNT(_,_)) :-
  % hack for now - never trace triggers.
  !.

pfcTraceRem(P) :-
  (pfc_settings(traced,P) 
     -> format('~nRemoving ~w.',[P])
      ; true),
  (pfc_settings(spied,P,pfcRem)
   -> (format("~nBreaking on pfcRem(~w)",[P]),
       break)
   ; true).


pfcTrace :- pfcTrace(_).

pfcTrace(Form) :-
  pfc_assert(pfc_settings(traced,Form)).

pfcTrace(Form,Condition) :- 
  pfc_assert((pfc_settings(traced,Form) :- Condition)).

pfcSpy(Form) :- pfcSpy(Form,[add,pfcRem],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[add,pfcRem],Condition) :-
  !,
  pfcSpy1(Form,add,Condition),
  pfcSpy1(Form,pfcRem,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  pfc_assert((pfc_settings(spied,Form,Mode) :- Condition)).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :- 
  pfc_clause_ref(pfc_settings(spied,Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- pfc_retractall(pfc_settings(traced,Form)).

% needed:  pfcTraceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :-
    pfc_settings(trace_exec,true),
    !,
    format(user_output, Msg, Args).
pfc_trace_msg(_Msg,_Args).

pfcWatch :- pfc_assert(pfc_settings(trace_exec,true)).

pfcNoWatch :-  pfc_retractall(pfc_settings(trace_exec,true)).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :- 
  format("~nERROR/Pfc: ",[]),
  format(Msg,Args).


%%
%= These control whether or not warnings are printed at all.
%=   pfcWarn.
%=   nopfcWarn.
%%
%= These print a warning message if the flag pfcWarnings is set.
%=   pfcWarn(+Message)
%=   pfcWarn(+Message,+ListOfArguments)
%%

pfcWarn :- 
  pfc_retractall(pfc_settings(warnings,_)),
  pfc_assert(pfc_settings(warnings,true)).

nopfcWarn :-
  pfc_retractall(pfc_settings(warnings,_)),
  pfc_assert(pfc_settings(warnings,false)).
 
pfcWarn(Msg) :-  pfcWarn(Msg,[]).

pfcWarn(Msg,Args) :- 
  pfc_settings(warnings,true),
  !,
  format("~nWARNING/Pfc: ",[]),
  format(Msg,Args).
pfcWarn(_,_).

%%
%= pfcWarnings/0 sets flag to cause pfc warning messages to print.
%= pfcNoWarnings/0 sets flag to cause pfc warning messages not to print.
%%

pfcWarnings :- 
  pfc_retractall(pfc_settings(warnings,_)),
  pfc_assert(pfc_settings(warnings,true)).

pfcNoWarnings :- 
  pfc_retractall(pfc_settings(warnings,_)).



% pfcFile('pfcjust').	% predicates to manipulate pfcJustification_L.


%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc Justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

pfcJustificationDB(F,J) :- justSupports(F,J).

pfcJustification_L(F,Js) :- bagof(J,pfcJustificationDB(F,J),Js).


justSupports(F,J):- support2(F,J).

%= pfcBase1(P,L) - is true iff L is a list of "pfcBase1" facts which, taken
%= together, allows us to deduce P.  A pfcBase1 fact is an pfcAxiom (a fact 
%= added by the pcfUser or a raw Prolog fact (i.e. one w/o any support))
%= or an assumption.

pfcBase1(F,[F]) :- (pfcAxiom(F) ; pfcAssumptionBase(F)),!.

pfcBase1(F,L) :-
  % i.e. (reduce 'append (map 'pfcBase1 (pfcJustificationDB f)))
  pfcJustificationDB(F,Js),
  pfcBases(Js,L).


%= pfcBases(L1,L2) is true if list L2 represents the union of all of the 
%= facts on which some conclusion in list L1 is based.

pfcBases([],[]).
pfcBases([X|Rest],L) :-
  pfcBase1(X,Bx),
  pfcBases(Rest,Br),
  pfcUnion(Bx,Br,L).
	
pfcAxiom(F) :- 
  pfcGetSupport(F,(pcfUser,pcfUser)); 
  pfcGetSupport(F,(pfcGod,pfcGod)).

%= an pfcAssumptionBase/1''s G was a failed goal, i.e. were assuming that our failure to 
%= prove P is a proof of not(P)

pfcAssumptionBase(P) :- pfc_negation(P,_).
   
%= pfcAssumptionsSet(X,As) if As is a set of assumptions which underly X.

pfcAssumptionsSet(X,[X]) :- pfcAssumptionBase(X).
pfcAssumptionsSet(X,[]) :- pfcAxiom(X).
pfcAssumptionsSet(X,L) :-
  pfcJustificationDB(X,Js),
  pfcAssumption1(Js,L).

pfcAssumption1([],[]).
pfcAssumption1([X|Rest],L) :-
  pfcAssumptionsSet(X,Bx),
  pfcAssumption1(Rest,Br),
  pfcUnion(Bx,Br,L).  


%= pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%= of the form
%%
%=     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%=          ^                         and has the form of
%=          [J11, J12,... J1n]      a list of proof trees.


% pfcChild(P,Q) is true iff P is an immediate justifier for Q.
% mode: pfcChild(+,?)

pfcChild(P,Q) :-
  pfcGetSupport(Q,(P,_)).

pfcChild(P,Q) :-
  pfcGetSupport(Q,(_,Trig)),
  pfcType(Trig,trigger),
  pfcChild(P,Trig).

pfcChildren(P,L) :- bagof(C,pfcChild(P,C),L).

% pfcDescendant(P,Q) is true iff P is a justifier for Q.

pfcDescendant(P,Q) :- 
   pfcDescendant1(P,Q,[]).

pfcDescendant1(P,Q,Seen) :-
  pfcChild(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfcDescendant1(P,X,[X|Seen])).
  
pfcDescendants(P,L) :- 
  bagof(Q,pfcDescendant1(P,Q,[]),L).



% pfcFile('pfcwhy').	% interactive exploration of pfcJustification_L.



%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc pfcJustification_L.

% ***** predicates for brousing pfcJustification_L *****

:- use_module(library(lists)).

pfcWhy :- 
  pfcWhyMemory1(P,_),
  pfcWhy(P).

pfcWhy(N) :-
  number(N),
  !,
  pfcWhyMemory1(P,Js),
  pfcWhyCommand(N,P,Js).

pfcWhy(P) :-
  pfcJustification_L(P,Js),
  pfc_retractall(pfcWhyMemory1(_,_)),
  pfc_assert(pfcWhyMemory1(P,Js)),
  pfcWhyBrouse(P,Js).

pfcWhy1(P) :-
  pfcJustification_L(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhyBrouse(P,Js) :-
  pfcShowJustifications(P,Js),
  pfcAskUser(' >> ',Answer),
  pfcWhyCommand(Answer,P,Js).

pfcWhyCommand(q,_,_) :- !.
pfcWhyCommand(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth pfcJustificationDB.
 N.M brouse step M of the Nth pfcJustificationDB
 u   up a level
",[]).

pfcWhyCommand(N,_P,Js) :-
  float(N),
  !,
  pfcSelectJustificationNode(Js,N,Node),
  pfcWhy1(Node).

pfcWhyCommand(u,_,_) :-
  % u=up
  !.

pfcCommand(N,_,_) :-
  integer(N),
  !,
  format("~n~w is a yet unimplemented command.",[N]),
  fail.

pfcCommand(X,_,_) :-
 format("~n~w is an unrecognized command, enter h. for help.",[X]),
 fail.
  
pfcShowJustifications(P,Js) :-
  format("~nJustifications for ~w:",[P]),
  pfcShowJustification1(Js,1).

pfcShowJustification1([],_).

pfcShowJustification1([J|Js],N) :-
  % show one pfcJustificationDB and recurse.
  nl,
  pfcShowJustifications2(J,N,1),
  N2 is N+1,
  pfcShowJustification1(Js,N2).

pfcShowJustifications2([],_,_).

pfcShowJustifications2([C|Rest],JustNo,StepNo) :- 
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  format("~n    ~w.~w ~w",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  pfcShowJustifications2(Rest,JustNo,StepNext).

pfcAskUser(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth(StepNo,Justification,Step).


user:decl_database_hook(change(assert,_),Rule):- not(pfcType(Rule,fact)),pfcAdd(Rule).
user:decl_database_hook(change(assert,_),Fact):- pfcType(Fact,fact),pfcAdd(Fact).
user:decl_database_hook(change(retract,_),FactOrRule):- pfcRem(FactOrRule). % pfcRem2 is too forcefull

:- if_startup_script(with_assertions(thlocal:pfcExpansion,ensure_loaded(dbase_i_mpred_pfc_testing))).

:- if_startup_script(prolog).


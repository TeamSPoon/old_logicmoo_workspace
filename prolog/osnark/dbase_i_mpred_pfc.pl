%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure

:- use_module('../../../src_lib/logicmoo_util/logicmoo_util_all.pl').


:-ignore((current_predicate(not/1),abolish(system:not/1),abolish(not/1),dynamic(not/1),assert(((not(P):- nonvar(P), \+ P))))).
:-meta_predicate(not(0)).

:- op(0,fx,'decl_mpred_pfc').
decl_mpred_pfc(F/A):-mpred_prop(F,prologHybrid),!. %,asserta_if_new(mpred_prop(F,isPfc)).
decl_mpred_pfc(F/A):-mpred_prop(F,_),!. %
decl_mpred_pfc(F/A):-!,decl_mpred(F/A),dynamic(F/A),asserta_if_new(mpred_prop(prologOnly)),asserta_if_new(mpred_prop(F,isPfc)).
decl_mpred_pfc(F):-trace,atom(F),!,decl_mpred_pfc(F/0).
:- op(1150,fx,'decl_mpred_pfc').

%:- include('../../../src_mud/vworld/moo_header.pl').
:- thread_local thlocal:pfcExpansion.
:- thread_local thlocal:pfcExpansionWas.

:-dynamic p.
:-dynamic x.
:-decl_mpred_pfc q.
:-decl_mpred_pfc fly/1.

:- decl_mpred_pfc old_clausedb/0.
:- decl_mpred_pfc local_clause/1.
:- decl_mpred_pfc old_call/0.
% :- decl_mpred_pfc use_if_modify_new/0.

local_clause(G):-get_functor(G,F),mpred_prop(F,isPfc).

old_clausedb.
% local_clause(_).
% old_call.

db_retractall(G):-local_clause(G),!,retractall(G).
db_retractall(G):-database_modify(retract(all),G).
db_retract(G):- local_clause(G),!,retract(G).
db_retract(G):-database_modify(retract(one),G).
db_assertz(G):-local_clause(G),!,assertz(G).
db_assertz(G):-database_modify(assert(z),G).
db_asserta(G):-local_clause(G),!,asserta(G).
db_asserta(G):-database_modify(assert(a),G).
db_assert(G):-local_clause(G),!,assert(G).
db_assert(G):-database_modify(assert(z),G).
db_clause(H,B,Ref):-local_clause(H),!,clause(H,B,Ref).
db_clause(H,B,Ref):-database_check(clauses(is_asserted),clause(H,B,Ref)).
db_clause(H,B):-local_clause(H),!,clause(H,B).
db_clause(H,B):-database_check(clauses(is_asserted),clause(H,B)).
db_call(G):-db_call(nonPFC,G).
db_call(_,G):- local_clause(G),!,predicate_property(G,_),!, debugOnError(call(G)).
db_call(What,X):-database_call(call(What),X).

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

:- multifile('pfc_term_expansion'/2).

pfc_term_expansion((P=>Q),(:- pfcAdd((P=>Q)))).
%pfc_term_expansion((P=>Q),(:- pfcAdd(('<='(Q,P))))).  % speed-up attempt
pfc_term_expansion(('<='(P,Q)),(:- pfcAdd(('<='(P,Q))))).
pfc_term_expansion((P<=>Q),(:- pfcAdd((P<=>Q)))).
pfc_term_expansion((RuleName :::: Rule),(:- pfcAdd((RuleName :::: Rule)))).
pfc_term_expansion((=>P),(:- pfcAdd(P))).

:- multifile(term_expansion/2).
term_expansion(A,B):- once(true ; thlocal:pfcExpansion), once(pfc_term_expansion(A,B)),A\=@=B.

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
%:- decl_mpred_pfc pfcTmsMode/1.
:- decl_mpred_pfc pfcQueue/1.
:- decl_mpred_pfc pfcDatabase/1.
:- decl_mpred_pfc pfcHaltSignal/0.
%:- decl_mpred_pfc pfcDebugging/0.
%:- decl_mpred_pfc pfcSelect/1.
%:- decl_mpred_pfc pfcSearch/1.

%%= initialization of global assertons 

%= pfcDefault/1 initialized a global assertion.
%=  pfcDefault(P,Q) - if there is any fact unifying with P, then do 
%=  nothing, else db_assert Q.

pfcDefault(GeneralTerm,Default) :-
  db_clause(GeneralTerm,true) -> true ; db_assert(Default).

%= pfcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfcDefault(pfc_settings(tmsMode,_), pfc_settings(tmsMode,cycles)).

% Pfc Search strategy. pfc_settings(searchMode,X) where X is one of {direct,depth,breadth}
:- pfcDefault(pfc_settings(searchMode,_), pfc_settings(searchMode,direct)).


% 

%= add/2 and pfcPost/2 are the main ways to db_assert new clauses into the
%= database and have forward reasoning done.

%= pfcAdd(P,S) asserts P into the dataBase with support from S.

pfcAdd(P) :-  pfcAdd(P,(pcfUser,pcfUser)).

pfcAdd((=>P),S) :- pfcAdd(P,S).

pfcAdd(P,S) :- 
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
  db_assert(P),
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
  \+ db_clause(Head,Tail).
pfcUnique(P) :-
  !,
  \+ db_clause(P,true).


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
  var(identifier),
  !,
  pfcWarn("variable used as an  rule name in ~w :::: ~w",
          [Identifier,Body]).

  
pfcRemoveOldVersion((Identifier::::Body)) :-
  nonvar(Identifier),
  db_clause((Identifier::::OldBody),_),
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
     ; db_assert(pfcHaltSignal).


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

pfcGetTriggerQuick(Trigger) :-  db_clause(Trigger,true).

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
  %= db_retract an arbitrary thing.
  pfcType(X,Type),
  pfcRetractType(Type,X),
  !.

pfcRetractType(fact,X) :-   
  %= db pfcAddDbToHead(X,X2), db_retract(X2). 
  db_retract(X).

pfcRetractType(rule,X) :- 
  %= db  pfcAddDbToHead(X,X2),  db_retract(X2).
  db_retract(X).
pfcRetractType(trigger,X) :- 
  db_retract(X)
    -> unFc(X)
     ; pfcWarn("Trigger not found to db_retract: ~w",[X]).

pfcRetractType(action,X) :- pfcRemActionTrace(X).
  

%= pfcAddSome(X) adds item X to some database

pfcAddSome(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  % db_call the appropriate predicate.
  pfcAddType(Type,X).

pfcAddType(fact,X) :- 
  pfcUnique(X), 
  db_assert(X),!.
pfcAddType(rule,X) :- 
  pfcUnique(X), 
  db_assert(X),!.
pfcAddType(trigger,X) :- 
  db_assert(X).
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
  (db_retract(pfcPT(Key,Head,Body))
    -> unFc(pfcPT(Head,Body))
     ; pfcWarn("Trigger not found to db_retract: ~w",[pfcPT(Head,Body)])).

pfcUndo(pfcNT(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (db_retract(pfcNT(Head,Condition,Body))
    -> unFc(pfcNT(Head,Condition,Body))
     ; pfcWarn("Trigger not found to db_retract: ~w",[pfcNT(Head,Condition,Body)])).

pfcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  db_retract(Fact),
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
  supports(F,Supporters),
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

supports(F,[Fact|MoreFacts]) :-
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
  (db_call(nonPFC,Test) -> pfcEvalLHS(Body,Support)),
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
  db_call(nonPFC,Action), 
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



:- ensure_loaded(dbase_i_pfc_testing).

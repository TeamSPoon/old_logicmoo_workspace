%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- op(500,fx,'-').
:- op(300,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

pfcCurrentDb(fooo).

 :- meta_predicate brake(0).
 :- meta_predicate fcEvalAction(0,*).
 :- meta_predicate pfcForEach(0,*).
 :- meta_predicate pfcdo(0).
 :- meta_predicate pfcFact(*,0).

:- multifile('term_expansion'/2).
:- dynamic((spft/3,whymemory/2)).

term_expansion((P==>Q),(:- add((P==>Q)))).
%term_expansion((P==>Q),(:- add(('<-'(Q,P))))).  % speed-up attempt
term_expansion(('<-'(P,Q)),(:- add(('<-'(P,Q))))).
term_expansion((P<==>Q),(:- add((P<==>Q)))).
term_expansion((RuleName :::: Rule),(:- add((RuleName :::: Rule)))).
term_expansion((==>P),(:- add(P))).

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

:- use_module(library(lists)).

:- dynamic ('==>')/2.
:- dynamic ('::::')/2.
:- dynamic '<==>'/2.
:- dynamic '<-'/2.
:- dynamic 'pt'/2.
:- dynamic 'nt'/3.
:- dynamic 'bt'/2.
:- dynamic fcUndoMethod/2.
:- dynamic fcAction/2.
:- dynamic fcTmsMode/1.
:- dynamic pfcQueue/1.
:- dynamic pfcDatabase/1.
:- dynamic pfcHaltSignal/0.
:- dynamic pfcDebugging/0.
:- dynamic pfcSelect/1.
:- dynamic pfcSearch/1.

% % initialization of global assertons 

%  pfcDefault/1 initialized a global assertion.
%   pfcDefault(P,Q) - if there is any fact unifying with P, then do 
%   nothing, else assert Q.

pfcDefault(GeneralTerm,Default) :-
  clause(GeneralTerm,true) -> true ; assert(Default).

%  fcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfcDefault(fcTmsMode(_), fcTmsMode(cycles)).

% Pfc Search strategy. pfcSearch(X) where X is one of {direct,depth,breadth}
:- pfcDefault(pfcSearch(_), pfcSearch(direct)).


% 

%  add/2 and post/2 are the main ways to assert new clauses into the
%  database and have forward reasoning done.

%  add(P,S) asserts P into the dataBase with support from S.

add(P) :-  add(P,(user,user)).

add(( \+ P ), S) :- rem(P, S).
add((==>P),S) :- add(P,S).

add(P,S) :- 
  post(P,S),
  pfcRun.

%add(_,_).
%add(P,S) :- pfcWarn("add(~p,~p) failed",[P,S]).


% post(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) post1 is called. It always succeeds.

post([H|T],S) :-
  !,
  post1(H,S),
  post(T,S).
post([],_) :- !.
post(P,S) :- post1(P,S).


% post1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

post1(P,S) :- 
  %  db pfcAddDbToHead(P,P2),
  % pfcRemoveOldVersion(P),
  pfcAddSupport(P,S),
  pfcUnique(P),
  assert(P),
  pfcAddSpecialSupport(P,S),
  !,
  pfcEnqueue(P,S),
  !.

post1(_,_).
% post1(P,S) :-  pfcWarn("add(~p,~p) failed",[P,S]).

% 
%  pfcAddDbToHead(+P,~NewP) talkes a fact P or a conditioned fact
%  (P:-C) and adds the Db context.
% 

pfcAddDbToHead(P,NewP) :-
  pfcCurrentDb(Db),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).




% pfcUnique(X) is true if there is no assertion X in the prolog db.

pfcUnique((Head:-Tail)) :- 
  !, 
  \+ pfc_clause_unique(Head,Tail).
pfcUnique(P) :-
  !,
  \+ pfc_clause_unique(P,true).


pfcEnqueue(P,S) :-
  pfcSearch(Mode) 
    -> (Mode=direct  -> fc(P) ;
	Mode=depth   -> pfcAsserta(pfcQueue(P),S) ;
	Mode=breadth -> pfcAssert(pfcQueue(P),S) ;
	true         -> pfcWarn("Unrecognized pfcSearch mode: ~p", Mode))
     ; pfcWarn("No pfcSearch mode").


% if there is a rule of the form Identifier ::: Rule then delete it.

pfcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  var(identifier),
  !,
  pfcWarn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]).

  
pfcRemoveOldVersion((Identifier::::Body)) :-
  nonvar(Identifier),
  clause((Identifier::::OldBody),_),
  \+(Body=OldBody),
  rem((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion(_).



% 

% pfcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfcQueue mechanism.

pfcRun :-
  (\+ pfcSearch(direct)),
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
  pfcdo(fc(P)),
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
  brake(format("~Npfc:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
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
     ; assert(pfcHaltSignal).


stop_trace(Msg):- notrace((tracing,leash(+all),dtrace(dmsg(Msg)))),!,rtrace.
stop_trace(Msg):- dtrace(dmsg(Msg)).

% 
% 
%  predicates for manipulating triggers
% 


pfcAddTrigger(pt(Trigger,Body),Support) :-
  !,
   pfc_trace_msg('~N~n\tAdding positive~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',
                 [Trigger,Body,Support]),
  pfcAssert(pt(Trigger,Body),Support),
  copy_term(pt(Trigger,Body),Tcopy),
  pfc_BC(Trigger),
  fcEvalLHS(Body,(Trigger,Tcopy)),
  fail.


pfcAddTrigger(nt(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('~N~n\tAdding negative~n\t\ttrigger: ~p~n\t\ttest: ~p~n\t\tbody: ~p~n\t Support: ~p~n',
		[Trigger,Test,Body,Support]),
  copy_term(Trigger,TriggerCopy),  
  pfcAssert(nt(TriggerCopy,Test,Body),Support),
 %  stop_trace(pfcAssert(nt(TriggerCopy,Test,Body),Support)),
  \+Test,
  fcEvalLHS(Body,((\+Trigger),nt(TriggerCopy,Test,Body))).

pfcAddTrigger(bt(Trigger,Body),Support) :-
  !,
   pfc_trace_msg('~N~n\tAdding backwards~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',
                 [Trigger,Body,Support]),
  pfcAssert(bt(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body,Support).

pfcAddTrigger(X,_Support) :-
  pfcWarn("Unrecognized trigger to pfcAddtrigger: ~p",[X]).


pfcBtPtCombine(Head,Body,Support) :- 
  %  a backward trigger (bt) was just added with head and Body and support Support
  %  find any pt's with unifying heads and add the instantied bt body.
  pfcGetTriggerQuick(pt(Head,_PtBody)),
  fcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  clause(Trigger,true).

% 
% 
%  predicates for manipulating action traces.
% 

pfcAddActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  pfcAddSupport(pfcAction(Action),Support).

pfcRemActionTrace(pfcAction(A)) :-
  fcUndoMethod(A,M),
  M,
  !.


% 
%  predicates to remove pfc facts, triggers, action traces, and queue items
%  from the database.
% 

pfcRetract(X) :- 
  %  retract an arbitrary thing.
  pfcType(X,Type),!,
  pfcRetractType(Type,X),
  !.

pfcRetractType(fact,X) :-   
  %  db pfcAddDbToHead(X,X2), retract(X2). 
  stop_trace(pfcRetractType(fact,X)),
  (retract(X) 
   *-> unFc(X) ; unFc(X)).

pfcRetractType(rule,X) :- 
  %  db  pfcAddDbToHead(X,X2),  retract(X2).
  retract(X).

pfcRetractType(trigger,X) :- 
  retract(X)
    -> unFc(X)
     ; pfcWarn("Trigger not found to retract: ~p",[X]).

pfcRetractType(action,X) :- pfcRemActionTrace(X).
  

%  pfcAdd(X) adds item X to some database

pfcAdd(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  % call the appropriate predicate.
  pfcAddType(Type,X).

pfcAddType(fact,X) :- 
  pfcUnique(X), 
  assert(X),!.
pfcAddType(rule,X) :- 
  pfcUnique(X), 
  assert(X),!.
pfcAddType(trigger,X) :- 
  assert(X).
pfcAddType(action,_Action) :- !.


  

%  rem(P,S) removes support S from P and checks to see if P is still supported.
%  If it is not, then the fact is retreactred from the database and any support
%  relationships it participated in removed.

rem(List) :- 
  % iterate down the list of facts to be rem'ed.
  nonvar(List),
  List=[_|_],
  remlist(List).
  
rem(P) :- 
  % rem/1 is the user's interface - it withdraws user support for P.
  rem(P,(user,user)).

remlist([H|T]) :-
  % rem each element in the list.
  rem(H,(user,user)),
  remlist(T).

rem(P,S) :-
  pfc_trace_msg('~N~n\tRemoving~n\t\tsupport: ~p~n\t\tfrom: ~p~n',[S,P]),
  pfcRemSupport(P,S)
     -> removeIfUnsupported(P)
      ; pfcWarn("rem/2 Could not find support ~p to remove from fact ~p",
                [S,P]).

% 
%  rem2 is like rem, but if P is still in the DB after removing the
%  user's support, it is retracted by more forceful means (e.g. remove).
% 

rem2(P) :- 
  % rem2/1 is the user's interface - it withdraws user support for P.
  rem2(P,(user,user)).

rem2(P,S) :-
  rem(P,S),
  pfc_BC(P)
     -> remove(P) 
      ; true.

% 
%  remove(+F) retracts fact F from the DB and removes any dependent facts */
% 

remove(F) :- 
  pfcRemoveSupports(F),
  fcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :- 
  pfcRemSupport(F,S),
  pfcWarn("~p was still supported by ~p",[F,S]),
  fail.
pfcRemoveSupports(_).

pfcRemoveSupportsQuietly(F) :- 
  pfcRemSupport(F,_),
  fail.
pfcRemoveSupportsQuietly(_).

% fcUndo(X) undoes X.


fcUndo(pfcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).

fcUndo(pt(Key,Head,Body)) :-  
  % undo a positive trigger.
  %
  !,
  (show_success(retract(pt(Key,Head,Body)))
    -> unFc(pt(Head,Body))
     ; pfcWarn("Trigger not found to retract: ~p",[pt(Head,Body)])).

fcUndo(nt(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (show_success(retract(nt(Head,Condition,Body)))
    -> unFc(nt(Head,Condition,Body))
     ; pfcWarn("Trigger not found to retract: ~p",[nt(Head,Condition,Body)])).

fcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract(Fact),
  pfcTraceRem(Fact),
  proper_unFc(Fact).


% proper_unFc(Fact):- atom(Fact),!,unFc(Fact).
proper_unFc(Fact):- unFc(Fact).
  


%  unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
%  been removed from the database, so remove all support relations it
%  participates in and check the things that they support to see if they
%  should stayu in the database or should also be removed.


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
  nt(Fcopy,Condition,Action),
  (\+ Condition),
  fcEvalLHS(Action,((\+F),nt(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractSupportRelations(Fact) :-
  pfcType(Fact,Type),
  (Type=trigger -> pfcRemSupport(P,(_,Fact))
                ; pfcRemSupport(P,(Fact,_))),
  removeIfUnsupported(P),
  fail.
pfcRetractSupportRelations(_).



%  removeIfUnsupported(+P) checks to see if P is supported and removes
%  it from the DB if it is not.

removeIfUnsupported(P) :- 
   fcSupported(P) -> true ;  fcUndo(P).


%  fcSupported(+P) succeeds if P is "supported". What this means
%  depends on the TMS mode selected.

fcSupported(P) :- 
  fcTmsMode(Mode),
  pfcSupported(Mode,P).

pfcSupported(local,P) :- !, pfcGetSupport(P,_).
pfcSupported(cycles,P) :-  !, wellFounded(P).
pfcSupported(_,_P) :- true.


% 
%  a fact is well founded if it is supported by the user
%  or by a set of facts and a rules, all of which are well founded.
% 

wellFounded(Fact) :- wf(Fact,[]).

wf(F,_) :-
  % supported by user (axiom) or an "absent" fact (assumption).
  (axiom(F) ; assumption(F)),
  !.

wf(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supports(F,Supporters),
  % all of whose members are well founded.
  wflist(Supporters,[F|Descendants]),
  !.

%  wflist(L) simply maps wf over the list.

wflist([],_).
wflist([X|Rest],L) :-
  wf(X,L),
  wflist(Rest,L).



% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [user].

supports(F,[Fact|MoreFacts]) :-
  pfcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(user,[]) :- !.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).


% 
% 
%  fc(X) forward chains from a fact or a list of facts X.
% 


fc([H|T]) :- !, fc1(H), fc(T).
fc([]) :- !.
fc(P) :- fc1(P).

% fc1(+P) forward chains for a single fact.

fc1(Fact) :-
  fc_rule_check(Fact),
  copy_term(Fact,F),
  % check positive triggers
  fcpt(Fact,F),
  % check negative triggers
  fcnt(Fact,F).


% 
%  fc_rule_check(P) does some special, built in forward chaining if P is 
%  a rule.
%  

fc_rule_check((P==>Q)) :-  
  !,  
  processRule(P,Q,(P==>Q)).
fc_rule_check((Name::::P==>Q)) :- 
  !,  
  processRule(P,Q,(Name::::P==>Q)).
fc_rule_check((P<==>Q)) :- 
  !, 
  processRule(P,Q,(P<==>Q)), 
  processRule(Q,P,(P<==>Q)).
fc_rule_check((Name::::P<==>Q)) :- 
  !, 
  processRule(P,Q,((Name::::P<==>Q))), 
  processRule(Q,P,((Name::::P<==>Q))).

fc_rule_check(('<-'(P,Q))) :-
  !,
  pfcDefineBcRule(P,Q,('<-'(P,Q))).

fc_rule_check(_).


fcpt(Fact,F) :- 
  pfcGetTriggerQuick(pt(F,Body)),
  pfc_trace_msg('~N~n\tFound positive trigger: ~p~n\t\tbody: ~p~n',
		[F,Body]),
  fcEvalLHS(Body,(Fact,pt(F,Body))),
  fail.

%fcpt(Fact,F) :- 
%  pfcGetTriggerQuick(pt(presently(F),Body)),
%  fcEvalLHS(Body,(presently(Fact),pt(presently(F),Body))),
%  fail.

fcpt(_,_).

fcnt(_Fact,F) :-
  spft(X,_,nt(F,Condition,Body)),
  Condition,
  rem(X,(_,nt(F,Condition,Body))),
  fail.
fcnt(_,_).


% 
%  pfcDefineBcRule(+Head,+Body,+ParentRule) - defines a backeard
%  chaining rule and adds the corresponding bt triggers to the database.
% 

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfcAtom(Head)),
  pfcWarn("Malformed backward chaining rule.  ~p not atomic.",[Head]),
  pfcWarn("rule: ~p",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Head,Rhs),
  pfcForEach(pfc_nf(Body,Lhs),
          (buildTrigger(Lhs,rhs(Rhs),Trigger),
           add(bt(Head,Trigger),(ParentRuleCopy,user)))).
 


% 
% 
%  eval something on the LHS of a rule.
% 

 
fcEvalLHS((Test->Body),Support) :-  
  !, 
  (call(Test) -> fcEvalLHS(Body,Support)),
  !.

fcEvalLHS(rhs(X),Support) :-
  !,
  pfc_eval_rhs(X,Support),
  !.

fcEvalLHS(X,Support) :-
  pfcType(X,trigger),
  !,
  pfcAddTrigger(X,Support),
  !.

%fcEvalLHS(snip(X),Support) :- 
%  snip(Support),
%  fcEvalLHS(X,Support).

fcEvalLHS(X,_) :-
  pfcWarn("Unrecognized item found in trigger body, namely ~p.",[X]).


% 
%  eval something on the RHS of a rule.
% 

pfc_eval_rhs([],_) :- !.
pfc_eval_rhs([Head|Tail],Support) :- 
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs(Tail,Support).


pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 fcEvalAction(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfcNegatedLiteral(P),
 !,
 rem(P).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs([X|Xrest],Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 post1(Assertion,Support).


pfc_eval_rhs1(X,_) :-
  pfcWarn("Malformed rhs of a rule: ~p",[X]).


% 
%  evaluate an action found on the rhs of a rule.
% 

fcEvalAction(Action,Support) :-
  call(Action), 
  (undoable(Action) 
     -> pfcAddActionTrace(Action,Support) 
      ; true).


% 
%  
% 

trigger_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfc_BC(Trigger),
%  fcEvalLHS(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfc_BC(Trigger),
  fcEvalLHS(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.



% 
%  pfc_BC(F) is true iff F is a fact available for forward chaining (from the backchaining store)
%  Note that this has the side effect of catching unsupported facts and
%  assigning them support from God.
% 

pfc_BC(P) :-
  % trigger any bc rules.
  bt(P,Trigger),
  pfcGetSupport(bt(P,Trigger),S),
  fcEvalLHS(Trigger,S),
  fail.

pfc_BC(P):- pfc_CALL(P).

pfc_CALL(F) :-
  %  this is probably not advisable due to extreme inefficiency.
  var(F)    ->  pfcFact(F) ;
  otherwise ->  clause(F,Condition),call(Condition).

% pfc_CALL(F) :- 
%   %  we really need to check for system predicates as well.
%   % current_predicate(_,F) -> call(F).
%   clause(F,Condition),call(Condition).


% an action is undoable if there exists a method for undoing it.
undoable(A) :- fcUndoMethod(A,_).



% 
% 
%  defining fc rules 
% 

%  pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form 
%  Out.  It also does certain optimizations.  Backtracking into this
%  predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  pfc_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


%  pfc_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%  Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

pfc_nf1(P/Cond,[(\+P)/Cond]) :- pfcNegatedLiteral(P), !.

pfc_nf1(P/Cond,[P/Cond]) :-  pfcAtom(P), !.

%  handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_negation(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

%  disjunction.

pfc_nf1((P;Q),NF) :- 
  !,
  (pfc_nf1(P,NF) ;   pfc_nf1(Q,NF)).


%  conjunction.

pfc_nf1((P,Q),NF) :-
  !,
  pfc_nf1(P,NF1),
  pfc_nf1(Q,NF2),
  append(NF1,NF2,NF).

%  handle a random atom.

pfc_nf1(P,[P]) :- 
  pfcAtom(P), 
  !.

% % shouln't we have something to catch the rest as errors?
pfc_nf1(Term,[Term]) :-
  pfcWarn("pfc_nf doesn't know how to normalize ~p",[Term]).


%  pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
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


%  pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%  changing -{...} to {\+...}
% % ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

pfc_nf_negation(Form,{\+ X}) :- 
  nonvar(Form),
  Form=(-({X})),
  !.
pfc_nf_negation(X,X).


% 
%  buildRhs(+Conjunction,-Rhs)
% 

buildRhs(X,[X]) :- 
  var(X),
  !.

buildRhs((A,B),[A2|Rest]) :- 
  !, 
  pfcCompileRhsTerm(A,A2),
  buildRhs(B,Rest).

buildRhs(X,[X2]) :-
   pfcCompileRhsTerm(X,X2).

pfcCompileRhsTerm((P/C),((P:-C))) :- !.

pfcCompileRhsTerm(P,P).


%  pfc_negation(N,P) is true if N is a negated term and P is the term
%  with the negation operator stripped.

% pfc_negation((-P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).

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
pfcConnective(('==>')).
pfcConnective(('<-')).
pfcConnective('<==>').

pfcConnective('-').
% pfcConnective('-').
pfcConnective('\\+').

processRule(Lhs,Rhs,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Rhs,Rhs2),
  pfcForEach(pfc_nf(Lhs,Lhs2), 
          buildRule(Lhs2,rhs(Rhs2),(ParentRuleCopy,user))).

buildRule(Lhs,Rhs,Support) :-
  buildTrigger(Lhs,Rhs,Trigger),
  fcEvalLHS(Trigger,Support).

buildTrigger([],Consequent,Consequent).

buildTrigger([V|Triggers],Consequent,pt(V,X)) :-
  var(V),
  !, 
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1/Test)|Triggers],Consequent,nt(T2,Test2,X)) :-
  pfc_negation(T1,T2),
  !, 
  buildNtTest(T2,Test,Test2),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1)|Triggers],Consequent,nt(T2,Test,X)) :-
  pfc_negation(T1,T2),
  !,
  buildNtTest(T2,true,Test),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  buildTrigger(Triggers,Consequent,X).

buildTrigger([T/Test|Triggers],Consequent,pt(T,X)) :-
  !, 
  buildTest(Test,Test2),
  buildTrigger([{Test2}|Triggers],Consequent,X).


%buildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  buildTrigger(Triggers,Consequent,X).

buildTrigger([T|Triggers],Consequent,pt(T,X)) :-
  !, 
  buildTrigger(Triggers,Consequent,X).

% 
%  buildNtTest(+,+,-).
% 
%  builds the test used in a negative trigger (nt/3).  This test is a
%  conjunction of the check than no matching facts are in the db and any
%  additional test specified in the rule attached to this - term.
% 

buildNtTest(T,Testin,Testout) :-
  buildTest(Testin,Testmid),
  pfcConjoin((pfc_BC(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

buildTest({Test},Test) :- !.
buildTest(Test,Test).

% 



%  simple typeing for pfc objects

pfcType(('==>'(_,_)),Type) :- !, Type=rule.
pfcType(('<==>'(_,_)),Type) :- !, Type=rule.
pfcType(('<-'(_,_)),Type) :- !, Type=rule.
pfcType(pt(_,_,_),Type) :- !, Type=trigger.
pfcType(pt(_,_),Type) :- !, Type=trigger.
pfcType(nt(_,_,_),Type) :- !,  Type=trigger.
pfcType(bt(_,_),Type) :- !,  Type=trigger.
pfcType(pfcAction(_),Type) :- !, Type=action.
pfcType((('::::'(_,X))),Type) :- !, pfcType(X,Type).
pfcType(_,fact) :-
  %  if it's not one of the above, it must be a fact!
  !.

pfcAssert(P,Support) :- 
  (pfc_clause(P) ; assert(P)),
  !,
  pfcAddSupport(P,Support).

pfcAsserta(P,Support) :-
  (pfc_clause(P) ; asserta(P)),
  !,
  pfcAddSupport(P,Support).

pfcAssertz(P,Support) :-
  (pfc_clause(P) ; assertz(P)),
  !,
  pfcAddSupport(P,Support).


pfc_clause_unique(H,B):-pfc_clause((H :- B))->true;clause(H,B).

pfc_clause((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  clause(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_clause(Head) :-
  % find a unit clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  clause(Head_copy,true),
  variant(Head,Head_copy).

pfcForEach(Binder,Body) :- Binder,pfcdo(Body),fail.
pfcForEach(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- X,!.
pfcdo(_).


%  pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
%  L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-  
  pfcUnion(Tail,L,Tail2).


%  pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
%  arg3 is a simplified expression representing the conjunction of
%  args 1 and 2.

pfcConjoin(true,X,X) :- !.
pfcConjoin(X,true,X) :- !.
pfcConjoin(C1,C2,(C1,C2)).



%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
% 	restore, reset, etc.0

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm(spft/3).
pfcDatabaseTerm(pt/2).
pfcDatabaseTerm(bt/2).
pfcDatabaseTerm(nt/3).
pfcDatabaseTerm('==>'/2).
pfcDatabaseTerm('<==>'/2).
pfcDatabaseTerm('<-'/2).
pfcDatabaseTerm(pfcQueue/1).

% removes all forward chaining rules and justifications from db.

pfcReset :-
  clause(spft(P,F,Trigger),true),
  pfcRetractOrWarn(P),
  pfcRetractOrWarn(spft(P,F,Trigger)),
  fail.
pfcReset :-
  pfcDatabaseItem(T),
  pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]).
pfcReset.

% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  clause(Term,_).

pfcRetractOrWarn(X) :-  retract(X), !.
pfcRetractOrWarn(X) :- 
  pfcWarn("Couldn't retract ~p.~n",[X]).




%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- dynamic pfcTraced/1.
:- dynamic pfcSpied/2.
:- dynamic pfcTraceExecution/0.
:- dynamic   pfcWarnings/1.

:- pfcDefault(pfcWarnings(_), pfcWarnings(true)).

%  predicates to examine the state of pfc

pfcQueue :- listing(pfcQueue/1).

pfcPrintDB :-
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
  pfcPrintSupports.

%  pfcPrintFacts ...

pfcPrintFacts :- pfcPrintFacts(_,true).

pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-
  pfcFacts(P,C,L),
  pfcClassifyFacts(L,User,Pfc,_Rule),
  format("~N~nUser added facts:",[]),
  pfcPrintitems(User),
  format("~N~nPfc added facts:",[]),
  pfcPrintitems(Pfc).


%  printitems clobbers it's arguments - beware!

pfcPrintitems([]).
pfcPrintitems([H|T]) :-
  numbervars(H,0,_),
  format("~N  ~p",[H]),
  pfcPrintitems(T).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,(user,user)),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcPrintRules :-
  bagof((P==>Q),clause((P==>Q),true),R1),
  pfcPrintitems(R1),
  bagof((P<==>Q),clause((P<==>Q),true),R2),
  pfcPrintitems(R2),
  bagof((P<-Q),clause((P<-Q),true),R3),
  pfcPrintitems(R3).

pfcPrintTriggers :-
  format("Positive triggers...~n",[]),
  bagof(pt(T,B),pfcGetTriggerQuick(pt(T,B)),Pts),
  pfcPrintitems(Pts),
  format("Negative triggers...~n",[]),
  bagof(nt(A,B,C),pfcGetTriggerQuick(nt(A,B,C)),Nts),
  pfcPrintitems(Nts),
  format("Goal triggers...~n",[]),
  bagof(bt(A,B),pfcGetTriggerQuick(bt(A,B)),Bts),
  pfcPrintitems(Bts).

pfcPrintSupports :- 
  % temporary hack.
  setof((S > P), pfcGetSupport(P,S),L),
  pfcPrintitems(L).

%  pfcFact(P) is true if fact P was asserted into the database via add.

pfcFact(P) :- pfcFact(P,true).

%  pfcFact(P,C) is true if fact P was asserted into the database via
%  add and contdition C is satisfied.  For example, we might do:
%  
%   pfcFact(X,pfcUserFact(X))
% 

pfcFact(P,C) :- 
  pfcGetSupport(P,_),
  pfcType(P,fact),
  call(C).

%  pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

%  pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof(P,pfcFact(P,C),L).

brake(X) :-  X, break.

% 
% 
%  predicates providing a simple tracing facility
% 

pfcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(o,o)).


pfcAddSpecialSupport(Fact,Support):- fail,
  Support = (How,pt(How2,rhs([Fact]))),  
  ignore(((pfcAddTrigger(nt(How,(pfc_CALL(How2)),rhs([Fact])),(How==>Fact,How)),fail))),
  dmsg('~n   \\\\^  Extra ^//  ~n',[]),
  fail.
pfcAddSpecialSupport(P,S):-pfcTraceAdd(P,S),!. 
  
pfcTraceAdd(P,S) :- 
 notrace((
   pfcTraceAddPrint(P,S),
   pfcTraceBreak(P,S))).
   

pfcTraceAddPrint(P,S) :-
  pfcTraced(P), !,
  (
  \+ \+ 
   (S=(U,U)
       -> wdmsg("~NAdding (~p) ~p",[U,P])
        ; wdmsg("~NAdding (:) ~p~NSupported By: ~p",[P,S]))),!.

  

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  pfcSpied(P,add) -> 
   (wdmsg("~NBreaking on add(~p)",[P]),
    break)
   ; true.



pfcTraceRem(pt(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem(nt(_,_)) :-
  % hack for now - never trace triggers.
  !.


pfcTraceRem(P) :-
  (pfcTraced(P) 
     -> wdmsg('~NRemoving ~p.',[P])
      ; true),
  (pfcSpied(P,rem)
   -> (wdmsg("~NBreaking on rem(~p)",[P]),
       break)
   ; true).


pfcTrace :- pfcTrace(_).

pfcTrace(Form) :-
  assert(pfcTraced(Form)).

pfcTrace(Form,Condition) :- 
  assert((pfcTraced(Form) :- Condition)).

pfcSpy(Form) :- pfcSpy(Form,[add,rem],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[add,rem],Condition) :-
  !,
  pfcSpy1(Form,add,Condition),
  pfcSpy1(Form,rem,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  assert((pfcSpied(Form,Mode) :- Condition)).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :- 
  clause(pfcSpied(Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- retractall(pfcTraced(Form)).

% needed:  pfcTraceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :- notrace((tracing,in_cmt(wdmsg(Msg, Args)))),!.
pfc_trace_msg(Msg,Args) :-
    pfcTraceExecution,
    !,
    in_cmt(wdmsg(Msg, Args)).

pfc_trace_msg(_Msg,_Args).

pfcWatch :- assert(pfcTraceExecution).

pfcNoWatch :-  retractall(pfcTraceExecution).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :- 
  format("~NERROR/Pfc: ",[]),
  format(Msg,Args).


% 
%  These control whether or not warnings are printed at all.
%    pfcWarn.
%    nopfcWarn.
% 
%  These print a warning message if the flag pfcWarnings is set.
%    pfcWarn(+Message)
%    pfcWarn(+Message,+ListOfArguments)
% 

pfcWarn :- 
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

nopfcWarn :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(false)).
 
pfcWarn(Msg) :-  pfcWarn(Msg,[]).

pfcWarn(Msg,Args) :- 
  format(string(S),Msg,Args),  
  (pfcWarnings(true) -> wdmsg(warn(pfc,S)) ; pfc_trace_msg('WARNING/PFC: ~s',[S])),!.


%%  pfcSetWarnings(+TF) is det.
%   true = sets flag to cause pfc warning messages to print.
%   false = sets flag to cause pfc warning messages not to print.
% 
pfcSetWarnings(true) :- 
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).
pfcSetWarnings(false) :- 
  retractall(pfcWarnings(_)).

%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%  *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

justification(F,J) :- supports(F,J).

justifications(F,Js) :- bagof(J,justification(F,J),Js).



%  pfcBaseF(P,L) - is true iff L is a list of "pfcBaseF" facts which, taken
%  together, allows us to deduce P.  A pfcBaseF fact is an axiom (a fact 
%  added by the user or a raw Prolog fact (i.e. one w/o any support))
%  or an assumption.

pfcBaseF(F,[F]) :- (axiom(F) ; assumption(F)),!.

pfcBaseF(F,L) :-
  % i.e. (reduce 'append (map 'pfcBaseF (justification f)))
  justification(F,Js),
  bases(Js,L).


%  bases(L1,L2) is true if list L2 represents the union of all of the 
%  facts on which some conclusion in list L1 is based.

bases([],[]).
bases([X|Rest],L) :-
  pfcBaseF(X,Bx),
  bases(Rest,Br),
  pfcUnion(Bx,Br,L).
	
axiom(F) :- 
  pfcGetSupport(F,(user,user)); 
  pfcGetSupport(F,(god,god)).

%  an assumption is a failed goal, i.e. were assuming that our failure to 
%  prove P is a proof of not(P)

assumption(P) :- pfc_negation(P,_).
   
%  assumptions(X,As) if As is a set of assumptions which underly X.

assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  pfcUnion(Bx,Br,L).  


%  pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%  of the form
% 
%      [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%           ^                         and has the form of
%           [J11, J12,... J1n]      a list of proof trees.


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


% 
% 
%  predicates for manipulating support relationships
% 

%  pfcAddSupport(+Fact,+Support)

pfcAddSupport(P,(Fact,Trigger)) :-
  (Trigger= nt(F,Condition,Action) -> 
    (pfc_trace_msg('~N~n\tAdding fcnt via support~n\t\ttrigger: ~p~n\t\tcond: ~p~n\t\taction: ~p~n\t from: ~p~N',
      [F,Condition,Action,pfcAddSupport(P,(Fact,Trigger))]));true),

  assert(spft(P,Fact,Trigger)).

pfcGetSupport(P,(Fact,Trigger)) :-
      spft(P,Fact,Trigger).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(P),
  !,
  pfcRetractOrWarn(spft(P,Fact,Trigger)).


pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(Fact),
  !,
  pfcRetractOrWarn(spft(P,Fact,Trigger)).

pfcRemSupport(P,(Fact,Trigger)) :-
  pfcRetractOrWarn(spft(P,Fact,Trigger)).


pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  spft(P,F,T).

pfc_make_supports((P,S1,S2)) :- 
  pfcAddSupport(P,(S1,S2)),
  (pfcAdd(P); true),
  !.

%  pfcTriggerKey(+Trigger,-Key) 
% 
%  Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey(pt(Key,_),Key).
pfcTriggerKey(pt(Key,_,_),Key).
pfcTriggerKey(nt(Key,_,_),Key).
pfcTriggerKey(Key,Key).


% ^L
%  Get a key from the trigger that will be used as the first argument of
%  the trigger pfcBaseF clause that stores the trigger.
% 

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).


%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

pfcWhy :- 
  whymemory(P,_),
  pfcWhy(P).

pfcWhy(N) :-
  number(N),
  !,
  whymemory(P,Js),
  pfcWhyCommand(N,P,Js).

pfcWhy(P) :-
  justifications(P,Js),
  retractall(whymemory(_,_)),
  assert(whymemory(P,Js)),
  in_cmt((pfcWhyBrouse(P,Js))).

pfcWhy1(P) :-
  justifications(P,Js),
  in_cmt((pfcWhyBrouse(P,Js))).

pfcWhyBrouse(P,Js) :-
   pfcShowJustifications(P,Js), !.

pfcWhyBrouse(P,Js) :-
  pfcShowJustifications(P,Js),
  pfcAsk(' >> ',Answer),
  pfcWhyCommand(Answer,P,Js).

pfcWhyCommand(q,_,_) :- !.
pfcWhyCommand(h,_,_) :- 
  !,
  format("~N
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level ~n",
  []).

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
  format("~N~p is a yet unimplemented command.",[N]),
  fail.

pfcCommand(X,_,_) :-
 format("~N~p is an unrecognized command, enter h. for help.",[X]),
 fail.
  
pfcShowJustifications(P,Js) :-
  format("~NJustifications for ~p:",[P]),
  pfcShowJustification1(Js,1).

pfcShowJustification1([],_).

pfcShowJustification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  pfcShowJustifications2(J,N,1),
  N2 is N+1,
  pfcShowJustification1(Js,N2).

pfcShowJustifications2([],_,_).

pfcShowJustifications2([C|Rest],JustNo,StepNo) :- 
 (StepNo==1->fmt('~N~n',[]);true),
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  format("~N    ~p.~p ~p",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  pfcShowJustifications2(Rest,JustNo,StepNext).

pfcAsk(Msg,Ans) :-
  format("~N~p",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).
 
:- use_module(library(logicmoo_utils)).

:- dynamic((foob/1,if_missing/2)).

:- pfcTrace.
:- pfcWatch.

% this should have been ok
% (if_missing(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
if_missing(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create).

:- add((good(X) ==> if_missing(foob(_),foob(X)))).

:- add(good(az)).

:- pfcWhy(foob(az)).

==> foob(b).

:- call(\+foob(az)).

==> (\+ foob(b)).

:- pfcWhy(foob(az)).

:- rtrace(rem( good(az) )).

:- listing([foob,good]).

:- break.

:- call( \+foob(az)).



:- call(foob(b)).

~ foob(b).

:- call(\+foob(b)).
:- call(foob(az)).



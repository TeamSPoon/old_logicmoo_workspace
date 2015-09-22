/** <module> special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

% we are in "prolog" consultation mode

:- ensure_loaded(logicmoo(logicmoo_engine)).
:- ensure_loaded(logicmoo_i_compiler).
:- enable_mpred_expansion.

are_clauses_entailed(E):-not(compound(E)),!,must(true==E).
are_clauses_entailed([E|List]):-is_list([E|List]),!,maplist(are_clauses_entailed,[E|List]).
are_clauses_entailed((C,L)):-!,are_clauses_entailed(C),are_clauses_entailed(L).
are_clauses_entailed(CL):- unnumbervars(CL,UCL),  !, \+ \+ show_call_failure(is_prolog_entailed(UCL)),!.

is_prolog_entailed(UCL):-clause_asserted(UCL),!.
is_prolog_entailed(UCL):-pfc_call(UCL),!.
 
member_ele(E,E):- (\+ (compound(E))),!.
member_ele([L|List],E):- is_list([L|List]),!,member(EE,[L|List]),member_ele(EE,E).
member_ele((H,T),E):- nonvar(H),nonvar(T),!, (member_ele(H,E);member_ele(T,E)).
member_ele(E,E).

delistify_last_arg(Arg,Pred,Last):-is_list(Arg),!,member(E,Arg),delistify_last_arg(E,Pred,Last).
delistify_last_arg(Arg,Pred,Last):- Pred=..[F|ARGS],append([Arg|ARGS],[NEW],NARGS),NEWCALL=..[F|NARGS],show_call(NEWCALL),!,member_ele(NEW,Last).

% sanity that mpreds (manage prolog prodicate) are abily to transform
:- thlocal:disable_mpred_term_expansions_locally->throw(thlocal:disable_mpred_term_expansions_locally);true.

% cwc "code-wise chaining" is always true in Prolog but will throw programming error if evalled in LogicMOO Prover.
% Use this to mark code and not axiomatic prolog

clif_to_prolog(CLIF,Prolog):-cwc,is_list(CLIF),!,must_maplist(clif_to_prolog,CLIF,Prolog).
clif_to_prolog((H,CLIF),(T,Prolog)):-cwc,sanity(must(nonvar(H))),!,trace,clif_to_prolog(H,T),clif_to_prolog(CLIF,Prolog).
clif_to_prolog((H<-B),(H<-B)):- cwc,!.
clif_to_prolog((P==>Q),(P==>Q)):- cwc,!.
clif_to_prolog((H:-B),PrologO):- cwc,!,must((show_call_failure(boxlog_to_pfc((H:-B),Prolog)),!,=(Prolog,PrologO))),!.
clif_to_prolog(CLIF,PrologO):- cwc,
  % somehow integrate why_to_id(tell,Wff,Why),
     must_det_l((
      kif_to_boxlog(CLIF,HORN),
      boxlog_to_pfc(HORN,Prolog),
      dmsg(pfc:-Prolog),
      =(Prolog,PrologO))),!.


% Sanity Test for expected side-effect entailments
% why does renumbervars work but not copy_term? 
is_entailed(CLIF):- cwc, sanity((clif_to_prolog(CLIF,Prolog),!,sanity(( \+ \+ (show_call_failure(are_clauses_entailed(Prolog))))))),!.

% Sanity Test for required absence of specific side-effect entailments
is_not_entailed(CLIF):- cwc, sanity((clif_to_prolog(CLIF,Prolog),show_call_failure(\+ are_clauses_entailed(Prolog)))),!.

:-op(1190,xfx,(:-)).
:-op(1200,fy,(is_entailed)).

% this defines a recogniser for clif syntax (well stuff that might be safe to send in thru kif_to_boxlog)
is_clif(all(_,X)):-cwc,compound(X),!,is_clif(X).
is_clif(forall(_,X)):-cwc,compound(X),!.
is_clif(CLIF):-cwc,
  VVs = v(if,iff,clif_forall,all,exists), % implies,equiv,forall
   (var(CLIF)-> (arg(_,VVs,F),functor(CLIF,F,2));
     compound(CLIF),functor(CLIF,F,2),arg(_,VVs,F)).

% we ensure we are in "pfc" consultation mode (so the syntax rules will define correctly)

% :- pfc_trace.
:- file_begin(pfc).

% make sure op alias for '=>' is not overriden
:- op_alias( (=>),  (=>)).

==> hybrid_support(clif,1).

% whenever we know about clif we''ll use the prolog forward chainging system
/*

% this is broken down to the next 6 clauses

(clif(CLIF) ==> 
   ({ clif_to_prolog(CLIF,PROLOG)},
      % this consequent asserts the new rules
      PROLOG,{slow_sanity(is_entailed(CLIF))})).
*/

arity(clif,1).
arity(boxlog,1).
arity(pfclog,1).

(clif(CLIF),{delistify_last_arg(CLIF,kif_to_boxlog,PROLOG)}) ==> (boxlog(PROLOG),{slow_sanity(is_entailed(CLIF))}).
(boxlog(BOXLOG),{delistify_last_arg(BOXLOG,boxlog_to_pfc,PROLOG)}) ==> pfclog(PROLOG).
(pfclog(PROLOG)==>(PROLOG,{slow_sanity(is_entailed(PROLOG))})).


/*

% this is accomplished by the previous 6 clauses

(clif(CLIF),{member_ele(CLIF,E)}) ==> clif1(E).
(clif1(CLIF),{kif_to_boxlog(CLIF,PROLOG)}) ==> boxlog(PROLOG).

(boxlog(CLIF),{member_ele(CLIF,E)}) ==> boxlog1(E).
(boxlog1(CLIF),{boxlog_to_pfc(CLIF,PROLOG)}) ==> pfclog(PROLOG).

(pfclog(CLIF),{member_ele(CLIF,E)}) ==> pfclog1(E).
(pfclog1(PROLOG)==>(PROLOG,{is_entailed(PROLOG)})).
*/

% we create code syntax listeners for [if,iff,clif_forall,all,exists]/2s
({is_clif(CLIF)} ==>
  (CLIF/is_clif(CLIF) ==> clif(CLIF))).


end_of_file.



:- if( if_defined(pfc_examples,user:startup_option(clif,sanity_tests))).

/* @see https://github.com/TeamSPoon/PrologMUD/blob/master/runtime/try_logicmoo_examples.pl */

:- endif.


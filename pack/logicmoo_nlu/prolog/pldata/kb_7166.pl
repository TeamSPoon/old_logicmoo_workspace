:- module(kb7166,
  [
 (forward/3),
 (forward/4),
 (forward/5),
 (forward/6),
 (forward/7),
 (forward/8),
 (forward/9),
 (forward/10),
 (forward/11),
 (forward/12),
 (backward/3),
 (backward/4),
 (backward/5),
 (backward/6),
 (backward/7),
 (backward/8),
 (backward/9),
 (backward/10),
 (backward/11),
 (backward/12),
 (code/3),
 (code/4),
 (code/5),
 (code/6),
 (code/7),
 (code/8),
 (code/9),
 (code/10),
 (code/11),
 (code/12),
 (deduction/3),
 (rename/2),
 (rename/1),
 (forward_default/4)
  ]).

:- style_check(-singleton).
:- style_check(-discontiguous).

:- multifile(forward/3).
:- multifile(forward/4).
:- multifile(forward/5).
:- multifile(forward/6).
:- multifile(forward/7).
:- multifile(forward/8).
:- multifile(forward/9).
:- multifile(forward/10).
:- multifile(forward/11).
:- multifile(forward/12).

:- multifile(backward/3).
:- multifile(backward/4).
:- multifile(backward/5).
:- multifile(backward/6).
:- multifile(backward/7).
:- multifile(backward/8).
:- multifile(backward/9).
:- multifile(backward/10).
:- multifile(backward/11).
:- multifile(backward/12).

:- multifile(code/3).
:- multifile(code/4).
:- multifile(code/5).
:- multifile(code/6).
:- multifile(code/7).
:- multifile(code/8).
:- multifile(code/9).
:- multifile(code/10).
:- multifile(code/11).
:- multifile(code/12).

:- dynamic(forward/3).
:- dynamic(forward/4).
:- dynamic(forward/5).
:- dynamic(forward/6).
:- dynamic(forward/7).
:- dynamic(forward/8).
:- dynamic(forward/9).
:- dynamic(forward/10).
:- dynamic(forward/11).
:- dynamic(forward/12).

:- dynamic(backward/3).
:- dynamic(backward/4).
:- dynamic(backward/5).
:- dynamic(backward/6).
:- dynamic(backward/7).
:- dynamic(backward/8).
:- dynamic(backward/9).
:- dynamic(backward/10).
:- dynamic(backward/11).
:- dynamic(backward/12).

:- dynamic(code/3).
:- dynamic(code/4).
:- dynamic(code/5).
:- dynamic(code/6).
:- dynamic(code/7).
:- dynamic(code/8).
:- dynamic(code/9).
:- dynamic(code/10).
:- dynamic(code/11).
:- dynamic(code/12).

:- multifile(deduction/3).
:- dynamic(deduction/3).

:- multifile(rename/2).
:- dynamic(rename/2).
:- multifile(rename/1).
:- dynamic(rename/1).

:- dynamic(forward_default/4).

:- dynamic(on_fin/1).


/*
:- call(asserta,((user:term_expansion(_, _):- !,fail))).
:- call(asserta,((user:goal_expansion(_, _):-!,fail))).
:- call(asserta,((system:term_expansion(_, _):-!,fail))).
:- call(asserta,((system:goal_expansion(_, _):-!,fail))).
:- call(asserta,((user:term_expansion(_,_, _,_):-!,fail))).
:- call(asserta,((user:goal_expansion(_,_, _,_):-!,fail))).
:- call(asserta,((system:term_expansion(_,_, _,_):-!,fail))).
:- call(asserta,((system:goal_expansion(_,_, _,_):-!,fail))).
*/

string_to_atomstring(A,SA):-atom_string(A,S),term_to_atom(S,SA).

convert_string(A,B):- atom_length(A,L),tokenize_atom(A,T),!,convert_string(A,L,T,B),!.
convert_string(A,B):- term_to_atom(A,B).

convert_string(A,0,_,'""'):-!.
convert_string(A,L,_,B):- L<3,!,term_to_atom(A,B).
convert_string(A,L,_,B):- L>3, \+ atom_contains(A," "), \+ atom_contains(A,"'"),!,term_to_atom(A,B).
convert_string(A,_,[],B):-!,term_to_atom(A,B).
convert_string(A,_,[_],B):-!,term_to_atom(A,B).
convert_string(_,_,List,B):-convert_string_list(List,B).

convert_string_list([],[]).
convert_string_list([T,-,P|List],B):-atomic_list_concat([T,P],-,TP),!,convert_string_list([TP|List],B).
convert_string_list([T,P|List],B):-member(T,['#','~','#$','\'']),atom_concat(T,P,TP),!,convert_string_list([TP|List],B).
convert_string_list([T|List],[P|BList]):-string_to_atomstring(T,P),convert_string_list(List,BList).


fix_var_name(A,B):- atomic_list_concat(AB,'-',A),atomic_list_concat(AB,'_',B).

do_renames(A,B):- var(A),!,A=B.
do_renames(uU('SubLQuoteFn',A),uSubLQuoteFn(A)):-var(A),!,nb_setval('$has_var',t),!.
do_renames(uU('SubLQuoteFn','$VAR'(A)),uSubLQuoteFn(A)):-!,nb_setval('$has_quote',t),!.
do_renames('$VAR'(A),'$VAR'(B)):- catch((fix_var_name(A,B),!,nb_setval('$has_var',t)),E,(dtrace(dmsg(E)))),!.
%do_renames('$VAR'(A),B):- catch((fix_var_name(A,B),!,nb_setval('$has_var',t)),E,(dtrace(dmsg(E)))),!.
do_renames(A,B):- atom(A),atom_contains(A,' '),!,convert_string(A,B),nb_setval('$has_var',t),!.
do_renames(A,B):- atom(A),atom_contains(A,'~'),!,convert_string(A,B),nb_setval('$has_var',t),!.
do_renames(A,B):- rename(B,A),atom(B),!.
do_renames(A,B):- string(A),!,convert_string(A,B).
do_renames(A,B):- \+ compound(A),!,A=B.
do_renames([A|Rest],[B|List]):- !, do_renames(A,B),do_renames(Rest,List).
do_renames(A,uN(P,ARGS),B):-!,maplist(do_renames,[P|ARGS],List),compound_name_arguments(B,uT,List).
do_renames(A,B):- compound_name_arguments(A,P,ARGS),maplist(do_renames,[P|ARGS],[T|L]),do_renames_pass2(T,L,B).

compute_argIsa(ARG1ISA,NN,ARGISA):-
  atom(ARG1ISA),
  atom_concat('arg',REST,ARG1ISA),
  member(E,['Genl','Isa','SometimesIsa','Format','QuotedIsa']),atom_concat(N,E,REST),
  atom_number(N,NN),
  atom_concat('arg',E,ARGISA),!.

do_renames_pass2(forward,[MT,C,ARG1ISA,P,ID],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),!, 
  do_renames_pass2(forward,[MT,P,ARGISA,NN,C,ID],OUT).
do_renames_pass2(t,[ARG1ISA,P,C],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),  OUT = t(ARGISA,P,NN,C).

do_renames_pass2(P,[],B):-!,do_renames(P,B).
do_renames_pass2(nartR,[P|ARGS],(B)):-atom(P),!,compound_name_arguments(B,P,ARGS).
do_renames_pass2(nartR,ARGS,B):-!,compound_name_arguments(B,nartR,ARGS).
do_renames_pass2(t,[P,I,C],B):- P==isa,atom(C),!,B=..[C,I].
do_renames_pass2(t,[P|IC],B):- intrinsicPred(P),!,B=..[P|IC].
do_renames_pass2(t,ARGS,B):- compound_name_arguments(B,t,ARGS).
do_renames_pass2(uU,ARGS,B):-!,compound_name_arguments(B,u,ARGS).
do_renames_pass2(P,ARGS,B):-!,compound_name_arguments(B,P,ARGS).

intrinsicPred(genlMt).
intrinsicPred(ist).
intrinsicPred(termOfUnit).

:- (current_prolog_flag(lm_expanders,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(lm_expanders,PrevValue))),
   set_prolog_flag(lm_expanders,false).

:- (current_prolog_flag(double_quotes,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(double_quotes,PrevValue))),
   set_prolog_flag(double_quotes,atom).

:- if(current_prolog_flag(logicmoo_simplify_te,true)).
:- (call(asserta,((system:term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- (call(asserta,((user:term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- (call(asserta,((term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- endif.

rename(A,B):-rename_rev(B,A).
:- dynamic(rename_rev/2).
:- multifile(rename_rev/2).
rename_rev('SetOrCollection',tSpec).
rename_rev('Collection',tCol).
rename_rev('CollectionType',ttTypeType).
rename_rev('SiblingDisjointCollectionType',tSet).
rename_rev('ObjectType',ttValueType).
rename_rev('AspatialThing',vtValue).
rename_rev('RelationshipType',ttRelationType).
rename_rev('Predicate',tPred).
rename_rev('ObjectType',tSet).
rename_rev('SubLExpressionType',ttExpressionType).

:- include('kb_7166.pl-a1').
:- include('kb_7166.pl-a3').
% :- call(asserta,((system:term_expansion(NV, UV):-!,unnumbervars(NV, UV)->NV\=@=UV))).


ra5(Often,PO) :-  
  expand_file_search_path((pldata('kb_7166.pl-a5')),Path),
  open(Path,read,In),
  repeat,
   once((rt(In,Wff,Vs),
   nb_setval('$has_var',[]),
   nb_setval('$variable_names',Vs),
   (do_renames(Wff,P)->true;throw(do_renames(Wff,P))),
   (nb_current('$has_var',[])-> (PO = P,V2s=Vs) ; ((wt(string(S),P,Vs),rt(string(S),PO,V2s)))),
   nb_setval('$variable_names',V2s),
   (V2s==[]->true;(functor(Wff,_,A),arg(A,Wff,ID),(maplist(arg(1),V2s,Names),wt(current_output,assertionVars(ID,Names),[])))),
   wt(current_output,PO,V2s))).
   % ((nb_current('$has_var',t);(flag('$ett',X,X+1),0 is X rem Often))-> wt(current_output,PO,V2s) ; true))).
     

rt(string(In),Wff,Vs):-!,catch(read_term_from_atom(In,Wff,[module(user),double_quotes(string),variable_names(Vs)]),E,(dmsg(E),dtrace,fail)).
rt(In,Wff,Vs):- catch(read_term(In,Wff,[module(user),double_quotes(string),variable_names(Vs)]),E,(dmsg(E),dtrace,fail)).
wt(string(O),P,Vs):- !, with_output_to(string(O), write_term(P,[variable_names(Vs),portrayed(true),quoted(true),fullstop(true),ignore_ops(true),nl(true),singletons(false)])).
wt(O,P,Vs):- write_term(O,P,[variable_names(Vs),portrayed(true),quoted(true),fullstop(true),ignore_ops(true),nl(true),singletons(false)]).

ra5:- tell(ra5),ra5(1,E),E==end_of_file,!,told.
% ra5:- ra5(1,E),E==end_of_file,!.

% :- include(pldata('kb_7166.pl-a5')).
 
:- if(exists_source('kb_7166.pl-a6')).
:- include('kb_7166.pl-a6').
:- endif.
:- set_prolog_flag(user:double_quotes,string).
% :- module(kb7166).
:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(kb7166:double_quotes,string).
:- (compiling->ra5;true).
:- include(ra5).


:- if(current_predicate(_,on_fin(_))).
:- forall(call(retract,(on_fin(CALL))),call(CALL)).
:- endif.

:- call(retractall,rename(_)),call(retractall,backward(_,'[]',_)),call(retractall,    code(_,'[]',_)),call(retractall, forward(_,'[]',_)).

/*
:- call(retract,((user:term_expansion(_, _):-!,fail))).
:- call(retract,((user:goal_expansion(_, _):-!,fail))).
:- call(retract,((system:term_expansion(_, _):-!,fail))).
:- call(retract,((system:goal_expansion(_, _):-!,fail))).
:- call(retract,((user:term_expansion(_,_, _,_):-!,fail))).
:- call(retract,((user:goal_expansion(_,_, _,_):-!,fail))).
:- call(retract,((system:term_expansion(_,_, _,_):-!,fail))).
:- call(retract,((system:goal_expansion(_,_, _,_):-!,fail))).
*/

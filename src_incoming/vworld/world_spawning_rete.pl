/** <module>  File is responsible for 
%  laying out new objects in the mud based on some frame rules (using RETE method)
%
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

end_of_file.


moo:on_world_load :- retractall(spawn_objects(_)).

growth :-
	findall(([Obj,Chance]),
	    props(Obj,spawn_rate(Chance)),
	    Objects),
	assert(spawn_objects(Objects)).



% For Quintus alter 'Y is random(LLL)' to random(0,LLL,Y)
% defined pred maybe.... be sure to comment this pred out before going
% back to quitus (located below).
spread :-
	spawn_objects(Objects),
	forall(prop_memb([Object,Chance],Objects),
	spread(Object,Chance)).

spread(Object,Chance) :-        
	maybe(Chance),!,
        gensym(Object,Named),
        create_instance(Named,Object,[]),!.

spread(_,_).

:-style_check(-singleton).
:-style_check(-discontiguous).


dmsg(M):-'format'('% ~q. ~n', [M]).




callStub(P,F,A):- predicate_property(P,number_of_clauses(N)),(N==1 -> (dmsg(failed(callStub(P,F,A),!,fail))); ((functor(PP,F,A),dmsg(callStub(P,F,A)),retractall((PP:-callStub(PP,F,A))),callStub(P,F,A)))).

createStub(F,A):- abolish(F/A),dynamic(F/A),!. % functor(P,F,A),asserta((P:-callStub(P,F,A))).

:- forall(member(F/A,[
      conflict_set/1,  
      cls/0,  
      create_instance/3,  
      frinst/4,  
      list/1,  
      mea/1,  
      memory/2,  
      prop_memb/2,  
      props/2,
      spawn_objects/1,  
      string_integer/2,  
      string_list/2,  
      frame/2,
      timer/1,
      root/3,
      bi/4,
      tes/4,
      rul/3,
      varg/1,
      nid/1
      ]),createStub(F,A)).

% rete - the predicates which implement the Rete pattern matching algorithm.

% It should be modified some day
% to use pointers to working memory in the memory predicates rather
% than the full tokens - this would save a lot of space.

% retecomp - compile rules into a rete network

:-op(800,xfx,==>).         % used to separate LHS and RHS of rule
:-op(500,xfy,#).           % used to separate attributes and values
:-op(810,fx,rule).         % used to define rule
:-op(700,xfy,#).           % used for unification instead of =
:-op(700,xfy,\=).		      % not equal
:-op(600,xfy,with).		   % used for frame instances in rules


reset_rete :-
        createStub(root,3),
	createStub(bi,4),
	createStub(tes,4),
	createStub(rul,3),
	createStub(varg,1),
	createStub(nid,1).


rete_compile :-
        reset_rete,
	asserta(nid(0)),
	rete_compil.
%	display_net.

display_net :-
	display_roots,nl,
	display_bis,nl,
	display_teses,nl,
	display_ruls.

display_roots :-
	root(N,A,B),
	write( root(N,A,B) ),nl,
	fail.
display_roots.

display_bis :-
	bi(A,B,C,D),
	write( bi(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),
	write(D),nl,nl,
	fail.
display_bis.

display_teses :-
	tes(A,B,C,D),
	write( tes(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),nl,
	write(D),nl,nl,
	fail.
display_teses.

display_ruls :-
	rul(A,B,C),
	write( rul(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),nl,
	fail.
display_ruls.

write_list([]).
write_list([H|T]) :-
	write(H),nl,
	wr_lis(T).
	
wr_lis([]).
wr_lis([H|T]) :-
	tab(5),write(H),nl,
	wr_lis(T).

% compile each rule into the rete net

rete_compil :-
	rule N# LHS ==> RHS,
	rete_comp(N,LHS,RHS),
	fail.
rete_compil :-
	message(201).

% compile an individual rule into the net

rete_comp(N,[H|T],RHS) :-
	term(H,Hw),
	check_root(RN,Hw,HList),
	retcom(root(RN),[Hw/_],HList,T,N,RHS),
	message(202,N), !.
rete_comp(N,_,_) :-
	message(203,N).

% the main compile loop
% 	PNID - the id of the previous node
%	OutTok - list of tokens from previous node
%	PrevList - transfer list from previous node 
%	[H|T] - list of remaining clauses in rule
%	N - The rule ID, for building the rule at the end
%	RHS - the rhs of the rule for building the rule at the end

retcom(PNID,OutTok,PrevList,[],N,RHS) :-
	build_rule(OutTok,PrevList,N,RHS),
	update_node(PNID,PrevList,rule-N),
	!.
retcom(PNID,PrevNode,PrevList,[H|T],N,RHS) :-
	term(H,Hw),
	check_root(RN,Hw,HList),
	check_node(PrevNode,PrevList,[Hw/_],HList,NID,OutTok,NList),
	update_node(PNID,PrevList,NID-l),
	update_root(RN,HList,NID-r),
	!,
	retcom(NID,OutTok,NList,T,N,RHS).	
retcom(PNID,PrevNode,PrevList,[H|T],N,RHS) :-	%some kind of tester call
	check_tnode(PrevNode,PrevList,[H/0],HList,NID,OutTok,NList),
	update_node(PNID,PrevList,test-NID),
	!,
	retcom(test-NID,OutTok,NList,T,N,RHS).	

term(Class-Name with List,Class-Name with List).
term(Class-Name, Class-Name with []).

check_root(NID,Term,[]) :-
	not(root(_,Term,_)),
	gen_nid(NID),
	assertz( root(NID,Term,[]) ), !.
check_root(N,Term,List) :-
	asserta(temp(Term)),
	retract(temp(T1)),
	root(N,Term,List),
	root(N,T2,_),
	comp_devar(T1,T2), !.
check_root(NID,Term,[]) :-
	gen_nid(NID),
	assertz( root(NID,Term,[]) ).


% if this node was already on the list do nothing, otherwise add it
% to the list

update_root(RN,HList,NID) :-
	member(NID,HList), !.
update_root(RN,HList,NID) :-
	retract( root(RN,H,HList) ),
	asserta( root(RN,H,[NID|HList]) ).

update_node(root(RN),HList,NID) :-
	update_root(RN,HList,NID), !.
update_node(X,PrevList,NID) :-
	member(NID,PrevList), !.
update_node(test-N,PrevList,NID) :-
	retract( tes(N,L,T,_) ),
	asserta( tes(N,L,T,[NID|PrevList]) ), !.
update_node(PNID,PrevList,NID) :-
	retract( bi(PNID,L,R,_) ),
	asserta( bi(PNID,L,R,[NID|PrevList]) ).

% check to see if there is a node which already fits, otherwise 
% create a new one
%	PNode - token list from previous node
%	PList - list of successor nodes from previous node
%	H - new token being added
%	HList - successor nodes from root for token H
%	NID - returned ID of the node
%	OutTok - returned tokenlist from the node
%	NList - returned list of successor nodes from the node

%	first case - there isn't a matching rule using Prolog's match, so 
%		build a new one

check_node(PNode,PList,H,HList,NID,OutTok,[]) :-
	not(bi(_,PNode,H,_)),
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( bi(NID,PNode,H,[]) ),
	!.

%	second case - there was a matching rule using Prolog's match, so
%		match again using generated constants instead of variables.  If
%		this matches then we have a match, otherwise we had a match
%		where variables don't line up and its no good. (asserts and
%		retracts allow different variables to have same information and
%		prevent binding of variables in one from affecting the other)

check_node(PNode,PList,H,HList,NID,OutTok,NList) :-
	append(PNode,H,OutTok),
	asserta(temp(OutTok)),
	retract(temp(Tot1)),
	bi(NID,PNode,H,NList),
	bi(NID,T2,T3,_),
	append(T2,T3,Tot2),
	comp_devar(Tot1,Tot2),
	!.

%	third case - the variables didn't line up from the second rule, so
%		make a new node.

check_node(PNode,PList,H,HList,NID,OutTok,[]) :-
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( bi(NID,PNode,H,[]) ).

% check for test node - similar to check for regular node

check_tnode(PNode,PList,H,HList,NID,OutTok,[]) :-
	not(tes(_,PNode,H,_)),
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( tes(NID,PNode,H,[]) ),
	!.

%	second case - there was a matching rule using Prolog's match, so
%		match again using generated constants instead of variables.  If
%		this matches then we have a match, otherwise we had a match
%		where variables don't line up and its no good. (asserts and
%		retracts allow different variables to have same information and
%		prevent binding of variables in one from affecting the other)

check_tnode(PNode,PList,H,HList,NID,OutTok,NList) :-
	append(PNode,H,OutTok),
	asserta(temp(OutTok)),
	retract(temp(Tot1)),
	tes(NID,PNode,H,NList),
	tes(NID,T2,T3,_),
	append(T2,T3,Tot2),
	comp_devar(Tot1,Tot2),
	!.

%	third case - the variables didn't line up from the second rule, so
%		make a new node.

check_tnode(PNode,PList,H,HList,NID,OutTok,[]) :-
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( tes(NID,PNode,H,[]) ).

build_rule(OutTok,PrevList,N,RHS) :-
	assertz( rul(N,OutTok,RHS) ).
	
gen_nid(NID) :-
	retract( nid(N) ),
	NID is N+1,
	asserta( nid(NID) ).

% the hard part, undo Prolog's pattern matching so variables match just
% variables and not constants.  de-var replaces all the variables with
% generated constants - this ensures only variables will match variables.

comp_devar(T1,T2) :-
	de_vari(T1),
	de_vari(T2),
	T1=T2.

de_vari([]).
de_vari([H|T]) :-
	de_var(H),
	de_vari(T).
de_vari(X) :- de_var(X).

de_var(X/_) :- de_var(X).
de_var(X-Y with List) :-
	init_vargen,
	de_v(X-Y),
	de_vl(List), !.
de_var(X-Y) :-
	init_vargen,
	de_v(X-Y), !.

de_vl([]).
de_vl([H|T]) :-
	de_v(H),
	de_vl(T).

de_v(X-Y) :-
	d_v(X),
	d_v(Y).

d_v(V) :-
	var(V),
	var_gen(V), !.
d_v(_).

init_vargen :-
	createStub(varg,1),
	asserta(varg(1)).
	
var_gen(V) :-
	retract(varg(N)),
	NN is N+1,
	asserta(varg(NN)),
	string_integer(NS,N),
	string_list(NS,NL),
	append("#VAR_",NL,X),
	name(V,X).

% predicates to update the rete network

% add a token to the rete net.  a token is of the form C-N with [S-V,...]
% ReqList gets bound with the values from the term added to the database.

addrete(Class,Name,TimeStamp) :-
	root(ID,Class-Name with ReqList, NextList),
	ffsend(Class,Name,ReqList,TimeStamp,NextList),
	fail.
addrete(_,_,_).

% fullfill the request list from the token, and send the instantiated
% token through the net.

ffsend(Class,Name,ReqList,TimeStamp,NextList) :-
	getf(Class,Name,ReqList),
	send(tok(add,[(Class-Name with ReqList)/TimeStamp]), NextList),
	!.

delrete(Class,Name,TimeStamp) :-
	root(ID,Class-Name with ReqList, NextList),
	delr(Class,Name,ReqList,TimeStamp),
	fail.
delrete(_,_,_).

delr(Class,Name,ReqList,TimeStamp) :-
	getf(Class,Name,ReqList),
	!, send(tok(del,[(Class-Name with ReqList)/TimeStamp]), NextList).
delr(Class,Name,ReqList,TimeStamp).

% send the new token to each of the succesor nodes

send(_,[]).
send(Tokens, [Node|Rest]) :-
	sen(Node, Tokens),
	send(Tokens, Rest).

% add or delete the new token from the appropriate memory, build new
% tokens from left or right and send them to successor nodes.

sen(rule-N,tok(AD,TokenList)) :-
	rul(N,TokenList,Actions),
	(AD = add, add_conflict_set(N,TokenList,Actions);
	 AD = del, del_conflict_set(N,TokenList,Actions)),
	!.
sen(Node-l,tok(AD,TokenList)) :-
	bi(Node,TokenList,Right,NextList),
	(AD = add, asserta( memory(Node-l,TokenList) );
	 AD = del, retract( memory(Node-l,TokenList) )),
	!,matchRight(Node,AD,TokenList,Right,NextList).
sen(Node-r,tok(AD,TokenList)) :-
	bi(Node,Left,TokenList,NextList),
	(AD = add, asserta( memory(Node-r,TokenList) );
	 AD = del, retract( memory(Node-r,TokenList) )),
	!,matchLeft(Node,AD,TokenList,Left,NextList).
sen(test-N,tok(AD,TokenList)) :-
	tes(N,TokenList,[Test/0],NextList),
	test(Test),
	append(TokenList,[Test/0],NewToks),
	!,send(tok(AD,NewToks),NextList).

matchRight(Node,AD,TokenList,Right,NextList) :-
	memory(Node-r,Right),
	append(TokenList,Right,NewToks),
	send(tok(AD,NewToks),NextList),
	fail.
matchRight(_,_,_,_,_).
	
matchLeft(Node,AD,TokenList,Left,NextList) :-
	memory(Node-l,Left),
	append(Left,TokenList,NewToks),
	send(tok(AD,NewToks),NextList),
	fail.
matchLeft(_,_,_,_,_).


	                                                                                                                           
% rete - the predicates which implement the Rete pattern matching algorithm.

% It should be modified some day
% to use pointers to working memory in the memory predicates rather
% than the full tokens - this would save a lot of space.

% retecomp - compile rules into a rete network

:-op(800,xfx,==>).         % used to separate LHS and RHS of rule
:-op(500,xfy,#).           % used to separate attributes and values
:-op(810,fx,rule).         % used to define rule
:-op(700,xfy,#).           % used for unification instead of =
:-op(700,xfy,\=).		      % not equal
:-op(600,xfy,with).		   % used for frame instances in rules

rete_compile :-
	createStub(root,3),
	createStub(bi,4),
	createStub(tes,4),
	createStub(rul,3),
	createStub(varg,1),
	createStub(nid,1),
	asserta(nid(0)),
	rete_compil.
%	display_net.

display_net :-
	display_roots,nl,
	display_bis,nl,
	display_teses,nl,
	display_ruls.

display_roots :-
	root(N,A,B),
	write( root(N,A,B) ),nl,
	fail.
display_roots.

display_bis :-
	bi(A,B,C,D),
	write( bi(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),
	write(D),nl,nl,
	fail.
display_bis.

display_teses :-
	tes(A,B,C,D),
	write( tes(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),nl,
	write(D),nl,nl,
	fail.
display_teses.

display_ruls :-
	rul(A,B,C),
	write( rul(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),nl,
	fail.
display_ruls.

write_list([]).
write_list([H|T]) :-
	write(H),nl,
	wr_lis(T).
	
wr_lis([]).
wr_lis([H|T]) :-
	tab(5),write(H),nl,
	wr_lis(T).

% compile each rule into the rete net

rete_compil :-
	rule N# LHS ==> RHS,
	rete_comp(N,LHS,RHS),
	fail.
rete_compil :-
	message(201).

% compile an individual rule into the net

rete_comp(N,[H|T],RHS) :-
	term(H,Hw),
	check_root(RN,Hw,HList),
	retcom(root(RN),[Hw/_],HList,T,N,RHS),
	message(202,N), !.
rete_comp(N,_,_) :-
	message(203,N).

% the main compile loop
% 	PNID - the id of the previous node
%	OutTok - list of tokens from previous node
%	PrevList - transfer list from previous node 
%	[H|T] - list of remaining clauses in rule
%	N - The rule ID, for building the rule at the end
%	RHS - the rhs of the rule for building the rule at the end

retcom(PNID,OutTok,PrevList,[],N,RHS) :-
	build_rule(OutTok,PrevList,N,RHS),
	update_node(PNID,PrevList,rule-N),
	!.
retcom(PNID,PrevNode,PrevList,[H|T],N,RHS) :-
	term(H,Hw),
	check_root(RN,Hw,HList),
	check_node(PrevNode,PrevList,[Hw/_],HList,NID,OutTok,NList),
	update_node(PNID,PrevList,NID-l),
	update_root(RN,HList,NID-r),
	!,
	retcom(NID,OutTok,NList,T,N,RHS).	
retcom(PNID,PrevNode,PrevList,[H|T],N,RHS) :-	%some kind of tester call
	check_tnode(PrevNode,PrevList,[H/0],HList,NID,OutTok,NList),
	update_node(PNID,PrevList,test-NID),
	!,
	retcom(test-NID,OutTok,NList,T,N,RHS).	

term(Class-Name with List,Class-Name with List).
term(Class-Name, Class-Name with []).

check_root(NID,Term,[]) :-
	not(root(_,Term,_)),
	gen_nid(NID),
	assertz( root(NID,Term,[]) ), !.
check_root(N,Term,List) :-
	asserta(temp(Term)),
	retract(temp(T1)),
	root(N,Term,List),
	root(N,T2,_),
	comp_devar(T1,T2), !.
check_root(NID,Term,[]) :-
	gen_nid(NID),
	assertz( root(NID,Term,[]) ).


% if this node was already on the list do nothing, otherwise add it
% to the list

update_root(RN,HList,NID) :-
	member(NID,HList), !.
update_root(RN,HList,NID) :-
	retract( root(RN,H,HList) ),
	asserta( root(RN,H,[NID|HList]) ).

update_node(root(RN),HList,NID) :-
	update_root(RN,HList,NID), !.
update_node(X,PrevList,NID) :-
	member(NID,PrevList), !.
update_node(test-N,PrevList,NID) :-
	retract( tes(N,L,T,_) ),
	asserta( tes(N,L,T,[NID|PrevList]) ), !.
update_node(PNID,PrevList,NID) :-
	retract( bi(PNID,L,R,_) ),
	asserta( bi(PNID,L,R,[NID|PrevList]) ).

% check to see if there is a node which already fits, otherwise 
% create a new one
%	PNode - token list from previous node
%	PList - list of successor nodes from previous node
%	H - new token being added
%	HList - successor nodes from root for token H
%	NID - returned ID of the node
%	OutTok - returned tokenlist from the node
%	NList - returned list of successor nodes from the node

%	first case - there isn't a matching rule using Prolog's match, so 
%		build a new one

check_node(PNode,PList,H,HList,NID,OutTok,[]) :-
	not(bi(_,PNode,H,_)),
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( bi(NID,PNode,H,[]) ),
	!.

%	second case - there was a matching rule using Prolog's match, so
%		match again using generated constants instead of variables.  If
%		this matches then we have a match, otherwise we had a match
%		where variables don't line up and its no good. (asserts and
%		retracts allow different variables to have same information and
%		prevent binding of variables in one from affecting the other)

check_node(PNode,PList,H,HList,NID,OutTok,NList) :-
	append(PNode,H,OutTok),
	asserta(temp(OutTok)),
	retract(temp(Tot1)),
	bi(NID,PNode,H,NList),
	bi(NID,T2,T3,_),
	append(T2,T3,Tot2),
	comp_devar(Tot1,Tot2),
	!.

%	third case - the variables didn't line up from the second rule, so
%		make a new node.

check_node(PNode,PList,H,HList,NID,OutTok,[]) :-
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( bi(NID,PNode,H,[]) ).

% check for test node - similar to check for regular node

check_tnode(PNode,PList,H,HList,NID,OutTok,[]) :-
	not(tes(_,PNode,H,_)),
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( tes(NID,PNode,H,[]) ),
	!.

%	second case - there was a matching rule using Prolog's match, so
%		match again using generated constants instead of variables.  If
%		this matches then we have a match, otherwise we had a match
%		where variables don't line up and its no good. (asserts and
%		retracts allow different variables to have same information and
%		prevent binding of variables in one from affecting the other)

check_tnode(PNode,PList,H,HList,NID,OutTok,NList) :-
	append(PNode,H,OutTok),
	asserta(temp(OutTok)),
	retract(temp(Tot1)),
	tes(NID,PNode,H,NList),
	tes(NID,T2,T3,_),
	append(T2,T3,Tot2),
	comp_devar(Tot1,Tot2),
	!.

%	third case - the variables didn't line up from the second rule, so
%		make a new node.

check_tnode(PNode,PList,H,HList,NID,OutTok,[]) :-
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( tes(NID,PNode,H,[]) ).

build_rule(OutTok,PrevList,N,RHS) :-
	assertz( rul(N,OutTok,RHS) ).
	
gen_nid(NID) :-
	retract( nid(N) ),
	NID is N+1,
	asserta( nid(NID) ).

% the hard part, undo Prolog's pattern matching so variables match just
% variables and not constants.  de-var replaces all the variables with
% generated constants - this ensures only variables will match variables.

comp_devar(T1,T2) :-
	de_vari(T1),
	de_vari(T2),
	T1=T2.

de_vari([]).
de_vari([H|T]) :-
	de_var(H),
	de_vari(T).
de_vari(X) :- de_var(X).

de_var(X/_) :- de_var(X).
de_var(X-Y with List) :-
	init_vargen,
	de_v(X-Y),
	de_vl(List), !.
de_var(X-Y) :-
	init_vargen,
	de_v(X-Y), !.

de_vl([]).
de_vl([H|T]) :-
	de_v(H),
	de_vl(T).

de_v(X-Y) :-
	d_v(X),
	d_v(Y).

d_v(V) :-
	var(V),
	var_gen(V), !.
d_v(_).

init_vargen :-
	createStub(varg,1),
	asserta(varg(1)).
	
var_gen(V) :-
	retract(varg(N)),
	NN is N+1,
	asserta(varg(NN)),
	string_integer(NS,N),
	string_list(NS,NL),
	append("#VAR_",NL,X),
	name(V,X).

% predicates to update the rete network

% add a token to the rete net.  a token is of the form C-N with [S-V,...]
% ReqList gets bound with the values from the term added to the database.

addrete(Class,Name,TimeStamp) :-
	root(ID,Class-Name with ReqList, NextList),
	ffsend(Class,Name,ReqList,TimeStamp,NextList),
	fail.
addrete(_,_,_).

% fullfill the request list from the token, and send the instantiated
% token through the net.

ffsend(Class,Name,ReqList,TimeStamp,NextList) :-
	getf(Class,Name,ReqList),
	send(tok(add,[(Class-Name with ReqList)/TimeStamp]), NextList),
	!.

delrete(Class,Name,TimeStamp) :-
	root(ID,Class-Name with ReqList, NextList),
	delr(Class,Name,ReqList,TimeStamp),
	fail.
delrete(_,_,_).

delr(Class,Name,ReqList,TimeStamp) :-
	getf(Class,Name,ReqList),
	!, send(tok(del,[(Class-Name with ReqList)/TimeStamp]), NextList).
delr(Class,Name,ReqList,TimeStamp).

% send the new token to each of the succesor nodes

send(_,[]).
send(Tokens, [Node|Rest]) :-
	sen(Node, Tokens),
	send(Tokens, Rest).

% add or delete the new token from the appropriate memory, build new
% tokens from left or right and send them to successor nodes.

sen(rule-N,tok(AD,TokenList)) :-
	rul(N,TokenList,Actions),
	(AD = add, add_conflict_set(N,TokenList,Actions);
	 AD = del, del_conflict_set(N,TokenList,Actions)),
	!.
sen(Node-l,tok(AD,TokenList)) :-
	bi(Node,TokenList,Right,NextList),
	(AD = add, asserta( memory(Node-l,TokenList) );
	 AD = del, retract( memory(Node-l,TokenList) )),
	!,matchRight(Node,AD,TokenList,Right,NextList).
sen(Node-r,tok(AD,TokenList)) :-
	bi(Node,Left,TokenList,NextList),
	(AD = add, asserta( memory(Node-r,TokenList) );
	 AD = del, retract( memory(Node-r,TokenList) )),
	!,matchLeft(Node,AD,TokenList,Left,NextList).
sen(test-N,tok(AD,TokenList)) :-
	tes(N,TokenList,[Test/0],NextList),
	test(Test),
	append(TokenList,[Test/0],NewToks),
	!,send(tok(AD,NewToks),NextList).

matchRight(Node,AD,TokenList,Right,NextList) :-
	memory(Node-r,Right),
	append(TokenList,Right,NewToks),
	send(tok(AD,NewToks),NextList),
	fail.
matchRight(_,_,_,_,_).
	
matchLeft(Node,AD,TokenList,Left,NextList) :-
	memory(Node-l,Left),
	append(Left,TokenList,NewToks),
	send(tok(AD,NewToks),NextList),
	fail.
matchLeft(_,_,_,_,_).


	                                                                                                                           
% RETEFOOPS - forward chaining, frames, and Rete algorithm, also using
%     LEX and MEA to sort the conflict set.
%
% Copyright (c) Dennis Merritt, 1988

% operator definitions

:-op(800,xfx,==>).          % used to separate LHS and RHS of rule
:-op(500,xfy,#).            % used to separate attributes and values
:-op(810,fx,rule).          % used to define rule
:-op(700,xfy,#).            % used for unification instead of =
:-op(700,xfy,\=).		       % not equal
:-op(600,xfy,with).		    % used for frame instances in rules

main :- welcome, supervisor.
amzi_timer(T1):-get_time(T1).

welcome  :-
	write('         RETEFOOP - A Toy Production System'),nl,nl,
	write('This is an interpreter for files containing rules coded in the'),nl,
	write('FOOPS format.'),nl,nl,
	write('The => prompt accepts three commands:'),nl,nl,
	write('   load.       - prompts for name of rules file'),nl,
	write('                 enclose in single quotes'),nl,
	write('   compile.    - compiles rules into a rete net'),nl,
	write('   displaynet. - displays the rete net'),nl,
	write('   list.       - lists stuff'),nl,
	write('   list(X).    - lists things which match X'),nl,
	write('   options.    - allows setting of message levels'),nl,
	write('   go.         -  starts the inference'),nl,
	write('   exit.       - does what you"d expect'),nl,nl,
        mess_types(X),
        dmsg(mess_types(X)),
        do(list),do(displaynet),load,
           ignore(do(go)),
              ignore(lst),
              ignore(display_net).

% the supervisor, uses a repeat fail loop to read and process commands
% from the user

supervisor :-
	repeat,
	write('=>'),
	read(X),
	doit(X),
	X = exit.

doit(X) :-
	amzi_timer(T1),
	do(X),
	timer(T2),
	T is (T2 - T1) / 600,
	message(101,T),!.

% actions to take based on commands

do(exit) :- !.
do(go) :-
	initialize,trace,
	go, !.
do(load) :-load,!.
do(compile) :- compile,!.
do(displaynet) :- display_net,!.
do(list) :- lst,!.       % lists all of working storage
do(list(X)) :- lst(X),!. % lists all which match the pattern
do(options) :- set_messtypes,!.
do(_) :- message(102).

% loads the rules (Prolog terms) into the Prolog database

load :- reconsult('rete.rkb'),!,rete_compile.
   

load :-
	write('Enter the file name in single quotes (ex. ''room.rkb''.): '),
	read(F),
	reconsult(F),        % loads a rule file into interpreter work space
	rete_compile.			% ** rete change **

compile :-
	rete_compile.

% assert each of the initial conditions into working storage

initialize :-
	message(120),
	createStub(memory,2),
	createStub(inst,3),
	setchron(1),
	delf(all),
	createStub(conflict_set,1),
	assert(conflict_set([])),
	assert(mea(no)),
	initial_data(X),
	assert_list(X),
	message(121), !.
initialize :-
	message(103).

% working storage is represented frame instances - frinsts and also
% stored in a rete net

assert_list([]) :- !.
assert_list([H|T]) :-
	assert_ws(H),
	!,assert_list(T).

% the main inference loop, find a rule and try it.  if it fired, say so
% and repeat the process.  if not go back and try the next rule.  when
% no rules succeed, stop the inference.

:-dynamic(is_using_rule/1).

go :-
	conflict_set(CS),
	select_rule(CS,inst(ID,LHS,RHS)),
	message(104,ID),
        not(is_using_rule(ID)),
        ignore(retract(is_using_rule(_))),
        asserta(is_using_rule(ID)),
	(process(ID,RHS,LHS); true),		% action side might fail
	del_conflict_set(ID,LHS,RHS),
	!,go.
go :-
	conflict_set([]),
	finished, !.			% supplied in  tbox for what to do at end
go :-
	message(119).

del_conflict_set(N,TokenList,Action) :-
	conflict_set(CS),
	remove(inst(N,TokenList,Action),CS,CS2),
	message(105,N),
	retract( conflict_set(_) ),
	asserta( conflict_set(CS2) ).
del_conflict_set(N,TokenList,Action) :-
	message(106,N).

add_conflict_set(N,TokenList,Action) :-
	message(107,N),
	retract( conflict_set(CS) ),
	asserta( conflict_set([inst(N,TokenList,Action)|CS]) ).

select_rule(CS,R) :-
	message(122,CS),
	mea_filter(0,CS,[],CSR),
	lex_sort(CSR,R).

% sort the rest of the conflict set according to the lex strategy

lex_sort(L,R) :-
	build_keys(L,LK),
	sort(LK,X),
	reverse(X,[K-R|_]).

% build lists of time stamps for lex sort keys

build_keys([],[]).
build_keys([inst(N,TokenList,C)|T],[Key-inst(N,TokenList,C)|TR]) :-
	build_chlist(TokenList,ChL),
	sort(ChL,X),
	reverse(X,Key),
	build_keys(T,TR).

% build a list of just the times of the various matched attributes
% for use in rule selection

build_chlist([],[]).
build_chlist([_/Chron|T],[Chron|TC]) :-
	build_chlist(T,TC).	

% add the test for mea if appropriate that emphasizes the first attribute
% selected.

mea_filter(_,X,_,X) :- not(mea(yes)), !.
mea_filter(_,[],X,X).
mea_filter(Max,[inst(N,[A/T|Z],C)|X],Temp,ML) :-
	T < Max,
	!, mea_filter(Max,X,Temp,ML).
mea_filter(Max,[inst(N,[A/T|Z],C)|X],Temp,ML) :-
	T = Max,
	!, mea_filter(Max,X,[inst(N,[A/T|Z],C)|Temp],ML).
mea_filter(Max,[inst(N,[A/T|Z],C)|X],Temp,ML) :-
	T > Max,
	!, mea_filter(T,X,[inst(N,[A/T|Z],C)],ML).
	
get_ws(Prem,Time) :-
	conv(Prem,Class,Name,ReqList),
	getf(Class,Name,ReqList,Time).

assert_ws(Prem) :-
	message(109,Prem),
	conv(Prem,Class,Name,AList),
	addf(Class,Name,AList,TimeStamp),
	addrete(Class,Name,TimeStamp).
	
update_ws(Prem) :-
	conv(Prem,Class,Name,UList),
	frinst(Class,Name,_,TS),
	uptrf(Class,Name,UList,TimeStamp),		% note - does delrete in uptrf
	addrete(Class,Name,TimeStamp),
	!.
update_ws(Prem) :-
	message(108,Prem).
	
retract_ws(Prem/T) :- retract_ws(Prem).
retract_ws(Prem) :-
	conv(Prem,Class,Name,UList),
	delrete(Class,Name,TimeStamp),
	delf(Class,Name,UList).

conv(Class-Name with List, Class, Name, List).
conv(Class-Name, Class, Name, []).

% various tests allowed on the LHS

test(not(X)) :-
	get_ws(X,_),
	!,fail.
test(not(X)) :- !.
test(X#Y) :- X=Y,!.
test(X>Y) :- X>Y,!.
test(X>=Y) :- X>=Y,!.
test(X<Y) :- X<Y,!.
test(X=<Y) :- X=<Y,!.
test(X \= Y) :- not(X=Y),!.
test(X = Y) :- X=Y, !.
test(X = Y) :- X is Y,!.
test(is_on(X,Y)) :- is_on(X,Y),!.
test(call(X)) :- call(X).

% recursively execute each of the actions in the RHS list

process(N,[],_) :- message(118,N), !.
process(N,[Action|Rest],LHS) :-
	take(Action,LHS),
	!,process(N,Rest,LHS).
process(N,[Action|Rest],LHS) :-
	message(110,N), !, fail.

% if its retract, use the reference numbers stored in the Lrefs list,
% otherwise just take the action

take(retract(N),LHS) :-
	(N == all; integer(N)),
	retr(N,LHS),!.
take(A,_) :-take(A),!.

take(retract(X)) :- retract_ws(X), !.
take(assert(X)) :-
	assert_ws(X),
	!.
take(update(X)) :-
	update_ws(X),
	!.
take(X # Y) :- X=Y,!.
take(X = Y) :- X is Y,!.
take(write(X)) :- write(X),!.
take(write_line(X)) :- write_line(X),!.
take(nl) :- nl,!.
take(read(X)) :- read(X),!.
take(prompt(X,Y)) :- nl,write(X),read(Y),!.
take(cls) :- cls, !.
take(is_on(X,Y)) :- is_on(X,Y), !.
take(list(X)) :- lst(X), !.
take(call(X)) :- call(X).

% logic for retraction

retr(all,LHS) :-retrall(LHS),!.
retr(N,[]) :- message(111,N), !.
retr(N,[N#Prem|_]) :- retract_ws(Prem),!.
retr(N,[_|Rest]) :- !,retr(N,Rest).

retrall([]).
retrall([N#Prem|Rest]) :-
	retract_ws(Prem),
	!, retrall(Rest).
retrall([Prem|Rest]) :-
	retract_ws(Prem),
	!, retrall(Rest).
retrall([_|Rest]) :-		% must have been a test
	retrall(Rest).

% list all of the terms in working storage

lst :- printfs.

% lists all of the terms which match the pattern

lst(X) :-
	get_ws(X,_),
	write(X),nl,
	fail.
lst(_) :- !.

% maintain a time counter

setchron(N) :-
	retract( chron(_) ),
	asserta( chron(N) ),!.
setchron(N) :-
	asserta( chron(N) ).

getchron(N) :-
	retract( chron(N) ),
	NN is N + 1,
	asserta( chron(NN) ), !.

% this implements a frame based scheme for knowledge representation

:- op(600,fy,val).
:- op(600,fy,calc).
:- op(600,fy,def).
:- op(600,fy,add).
:- op(600,fy,del).

% prep_req takes a request of the form Slot-Val, and forms it into the
% more accurate req(Class,Slot,Facet,Value).  If no facet was mentioned
% in the original request, then the facet of "any" is used to indicate
% the system should use everything possible to find a value.

prep_req(Slot-X,req(C,N,Slot,val,X)) :- var(X), !.
prep_req(Slot-X,req(C,N,Slot,Facet,Val)) :-
	nonvar(X),
	X =.. [Facet,Val],
	facet_list(FL),
	is_on(Facet,FL), !.
prep_req(Slot-X,req(C,N,Slot,val,X)).

facet_list([val,def,calc,add,del,edit]).


% retrieve a list of slot values

get_frame(Class, ReqList) :-
	frame(Class, SlotList),
	slot_vals(Class,_,ReqList,SlotList).

getf(Class,Name,ReqList) :-
	getf(Class,Name,ReqList,_).
	
getf(Class,Name,ReqList,TimeStamp) :-
	frinst(Class, Name, SlotList, TimeStamp),
	slot_vals(Class, Name, ReqList, SlotList).

slot_vals(_,_,[],_).
slot_vals(C,N,[Req|Rest],SlotList) :-
	prep_req(Req,req(C,N,S,F,V)),
	find_slot(req(C,N,S,F,V),SlotList), 
	!, slot_vals(C,N,Rest,SlotList).
slot_vals(C,N, Req, SlotList) :-
	prep_req(Req,req(C,N,S,F,V)),
	find_slot(req(C,N,S,F,V), SlotList).

find_slot(req(C,N,S,F,V), SlotList) :-
	nonvar(V), !,
	find_slot(req(C,N,S,F,Val), SlotList), !,
	(Val = V; list(Val),is_on(V,Val)).
find_slot(req(C,N,S,F,V), SlotList) :-
	is_on(S-FacetList, SlotList), !,
	facet_val(req(C,N,S,F,V),FacetList).
find_slot(req(C,N,S,F,V), SlotList) :-
	is_on(ako-FacetList, SlotList),
	facet_val(req(C,N,ako,val,Ako),FacetList),
	(is_on(X,Ako); X = Ako),
	frame(X, HigherSlots),
	find_slot(req(C,N,S,F,V), HigherSlots), !.
find_slot(Req,_) :-
	message(112,Req),fail.

facet_val(req(C,N,S,F,V),FacetList) :-
	FV =.. [F,V],
	is_on(FV,FacetList), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(val ValList,FacetList),
	is_on(V,ValList), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(calc Pred,FacetList),
	CalcPred =.. [Pred,C,N,S-V],
	call(CalcPred), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(def V,FacetList), !.

% add a list of slot values

add_frame(Class, UList) :-
	old_slots(Class,SlotList),
	add_slots(Class,_,UList,SlotList,NewList),
	retract(frame(Class,_)),
	asserta(frame(Class,NewList)), !.

addf(Class,Nm,UList) :- addf(Class,Nm,UList,TimeStamp).

addf(Class,Nm,UList,TimeStamp) :-
	(var(Nm),genid(Name);Name=Nm),
	add_slots(Class,Name,[ako-Class|UList],SlotList,NewList),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ),
	!.

uptf(Class,Name,UList) :- uptf(Class,Name,UList,TS).

uptf(Class,Name,UList,TimeStamp) :-
	frinst(Class,Name,SlotList,TS),
	add_slots(Class,Name,UList,SlotList,NewList),
	retract( frinst(Class,Name,_,_) ),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ),
	!.
uptf(Class,Name,UList,TimeStamp) :- trace,
	message(113,[Class,Name,UList]).

uptrf(Class,Name,UList) :- uptf(Class,Name,UList,TS).

uptrf(Class,Name,UList,TimeStamp) :-
	frinst(Class,Name,SlotList,TS),
	add_slots(Class,Name,UList,SlotList,NewList),
	delrete(Class,Name,TS),
	retract( frinst(Class,Name,_,_) ),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ),
	!.
uptrf(Class,Name,UList,TimeStamp) :- trace,
	message(113,[Class,Name,UList]).


genid(G) :-
	retract(gid(N)),
	G is N + 1,
	asserta(gid(G)).

gid(100).
	
old_slots(Class,SlotList) :-
	frame(Class,SlotList), !.
old_slots(Class,[]) :-
	asserta(frame(Class,[])).

old_flots(Class,Name,SlotList) :-
	frinst(Class,Name,SlotList,_).

add_slots(_,_,[],X,X).
add_slots(C,N,[U|Rest],SlotList,NewList) :-
	prep_req(U,req(C,N,S,F,V)),
	add_slot(req(C,N,S,F,V),SlotList,Z),
	!, add_slots(C,N,Rest,Z,NewList).
add_slots(C,N,X,SlotList,NewList) :-
	prep_req(X,req(C,N,S,F,V)),
	add_slot(req(C,N,S,F,V),SlotList,NewList).

add_slot(req(C,N,S,F,V),SlotList,[S-FL2|SL2]) :-
	delete(S-FacetList,SlotList,SL2),
	add_facet(req(C,N,S,F,V),FacetList,FL2).

add_facet(req(C,N,S,F,V),FacetList,[FNew|FL2]) :-
	FX =.. [F,OldVal],
	delete(FX,FacetList,FL2),
	add_newval(OldVal,V,NewVal),
	!, check_add_demons(req(C,N,S,F,V),FacetList),
	FNew =.. [F,NewVal].

add_newval(X,Val,Val) :- var(X), !.
add_newval(OldList,ValList,NewList) :-
	list(OldList),
	list(ValList),
	append(ValList,OldList,NewList), !.
add_newval([H|T],Val,[Val,H|T]).
add_newval(_,Val,Val).

check_add_demons(req(C,N,S,F,V),FacetList) :-
	get_frame(C,S-add(Add)), !,
	AddFunc =.. [Add,C,N,S-V],
	call(AddFunc).
check_add_demons(_,_).


% delete a list of slot values

del_frame(Class) :-
	retract(frame(Class,_)).
del_frame(Class) :-
	message(114,Class).

del_frame(Class, UList) :-
	old_slots(Class,SlotList),
	del_slots(Class,_,UList,SlotList,NewList),
	retract(frame(Class,_)),
	asserta(frame(Class,NewList)).

delf(all) :-
	retract( frinst(_,_,_,_) ),
	fail.
delf(all).

delf(Class,Name) :-
	retract( frinst(Class,Name,_,_) ),
	!.
delf(Class,Name) :-
	message(115,Class-Name).

delf(Class,Name,[]) :- !, delf(Class,Name).
delf(Class,Name,UList) :-
	old_flots(Class,Name,SlotList),
	del_slots(Class,Name,UList,SlotList,NewList),
	retract( frinst(Class,Name,_,_) ),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ).

del_slots(_,_,[],X,X).
del_slots(C,N,[U|Rest],SlotList,NewList) :-
	prep_req(U,req(C,N,S,F,V)),
	del_slot(req(C,N,S,F,V),SlotList,Z),
	del_slots(C,N,Rest,Z,NewList).
del_slots(C,N,X,SlotList,NewList) :-
	prep_req(X,req(C,N,S,F,V)),
	del_slot(req(C,N,S,F,V),SlotList,NewList).

del_slot(req(C,N,S,F,V),SlotList,[S-FL2|SL2]) :-
	remove(S-FacetList,SlotList,SL2),
	del_facet(req(C,N,S,F,V),FacetList,FL2).
del_slot(Req,_,_) :-
	message(116,Req).

del_facet(req(C,N,S,F,V),FacetList,FL) :-
	FV =.. [F,V],
	remove(FV,FacetList,FL),
	!, check_del_demons(req(C,N,S,F,V),FacetList).
del_facet(req(C,N,S,F,V),FacetList,[FNew|FL]) :-
	FX =.. [F,OldVal],
	remove(FX,FacetList,FL),
	remove(V,OldVal,NewValList),
	FNew =.. [F,NewValList],	
	!, check_del_demons(req(C,N,S,F,V),FacetList).
del_facet(Req,_,_) :-
	message(117,Req).

check_del_demons(req(C,N,S,F,V),FacetList) :-
	get_frame(C,S-del(Del)), !,
	DelFunc =.. [Del,C,N,S-V],
	call(DelFunc).
check_del_demons(_,_).

% print a frame

print_frames :-
	frame(Class, SlotList),
	print_frame(Class),
	fail.
print_frames.

print_frame(Class) :-
	frame(Class,SlotList),
	write_line(['Frame:',Class]),
	print_slots(SlotList), nl.

printfs :-
	frame(Class,_),
	printf(Class,_),
	fail.
printfs.

printf(Class,Name) :-
	frinst(Class,Name,SlotList,Time),
	write_line(['Frame:',Class,Name,Time]),
	print_slots(SlotList), nl.

printf(Class) :-
	frinst(Class,Name,SlotList,Time),
	write_line(['Frame:',Class,Name,Time]),
	print_slots(SlotList), nl, fail.
printf(_).

print_slots([]).
print_slots([Slot|Rest]) :-
	write_line(['  Slot:',Slot]),
	print_slots(Rest).

% utilities

delete(X,[],[]).
delete(X,[X|Y],Y) :- !.
delete(X,[Y|Z],[Y|W]) :- delete(X,Z,W).

remove(X,[X|Y],Y) :- !.
remove(X,[Y|Z],[Y|W]) :- remove(X,Z,W).

is_on(X,[X|Y]).
is_on(X,[Y|Z]) :- is_on(X,Z).

write_line([]) :- nl.
write_line([H|T]) :-
	write(H),tab(1),
	write_line(T).
	
time_test :-
	write('TT> '),
	read(X),
	timer(T1),
	X,
	timer(T2),
	nl,nl,
	T is (T2 - T1) / 10,
	write(time-T).
/*
append([H|T], W, [H|Z]) :- append(T, W, Z).
append([], W, W).

member(X, [X|_]).
member(X, [_|T]) :- member(X,T).
*/
reverse(L1,L2) :- revzap(L1,[],L2).
revzap([X|L],L2,L3) :-
	revzap(L,[X|L2],L3).
revzap([],L,L).

% Message handling and messages

message(N) :- message(N,'').

message(N,Args) :-
	mess(N,break,Text),
	write(break),tab(1),write(N),write(': '),write(Text),write(Args),nl.
%	break.
message(N,Args) :-
	mess(N,error,Text),
	write(error),tab(1),write(N),write(': '),write(Text),write(Args),nl,
	!, fail.
message(N,Args) :-
	mess(N,Type,Text),
	mess_types(TT),
	member(Type,TT),
	write(Type),tab(1),write(N),write(': '),write(Text),write(Args),nl,
	!.
message(_,_).

:-dynamic(mess_types/1).

mess_types([info,trace,warning,debug]).

set_messtypes :-
	message(123,[info,warn,trace,error,debug]),
	mess_types(X),
	message(124,X),
	read(MT),
	retract( mess_types(_) ),
	asserta( mess_types(MT) ).

mess(101,info , 'Time for command: ').			% retefoops doit
mess(102,error, 'Invalid Command').			% retefoops do
mess(103,error, 'Initialization Error').		% retefoops initialize
mess(104,trace, 'Rule Firing: ').				% retefoops go
mess(105,trace, 'Conflict Set Delete: ').		% retefoops del_confli...
mess(106,trace, 'Failed to CS Delete: ').		% retefoops del_confli...
mess(107,trace, 'Conflict Set Add: ').			% retefoops add_confli...
mess(108,error, 'Update Fails for: ').			% retefoops update_ws
mess(109,trace, 'Asserting: ').				% retefoops add_ws
mess(110,trace, 'Failing Action Part: ').		% retefoops process
mess(111,error, 'Retract Error, no: ').			% retefoops take
mess(112,debugx, 'Frame error looking for: ').	% retefoops find_slot
mess(113,error, 'Frame instance update error: ').	% retefoops uptf
mess(114,error, 'No frame to delete: ').		% retefoops del_frame
mess(115,error, 'No instance to delete: ').		% retefoops delf
mess(116,error, 'Unable to delete slot: ').		% retefoops del_slot
mess(117,error, 'Unable to delete facet: ').		% retefoops del_facet
mess(118,trace, 'Rule Fired: ').				% retefoops process
mess(119,error, 'Premature end to run: ').		% retefoops go
mess(120,info, 'Initializing').				% retefoops initialize
mess(121,info, 'Initialization Complete').		% retefoops initialize
mess(122,debugx, 'Conflict Set').				% retefoops select_rule
mess(123,info, 'Legal Message Types: ').		% retefoops set_message
mess(124,info, 'Current Message Types: ').		% retefoops set_message

mess(201,info, 'Rule Rete Network Complete').	% retecomp rete_compil
mess(202,info, 'Rule: ').					% retecomp rete_comp
mess(203,error, 'Rule Failed to Compile: ').		% retecomp rete_comp
                                                         

:-welcome.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module implements the neq/2 constraint. It constraints two terms
% to be not identical.
%
%	Author:		Tom Schrijvers, K.U.Leuven
%	E-mail:		Tom.Schrijvers@cs.kuleuven.ac.be
%	Copyright:	2003-2004, K.U.Leuven
%
% Update 7/3/2004:
%   Now uses unifiable/3. It enables neq/2 to work with infinite terms.
% Update 11/3/2004:
%   Cleaned up code. Now uses just one or znode for every call to neq/2.
% Update Jul 8, 2005 (JW)
%   Fixed spelling unifyable --> unifiable
% Update Sep 4, 2007 (JW)
%   Added support for current_prolog_flag(occurs_check, error) case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(neq,[neq/2]).
:- use_module(library(lists)).
:- set_prolog_flag(generate_debug_info, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 	 	 
%% neq( ?X, ?Y) is semidet.
%
% Negated Equality.
%
neq(X,Y) :-
	X \== Y,
	diz_c_c(X,Y,_).


% 	 	 
%% diz( ?X, ?Y) is semidet.
%
% Diz.
%
diz(X,Y) :-
	X \== Y,
	diz_c_c(X,Y,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% types of attributes?
%	vardiz: X is a variable
%%	znode(Parent,Children,Variables,Counter)


% 	 	 
%% diz_unifiable( ?X, ?Y, ?Us) is semidet.
%
% Diz Unifiable.
%
diz_unifiable(X, Y, Us) :-
	(    current_prolog_flag(occurs_check, error) ->
	     catch(unifiable(X,Y,Us), error(occurs_check(_,_),_), false)
	;    unifiable(X, Y, Us)
	).


% 	 	 
%% diz_c_c( ?X, ?Y, ?OrNode) is semidet.
%
% Diz Class Class.
%
diz_c_c(X,Y,OrNode) :-
	(	diz_unifiable(X, Y, Unifier) ->
		( Unifier == [] ->
			zor_one_fail(OrNode)
		;
			diz_c_c_l(Unifier,OrNode)
		)
	;
		zor_succeed(OrNode)
	).



% 	 	 
%% diz_c_c_l( ?Unifier, ?OrNode) is semidet.
%
% Diz Class Class (list Version).
%
diz_c_c_l(Unifier,OrNode) :-
	length(Unifier,N),
	extend_zornode(OrNode,N,List,Tail),
	diz_c_c_l_aux(Unifier,OrNode,List,Tail).


% 	 	 
%% extend_zornode( ?OrNode, ?N, ?List, ?Vars) is semidet.
%
% Extend Zornode.
%
extend_zornode(OrNode,N,List,Vars) :-
	( get_attr(OrNode,neq,Attr) ->
		Attr = znode(M,Vars),
		O is N + M - 1
	;
		O = N,
		Vars = []
	),
	put_attr(OrNode,neq,znode(O,List)).


% 	 	 
%% diz_c_c_l_aux( :Term_G23042, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Diz Class Class (list Version) Aux.
%
diz_c_c_l_aux([],_,List,List).
diz_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
	List = [X=Y|Rest],
	add_zornode(X,Y,OrNode),
	diz_c_c_l_aux(Unifier,OrNode,Rest,Tail).


% 	 	 
%% add_zornode( ?X, ?Y, ?OrNode) is semidet.
%
% Add Zornode.
%
add_zornode(X,Y,OrNode) :-
	add_zornode_var1(X,Y,OrNode),
	( var(Y) ->
		add_zornode_var2(X,Y,OrNode)
	;
		true
	).


% 	 	 
%% add_zornode_var1( ?X, ?Y, ?OrNode) is semidet.
%
% Add Zornode Variable Secondary Helper.
%
add_zornode_var1(X,Y,OrNode) :-
	( get_attr(X,neq,Attr) ->
		Attr = vardiz(V1,V2),
		put_attr(X,neq,vardiz([OrNode-Y|V1],V2))
	;
		put_attr(X,neq,vardiz([OrNode-Y],[]))
	).


% 	 	 
%% add_zornode_var2( ?X, ?Y, ?OrNode) is semidet.
%
% Add Zornode Variable Extended Helper.
%
add_zornode_var2(X,Y,OrNode) :-
	( get_attr(Y,neq,Attr) ->
		Attr = vardiz(V1,V2),
		put_attr(Y,neq,vardiz(V1,[OrNode-X|V2]))
	;
		put_attr(Y,neq,vardiz([],[OrNode-X]))
	).


% 	 	 
%% predopts_analysis:attr_unify_hook( :TermX, ?Other) is semidet.
%
% Hook To [predopts_analysis:attr_unify_hook/2] For Module Neq.
% Attr Unify Hook.
%
attr_unify_hook(vardiz(V1,V2),Other) :-
	( var(Other) ->
		reverse_lookupz(V1,Other,OrNodes1,NV1),
		zor_one_failz(OrNodes1),
		get_attr(Other,neq,OAttr),
		OAttr = vardiz(OV1,OV2),
		reverse_lookupz(OV1,Other,OrNodes2,NOV1),
		zor_one_failz(OrNodes2),
		remove_obsoletez(V2,Other,NV2),
		remove_obsoletez(OV2,Other,NOV2),
		append(NV1,NOV1,CV1),
		append(NV2,NOV2,CV2),
		( CV1 == [], CV2 == [] ->
			del_attr(Other,neq)
		;
			put_attr(Other,neq,vardiz(CV1,CV2))
		)
	;
		verify_compoundz(V1,Other),
		verify_compoundz(V2,Other)
	).


% 	 	 
%% remove_obsoletez( :Term_G28702, ?VALUE2, ?VALUE3) is semidet.
%
% Remove Obsoletez.
%
remove_obsoletez([], _, []).
remove_obsoletez([N-Y|T], X, L) :-
        (   Y==X ->
            remove_obsoletez(T, X, L)
        ;   L=[N-Y|RT],
            remove_obsoletez(T, X, RT)
        ).


% 	 	 
%% reverse_lookupz( :Term_G5719, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Reverse Lookupz.
%
reverse_lookupz([],_,[],[]).
reverse_lookupz([N-X|NXs],Value,Nodes,Rest) :-
	( X == Value ->
		Nodes = [N|RNodes],
		Rest = RRest
	;
		Nodes = RNodes,
		Rest = [N-X|RRest]
	),
	reverse_lookupz(NXs,Value,RNodes,RRest).


% 	 	 
%% verify_compoundz( :Term_G11547, ?VALUE2) is semidet.
%
% Verify Compoundz.
%
verify_compoundz([],_).
verify_compoundz([OrNode-Y|Rest],X) :-
	( var(Y) ->
		true
	; OrNode == (-) ->
		true
	;
		diz_c_c(X,Y,OrNode)
	),
	verify_compoundz(Rest,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 	 	 
%% zor_succeed( ?OrNode) is semidet.
%
% Zor Succeed.
%
zor_succeed(OrNode) :- 
	( attvar(OrNode) ->
		get_attr(OrNode,neq,Attr),
		Attr = znode(_Counter,Pairs),
		del_attr(OrNode,neq),
		OrNode = (-),
		del_zor_diz(Pairs)
	;
		true
	).


% 	 	 
%% zor_one_failz( :Term_G17904) is semidet.
%
% Zor One Failz.
%
zor_one_failz([]).
zor_one_failz([N|Ns]) :-
	zor_one_fail(N),
	zor_one_failz(Ns).


% 	 	 
%% zor_one_fail( ?OrNode) is semidet.
%
% Zor One Fail.
%
zor_one_fail(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,neq,Attr),
		Attr = znode(Counter,Pairs),
		NCounter is Counter - 1,
		( NCounter == 0 ->
			fail
		;
			put_attr(OrNode,neq,znode(NCounter,Pairs))
		)
	;
		fail
	).


% 	 	 
%% del_zor_diz( :Term_G25765) is semidet.
%
% Remove/erase Zor Diz.
%
del_zor_diz([]).
del_zor_diz([X=Y|Xs]) :-
	cleanup_dead_znode(X),
	cleanup_dead_znode(Y),
	del_zor_diz(Xs).


% 	 	 
%% cleanup_dead_znode( ?X) is semidet.
%
% Cleanup Dead Znode.
%
cleanup_dead_znode(X) :-
	( attvar(X) ->
		get_attr(X,neq,Attr),
		Attr = vardiz(V1,V2),
		filter_dead_zorz(V1,NV1),
		filter_dead_zorz(V2,NV2),
		( NV1 == [], NV2 == [] ->
			del_attr(X,neq)
		;
			put_attr(X,neq,vardiz(NV1,NV2))
		)
	;
		true
	).


% 	 	 
%% filter_dead_zorz( :Term_G9411, ?VALUE2) is semidet.
%
% Filter Dead Zorz.
%
filter_dead_zorz([],[]).
filter_dead_zorz([Or-Y|Rest],List) :-
	( var(Or) ->
		List = [Or-Y|NRest]
	;
		List = NRest
	),
	filter_dead_zorz(Rest,NRest).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The attribute of a variable X is vardiz/2. The first argument is a
   list of pairs. The first component of each pair is an OrNode. The
   attribute of each OrNode is znode/2. The second argument of znode/2
   is a list of equations A = B. If the LHS of the first equation is
   X, then return a goal, otherwise don't because someone else will.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


% 	 	 
%%  ?VALUE1--> ?VALUE2 is semidet.
%
% -->.
%
attribute_goalz(Var) -->
	(   { get_attr(Var, neq, vardiz(Ors,_)) } ->
	    zor_znode(Ors, Var)
	;   zor_node(Var)
	).

zor_node(O) -->
        (   { get_attr(O, neq, znode(_, Pairs)) } ->
            { split_equals_list(Pairs, As, Bs) },
            mydiz(As, Bs),
            { del_attr(O, neq) }
        ;   []
        ).

zor_znode([], _)       --> [].
zor_znode([O-_|Os], X) -->
	(   { get_attr(O, neq, znode(_, Eqs)) } ->
            (   { Eqs = [LHS=_|_], LHS == X } ->
                { split_equals_list(Eqs, As, Bs) },
                mydiz(As, Bs),
                { del_attr(O, neq) }
            ;   []
            )
        ;   [] % or-znode already removed
        ),
	zor_znode(Os, X).

mydiz([X], [Y]) --> !, [neq(X, Y)].
mydiz(Xs0, Ys0) -->
        { reverse(Xs0, Xs), reverse(Ys0, Ys), % follow original order
          X =.. [f|Xs], Y =.. [f|Ys] },
        (   { diz_unifiable(X, Y, _) } ->
            [neq(X,Y)]
        ;   []
        ).


% 	 	 
%% split_equals_list( :Term_G14651, :Term_G14780, :Term_G14909) is semidet.
%
% Split Equals List.
%
split_equals_list([], [], []).
split_equals_list([A=B|ABs], [A|As], [B|Bs]) :-
        split_equals_list(ABs, As, Bs).

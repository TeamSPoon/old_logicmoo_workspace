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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_multivar.pl
:- module(logicmoo_util_multivar,[]).


:- if(current_predicate(logicmoo_utils:combine_logicmoo_utils/0)).
:- module(multi,
          [ multi/2,multi/1,
            domain/2                    % Var, ?Domain
          ]).
:- set_prolog_flag(generate_debug_info, true).



% 	 	 
%% nobind0( ?X) is semidet.
%
% Nobind Primary Helper.
%
nobind0(X):- when((?=(X, Y);?=(X, X);?=(X, xx);nonvar(X)),unbind(X,Y)).

% 	 	 
%% nobind1( ?X) is semidet.
%
% Nobind Secondary Helper.
%
nobind1(X):- multi(X,Y),freeze(X,unbind(X,Y)),put_attr(X, multi,[x(X)]), when((?=(X, Y);nonvar(X)),unbind(X,Y)).

% 	 	 
%% nobind2( ?X) is semidet.
%
% Nobind Extended Helper.
%
nobind2(X):- when(?=(X, X),unbind(X,_)).

% 	 	 
%% project_attributes( ?QueryVars, ?ResidualVars) is semidet.
%
% Project Attributes.
%
project_attributes(QueryVars, ResidualVars):-fmsg(project_attributes(QueryVars, ResidualVars)).

% 	 	 
%% verify_attributes( ?Var, ?Other, ?Goals) is semidet.
%
% Verify Attributes.
%
verify_attributes(Var, Other, Goals) :- fmsg(verify_attributes(Var, Other, Goals)).


% 	 	 
%% predopts_analysis:attr_unify_hook( ?X, ?Other) is semidet.
%
% Hook To [predopts_analysis:attr_unify_hook/2] For Module Logicmoo_util_multivar.
% Attr Unify Hook.
%
attr_unify_hook(X, Other) :-  fmsg(attr_unify_hook(X, Other)).
attr_unify_hook(mdif(V1,V2),Other) :- dif_attr_unify_hook(mdif(V1,V2),Other).


% 	 	 
%% predicate_options:attribute_goals( ?X, ?In, ?Out) is semidet.
%
% Hook To [predicate_options:attribute_goals/3] For Module Logicmoo_util_multivar.
% Attribute Goals.
%
attribute_goals(X,In,Out) :- get_attr(X, multi, List), fmsg(attribute_goals(X,In,Out,multi, List)), fail.

% 	 	 
%%  ?VALUE1--> ?VALUE2 is semidet.
%
% -->.
%
attribute_goals(X) -->
        { get_attr(X, multi, List) },
        [ put_attr(X, multi, List) ].



% 	 	 
%% fmsg( ?D) is semidet.
%
% Functor Message.
%
fmsg(D):-format('~N',[]),writeq(dmsg(D)),nl,dumpST,!,fail.

% 	 	 
%% fdmsg_and_fail( ?D) is semidet.
%
% Fdmsg And Fail.
%
fdmsg_and_fail(D):-format('~N',[]),writeq(D),nl,!,fail.



% 	 	 
%% logicmoo_util_multivar:uhook( ?Module, ?AttVal, ?Value) is semidet.
%
% Hook To [logicmoo_util_multivar:uhook/3] For Module Logicmoo_util_multivar.
% Uhook.
%
'$attvar':uhook(Multi, AttVal, Value):- fmsg(uhook(user,Multi, AttVal, Value)),fail.
'$attvar':uhook(freeze, C, A) :- !,
        (   attvar(A)
        ->  (   get_attr(A, freeze, B)
            ->  put_attr(A, freeze, '$and'(B, C))
            ;   put_attr(A, freeze, C)
            )
        ;   unfreeze(C)
        ).
'$attvar':uhook(A, B, C) :- 
        call(A:attr_unify_hook(B, C)).



% 	 	 
%% logicmoo_util_multivar: $wakeup( ?Multi) is semidet.
%
% Hook To [logicmoo_util_multivar: $wakeup/1] For Module Logicmoo_util_multivar.
% $wakeup.
%
'$attvar':'$wakeup'(Multi):- fmsg('$attvar':'$wakeup'(Multi)).
'$attvar':'$wakeup'([]).
'$attvar':'$wakeup'(wakeup(A, B, C)) :-
        '$attvar':call_all_attr_uhooks(A, B),
        '$attvar':'$wakeup'(C).



% 	 	 
%% unbind( ?X, ?Y) is semidet.
%
% Unbind.
%
unbind(X,Y):- writeq(X=Y),dumpST.


% 	 	 
%% test_nb is semidet.
%
% Test Non Backtackable.
%
test_nb:- nobind(X),X = 1.

/*

?-  nobind(X),X=1,writeq(yes(X)).

writeq(yes(X)).
X{multi..[1]}
X = _G56.


*/

:- public
	portray_attvar/1.


% 	 	 
%% logicmoo_varnames:portray_attvar( ?Var) is semidet.
%
% Hook To [logicmoo_varnames:portray_attvar/1] For Module Logicmoo_util_multivar.
% Portray Attribute Variable.
%
'$attvar':portray_attvar(Var) :-
	write('{<'),
	get_attrs(Var, Attr),
	catch(writeq(Attr),_,'$attvar':portray_attrs(Attr, Var)),
	write('>}').



:- use_module(library(ordsets)).


% 	 	 
%% domain( ?X, ?Dom) is semidet.
%
% Domain.
%
domain(X, Dom) :-
        var(Dom), !,
        get_attr(X, domain, Dom).
domain(X, List) :-
        list_to_ord_set(List, Domain),
        put_attr(Y, domain, Domain),
        X = Y.

%       An attributed variable with attribute value Domain has been
%       assigned the value Y


% 	 	 
%% domain_attr_unify_hook( ?Domain, ?Y) is semidet.
%
% Domain Attr Unify Hook.
%
domain_attr_unify_hook(Domain, Y) :-
        (   get_attr(Y, domain, Dom2)
        ->  ord_intersection(Domain, Dom2, NewDomain),
            (   NewDomain == []
            ->  fail
            ;   NewDomain = [Value]
            ->  Y = Value
            ;   put_attr(Y, domain, NewDomain)
            )
        ;   var(Y)
        ->  put_attr( Y, domain, Domain )
        ;   ord_memberchk(Y, Domain)
        ).

%       Translate attributes from this module to residual goals

domain_attribute_goals(X) -->
        { get_attr(X, domain, List) },
        [domain(X, List)].




:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 	 	 
%% multi( ?X) is semidet.
%
% Multi.
%
multi(X) :- ignore(fdmsg_and_fail(multi_t(X))), dif_c_c(X,zNotz,_).

% 	 	 
%% multi( ?X, ?Y) is semidet.
%
% Multi.
%
multi(X,Y) :- ignore(fdmsg_and_fail(multi_t(X,Y))), X \== Y,  dif_c_c(X,Y,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% types of attributes?
%	mdif: X is a variable
%%	node(Parent,Children,Variables,Counter)


% 	 	 
%% dif:dif_c_c( ?X, ?Y, ?OrNode) is semidet.
%
% Hook To [dif:dif_c_c/3] For Module Logicmoo_util_multivar.
% Dif Class Class.
%
dif_c_c(X,Y,OrNode) :-
	(	( current_prolog_flag(occurs_check, error) ->
			catch(unifiable(X,Y,Unifier), error(occurs_check(_,_),_), fail)
		  ;
			unifiable(X,Y,Unifier)
		) ->
		( Unifier == [] ->
			or_one_fail(OrNode)
		;
			dif_c_c_l(Unifier,OrNode)
		)
	;
		or_succeed(OrNode)
	).



% 	 	 
%% dif:dif_c_c_l( ?Unifier, ?OrNode) is semidet.
%
% Hook To [dif:dif_c_c_l/2] For Module Logicmoo_util_multivar.
% Dif Class Class (list Version).
%
dif_c_c_l(Unifier,OrNode) :-
	length(Unifier,N),
	extend_ornode(OrNode,N,List,Tail),
	dif_c_c_l_aux(Unifier,OrNode,List,Tail).


% 	 	 
%% dif:extend_ornode( ?OrNode, ?N, ?List, ?Vars) is semidet.
%
% Hook To [dif:extend_ornode/4] For Module Logicmoo_util_multivar.
% Extend Ornode.
%
extend_ornode(OrNode,N,List,Vars) :-
	( get_attr(OrNode,multi,Attr) ->
		Attr = node(M,Vars),
		O is N + M - 1
	;
		O = N,
		Vars = []
	),
	put_attr(OrNode,multi,node(O,List)).


% 	 	 
%% dif:dif_c_c_l_aux( :Term_G24100, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Hook To [dif:dif_c_c_l_aux/4] For Module Logicmoo_util_multivar.
% Dif Class Class (list Version) Aux.
%
dif_c_c_l_aux([],_,List,List).
dif_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
	List = [X=Y|Rest],
	add_ornode(X,Y,OrNode),
	dif_c_c_l_aux(Unifier,OrNode,Rest,Tail).


% 	 	 
%% dif:add_ornode( ?X, ?Y, ?OrNode) is semidet.
%
% Hook To [dif:add_ornode/3] For Module Logicmoo_util_multivar.
% Add Ornode.
%
add_ornode(X,Y,OrNode) :-
	add_ornode_var1(X,Y,OrNode),
	( var(Y) ->
		add_ornode_var2(X,Y,OrNode)
	;
		true
	).


% 	 	 
%% dif:add_ornode_var1( ?X, ?Y, ?OrNode) is semidet.
%
% Hook To [dif:add_ornode_var1/3] For Module Logicmoo_util_multivar.
% Add Ornode Variable Secondary Helper.
%
add_ornode_var1(X,Y,OrNode) :-
	( get_attr(X,multi,Attr) ->
		Attr = mdif(V1,V2),
		put_attr(X,multi,mdif([OrNode-Y|V1],V2))
	;
		put_attr(X,multi,mdif([OrNode-Y],[]))
	).


% 	 	 
%% dif:add_ornode_var2( ?X, ?Y, ?OrNode) is semidet.
%
% Hook To [dif:add_ornode_var2/3] For Module Logicmoo_util_multivar.
% Add Ornode Variable Extended Helper.
%
add_ornode_var2(X,Y,OrNode) :-
	( get_attr(Y,multi,Attr) ->
		Attr = mdif(V1,V2),
		put_attr(Y,multi,mdif(V1,[OrNode-X|V2]))
	;
		put_attr(Y,multi,mdif([],[OrNode-X]))
	).



% 	 	 
%% dif_attr_unify_hook( ?VALUE1, ?VALUE2) is semidet.
%
% Dif Attr Unify Hook.
%
dif_attr_unify_hook(mdif(V1,V2),Other) :-
	( var(Other) ->
		reverse_lookups(V1,Other,OrNodes1,NV1),
		or_one_fails(OrNodes1),
		get_attr(Other,multi,OAttr),
		OAttr = mdif(OV1,OV2),
		reverse_lookups(OV1,Other,OrNodes2,NOV1),
		or_one_fails(OrNodes2),
		remove_obsolete(V2,Other,NV2),
		remove_obsolete(OV2,Other,NOV2),
		append(NV1,NOV1,CV1),
		append(NV2,NOV2,CV2),
		( CV1 == [], CV2 == [] ->
			del_attr(Other,multi)
		;
			put_attr(Other,multi,mdif(CV1,CV2))
		)
	;
		verify_compounds(V1,Other),
		verify_compounds(V2,Other)
	).


% 	 	 
%% dif:remove_obsolete( :Term_G5895, ?VALUE2, ?VALUE3) is semidet.
%
% Hook To [dif:remove_obsolete/3] For Module Logicmoo_util_multivar.
% Remove Obsolete.
%
remove_obsolete([], _, []).
remove_obsolete([N-Y|T], X, L) :-
        (   Y==X ->
            remove_obsolete(T, X, L)
        ;   L=[N-Y|RT],
            remove_obsolete(T, X, RT)
        ).


% 	 	 
%% dif:reverse_lookups( :Term_G12164, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Hook To [dif:reverse_lookups/4] For Module Logicmoo_util_multivar.
% Reverse Lookups.
%
reverse_lookups([],_,[],[]).
reverse_lookups([N-X|NXs],Value,Nodes,Rest) :-
	( X == Value ->
		Nodes = [N|RNodes],
		Rest = RRest
	;
		Nodes = RNodes,
		Rest = [N-X|RRest]
	),
	reverse_lookups(NXs,Value,RNodes,RRest).


% 	 	 
%% dif:verify_compounds( :Term_G17992, ?VALUE2) is semidet.
%
% Hook To [dif:verify_compounds/2] For Module Logicmoo_util_multivar.
% Verify Compounds.
%
verify_compounds([],_).
verify_compounds([OrNode-Y|Rest],X) :-
	( var(Y) ->
		true
	; OrNode == (-) ->
		true
	;
		dif_c_c(X,Y,OrNode)
	),
	verify_compounds(Rest,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 	 	 
%% dif:or_succeed( ?OrNode) is semidet.
%
% Hook To [dif:or_succeed/1] For Module Logicmoo_util_multivar.
% Or Succeed.
%
or_succeed(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,multi,Attr),
		Attr = node(_Counter,Pairs),
		del_attr(OrNode,multi),
		OrNode = (-),
		del_or_dif(Pairs)
	;
		true
	).


% 	 	 
%% dif:or_one_fails( :Term_G24349) is semidet.
%
% Hook To [dif:or_one_fails/1] For Module Logicmoo_util_multivar.
% Or One Fails.
%
or_one_fails([]).
or_one_fails([N|Ns]) :-
	or_one_fail(N),
	or_one_fails(Ns).


% 	 	 
%% dif:or_one_fail( ?OrNode) is semidet.
%
% Hook To [dif:or_one_fail/1] For Module Logicmoo_util_multivar.
% Or One Fail.
%
or_one_fail(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,multi,Attr),
		Attr = node(Counter,Pairs),
		NCounter is Counter - 1,
		( NCounter == 0 ->
			fail
		;
			put_attr(OrNode,multi,node(NCounter,Pairs))
		)
	;
		fail
	).


% 	 	 
%% dif:del_or_dif( :Term_G7545) is semidet.
%
% Hook To [dif:del_or_dif/1] For Module Logicmoo_util_multivar.
% Remove/erase Or Dif.
%
del_or_dif([]).
del_or_dif([X=Y|Xs]) :-
	cleanup_dead_nodes(X),
	cleanup_dead_nodes(Y),
	del_or_dif(Xs).


% 	 	 
%% dif:cleanup_dead_nodes( ?X) is semidet.
%
% Hook To [dif:cleanup_dead_nodes/1] For Module Logicmoo_util_multivar.
% Cleanup Dead Nodes.
%
cleanup_dead_nodes(X) :-
	( attvar(X) ->
		get_attr(X,multi,Attr),
		Attr = mdif(V1,V2),
		filter_dead_ors(V1,NV1),
		filter_dead_ors(V2,NV2),
		( NV1 == [], NV2 == [] ->
			del_attr(X,multi)
		;
			put_attr(X,multi,mdif(NV1,NV2))
		)
	;
		true
	).


% 	 	 
%% dif:filter_dead_ors( :Term_G18363, ?VALUE2) is semidet.
%
% Hook To [dif:filter_dead_ors/2] For Module Logicmoo_util_multivar.
% Filter Dead Ors.
%
filter_dead_ors([],[]).
filter_dead_ors([Or-Y|Rest],List) :-
	( var(Or) ->
		List = [Or-Y|NRest]
	;
		List = NRest
	),
	filter_dead_ors(Rest,NRest).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The attribute of a variable X is mdif/2. The first argument is a
   list of pairs. The first component of each pair is an OrNode. The
   attribute of each OrNode is node/2. The second argument of node/2
   is a list of equations A = B. If the LHS of the first equation is
   X, then return a goal, otherwise don't because someone else will.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

dif_attribute_goals(Var) -->
	(   { get_attr(Var, multi, mdif(Ors,_)) } ->
	    or_nodes(Ors, Var)
	;   or_node(Var)
	).

or_node(O) -->
        (   { get_attr(O, multi, node(_, Pairs)) } ->
            { eqs_lefts_rights(Pairs, As, Bs) },
            mydif(As, Bs),
            { del_attr(O, multi) }
        ;   []
        ).

or_nodes([], _)       --> [].
or_nodes([O-_|Os], X) -->
	(   { get_attr(O, multi, node(_, Eqs)) } ->
            (   { Eqs = [LHS=_|_], LHS == X } ->
                { eqs_lefts_rights(Eqs, As, Bs) },
                mydif(As, Bs),
                { del_attr(O, multi) }
            ;   []
            )
        ;   [] % or-node already removed
        ),
	or_nodes(Os, X).

mydif([X], [Y]) --> !, [multi(X, Y)].
mydif(Xs0, Ys0) --> [multi(X,Y)],
        { reverse(Xs0, Xs), reverse(Ys0, Ys), % follow original order
          X =.. [f|Xs], Y =.. [f|Ys] }.


% 	 	 
%% dif:eqs_lefts_rights( :Term_G23603, :Term_G23732, :Term_G23861) is semidet.
%
% Hook To [dif:eqs_lefts_rights/3] For Module Logicmoo_util_multivar.
% Using (==/2) (or =@=/2) )s Lefts Rights.
%
eqs_lefts_rights([], [], []).
eqs_lefts_rights([A=B|ABs], [A|As], [B|Bs]) :-
        eqs_lefts_rights(ABs, As, Bs).

:- else.
:- include('logicmoo_util_header.pi').
:- endif.



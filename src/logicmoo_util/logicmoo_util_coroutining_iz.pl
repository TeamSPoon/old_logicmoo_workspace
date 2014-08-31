/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Schrijvers, K.U.Leuven
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U.Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module implements the iz/2 constraint. It allows two terms
% to have at one time 
%
%	Author: 	Tom Schrijvers, K.U.Leuven
% 	E-mail: 	Tom.Schrijvers@cs.kuleuven.ac.be
%	Copyright:	2003-2004, K.U.Leuven
%
% Update 7/3/2004:
%   Now uses unifiable/3. It enables dif/2 to work with infinite terms.
% Update 11/3/2004:
%   Cleaned up code. Now uses just one or node for every call to dif/2.
% Update Jul 8, 2005 (JW)
%   Fixed spelling unifyable --> unifiable
% Update Sep 4, 2007 (JW)
%   Added support for current_prolog_flag(occurs_check, error) case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(iz,[iz/2]).
:- use_module(library(lists)).
:- set_prolog_flag(generate_debug_info, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iz:iz(X,Y) :-
	iz_c_c(X,Y,_).


iz:attr_portray_hook(variz(L,U),_Var) :-
	write(iz([L-U])).

:-meta_predicate(mshow_call(0)).

mshow_call(C):-call(C).

unifiable_iz(A,B,C):-mshow_call(unifiable(A,B,C)).

not_or_one_fail(OrNode):- mshow_call(or_one_fail(OrNode)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% types of attributes?
% 	variz: X is a variable
%%	node(Parent,Children,Variables,Counter)

iz_c_c(X,Y,OrNode) :-
	(	( current_prolog_flag(occurs_check, error) ->
			catch(unifiable_iz(X,Y,Unifier), error(occurs_check(_,_),_), fail)
		  ;
			unifiable_iz(X,Y,Unifier)
		) ->
		( Unifier == [] ->
			not_or_one_fail(OrNode)
		;
			mshow_call(iz_c_c_l(Unifier,OrNode))
		)
	;
		or_succeed(OrNode)
	).


iz_c_c_l(Unifier,OrNode) :-
	length(Unifier,N),
	extend_ornode(OrNode,N,List,Tail),
	iz_c_c_l_aux(Unifier,OrNode,List,Tail).

extend_ornode(OrNode,N,List,Vars) :-
	( get_attr(OrNode,iz,Attr) ->
		Attr = node(M,Vars),
		O is N + M - 1
	;
		O = N,
		Vars = []
	),
	put_attr(OrNode,iz,node(O,List)).

iz_c_c_l_aux([],_,List,List).
iz_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
	List = [X=Y|Rest],
	add_ornode(X,Y,OrNode),
	iz_c_c_l_aux(Unifier,OrNode,Rest,Tail).

add_ornode(X,Y,OrNode) :-
	add_ornode_var1(X,Y,OrNode),
	( var(Y) ->
		add_ornode_var2(X,Y,OrNode)
	;
		true
	).

add_ornode_var1(X,Y,OrNode) :-
	( get_attr(X,iz,Attr) ->
		Attr = variz(V1,V2),
		show_call(put_attr(X,iz,variz([OrNode-Y|V1],V2)))
	;
		put_attr(X,iz,variz([OrNode-Y],[]))
	).

add_ornode_var2(X,Y,OrNode) :-
	( get_attr(Y,iz,Attr) ->
		Attr = variz(V1,V2),
		put_attr(Y,iz,variz(V1,[OrNode-X|V2]))
	;
		put_attr(Y,iz,variz([],[OrNode-X]))
	).

iz:attr_unify_hook(variz(V1,V2),Other) :-
	( var(Other) ->
		reverse_lookups(V1,Other,OrNodes1,NV1),
		or_one_fails(OrNodes1),
		get_attr(Other,iz,OAttr),
		OAttr = variz(OV1,OV2),
		reverse_lookups(OV1,Other,OrNodes2,NOV1),
		or_one_fails(OrNodes2),
		remove_obsolete(V2,Other,NV2),
		remove_obsolete(OV2,Other,NOV2),
		append(NV1,NOV1,CV1),
		append(NV2,NOV2,CV2),
		( CV1 == [], CV2 == [] ->
			del_attr(Other,iz)
		;
			put_attr(Other,iz,variz(CV1,CV2))
		)
	;
		verify_compounds(V1,Other),
		verify_compounds(V2,Other)
	).

remove_obsolete(X,_,X):- wasiz,!.
remove_obsolete([], _, []).
remove_obsolete([N-Y|T], X, L) :-
        (   Y==X ->
            remove_obsolete(T, X, L)
        ;   L=[N-Y|RT],
            remove_obsolete(T, X, RT)
        ).

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

verify_compounds([],_).
verify_compounds([Node-Y|Rest],X) :-
	( var(Y) ->
		true
	;
		iz_c_c(X,Y,Node)
	),
	verify_compounds(Rest,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
or_succeed(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,iz,Attr),
		Attr = node(_Counter,Pairs),
		del_attr(OrNode,iz),
		OrNode = (-),
		del_or_iz(Pairs)
	;
		true
	).

or_one_fails([]).
or_one_fails([N|Ns]) :-
	or_one_fail(N),
	or_one_fails(Ns).

or_one_fail(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,iz,Attr),
		Attr = node(Counter,Pairs),
		NCounter is Counter - 1,
		( NCounter == 0 ->
			fail
		;
			put_attr(OrNode,iz,node(NCounter,Pairs))
		)
	;
		fail
	).

del_or_iz([]).
del_or_iz([X=Y|Xs]) :-
	cleanup_dead_nodes(X),
	cleanup_dead_nodes(Y),
	del_or_iz(Xs).

cleanup_dead_nodes(X) :-
 	( attvar(X) ->
 		get_attr(X,iz,Attr),
		Attr = variz(V1,V2),
		filter_dead_ors(V1,NV1),
		filter_dead_ors(V2,NV2),
		( NV1 == [], NV2 == [] ->
			del_attr(X,iz)
		;
			put_attr(X,iz,variz(NV1,NV2))
		)
	;
		true
	).

filter_dead_ors([],[]).
filter_dead_ors([Or-Y|Rest],List) :-
	( var(Or) ->
		List = [Or-Y|NRest]
	;
		List = NRest
	),
	filter_dead_ors(Rest,NRest).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The attribute of a variable X is variz/2. The first argument is a
   list of pairs. The first component of each pair is an OrNode. The
   attribute of each OrNode is node/2. The second argument of node/2
   is a list of equations A = B. If the LHS of the first equation is
   X, then return a goal, otherwise don't because someone else will.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

iz:attribute_goals(Var) -->
	(   { get_attr(Var, iz, variz(Ors,_)) } ->
	    or_nodes(Ors, Var)
	;   or_node(Var)
	).

or_node(O) -->
        (   { get_attr(O, iz, node(_, Pairs)) } ->
            { eqs_lefts_rights(Pairs, As, Bs) },
            myiz(As, Bs),
            { del_attr(O, iz) }
        ;   []
        ).

or_nodes([], _)       --> [].
or_nodes([O-_|Os], X) -->
	(   { get_attr(O, iz, node(_, Eqs)) } ->
            (   { Eqs = [LHS=_|_], LHS == X } ->
                { eqs_lefts_rights(Eqs, As, Bs) },
                myiz(As, Bs),
                { del_attr(O, iz) }
            ;   []
            )
        ;   [] % or-node already removed
        ),
	or_nodes(Os, X).

myiz([X], [Y]) --> !, [iz(X, Y)].
myiz(Xs0, Ys0) --> [iz(X,Y)],
        { reverse(Xs0, Xs), reverse(Ys0, Ys), % follow original order
          X =.. [f|Xs], Y =.. [f|Ys] }.

eqs_lefts_rights([], [], []).
eqs_lefts_rights([A=B|ABs], [A|As], [B|Bs]) :-
        eqs_lefts_rights(ABs, As, Bs).




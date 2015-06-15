
%% fluent.pl - CHR for SICSTUS

%% $Id: fluent.chr, v 2.0 2004/01/22 00:26:00 $
%%
%% FLUX: a Prolog library for high-level programming of cognitive agents
%% Copyright 2003, 2004  Michael Thielscher
%% This file belongs to the flux kernel package distributed at
%%   http://www.fluxagent.org
%%
%% This library is free software; you can redistribute it and/or modify it
%% under the terms of the GNU Library General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or (at your
%% option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
%% License for more details.
%%
%% You should have received a copy of the GNU Library General Public License
%% along with this library; if not, write to the Free Software Foundation,
%% Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
%%
%% Consult the file COPYING for license details.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Preamble
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handler fluent.

constraints not_holds/2, not_holds_all/2, duplicate_free/1,
            or_holds/2, or_holds/3, cancel/2, cancelled/2.

option(check_guard_bindings,off).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Constraint Handling Rules for state constraints
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


not_holds(F, [F1|Z]) <=> neq(F, F1), not_holds(F, Z).
not_holds(_, [])     <=> true.

not_holds_all(F, [F1|Z]) <=> neq_all(F, F1), not_holds_all(F, Z).
not_holds_all(_, [])     <=> true.

not_holds_all(F, Z) \ not_holds(G, Z)     <=> inst(G, F) | true.
not_holds_all(F, Z) \ not_holds_all(G, Z) <=> inst(G, F) | true.

duplicate_free([F|Z]) <=> not_holds(F,Z), duplicate_free(Z).
duplicate_free([])    <=> true.

or_holds([F],Z) <=> F\=eq(_,_) | fluent_holds_in_state(F,Z).

or_holds(V,_Z) <=> \+ ( member(F,V),F\=eq(_,_) ) | or_and_eq(V,D), call(D).

or_holds(V,[]) <=> member(F, V, W), F\=eq(_,_) | or_holds(W,[]).

or_holds(V,_Z) <=> member(eq(X,Y),V), or_neq(exists,X,Y,D), \+ call(D) | true.
or_holds(V,Z) <=> member(eq(X,Y),V,W), \+ (and_eq(X,Y,D), call(D)) | or_holds(W,Z).

not_holds(F, Z) \ or_holds(V, Z) <=> member(G, V, W), F==G | or_holds(W, Z).

not_holds_all(F, Z) \ or_holds(V, Z) <=> member(G, V, W), inst(G, F)
                                         | or_holds(W, Z).

or_holds(V, [F|Z]) <=> or_holds(V, [], [F|Z]).

or_holds([G|V],W,[F|Z]) <=> true | ( G==F -> true ;
                            G\=F -> or_holds(V,[G|W],[F|Z]) ;
                            G=..[_|ArgX], F=..[_|ArgY],
                            or_holds(V,[eq(ArgX,ArgY),G|W],[F|Z])).

or_holds([],W,[_|Z]) <=> or_holds(W,Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Constraint Handling Rules for cancellation of constraints on a fluent
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cancel(F,Z) \ not_holds(G,Z)     <=> \+ F\=G | true.

cancel(F,Z) \ not_holds_all(G,Z) <=> \+ F\=G | true.

cancel(F,Z) \ or_holds(V,Z)      <=> member(G,V), \+ F\=G | true.

cancel(F,Z), cancelled(F,Z) <=> true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Auxiliary clauses
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neq(F, F1)     :- or_neq(exists, F, F1).
neq_all(F, F1) :- or_neq(forall, F, F1).

or_neq(Q, Fx, Fy) :-
  functor(Fx, F, M), functor(Fy, G, N),
  ( F=G, M=N -> Fx =.. [_|ArgX], Fy =.. [_|ArgY], or_neq(Q, ArgX, ArgY, D), call(D)
              ; true ).

or_neq(_, [], [], (0#\=0)).
or_neq(Q, [X|X1], [Y|Y1], D) :-
  or_neq(Q, X1, Y1, D1),
  ( Q=forall, var(X), \+ is_domain(X) -> ( binding(X,X1,Y1,YE) ->

                                         D=((Y#\=YE)#\/D1) ; D=D1 )
                                         ; D=((X#\=Y)#\/D1) ).
                                         
binding(X,[X1|ArgX],[Y1|ArgY],Y) :-
   X==X1 -> Y=Y1 ; binding(X,ArgX,ArgY,Y).

and_eq([], [], (0#=0)).
and_eq([X|X1], [Y|Y1], D) :-
   and_eq(X1, Y1, D1),
   D = ((X#=Y)#/\D1).

or_and_eq([], (0#\=0)).
or_and_eq([eq(X,Y)|Eq], (D1#\/D2)) :-
   and_eq(X,Y,D1),
   or_and_eq(Eq,D2).

%% inst checks whether G is an instance of F and if so whether there is no
%% domain variable which is bound to some value in this instance

inst(G,F) :- subsumes_chk(F,G), var_chk(G,F).

var_chk(X,Y) :- var(Y), X==Y.
var_chk(_,Y) :- var(Y), \+ fd_var(Y).
var_chk(X,Y) :- is_list(Y), var_chk_list(X,Y).
var_chk(X,Y) :- compound(Y), \+ is_list(Y), X=..[_|Xs], Y=..[_|Ys], var_chk_list(Xs,Ys).

var_chk_list([],[]).
var_chk_list([X|Xs],[Y|Ys]) :- var_chk(X,Y), var_chk_list(Xs,Ys).

member(X, [X|T], T).
member(X, [H|T], [H|T1]) :- member(X, T, T1).

is_domain(X) :- fd_var(X), \+ ( fd_max(X,sup), fd_min(X,inf)).


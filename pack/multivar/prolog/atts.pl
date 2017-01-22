/*  Part of SWI-Prolog
    Author:        Douglas R. Miles, Jan Wielemaker
    E-mail:        logicmoo@gmail.com, jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org http://www.prologmoo.com
    Copyright (C): 2015, University of Amsterdam
                                    VU University Amsterdam
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/
:- module(attributes, ['attribute'/1,get_atts/2,put_atts/2,
   set_dict_attvar_reader/1,
   dict_to_attvar/2,
   dict_to_attvar/3,
   op(1150, fx, 'attribute')]).

:- meta_predicate('attribute'(:)).
:- meta_predicate(get_atts(+,:)).
:- meta_predicate(put_atts(+,:)).
%:- meta_predicate(dict_to_attvar(:,?)).

:- use_module(library(ordsets)).

% auto-define attributes otherwise signal error is undeclared attributes are used
:- create_prolog_flag(atts_declared,auto,[type(atom),keep(true)]).
% Users might need to read docs to decided they rather have auto?
:- set_prolog_flag(atts_declared,true).
% What is all this fuss about?
% We need some answer to what happens when ?- user:put_atts(Var,+a(1)).
% if attibute a/1 is declared in one module at least we have some sense
% Still doesnt solve the problem when if a/1 is declared in several modules
% Should we use the import_module/2 Dag?
% Still doesnt solve the problem when if a/1 is declared only in one unseen module!
% Though every option is simple to implement, it should be left to programmers to decide with flags/options
% and not left just to those editing these files.  Still we need to pick a default.


:- dynamic protobute/1.

%%    attribute(+AttributeSpec).
%
% :- attribute AttributeSpec,..., AttributeSpec.
%
% where each AttributeSpec has the form Functor/Arity.
% Having declared some attribute names, these attributes can be added, 
% updated and deleted from unbound variables using the following two predicates 
%(get_atts/2 and put_atts/2) defined in the module atts. 
% For each declared attribute name, any variable can have at most one such attribute (initially it has none).
'attribute'(M:V):- new_attribute(V,M),!.

new_attribute(V,M) :- var(V), !, throw(error(instantiation_error,'attribute'(M:V))).
new_attribute(Na/Ar,Mod) :- !, functor(At,Na,Ar),new_attribute(At,Mod).
new_attribute(Mod:ANY,_) :- !, new_attribute(ANY,Mod).
new_attribute([],_).
new_attribute((At1,At2),M) :- new_attribute(At1,M), new_attribute(At2,M).
new_attribute([At1|At2],M) :- new_attribute(At1,M), new_attribute(At2,M).
new_attribute(Na/Ar,Mod) :- functor(At,Na,Ar), (protobute(Mod:At) -> true; assertz(protobute(Mod:At))).
new_attribute(Mod:ANY,_) :- new_attribute(ANY,Mod).
new_attribute(At,Mod) :- (protobute(Mod:At) -> true; assertz(protobute(Mod:At))).

%%    put_atts(+Var, +AccessSpec)
%
% Sets the attributes of Var according to AccessSpec.
%
% Non-variable terms in Var cause a type error. 
%  if curent_prolog_flag(atts_compat,xsb).
%
% The effect of put_atts/2 are undone on backtracking. 
% (prefix + may be dropped for convenience).
% The prefixes of AccessSpec have the following meaning:
%  +(Attribute):
%     The corresponding actual attribute is set to Attribute. If the actual attribute was already present, it is simply replaced.
%  -(Attribute):
%     The corresponding actual attribute is removed. If the actual attribute is already absent, nothing happens.
%
%  Should we ignore The arguments of Attribute, only the name and arity are relevant? Currently coded to
%
% ==
% ?- m1:put_atts(Var,+a(x1,y1)).
% put_attr(Var, m1, [a(x1, y1)]).
%
% ?- m1:put_atts(V,+a(x1,y1)),m1:put_atts(V,+b(x1,y1)),m1:put_atts(V,-a(_,_)),m2:put_atts(V,+b(x2,y2)).
% put_attr(V, m1, [b(x1, y1)]),
% put_attr(V, m2, [b(x2, y2)]) .

put_atts(Var,M:Atts):- put_atts(Var,M,Atts).


%%    get_atts(+Var, ?AccessSpec) 
%
% Gets the attributes of Var according to AccessSpec. 
%  If AccessSpec is unbound, it will be bound to a list of all set attributes of Var. 
%
% Non-variable terms in Var cause a type error. 
%  if curent_prolog_flag(atts_compat,xsb).
%
% AccessSpec is either +(Attribute), -(Attribute), or a list of such 
% (prefix + may be dropped for convenience).
%
% The prefixes in the AccessSpec have the following meaning:
%  +(Attribute):
%     The corresponding actual attribute must be present and is unified with Attribute.
%  -(Attribute):
%     The corresponding actual attribute must be absent.
%
%  Should we ignore The arguments of Attribute are ignored, only the name and arity are relevant?
%   yes = XSB_compat, no = less control and perf
%
% ==
% ?- m1:put_atts(Var,+a(x1,y1)),m1:get_atts(Var,-missing(x1,y1)).
% put_attr(Var, m1, [a(x1, y1)]).
%
% ?- m1:put_atts(Var,+a(x1,y1)),m1:get_atts(Var,X).
% X=[a(x1, y1)],
% put_attr(Var, m1, [a(x1, y1)]).
% ==
% TODO/QUESTION  user:get_atts(Var,Atts) ->  ??? only attributes in 'user' or all attributes??? Attr=[m1:...]

get_atts(Var,M:Atts):- get_atts(Var,M,Atts).


atts_exist(_A,_At):- current_prolog_flag(atts_declared,auto),!.
atts_exist(_A,_At):- current_prolog_flag(set_dict_attvar_reader,true),!.
atts_exist(M,At):- \+ \+ (M:dynamic(protobute/3),assertion(M:protobute(M,At,_))).

atts_module(Var,M):- get_attr(Var,M,Was)->assertion(is_list(Was));put_attr(Var,M,[]).

atts_tmpl(At,Tmpl):-functor(At,F,A),functor(Tmpl,F,A).

atts_modulize([], _) --> [].
atts_modulize([G|Gs], M) --> !,
    atts_modulize(G, M),
    atts_modulize(Gs, M).
atts_modulize(G,M)-->
 {strip_module(G,_,GS),
     (G == GS -> MG = M:G ; MG = G)},
 [MG]. 



attrs_to_atts([])--> [].
attrs_to_atts(att(M,Att,Rest))-->
   atts_modulize(Att,M),
   attrs_to_atts(Rest).


% Should 'user' use the import_module/2 Dag? (curretly will just return all)
get_atts(Var,user,Atts):-var(Atts),!,get_attrs(Var,Attr),attrs_to_atts(Attr,Atts,[]).
% get_atts(Var,M,At):-var(At),!,get_attr(Var,M,At).
get_atts(Var,M,List):-is_list(List),!,maplist(get_atts(Var,M),List).
get_atts(Var,M,+At):- !,get_atts(M,Var,At).
get_atts(Var,_,-(M:At)):- !,get_atts(Var,M,-At).
get_atts(Var,_, (M:At)):- !,get_atts(Var,M,At).
%get_atts(Var,_,-(M:At)):- \+ meta_handler_name(M), !,get_atts(Var,M,-At).
%get_atts(Var,_, (M:At)):- \+ meta_handler_name(M), !,get_atts(Var,M,At).
get_atts(Var,M, - Pair):-!,
  atts_to_att(Pair,At),
   atts_exist(M,At),
   (get_attr(Var,M,Cur)->
      \+ memberchk(At,Cur) ;
    true).
get_atts(Var,M,Pair):-
   atts_to_att(Pair,At),
   atts_exist(M,At),
   (get_attr(Var,M,Cur)->
      memberchk(At,Cur) ;
    fail).


put_atts(_,M,At):-var(At),!,throw(error(instantiation_error,put_atts(M:At))).
put_atts(Var,M,List):-is_list(List),!,atts_module(Var,M),maplist(put_atts(Var,M),List).
put_atts(Var,M,+At):- !,put_atts(Var,M,At).
put_atts(Var,_,-(M:At)):- !,put_atts(Var,M,-At).
put_atts(Var,_, (M:At)):- !,put_atts(Var,M,At).

put_atts(Var,M,-Pair):-!,
  atts_to_att(Pair,Tmpl),
   atts_exist(M,Tmpl),
   (get_attr(Var,M,Cur)->
     (delete(Cur,Tmpl,Upd),put_attr(Var,M,Upd)) ;
    true).
put_atts(Var,M,Pair):-
   atts_to_att(Pair,At),
   atts_exist(M,At),
   (get_attr(Var,M,Cur) ->
    (atts_tmpl(At,Tmpl),
     delete(Cur,Tmpl,Mid), % ord_del_element wont work here because -a(_) stops short of finding a(1).
     ord_add_element(Mid,At,Upd),
     put_attr(Var,M,Upd));
    put_attr(Var,M,[At])).

attsep('=').
attsep(':').
attsep('-').

atts_to_att(Var,Var):-var(Var),!.
atts_to_att(N-V,Tmpl):-!,atts_to_att(N=V,Tmpl).
atts_to_att(N:V,Tmpl):-!,atts_to_att(N=V,Tmpl).
atts_to_att(N=V,Tmpl):-!,assertion(atom(N)),!,Tmpl=..[N,V].
atts_to_att(F/A,Tmpl):-!,assertion((atom(F),integer(A))),functor(Tmpl,F,A).
atts_to_att(Tmpl,Tmpl).



% This type-checking predicate succeeds iff its argument is an ordinary free variable, it fails if it is an attributed variable.
eclipse:free(X):-var(X),\+attvar(X).

% This type-checking predicate succeeds iff its argument is an attributed variable. For other type testing predicates an attributed variable behaves like a variable.
eclipse:meta(X):- attvar(X).

% A new attribute can be added to a variable using the tool predicate
% add_attribute(Var, Attr).
% An attribute whose name is not the current module name can be added using add_attribute/3 which is its tool body predicate (exported in sepia_kernel). If Var is a free variable, it will be bound to a new attributed variable whose attribute corresponding to the current module is Attr and all its other attributes are free variables. If Var is already an attributed variable and its attribute is uninstantiated, it will b
:- meta_predicate(add_attribute(+,:)).
add_attribute(Var, M:Attr):- put_atts(Var,M, Attr).
add_attribute(Var,M,Attr):- put_atts(Var,M, Attr).

:- meta_predicate(get_attribute(+,:)).
get_attribute(Var, M:Attr):- get_atts(Var,M, Attr).
get_attribute(Var, M, Attr):- get_atts(Var,M, Attr).



/*

where Attr is the value obtained from the handler. If there are several handled attributes, all attributes are qualified like in
X{a:A, b:B, c:C}.
pl_notrace(_)
*/

set_dict_attvar_reader(X):- set_prolog_flag(set_dict_attvar_reader,X).

attvar_to_dict(AttVar,Dict):-
   get_attrs(AttVar,Att3s),
   attrs_to_pairs(Att3s,DictPairs),
   dict_create(Dict,AttVar,DictPairs).

attrs_to_pairs(att(N,V,Att3s),[N=V|DictPairs]):-!,attrs_to_pairs(Att3s,DictPairs).
attrs_to_pairs(DictPairs,DictPairs).

% dict_to_attvar(Dict):- dict_to_attvar(Dict,_),!.

:- meta_predicate(dict_to_attvar(:,?)).
 
 
dict_to_attvar(Mod:Dict,Out):-!,
  dict_to_attvar(Mod,Dict,Out). 
dict_to_attvar(Dict,Out):-
  '$current_source_module'(Mod),
  dict_to_attvar(Mod,Dict,Out).

dict_to_attvar(_,Dict,Out):- \+ compound(Dict),!,Out=Dict.
dict_to_attvar(Mod,Dict,Out):- 
   is_dict(Dict),dict_pairs(Dict,M,Pairs),
   (atom(M)->put_atts(Out,M,Pairs);
   (var(M)-> (M=Out,put_atts(Out,Mod:Pairs)))),!.
dict_to_attvar(Mod,Dict,Out):- 
  compound_name_arguments(Dict,F,Args),
   maplist(dict_to_attvar(Mod),Args,ArgsO),!,
   compound_name_arguments(Out,F,ArgsO).



:- module_transparent(system:term_expansion/2).
:- module_transparent(system:goal_expansion/2).


system:term_expansion(Dict,X):- current_prolog_flag(set_dict_attvar_reader,true),'$current_source_module'(M),dict_to_attvar(M,Dict,X).

system:goal_expansion(Dict,X):- current_prolog_flag(set_dict_attvar_reader,true),'$current_source_module'(M),dict_to_attvar(M,Dict,X).

:- set_prolog_flag(atts_declared,auto).
% :- set_dict_attvar_reader(true).


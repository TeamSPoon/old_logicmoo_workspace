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

:- module(multivar,
 [
  /* mdwq/1, 
		  plvar/1,
          nb_var/1, nb_var/2,
          vdict/1, vdict/2,
		  un_mv/1, un_mv1/1,
		  mv_peek/2,mv_peek1/2,
		  mv_set/2,mv_set1/2,
		  mv_add1/2,mv_allow/2,
		  ic_text/1,

   is_mv/1, multivar/1 % create special varaible that cannot be bound
   */
   ]).

:- meta_predicate user:attvar_variant(0,0).
:- use_module(library(option),[dict_options/2,option/2]).

mdwq(Q):- format(user_error,'~NMWQ: ~q~n',[Q]).

:- meta_predicate mdwq_call(:).
mdwq_call(Q):- !, call(Q).
mdwq_call(Q):- call(Q) *-> mdwq(success:Q); (mdwq(failed:Q),!,fail).
:- export(mdwq_call/1).

% ==========================================
%  Unify hook
% ==========================================

'unify':attr_unify_hook(_,_).  % OR tracing with 'unify':attr_unify_hook(N,Var):- mdwq(meta_unify_hook(N,Var)).

user:meta_unify(Var,Rest,Val):-
    mdwq_call('$attvar':call_all_attr_uhooks(Rest, Val)),
	mv_add1(Var,Val).

multivar(Var):- put_attr(Var,unify,Var).
is_mv(Var):- attvar(Var),get_attr(Var,unify,Var).


% ==========================================
% Variant override TODO
% ==========================================

'variant':attr_unify_hook(_,_).
user:attvar_variant(N,Var):- (N==Var -> true ;  mdwq_call( \+ \+ =(N,Var) )).


% ==========================================
% Sets values
% ==========================================

'$value':attr_unify_hook(_,_).
mv_set(Var,Values):- put_attr(Var,'$value',v(Var,Values)).
mv_set1(Var,Val):- put_attr(Var,'$value',v(Var,[Val])).
mv_add1(Var,NewValue):-mv_prepend1(Var,'$value',NewValue).

mv_prepend1(Var,Mod,Val):- get_attr(Var,Mod,v(Var,Was))->(prepend_val(Val,Was,New)->put_attr(Var,Mod,v(Var,New)));put_attr(Var,Mod,v(Var,[Val])).

prepend_val(Val,[],[Val]).
prepend_val(Val,Was,[Val|NewList]):-delete_identical(Was,Val,NewList).

delete_identical([],_,[]).
delete_identical([Elem0|NewList],Elem1,NewList):-Elem1==Elem0,!.
delete_identical([ElemKeep|List],Elem1,[ElemKeep|NewList]):-delete_identical(List,Elem1,NewList).

% faster than mv_prepend1 - might use?
mv_prepend(Var,Mod,Val):- get_attr(Var,Mod,v(Var,Was))->put_attr(Var,Mod,v(Var,[Val|Was]));put_attr(Var,Mod,v(Var,[Val])).

% ==========================================
% Peeks values
% ==========================================

mv_peek(Var,Val):- mv_get_attr(Var,'$value',Val).
mv_peek1(Var,Val):- mv_peek(Var,Val),!.



% ==========================================
% Peeks any
% ==========================================

mv_get_attr(Var,Mod,Val):- get_attr(Var,Mod,v(_,Values)),member(Val,Values).
mv_get_attr1(Var,Mod,Val):- mv_get_attr(Var,Mod,Val),!.


% ==========================================
% Allow values
% ==========================================

mv_allow(Var,Allow):-mv_prepend(Var,'$allow',Allow).
'$allow':attr_unify_hook(v(Var,Allow),Val):- memberchk(Val,Allow)->true;get_attr(Var,ic_text,_).

% ==========================================
% Label values
% ==========================================

un_mv(Var):-del_attr(Var,unify),mv_peek(Var,Val)*->Var=Val;true.
un_mv1(Var):-del_attr(Var,unify),ignore(mv_peek1(Var,Var)).


% ==========================================
% Examples
% ==========================================


% ?- multivar(X),X=1,X=2,un_mv(X),writeq(X).
% ?- multivar(X),mv_allow(X,hello),mv_allow(X,hi), X=hello,X=hi,writeq(X).
% ?- multivar(X),mv_allow(X,hello),mv_allow(X,hi),X=hello,X=hi,X=hello,un_mv(X).
% ?- multivar(X),mv_allow(X,hello),mv_allow(X,hi),X=hello,X=hi,X=hello,!,un_mv(X)
% ?- multivar(X),mv_allow(X,One),X=good,!,un_mv(X).
% ?- \+ (multivar(X),mv_allow(X,One),X=good,X=bad,un_mv(X)).


% ?- \+ (ic_text(X),X="GOOD",X=good,X=one).
% ?- ic_text(X),X=good,X=gooD,un_mv(X).
% ?- ic_text(X),X="GOOD",X=good.
% ?- ic_text(X),mv_allow(X,"GOOD"),mv_allow(X,"BAD"),X=good,X=baD.
% ?- \+ (ic_text(X),mv_allow(X,"GOOD"),mv_allow(X,"BAD"),X=good,X=one).

% ==========================================
% Prolog-Like vars
% ==========================================
plvar(Var):- multivar(Var), put_attr(Var,plvar,Var).
plvar:attr_unify_hook(Var,Val):- mv_peek1(Var,Was)->Val=Was;mv_set1(Var,Val).


% Maybe Variables entering the clause database
:- meta_predicate multivar_call(1,0).
multivar_call(Type,Goal):-term_variables(Goal,Vars),maplist(Type,Vars),call(Goal).


% ==========================================
% Symbol-Like Global vars
% ==========================================
nb_var(Var):- gensym(nb_var_,Symbol),nb_var(Symbol, Var).
nb_var(Symbol, Var):- multivar(Var), put_attr(Var,nb_var,v(Var,Symbol)), nb_linkval(Symbol,Var).

% This should pretend to be be value1 slot instead
% so that we extext mv_peek1/2 and mv_set1/2
% to stroe things in GVAR in the case of a nb_var
nb_var:attr_unify_hook(v(_Var,Symbol),Val):-
       nb_getval(Symbol,Prev),
       ( % This is how we produce a binding for +multivar "iterator"
          (var(Val),nonvar(Prev)) ->  Val=Prev;
         % same binding (effectively)
             Val==Prev->true;
         % On unification we will update the internal '$value'
             Val=Prev->nb_setval(Symbol,Prev)).

% ==========================================
% Hashmap-Like vars
% ==========================================


vdict(Var):- multivar(Var), put_attr(Var,vdict,Var).
vdict(Val,Var):- vdict(Var),Var=Val.
vdict:attr_unify_hook(Var,OValue):- to_dict(OValue,Val)-> mv_peek(Var,Prev), merge_dicts(Prev,Val,Result)-> mv_set1(Var,Result).


to_dict(Val,Val):- is_dict(Val),!.
to_dict(OValue,Val):- is_list(OValue),!,dict_options(Val,OValue).
to_dict(OValue,Val):- compound(OValue),!,option(OValue,[MValue]),!,dict_options(Val,[MValue]).
to_dict(OValue,Val):- option('$value'=OValue,[MValue]),!,dict_options(Val,[MValue]).


merge_dicts(Val,Val,Val).
merge_dicts(Prev,Val,Prev):- Val :< Prev.
merge_dicts(Val,Prev,Prev):- Val :< Prev.
merge_dicts(Dict1,Dict2,Combined):- dicts_to_same_keys([Dict1,Dict2],dict_fill(_),[Combined,Combined]).



% ==========================================
% Insensitively cased text
% ==========================================

ic_text(Var):- multivar(Var),put_attr(Var,ic_text,Var).
ic_text:attr_unify_hook(Var,Val):- (mv_get_attr(Var,'$allow',One)*->ic_unify(One,Val); (mv_peek1(Var,One)->ic_unify(One,Val);true)).


ic_unify(One,Val):-term_upcase(One,UC1),term_upcase(Val,UC2),UC1==UC2.

term_upcase(Val,UC2):-catch(string_upper(Val,UC2),_,(format(string(UC1),'~w',Val),string_upper(UC1,UC2))).

:-
 source_location(S,_), prolog_load_context(module,LC),
 forall(source_file(M:H,S),
 (functor(H,F,A),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), 
  \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A])))),
    ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), 
  (current_predicate('system':F/A)->true; 'system':import(M:F/A))))))).



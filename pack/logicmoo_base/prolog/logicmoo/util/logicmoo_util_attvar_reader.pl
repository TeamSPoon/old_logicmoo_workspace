/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
:- module(logicmoo_util_attvar_reader, [
          deserialize_attvars/2,deserialize_attvars/3,
          serialize_attvars/2,
          put_dyn_attrs/2,
          ensure_named/3,
          read_attvars/1,read_attvars/0,
          mpred_get_attr/3,
          mpred_put_attr/3,
          mpred_get_attrs/2,
          mpred_put_attrs/2,
          install_attvar_expander/1,
          system_expanded_attvars/4,
          is_term_expanding_in_file/1,
          system_expanded_attvars/2]).


:- module_transparent((deserialize_attvars/2,deserialize_attvars/3,
          serialize_attvars/2,
          put_dyn_attrs/2,
          ensure_named/3,
          mpred_get_attr/3,
          mpred_put_attr/3,
          mpred_get_attrs/2,
          mpred_put_attrs/2,
          read_attvars/1,read_attvars/0,          
          install_attvar_expander/1,
          system_expanded_attvars/4,
          is_term_expanding_in_file/1,
          system_expanded_attvars/2)).


:- dynamic(lmcache:use_attvar_expander/1).
:- dynamic(lmcache:never_use_attvar_expander/1).

:- use_module(logicmoo_util_dmsg).

:- prolog_load_context(file,F),call(assert_if_new,baseKB:mpred_is_impl_file(F)).
:- current_prolog_flag(read_attvars,Was)->asserta(restore_attvar_reader(Was));asserta(restore_attvar_reader(false)).
:- set_prolog_flag(read_attvars,false).

% use this module to avoid some later conflicts
:- if(exists_source(library(base32))).
:- use_module(library(base32)).
:- endif.

mpred_get_attr(V,A,Value):- var(V),!,get_attr(V,A,Value),!.
mpred_get_attr('$VAR'(Name),_,_):- atom(Name),!,fail.
mpred_get_attr('$VAR'(Att3),A,Value):- put_attrs(NewVar,Att3),get_attr(NewVar,A,Value).
mpred_get_attr('avar'(Att3),A,Value):- put_attrs(NewVar,Att3),get_attr(NewVar,A,Value).
mpred_get_attr('avar'(_,Att3),A,Value):- put_attrs(NewVar,Att3),get_attr(NewVar,A,Value).
% mpred_get_attr(V,A,Value):- trace_or_throw(mpred_get_attr(V,A,Value)).

mpred_put_attrs(V,Att3s):- var(V),!,put_attrs(V,Att3s),!.
mpred_put_attrs(VAR,Att3s):- VAR='$VAR'(Name), atom(Name),!,setarg(1,VAR, att(vn, Name, Att3s)).
mpred_put_attrs(VAR,Att3s):- VAR='$VAR'(_Att3),!,setarg(1,VAR, Att3s).
mpred_put_attrs(VAR,Att3s):- VAR='avar'(_Att3),!,setarg(1,VAR, Att3s).
mpred_put_attrs(VAR,Att3s):- VAR='avar'(_,_),!,setarg(2,VAR, Att3s).
mpred_put_attrs(V,Att3s):- trace_or_throw(mpred_put_attrs(V,Att3s)).

mpred_get_attrs(V,Att3s):- var(V),!,get_attrs(V,Att3s),!.
mpred_get_attrs('$VAR'(Name),_):- atom(Name),!,fail.
mpred_get_attrs('$VAR'(Att3s),Att3):-!,Att3s=Att3.
mpred_get_attrs('avar'(Att3s),Att3):-!,Att3s=Att3.
mpred_get_attrs('avar'(_,Att3s),Att3):-!,Att3s=Att3.
% mpred_get_attrs(V,Value):- trace_or_throw(mpred_get_attrs(V,Value)).

mpred_put_attr(V,A,Value):- var(V),!,put_attr(V,A,Value),!.
mpred_put_attr(VAR,A,Value):- VAR='$VAR'(Name), atom(Name),!,setarg(1,VAR, att(vn, Name, att(A,Value, []))).
mpred_put_attr(VAR,A,Value):- VAR='$VAR'(Att3),!,setarg(1,VAR, att(A,Value,Att3)).
mpred_put_attr(VAR,A,Value):- VAR='avar'(Att3),!,setarg(1,VAR, att(A,Value,Att3)).
mpred_put_attr(VAR,A,Value):- VAR='avar'(_,Att3),!,setarg(2,VAR, att(A,Value,Att3)).
mpred_put_attr(V,A,Value):- trace_or_throw(mpred_put_attr(V,A,Value)).


ensure_named(Vs,V,N):- atom(N),member(N=VV,Vs),VV==V,put_attr(V,vn,N).
ensure_named(Vs,V,N):- atom(N),member(N=VV,Vs),put_attr(V,vn,N),!,maybe_must(VV= V).
ensure_named(Vs,V,N):- atom(N),put_attr(V,vn,N),set_in_vd(Vs,N=V).
ensure_named(Vs,V,N):- get_attr(V,vn,N),!,set_in_vd(Vs,N=V).
ensure_named(Vs,V,N):- member(N=NV,Vs),V==NV,!,put_attr(V,vn,N).
ensure_named(Vs,V,N):- get_varname_list(VsE), member(N=NV,VsE),V==NV,!,put_attr(V,vn,N),set_in_vd(Vs,N=V).

set_in_vd(Vs,N=V):-member(NN=VV,Vs),NN==N,V==VV,!.
set_in_vd(Vs,N=V):-member(NN=VV,Vs),NN==N,!,maybe_must(V=VV).
set_in_vd(Vs,N=V):-member(NN=VV,Vs),VV==V,maybe_must(N==NN).
set_in_vd(Vs,N=V):-Vs = [_|VT], nb_setarg(2,Vs,[N=V|VT]).

maybe_must(V=VV):-V==VV,!.
maybe_must(_).


% deserialize_attvars(V,O):- get_varname_list(Vs),!,loop_check(deserialize_attvars(['$variable_names'|Vs], V,O)),!.
deserialize_attvars(V,O):- loop_check(deserialize_attvars([localvs], V,O),V=O),!.

deserialize_attvars(_Vs, V,O):- cyclic_term(V),!,O=V.
deserialize_attvars(Vs, V,O):- nonvar(O),!,must(deserialize_attvars(Vs, V,M)),!,must(M=O).
deserialize_attvars(Vs, V,O):- var(V), get_attr(V,vn,N),set_in_vd(Vs,N=V),!,V=O.
deserialize_attvars(Vs, V,O):- var(V), member(N=VV,Vs),VV==V,put_attr(V,vn,N),!,V=O.
deserialize_attvars(_ ,IO,IO):- \+ compound(IO),!.

deserialize_attvars(_ ,(H:-BI),O):- fail,
  split_attrs(BI,AV,BO),AV\==true,AV\=bad:_,term_attvars((H:-BO),[]),
   must(call(AV)),!,(BO==true->(O=H);O=(H:-BO)).

deserialize_attvars(Vs,avar(S),V):- nonvar(S),!, show_call(put_dyn_attrs(V,S)),ensure_named(Vs,V,_).
deserialize_attvars(_ ,avar(V,_),V):- nonvar(V),!.
deserialize_attvars(Vs,avar(V,S),V):- var(V),nonvar(S),!, show_call(put_dyn_attrs(V,S)),ensure_named(Vs,V,_N).
deserialize_attvars(_ ,'$VAR'(N),'$VAR'(N)):- \+ atom(N),!.
deserialize_attvars(Vs,'$VAR'(N),V):- atom(N), member(N=V,Vs), ensure_named(Vs,V,N),!.
deserialize_attvars(Vs,C,A):- compound_name_arguments(C,F,Args),maplist(deserialize_attvars(Vs),Args,OArgs),compound_name_arguments(A,F,OArgs).

:- meta_predicate put_dyn_attrs(*,?).
put_dyn_attrs(V,S):- var(S),!,trace_or_throw(bad_put_dyn_attrs(V,S)),!.
put_dyn_attrs(V,S):-S=att(_,_,_), !, put_attrs(V,S).
put_dyn_attrs(V,M:AV):- atom(M),!, M:put_dyn_attrs(V,AV).
put_dyn_attrs(V,M=AV):- atom(M),!, ensure_attr_setup(M),!, must(put_attr(V,M,AV)).
put_dyn_attrs(_V,[]):- !.
put_dyn_attrs(V,List):- is_list(List),!, maplist(put_dyn_attrs(V),List),!.
put_dyn_attrs(V,[H|T]):- !, put_dyn_attrs(V,H),put_dyn_attrs(V,T),!.
put_dyn_attrs(_V,MAV):- must(MAV),!.

ensure_attr_setup(M):- atom(M),current_predicate(attribute_goals,M:attribute_goals(_,_,_)),!.
ensure_attr_setup(M):- atom(M),assert_if_new((M:attribute_goals(V,[put_attr(V,M,A)|R],R):- get_attr(V, M,A))).


read_attvars:- read_attvars(true).
read_attvars(TF):- 
  set_prolog_flag(read_attvars,TF),
  set_prolog_flag(assert_attvars,TF),
  prolog_load_context(module,M),
  install_attvar_expander(M).

is_term_expanding_in_file(I):- var(I),!,fail.
% is_term_expanding_in_file(_:-_):-!.
is_term_expanding_in_file(I):- source_file(_,_),nb_current('$source_term',CT),CT==I.

system_expanded_attvars(M:goal,_P,I,O):-
     \+ is_term_expanding_in_file(I),
    \+ lmcache:never_use_attvar_expander(M),
    prolog_load_context(module,LC),
    \+ lmcache:never_use_attvar_expander(LC),
    current_prolog_flag(read_attvars,true), 
    \+ current_prolog_flag(xref,true), 
     system_expanded_attvars(I,O),
     wdmsg(goal_xform(I --> O)).

system_expanded_attvars(M:term,_P,I,CO):- 
   is_term_expanding_in_file(I),
    \+ lmcache:never_use_attvar_expander(M),
    prolog_load_context(module,LC),
    \+ lmcache:never_use_attvar_expander(LC),
    current_prolog_flag(read_attvars,true), 
    \+ current_prolog_flag(xref,true), 
   system_expanded_attvars(I,O),
   clausify_attributes(O,CO),
   wdmsg(term_xform(I --> CO)),
   b_setval('$term',CO).

install_attvar_expander(M):-
  asserta(lmcache:use_attvar_expander(M)),
  system:multifile(M:term_expansion/4),
  system:module_transparent(M:term_expansion/4),
  system:dynamic(M:term_expansion/4),
  asserta_if_new(((M:term_expansion(I,P,O,P):- system_expanded_attvars(M:term,P,I,O)))),
  system:multifile(M:goal_expansion/4),
  system:module_transparent(M:goal_expansion/4),
  system:dynamic(M:goal_expansion/4),
  asserta_if_new(((M:goal_expansion(I,P,O,P):- system_expanded_attvars(M:goal,P,I,O)))).

uninstall_attvar_expander(M):-
  ignore(retract(lmcache:use_attvar_expander(M))),
  ignore(retract((M:goal_expansion(I,P,O,P):- system_expanded_attvars(M:goal,P,I,O)))),
  ignore(retract((M:term_expansion(I,P,O,P):- system_expanded_attvars(M:term,P,I,O)))).




%% serialize_attvars( +AttvarTerm, -PrintableTerm) is semidet.
%
% serialize attributed variables (this is for printing only currently)
%
serialize_attvars(I,O):- verbatum_term(I),!,O=I.
serialize_attvars(V,S):- var(V),must(serialize_1v(V,S)),!.
serialize_attvars(C,A):- compound_name_arguments(C,F,Args),maplist(serialize_attvars,Args,OArgs),compound_name_arguments(A,F,OArgs).

serialize_1v(V,'$VAR'(Name)):- get_attrs(V, att(vn, Name, [])),!.
serialize_1v(V,avar('$VAR'(N),SO)):- variable_name_or_ref(V,N),get_attrs(V, S),!,put_attrs(TEMP,S),del_attr(TEMP,vn),!,get_attrs(TEMP, SO),!.
serialize_1v(V,avar(S)):- get_attrs(V, S),!.
serialize_1v(V,'$VAR'(N)):-  variable_name_or_ref(V,N).
serialize_1v(V,V).


%% verbatum_term(TermT) is semidet.
%
% System Goal Expansion Sd.
%
verbatum_term(I):- attvar(I),!,fail.
verbatum_term(I):- \+ compound(I),!. % this is intended to include the non-attrbuted variables
verbatum_term('$VAR'(_)).
verbatum_term('avar'(_)).
verbatum_term('avar'(_,_)).
verbatum_term('$was_imported_kb_content$'(_,_)).
verbatum_term('varname_info'(_,_,_,_)).




%% system_expanded_attvars( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%
system_expanded_attvars(I,O):- (var(I);compound(I)),!,loop_check((deserialize_attvars(I,O))),O\=@=I,!.


:- retract(restore_attvar_reader(Was)),set_prolog_flag(read_attvars,Was).


end_of_file.
/*

% % :- '$set_source_module'( system).


:- public '$store_clause'/2.

'$store_clause'(A, C) :-
        '$clause_source'(A, B, D),
        '$store_clause'(B, _, C, D).

'$store_clause'((_, _), _, _, _) :- !,
        print_message(error, cannot_redefine_comma),
        fail.
'$store_clause'(A, _, B, C) :-
        '$valid_clause'(A), !,
        (   '$compilation_mode'(database)
        ->  '$record_clause'(A, B, C)
        ;   '$record_clause'(A, B, C, D),
            '$qlf_assert_clause'(D, development)
        ).

'$compile_term'(Clause, Layout, Id, SrcLoc) :-
	catch('$store_clause'(Clause, Layout, Id, SrcLoc), E,
	      '$print_message'(error, E)),
        catch((writeq(user_error,'$store_clause'(Clause,  Id)),nl(user_error)),_,true).

'$compile_aux_clauses'(Clauses, File) :-
	setup_call_cleanup(
	    '$start_aux'(File, Context),
	    '$store_aux_clauses'(Clauses, File),
	    '$end_aux'(File, Context)).

'$store_aux_clauses'(Clauses, File) :-
	is_list(Clauses), !,
	forall('$member'(C,Clauses),
	       '$compile_term'(C, _Layout, File)).
'$store_aux_clauses'(Clause, File) :-
	'$compile_term'(Clause, _Layout, File).
*/
/*
   compile_predicates(['$expand_goal'/2, '$expand_term'/4]),!.
:-   
   % '$set_predicate_attribute'('$expand_goal'(_,_), system, true),
   % '$set_predicate_attribute'('$expand_term'(_,_,_,_), system, true),
   '$set_predicate_attribute'('$expand_goal'(_,_), hide_childs, false),
   '$set_predicate_attribute'('$expand_term'(_,_,_,_), hide_childs, false).
   
*/



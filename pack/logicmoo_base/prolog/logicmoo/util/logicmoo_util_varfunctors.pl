:- module(logicmoo_util_varfunctors,
    [ expand_var_functors/5, set_functor_wrap/1, unset_functor_wrap/0 ]).


:- multifile((system:term_expansion/2,system:term_expansion/2)).
:- dynamic((system:term_expansion/2,system:term_expansion/2)).

:- dynamic((var_functor_quote/1,was_allow_variable_name_as_functor/1, var_functor_wrap/1)).

:- multifile(t_l:disable_px/0).
:- thread_local(t_l:disable_px/0).


compound_or_atom_name_arguments(In,Name,ArgsO):- compound(In),compound_name_arguments(In,Name,ArgsO).
compound_or_atom_name_arguments(In,Name,ArgsO):- fail,atom(In),Name=In,ArgsO=[].


expand_var_functors(T,VFE,Outer,In,Out):-   
   \+ compound(In)->In=Out;
  (compound_name_arguments(In,Name,Args),
   ((Args==[],\+ compound(In))->Out=Name;
      ((Name=VFE,Args=[JustOne] )-> (expand_var_functors(T,VFE,VFE,JustOne,VOut),((nonvar(VOut),functor(VOut,T,_))->Out=VOut;Out=..[VFE,VOut]));
      ( maplist(expand_var_functors(T,VFE,Name),Args,ArgsO),
      ((Name\='[|]',Outer=VFE,atom_codes(Name,[C|_]),code_type(C,prolog_var_start),
         (get_varname_list(Vs)->true;Vs=[]),(member(Name=Var,Vs)->true;put_variable_names( [Name=Var|Vs])))
           -> Out=..[T,Var|ArgsO];  (Args==ArgsO->(Out=In);compound_name_arguments(Out,Name,ArgsO))))))).


system:term_expansion(I,O):- var_functor_wrap(T),
          compound(I),functor(I,VFE,_), % var_functor_quote(VFE),
                     \+ t_l:disable_px,
                       must((w_tl(t_l:disable_px,expand_var_functors(T,VFE,(:-),I,O)))),I\=@=O.

system:goal_expansion(I,O):- var_functor_wrap(T),
          compound(I),functor(I,VFE,_), % var_functor_quote(VFE),
                     \+ t_l:disable_px,
                       must((expand_var_functors(T,VFE,(:-),I,O))),I\=@=O.

save_allow_variable_name_as_functor:- (was_allow_variable_name_as_functor(_)->true;current_prolog_flag(allow_variable_name_as_functor,Was),asserta(was_allow_variable_name_as_functor(Was))).
restore_allow_variable_name_as_functor:-current_prolog_flag(allow_variable_name_as_functor,Was),asserta(was_allow_variable_name_as_functor(Was)).

set_functor_wrap(T) :- save_allow_variable_name_as_functor, asserta(var_functor_wrap(T)),set_prolog_flag(allow_variable_name_as_functor,true).

unset_functor_wrap:- retract(var_functor_wrap(_)), (var_functor_wrap(_) -> true;restore_allow_variable_name_as_functor).

var_functor_quote('?').
var_functor_quote('&').
var_functor_quote('$').
% ttmricher requested these
var_functor_quote(A):-atom(A),atom_codes(A,[C]),C>255.
var_functor_quote('\2323\').

end_of_file.

/*
Old code


expand_var_functors(T,VFE,Outer,In,Out):-  
 var_functor_quote(VFE),
   \+ compound(In)->In=Out;
  (compound_name_arguments(In,Name,Args),
   (Args==[]->Out=Name;
      ((Name=VFE,Args=[JustOne] )-> (expand_var_functors(T,VFE,VFE,JustOne,VOut),(functor(VOut,t,_)->Out=VOut;Out=..[VFE,VOut]));
      ( maplist(expand_var_functors(T,VFE,Name),Args,ArgsO),
      ((Name\='[|]',Outer=VFE,atom_codes(Name,[C|_]),code_type(C,prolog_var_start),
         (get_varname_list(Vs)->true;Vs=[]),(member(Name=Var,Vs)->true;put_variable_names( [Name=Var|Vs])))
           -> Out=..[t,Var|ArgsO];  (Args==ArgsO->(Out=In);compound_name_arguments(Out,Name,ArgsO))))))).



system:term_expansion(I,O):- current_prolog_flag(allow_variable_name_as_functor,true),
          compound(I),functor(I,VFE,1),var_functor_quote(VFE),
                     \+ t_l:disable_px,
                       w_tl(t_l:disable_px,expand_var_functors(T,VFE,(:-),I,O)),I\=@=O.

system:goal_expansion(I,O):- current_prolog_flag(allow_variable_name_as_functor,true),
     compound(I),functor(I,VFE,1),var_functor_quote(VFE),
                     \+ t_l:disable_px,
                       expand_var_functors(T,VFE,(:-),I,O),I\=@=O.





*/


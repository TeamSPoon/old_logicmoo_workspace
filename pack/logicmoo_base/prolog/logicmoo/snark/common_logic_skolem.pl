/* <module> mpred_clausify
% Provides a prolog database replacement that uses an interpretation of KIF
%
%  t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(common_logic_skolem,
	  [ form_sk/2,
	    sk_form/2,
            push_skolem/2,push_skolem/3,
            push_dom/2,annote/4,
            annote/3,
            skolem_unify/2,
            with_no_kif_var_coroutines/1
	  ]).


:- module_transparent with_no_kif_var_coroutines/1.

with_no_kif_var_coroutines(Goal):- w_tl(t_l:no_kif_var_coroutines,Goal).

/** <module> Form Prolog sks (debugging)
*/

%%	form_sk(+Var, +Skolem) is det.
%
%	Assign a Skolem to a Var. Succeeds   silently if Sk is not a
%	sk (anymore).


form_sk(OtherValue, Skolem):- sk:attr_unify_hook(Skolem, OtherValue),!.
% form_sk(OtherValue, Skolem):- nonvar(OtherVAlue).


push_dom(_,_):- \+ is_skolem_setting(push_skolem),!.
push_dom(X,Form2):-annote(dom, X,Form2,_Merged).

annote(Dom,X,Form2):-annote(Dom,X,Form2,_).

annote(_,X,IO,IO):- nonvar(X),!.
annote(_,_,IO,IO):- \+ is_skolem_setting(push_skolem),!.
annote(_,X,Form2,Form2):- var(X), freeze(X,(ground(X)->show_call(for(X),lazy(call_u(Form2)));true)).
annote(Dom,X,Form2,SK_FINAL):-get_attr(X,Dom,Form1),merge_forms(Form1,Form2,SK_FINAL),put_attr(X,Dom,SK_FINAL),!.
annote(Dom,X,Form2,Form2):- put_attr(X,Dom,Form2).


%%	sk_form(+Sk, -Form) is semidet.
%
%	True if Sk has been assigned Form or is a Free variable.

sk_form(Sk, Form) :- get_attr(Sk, sk, Form),!.
sk_form(Var,Form):- var(Var),!,gensym(sk_other_,Form),put_attr(Var, sk, Form).
sk_form(sk(Value),Value):-!.


push_skolem(Onto,SK_ADD):-push_skolem(Onto,SK_ADD,_Merged).

push_skolem(Onto,SK_ADD,SK_FINAL):- get_attr(Onto,sk,SLPREV),!,merge_forms(SLPREV,SK_ADD,SK_FINAL),sk_replace(Onto,SK_FINAL),!.
push_skolem(Onto,SK_ADD,SK_FINAL):- var(Onto),!,SK_FINAL=SK_ADD,sk_replace(Onto,SK_FINAL),!.
push_skolem(Onto,SK_ADD,SK_FINAL):- sk_form(Onto,SLPREV),!,merge_forms(SLPREV,SK_ADD,SK_FINAL),sk_replace(Onto,SK_FINAL),!.
push_skolem(Onto,SK_ADD,SK_ADD):- sk_replace(Onto,SK_ADD).

sk_replace(Onto,SK_FINAL):-var(Onto),!,put_attr(Onto,sk,SK_FINAL).
sk_replace(_Into,_SKFINAL):-!,fail.


sk:attr_unify_hook(Form, OtherValue):-OtherValue==Form,!.
sk:attr_unify_hook(_Form, _OtherValue):- t_l:no_kif_var_coroutines,!,fail.
sk:attr_unify_hook(Form, OtherValue):- var(OtherValue),!,push_skolem(OtherValue,Form),!.
%sk:attr_unify_hook(Form, OtherValue):- contains_var(OtherValue,Form),!.
%sk:attr_unify_hook(Form, OtherValue):- contains_var(Form,OtherValue),!.
% sk:attr_unify_hook(Form, OtherValue):- skolem_unify(OtherValue,Form).

sk:attr_portray_hook(Form, SkVar) :- writeq(sk(SkVar,Form)).

sk:project_attributes(QueryVars, ResidualVars):- nop(wdmsg(sk:proj_attrs(skolem,QueryVars, ResidualVars))).

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
user:portray(Sk) :- get_attr(Sk, sk, Form), !, printable_variable_name(Sk,Name), format('sk_avar(~w,~q)',[Name,Form]).

%% sk_form:attribute_goals(@V)// is det.
%	copy_term/3, which also determines  the   toplevel  printing  of
%	residual constraints.
sk:attribute_goals(Sk,[form_sk(Sk,Form)|B],B) :- sk_form(Sk, Form).

skolem_test(_):- !.
skolem_test(Form):- show_call(call_u(Form)).

skolem_unify(_Var,Form):- skolem_test(Form).

merge_forms(A,B,A):- A==B,!.
merge_forms(A,B,B):- member(A,B),!.
merge_forms(A,B,A):- member(B,A),!.
merge_forms(A,B,A):- A=B,!,wdmsg(seeeeeeeeeeeee_merge_forms(A,B)),!.
merge_forms(A,B,C):- flatten([A,B],AB),list_to_set(AB,C).


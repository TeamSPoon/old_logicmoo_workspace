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

%%	form_sk(+Sk, +Form) is det.
%
%	Assign a form to a var. Succeeds   silently if Sk is not a
%	sk (anymore).

form_sk(Sk, Form) :-
	var(Sk), !,
	put_attr(Sk, sk, Form).
form_sk(_, _).


push_skolem(X,Form2):-push_skolem(X,Form2,_Merged).
push_skolem(X,Form2,Merged):-get_attr(X,sk,Form1),merge_forms(Form1,Form2,Merged),put_attr(X,sk,Merged),!.
push_skolem(X,Form2,Form2):-put_attr(X,sk,Form2).

push_dom(_,_):- \+ is_skolem_setting(push_skolem),!.
push_dom(X,Form2):-annote(dom, X,Form2,_Merged).

annote(Dom,X,Form2):-annote(Dom,X,Form2,_).

annote(_,X,IO,IO):- nonvar(X),!.
annote(_,_,IO,IO):- \+ is_skolem_setting(push_skolem),!.
annote(_,X,Form2,Form2):- var(X), freeze(X,(ground(X)->show_call(for(X),lazy(call_u(Form2)));true)).
annote(Dom,X,Form2,Merged):-get_attr(X,Dom,Form1),merge_forms(Form1,Form2,Merged),put_attr(X,Dom,Merged),!.
annote(Dom,X,Form2,Form2):- put_attr(X,Dom,Form2).


%%	sk_form(+Sk, -Form) is semidet.
%
%	True if Sk has been assigned Form.

sk_form(Sk, Form) :- get_attr(Sk, sk, Form).

sk:attr_unify_hook(Form, OtherValue):-OtherValue==Form,!.
sk:attr_unify_hook(_Form, _OtherValue):- t_l:no_kif_var_coroutines,!,fail.

% BEST 
sk:attr_unify_hook(Form, OtherValue):- var(OtherValue),!,push_skolem(OtherValue,Form).
%sk:attr_unify_hook(Form, OtherValue):- get_attr(OtherValue, sk, Form2),Form2=@=Form,!.
%sk:attr_unify_hook(Form, OtherValue):- get_attr(OtherValue, sk, Form2),merge_forms(Form,Form2,Merged),put_attr(OtherValue,sk,Merged).
%sk:attr_unify_hook(Form, Var):- var(Var),!, put_attr(Var, sk, Form).
sk:attr_unify_hook(Form, sk(Form2)):- !, merge_forms(Form,Form2,Merged),skolem_test(Merged).
sk:attr_unify_hook(Form, OtherValue):- skolem_unify(OtherValue,Form).

sk:attr_portray_hook(Form, SkVar) :- writeq(sk(SkVar,Form)).

sk:project_attributes(QueryVars, ResidualVars):- nop(wdmsg(sk:proj_attrs(skolem,QueryVars, ResidualVars))).

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
user:portray(Sk) :- get_attr(Sk, sk, Form), !, printable_variable_name(Sk,Name), format('sk_avar(~w,~q)',[Name,Form]).

%% sk_form:attribute_goals(@V)// is det.
%	copy_term/3, which also determines  the   toplevel  printing  of
%	residual constraints.

sk:attribute_goals(Sk) --> {sk_form(Sk, Form)},[form_sk(Sk,Form)].

skolem_test(_):- !.
skolem_test(Form):- show_call(call_u(Form)).

skolem_unify(_Var,Form):- skolem_test(Form).

merge_forms(A,B,A):- A==B,!.
merge_forms(A,B,B):- member(A,B),!.
merge_forms(A,B,A):- member(B,A),!.
merge_forms(A,B,A):- A=B,!,wdmsg(seeeeeeeeeeeee_merge_forms(A,B)),!.
merge_forms(A,B,C):- flatten([A,B],AB),list_to_set(AB,C).


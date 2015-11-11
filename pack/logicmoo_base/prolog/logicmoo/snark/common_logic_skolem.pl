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


%%	sk_form(+Sk, -Form) is semidet.
%
%	True if Sk has been assigned Form.

sk_form(Sk, Form) :- get_attr(Sk, sk, Form).

sk:attr_unify_hook(Form, OtherValue):-OtherValue==Form,!.
sk:attr_unify_hook(_Form, _OtherValue):- t_l:no_kif_var_coroutines,!,fail.

sk:attr_unify_hook(Form, OtherValue):- get_attr(OtherValue, sk, Form2),Form2=@=Form,!.
sk:attr_unify_hook(Form, OtherValue):- get_attr(OtherValue, sk, Form2),merge_forms(Form,Form2,Merged),put_attr(OtherValue,sk,Merged).
sk:attr_unify_hook(Form, Var):- var(Var),!, put_attr(Var, sk, Form).
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
skolem_test(Form):- show_call(req(Form)).
skolem_unify(_Var,Form):- skolem_test(Form).

merge_forms(A,B,A):- A==B,!,wdmsg(qqqqqqqqqqqqqqqseeeeeeeeeeeee_merge_forms(A)),!.
merge_forms(A,B,A):- A==B,!,wdmsg(qqqqqqqqqqqqqqqseeeeeeeeeeeee_merge_forms(A)),!.
merge_forms(A,B,A):- A=B,!,wdmsg(seeeeeeeeeeeee_merge_forms(A,B)),!.
merge_forms(A,B,(A,B)):- wdmsg(sksksksskskskssksksksskskskssksksks_merge_forms(A,B)),!.

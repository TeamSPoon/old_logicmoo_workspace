/** <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- ensure_loaded(logicmoo_engine).
:- ensure_loaded(plarkc/logicmoo_i_cyc_kb).

:- ltkb1.
:- doall((filematch(logicmoo('plarkc/logicmoo_i_cyc_kb_tinykb.pl'),F),must(source_file(X,F)),predicate_property(X,dynamic),retract(X:-_))).
:- doall((filematch(logicmoo('plarkc/logicmoo_i_cyc_kb_tinykb.pl'),F),source_file(X,F),predicate_property(X,static),X\='$pldoc'(_G8428,_G8429,_G8430,_G8431),listing(X))).

:- file_begin(pfc).

:- must_det(argIsa(genlPreds,2,_)).

transfer_predicate(C,If,Q):-doall((clause(C,true,Ref),If,must(Q),must(erase(Ref)))).
transTiny(Template,If):-transfer_predicate(tinyK8(Template),If,once(pfc_add(Template))).

:- pfc_no_trace.

:- transTiny(tCol(X),ground(X)).
:- transTiny(ttFormatType(X),ground(X)).
:- transTiny(arity(X,Y),ground((X,Y))).
:- transTiny(Form,(ground(Form),functor(Form,F,1),F\==neg)).



:- transfer_predicate(tinyK8(Form), ( \+ contains_term('$VAR'(_),Form)) , pfc_add((Form))).

:- pfc_trace.

:- transfer_predicate(tinyK8(Form),writeq(Form),pfc_add(clif(Form))).

:- prolog.

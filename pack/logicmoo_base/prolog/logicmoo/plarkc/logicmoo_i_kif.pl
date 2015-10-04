/** <module> logicmoo_i_kif
% Provides a specific compilation API for KIF axioms
%

% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- user:ensure_loaded(logicmoo(mpred/logicmoo_i_wff)).
:- user:ensure_loaded(logicmoo_i_clausify).

kif_hook(0=>0).
kif_hook(0<=>0).
kif_hook((0 & 0)).
kif_hook((0 v 0)).
kif_hook(0 <- 0).
kif_hook(~(0)).
kif_hook(not(0)).
kif_hook(all(+,0)).
kif_hook(exists(+,0)).
kif_hook(if(0,0)).
kif_hook(iff(0,0)).
kif_hook(C):- non_compound(C),!,fail.
kif_hook(H:- _):- !,nonvar(H),!,kif_hook(H).


:- style_check(+singleton).

correct_arities(_,FmlO,FmlO):-leave_as_is(FmlO),!.
correct_arities([],Fml,Fml):-!.
correct_arities([H|B],Fml,FmlO):-!,correct_arities(H,Fml,FmlM),correct_arities(B,FmlM,FmlO).
correct_arities(_,Fml,Fml):- \+ compound(Fml),!.
correct_arities(H,Fml,FmlO):- Fml=..[H,A|ARGS], ARGS\=[_],
  (ARGS==[]-> correct_arities(H,A,FmlO);
       (correct_arities(H,A,CA),FmlM=..[H|ARGS],correct_arities(H,FmlM,FmlMC),FmlO=..[H,CA,FmlMC])),!.
correct_arities(H,Fml,FmlM):- Fml=..[F|ARGS],must_maplist(correct_arities(H),ARGS,ARGSM),FmlM =.. [F|ARGSM].


:- export(subsT_each/3).
subsT_each(In,[],In):- !.
subsT_each(In,[KV|TODO],Out):- !,get_kv(KV,X,Y),subst_except(In,X,Y,Mid),!,subsT_each(Mid,TODO,Out),!.

:- dynamic(mudEquals/2).
:- export(mudEquals/2).
mudEquals(X,Y):- X=Y.
:- dynamic(skolem/3).
:- export(skolem/3).
skolem(X,Y,_):- X=Y.

:- export(not_mudEquals/2).
:- dynamic(not_mudEquals/2).
not_mudEquals(X,Y):- X \= Y.

:- export(type_of_var/3).
type_of_var(Fml,Var,Type):- contains_type_lits(Fml,Var,Lits),!,(member(Type,Lits)*->true;Type='Unk').
:- style_check(+singleton).


to_dlog_ops([
       'theExists'='exists',
       'thereExists'='exists',
       'ex'='exists',
       'forAll'='all',
       'forall'='all',
       ';'='v',
       ','='&',
       '~'='not',
     '-'='not',      
     'neg'='not',
     'naf'='not',
     'and'='&',
      'or'='v',
      ':-'=':-',
 'implies'='=>',
   'if'='=>',
   'iff'='<=>',
 'implies_fc'='=>',
 'implies_bc'=':-',
   'equiv'='<=>',
      '=>'='=>',
     '<=>'='<=>']).

to_symlog_ops([
   ';'='v',
   ','='&',
   '=>'='=>',
   '<=>'='<=>',
   '~'='-',
   ':-'=':-']).

to_prolog_ops([
   'v'=';',
   '&'=',',   
   '=>'='=>',
   '<=>'='<=>',
   '-'='not',
   ':-'=':-']).


to_nonvars(_Type,IN,IN):- is_ftVar(IN),!.
to_nonvars(_,Fml,Fml):- leave_as_is(Fml),!.
to_nonvars(Type,IN,OUT):- is_list(IN),!,must_maplist(to_nonvars(Type),IN,OUT),!.
to_nonvars(Type,IN,OUT):- call(Type,IN,OUT),!.


convertAndCall(Type,Call):- fail,Call=..[F|IN],must_maplist(to_nonvars(Type),IN,OUT), IN \=@= OUT, !, must(apply(F,OUT)).
convertAndCall(_Type,Call):-call_last_is_var(Call).

as_dlog(Fml,Fml):- leave_as_is(Fml),!.
as_dlog(Fml,FmlO):- to_dlog_ops(OPS),subsT_each(Fml,OPS,FmlM),!,correct_arities(['v','&'],FmlM,FmlO).




as_symlog(Fml,Fml):- leave_as_is(Fml),!.
as_symlog(Fml,FmlO):- as_dlog(Fml,FmlM),to_symlog_ops(OPS),subsT_each(FmlM,OPS,FmlM),correct_arities(['v','&'],FmlM,FmlO).

:- dynamic(thglobal:as_prolog/2).
thglobal:as_prolog(Fml,Fml):- is_ftVar(Fml),!.
thglobal:as_prolog(Fml,FmlO):- as_symlog(Fml,FmlM),
  to_prolog_ops(OPS),subsT_each(FmlM,OPS,FmlO).




adjust_kif(KB,Kif,KifO):-must(adjust_kif0(KB,Kif,KifO)),!.

% Converts to syntax that NNF/DNF/CNF/removeQ like


adjust_kif0(KB,I,O):- as_dlog(I,M),I\=@=M,!,adjust_kif0(KB,M,O).
adjust_kif0(_,V,V):- is_ftVar(V),!.
adjust_kif0(_,A,A):- \+ compound(A),!.

adjust_kif0(KB,~(Kif),(KifO)):- !,adjust_kif0(KB,not(Kif),KifO).
adjust_kif0(KB,neg(Kif),(KifO)):- !,adjust_kif0(KB,not(Kif),KifO).
adjust_kif0(KB,\+(Kif),(KifO)):- !,adjust_kif0(KB,not(Kif),KifO).


adjust_kif0(KB,nesc(N,Kif),nesc(N,KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,poss(N,Kif),poss(N,KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,not(Kif),not(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,not(KB,Kif),not(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,t(Kif),t(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,poss(Kif),poss(b_d(KB,nesc,poss),KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,nesc(Kif),nesc(b_d(KB,nesc,poss),KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,exists(L,Expr),               ExprO):-L==[],!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,exists(V,Expr),               ExprO):-atom(V),svar_fixvarname(V,L),subst(Expr,V,'$VAR'(L),ExprM),!,adjust_kif0(KB,exists('$VAR'(L),ExprM),ExprO).
adjust_kif0(KB,exists([L|List],Expr),        ExprO):-is_list(List),!,adjust_kif0(KB,exists(L,exists(List,Expr)),ExprO).
adjust_kif0(KB,exists(L,Expr),               ExprO):- \+ contains_var(L,Expr),!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,exists(L,Expr),exists(L,ExprO)):-!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,all(L,Expr),               ExprO):-L==[],!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,all(V,Expr),               ExprO):-atom(V),svar_fixvarname(V,L),subst(Expr,V,'$VAR'(L),ExprM),!,adjust_kif0(KB,all('$VAR'(L),ExprM),ExprO).
adjust_kif0(KB,all([L|List],Expr),all(L,ExprO)):-is_list(List),!,adjust_kif0(KB,exists(List,Expr),ExprO).
adjust_kif0(KB,all(L,Expr),               ExprO):- \+ contains_var(L,Expr),!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,all(L,Expr),all(L,ExprO)):-!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,[L|Ist],ConjO):- is_list([L|Ist]),must_maplist(adjust_kif0(KB),[L|Ist],ConjO),!.
adjust_kif0(KB,'&'([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts('&',[L|Ist],Conj),adjust_kif0(KB,Conj,ConjO).
adjust_kif0(KB,'v'([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts('v',[L|Ist],Conj),adjust_kif0(KB,Conj,ConjO).
adjust_kif0(KB,(H:-[L|Ist]),(HH:-ConjO)):- adjust_kif(KB,H,HH),is_list([L|Ist]),adjust_kif0(KB,'&'([L|Ist]),ConjO).
adjust_kif0(KB,(H:-B),(HH:-ConjO)):- adjust_kif(KB,H,HH),adjust_kif(KB,B,ConjO),!.
adjust_kif0(_,A,A):-leave_as_is(A),!.
adjust_kif0(KB,Kif,KifO):- Kif=..[F|ARGS],adjust_kif0(KB,F,ARGS,KifO),!.
adjust_kif0(KB,PAB,PABO):- PAB=..[P|AB],must_maplist(adjust_kif0(KB),AB,ABO),PABO=..[P|ABO].

adjust_kif0(KB,call_builtin,ARGS,O):-!,PARGS=..ARGS,adjust_kif0(KB,PARGS,O),!.
adjust_kif0(KB,true_t,[F|LIST],O3):-atom(F),!,PARGS=..[F|LIST],adjust_kif0(KB,(PARGS),O3),!.
adjust_kif0(KB,not_true_t,[F|LIST],O3):-atom(F),!,PARGS=..[F|LIST],adjust_kif0(KB,not(PARGS),O3),!.
adjust_kif0(KB,not,[A],not(O)):-!,adjust_kif0(KB,A,O),!.
adjust_kif0(KB,not,[A],not(O)):-!,adjust_kif0(KB,A,O),!.
adjust_kif0(KB,possible_t,[A],O):-!,adjust_kif0(KB,poss(A),O),!.
adjust_kif0(KB,possible_t,ARGS,O):-!,PARGS=..ARGS,adjust_kif0(KB,poss(PARGS),O).
adjust_kif0(KB,asserted_t,[A],O):-!,adjust_kif0(KB,t(A),O),!.
adjust_kif0(KB,asserted_t,ARGS,O):-!,PARGS=..ARGS,adjust_kif0(KB,t(PARGS),O).
adjust_kif0(KB,true_t,ARGS,O):-PARGS=..ARGS,adjust_kif0(KB,PARGS,O),!.
adjust_kif0(KB,Not_P,ARGS,O):-atom_concat('not_',P,Not_P),!,PARGS=..[P|ARGS],adjust_kif0(KB,not(PARGS),O).
adjust_kif0(KB,Int_P,ARGS,O):-atom_concat('int_',P,Int_P),!,append(LARGS,[_, _, _, _, _, _, _ ],ARGS),
   PLARGS=..[P|LARGS],adjust_kif0(KB,PLARGS,O).
adjust_kif0(KB,P,ARGS,O):-atom_concat(_,'_t',P),!,append(LARGS,[_, _, _, _, _, _],ARGS),
   PARGS=..[P|LARGS],adjust_kif0(KB,PARGS,O).

adjust_kif0(KB,W,[P,A,R|GS],O):-is_wrapper_pred(W),PARGS=..[P,A,R|GS],adjust_kif0(KB,t(PARGS),O).
adjust_kif0(KB,F,ARGS,O):-KIF=..[F|ARGS],length(ARGS,L),L>2,adjust_kif0(KB,KIF,F,ARGS,Conj),KIF\=@=Conj,!,adjust_kif0(KB,Conj,O).
% adjust_kif0(KB,W,[A],O):-is_wrapper_pred(W),adjust_kif(KB,A,O),!.

adjust_kif0(KB,KIF,OP,ARGS,Conj):-must_maplist(adjust_kif(KB),ARGS,ABO),adjust_kif5(KB,KIF,OP,ABO,Conj).

adjust_kif5(_KB,_KIF,',',ARGS,Conj):- list_to_conjuncts('&',ARGS,Conj).
adjust_kif5(_,_,';',ARGS,Conj):-list_to_conjuncts('v',ARGS,Conj).
adjust_kif5(_,_,'&',ARGS,Conj):-list_to_conjuncts('&',ARGS,Conj).
adjust_kif5(_,_,'v',ARGS,Conj):-list_to_conjuncts('v',ARGS,Conj).


local_pterm_to_sterm(P,['$VAR'(S)]):- if_defined(logicmoo_i_sexp_reader:svar(P,S)),!.
local_pterm_to_sterm(P,['$VAR'(S)]):- if_defined(logicmoo_i_sexp_reader:lvar(P,S)),!.
local_pterm_to_sterm(P,[P]):- leave_as_is(P),!.
local_pterm_to_sterm((H:-P),(H:-S)):-!,local_pterm_to_sterm(P,S),!.
local_pterm_to_sterm((P=>Q),[implies,PP,=>,QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm((P<=>Q),[equiv,PP,QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(all(P,Q),[all(PP),QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(exists(P,Q),[ex(PP),QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(not(Q),[not,QQ]):-local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(poss(Q),[pos(QQ)]):-local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm('&'(P,Q),PPQQ):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),flatten([PP,QQ],PPQQ0),list_to_set(PPQQ0,PPQQ).
local_pterm_to_sterm(','(P,Q),PPQQ):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),flatten([PP,QQ],PPQQ0),list_to_set(PPQQ0,PPQQ).
local_pterm_to_sterm('v'(P,Q),[or,[PP],[QQ]]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),!.
local_pterm_to_sterm('beliefs'(P,Q),[beliefs(PP),QQ]):-local_pterm_to_sterm2(P,PP),local_pterm_to_sterm(Q,QQ),!.
local_pterm_to_sterm(P,S):-subst_except(P,'&',',',Q),P\=@=Q,!,local_pterm_to_sterm(Q,S),!.
local_pterm_to_sterm(P,S):-subst_except(P,'v',';',Q),P\=@=Q,!,local_pterm_to_sterm(Q,S),!.
local_pterm_to_sterm(P,[Q]):-P=..[F|ARGS],maplist(local_pterm_to_sterm2,ARGS,QARGS),Q=..[F|QARGS].
local_pterm_to_sterm(P,[P]).

local_pterm_to_sterm2(P,Q):-local_pterm_to_sterm(P,PP),([Q]=PP;Q=PP),!.






%======  make a sequence out of a disjunction =====
flatten_or_list(A,B,C):- convertAndCall(as_symlog,flatten_or_list(A,B,C)).
flatten_or_list(KB,v(X , Y), F):- !,
   flatten_or_list(KB,X,A),
   flatten_or_list(KB,Y,B),
   flatten([A,B],F).
flatten_or_list(_KB,X,[X]).



fmtl(X):- thglobal:as_prolog(X,XX), fmt(XX).

write_list([F|R]):- write(F), write('.'), nl, write_list(R).
write_list([]).

numbervars_with_names(Term,CTerm):- ground(Term),!,duplicate_term(Term,CTerm).
numbervars_with_names(Term,CTerm):- 
 must_det_l((
   source_variables_l(NamedVars),!,
   copy_term(Term:NamedVars,CTerm:CNamedVars),
   term_variables(CTerm,Vars),   
   get_var_names(Vars,CNamedVars,Names),
   b_implode_varnames0(Names),
   numbervars(CTerm,91,_,[attvar(skip),singletons(false)]),
   append(CNamedVars,NamedVars,NewCNamedVars),
   list_to_set(NewCNamedVars,NewCNamedVarsS),
   remove_grounds(NewCNamedVarsS,NewCNamedVarsSG),
   b_setval('$variable_names',NewCNamedVarsSG))),!.

numbervars_with_names(Term,CTerm):- 
 must_det_l((
   source_variables_l(NamedVars),!,
   copy_term(Term:NamedVars,CTerm:CNamedVars),
   term_variables(CTerm,Vars),   
   get_var_names(Vars,CNamedVars,Names),
   b_implode_varnames0(Names),
   numbervars(CTerm,91,_,[attvar(skip),singletons(false)]),
   append(CNamedVars,NamedVars,NewCNamedVars),
   list_to_set(NewCNamedVars,NewCNamedVarsS),
   remove_grounds(NewCNamedVarsS,NewCNamedVarsSG),
   b_setval('$variable_names',NewCNamedVarsSG))),!.

get_var_names([],_,[]).
get_var_names([V|Vars],NamedVars,[S|SNames]):-
    get_1_var_name(V,NamedVars,S),
    get_var_names(Vars,NamedVars,SNames).

get_1_var_name(_V,[],_S).

get_1_var_name(Var,NamedVars,Name):- compound(Var),arg(1,Var,NV),!,get_1_var_name(NV,NamedVars,Name).
get_1_var_name(Var,NamedVars,Var=NV):-atom(Var),NamedVars=[_|T],nb_setarg(2,NamedVars,[Var=NV|T]),!.
get_1_var_name(Var,[N=V|_NamedVars],Name=V):-
     (Var == V -> Name = N ; (Var==Name -> Name=Var ; fail )),!.     
get_1_var_name(Var,[_|NamedVars],Name):- get_1_var_name(Var,NamedVars,Name).


wdmsgl(CNF):- compound(CNF),CNF=..[NAME,NF],!,must(wdmsgl(NAME:-NF)).
wdmsgl(CNF):- wdmsg(CNF),!.
wdmsgl(NF):- must((get_functor(NF,NAME),!,must(wdmsgl_2(NAME,NF)))).


wdmsgl_2(NAME,NF):- functor(NF,_,_),wdmsgl_3(NAME,&,NF).

wdmsgl_3(NAME,F,NF):- 
   numbervars_with_names(vv(NAME,F,NF),vv(NAME2,F2,NF2)),
   wdmsgl_4(NAME2,F2,NF2).

%wdmsgl_4(NAME,F,NF):- is_list(NF),!,list_to_set(NF,NS),must_maplist(wdmsgl_4(NAME,F),NS).
%wdmsgl_4(NAME,F,NF):- compound(NF),NF=..[FF,A,B],FF=F,is_ftNonvar(A),is_ftNonvar(B),!,must_maplist(wdmsgl_4(NAME,F),[A,B]).
% wdmsgl_4(NAME,_,NF):- as_symlog(NF,NF2), with_all_dmsg(display_form(KB,NAME:-NF2)).
wdmsgl_4(NAME,_,NF):- as_symlog(NF,NF2), with_all_dmsg(display_form(_KB,(NAME:-NF2))).



fresh_varname(F,NewVar):-is_ftVar(F),NewVar=F.
fresh_varname(F,NewVar):-var(F),fresh_varname('mudEquals',NewVar).
fresh_varname([F0|_],NewVar):-!,fresh_varname(F0,NewVar).
fresh_varname(F,NewVar):- compound(F),arg(_,F,F1),atom(F1),!,functor(F,F0,_),atom_concat(F0,F1,FN),upcase_atom(FN,FUP),gensym(FUP,VARNAME),NewVar = '$VAR'(VARNAME),!.
fresh_varname(F,NewVar):- functor(F,FN,_),!, upcase_atom(FN,FUP),gensym(FUP,VARNAME),NewVar = '$VAR'(VARNAME),!.



% kif_to_boxlog('=>'(WffIn,enables(Rule)),'$VAR'('MT2'),complete,Out1), % kif_to_boxlog('=>'(enabled(Rule),WffIn),'$VAR'('KB'),complete,Out).  

kif_to_prolog(X,E):- kif_to_boxlog(X,Y),!,list_to_set(Y,S),!,member(E,S).

%====== kif_to_boxlog(+Wff,-NormalClauses):-
:- export(kif_to_boxlog/2).
% kif_to_boxlog(Wff,Out):- loop_check(kif_to_boxlog(Wff,Out),Out=looped_kb(Wff)).
kif_to_boxlog(Wff,Out):- why_to_id(rule,Wff,Why), kif_to_boxlog(Wff,Why,Out),!.
kif_to_boxlog(WffIn,Out):-  why_to_id(rule,WffIn,Why), kif_to_boxlog(all('$VAR'('KB'),'=>'(asserted_t('$VAR'('KB'),WffIn),WffIn)),'$VAR'('KB'),Why,Out).
kif_to_boxlog(WffIn,NormalClauses):- why_to_id(rule,WffIn,Why), kif_to_boxlog(WffIn,'$VAR'('KB'),Why,NormalClauses).

alt_kif_to_boxlog(not( Wff),KB,Why,Out):- !, kif_to_boxlog(not( Wff),KB,Why,Out).
alt_kif_to_boxlog(Wff,KB,Why,Out):- loop_check(kif_to_boxlog((not(nesc(not(Wff)))),KB,Why,Out),Out=looped_kb(Wff)).

:- export(kif_to_boxlog/3).
kif_to_boxlog(WffIn,Why,Out):-  kif_to_boxlog(WffIn,'$VAR'('KB'),Why,Out),!.


:- export(kif_to_boxlog/4).
kif_to_boxlog(I,KB,Why,Flattened):- atom(I),atom_contains(I,('(')),!,
  must_det_l((input_to_forms(atom(I),Wff,Vs),b_setval('$variable_names',Vs),!,sexpr_sterm_to_pterm(Wff,PTerm),PTerm\=[_|_],
  kif_to_boxlog(PTerm,KB,Why,Flattened))),!.

% kif_to_boxlog(WffInIn,KB,Why,FlattenedO):-  as_dlog(WffInIn,WffIn),kif_to_boxlog_0(WffIn,KB,Why,FlattenedO),!.

% kif_to_boxlog(Wff,KB,Why,Out):- loop_check(kif_to_boxlog(Wff,KB,Why,Out),alt_kif_to_boxlog(Wff,KB,Why,Out)),!.

%kif_to_boxlog((Wff:- B),KB,Why,Flattened):- is_true(B),!, kif_to_boxlog(Wff,KB,Why,Flattened),!.
%kif_to_boxlog(Wff,KB,Why,Out):- adjust_kif(KB,Wff,M),Wff \=@= M ,!,kif_to_boxlog(M,KB,Why,Out).

kif_to_boxlog(HB,KB,Why,FlattenedO):- compound(HB),HB=(HEAD:- BODY),!,
  must_det_l((
   check_is_kb(KB),
   conjuncts_to_list(HEAD,HEADL),conjuncts_to_list(BODY,BODYL),
   correct_boxlog([cl(HEADL,BODYL)],KB,Why,FlattenedO))).

kif_to_boxlog(WffIn0,KB0,Why0,FlattenedO):-  
  must_det_l((nl,nl,nl,draw_line,draw_line,draw_line,draw_line)),
  must_det_l((
    must(numbervars_with_names(WffIn0:KB0:Why0,WffIn:KB:Why)),      
   ensure_quantifiers(WffIn,Wff),
   wdmsgl(kif(Wff)),
   % KB = WffQ,
    check_is_kb(KB),
    must(dif(KB,Why)),
   %with_assertions(thlocal:dont_use_mudEquals,defunctionalize('=>',WffQ,Wff)),
   %(WffQ\==Wff-> dmsg(defunctionalize('=>',WffQ,Wff));wdmsgl(kif(Wff))),
   as_dlog(Wff,Wff666),
   % kb_nlit(KB,Neg),
   % original(Why)=>Wff666
   add_nesc(Wff666,Wff6667),
   add_preconds(Wff6667,Wff6668),
   adjust_kif(KB,Wff6668,Wff6669),
   wdmsgl(pkif(Wff6669)),
   nnf(KB,Wff6669,NNF),
   %wdmsgl(nnf(NNF)),
   pnf(KB,NNF,PNF),
   %wdmsgl(pnf(PNF)),
   %save_wid(Why,kif,Wff),
   %save_wid(Why,pkif,Wff6669),
   cf(Why,KB,PNF,NCFsI),!,
   cf_to_flattened_clauses(KB,Why,NCFsI,Flattened),
   list_to_set(Flattened,FlattenedM),!,
   correct_boxlog(FlattenedM,KB,Why,FlattenedO))).
   
thlocal:no_rewrites.


check_is_kb(KB):-ignore('$VAR'('KB')=KB).

add_preconds(X,X):- thlocal:no_rewrites,!.
add_preconds(X,Z):-
 with_assertions(leave_as_is0('CollectionS666666666666666ubsetFn'(_,_)),
   with_assertions(thlocal:dont_use_mudEquals,defunctionalize('=>',X,Y))),add_preconds2(Y,Z).

add_preconds2(Wff6667,PreCondPOS):-
   must_det_l((get_lits(Wff6667,PreCond),list_to_set(PreCond,PreCondS),
     add_poss_to(PreCondS,Wff6667, PreCondPOS))).

add_poss_to([],Wff6667, Wff6667).
add_poss_to([PreCond|S],Wff6667, PreCondPOS):-!,
 add_poss_to(PreCond,Wff6667, PreCondM),
 add_poss_to(S,PreCondM, PreCondPOS).
 
add_poss_to(PreCond,Wff6667, PreCond=>Wff6667):-prequent(PreCond).
add_poss_to(PreCond,Wff6667, Wff6667):-leave_as_is(PreCond).
add_poss_to(not(_PreCond),Wff6667, Wff6667).
add_poss_to(PreCond,Wff6667, (poss(PreCond)=>Wff6667)).


add_nesc(X,X):-!.

add_nesc(IN,OUT):-is_list(IN),must_maplist(add_nesc,IN,OUT),!.
add_nesc(Wff666,Wff666):-leave_as_is(Wff666),!.
add_nesc(P<=>Q,O):-!,add_nesc(((P=>Q) & (Q=>P)),O).
add_nesc(PQ,PQO):- PQ=..[F,V|Q],pttp_quantifier(F),add_nesc(Q,QQ),PQO=..[F,V|QQ],!.
add_nesc(IN,poss(IN)):-IN=..[F|_],should_be_poss(F),!.
add_nesc(Wff666,Wff666):-is_modal(Wff666,_),!.
add_nesc(P=>Q,((PP & P & QP) =>Q)):-  add_poss(P,PP),add_poss(Q,QP).
add_nesc(IN,OUT):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist(add_nesc,INL,OUTL),OUT=..[F|OUTL].
add_nesc(Wff666,Wff666):-!.

add_nesc(Q,(PQ & Q)):-  add_poss(Q,PQ),!.
add_nesc((P & Q),(PQ & (P & Q))):-  add_poss(P & Q,PQ),!.
add_nesc(Wff666,Wff666):-!.
add_nesc(IN,OUT):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist(add_nesc,INL,OUTL),OUT=..[F|OUTL].
add_nesc(not(IN),not(IN)).
add_nesc(IN,(IN)).
add_nesc(IN,nesc(IN)).


% add_poss(Wff666,Wff666):-!.
% add_poss(X,X):-!.
add_poss(PQ,PQ):- var(PQ),!.
add_poss(PQ,PQO):- PQ=..[F,V,Q],pttp_quantifier(F),add_poss(Q,QQ),PQO=..[F,V,QQ],!.
add_poss(Wff666,true):-leave_as_is(Wff666),!.
add_poss(not(IN),not(IN)).
add_poss(INL,OUTC):-is_list(INL),must_maplist(add_poss,INL,OUTL),F='&',OUT=..[F|OUTL],correct_arities(F,OUT,OUTC).
add_poss(IN,OUT):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist(add_poss,INL,OUTL),OUT=..[F|OUTL].
add_poss(IN,poss(IN)).


% shall X => can X
% shall ~ X => ~ can X
% ~ shall X => can ~ X
get_lits(PQ,[]):- var(PQ),!.
get_lits(PQ,QQ):- PQ=..[F,_Vs,Q],pttp_quantifier(F),get_lits(Q,QQ).
get_lits(Wff666,[Wff666]):-leave_as_is(Wff666),!.
get_lits(not(IN),NOUT):-get_lits(IN,OUT),must_maplist(simple_negate_literal(not),OUT,NOUT).
get_lits(knows(WHO,IN),NOUT):-get_lits(IN,OUT),must_maplist(simple_negate_literal(knows(WHO)),OUT,NOUT).
get_lits(beliefs(WHO,IN),NOUT):-get_lits(IN,OUT),must_maplist(simple_negate_literal(beliefs(WHO)),OUT,NOUT).
get_lits(IN,OUTLF):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist(get_lits,INL,OUTL),flatten(OUTL,OUTLF).
get_lits(IN,[IN]).

simple_negate_literal(F,FX,X):-FX=..FXL,F=..FL,append(FL,[X],FXL),!.
simple_negate_literal(F,X,FX):-append_term(F,X,FX).

pttp_quantifier(F):- pttp_nnf_pre_clean_functor(F,(all),[]);pttp_nnf_pre_clean_functor(F,(ex),[]).

should_be_poss(argInst).

:- dynamic(elInverse/2).

clauses_to_boxlog(KB,Why,In,Prolog):- clauses_to_boxlog_0(KB,Why,In,Prolog).


clauses_to_boxlog_0(KB,Why,In,Prolog):-loop_check(clauses_to_boxlog_1(KB,Why,In,Prolog),show_call((clauses_to_boxlog_5(KB,Why,In,Prolog)))),!.
clauses_to_boxlog_0(KB,Why,In,Prolog):-correct_cls(KB,In,Mid),!,clauses_to_boxlog_1(KB,Why,Mid,PrologM),!,Prolog=PrologM.

clauses_to_boxlog_1(KB, Why,In,Prolog):- clauses_to_boxlog_2(KB,Why,In,PrologM),!,must(Prolog=PrologM).

clauses_to_boxlog_2(KB, Why,In,Prolog):- is_list(In),!,must_maplist(clauses_to_boxlog_1(KB,Why),In,Prolog).
clauses_to_boxlog_2(KB, Why,cl([],BodyIn),  Prolog):- !, (is_lit_atom(BodyIn) -> clauses_to_boxlog_1(KB,Why,cl([inconsistentKB(KB)],BodyIn),Prolog);  (trace,kif_to_boxlog(not(BodyIn),KB,Why,Prolog))).
clauses_to_boxlog_2(KB, Why,cl([HeadIn],[]),Prolog):- !, (is_lit_atom(HeadIn) -> Prolog=HeadIn ; (kif_to_boxlog(HeadIn,KB,Why,Prolog))).
clauses_to_boxlog_2(KB,_Why,cl([HeadIn],BodyIn),(HeadIn:- BodyOut)):-!, must_maplist(logical_pos(KB),BodyIn,Body), list_to_conjuncts(Body,BodyOut),!.

clauses_to_boxlog_2(KB, Why,cl([H,Head|List],BodyIn),Prolog):- trace,
  findall(Answer,((member(E,[H,Head|List]),delete_eq([H,Head|List],E,RestHead), 
     must_maplist(logical_neg(KB),RestHead,RestHeadS),append(RestHeadS,BodyIn,Body),
       clauses_to_boxlog_1(KB,Why,cl([E],Body),Answer))),Prolog),!.

clauses_to_boxlog_2(_KB,_Why,(H:-B),(H:-B)):- trace,!.

clauses_to_boxlog_5(KB, Why,In,Prolog):- is_list(In),!,must_maplist(clauses_to_boxlog_5(KB,Why),In,Prolog).
clauses_to_boxlog_5(_KB,_Why,(H:-B),(H:-B)):-!.
clauses_to_boxlog_5(_KB,_Why,cl([HeadIn],[]),HeadIn):-!.
clauses_to_boxlog_5(_KB,_Why,In,Prolog):-trace,In=Prolog.

mpred_t_tell_kif(OP2,RULE):- 
 with_assertions(thlocal:current_pttp_db_oper(mud_call_store_op(OP2)),
   (show_call(call((must(kif_tell(RULE))))))).


fix_input_vars(AIn,A):- copy_term(AIn,A),numbervars(A,672,_).

%:- export(show_boxlog/1).
%assert_boxlog(AIn):- fix_input_vars(AIn,A), as_dlog(A,AA),kif_to_boxlog(AA,B),!,must_maplist(kif_tell_boxes_undef(How,Why),B),!,nl,nl.
%:- export(show_boxlog2/2).
%assert_boxlog2(AIn):- fix_input_vars(AIn,A), with_all_dmsg((kif_to_boxlog(A,B),!,must_maplist(kif_tell_boxes_undef(How,Why),B),!,nl,nl)).




boxlog_to_pfc(PFCM,PFC):- is_list(PFCM),must_maplist(boxlog_to_pfc,PFCM,PFC).
boxlog_to_pfc((A,B),C):- !, must_maplist(boxlog_to_pfc,[A,B],[AA,BB]),conjoin(AA,BB,C).
boxlog_to_pfc(PFCM,PFCO):- boxlog_to_compile(PFCM,PFC),!, subst(PFC,(not),(neg),PFCO).


%:- export(tsn/0).
tsn:- with_all_dmsg(forall(clause(kif,C),must(C))).

% kif:- make.
:- dynamic(kif_test_string/1).
tkif:- kif_test_string(TODO),kif_io(string(TODO),current_output).

:- multifile(user:sanity_test/0).
user:regression_test:- tsn.

:- thread_local(kif_action_mode/1).
:- asserta_if_new(kif_action_mode(tell)).

:- thread_local(kif_reader_mode/1).
:- asserta_if_new(kif_reader_mode(lisp)).

kif_read(InS,Wff,Vs):- must(l_open_input(InS,In)),
  must(((kif_reader_mode(lisp) ,without_must( catch(input_to_forms(In,Wff,Vs),E,(dmsg(E:kif_read_input_to_forms(In,Wff,Vs)),fail)))) *-> true ;
      catch(read_term(In,Wff,[module(user),double_quotes(string),variable_names(Vs)]),E,(dmsg(E:kif_read_term_to_forms(In,Wff,Vs)),fail)))).

%= ===== to test program =====-
:- ensure_loaded(library(logicmoo/plarkc/dbase_i_sexpr_reader)).

:- export(kif/0).
kif:- current_input(In),current_output(Out),!,kif_io(In,Out).

%open_input(InS,InS):- is_stream(InS),!.
%open_input(string(InS),In):- text_to_string(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In),!.


:- export(kif_io/2).
kif_io(InS,Out):- 
  l_open_input(InS,In),
   repeat,             
      debugOnError((once((kif_action_mode(Mode),write(Out,Mode),write(Out,'> '))),
        kif_read(In,Wff,Vs),
         b_setval('$variable_names', Vs),
           portray_clause(Out,Wff,[variable_names(Vs),quoted(true)]),
           once(kif_process(Wff)),
           Wff == end_of_file)),!.

:- export(id_to_why/3).
why_to_id(Term,Wff,IDWhy):- not(atom(Term)),term_to_atom(Term,Atom),!,why_to_id(Atom,Wff,IDWhy).
why_to_id(Atom,Wff,IDWhy):- wid(IDWhy,Atom,Wff),!.
why_to_id(Atom,Wff,IDWhy):- must(atomic(Atom)),gensym(Atom,IDWhyI),kb_incr(IDWhyI,IDWhy),assertz_if_new(user:wid(IDWhy,Atom,Wff)).

:- export(kif_process/1).
kif_process(end_of_file):- !.
kif_process(prolog):- prolog,!.
kif_process(Assert):- atom(Assert),retractall(kif_action_mode(_)),asserta(kif_action_mode(Assert)),fmtl(kif_action_mode(Assert)),!.
kif_process(Wff):- kif_action_mode(Mode),kif_process(Mode,Wff),!.

kif_process(_,':-'(Wff)):- !, kif_process(call,Wff).
kif_process(_,'?-'(Wff)):- !, kif_ask(Wff).
kif_process(_,'ask'(Wff)):- !, kif_ask(Wff).
kif_process(_,'tell'(Wff)):- !, kif_tell(Wff).
kif_process(call,Call):- !,call(Call).
kif_process(tell,Wff):- !, kif_tell(Wff).
kif_process(ask,Wff):- !, kif_ask(Wff).
kif_process(Other,Wff):- !, wdmsg(error(missing_kif_process(Other,Wff))),!,fail.

:- export(kif_ask_sent/1).
kif_ask_sent(Wff):- 
   why_to_id(ask,Wff,Why),
   term_variables(Wff,Vars),
   gensym(z_q,ZQ),
   Query=..[ZQ,666|Vars],
   why_to_id(rule,'=>'(Wff,Query),Why),   
   kif_to_boxlog('=>'(Wff,Query),Why,QueryAsserts),!,
   kif_tell_boxes1(Why,QueryAsserts),!,
   call_cleanup(
     kif_ask(Query),
     retractall_wid(Why)).


:- export(kif_ask/1).
kif_ask(P <=> Q):- kif_ask_sent(P <=> Q).
kif_ask(P => Q):- kif_ask_sent(P => Q).
kif_ask((P v Q)):- kif_ask_sent(((P v Q))).
kif_ask((P & Q)):- kif_ask_sent((P & Q)).
kif_ask(Goal0):-  logical_pos(_KB,Goal0,Goal),
    no_repeats(user:(
	add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],_ProofOut1,Goal1,_),!,
        search(Goal1,60,0,1,3,DepthIn,DepthOut))).

:- export(kif_ask/2).
kif_ask(Goal0,ProofOut):- logical_pos(_KB,Goal0,Goal),
    no_repeats(user:(
	add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),!,
        search(Goal1,60,0,1,3,DepthIn,DepthOut),
        contract_output_proof(ProofOut1,ProofOut))).

kif_tell(InS):- atom(InS),must_det_l((kif_read(string(InS),Wff,Vs),b_implode_varnames0(Vs),local_sterm_to_pterm(Wff,Wff0),kif_tell(Wff0))),!.
% kif_tell(WffIn):- must_det_l((numbervars_with_names(WffIn,Wff),why_to_id(tell,Wff,Why),kif_tell(Why,Wff))),!.
kif_tell(WffIn):- must_det_l((numbervars_with_names(WffIn,Wff),pfc_add(clif(Wff)))),!.


local_sterm_to_pterm(Wff,WffO):- sexpr_sterm_to_pterm(Wff,WffO),!.



:- op(1000,fy,(kif_tell)).

/*
:- export((kif_tell)/2).

kif_tell(_,[]).
kif_tell(Why,[H|T]):- !,must_det_l((kif_tell(Why,H),kb_incr(Why,Why2),kif_tell(Why2,T))).
kif_tell(Why,Wff):-  
   must_det_l((kif_to_boxlog(Wff,Why,Asserts),
      kif_tell_boxes(assert_wfs_def,Why,Wff,Asserts))),!.


:- thread_local(thlocal:assert_wfs/2).
assert_wfs_def(HBINFO,HB):-if_defined(thlocal:assert_wfs(HBINFO,HB)),!.
assert_wfs_def(Why,H):-assert_wfs_fallback(Why,H).

assert_wfs_fallback(Why, HB):- subst(HB,(~),(-),HB2),subst(HB2,(not_proven_t),(not_true_t),HB1),subst(HB1,(poss),(possible_t),HBO),assert_wfs_fallback0(Why, HBO).
assert_wfs_fallback0(Why,(H:-B)):- adjust_kif('$VAR'(KB),B,HBK),demodal('$VAR'(KB),HBK,HBKD),
   wdmsg((H:-w_infer_by(Why),HBKD)),pttp_assert_wid(Why,pttp_in,(H:-B)),!.
assert_wfs_fallback0(Why, HB):- adjust_kif('$VAR'(KB),HB,HBK),demodal('$VAR'(KB),HBK,HBKD),
   wdmsg((HBKD:-w_infer_by(Why))),pttp_assert_wid(Why,pttp_in,(HB)),!.

*/

:- export(kb_incr/2).
kb_incr(WffNum1 ,WffNum2):-is_ftVar(WffNum1),trace_or_throw(kb_incr(WffNum1 ,WffNum2)).
kb_incr(WffNum1 ,WffNum2):-number(WffNum1),WffNum2 is WffNum1 + 1,!.
%kb_incr(WffNum1 ,WffNum2):-atom(WffNum1),WffNum2=..[WffNum1,0],!.
kb_incr(WffNum1 ,WffNum2):-atomic(WffNum1),WffNum2 = WffNum1:0,!.
kb_incr(WffNum1 ,WffNum2):-WffNum1=..[F,P,A|REST],kb_incr(A ,AA),!,WffNum2=..[F,P,AA|REST].
kb_incr(WffNum1 ,WffNum2):-trace_or_throw(kb_incr(WffNum1 ,WffNum2)).
/*
kif_tell_boxes(How,Why,Wff0,Asserts0):-
 must_det_l((
  show_call_failure(kif_unnumbervars(Asserts0+Wff0,Asserts+Wff)),  
  %fully_expand(Get1,Get),
  get_constraints(Wff,Isas), 
  kif_tell_adding_constraints(Why,Isas,Asserts))),
   findall(HB-WhyHB,retract(thlocal:in_code_Buffer(HB,WhyHB,_)),List),
   list_to_set(List,Set),
   forall(member(HB-WhyHB,Set),
      call(How,WhyHB,HB)).
*/

kif_tell_adding_constraints(Why,Isas,Get1Get2):- var(Get1Get2),!,trace_or_throw(var_kif_tell_isa_boxes(Why,Isas,Get1Get2)).
kif_tell_adding_constraints(Why,Isas,(Get1,Get2)):- !,kif_tell_adding_constraints(Why,Isas,Get1),kb_incr(Why,Why2),kif_tell_adding_constraints(Why2,Isas,Get2).
kif_tell_adding_constraints(Why,Isas,[Get1|Get2]):- !,kif_tell_adding_constraints(Why,Isas,Get1),kb_incr(Why,Why2),kif_tell_adding_constraints(Why2,Isas,Get2).
kif_tell_adding_constraints(_,_,[]).
kif_tell_adding_constraints(_,_,z_unused(_)):-!.
kif_tell_adding_constraints(Why,Isas,((H:- B))):- conjoin(Isas,B,BB), kif_tell_boxes1(Why,(H:- BB)).
kif_tell_adding_constraints(Why,Isas,((H))):- kif_tell_boxes1(Why,(H:- Isas)).

kif_tell_boxes1(_,[]).
kif_tell_boxes1(Why,List):- is_list(List),!,list_to_set(List,[H|T]),must_det_l((kif_tell_boxes1(Why,H),kb_incr(Why,Why2),kif_tell_boxes1(Why2,T))).
kif_tell_boxes1(_,z_unused(_)):-!.
kif_tell_boxes1(Why,AssertI):- must_det_l((simplify_bodies(AssertI,AssertO),kif_tell_boxes3(save_wfs,Why,AssertO))).

:- thread_local(thlocal:in_code_Buffer/3).


kif_tell_boxes3(How,Why,Assert):- 
  must_det_l((
  boxlog_to_prolog(Assert,Prolog1),
  defunctionalize(Prolog1,Prolog2),
  kif_unnumbervars(Prolog2,PTTP), 
  call(How,Why,PTTP))).

kif_unnumbervars(X,YY):-
 must_det_l((
   with_output_to(string(A),write_term(X,[character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,NamedVars),
   YY=Y,
   add_newvars(NamedVars))).


simplify_bodies((H:- B),(H:- BC)):- must_det_l((conjuncts_to_list(B,RB),simplify_list(_KB,RB,BB),list_to_conjuncts(BB,BC))).
simplify_bodies((B),(BC)):- must_det_l((conjuncts_to_list(B,RB),simplify_list(_KB,RB,BB),list_to_conjuncts(BB,BC))).


simplify_list(KB,RB,BBS):- list_to_set(RB,BB),must_maplist(removeQ(KB),BB,BBO),list_to_set(BBO,BBS).

save_wfs(Why,PrologI):- must_det_l((thglobal:as_prolog(PrologI,Prolog), 
   with_assertions(thlocal:current_local_why(Why,Prolog),
   pfc_add_h(save_in_code_buffer,Why,Prolog)))).

nots_to(H,To,HH):-subst_except(H,neg,To,HH),subst_except(H,-,To,HH),subst_except(H,~,To,HH),subst_except(H,neg,To,HH),!.
neg_h_if_neg(H,HH):-nots_to(H,'~',HH).
neg_b_if_neg(HBINFO,B,BBB):-nots_to(B,'~',BB),sort_body(HBINFO,BB,BBB),!.


sort_body(HBINFO,BB,BBB):-sort_body_0(HBINFO,BB,BBB),(BBB=@=BB->true; (expand_to_hb(HBINFO,H,_),nop(dmsg([(H:-BB),'=>',(H:-BBB)])))).

sort_body_0(_,SORTED,SORTED):-leave_as_is(SORTED).
sort_body_0(HBINFO,(A,B),SORTED):-!,conjuncts_to_list((A,B),List),
   must_maplist(sort_body_0(HBINFO),List,ListIn),
   predsort(litcost_compare(HBINFO),ListIn,SortedL),
   list_to_conjuncts(SortedL,SORTED).
sort_body_0(HBINFO,(A;B),SORTED):-!,disjuncts_to_list((A;B),List),
   must_maplist(sort_body_0(HBINFO),List,ListIn),
   predsort(litcost_compare(HBINFO),ListIn,SortedL),
   list_to_conjuncts((;),SortedL,SORTED).
sort_body_0(_,SORTED,SORTED).

litcost_compare(_,=,A,B):- A=@=B,!.
litcost_compare(HBINFO,Comp,A,B):-lit_cost(HBINFO,A,AC),lit_cost(HBINFO,B,BC),compare(CompC,AC,BC),
  (CompC\== (=) -> CompC = Comp ; Comp = (<)).

lit_cost(_,A,9):-isSlot(A).
lit_cost(_,A,0):- \+ compound(A),!.
lit_cost(HBINFO,A,AC):- A=..[F,ARG], is_log_op(F),!,lit_cost(HBINFO,ARG,AC0),!,
 % this removes the headvar bonus
  term_slots(A,Slots),length(Slots,SC),
  AC is AC0+SC.
lit_cost(HBINFO,A,AC):- expand_to_hb(HBINFO,H,B),
  var_count_num(A,H,SH,UH),
  var_count_num(A,B,VC,Singles),
  AC is Singles*3 + VC + UH - SH.

simp_code(HB,(H:-BS)):-expand_to_hb(HB,H,B),conjuncts_to_list(B,BL),sort(BL,BS),!.
simp_code(A,A).


var_count_num(Term,SharedTest,SharedCount,UnsharedCount):- term_slots(Term,Slots),term_slots(SharedTest,TestSlots),
  subtract(Slots,TestSlots,UnsharedSlots),
  subtract(Slots,UnsharedSlots,SharedSlots),
  length(SharedSlots,SharedCount),
  length(UnsharedSlots,UnsharedCount).

pfc_add_h(How,Why,(H:- B)):- neg_h_if_neg(H,HH), neg_b_if_neg((HH:- B),B,BB),!,call(How,Why,(HH:-BB)).
pfc_add_h(How,Why,(H)):- neg_h_if_neg(H,HH), call(How,Why,(HH)).

save_in_code_buffer(_ ,HB):- simp_code(HB,SIMP),thlocal:in_code_Buffer(HB,_,SIMP),!.
save_in_code_buffer(Why,HB):- simp_code(HB,SIMP),assert(thlocal:in_code_Buffer(HB,Why,SIMP)).

use_was_isa_h(_,ftTerm,true):- !.
use_was_isa_h(_,argi(mudEquals,_),true):- !.
use_was_isa_h(_,argi(skolem,_),true):- !.
use_was_isa_h(I,T,ISA):- to_isa_out(I,T,ISA),!.

generate_ante([],[],InOut,InOut).
generate_ante([I|VarsA],[T|VarsB],In,Isas):- use_was_isa_h(I,T,ISA), conjoin(In,ISA,Mid),generate_ante(VarsA,VarsB,Mid,Isas).

get_constraints(T,true):- T==true.
get_constraints(_,true):- !.
get_constraints(ListA,Isas):- 
     must_det_l((copy_term(ListA,ListB),
      term_variables(ListA,VarsA),
      term_variables(ListB,VarsB),
      attempt_attribute_args(isAnd,ftAskable,ListB),
      attribs_to_atoms(VarsB,VarsB),
      generate_ante(VarsA,VarsB,true,Isas))).


boxlog_to_prolog(IN,OUT):-notrace(leave_as_is(IN)),!,IN=OUT.
boxlog_to_prolog(IN,OUT):-once(demodal_sents('$VAR'('KB'),IN,MID)),IN\=@=MID,!,boxlog_to_prolog(MID,OUT).
boxlog_to_prolog(IN,OUT):-once(subst_except(IN,neg,~,MID)),IN\=@=MID,!,boxlog_to_prolog(MID,OUT).
boxlog_to_prolog(IN,OUT):-once(subst_except(IN,poss,possible_t,MID)),IN\=@=MID,!,boxlog_to_prolog(MID,OUT).
boxlog_to_prolog(H, HH):-is_list(H),!,must_maplist(boxlog_to_prolog,H,HH).

boxlog_to_prolog((V:- TRUE),VE):- is_true(TRUE),boxlog_to_prolog(V,VE),!.
boxlog_to_prolog((H:- B),(HH:- BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H & B),(HH , BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H v B),(HH ; BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H , B),(HH , BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H ; B),(HH ; BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog(H,O):- H=..[N,nesc(F)],kb_nlit(_,N),nonvar(F),!,HH=..[N,F],boxlog_to_prolog(HH,O).

/*
boxlog_to_prolog(nesc(not(F)),O):- nonvar(F),!,boxlog_to_prolog(neg(F),O).
boxlog_to_prolog(nesc(F),O):- nonvar(F),!,boxlog_to_prolog(F,O).
boxlog_to_prolog(not(nesc(F)),O):- nonvar(F),!,boxlog_to_prolog(naf(F),O).
boxlog_to_prolog(~poss(F),O):-nonvar(F),!,boxlog_to_prolog(not_poss(F),O).
boxlog_to_prolog(not(H),not(HH)):- !,boxlog_to_prolog(H,HH).
boxlog_to_prolog(not(F),neg(O)):- nonvar(F),!,boxlog_to_prolog(F,O).
*/

boxlog_to_prolog(IN,OUT):-demodal_sents(_KB,IN,M),IN\=@=M,!,boxlog_to_prolog(M,OUT).


boxlog_to_prolog( H, HH):- H=..[F|ARGS],!,boxlog_to_prolog(ARGS,ARGSO),!,HH=..[F|ARGSO].
boxlog_to_prolog(BL,PTTP):- thglobal:as_prolog(BL,PTTP).



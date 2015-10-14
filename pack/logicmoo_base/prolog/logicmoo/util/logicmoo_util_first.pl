% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_first.pl
:- module(logicmoo_util_first,
          [ 
          safe_numbervars/1,
          safe_numbervars/2,

          match_predicates/2,
          match_predicates/5,
          if_may_hide/1,
          mpred_trace_less/1,
          mpred_trace_none/1,
          mpred_trace_nochilds/1,
          mpred_trace_childs/1,

            add_newvar/2,
            add_newvars/1,
            always_show_dmsg/0,
            lbl_vars/4,

            mustvv/1,
            name_to_var/3,


            tlbugger:ifHideTrace/0,
            register_var/3,
            register_var/4,
            register_var_0/4,
            remove_grounds/2,
            renumbervars/2,
            renumbervars1/2,
            renumbervars1/4,
            samify/2,
            snumbervars/1,
            snumbervars/3,
            snumbervars/4,
            term_to_string/2,
            unnumbervars/2,
            unnumbervars_and_save/2,
            var_to_name/3
          ]).
:- meta_predicate

  if_may_hide(0),
   match_predicates(:, -),
   match_predicates(:,-,-,-,-),
   mpred_trace_none(:),
   mpred_trace_less(:),
   mpred_trace_childs(:),
   mpred_trace_nochilds(:),

        module_meta_transparent(:),
        mustvv(0),
        renumbervars(?, ?),
        snumbervars(?),
        snumbervars(*, ?, ?),
        snumbervars(*, ?, ?, ?).
:- module_transparent


          match_predicates/2,
          match_predicates/5,
          if_may_hide/1,
          mpred_trace_less/1,
          mpred_trace_none/1,
          mpred_trace_nochilds/1,
          mpred_trace_childs/1,
        add_newvar/2,
        add_newvars/1,
        always_show_dmsg/0,
        lbl_vars/4,
        name_to_var/3,
        register_var/3,
        register_var/4,
        register_var_0/4,
        remove_grounds/2,
        renumbervars1/2,
        renumbervars1/4,
        samify/2,
        export_file_preds/0,
        export_file_preds/1,
        export_file_preds/6,
        some_flocation/3,
        some_location/3,
        term_to_string/2,
        unnumbervars/2,
   safe_numbervars/1,
   safe_numbervars/2,
        unnumbervars_and_save/2,
        var_to_name/3.


:- meta_predicate snumbervars(?,?,?,?).
:- meta_predicate snumbervars(?,?,?).
:- meta_predicate safe_numbervars(?).
:- meta_predicate contains_singletons(?).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (0.206 sec)
:- meta_predicate renumbervars(?,?).
:- meta_predicate randomVars(?).
:- meta_predicate snumbervars(?).
% Restarting analysis ...
% Found new meta-predicates in iteration 3 (0.121 sec)
:- meta_predicate programmer_error(0).
:- meta_predicate safe_numbervars(*,?).



%================================================================
% pred tracing 
%================================================================

% = :- meta_predicate('match_predicates'(:,-)).

match_predicates(M:Spec,Preds):- catch('$find_predicate'(M:Spec, Preds),_,catch('$find_predicate'(Spec, Preds),_,catch('$find_predicate'(lmconf:Spec, Preds),_,fail))),!.
match_predicates(MSpec,MatchesO):- catch('$dwim':'$find_predicate'(MSpec,Matches),_,Matches=[]),!,MatchesO=Matches.

match_predicates(_:[],_M,_P,_F,_A):-!,fail.
match_predicates(IM:(ASpec,BSpec),M,P,F,A):-!, (match_predicates(IM:(ASpec),M,P,F,A);match_predicates(IM:(BSpec),M,P,F,A)).
match_predicates(IM:[ASpec|BSpec],M,P,F,A):-!, (match_predicates(IM:(ASpec),M,P,F,A);match_predicates(IM:(BSpec),M,P,F,A)).
match_predicates(IM:IF/IA,M,P,F,A):- '$find_predicate'(IM:P,Matches),member(CM:F/A,Matches),functor(P,F,A),(predicate_property(CM:P,imported_from(M))->true;CM=M),IF=F,IA=A.
match_predicates(Spec,M,P,F,A):- '$find_predicate'(Spec,Matches),member(CM:F/A,Matches),functor(P,F,A),(predicate_property(CM:P,imported_from(M))->true;CM=M).

:- module_transparent(if_may_hide/1).
% = :- meta_predicate(if_may_hide(0)).
if_may_hide(_G):-!.
% if_may_hide(G):-G.

:- meta_predicate with_unlocked_pred(:,0).
with_unlocked_pred(Pred,Goal):-
   (predicate_property(Pred,foreign)-> true ;
  (
 ('$get_predicate_attribute'(Pred, system, 0) -> Goal ;
 ('$set_predicate_attribute'(Pred, system, 0),
   catch(Goal,_,true),'$set_predicate_attribute'(Pred, system, 1))))).

:- export(mpred_trace_less/1).
mpred_trace_less(W):- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
  '$set_predicate_attribute'(M:Pred, noprofile, 1),
  (A==0 -> '$set_predicate_attribute'(M:Pred, hide_childs, 1);'$set_predicate_attribute'(M:Pred, hide_childs, 1)),
  (A==0 -> '$set_predicate_attribute'(M:Pred, trace, 0);'$set_predicate_attribute'(M:Pred, trace, 1))))))).

:- export(mpred_trace_none/1).
mpred_trace_none(W):- (forall(match_predicates(W,M,Pred,F,A),
with_unlocked_pred(M:Pred,(
('$hide'(M:F/A),'$set_predicate_attribute'(M:Pred, hide_childs, 1),noprofile(M:F/A),nospy(M:Pred)))))).

:- export(mpred_trace_nochilds/1).
mpred_trace_nochilds(W):- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
'$set_predicate_attribute'(M:Pred, trace, 1),
'$set_predicate_attribute'(M:Pred, noprofile, 1),
'$set_predicate_attribute'(M:Pred, hide_childs, 1)))))).

:- export(mpred_trace_childs/1).
mpred_trace_childs(W) :- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
'$set_predicate_attribute'(M:Pred, trace, 0),
'$set_predicate_attribute'(M:Pred, noprofile, 1),
'$set_predicate_attribute'(M:Pred, hide_childs, 0)))))).   

mpred_trace_all(W) :- forall(match_predicates(W,M,Pred,_,A),( 
 with_unlocked_pred(M:Pred,(
 (A==0 -> '$set_predicate_attribute'(M:Pred, trace, 0);'$set_predicate_attribute'(M:Pred, trace, 1)),
 '$set_predicate_attribute'(M:Pred, noprofile, 0),
'$set_predicate_attribute'(M:Pred, hide_childs, 0))))).

%:-mpred_trace_all(prolog:_).
%:-mpred_trace_all('$apply':_).
%:-mpred_trace_all(system:_).

:- include('logicmoo_util_header.pi').

:- export(tlbugger:ifHideTrace/0).
:- thread_local(tlbugger:ifHideTrace/0).
:- thread_local(tlbugger:tl_always_show_dmsg).
always_show_dmsg:- thread_self(main).
always_show_dmsg:- tlbugger:tl_always_show_dmsg.

term_to_string(IS,I):- on_x_fail(term_string(IS,I)),!.
term_to_string(I,IS):- on_x_fail(string_to_atom(IS,I)),!.
term_to_string(I,IS):- grtrace(term_to_atom(I,A)),string_to_atom(IS,A),!.


:- meta_predicate mustvv(0).
mustvv(G):-must(G).

:- export(unnumbervars/2).
unnumbervars(X,YY):- lbl_vars(X,[],Y,_Vs),!, mustvv(YY=Y).
% TODO compare the speed
% unnumbervars(X,YY):- mustvv(unnumbervars0(X,Y)),!,mustvv(Y=YY).



:- export(unnumbervars_and_save/2).
unnumbervars_and_save(X,YY):-
   lbl_vars(X,[],Y,Vs),
    (Vs==[]->mustvv(X=YY);
    ( % writeq((lbl_vars(X,Y,Vs))),nl,
     save_clause_vars(Y,Vs),mustvv(Y=YY))).

/*
% todo this slows the system!
unnumbervars0(X,clause(UH,UB,Ref)):- sanity(nonvar(X)),
  X = clause(H,B,Ref),!,
  mustvv(unnumbervars0((H:-B),(UH:-UB))),!.

unnumbervars0(X,YY):-lbl_vars(X,YY,_Vs).
*/
/*
lbl_vars(X,YY):-
   must_det_l((with_output_to(string(A),write_term(X,[snumbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,_NewVars),!,mustvv(YY=Y))),check_varnames(YY).
lbl_vars(X,YY,Vs):-!,lbl_vars(X,[],YY,Vs).
*/

lbl_vars(X,Vs,X,Vs):- ( \+ compound(X)),!.
lbl_vars('$VAR'(X),IVs,Y,Vs):-!, (memberchk(X=Y,IVs)->Vs=IVs;Vs=[X=Y|IVs]).
lbl_vars([X|XM],IVs,[Y|YM],Vs):-!,
  lbl_vars(X,IVs,Y,VsM),
  lbl_vars(XM,VsM,YM,Vs).
lbl_vars(XXM,IVs,YYM,Vs):-
  XXM=..[F,X|XM],
  lbl_vars(X,IVs,Y,VsM),
  lbl_vars(XM,VsM,YM,Vs),
  YYM=..[F,Y|YM].

/*
lbl_vars(X,YY,Vs):-
 must_det_l((
   with_output_to(codes(A),write_term(X,[numbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),   
   read_term_from_codes(A,Y,[variable_names(Vs),character_escapes(true),ignore_ops(true)]),!,mustvv(YY=Y),check_varnames(YY))).

unnumbervars_and_save(X,YO):-
 term_variables(X,TV),
 mustvv((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   mustvv(atom_to_term(A,Y,NewVars)),
   (NewVars==[]-> YO=X ; (length(TV,TVL),length(NewVars,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVars),Y=X)))).



unnumbervars_and_copy(X,YO):-
 term_variables(X,TV),
 mustvv((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   mustvv(atom_to_term(A,Y,NewVars)),
   (NewVars==[]-> YO=X ; (length(TV,TVL),length(NewVars,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVars),Y=X)))).
*/

%add_newvars(_):-!.
add_newvars(Vs):- (var(Vs);Vs=[]),!.
add_newvars([N=V|Vs]):- add_newvar(N,V),!,add_newvars(Vs).


add_newvar(_,V):-nonvar(V),!.
add_newvar(N,_):-var(N),!.
add_newvar('A',_):-!.
add_newvar('B',_):-!.
add_newvar(N,_):- atom(N),atom_concat('_',_,N),!.
add_newvar(N,V):- 
  b_getval('$variable_names', V0s),
  remove_grounds(V0s,Vs),
 once((member(NN=Was,Vs),N==NN,var(Was),var(V),(Was=V))-> (V0s==Vs->true;nb_linkval('$variable_names',Vs)); nb_linkval('$variable_names',[N=V|Vs])).

remove_grounds(Vs,Vs):-var(Vs),!.
remove_grounds([],[]):-!.
remove_grounds([N=V|NewCNamedVarsS],NewCNamedVarsSG):-
   (N==V;ground(V)),remove_grounds(NewCNamedVarsS,NewCNamedVarsSG).
remove_grounds([N=V|V0s],[N=NV|Vs]):-
   (var(V) -> NV=V ; NV=_ ),
   remove_grounds(V0s,Vs).

% renumbervars(X,X):-ground(X),!.
renumbervars(X,Y):-renumbervars1(X,[],Y,_),!.
renumbervars(X,Z):-unnumbervars(X,Y),safe_numbervars(Y,Z),!.
renumbervars(Y,Z):-safe_numbervars(Y,Z),!.


renumbervars1(X,Y):-renumbervars1(X,[],Y,_).

renumbervars1(V,IVs,'$VAR'(X),Vs):- var(V), sformat(atom(X),'~w_RNV',[V]), !, (memberchk(X=V,IVs)->Vs=IVs;Vs=[X=V|IVs]).
renumbervars1(X,Vs,X,Vs):- ( \+ compound(X)),!.
renumbervars1('$VAR'(V),IVs,Y,Vs):- sformat(atom(X),'~w_VAR',[V]), !, (memberchk(X=Y,IVs)->Vs=IVs;Vs=[X=Y|IVs]).
renumbervars1([X|XM],IVs,[Y|YM],Vs):-!,
  renumbervars1(X,IVs,Y,VsM),
  renumbervars1(XM,VsM,YM,Vs).
renumbervars1(XXM,IVs,YYM,Vs):-
  XXM=..[F,X|XM],
  renumbervars1(X,IVs,Y,VsM),
  renumbervars1(XM,VsM,YM,Vs),
  YYM=..[F,Y|YM].


  
% ========================================================================================
% safe_numbervars/1 (just simpler safe_numbervars.. will use a random start point so if a partially numbered getPrologVars wont get dup getPrologVars)
% Each prolog has a specific way it could unnumber the result of a safe_numbervars
% ========================================================================================
% 7676767
safe_numbervars(E,EE):-duplicate_term(E,EE),
  get_gtime(G),numbervars(EE,G,End,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  term_variables(EE,AttVars),
  numbervars(EE,End,_,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  forall(member(V,AttVars),(copy_term(V,VC,Gs),V='$VAR'(VC=Gs))),check_varnames(EE).

get_gtime(GG):- get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G),GG is (floor(G) rem 500).

safe_numbervars(EE):-get_gtime(G),numbervars(EE,G,_End,[attvar(skip),functor_name('$VAR'),singletons(true)]),check_varnames(EE).




% register_var(?, ?, ?)
%
%   During copying one has to remeber copies of variables which can be used further during copying.
%   Therefore the register of variable copies is maintained.
%
register_var(N=V,IN,OUT):- (var(N)->true;register_var(N,IN,V,OUT)),!.

register_var(N,T,V,OUTO):-register_var_0(N,T,V,OUT),mustvv(OUT=OUTO),!.
register_var(N,T,V,O):-append(T,[N=V],O),!.

register_var_0(N,T,V,OUT):- atom(N),is_list(T),member(NI=VI,T),atom(NI),N=NI,V=@=VI,samify(V,VI),!,OUT=T.
register_var_0(N,T,V,OUT):- atom(N),is_list(T),member(NI=VI,T),atom(NI),N=NI,V=VI,!,OUT=T.

register_var_0(N,T,V,OUT):- mustvv(nonvar(N)),
   ((name_to_var(N,T,VOther)-> mustvv((OUT=T,samify(V,VOther)));
     (once(nb_getval('$variable_names',Before);Before=[]),
      (name_to_var(N,Before,VOther)  -> mustvv((samify(V,VOther),OUT= [N=V|T]));
         (var_to_name(V,T,_OtherName)                  -> OUT= [N=V|T];
           (var_to_name(V,Before,_OtherName)              -> OUT= [N=V|T];fail)))))),!.


register_var_0(N,T,V,OUT):- var(N),
   (var_to_name(V,T,N)                -> OUT=T;
     (once(nb_getval('$variable_names',Before);Before=[]),
          (var_to_name(V,Before,N)   -> OUT= [N=V|T];
               OUT= [N=V|T]))),!.





% different variables (now merged)
samify(V,V0):-var(V),var(V0),!,mustvv(V=V0).
samify(V,V0):-mustvv(V=@=V0),V=V0. 

var_to_name(V,[N=V0|T],N):-
    V==V0 -> true ;          % same variables
    var_to_name(V,T,N).

name_to_var(N,T,V):- var(N),!,var_to_name(N,T,V).
name_to_var(N,[N0=V0|T],V):- 
   N0==N -> samify(V,V0) ; name_to_var(N,T,V).


/*
% ===================================================================
% Safely number vars
% ===================================================================
bugger_numbervars_with_names(Term):-
   term_variables(Term,Vars),bugger_name_variables(Vars),!,snumbervars(Vars,91,_,[attvar(skip),singletons(true)]),!,

bugger_name_variables([]).
bugger_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   bugger_name_variables(Vars).

*/
:- export(snumbervars/1).
snumbervars(Term):-snumbervars(Term,0,_).

:- export(snumbervars/3).
snumbervars(Term,Start,End):- integer(Start),var(End),!,snumbervars(Term,Start,End,[]).
snumbervars(Term,Start,List):- integer(Start),is_list(List),!,snumbervars(Term,Start,_,List).
snumbervars(Term,Functor,Start):- integer(Start),atom(Functor),!,snumbervars(Term,Start,_End,[functor_name(Functor)]).
snumbervars(Term,Functor,List):- is_list(List),atom(Functor),!,snumbervars(Term,0,_End,[functor_name(Functor)]).


:- export(snumbervars/4).
snumbervars(Term,Start,End,List):-numbervars(Term,Start,End,List).



% % :- use_module(logicmoo_util_varnames).

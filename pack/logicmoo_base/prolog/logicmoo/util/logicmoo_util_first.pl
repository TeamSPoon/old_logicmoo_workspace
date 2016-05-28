% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_first.pl
:- module(logicmoo_util_first,
          [ 
          safe_numbervars/1,
          safe_numbervars/2,
          put_variable_names/1,
          nput_variable_names/1,
          check_variable_names/2,
                  unnumbervars4/4,
                  get_varname_list/1,
                  set_varname_list/1,
                  icatch/1,
user_ensure_loaded/1,
user_use_module/1,
alldiscontiguous/0,
arg_is_transparent/1,
all_module_predicates_are_transparent/1,
alldiscontiguous/0,
arg_is_transparent/1,
module_meta_predicates_are_transparent/1,
module_predicate/3,
module_predicate/4,
module_predicates_are_exported/0,
module_predicates_are_exported/1,
module_predicates_are_exported0/1,
module_predicates_are_not_exported_list/2,
quiet_all_module_predicates_are_transparent/1,
          export_all_preds/0,
          export_all_preds/1,
          export_if_noconflict/2,

          if_may_hide/1,
          match_predicates/2,
          match_predicates/5,
          mpred_trace_childs/1,
          mpred_trace_less/1,
          mpred_trace_nochilds/1,
          mpred_trace_none/1,

            add_newvar/2,
            add_newvars/1,

            %lbl_vars/6,

            mustvv/1,
            name_to_var/3,
            source_context_module/1,


            tlbugger:ifHideTrace/0,
            register_var/3,
            register_var/4,
            register_var_0/4,
            remove_grounds/2,
            renumbervars_prev/2,
            renumbervars1/2,
            renumbervars1/4,
            add_var_to_env/2,
            
            samify/2,
            snumbervars/1,
            snumbervars/3,
            snumbervars/4,
            term_to_string/2,
            unnumbervars/2,
            unnumbervars_and_save/2,
            var_to_name/3
          ]).


:- if(\+ current_predicate(system:nop/1)).
system:nop(_).
:- endif.


:- meta_predicate

  if_may_hide(0),
   match_predicates(:, -),
   match_predicates(:,-,-,-,-),
   mpred_trace_none(:),
   mpred_trace_less(:),
   mpred_trace_childs(:),
   mpred_trace_nochilds(:),

        mustvv(0),
        icatch(0),
        renumbervars_prev(?, ?),
        snumbervars(?),
        snumbervars(*, ?, ?),
        snumbervars(*, ?, ?, ?).
:- module_transparent
source_context_module/1,

user_ensure_loaded/1,
icatch/1,
user_use_module/1,
alldiscontiguous/0,
arg_is_transparent/1,
all_module_predicates_are_transparent/1,
alldiscontiguous/0,
arg_is_transparent/1,
module_meta_predicates_are_transparent/1,
module_predicate/3,
module_predicate/4,
module_predicates_are_exported/0,
module_predicates_are_exported/1,
module_predicates_are_exported0/1,
module_predicates_are_not_exported_list/2,
quiet_all_module_predicates_are_transparent/1,

          match_predicates/2,
          match_predicates/5,
          if_may_hide/1,
          mpred_trace_less/1,
          mpred_trace_none/1,
          mpred_trace_nochilds/1,
          mpred_trace_childs/1,
        add_newvar/2,
        add_newvars/1,
        %lbl_vars/6,
        name_to_var/3,
        register_var/3,
        register_var/4,
        register_var_0/4,
        remove_grounds/2,
        renumbervars1/2,
        renumbervars1/4,
        samify/2,
        
        term_to_string/2,
        unnumbervars/2,
        add_var_to_env/2,
   safe_numbervars/1,
   safe_numbervars/2,
        unnumbervars_and_save/2,
        var_to_name/3.


:- meta_predicate snumbervars(?,?,?,?).
:- meta_predicate snumbervars(?,?,?).
:- meta_predicate safe_numbervars(?).
/*
        module_meta_transparent(:),
   some_flocation/3,

:- meta_predicate contains_singletons(?).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (0.206 sec)
:- meta_predicate renumbervars_prev(?,?).
:- meta_predicate randomVars(?).
:- meta_predicate snumbervars(?).
% Restarting analysis ...
% Found new meta-predicates in iteration 3 (0.121 sec)
:- meta_predicate programmer_error(0).
:- meta_predicate safe_numbervars(*,?).
        export_file_preds/1,
        export_file_preds/6,
   export_file_preds/0,
some_location/3,
*/

%= 	 	 

%% alldiscontiguous is semidet.
%
% Alldiscontiguous.
%
alldiscontiguous:-!.


%= 	 	 

%% source_context_module( ?CM) is semidet.
%
% Source Context Module.
%
source_context_module(CM):-'$set_source_module'(CM,CM).

%================================================================
% pred tracing 
%================================================================

% = :- meta_predicate('match_predicates'(:,-)).


%= 	 	 

%% match_predicates( ?MSpec, -MatchesO) is semidet.
%
% Match Predicates.
%
match_predicates(M:Spec,Preds):- catch('$find_predicate'(M:Spec, Preds),_,catch('$find_predicate'(Spec, Preds),_,catch('$find_predicate'(lmconf:Spec, Preds),_,fail))),!.
match_predicates(MSpec,MatchesO):- catch('$dwim':'$find_predicate'(MSpec,Matches),_,Matches=[]),!,MatchesO=Matches.


%= 	 	 

%% match_predicates( ?Spec, -M, -P, -F, -A) is semidet.
%
% Match Predicates.
%
match_predicates(_:[],_M,_P,_F,_A):-!,fail.
match_predicates(IM:(ASpec,BSpec),M,P,F,A):-!, (match_predicates(IM:(ASpec),M,P,F,A);match_predicates(IM:(BSpec),M,P,F,A)).
match_predicates(IM:[ASpec|BSpec],M,P,F,A):-!, (match_predicates(IM:(ASpec),M,P,F,A);match_predicates(IM:(BSpec),M,P,F,A)).
match_predicates(IM:IF/IA,M,P,F,A):- '$find_predicate'(IM:P,Matches),member(CM:F/A,Matches),functor(P,F,A),(predicate_property_safe(CM:P,imported_from(M))->true;CM=M),IF=F,IA=A.
match_predicates(Spec,M,P,F,A):- '$find_predicate'(Spec,Matches),member(CM:F/A,Matches),functor(P,F,A),(predicate_property_safe(CM:P,imported_from(M))->true;CM=M).

:- module_transparent(if_may_hide/1).
% = :- meta_predicate(if_may_hide(0)).
%if_may_hide(_G):-!.

%= 	 	 

%% if_may_hide( :GoalG) is semidet.
%
% If May Hide.
%
if_may_hide(G):-G.

:- meta_predicate with_unlocked_pred(:,0).

%= 	 	 

%% with_unlocked_pred( ?Pred, :GoalGoal) is semidet.
%
% Using Unlocked Predicate.
%
with_unlocked_pred(Pred,Goal):-
   (predicate_property_safe(Pred,foreign)-> true ;
  (
 ('$get_predicate_attribute'(Pred, system, 0) -> Goal ;
 ('$set_predicate_attribute'(Pred, system, 0),
   catch(Goal,_,true),'$set_predicate_attribute'(Pred, system, 1))))).


icatch(Goal):- ignore(catch(Goal,_,true)).

:- export(mpred_trace_less/1).

%= 	 	 

%% mpred_trace_less( ?W) is semidet.
%
% Managed Predicate  Trace less.
%
mpred_trace_less(W):- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
  '$set_predicate_attribute'(M:Pred, noprofile, 1),
  (A==0 -> '$set_predicate_attribute'(M:Pred, hide_childs, 1);'$set_predicate_attribute'(M:Pred, hide_childs, 1)),
  (A==0 -> '$set_predicate_attribute'(M:Pred, trace, 0);'$set_predicate_attribute'(M:Pred, trace, 1))))))).

:- export(mpred_trace_none/1).

%= 	 	 

%% mpred_trace_none( ?W) is semidet.
%
% Managed Predicate  Trace none.
%
mpred_trace_none(W):- (forall(match_predicates(W,M,Pred,F,A),
  with_unlocked_pred(M:Pred,(('$hide'(M:F/A),'$set_predicate_attribute'(M:Pred, hide_childs, 1),noprofile(M:F/A),nop(nospy(M:Pred))))))).

:- export(mpred_trace_nochilds/1).

%= 	 	 

%% mpred_trace_nochilds( ?W) is semidet.
%
% Managed Predicate  Trace nochilds.
%
mpred_trace_nochilds(W):- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
'$set_predicate_attribute'(M:Pred, trace, 1),
'$set_predicate_attribute'(M:Pred, noprofile, 0),
'$set_predicate_attribute'(M:Pred, hide_childs, 1)))))).

:- export(mpred_trace_childs/1).



%% mpred_trace_childs( ?W) is semidet.
%
% Managed Predicate  Trace childs.
%
mpred_trace_childs(W) :- if_may_hide(forall(match_predicates(W,M,Pred,_,_),(
with_unlocked_pred(M:Pred,(
'$set_predicate_attribute'(M:Pred, trace, 0),
'$set_predicate_attribute'(M:Pred, noprofile, 1),
'$set_predicate_attribute'(M:Pred, hide_childs, 0)))))).   


%= 	 	 

%% mpred_trace_all( ?W) is semidet.
%
% Managed Predicate  Trace all.
%
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



%% oncely_clean(Goal)
%
% throws an exception if Goal leaves choicepoints or
% if goal fails
oncely_clean(Goal):- 
 '$sig_atomic'((Goal,assertion(deterministic(true))))
  ->true;
   throw(failed_oncely_clean(Goal)).



%= 	 	 

%% term_to_string( ?IS, ?I) is semidet.
%
% Hook To [pldoc_html:term_to_string/2] For Module Logicmoo_util_first.
% Term Converted To String.
%
term_to_string(IS,I):- on_x_fail(term_string(IS,I)),!.
term_to_string(I,IS):- on_x_fail(string_to_atom(IS,I)),!.
term_to_string(I,IS):- grtrace(term_to_atom(I,A)),string_to_atom(IS,A),!.


:- meta_predicate mustvv(0).

%= 	 	 

%% mustvv( :GoalG) is semidet.
%
% Mustvv.
%
mustvv(G):-must(G).

%:- export(unnumbervars/2).
% unnumbervars(X,YY):- lbl_vars(_,_,X,[],Y,_Vs),!, mustvv(YY=Y).
% TODO compare the speed
% unnumbervars(X,YY):- mustvv(unnumbervars0(X,Y)),!,mustvv(Y=YY).


get_varname_list(VsOut):- nb_current('$variable_names',Vs),!,check_variable_names(Vs,VsOut).
set_varname_list(VsIn):- check_variable_names(VsIn,Vs),
  b_setval('$variable_names',[]),
  duplicate_term(Vs,VsD),
  nb_linkval('$variable_names',VsD).

add_var_to_env(Name,Var):- get_varname_list(VsIn),
   add_var_to_list(Name,Var,VsIn,NewName,NewVar,NewVs),
   (NewName\==Name -> put_attr(Var, vn, NewName) ; true),
   (NewVar \==Var  -> put_attr(NewVar, vn, Name) ; true),
   (NewVs  \==VsIn -> put_variable_names(NewVs) ; true).
   

%% add_var_to_list(Name,Var,Vs,NewName,NewVar,NewVs) is det.
add_var_to_list(Name,Var,Vs,NewName,NewVar,NewVs):- member(N0=V0,Vs), Var==V0,!,
            (Name==N0 -> ( NewName=Name,NewVar=Var, NewVs=Vs ) ;  ( NewName=N0,NewVar=Var,NewVs=[Name=Var|Vs])),!.
% a current name but points to a diffentrt var
add_var_to_list(Name,Var,Vs,NewName,NewVar,NewVs):- member(Name=_,Vs),
              length(Vs,Len),atom_concat(Name,Len,NameAgain0),( \+ member(NameAgain0=_,Vs)-> NameAgain0=NameAgain ; gensym(Name,NameAgain)),
              NewName=NameAgain,NewVar=Var, 
              NewVs=[NewName=NewVar|Vs],!.
add_var_to_list(Name,Var,Vs,NewName,NewVar,NewVs):-  
  NewName=Name,NewVar=Var,NewVs=[Name=Var|Vs],!.


%= 	 	 

%% unnumbervars( ?X, ?Y) is semidet.
%
% Unnumbervars.
%
unnumbervars(X,Y):- must(cnotrace(unnumbervars_and_save(X,Y))).


put_variable_names(NewVs):-  check_variable_names(NewVs,Checked),call(b_setval,'$variable_names',Checked).
nput_variable_names(NewVs):- check_variable_names(NewVs,Checked),call(nb_setval,'$variable_names',Checked).

check_variable_names(I,O):- (\+ member(free=_,I) -> O=I ; 
   (set_prolog_flag(variable_names_bad,true),trace_or_throw(bad_check_variable_names))).

%= 	 	 

%% unnumbervars_and_save( ?X, ?YO) is semidet.
%
% Unnumbervars And Save.
%

unnumbervars_and_save(X,YO):- must(cnotrace(unnumbervars4(X,[],_,YO))),!.
% unnumbervars_and_save(X,YO):- \+ ((sub_term(V,X),compound(V),'$VAR'(_)=V)),!,YO=X.

/*
unnumbervars_and_save(X,YO):- (get_varname_list(Vs)->true;Vs=[]),unnumbervars4(X,Vs,NewVs,YO),!,
   (NewVs  \==Vs   -> put_variable_names(NewVs) ; true).
unnumbervars_and_save(X,YO):-
 term_variables(X,TV),
 mustvv((source_variables_l(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   mustvv(atom_to_term(A,Y,NewVs)),
   (NewVs==[]-> YO=X ; (length(TV,TVL),length(NewVs,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (add_newvars(NewVs),YO=Y)))).
*/

%% unnumbervars4(TermIn,VsIn,NewVs,TermOut) is det.
%
% Unnumbervars And Save.
%
unnumbervars4(Var,Vs,Vs,Var):- \+ compound(Var),!.
unnumbervars4((I,TermIn),VsIn,NewVs,(O,TermOut)):- !,unnumbervars4(I,VsIn,VsM,O),unnumbervars4(TermIn,VsM,NewVs,TermOut).
unnumbervars4((I:TermIn),VsIn,NewVs,(O:TermOut)):- !,unnumbervars4(I,VsIn,VsM,O),unnumbervars4(TermIn,VsM,NewVs,TermOut).
unnumbervars4([I|TermIn],VsIn,NewVs,[O|TermOut]):- !,unnumbervars4(I,VsIn,VsM,O),unnumbervars4(TermIn,VsM,NewVs,TermOut).
unnumbervars4('$VAR'(Name),VsIn,NewVs,Var):- nonvar(Name),!, (member(Name=Var,VsIn)->NewVs=VsIn;NewVs=[Name=Var|VsIn]),!,put_attr(Var,vn,Name).
unnumbervars4(PTermIn,VsIn,NewVs,PTermOut):- compound_name_arguments(PTermIn,F,TermIn),unnumbervars4(TermIn,VsIn,NewVs,TermOut),compound_name_arguments(PTermOut,F,TermOut).
   

 

/*

unnumbervars_and_save(X,YO):-
 term_variables(X,TV),
 mustvv((source_variables_l(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   mustvv(atom_to_term(A,Y,NewVs)),
   (NewVs==[]-> YO=X ; (length(TV,TVL),length(NewVs,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVs),Y=X)))).


:- export(unnumbervars_and_save/2).
unnumbervars_and_save(X,YY):-
   lbl_vars(_,_,X,[],Y,Vs),
    (Vs==[]->mustvv(X=YY);
    ( % writeq((lbl_vars(N,NN,X,Y,Vs))),nl,
     save_clause_vars(Y,Vs),mustvv(Y=YY))).

% todo this slows the system!
unnumbervars0(X,clause(UH,UB,Ref)):- sanity(nonvar(X)),
  X = clause(H,B,Ref),!,
  mustvv(unnumbervars0((H:-B),(UH:-UB))),!.

unnumbervars0(X,YY):-lbl_vars(N,NN,X,YY,_Vs).

lbl_vars(N,NN,X,YY):-
   must_det_l((with_output_to(string(A),write_term(X,[snumbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,_NewVars),!,mustvv(YY=Y))),check_varnames(YY).
lbl_vars(N,NN,X,YY,Vs):-!,lbl_vars(N,NN,X,[],YY,Vs).

lbl_vars(S1,S1,A,OVs,A,OVs):- atomic(A),!.
lbl_vars(S1,S1,Var,IVs,Var,OVs):- attvar(Var),get_attr(Var,logicmoo_varnames,Nm), (memberchk(Nm=PreV,IVs)->(OVs=IVs,mustvv(PreV==Var));OVs=[Nm=Var|IVs]).
lbl_vars(S1,S2,Var,IVs,Var,OVs):- var(Var),!,(\+number(S1)->true;(((member(Nm=PreV,IVs),Var==PreV)->(OVs=IVs,put_attr(Var,logicmoo_varnames,Nm));
  (format(atom(Nm),'~q',['$VAR'(S1)]),S2 is S1+1,(memberchk(Nm=Var,IVs)->OVs=IVs;OVs=[Nm=Var|IVs]))))).

lbl_vars(S1,S1,NC,OVs,NC,OVs):- ( \+ compound(NC)),!.
lbl_vars(S1,S1,'$VAR'(Nm),IVs,PreV,OVs):-  atom(Nm), !, must(memberchk(Nm=PreV,IVs)->OVs=IVs;OVs=[Nm=PreV|IVs]).
lbl_vars(S1,S1,'$VAR'(N0),IVs,PreV,OVs):- (number(N0)->format(atom(Nm),'~q',['$VAR'(N0)]);Nm=N0), (memberchk(Nm=PreV,IVs)->OVs=IVs;OVs=[Nm=PreV|IVs]).
lbl_vars(S1,S3,[X|XM],IVs,[Y|YM],OVs):-!,lbl_vars(S1,S2,X,IVs,Y,VsM),lbl_vars(S2,S3,XM,VsM,YM,OVs).
lbl_vars(S1,S2,XXM,VsM,YYM,OVs):- XXM=..[F|XM],lbl_vars(S1,S2,XM,VsM,YM,OVs),!,YYM=..[F|YM].

*/

/*
lbl_vars(N,NN,X,YY,Vs):-
 must_det_l((
   with_output_to(codes(A),write_term(X,[numbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),   
   read_term_from_codes(A,Y,[variable_names(Vs),character_escapes(true),ignore_ops(true)]),!,mustvv(YY=Y),check_varnames(YY))).




unnumbervars_and_copy(X,YO):-
 term_variables(X,TV),
 mustvv((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   mustvv(atom_to_term(A,Y,NewVs)),
   (NewVs==[]-> YO=X ; (length(TV,TVL),length(NewVs,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVs),Y=X)))).
*/

%add_newvars(_):-!.

%= 	 	 

%% add_newvars( :TermVs) is semidet.
%
% Add Newvars.
%
add_newvars(Vs):- (var(Vs);Vs=[]),!.
add_newvars([N=V|Vs]):- add_newvar(N,V), (var(V)->put_attr(V,vn,N);true), !,add_newvars(Vs).



%= 	 	 

%% add_newvar( ?VALUE1, ?V) is semidet.
%
% Add Newvar.
%
add_newvar(_,V):-nonvar(V),!.
add_newvar(N,_):-var(N),!.
add_newvar('A',_):-!.
add_newvar('B',_):-!.
add_newvar(N,_):- atom(N),atom_concat('_',_,N),!.
add_newvar(N,V):- 
  (get_varname_list(V0s)->true;V0s=[]),
  remove_grounds(V0s,Vs),
 once((member(NN=Was,Vs),N==NN,var(Was),var(V),(Was=V))-> (V0s==Vs->true;set_varname_list(Vs)); set_varname_list([N=V|Vs])).


%= 	 	 

%% remove_grounds( :TermVs, :TermVs) is semidet.
%
% Remove Grounds.
%
remove_grounds(Vs,Vs):-var(Vs),!.
remove_grounds([],[]):-!.
remove_grounds([N=V|NewCNamedVarsS],NewCNamedVarsSG):-
   (N==V;ground(V)),remove_grounds(NewCNamedVarsS,NewCNamedVarsSG).
remove_grounds([N=V|V0s],[N=NV|Vs]):-
   (var(V) -> NV=V ; NV=_ ),
   remove_grounds(V0s,Vs).

% renumbervars_prev(X,X):-ground(X),!.

%= 	 	 

%% renumbervars_prev( ?X, ?Y) is semidet.
%
% Renumbervars Prev.
%
renumbervars_prev(X,Y):-renumbervars1(X,[],Y,_),!.
renumbervars_prev(X,Z):-unnumbervars(X,Y),safe_numbervars(Y,Z),!.
renumbervars_prev(Y,Z):-safe_numbervars(Y,Z),!.



%= 	 	 

%% renumbervars1( ?X, ?Y) is semidet.
%
% Renumbervars Secondary Helper.
%
renumbervars1(X,Y):-renumbervars1(X,[],Y,_).


%= 	 	 

%% renumbervars1( :TermV, ?IVs, :TermX, ?Vs) is semidet.
%
% Renumbervars Secondary Helper.
%
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

%= 	 	 

%% safe_numbervars( ?E, ?EE) is semidet.
%
% Safely Paying Attention To Corner Cases Numbervars.
%
safe_numbervars(E,EE):-duplicate_term(E,EE),
  get_gtime(G),numbervars(EE,G,End,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  term_variables(EE,AttVars),
  numbervars(EE,End,_,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  forall(member(V,AttVars),(copy_term(V,VC,Gs),V='$VAR'(VC=Gs))),check_varnames(EE).


%= 	 	 

%% get_gtime( ?GG) is semidet.
%
% Get Gtime.
%
get_gtime(GG):- get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G),GG is (floor(G) rem 500).


%= 	 	 

%% safe_numbervars( ?EE) is semidet.
%
% Safely Paying Attention To Corner Cases Numbervars.
%
safe_numbervars(EE):-get_gtime(G),numbervars(EE,G,_End,[attvar(skip),functor_name('$VAR'),singletons(true)]),check_varnames(EE).




% register_var(?, ?, ?)
%
%   During copying one has to remeber copies of variables which can be used further during copying.
%   Therefore the register of variable copies is maintained.
%

%= 	 	 

%% register_var( :TermN, ?IN, ?OUT) is semidet.
%
% Register Variable.
%
register_var(N=V,IN,OUT):- (var(N)->true;register_var(N,IN,V,OUT)),!.


%= 	 	 

%% register_var( ?N, ?T, ?V, ?OUTO) is semidet.
%
% Register Variable.
%
register_var(N,T,V,OUTO):-register_var_0(N,T,V,OUT),mustvv(OUT=OUTO),!.
register_var(N,T,V,O):-append(T,[N=V],O),!.


%= 	 	 

%% register_var_0( ?N, ?T, ?V, ?OUT) is semidet.
%
% register Variable  Primary Helper.
%
register_var_0(N,T,V,OUT):- atom(N),is_list(T),member(NI=VI,T),atom(NI),N=NI,V=@=VI,samify(V,VI),!,OUT=T.
register_var_0(N,T,V,OUT):- atom(N),is_list(T),member(NI=VI,T),atom(NI),N=NI,V=VI,!,OUT=T.

register_var_0(N,T,V,OUT):- mustvv(nonvar(N)),
   ((name_to_var(N,T,VOther)-> mustvv((OUT=T,samify(V,VOther)));
     ((get_varname_list(Before)->true;Before=[]),
      (name_to_var(N,Before,VOther)  -> mustvv((samify(V,VOther),OUT= [N=V|T]));
         (var_to_name(V,T,_OtherName)                  -> OUT= [N=V|T];
           (var_to_name(V,Before,_OtherName)              -> OUT= [N=V|T];fail)))))),!.


register_var_0(N,T,V,OUT):- var(N),
   (var_to_name(V,T,N)                -> OUT=T;
     ((get_varname_list(Before)->true;Before=[]),
          (var_to_name(V,Before,N)   -> OUT= [N=V|T];
               OUT= [N=V|T]))),!.





% different variables (now merged)

%= 	 	 

%% samify( ?V, ?V0) is semidet.
%
% Samify.
%
samify(V,V0):-var(V),var(V0),!,mustvv(V=V0).
samify(V,V0):-mustvv(V=@=V0),V=V0. 


%= 	 	 

%% var_to_name( ?V, :TermN, ?N) is semidet.
%
% Variable Converted To Name.
%
var_to_name(V,[N=V0|T],N):-
    V==V0 -> true ;          % same variables
    var_to_name(V,T,N).


%= 	 	 

%% name_to_var( ?N, :TermT, ?V) is semidet.
%
% Name Converted To Variable.
%
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

%= 	 	 

%% snumbervars( ?Term) is semidet.
%
% Snumbervars.
%
snumbervars(Term):-snumbervars(Term,0,_).

:- export(snumbervars/3).

%= 	 	 

%% snumbervars( ?Term, ?Start, ?End) is semidet.
%
% Snumbervars.
%
snumbervars(Term,Start,End):- integer(Start),var(End),!,snumbervars(Term,Start,End,[]).
snumbervars(Term,Start,List):- integer(Start),is_list(List),!,snumbervars(Term,Start,_,List).
snumbervars(Term,Functor,Start):- integer(Start),atom(Functor),!,snumbervars(Term,Start,_End,[functor_name(Functor)]).
snumbervars(Term,Functor,List):- is_list(List),atom(Functor),!,snumbervars(Term,0,_End,[functor_name(Functor)]).


:- export(snumbervars/4).

%= 	 	 

%% snumbervars( ?Term, ?Start, ?End, ?List) is semidet.
%
% Snumbervars.
%
snumbervars(Term,Start,End,List):-numbervars(Term,Start,End,List).








%= 	 	 

%% module_predicate( ?ModuleName, ?P, ?F, ?A) is semidet.
%
% Module Predicate.
%
module_predicate(ModuleName,P,F,A):-current_predicate(ModuleName:F/A),functor_catch(P,F,A), not((( predicate_property_safe(ModuleName:P,imported_from(IM)),IM\==ModuleName ))).


:- export((user_ensure_loaded/1)).
:- module_transparent user_ensure_loaded/1.

%= 	 	 

%% user_ensure_loaded( ?What) is semidet.
%
% User Ensure Loaded.
%
user_ensure_loaded(What):- !, '@'(ensure_loaded(What),'user').

:- module_transparent user_use_module/1.
% user_use_module(logicmoo(What)):- !, '@'(use_module(logicmoo(What)),'user').
% user_use_module(library(What)):- !, use_module(library(What)).

%= 	 	 

%% user_use_module( ?What) is semidet.
%
% User Use Module.
%
user_use_module(What):- '@'(use_module(What),'user').





%= 	 	 

%% export_all_preds is semidet.
%
% Export All Predicates.
%
export_all_preds:-source_location(File,_Line),module_property(M,file(File)),!,export_all_preds(M).


%= 	 	 

%% export_all_preds( ?ModuleName) is semidet.
%
% Export All Predicates.
%
export_all_preds(ModuleName):-forall(current_predicate(ModuleName:F/A),
                   ((export(F/A),functor_safe(P,F,A),mpred_trace_nochilds(ModuleName:P)))).







%= 	 	 

%% module_predicate( ?ModuleName, ?F, ?A) is semidet.
%
% Module Predicate.
%
module_predicate(ModuleName,F,A):-current_predicate(ModuleName:F/A),functor_safe(P,F,A),
   not((( predicate_property_safe(ModuleName:P,imported_from(IM)),IM\==ModuleName ))).

:- module_transparent(module_predicates_are_exported/0).
:- module_transparent(module_predicates_are_exported/1).
:- module_transparent(module_predicates_are_exported0/1).


%= 	 	 

%% module_predicates_are_exported is semidet.
%
% Module Predicates Are Exported.
%
module_predicates_are_exported:- source_context_module(CM),module_predicates_are_exported(CM).


%= 	 	 

%% module_predicates_are_exported( ?Ctx) is semidet.
%
% Module Predicates Are Exported.
%
module_predicates_are_exported(user):-!,source_context_module(CM),module_predicates_are_exported0(CM).
module_predicates_are_exported(Ctx):- module_predicates_are_exported0(Ctx).


%= 	 	 

%% module_predicates_are_exported0( ?ModuleName) is semidet.
%
% Module Predicates Are Exported Primary Helper.
%
module_predicates_are_exported0(user):- !. % dmsg(warn(module_predicates_are_exported(user))).
module_predicates_are_exported0(ModuleName):-
   module_property(ModuleName, exports(List)),
    findall(F/A,
    (module_predicate(ModuleName,F,A),
      not(member(F/A,List))), Private),
   module_predicates_are_not_exported_list(ModuleName,Private).

:- export(export_if_noconflict/2).
:- module_transparent(export_if_noconflict/2).

%= 	 	 

%% export_if_noconflict( ?M, :TermF) is semidet.
%
% Export If Noconflict.
%
export_if_noconflict(M,F/A):- current_module(M2),M2\=M,module_property(M2,exports(X)),member(F/A,X),dmsg(skipping_export(M2=M:F/A)),!.
export_if_noconflict(M,F/A):-M:export(F/A).

% module_predicates_are_not_exported_list(ModuleName,Private):- once((length(Private,Len),dmsg(module_predicates_are_not_exported_list(ModuleName,Len)))),fail.

%= 	 	 

%% module_predicates_are_not_exported_list( ?ModuleName, ?Private) is semidet.
%
% Module Predicates Are Not Exported List.
%
module_predicates_are_not_exported_list(ModuleName,Private):- forall(member(F/A,Private),export_if_noconflict(ModuleName,F/A)).






%= 	 	 

%% arg_is_transparent( :GoalArg) is semidet.
%
% Argument If Is A Transparent.
%
arg_is_transparent(Arg):- member(Arg,[':','^']).
arg_is_transparent(0).
arg_is_transparent(Arg):- number(Arg).

% make meta_predicate's module_transparent

%= 	 	 

%% module_meta_predicates_are_transparent( ?ModuleName) is semidet.
%
% Module Meta Predicates Are Transparent.
%
module_meta_predicates_are_transparent(_):-!.
module_meta_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)),
      ignore(((predicate_property_safe(ModuleName:P,(meta_predicate( P ))),
            not(predicate_property_safe(ModuleName:P,(transparent))), (compound(P),arg(_,P,Arg),arg_is_transparent(Arg))),
                   (nop(dmsg(todo(module_transparent(ModuleName:F/A)))),
                   (module_transparent(ModuleName:F/A)))))).

:- export(all_module_predicates_are_transparent/1).
% all_module_predicates_are_transparent(_):-!.

%= 	 	 

%% all_module_predicates_are_transparent( ?ModuleName) is semidet.
%
% All Module Predicates Are Transparent.
%
all_module_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)),
      ignore((
            not(predicate_property_safe(ModuleName:P,(transparent))),
                   ( nop(dmsg(todo(module_transparent(ModuleName:F/A))))),
                   (module_transparent(ModuleName:F/A))))).


%= 	 	 

%% quiet_all_module_predicates_are_transparent( ?ModuleName) is semidet.
%
% Quiet All Module Predicates Are Transparent.
%
quiet_all_module_predicates_are_transparent(_):-!.
quiet_all_module_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)),
      ignore((
            not(predicate_property_safe(ModuleName:P,(transparent))),
                   nop(dmsg(todo(module_transparent(ModuleName:F/A)))),
                   (module_transparent(ModuleName:F/A))))).


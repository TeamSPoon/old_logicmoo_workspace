% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_first.pl
:- module(logicmoo_util_first,
          [ add_newvar/2,
            add_newvars/1,
            always_show_dmsg/0,
            export_module_preds/0,
            functor_compare/3,
            helper_name/1,
            helper_name0/1,
            lbl_vars/4,
            list_file_preds/0,
            list_file_preds/1,
            list_file_preds/2,
            m_cp/3,
            make_module_name/2,
            module_meta_transparent/1,
            mpred_source_file/2,
            mpred_source_file_0/2,
            mpred_source_file_1/2,
            mustvv/1,
            name_to_var/3,
            no_location/3,
            portray_clause_pi/2,
            register_var/3,
            register_var/4,
            register_var_0/4,
            remove_grounds/2,
            renumbervars/2,
            renumbervars1/2,
            renumbervars1/4,
            samify/2,
            scan_file_preds/0,
            scan_file_preds/1,
            scan_file_preds/6,
            snumbervars/1,
            snumbervars/3,
            snumbervars/4,
            some_flocation/3,
            some_location/3,
            term_to_string/2,
            to_fa/3,
            unnumbervars/2,
            unnumbervars_and_save/2,
            var_to_name/3,
            write_modules/0
          ]).
:- meta_predicate
        module_meta_transparent(:),
        mustvv(0),
        renumbervars(?, 0),
        snumbervars(0),
        snumbervars(0, ?, ?),
        snumbervars(0, ?, ?, ?).
:- module_transparent
        add_newvar/2,
        add_newvars/1,
        always_show_dmsg/0,
        export_module_preds/0,
        functor_compare/3,
        helper_name/1,
        helper_name0/1,
        lbl_vars/4,
        list_file_preds/0,
        list_file_preds/1,
        list_file_preds/2,
        m_cp/3,
        make_module_name/2,
        mpred_source_file/2,
        mpred_source_file_0/2,
        mpred_source_file_1/2,
        name_to_var/3,
        no_location/3,
        portray_clause_pi/2,
        register_var/3,
        register_var/4,
        register_var_0/4,
        remove_grounds/2,
        renumbervars1/2,
        renumbervars1/4,
        samify/2,
        scan_file_preds/0,
        scan_file_preds/1,
        scan_file_preds/6,
        some_flocation/3,
        some_location/3,
        term_to_string/2,
        to_fa/3,
        unnumbervars/2,
        unnumbervars_and_save/2,
        var_to_name/3,
        write_modules/0.


:- meta_predicate snumbervars(?,?,?,?).
:- meta_predicate snumbervars(?,?,?).
:- meta_predicate safe_numbervars(0).
:- meta_predicate contains_singletons(0).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (0.206 sec)
:- meta_predicate renumbervars(?,?).
:- meta_predicate randomVars(?).
:- meta_predicate snumbervars(?).
% Restarting analysis ...
% Found new meta-predicates in iteration 3 (0.121 sec)
:- meta_predicate programmer_error(0).
:- meta_predicate safe_numbervars(*,0).


:- thread_local(tlbugger:tl_always_show_dmsg).
always_show_dmsg:- thread_self(main).
always_show_dmsg:- tlbugger:tl_always_show_dmsg.

:- export(tlbugger:ifHideTrace/0).
:- thread_local(tlbugger:ifHideTrace/0).

:- dynamic(lmconfig:mpred_is_impl_file/1).
:- multifile(lmconfig:mpred_is_impl_file/1).
:- volatile(lmconfig:mpred_is_impl_file/1).

:- if(false).
:- else.
:- include(logicmoo_util_header).
:- endif.

write_modules:- forall(lmconfig:mpred_is_impl_file(F),(scan_file_preds(F),list_file_preds(F))).

term_to_string(IS,I):- on_x_fail(term_string(IS,I)),!.
term_to_string(I,IS):- on_x_fail(string_to_atom(IS,I)),!.
term_to_string(I,IS):- grtrace(term_to_atom(I,A)),string_to_atom(IS,A),!.


make_module_name(P,M):-file_base_name(P,F),file_name_extension(M,_Ext,F).
helper_name0(F):- atom_chars(F,Chars),append(_,[U,N],Chars), ( char_type(U,digit) ;  char_type(N,digit)), !.
helper_name(F):-fail,helper_name0(F).
portray_clause_pi(_,[]):-!.
% portray_clause_pi(T,List):- list_to_conjuncts(List,E), P=..[T,E],portray_clause( (:-P )),!.
portray_clause_pi(T,LIST0):-list_to_set(LIST0,LIST),list_to_conjuncts(LIST,E), portray_clause( (T :- E )),!. % portray_clause_pi(T,L).

to_fa(P,_,A):-var(P),!,integer(A).
to_fa(_-FA,F,A):-!,to_fa(FA,F,A).
to_fa(_:FA,F,A):-!,to_fa(FA,F,A).
to_fa(F/A,F,A):-!.
to_fa(P,F,A):-functor(P,F,A).


functor_compare(R,P1,P2):-to_fa(P1,F1,A1),to_fa(P2,F2,A2),compare(FR,F1,F2),(FR\==(=)->R=FR;compare(R,A1,A2)).


list_file_preds:- source_location(S,_),list_file_preds(S).
list_file_preds(S):- make_module_name(S,MN),list_file_preds(S,MN).
list_file_preds(S,MN):-
 must_det_l((
   findall(M:P-F/A,(mpred_source_file_0(M:P,S),functor(P,F,A)),List),predsort(functor_compare,List,Set),
   findall(F/A,(member(M:P-F/A,Set),\+ predicate_property(M:P,multifile),\+ helper_name(F)),Exports),
   findall(F/A,(member(M:P-F/A,Set),predicate_property(M:P,multifile)),Multifile),
   format('~N~n~n% File: ~w ~n',[S]),
   findall(MP,   (member(M:P-F/A,Set),predicate_property(M:P,meta_predicate(MP))),MPList),
   findall(F/A,(member(M:P-F/A,Set),predicate_property(M:P,transparent),\+ predicate_property(M:P,meta_predicate(_))),Transparent),
   findall(F/A,(member(M:P-F/A,Set),predicate_property(M:P,thread_local)),ThreadLocal),
   findall(F/A,(member(M:P-F/A,Set),predicate_property(M:P,dynamic),\+ predicate_property(M:P,thread_local)),Dynamic),   
   findall(F/A,(member(M:P-F/A,Set),predicate_property(M:P,volatile)),Volatile),
   
   portray_clause( :- module(MN,Exports)),
   portray_clause_pi( multifile,Multifile),
   portray_clause_pi( meta_predicate,MPList),
   portray_clause_pi( module_transparent,Transparent),
   portray_clause_pi( thread_local,ThreadLocal),
   portray_clause_pi( dynamic,Dynamic),
   portray_clause_pi( volatile,Volatile))),!.



:-export(module_meta_transparent/1).
% = :- meta_predicate(module_meta_transparent(:)).
module_meta_transparent(M:F/A):-functor(P,F,A),!,module_meta_transparent(M:P).
module_meta_transparent(M:P):-predicate_property(M:P,meta_predicate(_)),!.
module_meta_transparent(M:P):-predicate_property(M:P,transparent),!.
module_meta_transparent(M:P):-functor(P,F,A),module_transparent(M:F/A),!. % ground(P),M:meta_predicate(P),!.
% module_meta_transparent(M:P):-P=..[_|Args],maplist('='(?),Args),module_meta_transparent(M:P).
module_meta_transparent(_).

mpred_source_file(M:P,S):-no_repeats(mpred_source_file_0(M:P,S)),once((to_fa(P,F,A),make_module_name(S,MN),assert_if_new(lmconfig:sf_known(S,F,A,MN)))).
mpred_source_file_0(M:P,S):-mpred_source_file_1(M:P,S).
mpred_source_file_1(M:P,S):-predicate_property(M:P,file(S)).
mpred_source_file_1(M:P,S):-source_file(M:P,S).

m_cp(M,F,A):-no_repeats(M:F/A,((functor(P,F,A),current_predicate(_,M:P)))),\+ predicate_property(M:P,imported_from(_)).

:-dynamic(lmconfig:sf_known/4).
no_location(M,F,A):-m_cp(M,F,A),\+ lmconfig:sf_known(_S,F,A,_MN).

some_location(M,F,A):-no_repeats(F/A,(( m_cp(M,F,A); lmconfig:sf_known(_S,F,A,_MN)))).
some_flocation(MN,F,A):-no_repeats(F/A,(( lmconfig:sf_known(_S,F,A,MN);m_cp(MN,F,A)))).


:- module_meta_transparent(scan_file_preds/1).
:- export(scan_file_preds/0).
scan_file_preds:- source_location(S,_),scan_file_preds(S),!.
:- export(scan_file_preds/1).
scan_file_preds(_):- current_prolog_flag(xref,true),!.
scan_file_preds(S):- context_module(NotUser),must(NotUser\==user),forall(mpred_source_file(M:P,S),(functor(P,F,A),scan_file_preds(NotUser,S,M,P,F,A))).

:- style_check(-singleton).
scan_file_preds(NotUser,S,M,P,F,A):- M==user,!,show_call(scan_file_preds(NotUser,S,NotUser,P,F,A)).
scan_file_preds(NotUser,S,M,P,F,A):- \+ helper_name(F), export(M:F/A), fail.
scan_file_preds(NotUser,S,M,P,F,A):- export(M:F/A), fail. % export anyways
scan_file_preds(NotUser,S,M,P,F,A):- module_transparent(M:F/A), fail. % export anyways
scan_file_preds(NotUser,S,M,P,F,A):- module_meta_transparent(M:F/A),!.
scan_file_preds(NotUser,S,M,P,F,A).
:- style_check(+singleton).

:- module_meta_transparent(export_module_preds/0).
:- export(export_module_preds/0).
export_module_preds:- current_prolog_flag(xref,true),!.
export_module_preds:- context_module(M),source_file_property(S,module(M)),scan_file_preds(S),forall(source_file_property(S,includes(F,_)),scan_file_preds(F)).


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



:- use_module(logicmoo_util_varnames).

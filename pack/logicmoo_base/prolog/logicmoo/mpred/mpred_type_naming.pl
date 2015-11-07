% =======================================================
/* 
% This Naming System is mainly used by the mpred_loader but also needed everywhere
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_naming.pl
:- module(mpred_type_naming,
          [ convertOneSpawnArg/4,
            convertSpawnArgs/4,
            convertToInstance/3,
            createByNameMangle/3,
            createByNameMangle0/3,
            createByNameMangle_compound/3,
            create_from_type/3,
            create_meta/4,
            get_source_suffix/1,
            i_name/2,
            i_name/3,
            i_name_lc/2,
            modality/3,
            onSpawn/1,
            onSpawn_0/2,
            onSpawn_f_args/3,
            spawnOneSpawnArg/4,
            split_name_type/3,
            split_name_type_0/3,
            toCamelAtom0/2,
            toUpperCamelcase/2,
            to_atomic_name/3,
            to_iname/2,
            to_prefixed/3,
            typename_to_iname0/3
          ]).
% :- use_module(logicmoo(util/logicmoo_util_preddefs)).


:- include('mpred_header.pi').

:- dynamic(lmconf:current_source_suffix/1).

% ================================================
% Naming System
% ================================================
:- was_export(create_meta/4).
% if SuggestedName was 'food666' it'd like the SuggestedClass to be 'food' and the stystem name will remain 'food666'
% if SuggestedName was 'food' it'd like the SuggestedClass to be 'food' and the stystem name will become a gensym like 'food1'

%= 	 	 

%% create_meta( ?SuggestedName, ?SuggestedClass, ?BaseClass, ?SystemName) is semidet.
%
% Create Meta.
%
create_meta(SuggestedName,SuggestedClass,BaseClass,SystemName):-
   must_det(split_name_type(SuggestedName,SystemName,NewSuggestedClass)),
   ignore(SuggestedClass=NewSuggestedClass),   
   assert_subclass_safe(SuggestedClass,BaseClass),
   assert_subclass_safe(NewSuggestedClass,BaseClass),
   assert_isa_safe(SystemName,BaseClass),
   assert_isa_safe(SystemName,NewSuggestedClass),
   assert_isa_safe(SystemName,SuggestedClass).



:- was_export(i_name_lc/2).

%= 	 	 

%% i_name_lc( ?OType, ?IType) is semidet.
%
% Instance Name Not Loop Checked.
%
i_name_lc(OType,IType):-typename_to_iname0('',OType,IOType),!,string_equal_ci(IOType,IType).



%= 	 	 

%% to_iname( ?T, ?T) is semidet.
%
% Converted To Iname.
%
to_iname(T,T):-!.
to_iname(T,TT):-not(current_predicate(i_name/3)),!,T=TT.
to_iname(T,TT):- (is_ftVar(T) -> TT=T; (not_log_op(T),i_name(t,T,TT))).



%= 	 	 

%% toUpperCamelcase( ?Type, ?TypeUC) is semidet.
%
% Converted To Upper Camelcase.
%
toUpperCamelcase(Type,TypeUC):-toCamelcase(Type,TypeUC). % ,toPropercase(TypeC,TypeUC),!.
:- was_export(i_name/2).

%= 	 	 

%% i_name( ?OType, ?IType) is semidet.
%
% Instance Name.
%
i_name(OType,IType):-typename_to_iname0('',OType,IOType),!,IOType=IType.
:- was_export(i_name/3).

%= 	 	 

%% i_name( ?I, ?OType, ?IType) is semidet.
%
% Instance Name.
%
i_name(I,OType,IType):-typename_to_iname0(I,OType,IOType),!,IOType=IType.

:- was_export(typename_to_iname0/3).


%= 	 	 

%% typename_to_iname0( ?I, ?OType, ?IType) is semidet.
%
% Typename Converted To Iname Primary Helper.
%
typename_to_iname0(I, [], O):- trace_or_throw(bad_typename_to_iname0(I, [], O)).
typename_to_iname0(I,OType,IType):-type_prefix(Prefix,_),atom_concat(Prefix,Type,OType),capitalized(Type),!,typename_to_iname0(I,Type,IType).
typename_to_iname0(I,Type,IType):-nonvar(Type),toUpperCamelcase(Type,UType),atom_concat(I,UType,IType).

:- was_export(split_name_type/3).
:- '$hide'(split_name_type/3).

%= 	 	 

%% split_name_type( ?Suggest, ?InstName, ?Type) is semidet.
%
% Split Name Type.
%
split_name_type(Suggest,InstName,Type):- must_det(split_name_type_0(Suggest,NewInstName,NewType)),!,must((NewInstName=InstName,NewType=Type)),!.


%= 	 	 

%% split_name_type_0( ?S, ?P, ?C) is semidet.
%
% split name type  Primary Helper.
%
split_name_type_0(S,P,C):- string(S),!,atom_string(A,S),split_name_type_0(A,P,C),!.
split_name_type_0(FT,FT,ttFormatType):-t(ttFormatType,FT),!,dmsg(trace_or_throw(ttFormatType(FT))),fail.
split_name_type_0(T,T,C):- compound(T),functor(T,C,_),!.
split_name_type_0(T,T,C):- hotrace((once(atomic_list_concat_safe([CO,'-'|_],T)),atom_string(C,CO))).
split_name_type_0(T,T,C):- hotrace((atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catch(number_codes(_,Digits),_,fail),atom_codes(CC,Type),!,i_name(t,CC,C))).
split_name_type_0(C,P,C):- var(P),atom(C),i_name(i,C,I),gensym(I,P),!.





%= 	 	 

%% toCamelAtom0( :TermA, ?O) is semidet.
%
% Converted To Camel Atom Primary Helper.
%
toCamelAtom0([A],O):-nonvar(A),!,toPropercase(A,O),!.
toCamelAtom0([A|List],O):-!,toPropercase(A,AO),toCamelAtom0(List,LO),atom_concat(AO,LO,O).
toCamelAtom0(A,O):-toPropercase(A,O),!.



%= 	 	 

%% to_prefixed( ?Prefix, ?I, ?O) is semidet.
%
% Converted To Prefixed.
%
to_prefixed(Prefix,I,O):-to_atomic_name(I,i_name(Prefix),O).

:- meta_predicate to_atomic_name(?,2,?).

%= 	 	 

%% to_atomic_name( ?I, :PRED2Pred, ?O) is semidet.
%
% Converted To Atomic Name.
%
to_atomic_name(I,Pred,O):-is_list(I),toCamelAtom0(I,A),!,to_atomic_name(A,Pred,O).
to_atomic_name(I,Pred,O):-string(I),!,string_to_atom(I,A),!,to_atomic_name(A,Pred,O).
to_atomic_name(Name,Pred,O):-atomic(Name),mudKeyword(W,KW),string_equal_ci(Name,KW),!,to_atomic_name(W,Pred,O).
to_atomic_name(Name,Pred,_):- not(atom(Name)),!,trace_or_throw(todo(not_atom_to_atomic_name(Name,Pred))).
to_atomic_name(Name,Pred,O):- call(Pred,Name,O).




%= 	 	 

%% createByNameMangle( ?Name, ?IDA, ?InstAO) is semidet.
%
% Create By Name Mangle.
%
createByNameMangle(Name,IDA,InstAO):-must(createByNameMangle0(Name,IDA,InstAO)),!.


%= 	 	 

%% createByNameMangle0( ?S, ?I, ?C) is semidet.
%
% Create By Name Mangle Primary Helper.
%
createByNameMangle0(S,I,C):-is_list(S),toCamelAtom0(S,A),!,createByNameMangle0(A,I,C).
createByNameMangle0(S,I,C):-string(S),!,string_to_atom(S,A),!,createByNameMangle0(A,I,C).
createByNameMangle0(OType,Name,Type):-compound(OType),!,must(createByNameMangle_compound(OType,Name,Type)),!.
createByNameMangle0(Name,_,_Type):- not(atom(Name)),!,trace_or_throw(todo(not_atom_createByNameMangle(Name))).
createByNameMangle0(OType,Name,Type):- isa_asserted(OType,tCol),!,create_from_type(OType,Name,Type).
createByNameMangle0(Suggest,Name,Type):- once(split_name_type(Suggest,Name,Type)),Suggest==Name,assert_isa(Name,Type).
createByNameMangle0(Name,I,C):-mudKeyword(W,KW),string_equal_ci(Name,KW),!,createByNameMangle0(W,I,C).
createByNameMangle0(OType,Name,Type):-create_from_type(OType,Name,Type),!.
createByNameMangle0(Name,IDA,Name):- gensym(Name,IDA), englishServerInterface([actCreate,Name,IDA]).


%= 	 	 

%% createByNameMangle_compound( ?Name, ?Name, ?Type) is semidet.
%
% Create By Name Mangle Compound.
%
createByNameMangle_compound(Name,Name,Type):- Name=..[Type|Props],assert_isa(Name,Type),w_tl(deduceArgTypes(_),padd(Name,Props)).
createByNameMangle_compound(Name,Inst,Type):- functor_catch(Name,Type,A),must(A==1),assert_isa(Name,Type),Name=Inst.


:- was_dynamic(lmconf:current_source_suffix/1).


%= 	 	 

%% get_source_suffix( ?SS) is semidet.
%
% Get Source Suffix.
%
get_source_suffix(SS):- lmconf:current_source_suffix(SS),!.
get_source_suffix('7').
%get_source_suffix(SS):- source_location(F,_),!,file_directory_name(F,DN),file_base_name(DN,SS),concat_atom(['-',SS,'7'],SSM),asserta_if_new(lmconf:current_source_suffix(SSM)).



%= 	 	 

%% create_from_type( ?OType, ?Name, ?Type) is semidet.
%
% Create Converted From Type.
%
create_from_type(OType,Name,Type):- sanity(var(Name)),
   i_name(OType,TypeWT),
   atom_concat('t',TypeWT,Type),
   get_source_suffix(SS),
   atom_concat(Type,SS,InstA7),!,
   i_name(i,InstA7,Name),
   must_det(assert_isa(Name,Type)),!.



% ========================================
% Spawn new instances
% ========================================



%= 	 	 

%% modality( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Modality.
%
modality(mpred_default, [usually],[]).
modality(~ , [cannot],[can]).
modality(mpred_default,[sometimes],[]).
modality(can,[can],[be]).
modality(possibly,[either],[]).
modality(~,[not],[]).
modality(~,[never],[]).



%= 	 	 

%% onSpawn( :TermA) is semidet.
%
% Whenever Spawn.
%
onSpawn(A):-A==true,!.
onSpawn((A,B)):-!,onSpawn(A),onSpawn(B).
onSpawn(ClassFact):-fully_expand(ClassFact,ClassFactO),!,onSpawn_0(t,ClassFactO).


%= 	 	 

%% onSpawn_0( ?Modality, ?ClassFact) is semidet.
%
% Whenever spawn  Primary Helper.
%
onSpawn_0(_Modality,ClassFact):- ClassFact=..[FunctArgType,Name],modality(FunctArgType,_,_),!,
 onSpawn_0(FunctArgType,Name).
   
onSpawn_0(Modality,ClassFact):- ClassFact=..[FunctArgType,Name],
 tCol(FunctArgType),
 createByNameMangle(Name,Inst,TypeA),
 assert_isa(TypeA,tCol),
 assert_isa(Inst,FunctArgType),
 assert_isa(Inst,TypeA),
 fully_expand(t(Modality,genls(TypeA,FunctArgType)),TO),
 ain(TO),!.

onSpawn_0(Modality,ClassFact):- ClassFact=..[Funct|InstADeclB],
  must_det(onSpawn_f_args(Modality,Funct,InstADeclB)).


%= 	 	 

%% onSpawn_f_args( ?Modality, ?Funct, ?List) is semidet.
%
% Whenever Spawn Functor Arguments.
%
onSpawn_f_args(Modality,Funct,List):-
  must(convertSpawnArgs(Funct,1,List,NewList)),
   Later =.. [Funct|NewList],
   fully_expand(t(Modality,Later),TO),
   ain(TO),!. 
  % call_after_mpred_load_slow(w_tl(deduceArgTypes(Funct), ain(Later))))),!.


%= 	 	 

%% convertSpawnArgs( ?Funct, ?N, :TermA, :TermO) is semidet.
%
% Convert Spawn Arguments.
%
convertSpawnArgs(_,_,[],[]).
convertSpawnArgs(Funct,N,[A|List],[O|NewList]):-
 must(convertOneSpawnArg(Funct,N,A,O)),!,
 N2 is N + 1,
 convertSpawnArgs(Funct,N2,List,NewList),!.


%= 	 	 

%% convertOneSpawnArg( ?VALUE1, ?VALUE2, ?O, ?O) is semidet.
%
% Convert One Spawn Argument.
%
convertOneSpawnArg(_,_,O,O):-string(O),!.
convertOneSpawnArg(_,_,O,O):-number(O),!.
convertOneSpawnArg(_,_,nospawn(O),O):-!.
convertOneSpawnArg(Funct,N,isInstFn(A),O):-spawnOneSpawnArg(Funct,N,A,O).
convertOneSpawnArg(Funct,N,A,O):-spawnOneSpawnArg(Funct,N,A,O).


%= 	 	 

%% spawnOneSpawnArg( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Spawn One Spawn Argument.
%
spawnOneSpawnArg(Funct,N,Name,Inst):- 
    must(argIsa(Funct,N,FunctArgType)),
    must(convertToInstance(Name,FunctArgType,Inst)),!.


%= 	 	 

%% convertToInstance( ?Name, ?FunctArgType, ?Inst) is semidet.
%
% Convert Converted To Instance.
%
convertToInstance(Name,FunctArgType,Inst):- isa(Name,FunctArgType),!,Inst=Name.
convertToInstance(Name,COLTHING,TypeA):- isa(COLTHING,ttTypeType),createByNameMangle(Name,_,TypeA),assert_isa(TypeA,COLTHING).
convertToInstance(Name,COLTHING,TypeA):- genls(COLTHING,tCol),createByNameMangle(Name,_,TypeA),assert_isa(TypeA,COLTHING).
convertToInstance(Name,FunctArgType,Inst):- createByNameMangle(Name,Inst,TypeA),
  %  assert_isa(Inst,FunctArgType),
    ain(genls(TypeA,FunctArgType)),!.

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).


% =======================================================
/** <module> 
% This Naming System is mainly used by the logicmoo_i_loader but also needed everywhere
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================



% ================================================
% Naming System
% ================================================
:-export(create_meta/4).
% if SuggestedName was 'food666' it'd like the SuggestedClass to be 'food' and the stystem name will remain 'food666'
% if SuggestedName was 'food' it'd like the SuggestedClass to be 'food' and the stystem name will become a gensym like 'food1'
create_meta(SuggestedName,SuggestedClass,BaseClass,SystemName):-
   must_det(split_name_type(SuggestedName,SystemName,NewSuggestedClass)),
   ignore(SuggestedClass=NewSuggestedClass),   
   assert_subclass_safe(SuggestedClass,BaseClass),
   assert_subclass_safe(NewSuggestedClass,BaseClass),
   assert_isa_safe(SystemName,BaseClass),
   assert_isa_safe(SystemName,NewSuggestedClass),
   assert_isa_safe(SystemName,SuggestedClass).



:-export(i_name_lc/2).
i_name_lc(OType,IType):-typename_to_iname0('',OType,IOType),!,string_equal_ci(IOType,IType).


to_iname(T,T):-!.
to_iname(T,TT):-not(current_predicate(i_name/3)),!,T=TT.
to_iname(T,TT):-is_ftVar(T)->TT=T;(not_log_op(T),i_name_lc(t,T,TT)).


toUpperCamelcase(Type,TypeUC):-toCamelcase(Type,TypeUC). % ,toPropercase(TypeC,TypeUC),!.
:-export(i_name/2).
i_name(OType,IType):-typename_to_iname0('',OType,IOType),!,IOType=IType.
:-export(i_name/3).
i_name(I,OType,IType):-typename_to_iname0(I,OType,IOType),!,IOType=IType.

:-export(typename_to_iname0/3).

typename_to_iname0(I, [], O):- trace_or_throw(bad_typename_to_iname0(I, [], O)).
typename_to_iname0(I,OType,IType):-type_prefix(Prefix,_),atom_concat(Prefix,Type,OType),capitalized(Type),!,typename_to_iname0(I,Type,IType).
typename_to_iname0(I,Type,IType):-nonvar(Type),toUpperCamelcase(Type,UType),atom_concat(I,UType,IType).

:-export(split_name_type/3).
:- '$hide'(split_name_type/3).
split_name_type(Suggest,InstName,Type):- must_det(split_name_type_0(Suggest,NewInstName,NewType)),!,must((NewInstName=InstName,NewType=Type)),!.

split_name_type_0(S,P,C):- string(S),!,atom_string(A,S),split_name_type_0(A,P,C),!.
split_name_type_0(FT,FT,ttFormatType):-t(ttFormatType,FT),!,dmsg(trace_or_throw(ttFormatType(FT))),fail.
split_name_type_0(T,T,C):- compound(T),functor(T,C,_),!.
split_name_type_0(T,T,C):- hotrace((once(atomic_list_concat_safe([CO,'-'|_],T)),atom_string(C,CO))).
split_name_type_0(T,T,C):- hotrace((atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catch(number_codes(_,Digits),_,fail),atom_codes(CC,Type),!,i_name(t,CC,C))).
split_name_type_0(C,P,C):- var(P),atom(C),i_name(i,C,I),gensym(I,P),!.




toCamelAtom0([A],O):-nonvar(A),!,toPropercase(A,O),!.
toCamelAtom0([A|List],O):-!,toPropercase(A,AO),toCamelAtom0(List,LO),atom_concat(AO,LO,O).
toCamelAtom0(A,O):-toPropercase(A,O),!.


to_prefixed(Prefix,I,O):-to_atomic_name(I,i_name(Prefix),O).

to_atomic_name(I,Pred,O):-is_list(I),toCamelAtom0(I,A),!,to_atomic_name(A,Pred,O).
to_atomic_name(I,Pred,O):-string(I),!,string_to_atom(I,A),!,to_atomic_name(A,Pred,O).
to_atomic_name(Name,Pred,O):-atomic(Name),mudKeyword(W,KW),string_equal_ci(Name,KW),!,to_atomic_name(W,Pred,O).
to_atomic_name(Name,Pred,_):- not(atom(Name)),!,trace_or_throw(todo(not_atom_to_atomic_name(Name,Pred))).
to_atomic_name(Name,Pred,O):- call(Pred,Name,O).



createByNameMangle(Name,IDA,InstAO):-must(createByNameMangle0(Name,IDA,InstAO)),!.

createByNameMangle0(S,I,C):-is_list(S),toCamelAtom0(S,A),!,createByNameMangle0(A,I,C).
createByNameMangle0(S,I,C):-string(S),!,string_to_atom(S,A),!,createByNameMangle0(A,I,C).
createByNameMangle0(OType,Name,Type):-compound(OType),!,must(createByNameMangle_compound(OType,Name,Type)),!.
createByNameMangle0(Name,_,_Type):- not(atom(Name)),!,trace_or_throw(todo(not_atom_createByNameMangle(Name))).
createByNameMangle0(OType,Name,Type):- isa_asserted(OType,tCol),!,create_from_type(OType,Name,Type).
createByNameMangle0(Suggest,Name,Type):- once(split_name_type(Suggest,Name,Type)),Suggest==Name,assert_isa(Name,Type).
createByNameMangle0(Name,I,C):-mudKeyword(W,KW),string_equal_ci(Name,KW),!,createByNameMangle0(W,I,C).
createByNameMangle0(OType,Name,Type):-create_from_type(OType,Name,Type),!.
createByNameMangle0(Name,IDA,Name):- gensym(Name,IDA), englishServerInterface([actCreate,Name,IDA]).

createByNameMangle_compound(Name,Name,Type):- Name=..[Type|Props],assert_isa(Name,Type),with_assertions(deduceArgTypes(_),padd(Name,Props)).
createByNameMangle_compound(Name,Inst,Type):- functor_catch(Name,Type,A),must(A==1),assert_isa(Name,Type),Name=Inst.


:-dynamic(thglobal:current_source_suffix/1).

get_source_suffix(SS):- thglobal:current_source_suffix(SS),!.
get_source_suffix('7').
%get_source_suffix(SS):- source_location(F,_),!,file_directory_name(F,DN),file_base_name(DN,SS),concat_atom(['-',SS,'7'],SSM),asserta_if_new(thglobal:current_source_suffix(SSM)).


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

onSpawn(A):-A==true,!.
onSpawn((A,B)):-!,onSpawn(A),onSpawn(B).
onSpawn(ClassFact):-fully_expand(ClassFact,ClassFactO),!,onSpawn_0(t,ClassFactO).

onSpawn_0(_Modality,ClassFact):- ClassFact=..[FunctArgType,Name],modality(FunctArgType,_,_),!,
 onSpawn_0(FunctArgType,Name).
   
onSpawn_0(Modality,ClassFact):- ClassFact=..[FunctArgType,Name],
 tCol(FunctArgType),
 createByNameMangle(Name,Inst,TypeA),
 assert_isa(TypeA,tCol),
 assert_isa(Inst,FunctArgType),
 assert_isa(Inst,TypeA),
 fully_expand(t(Modality,genls(TypeA,FunctArgType)),TO),
 add(TO),!.

onSpawn_0(Modality,ClassFact):- ClassFact=..[Funct|InstADeclB],
  must_det(onSpawn_f_args(Modality,Funct,InstADeclB)).

onSpawn_f_args(Modality,Funct,List):-
  must(convertSpawnArgs(Funct,1,List,NewList)),
   Later =.. [Funct|NewList],
   fully_expand(t(Modality,Later),TO),
   add(TO),!. 
  % call_after_mpred_load_slow(with_assertions(deduceArgTypes(Funct), add(Later))))),!.

convertSpawnArgs(_,_,[],[]).
convertSpawnArgs(Funct,N,[A|List],[O|NewList]):-
 must(convertOneSpawnArg(Funct,N,A,O)),!,
 N2 is N + 1,
 convertSpawnArgs(Funct,N2,List,NewList),!.

convertOneSpawnArg(_,_,O,O):-string(O),!.
convertOneSpawnArg(_,_,O,O):-number(O),!.
convertOneSpawnArg(_,_,nospawn(O),O):-!.
convertOneSpawnArg(Funct,N,isInstFn(A),O):-spawnOneSpawnArg(Funct,N,A,O).
convertOneSpawnArg(Funct,N,A,O):-spawnOneSpawnArg(Funct,N,A,O).

spawnOneSpawnArg(Funct,N,Name,Inst):- 
    must(argIsa(Funct,N,FunctArgType)),
    must(convertToInstance(Name,FunctArgType,Inst)),!.

convertToInstance(Name,FunctArgType,Inst):- isa(Name,FunctArgType),!,Inst=Name.
convertToInstance(Name,COLTHING,TypeA):- isa(COLTHING,ttTypeType),createByNameMangle(Name,_,TypeA),assert_isa(TypeA,COLTHING).
convertToInstance(Name,COLTHING,TypeA):- genls(COLTHING,tCol),createByNameMangle(Name,_,TypeA),assert_isa(TypeA,COLTHING).
convertToInstance(Name,FunctArgType,Inst):- createByNameMangle(Name,Inst,TypeA),
  %  assert_isa(Inst,FunctArgType),
    add(genls(TypeA,FunctArgType)),!.



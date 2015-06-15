% ===================================================================
% File 'parser_stanford.pl'
% Purpose: English to KIF conversions from SWI-Prolog  
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_stanford.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:-module(parser_stanford,[

         ]).

:-dynamic(tag_pos/2).
:-dynamic(tag_pos/3).
:-dynamic(get_pos_tagger/1).

% end_of_file.

atomic_subst(Before,Find,Replace,After):- atomic_list_concat(Atoms,Find,Before),atomic_list_concat(Atoms,Replace,After).

%:- setenv('CLASSPATH','/devel/LogicmooDeveloperFramework/PrologMUD/runtime/stanford-corenlp/*:/devel/LogicmooDeveloperFramework/PrologMUD/runtime/stanford-corenlp/classes:.').
%:- setenv('CLASSPATH','.:/opt/PrologMUD/runtime/stanford-parser-full-2014-08-27:/opt/PrologMUD/runtime/stanford-parser-full-2014-08-27/stanford-postagger.jar:/opt/PrologMUD/runtime/stanford-parser-full-2014-08-27/stanford-srpser-2014-08-28-models.jar:/opt/PrologMUD/runtime/stanford-parser-full-2014-08-27/stanford-parser-3.4.1-models.jar:/opt/PrologMUD/runtime/stanford-parser-full-2014-08-27/stanford-parser.jar').
:- use_module(library(jpl)).
:- jpl_set_default_jvm_opts(['-Xmx5G']).

:-if(\+ current_predicate(must/1)).
must(G):- G *-> true; (trace,G).
:-endif.

:-dynamic(pos_tagger/1).

get_pos_tagger(I):-pos_tagger(I)->true;(jpl_new(class([],['POSTaggerParser']),[],I),jpl_call(I,'init',[],@void),asserta(pos_tagger(I))).

call_pos_tagger(Call,Args,Out):- get_pos_tagger(I),jpl_call(I,Call,Args,Out).

unwrap_functor([],O,O).
unwrap_functor([F/A-Arg|Rest],I,O):-unwrap_functor_0(F/A,Arg,I,M),unwrap_functor(Rest,M,O).

unwrap_functor_0(F/A,Arg,I,M):-compound(I),functor(I,F,A),arg(Arg,I,M).
unwrap_functor_0(_,_,I,I).

% pos_tagger_test(Out),arg(3,Out,A).

s_to_sin(S,SIn):-
  string_to_atom(S,SIn),!.

s_to_sin(S,SIn):-
  string_to_atom(S,In),
   atomic_subst(In,' here, ',' here and is ',SIn0),
   atomic_subst(SIn0,'\'re ',' are ',SIn1),
   atomic_subst(SIn1,'n\'t ',' not ',SIn2),
   atomic_subst(SIn2,', ',' and ',SIn),!.

tag_pos(S,OO):-tag_pos(S,OO,_).
tag_pos(S,OO,OO2) :- 
  s_to_sin(S,SIn),
  call_pos_tagger(tagPOS, [SIn],O),
   j_get(O,[f(1),j_to_term],Out),once(unwrap_functor(['ROOT'/1-1,'S'/1-1],Out,OO)),
   j_get(O,[f(0),j_to_term],Out2),once(unwrap_functor(['ROOT'/1-1,'S'/1-1],Out2,OO2)).

typedDependencies(S,OO) :- 
  s_to_sin(S,SIn),
  call_pos_tagger(tagPOS, [SIn],O),
   j_get(O,[+j_to_term],Out),once(unwrap_functor(['ROOT'/1-1,'S'/1-1],Out,OO)).

annotateSentence(S,OO) :- 
  s_to_sin(S,SIn),
  call_pos_tagger(annotateSentence, [SIn],O),
   j_get(O,[j_to_term],Out),once(unwrap_functor(['ROOT'/1-1,'S'/1-1],Out,OO)).

spall(S,a_t(A,B)) :- typedDependencies(S,B),annotateSentence(S,A),!.
spall(S):-spall(S,O),portray_clause((S:-O)).

% jpl_call('java.lang.System',getProperty,['java.specification.version'],O).
pos_tagger_test(Out) :- tag_pos('Lieutenant Worf is here, looking pretty mean.',Out),writeq(Out).
pos_tagger_test(Out) :- typedDependencies('Lieutenant Worf is here, looking pretty mean.',Out),writeq(Out).
pos_tagger_test(Out) :- spall('The strongest rain ever recorded in India shut down the financial hub of Mumbai, snapped communication lines, closed airports and forced thousands of people to sleep in their offices or walk home during the night, officials said today.',Out),portray_clause(c:-Out).

% pos_tagger_test :- call_pos_tagger(tagPOS(class([java,lang],['String'])), ["The check is in your mouth. "],Out),writeq(Out).



j_get_l(E,I,O):-must(j_get(I,E,O)),!.

j_is_term(I,T):- ground(T),!,must(jpl_type_to_class(T,SC)),jpl_call(SC,isInstance,[I],IS),IS='@'(true).
j_is_term(I,T):- jpl_call(I,getClass,[],C),j_subclasses(C,TT),jpl_class_to_type(TT,Type),T=Type,!.

j_subclasses(C,C).
j_subclasses(C,TO):-jpl_call(C,getSuperclass,[],T),j_subclasses(T,TO).
j_subclasses(C,TE):-jpl_call(C,getInterfaces,[],T),jpl_array_to_list(T,L),member(E,L),j_subclasses(E,TE).


j_to_term_until_done(I,O):- j_to_term(I, M),(I\=@=M -> j_to_term_until_done(M, O) ; M=O).

j_to_term(I, O):- var(I),!,must(I=O).
j_to_term(I, O):- is_list(I),!,must(maplist(j_to_term,I,O)).
j_to_term(I, O):- jpl_is_object(I),!,must(jo_to_term(I, O)).
j_to_term(I, O):- compound(I),I=..[F|IA],!,must((maplist(j_to_term,IA,OA),O=..[F|OA])).
j_to_term(I, O):- must(I=O).


jo_to_term(I,OO):- jconvert(T,How),j_is_term(I,T),!,must(j_get(I,How,O)),j_to_term(O,OO).
jo_to_term(I, O):- catch(jpl_enumeration_to_list(I,M),_,fail),!,j_to_term(M,O).
jo_to_term(I, O):- catch(jpl_array_to_list(I,M),_,fail),!,j_to_term(M,O).
jo_to_term(I, O):- catch(jpl_map_element(I,M),_,fail),!,j_to_term(M,O).
jo_to_term(I, O):- jpl_is_object(I),j_get(I,[getClass,isEnum],T),jpl_is_true(T),!,must(j_get(I,[toString],O)).
jo_to_term(I, O):- jpl_is_object(I),j_get(I,pa(obj,[m(getClass,[]),jo_to_term],m(toString,[])),O).
jo_to_term(I, O):- jpl_is_object(I),must(j_get(I,[toString],O)).
jo_to_term(IO,IO):-!.

jconvert(class([edu, stanford, nlp, util], ['IntTuple']),pa('IntTuple',elems)).
jconvert(class([edu, stanford, nlp, trees], ['UniversalEnglishGrammaticalStructure']),term(ugs)).
jconvert(class([edu,stanford,nlp,semgraph],['SemanticGraph']),pa(sg,typedDependencies)).
jconvert(class([edu,stanford,nlp,dcoref],['CorefChain','CorefMention']),+ pa('CorefMention',-mentionSpan,-headIndex,-corefClusterID,-position, + animacy,+ gender,+ number,+ mentionType)).
jconvert(class([edu,stanford,nlp,dcoref],['CorefChain']),pa('CorefChain', - getRepresentativeMention,- getMentionsInTextualOrder)).
jconvert(class([java,lang],['String']),[toString,revcall(string_to_atom)]).
jconvert(class([edu,stanford,nlp,ling],['CoreLabel']),pa(twin,m(tag,[]),m(word,[]),m(index,[]),m(ner,[]))).
jconvert(class([edu,stanford,nlp,ling],['IndexedWord']),pa(wi,m(tag,[]),m(word,[]),m(index,[]))).
jconvert(class([edu,stanford,nlp,ling],['TaggedWord']),fa(m(tag,[]),m(word,[]))).
jconvert(array(class([java,lang],['Object'])),[call(jpl_array_to_list)]).
jconvert(class([edu,stanford,nlp,trees],['Tree']),pl([label,toString],[children,call(jpl_array_to_list)],[call(j_to_term)])).
jconvert(class([edu,stanford,nlp,trees],['TypedDependency']),fa([m(reln,[]),getShortName],m(gov,[]),m(dep,[]))).
jconvert(class([java,util],['Map']),[entrySet]).
jconvert(class([java,util],['Map','Entry']),[pa(kv,+getKey,+getValue)]).
jconvert(class([java,util],['List']),[toArray,+call(jpl_array_to_list)]).
jconvert(class([java,lang],['Class']),[call(jpl_class_to_type)]).
jconvert(class([java,lang],['Integer']),[m(intValue)]).
jconvert(class([java,util],['Collection']),[toArray,+call(jpl_array_to_list)]).



j_get(IO,[],IO):-!.
j_get(I,-(E),(E=O)):-!,must((j_get(I,E,M),j_to_term_until_done(M,O))).
j_get(I,+(E),O):-!,must((j_get(I,E,M),j_to_term_until_done(M,O))).
j_get(I,+,O):-!,must(j_to_term_until_done(I,O)).
j_get(I,[E|L],O):-!,must((j_get(I,E,M),j_get(M,L,O))),!.
j_get(I,E,O):- compound(E),compound_name_arguments(E,fa,Args),!,must((maplist(j_get(I),Args,ArgsO),O=..ArgsO)).
j_get(I,E,O):- compound(E),compound_name_arguments(E,pa,[F|Args]),!,must((maplist(j_get(I),Args,ArgsO),O=..[F|ArgsO])).
j_get(I,pl(FunctorGet,ArgsGet,ArgFormat),O):- !, must(j_get(I,FunctorGet,Atom)),must(j_get(I,ArgsGet,List)),must(maplist(j_get_l(ArgFormat),List,ListO)),O=..[Atom|ListO],!.
j_get(I,f(E),O):- jpl_get(I,E,O),!.
j_get(_,term(E),E):-!.
j_get(I,m(N),O):- jpl_call(I,N,[],O),!.
j_get(I,m(N,Args),O):- jpl_call(I,N,Args,O),!.
j_get(I,call(C),O):-!, must(call(C,I,O)),!.
j_get(I,revcall(C),O):-!, must(call(C,O,I)),!.
j_get(I,E,O):- compound(E),compound_name_arguments(E,N,Args),catch(jpl_call(I,N,Args,O),_,fail),!.
j_get(I,j_to_term,O):- must(j_to_term(I,O)),!.
j_get(I,E,O):- catch(jpl_get(I,E,O),_,fail),!.
j_get(I,E,O):- catch(jpl_call(I,E,[],O),_,fail),!.
j_get(I,E,O):- catch(jpl_call(I,get,[E],O),_,fail),!.
j_get(I,E,O):- catch(jpl_call(I,getValue,[E],O),_,fail),!.
j_get(I,E,O):- catch(jpl_call(I,E,[I],O),_,fail),!.
j_get(I,E,O):- catch(call(E,I,O),_,fail),!.

% j_get(O,[f(0),children,jpl_array_to_list,0,children,jpl_array_to_list],E).

to_string(I):-jpl_get(class([java,lang],['System']),'out',Out),jpl_call(Out,println,[I],_).
% :- pos_tagger_test.


% ===================================================================

:-if(\+ current_predicate(isSlot/1)).
isSlot(Var):-var(Var),!.
isSlot('$VAR'(Var)):-number(Var).




pterm_to_sterm(VAR,VAR):-isNonCompound(VAR),!.
pterm_to_sterm([X|L],[Y|Ls]):-!,pterm_to_sterm(X,Y),pterm_to_sterm(L,Ls),!.
pterm_to_sterm(X,Y):-compound(X),X=..L,pterm_to_sterm(L,Y),!.
pterm_to_sterm(X,X).

:-endif.


end_of_file.

1.	CC	Coordinating conjunction
2.	CD	Cardinal number
3.	DT	Determiner
4.	EX	Existential there
5.	FW	Foreign word
6.	IN	Preposition or subordinating conjunction
7.	JJ	Adjective
8.	JJR	Adjective, comparative
9.	JJS	Adjective, superlative
10.	LS	List item marker
11.	MD	Modal
12.	NN	Noun, singular or mass
13.	NNS	Noun, plural
14.	NNP	Proper noun, singular
15.	NNPS	Proper noun, plural
16.	PDT	Predeterminer
17.	POS	Possessive ending
18.	PRP	Personal pronoun
19.	PRP$	Possessive pronoun
20.	RB	Adverb
21.	RBR	Adverb, comparative
22.	RBS	Adverb, superlative
23.	RP	Particle
24.	SYM	Symbol
25.	TO	to
26.	UH	Interjection
27.	VB	Verb, base form
28.	VBD	Verb, past tense
29.	VBG	Verb, gerund or present participle
30.	VBN	Verb, past participle
31.	VBP	Verb, non-3rd person singular present
32.	VBZ	Verb, 3rd person singular present
33.	WDT	Wh-determiner
34.	WP	Wh-pronoun
35.	WP$	Possessive wh-pronoun
36.	WRB	Wh-adverb
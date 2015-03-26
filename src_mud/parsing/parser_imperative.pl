/* <module>
% Imperitive Sentence Parser (using DCG)
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:-swi_module(parser_imperative, []).

:-export((
                   parse_agent_text_command/5,            
                   parse_agent_text_command_0/5,            
                   objects_match/3,
                   match_object/2,
                   object_string/2,
                   coerce/3)).
:-export((
                   parseIsa//2,
                   phrase_parseForTypes_9//2,
                   parseForTypes//2)).

:- include(logicmoo('vworld/moo_header.pl')).

% :- register_module_type (utility).

% =====================================================================================================================
% get_agent_text_command/4
% =====================================================================================================================
:-export(get_agent_text_command/4).

get_agent_text_command(Agent,VERBOrListIn,AgentR,CMD):-
   debugOnError(loop_check(get_agent_text_command_0(Agent,VERBOrListIn,AgentR,CMD),fail)).

get_agent_text_command_0(Agent,ListIn,AgentR,CMD):- 
   (is_list(ListIn) -> UseList=ListIn ; UseList=[ListIn]),
       call_no_cuts(user:agent_text_command(Agent,UseList,AgentR,CMD)).


% ===========================================================
% PARSE command
% ===========================================================
user:type_action_info(tHumanPlayer,actParse(ftProlog,ftListFn(ftTerm)),"Development test to parse some Text for a human.  Usage: parse 'item' the blue backpack").

user:agent_call_command(_Gent,actParse(Type,StringM)):-
   parse_for(Type,StringM,_Term,_LeftOver).

% ===========================================================
% CMDPARSE command
% ===========================================================
user:type_action_info(tHumanPlayer,actCmdparse(ftListFn(ftTerm)),"Development test to parse some Text for a human.  Usage: cmdparse take the blue backpack").

user:agent_call_command(_Gent,actCmdparse(StringM)):- parse_for(ftAction,StringM,Term,LeftOver),fmt('=>'(parse_for(StringM) , [Term,LeftOver])).

% mud_test("cmdparse test",...)
  

% ===========================================================
% parsetempl command
% ===========================================================
user:type_action_info(tHumanPlayer,actParsetempl(ftListFn(ftTerm)),"Development test to see what verb phrase heads are found. (uses get_vp_templates/4)  Usage: parsetempl who").

user:agent_text_command(Agent,[actParsetempl|List],Agent,actParsetempl(List)).

user:agent_call_command(Agent,actParsetempl(StringM)):-
  to_word_list(StringM,[SVERB|ARGS]),
  get_vp_templates(Agent,SVERB,ARGS,TEMPLATES),fmt(templates=TEMPLATES),
  ignore((
     parse_for(vp,StringM,Goal,LeftOver),
     fmt([goal=Goal,lfto=LeftOver]))).

% ===========================================================
% parse_for/2-N
% ===========================================================
parse_for(Type,StringM):- parse_for(Type,StringM, _Term).

parse_for(Type,StringM, Term):-parse_for(Type,StringM, Term, []).

list_tail(_,[]).
list_tail(String,LeftOver):-ground(String),to_word_list(String,List),length(List,L),!,between(1,L,X),length(LeftOver,X).

:-export(parse_for/4).
parse_for(Type,StringM,Term,LeftOver):-
   to_word_list(StringM,String),  
   list_tail(String,LeftOver),
   HOW = phrase(parseIsa(Type,Term),String,LeftOver),
   fmt('parsing with ~q ~n.',[HOW]),
   (debugOnError(HOW)->
      fmt('Success! parse \'~q\' "~q" = ~q   (leftover=~q) . ~n',[Type,String,Term,LeftOver]);
      fmt('No Success.~n',[])).

meets_desc_spec(T,_L):- term_to_atom(T,S0),string_to_atom(S0,A),atomic_list_concat_catch([_,_|_],'mudBareHandDa',A),!,fail.
meets_desc_spec(_,[]):-!.
meets_desc_spec(S,[DS|SL]):-!,meets_desc_spec(S,DS),meets_desc_spec(S,SL),!.
meets_desc_spec(S,From-To):-!, desc_len(S,Len),!, between(From,To,Len).
meets_desc_spec(_,_).

desc_len(S0,Region):- call(term_to_atom(S0,S)),
   atomic_list_concat_catch(Words,' ',S),length(Words,Ws),atomic_list_concat_catch(Sents,'.',S),length(Sents,Ss),Region is Ss+Ws,!.


:-export(objects_match_for_agent/3).
objects_match_for_agent(Agent,Text,ObjList):- objects_match_for_agent(Agent,Text,[mudPossess(Agent,isSelf),isSame(mudAtLoc),isSame(localityOfObject),tAgentGeneric,tItem,tRegion],ObjList).  
:-export(objects_match_for_agent/4).
objects_match_for_agent(Agent,Text,Match,ObjList):- objects_for_agent(Agent,isOneOf([text_means(Agent,Text,isSelf),isAnd([isOneOf(Match),match_object(Text,isSelf)])]),ObjList).  


text_means(Agent,Text,Agent):- equals_icase(Text,"self"),!.
text_means(Agent,Text,Loc):- equals_icase(Text,"here"),where_atloc(Agent,Loc).
text_means(Agent,Text,Region):- equals_icase(Text,"region"),where_atloc(Agent,Loc),locationToRegion(Loc,Region).
text_means(_Agent,_Text,_Value):-fail.

relates(Agent,Relation,Obj):-loop_check(relates_ilc(Agent,Relation,Obj),fail).
relates_ilc(Agent,Relation,Obj):-text_means(Agent,Relation,Obj),!.
relates_ilc(_    ,Relation,Obj):- atom(Relation),tCol(Relation),!,isa(Obj,Relation).
relates_ilc(_    ,Relation,Obj):-contains_var(Relation,isSelf),subst(Relation,isSelf,Obj,Call),!,req(Call).
relates_ilc(Agent,isSame(Relation),Obj):- !, relates(Agent,Relation,Value),relates(Obj,Relation,Value).
relates_ilc(Agent,Relation,Obj):- atom(Relation),!, prop(Agent,Relation,Obj).
relates_ilc(_    ,Relation,Obj):-contains_var(Relation,Obj),!,req(Relation).
relates_ilc(Agent,Relation,Obj):-contains_var(Relation,Agent),append_term(Relation,Obj,Call),!,req(Call).
relates_ilc(Agent,Relation,Obj):-objects_for_agent(Agent,Relation,MatchList),MatchList\=[],!,member(MatchList,Obj).


objects_for_agent(_Agent,isAnd([]),[]):-!.
objects_for_agent(_Agent,isMost([]),[]):-!.
objects_for_agent(_Agent,isOneOf([]),[]):-!.

objects_for_agent(Agent,isOneOf([Possible|Relations]),MatchList):-!,
   objects_for_agent(Agent,Possible,L1),
   objects_for_agent(Agent,Relations,L2),
   append(L1,L2,MatchListL),!,
   list_to_set(MatchListL,MatchList),!.

objects_for_agent(Agent,members(List,Relations),MatchList):-!, findall(Obj,(member(Obj,List),relates(Agent,Relations,Obj)),MatchListL),list_to_set(MatchListL,MatchList),!.

objects_for_agent(Agent,isAnd([Possible|Relations]),MatchList):-!,
   objects_for_agent(Agent,Possible,L1),
   objects_for_agent(Agent,members(L1,Relations),MatchListL),
   list_to_set(MatchListL,MatchList),!.
objects_for_agent(Agent,Relation,MatchList):- findall(Obj, relates(Agent,Relation,Obj), MatchListL),list_to_set(MatchListL,MatchList),!.


objects_match(Text,Possibles,MatchList):- findall(Obj,(member(Obj,Possibles),match_object(Text,Obj)), MatchList).


object_string(O,String):-object_string(_,O,1-4,String),!.
object_string_0_5(O,String):-object_string(_,O,0-5,String),!.

:-export(object_string/4).
:-dynamic object_string_fmt/3.
:-retractall(object_string_fmt(_,_,_)).
object_string(_,O,DescSpecs,String):- object_string_fmt(O,DescSpecs,String),!.
object_string(Agent,O,DescSpecs,String):- String = [O], 
   object_print_details(save_fmt(String),Agent,O,DescSpecs,[tCol,tItem,ftProlog,ftID,ftTerm,tAgentGeneric,tChannel]),
   asserta(object_string_fmt(O,DescSpecs,String)),!.

save_fmt(OS,' ~w ',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,'~w',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,'~w',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,Fmt,[A|KW]):-sformat(Str,Fmt,[A|KW]),to_word_list(Str,WL),save_fmt_e(OS,WL),!.

save_fmt_e(_,E):-var(E),!.
save_fmt_e(_,[]):-!.
save_fmt_e(O,A):-atom(A),!,save_fmt_a(O,A),!.
save_fmt_e(O,[E|L]):-!,save_fmt_e(O,E),!,save_fmt_e(O,L),!.
save_fmt_e(O,isa(A)):-!,must(save_fmt_e(O,A)).
save_fmt_e(O,t(A,_)):-!,must(save_fmt_e(O,A)).
save_fmt_e(_,E):-compound(E),!. % cycPred(_),predStub(_),cycPlus2(_),predStub(_),predModule(_),arity(_),
%save_fmt_e(O,E):- string(E),!,must((to_word_list(E,WL),save_fmt_e(O,WL))),!.
save_fmt_e(O,E):- identical_member(E,O) -> true ; (O=[_|CDR],nb_setarg(2,O,[E|CDR])).

save_fmt_a(_,E):-var(E),!.
save_fmt_a(O,t(E,_)):-!,save_fmt_a(O,E).
save_fmt_a(O,E):-compound(O),arg(1,O,E),!,save_fmt_a(O,E).
save_fmt_a(_,A):-atom_length(A,L),L =< 1.
save_fmt_a(_,A):-vtSkippedPrintNames(A),!.
save_fmt_a(O,E):-to_case_breaks(E,List),maplist(save_fmt_a(O),List).


object_name_is_descriptive(O):- (isa(O,tCol);isa(O,tPred);hasInstance(functorDeclares,O);isa(O,ttValueType),isa(O,name_is_descriptive)).

:-export(object_print_details/5).


object_print_details(Print,Agent,O,DescSpecs,Skipped):- atoms_of(O,OS),!,
   forall(member(M,OS),object_print_details0(Print,Agent,M,DescSpecs,Skipped)).

object_print_details0(Print,Agent,O,DescSpecs,Skipped):-
   member(O,Skipped) -> true ;
  (    
    ignore(forall(name_text(O,KW),ignore((meets_desc_spec(KW,DescSpecs)->call(Print,' ~w ',[KW]))))),
   (object_name_is_descriptive(O) -> true ; 
    (( 
       forall(is_asserted(descriptionHere(O,KW)),ignore((meets_desc_spec(KW,DescSpecs)->call(Print,' ~w ',[KW])))),
       forall(isa(O,S),ignore((not(vtSkippedPrintNames(S)),object_print_details0(Print,Agent,S,DescSpecs,[O|Skipped])))))))).

%:-decl_type(ttTypeType).
vtSkippedPrintNames(T):-var(T),!,fail.
vtSkippedPrintNames(T):-ttFormatType(T).
%vtSkippedPrintNames(T):-isa(T,ttTypeType).
vtSkippedPrintNames(E):-member(E,[tObj,isSelf,the,is,tSpatialThing,ttNotSpatialType,ttSpatialType,prologHybrid,dbase_t,prologPTTP,prologSNARK,prologOnly,tRelation,tPred,'',[]]).


must_make_object_string_list(_,Obj,WList):- object_string(Obj,WList),!.
must_make_object_string_list(P,Obj,WList):- call_tabled(must_make_object_string_list_cached(P,Obj,WList)).
must_make_object_string_list_cached(P,Obj,WList):-
  must((object_string(P,Obj,0-5,String),nonvar(String),non_empty(String),string_ci(String,LString),to_word_list(LString,WList))).

same_ci(A,B):-notrace((must((non_empty(A),non_empty(B))),any_to_string(A,StringA),any_to_string(B,StringB),!,string_ci(StringA,StringB))),!.

match_object(S,Obj):-name_text(Obj,S).
match_object(S,Obj):-ground(S:Obj),match_object_exp(S,Obj),!.


match_object_exp(S,Obj):-sanity(ground(S:Obj)),must(((atoms_of(S,Atoms),!,Atoms\=[]))),match_object_0(Atoms,Obj).

match_object_0([S],Obj):-nonvar(S),match_object_1(S,Obj),!.
match_object_0(Atoms,Obj):-
   current_agent_or_var(P),notrace(must_make_object_string_list(P,Obj,WList)),!,
   forall(member(A,Atoms),(member(W,WList),notrace(string_equal_ci(A,W)))).

match_object_1(A,Obj):-same_ci(A,Obj),!.
match_object_1(A,Obj):-notrace((isa(Obj,Type))),same_ci(A,Type),!.


dmsg_parserm(D):-dmsg(D),!.
dmsg_parserm(D):-ignore((debugging(parser),dmsg(D))).
dmsg_parserm(F,A):-ignore((debugging(parser),dmsg(F,A))).


% ===========================================================
% PARSER
% ===========================================================
:-debug(parser).
:-nodebug(parser).

must_atomic(A):-must(atomic(A)).

parse_agent_text_command(Agent,SVERB,[],NewAgent,GOAL):-compound(SVERB),!,must((NewAgent=Agent,GOAL=SVERB)),!.
parse_agent_text_command(_Agent,SVERB,ARGS,_,_):-slow_sanity((must(atomic(SVERB)),maplist(must_atomic,ARGS))),fail.

parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
  dmsg(parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL)),
  parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),
   dmsg_parserm(succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),!.

parse_agent_text_command(Agent,VERB,[PT2|ARGS],NewAgent,GOAL):-
   atomic_list_concat_catch([VERB,PT2],'_',SVERB),
   parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),!,
   dmsg_parserm(special_succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),!.

parse_agent_text_command(Agent,PROLOGTERM,[],Agent,actProlog(PROLOGTERM)):- must(nonvar(PROLOGTERM)),predicate_property(PROLOGTERM,_),!.

parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
 dmsg(failed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),
 debugging(parser),
 debug,visible(+all),leash(+all), dtrace,
 parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),!.

parse_agent_text_command(Agent,IVERB,ARGS,Agent,GOAL):- 
  ground(IVERB), string_to_atom(IVERB,VERB),GOAL=..[VERB|ARGS],!.

% try directly parsing first
parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(user:agent_text_command(Agent,[SVERB|ARGS],NewAgent,GOAL)),nonvar(NewAgent),nonvar(GOAL),!.   

% try indirectly parsing
parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(user:agent_text_command(Agent,[VERB|ARGS],NewAgent,GOAL)),ground(GOAL),nonvar(VERB),
   verb_matches(SVERB,VERB).

parse_agent_text_command_0(Agent,SVERB,ARGS,Agent,GOAL):-
   parse_agent_text_command_1(Agent,SVERB,ARGS,Agent,GOAL).

parse_agent_text_command_0(Agent,IVERB,ARGS,NewAgent,GOAL):-
   verb_alias_to_verb(IVERB,SVERB), IVERB\=SVERB,!,
   parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL).

parse_agent_text_command_0(Agent,PROLOGTERM,[],Agent,actProlog(mpred_call(PROLOGTERM))):- compound(PROLOGTERM),functor(PROLOGTERM,F,_),user:mpred_prop(F,_),!.
parse_agent_text_command_0(Agent,PROLOGTERM,[],Agent,actProlog(req(PROLOGTERM))):- compound(PROLOGTERM),is_callable(PROLOGTERM),!.

:-export(parse_agent_text_command_1/5).
% parses a verb phrase and retuns one interpretation (action)
parse_agent_text_command_1(Agent,SVERB,ARGS,Agent,GOAL):-

 
   parse_vp_real(Agent,SVERB,ARGS,GOALANDLEFTOVERS),
   dmsg_parserm(parserm("GOALANDLEFTOVERS"=GOALANDLEFTOVERS)),
   GOALANDLEFTOVERS \= [],
   must(chooseBestGoal(GOALANDLEFTOVERS,GOAL)),
   dmsg_parserm(parserm("chooseBestGoal"=GOAL)).


verb_alias('l',actLook).
verb_alias('lo',actLook).
verb_alias('s',actMove(vSouth)).
% verb_alias('go','go').
verb_alias('where is',actWhere).

% pos_word_formula('infinitive',Verb,Formula):- 'infinitive'(TheWord, Verb, _, _G183), 'verbSemTrans'(TheWord, 0, 'TransitiveNPCompFrame', Formula, _, _).

verb_alias_to_verb(IVERB,SVERB):- verb_alias(L,Look),verb_matches(L,IVERB),SVERB=Look,!.
verb_alias_to_verb(IVERB,SVERB):- coerce(IVERB,vtVerb,SVERB), IVERB \= SVERB.

subst_parser_vars(Agent,TYPEARGS,TYPEARGS_R):- subst(TYPEARGS,isSelfAgent,Agent,S1),where_atloc(Agent,Here),subst(S1,vHere,Here,TYPEARGS_R).

% verb_matches("go",VERB):-!,VERB=go.
verb_matches(SVERB,VERB):-samef(VERB,SVERB).
verb_matches(SVERB,VERB):-name_text(VERB,SVERB).

get_vp_templates(_Agent,SVERB,_ARGS,TEMPLATES):-
   findall([VERB|TYPEARGS],
    ((
      get_all_templates(TEMPL),
     %isa(Agent,What),
     %action_info(What,TEMPL,_),
     TEMPL=..[VERB|TYPEARGS],
     verb_matches(SVERB,VERB))),
     TEMPLATES_FA),
    % ( TEMPLATES_FA=[] -> (dmsg(noTemplates(Agent,SVERB,ARGS)),!,fail); true),
   predsort(mostIdiomatic,TEMPLATES_FA,TEMPLATES).
   
% parses a verb phrase and retuns multiple interps
parse_vp_real(Agent,SVERB,ARGS,Sorted):- with_assertions(thlocal:infSkipFullExpand,parse_vp_real_no_arg_checking(Agent,SVERB,ARGS,Sorted)).
parse_vp_real_no_arg_checking(Agent,SVERB,ARGS,Sorted):-
   get_vp_templates(Agent,SVERB,ARGS,TEMPLATES),   
   dmsg_parserm(("TEMPLATES"= (orig([SVERB|ARGS]) = TEMPLATES))),
   TEMPLATES \= [],
   findall(LeftOver-GOAL,
     (( 
      member([VERB|TYPEARGS],TEMPLATES),
      subst_parser_vars(Agent,TYPEARGS,TYPEARGS_R),
      subst_parser_vars(Agent,ARGS,ARGS_R),
      dmsg_parserm(("parseForTypes"=phrase_parseForTypes(TYPEARGS_R,ARGS_R,GOODARGS,LeftOver))),      
      phrase_parseForTypes(TYPEARGS_R,ARGS_R,GOODARGS,LeftOver),
      GOAL=..[VERB|GOODARGS])),
      GOALANDLEFTOVERS_FA),
   sort(GOALANDLEFTOVERS_FA,GOALANDLEFTOVERS),
   predsort(bestParse,GOALANDLEFTOVERS,Sorted),!.

chooseBestGoal([_LeftOver - GOAL],GOAL):-!.
chooseBestGoal(GOALANDLEFTOVERS,GOAL):-
   predsort(bestParse,GOALANDLEFTOVERS,Sorted),
   dmsg_parserm(("Sorted"=Sorted)),
   member(_LeftOver - GOAL,Sorted),!.

% mostIdiomatic(?Order, @Term1, @Term2)
mostIdiomatic(Order, Term1, Term2):-mostComplex(Order, Term1, Term2).
% mostComplex(?Order, @Term1, @Term2)
mostComplex(Order, Term1, Term2):-complexity_count(Term1,Complexity1),complexity_count(Term2,Complexity2),compare(Order,Complexity2,Complexity1),Order \== '=' ,!.
mostComplex(Order, Term1, Term2):-compare(Order,Term1,Term2).

complexity_count(S,-1):-var(S).
complexity_count(S,L):-string(S),!,string_length(S,L).
complexity_count(S,1):-atomic(S),!.
complexity_count([H|T],L):-!,complexity_count(H,HL),complexity_count(T,TL),L is HL+TL.
complexity_count(S,L):-functor(S,_,A),S=..[_|ARGS],!,complexity_count(ARGS,AL),L is A+AL.

% bestParse(?Order, @Term1, @Term2)
bestParse(Order,LeftOver1-GOAL1,LeftOver2-GOAL2):-
   length(LeftOver1,L1),length(LeftOver2,L2),
   functor_safe(GOAL1,_,A1),functor_safe(GOAL2,_,A2),
   must(once(bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2))).

:-style_check(-singleton).

bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,L1,L2), Order \== '='.
bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,-A1,-A2), Order \== '='.
bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,GOAL1,GOAL2).

:-style_check(+singleton).

:-export(name_text/2).
name_text(Name,Text):-nameStrings(Name,Text).
name_text(Name,Text):-mudKeyword(Name,Text).
name_text(Name,Text):-nonvar(Text),!,name_text(Name,TextS),equals_icase(Text,TextS).
% name_text(Name,Text):-argIsa(N,2,ftString),not_asserted((argIsa(N,1,ftString))),dbase_t(N,Name,Text).
name_text(Name,Text):-atomic(Name),!,name_text_atomic(Name,Text).
name_text(Name,Text):-is_list(Name),!,member(N,Name),name_text(N,Text).
name_text(Name,Text):-compound(Name),!,Name=..[F,A|List],!,name_text([F,A|List],Text).


name_text_atomic(Name,Text):-string(Name),Name=Text.
name_text_atomic(Name,Text):-to_case_breaks(Name,[_|ListN]),member(t(Text,_),ListN).
name_text_atomic(Name,Text):-i_name('',Name,TextN),atom_string(TextN,Text).
name_text_atomic(Name,Text):-atom_string(Name,Text).


user:hook_coerce(TextS,vtDirection,Dir):-
  member(Dir-Text,[vNorth-"n",vSouth-"s",vEast-"e",vWest-"w",vNE-"ne",vNW-"nw",vSE-"se",vSW-"sw",vUp-"u",vDown-"d"]),
  (name_text(Dir,TextS);TextS=Text;atom_string(TextS,Text)).

user:hook_coerce(Text,Subclass,X):- 
   not(memberchk(Subclass,[vtDirection,tSpatialThing])),
   once((isa_asserted(X,Subclass),
   arg_to_var(ftText,Text,TextVar),
   req(mudKeyword(X,TextVar)),   
   same_arg(ftText,TextVar,Text))). % dmsg(todo(user:hook_coerce(Text,Subclass))),impliedSubClass(Subclass,tSpatialThing).


phrase_parseForTypes(TYPEARGS,ARGS,GOODARGS,LeftOver):- % length(TYPEARGS,N),length(GOODARGS,N),!,
  to_word_list(ARGS,ARGSL),!,phrase_parseForTypes_0(TYPEARGS,ARGSL,GOODARGS,LeftOver).

string_append(A,[B1,B2],C,ABC):-append(A,[B1,B2|C],ABC).
string_append(A,[B],C,ABC):-append(A,[B|C],ABC).


is_counted_for_parse(I):-hasInstance(tCountable,I),not(excluded_in_parse(I)),!.

excluded_in_parse(apathFn(_, _)).
excluded_in_parse(I):-tCol(I).
excluded_in_parse(I):-ttFormatType(I).
excluded_in_parse(I):-user:mpred_prop(_,predArgTypes(I)).
excluded_in_parse(apathFn(_ = _)).

instance_for_parse(I):-is_counted_for_parse(I).
insttype_for_parse(I):-findall(C,(instance_for_parse(I),isa(I,C)),List),list_to_set(List,Set),member(I,Set).

optional_strings_opt.

% optimization for isOptional strings
phrase_parseForTypes_0(TYPEARGS,ARGS,GOODARGS,LeftOver):- optional_strings_opt,
      (string_append(T1,[isOptionalStr(Str)],T2,TYPEARGS),
      (StrT =[_] /*;StrT=[_,_]*/),
      string_append(A1,StrT,A2,ARGS),
      equals_icase(Str,StrT)),!,
      (phrase_parseForTypes_1(T1,A1,G1,[]),
         phrase_parseForTypes_9(T2,A2,G2,LeftOver),      
         string_append(G1,[Str],G2,GOODARGS)).
      
phrase_parseForTypes_0(TYPEARGS,ARGS,GOODARGS,LeftOver):-phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver).

phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver):- catchv(phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver),_,fail),!.    
phrase_parseForTypes_1(TYPEARGS,In,Out,[]):- length(TYPEARGS,L),between(1,4,L),length(In,L),must(Out=In),!,nop(fmt(fake_phrase_parseForTypes_l(foreach_isa(In,TYPEARGS)))),fail.
phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver):- debugOnError(phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver)).    

phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver):- (LeftOver=[];LeftOver=_ /*[_|_]*/), phrase(parseForTypes(TYPEARGS,GOODARGS),ARGS,LeftOver).

parseForTypes([], [], A, A).
parseForTypes([TYPE|TYPES], [B|E], C, G) :-
        no_repeats_old(parseIsa_Call(TYPE, B, C, F)),
        parseForTypes(TYPES, E, F, G),!.


parseIsa_Call(FT, BO, CIn, D):- list_tail(CIn,D), to_word_list(CIn,C),!, parseIsa(FT, B, C, D),to_arg_value(B,BO).


% this parseIsa(T)-->parseIsa(T,_).
parseIsa(A, B, C) :- parseIsa(A, _, B, C).

is_parsable_type(T):-ttFormatType(T).
is_parsable_type(T):-tCol(T).
is_parsable_type(vp).


%:- begin_tests(test_bad_verb).

mud_test(test_bad_verb, [ true(
       not(phrase(parseIsa(vtVerb,ff),[ff],[]))
       )] ).


mud_test(food_is_a_droppable, [ true(
       parse_agent_text_command(iPlayer1,actDrop,[food],_D2,_E2))]).


%:- end_tests(test_bad_verb).


query_trans_subft(FT,Sub):-subFormat(FT,Sub).
query_trans_subft(FT,Sub):-subFormat(FT,A),genls(A,Sub).
query_trans_subft(FT,Sub):-subFormat(FT,A),subFormat(A,B),subFormat(B,Sub).


parseFmt_vp1(Agent, do(NewAgent,Goal),[SVERB|ARGS],[]):- parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,Goal),!.
parseFmt_vp2(Agent,GOAL,[SVERB|ARGS],UNPARSED):- parse_vp_real(Agent,SVERB,ARGS,TRANSLATIONS),!,member(UNPARSED-GOAL,TRANSLATIONS).

to_arg_value(Var,Var):-is_ftVar(Var),!.
to_arg_value(vHere,Here):-must((current_agent(Who),where_atloc(Who,Here))).
to_arg_value(isSelfAgent,Who):-must((current_agent(Who))).
to_arg_value(isRandom(Type),Term):- nonvar(Type),!,must((to_arg_value(Type,TypeR),random_instance(TypeR,Term,true))).
to_arg_value(Call,TermO):-compound(Call),Call=..[call|CALLARGS],must((subst(CALLARGS,isSelf,Term,CALLARGS2),maplist(to_arg_value,CALLARGS2,CALLARGS3),NewCall=..[call|CALLARGS3],must(req(NewCall)),to_arg_value(Term,TermO))).
to_arg_value(Term,TermO):-must((map_term(to_arg_value,Term,TermO))).

map_term(Pred,Term,TermO):-var(Term),!,must(call(Pred,Term,TermO)).
map_term(Pred,Term,TermO):-is_list(Term),!,must(maplist(Pred,Term,TermO)).
map_term(Pred,Term,TermO):-compound(Term),Term=..TermL,!,must(maplist(Pred,TermL,TermOL)),TermO=..TermOL.
map_term(_,Term,Term):-!.

/*

some tests

 phrase_parseForTypes([isOptional(tAgentGeneric, isRandom(tAgentGeneric))], ['Crush'], A, B).

  phrase_parseForTypes([isOptional(tAgentGeneric, isRandom(tAgentGeneric))], ['Crush'], A, B).

parser_imperative:phrase_parseForTypes_9([isOptional(isAnd([obj, isNot(region)]), 'NpcCol1000-Geordi684'), isOptionalStr("to"), isOptional(region, isRandom(region))], [food, 'Turbolift'], GOODARGS,[]).
parser_imperative:phrase_parseForTypes_9([isOptional(isAnd([obj, isNot(region)]), 'NpcCol1000-Geordi684')], [food], GOODARGS,[]).
parser_imperative:phrase_parseForTypes_9([isOptional(region, isRandom(region))], ['Turbolift'], GOODARGS,[]).


*/

parseIsa(_T, _, [AT|_], _):- var(AT),!,fail.
parseIsa(FT, B, C, D):- var(FT),trace_or_throw(var_parseIsa(FT, B, C, D)).
parseIsa(Str,A,B,C) :-string(Str),!, parseIsa(exactStr(Str),A,B,C).

% this parseIsa(isNot(T),Term) --> dcgAnd(dcgNot(parseIsa(T)),theText(Term)).
parseIsa(isNot(Type), Term, C, D) :- !, dcgAnd(dcgNot(parseIsa(Type)), theText(Term), C, D).

parseIsa(vp,Goal,Left,Right):-!,one_must(parseFmt_vp1(isSelfAgent,Goal,Left,Right),parseFmt_vp2(isSelfAgent,Goal,Left,Right)).

parseIsa(dbase_t(P,S,O),TermV) -->{!},parseIsa(call(dbase_t(P,S,O)),TermV).
parseIsa(call(Call),TermV) --> {!,subst(Call,isSelf,TermV,NewCall)},theText(TermT), {req(NewCall),match_object(TermT,TermV)}.
parseIsa(exactStr(Str),Str) --> {!},[Atom],{equals_icase(Atom,Str),!}.
parseIsa(isOptionalStr(Str),Str) --> {not(optional_strings_opt)},[Atom],{equals_icase(Atom,Str),!}.
parseIsa(isOptionalStr(_),isMissing) --> {!},[].
parseIsa(isOptionalStr(_Str),_) --> {!,fail}.
parseIsa(isOptional(_,Term),TermV) --> {to_arg_value(Term,TermV)}, [TermT], {samef(TermV,TermT)}.
parseIsa(isOptional(Type, _), TermV, C, D) :- nonvar(Type),parseIsa(Type, TermV, C, D).
parseIsa(isOptional(_Type,Default), DefaultV, D, D2):- !,D=D2,to_arg_value(Default,DefaultV).

%  parser_imperative:phrase_parseForTypes_9([isOptional(isAnd([obj, isNot(region)]),'NpcCol1000-Geordi684'),isOptionalStr("to"),isOptional(region, isRandom(region))], [food], GOODARGS,[]).
%  parser_imperative:phrase_parseForTypes_9([isOptional(isAnd([obj, isNot(region)]),'NpcCol1000-Geordi684'),isOptionalStr("to"),isOptional(region, isRandom(region))], [food,to,'Turbolift'], GOODARGS,[]).
%  parser_imperative:phrase_parseForTypes_9([region], ['Turbolift'], GOODARGS,[]).


parseIsa(ftString,String)--> {!}, theString(String).
parseIsa(FT, B, [AT|C], D) :- nonvar(AT),member_ci(AT,["the","a","an"]),parseIsa(FT, B, C, D).

parseIsa(isOneOf(List),Term) --> {!,member(E,List)},parseIsa(E,Term).


parseIsa(ftListFn(Type),[Term|List]) --> parseIsa(Type,Term),parseIsa(ftListFn(Type),List).
parseIsa(ftListFn(_Type),[]) --> {!},[].

parseIsa(countBetween(_Type,_,High),[]) --> {High==0,!}, [].
parseIsa(countBetween(Type,Low,High),[Term|List]) --> parseIsa(Type,Term),{!,Low2 is Low -1,High2 is High -1 },
   parseIsa(countBetween(Type,Low2,High2),List).
parseIsa(countBetween(_Type,Low,_),[]) --> {!, Low < 1}, [].

% parseIsa(isAnd([L|List]),Term1) --> dcgAnd(parseIsa(L,Term1),parseIsa(isAnd(List),Term2)),{ignore(Term1==Term2),!}.
parseIsa(isAnd([L]),Term1) --> {!},parseIsa(L,Term1).
parseIsa(isAnd([L|List]),Term) --> {!},dcgAnd(parseIsa(L,Term),parseIsa(isAnd(List),Term)).

parseIsa(isMost(List),Term1) --> {!},parseIsaMost(List,Term1).


parseIsa(Type,Term)--> dcgAnd(dcgLenBetween(1,2),theText(String)),{coerce(String,Type,Term)}.


parseIsaMost(List,Term) --> parseIsa(isAnd(List),Term),{!}.
% parseIsaMost(A, B, C, D) :- parseIsa(isAnd(A), B, C, E), !, D=E.

coerce(A,B,C):-no_repeats(coerce0(A,B,C)),(show_call_failure(isa(C,B))->!;true).

coerce0(String,Type,Inst):- var(Type),trace_or_throw(var_specifiedItemType(String,Type,Inst)).
coerce0(String,isNot(Type),Inst):-!,not(coerce0(String,Type,Inst)).
coerce0([String],Type,Inst):- nonvar(String),!,coerce0(String,Type,Inst).
coerce0(String,Type,Inst):- atomic(String),Type==tCol,i_name('t',String,Inst),hasInstance(tCol,Inst),!.
coerce0(String,Type,Inst):- ttFormatType(Type),checkAnyType(change(assert,actParse),String,Type,AAA),Inst=AAA.
coerce0(Text,Type,Inst):- (no_repeats_old(call_no_cuts(hook_coerce(Text,Type,Inst)))).
%coerce0(String,Type,Longest) :- findall(Inst, (user:hook_coerce(Inst,Type,Inst),equals_icase(Inst,String)), Possibles), sort_by_strlen(Possibles,[Longest|_]),!.
coerce0(String,Type,Inst):- var(String),!,instances_of_type(Inst,Type),name_text(Inst,String).
coerce0(String,Type,Inst):- not(ttFormatType(Type)),must(tCol(Type)),instances_of_type(Inst,Type),match_object(String,Inst).
coerce0(String,Type,Inst):- not(string(String)),!,text_to_string(String,StringS),!,coerce0(StringS,Type,Inst).
% coerce0(A,Type,AA):- correctAnyType(change(_,_),A,Type,AA).

instances_of_type(Inst,Type):- no_repeats_old(instances_of_type_0(Inst,Type)).

available_instances_of_type(Agent,Obj,Type):- must(current_agent(Agent)), current_agent_or_var(Agent), isa(Obj,Type), mudDistance(Agent,Obj,D),D<6.

instances_of_type_0(Inst,Type):- instances_sortable(Type,HOW),!,get_sorted_instances(Inst,Type,HOW).
% should never need this but .. instances_of_type_0(Inst,Type):- genls(SubType,Type),isa(Inst,SubType).
instances_of_type_0(Inst,Type):- isa(Inst,Type).

instances_sortable(TYPE,HOW):-instances_sortable0(TYPE,HOW),!.
instances_sortable(TYPE,HOW):-genls(TYPE,SUPER),instances_sortable0(SUPER,HOW),!.
instances_sortable(_,distance_to_current_avatar(Agent)):-current_agent_or_var(Agent).

instances_sortable0(tWieldAble,distance_to_current_avatar(Agent)):-current_agent_or_var(Agent).
instances_sortable0(tWearAble,distance_to_current_avatar(Agent)):-current_agent_or_var(Agent).

distance_to_current_avatar(Agent,ORDEROUT,L,R):-mudDistance(Agent,L,L1),mudDistance(Agent,R,R1),compare(ORDER,L1,R1),!, (ORDER == '=' -> naming_order(ORDEROUT,L,R) ; ORDEROUT=ORDER).

mudDistance(Agent,_Obj,(-1)):- var(Agent),!.
mudDistance(Agent,Obj,0):- mudWielding(Agent,Obj),!.
mudDistance(Agent,Obj,1):- wearsClothing(Agent,Obj),!.
mudDistance(Agent,Obj,2):- mudStowing(Agent,Obj),!.
mudDistance(Agent,Obj,3):- mudPossess(Agent,Obj),!.
mudDistance(Agent,Obj,N):- mudPossess(OtherAgent,Obj),mudDistance(OtherAgent,Obj,AD),!,
  (same_regions(Agent,OtherAgent)->OAD=5; OAD=20),!,N is AD + OAD.
mudDistance(Agent,Obj,5):- same_regions(Agent,Obj),!.
mudDistance(_Agent,_Obj,20).

naming_order(ORDER,L,R):-compare(ORDER,L,R).

get_sorted_instances(Inst,Type,HOW):-findall(Inst,isa(Inst,Type),List),sort(List,NoDupes),predsort(HOW,NoDupes,Sorted),!,member(Inst,Sorted).

% instances_of_type(Inst,Type):- atom(Type), Term =..[Type,Inst], logOnError(req(Term)).
% longest_string(?Order, @Term1, @Term2)
/*
longest_string(Order,TStr1,TStr2):-
   text_to_string(TStr1,Str1),string_length(Str1,L1),
   text_to_string(TStr2,Str2),string_length(Str2,L2),
   compare(Order,L2-Str2,L1-Str1).
*/

:- include(logicmoo('vworld/moo_footer.pl')).

end_of_file.

text_isa(I,T):-no_repeats_old(hook_text_isa(I,T)).

hook_text_isa(Text,Whatnot):- no_repeats_old(tCol(Whatnot)),isa(Inst,Whatnot),not(tCol(Inst)),once(name_text(Inst,Text)).
hook_text_isa(Text,txtVerb):- get_all_templates(A),nonvar(A),functor_safe(A,Inst,_),name_text(Inst,Text).



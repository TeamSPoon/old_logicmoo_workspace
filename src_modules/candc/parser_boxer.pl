
:-module(user).

user:file_search_path(semlib,     logicmoo('candc/src/prolog/lib')).
user:file_search_path(boxer,      logicmoo('candc/src/prolog/boxer')).
user:file_search_path(knowledge,  logicmoo('candc/src/prolog/boxer/knowledge')).
user:file_search_path(lex,        logicmoo('candc/src/prolog/boxer/lex')).

:-ensure_loaded(boxer(boxer)).

e2lf(Sent):-
   e2lf(Sent,LF),
   fmt(lf=LF).

e2lf(English,LF):-not(is_list(English)),atomic(English),!,
   convert_members([replace_periods_string_list,to_list_of_sents],English,Sents),
   e2lf(Sents,LF),!. 
e2lf([],done([])).
e2lf([sent(WList)|MORE],and(LF,MORELF)):-!, e2lf(sent(WList),LF),e2lf(MORE,MORELF).
e2lf(sent(WList),'FakedLFParsedNowFn'(WList)):-!.
e2lf(English,LF):- !,e2lf(sent(English),LF).

:- e2lf("You find yourself standing by the door of Captain Picard's quarters.
He isn't very fond of visitors, but you decide to stay and have a look around . 
You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south .
A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table .
Two large windows offer a great view of space . 
A small partition at the northern part of the room contains Picard's sleeping area.").


% process_create 

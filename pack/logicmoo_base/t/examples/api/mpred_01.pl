% #!/usr/bin/env swipl

:- module(myMicrotheory,[]).

:- use_module(library(logicmoo_user)).

:- begin_pfc.


:- mpred_trace_exec.

:- ain(clif(exists(P,tPerson(P)))).

end_of_file.

:- set_user_abox(myMicrotheory).

:- import_to_user(genlMt/2).

t(genlMt,myMicrotheory,baseKB).

genlMt(myMicrotheory,baseKB).

:- mpred_trace_exec.

:-ain(clif(exists(P,tPerson(P)))).

% ?- pp_facts.



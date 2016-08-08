% #!/usr/bin/env swipl

:- module(myMicrotheory,[]).

:- ensure_loaded(library(logicmoo_base)).

:- begin_pfc.


:- mpred_trace_exec.

%:- ain(clif(exists(P,tPerson(P)))).

end_of_file.

:- set_defaultAssertMt(myMicrotheory).

:- kb_dynamic(genlMt/2).

t(genlMt,myMicrotheory,baseKB).

genlMt(myMicrotheory,baseKB).

:- mpred_trace_exec.

:-ain(clif(exists(P,tPerson(P)))).

% ?- pp_facts.



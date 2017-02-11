

:- mpred_unload_file.


:- file_begin(pfc).


pfcControlled(if_missing(ftAskable,ftAssertable)).

% this should have been ok
% (if_missing(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
if_missing(Missing,Create) ==> 
 ( ( \+ (Missing/(Missing\=@=Create))) ==> Create).

:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).

:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'sanity_foob.pfc')).

:- endif.

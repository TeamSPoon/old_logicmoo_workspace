

pfcControlled(if_missing(ftAskable,ftAssertable)).

% this should have been ok
% (if_missing(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ neg(Create)) ==> Create)).
(if_missing(Missing,Create) ==> ((\+ Missing/(Missing\==Create) ) ==> Create)).

:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'foob.pfc')).

:- endif.

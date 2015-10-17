

pfcControlled(if_missing(ftAskable,ftAssertable)).

% this should have been ok
% (if_missing(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ neg(Create)) ==> Create)).
(if_missing(Missing,Create) ==> ((\+ Missing/(Missing\==Create) ) ==> Create)).

:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

if_missing(foob(_),foob(a)).

:- mpred_test(foob(a)).

foob(b).

:- mpred_test(\+foob(a)).
:- mpred_test(foob(b)).

~foob(b).

:- mpred_test(\+foob(b)).
:- mpred_test(foob(a)).


:- endif.

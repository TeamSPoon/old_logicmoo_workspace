

:- use_module(library(logicmoo_utils)).
:- use_listing_vars.
:- cls.




% :- set_prolog_flag(umt_local,true).


% :- set_prolog_flag(umt_local,false).
 :- use_module(library(logicmoo/mpred/mpred_pfc)).
%:- use_module(library(logicmoo_base)).
% :- use_module(library(logicmoo_base)).
%:- use_module(library(pfc)).

:- use_module('mpred_pfc_test_02').

:- mpred_reset.


:- get_user_abox(M),dynamic((M:current_ooQQ/1,M:default_ooQQ/1,M:if_mooQQ/2)).
:- mpred_trace.
:- mpred_watch.
:- mpred_reset.


:- rtrace(mpred_ain(default_ooQQ(booQQ))).

:- must(call_u(default_ooQQ(booQQ))).

:- must((default_ooQQ(booQQ))).

:- pp_cur_why(default_ooQQ(booQQ)).

:- mpred_test(default_ooQQ(booQQ)).

:- mpred_ain(\+default_ooQQ(booQQ)).
% this should have been ok
% (if_mooQQ(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
:- ((mpred_ain((if_mooQQ(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create))))).

:- mpred_ain((default_ooQQ(X) ==> if_mooQQ(current_ooQQ(_),current_ooQQ(X)))).

:- mpred_ain(default_ooQQ(booQQ)).

:- mpred_test(current_ooQQ(booQQ)).
   
% :- pp_cur_DB.

:- (mpred_ain(current_ooQQ(fooQQ))).

:- mpred_test(\+current_ooQQ(booQQ)).

:- (mpred_ain(\+ current_ooQQ(fooQQ))).

:- mpred_test(current_ooQQ(booQQ)).

:- (mpred_withdraw( default_ooQQ(booQQ) )).

:- listing([current_ooQQ,default_ooQQ]).

:- mpred_test( \+current_ooQQ(booQQ)).

:- mpred_ain(~ current_ooQQ(fooQQ)).

% :- pp_cur_DB.

:- mpred_test(~current_ooQQ(fooQQ)).

:- mpred_ain(default_ooQQ(booQQ)).
 
:- mpred_test(current_ooQQ(booQQ)).


:- get_user_abox(M),dynamic((M:current_ooTt/1,M:default_ooTt/1,M:if_mooTt/2)).

:- mpred_trace.
:- mpred_watch.

% this should have been ok
% (if_mooTt(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
:- mpred_ain((if_mooTt(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create))).

:- mpred_ain((default_ooTt(X) ==> if_mooTt(current_ooTt(_),current_ooTt(X)))).

:- mpred_ain(default_ooTt(defaultValueTt)).

:- mpred_test(current_ooTt(defaultValueTt)).

:- (mpred_ain(current_ooTt(fooTt))).

:- mpred_test(\+current_ooTt(defaultValueTt)).

:- (mpred_ain(\+ current_ooTt(fooTt))).

:- mpred_test(current_ooTt(defaultValueTt)).

:- (mpred_withdraw( default_ooTt(defaultValueTt) )).

:- listing([current_ooTt,default_ooTt]).

:- mpred_test( \+current_ooTt(defaultValueTt)).

:- mpred_ain(~ current_ooTt(fooTt)).

:- pp_cur_DB.

:- mpred_test(~current_ooTt(fooTt)).

:- mpred_ain(default_ooTt(defaultValueTt)).

:- mpred_test(current_ooTt(defaultValueTt)).






:- must(mpred_reset).


:- if(current_module(mpred_loader)).
:- wdmsg("Already loaded mpred_loader!").
:- endif.


% local_testing


:- make,check,ensure_loaded(library(logicmoo_base)),make,check.


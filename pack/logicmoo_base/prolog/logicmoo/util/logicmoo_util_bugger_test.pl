/** <module> Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_bugger.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
end_of_file.

:- module(logicmoo_util_bugger_test,[test1a/0,test1b/0,test_foo_test/1]).
%:- use_module(logicmoo_util_bugger_new).
%:- use_module(logicmoo_util_bugger_catch).
%:- ensure_loaded(logicmoo_util_bugger).
:- use_module(logicmoo_util_all).

%:- assert_if_new(tlbugger:use_bugger_expansion).

:- swi_export(test_foo_test_safe/1).
test_foo_test_safe(Arg):-throw(test_foo_test_safe(Arg)).
:- swi_export(test_foo_test/1).
test_foo_test(Arg):-dmsg(color(green,test_foo_test(Arg))).

:- swi_export(test_is/1).
test_is(Arg):-dmsg(color(red,test_is(Arg))).

test1a:-test_is(bar).

test1b:-test_foo_test(dont_call_self).

tst:-dmg(tSt).
:- swi_export(tst/0).

%example program for testing
%?-p(X).
p(X) :- a(X). 
p(X) :- b(X),c(X), d(X),e(X). 
p(X) :- f(X).
p(m):-!.  
p(o). 

b(Y) :- g(Y), h(Y).
b(1). 
b(2). 

a(1).
v(X):-p(X),s(X). 
v(n). 

s(o). 
s(p).
s(X):-v(X).


c(1). 
c(2).

d(1). 
d(2). 

e(2). 

f(3). 

g(2).
g(1).

h(2).

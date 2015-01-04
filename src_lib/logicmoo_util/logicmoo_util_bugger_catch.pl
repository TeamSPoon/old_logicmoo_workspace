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
:-module(logicmoo_util_bugger_catch,[
      ]).


% use ccatch/3 to replace catch/3 works around SWI specific issues arround using $abort/0 and block/3
% (catchv/3 allows you to have these exceptions bubble up past your catch block handlers)
:- meta_predicate((catchv(0, ?, 0))).
:- meta_predicate((ccatch(0, ?, 0))).
:- export((ccatch/3,catchv/3)).
bubbled_ex(block(_,_)).
bubbled_ex('$aborted').
bubbled_ex_check(E):- (\+ bubbled_ex(E)),!.
bubbled_ex_check(E):-throw(E).
ccatch(Goal,E,Recovery):- nonvar(E) -> catch(Goal,E,Recovery); % normal mode (the user knows what they want)
                         catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode
catchv(Goal,E,Recovery):- catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode



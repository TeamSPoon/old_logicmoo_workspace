/** <module> 
% ===================================================================
% File 'dbase_i_cyc.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which make us more like Cyc
%
% Dec 13, 2035
% Douglas Miles
*/

:-if_file_exists(user_ensure_loaded(logicmoo(ext/moo_ext_cyc_new))).

:-dynamic(isCycUnavailable_known/1).
:-dynamic(isCycAvailable_known/0).

:-swi_export(isCycAvailable/0).
isCycAvailable:-isCycUnavailable_known(_),!,fail.
isCycAvailable:-isCycAvailable_known,!.
isCycAvailable:-checkCycAvailablity,isCycAvailable.

:-swi_export(isCycUnavailable/0).
isCycUnavailable:-isCycUnavailable_known(_),!.
isCycUnavailable:-isCycAvailable_known,!,fail.
isCycUnavailable:-checkCycAvailablity,isCycUnavailable.

:-swi_export(checkCycAvailablity/0).
checkCycAvailablity:- (isCycAvailable_known;isCycUnavailable_known(_)),!.
checkCycAvailablity:- ccatch((ignore((invokeSubL("(+ 1 1)",R))),(R==2->assert_if_new(isCycAvailable_known);assert_if_new(isCycUnavailable_known(R)))),E,assert_if_new(isCycUnavailable_known(E))),!.

end_of_file.


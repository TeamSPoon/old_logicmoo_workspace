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

% ============================================
% DBASE to Cyc Predicate Mapping
% ============================================

mpred_arity('abbreviationString-PN', 2).

typical_mtvars([_,_]).

% arity 1 person
make_functorskel(Person,1,fskel(Person,hasInstance(Person,A),Call,A,[],MtVars,Call2)):-typical_mtvars(MtVars),Call=..[Person,A],Call2=..[Person,A|MtVars]. 
% arity 2 likes
make_functorskel(Likes,2,fskel(Likes,dbase_t(Likes,A,B),Call,A,B,MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Likes,A,B],Call2=..[Likes,A,B|MtVars]. 
% arity 3 between
make_functorskel(Between,3,fskel(Between,dbase_t(Between,A,B,C),Call,A,[B,C],MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Between,A,B,C],Call2=..[Between,A,B,C|MtVars]. 
% arity 4 xyz
make_functorskel(Xyz,4,fskel(Xyz,dbase_t(Xyz,I,X,Y,Z),Call,I,[X,Y,Z],MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Xyz,I,X,Y,Z],Call2=..[Xyz,I,X,Y,Z|MtVars]. 
% arity 5 rxyz
make_functorskel(RXyz,5,fskel(RXyz,dbase_t(RXyz,I,R,X,Y,Z),Call,I,[R,X,Y,Z],MtVars,Call2)):-typical_mtvars(MtVars),Call=..[RXyz,I,R,X,Y,Z],Call2=..[RXyz,I,R,X,Y,Z|MtVars]. 
% arity >6 
make_functorskel(F,N,fskel(F,DBASE,Call,I,NList,MtVars,Call2)):-typical_mtvars(MtVars),functor(Call,F,N),Call=..[F,I|NList],DBASE=..[dbase_t,F,I|NList],append([F,I|NList],MtVars,CALL2List),Call2=..CALL2List.


% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% :- decl_mpred('BaseKB':isa/2). 
% :- decl_mpred('BaseKB':isa(_,_)). 
% :- decl_mpred(isa(_,_),'BaseKB'). 
% :- decl_mpred('BaseKB',isa,2). 
%
%  Will make calls 
% :- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% decl_mpred/N
%
% ============================================

:-dynamic(isCycUnavailable_known/1).
:-dynamic(isCycAvailable_known/0).

:-dynamic_multifile_exported(isCycAvailable/0).
isCycAvailable:-isCycUnavailable_known(_),!,fail.
isCycAvailable:-isCycAvailable_known,!.
isCycAvailable:-checkCycAvailablity,isCycAvailable.

:-dynamic_multifile_exported(isCycUnavailable/0).
isCycUnavailable:-isCycUnavailable_known(_),!.
isCycUnavailable:-isCycAvailable_known,!,fail.
isCycUnavailable:-checkCycAvailablity,isCycUnavailable.

:-dynamic_multifile_exported(checkCycAvailablity/0).
checkCycAvailablity:- (isCycAvailable_known;isCycUnavailable_known(_)),!.
checkCycAvailablity:- ccatch((ignore((invokeSubL("(+ 1 1)",R))),(R==2->assert_if_new(isCycAvailable_known);assert_if_new(isCycUnavailable_known(R)))),E,assert_if_new(isCycUnavailable_known(E))),!.

end_of_file.


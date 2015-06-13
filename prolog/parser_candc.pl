% ===================================================================
% File 'parser_candc.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_candc.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:-module(parser_candc,[

         ]).


% ==============================================================================

:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).
:- user:ensure_loaded('candc/src/prolog/boxer/boxer.pl').


% ===================================================================
% File 'parser_all.pl'
% Purpose: English to KIF conversions from SWI-Prolog  
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_all.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:-module(parser_all,[

         ]).


% ==============================================================================

:- user:ensure_loaded(parser_candc).
:- user:ensure_loaded(parser_ape).
:- user:ensure_loaded(parser_chat80).
:- user:ensure_loaded(parser_talk).


% ===================================================================
% File 'parser_CURT.pl'
% Purpose: English to KIF conversions from SWI-Prolog  
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_CURT.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

end_of_file.
end_of_file.
end_of_file.

:-module(parser_CURT,[
         ]).

% ==============================================================================
:- include('CURT/logicmooCURT2').
% :- ['CURT/advertentCurt'].



% ===========================================================
% CURT80 command
% ===========================================================
type_action_info(human_player,curt80(list(term)),"Development test CURT Text for a human.  Usage: CURT80 Cant i see the blue backpack?").

user:agent_call_command(_Gent,curt80([])):- curt80.
user:agent_call_command(_Gent,curt80(StringM)):- curt80(StringM).  


% ===========================================================
% CURT80 REPL
% ===========================================================
:-thread_local t_l:curt80_interactive/0.
curt80 :- w_tl(tracing80,
           w_tl(t_l:curt80_interactive,
            w_tl(t_l:useOnlyExternalDBs,
             w_tl(lmconf:use_cyc_database,
              (told, repeat, prompt_read('CURT80> ',U),  
                            to_word_list(U,WL),((WL==[bye];WL==[end,'_',of,'_',file];((mmake,curt80(WL,State),State==stop))))))))).

curt80(Input,State):-  curtUpdate(Input,CurtsMoves,State), 
   curtPredicates:curtOutput(CurtsMoves).
   

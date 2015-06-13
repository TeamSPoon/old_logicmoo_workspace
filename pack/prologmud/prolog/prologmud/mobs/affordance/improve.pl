/*  *  <module> 
% Uses affordances in which agents belief that some outcome will happen
% We also in this file purposelty create disparities
% Example: Buy this new car and you will suddenly become sexy!
%  Result: less money in pocket now but have vehical - but not sexier!
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


((agentTODO(Agent,actDo(Something,Obj)),
   localityOfObject(Obj,LOC),
   localityOfObject(A,LOC)) => 
     (~ agentTODO(Agent,actDo(Something,Obj)),
     enqueue_agent_action(Agent,actDo(Something,Obj)))).

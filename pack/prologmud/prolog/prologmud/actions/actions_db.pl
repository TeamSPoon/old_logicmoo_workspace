% <?xml version="1.0"?>
% <!--
/*************************************************************************

    File: actionDatabase.pl
    Copyright (C) 2007 

    Programmer: Luciana Benotti

    This file is part of Frolog, version 0.1 (October 2007).

    Frolog is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Frolog is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Frolog; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/
:-swi_module(modActionDatabase, []).

end_of_file.

:-swi_module(modActionDatabase,[take/3,
			  eat/2,
			  drop/3,
			  throw/3,
			  put/3,
			  kiss/2,
			  kill/3,
			  look/2,
			  standup/3,
			  move/4,
			  unlock/3,
			  open/2,
			  lock/3,
			  shut/2,
			  sitdown/3,
			  wear/2 
]).  
 

/*************************************************************************

    Function: This is the action database of the scenario FairyTaleCastle.

    Technical: The thematic roles of the actions are obtained from the core 
    features of FrameNet.

    Wishlist: Another thing is that actions need to be primitives (I
    think in most of the AI literature actions are taken to be basic
    constructs). Modification of an action is a problem. For isa,
    you could have

    Take the key with your left hand!
    Lock the door with your right hand!
    Open the door carefully!
    etc.

    where the action is complex. An ad-hoc way of dealing with this is
    to have action schemata for all possible combinations (i.e. one
    for taking a key with the left hand, one for taking the key with
    the right hand, etc.) But this option ignores the compositional
    nature of instructions. And you would need an infinite number of
    action schemata.

    It would be interesting to have action schemas and perhaps action
    modifier schemas that extend existing action schemes. So for the
    above example you could have an action scheme for "take X" (as you
    already have) and action modifier scheme for "with Y hand" and
    combine the two when needed.


*************************************************************************/

:- use_module('NLPModules/Actions/execute',[k/1,notk/1,add/1,del/1]).

% TODO: Design the addition and deletion of the player effects.  
% TODO: Translate into PKS syntax:
% 1. Delete the pecentage symbol
% 2. Check if PKS accepts variables without question mark
% 3. Define all the predicates, functions and constants
% 4. Replace the letters k by K
% 5. In the preconditions, replace commas by ^
% 6. In the effects replace commas by ;

%-->
%<pks>
%<domain name="fairytalecastle">
%    <symbols>
%      <predicates>
%        takeAble/1, complete ...
%      </predicates>
%      <functions>
%
%      </functions>
%      <constants>
%        nirvana
%      </constants>
%    </symbols>

%<actions>

% <!-- looking: This frame concerns an Agent (a person or other intelligent being) paying close attention to something, the Something -->
% <action name="look">
%	<params>Agent,Something</params>
% <!--
look(agent(Agent),something(Something)) :- % -->
%   <preconds>
	k(here(Something)),
%	</preconds>
%	<effects>
	add(contains(Something,Agent)),
	add(describe(here)).
%	</effects>
%</action>

% <!-- looking: This frame concerns an Agent (a person or other intelligent being) paying close attention to something, the Something -->
% <action name="look">
%	<params>Agent,Something</params>
% <!--
look(agent(Agent),something(Something)) :- % -->
%   <preconds>
	notk(here(Something)),
	k(lookable(Something)),
%	</preconds>
%	<effects>
	add(describe(Something)).
%	</effects>
%</action>

% <!-- removing: An Agent causes a Something to move away from a location, the Source -->
% <action name="take">
%	<params>Agent,Something,Source</params>
% <!--
take(agent(Agent),something(Something),source(Source)) :- % -->
%       <preconds>
	k(takeAble(Something)),
	k(accessible(Something)),
	notk(contains(Agent,Something)),
	k(hasdetail(Source,Something)),
%	</preconds>
%	<effects>
	del(hasdetail(Source,Something)),
	add(contains(Agent,Something)).
%	</effects>
%</action>

% <!-- taking: An Agent removes a Something from a Source so that it is in the Agent's possession -->
% <action name="take">
%	<params>Agent,Something,Source</params>
% <!--
take(agent(Agent),something(Something),source(Source)) :- % -->
%       <preconds>
	k(takeAble(Something)),
	k(accessible(Something)),
	notk(contains(Agent,Something)),
	k(contains(Source,Something)),
%	</preconds>
%	<effects>
	del(contains(Source,Something)),
	add(contains(Agent,Something)).
%	</effects>
%</action>

% <!-- unlocking:  -->
% <action name="unlock">
%	<params>Agent,Something,Instrument</params>
% <!--
unlock(agent(Agent),something(Something),instrument(Instrument)) :- % -->
%   <preconds>
	k(accessible(Something)),
	k(locked(Something)),
	pk(fitsin(Instrument,Something)),
	k(contains(Agent,Instrument)),
%	</preconds>
%	<effects>
	del(locked(Something)),
	add(unlocked(Something)),
	add(fitsin(Instrument,Something)).
%	</effects>
%</action>


% <!-- locking:  -->
% <action name="lock">
%	<params>Agent,Something,Instrument</params>
% <!--
lock(agent(Agent),something(Something),instrument(Instrument)) :- % -->
%   <preconds>
	k(accessible(Something)),
	k(closed(Something)),
	k(unlocked(Something)),
	k(fitsin(Instrument,Something)),
	k(contains(Agent,Instrument)),
%	</preconds>
%	<effects>
	del(unlocked(Something)),
	add(locked(Something)).
%	</effects>
%</action>

% <!-- closure: The Agent opens/closes the Containing-object -->
% <action name="open">
%	<params>Agent,Object</params>
% <!--
open(agent(Agent),item(Object)) :- % -->
%   <preconds>
	k(openclosecontainer(Object)),
	k(closed(Object)),
	k(unlocked(Object)),
	k(accessible(Object)),
%	</preconds>
%	<effects>
	del(closed(Object)),
	add(open(Object)),
	add(describe(contents,Object)).
%	</effects>
%</action>

% <!-- closure: The Agent opens/closes the Containing-object -->
% <action name="shut">
%	<params>Agent,Object</params>
% <!--
shut(agent(Agent),item(Object)) :- % -->
%   <preconds>
	k(openclosecontainer(Object)),
	k(open(Object)),
	k(accessible(Object)),
%	</preconds>
%	<effects>
	del(open(Object)),
	add(closed(Object)).
%	</effects>
%</action>

% <!-- ingestion: An Ingestor consumes food, drink, or smoke (Ingestibles) 
% <action name="eat">
%	<params>Ingestor,Ingestible</params>
% <!--
eat(ingestor(Ingestor),ingestible(Ingestible)) :- % -->
%   <preconds>
	k(edible(Ingestible)),
	notk(disgusting(Ingestible)),
	k(contains(Ingestor,Ingestible)),
%	</preconds>
%	<effects>
	del(contains(Ingestor,Ingestible)),
	add(gone(Ingestible)).
%	</effects>
%</action>

% <!-- cause_motion: An Agent causes a Something to undergo directed motion, 
% the Agent has control of the Something only at the Source of motion-->
% <action name="drop">
%	<params>Agent,Something,Target</params>
% <!--
drop(agent(Agent),something(Something),goal(Target)) :- % -->
%       <preconds>
	k(contains(Target,Agent)),
	k(contains(Agent,Something)),
%	</preconds>
%	<effects>
	del(contains(Agent,Something)),
	add(contains(Target,Something)).
%	</effects>
%</action>

% <!-- cause_motion: An Agent causes a Something to undergo directed motion, 
% the Agent has control of the Something only at the Source of motion-->
% <action name="throw">
%	<params>Agent,Something,Target</params>
% <!--
throw(agent(Agent),something(Something),goal(Target)) :- % -->
%       <preconds>
	k(contains(Agent,Something)),
	k(alive(Something)),
	k(genericcontainer(Target)),
%	</preconds>
%	<effects>
	del(contains(Agent,Something)),
	del(alive(Something)),
	add(contains(Target,Something)),
	add(dead(Something)).
%	</effects>
%</action>

% <!-- placing: An Agent places a Something at a location, the Target, which is 
% profiled -->
% <action name="put">
%	<params>Agent,Something,Target</params>
% <!--
put(agent(Agent),something(Something),goal(Target)) :- % -->
%	<preconds>
	k(contains(Agent,Something)),
	k(genericcontainer(Target)),
	k(accessible(Target)),
%	</preconds>
%	<effects>
	del(contains(Agent,Something)),
	add(contains(Target,Something)).
%	</effects>
%</action>

% <!-- placing: An Agent places a Something at a location, the Target, which is 
% profiled -->
% <action name="put">
%	<params>Agent,Something,Target</params>
% <!--
put(agent(Agent),something(Something),goal(Target)) :- % -->
%	<preconds>
	k(contains(Agent,Something)),
	notk(genericcontainer(Target)),
	k(accessible(Target)),
%	</preconds>
%	<effects>
	del(contains(Agent,Something)),
	add(hasdetail(Target,Something)).
%	</effects>
%</action>

% <!-- placing: An Agent places a Something on himself -->
% <action name="wear">
%	<params>Agent,Something</params>
% <!--
wear(agent(Agent),something(Something)) :- % -->
%	<preconds>
	k(contains(Agent,Something)),
	k(accessible(Something)),
	k(wearable(Something)),
%	</preconds>
%	<effects>
	del(contains(Agent,Something)),
	add(hasdetail(Agent,Something)).
%	</effects>
%</action>

% <!-- manipulation: Touch or caress with the lips as a sign of love, affection, 
% or greeting. -->
% <action name="kiss">
%	<params>Agent,Entity</params>
% <!--
kiss(agent(Agent),entity(Entity)) :- % -->
%	<preconds>
	k(alive(Entity)),
	k(accessible(Entity)),
	k(beautiful(Agent)),
%	</preconds>
%	<effects>
	add(happy(Entity)),
	add(victorious(Agent)).
%	</effects>
%</action>

% <!-- manipulation: Touch or caress with the lips as a sign of love, affection, 
% or greeting. -->
% <action name="kiss">
%	<params>Agent,Entity</params>
% <!--
kiss(agent(Agent),entity(Entity)) :- % -->
%	<preconds>
	k(alive(Entity)),
	k(accessible(Entity)),
	notk(beautiful(Agent)),
%	</preconds>
%	<effects>
	add(bored(Entity)).
%	</effects>
%</action>

% <!--
% TODO: We could define only one action kill with 3 parameters. 
% Replacing the concepts easytokill and notsoeasytokill with a role like
% kills(knife1,dragon1), for example. In order to have the default use of
% 'kill the frog' (with your hands) the player will start knowing that
% kills(hands,frog1) or sth like that. These things could be defined in the
% TBOX. Needs thinking. -->

% <!-- A Killer causes the death of the Victim. -->
% <action name="kill">
%	<params>Killer,Victim</params>
% <!--
kill(killer(Killer),victim(Victim),instrument(Instrument)) :- % -->
%	<preconds>
	k(alive(Victim)),
	k(easytokill(Victim)),
	k(accessible(Victim)),
%	</preconds>
%	<effects>
	del(alive(Victim)),
	add(dead(Victim)). % TODO Here dead is verbalized but not asserted because it's not a primitive concept
					   % This treatment might be useful for other cases, keep it in mind.  
%	</effects>
%</action>

% <!-- A Killer causes the death of the Victim. -->
% <action name="kill">
%	<params>Killer,Victim,Instrument</params>
% <!--
kill(killer(Killer),victim(Victim),instrument(Instrument)) :- % -->
%	<preconds>
	k(alive(Victim)),
	k(accessible(Victim)),
	k(weapon(Instrument)),
%	</preconds>
%	<effects>
	del(alive(Victim)),
	add(dead(Victim)). % TODO Here dead is verbalized but not asserted because it's not a primitive concept
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Target. -->
% <action name="move">
%	<params>Protagonist,Source,Target</params>
% <!--
standup(protagonist(Protagonist),source(Source),goal(Target)) :- % -->
%	<preconds>
	k(contains(Source,Protagonist)),
	k(seated(Protagonist)),
	k(contains(Target,Source)),
%	</preconds>
%	<effects>
	del(contains(Source,Protagonist)),
	add(contains(Target,Protagonist)).
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Target. -->
% <action name="move">
%	<params>Protagonist,Source,Target</params>
% <!--
sitdown(protagonist(Protagonist),source(Source),goal(Target)) :- % -->
%	<preconds>
	k(contains(Source,Protagonist)),
	notk(seated(Protagonist)),
	k(seating(Target)),
	k(contains(Source,Target)),
%	</preconds>
%	<effects>
	del(contains(Source,Protagonist)),
	add(contains(Target,Protagonist)).
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Target. -->
% <action name="move">
%	<params>Protagonist,Source,Target</params>
% <!--
move(protagonist(Protagonist),exit(Exit),goal(Target),source(Source)) :- % -->
%	<preconds>
	k(here(Source)),
	% I'd like to add here k(hereroom(Room)), k(hasexit(Room,Exit))
	notk(seated(Protagonist)),
	k(hasexit(Source,Exit)),
	k(leadsto(Exit,Target)),
%	</preconds>
%	<effects>
	del(contains(Source,Protagonist)),
	add(contains(Target,Protagonist)),
	add(leadsto(Exit,Target)),
	add(describe(here)).
%	</effects>
%</action>

%</actions>
%</domain>


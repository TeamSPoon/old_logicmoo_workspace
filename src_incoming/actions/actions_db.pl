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
:- module(actionDatabase, []).

end_of_file.

:- module(actionDatabase,[take/3,
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
    constructs). Modification of an action is a problem. For instance,
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

:- use_module('NLPModules/Actions/execute',[holds/1,notk/1,add/1,del/1]).

% TODO: Design the addition and deletion of the player effects.  
% TODO: Translate into PKS syntax:
% 1. Delete the pecentage symbol
% 2. Check if PKS accepts variables without question mark
% 3. Define all the predicates, functions and constants
% 4. Replace the letters holds by K
% 5. In the preconditions, replace commas by ^
% 6. In the effects replace commas by ;

%-->
%<pks>
%<domain name="fairytalecastle">
%    <symbols>
%      <predicates>
%        takeable/1, complete ...
%      </predicates>
%      <functions>
%
%      </functions>
%      <constants>
%        nirvana
%      </constants>
%    </symbols>

%<actions>

% <!-- looking: This frame concerns an Agent (a person or other intelligent being) paying close attention to something, the Theme -->
% <action name="look">
%	<params>Agent,Theme</params>
% <!--
look(agent(Agent),theme(Theme)) :- % -->
%   <preconds>
	holds(here(Theme)),
%	</preconds>
%	<effects>
	add(hold(Theme,Agent)),
	add(describe(here)).
%	</effects>
%</action>

% <!-- looking: This frame concerns an Agent (a person or other intelligent being) paying close attention to something, the Theme -->
% <action name="look">
%	<params>Agent,Theme</params>
% <!--
look(agent(Agent),theme(Theme)) :- % -->
%   <preconds>
	notk(here(Theme)),
	holds(visible(Theme)),
%	</preconds>
%	<effects>
	add(describe(Theme)).
%	</effects>
%</action>

% <!-- removing: An Agent causes a Theme to move away from a location, the Source -->
% <action name="take">
%	<params>Agent,Theme,Source</params>
% <!--
take(agent(Agent),theme(Theme),source(Source)) :- % -->
%       <preconds>
	holds(takeable(Theme)),
	holds(accessible(Theme)),
	notk(hold(Agent,Theme)),
	holds(hasdetail(Source,Theme)),
%	</preconds>
%	<effects>
	del(hasdetail(Source,Theme)),
	add(hold(Agent,Theme)).
%	</effects>
%</action>

% <!-- taking: An Agent removes a Theme from a Source so that it is in the Agent's possession -->
% <action name="take">
%	<params>Agent,Theme,Source</params>
% <!--
take(agent(Agent),theme(Theme),source(Source)) :- % -->
%       <preconds>
	holds(takeable(Theme)),
	holds(accessible(Theme)),
	notk(hold(Agent,Theme)),
	holds(hold(Source,Theme)),
%	</preconds>
%	<effects>
	del(hold(Source,Theme)),
	add(hold(Agent,Theme)).
%	</effects>
%</action>

% <!-- unlocking:  -->
% <action name="unlock">
%	<params>Agent,Theme,Instrument</params>
% <!--
unlock(agent(Agent),theme(Theme),instrument(Instrument)) :- % -->
%   <preconds>
	holds(accessible(Theme)),
	holds(locked(Theme)),
	pk(fitsin(Instrument,Theme)),
	holds(hold(Agent,Instrument)),
%	</preconds>
%	<effects>
	del(locked(Theme)),
	add(unlocked(Theme)),
	add(fitsin(Instrument,Theme)).
%	</effects>
%</action>


% <!-- locking:  -->
% <action name="lock">
%	<params>Agent,Theme,Instrument</params>
% <!--
lock(agent(Agent),theme(Theme),instrument(Instrument)) :- % -->
%   <preconds>
	holds(accessible(Theme)),
	holds(closed(Theme)),
	holds(unlocked(Theme)),
	holds(fitsin(Instrument,Theme)),
	holds(hold(Agent,Instrument)),
%	</preconds>
%	<effects>
	del(unlocked(Theme)),
	add(locked(Theme)).
%	</effects>
%</action>

% <!-- closure: The Agent opens/closes the Containing-object -->
% <action name="open">
%	<params>Agent,Object</params>
% <!--
open(agent(Agent),object(Object)) :- % -->
%   <preconds>
	holds(openclosecontainer(Object)),
	holds(closed(Object)),
	holds(unlocked(Object)),
	holds(accessible(Object)),
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
shut(agent(Agent),object(Object)) :- % -->
%   <preconds>
	holds(openclosecontainer(Object)),
	holds(open(Object)),
	holds(accessible(Object)),
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
	holds(edible(Ingestible)),
	notk(disgusting(Ingestible)),
	holds(hold(Ingestor,Ingestible)),
%	</preconds>
%	<effects>
	del(hold(Ingestor,Ingestible)),
	add(gone(Ingestible)).
%	</effects>
%</action>

% <!-- cause_motion: An Agent causes a Theme to undergo directed motion, 
% the Agent has control of the Theme only at the Source of motion-->
% <action name="drop">
%	<params>Agent,Theme,Goal</params>
% <!--
drop(agent(Agent),theme(Theme),goal(Goal)) :- % -->
%       <preconds>
	holds(hold(Goal,Agent)),
	holds(hold(Agent,Theme)),
%	</preconds>
%	<effects>
	del(hold(Agent,Theme)),
	add(hold(Goal,Theme)).
%	</effects>
%</action>

% <!-- cause_motion: An Agent causes a Theme to undergo directed motion, 
% the Agent has control of the Theme only at the Source of motion-->
% <action name="throw">
%	<params>Agent,Theme,Goal</params>
% <!--
throw(agent(Agent),theme(Theme),goal(Goal)) :- % -->
%       <preconds>
	holds(hold(Agent,Theme)),
	holds(alive(Theme)),
	holds(genericcontainer(Goal)),
%	</preconds>
%	<effects>
	del(hold(Agent,Theme)),
	del(alive(Theme)),
	add(hold(Goal,Theme)),
	add(dead(Theme)).
%	</effects>
%</action>

% <!-- placing: An Agent places a Theme at a location, the Goal, which is 
% profiled -->
% <action name="put">
%	<params>Agent,Theme,Goal</params>
% <!--
put(agent(Agent),theme(Theme),goal(Goal)) :- % -->
%	<preconds>
	holds(hold(Agent,Theme)),
	holds(genericcontainer(Goal)),
	holds(accessible(Goal)),
%	</preconds>
%	<effects>
	del(hold(Agent,Theme)),
	add(hold(Goal,Theme)).
%	</effects>
%</action>

% <!-- placing: An Agent places a Theme at a location, the Goal, which is 
% profiled -->
% <action name="put">
%	<params>Agent,Theme,Goal</params>
% <!--
put(agent(Agent),theme(Theme),goal(Goal)) :- % -->
%	<preconds>
	holds(hold(Agent,Theme)),
	notk(genericcontainer(Goal)),
	holds(accessible(Goal)),
%	</preconds>
%	<effects>
	del(hold(Agent,Theme)),
	add(hasdetail(Goal,Theme)).
%	</effects>
%</action>

% <!-- placing: An Agent places a Theme on himself -->
% <action name="wear">
%	<params>Agent,Theme</params>
% <!--
wear(agent(Agent),theme(Theme)) :- % -->
%	<preconds>
	holds(hold(Agent,Theme)),
	holds(accessible(Theme)),
	holds(wearable(Theme)),
%	</preconds>
%	<effects>
	del(hold(Agent,Theme)),
	add(hasdetail(Agent,Theme)).
%	</effects>
%</action>

% <!-- manipulation: Touch or caress with the lips as a sign of love, affection, 
% or greeting. -->
% <action name="kiss">
%	<params>Agent,Entity</params>
% <!--
kiss(agent(Agent),entity(Entity)) :- % -->
%	<preconds>
	holds(alive(Entity)),
	holds(accessible(Entity)),
	holds(beautiful(Agent)),
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
	holds(alive(Entity)),
	holds(accessible(Entity)),
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
	holds(alive(Victim)),
	holds(easytokill(Victim)),
	holds(accessible(Victim)),
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
	holds(alive(Victim)),
	holds(accessible(Victim)),
	holds(weapon(Instrument)),
%	</preconds>
%	<effects>
	del(alive(Victim)),
	add(dead(Victim)). % TODO Here dead is verbalized but not asserted because it's not a primitive concept
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Goal. -->
% <action name="move">
%	<params>Protagonist,Source,Goal</params>
% <!--
standup(protagonist(Protagonist),source(Source),goal(Goal)) :- % -->
%	<preconds>
	holds(hold(Source,Protagonist)),
	holds(seated(Protagonist)),
	holds(hold(Goal,Source)),
%	</preconds>
%	<effects>
	del(hold(Source,Protagonist)),
	add(hold(Goal,Protagonist)).
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Goal. -->
% <action name="move">
%	<params>Protagonist,Source,Goal</params>
% <!--
sitdown(protagonist(Protagonist),source(Source),goal(Goal)) :- % -->
%	<preconds>
	holds(hold(Source,Protagonist)),
	notk(seated(Protagonist)),
	holds(seating(Goal)),
	holds(hold(Source,Goal)),
%	</preconds>
%	<effects>
	del(hold(Source,Protagonist)),
	add(hold(Goal,Protagonist)).
%	</effects>
%</action>

% <!-- A Protagonist changes the overall postion and posture of the body from a Source to a Goal. -->
% <action name="move">
%	<params>Protagonist,Source,Goal</params>
% <!--
move(protagonist(Protagonist),exit(Exit),goal(Goal),source(Source)) :- % -->
%	<preconds>
	holds(here(Source)),
	% I'd like to add here holds(hereroom(Room)), holds(hasexit(Room,Exit))
	notk(seated(Protagonist)),
	holds(hasexit(Source,Exit)),
	holds(leadsto(Exit,Goal)),
%	</preconds>
%	<effects>
	del(hold(Source,Protagonist)),
	add(hold(Goal,Protagonist)),
	add(leadsto(Exit,Goal)),
	add(describe(here)).
%	</effects>
%</action>

%</actions>
%</domain>


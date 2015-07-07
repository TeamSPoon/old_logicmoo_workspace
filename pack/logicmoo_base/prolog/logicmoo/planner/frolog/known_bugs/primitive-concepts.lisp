Primitive concepts
------------------
"drawingroom","treasury","room","wall","green","white","player",
"notsoeasytokill","alive","frog","ugly","brown","sword","small",
"crown","silver","apple","dragon","worm","red","chest","wooden",
"locked","closed","couch","couchleg","golden","key","table",
"southexit","northexit","hasdetail","hold","fitsin","hasexit",
"leadsto","hascounterpart"

nouns semantic predicate
------------------------
player dragon frog weapon sword object genericcontainer("container") room here wall 
treasury drawingroom exit northexit southexit eastexit westexit couch 
couchleg seat castle crown necklace key apple worm chest table one thing
object foodpaste openclosecontainer("openable")

adjectives semantic predicate
-----------------------------
alive dead easytokill notsoeasytokill takeable edible climbable wooden 
empty accessible visible seated green silver white yellow golden seating
brown red black happy open closed locked unlocked ugly small disgusting
gone here

stative verb semantic predicate
-------------------------------
hasdetail fitsin hasexit leadsto

Dynamic verbs semantic predicates
---------------------------------
look(agent(Agent)	,theme(Theme))
take(agent(Agent),theme(Theme),			source(Source))
unlock(agent(Agent),theme(Theme),		instrument(Instrument))
lock(agent(Agent),theme(Theme),			instrument(Instrument))
open(agent(Agent),object(Object))
shut(agent(Agent),object(Object))
eat(ingestor(Ingestor),ingestible(Ingestible))
drop(agent(Agent),theme(Theme),			goal(Goal))
throw(agent(Agent),theme(Theme),		goal(Goal))
kiss(agent(Agent),entity(Entity))
kill(killer(Killer),victim(Victim),		instrument(Instrument))
standup(protagonist(Protagonist),		source(Source),goal(Goal)) 
sitdown(protagonist(Protagonist),		source(Source),goal(Goal))
move(protagonist(Protagonist),			exit(Exit),goal(Goal))
put(agent(Agent),theme(Theme),			goal(Goal))

Prepositions
------------
at = theme
from = source
with = instrument
into = goal 
to = goal
on = goal
in = goal
through = exit
		   





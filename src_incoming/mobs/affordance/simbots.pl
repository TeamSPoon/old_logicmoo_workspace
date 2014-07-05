/** <module> 
% Uses affordances in which agents belief that some outcome will happen
% We also in this file purposelty create disparities
% Example: Buy this new car and you will suddenly become sexy!
%  Result: less money in pocket now but have vehical - but not sexier!
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

% See the the seemingly white (not dirrectly usable) in some usefull way
moo:defined_affordance([actionVerb="TravelThru",objType="Passable"]).

moo:defined_affordance([objType="Door",
stringMatch="*doorway",
stringMatch="gate",
stringMatch="Curtain",
superType="Passable"]).

moo:defined_affordance([objType="Floor",
stringMatch="floor",
superType="Passable"]).

moo:defined_affordance([objType="Ladder",
stringMatch="ladder",
superType="Passable"]).

moo:defined_affordance([objType=furnature,actionVerb="BumpIntoBarrier",
'Social'=-300*0,
'Hygiene'=-300*0,
'Comfort'=-300*0,
'Energy'=-300*0,
'Fun'=-300*0]).

% yet every minute you are alive, god wishes to punish you
moo:defined_affordance([objType=self,actionVerb="LiveAtLeastAMinute",
maximumDistance=2000,
'Energy'=100*-0.1,
'Hunger'=100*-1,
'Bladder'=100*-1,
'Hygiene'=100*0,
'Room'=100*-1,
'Social'=100*-1,
'Fun'=100*-1,
'GenerallySadToHappy'=100*-1,
'Comfort'=100*-1]).

moo:defined_affordance([objType="Shower",
actionVerb="CleanBodyWithObject",
slAnim=anim_AFRAID,
textName="Take a Shower",
'Comfort'=10*10,
'Hygiene'=30*30,
actionVerb="CleanTheObject"]).

moo:defined_affordance([objType="Bath",
stringMatch="bath",
stringMatch="bathtub",
dontMatch="Plastic Tub",
actionVerb="CleanBodyWithObject",
textName="Take a Bath",
slSit=true,
'Comfort'=20*20,
'Hygiene'=100*100,
actionVerb="CleanTheObject"]).

moo:defined_affordance([objType="Sink",
actionVerb="CleanBodyWithObject",
textName="Wash Hands",
'Comfort'=0*0,
'Hygiene'=10*10,
actionVerb="CleanTheObject"]).

moo:defined_affordance([objType="BigThing",require(size>8)]).

% moo:defined_affordance([actionVerb="AttachToSelf",textName="Attach it",slAnim=anim_RPS_PAPER,'Comfort'=20*20,'LispScript'="(progn (TheBot.AttachToSelf Target))"]).

moo:defined_affordance([objType="DanceBall",
actionVerb="Dance",
textName="Dance! Dance!",
isTouchDefined=true,
slAnim=anim_DANCE,
'Social'=10*10,
'Fun'=10*10,
'Hygiene'=-10*-10]).

moo:defined_affordance([objType="PoseBall",
stringMatch="*pose",
stringMatch="*Pose",
isTouchDefined=true]).

moo:defined_affordance([objType="WashingMachine",
stringMatch="Washing Machine",
actionVerb="WashTheClothes",
textName="Wash The Clothes",
'Comfort'=0*0,
'Hygiene'=10*10,
actionVerb="CleanTheObject"]).

moo:defined_affordance([objType="ClothesDryer",
stringMatch="Dryer",
actionVerb="DryTheClothes",
textName="Dry The Clothes",
'Comfort'=0*0,
'Hygiene'=10*10,
actionVerb="CleanTheObject"]).

moo:defined_affordance([objType="Bed",
actionVerb="SleepOnObject",
textSitName="Sleep a few",
slSit=true,
slAnim=anim_SLEEP,
'Comfort'=10*30,
'Energy'=100*80]).

moo:defined_affordance([objType="Mattress",
actionVerb="SleepOnObject",
textSitName="Sleep a few",
slSit=true,
slAnim=anim_SLEEP,
'Comfort'=10*30,
'Energy'=100*80]).

moo:defined_affordance([objType="Chair",
stringMatch="*chair",
stringMatch="*stool",
stringMatch="*recliner",
actionVerb="RelaxOnObject",
textSitName="Sit down",
slSit=true,
slAnim=anim_SMOKE_IDLE,
'Comfort'=15*10,
'Energy'=10*20]).

moo:defined_affordance([objType="Couch",
stringMatch="Sofa",
stringMatch="*luvseat*",
stringMatch="*loveseat*",
actionVerb="RelaxOnObject",
textSitName="Sit down",
slSit=true,
slAnim=anim_SMOKE_IDLE,
'Comfort'=20*20,
'Energy'=10*20]).

moo:defined_affordance([objType="Television",
stringMatch="TV",
actionVerb="ObserveObject",
textName="Watch TV",
maximumDistance=4,
'Hunger'=1*-1,
'Bladder'=0*0,
'Hygiene'=0*0,
'Room'=1*0,
'Social'=2*-1,
'Fun'=2*1,
'GenerallySadToHappy'=2*1,
'Energy'=1*-1]).

moo:defined_affordance([objType="Radio",
actionVerb="ObserveObject",
textName="Listen to Radio",
maximumDistance=4,
'Room'=1*0,
'Fun'=10*10,
'GenerallySadToHappy'=10*10,
'Energy'=1*-1]).

moo:defined_affordance([objType="Mirror",
actionVerb="ObserveObject",
textName="Pop your zits",
maximumDistance=2,
'Room'=1*0,
'Fun'=10*10,
'GenerallySadToHappy'=10*-1,
'Energy'=1*-1]).

moo:defined_affordance([objType="Toilet",
actionVerb="RelaxOnObject",
textSitName="Go potty",
slSit=true,
'Bladder'=100*100,
'Hygiene'=0*-10,
actionVerb="CleanTheObject",
textName="Flush it",
slAnim=anim_POINT_YOU,
'Hygiene'=1*4,
'Fun'=5*4]).

moo:defined_affordance([objType="Refrigerator",
stringMatch="*Fridge*",
stringMatch="*Frige*",
stringMatch="*icebox",
actionVerb="RetrieveFood",
textName="Raid the fridge",
slAnim=anim_DRINK,
slGrab=true]).

moo:defined_affordance([objType="Stove",
stringMatch="Oven",
stringMatch="*kitchen range",
actionVerb="RetrieveFood",
textName="Raid the fridge",
slAnim=anim_DRINK,
slGrab=true]).

moo:defined_affordance([objType="Microwave",
actionVerb="RetrieveFood",
textName="see what was forgotten in the microwave",
slAnim=anim_DRINK,
slGrab=true]).

moo:defined_affordance([objType="Treadmill",
actionVerb="ExcersizeUsingObject",
textName="Tread the mill",
slSit=true]).

moo:defined_affordance([objType="FixedLamp",
stringMatch="*floorlamp",
stringMatch="lamp",
stringMatch="lantern",
stringMatch="lightbulb",
stringMatch="lighting",
actionVerb="TouchTheObject",
textName="flip the switch",
slAnim=anim_AIM_BAZOOKA_R]).

moo:defined_affordance([objType="Pooltable",
stringMatch="*pool table*",
actionVerb="PlayWithObject",
textName="Play pool",
slAnim=anim_AIM_BAZOOKA_R]).

moo:defined_affordance([objType="Barrier",
stringMatch="*Wall*",
stringMatch="*Fence*",
stringMatch="*Pillar*",
stringMatch="*Roof*",
stringMatch="*Beam*"]).

moo:defined_affordance([objType="Shelf",
stringMatch="*cupboard",
stringMatch="*Cabinet",
stringMatch="*cabinate",
stringMatch="*FoodStore",
actionVerb="PlaceSomethingAtObject"]).

moo:defined_affordance([objType="Desk",
stringMatch="*Lab Bench",
stringMatch="*workbench",
stringMatch="*officedesk",
actionVerb="PlaceSomethingAtObject"]).

moo:defined_affordance([objType="Counter",stringMatch="Bar",actionVerb="PlaceSomethingAtObject"]).

moo:defined_affordance([objType="PlasticContainer",stringMatch="Plastic",actionVerb="PlaceSomethingAtObject"]).

moo:defined_affordance([objType="Table",
stringMatch="*Coffee Table",
acceptsChild="BookOrMagazine",
acceptsChild="Bread",
actionVerb="PlaceSomethingAtObject"]).

moo:defined_affordance([objType="TrashContainer",
stringMatch="garbage*c",
stringMatch="trash*c",
stringMatch="trash*bin",
stringMatch="waste",
stringMatch="recycle*bin",
acceptsChild="TakeTheObject",
actionVerb="PlaceSomethingAtObject"]).

moo:defined_affordance([objType="Bookcase",
stringMatch="*Bookcase",
stringMatch="*Bookshelf",
stringMatch="*Bookshelve",
acceptsChild="BookOrMagazine",
actionVerb="ObserveObject",
textName="Browse books",
slAnim=anim_YES,
'Fun'=10*10,
'Room'=20*20]).

moo:defined_affordance([objType="BookOrMagazine",
stringMatch="Book",
stringMatch="Magazine",
actionVerb="ObserveObject",
textName="Read book",
slGrab=true,
slAnim=anim_LAUGH_SHORT,
'Fun'=10*10,
'Room'=20*20,
actionVerb="TakeTheObject",
textName="Take the materials"]).

moo:defined_affordance([objType="Bread",
'AcceptsParent'="Avatar",
actionVerb="EatTheObject",
textName="Eat the bread",
slAnim=anim_DRINK,
actionVerb="TakeTheObject",
textName="Take the bread"]).

moo:defined_affordance([objType="ArtObject",
stringMatch="Art *",
actionVerb="ObserveObject",
textName="Apreciate the ArtObject",
slAnim=anim_YES_HAPPY,
'Fun'=10*10,
'Room'=20*20]).

moo:defined_affordance([objType="Dance",
actionVerb="PlayWithObject",
textName="Dance! Dance!",
slAnim=anim_DANCE2]).

moo:defined_affordance([objType="Computer",
stringMatch="keyboard",
stringMatch="keypad",
stringMatch="workstation",
stringMatch="Monitor",
actionVerb="PlayWithObject",
textName="Look busy doing something!",
slAnim=anim_TYPE]).

moo:defined_affordance([objType=agent,
actionVerb="TalkToObject",
'Social'=10*15,
'Fun'=1*1,
actionVerb="BeatOnObject",
'Social'=10*15,
'Energy'=0*-10,
'GenerallySadToHappy'=-10*-10,
'Fun'=20*10,
actionVerb="PushTheObject",
'Social'=10*15,
'Energy'=0*-10,
'GenerallySadToHappy'=0*-10,
'Fun'=20*10,
actionVerb="KissTheObject",
'Social'=10*15,
'GenerallySadToHappy'=10*10,
'Fun'=10*10]).

moo:defined_affordance([objType=touchable,actionVerb="TouchTheObject",
textName="Touch",
slGrab=true,
'Fun'=1*1,
'Room'=1*1]).

moo:defined_affordance([objType=sittable,actionVerb="SitOnObject",
textName="Sit on",
slSit=true,
'Fun'=1*1,
'Room'=1*1]).

moo:defined_affordance([objType=hasSurface,actionVerb="PlaceSomethingAtObject",
textName="This is a PlaceSomethingAtObject placeholder",
slAnim=anim_FINGER_WAG,
'Fun'=-2*2,
'Energy'=0*-1]).

moo:defined_affordance([objType=sittable,actionVerb="RelaxOnObject",
textName="Sit on",
slSit=true,
slAnim=anim_SIT,
'Comfort'=1*0]).

moo:defined_affordance([objType=consumable,actionVerb="EatTheObject",
textName="Eat it",
isDestroyedOnUse=true,
'Hunger'=100*100,
'Hygiene'=0*-10]).

moo:defined_affordance([objType=takeable,actionVerb="TakeTheObject",
textName="Take it",
'AcceptsParent'="Avatar"]).

moo:defined_affordance([objType=sittable,actionVerb="SleepOnObject",
textName="Lay on",
slSit=true,
slAnim=anim_SLEEP,
'Comfort'=5*5,
'Energy'=20*20]).

moo:defined_affordance([alsoType=visible,actionVerb="CleanTheObject",
textName="Clean",
slAnim=anim_FINGER_WAG,
'Fun'=-2*2,
'Energy'=0*-1]).

moo:defined_affordance([alsoType=visible,actionVerb="ObserveObject",
textName="Observe",
maximumDistance=5,
slAnim=anim_CLAP,
'Fun'=2*1,
'Energy'=0*-1]).

moo:defined_affordance([objType=takeable,actionVerb="CleanBodyWithObject",
textName="Wash",
slAnim=anim_RPS_PAPER,
'Comfort'=0*10,
'Hygiene'=20*10]).

moo:defined_affordance([objType=sittable,actionVerb="ExcersizeUsingObject",
textName="Excersize",
slAnim=anim_ONETWO_PUNCH,
'Fun'=10*10,
'Hygiene'=-10*-10]).

moo:defined_affordance([objType=agent,actionVerb="PlayWithObject",
textName="Play with",
slAnim=anim_SHOOT_BOW_L,
alsoType=visible,
'Energy'=-10*-10,
'Fun'=20*10]).

moo:defined_affordance([objType=container,actionVerb="RetrieveFood",
textName="Eat from",
slAnim=anim_DRINK,
'Hygiene'=0*-5,
'Hunger'=40*20]).

moo:defined_affordance([objType=agent,actionVerb="BeatOnObject",
textName="Beat up",
alsoType=visible,
slAnim=anim_ONETWO_PUNCH,
'Energy'=-11*-20]).

moo:defined_affordance([objType=agent,actionVerb="TalkToObject",
textName="Talk to",
maximumDistance=3,
alsoType=visible,
slAnim=anim_TALK,
'Social'=11*20]).

moo:defined_affordance([objType=agent,actionVerb="PushTheObject",
textName="Beat up",
slAnim=anim_SWORD_STRIKE,
'Energy'=-11*-20]).

moo:defined_affordance([objType=agent,actionVerb="KissTheObject",
textName="Kiss",
slAnim=anim_BLOW_KISS,
'Social'=11*20,
'Fun'=21*20]).

moo:defined_affordance([objType=visible,actionVerb="ThinkAboutObject",
textName="Think about",
slAnim=anim_SHRUG,
'Fun'=-10*0]).



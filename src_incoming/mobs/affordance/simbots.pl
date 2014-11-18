/*  *  <module> 
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
moo:defined_affordance([objType= "Passable",actionVerb= "TravelThru"]).


moo:defined_affordance([objType= "Television",
stringMatch= "TV",
actionVerb= "Observe",
textName= "Watch TV",
maximumDistance= 4,
'Social'= 3 * -2, % this form means the AI player thinks observing a TV will satisfy their social needs by 3% .. yet instead, it reduces by 2%
'Hunger'= 1 * -1, 
'Bladder'= 0 * 0,
'Hygiene'= 0 * 0,
'Room'= 1 * 0,
'Fun'= 2 * 1,
'GenerallySadToHappy'= 2 * 1,
'Energy'= 1 * -1]).

moo:defined_affordance([objType= "Door",
stringMatch= " * doorway",
stringMatch= "gate",
stringMatch= "Curtain",
superType= "Passable"]).

moo:defined_affordance([objType= "Floor",
stringMatch= "floor",
superType= "Passable"]).

moo:defined_affordance([objType= "Ladder",
stringMatch= "ladder",
superType= "Passable"]).

moo:defined_affordance([objType= furnature,actionVerb= "BumpIntoBarrier",
'Social'= -300 * 0,
'Hygiene'= -300 * 0,
'Comfort'= -300 * 0,
'Energy'= -300 * 0,
'Fun'= -300 * 0]).

% yet every minute you are alive, god wishes to punish you
moo:defined_affordance([objType= self,actionVerb= "LiveAtLeastAMinute",
   maximumDistance= 2000,
   'Energy'= 100 * -1,
   'UnBored'= 100 * -100,
   'Hunger'= 100 * -1,
   'Bladder'= 100 * -1,
   'Hygiene'= 100 * 0,
   'Room'= 100 * -1,
   'Social'= 100 * -1,
   'Fun'= 100 * -1,
   'GenerallySadToHappy'= 100 * -1,
   'Comfort'= 100 * -1 ]).

moo:defined_affordance([objType= "Shower",
actionVerb= "Clean_Body_Using",
slAnim= anim_AFRAID,
textName= "Take a Shower",
'Comfort'= 10 * 10,
'Hygiene'= 30 * 30,
actionVerb= "Clean"]).

moo:defined_affordance([objType= "Bath",
stringMatch= "bath",
stringMatch= "bathtub",
dontMatch= "Plastic Tub",
actionVerb= "Clean_Body_Using",
textName= "Take a Bath",
slSit= true,
'Comfort'= 20 * 20,
'Hygiene'= 100 * 100,
actionVerb= "Clean"]).

moo:defined_affordance([objType= "Sink",
actionVerb= "Clean_Body_Using",
textName= "Wash Hands",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

moo:defined_affordance([objType= "BigThing",require(size > 8)]).

% moo:defined_affordance([actionVerb= "AttachToSelf",textName= "Attach it",slAnim= anim_RPS_PAPER,'Comfort'= 20 * 20,'LispScript'= "(progn (TheBot.AttachToSelf Target))"]).

moo:defined_affordance([objType= "DanceBall",
actionVerb= "Dance",
textName= "Dance! Dance!",
isTouchDefined= true,
slAnim= anim_DANCE,
'Social'= 10 * 10,
'Fun'= 10 * 10,
'Hygiene'= -10 * -10]).

moo:defined_affordance([objType= "PoseBall",
stringMatch= " * pose",
stringMatch= " * Pose",
isTouchDefined= true]).

moo:defined_affordance([objType= washing_machine,
stringMatch= "Washing Machine",
actionVerb= "WashTheClothes",
textName= "Wash The Clothes",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

moo:defined_affordance([objType= "ClothesDryer",
stringMatch= "Dryer",
actionVerb= "DryTheClothes",
textName= "Dry The Clothes",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

moo:defined_affordance([objType= "Bed",
actionVerb= "Sleep_On",
textSitName= "Sleep a few",
slSit= true,
slAnim= anim_SLEEP,
'Comfort'= 10 * 30,
'Energy'= 100 * 80]).

moo:defined_affordance([objType= "Mattress",
actionVerb= "Sleep_On",
textSitName= "Sleep a few",
slSit= true,
slAnim= anim_SLEEP,
'Comfort'= 10 * 30,
'Energy'= 100 * 80]).

moo:defined_affordance([objType= "Chair",
stringMatch= " * chair",
stringMatch= " * stool",
stringMatch= " * recliner",
actionVerb= "Relax_On",
textSitName= "Sit down",
slSit= true,
slAnim= anim_SMOKE_IDLE,
'Comfort'= 15 * 10,
'Energy'= 10 * 20]).

moo:defined_affordance([objType= "Couch",
stringMatch= "Sofa",
stringMatch= " * luvseat * ",
stringMatch= " * loveseat * ",
actionVerb= "Relax_On",
textSitName= "Sit down",
slSit= true,
slAnim= anim_SMOKE_IDLE,
'Comfort'= 20 * 20,
'Energy'= 10 * 20]).

moo:defined_affordance([objType= "Radio",
actionVerb= "Observe",
textName= "Listen to Radio",
maximumDistance= 4,
'Room'= 1 * 0,
'Fun'= 10 * 10,
'GenerallySadToHappy'= 10 * 10,
'Energy'= 1 * -1]).

moo:defined_affordance([objType= "Mirror",
actionVerb= "Observe",
textName= "Pop your zits",
maximumDistance= 2,
'Room'= 1 * 0,
'Fun'= 10 * 10,
'GenerallySadToHappy'= 10 * -1,
'Energy'= 1 * -1]).

moo:defined_affordance([objType= "Toilet",
actionVerb= "Relax_On",
textSitName= "Go potty",
slSit= true,
'Bladder'= 100 * 100,
'Hygiene'= 0 * -10,
actionVerb= "Clean",
textName= "Flush it",
slAnim= anim_POINT_YOU,
'Hygiene'= 1 * 4,
'Fun'= 5 * 4]).

moo:defined_affordance([objType= "Refrigerator",
stringMatch= " * Fridge * ",
stringMatch= " * Frige * ",
stringMatch= " * icebox",
actionVerb= "RetrieveFood",
textName= "Raid the fridge",
slAnim= anim_DRINK,
slGrab= true]).

moo:defined_affordance([objType= "Stove",
stringMatch= "Oven",
stringMatch= " * kitchen range",
actionVerb= "RetrieveFood",
textName= "Raid the fridge",
slAnim= anim_DRINK,
slGrab= true]).

moo:defined_affordance([objType= "Microwave",
actionVerb= "RetrieveFood",
textName= "see what was forgotten in the microwave",
slAnim= anim_DRINK,
slGrab= true]).

moo:defined_affordance([objType= "Treadmill",
   actionVerb= "Excersize_using",
   textName= "Tread the mill",
   slSit= true]).

moo:defined_affordance([objType= "FixedLamp",
   stringMatch= " * floorlamp",
   stringMatch= "lamp",
   stringMatch= "lantern",
   stringMatch= "lightbulb",
   stringMatch= "lighting",
   actionVerb= "Touch",
   textName= "flip the switch",
   slAnim= anim_AIM_BAZOOKA_R]).

moo:defined_affordance([objType= "Pooltable",
stringMatch= " * pool table * ",
actionVerb= "Play_Using",
textName= "Play pool",
slAnim= anim_AIM_BAZOOKA_R]).

moo:defined_affordance([objType= "Barrier",
stringMatch= " * Wall * ",
stringMatch= " * Fence * ",
stringMatch= " * Pillar * ",
stringMatch= " * Roof * ",
stringMatch= " * Beam * "]).

moo:defined_affordance([objType= "Shelf",
stringMatch= " * cupboard",
stringMatch= " * Cabinet",
stringMatch= " * cabinate",
stringMatch= " * FoodStore",
actionVerb= "Put_On"]).

moo:defined_affordance([objType= "Desk",
stringMatch= " * Lab Bench",
stringMatch= " * workbench",
stringMatch= " * officedesk",
actionVerb= "Put_On"]).

moo:defined_affordance([objType= "Counter",stringMatch= "Bar",actionVerb= "Put_On"]).

moo:defined_affordance([objType= "Plastic_Container",stringMatch= "Plastic",actionVerb= "Put_On"]).

moo:defined_affordance([objType= "Table",
stringMatch= " * Coffee Table",
acceptsChild= readable,
acceptsChild= eatable,
actionVerb= "Put_On"]).

moo:defined_affordance([objType= "Trash_Container",
   stringMatch= "garbage * c",
   stringMatch= "trash * c",
   stringMatch= "trash * bin",
   stringMatch= "waste",
   stringMatch= "recycle * bin",
   acceptsChild= "Take",
   actionVerb= "Put_On"]).

moo:defined_affordance([objType= "Bookcase",
stringMatch= " * Bookcase",
stringMatch= " * Bookshelf",
stringMatch= " * Bookshelve",
acceptsChild= readable,
actionVerb= "Observe",
textName= "Browse books",
slAnim= anim_YES,
'Fun'= 10 * 10,
'Room'= 20 * 20]).

moo:defined_affordance([objType= readable,
stringMatch= "Book",
stringMatch= "Magazine",
actionVerb= "Observe",
textName= "Read book",
slGrab= true,
slAnim= anim_LAUGH_SHORT,
'Fun'= 10 * 10,
'Room'= 20 * 20,
actionVerb= "Take",
textName= "Take the materials"]).

moo:defined_affordance([objType= eatable,
'AcceptsParent'= "Avatar",
actionVerb= "Eat",
textName= "Eat the food",
slAnim= anim_DRINK,
actionVerb= "Take",
textName= "Take the food"]).

moo:defined_affordance([objType= "Art",
stringMatch= "Art  * ",
actionVerb= "Observe",
textName= "Apreciate the Art",
slAnim= anim_YES_HAPPY,
'Fun'= 10 * 10,
'Room'= 20 * 20]).

moo:defined_affordance([objType= "Dance",
actionVerb= "Play_Using",
textName= "Dance! Dance!",
slAnim= anim_DANCE2]).

moo:defined_affordance([objType= "Computer",
stringMatch= "keyboard",
stringMatch= "keypad",
stringMatch= "workstation",
stringMatch= "Monitor",
actionVerb= "Play_Using",
textName= "Look busy doing something!",
slAnim= anim_TYPE]).

moo:defined_affordance([objType= agent,
actionVerb= "Talk_To",
'Social'= 10 * 15,
'Fun'= 1 * 1,
actionVerb= "thump",
'Social'= 10 * 15,
'Energy'= 0 * -10,
'GenerallySadToHappy'= -10 * -10,
'Fun'= 20 * 10,
actionVerb= "Push",
'Social'= 10 * 15,
'Energy'= 0 * -10,
'GenerallySadToHappy'= 0 * -10,
'Fun'= 20 * 10,
actionVerb= "Kiss",
'Social'= 10 * 15,
'GenerallySadToHappy'= 10 * 10,
'Fun'= 10 * 10]).

moo:defined_affordance([objType= touchable,actionVerb= "Touch",
textName= "Touch",
slGrab= true,
'Fun'= 1 * 1,
'Room'= 1 * 1]).

moo:defined_affordance([objType= sittable,actionVerb= "Sit_On",
textName= "Sit on",
slSit= true,
'Fun'= 1 * 1,
'Room'= 1 * 1]).

moo:defined_affordance([objType= hasSurface,actionVerb= "Put_On",
textName= "This is a Put_On placeholder",
slAnim= anim_FINGER_WAG,
'Fun'= -2 * 2,
'Energy'= 0 * -1]).

moo:defined_affordance([objType= sittable,actionVerb= "Relax_On",
textName= "Sit on",
slSit= true,
slAnim= anim_SIT,
'Comfort'= 1 * 0]).

moo:defined_affordance([objType= consumable,actionVerb= "Eat",
textName= "Eat it",
isDestroyedOnUse= true,
'Hunger'= 100 * 100,
'Hygiene'= 0 * -10]).

moo:defined_affordance([objType= takeable,actionVerb= "Take",
textName= "Take it",
'AcceptsParent'= "Avatar"]).

moo:defined_affordance([objType= sittable,actionVerb= "Sleep_On",
   textName= "Lay on",
   slSit= true,
   slAnim= anim_SLEEP,
   'Comfort'= 5 * 5,
   'Energy'= 20 * 20]).

moo:defined_affordance([alsoType= visible,actionVerb= "Clean",
   textName= "Clean",
   slAnim= anim_FINGER_WAG,
   'Fun'= -2 * 2,
   'Energy'= 0 * -1]).

moo:defined_affordance([alsoType= visible,actionVerb= "Observe",
   textName= "Observe",
   maximumDistance= 5,
   slAnim= anim_CLAP,
   'Fun'= 2 * 1,
   'Energy'= 0 * -1]).

moo:defined_affordance([objType= takeable,actionVerb= "Clean_Body_Using",
   textName= "Wash",
   slAnim= anim_RPS_PAPER,
   'Comfort'= 0 * 10,
   'Hygiene'= 20 * 10]).

moo:defined_affordance([objType= sittable,actionVerb= "Excersize_using",
textName= "Excersize",
slAnim= anim_ONETWO_PUNCH,
'Fun'= 10 * 10,
'Hygiene'= -10 * -10]).

moo:defined_affordance([objType= agent,actionVerb= "Play_Using",
textName= "Play with",
slAnim= anim_SHOOT_BOW_L,
alsoType= visible,
'Energy'= -10 * -10,
'Fun'= 20 * 10]).

moo:defined_affordance([objType= container,actionVerb= "RetrieveFood",
textName= "Eat from",
slAnim= anim_DRINK,
'Hygiene'= 0 * -5,
'Hunger'= 40 * 20]).

moo:defined_affordance([objType= agent,actionVerb= "thump",
textName= "Beat up",
alsoType= visible,
slAnim= anim_ONETWO_PUNCH,
'Energy'= -11 * -20]).

moo:defined_affordance([objType= agent,actionVerb= "Talk_To",
textName= "Talk to",
maximumDistance= 3,
alsoType= visible,
slAnim= anim_TALK,
'Social'= 11 * 20]).

moo:defined_affordance([objType= agent,actionVerb= "Push",
textName= "Beat up",
slAnim= anim_SWORD_STRIKE,
'Energy'= -11 * -20]).

moo:defined_affordance([objType= agent,actionVerb= "Kiss",
textName= "Kiss",
slAnim= anim_BLOW_KISS,
'Social'= 11 * 20,
'Fun'= 21 * 20]).

moo:defined_affordance([objType= visible,actionVerb= "Think_About",
textName= "Think about",
slAnim= anim_SHRUG,
'Fun'= 1 * 2]).

to_concept(S,V):-string_lower(S,SL),atom_string(V,SL).

do_define_affordance(LIST):-(member(objType= SType,LIST);member(alsoType= SType,LIST)),to_concept(SType,Type),!,do_define_type_affordance(Type,LIST).

do_define_type_affordance1(Type,_= Type):-!.
do_define_type_affordance1(Type,actionVerb= SVerb):-to_concept(SVerb,Verb),nb_setval(actionVerb,Verb),!.
do_define_type_affordance1(Type,SPred= Wants * Gets):-to_concept(SPred,Pred),nb_getval(actionVerb,Verb),assert_if_new(verb_affordance(Type,Verb,Pred,Wants,Gets)).
do_define_type_affordance1(Type,maximumDistance= String):-nb_getval(actionVerb,Verb),assert_if_new(maximumDistance(Type,Verb,String)).
do_define_type_affordance1(Type,textSitName= String):-do_define_type_affordance1(Type,textName= String).
do_define_type_affordance1(Type,textName= String):-nb_getval(actionVerb,Verb),assert_if_new(verb_desc(Type,Verb,String)).
do_define_type_affordance1(Type,stringMatch= String):-assert_if_new(type_desc(Type,String)).
do_define_type_affordance1(Type,Skipped=_):-atom_concat('sl',_,Skipped).
do_define_type_affordance1(Type,Skipped):-dmsg(skipped(do_define_type_affordance1(Type,Skipped))).

do_define_type_affordance(Type,[]).
do_define_type_affordance(Type,[H|LIST]):-do_define_type_affordance1(Type,H),!,do_define_type_affordance(Type,LIST),!.

moo:action_info(Say,text("invokes",Does)):-socialCommand(Say,_SocialVerb,Does).
moo:agent_call_command(Agent,attack(Dir)).

:-forall(moo:defined_affordance(Attrs),show_call(do_define_affordance(Attrs))).
:-listing(verb_desc).
:-listing(verb_affordance).

/*

Yields

:- dynamic verb_desc/3.

verb_desc(shower, clean_body_using, "Take a Shower").
verb_desc(bath, clean_body_using, "Take a Bath").
verb_desc(sink, clean_body_using, "Wash Hands").
verb_desc(danceball, dance, "Dance! Dance!").
verb_desc(washing_machine, washtheclothes, "Wash The Clothes").
verb_desc(clothesdryer, drytheclothes, "Dry The Clothes").
verb_desc(bed, sleep_on, "Sleep a few").
verb_desc(mattress, sleep_on, "Sleep a few").
verb_desc(chair, relax_on, "Sit down").
verb_desc(couch, relax_on, "Sit down").
verb_desc(television, observe, "Watch TV").
verb_desc(radio, observe, "Listen to Radio").
verb_desc(mirror, observe, "Pop your zits").
verb_desc(toilet, relax_on, "Go potty").
verb_desc(toilet, clean, "Flush it").
verb_desc(refrigerator, retrievefood, "Raid the fridge").
verb_desc(stove, retrievefood, "Raid the fridge").
verb_desc(microwave, retrievefood, "see what was forgotten in the microwave").
verb_desc(treadmill, excersize_using, "Tread the mill").
verb_desc(fixedlamp, touch, "flip the switch").
verb_desc(pooltable, play_using, "Play pool").
verb_desc(bookcase, observe, "Browse books").
verb_desc(readable, observe, "Read book").
verb_desc(readable, take, "Take the materials").
verb_desc(eatable, eat, "Eat the food").
verb_desc(eatable, take, "Take the food").
verb_desc(art, observe, "Apreciate the Art").
verb_desc(dance, play_using, "Dance! Dance!").
verb_desc(computer, play_using, "Look busy doing something!").
verb_desc(touchable, touch, "Touch").
verb_desc(sittable, sit_on, "Sit on").
verb_desc(hassurface, put_on, "This is a Put_On placeholder").
verb_desc(sittable, relax_on, "Sit on").
verb_desc(consumable, eat, "Eat it").
verb_desc(takeable, take, "Take it").
verb_desc(sittable, sleep_on, "Lay on").
verb_desc(visible, clean, "Clean").
verb_desc(visible, observe, "Observe").
verb_desc(takeable, clean_body_using, "Wash").
verb_desc(sittable, excersize_using, "Excersize").
verb_desc(agent, play_using, "Play with").
verb_desc(container, retrievefood, "Eat from").
verb_desc(agent, thump, "Beat up").
verb_desc(agent, talk_to, "Talk to").
verb_desc(agent, push, "Beat up").
verb_desc(agent, kiss, "Kiss").
verb_desc(visible, think_about, "Think about").

:- dynamic verb_affordance/5.

verb_affordance(furnature, bumpintobarrier, social, -300, 0).
verb_affordance(furnature, bumpintobarrier, hygiene, -300, 0).
verb_affordance(furnature, bumpintobarrier, comfort, -300, 0).
verb_affordance(furnature, bumpintobarrier, energy, -300, 0).
verb_affordance(furnature, bumpintobarrier, fun, -300, 0).
verb_affordance(self, liveatleastaminute, energy, 100, -1).
verb_affordance(self, liveatleastaminute, unbored, 100, -100).
verb_affordance(self, liveatleastaminute, hunger, 100, -1).
verb_affordance(self, liveatleastaminute, bladder, 100, -1).
verb_affordance(self, liveatleastaminute, hygiene, 100, 0).
verb_affordance(self, liveatleastaminute, room, 100, -1).
verb_affordance(self, liveatleastaminute, social, 100, -1).
verb_affordance(self, liveatleastaminute, fun, 100, -1).
verb_affordance(self, liveatleastaminute, generallysadtohappy, 100, -1).
verb_affordance(self, liveatleastaminute, comfort, 100, -1).
verb_affordance(shower, clean_body_using, comfort, 10, 10).
verb_affordance(shower, clean_body_using, hygiene, 30, 30).
verb_affordance(bath, clean_body_using, comfort, 20, 20).
verb_affordance(bath, clean_body_using, hygiene, 100, 100).
verb_affordance(sink, clean_body_using, comfort, 0, 0).
verb_affordance(sink, clean_body_using, hygiene, 10, 10).
verb_affordance(danceball, dance, social, 10, 10).
verb_affordance(danceball, dance, fun, 10, 10).
verb_affordance(danceball, dance, hygiene, -10, -10).
verb_affordance(washing_machine, washtheclothes, comfort, 0, 0).
verb_affordance(washing_machine, washtheclothes, hygiene, 10, 10).
verb_affordance(clothesdryer, drytheclothes, comfort, 0, 0).
verb_affordance(clothesdryer, drytheclothes, hygiene, 10, 10).
verb_affordance(bed, sleep_on, comfort, 10, 30).
verb_affordance(bed, sleep_on, energy, 100, 80).
verb_affordance(mattress, sleep_on, comfort, 10, 30).
verb_affordance(mattress, sleep_on, energy, 100, 80).
verb_affordance(chair, relax_on, comfort, 15, 10).
verb_affordance(chair, relax_on, energy, 10, 20).
verb_affordance(couch, relax_on, comfort, 20, 20).
verb_affordance(couch, relax_on, energy, 10, 20).
verb_affordance(television, observe, hunger, 1, -1).
verb_affordance(television, observe, bladder, 0, 0).
verb_affordance(television, observe, hygiene, 0, 0).
verb_affordance(television, observe, room, 1, 0).
verb_affordance(television, observe, social, 2, -1).
verb_affordance(television, observe, fun, 2, 1).
verb_affordance(television, observe, generallysadtohappy, 2, 1).
verb_affordance(television, observe, energy, 1, -1).
verb_affordance(radio, observe, room, 1, 0).
verb_affordance(radio, observe, fun, 10, 10).
verb_affordance(radio, observe, generallysadtohappy, 10, 10).
verb_affordance(radio, observe, energy, 1, -1).
verb_affordance(mirror, observe, room, 1, 0).
verb_affordance(mirror, observe, fun, 10, 10).
verb_affordance(mirror, observe, generallysadtohappy, 10, -1).
verb_affordance(mirror, observe, energy, 1, -1).
verb_affordance(toilet, relax_on, bladder, 100, 100).
verb_affordance(toilet, relax_on, hygiene, 0, -10).
verb_affordance(toilet, clean, hygiene, 1, 4).
verb_affordance(toilet, clean, fun, 5, 4).
verb_affordance(bookcase, observe, fun, 10, 10).
verb_affordance(bookcase, observe, room, 20, 20).
verb_affordance(readable, observe, fun, 10, 10).
verb_affordance(readable, observe, room, 20, 20).
verb_affordance(art, observe, fun, 10, 10).
verb_affordance(art, observe, room, 20, 20).
verb_affordance(agent, talk_to, social, 10, 15).
verb_affordance(agent, talk_to, fun, 1, 1).
verb_affordance(agent, thump, social, 10, 15).
verb_affordance(agent, thump, energy, 0, -10).
verb_affordance(agent, thump, generallysadtohappy, -10, -10).
verb_affordance(agent, thump, fun, 20, 10).
verb_affordance(agent, push, social, 10, 15).
verb_affordance(agent, push, energy, 0, -10).
verb_affordance(agent, push, generallysadtohappy, 0, -10).
verb_affordance(agent, push, fun, 20, 10).
verb_affordance(agent, kiss, social, 10, 15).
verb_affordance(agent, kiss, generallysadtohappy, 10, 10).
verb_affordance(agent, kiss, fun, 10, 10).
verb_affordance(touchable, touch, fun, 1, 1).
verb_affordance(touchable, touch, room, 1, 1).
verb_affordance(sittable, sit_on, fun, 1, 1).
verb_affordance(sittable, sit_on, room, 1, 1).
verb_affordance(hassurface, put_on, fun, -2, 2).
verb_affordance(hassurface, put_on, energy, 0, -1).
verb_affordance(sittable, relax_on, comfort, 1, 0).
verb_affordance(consumable, eat, hunger, 100, 100).
verb_affordance(consumable, eat, hygiene, 0, -10).
verb_affordance(sittable, sleep_on, comfort, 5, 5).
verb_affordance(sittable, sleep_on, energy, 20, 20).
verb_affordance(visible, clean, fun, -2, 2).
verb_affordance(visible, clean, energy, 0, -1).
verb_affordance(visible, observe, fun, 2, 1).
verb_affordance(visible, observe, energy, 0, -1).
verb_affordance(takeable, clean_body_using, comfort, 0, 10).
verb_affordance(takeable, clean_body_using, hygiene, 20, 10).
verb_affordance(sittable, excersize_using, fun, 10, 10).
verb_affordance(sittable, excersize_using, hygiene, -10, -10).
verb_affordance(agent, play_using, energy, -10, -10).
verb_affordance(agent, play_using, fun, 20, 10).
verb_affordance(container, retrievefood, hygiene, 0, -5).
verb_affordance(container, retrievefood, hunger, 40, 20).
verb_affordance(agent, thump, energy, -11, -20).
verb_affordance(agent, talk_to, social, 11, 20).
verb_affordance(agent, push, energy, -11, -20).
verb_affordance(agent, kiss, social, 11, 20).
verb_affordance(agent, kiss, fun, 21, 20).
verb_affordance(visible, think_about, fun, 1, 2).
*/

:-prolog.

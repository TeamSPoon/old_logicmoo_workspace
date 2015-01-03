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

% See the the seemingly white (not dirrectly usable) in some usefull way
defined_affordance([objType= "Passable",actionVerb= "TravelThru"]).


defined_affordance([objType= "Television",
stringMatch= "TV",
actionVerb= "Observe",
maximumDistance= 4,
'NonLoneliness_Social'= 3 * -2, % this form means the AI player thinks observing a TV will satisfy their NonLoneliness_Social needs by 3% .. yet instead, it reduces by 2%
'NonHunger'= 1 * -1, 
'BladderEmpty'= 0 * 0,
'Hygiene'= 0 * 0,
'Secure_Room'= 1 * 0,
'Fun'= 2 * 1,
'Sad_To_Happy'= 2 * 1,
'Energy'= 1 * -1]).

defined_affordance([objType= "Door",
stringMatch= " * doorway",
stringMatch= "gate",
stringMatch= "Curtain",
superType= "Passable"]).

defined_affordance([objType= "Floor",
stringMatch= "floor",
superType= "Passable"]).

defined_affordance([objType= "Ladder",
stringMatch= "ladder",
superType= "Passable"]).

% looks like a very bad idea but not (this is mainly for testing the system)
defined_affordance([objType= tFurniture,actionVerb= "BumpIntoBarrier",
'NonLoneliness_Social'= -300 * 0,
'Hygiene'= -300 * 0,
'Comfort'= -300 * 0,
'Energy'= -300 * 0,
'Fun'= -300 * 0]).

% yet every minute you are alive, god wishes to punish you
defined_affordance([objType= self,actionVerb= "LiveAtLeastAMinute",
   maximumDistance= 2000,
   'Energy'= 0 * -1,
   'NonHunger'= 0 * -1,
   'BladderEmpty'= 0 * -1,
   'Hygiene'= 0 * -1,
   'Secure_Room'= 0 * -1,
   'NonLoneliness_Social'= 0 * -1,
   'Fun'= 0 * -1,
   'Sad_To_Happy'= 0 * -1,
   'Comfort'= 0 * -1 ]).

defined_affordance([objType= "Shower",
actionVerb= "Operate",
slAnim= anim_AFRAID,
textName= "wash self with X",
textName= "take a shower",
'Comfort'= 10 * 10,
'Hygiene'= 30 * 30,
actionVerb= "Clean"]).

defined_affordance([objType= "Bathtub",
stringMatch= "bath",
stringMatch= "bathtub",
dontMatch= "Plastic Tub",
actionVerb= "Operate",
textName= "wash self with X",
textName= "Take a Bath",
slSit= true,
'Comfort'= 20 * 20,
'Hygiene'= 100 * 100,
actionVerb= "Clean"]).

defined_affordance([objType= "Sink",
actionVerb= "Operate",
textName= "Wash Hands",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

defined_affordance([objType= "BigThing",require(mudSize > 8)]).

% defined_affordance([actionVerb= "Attach_To_Self",textName= "Attach it",slAnim= anim_RPS_PAPER,'Comfort'= 20 * 20,'LispScript'= "(progn (TheBot.Attach_To_Self Target))"]).

defined_affordance([objType= "DanceBall",
actionVerb= "Dance",
textName= "Dance! Dance!",
is_To_uchDefined= true,
slAnim= anim_DANCE,
'NonLoneliness_Social'= 10 * 10,
'Fun'= 10 * 10,
'Hygiene'= -10 * -10]).

defined_affordance([objType= "PoseBall",
stringMatch= " * pose",
stringMatch= " * Pose",
is_To_uchDefined= true]).

defined_affordance([objType= tWashingMachine,
stringMatch= "Washing Machine",
actionVerb= "operate",
textName= "Wash The Clothes",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

defined_affordance([objType= "clothes_dryer",
stringMatch= "Dryer",
actionVerb= "operate",
textName= "Dry The Clothes",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

defined_affordance([objType= "Bed",
actionVerb= "Sleep",
textSitName= "Sleep a few",
slSit= true,
slAnim= anim_SLEEP,
'Comfort'= 10 * 30,
'Energy'= 100 * 80]).

defined_affordance([objType= "Mattress",
actionVerb= "Sleep",
textSitName= "Sleep a few",
slSit= true,
slAnim= anim_SLEEP,
'Comfort'= 10 * 30,
'Energy'= 100 * 80]).

defined_affordance([objType= "Chair",
stringMatch= " * chair",
stringMatch= " * stool",
stringMatch= " * recliner",
actionVerb= "sit",
textSitName= "Sit down",
slSit= true,
slAnim= anim_SMOKE_IDLE,
'Comfort'= 15 * 10,
'Energy'= 10 * 20]).

defined_affordance([objType= "Couch",
stringMatch= "Sofa",
stringMatch= " * luvseat * ",
stringMatch= " * loveseat * ",
actionVerb= "sit",
textSitName= "Sit down",
slSit= true,
slAnim= anim_SMOKE_IDLE,
'Comfort'= 20 * 20,
'Energy'= 10 * 20]).

defined_affordance([objType= "Radio",
actionVerb= "Observe",
textName= "Listen to Radio",
maximumDistance= 4,
'Secure_Room'= 1 * 0,
'Fun'= 10 * 10,
'Sad_To_Happy'= 10 * 10,
'Energy'= 1 * -1]).

defined_affordance([objType= "Mirror",
actionVerb= "Observe",
textName= "Pop your zits",
maximumDistance= 2,
'Secure_Room'= 1 * 0,
'Fun'= 10 * 10,
'Sad_To_Happy'= 10 * -1,
'Energy'= 1 * -1]).

defined_affordance([objType= "Toilet",
actionVerb= "potty",
textSitName= "Go potty",
slSit= true,
'BladderEmpty'= 100 * 100,
'Hygiene'= 0 * -10,
actionVerb= "Clean",
textName= "Flush it",
slAnim= anim_POINT_YOU,
'Hygiene'= 1 * 4,
'Fun'= 5 * 4]).

defined_affordance([objType= "fridge",
stringMatch= " * Fridge * ",
stringMatch= " * Frige * ",
stringMatch= " * icebox",
actionVerb= "search",
textName= "Raid the fridge",
slAnim= anim_DRINK,
slGrab= true]).

defined_affordance([objType= "Stove",
stringMatch= "Oven",
stringMatch= " * kitchen range",
actionVerb= "operate",
slAnim= anim_DRINK,
slGrab= true]).

defined_affordance([objType= "Microwave",
actionVerb= "operate",
textName= "see what was forgotten in the microwave",
slAnim= anim_DRINK,
slGrab= true]).

defined_affordance([objType= "Treadmill",
   actionVerb= "Operate",
   textName= "Excersize with X",
   textName= "Tread the mill",
   slSit= true]).

defined_affordance([objType= "FixedLamp",
   stringMatch= " * floorlamp",
   stringMatch= "lamp",
   stringMatch= "lantern",
   stringMatch= "lightbulb",
   stringMatch= "lighting",
   actionVerb= "Operate",
   textName= "flip the switch",
   slAnim= anim_AIM_BAZOOKA_R]).

defined_affordance([objType= "Pooltable",
stringMatch= " * pool table * ",
actionVerb= "Operate",
textName= "Play pool",
slAnim= anim_AIM_BAZOOKA_R]).

defined_affordance([objType= "Barrier",
stringMatch= " * Wall * ",
stringMatch= " * Fence * ",
stringMatch= " * Pillar * ",
stringMatch= " * Roof * ",
stringMatch= " * Beam * "]).

defined_affordance([objType= "Shelf",
stringMatch= " * cupboard",
stringMatch= " * Cabinet",
stringMatch= " * cabinate",
stringMatch= " * FoodStore",
actionVerb= "Put_X_On"]).

defined_affordance([objType= "Desk",
stringMatch= " * Lab Bench",
stringMatch= " * workbench",
stringMatch= " * officedesk",
actionVerb= "Put_X_On"]).

defined_affordance([objType= "Counter",stringMatch= "Bar",actionVerb= "Put_X_On"]).

defined_affordance([objType= "Container",stringMatch= "Plastic",actionVerb= "Put_X_In"]).

defined_affordance([objType= "Table",
stringMatch= " * Coffee Table",
acceptsChild= tReadable,
acceptsChild= tEatable,
actionVerb= "Put_X_On"]).

defined_affordance([objType= "Trash_Container",
   stringMatch= "garbage * c",
   stringMatch= "trash * c",
   stringMatch= "trash * bin",
   stringMatch= "waste",
   stringMatch= "recycle * bin",
   acceptsChild= "Take",
   actionVerb= "Put_X_In"]).

defined_affordance([objType= "Bookcase",
stringMatch= " * Bookcase",
stringMatch= " * Bookshelf",
stringMatch= " * Bookshelve",
acceptsChild= tReadable,
actionVerb= "Put_X_On",
textName= "Browse books",
slAnim= anim_YES,
'Fun'= 10 * 10,
'Secure_Room'= 20 * 20]).

defined_affordance([objType= tReadable,
stringMatch= "Book",
stringMatch= "Magazine",
actionVerb= "Observe",
textName= "Read book",
slGrab= true,
slAnim= anim_LAUGH_SHORT,
'Fun'= 10 * 10,
'Secure_Room'= 20 * 20,
actionVerb= "Take",
textName= "Take the materials"]).

defined_affordance([objType= tEatable,
'AcceptsParent'= "Avatar",
actionVerb= "Eat",
textName= "Eat the food",
slAnim= anim_DRINK,
actionVerb= "Take",
textName= "Take the food"]).

defined_affordance([objType= "Art",
stringMatch= "Art  * ",
actionVerb= "Observe",
textName= "Apreciate the Art",
slAnim= anim_YES_HAPPY,
'Fun'= 10 * 10,
'Secure_Room'= 20 * 20]).

defined_affordance([objType= "Dance_floor",
actionVerb= "Operate",
textName= "Dance! Dance!",
slAnim= anim_DANCE2]).

defined_affordance([objType= "Computer",
stringMatch= "keyboard",
stringMatch= "keypad",
stringMatch= "workstation",
stringMatch= "Monitor",
actionVerb= "Operate",
textName= "Look busy doing something!",
slAnim= anim_TYPE]).

defined_affordance([objType= tAgentGeneric,
actionVerb= "Talk",
'NonLoneliness_Social'= 10 * 15,
'Fun'= 1 * 1,
actionVerb= "Argue",
'NonLoneliness_Social'= 10 * 15,
'Energy'= 0 * -10,
'Sad_To_Happy'= -10 * -10,
'Fun'= 20 * 10,
actionVerb= "Attack",
'NonLoneliness_Social'= 10 * 15,
'Energy'= 0 * -10,
'Sad_To_Happy'= 0 * -10,
'Fun'= 20 * 10,
actionVerb= "Kiss",
'NonLoneliness_Social'= 10 * 15,
'Sad_To_Happy'= 10 * 10,
'Fun'= 10 * 10]).

defined_affordance([objType= touchable,actionVerb= "Touch",
textName= "Touch",
slGrab= true,
'Fun'= 1 * 1,
'Secure_Room'= 1 * 1]).

defined_affordance([objType= tSittable,actionVerb= "Sit",
textName= "Sit on",
slSit= true,
slAnim= anim_SIT,
'Comfort'= 1 * 0,
'Fun'= 1 * 1,
'Secure_Room'= 1 * 1]).

defined_affordance([objType= hasSurface,actionVerb= "Put_X_On",
textName= "This is a Put_X_On placeholder",
slAnim= anim_FINGER_WAG,
'Fun'= -2 * 2,
'Energy'= 0 * -1]).


defined_affordance([objType= tConsumable,actionVerb= "Eat",
textName= "Eat it",
isDestroyedOnUse= true,
'NonHunger'= 100 * 100,
'Hygiene'= 0 * -10]).

defined_affordance([objType= tTakeable,actionVerb= "Take", 
textName= "Take it",
'AcceptsParent'= "Avatar"]).

defined_affordance([objType= tLayable,actionVerb= "Sleep",
   textName= "Lay on",
   slSit= true,
   slAnim= anim_SLEEP,
   'Comfort'= 5 * 5,
   'Energy'= 20 * 20]).

defined_affordance([alsoType= tLookable,actionVerb= "Clean",
   textName= "Clean",
   slAnim= anim_FINGER_WAG,
   'Fun'= -2 * 2,
   'Energy'= 0 * -1]).

defined_affordance([alsoType= tLookable,actionVerb= "Observe",
   textName= "Observe",
   maximumDistance= 5,
   slAnim= anim_CLAP,
   'Fun'= 2 * 1,
   'Energy'= 0 * -1]).

defined_affordance([objType= tSittable,actionVerb= "Excersize",
textName= "Excersize",
slAnim= animETWO_PUNCH,
'Fun'= 10 * 10,
'Hygiene'= -10 * -10]).

defined_affordance([objType= tAgentGeneric,actionVerb= "tickle",
textName= "Play with",
slAnim= anim_SHOOT_BOW_L,
alsoType= tLookable,
'Energy'= -10 * -10,
'Fun'= 20 * 10]).

defined_affordance([objType= tContainer,actionVerb= "search",
textName= "Eat_from",
slAnim= anim_DRINK,
'Hygiene'= 0 * -5,
'NonHunger'= 40 * 20]).

defined_affordance([objType= tAgentGeneric,actionVerb= "Argue",
textName= "Argue",
alsoType= tLookable,
slAnim= anim_ONETWO_PUNCH,
'Energy'= -11 * -20]).

defined_affordance([objType= tAgentGeneric,actionVerb= "Talk",
textName= "Talk to",
maximumDistance= 3,
alsoType= tLookable,
slAnim= anim_TALK,
'NonLoneliness_Social'= 11 * 20]).

defined_affordance([objType= tAgentGeneric,actionVerb= "Attack",
textName= "Beat up",
slAnim= anim_SWORD_STRIKE,
'Energy'= -11 * -20]).

defined_affordance([objType= tAgentGeneric,actionVerb= "Kiss",
textName= "Kiss",
slAnim= anim_BLOW_KISS,
'NonLoneliness_Social'= 11 * 20,
'Fun'= 21 * 20]).

defined_affordance([objType= tLookable,actionVerb= "Think_About",
textName= "Think about",
slAnim= anim_SHRUG,
'Fun'= 1 * 2]).

to_concept(S,V):-string_lower(S,SL),atom_string(V,SL).

to_personal(energy,energy).
to_personal(Pred,APred):-atom_concat('personal_',Pred,APred).

do_define_affordance(LIST):-(member(objType= SType,LIST);member(alsoType= SType,LIST)),to_concept(SType,Type),!,do_define_type_affordance(Type,LIST).

do_define_type_affordance1(Type,_= Type):-!.
do_define_type_affordance1(Type,actionVerb= SVerb):-to_concept(SVerb,Verb),nb_setval(actionVerb,Verb),!,assert_if_new(verb_for_type(Verb,Type)).
do_define_type_affordance1(Type,SPred= Wants * Gets):-to_concept(SPred,Pred),nb_getval(actionVerb,Verb),to_personal(Pred,APred),assert_if_new(verb_affordance(Verb,Type,APred,Wants,Gets)).
do_define_type_affordance1(Type,maximumDistance= String):-nb_getval(actionVerb,Verb),assert_if_new(maximumDistance(Verb,Type,String)).
do_define_type_affordance1(Type,textSitName= String):-do_define_type_affordance1(Type,textName= String).
do_define_type_affordance1(Type,textName= String):-nb_getval(actionVerb,Verb),assert_if_new(verb_desc(Verb,Type,String)).
do_define_type_affordance1(Type,stringMatch= String):-assert_if_new(type_desc(Type,String)).
do_define_type_affordance1(_,Skipped=_):-atom_concat('sl',_,Skipped).
do_define_type_affordance1(Type,Skipped):-dmsg(skipped(do_define_type_affordance1(Type,Skipped))).

do_define_type_affordance(_,[]).
do_define_type_affordance(Type,[H|LIST]):-do_define_type_affordance1(Type,H),!,do_define_type_affordance(Type,LIST),!.

world_agent_plan(_World,Agent,Act):-
   (mudIsa(Agent,simian);mudIsa(Agent,tAgentGeneric)),
   simian_idea(Agent,Act).

agent_available_type(_,Visible,Visible).

simian_ideas_possible(Agent,actTextcmd(Think_about,Visible)) :- verb_for_type(Think_about, Type),agent_available_type(Agent,Visible,Type).

simian_idea(Agent,Act):-
   findall(Act,simian_ideas_possible(Agent,Act),CMDS),choose_best(Agent,CMDS,Act).

% verb_affordance(Verb,Type,APred,Wants,Gets)
choose_best(_Agent,CMDS,Act):-random_permutation(CMDS,[Act|_]).


action_info(actTextcmd(tCommand,string),"reinterps a term as text").
agent_call_command(Agent,actTextcmd(A)):-sformat(CMD,'~w',[A]),!,do_player_action(Agent,CMD).
agent_call_command(Agent,actTextcmd(A,B)):-sformat(CMD,'~w ~w',[A,B]),!,do_player_action(Agent,CMD).
agent_call_command(Agent,actTextcmd(A,B,C)):-sformat(CMD,'~w ~w ~w',[A,B,C]),!,do_player_action(Agent,CMD).


:-forall(defined_affordance(Attrs),must(do_define_affordance(Attrs))).
:-listing(verb_desc).
:-listing(verb_affordance).
:-listing(verb_for_type).

mudSubclass(tShelf,tHassurface).
mudSubclass(tCounter,tHassurface).
mudSubclass(tEatable,tConsumable).
mudSubclass(tBar,tHassurface).
mudSubclass(tSittable,tHassurface).
mudSubclass(tSofa,tCouch).
mudSubclass(tCouch,tSittable).
mudSubclass(tChair,tSittable).
mudSubclass(tMattress,tLayable).
mudSubclass(tLayable,tSittable).
mudSubclass(tBed,tMattress).
mudSubclass(tCrib,tLayable).
mudSubclass(tHassurface, tContainer).
mudSubclass(tHassurface, tHasobjs).
mudSubclass(tContainer, tHasobjs).



mudSubclass(tClothesDryer,tFurniture).
mudSubclass(tWashingMachine,tFurniture).
mudSubclass(tShower,tFurniture).
mudSubclass(tSittable,tFurniture).
mudSubclass(tChair,tFurniture).
mudSubclass(tBed,tFurniture).
mudSubclass(tSink,tFurniture).
mudSubclass(tToilet,tFurniture).
mudSubclass(tBathtub,tFurniture).
mudSubclass(tFurniture,tUseable).
mudSubclass(tFurniture,tObj).

verb_alias(observe,actUse).
verb_alias(operate,actUse).

/*


Yields



:- dynamic verb_for_type/2.
verb_for_type(travelthru, passable).
verb_for_type(observe, television).
verb_for_type(bumpintobarrier, furniture).
verb_for_type(liveatleastaminute, self).
verb_for_type(operate, shower).
verb_for_type(clean, shower).
verb_for_type(operate, bathtub).
verb_for_type(clean, bathtub).
verb_for_type(operate, sink).
verb_for_type(clean, sink).
verb_for_type(dance, danceball).
verb_for_type(operate, washing_machine).
verb_for_type(clean, washing_machine).
verb_for_type(operate, clothes_dryer).
verb_for_type(clean, clothes_dryer).
verb_for_type(sleep, bed).
verb_for_type(sleep, mattress).
verb_for_type(sit, chair).
verb_for_type(sit, couch).
verb_for_type(observe, radio).
verb_for_type(observe, mirror).
verb_for_type(potty, toilet).
verb_for_type(clean, toilet).
verb_for_type(search, fridge).
verb_for_type(operate, stove).
verb_for_type(operate, microwave).
verb_for_type(operate, treadmill).
verb_for_type(operate, fixedlamp).
verb_for_type(operate, pooltable).
verb_for_type(put_x_on, shelf).
verb_for_type(put_x_on, desk).
verb_for_type(put_x_on, counter).
verb_for_type(put_x_in, container).
verb_for_type(put_x_on, table).
verb_for_type(put_x_in, trash_container).
verb_for_type(put_x_on, bookcase).
verb_for_type(observe, readable).
verb_for_type(take, readable).
verb_for_type(eat, eatable).
verb_for_type(take, eatable).
verb_for_type(observe, art).
verb_for_type(operate, dance_floor).
verb_for_type(operate, computer).
verb_for_type(talk, agent).
verb_for_type(argue, agent).
verb_for_type(attack, agent).
verb_for_type(kiss, agent).
verb_for_type(touch, touchable).
verb_for_type(sit, sittable).
verb_for_type(put_x_on, hassurface).
verb_for_type(eat, consumable).
verb_for_type(take, takeable).
verb_for_type(sleep, layable).
verb_for_type(clean, lookable).
verb_for_type(observe, lookable).
verb_for_type(excersize, sittable).
verb_for_type(tickle, agent).
verb_for_type(search, container).
verb_for_type(think_about, lookable).



*/
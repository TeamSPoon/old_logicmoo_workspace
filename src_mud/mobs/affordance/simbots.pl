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

% See the the seemingly white (not dirrectly usable) in some tUsefull way
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

% yet every minute you are alive, God wishes to punish you
defined_affordance([objType= isAgentSelf,actionVerb= "LiveAtLeastAMinute",
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
actionVerb= "actOperate",
textName= "Wash The Clothes",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

defined_affordance([objType= tClothesDryer,
stringMatch= "Dryer",
actionVerb= "actOperate",
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
actionVerb= "Sit",
textSitName= "Sit down",
slSit= true,
slAnim= anim_SMOKE_IDLE,
'Comfort'= 15 * 10,
'Energy'= 10 * 20]).

defined_affordance([objType= "Couch",
stringMatch= "Sofa",
stringMatch= " * luvseat * ",
stringMatch= " * loveseat * ",
actionVerb= "Sit",
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
actionVerb= "Potty",
textSitName= "Go potty",
slSit= true,
'BladderEmpty'= 100 * 100,
'Hygiene'= 0 * -10,
actionVerb= "Clean",
textName= "Flush it",
slAnim= anim_POINT_YOU,
'Hygiene'= 1 * 4,
'Fun'= 5 * 4]).

defined_affordance([objType= "Refrigerator",
stringMatch= " * Fridge * ",
stringMatch= " * Frige * ",
stringMatch= " * icebox",
actionVerb= "Search",
textName= "Raid the fridge",
slAnim= anim_DRINK,
slGrab= true]).

defined_affordance([objType= "Stove",
stringMatch= "Oven",
stringMatch= " * kitchen range",
actionVerb= "actOperate",
slAnim= anim_DRINK,
slGrab= true]).

defined_affordance([objType= "Microwave",
actionVerb= "actOperate",
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

defined_affordance([objType= tHasSurface,actionVerb= "Put_X_On",
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

defined_affordance([objType= tAgentGeneric,actionVerb= "Tickle",
textName= "Play with",
slAnim= anim_SHOOT_BOW_L,
alsoType= tLookable,
'Energy'= -10 * -10,
'Fun'= 20 * 10]).

defined_affordance([objType= tContainer,actionVerb= "Search",
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

:-retractall(verb_desc(_,_,_)).
:-retractall(verb_for_type(_,_)).

to_personal(mudCharge,mudCharge).
to_personal(Pred,APred):-atom_concat('',Pred,APred).

do_define_affordance(LIST):-(member(objType= SType,LIST);member(alsoType= SType,LIST)),typename_to_iname('t',SType,Type),!,do_define_type_affordance(Type,LIST).

do_define_type_affordance1(Type,_= Type):-!.
do_define_type_affordance1(Type,actionVerb= SVerb):-typename_to_iname(act,SVerb,Verb),nb_setval(actionVerb,Verb),!,assert_if_new(verb_for_type(Verb,Type)).
do_define_type_affordance1(Type,SPred= Wants * Gets):-typename_to_iname(mud,SPred,Pred),nb_getval(actionVerb,Verb),to_personal(Pred,APred),assert_if_new(verb_affordance(Verb,Type,APred,Wants,Gets)).
do_define_type_affordance1(Type,maximumDistance= String):-nb_getval(actionVerb,Verb),assert_if_new(maximumDistance(Verb,Type,String)).
do_define_type_affordance1(Type,textSitName= String):-do_define_type_affordance1(Type,textName= String).
do_define_type_affordance1(Type,textName= String):-nb_getval(actionVerb,Verb),assert_if_new(verb_desc(Verb,Type,String)).
do_define_type_affordance1(Type,stringMatch= String):-assert_if_new(type_desc(Type,String)).
do_define_type_affordance1(_,Skipped=_):-atom_concat('sl',_,Skipped).
do_define_type_affordance1(Type,Skipped):-dmsg(skipped(do_define_type_affordance1(Type,Skipped))).

do_define_type_affordance(_,[]).
do_define_type_affordance(Type,[H|LIST]):-do_define_type_affordance1(Type,H),!,do_define_type_affordance(Type,LIST),!.

world_agent_plan(_World,Agent,Act):-
   (mudIsa(Agent,tSimian);mudIsa(Agent,tAgentGeneric)),
   simian_idea(Agent,Act).

agent_available_type(_,Visible,Visible).

simian_ideas_possible(Agent,actTextcmd(Think_about,Visible)) :- verb_for_type(Think_about, Type),agent_available_type(Agent,Visible,Type).

simian_idea(Agent,Act):-
   findall(Act,simian_ideas_possible(Agent,Act),CMDS),choose_best(Agent,CMDS,Act).

% verb_affordance(Verb,Type,APred,Wants,Gets)
choose_best(_Agent,CMDS,Act):-random_permutation(CMDS,[Act|_]).


action_info(actTextcmd(ftString),"reinterps a term as text").
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

verb_alias("observe",actUse).
verb_alias("operate",actUse).

/*


Yields

:- dynamic verb_desc/3.
:- dynamic verb_desc/3.

verb_desc(actOperate, tShower, "wash self with X").
verb_desc(actOperate, tShower, "take a shower").
verb_desc(actOperate, tBathtub, "wash self with X").
verb_desc(actOperate, tBathtub, "Take a Bath").
verb_desc(actOperate, tSink, "Wash Hands").
verb_desc(actDance, tDanceBall, "Dance! Dance!").
verb_desc(actOperate, tWashingMachine, "Wash The Clothes").
verb_desc(actOperate, tClothesDryer, "Dry The Clothes").
verb_desc(actSleep, tBed, "Sleep a few").
verb_desc(actSleep, tMattress, "Sleep a few").
verb_desc(actSit, tChair, "Sit down").
verb_desc(actSit, tCouch, "Sit down").
verb_desc(actObserve, tRadio, "Listen to Radio").
verb_desc(actObserve, tMirror, "Pop your zits").
verb_desc(actPotty, tToilet, "Go potty").
verb_desc(actClean, tToilet, "Flush it").
verb_desc(actSearch, tRefrigerator, "Raid the fridge").
verb_desc(actOperate, tMicrowave, "see what was forgotten in the microwave").
verb_desc(actOperate, tTreadmill, "Excersize with X").
verb_desc(actOperate, tTreadmill, "Tread the mill").
verb_desc(actOperate, tFixedLamp, "flip the switch").
verb_desc(actOperate, tPooltable, "Play pool").
verb_desc(actPutXOn, tBookcase, "Browse books").
verb_desc(actObserve, tReadable, "Read book").
verb_desc(actTake, tReadable, "Take the materials").
verb_desc(actEat, tEatable, "Eat the food").
verb_desc(actTake, tEatable, "Take the food").
verb_desc(actObserve, tArt, "Apreciate the Art").
verb_desc(actOperate, tDanceFloor, "Dance! Dance!").
verb_desc(actOperate, tComputer, "Look busy doing something!").
verb_desc(actTouch, tTouchable, "Touch").
verb_desc(actSit, tSittable, "Sit on").
verb_desc(actPutXOn, tHasSurface, "This is a Put_X_On placeholder").
verb_desc(actEat, tConsumable, "Eat it").
verb_desc(actTake, tTakeable, "Take it").
verb_desc(actSleep, tLayable, "Lay on").
verb_desc(actClean, tLookable, "Clean").
verb_desc(actObserve, tLookable, "Observe").
verb_desc(actExcersize, tSittable, "Excersize").
verb_desc(actTickle, tAgentGeneric, "Play with").
verb_desc(actSearch, tContainer, "Eat_from").
verb_desc(actArgue, tAgentGeneric, "Argue").
verb_desc(actTalk, tAgentGeneric, "Talk to").
verb_desc(actAttack, tAgentGeneric, "Beat up").
verb_desc(actKiss, tAgentGeneric, "Kiss").
verb_desc(actThinkAbout, tLookable, "Think about").

:- dynamic verb_affordance/5.

verb_affordance(actObserve, tTelevision, mudNonLonelinessSocial, 3, -2).
verb_affordance(actObserve, tTelevision, mudNonHunger, 1, -1).
verb_affordance(actObserve, tTelevision, mudBladderEmpty, 0, 0).
verb_affordance(actObserve, tTelevision, mudHygiene, 0, 0).
verb_affordance(actObserve, tTelevision, mudSecureRoom, 1, 0).
verb_affordance(actObserve, tTelevision, mudFun, 2, 1).
verb_affordance(actObserve, tTelevision, mudSadToHappy, 2, 1).
verb_affordance(actObserve, tTelevision, mudEnergy, 1, -1).
verb_affordance(actBumpIntoBarrier, tFurniture, mudNonLonelinessSocial, -300, 0).
verb_affordance(actBumpIntoBarrier, tFurniture, mudHygiene, -300, 0).
verb_affordance(actBumpIntoBarrier, tFurniture, mudComfort, -300, 0).
verb_affordance(actBumpIntoBarrier, tFurniture, mudEnergy, -300, 0).
verb_affordance(actBumpIntoBarrier, tFurniture, mudFun, -300, 0).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudEnergy, 0, -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudNonHunger, 0, -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudBladderEmpty, 0, -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudHygiene, 0, -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudSecureRoom, 0, -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudNonLonelinessSocial, 0, -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudFun, 0, -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudSadToHappy, 0, -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudComfort, 0, -1).
verb_affordance(actOperate, tShower, mudComfort, 10, 10).
verb_affordance(actOperate, tShower, mudHygiene, 30, 30).
verb_affordance(actOperate, tBathtub, mudComfort, 20, 20).
verb_affordance(actOperate, tBathtub, mudHygiene, 100, 100).
verb_affordance(actOperate, tSink, mudComfort, 0, 0).
verb_affordance(actOperate, tSink, mudHygiene, 10, 10).
verb_affordance(actDance, tDanceBall, mudNonLonelinessSocial, 10, 10).
verb_affordance(actDance, tDanceBall, mudFun, 10, 10).
verb_affordance(actDance, tDanceBall, mudHygiene, -10, -10).
verb_affordance(actOperate, tWashingMachine, mudComfort, 0, 0).
verb_affordance(actOperate, tWashingMachine, mudHygiene, 10, 10).
verb_affordance(actOperate, tClothesDryer, mudComfort, 0, 0).
verb_affordance(actOperate, tClothesDryer, mudHygiene, 10, 10).
verb_affordance(actSleep, tBed, mudComfort, 10, 30).
verb_affordance(actSleep, tBed, mudEnergy, 100, 80).
verb_affordance(actSleep, tMattress, mudComfort, 10, 30).
verb_affordance(actSleep, tMattress, mudEnergy, 100, 80).
verb_affordance(actSit, tChair, mudComfort, 15, 10).
verb_affordance(actSit, tChair, mudEnergy, 10, 20).
verb_affordance(actSit, tCouch, mudComfort, 20, 20).
verb_affordance(actSit, tCouch, mudEnergy, 10, 20).
verb_affordance(actObserve, tRadio, mudSecureRoom, 1, 0).
verb_affordance(actObserve, tRadio, mudFun, 10, 10).
verb_affordance(actObserve, tRadio, mudSadToHappy, 10, 10).
verb_affordance(actObserve, tRadio, mudEnergy, 1, -1).
verb_affordance(actObserve, tMirror, mudSecureRoom, 1, 0).
verb_affordance(actObserve, tMirror, mudFun, 10, 10).
verb_affordance(actObserve, tMirror, mudSadToHappy, 10, -1).
verb_affordance(actObserve, tMirror, mudEnergy, 1, -1).
verb_affordance(actPotty, tToilet, mudBladderEmpty, 100, 100).
verb_affordance(actPotty, tToilet, mudHygiene, 0, -10).
verb_affordance(actClean, tToilet, mudHygiene, 1, 4).
verb_affordance(actClean, tToilet, mudFun, 5, 4).
verb_affordance(actPutXOn, tBookcase, mudFun, 10, 10).
verb_affordance(actPutXOn, tBookcase, mudSecureRoom, 20, 20).
verb_affordance(actObserve, tReadable, mudFun, 10, 10).
verb_affordance(actObserve, tReadable, mudSecureRoom, 20, 20).
verb_affordance(actObserve, tArt, mudFun, 10, 10).
verb_affordance(actObserve, tArt, mudSecureRoom, 20, 20).
verb_affordance(actTalk, tAgentGeneric, mudNonLonelinessSocial, 10, 15).
verb_affordance(actTalk, tAgentGeneric, mudFun, 1, 1).
verb_affordance(actArgue, tAgentGeneric, mudNonLonelinessSocial, 10, 15).
verb_affordance(actArgue, tAgentGeneric, mudEnergy, 0, -10).
verb_affordance(actArgue, tAgentGeneric, mudSadToHappy, -10, -10).
verb_affordance(actArgue, tAgentGeneric, mudFun, 20, 10).
verb_affordance(actAttack, tAgentGeneric, mudNonLonelinessSocial, 10, 15).
verb_affordance(actAttack, tAgentGeneric, mudEnergy, 0, -10).
verb_affordance(actAttack, tAgentGeneric, mudSadToHappy, 0, -10).
verb_affordance(actAttack, tAgentGeneric, mudFun, 20, 10).
verb_affordance(actKiss, tAgentGeneric, mudNonLonelinessSocial, 10, 15).
verb_affordance(actKiss, tAgentGeneric, mudSadToHappy, 10, 10).
verb_affordance(actKiss, tAgentGeneric, mudFun, 10, 10).
verb_affordance(actTouch, tTouchable, mudFun, 1, 1).
verb_affordance(actTouch, tTouchable, mudSecureRoom, 1, 1).
verb_affordance(actSit, tSittable, mudComfort, 1, 0).
verb_affordance(actSit, tSittable, mudFun, 1, 1).
verb_affordance(actSit, tSittable, mudSecureRoom, 1, 1).
verb_affordance(actPutXOn, tHasSurface, mudFun, -2, 2).
verb_affordance(actPutXOn, tHasSurface, mudEnergy, 0, -1).
verb_affordance(actEat, tConsumable, mudNonHunger, 100, 100).
verb_affordance(actEat, tConsumable, mudHygiene, 0, -10).
verb_affordance(actSleep, tLayable, mudComfort, 5, 5).
verb_affordance(actSleep, tLayable, mudEnergy, 20, 20).
verb_affordance(actClean, tLookable, mudFun, -2, 2).
verb_affordance(actClean, tLookable, mudEnergy, 0, -1).
verb_affordance(actObserve, tLookable, mudFun, 2, 1).
verb_affordance(actObserve, tLookable, mudEnergy, 0, -1).
verb_affordance(actExcersize, tSittable, mudFun, 10, 10).
verb_affordance(actExcersize, tSittable, mudHygiene, -10, -10).
verb_affordance(actTickle, tAgentGeneric, mudEnergy, -10, -10).
verb_affordance(actTickle, tAgentGeneric, mudFun, 20, 10).
verb_affordance(actSearch, tContainer, mudHygiene, 0, -5).
verb_affordance(actSearch, tContainer, mudNonHunger, 40, 20).
verb_affordance(actArgue, tAgentGeneric, mudEnergy, -11, -20).
verb_affordance(actTalk, tAgentGeneric, mudNonLonelinessSocial, 11, 20).
verb_affordance(actAttack, tAgentGeneric, mudEnergy, -11, -20).
verb_affordance(actKiss, tAgentGeneric, mudNonLonelinessSocial, 11, 20).
verb_affordance(actKiss, tAgentGeneric, mudFun, 21, 20).
verb_affordance(actThinkAbout, tLookable, mudFun, 1, 2).

:- dynamic verb_for_type/2.

verb_for_type(actTravelThru, tPassable).
verb_for_type(actObserve, tTelevision).
verb_for_type(actBumpIntoBarrier, tFurniture).
verb_for_type(actLiveAtLeastAMinute, tAgentSelf).
verb_for_type(actOperate, tShower).
verb_for_type(actClean, tShower).
verb_for_type(actOperate, tBathtub).
verb_for_type(actClean, tBathtub).
verb_for_type(actOperate, tSink).
verb_for_type(actClean, tSink).
verb_for_type(actDance, tDanceBall).
verb_for_type(actOperate, tWashingMachine).
verb_for_type(actClean, tWashingMachine).
verb_for_type(actOperate, tClothesDryer).
verb_for_type(actClean, tClothesDryer).
verb_for_type(actSleep, tBed).
verb_for_type(actSleep, tMattress).
verb_for_type(actSit, tChair).
verb_for_type(actSit, tCouch).
verb_for_type(actObserve, tRadio).
verb_for_type(actObserve, tMirror).
verb_for_type(actPotty, tToilet).
verb_for_type(actClean, tToilet).
verb_for_type(actSearch, tRefrigerator).
verb_for_type(actOperate, tStove).
verb_for_type(actOperate, tMicrowave).
verb_for_type(actOperate, tTreadmill).
verb_for_type(actOperate, tFixedLamp).
verb_for_type(actOperate, tPooltable).
verb_for_type(actPutXOn, tShelf).
verb_for_type(actPutXOn, tDesk).
verb_for_type(actPutXOn, tCounter).
verb_for_type(actPutXIn, tContainer).
verb_for_type(actPutXOn, tTable).
verb_for_type(actPutXIn, tTrashContainer).
verb_for_type(actPutXOn, tBookcase).
verb_for_type(actObserve, tReadable).
verb_for_type(actTake, tReadable).
verb_for_type(actEat, tEatable).
verb_for_type(actTake, tEatable).
verb_for_type(actObserve, tArt).
verb_for_type(actOperate, tDanceFloor).
verb_for_type(actOperate, tComputer).
verb_for_type(actTalk, tAgentGeneric).
verb_for_type(actArgue, tAgentGeneric).
verb_for_type(actAttack, tAgentGeneric).
verb_for_type(actKiss, tAgentGeneric).
verb_for_type(actTouch, tTouchable).
verb_for_type(actSit, tSittable).
verb_for_type(actPutXOn, tHasSurface).
verb_for_type(actEat, tConsumable).
verb_for_type(actTake, tTakeable).
verb_for_type(actSleep, tLayable).
verb_for_type(actClean, tLookable).
verb_for_type(actObserve, tLookable).
verb_for_type(actExcersize, tSittable).
verb_for_type(actTickle, tAgentGeneric).
verb_for_type(actSearch, tContainer).
verb_for_type(actThinkAbout, tLookable).


*/
/** <module> 
% This module is the CircleMUD configurator
*/

:-swi_module(mobs_conf,[]).

:- ensure_loaded(affordance/simbots).

end_of_file.

:- user_use_module(logicmoo('mobs/monster.pl')).
:- user_use_module(logicmoo('mobs/predator.pl')).
:- user_use_module(logicmoo('mobs/explorer.pl')).
:- user_use_module(logicmoo('mobs/prey.pl')).
:- user_use_module(logicmoo('mobs/vacuum.pl')).


Mob Flags
Positions:

0   DEAD       Reserved for internal use.  Do not set.
1   MORTALLYW  Reserved for internal use.  Do not set.
2   INCAP      Reserved for internal use.  Do not set.
3   STUNNED    Reserved for internal use.  Do not set.
4   SLEEPING   The monster is sleeping.
5   RESTING    The monster is resting.
6   SITTING    The monster is sitting.
7   FIGHTING   Reserved for internal use.  Do not set.
8   STANDING   The monster is standing.


Attack cols:

     0    hit/hits
     1    sting/stings
     2    whip/whips
     3    slash/slashes
     4    bite/bites
     5    bludgeon/bludgeons
     6    crush/crushes
     7    pound/pounds
     8    claw/claws
     9    maul/mauls
     10   thrash/thrashes
     11   pierce/pierces      When used on weapons, makes able to backstab
     12   blast/blasts
     13   punch/punches
     14   stab/stabs          When used on weapons, makes able to backstab
     15   impale/impales


Mob Flags:

1)      SPEC           This flag must be set on mobiles which have
                       special procedures written in C.  In addition to
                       setting this bit, the specproc must be assigned in
                       spec_assign.c, and the specproc itself must (of
                       course) must be written. Instead of using this
                       use scripts.
2)      SENTINEL       Mobiles wander around randomly by default; this
                       bit should be set for mobiles which are to remain
                       stationary.
3)      SCAVENGER      The mob should pick up valuables it finds on the
                       ground.  More expensive items will be taken first.
4)      ISNPC          Must be set on all mobs - set as standard. 
5)      AWARE          Set for mobs which cannot be backstabbed. 
6)      AGGRESSIVE     Mob will hit all players in the room it can see. 
                       See also the WIMPY bit.
7)      STAY_ZONE      Mob will not wander out of its own zone -- good
                       for keeping your mobs as only part of your own
                       area.
8)      WIMPY          Mob will flee when being attacked if it has less
                       than 20% of its hit points.  If the WIMPY bit is
                       set in conjunction with any of the forms of the
                       AGGRESSIVE bit, the mob will only attack mobs that
                       are unconscious (sleeping or incapacitated).
9)      AGGR_EVIL      Mob will attack players that are evil-aligned.
10)     AGGR_GOOD      Mob will attack players that are good-aligned.
11)     AGGR_NEUTRAL   Mob will attack players that are neutrally aligned.
12)     MEMORY         Mob will remember the players that initiate
                       attacks on it, and initiate an attack on that
                       player if it ever runs into him again.
13)     HELPER         The mob will attack any player it sees in the room
                       that is fighting with a mobile in the room. 
                       Useful for groups of mobiles that travel together;
                       i.e. three snakes in a pit, to force players to
                       fight all three simultaneously instead of picking
                       off one at a time.
14)     NOCHARM        Mob cannot be charmed.
15)     NOSUMMON       Mob cannot be summoned.
16)     NOSLEEP        Sleep spell cannot be cast on mob.
17)     NOBASH         Large mobs such as trees that cannot be bashed.
18)     NOBLIND        Mob cannot be blinded.
19)     AGGR_LIVING    Mob will attack players who are alive. 
                       
20)     AGGR_UNDEAD    Mob will attack players who are undead.
                       
21)     VIP            Mob is a VIP - people become OUTLAWS by killing it. 
                       
22)     NO_BLOOD       Mob cannot be drained for blood (ie. undead, statues).
                       
23)     DO_NOT_USE     Reserved for future use - do not set.
                       
24)     HYPER_AGG      Mob will attack ANYTHING (even other mobs) not protected from evil.
                       
25)     NO_KILL        
                       


Affection flags:

1)  BLIND          Mob is blind.
2)  INVISIBLE      Mob is invisible.
3)  DETECT_ALIGN   Mob is sensitive to the alignment of others.
4)  DETECT_INVIS   Mob can see invisible characters and objects.
5)  DETECT_MAGIC   Mob is sensitive to magical presence.
6)  SENSE_LIFE     Mob can sense hidden life.
7)  WATERWALK      Mob can traverse unswimmable water sectors.
8)  SANCTUARY      Mob is protected by sanctuary (takes half damage).
9)  GROUP          Reserved for internal use.  Do not set.
10) CURSE          Mob is cursed.
11) INFRAVISION    Mob can see in dark.
12) POISON         Reserved for internal use.  Do not set.
13) PROTECT_EVIL   Mob is protected from evil characters.
14) PROTECT_GOOD   Mob is protected from good characters.
15) SLEEP          Reserved for internal use.  Do not set.
16) NOTRACK        Mob cannot be tracked.
17) UNUSED         Unused (room for future expansion).
18) UNUSED         Unused (room for future expansion).
19) SNEAK          Mob can move quietly without room being informed.
20) HIDE           Mob is hidden; can only be seen with sense life.
21) BERSERK        Mob is under influence of berserk and cannot flee.
                   
22) CHARM          Reserved for internal use.  Do not set.
23) FIRESHIELD     Mob is protected by fireshield.
24) HASTE          Mob is hasted
                   
25) FLY            Mob is flying
                   


Mob Wealth:

The following choices are available:

 1) FIXED     The mob starts with the amount of gold you set it to above.
 2) POOR      The mob is poor, and starts with only a little gold compared to the mobs level.
 3) NORMAL    The mob has 'Normal' wealth - use this if in doubt.
 4) AVERAGE   The mob has average wealth - a little more gold than normal
 5) WEALTHY   Some mobs are powerful enough to demand a reasonable reward - they're wealthy
 6) RICH      This is for the few VERY rich mobs outthere.


Mob Types:

The following mobcols are currently implemented:

 1) Unknown                
 2) Undead                
 3) Humanoid               
 4) Animal                
 5) Dragon                 
 6) Giant                 
 7) Reptile                
 8) Mythical              
 9) Statue                
10) Insect                
11) Plant                 
12) Planar                
13) Troll                 
14) Spirit                
15) Elemental


 

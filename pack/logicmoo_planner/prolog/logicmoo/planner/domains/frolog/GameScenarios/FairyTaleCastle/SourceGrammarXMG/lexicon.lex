include macros.mac

%======
%Verbs
%======

% *ENTRY: have
% *CAT: v
% *SEM: binaryRel[theta1=agent,theta2=patient,rel=have]
% *ACC: 1
% *FAM: n0Vn1rel
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:

*ENTRY: hold
*CAT: v
*SEM: binaryState[rel=hold]
*ACC: 1
*FAM: n0Vn1state
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: fits-into
*CAT: v
*SEM: binaryState[rel=fitsin]
*ACC: 1
*FAM: n0Vn1state
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: leads-to
*CAT: v
*SEM: binaryState[rel=leadsto]
*ACC: 1
*FAM: n0Vn1state
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: has
*CAT: v
*SEM: binaryState[rel=hasexit]
*ACC: 1
*FAM: n0Vn1state
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: has
*CAT: v
*SEM: binaryState[rel=hasdetail]
*ACC: 1
*FAM: n0Vn1state
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

% *ENTRY: bake
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: bet
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1s2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: ask
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn2n1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:	
% *COANCHORS:
% 
% *ENTRY: ask
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:	
% *COANCHORS: Prep -> for/prep
% 
% *ENTRY: beat
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vpln1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Particle -> up/pl
% 
% *ENTRY: beat
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: Rn0Vn1A2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Adjective -> dead/a
% 
% *ENTRY: beat
% *CAT: v
% *SEM:  
% *ACC: 1
% *FAM: Rn0Vn1A2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Adjective -> how/a
% 
% 
% *ENTRY: bite
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VDN1
% *FILTERS: []
% *EX:
% *EQUATIONS:
% *COANCHORS: 
% Determiner -> the/det
% Noun -> dust/n
% 
% 
% *ENTRY: break
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  n0VDN1Pn2 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% Determiner -> the/det
% Noun -> news/n
% Prep -> to/prep 
% 
% 
% *ENTRY: close
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: En1V
% *FILTERS: []
% *EX:
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: consider
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vs1
% *FILTERS: []
% *EX:
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: cry
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VN1
% *FILTERS: []
% *EX:
% *EQUATIONS:
% *COANCHORS: Noun -> wolf/n
% 
% *ENTRY: cry
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VAN1
% *FILTERS: []
% *EX:
% *EQUATIONS:
% *COANCHORS: 
% Adjective -> bloody/a
% Noun -> murder/n
% 
% *ENTRY: upset
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: upset
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vpn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: use
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: venture
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vpn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% 
% *ENTRY: wake
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vpl
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Particle -> up/pl
% 
% *ENTRY: impress
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: s0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: insist
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vs1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: kill
 *CAT: v
 *SEM: binaryRel[theta1=killer,theta2=victim,rel=kill]
 *ACC: 1
 *FAM: n0Vn1rel
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: write
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: plant
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: punish
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: put
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1pn2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: remind
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1Pn2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Prep -> of/prep
% 
% *ENTRY: run
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: run
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  Rn0Vn1Pn2 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% Prep -> into/prep 
% 
% *ENTRY: sell
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: sing
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VDAN1
% *FILTERS: []
% *EX:
% *EQUATIONS:
% *COANCHORS: 
% Determiner -> a/det
% Adjective -> different/a
% Noun -> tune/n
% 

 *ENTRY: sit
 *CAT: v
 *SEM: unaryRel[theta1=protagonist,rel=sitdown]
 *ACC: 1
 *FAM: n0V
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
% *ENTRY: sit
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vpl
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Particle -> down/pl
% 
% 
% *ENTRY: eat
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0V
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: eat
 *CAT: v
 *SEM: binaryRel[theta1=ingestor,theta2=ingestible,rel=eat]
 *ACC: 1
 *FAM: n0Vn1rel
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: eat
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1pn2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: expect
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: Xn0Vs1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: exit
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0V
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: exit
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: sleep
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0V  
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% 
% *ENTRY: smell
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Va1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% 
% *ENTRY: stand
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vpl
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Particle -> up/pl
% 
% *ENTRY: dance
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0V
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: disappoint
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: drive
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: dread
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: fascinate
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% 
% *ENTRY: fear
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vs1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% 
% *ENTRY: fear
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: force
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1s2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: freeze
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: REn1VA2 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Adjective -> solid/a 
% 
% *ENTRY: freeze
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: REn1VA2 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Adjective -> how/a 
% 
% *ENTRY: give
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn2n1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% 
% *ENTRY: give
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1Pn2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Prep -> to/prep
% 
% *ENTRY: go
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0V
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: grow
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: fly
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: want
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vs1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: think
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vs1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: think
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Prep -> of/prep
 
 *ENTRY: throw
 *CAT: v
 *SEM: binaryRel[theta1=agent,theta2=theme,rel=throw]
 *ACC: 1
 *FAM: n0Vn1rel
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: kiss
 *CAT: v
 *SEM: binaryRel[theta1=agent,theta2=entity,rel=kiss]
 *ACC: 1
 *FAM: n0Vn1rel
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 

% *ENTRY: take
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vpln1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Particle -> out/pl
% 
% *ENTRY: take
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0lVN1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Noun -> notice/n
 
 *ENTRY: unlock
 *CAT: v
 *SEM: binaryRel[theta1=agent,theta2=theme,rel=unlock]
 *ACC: 1
 *FAM: n0Vn1rel
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: undo
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: approve
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Prep -> of/prep
% 
% 
% *ENTRY: disapprove
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Prep -> of/prep
% 
% *ENTRY: do
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: betaVs
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: might
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: betaVs
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
*ENTRY: be
*CAT: v
*SEM: 
*ACC: 1
*FAM: betaVv
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

% *ENTRY: be
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0BEn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: be
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  ItVn1s2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: CleftIt -> it/n
% 
% *ENTRY: be
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  ItVpn1s2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: CleftIt -> it/n
% 
% 
% *ENTRY: be
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:   ItVad1s2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: CleftIt -> it/n
% 
% 
% *ENTRY: be
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: betaVs
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% 
% *ENTRY: to
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: betaVv
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: would
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: betaVv
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: like
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: load
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:

 *ENTRY: lock
 *CAT: v
 *SEM: binaryRel[theta1=agent,theta2=theme,rel=lock]
 *ACC: 1
 *FAM: n0Vn1rel
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:

% *ENTRY: look
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Va1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: look
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: look
 *CAT: v
 *SEM: unaryRel[theta1=agent,rel=look]
 *ACC: 1
 *FAM: n0V
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: stand-up
 *CAT: v
 *SEM: unaryRel[theta1=protagonist,rel=standup]
 *ACC: 1
 *FAM: n0V
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: go
 *CAT: v
 *SEM: unaryRel[theta1=protagonist,rel=move]
 *ACC: 1
 *FAM: n0V
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: start
 *CAT: v
 *SEM: unaryRel[theta1=protagonist,rel=start]
 *ACC: 1
 *FAM: n0V
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: look
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0VPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:	
% *COANCHORS: Prep -> for/prep
% 
% *ENTRY: look
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  n0VN1Pn2 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% Noun -> daggers/n
% Prep -> at/prep 
% 
% *ENTRY: love
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: make
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  n0lVn2N1 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Noun -> promise/n 
% 	    	   
% 
% *ENTRY: make
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  n0lVN1Pn2 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Noun -> promise/n
% 
% *ENTRY: make
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  n0VDAN1Pn2 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% Determiner -> a/det
% Adjective -> big/a
% Noun -> deal/n
% Prep -> of/prep 
% 
% *ENTRY: make
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  n0VAN1Pn2 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% Adjective -> short/a
% Noun -> work/n
% Prep -> of/prep 
% 
% *ENTRY: make
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM:  s0Vs1 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
%   
% *ENTRY: matter
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: s0V
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: melt
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: En1V
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: melt
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: REn1VPn2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Prep -> into/prep 
% 
% *ENTRY: move
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0V
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: need
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vs1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: occur
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: s0Vton1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: 
% 
% *ENTRY: open
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vpln2n1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:Particle -> up/pl
% 
% *ENTRY: open
% *CAT: v
% *SEM: 
% *ACC: 1
% *FAM: n0Vn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% %======
% %Nouns
% %======
% 
% *ENTRY: account
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: accusation
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: n0N1s1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: accusation
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: action
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
*ENTRY: apple
*CAT: n
*SEM: nounSem[rel=apple]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: pizza
*CAT: n
*SEM: nounSem[rel=pizza]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: player
*CAT: n
*SEM: nounSem[rel=player]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: south-exit
*CAT: n
*SEM: nounSem[rel=southexit]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: exit
*CAT: n
*SEM: nounSem[rel=exit]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: north-exit
*CAT: n
*SEM: nounSem[rel=northexit]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: wall
*CAT: n
*SEM: nounSem[rel=wall]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: worm
*CAT: n
*SEM: nounSem[rel=worm]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:
 
% *ENTRY: bank
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: betaNn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: belief
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Cathcart
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: bombardier
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Bill
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: chaplain 
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: chest 
 *CAT: n
 *SEM: nounSem[rel=chest]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: chestnut
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: everyone
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: I
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% 
% *ENTRY: Lisa
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% 
% *ENTRY: who
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: who
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: n0N1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: what
 *CAT: n
 *SEM: nounSem[rel=what]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: what
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: n0N1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: which
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: man
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: marathon
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Mary
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Milo
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: mission
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: mockingbird
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Monday
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Orr
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: parent
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Pianosa
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: betaNn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: piece
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: pocket
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: puddle
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: question
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Raveesh
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: tree
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Tuesday
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: you
 *CAT: n
 *SEM: nounSem[rel=you]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: cake
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: cannabis
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: cave
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: river
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: room
 *CAT: n
 *SEM: nounSem[rel=room]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: sense
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: spirit
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: she
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: shoe
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: snow
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: south
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Syndicate
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: sword
 *CAT: n
 *SEM: nounSem[rel=sword]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: object
 *CAT: n
 *SEM: nounSem[rel=object]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: container
 *CAT: n
 *SEM: nounSem[rel=genericcontainer]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: crown
 *CAT: n
 *SEM: nounSem[rel=crown]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: necklace
 *CAT: n
 *SEM: nounSem[rel=necklace]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: table
 *CAT: n
 *SEM: nounSem[rel=table]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: treasury
 *CAT: n
 *SEM: nounSem[rel=treasury]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: wife
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: woman
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Yossarian
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: cherry
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: child
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: abortion
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: colonel
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: cotton
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: couch
 *CAT: n
 *SEM: nounSem[rel=couch]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: leg
 *CAT: n
 *SEM: nounSem[rel=couchleg]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: frog
 *CAT: n
 *SEM: nounSem[rel=frog]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: being
 *CAT: n
 *SEM: nounSem[rel=being]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: one
 *CAT: n
 *SEM: nounSem[rel=top]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: thing
 *CAT: n
 *SEM: nounSem[rel=object]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: foodpaste
 *CAT: n
 *SEM: nounSem[rel=foodpaste]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: openable
 *CAT: n
 *SEM: nounSem[rel=openclosecontainer]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: object
 *CAT: n
 *SEM: nounSem[rel=object]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: seat
 *CAT: n
 *SEM: nounSem[rel=seat]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: castle
 *CAT: n
 *SEM: nounSem[rel=castle]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: here
 *CAT: n
 *SEM: nounSem[rel=here]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: drawing-room
 *CAT: n
 *SEM: nounSem[rel=drawingroom]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: weapon
 *CAT: n
 *SEM: nounSem[rel=weapon]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: country
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: day
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: detective
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: insanity
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: s0N1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: document
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: exit
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: expectation
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: fish
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: flower
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: foot
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: gun
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: hand
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: door
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: dragon
 *CAT: n
 *SEM: nounSem[rel=dragon]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: handle
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: he
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: help
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Hindi
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: n0N1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: hope
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: house
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: how
% *CAT: a
% *SEM:
% *ACC: 1
% *FAM: adjective
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: how
% *CAT: a
% *SEM:
% *ACC: 1
% *FAM: betaAn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% 
% *ENTRY: how
% *CAT: a
% *SEM:
% *ACC: 1
% *FAM: n0A1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: how
% *CAT: a
% *SEM:
% *ACC: 1
% *FAM: s0A1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: how
% *CAT: adv
% *SEM:
% *ACC: 1
% *FAM: adverb
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: innovation
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: intention
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: island
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: Jaguar
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:

*ENTRY: key
*CAT: n
*SEM: nounSem[rel=key]
*ACC: 1
*FAM: noun
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

% *ENTRY: knife
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: it
 *CAT: n
 *SEM: nounSem[rel=it]
 *ACC: 1
 *FAM: noun
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% 
% %============
% %Determiners
% %============

*ENTRY: a
*CAT: det
*SEM: basicProperty[rel=indef]
*ACC: 1
*FAM: betaDn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

% *ENTRY: an
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: all
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: many
% *CAT: a
% *SEM:
% *ACC: 1
% *FAM: betaAn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
*ENTRY: the
*CAT: det
*SEM: basicProperty[rel=def]
*ACC: 1
*FAM: betaDn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

% *ENTRY: this
% *CAT: n
% *SEM:
% *ACC: 1
% *FAM: noun
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: this
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: these
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: determiner
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: other
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: your
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: the
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: determiner
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: whose
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: which
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: her
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: her
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: his
% *CAT: det
% *SEM:
% *ACC: 1
% *FAM: betaDn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% %===============
% %Complementizers
% %===============
% 
% *ENTRY: that
% *CAT: comp
% *SEM:
% *ACC: 1
% *FAM: betaCOMPs
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% %============
% %Prepositions
% %============
% 
% *ENTRY:abroad
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: n0P1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY:about
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: preposition
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: at
 *CAT: prep
 *SEM: binaryState[rel=theme]
 *ACC: 1
 *FAM: betavPn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: through
 *CAT: prep
 *SEM: binaryState[rel=exit]
 *ACC: 1
 *FAM: betavPn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: before
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: n0Pn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: beyond
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: s0Pn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY:from
 *CAT: prep
 *SEM: binaryState[rel=source]
 *ACC: 1
 *FAM: betavPn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY:from
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: betanPn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY:of
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: preposition
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY:of
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: n0ARBPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Anchor -> ahead/adv
% 
% *ENTRY: of  
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: s0APn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:Anchor -> devoid/a		  
% 
% *ENTRY:of
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: n0APn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Anchor -> void/a
% 
% *ENTRY: to
% *CAT: prep
% *SEM: 
% *ACC: 1
% *FAM: n0PPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Anchor -> on/prep
% 
% *ENTRY: of
% *CAT: prep
% *SEM: 
% *ACC: 1
% *FAM: s0PPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Anchor -> outside/prep
% 
% 
% *ENTRY: to
% *CAT: prep
% *SEM: 
% *ACC: 1
% *FAM: s0ARBPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Anchor -> due/adv
% 
% *ENTRY: to
% *CAT: prep
% *SEM: 
% *ACC: 1
% *FAM: s0ARBPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Anchor -> contrary/adv
% 
% *ENTRY:on
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: preposition 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY:on
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: alphaPPn 
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: on
 *CAT: prep
 *SEM: binaryState[rel=goal]
 *ACC: 1
 *FAM: betavPn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: to
 *CAT: prep
 *SEM: binaryState[rel=goal]
 *ACC: 1
 *FAM: betavPn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: to
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: betanPn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY:to
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: s0NPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Anchor -> thanks/n
% 
% *ENTRY:to
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: n0NPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Anchor -> thanks/n 
 
 *ENTRY: with
 *CAT: prep
 *SEM: binaryState[rel=instrument]
 *ACC: 1
 *FAM: betavPn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: with
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: n0PNaPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% Anchor -> line/n
% Anchor2 -> in/prep	
% 
% *ENTRY: of
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: n0PNaPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% Anchor -> charge/n
% Anchor2 -> in/prep	
% 
% *ENTRY: of
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: s0PNaPn1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% Anchor -> account/n
% Anchor2 -> on/prep		  
%   
% *ENTRY: with
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: betanPn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: in
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: preposition
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:

 *ENTRY: in
 *CAT: prep
 *SEM: binaryState[rel=haslocation] 
 *ACC: 1
 *FAM: n0Pn1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: in
% *CAT: prep
% *SEM: binaryState[rel=goal] 
% *ACC: 1
% *FAM: betavPn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
% *ENTRY: in
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: alphaPPn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: in
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: betavPn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: into
 *CAT: prep
 *SEM: binaryState[rel=goal]
 *ACC: 1
 *FAM: betavPn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: where
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: alphaPP
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: where
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: n0P1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: for
% *CAT: prep
% *SEM:
% *ACC: 1
% *FAM: preposition
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: s
% *CAT: poss
% *SEM: 
% *ACC: 1
% *FAM: possessive
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% %==========
% %Adjectives
% %==========

 *ENTRY: red
 *CAT: a
 *SEM: basicProperty[rel=red]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: red
 *CAT: a
 *SEM: basicProperty[rel=red]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: red
 *CAT: a
 *SEM: basicProperty[rel=red]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: disgusting
 *CAT: a
 *SEM: basicProperty[rel=disgusting]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: disgusting
 *CAT: a
 *SEM: basicProperty[rel=disgusting]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: disgusting
 *CAT: a
 *SEM: basicProperty[rel=disgusting]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: white
 *CAT: a
 *SEM: basicProperty[rel=white]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: white
 *CAT: a
 *SEM: basicProperty[rel=white]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: white
 *CAT: a
 *SEM: basicProperty[rel=white]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: yellow
 *CAT: a
 *SEM: basicProperty[rel=yellow]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: yellow
 *CAT: a
 *SEM: basicProperty[rel=yellow]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: yellow
 *CAT: a
 *SEM: basicProperty[rel=yellow]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:

% *ENTRY: able
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: n0A1s1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: annoying
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: n0A1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: blissful
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: s0A1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:

% *ENTRY: blue
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: betaAn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:

% *ENTRY: enthralling
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: adjective
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:

 *ENTRY: brown
 *CAT: a
 *SEM: basicProperty[rel=brown]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: brown
 *CAT: a
 *SEM: basicProperty[rel=brown]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: brown
 *CAT: a
 *SEM: basicProperty[rel=brown]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:

 *ENTRY: green
 *CAT: a
 *SEM: basicProperty[rel=green]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: green
 *CAT: a
 *SEM: basicProperty[rel=green]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:

 *ENTRY: green
 *CAT: a
 *SEM: basicProperty[rel=green]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:

 *ENTRY: open
 *CAT: a
 *SEM: basicProperty[rel=open]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: open
 *CAT: a
 *SEM: basicProperty[rel=open]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: open
 *CAT: a
 *SEM: basicProperty[rel=open]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: closed
 *CAT: a
 *SEM: basicProperty[rel=closed]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: closed
 *CAT: a
 *SEM: basicProperty[rel=closed]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: closed
 *CAT: a
 *SEM: basicProperty[rel=closed]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 

% *ENTRY: frightening
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: adjective
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:

*ENTRY: alive
*CAT: a
*SEM: basicProperty[rel=alive]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: alive
*CAT: a
*SEM: basicProperty[rel=alive]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: alive
*CAT: a
*SEM: basicProperty[rel=alive]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%%
*ENTRY: dead
*CAT: a
*SEM: basicProperty[rel=dead]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: dead
*CAT: a
*SEM: basicProperty[rel=dead]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: dead
*CAT: a
*SEM: basicProperty[rel=dead]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:
%%%%%%%%%%

*ENTRY: wooden
*CAT: a
*SEM: basicProperty[rel=wooden]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: wooden
*CAT: a
*SEM: basicProperty[rel=wooden]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: wooden
*CAT: a
*SEM: basicProperty[rel=wooden]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: here
*CAT: a
*SEM: basicProperty[rel=here]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: here
*CAT: a
*SEM: basicProperty[rel=here]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: gone
*CAT: a
*SEM: basicProperty[rel=gone]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: gone
*CAT: a
*SEM: basicProperty[rel=gone]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: small
*CAT: a
*SEM: basicProperty[rel=small]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: small
*CAT: a
*SEM: basicProperty[rel=small]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: small
*CAT: a
*SEM: basicProperty[rel=small]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: ugly
*CAT: a
*SEM: basicProperty[rel=ugly]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: ugly
*CAT: a
*SEM: basicProperty[rel=ugly]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: ugly
*CAT: a
*SEM: basicProperty[rel=ugly]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: happy
*CAT: a
*SEM: basicProperty[rel=happy]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: happy
*CAT: a
*SEM: basicProperty[rel=happy]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: happy
*CAT: a
*SEM: basicProperty[rel=happy]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: black
*CAT: a
*SEM: basicProperty[rel=black]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: black
*CAT: a
*SEM: basicProperty[rel=black]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: black
*CAT: a
*SEM: basicProperty[rel=black]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: seated
*CAT: a
*SEM: basicProperty[rel=seated]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: seated
*CAT: a
*SEM: basicProperty[rel=seated]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: seated
*CAT: a
*SEM: basicProperty[rel=seated]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: visible
*CAT: a
*SEM: basicProperty[rel=visible]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: visible
*CAT: a
*SEM: basicProperty[rel=visible]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: visible
*CAT: a
*SEM: basicProperty[rel=visible]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%

*ENTRY: victorious
*CAT: a
*SEM: basicProperty[rel=victorious]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: victorious
*CAT: a
*SEM: basicProperty[rel=victorious]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: victorious
*CAT: a
*SEM: basicProperty[rel=victorious]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%

*ENTRY: wearable
*CAT: a
*SEM: basicProperty[rel=wearable]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: wearable
*CAT: a
*SEM: basicProperty[rel=wearable]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: wearable
*CAT: a
*SEM: basicProperty[rel=wearable]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:


%%%%%%%%%%

*ENTRY: bored
*CAT: a
*SEM: basicProperty[rel=bored]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: bored
*CAT: a
*SEM: basicProperty[rel=bored]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: bored
*CAT: a
*SEM: basicProperty[rel=bored]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

%%%%%%%%%%
*ENTRY: accessible
*CAT: a
*SEM: basicProperty[rel=accessible]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: accessible
*CAT: a
*SEM: basicProperty[rel=accessible]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: accessible
*CAT: a
*SEM: basicProperty[rel=accessible]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:
%%%%%%%%%%

*ENTRY: beautiful
*CAT: a
*SEM: basicProperty[rel=beautiful]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: beautiful
*CAT: a
*SEM: basicProperty[rel=beautiful]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: beautiful
*CAT: a
*SEM: basicProperty[rel=beautiful]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:
%%%%%%%%%%

*ENTRY: yummy
*CAT: a
*SEM: basicProperty[rel=yummy]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: yummy
*CAT: a
*SEM: basicProperty[rel=yummy]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: yummy
*CAT: a
*SEM: basicProperty[rel=yummy]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:
%%%%%%%%%%

% TODO: I add location as an adjective for generating "you don't know what is location" 
% This should be fixed, location is a noun, I need to extend the grammar to generate
% "you don't know what is the target location" 
*ENTRY: location
*CAT: a
*SEM: basicProperty[rel=genericcontainer]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:
%%%%%%%%%%

*ENTRY: empty
*CAT: a
*SEM: basicProperty[rel=empty]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: empty
*CAT: a
*SEM: basicProperty[rel=empty]
*ACC: 1
*FAM: betaAn
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: empty
*CAT: a
*SEM: basicProperty[rel=empty]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:
%%%%%%%%%%

*ENTRY: not-so-easy-to-kill
*CAT: a
*SEM: basicProperty[rel=notsoeasytokill]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: not-so-easy-to-kill
*CAT: a
*SEM: basicProperty[rel=notsoeasytokill]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: easy-to-kill
*CAT: a
*SEM: basicProperty[rel=easytokill]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: easy-to-kill
*CAT: a
*SEM: basicProperty[rel=easytokill]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: takeable
*CAT: a
*SEM: basicProperty[rel=takeable]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: takeable
*CAT: a
*SEM: basicProperty[rel=takeable]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: edible
*CAT: a
*SEM: basicProperty[rel=edible]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: edible
*CAT: a
*SEM: basicProperty[rel=edible]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: climbable
*CAT: a
*SEM: basicProperty[rel=climbable]
*ACC: 1
*FAM: adjective
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

*ENTRY: climbable
*CAT: a
*SEM: basicProperty[rel=climbable]
*ACC: 1
*FAM: n0A1
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS:

 *ENTRY: locked
 *CAT: a
 *SEM: basicProperty[rel=locked]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: locked
 *CAT: a
 *SEM: basicProperty[rel=locked]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: locked
 *CAT: a
 *SEM: basicProperty[rel=locked]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: unlocked
 *CAT: a
 *SEM: basicProperty[rel=unlocked]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: unlocked
 *CAT: a
 *SEM: basicProperty[rel=unlocked]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 
 
 *ENTRY: unlocked
 *CAT: a
 *SEM: basicProperty[rel=unlocked]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS: 

% *ENTRY: last
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: betaAn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: necessary
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: adjective
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: right
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: betaAn
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: silver
 *CAT: a
 *SEM: basicProperty[rel=silver]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: silver
 *CAT: a
 *SEM: basicProperty[rel=silver]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: silver
 *CAT: a
 *SEM: basicProperty[rel=silver]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: golden
 *CAT: a
 *SEM: basicProperty[rel=golden]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: golden
 *CAT: a
 *SEM: basicProperty[rel=golden]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: seating
 *CAT: a
 *SEM: basicProperty[rel=seating]
 *ACC: 1
 *FAM: betaAn
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: seating
 *CAT: a
 *SEM: basicProperty[rel=seating]
 *ACC: 1
 *FAM: n0A1
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
 *ENTRY: golden
 *CAT: a
 *SEM: basicProperty[rel=golden]
 *ACC: 1
 *FAM: adjective
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: sure
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: n0A1s1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: uncertain
% *CAT: a
% *SEM: 
% *ACC: 1
% *FAM: s0A1
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
%
% *ENTRY: carefully
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM:  betavxARB
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: considerably
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM:  betavxARB
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: around
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM:  betavxARB
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: away
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM: n0n1ARB
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: even
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM:  betaARBvx
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: regularly
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM:  adverb
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% 
% *ENTRY: reluctantly
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM:  adverb
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: south
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM:  betavxARB
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: totally
% *CAT: adv
% *SEM: 
% *ACC: 1
% *FAM:  betaARBa
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
 *ENTRY: and
 *CAT: conj
 *SEM: basicProperty[rel=and]
 *ACC: 1
 *FAM:  betaa1CONJa2
 *FILTERS: []
 *EX: {}
 *EQUATIONS:
 *COANCHORS:
 
% *ENTRY: and
% *CAT: conj
% *SEM: 
% *ACC: 1
% *FAM:  betan1CONJn2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: and
% *CAT: conj
% *SEM: 
% *ACC: 1
% *FAM:  betas1CONJs2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
 
% *ENTRY: or
% *CAT: conj
% *SEM: 
% *ACC: 1
% *FAM:  betas1CONJs2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS:
% 
% *ENTRY: or
% *CAT: conj
% *SEM: 
% *ACC: 1
% *FAM: betaCONJs1CONJs2
% *FILTERS: []
% *EX: {}
% *EQUATIONS:
% *COANCHORS: Conjunction -> either/conj

*ENTRY: take
*CAT: v
*SEM: binaryRel[theta1=agent,theta2=theme,rel=take]
*ACC: 1
*FAM: n0Vn1rel
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS: 

*ENTRY: wear
*CAT: v
*SEM: binaryRel[theta1=agent,theta2=theme,rel=wear]
*ACC: 1
*FAM: n0Vn1rel
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS: 

*ENTRY: drop
*CAT: v
*SEM: binaryRel[theta1=agent,theta2=theme,rel=drop]
*ACC: 1
*FAM: n0Vn1rel
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS: 

*ENTRY: put
*CAT: v
*SEM: binaryRel[theta1=agent,theta2=theme,rel=put]
*ACC: 1
*FAM: n0Vn1rel
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS: 

*ENTRY: open
*CAT: v
*SEM: binaryRel[theta1=agent,theta2=object,rel=open]
*ACC: 1
*FAM: n0Vn1rel
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS: 

*ENTRY: close
*CAT: v
*SEM: binaryRel[theta1=agent,theta2=object,rel=shut]
*ACC: 1
*FAM: n0Vn1rel
*FILTERS: []
*EX: {}
*EQUATIONS:
*COANCHORS: 


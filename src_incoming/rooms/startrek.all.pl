/** <module> 
% Non-module Impl of CircleMUD StarTrek TNG files
%
% Dec 13, 2035
% Douglas Miles
%
*/


/*
dyn:type_grid('SpaceInAHOC',0, [ed,ed,ed,ex,ed,ed,ed]).
dyn:type_grid('SpaceInAHOC',1, [ed,--,--,--,--,--,ed]).
dyn:type_grid('SpaceInAHOC',2, [ed,--,--,--,--,--,ed]).
dyn:type_grid('SpaceInAHOC',3, [ex,--,--,--,--,--,ex]).
dyn:type_grid('SpaceInAHOC',4, [ed,--,--,--,--,--,ed]).
dyn:type_grid('SpaceInAHOC',5, [ed,--,--,--,--,--,ed]).
dyn:type_grid('SpaceInAHOC',6, [ed,ed,ed,ex,ed,ed,ed]).
*/
/*
dyn:type_grid('SpaceInAHOC',1, [--,--,--,--,--]).
dyn:type_grid('SpaceInAHOC',2, [--,--,--,--,--]).
dyn:type_grid('SpaceInAHOC',3, [--,--,--,--,--]).
dyn:type_grid('SpaceInAHOC',4, [--,--,--,--,--]).
dyn:type_grid('SpaceInAHOC',5, [--,--,--,--,--]).
*/
% center of room is 3,3,1 (rooms ranges are (1-5,1-5,1-5)

% :- style_check(-singleton).
:-set_prolog_flag(double_quotes,string).
:-style_check(-atom).

% % :-['../hyhtn/translog4.pl'].
%end_of_file.

predicates([
% dynamic startrek
  nameString(term,string),
  pathName(region,dir,string),
  inRegion(agent,'BPVLocation'),
  possess(agent,'ProtectiveAttire'),
  possess(agent,'PortableObject'),
  possess(agent,'Weapon'),
  wearing(agent,'ProtectiveAttire'),
  wearing(agent,'PortableObject'),

% static startrek
  pathBetween('BPVLocation',dir,'BPVLocation'),
  agentRoute(areaPath,'BPVLocation','BPVLocation') ]).

/* Prediate:  agent/1 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(55).
number_of_clauses(1).
Pattern: agent(_G1281). 
 */
agent('NpcCol1000-Geordi684').
agent(vacuum(1)).
agent(explorer(player1)).
agent('NpcCol1002-Worf720').
agent('NpcCol1003-Dr-Crusher677').
agent('NpcCol1004-Troi712').
agent('NpcCol1005-Riker707').
agent('NpcCol1006-Picard701').
agent('NpcCol1007-Guinan689').
agent('NpcCol1008-OBrien696').
agent('NpcCol1009-Wesley716').
agent('NpcCol1010-Livingston726').
agent('NpcCol1011-Spot727').
agent('NpcCol1012-Ensign728').
agent('NpcCol1012-Ensign732').
agent('NpcCol1012-Ensign736').
agent('NpcCol1012-Ensign740').
agent('NpcCol1012-Ensign744').
agent('NpcCol1012-Ensign748').
agent('NpcCol1012-Ensign752').
agent('NpcCol1013-Alexander671').

 /* found 21 for agent(_G1281). */ 

/* Prediate:  wearing/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(58).
number_of_clauses(1).
Pattern: wearing(_G1304,_G1305). 
 */
wearing(vacuum(1),'ArtifactCol1005-Boots673').
wearing(vacuum(1),'ArtifactCol1006-Comm-Badge674').
wearing(vacuum(1),'ArtifactCol1003-Gold-Uniform675').
wearing(explorer(player1),'ArtifactCol1005-Boots773').
wearing(explorer(player1),'ArtifactCol1006-Comm-Badge774').
wearing(explorer(player1),'ArtifactCol1003-Gold-Uniform775').
wearing('NpcCol1003-Dr-Crusher677','ArtifactCol1005-Boots678').
wearing('NpcCol1003-Dr-Crusher677','ArtifactCol1006-Comm-Badge679').
wearing('NpcCol1003-Dr-Crusher677','ArtifactCol1004-Blue-Uniform680').
wearing('NpcCol1000-Geordi684','ArtifactCol1005-Boots685').
wearing('NpcCol1000-Geordi684','ArtifactCol1006-Comm-Badge686').
wearing('NpcCol1000-Geordi684','ArtifactCol1003-Gold-Uniform687').
wearing('NpcCol1000-Geordi684','ArtifactCol1008-VISOR688').
wearing('NpcCol1008-OBrien696','ArtifactCol1005-Boots697').
wearing('NpcCol1008-OBrien696','ArtifactCol1006-Comm-Badge698').
wearing('NpcCol1008-OBrien696','ArtifactCol1003-Gold-Uniform699').
wearing('NpcCol1006-Picard701','ArtifactCol1005-Boots702').
wearing('NpcCol1006-Picard701','ArtifactCol1006-Comm-Badge703').
wearing('NpcCol1006-Picard701','ArtifactCol1002-Red-Uniform704').
wearing('NpcCol1005-Riker707','ArtifactCol1005-Boots708').
wearing('NpcCol1005-Riker707','ArtifactCol1006-Comm-Badge709').
wearing('NpcCol1005-Riker707','ArtifactCol1002-Red-Uniform710').
wearing('NpcCol1004-Troi712','ArtifactCol1005-Boots713').
wearing('NpcCol1004-Troi712','ArtifactCol1006-Comm-Badge714').
wearing('NpcCol1004-Troi712','ArtifactCol1004-Blue-Uniform715').
wearing('NpcCol1009-Wesley716','ArtifactCol1005-Boots717').
wearing('NpcCol1009-Wesley716','ArtifactCol1006-Comm-Badge718').
wearing('NpcCol1009-Wesley716','ArtifactCol1002-Red-Uniform719').
wearing('NpcCol1002-Worf720','ArtifactCol1005-Boots721').
wearing('NpcCol1002-Worf720','ArtifactCol1006-Comm-Badge722').
wearing('NpcCol1002-Worf720','ArtifactCol1003-Gold-Uniform723').
wearing('NpcCol1002-Worf720','ArtifactCol1007-Sash725').
wearing('NpcCol1012-Ensign728','ArtifactCol1005-Boots729').
wearing('NpcCol1012-Ensign728','ArtifactCol1006-Comm-Badge730').
wearing('NpcCol1012-Ensign728','ArtifactCol1003-Gold-Uniform731').
wearing('NpcCol1012-Ensign732','ArtifactCol1005-Boots733').
wearing('NpcCol1012-Ensign732','ArtifactCol1006-Comm-Badge734').
wearing('NpcCol1012-Ensign732','ArtifactCol1003-Gold-Uniform735').
wearing('NpcCol1012-Ensign736','ArtifactCol1005-Boots737').
wearing('NpcCol1012-Ensign736','ArtifactCol1006-Comm-Badge738').
wearing('NpcCol1012-Ensign736','ArtifactCol1002-Red-Uniform739').
wearing('NpcCol1012-Ensign740','ArtifactCol1005-Boots741').
wearing('NpcCol1012-Ensign740','ArtifactCol1006-Comm-Badge742').
wearing('NpcCol1012-Ensign740','ArtifactCol1002-Red-Uniform743').
wearing('NpcCol1012-Ensign744','ArtifactCol1005-Boots745').
wearing('NpcCol1012-Ensign744','ArtifactCol1006-Comm-Badge746').
wearing('NpcCol1012-Ensign744','ArtifactCol1004-Blue-Uniform747').
wearing('NpcCol1012-Ensign748','ArtifactCol1005-Boots749').
wearing('NpcCol1012-Ensign748','ArtifactCol1006-Comm-Badge750').
wearing('NpcCol1012-Ensign748','ArtifactCol1004-Blue-Uniform751').
wearing('NpcCol1012-Ensign752','ArtifactCol1005-Boots753').
wearing('NpcCol1012-Ensign752','ArtifactCol1006-Comm-Badge754').
wearing('NpcCol1012-Ensign752','ArtifactCol1004-Blue-Uniform755').

 /* found 53 for wearing(_G1304,_G1305). */ 

/* Prediate:  possess/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(57).
number_of_clauses(1).
Pattern: possess(_G1328,_G1329). 
 */
possess(vacuum(1),'ArtifactCol1000-Phaser676').
possess(explorer(player1),'ArtifactCol1000-Phaser776').
possess('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder681').
possess('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder682').
possess('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder683').
possess('NpcCol1007-Guinan689','ArtifactCol1020-Tea690').
possess('NpcCol1007-Guinan689','ArtifactCol1021-Synthehol691').
possess('NpcCol1007-Guinan689','ArtifactCol1022-Ferengi-Ale692').
possess('NpcCol1007-Guinan689','ArtifactCol1023-Romulan-Whisky693').
possess('NpcCol1007-Guinan689','ArtifactCol1024-Lemonade-Prune-Juice694').
possess('NpcCol1007-Guinan689','ArtifactCol1025-Vulcan-Beer695').
possess('NpcCol1008-OBrien696','ArtifactCol1000-Phaser700').
possess('NpcCol1006-Picard701','ArtifactCol1001-5-Phaser-Rifle705').
possess('NpcCol1006-Picard701','ArtifactCol1011-5-Picards-Flute706').
possess('NpcCol1005-Riker707','ArtifactCol1012-Trombone711').
possess('NpcCol1002-Worf720','ArtifactCol1000-Phaser724').

 /* found 16 for possess(_G1328,_G1329). */ 

/* Prediate:  item/1 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(56).
number_of_clauses(1).
Pattern: item(_G1352). 
 */
item('ArtifactCol1000-Phaser676').
item('ArtifactCol1000-Phaser776').
item('ArtifactCol1000-Phaser700').
item('ArtifactCol1000-Phaser724').
item('ArtifactCol1001-5-Phaser-Rifle705').
item('ArtifactCol1002-Red-Uniform704').
item('ArtifactCol1002-Red-Uniform710').
item('ArtifactCol1002-Red-Uniform719').
item('ArtifactCol1002-Red-Uniform739').
item('ArtifactCol1002-Red-Uniform743').
item('ArtifactCol1003-Gold-Uniform675').
item('ArtifactCol1003-Gold-Uniform775').
item('ArtifactCol1003-Gold-Uniform687').
item('ArtifactCol1003-Gold-Uniform699').
item('ArtifactCol1003-Gold-Uniform723').
item('ArtifactCol1003-Gold-Uniform731').
item('ArtifactCol1003-Gold-Uniform735').
item('ArtifactCol1004-Blue-Uniform680').
item('ArtifactCol1004-Blue-Uniform715').
item('ArtifactCol1004-Blue-Uniform747').
item('ArtifactCol1004-Blue-Uniform751').
item('ArtifactCol1004-Blue-Uniform755').
item('ArtifactCol1005-Boots673').
item('ArtifactCol1005-Boots773').
item('ArtifactCol1005-Boots678').
item('ArtifactCol1005-Boots685').
item('ArtifactCol1005-Boots697').
item('ArtifactCol1005-Boots702').
item('ArtifactCol1005-Boots708').
item('ArtifactCol1005-Boots713').
item('ArtifactCol1005-Boots717').
item('ArtifactCol1005-Boots721').
item('ArtifactCol1005-Boots729').
item('ArtifactCol1005-Boots733').
item('ArtifactCol1005-Boots737').
item('ArtifactCol1005-Boots741').
item('ArtifactCol1005-Boots745').
item('ArtifactCol1005-Boots749').
item('ArtifactCol1005-Boots753').
item('ArtifactCol1006-Comm-Badge674').
item('ArtifactCol1006-Comm-Badge774').
item('ArtifactCol1006-Comm-Badge679').
item('ArtifactCol1006-Comm-Badge686').
item('ArtifactCol1006-Comm-Badge698').
item('ArtifactCol1006-Comm-Badge703').
item('ArtifactCol1006-Comm-Badge709').
item('ArtifactCol1006-Comm-Badge714').
item('ArtifactCol1006-Comm-Badge718').
item('ArtifactCol1006-Comm-Badge722').
item('ArtifactCol1006-Comm-Badge730').
item('ArtifactCol1006-Comm-Badge734').
item('ArtifactCol1006-Comm-Badge738').
item('ArtifactCol1006-Comm-Badge742').
item('ArtifactCol1006-Comm-Badge746').
item('ArtifactCol1006-Comm-Badge750').
item('ArtifactCol1006-Comm-Badge754').
item('ArtifactCol1007-Sash725').
item('ArtifactCol1008-VISOR688').
item('ArtifactCol1009-Medical-Tricorder681').
item('ArtifactCol1009-Medical-Tricorder682').
item('ArtifactCol1009-Medical-Tricorder683').
item('ArtifactCol1009-Tricorder759').
item('ArtifactCol1009-Tricorder760').
item('ArtifactCol1009-Tricorder761').
item('ArtifactCol1010-Dilithium-Crystal756').
item('ArtifactCol1010-Dilithium-Crystal757').
item('ArtifactCol1010-Dilithium-Crystal758').
item('ArtifactCol1011-5-Picards-Flute706').
item('ArtifactCol1012-Trombone711').
item('ArtifactCol1020-Tea690').
item('ArtifactCol1021-Synthehol691').
item('ArtifactCol1022-Ferengi-Ale692').
item('ArtifactCol1023-Romulan-Whisky693').
item('ArtifactCol1024-Lemonade-Prune-Juice694').
item('ArtifactCol1025-Vulcan-Beer695').

 /* found 75 for item(_G1352). */ 

/* Prediate:  inRegion/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(59).
number_of_clauses(1).
Pattern: inRegion(_G1375,_G1376). 
 */
inRegion('Area1005-Object666','Area1005').
inRegion('Area1008-Object667','Area1008').
inRegion('Area1013-Object668','Area1013').
inRegion('Area1016-Object669','Area1016').
inRegion('Area1024-Object670','Area1024').
inRegion('NpcCol1013-Alexander671','Area1025').
inRegion(vacuum(1),'Area1010').
inRegion(explorer(player1),'Area1000').
inRegion('NpcCol1003-Dr-Crusher677','Area1015').
inRegion('NpcCol1000-Geordi684','Area1000').
inRegion('NpcCol1007-Guinan689','Area1021').
inRegion('NpcCol1008-OBrien696','Area1006').
inRegion('NpcCol1006-Picard701','Area1035').
inRegion('NpcCol1005-Riker707','Area1036').
inRegion('NpcCol1004-Troi712','Area1007').
inRegion('NpcCol1009-Wesley716','Area1016').
inRegion('NpcCol1002-Worf720','Area1025').
inRegion('NpcCol1010-Livingston726','Area1035').
inRegion('NpcCol1011-Spot727','Area1003').
inRegion('NpcCol1012-Ensign728','Area1000').
inRegion('NpcCol1012-Ensign732','Area1004').
inRegion('NpcCol1012-Ensign736','Area1011').
inRegion('NpcCol1012-Ensign740','Area1020').
inRegion('NpcCol1012-Ensign744','Area1024').
inRegion('NpcCol1012-Ensign748','Area1022').
inRegion('NpcCol1012-Ensign752','Area1036').
inRegion('ArtifactCol1010-Dilithium-Crystal756','Area1000').
inRegion('ArtifactCol1010-Dilithium-Crystal757','Area1000').
inRegion('ArtifactCol1010-Dilithium-Crystal758','Area1000').
inRegion('ArtifactCol1009-Tricorder759','Area1015').
inRegion('ArtifactCol1009-Tricorder760','Area1015').
inRegion('ArtifactCol1009-Tricorder761','Area1015').

 /* found 32 for inRegion(_G1375,_G1376). */ 

/* Prediate:  region/1 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(30).
number_of_clauses(1).
Pattern: region(_G1399). 
 */
region('Area1000').
region('Area1002').
region('Area1001').
region('Area1005').
region('Area1003').
region('Area1004').
region('Area1008').
region('Area1006').
region('Area1042').
region('Area1007').
region('Area1010').
region('Area1009').
region('Area1011').
region('Area1013').
region('Area1032').
region('Area1012').
region('Area1016').
region('Area1014').
region('Area1015').
region('Area1019').
region('Area1017').
region('Area1018').
region('Area1021').
region('Area1020').
region('Area1022').
region('Area1024').
region('Area1039').
region('Area1023').
region('Area1027').
region('Area1025').
region('Area1026').
region('Area1030').
region('Area1028').
region('Area1029').
region('Area1031').
region('Area1033').
region('Area1034').
region('Area1036').
region('Area1035').
region('Area1038').
region('Area1037').
region('Area1040').
region('Area1041').

 /* found 45 for region(_G1399). */ 

/* Prediate:  somethingIsa/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(50).
number_of_clauses(1).
Pattern: somethingIsa(_G1422,_G1423). 
 */
somethingIsa('NpcCol1000-Geordi684',['NpcCol1000',agent,'MaleAnimal']).
somethingIsa(vacuum(1),['NpcCol1001',agent,'MaleAnimal']).
% somethingIsa(explorer(player1),[['OSimClassFn',string("player_osimmarine_mp")],agent,'MaleAnimal']).
somethingIsa('NpcCol1002-Worf720',['NpcCol1002',agent,'MaleAnimal']).
somethingIsa('NpcCol1003-Dr-Crusher677',['NpcCol1003',agent,'FemaleAnimal']).
somethingIsa('NpcCol1004-Troi712',['NpcCol1004',agent,'FemaleAnimal']).
somethingIsa('NpcCol1005-Riker707',['NpcCol1005',agent,'MaleAnimal']).
somethingIsa('NpcCol1006-Picard701',['NpcCol1006',agent,'MaleAnimal']).
somethingIsa('NpcCol1007-Guinan689',['NpcCol1007',agent,'FemaleAnimal']).
somethingIsa('NpcCol1008-OBrien696',['NpcCol1008',agent,'MaleAnimal']).
somethingIsa('NpcCol1009-Wesley716',['NpcCol1009',agent,'MaleAnimal']).
somethingIsa('NpcCol1010-Livingston726',['NpcCol1010',agent,'MaleAnimal']).
somethingIsa('NpcCol1011-Spot727',['NpcCol1011',agent,'MaleAnimal']).
somethingIsa('NpcCol1012-Ensign728',['NpcCol1012',agent,'MaleAnimal']).
somethingIsa('NpcCol1012-Ensign732',['NpcCol1012',agent,'MaleAnimal']).
somethingIsa('NpcCol1012-Ensign736',['NpcCol1012',agent,'MaleAnimal']).
somethingIsa('NpcCol1012-Ensign740',['NpcCol1012',agent,'MaleAnimal']).
somethingIsa('NpcCol1012-Ensign744',['NpcCol1012',agent,'MaleAnimal']).
somethingIsa('NpcCol1012-Ensign748',['NpcCol1012',agent,'MaleAnimal']).
somethingIsa('NpcCol1012-Ensign752',['NpcCol1012',agent,'MaleAnimal']).
somethingIsa('NpcCol1013-Alexander671',['NpcCol1013',agent,'MaleAnimal']).
somethingIsa('ArtifactCol1000-Phaser676',['ArtifactCol1000','Handgun','Weapon','LightingDevice','PortableObject','Device-SingleUser','SomethingToWear']).
somethingIsa('ArtifactCol1000-Phaser776',['ArtifactCol1000','Handgun','Weapon','LightingDevice','PortableObject','Device-SingleUser','SomethingToWear']).
somethingIsa('ArtifactCol1000-Phaser700',['ArtifactCol1000','Handgun','Weapon','LightingDevice','PortableObject','Device-SingleUser','SomethingToWear']).
somethingIsa('ArtifactCol1000-Phaser724',['ArtifactCol1000','Handgun','Weapon','LightingDevice','PortableObject','Device-SingleUser','SomethingToWear']).
somethingIsa('ArtifactCol1001-5-Phaser-Rifle705',['ArtifactCol1001','Weapon','LightingDevice','PortableObject','Device-SingleUser','SomethingToWear']).
somethingIsa('ArtifactCol1002-Red-Uniform704',['ArtifactCol1002','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1002-Red-Uniform710',['ArtifactCol1002','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1002-Red-Uniform719',['ArtifactCol1002','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1002-Red-Uniform739',['ArtifactCol1002','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1002-Red-Uniform743',['ArtifactCol1002','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1003-Gold-Uniform675',['ArtifactCol1003','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1003-Gold-Uniform775',['ArtifactCol1003','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1003-Gold-Uniform687',['ArtifactCol1003','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1003-Gold-Uniform699',['ArtifactCol1003','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1003-Gold-Uniform723',['ArtifactCol1003','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1003-Gold-Uniform731',['ArtifactCol1003','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1003-Gold-Uniform735',['ArtifactCol1003','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1004-Blue-Uniform680',['ArtifactCol1004','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1004-Blue-Uniform715',['ArtifactCol1004','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1004-Blue-Uniform747',['ArtifactCol1004','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1004-Blue-Uniform751',['ArtifactCol1004','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1004-Blue-Uniform755',['ArtifactCol1004','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots673',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots773',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots678',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots685',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots697',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots702',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots708',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots713',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots717',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots721',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots729',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots733',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots737',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots741',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots745',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots749',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1005-Boots753',['ArtifactCol1005','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1006-Comm-Badge674',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge774',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge679',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge686',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge698',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge703',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge709',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge714',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge718',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge722',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge730',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge734',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge738',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge742',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge746',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge750',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1006-Comm-Badge754',['ArtifactCol1006','ProtectiveAttire','PortableObject','Necklace']).
somethingIsa('ArtifactCol1007-Sash725',['ArtifactCol1007','ProtectiveAttire','PortableObject','SomethingToWear']).
somethingIsa('ArtifactCol1008-VISOR688',['ArtifactCol1008','ProtectiveAttire','PortableObject']).
somethingIsa('ArtifactCol1009-Medical-Tricorder681',['ArtifactCol1009','RodShapedObject','ControlDevice','HandTool','PortableObject']).
somethingIsa('ArtifactCol1009-Medical-Tricorder682',['ArtifactCol1009','RodShapedObject','ControlDevice','HandTool','PortableObject']).
somethingIsa('ArtifactCol1009-Medical-Tricorder683',['ArtifactCol1009','RodShapedObject','ControlDevice','HandTool','PortableObject']).
somethingIsa('ArtifactCol1009-Tricorder759',['ArtifactCol1009','RodShapedObject','ControlDevice','HandTool','PortableObject']).
somethingIsa('ArtifactCol1009-Tricorder760',['ArtifactCol1009','RodShapedObject','ControlDevice','HandTool','PortableObject']).
somethingIsa('ArtifactCol1009-Tricorder761',['ArtifactCol1009','RodShapedObject','ControlDevice','HandTool','PortableObject']).
somethingIsa('ArtifactCol1010-Dilithium-Crystal756',['ArtifactCol1010','LightingDevice','PortableObject','HandTool']).
somethingIsa('ArtifactCol1010-Dilithium-Crystal757',['ArtifactCol1010','LightingDevice','PortableObject','HandTool']).
somethingIsa('ArtifactCol1010-Dilithium-Crystal758',['ArtifactCol1010','LightingDevice','PortableObject','HandTool']).
somethingIsa('ArtifactCol1011-5-Picards-Flute706',['ArtifactCol1011','Artifact-Generic','InformationStore','PortableObject','HandTool']).
somethingIsa('ArtifactCol1012-Trombone711',['ArtifactCol1012','Artifact-Generic','InformationStore','PortableObject','HandTool']).
somethingIsa('ArtifactCol1020-Tea690',['ArtifactCol1020','Flask-LabGlassware','Bottle','FluidReservoir','Container','Portal','PortableObject','HandTool']).
somethingIsa('ArtifactCol1021-Synthehol691',['ArtifactCol1021','Flask-LabGlassware','Bottle','FluidReservoir','Container','Portal','LimitedAccess','PortableObject','HandTool']).
somethingIsa('ArtifactCol1022-Ferengi-Ale692',['ArtifactCol1022','Flask-LabGlassware','Bottle','FluidReservoir','Container','Portal','LimitedAccess','PortableObject','HandTool']).
somethingIsa('ArtifactCol1023-Romulan-Whisky693',['ArtifactCol1023','Flask-LabGlassware','Bottle','FluidReservoir','Container','Portal','LimitedAccess','PortableObject','HandTool']).
somethingIsa('ArtifactCol1024-Lemonade-Prune-Juice694',['ArtifactCol1024','Flask-LabGlassware','Bottle','FluidReservoir','Container','Portal','LimitedAccess','PortableObject','HandTool']).
somethingIsa('ArtifactCol1025-Vulcan-Beer695',['ArtifactCol1025','Flask-LabGlassware','Bottle','FluidReservoir','Container','Portal','LimitedAccess','PortableObject','HandTool']).
somethingIsa('Area1000',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1002',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1001',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1005',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1003',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1004',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1008',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1006',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1042',['BPVLocation','FreeSpaceContext']).
somethingIsa('Area1007',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1010',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1009',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1011',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1013',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1032',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1012',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1016',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1014',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1015',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1019',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1017',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1018',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1021',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1020',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1022',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1024',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1039',['BPVLocation','FreeSpaceContent']).
somethingIsa('Area1023',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1027',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1025',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1026',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1030',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1028',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1029',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1031',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1033',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1034',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1036',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1035',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1038',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1037',['BPVLocation','Indoors-IsolatedFromOutside','SpaceInAHOC']).
somethingIsa('Area1040',['BPVLocation','FreeSpaceContent']).
somethingIsa('Area1041',['BPVLocation','FreeSpaceContent']).
somethingIsa('ShipMap123',[item]).
verbOverride('ShipMap123',examine(this),
  prologCall(fmt("
STARSHIP ENTERPRISE

             Turbolift               |                Ten Forward
               1010                  |                  1021
                 |                   |                     |
      School---1008----Holodeck 2    |     Crusher's-----1019----Security
       1007      |       1008        |   Quarters 1018     |       1020
                 |                   |                     |
        Brig---1005---Transporter    |     Sick Bay-----1016-----Holodeck 4
        1004     |     Room 1006     |      1015          |        1017
                 |                   |                    |
    Geordi's---1002----Data's        |        Cargo-----1013-----Riker's
 Quarters 1001   |  Quarters 1003    |       Bay 1012     |    Quarters 1014
                 |                   |                    |
            Engineering              |                Turbolift
               1000                  |                  1011
                                     |
              DECK 1                 |                DECK 2
-------------------------------------+-----------------------------------------
                                     |
             Turbolift               |      Ready-----Bridge
               1032                  |     Room 1035    1038         
                 |                   |                    |
   Science-----1030-----Cargo        |                 Bridge-----Conference
   Lab 1029      |     Bay 1031      |                  1036       Room 1037
                 |                   |                    |
       Gym-----1027-----Picard's     |                Turbolift   
      1026       |   Quarters 1028   |                  1034   
                 |                   |                    |
    Troi's-----1024-----Worf's       |                Turbolift   
 Quarters 1023   |   Quarters 1025   |                  1033      
                 |                   |                
             Shuttle Bay             |                    
               1022                  |
                                     |
              DECK 3                 |                DECK 4


"))).
%somethingIsa('Area4075',['BPVLocation']).

 /* found 141 for somethingIsa(_G1422,_G1423). */ 

/* Prediate:  nameString/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(52).
number_of_clauses(2).
Pattern: nameString(_G1446,_G1447). 
 */
nameString('Area1000',"Main Engineering").
nameString('Area1001',"Geordi's Quarters").
nameString('Area1002',"A Corridor").
nameString('Area1003',"Data's Quarters").
nameString('Area1004',"The Brig").
nameString('Area1005',"A Corridor").
nameString('Area1006',"Transporter Room").
nameString('Area1007',"School").
nameString('Area1008',"A Corridor").
nameString('Area1009',"Holodeck 2").
nameString('Area1010',"Turbolift").
nameString('Area1011',"Turbolift").
nameString('Area1012',"Cargo Bay 1").
nameString('Area1013',"A Corridor").
nameString('Area1014',"Riker's Quarters").
nameString('Area1015',"Sick Bay").
nameString('Area1016',"A Corridor").
nameString('Area1017',"Holodeck 4 Entrance - A Narrow Alley").
nameString('Area1018',"Crusher's Quarters").
nameString('Area1019',"A Corridor").
nameString('Area1020',"Enterprise Security").
nameString('Area1021',"Ten Forward").
nameString('Area1022',"Shuttle Bay").
nameString('Area1023',"Troi's Quarters").
nameString('Area1024',"A Corridor").
nameString('Area1025',"Worf's Quarters").
nameString('Area1026',"Enterprise Gym").
nameString('Area1027',"A Corridor").
nameString('Area1028',"Picard's Quarters").
nameString('Area1029',"Science Lab").
nameString('Area1030',"A Corridor").
nameString('Area1031',"Cargo Bay 2").
nameString('Area1032',"Turbolift").
nameString('Area1033',"Turbolift").
nameString('Area1034',"Turbolift").
nameString('Area1035',"Picard's Ready Room").
nameString('Area1036',"Main Bridge - Upper Half").
nameString('Area1037',"Conference Room").
nameString('Area1038',"Main Bridge - Lower Half").
nameString('Area1039',"Outer Space by the Enterprise").
nameString('Area1040',"Outer Space").
nameString('Area1041',"Outer Space").
nameString('Area1042',"Transporter Beam").
nameString('NpcCol1013-Alexander671',"Alexander").
nameString(vacuum(1),"Data").
nameString(vacuum(1),"CycLBot").
nameString(vacuum(1),"CycBot").
nameString(vacuum(1),"CycBot1").
nameString('ArtifactCol1005-Boots673',"Boots").
nameString('ArtifactCol1006-Comm-Badge674',"Comm Badge").
nameString('ArtifactCol1003-Gold-Uniform675',"Gold Uniform").
nameString('ArtifactCol1000-Phaser676',"Phaser").
nameString(explorer(player1),"Player").
nameString('ArtifactCol1005-Boots773',"Boots").
nameString('ArtifactCol1006-Comm-Badge774',"Comm Badge").
nameString('ArtifactCol1003-Gold-Uniform775',"Gold Uniform").
nameString('ArtifactCol1000-Phaser776',"Phaser").
nameString('NpcCol1003-Dr-Crusher677',"Dr. Crusher").
nameString('ArtifactCol1005-Boots678',"Boots").
nameString('ArtifactCol1006-Comm-Badge679',"Comm Badge").
nameString('ArtifactCol1004-Blue-Uniform680',"Blue Uniform").
nameString('ArtifactCol1009-Medical-Tricorder681',"Medical Tricorder").
nameString('ArtifactCol1009-Medical-Tricorder682',"Medical Tricorder").
nameString('ArtifactCol1009-Medical-Tricorder683',"Medical Tricorder").
nameString('NpcCol1000-Geordi684',"Geordi").
nameString('ArtifactCol1005-Boots685',"Boots").
nameString('ArtifactCol1006-Comm-Badge686',"Comm Badge").
nameString('ArtifactCol1003-Gold-Uniform687',"Gold Uniform").
nameString('ArtifactCol1008-VISOR688',"VISOR").
nameString('NpcCol1007-Guinan689',"Guinan").
nameString('ArtifactCol1020-Tea690',"Tea").
nameString('ArtifactCol1021-Synthehol691',"Synthehol").
nameString('ArtifactCol1022-Ferengi-Ale692',"Ferengi Ale").
nameString('ArtifactCol1023-Romulan-Whisky693',"Romulan Whisky").
nameString('ArtifactCol1024-Lemonade-Prune-Juice694',"Lemonade 'Prune Juice'").
nameString('ArtifactCol1025-Vulcan-Beer695',"Vulcan Beer").
nameString('NpcCol1008-OBrien696',"O'Brien").
nameString('ArtifactCol1005-Boots697',"Boots").
nameString('ArtifactCol1006-Comm-Badge698',"Comm Badge").
nameString('ArtifactCol1003-Gold-Uniform699',"Gold Uniform").
nameString('ArtifactCol1000-Phaser700',"Phaser").
nameString('NpcCol1006-Picard701',"Picard").
nameString('ArtifactCol1005-Boots702',"Boots").
nameString('ArtifactCol1006-Comm-Badge703',"Comm Badge").
nameString('ArtifactCol1002-Red-Uniform704',"Red Uniform").
nameString('ArtifactCol1001-5-Phaser-Rifle705',"5 Phaser Rifle").
nameString('ArtifactCol1011-5-Picards-Flute706',"5 Picard's Flute").
nameString('NpcCol1005-Riker707',"Riker").
nameString('ArtifactCol1005-Boots708',"Boots").
nameString('ArtifactCol1006-Comm-Badge709',"Comm Badge").
nameString('ArtifactCol1002-Red-Uniform710',"Red Uniform").
nameString('ArtifactCol1012-Trombone711',"Trombone").
nameString('NpcCol1004-Troi712',"Troi").
nameString('ArtifactCol1005-Boots713',"Boots").
nameString('ArtifactCol1006-Comm-Badge714',"Comm Badge").
nameString('ArtifactCol1004-Blue-Uniform715',"Blue Uniform").
nameString('NpcCol1009-Wesley716',"Wesley").
nameString('ArtifactCol1005-Boots717',"Boots").
nameString('ArtifactCol1006-Comm-Badge718',"Comm Badge").
nameString('ArtifactCol1002-Red-Uniform719',"Red Uniform").
nameString('NpcCol1002-Worf720',"Worf").
nameString('ArtifactCol1005-Boots721',"Boots").
nameString('ArtifactCol1006-Comm-Badge722',"Comm Badge").
nameString('ArtifactCol1003-Gold-Uniform723',"Gold Uniform").
nameString('ArtifactCol1000-Phaser724',"Phaser").
nameString('ArtifactCol1007-Sash725',"Sash").
nameString('NpcCol1010-Livingston726',"Livingston").
nameString('NpcCol1011-Spot727',"Spot").
nameString('NpcCol1012-Ensign728',"Ensign").
nameString('ArtifactCol1005-Boots729',"Boots").
nameString('ArtifactCol1006-Comm-Badge730',"Comm Badge").
nameString('ArtifactCol1003-Gold-Uniform731',"Gold Uniform").
nameString('NpcCol1012-Ensign732',"Ensign").
nameString('ArtifactCol1005-Boots733',"Boots").
nameString('ArtifactCol1006-Comm-Badge734',"Comm Badge").
nameString('ArtifactCol1003-Gold-Uniform735',"Gold Uniform").
nameString('NpcCol1012-Ensign736',"Ensign").
nameString('ArtifactCol1005-Boots737',"Boots").
nameString('ArtifactCol1006-Comm-Badge738',"Comm Badge").
nameString('ArtifactCol1002-Red-Uniform739',"Red Uniform").
nameString('NpcCol1012-Ensign740',"Ensign").
nameString('ArtifactCol1005-Boots741',"Boots").
nameString('ArtifactCol1006-Comm-Badge742',"Comm Badge").
nameString('ArtifactCol1002-Red-Uniform743',"Red Uniform").
nameString('NpcCol1012-Ensign744',"Ensign").
nameString('ArtifactCol1005-Boots745',"Boots").
nameString('ArtifactCol1006-Comm-Badge746',"Comm Badge").
nameString('ArtifactCol1004-Blue-Uniform747',"Blue Uniform").
nameString('NpcCol1012-Ensign748',"Ensign").
nameString('ArtifactCol1005-Boots749',"Boots").
nameString('ArtifactCol1006-Comm-Badge750',"Comm Badge").
nameString('ArtifactCol1004-Blue-Uniform751',"Blue Uniform").
nameString('NpcCol1012-Ensign752',"Ensign").
nameString('ArtifactCol1005-Boots753',"Boots").
nameString('ArtifactCol1006-Comm-Badge754',"Comm Badge").
nameString('ArtifactCol1004-Blue-Uniform755',"Blue Uniform").
nameString('ArtifactCol1010-Dilithium-Crystal756',"Dilithium Crystal").
nameString('ArtifactCol1010-Dilithium-Crystal757',"Dilithium Crystal").
nameString('ArtifactCol1010-Dilithium-Crystal758',"Dilithium Crystal").
nameString('ArtifactCol1009-Tricorder759',"Tricorder").
nameString('ArtifactCol1009-Tricorder760',"Tricorder").
nameString('ArtifactCol1009-Tricorder761',"Tricorder").
nameString('NpcCol1013-Alexander671',"alexander rozhenko").
nameString(vacuum(1),"Data").
nameString(explorer(player1),"Player").
nameString('NpcCol1003-Dr-Crusher677',"Doctor Crusher").
nameString('NpcCol1000-Geordi684',"Geordi LaForge").
nameString('NpcCol1007-Guinan689',"Guinan").
nameString('NpcCol1008-OBrien696',"Chief O'Brien").
nameString('NpcCol1006-Picard701',"Captain Picard").
nameString('NpcCol1005-Riker707',"Commander Riker").
nameString('NpcCol1004-Troi712',"Counselor Troi").
nameString('NpcCol1009-Wesley716',"Wesley").
nameString('NpcCol1002-Worf720',"Lieutenant Worf").
nameString('NpcCol1010-Livingston726',"Livingston").
nameString('NpcCol1011-Spot727',"Spot").
nameString('NpcCol1012-Ensign728',"the ensign").
nameString('NpcCol1012-Ensign732',"the ensign").
nameString('NpcCol1012-Ensign736',"the ensign").
nameString('NpcCol1012-Ensign740',"the ensign").
nameString('NpcCol1012-Ensign744',"the ensign").
nameString('NpcCol1012-Ensign748',"the ensign").
nameString('NpcCol1012-Ensign752',"the ensign").
nameString('ArtifactCol1005-Boots673',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge674',"a Starfleet communication badge").
nameString('ArtifactCol1003-Gold-Uniform675',"a gold Starfleet engineering uniform").
nameString('ArtifactCol1000-Phaser676',"a standard issue phaser").
nameString('ArtifactCol1005-Boots773',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge774',"a Starfleet communication badge").
nameString('ArtifactCol1003-Gold-Uniform775',"a gold Starfleet engineering uniform").
nameString('ArtifactCol1000-Phaser776',"a standard issue phaser").
nameString('ArtifactCol1005-Boots678',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge679',"a Starfleet communication badge").
nameString('ArtifactCol1004-Blue-Uniform680',"a blue Starfleet medical uniform").
nameString('ArtifactCol1009-Medical-Tricorder681',"a medical Tricorder").
nameString('ArtifactCol1009-Medical-Tricorder682',"a medical Tricorder").
nameString('ArtifactCol1009-Medical-Tricorder683',"a medical Tricorder").
nameString('ArtifactCol1005-Boots685',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge686',"a Starfleet communication badge").
nameString('ArtifactCol1003-Gold-Uniform687',"a gold Starfleet engineering uniform").
nameString('ArtifactCol1008-VISOR688',"Geordi's VISOR").
nameString('ArtifactCol1020-Tea690',"a small cup").
nameString('ArtifactCol1021-Synthehol691',"a synthehol").
nameString('ArtifactCol1022-Ferengi-Ale692',"a Ferengi bottle").
nameString('ArtifactCol1023-Romulan-Whisky693',"a Romulan bottle").
nameString('ArtifactCol1024-Lemonade-Prune-Juice694',"a small glass").
nameString('ArtifactCol1025-Vulcan-Beer695',"a Vulcan bottle").
nameString('ArtifactCol1005-Boots697',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge698',"a Starfleet communication badge").
nameString('ArtifactCol1003-Gold-Uniform699',"a gold Starfleet engineering uniform").
nameString('ArtifactCol1000-Phaser700',"a standard issue phaser").
nameString('ArtifactCol1005-Boots702',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge703',"a Starfleet communication badge").
nameString('ArtifactCol1002-Red-Uniform704',"a burgandy Starfleet command uniform").
nameString('ArtifactCol1001-5-Phaser-Rifle705',"a phaser rifle").
nameString('ArtifactCol1011-5-Picards-Flute706',"Picard's flute").
nameString('ArtifactCol1005-Boots708',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge709',"a Starfleet communication badge").
nameString('ArtifactCol1002-Red-Uniform710',"a burgandy Starfleet command uniform").
nameString('ArtifactCol1012-Trombone711',"Riker's trombone").
nameString('ArtifactCol1005-Boots713',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge714',"a Starfleet communication badge").
nameString('ArtifactCol1004-Blue-Uniform715',"a blue Starfleet medical uniform").
nameString('ArtifactCol1005-Boots717',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge718',"a Starfleet communication badge").
nameString('ArtifactCol1002-Red-Uniform719',"a burgandy Starfleet command uniform").
nameString('ArtifactCol1005-Boots721',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge722',"a Starfleet communication badge").
nameString('ArtifactCol1003-Gold-Uniform723',"a gold Starfleet engineering uniform").
nameString('ArtifactCol1000-Phaser724',"a standard issue phaser").
nameString('ArtifactCol1007-Sash725',"Worf's sash").
nameString('ArtifactCol1005-Boots729',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge730',"a Starfleet communication badge").
nameString('ArtifactCol1003-Gold-Uniform731',"a gold Starfleet engineering uniform").
nameString('ArtifactCol1005-Boots733',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge734',"a Starfleet communication badge").
nameString('ArtifactCol1003-Gold-Uniform735',"a gold Starfleet engineering uniform").
nameString('ArtifactCol1005-Boots737',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge738',"a Starfleet communication badge").
nameString('ArtifactCol1002-Red-Uniform739',"a burgandy Starfleet command uniform").
nameString('ArtifactCol1005-Boots741',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge742',"a Starfleet communication badge").
nameString('ArtifactCol1002-Red-Uniform743',"a burgandy Starfleet command uniform").
nameString('ArtifactCol1005-Boots745',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge746',"a Starfleet communication badge").
nameString('ArtifactCol1004-Blue-Uniform747',"a blue Starfleet medical uniform").
nameString('ArtifactCol1005-Boots749',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge750',"a Starfleet communication badge").
nameString('ArtifactCol1004-Blue-Uniform751',"a blue Starfleet medical uniform").
nameString('ArtifactCol1005-Boots753',"a pair of Starfleet black boots").
nameString('ArtifactCol1006-Comm-Badge754',"a Starfleet communication badge").
nameString('ArtifactCol1004-Blue-Uniform755',"a blue Starfleet medical uniform").
nameString('ArtifactCol1010-Dilithium-Crystal756',"a dilithium crystal").
nameString('ArtifactCol1010-Dilithium-Crystal757',"a dilithium crystal").
nameString('ArtifactCol1010-Dilithium-Crystal758',"a dilithium crystal").
nameString('ArtifactCol1009-Tricorder759',"a medical Tricorder").
nameString('ArtifactCol1009-Tricorder760',"a medical Tricorder").
nameString('ArtifactCol1009-Tricorder761',"a medical Tricorder").

 /* found 322 for nameString(_G1446,_G1447). */ 

/* Prediate:  somethingDescription/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(33).
number_of_clauses(1).
Pattern: somethingDescription(_G1470,_G1471). 
 */
somethingDescription('NpcCol1000-Geordi684',["Lieutenant","Commander","Geordi","LaForge","Geordi LaForge","Lieutenant Commander Geordi LaForge is standing here","Geordi is the Chief Engineer of the Enterprise","He's blind, so he wears a special VISOR that lets him see things","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Geordi","Geordi LaForge","Lieutenant Commander Geordi LaForge is standing here","Geordi is the Chief Engineer of the Enterprise","He's blind, so he wears a special VISOR that lets him see things"]).
somethingDescription(vacuum(1),["Lieutenant","Commander","Data","Android","Data","Lieutenant Commander Data is here, trying to be more human","Data is the only android on the Enterprise, and the only android in all of Starfleet","He possess super-human strength, and is extremely tough","ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON","NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 1","mudMaxHitPoints: 18d18+4000","#$PunchingSomething mudBareHandDamage: 10d10+75","Data","CycLBot","CycBot","CycBot1","Data","Lieutenant Commander Data is here, trying to be more human","Data is the only android on the Enterprise, and the only android in all of Starfleet","He possess super-human strength, and is extremely tough"]).
somethingDescription(explorer(player1),["Lieutenant","Commander","Human","Player",
            "Explorer Player",
            "ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON",
            "NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 1","mudMaxHitPoints: 18d18+4000",
            "#$PunchingSomething mudBareHandDamage: 10d10+75","Player","Player","Human",
            "Logged on player character"]).
somethingDescription('NpcCol1002-Worf720',["Lieutenant","Worf","Klingon","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3400","#$PunchingSomething mudBareHandDamage: 9d9+60","Worf","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong"]).
somethingDescription('NpcCol1003-Dr-Crusher677',["Doctor","Beverly","Crusher","Doctor Crusher","Lieutenant Beverly Crusher is here, looking for someone to heal","Doctor Crusher is the Enterprise's Chief Medical Officer","Wesley is her son","Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Dr. Crusher","Doctor Crusher","Lieutenant Beverly Crusher is here, looking for someone to heal","Doctor Crusher is the Enterprise's Chief Medical Officer","Wesley is her son","Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard"]).
somethingDescription('NpcCol1004-Troi712',["Counselor","Deanna","Troi","Counselor Troi","Counselor Deanna Troi is here","Counselor Troi is the ship's main counselor","She's half betazoid, which means that she can read people's minds","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Troi","Counselor Troi","Counselor Deanna Troi is here","Counselor Troi is the ship's main counselor","She's half betazoid, which means that she can read people's minds"]).
somethingDescription('NpcCol1005-Riker707',["Commander","William","Riker","Commander Riker","Commander William Riker is here, staring at you","Commander Riker is the Enterprise's first officer","He's in charge of keeping the crew in line","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+52","Riker","Commander Riker","Commander William Riker is here, staring at you","Commander Riker is the Enterprise's first officer","He's in charge of keeping the crew in line"]).
somethingDescription('NpcCol1006-Picard701',["Captain","Jean","Luc","Jean-Luc","Picard","Captain Picard","Captain Jean-Luc Picard is standing here, watching you","Captain Picard is a very important man","He's in charge of Starfleet's flagship, the Enterprise","He's very smart, and very wise","Don't mess with him!","ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON","NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_SANCTUARY","NPC_NOTRACK","+mudToHitArmorClass0: 0","mudMaxHitPoints: 20d20+5000","#$PunchingSomething mudBareHandDamage: 12d12+75","Picard","Captain Picard","Captain Jean-Luc Picard is standing here, watching you","Captain Picard is a very important man","He's in charge of Starfleet's flagship, the Enterprise","He's very smart, and very wise","Don't mess with him!"]).
somethingDescription('NpcCol1007-Guinan689',["Guinan","Guinan","Guinan is here, tending the bar","Guinan is a strange being","She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 4","mudMaxHitPoints: 12d12+2600","#$PunchingSomething mudBareHandDamage: 9d9+36","Guinan","Guinan","Guinan is here, tending the bar","Guinan is a strange being","She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender"]).
somethingDescription('NpcCol1008-OBrien696',["Chief","O'Brien","Transporter","Chief O'Brien","Chief O'Brien is here, waiting to teleport you somwhere","Chief O'Brien is the transporter chief on the Enterprise","It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 4","mudMaxHitPoints: 12d12+2600","#$PunchingSomething mudBareHandDamage: 9d9+36","O'Brien","Chief O'Brien","Chief O'Brien is here, waiting to teleport you somwhere","Chief O'Brien is the transporter chief on the Enterprise","It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms"]).
somethingDescription('NpcCol1009-Wesley716',["Wesley","Crusher","Wesley","Wesley Crusher is here, eagerly trying to earn your praise","Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge","He got this position only because Captain Picard feels guilty about killing his father","ACT_STAY_ZONE","ACT_WIMPY","wimpy mobile will try to flee when it gets low on hit points. A mobile which is both aggressive and wimpy will not attack a player that is awake","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+1400","#$PunchingSomething mudBareHandDamage: 9d9+24","Wesley","Wesley","Wesley Crusher is here, eagerly trying to earn your praise","Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge","He got this position only because Captain Picard feels guilty about killing his father"]).
somethingDescription('NpcCol1010-Livingston726',["Livingston","fish","Livingston","Livingston the fish is here, swimming about in his tank","Livingston is Captain Picard's pet fish","He's some sort of exotic breed, and he's expensive to feed and keep alive","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+800","#$PunchingSomething mudBareHandDamage: 9d9+14","Livingston","Livingston","Livingston the fish is here, swimming about in his tank","Livingston is Captain Picard's pet fish","He's some sort of exotic breed, and he's expensive to feed and keep alive"]).
somethingDescription('NpcCol1011-Spot727',["spot","the","cat","Spot","Spot, Data's pet cat, is sitting here looking at you","Spot is Data's orange coloured cat","Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+800","#$PunchingSomething mudBareHandDamage: 9d9+14","Spot","Spot","Spot, Data's pet cat, is sitting here looking at you","Spot is Data's orange coloured cat","Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal"]).
somethingDescription('NpcCol1012-Ensign728',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
somethingDescription('NpcCol1012-Ensign732',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
somethingDescription('NpcCol1012-Ensign736',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
somethingDescription('NpcCol1012-Ensign740',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
somethingDescription('NpcCol1012-Ensign744',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
somethingDescription('NpcCol1012-Ensign748',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
somethingDescription('NpcCol1012-Ensign752',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
somethingDescription('NpcCol1013-Alexander671',["alexander","rozhenko","alexander rozhenko","Alexander Rozhenko is here, practicing laughing hour","Alexander Rozhenko is Worf's son","His mother was half human and half Klingon, so Alexander is 3/4 Klingon","He's quite small, but since he's a Klingon he's very strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Alexander","alexander rozhenko","Alexander Rozhenko is here, practicing laughing hour","Alexander Rozhenko is Worf's son","His mother was half human and half Klingon, so Alexander is 3/4 Klingon","He's quite small, but since he's a Klingon he's very strong"]).
somethingDescription('ArtifactCol1000-Phaser676',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
somethingDescription('ArtifactCol1000-Phaser776',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
somethingDescription('ArtifactCol1000-Phaser700',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
somethingDescription('ArtifactCol1000-Phaser724',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
somethingDescription('ArtifactCol1001-5-Phaser-Rifle705',["phaser","rifle","a phaser rifle","A large phaser rifle is lying here","damageNumberDice 7","damageSizeDice 6","WeaponBlasting","This phaser rifle looks pretty powerful. These weapons are used mainly on assault type missions, where power is important","5 Phaser Rifle","a phaser rifle"]).
somethingDescription('ArtifactCol1002-Red-Uniform704',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
somethingDescription('ArtifactCol1002-Red-Uniform710',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
somethingDescription('ArtifactCol1002-Red-Uniform719',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
somethingDescription('ArtifactCol1002-Red-Uniform739',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
somethingDescription('ArtifactCol1002-Red-Uniform743',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
somethingDescription('ArtifactCol1003-Gold-Uniform675',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
somethingDescription('ArtifactCol1003-Gold-Uniform775',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
somethingDescription('ArtifactCol1003-Gold-Uniform687',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
somethingDescription('ArtifactCol1003-Gold-Uniform699',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
somethingDescription('ArtifactCol1003-Gold-Uniform723',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
somethingDescription('ArtifactCol1003-Gold-Uniform731',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
somethingDescription('ArtifactCol1003-Gold-Uniform735',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
somethingDescription('ArtifactCol1004-Blue-Uniform680',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
somethingDescription('ArtifactCol1004-Blue-Uniform715',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
somethingDescription('ArtifactCol1004-Blue-Uniform747',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
somethingDescription('ArtifactCol1004-Blue-Uniform751',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
somethingDescription('ArtifactCol1004-Blue-Uniform755',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
somethingDescription('ArtifactCol1005-Boots673',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots773',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots678',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots685',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots697',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots702',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots708',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots713',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots717',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots721',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots729',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots733',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots737',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots741',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots745',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots749',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1005-Boots753',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
somethingDescription('ArtifactCol1006-Comm-Badge674',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge774',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge679',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge686',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1",
                     "These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge698',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge703',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge709',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge714',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge718',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge722',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge730',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge734',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge738',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge742',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge746',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge750',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1006-Comm-Badge754',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
somethingDescription('ArtifactCol1007-Sash725',["worf's","worf","sash","Worf's sash","Worf's silver chain sash has been left here","armorLevel: 8","Worf's sash is some sort of Klingon clothing. Worf always wears it, which makes you wonder how you managed to get a hold of it..","Sash","Worf's sash"]).
somethingDescription('ArtifactCol1008-VISOR688',["geordi","geordi's","visor","Geordi's VISOR","Geordi's VISOR is lying here","armorLevel: 2","Geordi's VISOR was made specially for him, because he's blind. This piece of equipment allows him to see things, but differently than normal eyes. I wonder how Geordi is managing, now that you've stolen his only way of seeing?","VISOR","Geordi's VISOR"]).
somethingDescription('ArtifactCol1009-Medical-Tricorder681',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
somethingDescription('ArtifactCol1009-Medical-Tricorder682',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
somethingDescription('ArtifactCol1009-Medical-Tricorder683',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
somethingDescription('ArtifactCol1009-Tricorder759',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Tricorder","a medical Tricorder"]).
somethingDescription('ArtifactCol1009-Tricorder760',["medical","tricorder",
                     "a medical Tricorder","A medical Tricorder is lying here, ready to be used",
                     "mudLevelOf: 10",
                     "chargeCapacity: 5",
                     "chargeRemaining: 5",
                     "This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it",
                     "Tricorder","a medical Tricorder"]).
somethingDescription('ArtifactCol1009-Tricorder761',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Tricorder","a medical Tricorder"]).
somethingDescription('ArtifactCol1010-Dilithium-Crystal756',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
somethingDescription('ArtifactCol1010-Dilithium-Crystal757',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
somethingDescription('ArtifactCol1010-Dilithium-Crystal758',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
somethingDescription('ArtifactCol1011-5-Picards-Flute706',["picard","picard's","flute","Picard's flute","Captain Picard's wooden flute is sitting here","Captain Picard recieved this flute when he lost his memory and was stuck on some strange world. Now, he plays it to relieve stress","5 Picard's Flute","Picard's flute"]).
somethingDescription('ArtifactCol1012-Trombone711',["riker","riker's","trombone","Riker's trombone","Commander Riker's trombone has been placed here","Commander Riker considers himself to be a talented jazz musician. He practices on this trombone all the time","Trombone","Riker's trombone"]).
somethingDescription('ArtifactCol1020-Tea690',["tea","cup","a small cup","A small cup of tea is sitting here","Tea","a small cup"]).
somethingDescription('ArtifactCol1021-Synthehol691',["wine","bottle","synthehol","a synthehol","A bottle of synthehol is standing here","Synthehol","a synthehol"]).
somethingDescription('ArtifactCol1022-Ferengi-Ale692',["ale","ferengi","bottle","a Ferengi bottle","A bottle of Ferengi ale is sitting here","Ferengi Ale","a Ferengi bottle"]).
somethingDescription('ArtifactCol1023-Romulan-Whisky693',["whisky","whiskey","romulan","bottle","a Romulan bottle","A bottle of Romulan whiskey is sitting here","Romulan Whisky","a Romulan bottle"]).
somethingDescription('ArtifactCol1024-Lemonade-Prune-Juice694',["lemonade","prune","juice","glass","a small glass","A small glass of prune juice is sitting here","Lemonade 'Prune Juice'","a small glass"]).
somethingDescription('ArtifactCol1025-Vulcan-Beer695',["beer","vulcan","bottle","a Vulcan bottle","A bottle of Vulcan beer is standing here","Vulcan Beer","a Vulcan bottle"]).
somethingDescription('Area1000',["Main Engineering","You find yourself in the middle of main engineering","The room is longer than it is wide, and it has fairly low ceilings","Computer terminals cover all the walls, and a large table built into the floor sits in the middle of the room","At the far end of the room you see the warp core, a large pulsating vertical tube"]).
somethingDescription('Area1002',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
somethingDescription('Area1001',["Geordi's Quarters","You're in the middle of Geordi's quarters","The room is sparsely decorated, due to the fact that Geordi is blind","A small personal computer sits on a desk against the western wall, in between two windows that look out into space","A neatly made bed has been placed against the northern wall"]).
somethingDescription('Area1005',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
somethingDescription('Area1003',["Data's Quarters","You're in the middle of Data's quarters","Some easils and paintings have been left scattered around the southern part of the room, while a huge computer screen showing a cross section of the Enterprise covers the entire northern wall","In front of the screen is a large desk, which is covered in computer controls","You can't see a bed in this room, but you figure it's because Data doesn't sleep"]).
somethingDescription('Area1004',["The Brig","You're in the dimly lit Brig","This is where all the criminals and prisoners are kept while on board the Enterprise","Three fairly large cells can been seen in the southern part of the room, and they're all empty","A computer control panel is situated in the northwestern corner of the room, which is where the force fields for the cells are controlled",'The panel says:

***************************************************
*                                                 *
*            NCC-1701-D - ENTERPRISE              *
*                                                 *
*              *****                              *
*      **********************                     *
*      ***********************  _________         *
*              *****        ***(___  ____(        *
*                            ***** \\ \\*           *
*                             **********          *
*                                                 *
*          You are currently on deck 1            *
*                                                 *
***************************************************
']).
somethingDescription('Area1008',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You see the holodeck's control panel beside the holodeck door, and it has some information on it"]).
somethingDescription('Area1006',["Transporter Room","You're in the Enterprise transporter room","A computer terminal is sitting near the southern wall, where the transporter chief can control the transporters","Eight round transport pads have been arranged in a circle, on a raised platform against the northern wall"]).
somethingDescription('Area1042',["Transporter Beam","You find yourself in a transporter beam","All you can see is blue flashing light","It feels as though your body is racing around at high speeds","As you try to look down at your body, you realize that there's nothing there!"]).
somethingDescription('Area1007',["School","You step through the doors and find yourself in a large school room","Various tables and chairs are set up all around the room, and many paintings and drawings have been attached to the walls","Several computer consoles with a children's interface on them can be seen on the tables"]).
somethingDescription('Area1010',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
somethingDescription('Area1009',["Holodeck 2","You're now on Holodeck 2","The room is just a large cube, with jet black walls and a yellow grid painted on the floors, the walls, and the ceiling","This is where different programs can be loaded and experienced, which seem totally real","Right now, this holodeck is not functioning",'
***************************************************
*                                                 *
*            NCC-1701-D - "ENTERPRISE"            *
*                    HOLODECK 2                   *
*                                                 *
*              STATUS : Inactive                  *
*     CURRENT PROGRAM : N/A                       *
*            SAFETIES : N/A                       *
*                                                 *
*    NOTE: Starfleet is not responsible for       *
*          any injuries incurred while on this    *
*          holodeck!                              *
*                                                 *
* WARNING: While the safeties are disabled, you   *
*          CAN be injured, or even killed.        *
*                                                 *
***************************************************']).
somethingDescription('Area1011',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can accessthe other decks on the Enterprise"]).
somethingDescription('Area1013',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
somethingDescription('Area1032',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
somethingDescription('Area1012',["Cargo Bay 1","You're in the main cargo bay of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling"]).
somethingDescription('Area1016',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You see the holodeck's control panel beside the holodeck door, and it has some information on it"]).
somethingDescription('Area1014',["Riker's Quarters","You've arrived in Riker's quarters","The room is very neat and tidy, with a couch and several chairs aranged around a coffee table by the eastern wall","A small partition at the northern part of the room seperates his sleeping area with the rest of the room"]).
somethingDescription('Area1015',["Sick Bay","You're in the middle of the Enterprise's Sick Bay","About a dozen beds are arranged along the walls of the room, while several carts covered with medical supplies are scattered around the room","A large glass window in the northern part of the room separates the doctor's office with the rest of the room",'
***************************************************
*                                                 *
*            NCC-1701-D - "ENTERPRISE"            *
*                    HOLODECK 4                   *
*                                                 *
*              STATUS : Active                    *
*     CURRENT PROGRAM : Sherlock Holmes (19th     *
*                       century London)           *
*            SAFETIES : Disabled                  *
*                                                 *
*    NOTE: Starfleet is not responsible for       *
*          any injuries incurred while on this    *
*          holodeck!                              *
*                                                 *
* WARNING: While the safeties are disabled, you   *
*          CAN be injured, or even killed.        *
*                                                 *
*             ---ENTER WHEN READY---              *
*                                                 *
***************************************************
']).
somethingDescription('Area1019',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
somethingDescription('Area1017',["Holodeck 4 Entrance - A Narrow Alley","You emerge into a dark narrow alley","Tall dark brick buildings block your way north and south","You can see that the windows on the buildings are fairly high, and some have been boarded up","The smell from the rotting food and garbage mixing with the foul water on the ground is unbearable","You can hear the sounds of a bustling marketpace to the east","The archway leading out of the holodeck is west"]).
somethingDescription('Area1018',["Crusher's Quarters","You're in Doctor Crusher's quarters","Several different paintings are attached to the walls, and you also notice a few sculptures","A neatly made bed is located by the northern wall, in between two large windows looking out into space"]).
somethingDescription('Area1021',["Ten Forward","You're now in Ten Forward, the entertainment room of the Enterprise","The entire northern wall is covered with windows looking out into space, while two large wooden doors with the Starfleet insignia stamped on them face south","Many round metal tables are scattered around the room, surrounded by metal chairs","A long bar spans almost the entire length of the southern part of the room, and about two dozen bar stools are sitting in front of it","It's very noisy in here, due to all the talking and laughing"]).
somethingDescription('Area1020',["Enterprise Security","You're standing in the dimly lit Enterprise Security","Weapons lockers cover all of the walls, except along the northern wall, where a large glass window protects dozens of different phasors, blaster rifles, and other high tech weapons","Three long tables surrounded by chairs stretch across the room"]).
somethingDescription('Area1022',["Shuttle Bay","You're in the main shuttle bay of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see three different shuttle crafts sitting here, waiting to be flown","A large grey door leads into space"]).
somethingDescription('Area1024',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
somethingDescription('Area1039',["Outer Space by the Enterprise","You're floating in outer space right beside the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold","A large grey door leads into the Enterprise's Shuttle Bay"]).
somethingDescription('Area1023',["Troi's Quarters","You're in Counselor Deanna Troi's quarters","Several different paintings have been hung from the walls, and a small couch and a recliner are positioned around a coffee table","A neatly made bed is partially hidden behind a curtain at the northern part of the room"]).
somethingDescription('Area1027',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape",
"
***************************************************
*                                                 *
*            NCC-1701-D - ENTERPRISE            *
*                                                 *
*              *****                              *
*      **********************                     *
*      ***********************  _________         *
*              *****        ***(___  ____(        *
*                            ***** \\ \\*           *
*                             **********          *
*                                                 *
*          You are currently on deck 3            *
*                                                 *
***************************************************
"]).
somethingDescription('Area1025',["Worf's Quarters","You're in Worf's quarters","A small table is sitting in the southeastern corner, and on it is a small potted plant","An impressive selection of Klingon weapons have been mounted on the northern wall, and a partition splits this room with Worf's bedroom to the east"]).
somethingDescription('Area1026',["Enterprise Gym","You emerge into the Enterprise gym","The room is quite large, with a soft grey floor","A set of lockers against the southern wall contain all of the necessary equipment needed for using the gym","A thick stack of mats have been piled high in one corner, which can be used for different activities","Captain Picard likes to come here to practice his fencing"]).
somethingDescription('Area1030',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
somethingDescription('Area1028',["Picard's Quarters","You find yourself standing by the door of Captain Picard's quarters","He isn't very fond of visitors, but you decide to stay and have a look around","You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south","A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table","Two large windows offer a great view of space","A small partition at the northern part of the room contains Picard's sleeping area"]).
somethingDescription('Area1029',["Science Lab","You're in the Enterprise science lab","A strange looking machine sits in the middle of the room, up on a slightly raised platform","It looks as though something(or someone) could be placed inside, hooked up to the multitude of wires and cables, and have scientific tests performed on it(or them)","A complex looking computer console is facing this machine","Around the rest of the room are counterops with with the odd computer terminal"]).
somethingDescription('Area1031',["Cargo Bay 2","You're in the cargo bay 2 of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling"]).
somethingDescription('Area1033',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
somethingDescription('Area1034',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
somethingDescription('Area1036',["Main Bridge - Upper Half","You find yourself on the upper half of the main bridge of the USS Enterprise","Directly in front of you is a thick railing that contains many different computer panels used for the tactical systems of the ship","The entire southern wall is covered with computer consoles, where the ship's main systems are controlled","Two small curved ramps on either side of the room lead north to the lower part of the bridge, and a large circular skylight shows the space outside the ship"]).
somethingDescription('Area1035',["Picard's Ready Room","You're standing in Captain Picard's ready room","A long couch has been placed beside the door, while a large U shaped desk is located by the northern wall","A small computer screen is sitting on the desk, as well as several other papers and documents","A single high window beside the desk looks into space, and a fish tank is located in the northwestern corner of the room","This is where the Captain makes all of his important decisions"]).
somethingDescription('Area1038',["Main Bridge - Lower Half","You find yourself on the lower half of the main bridge of the USS Enterprise","An enormous view screen covers almost the entire northern wall, and is currently displaying a view of the stars rushing by","Three large chairs in the northern part of the room, in front of the railing, face the screen","This is where the Captain, Commander Riker, and Counselor Troi sit","Two computer consoles with built in chairs rest about ten feet in front of the chairs, also facing the view screen","This is where the ship's pilot and information officer sit"]).
somethingDescription('Area1037',["Conference Room","You're in the conference room of the Enterprise","A large glass rectangular table sits in the middle of the room, surrounded by about a dozen comfortable looking office chairs","The entire eastern wall is covered with windows, looking out into space","This is where the senior officers of the Enterprise meet and discuss important issues"]).
somethingDescription('Area1040',["Outer Space","You're floating in outer space right above the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold"]).
somethingDescription('Area1041',["Outer Space","You're floating in outer space right above the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold"]).


 /* found 141 for somethingDescription(_G1470,_G1471). */ 

/* Prediate:  pathBetween/3 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(31).
number_of_clauses(1).
Pattern: pathBetween(_G1494,_G1495,_G1496). 
 */
pathBetween('Area1000',"n",'Area1002').
pathBetween('Area1001',"e",'Area1002').
pathBetween('Area1002',"n",'Area1005').
pathBetween('Area1002',"e",'Area1003').
pathBetween('Area1002',"s",'Area1000').
pathBetween('Area1002',"w",'Area1001').
pathBetween('Area1003',"w",'Area1002').
pathBetween('Area1004',"e",'Area1005').
pathBetween('Area1005',"n",'Area1008').
pathBetween('Area1005',"e",'Area1006').
pathBetween('Area1005',"s",'Area1002').
pathBetween('Area1005',"w",'Area1004').
pathBetween('Area1006',"n",'Area1042').
pathBetween('Area1006',"w",'Area1005').
pathBetween('Area1007',"e",'Area1008').
pathBetween('Area1008',"n",'Area1010').
pathBetween('Area1008',"e",'Area1009').
pathBetween('Area1008',"s",'Area1005').
pathBetween('Area1008',"w",'Area1007').
pathBetween('Area1009',"w",'Area1008').
pathBetween('Area1010',"s",'Area1008').
pathBetween('Area1010',"u",'Area1011').
pathBetween('Area1011',"n",'Area1013').
pathBetween('Area1011',"u",'Area1032').
pathBetween('Area1011',"d",'Area1010').
pathBetween('Area1012',"e",'Area1013').
pathBetween('Area1013',"n",'Area1016').
pathBetween('Area1013',"e",'Area1014').
pathBetween('Area1013',"s",'Area1011').
pathBetween('Area1013',"w",'Area1012').
pathBetween('Area1014',"w",'Area1013').
pathBetween('Area1015',"e",'Area1016').
pathBetween('Area1016',"n",'Area1019').
pathBetween('Area1016',"e",'Area1017').
pathBetween('Area1016',"s",'Area1013').
pathBetween('Area1016',"w",'Area1015').
pathBetween('Area1017',"w",'Area1016').
pathBetween('Area1018',"e",'Area1019').
pathBetween('Area1019',"n",'Area1021').
pathBetween('Area1019',"e",'Area1020').
pathBetween('Area1019',"s",'Area1016').
pathBetween('Area1019',"w",'Area1018').
pathBetween('Area1020',"w",'Area1019').
pathBetween('Area1021',"s",'Area1019').
pathBetween('Area1022',"n",'Area1024').
pathBetween('Area1022',"s",'Area1039').
pathBetween('Area1023',"e",'Area1024').
pathBetween('Area1024',"n",'Area1027').
pathBetween('Area1024',"e",'Area1025').
pathBetween('Area1024',"s",'Area1022').
pathBetween('Area1024',"w",'Area1023').
pathBetween('Area1025',"w",'Area1024').
pathBetween('Area1026',"e",'Area1027').
pathBetween('Area1027',"n",'Area1030').
pathBetween('Area1027',"e",'Area1028').
pathBetween('Area1027',"s",'Area1024').
pathBetween('Area1027',"w",'Area1026').
pathBetween('Area1028',"w",'Area1027').
pathBetween('Area1029',"e",'Area1030').
pathBetween('Area1030',"n",'Area1032').
pathBetween('Area1030',"e",'Area1031').
pathBetween('Area1030',"s",'Area1027').
pathBetween('Area1030',"w",'Area1029').
pathBetween('Area1031',"w",'Area1030').
pathBetween('Area1032',"s",'Area1030').
pathBetween('Area1032',"u",'Area1033').
pathBetween('Area1032',"d",'Area1011').
pathBetween('Area1033',"u",'Area1034').
pathBetween('Area1033',"d",'Area1032').
pathBetween('Area1034',"u",'Area1036').
pathBetween('Area1034',"d",'Area1033').
pathBetween('Area1035',"e",'Area1038').
pathBetween('Area1036',"n",'Area1038').
pathBetween('Area1036',"e",'Area1037').
pathBetween('Area1036',"d",'Area1034').
pathBetween('Area1037',"w",'Area1036').
pathBetween('Area1038',"s",'Area1036').
pathBetween('Area1038',"w",'Area1035').
pathBetween('Area1039',"n",'Area1022').
pathBetween('Area1039',"u",'Area1040').
pathBetween('Area1040',"u",'Area1041').
pathBetween('Area1040',"d",'Area1039').
pathBetween('Area1041',"u",'Area1041').
pathBetween('Area1041',"d",'Area1040').
% pathBetween('Area1042',"n",'Area4075').
pathBetween('Area1042',"s",'Area1006').

 /* found 86 for pathBetween(_G1494,_G1495,_G1496). */ 

/* Prediate:  pathName/3 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(32).
number_of_clauses(1).
Pattern: pathName(_G1519,_G1520,_G1521). 
 */
pathName('Area1000',"n","A corridor is North").
pathName('Area1001',"e","A corridor is East").
pathName('Area1002',"n","The corridor continues North").
pathName('Area1002',"e","Data's Quarters are East").
pathName('Area1002',"s","Main Engineering is South").
pathName('Area1002',"w","Geordi's Quarters are West").
pathName('Area1003',"w","A corridor is West").
pathName('Area1004',"e","A corridor is East").
pathName('Area1005',"n","The corridor continues North").
pathName('Area1005',"e","The Transporter Room is East").
pathName('Area1005',"s","The corridor continues South").
pathName('Area1005',"w","The Brig is West").
pathName('Area1006',"n","A transporter beam is North").
pathName('Area1006',"w","A corridor is West").
pathName('Area1007',"e","A corridor is East").
pathName('Area1008',"n","The turbolift is North").
pathName('Area1008',"e","Holodeck 2 is East").
pathName('Area1008',"s","The corridor continues South").
pathName('Area1008',"w","The School is West").
pathName('Area1009',"w","A corridor is West").
pathName('Area1010',"s","A corridor leads South").
pathName('Area1010',"u","The turbolift goes Up").
pathName('Area1011',"n","A corridor leads North").
pathName('Area1011',"u","The turbolift goes Up").
pathName('Area1011',"d","The turbolift goes Down").
pathName('Area1012',"e","A corridor is East").
pathName('Area1013',"n","The corridor continues North").
pathName('Area1013',"e","Riker's Quarters are East").
pathName('Area1013',"s","The Turbolift is South").
pathName('Area1013',"w","The Cargo Bay is West").
pathName('Area1014',"w","A corridor is West").
pathName('Area1015',"e","A corridor is East").
pathName('Area1016',"n","The corridor continues North").
pathName('Area1016',"e","Holodeck 4 is East").
pathName('Area1016',"s","The corridor continues South").
pathName('Area1016',"w","Sick Bay is West").
pathName('Area1017',"w","A corridor is West").
pathName('Area1018',"e","A corridor is East").
pathName('Area1019',"n","Ten Forward is North").
pathName('Area1019',"e","Security is East").
pathName('Area1019',"s","The corridor continues South").
pathName('Area1019',"w","Crusher's Quarters are West").
pathName('Area1020',"w","A corridor is West").
pathName('Area1021',"s","A corridor is South").
pathName('Area1022',"n","A corridor is North").
pathName('Area1023',"e","A corridor is East").
pathName('Area1024',"n","The corridor continues North").
pathName('Area1024',"e","Worf's Quarters are East").
pathName('Area1024',"s","The shuttle bay is South").
pathName('Area1024',"w","Troi's Quarters are West").
pathName('Area1025',"w","A corridor is West").
pathName('Area1026',"e","A corridor is East").
pathName('Area1027',"n","The corridor continues North").
pathName('Area1027',"e","Picard's Quarters are East").
pathName('Area1027',"s","The corridor continues South").
pathName('Area1027',"w","The Enterprise Gym is West").
pathName('Area1028',"w","A corridor is West").
pathName('Area1029',"e","A corridor is East").
pathName('Area1030',"n","The Turbolift is North").
pathName('Area1030',"e","Empty Quarters are East").
pathName('Area1030',"s","The corridor continues South").
pathName('Area1030',"w","Empty Quarters are West").
pathName('Area1031',"w","A corridor is West").
pathName('Area1032',"s","A corridor leads South").
pathName('Area1032',"u","The turbolift goes Up").
pathName('Area1032',"d","The turbolift goes Down").
pathName('Area1033',"u","The turbolift goes Up").
pathName('Area1033',"d","The turbolift goes Down").
pathName('Area1034',"u","The Main Bridge is Up").
pathName('Area1034',"d","The turbolift goes Down").
pathName('Area1035',"e","The Main Bridge - Lower Half is East").
pathName('Area1036',"n","The Main Bridge - Lower Half is North").
pathName('Area1036',"e","The Conference Room is East").
pathName('Area1036',"d","The Turbolift is Down").
pathName('Area1037',"w","The Main Bridge - Upper Half is West").
pathName('Area1038',"s","The Main Bridge - Upper Half is South").
pathName('Area1038',"w","The Captain's Ready Room is West").
pathName('Area1039',"n","The Shuttle Bay is North").
pathName('Area1039',"u","Outer Space is Up").
pathName('Area1040',"u","Outer Space is Up").
pathName('Area1040',"d","Outer Space is Down").
pathName('Area1041',"u","* The Galaxy is Up").
pathName('Area1041',"d","Outer Space is Down").
pathName('Area1042',"s","The Transporter Room is South").

 /* found 84 for pathName(_G1519,_G1520,_G1521). */ 
/*
somethingIsa0(X,Y):-somethingIsa(X,List),member(Y,List).


domain_name(startrek).

sorts(non_primitive_sorts, [
       region, city_location,tcentre,not_tcentre, route, 
       physical_obj, vehicle, railv, actorNPC, dir]).
sorts(primitive_sorts, [
        airport, aircraft, train_station, post_office, clocation, city, package, 
        train, traincar, truck, road_route, rail_route, region,agent]).

sorts(physical_obj, [vehicle, package,'PortableObject',agent]).
sorts('PortableObject',['SomethingToWear','Weapon','ControlDevice','ProtectiveAttire','FluidReservoir']).
sorts(actorNPC, [agent]).
*/


% objects(SW,List):-member(SW,['Weapon','ControlDevice','ProtectiveAttire','FluidReservoir','BPVLocation','PortableObject','SomethingToWear',agent,'SomethingToWear']),findall(O,somethingIsa0(O,SW),ULIST),sort(ULIST,List).
%SLOW 
objects('Weapon',['ArtifactCol1000-Phaser676','ArtifactCol1000-Phaser700','ArtifactCol1000-Phaser724','ArtifactCol1000-Phaser776','ArtifactCol1001-5-Phaser-Rifle705']).
objects('ControlDevice',['ArtifactCol1009-Medical-Tricorder681','ArtifactCol1009-Medical-Tricorder682','ArtifactCol1009-Medical-Tricorder683','ArtifactCol1009-Tricorder759','ArtifactCol1009-Tricorder760','ArtifactCol1009-Tricorder761']).
objects('ProtectiveAttire',['ArtifactCol1002-Red-Uniform704','ArtifactCol1002-Red-Uniform710','ArtifactCol1002-Red-Uniform719','ArtifactCol1002-Red-Uniform739','ArtifactCol1002-Red-Uniform743','ArtifactCol1003-Gold-Uniform675','ArtifactCol1003-Gold-Uniform687','ArtifactCol1003-Gold-Uniform699','ArtifactCol1003-Gold-Uniform723','ArtifactCol1003-Gold-Uniform731','ArtifactCol1003-Gold-Uniform735','ArtifactCol1003-Gold-Uniform775','ArtifactCol1004-Blue-Uniform680','ArtifactCol1004-Blue-Uniform715','ArtifactCol1004-Blue-Uniform747','ArtifactCol1004-Blue-Uniform751','ArtifactCol1004-Blue-Uniform755','ArtifactCol1005-Boots673','ArtifactCol1005-Boots678','ArtifactCol1005-Boots685','ArtifactCol1005-Boots697','ArtifactCol1005-Boots702','ArtifactCol1005-Boots708','ArtifactCol1005-Boots713','ArtifactCol1005-Boots717','ArtifactCol1005-Boots721','ArtifactCol1005-Boots729','ArtifactCol1005-Boots733','ArtifactCol1005-Boots737','ArtifactCol1005-Boots741','ArtifactCol1005-Boots745','ArtifactCol1005-Boots749','ArtifactCol1005-Boots753','ArtifactCol1005-Boots773','ArtifactCol1006-Comm-Badge674','ArtifactCol1006-Comm-Badge679','ArtifactCol1006-Comm-Badge686','ArtifactCol1006-Comm-Badge698','ArtifactCol1006-Comm-Badge703','ArtifactCol1006-Comm-Badge709','ArtifactCol1006-Comm-Badge714','ArtifactCol1006-Comm-Badge718','ArtifactCol1006-Comm-Badge722','ArtifactCol1006-Comm-Badge730','ArtifactCol1006-Comm-Badge734','ArtifactCol1006-Comm-Badge738','ArtifactCol1006-Comm-Badge742','ArtifactCol1006-Comm-Badge746','ArtifactCol1006-Comm-Badge750','ArtifactCol1006-Comm-Badge754','ArtifactCol1006-Comm-Badge774','ArtifactCol1007-Sash725','ArtifactCol1008-VISOR688']).
objects('FluidReservoir',['ArtifactCol1020-Tea690','ArtifactCol1021-Synthehol691','ArtifactCol1022-Ferengi-Ale692','ArtifactCol1023-Romulan-Whisky693','ArtifactCol1024-Lemonade-Prune-Juice694','ArtifactCol1025-Vulcan-Beer695']).
objects('BPVLocation',['Area1000','Area1001','Area1002','Area1003','Area1004','Area1005','Area1006','Area1007','Area1008','Area1009','Area1010','Area1011','Area1012','Area1013','Area1014','Area1015','Area1016','Area1017','Area1018','Area1019','Area1020','Area1021','Area1022','Area1023','Area1024','Area1025','Area1026','Area1027','Area1028','Area1029','Area1030','Area1031','Area1032','Area1033','Area1034','Area1035','Area1036','Area1037','Area1038','Area1039','Area1040','Area1041','Area1042'
% ,'Area4075'
]).
objects(agent,['NpcCol1000-Geordi684','NpcCol1002-Worf720','NpcCol1003-Dr-Crusher677','NpcCol1004-Troi712','NpcCol1005-Riker707','NpcCol1006-Picard701','NpcCol1007-Guinan689','NpcCol1008-OBrien696','NpcCol1009-Wesley716','NpcCol1010-Livingston726','NpcCol1011-Spot727','NpcCol1012-Ensign728','NpcCol1012-Ensign732','NpcCol1012-Ensign736','NpcCol1012-Ensign740','NpcCol1012-Ensign744','NpcCol1012-Ensign748','NpcCol1012-Ensign752','NpcCol1013-Alexander671',vacuum(1),explorer(player1)]).

% % objects(areaPath,List):-findall(apath(A,B),pathBetween(A,B,C),List).
objects(areaPath,[apath('Area1000',"n"),apath('Area1001',"e"),apath('Area1002',"n"),apath('Area1002',"e"),apath('Area1002',"s"),apath('Area1002',"w"),apath('Area1003',"w"),apath('Area1004',"e"),apath('Area1005',"n"),apath('Area1005',"e"),apath('Area1005',"s"),apath('Area1005',"w"),apath('Area1006',"n"),apath('Area1006',"w"),apath('Area1007',"e"),apath('Area1008',"n"),apath('Area1008',"e"),apath('Area1008',"s"),apath('Area1008',"w"),apath('Area1009',"w"),apath('Area1010',"s"),apath('Area1010',"u"),apath('Area1011',"n"),apath('Area1011',"u"),apath('Area1011',"d"),apath('Area1012',"e"),apath('Area1013',"n"),apath('Area1013',"e"),apath('Area1013',"s"),apath('Area1013',"w"),apath('Area1014',"w"),apath('Area1015',"e"),apath('Area1016',"n"),apath('Area1016',"e"),apath('Area1016',"s"),apath('Area1016',"w"),apath('Area1017',"w"),apath('Area1018',"e"),apath('Area1019',"n"),apath('Area1019',"e"),apath('Area1019',"s"),apath('Area1019',"w"),apath('Area1020',"w"),apath('Area1021',"s"),apath('Area1022',"n"),apath('Area1022',"s"),apath('Area1023',"e"),apath('Area1024',"n"),apath('Area1024',"e"),apath('Area1024',"s"),apath('Area1024',"w"),apath('Area1025',"w"),apath('Area1026',"e"),apath('Area1027',"n"),apath('Area1027',"e"),apath('Area1027',"s"),apath('Area1027',"w"),apath('Area1028',"w"),apath('Area1029',"e"),apath('Area1030',"n"),apath('Area1030',"e"),apath('Area1030',"s"),apath('Area1030',"w"),apath('Area1031',"w"),apath('Area1032',"s"),apath('Area1032',"u"),apath('Area1032',"d"),apath('Area1033',"u"),apath('Area1033',"d"),apath('Area1034',"u"),apath('Area1034',"d"),apath('Area1035',"e"),apath('Area1036',"n"),apath('Area1036',"e"),apath('Area1036',"d"),apath('Area1037',"w"),apath('Area1038',"s"),apath('Area1038',"w"),apath('Area1039',"n"),apath('Area1039',"u"),apath('Area1040',"u"),apath('Area1040',"d"),apath('Area1041',"u"),apath('Area1041',"d"),apath('Area1042',"n"),apath('Area1042',"s")]).

objects(dir,["n","s","e","w","u","d"]).
/*********************** predcate defns ***********************************/




end_of_file.

/*********************** invariants ****************************************/

% LHS vars univ. quantified over primitive sorts
% RHS free vars are existentially quantified
/*

implied_invariant([loaded(P,V)], [at(V,L),at(P,L)]).

inconsistent_constraint([certified(P), not_insured(P)]).

% % atomic_invariants([]):-!.% % ,fail.
atomic_invariants([
      pathBetween('Area1000',"n",'Area1002'),
      pathBetween('Area1001',"e",'Area1002'),
      pathBetween('Area1002',"n",'Area1005'),
      pathBetween('Area1002',"e",'Area1003'),
      pathBetween('Area1002',"s",'Area1000'),
      pathBetween('Area1002',"w",'Area1001'),
      pathBetween('Area1003',"w",'Area1002'),
      pathBetween('Area1004',"e",'Area1005'),
      pathBetween('Area1005',"n",'Area1008'),
      pathBetween('Area1005',"e",'Area1006'),
      pathBetween('Area1005',"s",'Area1002'),
      pathBetween('Area1005',"w",'Area1004'),
      pathBetween('Area1006',"n",'Area1042'),
      pathBetween('Area1006',"w",'Area1005'),
      pathBetween('Area1007',"e",'Area1008'),
      pathBetween('Area1008',"n",'Area1010'),
      pathBetween('Area1008',"e",'Area1009'),
      pathBetween('Area1008',"s",'Area1005'),
      pathBetween('Area1008',"w",'Area1007'),
      pathBetween('Area1009',"w",'Area1008'),
      pathBetween('Area1010',"s",'Area1008'),
      pathBetween('Area1010',"u",'Area1011'),
      pathBetween('Area1011',"n",'Area1013'),
      pathBetween('Area1011',"u",'Area1032'),
      pathBetween('Area1011',"d",'Area1010'),
      pathBetween('Area1012',"e",'Area1013'),
      pathBetween('Area1013',"n",'Area1016'),
      pathBetween('Area1013',"e",'Area1014'),
      pathBetween('Area1013',"s",'Area1011'),
      pathBetween('Area1013',"w",'Area1012'),
      pathBetween('Area1014',"w",'Area1013'),
      pathBetween('Area1015',"e",'Area1016'),
      pathBetween('Area1016',"n",'Area1019'),
      pathBetween('Area1016',"e",'Area1017'),
      pathBetween('Area1016',"s",'Area1013'),
      pathBetween('Area1016',"w",'Area1015'),
      pathBetween('Area1017',"w",'Area1016'),
      pathBetween('Area1018',"e",'Area1019'),
      pathBetween('Area1019',"n",'Area1021'),
      pathBetween('Area1019',"e",'Area1020'),
      pathBetween('Area1019',"s",'Area1016'),
      pathBetween('Area1019',"w",'Area1018'),
      pathBetween('Area1020',"w",'Area1019'),
      pathBetween('Area1021',"s",'Area1019'),
      pathBetween('Area1022',"n",'Area1024'),
      pathBetween('Area1022',"s",'Area1039'),
      pathBetween('Area1023',"e",'Area1024'),
      pathBetween('Area1024',"n",'Area1027'),
      pathBetween('Area1024',"e",'Area1025'),
      pathBetween('Area1024',"s",'Area1022'),
      pathBetween('Area1024',"w",'Area1023'),
      pathBetween('Area1025',"w",'Area1024'),
      pathBetween('Area1026',"e",'Area1027'),
      pathBetween('Area1027',"n",'Area1030'),
      pathBetween('Area1027',"e",'Area1028'),
      pathBetween('Area1027',"s",'Area1024'),
      pathBetween('Area1027',"w",'Area1026'),
      pathBetween('Area1028',"w",'Area1027'),
      pathBetween('Area1029',"e",'Area1030'),
      pathBetween('Area1030',"n",'Area1032'),
      pathBetween('Area1030',"e",'Area1031'),
      pathBetween('Area1030',"s",'Area1027'),
      pathBetween('Area1030',"w",'Area1029'),
      pathBetween('Area1031',"w",'Area1030'),
      pathBetween('Area1032',"s",'Area1030'),
      pathBetween('Area1032',"u",'Area1033'),
      pathBetween('Area1032',"d",'Area1011'),
      pathBetween('Area1033',"u",'Area1034'),
      pathBetween('Area1033',"d",'Area1032'),
      pathBetween('Area1034',"u",'Area1036'),
      pathBetween('Area1034',"d",'Area1033'),
      pathBetween('Area1035',"e",'Area1038'),
      pathBetween('Area1036',"n",'Area1038'),
      pathBetween('Area1036',"e",'Area1037'),
      pathBetween('Area1036',"d",'Area1034'),
      pathBetween('Area1037',"w",'Area1036'),
      pathBetween('Area1038',"s",'Area1036'),
      pathBetween('Area1038',"w",'Area1035'),
      pathBetween('Area1039',"n",'Area1022'),
      pathBetween('Area1039',"u",'Area1040'),
      pathBetween('Area1040',"u",'Area1041'),
      pathBetween('Area1040',"d",'Area1039'),
      pathBetween('Area1041',"u",'Area1041'),
      pathBetween('Area1041',"d",'Area1040'),
      % pathBetween('Area1042',"n",'Area4075'),
      pathBetween('Area1042',"s",'Area1006'),

     % %  printAll(pathBetween(X,Y,Z),agentRoute(apath(X,Y),X,Z)).
      agentRoute(apath('Area1000',"n"),'Area1000','Area1002'),
      agentRoute(apath('Area1001',"e"),'Area1001','Area1002'),
      agentRoute(apath('Area1002',"n"),'Area1002','Area1005'),
      agentRoute(apath('Area1002',"e"),'Area1002','Area1003'),
      agentRoute(apath('Area1002',"s"),'Area1002','Area1000'),
      agentRoute(apath('Area1002',"w"),'Area1002','Area1001'),
      agentRoute(apath('Area1003',"w"),'Area1003','Area1002'),
      agentRoute(apath('Area1004',"e"),'Area1004','Area1005'),
      agentRoute(apath('Area1005',"n"),'Area1005','Area1008'),
      agentRoute(apath('Area1005',"e"),'Area1005','Area1006'),
      agentRoute(apath('Area1005',"s"),'Area1005','Area1002'),
      agentRoute(apath('Area1005',"w"),'Area1005','Area1004'),
      agentRoute(apath('Area1006',"n"),'Area1006','Area1042'),
      agentRoute(apath('Area1006',"w"),'Area1006','Area1005'),
      agentRoute(apath('Area1007',"e"),'Area1007','Area1008'),
      agentRoute(apath('Area1008',"n"),'Area1008','Area1010'),
      agentRoute(apath('Area1008',"e"),'Area1008','Area1009'),
      agentRoute(apath('Area1008',"s"),'Area1008','Area1005'),
      agentRoute(apath('Area1008',"w"),'Area1008','Area1007'),
      agentRoute(apath('Area1009',"w"),'Area1009','Area1008'),
      agentRoute(apath('Area1010',"s"),'Area1010','Area1008'),
      agentRoute(apath('Area1010',"u"),'Area1010','Area1011'),
      agentRoute(apath('Area1011',"n"),'Area1011','Area1013'),
      agentRoute(apath('Area1011',"u"),'Area1011','Area1032'),
      agentRoute(apath('Area1011',"d"),'Area1011','Area1010'),
      agentRoute(apath('Area1012',"e"),'Area1012','Area1013'),
      agentRoute(apath('Area1013',"n"),'Area1013','Area1016'),
      agentRoute(apath('Area1013',"e"),'Area1013','Area1014'),
      agentRoute(apath('Area1013',"s"),'Area1013','Area1011'),
      agentRoute(apath('Area1013',"w"),'Area1013','Area1012'),
      agentRoute(apath('Area1014',"w"),'Area1014','Area1013'),
      agentRoute(apath('Area1015',"e"),'Area1015','Area1016'),
      agentRoute(apath('Area1016',"n"),'Area1016','Area1019'),
      agentRoute(apath('Area1016',"e"),'Area1016','Area1017'),
      agentRoute(apath('Area1016',"s"),'Area1016','Area1013'),
      agentRoute(apath('Area1016',"w"),'Area1016','Area1015'),
      agentRoute(apath('Area1017',"w"),'Area1017','Area1016'),
      agentRoute(apath('Area1018',"e"),'Area1018','Area1019'),
      agentRoute(apath('Area1019',"n"),'Area1019','Area1021'),
      agentRoute(apath('Area1019',"e"),'Area1019','Area1020'),
      agentRoute(apath('Area1019',"s"),'Area1019','Area1016'),
      agentRoute(apath('Area1019',"w"),'Area1019','Area1018'),
      agentRoute(apath('Area1020',"w"),'Area1020','Area1019'),
      agentRoute(apath('Area1021',"s"),'Area1021','Area1019'),
      agentRoute(apath('Area1022',"n"),'Area1022','Area1024'),
      agentRoute(apath('Area1022',"s"),'Area1022','Area1039'),
      agentRoute(apath('Area1023',"e"),'Area1023','Area1024'),
      agentRoute(apath('Area1024',"n"),'Area1024','Area1027'),
      agentRoute(apath('Area1024',"e"),'Area1024','Area1025'),
      agentRoute(apath('Area1024',"s"),'Area1024','Area1022'),
      agentRoute(apath('Area1024',"w"),'Area1024','Area1023'),
      agentRoute(apath('Area1025',"w"),'Area1025','Area1024'),
      agentRoute(apath('Area1026',"e"),'Area1026','Area1027'),
      agentRoute(apath('Area1027',"n"),'Area1027','Area1030'),
      agentRoute(apath('Area1027',"e"),'Area1027','Area1028'),
      agentRoute(apath('Area1027',"s"),'Area1027','Area1024'),
      agentRoute(apath('Area1027',"w"),'Area1027','Area1026'),
      agentRoute(apath('Area1028',"w"),'Area1028','Area1027'),
      agentRoute(apath('Area1029',"e"),'Area1029','Area1030'),
      agentRoute(apath('Area1030',"n"),'Area1030','Area1032'),
      agentRoute(apath('Area1030',"e"),'Area1030','Area1031'),
      agentRoute(apath('Area1030',"s"),'Area1030','Area1027'),
      agentRoute(apath('Area1030',"w"),'Area1030','Area1029'),
      agentRoute(apath('Area1031',"w"),'Area1031','Area1030'),
      agentRoute(apath('Area1032',"s"),'Area1032','Area1030'),
      agentRoute(apath('Area1032',"u"),'Area1032','Area1033'),
      agentRoute(apath('Area1032',"d"),'Area1032','Area1011'),
      agentRoute(apath('Area1033',"u"),'Area1033','Area1034'),
      agentRoute(apath('Area1033',"d"),'Area1033','Area1032'),
      agentRoute(apath('Area1034',"u"),'Area1034','Area1036'),
      agentRoute(apath('Area1034',"d"),'Area1034','Area1033'),
      agentRoute(apath('Area1035',"e"),'Area1035','Area1038'),
      agentRoute(apath('Area1036',"n"),'Area1036','Area1038'),
      agentRoute(apath('Area1036',"e"),'Area1036','Area1037'),
      agentRoute(apath('Area1036',"d"),'Area1036','Area1034'),
      agentRoute(apath('Area1037',"w"),'Area1037','Area1036'),
      agentRoute(apath('Area1038',"s"),'Area1038','Area1036'),
      agentRoute(apath('Area1038',"w"),'Area1038','Area1035'),
      agentRoute(apath('Area1039',"n"),'Area1039','Area1022'),
      agentRoute(apath('Area1039',"u"),'Area1039','Area1040'),
      agentRoute(apath('Area1040',"u"),'Area1040','Area1041'),
      agentRoute(apath('Area1040',"d"),'Area1040','Area1039'),
      agentRoute(apath('Area1041',"u"),'Area1041','Area1041'),
      agentRoute(apath('Area1041',"d"),'Area1041','Area1040'),
      % agentRoute(apath('Area1042',"n"),'Area1042','Area4075'),
      agentRoute(apath('Area1042',"s"),'Area1042','Area1006')

     ]).
*/

/*********************** ss classes ****************************

substate_classes(physical_obj, P,
       [
        [at(P,L)]
       ]).

substate_classes(agent, P,
       [
        [inRegion(P,L)]
       ]).

substate_classes(railv, V,
       [
        [unattached(V)] , [attached(V,V1)]
       ]).
substate_classes(vehicle, T,
       [
        [moveable(T),available(T)], 
        [moveable(T),busy(T)] 
       ]).

substate_classes(package, P,
       [
        [uncertified(P)],
        [waiting(P),certified(P)],
        [loaded(P,V),certified(P)],
        [delivered(P)]
      ]) .

************/
/*********************** operators *****

% method(name,precons,transitions,statics,temps,decomposition)
% operator(name,prevail,transitions,cond_transitions)


method(
 % 1. name
      transport(P,O,D),
 % 2. dynamic constraints
      [ ],
 % 3. list of necessary substate changes
      [ sc(package, P, [at(P,O), is_of_sort(P,package)] => 
                       [at(P,D), delivered(P)]) ],
 % 4. static constraints
      [ ne(O,D),in_region(O,R),in_region(D,R)
       % list of static predicates that must be instantiated to
       % be true. Static preds may also appear in 2. and 3. if
       % its clearer that way 
       ],

 % 5.  temporal  constraints
       % list of static predicates before(N1,N2)
      [before(1,2),before(2,3)],
 % 6. decomposition
      [ achieve( ss(package, P,[waiting(P),certified(P)]) ), carry_direct(P,O,D), deliver(P,D)]
 ).

method(
      transport(P,O,D),
      [ ],
      [ sc(package, P, [at(P,O), is_of_sort(P,package)] =>
                       [at(P,D), delivered(P)]) ],
      [ ne(O,D),ne(R1,R2),is_of_sort(AV,aircraft),
        in_region(O,R1),in_region(D,R2),
        serves_region(A1,R1),serves_region(A2,R2)
       ],
      [before(1,2),before(2,3),before(3,4),before(4,5)],
      [ achieve( ss(package, P,[waiting(P),certified(P)]) ), 
        carry_direct(P,O,A1), carry_via_ap(A1,A2,P), 
        carry_direct(P,A2,D), deliver(P,D)]
 ).

method(
   carry_via_ap(O,D,P),
  [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] =>
                       [at(P,D),waiting(P),certified(P)]) ],
      [ ne(O,D), 
%                ne(O,O1), is_of_sort(O1,airport),
        is_of_sort(O,airport), is_of_sort(D,airport),
        is_of_sort(P,package), is_of_sort(V,aircraft)],
 [before(1,3), before(2,3),before(3,4), before(4,5)],
  [
%       fly(V,O1,O),
       commission(V),
       achieve(ss(aircraft,V,[at(V,O)])),
       load_package(P,V,O),
       fly(V,O,D),
       unload_package(P,V,D)
       ]
).

% carry in one city
method(  
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,truck),
       in_city(O,CY),
       in_city(D,CY)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5)
       ],
 [   %  achieve(ss(truck,V,[moveable(V), busy(V)])),
       commission(V),
       achieve(ss(truck,V,[at(V,O)])),
       load_package(P,V,O),
       move(V,O,D,local_roads),
       unload_package(P,V,D)
       ]
).

% carry between two cities by traincar
method(
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,traincar),
       is_of_sort(Train,train),
       connects(R,O,D),
       rv_compatible(R,traincar),
       route_available(R)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5),
       before(5,6),before(6,7),before(7,8) ],
 [     commission(V),
       achieve(ss(train,Train,[at(Train,O),attached(Train,V)])),
       load_package(P,V,O),
       pull_traincar(Train,V,O,D,R),
       detach_traincar(Train,V,D),
       unload_package(P,V,D)
       ]
).

% carry between two cities by truck
method(
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,truck),
       in_city(O,CY),
       in_city(D,CY1),
       ne(CY,CY1),
       connects(R,CY,CY1),
       is_of_sort(R,road_route),
       route_available(R)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5) ],
 [   %  achieve(ss(truck,V,[moveable(V), busy(V)])),
       commission(V),
       achieve(ss(truck,V,[at(V,O)])),
       load_package(P,V,O),
       move(V,O,D,R),
       unload_package(P,V,D)
       ]
).

method(
  move_traincar(V, O, L, R2),
 [ ],
         [sc(traincar,V,[at(V,O) ]
            =>[at(V,L)] )],
 [is_of_sort(V,traincar),
       connects(R2,O,L),
       is_of_sort(R2,rail_route),
       is_of_sort(Train,train) ],
 [before(1,2), before(2,3), before(3,4),before(4,5) ],
 [   achieve(ss(train,Train,[at(Train,O)])),
          attach_traincar(Train,V,O),
          pull_traincar(Train,V,O,L,R2),
          detach_traincar(Train,V,L) ]
).
***********************************/
/* getting 
 operator( pay_fees(P),
      [],
      [sc(package,P,[uncertified(P)]
      =>[waiting(P),certified(P)])],
      [ ]).

operator(fly(A,D1,D2),
      [ ],
      [sc(aircraft,A,[at(A,D1)]
            =>[at(A,D2)] )],
         [sc(package,X,[loaded(X,A),certified(X),at(X,D1)]
            => [loaded(X,A),certified(X),at(X,D2)])  ]
).


%move truck
operator( move(V, O, L, R), 
        [ ],
         [sc(truck,V,[at(V,O),
             is_of_sort(R,road_route),
             moveable(V),
             in_city(O,City),
             in_city(L,City1),
             ne(City,City1),
             connects(R,City,City1)]
            =>[at(V,L)] )],
         [sc(package,X,[loaded(X,V),certified(X),at(X,O)] 
            => [loaded(X,V),certified(X),at(X,L)])  ]
).

%move truck inside city
operator( move(V, O, L, local_roads), 
         [],
         [sc(truck,V,[at(V,O),
             moveable(V),
             in_city(O,City),
             in_city(L,City)]
            =>[at(V,L)]  )],
         [ sc(package,X,[loaded(X,V),certified(X),at(X,O)] 
            =>[loaded(X,V),certified(X),at(X,L)])   ]
).

%move traincar
operator( pull_traincar(Train,V1, O, L, Rt), 
         [  ],
         [ sc(train,Train,[at(Train,O),
             attached(Train,V1),
             moveable(Train),
             connects(Rt,O,L),
             is_of_sort(Rt,rail_route)]
            =>[at(Train,L),attached(Train,V1)] ),
           sc(traincar,V1,[at(V1,O),attached(V1,Train)]
            =>[at(V1,L),attached(V1,Train)]) ],
         [sc(package,P,[loaded(P,V1),certified(P),at(P,O)]
            =>[loaded(P,V1),certified(P),at(P,L)]) ]
).

operator( move_train(V, O, L, Rt),
         [ ],
          [sc(train,V,[at(V,O),unattached(V),
             moveable(V),available(V),
             connects(Rt,O,L),
             is_of_sort(Rt,rail_route)]
            =>[at(V,L),unattached(V),moveable(V),available(V)] )],
       [ ]
).

operator(attach_traincar(Train,V,O),
     [  ],
     [sc(train, Train, [at(Train,O),moveable(Train),available(Train),unattached(Train)]
        =>[at(Train,O),attached(Train,V),moveable(Train),busy(Train)] ),
     sc(traincar, V, [at(V,O),unattached(V)]
        =>[at(V,O),attached(V,Train)] ) ],
     [ ]
).

operator(detach_traincar(Train,V,O),
     [ ],
     [sc(train, Train, [attached(Train,V),moveable(Train),busy(Train)]
        =>[unattached(Train),moveable(Train),available(Train)] ),
     sc(traincar, V, [attached(V,Train)]
        =>[unattached(V)] ) ],
     [ ]
).

operator(commission(V),
      [ ],
      [sc(vehicle, V,[moveable(V),available(V)] =>[moveable(V), busy(V)])],
      [ ]).

         

operator( load_package(P,V,L),
   [ss(vehicle,V, [at(V,L),moveable(V),busy(V)])],
   [sc(package, P, [at(P,L),waiting(P),certified(P)]=>
      [at(P,L),loaded(P,V),certified(P)])],
   []
).

operator( unload_package(P,V,L),
 [],
 [sc(package, P, [at(P,L),loaded(P,V),certified(P)]=>[at(P,L),waiting(P),certified(P)]
),
 sc(vehicle,V, [at(V,L), moveable(V), busy(V)] => [at(V,L),moveable(V),available(V)])
],
 []
 ).


operator( deliver(P,L),
        [],
        [sc(package, P, [at(P,L),waiting(P),certified(P)]=>
          [at(P,L),delivered(P)] )],
        []
).

docs ready */


% TESTS

actorStartState(agent,X,[inRegion(X,Y)|List]):-agent(X),inRegion(X,Y),findall(possess(X,P),possess(X,P),List1),findall(wearing(X,P),wearing(X,P),List2),flatten([List1,List2],List).
initialStart([ 

/*
inRegion('Area1005-Object666','Area1005').
inRegion('Area1008-Object667','Area1008').
inRegion('Area1013-Object668','Area1013').
inRegion('Area1016-Object669','Area1016').
inRegion('Area1024-Object670','Area1024').
inRegion('ArtifactCol1010-Dilithium-Crystal756','Area1000').
inRegion('ArtifactCol1010-Dilithium-Crystal757','Area1000').
inRegion('ArtifactCol1010-Dilithium-Crystal758','Area1000').
inRegion('ArtifactCol1009-Tricorder759','Area1015').
inRegion('ArtifactCol1009-Tricorder760','Area1015').
inRegion('ArtifactCol1009-Tricorder761','Area1015').
*/
% %  printAll(actorStartState(agent,X,List),ss(agent,X,List)).
   ss(agent,'NpcCol1000-Geordi684',[inRegion('NpcCol1000-Geordi684','Area1000'),wearing('NpcCol1000-Geordi684','ArtifactCol1005-Boots685'),wearing('NpcCol1000-Geordi684','ArtifactCol1006-Comm-Badge686'),wearing('NpcCol1000-Geordi684','ArtifactCol1003-Gold-Uniform687'),wearing('NpcCol1000-Geordi684','ArtifactCol1008-VISOR688')]),
   ss(agent,vacuum(1),[inRegion(vacuum(1),'Area1010'),possess(vacuum(1),'ArtifactCol1000-Phaser676'),wearing(vacuum(1),'ArtifactCol1005-Boots673'),wearing(vacuum(1),'ArtifactCol1006-Comm-Badge674'),wearing(vacuum(1),'ArtifactCol1003-Gold-Uniform675')]),
   ss(agent,explorer(player1),[inRegion(explorer(player1),'Area1000'),possess(explorer(player1),'ArtifactCol1000-Phaser776'),wearing(explorer(player1),'ArtifactCol1005-Boots773'),wearing(explorer(player1),'ArtifactCol1006-Comm-Badge774'),wearing(explorer(player1),'ArtifactCol1003-Gold-Uniform775')]),
   ss(agent,'NpcCol1002-Worf720',[inRegion('NpcCol1002-Worf720','Area1025'),possess('NpcCol1002-Worf720','ArtifactCol1000-Phaser724'),wearing('NpcCol1002-Worf720','ArtifactCol1005-Boots721'),wearing('NpcCol1002-Worf720','ArtifactCol1006-Comm-Badge722'),wearing('NpcCol1002-Worf720','ArtifactCol1003-Gold-Uniform723'),wearing('NpcCol1002-Worf720','ArtifactCol1007-Sash725')]),
   ss(agent,'NpcCol1003-Dr-Crusher677',[inRegion('NpcCol1003-Dr-Crusher677','Area1015'),possess('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder681'),possess('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder682'),possess('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder683'),wearing('NpcCol1003-Dr-Crusher677','ArtifactCol1005-Boots678'),wearing('NpcCol1003-Dr-Crusher677','ArtifactCol1006-Comm-Badge679'),wearing('NpcCol1003-Dr-Crusher677','ArtifactCol1004-Blue-Uniform680')]),
   ss(agent,'NpcCol1004-Troi712',[inRegion('NpcCol1004-Troi712','Area1007'),wearing('NpcCol1004-Troi712','ArtifactCol1005-Boots713'),wearing('NpcCol1004-Troi712','ArtifactCol1006-Comm-Badge714'),wearing('NpcCol1004-Troi712','ArtifactCol1004-Blue-Uniform715')]),
   ss(agent,'NpcCol1005-Riker707',[inRegion('NpcCol1005-Riker707','Area1036'),possess('NpcCol1005-Riker707','ArtifactCol1012-Trombone711'),wearing('NpcCol1005-Riker707','ArtifactCol1005-Boots708'),wearing('NpcCol1005-Riker707','ArtifactCol1006-Comm-Badge709'),wearing('NpcCol1005-Riker707','ArtifactCol1002-Red-Uniform710')]),
   ss(agent,'NpcCol1006-Picard701',[inRegion('NpcCol1006-Picard701','Area1035'),possess('NpcCol1006-Picard701','ArtifactCol1001-5-Phaser-Rifle705'),possess('NpcCol1006-Picard701','ArtifactCol1011-5-Picards-Flute706'),wearing('NpcCol1006-Picard701','ArtifactCol1005-Boots702'),wearing('NpcCol1006-Picard701','ArtifactCol1006-Comm-Badge703'),wearing('NpcCol1006-Picard701','ArtifactCol1002-Red-Uniform704')]),
   ss(agent,'NpcCol1007-Guinan689',[inRegion('NpcCol1007-Guinan689','Area1021'),possess('NpcCol1007-Guinan689','ArtifactCol1020-Tea690'),possess('NpcCol1007-Guinan689','ArtifactCol1021-Synthehol691'),possess('NpcCol1007-Guinan689','ArtifactCol1022-Ferengi-Ale692'),possess('NpcCol1007-Guinan689','ArtifactCol1023-Romulan-Whisky693'),possess('NpcCol1007-Guinan689','ArtifactCol1024-Lemonade-Prune-Juice694'),possess('NpcCol1007-Guinan689','ArtifactCol1025-Vulcan-Beer695')]),
   ss(agent,'NpcCol1008-OBrien696',[inRegion('NpcCol1008-OBrien696','Area1006'),possess('NpcCol1008-OBrien696','ArtifactCol1000-Phaser700'),wearing('NpcCol1008-OBrien696','ArtifactCol1005-Boots697'),wearing('NpcCol1008-OBrien696','ArtifactCol1006-Comm-Badge698'),wearing('NpcCol1008-OBrien696','ArtifactCol1003-Gold-Uniform699')]),
   ss(agent,'NpcCol1009-Wesley716',[inRegion('NpcCol1009-Wesley716','Area1016'),wearing('NpcCol1009-Wesley716','ArtifactCol1005-Boots717'),wearing('NpcCol1009-Wesley716','ArtifactCol1006-Comm-Badge718'),wearing('NpcCol1009-Wesley716','ArtifactCol1002-Red-Uniform719')]),
   ss(agent,'NpcCol1010-Livingston726',[inRegion('NpcCol1010-Livingston726','Area1035')]),
   ss(agent,'NpcCol1011-Spot727',[inRegion('NpcCol1011-Spot727','Area1003')]),
   ss(agent,'NpcCol1012-Ensign728',[inRegion('NpcCol1012-Ensign728','Area1000'),wearing('NpcCol1012-Ensign728','ArtifactCol1005-Boots729'),wearing('NpcCol1012-Ensign728','ArtifactCol1006-Comm-Badge730'),wearing('NpcCol1012-Ensign728','ArtifactCol1003-Gold-Uniform731')]),
   ss(agent,'NpcCol1012-Ensign732',[inRegion('NpcCol1012-Ensign732','Area1004'),wearing('NpcCol1012-Ensign732','ArtifactCol1005-Boots733'),wearing('NpcCol1012-Ensign732','ArtifactCol1006-Comm-Badge734'),wearing('NpcCol1012-Ensign732','ArtifactCol1003-Gold-Uniform735')]),
   ss(agent,'NpcCol1012-Ensign736',[inRegion('NpcCol1012-Ensign736','Area1011'),wearing('NpcCol1012-Ensign736','ArtifactCol1005-Boots737'),wearing('NpcCol1012-Ensign736','ArtifactCol1006-Comm-Badge738'),wearing('NpcCol1012-Ensign736','ArtifactCol1002-Red-Uniform739')]),
   ss(agent,'NpcCol1012-Ensign740',[inRegion('NpcCol1012-Ensign740','Area1020'),wearing('NpcCol1012-Ensign740','ArtifactCol1005-Boots741'),wearing('NpcCol1012-Ensign740','ArtifactCol1006-Comm-Badge742'),wearing('NpcCol1012-Ensign740','ArtifactCol1002-Red-Uniform743')]),
   ss(agent,'NpcCol1012-Ensign744',[inRegion('NpcCol1012-Ensign744','Area1024'),wearing('NpcCol1012-Ensign744','ArtifactCol1005-Boots745'),wearing('NpcCol1012-Ensign744','ArtifactCol1006-Comm-Badge746'),wearing('NpcCol1012-Ensign744','ArtifactCol1004-Blue-Uniform747')]),
   ss(agent,'NpcCol1012-Ensign748',[inRegion('NpcCol1012-Ensign748','Area1022'),wearing('NpcCol1012-Ensign748','ArtifactCol1005-Boots749'),wearing('NpcCol1012-Ensign748','ArtifactCol1006-Comm-Badge750'),wearing('NpcCol1012-Ensign748','ArtifactCol1004-Blue-Uniform751')]),
   ss(agent,'NpcCol1012-Ensign752',[inRegion('NpcCol1012-Ensign752','Area1036'),wearing('NpcCol1012-Ensign752','ArtifactCol1005-Boots753'),wearing('NpcCol1012-Ensign752','ArtifactCol1006-Comm-Badge754'),wearing('NpcCol1012-Ensign752','ArtifactCol1004-Blue-Uniform755')]),
   ss(agent,'NpcCol1013-Alexander671',[inRegion('NpcCol1013-Alexander671','Area1025')])
   ]).


% 


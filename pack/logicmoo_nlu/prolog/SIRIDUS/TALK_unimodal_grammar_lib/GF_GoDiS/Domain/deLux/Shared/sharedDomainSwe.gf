--# -path=.:../:../../:../Shared/:../../../Resource/Home/:../../../Core:../../../Core/Shared/:../../../Core/System

concrete sharedDomainSwe of sharedDomain = sharedCoreSwe, DBSwe ** {

lin
	-- ANSWERS

	answerLampOn lamp = {s = lamp.s};
	answerLampOff lamp = {s = lamp.s};
	answerLocation loc = {s = loc.s};
	
	questionWhichLamp lamp = {s = lamp.s};
	questionLocation loc = {s = loc.s};

	
-- LEXICON

pattern

	turnOn = "t�nda";
	turnOn_alone = ["t�nda lampan"];

	turnOff = "sl�cka";
	turnOff_alone = ["sl�cka lampan"];

	turnOnThis = "t�nda";
	turnOffThis = "sl�cka";

	dimmerUp = ["dimma upp"];
	dimmerDown = ["dimma ner"];

	--askLamp = ["har jag en"];
	--askLocation = ["finns det ett"];

	askStatusLamp = ["vilka lampor �r t�nda"];

}









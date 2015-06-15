--# -path=.:../:../../:../Shared/:../../../Resource/Home/:../../../Core:../../../Core/Shared/:../../../Core/System

abstract sharedDomain = sharedCore, DB ** {

fun

-- ANSWERS


	-- Ett alternativ �r att g�ra en Task f�r varje action som 
	-- finns i dom�nen. Allts� En erase Task, en play Task och en add Task
	-- p� det sattet kan man gora funktioner som tar "nummer fem" och g�r en 
	-- "Object play" och en "Object erase" men inte en "Object add".

	-- Request Answers

	answerLampOn 		: Lamp 		-> Object onTask;
	answerLampOff		: Lamp 		-> Object offTask;

	answerLocation		: Location 	-> Object locateTask;

	-- Ask Answers
	questionWhichLamp	: Lamp		-> Object lampQuestion;
	questionLocation	: Location 	-> Object locQuestion;

-- LEXICON

	onTask	 		: Task;
	offTask			: Task;
	locateTask		: Task;
	lampQuestion 		: Task;
	locQuestion 		: Task;

	turnOn			: Action	onTask;
	turnOff			: Action 	offTask;

	turnOnThis		: SingleAction;
	turnOffThis		: SingleAction;

	dimmerUp		: SingleAction;
	dimmerDown		: SingleAction;

	--askLamp		: Ask			lampQuestion;
	--askLocation	: Ask			locQuestion;

	askStatusLamp	: SingleAsk;
}












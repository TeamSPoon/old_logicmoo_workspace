--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User


abstract sharedDomain = sharedCore, DB ** {

fun

-- ANSWERS

	makeStartTimeAnswer : Time -> Proposition addTask;
	makeEndTimeAnswer : Time -> Proposition addTask;
	makeTVStationAnswer : TVStation -> Proposition addTask;
	makeDayAnswer : Day -> Proposition addTask;
	makeWeekdayAnswer : Weekday -> Proposition addTask;
	

-- LEXICON

	addTask : Task;  

	addRecording : Action addTask; -- "l�gga till", "l�gga till en inspelning" 
	addRecording_alone : SingleAction;  -- "l�gga till en inspelning"

	removeAll : SingleAction;

	checkup : SingleAction;
	

}












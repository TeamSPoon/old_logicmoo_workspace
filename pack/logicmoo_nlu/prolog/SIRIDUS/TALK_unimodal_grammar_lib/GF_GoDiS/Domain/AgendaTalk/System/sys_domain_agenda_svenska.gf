--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

concrete sys_domain_agenda_svenska of  sys_domain_agenda = sharedDomainSwe, systemCoreSwe ** {

lin 

-- Special Proposition

	recordingDay channel day start_time end_time = {s = day.s ++ 
					"p�" ++ channel.s ++ "fr�n" ++
					start_time.s ++ "till" ++ 
					end_time.s}; 
	recordingWeekday channel weekday start_time end_time = {s = "p�" ++ weekday.s ++ 
					"p�" ++ channel.s ++ "fr�n" ++
					start_time.s ++ "till" ++ 
					end_time.s}; 
	

pattern
-- CONFIRMS!
	addedRecording = ["lade till en inspelning"];
	removedAll = ["raderade alla plannerade inspelningar"];

-- SYSTEMASK?

	whatDay = ["vilken dag vill du spela in p�"];
	whatStartTime = ["vilken tid ska inspeningen starta"];
	whatEndTime = ["n�r ska inspelningen sluta"];
	whatChannel = ["vilken kanal vill du spela in"];
}



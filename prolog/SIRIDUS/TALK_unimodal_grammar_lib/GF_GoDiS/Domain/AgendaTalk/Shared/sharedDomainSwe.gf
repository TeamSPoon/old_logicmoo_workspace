--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

concrete sharedDomainSwe of sharedDomain = sharedCoreSwe, DBSwe ** open SpecResSwe in{

lin
	-- ANSWERS


	makeStartTimeAnswer time = {s = "fr�n" ++ time.s};
	makeEndTimeAnswer time = {s = "till" ++ time.s};
	makeTVStationAnswer station = {s = "p�" ++ station.s};
	makeDayAnswer day = {s = day.s};
	makeWeekdayAnswer weekday = {s = "p�" ++ weekday.s};

-- LEXICON

pattern
	addRecording = variants {["l�gga till en inspelning"]};
	addRecording_alone = variants {["l�gga till en inspelning"]};

	removeAll = ["ta bort mina inspelningar"];

	checkup = ["vilka inspelningar har jag"];	

}
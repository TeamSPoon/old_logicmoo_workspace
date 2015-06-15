concrete systemCoreSwe of systemCore = sharedCoreSwe ** {

--flags lexer=codelit ; unlexer=codelit ; startcat=DMoveList ;
--# -path=.:../:../Shared
flags conversion=finite;

lin

-- Greet
	makeGreetMove gre = {s = gre.s ++ "!"};

-- Quit
	makeQuitMove qui = {s = qui.s ++ "!"};

-- Answer
	makeAnswer _ ans = {s = ans.s};
	makeNegAnswer _ ans = {s = "inte" ++ ans.s};
	makeAnswerMove _ sha = {s = sha.s ++ "."};
	makeNegAnswerMove  _ sha = {s = sha.s ++ "."};

-- Ask
	singleAsk _ ask = {s = ask.s};
	--makeYesNoAsk _ action = {s = action.s};
	makeAsk ask = {s = ask.s ++ "."};

	
-- Request

	-- makeRequestMove moved to System and User respectively 
	-- because of differing linearizations

	makeRequest req = {s = req.s ++ "."};

	makeRequestMove req = {s =  req.s };
	-- makeNegRequestMove req = {s = req.s};



-- BASICS

	makeSofDMoves dms = {s = dms.s};
	makeDMPair dm1 dm2 = {s = dm1.s ++ dm2.s};
	makeDMList dm dms = {s = dm.s ++ dms.s};


-- Confirm

	--makeConfirm req = {s = ["lyckades med att"] ++ req.s ++ "."};
	makeConfirmMove conM = {s = conM.s ++ "."};

-- Report 
	makeReport req status= {s = status.s ++ req.s ++ "."};
	makeReportMove repM = {s = repM.s};


-- ICM

	--makeICMPerString perI string = {s = perI.s ++ string.s ++ "."};
	makeICMPerString perI = {s = perI.s};

	makeICMSem semI = {s = semI.s};
	makeICMSemMoveReq semI req = {s = semI.s ++ req.s ++ "."};
	makeICMSemMoveAnswer _ semI ans = {s = semI.s ++ "med" ++ ans.s ++ "."};
	makeICMSemMoveAsk _ semI ask = {s = semI.s ++ ask.s ++ "."};


	makeICMUnd undI = {s = undI.s};
	makeICMUndProp _ undI prop = {s = prop.s ++ "," ++ undI.s};

	makeICMAccIssue accI issue = {s = accI.s ++ issue.s};

	makeICMOther otherI = {s = otherI.s ++ "."};
	makeICMOtherIssue otherI issue = {s = otherI.s ++ issue.s ++ "."};
	makeICMOtherReq _ other req  = {s = other.s ++ req.s ++ "."};

-- Ask

	makeSystemAsk sysA = {s = sysA.s ++ "?"};
	makeAskSet set = {s = ["vill du"] ++ set.s};
	makeInstantiatedAsk _ insA = {s = ["vill du"] ++ insA.s};
	makeInstantiatedAskSingle insA = {s = ["vill du"] ++ insA.s};

-- Isues

	makePropIssue _ prop = {s = prop.s};

	makeIssueProp pi = {s = pi.s};
	makeIssueAsk ai = {s = ai.s};
	makeIssueList li = {s = li.s};

	--makePropIssue prop = {s = prop.s};
	makeAskIssue _ ask = {s = ask.s};

	-- makeListItemProp propI = {s = propI.s};
	makeListItemAsk askI = {s = "fr�ga" ++ "om" ++ askI.s};
	makeListItemAction _ action = {s = action.s};
	makeListItemSingleAction action = {s = action.s};

	makeListIssue prop1 prop2 = {s = prop1.s ++ "eller" ++ prop2.s};
	


pattern

	makeBasicAsk = ["vad kan jag g�ra f�r dig"];

	sem_pos = "okej";
	sem_pos_followed = [""];
	sem_neg = ["f�rl�t jag f�rst�r inte vad du menar."];
	-- sem_int = ["vad menar du med"];

	und_pos = "okej.";
	und_pos_followed = ".";
	und_neg = ["jag f�rst�r inte vad du menar."];
	und_int = ["�r det korrekt?"]; -- f�ljer yttrandet!!!

	per_pos = ["jag tyckte du sa"]; -- f�ljs av en str�ng

	reraise = ["s� ,"];
	reraise_followed = ["s� ,"];
	loadplan = ["l�t oss se ."];
	accomodate = ["jag antar att du menar"];
	reaccomodate =  ["g� tillbaks till"];


	-- ICMs
	-- Moved from General because of differing linearisations user and system.

	per_neg = variants {"urs�kta"; "f�rl�t" ; ["urs�kta jag h�rde inte vad du sa"]};
	per_int = variants { "urs�kta" ; ["vad sa du"] };

	acc_pos = "okej";
	acc_neg = ["ledsen jag kan inte svara p� fr�gor om"]; 
	acc_neg_alone = "ledsen";

	status_done = ["lyckades med att"];
	status_initiated = ["b�rjade med att"];
	status_pending = ["avvaktar med att"];
	status_failed = ["misslyckades med att"];


}



-- File name TimeSwe

--# -path=.:..:../Time:../Weekday
concrete TimeSwe of Time = open ResourceSwe in {

--Hours
lin

--hour0 = {s = refs (variants{["noll noll"];["noll"]}) "tolv" "ett"} ; 
hour0 = {s = refs (variants{["noll noll"];["noll"]})(variants{}) (variants{})} ; 
hour1 = {s = refs ["noll ett"] "ett" "tv�"} ; 
hour2 = {s = refs ["noll tv�"] "tv�" "tre"} ; 
hour3 = {s = refs ["noll tre"] "tre" "fyra"} ; 
hour4 = {s = refs ["noll fyra"] "fyra" "fem"} ; 
hour5 = {s = refs ["noll fem"] "fem" "sex"} ; 
hour6 = {s = refs ["noll sex"] "sex" "sju"} ; 
hour7 = {s = refs ["noll sju"] "sju" "�tta"} ; 
hour8 = {s = refs ["noll �tta"] "�tta" "nio"} ; 
hour9 = {s = refs ["noll nio"] "nio" "tio"} ; 
hour10 = {s = refs "tio" "tio" "elva"} ; 
hour11 = {s = refs "elva""elva" "tolv"} ; 
hour12 = {s = refs "tolv" "tolv" "ett" } ;
 
hour13 = {s = refs "tretton" "ett" "tv�" };
hour14 = {s = refs "fjorton" "tv�" "tre" };
hour15 = {s = refs "femton" "tre" "fyra" };
hour16 = {s = refs "sexton" "fyra" "fem" };
hour17 = {s = refs "sjutton" "fem" "sex" };
hour18 = {s = refs "arton" "sex" "sju" };
hour19 = {s = refs "nitton" "sju" "�tta" } ;
hour20 = {s = refs "tjugo" "�tta" "nio" } ;
hour21 = {s = refs (variants{["tju ett"];["tjugo ett"]}) "nio" "tio" } ;
hour22 = {s = refs (variants{["tju tv�"];["tjugo tv�"]}) "tio" "elva" } ;
hour23 = {s = refs (variants{["tju tre"];["tjugo tre"]}) "elva" "tolv" };

--Minutes
--0-9
--minute0 = {s = mins (variants{["noll noll"]}) (variants{}) (variants{}) (variants{[""]})} ;
minute0 = {s = mins ["noll noll"] (variants{[""]}) (variants{})} ;
minute1 = {s = mins ["noll ett"] (variants {["ett �ver"] ; ["en minut �ver"]}) (variants{}) } ;
minute2 = {s = mins ["noll tv�"] (variants {["tv� �ver"] ; ["tv� minuter �ver"]}) (variants{})} ;
minute3 = {s = mins ["noll tre"] (variants { ["tre �ver"] ; ["tre minuter �ver"]}) (variants{})} ;
minute4 = {s = mins ["noll fyra"] (variants { ["fyra �ver"] ; ["fyra minuter �ver"]}) (variants{})} ;
minute5 = {s = mins ["noll fem"] (variants { ["fem �ver"] ; ["fem minuter �ver"]}) (variants{})} ;
minute6 = {s = mins ["noll sex"] (variants { ["sex �ver"] ; ["sex minuter �ver"]}) (variants{})} ;
minute7 = {s = mins ["noll sju"] (variants { ["sju �ver"] ; ["sju minuter �ver"]}) (variants{})} ;
minute8 = {s = mins ["noll �tta"] (variants { ["�tta �ver"] ; ["�tta minuter �ver"]}) (variants{})} ;
minute9 = {s = mins ["noll nio"] (variants { ["nio �ver"] ; ["nio minuter �ver"]}) (variants{})} ;

--10-19
minute10 = {s = mins ["tio"] (variants { ["tio �ver"] ; ["tio minuter �ver"]}) (variants{})} ;
minute11 = {s = mins ["elva"] (variants { ["elva �ver"] ; ["elva minuter �ver"]}) (variants{})} ;
minute12 = {s = mins ["tolv"] (variants { ["tolv �ver"] ; ["tolv minuter �ver"]}) (variants{})} ;
minute13 = {s = mins ["tretton"] (variants { ["tretton �ver"] ; ["tretton minuter �ver"]}) (variants{})} ;
minute14 = {s = mins ["fjorton"] (variants { ["fjorton �ver"] ; ["fjorton minuter �ver"]}) (variants{})} ;
minute15 = {s = mins ["femton"] (variants { ["femton �ver"] ; ["femton minuter �ver"] ; ["kvart �ver"]}) (variants{})} ;
minute16 = {s = mins ["sexton"] (variants { ["sexton �ver"] ; ["sexton minuter �ver"]}) (variants{})} ;
minute17 = {s = mins ["sjutton"] (variants { ["sjutton �ver"] ; ["sjutton minuter �ver"]}) (variants{})} ;
minute18 = {s = mins ["arton"] (variants { ["arton �ver"] ; ["arton minuter �ver"]}) (variants{})} ;
minute19 = {s = mins ["nitton"] (variants { ["nitton �ver"] ; ["nitton minuter �ver"]}) (variants{})} ;

--20-29
minute20 = {s = mins ["tjugo"] (variants { ["tjugo �ver"] ; ["tjugo minuter �ver"]}) (variants {})} ;
minute21 = {s = mins (variants{["tju ett"];["tjugo ett"]}) (variants {["tju en �ver"];["tjugo en �ver"];["tju ett �ver"];["tjugo ett �ver"];["tju en minuter �ver"];["tjugo en minuter �ver"]}) (variants {}) } ;
minute22 = {s = mins (variants{["tju tv�"];["tjugo tv�"]}) (variants {["tju tv� �ver"];["tjugo tv� �ver"];["tju tv� minuter �ver"];["tjugo tv� minuter �ver"]}) (variants {}) } ;
minute23 = {s = mins (variants{["tju tre"];["tjugo tre"]}) (variants {["tju tre �ver"];["tjugo tre �ver"];["tju tre minuter �ver"];["tjugo tre minuter �ver"];}) (variants {}) } ;
--minute23 = {s = mins ["tjugo tre"] (variants { ["tjugo tre �ver"] ; ["tjugo tre minuter �ver"]}) (variants {}) } ;
minute24 = {s = mins (variants{["tju fyra"];["tjugo fyra"]}) (variants {["tju fyra �ver"];["tjugo fyra �ver"];["tju fyra minuter �ver"];["tjugo fyra minuter �ver"];}) (variants {["sex minuter i halv"];["sex i halv"]}) } ;
--minute24 = {s = mins ["tjugo fyra"] (variants { ["tjugo fyra �ver"];["tjugo fyra minuter �ver"]}) (variants {["sex minuter i halv"];["sex i halv"]})} ;
minute25 = {s = mins (variants{["tju fem"];["tjugo fem"]}) (variants {["tju fem �ver"];["tjugo fem �ver"];["tju fem minuter �ver"];["tjugo fem minuter �ver"];}) (variants {["fem minuter i halv"];["fem i halv"]}) } ;
--minute25 = {s = mins ["tjugo fem"] (variants { ["tjugo fem �ver"] ; ["tjugo fem minuter �ver"]}) (variants {["fem minuter i halv"];["fem i halv"]}) } ;
minute26 = {s = mins (variants{["tju sex"];["tjugo sex"]}) (variants {["tju sex �ver"];["tjugo sex �ver"];["tju sex minuter �ver"];["tjugo sex minuter �ver"];}) (variants {["fyra minuter i halv"];["fyra i halv"]}) } ;
--minute26 = {s = mins ["tjugo sex"] (variants { ["tjugo sex �ver"] ; ["tjugo sex minuter �ver"]}) (variants {["fyra minuter i halv"];["fyra i halv"]}) } ;
minute27 = {s = mins (variants{["tju sju"];["tjugo sju"]}) (variants {["tju sju �ver"];["tjugo sju �ver"];["tju sju minuter �ver"];["tjugo sju minuter �ver"];}) (variants {["tre minuter i halv"];["tre i halv"]}) } ;
--minute27 = {s = mins ["tjugo sju"] (variants { ["tjugo sju �ver"] ; ["tjugo sju minuter �ver"]}) (variants {["tre minuter i halv"];["tre i halv"]}) } ;
minute28 = {s = mins (variants{["tju �tta"];["tjugo �tta"]}) (variants {["tju �tta �ver"];["tjugo �tta �ver"];["tju �tta minuter �ver"];["tjugo �tta minuter �ver"];}) (variants {["tv� minuter i halv"];["tv� i halv"]}) } ;
--minute28 = {s = mins ["tjugo �tta"] (variants { ["tjugo �tta �ver"] ; ["tjugo �tta minuter �ver"]}) (variants {["tv� minuter i halv"];["tv� i halv"]}) } ;
minute29 = {s = mins (variants{["tju nio"];["tjugo nio"]}) (variants {["tju nio �ver"];["tjugo nio �ver"];["tju nio minuter �ver"];["tjugo nio minuter �ver"];}) (variants {["en minuter i halv"];["en i halv"]}) } ;
--minute29 = {s = mins ["tjugo nio"] (variants { ["tjugo nio �ver"] ; ["tjugo nio minuter �ver"]}) (variants {["en minut i halv"];["en i halv"]}) } ;

--30-39
minute30 = {s = mins ["trettio"] (variants { ["trettio minuter �ver"]}) ["halv"] } ;
minute31 = {s = mins ["trettio ett"] (variants { ["trettio en �ver"] ; ["trettio ett �ver"] ; ["trettio en minuter �ver"]}) (variants {["tjugo nio minuter i"];["tjugo nio i"];["en minut �ver halv"];["en �ver halv"]}) } ;
minute32 = {s = mins ["trettio tv�"] (variants { ["trettio tv� �ver"] ; ["trettio tv� minuter �ver"]}) (variants {["tjugo �tta minuter i"];["tjugo �tta i"];["tv� minuter �ver halv"];["tv� �ver halv"]}) } ;
minute33 = {s = mins ["trettio tre"] (variants { ["trettio tre �ver"] ; ["trettio tre minuter �ver"]}) (variants {["tjugo sju minuter i"];["tjugo sju i"];["tre minuter �ver halv"];["tre �ver halv"]}) } ;
minute34 = {s = mins ["trettio fyra"] (variants { ["trettio fyra �ver"] ; ["trettio fyra minuter �ver"]}) (variants {["tjugo sex minuter i"];["tjugo sex i"];["fyra minuter �ver halv"];["fyra �ver halv"]}) } ;
minute35 = {s = mins ["trettio fem"] (variants { ["trettio fem �ver"] ; ["trettio fem minuter �ver"]}) (variants {["tjugo fem minuter i"];["tjugo fem i"];["fem minuter �ver halv"]; ["fem �ver halv"]}) } ;
minute36 = {s = mins ["trettio sex"] (variants { ["trettio sex �ver"] ; ["trettio sex minuter �ver"]}) (variants {["tjugo fyra minuter i"];["tjugo fyra i"];["sex minuter �ver halv"];["sex �ver halv"]}) } ;
minute37 = {s = mins ["trettio sju"] (variants { ["trettio sju �ver"] ; ["trettio sju minuter �ver"]}) (variants {["tjugo tre minuter i"];["tjugo tre i"];["sju minuter �ver halv"];["sju �ver halv"]}) } ;
minute38 = {s = mins ["trettio �tta"] (variants { ["trettio �tta �ver"] ; ["trettio �tta minuter �ver"]}) (variants {["tjugo tv� minuter i"];["tjugo tv� i"]}) } ;
minute39 = {s = mins ["trettio nio"] (variants { ["trettio nio �ver"] ; ["trettio nio minuter �ver"]}) (variants {["tjugo en minuter i"];["tjugo en i"];["tjugo ett i"]}) } ;

--40-49
minute40 = {s = mins ["fyrtio"] (variants {}) (variants {["tjugo minuter i"];["tjugo i"]}) } ;
minute41 = {s = mins ["fyrtio ett"] (variants {}) (variants {["nitton minuter i"];["nitton i"]}) } ;
minute42 = {s = mins ["fyrtio tv�"] (variants {}) (variants {["arton minuter i"];["arton i"]}) } ;
minute43 = {s = mins ["fyrtio tre"] (variants {}) (variants {["sjutton minuter i"];["sjutton i"]}) } ;
minute44 = {s = mins ["fyrtio fyra"] (variants {}) (variants {["sexton minuter i"];["sexton i"]}) } ;
minute45 = {s = mins (variants {["fyrtio fem"];["tre kvart"]}) (variants {}) (variants {["femton minuter i"];["femton i"];["kvart i"]}) } ;
minute46 = {s = mins ["fyrtio sex"] (variants {}) (variants {["fjorton minuter i"];["fjorton i"]}) } ;
minute47 = {s = mins ["fyrtio sju"] (variants {}) (variants {["tretton minuter i"];["tretton i"]}) } ;
minute48 = {s = mins ["fyrtio �tta"] (variants {}) (variants {["tolv minuter i"];["tolv i"]}) } ;
minute49 = {s = mins ["fyrtio nio"] (variants {}) (variants {["elva minuter i"];["elva i"]}) } ;

--50-59
minute50 = {s = mins ["femtio"] (variants {}) (variants {["tio minuter i"];["tio i"]}) } ;
minute51 = {s = mins ["femtio ett"] (variants {}) (variants {["nio minuter i"];["nio i"]}) } ;
minute52 = {s = mins ["femtio tv�"] (variants {}) (variants {["�tta minuter i"];["�tta i"]}) } ;
minute53 = {s = mins ["femtio tre"] (variants {}) (variants {["sju minuter i"];["sju i"]}) } ;
minute54 = {s = mins ["femtio fyra"] (variants {}) (variants {["sex minuter i"];["sex i"]}) } ;
minute55 = {s = mins ["femtio fem"] (variants {}) (variants {["fem minuter i"];["fem i"]}) } ;
minute56 = {s = mins ["femtio sex"] (variants {}) (variants {["fyra minuter i"];["fyra i"]}) } ;
minute57 = {s = mins ["femtio sju"] (variants {}) (variants {["tre minuter i"];["tre i"]}) } ;
minute58 = {s = mins ["femtio �tta"] (variants {}) (variants {["tv� minuter i"];["tv� i"]}) } ;
minute59 = {s = mins ["femtio nio"] (variants {}) (variants {["en minut i"];["en i"];["ett i"]}) } ;

lincat Hour = {s : RefHour => Str} ;

lincat Minute = {s : MinMin => Str} ;

-- Time expressions
lin 
--timeDotty h m = {s = h.s ! ThisLex ++ m.s ! Dot };
--timeInformal h m = {s = variants { 
	--m.s ! Past ++ h.s ! ThisLex ; 
	--m.s ! To ++ h.s ! NextLex 
	--} 
       --};
--timeFormal h m = {s = h.s ! ThisFormal ++ m.s ! Form} ;

--klockan arton och tjugo
--arton och tjugo
--arton och
--sex och

time h m = {s = 
     variants { 
	h.s ! ThisFormal ++ m.s ! Form ;
	h.s ! ThisFormal ++ "och" ++ m.s ! Form ;
	m.s ! Past ++ h.s ! ThisLex ;
	m.s ! To ++ h.s ! NextLex;
	"klockan" ++ h.s ! ThisFormal ++ m.s ! Form ;
	--"klockan" ++ h.s ! ThisFormal ++ "och" ++ m.s ! Form ;
	"klockan" ++ m.s ! Past ++ h.s ! ThisLex ;
	"klockan" ++ m.s ! To ++ h.s ! NextLex
	} 
       };

}















--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/swedish:resource-1.0/scandinavian

concrete TramUserSwe of TramUser = GodisUserSwe, StopsSwe ** 
    open Prelude, GrammarSwe, GodisLangSwe, TramLexiconSwe, TramSystemSwe, CommonScand in {

------------------------------------------------------------------------
-- Predicates
 
lin

shortest_route 
    = variants{ askQS shortest_route_Q;
		ss (variants{["hitta"]; ["ge mig"]; ["fr�ga efter"]} ++ 
			variants{["en rutt"]; ["kortaste v�gen"]; ["en resv�g"]})};

shortest_route__dept x 
    = ss ( ["jag vill �ka fr�n"] ++ x.s!NPNom);

shortest_route__dest x 
    = ss ( ["jag vill �ka till"] ++ x.s!NPNom);

shortest_route__dept_dest x y 
    = ss ( variants{["jag vill �ka fr�n"] ++ x.s!NPNom ++ "till" ++ y.s!NPNom ; 
	["jag vill �ka till"] ++ y.s!NPNom ++ "fr�n" ++ x.s!NPNom} );

dest_stop x = ss( "till" ++ x.s!NPNom);	
dept_stop x = ss( "fr�n" ++ x.s!NPNom);

dept_dest_stop x y = ss(variants{"till" ++ y.s!NPNom ++ "fr�n" ++ x.s!NPNom; 
"fr�n" ++ x.s!NPNom ++ "till" ++ y.s!NPNom} );

stop_dest_stop x y = ss(x.s!NPNom ++ "till" ++ y.s!NPNom);
stop_dept_stop x y = ss(x.s!NPNom ++ "fr�n" ++ y.s!NPNom);

----------------------------------------------------------------------
-- Short answers
lin
stop   x = ansNP x;


----------------------------------------------------------------------
-- Actions

lin
top = reqVP top;

help = variants{ reqVP help;
		 ss ["hur g�r jag nu"] }; 



}


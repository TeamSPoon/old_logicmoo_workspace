--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/scandinavian:resource-1.0/swedish

concrete AgendaUserSwe of AgendaUser = GodisUserSwe, BookingSwe ** 
    open GodisLangSwe, Prelude, AgendaLexiconSwe, AgendaSystemSwe, CommonScand in {

oper 

E_at_T : NP -> NP -> Str
    = \e,t -> variants{e.s!NPNom ++ "klockan" ++ t.s!NPNom;
                       e.s!NPNom ++ "vid" ++ t.s!NPNom};

E_on_D : NP -> NP -> Str
    = \e,d -> variants{e.s!NPNom ++ "p�" ++ d.s!NPNom;
		      e.s!NPNom ++ d.s!NPNom};


-- short answers

lin

event x = ansNP x;
time x = ansNP x;
date x = ansNP x;
event_time x y = ss (E_at_T x y);


-- predicates
-- U: How is the schedule for monday?
how_is_schedule d = ss ( ["hur ser schemat ut p�"] ++ d.s!NPNom) ;

-- U: When is event?
when_is_event_day e d = ss ( ["n�r �r"] ++ e.s!NPNom ++ ["p�"] ++ d.s!NPNom );


-- U: What is the date today?

what_is_the_date = ss (["vad �r det f�r datum idag?"]);


-- U: Am I booked Date Time?

am_I_booked d t = ss ( ["�r jag bokad"] ++ d.s!NPNom ++ "klockan" ++ t.s!NPNom);


-- U: What day is the Event?

what_day_is_event e = ss ( ["n�r �r"] ++ e.s!NPNom);


-- U: What day is the Event at Time?

what_day_is_event_at_time e t = ss (["vilket datum �r"] ++ e.s!NPNom ++ ["klockan"] ++ t.s!NPNom);



-- actions

-- action add
lin

add = variants{ reqVP agenda_add;
 		req1x ["l�gga till"] (optStr ["en bokning"] ++ to_agenda) };


add__event y = ss (["l�gga till"] ++ y.s!NPNom ++ to_agenda);

add__event_time x y = req1x ["l�gga till"] (E_at_T x y ++ to_agenda);
add__event_date e d = req1x ["l�gga till"] (E_on_D e d ++ to_agenda);
add__event_time_date e t d = req1x ["l�gga till"] ((E_at_T e t) ++ "p�" ++ d.s!NPNom ++ to_agenda);


oper to_agenda : Str = optStr ["i agendan"];


-- action delete
lin

delete = reqVP agenda_delete;

-- action move

move_booking = reqVP agenda_move;

-- action change_time

change_time = variants { reqVP agenda_change_time;
                         reqVP agenda_change_time__booking};

-- action change_date

change_date = variants { reqVP agenda_change_date;
                         reqVP agenda_change_date__booking};

}





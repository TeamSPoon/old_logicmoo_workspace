--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Media/Swedish/:../../../Resource/Numbers/:../../../Core:../../../Core/Shared/

concrete sharedDomainSwe of sharedDomain = sharedCoreSwe, DBSwe ** open SpecResSwe in{


flags conversion=finite;


lin
	-- ANSWERS
	answerSongPlay song = {s = variants { 
					(                      song.s );
					( (itemForm ! Song) ++ song.s )
				}
		  };
	answerSongAdd song = {s = variants { 
					(                      song.s );
					( (itemForm ! Song) ++ song.s )
				}
		  };
	answerSongRemove song = {s = variants { 
					(                      song.s );
					( (itemForm ! Song) ++ song.s )
				}
		  };
	questionSong song = {s = variants { 
					(                      song.s );
					( (itemForm ! Song) ++ song.s )
				}
		  };



	answerArtistPlay artist = {s = variants {
				     (                        artist.s );
				     ( (itemForm ! Artist) ++ artist.s )
				    }
		      };
	answerArtistAdd artist = {s = variants {
				     (                        artist.s );
				     ( (itemForm ! Artist) ++ artist.s )
				    }
		      };
	answerArtistRemove artist = {s = variants {
				     (                        artist.s );
				     ( (itemForm ! Artist) ++ artist.s )
				    }
		      };
	questionArtist artist = { s = variants {
				     (                        artist.s );
				     ( (itemForm ! Artist) ++ artist.s )
				    }
		      };



	answerStationPlay station = {s = variants {
					(			station.s);
					( "stationen" ++ station.s)
						}
		      };
	answerStationAdd station = {s = variants {
					(			station.s);
					( "stationen" ++ station.s)
						}
		      };
	answerStationRemove station = {s = variants {
					(			station.s);
					( "stationen" ++ station.s)
						}
		      };


	
	-- LIST RELATED ANSWERS

	-- nummer fem
	-- fem
	answerNumberInListPlay numb = {s = variants {
					( (listForm ! Numeric) ++ numb.s ); 
					(                         numb.s )
				     }
			};

	answerNumberInListRemove numb = {s = variants {
					( (listForm ! Numeric) ++ numb.s ); 
					(                         numb.s )
				     }
			};


	-- den femte l�ten
	-- den femte
	answerOrderInListPlay ordNum = 
		{s = variants {
				("den" ++ ordNum.s ++ (itemForm ! Post));
				("den" ++ ordNum.s)
			      }
		};
	answerOrderInListRemove ordNum = 
		{s = variants {
				("den" ++ ordNum.s ++ (itemForm ! Post));
				("den" ++ ordNum.s)
			      }
		};


-- LEXICON

pattern
	
	play_spec = (variants {["spela"] ; ["starta"] ; ["h�ra"] ; ["lyssna p�"]});
	play_spec_alone = variants {["spela den h�r"] ; ["spela den"] ; ["spela en speciell"] ; ["spela en speciell l�t"]};
	play = (variants {["spela fr�n b�rjan"] ; ["spela"] ; ["starta"]});
	stop = (variants {["stoppa"] ; ["avbryta"]});
	pause = (variants {["pausa"] });
	resume = (variants {["�teruppta spelningen"] ; ["starta igen"]});

	next = "n�sta";
	previous = "f�reg�ende";

	raise_volume	= "h�ja" ++ variants { ("volymen") ; ("ljudet")};
	lower_volume	= "s�nka" ++ variants { ("volymen") ; ("ljudet")}; 

	
	fastforward	= ["spola fram�t"];
	rewind		= ["spola bak�t"];


	shift = variants{ ["�ndra balansen"] ; "skifta"};
	right = variants{"" ; "till"} ++ "h�ger";
	left = variants{"" ; "till"} ++ "v�nster";
	center = variants{"" ; "till"} ++ "mitten";

	show_list = ["visa listan"];

	add = ["l�gga till"];
	add_alone = variants {["l�gga till"]; ["l�gg till den h�r"] ; ["l�gg till den"]};
	remove = ["ta bort"];
	remove_alone = variants { ["ta bort"] ; ["ta bort den"] ; ["ta bort den h�r"] };

	remove_all = variants {["rensa listan"] ; ["ta bort allt"]};

	handle_list 		= ["�ndra i spellistan"];
	handle_player 		= ["prata med spelaren"];
	handle_stations 	= ["v�lja en radiostation"];



	-- FLYTTAT TILL userSpecificSwe.gf och systemSpecificSwe.gf pga 
	-- olika linearisering for system och anv�ndare.

	--askArtist = variants { "l�tar" ; 
	--	variants { variants {"vad" ; ["vilka l�tar"]} ++ ["har jag"] ; 
	--		["har jag n�gonting"]} ++ variants {"med" ; "av"}};


	--askSong = variants { "artister" ; (["vem har"] ++ variants {"skrivit"; "gjort"})};

	--askCurrent = ["vad heter"] ++ variants {["den h�r"] ; ["l�ten som spelas nu"]};

}









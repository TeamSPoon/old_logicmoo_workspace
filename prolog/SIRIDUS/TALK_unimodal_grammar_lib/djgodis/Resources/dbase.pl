/**********************************************

       Testing database functionalitys

       b�r anv�ndas i valid_parameter

**********************************************/

:-module( dbase, [ findAlbums/2,
		   findArtistsSong/2,
		   findArtistsAlbum/2,
		   findSongsArtist/2,
		   song_list/1,
		   song_list2/1,
		   song_by_artist/3,
		   group_in_db/1,
		   album_in_db/1,
		   station_in_db/1,
		   make_name/2,
		   break_list/2] ).

:- use_module( library(lists), [member/2,
				append/3] ).


%%% default_path( -DefaultPathToMusicLibrary )
%%default_path( '/home/andreas/music/' ).
default_path( 'C:\\D1.1-release\\musik\\svenska\\' ).

%%% findAlbum( +Artist, -AlbumsAtom )
findAlbums( Artist, Albums ):-
	(
	  setof(Album, NotUsed^Not2^post(Artist,Album,NotUsed,Not2), AlbumList ), %% albums == list of lists
	  findall(Album2, (post(best_of,Album2,List),member(_Song-Artist,List) ), AlbList2Tmp),
	  sort(AlbList2Tmp,AlbumList2)
	->
	  nice_atom(AlbumList,Albs1),
	  nice_atom(AlbumList2,Albs2),
	  (
	    Albs2 = ''
	  ->
	    Albums = Albs1
	  ;
	    concat_atom(Albs1,',',TmpAlbs),
	    concat_atom(TmpAlbs,Albs2,Albums)
	  )
	;
	  setof(Album2, (post(best_of,Album2,List),member(_Song-Artist,List) ), AlbumList2),
	  nice_atom(AlbumList2,Albums)
	;
	  Albums = ''
	).

%%% findArtistsSong( +Song, -ArtistsAtom )
findArtistsSong( Song, Artists ):-
	(
	  setof(Artist, (post(Artist,_,SongList,_),member(Song,SongList)), ArtistList ),
	  findall(Artist2, (post(best_of,_Album,List),member(Song-Artist2,List) ), ArtistList2Tmp),
	  sort(ArtistList2Tmp,ArtistList2)
	  ->
	  nice_atom(ArtistList,Arts1),
	  nice_atom(ArtistList2,Arts2),	  
	  (
	    Arts2 = ''
	  ->
	    Artists = Arts1
	  ;
	    concat_atom(Arts1,',',TmpArts),
	    concat_atom(TmpArts,Arts2,Artists)
	  )
	;
	  setof(Artist3, (post(best_of,_Album2,List),member(Song-Artist3,List) ), ArtistList3),
	  nice_atom(ArtistList3,Artists)
	;
	  Artists = ''
	).

%%% findArtistsAlbum( +Album, -ArtistsAtom )
findArtistsAlbum( Album, Artists ):-
	(
	  setof(Artist, N^NN^post(Artist,Album,N,NN), ArtistList )
	->
	  nice_atom(ArtistList,Artists)
	;
	  post(best_of,Album,_List),
	  Artists = best_of
	)
	;
	Artists = ''.

findSongsArtist( Artist, Songs ):-
	(
	  setof(Song, Not^N2^post(Artist,Not,Song,N2), SongList),
	  findall(Song2, (post(best_of,_Not,List),member(Song2-Artist,List) ), SongList2Tmp),
	  sort(SongList2Tmp,SongList2)
	->
	  nice_song_atom(SongList,Songs1),
	  nice_atom(SongList2,Songs2),
	  %% if Songs2 == ''
	  (
	    Songs2 = ''
	  ->
	    Songs = Songs1
	  ;
	      concat_atom(Songs1,',',TmpSongs),
	      concat_atom(TmpSongs,Songs2,Songs)
	  ),
	  !
	;
	  setof(Song2, (post(best_of,Not,List),member(Song2-Artist,List) ), SongList2),
	  nice_atom(SongList2,Songs),
	  !
	;
	  Songs = ''
	).

%%% findSongsArtist( Artist, Songs ):-
%%% 	(
%%% 	  setof(Song, Not^N2^post(Artist,Not,Song,N2), SongList)
%%% 	->
%%% 	  nice_song_atom(SongList,Songs),
%%% 	  !
%%% 	;
%%% 	  Songs = ''
%%% 	).

song_list( List ):-
	post(_,_,List,_).

song_list2( List ):-
	post(_,_,List).

%%% song_by_artist( +Song, +Artist, -Path )
song_by_artist(Song,Artist,Path):-
	(
	  post(Artist,_,SongList,_),
	  member(Song,SongList)
	;
	  post(best_of,_Album,List),
	  member(Song-Artist,List)
	),
	default_path(Path).

group_in_db( Group ):-
	post(Group,_,_,_).

album_in_db( Album ):-
	post(_,Album,_,_)
	;
	post(_,Album,_).

station_in_db( Station ):-
	post(Station,_)
	;
	post(_,Station).


%%% post( ?Artist, ?Album, ?SongList, ?Genre )
%%% the arguments will be lists of lists
%%% the local file names are like: people_are_strange.mp3

%%% actual songs on local disk
post( [covenant],         [sequencer],                 [ [stalker],[leviathan],[edge,of,dawn],[tension] ],          _genre ).
post( [depeche,mode],     [violator],                  [ [world,in,my,eyes],[halo],[policy,of,truth] ],             _genre ).
post( [flesh,field],      [viral,extinction],          [ [fallen,angel],[silicon,skies],[where,angels,go,to,die] ], _genre ).
post( [komputer],         [valentia],                  [ [valentina] ],                                             _genre ).
post( [madonna],          [the,immaculate,collection], [ [justify,my,love],[lucky,star],[open,your,heart] ], _genre ).
post( [mesh],             [in,this,place,forever],     [ [you,didnt,want,me],[trust,you] ],                  _genre ).
post( [morrissey],        [bona,drag],                 [ [november,spawned,a,monster],[suedehead] ],         _genre ).
post( [nitzer,ebb],       [that,total,age],            [ [warsaw,ghetto],[murderous],[let,beauty,loose] ],   _genre ).
post( [peps,persson],     [peps,bitar],                 [ [h�g,standard],[fr�mmande],[h�ll,ut] ],             _genre ).
post( [pet,shop,boys],    [please],                    [ [west,end,girls],[love,comes,quickly] ],            _genre ).
post( [spock],            [single],                    [ [romulan,ale] ],                                    _genre ).

%%% songs that will be added to local disc soon...
post( [dia,psalma],       [gryningstid],               [ [alla,�lskar,dig],[hon,f�r],[tro,r�tt,tro,fel],[den,som,spar],
							 [emelie],[kalla,sinnen],[grytfot],[gryningsvisa,i,d,moll],
							 [sol,�ver,oss] ],  _genre ).

post( [ebba,gr�n],        [1978,1982],                 [ [profit],[ung,och,s�nkt],[tyst,f�r,fan],[mona,tumbas,slim,club],
							 [vad,skall,du,bli],[h�ng,gud],[totalv�gra],[bev�pna,er],
							 [det,m�ste,vara,radion],[pervers,politiker],[staten,och,kapitalet],
							 [ung,och,k�t],[�ttahundra,grader],[mamma,pappa,barn],[mental,istid],
							 [flyktsoda],[uppg�ng,och,fall],[tittar,p�,tv],
							 [nu,sl�cks,tusen,m�nniskoliv] ],  _genre ).

post( [jumper],           [jumper],                    [ [tapetklister],[den,v�gen],[i,v�rt,kvarter],[n�r,hela,v�rlden,st�r,utanf�r],
							 [n�got,som,hon,saknar],[p�,andra,sidan,molnen],[kom,som,en,man],
							 [tro,att,du,vet],[v�gsk�l],[jag,undrar] ],  _genre ).

post( [nordman],          [nordman],                   [ [vandraren],[under,norrskenet],[�nnu,gl�der,solen],[vill,ha,mer],
							 [s�,syns,du,inte,mer],[f�rlist],[lagl�st,land],[och,regnet,f�ll],
							 [nu,lever,sommaren],[stormens,�ga],[om,hon,vill,det,sj�lv],
							 [str�mkarlen],[lockl�t],[i,midsommartid] ],         _genre ).

post( [nordman],          [ingenmansland],             [ [det,sista,du,ser],[be,mig],[p�,mossen],[i,nattens,sista,timma],
							 [se,mig,idag],[brudrovet],[vem,kan,sl�cka,elden],[fly,i,ro],
							 [som,livet,och,d�den],[fick,jag,leva,igen],[fr�mlingen] ], _genre ).

post( [nordman],          [h�r,och,nu],                [ [kalla,mig,d�re],[hj�lp,mig,att,leva],[sorg,min,�lskarinna],
							 [se,dig,sj�lv],[det,var,inte,h�r],[barockpolska],[h�stl�ven,dansar,idag],
							 [kom,nu,gubbar],[i,det,bl�],[str�mt],[300,�r],[�det,�r,ditt,verk] ], _genre ).

%% har l�t med samma namn som album
post( [roger,pontare],   [i,vargens,sp�r],             [ [flykting],[sanningens,krigare],[berg,av,is],[n�r,vindarna,viskar,ditt,namn],
							 [h�vdingens,b�n],[siaren],[i,vargens,sp�r],[vid,bergens,rand],
							 [norden,befolkas],[h�r,mitt,ord] ],  _genre ).

post( [roger,pontare],   [julens,s�nger],              [ [nu,t�ndas,tusen,juleljus],[ett,barn,�r,f�tt,p�,denna,dag],
							 [fransk,julvisa],[den,signade,dag],[stilla,natt] ],  _genre ).

post( [viba,femba],      [alla,talar,svenska],         [ [sm�,m�nniskor],[bejaka],[det,k�nns,igen],[litet,bo,vill,jag,s�tta],
							 [puss],[amors,pilar],[en,bit,l�k],[herman,g�r],[sl�ttervisa],
							 [en,m�nskensnatt,p�,slottsbacken],[glimmande,nymf] ],  _genre ).


%%% specialpost f�r samlingsalbum...
%%% post( ?X, ?Title, ?ListWithSong-Group)
post( best_of, [blandat],           [ [undantag]-[bo,kaspers,orkester],
				      [precis,som,du]-[irma],
				      [om,du,var,h�r]-[kent],
				      [kom,ih�g,mig]-[lars,winnerb�ck],
				      [vem,vet]-[lisa,ekdahl],
				      [det,finns]-[mauro,scocco],
				      [sarah]-[maruo,scocco],
				      [du,f�r,g�ra,som,du,vill]-[patrik,isaksson],
				      [vinden,har,v�nt]-[petter],
				      [lilla,f�gel,bl�]-[staffan,hellstrand]
				    ]).

post( best_of, [topp,hits],         [ [se,p�,mig]-[jan,johansen],
				      [det,vackraste]-[cecilia,vennersten],
				      [dina,f�rger,var,bl�]-[tommy,nilsson],
				      [tess]-[peter,lemarc],
				      [vad,du,ser,�r,vad,du,f�r]-[lisa,nilsson],
				      [g�r,ut,med,mig,sj�lv]-[mauro,scocco],
				      [n�r,v�garna,lockar]-[malin,julia] ] ).


post( best_of, [svenska,klassiker], [ [flickorna,p�,tv,tv�]-[gyllene,tider],
				      [solglas�gon]-[docent,d�d],
				      [�ttahundra,grader]-[ebba,gr�n],
				      [flickan,och,kr�kan]-[mikael,wiehe],
				      [diamanter]-[lustans,lakejer],
				      [blinkar,bl�]-[adolphson,och,falk],
				      [sommaren,�r,kort]-[tomas,ledin],
				      [en,del,av,mitt,hj�rta]-[tomas,ledin],
				      [�ppna,landskap]-[ulf,lundell],
				      [segla,p�,ett,moln]-[annelie,ryde],
				      [vintersaga]-[monica,t�rnell],
				      [du,ska,va,president]-[imperiet],
				      [vindarna]-[freda],
				      [under,ytan]-[uno,svenningsson],
				      [jag,blir,hellre,jagad,av,vargar]-[orup],
				      [efter,stormen]-[marie,fredriksson],
				      [h�ll,om,mig]-[peter,lemarc],
				      [vem,ska,jag,tro,p�]-[di,leva],
				      [k�rlekens,tunga]-[eldkvarn],
				      [vingar]-[mikael,rickfors],
				      [vara,v�nner]-[jakob,hellman],
				      [�ngeln,i,rummet]-[eva,dahlgren],
				      [vem,t�nder,stj�rnorna]-[eva,dahlgren],
				      [det,hon,vill,ha]-[christer,sandelin] ] ).

				    
%%% post( ?RadioStation, ?IP-no/address )
post( [digital,gunfire], 'http://129.16.159.166:8000' ).
post( [rant,radio],      'http://130.240.207.88:9090' ).
post( [chat,radio],      'http://chatradio.myftp.org:10010' ).


%%% From a list of lists with album names to an atom
%%% of all albums
%%%
%%% nice_atom( +AlbumList, -AlbumsAtom ).
nice_atom( [], '' ).
nice_atom( [AlbumNameList|Xs], AlbumsAtom ):-
	make_name(AlbumNameList,TempAtom),
	nice_atom(Xs,Temp2Atom),
	(
	  Temp2Atom = ''
	->
	  AlbumsAtom = TempAtom
	;
	  concat_atom(TempAtom,',',T2),
	  concat_atom(T2,Temp2Atom,AlbumsAtom)
	).


%%% only for seeking songs
%[[[stalker],[feedback],[figurehead]]]
nice_song_atom( [NiceAtom], SongsAtom ):- 
	nice_atom(NiceAtom,SongsAtom).
%%% om lista med listor av listor
%%% artist har gjort flera album
nice_song_atom( [SongList|Rest], SongsAtom ):-
	nice_song_atom([SongList],Tmp),
	nice_song_atom(Rest,Tmp2),
	concat_atom(Tmp,',',T),
	concat_atom(T,Tmp2,SongsAtom).

make_name(List,Atom):-
	make_name(List,[],Atom),
	!.
make_name([],_,'').
make_name([Last],_,Atom):-
	concat_atom('',Last,Atom).
make_name([First|Rest],Temp,Atom):-
	make_name(Rest,Temp,A2),
	concat_atom(First,'_',Atom2),
	concat_atom(Atom2,A2,Atom).


%%% From ONE long list with all albums to a broken
%%% down list of lists with album names
%%%
%%% CharList == list of charCodes from the atom
%%% AlbumList == list of albumnames
break_list(CharList,AlbumList):-
	break_list(CharList,[],AlbumList),
	!.

break_list([],X,X).
break_list(CharList,Temp,AlbumList):-
	kom([X]),
	append(FirstList,[X|Rest],CharList),
	name(FirstAtom,FirstList),
	atom2list(FirstAtom,FirstAtomList),
	append([FirstAtomList],Temp,T2),
	break_list(Rest,T2,AlbumList).

break_list(CharList,Temp,AlbumList):-
	name(CharAtom,CharList),
	atom2list(CharAtom,CharAtomList),
	append(Temp,[CharAtomList],AlbumList).

atom2list(Atom,AtomList):-
	atom2list(Atom,[],AtomList),
	!.

atom2list('',AtomList,AtomList).
atom2list(Atom,TempList,AtomList):-
	name(Atom,A_list),
%	append(FirstList,[32|Rest],A_list),
	append(FirstList,[95|Rest],A_list),%% 95 == _
	name(First,FirstList),
	append(TempList,[First],T2),
	name(Second,Rest),
	atom2list(Second,T2,AtomList).
atom2list(Atom,TempList,AtomList):-
	append(TempList,[Atom],AtomList).

chk_albs( [] ).
chk_albs( [Album|Xs] ):-
	album(Album),
	chk_albs(Xs).

kom(X):-
	name(',',X).

%%% instead of atom_concat
concat_atom( PsAtom1, PsAtom2, Atom):-
	name(PsAtom1,X),
	atom_chars(A1,X),
	name(PsAtom2,X2),
	atom_chars(A2,X2),
	atom_concat(A1,A2,Atom).
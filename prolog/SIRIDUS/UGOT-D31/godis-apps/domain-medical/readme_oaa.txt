Nu �r den fin och f�rdig. Samma funktionalitet som test-programmet,
dock lite annorlunda utseende p� termerna (dock samma logiska
inneh�ll)

Liten OAA-lektion

F�r att testa:
0. se till att filen setup.pl finns i hembiblioteket

1. skapa l�mpligt bibliotek

1,5. prepenv CLASSPATH /users/ling/sl/oaa2.2.0/runtime/oaalib/oaa2.jar

2. packa upp diseasedb.zip d�r (den som kom med mail)

3. l�gg OAAWorkshopDatabase.java i samma bibliotek (den ska vara p� samma
niv� som t.ex. README-filen

4. > javac OAAWorkshopDatabase.java

5. starta facilitator
	> cd oaabibliotek/runtime
	> fac.sh

6.  starta debug-agenten
	> cd oaabibliotek/runtime
	> debug.sh

7 starta databasagenten
	cd l�mpligt_bibliotek
	java OAAWorkshopDatabase diseasedb.ser

Nu kan man st�lla fr�gor till databasen genom debug-agenten t.ex.
	oaa_Solve(disease_names(A),[])
	oaa_Solve(diagnose([not(symptom(headache)),symptom(cough)],A),[])
	...

solve-knappen ger ett oaa_Solve-predikatskal
agent info-knappen konstruerar en query som fr�gar facilitatorn vilka solvables
som finns deklarerade f�r olika agenter...
skicka query genom att trycka p� den lilla fyrkanten till h�ger.


Man kan ocks� testa genom att strunta i att starta debug-agenten och
konsultera oaag.pl-resursen och k�ra fr�n prolog...


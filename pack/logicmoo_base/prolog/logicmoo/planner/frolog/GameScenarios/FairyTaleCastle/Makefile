XMGGRAMMAR = SourceGrammarXMG/lu-small-grammar.mg
XMGLEXICON = SourceGrammarXMG/lexicon.lex
XMGMORPH = SourceGrammarXMG/morph.mph
XMGMACROS = SourceGrammarXMG/macros

TULIPALEXICON = ObjectGrammarTulipa/lexicon.xml
TULIPAMORPH = ObjectGrammarTulipa/morph.xml
TULIPAGRAMMAR = ObjectGrammarTulipa/lu-small-grammar.xml

GENILEXICON = ObjectGrammarGeni/lexicon.geni
GENIMORPH = ObjectGrammarGeni/morph.mph
GENIXMLGRAMMAR = ObjectGrammarGeni/lu-small-grammar-xmg.xml
GENIGRAMMAR = ObjectGrammarGeni/lu-small-grammar.geni

tulipa: 
	lexConverter -tL -i $(XMGLEXICON) -o $(TULIPALEXICON)
	lexConverter -tM -i $(XMGMORPH) -o $(TULIPAMORPH)
	MetaTAG $(XMGGRAMMAR)  --tulipa -o $(TULIPAGRAMMAR) 
	
geni:
	MetaTAG $(XMGGRAMMAR) --mac -s $(XMGMACROS)
	lexConverter -gL -i $(XMGLEXICON) -o $(GENILEXICON)
	MetaTAG $(XMGGRAMMAR) --xml -o $(GENIXMLGRAMMAR)
	./geniconvert --macros -f tagml -t geni -o $(GENIGRAMMAR) < $(GENIXMLGRAMMAR)
#	lexConverter -gM -i $(XMGMORPH) -o $(GENIMORPH) 



#The previous instructions generate the resources for Geni generator
#MetaTAG --mac is used to generate the semantics macros that are included in the lexicon that is inputed to lexConverter
#lexConverter is -gL is used to generate the lexicon in geni format
#MetaTAG --xml is used to generate the grammar in XMG standard XML format (Eric calls this format tagml but this name is missleanding because it's also used by Azim for a different format)
#geniconvert --macros is used to convert the grammar from XMG standard XML format to geni's grammar format (Eric calls the grammar "macros", this name is missleading, because the semantic macros for the lexicon also use this name but are different) 

#to run geni see the start-geni.sh

#to query geni first compile GeniSocket.java and then run it:
#benottil@nantois:~/geni$ javac GeniSocket.java
#benottil@nantois:~/geni$ java GeniSocket
#the query sent to geni can be modified in the code of GeniSocket.java
#the realized content is showed in stout





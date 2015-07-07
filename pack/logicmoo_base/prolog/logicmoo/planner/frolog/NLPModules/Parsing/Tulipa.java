package NLPModules.Parsing;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.w3c.dom.Document;
import de.tuebingen.anchoring.NameFactory;
import de.tuebingen.anchoring.TreeSelector;
import de.tuebingen.converter.GrammarConvertor;
import de.tuebingen.disambiguate.ComputeSubGrammar;
import de.tuebingen.disambiguate.PolarityAutomaton;
import de.tuebingen.disambiguate.PolarizedToken;
import de.tuebingen.forest.ExtractForest;
import de.tuebingen.forest.ProduceDOM;
import de.tuebingen.gui.DerivedTreeViewer;
import de.tuebingen.gui.ParseTreeCollection; //(LB 21-10-2008)
import de.tuebingen.io.RCGReader;
import de.tuebingen.io.TextRCGReader;
import de.tuebingen.io.XMLLemmaReader;
import de.tuebingen.io.XMLMorphReader;
import de.tuebingen.io.XMLRCGReader;
import de.tuebingen.io.XMLTTMCTAGReader;
import de.tuebingen.parser.RCGParser;
import de.tuebingen.parser.RCGParserBoullier2;
import de.tuebingen.parser.RCGParserEarley;
import de.tuebingen.parser.RCGParserEarleyNoTerm;
import de.tuebingen.rcg.RCG;
import de.tuebingen.rcg.RCGDOMbuilder;
import de.tuebingen.tag.*;
import de.tuebingen.tagger.ExternalTagger;
import de.tuebingen.tagger.TaggerException;
import de.tuebingen.tokenizer.BuiltinTokenizer;
import de.tuebingen.tokenizer.FileTokenizer;
import de.tuebingen.tokenizer.Tokenizer;
import de.tuebingen.tokenizer.TokenizerException;
import de.tuebingen.tokenizer.Word;
import de.tuebingen.tree.Grammar;
import de.tuebingen.util.XMLUtilities;

/**
 * A class for using Tulipa. With this class you can load a grammar 
 * and lexicons and then parse sentences using the loaded files. 
 *<p>
 *This class was copied and addapted from de.tuebingen.tree.ui.Interface
 *The project imports the tulipa_fat.jar in order to be able to import the 
 *libraries called de.tuebingen.*
 * @author benottil
 * 
 */
public class Tulipa {

	/**
	 * Takes a standard command line from Tulipa (@link http://sourcesup.cru.fr/tulipa/overview.html) 
	 * of the form "-g grammar.xml -l lexicon.xml -m morph.xml -a s -i"
	 * and loads the grammar and lexicons specified in the command line
	 * @param de.tuebingen.ui.CommandLineOptions
	 * @return Grammar
	 */
	public static Grammar initializeTulipa(de.tuebingen.ui.CommandLineOptions op)  {
		
		// initialization
		String gram = op.check("g") ? op.getVal("g") : "";
		String lem  = op.check("l") ? op.getVal("l") : "";
		String mo   = op.check("m") ? op.getVal("m") : "";
		Grammar g   = null;
	 
		// axiom's default value is "v"
		System.err.print("Axiom: ");
		String a = op.check("a") ? op.getVal("a") : "v";
		System.err.println(a);
		
		// we load the grammar (and lexicons)
		try {
			g = loadGrammar(op, gram, lem, mo);
		} catch (Exception e) {
			System.err.println("Error while loading grammar:");
			e.printStackTrace();
			System.err.println("Please check your command line options.");
			System.exit(1);
		}
		return g;
	}

	/**
	 * Takes a Tulipa command line and returns a data structure with its information
	 * @param cmdline
	 * @return de.tuebingen.ui.CommandLineOptions
	 */
	
	public static de.tuebingen.ui.CommandLineOptions processCommandLine(String[] cmdline) {
		//Command line processing
		de.tuebingen.ui.CommandLineOptions op = new de.tuebingen.ui.CommandLineOptions();
		// we declare the g option (for the grammar file)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "g", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the l option (for the lemma file)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "l", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the m option (for the morph file)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "m", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL b option (for batch processing on a corpus))
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "b", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL s option (sentence to parse)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "s", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL o option (output file (XML forest))
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "o", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL v option (verbose -- for debugging)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "v", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL w option (with derivation steps -- for grammar debugging)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "w", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL h option (prints help)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "h", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL d option (computing dependencies)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "d", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL a option (axiom (syntactic category))
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "a", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL k option (size of the LPA during TT-MCTAG to RCG conversion)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "k", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL i option (interactive mode)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "i", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL x option (XML output)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "x", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL z option (restricted tree to RCG conversion)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "z", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL r option (RCG mode)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "r", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL t option (tagger to use)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "t", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL e option (export RCG)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "e", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL f option (export XML forest)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "f", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL u option (utool deactivation)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "n", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL builtinTokenizer option (load builtin tokenizer, mode expected)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "builtinTokenizer", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL fileTokenizer option (load custom tokenizer from file)
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "fileTokenizer", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, true);
		// we declare the OPTIONAL q option (use earley algorithm with term transform) 
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "q1", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we declare the OPTIONAL q option (use earley algorithm) 
		op.add(de.tuebingen.ui.CommandLineOptions.Prefix.DASH, "q2", de.tuebingen.ui.CommandLineOptions.Separator.BLANK, false);
		// we compile the patterns for parsing the command line
		op.prepare();
		// we concatenate the command line		
		String line = "";
		for (int i=0; i< cmdline.length ; i++) {
			//System.out.println(cmdline[i]);
			String tmp = cmdline[i];
			tmp = tmp.replace(" ", "---");
			//System.out.println(tmp);
			line += "\""+tmp+"\" ";
		}
		// we parse the command line
		op.parse(line);
		return op;		
	}
	
	/**
	 * Loads a grammar
	 * @param op
	 * @param gram
	 * @param lem
	 * @param mo
	 * @return Grammar
	 * @throws Exception
	 */
	
	public static Grammar loadGrammar(de.tuebingen.ui.CommandLineOptions op, String gram, String lem, String mo) throws Exception  {
		Grammar g;
		File grammar = null;
		File lemmas  = null;
		File morphs  = null;
		
		if (op.check("g")) {
			grammar = new File(gram);
		}
		
		if (op.check("r")) {// RCG parsing
			long loadTime = System.nanoTime();
			String gext = grammar.getName();
			if (gext.length() > 4) {
				gext = gext.substring(gext.length() - 3);
			}
			RCGReader rcggr;
			if (gext.equals("xml")) {
				rcggr = new XMLRCGReader(grammar);
			} else {
				rcggr = new TextRCGReader(grammar);
			}
			g = rcggr.getRCG();
			long loadedTime = System.nanoTime() - loadTime;
			System.err.println("RCG grammar loading time: " + (loadedTime)/(Math.pow(10, 9))+" sec.");
		} else { // TAG/TT-MCTAG parsing
			XMLLemmaReader   xlr = null;
			XMLMorphReader   xmr = null;
			boolean needsAnchoring = false;

			long loadTime = System.nanoTime();
			// 1. Grammar processing
			XMLTTMCTAGReader xgr = new XMLTTMCTAGReader(grammar);
			g = new TTMCTAG(xgr.getTuples());
			needsAnchoring = xgr.needsAnchoring();
			g.setNeedsAnchoring(needsAnchoring);
			
			// 2. Lemmas processing
			if (needsAnchoring && !(op.check("l"))) {
				System.err.println("Anchoring needed, lexicon files not found, exit.");
				System.exit(1);
			} else if (op.check("l") && !(op.getVal("l").equals(""))) {
				lemmas = new File(lem);
				xlr = new XMLLemmaReader(lemmas);
				g.setLemmas(xlr.getLemmas());
			}
			// 3. Morphs processing
			if (needsAnchoring && !(op.check("m"))) {
				System.err.println("Anchoring needed, lexicon files not found, exit.");
				System.exit(1);
			} else if (op.check("m")) {
				morphs = new File(mo);
				xmr = new XMLMorphReader(morphs);
				g.setMorphEntries(xmr.getMorphs());
			}
			long loadedTime = System.nanoTime() - loadTime;
			System.err.println("Grammar and lexicons loading time: " + (loadedTime)/(Math.pow(10, 9))+" sec.");
			//System.err.println(g.toString());
		}		
		return g;
	}
	
	public static Tokenizer loadTokenizer(de.tuebingen.ui.CommandLineOptions op) throws TokenizerException, IOException {
		Tokenizer tok = null;
		if (op.check("fileTokenizer")) {
			tok = new FileTokenizer(op.getVal("fileTokenizer"));
		} else {
			String tokenizerMode = "";
			if (op.check("builtinTokenizer")) { tokenizerMode = op.getVal("builtinTokenizer"); }
			else { tokenizerMode = BuiltinTokenizer.GERMAN; }
			tok = new BuiltinTokenizer(tokenizerMode);
		}
		return tok;
	}
	
	/**
	 * Parse a sentence using the grammar and the options specified in the command line. 
	 * Throws two different exceptions, one when a word is unknown and other when there is
	 * a grammatical error.
	 * Returns all the possible semantic readings of the sentence. 
	 * 
	 * @param op
	 * @param g
	 * @param sentence
	 * @return ArrayList<String[]>
	 * @throws Exception
	 */
	
	public static ArrayList<List<SemLit>> parseSentence(de.tuebingen.ui.CommandLineOptions op, Grammar g, String sentence) throws Exception {
		
		ArrayList<List<SemLit>> readings = new ArrayList<List<SemLit>>();
		
	    long totalTime = 0;
		boolean verbose = op.check("v");
		boolean noUtool = op.check("n");
		boolean needsAnchoring = g.needsAnchoring();

		String axiom = "v"; // default axiom's value is v
		if (op.check("a")) {axiom = op.getVal("a");}
		
		List<String> slabels = new LinkedList<String>();
		
		// 4. Load the tokenizer
		Tokenizer tok = loadTokenizer(op);
		List<Word> tokens = null;
		tok.setSentence(sentence);
		tokens = tok.tokenize();
		if (verbose) {
			System.err.println("Tokenized sentence: " + tokens.toString());
		}
		List<String> toksentence = Tokenizer.tok2string(tokens);
		
		/* ******** external POS tagging ************/
		ExternalTagger tagger = new ExternalTagger();
		File taggerExec = op.check("t") ? new File(op.getVal("t")) : null; 
		tagger.setExec(taggerExec);
		tagger.setParams("");
		try {
			 tagger.doTagging(tokens);
		} catch (TaggerException e) {
			System.err.println(" ********** Tagging Exception *********");
			System.err.println(e.toString());
		}
		//ExternalTagger.printPosToken(tokens);
		/* ******************************************/
		
		// 5. Lexical selection and Anchoring
		TreeSelector ts = new TreeSelector(tokens, verbose);
		List<List<Tuple>> subgrammars = null;
		
		if (needsAnchoring) {
			long ancTime = System.nanoTime();
			// 5-a. According to the tokens, we retrieve the pertinent morph entries
			// 5-b. According to the morph entries, we instantiate the pertinent lemmas
			// 5-c. According to the instantiated lemmas, we instantiate the pertinent tuples
			// 6. Tree anchoring
			ts.retrieve(g.getMorphEntries(), g.getLemmas(), g.getGrammar(), slabels);
			//System.err.println(ts.toString());
			//System.err.println(ts.getTupleHash());
			
			long anchoredTime = System.nanoTime() - ancTime;
			System.err.println("Grammar anchoring time: " + (anchoredTime)/(Math.pow(10, 9))+" sec.");
			if (verbose) 
				System.err.println("Anchoring results:\n" + ts.toString());
			totalTime += anchoredTime; 
			//--------------------------------------------------------
			// before RCG conversion, we apply lexical disambiguation:
			//--------------------------------------------------------
			List<PolarizedToken> lptk = ts.getPtokens();
			if (verbose) {
				for(PolarizedToken ptk : lptk){
					System.err.println(ptk.toString());
				}
			}
			PolarityAutomaton pa = new PolarityAutomaton(toksentence, lptk, axiom, verbose, ts.getLexNodes(), ts.getCoancNodes());
			List<List<String>> tupleSets = pa.getPossibleTupleSets();
			subgrammars = ComputeSubGrammar.computeSubGrammar(verbose, tupleSets, ts.getTupleHash(), ts.getTreeHash()); 
			if (verbose) {
				System.err.println("Valid tuple sets:\n" + tupleSets);
				//System.err.println("\nCorresponding sub-grammars:\n" + subgrammars);
			}
			//--------------------------------------------------------
		} else {
			ts.store(g.getGrammar());
		}
		// Tree Selection results stored in specific variables to avoid 
		// keeping a pointer to the ts variable (and wasting memory)
		Map<String, TagTree> grammarDict = ts.getTreeHash();
		List<Tuple>       anchoredTuples = ts.getAnctuples();
		
		
		// 7. RCG conversion
		int limit   = op.check("z") ? Integer.parseInt(op.getVal("z")) : -1;
		int k_limit = op.check("k") ? Integer.parseInt(op.getVal("k")) : -1;

		RCG rcggrammar = null;
		long startTime = System.nanoTime();	
		if (subgrammars != null) { // i.e. we used lexical disambiguation 
			rcggrammar = new RCG();
			for(int sI = 0 ; sI < subgrammars.size() ; sI++) {
				List<Tuple> ltuples = subgrammars.get(sI);
				//System.err.println("Converting sub-grammar " + sI + "...");
				GrammarConvertor gc = new GrammarConvertor(ltuples, verbose, toksentence, grammarDict, !needsAnchoring, k_limit, limit);
				gc.buildAllClauses(axiom);
				rcggrammar.addGrammar(gc.getRcggrammar(), grammarDict);
			}
		} else {
			GrammarConvertor gc = new GrammarConvertor(anchoredTuples, verbose, toksentence, grammarDict, !needsAnchoring, k_limit, limit);
			gc.buildAllClauses(axiom);
			//rcggrammar = gc.getRcggrammar(); // we can no longer do this, because of the detection of de-facto non-adjoinable nodes
			rcggrammar = new RCG();
			rcggrammar.addGrammar(gc.getRcggrammar(), grammarDict);
		}
		long estimatedTime = System.nanoTime() - startTime;
		totalTime += estimatedTime;
		if (rcggrammar == null || rcggrammar.getStartPredicateLabel() == null) {
			//System.err.println("Grammar conversion failed. \nPlease check the value of the axiom.");
			throw new Exception("Polarity filtering / grammar conversion failed. \nPlease check the value of the axiom and the lexicon.");
		} else
			System.err.println("Grammar conversion time: "+(estimatedTime)/(Math.pow(10, 9))+" sec.");
		// for printing the RCG grammar computed
		// either pretty printed:
		//System.err.println(rcggrammar.toString(ts.getTreeHash())+"\n");
		// --------------------------------------------------------------
		// or not (i.e. the real grammar + id interpretations):
		if (verbose) {
			Iterator<String> itt = grammarDict.keySet().iterator();
			while(itt.hasNext()) {
				String ttree = itt.next();
				System.err.println("Tree " + ttree + "\t := " + grammarDict.get(ttree).getOriginalId());
			}
			System.err.println(rcggrammar.toString());
		}

		// 7'. RCG XML export (for DyALog)
		if (op.check("e")) {
			Document rcgd = RCGDOMbuilder.exportGrammar(rcggrammar, grammarDict);
			XMLUtilities.writeXML(rcgd, op.getVal("e"), "rcg.dtd,xml", true);
		}
		
		// 8. RCG parsing
		RCG rcgg = rcggrammar;
		long sTime = System.nanoTime();
		RCGParser parser = null;
		if (op.check("q1")) {
			parser = new RCGParserEarleyNoTerm(rcgg);
		} else if (op.check("q2")) {
			parser = new RCGParserEarley(rcgg);
		} else {
			parser = new RCGParserBoullier2(verbose, rcgg, grammarDict, rcgg.getCategories());
		}
		
		// for printing the categories modifiable within the subgrammar:
		//System.err.println(rcgg.getCategories());
		
		// the first parameter of parseSentence defines the verbosity
		boolean parseres = parser.parseSentence(verbose, tokens);
		if (parseres){
			System.err.println("Sentence \"" + tok.getSentence() + "\" parsed.");
			long estTime = System.nanoTime() - sTime;
			System.err.println("Parsing time: " + (estTime)/(Math.pow(10, 9)) + " sec.");
			// for printing the RCG derivation forest (also printed by the parser in verbose mode)
			//System.err.println(parser.printForest());
			
			// 9. Forest extraction
			ExtractForest ef = new ExtractForest(verbose, rcgg, parser.getAnswers(), parser.getParse());
			long fTime = System.nanoTime();
			ef.extract();
			long estfTime = System.nanoTime() - fTime;
			// for printing the tree derivation forest
			if (verbose)
				System.err.println(ef.printForest());
			Document fdoc = ProduceDOM.buildDOMForest(ef.getForest(), ef.getStart(), tok.getSentence(), op.getVal("g"), new NameFactory(), null);
			// 9'. forest XML export
			if (op.check("f")) {
				Document fdoc2 = ProduceDOM.buildDOMForest(ef.getForest(), ef.getStart(), tok.getSentence(), op.getVal("g"), new NameFactory(), grammarDict);
				XMLUtilities.writeXML(fdoc2, op.getVal("f"), "tulipa-forest3.dtd,xml", true);
			}
		
			System.err.println("Forest extraction time: "+(estfTime)/(Math.pow(10, 9))+" sec.");
			// update the time counter
			totalTime += estTime + estfTime;		
 			
			// 10. output of the parses 
  			  				
  			ArrayList<ParseTreeCollection> ptcs = DerivedTreeViewer.getViewTreesFromDOM(fdoc, grammarDict, false, false, false, needsAnchoring, slabels, noUtool);
			for (ParseTreeCollection ptc : ptcs){
				List<SemLit> reading = ptc.getSemantics();
				readings.add(reading);
			}
  			return readings;
  			
		} else {
			long noTime = System.nanoTime() - sTime;
			totalTime += noTime;
			System.err.println("No derivation forest available.");
		}
		
		// total = loading + anchoring + conversion + parsing + forest + XML / derivation trees
		System.err.println("\nTotal parsing time for sentence \""+ sentence + "\": " +(totalTime)/(Math.pow(10, 9))+" sec.");
		
		return readings;
	}
	
}

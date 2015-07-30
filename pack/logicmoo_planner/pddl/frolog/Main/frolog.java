package Main;


import UserInterface.PlayerInterface;
import jpl.*;
import de.tuebingen.ui.CommandLineOptions;
import de.tuebingen.tree.Grammar;
import NLPModules.Parsing.*;
import java.util.*; 



/**
 * This is the entry point of the application
 *  
 * There are several libraries that need to be loaded: gecodej, jpl
 * I've specified this as an argument to the VM in the eclipse project like this:
 * -Djava.library.path="/usr/lib/pl-5.6.54/lib/i386-linux:/usr/local/lib"
 * May be it's better to load them in the code using System.load(arg0)
 * 
 * @author benottil
 *
 */

public class frolog {

	/**
	 * @param args
	 */
	static Process geni;
	static Process racer;
	public static CommandLineOptions tulipaOptions;
	public static Grammar tulipaGrammar;
//	public static String[] thematicRoles= {"agent","patient","instrument","source","goal","instrument"};
	public static Set<String> discourseModel = new HashSet<String>();
	public static String lastIndividual = ""; 
	public static String PreferedOrdering = "['colour','(some haslocation *top*)','(some hasdetail *top*)']";
	//primitive concepts are those concepts that are not defined in the tbox, 
	//they are either in the game abox or asserted through action effects
	public static List<String> primitiveConcepts = Arrays.asList("drawingroom","treasury","room","wall","green","white",
		"player","alive","frog","ugly","brown","sword","small", "crown","silver","apple","dragon","worm",
		"red","chest","wooden","locked","unlocked","closed","open","couch","couchleg","golden","key","table","southexit","northexit",
		"hasdetail","hold","fitsin","hasexit","leadsto","hascounterpart","gone","happy","beautiful","pizza","yellow");
	
	
	public static void main(String[] args) {
		
//		System.load("/usr/lib/pl-5.6.54/lib/i386-linux/libjpl.so");
		
		initialize();
		
		PlayerInterface.startInterface();
		
		try {
			geni.destroy();
			System.out.println("Geni closed properly");
			racer.destroy();
			System.out.println("Racer closed properly");
		
		} catch (Exception e) {
    		System.err.println(e);
    		System.exit(0); 
		}
	}
	
	public static void initialize(){
	
		
		try {
			//I am not using morphology in geni because it's not working (see geni mailing list) --morphlexicon GameScenarios/FairyTaleCastle/ObjectGrammarGeni/morph.mph
			//Alexandre Denis has this option when calling Geni, I don't know what's its purpose --rootfeat='[cat:Nom|NomPropre|Sen|Prix|Pro]'
			geni = Runtime.getRuntime().exec("NLPModules/Realization/geniserver -m GameScenarios/FairyTaleCastle/ObjectGrammarGeni/lu-small-grammar.geni -l GameScenarios/FairyTaleCastle/ObjectGrammarGeni/lexicon.geni --rootfeat='[cat:s|np]' &");
			System.out.println("Geni (my generator) started successfully");
			
			//racer = Runtime.getRuntime().exec("KBInterfaces/RacerInterface/RacerPro-1-9-2-beta/RacerPro -silent > racer_log.txt &");
			racer = Runtime.getRuntime().exec("KBInterfaces/RacerInterface/RacerPro-Linux32-1-9-3-Beta/RacerPro -silent &");
			System.out.println("Racer (my knowledge base manager) started successfully");
			
			new Query("consult('KBInterfaces/RacerInterface/racer.pl')").hasSolution();			 
			Query q3 = new Query("racer:load_scenario(f)");
			System.out.println(q3.hasSolution() ? "The game scenario knowledge bases were loaded" : "The game scenario failed to load");
		
			String[] commandline = "-g GameScenarios/FairyTaleCastle/ObjectGrammarTulipa/lu-small-grammar.xml -l GameScenarios/FairyTaleCastle/ObjectGrammarTulipa/lexicon.xml -m GameScenarios/FairyTaleCastle/ObjectGrammarTulipa/morph.xml -a s -i -n".split(" ");
			tulipaOptions = Tulipa.processCommandLine(commandline);
			tulipaGrammar = Tulipa.initializeTulipa(tulipaOptions);
			System.out.println("Tulipa (my parser) started successfully");
			
			new Query("consult('NLPModules/ReferenceGeneration/referenceGeneration.pl')").hasSolution();
			new Query("consult('NLPModules/ReferenceResolution/resolve.pl')").hasSolution();
			new Query("consult('NLPModules/Actions/execute.pl')").hasSolution();
			new Query("consult('NLPModules/ContentDetermination/describe.pl')").hasSolution();
			new Query("consult('GameScenarios/FairyTaleCastle/actionDatabase.pl')").hasSolution();
			
			
			} catch (Exception e) {
				System.err.println(e);
			}	
	}
} 

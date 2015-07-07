package NLPModules.Parsing;

import java.util.*;

import de.tuebingen.tag.*;
import Tools.*;
import NLPModules.ReferenceResolution.*;


/**
 * @author benottil
 *
 */
public class Readings {
	
	private List<List<SemLit>> readings;
	
	public Readings(List<List<SemLit>> readings) {
		super();
		this.readings = readings;
	}

	// The class List cannot be instantiated because it's an abstract class, 
	// use LinkedList instead for initializing a List as below.
	// Arrays cannot be happily transformed into Strings, you get rubbish
	// The safe order is Collection -> Array -> List (with Arrays.asList(X)) -> String
	public List<Reading> cleanReadings() {
		
		Index<String,Literal> table = new Index<String,Literal>();
		List<Reading> cleanreadings = new LinkedList<Reading>();
		
		for (List<SemLit> reading:this.readings){
			for (SemLit element : reading){
//				String[] elementpair = element.toString().split(":");
				SemPred predicate = (SemPred) element;
				Literal literal = new Literal(predicate);
				String label = predicate.getLabel().toString(); 
				table.put(label,literal);
			}
			
			Collection<List<Literal>> cleanreading = table.values();
			Reading readingforRR = new Reading(cleanreading);
			cleanreadings.add(readingforRR);
		}
		return cleanreadings;
	}
}

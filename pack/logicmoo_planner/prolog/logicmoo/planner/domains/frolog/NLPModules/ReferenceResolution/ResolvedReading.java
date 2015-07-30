package NLPModules.ReferenceResolution;

import java.util.List;
import java.util.LinkedList;
import java.util.Hashtable;
import de.tuebingen.tag.*;


public class ResolvedReading {
	
	public List<Literal> event;
	public Hashtable<String,String> resolution;
	
	public ResolvedReading(List<Literal> event, Hashtable<String, String> resolution) {
		super();
		this.event = event;
		this.resolution = resolution;
	}
	
	public List<Literal> instantiate(){
		List<Literal> instantiated_reading = new LinkedList<Literal>();
		for (Object literal:this.event) {
			String[] elements = literal.toString().split("[(,)]"); 
			String predicate = elements[0];
			List<Object> arguments = new LinkedList<Object>();
			for (int i=1;i<elements.length;i++){
				String element = elements[i].replaceAll(" ", "");
				if (this.resolution.containsKey(element)){
					String instantiated_argument = this.resolution.get(element);
					arguments.add(instantiated_argument);
				}else{
					arguments.add(element);
				}
				
			}
			Literal formatted_literal = new Literal(predicate,arguments);
			instantiated_reading.add(formatted_literal);
		}
		return instantiated_reading;
	}

}

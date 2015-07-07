package NLPModules.Parsing;

import java.util.*;
import NLPModules.Parsing.*;
import de.tuebingen.tag.*;
import NLPModules.ReferenceResolution.*;

public class Reading {
	
	public List<Literal> event;
	public List<Argument> arguments = new LinkedList<Argument>();
	public Reading(Collection<List<Literal>> readingfromParser) {
		super();	
		for (List<Literal> entity:readingfromParser){
			String entityStr = entity.toString(); 
			if (entityStr.contains("event")){
				this.event = entity;
			}else{
				Argument argument = new Argument();
				for (Literal property:entity){
					String propertyStr = property.toString();
					if (propertyStr.contains("def") || propertyStr.contains("indef")){
						argument.article = propertyStr;
					}else{
						argument.properties.add(property);
					}
				}
				if (argument.article.equals("no")){
					//get the constant that represents the referent
					String constant = argument.properties.get(0).arguments.get(0).toString();
					argument.article = "def("+ constant +")";
					//if the referent is the pronoun "it" eliminate it's semantics
					if (argument.properties.get(0).predicate.equals("it")){
						argument.properties.remove(0);
					}
				}
				this.arguments.add(argument);
			}		
		}
	}
	

}

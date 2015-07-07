package NLPModules.ContentDetermination;

import java.util.LinkedList;
import java.util.List;
import java.util.HashSet;
import java.util.Set;
import de.tuebingen.tag.*;
import Tools.FrologException;
import NLPModules.ReferenceResolution.Literal;

public class Utterance {
	
//	public List<Literal> content_toverbalize = new LinkedList<Literal>();
	public List<Literal> content_toassert = new LinkedList<Literal>();
	public List<Literal> content_toretract = new LinkedList<Literal>();
	public String canned_text = "";
	
	public Utterance(List<Literal> assertions, List<Literal> retractions, String fixed_string){
		super();
		this.content_toassert(assertions);
		this.content_toretract = retractions;
		this.canned_text = fixed_string;
	}
	
	public Utterance(List<Literal> assertions, String fixed_string){
		super();
		this.content_toassert(assertions);
		this.canned_text = fixed_string;
	}
	
	public Utterance(List<Literal> assertions){
		super();
		this.content_toassert(assertions);
	}
	
	private void content_toassert(List<Literal> assertions){
		this.content_toassert = assertions;
//		this.content_toverbalize = assertions;
	}
	
	public String toString(){
		return this.content_toassert.toString();
	}
	
	public void updatePlayerKB() throws FrologException{
		for (Literal literal:this.content_toretract){
			literal.retractPlayerKB();
		}
		for (Literal literal:this.content_toassert){
			if (!this.canned_text.contains("the case that") && !this.canned_text.contains("accommodate") && !this.canned_text.contains("I suggest that")  && !this.canned_text.contains("and then")){
				//if the content to be verbalized is a positive precondition that failed 
				//such as "the apple is not takeable" 
				//then this means that the system is not asserting anything 
				literal.assertPlayerKB();
			}
		}
	}
	
	public boolean hascomplexargument(){
		boolean res = false;
		int i = 0; 
		while (!res && (i<this.content_toassert.size())){
			Literal literal = this.content_toassert.get(i);
			res = literal.hascomplexargument();
			i += 1;
		}
		return res;
	}
	
	public void indicate_n0A1(){
		List<Literal> ands = new LinkedList<Literal>();
		for (int i=0; i < this.content_toassert.size(); i++){
			Literal literal = this.content_toassert.get(i);
			if (literal.arguments.size() == 1) {
				List<String> realization_classes = new LinkedList<String>();
				//the first adjective is the anchor of the sentence
				if (i==0) { 
					realization_classes.add("n0A1");
				} else {
					realization_classes.add("adjective");
					List<Object> args = new LinkedList<Object>();
					args.add("x"+Integer.toString(i));
					Literal and = new Literal("and",args);
					ands.add(and);
				}
				literal.realization_classes = realization_classes;
			}
		}
		this.content_toassert.addAll(ands);
	}
	
	public List<Literal> getReferringExpression(String individual){
		List<Literal> re = new LinkedList<Literal>();
		for (Literal literal:this.content_toassert){
			if (literal.containsArgument(individual)){
				re.add(literal);
			}
		}
		return re;
	}

	public Set<String> referents(){
		Set<String> ref = new HashSet<String>();
		String eventargument = "";
		for (Literal literal: this.content_toassert){
			//TODO this solution for not generating referring expressions for events is very dirty
			// if the literal represents an event then the arguments are not referents
			if (!literal.arguments.get(0).equals("event")){
				if (!literal.predicate.equals("and")){
					for (int i=0; i< literal.arguments.size(); i++){
						Object argument = literal.arguments.get(i);
						if (argument instanceof String){
							if (argument.toString().startsWith("_")){
								argument = "xxx";
								literal.arguments.set(i, argument);
							} 
							ref.add(argument.toString());
						}else{
							List<String> list = (List<String>) argument;
							ref.addAll(list);
						}
					}
				}
			} else {
				eventargument = literal.arguments.get(1).toString();
			}
		}
		// if an event argument was found, eliminate it from the referents
		if (!eventargument.isEmpty()){
			ref.remove(eventargument);
		}
		
		return ref;
	}
	
	public boolean has_predicate(String predicate){
		boolean res = false;
		int i = 0; 
		while (!res && (i<this.content_toassert.size())){
			Literal literal = this.content_toassert.get(i);
			res = literal.predicate.equals(predicate);
			i += 1;
		}
		return res;
	}
	
	public List<Object> get_arguments(String predicate){
		boolean res = false;
		int i = 0; 
		List<Object> arguments = new LinkedList<Object>();
		while (!res && (i<this.content_toassert.size())){
			Literal literal = this.content_toassert.get(i);
			res = literal.predicate.equals(predicate);
			arguments = literal.arguments;
			i += 1;
		}
		return arguments;
	}
	
	public String toStringforGeneration(){
		String res = "";
		for (Literal literal:this.content_toassert){
			res += literal.toStringforGeneration();
			res += " ";
		}
		return res;
	}
	
	public boolean add_referringexpression(List<Literal> referringexpression){
		
		boolean meaningful;
		
		if (referringexpression.containsAll(this.content_toassert)){
			meaningful = false;
		} else {
			meaningful = true;
			for (Literal literal: referringexpression){
				boolean found = false;
				int i = 0;
				while ((!found) && (i<this.content_toassert.size())){
					found = this.content_toassert.get(i).equals(literal);
					i++;
				}
				if (!found) {
					this.content_toassert.add(literal); 
				} else {
					this.content_toassert.remove(i-1);
					this.content_toassert.add(literal); 
				}
			}
		}
		return meaningful;
	}
}

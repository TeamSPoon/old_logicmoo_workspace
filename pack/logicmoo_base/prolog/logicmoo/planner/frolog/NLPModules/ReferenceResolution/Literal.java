package NLPModules.ReferenceResolution;

import java.util.List;
import Tools.FrologException;
import java.util.LinkedList;
import java.util.Arrays;

import jpl.Query;
import de.tuebingen.tag.*;
import Main.frolog;

public class Literal {
	
	public String predicate;
	public List<Object> arguments = new LinkedList<Object>();
	public Boolean negated = false;
	public List<String> realization_classes = new LinkedList<String>();
	
	public Literal(SemPred sempred){
		super();
		this.predicate = sempred.getPred().toString().toLowerCase();
		this.negated = sempred.isNegated();
		this.arguments = new LinkedList<Object>();
		for (Value value:sempred.getArgs()){
			this.arguments.add(value.toString().toLowerCase().replace("?", ""));
		}
	}
	
	public Literal(String predicate, List<Object> arguments) {
		super();
		this.predicate = predicate;
		this.arguments = arguments;
		this.negated = false;
	}
	
	public Literal(String predicate, jpl.Term[] arguments, List<String> realization_classes) {
		super();
		this.predicate = predicate;
		for (jpl.Term argument:arguments){
			//an argument of a literal can be an atom, a variable or a list
			if (argument.isAtom() || argument.isVariable()){
				this.arguments.add(argument.toString());
			} else {
				List<jpl.Term> list_atoms = Arrays.asList(argument.toTermArray());
				List<String> list_strings = new LinkedList<String>();
				for (jpl.Term atom:list_atoms){
					list_strings.add(atom.toString());
				}
				this.arguments.add(list_strings);
			} 
		}
		this.realization_classes = realization_classes;
	}
	
	public Literal(String predicate, jpl.Term[] arguments) {
		super();
		this.predicate = predicate;
		for (jpl.Term argument:arguments){
			//an argument of a literal can be an atom, a variable or a list
			if (argument.isAtom() || argument.isVariable()){
				this.arguments.add(argument.toString());
			} else {
				List<jpl.Term> list_atoms = Arrays.asList(argument.toTermArray());
				List<String> list_strings = new LinkedList<String>();
				for (jpl.Term atom:list_atoms){
					list_strings.add(atom.toString());
				}
				this.arguments.add(list_strings);
			} 
		}
	}
	
	public void assertPlayerKB() throws FrologException {
		//if it is a primitive concept or role (i.e. not a defined concept), assert it
		if (frolog.primitiveConcepts.contains(this.predicate)){
			if (this.hascomplexargument()){
				List<Literal> flattened = this.flattencomplexargument();
				for (Literal literal:flattened){
					literal.assertPlayerKB();
				}
			} else {		 
				Query query = new Query("assert_playerKB("+ this.toString() +")");
				if (!query.hasSolution()){
					FrologException e = new FrologException("racer_error","");
					throw e;
				} else {
					System.out.print(query.toString()+",");
				}
			}
		}	
	}
	
	public void retractPlayerKB() throws FrologException {
		//if it is a primitive concept or role (i.e. not a defined concept), retract it
		if (frolog.primitiveConcepts.contains(this.predicate)){
			Query query = new Query("retract_playerKB("+ this.toString() +")");
			
			if (!query.hasSolution()){
				FrologException e = new FrologException("racer_error","");
				throw e;
			} else {
				System.out.print(query.toString()+",");
			}
		}
	}

	public boolean hascomplexargument(){
		boolean res = false;
		int i = 0; 
		while (!res && (i<this.arguments.size())){
			Object argument = this.arguments.get(i);
			res = (argument instanceof List); 
			i += 1;
		}
		return res;
	}
	
	public List<Literal> flattencomplexargument(){
		List<Literal> res = new LinkedList<Literal>();
	    //When the literal has a complex argument, it's the second argument that is complex
		String predicate = this.predicate;
		String firstargument = (String) this.arguments.get(0);
		List<Object> secondargument = (List<Object>) this.arguments.get(1);
		for (Object object:secondargument){
			List<Object> arguments = new LinkedList<Object>();
			arguments.add(0, firstargument);
			arguments.add(1,object);
			Literal literal = new Literal(predicate,  arguments);
			res.add(literal);
		}
		return res;
	}
		
	public String toString(){
		String res = "";
		if (this.negated) {
			res += "not(";
		}
		res += this.predicate + "(";
		for(int i = 0 ; i < this.arguments.size() ; i++){
			if (i>0)
				res += ", ";
			res += this.arguments.get(i).toString();
		}
		res += ")";
		if (this.negated) {
			res += ")";
		}
		return res;
	}
	
	public String toStringforGeneration(){
		String res = "";
		res += "A:" + this.predicate + "(";
		for(int i = 0 ; i < this.arguments.size() ; i++){
			if (i>0)
				res += " ";
			res += this.arguments.get(i).toString();
		}
		res += ")";
		if (!(this.realization_classes.isEmpty())){
			res += this.realization_classes;
		}
		return res;
	}
	
	public boolean equals(Object o) {
        return this.hashCode() == o.hashCode();
    }
	
    public int hashCode() {
        return this.toString().hashCode();
    }
	
	public boolean containsArgument(String argument){
		boolean res = false;
		int i = 0;
		while ((!res) && (i<this.arguments.size())){
			res = this.arguments.get(i).equals(argument);
			i = i + 1;
		}
		return res;
	}

}

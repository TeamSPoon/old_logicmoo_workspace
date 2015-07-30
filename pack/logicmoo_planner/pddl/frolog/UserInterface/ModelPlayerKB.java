package UserInterface;

import java.util.Arrays;
import java.util.List;
import java.util.Hashtable;
import Tools.FrologException;
import NLPModules.ReferenceResolution.*;
import java.util.LinkedList;

import jpl.Query;


public class ModelPlayerKB {
	
	public List<Literal> conceptassertions = new LinkedList<Literal>();
	public List<Literal> roleassertions = new LinkedList<Literal>();
	public List<Literal> individualsconcepts = new LinkedList<Literal>();
	public String[] roles = {"hold","hasexit","hascounterpart","leadsto","hasdetail","fitsin"};
	public String[] specialconcepts = {"top","hereobject","easytokill","colour","property",
			"lockedunlocked","openclosed","genericcontainer","opencontainer","object","here"};
	public String[] dynamicconcepts = {"open","accessible","alive","locked","closed","unlocked",
			"inventoryobject","here","seated","visible","disgusting","ugly","beautiful"};
	public List<String> individuals = new LinkedList<String>();

	
	public ModelPlayerKB(String kb) throws FrologException {
		
		super();
		
		//Get all the individuals from the player KB
		//all_individuals_playerKB(Individuals)
		Hashtable<String,jpl.Compound> table1 = query_prolog("all_individuals_"+kb+"KB(Individuals)");
		jpl.Term individuals = table1.get("Individuals");
		List<jpl.Term> list_individuals = Arrays.asList(individuals.toTermArray());
		
		for (jpl.Term individual:list_individuals){
			String ind = individual.toString();
			this.individuals.add(ind);
			jpl.Term[] conceptarg = new jpl.Term[1];
			conceptarg[0] = individual;
			
			// Get all the concepts in which the individuals are involved
			// individual_types_playerKB(Individual,Concepts)
			// construct literals with the assertion and add it to the field assertions
			Hashtable<String,jpl.Compound> table2 = query_prolog("individual_types_"+kb+"KB("+ind+",Concepts)");
			jpl.Compound concepts = table2.get("Concepts");
			jpl.Term[] array_concepts = concepts.toTermArray();
			// each element of the list individual concept has as predicate the individual and as arguments the 
			// concepts to which it belongs
			Literal individualconcepts = new Literal(ind,array_concepts);
			this.individualsconcepts.add(individualconcepts);
			List<jpl.Term> list_concepts = Arrays.asList(array_concepts);
			// Build the concept assertions for each concept to which the individual belongs
			//TODO eliminate the field conceptassertions
			for (jpl.Term concept:list_concepts){
				if (!concept.name().equals("top")){
					String con = concept.toString();
					Literal literal = new Literal(con,conceptarg);
					this.conceptassertions.add(literal);
				}
			}
			
			// Get all the roles in which the individuals are involved
			// individual_fillers_playerKB(Individual,Role,Individuals)
			// construct literals with the assertion and add it to the field assertions
			for (String role:roles){
				Hashtable<String,jpl.Compound> table3 = query_prolog("individual_fillers_"+kb+"KB("+ind+","+role+",Individuals)");
				jpl.Compound fillers = table3.get("Individuals");
				List<jpl.Term> list_fillers = Arrays.asList(fillers.toTermArray());
				for (jpl.Term filler:list_fillers){
					jpl.Term[] rolearg = new jpl.Term[2];
					rolearg[0] = individual;
					rolearg[1] = filler;
					Literal literal = new Literal(role,rolearg);
					this.roleassertions.add(literal);
				}
			}
		}		
		String dot = toStringforDOT();
		int i = 1;
		i = i + 1;	
	}
	
	public Hashtable<String,jpl.Compound> query_prolog(String query) throws FrologException{
		
		Hashtable<String,jpl.Compound> answer;
		Query queryprolog = new Query(query);
		if (queryprolog.hasSolution()) {
			answer = queryprolog.oneSolution();
		} else {
			FrologException fe = new FrologException("prolog_fail",query); 
			throw fe;
		}
		return answer;
	}
	
	public String toString(){
		String res = "";
		for (Literal literal:this.individualsconcepts){
			res += literal.toString()+"\n";
		}
		for (Literal literal:this.roleassertions){
			res += literal.toString()+"\n";
		}
		return res;
	}
	
	public String toStringforDOT(){
		
		String res = "digraph M {\n " +
						"\t graph [rankdir = \"LR\"]; \n" +
						"\t node [fontsize = \"10\"]; \n" +
						"\t edge [fontsize = \"10\"]; \n";
		List<String> special = Arrays.asList(this.specialconcepts);
		for (Literal literal:this.individualsconcepts){
			res +="\t "+ literal.predicate + " [shape = \"record\" label = \"";
			for(int i = 0 ; i < literal.arguments.size() ; i++){
				String concept = literal.arguments.get(i).toString();
				if (!special.contains(concept)){
					if (i>0) res += "|";
					res += concept;
				}
			}
			res +="\"];\n";
		}
		for (Literal literal:this.roleassertions){
			String rolename = literal.predicate;
			String firstargument = literal.arguments.get(0).toString();
			String secondargument = literal.arguments.get(1).toString();
			res += "\t " + firstargument + " -> " + secondargument + "[label =\""+ rolename +"\"];\n";
		}
		res += "}";
		return res;
	}
	
	public String toStringforPlanner(Literal goal){

		
		List<String> dynamicconcepts = Arrays.asList(this.dynamicconcepts);
		//Write heading of the planning problem
		String res = "(define (problem froza)\n" +
						"\t(:domain fairytalecastle)\n"; 
		
		//Declare the individuals
		res += "\t(:objects \n";
		for (Literal individual:this.individualsconcepts){
			res += "\t\t" + individual.predicate + " - ";
			res += "(either";
			//Write the types of the individuals
			for(int i = 0 ; i < individual.arguments.size() ; i++){
				String concept = individual.arguments.get(i).toString();
				//If the concept is not dynamic then it's a type
				if (!dynamicconcepts.contains(concept)){
					res += " " + concept;
				}
			}
			res += ")\n";
		}
		res += "\t)\n";
		
		//Declare the initial state
		res += "\t(:init \n";
		//Write the concept assertions
		for (Literal individual:this.individualsconcepts){
			for (String concept:this.dynamicconcepts){
				if (individual.containsArgument(concept)){
					res += "\t\t(" + concept + " " + individual.predicate + ")\n";
				} else {
					res += "\t\t(no-" + concept + " " + individual.predicate + ")\n";
				}
			}
			res += "\n";
		}
		res += "\n";
		
		//Write the role assertions
		List<Literal> allroleassertions = allroleassertions();
		for (Literal roleassertion:allroleassertions){
			String rolename = roleassertion.predicate;
			String rolearg0 = roleassertion.arguments.get(0).toString();
			String rolearg1 = roleassertion.arguments.get(1).toString();
			String assertionstr = rolename + " " + rolearg0 + " " + rolearg1;
			if (this.roleassertions.contains(roleassertion)){
				res += "\t\t(" + assertionstr + ")\n";
			} else {
				res += "\t\t(no-" + assertionstr + ")\n";
			}
		}
		res += "\t)\n";
		
		//Write the goal
		res += "\t(:goal\n";
		String goalstr = goal.predicate;
		for (Object arg:goal.arguments){
			goalstr += " " + arg.toString();
		}
		if (goal.negated){
			goalstr = "not(" + goalstr + ")";
		}
		res += "\t\t(" + goalstr + ")\n";
		res += "\t)\n";
		
		//Close the initial state
		res += ")";
		return res;
	}
	
	private List<Literal> allroleassertions(){ 
		
		List<Literal> allroleassertions = new LinkedList<Literal>();
		for (String role:this.roles){
			for (String individual0:this.individuals){
				for (String individual1:this.individuals){
					List<Object> listarguments = new LinkedList<Object>(); 
					listarguments.add(0, individual0);
					listarguments.add(1, individual1);
					Literal literal = new Literal(role,listarguments);
					allroleassertions.add(literal);
				}
			}
		}
		return allroleassertions;
	}
}



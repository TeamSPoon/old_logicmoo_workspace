package NLPModules.ReferenceResolution;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import jpl.Query;
import Main.frolog;
import NLPModules.Parsing.Argument;
import NLPModules.Parsing.Reading;
import UserInterface.PlayerInterface;
import NLPModules.ContentDetermination.*;
import NLPModules.Realization.*;
import Tools.*;

public class ResolvedReadings {

	public List<ResolvedReading> readings = new LinkedList<ResolvedReading>();
	
	public ResolvedReadings(List<Reading> cleanreadings) throws FrologException {
		
		Hashtable<String,String> resolution = new Hashtable<String,String>();
		for (Reading reading:cleanreadings){
			for (Argument argument:reading.arguments){
				PlayerInterface.reference_resolution.append("\t" + argument.article.toLowerCase() + "," + argument.properties.toString().toLowerCase() + " refers to -> ");
				String t5 = "resolveResolutionMain((" + argument.article.toLowerCase() + "," + argument.properties.toString().toLowerCase() + "),PIndividual,KBIndividual," + frolog.discourseModel + ").";
				Query q5 = new Query(t5); 
				Hashtable solution = q5.oneSolution(); 
				jpl.Compound KBIndividuals = (jpl.Compound) solution.get("KBIndividual");
				jpl.Atom PIndividual = (jpl.Atom) solution.get("PIndividual");
				String parsingVar = PIndividual.toString().toLowerCase();
				List<jpl.Term> terms = Arrays.asList(KBIndividuals.toTermArray());
				List<String> individuals = JPL_wrapper.listTermtolistString(terms);

				PlayerInterface.reference_resolution.append(individuals + "\n");
				if (((argument.article.contains("indef")) && (individuals.size() > 1)) || (individuals.size() == 1)) {
					//Here indefinite references are resolved to the first element in the list of individuals
					//The ambiguity of the definites should be trasferred to the action module, 
					//in order to take into account the affordability of the current state. 
					String individual = individuals.get(0).toString();
					resolution.put(parsingVar,individual);
					frolog.discourseModel.add(individual);
					if (!individual.equals("myself")){
						frolog.lastIndividual = individual;
					}
					
				}else{
					//Non existing referents and ambiguous ones. 
					
					List<Literal> np_semantics = argument.properties;
					if (np_semantics.isEmpty()){
						List<Object> args = new LinkedList<Object>();
						args.add("x");
						Literal pronoun = new Literal("it",args);
						np_semantics.add(pronoun);
					}
					String np_realization = Generation.generateNP(np_semantics);
					if (individuals.size() > 1) {
						Set<String> salient_individuals = frolog.discourseModel;
						salient_individuals.retainAll(individuals);
						if (salient_individuals.size() == 1){
							List<Object> list = Arrays.asList(salient_individuals.toArray());
							resolution.put(parsingVar,list.get(0).toString());
						} else {
							if ((np_semantics.get(0).predicate.equals("it")) && (!frolog.lastIndividual.isEmpty())){
								resolution.put(parsingVar, frolog.lastIndividual);
							} else{
								AmbiguousReference e = new AmbiguousReference("reference_ambig",np_realization,individuals);
								throw e;
							}
						}
					} 
					if (individuals.size() == 0) {
						FrologException e = new FrologException("no_ref_for",np_realization);
						throw e;
					}
				}
			}
			ResolvedReading resolved_reading = new ResolvedReading(reading.event,resolution);
			this.readings.add(resolved_reading);
		}
	}
	
	public List<List<Literal>> instantiate(){
		
		List<List<Literal>> resolved_readings = new LinkedList<List<Literal>>();
		for (ResolvedReading reading:readings){
			List<Literal> literals = reading.instantiate();
			resolved_readings.add(literals);
		}
		return resolved_readings;
	}

}

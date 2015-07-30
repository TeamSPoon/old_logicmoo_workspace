package NLPModules.ContentDetermination;

import java.util.List;
import java.util.LinkedList;
import NLPModules.Actions.*;
import NLPModules.ReferenceResolution.*;
import Tools.FrologException;

public class Utterances {
	
	public List<Utterance> content = new LinkedList<Utterance>();
	
	public Utterances(Command command) throws FrologException {
		super();
		switch (command.decision) {
			case executed: 
				//It can be assumed that the list has only one element otherwise the command would be ambiguous
				ExecutedReading executed_reading = command.execution.get(0);
				this.DetermineContent(executed_reading);
				break;
			
			case failed: 
				//I'm verbalizing the failed precondition of the first failed reading, 
				//something more intelligent might be necessary
				FailedReading failed_reading = (FailedReading) command.execution.get(0);
				Literal failed_precondition = failed_reading.failed_precondition;
				List<Literal> list_preconditions = new LinkedList<Literal>();
				list_preconditions.add(failed_precondition);
				if (failed_precondition.negated){
					//here the precondition is asserted because it is assumed that not(not(precondition)) = precondition
					Utterance utterance = new Utterance(list_preconditions,"You can't do that! It is the case that ");
					content.add(utterance);
				} else {
//					The failed precondition is not asserted
					Utterance utterance = new Utterance(list_preconditions,"You can't do that! It is not the case that ");
					content.add(utterance);
				}
				break;
			
			case ambiguous: 
				//TODO
				FrologException e = new FrologException("ambiguous",command.semantics.toString());
				throw e;
		}
	}
	
	public Utterances(List<Utterance> utterances){
		this.content = utterances;
	}
	
	public String toString(){
		String res = "";
		for (Utterance utterance:this.content){
			res += utterance.toString()+"\n";
		}
		return res;
	}
	
	public void DetermineContent(ExecutedReading executed_reading){

		for (ExecutedAction action:executed_reading.schemas){
			
			List<Utterance> utterances = new LinkedList<Utterance>();
			for (Literal effect:action.effects_del){
				List<Literal> listeffectadd = new LinkedList<Literal>();
				List<Literal> listeffectdel = new LinkedList<Literal>();
				listeffectdel.add(effect);
				Utterance utterance = new Utterance(listeffectadd,listeffectdel, "");
				utterances.add(utterance);
			}
			for (Literal effect:action.effects_add){
				List<Literal> listeffectadd = new LinkedList<Literal>();
				listeffectadd.add(effect);
				Utterance utterance = new Utterance(listeffectadd, "");
				utterances.add(utterance);
			}
			
			for (int i=0; i<utterances.size(); i++){
				//Check whether the utterance has a predicate "describe"
				Utterance utterance = utterances.get(i);
				if ((!utterance.content_toassert.isEmpty()) && (utterance.content_toassert.get(0).predicate.equals("describe"))){
					Description description = new Description();
					Literal describeliteral = utterance.content_toassert.get(0);
					//eliminate the predicate describe, it will be replaced by the description
					utterances.remove(i);
					List<Object> thing = describeliteral.arguments;
					if (thing.contains("here")){
						description.describe_here();
					} else {
						if (thing.contains("contents")) {
							//if the contents have to be decribed the pred "describe" has 2 arguments
							//the first argument is "contents" and the second the name of the container
							String object = thing.get(1).toString();
							description.describe_contents(object);
						} else {
							description.describe_thing(thing.get(0).toString());
						}
					}
//					for (Utterance describeut:description.utterances){
//						describeut.indicate_n0A1();
//					}
					content.addAll(i, description.utterances);
				} else {
					content.add(utterance);
				}
			}
		}
	}	
}




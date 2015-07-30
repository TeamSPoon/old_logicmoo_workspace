package NLPModules.Actions;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;
import java.util.LinkedList;

import UserInterface.PlayerInterface;
import jpl.Query;
import NLPModules.ReferenceResolution.*;
import Tools.FrologException;

public class Command {
		
	public List<List<Action>> semantics = new LinkedList<List<Action>>();
	public List<ExecutedReading> execution = new LinkedList<ExecutedReading>();
	public Decision decision;
	
	public enum Decision {executed, failed, ambiguous}

	public Command(List<List<List<Literal>>> readings) throws FrologException {
		super();
		for (List<List<Literal>> readingsem:readings) {
			List<Action> reading = new LinkedList<Action>();
			for (List<Literal> actionsem:readingsem){
				Action action = new Action(actionsem);
				reading.add(action);
			}
			
			this.semantics.add(reading);
		}
	}
	
	public Command(List<List<Literal>> readingsem, int i) throws FrologException {
		super();
		List<Action> reading = new LinkedList<Action>();
		for (List<Literal> actionsem:readingsem){
			Action action = new Action(actionsem);
			reading.add(action);
			}
			this.semantics.add(reading);
	}
	
	public void trytoExecute(){
		
		Query execute_query = new Query("execute:executeCommand("+ this.semantics + ",Executed,Failed,Decision).");
		Hashtable result_query = execute_query.oneSolution();
		
		jpl.Compound executed = (jpl.Compound) result_query.get("Executed");
		jpl.Compound failed = (jpl.Compound) result_query.get("Failed");
		jpl.Atom atomdecision = (jpl.Atom) result_query.get("Decision");
		List<jpl.Term> list_executed = Arrays.asList(executed.toTermArray());
		List<jpl.Term> list_failed = Arrays.asList(failed.toTermArray());
		
		this.decision = Decision.valueOf(atomdecision.toString());
		
		for (jpl.Term reading: list_executed){
			ExecutedReading executed_reading = new ExecutedReading(reading);
			execution.add(executed_reading);
		}
		for (jpl.Term reading: list_failed){
			FailedReading executed_reading = new FailedReading(reading);
			execution.add(executed_reading);
		}
	}
	
	public String toString() {
		String res = "Scenario command("+ this.decision +"): " + this.semantics.toString() + "\n\n";
		for (ExecutedReading executedreading:this.execution){
			res += executedreading.toString() + "\n";
		}
		
		return res;
	}
	
}

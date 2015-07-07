package NLPModules.Actions;

import java.util.List;
import java.util.LinkedList;

public class ExecutedReading {
	
	public List<ExecutedAction> schemas = new LinkedList<ExecutedAction>();

	public ExecutedReading(jpl.Term term) {
		super();		
		jpl.Term[] actions = term.args()[0].toTermArray();
		for (jpl.Term action:actions){
			ExecutedAction executed_action = new ExecutedAction(action);
			schemas.add(executed_action);
		}
	}
	
	public String toString(){
		String res = "";
		for (ExecutedAction schema:this.schemas){
			res += schema.toString() + "\n";
		}
		return res;
	}
}

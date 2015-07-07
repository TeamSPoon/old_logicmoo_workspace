package NLPModules.Actions;

import jpl.Term;
import NLPModules.ReferenceResolution.Literal;
import java.util.Iterator;
import java.util.Arrays;

public class FailedReading extends ExecutedReading {
	
	public ExecutedAction failed_action;
	public Literal failed_precondition;

	public FailedReading(Term term) {
		super(term);
		jpl.Term[] failure = term.args()[2].args();
		jpl.Term failedaction = failure[0];
		jpl.Term failedpre = failure[1];
		Iterator iterator = this.schemas.iterator();
		ExecutedAction action = (ExecutedAction) iterator.next();
		Boolean same_name = action.name.equals(failedaction.name());
		String actionroles = action.thematic_roles.toString();
		String failedroles = Arrays.asList(failedaction.args()).toString();
		Boolean same_roles = actionroles.equals(failedroles);
		Boolean is_the_action = same_name  && same_roles;
		while ((iterator.hasNext()) && !(is_the_action))	action = (ExecutedAction) iterator.next();
		this.failed_action = action;
		jpl.Term termpre = failedpre.args()[0]; 
		Literal literalpre = new Literal(termpre.name(),termpre.args());
		if (failedpre.name().equals("notk")) literalpre.negated = true;
		this.failed_precondition = literalpre;
	}
	
	public String toString(){
		String res = "";
		res += super.toString();
		res += "Failed action: " + this.failed_action.name + this.failed_action.thematic_roles + "\n";
		res += "Failed precondition: " + this.failed_precondition.toString() + "\n";
		return res;
	}

}

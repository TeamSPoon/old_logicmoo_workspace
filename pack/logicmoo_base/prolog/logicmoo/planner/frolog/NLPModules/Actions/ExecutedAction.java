package NLPModules.Actions;

import java.util.LinkedList;
import java.util.List;
import java.util.Arrays;
import NLPModules.ReferenceResolution.Literal;

public class ExecutedAction extends Action {
	
	public List<Literal> preconditions = new LinkedList<Literal>();
	public List<Literal> effects_add = new LinkedList<Literal>();
	public List<Literal> effects_del = new LinkedList<Literal>();
	
	public ExecutedAction(jpl.Term term) {
		super();
		
		jpl.Term[] schema = term.args();
		this.name = schema[0].name();
		for (jpl.Term role:Arrays.asList(schema[0].args())){
			Literal thematic_role = new Literal(role.name(),role.args());
			this.thematic_roles.add(thematic_role);
		}
		
		jpl.Term[] action_body = schema[1].args();
		jpl.Term init = action_body[0];
		List<jpl.Term> elements = new LinkedList<jpl.Term>();
		while (init.name().equals(",")) {
			jpl.Term[] args = init.args();
			jpl.Term element = args[0];
			init = args[1];
			elements.add(element);
		} 
		elements.add(init);
		for (jpl.Term element:elements){
			jpl.Term literal = element.args()[0];
			Literal lit = new Literal(literal.name(),literal.args());
			if (element.name().equals("k"))
				this.preconditions.add(lit);
			if (element.name().equals("notk")){
				lit.negated = true;
				this.preconditions.add(lit);
			}
			if (element.name().equals("add"))
				this.effects_add.add(lit);
			if (element.name().equals("del"))
				this.effects_del.add(lit);
		}
	}
	
	public String toString(){
		String res = "";
		res += "action: " + this.name + this.thematic_roles + "\n";
		res += "preconditions: " + this.preconditions + "\n";
		res += "effects (add): " + this.effects_add + "\n";
		res += "effects (del): " + this.effects_del + "\n";
		return res;
	}
}

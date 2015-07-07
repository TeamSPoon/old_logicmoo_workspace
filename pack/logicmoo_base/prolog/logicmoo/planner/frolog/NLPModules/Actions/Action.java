package NLPModules.Actions;

import java.util.List;
import java.util.Hashtable;
import java.util.LinkedList;

import jpl.Query;
import NLPModules.ReferenceResolution.*;
import Tools.FrologException;

public class Action {
	
	public String name;
	public List<Literal> thematic_roles = new LinkedList<Literal>();
	
	public Action(){
		super();
	}
	
	public Action(List<Literal> literals) throws FrologException {
		super();
		jpl.Compound action_schema = new jpl.Compound("",0);
		Hashtable<String,String> instantiation = new Hashtable<String,String>();
		for (Literal literal:literals){
			String first_argument = literal.arguments.get(0).toString();
			if (first_argument.equals("event")) {
				this.name = literal.predicate;
				String string = "current_predicate(" + this.name + ",Action), clause(Action,_)";
				Query query = new Query(string);
				if (query.hasSolution()){	
					Hashtable result_query = query.oneSolution();
					action_schema = (jpl.Compound) result_query.get("Action");
				} else {
					FrologException e = new FrologException("missing_action",this.name);
					throw e;
				}
			}else{
				instantiation.put(literal.predicate, literal.arguments.get(1).toString());
			}
		}
		jpl.Term[] roles = action_schema.args();
		for (jpl.Term role:roles){
			String role_name = role.name();
			List<Object> role_value = new LinkedList<Object>();
			if (instantiation.containsKey(role_name)){
				role_value.add(instantiation.get(role_name));
			}else{
				role_value.add("X_" + role_name);
			}
			Literal thematic_role = new Literal(role_name,role_value);
			thematic_roles.add(thematic_role);
		}
	}
	
	public String toString(){
		String res = "";
		res += this.name + "(";  
		for(int i = 0 ; i < this.thematic_roles.size() ; i++){
			if (i>0)
				res += ", ";
			res += this.thematic_roles.get(i);
		}
		res += ")";
		return res;
	}
}

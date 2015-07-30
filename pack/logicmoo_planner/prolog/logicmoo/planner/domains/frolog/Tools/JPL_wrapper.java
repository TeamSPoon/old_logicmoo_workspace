package Tools;

import java.util.List;
import java.util.LinkedList;
import NLPModules.ReferenceResolution.*;

public class JPL_wrapper {
	
	static public List<Literal> listTermtolistLiteral(List<jpl.Term> terms){
		 List<Literal> literals = new LinkedList<Literal>();
		 for (jpl.Term term: terms){
			 Literal literal = new Literal(term.name(),term.args());
			 literals.add(literal);
		 }
		 return literals;
	}
	static public List<String> listTermtolistString(List<jpl.Term> terms){
		List<String> strings = new LinkedList<String>();
		for (jpl.Term term:terms){
			String string = term.toString();
			strings.add(string);
		}
		return strings;
	}
}

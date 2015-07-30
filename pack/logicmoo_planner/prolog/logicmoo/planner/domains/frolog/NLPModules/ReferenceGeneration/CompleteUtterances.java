package NLPModules.ReferenceGeneration;

import NLPModules.ContentDetermination.*;
import NLPModules.ReferenceResolution.*;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;
import java.util.LinkedList;
import Main.frolog;
import java.util.HashSet;
import java.util.Set;

import jpl.Query;
import Tools.*;
import UserInterface.*;

public class CompleteUtterances {
	
	public List<Utterance> semantics = new LinkedList<Utterance>(); 
	
	public CompleteUtterances(Utterances utterances) throws FrologException {
		super();
		for (Utterance utterance:utterances.content){
			Set<String> referents = utterance.referents();
			boolean meaningful = true;
			List<Literal> list_literals;
			utterance.indicate_n0A1();
			for (String referent:referents){
				list_literals = reference_generation(referent);
				meaningful = utterance.add_referringexpression(list_literals);
			}
			if (meaningful){
				//if something is asserted (in constrast with sth retracted) 
				//the complete utterance is added for verbalization
				if (!utterance.content_toassert.isEmpty()){
					semantics.add(utterance);
				}
				//in any case the complete utterance is asserted/retracted in the playerKB
				utterance.updatePlayerKB();
			}
		}	
	}
	
	public String toString(){
		String res = "";
		for (Utterance utterance:this.semantics){
			res += utterance.toString() + "\n";
		}
		return res;
	}
	
	public String toStringforGeneration(){
		String res = "";
		for (Utterance utterance:this.semantics){
			res += utterance.toStringforGeneration() + "\n";
		}
		return res;
	}	
	
	public static List<Literal> reference_generation(String referent){
		List<Literal> list_literals;
		// if the referent is not ground, that is, it is a variable
		if (referent.equals("xxx")){
			list_literals = new LinkedList<Literal>();
			List<Object> args = new LinkedList<Object>();
			args.add(referent);
			Literal literal = new Literal("what",args);
			list_literals.add(literal);
			PlayerInterface.underspecified_command = true;
		} else {
			Query rg_query = new Query("referenceGeneration:generateReferringExpressions("+ referent +"," + frolog.PreferedOrdering + ",ReferringExpression).");
			Hashtable result_query = rg_query.oneSolution();
			jpl.Compound referring_expression = (jpl.Compound) result_query.get("ReferringExpression");
			List<jpl.Term> list_terms = Arrays.asList(referring_expression.toTermArray());
			list_literals = JPL_wrapper.listTermtolistLiteral(list_terms);
		}
		return list_literals;
	}
}


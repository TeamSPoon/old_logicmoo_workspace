package NLPModules.Realization;

import NLPModules.ContentDetermination.*;
import java.util.List;
import java.util.LinkedList;
import NLPModules.ReferenceGeneration.*;
import NLPModules.ReferenceResolution.*;

public class Generation {

	public List<String> utterances = new LinkedList<String>();
	
	public Generation(CompleteUtterances complete_utterances){
		
		for (Utterance utterance:complete_utterances.semantics){
			String answer;
			if (utterance.hascomplexargument()){
				//If the utterance has a complex literal it can be assumed that is the first
				//(because of the design of the content determination module)
				Literal predicate = utterance.content_toassert.remove(0);
				String name = predicate.predicate;
				if (name.equals("hasdetail")){
					name = "has";
				}
				List arguments = predicate.arguments;
				//The literal has 2 arguments and the second one is a list
				String individual = arguments.get(0).toString();
				List<String> role_fillers= (List<String>) arguments.get(1);
				
				try{
					// generate the referring expression of the subject
					List<Literal> re = utterance.getReferringExpression(individual);
					answer = generateNP(re);
					
					//add verb
					answer = answer + " " + name;
					
					//add conjunction of objects
					int i = 0;
					for (String object:role_fillers){
						i = i + 1;
						List<Literal> re_object = utterance.getReferringExpression(object);
						answer = answer + " " + generateNP(re_object); 
							
						if (i < role_fillers.size()){
							if (i < (role_fillers.size()-1)){
								answer = answer + ",";
							}else{
								answer = answer + " and";
							}
						}
					}
				} catch (Exception e){
					System.err.println(e);
					answer = "I cannot express what I want to say.";
				}
			}else{
				try {		
					String semantics = utterance.toStringforGeneration();
					GeniSocket geniSocket = new GeniSocket();
					answer = utterance.canned_text + geniSocket.generate(semantics).get(0);
					if (utterance.has_predicate("you")){
						answer = answer.replaceAll("COPULA", "are");
					} else {
						answer = answer.replaceAll("COPULA", "is");
					}
					
				} catch (Exception e) {
					System.err.println(e);
					answer = "I cannot express what I want to say.";
				}	
			}
			
			this.utterances.add(answer);
		}		
	}
	
	public String toString(){
		String res = "";
		for (String utterance:this.utterances){
			res += "Frolog: " + utterance + "\n";
		}
		return res;
	}
	
	public static String generateNP(List<Literal> referring_expression){
		Utterance re_subject = new Utterance(referring_expression);
		
		String answer = "";
		String re_subject_string = re_subject.toStringforGeneration();
		try{
			GeniSocket geniSocket = new GeniSocket();
			List<String> geni_answer = geniSocket.generate(re_subject_string);
			int i = 0;
			while ((i<geni_answer.size()) && (answer.equals(""))){
				if (!(geni_answer.get(i).contains("COPULA"))){
					answer = geni_answer.get(i);
				}
				i = i + 1;
			}
			if (answer.equals("")){
				Exception e = new Exception();
				throw e;
			}
		} catch (Exception e){
			answer = re_subject.toString();
			System.err.println("I could not generate" + re_subject.toString());
		}
		return answer;
	}
	
}

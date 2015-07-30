package NLPModules.ContentDetermination;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;
import java.util.LinkedList;
import Tools.JPL_wrapper;

import jpl.Query;

import NLPModules.ReferenceResolution.*;

public class Description {
	
	public List<Utterance> utterances = new LinkedList<Utterance>();
	
	public Description(){
		super();
	}
	
	public void describe_here(){

		Query prolog_describe = new Query("describe:describe(D).");
		Hashtable result_query = prolog_describe.oneSolution();
		jpl.Compound raw_description = (jpl.Compound) result_query.get("D");
		List<jpl.Term> list_raw_description = Arrays.asList(raw_description.toTermArray());
		
		//The first and second element of the list are the contents of the location of the player 
		//and the container of the location, respectively. If the container is not a room then 
		//the next two elements (fourth and fifth) will be location and container and so on. 
		int i = 0;
		jpl.Term raw_contents = list_raw_description.get(i);
		while (raw_contents.name().equals("hold")){
			this.parse_roles(raw_contents);
//			i = i+1;
//			jpl.Term raw_container = list_raw_description.get(i);
//			this.parse_roles(raw_container);
			i = i+1;
			raw_contents = list_raw_description.get(i);
		} 
		this.parse_description_thing(list_raw_description,i);
	}
	

	public void describe_thing(String thing){
		Query prolog_describe = new Query("describe:describe(" + thing + ",D).");
		Hashtable result_query = prolog_describe.oneSolution();
		jpl.Compound raw_description = (jpl.Compound) result_query.get("D");
		List<jpl.Term> list_raw_description = Arrays.asList(raw_description.toTermArray());
		this.parse_description_thing(list_raw_description,0);
	}

	public void describe_contents(String thing){
		Query prolog_describe = new Query("describe:getContentsRole(" + thing + ",D).");
		Hashtable result_query = prolog_describe.oneSolution();
		jpl.Term term = (jpl.Term) result_query.get("D");
		this.parse_roles(term);	
	}
	
	private void parse_description_thing(List<jpl.Term> description,int i){
		List<jpl.Term> properties = Arrays.asList(description.get(i).toTermArray());
		if (!properties.isEmpty()){
//			List<String> realization_classes = new LinkedList<String>();
//			realization_classes.add("n0A1");
			List<Literal> list_properties = JPL_wrapper.listTermtolistLiteral(properties);
			Utterance properties_utterance = new Utterance(list_properties,"");
			this.utterances.add(properties_utterance);
		}
		jpl.Term raw_contents = description.get(i+1);
		this.parse_roles(raw_contents);
		jpl.Term raw_exits = description.get(i+2);
		this.parse_roles(raw_exits);
		jpl.Term raw_parts = description.get(i+3);
		this.parse_roles(raw_parts);
	}
	
	private void parse_roles(jpl.Term role){
		jpl.Term fillers_term = role.arg(2);
		jpl.Term[] fillers = fillers_term.toTermArray();
		if (fillers.length != 0) {
			List<Literal> list = new LinkedList<Literal>();
			Literal literal;
			if (fillers.length == 1){
				jpl.Term filler = fillers[0];
				jpl.Term individual = role.arg(1);
				jpl.Term[] arguments = {individual,filler};
				literal = new Literal(role.name(),arguments);
			} else{
				literal = new Literal(role.name(),role.args());
			}
			list.add(literal);
			Utterance utterance = new Utterance(list,"");
			this.utterances.add(utterance);
		}
	}
	
}


package NLPModules.Parsing;

import java.util.LinkedList;
import java.util.List;
import de.tuebingen.tag.*;
import NLPModules.ReferenceResolution.*;

public class Argument {
	
	public String article;
	public List<Literal> properties;
	
	public Argument() {
		super();
		
		this.article = "no";
		this.properties = new LinkedList<Literal>();
	}

}

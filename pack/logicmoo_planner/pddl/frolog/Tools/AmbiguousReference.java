package Tools;

import java.util.List;
import NLPModules.ReferenceResolution.*;

public class AmbiguousReference extends FrologException {
	
	public List<String> referents;

	public AmbiguousReference(String type, String arg0, List<String> referents) {
		super(type, arg0);
		this.referents = referents;
	}

}

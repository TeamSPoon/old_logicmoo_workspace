/*
 * Created on 10-Mar-2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package jplan.tools.lifeHist;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.text.html.*;
/**
 * @author ron
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class EnlargedHTMLEditorKit  extends HTMLEditorKit{
	public Object clone() {
		return new EnlargedHTMLEditorKit();
	}

	public Action[] getActions() {
		return TextAction.augmentList(super.getActions(), extraActions);
	}

	private static final InsertHTMLTextAction[] extraActions = 
		new InsertHTMLTextAction[] {
			new InsertHTMLTextAction("Heading 1", "<h1>[H1]</h1>",
					HTML.Tag.BODY, HTML.Tag.H1),
			new InsertHTMLTextAction("Heading", "<h2><font color=red>[Heading]</font></h2>",
					HTML.Tag.BODY, HTML.Tag.H2),
			new InsertHTMLTextAction("Break", "<br>",
					HTML.Tag.BODY, HTML.Tag.BR),
			new InsertHTMLTextAction("Property Description", "<P><font color=red>Property</font> [Description]</P>",
					HTML.Tag.BODY, HTML.Tag.P),
	};
	
}

/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the names of
 * the authors or their employers not be used in advertising or publicity 
 * pertaining to distribution of the software without specific, written 
 * prior permission.
 *
 * The authors and their employers disclaim all warranties with regard to 
 * this software, including all implied warranties of merchantability and 
 * fitness.  In no event shall the authors or their employers be liable 
 * for any special, indirect or consequential damages or any damages 
 * whatsoever resulting from loss of use, data or profits, whether in an 
 * action of contract, negligence or other tortious action, arising out of 
 * or in connection with the use or performance of this software.
 */
package jplan.graphics.transition;
/**
 * PropertyButton.java
 *
 *
 * Created: Wed Apr 24 16:24:46 2002
 *
 * @author W Zhao
 * @version
 */
import javax.swing.*;
import jplan.ocl.*;

public class PropertyButton extends JPanel {
    JButton button;
    jplan.general.MethodVarPane methodVarPane;
    oclDomain domain;
    JDialog parentDig;

    public PropertyButton(JDialog parent, String name, jplan.general.MethodVarPane mp, oclDomain curDomain) {
	methodVarPane = mp;
	this.domain = curDomain;
	this.parentDig = parent;
	setLayout(new java.awt.GridLayout(1,2)); 
// 	setLayout(new BoxLayout(this,BoxLayout.X_AXIS));/* WZ 1/5/02 */
	button =  new JButton("  "+name);
// 	button.setPreferredSize(new java.awt.Dimension(120, methodVarPane.getSize().height));

	/* WZ 26/4/02 */
	Object obj = methodVarPane.getObject();
	if (obj != null){
	    String str = obj.getClass().getName();
	    if (str.equals("jplan.ocl.oclOperator")){
		button.setToolTipText("Click to show property");
		button.addActionListener (new java.awt.event.ActionListener () {
		    public void actionPerformed (java.awt.event.ActionEvent evt) {
			OperatorPropertyWindow opw = new OperatorPropertyWindow(parentDig, (oclOperator)methodVarPane.getObject(), domain);
		    }
		}
					  );
	    }
	    else if(str.equals("jplan.ocl.oclMethod")){
		button.setToolTipText("Click to show property");
		button.addActionListener (new java.awt.event.ActionListener () {
		    public void actionPerformed (java.awt.event.ActionEvent evt) {
			OperatorPropertyWindow opw = new OperatorPropertyWindow(parentDig, (oclMethod)methodVarPane.getObject(), domain);
		    }
		}
					  );
	    }
	    else
		button.setEnabled(false);
	}
	else{
	    button.setEnabled(false);
	}
	add(button);
	add(methodVarPane);
    }

    /* WZ 29/4/02 */
    /**
     * to return MethodVarPane
     */
    public jplan.general.MethodVarPane getMethodVarPane(){
	return methodVarPane;
    }

} // PropertyButton

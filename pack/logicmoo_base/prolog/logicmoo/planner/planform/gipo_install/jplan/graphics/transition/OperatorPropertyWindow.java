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
 * OperatorPropertyWindow.java
 *
 *
 * Created: Wed Apr 24 15:27:01 2002
 *
 * @author W Zhao
 * @version
 */
import jplan.ocl.*;

public class OperatorPropertyWindow extends javax.swing.JDialog {
    
    public OperatorPropertyWindow(javax.swing.JDialog parent, oclOperator operator, jplan.ocl.oclDomain curDomain) {
	super(parent);
	setModal(true);

	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );

	OperatorPropertyCanvas opCanvas = new OperatorPropertyCanvas(curDomain);
	opCanvas.setBorder(new javax.swing.border.BevelBorder(1));
	getContentPane().add(opCanvas);
	ActionWindow.showOperatorGraphics(operator, opCanvas);

	pack();
	setSize(500, 180);
	setLocation(getToolkit().getScreenSize().width/2 - getBounds().width/2, getToolkit().getScreenSize().height/2 - getBounds().height/2);
	setVisible(true);
    }
    
    public OperatorPropertyWindow(javax.swing.JDialog parent, oclMethod method, jplan.ocl.oclDomain curDomain) {
	super(parent);
	setModal(true);

	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );

	MethodPropertyCanvas opCanvas = new MethodPropertyCanvas(curDomain);
	opCanvas.setBorder(new javax.swing.border.BevelBorder(1));
	getContentPane().add(opCanvas);
	HighLevelTransitionWindow.showOclMethod(true, method, opCanvas);

	pack();
	setSize(500, 180);
	setLocation(getToolkit().getScreenSize().width/2 - getBounds().width/2, getToolkit().getScreenSize().height/2 - getBounds().height/2);
	setVisible(true);
    }

    public OperatorPropertyWindow(javax.swing.JDialog parent, oclSS ss, jplan.ocl.oclDomain curDomain) {
	super(parent);
	setModal(true);

	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );

	OperatorPropertyCanvas opCanvas = new OperatorPropertyCanvas(curDomain);
	opCanvas.setBorder(new javax.swing.border.BevelBorder(1));
	getContentPane().add(opCanvas);
	MethodHeadCanvas.showOclSS(opCanvas,ss);

	pack();
	setSize(500, 180);
	setLocation(getToolkit().getScreenSize().width/2 - getBounds().width/2, getToolkit().getScreenSize().height/2 - getBounds().height/2);
	setVisible(true);
    }

    /** 
     * close this dialog window
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }
} // OperatorPropertyWindow

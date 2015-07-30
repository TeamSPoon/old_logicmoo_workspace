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

package jplan.tools.animator;

/**
 * OperatorProperty.java
 * @author Weihong Zhao
 * 05/07/2001
 */


import jplan.ocl.*;
import java.io.*;

import java.awt.event.*;
import jplan.general.UnderlineHighlighter;
import javax.swing.text.Highlighter;
import javax.swing.plaf.basic.BasicComboPopup;
import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;
import jplan.general.OPredicate;  
import jplan.images.ImageLoader; 	/* Weihong changed on 5/9/2001 */
import jplan.general.GipoInternalFrame;/* Weihong added on 1/11/01 */


/**
 * to show the operators property only without any function of instantiation of the operators.
 */
public class OperatorProperty extends javax.swing.JDialog {
    
    // Variables declaration
    private GipoInternalFrame parent;/* Weihong added on 1/11/01 */
    private javax.swing.JToolBar northToolBar;
    private javax.swing.JButton jbn_OK;
    private javax.swing.JTextField briefArea = new javax.swing.JTextField("briefArea");
    private javax.swing.JTextArea detailedArea =  new javax.swing.JTextArea("detailedArea");
    private javax.swing.JButton downArrow;
    private javax.swing.JButton upArrow;
   
    private oclDomain curDom;
    private oclOperator operator;
    private oclPredicate briefAreaPred;
    private int ArgNo = -1; //the index of the sort/varible in a predicate - briefAreaPred
    private UnderlineHighlighter highlighter = null;
    private String objname = null;
    private OPredicate.pArg selectedPArg;
    private String curPath;



  /** 
   * Creates an instance of the OperatorProperty dialog window
   * @param curDom oclDomain
   * @param operator the operator which content requires to display
   * @param parent parent frame
   * @param curPath the path to get images from the image store
   */
    public OperatorProperty(oclDomain curDom, oclOperator operator,
			    GipoInternalFrame parent, String curPath) {
	super();
	setTitle("Operator Property Window");
	this.parent = parent;
	this.curDom = curDom;
	this.operator = operator;
	this.curPath = curPath;
	initComponents ();
	pack ();
    }
    
    /** 
     * Initialisation
     * 
     */
    private void initComponents () {
	setBackground (new java.awt.Color (114, 159, 255));
	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );
	getContentPane ().setLayout (new java.awt.BorderLayout ());
	
	briefArea.setEditable(false);
	briefArea.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16));
	briefAreaPred = operator.opName;
	briefArea.setText(briefAreaPred.toString());
	getContentPane ().add (briefArea, "North");
	
	showOperatorDetail();
	detailedArea.setLineWrap(true);
	detailedArea.setEditable(false);
	detailedArea.setVisible(true);
	detailedArea.setSize(500, 300);/* Weihong changed on 10/10/2001 */
	detailedArea.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 11));
	getContentPane ().add (detailedArea, "Center");

	detailedArea.setBorder (new javax.swing.border.BevelBorder(1));
	
	northToolBar = new javax.swing.JToolBar ();
	northToolBar.setLayout (new java.awt.FlowLayout ());
	northToolBar.setFloatable(false);	/* Weihong added on 12/10/2001 */
	
	jbn_OK = new javax.swing.JButton ();
	jbn_OK.setText ("   OK   ");
	jbn_OK.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_OKActionPerformed ();
	    }
	}
				  );
	northToolBar.add (jbn_OK);
	
	getContentPane ().add (northToolBar, "South");
    }
    
    /** 
     * display operator details (OCLh language format) to a JTextArea
     * 
     */
    private void showOperatorDetail(){
	StringWriter opDetail = new StringWriter();
	operator.oclPrintComponent(new PrintWriter (opDetail) ,0,false);
	detailedArea.setText(new String(opDetail.getBuffer()));
    }
    
    /** 
     * when ok button to be pressed, close window only.
     * 
     */
    private void jbn_OKActionPerformed () {
	closeDialog();
    }
    
    /** 
     * close window
     * 
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }   
}

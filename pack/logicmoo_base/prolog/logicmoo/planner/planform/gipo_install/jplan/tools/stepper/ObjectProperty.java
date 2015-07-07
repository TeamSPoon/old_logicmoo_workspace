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

package jplan.tools.stepper;

/**
 * ObjectProperty.java
 * @author Weihong Zhao
 * 07/06/2001
 */


import jplan.ocl.*;

import java.awt.event.*;
import javax.swing.*;
import java.util.*;
import java.util.List;


/**
 * to show the state property of an object
 */
public class ObjectProperty extends javax.swing.JDialog {
    
    // Variables declaration
    private JToolBar northToolBar;
    private JButton jbn_OK;
    private JLabel briefArea;
    private JList detailedArea;
    
    private List stateList;
    private String object;
    private DefaultListModel lmStates; 
    private JScrollPane jscrollState;

  /** 
   * Creates an instance of the OperatorInstantiation dialog window
   * @param owner parent frame
   * @param stateList a list of states of the current object
   * @param object name of the object which property is displaying.
   */
    public ObjectProperty(JFrame owner, List stateList, String object) {
	super(owner);
	setTitle("Object's State Property Window");
	this.stateList = stateList;
	this.object = object; //lists of oclObject
	initComponents ();
	pack ();
	setSize(400, 150);
	repaint();
    }
    
     /** 
     * Initialisation
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
	briefArea = new JLabel("Current State of Object '"+object+"'");
	briefArea.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16));
	getContentPane ().add (briefArea, "North");
	
	//show state property
	lmStates = new DefaultListModel();
	ListIterator li = stateList.listIterator();
	while (li.hasNext()){
	    lmStates.addElement((oclPredicate)li.next());
	}
	detailedArea = new JList(lmStates);
	detailedArea.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 11));
	jscrollState = new JScrollPane(detailedArea);
	getContentPane ().add (jscrollState, "Center");
	
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
    
    /*
     * when ok button is pressed, close Window only.
     * @return void
     */
    private void jbn_OKActionPerformed () {
	closeDialog();
    }
    
    /*
     * close dialog window
     * @return void
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }
    
}

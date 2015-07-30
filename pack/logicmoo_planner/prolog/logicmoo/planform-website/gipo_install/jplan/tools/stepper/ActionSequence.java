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
 * ActionSequence.java
 * @author Weihong Zhao
 * 27/06/2002
 */
import java.io.*;
import jplan.ocl.*;
import javax.swing.*;
import java.util.*;
import java.util.List;
import jplan.general.HardcopyWriter; 

/**
 * to show the all state property
 */
public class ActionSequence extends javax.swing.JDialog {
    
    // Variables declaration
    private JToolBar northToolBar;
    private JButton jbn_OK;
    private JButton jbn_Print;
    private JTextArea detailedArea =  new JTextArea("detailedArea");
    private List actionSequence;
    private oclTask curTask;
    private oclDomain curDomain;
    private JFrame owner;
  /** 
   * Creates an instance of the OperatorInstantiation dialog window
   * @param owner parent frame
   * @param actionSequence action sequence list
   * @param curTask
   * @param curDomain
   */
    public ActionSequence(JFrame owner, List actionSequence, oclTask curTask, oclDomain curDomain) {
	super(owner);
	setModal(true);
	setTitle("Action Sequence Window");
	this.actionSequence = actionSequence;
	this.curTask = 	curTask;
	this.curDomain = curDomain;
	this.owner = owner;
	initComponents ();
	pack ();
	setSize(400, 300);
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

	JPanel actionPanel = new JPanel();
	actionPanel.setLayout(new java.awt.GridLayout(1, 0));
	detailedArea.setEditable(false);
	detailedArea.setVisible(true);
	detailedArea.setSize(500, 300);
	detailedArea.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 11));
	StringWriter opDetail = new StringWriter();
	actionPrint(new PrintWriter (opDetail));
	detailedArea.setText(new String(opDetail.getBuffer()));
	JScrollPane scroll = new JScrollPane(detailedArea);
	actionPanel.add(scroll);
	getContentPane ().add (actionPanel, "Center");

	northToolBar = new javax.swing.JToolBar ();
	northToolBar.setLayout (new java.awt.FlowLayout ());
	northToolBar.setFloatable(false);

	jbn_Print = new javax.swing.JButton ();
	jbn_Print.setText ("   Print   ");
	jbn_Print.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_PrintActionPerformed ();
	    }
	}
				  );
	northToolBar.add (jbn_Print);

	jbn_OK = new javax.swing.JButton ();
	jbn_OK.setText ("   Close   ");
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
     * 
     */
    private void jbn_OKActionPerformed () {
	closeDialog();
    }
    
    /**
     * print action text
     * 
     */
    private void jbn_PrintActionPerformed () {
	String header = "Gipo - Domain [" + curDomain.getName()
	    + "] Author [" +curDomain.getAuthor() +"]"; 
	HardcopyWriter hw;
	try { 
	    hw=new HardcopyWriter(owner, header,10,.75,.5,.75,.5);}
	catch (HardcopyWriter.PrintCanceledException e) {
	    JOptionPane.showMessageDialog(this,
					  "Error Printing Action Sequence.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);
	    return;  
	}
            
	// Send output to it through a PrintWriter stream
	PrintWriter out = new PrintWriter(hw);
	actionPrint(out);
	out.close();
    }

    /* 
     * Print the action sequence (Probably) to the printer
     */
    public void actionPrint(PrintWriter ps) {
	int i = 0;
	ps.println("Stepper - action sequence");
	ps.println("Task ID - "+curTask);
	ps.println();
	ListIterator li = actionSequence.listIterator();
	while (li.hasNext()) {
	    i++;
	    oclPredicate pred = (oclPredicate)li.next();
	    ps.println(i + ": " + pred.toString());
	}
    }

    /*
     * close dialog window
     * 
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }
    
    /**
     * factory method to create and display box
     * @param parent owner
     * @param actionList action sequence list
     * 
     */
    public static void showActionSequence(JFrame parent, List actionList, oclTask curTask, oclDomain curDomain){
	ActionSequence pw = new ActionSequence(parent, actionList, curTask, curDomain);
	pw.setLocation((int) (0.5 * parent.getWidth()),(int) (0.5 * parent.getHeight()));
	pw.show();
    }

}

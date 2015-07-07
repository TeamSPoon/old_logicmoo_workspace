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

package jplan.general;

/**
 * GipoInputBox.java
 *
 *
 * Created: Mon Aug 20 10:49:22 2001
 *
 * @author Weihong Zhao
 * @version 1.0
 */

import javax.swing.text.*;
import javax.swing.*;
import java.awt.*;
import jplan.general.OCLVariableDocument;  



public class GipoInputBox extends JDialog {
    
    private JLabel prompText;
    private JLabel rulesText;
    private JTextField inputArea = new JTextField(20);
    private String returnText = new String("");
    public boolean clicked = false;



    /**
     * forces OCL variable rules when entering text
     * @param owner top level frame generating this dialog
     * @param title the title for the box
     * @param prompText prompt question for user
     * @param docum document for textfield to enforce its own input rules
     */
    public GipoInputBox(Frame owner, String title, 
			String prompText, String rules,
			PlainDocument docum) { 
	super(owner);
	setTitle(title);
	setModal(true);
	this.prompText = new JLabel(prompText);
	this.inputArea.setDocument(docum);
	this.rulesText = new JLabel(rules);
	initComponents ();
	pack ();
	setPosition(owner);
	setSize(300, 150);
    }

    /**
     * Initialisation
     * 
     */
    private void initComponents () {
	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );

	getContentPane ().setLayout (new java.awt.BorderLayout ());

	JToolBar northToolBar = new JToolBar ();
	northToolBar.setLayout (new FlowLayout ());
	northToolBar.setFloatable(false); 	/* Weihong changed/added on 8/11/2001 */
	
	JButton jbn_OK = new JButton ();
	jbn_OK.setText ("   OK   ");
	jbn_OK.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_OKActionPerformed ();
	    }
	}
				  );
	northToolBar.add (jbn_OK);
	
	JButton jbn_Cancel = new JButton ();
	jbn_Cancel.setText ("  Cancel  ");
	jbn_Cancel.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_CancelActionPerformed ();
	    }
	}
				      );
	northToolBar.add (jbn_Cancel);

	JPanel topPane = new JPanel();
	topPane.setLayout(new BorderLayout());
	/* Weihong changed/added on 3/9/2001 */
	JPanel P1 = new JPanel(); 
	P1.setLayout(new FlowLayout());
	P1.add(prompText);
	topPane.add(P1,"North");	
	/* Weihong changed/added end */

	Font rf = rulesText.getFont();
	Font newrf = new Font(rf.getName(),
			      Font.ITALIC,
			      rf.getSize());
	rulesText.setFont(newrf);
	/* Weihong changed/added on 3/9/2001 */
	JPanel P2 = new JPanel(); 
	P2.setLayout(new FlowLayout());
	P2.add(rulesText);
	topPane.add(P2,"South");	
	/* Weihong changed/added end */

	prompText.setSize(200,80);

	JPanel centerPane = new JPanel();
	centerPane.setLayout(new FlowLayout());
	centerPane.add(inputArea);
	inputArea.setSize(100, 60);

	getContentPane ().add (northToolBar, "South");
	getContentPane ().add (topPane, "North");
	getContentPane ().add (centerPane, "Center");
    }

    /* Weihong changed/added on 3/9/2001 */
    /**
     * Sets default text for the input box
     * @param text default text for the input box
     * 
     */
    public void setDefaultText(String text){
	inputArea.setText(text);
    }

    /**
     * Sets position of the input box
     * @param owner parent frame
     * 
     */
    private void setPosition(Frame owner){
	if (owner == null) {
	    return;
	}
	int X = (owner.getX() + (owner.getWidth() - getWidth())/2);
	int Y = (owner.getY() + (owner.getHeight() - getHeight())/2);
	if (owner.getWidth() < getWidth()) {
	    X = owner.getX();
	}
	if (owner.getHeight() < getHeight()) {
	    Y = owner.getY();
	}
	setLocation(X,Y);
    }

    /**
     * when the ok button is pressed, get the input text
     * 
     */
    private void jbn_OKActionPerformed (){
	if (inputArea.getText().length() == 0) 	    
	    returnText =  null;
	else
	    returnText = inputArea.getText();

	clicked = true;
	closeDialog();
    }

    /**
     * factory method to create and display box with nforced variable rules
     * @param owner top level frame generating this dialog
     * @param title the title for the box
     * @param prompText prompt question for user
     * @return String the input string
     */
    public static String showVarableInputBox(Frame owner,String title, 
    String prompText){
	OCLVariableDocument varDoc = new OCLVariableDocument();
	GipoInputBox inputBox = 
	    new GipoInputBox(owner, title, prompText, 
			     "Captial followed by alphanumeric",
			     varDoc);
	inputBox.show();
	return inputBox.getInput();
    }

    /**
     * factory method to create and display box with enforced variable rules; 
     * provides default title
     * @param owner top level frame generating this dialog
     * @param prompText prompt question for user
     * @return String the input string
     */
    public static String showVarableInputBox (Frame owner,String prompText){
	OCLVariableDocument varDoc = new OCLVariableDocument();
	GipoInputBox inputBox = 
	    new GipoInputBox(owner,"GIPO input", prompText,
			     "Captial followed by alphanumeric", varDoc);
	inputBox.show();
	return inputBox.getInput();
    }

    /**
     * factory method to create and display box with enforced identifier rules
     * @param owner top level frame generating this dialog
     * @param title the title for the box
     * @param prompText prompt question for user
     * @return String the input string
     */
    public static String showIdentifierInputBox(Frame owner,String title, 
    String prompText){
	OCLIdentifierDocument identDoc = new OCLIdentifierDocument();
	GipoInputBox inputBox = 
	    new GipoInputBox(owner, title, prompText, 
			     "Lower case followed by alphanumeric",identDoc);
	inputBox.show();
	return inputBox.getInput();
    }

    /* Weihong changed/added on 3/9/2001 */
    /**
     * showIdentifierInputBox with default text
     * factory method to create and display box
     * Enforces identifier rules
     * @param owner - top level frame generating this dialog
     * @param title - the title for the box
     * @param prompText - prompt question for user
     * @return String - the input string
     */
    public static String showIdentifierInputBox(Frame owner,String title, 
    String prompText, String sefaultText){
	OCLIdentifierDocument identDoc = new OCLIdentifierDocument();
	GipoInputBox inputBox = 
	    new GipoInputBox(owner, title, prompText, 
			     "Lower case followed by alphanumeric",identDoc);
	inputBox.setDefaultText(sefaultText);
	inputBox.show();
	return inputBox.getInput();
    }


    /**
     * factory method to create and display box with enforced identifier rules;
     * provides default title
     * @param owner top level frame generating this dialog
     * @param prompText prompt question for user
     * @return String the input string
     */
    public static String showIdentifierInputBox (Frame owner,String prompText){
	OCLIdentifierDocument identDoc = new OCLIdentifierDocument();
	GipoInputBox inputBox = 
	    new GipoInputBox(owner,"GIPO input", prompText,
			     "Lower case followed by alphanumeric", identDoc);
	inputBox.show();
	return inputBox.getInput();
    }

    /**
     * showIntegerInputBox
     * factory method to create and display box
     * Enforces integer rules
     * @param owner - top level frame generating this dialog
     * @param title - the title for the box
     * @param prompText - prompt question for user
     * @return String - the input string
     */
    public static String showIntegerInputBox(Frame owner,String title, 
    String prompText){
	OCLIntegerDocument doc = new OCLIntegerDocument();
	GipoInputBox inputBox = 
	    new GipoInputBox(owner, title, prompText, 
			     "Integer only", /* Weihong changed/added on 6/9/2001 */
			     doc);
	inputBox.show();
	return inputBox.getInput();
    }
    
	// Ron 3/07/03 added for oclPlus
	/**
	 * showDecimalInputBox
	 * factory method to create and display box
	 * Enforces integer rules
	 * @param owner - top level frame generating this dialog
	 * @param title - the title for the box
	 * @param promptText - prompt question for user
	 * @return String - the input string
	 */
	public static String showDecimalInputBox(
		Frame owner,
		String title,
		String prompText) {
		oclDecimalDocument doc = new oclDecimalDocument();
		GipoInputBox inputBox =
			new GipoInputBox(owner, title, prompText, "Decimal only", doc);
		inputBox.show();
		return inputBox.getInput();
	}

    /**
     * factory method to create and display box with enforced integer rules;
     * provides default title
     * @param owner top level frame generating this dialog
     * @param prompText prompt question for user
     * @return String the input string
     */
    public static String showIntegerInputBox (Frame owner,String prompText){
	OCLIntegerDocument doc = new OCLIntegerDocument(); /* Weihong changed/added on 6/9/2001 */
	GipoInputBox inputBox = 
	    new GipoInputBox(owner,"GIPO input", prompText,
			     "Integer only", doc); /* Weihong changed/added on 6/9/2001 */
	inputBox.show();
	return inputBox.getInput();
    }


    /**
     * factory method to create and display box
     * @param owner top level frame generating this dialog
     * @param title the title for the box
     * @param prompText prompt question for user
     * @param rulesText the imput format rules
     * @param docum Document for textfield to enforce its own input rules
     * @return String the input string
     */
    public static String showInputBox(Frame owner,String title, String prompText, String rulesText,PlainDocument docum){
	GipoInputBox inputBox = new GipoInputBox(owner, title, prompText, 
						 rulesText, docum);
	inputBox.show();
	return inputBox.getInput();
    }


    /**
     * factory method to create and display box;
     * provides default title
     * @param owner top level frame generating this dialog
     * @param prompText prompt question for user
     * @param rulesText the imput format rules
     * @param docum Document for textfield to enforce its own input rules
     * @return String the input string
     */
    public static String showInputBox(Frame owner,String prompText,
				      String rulesText,
				      PlainDocument docum){
	GipoInputBox inputBox = 
	    new GipoInputBox(owner, "GIPO Input",prompText, rulesText, docum);
	inputBox.show();
	return inputBox.getInput();
    }

    /**
     * cancel the box
     * 
     */
    private void jbn_CancelActionPerformed (){
	returnText =  null;
	closeDialog();
	clicked = true;
	synchronized(inputArea){
	    inputArea.notifyAll();
	}
    }

    /**
     * Returns the input text
     * @return String the input string
     */
    public String getInput(){
	return returnText;
    }

    /**
     * Close dialog window
     * 
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }

} // GipoInputBox

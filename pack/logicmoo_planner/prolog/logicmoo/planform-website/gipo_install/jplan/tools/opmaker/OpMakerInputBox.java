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

package jplan.tools.opmaker;

import java.util.*;
import jplan.ocl.*;



import javax.swing.text.*;
import javax.swing.*;
import java.awt.*;
import jplan.general.OCLVariableDocument;  
import jplan.general.*;


/**
 * OpMakerInputBox.java
 * This input box gathers the information to confirm the details
 * of a transition
 *
 * @author Ron Simpson
 * @version 1.0
 */
public class OpMakerInputBox extends JDialog {
    

    private JTextPane promptText;
    private OpMakerData retData = null;
    private java.util.List post = null;
    private OPredicate.pArg object = null;
    private oclSS pre = null;
    private oclPredicate action = null;
    private boolean hasGeneralisation = false;
    public boolean clicked = false;
    private JCheckBox cond= null;
    private JCheckBox prevail = null;
    private JCheckBox gen = null;
    private JComboBox states = null;
    private JComboBox genStates = null;

    /**
     * @param owner top level frame generating this dialog
     * @param object the parameter of the action name relating to the 
     *               transition being defined
     * @param action the complete action signature
     * @param pre the lhs already deduced
     * @param stateList the list of possible rhs state definitions
     */
    public OpMakerInputBox(Frame owner, OPredicate.pArg object, 
			   oclPredicate action , oclSS pre,
			   java.util.List stateList) { 
	super(owner);
	setTitle("Op Maker Input");
	setModal(true); 
// 	String prompt = "Select a state for " + object.name + " " +
// 	    "that results from applying the action\n" +
// 	    action.toString() + "\n" +
// 	    object.name + " begins the action in state " +
// 	    pre.toString() + "\n";
// 	promptText = new JTextArea(prompt);
// 	promptText.setEditable(false);
// 	JLabel dummy = new JLabel("xxx");
// 	promptText.setBackground(dummy.getBackground());
	post = stateList;
	this.object = object;
	this.pre = pre;
	this.action = action;
	initComponents ();
	pack ();
	setPosition(owner);
	if (hasGeneralisation)
	    setSize(560,270);
	else
	    setSize(560, 220);
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
	// Setup the prompt text
	StyleContext context = new StyleContext();
	Style plain = context.getStyle(StyleContext.DEFAULT_STYLE);
	StyleConstants.setFontSize(plain,12);
	StyleConstants.setBold(plain,true);
	Style stateStyle = context.addStyle("Static",null);
	StyleConstants.setFontSize(stateStyle,14);
	StyleConstants.setForeground(stateStyle,Color.red);
	StyleConstants.setBold(stateStyle,true);
	//StyleConstants.setItalic(stateStyle,true);
	StyledDocument document = new DefaultStyledDocument(context);
	try {
	    document.insertString(document.getLength(),
				  "Select a state for ",plain);
	    document.insertString(document.getLength(),
				  object.name,stateStyle);
	    document.insertString(document.getLength(),
				  " that results from applying the action\n",
				  plain);
	    document.insertString(document.getLength(),
				  action.toString() + "\n" +
				  object.name,stateStyle);
	    document.insertString(document.getLength(),
				  " begins the action in state ",plain);
	    document.insertString(document.getLength(),
				  pre.toSugarString(),stateStyle);
	} catch (BadLocationException e) {
	    System.err.println("Error cannot insert prompt text [OpMakerInputBox]");
	}
	promptText = new JTextPane(document);
	promptText.setEditable(false);
 	JLabel dummy = new JLabel("xxx");
 	promptText.setBackground(dummy.getBackground());

	JPanel P1 = new JPanel(); 
	P1.setLayout(new BorderLayout());
	P1.add(promptText,"North");	

	JPanel postPan = new JPanel();
	postPan.setLayout(new FlowLayout());
	java.util.List otherStates = filterStates(pre,post,action);
	states = new JComboBox(otherStates.toArray());
	postPan.add(new JLabel("End State >> "));
	
	postPan.add(states);
 	P1.add(postPan,"South");
 	topPane.add(P1,"Center");

	JPanel P2 = new JPanel();
	JPanel P2inner = new JPanel();
	P2.setLayout(new BorderLayout());
	P2inner.setLayout(new BorderLayout());
	String condPrompt = "Changes all objects of sort " + object.sort +
	    " in the same prior state";
	cond = new JCheckBox(condPrompt);
	String prevailPrompt = object.name + " does not change state in this action.";
	prevail = new JCheckBox(prevailPrompt);
	P2inner.add(prevail,"North");
	P2inner.add(cond,"Center");

	java.util.List genStateList = generalise(pre,post);
	if (genStateList.size() > 0) {
	    hasGeneralisation = true;
	    JPanel genPan = new JPanel();
	    genPan.setLayout(new BorderLayout());
	    String genPrompt = "Generalise the precondition to ";
	    gen = new JCheckBox(genPrompt);
	    genStates = new JComboBox(genStateList.toArray());
	    genPan.add(gen,"West");
	    genPan.add(genStates,"East");
	    P2inner.add(genPan,"South");
	} else {
	    hasGeneralisation = false;
	}
	P2.add(P2inner,"South");
	topPane.add(P2,"South");
	

// 	prompText.setSize(200,80);

// 	JPanel centerPane = new JPanel();
// 	centerPane.setLayout(new FlowLayout());
// 	centerPane.add(inputArea);
// 	inputArea.setSize(100, 60);

	getContentPane ().add (northToolBar, "South");
 	getContentPane ().add (topPane, "North");
// 	getContentPane ().add (centerPane, "Center");
    }


    /**
     * Factory method to create the input box
     * @param owner top level frame generating this dialog
     * @param object the parameter of the action name relating to the 
     *               transition being defined
     * @param action the complete action signature
     * @param pre the lhs already deduced
     * @param stateList the list of possible rhs state definitions
     */
    public static OpMakerData getOpMakerInput(Frame owner, 
					      OPredicate.pArg object, 
					      oclPredicate action , oclSS pre,
					      java.util.List stateList){
	OpMakerInputBox inputBox = 
	    new OpMakerInputBox(owner, object, action, pre, stateList);
	inputBox.show();
	return inputBox.getInput();
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
	oclStateList state = null;
	if (!prevail.isSelected()) {
	    state = (oclStateList)states.getSelectedItem();
	}
	java.util.List genState = null;
	boolean isGeneralised = false;
	if (hasGeneralisation && gen.isSelected()) {
	    genState = (java.util.List)genStates.getSelectedItem();
	    isGeneralised = true;
	}
	retData = new OpMakerData(prevail.isSelected(),
				  cond.isSelected(),
				  isGeneralised,
				  state,
				  genState
				  );
	clicked = true;
	closeDialog();
    }



    /**
     * cancel the box
     * 
     */
    private void jbn_CancelActionPerformed (){
	retData =  null;
	closeDialog();
	clicked = true;
// 	synchronized(inputArea){
// 	    inputArea.notifyAll();
// 	}
    }

    /**
     * Returns the input text
     * @return the collected user input
     */
    public OpMakerData getInput(){
	return retData;
    }

    /**
     * Close dialog window
     * 
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }

    /**
     * filterStates
     * remove the state contained in the given ss expression
     * from the state list
     * ONLY do so if it cannot take on another value i.e. no duplicate
     * argument of the same sort in the action name
     * @param target - the ss expression
     * @param stateList - the state list
     * @param action - the action signature
     * @return the filtered list
     */
    private java.util.List filterStates(oclSS target, 
					java.util.List stateList,
					oclPredicate action) {
	java.util.List res = new ArrayList(stateList.size() - 1);
	ListIterator li = stateList.listIterator();
	boolean found = false;
	while(li.hasNext()) {
	    oclStateList state = (oclStateList)li.next();
	    if (!found) {
		if (target.getState().size() != 
		    state.getPredicateList().size()) {
		    // This is not it 
		    res.add(state);
		} else {
		    // Same size may be it
		    ListIterator liPreds = target.getState().listIterator();
		    boolean ok = true;
		    while (ok && liPreds.hasNext()) {
			oclPredicate cur = (oclPredicate)liPreds.next();
			if (! Utility.memberPred(cur,state)) {
			    ok = false;
			}
		    }
		    if (!ok) {
			res.add(state);
		    } else {
			found = true;
			if (valuesMayChange(state.getPredicateList()
					    ,action,target.getName())) {
			    res.add(state);
			}
		    }
		}
	    } else {
		res.add(state);
	    }
	}
	return res;
    }


    /**
     * valuesMayChange
     * check to see if an predicate argument in the state list has
     * potentially more than one value in the action signature
     * need to exclude the state ID as it cannot change value
     * @param state - the state list
     * @param action - the action signature
     * @param ID - the state ID
     * @return - true if multiple values
     */
    private boolean valuesMayChange(java.util.List state, 
				    oclPredicate action, String ID){
	// Collect sorts with more than one value in the action name
	java.util.List dups = new ArrayList();
	java.util.List work = new ArrayList();
	java.util.List args = action.getArguments();
	ListIterator li = args.listIterator();
	while (li.hasNext()) {
	    oclPredicate.pArg arg = (oclPredicate.pArg)li.next();
	    Utility.debugPrintln("opmaker","Check DUP " + arg.sort);
	    if (Utility.listContainsString(arg.sort,work)) {
		Utility.debugPrintln("opmaker","Check DUP in Work " + arg.sort);
		if (!Utility.listContainsString(arg.sort,dups)) {
		    dups.add(arg.sort);
		    Utility.debugPrintln("opmaker","Added DUP " + arg.sort);
		}
	    } else {
		work.add(arg.sort);
	    }
	}
	// Now check the predicates in the predicate list
	li = state.listIterator();
	while(li.hasNext()) {
	    oclPredicate curPred = (oclPredicate)li.next();
	    ListIterator liArgs = curPred.getArguments().listIterator();
	    while (liArgs.hasNext()) {
		oclPredicate.pArg argP = (oclPredicate.pArg)liArgs.next();
		if (!argP.name.equals(ID)) {
		    if (Utility.listContainsString(argP.sort,dups)) {
			return true;
		    }
		}
	    }
	}
	return false;
    }

    /**
     * generalise
     * find all states capable of being generalised with a subset of
     * the precondition state predicates
     * @param pre the transition instantiated precondition
     * @param stateListList the list of all possible states for the same sort
     * THIS LIST SHOULD BE FILTERED TO REMOVE THE STATE LIST ITSELF
     * @return list of generalised states 0 length if none
     */
    private java.util.List generalise(oclSS pre, java.util.List stateListList){
	java.util.List res = new ArrayList();
	java.util.List preState = pre.getState();
	if (preState.size() == 1) {
	    // no generalisation possible
	    return res;
	}
	java.util.List candidates = new ArrayList();
	ListIterator liOuter = preState.listIterator();
	while (liOuter.hasNext()) {
	    oclPredicate cur = (oclPredicate)liOuter.next();
	    ListIterator liInner = stateListList.listIterator();
	    boolean added = false;
	    while (!added && liInner.hasNext()) {
		oclStateList target = (oclStateList)liInner.next();
		if (Utility.memberPred(cur,target)) {
		    candidates.add(cur);
		    added = true; //Only add once
		}
	    }
	}
	if (candidates.size() > 0) {
	    res = formAllSubsets(candidates);
	    res = deleteEmptyState(res);
	}
	return res; 
    }

    /**
     * formAllSubsets
     * @param candidates List of predicates
     * @return List of Lists of predicates
     */
    private java.util.List formAllSubsets(java.util.List candidates){
	if (candidates.size() == 0) {
	    java.util.List a = new ArrayList();
	    java.util.List b = new ArrayList();
	    b.add(a);
	    return b;
	} else {
	    java.util.List tailList = tail(candidates);
	    oclPredicate cur = (oclPredicate)candidates.get(0);
	    try {
		cur = (oclPredicate)cur.clone();
	    } catch (Exception e) {
		System.err.println("Unexpected failure to clone predicate [OpmakerInputBox]");
	    }
	    java.util.List res = new ArrayList();
	    java.util.List work = formAllSubsets(tailList);
	    ListIterator liWork = work.listIterator();
	    while (liWork.hasNext()) {
		java.util.List subs = (java.util.List)liWork.next();
		res.add(subs);
		java.util.List newElem = cloneAll(subs);
		newElem.add(cur);
		res.add(newElem);
	    }
	    return res;
	}
    }

    /**
     * tail
     * form the copy of the given list without the first item
     * @param lst List of oclPredicates
     * @return list of oclPredicates
     */
    private java.util.List tail (java.util.List lst) {
	java.util.List res = new ArrayList();
	ListIterator li = lst.listIterator();
	if (li.hasNext()) {
	    Object head = li.next();
	}
	while (li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    try {
		cur = (oclPredicate)cur.clone();
	    } catch (Exception e) {
		System.err.println("Unexpected failure to clone predicate [OpmakerInputBox]");
	    }
	    res.add(cur);
	}
	return res;
    }

     /**
     * cloneAll
     * form the copy of the given list 
     * @param lst List of oclPredicates
     * @return list of oclPredicates
     */
    private java.util.List cloneAll (java.util.List lst) {
	java.util.List res = new ArrayList();
	ListIterator li = lst.listIterator();
	while (li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    try {
		cur = (oclPredicate)cur.clone();
	    } catch (Exception e) {
		System.err.println("Unexpected failure to clone predicate [OpmakerInputBox]");
	    }
	    res.add(cur);
	}
	return res;
    }

    /**
     * deleteEmptyState
     * remove the empty set i.e. empty list
     * @param  given List of Lists
     * @return - the list without the empt set
     */
    private java.util.List deleteEmptyState(java.util.List given) {
	boolean found = false;
	int size = given.size();
	int i = 0;
	while (!found && i < size) {
	    java.util.List cur = (java.util.List)given.get(i);
	    if (cur.size() == 0) {
		given.remove(i);
		found = true;
	    }
	    i++;
	}
	return given;
    }

    /**
     * OpMakerData
     * passes the users selections to the calling program
     */
    public class OpMakerData {
	public boolean prevail = false;
	public boolean conditional = false;
	public boolean generalised = false;
	oclStateList post = null;
	java.util.List generalisation = null;

	public OpMakerData(boolean prevail, boolean conditional,
			   boolean generalised,
			   oclStateList post, 
			   java.util.List generalisation) {
	    this.prevail = prevail;
	    this.conditional = conditional;
	    this.generalised = generalised;
	    this.post = post;
	    this.generalisation = generalisation;
	}
    } // OpMakerData

} // OpMakerInputBox

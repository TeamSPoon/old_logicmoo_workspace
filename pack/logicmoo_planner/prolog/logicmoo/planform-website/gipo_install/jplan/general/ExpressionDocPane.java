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
 * ExpressionDocPane
 * This is a pane that contains an editable expression
 * that supports the editing of variables
 * @author Ron Simpson
 * @version 0
 */

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.util.List;
import java.util.*;
import java.util.NoSuchElementException;
import java.awt.event.*;
import java.awt.*;
import java.beans.*;


import jplan.ocl.*;

public class ExpressionDocPane extends JTextPane {
    protected Vector changeListeners = new Vector();
    protected oclDomain curDomain; // Reference to current domain
    private StyleContext stateStyle;
    private Style staticStyle;
    protected DefaultStyledDocument doc; //Ron 18/10/02 changed from private
    private static Highlighter highlighter;
    protected SortSearcher searcher;
    
    static int lastPos = -1;   // Keep track of the last caret position
    protected StateModel curStateModel; // This is the state being edited
    protected StateModel.PredDetail lastSelPred;
    private List lstPreds;
    // keep track of last predicate with main highlight
    protected JPopupMenu popupMenu; // THE POPUP MENU
    protected Point popupTarget; // The location of the popup menu in the

    protected String selectedArg;   //Store details of last selected
    protected String selectedType;  // variable
    protected List selectedUnifiers; //List of matching types

    protected boolean ignoreCaretMove = false;  // used to disable caretWatch when editing is going on
	//Ron 18/10/02 changed from private to protected
    /* Weihong added on 3/5/2001 */
    private boolean MListenerEnabled = false;

    /* Weihong added on 1/2/2002 */
    private int ID = -1; // Stores position of this pane in a pane group

    /* Weihong added on 3/5/2001 */
    private MouseListener mouseListener = new MouseInputAdapter(){
	public void mouseClicked(MouseEvent me){
	    mouseWatch(me);
	}
    };


    public ExpressionDocPane (oclDomain cur) {
	super();
	setBorder(new javax.swing.border.BevelBorder(1));/* Weihong changed on 5/2/2002 */
	curDomain = cur;
	lstPreds = curDomain.predicates;
	selectedArg = new String("none");
	initComponents();
    }


    /* Weihong added on 3/5/2001 */ 
    public ExpressionDocPane (){
	super();
	setBorder(new javax.swing.border.BevelBorder(1));/* Weihong changed on 5/2/2002 */
	selectedArg = new String("none");
	stateStyle = new StyleContext();
	staticStyle = stateStyle.addStyle("Static",null);
	StyleConstants.setForeground(staticStyle,Color.blue);/* WZ 3/5/2002 */ 
	doc  = new DefaultStyledDocument(stateStyle);
	highlighter = new UnderlineHighlighter(null);
	this.setDocument(doc);
	this.setEditable(false);
	this.setHighlighter(highlighter);
	this.setToolTipText("Left click to select variable then Right click to edit properties.");
    }


    private void initComponents() {
	stateStyle = new StyleContext();
	staticStyle = stateStyle.addStyle("Static",null);
	StyleConstants.setForeground(staticStyle,Color.green);
	doc  = new DefaultStyledDocument(stateStyle);
	highlighter = new UnderlineHighlighter(null);
	this.setDocument(doc);
	this.setEditable(false);
	//((DefaultCaret)this.getCaret()).setVisible(true);
	this.setHighlighter(highlighter);
	this.setToolTipText("Left click to select variable then Right click to edit properties.");
	curStateModel = new StateModel(lstPreds);
	searcher = new SortSearcher(this,curStateModel);
	initStateEditPopup();

    /* Weihong added on 3/5/2001 */
	addMyMouseListener();
// 	this.addMouseListener(new MouseInputAdapter(){
// 		public void mouseClicked(MouseEvent me){
// 		mouseWatch(me);
// 		}
// 	    }
// 			      );
    }
    /* Weihong added on 1/2/2002 */
    
    /**
     * setID 
     * set the position of this pane in a pane group
     * @param inx  the index of the pane in the group
     */
    public void setID(int inx) {
	ID = inx;
    }

    /* Weihong added on 1/2/2002 */
    /**
     * getID
     * return the position of this pane in a pane group
     * @return int
     */
    public int getID() {
	return ID;
    }

    /* Weihong added on 3/5/2001 */
    public void addMyMouseListener(){
	if (!MListenerEnabled){
	    addMouseListener(mouseListener);
	    MListenerEnabled = true;
	}
    }


    /* Weihong added on 3/5/2001 */
    public void removeMyMouseListener() {
	if (MListenerEnabled) {
	    removeMouseListener(mouseListener);
	    MListenerEnabled = false;
	}
    }


    /**
     * addChangeListener 
     * register a listener
     * @param lst - the listener object
     */
    public void addChangeListener(ExpressionPaneListener lst) {
	if (changeListeners == null || lst == null) {
	    Utility.debugPrintln("NULL CHANGE LISTERER");
	    return;
	}   
	changeListeners.addElement(lst);
    }

    /**
     * removeChangeListener 
     * un-register a listener
     * @param lst - the listener object
     */
    public void removeChangeListener(ExpressionPaneListener lst) {
	if (changeListeners == null || lst == null) {
	    Utility.debugPrintln("NULL CHANGE LISTERER");
	    return;
	}   
	changeListeners.removeElement(lst);
    }

    protected void fireEvent(ExpressionPaneEvent evt) {
	Vector list = (Vector)changeListeners.clone();
	for (int i = 0; i < list.size(); i++) {
	    ExpressionPaneListener listener = 
		(ExpressionPaneListener)list.elementAt(i);
	    listener.getChange(evt);
	}
    }

    /**
     * setPredList
     * read only property to provide the pane with the list of Prototype
     * predicates
     * @param preds - the oclPredicate list
     */
    public void setPredList(List preds) {
	lstPreds = preds;
    }


    /* Weihong added on 3/5/2001 */
    public List getPredList() {
	return lstPreds;
    }

    /* Weihong added on 3/5/2001 */
    public void setPredicateList(List preds) {
	lstPreds = preds;
	curStateModel = new StateModel(lstPreds);
	searcher = new SortSearcher(this,curStateModel);
	initStateEditPopup();
    }


    /* Weihong added on 02/04/2001 */
    public void setCurDomain(oclDomain cur) {
	curDomain = cur;
	lstPreds = curDomain.predicates;
	curStateModel = new StateModel(lstPreds);
	searcher = new SortSearcher(this,curStateModel);
	initStateEditPopup();
    }

    /* Weihong added on 3/5/2001 */
    public oclDomain getCurDomain() {
	return curDomain;
    }

    /**
     * getNEList 
     * get the current state model ne list
     * @return DefaultListModel the NE list
     */
    public DefaultListModel getNEList () {
	return curStateModel.neList;
    }

    /**
     * getSelectedVariable
     * @return String the selected variable name
     */
    public String getSelectedVariable() {
	return selectedArg;
    }

    /**
     * getSelectedSort
     * @return String the selected variable type
     */
    public String getSelectedSort() {
	return selectedType;
    }

    /**
     * getSElectedUnifiers
     * @return List of Strings - Sort names matching the selected sort
     */
    public List getSelectedUnifiers() {
	return selectedUnifiers;
    }

    /**
     * highlightForeignUnifiers
     * highlight a list of unifiers determined by some other document pane
     * @param unifiers - List of sort name to highlight
     * @param selSort - The selected sort name
     * @param selName - The selected variable name
     */
    public void highlightForeignUnifiers(List unifiers,String selSort,
					 String selName) {
	selectedType = selSort;
	selectedArg = selName;
	selectedUnifiers = unifiers;
	searcher.removeHighlights();
	searcher.searchUnifiers(unifiers,-1,-1);
    }

    /**
     * mouseWatch
     * watch for selections of variables and requests for popups
     *
     */
    public void mouseWatch(MouseEvent mouseEvent) {
// 	Utility.debugPrintln("Mouse Pressed " + mouseEvent.paramString());
// 	if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) ==
// 	    InputEvent.BUTTON1_MASK)
// 	    Utility.debugPrintln("Button 1 pressed");
	Point viewPoint = mouseEvent.getPoint();
	int clickPos = this.viewToModel(viewPoint);
	DefaultCaret caret = (DefaultCaret)this.getCaret();
	if(SwingUtilities.isRightMouseButton(mouseEvent)) {
	    //caret.positionCaret(mouseEvent);
	    int pos = caret.getDot();
// 	    Utility.debugPrintln("Dot = " + pos + " View = " + clickPos);
	    ListIterator li = getMatchingVars().listIterator();
	    boolean found = false;
	    while (li.hasNext() && !found) {
		Point next = (Point)li.next();
// 		Utility.debugPrintln("X = " + next.x + " Y = " + next.y);
		if(next.x <= clickPos && next.y >= clickPos) {
		    popupTarget.x = next.x;
		    popupTarget.y = next.y;
		    found = true;
		}
	    }
	    if (found)
		popupMenu.show(mouseEvent.getComponent(),mouseEvent.getX(),
			   mouseEvent.getY());
	} else {
	    int pos = clickPos;
	    StateModel.PredDetail cur = null;
	    String arg = null;
	    try {
		cur = curStateModel.getPredDetailAt(pos);
	    } catch (Exception e) {
		// No predicate found
		return;
	    }
	    try {
		arg = cur.pred.elementAt(pos - cur.startOffset);
		int startOffset = cur.startOffset +
		    cur.pred.startElementAt(pos - cur.startOffset);
		int n = cur.pred.elementNoAt(pos - cur.startOffset);
		selectedType = cur.proto.getNthElementName(n);
		List unifiers = curDomain.getSortUnifiers(selectedType);
// 		ListIterator li = unifiers.listIterator();
// 		while (li.hasNext()) {
// 		    Utility.debugPrintln("UNIFIER >>>>> " + (String)li.next());
// 		}
		selectedArg = arg;
		searcher.markSelected(startOffset,startOffset + arg.length());
		unifiers.add(selectedType);
		selectedUnifiers = unifiers;
		searcher.searchUnifiers(unifiers,startOffset,startOffset + arg.length());
		//	    searcher.searchOthers(selectedType,startOffset,startOffset + arg.length());
		lastSelPred = cur;
		fireEvent(new ExpressionPaneEvent(this,
						  ExpressionPaneEvent.SELECTION,
						  arg));
		selectedArg = arg;
	    } catch (Exception e) {
		if (e instanceof NoSuchElementException) {
		    Utility.debugPrintln("Illegal sort name!!");
		    return;
		}
		
		// Ignore dot outsied predicate most likely at the 
		// end of the document
	    }
	}
    }
					 
    /**
     * caretWatch - called when caret moves
     * look for entry or exit into a predicate variable
     * @parameter a CaretEvent
     */
    private void caretWatch(CaretEvent caretEvent){
	StateModel.PredDetail cur = null;
	String arg = null;
	if (ignoreCaretMove) { // Variable editing going on ignore
	    Utility.debugPrintln("Caret Watch ignoreCaretMove On");
	    ignoreCaretMove = false;
	    return;
	}
	int pos = caretEvent.getDot();
	if (pos == lastPos) {
	    return;
	} else {
	    lastPos = pos;
	}
	try {
	    cur = curStateModel.getPredDetailAt(pos);
	} catch (Exception e) {
	    // No predicate found
	    return;
	}
	try {
	    arg = cur.pred.elementAt(pos - cur.startOffset);
	    int startOffset = cur.startOffset +
		cur.pred.startElementAt(pos - cur.startOffset);
	    int n = cur.pred.elementNoAt(pos - cur.startOffset);
	    selectedType = cur.proto.getNthElementName(n);
	    List unifiers = curDomain.getSortUnifiers(selectedType);
// 	    ListIterator li = unifiers.listIterator();
// 	    while (li.hasNext()) {
// 		Utility.debugPrintln("UNIFIER >>>>> " + (String)li.next());
// 	    }
	    selectedArg = arg;
	    searcher.markSelected(startOffset,startOffset + arg.length());
	    unifiers.add(selectedType);
	    selectedUnifiers = unifiers;
	    searcher.searchUnifiers(unifiers,startOffset,startOffset + arg.length());
	    //	    searcher.searchOthers(selectedType,startOffset,startOffset + arg.length());
	    lastSelPred = cur;
	    fireEvent(new ExpressionPaneEvent(this,
					      ExpressionPaneEvent.SELECTION,
					      arg));
	    selectedArg = arg;
	} catch (Exception e) {
	    if (e instanceof NoSuchElementException) {
		Utility.debugPrintln("Illegal sort name!!");
		return;
	    }
		    
	    // Ignore dot outsied predicate most likely at the 
	    // end of the document
	}
    }

    /**
     * editVarName - change the selected variable's name
     * @param newVName the variable's name
     */
    public void editVarName(String newVName) throws OCLSelectionException  {
	Point selPoint = getSelectedVar();
	try {
	    curStateModel.editVar(selPoint.x,newVName);
	    ignoreCaretMove = true;
	    doc.remove(selPoint.x,(selPoint.y - selPoint.x));
	    ignoreCaretMove = true;
	    doc.insertString(selPoint.x,
			     newVName,
			     new SimpleAttributeSet());
	} catch (Exception e) {
	    throw new OCLSelectionException("cannot replace variable name");
	}
    }

    /**
     * getStateList
     * get the Model predicate list
     * @return oclStateList - the list of predicates being edited
     */
    public oclStateList getStateList() {
	return curStateModel.getStateList();
    }

    /**
     * getPlainList
     * get the Model predicate list
     * @return List - the list of predicates being edited
     */
    public List getPlainList() {
	return curStateModel.getPlainList();
    }


    /**
     * clearPane
     * - empty the document pane and reset the state model
     */
    public void clearPane() throws BadLocationException {
	ignoreCaretMove = true;
	doc.remove(0,doc.getLength());
	curStateModel.resetModel();
	lastSelPred = null; 
    }

    /**
     * isDirty
     * simple check if text in edit pane -coud do better than this!!
     * @return boolean
     */
    public boolean isDirty() {
	return (doc.getLength() > 0);
    }

    /**
     * init popup menu
     * create the popup menu
     */
    private void initStateEditPopup() {
	popupMenu = new JPopupMenu();
	JMenuItem sameMI = new JMenuItem("Same");
	popupTarget = new Point(-1,-1);
	sameMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    // Change target Variable to match selected
		    String select = null;
		    Point selPoint = getSelectedVar();
// 		    Utility.debugPrintln("<<>>Fired Same selPoint x = " + selPoint.x);
		    try {
			if (selPoint.x != -1) { 
			    // Selection in this pane
			    select = 
				curStateModel.getArgumentAt(selPoint.x);
			} else {
			    select = selectedArg;
			}
			oclPredicate targPred = 
			    curStateModel.getPredicateAt(popupTarget.x);
			String oldVName =
			    curStateModel.editVar(popupTarget.x,select);
			ignoreCaretMove = true;
			doc.remove(popupTarget.x,
				   (popupTarget.y - popupTarget.x));
			ignoreCaretMove = true;
			doc.insertString(popupTarget.x,
					 select,
					 new SimpleAttributeSet());
			// Deal with NEs
			int neIndex = curStateModel.neExists(oldVName,select);
			if (neIndex != -1) {
			    if (JOptionPane.YES_OPTION == 
				JOptionPane.showConfirmDialog(null,
					   "Remove restriction clause  ne(" +
						 oldVName + "," +
						 select +")",
				            "GIPO Query",
				            JOptionPane.YES_NO_OPTION)) {
				curStateModel.neList.removeElementAt(neIndex);
			    }
			}
			// redo highlights
			if (selPoint.x != -1) {
			    searcher.searchUnifiers(selectedUnifiers,
						    searcher.selectedVar.x,
						    searcher.selectedVar.y);
			} else {
			    searcher.searchUnifiers(selectedUnifiers,-1,-1);
			}
		    } catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to select argument");
		    }
		}
	    }
				  );
	JMenuItem diffMI = new JMenuItem("Different");
	diffMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    // Make sure selected and target are different
		    // if needed and ne clause
		    enforceDifferentBindings(evt);

		}
	    }
				  );
	JMenuItem optMI = new JMenuItem("Optional");
	optMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    // Make sure selected and target are different
		    // if needed and ne clause
		    allowDifferentBindings(evt);

		}
	    }
				  );
	//cutMI.addActionListener(aL);
	popupMenu.add(sameMI);
	popupMenu.add(diffMI);
	popupMenu.add(optMI);
	//	MouseListener mouseListener = 
	//   new StateEdJPopupMenuShower(popupMenu,this,
	//			getMatchingVars(),popupTarget);
	//this.addMouseListener(mouseListener);
    }


    /**
     * addPredToState - add the currently selected predicate
     * to the state being edited.
     * @param cur - the predicate to add to the list
     */
    public void addPredicate(oclPredicate cur) {
    //throws OCLSelectionException {
	try {
	    StateModel.PredDetail cpd =
		curStateModel.addPredicate(cur,doc.getLength());
	    if (!"ne".equals(cur.getName()) &&
		!"is_of_sort".equals(cur.getName())) {
		int offset = doc.getLength();
		ignoreCaretMove = true;
		doc.insertString(offset,
				 (cur.toString() + 
				  "\n"),null);
		if (cpd.proto.isStatic()) {
// 		    Utility.debugPrintln("Adding static predicate " + cur.toString());
		    doc.setParagraphAttributes(offset,
					       cur.toString().length(),
					       staticStyle,true);
		}   
	    }
	} catch (Exception e) {
	    //throw new OCLSelectionException("Failed to insert predicate");
	    Utility.debugPrintln("patterns","OCLSelectionException"+e);
	}
    }    

    /**
     * addSortedPredicate - add predicate without seperate prototype
     * to the state being edited. The predicate must have sorts added
     * @param cur - the predicate to add to the list
     */
    public void addSortedPredicate(oclPredicate cur) {
    //throws OCLSelectionException {
	try {
	    StateModel.PredDetail cpd =
		curStateModel.addSortedPredicate(cur,doc.getLength());
	    int offset = doc.getLength();
	    ignoreCaretMove = true;
	    doc.insertString(offset,
			     (cur.toString() + 
			      "\n"),null); 
	} catch (Exception e) {
	    //throw new OCLSelectionException("Failed to insert predicate");
	    Utility.debugPrintln("patterns","OCLSelectionException"+e);
	}
    }    

    /**
     * removePredFromState - add the currently selected predicate
     * to the state being edited
     */
    public void removePredFromState() {
	List staticPreds;
	if (lastSelPred == null) {
	    JOptionPane.showMessageDialog(this,
		     "Select a variable in the predicate to be removed first.",
		      "GIPO Error",
		      JOptionPane.WARNING_MESSAGE,
		   null);
	    return;
	}
	try {
	    searcher.removeHighlights();
	    ignoreCaretMove = true;
	    doc.remove(lastSelPred.startOffset,lastSelPred.length + 1);
	    staticPreds =
		curStateModel.removePredicateAt(lastSelPred.startOffset+1);
	    if (staticPreds.size() > 0) {
		ListIterator li = staticPreds.listIterator();
		while (li.hasNext()) {
		    StateModel.PredDetail cur = 
			(StateModel.PredDetail)li.next();
		    doc.setParagraphAttributes(cur.startOffset,
					       cur.length,
					       staticStyle,true);
		}
	    }
	    lastSelPred = null;
	    List nes = curStateModel.illegalNEs();
	    if (nes.size() > 0) {
		ListIterator li = nes.listIterator();
		int count = 0;
		while (li.hasNext()) {
		    int inx = ((Integer)li.next()).intValue() - count;
		    curStateModel.neList.remove(inx);
		    count++;
		}
	    }
	} catch(Exception sme) {
	    Utility.debugPrintln("Cannot remove predicate " +sme.toString());
	    return;
	}
    }

    /**
     * getSelectedVar - get currently selected variable
     * @return Point from and to offsets in the document
     */
    protected Point getSelectedVar() {
	return searcher.selectedVar;
    }

    /**
     * getMatchingVars - get variable offsets of matching variables
     * @return List of Point(s) each with from and to offsets
     */
    protected List getMatchingVars() {
	return searcher.vars;
    }

    /**
     * enforceDifferentBindings
     * ensure target and selected variable have different names and 
     * appropriate ne clause exists
     * @param evt Event 
     */
    protected void enforceDifferentBindings(java.awt.event.ActionEvent evt){
	Point selPoint = getSelectedVar();
	try {
	    String select = null;
	    if (selPoint.x != -1) { 
			    // Selection in this pane
		select = 
		    curStateModel.getArgumentAt(selPoint.x);
	    } else {
		select = selectedArg;
	    }
	    String targArg = 
		curStateModel.getArgumentAt(popupTarget.x);
	    if (select.equals(targArg)) { // Currently same
		String newVName= JOptionPane.showInputDialog(null,
			      "Enter New Name for Variable.");
		// Should check that it doesnt exist
		curStateModel.editVar(popupTarget.x,newVName);
		ignoreCaretMove = true;
		doc.remove(popupTarget.x,
			   (popupTarget.y - popupTarget.x));
		ignoreCaretMove = true;
		doc.insertString(popupTarget.x,
				 newVName,
				 new SimpleAttributeSet());
		if (curStateModel.neExists(newVName,select) == -1) {
		    oclPredicate curNE = curStateModel.addNE(newVName,select);
		} 
		// redo highlights
		if (selPoint.x != -1) {
		    searcher.searchUnifiers(selectedUnifiers,
					    searcher.selectedVar.x,
					    searcher.selectedVar.y);
		} else {
		    searcher.searchUnifiers(selectedUnifiers,-1,-1);
		}
	    } else {
		// Check that appropriate NE exists
		if (curStateModel.neExists(targArg,select) == -1) {
		    oclPredicate curNE = curStateModel.addNE(targArg,select);
		} 
		// Should check that appropriate NE exists
// 		Utility.debugPrintln("These variables are already different" +
// 				   "\n given " + targArg + " and " +
// 				   select);
	    }


	    
	} catch (Exception sme) {
	    Utility.debugPrintln("Unexpected failure to select argument");
	}
    }

    /**
     * allowDifferentBindings
     * ensure target and selected variable have different names BUT no
     * ne clause exists
     * @param evt Event 
     */
    protected void allowDifferentBindings(java.awt.event.ActionEvent evt){
	Point selPoint = getSelectedVar();
	try {
// 	    Utility.debugPrintln("Selection Point X = " + selPoint.x);
	    String select = 
		curStateModel.getArgumentAt(selPoint.x);
// 	    Utility.debugPrintln("Selected Variable " + select +
// 			       " popup target x = " + popupTarget.x +
// 			       " popup target y = " + popupTarget.y );
	    String targArg = 
		curStateModel.getArgumentAt(popupTarget.x);
	    if (select.equals(targArg)) { // Currently same
		String newVName= JOptionPane.showInputDialog(null,
			      "Enter New Name for Variable.");
		// Should check that it doesnt exist
		curStateModel.editVar(popupTarget.x,newVName);
		ignoreCaretMove = true;
		doc.remove(popupTarget.x,
			   (popupTarget.y - popupTarget.x));
		ignoreCaretMove = true;
		doc.insertString(popupTarget.x,
				 newVName,
				 new SimpleAttributeSet());
		int neIndex = curStateModel.neExists(newVName,select);
		if (neIndex != -1) {
		    if (JOptionPane.YES_OPTION == 
			JOptionPane.showConfirmDialog(null,
				    "Remove ne restriction clause",
				    "GIPO Query",
				     JOptionPane.YES_NO_OPTION)) {
			curStateModel.neList.removeElementAt(neIndex);
		    }
		    
		} 
// 		Utility.debugPrintln("Name = " + newVName);
		// redo highlights
 		searcher.searchUnifiers(selectedUnifiers,
 				      searcher.selectedVar.x,
 				      searcher.selectedVar.y);
	    } else {
	    }


	    
	} catch (Exception sme) {
	    Utility.debugPrintln("Unexpected failure to select argument");
	}
    }

}

/**
 * class to search for occurrances of the given sort name
 * and highlight (underline) those words
 */
class SortSearcher {
    public SortSearcher(JTextComponent comp,StateModel stateModel) {
	this.comp = comp;
	this.painter = 
	    new UnderlineHighlighter.UnderlineHighlightPainter
		(Color.red);
	this.selPainter = new UnderlineHighlighter.UnderlineHighlightPainter
		(Color.green);
	this.stateModel = stateModel;
	vars = new ArrayList();
	selectedVar = new Point(-1,-1);
    }
    /**
     * Search for sort
     * Highlights are added for all occurrences found.
     */
    public void search(String sort) {
	int firstOffset = -1;
	Highlighter highlighter = comp.getHighlighter();
	
	// Remove any existing highlights for last sort
	Highlighter.Highlight[] highlights = highlighter.getHighlights();
	for (int i = 0; i < highlights.length; i++) {
	    Highlighter.Highlight h = highlights[i];
	    if (h.getPainter() instanceof 
		UnderlineHighlighter.UnderlineHighlightPainter) {
		highlighter.removeHighlight(h);
	    }
	}
	
	if (sort == null || sort.equals("")) {
	    return;
	}
	
	// Look for the sort we are given 

	int docLength = comp.getDocument().getLength();
	int lastIndex = 0;
	int sortSize = sort.length();
	while (lastIndex < (docLength - (sortSize + 2))) {	
	    oclPredicate curPred = null;
	    try {
		curPred = stateModel.getPredicateAt(lastIndex + 1);
	    } catch (Exception e) {
		Utility.debugPrintln("No predicate found at " + (lastIndex + 1));
		break;
	    }
	    lastIndex += curPred.getName().length() + 1;
	    List args = curPred.getArguments();
	    ListIterator li =  args.listIterator();
	    while (li.hasNext()) {
		String argName = ((oclPredicate.pArg)li.next()).name;
		if (sort.equals(argName)) {
		    int endIndex = lastIndex + argName.length();
		    try {
			highlighter.addHighlight(lastIndex, endIndex, 
						 painter);
		    } catch (BadLocationException e) {
			Utility.debugPrintln("Unexpected failure - search");
				// Nothing to do
		    }
		}
		// Adjust indexes
		lastIndex += argName.length() + 1; // +1 for comma or )
	    }
	    lastIndex++;  // increment for return
	}
    }

    /**
     * searchOthers Search for sort
     * Highlights are added for all occurrences found. except
     * the selection in the range specified
     * Resets the selectedVar point and
     * recreates the list of available matching variables
     * @param sort
     * @param from - start of element to be skipped
     * @param to - end of element to be skipped
     */
    public void searchOthers(String sort,int from, int to) {
	selectedVar.x = from;
	selectedVar.y = to;
	vars.clear();
	int firstOffset = -1;
	Highlighter highlighter = comp.getHighlighter();
	
	// Do NOT Remove any existing highlights will include selected
	
	if (sort == null || sort.equals("")) {
	    return;
	}
	
	// Look for the sort we are given 
	
	int docLength = comp.getDocument().getLength();
	int lastIndex = 0;
	int sortSize = sort.length();
	while (lastIndex < (docLength - (sortSize + 1))) {	
	    oclPredicate curPred = null;
	    oclPredicate curProto = null;
	    StateModel.PredDetail curDet;
	    try {
		//curPred = stateModel.getPredicateAt(lastIndex + 1);
		curDet = stateModel.getPredDetailAt(lastIndex + 1);
		curPred = curDet.pred;
		curProto = curDet.proto;
	    } catch (Exception e) {
		Utility.debugPrintln("No predicate found at " + (lastIndex + 1));
		break;
	    }
	    lastIndex += curPred.getName().length() + 1;
	    List args = curPred.getArguments();
	    ListIterator li =  args.listIterator();
	    List protoArgs = curProto.getArguments();
	    ListIterator liProto = protoArgs.listIterator();
	    while (li.hasNext()) {
		String argName = ((oclPredicate.pArg)li.next()).name;
		String sortName = ((oclPredicate.pArg)liProto.next()).name;
// 		Utility.debugPrintln("Found Sort name " + sortName);
		if (sort.equals(sortName)) {
		    int endIndex = lastIndex + argName.length();
		    if ((endIndex < from) || (lastIndex > to)) { //SKIP
			try {
			    highlighter.addHighlight(lastIndex, endIndex, 
						     painter);
			    vars.add(new Point(lastIndex,endIndex));
// 			    Utility.debugPrint("Adding " + lastIndex + " to " + endIndex);
			} catch (BadLocationException e) {
			    Utility.debugPrintln("Unexpected failure - searchOthers");
			    // Nothing to do
			}
		    }
		}
		// Adjust indexes
		lastIndex += argName.length() + 1; // +1 for comma or )
	    }
	    lastIndex++;  // increment for return
	}
    }

    /**
     * searchUnifiers Search for sort - sort subtypes
     * and parent hierarchy
     * Highlights are added for all occurrences found. except
     * the selection in the range specified
     * Resets the selectedVar point and
     * recreates the list of available matching variables
     * @param unifiers - names of matching sorts
     * @param from - start of element to be skipped
     * @param to - end of element to be skipped
     */
    public void searchUnifiers(List unifiers,int from, int to) {
	selectedVar.x = from;
	selectedVar.y = to;
	vars.clear();
	int firstOffset = -1;
	Highlighter highlighter = comp.getHighlighter();
	
	// Do NOT Remove any existing highlights will include selected
	
	if (unifiers == null || unifiers.size() == 0) {
	    return;
	}
	
	// Look for the sort/unifiers we are given 
	
	int docLength = comp.getDocument().getLength();
	int lastIndex = 0;
	//int sortSize = sort.length();
	while (lastIndex < (docLength - 1)) {	
	    oclPredicate curPred = null;
	    oclPredicate curProto = null;
	    StateModel.PredDetail curDet;
	    try {
		//curPred = stateModel.getPredicateAt(lastIndex + 1);
		curDet = stateModel.getPredDetailAt(lastIndex + 1);
		curPred = curDet.pred;
		curProto = curDet.proto;
	    } catch (Exception e) {
		Utility.debugPrintln("No predicate found at " + (lastIndex + 1));
		break;
	    }
	    lastIndex += curPred.getName().length() + 1;
	    List args = curPred.getArguments();
	    ListIterator li =  args.listIterator();
	    List protoArgs = curProto.getArguments();
	    ListIterator liProto = protoArgs.listIterator();
	    while (li.hasNext()) {
		String argName = ((oclPredicate.pArg)li.next()).name;
		String sortName = ((oclPredicate.pArg)liProto.next()).name;
// 		Utility.debugPrintln("Found Sort name " + sortName);
		ListIterator liUnifiers = unifiers.listIterator();
		boolean found = false;
		while (!found && liUnifiers.hasNext()) {
		    String sort = (String)liUnifiers.next();
		    if (sort.equals(sortName)) {
			found = true;
			int endIndex = lastIndex + argName.length();
			if ((endIndex < from) || (lastIndex > to)) { //SKIP
			    try {
				highlighter.addHighlight(lastIndex, endIndex, 
							 painter);
				vars.add(new Point(lastIndex,endIndex));
// 				Utility.debugPrint("Adding " + lastIndex + " to " + endIndex);
			    } catch (BadLocationException e) {
				Utility.debugPrintln("Unexpected failure - searchOthers");
				// Nothing to do
			    }
			}
		    }
		}
		// Adjust indexes
		lastIndex += argName.length() + 1; // +1 for comma or )
	    }
	    lastIndex++;  // increment for return
	}
    }


    /**
     * markSelected - Underline the selected word
     * @param from - index of start of text
     * @param to   - index of end of selected text
     */
    public void markSelected(int from,int to) {
	Highlighter highlighter = comp.getHighlighter();
	// Remove any existing highlights for last selected
	Highlighter.Highlight[] highlights = highlighter.getHighlights();
	for (int i = 0; i < highlights.length; i++) {
	    Highlighter.Highlight h = highlights[i];
	    if (h.getPainter() instanceof 
		UnderlineHighlighter.UnderlineHighlightPainter) {
		highlighter.removeHighlight(h);
	    }
	}
	try {
	    highlighter.addHighlight(from, to, 
				     selPainter);
	} catch (BadLocationException e) {
	    Utility.debugPrintln("Unexpected failure - markSelected");
	    // Nothing to do
	}
    }

    /**
     * removeHighlights 
     * remove all underline highlights
     */
    public void removeHighlights() {
	Highlighter highlighter = comp.getHighlighter();
	// Remove any existing highlights for last selected
	Highlighter.Highlight[] highlights = highlighter.getHighlights();
	for (int i = 0; i < highlights.length; i++) {
	    Highlighter.Highlight h = highlights[i];
	    if (h.getPainter() instanceof 
		UnderlineHighlighter.UnderlineHighlightPainter) {
		highlighter.removeHighlight(h);
	    }
	}
    }
    
    protected JTextComponent comp;
    protected Highlighter.HighlightPainter painter;
    protected Highlighter.HighlightPainter selPainter;
    protected StateModel stateModel;
    public List vars;
    public Point selectedVar;

}

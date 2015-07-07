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

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.util.List;
import java.util.*;
import java.util.NoSuchElementException;
import java.awt.event.*;
import java.awt.*;
import java.beans.*;
import jplan.general.ExpressionModel.ExpressionException;

import jplan.ocl.*;

/**
 * This is a pane that contains an editable expression
 * that supports the editing of variables
 * this pane displays ne clauses in addition to normal predicates
 * @author Ron Simpson
 * @version 0
 */
public class FullExpressionDocPane extends JTextPane {

	protected Vector changeListeners = new Vector();

	protected oclDomain curDomain; // Reference to current domain
	private StyleContext exprStyle;
	private Style defaultStyle;
	private Style staticStyle;
	protected Style neStyle; //Ron 11/10/02 changed to protected
	protected DefaultStyledDocument doc;
	private static Highlighter highlighter;
	protected ExpSearcher searcher;

	protected ExpressionModel curExpModel; // This is the state being edited
	protected ExpressionModel.PredDetail lastSelPred;
	private List lstPreds;

	protected JPopupMenu popupMenu; // THE POPUP MENU
	protected Point popupTarget; // The location of the popup menu in the
	protected JPopupMenu popupMenuRename; // The basic rename popup
	protected JPopupMenu restrictPopupMenu; // Rename or restrict
	protected JPopupMenu deletePopupMenu; // Delete 

	// keep track of last predicate with main highlight
	protected String selectedArg; //Store details of last selected
	protected String selectedType; // variable
	protected List selectedUnifiers; //List of matching types
	protected int clickPos = -1;

	private int ID = -1; // Stores position of this pane in a pane group

	/* Weihong added on 3/5/2001 */
	private boolean MListenerEnabled = false;

	public FullExpressionDocPane(oclDomain cur) {
		super();
		setBorder(new javax.swing.border.BevelBorder(1));
		/* Weihong changed on 5/2/2002 */
		lstPreds = cur.predicates;
		curDomain = cur;
		selectedArg = new String("none");
		initComponents();
	}

	/* Weihong added on 3/5/2001 */
	public FullExpressionDocPane() {
		super();
		setBorder(new javax.swing.border.BevelBorder(1));
		/* Weihong changed on 5/2/2002 */
		exprStyle = new StyleContext();
		defaultStyle = exprStyle.getStyle(StyleContext.DEFAULT_STYLE);
		staticStyle = exprStyle.addStyle("Static", null);
		StyleConstants.setForeground(staticStyle, Color.blue);
		neStyle = exprStyle.addStyle("NE", null);
		doc = new DefaultStyledDocument(exprStyle);
		StyleConstants.setForeground(neStyle, Color.red);
		highlighter = new UnderlineHighlighter(null);
		this.setDocument(doc);
		this.setEditable(false);
		this.setHighlighter(highlighter);
		this.setToolTipText(
			"Click on variable (red underlined) to edit properties.");
	}

	/* Weihong added on 3/5/2001 */
	private MouseListener mouseListener = new MouseInputAdapter() {
		public void mouseClicked(MouseEvent me) {
			mouseWatch(me);
		}
	};
	
	/* Ron 19/3/05 */
	private MouseListener littleMouseListener = new MouseInputAdapter() {
			public void mouseClicked(MouseEvent me) {
				littleMouseWatch(me);
			}
		};

	/**
	 * setID 
	 * set the position of this pane in a pane group
	 * @param inx - the index of the pane in the group
	 */
	public void setID(int inx) {
		ID = inx;
	}

	/**
	 * getID
	 * return the position of this pane in a pane group
	 * @return int
	 */
	public int getID() {
		return ID;
	}

	private void initComponents() {
		exprStyle = new StyleContext();
		defaultStyle = exprStyle.getStyle(StyleContext.DEFAULT_STYLE);
		staticStyle = exprStyle.addStyle("Static", null);
		StyleConstants.setForeground(staticStyle, Color.blue);
		neStyle = exprStyle.addStyle("NE", null);
		doc = new DefaultStyledDocument(exprStyle);
		StyleConstants.setForeground(neStyle, Color.red);
		highlighter = new UnderlineHighlighter(null);
		this.setDocument(doc);
		this.setEditable(false);
		this.setHighlighter(highlighter);
		this.setToolTipText(
			"Click on variable (red underlined) to edit properties.");
		curExpModel = new ExpressionModel(curDomain);
		searcher = new ExpSearcher(this, curExpModel);
		initStateEditPopup();
		initRenamePopup();
		initRestrictPopup();
		initDeletePopup();

		/* Weihong added on 3/5/2001 */
		addMyMouseListener();
	}

	/* Weihong added on 3/5/2001 */
	public void addMyMouseListener() {
		if (!MListenerEnabled) {
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
	
	/* Ron 19/5/03 only highlight selected variable */
	public void useLittleMouseListener() {
		if (MListenerEnabled) {
			removeMouseListener(mouseListener);
			addMouseListener(littleMouseListener);
			MListenerEnabled = true;
		}
	}


	/* Weihong added on 3/5/2001 */
	public List getPredList() {
		return lstPreds;
	}

	/* Weihong added on 02/04/2001 */
	public void setCurDomain(oclDomain cur) {
		curDomain = cur;
		lstPreds = curDomain.predicates;
		curExpModel = new ExpressionModel(curDomain);
		searcher = new ExpSearcher(this, curExpModel);
		initStateEditPopup();
		initRestrictPopup();
		initRenamePopup();
		initDeletePopup();
	}

	/* Weihong added on 3/5/2001 */
	public oclDomain getCurDomain() {
		return curDomain;
	}

	/**
	 * getDocumentLength
	 * get the textpane document length
	 * @return int
	 */
	public int getDocumentLength() {
		return doc.getLength();
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
		Vector list = (Vector) changeListeners.clone();
		for (int i = 0; i < list.size(); i++) {

			Utility.debugPrintln("fireEvent: " + list.size());

			ExpressionPaneListener listener =
				(ExpressionPaneListener) list.elementAt(i);
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

	/**
	 * getNEList 
	 * get the current state model ne list
	 * @return DefaultListModel the NE list
	 */
	public DefaultListModel getNEList() {
		return curExpModel.neList;
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
	 * highlight a list of unifiers determined by fome other document pane
	 * @param unifiers - List of sort name to highlight
	 * @param selSort - The selected sort name
	 * @param selName - The selected variable name
	 */
	public void highlightForeignUnifiers(
		List unifiers,
		String selSort,
		String selName) {
		if (lstPreds.size() == 0) {
			return;
		}
		selectedType = selSort;
		selectedArg = selName;
		selectedUnifiers = unifiers;
		searcher.removeHighlights();
		searcher.searchUnifiers(unifiers, -1, -1);
	}

	/**
	 * mouseWatch
	 * watch for selections of variables and requests for popups
	 *
	 */
	public void mouseWatch(MouseEvent mouseEvent) {
		Point viewPoint = mouseEvent.getPoint();
		clickPos = this.viewToModel(viewPoint);
		oclPredicate curPred = null;
		try {
			curPred = curExpModel.getPredicateAt(clickPos);
		} catch (ExpressionModel.ExpressionException ee) {
			return;
		}

		if (SwingUtilities.isRightMouseButton(mouseEvent)) {
			if (curPred.getName().equals("ne")
				|| curPred.getName().equals("is_of_sort")) {
				deletePopupMenu.show(
					mouseEvent.getComponent(),
					mouseEvent.getX(),
					mouseEvent.getY());
			} else {
				Point selPoint = getSelectedVar();
				if (selPoint.x <= clickPos && selPoint.y >= clickPos) {
					// This is the currently selected variable
					List subTypes = curDomain.getSortSubTypes(selectedType);
					if (subTypes != null) {
						Utility.debugPrintln(
							"TYPE " + selectedType + " has sub types");
						restrictPopupMenu.show(
							mouseEvent.getComponent(),
							mouseEvent.getX(),
							mouseEvent.getY());
					} else {
						Utility.debugPrintln("RENAME");
						popupMenuRename.show(
							mouseEvent.getComponent(),
							mouseEvent.getX(),
							mouseEvent.getY());
					}
				} else {
					ListIterator li = getMatchingVars().listIterator();
					boolean found = false;
					while (li.hasNext() && !found) {
						Point next = (Point) li.next();
						if (next.x <= clickPos && next.y >= clickPos) {
							popupTarget.x = next.x;
							popupTarget.y = next.y;
							found = true;
						}
					}
					if (found) {
						popupMenu.show(
							mouseEvent.getComponent(),
							mouseEvent.getX(),
							mouseEvent.getY());
					}
				}
			}
		} else {
			// We need to highlight variables
			int pos = clickPos;
			ExpressionModel.PredDetail cur = null;
			String arg = null;
			try {
				cur = curExpModel.getPredDetailAt(pos);
			} catch (Exception e) {
				// No predicate found
				return;
			}
			try {
				arg = cur.pred.elementAt(pos - cur.startOffset);
				int startOffset =
					cur.startOffset
						+ cur.pred.startElementAt(pos - cur.startOffset);
				int n = cur.pred.elementNoAt(pos - cur.startOffset);
				selectedType = cur.proto.getNthElementName(n);
				List unifiers = curDomain.getSortUnifiers(selectedType);
				selectedArg = arg;
				searcher.markSelected(startOffset, startOffset + arg.length());
				unifiers.add(selectedType);
				selectedUnifiers = unifiers;
				searcher.searchUnifiers(
					unifiers,
					startOffset,
					startOffset + arg.length());
				lastSelPred = cur;
				fireEvent(
					new ExpressionPaneEvent(
						this,
						ExpressionPaneEvent.SELECTION,
						arg));
				selectedArg = arg;
			} catch (Exception e) {
				if (e instanceof NoSuchElementException) {
					Utility.debugPrintln("Illegal sort name!!");
					return;
				}

				// Ignore dot outside predicate most likely at the 
				// end of the document
			}
		}
	}
	
	/**
	 * littleMouseWatch
	 * watch for selections of variables and requests for popups
	 * ONLY highlight selected variable
	 */
	public void littleMouseWatch(MouseEvent mouseEvent) {
		Point viewPoint = mouseEvent.getPoint();
		clickPos = this.viewToModel(viewPoint);
		oclPredicate curPred = null;
		try {
			curPred = curExpModel.getPredicateAt(clickPos);
		} catch (ExpressionModel.ExpressionException ee) {
			return;
		}

		if (!SwingUtilities.isRightMouseButton(mouseEvent)) {
			// We need to highlight variables
			int pos = clickPos;
			ExpressionModel.PredDetail cur = null;
			String arg = null;
			try {
				cur = curExpModel.getPredDetailAt(pos);
			} catch (Exception e) {
				// No predicate found
				return;
			}
			try {
				arg = cur.pred.elementAt(pos - cur.startOffset);
				int startOffset =
					cur.startOffset
						+ cur.pred.startElementAt(pos - cur.startOffset);
				int n = cur.pred.elementNoAt(pos - cur.startOffset);
				selectedType = cur.proto.getNthElementName(n);
				selectedArg = arg;
				searcher.markSelected(startOffset, startOffset + arg.length());
				lastSelPred = cur;
			} catch (Exception e) {
				if (e instanceof NoSuchElementException) {
					Utility.debugPrintln("Illegal sort name!!");
					return;
				}

				// Ignore dot outside predicate most likely at the 
				// end of the document
			}
		}
	}

	/**
	 * editVarName - change the selected variable's name
	 * redo mark highlight - no others need changing
	 * @param newVName the variable's name
	 */
	public void editVarName(String newVName) throws OCLSelectionException {
		Point selPoint = getSelectedVar();
		try {
			curExpModel.editVar(selPoint.x, newVName);
			doc.remove(selPoint.x, (selPoint.y - selPoint.x));
			doc.insertString(selPoint.x, newVName, new SimpleAttributeSet());
			searcher.markSelected(selPoint.x, selPoint.x + newVName.length());
			searcher.searchUnifiers(
				selectedUnifiers,
				selPoint.x,
				selPoint.x + newVName.length());
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
		return curExpModel.getStateList();
	}

	/**
	 * getPlainList
	 * get the Model predicate list
	 * @return List - the list of predicates being edited
	 */
	public List getPlainList() {
		return curExpModel.getPlainList();
	}

	/**
	 * getSCSEName
	 * @return oclPredicate - the se or sc clause provided for this
	 *                      - expression list
	 */
	public oclPredicate getSCSEName() throws ExpressionException {
		return curExpModel.getSCSEName();
	}

	/**
	 * getPurePredicateList
	 * @return List - the list of predicate in the expression
	 *                without the se or sc name
	 */
	public List getPurePredicateList() {
		return curExpModel.getPurePredicateList();
	}

	/* Weihong added on 16/5/2001 */
	/**
	 * getFullPredicateList
	 * @return List - the list of predicate in the expression
	 * 
	 */
	public List getFullPredicateList() {
		return curExpModel.getFullPredicateList();
	}

	/**
	 * clearPane
	 * - empty the document pane and reset the state model
	 */
	public void clearPane() throws BadLocationException {
		doc.remove(0, doc.getLength());
		curExpModel.resetModel();
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
	 * init Transaction popup menu2
	 * create the edit target variable name
	 */
	protected void initRenamePopup() {
		popupMenuRename = new JPopupMenu();
		JMenuItem renameMI = new JMenuItem("Rename");
		renameMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Renam target Variable
				Utility.debugPrintln("Fired Rename");
				renameTargetVariable(evt);
			}
		});
		popupMenuRename.add(renameMI);
	}

	/**
	 * renameTargetVariable
	 * rename target (selected - green) variable
	 * ask if unifiers should be renamed
	 * appropriate ne clause exists
	 * @param evt Event 
	 */
	protected void renameTargetVariable(java.awt.event.ActionEvent evt) {
		Point selPoint = getSelectedVar();
		try {
			String select = curExpModel.getArgumentAt(selPoint.x);
			JPanel jpanUnify = new JPanel();
			jpanUnify.setBorder(BorderFactory.createTitledBorder("Rename .."));

			JRadioButton jradThis =
				new JRadioButton("This variable only", true);
			JRadioButton jradNode = new JRadioButton("This Node Only", false);
			JRadioButton jradAll = new JRadioButton("All occurances", true);
			jradThis.setMnemonic(KeyEvent.VK_V);
			jradNode.setMnemonic(KeyEvent.VK_N);
			jradAll.setMnemonic(KeyEvent.VK_A);
			Box boxUnify = Box.createVerticalBox();
			ButtonGroup groupUnify = new ButtonGroup();
			boxUnify.add(jradThis);
			boxUnify.add(jradNode);
			boxUnify.add(jradAll);
			groupUnify.add(jradThis);
			groupUnify.add(jradNode);
			groupUnify.add(jradAll);
			jpanUnify.add(boxUnify);
			Object message[] = { "Enter New Name for Variable.", jpanUnify };

			JOptionPane query = new JOptionPane();
			query.setMessage(message);
			query.setMessageType(JOptionPane.QUESTION_MESSAGE);
			query.setOptionType(JOptionPane.OK_CANCEL_OPTION);
			query.setWantsInput(true);
			JDialog dia = query.createDialog(null, "Query");
			dia.show();
			if (((Integer) query.getValue()).intValue() == 0) {
				// Selected OK
				String newVName = (String) query.getInputValue();
				// Should check that it doesnt exist
				if (jradThis.isSelected()) {
					Utility.debugPrintln(" XXXXX This Selected");
					curExpModel.editVar(selPoint.x, newVName);
					doc.remove(selPoint.x, (selPoint.y - selPoint.x));
					doc.insertString(
						selPoint.x,
						newVName,
						new SimpleAttributeSet());
				} else if (jradNode.isSelected()) {
					Utility.debugPrintln(" XXXXX Node Selected");
					selPoint.x =
						replaceAllVariables(selPoint.x, select, newVName);
					selPoint.y = selPoint.x + newVName.length();
				} else if (jradAll.isSelected()) {
					Utility.debugPrintln(" XXXXX All Selected");
					selPoint.x =
						replaceAllVariables(selPoint.x, select, newVName);
					fireEvent(
						new ExpressionPaneEvent(
							this,
							ExpressionPaneEvent.RENAME,
							newVName,
							select,
							"none",
							ExpressionPaneEvent.GLOBAL));
				} else
					Utility.debugPrintln(" XXXXX NONE Selected");

				// redo highlights
				removeHighlights();
				fireEvent(
					new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
			}
		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to rename argument" + sme);
		}
	}

	/**
	 * init popup menu
	 * create the popup menu
	 */
	private void initStateEditPopup() {
		popupMenu = new JPopupMenu();
		JMenuItem sameMI = new JMenuItem("Same");
		popupTarget = new Point(-1, -1);
		sameMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				sameBindings(evt);
			}
		});
		JMenuItem diffMI = new JMenuItem("Different");
		diffMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Make sure selected and target are different
				// if needed and ne clause
				enforceDifferentBindings(evt);

			}
		});
		JMenuItem optMI = new JMenuItem("Optional");
		optMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Make sure selected and target are different
				// if needed and ne clause
				allowDifferentBindings(evt);

			}
		});
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
	 * addPredicate - add the currently selected predicate
	 * to the state being edited.
	 * @param cur - the predicate to add to the list
	 */
	public void addPredicate(oclPredicate cur) throws OCLSelectionException {
		Utility.debugPrintln("AddPredicate = " + cur.toString());
		try {
			ExpressionModel.PredDetail cpd =
				curExpModel.addPredicate(cur, doc.getLength());
			int offset = doc.getLength();
			doc.insertString(offset, (cur.toString() + "\n"), null);
			if (cpd.proto.isStatic()) {
				doc.setParagraphAttributes(
					offset,
					cur.toString().length(),
					staticStyle,
					true);
			} else if (
				cpd.pred.getName().equals("ne")
					|| cpd.pred.getName().equals("is_of_sort")) {
				doc.setParagraphAttributes(
					offset,
					cur.toString().length(),
					neStyle,
					true);
			}
		} catch (Exception e) {
			throw new OCLSelectionException(
				"Failed to insert predicate " + e.toString());
		}
	}

	/**
	 * removePredFromState - add the currently selected predicate
	 * to the state being edited
	 */
	public void removePredFromState() {
		List followPreds;
		if (lastSelPred == null) {
			JOptionPane.showMessageDialog(
				this,
				"Select a variable in the predicate to be removed first.",
				"GIPO Error",
				JOptionPane.WARNING_MESSAGE,
				null);
			return;
		}
		try {
			searcher.removeHighlights();
			fireEvent(new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
			doc.remove(lastSelPred.startOffset, lastSelPred.length + 1);
			followPreds =
				curExpModel.removePredicateAt(lastSelPred.startOffset + 1);
		} catch (Exception sme) {
			Utility.debugPrintln("Cannot remove predicate " + sme.toString());
			return;
		}
		try {
			if (followPreds.size() > 0) {
				setDocAttributes(followPreds);
			}

			lastSelPred = null;
			List nes = curExpModel.illegalNEs();
			if (nes.size() > 0) {
				ListIterator li = nes.listIterator();
				int count = 0;
				while (li.hasNext()) {
					int inx = ((Integer) li.next()).intValue() - count;
					curExpModel.neList.remove(inx);
					count++;
				}
			}
		} catch (Exception sme) {
			Utility.debugPrintln(
				"Cannot adjust remaining predicate" + sme.toString());
			return;
		}
	}

	/**
	 * setDocAttributes
	 * set or reset all the attributes - colour etc
	 * that applies to predicates in this pane
	 * @param predDets - list of PredDetails of predicates in the pane
	 */
	protected void setDocAttributes(List predDets) {
		ListIterator li = predDets.listIterator();
		while (li.hasNext()) {
			ExpressionModel.PredDetail cur =
				(ExpressionModel.PredDetail) li.next();
			if (cur.pred.isStatic()) {
				doc.setParagraphAttributes(
					cur.startOffset,
					cur.length,
					staticStyle,
					true);
			} else if (
				cur.pred.getName().equals("ne")
					|| cur.pred.getName().equals("is_of_sort")) {
				doc.setParagraphAttributes(
					cur.startOffset,
					cur.length,
					neStyle,
					true);
			} else {
				doc.setParagraphAttributes(
					cur.startOffset,
					cur.length,
					defaultStyle,
					true);
			}
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
	 * sameBindings
	 * ensure target and selected variable have same names and 
	 * remove nes if they exist
	 * @param evt Event 
	 */
	// Ron 28/8/01
	// Ron 11/10/02 changed to protected to allow overriding in Trans Doc
	protected void sameBindings(java.awt.event.ActionEvent evt) {
		// Change target Variable to match selected
		String select = null;
		Point selPoint = getSelectedVar();
		// 		    Utility.debugPrintln("<<>>Fired Same selPoint x = " + selPoint.x);
		try {
			if (selPoint.x != -1) {
				// Selection in this pane
				select = curExpModel.getArgumentAt(selPoint.x);
			} else {
				select = selectedArg;
			}
			Utility.debugPrintln("<<>> found select " + select);
			oclPredicate targPred = curExpModel.getPredicateAt(popupTarget.x);
			String oldVName = curExpModel.editVar(popupTarget.x, select);
			doc.remove(popupTarget.x, (popupTarget.y - popupTarget.x));
			doc.insertString(popupTarget.x, select, new SimpleAttributeSet());
			// Deal with NEs
			Utility.debugPrintln("<<>> Done insert");
			int neIndex = curExpModel.neExists(oldVName, select);
			Utility.debugPrintln("<<>> neIndex = " + neIndex);
			if (neIndex != -1) {
				if (JOptionPane.YES_OPTION
					== JOptionPane.showConfirmDialog(
						null,
						"Remove restriction clause  ne("
							+ oldVName
							+ ","
							+ select
							+ ")",
						"GIPO Query",
						JOptionPane.YES_NO_OPTION)) {
					curExpModel.neList.removeElementAt(neIndex);
				}
			}

			removeHighlights();
			Utility.debugPrintln("<<>> Removed highlights");

			fireEvent(new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
			Utility.debugPrintln("<<>> Fired CLEAR Event");
		} catch (Exception sme) {
			Utility.debugPrintln(
				"Unexpected failure to select argument" + sme.toString());
		}
	}

	/**
	 * enforceDifferentBindings
	 * ensure target and selected variable have different names and 
	 * appropriate ne clause exists
	 * @param evt Event 
	 */
	// Ron 11/10/02 changed to protected to allow overriding in Trans Doc
	protected void enforceDifferentBindings(java.awt.event.ActionEvent evt) {
		Point selPoint = getSelectedVar();
		try {
			String select = null;
			if (selPoint.x != -1) {
				// Selection in this pane
				select = curExpModel.getArgumentAt(selPoint.x);
			} else {
				select = selectedArg;
			}
			String targArg = curExpModel.getArgumentAt(popupTarget.x);
			String targSort = curExpModel.getSortForArgumentAt(popupTarget.x);
			if (select.equals(targArg)) { // Currently same
				// 		String newVName= JOptionPane.showInputDialog(null,
				// 			      "Enter New Name for Variable.");

				/* Weihong changed/added on 13/2/2002 */
				String newVName =
					GipoInputBox.showVarableInputBox(
						null,
						"GIPO Input",
						"Enter New Name for Variable.");
				if (newVName != null) {
					// Should check that it doesnt exist
					curExpModel.editVar(popupTarget.x, newVName);
					doc.remove(popupTarget.x, (popupTarget.y - popupTarget.x));
					doc.insertString(
						popupTarget.x,
						newVName,
						new SimpleAttributeSet());
					if (curExpModel.neExists(newVName, select) == -1) {
						oclPredicate curNE;
						if (curDomain
							.sortIsSubSortOf(selectedType, targSort)) {
							curNE =
								curExpModel.addNE(
									newVName,
									select,
									selectedType,
									doc.getLength());
						} else {
							curNE =
								curExpModel.addNE(
									newVName,
									select,
									targSort,
									doc.getLength());
						}
						int offset = doc.getLength();
						doc.insertString(
							offset,
							curNE + "\n".toString(),
							neStyle);
					}
					// redo highlights
					// 		if (selPoint.x != -1) {
					// 		    searcher.searchUnifiers(selectedUnifiers,
					// 					    searcher.selectedVar.x,
					// 					    searcher.selectedVar.y);
					// 		} else {
					// 		    searcher.searchUnifiers(selectedUnifiers,-1,-1);
					// 		}
					searcher.removeHighlights();
					fireEvent(
						new ExpressionPaneEvent(
							this,
							ExpressionPaneEvent.CLEAR));
				}
			} else {
				// Check that appropriate NE exists
				if (curExpModel.neExists(targArg, select) == -1) {
					oclPredicate curNE;
					if (curDomain.sortIsSubSortOf(selectedType, targSort)) {
						curNE =
							curExpModel.addNE(
								targArg,
								select,
								selectedType,
								doc.getLength());
					} else {
						curNE =
							curExpModel.addNE(
								targArg,
								select,
								targSort,
								doc.getLength());
					}
					doc.insertString(
						doc.getLength(),
						curNE + "\n".toString(),
						neStyle);
					searcher.removeHighlights();
					fireEvent(
						new ExpressionPaneEvent(
							this,
							ExpressionPaneEvent.CLEAR));

				}
				// Should check that appropriate NE exists
				Utility.debugPrintln(
					"These variables are already different"
						+ "\n given "
						+ targArg
						+ " and "
						+ select);
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
	// Ron 28/8/01 Bug Fix
	// Ron 11/10/02 changed to protected to allow for overriding
	protected void allowDifferentBindings(java.awt.event.ActionEvent evt) {
		Point selPoint = getSelectedVar();
		try {
			String select = null;
			if (selPoint.x != -1) {
				// Selection in this pane
				select = curExpModel.getArgumentAt(selPoint.x);
			} else {
				select = selectedArg;
			}
			String targArg = curExpModel.getArgumentAt(popupTarget.x);
			if (select.equals(targArg)) { // Currently same
				String newVName =
					JOptionPane.showInputDialog(
						null,
						"Enter New Name for Variable.");
				// Should check that it doesnt exist
				curExpModel.editVar(popupTarget.x, newVName);
				doc.remove(popupTarget.x, (popupTarget.y - popupTarget.x));
				doc.insertString(
					popupTarget.x,
					newVName,
					new SimpleAttributeSet());
				int neIndex = curExpModel.neExists(newVName, select);
				if (neIndex != -1) {
					if (JOptionPane.YES_OPTION
						== JOptionPane.showConfirmDialog(
							null,
							"Remove ne restriction clause",
							"GIPO Query",
							JOptionPane.YES_NO_OPTION)) {
						curExpModel.neList.removeElementAt(neIndex);
					}

				}
				// redo highlights
				searcher.removeHighlights();
				fireEvent(
					new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
			} else {
			}

		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to select argument");
		}
	}

	/**
	 * replaceAllVariables
	 * Find all variables in the model with the given name
	 * replace with the new name
	 * @param docPoint - the start of the
	 * @param oldV - the current name 
	 * @param newV - the new Variable name
	 * @return int - the start of the new selected variable
	 */
	public int replaceAllVariables(int docPoint, String oldV, String newV) {
		int diff = newV.length() - oldV.length();
		List predList = curExpModel.getExpressionList();
		ListIterator li = predList.listIterator();
		while (li.hasNext()) {
			ExpressionModel.PredDetail curPredDetail =
				(ExpressionModel.PredDetail) li.next();
			List args = curPredDetail.pred.getArguments();
			ListIterator liArgs = args.listIterator();
			int n = 1;
			while (liArgs.hasNext()) {
				oclPredicate.pArg arg = (oclPredicate.pArg) liArgs.next();
				if (oldV.equals(arg.name)) {
					// found one now edit it
					int offset = -1;
					try {
						offset = curPredDetail.pred.startElementNo(n);
					} catch (Exception e) {
						Utility.debugPrintln(
							"Unexpected failure to find start of argument");
						return 0;
					}
					try {
						int argStart = curPredDetail.startOffset + offset;
						curExpModel.editVar(argStart, newV);
						doc.remove(argStart, oldV.length());
						doc.insertString(
							argStart,
							newV,
							new SimpleAttributeSet());
						if (argStart < docPoint) {
							docPoint += diff;
						}
					} catch (Exception e) {
						Utility.debugPrintln(
							"Unexpected failure to edit variable" + e);
						return 0;
					}
				}
				n++;
			}
		}
		return docPoint;
	}

	/**
	 * initRestrictPopup
	 * init popup menu to allow renaming single variable
	 * create the popup menu + addition of is_of_sort clauses
	 */
	protected void initRestrictPopup() { /* WZ 8/5/02 change to protected */
		restrictPopupMenu = new JPopupMenu();
		JMenuItem renameMI = new JMenuItem("Re-Name");
		popupTarget = new Point(-1, -1);
		renameMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Change target Variable
				renameTargetVariable(evt);
			}
		});
		JMenuItem restrictMI = new JMenuItem("Restrict Sort");
		popupTarget = new Point(-1, -1);
		restrictMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Change target Variable
				restrictAL(evt);
			}
		});
		JMenuItem delRestrictMI = new JMenuItem("Delete Restriction");
		popupTarget = new Point(-1, -1);
		delRestrictMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Change target Variable
				delRestrictAL(evt);
			}
		});
		restrictPopupMenu.add(renameMI);
		restrictPopupMenu.add(restrictMI);
		restrictPopupMenu.add(delRestrictMI);

	}

	/**
	 * initDeletePopup
	 * init popup menu to allow deletion of ne and is_of_sort clauses
	 * create the popup menu
	 */
	protected void initDeletePopup() { /* WZ 27/8/02 */
		deletePopupMenu = new JPopupMenu();
		JMenuItem deleteMI = new JMenuItem("Delete");
		popupTarget = new Point(-1, -1);
		deleteMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Change target Variable
				deleteAL(evt);
			}
		});

		deletePopupMenu.add(deleteMI);
	}

	/**
	 * deleteAL
	 * event listener for delete menu - only delete predicate selected 
	 * @param evt the event causing this popup to show
	 */
	//ron 29/8/01 changed to protected
	protected void deleteAL(java.awt.event.ActionEvent evt) {
		Utility.debugPrintln("Fired del at pos " + clickPos);
		try {
			ExpressionModel.PredDetail curPredDetail =
				curExpModel.getPredDetailAt(clickPos);
			Utility.debugPrintln("Found pred " + curPredDetail.pred.toString());
			if (curPredDetail.pred.getName().equals("is_of_sort")) {
				List followPreds =
					curExpModel.removeRestrictionForVariable(
						curPredDetail.pred.getNthElementName(0));
				doc.remove(curPredDetail.startOffset, curPredDetail.length + 1);
				// redo highlights
				followPreds.remove(0);
				setDocAttributes(followPreds);
				searcher.removeHighlights();
				fireEvent(
					new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
			} else {
				Utility.debugPrintln("Must be NE");
				if (curPredDetail.pred.getName().equals("ne")) {
					curExpModel.removeNE(curPredDetail.pred);
					Utility.debugPrintln("Removed hidden NE");
				}
				//Now remove the predicate in the visable list
				List followPreds = curExpModel.removePredicateAt(clickPos);
				Utility.debugPrintln(
					"Pred To DELETE Offset = "
						+ curPredDetail.startOffset
						+ " length = "
						+ curPredDetail.length
						+ " clickPos = "
						+ clickPos
						+ " pred "
						+ curPredDetail.pred.toString());
				doc.remove(curPredDetail.startOffset, curPredDetail.length + 1);
				if (followPreds.size() > 0) {
					setDocAttributes(followPreds);
				}

				// redo highlights
				searcher.removeHighlights();
				fireEvent(
					new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
			}
		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to delete predicate");
		}
	}

	/**
	 * delRestrictAL
	 * event listener for restrict popup menu
	 * @param evt the event causing this popup to show
	 */
	private void delRestrictAL(java.awt.event.ActionEvent evt) {
		Utility.debugPrintln("Fired del Restrict");
		List followPreds = null;
		ExpressionModel.PredDetail curPredDetail;
		try {
			followPreds = curExpModel.removeRestrictionForVariable(selectedArg);
			try {
				curPredDetail = (ExpressionModel.PredDetail) followPreds.get(0);
				doc.remove(curPredDetail.startOffset, curPredDetail.length + 1);
				followPreds.remove(0);
				setDocAttributes(followPreds);
			} catch (BadLocationException e) {
				Utility.debugPrintln(
					"Unexpected failure to remove predicate from document");
			}
		} catch (ExpressionModel.ExpressionException e) {
			JOptionPane.showMessageDialog(
				this,
				"No restriction for this argument.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
		searcher.removeHighlights();
		fireEvent(new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
	}

	/**
	 * restrictAL
	 * event listener for delete restriction popup menu
	 * @param evt the event causing this popup to show
	 */
	private void restrictAL(java.awt.event.ActionEvent evt) {
		Utility.debugPrintln("Fired Restrict");
		List subTypes = curDomain.getSortSubTypes(selectedType);
		Object options[] = subTypes.toArray();
		String restrictSort =
			(String) JOptionPane.showInputDialog(
				this,
				"Select the restricting sort",
				"GIPO Query",
				JOptionPane.QUESTION_MESSAGE,
				null,
				options,
				options[0]);
		oclPredicate restrictPred = new oclPredicate("is_of_sort");
		restrictPred.addVarArgument(selectedArg);
		restrictPred.addConstArgument(restrictSort);
		Utility.debugPrintln("Restrict  " + restrictPred.toString());
		if (curExpModel.checkRestriction(restrictPred)) {
			JOptionPane.showMessageDialog(
				this,
				"A restriction for this argument already exists.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		} else {
			try {
				addPredicate(restrictPred);
			} catch (OCLSelectionException e) {
				Utility.debugPrintln(
					"Failed to add restriction Clause "
						+ restrictPred.toString());
			}
			//searcher.removeHighlights();
			searcher.removeHighlights();
			fireEvent(new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
		}
	}

	/**
	 * removeHighlights
	 * remove all the highlights from this pane
	 */
	public void removeHighlights() {
		searcher.removeHighlights();
	}

}

/**
 * class to search for occurrances of the given sort name
 * and highlight (underline) those words
 */
class ExpSearcher {
	public ExpSearcher(JTextComponent comp, ExpressionModel expModel) {
		this.comp = comp;
		this.painter =
			new UnderlineHighlighter.UnderlineHighlightPainter(Color.red);
		this.selPainter =
			new UnderlineHighlighter.UnderlineHighlightPainter(Color.green);
		this.expModel = expModel;
		vars = new ArrayList();
		selectedVar = new Point(-1, -1);
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
			if (h.getPainter()
				instanceof UnderlineHighlighter.UnderlineHighlightPainter) {
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
				curPred = expModel.getPredicateAt(lastIndex + 1);
			} catch (Exception e) {
				Utility.debugPrintln(
					"No predicate found at " + (lastIndex + 1));
				break;
			}
			lastIndex += curPred.getName().length() + 1;
			List args = curPred.getArguments();
			ListIterator li = args.listIterator();
			while (li.hasNext()) {
				String argName = ((oclPredicate.pArg) li.next()).name;
				if (sort.equals(argName)) {
					int endIndex = lastIndex + argName.length();
					try {
						highlighter.addHighlight(lastIndex, endIndex, painter);
						// 			Utility.debugPrintln("red highlight [search]... "+curPred.toString());
					} catch (BadLocationException e) {
						Utility.debugPrintln("Unexpected failure - search");
						// Nothing to do
					}
				}
				// Adjust indexes
				lastIndex += argName.length() + 1; // +1 for comma or )
			}
			lastIndex++; // increment for return
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
	public void searchOthers(String sort, int from, int to) {
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
			ExpressionModel.PredDetail curDet;
			try {
				//curPred = expModel.getPredicateAt(lastIndex + 1);
				curDet = expModel.getPredDetailAt(lastIndex + 1);
				curPred = curDet.pred;
				curProto = curDet.proto;
			} catch (Exception e) {
				Utility.debugPrintln(
					"No predicate found at " + (lastIndex + 1));
				break;
			}
			lastIndex += curPred.getName().length() + 1;
			List args = curPred.getArguments();
			ListIterator li = args.listIterator();
			List protoArgs = curProto.getArguments();
			ListIterator liProto = protoArgs.listIterator();
			while (li.hasNext()) {
				String argName = ((oclPredicate.pArg) li.next()).name;
				String sortName = ((oclPredicate.pArg) liProto.next()).name;
				if (sort.equals(sortName)) {
					int endIndex = lastIndex + argName.length();
					if ((endIndex < from) || (lastIndex > to)) { //SKIP
						try {
							highlighter.addHighlight(
								lastIndex,
								endIndex,
								painter);
							// 			    Utility.debugPrintln("red highlight [searchOthers]... "+curPred.toString());
							vars.add(new Point(lastIndex, endIndex));
						} catch (BadLocationException e) {
							Utility.debugPrintln(
								"Unexpected failure - searchOthers");
							// Nothing to do
						}
					}
				}
				// Adjust indexes
				lastIndex += argName.length() + 1; // +1 for comma or )
			}
			lastIndex++; // increment for return
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
	public void searchUnifiers(List unifiers, int from, int to) {
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
			ExpressionModel.PredDetail curDet;
			try {
				//curPred = expModel.getPredicateAt(lastIndex + 1);
				curDet = expModel.getPredDetailAt(lastIndex + 1);
				curPred = curDet.pred;
				curProto = curDet.proto;
			} catch (Exception e) {
				Utility.debugPrintln(
					"No predicate found at " + (lastIndex + 1));
				break;
			}
			lastIndex += curPred.getName().length() + 1;
			List args = curPred.getArguments();
			ListIterator li = args.listIterator();
			List protoArgs = curProto.getArguments();
			ListIterator liProto = protoArgs.listIterator();
			while (li.hasNext()) {
				String argName = ((oclPredicate.pArg) li.next()).name;
				String sortName = ((oclPredicate.pArg) liProto.next()).name;
				ListIterator liUnifiers = unifiers.listIterator();
				boolean found = false;
				while (!found && liUnifiers.hasNext()) {
					String sort = (String) liUnifiers.next();
					if (sort.equals(sortName)) {
						found = true;
						int endIndex = lastIndex + argName.length();
						if ((endIndex < from) || (lastIndex > to)) { //SKIP
							try {
								highlighter.addHighlight(
									lastIndex,
									endIndex,
									painter);
								// 				Utility.debugPrintln("red highlight [searchUnifiers]... "+curPred.toString());
								vars.add(new Point(lastIndex, endIndex));
							} catch (BadLocationException e) {
								Utility.debugPrintln(
									"Unexpected failure - searchOthers");
								// Nothing to do
							}
						}
					}
				}
				// Adjust indexes
				lastIndex += argName.length() + 1; // +1 for comma or )
			}
			lastIndex++; // increment for return
		}
	}

	/**
	 * markSelected - Underline the selected word
	 * @param from - index of start of text
	 * @param to   - index of end of selected text
	 */
	public void markSelected(int from, int to) {
		Highlighter highlighter = comp.getHighlighter();
		// Remove any existing highlights for last selected
		Highlighter.Highlight[] highlights = highlighter.getHighlights();
		for (int i = 0; i < highlights.length; i++) {
			Highlighter.Highlight h = highlights[i];
			if (h.getPainter()
				instanceof UnderlineHighlighter.UnderlineHighlightPainter) {
				highlighter.removeHighlight(h);
			}
		}
		try {
			highlighter.addHighlight(from, to, selPainter);
			// 	    Utility.debugPrintln("green highlight [markSelected]... "+from+","+to);
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
			if (h.getPainter()
				instanceof UnderlineHighlighter.UnderlineHighlightPainter) {
				highlighter.removeHighlight(h);
			}
		}
	}

	protected JTextComponent comp;
	protected Highlighter.HighlightPainter painter;
	protected Highlighter.HighlightPainter selPainter;
	protected ExpressionModel expModel;
	public List vars;
	public Point selectedVar;

}

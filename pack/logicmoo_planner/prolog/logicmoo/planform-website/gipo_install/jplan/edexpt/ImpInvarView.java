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

package jplan.edexpt;

/**
 * ImpInvarView
 * This is the GIPO expert editor for implied_invariant(s)
 * Part of the edexpt (Editor for Experts) Views
 * @author Ron Simpson
 * @version 0
 */

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import javax.swing.text.*;
import java.util.List;
import java.util.*;
import java.util.NoSuchElementException;
import java.awt.event.*;
import java.awt.*;
import java.beans.*;


import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.edexpt.BaseTree.SortSelectionException;
import jplan.general.*;
import jplan.images.ImageLoader; 	/* Weihong changed on 5/9/2001 */


public class ImpInvarView extends GipoInternalFrame {/* Weihong added on 24/10/2001 */
    private OclEd top;
    private oclDomain curDomain;
    private List sorts;
    private List lstPreds;          // This is the working list
    private DefaultListModel lmImpInvars; 
    // This is all the implied_invariants
    private int selPredIndex = -1; // The index of the currently edited
                                   //predicate in the list box
    private boolean dirty = false; // flag to monitor changes being committed
    // A simple view of this would be if there is any text in the edit
    // pane then the editing is dirty!

    // Interface components or their Models
    private SortOnlyTree sortTree;
    private JList jlstPreds;
    private DefaultListModel lmPreds;

    private JRadioButton jradSelected;
    private JRadioButton jradReferenced;
    private JRadioButton jradAll;
    private JButton cmdUpdateSel;

    // The antecedents of the implication
    private FullExpressionDocPane antePane; 
    // The consequents of the implication
    private FullExpressionDocPane conseqPane; 
    // The pane last generated a change event
    private FullExpressionDocPane changePane = null; 

    private JList jlstRestrict;
    private String selectedSort = "none"; 
    //the selected predicate argument sort
 
    // edit window

    // The Inconsistent Constraints List box
    private JList jlstImpInvars;

    private JTextField jtxtVName;

    /**
     * Constructor
     * @param curDomain the current active domain
     * @param parent top level reference
     */
    public ImpInvarView (oclDomain curDomain,OclEd parent) {
//     super("",true,true,true,true);
	super(parent);	/* Weihong added on 24/10/2001 */
	setClosable(false); 	/* Weihong added on 11/10/2001 */
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(parent,
		"No Domain currently being edited.",
		 "GIPO Error",
		 JOptionPane.ERROR_MESSAGE,
		 null);	    
	    return;
	}
	setTitle("Implied Invariants Editor (Expert)");
	sorts = curDomain.sorts;   //had reference to parent
	lstPreds = curDomain.predicates;
	lmImpInvars = new DefaultListModel(); 
	//Need to clone the implied_invariants
	ListIterator li = curDomain.impliedInvars.listIterator();
	try {
	    while (li.hasNext()) {
		lmImpInvars.addElement(((oclImpliedInvar)li.next()).clone());
	    }
	}catch(CloneNotSupportedException e) {
	    // This should not happen
	    Utility.debugPrintln("Cannot clone substates. " + e.toString());
	}
	this.curDomain = curDomain;
	top = parent;
	initComponents();
	dirty = false;
	pack();
	setVisible(true);
    }
    
    /**
     * initComponents
     * Sets up the user interface
     */
    private void initComponents() {
	int tWidth;

	JButton cmdDummy = new JButton("XXX");
	int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
	cmdDummy = null;

	Box boxTop = Box.createVerticalBox();  // Holds views + final buttons
	Box boxEd = Box.createHorizontalBox(); // Holds the main panes
	Box boxFinalButtons = Box.createHorizontalBox();

	// The first Vertical pane contains the Sort Tree
 
	//  Now put the sort components together
	sortTree = new SortOnlyTree(curDomain);
	// Don't listen to this tree selections only important to allow
	// filtering of predicates

	JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
	TitledBox tbSorts = new TitledBox("Sorts ..",scrollPaneSorts);

	// The Second Vertical pane contains the Predicate List
	// Now deal with Predicates - maintain a list
	// And A set of Filter Controls
	// A panel to add and delete predicates

	Box boxPreds = Box.createVerticalBox();
	// First the predicate List
	lmPreds = new DefaultListModel();
	// Fill the model;
	ListIterator liPreds = lstPreds.listIterator();
 	while (liPreds.hasNext()) {
	    lmPreds.addElement(liPreds.next());
	}
	jlstPreds = new JList(lmPreds);
	jlstPreds.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	ListCellRenderer renderer = new PredCellRenderer(
                         top.strImageDir , "static.gif");
	jlstPreds.setCellRenderer(renderer);
	JScrollPane scrollPanePreds = new JScrollPane(jlstPreds);
	TitledBox tbPreds = new TitledBox("Predicates ..",scrollPanePreds);
	//maark1

	// Second the Filter Radio Buttons
	JPanel jpanFilter = new JPanel();
	jpanFilter.setBorder(BorderFactory.createTitledBorder("Filter By .."));

	jradSelected = new JRadioButton("First Reference Only",false);
	jradReferenced = new JRadioButton("All Referenced",false);
	jradAll = new JRadioButton("All Predicates",true);
	jradSelected.setMnemonic(KeyEvent.VK_F);
	jradReferenced.setMnemonic(KeyEvent.VK_R);
	jradAll.setMnemonic(KeyEvent.VK_A);   
	Box boxFilter = Box.createVerticalBox();
	ButtonGroup groupFilter = new ButtonGroup();
	boxFilter.add(jradSelected);
	boxFilter.add(jradReferenced);
	boxFilter.add(jradAll);
	groupFilter.add(jradSelected);
	groupFilter.add(jradReferenced);
	groupFilter.add(jradAll);
	/* Weihong added on 4/07/2001 */
// 	ImageIcon ii = new ImageIcon(top.strImageDir + "Refresh16.gif");
	/* Weihong changed on 4/9/2001 */
	ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
	cmdUpdateSel = new JButton("Refresh Selection", ii);
	cmdUpdateSel.setMnemonic(KeyEvent.VK_S);
	cmdUpdateSel.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    updatePredSelection();
		}
	    }
				  );
	boxFilter.add (cmdUpdateSel);
	jpanFilter.add(boxFilter);

	boxPreds.add(tbPreds);
// 	boxPreds.add(boxAddRem);
// 	boxPreds.add(boxAddRemConseq);
	boxPreds.add(jpanFilter);

	// Build the StateEditor Box
	 Box boxInvarEd = buildImpInvarEditorBox();

	 // Build the inconsistent_constraints List
	 jlstImpInvars = new JList(lmImpInvars);
	 jlstImpInvars.setToolTipText("Select invariant description to edit.");
	 jlstImpInvars.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	 jlstImpInvars.addListSelectionListener(negInvarSelListener);
	 JScrollPane jscrollImpInvars = new JScrollPane(jlstImpInvars);
	 ListCellRenderer plRenderer = new PredListCellRenderer();
	 jlstImpInvars.setCellRenderer(plRenderer);
	 JLabel dummy = new JLabel("Existing Invariants ..");
	 Dimension dDim = dummy.getPreferredSize();
	 Dimension sDim = jscrollImpInvars.getPreferredSize();
	 jscrollImpInvars.setPreferredSize(new Dimension(dDim.width,sDim.height));
	 TitledBox tbInvars = new TitledBox("Existing Invariants ..",jscrollImpInvars);

	

	// Create The Final Buttons
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "SendMail16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
	JButton cmdCommit = new JButton("Commit", ii);
	cmdCommit.setMnemonic(KeyEvent.VK_M);
	cmdCommit.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    commitEdits();
		}
	    }
				  );
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Remove16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Remove16.gif");
	JButton cmdRestore = new JButton ("Restore", ii);
	cmdRestore.setMnemonic(KeyEvent.VK_R);
	cmdRestore.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    // Save COPY of the domain predicates
		    lmImpInvars.clear();
		    ListIterator li = 
			curDomain.impliedInvars.listIterator();
		    try {
			while (li.hasNext()) {
			    lmImpInvars.addElement(((oclImpliedInvar)li.next()).clone());
			}
		    }catch(CloneNotSupportedException e) {
			// This should not happen
			Utility.debugPrintln("Cannot clone substates. " + 
					   e.toString());
		    }
		    clearEditInvar();
		    dirty = false;
		}
	    }
				  );
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Replace16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
	JButton cmdVerify = new JButton("Verify States", ii);
	cmdVerify.setMnemonic(KeyEvent.VK_V);
	/* Weihong added on 4/07/2001 */
// 	JButton cmdCancel = new JButton("Cancel");
// 	cmdCancel.setMnemonic(KeyEvent.VK_N);
// 	cmdCancel.addActionListener (new java.awt.event.ActionListener () {
// 		public void actionPerformed (java.awt.event.ActionEvent evt) {
// 		    // Just assume that the know what they are doing
// 		    setVisible (false);
// 		    dispose ();	    
// 		}		    
// 	    }
// 				     );
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Stop16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
	JButton cmdClose = new JButton ("Close", ii);
	cmdClose.setMnemonic(KeyEvent.VK_L);
	cmdClose.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    closeDialog();
		}
	    }
				    );

	boxFinalButtons.add(cmdCommit);
	boxFinalButtons.add(cmdRestore);
	boxFinalButtons.add(cmdVerify);
// 	boxFinalButtons.add(cmdCancel);	/* Weihong added on 4/07/2001 */
	boxFinalButtons.add(cmdClose);
	
	
	boxEd.add(tbSorts);
	boxEd.add(boxPreds);
	boxEd.add(boxInvarEd);
	boxEd.add(tbInvars);

	boxTop.add(boxEd);
	boxTop.add(boxFinalButtons);
	
	getContentPane().add(boxTop);
	
    } 

    /**
     * Do the main editor panel with 2 edit boxes  for the elements
     * of the implication
     */
    private Box buildImpInvarEditorBox(){
	int tWidth;

	JButton cmdDummy = new JButton("XXX");
	int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
	cmdDummy = null;
	// This is the whole box
	Box invarEdBox = Box.createVerticalBox();
	// First the antecedent panel
	JPanel jpanAnte = new JPanel();
	jpanAnte.setLayout(new BoxLayout(jpanAnte,BoxLayout.Y_AXIS));
	jpanAnte.setBorder(BorderFactory.createTitledBorder("Implication Antecedent .."));
	
	antePane = new FullExpressionDocPane(curDomain);
	antePane.addChangeListener(new ExpressionPaneListener() {
		public void getChange(ExpressionPaneEvent evt) {
		    if (evt.getID() == ExpressionPaneEvent.SELECTION) {
			changePane = antePane;
			String varName = evt.getVName();
			selectedSort = antePane.getSelectedSort();
			conseqPane.highlightForeignUnifiers(antePane.getSelectedUnifiers(),selectedSort,varName);
		    } else if (evt.getID() == ExpressionPaneEvent.CLEAR) {			
			changePane = antePane;

			selectedSort = antePane.getSelectedSort();
			conseqPane.removeHighlights();
		    } else if (evt.getID() == ExpressionPaneEvent.RENAME) {
			if (evt.getScope() == ExpressionPaneEvent.GLOBAL) {
			    changePane = antePane;
			    conseqPane.replaceAllVariables(0,evt.getOldVName(),
							   evt.getVName());
			}
		    }
		    
		}
	    }
				   );
	JScrollPane jscrollInvarAnteEd = new JScrollPane(antePane);
	jscrollInvarAnteEd.setPreferredSize(new Dimension(50,30));/* Weihong changed on 05/07/2001 */
	jpanAnte.add(jscrollInvarAnteEd);

	// The predicate add/delete button
	// Need two pairs one to add to the antecedent
	// other to add to the consequent
	Box boxAddRem = Box.createHorizontalBox();
// 	ImageIcon ii = new ImageIcon(top.strImageDir + "Cut16.gif");	/* Weihong added on 4/07/2001 */
	/* Weihong changed on 4/9/2001 */
	ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
	JButton cmdRemPred = new JButton("<< Remove", ii);
	cmdRemPred.setHorizontalTextPosition(AbstractButton.LEFT);/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Properties16.gif");/* Weihong added on 4/07/2001 */
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Properties16.gif");
	JButton cmdAddPred = new JButton("Add >>", ii);
	cmdAddPred.setHorizontalTextPosition(AbstractButton.RIGHT);/* Weihong added on 4/07/2001 */
	cmdAddPred.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    addPredToInvar(antePane);
		}
	    }
				  );
	cmdRemPred.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    antePane.removePredFromState();
		    conseqPane.removeHighlights();
		    changePane = null;
		}
	    }
				  );
	boxAddRem.add(cmdRemPred);
	boxAddRem.add(cmdAddPred);
	jpanAnte.add(boxAddRem);   /* Weihong added on 05/07/2001 */
	invarEdBox.add(jpanAnte);

	// Now the Consequent panel
	JPanel jpanConseq = new JPanel();
	jpanConseq.setLayout(new BoxLayout(jpanConseq,BoxLayout.Y_AXIS));
	jpanConseq.setBorder(BorderFactory.createTitledBorder("Implication Consequent .."));
	
	conseqPane = new FullExpressionDocPane(curDomain);
	conseqPane.addChangeListener(new ExpressionPaneListener() {
		public void getChange(ExpressionPaneEvent evt) {
		    if (evt.getID() == ExpressionPaneEvent.SELECTION) {
			changePane = conseqPane;
			String varName = evt.getVName();
			selectedSort = conseqPane.getSelectedSort();
			antePane.highlightForeignUnifiers(conseqPane.getSelectedUnifiers(),selectedSort,varName);
		    } else if (evt.getID() == ExpressionPaneEvent.CLEAR) {			
			changePane = conseqPane;

			selectedSort = conseqPane.getSelectedSort();
			antePane.removeHighlights();
		    } else if (evt.getID() == ExpressionPaneEvent.RENAME) {
			if (evt.getScope() == ExpressionPaneEvent.GLOBAL) {
			    changePane = conseqPane;
			    antePane.replaceAllVariables(0,evt.getOldVName(),
							   evt.getVName());
			}
		    }
								       
		}
	    }
				   );
	JScrollPane jscrollInvarConseqEd = new JScrollPane(conseqPane);
	jscrollInvarConseqEd.setPreferredSize(new Dimension(50,30));/* Weihong added on 05/07/2001 */
	jpanConseq.add(jscrollInvarConseqEd);

	//Now the pair for the consequent
	Box boxAddRemConseq = Box.createHorizontalBox();
// 	ii = new ImageIcon(top.strImageDir + "Cut16.gif");	/* Weihong added on 4/07/2001 */
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
	JButton cmdRemConseq = new JButton("<< Remove", ii);
	cmdRemConseq.setHorizontalTextPosition(AbstractButton.LEFT);/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Properties16.gif");/* Weihong added on 4/07/2001 */
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Properties16.gif");
	JButton cmdAddConseq = new JButton("Add >>", ii);
	cmdAddConseq.setHorizontalTextPosition(AbstractButton.RIGHT);/* Weihong added on 4/07/2001 */
	cmdAddConseq.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    addPredToInvar(conseqPane);
		}
	    }
				  );
	cmdRemConseq.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    conseqPane.removePredFromState();
		    antePane.removeHighlights();
		    changePane = null;
		}
	    }
				  );
	boxAddRemConseq.add(cmdRemConseq);
	boxAddRemConseq.add(cmdAddConseq);
	jpanConseq.add(boxAddRemConseq);/* Weihong changed on 05/07/2001 */
	invarEdBox.add(jpanConseq);

	// The radio buttons to control unifying
	JPanel jpanUnify = new JPanel();
	jpanUnify.setLayout(new BoxLayout(jpanUnify,BoxLayout.Y_AXIS));
	jpanUnify.setBorder(BorderFactory.createTitledBorder("Implied Invariant .."));

	Box boxVName = Box.createHorizontalBox();
	JLabel jlblVName = new JLabel("Re-name");
	OCLVariableDocument varDoc = new OCLVariableDocument();
	jtxtVName = new JTextField(10);
	tWidth = jtxtVName.getPreferredSize().width;
	jtxtVName.setMaximumSize(new Dimension(tWidth,txtFieldMaxHeight));
	jtxtVName.setDocument(varDoc);
	ActionListener alVName = new ActionListener() {
		public void actionPerformed(ActionEvent av) {
		    if (changePane == null) {
			JOptionPane.showMessageDialog(ImpInvarView.this,
					  "Select a variable to edit First.",
					   "GIPO Error",
					   JOptionPane.ERROR_MESSAGE,
					   null);
			return;
		    }
		    try {
			String newVName = jtxtVName.getText();
			changePane.editVarName(newVName);
		    } catch (OCLSelectionException ose) {
			Utility.debugPrintln("Var Update failed " + ose.toString());
			return;
		    }
		    Utility.debugPrintln("\nVAR = "+ jtxtVName.getText());
		}
	    };
	jtxtVName.addActionListener(alVName);
	boxVName.add(jlblVName);
	boxVName.add(jtxtVName);
	jpanUnify.add(boxVName);


	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "AlignCenter16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "AlignCenter16.gif");
	JButton cmdClear = new JButton("Clear", ii);
	cmdClear.setMnemonic(KeyEvent.VK_C);
	cmdClear.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    clearEditInvar();
		}
	    }
				  );
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Refresh16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
	JButton cmdUpdateSel = new JButton("Update", ii);
	cmdUpdateSel.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    updateInvarList();
		}
	    }
					);
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Add16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
	JButton cmdAddNew = new JButton("Add", ii);
	cmdAddNew.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    addImpInvar();
		}
	    }
				     );
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Delete16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
	JButton cmdDelSel = new JButton("Delete", ii);
	cmdDelSel.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    delSelectedInvar();
		}
	    }
				     );
	Box boxEditButtons = Box.createHorizontalBox();
	boxEditButtons.add(cmdClear);
	boxEditButtons.add(cmdAddNew);
	boxEditButtons.add(cmdUpdateSel);
	boxEditButtons.add(cmdDelSel);
	jpanUnify.add (boxEditButtons);
	
	invarEdBox.add(jpanUnify);
	return invarEdBox;
    }

    /**
     * updatePredSelection - update the filtering on the displayed predicates
     */
    private void updatePredSelection () {
	String sortName = "none";
	List tmpPreds;
	if (! jradAll.isSelected()) {
	    try {
		sortName = sortTree.getSelectedNodeName();
	    } catch (SortSelectionException e) {
		JOptionPane.showMessageDialog(this,
					      e.getMessage(),
					      "GIPO Error",
					      JOptionPane.ERROR_MESSAGE,
					      null);	    
		return;
	    }
	}
	if (jradSelected.isSelected()) {
	    tmpPreds =  
		curDomain.getPredicatesByFirstRefSortFromList(sortName,lstPreds,false);
	    lmPreds = new DefaultListModel();
	    // Fill the model;
	    ListIterator liPreds = tmpPreds.listIterator();
	    while (liPreds.hasNext()) {
		lmPreds.addElement(liPreds.next());
	    }
	    jlstPreds.setModel(lmPreds);
	} else if(jradReferenced.isSelected()) {
	    tmpPreds =  
		curDomain.getPredicatesBySortFromList(sortName,lstPreds,false);
	    lmPreds = new DefaultListModel();
	    // Fill the model;
	    ListIterator liPreds = tmpPreds.listIterator();
	    while (liPreds.hasNext()) {
		lmPreds.addElement(liPreds.next());
	    }
	    jlstPreds.setModel(lmPreds);
	} else {
	    tmpPreds = lstPreds;
	    lmPreds = new DefaultListModel();
	    // Fill the model;
	    ListIterator liPreds = tmpPreds.listIterator();
	    while (liPreds.hasNext()) {
		lmPreds.addElement(liPreds.next());
	    }
	    jlstPreds.setModel(lmPreds);
	}
    }

    /**
     * ListSelectionListener
     * Updates editing boxed when a Negative Invariant is choosen
     */
    ListSelectionListener negInvarSelListener = 
	new ListSelectionListener() {
		public void valueChanged(ListSelectionEvent lse) {
		    try {
			if (! lse.getValueIsAdjusting()) {
			    JList lst = (JList)lse.getSource();
			    oclImpliedInvar invar 
				= (oclImpliedInvar)lst.getSelectedValue();
			    if (invar == null) {
				Utility.debugPrintln("List selection problem");
			    } else {
				antePane.clearPane();
				conseqPane.clearPane();
				changePane = null;
				ListIterator li = 
				    invar.getLeftList().listIterator();
				while(li.hasNext()) {
				    oclPredicate cur = (oclPredicate)li.next();
				    antePane.addPredicate(cur);
				}
				li = 
				    invar.getRightList().listIterator();
				while(li.hasNext()) {
				    oclPredicate cur = (oclPredicate)li.next();
				    conseqPane.addPredicate(cur);
				}
			    }
			    changePane = null;
			}
		    } catch (Exception e) {
			Utility.debugPrintln("Selection exception " + e.toString());
			// Should not happen!!
		    }
		}
	    };


    /**
     * addPredToInvar - add the currently selected predicate
     * to the invariant being edited
     */
    private void addPredToInvar(FullExpressionDocPane pane) {
	if(jlstPreds.isSelectionEmpty()) {
	    JOptionPane.showMessageDialog(this,
					  "Please select a predicate first.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);
	    return;
	}
	oclPredicate selPred = (oclPredicate)jlstPreds.getSelectedValue();
	oclPredicate stndPred = selPred.copySortsToVars();
	try {
	    pane.addPredicate(stndPred);
	} catch (Exception e) {
	    Utility.debugPrintln("Failed to insert predicate " + e.toString());
	}
	try {
	    conseqPane.removeHighlights();
	    antePane.removeHighlights();
	} catch (Exception e) {
	    Utility.debugPrintln("Failed to deal with hihjlights " + 
			       e.toString());
	}
	changePane = null;
    }


    /**
     * clearEditInvar - abandon and clear the edit pane
     */
    public void clearEditInvar(){
	try {
	    selectedSort = "none";
	    antePane.clearPane();
	    conseqPane.clearPane();
	    jlstImpInvars.clearSelection();
	} catch (Exception e) {
	    Utility.debugPrintln("Failed to remove document text or model");
	}
    }
    /**
     * updateInvarList - replace the state list selected state with
     * the result of editing the state
     */
    private void updateInvarList(){
	int inx = jlstImpInvars.getSelectedIndex();
	if (inx == -1 ) {
	    JOptionPane.showMessageDialog(this,
			"No no existing state being edited.",
			 "GIPO Error",
			 JOptionPane.ERROR_MESSAGE,
		   null);
	    return;
	}
	lmImpInvars.remove(inx);
 	oclImpliedInvar invar = new oclImpliedInvar();
	List left = antePane.getPlainList();
	ListIterator li = left.listIterator();
	while(li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    invar.addLeft(cur);
	}
 	List right = conseqPane.getPlainList();
	li = right.listIterator();
	while(li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    invar.addRight(cur);
	}
	lmImpInvars.insertElementAt(invar,inx);
	clearEditInvar();
    }

    /**
     * delSelectedInvar - delete the selected item for the
     * state definition list
     */
    private void delSelectedInvar(){	
	int inx = jlstImpInvars.getSelectedIndex();
	if (inx == -1 ) {
	    JOptionPane.showMessageDialog(this,
			"No no existing state being edited.",
			 "GIPO Error",
			 JOptionPane.ERROR_MESSAGE,
		   null);
	    return;
	}
	lmImpInvars.remove(inx);
	clearEditInvar();
    }

    /**
     * addImpInvar - add the edited state as a new state in the state list
     */
    private void addImpInvar() {
	if (antePane.getDocumentLength() == 0 ||
	    conseqPane.getDocumentLength() == 0) {
	    JOptionPane.showMessageDialog(this,
 			"Define both the Antecedents and the\n Consequent of the implication first.",
 			"GIPO Warning",
 			 JOptionPane.ERROR_MESSAGE);
 	    return;
	}
 	int inx = lmImpInvars.getSize();
 	oclImpliedInvar invar = new oclImpliedInvar();
	List left = antePane.getPlainList();
	ListIterator li = left.listIterator();
	while(li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    invar.addLeft(cur);
	}
 	List right = conseqPane.getPlainList();
	li = right.listIterator();
	while(li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    invar.addRight(cur);
	}
 	lmImpInvars.insertElementAt(invar,inx);
	clearEditInvar();
    }


    /** Closes the dialog */
    private void closeDialog() {
	if (antePane.isDirty() || conseqPane.isDirty()) {
	    int res = JOptionPane.showConfirmDialog(ImpInvarView.this,
			"Changes not Committed - Exit anyway.",
			"GIPO Warning",
			 JOptionPane.YES_NO_OPTION);
	    if (res == JOptionPane.YES_OPTION) {
		setVisible (false);
		dispose ();	    
	    } else {
		return;
	    }
	} else {
	    setVisible (false);
	    dispose ();	 
	}			  
    }

    /**
     * commitEdits - save the changes back into main domain model
     */
    private void commitEdits() {
	// Check of possible ongoing edits
	if (antePane.isDirty() || conseqPane.isDirty()) {
	    JOptionPane.showMessageDialog(this,
			"Possible on-going edit - clear edits before commiting changes.",
			"GIPO Warning",
			 JOptionPane.ERROR_MESSAGE);
	    return;
	}
	curDomain.impliedInvars.clear();
	for(int i = 0; i < lmImpInvars.size();i++) {
	    try {
		curDomain.impliedInvars.add(
		      ((oclImpliedInvar)lmImpInvars.get(i)).clone());
	    } catch (CloneNotSupportedException cne) {
		//!!!
		Utility.debugPrintln("Cannot clone StateDefinitions");
	    }
	}

	/* Weihong added on 18/07/2001 */
	top.updateWindow(getTitle());
    }
}


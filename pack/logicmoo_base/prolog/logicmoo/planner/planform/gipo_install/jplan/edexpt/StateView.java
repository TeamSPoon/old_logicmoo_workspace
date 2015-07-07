/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D.Liu D. Kitchin
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
 * StateView
 * This is the GIPO expert editor for substate class expressions
 * Part of the edexpt (Editor for Experts) Views
 * @author Ron Simpson
 * @version 0
 */

/**
 * History:
 * Ron 18/10/02 - changed abilty of filter on predicates to filter on sorts
 *                unifying with the selected sort
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
import jplan.images.ImageLoader; /* WZ 5/9/2001 */

public class StateView extends GipoInternalFrame { /* WZ 24/10/2001 */
	private OclEd top;
	private oclDomain curDomain;
	private List sorts;
	private List curSorts;//Sort list that current using,added by donghong 2/12/02
	private List lstPreds; // This is the working list
	private List lstSSC; // This is all the classdefs 
	private List cplstSSC; //a copied classdefs of lstSSc, added by donghong 29/11/02
	private List cplstPreds; //a copied predicates editing, added by donghong 03/12/02
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
	private JCheckBox jchkUnify; //Ron 18/10/02 allow filters to unify

	private StateExpressionDocPane editPane;

	//    private JTextField jtxtVName;
	private JLabel jlblIndicator; //used to indicate the currently edited sort

	private JList jlstRestrict;
	private JTextField jtxtStateVar;
	private JButton cmdChangeStateVar;
	private String editSort = "none"; // current sort being edited
	private String selectedSort = "none";
	private TreePath selectedPath = null;
	//the selected predicate argument sort

	// edit window

	// The Sub-state classes List box
	private JList jlstStateDefs;
	private DefaultListModel lmStates;
	private oclSSClassDef curStateList; //The ocl version of the list
	/* Weihong 31/1/2002 */
	private HierarchicalStateEXDocPane hEditPane;
	private EditPaneGroup editPaneGroup = new EditPaneGroup();
	/* end 31/1/2002 */

	/**
	 * Constructor
	 * @param curDomain the current active domain
	 * @param parent - top level Window reference
	 */
	public StateView(oclDomain curDomain, OclEd parent) {
		super(parent); /* WZ 24/10/2001 */
		setClosable(false); /* WZ 11/10/2001 */
		if (curDomain == null) {
			JOptionPane.showMessageDialog(
				parent,
				"No Domain currently being edited.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		setTitle("State View (Expert)");
		sorts = curDomain.sorts; //had reference to parent
		lstPreds = curDomain.predicates;
		lstSSC = new ArrayList(); //Need to clone the statelist
		ListIterator li = curDomain.classDefs.listIterator();
		try {
			while (li.hasNext()) {
				oclSSClassDef curDef =
					(oclSSClassDef) ((oclSSClassDef) li.next()).clone();
				if (curDomain.isSort(curDef.getStateSort())) {
					lstSSC.add(curDef);
				} else {
					JOptionPane.showMessageDialog(
						parent,
						"There are state definitions for a non-existant sort ["
							+ curDef.getStateSort()
							+ "]\n"
							+ "They will be deleted when you commit your edits.",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
				}
			}
		} catch (CloneNotSupportedException e) {
			// This should not happen
			Utility.debugPrintln("Cannot clone substates. " + e.toString());
		}
		this.curDomain = curDomain;
		top = parent;
		initComponents();
		dirty = false;
		updateUI();
		// 	setPreferredSize(new Dimension(900,400));
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

		Box boxTop = Box.createVerticalBox(); // Holds views + final buttons
		Box boxEd = Box.createHorizontalBox(); // Holds the main panes
		/* end WZ 10/10/2001 */
		JPanel boxFinalButtons = new JPanel();
		boxFinalButtons.setLayout(new FlowLayout());
		/* end WZ 10/10/2001 */

		// The first Vertical pane contains the Sort Tree

		//  Now put the sort components together
		sortTree = new SortOnlyTree(curDomain);
		TreeSelectionListener sortTreeSelectionListener =
			new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent tse) {
				/* WZ 5/10/2001 */
				if (editPane.isDirty()) {
					int res =
						JOptionPane.showConfirmDialog(
							StateView.this,
							"Content in the task editing window has not been updated.\nClear the content without updating?",
							"GIPO Warning",
							JOptionPane.YES_NO_OPTION);
					if (res == JOptionPane.NO_OPTION) {
						return;
					}
				}
				/* WZ 5/10/2001 end */

				final SortOnlyTree tree = (SortOnlyTree) tse.getSource();
				try {
					String sortName = tree.getSelectedNodeName();
					if (sortName.equals(editSort))
						return;
					if (editPane.isDirty() && !sortName.equals(editSort)) {
						JOptionPane.showMessageDialog(
							StateView.this,
							"Clear or update ongoing edits first.",
							EdStrings.strInfoHeading,
							JOptionPane.INFORMATION_MESSAGE,
							null);
						if (selectedPath != null)
							tree.setSelectionPath(selectedPath);
						return;
					}
					// OK store this path
					TreePath[] paths = tree.getSelectionPaths();
					if (paths != null && paths.length > 0) {
						selectedPath = paths[0];
					}
					// Now update the states list
					populateStates(sortName);
					/* WZ 31/1/2002 */
					try {
						hEditPane.clearPane();
					} catch (BadLocationException ble) {
					}
					String xx;
					for (int i = selectedPath.getPathCount(); i > 1; i--) {
						xx =
							((Object) selectedPath.getPathComponent(i - 2))
								.toString();
						populateInheriatedStates(xx);
					}
					/* end WZ 31/1/2002 */
				} catch (SortSelectionException e) {
					Utility.debugPrintln("Unexpected failure - initComponents");
				}
				updateUI(); /* WZ 23/8/02 */
			}
		};
		sortTree.addTreeSelectionListener(sortTreeSelectionListener);
		JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
		// Ron 7/3/02 control pane size
		scrollPaneSorts.setPreferredSize(new Dimension(150, 100));
		JPanel jpanSorts = new JPanel();
		jpanSorts.setLayout(new BoxLayout(jpanSorts, BoxLayout.Y_AXIS));
		jpanSorts.setBorder(BorderFactory.createTitledBorder("Sorts .."));
		jpanSorts.add(scrollPaneSorts);
		jpanSorts.setPreferredSize(new Dimension(150, 100));
		ToolTipManager.sharedInstance().registerComponent(sortTree);
		DefaultTreeCellRenderer sortTreeRend =
			(DefaultTreeCellRenderer) sortTree.getCellRenderer();
		sortTreeRend.setToolTipText(
			"Select Sort to initiate editing sub-states for that sort.");
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
		ListCellRenderer renderer =
			new PredCellRenderer(top.strImageDir, "static.gif");
		jlstPreds.setCellRenderer(renderer);
		jlstPreds.setToolTipText(
			"Select predicate then Press Add to add to sub state definition.");
		JScrollPane scrollPanePreds = new JScrollPane(jlstPreds);
		JPanel jpanPreds = new JPanel();
		jpanPreds.setLayout(new BoxLayout(jpanPreds, BoxLayout.Y_AXIS));
		jpanPreds.setBorder(BorderFactory.createTitledBorder("Predicates .."));
		jpanPreds.add(scrollPanePreds);
		// The predicate add/delete button

		/* end WZ 10/10/2001 */
		JPanel boxAddRem = new JPanel();
		boxAddRem.setLayout(new FlowLayout());
		/* end 10/10/2001 */

		/* WZ 4/9/2001 */
		ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
		JButton cmdRemPred = new JButton("<< Remove", ii);
		cmdRemPred.setVerticalTextPosition(AbstractButton.BOTTOM);
		/* WZ 4/07/2001 */
		cmdRemPred.setHorizontalTextPosition(AbstractButton.CENTER);
		/* WZ 4/07/2001 */

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Properties16.gif");
		JButton cmdAddPred = new JButton("Add >>", ii);
		cmdAddPred.setVerticalTextPosition(AbstractButton.BOTTOM);
		/* WZ 4/07/2001 */
		cmdAddPred.setHorizontalTextPosition(AbstractButton.CENTER);
		/* WZ 4/07/2001 */
		cmdAddPred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				addPredToState();
			}
		});
		cmdRemPred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				editPane.removePredFromState();
			}
		});
		boxAddRem.add(cmdRemPred);
		boxAddRem.add(cmdAddPred);
		// Second the Filter Radio Buttons
		JPanel jpanFilter = new JPanel();
		jpanFilter.setBorder(BorderFactory.createTitledBorder("Filter By .."));

		jradSelected = new JRadioButton("First Reference Only", false);
		jradReferenced = new JRadioButton("All Referenced", false);
		jradAll = new JRadioButton("All Predicates", true);
		jradSelected.setMnemonic(KeyEvent.VK_F);
		jradReferenced.setMnemonic(KeyEvent.VK_R);
		jradAll.setMnemonic(KeyEvent.VK_A);
		jchkUnify = new JCheckBox("Unifying Sorts"); // Ron 18/10/02  
		Box boxFilter = Box.createVerticalBox();
		ButtonGroup groupFilter = new ButtonGroup();
		boxFilter.add(jradSelected);
		boxFilter.add(jradReferenced);
		boxFilter.add(jradAll);
		boxFilter.add(jchkUnify); //Ron 18/10/02
		
		groupFilter.add(jradSelected);
		groupFilter.add(jradReferenced);
		groupFilter.add(jradAll);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		cmdUpdateSel = new JButton("Refresh Selection", ii);
		cmdUpdateSel.setMnemonic(KeyEvent.VK_S);
		cmdUpdateSel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updatePredSelection();
			}
		});
		boxFilter.add(cmdUpdateSel);
		jpanFilter.add(boxFilter);

		// The state variable editing panel
		JPanel jpanSVar = new JPanel();
		jpanSVar.setBorder(
			BorderFactory.createTitledBorder("State Variable/ID .."));
		jpanSVar.setLayout(new BoxLayout(jpanSVar, BoxLayout.X_AXIS));
		jtxtStateVar = new JTextField(8);
		jtxtStateVar.setMaximumSize(new Dimension(1000, txtFieldMaxHeight));
		/* WZ 18/10/2001 */
		OCLVariableDocument stateVarDoc = new OCLVariableDocument();
		jtxtStateVar.setDocument(stateVarDoc);
		jtxtStateVar.setAlignmentY(0.5f);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Change16.gif");
		cmdChangeStateVar = new JButton("Change", ii);
		cmdChangeStateVar.setAlignmentY(0.5f);
		cmdChangeStateVar
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				changeStateVar();
			}
		});
		jpanSVar.add(jtxtStateVar);
		jpanSVar.add(cmdChangeStateVar);

		boxPreds.add(jpanPreds);
		boxPreds.add(boxAddRem);
		boxPreds.add(jpanFilter);
		boxPreds.add(jpanSVar);

		// Build the StateEditor Box
		Box boxStateEd = buildStateEditorBox();

		// Build the SubStateClassList
		jlstStateDefs = new JList();
		lmStates = new DefaultListModel();
		jlstStateDefs.setToolTipText("Select state description to edit.");
		jlstStateDefs.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		jlstStateDefs.addListSelectionListener(stateListSelListener);
		jlstStateDefs.setToolTipText("Select sub state to edit.");
		ListCellRenderer plRenderer = new PredListCellRenderer();
		jlstStateDefs.setCellRenderer(plRenderer);
		JScrollPane jscrollStateDefs = new JScrollPane(jlstStateDefs);
		JPanel jpanSDefs = new JPanel();
		jpanSDefs.setLayout(new BoxLayout(jpanSDefs, BoxLayout.Y_AXIS));
		jpanSDefs.setBorder(
			BorderFactory.createTitledBorder("State Definitions .."));
		jpanSDefs.add(jscrollStateDefs);

		// Create The Final Buttons
		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		JButton cmdCommit = new JButton("Commit", ii);
		cmdCommit.setMnemonic(KeyEvent.VK_M);
		cmdCommit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				commitEdits();
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Remove16.gif");
		JButton cmdRestore = new JButton("Restore", ii);
		cmdRestore.setMnemonic(KeyEvent.VK_R);
		cmdRestore.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Save COPY of the domain predicates
				lstSSC.clear();
				ListIterator li = curDomain.classDefs.listIterator();
				try {
					while (li.hasNext()) {
						lstSSC.add(((oclSSClassDef) li.next()).clone());
					}
				} catch (CloneNotSupportedException e) {
					// This should not happen
					Utility.debugPrintln(
						"Cannot clone substates. " + e.toString());
				}
				clearEditState();
				populateStates(editSort);
				dirty = false;
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
		JButton cmdVerify = new JButton("Verify States", ii);
		cmdVerify.setMnemonic(KeyEvent.VK_V);
		cmdVerify.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				if (editPane.isDirty()) {
					JOptionPane.showMessageDialog(
						StateView.this,
						"Possible edits ongoing - please clear or update before checking.",
						EdStrings.strInfoHeading,
						JOptionPane.INFORMATION_MESSAGE,
						null);
					return;
				}
				List mssgs = verifyStates();
				if (mssgs.size() > 0) {
					String msg = new String("");
					ListIterator li = mssgs.listIterator();
					while (li.hasNext()) {
						msg = msg.concat((String) li.next());
						if (li.hasNext()) {
							msg = msg.concat("\n");
						}
					}
					JOptionPane.showMessageDialog(
						StateView.this,
						msg,
						EdStrings.strInfoHeading,
						JOptionPane.INFORMATION_MESSAGE,
						null);
				} else {
					JOptionPane.showMessageDialog(
						StateView.this,
						EdStrings.strVerifyOK,
						EdStrings.strInfoHeading,
						JOptionPane.INFORMATION_MESSAGE,
						null);
				}

			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		JButton cmdClose = new JButton("Close", ii);
		cmdClose.setMnemonic(KeyEvent.VK_L);
		cmdClose.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				closeDialog();
			}
		});

		boxFinalButtons.add(cmdCommit);
		boxFinalButtons.add(cmdRestore);
		boxFinalButtons.add(cmdVerify);
		boxFinalButtons.add(cmdClose);

		boxEd.add(jpanSorts);
		boxEd.add(boxPreds);
		boxEd.add(boxStateEd);
		boxEd.add(jpanSDefs);

		boxTop.add(boxEd);
		boxTop.add(boxFinalButtons);

		getContentPane().add(boxTop);

	}

	/**
	 * Do the main editor panel with the Substate Class currently 
	 * being edited
	 */
	private Box buildStateEditorBox() {
		// First the state predicate List
		Box stateEdBox = Box.createVerticalBox();
		editPane = new StateExpressionDocPane(top, curDomain, false);
		/* WZ 7/9/2001 */
		editPaneGroup.addPane(editPane); /* WZ 31/1/2002 */
		editPane.addChangeListener(new ExpressionPaneListener() {
			public void getChange(ExpressionPaneEvent evt) {
				if (evt.getID() == ExpressionPaneEvent.SELECTION) {
					selectedSort = editPane.getSelectedSort();
				}

			}
		});
		JScrollPane jscrollStateEd = new JScrollPane(editPane);
		jscrollStateEd.setBorder(
			BorderFactory.createTitledBorder("Editable State .."));
		/* WZ 5/2/2002 */
		/* WZ on 31/1/2002 */
		jscrollStateEd.setPreferredSize(new Dimension(150, 60));
		hEditPane = new HierarchicalStateEXDocPane(top, curDomain, true);
		editPaneGroup.addPane(hEditPane);
		JScrollPane jsp = new JScrollPane(hEditPane);
		jsp.setBorder(BorderFactory.createTitledBorder("Inherited State .."));
		/* end 31/1/2002 */

		if (curDomain.isHierarchical()) // Ron 5/5/03
			stateEdBox.add(jsp); /* WZ 5/2/2002 */
		stateEdBox.add(jscrollStateEd); /* WZ 5/2/2002 */
		// The Sort indicator details
		JPanel jpanIndicator = new JPanel();
		jlblIndicator = new JLabel("[ None ]");
		jpanIndicator.add(jlblIndicator);
		//stateEdBox.add(jpanIndicator);

		JPanel jpanUnify = new JPanel();
		jpanUnify.setLayout(new BoxLayout(jpanUnify, BoxLayout.Y_AXIS));
		jpanUnify.setBorder(
			BorderFactory.createTitledBorder("State Variable Bindings .."));
		jpanUnify.setPreferredSize(new Dimension(150, 20)); /* WZ 5/2/2002 */

		jlstRestrict = new JList(editPane.getNEList());
		jlstRestrict.setToolTipText("Variable Constraints.");
		jlstRestrict.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		//jlstRestrict.addListSelectionListener(?????);
		JScrollPane jscrollRestrict = new JScrollPane(jlstRestrict);
		jpanUnify.add(jscrollRestrict);

		/* WZ 4/9/2001 */
		ImageIcon ii =
			ImageLoader.getImageIcon(top.strImageDir, "AlignCenter16.gif");
		JButton cmdClear = new JButton("Clear", ii);
		cmdClear.setMnemonic(KeyEvent.VK_C);
		cmdClear.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				clearEditState();
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		JButton cmdUpdateSel = new JButton("Update", ii);
		cmdUpdateSel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updateStateList();
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
		JButton cmdAddNew = new JButton("Add", ii);
		cmdAddNew.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				addEditedState();
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
		JButton cmdDelSel = new JButton("Delete", ii);
		cmdDelSel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				delSelectedState();
			}
		});
		Box boxEditButtons = Box.createHorizontalBox();
		boxEditButtons.add(cmdClear);
		boxEditButtons.add(cmdAddNew);
		boxEditButtons.add(cmdUpdateSel);
		boxEditButtons.add(cmdDelSel);

		stateEdBox.add(jpanUnify);
		stateEdBox.add(boxEditButtons);
		return stateEdBox;
	}

	/**
	 * updatePredSelection - update the filtering on the displayed predicates
	 */
	private void updatePredSelection() {
		String sortName = "none";
		List tmpPreds;
		if (!jradAll.isSelected()) {
			try {
				sortName = sortTree.getSelectedNodeName();
			} catch (SortSelectionException e) {
				JOptionPane.showMessageDialog(
					this,
					e.getMessage(),
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
		}
		if (jradSelected.isSelected()) {
			tmpPreds =
				curDomain.getPredicatesByFirstRefSortFromList(
					sortName,
					lstPreds,
					jchkUnify.isSelected());
			lmPreds = new DefaultListModel();
			// Fill the model;
			ListIterator liPreds = tmpPreds.listIterator();
			while (liPreds.hasNext()) {
				lmPreds.addElement(liPreds.next());
			}
			jlstPreds.setModel(lmPreds);
		} else if (jradReferenced.isSelected()) {
			tmpPreds =
				curDomain.getPredicatesBySortFromList(sortName, 
													lstPreds,
													jchkUnify.isSelected());
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
		// Ron 12/3/02 close not detecting all edits
		dirty = true;
	}

	/**
	 * ListSelectionListener
	 * Updates editing box when a state is choosen
	 */
	ListSelectionListener stateListSelListener = new ListSelectionListener() {
		public void valueChanged(ListSelectionEvent lse) {
			try {
				if (!lse.getValueIsAdjusting()) {
					JList lst = (JList) lse.getSource();
					oclStateList state = (oclStateList) lst.getSelectedValue();
					if (state == null) {
						Utility.debugPrintln("List selection problem");
					} else {
						editPane.clearPane();
						// jtxtVName.setText("");
						ListIterator li =
							state.getPredicateList().listIterator();
						while (li.hasNext()) {
							oclPredicate cur = (oclPredicate) li.next();
							editPane.addPredicate(cur);
						}
					}
				}
			} catch (Exception e) {
				Utility.debugPrintln("Selection exception " + e.toString());
				// Should not happen!!
			}
		}
	};

	/**
	 * addPredToState - add the currently selected predicate
	 * to the state being edited
	 */
	private void addPredToState() {
		if (editSort.equals("none")) {
			JOptionPane.showMessageDialog(
				this,
				"Please select a sort to edit first.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (jlstPreds.isSelectionEmpty()) {
			JOptionPane.showMessageDialog(
				this,
				"Please select a predicate first.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		oclPredicate selPred = (oclPredicate) jlstPreds.getSelectedValue();
		oclPredicate stndPred = selPred.copySortsToVars();
		try {
			editPane.addPredicate(stndPred);
		} catch (Exception e) {
			Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	}

	/**
	 * populateStates 
	 * Update the state list after new sort has been choosen
	 * @param sortName the choosen sort name String
	 */
	public void populateStates(String sortName) {
		editSort = sortName;
		curStateList = null;
		ListIterator li = lstSSC.listIterator();
		//curSorts = new ArrayList(); //added by donghong 2/12/02
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			if (sortName.equals(cur.getStateSort())) {
				curStateList = cur;
			}
		}
		if (curStateList == null) {
			/*JOptionPane.showMessageDialog(
				this,
				"No state definitions for this sort.",
				"GIPO Message",
				JOptionPane.INFORMATION_MESSAGE,
				null);*/ //comment out by Donghong 26/11/02
			jtxtStateVar.setText(oclPredicate.toVar(sortName));
			//	    jchkStateVar.setSelected(true);
			// RMS Following bug fix 3/7/01
			curStateList =
				new oclSSClassDef(sortName, oclPredicate.toVar(sortName));
			lmStates.clear();
			try {
				editPane.clearPane();
				editPane.setStateID(oclPredicate.toVar(sortName));
				editPane.setStateSort(sortName);
			} catch (BadLocationException ble) {
				Utility.debugPrintln("Cannot remove document content");
			}
			lstSSC.add(curStateList);
			jlstStateDefs.setModel(lmStates);
		} else {
			lmStates.clear();
			// Fill the model;
			li = curStateList.getStateList().listIterator();
			jtxtStateVar.setText(
				oclPredicate.toVar(curStateList.getStateSortId()));
			while (li.hasNext()) {
				oclStateList cur = (oclStateList) li.next();
				lmStates.addElement(cur);
			}
			jlstStateDefs.setModel(lmStates);
			try {
				editPane.clearPane();
				editPane.setStateID(
					oclPredicate.toVar(curStateList.getStateSortId()));
				editPane.setStateSort(sortName);
			} catch (BadLocationException ble) {
				Utility.debugPrintln("Cannot remove document content");
			}
		}
	}

	/* WZ 31/1/2002 */
	/**
	 * populateInheriatedStates 
	 * Update the state list after new sort has been choosen
	 * @param sortName the choosen sort name String
	 */
	public void populateInheriatedStates(String sortName) {
		// 	try {
		// 	    hEditPane.clearPane();/* WZ 31/5/02 */
		// 	} catch (javax.swing.text.BadLocationException e){}
		//changed donghong 29/11/02
		cplstSSC = new ArrayList(); //Need to clone the statelist
		ListIterator li1 = curDomain.classDefs.listIterator();
		try {
			while (li1.hasNext()) {
				oclSSClassDef curDef =
					(oclSSClassDef) ((oclSSClassDef) li1.next()).clone();
				if (curDomain.isSort(curDef.getStateSort())) {
					cplstSSC.add(curDef);
				} 
			}
		} catch (CloneNotSupportedException e) {
			// This should not happen
			Utility.debugPrintln("Cannot clone substates. " + e.toString());
		}
		oclSSClassDef curSList = null;
		ListIterator li = cplstSSC.listIterator();
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			if (sortName.equals(cur.getStateSort())) {
				curSList = cur;
			}
		}
		if (curSList != null) {
			String newName = jtxtStateVar.getText();
			List curState = curSList.getStateList();
			String oldName = curSList.getStateSortId();
			li = curSList.getStateList().listIterator();
			while (li.hasNext()) {
				oclStateList state = (oclStateList) li.next();
				if (state != null) {
			        state.renameArgInState(oldName,newName);
					ListIterator lili = state.getPredicateList().listIterator();
					while (lili.hasNext()) {
						oclPredicate curP = (oclPredicate) lili.next();
						hEditPane.addPredicate(curP);
					}
				}
			}
		}
	}

	/**
	 * clearEditState - abandon and clear the edit pane
	 * do not change the sort being edited
	 */
	public void clearEditState() {
		try {
			selectedSort = "none";
			editPane.clearPane();
			// jtxtVName.setText("");
			jlstStateDefs.clearSelection();
		} catch (Exception e) {
			Utility.debugPrintln("Failed to remove document text or model");
		}
	}
	/**
	 * updateStateList - replace the state list selected state with
	 * the result of editing the state
	 */
	private void updateStateList() {
		int inx = jlstStateDefs.getSelectedIndex();
		if (inx == -1) {
			JOptionPane.showMessageDialog(
				this,
				"No existing state being edited.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (editPane.getStateList().getPredicateList().size() == 0) {
			JOptionPane.showMessageDialog(
				this,
				"No Predicates in the state definition.\n"
					+ "Use delete to remove the state definition.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		curStateList.getStateList().remove(inx);
		curStateList.getStateList().add(inx, editPane.getStateList());
		lmStates.remove(inx);
		lmStates.insertElementAt(editPane.getStateList(), inx);
		clearEditState();
	}

	/**
	 * changeStateVar
	 * change the state variable/id for the state definition currently edited
	 * change both the existing states and the state being edited at the 
	 * moment.
	 */
	private void changeStateVar() {
		if (jtxtStateVar.getText().length() == 0) {
			JOptionPane.showMessageDialog(
				this,
				"The variable has not been defined",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		} else if(jtxtStateVar.getText().equals(curStateList.getStateSortId())) {
			JOptionPane.showMessageDialog(
				this,
				"This is the current State Id",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		} else if (
			curStateList.getStateSortId().equals(jtxtStateVar.getText())) {
			return;
		} else if (editPane.isDirty()) {
			JOptionPane.showMessageDialog(
				StateView.this,
				"Clear or update edit state before changing the state variable.",
				"GIPO Warning",
				JOptionPane.ERROR_MESSAGE);
			return;
		} else {
			// We may now have some work force this ID to be destinct from others
			// changed by donghong liu 2/12/02
			ListIterator li = findUsedArgs().listIterator();
			boolean found = false;
			//String clash = null;
			while (!found && li.hasNext()) {
				String aa = li.next().toString();
				if (aa.equals(jtxtStateVar.getText())) {
					found = true;
				}
			}
			if (found) {
				JOptionPane.showMessageDialog(
						StateView.this,
					"This Id is in use" +".\nPlease select another.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}
			int inx = 0;
			String oldName = curStateList.getStateSortId();
			String newName = jtxtStateVar.getText();
			while (inx < lmStates.getSize()) {
				oclStateList cur = (oclStateList) lmStates.getElementAt(inx);
				lmStates.remove(inx);
				cur.renameArgInState(oldName, newName);
				lmStates.insertElementAt(cur, inx);
				inx++;
			}
			curStateList.setStateSortId(newName);
			editPane.setStateID(newName);
			try {
						hEditPane.clearPane();
			} catch (BadLocationException ble) {
			}
			String xx;
			for (int i = selectedPath.getPathCount(); i > 1; i--) {
				xx =((Object) selectedPath.getPathComponent(i - 2))
								.toString();
				populateInheriatedStates(xx);
			}
			
		}
	}

	/**
	 * delSelectedState - delete the selected item for the
	 * state definition list
	 */
	private void delSelectedState() {
		int inx = jlstStateDefs.getSelectedIndex();
		if (inx == -1) {
			JOptionPane.showMessageDialog(
				this,
				"No no existing state being edited.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		curStateList.getStateList().remove(inx);
		lmStates.remove(inx);
		clearEditState();
		// Ron 12/3/02 edits not detected on closing
		dirty = true;
	}

	/**
	 * addEditedState - add the edited state as a new state in the state list
	 */
	private void addEditedState() {
		int inx = lmStates.getSize();
		if (editPane.getStateList().getPredicateList().size() == 0) {
			JOptionPane.showMessageDialog(
				this,
				"No Predicates in the state definition.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		lmStates.insertElementAt(editPane.getStateList(), inx);
		if (curStateList == null) {
			JOptionPane.showMessageDialog(
				this,
				"Please select the states sort first.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		curStateList.getStateList().add(inx, editPane.getStateList());
		clearEditState();
		// Ron 12/3/02 edits not detected on closing
		dirty = true;
	}

	/**
	 * verifyStates
	 * do verification checks on states
	 * @return List of (Strings) error messages
	 */
	public List verifyStates() {
		List msgList = new ArrayList();

		ListIterator li = lstSSC.listIterator();
		while (li.hasNext()) {
			oclSSClassDef curDef = (oclSSClassDef) li.next();
			curDef.check(curDomain, msgList);
		}
		// Output to stdout for the moment
		// 	li = msgList.listIterator();
		// 	while (li.hasNext()) {
		// 	    Utility.debugPrintln((String)li.next());
		// 	}
		return msgList;
	}

	/** Closes the dialog */
	private void closeDialog() {
		if (dirty || editPane.isDirty()) {
			int res =
				JOptionPane.showConfirmDialog(
					StateView.this,
					"Changes not Committed - Exit anyway.",
					"GIPO Warning",
					JOptionPane.YES_NO_OPTION);
			if (res == JOptionPane.YES_OPTION) {
				setVisible(false);
				dispose();
			} else {
				return;
			}
		} else {
			setVisible(false);
			dispose();
		}
	}

	/**
	 * commitEdits - save the changes back into main domain model
	 */
	private void commitEdits() {
		// Check of possible ongoing edits Ronn added dirty check 12/3/02
		if (editPane.isDirty()) {
			JOptionPane.showMessageDialog(
				StateView.this,
				"Possible on-going edit - clear edits before commiting changes.",
				"GIPO Warning",
				JOptionPane.ERROR_MESSAGE);
			return;
		}
		ListIterator li = lstSSC.listIterator();
		curDomain.classDefs.clear();
		while (li.hasNext()) {
			try {
				// RMS Bug deal with empty class defs
				oclSSClassDef cur = (oclSSClassDef) li.next();
				if (cur.getStateList().size() > 0) {
					curDomain.classDefs.add(cur.clone());
				}
			} catch (CloneNotSupportedException cne) {
				//!!!
				Utility.debugPrintln("Cannot clone StateDefinitions");
			}
		}
		// Ron 12/3/02 clear dirty flag
		dirty = false;
		/* WZ 18/07/2001 */
		top.updateWindow(getTitle());
	}
	
    //find out all the predicates and sorts list being used in the state list
    //added by donghong 03/12/02
   private List findUsedArgs(){
   	   cplstPreds = new ArrayList();
   	   curSorts = new ArrayList();
   	   ListIterator li1 = curStateList.getStateList().listIterator();
   	   while (li1.hasNext()){
   	   	  try {
			oclStateList curSLSS = (oclStateList) ((oclStateList) li1.next()).clone();
			ListIterator liSS = curSLSS.getPredicateList().listIterator();
			while (liSS.hasNext()){
				oclPredicate curPred = (oclPredicate) ((oclPredicate) liSS.next()).clone();
			    if (! cplstPreds.contains(curPred)) {
			       cplstPreds.add(curPred);
			       ListIterator liSo = curPred.getArguments().listIterator();
				   while (liSo.hasNext()) {
					OPredicate.pArg aa = (OPredicate.pArg) liSo.next();
				    if (!curSorts.contains(aa)){
				    	curSorts.add(aa);
				    } 
				   } 
			    }
			}
   	   	  } catch (CloneNotSupportedException cne) {
				//!!!
				Utility.debugPrintln("Cannot clone Predicates");
		  }
		} 
	   ListIterator li2 = hEditPane.getStateList().getPredicateList().listIterator();
       while (li2.hasNext()){
       	  try {
			oclPredicate curPred = (oclPredicate) ((oclPredicate) li2.next()).clone();
			if (! cplstPreds.contains(curPred)) {
			    cplstPreds.add(curPred);
			    ListIterator liSo = curPred.getArguments().listIterator();
				while (liSo.hasNext()) {
					OPredicate.pArg bb = (OPredicate.pArg) liSo.next();
				    if (!curSorts.contains(bb)){
				    	curSorts.add(bb);
				    } 
				} 
			}
       	  }catch (CloneNotSupportedException cne) {
				//!!!
				Utility.debugPrintln("Cannot clone Predicates");
		  }
		} 
		Utility.debugPrintln(curSorts+"test2222");
		return curSorts;
   }
}

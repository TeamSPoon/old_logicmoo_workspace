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
 * HGoalStateWindow.java
 *
 *
 * Created: Tue Mar 12 12:01:23 2002
 *
 * @author W Zhao
 * @version 2.0
 */
import java.util.*;
import java.util.List;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*; /* WZ 25/3/02 */

import jplan.edexpt.SortOnlyTree; /* WZ 25/3/02 */
import jplan.graphics.gTool.Windows.vFilter;
import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.general.*;
import jplan.images.ImageLoader;
import jplan.graphics.*;
import jplan.graphics.transition.*;

public class HGoalStateWindow extends HighLevelTransitionWindow {
	// Variables declaration

	private MethodHeadCanvas drawingCanvas;
	private int mouseActionID = 0; //do nothing

	private List lstSSC; // This is all the classdefs 

	private List sorts;
	private List lstOM; //Methods or Operators
	private DefaultListModel lmOM;
	private JList jlstOM;

	private JPanel westPanel = new JPanel(); //panel to place at west
	private JPanel eastPanel = new JPanel(); //panel to place at east
	private JPanel topPanel = new JPanel(); /* WZ 1/5/02 */

	// The Sub-state classes List box
	private JList jlstStateDefs;
	private DefaultListModel lmStates;
	private oclSSClassDef curStateList; //The ocl version of the list
	private JScrollPane jscrollStateDefs;
	private JScrollPane jscrollOM;
	private oclMethod oclOM = null; //tempFile

	private JSplitPane splitPane1, splitPane2;

	private Vector privateList;
	private Vector objList;
	private JList privateSortList;
	private JList privateObjectList; //to save the object ID's list
	private int seleIndex;
	//the selected index of operators list, to use for saving the existing operator into the same pisition
	private JScrollPane jscrollOP;
	private oclHTNTask curTask; /* WZ 3/5/02 */

	/* WZ 25/3/02 */
	oclSSClassDef curSSClassDef = null;
	String editSort = null; //curent edit sort
	/* end WZ 25/3/02 */

	FullExpressionDocPane editPane; /* WZ 1/5/02 */
	JList jlstPreds; /* WZ 1/5/02 */

	//     oclMethod curMethod; /* WZ 3/5/02 */
	TitledBox tbCanvas; /* WZ 3/5/02 */

	public HGoalStateWindow(OclEd parent, oclHTNTask curTask) {
		super(parent);
		if (curTask == null) {
			JOptionPane.showMessageDialog(
				parent,
				"Please select a task first.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		setTitle(
			"Hierarchical Goal State Editor (oclHTNTask " + curTask.ID + ")");
		this.curDomain = parent.curDomain;
		this.top = parent;
		this.curTask = curTask;

		if (curDomain == null) {
			JOptionPane.showMessageDialog(
				top,
				"No Domain currently being edited.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}

		sorts = curDomain.sorts; //had reference to parent
		lstSSC = new ArrayList(); //Need to clone the statelist
		ListIterator li = curDomain.classDefs.listIterator();
		try {
			while (li.hasNext()) {
				lstSSC.add(((oclSSClassDef) li.next()).clone());
			}
		} catch (CloneNotSupportedException e) {
			// This should not happen
			Utility.debugPrintln("Cannot clone substates. " + e.toString());
		}
		if (lstSSC.size() == 0) {
			JOptionPane.showMessageDialog(
				top,
				"No states are available.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}

		updateMethodList();

		initComponents();

		//show goals on canvas
		showGoals();

		pack();
		setVisible(true);
	}

	/**
	 * Initialisation
	 */
	protected void initComponents() {

		getContentPane().setLayout(new java.awt.BorderLayout());

		/* 
		   add Toolbars
		*/

		javax.swing.JToolBar bottomPanel = new javax.swing.JToolBar();
		bottomPanel.setLayout(new java.awt.FlowLayout());
		bottomPanel.setFloatable(false);

		ImageIcon ii =
			ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		/* WZ 19/8/02 */
		javax.swing.JButton okButton =
			new javax.swing.JButton(" Commit goal ", ii);
		okButton.setToolTipText(
			"commit the current edited content as the goal of the current task, then close window.");
		/* WZ 19/8/02 */
		okButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_OKActionPerformed();
			}
		});
		bottomPanel.add(okButton);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		/* WZ 19/8/02 */
		javax.swing.JButton cancelButton =
			new javax.swing.JButton("  Close  ", ii);
		okButton.setToolTipText("Close the window."); /* WZ 19/8/02 */
		cancelButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_CancelActionPerformed();
			}
		});
		bottomPanel.add(cancelButton);

		/* WZ 7/5/02 */
		bottomPanel.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "Edit16.gif");
		/* WZ 19/8/02 */
		javax.swing.JButton editButton =
			new javax.swing.JButton("  Edit variables  ", ii);
		editButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jplan.general.MethodVarEdit mvd =
					new jplan.general.MethodVarEdit(top, drawingCanvas, true);
			}
		});
		bottomPanel.add(editButton);

		bottomPanel.addSeparator();
		/* end 7/5/02 */

		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		/* WZ 19/8/02 */
		javax.swing.JButton bt = new javax.swing.JButton(" Zoom In ", ii);
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent e) {
				jMI_ZoomInActionPerformed();
			}
		});
		bottomPanel.add(bt);

		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomOut16.gif");
		/* WZ 19/8/02 */
		bt = new javax.swing.JButton(" Zoom Out ");
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent e) {
				jMI_ZoomOutActionPerformed();
			}
		});
		bottomPanel.add(bt);

		getContentPane().add(bottomPanel, "South");

		//add static predicates
		topPanel = addStaticPreds(); /* WZ 1/5/02 */
		getContentPane().add(topPanel, "North");

		//add splitpane2 (will contain drawingCanvas and jscollOM)
		splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane2.setResizeWeight(0.7);
		splitPane2.setDividerSize(4); /* WZ 13/5/02 */

		/* add drawing Canvas*/
		addDrawingCanvas();

		/* WZ 25/3/02 */
		//build sort tree
		JScrollPane treePane = addSortTree();

		// Build the SubStateClassList
		addStateList();

		//add a operators list
		addOperatorList();

		//assembly west panel
		westPanel.setLayout(new GridLayout(0, 1));
		westPanel.add(treePane); /* WZ 25/3/02 */
		westPanel.add(jscrollStateDefs);

		// Build the Operator List
		showMethodLists();

		//assembly eastPanel
		eastPanel.setLayout(new GridLayout(0, 1));
		eastPanel.add(jscrollOP);
		eastPanel.add(jscrollOM);

		splitPane2.add(eastPanel);
		splitPane2.setRightComponent(eastPanel);

		splitPane1 =
			new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, westPanel, splitPane2);
		splitPane1.setResizeWeight(0.15);
		splitPane1.setDividerSize(4); /* WZ 13/5/02 */
		getContentPane().add(splitPane1, "Center");
	}

	/* WZ 1/5/02 */
	/**
	 * assemble static predicates and its editing pane.
	 * @return a JPanel which contains all the statics and its editing pane
	 */
	public JPanel addStaticPreds() {
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

		//add static predicate list
		DefaultListModel lmPreds = new DefaultListModel();
		ListIterator liPreds = curDomain.predicates.listIterator();
		oclPredicate opde;
		while (liPreds.hasNext()) {
			opde = (oclPredicate) liPreds.next();
			if (opde.isStatic())
				lmPreds.addElement(opde);
		}
		jlstPreds = new JList(lmPreds);
		jlstPreds.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListCellRenderer renderer =
			new PredCellRenderer(top.strImageDir, "static.gif");
		jlstPreds.setCellRenderer(renderer);
		jlstPreds.setToolTipText(
			"Select predicate then Press Add to add to sub state definition.");
		JScrollPane scrollPanePreds = new JScrollPane(jlstPreds);
		TitledBox jpanPreds = new TitledBox("Predicates ..", scrollPanePreds);

		panel.add(jpanPreds);

		//add button
		JPanel boxAddRem = new JPanel();
		boxAddRem.setLayout(new BoxLayout(boxAddRem, BoxLayout.Y_AXIS));

		ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
		JButton cmdRemPred = new JButton("<<", ii);
		cmdRemPred.setToolTipText("Remove");
		cmdRemPred.setVerticalTextPosition(AbstractButton.BOTTOM);
		cmdRemPred.setHorizontalTextPosition(AbstractButton.CENTER);
		ii = ImageLoader.getImageIcon(top.strImageDir, "Properties16.gif");
		JButton cmdAddPred = new JButton(">>", ii);
		cmdAddPred.setToolTipText("Add");
		cmdAddPred.setVerticalTextPosition(AbstractButton.BOTTOM);
		cmdAddPred.setHorizontalTextPosition(AbstractButton.CENTER);
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

		panel.add(boxAddRem);

		//add predicates editing pane	
		editPane = new FullExpressionDocPane(curDomain);
		editPane.useLittleMouseListener(); // Ron 19/5/03 just highlight
		editPane.setToolTipText(
			"Click on button 'Edit variable' to edit variables.");
		/* WZ 8/5/02 */
		JScrollPane jscrollStaticsEd = new JScrollPane(editPane);
		TitledBox tbStatics =
			new TitledBox("Edit statics list ..", jscrollStaticsEd);

		panel.add(tbStatics);

		return panel;
	}

	/* WZ & Ron 1/5/02 */
	/**
	 * addPredToState - add the currently selected predicate
	 * to the state being edited
	 */
	private void addPredToState() {
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
	 * Get the value of sort.
	 * @return Value of sort.
	 */
	public String getSort() {
		return editSort;
	}

	/**
	 * Get the value of object.
	 * @return Value of object.
	 */
	public String getObject() {
		return curSSClassDef.getStateSortId();
	}

	/* WZ 25/3/02 */
	private JScrollPane addSortTree() {
		//  Now put the sort components together
		SortOnlyTree sortTree = new SortOnlyTree(curDomain);
		TreeSelectionListener sortTreeSelectionListener =
			new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent tse) {
				final SortOnlyTree tree = (SortOnlyTree) tse.getSource();
				try {
					String sortName = tree.getSelectedNodeName();
					if (sortName.equals(editSort))
						return;
					// Now update the states list
					populateStates(sortName);

				} catch (jplan.edexpt.BaseTree.SortSelectionException e) {
					Utility.debugPrintln("Unexpected failure - initComponents");
				}
			}
		};
		sortTree.addTreeSelectionListener(sortTreeSelectionListener);
		JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
		scrollPaneSorts.setBorder(BorderFactory.createTitledBorder("Sorts .."));
		DefaultTreeCellRenderer sortTreeRend =
			(DefaultTreeCellRenderer) sortTree.getCellRenderer();
		sortTreeRend.setToolTipText(
			"Select Sort to initiate editing sub-states fot that sort.");

		return scrollPaneSorts;
	}

	/* WZ 25/3/02 */
	/**
	 * populateStates 
	 * Update the state list after new sort has been choosen
	 * @param sortName the choosen sort name String
	 */
	private void populateStates(String sortName) {
		editSort = sortName;
		curSSClassDef = curDomain.getIntegClassDEF(sortName);
		resetStateList();
	}

	/**
	 * to add a canvas for displaying/editing graphics
	 * return - void
	 */
	protected void addDrawingCanvas() {
		drawingCanvas = new MethodHeadCanvas(false, null, this);
		drawingCanvas.setVisible(true);
		JScrollPane scrollPanel = new JScrollPane(drawingCanvas);
		drawingCanvas.setToolTipText(
			"Drag contents to form hierachical goal state.");
		/* WZ 1/5/02 */
		tbCanvas = new TitledBox("Edit goal list ..", scrollPanel);
		splitPane2.add(tbCanvas);
		splitPane2.setLeftComponent(tbCanvas);
	}

	/* WZ 3/5/02 */
	/**
	 * to reset window to its original states
	 */
	public void refreshWindow() {
		splitPane2.remove(tbCanvas);
		try {
			editPane.clearPane();
		} catch (javax.swing.text.BadLocationException ble) {
		}
		addDrawingCanvas();
	}

	/* WZ 4/4/02 */
	/**
	 * change current htnTask to oclMethod
	 */
	private void showGoals() { /* WZ 3/5/02 */
		//build an oclMethod first
		oclMethod curOMD = new oclMethod();

		//set decompositions
		List decompList = curTask.getGoals();
		curOMD.setDecomps(decompList);

		//to build temporal clause
		curOMD.setTemps(curTask.constraints);

		//to build statics
		curOMD.setStatic(curTask.statics);

		populateTask(curOMD); /* WZ 3/5/02 */
	}

	/* WZ 3/5/02 */
	/**
	 * show current htn Tasks to the screen
	 * @param curOMD oclMethod
	 */
	public void populateTask(oclMethod curOMD) {
		//show goal lists
		jplan.graphics.transition.MethodHeadCanvas.setDecomposition(
			drawingCanvas,
			drawingCanvas.getWorkingDomain(),
			curOMD);

		//show statics
		ListIterator li = curOMD.getStatics().listIterator();
		while (li.hasNext()) {
			oclPredicate oPred = (oclPredicate) li.next();
			try {
				editPane.addPredicate(oPred);
			} catch (Exception e) {
				Utility.debugPrintln(
					"Failed to insert predicate " + e.toString());
			}
		}
	}

	/**
	 * add a operators list
	 * return void
	 */
	private void addOperatorList() {
		JList opJList = new DNDList();
		DefaultListModel opModel = new DefaultListModel();
		opJList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListIterator li = curDomain.operators.listIterator();
		while (li.hasNext()) {
			try {
				while (li.hasNext()) {
					opModel.addElement(((oclOperator) li.next()).clone());
				}
			} catch (CloneNotSupportedException e) {
				// This should not happen
				Utility.debugPrintln("Cannot clone substates. " + e.toString());
			}
		}
		opJList.setModel(opModel);
		opJList.setSelectedIndex(0);

		jscrollOP = new JScrollPane(opJList);
		jscrollOP.setBorder(BorderFactory.createTitledBorder("Operators List"));
	}

	/**
	 * reset statelist to its states after the last "commit".
	 */
	public void resetStateList() {
		lmStates.clear();
		privateList.clear();
		objList.clear();
		/* WZ 25/3/02 */
		if (curSSClassDef != null && !curSSClassDef.isEmpty()) {
			String curSort = curSSClassDef.getStateSort(); //string
			ListIterator lii = curSSClassDef.getStateList().listIterator();
			while (lii.hasNext()) {
				lmStates.addElement((oclStateList) lii.next());
				privateList.addElement(curSort);
				objList.addElement(curSSClassDef.getStateSortId());
			}
			jlstStateDefs.setSelectedIndex(0);
		}
		updateUI();
	}

	/**
	 * Taking states from the domain then display them in a JList.
	 */
	protected void addStateList() {
		jlstStateDefs = new DNDList();
		lmStates = new DefaultListModel();
		jlstStateDefs.setToolTipText(
			"Select state description then drag onto the canvas to generate its graphics");
		jlstStateDefs.setSelectionMode(
			ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		privateList = new java.util.Vector();
		objList = new java.util.Vector();
		resetStateList();
		privateSortList = new JList(privateList);
		privateSortList.setSelectionMode(
			ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		privateObjectList = new JList(objList);
		privateObjectList.setSelectionMode(
			ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		jlstStateDefs.setModel(lmStates);
		ListCellRenderer plRenderer = new PredListCellRenderer();
		jlstStateDefs.setCellRenderer(plRenderer);
		jlstStateDefs.setSelectedIndex(0);
		jscrollStateDefs = new JScrollPane(jlstStateDefs);
		jscrollStateDefs.setBorder(
			BorderFactory.createTitledBorder("Substates List"));
	}

	/**
	 * taking operators from the domain then populate them in a JList
	 */
	/* WZ 8/5/02 to remove duplicates */
	private void showMethodLists() {
		List recList = new ArrayList(); /* WZ 8/5/02 */
		jlstOM = new DNDList();
		lmOM = new DefaultListModel();
		jlstOM.setToolTipText(
			"Double click the compound operator to view the graphical display");
		jlstOM.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListIterator li = lstOM.listIterator();
		while (li.hasNext()) {
			oclMethod md = (oclMethod) li.next();
			if (!hasIt(recList, (oclPredicate) md.getName())) { /* WZ 8/5/02 */
				lmOM.addElement(md);
				recList.add((oclPredicate) md.getName());
			}
		}
		jlstOM.setModel(lmOM);
		jlstOM.setSelectedIndex(0);

		jscrollOM = new JScrollPane(jlstOM);
		jscrollOM.setBorder(
			BorderFactory.createTitledBorder("Compound Operators List"));
	}

	/* WZ 8/5/02 */
	/**
	 * return true if the given oclPredicate is the same
	 * as the one already in the given list.
	 * @param cur given oclPredicate
	 * @param list given list
	 * @return true if the given oclPredicate is the same
	 */
	private boolean hasIt(List list, oclPredicate cur) {
		ListIterator li = list.listIterator();
		while (li.hasNext()) {
			oclPredicate opd = (oclPredicate) li.next();
			if (opd.equals(cur))
				return true;
		}
		return false;
	}

	/**
	 * Update the methods' list from the domain information.
	 */
	private void updateMethodList() {
		lstOM = new ArrayList();
		ListIterator li = curDomain.methods.listIterator();
		while (li.hasNext()) {
			try {
				while (li.hasNext()) {
					lstOM.add(((oclMethod) li.next()).clone());
				}
			} catch (CloneNotSupportedException e) {
				// This should not happen
				Utility.debugPrintln("Cannot clone substates. " + e.toString());
			}
		}
	}

	/**
	 * update statelist after the current "commit".
	 */
	private void stateListChanged(JList jlist) {

	}

	/** 
	 * when ok button to be pressed, close this dialog window
	 * and show the graphics at parent's canvas.
	 */
	private void jbn_OKActionPerformed() {
		//take the changed variables and save them
		oclMethod md = getCurMethod();
		curTask.translate(md);
		closeDialog();
	}

	/** 
	 * when cancel button is pressed, close this dialog window
	 */
	private void jbn_CancelActionPerformed() {
		closeDialog();
	}

	/** 
	 * close this dialog window
	 */
	private void closeDialog() {
		setVisible(false);
		dispose();
	}

	/**
	 * to zoom out the view of the canvas
	 */
	private void jMI_ZoomOutActionPerformed() {
		drawingCanvas.setScale(1 / 1.2);
	}

	/**
	 * to zoom in the view of the canvas
	 */
	private void jMI_ZoomInActionPerformed() {
		drawingCanvas.setScale(1.2);
	}

	/* WZ 3/5/02 */
	/**
	 * return current hierarchical task in terms of an oclMethod
	 * @return oclMethod
	 */
	public oclMethod getCurMethod() {
		oclMethod curMethod = drawingCanvas.getDecomposition();
		curMethod.setStatic(editPane.getFullPredicateList());
		return curMethod;
	}

} // HGoalStateWindow

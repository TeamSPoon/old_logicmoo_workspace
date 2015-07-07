/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 * 
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both the
 * copyright notice and this permission notice and warranty disclaimer appear in
 * supporting documentation, and that the names of the authors or their
 * employers not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 * 
 * The authors and their employers disclaim all warranties with regard to this
 * software, including all implied warranties of merchantability and fitness. In
 * no event shall the authors or their employers be liable for any special,
 * indirect or consequential damages or any damages whatsoever resulting from
 * loss of use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 */
package jplan.tools.opmaker;
/**
 * OpMakermain This is the Op Maker Main Window This window is responsible for
 * forming the sequence of operators signatures used by the opmaker algorithm
 * 
 * @author Ron Simpson
 * @version 0
 */
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.util.List;
import java.util.*;
import java.util.NoSuchElementException;
import java.awt.event.*;
import java.awt.*;
import java.awt.dnd.*;
import java.awt.print.*; /* Ron/Weihong 12/3/02 */
import java.awt.Color;/* Ron/Weihong 12/3/02 */
import java.awt.image.*;/* Ron/Weihong 12/3/02 */
import java.text.SimpleDateFormat;
import java.io.*;
import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.edexpt.SortTree;
import jplan.edexpt.PredTree;
import jplan.edexpt.JPopupMenuShower;
import jplan.edexpt.SortTreeDragSource;
import jplan.edexpt.PredTreeDragSource;
import jplan.edexpt.PredTreeDropTarget;
import jplan.edexpt.PredChangeListener;
import jplan.edexpt.PredListCellRenderer;
import jplan.edexpt.BaseTree.SortSelectionException;
import jplan.general.*;
import jplan.images.ImageLoader;
import java.beans.*;
/**
 * public class OpMakerMain extends GipoInternalFrame
 */
public class OpMakerMain extends GipoInternalFrame
		implements
			PredChangeListener,
			oclPrint {
	private OclEd top;
	private oclDomain curDomain;
	private oclDomain taskDom;
	private List sorts;
	private oclPredicate dummyPred; // Used in empty pred tree
	private List lstActions; // This is the working list
	private int selPredIndex = -1; // The index of the currently edited
	//predicate in the list box
	private boolean dirty = false; // flag to monitor changes being committed
	// Interface components or their Models
	// The tasks
	private JPanel westPanel = new JPanel(); //panel to place at west
	private JComboBox jcomTasks;
	private Vector vecTasks = new Vector();
	private oclTask curTask = null;
	private List lstTask = null;
	// The initial state classes list box
	private JList jlstInitState;
	private DefaultListModel lmInitState;
	private JScrollPane jscrollInitState;
	private int seleIndex; //the selected index of operators list, to use for
						   // saving the existing operator into the same
						   // pisition
	// The goal state classes list box
	private JList jlstGoalState;
	private DefaultListModel lmGoalState;
	private JScrollPane jscrollGoalState;
	// Sort Object Tree
	private SortTree sortTree;
	private JList jlstActions;
	private DefaultListModel lmPreds;
	private PredTree predTree;
	private JTextField jtxtName;
	private String lastPredName;
	// The generated operators
	private List workOps = null;
	// curState tracks the current state of each object during operator
	// generation
	private List curState = null;
	// Ron 16/3/02 show operators already defined
	private JList jlstKnown;
	private DefaultListModel lmKnown;
	/**
	 * Constructor
	 * 
	 * @param curDomain
	 *            the current active domain
	 * @param parent
	 *            top level reference to main GIPO Window
	 */
	public OpMakerMain(oclDomain curDomain, OclEd parent) {
		super(parent);
		setClosable(false);
		if (curDomain == null) {
			JOptionPane.showMessageDialog(parent,
					"No Domain currently being edited.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		List mssgs = curDomain.sortCycleTest();
		if (mssgs.size() > 0) {
			JOptionPane
					.showMessageDialog(
							parent,
							"Cannot display the sort/object tree\n"
									+ (String) mssgs.get(0)
									+ "\nEdit and repair the sort/object definitions first.",
							EdStrings.strErrorHeading,
							JOptionPane.ERROR_MESSAGE);
			return;
		}
		setTitle("Op Maker");
		this.curDomain = curDomain;
		sorts = curDomain.sorts; //had reference to parent
		lstTask = curDomain.tasks;
		lstActions = new ArrayList();
		top = parent;
		// Ron 19/3/02
		taskDom = new oclDomain(false);
		initComponents();
		dirty = false;
		lastPredName = new String("");
		pack();
		setVisible(true);
		try {
			SortTreeDragSource dragSource = new SortTreeDragSource(sortTree,
					DnDConstants.ACTION_COPY, true);
			// Cannot drop on this tree
			sortTree.setEditable(false);
		} catch (Exception e) {
			Utility.debugPrintln("Tree exception " + e.toString());
		}
		initTaskList();
	}
	/**
	 * initComponents Sets up the user interface
	 */
	private void initComponents() {
		int tWidth;
		addInternalFrameListener(new InternalFrameAdapter() {
			public void internalFrameClosing(InternalFrameEvent evt) {
				if (dirty) {
					int res = JOptionPane.showConfirmDialog(OpMakerMain.this,
							EdStrings.strNoCommit, EdStrings.strWarningHeading,
							JOptionPane.YES_NO_OPTION);
					if (res == JOptionPane.YES_OPTION) {
						commitChanges();
						setVisible(false);
						dispose();
					} else {
						setVisible(false);
						dispose();
					}
				} else {
					setVisible(false);
					dispose();
				}
			}
		});
		JButton cmdDummy = new JButton("XXX");
		int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
		cmdDummy = null;
		//add tasks panel (west panel)
		// Build the initial State Class List
		jlstInitState = new JList();
		lmInitState = new DefaultListModel();
		jlstInitState.setModel(lmInitState);
		jlstInitState.setEnabled(false);
		initStateList(curTask);
		jscrollInitState = new JScrollPane(jlstInitState);
		jscrollInitState.setBorder(BorderFactory
				.createTitledBorder("Initial State"));
		jlstInitState.setCellRenderer(new PredListCellRenderer());
		// Build the goal State Class List
		jlstGoalState = new JList();
		lmGoalState = new DefaultListModel();
		jlstGoalState.setModel(lmGoalState);
		jlstGoalState.setEnabled(false);
		initStateList(curTask);
		jscrollGoalState = new JScrollPane(jlstGoalState);
		jscrollGoalState.setBorder(BorderFactory
				.createTitledBorder("Goal State"));
		jlstGoalState.setCellRenderer(new PredListCellRenderer());
		//add the tasks combobox
		ListIterator li = lstTask.listIterator();
		while (li.hasNext()) {
			vecTasks.addElement((oclTask) li.next());
		}
		jcomTasks = new JComboBox(vecTasks);
		// 	  jcomTasks.setSelectedIndex(-1);
		jcomTasks.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				populateTasks();
			}
		});
		jcomTasks.setAlignmentY(0);
		jcomTasks.setBackground(Color.white);
		//assembly west panel
		westPanel.setBorder(BorderFactory.createTitledBorder("Task .."));
		westPanel.setLayout(new BoxLayout(westPanel, BoxLayout.Y_AXIS));
		westPanel.add(jcomTasks);
		westPanel.add(jscrollInitState);
		westPanel.add(jscrollGoalState);
		//getContentPane().add(westPanel, "West");
		Box boxTop = Box.createVerticalBox();
		Box boxEd = Box.createHorizontalBox(); // Holds the main panes
		JPanel boxFinalButtons = new JPanel();
		boxFinalButtons.setLayout(new FlowLayout());
		// The first Vertical pane contains the Sort/Object Tree
		Box boxSort = Box.createVerticalBox();
		//  Now put the sort components together
		sortTree = new SortTree(taskDom);
		ToolTipManager.sharedInstance().registerComponent(sortTree);
		DefaultTreeCellRenderer sortTreeRend = (DefaultTreeCellRenderer) sortTree
				.getCellRenderer();
		sortTreeRend
				.setToolTipText("Drag and drop objects to add action arguments.");
		sortTree.setPreferredSize(new Dimension(250, 100));
		JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
		JPanel jpanSorts = new JPanel();
		jpanSorts.setLayout(new BoxLayout(jpanSorts, BoxLayout.Y_AXIS));
		jpanSorts.setBorder(BorderFactory.createTitledBorder("Object Tree .."));
		jpanSorts.add(scrollPaneSorts);
		boxSort.add(jpanSorts);
		// The Second Vertical pane contains the Action List
		// A panel to add and delete Actions
		Box boxPreds = Box.createVerticalBox();
		lmPreds = new DefaultListModel();
		// Fill the model - nothing unless we save old sequences;
		ListIterator liPreds = lstActions.listIterator();
		jlstActions = new JList(lmPreds);
		jlstActions.setToolTipText("Select action to edit.");
		jlstActions.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		jlstActions.addListSelectionListener(predListSelListener);
		JScrollPane scrollPanePreds = new JScrollPane(jlstActions);
		JPanel jpanPreds = new JPanel();
		jpanPreds.setBorder(BorderFactory
				.createTitledBorder("Action Sequence .."));
		jpanPreds.setLayout(new BoxLayout(jpanPreds, BoxLayout.Y_AXIS));
		jpanPreds.add(scrollPanePreds);
		Box boxEdPreds = Box.createVerticalBox();
		JPanel boxEdPreds1 = new JPanel();
		boxEdPreds1.setLayout(new FlowLayout());
		JPanel boxEdPreds2 = new JPanel();
		boxEdPreds2.setLayout(new FlowLayout());
		ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "New16.gif");
		JButton cmdAddPred = new JButton("New", ii);
		cmdAddPred
				.setToolTipText("Enter action name then Drag and drop object afected to form action signature.");
		cmdAddPred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				addNewPred();
			}
		});
		cmdAddPred.setAlignmentY(0.5f);
		JLabel jlblName = new JLabel("  Action Name  ");
		jlblName.setAlignmentY(0.5f);
		jtxtName = new JTextField(14);
		jtxtName.setAlignmentY(0.5f);
		jtxtName.setDocument(new OCLIdentifierDocument());
		tWidth = jtxtName.getPreferredSize().width;
		jtxtName.setMaximumSize(new Dimension(tWidth, txtFieldMaxHeight));
		ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
		JButton cmdDelPred = new JButton("Delete", ii);
		cmdDelPred.setToolTipText("Delete the selected action.");
		cmdDelPred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				delSelectedPred();
			}
		});
		cmdDelPred.setAlignmentY(0.5f);
		boxEdPreds1.add(cmdAddPred);
		boxEdPreds2.add(jlblName);
		boxEdPreds2.add(jtxtName);
		boxEdPreds1.add(cmdDelPred);
		boxEdPreds.add(boxEdPreds1);
		boxEdPreds.add(boxEdPreds2);
		boxPreds.add(jpanPreds);
		boxPreds.add(boxEdPreds);
		// Predicate Editing tree
		dummyPred = new oclPredicate("none-selected");
		dummyPred.addConstArgument("empty");
		predTree = new PredTree(dummyPred, this, -1);
		ToolTipManager.sharedInstance().registerComponent(predTree);
		DefaultTreeCellRenderer treeRend = (DefaultTreeCellRenderer) predTree
				.getCellRenderer();
		treeRend.setToolTipText("Drag and drop objects to define arguments.");
		JScrollPane scrollPaneEdPred = new JScrollPane(predTree);
		try {
			PredTreeDragSource dragSource = new PredTreeDragSource(predTree);
			PredTreeDropTarget dropTarget = new PredTreeDropTarget(predTree);
			predTree.setEditable(true);
		} catch (Exception e) {
			Utility.debugPrintln("Tree exception " + e.toString());
		}
		// Update the tree with first predicate in list box if any
		if (lmPreds.getSize() > 0)
			jlstActions.setSelectedIndex(0);
		JPanel jpanEdPred = new JPanel();
		jpanEdPred.setLayout(new BoxLayout(jpanEdPred, BoxLayout.Y_AXIS));
		jpanEdPred
				.setBorder(BorderFactory.createTitledBorder("Edit Action .."));
		//Ron 16/3/02
		// Add list of already defined operator name
		lmKnown = new DefaultListModel();
		ListIterator liOps = curDomain.operators.listIterator();
		while (liOps.hasNext()) {
			oclOperator curOp = (oclOperator) liOps.next();
			oclPredicate opName = null;
			try {
				opName = (oclPredicate) curOp.opName.clone();
			} catch (CloneNotSupportedException e) {
				System.err.println("Unexpected clone failure.");
			}
			lmKnown.addElement(opName);
		}
		jlstKnown = new JList(lmKnown);
		jlstKnown.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		jlstKnown.addListSelectionListener(knownListSelListener);
		JScrollPane scrollPaneKnown = new JScrollPane(jlstKnown);
		scrollPaneKnown.setBorder(BorderFactory
				.createTitledBorder("Known Actions .."));
		jpanEdPred.add(scrollPaneEdPred);
		jpanEdPred.add(scrollPaneKnown);
		ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		JButton cmdCommit = new JButton("Commit", ii);
		cmdCommit.setMnemonic(KeyEvent.VK_M);
		cmdCommit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				ArrayList newOps = new ArrayList();
				lmKnown.clear();
				try {
					ListIterator li = workOps.listIterator();
					while (li.hasNext()) {
						oclOperator cur = (oclOperator) ((oclOperator) li
								.next()).clone();
						lmKnown.addElement(cur.opName);
						newOps.add(cur);
					}
				} catch (CloneNotSupportedException e) {
					System.err.println("Unexpected clone failure.");
					return;
				}
				curDomain.operators = newOps;
				dirty = false;
				top.updateWindow(getTitle());
			}
		});
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "Replace16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
		JButton cmdRun = new JButton("Generate Operators", ii);
		cmdRun.setMnemonic(KeyEvent.VK_G);
		cmdRun.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				runOpMaker();
			}
		});
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		JButton cmdPrint = new JButton("Print Actions", ii);
		cmdPrint.setMnemonic(KeyEvent.VK_P);
		cmdPrint.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				printActions();
			}
		});
		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		JButton cmdClose = new JButton("Close", ii);
		cmdClose.setMnemonic(KeyEvent.VK_L);
		cmdClose.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				if (dirty) {
					int res = JOptionPane.showConfirmDialog(OpMakerMain.this,
							"Changes not Committed - Exit anyway.",
							"GIPO Warning", JOptionPane.YES_NO_OPTION);
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
		});
		boxFinalButtons.add(cmdCommit);
		boxFinalButtons.add(cmdRun);
		//	boxFinalButtons.add(cmdRestore);
		boxFinalButtons.add(cmdPrint);
		boxFinalButtons.add(cmdClose);
		boxEd.add(westPanel);
		boxEd.add(boxSort);
		boxEd.add(boxPreds);
		boxEd.add(jpanEdPred);
		boxTop.add(boxFinalButtons);
		boxTop.add(boxEd);
		getContentPane().add(boxTop);
		initPredTreePopup(predTree);
	}
	/**
	 * init popup menu create the popup menu
	 */
	private void initPredTreePopup(Component pTree) {
		ActionListener aL = new ActionListener() {
			public void actionPerformed(ActionEvent av) {
				cutNodeCommand();
			}
		};
		JPopupMenu popupMenu = new JPopupMenu();
		JMenuItem cutMI = new JMenuItem("Cut");
		cutMI.addActionListener(aL);
		popupMenu.add(cutMI);
		MouseListener mouseListener = new JPopupMenuShower(popupMenu);
		pTree.addMouseListener(mouseListener);
	}
	/**
	 * cutNodeCommand - delete the selected argument from the action tree
	 */
	private void cutNodeCommand() {
		TreePath path = predTree.getSelectionPath();
		if (path == null) {
			JOptionPane.showMessageDialog(top,
					"Select an object in the action first.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		DefaultMutableTreeNode toDel = (DefaultMutableTreeNode) path
				.getLastPathComponent();
		boolean res = predTree.delNode(toDel);
		if (!res) {
			JOptionPane
					.showMessageDialog(
							top,
							"Select a action argument. \nCannot delete the Action Name here.",
							"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
	}
	/**
	 * ListSelectionListener Updates editing boxed when a predicate is choosen
	 */
	ListSelectionListener predListSelListener = new ListSelectionListener() {
		public void valueChanged(ListSelectionEvent lse) {
			try {
				if (!lse.getValueIsAdjusting()) {
					JList lst = (JList) lse.getSource();
					oclPredicate pred = (oclPredicate) lst.getSelectedValue();
					if (pred == null) {
						predTree.setTreePred(dummyPred, -1);
						selPredIndex = -1;
					} else {
						selPredIndex = lst.getSelectedIndex();
						predTree.setTreePred(pred, selPredIndex);
					}
				}
				jlstKnown.clearSelection();
			} catch (Exception e) {
				Utility.debugPrintln("Selection exception " + e.toString());
				// Should not happen!!
			}
		}
	};
	// Ron 16/3/02
	/**
	 * knownSelectionListener Updates editing box when a known operator is
	 * choosen
	 */
	ListSelectionListener knownListSelListener = new ListSelectionListener() {
		public void valueChanged(ListSelectionEvent lse) {
			try {
				if (!lse.getValueIsAdjusting()) {
					JList lst = (JList) lse.getSource();
					oclPredicate pred = (oclPredicate) lst.getSelectedValue();
					if (pred == null) {
						return;
					} else {
						selPredIndex = lst.getSelectedIndex();
						oclPredicate cur = new oclPredicate(pred.getName());
						lstActions.add(cur);
						selPredIndex = jlstActions.getModel().getSize();
						lmPreds.addElement(cur);
						jlstActions.setSelectedIndex(selPredIndex);
						dirty = true;
					}
				}
			} catch (Exception e) {
				Utility.debugPrintln("Selection exception " + e.toString());
				// Should not happen!!
			}
		}
	};
	/**
	 * add a new predicate to the domain
	 */
	public void addNewPred() {
		if (jtxtName.getText().equals("")) {
			JOptionPane.showMessageDialog(top, "Enter an action name.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		lastPredName = jtxtName.getText();
		oclPredicate cur = new oclPredicate(jtxtName.getText());
		jtxtName.setText("");
		//predTree.setTreePred(cur,selPredIndex);
		lstActions.add(cur);
		selPredIndex = jlstActions.getModel().getSize();
		lmPreds.addElement(cur);
		jlstActions.setSelectedIndex(selPredIndex);
		dirty = true;
	}
	/**
	 * updatePredicateAt change the predicate at specified index position
	 * 
	 * @param pred
	 *            the oclPredicate
	 * @param index
	 *            the index position
	 */
	public void updatePredicateAt(oclPredicate pred, int index) {
		oclPredicate old = (oclPredicate) lmPreds.getElementAt(index);
		// Now find it in the predicate list and change
		ListIterator li = lstActions.listIterator();
		int ix = 0;
		while (li.hasNext()) {
			// == test for references should be good enough
			if (li.next() == old) {
				Utility.debugPrintln("Found it");
				lstActions.set(ix, pred);
				break;
			}
			ix++;
		}
		dirty = true;
		lmPreds.setElementAt(pred, index);
	}
	/**
	 * delSelectedPred - delete the currently selected predicate from the list
	 */
	public void delSelectedPred() {
		int inx = jlstActions.getSelectedIndex();
		if (inx == -1) {
			JOptionPane.showMessageDialog(top, "No Action Currently Selected.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		oclPredicate old = (oclPredicate) jlstActions.getSelectedValue();
		// Now get rid of it from the list of predicates
		ListIterator li = lstActions.listIterator();
		int ix = 0;
		while (li.hasNext()) {
			// == test for references should be good enough
			if (li.next() == old) {
				Utility.debugPrintln("Found it");
				lstActions.remove(ix);
				break;
			}
			ix++;
		}
		// Now get rid from the JList
		DefaultListModel lmPreds = (DefaultListModel) jlstActions.getModel();
		lmPreds.removeElementAt(inx);
		// Reset the predicate tree
		predTree.setTreePred(dummyPred, -1);
		selPredIndex = -1;
		dirty = true;
	}
	//==================================================================
	/**
	 * populateTasks show task contents on the property window
	 * 
	 * @Param
	 */
	private void populateTasks() {
		Utility.debugPrintln("opmaker", "Fired Populate Tasks");
		if (curTask != null) {
			curTask = (oclTask) jcomTasks.getSelectedItem();
			initStateList(curTask);
			goalStateList(curTask);
			copyTaskSortsAndObjects();
			sortTree.restoreTree();
			Utility.debugPrintln("opmaker", "curTask -- " + curTask);
			updateUI();
		}
	}
	/**
	 * initTaskList initialise the task list
	 * 
	 * @Param
	 */
	public void initTaskList() {
		if (jcomTasks.getItemCount() > 0) {
			jcomTasks.setSelectedIndex(0);
			curTask = (oclTask) jcomTasks.getItemAt(0);
			Utility.debugPrintln("opmaker", "curTask -- " + curTask);
			populateTasks();
		}
	}
	/**
	 * initStateList show init states (oclSS)
	 * 
	 * @param task -
	 *            oclTask
	 */
	private void initStateList(oclTask task) {
		if (task != null) {
			showStateProperty(lmInitState, task.getInits(), 1); //oclSS
		}
	}
	/**
	 * goalStateList show goal states (oclSS)
	 * 
	 * @param task -
	 *            oclTask
	 */
	private void goalStateList(oclTask task) {
		if (task != null) {
			showStateProperty(lmGoalState, task.getGoals(), 2); //oclSS
		}
	}
	/**
	 * showStateProperty show states to a jlist
	 * 
	 * @param stateModel -
	 *            destination
	 * @param stateList -
	 *            input
	 * @param mySwitch -
	 *            switch between oclSS and oclSE
	 */
	public void showStateProperty(DefaultListModel stateModel, List stateList,
			int mySwitch) {
		Utility.debugPrintln("opmaker", "Fired showStateProperty");
		stateModel.clear();
		ListIterator li = stateList.listIterator();
		while (li.hasNext()) {
			if (mySwitch == 1) {
				oclSS ss = (oclSS) li.next();
				stateModel.addElement(ss);
			} else if (mySwitch == 2) {
				oclSE se = (oclSE) li.next();
				stateModel.addElement(se);
			}
		}
		updateUI();
	}
	//===================== manage sort tree relative to current task ====
	/**
	 * copyTaskSortsAndObjects
	 */
	private void copyTaskSortsAndObjects() {
		java.util.List copySorts = taskDom.sorts;
		copySorts.clear();
		ListIterator liSorts = curDomain.sorts.listIterator();
		while (liSorts.hasNext()) {
			oclSort curSortCopy;
			try {
				curSortCopy = (oclSort) ((oclSort) liSorts.next()).clone();
			} catch (CloneNotSupportedException e) {
				System.out
						.println("Unexpected failure to clone sort [OpMaker]");
				return;
			}
			copySorts.add(curSortCopy);
		}
		java.util.List copyObjects = taskDom.objects;
		copyObjects.clear();
		ListIterator liObjects = curDomain.objects.listIterator();
		while (liObjects.hasNext()) {
			oclObject curObject = (oclObject) liObjects.next();
			if (isStaticSort(curObject.getObjectSort())) {
				oclObject curObjectCopy;
				try {
					curObjectCopy = (oclObject) curObject.clone();
				} catch (CloneNotSupportedException e) {
					System.out
							.println("Unexpected failure to clone sort [OpMaker]");
					return;
				}
				copyObjects.add(curObjectCopy);
			} else {
				oclObject curObjList = new oclObject(curObject.getObjectSort());
				ListIterator liObjInst = curObject.getObjectNames()
						.listIterator();
				while (liObjInst.hasNext()) {
					String objID = (String) liObjInst.next();
					if (curTask.objectUsedInInits(curObject.getObjectSort(),
							objID)) {
						curObjList.addObjID(objID);
					}
				}
				copyObjects.add(curObjList);
			}
		}
	}
	/**
	 * isStaticSort check to see if sort has a class definition if so return
	 * false
	 * 
	 * @param sort -
	 *            the sort id
	 * @return - true if no class definitions
	 */
	private boolean isStaticSort(String sort) {
		try {
			curDomain.getStateListForSort(sort);
			return false;
		} catch (OCLNoSuchElementException e) {
			return true;
		}
	}
	//===================== opMaker Algorithm =============================
	/**
	 * runOpMaker top level call to run the opmaker algorithm
	 */
	private void runOpMaker() {
		if (lstActions.size() == 0) {
			JOptionPane.showMessageDialog(top,
					"No Action names have been specified.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (curTask == null) {
			JOptionPane.showMessageDialog(top,
					"You must specify a task first.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Create the working operator list -
		// copy any existing or committed operators
		workOps = new ArrayList();
		ListIterator liOps = curDomain.operators.listIterator();
		while (liOps.hasNext()) {
			workOps.add((oclOperator) liOps.next());
		}
		// initialise the current state list
		Utility.debugPrintln("opmaker", "Curtask init size = "
				+ curTask.getInits().size());
		ListIterator li = curTask.getInits().listIterator();
		curState = new ArrayList();
		while (li.hasNext()) {
			try {
				oclSS cur = (oclSS) ((oclSS) li.next()).clone();
				curState.add(cur);
			} catch (Exception e) {
				System.err
						.println("Unexpected failure to clone initial state [OpMakermain]");
				break;
			}
		}
		// Now do the generation
		try {
			li = lstActions.listIterator();
			while (li.hasNext()) {
				oclPredicate action = (oclPredicate) li.next();
				oclOperator curOp = null;
				oclOperator curOpInst = null;
				if ((curOp = alreadyDefined(action)) == null) {
					try {
						curOpInst = createNewOpInstance(action);
						Utility.debugPrintln("opmaker", "\n\n"
								+ curOpInst.toOPString());
						curOp = generaliseOP(curOpInst);
						Utility.debugPrintln("opmaker", "\n"
								+ curOp.toOPString() + "\n");
					} catch (OCLException e) {
						JOptionPane
								.showMessageDialog(
										top,
										"Cannot derive operator for action \n"
												+ action.toString()
												+ "\nCheck the action parameters.\n"
												+ "Values for all objects referenced in the states of the \nrelevant dynamic objects must be provided.",
										"GIPO Error",
										JOptionPane.ERROR_MESSAGE, null);
						return;
					} catch (OCLUserException e) {
						JOptionPane.showMessageDialog(top, "Cannot continue \n"
								+ action.toString() + "\n"
								+ "User cancelled operation.",
								"GIPO Information",
								JOptionPane.INFORMATION_MESSAGE, null);
						return;
					}
					workOps.add(curOp);
				} else {
					// This is an existing operator
					boolean makeGood = true;
					while (makeGood) {
						try {
							curOpInst = (oclOperator) curOp.clone();
							instantiateOP(action, curOpInst);
							makeGood = false;
						} catch (CloneNotSupportedException e) {
							System.err
									.println("Unexpected failure to clone operator {OpMakerMain].");
							return;
						} catch (OCLOperatorAppException e) {
							String msg = e.toString();
							int matchPos = msg
									.indexOf("Unmatched Conditional ");
							if (matchPos > 0) {
								makeGood = repairOP(action, curOpInst, msg);
							} else {
								JOptionPane
										.showMessageDialog(
												top,
												"Cannot use existing operator for action \n"
														+ action.toString()
														+ "\n"
														+ e.toString()
														+ "\nCheck the action parameters.",
												"GIPO Error",
												JOptionPane.ERROR_MESSAGE, null);
								return;
							}
							Utility.debugPrintln("opmaker",
									"OUT of repairOP matched = "
											+ curOpInst.toOPString());
							curOp = generaliseOP(curOpInst);
							Utility.debugPrintln("opmaker",
									"OUT of generaliseOP" + curOp.toOPString());
							replaceOperator(curOp);
						}
					}
				}
				Utility.debugPrintln("opmaker", "ABOUT to apply Action");
				ListIterator liSt = curState.listIterator();
				while (liSt.hasNext()) {
					oclSS curSt = (oclSS) liSt.next();
					Utility.debugPrintln("opmaker", "Object State "
							+ curSt.toString());
				}
				applyAction(curOpInst, curState);
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(top,
					"System error cannot continue.\n" + e.toString(),
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (isGoalState(curState)) {
			JOptionPane.showMessageDialog(top,
					"Generation Complete - The Goal State has been reached.",
					"GIPO Information", JOptionPane.INFORMATION_MESSAGE, null);
		} else {
			JOptionPane.showMessageDialog(top, "Generation Complete.",
					"GIPO Information", JOptionPane.INFORMATION_MESSAGE, null);
		}
	}
	/**
	 * isGoalState determine if the current generated state is the goal state
	 * 
	 * @param curState
	 *            list of object states
	 * @return - true if goal state
	 */
	private boolean isGoalState(List curState) {
		int inx = 0;
		boolean match = true;
		while (match && inx < lmGoalState.getSize()) {
			oclSE gState = (oclSE) lmGoalState.elementAt(inx);
			ListIterator liCur = curState.listIterator();
			boolean found = false;
			while (!found && liCur.hasNext()) {
				oclSS cState = (oclSS) liCur.next();
				if (gState.getName().equals(cState.getName())) {
					found = true;
					List gStateList = gState.getState();
					List cStateList = cState.getState();
					ListIterator liGSL = gStateList.listIterator();
					boolean matchState = true;
					while (matchState && liGSL.hasNext()) {
						oclPredicate gslPred = (oclPredicate) liGSL.next();
						ListIterator liCSL = cStateList.listIterator();
						boolean foundPred = false;
						while (!foundPred && liCSL.hasNext()) {
							oclPredicate cslPred = (oclPredicate) liCSL.next();
							if (gslPred.equals(cslPred)) {
								foundPred = true;
							}
						}
						if (!foundPred) {
							matchState = false;
						}
					}
					if (!matchState) {
						match = false;
					}
				}
			}
			if (!found) {
				// Mmmm no state for this object
				// no point continuing
				match = false;
			}
			inx++;
		}
		return match;
	}
	/**
	 * repairOP try to add a conditional transition to an already defined
	 * operator
	 * 
	 * @param action -
	 *            the action name
	 * @param curOP -
	 *            the operator to repair
	 * @param msg -
	 *            Exception message containing the arg name and sort
	 * @return - true if repair attempted
	 */
	private boolean repairOP(oclPredicate action, oclOperator curOP, String msg)
			throws OCLException, OCLUserException {
		int matchPos = msg.indexOf("Unmatched Conditional ");
		int commaPos = msg.indexOf(",", matchPos);
		String argName = msg.substring(matchPos + 22, commaPos);
		String argSort = msg.substring(commaPos + 1, msg.length());
		Utility.debugPrintln("opmaker", "UNMATCHED " + argName + " of sort "
				+ argSort);
		Utility.debugPrintln("opmaker", "OP IS\n" + curOP.toOPString());
		// Check to see if this action compatible with any static predicates
		List statics = actionSupportsStatics(action);
		// Find Current state
		oclSS LHS = null;
		ListIterator liStates = curState.listIterator();
		while (LHS == null && liStates.hasNext()) {
			oclSS init = (oclSS) liStates.next();
			if (argName.equals(init.getName())) {
				LHS = init;
			}
		}
		if (LHS == null) {
			Utility.debugPrintln("opmaker",
					"OP Repair Help?? Assume static sort " + argName
							+ " of sort " + argSort);
		} else {
			Utility.debugPrintln("opmaker", "Matching state = "
					+ LHS.toString());
			// Now find a RHS for this transition
			oclSSClassDef classDef = null;
			try {
				classDef = curDomain.getStateListForSort(argSort);
			} catch (jplan.ocl.OCLNoSuchElementException e) {
				// Should not happen
			}
			List stateList = classDef.getStateList();
			oclStateList RHS = null;
			oclStateList val = (oclStateList) JOptionPane.showInputDialog(top,
					"If the change in state is optional when applying the operator \n"
							+ action.getName() + " for objects of sort "
							+ argSort + "\n select the resulting state for\n "
							+ argName + "\n otherwise cancel.", "GIPO Input",
					JOptionPane.QUESTION_MESSAGE, null, stateList.toArray(),
					stateList.get(0));
			Utility.debugPrintln("opmaker", "REPAIR RHS = " + val.toString());
			if (val == null) {
				throw new OCLUserException("User cancelled input.");
			}
			// Now form the transition
			try {
				RHS = ground(val, classDef.getStateSortId(), argName, action);
			} catch (OCLException e) {
				throw e;
			}
			Utility.debugPrintln("opmaker", "Post = " + RHS.toString());
			oclSC trans = new oclSC(argSort, argName);
			// How about generalisation
			trans.setPre(LHS.getState());
			trans.setPost(RHS.getPredicateList());
			// How about statics
			// Add this parameter to the op name
			// used by generalise op
			curOP.opName.addSortedVarArgument(argName, argSort);
			curOP.addCondSC(trans);
			Utility.debugPrintln("opmaker", "Add Cond to op "
					+ curOP.toOPString());
			return true;
		}
		return false;
	}
	/**
	 * replaceOperator replace the existing definition of an operator in the
	 * working operator list
	 * 
	 * @param curOp -
	 *            the operator to replace the existing operator with same name
	 */
	private void replaceOperator(oclOperator curOp) {
		int len = workOps.size();
		boolean found = false;
		int inx = 0;
		while (!found && inx < len) {
			oclOperator cur = (oclOperator) workOps.get(inx);
			if (curOp.opName.getName().equals(cur.opName.getName())) {
				found = true;
				workOps.set(inx, curOp);
			}
			inx++;
		}
	}
	/**
	 * alreadyDefined check to see if an operator with this name exists
	 * 
	 * @param action -
	 *            the action name
	 * @return the found action or null if not found
	 */
	private oclOperator alreadyDefined(oclPredicate action) {
		ListIterator li = workOps.listIterator();
		boolean found = false;
		oclOperator theOp = null;
		while (!found && li.hasNext()) {
			oclOperator curOp = (oclOperator) li.next();
			if (action.getName().equals(curOp.opName.getName())) {
				found = true;
				theOp = curOp;
			}
		}
		return theOp;
	}
	/**
	 * applyAction use an already defined operator instantiated by the
	 * parameters in the action name to advance the current state
	 * 
	 * @param action -
	 *            the action signature
	 * @param curState -
	 *            the current state list
	 */
	private void applyAction(oclOperator action, List curState) {
		// Only need to look at necessary and conditional transitions
		// First necessary
		ListIterator li = action.getNecessary().listIterator();
		while (li.hasNext()) {
			oclSC trans = (oclSC) li.next();
			int inx = 0;
			boolean found = false;
			while (!found && inx < curState.size()) {
				oclSS state = (oclSS) curState.get(inx);
				Utility.debugPrintln("opmaker", "checking  - "
						+ trans.getName() + " against " + state.getName());
				if (trans.getName().equals(state.getName())) {
					found = true;
					oclSS rhs = new oclSS(state.getSort(), state.getName());
					rhs.setState(trans.getPost());
					Utility.debugPrintln("opmaker", "Update - "
							+ rhs.toString());
					curState.set(inx, rhs);
				}
				inx++;
			}
		}
		// Now do conditional
		li = action.getConditional().listIterator();
		while (li.hasNext()) {
			oclSC trans = (oclSC) li.next();
			int inx = 0;
			boolean found = false;
			while (!found && inx < curState.size()) {
				oclSS state = (oclSS) curState.get(inx);
				Utility.debugPrintln("opmaker", "checking  - "
						+ trans.getName() + " against " + state.getName());
				if (trans.getName().equals(state.getName())) {
					found = true;
					oclSS rhs = new oclSS(state.getSort(), state.getName());
					rhs.setState(trans.getPost());
					Utility.debugPrintln("opmaker", "Update - "
							+ rhs.toString());
					curState.set(inx, rhs);
				}
				inx++;
			}
		}
	}
	/**
	 * createNewOpInstance induce a new operator - fully instantiated
	 * 
	 * @param name -
	 *            the operator name with all affected object ids
	 * @return oclOperator - the induced operator
	 */
	private oclOperator createNewOpInstance(oclPredicate name)
			throws OCLException, OCLUserException {
		OpMakerInputBox.OpMakerData userIN;
		// Make shure that all name parameters have sorts
		ListIterator li = name.getArguments().listIterator();
		while (li.hasNext()) {
			OPredicate.pArg arg = (OPredicate.pArg) li.next();
			arg.sort = curDomain.getSortOfObject(arg.name);
			Utility.debugPrintln("opmaker", "Name = " + arg.name + " Sort = "
					+ arg.sort);
		}
		// Check to see if this action compatible with any static predicates
		java.util.List statics = actionSupportsStatics(name);
		if (statics.size() > 0) {
			li = statics.listIterator();
			while (li.hasNext()) {
				oclPredicate curStatic = (oclPredicate) li.next();
				Utility.debugPrintln("opmaker", "STATIC >> "
						+ curStatic.toString());
			}
		} else {
			Utility.debugPrintln("opmaker", "STATIC >> NONE");
		}
		// Create the empty operator
		oclOperator op = new oclOperator();
		try {
			op.setName((oclPredicate) name.clone());
		} catch (Exception e) {
			System.err
					.println("Unexpected failure to clone predicate [OpMakerMain]");
			return null;
		}
		// Ron 20/3/02
		java.util.List reqnes = inequalitiesRequired(name);
		// Now deal with each argument in the action name
		li = name.getArguments().listIterator();
		while (li.hasNext()) {
			OPredicate.pArg arg = (OPredicate.pArg) li.next();
			// Find Current state
			oclSS LHS = null;
			ListIterator liStates = curState.listIterator();
			while (LHS == null && liStates.hasNext()) {
				oclSS init = (oclSS) liStates.next();
				if (arg.name.equals(init.getName())) {
					LHS = init;
				}
			}
			if (LHS == null) {
				Utility.debugPrintln("opmaker", "Assume static sort "
						+ arg.sort);
			} else {
				Utility.debugPrintln("opmaker", "Matching state = "
						+ LHS.toString());
				// Check to see that all objects referenced in LHS are
				// given in the operator name
				if (!allObjectsValuesProvided(LHS, name)) {
					throw new OCLException(
							"Not all object values provided for LHS");
				}
				// If we already have conditional transitions
				// check and see if the current object matches that
				// transition if so do nothing
				if (matchesConditional(arg, op, LHS)) {
					continue;
				}
				if (reqnes.size() > 0) {
					oclPredicate reqNE = neRequired(arg.name, reqnes);
					// add ne to LHS and remove it from reqnes
					// neRequired does the removing when ne found
					if (reqNE != null) {
						LHS.addPredicate(reqNE);
					}
				}
				// Now find a RHS for this transition
				oclSSClassDef classDef = null;
				try {
					classDef = curDomain.getStateListForSort(arg.sort);
				} catch (jplan.ocl.OCLNoSuchElementException e) {
					// Should not happen
				}
				List stateList = classDef.getStateList();
				oclStateList RHS = null;
				if (stateList.size() == 1) {
					try {
						RHS = ground((oclStateList) stateList.get(0), classDef
								.getStateSortId(), arg.name, name);
					} catch (OCLException e) {
						throw e;
					}
					Utility.debugPrintln("opmaker", " One state "
							+ RHS.toString());
					if (sameStates(LHS, RHS)) {
						//this is a prevail
						oclSE prev = new oclSE(arg.sort, arg.name);
						prev.setPredicateList(LHS.getState());
						Utility.debugPrintln("opmaker", "Add Prev to op "
								+ prev.toString());
						op.addPrevSE(prev);
					} else {
						oclSC trans = new oclSC(arg.sort, arg.name);
						trans.setPre(LHS.getState());
						trans.setPost(RHS.getPredicateList());
						if (statics.size() > 0) {
							transValidatesStatics(trans, statics);
						}
						op.addNecSC(trans);
						// WATCH could this be conditional???
					}
				} else if (stateList.size() > 1) {
					Utility.debugPrintln("opmaker", " More than one state");
					userIN = OpMakerInputBox.getOpMakerInput(top, arg, name,
							LHS, stateList);
					if (userIN == null) {
						throw new OCLUserException("User cancelled input.");
					}
					// Now form the transition
					if (userIN.prevail) {
						// This is a prevail
						if (userIN.generalised) {
							oclSE prev = new oclSE(arg.sort, arg.name);
							List lhsState = userIN.generalisation;
							prev.setPredicateList(lhsState);
							if (statics.size() > 0) {
								prevailValidatesStatics(prev, statics);
							}
							Utility.debugPrintln("opmaker", "Add Prev to op "
									+ prev.toString());
							op.addPrevSE(prev);
						} else {
							oclSE prev = new oclSE(arg.sort, arg.name);
							prev.setPredicateList(LHS.getState());
							if (statics.size() > 0) {
								prevailValidatesStatics(prev, statics);
							}
							Utility.debugPrintln("opmaker", "Add Prev to op "
									+ prev.toString());
							op.addPrevSE(prev);
						}
					} else {
						// this is a transition
						Utility.debugPrintln("opmaker", "Post = "
								+ userIN.post.toString());
						try {
							RHS = ground(userIN.post,
									classDef.getStateSortId(), arg.name, name);
						} catch (OCLException e) {
							throw e;
						}
						oclSC trans = new oclSC(arg.sort, arg.name);
						if (userIN.generalised) {
							trans.setPre(userIN.generalisation);
						} else {
							trans.setPre(LHS.getState());
						}
						trans.setPost(RHS.getPredicateList());
						if (statics.size() > 0) {
							transValidatesStatics(trans, statics);
						}
						if (userIN.conditional) {
							op.addCondSC(trans);
							Utility.debugPrintln("opmaker", "Add Cond to op "
									+ trans.toString());
						} else {
							op.addNecSC(trans);
							Utility.debugPrintln("opmaker", "Add Nec to op "
									+ trans.toString());
						}
					}
				} else {
					JOptionPane.showMessageDialog(top,
							"The object state definition for this sort are incomplete :"
									+ arg.sort, "GIPO Error",
							JOptionPane.ERROR_MESSAGE, null);
					return op;
				}
			}
		}
		return op;
	}
	/**
	 * allObjectsValuesProvided check that all referenced objects in given ss
	 * occur in given predicate/action name
	 * 
	 * @param LHS -
	 *            the given state
	 * @param name -
	 *            the action name
	 * @return true if all values given
	 */
	private boolean allObjectsValuesProvided(oclSS LHS, oclPredicate name) {
		boolean allFound = true;
		ListIterator liPreds = LHS.getState().listIterator();
		while (allFound && liPreds.hasNext()) {
			oclPredicate cur = (oclPredicate) liPreds.next();
			int noArgs = cur.size();
			int i = 0;
			while (allFound && i < noArgs) {
				String val = cur.getNthElementName(i);
				if (!name.refersTo(val)) {
					allFound = false;
				}
				i++;
			}
		}
		return allFound;
	}
	/**
	 * matchesConditional Check If we already have conditional transitions check
	 * and see if the current object matches that transition if so add another
	 * instantiated version of the transition to conds
	 * 
	 * @param arg -
	 *            the action argument we are matching
	 * @param op -
	 *            the instantiated operator already defined
	 * @param LHS -
	 *            the LHS found for the action argument
	 * @return - true if there is a matching conditional
	 */
	private boolean matchesConditional(OPredicate.pArg arg, oclOperator op,
			oclSS LHS) {
		List conds = op.getConditional();
		if (conds.size() == 0) {
			return false;
		}
		boolean found = false;
		ListIterator li = conds.listIterator();
		while (!found && li.hasNext()) {
			oclSC trans = (oclSC) li.next();
			if (arg.sort.equals(trans.getSort())) {
				// Possible now check LHS
				// Find the state definition that matches
				oclSSClassDef classDef = null;
				try {
					classDef = curDomain.getStateListForSort(arg.sort);
				} catch (jplan.ocl.OCLNoSuchElementException e) {
					// Should not happen
				}
				List stateList = classDef.getStateList();
				ListIterator liStates = stateList.listIterator();
				oclStateList lhsState = null;
				while (lhsState == null && liStates.hasNext()) {
					oclStateList curState = (oclStateList) liStates.next();
					if (curState.ssUnifiesWithStateList(arg.name, classDef
							.getStateSortId(), LHS.getState())) {
						lhsState = curState;
					}
				}
				//Now find the state for the conditional transition lhs
				List transLHS = trans.getPre();
				List strippedState = new ArrayList();
				ListIterator liTLHS = transLHS.listIterator();
				while (liTLHS.hasNext()) {
					oclPredicate cur = (oclPredicate) liTLHS.next();
					if (!cur.isStatic()) {
						strippedState.add(cur);
					}
				}
				if (lhsState.seUnifiesWithStateList(trans.getName(), classDef
						.getStateSortId(), strippedState)) {
					found = true;
					oclSC newTrans = null;
					try {
						newTrans = (oclSC) trans.clone();
					} catch (CloneNotSupportedException e) {
						System.err
								.println("Unexpected failure to clone transition [OpMakerMain]");
					}
					newTrans.replaceVariableName(newTrans.getName(), arg.name);
					op.addCondSC(newTrans);
				}
			}
		}
		return found;
	}
	/**
	 * transValidatesStatics check to see if the given transition refers to each
	 * of the arguments of any statics in the given list of statics if so and
	 * user confirms then add to the transition LHS and remove from statics list
	 * 
	 * @param trans -
	 *            the given transition
	 * @param statics -
	 *            the list of static predicates
	 */
	private void transValidatesStatics(oclSC trans, java.util.List statics) {
		java.util.List usedStatics = new ArrayList();
		ListIterator li = statics.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			java.util.List args = cur.getArguments();
			ListIterator liArgs = args.listIterator();
			boolean allOK = true;
			while (allOK && liArgs.hasNext()) {
				oclPredicate.pArg arg = (oclPredicate.pArg) liArgs.next();
				if (!trans.transitionRefersTo(arg.name)) {
					allOK = false;
				}
			}
			if (allOK) {
				usedStatics.add(cur);
			}
		}
		if (usedStatics.size() > 0) {
			ListIterator liSt = usedStatics.listIterator();
			while (liSt.hasNext()) {
				oclPredicate pr = (oclPredicate) liSt.next();
				//Need to ask user is static is necessarily true
				int res = JOptionPane.showConfirmDialog(top,
						"Is the equivalent static predicate\n" + pr.toString()
								+ "\nallways true when applying this operator",
						"GIPO Query", JOptionPane.YES_NO_OPTION);
				if (res == JOptionPane.YES_OPTION) { 
					try {
						trans.addPre((oclPredicate) pr.clone());
					} catch (Exception e) {
						System.err
								.println("Unexpected failure to clone predicate [OpMakerMain]");
					}
				}
				// Bug Remove even if answer was no assume always same
					 // remove static from static list
					//int inx = statics.size(); BUG 26/08/04
					int inx = 0;
					boolean found = false;
					while (!found && inx < statics.size()) {
						oclPredicate st = (oclPredicate) statics.get(inx);
						if (pr.getName().equals(st.getName())) {
							statics.remove(inx);
							found = true;
						} else {
							inx++;
						}
					}
				// }
				Utility.debugPrintln("opmaker", "VALIDATED " + pr.toString());
			}
		}
	}
	/**
	 * prevailValidatesStatics check to see if the given se refers to each of
	 * the arguments of any statics in the given list of statics if so and user
	 * confirms then add to the prevail and remove from statics list
	 * 
	 * @param prev -
	 *            the given se
	 * @param statics -
	 *            the list of static predicates
	 */
	private void prevailValidatesStatics(oclSE prev, java.util.List statics) {
		java.util.List usedStatics = new ArrayList();
		ListIterator li = statics.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			java.util.List args = cur.getArguments();
			ListIterator liArgs = args.listIterator();
			boolean allOK = true;
			while (allOK && liArgs.hasNext()) {
				oclPredicate.pArg arg = (oclPredicate.pArg) liArgs.next();
				if (!prev.refersTo(arg.name)) {
					allOK = false;
				}
			}
			if (allOK) {
				usedStatics.add(cur);
			}
		}
		if (usedStatics.size() > 0) {
			ListIterator liSt = usedStatics.listIterator();
			while (liSt.hasNext()) {
				oclPredicate pr = (oclPredicate) liSt.next();
				//Need to ask user is static is necessarily true
				int res = JOptionPane.showConfirmDialog(top,
						"Is the equivalent static predicate\n" + pr.toString()
								+ "\nallways true when applying this operator",
						"GIPO Query", JOptionPane.YES_NO_OPTION);
				if (res == JOptionPane.YES_OPTION) {
					try {
						prev.addPredicate((oclPredicate) pr.clone());
					} catch (Exception e) {
						System.err
								.println("Unexpected failure to clone predicate [OpMakerMain]");
					}
				}
				// 26/08/04 BUG remove even if answer was no
					// remove static from static list
					// int inx = statics.size(); BUG 26/08/04
					int inx = 0;
					boolean found = false;
					while (!found && inx < statics.size()) {
						oclPredicate st = (oclPredicate) statics.get(inx);
						if (pr.equals(st)) {
							statics.remove(inx);
							found = true;
						} else {
							inx++;
						}
					}
				//}
				Utility.debugPrintln("opmaker", "VALIDATED " + pr.toString());
			}
		}
	}
	/**
	 * actionSupportsStatics find any static predicates that could be made true
	 * by the argument to the action name
	 * 
	 * @param action -
	 *            the action name
	 * @return - list of matching statics
	 */
	private java.util.List actionSupportsStatics(oclPredicate action) {
		java.util.List supportedStatics = new ArrayList();
		ListIterator liPreds = curDomain.predicates.listIterator();
		while (liPreds.hasNext()) {
			oclPredicate curPred = (oclPredicate) liPreds.next();
			if (curPred.isStatic()) {
				if (typeCompatible(action, curPred)) {
					Utility.debugPrintln("opmaker", "TYPE COMPATIBLE");
					java.util.List matching = matchingStatics(action, curPred);
					if (matching.size() > 0) {
						supportedStatics.addAll(matching);
					}
				}
			}
		}
		return supportedStatics;
	}
	/**
	 * typeCompatible check to see of the sorts in the given predicate match the
	 * sorts in the given action name
	 * 
	 * @param action -
	 *            action name
	 * @param sPred
	 * @return - true if matching
	 */
	private boolean typeCompatible(oclPredicate action, oclPredicate sPred) {
		oclPredicate act = null;
		try {
			act = (oclPredicate) action.clone();
		} catch (Exception e) {
			System.err
					.println("Unexpected failure to clone action name [OpMakerMain]");
			return false;
		}
		ListIterator li = sPred.getArguments().listIterator();
		while (li.hasNext()) {
			OPredicate.pArg arg = (OPredicate.pArg) li.next();
			if (!markArg(act, arg.name)) {
				return false;
			}
		}
		return true;
	}
	/**
	 * markArg check the given sort against the action arguments if the sort is
	 * found and not already marked mark the argument by setting the argument
	 * type to -1
	 * 
	 * @param action -
	 *            action name
	 * @param sort
	 * @return true if sort can be marked
	 */
	private boolean markArg(oclPredicate action, String sort) {
		ListIterator li = action.getArguments().listIterator();
		boolean marked = false;
		while (!marked && li.hasNext()) {
			OPredicate.pArg arg = (OPredicate.pArg) li.next();
			Utility
					.debugPrintln("opmaker", ">>>> " + arg.sort + " >>> "
							+ sort);
			if ((arg.aType != -1) && sort.equals(arg.sort)) {
				arg.aType = -1;
				marked = true;
			}
		}
		return marked;
	}
	/**
	 * matchingStatics return list of all statics that can instantiated to a
	 * true predicate in the domain
	 * 
	 * @param action -
	 *            the action name
	 * @param sProto -
	 *            the static prototype
	 * @return - matching list
	 */
	private java.util.List matchingStatics(oclPredicate action,
			oclPredicate sProto) {
		java.util.List matching = new ArrayList();
		String sName = sProto.getName();
		ListIterator liPreds = curDomain.atomicInvars.listIterator();
		while (liPreds.hasNext()) {
			oclPredicate stat = (oclPredicate) liPreds.next();
			if (sName.equals(stat.getName())) {
				if (allArgsFound(stat, action)) {
					matching.add(stat);
				}
			}
		}
		return matching;
	}
	/**
	 * allArgsFound check that all arguments in the static predicate are present
	 * in the action name
	 * 
	 * @param stat -
	 *            the static predicate
	 * @param action -
	 *            the action name
	 * @return - boolean
	 */
	private boolean allArgsFound(oclPredicate stat, oclPredicate action) {
		ListIterator liArgs = stat.getArguments().listIterator();
		boolean ok = true;
		while (ok && liArgs.hasNext()) {
			OPredicate.pArg arg = (OPredicate.pArg) liArgs.next();
			if (!action.refersTo(arg.name)) {
				ok = false;
			}
		}
		return ok;
	}
	/**
	 * inequalitiesRequired check to see if two or more instances of the same
	 * dynamic type occur in the action name if so thay may potentially require
	 * that an ne clause be generated
	 * 
	 * @param actionName -
	 *            the typed instantaited action name
	 * @return - list of required ne predicates
	 */
	private java.util.List inequalitiesRequired(oclPredicate actionName) {
		java.util.List res = new ArrayList();
		java.util.List args = actionName.getArguments();
		ListIterator liArgs = args.listIterator();
		int index = 1;
		while (liArgs.hasNext() && index < actionName.size()) {
			OPredicate.pArg arg = (OPredicate.pArg) liArgs.next();
			String curSort = arg.sort;
			int search = index;
			while (search < actionName.size()) {
				OPredicate.pArg targArg = (OPredicate.pArg) args.get(search);
				if (curSort.equals(targArg.sort) && !isStaticSort(targArg.sort)) {
					// found one
					oclPredicate nePred = new oclPredicate("ne");
					nePred.addSortedVarArgument(arg.name, arg.sort);
					nePred.addSortedVarArgument(targArg.name, arg.sort);
					Utility.debugPrintln("opmaker", "REQ ne "
							+ nePred.toString());
					res.add(nePred);
				}
				search++;
			}
			index++;
		}
		return res;
	}
	/**
	 * neRequired look at the required nes to see if this name is in the first
	 * argument position of any ne The ne if found will be removed from the
	 * required ne list
	 * 
	 * @param argName -
	 *            given argument name
	 * @param reqNes -
	 *            the required ne list
	 * @return - the ne if required otherwise null
	 */
	private oclPredicate neRequired(String argName, java.util.List reqNes) {
		oclPredicate res = null;
		boolean found = false;
		int i = 0;
		int top = reqNes.size();
		while (!found && i < top) {
			oclPredicate cur = (oclPredicate) reqNes.get(i);
			if (argName.equals(cur.getNthElementName(0))) {
				res = cur;
				found = true;
			}
			i++;
		}
		return res;
	}
	/**
	 * generaliseOP convert an instantiated operator instance to an operator by
	 * replacing constants by their sort names indexed by the number of
	 * occurances of values of the same sort Remove duplicated conditional
	 * transitions
	 * 
	 * @param curOP -
	 *            the instantiated operator/action
	 * @return - the operator
	 */
	private oclOperator generaliseOP(oclOperator curOP) {
		oclPredicate sig = null;
		try {
			sig = (oclPredicate) curOP.opName.clone();
			curOP = (oclOperator) curOP.clone();
		} catch (Exception e) {
			System.err
					.println("Unexpected failure to clone operator-or-name [OpMakerMain]");
			return null;
		}
		Utility.debugPrintln("opmaker", curOP.toOPString());
		List condArgs = new ArrayList();
		int index = 0;
		List sortsUsed = new ArrayList();
		ListIterator li = sig.getArguments().listIterator();
		while (li.hasNext()) {
			oclPredicate.pArg arg = (oclPredicate.pArg) li.next();
			int count = countOccurances(arg.sort, sortsUsed);
			String param = oclPredicate.toVar(arg.sort);
			param = param.concat((new Integer(count)).toString());
			//Check to see if arg corresponds to a conditional transition
			List conds = curOP.getConditional();
			boolean found = false;
			ListIterator liConds = conds.listIterator();
			while (!found && liConds.hasNext()) {
				oclSC cond = (oclSC) liConds.next();
				Utility.debugPrintln("opmaker", "COND Compare "
						+ cond.getName() + " " + arg.name);
				if (cond.getName().equals(arg.name)) {
					found = true;
					Utility
							.debugPrintln("opmaker", "Added Cond index "
									+ index);
					condArgs.add(new Integer(index));
				}
			}
			curOP.replaceVariableName(arg, param);
			sortsUsed.add(arg.sort);
			index++;
		}
		// Now remove the IDs of objects conditionally changed from
		// the operator signature
		// NOTE : this only works on the assumption that conds have all come
		// At the end of the action signature
		li = condArgs.listIterator();
		List args = curOP.opName.getArguments();
		int noConds = 0;
		while (li.hasNext()) {
			int inx = ((Integer) li.next()).intValue();
			args.remove(inx - noConds);
			noConds++;
		}
		Utility.debugPrintln("opmaker", "OUT of remove sig args loop");
		// Now find any conditional transitions not referenced in the
		// signature that still have ids rather than variable
		ListIterator liConds = curOP.getConditional().listIterator();
		while (liConds.hasNext()) {
			oclSC trans = (oclSC) liConds.next();
			if (!OEnviron.isVar(trans.getName())) {
				int count = countOccurances(trans.getSort(), sortsUsed);
				String param = oclPredicate.toVar(trans.getSort());
				param = param.concat((new Integer(count)).toString());
				sortsUsed.add(trans.getSort());
				Utility.debugPrintln("opmaker", ">>>>> replace "
						+ trans.getName() + " with " + param);
				curOP.replaceVariableName(trans.getName(), param);
			}
		}
		curOP.removeDuplicatedConditionals();
		Utility.debugPrintln("opmaker", "OUT OF Removed duplicate conds");
		return curOP;
	}
	/**
	 * instantiateOP convert an operator to an instance of the operator by
	 * replacing sorts by their values
	 * 
	 * @param action -
	 *            the action / instantiated name
	 * @param instOP -
	 *            the operator to instantiate
	 * @throws OCLOperatorAppException -
	 *             exception when operator cannot be instantiated
	 */
	private void instantiateOP(oclPredicate action, oclOperator instOP)
			throws OCLOperatorAppException, CloneNotSupportedException {
		// Make shure that all action parameters have sorts
		ListIterator li = action.getArguments().listIterator();
		while (li.hasNext()) {
			OPredicate.pArg arg = (OPredicate.pArg) li.next();
			arg.sort = curDomain.getSortOfObject(arg.name);
			Utility.debugPrintln("opmaker", "Name = " + arg.name + " Sort = "
					+ arg.sort);
		}
		oclPredicate opSig = (oclPredicate) instOP.opName.clone();
		List argsList = action.getArguments();
		List sigArgsList = opSig.getArguments();
		// Need to allow for conditional argList may be smaller
		if (argsList.size() < sigArgsList.size()) {
			Utility
					.debugPrintln("opmaker", "Op name "
							+ instOP.opName.toString() + " action "
							+ action.toString());
			throw new OCLOperatorAppException(
					"Action does not provide values for all \nobjects that MUST be involved in the action.");
		}
		li = argsList.listIterator();
		ListIterator liSig = sigArgsList.listIterator();
		while (li.hasNext() && liSig.hasNext()) {
			oclPredicate.pArg arg = (oclPredicate.pArg) li.next();
			oclPredicate.pArg sigArg = (oclPredicate.pArg) liSig.next();
			Utility.debugPrintln("opmaker", "INSTANTIATE " + sigArg.name
					+ " to " + arg.name);
			Utility.debugPrintln("opmaker", "INSTANTIATE Sorts " + sigArg.sort
					+ " to " + arg.sort);
			if (!arg.sort.equals(sigArg.sort)) {
				throw new OCLOperatorAppException("Sort " + arg.sort
						+ " in action do not match sort in defined operator."
						+ sigArg.sort);
			}
			instOP.replaceVariableName(sigArg, arg.name);
		}
		Utility.debugPrintln("opmaker", "INSTANTIATE " + instOP.toOPString());
		// Should only have conditional parameters left to deal with
		// Find IDs in the action name that are conditional and add an
		// instantiated sc clause for each
		java.util.List instConds = new ArrayList();
		while (li.hasNext()) { // Parameter not in necessary or prevail list
			oclPredicate.pArg arg = (oclPredicate.pArg) li.next();
			java.util.List opConds = instOP.getConditional();
			boolean found = false;
			ListIterator liConds = opConds.listIterator();
			while (!found && liConds.hasNext()) {
				oclSC curTrans = (oclSC) liConds.next();
				if (curTrans.getSort().equals(arg.sort)) {
					//Possible but may be more than one need to check
					// LHS against the current state
					oclSC checkTrans = (oclSC) curTrans.clone();
					checkTrans.replaceVariableName(checkTrans.getName(),
							arg.name);
					checkTrans.setName(arg.name);
					if (matchWorldState(checkTrans)) {
						instConds.add(checkTrans);
						found = true;
					}
				}
			}
			if (!found) {
				Utility.debugPrintln("opmaker", "OP AT throw\n"
						+ instOP.toOPString());
				throw new OCLOperatorAppException("Unmatched Conditional "
						+ arg.name + "," + arg.sort);
			}
		}
		instOP.getConditional().clear();
		ListIterator liInstConds = instConds.listIterator();
		while (liInstConds.hasNext()) {
			oclSC trans = (oclSC) liInstConds.next();
			instOP.addCondSC(trans);
		}
	}
	/**
	 * matchWorldState
	 * 
	 * @param trans -
	 *            transition to match against the world state
	 * @return - true if matches
	 */
	private boolean matchWorldState(oclSC trans) {
		ListIterator li = curState.listIterator();
		boolean found = false;
		while (!found && li.hasNext()) {
			oclSS state = (oclSS) li.next();
			if (trans.getName().equals(state.getName())) {
				// This is the correct state;
				if (unifiesSSwithSE(state.getState(), trans.getPre())) {
					found = true;
				}
			}
		}
		return found;
	}
	// Do I need to do real unification here and keep Variable values?
	/**
	 * unifiesSSwithSE check between two states to see if the given states are
	 * all included in the current states.
	 * 
	 * @param SSList
	 *            List of oclPredicate (SS)
	 * @param SEList
	 *            List of oclPredicate (SE)
	 * @return boolean
	 */
	private boolean unifiesSSwithSE(List SSList, List SEList) {
		boolean AllOK = true;
		ListIterator liSE = SEList.listIterator();
		while (AllOK && liSE.hasNext()) { //for every predicate in the SE
			oclPredicate oprd = (oclPredicate) liSE.next();
			ListIterator liSS = SSList.listIterator();
			boolean found = false;
			while (!found && liSS.hasNext()) { //for every predicate in the SS
				oclPredicate basePred = (oclPredicate) liSS.next();
				if (oprd.equals(basePred)) {
					Utility.debugPrintln("opmaker", "Found " + oprd.toString());
					found = true;
				}
			}
			if (!found) {
				AllOK = false;
			}
		}
		return AllOK;
	}
	/**
	 * occurancesOp count the number of occurances of the given String in the
	 * list
	 * 
	 * @param given -
	 *            the given string to search for
	 * @param lst -
	 *            the list to search
	 * @return - the count of the number of occurances
	 */
	private int countOccurances(String given, List lst) {
		int count = 0;
		ListIterator li = lst.listIterator();
		while (li.hasNext()) {
			String str = (String) li.next();
			if (given.equals(str)) {
				count++;
			}
		}
		return count;
	}
	/**
	 * ground ground a state list replacing StateID with ID of the main Object
	 * replace any other Variables with matching sorts from the action name.
	 * 
	 * @param in -
	 *            the statelist
	 * @param oldID -
	 *            The Old ID
	 * @param newID -
	 *            The new Object ID
	 * @param name -
	 *            the action name
	 * @return oclStateList - the ground state list
	 */
	private oclStateList ground(oclStateList in, String oldID, String newID,
			oclPredicate name) throws OCLException {
		oclStateList newSL = null;
		try {
			newSL = (oclStateList) in.clone();
		} catch (Exception e) {
			System.err
					.println("Unexpected failure to clone oclStateList [OpMakerMain]");
		}
		newSL.renameArgInState(oldID, newID);
		// Now find remaining variables - if only one instance of the sort
		// in the name use this - if more than one ask
		ListIterator li = newSL.getPredicateList().listIterator();
		while (li.hasNext()) {
			oclPredicate curPred = (oclPredicate) li.next();
			ListIterator liArgs = curPred.getArguments().listIterator();
			while (liArgs.hasNext()) {
				oclPredicate.pArg arg = (oclPredicate.pArg) liArgs.next();
				if (OEnviron.isVar(arg.name)) {
					Utility.debugPrintln("opmaker", "Unbound variable "
							+ arg.name + " of sort " + arg.sort);
					// find sort in name
					ListIterator liName = name.getArguments().listIterator();
					List vals = new ArrayList();
					oclPredicate.pArg argName = null;
					while (liName.hasNext()) {
						argName = (oclPredicate.pArg) liName.next();
						if (arg.sort.equals(argName.sort)) {
							vals.add(argName.name);
						}
					}
					if (vals.size() == 0) {
						throw new OCLException("Not a possible state");
					} else if (vals.size() == 1) {
						newSL.renameArgInState(arg.name, (String) vals.get(0));
					} else {
						// Multi values must choose
						String val = (String) JOptionPane.showInputDialog(top,
								"Select a value for " + arg.sort + " in\n"
										+ newSL.toString()
										+ "\nafter applying the action\n"
										+ name.toString(), "GIPO Input",
								JOptionPane.QUESTION_MESSAGE, null, vals
										.toArray(), vals.get(0));
						if (val != null) {
							newSL.renameArgInState(arg.name, val);
						} else {
							throw new OCLException("User Cancelled Selection");
						}
					}
				}
			}
		}
		return newSL;
	}
	/**
	 * sameStates test to see if LHS and RHS refer to same states
	 * 
	 * @param LHS
	 * @param RHS
	 * @return boolean
	 */
	private boolean sameStates(oclSS LHS, oclStateList RHS) {
		List pre = LHS.getState();
		List post = RHS.getPredicateList();
		boolean ok = true;
		if (pre.size() == post.size()) {
			ListIterator liPre = pre.listIterator();
			ok = true;
			while (ok && liPre.hasNext()) {
				oclPredicate prePred = (oclPredicate) liPre.next();
				ListIterator liPost = post.listIterator();
				boolean found = false;
				while (!found && liPost.hasNext()) {
					oclPredicate postPred = (oclPredicate) liPost.next();
					if (prePred.equals(postPred)) {
						found = true;
					}
				}
				if (!found) {
					ok = false;
				}
			}
			return ok;
		} else {
			return false;
		}
	}
	/**
	 * generaliseState
	 * remove from the current LHS expression predicates not in
	 * the given oclStateList
	 * @param gen - given oclStateList generalisation required predicates
	 * @param lhs - the LHS oclSS
	 * @returm - the cutdown oclSS
	 */
	private oclSS generaliseState(oclStateList gen, oclSS lhs) {
		oclSS res = null;
		try {
			res = (oclSS) lhs.clone();
		} catch (Exception e) {
			System.err
					.println("Unexpected failure to clone oclSS [OpmakerMain]");
			return lhs;
		}
		List statePreds = lhs.getState();
		ListIterator li = statePreds.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (!Utility.memberPred(cur, gen)) {
				statePreds.remove(cur);
			}
		}
		return res;
	}
	//==================================================================
	/** Closes the dialog */
	private void closeDialog(java.awt.event.WindowEvent evt) {
		if (dirty) {
			int res = JOptionPane.showConfirmDialog(OpMakerMain.this,
					"Changes not Committed - Exit anyway.", "GIPO Warning",
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
	private void commitChanges() {
		dirty = false;
	}
	/* Ron 12/3/02 */
	/*
	 * Print the training sequence (Probably)to the printer
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		ps.println("OpMaker");
		ps.println();
		ps.println("Training Task:");
		curTask.oclPrintComponent(ps, indent, nline);
		ps.println();
		ps.println("OpMaker - training sequence");
		ps.println();
		ListIterator li = lstActions.listIterator();
		while (li.hasNext()) {
			oclPredicate pred = (oclPredicate) li.next();
			ps.println(pred.toString());
		}
	}
	/* Ron/Weihong 12/3/02 */
	/**
	 * Print out the current action sequence
	 */
	private void printActions() {
		String header = "Gipo - Domain [" + curDomain.getName() + "] Author ["
				+ curDomain.getAuthor() + "]";
		HardcopyWriter hw;
		try {
			hw = new HardcopyWriter(top, header, 10, .75, .5, .75, .5);
		} catch (HardcopyWriter.PrintCanceledException e) {
			JOptionPane.showMessageDialog(this, "Error Printing Domain.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Send output to it through a PrintWriter stream
		PrintWriter out = new PrintWriter(hw);
		oclPrintComponent(out, 0, false);
		out.close();
	}
}
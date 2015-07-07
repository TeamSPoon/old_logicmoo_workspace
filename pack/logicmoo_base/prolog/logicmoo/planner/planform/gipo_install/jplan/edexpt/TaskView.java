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
 * TaskView This is the GIPO expert editor for Problem / Tasks Part of the
 * edexpt (Editor for Experts) Views
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
import javax.swing.text.BadLocationException;
import java.awt.print.*; /* Weihong added on 28/08/2001 */
import java.io.*; /* Weihong added on 28/08/2001 */

import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.edexpt.BaseTree.SortSelectionException;
import jplan.general.*;
import jplan.general.GipoInputBox;
import jplan.images.ImageLoader; /* WZ 5/9/2001 */

public class TaskView extends GipoInternalFrame { /* Weihong added on 24/10/2001 */
	private OclEd top;

	private oclDomain curDomain;

	private List sorts;

	private oclPredicate dummyPred; // Used in empty pred tree

	private List lstSSC; // This is the list of states

	private List curStateList; // The selected state list

	private DefaultListModel lmStates; //The current/displayed sort's states

	private boolean dirty = false; // flag to monitor changes being committed

	// Interface components or their Models
	private SortTree sortTree;

	private JList jlstStateDefs;

	//details of the currently selected Calss Definition
	private int selStateIndex = -1;

	private String selStateSort = null;

	private String selStateId = null;

	// the index position of current state definition
	private TaskExpressionDocPane editPane;

	private MultiPredTree predTree;

	private JList jlstInitState, jlstGoalState;

	private DefaultListModel lmInitState, lmGoalState;

	private JScrollPane jscrollInitState, jscrollGoalState;

	/* Weihong added on 28/06/2001 */
	private JToolBar jToolBar1;

	private String curSort = null, curObject = null;

	private DefaultListModel curModel;

	private int selectionIndex = -1;

	// The tasks
	private List lstTask; // This is all the tasks

	private JComboBox jcomTasks;

	private Vector vecTasks = new Vector();

	private oclTask curTask = null;

	/* end Weihong add */

	private JList hStateList; /* Weihong on 26/2/02 */

	private DefaultListModel hLMStates; /* Weihong on 26/2/02 */

	private List curInheriatedStates = new ArrayList(); /* Weihong 12/3/02 */

	private boolean hTask; /* WZ 30/4/02 */

	/**
	 * Constructor
	 * 
	 * @param curDomain
	 *            the current active domain
	 * @param parent
	 *            top level Window reference
	 */
	public TaskView(oclDomain curDomain, OclEd parent) {
		super(parent); /* Weihong added on 24/10/2001 */
		setClosable(false); /* Weihong added on 11/10/2001 */

		if (curDomain == null) {
			JOptionPane.showMessageDialog(parent,
					"No Domain currently being edited.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (curDomain.oclPlus) {
			setTitle("Task View (Durative Actions)");
		} else {
			setTitle("Task View");
		}
		this.curDomain = curDomain;
		top = parent;
		sorts = curDomain.sorts; //had reference to parent

		/* WZ 30/4/02 */
		// Ron 5/5/03 use domain record
		if (!curDomain.isHierarchical()) {
			hTask = false;
		} else {
			hTask = true;
		}
		/* end 30/4/02 */

		// the domain predicates
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

		/* Weihong added on 28/06/2001 */
		lstTask = getExistingTasks();
		/* end add */

		// Fill the model;
		initComponents();
		dirty = false;
		pack();
		setVisible(true);

		/* WZ 28/06/2001 */
		sortTree.setEditable(false);

	}

	/**
	 * initComponents Sets up the user interface
	 */
	private void initComponents() {
		int tWidth;

		JButton cmdDummy = new JButton("XXX");
		int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
		cmdDummy = null;

		Box boxTop = Box.createVerticalBox();
		Box boxEd = Box.createHorizontalBox(); // Holds the main panes

		/*
		 * add Toolbars
		 */
		jToolBar1 = new JToolBar();
		jToolBar1.setFloatable(false); /* Weihong added on 12/10/2001 */

		// Create The Final Buttons
		/* WZ 4/9/2001 */
		ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir,
				"SendMail16.gif");
		JButton cmdCommit = new GipoButton(" Commit ", ii); /* WZ 5/12/2001 */
		cmdCommit.setMnemonic(KeyEvent.VK_M);
		cmdCommit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				ArrayList newTasks = new ArrayList();
				/* WZ 30/4/02 to distinguish hierarchical task view */
				if (!hTask) {
					try {
						ListIterator lit = lstTask.listIterator();
						while (lit.hasNext()) {
							/* WZ 29/08/2001 */
							oclTask tk = (oclTask) ((oclTask) lit.next())
									.clone();
							if (checkTask(tk))
								newTasks.add(tk);
							else {
								return;
							}
							/* Weihong changed end */
						}
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln("Unexpected clone failure.");
						return;
					}
					curDomain.tasks = newTasks;
				} else {
					try {
						ListIterator lit = lstTask.listIterator();
						while (lit.hasNext()) {
							/* WZ 29/08/2001 */
							oclHTNTask tk = (oclHTNTask) ((oclHTNTask) lit
									.next()).clone();
							if (checkTask(tk))
								newTasks.add(tk);
							else {
								return;
							}
							/* Weihong changed end */
						}
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln("Unexpected clone failure.");
						return;
					}
					curDomain.htntasks = newTasks;
				}
				dirty = false;

				/* Weihong added on 18/07/2001 */
				top.updateWindow(getTitle());
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Remove16.gif");
		JButton cmdRestore = new GipoButton(" Restore ", ii);
		/* WZ 5/12/2001 */
		cmdRestore.setMnemonic(KeyEvent.VK_R);
		cmdRestore.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				lstTask = getExistingTasks();
				resetTaskList();
				initTaskList();
				dirty = true; //Ron 3/10/04
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Edit16.gif");
		JButton cmdRename = new GipoButton(" Rename ", ii); /* WZ 5/12/2001 */
		cmdRename.setMnemonic(KeyEvent.VK_M);
		cmdRename.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				renameTask();
				dirty = true; //Ron 3/10/04
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Copy16.gif");
		JButton cmdCopy = new GipoButton(" Copy ", ii); /* WZ 5/12/2001 */
		cmdCopy.setToolTipText("Copy selected task to a new task.");
		cmdCopy.setMnemonic(KeyEvent.VK_C);
		cmdCopy.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				copyAsNewTask();
				dirty = true; //Ron 3/10/04
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
		JButton cmdDelete = new GipoButton(" Delete ", ii); /* WZ 5/12/2001 */
		cmdDelete.setToolTipText("Delete Task");
		cmdDelete.setMnemonic(KeyEvent.VK_D);
		cmdDelete.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				DeleteTask();
				dirty = true; //Ron 3/10/04
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		JButton cmdPrint = new GipoButton(" Print ", ii); /* WZ 5/12/2001 */
		cmdPrint.setMnemonic(KeyEvent.VK_P);
		cmdPrint.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				PrintTask();
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		JButton cmdPrintAll = new GipoButton(" Print All ", ii);
		/* WZ 5/12/2001 */
		cmdPrintAll.setMnemonic(KeyEvent.VK_A);
		cmdPrintAll.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				PrintAllTask();
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
		JButton cmdVerify = new GipoButton("Verify", ii); /* WZ 5/12/2001 */
		cmdVerify.setToolTipText("Verify Tasks");
		cmdVerify.setMnemonic(KeyEvent.VK_V);
		// 16/8/01 Ron added
		cmdVerify.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				verifyTasks();
			}
		});
		// Ron addition end

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		JButton cmdClose = new GipoButton(" Close ", ii); /* WZ 5/12/2001 */
		cmdClose.setMnemonic(KeyEvent.VK_L);
		cmdClose.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				if (dirty || editPane.isDirty()) { // Ron 3/10/04 Added check on
												   // dirty
					int res = JOptionPane.showConfirmDialog(TaskView.this,
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

		jToolBar1.add(cmdCommit);
		jToolBar1.add(cmdRestore);
		jToolBar1.add(cmdVerify); /* Weihong added on 12/07/2001 */
		jToolBar1.addSeparator(); /* Weihong added on 22/08/2001 */
		jToolBar1.add(cmdRename); /* Weihong added on 22/08/2001 */
		jToolBar1.add(cmdCopy); /* Weihong added on 22/08/2001 */
		jToolBar1.add(cmdDelete); /* Weihong added on 12/07/2001 */
		jToolBar1.addSeparator(); /* Weihong added on 29/08/2001 */
		jToolBar1.add(cmdPrint); /* Weihong added on 28/08/2001 */
		jToolBar1.addSeparator(); /* Weihong added on 22/08/2001 */
		jToolBar1.add(cmdClose);
		// 	  jToolBar1.setBorder (new javax.swing.border.BevelBorder(0));/* WZ
		// 5/12/2001 */
		// The first Vertical pane contains the Sort Tree

		//  Now put the sort components together
		sortTree = new SortTree(curDomain);
		TreeSelectionListener sortTreeSelListener = new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent tse) {
				final SortTree tree = (SortTree) tse.getSource();
				try {
					if (tree.isSelectedObject()) {
						/* Weihong changed/added on 3/9/2001 */
						if (editPane.isDirty()) {
							int k = JOptionPane
									.showConfirmDialog(
											top,
											"Content in the task editing window has not been updated.\nClear the content without updating?",
											"GIPO Confirm",
											JOptionPane.YES_NO_OPTION);
							if (k == JOptionPane.YES_OPTION) {
								try {
									editPane.clearPane();
								} catch (BadLocationException ble) {
									Utility
											.debugPrintln("Cannot remove document content");
								}
							} else {
								return;
							}
						}
						String nodeName = tree.getSelectedNodeName();
						curSort = curDomain.getSortOfObject(nodeName);
						curObject = nodeName;
						// 				/* Weihong 12/3/02 */
						// 				String xx;
						// 				TreePath selectedPath = tree.getSelectionPath();
						// 				for (int i = selectedPath.getPathCount(); i>3; i--){
						// 				    xx =
						// ((Object)selectedPath.getPathComponent(i-3)).toString();
						// 				    Utility.debugPrintln("XX '"+xx+"'");
						// 				    List tmplst = getInheriatedStates(xx);
						// 				    if (tmplst != null){
						// 					ListIterator lili = tmplst.listIterator();
						// 					while(lili.hasNext()) {
						// 					    oclPredicate tmpopd = (oclPredicate)lili.next();
						// 					    curInheriatedStates.add(tmpopd);
						// 					    Utility.debugPrintln("added state
						// '"+tmpopd.toString()+"'");
						// 					}
						// 				    }
						// 				    else
						// 					continue;
						// 				}
						/* Weihong changed/added end */
						populateStates(curSort, nodeName);
					}
				} catch (SortSelectionException e) {
					Utility.debugPrintln("Unexpected failure - initComponents");
				}
				updateUI(); /* Weihong added on 7/12/2001 */
			}
		};
		sortTree.addTreeSelectionListener(sortTreeSelListener);
		JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
		JPanel jpanSorts = new JPanel();
		jpanSorts.setLayout(new BoxLayout(jpanSorts, BoxLayout.Y_AXIS));
		jpanSorts.setBorder(BorderFactory.createTitledBorder("Sorts .."));
		jpanSorts.add(scrollPaneSorts);

		// The Second Vertical pane contains the States for the selected Sort

		// Build the SubStateClassList
		// 	/* Weihong 26/2/02 */
		// 	hStateList = new JList();
		// 	hLMStates = new DefaultListModel();
		// 	hStateList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		// 	hStateList.addListSelectionListener(stateListSelListener);
		// 	hStateList.setCellRenderer(new PredListCellRenderer());
		// 	JScrollPane jsp = new JScrollPane(hStateList);
		// 	jsp.setBorder(BorderFactory.createTitledBorder("Inherited State
		// .."));

		jlstStateDefs = new JList();
		lmStates = new DefaultListModel();
		jlstStateDefs.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		jlstStateDefs.addListSelectionListener(stateListSelListener);
		ListCellRenderer plRenderer = new PredListCellRenderer();
		jlstStateDefs.setCellRenderer(plRenderer);
		JScrollPane jscrollStateDefs = new JScrollPane(jlstStateDefs);
		jscrollStateDefs.setBorder(BorderFactory
				.createTitledBorder("States .."));
		JPanel jpanSDefs = new JPanel();
		jpanSDefs.setLayout(new BoxLayout(jpanSDefs, BoxLayout.Y_AXIS));
		// 	jpanSDefs.setBorder(BorderFactory.createTitledBorder("States .."));
		// 	if (top.hierarchicalSwitch)/* Weihong 5/3/02 */
		// 	    jpanSDefs.add(jsp);
		jpanSDefs.add(jscrollStateDefs);
		/* end 26/2/02 */

		/* WZ 28/06/2001 */
		// The 3rd Vertical pane contains edit pane
		// Build the StateEditor Box
		Box boxStateEd = buildStateEditorBox();

		/* Weihong added on 28/06/2001 */
		// The 3rd Vertical pane contains the task List
		// Build the initial State Class List
		jlstInitState = new JList();
		lmInitState = new DefaultListModel();
		jlstInitState.setModel(lmInitState);
		jlstInitState.setToolTipText("Select state description to edit.");
		initStateList(curTask);
		jlstInitState.addListSelectionListener(oclSSListSelListener);
		jscrollInitState = new JScrollPane(jlstInitState);
		jscrollInitState.setBorder(BorderFactory
				.createTitledBorder("Initial State"));
		jlstInitState.setCellRenderer(new PredListCellRenderer());

		// Build the goal State Class List
		jlstGoalState = new JList();
		lmGoalState = new DefaultListModel();
		jlstGoalState.setModel(lmGoalState);
		jlstGoalState.setToolTipText("Select state description to edit.");
		initStateList(curTask);
		jlstGoalState.addListSelectionListener(oclSEListSelListener);
		jscrollGoalState = new JScrollPane(jlstGoalState);
		jscrollGoalState.setBorder(BorderFactory
				.createTitledBorder("Goal State"));
		jlstGoalState.setCellRenderer(new PredListCellRenderer());

		//add the tasks combobox
		ListIterator li = lstTask.listIterator();
		while (li.hasNext()) {
			if (!hTask)
				vecTasks.addElement((oclTask) li.next());
			else
				vecTasks.addElement((oclHTNTask) li.next());
		}
		jcomTasks = new JComboBox(vecTasks);
		initTaskList();
		jcomTasks.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				populateTasks();
			}
		});
		jcomTasks.setAlignmentY(0);
		jcomTasks.setBackground(Color.white);
		/* WZ 15/5/02 */
		jcomTasks.setMaximumSize(new Dimension(1000, txtFieldMaxHeight));

		JPanel jpTasks = new JPanel();
		jpTasks.setLayout(new BoxLayout(jpTasks, BoxLayout.Y_AXIS));
		jpTasks.setBorder(BorderFactory.createTitledBorder("Task .."));
		jpTasks.add(jcomTasks);
		jpTasks.add(jscrollInitState);
		if (!hTask) /* Weihong 12/3/02 */
			jpTasks.add(jscrollGoalState);
		/* end Weihong add */

		boxEd.add(jpanSorts);
		boxEd.add(jpanSDefs);
		boxEd.add(boxStateEd); /* Weihong added on 28/06/2001 */
		boxEd.add(jpTasks); /* Weihong added on 28/06/2001 */

		boxTop.add(boxEd);
		getContentPane().add(jToolBar1, "North");
		getContentPane().add(boxTop, "Center");

	}

	/* Weihong added on 29/08/2001 */
	/**
	 * checkTask
	 * 
	 * @Param oclTask -
	 *            task
	 */
	private boolean checkTask(oclTask task) {
		if (task.goals.size() <= 0 || task.inits.size() <= 0) {
			JOptionPane.showMessageDialog(this, "Task " + task.ID
					+ " has an empty initial/goal states.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return false;
		}
		return true;
	}

	/* Weihong added on 3/07/2001 */
	/**
	 * populateTasks show task contents on the property window
	 * 
	 * @Param
	 */
	private void populateTasks() {
		if (!hTask) { /* WZ 30/4/02 */
			curTask = (oclTask) jcomTasks.getSelectedItem();
		} else {
			curTask = (oclHTNTask) jcomTasks.getSelectedItem();
		}

		if (curTask != null) {
			initStateList(curTask);
			if (!hTask) { /* WZ 30/4/02 */
				goalStateList(curTask);
			}
			updateUI();
		} else { /* WZ 3/10/2001 */
			jcomTasks.setSelectedItem(null);
			lmInitState.clear();
			if (!hTask) { /* WZ 30/4/02 */
				lmGoalState.clear();
			}
		}
	}

	/* Weihong added on 3/07/2001 */
	/**
	 * initTaskList initialise the task list
	 * 
	 * @Param
	 */
	private void initTaskList() {
		if (jcomTasks.getItemCount() > 0) {
			jcomTasks.setSelectedIndex(0);
			curTask = (oclTask) jcomTasks.getItemAt(0);
		}
		populateTasks(); /* WZ 3/10/2001 */
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * Do the main editor panel with the Substate Class currently being edited
	 * 
	 * @Param return
	 *            List - the task list from current domain
	 */
	private List getExistingTasks() {
		List lst = new ArrayList(); //Need to clone the tasks

		if (!hTask) { /* WZ 30/4/02 */
			ListIterator li = curDomain.tasks.listIterator();
			while (li.hasNext()) {
				try {
					lst.add((oclTask) ((oclTask) li.next()).clone());
				} catch (CloneNotSupportedException e) {
					Utility.debugPrintln(e);
				}
			}
		} else {
			ListIterator li = curDomain.htntasks.listIterator();
			while (li.hasNext()) {
				try {
					lst.add((oclHTNTask) ((oclHTNTask) li.next()).clone());
				} catch (CloneNotSupportedException e) {
					Utility.debugPrintln(e);
				}
			}
		}
		return lst;
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * resetTaskList refresh task list
	 * 
	 * @Param
	 */
	private void resetTaskList() {
		vecTasks.clear();
		jcomTasks.setSelectedItem(null); /* Weihong added on 3/10/2001 */
		ListIterator li = lstTask.listIterator();
		while (li.hasNext()) { /* WZ 30/4/02 */
			if (!hTask)
				vecTasks.addElement((oclTask) li.next());
			else
				vecTasks.addElement((oclHTNTask) li.next());
		}
		jcomTasks.updateUI();
	}

	/* WZ 28/06/2001 */
	/**
	 * Do the main editor panel with the Substate Class currently being edited
	 */
	private Box buildStateEditorBox() {
		// First the state predicate List
		Box stateEdBox = Box.createVerticalBox();
		editPane = new TaskExpressionDocPane(curDomain);
		/* Weihong added on 28/06/2001 */
		editPane.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16));
		/* Weihong added on 28/06/2001 */
		JScrollPane jscrollStateEd = new JScrollPane(editPane);
		jscrollStateEd.setPreferredSize(new Dimension(50, 500));
		/* Weihong added on 28/06/2001 */
		JPanel jpanStateEd = new JPanel();
		jpanStateEd.setLayout(new BoxLayout(jpanStateEd, BoxLayout.Y_AXIS));
		jpanStateEd.setBorder(BorderFactory
				.createTitledBorder("Edit States for Task .."));
		jpanStateEd.add(jscrollStateEd);
		stateEdBox.add(jpanStateEd);

		JPanel jpanUnify = new JPanel(); //add buttons
		/* WZ 4/9/2001 */
		ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir,
				"AlignCenter16.gif");
		JButton cmdClear = new GipoButton("Clear", ii);
		cmdClear.setMnemonic(KeyEvent.VK_C);
		cmdClear.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				clearEditState();
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		JButton cmdUpdateSel = new GipoButton("Update", ii);
		cmdUpdateSel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updateStateList();
				dirty = true; //Ron 3/10/04
			}
		});

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
		JButton cmdAddInitial = new GipoButton("Initial", ii);
		cmdAddInitial.setToolTipText("Add to the initial state");
		cmdAddInitial.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				AddInitial();
				dirty = true; //Ron 3/10/04
			}
		});
		/* end add */

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Properties16.gif");
		JButton cmdAddGoal = new GipoButton("Goal", ii);
		cmdAddGoal.setToolTipText("Add to the goal state");
		cmdAddGoal.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				AddGoal();
				dirty = true; //Ron 3/10/04
			}
		});
		/* end add */

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "New16.gif");
		JButton cmdNew = new GipoButton("New", ii);
		cmdNew.setToolTipText("Create a new task.");
		cmdNew.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				newTask();
				dirty = true; //Ron 3/10/04
			}
		});
		/* end add */

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
		JButton cmdDelSel = new GipoButton("Delete", ii);
		cmdDelSel.setToolTipText("Delete State");
		cmdDelSel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				delSelectedState();
				dirty = true; //Ron 3/10/04
			}
		});
		JPanel boxEditButtons = new JPanel(); /* Weihong added on 28/06/2001 */
		boxEditButtons.setLayout(new GridLayout(2, 0));
		/* Weihong added on 28/06/2001 */
		boxEditButtons.add(cmdNew);
		boxEditButtons.add(cmdClear);
		boxEditButtons.add(cmdDelSel);
		boxEditButtons.add(cmdAddInitial);
		boxEditButtons.add(cmdAddGoal);
		boxEditButtons.add(cmdUpdateSel);
		jpanUnify.add(boxEditButtons);

		stateEdBox.add(jpanUnify);
		return stateEdBox;
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * newTask - clear init and goal lists and edit pane
	 */
	public void newTask() {
		//propose a new name
		String taskName = GipoInputBox.showIntegerInputBox(top, "Task Name",
				"Please Input a task number.");
		if (taskName != null) {
			if (taskName.length() != 0) {
				if (!verifyNames(taskName))
					return;

				/* WZ 30/4/02 */
				if (!hTask) {
					oclTask thisTask = new oclTask(taskName);
					curTask = thisTask;
					lstTask.add(thisTask); //add this task to the list
					resetTaskList();
					jcomTasks.setSelectedItem(thisTask);
					curModel = null;
				} else {
					oclHTNTask thisTask = new oclHTNTask(taskName);
					curTask = thisTask;

					lstTask.add(thisTask); //add this task to the list
					resetTaskList();
					jcomTasks.setSelectedItem(thisTask);
					curModel = null;
				}

				lmInitState.clear();
				lmGoalState.clear();
				// 		try {
				// 		    editPane.clearPane();
				// 		} catch (BadLocationException ble) {
				// 		    Utility.debugPrintln("Cannot remove document content");
				// 		}
				updateUI();
			} else {
				JOptionPane.showMessageDialog(this, "Please enter a name.",
						"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			}
		}
	}

	/* Weihong added on 22/08/2001 */
	/**
	 * copyAsNewTask - copy an existing task as a new task
	 */
	public void copyAsNewTask() {
		/* Weihong changed/added on 6/9/2001 */
		String taskName = GipoInputBox.showIntegerInputBox(top, "Task Name",
				"Please Input a task number.");
		if (taskName != null) {
			if (taskName.length() != 0) {
				if (!verifyNames(taskName))
					return;

				/* WZ 30/4/02 */
				if (!hTask) {
					oclTask thisTask = null;
					try {
						thisTask = (oclTask) curTask.clone();
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln(e);
					}

					thisTask.ID = taskName;
					curTask = thisTask;
					lstTask.add(thisTask); //add this task to the list
					resetTaskList();
					jcomTasks.setSelectedItem(thisTask);
					curModel = null;
				} else {
					oclHTNTask thisTask = null;
					try {
						thisTask = (oclHTNTask) curTask.clone();
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln(e);
					}

					thisTask.ID = taskName;
					curTask = thisTask;
					lstTask.add(thisTask); //add this task to the list
					resetTaskList();
					jcomTasks.setSelectedItem(thisTask);
					curModel = null;
				}

				try {
					editPane.clearPane();
				} catch (BadLocationException ble) {
					Utility.debugPrintln("Cannot remove document content");
				}
				updateUI();
			} else {
				JOptionPane.showMessageDialog(this, "Please enter a name.",
						"Error", JOptionPane.ERROR_MESSAGE, null);
			}
		}
	}

	/* Weihong added on 22/08/2001 */
	/**
	 * renameTask - rename an existing task
	 */
	public void renameTask() {
		//propose a new name
		/* Weihong changed/added on 6/9/2001 */
		String taskName = GipoInputBox.showIntegerInputBox(top, "Task Name",
				"Please Input a task number.");
		if (taskName != null) {
			if (taskName.length() != 0) {
				if (!verifyNames(taskName))
					return;

				if (taskName.length() != 0) {
					curTask.ID = taskName;
					curModel = null;

					try {
						editPane.clearPane();
					} catch (BadLocationException ble) {
						Utility.debugPrintln("Cannot remove document content");
					}
					updateUI();
				} else {
					JOptionPane.showMessageDialog(this, "Please enter a name.",
							"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
				}
			}
		}
	}

	/* Weihong added on 22/08/2001 */
	/**
	 * verifyNames
	 */
	public boolean verifyNames(String taskName) {
		oclTask tmpTask;
		for (int i = 0; i < jcomTasks.getItemCount(); i++) {
			tmpTask = (oclTask) jcomTasks.getItemAt(i);
			if (taskName.equals(tmpTask.ID)) {
				JOptionPane
						.showMessageDialog(
								this,
								"Please select another name as the task with selected name exists.",
								"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
				return false;
			}
		}
		return true;
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * clearEditState - abandon and clear the edit pane do not change the sort
	 * being edited
	 */
	public void clearEditState() {
		try {
			editPane.clearPane();
			jlstInitState.clearSelection();
			jlstGoalState.clearSelection();

		} catch (Exception e) {
			Utility.debugPrintln("Failed to remove document text or model");
		}
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * updateStateList - replace the state list selected state with the result
	 * of editing the state
	 */
	private void updateStateList() {
		if (selectionIndex == -1) {
			JOptionPane.showMessageDialog(this,
					"No existing state being edited.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}

		/* Weihong added on 29/08/2001 */
		if (editPane.onceDeleted && curModel == lmInitState) {
			if (JOptionPane
					.showConfirmDialog(
							this,
							"Initial states must be the full states (oclSS)!\nSome predicates have been deleted when editing states.Confirm if you wish to proceed",
							"GIPO Error", JOptionPane.YES_NO_OPTION,
							JOptionPane.ERROR_MESSAGE, null) == JOptionPane.NO_OPTION)
				return;
		}
		/* Weihong added end */

		if (editPane.getPlainList().size() > 0) {
			//updates task property window
			curModel.remove(selectionIndex);
			if (curModel == lmInitState) {
				oclSS ss = new oclSS(curSort, curObject);
				ss.setState(editPane.getPurePredicateList());
				curModel.insertElementAt(ss, selectionIndex);
			} else if (curModel == lmGoalState) {
				oclSE se = new oclSE(curSort, curObject);
				se.setPredicateList(editPane.getPurePredicateList());
				curModel.insertElementAt(se, selectionIndex);
			}
			clearEditState();
			updateUI();
			//updates lstTask list
			updatesTaskList();
		}
	}

	/* Weihong added on 4/7/2001 */
	/**
	 * updatesTaskList - updates lstTask list
	 */
	private void updatesTaskList() {
		curTask.getInits().clear();
		for (Enumeration e = lmInitState.elements(); e.hasMoreElements();) {
			curTask.addInitSS((oclSS) e.nextElement());
		}
		// RON 8/4/03 BUG clearing goals in hierarchical editor when
		// nothing to replace them
		if (!hTask) {
			curTask.getGoals().clear();
			for (Enumeration e = lmGoalState.elements(); e.hasMoreElements();) {
				curTask.addGoalSE((oclSE) e.nextElement());
			}
		}
	}

	/* Weihong added on 12/7/2001 */
	/**
	 * DeleteOperator - delete the selected task
	 */
	private void DeleteTask() {
		if (curTask != null) { /* Weihong added on 3/10/2001 */
			int k = JOptionPane.showConfirmDialog(top,
					"Delete the chosen task?", "GIPO Confirm",
					JOptionPane.YES_NO_OPTION);
			if (k == JOptionPane.YES_OPTION) {
				lstTask.remove(curTask);
				resetTaskList();
				initTaskList();
			}
		} else { /* Weihong added on 3/10/2001 */
			JOptionPane.showMessageDialog(this, "No task exists.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
		}
	}

	/* Weihong added on 28/08/2001 */
	/**
	 * DeleteOperator - delete the selected task
	 */
	private void PrintTask() {
		int k = JOptionPane.showConfirmDialog(top, "Print the chosen task?",
				"GIPO Confirm", JOptionPane.YES_NO_OPTION);
		if (k == JOptionPane.YES_OPTION) {
			PrinterJob pj = PrinterJob.getPrinterJob();
			pj.setPrintable(curTask);
			if (pj.printDialog()) {
				try {
					pj.print();
				} catch (PrinterException ex) {
					System.err.println(ex);
				}
			}
		}
	}

	/* Weihong added on 28/08/2001 */
	/**
	 * DeleteOperator - delete the selected task
	 */
	private void PrintAllTask() {
		// 	    PrinterJob pj = PrinterJob.getPrinterJob();
		// 	    pj.setPrintable(curTask);
		// 	    if (pj.printDialog()) {
		// 		try {
		// 		    pj.print();
		// 		} catch(PrinterException ex) {
		// 		    System.err.println(ex);
		// 		}
		// 	    }
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * delSelectedState - delete the selected item for the state definition list
	 */
	private void delSelectedState() {
		if (selectionIndex == -1) {
			JOptionPane.showMessageDialog(this,
					"No existing state being edited.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}

		if (curModel != null) {
			curModel.remove(selectionIndex);
			updatesTaskList();
			clearEditState();
			updateUI();
		}
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * AddGoal - add the edited state as a new state in the Goal state list
	 */
	private void AddGoal() {
		if (hTask) { /* Weihong 12/3/02 */
			HGoalStateWindow goalWindow = new HGoalStateWindow(top,
					(oclHTNTask) curTask);
			top.callInternalFrame(goalWindow, true);
		} else {
			if (curTask != null) {
				if (editPane.isDirty()) {
					int inx = lmGoalState.getSize();
					oclSE se = new oclSE(curSort, curObject);
					se.setPredicateList(editPane.getPurePredicateList());
					lmGoalState.insertElementAt(se, inx);

					curTask.addGoalSE(se);
					clearEditState();
					updateUI();
				} else {
					JOptionPane.showMessageDialog(this,
							"No states being edited.", "GIPO Error",
							JOptionPane.ERROR_MESSAGE, null);
				}
			} else {
				JOptionPane.showMessageDialog(this,
						"Please select an existing task or create a new task.",
						"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			}
		}
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * AddInitial - add the edited state as a new state in the initial state
	 * list
	 */
	private void AddInitial() {
		if (editPane.onceDeleted) {
			if (JOptionPane
					.showConfirmDialog(
							this,
							"Initial states must be the full states (oclSS)!\nSome predicates have been deleted when editing states.Confirm if you wish to proceed",
							"GIPO Error", JOptionPane.YES_NO_OPTION,
							JOptionPane.ERROR_MESSAGE, null) == JOptionPane.NO_OPTION)
				return;

		}
		if (curTask != null) {
			if (editPane.isDirty()) {
				int inx = lmInitState.getSize();
				oclSS ss = new oclSS(curSort, curObject);
				ss.setState(editPane.getPurePredicateList());
				lmInitState.insertElementAt(ss, inx);

				curTask.addInitSS(ss);
				clearEditState();
				updateUI();
			} else {
				JOptionPane.showMessageDialog(this, "No states being edited.",
						"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			}
		} else {
			JOptionPane.showMessageDialog(this,
					"Please select an existing task or create a new task.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
		}
	}

	/**
	 * populateInheriatedStates Update the state list after new sort has been
	 * choosen
	 * 
	 * @param sortName -
	 *            the choosen object name String
	 */
	private void populateInheriatedStates(String sortName) {
		curStateList = null;
		ListIterator li = lstSSC.listIterator();
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			if (sortName.equals(cur.getStateSort())) {
				curStateList = cur.getStateList();
				selStateSort = sortName;
				selStateId = cur.getStateSortId();
			}
		}
		if (curStateList == null) {
			return;
		}

		//instantiate curStateList
		List instantiatedList = curDomain.instantiateStateList(selStateId,
				curStateList, curObject);
		/* WZ 20/8/02 */

		// Fill the model;
		li = instantiatedList.listIterator();
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			hLMStates.addElement(cur);
		}
		hStateList.setModel(hLMStates);
	}

	/**
	 * getInheriatedStates return a predicates list of all inheriated states of
	 * a choosen sort
	 * 
	 * @param sortName
	 *            the choosen object name String
	 */
	private List getInheriatedStates(String sortName) {
		List stateList = null;
		List returnList = new ArrayList();
		ListIterator li = lstSSC.listIterator();
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			Utility.debugPrintln("cur.getStateSort()  '" + cur.getStateSort()
					+ "'");
			if (sortName.equals(cur.getStateSort())) {
				stateList = cur.getStateList();
				// 		Utility.debugPrintln("yes!");
				break;
				// 		selStateSort = sortName;
				// 		selStateId = cur.getStateSortId();
			}
		}
		if (stateList == null) {
			return null;
		}

		// 	//instantiate curStateList
		// 	List instantiatedList =
		// instantiateStateList(selStateId,curStateList);

		// 	// Fill the model;
		li = stateList.listIterator();
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			// 	    Utility.debugPrintln("cur '"+cur.toString()+"'");
			ListIterator lili = cur.getPredicateList().listIterator();
			while (lili.hasNext()) {
				returnList.add((oclPredicate) lili.next());
				// 		Utility.debugPrintln("curPredicateAdded .");
			}
		}
		return returnList;
	}

	/* WZ changed for better way to populate inheriated statelist */
	/**
	 * populateStates Update the state list after new sort has been choosen
	 * 
	 * @param sortName
	 * @nodeName
	 */
	public void populateStates(String sortName, String nodeName) {
		curStateList = null;
		oclSSClassDef curSSClassDef = curDomain.getIntegClassDEF(sortName);
		if (curSSClassDef != null && !curSSClassDef.isEmpty()) {
			curStateList = curSSClassDef.getStateList();
			selStateSort = sortName;
			selStateId = curSSClassDef.getStateSortId();
		}
		if (curStateList == null) {
			lmStates.clear();
			return;
		}

		//instantiate curStateList
		List instantiatedList = curDomain.instantiateStateList(selStateId,
				curStateList, curObject);
		/* WZ 20/8/02 */

		// Fill the model;
		lmStates.clear();
		ListIterator li = instantiatedList.listIterator();
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			lmStates.addElement(cur);
		}
		jlstStateDefs.setModel(lmStates);
	}

	/* WZ 28/06/2001 */
	/**
	 * ListSelectionListener Updates editing boxed when a state is choosen
	 */
	//Ron 1/9/01 added clone to predicate
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
						editPane.setObjectID(curObject); /* WZ 22/8/02 */
						ListIterator li = state.getPredicateList()
								.listIterator();
						while (li.hasNext()) {
							oclPredicate cur = (oclPredicate) li.next();
							// Ron 1/9/01 added clone
							editPane.addPredicate((oclPredicate) cur.clone());
						}
					}
				}
			} catch (Exception e) {
				Utility.debugPrintln("Selection exception " + e.toString());
				// Should not happen!!
			}
		}
	};

	/* Weihong added on 28/06/2001 */
	/**
	 * oclSSListSelListener Updates editing boxed when a initial state is
	 * choosen
	 */
	ListSelectionListener oclSSListSelListener = new ListSelectionListener() {
		public void valueChanged(ListSelectionEvent lse) {
			try {
				if (!lse.getValueIsAdjusting()) {
					JList lst = (JList) lse.getSource();
					oclSS state = (oclSS) lst.getSelectedValue();
					if (state == null) {
						Utility.debugPrintln("List selection problem");
					} else {
						curModel = lmInitState;
						selectionIndex = lst.getSelectedIndex();
						curSort = state.getSort();
						curObject = state.getName();

						editPane.clearPane();
						ListIterator li = state.getState().listIterator();
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

	/* Weihong added on 28/06/2001 */
	/**
	 * oclSEListSelListener Updates editing boxed when a goal state is choosen
	 */
	ListSelectionListener oclSEListSelListener = new ListSelectionListener() {
		public void valueChanged(ListSelectionEvent lse) {
			try {
				if (!lse.getValueIsAdjusting()) {
					JList lst = (JList) lse.getSource();
					oclSE state = (oclSE) lst.getSelectedValue();
					if (state == null) {
						Utility.debugPrintln("List selection problem");
					} else {
						curModel = lmGoalState; //for delete
						selectionIndex = lst.getSelectedIndex();
						curSort = state.getSort();
						curObject = state.getName();

						editPane.clearPane();
						ListIterator li = state.getState().listIterator();
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

	/* Weihong added on 28/06/2001 */
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

	/* Weihong added on 28/06/2001 */
	/**
	 * goalStateList show goal states (oclSE)
	 * 
	 * @param task -
	 *            oclTask
	 */
	private void goalStateList(oclTask task) {
		if (task != null) {
			showStateProperty(lmGoalState, task.getGoals(), 2); //oclSS
		}
	}

	/* Weihong added on 28/06/2001 */
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

	/**
	 * verifyTasks carryout the verifications tasks on each defined task
	 */
	private void verifyTasks() {
		if (curDomain.isHierarchical()) {
			ListIterator li = lstTask.listIterator();
			List mssgs = new ArrayList();
			while (li.hasNext()) {
				oclHTNTask cur = (oclHTNTask) li.next();
				mssgs.add("Check task id = " + cur.ID);
				cur.check(curDomain, mssgs);
			}
			if (mssgs.size() > lstTask.size()) {
				//Something to report
				CheckResultFrame.showMessages(top, mssgs, "Task Check Message");
			} else {
				JOptionPane.showMessageDialog(top, "All task checks passed.",
						"GIPO Message", JOptionPane.INFORMATION_MESSAGE, null);
			}

		} else {
			ListIterator li = lstTask.listIterator();
			List mssgs = new ArrayList();
			while (li.hasNext()) {
				oclTask cur = (oclTask) li.next();
				mssgs.add("Check task id = " + cur.ID);
				cur.check(curDomain, mssgs);
			}
			if (mssgs.size() > lstTask.size()) {
				//Something to report
				/* Donghong changed 25/10/02 */
				CheckResultFrame.showMessages(top, mssgs, "Task Check Message");
			} else {
				JOptionPane.showMessageDialog(top, "All task checks passed.",
						"GIPO Message", JOptionPane.INFORMATION_MESSAGE, null);
			}
		}
	}

	/** Closes the dialog */
	private void closeDialog(java.awt.event.WindowEvent evt) {
		if (editPane.isDirty()) {
			int res = JOptionPane.showConfirmDialog(TaskView.this,
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
}
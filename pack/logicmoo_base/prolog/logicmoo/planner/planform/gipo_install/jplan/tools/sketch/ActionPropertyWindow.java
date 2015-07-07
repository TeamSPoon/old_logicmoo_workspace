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

/*
* Created on 11-Jul-2003
*
* Author ron
*/
package jplan.tools.sketch;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import javax.swing.text.BadLocationException;
import java.util.List;
import java.util.*;
import java.util.NoSuchElementException;
import java.awt.event.*;
import java.awt.*;
import java.awt.dnd.*;
import java.awt.print.*; /* Ron/Weihong 12/3/02 */
import java.awt.Color; /* Ron/Weihong 12/3/02 */
import java.awt.image.*; /* Ron/Weihong 12/3/02 */
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
 * @author ron
 *
 * This class implements the ActionPropertyWindow
 * It both displays the current state of an action and
 * in the style of Op maker allows an operator to be defined by generalising from the specific instance
 */
public class ActionPropertyWindow
	extends GipoInternalFrame
	implements PredChangeListener {

	private OclEd top;
	private oclDomain curDomain;
	private List sorts;
	private oclPredicate actionPred; // Used in the editable pred tree
	private List lstActions; // This is the working list
	private List lstTasks; // Currently defined tasks
	private int selPredIndex = -1; // The index of the currently edited
	//predicate in the list box
	private boolean dirty = false; // flag to monitor changes being committed
	private String actionName;
	private PredTree predTree;
	//This is the predicate version of the actionName
	// Sort Object Tree
	private SortTree sortTree;
	
	private TaskExpressionDocPane preTrans;
	private TaskExpressionDocPane postTrans;
	private JList jlstStateDefs;
	private DefaultListModel lmStates; //The current/displayed sort's states
	private String curSort = null, curObject = null;
	private FilterStatePane flwCurState; //This displays the current state
	private ArrayList curState; // The current state
	private oclTask curTask; // The task to be sketched
	
	/**
	   * Constructor
	   * @param the action name
	   * @param the action start time
	   * @param toclDomain the current active domain
	   * @param top level reference to main GIPO Window
	   */
	public ActionPropertyWindow(
		String name,
		int time,
		oclDomain curDomain,
		OclEd parent) {
		super(parent,false);
		//setModal(true);
		if (curDomain == null) {
			JOptionPane.showMessageDialog(
				parent,
				"No Domain currently being edited.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		List mssgs = curDomain.sortCycleTest();
		if (mssgs.size() > 0) {
			JOptionPane.showMessageDialog(
				parent,
				"Cannot display the sort/object tree\n"
					+ (String) mssgs.get(0)
					+ "\nEdit and repair the sort/object definitions first.",
				EdStrings.strErrorHeading,
				JOptionPane.ERROR_MESSAGE);
			return;
		}
		setTitle("Action Properties [" + name + " at " + time +"]");
		this.curDomain = curDomain;
		actionName = name;
		sorts = curDomain.sorts; //had reference to parent
		lstTasks = curDomain.tasks;
		lstActions = new ArrayList();
		top = parent;

		initComponents();
		dirty = false;
		pack();
		setVisible(true);
		try {
			SortTreeDragSource dragSource =
				new SortTreeDragSource(
					sortTree,
					DnDConstants.ACTION_COPY,
					true);
			// Cannot drop on this tree
			sortTree.setEditable(false);
		} catch (Exception e) {
			Utility.debugPrintln("Tree exception " + e.toString());
		}
	}

	/**
	 * initComponents
	 * Sets up the user interface
	 */
	private void initComponents() {
		addInternalFrameListener(new InternalFrameAdapter() {
				public void internalFrameClosing (InternalFrameEvent evt) {
					closeDialog();
				}
		});
		JButton cmdDummy = new JButton("XXX");
		int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
		cmdDummy = null;

		// The first Vertical pane contains the Sort/Object Tree
		Box boxSort = Box.createVerticalBox();

		//  Now put the sort components together
		sortTree = new SortTree(curDomain);
		ToolTipManager.sharedInstance().registerComponent(sortTree);
		DefaultTreeCellRenderer sortTreeRend =
			(DefaultTreeCellRenderer) sortTree.getCellRenderer();
		sortTreeRend.setToolTipText(
			"Drag and drop objects to add action arguments.");
		sortTree.setPreferredSize(new Dimension(250, 100));
		JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
		JPanel jpanSorts = new JPanel();
		jpanSorts.setLayout(new BoxLayout(jpanSorts, BoxLayout.Y_AXIS));
		jpanSorts.setBorder(BorderFactory.createTitledBorder("Object Tree .."));
		jpanSorts.add(scrollPaneSorts);

		boxSort.add(jpanSorts);
		// Now the object states
//		build current state List
//		build current state List
		flwCurState = new FilterStatePane(curDomain,"Current State");
		if (curTask != null) {
			java.util.List inits = curTask.getInits();
			flwCurState.showStateProperty(inits, 1);
			ListIterator li = inits.listIterator();
			while (li.hasNext()) {
				curState.add(li.next());
			}
		}
			
//		jlstStateDefs = new JList();
//		lmStates = new DefaultListModel();
//		jlstStateDefs.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
//		jlstStateDefs.addListSelectionListener(stateListSelListener);
//		ListCellRenderer plRenderer = new PredListCellRenderer();
//		jlstStateDefs.setCellRenderer(plRenderer);
//		JScrollPane jscrollStateDefs = new JScrollPane(jlstStateDefs);
//		jscrollStateDefs.setBorder(
//			BorderFactory.createTitledBorder("States .."));
		boxSort.add(flwCurState);

		// The Second Vertical pane contains the action/Object Tree
		Box boxAction = Box.createVerticalBox();

		//		Action Name Predicate Editing tree

		actionPred = new oclPredicate(actionName);
		//actionPred.addConstArgument("empty");
		predTree = new PredTree(actionPred, this, -1);
		TreeSelectionListener predTreeSelListener =
			new TreeSelectionListener() {
				public void valueChanged(TreeSelectionEvent tse) {
				final SortTree tree = (SortTree) tse.getSource();
				try {
					if (tree.isSelectedObject()) {
						if (preTrans.isDirty()) {
							int k =
								JOptionPane.showConfirmDialog(
									top,
									"Content in the task editing window has not been updated.\nClear the content without updating?",
									"GIPO Confirm",
									JOptionPane.YES_NO_OPTION);
							if (k == JOptionPane.YES_OPTION) {
								try {
									preTrans.clearPane();
								} catch (BadLocationException ble) {
									Utility.debugPrintln(
										"Cannot remove document content");
								}
							} else {
								return;
							}
						}
						String nodeName = tree.getSelectedNodeName();
						curSort = curDomain.getSortOfObject(nodeName);
						curObject = nodeName;
						populateStates(curSort, nodeName);
					}
				} catch (SortSelectionException e) {
					Utility.debugPrintln("Unexpected failure - initComponents");
				}
				updateUI(); /* Weihong added on 7/12/2001 */
			}
		};
		ToolTipManager.sharedInstance().registerComponent(predTree);
		DefaultTreeCellRenderer treeRend =
			(DefaultTreeCellRenderer) predTree.getCellRenderer();
		treeRend.setToolTipText("Drag and drop objects to define arguments.");
		JScrollPane scrollPaneEdPred = new JScrollPane(predTree);
		try {
			PredTreeDragSource dragSource = new PredTreeDragSource(predTree);
			PredTreeDropTarget dropTarget = new PredTreeDropTarget(predTree);
			predTree.setEditable(true);
		} catch (Exception e) {
			Utility.debugPrintln("Tree exception " + e.toString());
		}
		// TODO Remember action name could be existing action
		JPanel jpanEdPred = new JPanel();
		jpanEdPred.setLayout(new BoxLayout(jpanEdPred, BoxLayout.Y_AXIS));
		jpanEdPred.setBorder(
			BorderFactory.createTitledBorder("Edit Action .."));
		boxAction.add(scrollPaneEdPred);
		
		// The third vertical pane contains the transition panes for the selected object
		Box boxTrans = Box.createVerticalBox();
		preTrans = new TaskExpressionDocPane(curDomain);
		preTrans.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16));
		JScrollPane jscrollPre = new JScrollPane(preTrans);
		jscrollPre.setPreferredSize(new Dimension(50, 500));
		JPanel jpanPre = new JPanel();
		jpanPre.setLayout(new BoxLayout(jpanPre, BoxLayout.Y_AXIS));
		jpanPre.setBorder(
			BorderFactory.createTitledBorder("Current State of Object .."));
		jpanPre.add(jscrollPre);
		boxTrans.add(jpanPre);
		
		postTrans = new TaskExpressionDocPane(curDomain);
		postTrans.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16));
		JScrollPane jscrollPost = new JScrollPane(postTrans);
		jscrollPost.setPreferredSize(new Dimension(50, 500));
		JPanel jpanPost = new JPanel();
		jpanPost.setLayout(new BoxLayout(jpanPost, BoxLayout.Y_AXIS));
		jpanPost.setBorder(
			BorderFactory.createTitledBorder("Next State of Object .."));
		jpanPost.add(jscrollPost);
		boxTrans.add(jpanPost);


		// The command buttons
		ImageIcon ii =
			ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		JButton cmdCommit = new JButton("Commit", ii);
		cmdCommit.setMnemonic(KeyEvent.VK_M);
		cmdCommit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				//				TODO commitAction();
			}
		});

		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		JButton cmdClose = new JButton("Close", ii);
		cmdClose.setMnemonic(KeyEvent.VK_L);
		cmdClose.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				closeDialog();
			}
		});

		Box boxTop = Box.createVerticalBox();
		Box boxEd = Box.createHorizontalBox(); // Holds the main panes
		JPanel boxFinalButtons = new JPanel();
		boxFinalButtons.setLayout(new FlowLayout());

		boxFinalButtons.add(cmdCommit);
		boxFinalButtons.add(cmdClose);

		boxEd.add(boxSort);
		boxEd.add(boxAction);
		boxEd.add(boxTrans);

		boxTop.add(boxEd);
		boxTop.add(boxFinalButtons);

		getContentPane().add(boxTop);
		//		initPredTreePopup(predTree);

	}
	
	/**
	 * ListSelectionListener
	 * Updates editing boxed when a state is choosen
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
						postTrans.clearPane();
						postTrans.setObjectID(curObject); /* WZ 22/8/02 */
						ListIterator li =
							state.getPredicateList().listIterator();
						while (li.hasNext()) {
							oclPredicate cur = (oclPredicate) li.next();
							postTrans.addPredicate((oclPredicate) cur.clone());
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
	 * populateStates
	 * Update the current state box for selected action argument
	 */
	private void populateStates(String curSort, String nodeName){
		
	}

	/**
	 * setCurState
	 * set the state to calculated state for action start
	 * @param - the state
	 */
	public void setCurState(List state) {
		if (state != null)
			flwCurState.showStateProperty(state, 1);
	}

	/* (non-Javadoc)
	 * @see jplan.edexpt.PredChangeListener#updatePredicateAt(jplan.ocl.oclPredicate, int)
	 */
	public void updatePredicateAt(oclPredicate curPred, int lstIndex) {
		// TODO Auto-generated method stub

	}

	/** 
	 * close this dialog window
	 * @return void
	 */
	private void closeDialog() {
		if (dirty) {
			int res =
				JOptionPane.showConfirmDialog(
					ActionPropertyWindow.this,
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

}

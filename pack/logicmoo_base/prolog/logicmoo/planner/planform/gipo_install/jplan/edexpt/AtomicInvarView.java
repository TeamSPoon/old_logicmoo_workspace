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
 * AtomicInvarView
 * This is the OCL expert editor for atomic invariants  
 * Part of the edexpt (Editor for Experts) Views
 * @author Ron Simpson
 * @version oclPlus
 * @history this is heavily reworked for oclPlus
 */

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.util.List;
import java.util.*;
import java.awt.event.*;
import java.awt.*;
import java.awt.dnd.*;

import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.general.*;
import jplan.images.ImageLoader; /* Weihong changed on 5/9/2001 */

public class AtomicInvarView
	extends GipoInternalFrame { /* Weihong added on 24/10/2001 */
	private OclEd top;
	private oclDomain curDomain;
	private List sorts;
	private oclPredicate dummyPred; // Used in empty pred tree
	private List lstPreds; // This is the working list
	private int selPredIndex = -1; // The index of the currently edited
	//predicate in the list box
	private boolean dirty = false; // flag to monitor changes being committed

	// Interface components or their Models
	private SortTree sortTree;
	private JList jlstPreds;
	private JList jlstAtomic;
	private DefaultListModel lmPreds; //Use for static predicates
	private OrderedListModel lmAtomic; //Use for atomic predicates
	private JTextField txtFluentVal = null; // Ron 30/06/03 added to manafe fluen values
	private JLabel lblFluentVal = null;  // Ron 30/06/03 added to manafe fluen values
	private StaticPredTree predTree;
	private JButton cmdAddPred = null; 
	private JButton cmdUpdatePred = null;
	
	private static final int NEWPRED = 1;
	private static final int UPDATEPRED = 2;
	
	
	/**
	 * Constructor
	 * @param toclDomain the current active domain
	 * @param top level Podium reference
	 * @param boolean to determine if dialog is modal normally not
	 */
	public AtomicInvarView(oclDomain curDomain, OclEd parent) {
		//     super("",true,true,true,true);
		super(parent); /* Weihong added on 24/10/2001 */
		setClosable(false); /* Weihong added on 11/10/2001 */
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
				"Cannot display atomic invariants\n"
					+ (String) mssgs.get(0)
					+ "\nEdit and repair the sort definitions first.",
				EdStrings.strErrorHeading,
				JOptionPane.ERROR_MESSAGE);
			return;
		}
		this.curDomain = curDomain;
		if (curDomain.oclPlus) {
			setTitle("Atomic Invariants View (Durative Actions)");
		} else {
			setTitle("Atomic Invariants View");
		}
		sorts = curDomain.sorts; //had reference to parent
		// the domain predicates
		lstPreds = curDomain.predicates;
		top = parent;
		// Find the Static predicates
		lmPreds = new DefaultListModel();
		// Fill the model;
		ListIterator liPreds = lstPreds.listIterator();
		while (liPreds.hasNext()) {
			oclPredicate cur = (oclPredicate) liPreds.next();
			if (cur.isStatic()) {
				lmPreds.addElement(cur);
			}
		}
		if (curDomain.oclPlus) {
			liPreds = curDomain.functors.listIterator();
			while (liPreds.hasNext()) {
				Object pred = liPreds.next();
				if (pred instanceof oclFunctor) {
					oclFunctor cur = (oclFunctor)pred;
					if (cur.isStatic()) {
						lmPreds.addElement(cur);
					}
				} else if (pred instanceof oclPredicate) {
					oclPredicate cur = (oclPredicate)pred;
					if (cur.isStatic()) {
						lmPreds.addElement(cur);
					}
				}
			}
		}
		// Ron 30/06/03 Added for oclPlus static fluents
		if (lmPreds.getSize() == 0) {
			JOptionPane.showMessageDialog(
				parent,
				"There are no static predicates defined for this domain.\n"
					+ "If required add in the Predicate view first",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		// Find the existing Atomic Predicates
		lmAtomic = new OrderedListModel();
		// Fill the model;
		try {
			ListIterator liAtomic = curDomain.atomicInvars.listIterator();
			while (liAtomic.hasNext()) {
				oclPredicate cur = (oclPredicate) liAtomic.next();
				if (cur.isFluent())
					lmAtomic.addOrdElement((oclFunctor) cur.clone());
				else
					lmAtomic.addOrdElement((oclPredicate) cur.clone());
			}
		} catch (Exception e) {
			Utility.debugPrintln("Unexpected clone problem");
		}
		initComponents();
		dirty = false;
		//setSize(400,400);
		//show();
		pack();
		setVisible(true);
		try {
			ObjectDragSource dragSource =
				new ObjectDragSource(sortTree, DnDConstants.ACTION_COPY);
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
		int tWidth;

		JButton cmdDummy = new JButton("XXX");
		int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
		cmdDummy = null;

		Box boxTop = Box.createVerticalBox();
		Box boxEd = Box.createHorizontalBox(); // Holds the main panes
		Box boxFinalButtons = Box.createHorizontalBox();

		// The first Vertical pane contains the Sort Tree

		//  Now put the sort components together
		sortTree = new SortTree(curDomain);
		JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
		JPanel jpanSorts = new JPanel();
		jpanSorts.setLayout(new BoxLayout(jpanSorts, BoxLayout.Y_AXIS));
		jpanSorts.setBorder(BorderFactory.createTitledBorder("Sorts .."));
		jpanSorts.add(scrollPaneSorts);

		// The Second Vertical pane contains the Predicate List

		Box boxPreds = Box.createVerticalBox();
		// The list of static predicates is already created
		jlstPreds = new JList(lmPreds);
		jlstPreds.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListCellRenderer renderer =
			new PredCellRenderer(top.strImageDir, "static.png","fluent.png","sfluent.png");
		jlstPreds.setCellRenderer(renderer);
		jlstPreds.addListSelectionListener(predListSelListener);
		JScrollPane scrollPanePreds = new JScrollPane(jlstPreds);
		JPanel jpanPreds = new JPanel();
		jpanPreds.setLayout(new BoxLayout(jpanPreds, BoxLayout.Y_AXIS));
		jpanPreds.setBorder(
			BorderFactory.createTitledBorder("Static Predicates .."));
		jpanPreds.add(scrollPanePreds);

		/* Weihong added on 4/07/2001 */
		// 	ImageIcon ii = new ImageIcon(top.strImageDir + "Add16.gif");
		/* Weihong changed on 4/9/2001 */
		ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
		cmdAddPred = new JButton("Add", ii);
		cmdAddPred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				addNewPred();
			}
		});
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "Refresh16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		cmdUpdatePred = new JButton("Update", ii);
		cmdUpdatePred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updatePred();
			}
		});
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "Delete16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
		JButton cmdDelPred = new JButton("Delete", ii);
		cmdDelPred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				delSelectedPred();
			}
		});

		// Predicate Editing tree
		dummyPred = new oclPredicate("none-selected");
		dummyPred.addConstArgument("empty");
		predTree = new StaticPredTree(dummyPred, this, -1);
		predTree.setCurDomain(curDomain);
		JScrollPane scrollPaneEdPred = new JScrollPane(predTree);
		try {
			StaticPredTreeDropTarget dropTarget =
				new StaticPredTreeDropTarget(predTree);
			predTree.setEditable(true);
		} catch (Exception e) {
			Utility.debugPrintln("Tree exception " + e.toString());
		}
		// Update the tree with first predicate in list box if any
		if (lmPreds.getSize() > 0)
			jlstPreds.setSelectedIndex(0);
		Dimension d = scrollPaneEdPred.getPreferredSize();
		scrollPaneEdPred.setPreferredSize(new Dimension(d.width, 150));
		JPanel jpanEdPred = new JPanel();
		jpanEdPred.setLayout(new BoxLayout(jpanEdPred, BoxLayout.Y_AXIS));
		jpanEdPred.setBorder(
			BorderFactory.createTitledBorder("Edit Predicate .."));
		jpanEdPred.add(scrollPaneEdPred);
		boxPreds.add(jpanPreds);
		boxPreds.add(jpanEdPred);
		// Ron 30/06/03 oclPlus + box to get / display fluent value
		if (curDomain.oclPlus) {
			JPanel jpanFluentVal = new JPanel();
			jpanFluentVal.setBorder(
			BorderFactory.createTitledBorder("Functor Value .."));
			lblFluentVal = new JLabel("Value");
			txtFluentVal = new JTextField("0.0",13);
			jpanFluentVal.add(lblFluentVal);
			jpanFluentVal.add(txtFluentVal);
			boxPreds.add(jpanFluentVal);
			updateValField(false);
		}
		Box boxCmds = Box.createHorizontalBox();
		boxCmds.add(cmdAddPred);
		boxCmds.add(cmdUpdatePred);
		boxCmds.add(cmdDelPred);

		boxPreds.add(boxCmds);

		// The list of created atomic invariants
		jlstAtomic = new JList(lmAtomic);
		jlstAtomic.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		jlstAtomic.addListSelectionListener(atomicListSelListener);
		FluentRenderer rendererFluents =
					new FluentRenderer();
		jlstAtomic.setCellRenderer(rendererFluents);
		JScrollPane scrollPaneAtomic = new JScrollPane(jlstAtomic);
		JPanel jpanAtomic = new JPanel();
		jpanAtomic.setLayout(new BoxLayout(jpanAtomic, BoxLayout.Y_AXIS));
		jpanAtomic.setBorder(
			BorderFactory.createTitledBorder("All Predicates .."));
		jpanAtomic.add(scrollPaneAtomic);

		// Create The Final Buttons
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "SendMail16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		JButton cmdCommit = new JButton("Commit", ii);
		cmdCommit.setMnemonic(KeyEvent.VK_M);
		cmdCommit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				java.util.List errs = checkAtomic(false);
				if (errs.size() > 0) {
					ListIterator li = errs.listIterator();
					String mssgs = (String) li.next();
					while (li.hasNext()) {
						mssgs = mssgs + "\n" + (String) li.next();
					}
					JOptionPane.showMessageDialog(
						AtomicInvarView.this,
						mssgs,
						EdStrings.strErrorHeading,
						JOptionPane.ERROR_MESSAGE,
						null);

				} else {
					checkAtomic(true);
					ArrayList newPreds = new ArrayList();
					try {
						for (int n = 0; n < lmAtomic.size(); n++) {
							oclPredicate cur = (oclPredicate)lmAtomic.getElementAt(n);
							if (cur.isFluent())
								newPreds.add((oclFunctor)cur.clone());
							else
								newPreds.add((oclPredicate)cur.clone());
						}
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln("Unexpected clone failure.");
						return;
					}
					curDomain.atomicInvars = newPreds;
					dirty = false;

					/* Weihong added on 18/07/2001 */
					top.updateWindow(getTitle());
				}
			}
		});
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "Remove16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Remove16.gif");
		JButton cmdRestore = new JButton("Restore", ii);
		cmdRestore.setMnemonic(KeyEvent.VK_R);
		cmdRestore.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Save COPY of the domain atomic predicates
				// Find the existing Atomic Predicates
				lmAtomic.removeAllElements();
				// Fill the model;
				try {
					ListIterator liAtomic =
						curDomain.atomicInvars.listIterator();
					while (liAtomic.hasNext()) {
						oclPredicate cur = (oclPredicate) liAtomic.next();
						if (cur.isFluent())
							lmAtomic.addOrdElement((oclFunctor) cur.clone());
						else
							lmAtomic.addOrdElement((oclPredicate) cur.clone());
					}
				} catch (Exception e) {
					Utility.debugPrintln("Unexpected clone problem");
				}
				dirty = false;
			}
		});
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "Replace16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
		JButton cmdVerify = new JButton("Verify", ii);
		cmdVerify.setMnemonic(KeyEvent.VK_V);
		cmdVerify.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				List mssgs = checkAtomic(false);
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
						AtomicInvarView.this,
						msg,
						EdStrings.strErrorHeading,
						JOptionPane.ERROR_MESSAGE,
						null);
				} else {
					JOptionPane.showMessageDialog(
						AtomicInvarView.this,
						EdStrings.strVerifyOK,
						EdStrings.strInfoHeading,
						JOptionPane.INFORMATION_MESSAGE,
						null);
				}

			}
		});
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "Stop16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		JButton cmdClose = new JButton("Close", ii);
		cmdClose.setMnemonic(KeyEvent.VK_L);
		cmdClose.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				if (dirty) {
					int res =
						JOptionPane.showConfirmDialog(
							AtomicInvarView.this,
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
		});

		boxFinalButtons.add(cmdCommit);
		boxFinalButtons.add(cmdVerify);
		boxFinalButtons.add(cmdRestore);
		boxFinalButtons.add(cmdClose);

		boxEd.add(jpanSorts);
		boxEd.add(boxPreds);
		boxEd.add(jpanAtomic);

		boxTop.add(boxEd);
		boxTop.add(boxFinalButtons);

		getContentPane().add(boxTop);
		initPredTreePopup(predTree);

	}

	/**
	 * init popup menu
	 * create the popup menu
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
	 * cutNodeCommand - delete the selected argument from the predicare tree
	 */

	private void cutNodeCommand() {
		TreePath path = predTree.getSelectionPath();
		if (path == null) {
			JOptionPane.showMessageDialog(
				top,
				"Select a predicate argument first.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		DefaultMutableTreeNode toDel =
			(DefaultMutableTreeNode) path.getLastPathComponent();
		boolean res = predTree.delNode(toDel);
		if (!res) {
			JOptionPane.showMessageDialog(
				top,
				"Select a predicate argument. \nCannot delete the Predicate Name here.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
	}

	/**
	 * ListSelectionListener
	 * Updates editing boxed when a predicate is choosen
	 */
	ListSelectionListener predListSelListener = new ListSelectionListener() {
		public void valueChanged(ListSelectionEvent lse) {
			try {
				if (!lse.getValueIsAdjusting()) {
					// First disable update button and enable add button
					enableCommandButtons(NEWPRED);
					JList lst = (JList) lse.getSource();
					if (curDomain.oclPlus) {
						Object pred =  lst.getSelectedValue();
						if (pred == null) {
							predTree.setTreePred(dummyPred, -1);
							selPredIndex = -1;
						} else {
							selPredIndex = lst.getSelectedIndex();
							if (((oclPredicate)pred).isFluent()) {
								try {
									oclFunctor newFluent = (oclFunctor) ((oclFunctor)pred).clone();
									predTree.setTreePred(newFluent, selPredIndex);
									updateValField(true);
								} catch (Exception e) {}
							} else {
								try {
									oclPredicate newPred = (oclPredicate) ((oclPredicate)pred).clone();
									predTree.setTreePred(newPred, selPredIndex);
									updateValField(false);
								} catch (Exception e) {}
							}
						}
					} else {
						oclPredicate pred = (oclPredicate) lst.getSelectedValue();
						if (pred == null) {
							predTree.setTreePred(dummyPred, -1);
							selPredIndex = -1;
						} else {
							selPredIndex = lst.getSelectedIndex();
							try {
								oclPredicate newPred = (oclPredicate) pred.clone();
								predTree.setTreePred(newPred, selPredIndex);
							} catch (Exception e) {
							}
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
	 * ListSelectionListener for the atomic invariant list
	 * Updates editing boxed when a predicate is choosen
	 */
	ListSelectionListener atomicListSelListener = new ListSelectionListener() {
		public void valueChanged(ListSelectionEvent lse) {
			try {
				if (!lse.getValueIsAdjusting()) {
					// First clear buttons
					enableCommandButtons(UPDATEPRED);
					JList lst = (JList) lse.getSource();
					oclPredicate pred = (oclPredicate) lst.getSelectedValue();
					selPredIndex = lst.getSelectedIndex();
					// find the prototype
					int count = 0;
					boolean found = false;
					oclPredicate proto = null;
					while (!found && count < lmPreds.getSize()) {
						proto = (oclPredicate) lmPreds.get(count);
						if (proto.getName().equals(pred.getName()))
							found = true;
						count++;
					}
					if (!found) {
						JOptionPane.showMessageDialog(
							top,
							"A matching static predicate does not"
								+ " exist.\nPlease define one first.",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE,
							null);
						return;
					}
					if (pred.isFluent()) {
						oclFunctor newPred = null;
						try {
							newPred = (oclFunctor) pred.clone();

						} catch (Exception e) {
							Utility.debugPrintln("Unexpected clone problem");
						}
						predTree.setTreeExistingPred(newPred, proto, count - 1);
						updateValField(true);
						txtFluentVal.setText(Double.toString(pred.getFluentValue()));
					} else {
						oclPredicate newPred = null;
						try {
							newPred = (oclPredicate) pred.clone();

						} catch (Exception e) {
							Utility.debugPrintln("Unexpected clone problem");
						}
						updateValField(false);
						predTree.setTreeExistingPred(newPred, proto, count - 1);
					}
				}
			} catch (Exception e) {
				Utility.debugPrintln("Selection exception " + e.toString());
				// Should not happen!!
			}
		}
	};
	
	/**
	 * enableCommandButtons
	 * toggle between new and update button
	 * @param enable
	 */
	private void enableCommandButtons(int enable){
		if (enable == NEWPRED) {
			cmdAddPred.setEnabled(true);
			cmdUpdatePred.setEnabled(false);
		} else {
			cmdAddPred.setEnabled(false);
			cmdUpdatePred.setEnabled(true);	
		}
	}
	
	/**
	 * updateValField
	 * @param - true if Functor value editing is enabled
	 */
	private void updateValField(boolean enable){
		lblFluentVal.setEnabled(enable);
		txtFluentVal.setEnabled(enable);
		txtFluentVal.setText("<value>");
		updateUI();
	}
	
	/**
	 * addNewPred
	 * add the edited predicate to the atomic invariant list
	 */
	public void addNewPred() {
		if (!predTree.predicateComplete()) {
			JOptionPane.showMessageDialog(
				top,
				"Define all arguments.\nReplace sort names.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		oclPredicate cur = predTree.treeToPredicate();
		// Now check that this is indeed a new predicate
		String strCur = cur.toString();
		int inx = 0;
		boolean found = false;
		while (!found && inx < lmAtomic.getSize()) {
			oclPredicate atom = (oclPredicate) lmAtomic.get(inx);
			if (strCur.equals(atom.toString())) {
				found = true;
			}
			inx++;
		}
		if (found) {
			JOptionPane.showMessageDialog(
				top,
				"This predicate already exists in the list.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (curDomain.oclPlus) {
			// Find the prototype is this a fluent
			// Ron 1/07/03
			inx = 0;
			found = false;
			oclPredicate atom = null;
			while (!found && inx < lmPreds.getSize()) {
				atom = (oclPredicate) lmPreds.get(inx);
				if (atom.getName().equals(cur.getName())) {
					found = true;
				}
				inx++;	
			}
			if (atom != null && atom.isFluent()) {
				// Check the value
				double dVal;
				String strVal = txtFluentVal.getText();
				try {
					dVal = Double.parseDouble(strVal);
				} catch (Exception e) {
					JOptionPane.showMessageDialog(
						top,
						"The fluent value is not given as a legal number.",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					return;
				}
				oclFunctor fun = new oclFunctor(cur);
				fun.setFluentValue(dVal);
				lmAtomic.addOrdElement(fun);
			} else {
				lmAtomic.addOrdElement(cur);
			}
		} else {
			lmAtomic.addOrdElement(cur);
		}
		setEditPred();
		dirty = true;

	}

	/**
	 * updatePred
	 * add the edited predicate to the atomic invariant list
	 * replace the currently selected invariant
	 */
	public void updatePred() {
		int selinx = jlstAtomic.getSelectedIndex();
		if (selinx == -1) {
			JOptionPane.showMessageDialog(
				top,
				"There is no selected invariant to update",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (!predTree.predicateComplete()) {
			JOptionPane.showMessageDialog(
				top,
				"Define all arguments.\nReplace sort names.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			jlstAtomic.clearSelection();
			return;
		}

		oclPredicate cur = predTree.treeToPredicate();
		// Now check that this is indeed a new predicate
		String strCur = cur.toString();
		if (curDomain.oclPlus) {
			// Find the prototype for this  fluent/predicate
			// Ron 1/07/03
			int inx = 0;
			boolean found = false;
			oclPredicate proto = null;
			while (!found && inx < lmPreds.getSize()) {
				proto = (oclPredicate) lmPreds.get(inx);
				if (proto.getName().equals(cur.getName())) {
					found = true;
				}
				inx++;	
			}
			if (!found) {
				JOptionPane.showMessageDialog(
					top,
					"This functor/predicate does not exists in the list.\n" +
					"please add prototype to the predicates list or delete.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			// Now check if we already have entries for this predicate/fluent
			found = false;
			inx = 0;
			oclPredicate atom = null;
			while (!found && inx < lmAtomic.getSize()) {
				atom = (oclPredicate) lmAtomic.get(inx);
				if (strCur.equals(atom.toString())) {
					found = true;
					if (proto.isFluent() && inx != selinx) {
						JOptionPane.showMessageDialog(
							top,
							"This functor already exists in the list.",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE,
							null);
						return;
					} else if (!proto.isFluent()) {
						JOptionPane.showMessageDialog(
							top,
							"This predicate already exists in the list.",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE,
							null);
						return;
					}
				}
				inx++;
			}
			if (atom != null && proto.isFluent()) {
				// Check the value
				double dVal;
				String strVal = txtFluentVal.getText();
				try {
					dVal = Double.parseDouble(strVal);
				} catch (Exception e) {
					JOptionPane.showMessageDialog(
						top,
						"The fluent value is not given as a legal number.",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					return;
				}
				oclFunctor fun = new oclFunctor(cur);
				fun.setFluentValue(dVal);
				lmAtomic.removeElementAt(selinx);
				lmAtomic.insertElementAt(fun, selinx);
				
			} else {
				lmAtomic.removeElementAt(selinx);
				lmAtomic.insertElementAt(cur, selinx);
			}
		} else {
			int inx = 0;
			boolean found = false;
			while (!found && inx < lmAtomic.getSize()) {
				oclPredicate atom = (oclPredicate) lmAtomic.get(inx);
				if (strCur.equals(atom.toString())) {
					found = true;
				}
				inx++;
			}
			if (found) {
				JOptionPane.showMessageDialog(
					top,
					"This predicate already exists in the list.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			
		}
		setEditPred();
		dirty = true;

	}

	/**
	 * setEditPred
	 * set the edit tree to the currently selected predicate prototype
	 */
	private void setEditPred() {
		jlstAtomic.clearSelection();
		oclPredicate pred = (oclPredicate) jlstPreds.getSelectedValue();
		if (pred == null) {
			predTree.setTreePred(dummyPred, -1);
			selPredIndex = -1;
		} else {
			selPredIndex = jlstPreds.getSelectedIndex();
			try {
				oclPredicate newPred = (oclPredicate) pred.clone();
				predTree.setTreePred(newPred, selPredIndex);
				if (curDomain.oclPlus) {
					if (newPred.isFluent()) {
						updateValField(true);
					} else {
						updateValField(false);
					}
				}
			} catch (Exception e) {
			}
		}
	}

	/**
	 * delSelectedPred - delete the currently selected predicate
	 * from the  atomic invar list
	 */
	public void delSelectedPred() {
		int inx = jlstAtomic.getSelectedIndex();
		if (inx == -1) {
			JOptionPane.showMessageDialog(
				top,
				"No Atomic Invariant Predicate Currently Selected.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		// Now get rid from the JList
		DefaultListModel lmAtomic = (DefaultListModel) jlstAtomic.getModel();
		lmAtomic.removeElementAt(inx);
		// Reset the predicate tree
		setEditPred();
	}

	/**
	 * checkAtomic
	 * check the atomic invariants
	 * 1. predicates are listed
	 * 2. all arguments are of the correct sort
	 * 3. all entries are unique
	 * 4. all static predicates have entries in the atomic invariants list
	 * SIDE EFFECT - mark predicates as static if not already done so
	 * pre-condition sort and predicate tests have been done.
	 * @param boolean doSideEffects - if true the static predicates are 
	 *                marked as static
	 * @return List  of (Strings) error messages - empty if all checks passed
	 */
	public List checkAtomic(boolean doSideEffects) {
		List mssgs = new ArrayList();
		List badPreds = new ArrayList();
		List badObjects = new ArrayList();
		TreeSet atomicSet = new TreeSet();
		TreeSet staticPreds = new TreeSet();

		oclPredicate proto = null;
		for (int n = 0; n < lmAtomic.size(); n++) {
			oclPredicate cur = (oclPredicate) lmAtomic.getElementAt(n);
			try {
				proto = curDomain.findPrototype(cur);
			} catch (OCLSelectionException e) {
				if (!oclDomain.listContainsString(cur.getName(), badPreds)) {
					mssgs.add(
						"There is no predicate definition for the"
							+ " predicate "
							+ cur.getName());
					badPreds.add(cur.getName());
				}
				break;
			}
			if (doSideEffects && !proto.isStatic()) {
				proto.setStatic(true);
			}
			staticPreds.add(proto.toString());
			List args = cur.getArguments();
			List sorts = proto.getArguments();
			ListIterator liArgs = args.listIterator();
			ListIterator liSorts = sorts.listIterator();
			while (liArgs.hasNext()) {
				String argName = ((OPredicate.pArg) liArgs.next()).name;
				String sortName = ((OPredicate.pArg) liSorts.next()).name;
				if (!curDomain.isObjectOfSort(argName, sortName)) {
					if (!oclDomain.listContainsString(argName, badObjects))
						mssgs.add(
							"Object "
								+ argName
								+ " is not defined as an object of sort "
								+ sortName);
					badObjects.add(argName);
				}
			}
			if (!atomicSet.add(cur.toString())) {
				mssgs.add(
					cur.toString()
						+ " is already defined as an atomic invariant.");
			}
		}
		// Now check that all static predicates have been used
		ListIterator li = curDomain.predicates.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (cur.isStatic()) {
				if (staticPreds.add(cur.toString())) {
					mssgs.add(
						cur.toString()
							+ " is defined as static but has no examples defined.");
				}
			}
		}
		return mssgs;
	}

	/** Closes the dialog */
	private void closeDialog(java.awt.event.WindowEvent evt) {
		if (dirty) {
			int res =
				JOptionPane.showConfirmDialog(
					AtomicInvarView.this,
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
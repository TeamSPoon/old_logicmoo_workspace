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
 * PredicateView
 * This is the OCL expert editor for predicates 
 * Part of the edexpt (Editor for Experts) Views
 * @author Ron Simpson
 * @version 0
 */

/**
 * History:
 * Ron 18/10/02 Allowed filtering by unifying sorts
 *              Changed checkbox for button to set static predicates
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

import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.edexpt.BaseTree.SortSelectionException;
import jplan.general.*;
import jplan.images.ImageLoader; /* Weihong changed on 5/9/2001 */
import java.beans.*;

// public class PredicateView extends JInternalFrame implements VetoableChangeListener {
public class PredicateView
	extends GipoInternalFrame
	implements PredChangeListener { /* Weihong added on 24/10/2001 */
	private OclEd top;
	private oclDomain curDomain;
	private List sorts;
	private oclPredicate dummyPred; // Used in empty pred tree
	protected List lstPreds; // This is the working list
	private int selPredIndex = -1; // The index of the currently edited
	//predicate in the list box
	private boolean dirty = false; // flag to monitor changes being committed

	// Interface components or their Models
	private SortOnlyTree sortTree;
	private JList jlstPreds;
	private DefaultListModel lmPreds;

	private JRadioButton jradSelected;
	private JRadioButton jradReferenced;
	private JRadioButton jradAll;
	private JButton cmdUpdateSel;
	private JButton cmdToggleStatic;
	private JButton cmdToggleFunctor; // Ron 25/06/03 needed for oclPlus
	private JCheckBox jchkUnify;

	private PredTree predTree;
	private JTextField jtxtName;
	private String lastPredName;

	/**
	 * Constructor
	 * @param curDomain the current active domain
	 * @param parent top level reference
	 */
	public PredicateView(oclDomain curDomain, OclEd parent) {
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
				"Cannot display predicates\n"
					+ (String) mssgs.get(0)
					+ "\nEdit and repair the sort definitions first.",
				EdStrings.strErrorHeading,
				JOptionPane.ERROR_MESSAGE);
			return;
		}
		if( parent.oclplus && curDomain.isOclPlus()) {
			setTitle("Predicate View (Durative Actions)");
		} else {
			setTitle("Predicate View (Expert)");
		}
		sorts = curDomain.sorts; //had reference to parent
		// Save COPY of the domain predicates
		ListIterator li = curDomain.predicates.listIterator();
		lstPreds = new ArrayList();
		try {
			while (li.hasNext()) {
				lstPreds.add(((oclPredicate) li.next()).clone());
			}
			this.curDomain = curDomain;
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln("Cannot clone predicates. " + e.toString());
		}
		// Ron 25/06/03 deal with oclPlus add functors to internal predicate list
		if (parent.oclplus  && curDomain.isOclPlus()) {
			li = curDomain.functors.listIterator();
			while (li.hasNext()) {
				Object cur = li.next();
				if (cur instanceof oclFunctor) {
					try {
						oclFunctor curF = (oclFunctor)((oclFunctor)cur).clone();
						lstPreds.add(curF);
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln("Cannot clone functor. " + e.toString());
					}
				} else if (cur instanceof oclPredicate) {
					try {
						oclPredicate curP = (oclPredicate)((oclPredicate)cur).clone();
						lstPreds.add(curP);
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln("Cannot clone predicate. " + e.toString());
					}
				}
			}
			   
		}
		top = parent;
		initComponents();
		dirty = false;
		lastPredName = new String("");
		//setSize(400,400);
		//show();
		pack();
		setVisible(true);
		try {
			SortTreeDragSource dragSource =
				new SortTreeDragSource(sortTree, DnDConstants.ACTION_COPY);
			// Cannot drop on this tree
			sortTree.setEditable(false);
		} catch (Exception e) {
			Utility.debugPrintln("Tree exception " + e.toString());
		}
	}

	//     public void vetoableChange (PropertyChangeEvent event)
	// 	throws PropertyVetoException {
	// Utility.debugPrintln("VETOABLECHANGE");
	// 	if (event.getPropertyName ().equals (IS_CLOSED_PROPERTY)) {

	// 	    boolean oldValue = ((Boolean) event.getOldValue ()).booleanValue ();
	// 	    boolean newValue = ((Boolean) event.getNewValue ()).booleanValue ();

	// 	    if (oldValue != newValue && newValue == true)
	//                 if (!canClose ())
	// 		    throw new PropertyVetoException ("Not allowed", event);
	// 	}
	//     }

	//     private boolean canClose(){
	// 	int res = JOptionPane.showConfirmDialog(PredicateView.this,
	// 			"Really!??.",
	// 			"GIPO Warning",
	// 			 JOptionPane.YES_NO_OPTION);
	// 	if (res == JOptionPane.YES_OPTION) {
	// 	    return true;   
	// 	}
	// 	return false;
	//     }

	/**
	 * initComponents
	 * Sets up the user interface
	 */
	private void initComponents() {
		int tWidth;

		addInternalFrameListener(new InternalFrameAdapter() {
			public void internalFrameClosing(InternalFrameEvent evt) {
				if (dirty) {
					int res =
						JOptionPane.showConfirmDialog(
							PredicateView.this,
							EdStrings.strNoCommit,
							EdStrings.strWarningHeading,
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

		Box boxTop = Box.createVerticalBox();
		Box boxEd = Box.createHorizontalBox(); // Holds the main panes
		/* Weihong added on 10/10/2001 */
		// 	Box boxFinalButtons = Box.createHorizontalBox();
		JPanel boxFinalButtons = new JPanel();
		boxFinalButtons.setLayout(new FlowLayout());
		/* end Weihong added on 10/10/2001 */

		// The first Vertical pane contains the Sort Tree
		Box boxSort = Box.createVerticalBox();

		//  Now put the sort components together
		sortTree = new SortOnlyTree(curDomain);
		ToolTipManager.sharedInstance().registerComponent(sortTree);
		DefaultTreeCellRenderer sortTreeRend =
			(DefaultTreeCellRenderer) sortTree.getCellRenderer();
		sortTreeRend.setToolTipText(
			"Drag and drop sorts to add predicate arguments.");

		JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
		JPanel jpanSorts = new JPanel();
		jpanSorts.setLayout(new BoxLayout(jpanSorts, BoxLayout.Y_AXIS));
		jpanSorts.setBorder(BorderFactory.createTitledBorder("Sorts .."));
		jpanSorts.add(scrollPaneSorts);

		boxSort.add(jpanSorts);

		// The Second Vertical pane contains the Predicate List
		// Now deal with Predicates - maintain a list
		// And A set of Filter Controls
		// A panel to add and delete predicates

		Box boxPreds = Box.createVerticalBox();

		lmPreds = new DefaultListModel();
		// Fill the model;
		ListIterator liPreds = lstPreds.listIterator();
		while (liPreds.hasNext()) {
			lmPreds.addElement(liPreds.next());
		}
		jlstPreds = new JList(lmPreds);
		jlstPreds.setToolTipText("Select predicate to edit.");
		jlstPreds.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListCellRenderer renderer =
			new PredCellRenderer(top.strImageDir, "static.png","fluent.png","sfluent.png");
		jlstPreds.setCellRenderer(renderer);
		jlstPreds.addListSelectionListener(predListSelListener);
		JScrollPane scrollPanePreds = new JScrollPane(jlstPreds);
		JPanel jpanPreds = new JPanel();
		jpanPreds.setBorder(BorderFactory.createTitledBorder("Predicates .."));
		jpanPreds.setLayout(new BoxLayout(jpanPreds, BoxLayout.Y_AXIS));
		jpanPreds.add(scrollPanePreds);

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
		boxFilter.add(jchkUnify);
		groupFilter.add(jradSelected);
		groupFilter.add(jradReferenced);
		groupFilter.add(jradAll);
		/* Weihong added on 4/07/2001 */
		// 	ImageIcon ii = new ImageIcon(top.strImageDir + "Refresh16.gif");
		/* Weihong changed on 4/9/2001 */
		ImageIcon ii =
			ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		cmdUpdateSel = new JButton("Refresh Selection", ii);
		cmdUpdateSel.setMnemonic(KeyEvent.VK_S);
		cmdUpdateSel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updatePredSelection();
			}
		});
		boxFilter.add(cmdUpdateSel);
		ii = ImageLoader.getImageIcon(top.strImageDir, "static.png");
		cmdToggleStatic = new JButton("Toggle",ii);
		cmdToggleStatic.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updateCheckSelection();
			}
		});
		boxFilter.add(cmdToggleStatic);
		if (top.oclplus  && curDomain.isOclPlus()) {
			ii = ImageLoader.getImageIcon(top.strImageDir, "fluent.png");
			cmdToggleFunctor = new JButton("Toggle",ii);
			cmdToggleFunctor.addActionListener(new java.awt.event.ActionListener() {
						public void actionPerformed(java.awt.event.ActionEvent evt) {
							updateFunctorCheckSelection();
						}
					});
			boxFilter.add(cmdToggleFunctor);
		}
		jpanFilter.add(boxFilter);

		Box boxEdPreds = Box.createVerticalBox();
		/* Weihong added on 10/10/2001 */
		// 	Box boxEdPreds1 = Box.createHorizontalBox();
		JPanel boxEdPreds1 = new JPanel();
		boxEdPreds1.setLayout(new FlowLayout());
		/* end Weihong added on 10/10/2001 */

		Box boxEdPreds2 = Box.createHorizontalBox();

		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "New16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "New16.gif");
		JButton cmdAddPred = new JButton("New", ii);
		cmdAddPred.setToolTipText(
			"Define name then Drag and drop sorts on the predicate tree to define arguments.");
		cmdAddPred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				addNewPred();
			}
		});
		cmdAddPred.setAlignmentY(0.5f);
		JLabel jlblName = new JLabel("  Name  ");
		jlblName.setAlignmentY(0.5f); /* Weihong added on 4/07/2001 */
		jtxtName = new JTextField(14);
		jtxtName.setAlignmentY(0.5f); /* Weihong added on 4/07/2001 */
		jtxtName.setDocument(new OCLIdentifierDocument());
		tWidth = jtxtName.getPreferredSize().width;
		jtxtName.setMaximumSize(new Dimension(tWidth, txtFieldMaxHeight));
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "Cut16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
		JButton cmdDelPred = new JButton("Delete", ii);
		cmdDelPred.setToolTipText("Delete the selected predicate.");
		cmdDelPred.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				delSelectedPred();
			}
		});
		cmdDelPred.setAlignmentY(0.5f);
		boxEdPreds1.add(cmdAddPred);
		boxEdPreds1.add(jlblName);
		boxEdPreds1.add(jtxtName);
		boxEdPreds1.add(cmdDelPred);

		boxEdPreds.add(boxEdPreds1);

		boxPreds.add(jpanPreds);
		boxPreds.add(jpanFilter);
		boxPreds.add(boxEdPreds);

		// Predicate Editing tree

		dummyPred = new oclPredicate("none-selected");
		dummyPred.addConstArgument("empty");
		predTree = new PredTree(dummyPred, this, -1);
		ToolTipManager.sharedInstance().registerComponent(predTree);
		DefaultTreeCellRenderer treeRend =
			(DefaultTreeCellRenderer) predTree.getCellRenderer();
		treeRend.setToolTipText("Drag and drop sorts to define arguments.");
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
			jlstPreds.setSelectedIndex(0);
		JPanel jpanEdPred = new JPanel();
		jpanEdPred.setLayout(new BoxLayout(jpanEdPred, BoxLayout.Y_AXIS));
		jpanEdPred.setBorder(
			BorderFactory.createTitledBorder("Edit Predicate .."));
		jpanEdPred.add(scrollPaneEdPred);
		// Create The Final Buttons
		/* Weihong added on 4/07/2001 */
		// 	ii = new ImageIcon(top.strImageDir + "SendMail16.gif");
		/* Weihong changed on 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		JButton cmdCommit = new JButton("Commit", ii);
		cmdCommit.setMnemonic(KeyEvent.VK_M);
		cmdCommit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Ron 25/06/03 - use commit changes rather than code inline
				commitChanges();
				/* Weihong added on 18/07/2001 */
				top.updateWindow(getTitle());
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
				List mssgs = checkPredicates();
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
						PredicateView.this,
						msg,
						EdStrings.strErrorHeading,
						JOptionPane.ERROR_MESSAGE,
						null);
				} else {
					JOptionPane.showMessageDialog(
						PredicateView.this,
						EdStrings.strVerifyOK,
						EdStrings.strInfoHeading,
						JOptionPane.INFORMATION_MESSAGE,
						null);
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
				// Save COPY of the domain predicates
				ListIterator li = curDomain.predicates.listIterator();
				lstPreds = new ArrayList();
				try {
					while (li.hasNext()) {
						lstPreds.add(((oclPredicate) li.next()).clone());
					}
				} catch (CloneNotSupportedException e) {
					Utility.debugPrintln(
						"Cannot clone predicates. " + e.toString());
				}
				if (curDomain.isOclPlus()) {
					li = curDomain.functors.listIterator();
					try {
						while (li.hasNext()) {
							lstPreds.add(((oclPredicate) li.next()).clone());
						}
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln(
							"Cannot clone functor. " + e.toString());
					}
				}
				lmPreds = new DefaultListModel();
				// Fill the model;
				li = lstPreds.listIterator();
				while (li.hasNext()) {
					lmPreds.addElement(li.next());
				}
				jlstPreds.setModel(lmPreds);
				dirty = false;
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
							PredicateView.this,
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

		boxEd.add(boxSort);
		boxEd.add(boxPreds);
		boxEd.add(jpanEdPred);

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
				curDomain.getPredicatesBySortFromList(
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
	 * updateCheckSelection
	 * change the status of selected predicate to
	 * static / dynamic
	 */
	private void updateCheckSelection() {
		if (jlstPreds.isSelectionEmpty()) {
			JOptionPane.showMessageDialog(
				this,
				"You must First select a predicate.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		oclPredicate pred = (oclPredicate) jlstPreds.getSelectedValue();
		int selPredIndex = jlstPreds.getSelectedIndex();
		if (pred.isStatic()) {
			pred.setStatic(false);
		} else {
			pred.setStatic(true);
		}
		lmPreds.remove(selPredIndex);
		lmPreds.add(selPredIndex, pred);

	}
	
	/**
	 * updateFunctorCheckSelection
	 * change the status of selected predicate to
	 * static / dynamic
	 */
	private void updateFunctorCheckSelection() {
		if (jlstPreds.isSelectionEmpty()) {
			JOptionPane.showMessageDialog(
				this,
				"You must First select a predicate.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		oclPredicate pred = (oclPredicate) jlstPreds.getSelectedValue();
		int selPredIndex = jlstPreds.getSelectedIndex();
		if (pred.isFluent()) {
			pred.setFluent(false);
		} else {
			pred.setFluent(true);
		}
		lmPreds.remove(selPredIndex);
		lmPreds.add(selPredIndex, pred);

	}

	/**
	 * ListSelectionListener
	 * Updates editing boxed when a predicate is choosen
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
//						jchkIsStatic.setSelected(pred.isStatic());
					}
				}
			} catch (Exception e) {
				Utility.debugPrintln("SElection exception " + e.toString());
				// Should not happen!!
			}
		}
	};

	/**
	 * add a new predicate to the domain
	 */
	public void addNewPred() {
		if (jtxtName.getText().equals("")) {
			JOptionPane.showMessageDialog(
				top,
				"Enter a predicate name.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		} else if (lastPredName.equals(jtxtName.getText())) {
			JOptionPane.showMessageDialog(
				top,
				"Duplicate predicate name please edit.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		lastPredName = jtxtName.getText();
		oclPredicate cur = new oclPredicate(jtxtName.getText());

		//predTree.setTreePred(cur,selPredIndex);
		lstPreds.add(cur);
		selPredIndex = jlstPreds.getModel().getSize();
		lmPreds.addElement(cur);
		jlstPreds.setSelectedIndex(selPredIndex);
		dirty = true;

	}

	/**
	 * updatePredicateAt
	 * change the predicate at specified index position
	 * @param pred the oclPredicate
	 * @param index - the index position
	 */
	public void updatePredicateAt(oclPredicate pred, int index) {
		oclPredicate old = (oclPredicate) lmPreds.getElementAt(index);
		// Now find it in the predicate list and change
		ListIterator li = lstPreds.listIterator();
		int ix = 0;
		while (li.hasNext()) {
			// == test for references should be good enough
			if (li.next() == old) {
				Utility.debugPrintln("Found it");
				lstPreds.set(ix, pred);
				break;
			}
			ix++;
		}
		dirty = true;
		lmPreds.setElementAt(pred, index);
	}

	/**
	 * delSelectedPred - delete the currently selected predicate
	 * from the list
	 */
	public void delSelectedPred() {
		int inx = jlstPreds.getSelectedIndex();
		if (inx == -1) {
			JOptionPane.showMessageDialog(
				top,
				"No Predicate Currently Selected.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		oclPredicate old = (oclPredicate) jlstPreds.getSelectedValue();
		// Now get rid of it from the list of predicates
		ListIterator li = lstPreds.listIterator();
		int ix = 0;
		while (li.hasNext()) {
			// == test for references should be good enough
			if (li.next() == old) {
				Utility.debugPrintln("Found it");
				lstPreds.remove(ix);
				break;
			}
			ix++;
		}
		// Now get rid from the JList
		DefaultListModel lmPreds = (DefaultListModel) jlstPreds.getModel();
		lmPreds.removeElementAt(inx);
		// Reset the predicate tree
		predTree.setTreePred(dummyPred, -1);
		selPredIndex = -1;
		dirty = true;
	}

	/** Closes the dialog */
	private void closeDialog(java.awt.event.WindowEvent evt) {
		if (dirty) {
			int res =
				JOptionPane.showConfirmDialog(
					PredicateView.this,
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

	private void commitChanges() {
		ArrayList newPreds = new ArrayList();
		ArrayList newFluents = new ArrayList();
		try {
			ListIterator li = lstPreds.listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate)((oclPredicate) li.next()).clone();
				if (cur.isFluent()) {
					newFluents.add(cur);
				} else {
					newPreds.add(cur);
				}
			}
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln("Unexpected clone failure.");
			return;
		}
		curDomain.predicates = newPreds;
		if (curDomain.isOclPlus())
			curDomain.functors = newFluents;
		dirty = false;
	}

	/**
	 * checkPredicates
	 * perform semantic checks on predicates
	 * 1. check that all predicate names are unique
	 * 2. check that all sort names are recorded in the sort tree
	 * @return List of (Strings) error messages - empty if all checks passed
	 */
	public List checkPredicates() {
		List mssgs = new ArrayList();
		List functors = new ArrayList();
		List badArgs = new ArrayList();

		ListIterator li = lstPreds.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (curDomain.listContainsString(cur.getName(), functors)) {
				mssgs.add(
					"There are duplicate entries for the predicate name "
						+ cur.getName());
			} else {
				functors.add(cur.getName());
			}
			ListIterator liArgs = cur.getArguments().listIterator();
			while (liArgs.hasNext()) {
				String arg = ((OPredicate.pArg) liArgs.next()).name;
				if (!curDomain.isSort(arg)) {
					if (!curDomain.listContainsString(arg, badArgs)) {
						mssgs.add(
							"Predicate argument "
								+ arg
								+ " of predicate "
								+ cur.getName()
								+ " is not a defined sort");
						badArgs.add(arg);
					}
				}
			}
		}
		return mssgs;
	}

}

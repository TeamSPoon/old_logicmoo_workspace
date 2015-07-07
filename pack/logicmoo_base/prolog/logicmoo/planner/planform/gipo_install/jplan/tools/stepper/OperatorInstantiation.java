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

package jplan.tools.stepper;

/**
 * OperatorInstantiation.java
 * Weihong Zhao
 * 05/03/2001
 */

import jplan.ocl.*;
import java.io.*;

import java.awt.event.*;
import jplan.general.UnderlineHighlighter;
import javax.swing.text.Highlighter;
import javax.swing.plaf.basic.BasicComboPopup;
import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;
import jplan.general.OPredicate;
import jplan.images.ImageLoader; /* Weihong changed on 5/9/2001 */
import jplan.general.Utility;
import jplan.graphics.*; /* WZ 20/5/02 */
// Ron adding state and atomic invariants viewing and filtering 4/11/02
import jplan.general.FilterStatePane;
import jplan.general.FilterStaticsPane;

/**
 * to show the operators for instantiation with mouse clicking.
 */
public class OperatorInstantiation extends javax.swing.JDialog {

	// Variables declaration
	private JGraphCanvas parent;
	private HStepperCanvas hsParent; //Ron 6/11/02
	private javax.swing.JToolBar northToolBar;
	private javax.swing.JButton jbn_OK;
	private javax.swing.JButton jbn_Cancel;
	private javax.swing.JTextField briefArea =
		new javax.swing.JTextField("briefArea");
	private javax.swing.JTextArea detailedArea =
		new javax.swing.JTextArea("detailedArea");
	private javax.swing.JButton downArrow;
	private javax.swing.JButton upArrow;
	// Filtering viewing curstate etc 4/11/02
	private FilterStatePane flwCurState; //This displays the current state
	private FilterStaticsPane flStatics; // This displays the atomic invars
	private boolean viewState = true; // Boolean to control toggle between 
	private JRadioButton jradAtomic;
	private JRadioButton jradState;

	private oclDomain curDom;
	private oclOperator operator;
	private oclPredicate briefAreaPred;
	private int ArgNo = -1;
	//the index of the sort/varible in a predicate - briefAreaPred
	private UnderlineHighlighter highlighter = null;
	private BasicComboPopup popupMenu;
	private String objname = null;
	private List objectList = new ArrayList();
	//the whole object list from the curDomain
	private OPredicate.pArg selectedPArg;
	private String curPath;
	JComboBox combo;

	/** 
	 * Creates an instance of the OperatorInstantiation dialog window
	 * @param curDom oclDomain
	 * @param operator the operator which content requires to display
	 * @param parent parent frame
	 * @param curPath the path to get images from the image store
	 */
	public OperatorInstantiation(
		oclDomain curDom,
		oclOperator operator,
		StepperCanvas parent,
		String curPath) {
		/* Weihong changed on 10/10/2001 */
		super(parent.parent.getTheParent(), "Operator Instantiation Window");
		/* Weihong changed on 1/11/01 */
		setModal(true);
		/* end Weihong changed on 10/10/2001 */
		this.parent = parent;
		this.curDom = curDom;
		this.operator = operator;
		this.objectList = curDom.objects; //lists of oclObject
		this.curPath = curPath;
		Utility.debugPrintln("Constructor 0");
		initComponents();
		pack();
		setSize(400, 100);
	}

	/** 
	 * Creates an instance of the OperatorInstantiation dialog window
	 * @param curDom oclDomain
	 * @param operator the operator which content requires to display
	 * @param parent parent frame
	 * @param curPath the path to get images from the image store
	 */
	public OperatorInstantiation(
		HStepperCanvas hsParent,
		oclDomain curDom,
		oclOperator operator,
		JFrame parent,
		String curPath) {
		super(parent, "Operator Instantiation Window");
		setModal(true);
		this.curDom = curDom;
		this.operator = operator;
		this.objectList = curDom.objects; //lists of oclObject
		this.curPath = curPath;
		this.hsParent = hsParent;
		Utility.debugPrintln("Constructor 1");
		initComponents();
		pack();
		setSize(400, 400);
	}

	/* WZ 20/5/02 */
	/** 
	 * Creates an instance of the OperatorInstantiation dialog window
	 * @param curDom oclDomain
	 * @param operator the operator which content requires to display
	 * @param parent parent frame HStepperCanvas
	 * @param curPath the path to get images from the image store
	 */
	public OperatorInstantiation(
		oclDomain curDom,
		oclOperator operator,
		HStepperCanvas parent,
		String curPath) {
		super(
			parent.getTheParent().getTheParent(),
			"Operator Instantiation Window");
		setModal(true);
		this.parent = parent;
		hsParent = parent; // Ron 6/11/02
		this.curDom = curDom;
		this.operator = operator;
		this.objectList = curDom.objects; //lists of oclObject
		this.curPath = curPath;
		Utility.debugPrintln("Constructor 2");
		initComponents();
		pack();
		setSize(400, 400);
	}

	/** 
	 * Initialisation
	 * 
	 */
	private void initComponents() {
		setBackground(new java.awt.Color(114, 159, 255));
		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt) {
				closeDialog();
			}
		});
		getContentPane().setLayout(new java.awt.BorderLayout());

		briefArea.setEditable(false);
		briefArea.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16));
		briefAreaPred = (oclPredicate) curDom.createOperatorSignature(operator);
		briefArea.setText(briefAreaPred.toString());
		//Ron 29/8/01 make it all visible
		briefArea.setScrollOffset(0);
		highlighter = new jplan.general.UnderlineHighlighter(null);
		briefArea.setHighlighter(highlighter);
		getContentPane().add(briefArea, "North");
		briefArea.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent me) {
				if (me.getModifiers() == MouseEvent.BUTTON3_MASK) {
					java.awt.Point viewPoint = me.getPoint();
					int clickPos = briefArea.viewToModel(viewPoint);
					if (briefAreaPred != null) {
						try {
							String varString =
								briefAreaPred.elementAt(clickPos);
							ArgNo = briefAreaPred.elementNoAt(clickPos);
							selectedPArg =
								(OPredicate.pArg) briefAreaPred
									.pArgAt(clickPos)
									.clone();
							//highlighting
							removeHighlights();
							int start = briefAreaPred.startElementAt(clickPos);
							highlighter.addHighlight(
								start,
								start + varString.length(),
								new UnderlineHighlighter
									.UnderlineHighlightPainter(
									Color.red));
							briefArea.updateUI();

							String sortBranch =
								briefAreaPred.getNthElementSort(ArgNo);
							//wait for it's available	
							initPopupMenu(sortBranch);
							if (popupMenu.getComponentCount() > 0) {
								popupMenu.show(
									briefArea,
									me.getX(),
									briefArea.getHeight());
							} else {
								Utility.debugPrintln(
									"No object itmes in the popup menu.");
							}

						} catch (Exception e) {
							Utility.debugPrintln(e);
						}
					}
				}
			}
		});

		showOperatorDetail();
		detailedArea.setLineWrap(true);
		detailedArea.setEditable(false);
		detailedArea.setSize(500, 300); /* Weihong changed on 10/10/2001 */
		detailedArea.setFont(
			new java.awt.Font("Arial", java.awt.Font.PLAIN, 11));
		getContentPane().add(detailedArea, "Center");
		detailedArea.setVisible(false);
		detailedArea.setBorder(new javax.swing.border.BevelBorder(1));

		northToolBar = new javax.swing.JToolBar();
		northToolBar.setLayout(new java.awt.FlowLayout());
		northToolBar.setFloatable(false); /* Weihong added on 12/10/2001 */

		jbn_OK = new javax.swing.JButton();
		jbn_OK.setText("   OK   ");
		jbn_OK.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_OKActionPerformed();
			}
		});
		northToolBar.add(jbn_OK);

		jbn_Cancel = new javax.swing.JButton();
		jbn_Cancel.setText("  Cancel  ");
		jbn_Cancel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_CancelActionPerformed();
			}
		});
		northToolBar.add(jbn_Cancel);

		northToolBar.addSeparator();

		/* Weihong changed/added on 5/9/2001 */
		ImageIcon ii = ImageLoader.getImageIcon(curPath, "Down16.gif");
		downArrow = new javax.swing.JButton("", ii);
		downArrow.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				downArrow.setVisible(false);
				upArrow.setVisible(true);
				detailedArea.setVisible(true);
				pack();
			}
		});
		northToolBar.add(downArrow);
		downArrow.setVisible(true);
		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(curPath, "Up16.gif");
		upArrow = new javax.swing.JButton("", ii);
		upArrow.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				downArrow.setVisible(true);
				upArrow.setVisible(false);
				detailedArea.setVisible(false);
				pack();
			}
		});
		northToolBar.add(upArrow);
		upArrow.setVisible(false);
		// Ron 4/11/02 add ability to toggle to view and filter atomic invarients
		// This is the hierarchical version
		if (hsParent != null) {
			downArrow.setVisible(false);
			upArrow.setVisible(true);
			detailedArea.setVisible(true);
			jradAtomic = new JRadioButton("View Atomic Invariants", false);
			jradState = new JRadioButton("View Current State", true);
			ButtonGroup bgrView = new ButtonGroup();
			bgrView.add(jradState);
			bgrView.add(jradAtomic);
			jradState.addActionListener(new ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent evt) {
					Utility.debugPrintln("Fired jradState");
					if (!viewState) {
						getContentPane().remove(flStatics);
						getContentPane().add(flwCurState, "East");
						viewState = true;
						getContentPane().paintAll(
							getContentPane().getGraphics());

					}
					viewState = true;

				}
			});
			jradAtomic.addActionListener(new ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent evt) {
					Utility.debugPrintln("Fired jradAtomic");
					if (viewState) {
						getContentPane().remove(flwCurState);
						getContentPane().add(flStatics, "East");
						getContentPane().paintAll(
							getContentPane().getGraphics());
					}
					viewState = false;
				}
			});
			northToolBar.add(jradState);
			northToolBar.add(jradAtomic);
			// Ron adding filtering cur state atomic invars 4/11/02
			flwCurState = new FilterStatePane(curDom, "Current State");
			flwCurState.showStateProperty(hsParent.getCurState(), 1);
			getContentPane().add(flwCurState, "East");
			flStatics = new FilterStaticsPane(curDom, "Atomic Invariants");
			getContentPane().add(flwCurState, "East");
		}
		// Ron 9/4/03 moved following outside scope of if
		// Ordinary stepper needs it too
		getContentPane().add(northToolBar, "South");

	}

	/** 
	 * display operator details (OCLh language format) to a JTextArea
	 * 
	 */
	private void showOperatorDetail() {
		StringWriter opDetail = new StringWriter();
		operator.oclPrintComponent(new PrintWriter(opDetail), 0, false);
		detailedArea.setText(new String(opDetail.getBuffer()));
	}

	/**
	 * remove all underline highlights
	 * 
	 */
	private void removeHighlights() {
		Highlighter highlighter = briefArea.getHighlighter();
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

	/** 
	 * initiate the popup menu for the operator's instantiation by mouse selection 
	 * @param sort the parameter/variable of the operator which requires instantiation
	 * 
	 */
	public void initPopupMenu(String sort) {
		Vector items = new Vector();
		java.util.List objNames = curDom.getObjectsOfSubTypes(sort);
		// Process the objects
		if (objNames != null) {
			ListIterator liObj = objNames.listIterator();
			while (liObj.hasNext()) {
				objname = (String) liObj.next();
				items.addElement(objname);
			}
		}
		//build the PopupMenu
		combo = new JComboBox(items);
		combo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				try {
					objname = (String) (popupMenu.getList().getSelectedValue());
					//replace the old varibles with this objname
					briefAreaPred.replaceVariableNo(ArgNo, objname);
					briefArea.setText(briefAreaPred.toString());
					operator.replaceVariableName(selectedPArg, objname);
					showOperatorDetail();
					popupMenu.hide(); /* WZ 15/5/02 */
				} catch (Exception e) {
					Utility.debugPrintln(e);
				}
			}
		});
		popupMenu = new BasicComboPopup(combo);
	}

	/** 
	 * when ok button to be pressed, close this dialog window
	 * and show the graphics at parent's canvas.
	 * 
	 */
	private void jbn_OKActionPerformed() {
		closeDialog();
		if (parent == null)
			return;

		if (parent
			.getClass()
			.getName()
			.equals("jplan.tools.stepper.StepperCanvas")) {
			((StepperCanvas) parent).showAsAOperator(operator);
		} else if (
			parent.getClass().getName().equals(
				"jplan.tools.stepper.HStepperCanvas")) {
			((StepperCanvas) parent).showAsAOperator(operator);
		}
	}

	/** 
	 * when cancel button is pressed, close this dialog window
	 * 
	 */
	private void jbn_CancelActionPerformed() {
		operator = null; /* WZ 31/5/02 */
		closeDialog();
	}

	/* WZ 31/5/02 */
	/**
	 * factory method to create and display box
	 * @param curDom
	 * @param operator
	 * @param parent
	 * @param curPath
	 */
	public static oclOperator showInstantiation(
		HStepperCanvas hsParent,
		oclDomain curDom,
		oclOperator operator,
		JFrame parent,
		String curPath) {
		try {
			oclOperator op = (oclOperator) operator.clone();
			OperatorInstantiation pw =
				new OperatorInstantiation(
					hsParent,
					curDom,
					op,
					parent,
					curPath);
			pw.setLocation(
				(int) (0.5 * parent.getWidth()),
				(int) (0.5 * parent.getHeight()));
			pw.show();
			return pw.getInstOperator();
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln(e);
		}
		return null;
	}

	/* WZ 31/5/02 */
	/** 
	 * return the instantiated method
	 * @return the instantiated method
	 */
	public oclOperator getInstOperator() {
		return operator;
	}

	/** 
	 * close this dialog window
	 * 
	 */
	private void closeDialog() {
		setVisible(false);
		dispose();
	}

}

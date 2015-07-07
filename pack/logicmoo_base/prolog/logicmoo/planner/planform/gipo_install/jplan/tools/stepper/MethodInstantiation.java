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
 * MethodInstantiation.java
 * Weihong Zhao
 * 05/02/2002
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
import jplan.images.ImageLoader;
import jplan.general.Utility;
// Ron adding state and atomic invariants viewing and filtering 4/11/02
import jplan.general.FilterStatePane;
import jplan.general.FilterStaticsPane;

/**
 * to show the methods for instantiation with mouse clicking.
 */
public class MethodInstantiation extends javax.swing.JDialog {

	// Variables declaration
	private HStepperCanvas parent;
	private javax.swing.JToolBar northToolBar;
	private javax.swing.JButton jbn_OK;
	private javax.swing.JButton jbn_Cancel;
	private javax.swing.JTextField briefArea = new javax.swing.JTextField("");
	private javax.swing.JTextArea detailedArea = new javax.swing.JTextArea("");
	private javax.swing.JButton downArrow;
	private javax.swing.JButton upArrow;
	// Filtering viewing curstate etc 4/11/02
	private FilterStatePane flwCurState; //This displays the current state
    private FilterStaticsPane flStatics; // This displays the atomic invars
    private boolean viewState = true; // Boolean to control toggle between 
    private JRadioButton jradAtomic;
    private JRadioButton jradState;

	private oclDomain curDom;
	private oclMethod mdRef;

	private oclPredicate briefAreaPred;
	private int ArgNo = -1;
	//the index of the sort/varible in a predicate - briefAreaPred
	private UnderlineHighlighter highlighter = null;
	private BasicComboPopup popupMenu;
	private String objname = null;
	//     private List objectList = new ArrayList(); //the whole object list from the curDomain
	private OPredicate.pArg selectedPArg;
	private String curPath;

	/* WZ 24/5/02 added md reference selection panel */
	private JList jlstMD;
	private oclMethod curMethod;
	/* end 24/5/02 */
	/* WZ 7/6/02 */
	private boolean editable;

	/** 
	 * Creates an instance of the MethodInstantiation dialog window
	 * @param curDom oclDomain
	 * @param mdRef - method a reference method
	 * @param parent parent frame
	 * @param curPath the path to get images from the image store
	 */
	public MethodInstantiation(
		oclDomain curDom,
		oclMethod mdRef,
		HStepperCanvas parent,
		String curPath) {
		super(
			parent.getTheParent().getTheParent(),
			"Method Instantiation Window");
		setModal(true);
		this.parent = parent;
		this.curDom = curDom;
		this.mdRef = mdRef;
		this.curPath = curPath;
		editable = true; /* WZ 7/6/02 */
		initComponents();
		pack();
	}

	/* WZ 7/6/02 */
	/** 
	 * Creates dialog window to method property
	 * @param curDom oclDomain
	 * @param mdReal - method a reference method
	 * @param parent parent frame
	 */
	public MethodInstantiation(
		oclDomain curDom,
		oclMethod mdReal,
		HStepperCanvas parent) {
		super(parent.getTheParent().getTheParent(), "Method Property Window");
		setModal(true);
		this.parent = parent;
		this.curDom = curDom;
		this.mdRef = mdReal;
		editable = false;
		initComponents();
		pack();
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

		/* WZ 24/5/02 added md reference selection panel */
		//assemble the checkbox list
		//list all matching methods with same reference
		JPanel mdNamePanel = new JPanel();
		mdNamePanel.setLayout(new BorderLayout());

		if (editable) { /* WZ 7/6/02 */
			jlstMD = new JList();
			DefaultListModel lmMD = new DefaultListModel();
			jlstMD.setToolTipText(
				"select to instantiate; right click to view description");
			jlstMD.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

			oclPredicate refPrd = mdRef.getName();
			ListIterator li = curDom.methods.listIterator();
			while (li.hasNext()) {
				oclMethod tmpMD = (oclMethod) li.next();
				if (refPrd.isSameType(tmpMD.getName())) {
					try {
						oclMethod addMD = (oclMethod) tmpMD.clone();
						/* WZ 28/5/02 */
						addMD.instantiateWith(refPrd); /* WZ 28/5/02 */
						lmMD.addElement(addMD); /* WZ 28/5/02 */
					} catch (CloneNotSupportedException e) {
					}
				}
			}
			jlstMD.setModel(lmMD);
			if (lmMD.size() > 0) { /* WZ 14/6/02 */
				jlstMD.setSelectedIndex(0);
				curMethod = (oclMethod) jlstMD.getSelectedValue();
				populateMethod();
			}
			jlstMD.addMouseListener(new MouseAdapter() {
				public void mouseClicked(MouseEvent me) {
					curMethod = (oclMethod) jlstMD.getSelectedValue();
					if (me.getModifiers() == MouseEvent.BUTTON1_MASK) {
						populateMethod();
					}

					if (me.getModifiers() == MouseEvent.BUTTON3_MASK) {
						String dec = curMethod.getDescription();
						if (dec.trim().equals(new String()))
							dec = "No decription for this method.";
						JOptionPane.showMessageDialog(
							MethodInstantiation.this,
							dec,
							"METHOD - " + curMethod.getName().getName(),
							JOptionPane.PLAIN_MESSAGE);
					}
				}
			});
			jlstMD.setBorder(new javax.swing.border.BevelBorder(0));
			if (lmMD.size() > 1)
				mdNamePanel.add(jlstMD, "North");
			else if (lmMD.size() == 1) {
				jlstMD.setSelectedIndex(0);
				curMethod = (oclMethod) jlstMD.getSelectedValue();
				populateMethod();
			} else if (lmMD.size() == 0) {
				JOptionPane.showMessageDialog(
					this,
					"No matching methods have been defined.",
					"GIPO ERROR",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			/* end 24/5/02 */
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
								int start =
									briefAreaPred.startElementAt(clickPos);
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
								Utility.debugPrintln("ArgNo::::" + ArgNo);
								Utility.debugPrintln(
									"sortBranch::::" + sortBranch);
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
			// 	    detailedArea.setVisible(false);
		} else { /* WZ 7/6/02 */
			curMethod = mdRef;
		}

		//assemble instantiation panel
		briefArea.setEditable(false);
		briefArea.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16));
		populateMethod();

		briefArea.setScrollOffset(0);
		highlighter = new jplan.general.UnderlineHighlighter(null);
		briefArea.setHighlighter(highlighter);
		mdNamePanel.add(briefArea, "South");
		getContentPane().add(mdNamePanel, "North");

		detailedArea.setLineWrap(true);
		detailedArea.setSize(500, 300);
		detailedArea.setFont(
			new java.awt.Font("Arial", java.awt.Font.PLAIN, 11));
		getContentPane().add(detailedArea, "Center");
		detailedArea.setEditable(false);
		detailedArea.setBorder(new javax.swing.border.BevelBorder(0));

		northToolBar = new javax.swing.JToolBar();
		northToolBar.setLayout(new java.awt.FlowLayout());
		northToolBar.setFloatable(false);

		jbn_OK = new javax.swing.JButton();
		jbn_OK.setText("   OK   ");
		jbn_OK.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_OKActionPerformed();
			}
		});
		northToolBar.add(jbn_OK);
		if (editable) { /* WZ 7/6/02 */
			jbn_Cancel = new javax.swing.JButton();
			jbn_Cancel.setText("  Cancel  ");
			jbn_Cancel.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent evt) {
					jbn_CancelActionPerformed();
				}
			});
			northToolBar.add(jbn_Cancel);

			northToolBar.addSeparator();

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
			downArrow.setVisible(false);

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
		}
	// Ron 4/11/02 add ability to toggle to afview and filter atomic invarients
	jradAtomic = new JRadioButton("View Atomic Invariants",false);
	jradState = new JRadioButton("View Current State",true);
	ButtonGroup bgrView = new ButtonGroup();
	bgrView.add(jradState);
	bgrView.add(jradAtomic);
	jradState.addActionListener(new ActionListener() {
		public void actionPerformed(java.awt.event.ActionEvent evt){
			Utility.debugPrintln("Fired jradState");
			if (!viewState) {
				getContentPane().remove(flStatics);
				getContentPane().add(flwCurState,"East");
				viewState = true;
				getContentPane().paintAll(getContentPane().getGraphics());
				
			}
			viewState = true; 
			
		}
	});
	jradAtomic.addActionListener(new ActionListener() {
		public void actionPerformed(java.awt.event.ActionEvent evt){
			Utility.debugPrintln("Fired jradAtomic");
			if (viewState) {
				getContentPane().remove(flwCurState);
				getContentPane().add(flStatics,"East");
				getContentPane().paintAll(getContentPane().getGraphics());
			}
			viewState = false;
		}
	});
	northToolBar.add (jradState);
	northToolBar.add (jradAtomic);
		getContentPane().add(northToolBar, "South");
		// Ron adding filtering cur state atomic invars 4/11/02
		flwCurState = new FilterStatePane(curDom,"Current State");
	 	flwCurState.showStateProperty(parent.getCurState(), 1);
	 	mdNamePanel.add(flwCurState, "East");
		flStatics = new FilterStaticsPane(curDom,"Atomic Invariants");
		getContentPane().add(flwCurState, "East");
	}

	/* WZ 7/6/02 */
	/**
	 * To remove mouse listener
	 * 
	 */
	public void setEditable(boolean torf) {
		detailedArea.setVisible(true);
	}

	private void populateMethod() {
		if (curMethod != null) {
			briefAreaPred =
				(oclPredicate) curDom.createMethodSignature(curMethod);
			// 	    briefAreaPred = curMethod.getName();/* WZ 29/5/02 */
			briefArea.setText(briefAreaPred.toString());
			showMethodDetail();
			pack();
		}
	}

	/** 
	 * display method details (OCLh language format) to a JTextArea
	 * 
	 */
	private void showMethodDetail() {
		if (curMethod != null) {
			StringWriter opDetail = new StringWriter();
			curMethod.oclPrintComponent(new PrintWriter(opDetail), 0, false);
			detailedArea.setText(new String(opDetail.getBuffer()));
		}
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
	 * initiate the popup menu for the method's instantiation by mouse selection 
	 * @param sort the parameter/variable of the method which requires instantiation
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
		JComboBox combo = new JComboBox(items);
		combo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				try {
					objname = (String) (popupMenu.getList().getSelectedValue());
					//replace the old varibles with this objname
					briefAreaPred.replaceVariableNo(ArgNo, objname);
					briefArea.setText(briefAreaPred.toString());
					curMethod.replaceVariableName(selectedPArg, objname);
					showMethodDetail();
					popupMenu.hide(); /* WZ 15/5/02 */
					pack(); /* WZ 28/5/02 */
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
		// 	parent.showMethodRunningResult(method);
	}

	/** 
	 * when cancel button is pressed, close this dialog window
	 * 
	 */
	private void jbn_CancelActionPerformed() {
		curMethod = null; /* WZ 29/5/02 */
		closeDialog();
	}

	/**
	 * factory method to create and display box
	 * @param curDom
	 * @param method
	 * @param parent
	 * @param curPath
	 */
	public static oclMethod showInstantiation(
		oclDomain curDom,
		oclMethod method,
		HStepperCanvas parent,
		String curPath) {
		try {
			oclMethod md = (oclMethod) method.clone();
			MethodInstantiation pw =
				new MethodInstantiation(curDom, md, parent, curPath);
			pw.setLocation(
				(int) (0.5 * parent.getWidth()),
				(int) (0.5 * parent.getHeight()));
			/* WZ 27/5/02 */
			pw.show();
			return pw.getInstMethod();
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln(e);
			return null;
		}
	}

	/* 7/6/02 */
	/**
	 * factory method to create and display box
	 * @param curDom
	 * @param method
	 * @param parent
	 */
	public static void showProperty(
		oclDomain curDom,
		oclMethod method,
		HStepperCanvas parent) {
		MethodInstantiation pw =
			new MethodInstantiation(curDom, method, parent);
		pw.setLocation(
			(int) (0.5 * parent.getWidth()),
			(int) (0.5 * parent.getHeight()));
		/* WZ 27/5/02 */
		pw.show();
	}

	/* 24/5/02 */
	/** 
	 * return the instantiated method
	 * @return the instantiated method
	 */
	public oclMethod getInstMethod() {
		return curMethod;
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

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
 * PlannerDia.java
 * This class allows the user to select and control running of external
 * planning algorithms
 * Created on 31 January 2001, 20:29
 */

package jplan.top;

import java.awt.Point;
import javax.swing.*;
import java.util.StringTokenizer;
import jplan.general.Utility;

/** 
 *
 * @author  ron
 * @version 0
 */
public class PlannerDia extends JDialog {
	// Variables declaration 
	private Box boxSel;
	private JLabel jlblSel;
	private JComboBox jcmbSel;
	private Box boxAlgDesc;
	private Box boxDesc;
	private JLabel jlblDesc;
	private JScrollPane jScrollPane1;
	private JTextArea jtxtaDesc;
	private Box boxDesc2;
	private JCheckBox jchkCond;
	private JCheckBox jchkHier;
	private JCheckBox jchkHierSorts;
	private JPanel jPanButtons;
	private Box boxButtons;
	private JButton jbtnOK;
	private JButton jbtnCancel;
	// End component variables
	// The top level podium
	OclEd top;
	String strOCLPath;

	//Ron 15/9/01
	int callMethod;

	/** 
	 * Creates new form PlannerDia - This dialog allows for the selection
	 * of planning algorithms and setting up any necessary parameters.
	 */
	public PlannerDia(OclEd top) {
		super(top, true);
		initComponents();
		strOCLPath = new String(System.getProperty("ocled.path"));
		String algs = System.getProperty("ocled.planner.algs");
		StringTokenizer tokPlanners = new StringTokenizer(algs);
		while (tokPlanners.hasMoreTokens()) {
			/* WZ 4/7/02 */
			String algorithm = tokPlanners.nextToken();
			String str =
				new String(
					System.getProperty("ocled.hierarchical." + algorithm));
			// Ron 5/5/03
			if (top.curDomain.isHierarchical()) {
				if (str.equals("yes"))
					jcmbSel.addItem(algorithm);
			} else {
				//if (str.equals("no")) Ron 5/6/03 all HyHtn our onlu hierarchical planner to be used on flat plans
					jcmbSel.addItem(algorithm);
			}
			/* end 4/7/02 */
		}
		if (!"none".equals(top.plannerConfig.algName)) {
			jcmbSel.setSelectedItem(top.plannerConfig.algName);
			setPlannerDetails();
		} else {
			// Ron 15/9/01
			callMethod = RuntimeConfig.INTERNAL;
		}
		Point topPt = top.getLocationOnScreen();
		setLocation(topPt.x + 10, (topPt.y + top.getHeight()) / 2);
		this.top = top;
		pack();
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 */
	private void initComponents() {
		boxSel = Box.createHorizontalBox();
		jlblSel = new JLabel();
		jcmbSel = new JComboBox();
		boxAlgDesc = Box.createVerticalBox();
		boxDesc = Box.createHorizontalBox();
		jlblDesc = new JLabel();
		jScrollPane1 = new JScrollPane();
		jtxtaDesc = new JTextArea(5, 30);
		boxDesc2 = Box.createHorizontalBox();
		jchkCond = new JCheckBox();
		jchkHier = new JCheckBox();
		jchkHierSorts = new JCheckBox();

		jPanButtons = new JPanel();
		boxButtons = Box.createHorizontalBox();
		jbtnOK = new JButton();
		jbtnCancel = new JButton();
		getContentPane().setLayout(new BoxLayout(getContentPane(), 1));
		setTitle("Planning Algorithm Selector");
		// 	addWindowListener (new java.awt.event.WindowAdapter () {
		// 		public void windowClosing (java.awt.event.WindowEvent evt) {
		// 		    closeDialog (evt);
		// 		}
		// 	    }
		// 			   );
		// The Algorithm Selector Box
		jlblSel.setText("Select Planner");
		jlblSel.setLabelFor(jcmbSel);
		jcmbSel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				setPlannerDetails();
			}
		});
		getContentPane().add(Box.createVerticalStrut(10));

		boxSel.add(Box.createHorizontalStrut(10));
		boxSel.add(jlblSel);
		boxSel.add(Box.createHorizontalStrut(10));
		boxSel.add(jcmbSel);
		boxSel.add(Box.createHorizontalGlue());

		getContentPane().add(boxSel);
		getContentPane().add(Box.createVerticalStrut(10));
		// Algorithim Description Box
		jlblDesc.setText("Algorithm Description");
		jScrollPane1.setViewportView(jtxtaDesc);

		boxDesc.add(Box.createHorizontalStrut(10));
		boxDesc.add(jlblDesc);
		boxDesc.add(Box.createHorizontalStrut(10));
		boxDesc.add(jScrollPane1);

		boxAlgDesc.add(boxDesc);
		boxAlgDesc.add(Box.createVerticalStrut(10));
		jchkCond.setHorizontalTextPosition(SwingConstants.LEFT);
		jchkCond.setText("Supports Conditional Effects");
		jchkCond.setEnabled(false);
		jchkHier.setHorizontalTextPosition(SwingConstants.LEFT);
		jchkHier.setText("Supports Hierarchical Methods");
		jchkHier.setEnabled(false);
		jchkHierSorts.setHorizontalTextPosition(SwingConstants.LEFT);
		jchkHierSorts.setText("Supports Hierarchical Sorts");
		jchkHierSorts.setEnabled(false);
		boxDesc2.add(Box.createHorizontalStrut(10));
		boxDesc2.add(jchkCond);
		boxDesc2.add(Box.createHorizontalGlue());
		boxDesc2.add(jchkHier);
		boxDesc2.add(jchkHierSorts);
		boxDesc2.add(Box.createHorizontalStrut(10));

		boxAlgDesc.add(boxDesc2);

		getContentPane().add(boxAlgDesc);
		getContentPane().add(Box.createVerticalStrut(10));

		// Buttons
		jPanButtons.setBorder(new javax.swing.border.BevelBorder(0));
		jPanButtons.setLayout(new javax.swing.BoxLayout(jPanButtons, 0));
		jbtnOK.setText("OK");
		jbtnOK.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbtnOKActionPerformed(evt);
			}
		});

		jbtnCancel.setText("Cancel");
		jbtnCancel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbtnCancelActionPerformed(evt);
			}
		});
		//boxButtons.add(Box.createHorizontalGlue());
		boxButtons.add(jbtnOK);
		//boxButtons.add(Box.createHorizontalGlue());
		boxButtons.add(jbtnCancel);
		//boxButtons.add(Box.createHorizontalGlue());

		jPanButtons.add(boxButtons);
		getContentPane().add(jPanButtons);

	}

	private void jbtnCancelActionPerformed(java.awt.event.ActionEvent evt) {
		// Add your handling code here:
		setVisible(false);
		dispose();
	}

	/**
	 * store all the details in the OclEd's plannerConfig structure
	 */
	private void jbtnOKActionPerformed(java.awt.event.ActionEvent e) {
		top.plannerConfig.algName = (String) jcmbSel.getSelectedItem();
		top.plannerConfig.condEffects = jchkCond.isSelected();
		top.plannerConfig.hierMethods = jchkHier.isSelected();
		top.plannerConfig.hierSorts = jchkHierSorts.isSelected();
		top.plannerConfig.dirty = true;
		// Ron 15/9/01
		top.plannerConfig.callMethod = callMethod;
		if (callMethod == RuntimeConfig.SHELL)
			top.plannerConfig.command =
				System.getProperty(
					"ocled.command." + top.plannerConfig.algName);
		//top.getMessagePane().append("Planner configuration set.\n");    
		setVisible(false);
		dispose();
	}

	/** Closes the dialog */
	private void closeDialog(java.awt.event.WindowEvent evt) {
		setVisible(false);
		dispose();
	}

	/**
	 * setPlannerDetails
	 * update dialog box with the details of the Planning algorithm selected
	 * in the algorithm combo
	 * box
	 */
	private void setPlannerDetails() {
		String alg = (String) jcmbSel.getSelectedItem();
		String algsDesc = System.getProperty("ocled.description." + alg);
		String supCond = System.getProperty("ocled.condeffects." + alg);
		String supHier = System.getProperty("ocled.hierarchical." + alg);
		String callMethodstr = System.getProperty("ocled.callmethod." + alg);
		String supHierSorts =
			System.getProperty("ocled.hierarchicalsorts." + alg);
		if (callMethodstr != null && callMethodstr.equals("shell"))
			callMethod = RuntimeConfig.SHELL;
		else
			callMethod = RuntimeConfig.INTERNAL;
		if (supHierSorts != null && supHierSorts.equals("yes"))
			jchkHierSorts.setSelected(true);
		else
			jchkHierSorts.setSelected(false);
		if (supCond != null && supCond.equals("yes"))
			jchkCond.setSelected(true);
		else
			jchkCond.setSelected(false);
		if (supHier != null && supHier.equals("yes"))
			jchkHier.setSelected(true);
		else
			jchkHier.setSelected(false);
		jtxtaDesc.setText(algsDesc);
	}
}

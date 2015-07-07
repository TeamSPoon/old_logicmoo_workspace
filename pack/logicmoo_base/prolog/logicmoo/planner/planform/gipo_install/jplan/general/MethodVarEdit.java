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
package jplan.general;

/**
 * MethodVarEdit.java
 *
 *
 * Created: Mon Mar 11 10:34:28 2002
 *
 * @author W Zhao
 * @version
 */

import jplan.graphics.transition.*;
import jplan.ocl.*;
import jplan.graphics.gTool.Graphics.vShape;

public class MethodVarEdit extends javax.swing.JDialog {
	private HGoalVarEditPanel drawingCanvas; /* WZ 29/4/02 */
	private boolean embedded;
	private MethodHeadCanvas mdCanvas;

	public MethodVarEdit(
		javax.swing.JFrame parent,
		MethodHeadCanvas mdc,
		boolean embedded) {
		super(parent); /* WZ 25/4/02 */
		setModal(true); /* WZ 25/4/02 */
		this.embedded = embedded; /* WZ 29/4/02 */
		mdCanvas = mdc;
		setTitle("Variable Editing Window");
		initComponents();
		pack();
		setSize(300, 400);
		setLocation(
			getToolkit().getScreenSize().width / 2 - getBounds().width / 2,
			getToolkit().getScreenSize().height / 2 - getBounds().height / 2);
		setVisible(true);
	}

	/**
	 * Initialisation
	 * 
	 */
	protected void initComponents() {
		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt) {
				closeDialog();
			}
		});

		getContentPane().setLayout(new java.awt.BorderLayout());
		drawingCanvas = new HGoalVarEditPanel(this, mdCanvas); /* WZ 29/4/02 */
		getContentPane().add(drawingCanvas, "Center");

		javax.swing.JToolBar bottomPanel = new javax.swing.JToolBar();
		bottomPanel.setLayout(new java.awt.FlowLayout());
		bottomPanel.setFloatable(false);

		javax.swing.JButton okButton = new javax.swing.JButton(" OK ");
		okButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_OKActionPerformed();
			}
		});
		bottomPanel.add(okButton);

		javax.swing.JButton cancelButton =
			new javax.swing.JButton("  Cancel  ");
		cancelButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_CancelActionPerformed();
			}
		});
		bottomPanel.add(cancelButton);

		getContentPane().add(bottomPanel, "South");
	}

	/**
	 * return true is this window is called by a methodHeadCanvas,
	 * which is embedded in a vShape
	 * 
	 */
	public boolean isEmbedded() {
		return embedded;
	}

	/** 
	 * when ok button to be pressed, close this dialog window
	 * and show the graphics at parent's canvas.
	 * 
	 */
	private void jbn_OKActionPerformed() {
		drawingCanvas.updateVariable();
		closeDialog();
	}

	/** 
	 * when cancel button is pressed, close this dialog window
	 * 
	 */
	private void jbn_CancelActionPerformed() {
		closeDialog();
	}

	/** 
	 * close this dialog window
	 * 
	 */
	private void closeDialog() {
		setVisible(false);
		dispose();
	}

} // VarEditWindow

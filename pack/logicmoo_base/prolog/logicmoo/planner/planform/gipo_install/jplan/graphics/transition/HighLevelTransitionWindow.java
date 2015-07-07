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

package jplan.graphics.transition;

/** 
 * HighLevelTransitionWindow.java
 * @author Weihong Zhao
 * 6/11/2001
 */

import java.util.Hashtable;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Vector;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import javax.swing.undo.*;
import javax.swing.event.*;
import java.awt.print.*;
import java.beans.*;
import javax.swing.tree.*; /* WZ 25/3/02 */

import jplan.graphics.PrintPreview;
import jplan.graphics.gTool.Windows.vFilter;
import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.edexpt.BaseTree.SortSelectionException;
import jplan.edexpt.PredListCellRenderer;
import jplan.edexpt.PredCellRenderer;
import jplan.edexpt.SortOnlyTree; /* WZ 25/3/02 */
import jplan.general.*;
import jplan.general.ExpressionModel.ExpressionException;
import jplan.images.ImageLoader;
import jplan.graphics.*;
import jplan.general.GipoTab; /* Weihong 26/11/2001 */

/**
 * Extended from ActionWindow, this is a window to create/edit high level
 * transitions (method for the planning domain model.<br>
 * This is an interface between the internal domain high level transition representation
 * and the graphical display. This high level transition (method) can be decomposed to
 * include lower level transition (operator).<br>
 * Its function including loading/unloading existing domain information, 
 * e.g. states, method, creating/editing operators by simple mouse action.
 */
public class HighLevelTransitionWindow extends ActionWindow {
	/* Weihong 11/3/02 */
	/** 
	 * Create an dummy instance of HighLevelTransitionWindow with given domain.
	 */
	public HighLevelTransitionWindow() {
		super(null);
	}

	/* Weihong 11/3/02 */
	/** 
	 * Create an dummy instance of HighLevelTransitionWindow with given domain.
	 */
	public HighLevelTransitionWindow(OclEd parent) {
		super(parent);
	}

	/** 
	 * Create an instance of HighLevelTransitionWindow with given domain.
	 * @param curDomain oclDomain
	 * @param parent its parent frame
	 */
	public HighLevelTransitionWindow(oclDomain curDomain, OclEd parent) {
		super(parent);
		setTitle("Compound Transition Window");
		setClosable(false);
		this.curDomain = curDomain;
		top = parent;

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
		dirty = false; /* WZ 14/8/02 */
		tempFile = null;
		initComponents();
		pack();
		setVisible(true);
	}

	/**
	 * Update the methods' list from the domain information.
	 * 
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
	 * Initialisation
	 * 
	 */
	protected void initComponents() {

		getContentPane().setLayout(new java.awt.BorderLayout());

		jMenuBar2 = new javax.swing.JMenuBar();
		jM_File = new javax.swing.JMenu();
		jM_File.setText("File");

		ImageIcon ii = null;

		ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		jMI_Save = new javax.swing.JMenuItem("Commit", ii);
		jMI_Save.setToolTipText(
			"Save (update) the current operators list to the domain.");
		jMI_Save.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_SaveActionPerformed();
			}
		});
		jM_File.add(jMI_Save);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Remove16.gif");
		jMI_Restore = new javax.swing.JMenuItem("Restore", ii);
		jMI_Restore.setToolTipText(
			"Restore original states which before the last call of 'Commit Changes'.");
		jMI_Restore.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_RestoreActionPerformed();
			}
		});
		jM_File.add(jMI_Restore);

		jM_File.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
		jMI_PrintPreview = new javax.swing.JMenuItem("Print Preview", ii);
		jMI_PrintPreview
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintPreviewActionPerformed();
			}
		});
		jM_File.add(jMI_PrintPreview);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		jMI_Print = new javax.swing.JMenuItem("Print", ii);
		jMI_Print.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintActionPerformed();
			}
		});
		jM_File.add(jMI_Print);

		jM_File.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		jMI_Exit = new javax.swing.JMenuItem("Close", ii);
		jMI_Exit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_ExitActionPerformed();
			}
		});
		jM_File.add(jMI_Exit);

		jMenuBar2.add(jM_File);

		jM_Operator = new javax.swing.JMenu();
		jM_Operator.setText("Method");

		ii = ImageLoader.getImageIcon(top.strImageDir, "New16.gif");
		jMI_New = new javax.swing.JMenuItem("New", ii);
		jMI_New.setToolTipText("Create a new compound operator");
		jMI_New.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_NewActionPerformed();
			}
		});
		jM_Operator.add(jMI_New);

		jM_Operator.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
		jMI_Add = new javax.swing.JMenuItem("Add", ii);
		jMI_Add.setToolTipText("Add to the list");
		jMI_Add.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_AddActionPerformed();
			}
		});
		jM_Operator.add(jMI_Add);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		jMI_Update = new javax.swing.JMenuItem("Update", ii);
		jMI_Update.setToolTipText("Update Existing ...");
		jMI_Update.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_UpdateMethodActionPerformed();
			}
		});
		jM_Operator.add(jMI_Update);

		jM_Operator.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
		jMI_DeleteOperator =
			new javax.swing.JMenuItem("Delete Selected ...", ii);
		jMI_DeleteOperator
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_DeleteOperatorActionPerformed();
			}
		});
		jM_Operator.add(jMI_DeleteOperator);

		jMenuBar2.add(jM_Operator);

		jM_Edit = new javax.swing.JMenu();
		jM_Edit.setText("Design & Edit");

		jMI_DrawSquare =
			new javax.swing.JRadioButtonMenuItem("Add Precondition");
		jMI_DrawSquare.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_DrawSquareActionPerformed();
			}
		});

		jMI_DrawRoundSquare =
			new javax.swing.JRadioButtonMenuItem(
				"Add Index (Necessary Object Transitions)");
		jMI_DrawRoundSquare
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_DrawRoundSquareActionPerformed();
			}
		});

		jMI_DrawShape5 =
			new javax.swing.JRadioButtonMenuItem("Add Static Predicates");
		jMI_DrawShape5.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_DrawShape5ActionPerformed();
			}
		});

		jM_Edit.add(jMI_DrawSquare);
		jM_Edit.add(jMI_DrawRoundSquare);
		jM_Edit.add(jMI_DrawShape5);

		jM_Edit.addSeparator();

		jMI_Select = new javax.swing.JRadioButtonMenuItem("Select ...");
		jMI_Select.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_SelectActionPerformed();
			}
		});
		jM_Edit.add(jMI_Select);

		jM_Edit.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
		jMI_DeleteShapeLink =
			new javax.swing.JMenuItem("Delete shape/link", ii);
		jMI_DeleteShapeLink
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_DeleteShapeLinkActionPerformed();
			}
		});
		jM_Edit.add(jMI_DeleteShapeLink);
		jM_Edit.addSeparator();

		//add radio buttons to the buttongroup
		buttonGroup1.add(jMI_DrawSquare);
		buttonGroup1.add(jMI_DrawRoundSquare);
		buttonGroup1.add(jMI_DrawShape5);
		buttonGroup1.add(jMI_Select);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Undo16.gif");
		jMI_Undo = new javax.swing.JMenuItem("Undo", ii);
		jMI_Undo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_UndoActionPerformed();
			}
		});
		jM_Edit.add(jMI_Undo);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Redo16.gif");
		jMI_Redo = new javax.swing.JMenuItem("Redo", ii);
		jMI_Redo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_RedoActionPerformed();
			}
		});
		jM_Edit.add(jMI_Redo);

		/* WZ 16/4/02 */
		jM_Edit.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "Edit16.gif");
		javax.swing.JMenuItem jMI_EditVar =
			new javax.swing.JMenuItem("Edit Variables", ii);
		jMI_EditVar.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_EditVarActionPerformed();
			}
		});
		jM_Edit.add(jMI_EditVar);
		/* WZ 16/4/02 end */

		jMenuBar2.add(jM_Edit);

		jM_View = new javax.swing.JMenu("View");

		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		jMI_ZoomIn = new javax.swing.JMenuItem("Zoom In", ii);
		jMI_ZoomIn.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_ZoomInActionPerformed();
			}
		});
		jM_View.add(jMI_ZoomIn);

		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomOut16.gif");
		jMI_ZoomOut = new javax.swing.JMenuItem("Zoom Out", ii);
		jMI_ZoomOut.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_ZoomOutActionPerformed();
			}
		});
		jM_View.add(jMI_ZoomOut);
		jM_View.addSeparator();

		jMI_FileToolbar = new javax.swing.JCheckBoxMenuItem("File Toolbar");
		jMI_FileToolbar.setState(true);
		jMI_FileToolbar.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				if (jMI_FileToolbar.getState())
					topToolBarPanel.add(jToolBar1);
				else
					topToolBarPanel.remove(jToolBar1);
				updateUI();
			}
		});
		jM_View.add(jMI_FileToolbar);

		jMI_OperatorToolBar =
			new javax.swing.JCheckBoxMenuItem("Operator Toolbar");
		jMI_OperatorToolBar.setState(true);
		jMI_OperatorToolBar
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				if (jMI_OperatorToolBar.getState())
					topToolBarPanel.add(jToolBar3);
				else
					topToolBarPanel.remove(jToolBar3);
				updateUI();
			}
		});
		jM_View.add(jMI_OperatorToolBar);

		jMI_EditingToolbar =
			new javax.swing.JCheckBoxMenuItem("Editing Toolbar");
		jMI_EditingToolbar.setState(true);
		jMI_EditingToolbar
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jToolBar2.setVisible(jMI_EditingToolbar.getState());
				updateUI();
			}
		});
		jM_View.add(jMI_EditingToolbar);

		jMenuBar2.add(jM_View);

		setJMenuBar(jMenuBar2);

		/* 
		   add Toolbars
		*/

		jToolBar1 = new JToolBar();
		jToolBar1.setFloatable(false);
		JButton bt = null;

		ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		bt = new GipoButton(" Commit ", ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_SaveActionPerformed();
			}
		});
		jToolBar1.add(bt);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Remove16.gif");
		bt = new GipoButton(" Restore ", ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_RestoreActionPerformed();
			}
		});
		jToolBar1.add(bt);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
		bt = new GipoButton("Verify", ii);
		bt.setMnemonic(KeyEvent.VK_V);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				verifyActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* WZ 21/8/02 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
		bt = new GipoButton("Verify current", ii);
		bt.setToolTipText("Verify current selected method.");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				verifyCurrentActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
		bt = new GipoButton(" Preview ", ii);
		/* Weihong changed on 5/12/2001 */
		bt.setToolTipText("Prin Preview");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintPreviewActionPerformed();
			}
		});
		jToolBar1.add(bt);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		bt = new GipoButton(" Print ", ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		bt = new GipoButton(" Close ", ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ExitActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		jToolBar3 = new JToolBar();
		jToolBar3.setFloatable(false);

		ii = ImageLoader.getImageIcon(top.strImageDir, "New16.gif");
		bt = new GipoButton(" New ", ii);
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_NewActionPerformed();
			}
		});
		jToolBar3.add(bt);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
		bt = new GipoButton(" Add ", ii);
		bt.setAlignmentY(0.5f);
		bt.setToolTipText("Add to the list");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_AddActionPerformed();
			}
		});
		jToolBar3.add(bt);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		bt = new GipoButton(" Update ", ii);
		bt.setAlignmentY(0.5f);
		bt.setToolTipText("Update Existing ...");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_UpdateMethodActionPerformed();
			}
		});
		jToolBar3.add(bt);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
		bt = new GipoButton(" Delete ", ii);
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_DeleteOperatorActionPerformed();
			}
		});
		jToolBar3.add(bt);

		jToolBar3.addSeparator();

		/* WZ 29/4/02 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Edit16.gif");
		bt = new GipoButton(" Edit ", ii);
		bt.setToolTipText("Edit Variables");
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_EditVarActionPerformed();
			}
		});
		jToolBar3.add(bt);

		jToolBar3.addSeparator();
		/* WZ 29/4/02 end */

		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		bt = new GipoButton(" Zoom In ", ii);
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ZoomInActionPerformed();
			}
		});
		jToolBar3.add(bt);

		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomOut16.gif");
		bt = new GipoButton(" Zoom Out ", ii);
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ZoomOutActionPerformed();
			}
		});
		jToolBar3.add(bt);

		jToolBar3.add(new JLabel("              "));

		editMode = new JCheckBox("Edit Mode");
		editMode.setAlignmentY(0.5f);
		editMode.setToolTipText("To enable editing varibles");
		editMode.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_editModeActionPerformed();
			}
		});
		jToolBar3.add(editMode);

		Font font_jToolBar2 = new Font("Arial", Font.PLAIN, 12);
		jToolBar2 = new JToolBar();
		jToolBar2.setFloatable(false);
		jToolBar2.setLayout(new GridLayout(1, 0));

		JRadioButton rbt = new JRadioButton("Pre-condition");
		rbt.setToolTipText("Add Pre-conditions");
		rbt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_DrawSquareActionPerformed();
				jMI_DrawSquare.setSelected(true);
			}
		});
		rbt.setAlignmentY(0.5f);
		rbt.setMargin(new Insets(3, 5, 3, 5));
		rbt.setFont(font_jToolBar2);
		rbt.setFont(font_jToolBar2);
		jToolBar2.add(rbt);
		buttonGroup2.add(rbt);

		rbt = new JRadioButton("Index");
		rbt.setToolTipText("Add Index");
		rbt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_DrawRoundSquareActionPerformed();
				jMI_DrawRoundSquare.setSelected(true);
			}
		});
		rbt.setAlignmentY(0.5f);
		rbt.setMargin(new Insets(3, 5, 3, 5));
		rbt.setFont(font_jToolBar2);
		jToolBar2.add(rbt);
		buttonGroup2.add(rbt);

		rbt = new JRadioButton("Static");
		rbt.setToolTipText("Add Static Predicates");
		rbt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_DrawShape5ActionPerformed();
				jMI_DrawShape5.setSelected(true);
			}
		});
		rbt.setAlignmentY(0.5f);
		rbt.setMargin(new Insets(3, 5, 3, 5));
		rbt.setFont(font_jToolBar2);
		jToolBar2.add(rbt);
		buttonGroup2.add(rbt);

		rbt_select = new JRadioButton("Select");
		rbt_select.setToolTipText("Select a Shape(s)/Link(s)");
		rbt_select.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setSelectionButton(true); /* Weihong 15/11/2001 */
			}
		});
		rbt_select.setAlignmentY(0.5f);
		rbt_select.setMargin(new Insets(3, 5, 3, 5));
		rbt_select.setFont(font_jToolBar2);
		jToolBar2.add(rbt_select);
		buttonGroup2.add(rbt_select);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
		JButton bt_Delete = new JButton("Delete", ii);
		bt_Delete.setToolTipText("Delete shape/link");
		bt_Delete.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_DeleteShapeLinkActionPerformed();
			}
		});
		jToolBar2.add(bt_Delete);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Undo16.gif");
		JButton bt_Undo = new JButton("Undo", ii);
		bt_Undo.setToolTipText("Undo");
		bt_Undo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_UndoActionPerformed();
			}
		});
		jToolBar2.add(bt_Undo);

		ii = ImageLoader.getImageIcon(top.strImageDir, "Redo16.gif");
		JButton bt_Redo = new JButton("Redo", ii);
		bt_Redo.setToolTipText("Redo");
		bt_Redo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_RedoActionPerformed();
			}
		});
		jToolBar2.add(bt_Redo);

		topToolBarPanel = new JPanel();
		topToolBarPanel.setLayout(new GridLayout(1, 0));
		/* Weihong changed on 5/12/2001 */
		topToolBarPanel.add(jToolBar1);
		topToolBarPanel.add(jToolBar3);
		getContentPane().add(topToolBarPanel, "North");
		getContentPane().add(jToolBar2, "South");

		//add splitpane2 (will contain drawingCanvas and jscollOM)
		splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane2.setResizeWeight(0.8);
		splitPane2.setOneTouchExpandable(true); /* WZ 21/8/02 */
		splitPane2.setDividerSize(6); /* WZ 13/5/02 */

		/* add drawing Canvas*/
		addDrawingCanvas();

		/* WZ 25/3/02 */
		//build sort tree
		JScrollPane treePane = addSortTree();

		// Build the SubStateClassList
		addStateList();

		//assembly west panel
		westPanel.setLayout(new GridLayout(0, 1));
		westPanel.add(treePane); /* WZ 25/3/02 */
		westPanel.add(jscrollStateDefs);

		//add static predicate list
		ListIterator liPreds = curDomain.predicates.listIterator();
		oclPredicate opde;
		while (liPreds.hasNext()) {
			opde = (oclPredicate) liPreds.next();
			if (opde.isStatic())
				lmPreds.addElement(opde);
		}
		jlstPreds.setModel(lmPreds);
		jlstPreds.setSelectionMode(
			ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		jlstPreds.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent lse) {
				JList jlist = (JList) lse.getSource();
				List staticList = new ArrayList();
				oclPredicate opd;
				StringBuffer sb = new StringBuffer();
				drawingCanvas.predicateList.clear();
				for (int i = 0;
					i
						< java.lang.reflect.Array.getLength(
							jlist.getSelectedValues());
					i++) {
					try {
						opd =
							(oclPredicate) ((oclPredicate) jlist
								.getSelectedValues()[i])
								.clone();
						opd.toVar(); /* Weihong 3/12/2001 */
						staticList.add(opd);
						sb.append(opd.toString());
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln(e);
						return;
					}
				}
				drawingCanvas.setStaticLabel(sb.toString());
				drawingCanvas.setTmpStatic(staticList);
			}
		});
		ListCellRenderer renderer =
			new PredCellRenderer(top.strImageDir, "static.gif");
		jlstPreds.setCellRenderer(renderer);
		if (jlstPreds.getModel().getSize() > 0) /* WZ 25/3/02 */
			jlstPreds.setSelectedIndex(-1);
		scrollPanePreds = new JScrollPane(jlstPreds);
		scrollPanePreds.setBorder(
			BorderFactory.createTitledBorder("Static Predicates List"));
		jlstPreds.setToolTipText(
			"SHIFT key for continuous multi-selection;\n"
				+ "CONTROL key for non-continuous muti-selection.");

		/* Weihong 30/11/2001 */
		//add a operators list
		addOperatorList();

		// Build the Operator List
		showMethodLists();

		//assembly eastPanel
		eastPanel.setLayout(new GridLayout(0, 1));
		eastPanel.add(jscrollOM);
		eastPanel.add(scrollPanePreds);

		splitPane2.add(eastPanel);
		splitPane2.setRightComponent(eastPanel);

		splitPane1 =
			new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, westPanel, splitPane2);
		splitPane1.setResizeWeight(0.15);
		splitPane1.setOneTouchExpandable(true); /* WZ 21/8/02 */
		splitPane1.setDividerSize(6); /* WZ 13/5/02 */
		getContentPane().add(splitPane1, "Center");

		//Initiate varibles
		viewSize = scrollPanel.getViewport().getViewSize();
	}

	/* WZ 25/3/02 */
	private JScrollPane addSortTree() {
		//  Now put the sort components together
		sortTree = new SortOnlyTree(curDomain);
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

				} catch (SortSelectionException e) {
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
		// 	curDomain.instantiateStateList(curSSClassDef.getStateSort(),curSSClassDef.getStateList(),sortName);
		resetStateList();
	}

	/* Weihong 30/11/2001 */
	/**
	 * to show/hide the operators list panel
	 * return void
	 */
	public void showOperatorsList(boolean torf) {
		if (torf) {
			eastPanel.add(jscrollOP);
			eastPanel.remove(scrollPanePreds);
		} else {
			eastPanel.remove(jscrollOP);
			eastPanel.add(scrollPanePreds);
		}
		updateUI();
	}

	/**
	 * to add a canvas for displaying/editing graphics
	 * return - void
	 */
	protected void addDrawingCanvas() {
		/* Weihong 26/11/2001 */
		tabPane = new GipoTab(this);
		drawingCanvas = new HighLevelTransitionCanvas(this);
		drawingCanvas.setWorkingDomain(this.curDomain);
		//pass the working domain
		scrollPanel = new JScrollPane(drawingCanvas);
		// 	scrollPanel.setBorder(BorderFactory.createTitledBorder("Editing/Drawing Canvas"));
		drawingCanvas.setToolTipText("Graphical Design & Editing Canvas");
		/* Weihong 26/11/2001 */
		tabPane.addTab("Major Window", scrollPanel);
		splitPane2.add(tabPane);
		splitPane2.setLeftComponent(tabPane);
	}

	/* Weihong 26/11/2001 */
	/**
	 * returns the tab pane.
	 * @return the tab pane - GipoTab.
	 */
	public GipoTab getTabParent() {
		return tabPane;
	}

	/* Weihong 15/11/2001 */
	/**
	 * Set selection radio buttons.
	 * @param tf true or false
	 * 
	 */
	public void setSelectionButton(boolean tf) {
		rbt_select.setSelected(tf);
		jMI_Select.setSelected(tf);
		drawingCanvas.setMouseAction(drawingCanvas.SELECT);
	}

	/* Weihong 12/11/2001 */
	/**
	 * Reset the focus of the view port to its top left corner.
	 * 
	 */
	public void resetViewport() {
		scrollPanel.getHorizontalScrollBar().setValue(0);
		scrollPanel.getVerticalScrollBar().setValue(0);
		updateUI();
	}

	/* Weihong 12/11/2001 */
	/**
	 * Get the size of the graphics window.
	 * @return double valued dimension size
	 */
	public jplan
		.graphics
		.gTool
		.Graphics
		.Double_Dimension getGraphicsWindowSize() {
		return new jplan.graphics.gTool.Graphics.Double_Dimension(
			(double) scrollPanel.getViewport().getExtentSize().width,
			(double) scrollPanel.getViewport().getExtentSize().height);
	}

	/**
	 * Taking states from the domain then display them in a JList.
	 * 
	 */
	protected void addStateList() {
		jlstStateDefs = new DNDList(); /* Weihong 30/11/2001 */
		lmStates = new DefaultListModel();
		jlstStateDefs.setToolTipText(
			"Select state description then click on the canvas to generate its graphics");
		/* Weihong 7/3/02 */
		jlstStateDefs.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		privateList = new java.util.Vector();
		objList = new java.util.Vector();
		resetStateList();
		privateSortList = new JList(privateList);
		/* Weihong 7/3/02 */
		privateSortList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		privateObjectList = new JList(objList);
		/* Weihong 7/3/02 */
		privateObjectList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		jlstStateDefs.setModel(lmStates);
		jlstStateDefs.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent lse) {
				stateListChanged((JList) lse.getSource());
			}
		});
		ListCellRenderer plRenderer = new PredListCellRenderer();
		jlstStateDefs.setCellRenderer(plRenderer);
		if (jlstStateDefs.getModel().getSize() > 0) /* WZ 25/3/02 */
			jlstStateDefs.setSelectedIndex(-1);
		jscrollStateDefs = new JScrollPane(jlstStateDefs);
		jscrollStateDefs.setBorder(
			BorderFactory.createTitledBorder("Substates List"));
	}

	//     /*Weihong 7/3/02 */
	//     /** to return true if "return" key is pressed
	//      * @return true if "return" key is pressed.
	//      */
	//     public boolean getKeyedAction(){
	// 	boolean tmpBoolean = keyedAction;
	// 	keyedAction = false; 
	// 	return tmpBoolean;
	//     }

	/**
	 * reset statelist to its states after the last "commit".
	 * 
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
			jlstStateDefs.setSelectedIndex(-1);
		}

		updateUI();
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
		if (opJList.getModel().getSize() > 0) /* WZ 25/3/02 */
			opJList.setSelectedIndex(-1);

		jscrollOP = new JScrollPane(opJList);
		jscrollOP.setBorder(BorderFactory.createTitledBorder("Operators List"));
	}

	/**
	 * update statelist after the current "commit".
	 * 
	 */
	public void updateStateList(String sort) {
		oclSSClassDef cur = null;
		try {
			cur = (oclSSClassDef) curDomain.getStateListForSort(sort);
		} catch (OCLNoSuchElementException e) {
			Utility.debugPrintln(e);
		}
		lmStates.clear();
		privateList.clear();
		objList.clear();
		ListIterator lii = cur.getStateList().listIterator();
		while (lii.hasNext()) {
			lmStates.addElement((oclStateList) lii.next());
			privateList.addElement(sort);
			objList.addElement(cur.getStateSortId());
		}
		updateUI();
	}

	/**
	 * taking operators from the domain then populate them in a JList
	 * 
	 */
	private void showMethodLists() {
		jlstOM = new DNDList(); /* Weihong 29/11/2001 */
		lmOM = new DefaultListModel();
		jlstOM.setToolTipText(
			"Double click the compound operator to view the graphical display");
		jlstOM.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListIterator li = lstOM.listIterator();
		while (li.hasNext()) {
			lmOM.addElement((oclMethod) li.next());
		}
		jlstOM.setModel(lmOM);
		if (jlstOM.getModel().getSize() > 0) /* WZ 25/3/02 */
			jlstOM.setSelectedIndex(-1);
		jlstOM.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				int i = jlstOM.locationToIndex(e.getPoint());
				oclMethod om = (oclMethod) jlstOM.getSelectedValue();
				if (e.getClickCount() == 2) {
					try {
						if (drawingCanvas.isDirty) {
							int k =
								JOptionPane.showConfirmDialog(
									top,
									"Editing contents have not been updated."
										+ " \nClear content without updating?",
									"",
									JOptionPane.YES_NO_OPTION);
							if (k == JOptionPane.NO_OPTION) {
								return;
							}
						}
						showGraphics((oclMethod) om.clone()); /* WZ 28/3/02 */
						tabPane.setTitleAt(0, om.getName().getName());
						/* Weihong 28/11/2001 */
						oclOM = om; //set the tempMethod
						seleIndex = jlstOM.getSelectedIndex();
					} catch (CloneNotSupportedException ce) {
					}
				}

				/* WZ 14/5/02 right click to show property description */
				if (e.getModifiers() == MouseEvent.BUTTON3_MASK
					&& om != null) {
					String dec = om.getDescription();
					if (dec.trim().equals(new String()))
						dec = "No decription for this method.";

					JOptionPane.showMessageDialog(
						top,
						dec,
						"METHOD - " + om.getName().getName(),
						JOptionPane.PLAIN_MESSAGE);
				}
			}
		});

		jscrollOM = new JScrollPane(jlstOM);
		jscrollOM.setBorder(
			BorderFactory.createTitledBorder("Compound Operators List"));
		splitPane2.add(jscrollOM);
		splitPane2.setRightComponent(jscrollOM);
	}

	/**
	 * reset operators' list to its states after last "commit"
	 * 
	 */
	private void refreshStateList() {
		splitPane2.remove(jscrollOM);
		showMethodLists();
		updateUI();
	}

	/* Weihong 30/11/2001 */
	/**
	 * Get the value of sort.
	 * @return Value of sort.
	 */
	public String getSort() {
		return drawingCanvas.tmpSort;
	}

	/* Weihong 30/11/2001 */
	/**
	 * Get the value of object.
	 * @return Value of object.
	 */
	public String getObject() {
		return drawingCanvas.curObjectID;
	}

	/**
	 * update statelist after the current "commit".
	 * 
	 */
	private void stateListChanged(JList jlist) {
		if (jlist.getSelectedValue() == null)
			return;

		drawingCanvas.predicateList.clear();
		String buf = ((oclStateList) jlist.getSelectedValue()).toString();
		drawingCanvas.setBuffLabel(buf);
		try {
			oclStateList olist =
				(oclStateList) ((oclStateList) jlist.getSelectedValue())
					.clone();
			drawingCanvas.predicateList = olist.getPredicateList();
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln(e);
		}

		privateSortList.setSelectedIndices(jlist.getSelectedIndices());
		drawingCanvas.tmpSort = (String) privateSortList.getSelectedValue();
		privateObjectList.setSelectedIndices(jlist.getSelectedIndices());
		drawingCanvas.curObjectID =
			(String) privateObjectList.getSelectedValue();
	}

	/* Weihong 11/3/02 */
	/**
	 * to show the graphical representation of the given
	 * oclMethod on the given canvas.
	 * @param secondLevel
	 * @param oMethod the compound operator
	 * @param canvas where to show the graphical representation
	 * 
	 */
	public static void showOclMethod(
		boolean secondLevel,
		oclMethod oMethod,
		HighLevelTransitionCanvas canvas) {
		HighLevelTransitionWindow.showOclMethod(
			secondLevel,
			oMethod,
			canvas,
			30);
	}

	/* Weihong 26/11/2001 */
	/**
	 * to show the graphical representation of the given
	 * oclMethod on the given canvas.
	 * @param secondLevel
	 * @param oMethod the compound operator
	 * @param canvas where to show the graphical representation
	 * @param yPosition
	 */
	public static int showOclMethod(
		boolean secondLevel,
		oclMethod oMethod,
		HighLevelTransitionCanvas canvas,
		int yPosition) {
		oclDomain theDomain = canvas.getWorkingDomain();
		// 	HighLevelTransitionWindow theParentFrame = canvas.getParentFrame();
		int i = 0, x1 = 30, y1 = yPosition, x2 = 330;
		vShape vsID, vs;
		oclPredicate opd;

		canvas.addMListener();

		//draw the Id
		canvas.setDrawingShapeID(5);
		i = oMethod.getPrecondition().size() + oMethod.getIndex().size() + 1;
		vsID = canvas.createVShape(180, (i - 1) * 50 + 30);
		vsID.removeTextField();

		/* Weihong 30/11/2001 */
		// 	    MethodHeadCanvas methodCanvas = new MethodHeadCanvas(secondLevel, vsID, theParentFrame);
		// 	    vsID.addDecompCanvas(methodCanvas);

		canvas.setOPName(vsID); //set center shape
		vsID.setLabel(oMethod.getName().getName());
		vsID.setObject(oMethod.getName());
		canvas.initPopupMenuForMethodHead();

		//draw preconditions
		ListIterator li = oMethod.getPrecondition().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			canvas.setDrawingShapeID(1);
			vs = canvas.createVShape(x1, y1);
			if (theDomain != null) /* WZ 25/4/02 */
				/* WZ 29/8/02 */
				vs.textField.setHierFlag(
						theDomain.isHierarchical());
					//canvas.getParentFrame().top.hierarchicalSwitch);
			vs.textField.setSort(se.getSort());
			/* end 29/8/02 */
			vs.textField.setCurDomain(theDomain);
			vs.textField.setObjectID(se.getName());
			vs.textField.setTransElement(TransExpressionDocPane.PREV);
			vs.setDrawLabel(false);
			ListIterator lili = se.getPredicateList().listIterator();
			while (lili.hasNext()) {
				try {
					oclPredicate opt = (oclPredicate) lili.next();
					vs.textField.addPredicate(opt);
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
			}
			vs.setLabel(se.toString());
			vs.setObject(se);

			canvas.setDrawingLinkID(0);
			canvas.createVLink(vs, vsID); //set the relationship

			y1 += 120;
		}

		//draw Index
		li = oMethod.getIndex().listIterator();
		while (li.hasNext()) {
			oclSC oclsc = (oclSC) li.next();
			canvas.setDrawingShapeID(2); //set the mousemode
			vs = canvas.createVShape(x1, y1);
			if (theDomain != null) /* WZ 25/4/02 */
				/* WZ 29/8/02 */
				vs.textField.setHierFlag(
					theDomain.isHierarchical());
					//canvas.getParentFrame().top.hierarchicalSwitch);
			vs.textField.setSort(oclsc.getSort());
			/* end 29/8/02 */
			vs.textField.setCurDomain(theDomain);
			vs.textField.setObjectID(oclsc.getName());
			vs.textField.setTransElement(TransExpressionDocPane.LHS);
			vs.setDrawLabel(false);
			String str = "sc(" + oclsc.getSort() + "," + oclsc.getName() + ",";
			vs.setLabel(str + oclsc.getPre().toString());
			vs.setObject(oclsc);
			ListIterator lili = oclsc.getPre().listIterator();
			while (lili.hasNext()) {
				try {
					oclPredicate opt = (oclPredicate) lili.next();
					vs.textField.addPredicate(opt);
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
			}
			canvas.setMouseAction(canvas.CREATE_LINK);
			canvas.setDrawingLinkID(canvas.calculateLinkTypeID());
			canvas.createVLink(vs, vsID); //set the relationship

			canvas.setDrawingShapeID(2); //set the mousemode
			vs = canvas.createVShape(x2, y1);
			if (theDomain != null) /* WZ 25/4/02 */
				/* WZ 29/8/02 */
				vs.textField.setHierFlag(
					theDomain.isHierarchical());
					//canvas.getParentFrame().top.hierarchicalSwitch);
			vs.textField.setSort(oclsc.getSort());
			/* end 29/8/02 */
			vs.textField.setCurDomain(theDomain);
			vs.textField.setObjectID(oclsc.getName());
			vs.textField.setTransElement(TransExpressionDocPane.RHS);
			vs.setDrawLabel(false);
			vs.setLabel(str + oclsc.getPost().toString());
			vs.setObject(oclsc);
			lili = oclsc.getPost().listIterator();
			while (lili.hasNext()) {
				try {
					oclPredicate opt = (oclPredicate) lili.next();
					vs.textField.addPredicate(opt);
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
			}
			canvas.setMouseAction(canvas.CREATE_LINK);
			canvas.createVLink(vsID, vs); //set the relationship

			y1 += 120;
		}

		//draw static conditions
		//attach a List to the object of the vShape
		li = ((List) oMethod.getStatics()).listIterator();
		if (li.hasNext()) {
			canvas.setDrawingShapeID(4); //set the mousemode
			vs = canvas.createVShape(x1, y1);
			vs.setSize((double) 200, (double) 150);
			if (theDomain != null) /* WZ 25/4/02 */
				//vs.textField.setHierFlag(canvas.getParentFrame().top.hierarchicalSwitch);/* WZ 29/8/02 */	   	    
				vs.textField.setCurDomain(theDomain);
			vs.textField.setTransElement(TransExpressionDocPane.PREV);
			//only left hand side
			vs.setDrawLabel(false);
			vs.setLabel(oMethod.getStatics().toString());
			vs.setObject(oMethod.getStatics());
			ListIterator lili = oMethod.getStatics().listIterator();
			while (lili.hasNext()) {
				try {
					oclPredicate opt = (oclPredicate) lili.next();
					vs.textField.addPredicate(opt);
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
			}
			canvas.setMouseAction(canvas.CREATE_LINK);
			canvas.setDrawingLinkID(canvas.calculateLinkTypeID());
			canvas.createVLink(vs, vsID); //set the relationship

			y1 += 120;

			canvas.setMouseAction(canvas.SELECT);
			canvas.recordState(); //record state for undo
		}

		/* WZ 26/4/02 referenced */
		//draw decompositions
		// 	li = ((List)oMethod.getDecomps()).listIterator();
		// 	if (li.hasNext()){
		// 	    MethodHeadCanvas mdCanvas = (MethodHeadCanvas)vsID.getDecompCanvas();
		// 	    MethodHeadCanvas.setDecomposition(mdCanvas, mdCanvas.getWorkingDomain(), oMethod);
		// 	}

		canvas.setMouseAction(canvas.SELECT); /* WZ 25/4/02 */
		return y1;
	}

	/**
	 * when double click on the operator in the methods' list,
	 * then show the graphical representation on the canvas
	 * @param oMethod the oclMethod which has been double clicked
	 * 
	 */
	public void showGraphics(oclMethod oMethod) {
		int i = 0, x1 = 30, y1 = 30, x2 = 430;
		vShape vsID, vs;
		oclPredicate opd;

		//clear the Canvas
		refreshDrawingWindow();

		drawingCanvas.addMListener();

		//draw the Id
		drawingCanvas.setMouseAction(drawingCanvas.CREATE_SHAPE);
		drawingCanvas.setDrawingShapeID(5);
		i = oMethod.getPrecondition().size() + oMethod.getIndex().size() + 1;
		vsID = drawingCanvas.createVShape(260, (i - 1) * 50 + 30);
		vsID.removeTextField();
		drawingCanvas.setOPName(vsID); //set center shape
		vsID.setLabel(oMethod.getName().getName());
		//method's name(predicate)'s name(string)
		vsID.setObject(oMethod.getName());
		// to add a decompCanvas(JGraphCanvas) to the vShape
		vsID.addDecompCanvas(new MethodHeadCanvas(false, vsID, this));
		/* Weihong 26/11/2001 */
		drawingCanvas.initPopupMenuForMethodHead(); /* Weihong 12/11/2001 */

		//draw preconditions
		ListIterator li = oMethod.getPrecondition().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			jMI_DrawSquareActionPerformed(); //set the mousemode
			vs = drawingCanvas.createVShape(x1, y1);
			drawingCanvas.addToMonitor(vs.textField);
			/* WZ 27/3/02 to monitor changes in the pane */
			/* WZ 29/8/02 */
			//vs.textField.setHierFlag(top.hierarchicalSwitch);
			vs.textField.setHierFlag(curDomain.isHierarchical());
			vs.textField.setSort(se.getSort());
			/* end 29/8/02 */
			vs.textField.setCurDomain(curDomain);
			vs.textField.setObjectID(se.getName());
			vs.textField.setTransElement(TransExpressionDocPane.PREV);
			vs.setDrawLabel(false);
			assignText(vs, se.getPredicateList());
			vs.setLabel(se.toString());
			vs.setObject(se);

			jMI_LinkType1ActionPerformed();
			drawingCanvas.createVLink(vs, vsID); //set the relationship

			y1 += 120;
		}

		//draw Index
		li = oMethod.getIndex().listIterator();
		while (li.hasNext()) {
			oclSC oclsc = (oclSC) li.next();
			jMI_DrawRoundSquareActionPerformed(); //set the mousemode
			vs = drawingCanvas.createVShape(x1, y1);
			drawingCanvas.addToMonitor(vs.textField);
			/* WZ 27/3/02 to monitor changes in the pane */
			/* WZ 29/8/02 */
			vs.textField.setHierFlag(top.hierarchicalSwitch);
			vs.textField.setSort(oclsc.getSort());
			/* end 29/8/02 */
			vs.textField.setCurDomain(curDomain);
			vs.textField.setObjectID(oclsc.getName());
			vs.textField.setTransElement(TransExpressionDocPane.LHS);
			vs.setDrawLabel(false);
			String str = "sc(" + oclsc.getSort() + "," + oclsc.getName() + ",";
			vs.setLabel(str + oclsc.getPre().toString());
			vs.setObject(oclsc);
			assignText(vs, oclsc.getPre());

			drawingCanvas.setMouseAction(drawingCanvas.CREATE_LINK);
			drawingCanvas.setDrawingLinkID(drawingCanvas.calculateLinkTypeID());
			drawingCanvas.createVLink(vs, vsID); //set the relationship

			jMI_DrawRoundSquareActionPerformed(); //set the mousemode
			vs = drawingCanvas.createVShape(x2, y1);
			drawingCanvas.addToMonitor(vs.textField);
			/* WZ 27/3/02 to monitor changes in the pane */
			/* WZ 29/8/02 */
			//vs.textField.setHierFlag(top.hierarchicalSwitch);
			vs.textField.setHierFlag(curDomain.isHierarchical());
			vs.textField.setSort(oclsc.getSort());
			/* end 29/8/02 */
			vs.textField.setCurDomain(curDomain);
			vs.textField.setObjectID(oclsc.getName());
			vs.textField.setTransElement(TransExpressionDocPane.RHS);
			vs.setDrawLabel(false);
			vs.setLabel(str + oclsc.getPost().toString());
			vs.setObject(oclsc);
			assignText(vs, oclsc.getPost());

			drawingCanvas.setMouseAction(drawingCanvas.CREATE_LINK);
			drawingCanvas.createVLink(vsID, vs); //set the relationship

			y1 += 120;
		}

		//draw static conditions
		//attach a List to the object of the vShape
		List staticList = (List) oMethod.getStatics();
		jMI_DrawShape5ActionPerformed(); //set the mousemode
		vs = drawingCanvas.createVShape(x1, y1);
		drawingCanvas.addToMonitor(vs.textField);
		/* WZ 27/3/02 to monitor changes in the pane */
		vs.setSize((double) 200, (double) 150);
		//vs.textField.setHierFlag(top.hierarchicalSwitch);/* WZ 29/8/02 */	   		    
		vs.textField.setCurDomain(curDomain);
		vs.textField.setTransElement(TransExpressionDocPane.PREV);
		//only left hand side
		vs.setDrawLabel(false);
		vs.setLabel(staticList.toString());
		vs.setObject(staticList);
		assignText(vs, staticList);

		drawingCanvas.setMouseAction(drawingCanvas.CREATE_LINK);
		drawingCanvas.setDrawingLinkID(drawingCanvas.calculateLinkTypeID());
		drawingCanvas.createVLink(vs, vsID); //set the relationship

		y1 += 120;

		setSelectionButton(true); /* Weihong 15/11/2001 */

		drawingCanvas.recordState(); //record state for undo

		//draw decompositions
		// 	li = ((List)oMethod.getDecomps()).listIterator();
		// 	if (li.hasNext()){
		/* WZ 4/4/02 */
		MethodHeadCanvas mdCanvas = (MethodHeadCanvas) vsID.getDecompCanvas();
		MethodHeadCanvas.setDecomposition(
			mdCanvas,
			mdCanvas.getWorkingDomain(),
			oMethod);
		// 	}

		//pass description/* WZ 21/8/02 */
		drawingCanvas.setMd_description(oMethod.getDescription());

		resetViewport(); /* WZ 14/5/02 */
	}

	/**
	 * set lists of text(string) to the vshape (its textField)
	 * @param theShape the vshape
	 * @param list the list of text
	 * 
	 */
	protected void assignText(vShape theShape, List list) {
		ListIterator li = list.listIterator();
		while (li.hasNext()) {
			try {
				oclPredicate opt = (oclPredicate) li.next();
				theShape.textField.addPredicate(opt);
			} catch (Exception e) {
				Utility.debugPrintln(
					"Failed to insert predicate " + e.toString());
			}
		}
	}

	/**
	 * to zoom out the view of the canvas
	 * 
	 */
	private void jMI_ZoomOutActionPerformed() {
		viewSize =
			new Dimension(
				(int) (viewSize.width / zoom),
				(int) (viewSize.height / zoom));
		/* Weihong 28/11/2001 */
		tabPane.getSelectedCanvas().setScale(1 / zoom);
	}

	/**
	 * to zoom in the view of the canvas
	 * 
	 */
	private void jMI_ZoomInActionPerformed() {
		viewSize =
			new Dimension(
				(int) (viewSize.width * zoom),
				(int) (viewSize.height * zoom));
		/* Weihong 28/11/2001 */
		tabPane.getSelectedCanvas().setScale(zoom);
	}

	/**
	 * print preview
	 * 
	 */
	private void jMI_PrintPreviewActionPerformed() {
		Thread runner = new Thread() {
			public void run() {
				setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				drawingCanvas.freezeImageForPrinting();
				new PrintPreview(drawingCanvas, "Print Preview");
				setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
		};
		runner.start();
	}

	/**
	 * print graphics on the canvas
	 * 
	 */
	private void jMI_PrintActionPerformed() {
		PrinterJob pj = PrinterJob.getPrinterJob();
		drawingCanvas.freezeImageForPrinting();
		pj.setPrintable(drawingCanvas);
		if (pj.printDialog()) {
			try {
				pj.print();
			} catch (PrinterException ex) {
				System.err.println(ex);
			}
		}
	}

	/**
	 * redo the last undo action
	 * 
	 */
	private void jMI_RedoActionPerformed() {
		try {
			drawingCanvas.redo();
		} catch (CannotRedoException ex) {
			JOptionPane.showMessageDialog(
				top,
				"Unable to redo: " + ex,
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
	}

	/**
	 * Edit variables
	 * 
	 */
	private void jMI_EditVarActionPerformed() {
		if (drawingCanvas.isDirty) {
			vShape tmpSP = (vShape) drawingCanvas.getMethodHead();
			MethodHeadCanvas mdCanvas =
				(MethodHeadCanvas) tmpSP.getDecompCanvas();
			MethodVarEdit mvd = new MethodVarEdit(top, mdCanvas, true);
			/* WZ 25/4/02 */
		} else {
			JOptionPane.showMessageDialog(
				top,
				"please load a compound operator to edit.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
	}

	/**
	 * restore the last action recorded
	 * 
	 */
	private void jMI_UndoActionPerformed() {
		try {
			drawingCanvas.undo();
		} catch (CannotUndoException ex) {
			JOptionPane.showMessageDialog(
				top,
				"Unable to undo: " + ex,
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
	}

	/**
	 * delete selected operator
	 * 
	 */
	private void jMI_DeleteOperatorActionPerformed() {
		int k =
			JOptionPane.showConfirmDialog(
				top,
				"Delete the chosen compound operator?",
				"",
				JOptionPane.YES_NO_OPTION);
		if (k == JOptionPane.YES_OPTION) {
			int i = jlstOM.getSelectedIndex();
			lmOM.remove(i);
			jlstOM.updateUI();
			if (i == seleIndex)
				refreshDrawingWindow();
		}
	}

	/**
	 * Delete links
	 * 
	 */
	private void jMI_DeleteShapeLinkActionPerformed() {
		mouseActionID = drawingCanvas.DELETE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.deleteShape();
		drawingCanvas.deleteLink();
		setSelectionButton(true); /* Weihong 15/11/2001 */
	}

	/**
	 * to set the mode to "selection"
	 * 
	 */
	private void jMI_SelectActionPerformed() {
		mouseActionID = drawingCanvas.SELECT;
		drawingCanvas.setMouseAction(mouseActionID);
	}

	/**
	 * to load a graphical .vm file
	 * 
	 */
	private void jMI_OpenActionPerformed() {
		if (tempFile != null) {
			//ask if the user really want to save the current file first
		}

		chooser.setCurrentDirectory(tempFile);
		chooser.setFileFilter(
			new vFilter(".vm", "Visual Modeller File - *.vm"));
		if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION)
			return;

		Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
		Thread runner = new Thread() {
			public void run() {
				tempFile = chooser.getSelectedFile();
				try {
					BufferedReader in =
						new BufferedReader(
							new InputStreamReader(
								new FileInputStream(tempFile)));
					if (in.ready())
						readFile(in);

					in.close();
				} catch (IOException ex) {
					ex.printStackTrace();
				}

				chooser.rescanCurrentDirectory();
				Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
			}
		};
		runner.start();
	}

	/**
	 * to read from a bufferReader
	 * @param br BufferedReader
	 * 
	 */
	private void readFile(BufferedReader br) {
		drawingCanvas.loadFile(br);
		drawingCanvas.repaint();
	}

	/**
	 * set links to type1
	 * 
	 */
	private void jMI_LinkType1ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_LINK;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingLinkID(0);
	}

	/**
	 * set links to type2
	 * 
	 */
	private void jMI_LinkType2ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_LINK;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingLinkID(1);
	}

	/**
	 * set links to type3
	 * 
	 */
	private void jMI_LinkType3ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_LINK;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingLinkID(2);
	}

	/**
	 * set links to type4
	 * 
	 */
	private void jMI_LinkType4ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_LINK;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingLinkID(3);
	}

	/**
	 * set links to type5
	 * 
	 */
	private void jMI_LinkType5ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_LINK;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingLinkID(4);
	}

	/**
	 * set links to type6
	 * 
	 */
	private void jMI_LinkType6ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_LINK;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingLinkID(5);
	}

	/**
	 * to update the editing operator
	 * 
	 */
	private void jMI_UpdateMethodActionPerformed() {
		if (oclOM == null) {
			JOptionPane.showMessageDialog(
				top,
				"This is a new compound operator. Please use 'Add' button.",
				"GIPO Information",
				JOptionPane.ERROR_MESSAGE,
				null);
		} else { //to save an existing operator
			oclMethod omd = holdThisOperator();
			// Ron 9/4/03
			omd.reOrderDecomp(); // Ron 9/4/03
			if (omd == null) /* Weihong 13/2/02 */
				return;
			lmOM.remove(seleIndex);
			lmOM.insertElementAt(omd, seleIndex);
			refreshDrawingWindow(); //clear window
		}
		dirty = true; /* WZ 14/8/02 */
		jlstOM.updateUI();
	}

	/**
	 * to save as a new graphics .vm
	 * 
	 */
	private void jMI_SaveAsGraphicsActionPerformed() {
		chooser.setCurrentDirectory(tempFile);
		chooser.setFileFilter(
			new vFilter(".vm", "Visual Modeller File - *.vm"));
		if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION)
			return;
		try {
			tempFile = chooser.getSelectedFile();
			PrintStream ps = new PrintStream(new FileOutputStream(tempFile));
			saveVMFile(ps);
		} catch (Exception e) {
		};
	}

	/**
	 * close window
	 * 
	 */
	private void jMI_ExitActionPerformed() {
		if (dirty || drawingCanvas.isDirty) { /* WZ 14/8/02 */
			int k =
				JOptionPane.showConfirmDialog(
					top,
					"Editing contents have not been updated. \nClear content without updating?",
					"",
					JOptionPane.YES_NO_OPTION);
			if (k == JOptionPane.NO_OPTION) {
				return;
			}
		}

		setVisible(false);
		dispose();
	}

	/**
	 * to reset the operators to its states after last "commit"
	 * 
	 */
	private void jMI_RestoreActionPerformed() {
		refreshStateList();
		dirty = false; /* WZ 14/8/02 */
	}

	/**
	 * to add a new operator to the list
	 * 
	 */
	private void jMI_AddActionPerformed() {
		//to save a new compound operator
		oclMethod omd = holdThisOperator();
		if (omd == null) /* Weihong 13/2/02 */
			return;
		omd.reOrderDecomp(); // Ron 9/4/03
		lmOM.addElement(holdThisOperator());
		seleIndex = lmOM.size() - 1;
		jlstOM.setSelectedIndex(seleIndex);
		jlstOM.updateUI();
		refreshDrawingWindow(); //clear window
		dirty = true; /* WZ 14/8/02 */
	}

	/**
	 * to commit changes
	 * 
	 */
	private void jMI_SaveActionPerformed() {
		//to save a new oclMethod
		curDomain.methods.clear();
		lstOM.clear();
		for (int i = 0; i <= lmOM.size() - 1; i++) {
			oclMethod opr = (oclMethod) lmOM.getElementAt(i);
			curDomain.methods.add(opr);
		}

		// 	updateMethodList();/* WZ 14/8/02 */
		// 	refreshStateList();
		top.updateWindow(getTitle());

		dirty = false; /* WZ 14/8/02 */
	}

	/**
	 * to get the oclMethod currently edited
	 * @return oclMethod
	 */
	private oclMethod holdThisOperator() {
		//oclOM is the temp ocloperator file to deal with
		vShape vs = null;

		//get the methodName shape
		vs = (vShape) drawingCanvas.getVShape(5)[0];
		if (vs == null) {
			JOptionPane.showMessageDialog(
				top,
				"No Compound Operator ID. Start From New.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return null;
		}

		return getMethod(vs);
	}

	/* Weihong 15/3/02 */
	/**
	 * return updated operator
	 * @param vs vShape which represents an operator's head shape
	 * @return oclOperator
	 */
	public oclMethod getMethod(vShape vs) {
		vLink tmpLink = null;
		vShape tmpShape = null;
		oclSC tmpSC = null;
		oclSE tmpSE = null;
		oclMethod tmpOP = new oclMethod();
		oclPredicate OPName = null, oprde = null;
		int k = 1;

		//set the oclMethod name - oclPredicate
		OPName = new oclPredicate(vs.getLabel());

		/* WZ 16/8/02 save description */
		tmpOP.addDocmLine(drawingCanvas.getMd_description()); /* WZ 21/8/02 */

		//save decomposition and temporal constraint
		oclMethod omdDEC =
			(oclMethod) ((MethodHeadCanvas) vs.getDecompCanvas())
				.getDecomposition();
		if (omdDEC == null) /* Weihong 13/2/02*/
			return null;
		tmpOP.setTemps(omdDEC.getTemps());
		tmpOP.setDecomps(omdDEC.getDecomps());

		//save other parts
		try {
			for (int i = 1; i < vs.getInLinks().size() + 1; i++) {
				tmpLink = (vLink) vs.getInLinks().get("inlinks" + i);
				tmpShape = tmpLink.getStartShape();
				if (drawingCanvas.isAncestorOf(tmpShape)) {
					switch (tmpShape.getShapeID()) {
						case 1 : //save pre conditions - oclSE 
							//update objectID
							((oclSE) tmpShape.getObject()).setName(
								tmpShape.textField.getObjectID());
							tmpSE =
								new oclSE(
									((oclSE) tmpShape.getObject()).getSort(),
									((oclSE) tmpShape.getObject()).getName());
							ListIterator l =
								tmpShape
									.textField
									.getPurePredicateList()
									.listIterator();
							while (l.hasNext()) {
								oclPredicate oclpd = (oclPredicate) l.next();
								tmpSE.addPredicate(oclpd);
								// 			    curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
							}
							tmpOP.addPreSE(tmpSE);
							break;

						case 2 : //save index - oclSC
							//update objectID
							((oclSC) tmpShape.getObject()).setName(
								tmpShape.textField.getObjectID());
							/* to add preclause*/
							tmpSC =
								new oclSC(
									((oclSC) tmpShape.getObject()).getSort(),
									((oclSC) tmpShape.getObject()).getName());
							l =
								tmpShape
									.textField
									.getPurePredicateList()
									.listIterator();
							while (l.hasNext()) {
								oclPredicate oclpd = (oclPredicate) l.next();
								tmpSC.addPre(oclpd);
								curDomain.addSignatureArgument(OPName, oclpd);
								//take the arguments for the operator
							}

							/* to add PostClause*/
							tmpLink =
								(vLink) vs.getOutLinks().get("outlinks" + k);
							tmpShape = tmpLink.getStopShape();
							l =
								tmpShape
									.textField
									.getPurePredicateList()
									.listIterator();
							while (l.hasNext()) {
								oclPredicate oclpd = (oclPredicate) l.next();
								tmpSC.addPost(oclpd);
								curDomain.addSignatureArgument(OPName, oclpd);
								//take the arguments for the operator
							}
							tmpOP.addIndexSC(tmpSC);
							k++;
							break;

						case 4 : //save statics - oclPredicate
							l =
								tmpShape
									.textField
									.getPurePredicateList()
									.listIterator();
							while (l.hasNext()) {
								oclPredicate oclpd = (oclPredicate) l.next();
								tmpOP.addStatic(oclpd);
								// 			    curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
							}
							break;
					}
				} else if (tmpShape.getShapeID() > 1)
					k++;

				//to hold the information of the decomposition and temporal constraints
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(
				top,
				"Creation of the current compound operator failed.\n"
					+ "Please check the pre-condition and post-condition exist in pair.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return null;
		}

		/* WZ 19/3/02 */
		// add variables to the opname
		// 	curDomain.addSignatureArgument(OPName, (oclPredicate)omdDEC.getName());

		tmpOP.setName(OPName);
		oclOM = tmpOP;
		return tmpOP;
	}

	/**
	 * save current graphics to .vm file
	 * 
	 */
	private void jMI_SaveGraphicsActionPerformed() {
		if (tempFile == null)
			jMI_SaveAsGraphicsActionPerformed();
		else
			try {
				saveVMFile(new PrintStream(new FileOutputStream(tempFile)));
			} catch (Exception e) {
			};
	}

	/**
	 * to create oclMethod head - actionID
	 * 
	 */
	private void jMI_DrawCircleActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_SHAPE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingShapeID(5); //purple circle
		drawingCanvas.createActionID();
		drawingCanvas.setMouseAction(drawingCanvas.SELECT);
	}

	/**
	 * to create Prevail Condition
	 * 
	 */
	private void jMI_DrawSquareActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_SHAPE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingShapeID(1); //rectangle
	}

	/**
	 * to create Necessary Change
	 * 
	 */
	private void jMI_DrawRoundSquareActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_SHAPE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingShapeID(2); //round rectangle
	}

	/**
	 * to create Static Predicates
	 * 
	 */
	private void jMI_DrawShape5ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_SHAPE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingShapeID(4); //round rectangle
	}

	/* Weihong 28/11/2001 */
	/**
	 * enable/disable the bottom tool bar
	 * @param torf true or false
	 * 
	 */
	public void setBottomToolBarEnabled(boolean torf) {
		jToolBar2.setVisible(torf);
	}

	/* Weihong 28/11/2001 */
	/**
	 * enable/disable the check box button for edit mode
	 * @param torf true or false
	 * 
	 */
	public void setEditModeButtonEnabled(boolean torf) {
		editMode.setEnabled(torf);
	}

	/**
	 * switch to the edit mode
	 * 
	 */
	private void jMI_editModeActionPerformed() {
		// 	JGraphCanvas tmpCanvas = (JGraphCanvas)tabPane.getSelectedCanvas();
		JGraphCanvas tmpCanvas = drawingCanvas;
		tabPane.setSelectedIndex(0);
		tmpCanvas.setEditMode(editMode.isSelected());
		if (editMode.isSelected()) {
			tmpCanvas.removeMListener();
			tmpCanvas.repaint();
		} else {
			tmpCanvas.addMListener();
			tmpCanvas.removeHighlighter();
			tmpCanvas.repaint();
		}
	}

	/**
	 * to create a new operator
	 * 
	 */
	private void jMI_NewActionPerformed() {
		//reset all the varibles to its initial status
		//clean the drawing canvas ready for new edit.
		if (drawingCanvas.isDirty) {
			int k =
				JOptionPane.showConfirmDialog(
					top,
					"Editing contents have not been updated. \nClear content without updating?",
					"",
					JOptionPane.YES_NO_OPTION);
			if (k == JOptionPane.NO_OPTION) {
				return;
			}
		}
		refreshDrawingWindow();
		drawingCanvas.createActionID();
		drawingCanvas.addMListener();
		tabPane.setTitleAt(0, drawingCanvas.getMethodHead().getLabel());
		/* Weihong 4/12/2001 */
	}

	/* WZ 21/8/02 */
	/**
	 * to verify all changes
	 * 
	 */
	private void verifyActionPerformed() {
		//save the existing operators
		List tmpSave = new ArrayList();
		ListIterator li = curDomain.methods.listIterator();
		while (li.hasNext()) {
			tmpSave.add((oclMethod) li.next());
		}
		curDomain.methods.clear();
		for (int i = 0; i <= lmOM.size() - 1; i++) {
			oclMethod opr = (oclMethod) lmOM.getElementAt(i);
			curDomain.methods.add(opr);
		}
		// Now verify them
		List mssgs = curDomain.checkMethods();
		if (mssgs.size() > lmOM.size()) {
			//Something to report
			top.checkResultsDialog(mssgs, "All method checks passed");
		} else {
			JOptionPane.showMessageDialog(
				top,
				"All method checks passed.",
				"GIPO Message",
				JOptionPane.INFORMATION_MESSAGE,
				null);
		}
		//Now restore the tasks
		curDomain.methods.clear();
		li = tmpSave.listIterator();
		while (li.hasNext()) {
			curDomain.methods.add((oclMethod) li.next());
		}
	}

	/* WZ 21/8/02 */
	/**
	 * to verify current selected method
	 * 
	 */
	private void verifyCurrentActionPerformed() {
		if (oclOM != null) {
			List msgList = new ArrayList();
			oclOM.check(curDomain, msgList);
			top.checkResultsDialog(msgList, "selected method checks passed");
		} else {
			JOptionPane.showMessageDialog(
				top,
				"There is no currently selected method",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
	}

	/* WZ 2/5/02 */
	/**
	 * to enable/diable sort tree
	 * 
	 */
	public void enableSortTree(boolean torf) {
		sortTree.setEnabled(torf);
	}

	/**
	 * to reset drawing canvas to its initial states
	 * 
	 */
	private void refreshDrawingWindow() {
		drawingCanvas.removeMListener();

		int ww, hh;
		ww = getSize().width;
		hh = getSize().height;
		splitPane2.remove(tabPane);

		//add Drawing Canvas
		addDrawingCanvas();
		if (jlstStateDefs.getModel().getSize() > 0) { /* WZ 25/3/02 */
			jlstStateDefs.setSelectedIndex(-1);
			stateListChanged(jlstStateDefs);
		}

		setSelectionButton(true); /* Weihong 15/11/2001 */

		//set the tempMethod to null
		oclOM = null;
		tempFile = null;

		//reset edit mode
		editMode.setSelected(false);
		drawingCanvas.setEditMode(editMode.isSelected());

		setVisible(false);
		pack();
		setSize(ww, hh);
		setVisible(true);
		updateUI();
	}

	/* WZ 29/4/02 */
	/**
	 * to set the curent editing oclMethod
	 * @param md oclMethod
	 */
	public void setCurMethod(oclMethod md) {
		oclOM = md;
	}

	/* WZ 14/5/02 */
	/**
	 * to get the curent editing oclMethod
	 * @return the curent editing oclMethod
	 */
	public oclMethod getCurMethod() {
		return oclOM;
	}

	/* WZ 27/3/02 */
	/**
	 * return the default canvas
	 * @return the default canvas
	 */
	public HighLevelTransitionCanvas getCanvas() {
		return drawingCanvas;
	}

	/* WZ 27/3/02 */
	/**
	 * to redraw the canvas with updated oclMethod
	 * 
	 */
	public void refreshCanvas() {
		oclMethod curOM =
			(oclMethod) curDomain.checkObjectType(
				(oclPredicate) drawingCanvas.getMethodHead().getObject());
		if (curOM == null) {
			Utility.debugPrintln(
				"No oclMethod found matching the head shape ...");
			return;
		}
		showGraphics(curOM);
	}

	/**
	 * to save VM File
	 * @param ps PrintStream
	 * 
	 */
	private void saveVMFile(PrintStream ps) {
		ps.println("******** VISUAL MODELLER ********\n");
		ps.println("\n");
		ps.println("BEGIN WINDOW VARIBLES\n");
		ps.println("viewSize:" + viewSize.toString() + "\n");
		ps.println("END WINDOW VARIBLES\n");
		ps.println("\n");
		ps.println(drawingCanvas.to_String());
		ps.println("\n");
		ps.close();
	}

	// Variables declaration
	private JPanel topToolBarPanel;
	private JToolBar jToolBar1;
	private JToolBar jToolBar2;
	private JToolBar jToolBar3;
	private JTable jTable1;
	private JTree jTree1;

	private JMenuBar jMenuBar2;

	private JMenu jM_File;
	private JMenuItem jMI_Open;
	private JMenuItem jMI_SaveGraphics;
	private JMenuItem jMI_SaveAsGraphics;
	private JMenuItem jMI_Restore;
	private JMenuItem jMI_Save;
	private JMenuItem jMI_Update;
	private JMenuItem jMI_Exit;
	private JMenuItem jMI_PrintPreview;
	private JMenuItem jMI_Print;

	private JMenu jM_Operator;
	private JMenuItem jMI_New;
	private JMenuItem jMI_Add;
	private JMenuItem jMI_DeleteOperator;

	private JMenu jM_Edit;
	private JMenuItem jMI_Undo;
	private JMenuItem jMI_Redo;

	private ButtonGroup buttonGroup1 = new ButtonGroup();
	private ButtonGroup buttonGroup2 = new ButtonGroup();
	private JMenuItem jMI_DrawCircle;
	private JRadioButtonMenuItem jMI_DrawSquare;
	private JRadioButtonMenuItem jMI_DrawRoundSquare;
	private JRadioButtonMenuItem jMI_DrawShape4;
	private JRadioButtonMenuItem jMI_DrawShape5;
	private JRadioButtonMenuItem jMI_DrawShape6;
	private JRadioButtonMenuItem jMI_LinkType1;
	private JRadioButtonMenuItem jMI_LinkType2;
	private JRadioButtonMenuItem jMI_LinkType3;
	private JRadioButtonMenuItem jMI_LinkType4;
	private JRadioButtonMenuItem jMI_LinkType5;
	private JRadioButtonMenuItem jMI_LinkType6;
	/**
	 * default mouse mode button
	 */
	private JRadioButtonMenuItem jMI_Select; /* Weihong 15/11/2001 */
	private JMenuItem jMI_DeleteShapeLink;
	/**
	 * default mouse mode button
	 */
	private JRadioButton rbt_select; /* Weihong 15/11/2001 */

	private JMenu jM_View;
	private JMenuItem jMI_ZoomOut;
	private JMenuItem jMI_ZoomIn;
	private JCheckBoxMenuItem jMI_FileToolbar;
	private JCheckBoxMenuItem jMI_OperatorToolBar;
	private JCheckBoxMenuItem jMI_EditingToolbar;
	private JCheckBoxMenuItem jMI_EditingStatesList;

	private JCheckBox editMode;

	private JScrollPane scrollPanel;
	private HighLevelTransitionCanvas drawingCanvas;
	private int mouseActionID = 0; //do nothing

	private Dimension viewSize;

	private double zoom = 1.2;

	private JFileChooser chooser = new JFileChooser();
	private File tempFile; //for managing the i/o

	private List lstSSC; // This is all the classdefs 
	/**
	 * parent frame
	 */
	private List sorts;
	private List lstOM; //Methods or Operators
	private DefaultListModel lmOM;
	private JList jlstOM;

	private JPanel westPanel = new JPanel(); //panel to place at west
	private JPanel eastPanel = new JPanel(); //panel to place at east

	// The static predicate List box
	private JList jlstPreds = new JList();
	private DefaultListModel lmPreds = new DefaultListModel();
	private JScrollPane scrollPanePreds;

	// The Sub-state classes List box
	private JList jlstStateDefs;
	private DefaultListModel lmStates;
	private oclSSClassDef curStateList; //The ocl version of the list
	private JScrollPane jscrollStateDefs;
	private JScrollPane jscrollOM;
	private oclMethod oclOM = null; //tempMethod

	private JSplitPane splitPane1, splitPane2;

	private Vector privateList;
	private Vector objList;
	private JList privateSortList;
	private JList privateObjectList; //to save the object ID's list
	private int seleIndex;
	//the selected index of operators list, to use for saving the existing operator into the same pisition

	private GipoTab tabPane; /* Weihong 26/11/2001 */
	private JScrollPane jscrollOP; /* Weihong 30/11/2001 */

	//     private boolean keyedAction = false; /*Weihong 7/3/02 */

	/* WZ 25/3/02 */
	oclSSClassDef curSSClassDef = null;
	String editSort = null; //curent edit sort
	/* end WZ 25/3/02 */

	private SortOnlyTree sortTree; /* WZ 2/5/02 */

	/* WZ 14/8/02 */
	private boolean dirty = false; // flag to monitor changes being committed
}

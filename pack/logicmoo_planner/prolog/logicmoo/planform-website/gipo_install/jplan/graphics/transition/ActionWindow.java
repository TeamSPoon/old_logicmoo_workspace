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

/*
 * Graphical Display Window for editing basic operators
 * @author Weihong Zhao
 * 2/4/2001
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
import javax.swing.tree.*; /* WZ 8/4/02 */

import jplan.edexpt.SortOnlyTree; /* WZ 8/4/02 */
import jplan.graphics.PrintPreview;
import jplan.graphics.gTool.Windows.vFilter;
import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.edexpt.BaseTree.SortSelectionException;
import jplan.edexpt.PredListCellRenderer;
import jplan.edexpt.PredCellRenderer;
import jplan.general.*;
import jplan.general.ExpressionModel.ExpressionException;
import jplan.images.ImageLoader; /* WZ 5/9/2001 */

/**
 * Extended from GipoInternalFrame, this is a window to create/edit
 * transitions for the planning domain model.<br>
 * ActionWindow always works with TransitionCanvas as a interface
 * between the internal domain transition representation and the graphical display.<br>
 * Its function including loading/unloading existing domain information, 
 * e.g. states, operators, creating/editing operators by simple mouse action.
 */
public class ActionWindow extends GipoInternalFrame { /* WZ 24/10/2001 */

	/** 
	 * Create an dummy
	 */
	public ActionWindow(OclEd parent) {
		super(parent);
		//a dummy instance
	}

	/** 
	 * Create an instance of ActionWindow with given domain
	 * @param curDomain oclDomain
	 * @param parent its parent frame
	 */
	public ActionWindow(oclDomain curDomain, OclEd parent) {
		super(parent); /* WZ 24/10/2001 */
		setTitle("Transition Window");
		setClosable(false); /* WZ 12/10/2001 */
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

		/* WZ 3/9/2001 */
		updateOPList();

		dirty = false; /* WZ 19/3/02 */

		tempFile = null;
		initComponents();
		pack();
		setVisible(true);
	}

	/**
	 * Update the operators' list from the domain information.
	 * 
	 */
	private void updateOPList() {
		lstOM = new ArrayList();
		ListIterator li = curDomain.operators.listIterator();
		while (li.hasNext()) {
			try {
				while (li.hasNext()) {
					lstOM.add(((oclOperator) li.next()).clone());
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

		/* WZ 4/9/2001 */
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
		/* WZ 4/9/2001 */
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
		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
		jMI_PrintPreview = new javax.swing.JMenuItem("Print Preview", ii);
		jMI_PrintPreview
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintPreviewActionPerformed();
			}
		});
		jM_File.add(jMI_PrintPreview);
		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		jMI_Print = new javax.swing.JMenuItem("Print", ii);
		jMI_Print.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintActionPerformed();
			}
		});
		jM_File.add(jMI_Print);

		jM_File.addSeparator();
		/* WZ 4/9/2001 */
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
		jM_Operator.setText("Operator");

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "New16.gif");
		jMI_New = new javax.swing.JMenuItem("New", ii);
		jMI_New.setToolTipText("Create a new operator");
		jMI_New.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_NewActionPerformed();
			}
		});
		jM_Operator.add(jMI_New);

		jM_Operator.addSeparator();

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
		jMI_Add = new javax.swing.JMenuItem("Add", ii);
		jMI_Add.setToolTipText("Add to the list");
		jMI_Add.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_AddActionPerformed();
			}
		});
		jM_Operator.add(jMI_Add);
		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		jMI_SaveAs = new javax.swing.JMenuItem("Update", ii);
		jMI_SaveAs.setToolTipText("Update Existing ...");
		jMI_SaveAs.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_SaveAsActionPerformed();
			}
		});
		jM_Operator.add(jMI_SaveAs);

		jM_Operator.addSeparator();
		/* WZ 4/9/2001 */
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
			new javax.swing.JRadioButtonMenuItem(
				"Add Object Expressions (Prevail Condition)");
		jMI_DrawSquare.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_DrawSquareActionPerformed();
			}
		});

		jMI_DrawRoundSquare =
			new javax.swing.JRadioButtonMenuItem(
				"Add Necessary Object Transitions (Necessary Change)");
		jMI_DrawRoundSquare
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_DrawRoundSquareActionPerformed();
			}
		});

		jMI_DrawShape4 =
			new javax.swing.JRadioButtonMenuItem(
				"Add Conditional Object Transitions (Conditional Change)");
		jMI_DrawShape4.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_DrawShape4ActionPerformed();
			}
		});

		jM_Edit.add(jMI_DrawSquare);
		jM_Edit.add(jMI_DrawRoundSquare);
		jM_Edit.add(jMI_DrawShape4);

		jM_Edit.addSeparator();

		jMI_Select = new javax.swing.JRadioButtonMenuItem("Select ...");
		jMI_Select.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_SelectActionPerformed();
			}
		});
		jM_Edit.add(jMI_Select);

		jM_Edit.addSeparator();
		/* WZ 4/9/2001 */
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
		buttonGroup1.add(jMI_DrawShape4);
		buttonGroup1.add(jMI_Select);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Undo16.gif");
		jMI_Undo = new javax.swing.JMenuItem("Undo", ii);
		jMI_Undo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_UndoActionPerformed();
			}
		});
		jM_Edit.add(jMI_Undo);
		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Redo16.gif");
		jMI_Redo = new javax.swing.JMenuItem("Redo", ii);
		jMI_Redo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_RedoActionPerformed();
			}
		});
		jM_Edit.add(jMI_Redo);

		jMenuBar2.add(jM_Edit);

		jM_View = new javax.swing.JMenu("View");
		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		jMI_ZoomIn = new javax.swing.JMenuItem("Zoom In", ii);
		jMI_ZoomIn.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_ZoomInActionPerformed();
			}
		});
		jM_View.add(jMI_ZoomIn);
		/* WZ 4/9/2001 */
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
		jToolBar1.setFloatable(false); /* WZ 12/10/2001 */
		JButton bt = null;

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
		bt = new GipoButton(" Commit ", ii); /* WZ 5/12/2001 */
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_SaveActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Remove16.gif");
		bt = new GipoButton(" Restore ", ii); /* WZ 5/12/2001 */
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_RestoreActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
		bt = new GipoButton("Verify", ii); /* WZ 5/12/2001 */
		bt.setMnemonic(KeyEvent.VK_V);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Ron 9/6/01
				verifyActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* WZ 21/8/02 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
		bt = new GipoButton("Verify current", ii); /* WZ 5/12/2001 */
		bt.setToolTipText("Verify current selected opeartor.");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Ron 9/6/01
				verifyCurrentActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
		bt = new GipoButton(" Preview ", ii); /* WZ 5/12/2001 */
		bt.setToolTipText("Prin Preview");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintPreviewActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		bt = new GipoButton(" Print ", ii); /* WZ 5/12/2001 */
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		bt = new GipoButton(" Close ", ii); /* WZ 5/12/2001 */
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ExitActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		jToolBar3 = new JToolBar();
		jToolBar3.setFloatable(false); /* WZ 12/10/2001 */
		//jToolBar1.setLayout(new FlowLayout());
		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "New16.gif");
		bt = new GipoButton(" New ", ii); /* WZ 5/12/2001 */
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_NewActionPerformed();
			}
		});
		jToolBar3.add(bt);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
		bt = new GipoButton(" Add ", ii); /* WZ 5/12/2001 */
		bt.setAlignmentY(0.5f);
		bt.setToolTipText("Add to the list");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_AddActionPerformed();
			}
		});
		jToolBar3.add(bt);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Refresh16.gif");
		bt = new GipoButton(" Update ", ii); /* WZ 5/12/2001 */
		bt.setAlignmentY(0.5f);
		bt.setToolTipText("Update Existing ...");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_SaveAsActionPerformed();
			}
		});
		jToolBar3.add(bt);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
		bt = new GipoButton(" Delete ", ii); /* WZ 5/12/2001 */
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_DeleteOperatorActionPerformed();
			}
		});
		jToolBar3.add(bt);

		jToolBar3.addSeparator();

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		bt = new GipoButton(" Zoom In ", ii); /* WZ 5/12/2001 */
		bt.setAlignmentY(0.5f);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ZoomInActionPerformed();
			}
		});
		jToolBar3.add(bt);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomOut16.gif");
		bt = new GipoButton(" Zoom Out ", ii); /* WZ 5/12/2001 */
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
		jToolBar2.setFloatable(false); /* WZ 12/10/2001 */
		jToolBar2.setLayout(new GridLayout(0, 7));

		JRadioButton rbt = new JRadioButton("Add Prevail");
		rbt.setToolTipText("Add Prevail Condition");
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

		rbt = new JRadioButton("Add Necessary");
		rbt.setToolTipText("Add Necessary Change");
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

		rbt = new JRadioButton("Add Conditional");
		rbt.setToolTipText("Add Conditional Change");
		rbt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_DrawShape4ActionPerformed();
				jMI_DrawShape4.setSelected(true);
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
				jMI_SelectActionPerformed();
				jMI_Select.setSelected(true);
			}
		});
		rbt_select.setAlignmentY(0.5f);
		rbt_select.setMargin(new Insets(3, 5, 3, 5));
		rbt_select.setFont(font_jToolBar2);
		jToolBar2.add(rbt_select);
		buttonGroup2.add(rbt_select);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Cut16.gif");
		JButton bt_Delete = new JButton("Delete", ii);
		bt_Delete.setToolTipText("Delete shape/link");
		bt_Delete.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_DeleteShapeLinkActionPerformed();
			}
		});
		jToolBar2.add(bt_Delete);

		/* WZ 4/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Undo16.gif");
		JButton bt_Undo = new JButton("Undo", ii);
		bt_Undo.setToolTipText("Undo");
		bt_Undo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_UndoActionPerformed();
			}
		});
		jToolBar2.add(bt_Undo);

		/* WZ 4/9/2001 */
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
		topToolBarPanel.setLayout(new GridLayout(1, 0)); /* WZ 5/12/2001 */
		topToolBarPanel.add(jToolBar1);
		topToolBarPanel.add(jToolBar3);
		getContentPane().add(topToolBarPanel, "North");
		getContentPane().add(jToolBar2, "South");

		/* WZ 29/11/2001 */
		//add splitpane2 (will contain drawingCanvas and jscollOM)
		splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane2.setResizeWeight(0.8);
		splitPane2.setDividerSize(6); /* WZ 13/5/02 */
		splitPane2.setOneTouchExpandable(true); /* WZ 21/8/02 */
		/* add drawing Canvas*/
		addDrawingCanvas();

		/* WZ 8/4/02 */
		//build sort tree
		JScrollPane treePane = addSortTree();

		// Build the SubStateClassList
		addStateList();

		//assembly west panel
		westPanel.setLayout(new GridLayout(0, 1));
		westPanel.add(treePane); /* WZ 8/4/02 */
		westPanel.add(jscrollStateDefs);

		//add static predicate list
		ListIterator liPreds = curDomain.predicates.listIterator();
		oclPredicate opde;
		while (liPreds.hasNext()) {
			opde = (oclPredicate) liPreds.next();
			if (opde.isStatic())
				lmPreds.addElement(opde);
		}
		// 	  jlstPreds = new JList(lmPreds);
		jlstPreds.setModel(lmPreds); /* WZ 8/4/02 */
		jlstPreds.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListCellRenderer renderer =
			new PredCellRenderer(top.strImageDir, "static.gif");
		jlstPreds.setCellRenderer(renderer);
		scrollPanePreds = new JScrollPane(jlstPreds);
		scrollPanePreds.setBorder(
			BorderFactory.createTitledBorder("Static Predicates List"));

		// Build the Operator List
		showOperatorLists();

		/* WZ 8/4/02 */
		//assembly eastPanel
		eastPanel.setLayout(new GridLayout(0, 1));
		eastPanel.add(jscrollOM);
		eastPanel.add(scrollPanePreds);

		splitPane2.add(eastPanel);
		splitPane2.setRightComponent(eastPanel);
		/* WZ 8/4/02 end */

		/* WZ 29/11/2001 */
		splitPane1 =
			new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, westPanel, splitPane2);
		splitPane1.setResizeWeight(0.15);
		splitPane1.setDividerSize(6); /* WZ 13/5/02 */
		splitPane1.setOneTouchExpandable(true); /* WZ 21/8/02 */
		getContentPane().add(splitPane1, "Center");

		//Initiate varibles
		viewSize = scrollPanel.getViewport().getViewSize();
	}

	/* WZ 8/4/02 */
	/**
	 * populateStates 
	 * Update the state list after new sort has been choosen
	 * @param sortName the choosen sort name String
	 */
	private void populateStates(String sortName) {
		editSort = sortName;
		curSSClassDef = curDomain.getIntegClassDEF(sortName);
		resetStateList();
	}

	/* WZ 8/4/02 */
	private JScrollPane addSortTree() {
		//  Now put the sort components together
		sortTree = new SortOnlyTree(curDomain); /* WZ 2/5/02 */
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

	/**
	 * to add a canvas for displaying/editing graphics
	 * return void
	 */
	protected void addDrawingCanvas() {
		drawingCanvas = new TransitionCanvas(this);
		drawingCanvas.setWorkingDomain(this.curDomain);
		//pass the working domain
		scrollPanel = new JScrollPane(drawingCanvas);
		scrollPanel.setBorder(
			BorderFactory.createTitledBorder("Editing/Drawing Canvas"));
		/* WZ 29/11/2001 */
		splitPane2.add(scrollPanel);
		splitPane2.setLeftComponent(scrollPanel);
	}

	/**
	 * Taking states from the domain then display them in a JList.
	 * 
	 */
	protected void addStateList() {
		jlstStateDefs = new JList();
		lmStates = new DefaultListModel();
		jlstStateDefs.setToolTipText(
			"Select state description then click on the canvas to generate its graphics");
		jlstStateDefs.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		privateList = new java.util.Vector();
		objList = new java.util.Vector();
		resetStateList();
		privateSortList = new JList(privateList);
		privateObjectList = new JList(objList);
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

	/* WZ 2/5/02 */
	/**
	 * to enable/diable sort tree
	 * 
	 */
	public void enableSortTree(boolean torf) {
		sortTree.setEnabled(torf);
		// 	if (torf)
		// 	    sortTree.addTreeSelectionListener(sortTreeSelectionListener);
		// 	else
		// 	    sortTree.removeTreeSelectionListener(sortTreeSelectionListener);
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
	private void showOperatorLists() {
		jlstOM = new JList();
		lmOM = new DefaultListModel();
		jlstOM.setToolTipText(
			"Double click the operator to view the graphical display");
		jlstOM.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListIterator li = lstOM.listIterator();
		while (li.hasNext()) {
			lmOM.addElement((oclOperator) li.next());
		}
		jlstOM.setModel(lmOM);
		if (jlstOM.getModel().getSize() > 0) /* WZ 8/4/02 */
			jlstOM.setSelectedIndex(-1);
		jlstOM.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					int i = jlstOM.locationToIndex(e.getPoint());
					oclOperator op = (oclOperator) jlstOM.getSelectedValue();
					try {
						showGraphics((oclOperator) op.clone());
						/* WZ 28/3/02 */
						oclOP = op; //set the tempFile
						seleIndex = jlstOM.getSelectedIndex();
					} catch (CloneNotSupportedException ce) {
					}
				}
			}
		});

		jscrollOM = new JScrollPane(jlstOM);
		jscrollOM.setBorder(BorderFactory.createTitledBorder("Operators List"));

		/* WZ 28/11/2001 */
		splitPane2.add(jscrollOM);
		splitPane2.setRightComponent(jscrollOM);
		// 	  getContentPane().add(jscrollOM, "East");
	}

	/**
	 * reset operators' list to its states after last "commit"
	 * 
	 */
	private void refreshStateList() {
		splitPane2.remove(jscrollOM); /* WZ 28/11/2001 */
		showOperatorLists();
		updateUI();
		// 	jMI_EditingOperatorsList.setState(true);
	}

	/**
	 * update statelist after the current "commit".
	 * 
	 */
	private void stateListChanged(JList jlist) {
		if (jlist.getSelectedValue() == null)
			return;

		drawingCanvas.predicateList.clear();
		drawingCanvas.buffLabel =
			((oclStateList) jlist.getSelectedValue()).toString();
		try {
			oclStateList olist =
				(oclStateList) ((oclStateList) jlist.getSelectedValue())
					.clone();
			drawingCanvas.predicateList = olist.getPredicateList();
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln(e);
		}

		privateSortList.setSelectedIndex(jlist.getSelectedIndex());
		drawingCanvas.tmpSort = (String) privateSortList.getSelectedValue();
		privateObjectList.setSelectedIndex(jlist.getSelectedIndex());
		drawingCanvas.curObjectID =
			(String) privateObjectList.getSelectedValue();
	}

	/**
	 * to show the graphical representation of the given
	 * oclOperator on the given canvas.
	 * @param op the operator which has been double clicked
	 * @param canvas where to show the graphical representation
	 * 
	 */
	public static void showOperatorGraphics(
		oclOperator op,
		TransitionCanvas canvas) {
		if (canvas.isDirty) {
			return;
		}

		oclDomain theDomain = canvas.getWorkingDomain();
		int i = 0, x1 = 30, y1 = 30, x2 = 330;
		vShape vsID, vs;
		oclPredicate opd;

		canvas.addMListener();

		//draw the Id
		canvas.setMouseAction(canvas.CREATE_SHAPE);
		canvas.setDrawingShapeID(0);
		i =
			op.getPrevail().size()
				+ op.getNecessary().size()
				+ op.getConditional().size();
		vsID = canvas.createVShape(190, (i - 1) * 50 + 30);
		vsID.removeTextField();
		canvas.setOPName(vsID); //set center shape
		vsID.setLabel(op.opName.getName());
		//operator's name(predicate)'s name(string)
		vsID.setObject(op.opName);

		//draw prevail conditions
		ListIterator li = op.getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			canvas.setDrawingShapeID(1); //set the mousemode
			vs = canvas.createVShape(x1, y1);
			if (theDomain != null) /* WZ 25/4/02 */
				vs.textField.setHierFlag(
					//	Ron 5/5/03 use domain record
					theDomain.isHierarchical());
					//canvas.getParentFrame().top.hierarchicalSwitch);
			/* WZ 27/8/02 */
			vs.textField.setObjectID(se.getName());
			vs.textField.setSort(se.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(theDomain);
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

			y1 += 100;
		}

		//draw necessary change
		li = op.getNecessary().listIterator();
		while (li.hasNext()) {
			oclSC necessary = (oclSC) li.next();
			canvas.setDrawingShapeID(2); //set the mousemode
			vs = canvas.createVShape(x1, y1);
			if (theDomain != null) /* WZ 25/4/02 */
				vs.textField.setHierFlag(
				//Ron 5/5/03 use domain record
				theDomain.isHierarchical());
					//canvas.getParentFrame().top.hierarchicalSwitch);
			/* WZ 27/8/02 */
			vs.textField.setObjectID(necessary.getName());
			vs.textField.setSort(necessary.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(theDomain);
			vs.textField.setTransElement(TransExpressionDocPane.LHS);
			vs.setDrawLabel(false);
			String str =
				"sc(" + necessary.getSort() + "," + necessary.getName() + ",";
			vs.setLabel(str + necessary.getPre().toString());
			vs.setObject(necessary);
			ListIterator lili = necessary.getPre().listIterator();
			while (lili.hasNext()) {
				try {
					oclPredicate opt = (oclPredicate) lili.next();
					vs.textField.addPredicate(opt);
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
			}

			canvas.setDrawingLinkID(canvas.calculateLinkTypeID());
			canvas.createVLink(vs, vsID); //set the relationship

			vs = canvas.createVShape(x2, y1);
			if (theDomain != null) /* WZ 25/4/02 */
				vs.textField.setHierFlag(
				// Ron 5/5/03 use domain record
					theDomain.isHierarchical());
					//canvas.getParentFrame().top.hierarchicalSwitch);
			/* WZ 27/8/02 */
			vs.textField.setObjectID(necessary.getName());
			vs.textField.setSort(necessary.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(theDomain);
			vs.textField.setTransElement(TransExpressionDocPane.RHS);
			vs.setDrawLabel(false);
			vs.setLabel(str + necessary.getPost().toString());
			vs.setObject(necessary);
			lili = necessary.getPost().listIterator();
			while (lili.hasNext()) {
				try {
					oclPredicate opt = (oclPredicate) lili.next();
					vs.textField.addPredicate(opt);
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
			}

			// 	    canvas.setMouseAction( canvas.CREATE_LINK);
			canvas.createVLink(vsID, vs); //set the relationship

			y1 += 100;
		}

		//draw conditional change
		li = op.getConditional().listIterator();
		while (li.hasNext()) {
			oclSC conditional = (oclSC) li.next();
			canvas.setDrawingShapeID(3); //set the mousemode
			vs = canvas.createVShape(x1, y1);
			if (theDomain != null) /* WZ 25/4/02 */
				vs.textField.setHierFlag(
				// Ron 5/5/03 use domain record
					theDomain.isHierarchical());
					//canvas.getParentFrame().top.hierarchicalSwitch);
			/* WZ 27/8/02 */
			vs.textField.setObjectID(conditional.getName());
			vs.textField.setSort(conditional.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(theDomain);
			vs.textField.setTransElement(TransExpressionDocPane.LHS);
			vs.setDrawLabel(false);
			String str =
				"sc("
					+ conditional.getSort()
					+ ","
					+ conditional.getName()
					+ ",";
			vs.setLabel(str + conditional.getPre().toString());
			vs.setObject(conditional);
			ListIterator lili = conditional.getPre().listIterator();
			while (lili.hasNext()) {
				try {
					oclPredicate opt = (oclPredicate) lili.next();
					vs.textField.addPredicate(opt);
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
			}
			// 	    canvas.setMouseAction( canvas.CREATE_LINK);
			canvas.setDrawingLinkID(canvas.calculateLinkTypeID());
			canvas.createVLink(vs, vsID); //set the relationship

			// 	    canvas.setDrawingShapeID(3);//set the mousemode
			vs = canvas.createVShape(x2, y1);
			if (theDomain != null) /* WZ 25/4/02 */
				vs.textField.setHierFlag(
				// Ron 5/5/03 use domain record
					theDomain.isHierarchical());
					//canvas.getParentFrame().top.hierarchicalSwitch);
			/* WZ 27/8/02 */
			vs.textField.setObjectID(conditional.getName());
			vs.textField.setSort(conditional.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(theDomain);
			vs.textField.setTransElement(TransExpressionDocPane.RHS);
			vs.setDrawLabel(false);
			vs.setObject(conditional);
			lili = conditional.getPost().listIterator();
			while (lili.hasNext()) {
				try {
					oclPredicate opt = (oclPredicate) lili.next();
					vs.textField.addPredicate(opt);
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
			}
			// 	    canvas.setMouseAction( canvas.CREATE_LINK);
			canvas.createVLink(vsID, vs); //set the relationship

			y1 += 100;
		}

		canvas.setMouseAction(canvas.SELECT);
		canvas.recordState(); //record state for undo

		canvas.repaint();
	}

	/**
	 * when double click on the operator in the operators' list,
	 * then show the graphical representation on the canvas
	 * @param op the operator which has been double clicked
	 * 
	 */
	private void showGraphics(oclOperator op) {
		/* Weihong changed/added on 2/10/2001 */
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
		/* Weihong changed/added on 2/10/2001 end*/

		/* Weihong 15/3/02 clear pane group registration */
		// 	drawingCanvas.clearPaneGroup();

		int i = 0, x1 = 10, y1 = 20;
		/* WZ 14/6/02 */
		int spaceBetween = 30;
		int d_Width = (int) drawingCanvas.getD_Width();
		int d_Height = (int) drawingCanvas.getD_Height();
		int x2 = x1 + 2 * (d_Width + spaceBetween);
		/* end 14/6/02 */
		vShape vsID, vs;
		oclPredicate opd;

		//clear the Canvas
		refreshDrawingWindow();

		drawingCanvas.addMListener(); /* Weihong added/changed on 2/10/01 */

		//draw the Id
		drawingCanvas.setMouseAction(drawingCanvas.CREATE_SHAPE);
		drawingCanvas.setDrawingShapeID(0);
		i =
			op.getPrevail().size()
				+ op.getNecessary().size()
				+ op.getConditional().size();
		vsID =
			drawingCanvas.createVShape(
				d_Width + x1 + spaceBetween,
				(i - 1) * d_Height + spaceBetween);
		vsID.removeTextField();
		drawingCanvas.setOPName(vsID); //set center shape
		vsID.setLabel(op.opName.getName());
		//operator's name(predicate)'s name(string)
		vsID.setObject(op.opName);

		//draw prevail conditions
		ListIterator li = op.getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			jMI_DrawSquareActionPerformed(); //set the mousemode
			vs = drawingCanvas.createVShape(x1, y1);
			// Ron 5/5/03 use the domain flag
			vs.textField.setHierFlag(curDomain.isHierarchical());
			//vs.textField.setHierFlag(top.hierarchicalSwitch); /* WZ 27/8/02 */
			vs.textField.setObjectID(se.getName()); /*WZ 22/08/2001 */
			vs.textField.setSort(se.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(curDomain);
			vs.textField.setTransElement(TransExpressionDocPane.PREV);
			/* WZ 30/08/01 */
			vs.setDrawLabel(false);
			assignText(vs, se.getPredicateList());
			vs.setLabel(se.toString());
			vs.setObject(se);

			jMI_LinkType1ActionPerformed();
			drawingCanvas.createVLink(vs, vsID); //set the relationship

			y1 += d_Height + spaceBetween; /* WZ 14/6/02 */
		}

		//draw necessary change
		li = op.getNecessary().listIterator();
		while (li.hasNext()) {
			oclSC necessary = (oclSC) li.next();
			jMI_DrawRoundSquareActionPerformed(); //set the mousemode
			vs = drawingCanvas.createVShape(x1, y1);
//			// Ron 5/5/03 use the domain flag
			vs.textField.setHierFlag(curDomain.isHierarchical());
			//vs.textField.setHierFlag(top.hierarchicalSwitch); /* WZ 27/8/02 */
			vs.textField.setObjectID(necessary.getName()); /*WZ 22/08/2001 */
			vs.textField.setSort(necessary.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(curDomain);
			vs.textField.setTransElement(TransExpressionDocPane.LHS);
			/* WZ 30/08/01 */
			vs.setDrawLabel(false);
			String str =
				"sc(" + necessary.getSort() + "," + necessary.getName() + ",";
			vs.setLabel(str + necessary.getPre().toString());
			vs.setObject(necessary);
			assignText(vs, necessary.getPre());

			drawingCanvas.setMouseAction(drawingCanvas.CREATE_LINK);
			drawingCanvas.setDrawingLinkID(drawingCanvas.calculateLinkTypeID());
			drawingCanvas.createVLink(vs, vsID); //set the relationship

			jMI_DrawRoundSquareActionPerformed(); //set the mousemode
			vs = drawingCanvas.createVShape(x2, y1);
//			//Ron 5/5/03 use the domain flag
			vs.textField.setHierFlag(curDomain.isHierarchical());
			//vs.textField.setHierFlag(top.hierarchicalSwitch); /* WZ 27/8/02 */
			vs.textField.setObjectID(necessary.getName()); /*WZ 22/08/2001 */
			vs.textField.setSort(necessary.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(curDomain);
			vs.textField.setTransElement(TransExpressionDocPane.RHS);
			/* WZ 30/08/01 */
			vs.setDrawLabel(false);
			vs.setLabel(str + necessary.getPost().toString());
			vs.setObject(necessary);
			assignText(vs, necessary.getPost());

			drawingCanvas.setMouseAction(drawingCanvas.CREATE_LINK);
			drawingCanvas.createVLink(vsID, vs); //set the relationship

			y1 += d_Height + spaceBetween; /* WZ 14/6/02 */
		}

		//draw conditional change
		li = op.getConditional().listIterator();
		while (li.hasNext()) {
			oclSC conditional = (oclSC) li.next();
			jMI_DrawShape4ActionPerformed(); //set the mousemode
			vs = drawingCanvas.createVShape(x1, y1);
//			// Ron 5/5/03 use the domain flag
			vs.textField.setHierFlag(curDomain.isHierarchical());
			//vs.textField.setHierFlag(top.hierarchicalSwitch); /* WZ 27/8/02 */
			vs.textField.setObjectID(conditional.getName()); /*WZ 22/08/2001 */
			vs.textField.setSort(conditional.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(curDomain);
			vs.textField.setTransElement(TransExpressionDocPane.LHS);
			/* WZ 30/08/01 */
			vs.setDrawLabel(false);
			String str =
				"sc("
					+ conditional.getSort()
					+ ","
					+ conditional.getName()
					+ ",";
			vs.setLabel(str + conditional.getPre().toString());
			vs.setObject(conditional);
			assignText(vs, conditional.getPre());

			drawingCanvas.setMouseAction(drawingCanvas.CREATE_LINK);
			drawingCanvas.setDrawingLinkID(drawingCanvas.calculateLinkTypeID());
			drawingCanvas.createVLink(vs, vsID); //set the relationship

			jMI_DrawShape4ActionPerformed(); //set the mousemode
			vs = drawingCanvas.createVShape(x2, y1);
//			// Ron 5/5/03 use the domain flag
			vs.textField.setHierFlag(curDomain.isHierarchical());
			//vs.textField.setHierFlag(top.hierarchicalSwitch); /* WZ 27/8/02 */
			vs.textField.setObjectID(conditional.getName()); /*WZ 22/08/2001 */
			vs.textField.setSort(conditional.getSort()); /* WZ 27/8/02 */
			vs.textField.setCurDomain(curDomain);
			vs.textField.setTransElement(TransExpressionDocPane.RHS);
			/* WZ 30/08/01 */
			vs.setDrawLabel(false);
			vs.setLabel(str + conditional.getPost().toString());
			/* WZ 14/6/02 */
			vs.setObject(conditional);
			assignText(vs, conditional.getPost());
			drawingCanvas.setMouseAction(drawingCanvas.CREATE_LINK);
			drawingCanvas.createVLink(vsID, vs); //set the relationship

			y1 += d_Height + spaceBetween; /* WZ 14/6/02 */
		}

		rbt_select.setSelected(true);
		jMI_SelectActionPerformed();

		drawingCanvas.recordState(); //record state for undo

		resetViewport(); /* WZ 14/5/02 */
	}

	/* WZ 14/5/02 */
	/**
	 * Reset the focus of the view port to its top left corner.
	 * 
	 */
	public void resetViewport() {
		scrollPanel.getHorizontalScrollBar().setValue(0);
		scrollPanel.getVerticalScrollBar().setValue(0);
		updateUI();
	}

	/**
	 * set lists of text(string) to the vshape (its textField)
	 * @param theShape the vshape
	 * @param list the list of text
	 * 
	 */
	private void assignText(vShape theShape, List list) {
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
		drawingCanvas.setScale(1 / zoom);
		repaint();
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
		drawingCanvas.setScale(zoom);
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
				"Delete the chosen operator?",
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
		Utility.debugPrintln("\nDELETING\n");
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.deleteShape();
		drawingCanvas.deleteLink();

		jMI_SelectActionPerformed();
		jMI_Select.setSelected(true);
	}

	/**
	 * to set the mode to "selection"
	 * 
	 */
	private void jMI_SelectActionPerformed() {
		mouseActionID = drawingCanvas.SELECT;
		Utility.debugPrintln("\nSELECT\n");
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
	private void jMI_SaveAsActionPerformed() {
		if (oclOP == null) {
			/* WZ 3/9/2001 */
			JOptionPane.showMessageDialog(
				top,
				"This is a new operator. Please use 'Add' button.",
				"GIPO Information",
				JOptionPane.ERROR_MESSAGE,
				null);
		} else { //to save an existing operator
			lmOM.remove(seleIndex);
			lmOM.insertElementAt(holdThisOperator(), seleIndex);
			refreshDrawingWindow(); //clear window
		}
		dirty = true; /* WZ 19/3/02 */
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
		/* Weihong changed/added on 2/10/2001 */
		if (dirty || drawingCanvas.isDirty) { /* WZ 19/3/02 */
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
		/* Weihong changed/added on 2/10/2001 end*/

		setVisible(false);
		dispose();
	}

	/**
	 * to reset the operators to its states after last "commit"
	 * 
	 */
	private void jMI_RestoreActionPerformed() {
		refreshStateList();
		dirty = false; /* WZ 19/3/02 */
	}

	/**
	 * to add a new operator to the list
	 * 
	 */
	private void jMI_AddActionPerformed() {
		//to save a new operator
		lmOM.addElement(holdThisOperator());
		seleIndex = lmOM.size() - 1; /* Weihong changed/added on 3/9/2001 */
		jlstOM.setSelectedIndex(seleIndex);
		jlstOM.updateUI();
		refreshDrawingWindow(); //clear window
		dirty = true; /* WZ 19/3/02 */
	}

	/**
	 * to commit changes
	 * 
	 */
	private void jMI_SaveActionPerformed() {
		//to save a new operator
		curDomain.operators.clear();
		lstOM.clear();
		for (int i = 0; i <= lmOM.size() - 1; i++) {
			oclOperator opr = (oclOperator) lmOM.getElementAt(i);
			curDomain.operators.add(opr);
			// 	    lstOM.add(opr);
		}

		/* WZ 3/9/2001 */
		// 	updateOPList();/* WZ 14/8/02 */

		// 	refreshStateList();/* WZ 14/8/02 */

		/* WZ 18/07/2001 */
		top.updateWindow(getTitle());

		dirty = false; /* WZ 19/3/02 */
	}

	/**
	 * to get the operator currently edited
	 * @return oclOperator
	 */
	private oclOperator holdThisOperator() {
		vShape vs = null;
		vs = drawingCanvas.getVShape(0)[0];
		if (vs == null) {
			JOptionPane.showMessageDialog(
				top,
				"No Operator ID. Start From New.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return null;
		}

		return getOpertor(vs);
	}

	/* Weihong 15/3/02 */
	/**
	 * return updated operator
	 * @param vs vShape which represents an operator's head shape
	 * @return oclOperator
	 */
	public oclOperator getOpertor(vShape vs) {
		vLink tmpLink = null;
		vShape tmpShape = null;
		oclSC tmpSC = null;
		oclSE tmpSE = null;
		oclOperator tmpOP = new oclOperator();
		oclPredicate OPName = null, oprde = null;
		int k = 1;

		OPName = new oclPredicate(vs.getLabel());

		try {
			for (int i = 1; i < vs.getInLinks().size() + 1; i++) {
				tmpLink = (vLink) vs.getInLinks().get("inlinks" + i);
				tmpShape = tmpLink.getStartShape();
				if (drawingCanvas.isAncestorOf(tmpShape)) {
					switch (tmpShape.getShapeID()) {
						case 1 : //oclSE
							//update objectID
							/*WZ 23/08/01 */
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
								curDomain.addSignatureArgument(OPName, oclpd);
								//take the arguments for the operator
							}
							tmpOP.addPrevSE(tmpSE);
							break;

						case 2 : //oclSC
							//update objectID
							/*WZ 23/08/01 */
							((oclSC) tmpShape.getObject()).setName(
								tmpShape.textField.getObjectID());
							Utility.debugPrintln(
								"Update Object = "
									+ ((oclSC) tmpShape.getObject()).getName());
							Utility.debugPrintln(
								"Update Object from textfield = "
									+ tmpShape.textField.getObjectID());
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
							tmpOP.addNecSC(tmpSC);
							k++;
							break;

						case 3 : //oclSC
							//update objectID
							/*WZ 23/08/01 */
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
								/* Weihong changed/added on 3/0/2001 */
								//curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
								/* Weihong changed/added end */
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
								/* Weihong changed/added on 3/0/2001 */
								//curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
								/* Weihong changed/added end */
							}

							tmpOP.addCondSC(tmpSC);
							k++;
							break;
					}
				} else if (tmpShape.getShapeID() > 1)
					k++;
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(
				top,
				"Creation of the current operator failed. Please check the pre-condition and post-condition exist in pair.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return null;
		}

		tmpOP.setName(OPName);
		/* WZ 3/9/2001 */
		oclOP = tmpOP;
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
	 * to create operator head - actionID
	 * 
	 */
	private void jMI_DrawCircleActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_SHAPE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingShapeID(0); //circle
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
	 * to create Conditional Change
	 * 
	 */
	private void jMI_DrawShape4ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_SHAPE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingShapeID(3); //round rectangle
	}

	/**
	 * to create Static Predicates
	 * 
	 */
	private void jMI_DrawShape5ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_SHAPE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingShapeID(4); //round rectangle
		Utility.debugPrintln("CREATE_SHAPE ---- Static Predicates.\n");
	}

	/**
	 * to create Temporal Predicates
	 * 
	 */
	private void jMI_DrawShape6ActionPerformed() {
		mouseActionID = drawingCanvas.CREATE_SHAPE;
		drawingCanvas.setMouseAction(mouseActionID);
		drawingCanvas.setDrawingShapeID(5); //round rectangle
		Utility.debugPrintln("CREATE_SHAPE ---- Temporal Predicates.\n");
	}

	/**
	 * switch to the edit mode
	 * 
	 */
	private void jMI_editModeActionPerformed() {
		drawingCanvas.setEditMode(editMode.isSelected());
		if (editMode.isSelected()) {
			// 	    drawingCanvas.removeMouseListener(drawingCanvas);
			// 	    drawingCanvas.removeMouseMotionListener(drawingCanvas);
			drawingCanvas.removeMListener();
			drawingCanvas.repaint();
		} else {
			// 	    drawingCanvas.addMouseListener(drawingCanvas);	    
			// 	    drawingCanvas.addMouseMotionListener(drawingCanvas);
			drawingCanvas.addMListener();
			drawingCanvas.removeHighlighter();
			drawingCanvas.repaint();
		}
	}

	/**
	 * to create a new operator
	 * 
	 */
	private void jMI_NewActionPerformed() {
		//reset all the varibles to its initial status
		//clean the drawing canvas ready for new edit.

		/* Weihong changed/added on 2/10/2001 */
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
		/* Weihong changed/added on 2/10/2001 end*/
		refreshDrawingWindow();
		drawingCanvas.createActionID();
		drawingCanvas.addMListener(); /* Weihong added/changed on 2/10/01 */
	}

	/* WZ 21/8/02 */
	/**
	 * to verify current selected method
	 * 
	 */
	private void verifyCurrentActionPerformed() {
		if (oclOP != null) {
			List msgList = new ArrayList();
			oclOP.check(curDomain, msgList);
			top.checkResultsDialog(msgList, "selected operator checks passed");
		}
	}

	// Ron 9/6/01
	/**
	 * to verify all changes
	 * 
	 */
	private void verifyActionPerformed() {
		//save the existing operators
		List tmpSave = new ArrayList();
		ListIterator li = curDomain.operators.listIterator();
		while (li.hasNext()) {
			tmpSave.add(li.next());
		}
		curDomain.operators.clear();
		for (int i = 0; i <= lmOM.size() - 1; i++) {
			oclOperator opr = (oclOperator) lmOM.getElementAt(i);
			curDomain.operators.add(opr);
		}
		// Now verify them
		List mssgs = curDomain.checkOperators(); /* 21/8/02 */
		if (mssgs.size() > lmOM.size()) {
			//Something to report
			top.checkResultsDialog(mssgs, "All operator checks passed");
		} else {
			JOptionPane.showMessageDialog(
				top,
				"All operator checks passed.",
				"GIPO Message",
				JOptionPane.INFORMATION_MESSAGE,
				null);
		}
		//Now restore the tasks
		curDomain.operators.clear();
		li = tmpSave.listIterator();
		while (li.hasNext()) {
			curDomain.operators.add(li.next());
		}
	}

	/**
	 * to reset drawing canvas to its initial states
	 * 
	 */
	private void refreshDrawingWindow() {
		drawingCanvas.removeMListener(); /* Weihong added/changed on 2/10/01 */

		int ww, hh;
		ww = getSize().width;
		hh = getSize().height;
		splitPane2.remove(scrollPanel); /* WZ 29/11/2001 */

		//add Drawing Canvas
		addDrawingCanvas();
		if (jlstStateDefs.getModel().getSize() > 0) { /* WZ 8/4/02 */
			jlstStateDefs.setSelectedIndex(-1);
			stateListChanged(jlstStateDefs);
		}

		rbt_select.setSelected(true);
		drawingCanvas.setMouseAction(drawingCanvas.SELECT);
		jMI_Select.setSelected(true);

		//set the tempFile to null
		oclOP = null;
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

	/* WZ 26/11/2001 */
	/**
	 * return the domain
	 * @return the ocl domain
	 */
	public oclDomain getWorkingDomain() {
		return curDomain;
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
	private JMenuItem jMI_SaveAs;
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
	public JRadioButtonMenuItem jMI_Select;
	private JMenuItem jMI_DeleteShapeLink;
	/**
	 * default mouse mode button
	 */
	public JRadioButton rbt_select;

	private JMenu jM_View;
	private JMenuItem jMI_ZoomOut;
	private JMenuItem jMI_ZoomIn;
	private JCheckBoxMenuItem jMI_FileToolbar;
	private JCheckBoxMenuItem jMI_OperatorToolBar;
	private JCheckBoxMenuItem jMI_EditingToolbar;
	private JCheckBoxMenuItem jMI_EditingStatesList;
	private JCheckBoxMenuItem jMI_StaticPredicateList;
	//     private  JCheckBoxMenuItem jMI_EditingOperatorsList;

	private JCheckBox editMode;

	private JScrollPane scrollPanel;
	protected TransitionCanvas drawingCanvas;
	private int mouseActionID = 0; //do nothing

	private Dimension viewSize;

	private double zoom = 1.2;

	private JFileChooser chooser = new JFileChooser();
	private File tempFile; //for managing the i/o

	private List lstSSC; // This is all the classdefs 
	/**
	 * parent frame
	 */
	public OclEd top; /* Weihong changed/added on 3/9/2001 */
	/**
	 * the current oclDomain
	 */
	protected oclDomain curDomain;
	private List sorts;
	private List lstOM; //Methods or Operators
	private DefaultListModel lmOM;
	private JList jlstOM;

	private JPanel westPanel = new JPanel(); //panel to place at west
	private JPanel eastPanel = new JPanel();
	//panel to place at east/* WZ 8/4/02 */

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
	private oclOperator oclOP = null; //tempFile

	private JSplitPane splitPane1, splitPane2; /* WZ 29/11/2001 */

	private Vector privateList;
	private Vector objList;
	private JList privateSortList;
	private JList privateObjectList; //to save the object ID's list
	private int seleIndex;
	//the selected index of operators list, to use for saving the existing operator into the same pisition

	/* WZ 19/3/02 */
	private boolean dirty = false; // flag to monitor changes being committed
	private oclSSClassDef curSSClassDef = null; /* WZ 8/4/02 */
	private String editSort = null; //curent edit sort /* WZ 8/4/02 */

	private SortOnlyTree sortTree; /* WZ 2/5/02 */
}

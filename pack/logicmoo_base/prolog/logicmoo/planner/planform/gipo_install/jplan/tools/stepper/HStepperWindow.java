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

/*
 * Stepper Graphical Display Window
 * Weihong Zhao
 * 22/5/2001
 * October 02 - Ron 
 * Added filtering to the current and goal states to make it easier to
 * see what is happening to individual objects
 */

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

import jplan.graphics.PrintPreview;
import jplan.graphics.gTool.Windows.vFilter;
import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.edexpt.PredListCellRenderer;
import jplan.images.ImageLoader; /* WZ 5/9/2001 */
import jplan.general.*; 

/**
 * As a interface StepperWindow manually shows the graphical
 * representation of the result after execution of an operator.
 * Goal state can be auto detected.
 */
public class HStepperWindow
	extends GipoInternalFrame
	implements oclPrint { /* WZ 24/10/2001 */

	// Variables declaration
	private JPanel topToolBarPanel;
	private JToolBar jToolBar1;
	private JToolBar jToolBar3;
	private JMenuBar jMenuBar2;
	private JMenu jM_File;
	private JMenuItem jMI_Open;
	private JMenuItem jMI_SaveGraphics;
	private JMenuItem jMI_SaveAsGraphics;
	private JMenuItem jMI_Save;
	private JMenuItem jMI_SaveAs;
	private JMenuItem jMI_Exit;
	private JMenuItem jMI_PrintPreview;
	private JMenuItem jMI_Print;
	private JMenuItem jMI_PrintText; /*Weihong 14/3/02 */

	private JMenu jM_Action;
	private JMenuItem jMI_Pre;
	private JMenuItem jMI_Next;

	private JMenu jM_View;
	private JMenuItem jMI_Toggle_Invars;
	private boolean showInvars = false;
	private JMenuItem jMI_ZoomOut;
	private JMenuItem jMI_ZoomIn;
	private JCheckBoxMenuItem jMI_FileToolbar;
	private JCheckBoxMenuItem jMI_ActionToolBar;
	private JCheckBoxMenuItem jMI_EditingStatesList;
	private JCheckBoxMenuItem jMI_EditingOperatorsList;

	private JScrollPane scrollPanel;
	private HStepperCanvas drawingCanvas;
	private Dimension viewSize;
	private double zoom = 1.5;

	private JFileChooser chooser = new JFileChooser();
	private File tempFile; //for managing the i/o

	private List lstTask; // This is all thetasks
	private FilterStatePane flwCurState; //This displays the current state
	private FilterStatePane flwInitState; //This displays the initial state
	private FilterStaticsPane flStatics; // This displays the atomic invars
	private JSplitPane staskPanel = null; // Now need access to this to change content
	/**
	 * parent frame
	 */
	private OclEd top; /* WZ 1/11/01 */
	private oclDomain curDomain;
	private List lstOP; //Operators
	private DefaultListModel lmOP;
	private JList jlstOP;
	/* WZ 5/2/02 */
	private List lstMD; //Methods
	private DefaultListModel lmMD;
	private JList jlstMD;
	//     private JScrollPane jscrollOM;
	private JPanel jPanelOM;
	/* end WZ 5/2/02 */
	private JPanel westPanel = new JPanel(); //panel to place at west

	// The tasks
	private JComboBox jcomTasks;
	private Vector vecTasks = new Vector();
	private oclHTNTask curTask = null;

	// The initial state classes list box
	private List initState = new ArrayList();
	private int seleIndex;
	//the selected index of operators list, to use for saving the existing operator into the same pisition


	// The goal state classes list box
	private JList jlstGoalState;
	private DefaultListModel lmGoalState;
	private JScrollPane jscrollGoalState;

	private JSplitPane splitPane1, splitPane2; /* WZ 5/12/2001 */

	DefaultListModel lmStatics, lmTemps; /* WZ 13/5/02 */

	/**
	 * Create an instance of StepperWindow
	 * @param title String title
	 * @param curDomain oclDomain
	 * @param parent parent frame
	 */
	public HStepperWindow(String title, oclDomain curDomain, OclEd parent) {
		super(parent); /* WZ 24/10/2001 */
		setTitle(title);
		setClosable(false); /* WZ 12/10/2001 */

		this.curDomain = curDomain;
		top = parent;

		lstTask = new ArrayList(); //Need to clone the tasks
		ListIterator li = curDomain.htntasks.listIterator();
		while (li.hasNext()) {
			lstTask.add(((oclHTNTask) li.next()));
		}

		lstOP = new ArrayList();
		li = curDomain.operators.listIterator();
		while (li.hasNext()) {
			/* WZ 22/5/02 added clone */
			oclOperator op = (oclOperator) li.next();
			try {
				lstOP.add((oclOperator) op.clone());
			} catch (CloneNotSupportedException e) {
			}
		}

		lstMD = new ArrayList();
		li = curDomain.methods.listIterator();
		while (li.hasNext()) {
			/* WZ 22/5/02 added clone */
			oclMethod md = (oclMethod) li.next();
			try {
				lstMD.add(md.clone());
			} catch (CloneNotSupportedException e) {
			}
		}

		tempFile = null;
		initComponents();
		pack();
		setVisible(true);
	}

	/**
	 * Initialisation
	 * 
	 */
	private void initComponents() {

		getContentPane().setLayout(new java.awt.BorderLayout());

		jMenuBar2 = new javax.swing.JMenuBar();
		jM_File = new javax.swing.JMenu();
		jM_File.setText("File");

		jM_File.addSeparator();

		/* WZ 5/9/2001 */
		ImageIcon ii =
			ImageLoader.getImageIcon(top.strImageDir, "Import16.gif");
		jMI_Open = new javax.swing.JMenuItem("Load stepper file", ii);
		jMI_Open.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_OpenActionPerformed();
			}
		});
		jM_File.add(jMI_Open);

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Export16.gif");
		jMI_SaveGraphics = new javax.swing.JMenuItem("Save stepper file", ii);
		jMI_SaveGraphics
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_SaveGraphicsActionPerformed();
			}
		});
		jM_File.add(jMI_SaveGraphics);
		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "SaveAs16.gif");
		jMI_SaveAsGraphics =
			new javax.swing.JMenuItem("Save as new stepper file", ii);
		jMI_SaveAsGraphics
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_SaveAsGraphicsActionPerformed();
			}
		});
		jM_File.add(jMI_SaveAsGraphics);

		jM_File.addSeparator();
		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
		jMI_PrintPreview = new javax.swing.JMenuItem("Print Preview", ii);
		jMI_PrintPreview
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintPreviewActionPerformed();
			}
		});
		jM_File.add(jMI_PrintPreview);
		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		jMI_Print = new javax.swing.JMenuItem("Print graphics", ii);
		jMI_Print.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintActionPerformed();
			}
		});
		jM_File.add(jMI_Print);

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		jMI_PrintText = new javax.swing.JMenuItem("Action sequence", ii);
		jMI_PrintText.setToolTipText("Display/Print a text format action list");
		jMI_PrintText.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintTextActionPerformed();
			}
		});
		jM_File.add(jMI_PrintText);

		jM_File.addSeparator();
		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		jMI_Exit = new javax.swing.JMenuItem("Close", ii);
		jMI_Exit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_ExitActionPerformed();
			}
		});
		jM_File.add(jMI_Exit);

		jMenuBar2.add(jM_File);

		jM_Action = new javax.swing.JMenu();
		jM_Action.setText("Action");
		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Undo16.gif");
		jMI_Pre = new javax.swing.JMenuItem("Pre Step", ii);
		jMI_Pre.setToolTipText("Go to the previous step");
		jMI_Pre.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PreActionPerformed();
			}
		});
		jM_Action.add(jMI_Pre);
		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Redo16.gif");
		jMI_Next = new javax.swing.JMenuItem("Next Step", ii);
		jMI_Next.setToolTipText("Go to the next step");
		jMI_Next.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_NextActionPerformed();
			}
		});
		jM_Action.add(jMI_Next);

		jMenuBar2.add(jM_Action);

		jM_View = new javax.swing.JMenu("View");
		// Ron 30/10/02
		jMI_Toggle_Invars = new JMenuItem("Atomic Invariants");
		jMI_Toggle_Invars.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_ToggleInvarsActionPerformed();
			}
		});
		jM_View.add(jMI_Toggle_Invars);
		jM_View.addSeparator();
		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		jMI_ZoomIn = new javax.swing.JMenuItem("Zoom In", ii);
		jMI_ZoomIn.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_ZoomInActionPerformed();
			}
		});
		jM_View.add(jMI_ZoomIn);
		/* WZ 5/9/2001 */
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

		jMI_ActionToolBar = new javax.swing.JCheckBoxMenuItem("Action Toolbar");
		jMI_ActionToolBar.setState(true);
		jMI_ActionToolBar
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jToolBar3.setVisible(jMI_ActionToolBar.getState());
				updateUI();
			}
		});
		jM_View.add(jMI_ActionToolBar);

		jMenuBar2.add(jM_View);

		setJMenuBar(jMenuBar2);

		/* 
		   add Toolbars
		*/

		jToolBar1 = new JToolBar();
		jToolBar1.setFloatable(false); /* WZ 19/10/2001 */
		// 	  jToolBar1.setRollover(true);

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Import16.gif");
		JButton bt = new GipoButton(" Load ", ii); /* WZ 5/12/2001 */
		bt.setToolTipText("Load Graphics");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_OpenActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Export16.gif");
		bt = new GipoButton(" Save ", ii); /* WZ 5/12/2001 */
		bt.setToolTipText("Save Graphics");
		bt.setToolTipText("Save as a graphics file only");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_SaveGraphicsActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
		bt = new GipoButton(" Preview ", ii); /* WZ 5/12/2001 */
		bt.setToolTipText("Print Preview");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintPreviewActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		bt = new GipoButton("Print graphics", ii); /* WZ 5/12/2001 */
		bt.setToolTipText("Print a graphicalyl formatted action sequence");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		bt = new GipoButton("Action sequence", ii); /* WZ 5/12/2001 */
		bt.setToolTipText("Display/Print a text format action sequence list");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintTextActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		bt = new GipoButton(" Close ", ii); /* WZ 5/12/2001 */
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ExitActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* jToolBar3 */
		jToolBar3 = new JToolBar();
		jToolBar3.setFloatable(false); /* WZ 19/10/2001 */

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Undo16.gif");
		bt = new JButton(" Pre ", ii);
		bt.setToolTipText("Go to the previous step");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PreActionPerformed();
			}
		});
		jToolBar3.add(bt);

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Redo16.gif");
		bt = new JButton(" Next ", ii);
		bt.setToolTipText("Go to the next step");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_NextActionPerformed();
			}
		});
		jToolBar3.add(bt);

		jToolBar3.addSeparator();

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		bt = new JButton(" Zoom In ", ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ZoomInActionPerformed();
			}
		});
		jToolBar3.add(bt);

		/* WZ 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomOut16.gif");
		bt = new JButton(" Zoom Out ", ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ZoomOutActionPerformed();
			}
		});
		jToolBar3.add(bt);

		topToolBarPanel = new JPanel();
		topToolBarPanel.setLayout(new GridLayout(0, 1));
		topToolBarPanel.add(jToolBar1);
		getContentPane().add(topToolBarPanel, "North");
		getContentPane().add(jToolBar3, "South");

		/* WZ 5/12/2001 */
		//add splitpane2 (will contain drawingCanvas and jscollOM)
		splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane2.setResizeWeight(0.8);
		splitPane2.setOneTouchExpandable(true); /* WZ 14/6/02 */
		splitPane2.setDividerSize(6);
		// 	  splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);/* WZ 24/5/02 */
		/* add drawing Canvas*/
		addDrawingCanvas();

		//add tasks panel (west panel)

		// Build the initial State Class List
		flwInitState = new FilterStatePane(curDomain,"Initial State");
		flStatics = new FilterStaticsPane(curDomain,"Atomic Invariants");
		// 	  populateCurState(curTask.getInits());	
//		if (curTask != null)
//			flwCurState.showStateProperty(curTask.getInits(), 1);
//		jlstInitState = new JList();
//		lmInitState = new DefaultListModel();
//		jlstInitState.setModel(lmInitState);
//		jlstInitState.setEnabled(false);
//		initStateList(curTask);
//		jscrollInitState = new JScrollPane(jlstInitState);
//		jscrollInitState.setBorder(
//			BorderFactory.createTitledBorder("Initial State"));
//		jlstInitState.setCellRenderer(new PredListCellRenderer());

		// Build the goal State Class List
		jlstGoalState = new JList();
		lmGoalState = new DefaultListModel();
		jlstGoalState.setModel(lmGoalState);
		jlstGoalState.setEnabled(false);

		jscrollGoalState = new JScrollPane(jlstGoalState);
		jscrollGoalState.setBorder(
			BorderFactory.createTitledBorder("Goal State"));
		// 	  jlstGoalState.setCellRenderer(new ListCellRenderer());

		// Build statics list
		JList jlstStatics = new JList();
		lmStatics = new DefaultListModel();
		jlstStatics.setModel(lmStatics);
		jlstStatics.setEnabled(false);

		JScrollPane jscrollStatics = new JScrollPane(jlstStatics);
		jscrollStatics.setBorder(BorderFactory.createTitledBorder("Statics"));
		// 	  jlstStatics.setCellRenderer(new ListCellRenderer());

		// Build temporal list
		JList jlstTemps = new JList();
		lmTemps = new DefaultListModel();
		jlstTemps.setModel(lmTemps);
		jlstTemps.setEnabled(false);

		JScrollPane jscrollTemps = new JScrollPane(jlstTemps);
		jscrollTemps.setBorder(BorderFactory.createTitledBorder("Temporal"));
		// 	  jlstTemps.setCellRenderer(new ListCellRenderer());

		//populate goals
		goalStateList(curTask);

		//add the tasks combobox
		ListIterator li = lstTask.listIterator();
		while (li.hasNext()) {
			vecTasks.addElement((oclHTNTask) li.next());
		}
		jcomTasks = new JComboBox(vecTasks);
		jcomTasks.addActionListener(taskListener);
		jcomTasks.setAlignmentY(0);
		jcomTasks.setBackground(Color.white);
		/* WZ 13/5/02 */
		JButton cmdDummy = new JButton("XXX");
		int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
		jcomTasks.setPreferredSize(new Dimension(80, txtFieldMaxHeight));

		//assembly west panel
		westPanel.setBorder(BorderFactory.createTitledBorder("Task .."));
		/* WZ 20/5/02 */
		GridBagLayout gridbag = new GridBagLayout();
		westPanel.setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridwidth = GridBagConstraints.REMAINDER; //end row
		gridbag.setConstraints(jcomTasks, c);
		// 	  westPanel.setLayout(new BoxLayout(westPanel,BoxLayout.Y_AXIS));
		westPanel.add(jcomTasks);

		/* WZ 13/5/02 */
		JPanel goalPanel = new JPanel();
		goalPanel.setLayout(new GridLayout(0, 1));
		goalPanel.add(jscrollStatics);
		goalPanel.add(jscrollGoalState);
		goalPanel.add(jscrollTemps);

		staskPanel =
			new JSplitPane(
				JSplitPane.HORIZONTAL_SPLIT,
				flwInitState,  // Ron use filter window
				goalPanel);
		staskPanel.setDividerSize(6);
		staskPanel.setOneTouchExpandable(true);
		staskPanel.setDividerLocation(0.5);
		staskPanel.setResizeWeight(0.5);

		c.weighty = 1.0;
		gridbag.setConstraints(staskPanel, c);
		westPanel.add(staskPanel);
		/* end 13/5/02 */

		// Build the Operator List
		// 	  showOperatorLists();

		//build current state List
		flwCurState = new FilterStatePane(curDomain,"Current State");
		// 	  populateCurState(curTask.getInits());	
		if (curTask != null)
			flwCurState.showStateProperty(curTask.getInits(), 1);
		
//		jscrollCurState.setBorder(
//			BorderFactory.createTitledBorder("Current State"));
		
		// 	  splitPane2.add(jlstCurState);

		splitPane2.setRightComponent(flwCurState);
		//splitPane2.setRightComponent(jscrollCurState);

		/* WZ 5/12/2001 */
		splitPane1 =
			new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, westPanel, splitPane2);
		// 	  splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, westPanel, drawingCanvas);
		// 	  splitPane1.add(westPanel);/* WZ 24/5/02 */
		splitPane1.setResizeWeight(0.2);
		splitPane1.setOneTouchExpandable(true);
		splitPane1.setDividerSize(6);
		getContentPane().add(splitPane1, "Center");

		//Initiate varibles
		viewSize = scrollPanel.getViewport().getViewSize();
	}

	private ActionListener taskListener = new ActionListener() {
		public void actionPerformed(ActionEvent evt) {
			if (drawingCanvas.getShapeCount() > 2) { /* WZ 14/6/02 */
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
			populateTasks();
		}
	};

	/**
	 * add drawing canvas
	 * 
	 */
	private void addDrawingCanvas() {
		drawingCanvas = new HStepperCanvas(this);
		drawingCanvas.setWorkingDomain(this.curDomain);
		//pass the working domain
		scrollPanel = new JScrollPane(drawingCanvas);

		/* WZ 5/12/2001 */
		// 	  splitPane2.add(scrollPanel);	
		splitPane2.setLeftComponent(scrollPanel); /* WZ 6/6/02 */
		// 	  splitPane1.setRightComponent(scrollPanel);
	}

	/* WZ 3/07/2001 */
	/**
	 * show task contents on the property window
	 * 
	 */
	private void populateTasks() {
		if (curTask != null) {
			refreshDrawingWindow();
			curTask = (oclHTNTask) jcomTasks.getSelectedItem();
			initStateList(curTask);
			goalStateList(curTask);
			jMI_AddInitialStateActionPerformed();
			updateUI();
		}
	}

	/* WZ 3/07/2001 */
	/**
	 * initialise the task list
	 * 
	 */
	public void initTaskList() {
		if (jcomTasks.getItemCount() > 0) {
			jcomTasks.setSelectedIndex(0);
			curTask = (oclHTNTask) jcomTasks.getItemAt(0);
			populateTasks();
		}
	}

	/**
	 * show init states (oclSS)
	 * @param task oclHTNTask
	 */
	private void initStateList(oclHTNTask task) {
		if (task != null) {
			initState.clear();
			ListIterator li = task.getInits().listIterator();
			while (li.hasNext()) {
				oclSS ss = (oclSS) li.next();
				initState.add(ss);
			}
			flwInitState.showStateProperty(task.getInits(), 1); //oclSS
		}
	}
	
		/**
	  * show states to a JList.
	  * @param lmState - stateModel destination (DefaultListModel)
	  * @param stateList input
	  * @param mySwitch switch between oclSS and oclSE
	  * 
	  */
	public void showStateProperty(DefaultListModel lmState,
		List stateList,int mySwitch) {
		lmState.clear();
		ListIterator li = stateList.listIterator();
		while (li.hasNext()) {
			if (mySwitch == 1) {
				oclSS ss = (oclSS) li.next();
				lmState.addElement(ss);
			} else if (mySwitch == 2) {
				oclSE se = (oclSE) li.next();
				lmState.addElement(se);
			}
		}
		updateUI();
	}

	/**
	 * show goal states (oclSS)
	 * @param task oclHTNTask
	 * 
	 */
	private void goalStateList(oclHTNTask task) {
		if (task != null) {
			lmGoalState.clear();
			lmStatics.clear();
			lmTemps.clear();
			// Build the goal State Class List
			ListIterator li = task.getGoalLists().listIterator();
			while (li.hasNext()) {
				lmGoalState.addElement(li.next());
			}
			// Build statics list
			li = task.getStatics().listIterator();
			while (li.hasNext()) {
				oclPredicate statics = (oclPredicate) li.next();
				lmStatics.addElement(statics);
				Utility.debugPrintln("Statics -- " + statics.toString());
			}
			// Build temporal list
			li = task.getTemps().listIterator();
			while (li.hasNext()) {
				oclPredicate tmps = (oclPredicate) li.next();
				lmTemps.addElement(tmps);
				Utility.debugPrintln("tmps -- " + tmps.toString());
			}
			updateUI();
		}
	}

	/* WZ 5/6/02 */
	/**
	 * @param curState
	 */
	public void updateCurState(List curState) {
		flwCurState.showStateProperty(curState, 1);
	}

	/* WZ 6/6/02 */
	/**
	 * Change the focus of the scollPane
	 * @param newRateH
	 * @param newRateV
	 * 
	 */
	public void focus(double newRateH, double newRateV) {
		// 	Utility.debugPrintln("getVerticalScrollBar() -- "+scrollPanel.getVerticalScrollBar().getValue());
		// 	Utility.debugPrintln("getMaximum() -- "+scrollPanel.getVerticalScrollBar().getMaximum());
		// 	Utility.debugPrintln("getMaximum() -- "+scrollPanel.getVerticalScrollBar().getMinimum());
		// 	Utility.debugPrintln("newRateH -- "+newRateH);
		// 	Utility.debugPrintln("newRateV -- "+newRateV);
		int newValueV =
			(int) (newRateV
				* (scrollPanel.getVerticalScrollBar().getMaximum()
					- scrollPanel.getVerticalScrollBar().getMinimum()));
		// 	Utility.debugPrintln("new Value -- "+newValueV);
		scrollPanel.getVerticalScrollBar().setValue(newValueV);

		int newValueH =
			(int) (newRateH
				* (scrollPanel.getHorizontalScrollBar().getMaximum()
					- scrollPanel.getHorizontalScrollBar().getMinimum()));
		scrollPanel.getHorizontalScrollBar().setValue(newValueH);

		updateUI();
	}



	/*
	 * show operators in a JLists
	 * @return File
	 */
	private void showOperatorLists() {
		//show primitive operators
		jlstOP = new JList();
		lmOP = new DefaultListModel();
		jlstOP.setToolTipText(
			"Double click the operator to view the graphical display");
		jlstOP.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListIterator li = lstOP.listIterator();
		while (li.hasNext()) {
			lmOP.addElement((oclOperator) li.next());
		}
		jlstOP.setModel(lmOP);
		jlstOP.setSelectedIndex(0);
		jlstOP.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent me) {
				if (me.getClickCount() == 2) {
					int i = jlstOP.locationToIndex(me.getPoint());
					oclOperator op = (oclOperator) jlstOP.getSelectedValue();
					try {
						OperatorInstantiation pw =
							new OperatorInstantiation(
								curDomain,
								(oclOperator) op.clone(),
								drawingCanvas,
								top.strImageDir);
						pw.setLocation(
							(int) (0.5 * getWidth()),
							(int) (0.5 * getHeight()));
						pw.show();
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln(e);
					}
					seleIndex = jlstOP.getSelectedIndex();
				}
			}
		});

		JScrollPane jscrollOP = new JScrollPane(jlstOP);
		jscrollOP.setBorder(BorderFactory.createTitledBorder("Primitive"));

		//show primitive operators
		jlstMD = new JList();
		lmMD = new DefaultListModel();
		jlstMD.setToolTipText("Double click to view the graphical display");
		jlstMD.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		li = lstMD.listIterator();
		while (li.hasNext()) {
			lmMD.addElement((oclMethod) li.next());
		}
		jlstMD.setModel(lmMD);
		jlstMD.setSelectedIndex(0);
		jlstMD.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent me) {
				int i = jlstMD.locationToIndex(me.getPoint());
				oclMethod md = (oclMethod) jlstMD.getSelectedValue();
				if (me.getClickCount() == 2) {
					try {
						MethodInstantiation pw =
							new MethodInstantiation(
								curDomain,
								(oclMethod) md.clone(),
								drawingCanvas,
								top.strImageDir);
						pw.setLocation(
							(int) (0.5 * getWidth()),
							(int) (0.5 * getHeight()));
						pw.show();
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln(e);
					}
					seleIndex = jlstMD.getSelectedIndex();
				}

				/* WZ 17/5/02 right click to show property description */
				if (me.getModifiers() == MouseEvent.BUTTON3_MASK) {
					String dec = md.getDescription();
					if (dec.trim().equals(new String()))
						dec = "No decription for this method.";
					JOptionPane.showMessageDialog(
						top,
						dec,
						"METHOD - " + md.getName().getName(),
						JOptionPane.PLAIN_MESSAGE);
				}
			}
		});

		JScrollPane jscrollMD = new JScrollPane(jlstMD);
		jscrollMD.setBorder(BorderFactory.createTitledBorder("Compound"));

		jPanelOM = new JPanel();
		jPanelOM.setLayout(new GridLayout(0, 1));
		jPanelOM.add(jscrollOP);
		//if (top.hierarchicalSwitch) /* Weihong 4/3/02 */
		if (curDomain.isHierarchical())
			jPanelOM.add(jscrollMD);

		jPanelOM.setBorder(BorderFactory.createTitledBorder("Operators List"));
		/* WZ 28/11/2001 */
		// 	splitPane2.add(jPanelOM);
		// 	splitPane2.setRightComponent(jPanelOM);
	}

	/**
	 * refresh state list after anychange
	 * 
	 */
	//     private void refreshStateList(){
	// 	splitPane2.remove(jPanelOM);	/* WZ 5/12/2001 */
	// 	showOperatorLists();
	// 	updateUI();
	//     }

	/**
	 * zoom out
	 * 
	 */
	private void jMI_ZoomOutActionPerformed() {
		viewSize =
			new Dimension(
				(int) (viewSize.width / zoom),
				(int) (viewSize.height / zoom));
		drawingCanvas.setScale(1 / zoom);
	}
	
	/**
	 * Toggle between the Initial State and The atomic invars
	 */
	private void jMI_ToggleInvarsActionPerformed(){
		if (!showInvars) {
			showInvars = true;
			staskPanel.setLeftComponent(flStatics);
			jMI_Toggle_Invars.setText("Initial State");
		} else {
			showInvars = false;
			staskPanel.setLeftComponent(flwInitState);
			jMI_Toggle_Invars.setText("Atomic Invariants");
		}
	}
	/**
	 * zoom in
	 * 
	 */
	private void jMI_ZoomInActionPerformed() {
		viewSize =
			new Dimension(
				(int) (viewSize.width * zoom),
				(int) (viewSize.height * zoom));
		drawingCanvas.setScale(zoom);
	}

	/* WZ 27/6/02 */
	/**
	 * print preview
	 * 
	 */
	private void jMI_PrintTextActionPerformed() {
		ActionSequence.showActionSequence(
			top,
			drawingCanvas.getCurActionList(),
			curTask,
			curDomain);
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

	/* Weihong 14/3/02 */
	/* 
	 * Print the action sequence (Probably) to the printer
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		int i = 0;
		ps.println("Stepper - action sequence");
		ps.println("Task ID - " + curTask);
		ps.println();
		ListIterator li = drawingCanvas.getCurActionList().listIterator();
		while (li.hasNext()) {
			i++;
			oclPredicate pred = (oclPredicate) li.next();
			ps.println(i + ": " + pred.toString());
		}
	}

	/**
	 * print the graphics
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
	 * draw initial and goal states on the canvas
	 * 
	 */
	private void jMI_AddInitialStateActionPerformed() {
		if (initState.size() > 0 && lmGoalState.size() > 0) {
			/* WZ 24/5/02 */
			List listInit = new ArrayList();
			for (int i = 0; i < initState.size(); i++) {
				oclSS ss = (oclSS) initState.get(i);
				listInit.add(ss);
			}
			drawingCanvas.setCurrentState(listInit);
			updateCurState(listInit);
			/* end 24/5/02 */

			jplan.graphics.gTool.Graphics.vShape vs =
				drawingCanvas.drawTaskShape(curTask);

			//show decompositions on the canvas
			drawingCanvas.showDecomposition(vs);
		} else {
			JOptionPane.showMessageDialog(
				top,
				"No current states.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
	}

	/**
	 * redo the undo action
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
	 * load a graphics file .vm
	 * 
	 */
	private void jMI_OpenActionPerformed() {
		if (drawingCanvas.getShapeCount() > 2) { /* WZ 14/6/02 */
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
	 * read file from a BufferedReader
	 * @param br BufferedReader
	 * 
	 */
	private void readFile(BufferedReader br) {
		refreshDrawingWindow();
		drawingCanvas.loadFile(br);
		/* WZ 20/6/02 */
		String taskName = drawingCanvas.getShape(1).getLabel();
		for (int i = 0; i < jcomTasks.getItemCount(); i++) {
			oclHTNTask task = (oclHTNTask) jcomTasks.getItemAt(i);
			if (taskName.equals(task.toString())) {
				curTask = task;
				jcomTasks.removeActionListener(taskListener);
				jcomTasks.setSelectedIndex(i);
				jcomTasks.addActionListener(taskListener);
				initStateList(curTask);
				goalStateList(curTask);
				updateUI();
			}
		}
		/* end 20/6/02 */
	}

	/**
	 * show next states after executing an operator
	 * 
	 */
	private void jMI_NextActionPerformed() { // to show the operator
		drawingCanvas.startStepper();
	}

	/**
	 * save graphics as a new .vm file
	 * 
	 */
	private void jMI_SaveAsGraphicsActionPerformed() {
		//JFileChooser chooser = new JFileChooser();
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
		setVisible(false);
		dispose();
	}

	/**
	 * go to the previous step (states)
	 * 
	 */
	private void jMI_PreActionPerformed() {
		try {
			drawingCanvas.undo();
		} catch (CannotUndoException ex) {
			JOptionPane.showMessageDialog(
				this,
				"Unable to undo: " + ex,
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
	}

	/**
	 * save graphics as .vm file
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
	 * reset drawing canvas to its initial state
	 * 
	 */
	private void refreshDrawingWindow() {
		int ww, hh;
		ww = getSize().width;
		hh = getSize().height;
		splitPane1.remove(scrollPanel); /* WZ 5/12/2001 */

		//add Drawing Canvas
		addDrawingCanvas();

		initState.clear();
		lmGoalState.clear();
		updateUI();

		//set the tempFile to null
		tempFile = null;

		setVisible(false);
		pack();
		setSize(ww, hh);
		setVisible(true);
	}

	/**
	 * save .vm file
	 * @param ps PrintStream
	 * 
	 */
	private void saveVMFile(PrintStream ps) {

		ps.println("******** HIERARCHICAL STEPPER ********\n");
		ps.println("AUTHOR:" + curDomain.getAuthor() + "\n");
		ps.println(
			"TIME:"
				+ java.util.Calendar.getInstance().getTime().toString()
				+ "\n");
		ps.println("DOMAIN:" + curDomain.getName() + "\n");
		ps.println("TASK:" + curTask.toString() + "\n");
		ps.println("***************************************\n");
		// 	ps.println("\n");
		// 	ps.println("BEGIN WINDOW VARIBLES\n");
		// // 	ps.println("viewSize:" +viewSize.toString() + "\n");
		// 	ps.println("END WINDOW VARIBLES\n");
		// 	ps.println("\n");
		ps.println(drawingCanvas.to_String());
		ps.println("\n");

		ps.close();
	}

}

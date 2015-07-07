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

package jplan.tools.animator;

/*
 * Animator Graphical Display Window
 * @author Weihong Zhao
 * 5/7/2001
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

import jplan.general.OPredicate;
import jplan.graphics.PrintPreview;
import jplan.graphics.gTool.Windows.vFilter;
import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.edexpt.PredListCellRenderer;
import jplan.images.ImageLoader; /* Weihong changed on 5/9/2001 */
import jplan.general.Utility;
import jplan.general.GipoInternalFrame;/* Weihong added on 24/10/2001 */
import jplan.tools.stepper.StepperCanvas; /* Weihong added on 1/11/01 */
import jplan.general.GipoButton;/* Weihong added on 5/12/2001 */

/**
 * As a interface AnimatorWindow automatically shows the graphical
 * representation of the planning result step by step.
 */
public class AnimatorWindow extends GipoInternalFrame {/*
													    * Weihong added on
													    * 24/10/2001
													    */
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

	private JMenu jM_Action;

	private JMenuItem jMI_Pre;

	private JMenuItem jMI_Next;

	private JMenu jM_View;

	private JMenuItem jMI_ZoomOut;

	private JMenuItem jMI_ZoomIn;

	private JCheckBoxMenuItem jMI_FileToolbar;

	private JCheckBoxMenuItem jMI_ActionToolBar;

	private JCheckBoxMenuItem jMI_EditingStatesList;

	private JCheckBoxMenuItem jMI_EditingOperatorsList;

	private JScrollPane scrollPanel;

	private StepperCanvas drawingCanvas; /* Weihong added on 1/11/01 */

	private Dimension viewSize;

	private double zoom = 1.5;

	private JFileChooser chooser = new JFileChooser();

	private File tempFile; //for managing the i/o

	private int curStep = -1;

	private oclOperator curOperator = null;

	private List lstTask; // This is all thetasks

	/**
	 * parent frame
	 */
	private OclEd top; /* Weihong added on 1/11/01 */

	private oclDomain curDomain;

	private List lstOM; //Methods or Operators

	private DefaultListModel lmOM;

	private JList jlstOM;

	private JScrollPane jscrollOM;

	private JPanel westPanel = new JPanel(); //panel to place at west

	// The tasks
	private JComboBox jcomTasks;

	private Vector vecTasks = new Vector();

	private oclTask curTask = null;

	private oclTask thisTask = null;

	private String taskID = null;

	private List lstOp = new ArrayList();

	// The initial state classes list box
	private JList jlstInitState;

	private DefaultListModel lmInitState;

	private JScrollPane jscrollInitState;

	private int seleIndex; //the selected index of operators list, to use for
						   // saving the existing operator into the same
						   // pisition

	// The goal state classes list box
	private JList jlstGoalState;

	private DefaultListModel lmGoalState;

	private JScrollPane jscrollGoalState;

	private JSplitPane splitPane1, splitPane2; /* Weihong added on 5/12/2001 */

	//     /* WZ 19/7/02 */
	//     private JList jlstMD;
	//     private DefaultListModel lmMD;
	//     /* end 19/7/02 */

	/**
	 * Create an instance of AnimatorWindow
	 * 
	 * @param title
	 *            String title
	 * @param curDomain
	 *            oclDomain
	 * @param parent
	 *            parent frame
	 */
	public AnimatorWindow(String title, oclDomain curDomain, OclEd parent) {
		super(parent); /* Weihong added on 24/10/2001 */
		setTitle(title);
		setClosable(false); /* Weihong added on 12/10/2001 */
		this.curDomain = curDomain;
		top = parent;

		if (curDomain == null) {
			JOptionPane.showMessageDialog(top,
					"No Domain currently being edited.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}

		Utility.debugPrintln("loading planning result...");
		lstOM = new ArrayList();

		loadPlanningResult();

		lstTask = new ArrayList(); //Need to clone the tasks
		// Ron 5/5/03 use domain record
		if (!curDomain.isHierarchical()) {
			ListIterator li = curDomain.tasks.listIterator();
			while (li.hasNext()) {
				oclTask task = (oclTask) li.next();
				if (task.ID.equals(taskID))
					lstTask.add(task);
			}
		} else {/* WZ 18/7/02 */
			ListIterator li = curDomain.htntasks.listIterator();
			while (li.hasNext()) {
				oclHTNTask task = (oclHTNTask) li.next();
				if (task.ID.equals(taskID))
					lstTask.add(task);
			}
		}

		tempFile = null;
		initComponents();

		setStep(-1); //initial

		pack();
		updateUI();
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

		/* Weihong changed/added on 5/9/2001 */
		ImageIcon ii = ImageLoader
				.getImageIcon(top.strImageDir, "Import16.gif");
		jMI_Open = new javax.swing.JMenuItem("Load Graphics", ii);
		jMI_Open.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_OpenActionPerformed();
			}
		});
		jM_File.add(jMI_Open);

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Export16.gif");
		jMI_SaveGraphics = new javax.swing.JMenuItem("Save Graphics", ii);
		jMI_SaveGraphics.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_SaveGraphicsActionPerformed();
			}
		});
		jM_File.add(jMI_SaveGraphics);

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "SaveAs16.gif");
		jMI_SaveAsGraphics = new javax.swing.JMenuItem("Save As New Graphics",
				ii);
		jMI_SaveAsGraphics
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						jMI_SaveAsGraphicsActionPerformed();
					}
				});
		jM_File.add(jMI_SaveAsGraphics);

		jM_File.addSeparator();

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
		jMI_PrintPreview = new javax.swing.JMenuItem("Print Preview", ii);
		jMI_PrintPreview.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintPreviewActionPerformed();
			}
		});
		jM_File.add(jMI_PrintPreview);

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		jMI_Print = new javax.swing.JMenuItem("Print", ii);
		jMI_Print.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PrintActionPerformed();
			}
		});
		jM_File.add(jMI_Print);

		jM_File.addSeparator();

		/* Weihong changed/added on 5/9/2001 */
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

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "StepBack16.gif");
		jMI_Pre = new javax.swing.JMenuItem("Pre Step", ii);
		jMI_Pre.setToolTipText("Go to the previous step");
		jMI_Pre.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_PreActionPerformed();
			}
		});
		jM_Action.add(jMI_Pre);

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "StepForward16.gif");
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

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		jMI_ZoomIn = new javax.swing.JMenuItem("Zoom In", ii);
		jMI_ZoomIn.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jMI_ZoomInActionPerformed();
			}
		});
		jM_View.add(jMI_ZoomIn);
		/* Weihong changed/added on 5/9/2001 */
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
		 * add Toolbars
		 */

		jToolBar1 = new JToolBar();
		jToolBar1.setFloatable(false); /* Weihong added on 12/10/2001 */

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Import16.gif");
		JButton bt = new GipoButton(" Load ", ii);/*
												   * Weihong changed on
												   * 5/12/2001
												   */
		bt.setToolTipText("Load Graphics");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_OpenActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Export16.gif");
		bt = new GipoButton(" Save ", ii);/* Weihong changed on 5/12/2001 */
		bt.setToolTipText("Save Graphics");
		bt.setToolTipText("Save as a graphics file only");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_SaveGraphicsActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
		bt = new GipoButton(" Preview ", ii);/* Weihong changed on 5/12/2001 */
		bt.setToolTipText("Print Preview");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintPreviewActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
		bt = new GipoButton(" Print ", ii);/* Weihong changed on 5/12/2001 */
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PrintActionPerformed();
			}
		});
		jToolBar1.add(bt);

		jToolBar1.addSeparator();

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
		bt = new GipoButton(" Close ", ii);/* Weihong changed on 5/12/2001 */
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ExitActionPerformed();
			}
		});
		jToolBar1.add(bt);

		/* jToolBar3 */
		jToolBar3 = new JToolBar();
		jToolBar3.setFloatable(false); /* Weihong added on 12/10/2001 */

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "StepBack16.gif");
		bt = new JButton(" Pre ", ii);
		bt.setToolTipText("Go to the previous step");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_PreActionPerformed();
			}
		});
		jToolBar3.add(bt);

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "StepForward16.gif");
		bt = new JButton(" Next ", ii);
		bt.setToolTipText("Go to the next step");
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_NextActionPerformed();
			}
		});
		jToolBar3.add(bt);

		jToolBar3.addSeparator();

		/* Weihong changed/added on 5/9/2001 */
		ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
		bt = new JButton(" Zoom In ", ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jMI_ZoomInActionPerformed();
			}
		});
		jToolBar3.add(bt);

		/* Weihong changed/added on 5/9/2001 */
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

		/* Weihong added on 5/12/2001 */
		//add splitpane2 (will contain drawingCanvas and jscollOM)
		splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane2.setResizeWeight(0.8);
		splitPane2.setOneTouchExpandable(true);
		splitPane2.setDividerSize(4);/* WZ 8/7/02 */

		/* add drawing Canvas */
		addDrawingCanvas();

		//add tasks panel (west panel)

		// Build the initial State Class List
		jlstInitState = new JList();
		lmInitState = new DefaultListModel();
		jlstInitState.setModel(lmInitState);
		jlstInitState.setEnabled(false);
		initStateList(curTask);
		jscrollInitState = new JScrollPane(jlstInitState);
		jscrollInitState.setBorder(BorderFactory
				.createTitledBorder("Initial State"));
		jlstInitState.setCellRenderer(new PredListCellRenderer());

		// Build the goal State Class List
		jlstGoalState = new JList();
		lmGoalState = new DefaultListModel();
		jlstGoalState.setModel(lmGoalState);
		jlstGoalState.setEnabled(false);
		initStateList(curTask);
		jscrollGoalState = new JScrollPane(jlstGoalState);
		jscrollGoalState.setBorder(BorderFactory
				.createTitledBorder("Goal State"));
		jlstGoalState.setCellRenderer(new PredListCellRenderer());

		//add the tasks combobox
		//	  ron 5/5/03 use domain record
		if (!curDomain.isHierarchical()) {
			ListIterator li = lstTask.listIterator();
			while (li.hasNext()) {
				vecTasks.addElement((oclTask) li.next());
			}
		} else {/* WZ 19/7/02 */
			ListIterator li = lstTask.listIterator();
			while (li.hasNext()) {
				vecTasks.addElement((oclHTNTask) li.next());
			}
		}
		jcomTasks = new JComboBox(vecTasks);
		initTaskList();
		jcomTasks.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				populateTasks();
			}
		});
		jcomTasks.setAlignmentY(0);
		jcomTasks.setBackground(Color.white);
		/* WZ 8/7/02 */
		JButton cmdDummy = new JButton("XXX");
		int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
		jcomTasks.setMaximumSize(new Dimension(1000, txtFieldMaxHeight));

		//assembly west panel
		westPanel.setBorder(BorderFactory.createTitledBorder("Task .."));
		westPanel.setLayout(new BoxLayout(westPanel, BoxLayout.Y_AXIS));
		westPanel.add(jcomTasks);
		westPanel.add(jscrollInitState);
		westPanel.add(jscrollGoalState);

		// Build the Operator List
		showOperatorLists();

		/* Weihong added on 5/12/2001 */
		splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, westPanel,
				splitPane2);
		splitPane1.setResizeWeight(0.15);
		splitPane1.setOneTouchExpandable(true);
		splitPane1.setDividerSize(4);/* WZ 8/7/02 */
		getContentPane().add(splitPane1, "Center");

		//Initiate varibles
		viewSize = scrollPanel.getViewport().getViewSize();
		Utility.debugPrintln("scrollPanel.getViewport().getViewSize()"
				+ scrollPanel.getViewport().getViewSize());
	}

	/**
	 * add drawing canvas
	 *  
	 */
	private void addDrawingCanvas() {
		drawingCanvas = new StepperCanvas(this);/* Weihong added on 1/11/01 */
		drawingCanvas.setWorkingDomain(this.curDomain); //pass the working
														// domain
		scrollPanel = new JScrollPane(drawingCanvas);
		scrollPanel.setBorder(BorderFactory
				.createTitledBorder("Editing/Drawing Canvas"));
		/* Weihong added on 5/12/2001 */
		splitPane2.add(scrollPanel);
		splitPane2.setLeftComponent(scrollPanel);
	}

	/* Weihong added on 3/07/2001 */
	/**
	 * show task contents on the property window
	 *  
	 */
	private void populateTasks() {
		if (curTask != null) {
			refreshDrawingWindow();
			curTask = (oclTask) jcomTasks.getSelectedItem();
			initStateList(curTask);
			goalStateList(curTask);
			jMI_AddInitialStateActionPerformed();
			updateUI();
		}
	}

	/* Weihong added on 3/07/2001 */
	/**
	 * initialise the task list
	 *  
	 */
	private void initTaskList() {
		if (jcomTasks.getItemCount() > 0) {
			jcomTasks.setSelectedIndex(0);
			curTask = (oclTask) jcomTasks.getItemAt(0);
			initStateList(curTask);
			goalStateList(curTask);
			updateUI();
		}
	}

	/**
	 * show init states (oclSS)
	 * 
	 * @param task
	 *            oclTask
	 */
	private void initStateList(oclTask task) {
		if (task != null) {
			showStateProperty(lmInitState, task.getInits(), 1); //oclSS
		}
	}

	/**
	 * show goal states (oclSS)
	 * 
	 * @param task
	 *            oclTask
	 *  
	 */
	private void goalStateList(oclTask task) {
		if (task != null) {
			showStateProperty(lmGoalState, task.getGoals(), 2); //oclSS
		}
	}

	/**
	 * show goal states (oclSS)
	 * 
	 * @param t -
	 *            task oclTask
	 *  
	 */
	private void setStep(int t) {
		curStep = t;
		jlstOM.setSelectedIndex(curStep);
		curOperator = (oclOperator) jlstOM.getSelectedValue();
	}

	/**
	 * show states to a JList.
	 * 
	 * @param stateModel
	 *            destination (DefaultListModel)
	 * @param stateList
	 *            input
	 * @param mySwitch
	 *            switch between oclSS and oclSE
	 *  
	 */
	public void showStateProperty(DefaultListModel stateModel, List stateList,
			int mySwitch) {
		stateModel.clear();
		ListIterator li = stateList.listIterator();
		while (li.hasNext()) {
			if (mySwitch == 1) {
				oclSS ss = (oclSS) li.next();
				stateModel.addElement(ss);
			} else if (mySwitch == 2) {
				oclSE se = (oclSE) li.next();
				stateModel.addElement(se);
			}
		}
		updateUI();
	}

	/*
	 * load planning result
	 *  
	 */
	private void loadPlanningResult() {
		File pResultFile = new File(top.strOCLPath + File.separator + "tmp"
				+ File.separator + "run.txt");
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(
					new FileInputStream(pResultFile)));
			if (in.ready()) {
				/* Weihong changed/added on 18/9/2001 */
				in.mark(1);
				String str = in.readLine();
				Utility.debugPrintln("str ===>" + str);
				if (str.startsWith("TASK")) {
					Utility.debugPrintln("Read planning result.");
					in.reset();
					readPlanningResult(in);
				} else {
					Utility.debugPrintln("<<else>>");
					File fis = translateResult(in);
					BufferedReader another = new BufferedReader(
							new InputStreamReader(new FileInputStream(fis)));
					if (another.ready()) {
						readPlanningResult(another);
					}
					another.close();
				}
				/* Weihong changed/added on 18/9/2001 end */
			}

			in.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

	/*
	 * translate Result from FF format @param br BufferedReader @return File
	 */
	private File translateResult(BufferedReader br) {
		String tmpStr = null;
		PrintStream ps = null;
		File returnFile = new File(top.strOCLPath + File.separator + "tmp"
				+ File.separator + "test.txt");
		String str = "";
		boolean started = false;
		try {
			ps = new PrintStream(new FileOutputStream(returnFile));
		} catch (IOException ex) {
		}
		while (true) {
			try {
				str = br.readLine();
				if (str.startsWith("problem")) {
					tmpStr = str.substring(str.indexOf("TASK"));
					String ID = tmpStr.substring(tmpStr.indexOf("TASK") + 4,
							tmpStr.indexOf("'"));
					ps.println("TASK " + ID);
					ps.println("SOLUTION");
					started = true;
				}

				if (str.startsWith("step")) {
					while (started && str.trim().length() != 0) {
						tmpStr = str.substring(str.indexOf(":") + 2);
						int position = tmpStr.indexOf(" ");
						String s = null;
						s = tmpStr.substring(0, position);
						ps.print(s.toLowerCase() + "(");
						tmpStr = tmpStr.substring(position + 1);
						while (tmpStr.trim().length() != 0) {
							position = tmpStr.indexOf(" ");
							if (position == -1) {
								s = tmpStr;
								tmpStr = "";
								ps.print(s.toLowerCase());
							} else {
								s = tmpStr.substring(0, position);
								tmpStr = tmpStr.substring(position + 1);
								ps.print(s.toLowerCase() + ",");
							}
						}
						ps.println(")");
						str = br.readLine();
					}
					ps.println("END FILE");
					return returnFile;
				}
			} catch (java.io.IOException ex) {
				Utility.debugPrintln(ex);
			}
		}
	}

	/*
	 * load planning result @param br BufferedReader
	 *  
	 */
	private void readPlanningResult(BufferedReader br) {
		String str = "";
		boolean opList = false;
		int j = 0;
		oclOperator op = null;

		while (true) {
			try {
				str = br.readLine();
				if (str.equals("END FILE"))
					return;
				Utility.debugPrintln(str);
				if (str.startsWith("TASK")) {
					taskID = str.substring(str.indexOf("TASK ") + 5);
				}
				if (str.equals("SOLUTION"))
					opList = true;

				if (opList) {
					//for the operators list
					if (j > 0) {
						String opName = str.substring(0, str.indexOf("("));
						//check the list from the curdomain's operator list and
						// get the right op
						ListIterator li = curDomain.operators.listIterator();
						while (li.hasNext()) {
							oclOperator temOP = (oclOperator) li.next();
							oclPredicate temPred = (oclPredicate) temOP.opName;
							if (temPred.getName().equalsIgnoreCase(opName)) {
								try {
									op = (oclOperator) temOP.clone();
								} catch (CloneNotSupportedException e) {
									Utility.debugPrintln(e);
								}
								break;
							}
						}
						if (op != null) {
							//instantiate this operator
							//first get the signature
							oclPredicate signaturePred = (oclPredicate) curDomain
									.createOperatorSignature(op);

							//then instantiate it to get the desire operator
							String argStr = str.substring(opName.length() + 1,
									str.indexOf(")"))
									+ ",";
							for (int i = 0; i < signaturePred.size(); i++) {
								if (i > 0) {
									argStr = argStr.substring(argStr
											.indexOf(",") + 1);
								}
								String newStr = argStr.substring(0, argStr
										.indexOf(","));
								try {
									OPredicate.pArg selectedPArg = signaturePred
											.pArgOf(i);
									op
											.replaceVariableName(selectedPArg,
													newStr);
								} catch (Exception e) {
									Utility
											.debugPrintln("Failed to replace var Name"
													+ e);
								}
							}
							Utility.debugPrintln(">>>>>>>>> "
									+ op.opName.toString());
							//finally add this instantiated operator to the
							// return list
							lstOM.add(op);
						} else {
							Utility
									.debugPrintln(" !!!! Problems with obtaining an operator. !!!!");
						}
					}
					j++;
				}
			} catch (java.io.IOException ex) {
				Utility.debugPrintln(ex);
			}
			;
		}
	}

	/*
	 * show operators in a JLists @return File
	 */
	private void showOperatorLists() {
		jlstOM = new JList();
		lmOM = new DefaultListModel();
		jlstOM.setToolTipText("Double click to view property");
		jlstOM.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListIterator li = lstOM.listIterator();
		while (li.hasNext()) {
			Utility.debugPrintln(">>>>>>.");
			lmOM.addElement((oclOperator) li.next());
		}
		jlstOM.setModel(lmOM);
		jlstOM.setSelectedIndex(0);
		jlstOM.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent me) {
				if (me.getClickCount() == 2) {
					int i = jlstOM.locationToIndex(me.getPoint());
					oclOperator op = (oclOperator) jlstOM.getSelectedValue();
					try {
						OperatorProperty pw = new OperatorProperty(curDomain,
								(oclOperator) op.clone(), AnimatorWindow.this,
								top.strImageDir);
						pw.setLocation((int) (0.5 * getWidth()),
								(int) (0.5 * getHeight()));
						pw.show();
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln(e);
					}
					seleIndex = jlstOM.getSelectedIndex();
				}
			}
		});

		jscrollOM = new JScrollPane(jlstOM);
		jscrollOM.setBorder(BorderFactory.createTitledBorder("Operators List"));

		splitPane2.add(jscrollOM);
		splitPane2.setRightComponent(jscrollOM);
	}

	/**
	 * refresh state list after anychange
	 *  
	 */
	private void refreshStateList() {
		splitPane2.remove(jscrollOM); /* Weihong added on 5/12/2001 */
		showOperatorLists();
		updateUI();
	}

	/**
	 * zoom out
	 *  
	 */
	private void jMI_ZoomOutActionPerformed() {
		viewSize = new Dimension((int) (viewSize.width / zoom),
				(int) (viewSize.height / zoom));
		/* Weihong added on 26/10/2001 */
		// 	scrollPanel.getViewport().setViewSize(viewSize);
		// 	drawingCanvas.reshape(0,0,viewSize.width, viewSize.height);
		/* end Weihong added on 26/10/2001 */
		drawingCanvas.setScale(1 / zoom);

		Utility.debugPrintln("Viewport Size: "
				+ scrollPanel.getViewport().getViewSize());
		Utility.debugPrintln("Drawing Canvas Size: " + drawingCanvas.getSize());
		Utility.debugPrintln("Max View Size: "
				+ scrollPanel.getHorizontalScrollBar().getMaximum());
		Utility.debugPrintln("Visible View Size: "
				+ scrollPanel.getHorizontalScrollBar().getVisibleAmount()
				+ "\n");
	}

	/**
	 * zoom in
	 *  
	 */
	private void jMI_ZoomInActionPerformed() {
		viewSize = new Dimension((int) (viewSize.width * zoom),
				(int) (viewSize.height * zoom));
		/* Weihong added on 26/10/2001 */
		// 	drawingCanvas.reshape(0,0,viewSize.width, viewSize.height);
		/* end Weihong added on 26/10/2001 */
		drawingCanvas.setScale(zoom);
		Utility.debugPrintln("Viewport Size: "
				+ scrollPanel.getViewport().getViewSize());
		Utility.debugPrintln("Drawing Canvas Size: " + drawingCanvas.getSize());
		Utility.debugPrintln("Max View Size: "
				+ scrollPanel.getHorizontalScrollBar().getMaximum());
		Utility.debugPrintln("Visible View Size: "
				+ scrollPanel.getHorizontalScrollBar().getVisibleAmount()
				+ "\n");
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
		if (lmInitState.size() > 0) {
			List listInit = new ArrayList();
			for (int i = 0; i < lmInitState.size(); i++) {
				oclSS ss = (oclSS) lmInitState.get(i);
				listInit.add(ss);
			}
			List listGoal = new ArrayList();
			for (int j = 0; j < lmGoalState.size(); j++) {
				oclSE se = (oclSE) lmGoalState.get(j);
				listGoal.add(se);
			}
			drawingCanvas.showOclSS(listInit, listGoal);
		} else {
			JOptionPane.showMessageDialog(top, "No current states.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
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
			JOptionPane.showMessageDialog(top, "Unable to redo: " + ex,
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
		}
	}

	/**
	 * load a graphics file .vm
	 *  
	 */
	private void jMI_OpenActionPerformed() {
		if (tempFile != null) {
			//ask if the user really want to save the current file first
		}

		chooser.setCurrentDirectory(tempFile);
		chooser
				.setFileFilter(new vFilter(".vm", "Visual Modeller File - *.vm"));
		if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION)
			return;

		Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
		Thread runner = new Thread() {
			public void run() {
				tempFile = chooser.getSelectedFile();
				try {
					BufferedReader in = new BufferedReader(
							new InputStreamReader(new FileInputStream(tempFile)));
					//Utility.debugPrintln(in.toString());
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
	 * 
	 * @param br
	 *            BufferedReader
	 *  
	 */
	private void readFile(BufferedReader br) {
		//ron 5/5/03 use domain record
		if (!curDomain.isHierarchical())
			drawingCanvas.loadFile(br);
		else
			loadFile(br);
		drawingCanvas.repaint();
	}

	/* WZ 18/7/02 */
	/**
	 * To open a planner's running result, load data from a bufferredReader.
	 *  
	 */
	public void loadFile(BufferedReader br) {
		// 	String str= "";
		// 	int mySwitch = 0, i = 0, k = 0, j = 0, m = 0, case5Line = 0, n = 0, x
		// = 0;
		// 	vShape tempSP = null;
		// 	vLink tempLK = null;
		// 	int ownerShapeID = 0, row = 0, column = 0;
		// 	String referType = "";
		// 	HSShapeProperty hsProperty = null;
		// 	int parentShapeKey = 0;

		// 	while (!str.equals("END PLANNER RESULT")) {
		// 	    try {
		// 		str = br.readLine();
		// 		Utility.debugPrintln(str);
		// 		if (str.equals("BEGIN METHOD")){
		// 		    mySwitch = 1; //for the HSShapeProperty varibles
		// 		    i = 0;
		// 		    hsProperty = null;
		// 		}

		// // if(str.startsWith("END")){
		// // mySwitch = 0;//reset
		// // }
		// 		switch (mySwitch) {
		// 		case 1://HSShapeProperty varibles
		// 		    switch (i) {
		// 		    case 1: //line 1
		// 			ownerShapeID = parseInt(str, "owner shape ID:");
		// 			break;
		// 		    case 2://line 2
		// 			row = parseInt(str, "row:");
		// 			break;
		// 		    case 3://line 3
		// 			column = parseInt(str, "column:");
		// 			break;
		// 		    case 4://line 4
		// 			parentShapeKey = parseInt(str, "parent shape key:");
		// 			break;
		// 		    }

		// 		    i++;
		// 		    break;
		// 		case 2://REFERENCE varibles
		// 		    switch (j) {
		// 		    case 1: //line 1
		// 			referType = getString(str, "reference type:");
		// 			break;
		// 		    case 2://line 2
		// 			if (referType.equals("oclMethod")){
		// 			    oclMethod md = parseMethod(str);
		// 			    hsProperty = new HSShapeProperty(md, row, column);
		// 			}
		// 			else if (referType.equals("oclOperator")){
		// 			    oclOperator op = parseOperator(str);
		// 			    hsProperty = new HSShapeProperty(op, row, column);
		// 			}
		// 			else if (referType.equals("oclSS")){
		// 			    oclSS ss = parseSS(str);
		// 			    hsProperty = new HSShapeProperty(ss, row, column);
		// 			}

		// 			hsProperty.setOwnerShapeIndex(ownerShapeID);
		// 			hsProperty.setParentShapeKey(parentShapeKey);
		// 			hsProperty.setRow(row);
		// 			hsProperty.setColumn(column);
		// 			/* WZ 21/6/02 */
		// 			for (int linkCounter=1; linkCounter <
		// shape[ownerShapeID].getOutLinks().size()+1; linkCounter++) {
		// 			    vLink tmpLink
		// =(vLink)shape[ownerShapeID].getOutLinks().get("outlinks"+linkCounter);
		// 			    if (tmpLink.getType() == 5)//if it is pink line
		// 				hsProperty.addDecompItem(tmpLink.getStopShape());
		// 			}
		// 			/* end 21/6/02 */
		// 			shape[ownerShapeID].setObject(hsProperty);
		// 			break;
		// 		    }

		// 		    j++;
		// 		    break;
		// 		case 3://object varibles
		// 		    switch (k) {
		// 		    case 1: //line 1
		// 			referType = getString(str, "object type:");
		// 			break;
		// 		    case 2://line 2
		// 			if (referType.equals("oclMethod")){
		// 			    oclMethod md = parseMethod(str);
		// 			    hsProperty.setObject(md);
		// 			}
		// 			else if (referType.equals("oclOperator")){
		// 			    oclOperator op = parseOperator(str);
		// 			    hsProperty.setObject(op);
		// 			}
		// 			else if (referType.equals("oclSS")){
		// 			    oclSS ss = parseSS(str);
		// 			    hsProperty.setObject(ss);
		// 			}
		// 			break;
		// 		    }

		// 		    k++;
		// 		    break;
		// 		case 4://preState varibles/* WZ 20/6/02 */
		// 		    if (m > 0){
		// 			oclSS ss = parseSS(str);
		// 			hsProperty.addPreState(ss);
		// 		    }
		// 		    else
		// 			m++;

		// 		    break;
		// 		case 5://postState varibles/* WZ 20/6/02 */
		// 		    if (case5Line > 0){
		// 			oclSS ss = parseSS(str);
		// 			hsProperty.addPostState(ss);
		// 		    }
		// 		    else

		// 		    case5Line++;
		// 		    break;
		// 		case 6://STEPPER CANVAS varibles/* WZ 20/6/02 */
		// 		    switch (n) {
		// 		    case 1: //line 1:current decomposition list
		// 			String tmpStr = getString(str, "current decomposition list:");
		// 			StringTokenizer st = new StringTokenizer(tmpStr, ",");
		// 			while (st.hasMoreTokens()) {
		// 			    int spID = Integer.parseInt(st.nextToken());
		// 			    vShape vs = shape[spID];
		// 			    curDecompList.add(vs);
		// 			}
		// 			break;
		// 		    case 2://line 2:subgoal shape id
		// 			tmpStr = getString(str, "subgoal shape id:");
		// 			int spID = Integer.parseInt(tmpStr);
		// 			if (spID > 0){
		// 			    subGoalShape = shape[spID];
		// 			    HSShapeProperty hspty = (HSShapeProperty)subGoalShape.getObject();
		// 			    oclSS ss = (oclSS)hspty.getObject();
		// 			    subGoal = ss;
		// 			}
		// 			else{
		// 			    subGoalShape = null;
		// 			    subGoal = null;
		// 			}

		// 			break;
		// 		    case 3://line 3:onceInstantiated
		// 			tmpStr = getString(str, "onceInstantiated");
		// 			if (tmpStr.equals("true"))
		// 			    onceInstantiated = true;
		// 			else
		// 			    onceInstantiated = false;

		// 			break;
		// 		    }

		// 		    n++;
		// 		    break;
		// 		case 7://CURRENT STATE varibles/* WZ 20/6/02 */
		// 		    if (x > 0){
		// 			oclSS ss = parseSS(str);
		// 			curState.add(ss);
		// 		    }
		// 		    else
		// 			x++;
		// 		    break;
		// 		}
		// 	    } catch(java.io.IOException ex){};
		// 	}

		// 	//get other inferred variables
		// 	taskShape = shape[1];
		// 	//populate current states
		// 	((HStepperWindow)parent).updateCurState(curState);
		// 	//curShape
		// 	curShape = getFirstSelectedShape();
		// 	//positionRecord
		// 	int baseRow = 0, baseCol = 0;
		// 	for (int ii = 1; ii < vShapeList.size()+1; ii++) {
		// 	    vShape vs = (vShape)getShape(ii);
		// 	    HSShapeProperty hsp = (HSShapeProperty)vs.getObject();
		// 	    int r = hsp.getRow();
		// 	    int c = hsp.getColumn();
		// 	    int cp = positionRecord.getColumn(r);
		// 	    while (cp == -1) {
		// 		positionRecord.addPosition();
		// 		cp = positionRecord.getColumn(r);
		// 	    };
		// 	    if (c > cp)
		// 		positionRecord.setColumn(r, c);

		// 	}
	}

	/**
	 * show next states after executing an operator
	 *  
	 */
	private void jMI_NextActionPerformed() {// to show the operator
		int i = curStep + 1;
		if (i < lstOM.size()) {
			if (drawingCanvas.getShapeList().size() <= 0) {//drawing initial
														   // states
				jMI_AddInitialStateActionPerformed();
			} else {
				setStep(i);
				drawingCanvas.showAsAOperator(curOperator);
			}
		} else {
			JOptionPane.showMessageDialog(this, "No more steps.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
		}
	}

	/**
	 * save graphics as a new .vm file
	 *  
	 */
	private void jMI_SaveAsGraphicsActionPerformed() {
		chooser.setCurrentDirectory(tempFile);
		chooser
				.setFileFilter(new vFilter(".vm", "Visual Modeller File - *.vm"));
		if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION)
			return;
		try {
			tempFile = chooser.getSelectedFile();
			PrintStream ps = new PrintStream(new FileOutputStream(tempFile));
			saveVMFile(ps);
		} catch (Exception e) {
		}
		;
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
		int i = curStep - 1;
		if (i >= -1 && i < lstOM.size()) {
			setStep(i);
			try {
				drawingCanvas.undo();
			} catch (CannotUndoException ex) {
				JOptionPane.showMessageDialog(this, "Unable to undo: " + ex,
						"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			}
		} else {
			JOptionPane.showMessageDialog(this, "No more steps.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
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
			}
		;
	}

	/**
	 * reset drawing canvas to its initial state
	 *  
	 */
	private void refreshDrawingWindow() {
		int ww, hh;
		ww = getSize().width;
		hh = getSize().height;
		splitPane2.remove(scrollPanel); /* Weihong added on 5/12/2001 */

		//add Drawing Canvas
		addDrawingCanvas();

		lmInitState.clear();
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
	 * 
	 * @param ps
	 *            PrintStream
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

}
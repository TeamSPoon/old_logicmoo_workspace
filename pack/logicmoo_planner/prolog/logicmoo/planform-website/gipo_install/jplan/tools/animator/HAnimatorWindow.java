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
 * Hierarchical Animator Graphical Display Window
 * @author Weihong Zhao
 * 22/7/2002
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
import jplan.images.ImageLoader; 
import jplan.general.Utility;
import jplan.general.GipoInternalFrame;
import jplan.tools.stepper.HStepperCanvas; 
import jplan.general.GipoButton;

/**
 * As a interface AnimatorWindow automatically shows the graphical
 * representation of the planning result step by step.
 */
public class HAnimatorWindow extends GipoInternalFrame {
// Variables declaration
    private JPanel topToolBarPanel;
    private  JToolBar jToolBar1;
    private  JToolBar jToolBar3;
    private  JMenuBar jMenuBar2;
    private  JMenu jM_File;
    private  JMenuItem jMI_Open;
    private  JMenuItem jMI_SaveGraphics;
    private  JMenuItem jMI_SaveAsGraphics;
    private  JMenuItem jMI_Save;
    private  JMenuItem jMI_SaveAs;
    private  JMenuItem jMI_Exit;
    private  JMenuItem jMI_PrintPreview;
    private  JMenuItem jMI_Print;

    private  JMenu jM_Action;
    private  JMenuItem jMI_Pre;
    private  JMenuItem jMI_Next;

    private  JMenu jM_View;
    private  JMenuItem jMI_ZoomOut;
    private  JMenuItem jMI_ZoomIn;
    private  JCheckBoxMenuItem jMI_FileToolbar;
    private  JCheckBoxMenuItem jMI_ActionToolBar;
    private  JCheckBoxMenuItem jMI_EditingStatesList;
    private  JCheckBoxMenuItem jMI_EditingOperatorsList;

    private JScrollPane scrollPanel;
    private HStepperCanvas drawingCanvas; /* WZ 1/11/01 */
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
    private OclEd top; /* WZ 1/11/01 */
    private oclDomain curDomain;
    private List lstOM; //Methods or Operators
    private DefaultListModel lmOM; 
    private JList jlstOM;
    private JScrollPane jscrollOM;
    private JPanel westPanel = new JPanel(); //panel to place at west

    // The tasks
    private JComboBox jcomTasks;
    private Vector vecTasks = new Vector();
    private oclHTNTask curTask = null;
    private oclHTNTask thisTask = null;
    private String taskID = null;
    private List lstOp = new ArrayList();

    // The initial state classes list box
    private JList jlstInitState;
    private DefaultListModel lmInitState; 
    private JScrollPane jscrollInitState;
    private int seleIndex; //the selected index of operators list, to use for saving the existing operator into the same pisition

    // The goal state classes list box
    private JList jlstGoalState;
    private DefaultListModel lmGoalState; 
    private JScrollPane jscrollGoalState;

    private JSplitPane splitPane2; 

    /* WZ 22/7/02 */
    DefaultListModel lmStatics, lmTemps;


    /**
     * Create an instance of AnimatorWindow
     * @param title String title
     * @param curDomain oclDomain
     * @param parent parent frame
     */
    public HAnimatorWindow(String title, oclDomain curDomain, OclEd parent) {
	super(parent);	
	setTitle(title);
	setClosable(false); 
	this.curDomain = curDomain;
	top = parent; 

	if (curDomain == null) {
	    JOptionPane.showMessageDialog(top,"No Domain currently being edited.",
                                           "GIPO Error",JOptionPane.ERROR_MESSAGE,null);	    
	    return;
	}

	lstOM = new ArrayList();

	Utility.debugPrintln("loading planning result...");
	loadPlanningResult();

	lstTask = new ArrayList(); //Need to clone the tasks
	ListIterator li = curDomain.htntasks.listIterator();
	while (li.hasNext()) {
	    oclHTNTask task = (oclHTNTask)li.next();
	    if (task.ID.equals(taskID))
		lstTask.add(task);
	}

	tempFile = null;
	initComponents ();

	/* WZ 23/7/02 */
	Utility.debugPrintln("loading planning result details...");
	loadPlanningResultDetail();	

	pack ();
	updateUI();
	setVisible(true);
    }
    
    /**
     * Initialisation
     */
    private void initComponents () {
	getContentPane ().setLayout (new java.awt.BorderLayout ());

	jMenuBar2 = new javax.swing.JMenuBar ();
	jM_File = new javax.swing.JMenu ();
	jM_File.setText ("File");

	ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Import16.gif");
        jMI_Open = new javax.swing.JMenuItem ("Load Graphics", ii);
        jMI_Open.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_OpenActionPerformed ();
            }
	}
				    );
        jM_File.add(jMI_Open);

	ii = ImageLoader.getImageIcon(top.strImageDir, "Export16.gif");
        jMI_SaveGraphics = new javax.swing.JMenuItem ("Save Graphics", ii);
        jMI_SaveGraphics.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_SaveGraphicsActionPerformed ();
            }
          }
				    );
        jM_File.add(jMI_SaveGraphics);

	ii = ImageLoader.getImageIcon(top.strImageDir, "SaveAs16.gif");
        jMI_SaveAsGraphics = new javax.swing.JMenuItem ("Save As New Graphics", ii);
        jMI_SaveAsGraphics.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_SaveAsGraphicsActionPerformed ();
            }
          }
				    );
        jM_File.add(jMI_SaveAsGraphics);

        jM_File.addSeparator();

	ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
        jMI_PrintPreview = new javax.swing.JMenuItem ("Print Preview", ii);
        jMI_PrintPreview.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
              jMI_PrintPreviewActionPerformed ();
            }
	}
					    );
        jM_File.add(jMI_PrintPreview);

	ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
        jMI_Print = new javax.swing.JMenuItem ("Print", ii);
        jMI_Print.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_PrintActionPerformed ();
            }
	}
				     );
        jM_File.add(jMI_Print);

        jM_File.addSeparator();

	ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");	
        jMI_Exit = new javax.swing.JMenuItem ("Close", ii);
        jMI_Exit.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_ExitActionPerformed ();
            }
	}
        );
        jM_File.add(jMI_Exit);
	
	jMenuBar2.add(jM_File);

	jM_View = new javax.swing.JMenu ("View");
	
	ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
	jMI_ZoomIn = new javax.swing.JMenuItem ("Zoom In", ii);
	jMI_ZoomIn.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_ZoomInActionPerformed ();
	    }
	}
				      );
	jM_View.add(jMI_ZoomIn);

	ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomOut16.gif");
	jMI_ZoomOut = new javax.swing.JMenuItem ("Zoom Out", ii);
	jMI_ZoomOut.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_ZoomOutActionPerformed ();
	    }
	}
				       );
	jM_View.add(jMI_ZoomOut);
	jM_View.addSeparator();
	
	jMI_FileToolbar = new javax.swing.JCheckBoxMenuItem ("File Toolbar");
	jMI_FileToolbar.setState(true);
	jMI_FileToolbar.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		if (jMI_FileToolbar.getState()) 
		    topToolBarPanel.add(jToolBar1);
		else
		    topToolBarPanel.remove(jToolBar1);
		updateUI();
	    }
	}
					   );
	  jM_View.add(jMI_FileToolbar);
	  
	  jMI_ActionToolBar = new javax.swing.JCheckBoxMenuItem ("Action Toolbar");
	  jMI_ActionToolBar.setState(true);
	  jMI_ActionToolBar.addActionListener (new java.awt.event.ActionListener () {
	      public void actionPerformed (java.awt.event.ActionEvent evt) {
		  jToolBar3.setVisible(jMI_ActionToolBar.getState());
		  updateUI();
	      }
          }
					     );
	  jM_View.add(jMI_ActionToolBar);

	  jMenuBar2.add(jM_View);
	  
	  setJMenuBar(jMenuBar2);
	  
	  
	  /* 
	     add Toolbars
	  */
	  
	  jToolBar1 = new JToolBar();
	  jToolBar1.setFloatable(false);

	ii = ImageLoader.getImageIcon(top.strImageDir, "Import16.gif");
	  JButton bt = new GipoButton(" Load ", ii);
	  bt.setToolTipText("Load Graphics");
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_OpenActionPerformed ();
	      }
	  });
	  jToolBar1.add(bt);

	ii = ImageLoader.getImageIcon(top.strImageDir, "Export16.gif");
	  bt = new GipoButton(" Save ", ii);
	  bt.setToolTipText("Save Graphics");
	  bt.setToolTipText("Save as a graphics file only"); 
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_SaveGraphicsActionPerformed ();
	      }
	  });
 	  jToolBar1.add(bt);

	  jToolBar1.addSeparator();

	ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
	  bt = new GipoButton(" Preview ", ii);
	  bt.setToolTipText("Print Preview");
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_PrintPreviewActionPerformed ();
	      }
	  });
	  jToolBar1.add(bt);

	ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
	  bt = new GipoButton(" Print ", ii);
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_PrintActionPerformed ();
	      }
	  });
	  jToolBar1.add(bt);

	  jToolBar1.addSeparator();

	ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
	  bt = new GipoButton(" Close ", ii);
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_ExitActionPerformed ();
	      }
	  });
	  jToolBar1.add(bt);
	  
	  /* jToolBar3 */
	  jToolBar3 = new JToolBar();
	  jToolBar3.setFloatable(false);

	ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
	  bt = new JButton(" Zoom In ", ii);
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_ZoomInActionPerformed ();
	      }
	  });
 	  jToolBar3.add(bt);

	ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomOut16.gif");
	  bt = new JButton(" Zoom Out ", ii);
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_ZoomOutActionPerformed ();
	      }
	  });
 	  jToolBar3.add(bt);

	  topToolBarPanel = new JPanel();
	  topToolBarPanel.setLayout(new GridLayout(0, 1));
	  topToolBarPanel.add(jToolBar1);
	  getContentPane().add(topToolBarPanel, "North");
	  getContentPane().add(jToolBar3, "South");
	  
	  //add splitpane2 (will contain drawingCanvas and jscollOM)
	  splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);

	  /* add drawing Canvas*/
	  addDrawingCanvas();
	  
	  //add tasks panel (west panel)

	  // Build the initial State Class List
 	  jlstInitState = new JList();
 	  lmInitState = new DefaultListModel();
	  jlstInitState.setModel(lmInitState);
	  jlstInitState.setEnabled(false);
	  initStateList(curTask);
	  jscrollInitState = new JScrollPane(jlstInitState);
	  jscrollInitState.setBorder(BorderFactory.createTitledBorder("Initial State"));
	  jlstInitState.setCellRenderer(new PredListCellRenderer());

	  // Build the goal State Class List
 	  jlstGoalState = new JList();
 	  lmGoalState = new DefaultListModel();
	  jlstGoalState.setModel(lmGoalState);
	  jlstGoalState.setEnabled(false);
	  initStateList(curTask);
	  jscrollGoalState = new JScrollPane(jlstGoalState);
	  jscrollGoalState.setBorder(BorderFactory.createTitledBorder("Goal State"));
	  jlstGoalState.setCellRenderer(new PredListCellRenderer());
	  
	  /* WZ 22/7/02 */
	  //add tasks panel (west panel)
	  //add the tasks combobox
	  ListIterator li = lstTask.listIterator();
	  while (li.hasNext()){
	      vecTasks.addElement((oclHTNTask)li.next());
	  }
	  jcomTasks = new JComboBox(vecTasks);
	  jcomTasks.setAlignmentY(0);
	  jcomTasks.setBackground(Color.white);
 	  initTaskList();
// 	  JButton cmdDummy = new JButton("XXX");
// 	  int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
// 	  jcomTasks.setPreferredSize(new Dimension(80,txtFieldMaxHeight));

	  // Build the initial State Class List
 	  jlstInitState = new JList();
 	  lmInitState = new DefaultListModel();
	  jlstInitState.setModel(lmInitState);
	  jlstInitState.setEnabled(false);
	  initStateList(curTask);
	  jscrollInitState = new JScrollPane(jlstInitState);
	  jscrollInitState.setBorder(BorderFactory.createTitledBorder("Initial State"));
	  jlstInitState.setCellRenderer(new PredListCellRenderer());

	  // Build the goal State Class List
 	  jlstGoalState = new JList();
 	  lmGoalState = new DefaultListModel();
	  jlstGoalState.setModel(lmGoalState);
	  jlstGoalState.setEnabled(false);

	  jscrollGoalState = new JScrollPane(jlstGoalState);
	  jscrollGoalState.setBorder(BorderFactory.createTitledBorder("Goal State"));

	  // Build statics list
 	  JList jlstStatics = new JList();
 	  lmStatics = new DefaultListModel();
	  jlstStatics.setModel(lmStatics);
	  jlstStatics.setEnabled(false);

	  JScrollPane jscrollStatics = new JScrollPane(jlstStatics);
	  jscrollStatics.setBorder(BorderFactory.createTitledBorder("Statics"));

	  // Build temporal list
 	  JList jlstTemps = new JList();
 	  lmTemps = new DefaultListModel();
	  jlstTemps.setModel(lmTemps);
	  jlstTemps.setEnabled(false);

	  JScrollPane jscrollTemps = new JScrollPane(jlstTemps);
	  jscrollTemps.setBorder(BorderFactory.createTitledBorder("Temporal"));

	  //populate goals
	  goalStateList(curTask);

	  //assembly west panel
	  westPanel.setBorder(BorderFactory.createTitledBorder("Task .."));
	  GridBagLayout gridbag = new GridBagLayout();
	  westPanel.setLayout(gridbag);
	  GridBagConstraints c = new GridBagConstraints();
 	  c.fill = GridBagConstraints.BOTH;
	  c.weightx = 1.0;
	  c.gridwidth = GridBagConstraints.REMAINDER; //end row
	  gridbag.setConstraints(jcomTasks, c);
	  westPanel.add(jcomTasks);

	  JPanel goalPanel = new JPanel();
	  goalPanel.setLayout(new GridLayout(0, 1));
	  goalPanel.add(jscrollStatics);
	  goalPanel.add(jscrollGoalState);
	  goalPanel.add(jscrollTemps);

	  JSplitPane staskPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, jscrollInitState, goalPanel);
	  staskPanel.setDividerSize(6);
	  staskPanel.setOneTouchExpandable(true);
	  staskPanel.setDividerLocation(0.5);
	  staskPanel.setResizeWeight(0.5);

	  c.weighty = 1.0;
	  gridbag.setConstraints(staskPanel, c);
	  westPanel.add(staskPanel);
	  staskPanel.updateUI();
	  // Build the Operator List
// 	  showOperatorLists();

// 	  splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, westPanel, splitPane2);
// 	  splitPane1.setResizeWeight(0.15);
// 	  splitPane1.setOneTouchExpandable(true);
// 	  splitPane1.setDividerSize(6);
// 	  getContentPane().add(splitPane1, "Center");

	  splitPane2.add(westPanel);
	  splitPane2.setOneTouchExpandable(true);
	  splitPane2.setDividerSize(6);/* WZ 8/7/02 */
	  splitPane2.setResizeWeight(0.5);
// 	  splitPane2.setRightComponent(westPanel);
	  getContentPane().add(splitPane2, "Center");

	  //Initiate varibles
	  viewSize = scrollPanel.getViewport().getViewSize();
    }
    
    /**
     * add drawing canvas
     * 
     */
    private void addDrawingCanvas() {
	  drawingCanvas = new HStepperCanvas(this);
      	  drawingCanvas.setAnimatorFlag(true);    /* WZ 21/8/02 */
      	  drawingCanvas.setWorkingDomain(this.curDomain); //pass the working domain
	  scrollPanel = new JScrollPane(drawingCanvas);
// 	  scrollPanel.setBorder(BorderFactory.createTitledBorder("Editing/Drawing Canvas"));
	  splitPane2.add(scrollPanel);	
	  splitPane2.setRightComponent(scrollPanel);
    }

    /* WZ 3/07/2001 */
    /**
     * show task contents on the property window
     * 
     */
    private void populateTasks(){
	if (curTask != null){
	    refreshDrawingWindow();
	    curTask = (oclHTNTask)jcomTasks.getSelectedItem();
	    initStateList(curTask);
	    goalStateList(curTask);
// 	    jMI_AddInitialStateActionPerformed ();
	    updateUI();
	}
    }

    /**
     * initialise the task list
     * 
     */
    private void initTaskList(){
	if (jcomTasks.getItemCount() >0) {
	    jcomTasks.setSelectedIndex(0);
	    curTask = (oclHTNTask)jcomTasks.getItemAt(0) ;
// 	    initStateList(curTask);
// 	    goalStateList(curTask);
// 	    updateUI();
	}
    }

    /**
     * show init states (oclSS)
     * @param task oclHTNTask
     */
    private void initStateList(oclHTNTask task){
	if (task != null){
	    showStateProperty(lmInitState, task.getInits(), 1); //oclSS
	}
    }


    /**
     * show goal states (oclSS)
     * @param task oclHTNTask
     * 
     */
    private void goalStateList(oclHTNTask task){
	if (task != null){
	    lmGoalState.clear();
	    lmStatics.clear();
	    lmTemps.clear();
	  // Build the goal State Class List
	  ListIterator li = task.getGoalLists().listIterator();
	  while (li.hasNext()){
	      lmGoalState.addElement(li.next());
	  }
	  // Build statics list
	  li = task.getStatics().listIterator();
	  while (li.hasNext()){
	      oclPredicate statics = (oclPredicate)li.next();
	      lmStatics.addElement(statics);
	      Utility.debugPrintln("Statics -- "+statics.toString());
	  }
	  // Build temporal list
	  li = task.getTemps().listIterator();
	  while (li.hasNext()){
	      oclPredicate tmps = (oclPredicate)li.next();
	      lmTemps.addElement(tmps);
	      Utility.debugPrintln("tmps -- "+tmps.toString());
	  }
	  updateUI();
	}
    }

    /**
     * show states to a JList.
     * @param stateModel destination (DefaultListModel)
     * @param stateList input
     * @param mySwitch switch between oclSS and oclSE
     * 
     */
    public void showStateProperty(DefaultListModel stateModel, List stateList, int mySwitch){
	stateModel.clear();
	ListIterator li = stateList.listIterator();
	while (li.hasNext()) {
	    if (mySwitch == 1) {
		oclSS ss = (oclSS)li.next();
		stateModel.addElement(ss);
	    }
	    else if (mySwitch == 2) {
		oclSE se = (oclSE)li.next();
		stateModel.addElement(se);
	    }		
	}	    
	updateUI();
    }

    /*
     * load planning result
     * 
     */
    private void loadPlanningResult(){
	File pResultFile = new File(top.strOCLPath + File.separator + "tmp" + File.separator + "run.txt");
	try {
	    BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(pResultFile)));
	    if (in.ready()) {
		readPlanningResult(in);
	    }
	    in.close();
	} catch (IOException ex) {ex.printStackTrace();}
    }

    /* WZ 23/7/02 */
    /*
     * load planning result detail
     * 
     */
    private void loadPlanningResultDetail(){
	File pResultFile = new File(top.strOCLPath + File.separator + "tmp" + File.separator + "run.txt"); /* WZ 19/8/02 */

	try {
	    BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(pResultFile)));
	    if (in.ready()) {
		drawingCanvas.drawTaskShape(curTask);
		drawingCanvas.loadHTNResult(in);
	    }
	    in.close();
	} catch (IOException ex) {ex.printStackTrace();}
    }

    /*
     * translate Result from FF format
     * @param br BufferedReader
     * @return File
     */
    private File translateResult(BufferedReader br){
	String tmpStr = null;
	PrintStream ps = null;
	File returnFile = new File(top.strOCLPath + File.separator + "tmp" + File.separator + "test.txt");
	String str= "";
	boolean started = false;
	try {
	    ps = new PrintStream(new FileOutputStream(returnFile));
	} catch (IOException ex) {}
	while (true)  {
	    try {
		str = br.readLine();
		if (str.startsWith("problem")){
		    tmpStr = str.substring(str.indexOf("TASK"));
		    String ID = tmpStr.substring(tmpStr.indexOf("TASK") + 4, tmpStr.indexOf("'"));
		    ps.println("TASK "+ID);
		    ps.println("SOLUTION");
		    started = true;
		}
		
		if (str.startsWith("step")){
		    while (started && str.trim().length() != 0){
			tmpStr = str.substring(str.indexOf(":")+2);
			int position = tmpStr.indexOf(" ");
			String s=null;
			s = tmpStr.substring(0,position);		
			ps.print(s.toLowerCase()+"(");
			tmpStr = tmpStr.substring(position+1);
			while (tmpStr.trim().length() != 0){
			    position = tmpStr.indexOf(" ");
			    if (position == -1){
				s = tmpStr;
				tmpStr = "";
				ps.print(s.toLowerCase());
			    }
			    else {
				s = tmpStr.substring(0,position);
				tmpStr = tmpStr.substring(position+1);
				ps.print(s.toLowerCase()+",");
			    }	
			}
			ps.println(")");
			str = br.readLine();
		    }
		    ps.println("END FILE");
		    return returnFile;
		}
	    } catch(java.io.IOException ex){
		Utility.debugPrintln(ex);
	    }
	}
    }

    /*
     * load planning result
     * @param br BufferedReader
     * 
     */
    private void readPlanningResult(BufferedReader br){
	String str= "";
	boolean opList = false;
	int j = 0;
	oclOperator op = null;

	while (true)  {
	    try {
		str = br.readLine();
		if(str.equals("END FILE"))
		    return;
		Utility.debugPrintln(str);
		if (str.startsWith("TASK")){
		    taskID = str.substring(str.indexOf("TASK ") + 5);
		}
		if (str.equals("SOLUTION"))
		    opList = true;

		if (opList){
		    //for the operators list
		    if (j > 0){
			String opName = str.substring(0, str.indexOf("("));
			//check the list from the curdomain's operator list and get the right op
			ListIterator li = curDomain.operators.listIterator();
			while (li.hasNext()){
			    oclOperator temOP = (oclOperator)li.next();
			    oclPredicate temPred = (oclPredicate)temOP.opName;
			    if (temPred.getName().equals(opName)){
				try {
				    op = (oclOperator)temOP.clone();
				} catch (CloneNotSupportedException e) {
				    Utility.debugPrintln(e);
				}
				break;
			    }
			}
			if (op != null){
			    //instantiate this operator
			    //first get the signature
			    oclPredicate signaturePred = (oclPredicate)curDomain.createOperatorSignature(op);

			    //then instantiate it to get the desire operator
			    String argStr = str.substring(opName.length()+1, str.indexOf(")")) + ",";
			    for (int i = 0; i < signaturePred.size(); i++){
				if (i>0) {
				    argStr = argStr.substring(argStr.indexOf(",")+1);
				}
				String newStr = argStr.substring(0, argStr.indexOf(","));
				try {
				    OPredicate.pArg selectedPArg = signaturePred.pArgOf(i);
				    op.replaceVariableName(selectedPArg, newStr);
				} catch (Exception e) {
				    Utility.debugPrintln("Failed to replace var Name" + e);
				}   
			    }
			    //finally add this instantiated operator to the return list
			    lstOM.add(op);
			}
			else {
			    Utility.debugPrintln(" !!!! Problems with obtaining an operator. !!!!");
			}
		    }
		    j++;
		}
	    } catch(java.io.IOException ex){Utility.debugPrintln(ex);};
	}
    }

    /*
     * show operators in a JLists
     * @return File
     */
//     private void showOperatorLists(){
// 	jlstOM = new JList();
// 	lmOM = new DefaultListModel();
// 	jlstOM.setToolTipText("Double click to view property");
// 	jlstOM.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
// 	ListIterator li = lstOM.listIterator();
// 	while (li.hasNext()) {
// 	    lmOM.addElement((oclOperator)li.next());
// 	}
// 	jlstOM.setModel(lmOM);
// 	jlstOM.setSelectedIndex(0);
// 	jlstOM.addMouseListener(new MouseAdapter() {
// 	    public void mouseClicked(MouseEvent me) {
// 		if (me.getClickCount() == 2) {
// 		    int i = jlstOM.locationToIndex(me.getPoint());
// 		    oclOperator op = (oclOperator)jlstOM.getSelectedValue();
// 		    try {
// 			OperatorProperty pw = new OperatorProperty(curDomain, (oclOperator)op.clone(), HAnimatorWindow.this, top.strImageDir);
// 			pw.setLocation((int) (0.5 * getWidth()),(int) (0.5 * getHeight()));
// 			pw.show();
// 		    } catch (CloneNotSupportedException e){Utility.debugPrintln(e);}
// 		    seleIndex = jlstOM.getSelectedIndex();
// 		}
// 	    }
// 	});

// 	jscrollOM = new JScrollPane(jlstOM);
// 	jscrollOM.setBorder(BorderFactory.createTitledBorder("Operators List"));

// 	splitPane2.add(jscrollOM);
// 	splitPane2.setRightComponent(jscrollOM);
//     }

    /**
     * refresh state list after anychange
     * 
     */
//     private void refreshStateList(){
// 	splitPane2.remove(jscrollOM);
// 	showOperatorLists();
// 	updateUI();
//     }

    /**
     * zoom out
     * 
     */
    private void jMI_ZoomOutActionPerformed () {
	viewSize = new Dimension((int)(viewSize.width /zoom), (int)(viewSize.height/zoom));
	drawingCanvas.setScale(1/zoom);
    }

    /**
     * zoom in
     * 
     */
    private void jMI_ZoomInActionPerformed () {
	viewSize = new Dimension((int)(viewSize.width * zoom), (int)(viewSize.height * zoom));
        drawingCanvas.setScale(zoom);
    }

    /**
     * print preview
     * 
     */
    private void jMI_PrintPreviewActionPerformed () {
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
    private void jMI_PrintActionPerformed () {
	PrinterJob pj = PrinterJob.getPrinterJob();
	drawingCanvas.freezeImageForPrinting();	
	pj.setPrintable(drawingCanvas);
	if (pj.printDialog()) {
	    try {
		pj.print();
	    } catch(PrinterException ex) {
		System.err.println(ex);
	    }
	}
    }

    /**
     * load a graphics file .vm
     * 
     */
    private void jMI_OpenActionPerformed () {
	if (drawingCanvas.getShapeCount() > 2) {
	    int k = JOptionPane.showConfirmDialog(top, "Editing contents have not been updated. \nClear content without updating?", "", JOptionPane.YES_NO_OPTION);
	    if (k == JOptionPane.NO_OPTION) {
		return;
	    }
	}

	chooser.setCurrentDirectory(tempFile);
	chooser.setFileFilter(new vFilter(".vm", "Visual Modeller File - *.vm"));
	if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION)
	    return;

	Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
	Thread runner = new Thread() {
	    public void run() {
		tempFile = chooser.getSelectedFile();
		try {
		    BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(tempFile)));
		    if (in.ready())
			readFile(in);
	  
		    in.close();
		} catch (IOException ex) {ex.printStackTrace();}
		
		chooser.rescanCurrentDirectory();
		Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
	    }
	};
	runner.start();
    }

    /* WZ 23/7/02 */
    /**
     * read file from a BufferedReader
     * @param br BufferedReader
     * 
     */
    private void readFile(BufferedReader br) {
	refreshDrawingWindow();
	drawingCanvas.loadFile(br);
	String taskName = drawingCanvas.getShape(1).getLabel();
	for (int i=0; i<jcomTasks.getItemCount(); i++){
	    oclHTNTask task = (oclHTNTask)jcomTasks.getItemAt(i);
	    if (taskName.equals(curTask.toString())){
		curTask = task;
		jcomTasks.setSelectedIndex(i);
		initStateList(curTask);
		goalStateList(curTask);
		updateUI();
	    }
	}
    }

    /**
     * save graphics as a new .vm file
     * 
     */
    private void jMI_SaveAsGraphicsActionPerformed () {
	chooser.setCurrentDirectory(tempFile);
	chooser.setFileFilter(new vFilter(".vm", "Visual Modeller File - *.vm"));
	if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION)
	    return;
	try {
	    tempFile = chooser.getSelectedFile();
	    PrintStream ps = new PrintStream(new FileOutputStream(tempFile));
	    saveVMFile(ps);
	}
	catch(Exception e) {};
    }

    /**
     * close window
     * 
     */
    private void jMI_ExitActionPerformed () {
	setVisible (false);
	dispose ();	
    }

    /**
     * save graphics as .vm file
     * 
     */
    private void jMI_SaveGraphicsActionPerformed () {
	if(tempFile == null)
	    jMI_SaveAsGraphicsActionPerformed();
	else
	    try {
		saveVMFile(new PrintStream(new FileOutputStream(tempFile)));
	    } catch(Exception e){};
    }

    /**
     * reset drawing canvas to its initial state
     * 
     */
    private void refreshDrawingWindow() {
	int ww, hh;
	ww = getSize().width;
	hh = getSize().height;
	splitPane2.remove(scrollPanel);

	//add Drawing Canvas
	addDrawingCanvas();

	lmInitState.clear();
	lmGoalState.clear();
	updateUI();

	//set the tempFile to null
	tempFile = null;

	setVisible(false);
	pack();
	setSize(ww,hh);
	setVisible(true);
    }

    /**
     * save .vm file
     * @param ps PrintStream
     * 
     */
    private void saveVMFile(PrintStream ps) {

	ps.println("******** HIERARCHICAL PLANNING RESULT ********\n");
	ps.println("AUTHOR:"+curDomain.getAuthor()+"\n");
	ps.println("TIME:"+java.util.Calendar.getInstance().getTime().toString()+"\n");
	ps.println("DOMAIN:"+curDomain.getName()+"\n");
	ps.println("TASK:"+curTask.toString()+"\n");
	ps.println("***************************************\n");
	ps.println(drawingCanvas.to_String());
	ps.println("\n");

	ps.close();
    }

}

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
import jplan.images.ImageLoader; 	/* Weihong changed on 5/9/2001 */
import jplan.general.Utility;
import jplan.general.GipoInternalFrame;/* Weihong added on 24/10/2001 */
import jplan.general.GipoButton;/* Weihong added on 5/12/2001 */
import jplan.general.HardcopyWriter; /* Weihong 14/3/02 */


/**
 * As a interface StepperWindow manually shows the graphical
 * representation of the result after execution of an operator.
 * Goal state can be auto detected.
 */
public class StepperWindow extends GipoInternalFrame implements oclPrint{/* Weihong added on 24/10/2001 */

    /**
     * Create an instance of StepperWindow
     * @param title String title
     * @param curDomain oclDomain
     * @param parent parent frame
     */ 
    public StepperWindow(String title, oclDomain curDomain, OclEd parent) {
	super(parent);	/* Weihong added on 24/10/2001 */
	setTitle(title);
	setClosable(false); 	/* Weihong added on 12/10/2001 */
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(top,"No Domain currently being edited.",
                                           "GIPO Error",JOptionPane.ERROR_MESSAGE,null);	    
	    return;
	}
	this.curDomain = curDomain;
	top = parent; 

	lstTask = new ArrayList(); //Need to clone the tasks
	ListIterator li = curDomain.tasks.listIterator();
	while (li.hasNext()) {
	    lstTask.add(((oclTask)li.next()));
	}
	
	lstOP = new ArrayList();
	li = curDomain.operators.listIterator();
	while (li.hasNext()) {
	    lstOP.add((oclOperator)li.next());
	}

	tempFile = null;
	initComponents ();
	pack();
	setVisible(true);
    }
    
    /**
     * Initialisation
     * 
     */  
    private void initComponents () {
	
	getContentPane ().setLayout (new java.awt.BorderLayout ());

	jMenuBar2 = new javax.swing.JMenuBar ();
	jM_File = new javax.swing.JMenu ();
	jM_File.setText ("File");
	
        jM_File.addSeparator();

	/* Weihong changed/added on 5/9/2001 */
	ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Import16.gif");
        jMI_Open = new javax.swing.JMenuItem ("Load Graphics", ii);
        jMI_Open.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_OpenActionPerformed ();
            }
	}
				    );
        jM_File.add(jMI_Open);
	
	/* Weihong changed/added on 5/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Export16.gif");
        jMI_SaveGraphics = new javax.swing.JMenuItem ("Save Graphics", ii);
        jMI_SaveGraphics.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_SaveGraphicsActionPerformed ();
            }
          }
				    );
        jM_File.add(jMI_SaveGraphics);
	/* Weihong changed/added on 5/9/2001 */
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
	/* Weihong changed/added on 5/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "PrintPreview16.gif");
        jMI_PrintPreview = new javax.swing.JMenuItem ("Print Preview", ii);
        jMI_PrintPreview.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
              jMI_PrintPreviewActionPerformed ();
            }
	}
					    );
        jM_File.add(jMI_PrintPreview);
	/* Weihong changed/added on 5/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
        jMI_Print = new javax.swing.JMenuItem ("Print graphics", ii);
        jMI_Print.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_PrintActionPerformed ();
            }
	}
				     );
        jM_File.add(jMI_Print);

	/* Weihong changed/added on 5/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
        jMI_PrintText = new javax.swing.JMenuItem ("Print text", ii);
	jMI_PrintText.setToolTipText("Print a text format action list");
        jMI_PrintText.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_PrintTextActionPerformed ();
            }
	}
				     );
        jM_File.add(jMI_PrintText);

        jM_File.addSeparator();
	/* Weihong changed/added on 5/9/2001 */
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

	jM_Action = new javax.swing.JMenu ();
	jM_Action.setText ("Action");
	/* Weihong changed/added on 5/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Undo16.gif");
        jMI_Pre = new javax.swing.JMenuItem ("Pre Step", ii);
        jMI_Pre.setToolTipText ("Go to the previous step");
        jMI_Pre.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_PreActionPerformed ();
            }
          }
				    );
        jM_Action.add(jMI_Pre);
	/* Weihong changed/added on 5/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Redo16.gif");	
        jMI_Next = new javax.swing.JMenuItem ("Next Step", ii);
        jMI_Next.setToolTipText ("Go to the next step");
        jMI_Next.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
              jMI_NextActionPerformed ();
            }
	}
        );
        jM_Action.add(jMI_Next);  

	jMenuBar2.add(jM_Action);

	jM_View = new javax.swing.JMenu ("View");
	/* Weihong changed/added on 5/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
	jMI_ZoomIn = new javax.swing.JMenuItem ("Zoom In", ii);
	jMI_ZoomIn.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jMI_ZoomInActionPerformed ();
	    }
	}
				      );
	jM_View.add(jMI_ZoomIn);
	/* Weihong changed/added on 5/9/2001 */
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
	  jToolBar1.setFloatable(false);	/* Weihong added on 19/10/2001 */

	  /* Weihong changed/added on 5/9/2001 */
	  ii = ImageLoader.getImageIcon(top.strImageDir, "Import16.gif");
	  JButton bt = new GipoButton(" Load ", ii);/* Weihong changed on 5/12/2001 */
	  bt.setToolTipText("Load Graphics");
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_OpenActionPerformed ();
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
		  jMI_SaveGraphicsActionPerformed ();
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
		  jMI_PrintPreviewActionPerformed ();
	      }
	  });
	  jToolBar1.add(bt);

	  /* Weihong changed/added on 5/9/2001 */
	  ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
	  bt = new GipoButton("Print graphics", ii);/* Weihong changed on 5/12/2001 */
	  bt.setToolTipText("Print a graphicalyl formatted action sequence");
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_PrintActionPerformed ();
	      }
	  });
	  jToolBar1.add(bt);

	  /* Weihong changed/added on 5/9/2001 */
	  ii = ImageLoader.getImageIcon(top.strImageDir, "Print16.gif");
	  bt = new GipoButton("Print text", ii);/* Weihong changed on 5/12/2001 */
	  bt.setToolTipText("Print a text format action sequence list");
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_PrintTextActionPerformed ();
	      }
	  });
	  jToolBar1.add(bt);

	  jToolBar1.addSeparator();

	  /* Weihong changed/added on 5/9/2001 */
	  ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
	  bt = new GipoButton(" Close ", ii);/* Weihong changed on 5/12/2001 */
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_ExitActionPerformed ();
	      }
	  });
	  jToolBar1.add(bt);
	  

	  /* jToolBar3 */
	  jToolBar3 = new JToolBar();
	  jToolBar3.setFloatable(false);	/* Weihong added on 19/10/2001 */

	  /* Weihong changed/added on 5/9/2001 */
	  ii = ImageLoader.getImageIcon(top.strImageDir, "Undo16.gif");
	  bt = new JButton(" Pre ", ii);
	  bt.setToolTipText ("Go to the previous step");
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_PreActionPerformed ();
	      }
	  });
	  jToolBar3.add(bt);

	  /* Weihong changed/added on 5/9/2001 */
	  ii = ImageLoader.getImageIcon(top.strImageDir, "Redo16.gif");	  
	  bt = new JButton(" Next ", ii);
	  bt.setToolTipText ("Go to the next step");
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_NextActionPerformed ();
	      }
	  });
	  jToolBar3.add(bt);	  

	  jToolBar3.addSeparator();

	  /* Weihong changed/added on 5/9/2001 */
	  ii = ImageLoader.getImageIcon(top.strImageDir, "ZoomIn16.gif");
	  bt = new JButton(" Zoom In ", ii);
	  bt.addActionListener(new java.awt.event.ActionListener() {
	      public void actionPerformed(ActionEvent e) {
		  jMI_ZoomInActionPerformed ();
	      }
	  });
 	  jToolBar3.add(bt);

	  /* Weihong changed/added on 5/9/2001 */
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
	  
	  /* Weihong added on 5/12/2001 */
	  //add splitpane2 (will contain drawingCanvas and jscollOM)
	  splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
	  splitPane2.setResizeWeight(0.8);
	  splitPane2.setDividerSize(4);/* WZ 13/5/02 */

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

	  //add the tasks combobox
	  ListIterator li = lstTask.listIterator();
	  while (li.hasNext()){
	      vecTasks.addElement((oclTask)li.next());
	  }
	  jcomTasks = new JComboBox(vecTasks);
	  jcomTasks.addActionListener(new ActionListener() {
	      public void actionPerformed(ActionEvent evt) {
		  populateTasks();
	      }
	  });
	  jcomTasks.setAlignmentY(0);
	  jcomTasks.setBackground(Color.white);
	  /* WZ 13/5/02 */
	  JButton cmdDummy = new JButton("XXX");
	  int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
	  jcomTasks.setMaximumSize(new Dimension(1000,txtFieldMaxHeight));

	  //assembly west panel
	  westPanel.setBorder(BorderFactory.createTitledBorder("Task .."));
	  westPanel.setLayout(new BoxLayout(westPanel,BoxLayout.Y_AXIS));
	  westPanel.add(jcomTasks);
	  westPanel.add(jscrollInitState);
	  westPanel.add(jscrollGoalState);

	  // Build the Operator List
	  showOperatorLists();

	  /* Weihong added on 5/12/2001 */
	  splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, westPanel, splitPane2);
	  splitPane1.setResizeWeight(0.15);
	  splitPane1.setDividerSize(4);/* WZ 13/5/02 */
	  getContentPane().add(splitPane1, "Center");

	  //Initiate varibles
	  viewSize = scrollPanel.getViewport().getViewSize();
    }
    
    /**
     * add drawing canvas
     * 
     */
    private void addDrawingCanvas() {
	  drawingCanvas = new StepperCanvas(this);
      	  drawingCanvas.setWorkingDomain(this.curDomain); //pass the working domain
	  scrollPanel = new JScrollPane(drawingCanvas);
	  scrollPanel.setBorder(BorderFactory.createTitledBorder("Editing/Drawing Canvas"));
	  /* Weihong added on 5/12/2001 */
	  splitPane2.add(scrollPanel);	
	  splitPane2.setLeftComponent(scrollPanel);
    }

    /* Weihong added on 3/07/2001 */
    /**
     * show task contents on the property window
     * 
     */
    private void populateTasks(){
	if (curTask != null){
	    refreshDrawingWindow();
	    curTask = (oclTask)jcomTasks.getSelectedItem();
	    initStateList(curTask);
	    goalStateList(curTask);
// 	    Utility.debugPrintln("curTask -- "+curTask);
	    jMI_AddInitialStateActionPerformed ();
	    updateUI();
	}
    }

    /* Weihong added on 3/07/2001 */
    /**
     * initialise the task list
     * 
     */
    public void initTaskList(){
	if (jcomTasks.getItemCount() >0) {
	    jcomTasks.setSelectedIndex(0);
	    curTask = (oclTask)jcomTasks.getItemAt(0) ;
// 	    Utility.debugPrintln("curTask -- "+curTask);
	    populateTasks();
	}
    }

    /**
     * show init states (oclSS)
     * @param task oclTask
     */
    private void initStateList(oclTask task){
	if (task != null){
	    showStateProperty(lmInitState, task.getInits(), 1); //oclSS
	}
    }


    /**
     * show goal states (oclSS)
     * @param task oclTask
     * 
     */
    private void goalStateList(oclTask task){
	if (task != null){
	    showStateProperty(lmGoalState, task.getGoals(), 2); //oclSS
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
     * show operators in a JLists
     * @return File
     */
    private void showOperatorLists(){
	//show primitive operators
	jlstOP = new JList();
	lmOP = new DefaultListModel();
	jlstOP.setToolTipText("Double click the operator to view the graphical display");
	jlstOP.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	ListIterator li = lstOP.listIterator();
	while (li.hasNext()) {
	    lmOP.addElement((oclOperator)li.next());
	}
	jlstOP.setModel(lmOP);
	jlstOP.setSelectedIndex(0);
	jlstOP.addMouseListener(new MouseAdapter() {
	    public void mouseClicked(MouseEvent me) {
		if (me.getClickCount() == 2) {
		    int i = jlstOP.locationToIndex(me.getPoint());
		    oclOperator op = (oclOperator)jlstOP.getSelectedValue();
		    try {
			OperatorInstantiation pw = new OperatorInstantiation(curDomain, (oclOperator)op.clone(), drawingCanvas, top.strImageDir);
			pw.setLocation((int) (0.5 * getWidth()),(int) (0.5 * getHeight()));
			pw.show();
		    } catch (CloneNotSupportedException e){Utility.debugPrintln(e);}
		    seleIndex = jlstOP.getSelectedIndex();
		}
	    }
	});

	JScrollPane jscrollOP = new JScrollPane(jlstOP);

	jPanelOM = new JPanel();
	jPanelOM.setLayout(new GridLayout(0,1));
	jPanelOM.add(jscrollOP);

	jPanelOM.setBorder(BorderFactory.createTitledBorder("Operators List"));
	/* Weihong added on 28/11/2001 */
	splitPane2.add(jPanelOM);
	splitPane2.setRightComponent(jPanelOM);
    }

    /**
     * refresh state list after anychange
     * 
     */
    private void refreshStateList(){
	splitPane2.remove(jPanelOM);	/* Weihong added on 5/12/2001 */
	showOperatorLists();
	updateUI();
    }

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

    /*Weihong 14/3/02 */
    /**
     * print action text
     * 
     */
    private void jMI_PrintTextActionPerformed () {
	String header = "Gipo - Domain [" + curDomain.getName()
	    + "] Author [" +curDomain.getAuthor() +"]"; 
	HardcopyWriter hw;
	try { 
	    hw=new HardcopyWriter(top, header,10,.75,.5,.75,.5);}
	catch (HardcopyWriter.PrintCanceledException e) {
	    JOptionPane.showMessageDialog(this,
					  "Error Printing Action Sequence.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);
	    return;  
	}
            
	// Send output to it through a PrintWriter stream
	PrintWriter out = new PrintWriter(hw);
	oclPrintComponent(out,0,false);
	out.close();
    }

    /* Weihong 14/3/02 */
    /* 
     * Print the action sequence (Probably) to the printer
     */
    public void oclPrintComponent(PrintWriter ps,int indent,boolean nline) {
	int i = 0;
	ps.println("Stepper - action sequence");
	ps.println("Task ID - "+curTask);
	ps.println();
	ListIterator li = drawingCanvas.getCurActionList().listIterator();
	while (li.hasNext()) {
	    i++;
	    oclPredicate pred = (oclPredicate)li.next();
	    ps.println(i + ": " + pred.toString());
	}
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
     * draw initial and goal states on the canvas
     * 
     */
    private void jMI_AddInitialStateActionPerformed() {
	if (lmInitState.size() > 0) {
	    List listInit  = new ArrayList();
	    for (int i = 0; i < lmInitState.size(); i++){
		oclSS ss = (oclSS)lmInitState.get(i);
		listInit.add(ss);
	    }
	    List listGoal  = new ArrayList();
	    for (int j = 0; j < lmGoalState.size(); j++){
		oclSE se = (oclSE)lmGoalState.get(j);
		listGoal.add(se);
	    }
	    drawingCanvas.showOclSS(listInit, listGoal);
	}
	else {
	    JOptionPane.showMessageDialog(top,"No current states.",
                               "GIPO Error",JOptionPane.ERROR_MESSAGE,null);	 
	}
    }

    /**
     * redo the undo action
     * 
     */
    private void jMI_RedoActionPerformed () {
	try {
	    drawingCanvas.redo();
	} catch(CannotRedoException ex) {
	    JOptionPane.showMessageDialog(top,"Unable to redo: " + ex,
                                  "GIPO Error",JOptionPane.ERROR_MESSAGE,null);	 
	}
    }

    /**
     * load a graphics file .vm
     * 
     */
    private void jMI_OpenActionPerformed () {
	if (tempFile != null){
	    //ask if the user really want to save the current file first
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

    /**
     * read file from a BufferedReader
     * @param br BufferedReader
     * 
     */
    private void readFile(BufferedReader br) {
	drawingCanvas.loadFile(br);
	drawingCanvas.repaint();
    }

    /**
     * show next states after executing an operator
     * 
     */
    private void jMI_NextActionPerformed () {// to show the operator
	try {
	    drawingCanvas.redo();
	} catch(CannotRedoException ex) {
	    JOptionPane.showMessageDialog(this,"Unable to redo: " + ex,
                                           "GIPO Error",JOptionPane.ERROR_MESSAGE,null);	 
	}
    }

    /**
     * save graphics as a new .vm file
     * 
     */
    private void jMI_SaveAsGraphicsActionPerformed () {
	//JFileChooser chooser = new JFileChooser();
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
     * go to the previous step (states)
     * 
     */
    private void jMI_PreActionPerformed () {
	try {
	    drawingCanvas.undo();
	} catch(CannotUndoException ex) {
	    JOptionPane.showMessageDialog(this,"Unable to undo: " + ex,
                                           "GIPO Error",JOptionPane.ERROR_MESSAGE,null);	 
	}
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
	setSize(ww,hh);
	setVisible(true);
    }

    /**
     * save .vm file
     * @param ps PrintStream
     * 
     */
    private void saveVMFile(PrintStream ps) {

	ps.println("******** VISUAL MODELLER ********\n");
	ps.println("\n");
	ps.println("BEGIN WINDOW VARIBLES\n");
	ps.println("viewSize:" +viewSize.toString() + "\n");
	ps.println("END WINDOW VARIBLES\n");
	ps.println("\n");
	ps.println(drawingCanvas.to_String());
	ps.println("\n");

	ps.close();
    }


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
    private  JMenuItem jMI_PrintText; /*Weihong 14/3/02 */

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
    private StepperCanvas drawingCanvas;
    private Dimension viewSize;
    private double zoom = 1.5;

    private JFileChooser chooser = new JFileChooser();
    private File tempFile; //for managing the i/o

    private List lstTask; // This is all thetasks
    /**
     * parent frame
     */
    private OclEd top; /* Weihong added on 1/11/01 */
    private oclDomain curDomain;
    private List lstOP; //Operators
    private DefaultListModel lmOP; 
    private JList jlstOP;
    private JPanel jPanelOM;
 /* end Weihong added on 5/2/02 */
    private JPanel westPanel = new JPanel(); //panel to place at west

    // The tasks
    private JComboBox jcomTasks;
    private Vector vecTasks = new Vector();
    private oclTask curTask = null;

    // The initial state classes list box
    private JList jlstInitState;
    private DefaultListModel lmInitState; 
    private JScrollPane jscrollInitState;
    private int seleIndex; //the selected index of operators list, to use for saving the existing operator into the same pisition

    // The goal state classes list box
    private JList jlstGoalState;
    private DefaultListModel lmGoalState; 
    private JScrollPane jscrollGoalState;

    private JSplitPane splitPane1, splitPane2; /* Weihong added on 5/12/2001 */
}

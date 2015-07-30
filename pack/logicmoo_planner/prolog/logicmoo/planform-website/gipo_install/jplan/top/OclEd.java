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
 * OclEd.java
 * This is the top level control class for the OCL Editor Project
 * This Desk Top window implements a MDI top level class
 * Created on 21 February 2001
 */
 
package jplan.top;

import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import jplan.ocl.*;
import jplan.pddl.*;
import jplan.edexpt.*;
import jplan.graphics.transition.*;/* Weihong changed on 6/11/2001 */
import jplan.tools.stepper.StepperWindow; //WZ 22/05/2001
import jplan.tools.stepper.HStepperWindow; // WZ 10/5/02
import jplan.tools.animator.AnimatorWindow; //WZ 05/07/2001
import jplan.tools.animator.HAnimatorWindow; /* WZ 22/7/02 */
import jplan.tools.opmaker.OpMakerMain; /* Ron added 2/11/01 */
import jplan.tools.lifeHist.HistoryWindow; /* Ron added 12/10/04 */
import jplan.tools.sketch.SketchWindow; //Ron 27/06/03 the oclplus sketcher
import jplan.general.*;
import jplan.images.ImageLoader; /* WZ 5/9/2001 */
import java.awt.print.*;     /* Weihong 1/3/02 */


/** 
 * OclEd.java
 * This is the top level control class for the OCL Editor Project
 * This Desk Top window implements a MDI top level class
 * Created on 21 February 2001
 * @author  ron
 * @version 0
 */
public class OclEd extends JFrame{
	/**
	 * constants that identify the type of ocl domain loaded
	 */
	public static final int OCLFLAT = 1;
	public static final int OCLHIER = 2;
	public static final int OCLPLUS = 3;
	
//     // Interface Variables declaration
    /**
     * used as the parent of JInternalFrames to provide a pluggable
     * DesktopManager object to the GIPOInternalFrames. 
     */
    public JDesktopPane desktop;
    /**
     * DesktopManager objects owned by the desktop
     */
    public DesktopManager deskManager;
    private JMenuBar jmbDesktop;
    private JMenu fileMenu;
    private JMenu viewMenu;
    private JMenu editMenu;
    private JMenu planMenu;
    private JMenu toolsMenu;
    private JMenu windowsMenu;	/* WZ 22/10/2001 */
    private JMenu checkMenu;            /* Ron 1/6/01 */
    private JMenu helpMenu;

//    private JMenu optionsMenu;	/* WZ 13/8/02 */

    private JMenuItem newOCLMI;
    private JMenuItem openOCLMI;
    private JMenuItem openPDDLMI;
    private JMenuItem saveOCLMI;
    private JMenuItem saveAsOCLMI;
    private JMenuItem saveAsPDDLMI;
    private JMenuItem print; 	/* Weihong 1/3/02 */
    private JMenuItem exitMI;
    
    private JMenuItem textVMI;
    private JMenuItem textPDDLVMI;
    private JMenuItem nameVMI;
    private JMenuItem sortVMI;
    private JMenuItem predVMI;
    private JMenuItem stateVMI;
    private JMenuItem transitionVMI;	/* WZ 02/04/2001 */
    private JMenuItem compoundTransitionVMI; /* WZ 6/11/2001 */
    private JMenuItem atomInvarVMI;
    private JMenuItem negInvarVMI;
    private JMenuItem posInvarVMI;
    private JMenuItem taskVMI;
    private JMenuItem preferencesVMI;

    private JMenuItem historyMI; // Ron 12/10/04
    private JSeparator patternsSeparator;
    
    private JMenuItem algsMI;
    private JMenuItem runMI;
    private JMenuItem runAnotherMI;

    private JMenuItem checkSortsMI;      /* Ron 4/6/01 */
    private JMenuItem checkPredicatesMI;     /* Ron 4/6/01 */
    private JMenuItem checkStatesMI;     /* Ron 4/6/01 */
    private JMenuItem checkAtomicMI;
    private JMenuItem checkOpsMI;        /* Ron 4/6/01 */
    private JMenuItem checkMDMI; /* WZ 21/8/02 */
    private JMenuItem checkTasksMI;        /* Ron 14/7/01 */
    private JMenuItem checkAllMI;        /* Ron 14/7/01 */
    
    private JMenuItem groundMI;
    private JMenuItem convToPDDLMI;
    private JMenuItem reachabilityMI;    /* Ron 24/7/01 */
    private JMenuItem predUseMI;         /* Ron 5/10/01 */
    private JMenuItem checkTransMI; 		 /* Ron 6/4/03 */
    private JMenuItem genRandTasksMI;    /* Ron 31/8/01 */
    private JMenuItem stepperWindow;     /* WZ 22/05/2001 */
    private JMenuItem hStepperWindow;/* WZ 10/5/02 */
    private JMenuItem animatorWindow;     /* WZ 05/07/2001 */
    private JMenuItem hAnimatorWindow; /* WZ 22/7/02 */
    private JMenuItem opMakerMI;         /* Ron 2/11/01 */
    private JMenuItem planSketchMI;

    /* WZ 13/8/02 */
    private JCheckBoxMenuItem hierarchyMI; 	
    private GipoButton bt_method;
    private GipoButton bt_fStepper;
    private GipoButton bt_hStepper;
    private GipoButton bt_plusStepper;
    private GipoButton bt_fAnimator;
    private GipoButton bt_hAnimator;
    /* end 13/8/02 */

    /* WZ 22/10/2001 */
    private JMenuItem cascade;	
    private JMenuItem titleH;
    private JMenuItem titleV;	
    private JMenuItem arrangeIcon;
    /* end WZ 22/10/2001 */

    private JMenuItem userMI;
    private JMenuItem manMI;
    private JMenuItem tutorialMI; 
    private JMenuItem opmakerTutMI;
    private JMenuItem aboutMI;

    // Open Save Dialogues
    public OpenSaveDiags osDiags;
    
    /**
     * store the working directory of the ocl editor
     */
    public String strOCLPath;  
    /**
     * store root directory for jplan
     */
    public String strCodeBase;
    /**
     * store directory for images
     */
    public String strImageDir;
    /**
     * store the working directory for domains
     */
    public String strDomainsDir;
    /**
     * store the system properties
     */
    public Properties props;    
    /**
     * Store the Domain Model
     */
    public oclDomain curDomain = null;
    /**
     * Store the PDDL Domain Model
     */
    public pddlDomain curPDDLDomain = null;
    /**
     * Encapsulate calls to Prolog Tools
     */
    private PToolsControl pTools;
    /**
     * Store Planning algorithm runtime parameters / details
     */
    public RuntimeConfig plannerConfig;
    /**
     * to indicate whether some edited contents have not been saved yet.
     */
    public boolean flagDirty; /* WZ 12/10/2001 */

    private Hashtable windowList = new Hashtable(); /* WZ 24/10/2001 */
    private ButtonGroup menuButtonGroup; /* WZ 24/10/2001 */

    public boolean hierarchicalSwitch = false; /* Weihong 4/3/02 */
    
    public boolean rhsWarnings = true; // Ron 12/11/02 allow RHS warnings to be turned off
//  Following added Ron 25/06/03
    /**
     * Does version deal with oclplus [NOTE: not same as is domain oclplus]
     * Set in gipo properties file
     */
	public boolean oclplus = false;
     /** 
      * Creates new form OclEd - the top level constructor.
      */
     public OclEd() {
	flagDirty = false;  /* WZ 12/10/2001 */
	setDefaultCloseOperation(DO_NOTHING_ON_CLOSE); /* WZ 15/10/2001 */
	props = new Properties(System.getProperties());
	try {
	    strOCLPath = new String(System.getProperty("ocled.path"));
	    strCodeBase = new String(System.getProperty("ocled.codebase"));
	    strImageDir = strCodeBase + File.separator +
	    "jplan" + File.separator + "images" + File.separator;
	    props.load(new BufferedInputStream(new FileInputStream( 
				  strCodeBase +  
				  File.separator +
				  "jplan" + File.separator + 
				  "gipo.properties")));
	    System.setProperties(props);
	} catch (Exception e) {
	    JOptionPane.showMessageDialog(this,
		"The gipo.properties file cannot be found \n The System will not be fully operational. See your systems administrator.\n You are advised to exit the system.",
		 "GIPO Error",
		 JOptionPane.ERROR_MESSAGE,
		 null);	    
	}
	try {
		strDomainsDir = new String(System.getProperty("ocled.domains"));
	} catch (Exception ex) {
		if (strOCLPath != null) {
			strDomainsDir = strOCLPath + File.separator + "domains";
		}
	}
	Image SysIcon = ImageLoader.getImage(strImageDir + "gipog.gif", "gipog.gif");
	setIconImage(SysIcon);
	osDiags = new OpenSaveDiags(this);
	pTools = new PToolsControl(this);
	plannerConfig = new RuntimeConfig();

	/* Weihong 4/3/02 */
	String str = new String(System.getProperty("ocled.hierarchical"));
	if (str.equals("yes"))
	    hierarchicalSwitch = true;
	// Ron 12/11/02 allow rhs warnings to be switched off
	str = new String(System.getProperty("ocled.rhswarnings"));
	if (str.equals("no"))
	    rhsWarnings = false;
	//	Ron 25/06/03 permit oclPlus processing
	str = System.getProperty("ocled.oclplus");
	if (str != null && str.equals("yes"))
			 oclplus = true;
 	initComponents ();

	setSize( 1000, 800 );
	/* Weihong 7/3/02 */
	setLocation(getToolkit().getScreenSize().width/2 - getBounds().width/2, getToolkit().getScreenSize().height/2 - getBounds().height/2);
 	setVisible( true );
     }

     /**
      * This method is called from within the constructor to
      * initialize the form.
      */
     private void initComponents () {	
	 desktop = new JDesktopPane();
	 desktop.setDragMode(JDesktopPane.OUTLINE_DRAG_MODE);
	deskManager = desktop.getDesktopManager();
 	jmbDesktop = new JMenuBar ();
	// File manu
	fileMenu = new JMenu ("File");
	newOCLMI = new JMenuItem("New Domain ...");
	openOCLMI = new JMenuItem ("Open Domain ...");
	
	openPDDLMI = new JMenuItem ("Import PDDL Domain ...");
	openPDDLMI.setVisible(!hierarchicalSwitch);/* WZ 13/8/02 */
	saveOCLMI = new JMenuItem ("Save Current Domain");
	saveAsOCLMI = new JMenuItem ("Save Current Domain As ...");
	saveAsPDDLMI = new JMenuItem ("Export Current Domain As PDDL ...");
	saveAsPDDLMI.setVisible(!hierarchicalSwitch);/* WZ 13/8/02 */
	print = new JMenuItem ("Print current Domain");
	exitMI = new JMenuItem ("Exit");

	viewMenu = new JMenu ("View");
	textVMI = new JMenuItem("View Plain Text");
	textPDDLVMI = new JMenuItem("View PDDL Plain Text");

	editMenu = new JMenu("Edit");
	historyMI = new JMenuItem("Object Life Histories ..."); 
	nameVMI = new JMenuItem("Domain Properties ..");
	sortVMI = new JMenuItem("Sorts ...");
	predVMI = new JMenuItem("Predicates ...");
	stateVMI = new JMenuItem("States ...");
	transitionVMI =  new JMenuItem("Primitive Transitions ..."); /* WZ 02/04/2001 */
	compoundTransitionVMI = new JMenuItem("Compound Transitions ..."); /* WZ 6/11/2001 */
	atomInvarVMI = new JMenuItem("Atomic Invariants ...");
	negInvarVMI = new JMenuItem("Negative Invariants ...");
	posInvarVMI = new JMenuItem("Implied Invariants ...");
	taskVMI = new JMenuItem("Tasks ...");
	preferencesVMI = new JMenuItem("Preferences ...");
	/* Ron 4/6/01 */
	checkMenu = new JMenu("Validation");
	checkSortsMI = new JMenuItem("Sorts");
	checkPredicatesMI = new JMenuItem("Predicates");
	checkStatesMI = new JMenuItem("States");
	checkAtomicMI = new JMenuItem("Atomic Invarients");
	checkOpsMI = new JMenuItem("Operators");
	checkMDMI = new JMenuItem("Methods"); /* WZ 21/8/02 */
	checkTasksMI = new JMenuItem("Tasks");
	checkAllMI = new JMenuItem("All Checks");
	/* END Ron 4/6/01 */
	planMenu = new JMenu ("Plan");
	algsMI = new JMenuItem ("Select Planning Algorithm ...");
	runMI = new JMenuItem ("Run Planner ...");
	runAnotherMI = new JMenuItem("Run Planner Again ...");

	toolsMenu = new JMenu ("Tools");
	groundMI = new JMenuItem("Ground Domain Operators");
	convToPDDLMI = new JMenuItem("Convert to PDDL");
	reachabilityMI = new JMenuItem("Check Operator State Use"); // Ron 24/7/01
	predUseMI = new JMenuItem("Check predicate Use"); // Ron 5/10/01
	checkTransMI = new JMenuItem("Check Method Transparency");
	genRandTasksMI = new JMenuItem("Generate Random Tasks"); // Ron 31/8/01
	stepperWindow = new JMenuItem("Stepper (Flat)"); /* WZ 22/05/2001 */
	hStepperWindow = new JMenuItem("Stepper (Hierarchical)"); /* WZ 10/5/02 */
	animatorWindow = new JMenuItem("Animator");     /* WZ 05/07/2001 */
	hAnimatorWindow= new JMenuItem("Animator (Hierarchical)"); /* WZ 22/7/02 */
	opMakerMI = new JMenuItem("Op Maker"); /* Ron added 2/11/01 */
	planSketchMI = new JMenuItem("Stepper (oclPlus)"); // Ron 27/06/03 oclPlus sketcher
	/* WZ 13/8/02 */
//	optionsMenu = new JMenu ("Options");
	hierarchyMI = new JCheckBoxMenuItem("Hierarchical Domains"); 
	hierarchyMI.setState(hierarchicalSwitch);
	/* end 13/8/02 */

	/* WZ 22/10/2001 */
	windowsMenu = new JMenu ("Windows");
	cascade = new JMenuItem("Cascade"); 
	titleH = new JMenuItem("Title Horizontally"); 
	titleV = new JMenuItem("Title Vertically"); 
	arrangeIcon = new JMenuItem("Arrange Icon"); 
	/* end WZ 22/10/2001 */
	helpMenu = new JMenu("Help");
	userMI = new JMenuItem("User Manual ...");
	manMI = new JMenuItem ("OCL Manual ...");
	tutorialMI = new JMenuItem ("Tutorial ...");
	opmakerTutMI = new JMenuItem("Op Maker ...");
	aboutMI = new JMenuItem ("About ...");

	// The File Sub Menu  
 	fileMenu.add (newOCLMI); //hook for Salford's code
 	fileMenu.add(new JSeparator());
	fileMenu.add (openOCLMI);
	fileMenu.add (openPDDLMI);
	fileMenu.add (new JSeparator());
	fileMenu.add (saveOCLMI);
	fileMenu.add (saveAsOCLMI);
	fileMenu.add (saveAsPDDLMI);
	fileMenu.add (new JSeparator());
	fileMenu.add (print); /* Weihong 1/3/02 */
	fileMenu.add (new JSeparator());

	newOCLMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    osDiags.newOCLDomain ();
		    // mark the creation time/date
		    if (curDomain != null)
		    	curDomain.setDateCreated(Calendar.getInstance().getTime().toString());
		}
	    }
				     );
	openOCLMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    osDiags.loadOCLFile ();
		    if (curDomain == null)
		    	return;
		    if (!curDomain.lifeHistoryFileName.equals("none")) {
				HistoryWindow patMan = new HistoryWindow("Life Histories",curDomain,OclEd.this);
		    	desktop.add(patMan);
		    	deskManager.activateFrame(patMan);
		    	patMan.show();
		    }
		}
	    }
				     );

	openPDDLMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    osDiags.loadPDDLFile ();
		}
	    }
				     );
	saveOCLMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    int res = osDiags.saveFile ();
		    flagDirty = false;  /* WZ 12/10/2001 */
		    /* WZ 18/10/2001 */
		    // mark the modified time/date
		    curDomain.setDateModified(Calendar.getInstance().getTime().toString());
		}
	    }
				     );
	saveAsOCLMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    int res = osDiags.saveAsFile (OpenSaveDiags.OCL);
		    flagDirty = false;  /* WZ 12/10/2001 */
		}
	    }
				     );
	saveAsPDDLMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    int res = osDiags.saveAsFile(OpenSaveDiags.PDDL);
		}
	    }
				     );
	print.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    printDomain ();
		}
	    }
				     );
	exitMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    exitMIActionPerformed (evt);
		}
	    }				  );
 	fileMenu.add (exitMI);
 	jmbDesktop.add (fileMenu);

	// View menu 
	textVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) { 
		    /* WZ 6/9/2001 */
		    viewOclText();
		}
	    }
				  );
	// PDDL View menu 
	textPDDLVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    ViewPDDLText vt = new ViewPDDLText(OclEd.this);
		    desktop.add(vt,BorderLayout.NORTH);
		    deskManager.activateFrame(vt);
		}
	    }
				  );

	// Change Domain name 
	nameVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    nameVMIactionPerformed();
		}
	    }
				  );
	// Use Object Life Histories // Ron 12/10/04
	historyMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    historyMIactionPerformed();
		}
	    }
				  );
	// Sort View menu 
	sortVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    sortVMIactionPerformed();
		}
	    }
				  );
	// Predicate View menu 
	predVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    predVMIactionPerformed();
		}
	    }
				  );
	// Sub-State-Classes View menu 
	stateVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    stateVMIactionPerformed();
		}
	    }
				  );

	/* WZ 6/11/2001 */
	//compound transition View menu 
	compoundTransitionVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    compoundTransitionVMIactionPerformed();
		}
	    }
					 );

	/* WZ 02/04/2001 */
	//transition View menu 
	transitionVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    transitionVMIactionPerformed();
		}
	    }
					 );

	// Atomic Invariants View menu 
	atomInvarVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    atomicInvarVMIactionPerformed();
		}
	    }
				  );
	// inconsistent_constraint View 
	negInvarVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    negInvarVMIactionPerformed();
		}
	    }
				  );
	// implied_invariants View 
	posInvarVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    impliedInvarVMIactionPerformed();
		}
	    }
				  );
	// Task View 
	taskVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    taskVMIactionPerformed();
		}
	    }
				  );
	// Preferences 
	preferencesVMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    Preferences pref = new Preferences(OclEd.this);
		    pref.show();
		}
	    }
				  );
	viewMenu.add(textVMI);
	viewMenu.add(textPDDLVMI);
	jmbDesktop.add (viewMenu);

	editMenu.add(nameVMI);
	editMenu.add(new JSeparator());
	editMenu.add(historyMI);
	patternsSeparator = new JSeparator();
	editMenu.add(patternsSeparator);
	editMenu.add(sortVMI);
	editMenu.add(predVMI);
	editMenu.add(stateVMI);
	editMenu.add(compoundTransitionVMI);	/* WZ 6/11/2001 */
	compoundTransitionVMI.setVisible(hierarchicalSwitch);/* Weihong 4/3/02 */

	editMenu.add(transitionVMI);	/* WZ 02/04/2001 */
	editMenu.add(new JSeparator());
	editMenu.add(atomInvarVMI);
	editMenu.add(negInvarVMI);
	editMenu.add(posInvarVMI);
	editMenu.add(new JSeparator());
	editMenu.add(taskVMI);
	editMenu.add(new JSeparator());
	editMenu.add(preferencesVMI);
	jmbDesktop.add (editMenu);

	/* Ron 4/6/01 The Check/Verfiy Menu*/
	checkSortsMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (curDomain != null) {
			checkSortsMIActionPerformed();
		    }
		}
	    }
				  );
	checkPredicatesMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (curDomain != null) {
			checkPredicatesMIActionPerformed();
		    }
		}
	    }
				  );
	checkStatesMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (curDomain != null) {
			checkStatesMIActionPerformed();
		    }
		}
	    }
				  );
	checkAtomicMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (curDomain != null) {
			checkAtomicMIActionPerformed();
		    }
		}
	    }
				  );
	checkOpsMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (curDomain != null) {
			checkOpsMIActionPerformed();
		    }
		}
	    }
				  );
	/* WZ 21/8/02 */
	checkMDMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (curDomain != null) {
			checkMDMIActionPerformed();
		    }
		}
	    }
				  );
	checkTasksMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (curDomain != null) {
			checkTasksMIActionPerformed();
		    }
		}
	    }
				  );
	checkAllMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (curDomain != null) {
			checkAllMIActionPerformed();
		    }
		}
	    }
				  );
	checkMenu.add(checkSortsMI);
	checkMenu.add(checkPredicatesMI);
	checkMenu.add(checkStatesMI);
	checkMenu.add(checkAtomicMI);
	checkMenu.add(checkOpsMI);
	checkMenu.add(checkMDMI); /* WZ 21/8/02 */
	checkMDMI.setVisible(hierarchicalSwitch);/* WZ 21/8/02 */
	checkMenu.add(checkTasksMI);
	checkMenu.add(new JSeparator());
	checkMenu.add(checkAllMI);
	
	jmbDesktop.add (checkMenu);

	// The Plan menu items - these call up Prolog Planning algorithms
	algsMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    algsMIActionPerformed ();
		}
	    }
				  );
	runMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    pTools.runPlanner();
		}
	    }
				  );
	runAnotherMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    pTools.runAnother();
		}
	    }
				  );
 	planMenu.add (algsMI);
 	planMenu.add(runMI);
	planMenu.add(runAnotherMI);
 	jmbDesktop.add (planMenu);

	// Tools Menu items 
	groundMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    pTools.groundOpsTool();
		}
	    }
				  );
	convToPDDLMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    pTools.ocl2pddl();
		}
	    }
				  );
	// generate random tasks 
	genRandTasksMI.addActionListener (new java.awt.event.ActionListener() {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    pTools.genRandomTasks();
		}
	    }
				  );

	 // Reachability Analysis
	reachabilityMI.addActionListener (new java.awt.event.ActionListener() {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    ViewReachability vt = 
			new ViewReachability(OclEd.this,ViewReachability.REACH);
		    desktop.add(vt,BorderLayout.NORTH);
		    deskManager.activateFrame(vt);
		}
	    }
				  );
	
	// Predicate Use Analysis
	predUseMI.addActionListener (new java.awt.event.ActionListener() {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    ViewReachability vt = 
			new ViewReachability(OclEd.this,ViewReachability.PREDUSE);
		    desktop.add(vt,BorderLayout.NORTH);
		    deskManager.activateFrame(vt);
		}
	    }
				  );
	checkTransMI.addActionListener (new java.awt.event.ActionListener() {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
			java.util.List mssgs = new ArrayList();
			mssgs = (java.util.List)curDomain.checkTransparency();
			CheckResultFrame.showMessages(OclEd.this,mssgs,"Transparency Check Messages");
		}
		}
			);
	/* WZ 22/05/2001 */
	stepperWindow.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    showStepperWindow();
		}
	    }
				  );

	/* WZ 10/5/02 */
	hStepperWindow.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    showHierarchicalStepperWindow();
		}
	    }
				  );

	/* WZ 05/07/2001 */
	animatorWindow.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    showAnimatorWindow();
		}
	    }
				  );

	/* WZ 22/7/02 */
	hAnimatorWindow.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    showHierarchicalAnimatorWindow();
		}
	    }
				  );

	/* Ron added 2/11/01 */
	opMakerMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    showOpMakerMain();
		}
	    }
				  );
	/* Ron added 27/06/03 oclPlus sketcher */
	planSketchMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
			showSketchWindow();
		}
	});
	//Ron 10/10/01 removed these though left the hooks etc
	// all that is needed is to uncomment these to re-enable
	toolsMenu.add(genRandTasksMI);  // Ron 31/8/01
	toolsMenu.addSeparator();
	toolsMenu.add(reachabilityMI);
	toolsMenu.add(predUseMI);
	toolsMenu.addSeparator();	/* WZ 22/05/2001 */
 	toolsMenu.add(stepperWindow);	/* WZ 22/05/2001 */
	genRandTasksMI.setVisible(!hierarchicalSwitch);
	stepperWindow.setVisible(!hierarchicalSwitch);/* WZ 22/7/02 */
 	toolsMenu.add(hStepperWindow);/* WZ 10/5/02 */
	hStepperWindow.setVisible(hierarchicalSwitch);/* WZ 1/7/02 */
 	toolsMenu.add(animatorWindow);  /* WZ 05/07/2001 */
	animatorWindow.setVisible(!hierarchicalSwitch);/* WZ 22/7/02 */
 	toolsMenu.add(hAnimatorWindow);
	hAnimatorWindow.setVisible(hierarchicalSwitch);/* WZ 22/7/02 */
	toolsMenu.add(checkTransMI); /* Ron 6/4/03 */
	checkTransMI.setVisible(hierarchicalSwitch); /* Ron 6/4/03 */
	toolsMenu.add(opMakerMI);       /* Ron added 2/11/01 */
	opMakerMI.setVisible(!hierarchicalSwitch);/* WZ 21/8/02 */
	toolsMenu.add(planSketchMI);
	planSketchMI.setVisible(oclplus);
 	jmbDesktop.add (toolsMenu);

	/* WZ 22/10/2001 */
	cascade.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    casCade(desktop);
		}
	    }
				  );

	titleH.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    tile(desktop, true);
		}
	    }
				  );

	titleV.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    tile(desktop, false);
		}
	    }
				  );

	arrangeIcon.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    arrangeIcons(desktop);
		}
	    }
				  );

	/* WZ 13/8/02 */
//	optionsMenu.add(hierarchyMI);
//	hierarchyMI.addActionListener (new java.awt.event.ActionListener () {
//		public void actionPerformed (java.awt.event.ActionEvent evt) {
//		    hierarchicalSwitch = hierarchyMI.getState();
//		    updateMenuBar(hierarchicalSwitch);
//		}
//	    }
//				  );    
	/* end 13/8/02 */
// 	jmbDesktop.add (optionsMenu);

	windowsMenu.add(cascade);
	windowsMenu.add(titleH);
	windowsMenu.add(titleV);
 	windowsMenu.add(arrangeIcon);  
 	jmbDesktop.add (windowsMenu);
	/* end WZ 22/10/2001 */

	userMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    Manual manWin = new Manual(strImageDir, "user.html");
		}
	    }
				  );    
	tutorialMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
//			Process p = null;
//			try {
//				// Now run the command
//				p = Runtime.getRuntime().exec("netscape jplan/docs/tutorial.html");
//			} catch (java.io.IOException e) {
//				JOptionPane.showMessageDialog(
//					null,
//					"Can't run netscape.\n" + "Using internal viewer. \n",
//					"GIPO Error",
//					JOptionPane.ERROR_MESSAGE,
//					null);
//		    	Manual manWin = new Manual(strImageDir, "tutorial.html");
//			}
			Manual manWin = new Manual(strImageDir, "tutorial.html");
		}
	    }
				  );    	
	opmakerTutMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    Manual manWin = new Manual(strImageDir, "Opmaker.html");
		}
	    }
				  );    
	manMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    Manual manWin = new Manual(strImageDir, "content.html");
		}
	    }
				  );    

	/* WZ 30/08/2001 */
	aboutMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    About aboutWindow = new About(OclEd.this);
		    aboutWindow.setLocation((int) (0.5 *getWidth() - 0.5 *aboutWindow.getWidth()),(int) (0.5 * (getHeight()-aboutWindow.getHeight())));
		    aboutWindow.show();
		}
	    }
				  );    
	/* Weihong added end */

 	jmbDesktop.add(Box.createHorizontalGlue());
	helpMenu.add(userMI);
	helpMenu.add(tutorialMI);
	helpMenu.add(opmakerTutMI);
	helpMenu.add(manMI);
 	helpMenu.add(aboutMI);

 	jmbDesktop.add (helpMenu);
	jmbDesktop.add(Box.createHorizontalStrut(100));


// 	// End Menu Construction
	setTitle ("GIPO - Planning with Objects");
	addWindowListener (new java.awt.event.WindowAdapter () {
		public void windowClosing (java.awt.event.WindowEvent evt) {
		    exitForm ();	/* Weihong added/changed on 12/10/2001 */
		}
	    }
			   );

 	setJMenuBar (jmbDesktop);
	getContentPane().add(desktop);

	/* Weihong added/changed on 6/9/2001 */
	//adding a toolbar
	JToolBar jToolBar1 = new JToolBar();
// 	jToolBar1.setBorder (new javax.swing.border.BevelBorder(0));/* Weihong changed on 5/12/2001 */
	JButton bt = null;

	//open ocl domain
	ImageIcon ii = ImageLoader.getImageIcon(strImageDir, "Open16.gif");
	bt = new GipoButton("Open ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Open");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		osDiags.loadOCLFile ();
		if (!(curDomain == null) && !curDomain.lifeHistoryFileName.equals("none")) {
			HistoryWindow patMan = new HistoryWindow("Life Histories",curDomain,OclEd.this);
	    	desktop.add(patMan);
	    	deskManager.activateFrame(patMan);
	    	patMan.show();
	    }
	    }
	});
	jToolBar1.add(bt);

	//save ocl domain
	ii = ImageLoader.getImageIcon(strImageDir, "Save16.gif");
	bt = new GipoButton("Save ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Save");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		int res = osDiags.saveFile ();
		flagDirty = false;  /* WZ 12/10/2001 */
	    }
	});
	jToolBar1.add(bt);

	//view ocl domain
	ii = ImageLoader.getImageIcon(strImageDir, "Edit16.gif");
	bt = new GipoButton("View ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("View");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		viewOclText();
	    }
	});
	jToolBar1.add(bt);

	/* Weihong 1/3/02 print domain */

	jToolBar1.addSeparator();

	ii = ImageLoader.getImageIcon(strImageDir, "Print16.gif");
	bt = new GipoButton("Print ", ii);
	bt.setToolTipText("Print current domain");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		printDomain();
	    }
	});
	jToolBar1.add(bt);

	jToolBar1.addSeparator();

	//Edit sort
	ii = ImageLoader.getImageIcon(strImageDir, "Sort16.gif");
	bt = new GipoButton("Sorts ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Sorts");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		sortVMIactionPerformed();
	    }
	});
	jToolBar1.add(bt);

	//Edit Predicate
	ii = ImageLoader.getImageIcon(strImageDir, "Predicate16.gif");
	bt = new GipoButton(" Predicates ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Predicates");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		predVMIactionPerformed();
	    }
	});
	jToolBar1.add(bt);

	//Edit States
	ii = ImageLoader.getImageIcon(strImageDir, "State16.gif");
	bt = new GipoButton(" States ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("States");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		stateVMIactionPerformed();
	    }
	});
	jToolBar1.add(bt);

	/* WZ 5/12/2001 */
	//Edit Transitions
	ii = ImageLoader.getImageIcon(strImageDir, "CTransition16.gif");
	bt_method = new GipoButton("C Trans ", ii);/* Weihong changed on 5/12/2001 */
	bt_method.setToolTipText("Compound Transitions");
	bt_method.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		compoundTransitionVMIactionPerformed();
	    }
	});
	jToolBar1.add(bt_method);
	bt_method.setVisible(hierarchicalSwitch);/* Weihong 4/3/02 */
	/* end WZ 5/12/2001 */

	//Edit Transitions
	ii = ImageLoader.getImageIcon(strImageDir, "Transition16.gif");
	bt = new GipoButton("P Trans ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Primitive Transitions");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		transitionVMIactionPerformed();
	    }
	});
	jToolBar1.add(bt);

	//Edit Tasks
	ii = ImageLoader.getImageIcon(strImageDir, "Task16.gif");
	bt = new GipoButton("Tasks ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Tasks");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		taskVMIactionPerformed();
	    }
	});
	jToolBar1.add(bt);

	jToolBar1.addSeparator();

	//all checks
	ii = ImageLoader.getImageIcon(strImageDir, "Check16.gif");
	bt = new GipoButton("Check ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Global Check");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		if (curDomain != null) 
		    checkAllMIActionPerformed();
	    }
	});
	jToolBar1.add(bt);

	jToolBar1.addSeparator();

	//Set planning algorithm
	ii = ImageLoader.getImageIcon(strImageDir, "Algorithm16.gif");
	bt = new GipoButton("Algorithm ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Set planning algorithm");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		algsMIActionPerformed ();
	    }
	});
	jToolBar1.add(bt);

	//Run planner
	ii = ImageLoader.getImageIcon(strImageDir, "Run16.gif");
	bt = new GipoButton("Run ", ii);/* Weihong changed on 5/12/2001 */
	bt.setToolTipText("Run planner");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		pTools.runPlanner();
	    }
	});
	jToolBar1.add(bt);

	jToolBar1.addSeparator();

	//stepper (flat)
	ii = ImageLoader.getImageIcon(strImageDir, "Stepper16.png");
	bt_fStepper = new GipoButton("F Stepper ", ii);/* Weihong changed on 5/12/2001 */
	bt_fStepper.setToolTipText("Run Flat Stepper");
	bt_fStepper.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		showStepperWindow();
	    }
	});
	jToolBar1.add(bt_fStepper);
	bt_fStepper.setVisible(!hierarchicalSwitch);/* WZ 22/7/02 */

	/* WZ 10/5/02 */
	//stepper (hierarchical)
	ii = ImageLoader.getImageIcon(strImageDir, "Stepper16.png");
	bt_hStepper = new GipoButton("H Stepper ", ii);/* Weihong changed on 5/12/2001 */
	bt_hStepper.setToolTipText("Run Hierarchical Stepper");
	bt_hStepper.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		showHierarchicalStepperWindow();
	    }
	});
	jToolBar1.add(bt_hStepper);
	bt_hStepper.setVisible(hierarchicalSwitch);/* WZ 8/7/02 */
	
	//	stepper (oclPlus) // Ron
	ii = ImageLoader.getImageIcon(strImageDir, "Stepper16.png");
	bt_plusStepper = new GipoButton("Process Stepper ", ii);
	bt_plusStepper.setToolTipText("Run Process Stepper");
	bt_plusStepper.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
	    	showSketchWindow();
	    }
	});
	jToolBar1.add(bt_plusStepper);
	bt_plusStepper.setVisible(false);

	//animator
	ii = ImageLoader.getImageIcon(strImageDir, "Animator16.gif");
	bt_fAnimator = new GipoButton("Animator ", ii);/* Weihong changed on 5/12/2001 */
	bt_fAnimator.setToolTipText("Run Animator");
	bt_fAnimator.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		showAnimatorWindow();
	    }
	});
	jToolBar1.add(bt_fAnimator);
	bt_fAnimator.setVisible(!hierarchicalSwitch);/* WZ 22/7/02 */

	//animator
	ii = ImageLoader.getImageIcon(strImageDir, "Animator16.gif");
	bt_hAnimator = new GipoButton("H Animator ", ii);/* Weihong changed on 5/12/2001 */
	bt_hAnimator.setToolTipText("Run Hierarchical Animator");
	bt_hAnimator.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		showHierarchicalAnimatorWindow();
	    }
	});
	jToolBar1.add(bt_hAnimator);
	bt_hAnimator.setVisible(hierarchicalSwitch);/* WZ 22/7/02 */

	getContentPane().add(jToolBar1, "North");
	/* Weihong added end 6/9/2001 */
     }

    /* WZ 13/8/02 */
    // Ron 5/5/03 made public and changed
    public void updateMenuBar(int domType){
    	switch (domType) {
    	case OCLHIER:
    		bt_method.setVisible(true);
    		compoundTransitionVMI.setVisible(true);
    		stepperWindow.setVisible(false);
    		hStepperWindow.setVisible(true);
    		animatorWindow.setVisible(false);
    		hAnimatorWindow.setVisible(true);
    		bt_fStepper.setVisible(false);
    		bt_hStepper.setVisible(true);
    		bt_fAnimator.setVisible(false);
    		bt_hAnimator.setVisible(true);
    		openPDDLMI.setVisible(false);
    		saveAsPDDLMI.setVisible(true);
    		checkMDMI.setVisible(false); 
    		opMakerMI.setVisible(false);
    		checkTransMI.setVisible(true);
    		genRandTasksMI.setVisible(false);
    		planSketchMI.setVisible(false);
    		bt_plusStepper.setVisible(false);
    		break;
    	case OCLPLUS:
    		bt_method.setVisible(false);
    		compoundTransitionVMI.setVisible(false);
    		stepperWindow.setVisible(false);
    		hStepperWindow.setVisible(false);
    		animatorWindow.setVisible(false);
    		hAnimatorWindow.setVisible(false);
    		bt_fStepper.setVisible(false);
    		bt_hStepper.setVisible(false);
    		bt_fAnimator.setVisible(false);
    		bt_hAnimator.setVisible(false);
    		openPDDLMI.setVisible(false);
    		saveAsPDDLMI.setVisible(true);
    		checkMDMI.setVisible(false); 
    		opMakerMI.setVisible(false);
    		checkTransMI.setVisible(false);
    		genRandTasksMI.setVisible(false);
    		planSketchMI.setVisible(true);
    		bt_plusStepper.setVisible(true);
    		break;
    	case OCLFLAT:
    		bt_method.setVisible(false);
    		compoundTransitionVMI.setVisible(false);
    		stepperWindow.setVisible(true);
    		hStepperWindow.setVisible(false);
    		animatorWindow.setVisible(true);
    		hAnimatorWindow.setVisible(false);
    		bt_fStepper.setVisible(true);
    		bt_hStepper.setVisible(false);
    		bt_fAnimator.setVisible(true);
    		bt_hAnimator.setVisible(false);
    		openPDDLMI.setVisible(true);
    		saveAsPDDLMI.setVisible(true);
    		checkMDMI.setVisible(false); 
    		opMakerMI.setVisible(true);
    		checkTransMI.setVisible(false);
    		genRandTasksMI.setVisible(true);
    		planSketchMI.setVisible(false);
    		bt_plusStepper.setVisible(false);
    		break;
    	default:
    		bt_method.setVisible(false);
    		compoundTransitionVMI.setVisible(false);
    		stepperWindow.setVisible(true);
    		hStepperWindow.setVisible(false);
    		animatorWindow.setVisible(true);
    		hAnimatorWindow.setVisible(false);
    		bt_fStepper.setVisible(true);
    		bt_hStepper.setVisible(false);
    		bt_fAnimator.setVisible(true);
    		bt_hAnimator.setVisible(false);
    		openPDDLMI.setVisible(true);
    		saveAsPDDLMI.setVisible(true);
    		checkMDMI.setVisible(false); 
    		opMakerMI.setVisible(true);
    		checkTransMI.setVisible(false);
    		genRandTasksMI.setVisible(true);
    		planSketchMI.setVisible(false);
    		bt_plusStepper.setVisible(false);
    		break;
    	}
    }

    /* WZ 6/9/2001 */
    private void viewOclText(){
	ViewOCLText vt = new ViewOCLText(OclEd.this);
	desktop.add(vt,BorderLayout.NORTH);
	deskManager.activateFrame(vt);
    }
    
    private void nameVMIactionPerformed() {
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(this,
					  "No current domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);
	    return;
	}

	/* WZ 17/10/2001 */
	DomainProperties domainProp = new DomainProperties(this, curDomain);
	domainProp.setLocation((int) (0.5 *getWidth() - 0.5 *domainProp.getWidth()),(int) (0.5 * (getHeight()-domainProp.getHeight())));
	domainProp.show();
    }
    
    /* Weihong 1/3/02 */
    /**
     * Print out the current domain
     */
    private void printDomain (){
	String header = "Gipo - Domain [" + curDomain.getName()
	    + "] Author [" +curDomain.getAuthor() +"]"; 
	HardcopyWriter hw;
	try { 
	    hw=new HardcopyWriter(this, header,8,.75,.5,.75,.5);}
	catch (HardcopyWriter.PrintCanceledException e) {
	    JOptionPane.showMessageDialog(this,
					  "Error Printing Domain.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);
	    return;  
	}
            
	// Send output to it through a PrintWriter stream
	PrintWriter out = new PrintWriter(hw);
	curDomain.oclPrintComponent(out,0,false);
	out.close();
    }

    private void sortVMIactionPerformed() {
	SortView sortView = new SortView(this.curDomain,OclEd.this);
	desktop.add(sortView);
	deskManager.activateFrame(sortView);
    }

    private void predVMIactionPerformed() {
	PredicateView predView = new PredicateView(this.curDomain,this);
	desktop.add(predView);
	deskManager.activateFrame(predView);
    }

    private void stateVMIactionPerformed() {
	StateView stateView = new StateView(this.curDomain,this);
	desktop.add(stateView);
	deskManager.activateFrame(stateView);
	try {/* WZ 21/8/02 */
	    stateView.setMaximum(true);
	} catch (java.beans.PropertyVetoException e) {
	}
    }
    
    /* WZ 6/11/2001 */
    private void compoundTransitionVMIactionPerformed(){
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
	HighLevelTransitionWindow methodView = new HighLevelTransitionWindow(this.curDomain,this);
	desktop.add(methodView);
	deskManager.activateFrame(methodView);
	Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
	try {
	    methodView.setMaximum(true);
	} catch (java.beans.PropertyVetoException e) {
	}
    }

    /*Weihong 12/3/02 */
    /**
     * to show an internal frame in this frame.
     * return void
     */
    public void callInternalFrame(JInternalFrame frameToCall, boolean maxi){
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
	desktop.add(frameToCall);
	deskManager.activateFrame(frameToCall);
	Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
	if (maxi){
	    try {
		frameToCall.setMaximum(true);
	    } catch (java.beans.PropertyVetoException e) {
	    }
	}
    }

    /* WZ 02/04/2001 */
    private void transitionVMIactionPerformed(){
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);/* WZ 22/10/2001 */
	ActionWindow actionView = new ActionWindow(this.curDomain,this);
	desktop.add(actionView);
	deskManager.activateFrame(actionView);
	Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);/* WZ 22/10/2001 */
	// Ron 4/10/01
	try {
	    actionView.setMaximum(true);
	} catch (java.beans.PropertyVetoException e) {
	}
    }

    private void atomicInvarVMIactionPerformed() {
	AtomicInvarView atomicView = 
	    new AtomicInvarView(this.curDomain,this);
	desktop.add(atomicView);
	deskManager.activateFrame(atomicView);
    }
    private void negInvarVMIactionPerformed() {
	NegInvarView negView = new NegInvarView(this.curDomain,this);
	desktop.add(negView);
	deskManager.activateFrame(negView);
    }
    private void impliedInvarVMIactionPerformed() {
	ImpInvarView impView = new ImpInvarView(this.curDomain,this);
	desktop.add(impView);
	deskManager.activateFrame(impView);
    }
    private void taskVMIactionPerformed() {
	//Ron 6/9/01 Added precondition checks
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	java.util.List mssgs = curDomain.checkStates();
	if (mssgs.size() != 0) {
	    checkResultsDialog(mssgs,"WARNING these results may impair the \nfunctioning of the Task Editor."); 
	} 
	//Ron end 6/9/01
	TaskView taskView = new TaskView(this.curDomain,this);
	desktop.add(taskView);
	deskManager.activateFrame(taskView);
	// Ron 4/10/01
	try {
	    taskView.setMaximum(true);
	} catch (java.beans.PropertyVetoException e) {
	}
    }

    // Ron 1/6/01
    /**
     * used to display a success message if all checks passed
     * otherwise display a scrolling list of error messages
     * indicating checks failed.
     * @param mssgs the list of error messages
     * @param successMessage the success message
     */
    public void checkResultsDialog(java.util.List mssgs,
				    String successMessage) {
	if (mssgs.size() == 0) {
	    JOptionPane.showMessageDialog(this,
		successMessage,
		 "GIPO Message",
		 JOptionPane.INFORMATION_MESSAGE,
		 null);
	} else {
	    JPanel jpanMssgs = new JPanel();
	    jpanMssgs.setBorder(BorderFactory.createTitledBorder("Check Messages"));
	    jpanMssgs.setLayout(new BorderLayout());
	    JScrollPane jScrollPane = new JScrollPane();
	    JTextArea jtxtMessages = new JTextArea ();
	    jtxtMessages.setLineWrap (true);
	    jtxtMessages.setEditable (false);
	    jScrollPane.setViewportView(jtxtMessages);
	    jpanMssgs.add(jScrollPane, java.awt.BorderLayout.CENTER);
	    jpanMssgs.setPreferredSize(new Dimension(400,200));
	    java.util.ListIterator li = mssgs.listIterator();
	    jtxtMessages.setText("");
	    while (li.hasNext()) {
		jtxtMessages.append((String)li.next() +"\n");
	    }
	    JOptionPane.showMessageDialog(this,
		jpanMssgs,
		 "GIPO Information",
		 JOptionPane.ERROR_MESSAGE,
		 null);
	    
	}
    }

    /* Donghong changed 25/10/02 */
    private void checkSortsMIActionPerformed() {
	java.util.List mssgs = new ArrayList();
	try {
	    mssgs = curDomain.checkSortTree();
	    if (mssgs.size() >= 2) {
	    	CheckResultFrame.showMessages(this,mssgs,"Sorts Check Messages");	
	    } else {
	    	checkResultsDialog(mssgs,"All sort checks passed.");
	    }
	} catch (OCLException e) {
	    checkResultsDialog(mssgs,"There are no sorts defined for the domain.");
	}
	
    }
    /* Donghong changed 25/10/02 */
    private void checkPredicatesMIActionPerformed() {
  	java.util.List mssgs = new ArrayList();
    mssgs = curDomain.checkPredicates();
	if (mssgs.size() < 2) {
	    checkResultsDialog(mssgs,"All predicate checks passed.");	
	} else {
	    CheckResultFrame.showMessages(OclEd.this,mssgs,"Predicates Check Messages"); 
	}
    }
    /* Donghong changed 25/10/02 */
    private void checkStatesMIActionPerformed() {
  	java.util.List mssgs = new ArrayList();
	mssgs = curDomain.checkStates();
	if (mssgs.size() < 2) {
	    checkResultsDialog(mssgs,"All state checks passed.");
	} else {
	    CheckResultFrame.showMessages(OclEd.this,mssgs,"States Check Messages");
    }
    }
    /* Donghong changed 25/10/02 */
    private void checkAtomicMIActionPerformed() {
  	java.util.List mssgs = new ArrayList();
	mssgs = curDomain.checkAtomic();
	if (mssgs.size() < 2) {
	    checkResultsDialog(mssgs,"All atomic invarient checks passed.");
	} else {
	    CheckResultFrame.showMessages(OclEd.this,mssgs,"Atomic invarients Check Messages");
    }
    }
    /* Donghong changed 25/10/02 */
    private void checkOpsMIActionPerformed() {
  	java.util.List mssgs = new ArrayList();
	mssgs = curDomain.checkOperators();
	if (mssgs.size() < 2) {
		checkResultsDialog(mssgs,"All operator checks passed.");  
	} else {
        CheckResultFrame.showMessages(OclEd.this,mssgs,"Operators Check Messages");
    }
    }
    /* Donghong changed 25/10/02 */
    private void checkMDMIActionPerformed() {
  	java.util.List mssgs = new ArrayList();
  	mssgs = curDomain.checkMethods();
    if (mssgs.size() < 2) {
	    checkResultsDialog(mssgs,"All method checks passed.");  
	} else {
        CheckResultFrame.showMessages(OclEd.this,mssgs,"Methods Check Messages");
    }
    }
    /* Donghong changed 25/10/02 */
    private void checkTasksMIActionPerformed() {
  	java.util.List mssgs = new ArrayList();
  	mssgs = curDomain.checkTasks();
	if (mssgs.size() < 2) {
	    checkResultsDialog(mssgs,"All task checks passed.");  
	} else {
        CheckResultFrame.showMessages(OclEd.this,mssgs,"Tasks Check Messages");
    }
    }
    /* Donghong changed 25/10/02 */
    private void checkAllMIActionPerformed() {
	java.util.List mssgs = new ArrayList();
	java.util.List temp = new ArrayList();
	boolean carryOn = true;

	mssgs.add("Doing Sorts and Objects check.");
	try {
	    temp =  curDomain.checkSortTree();
	} catch(jplan.general.OCLException e) {
	    mssgs.add("Sorts not defined for the domain.");
	    carryOn = false;
	}
	if (carryOn) {
	    mssgs.addAll(temp);
	    temp.clear();
	    mssgs.add("Doing predicate checks.");
	    temp = curDomain.checkPredicates();
	    mssgs.addAll(temp);
	    temp.clear();
	    mssgs.add("Doing state definition checks.");
	    temp = curDomain.checkStates();
	    mssgs.addAll(temp);
	    temp.clear();
	    mssgs.add("Doing atomic invariant checks.");
	    temp = curDomain.checkAtomic();
	    mssgs.addAll(temp);
	    temp.clear();
	    mssgs.add("Doing operator checks.");
	    temp = curDomain.checkOps();
	    mssgs.addAll(temp);
	    temp.clear();
	    mssgs.add("Doing task checks.");
	    temp = curDomain.checkTasks();
	    mssgs.addAll(temp);
	    temp.clear();
	} 
		/*Donghong changed 28/10/02 */
	if (mssgs.size() < 9) {
		checkResultsDialog(mssgs,"All checks passed.");
	} else {
	   CheckResultFrame.showMessages(OclEd.this,mssgs,"Domain Check Messages");/*Donghong changed 25/10/02 */
    }
    }
    // END Ron 1/6/01
    private void algsMIActionPerformed () {    /* Weihong changed/added on 06/09/2001 */
	PlannerDia pd = new PlannerDia(OclEd.this);
	pd.setVisible(true);
    }
    
    private void showSketchWindow(){
    	if (curDomain == null) {
    		JOptionPane.showMessageDialog(OclEd.this,
    					  "No domain loaded.",
    					  "GIPO Error",
    					  JOptionPane.ERROR_MESSAGE,
    					  null);	 
    		return;
    	}
    	java.util.List mssgs = curDomain.checkOps();
    	mssgs.addAll(curDomain.checkProcesses());
    	mssgs.addAll(curDomain.checkEvents());
    	if (mssgs.size() != 0) {
//    		mssgs.add("WARNING these results may result in the stepper failing.");
    		checkResultsDialog(mssgs,
    				   "All operator/Processes checks passed."); 
    	} 

    	SketchWindow sketch = 
    		new SketchWindow("Plan Sketch Window", this.curDomain, this);
    	desktop.add(sketch);
    	deskManager.activateFrame(sketch);

    	try {
    		sketch.setMaximum(true);
    	} catch (java.beans.PropertyVetoException e) {
    	}
    	sketch.initTaskList();  
        }

    /* WZ 22/05/2001 */
    private void showStepperWindow(){
	//Ron 30/8/01 Added precondition checks
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	java.util.List mssgs = curDomain.checkOps();
	if (mssgs.size() != 0) {
	    mssgs.add("WARNING these results may result in the stepper failing.");
	    checkResultsDialog(mssgs,
			       "All operator checks passed."); 
	} 
	//Ron end 30/8/01

	StepperWindow steppers = 
	    new StepperWindow("Stepper Window", this.curDomain, this);
	desktop.add(steppers);
	deskManager.activateFrame(steppers);
	// Ron 4/10/01
	try {
	    steppers.setMaximum(true);
	} catch (java.beans.PropertyVetoException e) {
	}
	steppers.initTaskList();    /* WZ 10/07/2001 */
    }

    /* WZ 10/5/02 */
    private void showHierarchicalStepperWindow(){
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	java.util.List mssgs = curDomain.checkOps();
	if (mssgs.size() != 0) {
// 	    mssgs.add("WARNING these results may result in the stepper failing.");
	    checkResultsDialog(mssgs,
			       "All operator checks passed."); 
	} 

	HStepperWindow steppers = 
	    new HStepperWindow("Hierarchical Stepper Window", this.curDomain, this);
	desktop.add(steppers);
	deskManager.activateFrame(steppers);

	try {
	    steppers.setMaximum(true);
	} catch (java.beans.PropertyVetoException e) {
	}
	steppers.initTaskList();  
    }

    /* WZ 05/07/2001 */
    private void showAnimatorWindow(){
	//Ron 30/8/01 Added precondition checks
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	if (plannerConfig.dirty) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No planner has been run on this domain." +
					  " \nRun planner first.",
					  "GIPO Warning",
					  JOptionPane.INFORMATION_MESSAGE,
					  null);	 
	    return;
	}
	java.util.List mssgs = curDomain.checkOps();
	if (mssgs.size() != 0) {
	    mssgs.add("WARNING these results may result in the planner failing.");
	    checkResultsDialog(mssgs,
			       "All operator checks passed."); 
	} 
	//Ron end 30/8/01

	AnimatorWindow animators = 
	    new AnimatorWindow("Animator Window", this.curDomain, this);
	desktop.add(animators);
	deskManager.activateFrame(animators);
	// Ron 4/10/01
	try {
	    animators.setMaximum(true);
	} catch (java.beans.PropertyVetoException e) {
	}
    }

    /* WZ 22/7/02 */
    private void showHierarchicalAnimatorWindow(){
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	if (plannerConfig.dirty) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No planner has been run on this domain." +
					  " \nRun planner first.",
					  "GIPO Warning",
					  JOptionPane.INFORMATION_MESSAGE,
					  null);	 
	    return;
	}
	java.util.List mssgs = curDomain.checkOps();
	if (mssgs.size() != 0) {
	    mssgs.add("WARNING these results may result in the planner failing.");
	    checkResultsDialog(mssgs,
			       "All operator checks passed."); 
	} 
	//Ron end 30/8/01

	HAnimatorWindow animators = 
	    new HAnimatorWindow("Hierarchical Animator Window", this.curDomain, this);
	desktop.add(animators);
	deskManager.activateFrame(animators);
	// Ron 4/10/01
	try {
	    animators.setMaximum(true);
	} catch (java.beans.PropertyVetoException e) {
	}
    }

    /* Ron added 2/11/01 */
    private void showOpMakerMain(){
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	if (curDomain.classDefs.size() == 0) {
	    JOptionPane.showMessageDialog(OclEd.this,
				    "No Object Class Definitions Present." +
				    "\n You must define the sorts - objects and states first.",
				    "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	// Perhaps should check earlier stages as well
	java.util.List mssgs = curDomain.checkStates();
	if (mssgs.size() != 0) {
	    mssgs.add("WARNING these results may result in the planner failing.");
	    checkResultsDialog(mssgs,
			       "All state checks passed."); 
	} 

	OpMakerMain opmaker = 
	    new OpMakerMain(this.curDomain, this);
	desktop.add(opmaker);
// 	deskManager.activateFrame(opmaker);
// 	try {
// 	    opmaker.setMaximum(true);
// 	} catch (java.beans.PropertyVetoException e) {
// 	}
    }

    private void exitMIActionPerformed (java.awt.event.ActionEvent evt) {
	exitForm();
    }

    /** Exit the Application */
    /* Weihong 25/2/02 added for explicitly exit */
    private void exitForm() {  
	/* Weihong changed/added on 12/10/2001 */
 	JInternalFrame curFrames = new JInternalFrame();
	for (int i = 0; i< desktop.getComponentCount(); i++){
	    curFrames = desktop.getAllFrames()[i];
	    if (curFrames.isVisible()){
		int k = JOptionPane.showConfirmDialog(this, "To prevent data from missing, \nit is recommended to close all internal windows before exit.\n\n Do you like to exit explicitly?",  "GIPO Message", JOptionPane.YES_NO_OPTION);
		if (k == JOptionPane.YES_OPTION)
		    System.exit (0);
		else
		    return;
	    }
	}
// 	Utility.debugPrintln("flagDirty:  "+flagDirty);
	if (flagDirty){
	    int k = JOptionPane.showConfirmDialog(this, "Editing contents have not been saved in the domain. \nExit anyway?", "", JOptionPane.YES_NO_OPTION);
	    if (k == JOptionPane.NO_OPTION) 
		return;
	}
	
	System.exit (0);
	/* end Weihong changed/added on 12/10/2001 */
    }

    /**
     * get The file object for the current domain
     * return file
     */
    public File getOclFile() {
	return osDiags.getOclFile();
    }

    /**
     * allow reference to the PrologTools Object
     * @return reference to pTools
     */
    PToolsControl getPrologTools() {
	return pTools;
    }

    /* WZ 24/10/2001 */
    /**
     * to mark menu item of this actived/deactived frame.
     * @param newWindow the internal frame
     * @param isActive true - actived; false - deactived
     * 
     */
    public void setActiveFrame(JInternalFrame newWindow, boolean isActive){
	String key = newWindow.getTitle();
	((JCheckBoxMenuItem)windowList.get(key)).setState(isActive);
    }

    /* WZ 24/10/2001 */
    /**
     * to remove the menu item representing a frame which has been disposed.
     * @param newWindow the frame which has been disposed
     * 
     */
    public void removeOnMenubar(JInternalFrame newWindow){
	String key = newWindow.getTitle();
	JCheckBoxMenuItem tmpMenuItem = (JCheckBoxMenuItem)windowList.get(key);
	windowsMenu.remove(tmpMenuItem);
	menuButtonGroup.remove(tmpMenuItem);

	//check if no windows opened then remove the seperator
	if (windowList.size() ==0)
	    windowsMenu.remove(4); //the 4th one is the separator
    }

    /* WZ 23/10/2001 */
    /**
     * to register as a list item on the "windows" menubar
     * @param key the title of the internal frame
     * @return the menu item which represents the internal frame with key as its title.
     */
    public JCheckBoxMenuItem registerOnMenubar(String key){
	//if this is the first internal frame, then add a separator first on the menu
	if (windowList.size() ==0){
	    windowsMenu.addSeparator();
	    menuButtonGroup = new ButtonGroup();
	}

	JCheckBoxMenuItem tmpMenuItem = new JCheckBoxMenuItem(key);
	windowsMenu.add(tmpMenuItem);
	tmpMenuItem.setState(true);
	menuButtonGroup.add(tmpMenuItem);

	//register it with the windowlist
	windowList.put(key, tmpMenuItem);

	return tmpMenuItem;
    }

    /* WZ 18/07/2001 */
    /**
     * to update other windows when a commit to a change happens
     * @param windowTitle - the title of the internal frame
     * 
     */
    public void updateWindow(String windowTitle){
	//to mark that there are some contents have not been saved in an oclfile.
	flagDirty = true;  /* WZ 12/10/2001 */

	int windowPriority = getWindowsPriority(windowTitle);
 	JInternalFrame[] curFrames = new JInternalFrame[15];
	String theTitle = null;
	for (int i = 0; i< desktop.getComponentCount(); i++){
	    curFrames[i] = desktop.getAllFrames()[i];
	    theTitle = curFrames[i].getTitle();
	    
	    if (getWindowsPriority(theTitle) > windowPriority){
		//draw a glass pane to the window
		curFrames[i].getGlassPane().setVisible(true);
		curFrames[i].getGlassPane().addMouseListener(new MouseAdapter(){
		    public void mousePressed(MouseEvent e){
			JOptionPane.showMessageDialog(null,
		   "Please close this window as the domain information has been updated. ",
			   "GIPO Message", JOptionPane.INFORMATION_MESSAGE,  null);
		    }
		});
	    }
	}
    }


    /* WZ 18/07/2001 */
    /**
     * getWindowsPriority
     * @param windowTitle the title of the jinternalframe 
     */
    private int getWindowsPriority(String windowTitle){
	int priority = 0;
	if (windowTitle.startsWith("Sort")){
	    priority = 10;
	}
	else if (windowTitle.startsWith("Predicate")){
 	    priority = 20;
	}
	else if (windowTitle.startsWith("State View")){
	    priority = 30;
	}
	else if (windowTitle.startsWith("Transition")){
	    priority = 40;
	}
	else if (windowTitle.startsWith("Op Maker")){
	    priority = 40;
	}
	else if (windowTitle.startsWith("Compound Transition")){/* WZ 6/11/2001 */
	    priority = 45;
	}
	else if (windowTitle.startsWith("Atomic")){
	    priority = 50;
	}
	else if (windowTitle.startsWith("Negative")){
	    priority = 60;
	}
	else if (windowTitle.startsWith("Implied")){
	    priority = 60;
	}
	else if (windowTitle.startsWith("RandGenerateTask")){    /* WZ 5/10/2001 */
	    priority = 65;
	}
	else if (windowTitle.startsWith("Task")){
	    priority = 70;
	}
	else if (windowTitle.startsWith("Stepper")){
	    priority = 80;
	}
	else if (windowTitle.startsWith("Hierarchical Stepper Window")){/*WZ 10/5/02*/
	    priority = 80;
	}
	else if (windowTitle.startsWith("Animator")){
	    priority = 80;
	}
	else if (windowTitle.startsWith("OCL Text View")){
	    priority = 80;
	}
	return priority;
    }

    /**
     * Arrange any icons on the passed desktop, return a desktop height.
     * @param _deskTop The Desktop to have its icons arranged.
     * @return The height of the desktop visible area, atop the icons.
     * Author: nskogler - Java Developers' conection
     */
    private final int arrangeIcons(JDesktopPane _deskTop)
    {
	int _iconCnt = 0;
	JInternalFrame _allFrames[] = _deskTop.getAllFrames();
	for (int _x = 0; _x < _allFrames.length; _x++)
	    if ((_allFrames[_x].isVisible()) && (_allFrames[_x].isIcon()))
                _iconCnt++;
	int _height = _deskTop.getBounds().height;
	int _yPos = _height;
	if (_iconCnt != 0)
	    {
                int _width = _deskTop.getBounds().width;
                int _xPos = 0;
                for (int _x = 0; _x < _allFrames.length; _x++)
		    {
			JInternalFrame _frame = _allFrames[_x];
			if ((_frame.isVisible()) && (_frame.isIcon()))
			    {
				Dimension _dim = _frame.getDesktopIcon().getSize();
				int _iWidth = _dim.width; 
				int _iHeight = _dim.height; 
				if (_yPos == _height)
				    _yPos = _height - _iHeight;
				if ((_xPos + _iWidth > _width) && (_xPos != 0))
				    {
					_xPos = 0;
					_yPos -= _iHeight;
				    }
				_frame.getDesktopIcon().setLocation(_xPos,_yPos);
				_xPos += _iWidth;
			    } // End if
		    } // End for
	    } // End if
	return(_yPos);
    } // End method
    
    /**
     * Tile the passed desktop.
     * @param _deskTop The Desktop to be tiled.
     * @param _horizontal A positive indication of horizontal-ness.
     * Original Author: nskogler - Java Developers' conection
     * Modified by Weihong Zhao
     */
    private final void tile(JDesktopPane _deskTop, boolean _horizontal){
	int _resizableCnt = 0;
	JInternalFrame _allFrames[] = _deskTop.getAllFrames();
	for (int _x = 0; _x < _allFrames.length; _x++) {
	    JInternalFrame _frame = _allFrames[_x];
	    if ((_frame.isVisible()) && (!_frame.isIcon())) {
		if (!_frame.isResizable())
		    try{
			_frame.setMaximum(false);
		    }catch (java.beans.PropertyVetoException _e) {
				// OK, to take no action here
		    }
		if (_frame.isResizable())
		    _resizableCnt++;
	    }
	} // End for
	int _width = _deskTop.getBounds().width;
	int _height = arrangeIcons(_deskTop);
	if (_resizableCnt != 0){
	    if (_horizontal){
		int _fHeight = _height / _resizableCnt;
		int _yPos = 0;
		for (int _x = 0; _x < _allFrames.length; _x++){
		    JInternalFrame _frame = _allFrames[_x];
		    if ((_frame.isVisible()) && (_frame.isResizable()) && (!_frame.isIcon())){ 
			_frame.setSize(_width,_fHeight);
			_frame.setLocation(0,_yPos);
			_yPos += _fHeight;
		    } 
		} // End for
	    }
	    else {
		int _fWidth = _width / _resizableCnt;
		int _xPos = 0;
		for (int _x = 0; _x < _allFrames.length; _x++){
		    JInternalFrame _frame = _allFrames[_x];
		    if ((_frame.isVisible()) && (_frame.isResizable()) && (!_frame.isIcon())){ 
			_frame.setSize(_fWidth, _height);
			_frame.setLocation(_xPos, 0);
			_xPos += _fWidth;
		    } 
		} // End for
	    }
	}
    }

    /** WZ 22/10/2001 
     * Cascade the passed desktop.
     * @param _deskTop The Desktop to be cascaded.
     */
    private final void casCade(JDesktopPane _deskTop){
	int _resizableCnt = 0;
	JInternalFrame _allFrames[] = _deskTop.getAllFrames();
	int w = 0, h = 0;
	for (int _x = 0; _x < _allFrames.length; _x++) {
	    JInternalFrame _frame = _allFrames[_x];
	    if ((_frame.isVisible()) && (!_frame.isIcon())) {
		try{
		    _frame.setMaximum(false);
		    _frame.setSelected(true);
		}catch (java.beans.PropertyVetoException _e) {
		    
		}
		_frame.setSize(getWidth()*2/3, getHeight()*2/3);
		_frame.setLocation(w, h);
		w += 20;
		h += 20;
	    }
	} // End for
    }


    
    /**
     * historyMIactionPerformed
     * call up the pattern manager
     */
    private void historyMIactionPerformed() {
	if (curDomain == null) {
	    JOptionPane.showMessageDialog(OclEd.this,
					  "No domain loaded.",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE,
					  null);	 
	    return;
	}
	HistoryWindow patMan = new HistoryWindow("Life Histories",curDomain,OclEd.this);
	desktop.add(patMan);
	deskManager.activateFrame(patMan);
	patMan.show();
    }

    /**
     * to start the programme.
     * @param args the command line arguments
     */
    public static void main (String args[]) {
	jplan.images.FlashScreen flsrn = new jplan.images.FlashScreen();
	new OclEd();
	flsrn.dispose();
    }

}

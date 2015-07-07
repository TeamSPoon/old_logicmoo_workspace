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
 * InstStepper.java
 *
 *
 * Created: Mon May 27 10:51:40 2002
 *
 * @author W Zhao
 * @version
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
import jplan.general.FilterStatePane;
import jplan.general.FilterStaticsPane;

/**
 * to show the methods and operator for instantiation with mouse clicking.
 */
public class InstStepper extends javax.swing.JDialog {
    
    // Variables declaration
    private HStepperCanvas parent;
    private javax.swing.JToolBar northToolBar;
    private javax.swing.JButton jbn_OK;
    private javax.swing.JButton jbn_Cancel; 
    private javax.swing.JTextField briefArea = new javax.swing.JTextField("");
    private javax.swing.JTextArea detailedArea =  new javax.swing.JTextArea("");
    private javax.swing.JButton downArrow;
    private javax.swing.JButton upArrow;
    private FilterStatePane flwCurState; //This displays the current state
    private FilterStaticsPane flStatics; // This displays the atomic invars
    private boolean viewState = true; // Boolean to control toggle between 
    private JRadioButton jradAtomic;
    private JRadioButton jradState;
    private JPanel mdNamePanel;
    
    // viewing current state and atomic invariants
    private oclDomain curDom;
    private oclMethod mdRef;

    private oclPredicate briefAreaPred;
    private int ArgNo = -1; //the index of the sort/varible in a predicate - briefAreaPred
    private UnderlineHighlighter highlighter = null;
    private BasicComboPopup popupMenu; 
    private String objname = null;
    private List objectList = new ArrayList(); //the whole object list from the curDomain
    private OPredicate.pArg selectedPArg;
    private String curPath;

    private JList jlstMD;
    private JList jlstOP;
    private oclMethod curMethod;

    private oclOperator curOP;
    private JTabbedPane tabPane;

    private String subGoalState;



  /** 
   * Creates an instance of the dialog window
   * @param curDom oclDomain
   * @param parent parent frame
   * @param curPath the path to get images from the image store
   */
    public InstStepper(oclDomain curDom, HStepperCanvas parent, String curPath, String subGoalState) {
	super(parent.getTheParent().getTheParent(), "Stepper Instantiation Window"); 
	setModal(true);
	this.parent = parent;
	this.curDom = curDom;
	this.subGoalState = subGoalState;
	this.objectList = curDom.objects; //lists of oclObject
	this.curPath = curPath;
	initComponents ();
	pack ();
	setSize(500, 650);
    }
    
    /** 
     * Initialisation
     * 
     */  
    private void initComponents () {
	setBackground (new java.awt.Color (114, 159, 255));
	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );
	getContentPane ().setLayout (new java.awt.BorderLayout ());

	//assemble the checkbox list
	//list all matching methods with same reference
	mdNamePanel = new JPanel();
	mdNamePanel.setLayout(new BorderLayout());

	/* WZ 22/5/02 */
	JPanel titlePanel = new JPanel(); 
	titlePanel.setLayout(new GridLayout(0, 1));
	JLabel label1 = new JLabel("To finally achieve the sub state:");
	label1.setFont(new Font("Aril", Font.BOLD, 14));
	label1.setHorizontalTextPosition(SwingConstants.CENTER);
	JLabel label2 = new JLabel(subGoalState);
	label2.setFont(new Font("Currier", Font.ITALIC, 12));
	label2.setHorizontalTextPosition(SwingConstants.CENTER); 
	JLabel label3 = new JLabel("please select operator/method to run.");
	label3.setFont(new Font("Aril", Font.BOLD, 14));
	label3.setHorizontalTextPosition(SwingConstants.CENTER); 
	titlePanel.add(label1);
	titlePanel.add(label2);
	titlePanel.add(label3);
	mdNamePanel.add(titlePanel, "North");

	tabPane = new JTabbedPane();
	tabPane.setPreferredSize(new Dimension(200, 150));

	jlstMD = new JList();
	DefaultListModel lmMD = new DefaultListModel();
	jlstMD.setToolTipText("select to instantiate; right click to view description");
	jlstMD.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	
	if (curDom.methods.size()>0){/* WZ 16/8/02 */
	    ListIterator li = curDom.methods.listIterator();
	    while (li.hasNext()) {
		oclMethod tmpMD = (oclMethod)li.next();
		try {
		    lmMD.addElement(tmpMD.clone());
		}catch (CloneNotSupportedException e) {
		    Utility.debugPrintln(e);
		}
	    }
	    jlstMD.setModel(lmMD);
	    jlstMD.setSelectedIndex(-1);
	    jlstMD.addMouseListener(new MouseAdapter() {
		public void mouseClicked(MouseEvent me) {
		    try {
			curMethod = (oclMethod)((oclMethod)jlstMD.getSelectedValue()).clone();
			if (me.getModifiers() == MouseEvent.BUTTON1_MASK) {
			    populateMethod();
			}
			
			if(me.getModifiers() == MouseEvent.BUTTON3_MASK) {
			    String dec = curMethod.getDescription();
			    if (dec.trim().equals(new String()))
				dec = "No decription for this method.";
			    JOptionPane.showMessageDialog(InstStepper.this,dec,
							  "METHOD - "+curMethod.getName().getName(),  
							  JOptionPane.PLAIN_MESSAGE);
			}
		    }catch (CloneNotSupportedException e){Utility.debugPrintln(e);}
		}
	    });
	}
	jlstMD.setBorder (new javax.swing.border.BevelBorder(0));
	JScrollPane jsOM = new JScrollPane(jlstMD);
	tabPane.add(" Compound Operator ", jsOM);


	jlstOP = new JList();
	DefaultListModel lmOP = new DefaultListModel();
	jlstOP.setToolTipText("select to instantiate; right click to view description");
	jlstOP.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

	if (curDom.operators.size()>0){/* WZ 16/8/02 */
	    ListIterator li = curDom.operators.listIterator();
	    while (li.hasNext()) {
		oclOperator tmpOP = (oclOperator)li.next();
		try {
		    lmOP.addElement(tmpOP.clone());
		}catch (CloneNotSupportedException e) {
		    Utility.debugPrintln(e);
		}
	    }
	    jlstOP.setModel(lmOP);
	    jlstOP.setSelectedIndex(-1);
	    jlstOP.addMouseListener(new MouseAdapter() {
		public void mouseClicked(MouseEvent me) {
		    if (me.getClickCount() == 1 && me.getModifiers() == MouseEvent.BUTTON1_MASK){
			try{
			    curOP = (oclOperator)((oclOperator)jlstOP.getSelectedValue()).clone();
			    if (me.getModifiers() == MouseEvent.BUTTON1_MASK) {
				populateOperator();
			    }
			}catch (CloneNotSupportedException e){Utility.debugPrintln(e);}
		    }
		}
	    });
	}
	jlstOP.setBorder (new javax.swing.border.BevelBorder(0));
	JScrollPane jsOP = new JScrollPane(jlstOP);
	tabPane.add(" Primitive Operator ", jsOP);
	mdNamePanel.add(tabPane, "Center");

	/* WZ 28/5/02 */
	//assemble current state panel
	// Ron 31/10/02 changed to a filter state pane
	/*
 	 JList jlstInitState = new JList();
 	 DefaultListModel stateModel = new DefaultListModel();
	 ListIterator liState = parent.getCurState().listIterator();
	 while (liState.hasNext()) {
	     oclSS ss = (oclSS)liState.next();
	     stateModel.addElement(ss);
	 }
	 jlstInitState.setModel(stateModel);
	 jlstInitState.setEnabled(false);
	 JScrollPane jscrollInitState = new JScrollPane(jlstInitState);
	 jscrollInitState.setBorder(BorderFactory.createTitledBorder("Current State"));
	 jlstInitState.setCellRenderer(new jplan.edexpt.PredListCellRenderer());
	 */
	 flwCurState = new FilterStatePane(curDom,"Current State");
	 flwCurState.showStateProperty(parent.getCurState(), 1);
	 mdNamePanel.add(flwCurState, "East");
	flStatics = new FilterStaticsPane(curDom,"Atomic Invariants");
	//assemble instantiation panel
	briefArea.setEditable(false);
	briefArea.setFont(new java.awt.Font("Arial", java.awt.Font.BOLD, 16));
	populateMethod();

	briefArea.setScrollOffset(0);
	highlighter = new jplan.general.UnderlineHighlighter(null);
	briefArea.setHighlighter(highlighter);
	mdNamePanel.add(briefArea, "South");
	getContentPane ().add (mdNamePanel, "North");

	briefArea.addMouseListener(new MouseAdapter(){
	    public void mouseClicked(MouseEvent me){
		if(me.getModifiers() == MouseEvent.BUTTON3_MASK) {
		    java.awt.Point viewPoint = me.getPoint();
		    int clickPos = briefArea.viewToModel(viewPoint);
		    if (briefAreaPred != null){
		    try {
			String varString = briefAreaPred.elementAt(clickPos);
			ArgNo = briefAreaPred.elementNoAt(clickPos);
			selectedPArg = (OPredicate.pArg)briefAreaPred.pArgAt(clickPos).clone();
			//highlighting
			removeHighlights();
			int start = briefAreaPred.startElementAt(clickPos);
			highlighter.addHighlight(start,  start+varString.length(), new UnderlineHighlighter.UnderlineHighlightPainter (Color.red));
			briefArea.updateUI();

			String sortBranch = briefAreaPred.getNthElementSort(ArgNo); //wait for it's available
			Utility.debugPrintln("ArgNo::::"+ArgNo);
			Utility.debugPrintln("sortBranch::::"+sortBranch);
			initPopupMenu(sortBranch);
			if (popupMenu.getComponentCount() > 0) {
			    popupMenu.show(briefArea, me.getX(), briefArea.getHeight());
			} else {
			    Utility.debugPrintln("No object itmes in the popup menu.");
			}
			
		    } catch (Exception e){Utility.debugPrintln(e);}
		    }
		}
	    }
	});
	
	detailedArea.setLineWrap(true);
	detailedArea.setEditable(false);
// 	detailedArea.setSize(500, 300);
	detailedArea.setPreferredSize(new Dimension(200, 150));/* WZ 21/8/02 */
	detailedArea.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 11));
	JScrollPane jsDetail = new JScrollPane(detailedArea);
	getContentPane ().add (jsDetail, "Center");
// 	detailedArea.setVisible(false);
	detailedArea.setBorder (new javax.swing.border.BevelBorder(0));
	
	northToolBar = new javax.swing.JToolBar ();
	northToolBar.setLayout (new java.awt.FlowLayout ());
	northToolBar.setFloatable(false);
	
	jbn_OK = new javax.swing.JButton ();
	jbn_OK.setText ("   Run   ");
	jbn_OK.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_OKActionPerformed ();
	    }
	}
				  );
	northToolBar.add (jbn_OK);
	
	jbn_Cancel = new javax.swing.JButton ();
	jbn_Cancel.setText ("  Cancel  ");
	jbn_Cancel.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_CancelActionPerformed ();
	    }
	}
				      );
	northToolBar.add (jbn_Cancel);

	northToolBar.addSeparator();
	// Ron 31/10/02 add ability to tohggle to afview and filter atomic invarients
	jradAtomic = new JRadioButton("View Atomic Invariants",false);
	jradState = new JRadioButton("View Current State",true);
	ButtonGroup bgrView = new ButtonGroup();
	bgrView.add(jradState);
	bgrView.add(jradAtomic);
	jradState.addActionListener(new ActionListener() {
		public void actionPerformed(java.awt.event.ActionEvent evt){
			Utility.debugPrintln("Fired jradState");
			if (!viewState) {
				mdNamePanel.remove(flStatics);
				mdNamePanel.add(flwCurState,"East");
				viewState = true;
				mdNamePanel.updateUI();
			} 
			
		}
	});
	jradAtomic.addActionListener(new ActionListener() {
		public void actionPerformed(java.awt.event.ActionEvent evt){
			Utility.debugPrintln("Fired jradAtomic");
			if (viewState) {
				mdNamePanel.remove(flwCurState);
				mdNamePanel.add(flStatics,"East");
				mdNamePanel.updateUI();
			}
			viewState = false;
		}
	});
	northToolBar.add (jradState);
	northToolBar.add (jradAtomic);
	
// 	ImageIcon ii = ImageLoader.getImageIcon(curPath, "Down16.gif");
// 	downArrow = new javax.swing.JButton ("", ii);
// 	downArrow.addActionListener (new java.awt.event.ActionListener () {
// 	    public void actionPerformed (java.awt.event.ActionEvent evt) {
// 		downArrow.setVisible(false);
// 		upArrow.setVisible(true);
// 		detailedArea.setVisible(true);
// 		pack();
// 	    }
// 	}
// 				     );
// 	northToolBar.add (downArrow);

// 	ii = ImageLoader.getImageIcon(curPath, "Up16.gif");
// 	upArrow = new javax.swing.JButton ("", ii);
// 	upArrow.addActionListener (new java.awt.event.ActionListener () {
// 	    public void actionPerformed (java.awt.event.ActionEvent evt) {
// 		downArrow.setVisible(true);
// 		upArrow.setVisible(false);
// 		detailedArea.setVisible(false);
// 		pack();
// 	    }
// 	}
// 				   );
// 	northToolBar.add (upArrow);
// 	upArrow.setVisible(false);
	
	getContentPane ().add (northToolBar, "South");
	
    }
    
    private void populateMethod(){
	if (curMethod != null){
	    briefAreaPred = (oclPredicate)curDom.createMethodSignature(curMethod);
	    briefArea.setText(briefAreaPred.toString());
	    showMethodDetail();
// 	    pack();
	}
    }

    private void populateOperator(){
	if (curOP != null){
	    briefAreaPred = (oclPredicate)curDom.createOperatorSignature(curOP);
	    briefArea.setText(briefAreaPred.toString());
	    showOperatorDetail();
// 	    pack();
	}
    }

    /** 
     * display method details (OCLh language format) to a JTextArea
     * 
     */
    private void showMethodDetail(){
	if (curMethod != null){
	    StringWriter opDetail = new StringWriter();
	    curMethod.oclPrintComponent(new PrintWriter (opDetail) ,0,false);
	    detailedArea.setText(new String(opDetail.getBuffer()));
	}
    }
    
    /** 
     * display operator details (OCLh language format) to a JTextArea
     * 
     */
    private void showOperatorDetail(){
	StringWriter opDetail = new StringWriter();
	curOP.oclPrintComponent(new PrintWriter (opDetail) ,0,false);
	detailedArea.setText(new String(opDetail.getBuffer()));
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
	    if (h.getPainter() instanceof 
		UnderlineHighlighter.UnderlineHighlightPainter) {
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
		objname = (String)liObj.next();
		items.addElement(objname);
	    }
	}
	//build the PopupMenu
	JComboBox combo = new JComboBox(items);
	combo.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		try {
		    objname = (String)(popupMenu.getList().getSelectedValue());
		    //replace the old varibles with this objname
		    briefAreaPred.replaceVariableNo(ArgNo, objname);
		    briefArea.setText(briefAreaPred.toString());
		    if (tabPane.getSelectedIndex() == 0){
			curMethod.replaceVariableName(selectedPArg, objname);
			showMethodDetail();
		    }
		    if (tabPane.getSelectedIndex() == 1){
			curOP.replaceVariableName(selectedPArg, objname);
			showOperatorDetail();
		    }
		    popupMenu.hide();
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
    private void jbn_OKActionPerformed () {
	closeDialog();
    }
    
    /** 
     * when cancel button is pressed, close this dialog window
     * 
     */
    private void jbn_CancelActionPerformed () {
	if (tabPane.getSelectedIndex() == 0)
	    curMethod = null;
	if (tabPane.getSelectedIndex() == 1)
	    curOP = null;
	closeDialog();
    }
    
    /**
     * factory method to create and display box
     * @param curDom
     * @param parent
     * @param curPath
     * @param state
     */
    public static Object showInstantiation(oclDomain curDom, HStepperCanvas parent, String curPath, String state){
	InstStepper pw = new InstStepper(curDom, parent, curPath, state);
	pw.setLocation((int) (0.5 * parent.getWidth()),(int) (0.5 * parent.getHeight()));
	pw.show();
	return pw.getInstObject();
    }

    /** 
     * return the instantiated method
     * @return the instantiated method
     */
    public Object getInstObject(){
	if (tabPane.getSelectedIndex() == 0)
	    return curMethod;
	if (tabPane.getSelectedIndex() == 1)
	    return curOP;
	return null;
    }

    /** 
     * close this dialog window
     * 
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }
        
} // InstStepper

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

package jplan.edexpt;

/**
 * SortView
 * This is the OCL expert Sort editor
 * Part of the edexpt (Editor for Experts) Views
 * @author Ron Simpson
 * @version 0
 */

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.awt.event.*;
import java.awt.*;

import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.general.*;
import jplan.images.ImageLoader; 	/* Weihong changed on 5/9/2001 */

public class SortView extends GipoInternalFrame {/* Weihong added on 24/10/2001 */
  private OclEd top;
  private oclDomain curDomain;
  private List sorts;
  // Interface components or their Models
  private SortTree sortTree;
  private JTextField jtxtSortName;
  private JTextField jtxtObjName;
    
  /**
   * Constructor
   * @param curDomain the current active domain
   * @param parent top level Window reference
   */
  public SortView (oclDomain curDomain,OclEd parent) {
//     super("",true,true,true,true);
      super(parent);	/* Weihong added on 24/10/2001 */
    setClosable(false); 	/* Weihong added on 11/10/2001 */
    if (curDomain == null) {
      JOptionPane.showMessageDialog(parent,
				    EdStrings.strNoDomain,
				    EdStrings.strErrorHeading,
				    JOptionPane.ERROR_MESSAGE,
				    null);	    
      return;
    }
    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    List mssgs = curDomain.sortCycleTest();
    while (mssgs.size() > 0) {
	int ans = JOptionPane.showConfirmDialog(parent,
				      "Cannot display tree\n" + (String)mssgs.get(0) +"\nTry to remove cycle?",
				    EdStrings.strErrorHeading,
				    JOptionPane.YES_NO_OPTION,
				    JOptionPane.INFORMATION_MESSAGE);
	if (ans == JOptionPane.YES_OPTION) {
	    String msg = (String)mssgs.get(0);
	    int space = msg.lastIndexOf(' ');
	    String dupSort = msg.substring(space+1);
	    Utility.debugPrintln("Cycle sort:" + dupSort + ":");
	    curDomain.removeSortDefinition(dupSort);
	    curDomain.removeObjectsOfSort(dupSort);
	    mssgs = curDomain.sortCycleTest();
	} else {
	    return;
	}
    }
    setTitle(EdStrings.strSortViewTitle);
    sorts = curDomain.sorts;   //had reference to parent
    top = null;
    this.curDomain = curDomain;
    top = parent;
    initComponents();
    //setSize(400,400);
    //show();
    pack();
    setVisible(true);
    try {

      SortTreeDragSource dragSource = new SortTreeDragSource(sortTree);
      SortTreeDropTarget dropTarget = new SortTreeDropTarget(sortTree);
      sortTree.setEditable(true);
    } catch (Exception e) {
      Utility.debugPrintln("Tree exception " + e.toString());
    }
  }
    
    /**
     * initComponents
     * Sets up the user interface
     */
    private void initComponents() {
	int tWidth;

	addInternalFrameListener(new InternalFrameAdapter() {
		public void internalFrameClosing (InternalFrameEvent evt) {
		    frameClosingEvent(evt);
		}		
	    }
				 );
	JButton cmdDummy = new JButton("XXX");
	int txtFieldMaxHeight = cmdDummy.getMaximumSize().height;
	cmdDummy = null;
	Box boxEd = Box.createHorizontalBox();

	/* end Weihong added on 10/10/2001 */
// 	Box boxFinalButtons = Box.createHorizontalBox();
	JPanel boxFinalButtons = new JPanel();
	boxFinalButtons.setLayout(new FlowLayout());
	/* end Weihong added on 10/10/2001 */

	Box boxSort = Box.createVerticalBox();
 
	//  Now put the components together
	    sortTree = new SortTree(curDomain);
	    
	
	JScrollPane scrollPaneSorts = new JScrollPane(sortTree);
	
	boxSort.add(scrollPaneSorts);

	/* Weihong added on 10/10/2001 */
// 	Box boxEdSort = Box.createHorizontalBox();
	JPanel boxEdSort = new JPanel();
	boxEdSort.setLayout(new FlowLayout());

// 	Box boxEdObj = Box.createHorizontalBox();
	JPanel boxEdObj = new JPanel();
	boxEdObj.setLayout(new FlowLayout());
	/* end Weihong added on 10/10/2001 */

	JLabel jlblSortName = new JLabel("Sort Name");
	jlblSortName.setAlignmentY(0.5f);    /* Weihong added on 4/07/2001 */
	jtxtSortName = new JTextField(14);
	jtxtSortName.setAlignmentY(0.5f);   /* Weihong added on 4/07/2001 */
	jtxtSortName.setDocument(new OCLIdentifierDocument());
	jtxtSortName.setToolTipText(EdStrings.identifierToolTipText);
	tWidth = jtxtSortName.getPreferredSize().width;
	jtxtSortName.setMaximumSize(new Dimension(tWidth,txtFieldMaxHeight));
	jlblSortName.setDisplayedMnemonic(KeyEvent.VK_S);
	jlblSortName.setLabelFor(jtxtSortName);
	/* Weihong added on 4/07/2001 */
// 	ImageIcon ii = new ImageIcon(top.strImageDir + "Add16.gif");
	/* Weihong changed on 4/9/2001 */
	ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Add16.gif");
	JButton cmdAdd = new JButton("Add", ii);
	
	cmdAdd.setAlignmentY(0.5f);
	cmdAdd.setMnemonic(KeyEvent.VK_A);
	cmdAdd.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    addSort();
		}
	    }
				  );
	JLabel jlblObjName = new JLabel("Object Name");
	jlblObjName.setAlignmentY(0.5f);   /* Weihong added on 4/07/2001 */
	jtxtObjName = new JTextField(14);
	jtxtObjName.setAlignmentY(0.5f);   /* Weihong added on 4/07/2001 */
	jtxtObjName.setDocument(new OCLIdentifierDocument());
	jtxtObjName.setToolTipText(EdStrings.identifierToolTipText);
	tWidth = jtxtObjName.getPreferredSize().width;
	jtxtObjName.setMaximumSize(new Dimension(tWidth,txtFieldMaxHeight));;
	jlblObjName.setDisplayedMnemonic(KeyEvent.VK_O);
	jlblObjName.setLabelFor(jtxtObjName);
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Properties16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Properties16.gif");
	JButton cmdAddObj = new JButton("Add Object", ii);
	cmdAddObj.setAlignmentY(0.5f);
	cmdAddObj.setMnemonic(KeyEvent.VK_B);
	cmdAddObj.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    addObject();
		}
	    }
				  );
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Delete16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Delete16.gif");
	JButton cmdDel = new JButton("Delete Selected", ii);
	cmdDel.setAlignmentY(0.5f);
	cmdDel.setMnemonic(KeyEvent.VK_D);
	cmdDel.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    delNode();
		}
	    }
				  );
	boxEdSort.add(jlblSortName);	
	boxEdSort.add(jtxtSortName);
	boxEdSort.add(cmdAdd);
	boxEdObj.add(jlblObjName);
	boxEdObj.add(jtxtObjName);
	boxEdObj.add(cmdAddObj);
	boxEdSort.add(cmdDel);
// 	boxSort.add(Box.createVerticalStrut(5));/* Weihong added on 10/10/2001 */
	boxSort.add(boxEdSort);
// 	boxSort.add(Box.createVerticalStrut(5));/* Weihong added on 10/10/2001 */
	boxSort.add(boxEdObj);

	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "SendMail16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "SendMail16.gif");
	JButton cmdCommit = new JButton ("Commit", ii);
	cmdCommit.setMnemonic(KeyEvent.VK_M);
	cmdCommit.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    commitChanges();
		}
	    }
				  );
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Replace16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Replace16.gif");
	JButton cmdVerify = new JButton ("Verify", ii);
	cmdVerify.setMnemonic(KeyEvent.VK_V);
	cmdVerify.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    java.util.List dups = sortTree.checkTree();
		    if (dups.size() > 0) {
			ListIterator li = dups.listIterator();
			String dupNames = (String)li.next();
			while (li.hasNext()) {
			    dupNames = dupNames + ", " + (String)li.next();
			}
			JOptionPane.showMessageDialog(SortView.this,
				    EdStrings.strDupEntries + dupNames,
				    EdStrings.strErrorHeading,
				    JOptionPane.ERROR_MESSAGE,
				    null);
			
		    } else {
			JOptionPane.showMessageDialog(SortView.this,
				    EdStrings.strVerifyOK,
				    EdStrings.strInfoHeading,
				    JOptionPane.INFORMATION_MESSAGE,
				    null);
		    } 
		}
	    }
				  );
	
	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Remove16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Remove16.gif");
	JButton cmdRestore = new JButton ("Restore", ii);
	cmdRestore.setMnemonic(KeyEvent.VK_R);
	cmdRestore.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    sortTree.restoreTree();		    
		}
	    }
				  );

	/* Weihong added on 4/07/2001 */
// 	ii = new ImageIcon(top.strImageDir + "Stop16.gif");
	/* Weihong changed on 4/9/2001 */
	ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
	JButton cmdClose = new JButton ("Close", ii);
	cmdClose.setMnemonic(KeyEvent.VK_L);
	cmdClose.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    if (sortTree.isDirty()) {
			int res = JOptionPane.showConfirmDialog(SortView.this,
					  EdStrings.strTreeChanged,
					  EdStrings.strWarningHeading,
					  JOptionPane.YES_NO_OPTION);
			if (res == JOptionPane.YES_OPTION) {
			    setVisible (false);
			    dispose ();	    	/* Weihong changed on 18/10/2001 */
			} else {
			    return;
			}
		    } else {
			setVisible (false);
			dispose ();	 	/* Weihong changed on 18/10/2001 */
		    }			    
		}
	    }
				  );
	
	boxFinalButtons.add(cmdCommit);
	boxFinalButtons.add(cmdVerify);
	boxFinalButtons.add(cmdRestore);
	boxFinalButtons.add(cmdClose);
// 	boxSort.add(Box.createVerticalStrut(5));/* end Weihong added on 10/10/2001 */
	boxSort.add(boxFinalButtons);
	
	
	boxEd.add(boxSort);
	
	getContentPane().add(boxEd);
	
    } 

    public void frameClosingEvent(InternalFrameEvent evt) {
	if (sortTree.isDirty()) {
	    int res = JOptionPane.showConfirmDialog(SortView.this,
						    EdStrings.strNoCommit,
						    EdStrings.strWarningHeading,
						    JOptionPane.YES_NO_OPTION);
	    if (res == JOptionPane.YES_OPTION) {
		commitChanges();
		setVisible (false);
		dispose ();	    
	    } else {
		Utility.debugPrintln("Doing close anyway");
	    }
	} else {
	    setVisible (false);
	    dispose ();	 
	}
    }
    /**
     * commitChanges
     * write the sort/object tree into the current domain
     */
    private void commitChanges () {
	java.util.List dups = sortTree.checkTree();
	if (dups.size() > 0) {
	    ListIterator li = dups.listIterator();
	    String dupNames = (String)li.next();
	    while (li.hasNext()) {
		dupNames = dupNames + ", " + (String)li.next();
	    }
	    JOptionPane.showMessageDialog(SortView.this,
		      EdStrings.strDupEntries + dupNames,
		      EdStrings.strErrorHeading,
		      JOptionPane.ERROR_MESSAGE,
		      null);
	    
	} else {
	    sortTree.updateDomain();	

	    /* Weihong added on 18/07/2001 */
	    top.updateWindow(getTitle());	    
	}
    }

    /** 
     * closeDialog
     * Closes the dialog 
     */
    private void closeDialog() {
	if (sortTree.isDirty()) {
	    int res = JOptionPane.showConfirmDialog(SortView.this,
				    EdStrings.strTreeChanged,
				    EdStrings.strWarningHeading,
				    JOptionPane.YES_NO_OPTION);
	    if (res == JOptionPane.YES_OPTION) {
		setVisible (false);
		dispose ();	    
	    } else {
		return;
	    }
	} else {
	    setVisible (false);
	    dispose ();	 
	}		
    }

    /**
     * addSort
     * add a sort name to the sort tree at the selected node
     */
    private void addSort() {
	TreePath[] paths = sortTree.getSelectionPaths();
	if (paths != null && paths.length > 0) {
	    SortNode selNode = null;
	    selNode =
		(SortNode) paths[0].getLastPathComponent();
	    if (SortTree.isObject(selNode)) {
		sortTree.getToolkit().beep();
		JOptionPane.showMessageDialog(top,
					      EdStrings.strSelectSort,
					      EdStrings.strErrorHeading,
					      JOptionPane.ERROR_MESSAGE,
					      null);
		return;
	    } else if (jtxtSortName.getText().length() == 0) {
		JOptionPane.showMessageDialog(top,
			     "No sort name entered.",
			     EdStrings.strErrorHeading,
			     JOptionPane.ERROR_MESSAGE,
					      null);
			    return;
	    } else {
		try {
		    SortNode child;
		    child = (SortNode)selNode.getFirstChild();
		    if (SortTree.isObject(child)) {
			sortTree.getToolkit().beep();
			JOptionPane.showMessageDialog(top,
			     EdStrings.strDontMixSortsObjs,
			     EdStrings.strErrorHeading,
			     JOptionPane.ERROR_MESSAGE,
					      null);
			return;
		    }
		} catch (NoSuchElementException e) {
		    // This is ok sort node acceptable
		}
		sortTree.addNode(selNode,jtxtSortName.getText(),false);
		jtxtSortName.setText("");
	    }  
	} else {
	    JOptionPane.showMessageDialog(top,
				  EdStrings.strSelectNode,
				  EdStrings.strErrorHeading,
				   JOptionPane.ERROR_MESSAGE,
							  null);
	}
	//sortTree.addToRoot(jtxtSortName.getText());
    }

    /**
     * addObject - add an object to the tree
     *             at the selected node
     */
    private void addObject() {
	TreePath[] paths = sortTree.getSelectionPaths();
	if (paths != null && paths.length > 0) {
	    SortNode selNode = null;
	    selNode =
		(SortNode) paths[0].getLastPathComponent();
	    if (SortTree.isObject(selNode)) {
		sortTree.getToolkit().beep();
		JOptionPane.showMessageDialog(top,
			     EdStrings.strSelectSortNode,
			     EdStrings.strErrorHeading,
			     JOptionPane.ERROR_MESSAGE,
					      null);
		return;
	    } else {
		try {
		    SortNode child;
		    child = (SortNode)selNode.getFirstChild();
		    if (!SortTree.isObject(child)) {
			sortTree.getToolkit().beep();
			JOptionPane.showMessageDialog(top,
			     EdStrings.strDontMixSortsObjs,
			     EdStrings.strErrorHeading,
			     JOptionPane.ERROR_MESSAGE,
					      null);
			return;
		    } else {
			if (selNode.isRoot()) {
			    JOptionPane.showMessageDialog(top,
			     "Cannot add objects directly to the root node.",
			     EdStrings.strErrorHeading,
			     JOptionPane.ERROR_MESSAGE,
					      null);
			    return;
			} else if (jtxtObjName.getText().length() == 0) {
			    JOptionPane.showMessageDialog(top,
			     "No object name entered.",
			     EdStrings.strErrorHeading,
			     JOptionPane.ERROR_MESSAGE,
					      null);
			    return;
			} else {
			    sortTree.addNode(selNode
					     ,jtxtObjName.getText(),true);
			    jtxtObjName.setText("");
			}
		    }
		} catch (NoSuchElementException e) {
		    // This is ok sort node acceptable
		    sortTree.addNode(selNode
				     ,jtxtObjName.getText(),true);
		    jtxtObjName.setText("");
		}
	    }  
	} else {
	    JOptionPane.showMessageDialog(top,
		                 "Select a node to add to.",
				  EdStrings.strErrorHeading,
				   JOptionPane.ERROR_MESSAGE,
							  null);
	}
    }

    /**
     * delNode - delete the selected node
     */
    private void delNode() {
	TreePath[] paths = sortTree.getSelectionPaths();
	if (paths != null && paths.length > 0) {
	    SortNode selNode = null;
	    selNode =
		(SortNode) paths[0].getLastPathComponent();
	    try {
		SortNode child;
		child = (SortNode)selNode.getFirstChild();
		int res = JOptionPane.showConfirmDialog(SortView.this,
					  "Delete all sub-nodes.",
					  EdStrings.strErrorHeading,
					  JOptionPane.YES_NO_OPTION);
			if (res == JOptionPane.YES_OPTION) {
			    boolean ok = sortTree.delNode(selNode);
			    if (!ok) {
				JOptionPane.showMessageDialog(top,
		                 "Cannnot delete the Root Node.",
				  EdStrings.strErrorHeading,
				   JOptionPane.ERROR_MESSAGE,
							  null);
				return;
			    }
			} else {
			    return;
			}
		} catch (NoSuchElementException e) {
		    // This is ok just do it    
		    boolean ok = sortTree.delNode(selNode);
		    if (!ok) {
			JOptionPane.showMessageDialog(top,
		                 "Cannnot delete the Root Node.",
				  EdStrings.strErrorHeading,
				   JOptionPane.ERROR_MESSAGE,
							  null);
		    }
		    return;
		}
	} else {
	    JOptionPane.showMessageDialog(top,
		                 "Select a node to delete.",
				  EdStrings.strErrorHeading,
				   JOptionPane.ERROR_MESSAGE,
							  null);
	}
    }
    
}

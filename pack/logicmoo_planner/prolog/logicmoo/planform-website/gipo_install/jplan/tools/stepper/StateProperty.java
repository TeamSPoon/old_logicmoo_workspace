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
 * StateProperty.java
 * @author Weihong Zhao
 * 07/06/2002
 */


import jplan.ocl.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;
import java.util.List;
import jplan.edexpt.PredListCellRenderer;
import javax.swing.table.*; /* WZ 12/6/02 */
import jplan.general.GipoTableCellRenderer;/* WZ 12/6/02 */

/**
 * to show the all state property
 */
public class StateProperty extends javax.swing.JDialog {
    
    // Variables declaration
    private JToolBar northToolBar;
    private JButton jbn_OK;
    
    private List preStates;
    private List postStates;

  /** 
   * Creates an instance of the OperatorInstantiation dialog window
   * @param owner parent frame
   * @param preStates -  a list of states of the current object
   * @param postStates name of the object which property is displaying.
   */
    public StateProperty(JFrame owner, List preStates, List postStates) {
	super(owner);
	setModal(true);
	setTitle("States Window");
	this.preStates = preStates;
	this.postStates = postStates;
	initComponents ();
	pack ();
	setSize(600, 400);
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

	JPanel statePanel = new JPanel();
	statePanel.setLayout(new java.awt.GridLayout(1, 0));

	/* WZ 12/6/02 */
	TableModel tableModel = new AbstractTableModel(){
	    Object [][] rowData = new Object[preStates.size()][3];
	    final String [] columnNames = {"Object", "Pre-States", "Post-States"};
	    public String getColumnName(int col) { 
                return columnNames[col].toString(); 
            }
	    public void setColumnName(int col, String columnName) { 
                columnNames[col] = columnName; 
            }
            public int getRowCount() { return rowData.length; }
            public int getColumnCount() { return columnNames.length; }
            public Object getValueAt(int row, int col) { 
                return rowData[row][col]; 
            }
            public boolean isCellEditable(int row, int col)
                { return false; }
            public void setValueAt(Object value, int row, int col) {
                rowData[row][col] = value;
                fireTableCellUpdated(row, col);
            }
	};

	showStateProperty(tableModel);
	JTable jtable = new JTable(tableModel);
	jtable.setDefaultRenderer(Object.class, new GipoTableCellRenderer());
	JScrollPane jscTable = new JScrollPane(jtable);
	statePanel.add (jscTable);
	/* end 12/6/02 */

	getContentPane ().add (statePanel, "Center");

	northToolBar = new javax.swing.JToolBar ();
	northToolBar.setLayout (new java.awt.FlowLayout ());
	northToolBar.setFloatable(false);

	jbn_OK = new javax.swing.JButton ();
	jbn_OK.setText ("   OK   ");
	jbn_OK.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_OKActionPerformed ();
	    }
	}
				  );
	northToolBar.add (jbn_OK);
	
	getContentPane ().add (northToolBar, "South");
	
    }
    
   /**
     * show states to a JTable.
     * @param tableModel TableModel
     * 
     */
    private void showStateProperty(TableModel tableModel){
	int row = 0;
	ListIterator liPre = preStates.listIterator();
	ListIterator liPost = postStates.listIterator();
	while (liPre.hasNext()) {
	    int col = 0;
	    oclSS ssPre = (oclSS)liPre.next();
	    //1st column
	    tableModel.setValueAt(ssPre.getName(), row, col);
	    //2nd column
	    col++;
	    tableModel.setValueAt(ssPre, row, col);
	    //3rd column
	    if (liPost.hasNext()) {/* WZ 13/6/02 */
		oclSS ssPost = (oclSS)liPost.next();
		col++;
		tableModel.setValueAt(ssPost, row, col);
	    }
	    row++;
	}
    }

   /**
     * show states to a JList.
     * @param stateModel destination (DefaultListModel)
     * @param stateList input
     * 
     */
    private void showStateProperty(DefaultListModel stateModel, List stateList){
	stateModel.clear();
	ListIterator li = stateList.listIterator();
	while (li.hasNext()) {
	    oclSS ss = (oclSS)li.next();
	    stateModel.addElement(ss);
	}
    }

    /*
     * when ok button is pressed, close Window only.
     * 
     */
    private void jbn_OKActionPerformed () {
	closeDialog();
    }
    
    /*
     * close dialog window
     * 
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }
    
    /**
     * factory method to create and display box
     * @param parent owner
     * @param preStates states before execution
     * @param postStates states after execution
     * 
     */
    public static void showProperty(JFrame parent, List preStates, List postStates){
	StateProperty pw = new StateProperty(parent, preStates, postStates);
	pw.setLocation((int) (0.5 * parent.getWidth()),(int) (0.5 * parent.getHeight()));
	pw.show();
    }

}

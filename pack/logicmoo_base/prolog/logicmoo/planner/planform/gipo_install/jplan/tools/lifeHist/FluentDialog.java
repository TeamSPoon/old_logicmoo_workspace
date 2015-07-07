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
 *
 *
 * @author ron
 *
 * Dialog to allow the Fluents to be bothe tested and updated
 * Provides means of writing expressions using fluents
 */

package jplan.tools.lifeHist;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import java_cup.runtime.Symbol;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;

import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphModel;
import org.jgraph.graph.DefaultGraphCell;

import jplan.general.CheckResultFrame;
import jplan.general.EdTextComboBox;
import jplan.general.OCLIdentifierDocument;
import jplan.general.OCLIntegerDocument;
import jplan.general.TextComboBoxEditor;
import jplan.general.Utility;
import jplan.ocl.oclExpression;

public class FluentDialog extends JDialog {
	
	private static final int TRANSITION = 0; // This is the default
	private static final int PROCESS = 1;
	
	private LHGraph graph;
	private LHUserObject user;
	private DefaultGraphCell cell;
	private int mode = TRANSITION;


	//Interface objects

	private JComboBox cmbAction, cmbSort, cmbFluent;
	private JTextPane expression;
	protected transient JTable table;
	protected transient DefaultTableModel dataModel;

	/**
	 * constructor used by transition cells
	 * @param graph
	 * @param user
	 * @param cell
	 */
	public FluentDialog(
		LHGraph graph,
		LHUserObject user,
		TransitionCell cell) {
		super((Frame) SwingUtilities.windowForComponent(graph), "", false);
		this.graph = graph;
		this.cell = cell;
		this.user = user;
		initComponents();
	}
	
	/**
	 * constructor used by Process Cells
	 * @param graph
	 * @param user
	 * @param cell
	 */
	public FluentDialog(
			LHGraph graph,
			LHUserObject user,
			ProcessCell cell) {
			super((Frame) SwingUtilities.windowForComponent(graph), "", false);
			this.graph = graph;
			this.cell = cell;
			this.user = user;
			mode = PROCESS;
			initComponents();
		}

	private void initComponents() {
		String objSort = (String) user.getProperty("ObjectSort");
		setTitle(
			"Expression Editor [" + (String) user.getProperty("label") + "]");
		Container fContentPane = getContentPane();
		fContentPane.setLayout(new BorderLayout());
		JPanel top = new JPanel();
		JPanel midContent = new JPanel();
		midContent.setLayout(new BoxLayout(midContent, BoxLayout.Y_AXIS));
		JPanel cmbPanel = new JPanel();
		cmbPanel.setLayout(new BoxLayout(cmbPanel, BoxLayout.X_AXIS));
		JPanel actPanel = new JPanel(new BorderLayout());
		JPanel sortPanel = new JPanel(new BorderLayout());
		JPanel fluentPanel = new JPanel(new BorderLayout());
		actPanel.add(new JLabel("Action"), BorderLayout.NORTH);
		sortPanel.add(new JLabel("Object"), BorderLayout.NORTH);
		fluentPanel.add(new JLabel("Fluent"), BorderLayout.NORTH);
		cmbAction = new JComboBox();
		cmbSort = new JComboBox();
		cmbFluent = new JComboBox();
		actPanel.add(cmbAction, BorderLayout.CENTER);
		sortPanel.add(cmbSort, BorderLayout.CENTER);
		fluentPanel.add(cmbFluent, BorderLayout.CENTER);
		cmbPanel.add(actPanel);
		cmbPanel.add(sortPanel);
		cmbPanel.add(fluentPanel);
		fContentPane.add(BorderLayout.NORTH, cmbPanel);
		popComboBoxes();
		expression = new JTextPane();
		JScrollPane scrPane = new JScrollPane(expression);
		midContent.add(scrPane);
		
		
		dataModel =
			new DefaultTableModel(new Object[] { "Expression"}, 0);
		table = new JTable(dataModel);
		table.setEnabled(true);
		table.setRowSelectionAllowed(true);
		JScrollPane scrollpane = new JScrollPane(table);
		midContent.add(scrollpane);
		fContentPane.add(BorderLayout.CENTER, midContent);
		popDataModel();
		
		JButton okButton = new JButton("OK");
		JButton cancelButton = new JButton("Cancel");
		JButton deleteButton = new JButton("Delete");
		JButton addButton = new JButton("Add");
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(okButton);
		buttonPanel.add(cancelButton);
		buttonPanel.add(deleteButton);
		buttonPanel.add(addButton);
		fContentPane.add(BorderLayout.SOUTH, buttonPanel);
		deleteButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				deleteExpression();
			}
		});
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				commit();
				dispose();
			}
		});
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Nothing to do
				dispose();
			}
		});
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addExpression();
			}
		});
		setSize(new Dimension(300, 300));
		setLocationRelativeTo((Frame) SwingUtilities.windowForComponent(graph));
		show();
	}
	
	private void popDataModel(){
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()) {
			String key = (String) it.next();
			if (key.startsWith("FExpr")) {
				String strExp = (String)user.getProperty(key);
				dataModel.addRow(new Object[] {strExp});
			}
		}
	}

	/**
	 * popComboBoxes
	 * initialise the combo boxes content
	 *
	 */
	private void popComboBoxes() {
		cmbAction.addItem("test");
		cmbAction.addItem("assign");
		cmbAction.addItem("increase");
		cmbAction.addItem("decrease");
		cmbAction.addItem("scaleup");
		cmbAction.addItem("scaledown");
		if (mode == PROCESS) {
			processPopFluents();
			return;
		}
		cmbSort.addItem((String) user.getProperty("ObjectSort"));
		//TODO look for merges and add their sort and fluents
		//	First find a state for this transition
		StateCell sortCell = null;
		LHUserObject stateUser = null;
		String sortName = ((TransitionCell) cell).getObjectSort();
		GraphModel model = graph.getModel();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				LHUserObject userObj =
					(LHUserObject) ((StateCell) curRoot).getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				Utility.debugPrintln(
					"patterns",
					"Found State for " + curSortName);
				if (curSortName.equals(sortName)) {
					sortCell = (StateCell) curRoot;
					stateUser = userObj;
					break;
				}
			}
		}
		if (sortCell == null) {
			JOptionPane.showMessageDialog(
				SwingUtilities.windowForComponent(graph),
				"Connect Transition to State Cells First",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		Set keys = stateUser.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()) {
			String key = (String) it.next();
			if (key.startsWith("Fluent")) {
				cmbFluent.addItem((String) stateUser.getProperty(key));
			}
		}
	}
	
	/**
	 * processPopFluents
	 * populate combo boxes for this process
	 *
	 */
	public void processPopFluents(){
		ProcessCell proc = (ProcessCell)cell;
		List fluents = proc.collectKnownFluents(graph);
		Iterator li = fluents.listIterator();
		while(li.hasNext()) {
			String fluent = (String)li.next();
			cmbFluent.addItem(fluent);
		}
		List sorts = proc.collectKnownSorts(graph);
		li = sorts.listIterator();
		while(li.hasNext()) {
			String sort = (String)li.next();
			if (sort.length() > 0)
				cmbSort.addItem(sort);
		}
	}
	
	/**
	 * addExpression
	 * parse current expression and add it to the data model
	 *
	 */
	private void addExpression(){
		oclExpression curExp = checkExpression();
		if (curExp == null)
			return;
		String strExp = curExp.toString();
		// Check if constraint is new
		boolean done = false;
		// Do we have it already?  Need to check data model
		// We can only have one so just update it
		int row = 0;
		boolean found = false;
		while (!found && row < dataModel.getRowCount()) {
			if (strExp.equalsIgnoreCase((String)dataModel.getValueAt(row,0))) {
				JOptionPane.showMessageDialog(
						SwingUtilities.windowForComponent(graph),
						"Expression is already in list",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					return;
			}
			row++;
		}
		if (!done) {
			// Must be new constraint
			dataModel.addRow(new Object[] {strExp});
			expression.setText("");
		}
		
	}
	
	private void deleteExpression(){
		int row = table.getSelectedRow();
		if (row == -1) {
			JOptionPane.showMessageDialog(
					SwingUtilities.windowForComponent(graph),
					"Please select the expression in the table to be deleted",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
		} 
		dataModel.removeRow(row);
	}
	/**
	 * commit
	 * do the updating by parsing and building the oclPlus expressions
	 * and save the text as a property of the user object
	 *
	 */
	private void commit(){
		String strPropID = "FExpr";
		int suffix = 2;
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		List delKeys = new ArrayList();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("FExpr")) {
				delKeys.add(key);
			}
		}
		it = delKeys.listIterator();
		while (it.hasNext()){
			user.deleteProperty((String)it.next());
		}
		// Now add the new constraints
		int row = 0;
		String strSuffix = "";
		boolean addProperty = false;
		while (row < dataModel.getRowCount()) {
			String curExp = (String)dataModel.getValueAt(row,0);
			user.putProperty(strPropID + strSuffix,curExp);
			addProperty = true;
			strSuffix = new String("" + suffix++);
			row++;
		}
		if (addProperty && cell instanceof TransitionCell) {
			((TransitionCell)cell).setRole(TransitionCell.VALUE_TRANSITION);
			// Should set attributes and change border color
			//TODO this may be removed hence role set back
		}
	}
	
	/**
	 * checkExpression
	 * parse the entered text
	 * @return
	 */
	private oclExpression checkExpression(){
		String exp = expression.getText();
		Symbol parsRes = null;
		oclExpression curExp = null;
		List mssgs = new ArrayList();
		int errPos = -1;
		try {
			StringReader sread = new StringReader(exp);
			parsRes = new parser(new Yylex(sread)).parse();
			curExp = (oclExpression) (parsRes.value);
		} catch (Exception e) {
			mssgs = parser.getWarnMssgs();
			ListIterator li;
			li = mssgs.listIterator();
			String strMssgs = "";
			while (li.hasNext()) {
				String err = (String) li.next();
				int start = err.indexOf("position ");
				int nend = err.indexOf(" of input");
				if (start != -1)
					try {
						errPos = Integer.parseInt(err.substring(start+9,nend));
					} catch (NumberFormatException ex) {
						errPos = -1;
					}
				strMssgs = strMssgs.concat(err);
				Utility.debugPrintln("life", "ERROR " + err);
				Utility.debugPrintln("life", "ERROR POSITION " + errPos);
			}
			if (errPos != -1) {
				this.requestFocus();
				expression.requestFocus();
				expression.select(errPos,exp.length());
				JOptionPane.showMessageDialog(
					SwingUtilities.windowForComponent(graph),
					strMssgs,
					"Syntax Error",
					JOptionPane.ERROR_MESSAGE);
			} else {
				JOptionPane.showMessageDialog(
						SwingUtilities.windowForComponent(graph),
						"Unexpected end of text",
						"Syntax Error",
						JOptionPane.ERROR_MESSAGE);
			}
			return null;
		}
		Utility.debugPrintln("life", "Expression = " + curExp.toString());
		return curExp;
	}

}

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
 *
 * @author ron
 *
 * Dialog to allow the properties of states to be edited
 */

package jplan.tools.lifeHist;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;

import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphModel;
import org.jgraph.graph.DefaultGraphCell;

import jplan.general.EdTextComboBox;
import jplan.general.OCLIdentifierDocument;
import jplan.general.OCLIntegerDocument;
import jplan.general.TextComboBoxEditor;
import jplan.general.Utility;

public class PropertyDialog extends JDialog {
	
	public final static int STATEPROPERTY = 0;
	public final static int PROPERTYCHANGE = 1;
	public final static int PROPERTYCONSTRAINT = 2;
	public final static int NUMBERCONSTRAINT = 3;
	public final static int COORDINATIONCONSTRAINT = 4;
	public final static int STATEFLUENT = 5;
	public final static int TRANSFORMPROPERTY = 6;
	public final static int PROCESSPRECONDITION = 7;
	
	int role = STATEPROPERTY;
	
	LHGraph graph = null;
	DefaultGraphCell cell = null;
	LHUserObject user = null;
	// Screen items
	protected EdTextComboBox cmbLeft = null;
	protected EdTextComboBox cmbRight = null;
	protected transient JTable table;
	protected JTextField inputText = null;
	protected JComboBox cmbSource = null;
	protected JComboBox cmbDest = null;
	protected JCheckBox chkOnOff = null;
	protected transient DefaultTableModel dataModel;
	
	
	public PropertyDialog(int role,LHGraph graph, LHUserObject user, StateCell cell){
		super((Frame) SwingUtilities.windowForComponent(graph),"",false);
		this.role = role;
		this.graph = graph;
		this.cell = cell;
		this.user = user;
		initComponents();
	}
	
	public PropertyDialog(int role,LHGraph graph, LHUserObject user, TransitionCell cell){
		super((Frame) SwingUtilities.windowForComponent(graph),"",false);
		this.role = role;
		this.graph = graph;
		this.cell = cell;
		this.user = user;
		initComponents();
	}
	
	public PropertyDialog(int role,LHGraph graph, LHUserObject user, MergeEdge cell){
		super((Frame) SwingUtilities.windowForComponent(graph),"",false);
		this.role = role;
		this.graph = graph;
		this.cell = cell;
		this.user = user;
		initComponents();
	}
	
	public PropertyDialog(int role,LHGraph graph, LHUserObject user, ProcessEdge cell){
		super((Frame) SwingUtilities.windowForComponent(graph),"",false);
		this.role = role;
		this.graph = graph;
		this.cell = cell;
		this.user = user;
		initComponents();
	}
	
	/**
	 * initComponents
	 * * build the dialog box
	 */
	private void initComponents(){
		Utility.debugPrintln("patterns","INIT Properties");
		String objSort = (String) user.getProperty("ObjectSort");
		switch (role) {
			case STATEPROPERTY:
				setTitle("Properties [" + objSort + "]");
				break;
			case PROPERTYCHANGE:
				setTitle("Change [" + objSort + "]");
				break;
			case NUMBERCONSTRAINT:
				setTitle("Participants [" + objSort + "]");
				break;
			case COORDINATIONCONSTRAINT:
				setTitle("Coordinate");
				break;
			case STATEFLUENT:
				setTitle("Fluents ["+ objSort + "]");
				break;
			case TRANSFORMPROPERTY:
				setTitle("Transforming Property [" + objSort + "]");
				break;
			case PROCESSPRECONDITION:
				setTitle("Property Precondition [" + objSort + "]");
				break;
		}
		
		Container fContentPane = getContentPane();
		fContentPane.setLayout(new BorderLayout());
		JPanel top = new JPanel();
		JPanel cmbPanel = new JPanel();
		cmbPanel.setLayout(new BorderLayout());
		if (role == STATEPROPERTY) {
			dataModel =
				new DefaultTableModel(new Object[] { "Property Name", "Value Type" }, 0);
		} else if (role == PROPERTYCHANGE) {
			dataModel =
				new DefaultTableModel(new Object[] { "Constraint", "Property" }, 0);
		} else if (role == NUMBERCONSTRAINT) {
			dataModel =
				new DefaultTableModel(new Object[] { "Relationship", "Number Needed" }, 0);
		} else if (role == COORDINATIONCONSTRAINT) {
			dataModel =
				new DefaultTableModel(new Object[] { "Constraint", "Source" ,"Destination"}, 0);
		} else if (role == STATEFLUENT) {
			dataModel =
				new DefaultTableModel(new Object[] { "Fluent Name","Static"}, 0);
		}  else if (role == TRANSFORMPROPERTY) {
			dataModel =
				new DefaultTableModel(new Object[] { "Property Name"}, 0);
		} else if (role == PROCESSPRECONDITION) {
			dataModel =
				new DefaultTableModel(new Object[] { "Boolean Property Name"}, 0);
		}
		JPanel midContent = new JPanel();
		cmbLeft = new EdTextComboBox();
		cmbLeft.setEditor(new TextComboBoxEditor(
				new OCLIdentifierDocument()));
		midContent.add(cmbLeft);
		if (role == NUMBERCONSTRAINT) {
			inputText = new JTextField(12);
			inputText.setDocument(new OCLIntegerDocument());
			midContent.add(inputText);
		} else if (role == COORDINATIONCONSTRAINT) {
			cmbSource = new JComboBox();
			cmbDest = new JComboBox();
			midContent.add(cmbSource);
			midContent.add(cmbDest);
		} else if (role == STATEFLUENT) {
			chkOnOff = new JCheckBox("Is Static");
			midContent.add(chkOnOff);
			
		} else if (role == TRANSFORMPROPERTY || role == PROCESSPRECONDITION) {
			// No right component to add
		} else {
			cmbRight = new EdTextComboBox();
			cmbRight.setEditor(new TextComboBoxEditor(
					new OCLIdentifierDocument()));
			midContent.add(cmbRight);
		}
		top.add(midContent);

		table = new JTable(dataModel);
		table.setEnabled(true);
		table.setRowSelectionAllowed(true);
		JScrollPane scrollpane = new JScrollPane(table);
		
		fContentPane.add(BorderLayout.NORTH, top);
		fContentPane.add(BorderLayout.CENTER, scrollpane);
		
		// Populate combo boxes and store initial setting
		boolean buildOK = true;
		switch (role) {
			case STATEPROPERTY:
				popStateProps();;
				break;
			case PROPERTYCHANGE:
				popPropertyChange();
				break;
			case NUMBERCONSTRAINT:
				popNumberConstraint();
				break;
			case COORDINATIONCONSTRAINT:
				buildOK = popCoordinationConstraints();
				break;
			case STATEFLUENT:
				popStateFluents();
				break;
			case TRANSFORMPROPERTY:
				popTransformStateProps();
				break;
			case PROCESSPRECONDITION:
				popProcessPrecondition();
				break;
		}
		if (!buildOK)
			return;
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
				switch (role) {
					case STATEPROPERTY:
						deleteProperty();
						break;
					case PROPERTYCHANGE:
						deletePropertyChange();
						break;
					case NUMBERCONSTRAINT:
						deleteNumberConstraint();
						break;
					case COORDINATIONCONSTRAINT:
						deleteCoordinationConstraint();
						break;
					case STATEFLUENT:
						deleteFluent();
						break;
					case TRANSFORMPROPERTY:
					case PROCESSPRECONDITION:
						deletePropertyName();
						break;
				}
			}
		});
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				switch (role) {
					case STATEPROPERTY:
						updateStateProperties(graph.getModel(),user.getObjectSort((StateCell)cell));
						break;
					case PROPERTYCHANGE:
						updateTransitionPropertyChanges();
						break;
					case NUMBERCONSTRAINT:
						updateNumberConstraints();
						break;
					case COORDINATIONCONSTRAINT:
						updateCoordinationConstraints();
						break;
					case STATEFLUENT:
						updateStateFluents(graph.getModel(),user.getObjectSort((StateCell)cell));
						break;
					case TRANSFORMPROPERTY:
						updateTransformProperty();
						break;
					case PROCESSPRECONDITION:
						updateProcessPrecondition();
						break;
				}
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
				switch (role) {
					case STATEPROPERTY:
						addProperty();
						break;
					case PROPERTYCHANGE:
						addPropertyChange();
						break;
					case NUMBERCONSTRAINT:
						addNumberConstraint();
						break;
					case COORDINATIONCONSTRAINT:
						addCoordinationConstraint();
						break;
					case STATEFLUENT:
						addFluent();
						break;
					case TRANSFORMPROPERTY:
					case PROCESSPRECONDITION:
						addPropertyName();
						break;
				}
			}
		});
		setSize(new Dimension(300, 300));
		setLocationRelativeTo((Frame) SwingUtilities.windowForComponent(graph));

		show();
	}
	
	/**
	 * popStateProps
	 * populate combo boxes for state properties
	 * and the table to view existing properties
	 */
	private void popStateProps(){
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Property") && !key.startsWith("Property Type")) {
				cmbLeft.addItem((String)user.getProperty(key));
				String propType = "Property Type" + key.substring("Property".length());
				cmbRight.addItem((String)user.getProperty(propType));
				dataModel.addRow(new Object[] {(String)user.getProperty(key),(String)user.getProperty(propType) });
			}
		}
	}
	
	/**
	 * popTransformStateProps
	 * populate combo boxes for state properties
	 * and the table to view existing properties
	 */
	private void popTransformStateProps(){
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			Utility.debugPrintln("patterns","Key >> "+ key);
			if (key.startsWith("Property") && !key.startsWith("Property Type")) {
				cmbLeft.addItem((String)user.getProperty(key));
			} else if (key.startsWith("Transform")) {
				dataModel.addRow(new Object[] {(String)user.getProperty(key) });
			}
		}
	}
	
	/**
	 * popStateFluents
	 * populate combo boxes for state fluents
	 * and the table to view existing fluents
	 */
	private void popStateFluents(){
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Fluent")) {
				cmbLeft.addItem((String)user.getProperty(key));
				dataModel.addRow(new Object[] {(String)user.getProperty(key),"false"});
			} else if (key.startsWith("StaticFluent")) {
				cmbLeft.addItem((String)user.getProperty(key));
				dataModel.addRow(new Object[] {(String)user.getProperty(key),"true"});
			}
		}
	}
	
	/**
	 * popProcessPrecondition
	 *
	 */
	private void popProcessPrecondition(){
		TransitionCell source = (TransitionCell)((ProcessEdge)cell).getEdgeSource();
		LHUserObject sUser = (LHUserObject)source.getUserObject();
		Set keys = sUser.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("ValueSort")) {
				String propName = (String)sUser.getProperty(key);
				String propTypeKey = "EdgeName" + key.substring("ValueSort".length());
				String propType = ((String)sUser.getProperty(propTypeKey));
				if ("negation".equals(propType)) {
					cmbLeft.addItem(propName);
				}
			} 
		}
		keys = user.properties.keySet();
		it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Precondition")) {
				String propName = (String)user.getProperty(key);
				dataModel.addRow(new Object[] {propName});
			} 
		}
		
	}
	
	/**
	 * popNumberConstraint
	 * populate combo boxes for number constraints and the
	 * table to view existing constraints
	 */
	private void popNumberConstraint(){
		cmbLeft.addItem("none");
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("constraint")) {
				String relationship = (String)user.getProperty(key);
				if (!relationship.equals("none")) {
					cmbLeft.addItem(relationship);
				}
				String strNum = "number" + key.substring("constraint".length());
				dataModel.addRow(new Object[] {relationship,(String)user.getProperty(strNum) });
			}
		}
	}
	
	/**
	 * popPropertyChange
	 * populate combo boxes for state property changes
	 */
	private void popPropertyChange(){
		// First find a state for this transition
		StateCell sortCell = null;
		LHUserObject stateUser = null;
		String sortName = ((TransitionCell)cell).getObjectSort();
		GraphModel model = graph.getModel();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				LHUserObject userObj = (LHUserObject) ((StateCell) curRoot)
						.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				Utility.debugPrintln("patterns","Found State for " + curSortName);
				if (curSortName.equals(sortName)) {
					sortCell = (StateCell)curRoot;
					stateUser = userObj;
					break;
				}
			}
		}
		if (sortCell == null ){
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Connect Transition to State Cells First", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		cmbLeft.addItem("none");
		Set keys = stateUser.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			Utility.debugPrintln("patterns","Key >> "+ key);
			if (key.startsWith("Property") && !key.startsWith("Property Type")) {
				cmbRight.addItem((String)stateUser.getProperty(key));
				//String propType = "Property Type" + key.substring("Property".length());
				//sortOrg.add((String)user.getProperty(propType));
				//indexOrg.add(key.substring("Property".length()));
				//Utility.debugPrintln("patterns","PropertyType >> "+ key.substring("Property".length()));
				//cmbPropertyTypeName.addItem((String)user.getProperty(propType));
			}
		}
		keys = user.properties.keySet();
		it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("EdgeName")) {
				cmbLeft.addItem((String)user.getProperty(key));
				String propType = "ValueSort" + key.substring("EdgeName".length());
				// The property should already be in the right combo box
				String constName = (String)user.getProperty(key);
				if (constName.equals("fullyConnected")) {
					dataModel.addRow(new Object[] {"none",(String)user.getProperty(propType) });
				} else {
					dataModel.addRow(new Object[] {constName,(String)user.getProperty(propType) });
				}
			}
		}
	}
	
	/**
	 * popMergeCoordination
	 * populate combo boxes for merges
	 */
	private boolean popCoordinationConstraints(){
		String sourceSort = null;
		String destSort = null;
		StateCell sourceState = null;
		LHUserObject sourceUser = null;
		StateCell destState = null;
		LHUserObject destUser = null;
		DefaultGraphCell source = ((MergeEdge)cell).getSourceCell();
		DefaultGraphCell dest = ((MergeEdge)cell).getDestinationCell();
		if (source instanceof StateCell) {
			sourceSort = ((StateCell)source).getObjectSort();
		} else {
			sourceSort = ((TransitionCell)source).getObjectSort();
		}
		if (dest instanceof TransitionCell) {
			destSort = ((TransitionCell)dest).getObjectSort();
		}
		// First find a state for source
		StateCell sortCell = null;
		LHUserObject stateUser = null;
		GraphModel model = graph.getModel();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				LHUserObject userObj = (LHUserObject) ((StateCell) curRoot)
						.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				Utility.debugPrintln("patterns","Found State for " + curSortName);
				if (curSortName.equals(sourceSort)) {
					sourceState = (StateCell)curRoot;
					sourceUser = userObj;
					break;
				}
			}
		}
		if (sourceState == null ){
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Define the source state nodes First", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return false;
		}
		// Now find a state for destination
		sortCell = null;
		stateUser = null;
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				LHUserObject userObj = (LHUserObject) ((StateCell) curRoot)
						.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				Utility.debugPrintln("patterns","Found State for " + curSortName);
				if (curSortName.equals(destSort)) {
					destState = (StateCell)curRoot;
					destUser = userObj;
					break;
				}
			}
		}
		if (destState == null ){
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Define the destination state nodes First", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return false;
		}
		cmbSource.addItem("isa " + sourceSort);
		Set keys = sourceUser.properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Property") && !key.startsWith("Property Type")) {
				cmbSource.addItem((String)sourceUser.getProperty(key));
			}
		}
		cmbDest.addItem("isa " + destSort);
		keys = destUser.properties.keySet();
		it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Property") && !key.startsWith("Property Type")) {
				cmbDest.addItem((String)destUser.getProperty(key));
			}
		}
		// Now look for coordination constraints in the merge itself
		keys = user.properties.keySet();
		it = keys.iterator();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Coordinate")) {
				String cordName = (String)user.getProperty(key);
				cmbLeft.addItem(cordName);
				String sourceKey = "CoordSource" + key.substring("Coordinate".length());
				String destKey = "CoordDest" + key.substring("Coordinate".length());
				String sourceProp = (String)user.getProperty(sourceKey);
				String destProp = (String)user.getProperty(destKey);
				dataModel.addRow(new Object[] {cordName,sourceProp,destProp});
			}
		}
		return true;
	}
	
	/**
	 * addPropertyChange
	 * add to list of property changes to transition
	 * added when OK selected
	 */
	private void addPropertyChange(){
		String constraintName = (String)cmbLeft.getSelectedItem();
		String propName = (String)cmbRight.getSelectedItem();
		if (constraintName == null) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Please select the constraint name", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (propName.length() == 0 || constraintName.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must define non empty names", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		int inx = 0;
		while (inx  < cmbLeft.getItemCount()) {
			String cName = (String)cmbLeft.getItemAt(inx);
			if (cName.equalsIgnoreCase(constraintName) && !cName.equalsIgnoreCase("none")) {
				JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
						"Constraint already defined", "GIPO Error",
						JOptionPane.ERROR_MESSAGE, null);
				return;
			}
			inx++;
		}
		// Check if constraint is new
		if (constraintName.equals("none")) {
			// this is a potential  fully connected property change 
			// Do we have it already?  Need to check data model
			// Need to look at propertyName
			int row = 0;
			boolean found = false;
			while (!found && row < dataModel.getRowCount()) {
				if ("none".equals((String)dataModel.getValueAt(row,0))) {
					if (propName.equalsIgnoreCase((String)dataModel.getValueAt(row,1))) {
						JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
								"Constraint already defined", "GIPO Error",
								JOptionPane.ERROR_MESSAGE, null);
						return;
					}
				}
				row++;
			}
		}
		// Must be new constraint
		dataModel.addRow(new Object[] {constraintName,propName});
	}
	
	/**
	 * addPropertyName
	 * add to list of properties
	 * added when OK selected
	 */
	private void addPropertyName(){
		String propName = (String)cmbLeft.getSelectedItem();
		if (propName == null) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Please select the property name", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		int row = 0;
		boolean found = false;
		while (!found && row < dataModel.getRowCount()) {
			if (propName.equalsIgnoreCase((String)dataModel.getValueAt(row,0))) {
				JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
						"Property already in list", "GIPO Error",
						JOptionPane.ERROR_MESSAGE, null);
				return;
			}
			row++;
		}
		// Must be new constraint
		dataModel.addRow(new Object[] {propName});
	}
	
	
	/**
	 * addCoordinationConstraint
	 * add a coordination constraint to the destination transition
	 * of a merge
	 * added when OK selected
	 */
	private void addCoordinationConstraint(){
		String constraintName = (String)cmbLeft.getSelectedItem();
		String sourcePropName = (String)cmbSource.getSelectedItem();
		String destPropName = (String)cmbDest.getSelectedItem();
		if (constraintName == null || constraintName.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Please select or enter the constraint name", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		int inx = 0;
		while (inx  < cmbLeft.getItemCount()) {
			String cName = (String)cmbLeft.getItemAt(inx);
			if (cName.equalsIgnoreCase(constraintName) && !cName.equalsIgnoreCase("none")) {
				JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
						"Constraint already defined", "GIPO Error",
						JOptionPane.ERROR_MESSAGE, null);
				return;
			}
			inx++;
		}
		// Must be new constraint
		dataModel.addRow(new Object[] {constraintName,sourcePropName,destPropName});
	}
	
	/**
	 * addNumberConstraint
	 * add to list of number constraints on transition
	 * added when OK selected
	 */
	private void addNumberConstraint(){
		String constraintName = (String)cmbLeft.getSelectedItem();
		String strNum = inputText.getText();
		if (constraintName == null) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Please select the constraint name", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (strNum.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must specify an Integer value", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		try {
			int num = Integer.parseInt(strNum);
		} catch (NumberFormatException ex) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must specify an Integer value", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Check if constraint is new
		boolean done = false;
		// Do we have it already?  Need to check data model
		// We can only have one so just update it
		int row = 0;
		boolean found = false;
		while (!found && row < dataModel.getRowCount()) {
			if (constraintName.equals((String)dataModel.getValueAt(row,0))) {
				int res = JOptionPane.showConfirmDialog(SwingUtilities.windowForComponent(graph),"Relationship already defined. Update","Gipo Query",JOptionPane.YES_NO_OPTION);
				if (res == JOptionPane.YES_OPTION) {
					dataModel.setValueAt(strNum,row,1);
					done = true;
				} else {
					return;
				}
			}
			row++;
		}
		if (!done) {
			// Must be new constraint
			cmbLeft.addItem(constraintName);
			dataModel.addRow(new Object[] {constraintName,strNum});
		}
	}
	
	/**
	 * addProperty
	 * add to list of properties to state
	 * added when OK selected
	 */
	private void addProperty(){
		String propName = (String)cmbLeft.getSelectedItem();
		String propSort = (String)cmbRight.getSelectedItem();
		if (propName == null) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Please select the property name", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (propName.length() == 0 || propSort.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must define non empty names", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		int inx = 0;
		while (inx  < cmbLeft.getItemCount()) {
			String pName = (String)cmbLeft.getItemAt(inx);
			if (pName.equalsIgnoreCase(propName)) {
				JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
						"Property already defined", "GIPO Error",
						JOptionPane.ERROR_MESSAGE, null);
				return;
			}
			inx++;
		}
		// Check if sort is new
		inx = 0;
		boolean sortNew = true;
		while (sortNew && inx  < cmbRight.getItemCount()) {
			String pName = (String)cmbRight.getItemAt(inx);
			if (pName.equalsIgnoreCase(propSort)) {
				sortNew = false;
				propSort = pName;
			}
			inx++;
		}
		// Must be new sort
		cmbLeft.addItem(propName);
		if (sortNew) {
			cmbRight.addItem(propSort);
		}
		dataModel.addRow(new Object[] {propName,propSort });
		
	}
	
	/**
	 * addFluent
	 * add to list of fluents to state
	 * added when OK selected
	 */
	private void addFluent(){
		String propName = (String)cmbLeft.getSelectedItem();
		if (propName == null) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Please select the fluent name", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (propName.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must define non empty names", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		int inx = 0;
		while (inx  < cmbLeft.getItemCount()) {
			String pName = (String)cmbLeft.getItemAt(inx);
			if (pName.equalsIgnoreCase(propName)) {
				JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
						"Fluent already defined", "GIPO Error",
						JOptionPane.ERROR_MESSAGE, null);
				return;
			}
			inx++;
		}
		// Must be new sort
		cmbLeft.addItem(propName);
		if (chkOnOff.isSelected()) {
			dataModel.addRow(new Object[] {propName,"true"});
		} else {
			dataModel.addRow(new Object[] {propName,"false"});
		}
		
	}
	
	/**
	 * deleteProperty
	 * remove selected property from state list
	 */
	private void deleteProperty(){
		String propName = (String)cmbLeft.getSelectedItem();
		String propSort = (String)cmbRight.getSelectedItem();
		if (propName.length() == 0 || propSort.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must select non empty names", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Is property in the dataModel
		int row = 0;
		boolean deleted = false;
		
		while (!deleted && row < dataModel.getRowCount()) {
			String pName = (String)dataModel.getValueAt(row,0);
			if (pName.equalsIgnoreCase(propName)) {
				deleted = true;
				dataModel.removeRow(row);
			}
			row++;
		}
		if (cmbLeft.getSelectedIndex() > -1) {
			cmbLeft.removeItemAt(cmbLeft.getSelectedIndex());
		}
	}
	
	/**
	 * deleteFluent
	 * remove selected fluent from state list
	 */
	private void deleteFluent(){
		String propName = (String)cmbLeft.getSelectedItem();
		if (propName == null || propName.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must select non empty names", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Is property in the dataModel
		int row = 0;
		boolean deleted = false;
		
		while (!deleted && row < dataModel.getRowCount()) {
			String pName = (String)dataModel.getValueAt(row,0);
			if (pName.equalsIgnoreCase(propName)) {
				deleted = true;
				dataModel.removeRow(row);
			}
			row++;
		}
		if (cmbLeft.getSelectedIndex() > -1) {
			cmbLeft.removeItemAt(cmbLeft.getSelectedIndex());
		}
	}
	
	/**
	 * deletePropertyName
	 * remove selected property from list
	 */
	private void deletePropertyName(){
		String propName = (String)cmbLeft.getSelectedItem();
		if (propName == null || propName.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must select The property name", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Is property in the dataModel
		int row = 0;
		boolean deleted = false;
		
		while (!deleted && row < dataModel.getRowCount()) {
			String pName = (String)dataModel.getValueAt(row,0);
			if (pName.equalsIgnoreCase(propName)) {
				deleted = true;
				dataModel.removeRow(row);
			}
			row++;
		}
	}
	
	/**
	 * deletePropertyChange
	 * remove selected property change from transition
	 */
	private void deletePropertyChange(){
		String propName = (String)cmbLeft.getSelectedItem();
		String propSort = (String)cmbRight.getSelectedItem();
		if (propName.length() == 0 || propSort.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must select non empty names", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Is property in the dataModel
		int row = 0;
		boolean deleted = false;
		
		while (!deleted && row < dataModel.getRowCount()) {
			String pName = (String)dataModel.getValueAt(row,0);
			if (pName.equalsIgnoreCase(propName)) {
				if (pName.equals("none")) {
					String pVal = (String)dataModel.getValueAt(row,1);
					if (pVal.equalsIgnoreCase(propSort)) {
						deleted = true;
						dataModel.removeRow(row);
					}
				} else {
					deleted = true;
					dataModel.removeRow(row);
				}
			}
			row++;
		}
		if (deleted) {
			if (!propName.equalsIgnoreCase("none")) {
				cmbLeft.removeItemAt(cmbLeft.getSelectedIndex());
			}
		} else {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Property Changing constraint does not exist.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
		}
	}
	
	/**
	 * deleteCoordinationConstraint
	 * remove selected coordination constraint
	 */
	private void deleteCoordinationConstraint(){
		String propName = (String)cmbLeft.getSelectedItem();
		if (propName == null || propName.length() == 0) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must select the Constraint name to delete", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Is property in the dataModel
		int row = 0;
		boolean deleted = false;
		
		while (!deleted && row < dataModel.getRowCount()) {
			String pName = (String)dataModel.getValueAt(row,0);
			if (pName.equalsIgnoreCase(propName)) {
				deleted = true;
				dataModel.removeRow(row);
			}
			row++;
		}
		if (deleted) {
			cmbLeft.removeItemAt(cmbLeft.getSelectedIndex());
		} else {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Coordination constraint does not exist.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
		}
	}
	
	/**
	 * deleteNumberConstraint
	 * remove selected number constraint from transition
	 */
	private void deleteNumberConstraint(){
		String propName = (String)cmbLeft.getSelectedItem();
		if (propName == null || propName.length() == 0 ) {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"You must select non empty names", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Is property in the dataModel
		int row = 0;
		boolean deleted = false;
		
		while (!deleted && row < dataModel.getRowCount()) {
			String pName = (String)dataModel.getValueAt(row,0);
			if (pName.equalsIgnoreCase(propName)) {
				deleted = true;
				dataModel.removeRow(row);
			}
			row++;
		}
		if (deleted) {
			if (!propName.equalsIgnoreCase("none")) {
				cmbLeft.removeItemAt(cmbLeft.getSelectedIndex());
			}
		} else {
			JOptionPane.showMessageDialog(SwingUtilities.windowForComponent(graph),
					"Number constraint does not exist.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
		}
	}
	
	

	/**
	 * updateStateProperties iterate through nodes finding state nodes matching
	 * selected state and update propery name/sort from each such node.
	 * 
	 * @param model -
	 *            the graph model
	 * @param sortName -
	 *            the sort of the selected object state
	 */
	private void updateStateProperties(GraphModel model, String sortName) {
		String strPropID = "Property";
		String strPropTypeID = "Property Type";
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				LHUserObject userObj = (LHUserObject) ((StateCell) curRoot)
						.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				if (curSortName.equals(sortName)) {
					// First delete all existing properties
					int suffix = 2;
					Set keys = userObj.properties.keySet();
					Iterator it = keys.iterator();
					List delKeys = new ArrayList();
					while (it.hasNext()){
						String key = (String)it.next();
						if (key.startsWith("Property")) {
							delKeys.add(key);
						}
					}
					it = delKeys.listIterator();
					while (it.hasNext()){
						userObj.deleteProperty((String)it.next());
					}
					// Now add the new properies
					int row = 0;
					String strSuffix = "";
					while (row < dataModel.getRowCount()) {
						userObj.putProperty(strPropID + strSuffix,dataModel.getValueAt(row,0));
						userObj.putProperty(strPropTypeID + strSuffix,dataModel.getValueAt(row,1));
						strSuffix = new String("" + suffix++);
						row++;
					}
				}
			}
		}
	}
	
	/**
	 * updateStateFluents iterate through nodes finding state nodes matching
	 * selected state and update fluent name from each such node.
	 * 
	 * @param model -
	 *            the graph model
	 * @param sortName -
	 *            the sort of the selected object state
	 */
	private void updateStateFluents(GraphModel model, String sortName) {
		String strPropID = "Fluent";
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				LHUserObject userObj = (LHUserObject) ((StateCell) curRoot)
						.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				if (curSortName.equals(sortName)) {
					// First delete all existing properties
					Set keys = userObj.properties.keySet();
					Iterator it = keys.iterator();
					List delKeys = new ArrayList();
					while (it.hasNext()){
						String key = (String)it.next();
						if (key.startsWith("Fluent")) {
							delKeys.add(key);
						} else if (key.startsWith("StaticFluent")) {
							delKeys.add(key);
						}
					}
					it = delKeys.listIterator();
					while (it.hasNext()){
						userObj.deleteProperty((String)it.next());
					}
					// Now add the new properies
					int row = 0;
					int suffix = 2;
					int staticsuffix = 2;
					String strSuffix = "";
					String strStaticSuffix = "";
					while (row < dataModel.getRowCount()) {
						if ("true".equals((String)dataModel.getValueAt(row,1))) {
							userObj.putProperty("StaticFluent" + strStaticSuffix,dataModel.getValueAt(row,0));
							strStaticSuffix = new String("" + staticsuffix++);
						} else {
							userObj.putProperty("Fluent" + strSuffix,dataModel.getValueAt(row,0));
							strSuffix = new String("" + suffix++);
						}
						row++;
					}
				}
			}
		}
	}
	
	/**
	 * updateTransformProperty 
	 * updates list of Transform properties associated with this state
	 * @param model -
	 *            the graph model
	 * @param sortName -
	 *            the sort of the selected object state
	 */
	private void updateTransformProperty() {
		//		 First delete all existing transform properties
		int suffix = 2;
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		List delKeys = new ArrayList();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Transform")) {
				delKeys.add(key);
			}
		}
		it = delKeys.listIterator();
		while (it.hasNext()){
			user.deleteProperty((String)it.next());
		}
		// Now add the new properies
		int row = 0;
		String strSuffix = "";
		while (row < dataModel.getRowCount()) {
			user.putProperty("Transform" + strSuffix,dataModel.getValueAt(row,0));
			strSuffix = new String("" + suffix++);
			row++;
		}
	}
	
	/**
	 * updateProcessPrecondition 
	 * updates list of process preconditions associated with this transition
	 * @param model -
	 *            the graph model
	 * @param sortName -
	 *            the sort of the selected object state
	 */
	private void updateProcessPrecondition() {
		//		 First delete all existing transform properties
		int suffix = 2;
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		List delKeys = new ArrayList();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Precondition")) {
				delKeys.add(key);
			}
		}
		it = delKeys.listIterator();
		while (it.hasNext()){
			user.deleteProperty((String)it.next());
		}
		// Now add the new properies
		int row = 0;
		String strSuffix = "";
		while (row < dataModel.getRowCount()) {
			user.putProperty("Precondition" + strSuffix,dataModel.getValueAt(row,0));
			strSuffix = new String("" + suffix++);
			row++;
		}
	}
	
	/**
	 * updateTransitionPropertyChanges
	 */
	private void updateTransitionPropertyChanges() {
		String strPropID = "EdgeName";
		String strPropTypeID = "ValueSort";
		int suffix = 2;
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		List delKeys = new ArrayList();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("EdgeName") || key.startsWith("ValueSort")) {
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
			String curEdge = (String)dataModel.getValueAt(row,0);
			if (curEdge.equalsIgnoreCase("none")) {
				user.putProperty(strPropID + strSuffix,"fullyConnected");
			} else {
				user.putProperty(strPropID + strSuffix,curEdge);
			}
			user.putProperty(strPropTypeID + strSuffix,dataModel.getValueAt(row,1));
			addProperty = true;
			strSuffix = new String("" + suffix++);
			row++;
		}
		if (addProperty && TransitionCell.isStateTransitionCell(cell)) {
			// Need to record this as property changing cell
			((TransitionCell)cell).setRole(TransitionCell.VALUE_TRANSITION);
			if (((TransitionCell)cell).isEvent()) {
				Map nested = new Hashtable();
				AttributeMap attr = new AttributeMap();
				GraphConstants.setValue(attr, user);
				GraphConstants.setBorderColor(attr,Color.BLUE);
				GraphConstants.setGradientColor(attr, Color.PINK);
				nested.put(cell, attr);
				graph.getModel().edit(nested, null, null, null);
			} else {
				Map nested = new Hashtable();
				AttributeMap attr = new AttributeMap();
				GraphConstants.setValue(attr, user);
				GraphConstants.setBorderColor(attr,Color.BLUE);
				GraphConstants.setGradientColor(attr, Color.cyan);
				nested.put(cell, attr);
				graph.getModel().edit(nested, null, null, null);
			}
		} else if (!addProperty && TransitionCell.isValueTransitionCell(cell)) {
			//	Need to record this as NO LONGER property changing cell
			((TransitionCell)cell).setRole(TransitionCell.STATE_TRANSITION);
			if (((TransitionCell)cell).isEvent()) {
				Map nested = new Hashtable();
				AttributeMap attr = new AttributeMap();
				GraphConstants.setValue(attr, user);
				GraphConstants.setBorderColor(attr,Color.PINK);
				GraphConstants.setGradientColor(attr, Color.PINK);
				nested.put(cell, attr);
				graph.getModel().edit(nested, null, null, null);
			} else {
				Map nested = new Hashtable();
				AttributeMap attr = new AttributeMap();
				GraphConstants.setValue(attr, user);
				GraphConstants.setBorderColor(attr,Color.BLACK);
				GraphConstants.setGradientColor(attr, Color.green);
				nested.put(cell, attr);
				graph.getModel().edit(nested, null, null, null);
			}
		}
	}
	
	/**
	 * updateCoordinationConstraints
	 */
	private void updateCoordinationConstraints() {
		String strPropID = "Coordinate";
		String sourceProp = "CoordSource";
		String destProp = "CoordDest";
		int suffix = 2;
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		List delKeys = new ArrayList();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("Coord")) {
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
		boolean addConstraint = false;
		String strOldLabel = (String)user.getProperty("label");
		String label = "";
		if (strOldLabel != null) {
			if (strOldLabel.startsWith("+")) {
				label = "+";
			} else if (strOldLabel.startsWith("-")) {
				label = "-";
			}
		}
		while (row < dataModel.getRowCount()) {
			user.putProperty(strPropID + strSuffix, (String)dataModel.getValueAt(row,0));
			user.putProperty(sourceProp + strSuffix,dataModel.getValueAt(row,1));
			user.putProperty(destProp + strSuffix,dataModel.getValueAt(row,2));
			label = label + " " + (String)dataModel.getValueAt(row,0);
			addConstraint = true;
			strSuffix = new String("" + suffix++);
			row++;
		}
		if (addConstraint ) {
			// Need to record this as Coordination merge
			// We adjust the label to the arrow
			user.putProperty("label", label);
			Map nested = new Hashtable();
			AttributeMap attr = new AttributeMap();
			GraphConstants.setValue(attr, user);
			nested.put(cell, attr);
			graph.getModel().edit(nested, null, null, null);
		} 
	}


	/**
	 * updateNumberConstraints
	 */
	private void updateNumberConstraints() {
		String strPropID = "constraint";
		String strPropTypeID = "number";
		int suffix = 2;
		Set keys = user.properties.keySet();
		Iterator it = keys.iterator();
		List delKeys = new ArrayList();
		while (it.hasNext()){
			String key = (String)it.next();
			if (key.startsWith("constraint") || key.startsWith("number")) {
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
			String curEdge = (String)dataModel.getValueAt(row,0);
			user.putProperty(strPropID + strSuffix,curEdge);
			user.putProperty(strPropTypeID + strSuffix,dataModel.getValueAt(row,1));
			addProperty = true;
			strSuffix = new String("" + suffix++);
			row++;
		}
		if (addProperty) {
			// Need to show as containing constraint
			Map nested = new Hashtable();
			AttributeMap attr = new AttributeMap();
			GraphConstants.setValue(attr, user);
			GraphConstants.setBorderColor(attr, Color.red);
			nested.put(cell, attr);
			graph.getModel().edit(nested, null, null, null);
		} else if (!addProperty) {
			//	Need to remove constraint indication
			Map nested = new Hashtable();
			AttributeMap attr = new AttributeMap();
			GraphConstants.setValue(attr, user);
			GraphConstants.setBorderColor(attr, Color.black);
			nested.put(cell, attr);
			graph.getModel().edit(nested, null, null, null);
		}
	}

	
	/**
	 * addPropertyToSort iterate through nodes finding state nodes matching
	 * selected state and add propery sort to each such node.
	 * 
	 * @param model -
	 *            the graph model
	 * @param sortName -
	 *            the sort of the selected object state
	 * @param propertySort -
	 *            the property sort to add
	 * @param propertyName -
	 *            the name of this property
	 */
	private void addPropertyToSort(GraphModel model, String sortName,
			String propertyName, String propertySort) {
		String strPropID = "Property";
		String strPropTypeID = "Property Type";
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				LHUserObject userObj = (LHUserObject) ((StateCell) curRoot)
						.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				if (curSortName.equals(sortName)) {
					int suffix = 2;
					boolean found = false;
					String propVal = (String) userObj.getProperty("Property");
					while (!found && propVal != null) {
						if (propVal.equals(propertySort)) {
							found = true;
						} else {
							propVal = (String) userObj.getProperty("Property"
									+ suffix++);
						}
					}
					if (!found) {
						if (suffix != 2) {
							strPropID = "Property" + (suffix - 1);
							strPropTypeID = "Property Type" + (suffix - 1);
						}
						userObj.putProperty(strPropID, propertyName);
						userObj.putProperty(strPropTypeID, propertySort);
					} else {
						userObj.putProperty(strPropID, propertyName);
						userObj.putProperty(strPropTypeID, propertySort);
					}
				}
			}
		}
	}

}

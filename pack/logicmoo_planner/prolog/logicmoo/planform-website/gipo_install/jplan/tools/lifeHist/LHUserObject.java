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
 * Created on 16-Oct-2004
 *
 * Author ron
 * 
 */

package jplan.tools.lifeHist;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.border.*;

import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;

import jplan.general.EdTextComboBox;
import jplan.general.OCLIdentifierDocument;
import jplan.general.TextComboBoxEditor;
import jplan.general.Utility;
import jplan.tools.lifeHist.OclModel.Property;

public class LHUserObject
implements Serializable,Cloneable {
	
	/* defaultKey of the property used to return a value from the toString method
	 */
	public static String keyValue = "label";
	public static String constraintKey = "constraint";
	
	
	/* Map that holds the attributes (key value pairs)
	 */
	protected Map properties;
	
	protected Map internalPrivateConnections;
	
	protected transient JDialog propertyDlg;
	
	protected transient JTable table;
	
	protected transient DefaultTableModel dataModel;
	
	public LHUserObject() {
		// empty constructor required by XMLDecoder
		this(null, null);
	}
	
	/**
	 * 
	 */
	public LHUserObject(String label, Map properties) {
		if (properties == null)
			properties = new Hashtable();
		this.properties = new Hashtable(properties);
		if (label != null)
			setValue(label);
	}
	
	public void setValue(Object label) {
		putProperty(keyValue, label);
	}
	
	public LHUserObject(String label) {
		this(label, null);
	}
	
	public LHUserObject(Map properties) {
		this(null, properties);
	}
	
	public Object getProperty(Object key) {
		return properties.get(key);
	}
	
	public Object putProperty(Object key, Object value) {
		if (value != null)
			return properties.put(key, value);
		return properties.remove(key);
	}
	
	/**
	 * @return
	 */
	public Map getProperties() {
		return properties;
	}
	
	/**
	 * @param map
	 */
	public void setProperties(Map map) {
		properties = map;
	}
	/**
	 * switchStateDescription
	 * Toggle full description with short description
	 * Short description - just state name
	 * Full Description State nemae and all properties
	 * @param on
	 */
	public void switchStateDescription(boolean on){
		if (on) {
			String desc = "<html>" + getProperty("label") + 
			        "<hr><font color=blue> Property::Value</font>";
			Map properties = getProperties();
			String strProp = "Property";
			String strArg = "Property Type";
			int suffix = 2;
			String propName = (String) getProperty(strProp);
			if (propName == null) {
				desc = "<html>" + getProperty("label");
			}
			// TODO This is not good enough
			while (propName != null) {
				String propArgSort = (String)getProperty(strArg);
				if (propArgSort != null) {
					desc = desc.concat("<br>" + propName + "::" + propArgSort);
				}
				propName = (String) getProperty("Property" + suffix);
				strArg = "Property Type" + suffix++;
			}
			desc = desc.concat("</html>");
			putProperty("label",desc);
		} else {
			String desc = (String)getProperty("label");
			String stateName = null;
			if (desc.indexOf("<br>") == -1) {
				stateName = desc.substring(6,desc.indexOf("</html>"));
			} else {
				stateName = desc.substring(6,desc.indexOf("<hr>"));
			}
			putProperty("label",stateName);
		}
	}
	
	/**
	 * switchVTDescription
	 * Toggle full description with short description
	 * Short description - just transition
	 * Full Description Transition Name and Value changing predicates
	 * @param on
	 */
	public void switchVTDescription(boolean on){
		if (on) {
			String desc = "<html><center>" + getProperty("label") + 
			        "<hr><font color=red> Constraint::Property</font>";
			Map properties = getProperties();
			String strArg = "ValueSort";
			String strProp = "EdgeName";
			int suffix = 2;
			String propName = (String) getProperty(strProp);
			if (propName == null) {
				desc = "<html><center>" + getProperty("label");
			}
			// TODO This is not good enough
			while (propName != null) {
				String propArgSort = (String)getProperty(strArg);
				if (propArgSort != null) {
					desc = desc.concat("<br>" + propName + "::" + propArgSort);
				}
				propName = (String) getProperty("ValueSort" + suffix);
				strArg = "EdgeName" + suffix++;
			}
			desc = desc.concat("</center></html>");
			putProperty("label",desc);
		} else {
			String desc = (String)getProperty("label");
			String stateName = null;
			if (desc.indexOf("<br>") == -1) {
				stateName = desc.substring(14,desc.indexOf("</center>"));
			} else {
				stateName = desc.substring(14,desc.indexOf("<hr>"));
			}
			putProperty("label",stateName);
		}
	}
	
	/**
	 * showPropertyDialog
	 * This is a general property editor - use as a default only when no more
	 * specific editor has been defined - this gives no user support
	 * @param graph
	 * @param cell
	 */
	protected void showPropertyDialog(final LHGraph graph, final Object cell) {
		Frame frame = (Frame) SwingUtilities.windowForComponent(graph);
		if (frame != null && propertyDlg == null) {
			propertyDlg = new JDialog(frame, "", false);
			Container fContentPane = propertyDlg.getContentPane();
			fContentPane.setLayout(new BorderLayout());
			dataModel =
				new DefaultTableModel(new Object[] { "Key", "Value" }, 0);
			table = new JTable(dataModel);
			JScrollPane scrollpane = new JScrollPane(table);
			
			fContentPane.add(BorderLayout.CENTER, scrollpane);
			JButton okButton = new JButton("OK"); //Translator.getString("OK")
			JButton cancelButton = new JButton("Close"); //Translator.getString("Close")
			JButton applyButton =
				new JButton("Apply"); //Translator.getString("Apply")
			JButton addButton = new JButton("New"); //Translator.getString("New")
			JPanel buttonPanel = new JPanel();
			buttonPanel.add(okButton);
			buttonPanel.add(cancelButton);
			buttonPanel.add(applyButton);
			buttonPanel.add(addButton);
			fContentPane.add(BorderLayout.SOUTH, buttonPanel);
			applyButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					apply(graph, cell, dataModel);
				}
			});
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					apply(graph, cell, dataModel);
					propertyDlg.dispose();
				}
			});
			cancelButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					propertyDlg.dispose();
				}
			});
			addButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					dataModel.addRow(new Object[] { "Key", "Value" });
				}
			});
			propertyDlg.setSize(new Dimension(300, 300));
			propertyDlg.setLocationRelativeTo(frame);
		}
		dataModel = new DefaultTableModel(new Object[] { "Key", "Value" }, 0);
		Iterator it = properties.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry entry = (Map.Entry) it.next();
			dataModel.addRow(new Object[] { entry.getKey(), entry.getValue()});
		}
		table.setModel(dataModel);
		propertyDlg.setTitle("Properties of " + toString());
		propertyDlg.show();
	}
	
	/**
	 * propertyViewer
	 * This is a general property Viewer -
	 * @param graph
	 * @param cell
	 */
	protected void propertyViewer(final LHGraph graph, final Object cell) {
		Frame frame = (Frame) SwingUtilities.windowForComponent(graph);
		final JDialog propertyViewer = new JDialog(frame, "", false);
		Container fContentPane = propertyViewer.getContentPane();
		fContentPane.setLayout(new BorderLayout());
		JPanel tables = new JPanel();
		tables.setLayout(new BoxLayout(tables,BoxLayout.Y_AXIS));
		JButton okButton = new JButton("Close");
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(okButton);
		fContentPane.add(BorderLayout.SOUTH, buttonPanel);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				propertyViewer.dispose();
			}
		});
		if (cell instanceof StateCell) {
			DefaultTableModel dataModel =
				new DefaultTableModel(new Object[] { "Name", "Object Sort" }, 0);
			dataModel.addRow(new Object[] {(String)getProperty("label"),(String)getProperty("ObjectSort")});
			JTable table = new JTable(dataModel);
			JScrollPane scrollpane = new JScrollPane(table);
			tables.add(scrollpane);
			// Next get properties
			dataModel = new DefaultTableModel(new Object[] { "Property", "Value Type" }, 0);
			String strProp = "Property";
			String strArg = "Property Type";
			int suffix = 2;
			String propName = (String) getProperty(strProp);
			boolean found = false;
			while (propName != null) {
				found = true;
				String propArgSort = (String)getProperty(strArg);
				if (propArgSort != null) {
					dataModel.addRow(new Object[] {propName,propArgSort});
				}
				propName = (String) getProperty("Property" + suffix);
				strArg = "Property Type" + suffix++;
			}
			if (found) {
				table = new JTable(dataModel);
				scrollpane = new JScrollPane(table);
				tables.add(scrollpane);
			}
			//TODO If this is oclPlus then there may be fluents
		}
		if (cell instanceof TransitionCell) {
			DefaultTableModel dataModel =
				new DefaultTableModel(new Object[] { "Name", "Object Sort" }, 0);
			dataModel.addRow(new Object[] {(String)getProperty("label"),(String)getProperty("ObjectSort")});
			JTable table = new JTable(dataModel);
			JScrollPane scrollpane = new JScrollPane(table);
			tables.add(scrollpane);
			// Next get property changes
			dataModel = new DefaultTableModel(new Object[] { "Constraint", "Property" }, 0);
			String strProp = "EdgeName";
			String strArg = "ValueSort";
			int suffix = 2;
			String propName = (String) getProperty(strProp);
			boolean found = false;
			while (propName != null) {
				found = true;
				String propArgSort = (String)getProperty(strArg);
				if (propArgSort != null) {
					dataModel.addRow(new Object[] {propName,propArgSort});
				}
				propName = (String) getProperty("EdgeName" + suffix);
				strArg = "ValueSort" + suffix++;
			}
			if (found) {
				table = new JTable(dataModel);
				scrollpane = new JScrollPane(table);
				tables.add(scrollpane);
			}
			//	Next number constraints
			dataModel = new DefaultTableModel(new Object[] { "Constraint", "Number" }, 0);
			strProp = "number";
			strArg = "constraint";
			propName = (String) getProperty(strProp);
			found = false;
			// Can only be one of these
			if (propName != null) {
				found = true;
				String propArgSort = (String)getProperty(strArg);
				if (propArgSort != null) {
					dataModel.addRow(new Object[] {propArgSort,propName});
				}
			}
			if (found) {
				table = new JTable(dataModel);
				scrollpane = new JScrollPane(table);
				tables.add(scrollpane);
			}
			//TODO If this is oclPlus then there may be fluents
		}
		tables.add(Box.createVerticalGlue());
		fContentPane.add(BorderLayout.CENTER, tables);
		propertyViewer.setTitle("Properties of " + toString());
		propertyViewer.setSize(new Dimension(300, 200));
		propertyViewer.setLocationRelativeTo(frame);
		propertyViewer.show();
	}
	
	protected void apply(LHGraph graph, Object cell, TableModel model) {
		properties.clear();
		for (int i = 0; i < model.getRowCount(); i++) {
			//properties.put(model.getValueAt(i, 0), model.getValueAt(i, 1));
			putProperty(model.getValueAt(i, 0), model.getValueAt(i, 1));
			Utility.debugPrintln("patterns","Key = " +model.getValueAt(i, 0)+ " Value = " + model.getValueAt(i, 1));
		}
		Map nested = new Hashtable();
		AttributeMap attr = new AttributeMap();
		Object value = getProperty(keyValue);
		if (value == null)
			putProperty(keyValue, "");
		GraphConstants.setValue(attr, this);
		Object constraint = getProperty("constraint");
		if (constraint != null) {
			GraphConstants.setBorderColor(attr,Color.red);
		}
		nested.put(cell, attr);
		graph.getModel().edit(nested, null, null, null);
	}
	
	/**
	 * findNextPropertySuffix(
	 * find next available property suffix 
	 * @param propName the property name
	 * @return next free number return 0 if no property with name found
	 */
	public int findNextPropertySuffix(String propName) {
		int suffix = 2;
		String pValue = (String) getProperty(propName);
		while (pValue != null) {
			pValue = (String) getProperty(propName
					+ suffix++);
		}
		if (suffix == 2)
			return 0;
		else
			return suffix - 1;
	}
	
	/**
	 * getAllPropertyInstances
	 * @param propName - the key name for the property
	 * @return
	 */
	public List getAllPropertyInstances(String propName) {
		List res = new ArrayList();
		Set keys = properties.keySet();
		Iterator it = keys.iterator();
		while (it.hasNext()) {
			String key = (String) it.next();
			if (key.startsWith(propName)) {
				String propVal = (String)getProperty(key);
				res.add(propVal);
			}
		}
		return res;
	}
	
	
	/**
	 * makeDisjunction
	 * make transition disjunction relative to multiple merge constraints
	 * @param graph
	 * @param cell
	 */
	public void makeDisjunction(LHGraph graph, TransitionCell cell){
		putProperty("Disjunction","true"); 
		Map nested = new Hashtable();
		AttributeMap attr = new AttributeMap();
		GraphConstants.setValue(attr,this);
		//GraphConstants.setBorder(attr, new TitledBorder(new LineBorder(Color.red,4),"or"));
		GraphConstants.setBorderColor(attr,Color.red);
		GraphConstants.setGradientColor(attr,Color.red);
		nested.put(cell,attr);
		graph.getModel().edit(nested, null, null, null);	
	}
	
	/**
	 * removeDisjunction
	 * remove transition disjunction relative to multiple merge constraints
	 * @param graph
	 * @param cell
	 */
	public void removeDisjunction(LHGraph graph, TransitionCell cell){
		deleteProperty("Disjunction");
		// Now need to work out what attributes are needed
		Map nested = new Hashtable();
		AttributeMap attr = new AttributeMap();
		GraphConstants.setValue(attr,this);
		if (cell.isValueTransition()) {
			GraphConstants.setGradientColor(attr,Color.cyan);
		} else {
			GraphConstants.setGradientColor(attr,Color.green);
		}
		Object constraint = getProperty("constraint");
		if (constraint != null) {
			GraphConstants.setBorderColor(attr,Color.red);
		} else {
			GraphConstants.setBorderColor(attr,Color.black);
		}
		nested.put(cell,attr);
		graph.getModel().edit(nested, null, null, null);	
	}
	
	/**
	 * makeEvent
	 * make transition an event
	 * @param graph
	 * @param cell
	 */
	public void makeEvent(LHGraph graph, TransitionCell cell){
		putProperty("Event","true"); 
		Map nested = new Hashtable();
		AttributeMap attr = new AttributeMap();
		GraphConstants.setValue(attr,this);
		if (TransitionCell.isValueTransitionCell(cell)) {
			GraphConstants.setBorderColor(attr,Color.BLUE);
		} else {
			GraphConstants.setBorderColor(attr,Color.PINK);
		}
		GraphConstants.setGradientColor(attr,Color.PINK);
		nested.put(cell,attr);
		graph.getModel().edit(nested, null, null, null);	
	}
	/**
	 * removeEvent
	 * remove event turn transition back to ordinary transition
	 * @param graph
	 * @param cell
	 */
	public void removeEvent(LHGraph graph, TransitionCell cell){
		deleteProperty("Event");
		// Now need to work out what attributes are needed
		Map nested = new Hashtable();
		AttributeMap attr = new AttributeMap();
		GraphConstants.setValue(attr,this);
		if (cell.isValueTransition()) {
			GraphConstants.setBorderColor(attr,Color.BLUE);
			GraphConstants.setGradientColor(attr,Color.cyan);
		} else {
			GraphConstants.setGradientColor(attr,Color.green);
			GraphConstants.setBorderColor(attr,Color.BLACK);
		}
		Object constraint = getProperty("constraint");
		if (constraint != null) {
			GraphConstants.setBorderColor(attr,Color.red);
		} else {
			GraphConstants.setBorderColor(attr,Color.black);
		}
		nested.put(cell,attr);
		graph.getModel().edit(nested, null, null, null);	
	}
	
	/**
	 * addInternalConnection
	 * Store the old source node for edge that has been moved to parent node.
	 * @param oldSource
	 * @param edge
	 */
	public void addInternalConnection(DefaultGraphCell oldSource, DefaultEdge edge){
		if (internalPrivateConnections == null){
			internalPrivateConnections = new Hashtable();
		}
		internalPrivateConnections.put(oldSource,edge);
	}
	
	public Object clone() {
		// TODO: Deep clone?
		return new LHUserObject(null, new Hashtable(properties));
	}
		
	public String toString() {
		Object label = properties.get(keyValue);
		if (label != null)
			return label.toString();
		return super.toString();
	}
	
	// These are some convenience methods - to make code less cluttered
	/**
	 * getObjectSort
	 * fetch the object sort for this node
	 * @param cell - any derivitive of a StateCell
	 * @return
	 */
	public static String getObjectSort(StateCell cell){
		return (String) ((LHUserObject) cell.getUserObject()).getProperty("ObjectSort");
	}
	/**
	 * getObjectSort
	 * fetch the object sort for this node
	 * @param cell - any derivitive of a StateCell
	 * @return
	 */
	public static String getObjectSort(TransitionCell cell){
		return (String) ((LHUserObject) cell.getUserObject()).getProperty("ObjectSort");
	}
	public static String getLabel(DefaultGraphCell cell){
		return (String) ((LHUserObject) cell.getUserObject()).getProperty("label");
	}
	/**
	 * get any string valued proprty from a node
	 * @param cell
	 * @param property
	 * @return
	 */
	public static String getStrObjectProperty(DefaultGraphCell cell,String property) {
		return (String) ((LHUserObject) cell.getUserObject()).getProperty(property);
	}
	
	/**
	 * deleteProperty
	 * Utility to remove a property - in the sence of a hash key
	 * @param propName
	 */
	public void deleteProperty(String propName){
		properties.remove(propName);
	}
}

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

import java.util.*;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphModel;


import jplan.general.Utility;

/**
 * @author ron
 * This is just a wrapper to allow identification of cells
 */
public class StateCell extends DefaultGraphCell {
	
		public StateCell(){
			this(null);
		}
		public StateCell(Object userObject){
			super(userObject);
		}
		public void setUserObject(Object obj){
			if (obj == null) {
				Utility.debugPrintln("patterns", "setUserObject NULL object");
				super.setUserObject(null);
				return;
			}
			Utility.debugPrintln("patterns", "Change attributes label =" + obj.toString());
			if (obj instanceof String ) {
				LHUserObject uObj = (LHUserObject)this.getUserObject();
				uObj.putProperty("label",obj);
				super.setUserObject(uObj);
			} else if (obj instanceof LHUserObject) {
				Utility.debugPrintln("patterns", "Given a UserObject this time");
			}
		}
		
		public String getLabel(){
			return (String)((LHUserObject)getUserObject()).getProperty("label");
		}
		/**
		 * getObjectSort
		 * get the OCL sort for this transition
		 * @return - the sort
		 */
		public String getObjectSort() {
			LHUserObject userObj = (LHUserObject)getUserObject();
			return (String)userObj.getProperty("ObjectSort");
		}
		/**
		 * setObjectSort
		 * set the OCL sort for the object kind
		 * @param sort
		 */
		public void setObjectSort(String sort){
			LHUserObject userObj = (LHUserObject)getUserObject();
			userObj.putProperty("ObjectSort",sort);
		}
		/**
		 * setPackageID
		 * the id of this node within its enclosing package
		 * @param count - current count value
		 */
		public void setPackageID(int count) {
			LHUserObject uObj = (LHUserObject)this.getUserObject();
			uObj.putProperty("PackageID",new Integer(count).toString());
		}
		/**
		 * getPackageID
		 * the id of this node within the enclosing package
		 * @return - the current value
		 */
		public int getPackageID(){
			LHUserObject uObj = (LHUserObject)this.getUserObject();
			String val = (String)uObj.getProperty("PackageID");
			if (val == null) {
				return -1;
			} else {
				return Integer.parseInt(val);
			}
		}
		
		public static void getAllStatePropertyInstances(List given, LHGraph graph, String sortName, String propName){
			StateCell sortCell = null;
			LHUserObject stateUser = null;
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
				return;
			}
			Set keys = stateUser.properties.keySet();
			Iterator it = keys.iterator();
			while (it.hasNext()) {
				String key = (String) it.next();
				if (key.startsWith(propName)) {
					given.add((String) stateUser.getProperty(key));
				}
			}
		}
}

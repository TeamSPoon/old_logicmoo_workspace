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
 * Created on 9-5-05
 *
 * Author ron
 * 
 */
package jplan.tools.lifeHist;


import java.util.*;

import jplan.general.OCLException;
import jplan.general.Utility;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultPort;


/**
 * @author ron
 * Hold an ocl plus cell
 */
public class ProcessCell extends DefaultGraphCell {
	
		public ProcessCell(){
			this(null);
		}
		public ProcessCell(Object userObject){
			super(userObject);
		}
		public void setUserObject(Object obj){
			if (obj == null) {
				super.setUserObject(null);
				return;
			}
			if (obj instanceof String ) {
				LHUserObject uObj = (LHUserObject)this.getUserObject();
				uObj.putProperty("label",obj);
				super.setUserObject(uObj);
			} 
		}
		
		public String getLabel(){
			return (String)((LHUserObject)getUserObject()).getProperty("label");
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
		
		public List collectKnownFluents(LHGraph graph){
			List res = new ArrayList();
			boolean edgeOut = false;
			boolean edgeIn = false;
			String myLabel = LHUserObject.getLabel(this);
			Enumeration enum = children();
			while ( enum.hasMoreElements() ) {
				DefaultPort port = (DefaultPort)enum.nextElement();
				Iterator li = port.edges();	
				while (li.hasNext()) {
					DefaultEdge edge = (DefaultEdge)li.next();
					if (edge instanceof ProcessEdge) {
						DefaultGraphCell connCell = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
						if (connCell != this) {
							if (connCell instanceof StateCell){
								LHUserObject uObj = (LHUserObject)((StateCell)connCell).getUserObject();
								List fluents = uObj.getAllPropertyInstances("Fluent");
								res = Utility.addAllNew(fluents,res);
							} else if (connCell instanceof TransitionCell) {
								String objSort = ((TransitionCell)connCell).getObjectSort();
								List fluents = new ArrayList();
								StateCell.getAllStatePropertyInstances(fluents,graph,objSort,"Fluent");
								res = Utility.addAllNew(fluents,res);
							}

						}
						connCell = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
						if (connCell != this) {
							if (connCell instanceof StateCell){
								LHUserObject uObj = (LHUserObject)((StateCell)connCell).getUserObject();
								List fluents = uObj.getAllPropertyInstances("Fluent");
								res = Utility.addAllNew(fluents,res);
							} else if (connCell instanceof TransitionCell) {
								String objSort = ((TransitionCell)connCell).getObjectSort();
								List fluents = new ArrayList();
								StateCell.getAllStatePropertyInstances(fluents,graph,objSort,"Fluent");
								res = Utility.addAllNew(fluents,res);
							}
							
						}
					}
				}
			}
			return res;
		}
		
		public List collectKnownSorts(LHGraph graph){
			List res = new ArrayList();
			boolean edgeOut = false;
			boolean edgeIn = false;
			String myLabel = LHUserObject.getLabel(this);
			Enumeration enum = children();
			while ( enum.hasMoreElements() ) {
				DefaultPort port = (DefaultPort)enum.nextElement();
				Iterator li = port.edges();	
				while (li.hasNext()) {
					DefaultEdge edge = (DefaultEdge)li.next();
					if (edge instanceof ProcessEdge) {
						DefaultGraphCell connCell = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
						if (connCell != this) {
							if (connCell instanceof StateCell){
								String sort = ((StateCell)connCell).getObjectSort();
								Utility.addNewInOrder(res,sort);
							} else if (connCell instanceof TransitionCell) {
								String objSort = ((TransitionCell)connCell).getObjectSort();
								Utility.addNewInOrder(res,objSort);
							}
						}
						connCell = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
						if (connCell != this) {
							if (connCell instanceof StateCell){
								String sort = ((StateCell)connCell).getObjectSort();
								Utility.addNewInOrder(res,sort);
							} else if (connCell instanceof TransitionCell) {
								String objSort = ((TransitionCell)connCell).getObjectSort();
								Utility.addNewInOrder(res,objSort);
							}
						}
					}
				}
			}
			return res;
		}
			
}

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
 * Created on 22-Jan-2005
 *
 * Author ron
 * 
 */
package jplan.tools.lifeHist;

import java.util.Map;

import org.jgraph.graph.DefaultGraphCell;


import jplan.general.Utility;

/**
 * @author ron
 *
 * This class defines the graph nodes representing Packages
 */
public class PackageCell extends DefaultGraphCell {
	
	public PackageCell(){
		this(null);
	}
	public PackageCell(Object userObject){
		super(userObject);
		//setConnectionCount(0);
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
	 * setPrivateNodes
	 * This is the xml Description of the package sub graph
	 * @param xmlDesc - xml Description
	 */
	public void setPrivateNodes(String xmlDesc) {
		LHUserObject uObj = (LHUserObject)this.getUserObject();
		uObj.putProperty("PrivateNodes",xmlDesc);
	}
	/**
	 * getPrivateNodes
	 * This is the xml description of the sub graph
	 * @return
	 */
	public String getPrivateNodes(){
		LHUserObject uObj = (LHUserObject)this.getUserObject();
		return (String)uObj.getProperty("PrivateNodes");
	}
	
	/**
	 * addRedirectIDs
	 * store the old id of a edge that links an internal node of the package to an external node
	 * @param newID
	 * @param oldID
	 */
	public void addRedirectIDs(int newID,int oldID){
		((LHUserObject)userObject).putProperty("Redirect"+newID,new Integer(oldID).toString());
	}
	/**
	 * getOldEdgeValue
	 * get the ID this edge has in any internal nested package
	 * @param packageID
	 * @return
	 */
	public int getOldEdgeValue(int packageID){
		String id = (String)((LHUserObject)userObject).getProperty("Redirect"+packageID);
		if (id == null)
			return -1;
		return Integer.parseInt(id);
	}
}

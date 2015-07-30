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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jplan.general.OCLException;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultPort;

/**
 * @author ron
 *
 * This is a wrapper to identify process edges
 */
public class ProcessEdge extends DefaultEdge{
	public ProcessEdge(){
		this(null);
	}
	public ProcessEdge(Object userObject){
		super(userObject);
	}
	/**
	 * setPackageID
	 * the id of this node within its enclosing package
	 * @param count - current count value
	 */
	public void setPackageID(int count) {
		Object obj = getUserObject();
		if (!(obj instanceof LHUserObject)) {
			if (getUserObject() == null || "".equals((String)getUserObject())) {
				setUserObject(new LHUserObject(null,null));
				((LHUserObject)getUserObject()).setValue("");
			}
		}
		LHUserObject uObj = (LHUserObject)this.getUserObject();
		uObj.putProperty("PackageID",new Integer(count).toString());
	}
	/**
	 * getPackageID
	 * the id of this node within the enclosing package
	 * @return - the current value
	 */
	public int getPackageID(){
		Object obj = getUserObject();
		if (!(obj instanceof LHUserObject)) {
			if (getUserObject() == null || "".equals((String)getUserObject())) {
				setUserObject(new LHUserObject(null,null));
				((LHUserObject)getUserObject()).setValue("");
			}
		}
		LHUserObject uObj = (LHUserObject)this.getUserObject();
		String val = (String)uObj.getProperty("PackageID");
		if (val == null) {
			return -1;
		} else {
			return Integer.parseInt(val);
		}
	}
	
	/**
	 * getEdgeSource 
	 * find source node of edge
	 * 
	 * @return
	 */
	protected DefaultGraphCell getEdgeSource() {
		return (DefaultGraphCell)((DefaultPort)getSource()).getParent();
	}
	
	/**
	 * getEdgeDestination 
	 * find source node of edge
	 * 
	 * @return
	 */
	protected DefaultGraphCell getEdgeDestination() {
		return (DefaultGraphCell)((DefaultPort)getTarget()).getParent();
	}

}

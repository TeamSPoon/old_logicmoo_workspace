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
 * Created on 25-Oct-2004
 *
 */
package jplan.tools.lifeHist;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultPort;

import jplan.general.OCLException;

/**
 * @author ron
 *
 * This class provides a parent class for all transition type
 * Used for identification of GraphCells
 */
public class TransitionCell extends DefaultGraphCell {
	
	public static final int STATE_TRANSITION = 0;
	public static final int VALUE_TRANSITION = 1;
	
	/**
	 * role determins whether or not cell represents value transition
	 * or simple state transition
	 */
	protected int role = 0;
	
	// TransitionCell
		public TransitionCell(){
			this(null);
		}
		public TransitionCell(Object userObject){
			super(userObject);
		}
		public TransitionCell(Object userObject,int role){
			super(userObject);
			this.role = role;
		}
	
	/**
	 * @return Returns the role.
	 */
	public int getRole() {
		return role;
	}
	/**
	 * @param role The role to set.
	 */
	public void setRole(int role) {
		this.role = role;
	}
	/**
	 * isStateTransition
	 * @return
	 */
	public boolean isStateTransition(){
		return (role == STATE_TRANSITION);
	}
	/**
	 * isValueTransition
	 * @return
	 */
	public boolean isValueTransition(){
		return role == VALUE_TRANSITION;
	}
	/**
	 * isValueTransitionCell
	 * convience to test if object plays the role of a value transition cell
	 * @param cell the object to test
	 */
	public static boolean isValueTransitionCell(Object cell) {
		if (cell instanceof TransitionCell) {
			return ((TransitionCell)cell).isValueTransition();
		}
		return false;
	}
	/**
	 * isStateTransitionCell
	 * convience to test if object plays the role of a state transition cell
	 * @param cell the object to test
	 */
	public static boolean isStateTransitionCell(Object cell) {
		if (cell instanceof TransitionCell) {
			return ((TransitionCell)cell).isStateTransition();
		}
		return false;
	}
	/**
	 * isEventCell
	 * convience to test if object plays the role of a an event cell
	 * @param cell the object to test
	 */
	public static boolean isEventCell(Object cell) {
		if (cell instanceof TransitionCell) {
			return ((TransitionCell)cell).isEvent();
		}
		return false;
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
	
	public boolean isDisjunction(){
		LHUserObject uObj = (LHUserObject)getUserObject();
		String disj = (String)uObj.getProperty("Disjunction");
		return (disj != null && disj.equals("true"));
	}
	
	public boolean isEvent(){
		LHUserObject uObj = (LHUserObject)getUserObject();
		String evnt = (String)uObj.getProperty("Event");
		return (evnt != null && evnt.equals("true"));
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
	
	/**
	 * checkTransitionConnections
	 * check that one edge out and one edge in both linking to states with the same sort
	 * @throws OCLException
	 */
	public void checkTransitionConnections() throws OCLException{
		boolean edgeOut = false;
		boolean edgeIn = false;
		String myLabel = LHUserObject.getLabel(this);
		Enumeration enum = children();
		while ( enum.hasMoreElements() ) {
			DefaultPort port = (DefaultPort)enum.nextElement();
			Iterator li = port.edges();	
			while (li.hasNext()) {
				DefaultEdge edge = (DefaultEdge)li.next();
				if (edge instanceof TransitionEdge) {
					DefaultGraphCell connTrans = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
					if (connTrans != this) {
						if (!(connTrans instanceof StateCell)){
							throw new OCLException("Transition " + myLabel + " is connected by Black arrow to non state node.");
						}
						if (!edgeOut) {
							edgeOut = true;
							if (!getObjectSort().equals(LHUserObject.getObjectSort((StateCell)connTrans))) {
								throw new OCLException("Transition " + myLabel + " sort does not match connecting state sort");
							}
						} else {
							throw new OCLException("Transition " + myLabel + " has more than one outwards connection to another node");
						}
					}
					connTrans = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
					if (connTrans != this) {
						if (!(connTrans instanceof StateCell)){
							throw new OCLException("Transition " + myLabel + " is connected by Black arrow to non state node.");
						}
						if (!edgeIn) {
							edgeIn = true;
							if (!getObjectSort().equals(LHUserObject.getObjectSort((StateCell)connTrans))) {
								throw new OCLException("Transition " + myLabel + " sort does not match connecting state sort");
							}
						} else {
							throw new OCLException("Transition " + myLabel + " has more than one inwards connection to another node");
						}
					}
				}
			}
		}
		if (!edgeOut)
			throw new OCLException("Transition " + myLabel + " is not connected to a target state cell");
		if (!edgeIn)
			throw new OCLException("Transition " + myLabel + " is not connected to a source state cell");
	}
	/**
	 * describe
	 * utility to produce a string describing the transition
	 * @return
	 */
	public String describe(){
		String sourceName = null;
		String targetName = null;
		String me = (String)((LHUserObject)getUserObject()).getProperty("label");
		Enumeration enum = children();
		while ( enum.hasMoreElements() ) {
			DefaultPort port = (DefaultPort)enum.nextElement();
			Iterator li = port.edges();	
			while (li.hasNext()) {
				DefaultEdge edge = (DefaultEdge)li.next();
				if (edge instanceof TransitionEdge) {
					DefaultGraphCell sourceCell = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
					DefaultGraphCell targetCell = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
					if (sourceCell == this) {
						targetName = (String)((LHUserObject)targetCell.getUserObject()).getProperty("label");	
					} else if (targetCell == this) {
						sourceName = (String)((LHUserObject)sourceCell.getUserObject()).getProperty("label");
					}
				}
			}
		}
		return sourceName + "  -> " + me + " -> " + targetName;
	}
}

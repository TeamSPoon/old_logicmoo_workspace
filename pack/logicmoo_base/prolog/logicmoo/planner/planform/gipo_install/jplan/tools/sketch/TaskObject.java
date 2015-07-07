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
 * Created on 03-Sep-2003
 *
 * Author ron
 * 
 */
package jplan.tools.sketch;

import javax.swing.JComponent;
import java.util.*;

import jplan.ocl.oclSS;

/**
 * @author ron
 *
 * This is a wrapper for the task objects to be displayed
 */
public class TaskObject implements DrawableObject {
	public static final int ACTION = 0;
	public static final int OBJECT = 1;
	public static final int PROCESS = 2;
	public static final int EVENT = 3;
	
	private Object taskObject; //The actual thing being represented
	private int objectType;
	
	private String objName;
	private String objSort;
	private JComponent objIcon;
	// The time aspects of the object
	private int startTime = 0;
	private int endTime = 0;
	private List stateList = null;
	private List resultStateList = null; //Ron 24/05/05 added to show effects of applying operator
	

	public TaskObject(String name,String sort){
		objName = name;
		objSort = sort;
		stateList = new ArrayList();
		resultStateList = new ArrayList(); //Ron 24/05/05
	}
	
	
	/**
	 * setObject
	 * store the task object associated with this visible image
	 * @param thing
	 * @param kind
	 */
	public void setObject(Object thing,int kind) {
		taskObject = thing;
		objectType = kind;
		
	}
	
	/**
	 * addResultToResultState(
	 * @param res
	 */
	public void addResultToResultState(oclSS res) {
		oclSS store = null;
		try {
			store = (oclSS)res.clone();
		} catch (CloneNotSupportedException e){}
		resultStateList.add(store);
	}
	
	/**
	 * getResultList
	 * @return
	 */
	public List getResultList(){
		return resultStateList;
	}

	/**
	 * getType
	 * return the object type - i.e. ACTION OBJECT etc
	 * @return = the constant value
	 */
	public int getType() {
		return objectType;
	}
	
	/**
	 * getObject
	 * @return - the object associated with this drawable
	 */
	public Object getObject() {
		return taskObject;
	}
	/**
	 * getTitle
	 * this is the display name
	 * @return - the name
	 */
	public String getTitle() {
		return objName;
	}

	/**
	 * getIcon
	 * @return - the displayable icon
	 */
	public JComponent getIcon() {
		return objIcon;
	}

	/**
	 * Set the displayable icon
	 * @param - the icon
	 */
	public void setIcon(JComponent newIcon) {
		objIcon = newIcon;

	}
	/**
	 * Sets the start time of the Process or the time an Action or Event occurred.
	 * @param time the start time of the Process or the time an Action or Event occurred.
	 */
	public void setStartTime(int time) {
		startTime = time;
	}
	
	/**
	 * Returns the start time of the Process or the time an Action or Event occurred.
	 * @return the start time of the Process or the time an Action or Event occurred.
	 */
	public int getStartTime() {
		return startTime;
	}
	
	/**
	 * Sets the end time of the Process.
	 * This can be ignored if the DrawableObject represents an Action or Event.
	 * @param time the end time of the Process.
	 */
	public void setEndTime(int time) {
		endTime = time;
	}
	
	/**
	 * Gets the end time of the Process.
	 * This can be ignored if the DrawableObject represents an Action or Event.
	 */
	public int getEndTime() {
		return endTime;
	}
	
	/**
	 * addObjectState
	 * @param - a list of object state
	 */
	public void setObjectStates(List states) {
		stateList = states;
	}
	
	/**
	 * getStateList
	 * @return - the list of object ss clauses
	 */
	public List getStateList() {
		return stateList;
	}


}

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
 * Current dynamic state
 * @author Weihong Zhao
 * 13/6/2001
 */


import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import javax.swing.undo.*;
import java.util.Hashtable;

import jplan.ocl.*;
import jplan.graphics.gTool.Graphics.vShape;

/**
 * The class to store all the objects' states.
 */
public class DynamicState implements Cloneable{
    /**
     * Number of objects
     */
    public int stateSize = 0;
    private Hashtable stateShapeList = new Hashtable();
    private oclSS [] dynamicState;
    private int statePointer = -1;
    /**
     * Position on the canvas
     */
    public int x,y;
    private int lineSpace;
    private double d_Height;


  /** 
   * Creates a instance of the DynamicState with given size
   * @param stateSize number of objects involved
   * @param d_Height default height
   * @param lineSpace default space between each object (vShape)
   */
    public DynamicState(int stateSize, double d_Height, int lineSpace) {
	this.stateSize = stateSize;
	dynamicState = new oclSS [stateSize];
	this.d_Height = d_Height;
	this.lineSpace = lineSpace;
    }
    
    /**
     * add to the dynamicState AND register to the stateShapeList
     * @param sp vShape which represents the object with the graphics shown on the canvas.
     * @param ss internal representation of the state of the object
     */
    public void addState(oclSS ss, vShape sp){
	//add to the dynamicState
	statePointer ++;
	dynamicState[statePointer] = ss;

	//register to the stateShapeList
	stateShapeList.put(ss.getName(), sp); //to distiguish by object name
    }


    /**
     * change the status of one state 
     * @param newShape graphical representation of the state
     * @param ss internal representation of the state
     */
    public void refresh(List oclStates, oclSS ss, vShape newShape){
	oclSS newSS = refreshDynamicState(oclStates, ss);
	refreshStateShapeList(newShape, newSS);
    }

    /**
     * change the internal representation of the state.
     * Update the state list with the latest states.
     * @param ss new internal representation of the state
     */
    //Weihong 25/2/02 to update part of the state
    // and keep the higher level states
    private oclSS refreshDynamicState(List oclStates, oclSS ss){
	oclSS returnSS = null;
	for (int i = 0; i< stateSize; i++){
	    if (dynamicState[i].getName().equals(ss.getName())){
		jplan.general.Utility.debugPrintln("-- State before updating --\n" + dynamicState[i].toString());
		dynamicState[i].updateWith(oclStates, ss);
		returnSS = dynamicState[i]; //update ss
		jplan.general.Utility.debugPrintln("-- State after updating --\n" + returnSS.toString());
	    }
	}
	return returnSS;
    }

    /**
     * change the graphical representation of the state.
     * update the state shape list with the latest created objects
     * @param ss new graphical representation of the state
     */
    private void refreshStateShapeList(vShape newShape, oclSS ss){
	try {
	    oclSS theSS = (oclSS)ss.clone();
	    newShape.setObject(theSS);
	    jplan.general.Utility.debugPrintln("newShape"+newShape.getID()+"!");
	}catch (CloneNotSupportedException e){jplan.general.Utility.debugPrintln(e);}
    }


    /**
     * reset the DynamicState to its initial: empty
     */
    public void reset(){
	statePointer = -1;
	for (int i = 0; i< stateSize; i++){
	    dynamicState[i] = null;
	}
	stateShapeList.clear();
    }


    /**
     * get the position of the vshape in the list
     * @param vp given vShape
     * @return the position of the vshape in the list
     */
    private int getPosition(vShape vp){
	for (int i = 0; i< stateSize; i++){
	    if (dynamicState[i].getName() == vp.getLabel()) {
		return i;
	    }
	}
	return 0;
    }

    /**
     * Returns the statelist 
     * @return a list of oclSS
     */
    public List getState(){
	List list = new ArrayList();
	for (int i = 0; i< stateSize; i++){
	    list.add(dynamicState[i]);
	}
	return list;
    }


    /**
     * clone a copy of this DynamicState
     * @return DynamicState
     */
    public Object clone(){
	DynamicState copy = new DynamicState(stateSize, d_Height, lineSpace);
	copy.statePointer = this.statePointer;
	for (int i = 0; i< stateSize; i++){
	    try {
		copy.dynamicState[i] = (oclSS)((oclSS)this.dynamicState[i]).clone(); /* Weihong 25/2/02 */
	    } catch (CloneNotSupportedException e) {}
	}
	copy.stateShapeList = (Hashtable)this.stateShapeList.clone();

	return copy;
    }


    /**
     * find matching shape for the cur operator to link
     * @param objectName the name of the object which is unique
     * @return graphical representation
     */
    public vShape findMatchingShape(String objectName){
	vShape vs = null;
	vs = (vShape)stateShapeList.get(objectName);
	return vs;
    }
}

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
 * HSShapeProperty.java
 *
 *
 * Created: Tue May 21 14:13:52 2002
 *
 * @author W Zhao
 * @version
 */
import jplan.graphics.JGraphCanvas;
import jplan.graphics.gTool.Graphics.vShape;
import java.util.List;/* WZ 22/5/02 */
import java.util.ArrayList;/* WZ 22/5/02 */
import java.util.ListIterator;/* WZ 28/5/02 */
import jplan.ocl.*;/* WZ 28/5/02 */

/**
 * Data store/structure to save shapes' information
 * in the stepper canvas/window.
 */
public class HSShapeProperty  {
    
    private int row, column; //position in a chess board coordination system
    private Object object; //oclSS, oclOperator, oclMethod
    private Object reference; //partial information
    private int parentShapeKey = 0;
    /* WZ 22/5/02 */
    private List decompList = new ArrayList();//the decomposition list
    /* WZ 27/5/02 */
    private List preState = new ArrayList(); //substates before action taken
    private List postState = new ArrayList();//substates after action taken
    private int ownerShapeIndex;/* WZ 18/6/02 */

    public HSShapeProperty() {
	row = 0;
	column = 0;
    }
    
    public HSShapeProperty(int row, int column) {
	this.row = row;
	this.column = column;
    }

    public HSShapeProperty(Object reference, int row, int column) {
	this.reference = reference;
	this.row = row;
	this.column = column;
    }
    
    /* WZ 18/6/02 */
    /**
       * Get the value of shapeIndex.
       * @return Value of shapeIndex.
       */
    public int getOwnerShapeIndex() {return ownerShapeIndex;}

    /* WZ 18/6/02 */
    /**
       * Set the value of shapeIndex.
       * @param v  Value to assign to shapeIndex.
       */
    public void setOwnerShapeIndex(int  v) {this.ownerShapeIndex = v;}
    
    /* WZ 27/5/02 */
    /**
       * Get the value of preState.
       * @return Value of preState.
       */
    public List getPreState() {return preState;}
    
    /* WZ 27/5/02 */
    /**
       * Set the value of preState.
       * @param v  Value to assign to preState.
       */
    public void setPreState(List  v) {
	ListIterator li = v.listIterator();
	while (li.hasNext()){
	    oclSS ss = (oclSS)li.next();
	    try {
		preState.add((oclSS)ss.clone());
	    }catch (CloneNotSupportedException e){jplan.general.Utility.debugPrintln(e);}
	}
    }
    
    /* WZ 20/6/02 */
    /**
       * Add an oclSS to preState.
       * @param ss a object state
       */
    public void addPreState(oclSS ss) {
	try {
	    preState.add((oclSS)ss.clone());
	}catch (CloneNotSupportedException e){
	    jplan.general.Utility.debugPrintln(e);
	}
    }

    /* WZ 20/6/02 */
    /**
       * Add an oclSS to preState.
       * @param ss a object state
       */
    public void addPostState(oclSS ss) {
	try {
	    postState.add((oclSS)ss.clone());
	}catch (CloneNotSupportedException e){
	    jplan.general.Utility.debugPrintln(e);
	}
    }

    /* WZ 27/5/02 */
    /**
       * Get the value of postState.
       * @return Value of postState.
       */
    public List getPostState() {return postState;}
    
    /* WZ 27/5/02 */
    /**
       * Set the value of postState.
       * @param v  Value to assign to postState.
       */
    public void setPostState(List  v) {/* WZ 28/5/02 */
	ListIterator li = v.listIterator();
	while (li.hasNext()){
	    oclSS ss = (oclSS)li.next();
	    try {
		postState.add((oclSS)ss.clone());
	    }catch (CloneNotSupportedException e){jplan.general.Utility.debugPrintln(e);}
	}
    }
    
    /**
       * Get the value of row.
       * @return Value of row.
       */
    public int getRow() {return row;}
    
    /**
       * Set the value of row.
       * @param v  Value to assign to row.
       */
    public void setRow(int  v) {this.row = v;}
    
    /**
       * Get the value of column.
       * @return Value of column.
       */
    public int getColumn() {return column;}
    
    /**
       * Set the value of column.
       * @param v  Value to assign to column.
       */
    public void setColumn(int  v) {this.column = v;}
    
    /**
       * Get the value of object.
       * @return Value of object.
       */
    public Object getObject() {return object;}
    
    /**
       * Set the value of object.
       * @param v  Value to assign to object.
       */
    public void setObject(Object  v) {this.object = v;}

    /**
       * Get the value of reference.
       * @return Value of reference.
       */
    public Object getReference() {return reference;}
    
    /**
       * Set the value of reference.
       * @param v  Value to assign to reference.
       */
    public void setReference(Object  v) {this.reference = v;}

    /**
       * Return true if object is not empty.
       * @return true if object is not empty.
       */
    public boolean hasObject(){
	if (object != null)
	    return true;
	return false;
    }
    /**
       * Get the value of parentShape
       * @return Value of parentShape.
       */
    public vShape getParentShape(JGraphCanvas canvas) {
	if (parentShapeKey == 0)
	    return null;
	else
	    return canvas.getShape(parentShapeKey);
    }
    
    /**
       * Set the value of parentShape.
       * @param vs  Value to assign to reference.
       * @param canvas
       */
    public void setParentShape(vShape vs, JGraphCanvas canvas) {
	parentShapeKey = canvas.getShapeKey(String.valueOf(vs.getID()));
    }

    /**
       * Set the value of parentShapeKey.
       * @param key  Value to assign to parentShapeKey.
       */
    public void setParentShapeKey(int key) {
	parentShapeKey = key;
    }

    /* WZ 22/5/02 */
    /**
       * Add an item to the decompList.
       */
    public void addDecompItem(vShape vs) {
	decompList.add(vs);
    }

    /* WZ 22/5/02 */
    /**
       * Get the value of decompList.
       * @return Value of decompList.
       */
    public List getDecompList() {return decompList;}

    /* WZ 22/5/02 */
    /**
       * Set the value of decompList.
       * @param v  Value to assign to decompList.
       */
    public void setDecompList(List  v) {this.decompList = v;}
    
    /**
     * a string expression, mainly used for saving purpose
     * @return String
     */
    public String to_String() {
// jplan.general.Utility.debugPrintln("\nBEGIN H STEPPER SHAPE PROPERTY...\n");
	StringBuffer str = new StringBuffer();
	str = str.append( "BEGIN H STEPPER SHAPE PROPERTY\n");
	str = str.append( "owner shape ID:"+ownerShapeIndex+"\n");
	str = str.append( "row:"+row+"\n");
	str = str.append( "column:"+column+"\n");
	str = str.append( "parent shape key:"+parentShapeKey+"\n");

	//save reference
// jplan.general.Utility.debugPrintln("\nBEGIN REFERENCE...\n");
	str = str.append( "\nBEGIN REFERENCE\n");
	if (reference.getClass().getName().equals("jplan.ocl.oclSS")){
	    oclSS ss = (oclSS)reference;
	    str = str.append( "reference type:"+"oclSS"+"\n");
	    str = str.append(ss.to_String());
	}
	else if (reference.getClass().getName().equals("jplan.ocl.oclOperator")){
	    oclOperator op = (oclOperator)reference;
	    str = str.append( "reference type:"+"oclOperator"+"\n");
	    str = str.append(op.to_String());
	}
	else if (reference.getClass().getName().equals("jplan.ocl.oclMethod")){
	    oclMethod om = (oclMethod)reference;
	    str = str.append( "reference type:"+"oclMethod"+"\n");
	    str = str.append(om.to_String());
	}
	str = str.append( "\nEND REFERENCE\n");

	//save object
// jplan.general.Utility.debugPrintln("\nBEGIN OBJECT...\n");
	if (object != null){
	    str = str.append( "\nBEGIN OBJECT\n");
	    if (reference.getClass().getName().equals("jplan.ocl.oclSS")){
		oclSS ss = (oclSS)object;
		str = str.append( "object type:"+"oclSS"+"\n");
		str = str.append(ss.to_String());
	    }
	    else if (reference.getClass().getName().equals("jplan.ocl.oclOperator")){
		oclOperator op = (oclOperator)object;
		str = str.append( "object type:"+"oclOperator"+"\n");
		str = str.append(op.to_String());
	    }
	    else if (reference.getClass().getName().equals("jplan.ocl.oclMethod")){
		oclMethod om = (oclMethod)object;
		str = str.append( "object type:"+"oclMethod"+"\n");
		str = str.append(om.to_String());
	    }
	    str = str.append( "\nEND OBJECT\n");
	}

	//save post state
// jplan.general.Utility.debugPrintln("\nEGIN PRE STATE...\n");
	if (preState.size() > 0){
	    str = str.append( "\nBEGIN PRE STATE\n");
	    ListIterator li = preState.listIterator();
	    while (li.hasNext()){
		oclSS ss = (oclSS)li.next();
		str = str.append(ss.to_String()+"\n");
	    }
	    str = str.append( "END PRE STATE\n");
	}

	//save post state
// jplan.general.Utility.debugPrintln("\nEGIN POST STATE...\n");
	if (postState.size() > 0){
	    str = str.append( "\nBEGIN POST STATE\n");
	    ListIterator li = postState.listIterator();
	    while (li.hasNext()){
		oclSS ss = (oclSS)li.next();
		str = str.append(ss.to_String()+"\n");
	    }
	    str = str.append( "END POST STATE\n");
	}

	str = str.append( "END H STEPPER SHAPE PROPERTY\n");

	return str.toString();
    }
} // HSShapeProperty

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

// oclObject - Store object Spec 
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*;

import jplan.general.O2HTML;

/**
 * oclObject - store an OCL Object clause
 * @author Ron Simpson
 * @version 0
 */
public class oclObject implements oclPrint,Serializable{
    private String kind;
    private List  objIDs = new ArrayList();

    /**
     * @constructor
     * @param  name - the Sort/type for these objects
     */
    public oclObject (String name) {
	kind = new String(name);
    }

    /**
     * addObjID - add an object/ID to the current sort
     * @param name - the object ID
     */
    public void addObjID(String name) {
	if (! objIDs.contains(name)) {
	    objIDs.add(new String(name));
	}
    }
    
    /**
     * getObjectSort - get the sort/kind for these objects
     * @return String sort name
     */
    public String getObjectSort () {
	return kind;
    }

    /**
     * setObjectSort - set the sort/kind for these objects
     * @param  name   sort name
     */
    public void setObjectSort (String name) {
	kind = name;
    }
    /**
     * isObject
     * test if given id is the name of an object
     * @param  id
     * @return boolean - true if Id in list
     */
    public boolean isObject(String id) {
	ListIterator li = objIDs.listIterator();
	boolean found = false;
	while (!found && li.hasNext()) {
	    if (id.equals((String)li.next())) {
		found = true;
	    }
	}
	return found;
    }

    /**
     * getObjectNames - get the names of the objects of the current sort
     * @return List the ArrayList with the IDs (String)
     */
    public List getObjectNames() {
	return objIDs;
    }

    /**
     * clone a copy of oclObject
     * @return the oclObject clause 
     * @throws CloneNotSupportedException 
     */
    public Object clone() throws CloneNotSupportedException {
	try{
	    oclObject ret = new oclObject(this.kind);
	    ListIterator li = objIDs.listIterator();
	    while (li.hasNext()) {
	        String id = (String)li.next();
		ret.addObjID(id);
	    }
	    return (oclObject)ret;
	} catch (Exception e){
	    throw new CloneNotSupportedException("Cannot clone oclObject");
	}
    }

    public void oclPrintComponent(PrintWriter ps,int indent,boolean nline) {
	ListIterator li = objIDs.listIterator();
	ps.print("objects(" + kind + ",[");
	if (li.hasNext())
	    ps.print(li.next());
	while(li.hasNext()) {
	    ps.print("," + li.next());
	}
	ps.println("]).");
    }

    public void htmlPrint(PrintWriter ps,int indent,boolean nindent) {
	ListIterator li = objIDs.listIterator();
	ps.print(O2HTML.red("objects(") + kind + O2HTML.red(",["));
	if (li.hasNext())
	    ps.print(li.next());
	while(li.hasNext()) {
	    ps.print(O2HTML.red(",") + li.next());
	}
	ps.println(O2HTML.red("])."));
    }
					  
}

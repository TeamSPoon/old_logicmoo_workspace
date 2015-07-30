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

package jplan.pddl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.PrintWriter;

import jplan.general.Utility;
import jplan.general.OPredicate;

public class pddlTypedVar implements pddlPrint {
    String vName;
    private List  types = new ArrayList();
    
    public pddlTypedVar(String name) {
	vName = name;
    }

    public void addType(String tname) {
	types.add(tname);
    }

    public List getTypeList () {
	return types;
    }

    public String getvName() {
	return vName;
    }

    /**
     * isVar
     * test to see if name represents a variable
     * i.e. starts with '?'
     * @return boolean
     */
    public boolean isVar() {
	return (vName.charAt(0) == '?');
    }

    /**
     * transToOcl
     * just translate the name ignore type info
     * @return String
     */
    public String transToOcl() {
	if (vName.charAt(0) == '?') {
	    String res = vName.substring(1);
	    res = OPredicate.toVar(res);
	    return res;
	} else {
	    String res = OPredicate.toConst(vName);
	    return res;
	}
    }

    /**
     * equals
     * test against given typed var for equality
     * name and types must be identical
     * should not be sensitive to order of types in or types
     * @param given - pddlTypedVar - the variable to test againsa
     * @return boolean
     */
    public boolean equals(pddlTypedVar given) {
	if (vName.equals(given.vName)) {
	    ListIterator li = types.listIterator();
	    boolean ok = true;
	    while (ok && li.hasNext()) {
		if (!Utility.listContainsString((String)li.next(),given.types)) {
		    ok = false;
		}
	    }
	    return ok;
	} else {
	    return false;
	}
    }

    public Object clone() throws CloneNotSupportedException {
	pddlTypedVar temp = new pddlTypedVar(vName);
	ListIterator li = types.listIterator();
	while (li.hasNext()) {
	    temp.addType((String)li.next());
	}
	return temp;
    }

    /**
     * toString
     * @return String
     */
    public String toString() {
	ListIterator li;

	String res = new String(vName);
	if (types.size() > 0) {
	    res = res.concat(" - ");
	    li = types.listIterator();
	    if (types.size() == 1) {
		res = res.concat((String)li.next());
	    } else {
		res = res.concat("(either ");
		while( li.hasNext()) {
		    res = res.concat((String)li.next());
		    if (li.hasNext()) {
			res = res.concat(" ");
		    }
		}
		res = res.concat(") ");
	    }
	}
	return res;
    }	

    public void pddlPrintComponent(PrintWriter ps,int indent,boolean nline) {
	ListIterator li;

	ps.print(vName);
	if (types.size() > 0) {
	    ps.print(" - ");
	    li = types.listIterator();
	    if (types.size() == 1) {
		ps.print((String)li.next());
	    } else {
		ps.print("(either ");
		while( li.hasNext()) {
		    ps.print((String)li.next());
		    if (li.hasNext()) {
			ps.print(" ");
		    }
		}
		ps.print(") ");
	    }
	}
	if (nline) 
	    ps.println("");
    }
				      
			 
}

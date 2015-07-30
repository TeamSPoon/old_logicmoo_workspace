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

/**
 * oclSort - Store sort details
 */
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*;

import jplan.general.O2HTML;
/**
 * oclSort - store an OCL Sort clause
 * @author Ron Simpson
 * @version 0
 */
public class oclSort implements oclPrint, Serializable {
	private String kind;
	private List subTypes = new ArrayList();
	private oclDescriptions descriptions = null;

	/**
	 * @param desc The text description object
	 * @param name the Sort/type for these sub types
	 */
	public oclSort(oclDescriptions desc, String name) {
		kind = new String(name);
		descriptions = desc;
	}

	/**
	 * add sub type
	 * @param name of subtype to add
	 */
	public void addSubType(String name) {
		subTypes.add(new String(name));
	}

	/**
	 * get the current sort name
	 * @return the name String
	 */
	public String getSortName() {
		return kind;
	}

	/**
	 * set the current sort name
	 * @param name - the new name
	 */
	public void setSortName(String name) {
		kind = name;
	}

	/**
	 * get the sub types of the current sort
	 * @return List the sub types list
	 */
	public List getSubTypes() {
		return subTypes;
	}

	/**
	 * get the parent sort name for currents sorts
	 * @return parent sort name (String)
	 */
	public String getSortParent() {
		return kind;
	}

	/**
	 * find if sort is a sub-type of the current kind
	 * @param  name String
	 * @return boolean
	 */
	public boolean isSubType(String name) {
		ListIterator li = subTypes.listIterator();
		while (li.hasNext()) {
			if (name.equals((String) li.next())) {
				return true;
			}
		}
		return false;
	}


	/**
	* retrieve the text description for the sort
	* @return String - the description
	*/
	public String getMyDescription() {
		return descriptions.getDescription("sort" + kind);
	}

	/**
	 * store the description for this sort
	 * @param desc - the text description
	 */
	public void setMyDescription(String desc) {
		descriptions.addDescription("sort" + kind, desc);
	}

	/**
	 * retrieve the text description for the sort
	 * @param name the sort name
	 * @return the description
	 */
	public String getSortDescription(String name) {
		return descriptions.getDescription("sort" + name);
	}

	/**
	 * store the description for this sort
	 * @param desc - the text description
	 */
	public void setSortDescription(String desc) {
		descriptions.addDescription("sort" + kind, desc);
	}

	// Ron 19/3/02 assed for OpMaker
	/**
	 * clone a copy of oclSort
	 * @return the oclSort clause 
	 * @throws CloneNotSupportedException 
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclSort ret = new oclSort(null, this.kind);
			ListIterator li = subTypes.listIterator();
			while (li.hasNext()) {
				String id = (String) li.next();
				ret.addSubType(id);
			}
			return (oclSort) ret;
		} catch (Exception e) {
			throw new CloneNotSupportedException("Cannot clone oclSort");
		}
	}

	/**
	 * implements basic print-out to stream of sort clause
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nindent not being used really
	 * 
	 */
	public void oclPrintComponent(
		PrintWriter ps,
		int indent,
		boolean nindent) {
		ListIterator li = subTypes.listIterator();
		ps.print("sorts(" + kind + ",[");
		if (li.hasNext())
			ps.print(li.next());
		while (li.hasNext()) {
			ps.print("," + li.next());
		}
		ps.println("]).");
	}


	/**
	 * to print the current oclSort to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nindent not being used really
	 * 
	 */
	public void htmlPrint(PrintWriter ps, int indent, boolean nindent) {
		ListIterator li = subTypes.listIterator();
		ps.print("<PRE>" + O2HTML.red("sorts(") + kind + O2HTML.red(",["));
		if (li.hasNext())
			ps.print(li.next());
		while (li.hasNext()) {
			ps.print(O2HTML.red(",") + li.next());
		}
		ps.println(O2HTML.red("]).</PRE>"));
	}

}

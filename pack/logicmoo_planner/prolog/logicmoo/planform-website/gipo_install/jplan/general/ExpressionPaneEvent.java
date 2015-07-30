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

package jplan.general;

/**
 * ExpressionPaneEvent
 * Event to pass to listining objects
 * @author Ron Simpson
 * @version 0
 */

public class ExpressionPaneEvent extends java.util.EventObject {
	protected int id;
	protected String vName;
	protected String vSort;
	protected String oldVName;
	protected String transObjID;
	protected int scope;

	public static final int SELECTION = 0;
	public static final int CLEAR = 1;
	public static final int RENAME = 2;
	public static final int QUERYID = 5; // Ron 10/10/02 
	// Query use of variable name to prevent transition IDs conflicting
	// Constant for exporting/deleting variables when editing patterns
	public static final int EXPORT = 3;
	public static final int DELETE = 4;

	//constants for the dirrerent type of renaming event (scope)
	public static final int TRANSITION = 10;
	public static final int GLOBAL = 11;

	public ExpressionPaneEvent(Object source, int id, String vName) {
		super(source);
		this.id = id;
		this.vName = vName;
		this.oldVName = new String("none");
		this.transObjID = new String("none");
		this.vSort = new String("none");
		this.scope = -1;
	}

	public ExpressionPaneEvent(
		Object source,
		int id,
		String vName,
		String vSort) {
		super(source);
		this.id = id;
		this.vName = vName;
		this.oldVName = new String("none");
		this.transObjID = new String("none");
		this.vSort = vSort;
		this.scope = -1;
	}
	public ExpressionPaneEvent(Object source, int id) {
		super(source);
		this.id = id;
		this.vName = new String("none");
		this.oldVName = new String("none");
		this.transObjID = new String("none");
		this.vSort = new String("none");
		this.scope = -1;
	}

	public ExpressionPaneEvent(
		Object source,
		int id,
		String vName,
		String oldVName,
		String transObjID,
		int scope) {
		super(source);
		this.id = id;
		this.vName = vName;
		this.oldVName = oldVName;
		this.transObjID = transObjID;
		this.scope = scope;
		this.vSort = new String("none");
	}

	public int getID() {
		return id;
	}
	public String getVName() {
		return vName;
	}
	public String getOldVName() {
		return oldVName;
	}
	public String getTransObjID() {
		return transObjID;
	}
	public int getScope() {
		return scope;
	}
	public String getVSort() {
		return vSort;
	}
}

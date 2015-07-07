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

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;

/** OEnviron This class provides Environment bindings for Variables
 * produced as part of the unification process.
 */
public class OEnviron implements Cloneable {
	private List bindings = new ArrayList();

	/** Test if environment has no bindings.
	 * @return boolean
	 */
	public boolean isEmpty() {
		return (bindings.size() == 0);
	}

	/** Add a Variable and its Value to the Environment
	 * @param  variable name as a String
	 * @param value The variable's value as a String
	 */
	public void addBinding(String variable, String value) {
		bindings.add(new bind(variable, value));
	}

	/** Test that a variable with its binding is compatable with the current
	 * environment. It is compatable if either there is no current entry
	 * for the variable in the environment. Or there is an entry with the
	 * same constant value. Or the value is itself a variable it either has
	 * no value in the environment or has the same value
	 * @param vName - variable name as a String
	 * @param vVal - The variable's value as a String
	 * @return boolean indicating compatibility
	 */

	public boolean match(String vName, String vVal) {
		if (vName.equals(vVal)) {
			return true;
		} else {
			String bindVal = getVarVal(vName);
			if (vName.equals(bindVal)) { // Variable not set
				addBinding(vName, vVal);
				return true;
			}
			if (isVar(bindVal)) { // end of variable chain
				addBinding(bindVal, vVal);
				return true;
			} else if (bindVal.equals(vVal)) { // already bound to this value
				return true;
			} else {
				return false;
			}
		}
	}

	/** Fetch the current value of a variable. This is a recursive search
	 * either to find a constant value which may be the value of a variable 
	 * which at some remove is set as the value of the given variable.
	 * Or if there is no value for the variable an empty string is returned.
	 * @param vName - The name of the variable
	 * @return The value of the variable as a String 
	 * - The variable name itself if there is no value
	 * - The last variable if there is a variable chain.
	 */
	public String getVarVal(String vName) {
		bind cur;
		// 	Utility.debugPrintln("getVarVal " + vName);
		ListIterator li = bindings.listIterator();
		while (li.hasNext()) {
			cur = (bind) li.next();
			if (vName.equals(cur.var)) {
				if (!isVar(cur.val)) {
					// 		    Utility.debugPrintln("  >>  getVarVal val " + cur.val);
					return cur.val;
				} else {
					// 		    Utility.debugPrintln("  >>  getVarVal val " + cur.val);
					return cur.val;
					//return getVarVal(cur.val);
				}
			}
		}
		return vName;
	}

	/**
	 * clone
	 */
	public Object clone() throws CloneNotSupportedException {
		OEnviron ret = new OEnviron();
		try {
			ListIterator li = bindings.listIterator();
			while (li.hasNext()) {
				bind cur = (bind) li.next();
				ret.addBinding(new String(cur.var), new String(cur.val));
			}
		} catch (Exception e) {
			throw new CloneNotSupportedException(e.toString());
		}
		return ret;
	}
	
	// Ron 8/4/03
	/**
	 * copy an existing environment
	 * @param other environment to copy
	 */
	public void copy(OEnviron other) {
		ListIterator li = other.bindings.listIterator();
		while (li.hasNext()) {
			bind cur = (bind) li.next();
			addBinding(new String(cur.var), new String(cur.val));
		}
	}

	/** Test a String to determine if it is a OCL variable
	 * @param name as a String
	 * @return boolean
	 */
	public static boolean isVar(String name) {
		return Character.isUpperCase(name.charAt(0));
	}

	protected class bind {
		public String var;
		public String val;

		public bind(String v1, String v2) {
			var = v1;
			val = v2;
		}
	}
}

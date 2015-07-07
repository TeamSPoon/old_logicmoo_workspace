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

import jplan.ocl.*;

/**
 * Utility.java
 * @author Ron
 * Contains general utility methods - all static
 */

public class Utility {

	public Utility() {
	};
	
	/**
	 * addAllNew
	 * adds new strings in newItems to list oldItems
	 * @param newItems - list of strings - the things to add
	 * @param oldItems - list of strings the org list
	 */
	public static List addAllNew(List newItems, List oldItems) {
		ListIterator li = newItems.listIterator();
		while (li.hasNext()) {
			String val = (String)li.next();
			if (!listContainsString(val,oldItems)) {
				oldItems.add(val);
			}
		}
		return oldItems;
	}

	/**
	 * listContainsString
	 * check a list of strings to see if it contains the given string
	 * @param String - the given string
	 * @param List - the list to check
	 * @return boolean - true if list contains string
	 */
	public static boolean listContainsString(String str, List lst) {
		ListIterator li = lst.listIterator();
		boolean found = false;

		while (!found && li.hasNext()) {
			if (str.equals((String) li.next())) {
				found = true;
			}
		}
		return found;
	}
	
	/**
	 * isLegalIdentifier
	 * check rules for prolog names
	 * @param name
	 * @return
	 */
	public static boolean isLegalIdentifier(String name){
		char n[] = name.toCharArray();
		if (! Character.isLowerCase(n[0])) {
			return false;
		}
		for(int i = 1 ; i < n.length; i++) {
			if (!Character.isLetterOrDigit(n[i]) && n[i] != '_') {
				return false;
			}
		}
		return true;
	}

	/**
	 * this is a debugging utility
	 */
	public static void printList(List l, String msg) {
		Utility.debugPrintln(msg);
		ListIterator li = l.listIterator();
		while (li.hasNext()) {
			Utility.debugPrint("    : ");
			Utility.debugPrintln((String) li.next());
		}
	}

	/**
	 * getListDifference
	 * finds all elements of the first list that are not elements of the
	 * second list
	 * @param  first list of Strings
	 * @param  second list of strings
	 * @return List 
	 */
	private static List getListDifference(List first, List second) {
		List diff = new ArrayList();
		ListIterator liFirst = first.listIterator();
		while (liFirst.hasNext()) {
			Object cur = liFirst.next();
			if (!second.contains(cur)) {
				diff.add(cur);
			}
		}
		return diff;
	}

	/**
	 * sameSet
	 * test to see if two lists contain the same objects
	 * @param  first list
	 * @param  second list
	 * @return boolean - true if same
	 */
	private static boolean sameSet(List first, List second) {
		return (
			(getListDifference(first, second).size() == 0)
				&& (getListDifference(first, second).size() == 0));
	}

	/**
	 * checkListUnique
	 * check that all strings - names are unique within the list
	 * adds messages to the mssg list to state which strings are duplicated 
	 * @param toCheck - the list of strings to check
	 * @param mssgs - the message list to add error mesages to
	 * @param prefix - prefix to add to error messages
	 */
	public static void checkListUnique(
		List toCheck,
		List mssgs,
		String prefix) {
		List dup = new ArrayList(toCheck.size());

		ListIterator li = toCheck.listIterator();
		while (li.hasNext()) {
			Object o = li.next();
			if (dup.contains(o)) {
				if (prefix.equals(""))
					mssgs.add((String) o);
				else
					mssgs.add(prefix + (String) o + ".");
			} else {
				dup.add(o);
			}
		}
	}

	/**
	 * addInOrder
	 * add string to a sorted list of strings
	 * @param soFar - the list of strings so far
	 * @param given - the string to add
	 * @return List the sorted list
	 */
	public static List addInOrder(List soFar, String given) {
		List ret = new ArrayList();
		if (soFar.size() == 0) {
			ret.add(given);
		} else {
			ListIterator li = soFar.listIterator();
			boolean added = false;
			while (li.hasNext()) {
				String cur = (String) li.next();
				if (!added && cur.compareTo(given) > 0) {
					ret.add(given);
					added = true;
				}
				ret.add(cur);
			}
			if (!added) {
				ret.add(given);
			}
		}
		return ret;
	}

	/**
	 * addNewInOrder
	 * add string to a sorted list of strings
	 * silently ignore duplicates
	 * @param lst - the list of strings to add to
	 * @param given - the string to add
	 */
	public static void addNewInOrder(List lst, String given) {
		if (lst.size() == 0) {
			lst.add(given);
		} else {
			int size = lst.size();
			int inx = 0;
			boolean added = false;
			while (!added && inx < size) {
				String cur = (String) lst.get(inx);
				int comp = cur.compareTo(given);
				if (!added && comp > 0) {
					lst.add(inx,given);
					added = true;
				} else if (comp == 0) {
					added = true; // duplicate ignore
				}
				inx++;
			}
			if (!added) {
				lst.add(size,given);
			}
		}
	}

	/**
	 * memberPred
	 * check to see if a predicate is a member of a list of preds
	 * just check predicate names
	 * @param pred - the predicate
	 * @param lst - list of predicates (in oclStateList)
	 * @return true if found
	 */
	public static boolean memberPred(oclPredicate pred, oclStateList lst) {
		boolean found = false;
		ListIterator li = lst.getPredicateList().listIterator();
		while (!found && li.hasNext()) {
			oclPredicate target = (oclPredicate) li.next();
			if (pred.getName().equals(target.getName())) {
				found = true;
			}
		}
		return found;
	}

	/**
	 * memberPred
	 * check to see if a predicate is a member of a list of preds
	 * just check predicate names
	 * @param pred - the predicate
	 * @param lst - list of predicates
	 * @return true if found
	 */
	public static boolean memberPred(oclPredicate pred, List lst) {
		boolean found = false;
		ListIterator li = lst.listIterator();
		while (!found && li.hasNext()) {
			oclPredicate target = (oclPredicate) li.next();
			if (pred.getName().equals(target.getName())) {
				found = true;
			}
		}
		return found;
	}

	/**
	* memberPredEqual
	* check to see if a predicate is a member of a list of preds
	* use equal to compare
	* @param pred - the predicate
	* @param lst - list of predicates (in oclStateList)
	* @return true if found
	*/
	public static boolean memberPredEqual(
		oclPredicate pred,
		oclStateList lst) {
		boolean found = false;
		ListIterator li = lst.getPredicateList().listIterator();
		while (!found && li.hasNext()) {
			oclPredicate target = (oclPredicate) li.next();
			if (pred.equals(target)) {
				found = true;
			}
		}
		return found;
	}

	/**
	* memberPredEqual
	* check to see if a predicate is a member of a list of preds
	* compare with equal
	* @param pred - the predicate
	* @param lst - list of predicates
	* @return true if found
	*/
	public static boolean memberPredEqual(oclPredicate pred, List lst) {
		boolean found = false;
		ListIterator li = lst.listIterator();
		while (!found && li.hasNext()) {
			oclPredicate target = (oclPredicate) li.next();
			if (pred.equals(target)) {
				found = true;
			}
		}
		return found;
	}

	/**
	 * isDebugEnabled
	 * see if system propert debug.<category> is set
	 * i.e. -Ddebug.<category>
	 * @param cat - category
	 * @return boolean
	 */
	public static boolean isDebugEnabled(String cat) {
		return System.getProperty("debug." + cat) != null;
	}

	/**
	 * debugPrintln
	 * print the message
	 * @param  mssg - the Message
	 */
	public static void debugPrintln(String mssg) {
		if (isDebugEnabled("all")) {
			System.out.println(mssg);
		}
	}
	/**
	 * debugPrintln
	 * print the message
	 * @param cat - category
	 * @param  mssg - the Message
	 */
	public static void debugPrintln(String cat, String mssg) {
		if (isDebugEnabled(cat)) {
			System.out.println(cat + ": " + mssg);
		}
	}

	/**
	 * debugPrintln
	 * print the message
	 * @param mssg - print toString
	 */
	public static void debugPrintln(Object mssg) {
		if (isDebugEnabled("all")) {
			System.out.println(mssg.toString());
		}
	}
	/**
	 * debugPrintln
	 * print the message
	 * @param cat -  category
	 * @param mssg uses toString
	 */
	public static void debugPrintln(String cat, Object mssg) {
		if (isDebugEnabled(cat)) {
			System.out.println(cat + ": " + mssg.toString());
		}
	}

	/**
	 * debugPrint
	 * print the message
	 * @param mssg - print toString
	 */
	public static void debugPrint(Object mssg) {
		if (isDebugEnabled("all")) {
			System.out.print(mssg.toString());
		}
	}
	/**
	 * debugPrint
	 * print the message
	 * @param cat - category
	 * @param mssg - uses toString
	 */
	public static void debugPrint(String cat, Object mssg) {
		if (isDebugEnabled(cat)) {
			System.out.print(cat + ": " + mssg.toString());
		}
	}

}

/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 *
 * Permision to use, copy, modify, and distribute this software and its
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
 * The ocl package contains all ocl specification elements.
 */
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.TreeSet;
import java.util.NoSuchElementException;
import java.io.*;
import jplan.top.OclEd;
import jplan.general.*;

/*
 * History
 * Ron 8/11/02 - Changed checkAtomic() Added side effect: to add sorts to atomic invariants arguments
 */	

/**
 * @brief Stores a domain description in an ocl structure.
 *
 * oclDomains : Top level class to store the representation of
 * an OCL(h) domain. Corresponds to OCL Version 1.2
 * All domain specification elements are accessible from this class
 * @author Ron Simpson
 */
public class oclDomain
	implements oclPrint, Serializable { 
	private String author;
	private String institution;
	private String dateCreated;
	private String dateModified;
	public List domDesc;

	private String name;
	public List sorts = new ArrayList();
	public List objects = new ArrayList();
	public List predicates = new ArrayList();
	public List classDefs = new ArrayList();
	public List atomicInvars = new ArrayList();
	public List impliedInvars = new ArrayList();
	public List inconsistentConst = new ArrayList();
	public List operators = new ArrayList();
	public List tasks = new ArrayList();
	public List methods = new ArrayList();
	public List patterns = new ArrayList();
	public List htntasks = new ArrayList(); //Ron 10/4/02
	public List functors = new ArrayList(); // Ron 25/06/03 For oclPlus
	public List processes = new ArrayList(); // Ron 1/9/03
	public List events = new ArrayList(); // Ron 1/9/03
	
	public String lifeHistoryFileName = "none"; //Ron 12/12/04

	private OclEd ocledEnv = null; //reference to ocled top
	private boolean hierarchical = false;
	public boolean rhsWarnings = true; // Ron 12/11/02 allow to be switched off

	public boolean oclPlus = false; // Ron 25/06/03 for oclPlus
	
	/**
	 * OCLSortImpliedRoot - a name for the sort tree root
	 */
	public static final String OCLSortImpliedRoot = "AllSorts";

	private oclDescriptions descriptions; // The ocl Description object

	/**
	 * Basic constructor - passing top environment
	 */
	public oclDomain(OclEd top) {
		name = new String("oclDefault");
		descriptions = new oclDescriptions();
		author = "none";
		domDesc = new ArrayList();
		ocledEnv = top;
		hierarchical = top.hierarchicalSwitch;
		rhsWarnings = top.rhsWarnings;
		//oclPlus = top.oclplus;   //Ron 25/06/03 
	}

	/**
	 * Basic constructor - passing flag to indicate if hierarchical domain
	 */
	public oclDomain(boolean hier) {
		name = new String("oclDefault");
		descriptions = new oclDescriptions();
		author = "none";
		domDesc = new ArrayList();
		ocledEnv = null;
		hierarchical = hier;
		String str = new String(System.getProperty("ocled.rhswarnings"));
		if (str.equals("no"))
	    	rhsWarnings = false;
		else
			rhsWarnings = true;
//		str = System.getProperty("ocled.oclplus"); // Ron 25/06/03
//		if (str != null && str.equals("yes")) 
//			oclPlus = true;
//		else
//			oclPlus = false; 
	}
	
	/**
	 * addOption
	 * Utility to keep track of GIPO options specific to this domain.
	 * Stored in prolog option clauses
	 * e.g. option(hierarchical).
	 * @param opt - option name
	 */
	public void addOption(String opt){
		// Do nothing for the moment
		Utility.debugPrintln("FOUND OPTION " + opt);
		if (opt.equals("hierarchical")) {
			hierarchical = true;
		} else {
			hierarchical = false;
		}
		if (opt.equals("oclPlus")) {
			oclPlus = true;
		} else {
			oclPlus = false;
		}
	}


	/**
	 * setEnvironment
	 * set the reference to the ocled environment
	 * @param top - the top environment
	 */
	public void setEnvironment(OclEd top) {
		ocledEnv = top;
	}

	/**
	 * Get the value of institution.
	 * @return value of institution.
	 */
	public String getInstitution() {
		return institution;
	}

	/**
	 * Set the value of institution.
	 * @param v  Value to assign to institution.
	 */
	public void setInstitution(String v) {
		this.institution = v;
	}

	/**
	   * Get the value of author.
	   * @return value of author.
	   */
	public String getAuthor() {
		return author;
	}

	/**
	   * Set the value of author.
	   * @param v  Value to assign to author.
	   */
	public void setAuthor(String v) {
		this.author = v;
	}

	/**
	   * Get the value of dateCreated.
	   * @return value of dateCreated.
	   */
	public String getDateCreated() {
		return dateCreated;
	}

	/**
	   * Set the value of dateCreated.
	   * @param v  Value to assign to dateCreated.
	   */
	public void setDateCreated(String v) {
		this.dateCreated = v;
	}

	/**
	   * Get the value of dateModified.
	   * @return value of dateModified.
	   */
	public String getDateModified() {
		return dateModified;
	}

	/**
	   * Set the value of dateModified.
	   * @param v  Value to assign to dateModified.
	   */
	public void setDateModified(String v) {
		this.dateModified = v;
	}

	/**
	 * addDomDescLine
	 * add a line of textual description of the domain
	 * @param line descriptive line
	 */
	public void addDomDescLine(String line) {
		domDesc.add(new String(line));
	}

	/* Weihong added on 17/10/2001 */
	/**
	 * clearDomDesc
	 */
	public void clearDomDesc() {
		domDesc.clear();
	}

	/* Weihong added on 17/10/2001 */
	/**
	 * getDomDesc
	 */
	public String getDomDesc() {
		StringBuffer str = new StringBuffer();
		ListIterator li = domDesc.listIterator();
		while (li.hasNext()) {
			String s = (String) li.next();
			str.append(s + "\n");
			// 	    Utility.debugPrintln(s);
		}
		return str.toString();
	}
	
	/**
	 * setLifeHistoryFile
	 * store name of life history graphics file
	 * @param fName
	 */
	public void setLifeHistoryFile(String fName) {
		lifeHistoryFileName = fName;
	}

	/**
	 * setName - give the domain a name 
	 * Use file name if no name provided
	 *@param n the domain name
	 */
	public void setName(String n) {
		name = new String(n);
	}

	/**
	 * @return the domain name as a String
	 */
	public String getName() {
		return name;
	}

	/**
	 * isHierarchical
	 * determine if domain is a hierarchical domain
	 * @return - true if Hierarchical
	 */
	public boolean isHierarchical() {
//		if (ocledEnv != null) {
//			Utility.debugPrintln("The environment is SET");
//			return ocledEnv.hierarchicalSwitch;
//		} else
//			return hierarchical;
		// Ron 5/5/03 - just look at the domain hierarchical switch
		return hierarchical;
	}

	/**
	 * setHierarchical
	 * Record this domain as hierarchical or flat
	 * @param h - true if hierarchical
	 */
	public void setHierarchical(boolean h){
		hierarchical = h;
	}
	
	/**
	 * isOclPlus
	 * determine if domain is a OclPlus domain
	 * @return - true if oclplus
	 */
	public boolean isOclPlus() {
		return oclPlus;
	}

	/**
	 * setOclPlus
	 * Record this domain as hierarchical or flat
	 * @param h - true if hierarchical
	 */
	public void setOclPlus(boolean o){
		oclPlus = o;
	}
	
	/**
	 * oclSort - add a new identifier as a sort.
	 * @param kind sort name
	 */
	public oclSort addSort(String kind) {
		oclSort cur = new oclSort(descriptions, kind);
		sorts.add(cur);
		return cur;
	}
	/**
	 * oclObject - add an object to the domain
	 * @param kind the sort name of the object
	 * @return reference to the object clause
	 */
	public oclObject addObject(String kind) {
		oclObject cur = new oclObject(kind);
		objects.add(cur);
		return cur;
	}
	

	/**
	 * addPredicate
	 * add a new predicate to the predicate list
	 * @param functorName - the predicate / functor name
	 * @return - oclPredicate - reference to the new predicate
	 */
	public oclPredicate addPredicate(String functorName) {
		oclPredicate cur = new oclPredicate(functorName);
		predicates.add(cur);
		return cur;
	}

	/**
	 * addCompletePredicate
	 * add a new predicate to the predicate list
	 * @param pred - the predicate 
	 */
	// Ron added check to see if not present 9/2/02
	public void addCompletePredicate(oclPredicate pred) {
		ListIterator li = predicates.listIterator();
		boolean found = false;
		String predStr = pred.toString();
		while (!found && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (predStr.equals(cur.toString())) {
				found = true;
			}
		}
		if (!found) {
			predicates.add(pred);
		}
	}
	
	/**
	 * addFunctor
	 * add a new functor to the functor list
	 * @param String - the predicate / functor name
	 * @return - oclPredicate - reference to the new predicate
	 */
	public oclPredicate addFunctor(String functorName) {
		oclFunctor cur = new oclFunctor(functorName);
		cur.setFluent(true);
		functors.add(cur);
		return (oclPredicate)cur;
	}


	public oclPredicate addAtomicInvar(String functorName) {
		oclPredicate cur = new oclPredicate(functorName);
		atomicInvars.add(cur);
		return cur;
	}
	
	public oclPredicate addAtomicFunInvar(String functorName) {
		oclFunctor cur = new oclFunctor(functorName);
		atomicInvars.add(cur);
		return (oclPredicate)cur;
	}

	public oclSSClassDef addClassDef(String type, String Id) {
		oclSSClassDef cur = new oclSSClassDef(type, Id);
		classDefs.add(cur);
		return cur;
	}

	public oclInconsistentConst addInconsistentConst() {
		oclInconsistentConst cur = new oclInconsistentConst();
		inconsistentConst.add(cur);
		return cur;
	}

	public oclImpliedInvar addImpliedInvar() {
		oclImpliedInvar cur = new oclImpliedInvar();
		impliedInvars.add(cur);
		return cur;
	}

	public oclOperator addOP() {
		oclOperator cur = new oclOperator();
		operators.add(cur);
		return cur;
	}
	
	// Ron 1/9/03
	/**
	 * addProc
	 * Add an ocl plus process to the model
	 * @return the reference to an empty process
	 */
	public oclProcess addProc() {
		oclProcess cur = new oclProcess();
		processes.add(cur);
		return cur;
	}

	// Ron 1/9/03
	/**
	 * addEvent
	 * Add an ocl plus event to the model
	 * @return the reference to an empty event
	 */
	public oclEvent addEvent() {
		oclEvent cur = new oclEvent();
		events.add(cur);
		return cur;
	}
	
	/**
	 * addCompleteEvent
	 * Add an ocl plus event to the model
	 * @return the reference to an empty event
	 */
	public void addCompleteEvent(oclEvent evt) {
		events.add(evt);
	}
	
	/**
	 * removeOperator
	 * remove a operator from the operator list
	 * @param op - must be a reference to operator in the operators list
	 */
	public void removeOperator(oclOperator op) {
		operators.remove(op);
	}

	public oclMethod addMethod() {
		oclMethod cur = new oclMethod();
		methods.add(cur);
		return cur;
	}

	public oclTask addTask(String ID) {
		oclTask cur = new oclTask(ID);
		tasks.add(cur);
		return cur;
	}

	public oclHTNTask addHTNTask(String ID) {
		oclHTNTask cur = new oclHTNTask(ID);
		htntasks.add(cur);
		return cur;
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * getSortOfObject - finds the sort for given object names for
	 * @param objName - Object name
	 * @return String - the sort name (or null if not found)
	 */
	public String getSortOfObject(String objName) {

		oclObject curObj = null;
		String objID = null;

		ListIterator li = objects.listIterator();
		while (li.hasNext()) {
			curObj = (oclObject) li.next();
			ListIterator lii = curObj.getObjectNames().listIterator();
			while (lii.hasNext()) {
				objID = (String) lii.next();
				if (objID.equals(objName)) {
					return curObj.getObjectSort();
				}
			}
		}
		return null;
	}

	/**
	 * getObjectsOfSort - finds the object names for a given sort
	 * @param sortName - Sort name
	 * @return List - the list of objects of this sort or null
	 */
	public List getObjectsOfSort(String sortName) {
		oclObject cur;
		ListIterator li = objects.listIterator();
		while (li.hasNext()) {
			cur = (oclObject) li.next();
			if (sortName.equals(cur.getObjectSort())) {
				return cur.getObjectNames();
			}
		}
		return null;
	}

	/**
	 * return a list of oclSSClassDef
	 * @return a list of oclSSClassDef
	 */
	public List copyClasDEFList(List classDEFList) {
		List returnList = new ArrayList();
		ListIterator li = classDEFList.listIterator();
		while (li.hasNext()) {
			try {
				oclSSClassDef cur =
					(oclSSClassDef) ((oclSSClassDef) li.next()).clone();
				returnList.add(cur);
			} catch (CloneNotSupportedException e) {
			}
		}
		return returnList;
	}

	/* WZ 20/3/02 */
	/**
	 * getAllStates including inheriated states
	 * return a list of oclSSClassDef 
	 * @return a list of oclSSClassDef
	 */
	public List getAllStates() {
		List copyCDF = copyClasDEFList(classDefs);
		List sortList = getSortRoots();
		ListIterator li = sortList.listIterator();
		while (li.hasNext()) {
			String tmpSort = (li.next()).toString();
			populateSorts(tmpSort);
		}
		List returnList = copyClasDEFList(classDefs);
		classDefs = copyClasDEFList(copyCDF);
		return returnList;
	}

	/* WZ 20/3/02 */
	/**
	 * merge higher level states to the lower level in the same sort tree
	 * 
	 */
	public void populateSorts(String tmpSort) {
		Utility.debugPrintln("looking at " + tmpSort);
		try {
			oclSSClassDef cur = getStateListForSort(tmpSort);
			//for every statelist
			ListIterator liDEF = cur.getStateList().listIterator();
			while (liDEF.hasNext()) {
				Utility.debugPrintln("to merge ... ");
				mergeSort(tmpSort, (oclStateList) liDEF.next());
			}
		} catch (OCLNoSuchElementException e) {
			Utility.debugPrintln(e);
		}

		//for the sub level
		List sortList = getSortSubTypes(tmpSort);
		if (sortList == null)
			return;
		ListIterator li = sortList.listIterator();
		while (li.hasNext()) {
			String tmpSortName = (li.next()).toString();
			populateSorts(tmpSortName);
		}
	}

	/* WZ 20/3/02 */
	/**
	 * merge higher level states to the lower level in the same sort tree
	 * 
	 */
	public void mergeSort(String sortName, oclStateList stateList) {
		List sortList = getSortSubTypes(sortName);
		if (sortList == null)
			return;
		ListIterator li = sortList.listIterator();
		while (li.hasNext()) {
			String tmpSortName = (li.next()).toString();
			try {
				oclSSClassDef cur = getStateListForSort(tmpSortName);
				//for every sort's sub states
				ListIterator liDEF = cur.getStateList().listIterator();
				while (liDEF.hasNext()) {
					oclStateList baseList = (oclStateList) liDEF.next();
					//to add new predicates
					ListIterator liGiven =
						stateList.getPredicateList().listIterator();
					while (liGiven.hasNext()) {
						baseList.addPredicate((oclPredicate) liGiven.next());
						Utility.debugPrintln(
							"sort ["
								+ tmpSortName
								+ "] add predicate ["
								+ baseList.toString()
								+ "]");
					}
				}
			} catch (OCLNoSuchElementException e) {
				Utility.debugPrintln(e);
			}

			//move on to the subtypes (next level)
			//recursion
			mergeSort(tmpSortName, stateList);
		}
	}

	/* WZ 22/3/02 */
	/**
	 * getAllIntegClassDEF 
	 * return a list of all current states and inheriated states of a choosen sort 
	 * @return oclSSClassDef
	 */
	public List getAllIntegClassDEF() {
		List returnList = new ArrayList();
		ListIterator li = createSortList("").listIterator();
		while (li.hasNext()) {
			String cur = li.next().toString();
			oclSSClassDef curDef = getIntegClassDEF(cur);
			if (!curDef.isEmpty()) {
				returnList.add(curDef);
			}
		}
		return returnList;
	}

	/* WZ 22/3/02 */
	/**
	 * getIntegClassDEF 
	 * return a list of all current states and inheriated states of a choosen sort 
	 * @param sortName the choosen object name String
	 * @return oclSSClassDef
	 */
	public oclSSClassDef getIntegClassDEF(String sortName) {
		oclSSClassDef cur;
		try {
			oclSSClassDef curDEF = getStateListForSort(sortName);
			try {
				cur = (oclSSClassDef) curDEF.clone();
			} catch (CloneNotSupportedException e) { /* WZ 20/8/02 */
				cur = new oclSSClassDef(sortName, OPredicate.toVar(sortName));
			}

		} catch (OCLNoSuchElementException e) {
			cur = new oclSSClassDef(sortName, OPredicate.toVar(sortName));
		}

		try {
			String tmpSort = findSortParent(sortName);
			oclSSClassDef tmpDef = (oclSSClassDef) getIntegClassDEF(tmpSort);
			/* WZ 20/8/02 */
			tmpDef.replaceID(cur.getStateSortId());
			tmpDef.replaceSort(cur.getStateSort());
			/* end 20/8/02 */
			try {
				return mergeStates((oclSSClassDef) cur.clone(), tmpDef);
			} catch (CloneNotSupportedException e) {
				return null;
			}
		} catch (NoSuchElementException e) {
			return cur;
		}
	}

	/* WZ 22/3/02 */
	/**
	 * mergeStates merge two oclSSClassDefs together and
	 * return a new oclSSClassDef
	 * @param def1 oclSSClassDef
	 * @param def2 oclSSClassDef
	 * @return oclSSClassDef
	 */
	public oclSSClassDef mergeStates(oclSSClassDef def1, oclSSClassDef def2) {
		if (def2 == null) {
			try {
				return (oclSSClassDef) def1.clone();
			} catch (CloneNotSupportedException e) {
				return null;
			}
		}

		String sortID = def1.getStateSortId();
		if (sortID == "" || sortID == null)
			sortID = def2.getStateSortId();

		oclSSClassDef cur = new oclSSClassDef(def1.getStateSort(), sortID);

		//for every sort's sub states
		List tmpList = def1.getStateList();
		if (tmpList.size() != 0) {
			ListIterator liDEF1 = def1.getStateList().listIterator();
			while (liDEF1.hasNext()) {
				oclStateList baseList1 = (oclStateList) liDEF1.next();
				//for every sort's sub states
				if (def2.getStateList().size() == 0)
					cur.setStateList(def1.getStateList());
				ListIterator liDEF2 = def2.getStateList().listIterator();
				while (liDEF2.hasNext()) {
					oclStateList baseList2 = (oclStateList) liDEF2.next();
					oclStateList tmpStateList =
						mergeStates(baseList1, baseList2);
					if (tmpStateList != null)
						cur.addState(tmpStateList);
				}
			}
		} else {
			if (def2.getStateList().size() != 0)
				cur.setStateList(def2.getStateList());
		}

		return cur;
	}

	/* WZ 22/3/02 */
	/**
	 * mergeStates merge two oclStateList together and
	 * return a new oclStateList
	 * @param stateList1 oclStateList
	 * @param stateList2 oclStateList
	 * @return oclSSClassDef
	 */
	public oclStateList mergeStates(
		oclStateList stateList1,
		oclStateList stateList2) {
		// 	 Utility.debugPrintln("merging statelist..."+stateList1.toString());
		try {
			oclStateList cur = (oclStateList) stateList1.clone();
			//to add new predicates
			ListIterator liGiven = stateList2.getPredicateList().listIterator();
			while (liGiven.hasNext()) {
				cur.addPredicate((oclPredicate) liGiven.next());
			}
			// 	     Utility.debugPrintln("add predicate ["+cur.toString()+"]");
			return cur;
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln("can't clone.");
			return null;
		}
	}

	/**
	 * getObjectDefOfSort - finds the object definition for a given sort
	 * @param sortName - Sort name
	 * @return oclObject - the object definition or null
	 */
	public oclObject getObjectDefOfSort(String sortName) {
		oclObject cur;
		ListIterator li = objects.listIterator();
		while (li.hasNext()) {
			cur = (oclObject) li.next();
			if (sortName.equals(cur.getObjectSort())) {
				return cur;
			}
		}
		return null;
	}

	/**
	 * getObjectsOfSubTypes
	 * collect the object names for all the subtypes of the given sort
	 * @param sortName name of the given sort
	 * @return List - of object names
	 */
	public List getObjectsOfSubTypes(String sortName) {
		List objects = new ArrayList();
		List sortSubs = getSortSubTypes(sortName);
		if (sortSubs == null) {
			return getObjectsOfSort(sortName);
		} else {
			ListIterator li = sortSubs.listIterator();
			while (li.hasNext()) {
				String sub = (String) li.next();
				List subSubs = getObjectsOfSubTypes(sub);
				if (subSubs != null) {
					objects.addAll(subSubs);
				}
			}
		}
		return objects;
	}

	/**
	 * isSort
	 * determine if given name is a defined sort
	 * @param name given name
	 * @return boolean
	 */
	public boolean isSort(String name) {
		if (sorts.size() == 0) {
			return false;
		}
		ListIterator li = sorts.listIterator();
		while (li.hasNext()) {
			oclSort cur = (oclSort) li.next();
			String candidate = cur.getSortParent();
			if (!"primitive_sorts".equals(candidate)) {
				if (name.equals(candidate)) {
					return true;
				}
			}
			if (cur.isSubType(name)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * editSortName
	 * utility for pattern editors to change the name of a sort in a temporary
	 * domain - just changes sort entries and object entries
	 * @param old - the old sort name
	 * @param newName - the new sort name
	 */
	public void editSortName(String old, String newName) {
		// 	Utility.debugPrintln("patterns","Asked to change " + old + " for " + newName);
		ListIterator li = sorts.listIterator();
		while (li.hasNext()) {
			oclSort curSort = (oclSort) li.next();
			if (old.equals(curSort.getSortName())) {
				curSort.setSortName(newName);
			} else {
				if (curSort.isSubType(old)) {
					List subs = curSort.getSubTypes();
					subs.remove(old);
					subs.add(newName);
				}
			}
		}
		li = objects.listIterator();
		while (li.hasNext()) {
			oclObject curObj = (oclObject) li.next();
			// 	    Utility.debugPrintln("patterns","Found objects for name " + curObj.getObjectSort());
			if (old.equals(curObj.getObjectSort())) {
				// 		Utility.debugPrintln("patterns","Changing objects name " + old);
				curObj.setObjectSort(newName);
			}
		}
	}

	// Ron 8/1/02 - added as utility for patterns
	/**
	 * createSortList
	 * returns a sort list from walking the sort tree 
	 * @param pname - the start position for the walk
	 *               usually implied-root
	 * @return - list of sort names
	 */
	public List createSortList(String pname) {
		java.util.List allSorts = new ArrayList();
		java.util.List subTypes = null;
		if (pname.equals(oclDomain.OCLSortImpliedRoot)) {
			subTypes = this.getSortRoots();
		} else {
			subTypes = this.getSortSubTypes(pname);
		}
		// Process the subTypes
		if (subTypes != null) {
			allSorts.addAll(subTypes);
			ListIterator li = subTypes.listIterator();
			while (li.hasNext()) {
				String subname = (String) li.next();
				List subs = createSortList(subname);
				allSorts.addAll(subs);
			}
		}
		return allSorts;
	}

	/**
	 * getSort
	 * determine if given name is a defined sort
	 * @param name given name
	 * @return oclSort
	 */
	public oclSort getSort(String name) throws NoSuchElementException {
		if (sorts.size() == 0) {
			throw new NoSuchElementException("No sorts defined");
		}
		ListIterator li = sorts.listIterator();
		while (li.hasNext()) {
			oclSort cur = (oclSort) li.next();
			String candidate = cur.getSortParent();
			if (name.equals(candidate)) {
				return cur;
			}
		}
		throw new NoSuchElementException("No sorts defined");
	}

	/**
	 * getSortRoots - get the top levels of the sort tree
	 * the sort tree is a multi-tree (may have multiple roots
	 * primitive-sorts - collects the base level sorts
	 * @return List (ArrayList of sort names)
	 */
	public List getSortRoots() {
		if (sorts.size() == 0) {
			return null;
		} else if (
			(sorts.size() == 1)
				&& ("primitive_sorts"
					.equals(((oclSort) sorts.get(0)).getSortParent()))) {
			return ((oclSort) sorts.get(0)).getSubTypes();
		} else if (sorts.size() == 1) {
			ArrayList subs = new ArrayList();
			subs.add(((oclSort) sorts.get(0)).getSortParent());
			return subs;
		} else {
			ArrayList roots = new ArrayList(); //Store results
			oclSort primSorts = null;
			ListIterator liOuter = sorts.listIterator();
			while (liOuter.hasNext()) {
				oclSort cur = (oclSort) liOuter.next();
				String candidate = cur.getSortParent();
				if (!"primitive_sorts".equals(candidate)) {
					if (!isSubTypeOfAny(candidate)) {
						roots.add(candidate);
					}
				} else {
					primSorts = cur;
				}
			}
			if (primSorts != null) { // Are any of these roots?
				ListIterator liPrim = primSorts.getSubTypes().listIterator();
				while (liPrim.hasNext()) {
					String prim = (String) liPrim.next();
					if (!isSubTypeOfAny(prim)) {
						roots.add(prim);
					}
				}
			}
			return roots;
		}
	}

	/** 
	 * isSubTypeOfAny -determine if given sort is a sub type of any other sort
	 * @param name sort name
	 * @return boolean
	 */
	public boolean isSubTypeOfAny(String name) {
		if (sorts.size() == 0) {
			return false;
		} else if (
			(sorts.size() == 1)
				&& ("primitive_sorts"
					.equals(((oclSort) sorts.get(0)).getSortParent()))) {
			return false;
		} else {
			ListIterator li = sorts.listIterator();
			while (li.hasNext()) {
				oclSort cur = (oclSort) li.next();
				if (!"primitive_sorts".equals(cur.getSortParent())) {
					if (cur.isSubType(name)) {
						return true;
					}
				}
			}
			return false;
		}
	}

	/**
	 * getSortSubTypes - find the direct subtypes of a given sort
	 * @param  name of given sort
	 * @ return List - of subtypes
	 */
	public List getSortSubTypes(String name) {
		// 	Utility.debugPrintln("Asked for sort subtypes " + name);
		ListIterator li = sorts.listIterator();
		while (li.hasNext()) {
			oclSort cur = (oclSort) li.next();
			if (name.equals(cur.getSortParent())) {
				return cur.getSubTypes();
			}
		}
		return null;
	}

	/**
	 * getAllSortSubTypes - find the direct and indirect 
	 * subtypes of a given sort
	 * @param sort name of given sort
	 * @ return List - of subtypes
	 */
	public List getAllSortSubTypes(String sort) {
		List ret = new ArrayList();

		List subs = getSortSubTypes(sort);
		if (subs != null) {
			ListIterator li = subs.listIterator();
			while (li.hasNext()) {
				String curSort = (String) li.next();
				List children = getAllSortSubTypes(curSort);
				ret.add(curSort);
				if (children != null) {
					ret.addAll(children);
				}
			}
		}
		return ret;

	}
	
	/**
		 * getprimitiveSubTypes - find the direct and indirect 
		 * primitive subtypes of a given sort
		 * @param sort name of given sort
		 * @ return List - of primitive subtypes
		 */
		public List getPrimitiveSubTypes(String sort) {
			List ret = new ArrayList();

			List subs = getSortSubTypes(sort);
			if (subs != null) {
				ListIterator li = subs.listIterator();
				while (li.hasNext()) {
					String curSort = (String) li.next();
					List curSubs = getSortSubTypes(curSort);
					if (curSubs != null) {
						List children = getPrimitiveSubTypes(curSort);
						if (children != null) {
							ret.addAll(children);
						}
					} else {
						ret.add(curSort);
					}
				}
			}
			return ret;

		}

	/**
	 * getBaseSortSubTypes - find the bottom level sorts from this sort
	 * subtypes of a given sort
	 * @param sort name of given sort
	 * @ return List - of subtypes
	 */
	public List getBaseSortSubTypes(String sort) {
		List ret = new ArrayList();

		List subs = getSortSubTypes(sort);
		if (subs != null) {
			ListIterator li = subs.listIterator();
			while (li.hasNext()) {
				String curSort = (String) li.next();
				List children = getSortSubTypes(curSort);
				if (children != null) {
					List ChildsSubs = getBaseSortSubTypes(curSort);
					ret.addAll(ChildsSubs);
				} else {
					ret.add(curSort);
				}
			}
		}
		return ret;

	}

	/**
	 * getSortUnifiers
	 * find all sorts where instances may unify with an instance of this sort
	 * The list includes all subtypes of this sort +
	 * parent sorts on route up to the top of the sort tree
	 * @param sort - the sort name
	 * @return - the list of sort names
	 */
	public List getSortUnifiers(String sort) throws NoSuchElementException {
		String parent = null;
		List ret = new ArrayList();

		List subs = getSortSubTypes(sort);
		if (subs != null) {
			ListIterator li = subs.listIterator();
			while (li.hasNext()) {
				ret.add(li.next());
			}
		}
		try {
			parent = findSortParent(sort);
		} catch (NoSuchElementException nse) {
			return ret;
		}
		ret.add(parent);
		while (parent != null) {
			String ancestor = null;
			// 	    Utility.debugPrintln("Looking for parent of " + parent);
			try {
				ancestor = findSortParent(parent);
			} catch (NoSuchElementException nse2) {
				return ret; // Found them all
			}
			// 	    Utility.debugPrintln("Found parent " + ancestor);
			ret.add(ancestor);
			parent = ancestor;
		}
		return ret;
	}

	/**
	 * findSortParent
	 * get the parent sort of the given sort
	 * an exception is thrown if there is no parent or this is not a sort
	 * @param name - the sort name
	 * @return String - the parent name
	 * @throws NoSuchElementException
	 */
	public String findSortParent(String name) throws NoSuchElementException {
		ListIterator li = sorts.listIterator();
		// 	Utility.debugPrintln(">> Looking for parent of " + name);
		while (li.hasNext()) {
			oclSort cur = (oclSort) li.next();
			if (cur.isSubType(name)) {
				if (!"primitive_sorts".equals(cur.getSortParent())) {
					// 		    Utility.debugPrintln(">> Found parent " + cur.getSortParent());
					return cur.getSortParent();
				}
			}
		}
		throw new NoSuchElementException("Cannot find the sort");
	}

	/**
	 * sortIsSubSortOf
	 * - find if the first named sort is a direct or indirect subsort of
	 * the second named sort
	 * @param sort1
	 * @param sort2
	 * @return boolean
	 */
	public boolean sortIsSubSortOf(String sort1, String sort2) {
		try {
			String parent = findSortParent(sort1);
			if (sort2.equals(parent)) {
				return true;
			} else {
				return sortIsSubSortOf(parent, sort2);
			}
		} catch (NoSuchElementException nse) {
			return false;
		}
	}

	/**
	 * allSortsHaveObjects()
	 * @return - boolean true if all sorts have object definitions
	 */
	public boolean allSortsHaveObjects() {
		if (sorts.size() > 1) {
			return false; // must be higherarchy of sorts
		}
		ListIterator li = ((oclSort) sorts.get(0)).getSubTypes().listIterator();
		while (li.hasNext()) {
			String curSort = (String) li.next();
			List objs = getObjectsOfSort(curSort);
			if (objs == null) {
				return false;
			}
		}
		return true;
	}

	/**
	 * allPrimSortsHaveObjects()
	 * @return - boolean true if all primitive sorts have object definitions
	 */
	public boolean allPrimSortsHaveObjects() throws NoSuchElementException {
		if (sorts.size() == 1) {
			ListIterator li =
				((oclSort) sorts.get(0)).getSubTypes().listIterator();
			while (li.hasNext()) {
				String curSort = (String) li.next();
				List objs = getObjectsOfSort(curSort);
				if (objs == null) {
					return false;
				}
			}
			return true;
		} else {
			// Must be hierarchy find primitive_sorts clause
			List subTypes = null;
			ListIterator li = sorts.listIterator();
			boolean found = false;
			while (!found && li.hasNext()) {
				oclSort curSort = (oclSort) li.next();
				if (curSort.getSortName().equals("primitive_sorts")) {
					subTypes = curSort.getSubTypes();
					found = true;
				}
			}
			if (!found) {
				throw new NoSuchElementException("No primitive Sorts Defined");
			} else {
				li = subTypes.listIterator();
				while (li.hasNext()) {
					String curSort = (String) li.next();
					List objs = getObjectsOfSort(curSort);
					if (objs == null) {
						return false;
					}
				}
				return true;
			}
		}
	}

	/**
	 * getPredicatesBySort - get list of all predicates that
	 * reference a given sort.
	 * @param name - String
	 * @return List of references to predicates
	 */
	public List getPredicatesBySort(String name) {
		ArrayList ans = new ArrayList();
		ListIterator liPreds = predicates.listIterator();
		while (liPreds.hasNext()) {
			oclPredicate cur = (oclPredicate) liPreds.next();
			ListIterator liArgs = cur.getArguments().listIterator();
			while (liArgs.hasNext()) {
				oclPredicate.pArg cArg = (oclPredicate.pArg) liArgs.next();
				if (name.equals(cArg.name)) {
					ans.add(cur);
					break; // dont add this predicate again
				}
			}
		}
		return ans;
	}

	// Ron 18/10/02 Added filter for unifiers
	/**
	 * getPredicatesBySortFromList - get list of all predicates that
	 * reference a given sort.
	 * @param  name - String
	 * @param preds the list of predicates
	 * @param incUnifiers include unifers if true
	 * @return List of references to predicates
	 */
	public List getPredicatesBySortFromList(String name, List preds, boolean incUnifiers) {
		ArrayList ans = new ArrayList();
		ListIterator liPreds = preds.listIterator();
		while (liPreds.hasNext()) {
			oclPredicate cur = (oclPredicate) liPreds.next();
			ListIterator liArgs = cur.getArguments().listIterator();
			boolean added = false;
			while (!added && liArgs.hasNext()) {
				oclPredicate.pArg cArg = (oclPredicate.pArg) liArgs.next();
				if (incUnifiers) {
					if (name.equals(cArg.name) || 
					    sortIsSubSortOf(cArg.name,name)) {
						ans.add(cur);
						added = true;
					}
				} else {
					if (name.equals(cArg.name)) {
						ans.add(cur);
						added = true;
						
					}
				}
			}
		}
		return ans;
	}

	/**
	 * getPredicatesByFirstRefSort - get list of all predicates that
	 * reference a given sort as the first argument.
	 * @param  name - String
	 * @return List of references to predicates
	 */
	public List getPredicatesByFirstRefSort(String name) {
		ArrayList ans = new ArrayList();
		ListIterator liPreds = predicates.listIterator();
		while (liPreds.hasNext()) {
			oclPredicate cur = (oclPredicate) liPreds.next();
			ListIterator liArgs = cur.getArguments().listIterator();
			if (liArgs.hasNext()) {
				oclPredicate.pArg cArg = (oclPredicate.pArg) liArgs.next();
				if (name.equals(cArg.name)) {
					ans.add(cur);
				}
			}
		}
		return ans;
	}

	// Ron 18/10/02 include filter to find unifiers if required	
	/**
	 * getPredicatesByFirstRefSortFromList - get list of all predicates that
	 * reference a given sort as the first argument.
	 * @param  name - String
	 * @param preds Predicate list
	 * @param incUnifiers - include unifiers if true
	 * @return List of references to predicates
	 */
	public List getPredicatesByFirstRefSortFromList(String name, List preds, boolean incUnifiers) {
		ArrayList ans = new ArrayList();
		ListIterator liPreds = preds.listIterator();
		while (liPreds.hasNext()) {
			oclPredicate cur = (oclPredicate) liPreds.next();
			ListIterator liArgs = cur.getArguments().listIterator();
			if (liArgs.hasNext()) {
				oclPredicate.pArg cArg = (oclPredicate.pArg) liArgs.next();
				if (incUnifiers) {
					if (name.equals(cArg.name) ||
					    sortIsSubSortOf(cArg.name,name)) {
						ans.add(cur);
					}
				} else {
					if (name.equals(cArg.name)) {
						ans.add(cur);
					}
				}
			}
		}
		return ans;
	}

	/**
	 * getPredicateByName
	 *      find the prototype definition of a predicate from its name
	 * @param predName - the predicate name
	 * @return - the predicate or null if not found
	 */
	public oclPredicate getPredicateByName(String predName) {
		ListIterator li = predicates.listIterator();
		oclPredicate result = null;
		while (result == null && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (predName.equals(cur.getName())) {
				result = cur;
			}
		}
		return result;
	}
	
	/**
	 * getFunctorByName
	 *      find the prototype definition of a functor from its name
	 * @param predName - the predicate name
	 * @return - the predicate or null if not found
	 */
	public oclFunctor getFunctorByName(String predName) {
		ListIterator li = functors.listIterator();
		oclFunctor result = null;
		while (result == null && li.hasNext()) {
			oclFunctor cur = (oclFunctor) li.next();
			if (predName.equals(cur.getName())) {
				result = cur;
			}
		}
		return result;
	}

	/**
	 * getStateListForSort - get the State list for the given sort
	 * @param name the sort name
	 * @return the state list
	 */
	public oclSSClassDef getStateListForSort(String name)
		throws OCLNoSuchElementException {
		ListIterator li = classDefs.listIterator();
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			if (name.equals(cur.getStateSort())) {
				return cur;
			}
		}
		throw new OCLNoSuchElementException("No definitions for sort");
	}

	/* WZ 22/3/02 */
	/**
	 * getStateListForSort - get the State list for the given sort
	 * do not use default classDefs
	 * @param classDEFList
	 * @param name
	 * @return the state list
	 */
	public oclSSClassDef getStateListForSort(List classDEFList, String name)
		throws OCLNoSuchElementException {
		ListIterator li = classDEFList.listIterator();
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			if (name.equals(cur.getStateSort())) {
				return cur;
			}
		}
		throw new OCLNoSuchElementException("No definitions for sort");
	}

	/**
	 * markStaticPredicates
	 * used to find the implied static predicates in the domain
	 * a predicate is static if there are occurances of it in the
	 * atomic invarients
	 */
	public void markStaticPredicates() {
		ListIterator liPreds = predicates.listIterator();
		ListIterator liAtoms;
		if (atomicInvars.size() < 1)
			return;
		while (liPreds.hasNext()) {
			oclPredicate curPred = (oclPredicate) liPreds.next();
			liAtoms = atomicInvars.listIterator();
			boolean found = false;
			while (!found && liAtoms.hasNext()) {
				oclPredicate curAtom = (oclPredicate) liAtoms.next();
				if (curPred.getName().equals(curAtom.getName())
					&& curPred.size() == curAtom.size()) {
					found = true;
					curPred.setStatic(true);
				}
			}
		}
	}

	/**
	 * createOperatorSignature
	 * find all predicate parameters and their sorts and form the operator name
	 * with conditional effect
	 * @param curOp - the given operator
	 * @return oclPredicate - the signature / name
	 */
	public oclPredicate createOPSignature(oclOperator curOp) {
		oclPredicate variblePredicate =
			new oclPredicate(curOp.opName.getName());

		ListIterator li, lii;
		//oclSE
		li = curOp.getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			lii = se.getPredicateList().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}
		//oclSC 
		for (int i = 1; i < 3; i++) {
			if (i == 1)
				li = curOp.getNecessary().listIterator();
			if (i == 2)
				li = curOp.getConditional().listIterator();

			while (li.hasNext()) {
				oclSC necessary = (oclSC) li.next();
				lii = necessary.getPre().listIterator();
				while (lii.hasNext()) {
					addSignatureArgument(
						variblePredicate,
						(oclPredicate) lii.next());
				}
				lii = necessary.getPost().listIterator();
				while (lii.hasNext()) {
					addSignatureArgument(
						variblePredicate,
						(oclPredicate) lii.next());
				}
			}
		}

		return variblePredicate;
	}

	/**
	 * createOperatorSignature
	 * find all predicate parameters and their sorts and form the operator name
	 * ignore conditional effect
	 * @param curOp - the given operator
	 * @return oclPredicate - the signature / name
	 */
	public oclPredicate createOperatorSignature(oclOperator curOp) {
		oclPredicate variblePredicate =
			new oclPredicate(curOp.opName.getName());

		ListIterator li, lii;
		//oclSE
		li = curOp.getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			lii = se.getPredicateList().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}

		//oclSC 
		li = curOp.getNecessary().listIterator();
		while (li.hasNext()) {
			oclSC necessary = (oclSC) li.next();
			lii = necessary.getPre().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
			lii = necessary.getPost().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}

		return variblePredicate;
	}
	
	/**
	 * createEventSignature
	 * find all predicate parameters and their sorts and form the event name
	 * ignore conditional effect
	 * @param oclOperator - the given process
	 * @return oclPredicate - the signature / name
	 */
	public oclPredicate createEventSignature(oclEvent curOp) {
		oclPredicate variablePredicate =
			new oclPredicate(curOp.opName.getName());

		ListIterator li, lii;
		//oclSE
		li = curOp.getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			lii = se.getPredicateList().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variablePredicate,
					(oclPredicate) lii.next());
			}
		}
		//oclSC 
		li = curOp.getNecessary().listIterator();
		while (li.hasNext()) {
			oclSC necessary = (oclSC) li.next();
			lii = necessary.getPre().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variablePredicate,
					(oclPredicate) lii.next());
			}
			lii = necessary.getPost().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variablePredicate,
					(oclPredicate) lii.next());
			}
		}
		return variablePredicate;
	}
	
	/**
	 * createProcessSignature
	 * find all predicate parameters and their sorts and form the process name
	 * ignore conditional effect
	 * @param oclOperator - the given process
	 * @return oclPredicate - the signature / name
	 */
	public oclPredicate createProcessSignature(oclProcess curOp) {
		oclPredicate variablePredicate =
			new oclPredicate(curOp.opName.getName());

		ListIterator li, lii;
		//oclSE
		li = curOp.getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			variablePredicate.addSortedVarArgument(se.name,se.sort);
			lii = se.getPredicateList().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variablePredicate,
					(oclPredicate) lii.next());
			}
		}

		//oclSC 
		li = curOp.getNecessary().listIterator();
		while (li.hasNext()) {
			oclSC necessary = (oclSC) li.next();
			variablePredicate.addSortedVarArgument(necessary.name,necessary.sort);
			lii = necessary.getPre().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variablePredicate,
					(oclPredicate) lii.next());
			}
			lii = necessary.getPost().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variablePredicate,
					(oclPredicate) lii.next());
			}
			//TODO should look at conditions and updates for arguments
		}

		return variablePredicate;
	}

	/**
	 * addSignatureArgument
	 * add a new sorted argument name (variable only) to the signature
	 * @param operatorName - the signature
	 * @param prd - a predicate to be searched for new arguments to add
	 */
	public void addSignatureArgument(
		oclPredicate operatorName,
		oclPredicate prd) {
		boolean gotIt = false;
		OPredicate.pArg newStr, existingStr;
		//get functor/name prd.getName
		// if name is ne ignore
		if (prd.getName().equals("ne")
			|| prd.getName().equals("is_of_sort")
			|| prd.getName().equals("is_of_primitive_sort")) {
			return;
		}
		// otherwise search predicates for matching prd.name
		List list = prd.getArguments();
		ListIterator li = list.listIterator();
		while (li.hasNext()) {
			newStr = (OPredicate.pArg) li.next();
			if (OEnviron.isVar(newStr.name)) {
				ListIterator liOPName =
					(operatorName.getArguments()).listIterator();
				while (liOPName.hasNext()) {
					existingStr = (OPredicate.pArg) liOPName.next();
					if (existingStr.name.equals(newStr.name)) {
						if (!newStr.sort.equals(existingStr.sort)) {
							// Ron 29/901 bug fix getAll need to go 
							// down to any depth
							List existSubs =
								getAllSortSubTypes(existingStr.sort);
							List newSortSubs = getAllSortSubTypes(newStr.sort);
							if (newSortSubs == null) { // bottom level
								existingStr.sort = newStr.sort;
								gotIt = true;
								break;
							} else if (existSubs.size() > newSortSubs.size()) {
								existingStr.sort = newStr.sort;
								gotIt = true;
								break;
							} else {
								gotIt = true;
								break;
							}
						} else {
							gotIt = true;
							break;
						}
					}
				}
				if (!gotIt) {
					//Utility.debugPrintln("opmaker","Adding signature for arg " + newStr.name + " of sort " + newStr.sort);
					operatorName.addSortedVarArgument(
						new String(newStr.name),
						new String(newStr.sort));
					/* WZ added colne 26/3/02 */
				} else
					gotIt = false;
			}
		}
	}

	/**
	 * createMethodSignature
	 * find all predicate parameters and their sorts and form the compound operator name
	 * @param curOM - the given oclMethod
	 * @return oclPredicate - the signature / name
	 */
	public oclPredicate createMethodSignature(oclMethod curOM) {
		oclPredicate variblePredicate =
			new oclPredicate(curOM.getName().getName());

		ListIterator li, lii;
		//oclSE
		// 	li = curOM.getPrecondition().listIterator();
		// 	while (li.hasNext()) {
		// 	    oclSE se = (oclSE)li.next();
		// 	    lii = se.getPredicateList().listIterator();
		// 	    while (lii.hasNext()) {
		// 		addSignatureArgument(variblePredicate, (oclPredicate) lii.next());
		// 	    }
		// 	}

		//Index - oclSC
		li = curOM.getIndex().listIterator();
		while (li.hasNext()) {
			oclSC sc = (oclSC) li.next();
			lii = sc.getPre().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
			lii = sc.getPost().listIterator();
			while (lii.hasNext()) {
				addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}

		// 	//statics - oclPredicate
		// 	List staticList = (List)curOM.getStatics();
		// 	lii = staticList.listIterator();
		// 	while (lii.hasNext()) {
		// 	    addSignatureArgument(variblePredicate, (oclPredicate) lii.next());
		// 	}

		return variblePredicate;
	}

	/**
	 * addAllArgument
	 * add a new sorted argument name (variable or constant) to the signature
	 * @param operatorName - the signature
	 * @param prd - a predicate to be searched for new arguments to add
	 */
	public void addAllArgument(oclPredicate operatorName, oclPredicate prd) {
		boolean gotIt = false;
		OPredicate.pArg newStr, existingStr;
		//get functor/name prd.getName
		// if name is ne ignore
		if (prd.getName().equals("ne")
			|| prd.getName().equals("is_of_sort")
			|| prd.getName().equals("is_of_primitive_sort")) {
			return;
		}
		// otherwise search predicates for matching prd.name
		List list = prd.getArguments();
		ListIterator li = list.listIterator();
		while (li.hasNext()) {
			newStr = (OPredicate.pArg) li.next();
			ListIterator liOPName =
				(operatorName.getArguments()).listIterator();
			while (liOPName.hasNext()) {
				existingStr = (OPredicate.pArg) liOPName.next();
				if (existingStr.name.equals(newStr.name)) {
					if (!newStr.sort.equals(existingStr.sort)) {
						// down to any depth
						List existSubs = getAllSortSubTypes(existingStr.sort);
						List newSortSubs = getAllSortSubTypes(newStr.sort);
						if (newSortSubs == null) { // bottom level
							existingStr.sort = newStr.sort;
							gotIt = true;
							break;
						} else if (existSubs.size() > newSortSubs.size()) {
							existingStr.sort = newStr.sort;
							gotIt = true;
							break;
						} else {
							gotIt = true;
							break;
						}
					} else {
						gotIt = true;
						break;
					}
				}
			}
			if (!gotIt) {
				operatorName.addSortedVarArgument(
					new String(newStr.name),
					new String(newStr.sort));
			} else
				gotIt = false;
		}
	}

	/**
	 * to find out a name (oclPredicate) stands for oclMethod or oclOperator.
	 * when oprd stands for an oclMethod 
	 * return an oclMethod with its name only (no contents);
	 * when oprd stands for an oclOperator
	 * return an oclOperator with all contents
	 * @param oprd oclPredicate
	 * @return object
	 */
	public Object checkObjectType(oclPredicate oprd) {
		oclPredicate checkOPD = null;
		//check methods first
		oclMethod returnMethod = new oclMethod();
		ListIterator li = methods.listIterator();
		while (li.hasNext()) {
			try {
				oclMethod curMD = (oclMethod) ((oclMethod) li.next()).clone();
				checkOPD = (oclPredicate) curMD.getName();
				//check if method name are the same
				if (checkOPD.isSameType(oprd)) {
					//check if statics of all variables are true 5/4/02

					/* WZ 17/4/02 do not return a real method 
					   but only a reference */
					// 		    //instantiate current method
					curMD.instantiateWith(oprd);
					returnMethod.setName(createMethodSignature(curMD));
					//WZ 31/5/02
					returnMethod.setName(curMD.getName()); //WZ 31/5/02
					returnMethod.setIndexSC(curMD.getIndex()); //WZ 25/4/02

					return returnMethod;
				}
			} catch (CloneNotSupportedException e) {
				continue;
			}
		}

		//check operator next
		oclOperator returnOperator = null;
		li = operators.listIterator();
		while (li.hasNext()) {
			try {
				returnOperator =
					(oclOperator) ((oclOperator) li.next()).clone();
				;
				checkOPD = (oclPredicate) returnOperator.opName;
				if (checkOPD.isSameType(oprd)) { //WZ 23/4/02 to check more
					//instantiate current operator
					jplan.general.Utility.debugPrintln(
						"inst Operator >>  "
							+ returnOperator.opName.toString());
					returnOperator.instantiateWith(oprd);
					return returnOperator;
				}
			} catch (CloneNotSupportedException e) {
				continue;
			}
		}

		return null;
	}

	/**
	 * hasTasksOnly
	 * Tests to see if the current domain only stores tasks
	 * @return boolean - if only tasks elements defined
	 */
	public boolean hasTasksOnly() {
		if ((tasks.size() > 0 || htntasks.size() > 0)
			&& (sorts.size() == 0)
			&& (objects.size() == 0)
			&& (predicates.size() == 0)
			&& (classDefs.size() == 0)
			&& (atomicInvars.size() == 0)
			&& (impliedInvars.size() == 0)
			&& (inconsistentConst.size() == 0)
			&& (operators.size() == 0)
			&& (methods.size() == 0))
			return true;
		else
			return false;
	}

	/**
	 * getMyDescription 
	 * retrieve the text description for the domain
	 * @return String - the description
	 */
	public String getMyDescription() {
		return descriptions.getDescription("domain" + name);
	}

	/**
	 * setMyDescription 
	 * @param desc - the text description of the domain
	 * set the text description for the domain
	 */
	public void setMyDescription(String desc) {
		descriptions.addDescription("domain" + name, desc);
	}

	/**
	 * getDescriptionObject
	 * @return - the description object
	 */
	public oclDescriptions getDescriptionObject() {
		return descriptions;
	}

	/**
	 * checkSortTree
	 * do verification checks on sorts and objects
	 * pre - no checks need be done before this
	 * @return List of (Strings) error messages - empty if all checks passed
	 */
	public List checkSortTree() throws jplan.general.OCLException {
		List mssgs = new ArrayList();

		if (sorts.size() == 0) {
			throw new jplan.general.OCLException("No sorts defined");
		} else if (
			(sorts.size() == 1)
				&& ("primitive_sorts"
					.equals(((oclSort) sorts.get(0)).getSortParent()))) {
			checkListUnique(
				((oclSort) sorts.get(0)).getSubTypes(),
				mssgs,
				"There are two or more primitive sorts with the name ");
			return mssgs;
		} else {
			ListIterator li = sorts.listIterator();
			boolean foundPrims = false;
			List primSorts = null;
			List subSorts = new ArrayList();
			List parentSorts = new ArrayList();
			while (li.hasNext()) {
				oclSort cur = (oclSort) li.next();
				if ("primitive_sorts".equals(cur.getSortParent())) {
					primSorts = cur.getSubTypes();
					if (foundPrims) {
						mssgs.add(
							"There are two 'primitive_sorts' clauses in the definition.");
					} else {
						checkListUnique(
							((oclSort) sorts.get(0)).getSubTypes(),
							mssgs,
							"There are two or more primitive sorts with the name ");
						foundPrims = true;
					}
				} else {
					subSorts.addAll(cur.getSubTypes());
					parentSorts.add(cur.getSortName());
					checkCycles(cur.getSortName(), new ArrayList(), mssgs);
				}
			}
			if (foundPrims) {
				List derivedPrims = getListDifference(subSorts, parentSorts);
				printList(derivedPrims, "Derived prims");
				if (getListDifference(derivedPrims, primSorts).size() != 0) {
					mssgs.add(
						"Not all base sorts are recorded as primitive_sorts.");
				}
				// Are the defined primitives really primitive?
				ListIterator liPrims = primSorts.listIterator();
				while (liPrims.hasNext()) {
					String name = (String) liPrims.next();
					if (getSortSubTypes(name) != null) {
						mssgs.add(
							"Sort "
								+ name
								+ " defined as primitive but has sub-sorts.");
					}
				}

			}
			checkListUnique(
				subSorts,
				mssgs,
				"This sort is defined as the sub-sort\n of more than one parent sort ");
			checkListUnique(
				parentSorts,
				mssgs,
				"This sort is defined more than once as a parent sort  ");
			// Now check the object clauses
			if (!foundPrims) {
				primSorts = getListDifference(subSorts, parentSorts);
			}
			List allObjs = new ArrayList();
			ListIterator liObjs = objects.listIterator();
			while (liObjs.hasNext()) {
				oclObject curDef = (oclObject) liObjs.next();
				if (!primSorts.contains(curDef.getObjectSort())) {
					mssgs.add(
						"There are object definitions for the non-base sort "
							+ curDef.getObjectSort()
							+ ".");
				}
				allObjs.addAll(curDef.getObjectNames());
			}
			checkListUnique(
				allObjs,
				mssgs,
				"There are duplicate occurances of the object name ");

			return mssgs;
		}
	}

	/**
	 * sortCycleTest
	 * this check for cycles in the adjacency lists defining sorts
	 * @return List - list of strings identifying sorts participating
	 *                in cycles
	 */
	public List sortCycleTest() {
		List mssgs = new ArrayList();
		ListIterator li = sorts.listIterator();
		oclSort cur = null;
		boolean found = false;
		while (!found && li.hasNext()) {
			cur = (oclSort) li.next();
			found = checkCycles(cur.getSortName(), new ArrayList(), mssgs);
		}
		return mssgs;
	}

	/**
	 * checkCycles
	 * checks to see if there are cycles in the defined sort tree
	 * @param  name : the name of the node to search from
	 * @param been : List of names of visited nodes
	 * @param mssgs - message list to record cycle failures
	 * @return boolean - true if cycle found;
	 */
	private boolean checkCycles(String name, List been, List mssgs) {
		ListIterator li = sorts.listIterator();
		oclSort cur = null;
		boolean found = false;
		while (!found && li.hasNext()) {
			cur = (oclSort) li.next();
			if (name.equals(cur.getSortName())) {
				found = true;
			}
		}
		if (found) {
			li = cur.getSubTypes().listIterator();
			while (li.hasNext()) {
				String sub = (String) li.next();
				if (been.contains(sub)) {
					mssgs.add(
						"There is a cycle in the sort tree for the sort "
							+ sub);
					return true;
				} else {
					been.add(name);
					checkCycles(sub, been, mssgs);
				}
			}
		}
		return false;
	}

	/**
	 * removeSortDefinition
	 * deletes a sort definition for the given sort
	 * used to try and repair cycles
	 * @param  name of sort definition to remove
	 */
	public void removeSortDefinition(String name) {
		ListIterator li = sorts.listIterator();
		oclSort cur = null;
		int inx = 0;
		boolean found = false;
		while (!found && li.hasNext()) {
			cur = (oclSort) li.next();
			if (name.equals(cur.getSortName())) {
				sorts.remove(inx);
				found = true;
			}
			inx++;
		}
	}

	/**
	 * removeObjectOfSort
	 * deletes the object of a sort 
	 * used to try and repair cycles
	 * @param  name of sort of objects to be removed
	 */
	public void removeObjectsOfSort(String name) {
		ListIterator li = objects.listIterator();
		oclObject cur = null;
		int inx = 0;
		boolean found = false;
		while (!found && li.hasNext()) {
			cur = (oclObject) li.next();
			if (name.equals(cur.getObjectSort())) {
				objects.remove(inx);
				found = true;
			}
			inx++;
		}
	}

	/**
	 * isObjectOfSort
	 * test to see if the given object name is defined as being
	 * of the given sort name
	 * @param  object name
	 * @param  sort name
	 * @return boolean - true if object is of the sort
	 */
	public boolean isObjectOfSort(String object, String sort) {
		if (isHierarchical()) {
			List objs = getObjectsOfSubTypes(sort);
			return Utility.listContainsString(object, objs);
		} else {
			boolean found = false;
			ListIterator li = objects.listIterator();
			while (!found && li.hasNext()) {
				oclObject cur = (oclObject) li.next();
				if (sort.equals(cur.getObjectSort())) {
					found = true;
					return cur.isObject(object);
				}
			}
			return found;
		}
	}

	/**
	 * checkPredicates
	 * perform semantic checks on predicates
	 * 1. check that all predicate names are unique
	 * 2. check that all sort names are recorded in the sort tree
	 * pre-condition - sort checks have been done
	 * @return List of (Strings) error messages - empty if all checks passed
	 */
	public List checkPredicates() {
		List mssgs = new ArrayList();
		List functors = new ArrayList();
		List badArgs = new ArrayList();

		ListIterator li = predicates.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (listContainsString(cur.getName(), functors)) {
				mssgs.add(
					"There are duplicate entries for the predicate name "
						+ cur.getName());
			} else {
				functors.add(cur.getName());
			}
			ListIterator liArgs = cur.getArguments().listIterator();
			while (liArgs.hasNext()) {
				String arg = ((OPredicate.pArg) liArgs.next()).name;
				if (!isSort(arg)) {
					if (!listContainsString(arg, badArgs)) {
						mssgs.add(
							"Predicate argument "
								+ arg
								+ " of predicate "
								+ cur.getName()
								+ " is not a defined sort");
						badArgs.add(arg);
					}
				}
			}
		}
		return mssgs;
	}

	// Ron 8/11/02 - Added side effect to add sorts to atomic invariants arguments
	
	/**
	 * checkAtomic
	 * check the atomic invariants
	 * 1. predicates are listed in the predicates definition
	 * 2. all arguments are of the correct sort
	 * 3. all entries are unique
	 * 4. all static predicates have entries in the atomic invatiants list
	 * SIDE EFFECT - mark predicates as static if not already done so
	 *             - add sorts to arguments if not already set
	 * pre-condition sort and predicate tests have been done.
	 * @return List  of (Strings) error messages - empty if all checks passed
	 */
	public List checkAtomic() {
		List mssgs = new ArrayList();
		List badPreds = new ArrayList();
		List badObjects = new ArrayList();
		TreeSet atomicSet = new TreeSet();
		TreeSet staticPreds = new TreeSet();
		oclPredicate proto = null;

		ListIterator li = atomicInvars.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			try {
				proto = findPrototype(cur);
			} catch (OCLSelectionException e) {
				if (!listContainsString(cur.getName(), badPreds)) {
					mssgs.add(
						"There is no predicate definition for the"
							+ " predicate "
							+ cur.getName());
					badPreds.add(cur.getName());
				}
				break;
			}
			if (!proto.isStatic()) {
				proto.setStatic(true);
			}
			staticPreds.add(proto.toString());
			List args = cur.getArguments();
			List sorts = proto.getArguments();
			ListIterator liArgs = args.listIterator();
			ListIterator liSorts = sorts.listIterator();
			while (liArgs.hasNext()) {
				OPredicate.pArg arg = (OPredicate.pArg) liArgs.next();
				String argName = arg.name;
				String sortName = ((OPredicate.pArg) liSorts.next()).name;
				// Ron 29/5/03 May be a sub type
				List objs = getObjectsOfSubTypes(sortName);
				// Condition was isObjectOfSort
				if (!Utility.listContainsString(argName, objs)) {
					if (!listContainsString(argName, badObjects))
						mssgs.add(
							"Object "
								+ argName
								+ " is not defined as an object of sort "
								+ sortName);
					badObjects.add(argName);
				} else if (arg.sort.equals("N/A")) {
					arg.sort = sortName;
				}
				
			}
			if (!atomicSet.add(cur.toString())) {
				mssgs.add(
					cur.toString()
						+ " is already defined as an atomic invariant.");
			}
		}
		// Now check that all static predicates have been used
		li = predicates.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (cur.isStatic()) {
				if (staticPreds.add(cur.toString())) {
					mssgs.add(
						cur.toString()
							+ " is defined as static but has no examples defined.");
				}
			}
		}
		return mssgs;
	}

	/**
	 * listContainsString
	 * check a list of strings to see if it contains the given string
	 * @param str - the given string
	 * @param lst - the list to check
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
	 * this is a debugging utility
	 */
	private static void printList(List l, String msg) {
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

	/* WZ 21/8/02 */
	/**
	 * checkOps
	 * do verification checks on operators and methods
	 * pre - check states done prior to this
	 * @return List of (Strings) error messages - empty if all checks passed
	 */
	public List checkOps() {
		List mssgs = new ArrayList();
		List temp = new ArrayList();
		temp = checkOperators();
		mssgs.addAll(temp);
		temp = checkMethods();
		mssgs.addAll(temp);
		return mssgs;
	}

	/* WZ 21/8/02 */
	// Ron 12/5/03 add check names unique
	/**
	 * do verification checks on operators
	 * pre - check states done prior to this
	 * @return List of (Strings) error messages - empty if all checks passed
	 */
	public List checkOperators() {
		List msgList = new ArrayList();
		List opNames = new ArrayList();
		boolean checkOK = true;
		boolean allChecksOK = true;

		ListIterator li = operators.listIterator();
		while (li.hasNext()) {
			oclOperator curOp = (oclOperator) li.next();
			checkOK = curOp.check(this, msgList);
			if (!checkOK) {
				allChecksOK = false;
			}
			if (Utility.listContainsString(curOp.opName.getName(),opNames)) {
				msgList.add("Duplicate Operator Name " + curOp.opName.getName());
				allChecksOK = false;
			} else {
				opNames.add(curOp.opName.getName());
			}
		}

		if (!allChecksOK) {
			// Output to stdout for the moment
			li = msgList.listIterator();
			while (li.hasNext()) {
				Utility.debugPrintln((String) li.next());
			}
		} else {
			msgList.clear();
		}
		return msgList;
	}
	
	/**
	 * getOperatorByFunctorName
	 * find operator from op functor name
	 * @param fName
	 * @return
	 */
	// Ron 19/10/04
	public oclOperator getOperatorByFunctorName(String fName){
		ListIterator li = operators.listIterator();
		while (li.hasNext()) {
			oclOperator curOp = (oclOperator) li.next();
			if (curOp.opName.getName().equals(fName))
				return curOp;
		}
		return null;
	}

	/* WZ 21/8/02 */
	/**
	 * do verification checks on methods
	 * pre - check states done prior to this
	 * @return List of (Strings) error messages - empty if all checks passed
	 */
	public List checkMethods() {
		List msgList = new ArrayList();
		boolean checkOK = true;
		boolean allChecksOK = true;

		ListIterator li = methods.listIterator();
		while (li.hasNext()) {
			oclMethod curMD = (oclMethod) li.next();
			checkOK = curMD.check(this, msgList);
			if (!checkOK) {
				allChecksOK = false;
			}
		}

		if (!allChecksOK) {
			// Output to stdout for the moment
			li = msgList.listIterator();
			while (li.hasNext()) {
				Utility.debugPrintln((String) li.next());
			}
		} else {
			msgList.clear();
		}
		return msgList;
	}

	/**
	 * do transparency checks on methods
	 * pre - other checks should have been passed
	 * @return List of (Strings) error messages - empty if all checks passed
	 */
	public List checkTransparency() {
		List msgList = new ArrayList();
		boolean checkOK = true;

		ListIterator li = methods.listIterator();
		while (li.hasNext()) {
			oclMethod curMD = (oclMethod) li.next();
			msgList.add("Checking Method " + curMD.getName().toString());
			int len = msgList.size();
			checkOK = curMD.checkTransparency(this, msgList);
			if (msgList.size() == len) {
				msgList.add("        OK");
			}
		}
		
		return msgList;
	}
	/**
	 * checkStateUses
	 * Do reachability checks on states against operators
	 * make sure producers and consumers lists of state definintions
	 * are populated
	 */
	public void checkStateUses() {
		// First clear any old analysis - empty producers and consumers lists
		ListIterator li = classDefs.listIterator();
		while (li.hasNext()) {
			oclSSClassDef curDef = (oclSSClassDef) li.next();
			curDef.clearProducersConsumers();
		}
		// Now do the analysis
		li = operators.listIterator();
		while (li.hasNext()) {
			oclOperator curOp = (oclOperator) li.next();
			curOp.checkUses(this);
		}
	}

	/**
	 * checkStates
	 * do verification checks on states
	 * @return List of (Strings) error messages
	 */
	public List checkStates() {
		List msgList = new ArrayList();

		ListIterator li = classDefs.listIterator();
		while (li.hasNext()) {
			oclSSClassDef curDef = (oclSSClassDef) li.next();
			curDef.check(this, msgList);
		}
		return msgList;
	}

	/**
	 * checkAllPredicateUses
	 * check to see that all dynamic predicates are used in atleast one
	 * substate definition
	 * also warn if predicates used in substate definitions of more than 
	 * one sort
	 * @return List mssgs List of (Strings) error messages
	 */
	public List checkAllPredicateUses() {
		List mssgs = new ArrayList();
		ListIterator li = predicates.listIterator();
		while (li.hasNext()) {
			oclPredicate curPred = (oclPredicate) li.next();
			if (!curPred.isStatic()) {
				checkPredicateUse(curPred, mssgs);
			}
		}
		return mssgs;
	}

	/**
	 * checkPredicateUse
	 * check to see that the givenpredicate is used in atleast one
	 * substate definition
	 * also warn if predicate used in substate definitions of more than 
	 * one sort
	 * @param curPred - the given predicate
	 * @param mssgs - the message list
	 * @return List mssgs List of (Strings) error messages
	 */
	public List checkPredicateUse(oclPredicate curPred, List mssgs) {
		ListIterator li = classDefs.listIterator();
		String firstUse = curPred.toString();
		int useCount = 0;
		while (li.hasNext()) {
			oclSSClassDef curDef = (oclSSClassDef) li.next();
			if (curDef.usesPred(curPred)) {
				if (useCount == 0) {
					firstUse =
						firstUse.concat(
							" is used in the definition of the sort "
								+ curDef.getStateSort());
				} else if (useCount == 1) {
					mssgs.add(firstUse);
					mssgs.add("     and in " + curDef.getStateSort());
				} else {
					mssgs.add("     and in " + curDef.getStateSort());
				}
				useCount++;
			}
		}
		if (useCount == 0) {
			mssgs.add(
				curPred.toString()
					+ " is not used in any substate definition.");
		}
		return mssgs;
	}

	// Ron added 25/01/02 to support pattern validation
	/**
	 * predicateNameUsed
	 *     check to see if the given name is the name of an existing
	 *     predicate
	 * @param name - the predicate name
	 * @return - true if found
	 */
	public boolean predicateNameUsed(String name) {
		ListIterator li = predicates.listIterator();
		while (li.hasNext()) {
			oclPredicate pred = (oclPredicate) li.next();
			if (name.equals(pred.getName())) {
				return true;
			}
		}
		return false;
	}

	/* WZ note: 
	   something should be added like 'isofpremitivesort', 'ne' */
	/**
	 * findPrototype
	 * find a matching predicate from the predicates list
	 * to the given predicate
	 * @param given - the given predicate to find the match for
	 * @return oclPredicate - reference to the matching predicate
	 */
	public oclPredicate findPrototype(oclPredicate given)
		throws OCLSelectionException {
		if (given.getName().equals("is_of_sort")
			&& given.size() == 2
			&& isSort(given.getNthElementName(1))) {
			oclPredicate ret = new oclPredicate("is_of_sort");
			ret.addConstArgument(given.getNthElementName(1));
			ret.addConstArgument(given.getNthElementName(1));
			return ret;
		}
		//WZ 22/4/02
		// 	if(given.getName().equals("is_of_primitive_sort") &&
		// 	   given.size() == 2 &&
		// 	   isSort(given.getNthElementName(1)) ) {
		// 	    oclPredicate ret = new oclPredicate("is_of_primitive_sort");
		// 	    ret.addConstArgument(given.getNthElementName(1));
		// 	    ret.addConstArgument(given.getNthElementName(1));
		// 	    return ret;
		// 	}
		//WZ 22/4/02 end 
		ListIterator li = predicates.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (cur.getName().equals(given.getName())
				&& cur.size() == given.size()) {
				return cur;
			}
		}
		if (oclPlus){
			li = functors.listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				if (cur.getName().equals(given.getName())
						&& cur.size() == given.size()) {
					return cur;
				}
			}
		}
		throw new OCLSelectionException(
			"No prototype for predicate " + given.toString());
	}

	/**
	 * findPrototype
	 * find a matching predicate from the predicates list
	 * to the given predicate
	 * @param given predicate name to find the match for
	 * @return oclPredicate - reference to the matching predicate
	 */
	public oclPredicate findPrototype(String given)
		throws OCLSelectionException {
		ListIterator li = predicates.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (cur.getName().equals(given)) {
				return cur;
			}
		}
		throw new OCLSelectionException(
			"No prototype for predicate " + given.toString());
	}

	/* 6/6/02 */
	/**
	 * Return states for the given sortname
	 * @param sortName given sort name
	 * @return oclSSClassDef
	 */
	public oclSSClassDef getStates(String sortName) {
		ListIterator li = classDefs.listIterator();
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			if (sortName.equals(cur.getStateSort())) {
				return cur;
			}
		}
		return null;
	}

	/* WZ 27/8/02 */
	/**
	 * Return true if the given sort has substates
	 * @param sortName given sort name
	 * @return true if the given sort has substates
	 */
	public boolean hasStates(String sortName) {
		ListIterator li = classDefs.listIterator();
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			if (sortName.equals(cur.getStateSort())) {
				if (cur.getStateList().size() > 0)
					return true;
			}
		}
		return false;
	}

	/**
	 * seUnifiesWithStateDef
	 * find a state definition to unify with the given elements 
	 * of an sub-state expression
	 * @param  name - the se Id
	 * @param sort - the sort of the Id - the se expression
	 * @param strippedState - predicate list - stripped of ne and static predicates
	 * @return boolean - true if matching definition found
	 */
	public boolean seUnifiesWithStateDef(
		String name,
		String sort,
		List strippedState) {
		if (isHierarchical()) {
			Utility.debugPrintln("This domain is Hierarchical");
			return seUnifiesWithHierarchicalStateDef(name, sort, strippedState);
		}
		oclSSClassDef def = null;

		ListIterator li = classDefs.listIterator();
		boolean foundDef = false;
		while (!foundDef && li.hasNext()) {
			def = (oclSSClassDef) li.next();
			if (sort.equals(def.getStateSort())) {
				// found correct state def
				foundDef = true;
			}
		}
		if (!foundDef) {
			return false;
		}
		return def.seUnifiesWithStateDef(name, strippedState);
	}
	/**
	 * opTransReferencesRHS
	 * find the ID of the transition with the given sort that references the second
	 * given sort on the RHS of the transition
	 * @param opName
	 * @param transSort
	 * @param refSort
	 * @return
	 */
	public String opTransReferencesRHS(String opName,String transSort, String refSort){
		oclOperator op = getOperatorByFunctorName(opName);
		if (op != null) {
			ListIterator li = op.getNecessary().listIterator();
			while (li.hasNext()) {
				oclSC sc = (oclSC)li.next();
				if (transSort.equals(sc.getSort())) {
					if (sc.transitionRHSRefersTo(refSort)) {
						return sc.getName();
					}
				}
			}
		}
		return null;
		
	}
	/**
	 * opTransReferencesLHS
	 * find the ID of the transition with the given sort that references the second
	 * given sort on the LHS of the transition
	 * @param opName
	 * @param transSort
	 * @param refSort
	 * @return
	 */
	public String opTransReferencesLHS(String opName,String transSort, String refSort){
		oclOperator op = getOperatorByFunctorName(opName);
		if (op != null) {
			ListIterator li = op.getNecessary().listIterator();
			while (li.hasNext()) {
				oclSC sc = (oclSC)li.next();
				if (transSort.equals(sc.getSort())) {
					if (sc.transitionLHSRefersTo(refSort)) {
						return sc.getName();
					}
				}
			}
		}
		return null;
		
	}

	/**
	 * seUnifiesWithHierarchicalStateDef
	 * find a state definition to unify with the given elements 
	 * of an sub-state expression in a hierarchical domain
	 * @param  name - the se Id
	 * @param  sort - the sort of the Id - the se expression
	 * @param strippedState - predicate list - stripped of ne and static predicates
	 * @return boolean - true if matching definition found
	 */
	public boolean seUnifiesWithHierarchicalStateDef(
		String name,
		String sort,
		List strippedState) {
		Utility.debugPrintln(
			"check states for sort " + sort + " with name " + name);
		oclSSClassDef def = null;

		ListIterator li = classDefs.listIterator();
		boolean foundDef = false;
		while (!foundDef && li.hasNext()) {
			def = (oclSSClassDef) li.next();
			if (sort.equals(def.getStateSort())) {
				// found correct state def
				foundDef = true;
			}
		}
		if (!foundDef) {
			String parent = "none";
			try {
				parent = findSortParent(sort);
			} catch (NoSuchElementException e) {
				Utility.debugPrintln("No parent for sort " + sort);
				if (strippedState.size() == 0)
					return true;
				else
					return false;
			}
			return seUnifiesWithHierarchicalStateDef(
				name,
				parent,
				strippedState);
		}
		return def.seUnifiesWithHierarchicalStateDef(this, name, strippedState);
	}

	/**
	 * ssUnifiesWithStateDef
	 * find a state definition to unify with the given elements 
	 * of an sub-state description
	 * @param  name - the ss/sc Id
	 * @param  sort - the sort of the Id - the ss/sc expression
	 * @param stripped- predicate list - stripped of static predicates
	 * @return boolean - true if matching definition found
	 */
	public boolean ssUnifiesWithStateDef(
		String name,
		String sort,
		List stripped) {
		if (isHierarchical()) {
			return ssUnifiesWithHierarchicalStateDef(name, sort, stripped,true);
		}
		oclSSClassDef def = null;

		ListIterator li = classDefs.listIterator();
		boolean foundDef = false;
		while (!foundDef && li.hasNext()) {
			def = (oclSSClassDef) li.next();
			if (sort.equals(def.getStateSort())) {
				// found correct state def
				foundDef = true;
			}
		}
		if (!foundDef) {
			return false;
		}
		return def.ssUnifiesWithStateDef(name, stripped);
	}

	/**
	 * ssUnifiesWithHierarchicalStateDef
	 * find a state definition to unify with the given elements 
	 * of an sub-state definition in a hierarchical domain
	 * @param  name - the ss Id
	 * @param  sort - the sort of the Id - the se expression
	 * @param strippedState - predicate list - stripped of ne and static predicates
	 * @param baseSortLevel - set to true if sort is the base sort 
	 *                  in the hierarchy for this transition i.e. definition must
	 *                  be present
	 * @return boolean - true if matching definition found
	 */
	public boolean ssUnifiesWithHierarchicalStateDef(
		String name,
		String sort,
		List strippedState,
		boolean baseSortLevel) {
		Utility.debugPrintln(
			"SS check states for sort " + sort + " with name " + name);
		oclSSClassDef def = null;

		ListIterator li = classDefs.listIterator();
		boolean foundDef = false;
		while (!foundDef && li.hasNext()) {
			def = (oclSSClassDef) li.next();
			if (sort.equals(def.getStateSort())) {
				// found correct state def
				foundDef = true;
			}
		}
		if (!foundDef) {
			String parent = "none";
			try {
				parent = findSortParent(sort);
			} catch (NoSuchElementException e) {
				Utility.debugPrintln("SS No parent for sort " + sort);
				if (strippedState.size() == 0)
					return true;
				else
					return false;
			}
			return ssUnifiesWithHierarchicalStateDef(
				name,
				parent,
				strippedState,
				false);
		}
		return def.ssUnifiesWithHierarchicalStateDef(this, name, strippedState,baseSortLevel);
	}

	/**
	 * seUsesStateDef
	 * find the state definition to unify with the given elements 
	 * of an sub-state expression and store name in consumer list
	 * @param  name - the se Id
	 * @param sort - the sort of the Id - the se expression
	 * @param strippedState - predicate list - stripped of ne and static predicates
	 * @param opId - op name/id
	 */
	public void seUsesStateDef(
		String name,
		String sort,
		List strippedState,
		String opId) {
		oclSSClassDef def = null;

		ListIterator li = classDefs.listIterator();
		boolean foundDef = false;
		while (!foundDef && li.hasNext()) {
			def = (oclSSClassDef) li.next();
			if (sort.equals(def.getStateSort())) {
				// found correct state def
				foundDef = true;
			}
		}
		if (!foundDef) {
			return;
		}
		def.seUsesStateDef(name, strippedState, opId);
	}

	/**
	 * ssUsesStateDef
	 * find a state definition to unify with the given elements 
	 * and store opId in producers list
	 * @param  name - the ss/sc Id
	 * @param  sort - the sort of the Id - the ss/sc expression
	 * @param stripped - predicate list - stripped of static predicates
	 * @param opID - of operator containing clause
	 */
	public void ssUsesStateDef(
		String name,
		String sort,
		List stripped,
		String opID) {
		oclSSClassDef def = null;

		ListIterator li = classDefs.listIterator();
		boolean foundDef = false;
		while (!foundDef && li.hasNext()) {
			def = (oclSSClassDef) li.next();
			if (sort.equals(def.getStateSort())) {
				// found correct state def
				foundDef = true;
			}
		}
		if (!foundDef) {
			return;
		}
		def.ssUsesStateDef(name, stripped, opID);
	}

	/**
	 * printStateUses 
	 * print the result of reachability analysis
	 * @param ps - to write to
	 */
	public void printStateUses(PrintWriter ps) {
		ps.println("Reachability analysis");
		ListIterator li = classDefs.listIterator();
		while (li.hasNext()) {
			oclSSClassDef cur = (oclSSClassDef) li.next();
			cur.printReachabilityAnalysis(ps);
		}
	}

	/**
	 * hasCondEffects
	 * check to see if domain as currently defined uses conditional effects
	 * @return boolean
	 */
	public boolean hasCondEffects() {
		ListIterator li = operators.listIterator();
		boolean found = false;

		while (!found && li.hasNext()) {
			oclOperator cur = (oclOperator) li.next();
			if (cur.hasCondEffects()) {
				found = true;
			}
		}
		return found;
	}

	/**
	 * hasHierMethods
	 * check to see if domain requires Hierarchical methods
	 * @return boolean
	 */
	public boolean hasHierMethods() {
		return (methods.size() > 0);
	}

	/**
	 * getHighestTaskNo
	 * if task ids are integers find the highest otherwise return 0
	 * @return int
	 */
	public int getHighestTaskNo() {
		ListIterator li = tasks.listIterator();
		int high = 0;
		int curID = 0;
		while (li.hasNext()) {
			oclTask cur = (oclTask) li.next();
			try {
				curID = Integer.parseInt(cur.ID);
				if (curID > high) {
					high = curID;
				}
			} catch (NumberFormatException e) {
			}
		}
		return high;
	}

	/**
	 * checkProcesses
	 * @return list of error messages
	 */
	public List checkProcesses() {
		List msgList = new ArrayList();
		List opNames = new ArrayList();
		boolean checkOK = true;
		boolean allChecksOK = true;

		ListIterator li = processes.listIterator();
		while (li.hasNext()) {
			oclProcess curOp = (oclProcess) li.next();
			checkOK = curOp.check(this, msgList);
			if (!checkOK) {
				allChecksOK = false;
			}
			if (Utility.listContainsString(curOp.opName.getName(),opNames)) {
				msgList.add("Duplicate Process Name " + curOp.opName.getName());
				allChecksOK = false;
			} else {
				opNames.add(curOp.opName.getName());
			}
		}

		if (!allChecksOK) {
			// Output to stdout for the moment
			li = msgList.listIterator();
			while (li.hasNext()) {
				Utility.debugPrintln((String) li.next());
			}
		} else {
			msgList.clear();
		}
		return msgList;
	}
	
	/**
	 * checkEvents
	 * @return list of error messages
	 */
	public List checkEvents() {
		List msgList = new ArrayList();
		List opNames = new ArrayList();
		boolean checkOK = true;
		boolean allChecksOK = true;

		ListIterator li = events.listIterator();
		while (li.hasNext()) {
			oclEvent curOp = (oclEvent) li.next();
			checkOK = curOp.check(this, msgList);
			if (!checkOK) {
				allChecksOK = false;
			}
			if (Utility.listContainsString(curOp.opName.getName(),opNames)) {
				msgList.add("Duplicate Event Name " + curOp.opName.getName());
				allChecksOK = false;
			} else {
				opNames.add(curOp.opName.getName());
			}
		}

		if (!allChecksOK) {
			// Output to stdout for the moment
			li = msgList.listIterator();
			while (li.hasNext()) {
				Utility.debugPrintln((String) li.next());
			}
		} else {
			msgList.clear();
		}
		return msgList;
	}
	// Ron 15/4/02 XXXX Need to deal with HTN TASKS STILL TO BE DONE
	// This method should look tasks and htntasks to decide which to check
	// Ron 8/11/02 Started this task
	/**
	 * checkTasks
	 * check that all initial states are fully instantiated and
	 * unify with a state definition for the object
	 * check that goal states are valid sub-state expressions and
	 * that for each goal state there is an initial state for that
	 * object
	 * @return List - message list
	 */
	public List checkTasks() {
		List mssgs = new ArrayList();
		ListIterator li = null;
		if (htntasks.size() != 0) {
			li = htntasks.listIterator();
			while (li.hasNext()) {
				oclHTNTask cur = (oclHTNTask) li.next();
				mssgs.add(" Checking task ID " + cur.ID);
				cur.check(this, mssgs);
			}
			if (mssgs.size() == htntasks.size()) {
			// No real messages
				mssgs.clear();
			}
		} else {
			li = tasks.listIterator();
			while (li.hasNext()) {
				oclTask cur = (oclTask) li.next();
				mssgs.add(" Checking task ID " + cur.ID);
				cur.check(this, mssgs);
			}
			if (mssgs.size() == tasks.size()) {
				// No real messages
				mssgs.clear();
			}
		}
		return mssgs;
	}

//	/**
//	 * findPattern
//	 *   locate a pattern from the patterns list
//	 * @param type - the pattern type
//	 * @param name - the pattern name
//	 * @return - the pattern found
//	 * @throws jplan.general.OCLException - OCLException if pattern not found
//	 */
//	public oclPattern findPattern(String type, String name)
//		throws jplan.general.OCLException {
//		ListIterator li = patterns.listIterator();
//		while (li.hasNext()) {
//			oclPattern curPat = (oclPattern) li.next();
//			if (type.equals(curPat.getPatternType())
//				&& name.equals(curPat.getName())) {
//				return curPat;
//			}
//		}
//		throw new jplan.general.OCLException(
//			"Cannot find pattern " + type + "/" + name);
//	}
//
//	/**
//	 * findPatternsOfType
//	 *     find all the patterns of a given type
//	 * @param type - the pattern type
//	 * @return - the list of matching patterns
//	 */
//	public List findPatternsOfType(String type) {
//		List matches = new ArrayList();
//		ListIterator li = patterns.listIterator();
//		while (li.hasNext()) {
//			oclPattern curPat = (oclPattern) li.next();
//			if (type.equals(curPat.getPatternType())) {
//				matches.add(curPat);
//			}
//		}
//		return matches;
//	}
//
//	/**
//	* findPatternsOfType
//	*     find all the patterns of a given type
//	* @param type - the pattern type
//	* @return - the list of matching pattern names
//	*/
//	public List findPatternNamesOfType(String type) {
//		List matches = new ArrayList();
//		ListIterator li = patterns.listIterator();
//		while (li.hasNext()) {
//			oclPattern curPat = (oclPattern) li.next();
//			if (type.equals(curPat.getPatternType())) {
//				matches.add(curPat.getName());
//			}
//		}
//		return matches;
//	}

	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		ps.println("/**");
		ps.println(
			" *  All rights reserved. Use of this software is permitted for non-commercial");
		ps.println(
			" *  research purposes, and it may be copied only for that use.  All copies must");
		ps.println(
			" *  include this copyright message.  This software is made available AS IS, and");
		ps.println(
			" *  neither the GIPO team nor the University of Huddersfield make any warranty");
		ps.println(" *  about the software or its performance.");
		ps.println(" *");
		ps.println(
			" *  Automatically generated OCL Domain from  GIPO Version 3.0");
		ps.println(" *");
		ps.println(" *  Author: " + author);
		ps.println(" *  Institution: " + institution);
		ps.println(" *  Date created: " + dateCreated);
		ps.println(" *  Date last modified: " + dateModified);
		ps.println(" *  Description:");
		ListIterator li = domDesc.listIterator();
		while (li.hasNext()) {
			ps.println(" *    " + (String) li.next());
		}
		if (! lifeHistoryFileName.equals("none")) {
			ps.println(" * Object Life History : " + lifeHistoryFileName);
		}
//		if (patterns.size() > 0) {
//			ps.println(" *");
//			ps.println(" * Implements the following Generic Types");
//			ListIterator liPats = patterns.listIterator();
//			while (liPats.hasNext()) {
//				oclPattern pat = (oclPattern) liPats.next();
//				pat.oclPrintComponent(ps, 0, true);
//			}
//		}
		ps.println(" */\n");
		ps.println("domain_name(" + name + ").");
		if (isHierarchical()) {
			ps.println("\noption(hierarchical).");
		}
		if (oclPlus) {
			ps.println("\noption(oclPlus).");
		}
		ps.println("\n% Sorts");
		li = sorts.listIterator();
		while (li.hasNext()) {
			((oclSort) li.next()).oclPrintComponent(ps, indent, nline);
		}
		ps.println("\n% Objects");
		li = objects.listIterator();
		while (li.hasNext()) {
			((oclObject) li.next()).oclPrintComponent(ps, indent, nline);
		}
		ps.println("\n% Predicates");
		if (predicates.size() > 0) {
			ps.println("predicates([");
			li = predicates.listIterator();
			while (li.hasNext()) {
				((oclPredicate) li.next()).oclPrintComponent(ps, 4, false);
				if (li.hasNext())
					ps.println(",");
			}
			ps.println("]).");
		}
		if (oclPlus) {
			ps.println("\n% Functors");
			if (functors.size() > 0) {
				ps.println("functors([");
				li = functors.listIterator();
				while (li.hasNext()) {
					((oclPredicate) li.next()).oclPrintComponent(ps, 4, false);
					if (li.hasNext())
						ps.println(",");
				}
				ps.println("]).");
			}
		}
		ps.println("\n% Object Class Definitions");
		li = classDefs.listIterator();
		while (li.hasNext()) {
			((oclSSClassDef) li.next()).oclPrintComponent(ps, indent, nline);
		}
		ps.println("\n% Atomic Invariants");
		if (atomicInvars.size() > 0) {
			ps.println("atomic_invariants([");
			// Ron 1/7/03 display fluent values in oclPlus
			if (oclPlus){
				li = atomicInvars.listIterator();
				while (li.hasNext()) {
					oclPredicate cur = (oclPredicate)li.next();
					if (cur.isFluent()) {
						((oclFunctor)cur).oclPrintFluent(ps,4,false);
					} else {
						cur.oclPrintComponent(ps, 4, false);
					}
					if (li.hasNext())
						ps.println(",");
				}
				ps.println("]).");
			} else {
				li = atomicInvars.listIterator();
				while (li.hasNext()) {
					((oclPredicate) li.next()).oclPrintComponent(ps, 4, false);
					if (li.hasNext())
						ps.println(",");
				}
				ps.println("]).");
			}
		}
		ps.println("\n% Implied Invariants");
		li = impliedInvars.listIterator();
		while (li.hasNext()) {
			((oclImpliedInvar) li.next()).oclPrintComponent(ps, 0, false);
		}
		ps.println("\n% Inconsistent Constraints");
		li = inconsistentConst.listIterator();
		while (li.hasNext()) {
			((oclInconsistentConst) li.next()).oclPrintComponent(ps, indent, nline);
		}
		ps.println("\n% Operators");
		li = operators.listIterator();
		while (li.hasNext()) {
			((oclOperator) li.next()).oclPrintComponent(ps, indent, nline);
		}
		if (oclPlus) {
			ps.println("\n% Processes");
			if (processes.size() > 0) {
				li = processes.listIterator();
				while (li.hasNext()) {
					((oclProcess) li.next()).oclPrintComponent(ps, indent, nline);
				}
			}
			ps.println("\n% Events");
			if (events.size() > 0) {
				li = events.listIterator();
				while (li.hasNext()) {
					((oclEvent) li.next()).oclPrintComponent(ps, indent, nline);
				}
			}
		}
		ps.println("\n% Methods");
		li = methods.listIterator();
		while (li.hasNext()) {
			((oclMethod) li.next()).oclPrintComponent(ps, indent, nline);
		}
		ps.println("\n% Domain Tasks");
		li = tasks.listIterator();
		while (li.hasNext()) {
			((oclTask) li.next()).oclPrintComponent(ps, indent, nline);
		}
		if (htntasks.size() > 0) {
			ps.println("\n% HTN Domain Tasks");
			li = htntasks.listIterator();
			while (li.hasNext()) {
				((oclHTNTask) li.next()).oclPrintComponent(ps, indent, nline);
			}
		}
	}
	
	public void pddlPrintDomain(PrintWriter ps){
		pddlPrintDomainBody(ps);
		pddlPrintDomainTasks(ps,null);
	}
	
	/**
	 * pddlPrintDomain
	 * translate domain to PDDL and print
	 * @param ps - the PrintWriter to print to
	 */
	public void pddlPrintDomainBody(PrintWriter ps) {
		ps.println("; ***************************************************************************");
		ps.println(
				"; *  All rights reserved. Use of this software is permitted for non-commercial");
		ps.println(
				"; *  research purposes, and it may be copied only for that use.  All copies must");
		ps.println(
				"; *  include this copyright message.  This software is made available AS IS, and");
		ps.println(
				"; *  neither the GIPO team nor the University of Huddersfield make any warranty");
		ps.println("; *  about the software or its performance.");
		ps.println("; *");
		ps.println(
				"; *  Automatically generated PDDL Domain from  GIPO Version 3.0");
		ps.println("; *");
		ps.println("; *  Author: " + author);
		ps.println("; *  Institution: " + institution);
		ps.println("; *  Date created: " + dateCreated);
		ps.println("; *  Date last modified: " + dateModified);
		ps.println("; *  Description:");
		ListIterator li = domDesc.listIterator();
		while (li.hasNext()) {
			ps.println("; *    " + (String) li.next());
		}
		if (ocledEnv.osDiags.getOclFile() != null) {
			ps.println("; * OCL File name : " + ocledEnv.osDiags.getOclFile().getName());
		}
		if (! lifeHistoryFileName.equals("none")) {
			ps.println("; * Object Life History : " + lifeHistoryFileName);
		}
		ps.println("; *************************************************************************\n");
		ps.println("(define (domain " + name + ")");
		ps.print("  (:requirements :strips :equality :typing");
		if (hasCondEffects()) {
			ps.print(" :conditional-effects");
		}
		if (isOclPlus()) {
			ps.print(" :time");	
		}
		ps.println(")");
		ps.print("\n  (:types ");
		li = sorts.listIterator();
		while (li.hasNext()) {
			oclSort curSort = (oclSort)li.next();
			if (curSort.getSortName().equals("primitive_sorts")) {
				List subs = curSort.getSubTypes();
				ListIterator liSubs = subs.listIterator();
				while (liSubs.hasNext()) {
					String sName = (String)liSubs.next();
					ps.print(" " + sName);
				}
			} else {
				List subs = curSort.getSubTypes();
				ps.print("\n     " + curSort.getSortName() + " - (either");
				ListIterator liSubs = subs.listIterator();
				while (liSubs.hasNext()){
					String thisSort = (String)liSubs.next();
					List prims = getPrimitiveSubTypes(thisSort);
					if (prims.size() ==0) {
						ps.print(" " + thisSort);
					}
					ListIterator liPrims = prims.listIterator();
					while (liPrims.hasNext()) {
						ps.print(" " + (String)liPrims.next());
					}
				}
				ps.print(") ");
				
			}
		}
		ps.println(")\n");
		if (predicates.size() > 0) {
			ps.println("\n  (:predicates");
			li = predicates.listIterator();
			while (li.hasNext()) {
				((oclPredicate) li.next()).pddlPrintPrototype(this, ps, 4, true);
			}
			ps.println("  )");
		}
		if (isOclPlus()) {
			if (functors.size() > 0) {
				ps.println("\n  (:functors");
				li = functors.listIterator();
				while (li.hasNext()) {
					((oclPredicate) li.next()).pddlPrintPrototype(this, ps, 4, true);
				}
				ps.println("  )");
			}
		}
		if (operators.size() > 0) {
			li = operators.listIterator();
			while (li.hasNext()) {
				((oclOperator) li.next()).pddlPrint(this, ps, 4, true);
			}
		}
		if (isOclPlus() && events.size() > 0) {
			li = events.listIterator();
			while (li.hasNext()) {
				((oclEvent) li.next()).pddlPrint(this, ps, 4, true);
			}
		}
		if (isOclPlus() && processes.size() > 0) {
			li = processes.listIterator();
			while (li.hasNext()) {
				((oclProcess) li.next()).pddlPrint(this, ps, 4, true);
			}
		}
		ps.println("  )");
	}
	
	/**
	 * translate and print the domain tasks
	 * @param ps - the stream to print to
	 * @param taskID - if null print all tasks otherwise the specified task
	 */
	public void pddlPrintDomainTasks(PrintWriter ps, String taskID) {
		// That is the end of the domain specification
		// Task specifications
		if (taskID == null) {
			ListIterator li = tasks.listIterator();
			while (li.hasNext()){
				oclTask task = (oclTask)li.next();
				task.pddlPrintTask(this, ps);
			}
		} else {
			boolean found = false;
			ListIterator li = tasks.listIterator();
			while (li.hasNext()){
				oclTask task = (oclTask)li.next();
				if (taskID.equals(task.ID)) {
					task.pddlPrintTask(this, ps);
					found = true;
				}
			}
		}
	}

	public void htmlPrint(PrintWriter ps, int indent, boolean nline) {
		ListIterator li;
		ps.println("<HTML>");
		ps.println("<HEAD>");
		ps.println("<TITLE> OCL DOMAIN DESCRIPTION </TITLE>");
		ps.println("</HEAD>");
		ps.println("<BODY>");
		ps.println(
			O2HTML.H3(
				O2HTML.green(
					"% OCL Domain description for the " + name + " domain")));
		ps.print(
			"<PRE>"
				+ O2HTML.red("domain_name(")
				+ name
				+ O2HTML.red(").</PRE>"));
		ps.println(O2HTML.H4(O2HTML.green("% Sorts")));
		li = sorts.listIterator();
		while (li.hasNext()) {
			((oclSort) li.next()).htmlPrint(ps, indent, nline);
		}
		ps.println(O2HTML.H4(O2HTML.green("% Objects")));
		li = objects.listIterator();
		ps.print("<PRE>");
		while (li.hasNext()) {
			((oclObject) li.next()).htmlPrint(ps, indent, nline);
		}
		ps.print("</PRE>");
		ps.println(O2HTML.H4(O2HTML.green("% Predicates")));
		ps.println("<PRE>" + O2HTML.red("predicates(["));
		li = predicates.listIterator();
		while (li.hasNext()) {
			((oclPredicate) li.next()).htmlPrint(ps, 4, false);
			if (li.hasNext()) {
				ps.println(",");
			}
		}
		ps.println(O2HTML.red("]).</PRE>"));
		ps.println("</BODY>");
		ps.println("</HTML>");
	}

	/* WZ 20/8/02 */
	/* Weihong added on 28/06/2001 */
	// Ron 29/9/01 Try to deal with is of sort clauses
	public List instantiateStateList(
		String stateId,
		List list_StateList,
		String newName) {
		List returnList = new ArrayList();
		oclStateList stateList;
		ListIterator li = list_StateList.listIterator();
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			stateList = new oclStateList();
			ListIterator lii = cur.getPredicateList().listIterator();
			while (lii.hasNext()) {
				oclPredicate opd = (oclPredicate) lii.next();
				if (!opd.getName().equals("ne")
					&& !opd.getName().equals("is_of_sort")) {
					if (!opd.isStatic()) {
						//to instantiate predicate
						try {
							// 			    oclPredicate opdClone = (oclPredicate)opd.clone();
							// 			    opdClone.replaceVariableNameByName(stateId, curObject);
							// 			    stateList.addPredicate(opdClone);
							opd.replaceVariableNameByName(stateId, newName);
							/* WZ 14/8/02 */
						} catch (Exception e) {
						}
					}
				}
			}
			// 	    returnList.add(stateList);/* WZ 14/8/02 */
		}
		// 	return returnList;/* WZ 14/8/02 */
		return list_StateList; /* WZ 14/8/02 */
	}
}

class OCLException extends Exception {
	public OCLException() {
		super();
	}
	public OCLException(String s) {
		super(s);
	}
}

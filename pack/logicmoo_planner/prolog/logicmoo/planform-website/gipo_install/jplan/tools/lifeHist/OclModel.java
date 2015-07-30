/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 * 
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both the
 * copyright notice and this permission notice and warranty disclaimer appear in
 * supporting documentation, and that the names of the authors or their
 * employers not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 * 
 * The authors and their employers disclaim all warranties with regard to this
 * software, including all implied warranties of merchantability and fitness. In
 * no event shall the authors or their employers be liable for any special,
 * indirect or consequential damages or any damages whatsoever resulting from
 * loss of use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 * 
 * 
 * 
 * Created on 16-Oct-2004
 * 
 * Author ron
 *  
 */

package jplan.tools.lifeHist;
import javax.swing.JOptionPane;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.Map;
import java.util.Iterator;
import java.util.Enumeration;
import java.util.Hashtable;
import org.jgraph.graph.GraphModel;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.DefaultEdge;
import jplan.ocl.*;
import jplan.general.OCLException;
import jplan.general.Utility;
import jplan.general.OPredicate;
/**
 * @author ron
 * 
 * This is the Life History Graph to OCL translator
 */

public class OclModel {
	oclDomain curDom = null;
	oclDomain tempDom = null; // Used while building new domain - copied to
	// curDom
	// if no problems
	/**
	 * constructor
	 * 
	 * @param dom
	 *            the current domain - only the name and comments will be
	 *            preserved - everything else is over written
	 */
	public OclModel(oclDomain dom) {
		curDom = dom;
		tempDom = new oclDomain(false); // This is not a hierarchical domain
	}
	
	/**
	 * toOCL translate the graph model to an OCL spec
	 * 
	 * @param model -
	 *            the graph model
	 * @throws OCLException
	 */
	public void toOCL(GraphModel model) throws OCLException {
		tempDom = new oclDomain(false);
		Hashtable hashValueArgs = new Hashtable();
		Hashtable hashStatePreds = new Hashtable();
		Hashtable hashProperties = new Hashtable();
		Hashtable hashAssociatedArgs = new Hashtable();
		List simpleTransitions = new ArrayList();
		List valueTransitions = new ArrayList();
		List mergeEdges = new ArrayList();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				String sortName = LHUserObject.getObjectSort((StateCell)curRoot);
				String stateName = ((StateCell) curRoot).toString();
				// Put the state details in a hash table
				List stateList = (List) hashStatePreds.get(sortName);
				if (stateList == null) {
					stateList = new ArrayList();
					stateList.add(stateName);
					hashStatePreds.put(sortName, stateList);
				} else {
					stateList.add(stateName);
				}
				collectStateProperties((LHUserObject)((StateCell)curRoot).getUserObject(),sortName,hashProperties);
			} else if (TransitionCell.isStateTransitionCell(curRoot)) {
				simpleTransitions.add(curRoot);
			} else if (TransitionCell.isValueTransitionCell(curRoot)) {
				valueTransitions.add(curRoot);
				collectValueArgs((TransitionCell) curRoot, hashValueArgs);
			} else if (curRoot instanceof MergeEdge) {
				collectAssociationArgs((MergeEdge)curRoot,hashAssociatedArgs);
				mergeEdges.add(curRoot);
			}
		}
		List allTransitions = new ArrayList();
		allTransitions.addAll(simpleTransitions);
		allTransitions.addAll(valueTransitions);
		allTransitions = topSortTransitions(allTransitions,mergeEdges);
		simpleTransitions = stripValueTransitions(allTransitions);
		buildDomain(tempDom, hashStatePreds, hashValueArgs, hashProperties, hashAssociatedArgs, simpleTransitions,
				valueTransitions, mergeEdges);
		exchangeDomainStructure(tempDom);
	}
	
	protected List stripValueTransitions(List all){
		List ret = new ArrayList();
		Iterator li = all.listIterator();
		while (li.hasNext()) {
			TransitionCell cell = (TransitionCell)li.next();
			if (!cell.isValueTransition()) {
				ret.add(cell);
			}
		}
		return ret;
	}
	
	protected List topSortTransitions(List transitions,List mergEdges){
		System.out.println("NUMBER TRANSITIONS = " + transitions.size());
		List sorted = new ArrayList(transitions.size());
		Object[] arrTrans = transitions.toArray();
		int[] refCount = new int[arrTrans.length];
		boolean[] visited = new boolean[arrTrans.length];
		for (int i = 0; i < arrTrans.length;i++){
			refCount[i] = countMergesOut((TransitionCell)arrTrans[i]);
			visited[i] = false;
		}
		System.out.println("Before topSort");
		for (int i = 0; i < arrTrans.length;i++){
			TransitionCell cell = (TransitionCell)arrTrans[i];
			System.out.println("RefCount = " + refCount[i] + "  " + cell.describe());
		}
		int count = 0;
		int attempts = 0;
		while (count < arrTrans.length) {
			for (int inx = 0; inx < arrTrans.length;inx++ ){
				if (refCount[inx] == 0 && !visited[inx]) {
					visited[inx] = true;
					sorted.add(sorted.size(),arrTrans[inx]);
					decrementCounts((TransitionCell)arrTrans[inx],arrTrans,refCount);
					count++;
				}
			}
			if (attempts++ > arrTrans.length) {
				for (int inx = 0; inx < arrTrans.length;inx++ ){
					if (!visited[inx]) {
						TransitionCell cell = (TransitionCell)arrTrans[inx];
						System.out.println("ERROR  RefCount = " + refCount[inx] + "  " + cell.describe());
					}
				}
				System.out.println("ERROR Cycle in merger edges");
				return transitions;
			}
		}
		Iterator li = sorted.listIterator();
		while (li.hasNext()){
			TransitionCell cell = (TransitionCell)li.next();
			System.out.println("Cell = " + cell.describe());
		}
		return sorted;
	}
	
	private void decrementCounts(TransitionCell cell,Object[] allTrans,int[] refCount){
		Enumeration enum = cell.children();
		while ( enum.hasMoreElements() ) {
			DefaultPort port = (DefaultPort)enum.nextElement();
			Iterator li = port.edges();	
			while (li.hasNext()) {
				DefaultEdge edge = (DefaultEdge)li.next();
				if (edge instanceof MergeEdge) {
					DefaultGraphCell connTrans = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
					if (connTrans == cell) {
						//This is an incomming edge
						DefaultGraphCell source = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
						if (source instanceof TransitionCell) {
							int inx = 0;
							boolean found = false;
							while (!found && inx < allTrans.length ) {
								if (source == allTrans[inx]) {
									refCount[inx] = refCount[inx] - 1;
									found = true;
								}
								inx++;
							}
							if (!found) {
								System.out.println("ERROR Cannot find merge source" +((TransitionCell)source).describe());
							}
						}
					}
				}
			}
		}
	}
	
	private int countMergesOut(TransitionCell cell){
		int ret = 0;
		Enumeration enum = cell.children();
		while ( enum.hasMoreElements() ) {
			DefaultPort port = (DefaultPort)enum.nextElement();
			Iterator li = port.edges();	
			while (li.hasNext()) {
				DefaultEdge edge = (DefaultEdge)li.next();
				if (edge instanceof MergeEdge) {
					DefaultGraphCell connTrans = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
					if (connTrans == cell) {
						//This is an out going merge edge
						ret++;		
					}
				}
			}
		}
		return ret;	
	}
	/**
	 * collectStateProperties
	 * collect the property predicates associated with this object sort
	 * @param userObj
	 * @param objSort
	 * @param hashProps
	 */
	protected void collectStateProperties(LHUserObject userObj,String objSort,Hashtable hashProps){
		String strProp = "Property";
		String strArg = "Property Type";
		int suffix = 2;
		List objProps = null;
		String propName = (String) userObj.getProperty(strProp);
		while (propName != null) {
			String propArgSort = (String) userObj.getProperty(strArg);
			Property prop = new Property(objSort,propName,propArgSort);
			objProps = (List)hashProps.get(objSort);
			if (objProps == null) {
				objProps = new ArrayList();
				objProps.add(prop);
			} else {
				if (!listContainsProperty(objProps,prop)) {
					objProps.add(prop);
				}
			}
			propName = (String) userObj.getProperty("Property" + suffix);
			strArg = "Property Type" + suffix++;
		}
		if (objProps != null) {
			hashProps.put(objSort,objProps);
		}
	}
	/**
	 * listContainsProperty
	 * find given property record in the list of properties.
	 * @param props
	 * @param prop
	 * @return
	 */
	private boolean listContainsProperty(List props,Property prop) {
		Iterator li = props.listIterator();
		while (li.hasNext()) {
			Property curProp = (Property)li.next();
			if (curProp.name.equals(prop.name) && curProp.argType.equals(prop.argType)) {
				return true;
			}
		}
		return false;
	}
	/**
	 * collectValueArgs find all additional state arguments introduced by this
	 * value transition
	 * 
	 * @param node -
	 *            the value transition node
	 * @param hashValueArgs -
	 *            hash table with state sorts as keys and lists of additional
	 *            arguments
	 */
	protected void collectValueArgs(TransitionCell node, Hashtable hashValueArgs) {
		String valueSort = (String) ((LHUserObject) node.getUserObject())
				.getProperty("ValueSort");
		String sortName = (String) ((LHUserObject) node.getUserObject())
				.getProperty("ObjectSort");
		List argList = (List) hashValueArgs.get(sortName);
		if (argList == null) {
			argList = new ArrayList();
			argList.add(valueSort);
			hashValueArgs.put(sortName, argList);
		} else if (!argList.contains(valueSort)) {
			argList.add(valueSort);
		}
		// Are there multiple value changes in this transition
		int suffix = 2;
		boolean look = true;
		while (look) {
			valueSort = (String) ((LHUserObject) node.getUserObject())
					.getProperty("ValueSort" + suffix);
			if (valueSort != null) {
				suffix++;
				argList.add(valueSort);
			} else {
				look = false;
			}
		}
	}
	
	/**
	 * collectAssociationArgs
	 * build a hash table of all additional state association variables
	 * table key state names 
	 * value list of variable/object names
	 * @param edge the start merge edge
	 * @param hash - the table to populate
	 */
	protected void collectAssociationArgs(MergeEdge edge,Hashtable hash){
		String label = (String)((LHUserObject)edge.getUserObject()).getProperty("label");
		if (label != null && label.startsWith("+")) {
			Utility.debugPrintln("patterns","FOUND Label " + label);
			DefaultGraphCell dest = edge.getDestinationCell();
			DefaultGraphCell source = edge.getSourceCell();
			String associationSort = null;
			if (source instanceof TransitionCell) {
				associationSort = ((TransitionCell)source).getObjectSort();
			} else if (source instanceof StateCell) {
				// Must be one or other
				associationSort = ((StateCell)source).getObjectSort();
			}
			if (dest instanceof TransitionCell) {
				// Must be otherwise I am not enforcing the rules
				// If this is a disjunctive transition only trace
				// one branch - both should give same result
				// But I don't yet check that
				StateCell stateDest = getStateDest(dest);
				if (!disjunctionDone((TransitionCell)dest,stateDest.getLabel(),hash)) {
					addStateAssociation(stateDest,associationSort,hash);
					List search = new ArrayList();
					List visited = new ArrayList();
					search.add(stateDest);
					bfTraceAssociation(search,associationSort,hash,visited);
				}
			}
		}	
	}
	/**
	 * disjunctionDone
	 * check that if this is a disjunction transition has the association 
	 * already been recorded
	 * @param trans
	 * @param destStateName
	 * @param hash -
	 *           hashtable linking statenames with association arguments
	 * @return
	 */
	private boolean disjunctionDone(TransitionCell trans, String destStateName, Hashtable hash) {
		if (trans.isDisjunction()){
			List assocs = (List)hash.get(destStateName);
			if (assocs == null)
				return false;
			else
				return true;
		}
		return false;
	}
	
	/**
	 * bfTraceAssociation follow transitions from given state node
	 * does a breadth first search
	 * @param pending - List of states to be explored
	 * @param plusArg
	 * @param hash
	 * @param visited - list of visited states
	 */
	private void bfTraceAssociation(List pending,String plusArg,Hashtable hash,List visited){
		boolean done = false;
		while (!done) {
			List nextLevel = new ArrayList();
			Iterator li = pending.listIterator();
			while (li.hasNext()) {
				StateCell state = (StateCell)li.next();
				visited.add(state);
				List transOut = getOutwardTransitions(state);
				Iterator liTrans = transOut.listIterator();
				while (liTrans.hasNext()) {
					TransitionCell trans = (TransitionCell)liTrans.next();
					if (! removesAssociation(trans, plusArg)) {
						StateCell dest = getStateDest(trans);
						if (!visited.contains(dest)) {
							nextLevel.add(dest);
							//This state needs the added argument
							addStateAssociation(dest,plusArg,hash);
						}
					}
				}
			}
			if (nextLevel.size() == 0) {
				done = true;
			} else {
				pending.clear();
				pending.addAll(nextLevel);
			}
		}		
	}
	
	/**
	 * removesAssociation
	 * look for a connecting merge edge with a minus label removing the plusArg
	 * @param trans
	 * @param plusArg
	 * @return 
	 */
	private boolean removesAssociation(TransitionCell trans, String plusArg){
		List merges = getTransitionMerges(trans);
		Iterator li = merges.listIterator();
		while (li.hasNext()) {
			MergeEdge edge = (MergeEdge)li.next();
			String label = (String)((LHUserObject)edge.getUserObject()).getProperty("label");
			if (label != null && label.startsWith("-")) {
				DefaultGraphCell cell = edge.getSourceCell();
				if (cell instanceof TransitionCell) {
					String objSort = ((TransitionCell)cell).getObjectSort();
					if (plusArg.equals(objSort)) {
						return true;
					}
				} else if (cell instanceof StateCell) {
					String objSort = ((StateCell)cell).getObjectSort();
					if (plusArg.equals(objSort)) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	/**
	 * addStateAssociation
	 * record statename object ID association in hash table
	 * @param state
	 * @param plusArg
	 * @param hash
	 */
	private void addStateAssociation(StateCell state, String plusArg, Hashtable hash){
		LHUserObject userObj = (LHUserObject)((StateCell)state).getUserObject();
		String stateName = (String)userObj.getProperty("label");
		List val = (List)hash.get(stateName);
		if (val == null) {
			List vals = new ArrayList();
			vals.add(plusArg);
			hash.put(stateName,vals);
		} else if (!val.contains(plusArg)){
			// Only add an association one for the same type 
			// TODO this could be wrong but not sure where we might require multiple associations
			val.add(plusArg);
		}
	}
	
	/**
	 * buildDomain take the classified graph nodes and sort associations (hash
	 * tables) and build the ocl spec
	 * 
	 * @param tempDom-
	 *            empty domain to populate
	 * @param hashStates -
	 *            sort statenames assocuiations
	 * @param hashArgs -
	 *            sort value args associations
	 * @param hashProperties
	 *            sort value Properties associations
	 * @param hashAssocs 
	 * 			  Association arguments indexed by state name
	 * @param simpleTransitions -
	 *            the non value transforming transition names
	 * @param valueTransitions -
	 *            the value changing transition names
	 * @param mergeEdges -
	 *            list of merge graph edges
	 */
	protected void buildDomain(oclDomain tempDom, Hashtable hashStates,
			Hashtable hashArgs, Hashtable hashProperties, 
			Hashtable hashAssocs, 
			List simpleTransitions, List valueTransitions,
			List mergeEdges) {
		oclSort prims = tempDom.addSort("primitive_sorts");
		//Hashtable hashStateMergeArgs = new Hashtable();
		//buildMergeArgsMap(mergeEdges, hashStateMergeArgs);
		Set sortKeys = hashStates.keySet();
		Iterator li = sortKeys.iterator();
		while (li.hasNext()) {
			String sortName = (String) li.next();
			oclSSClassDef def = tempDom.getStates(sortName);
			if (def == null) {
				if (!prims.isSubType(sortName)) {
					prims.addSubType(sortName);
				}
				def = tempDom.addClassDef(sortName, oclPredicate.toVar(sortName));
			}
			List states = (List) hashStates.get(sortName);
			Iterator liStates = states.listIterator();
			while (liStates.hasNext()) {
				String stateName = (String) liStates.next();
				oclPredicate curStatePred = tempDom.addPredicate(stateName);
				curStatePred.addConstArgument(sortName);
// Now trying to do this with properties
//				// Now get the additional arguments
//				List args = (List) hashArgs.get(sortName);
//				if (args != null) {
//					Iterator liArgs = args.listIterator();
//					while (liArgs.hasNext()) {
//						String argSort = (String) liArgs.next();
//						curStatePred.addConstArgument(argSort);
//					}
//				}
				// Deal with association arguments
				List mergeArgs = (List) hashAssocs.get(stateName);
				//List mergeArgs = (List) hashStateMergeArgs.get(stateName);
				if (mergeArgs != null) {
					// We have some merges
					Iterator liArgs = mergeArgs.listIterator();
					while (liArgs.hasNext()) {
						String baseArg = (String) liArgs.next();
						curStatePred.addConstArgument(baseArg);
					}
				}
				oclStateList sList = def.addState();
				oclPredicate cur = curStatePred.promoteSortsToVars();
				sList.addPredicate(cur.destinguishVars());
				addStateProperties(tempDom,prims,sList,sortName,hashProperties);
			}
		}
		buildSimpleTransitions(tempDom, simpleTransitions, mergeEdges);
		buildValueTransitions(tempDom, valueTransitions, mergeEdges);
	}
	
	
//	protected void buildDomain(oclDomain tempDom, Hashtable hashStates,
//			Hashtable hashArgs, Hashtable hashProperties, Hashtable hashAssocs, 
//			List simpleTransitions, List valueTransitions,
//			List mergeEdges) {
//		oclSort prims = tempDom.addSort("primitive_sorts");
//		//Hashtable hashStateMergeArgs = new Hashtable();
//		//buildMergeArgsMap(mergeEdges, hashStateMergeArgs);
//		Set sortKeys = hashStates.keySet();
//		Iterator li = sortKeys.iterator();
//		while (li.hasNext()) {
//			String sortName = (String) li.next();
//			oclSSClassDef def = tempDom.getStates(sortName);
//			if (def == null) {
//				prims.addSubType(sortName);
//				def = tempDom.addClassDef(sortName, oclPredicate.toVar(sortName));
//			}
//			List states = (List) hashStates.get(sortName);
//			Iterator liStates = states.listIterator();
//			while (liStates.hasNext()) {
//				String stateName = (String) liStates.next();
//				oclPredicate curStatePred = tempDom.addPredicate(stateName);
//				curStatePred.addConstArgument(sortName);
//				// Now get the additional arguments
//				List args = (List) hashArgs.get(sortName);
//				if (args != null) {
//					Iterator liArgs = args.listIterator();
//					while (liArgs.hasNext()) {
//						String argSort = (String) liArgs.next();
//						curStatePred.addConstArgument(argSort);
//					}
//				}
//				// Deal with association arguments
//				List mergeArgs = (List) hashAssocs.get(stateName);
//				//List mergeArgs = (List) hashStateMergeArgs.get(stateName);
//				if (mergeArgs != null) {
//					// We have some merges
//					Iterator liArgs = mergeArgs.listIterator();
//					while (liArgs.hasNext()) {
//						String baseArg = (String) liArgs.next();
//						curStatePred.addConstArgument(baseArg);
//					}
//				}
//				oclStateList sList = def.addState();
//				oclPredicate cur = curStatePred.promoteSortsToVars();
//				sList.addPredicate(cur.destinguishVars());
//				addStateProperties(tempDom,prims,sList,sortName,hashProperties);
//			}
//		}
//		buildSimpleTransitions(tempDom, simpleTransitions, mergeEdges);
//		buildValueTransitions(tempDom, valueTransitions, mergeEdges);
//	}
	protected void addStateProperties(oclDomain tempDom,oclSort prims,oclStateList sList,String sortName,Hashtable hashProperties){
		List props = (List)hashProperties.get(sortName);
		if (props != null) {
			Iterator li = props.listIterator();
			while (li.hasNext()){
				Property prop = (Property)li.next();
				if (!tempDom.isSort(prop.argType)) {
					prims.addSubType(prop.argType);
				}
				oclPredicate pred = null;
				if ((pred = tempDom.getPredicateByName(prop.name)) == null) {
					pred = tempDom.addPredicate(prop.name);
					pred.addConstArgument(sortName);
					pred.addConstArgument(prop.argType);
				}
				oclPredicate cur = pred.promoteSortsToVars();
				sList.addPredicate(cur);
			}
		}
	}
	/**
	 * buildMergeArgsMap build a hash table with keys for state names and values
	 * of lists of added arguments imported from other object types to the state
	 * predicate
	 * 
	 * @param mergeEdges -
	 *            a list of all the merge edges in the domain
	 * @param hashStateMergeArgs -
	 *            the hashtable to be built
	 */
	private void buildMergeArgsMap(List mergeEdges, Hashtable hashStateMergeArgs) {
		Iterator li = mergeEdges.listIterator();
		while (li.hasNext()) {
			Object node = li.next();
			OneWayMergeEdge mEdge = null;
			if (node instanceof OneWayMergeEdge){
				mEdge = (OneWayMergeEdge)node;
			} else {
				continue;
			}
			if (mEdge.toString() != null && mEdge.toString().startsWith("+")) {
				// We have a label need to import to connected states
				DefaultGraphCell source = mEdge.getSourceCell();
				DefaultGraphCell dest = mEdge.getDestinationCell();
				if (source instanceof StateCell
						&& dest instanceof  TransitionCell) {
					// This is a prevail merge
					// Find state connected to transition
					StateCell transEnd = getStateDest(dest);
					String stateName = transEnd.toString();
					List args = (List) hashStateMergeArgs.get(stateName);
					if (args == null) {
						args = new ArrayList();
						// TODO I am making the assumption that only one
						// argument is added ? Is that correct?
						args.add(mEdge.toString().substring(2));
						hashStateMergeArgs.put(stateName, args);
					} else {
						args.add(mEdge.toString().substring(2));
					}
				}
			}
		}
	}
	/**
	 * buildSimpleTransitions creates the ocl operator definitions
	 * 
	 * @param tempDom -
	 *            domain to be populated
	 * @param simpleTransitions -
	 *            non value changing transition names
	 * @param mergeEdges -
	 *            graph merge edges
	 */
	protected void buildSimpleTransitions(oclDomain tempDom,
			List simpleTransitions, List mergeEdges) {
		Iterator li = simpleTransitions.listIterator();
		while (li.hasNext()) {
			DefaultGraphCell node = (DefaultGraphCell) li.next();
			buildStaticSorts(tempDom,(TransitionCell)node);
			buildStaticConstraints(tempDom, (TransitionCell) node);
			StateCell source = getStateSource(node);
			StateCell dest = getStateDest(node);
			String objSort = LHUserObject.getObjectSort(source);
			// Is there a number constraint
			int noConsts = 0;
			oclPredicate constPred = null;
			String constraint = LHUserObject.getStrObjectProperty(node,"constraint");
			if (constraint != null) {
				String strNumber = LHUserObject.getStrObjectProperty(node,"number");
				if (strNumber != null) {
					noConsts = Integer.parseInt(strNumber);
					// lets get the constraint predicate
					constPred = tempDom.getPredicateByName(constraint);
					try {
						constPred = (oclPredicate) constPred.clone();
					} catch (CloneNotSupportedException e) {
					}
					constPred.promoteSortsToVars();
				}
			}
			String sourceStateName = source.toString();
			String destStateName = dest.toString();
			oclPredicate lhsPred = null;
			oclPredicate rhsPred = null;
			oclStateList lhsPreds = null;
			oclStateList rhsPreds = null;
			oclSC trans = new oclSC(objSort, oclPredicate.toVar(objSort));
			oclSSClassDef classDef = null;
			try {
				classDef = tempDom.getStateListForSort(objSort);
			} catch (OCLNoSuchElementException e) {
				// Should not happen
				System.err
						.println("ERROR Cannot find state definition :: OclModel");
				return;
			}
			Iterator liStates = classDef.getStateList().listIterator();
			while ((rhsPreds == null || lhsPreds == null) && liStates.hasNext()) {
				oclStateList state = (oclStateList) liStates.next();
				List preds = state.getPredicateList();
				Iterator liPreds = preds.listIterator();
				while (liPreds.hasNext()) {
					oclPredicate pred = (oclPredicate) liPreds.next();
					if (pred.getName().equals(sourceStateName)) {
						// TODO I think that this is still required - also for RHS
						//lhsPred = pred.destinguishVars();
						try {
							lhsPred = (oclPredicate)pred.clone();
							lhsPreds = (oclStateList)state.clone();
						} catch (CloneNotSupportedException e){};
					}
					if (pred.getName().equals(destStateName)) {
						try {
							rhsPred = (oclPredicate)pred.clone();
							rhsPreds = (oclStateList)state.clone();
						} catch (CloneNotSupportedException e){};
					}
				}
			}
			trans.setPost(rhsPreds.getPredicateList());
			trans.setPre(lhsPreds.getPredicateList());
			// TODO Mmmmm is this correct?
			addTransitionGraphDetails(tempDom, (TransitionCell) node, trans,
					lhsPred, rhsPred,lhsPreds,rhsPreds);
			// Now create the operator(s)
			// If this is a disjunction there will be multiple operators to create
			if (((TransitionCell)node).isDisjunction()) {
				List opNames = fetchDisjOpNames((TransitionCell)node);
				Iterator liOpNames = opNames.listIterator();
				while (liOpNames.hasNext()){
					String opName = (String)liOpNames.next();
					createOperator(opName,tempDom,(TransitionCell)node,trans,
							objSort,noConsts,constPred,mergeEdges);
				}
			} else {
				createOperator(node.toString(),tempDom,(TransitionCell)node,trans,
						objSort,noConsts,constPred,mergeEdges);
			}
		}
	}
	
	protected List fetchDisjOpNames(TransitionCell node){
		List opNames = new ArrayList();
		opNames.add(node.toString());
		List merges = getTransitionMerges(node);
		Iterator li = merges.listIterator();
		while (li.hasNext()) {
			MergeEdge edge = (MergeEdge)li.next();
			DefaultGraphCell cell = edge.getSourceCell();
			if (cell instanceof TransitionCell){
				TransitionCell trans = (TransitionCell)cell;
				if (trans.isDisjunction()) {
					List ancestorOpNames = fetchDisjOpNames(trans);
					Iterator liAncs = ancestorOpNames.listIterator();
					while (liAncs.hasNext()) {
						String opName = (String)liAncs.next();
						if (!opName.equals(node.toString())) {
							opNames.add(opName);
						}
					}
				} else {
					String opName = ((TransitionCell)cell).toString();
					if (!opName.equals(node.toString())) {
						opNames.add(opName);
					}
				}
			}
		}
		return opNames;
	}
	/**
	 * createOperator
	 * create the operator definition or if exists add new transitions to operator
	 * @param strOpName - operator name
	 * @param tempDom - the domain specification so far
	 * @param node - the graph transition node
	 * @param orgTrans - the transition definition
	 * @param objSort - the object sort for the transition
	 * @param noConsts - number constraint - number involved
	 * @param constPred - the number constraint predicate if present
	 * @param mergeEdges - graph merge edges
	 */
	protected void createOperator(String strOpName,oclDomain tempDom,TransitionCell node,oclSC orgTrans,
			String objSort, int noConsts, oclPredicate constPred, List mergeEdges){
		oclSC trans = null;
		try {
			trans = (oclSC)orgTrans.clone();
		} catch (CloneNotSupportedException e){}
		oclOperator curOp = tempDom.getOperatorByFunctorName(strOpName);
		if (curOp == null) {
			// need a new one
			curOp = tempDom.addOP();
		}
		// is this a necessary or conditional transition
		// check merges
		// It can be both if there are multiple merges to this transition node
		boolean conditionalTrans = false;
		int mergeAssociation =  hasNecessaryMerge(node, mergeEdges);
		if (isConditionalTransition(node, mergeEdges)) {
			if (mergeAssociation != -1){
				// Need to add a ne clause
				// TODO if there is a number constraint I will need multiple ne(s)
				oclSC condTrans = null;
				try {
					condTrans = (oclSC)trans.clone();
				} catch (CloneNotSupportedException e) {}
				String scSort = trans.getSort();
				String scID = trans.getName();
				condTrans.replaceVariableName(scID,scID+"A");
				oclPredicate nePred = condTrans.addPreClause("ne");
				nePred.addSortedVarArgument(scID,scSort);
				nePred.addSortedVarArgument(scID+"A",scSort);
				curOp.addCondSC(condTrans);
			} else {
				curOp.addCondSC(trans);
			}
			conditionalTrans = true;
		}
		if (!conditionalTrans || mergeAssociation != -1){
			int exist = existingObjectTrans(curOp.getNecessary(),objSort);
			if (noConsts > 0) {
				for (int i = 1; i <= noConsts; i++) {
					oclSC addTrans = null;
					String ID = oclPredicate.toVar(objSort);
					String newID = new String(ID + i);
					try {
						addTrans = (oclSC) trans.clone();
						if (i == 1) {
							addTrans.getPre().add(constPred);
						}
					} catch (CloneNotSupportedException e) {
					}
					addTrans.replaceVariableName(ID, newID);
					try {
						constPred.replaceVariableNo(i - 1, newID);
					} catch (Exception ex) {
						Utility
								.debugPrintln("patterns",
										"[OclModel] Unexpected failure to replace variable name.");
					}
					curOp.addNecSC(addTrans);
				}
			} else {
				if (exist > 0) {
					String baseID = oclPredicate.toVar(objSort);
					String ID = baseID + (exist + 1);
					String sort = trans.getSort();
					boolean top = false;
					String tmpID = ID;
					int suffix = (exist + 1);
					while (trans.transitionRefersTo(tmpID)) {
						suffix++;
						tmpID = baseID + suffix;
					}
					while (!tmpID.equals(ID)){
						String beforeID = baseID + --suffix;
						trans.replaceVariableName(beforeID,tmpID);
						tmpID = beforeID;
					}
					trans.replaceVariableName(oclPredicate.toVar(objSort),ID);
					for (int i = 0; i < exist;i++) {
						// need one ne for each existing transition
						oclPredicate nePred = trans.addPreClause("ne");
						String otherID = getNthSortID(curOp.getNecessary(),sort,i);
						nePred.addSortedVarArgument(otherID,sort);
						nePred.addSortedVarArgument(ID,sort);
					}
					if (mergeAssociation == MergeEdge.ADDASSOCIATION ||
							mergeAssociation == MergeEdge.REMOVEASSOCIATION){
						// This association must already be referenced in the merge target transition
						// Must be on RHS of transition if add LHS if remove
						adjustTransitionID(trans,node,curOp,mergeEdges);
					}
					curOp.addNecSC(trans);
				} else {
					if (mergeAssociation == MergeEdge.ADDASSOCIATION ||
							mergeAssociation == MergeEdge.REMOVEASSOCIATION){
						// This association must already be referenced in the merge target transition
						// Must be on RHS of transition if add LHS if remove
						adjustTransitionID(trans,node,curOp,mergeEdges);
					}
					curOp.addNecSC(trans);
				}
			}
		}
		oclPredicate opName = curOp.addName(strOpName);
		addPrevailsToOp(tempDom, curOp, mergeEdges);
		// TODO this should be done if there are multiple properties refering ot the same type
		// BUT not if it is also an association arg
		curOp.renameConflictingVariables(tempDom);
		curOp.setName(curOp.createOperatorName(tempDom));
		Utility.debugPrintln("patterns", curOp.toOPString());
	}
	
	private void adjustTransitionID(oclSC trans,TransitionCell transNode,oclOperator curOP,List mergeEdges) {
		Iterator li = mergeEdges.listIterator();
		while (li.hasNext()){
			Object node = li.next();
			TwoWayMergeEdge curMerge = null;
			if (node instanceof OneWayMergeEdge) {
				continue;
			} else {
				curMerge = (TwoWayMergeEdge)node;
			}
			if (curMerge.getSourceCell() == transNode ||
					curMerge.getDestinationCell() == transNode) {
				
				DefaultGraphCell cell = curMerge.getDestinationCell();
				if (cell instanceof TransitionCell) {
					if (curMerge.association() == MergeEdge.ADDASSOCIATION) {
						String targetObjSort = LHUserObject.getObjectSort((TransitionCell)cell);
						Iterator liNec = curOP.getNecessary().listIterator();
						boolean found = false;
						while (!found && liNec.hasNext()) {
							oclSC necTrans = (oclSC)liNec.next();
							if (necTrans.getSort().equals(targetObjSort)) {
								List rhs = necTrans.getPost();
								Iterator liPreds = rhs.listIterator();
								while (!found && liPreds.hasNext()) {
									oclPredicate pred = (oclPredicate)liPreds.next();
									Iterator liArgs = pred.getArguments().listIterator();
									while (!found && liArgs.hasNext()) {
										OPredicate.pArg arg = (OPredicate.pArg)liArgs.next();
										if (arg.sort.equals(trans.getSort()) &&
												!arg.name.equals(necTrans.getName())) {
											// Needed for cases where the two transitions are of the same sort
											found = true;
											trans.replaceVariableName(trans.getName(),arg.name);
										}
									}
								}
							}
						}
					} else if (curMerge.association() == MergeEdge.REMOVEASSOCIATION){
						String targetObjSort = ((TransitionCell)cell).getObjectSort();
						Iterator liNec = curOP.getNecessary().listIterator();
						boolean found = false;
						while (!found && liNec.hasNext()) {
							oclSC necTrans = (oclSC)liNec.next();
							if (necTrans.getSort().equals(targetObjSort)) {
								List lhs = necTrans.getPre();
								Iterator liPreds = lhs.listIterator();
								while (!found && liPreds.hasNext()) {
									oclPredicate pred = (oclPredicate)liPreds.next();
									Iterator liArgs = pred.getArguments().listIterator();
									while (!found && liArgs.hasNext()) {
										OPredicate.pArg arg = (OPredicate.pArg)liArgs.next();
										if (arg.sort.equals(trans.getSort()) &&
												!arg.name.equals(necTrans.getName())) {
//											 Needed for cases where the two transitions are of the same sort
											found = true;
											trans.replaceVariableName(trans.getName(),arg.name);
										}
									}
								}
							}
						}
					}
				}
			}
		}
		
	}
	
	/**
	 * buildValueTransitions creates the ocl operator definitions
	 * 
	 * @param tempDom -
	 *            domain to be populated
	 * @param valueTransitions -
	 *            non value changing transition names
	 * @param mergeEdges -
	 *            graph merge edges
	 */
	protected void buildValueTransitions(oclDomain tempDom,
			List valueTransitions, List mergeEdges) {
		Iterator li = valueTransitions.listIterator();
		while (li.hasNext()) {
			TransitionCell node = (TransitionCell) li.next();
			buildValueStaticSorts(tempDom, node);
			buildStaticConstraints(tempDom, node);
			StateCell source = getStateSource(node);
			StateCell dest = getStateDest(node);
			String objSort = source.getObjectSort();
			String sourceStateName = source.toString();
			String destStateName = dest.toString();
			// Is there a number constraint
			int noConsts = 0;
			oclPredicate constPred = null;
			String constraint = (String) ((LHUserObject) node.getUserObject())
					.getProperty("constraint");
			if (constraint != null) {
				String strNumber = (String) ((LHUserObject) node
						.getUserObject()).getProperty("number");
				if (strNumber != null) {
					noConsts = Integer.parseInt(strNumber);
					// lets get the constraint predicate
					constPred = tempDom.getPredicateByName(constraint);
					try {
						constPred = (oclPredicate) constPred.clone();
					} catch (CloneNotSupportedException e) {
					}
					constPred.promoteSortsToVars();
				}
			}
			oclPredicate lhsPred = null;
			oclPredicate rhsPred = null;
			oclStateList lhsState = null;
			oclStateList rhsState = null;
			// If there are no merges this is a single transition action
			oclSC trans = new oclSC(objSort, oclPredicate.toVar(objSort));
			oclSSClassDef classDef = null;
			try {
				classDef = tempDom.getStateListForSort(objSort);
			} catch (OCLNoSuchElementException e) {
				// Should not happen
				System.err
						.println("ERROR Cannot find state definition :: OclModel");
				return;
			}
			Iterator liStates = classDef.getStateList().listIterator();
			while ((rhsPred == null || lhsPred == null) && liStates.hasNext()) {
				oclStateList state = (oclStateList) liStates.next();
				List preds = state.getPredicateList();
				Iterator liPreds = preds.listIterator();
				while (liPreds.hasNext()) {
					oclPredicate pred = (oclPredicate) liPreds.next();
					if (pred.getName().equals(sourceStateName)) {
						// Better Clone it
						try {
							lhsState = (oclStateList)state.clone();
							lhsPred = (oclPredicate) pred.clone();
						} catch (CloneNotSupportedException ex) {
							// Should not happen
						}
					}
					if (pred.getName().equals(destStateName)) {
						// Better Clone it
						try {
							rhsState = (oclStateList)state.clone();
							rhsPred = (oclPredicate) pred.clone();
						} catch (CloneNotSupportedException ex) {
							// Should not happen
						}
					}
				}
			}
			Utility.debugPrintln("patterns", rhsPred.toString());
			addValueTransitionGraphDetails(tempDom, (TransitionCell) node, trans,lhsState,rhsState);
			// Now create the operator
			// If this is a disjunction there will be multiple operators to create
			if (((TransitionCell)node).isDisjunction()) {
				List opNames = fetchDisjOpNames((TransitionCell)node);
				Iterator liOpNames = opNames.listIterator();
				while (liOpNames.hasNext()){
					String opName = (String)liOpNames.next();
					createOperator(opName,tempDom,(TransitionCell)node,trans,
							objSort,noConsts,constPred,mergeEdges);
				}
			} else {
				createOperator(node.toString(),tempDom,(TransitionCell)node,trans,
						objSort,noConsts,constPred,mergeEdges);
			}
						
		}
	}
	
//	/**
//	 * buildValueTransitions creates the ocl operator definitions
//	 * 
//	 * @param tempDom -
//	 *            domain to be populated
//	 * @param valueTransitions -
//	 *            non value changing transition names
//	 * @param mergeEdges -
//	 *            graph merge edges
//	 */
//	protected void buildValueTransitions(oclDomain tempDom,
//			List valueTransitions, List mergeEdges) {
//		Iterator li = valueTransitions.listIterator();
//		while (li.hasNext()) {
//			DefaultGraphCell node = (DefaultGraphCell) li.next();
//			buildValueStaticSorts(tempDom, (TransitionCell) node);
//			buildStaticConstraints(tempDom, (TransitionCell) node);
//			StateCell source = getStateSource(node);
//			StateCell dest = getStateDest(node);
//			String objSort = source.getObjectSort();
//			String sourceStateName = source.toString();
//			String destStateName = dest.toString();
//			// Is there a number constraint
//			int noConsts = 0;
//			oclPredicate constPred = null;
//			String constraint = (String) ((LHUserObject) node.getUserObject())
//					.getProperty("constraint");
//			if (constraint != null) {
//				String strNumber = (String) ((LHUserObject) node
//						.getUserObject()).getProperty("number");
//				if (strNumber != null) {
//					noConsts = Integer.parseInt(strNumber);
//					// lets get the constraint predicate
//					constPred = tempDom.getPredicateByName(constraint);
//					try {
//						constPred = (oclPredicate) constPred.clone();
//					} catch (CloneNotSupportedException e) {
//					}
//					constPred.promoteSortsToVars();
//				}
//			}
//			oclPredicate lhsPred = null;
//			oclPredicate rhsPred = null;
//			oclStateList lhsState = null;
//			oclStateList rhsState = null;
//			// If there are no merges this is a single transition action
//			oclSC trans = new oclSC(objSort, oclPredicate.toVar(objSort));
//			oclSSClassDef classDef = null;
//			try {
//				classDef = tempDom.getStateListForSort(objSort);
//			} catch (OCLNoSuchElementException e) {
//				// Should not happen
//				System.err
//						.println("ERROR Cannot find state definition :: OclModel");
//				return;
//			}
//			Iterator liStates = classDef.getStateList().listIterator();
//			while ((rhsPred == null || lhsPred == null) && liStates.hasNext()) {
//				oclStateList state = (oclStateList) liStates.next();
//				List preds = state.getPredicateList();
//				Iterator liPreds = preds.listIterator();
//				while (liPreds.hasNext()) {
//					oclPredicate pred = (oclPredicate) liPreds.next();
//					if (pred.getName().equals(sourceStateName)) {
//						// Better Clone it
//						try {
//							lhsState = (oclStateList)state.clone();
//							lhsPred = (oclPredicate) pred.clone();
//						} catch (CloneNotSupportedException ex) {
//							// Should not happen
//						}
//					}
//					if (pred.getName().equals(destStateName)) {
//						// Better Clone it
//						try {
//							rhsState = (oclStateList)state.clone();
//							rhsPred = (oclPredicate) pred.clone();
//						} catch (CloneNotSupportedException ex) {
//							// Should not happen
//						}
//					}
//				}
//			}
//			Utility.debugPrintln("patterns", rhsPred.toString());
//			addValueTransitionGraphDetails(tempDom, (TransitionCell) node, trans,
//					lhsPred, rhsPred,lhsState,rhsState);
//			// Now create the operator
//			// If this is a disjunction there will be multiple operators to create
//			if (((TransitionCell)node).isDisjunction()) {
//				List opNames = fetchDisjOpNames((TransitionCell)node);
//				Iterator liOpNames = opNames.listIterator();
//				while (liOpNames.hasNext()){
//					String opName = (String)liOpNames.next();
//					createOperator(opName,tempDom,(TransitionCell)node,trans,
//							objSort,noConsts,constPred,mergeEdges);
//				}
//			} else {
//				createOperator(node.toString(),tempDom,(TransitionCell)node,trans,
//						objSort,noConsts,constPred,mergeEdges);
//			}
//						
//		}
//	}
	/**
	 * existingObjectTrans 
	 * 			find if we already have necessary transitions for objects of this sort
	 * @param lstTrans - List od oclSC transitions
	 * @param objSort - the sort we are interested in
	 * @return the count of such transitions
	 */
	private int existingObjectTrans(List lstTrans,String objSort){
		int ret = 0;
		if (lstTrans == null)
			return 0;
		Iterator li = lstTrans.listIterator();
		while (li.hasNext()) {
			oclSC trans = (oclSC)li.next();
			String sort = trans.getSort();
			if (sort.equals(objSort))
				ret++;
		}
		return ret;
	}
	/**
	 * getNthSortID
	 * find the ID of the inx(th) transition for this sort
	 * @param lstTrans
	 * @param objSort
	 * @param inx
	 * @return
	 */
	private String getNthSortID(List lstTrans,String objSort,int inx){
		Iterator li = lstTrans.listIterator();
		int count = 0;
		while (li.hasNext()) {
			oclSC trans = (oclSC)li.next();
			String sort = trans.getSort();
			if (sort.equals(objSort))
				count++;
			if (count == inx + 1)
				return trans.getName();
		}
		return null;
	}
	
	/**
	 * getState
	 * find the state predicate for the given sort and functor name
	 * @param tempDom
	 * @param sortName - the sort we are interested in
	 * @param stateName - the predicate name
	 * @return
	 */
	protected oclPredicate getState(oclDomain tempDom, String sortName,
			String stateName) {
		oclSSClassDef classDef = null;
		try {
			classDef = tempDom.getStateListForSort(sortName);
		} catch (OCLNoSuchElementException e) {
			// Should not happen
			System.err
					.println("ERROR Cannot find state definition :: OclModel");
			return null;
		}
		Iterator liStates = classDef.getStateList().listIterator();
		oclPredicate statePred = null;
		while (statePred == null && liStates.hasNext()) {
			oclStateList state = (oclStateList) liStates.next();
			List preds = state.getPredicateList();
			Iterator liPreds = preds.listIterator();
			while (liPreds.hasNext()) {
				oclPredicate pred = (oclPredicate) liPreds.next();
				if (pred.getName().equals(stateName)) {
					// Better Clone it
					try {
						statePred = (oclPredicate) pred.clone();
					} catch (CloneNotSupportedException ex) {
						// Should not happen
					}
				}
			}
		}
		return statePred;
	}
	
	
	/**
	 * getState
	 * find the state predicate for the given sort and functor name
	 * @param tempDom
	 * @param sortName - the sort we are interested in
	 * @param stateName - the predicate name
	 * @return
	 */
	protected oclStateList getFullState(oclDomain tempDom, String sortName,
			String stateName) {
		oclSSClassDef classDef = null;
		oclStateList ret = null;
		try {
			classDef = tempDom.getStateListForSort(sortName);
		} catch (OCLNoSuchElementException e) {
			// Should not happen
			System.err
					.println("ERROR Cannot find state definition :: OclModel");
			return null;
		}
		Iterator liStates = classDef.getStateList().listIterator();
		boolean found = false;
		while (!found && liStates.hasNext()) {
			oclStateList state = (oclStateList) liStates.next();
			List preds = state.getPredicateList();
			Iterator liPreds = preds.listIterator();
			while (liPreds.hasNext()) {
				oclPredicate pred = (oclPredicate) liPreds.next();
				if (pred.getName().equals(stateName)) {
					// Better Clone it
					try {
						ret = (oclStateList) state.clone();
					} catch (CloneNotSupportedException ex) {
						// Should not happen
					}
				}
			}
		}
		return ret;
	}
	
	/**
	 * addTransitionGraphDetails add predicates (if necessary) to select next
	 * value for value changing parameter adjust rhs and lhs predicates to
	 * reflect new value arguments
	 * 
	 * @param tempDom
	 *            the domain to populate
	 * @param node -
	 *            the value changing transition node
	 * @param trans -
	 *            the oclSC clause being built
	 * @param lhsState -
	 *            current LHS State - carries additional properties
	 * @param rhsState -
	 *            current RHS State - carries additional properties
	 */
	protected void addValueTransitionGraphDetails(oclDomain tempDom,
			TransitionCell node, oclSC trans,
			oclStateList lhsState,oclStateList rhsState) {
		// There may be multiple value changes in this transition
		List connectPreds = new ArrayList();
		int iSuffix = 1;
		String strSuffix = "";
		boolean look = true;
		while (look) {
			// valueSort is the property name
			String valueSort = (String) ((LHUserObject) node.getUserObject())
					.getProperty("ValueSort" + strSuffix);
			if (valueSort != null) {
				// Get property predicate
				oclPredicate propertyPred = tempDom.getPredicateByName(valueSort);
				String arg = oclPredicate.toVar(propertyPred.getNthElementName(1));
				boolean fullyConnected = "fullyConnected"
						.equals((String) ((LHUserObject) node.getUserObject())
								.getProperty("EdgeName" + strSuffix));
				if (fullyConnected) {
					// TODO Reconsider this may be problem with multiple properties with
					// same type
					//lhsState.renamePredArgInState(propertyPred.getName(),arg,arg + "A");
					//rhsState.renamePredArgInState(propertyPred.getName(),arg,arg + "B");
					lhsState.renamePropertyArgInState(propertyPred.getName(),arg,arg + "A");
					rhsState.renamePropertyArgInState(propertyPred.getName(),arg,arg + "B");
				} else {
					// This is a directed graph
					oclPredicate connectPred = null;
					try {
						String predName = ((String) ((LHUserObject) node
								.getUserObject()).getProperty("EdgeName"
								+ strSuffix));
						oclPredicate pred = tempDom
								.getPredicateByName(predName);
						connectPred = (oclPredicate) pred.clone();
					} catch (CloneNotSupportedException e) {
					}
					connectPred.promoteSortsToVars();
					try {
						connectPred.replaceVariableNo(0, arg + "A");
						connectPred.replaceVariableNo(1, arg + "B");
					} catch (Exception ex) {
					}
					// TODO Consider was renamePredArgInState i.e. onth the reference to the property name
					//lhsState.renamePredArgInState(propertyPred.getName(),arg,arg + "A");
					//rhsState.renamePredArgInState(propertyPred.getName(),arg,arg + "B");
					lhsState.renamePropertyArgInState(propertyPred.getName(),arg,arg + "A");
					rhsState.renamePropertyArgInState(propertyPred.getName(),arg,arg + "B");
					connectPreds.add(connectPred);
				}
				iSuffix++;
				strSuffix = Integer.toString(iSuffix);
			} else {
				look = false;
			}
		}
		Iterator liStates = lhsState.getPredicateList().listIterator();
		while (liStates.hasNext()) {
			oclPredicate curPred = (oclPredicate)liStates.next();
			trans.addPre(curPred);
		}
		liStates = rhsState.getPredicateList().listIterator();
		while (liStates.hasNext()) {
			oclPredicate curPred = (oclPredicate)liStates.next();
			trans.addPost(curPred);
		}
		Iterator li = connectPreds.listIterator();
		while (li.hasNext()) {
			oclPredicate connectPred = (oclPredicate) li.next();
			trans.addPre(connectPred);
		}
	}
	
	
	/**
	 * addTransitionGraphDetails add predicates (if necessary) to select next
	 * value for value changing parameter adjust rhs and lhs predicates to
	 * reflect new value arguments.
	 * This is a Transition Node so may not involve any value changes
	 * 
	 * @param tempDom
	 *            the domain to populate
	 * @param node -
	 *            the value changing transition node (if change made)
	 * @param trans -
	 *            the oclSC clause being built
	 * @param lhsPred -
	 *            current LHS of transition
	 * @param rhsPred -
	 *            current RHS of transition
	 * @param lhsState -
	 *            current LHS State - carries additional properties
	 * @param rhsState -
	 *            current RHS State - carries additional properties
	 */
	
	protected void addTransitionGraphDetails(oclDomain tempDom,
			TransitionCell node, oclSC trans, oclPredicate lhsPred,
			oclPredicate rhsPred,oclStateList lhsState,oclStateList rhsState) {
		// There may be multiple value changes in this transition
		List connectPreds = new ArrayList();
		int iSuffix = 1;
		String strSuffix = "";
		boolean look = true;
		while (look) {
			String valueSortProp = LHUserObject.getStrObjectProperty(node,"ValueSort" + strSuffix);
			if (valueSortProp != null) {
				// We are dealing with properties
				oclPredicate propPred = tempDom.getPredicateByName(valueSortProp);
				String valueSort = propPred.getNthElementName(1);
				String valueVar = oclPredicate.toVar(valueSort);
				String valueVarA = null;
				String valueVarB = null;
				if( null == tempDom.getStates(valueSort)) {
					valueVarA = valueVar + "A";
					valueVarB = valueVar + "B";
				} else {
					String opName = LHUserObject.getLabel(node);
					String transSort = LHUserObject.getObjectSort(node);
					valueVarA = tempDom.opTransReferencesLHS(opName,valueSort,oclPredicate.toVar(transSort));
					valueVarB = tempDom.opTransReferencesRHS(opName,valueSort,oclPredicate.toVar(transSort));
					if (valueVarA == null || valueVarB == null) {
						valueVarA = valueVar + "A";
						valueVarB = valueVar + "B";
					}
				}
				boolean fullyConnected = "fullyConnected"
						.equals((String) ((LHUserObject) node.getUserObject())
								.getProperty("EdgeName" + strSuffix));
				if (fullyConnected) {
					if (lhsPred.refersTo(valueVar)) {
						try {
							lhsPred.replaceVariableNameByName(valueVar,
									valueVarA);
						} catch (Exception e) {
							// Should not happen
						}
					}
					if (rhsPred.refersTo(valueVar)) {
						try {
							rhsPred.replaceVariableNameByName(valueVar,
									valueVarB);
						} catch (Exception e) {
							// Should not happen
						}
					}
				} else {
					// This is a directed graph
					oclPredicate connectPred = null;
					try {
						String predName = ((String) ((LHUserObject) node
								.getUserObject()).getProperty("EdgeName"
								+ strSuffix));
						oclPredicate pred = tempDom
								.getPredicateByName(predName);
						connectPred = (oclPredicate) pred.clone();
					} catch (CloneNotSupportedException e) {
					}
					connectPred.promoteSortsToVars();
					try {
						connectPred.replaceVariableNo(0, valueVarA);
						connectPred.replaceVariableNo(1, valueVarB);
					} catch (Exception ex) {
					}
					lhsState.renameArgInState(valueVar,valueVarA);
					rhsState.renameArgInState(valueVar,valueVarB);
					lhsState.addPredicate(connectPred);
				}
				iSuffix++;
				strSuffix = Integer.toString(iSuffix);
			} else {
				look = false;
			}
		}
	}
	
//	/**
//	 * addTransitionGraphDetails add predicates (if necessary) to select next
//	 * value for value changing parameter adjust rhs and lhs predicates to
//	 * reflect new value arguments
//	 * 
//	 * @param tempDom
//	 *            the domain to populate
//	 * @param node -
//	 *            the value changing transition node
//	 * @param trans -
//	 *            the oclSC clause being built
//	 * @param lhsPred -
//	 *            current LHS of transition
//	 * @param rhsPred -
//	 *            current RHS of transition
//	 * @param lhsState -
//	 *            current LHS State - carries additional properties
//	 * @param rhsState -
//	 *            current RHS State - carries additional properties
//	 */
//	protected void addValueTransitionGraphDetails(oclDomain tempDom,
//			TransitionCell node, oclSC trans, oclPredicate lhsPred,
//			oclPredicate rhsPred,oclStateList lhsState,oclStateList rhsState) {
//		// There may be multiple value changes in this transition
//		List connectPreds = new ArrayList();
//		int iSuffix = 1;
//		String strSuffix = "";
//		boolean look = true;
//		while (look) {
//			String valueSort = (String) ((LHUserObject) node.getUserObject())
//					.getProperty("ValueSort" + strSuffix);
//			if (valueSort != null) {
//				String valueVar = oclPredicate.toVar(valueSort);
//				boolean fullyConnected = "fullyConnected"
//						.equals((String) ((LHUserObject) node.getUserObject())
//								.getProperty("EdgeName" + strSuffix));
//				if (fullyConnected) {
//					if (lhsPred.refersTo(valueVar)) {
//						try {
//							lhsPred.replaceVariableNameByName(valueVar,
//									valueVar + "A");
//						} catch (Exception e) {
//							// Should not happen
//						}
//					}
//					if (rhsPred.refersTo(valueVar)) {
//						try {
//							rhsPred.replaceVariableNameByName(valueVar,
//									valueVar + "B");
//						} catch (Exception e) {
//							// Should not happen
//						}
//					}
//				} else {
//					// This is a directed graph
//					oclPredicate connectPred = null;
//					try {
//						String predName = ((String) ((LHUserObject) node
//								.getUserObject()).getProperty("EdgeName"
//								+ strSuffix));
//						oclPredicate pred = tempDom
//								.getPredicateByName(predName);
//						connectPred = (oclPredicate) pred.clone();
//					} catch (CloneNotSupportedException e) {
//					}
//					connectPred.promoteSortsToVars();
//					try {
//						connectPred.replaceVariableNo(0, valueVar + "A");
//						connectPred.replaceVariableNo(1, valueVar + "B");
//					} catch (Exception ex) {
//					}
//					try {
//						lhsPred.replaceVariableNameByName(valueVar,valueVar + "A");
//						rhsPred.replaceVariableNameByName(valueVar,valueVar + "B");
//					} catch (Exception e){}
//					lhsState.renameArgInState(valueVar,valueVar + "A");
//					rhsState.renameArgInState(valueVar,valueVar + "B");
//					connectPreds.add(connectPred);
//				}
//				iSuffix++;
//				strSuffix = Integer.toString(iSuffix);
//			} else {
//				look = false;
//			}
//		}
//		trans.addPost(rhsPred);
//		trans.addPre(lhsPred);
//		Iterator liStates = lhsState.getPredicateList().listIterator();
//		while (liStates.hasNext()) {
//			oclPredicate curPred = (oclPredicate)liStates.next();
//			if (!curPred.getName().equals(lhsPred.getName())) {
//				trans.addPre(curPred);
//			}
//		}
//		liStates = rhsState.getPredicateList().listIterator();
//		while (liStates.hasNext()) {
//			oclPredicate curPred = (oclPredicate)liStates.next();
//			if (!curPred.getName().equals(rhsPred.getName())) {
//				trans.addPost(curPred);
//			}
//		}
//		Iterator li = connectPreds.listIterator();
//		while (li.hasNext()) {
//			oclPredicate connectPred = (oclPredicate) li.next();
//			trans.addPre(connectPred);
//		}
//	}
	/**
	 * addPrevailsToOp search the merge list to see if this operator requires
	 * ant prevail clauses
	 * 
	 * @param tempDom -
	 *            the domain being built
	 * @param curOp -
	 *            the operator to add to
	 * @param mergeEdges -
	 *            list of all domain merges
	 */
	protected void addPrevailsToOp(oclDomain tempDom, oclOperator curOp,
			List mergeEdges) {
		Iterator li = mergeEdges.listIterator();
		while (li.hasNext()) {
			Object merge = li.next();
			OneWayMergeEdge mEdge = null;
			if (! (merge instanceof OneWayMergeEdge)) {
				continue;
			} else {
				mEdge = (OneWayMergeEdge)merge;
			}
			DefaultGraphCell node = mEdge.getDestinationCell();
			if (node.toString().equals(curOp.opName.getName())) {
				// Relates to this operator
				DefaultGraphCell source = mEdge.getSourceCell();
				StateCell xSCell = new StateCell();
				if (xSCell.getClass().isInstance(source)) {
					// This is a prevail
					String prevSort = (String) ((LHUserObject) ((StateCell) source)
							.getUserObject()).getProperty("ObjectSort");
					String prevStateName = ((StateCell) source).toString();
					oclStateList prevState = getFullState(tempDom, prevSort,
							prevStateName);
					oclSE prevSE = new oclSE(prevSort, oclPredicate
							.toVar(prevSort));
					Iterator liStates = prevState.getPredicateList().listIterator();
					while (liStates.hasNext()){
						oclPredicate curPred = (oclPredicate)liStates.next();
						curPred.promoteSortsToVars();
						prevSE.addPredicate(curPred);
					}
					if (!curOp.containsPrevail(prevSE))
						curOp.addPrevSE(prevSE);
				}
			}
		}
	}
	
//	protected void addPrevailsToOp(oclDomain tempDom, oclOperator curOp,
//			List mergeEdges) {
//		Iterator li = mergeEdges.listIterator();
//		while (li.hasNext()) {
//			Object merge = li.next();
//			OneWayMergeEdge mEdge = null;
//			if (! (merge instanceof OneWayMergeEdge)) {
//				continue;
//			} else {
//				mEdge = (OneWayMergeEdge)merge;
//			}
//			DefaultGraphCell node = mEdge.getDestinationCell();
//			if (node.toString().equals(curOp.opName.getName())) {
//				// Relates to this operator
//				DefaultGraphCell source = mEdge.getSourceCell();
//				StateCell xSCell = new StateCell();
//				if (xSCell.getClass().isInstance(source)) {
//					// This is a prevail
//					String prevSort = (String) ((LHUserObject) ((StateCell) source)
//							.getUserObject()).getProperty("ObjectSort");
//					String prevStateName = ((StateCell) source).toString();
//					oclPredicate prevPred = getState(tempDom, prevSort,
//							prevStateName);
//					try {
//						prevPred = (oclPredicate) prevPred.clone();
//					} catch (CloneNotSupportedException e) {
//					}
//					prevPred.promoteSortsToVars();
//					oclSE prevSE = new oclSE(prevSort, oclPredicate
//							.toVar(prevSort));
//					prevSE.addPredicate(prevPred);
//					if (!curOp.containsPrevail(prevSE))
//						curOp.addPrevSE(prevSE);
//				}
//			}
//		}
//	}
	/**
	 * isConditionalTransition look for merges between transitions with the same
	 * name but only one way
	 * 
	 * @param opNode -
	 *            the graph node of the current
	 * @param mergeEdges
	 * @return
	 */
	private boolean isConditionalTransition(DefaultGraphCell opNode,
			List mergeEdges) {
		Iterator li = mergeEdges.listIterator();
		boolean mergeTo = false;
		boolean mergeFrom = false;
		while (li.hasNext()) {
			Object node = li.next();
			OneWayMergeEdge curMerge = null;
			if (node instanceof TwoWayMergeEdge) {
				continue;
			} else {
				curMerge = (OneWayMergeEdge)node;
			}
			if (curMerge.getDestinationCell() == opNode) {
				// does source have same opName
				DefaultGraphCell source = curMerge.getSourceCell();
				if (source.toString().equals(opNode.toString())) {
					mergeTo = true;
				}
			} else if (curMerge.getSourceCell() == opNode) {
				// does destination have same op name
				DefaultGraphCell dest = curMerge.getDestinationCell();
				if (dest.toString().equals(opNode.toString())) {
					mergeFrom = true;
				}
			}
		}
		return !mergeFrom && mergeTo;
	}
	
	/**
	 * getTransitionMerges
	 * @param trans
	 * @return
	 */
	private List getTransitionMerges(TransitionCell trans) {
		List ret = new ArrayList();
		Enumeration enum = trans.children();
		while ( enum.hasMoreElements() ) {
			DefaultPort port = (DefaultPort)enum.nextElement();
			Iterator li = port.edges();	
			while (li.hasNext()) {
				DefaultEdge edge = (DefaultEdge)li.next();
				if (edge instanceof MergeEdge) {
					DefaultGraphCell connTrans = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
					if (connTrans == trans) {
						//This is an incomming merge edge
						ret.add(edge);		
					}
				}
			}
		}
		return ret;
	}
	
	/**
	 * hasNecessaryMerge look for merges between transitions with the same
	 * name but only two way
	 * if none then this is necessary
	 * @param opNode -
	 *            the graph node of the current
	 * @param mergeEdges
	 * @return int returns -1 if no merge otherwise returns merge constant describing
	 *                     the association (NOASSOCIATION,ADDASSOCIATION or REMOVEASSOCIATION)
	 */
	private int hasNecessaryMerge(DefaultGraphCell opNode,
			List mergeEdges) {
		Iterator li = mergeEdges.listIterator();
		boolean mergeTo = false;
		boolean mergeFrom = false;
		while (li.hasNext()) {
			Object node = li.next();
			TwoWayMergeEdge curMerge = null;
			if (node instanceof OneWayMergeEdge) {
				continue;
			} else {
				curMerge = (TwoWayMergeEdge)node;
			}
			if (curMerge.getSourceCell() == opNode ||
					curMerge.getDestinationCell() == opNode) {
				
				return curMerge.association();
			}
		}
		return -1;
	}
	/**
	 * buildStaticSorts create the static graph details
	 * 
	 * @param tempDom
	 * @param node
	 */
	protected void buildStaticSorts(oclDomain tempDom, TransitionCell node) {
		String valueSortProp = LHUserObject.getStrObjectProperty(node,"ValueSort");
		if (valueSortProp == null)
			return; // Nothing to do
		// We are dealing with a change to property values
		oclPredicate propPred = tempDom.getPredicateByName(valueSortProp);
		String valueSort = propPred.getNthElementName(1);
		if (!"fullyConnected".equals(LHUserObject.getStrObjectProperty(node,"EdgeName"))) {
			String connectName = LHUserObject.getStrObjectProperty(node,"EdgeName");
			oclPredicate connectPred = null;
			if ((connectPred = tempDom.getPredicateByName(connectName)) == null) {
				connectPred = tempDom.addPredicate(connectName);
				connectPred.addConstArgument(valueSort);
				connectPred.addConstArgument(valueSort);
				connectPred.setStatic(true);
			}
		}
		// Deal with multiple value changes
		int suffix = 2;
		boolean look = true;
		while (look) {
			valueSortProp = (String) ((LHUserObject) node.getUserObject())
					.getProperty("ValueSort" + suffix);
			if (valueSortProp != null) {
				propPred = tempDom.getPredicateByName(valueSortProp);
				valueSort = propPred.getNthElementName(1);
				if (!"fullyConnected".equals((String) ((LHUserObject) node
						.getUserObject()).getProperty("EdgeName" + suffix))) {
					String connectName = ((String) ((LHUserObject) node
							.getUserObject()).getProperty("EdgeName" + suffix));
					oclPredicate connectPred = null;
					if ((connectPred = tempDom.getPredicateByName(connectName)) == null) {
						connectPred = tempDom.addPredicate(connectName);
						connectPred.addConstArgument(valueSort);
						connectPred.addConstArgument(valueSort);
						connectPred.setStatic(true);
					}
				}
				suffix++;
			} else {
				look = false;
			}
		}
	}
	
	/**
	 * buildValueStaticSorts create the static graph details
	 * 
	 * @param tempDom
	 * @param node
	 */
	protected void buildValueStaticSorts(oclDomain tempDom, TransitionCell node) {
		String valueSort = (String) ((LHUserObject) node.getUserObject())
				.getProperty("ValueSort");
		if (valueSort == null)
			return; // Nothing to do
		// Sorts already created
		if (!"fullyConnected".equals((String) ((LHUserObject) node
				.getUserObject()).getProperty("EdgeName"))) {
			String connectName = ((String) ((LHUserObject) node.getUserObject())
					.getProperty("EdgeName"));
			oclPredicate connectPred = null;
			if ((connectPred = tempDom.getPredicateByName(connectName)) == null) {
				connectPred = tempDom.addPredicate(connectName);
				oclPredicate propPred = tempDom.getPredicateByName(valueSort);
				String var = propPred.getNthElementName(1);
				connectPred.addConstArgument(var);
				connectPred.addConstArgument(var);
				connectPred.setStatic(true);
			}
		}
		// Deal with multiple value changes
		int suffix = 2;
		boolean look = true;
		while (look) {
			valueSort = (String) ((LHUserObject) node.getUserObject())
					.getProperty("ValueSort" + suffix);
			//	Sorts already created
			if (valueSort != null) {
				if (!"fullyConnected".equals((String) ((LHUserObject) node
						.getUserObject()).getProperty("EdgeName" + suffix))) {
					String connectName = ((String) ((LHUserObject) node
							.getUserObject()).getProperty("EdgeName" + suffix));
					oclPredicate connectPred = null;
					if ((connectPred = tempDom.getPredicateByName(connectName)) == null) {
						connectPred = tempDom.addPredicate(connectName);
						oclPredicate propPred = tempDom.getPredicateByName(valueSort);
						String var = propPred.getNthElementName(1);
						connectPred.addConstArgument(var);
						connectPred.addConstArgument(var);
						connectPred.setStatic(true);
					}
				}
				suffix++;
			} else {
				look = false;
			}
		}
	}
	
	
//	/**
//	 * buildValueStaticSorts create the static graph details
//	 * 
//	 * @param tempDom
//	 * @param node
//	 */
//	protected void buildValueStaticSorts(oclDomain tempDom, TransitionCell node) {
//		String valueSort = (String) ((LHUserObject) node.getUserObject())
//				.getProperty("ValueSort");
//		if (valueSort == null)
//			return; // Nothing to do
//		oclSort prims = tempDom.getSort("primitive_sorts");
//		if (!tempDom.isSort(valueSort)) {
//			prims.addSubType(valueSort);
//		}
//		if (!"fullyConnected".equals((String) ((LHUserObject) node
//				.getUserObject()).getProperty("EdgeName"))) {
//			String connectName = ((String) ((LHUserObject) node.getUserObject())
//					.getProperty("EdgeName"));
//			oclPredicate connectPred = null;
//			if ((connectPred = tempDom.getPredicateByName(connectName)) == null) {
//				connectPred = tempDom.addPredicate(connectName);
//				connectPred.addConstArgument(valueSort);
//				connectPred.addConstArgument(valueSort);
//				connectPred.setStatic(true);
//			}
//		}
//		// Deal with multiple value changes
//		int suffix = 2;
//		boolean look = true;
//		while (look) {
//			valueSort = (String) ((LHUserObject) node.getUserObject())
//					.getProperty("ValueSort" + suffix);
//			if (valueSort != null) {
//				if (!tempDom.isSort(valueSort)) {
//					prims.addSubType(valueSort);
//				}
//				if (!"fullyConnected".equals((String) ((LHUserObject) node
//						.getUserObject()).getProperty("EdgeName" + suffix))) {
//					String connectName = ((String) ((LHUserObject) node
//							.getUserObject()).getProperty("EdgeName" + suffix));
//					oclPredicate connectPred = null;
//					if ((connectPred = tempDom.getPredicateByName(connectName)) == null) {
//						connectPred = tempDom.addPredicate(connectName);
//						connectPred.addConstArgument(valueSort);
//						connectPred.addConstArgument(valueSort);
//						connectPred.setStatic(true);
//					}
//				}
//				suffix++;
//			} else {
//				look = false;
//			}
//		}
//	}
	/**
	 * buildStaticConstraints create the static constraints TODO Deal with ne
	 * constraints
	 * Only deals with NUMBER constraints at the moment
	 * @param tempDom
	 * @param node
	 */
	protected void buildStaticConstraints(oclDomain tempDom, TransitionCell node) {
		String constraint = (String) ((LHUserObject) node.getUserObject())
				.getProperty("constraint");
		if (constraint != null) {
			String strNumber = (String) ((LHUserObject) node.getUserObject())
					.getProperty("number");
			if (strNumber != null) {
				int no = Integer.parseInt(strNumber);
				oclPredicate constPred = null;
				if ((constPred = tempDom.getPredicateByName(constraint)) == null) {
					constPred = tempDom.addPredicate(constraint);
					constPred.setStatic(true);
					String sort = (String) ((LHUserObject) node.getUserObject())
							.getProperty("ObjectSort");
					for (int i = 0; i < no; i++) {
						constPred.addConstArgument(sort);
					}
				}
			}
		}
	}
	/**
	 * getStateSource find transition source state ie LHS of transition
	 * 
	 * @param node -
	 *            transition node
	 * @return
	 */
	protected StateCell getStateSource(DefaultGraphCell node) {
		int noChildren = node.getChildCount();
		for (int i = 0; i < noChildren; i++) {
			Object obj = node.getChildAt(i);
			DefaultPort dp = new DefaultPort();
			if (dp.getClass().isInstance(obj)) {
				DefaultPort curPort = (DefaultPort) obj;
				Set edgeSet = ((DefaultPort) obj).getEdges();
				Iterator setIt = edgeSet.iterator();
				while (setIt.hasNext()) {
					DefaultEdge edge = (DefaultEdge) setIt.next();
					TransitionEdge tEdge = new TransitionEdge();
					if (tEdge.getClass().isInstance(edge)
							&& edge.getSource() != curPort) {
						DefaultGraphCell sCell = (DefaultGraphCell) ((DefaultPort) edge
								.getSource()).getParent();
						StateCell xStateCell = new StateCell();
						if (xStateCell.getClass().isInstance(sCell)) {
							// This must be it
							return (StateCell) sCell;
						}
					}
				}
			}
		}
		return null; // Should not happen
	}
	/**
	 * getStateDest find transition destination state ie RHS of transition
	 * 
	 * @param node -
	 *            transition node
	 * @return
	 */
	protected StateCell getStateDest(DefaultGraphCell node) {
		int noChildren = node.getChildCount();
		for (int i = 0; i < noChildren; i++) {
			Object obj = node.getChildAt(i);
			DefaultPort dp = new DefaultPort();
			if (dp.getClass().isInstance(obj)) {
				DefaultPort curPort = (DefaultPort) obj;
				Set edgeSet = ((DefaultPort) obj).getEdges();
				Iterator setIt = edgeSet.iterator();
				while (setIt.hasNext()) {
					DefaultEdge edge = (DefaultEdge) setIt.next();
					if (edge instanceof TransitionEdge
							&& edge.getSource() == curPort) {
						DefaultGraphCell sCell = (DefaultGraphCell) ((DefaultPort) edge
								.getTarget()).getParent();
						if (sCell instanceof StateCell) {
							// This must be it
							return (StateCell) sCell;
						}
					}
				}
			}
		}
		return null; // Should not happen
	}
	
	/**
	 *  getOutwardTransitions
	 * Find all transition nodes departing from the gives state node
	 * @param given the state cell to search from
	 * @return List of the outwards transition nodes
	 */
	private List getOutwardTransitions(StateCell given){
		List ret = new ArrayList();
		Enumeration enum = given.children();
		while ( enum.hasMoreElements() ) {
			DefaultPort port = (DefaultPort)enum.nextElement();
			Iterator li = port.edges();	
			while (li.hasNext()) {
				DefaultEdge edge = (DefaultEdge)li.next();
				if (edge instanceof TransitionEdge) {
					DefaultGraphCell connTrans = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
					if (connTrans != given) {
						//This is outwards bound
						ret.add(connTrans);		
					}
				}
			}
		}
		return ret;
	}
	/**
	 * checkConsistency
	 * works at the level of the diagram to ensure that
	 * consistency rules have been met
	 * 1. All state names are unique
	 * @param model
	 * @throws OCLException
	 */
	public void checkConsistency(GraphModel model) throws OCLException {
		Hashtable hashStatePreds = new Hashtable();
		List propNames = new ArrayList();
		Hashtable hashPropNames = new Hashtable();
		Hashtable hashAssociatedArgs = new Hashtable();
		Hashtable hashTransNames = new Hashtable();
		List mergeEdges = new ArrayList();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				StateCell stateNode = (StateCell)curRoot;
				LHUserObject user = (LHUserObject) (stateNode).getUserObject();
				String sortName = LHUserObject.getObjectSort(stateNode);
				String stateName = LHUserObject.getLabel(stateNode);
				// Put the node in a hash table indexed by state name
				DefaultGraphCell cell = (DefaultGraphCell) hashStatePreds.get(stateName.toLowerCase());
				if (cell != null) {
					throw new OCLException("The state name [" + stateName + "] is used for more than one object state.");
				}
				if (Utility.listContainsString(stateName.toLowerCase(),propNames)) {
					throw new OCLException("The name [" + stateName + "] is used as the name of a state and as a state property");
				}
				if (!Utility.isLegalIdentifier(stateName)){
					throw new OCLException("The name [" + stateName + "] is not legal - must start with lowercase letter and be followed by letters or digits or _");
				}
				if (!Utility.isLegalIdentifier(sortName)){
					throw new OCLException("The sort name [" + sortName + "] is not legal - must start with lowercase letter and be followed by letters or digits or _");
				}
				hashStatePreds.put(stateName.toLowerCase(),stateNode);
				checkStateProperties(user,sortName,hashPropNames,propNames,hashStatePreds,hashTransNames);
				checkConnectedTransitionsHaveCorrectSort(stateNode);
			} else if (curRoot instanceof TransitionCell) {
				String transName = LHUserObject.getLabel((TransitionCell)curRoot);
				if (!Utility.isLegalIdentifier(transName)){
					throw new OCLException("The name [" + transName + "] is not legal - must start with lowercase letter and be followed by letters or digits or _");
				}
				if (Utility.listContainsString(transName.toLowerCase(),propNames)) {
					throw new OCLException("The name [" + transName + "] is used as the name of a transition and as a state property name");
				}
				List same = (List)hashTransNames.get(transName.toLowerCase());
				if (same == null) {
					List cells = new ArrayList();
					cells.add(curRoot);
					hashTransNames.put(transName.toLowerCase(),cells);
				} else if (!mergeRouteTo((TransitionCell)curRoot,same,new ArrayList())){
					throw new OCLException("The transition name [" + transName + "] is used for transitions that are not shown as merged");
				}
				((TransitionCell)curRoot).checkTransitionConnections();
			} else if (curRoot instanceof MergeEdge) {
				//collectAssociationArgs((MergeEdge)curRoot,hashAssociatedArgs);
				mergeEdges.add(curRoot);
			}
		}
		//simpleTransitions = topSortTransitions(simpleTransitions,mergeEdges);
	}
	/**
	 * checkStateProperties
	 * 1 Property names must not be overloaded i.e. same then same object sort same property sort
	 * 2 All objects of the same sort must have the same set of properties
	 * @param userObj
	 * @param objSort
	 * @param hashProps
	 * @param hashPropnames
	 * @throws OCLException
	 */
	private void checkStateProperties(LHUserObject userObj,String objSort,Hashtable hashProps,
			List propNames,Hashtable hashStatePreds,Hashtable hashTransNames)throws OCLException{
		checkPropertiesContinuous(userObj);
		List objProps = (List)hashProps.get(objSort);
		boolean newObjectSort = true;
		if (objProps != null) {
			newObjectSort = false;
			// We have already dealt with this sort of object check that all properties are present
			Iterator li = objProps.listIterator();
			while (li.hasNext()) {
				Property prop = (Property)li.next();
				Iterator setli = userObj.properties.entrySet().iterator();
				boolean found = false;
				boolean sortOK = false;
				while (setli.hasNext()) {
					Map.Entry ent = (Map.Entry)setli.next();
					if (ent.getValue() instanceof String && ent.getValue().equals(prop.name)) {
						String key = (String)ent.getKey();
						if (key.startsWith("Property")) {
							found = true;
							String suffix = key.substring("Property".length());
							String propSort = (String)userObj.getProperty("Property Type"+suffix);
							if (propSort != null && prop.argType.equals(propSort)) {
								sortOK = true;
							}
						}
					}
				}
				if (!found) {
					throw new OCLException("The property " + prop.name + " is not present in the state " + userObj.getProperty("label"));
				}
				if (!sortOK){
					throw new OCLException("The property " + prop.name + " is of a different type in state " + userObj.getProperty("label"));
				}
			}
		} else {
			// Make a property list for this sort
			newObjectSort = true;
			objProps = new ArrayList();
			hashProps.put(objSort,objProps);
		}
		// If we get here that all known properties for this sort of object are present
		String strProp = "Property";
		String strArg = "Property Type";
		int suffix = 2;
		
		String propName = (String) userObj.getProperty(strProp);
		while (propName != null) {
			Utility.addNewInOrder(propNames,propName.toLowerCase());
			if (!Utility.isLegalIdentifier(propName)){
				throw new OCLException("The property name [" + propName + "] is not legal - must start with lowercase letter and be followed by letters or digits or _");
			}
			Object vals = hashStatePreds.get(propName.toLowerCase());
			if (vals != null) {
				throw new OCLException("The property " + propName + " is also used as a state name.");
			}
			String propArgSort = (String) userObj.getProperty(strArg);
			Property prop = new Property(objSort,propName,propArgSort);
			if (listContainsProperty(objProps,prop)) {
				if (newObjectSort) {
					throw new OCLException("The property " + prop.name + " appears twice in the state " + userObj.getProperty("label"));
				}
			} else {
				if (newObjectSort){
					objProps.add(prop);
				} else {
					throw new OCLException("The property " + prop.name + " with argument type "+ prop.argType + " is present in state " + userObj.getProperty("label") + " but not other states of this sort.");
				}
			}
			propName = (String) userObj.getProperty("Property" + suffix);
			strArg = "Property Type" + suffix++;
		}
	}
	
	/**
	 * checkConnectedTransitionsHaveCorrectSort
	 * @param stateNode
	 * @throws OCLException when incorrect sort
	 */
	private void checkConnectedTransitionsHaveCorrectSort(StateCell stateNode) throws OCLException{
		List out = getOutwardTransitions(stateNode);
		String stateSort = LHUserObject.getObjectSort(stateNode);
		Iterator li = out.listIterator();
		while (li.hasNext()) {
			TransitionCell transNode = (TransitionCell)li.next();
			String transSort = transNode.getObjectSort();
			if (!transSort.equals(stateSort)) {
				throw new OCLException("The transition " + transNode.toString() + " does not have the correct object sort");
			}
		}
	}
	/**
	 * checkPropertiesContinuous
	 * check that suffix property numbers do not skip
	 * @param userObj
	 * @throws OCLException
	 */
	private void checkPropertiesContinuous(LHUserObject userObj) throws OCLException {
		Iterator setli = userObj.properties.entrySet().iterator();
		int noProperties = 0;
		while (setli.hasNext()) {
			Map.Entry ent = (Map.Entry)setli.next();
			if (ent.getKey() instanceof String) {
				if (((String)ent.getKey()).startsWith("Property")) {
					noProperties++;
				}
			}
		}
		// noProperties will be 2x number of actual properties found Property Type as well
		int count = 2;
		String suffix = "";
		while (noProperties > 0) {
			String prop = (String)userObj.getProperty("Property" + suffix);
			String sort = (String)userObj.getProperty("Property Type" + suffix);
			if (prop == null) {
				throw new OCLException("The property numbers are not continuous in state " + userObj.getProperty("label"));
			}
			if (sort == null) {
				throw new OCLException("The property Type numbers are not continuous in state " + userObj.getProperty("label"));
			}
			suffix = (new Integer(count)).toString();
			count++;
			noProperties -= 2;
			if (noProperties < 0) {
				throw new OCLException("The property numbers for  Property Types are incorrect in state" + userObj.getProperty("label"));
			}
		}
	}
	
	/**
	 * mergeRouteTo
	 * follow merge connections to check that given node is connected
	 * directly or indirectly to at least one of the nodes in the list of same
	 * Do depth first search
	 * @param source the target node to check.
	 * @param same - the nodes with the same name
	 * @param visited - nodes already searched
	 * @return true if connected
	 */
	private boolean mergeRouteTo(TransitionCell source, List same,List visited){
		String sourceName = LHUserObject.getLabel(source);
		Enumeration enum = source.children();
		visited.add(source);
		while ( enum.hasMoreElements() ) {
			DefaultPort port = (DefaultPort)enum.nextElement();
			Iterator li = port.edges();	
			while (li.hasNext()) {
				DefaultEdge edge = (DefaultEdge)li.next();
				if (edge instanceof MergeEdge) {
					DefaultGraphCell connTrans = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
					if (connTrans instanceof TransitionCell && !visited.contains(connTrans)) {
						if (connTrans != source) {
							//This is outwards bound
							if 	(same.contains(connTrans)) {
								return true;
							} else {
								if (mergeRouteTo((TransitionCell)connTrans,same,visited)) {
									return true;
								}
							}
						} 
					}
					// Try other end of connection
					connTrans = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
					if (connTrans instanceof TransitionCell && !visited.contains(connTrans)) {
						if (connTrans != source) {
							//This is outwards bound
							if 	(same.contains(connTrans)) {
								return true;
							} else {
								if (mergeRouteTo((TransitionCell)connTrans,same,visited)) {
									return true;
								}
							}
						} 
					}
				}
			}
		}
		return false;
	}
		
	/**
	 * exchangeDomainStructure used after a successful commit of pattern
	 * structure to set the current domain to hold the new ocl structure
	 * 
	 * @param tempDom -
	 *            the oclDomain storing the new structure
	 */
	protected void exchangeDomainStructure(oclDomain tempDom) {
		curDom.sorts = tempDom.sorts;
		//curDom.objects = tempDom.objects;
		curDom.predicates = tempDom.predicates;
		curDom.classDefs = tempDom.classDefs;
		//curDom.atomicInvars = tempDom.atomicInvars;
		//curDom.impliedInvars = tempDom.impliedInvars;
		//curDom.inconsistentConst = tempDom.inconsistentConst;
		curDom.operators = tempDom.operators;
		// Ron 3/10/04 allow existing tasks to be retained
		if (curDom.tasks.size() > 0) {
			int ret = JOptionPane.showConfirmDialog(null,
					"Retain domain tasks", "GIPO Query",
					JOptionPane.YES_NO_OPTION);
			if (ret != JOptionPane.YES_OPTION)
				curDom.tasks = tempDom.tasks;
		}
		if (curDom.atomicInvars.size() > 0) {
			int ret = JOptionPane.showConfirmDialog(null,
					"Retain instances of static predicates", "GIPO Query",
					JOptionPane.YES_NO_OPTION);
			if (ret == JOptionPane.YES_OPTION) {
				curDom.atomicInvars = tempDom.atomicInvars;
				// Need to install static predicate definitions
				Iterator li = curDom.atomicInvars.listIterator();
				while (li.hasNext()) {
					oclPredicate curStatic = (oclPredicate)li.next();
					if (curDom.getPredicateByName(curStatic.getName()) == null){
						curDom.addCompletePredicate(tempDom.getPredicateByName(curStatic.getName()));
					}
				}
			}
		}
		//curDom.methods = tempDom.methods;
		//curDom.htntasks = tempDom.htntasks;
	}
	/**
	 * class Property 
	 * used to store properties associated with object states
	 */
	class Property {
		String name = null;
		String argType = null;
		String sort = null;
		/**
		 * constructor 
		 * @param sort the object sort that this property qualifies
		 * @param name the name for the property
		 * @param argType the type of the property
		 */
		public Property(String sort, String name, String argType){
			this.sort = sort;
			this.name = name;
			this.argType = argType;
		}
	}
	
}
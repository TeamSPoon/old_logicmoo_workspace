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
 * Created on 16-May-2005
 *
 */
package jplan.tools.lifeHist;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import java_cup.runtime.Symbol;

import javax.swing.JOptionPane;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.GraphModel;

import jplan.general.OCLException;
import jplan.ocl.*;
import jplan.general.OCLSelectionException;
import jplan.tools.lifeHist.OclModel.Property;
import jplan.general.Utility;

/**
 * @author ron
 * 
 * Translates an Life History Model to ocl model
 */
public class OclPlusModel extends OclModel {

	public OclPlusModel(oclDomain dom) {
		super(dom);
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
		Hashtable hashFluents = new Hashtable();
		Hashtable hashStaticFluents = new Hashtable();
		Hashtable hashStatePropertyTransforms = new Hashtable();
		Hashtable hashAssociatedArgs = new Hashtable();
		List simpleTransitions = new ArrayList();
		List valueTransitions = new ArrayList();
		List mergeEdges = new ArrayList();
		List processes = new ArrayList();
		List events = new ArrayList();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				String sortName = LHUserObject
						.getObjectSort((StateCell) curRoot);
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
				collectStateProperties((LHUserObject) ((StateCell) curRoot)
						.getUserObject(), sortName, hashProperties);
				collectSortValues((LHUserObject) ((StateCell) curRoot)
						.getUserObject(), sortName, hashFluents, "Fluent");
				collectSortValues((LHUserObject) ((StateCell) curRoot)
						.getUserObject(), sortName, hashStaticFluents,
						"StaticFluent");
				collectStatePropertyTransforms(
						(LHUserObject) ((StateCell) curRoot).getUserObject(),
						stateName, hashStatePropertyTransforms);
			} else if (TransitionCell.isStateTransitionCell(curRoot)) {
				simpleTransitions.add(curRoot);
			} else if (TransitionCell.isValueTransitionCell(curRoot)) {
				valueTransitions.add(curRoot);
				collectValueArgs((TransitionCell) curRoot, hashValueArgs);
				//collectFluentExpressions((TransitionCell) curRoot, hashFExprs);
			} else if (curRoot instanceof MergeEdge) {
				collectAssociationArgs((MergeEdge) curRoot, hashAssociatedArgs);
				mergeEdges.add(curRoot);
			} else if (curRoot instanceof ProcessCell) {
				processes.add((ProcessCell) curRoot);
			}
			if (TransitionCell.isEventCell(curRoot)) {
				events.add((TransitionCell) curRoot);
			}
		}
		simpleTransitions = topSortTransitions(simpleTransitions, mergeEdges);
		buildDomain(tempDom, hashStatePreds, hashValueArgs, hashProperties,
				hashFluents, hashStaticFluents, hashAssociatedArgs,
				hashStatePropertyTransforms, simpleTransitions,
				valueTransitions, mergeEdges, processes, events);
		exchangeDomainStructure(tempDom);
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
	 * @param hashFluents
	 *            sort value Fluent associations
	 * @param hashStatePropertyTransforms
	 *            state name propert associations for properties changing in
	 *            this state
	 * @param hashAssocs
	 *            Association arguments indexed by state name
	 * @param simpleTransitions -
	 *            the non value transforming transition names
	 * @param valueTransitions -
	 *            the value changing transition names
	 * @param mergeEdges -
	 *            list of merge graph edges
	 * @para. process - list of process nodes
	 * @para. events - list of event nodes - Note these will also feature as
	 *        Transition nodes
	 */
	protected void buildDomain(oclDomain tempDom, Hashtable hashStates,
			Hashtable hashArgs, Hashtable hashProperties,
			Hashtable hashFluents, Hashtable hashStaticFluents,
			Hashtable hashAssocs,
			Hashtable hashStatePropertyTransforms, List simpleTransitions,
			List valueTransitions, List mergeEdges, List processes, List events) {
		oclSort prims = tempDom.addSort("primitive_sorts");
		//Hashtable hashStateMergeArgs = new Hashtable();
		//buildMergeArgsMap(mergeEdges, hashStateMergeArgs);
		Set sortKeys = hashStates.keySet();
		Iterator li = sortKeys.iterator();
		while (li.hasNext()) {
			String sortName = (String) li.next();
			oclSSClassDef def = tempDom.getStates(sortName);
			if (def == null) {
				prims.addSubType(sortName);
				def = tempDom.addClassDef(sortName, oclPredicate
						.toVar(sortName));
			}
			List states = (List) hashStates.get(sortName);
			Iterator liStates = states.listIterator();
			while (liStates.hasNext()) {
				String stateName = (String) liStates.next();
				oclPredicate curStatePred = tempDom.addPredicate(stateName);
				curStatePred.addConstArgument(sortName);
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
				addStateProperties(tempDom, prims, sList, sortName,
						hashProperties);
				List transforms = (List) hashStatePropertyTransforms
						.get(stateName);
				if (transforms != null) {
					addTransformPropertyChanges(tempDom, sList, sortName,
							stateName, transforms);
				}
				addStateFluents(tempDom, sList, sortName, hashFluents);
			}
		}
		buildStaticFluents(tempDom, hashStaticFluents);
		buildSimpleTransitions(tempDom, simpleTransitions, mergeEdges);
		buildValueTransitions(tempDom, valueTransitions,
				hashStatePropertyTransforms,mergeEdges);
		buildProcesses(tempDom, processes);
		upGradeActionsToEvents(tempDom, events);
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
			oclSCPlus trans = new oclSCPlus(objSort, oclPredicate.toVar(objSort));
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

	/**
	 * buildValueTransitions
	 * 
	 * @param tempDom
	 * @param valueTransitions
	 * @param hashStatePropertyTransforms
	 * @param mergeEdges
	 */
	protected void buildValueTransitions(oclDomain tempDom,
			List valueTransitions, Hashtable hashStatePropertyTransforms,
			List mergeEdges) {
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
			oclSCPlus trans = new oclSCPlus(objSort, oclPredicate
					.toVar(objSort));
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
							lhsState = (oclStateList) state.clone();
							lhsPred = (oclPredicate) pred.clone();
						} catch (CloneNotSupportedException ex) {
							// Should not happen
						}
					}
					if (pred.getName().equals(destStateName)) {
						// Better Clone it
						try {
							rhsState = (oclStateList) state.clone();
							rhsPred = (oclPredicate) pred.clone();
						} catch (CloneNotSupportedException ex) {
							// Should not happen
						}
					}
				}
			}
			Utility.debugPrintln("patterns", rhsPred.toString());
			List sourceTransforms = (List) hashStatePropertyTransforms
					.get(sourceStateName);
			List destTransforms = (List) hashStatePropertyTransforms
					.get(destStateName);
			if (sourceTransforms == null && destTransforms == null) {
				addValueTransitionGraphDetails(tempDom, (TransitionCell) node,
						trans, lhsState, rhsState);
			} else {
				addTransformingValueTransitionGraphDetails(tempDom,
						(TransitionCell) node, trans, lhsState, rhsState,
						sourceTransforms, destTransforms);
			}
			List fexprs = collectFluentExpressions((TransitionCell)node);
			if (fexprs.size() > 0) {
				addFluentExpressions(tempDom, (TransitionCell) node, trans,
						lhsState, rhsState, fexprs);
			}
			// Now create the operator
			// If this is a disjunction there will be multiple operators to
			// create
			if (((TransitionCell) node).isDisjunction()) {
				List opNames = fetchDisjOpNames((TransitionCell) node);
				Iterator liOpNames = opNames.listIterator();
				while (liOpNames.hasNext()) {
					String opName = (String) liOpNames.next();
					createOperator(opName, tempDom, (TransitionCell) node,
							trans, objSort, noConsts, constPred, mergeEdges);
				}
			} else {
				createOperator(node.toString(), tempDom, (TransitionCell) node,
						trans, objSort, noConsts, constPred, mergeEdges);
			}

		}
	}

	public static oclStateList getStateListForState(oclDomain dom, String sort,
			String stateName) {
		oclSSClassDef classDef = null;
		try {
			classDef = dom.getStateListForSort(sort);
		} catch (OCLNoSuchElementException e) {
			//Should not happen
			System.out
					.println("ERROR [OclPlusModel] cannot find state definition");
			return null;
		}
		Iterator liStates = classDef.getStateList().listIterator();
		while (liStates.hasNext()) {
			oclStateList state = (oclStateList) liStates.next();
			List preds = state.getPredicateList();
			Iterator liPreds = preds.listIterator();
			while (liPreds.hasNext()) {
				oclPredicate pred = (oclPredicate) liPreds.next();
				if (pred.getName().equals(stateName)) {
					// Better Clone it
					oclStateList lhsState;
					try {
						lhsState = (oclStateList) state.clone();
					} catch (CloneNotSupportedException ex) {
						// Should not happen
						return null;
					}
					return lhsState;
				}
			}
		}
		// Should not get here
		return null;
	}

	/**
	 * collectStateFluents collect the fluent predicates associated with this
	 * object sort
	 * 
	 * @param userObj
	 * @param objSort
	 * @param hashFluents
	 * @paral key - the property key to look for
	 */
	protected void collectSortValues(LHUserObject userObj, String objSort,
			Hashtable hashTable, String key) {
		int suffix = 2;
		List objFluents = null;
		String fluentName = (String) userObj.getProperty(key);
		while (fluentName != null) {
			objFluents = (List) hashTable.get(objSort);
			if (objFluents == null) {
				objFluents = new ArrayList();
				objFluents.add(fluentName);
			} else {
				if (!Utility.listContainsString(fluentName, objFluents)) {
					objFluents.add(fluentName);
				}
			}
			fluentName = (String) userObj.getProperty(key + suffix++);
		}
		if (objFluents != null) {
			hashTable.put(objSort, objFluents);
		}
	}

	/**
	 * collectStatePropertyTransforms collect the properties that this state
	 * transforms
	 * 
	 * @param userObj
	 * @param sortName
	 * @param hashStatePTransforms
	 */
	protected void collectStatePropertyTransforms(LHUserObject userObj,
			String stateName, Hashtable hashStatePTranforms) {
		String strTrans = "Transform";
		int suffix = 2;
		List transforms = null;
		String propertyName = (String) userObj.getProperty(strTrans);
		while (propertyName != null) {
			transforms = (List) hashStatePTranforms.get(stateName);
			if (transforms == null) {
				transforms = new ArrayList();
				transforms.add(propertyName);
			} else {
				if (!Utility.listContainsString(propertyName, transforms)) {
					transforms.add(propertyName);
				}
			}
			propertyName = (String) userObj.getProperty(strTrans + suffix++);
		}
		if (transforms != null) {
			hashStatePTranforms.put(stateName, transforms);
		}
	}

	/**
	 * collectFluentExpressions collect the transitions fluent expressions
	 * 
	 * @param cell -
	 *            the transition node
	 */
	protected List collectFluentExpressions(TransitionCell cell) {
		List fexprs = new ArrayList();
		LHUserObject userObj = (LHUserObject) cell.getUserObject();
		String transName = LHUserObject.getLabel(cell);
		int suffix = 2;
		String expression = (String) userObj.getProperty("FExpr");
		while (expression != null) {
			fexprs.add(expression);
			expression = (String) userObj.getProperty("FExpr" + suffix++);
		}
		return fexprs;
	}

	/**
	 * addTransformPropertyChanges find transforming property in state list and
	 * replase with standard predicate of the form transforming <Property>(
	 * <PropertySort><PropertySort>)
	 * 
	 * @param tempDom
	 * @param sList
	 * @param sortname
	 * @param stateName
	 * @param transforms
	 */
	private void addTransformPropertyChanges(oclDomain tempDom,
			oclStateList sList, String objSort, String stateName,
			List transforms) {
		List newSList = new ArrayList();
		Iterator li = sList.getPredicateList().listIterator();
		while (li.hasNext()) {
			oclPredicate statePred = (oclPredicate) li.next();
			Iterator liTransform = transforms.listIterator();
			boolean found = false;
			while (liTransform.hasNext()) {
				String propName = (String) liTransform.next();
				if (propName.equals(statePred.getName())) {
					found = true;
					String propSort = oclPredicate.toConst(statePred
							.getNthElementName(1));
					try {
						oclPredicate proto = tempDom
								.findPrototype("transforming" + propName);
					} catch (OCLSelectionException e) {
						// Need to add prototype
						oclPredicate transPred = new oclPredicate(
								"transforming" + propName);
						transPred.addConstArgument(objSort);
						transPred.addConstArgument(propSort);
						transPred.addConstArgument(propSort);
						tempDom.addCompletePredicate(transPred);
					}
					oclPredicate transPred = new oclPredicate("transforming"
							+ propName);
					transPred.addConstArgument(oclPredicate.toVar(objSort));
					transPred.addConstArgument(oclPredicate.toVar(propSort));
					transPred.addConstArgument(oclPredicate.toVar(propSort));
					newSList.add(transPred);
				}
			}
			if (!found) {
				newSList.add(statePred);
			}
		}
		sList.setPredicateList(newSList);
	}

	/**
	 * addTransformValueTransitionGraphDetails add predicates (if necessary) to
	 * select next value for value changing parameter adjust rhs and lhs
	 * predicates to reflect new value arguments
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
	 * @param sourceTransforms -
	 *            list of transforms for source state
	 * @param destTransforms -
	 *            list of transforms for destination state
	 */
	protected void addTransformingValueTransitionGraphDetails(
			oclDomain tempDom, TransitionCell node, oclSC trans,
			oclStateList lhsState, oclStateList rhsState,
			List sourceTransforms, List destTransforms) {
		// There may be multiple value changes in this transition
		List connectPreds = new ArrayList();
		int iSuffix = 1;
		String strSuffix = "";
		boolean look = true;
		while (look) {
			// propertyName is the property name
			String propertyName = (String) ((LHUserObject) node.getUserObject())
					.getProperty("ValueSort" + strSuffix);
			if (propertyName != null) {
				// Get property predicate
				oclPredicate propertyPred = tempDom
						.getPredicateByName(propertyName);
				String arg = oclPredicate.toVar(propertyPred
						.getNthElementName(1));
				boolean fullyConnected = "fullyConnected"
						.equals((String) ((LHUserObject) node.getUserObject())
								.getProperty("EdgeName" + strSuffix));
				if (fullyConnected) {
					if (sourceTransforms != null
							&& sourceTransforms.contains(propertyName)) {
						oclPredicate transPred = lhsState
								.getPredicateByName("transforming"
										+ propertyName);
						try {
							transPred.replaceVariableNo(1, arg + "A");
							transPred.replaceVariableNo(2, arg + "B");
						} catch (Exception e) {
							System.out
									.println("ERROR OClPlus missing transform predicate");
						}
					} else {
						lhsState.renamePredArgInState(propertyPred.getName(),
								arg, arg + "A");
					}
					if (destTransforms != null
							&& destTransforms.contains(propertyName)) {
						oclPredicate transPred = rhsState
								.getPredicateByName("transforming"
										+ propertyName);
						try {
							transPred.replaceVariableNo(1, arg + "A");
							transPred.replaceVariableNo(2, arg + "B");
						} catch (Exception e) {
							System.out
									.println("ERROR OClPlus missing transform predicate");
						}
					} else {
						rhsState.renamePredArgInState(propertyPred.getName(),
								arg, arg + "B");
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
						connectPred.replaceVariableNo(0, arg + "A");
						connectPred.replaceVariableNo(1, arg + "B");
					} catch (Exception ex) {
					}
					if (sourceTransforms != null
							&& sourceTransforms.contains(propertyName)) {
						oclPredicate transPred = lhsState
								.getPredicateByName("transforming"
										+ propertyName);
						try {
							transPred.replaceVariableNo(1, arg + "A");
							transPred.replaceVariableNo(2, arg + "B");
						} catch (Exception e) {
							System.out
									.println("ERROR OClPlus missing transform predicate");
						}
					} else {
						lhsState.renamePredArgInState(propertyPred.getName(),
								arg, arg + "A");
					}
					if (destTransforms != null
							&& destTransforms.contains(propertyName)) {
						oclPredicate transPred = rhsState
								.getPredicateByName("transforming"
										+ propertyName);
						try {
							transPred.replaceVariableNo(1, arg + "A");
							transPred.replaceVariableNo(2, arg + "B");
						} catch (Exception e) {
							System.out
									.println("ERROR OClPlus missing transform predicate");
						}
					} else {
						rhsState.renamePredArgInState(propertyPred.getName(),
								arg, arg + "B");
					}
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
			oclPredicate curPred = (oclPredicate) liStates.next();
			trans.addPre(curPred);
		}
		liStates = rhsState.getPredicateList().listIterator();
		while (liStates.hasNext()) {
			oclPredicate curPred = (oclPredicate) liStates.next();
			trans.addPost(curPred);
		}
		Iterator li = connectPreds.listIterator();
		while (li.hasNext()) {
			oclPredicate connectPred = (oclPredicate) li.next();
			trans.addPre(connectPred);
		}
	}

	/**
	 * addStateFluents add the fluent predicates to states
	 * 
	 * @param tempDom
	 * @param sList
	 * @param sortName
	 * @param hashFluents
	 */
	protected void addStateFluents(oclDomain tempDom, oclStateList sList,
			String sortName, Hashtable hashFluents) {
		List fluents = (List) hashFluents.get(sortName);
		if (fluents != null) {
			Iterator li = fluents.listIterator();
			while (li.hasNext()) {
				String fluent = (String) li.next();
				oclPredicate newFunc = null;
				if ((tempDom.getFunctorByName(fluent)) == null) {
					newFunc = tempDom.addFunctor(fluent);
					newFunc.addConstArgument(sortName);
				}
				oclFunctor cur = new oclFunctor(fluent);
				cur.addConstArgument(oclPredicate.toVar(sortName));
				sList.addPredicate(cur);
			}
		}
	}

	/**
	 * addFluentExpressions add expressions from given list of expressions test
	 * expressions to LHS all others to RHS
	 * 
	 * @param tempDom
	 * @param node
	 * @param trans
	 * @param lhsState
	 * @param rhsState
	 * @param fexprs
	 */
	protected void addFluentExpressions(oclDomain tempDom, TransitionCell node,
			oclSCPlus trans, oclStateList lhsState, oclStateList rhsState,
			List fexprs) {
		Iterator li = fexprs.listIterator();
		while (li.hasNext()) {
			String strExpr = (String) li.next();
			oclExpression curExp = parseFExpr(strExpr);
			if (curExp.getExprtType() == oclExpression.TEST) {
				trans.addCondition(curExp);
			} else {
				trans.addUpdate(curExp);
				oclExpression lhs = curExp.getLHS();
				oclPredicate lhsFun = (oclPredicate) lhs.getValue();
				List rhsList = trans.getPost();
				Iterator liRhs = rhsList.listIterator();
				int inx = -1;
				int count = 0;
				while (inx == -1 && liRhs.hasNext()) {
					oclPredicate curPred = (oclPredicate) liRhs.next();
					if (curPred.getName().equals(lhsFun.getName())) {
						inx = count;
					}
					count++;
				}
				if (inx != -1)
					rhsList.remove(inx);
			}
		}
	}

	/**
	 * parseFExpr parses the stored string expression. Should not fail as
	 * expression has already been parsed when added to property list
	 * 
	 * @param exp
	 * @return
	 */
	public oclExpression parseFExpr(String exp) {
		Symbol parsRes = null;
		oclExpression curExp = null;
		try {
			StringReader sread = new StringReader(exp);
			parsRes = new parser(new Yylex(sread)).parse();
			curExp = (oclExpression) (parsRes.value);
			return curExp;
		} catch (Exception e) {
			// Should not happen
			System.out.println("ERROR : Stored expresion does not parse \n"
					+ exp);
		}
		return null;
	}

	/**
	 * buildProcesses build the ocl processes from the graph process nodes
	 * 
	 * @param tempDom
	 * @param processes
	 */
	private void buildProcesses(oclDomain tempDom, List processes) {
		Iterator li = processes.listIterator();
		while (li.hasNext()) {
			ProcessCell curProcessCell = (ProcessCell) li.next();
			oclProcess curProcess = tempDom.addProc();
			oclPredicate opName = curProcess.addName(curProcessCell.getLabel());
			LHUserObject user = (LHUserObject) curProcessCell.getUserObject();
			List stateSources = getProcessStateSources(curProcessCell);
			List transitionEdgeSources = getProcessTransitionSources(curProcessCell);
			oclSCPlus curSCPlus = null;
			if (stateSources.size() == 1) {
				// State forms LHS of update
				StateCell source = (StateCell) stateSources.get(0);
				String sort = source.getObjectSort();
				String ID = oclPredicate.toVar(sort);
				curSCPlus = curProcess.addNecSCPlus(sort, ID);
				// Get the state definition
				oclStateList lhsState = getStateListForState(tempDom, sort,
						source.getLabel());
				Iterator liStates = lhsState.getPredicateList().listIterator();
				while (liStates.hasNext()) {
					oclPredicate curPred = (oclPredicate) liStates.next();
					curPred = curPred.destinguishVarsAlpha();
					// We only want the state name predicate and 
					// Property transforming predicate
					String predName = curPred.getName();
					if (predName.equals(LHUserObject.getLabel(source)) ||
							(predName.startsWith("transforming"))) {
						curSCPlus.addPre(curPred);
					}
				}
				// Now add fluent expressions
				String expression = (String) user.getProperty("FExpr");
				int suffix = 2;
				while (expression != null) {
					oclExpression curFExp = parseFExpr(expression);
					if (curFExp.getExprtType() == oclExpression.TEST) {
						curSCPlus.addCondition(curFExp);
					} else {
						curSCPlus.addUpdate(curFExp);
					}
					expression = (String) user.getProperty("FExpr" + suffix++);
				}

			}
			List scDone = new ArrayList();
			Iterator eli = transitionEdgeSources.listIterator();
			while (eli.hasNext()) {
				Object[] sourceElements = (Object[]) eli.next();
				LHUserObject eUser = (LHUserObject) ((ProcessEdge) sourceElements[0])
						.getUserObject();
				StateCell sourceState = (StateCell) sourceElements[2];
				LHUserObject sUser = (LHUserObject) sourceState.getUserObject();
				String sort = sourceState.getObjectSort();
				if (Utility.listContainsString(sort,scDone)) {
					break; // already added transitionfor this sort
				} else {
					scDone.add(sort);
				}
				String ID = oclPredicate.toVar(sort);
				curSCPlus = curProcess.addNecSCPlus(sort, ID);
				//Add property preconditions
				String propName = (String) eUser.getProperty("Precondition");
				int suffix = 2;
				while (propName != null) {
					oclPredicate propPred = null;
					try {
						propPred = tempDom.findPrototype(propName);
					} catch (Exception ex) {
						System.out
								.println("ERROR [OclPlusModel] cannot find property prototype.");
						break;
					}
					propPred = propPred.promoteSortsToVars();
					try {
						propPred.replaceVariableNo(1,"true");
					} catch (Exception ex) {
						System.out
								.println("ERROR [OclPlusModel] cannot insert boolean value.");
						break;
					}
					curSCPlus.addPre(propPred);
					propName = (String) eUser.getProperty("Precondition"
							+ suffix++);
				}
				//					 Now add fluent expressions
				String expression = (String) user.getProperty("FExpr");
				suffix = 2;
				while (expression != null) {
					oclExpression curFExp = parseFExpr(expression);
					if (curFExp.getExprtType() == oclExpression.TEST) {
						curSCPlus.addCondition(curFExp);
					} else {
						curSCPlus.addUpdate(curFExp);
					}
					expression = (String) user.getProperty("FExpr" + suffix++);
				}
			}
			oclPredicate sig = tempDom.createOperatorSignature(curProcess);
			curProcess.setName(sig);
		}

	}

	/**
	 * getProcessStateSources find source states ie LHS of process
	 * 
	 * @param node -
	 *            process node
	 * @return List
	 */
	protected List getProcessStateSources(ProcessCell node) {
		List sources = new ArrayList();
		int noChildren = node.getChildCount();
		for (int i = 0; i < noChildren; i++) {
			Object obj = node.getChildAt(i);
			if (obj instanceof DefaultPort) {
				DefaultPort curPort = (DefaultPort) obj;
				Set edgeSet = ((DefaultPort) obj).getEdges();
				Iterator setIt = edgeSet.iterator();
				while (setIt.hasNext()) {
					DefaultEdge edge = (DefaultEdge) setIt.next();
					if (edge instanceof ProcessEdge
							&& edge.getSource() != curPort) {
						DefaultGraphCell sCell = (DefaultGraphCell) ((DefaultPort) edge
								.getSource()).getParent();
						if (sCell instanceof StateCell) {
							sources.add((StateCell) sCell);
						}
					}
				}
			}
		}
		return sources;
	}

	/**
	 * getProcessTransitionSources find source edges from transitions ie LHS of
	 * process must contain boolean trigger
	 * 
	 * @param node -
	 *            process node
	 * @return a list of three element arrays 0 - edge 1 transition 2 state
	 */
	protected List getProcessTransitionSources(ProcessCell node) {
		List sources = new ArrayList();
		int noChildren = node.getChildCount();
		for (int i = 0; i < noChildren; i++) {
			Object obj = node.getChildAt(i);
			if (obj instanceof DefaultPort) {
				DefaultPort curPort = (DefaultPort) obj;
				Set edgeSet = ((DefaultPort) obj).getEdges();
				Iterator setIt = edgeSet.iterator();
				while (setIt.hasNext()) {
					DefaultEdge edge = (DefaultEdge) setIt.next();
					if (edge instanceof ProcessEdge
							&& edge.getSource() != curPort) {
						DefaultGraphCell tCell = (DefaultGraphCell) ((DefaultPort) edge
								.getSource()).getParent();
						if (tCell instanceof TransitionCell) {
							StateCell sCell = getStateSource((TransitionCell) tCell);
							Object[] element = new Object[] { edge, tCell,
									sCell };
							sources.add(element);
						}
					}
				}
			}
		}
		return sources;
	}

	/**
	 * upGradeActionsToEvents turn the already constructed action definitions to
	 * event definitions
	 * 
	 * @param tempDom
	 * @param events
	 */
	private void upGradeActionsToEvents(oclDomain tempDom, List events) {
		Iterator li = events.listIterator();
		List delOps = new ArrayList();
		List doneEvents = new ArrayList();
		while (li.hasNext()) {
			TransitionCell curEventCell = (TransitionCell) li.next();
			String eventName = LHUserObject.getLabel(curEventCell);
			if (!Utility.listContainsString(eventName,doneEvents)) {
				Iterator liOps = tempDom.operators.listIterator();
				while (liOps.hasNext()) {
					oclOperator op = (oclOperator) liOps.next();
					if (op.opName.getName().equals(eventName)) {
						// Found event upgrade
						oclEvent event = new oclEvent(op);
						tempDom.addCompleteEvent(event);
						delOps.add(op);
						doneEvents.add(eventName);
					}
				}
			}
		}
		li = delOps.listIterator();
		while (li.hasNext()) {
			oclOperator op = (oclOperator) li.next();
			tempDom.removeOperator(op);
		}
	}

	/**
	 * buildStaticFluents add static fluents to the domain functors list
	 * 
	 * @param tempDom
	 * @param hashStaticFluents
	 */
	private void buildStaticFluents(oclDomain tempDom,
			Hashtable hashStaticFluents) {
		Set sortKeys = hashStaticFluents.keySet();
		Iterator li = sortKeys.iterator();
		while (li.hasNext()) {
			String objSort = (String) li.next();
			List fluents = (List) hashStaticFluents.get(objSort);
			Iterator liFluents = fluents.listIterator();
			while (liFluents.hasNext()) {
				String fluentName = (String) liFluents.next();
				oclPredicate curFluent = tempDom.addFunctor(fluentName);
				curFluent.setStatic(true);
				curFluent.addConstArgument(objSort);
			}
		}

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
		curDom.functors = tempDom.functors;
		curDom.processes = tempDom.processes;
		curDom.events = tempDom.events;

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
		//curDom.methods = tempDom.methods;
		//curDom.htntasks = tempDom.htntasks;
	}

}
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
 *
 *
 *
 * Created on 13-Sep-2003
 *
 * Author ron
 * 
 */
package jplan.tools.sketch;

import java.awt.event.*;
import javax.swing.event.*;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Vector;

import jplan.top.OclEd;
import jplan.ocl.*;
import jplan.general.*;

/**
 * @author ron
 *
 * This class maintains the model of the evolving plan
 * and interacts with the edit canvas to keep the view in step.
 */
public class PlanController implements ChangeListener {
	/*
	 * Domain elements
	 */
	private oclDomain curDomain;
	
	/*
	 * Interface Elements
	 */
	private OclEd top;
	private EditCanvas editCanvas;
	
	/* 
	 * Plan Elements
	 */
	private oclTask curTask = null;
	private List initState;
	private List possibleProcesses = null; // All instantiations of processes
	private List possibleEvents = null; //All instantiations of events
	private PlanState curState;   // The current state
	private PlanState resultState; // the resultant state from advancing an operator or an event
	private List activeProcesses; // List of all running processes
	private List stoppedProcesses; //Processes that have been stopped - they still have a time line
	// Note this stores the drawabe objects with the process associated(added)
	private int sliderTime = 0; //Set when slider moved
	 /**
	 * @author ron
	 *
	 * To change the template for this generated type comment go to
	 * Window>Preferences>Java>Code Generation>Code and Comments
	 */
	 public PlanController(OclEd top, oclDomain curDom) {
	 	this.top = top;
	 	curDomain = curDom;
	 	activeProcesses = new ArrayList();
	 	stoppedProcesses = new ArrayList();
	 }
	 
	 /**
	  * setEditCanvas
	  * @param the edit Canvas
	  */
	 public void setEditCanvas(EditCanvas edit) {
	 	editCanvas = edit;
	 	editCanvas.addTimeListener(this);
	 }
	 
	 public void newTask(oclTask curTask) {
	 	this.curTask = curTask;
	 	curState = new PlanState(curTask,curDomain); 
	 	curState.setTime(0);
	 	activeProcesses.clear();
	 }
	 
	/**
	 * showInstantiatedOperator
	 * test that operator is valid in current state
	 * if valid apply and display
	 * otherwise show error message
	 * @param an instantiated operator
	 */
	public void showInstantiatedOperator(oclOperator op){
		// store the list of objects affected
		List lstObjStates = new ArrayList();
		List lstObjs = new ArrayList();
		List mssgs = new ArrayList(); // possible error mesages
		// get the object affected by this operator ignoring cond effects for the moment
		curState.setTime(sliderTime); // Make certain that time has caught up with visable time
		ListIterator liObjs = op.getPrevail().listIterator();
		while (liObjs.hasNext()) {
			oclSEPlus curSE = (oclSEPlus)liObjs.next();
			DrawableObject obj = null;
			if (curState.objectStateMatches(curSE,mssgs)) {
				obj = getKnownObject(curSE.getName());
				lstObjStates.add(curState.getObjectState(curSE.getName()));
			} else {
				CheckResultFrame.showMessages(top,mssgs,"State Advance Error");
				return;
			}
			if (obj != null)
				lstObjs.add(obj);
		}
		liObjs = op.getNecessary().listIterator();
		while (liObjs.hasNext()) {
			oclSCPlus curSC = (oclSCPlus)liObjs.next();
			DrawableObject obj = null;
			if (curState.objectStateMatches(curSC,mssgs)) {
				obj = getKnownObject(curSC.getName());
				lstObjStates.add(curState.getObjectState(curSC.getName()));
			} else {
				CheckResultFrame.showMessages(top,mssgs,"State Advance Error");
				return;
			}
			if (obj != null)
				lstObjs.add(obj);
		}
		// Ron 23/05/05 Deal with conditional effects
		ListIterator liSC = op.getConditional().listIterator();
		List condMatches = new ArrayList();
		while (liSC.hasNext()) {
			oclSCPlus curSC = (oclSCPlus)liSC.next();
			try {
				curSC = (oclSCPlus)curSC.clone();
			} catch (CloneNotSupportedException ex){}
			DrawableObject obj = null;
			List matches = curState.objectStateMatchesConditional(curSC);
			ListIterator mli = matches.listIterator();
			while (mli.hasNext()) {
				Object[] match = (Object[])mli.next();
				oclSCPlus matchSC = (oclSCPlus)match[0];
				obj = getKnownObject(matchSC.getName());
				lstObjStates.add(curState.getObjectState(matchSC.getName()));
				if (obj != null) {
					lstObjs.add(obj);
					condMatches.add(matchSC);
				}
			}
		}
		// If we have got here then the operator is OK
		TaskObject taskObj = new TaskObject(op.opName.toString(),"none");
		taskObj.setObject(op,TaskObject.ACTION);
		taskObj.setObjectStates(lstObjStates);
		editCanvas.addAction(taskObj,lstObjs);
		advanceState(op,condMatches,taskObj);
	}
	
	/**
	 * advanceState 
	 * @param op
	 * @param matches
	 * @param taskObj
	 */
	private void advanceState(oclOperator op,List matches,TaskObject taskObj) {
		if (resultState == null) {
			resultState = new PlanState(curState);
		} else {
			// Operators applied at same instant
			// Check that not mutexed
			//TODO
		}
		ListIterator liObjs = op.getNecessary().listIterator();
		while (liObjs.hasNext()) {
			oclSCPlus curSC = (oclSCPlus)liObjs.next();
			resultState.makeTransition(curSC);
			taskObj.addResultToResultState(resultState.getObjectState(curSC.getName())); // Ron 24/05/05
		}
		advanceCondMatches(matches,taskObj);   // Ron  24/05/05
		resultState.refreshFluentList();
		startProcesses(resultState.getTime());
		stopProcesses(resultState.getTime());
		resultState.setTime(curState.getTime());
	}
	
	/**
	 * advanceCondMatches
	 * advance state for each matching conditional clause
	 * @param matches
	 */
	private void advanceCondMatches(List matches,TaskObject taskObj) {
		ListIterator li = matches.listIterator();
		while (li.hasNext()) {
			oclSCPlus curSC = (oclSCPlus)li.next();
			resultState.makeTransition(curSC);
			taskObj.addResultToResultState(resultState.getObjectState(curSC.getName())); // Ron 24/05/05
		}
	}
	
	/**
	 * showInstantiatedEvents
	 * test that event is valid in current state
	 * if valid apply and display
	 * otherwise ignore
	 * @param an instantiated event
	 * @return - true if event fired
	 */
	public boolean showInstantiatedEvent(oclEvent evnt,PlanState tmpState,int moment){
		// store the list of objects affected
		List lstObjStates = new ArrayList();
		List lstObjs = new ArrayList();
		List mssgs = new ArrayList(); // possible error mesages will be ignored
		// get the object affected by this operator ignoring cond effects for the moment
		ListIterator liObjs = evnt.getPrevail().listIterator();
		while (liObjs.hasNext()) {
			oclSEPlus curSE = (oclSEPlus)liObjs.next();
			DrawableObject obj = null;
			if (tmpState.objectStateMatches(curSE,mssgs)) {
				obj = getKnownObject(curSE.getName());
				lstObjStates.add(tmpState.getObjectState(curSE.getName()));
			} else {
				return false; //Didn't match just stop
			}
			if (obj != null)
				lstObjs.add(obj);
		}
		liObjs = evnt.getNecessary().listIterator();
		while (liObjs.hasNext()) {
			oclSCPlus curSC = (oclSCPlus)liObjs.next();
			DrawableObject obj = null;
			if (tmpState.objectStateMatches(curSC,mssgs)) {
				obj = getKnownObject(curSC.getName());
				lstObjStates.add(tmpState.getObjectState(curSC.getName()));
			} else {
				return false; //Didn't match just stop
			}
			if (obj != null)
				lstObjs.add(obj);
		}
		// If we have got here then the Event is fired
		//  Ron 23/05/05 Deal with conditional effects
		ListIterator liSC = evnt.getConditional().listIterator();
		List condMatches = new ArrayList();
		while (liSC.hasNext()) {
			oclSCPlus curSC = (oclSCPlus)liSC.next();
			try {
				curSC = (oclSCPlus)curSC.clone();
			} catch (CloneNotSupportedException ex){}
			DrawableObject obj = null;
			List matches = curState.objectStateMatchesConditional(curSC);
			ListIterator mli = matches.listIterator();
			while (mli.hasNext()) {
				Object[] match = (Object[])mli.next();
				oclSCPlus matchSC = (oclSCPlus)match[0];
				obj = getKnownObject(matchSC.getName());
				lstObjStates.add(curState.getObjectState(matchSC.getName()));
				if (obj != null) {
					lstObjs.add(obj);
					condMatches.add(matchSC);
				}
			}
		}
		TaskObject taskObj = new TaskObject(evnt.opName.toString(),"none");
		taskObj.setObject(evnt,TaskObject.EVENT);
		taskObj.setObjectStates(lstObjStates);
		editCanvas.addEvent(taskObj,lstObjs,moment);
		advanceState(evnt,tmpState,moment,condMatches,taskObj);
		return true;
		
	}
	
	/**
	 * startProcesses
	 * See if action change requires any processes to start
	 */
	private void startProcesses(int instance){
		if (possibleProcesses == null)
			createPossibleProcessList(curTask);
		ListIterator li = possibleProcesses.listIterator();
		while (li.hasNext()) {
			oclProcess proc = (oclProcess)li.next();
			Utility.debugPrintln("patterns","PROC" + proc.toString());
			showProcess(proc,instance);
		}
	}
	
	/**
	 * stopProcess
	 * See if any active processes are stopped by applied actions/events
	 */
	private void stopProcesses(int instance) {
		ListIterator li = activeProcesses.listIterator();
		ArrayList procsToStop = new ArrayList();
		List mssgs = new ArrayList();
		boolean stop = false;
		int inx = 0;
		while (li.hasNext()) {
			TaskObject procLine = (TaskObject)li.next();
			oclProcess proc = (oclProcess)procLine.getObject();
			ListIterator liSE = proc.getPrevail().listIterator();
			while (liSE.hasNext()) {
				oclSEPlus se = (oclSEPlus)liSE.next();
				if (!resultState.objectStateMatches(se,mssgs)) {
					stop = true;
				}
			}
			ListIterator liSC = proc.getNecessary().listIterator();
			while (liSC.hasNext()) {
				oclSCPlus sc = (oclSCPlus)liSC.next();
				if (!resultState.objectStateMatches(sc,mssgs)) {
					stop = true;
				}
			}
			if (stop) {
				editCanvas.stopProcess(procLine,instance);
				stoppedProcesses.add(procLine);
				procsToStop.add(new Integer(inx));
			} 
			inx++;
			stop = false;
		}
		if (procsToStop.size() > 0) {
			ListIterator liStop = procsToStop.listIterator();
			while (liStop.hasNext()) {
				inx = ((Integer)liStop.next()).intValue();
				activeProcesses.remove(inx);
			}
		}
	}
	
	/**
	 * stopProcess
	 * See if any active processes are stopped by applied actions/events
	 */
	private void stopProcesses(int instance,PlanState workingState) {
		ListIterator li = activeProcesses.listIterator();
		ArrayList procsToStop = new ArrayList();
		List mssgs = new ArrayList();
		boolean stop = false;
		int inx = 0;
		while (li.hasNext()) {
			TaskObject procLine = (TaskObject)li.next();
			oclProcess proc = (oclProcess)procLine.getObject();
			ListIterator liSE = proc.getPrevail().listIterator();
			while (liSE.hasNext()) {
				oclSEPlus se = (oclSEPlus)liSE.next();
				if (!workingState.objectStateMatches(se,mssgs)) {
					stop = true;
				}
			}
			ListIterator liSC = proc.getNecessary().listIterator();
			while (liSC.hasNext()) {
				oclSCPlus sc = (oclSCPlus)liSC.next();
				if (!workingState.objectStateMatches(sc,mssgs)) {
					stop = true;
				}
			}
			if (stop) {
				editCanvas.stopProcess(procLine,instance);
				stoppedProcesses.add(procLine);
				procsToStop.add(new Integer(inx));
			} 
			inx++;
			stop = false;
		}
		if (procsToStop.size() > 0) {
			ListIterator liStop = procsToStop.listIterator();
			while (liStop.hasNext()) {
				inx = ((Integer)liStop.next()).intValue();
				activeProcesses.remove(inx);
			}
		}
	}
	
	/**
	 * showProcess
	 * test that process can start in current state
	 * if valid apply and display
	 * otherwise ignore
	 * @param an instantiated process
	 */
	public void showProcess(oclProcess proc,int instance){
		// store the list of objects affected
		List lstObjStates = new ArrayList();
		List lstObjs = new ArrayList();
		List mssgs = new ArrayList();
		// get the object affected by this process ignoring cond effects for the moment
		ListIterator liObjs = proc.getPrevail().listIterator();
		while (liObjs.hasNext()) {
			oclSEPlus curSE = (oclSEPlus)liObjs.next();
			DrawableObject obj = null;
			if (resultState.objectStateMatches(curSE,mssgs)) {
				obj = getKnownObject(curSE.getName());
				lstObjStates.add(resultState.getObjectState(curSE.getName()));
			} else {
				return;
			}
			if (obj != null)
				lstObjs.add(obj);
		}
		liObjs = proc.getNecessary().listIterator();
		while (liObjs.hasNext()) {
			oclSCPlus curSC = (oclSCPlus)liObjs.next();
			DrawableObject obj = null;
			if (resultState.objectStateMatches(curSC,mssgs)) {
				obj = getKnownObject(curSC.getName());
				lstObjStates.add(resultState.getObjectState(curSC.getName()));
			} else {
				return;
			}
			if (obj != null)
				lstObjs.add(obj);
		}
		// If we have got here then the process can be started
		// Is the process already running ?
		if (activeProcesses.size() > 0) {
			ListIterator liP = activeProcesses.listIterator();
			while (liP.hasNext()) {
				TaskObject pr = (TaskObject)liP.next();
				if (pr.getTitle().equals(proc.opName.toString())) {
					// Do nothing
					return;
				}
			}
		} else if (stoppedProcesses.size() > 0) {
			// Have we stopped this process
			ListIterator liP = stoppedProcesses.listIterator();
			int inx = 0;
			boolean restarted = false;
			while (!restarted && liP.hasNext()) {
				TaskObject procLine = (TaskObject)liP.next();
				if (procLine.getTitle().equals(proc.opName.toString())) {
					// Use that line
					procLine.setObjectStates(lstObjStates);
					procLine.setStartTime(instance);
					editCanvas.startProcess(procLine,instance);
					activeProcesses.add(procLine);
					restarted = true;
				} else {
					inx++;
				}
			}
			if (restarted) {
				stoppedProcesses.remove(inx);
				return;
			}
		}
		TaskObject procLine = new TaskObject(proc.opName.toString(),"none");
		procLine.setObject(proc,TaskObject.PROCESS);
		procLine.setObjectStates(lstObjStates);
		procLine.setStartTime(instance);
		editCanvas.addProcessLine(procLine);
		editCanvas.startProcess(procLine,instance);
		activeProcesses.add(procLine);
	}
	
	private DrawableObject getKnownObject(String name){
		ListIterator li = editCanvas.getObjects().listIterator();
		while(li.hasNext()) {
			DrawableObject obj = (DrawableObject)li.next();
			if (obj.getTitle().equals(name)) {
				return obj;
			}
		}
		return null;
	}
	 
	/* The following methods are used to calculate and maintain
	 * the state of the plan.
	 */
	 
	 /**
	  * createPossibleProcessList
	  * @param - the current task
	  */
	private void createPossibleProcessList(oclTask task) {
		possibleProcesses = new ArrayList();
		ListIterator li = curDomain.processes.listIterator();
		while (li.hasNext()) {
			oclProcess proc = (oclProcess)li.next();
			List insts = getAllProcInstantiations(proc,task);
			if (insts != null)
				possibleProcesses.addAll(insts);
		}
	}
	
	/**
	 * getAllProcInstantiations
	 * @param - the current raw process
	 * @param - the current task
	 * @return - the list of fully instantiated processes
	 */
	private List getAllProcInstantiations(oclProcess orgProc,oclTask task) {
		List posProcs = new ArrayList();
		oclProcess proc = null;
		boolean more = true;
		try {
			posProcs.add((oclProcess)orgProc.clone());
		} catch (CloneNotSupportedException e){}
		for (int argNo = 0; more && argNo < orgProc.opName.size();argNo++) {
			oclPredicate.pArg arg = (oclPredicate.pArg)(orgProc.opName.getArguments().get(argNo));
			int candidates = posProcs.size();
			for (int indiv = 0; more && indiv < candidates; indiv++) {	
				try {
					proc = (oclProcess)((oclProcess)posProcs.get(indiv)).clone();
				} catch (CloneNotSupportedException e){}		
				ListIterator liObjs = task.getInits().listIterator();
				boolean found = false;
				while (liObjs.hasNext()) {
					oclSS objState = (oclSS)liObjs.next();
					oclProcess newProc = null;
					if (arg.sort.equals(objState.getSort())) {
						try {
							newProc = (oclProcess)proc.clone();
						} catch (CloneNotSupportedException e){}
						newProc.replaceVariableName(arg.name,objState.getName());
						posProcs.add(newProc);
						found = true;
					}
				}
				if (!found) {// May be static object
					liObjs = curDomain.getObjectsOfSort(arg.sort).listIterator();
					while (liObjs.hasNext()) {
						oclProcess newProc = null;
						try {
							newProc = (oclProcess)proc.clone();
						} catch (CloneNotSupportedException e){}
						newProc.replaceVariableName(arg.name,(String)liObjs.next());
						posProcs.add(newProc);
						found = true;
					}
				}
				if (!found)
					more = false;
			}
			// Remove partially instantiated process from list
			for (int i = 0; i < candidates; i++)
				posProcs.remove(0);
		}
		return posProcs;
	}
	
	/**
	 * createPossibleEventsList
	 * @param - the current task
	 */
   private void createPossibleEventsList(oclTask task) {
	   possibleEvents = new ArrayList();
	   ListIterator li = curDomain.events.listIterator();
	   while (li.hasNext()) {
		   oclEvent evnt = (oclEvent)li.next();
		   List insts = getAllEventInstantiations(evnt,task);
		   if (insts != null)
			   possibleEvents.addAll(insts);
	   }
   }
	
   /**
	* getAllEventInstantiations
	* @param - the current raw event
	* @param - the current task
	* @return - the list of fully instantiated Events
	*/
   private List getAllEventInstantiations(oclEvent orgEvent,oclTask task) {
	   List posEvents = new ArrayList();
	   oclEvent evnt = null;
	   boolean more = true;
	   try {
		   posEvents.add((oclEvent)orgEvent.clone());
	   } catch (CloneNotSupportedException e){}
	   for (int argNo = 0; more && argNo < orgEvent.opName.size();argNo++) {
		   oclPredicate.pArg arg = (oclPredicate.pArg)(orgEvent.opName.getArguments().get(argNo));
		   int candidates = posEvents.size();
		   for (int indiv = 0; more && indiv < candidates; indiv++) {	
			   try {
				   evnt = (oclEvent)((oclEvent)posEvents.get(indiv)).clone();
			   } catch (CloneNotSupportedException e){}		
			   ListIterator liObjs = task.getInits().listIterator();
			   boolean found = false;
			   while (liObjs.hasNext()) {
				   oclSS objState = (oclSS)liObjs.next();
				   oclEvent newEvent = null;
				   if (arg.sort.equals(objState.getSort())) {
					   try {
						   newEvent = (oclEvent)evnt.clone();
					   } catch (CloneNotSupportedException e){}
					   newEvent.replaceVariableName(arg.name,objState.getName());
					   posEvents.add(newEvent);
					   found = true;
				   }
			   }
				if (!found) {// May be static object
					liObjs = curDomain.getObjectsOfSort(arg.sort).listIterator();
					while (liObjs.hasNext()) {
						oclEvent newEvent = null;
						try {
							newEvent = (oclEvent)evnt.clone();
						} catch (CloneNotSupportedException e){}
						newEvent.replaceVariableName(arg.name,(String)liObjs.next());
						posEvents.add(newEvent);
						found = true;
					}
				}
			   if (!found) // Give up
				   more = false;
		   }
		   // Remove partially instantiated process from list
		   for (int i = 0; i < candidates; i++)
			   posEvents.remove(0);
	   }
	   return posEvents;
   }
   
	/**
	 * advanceState 
	 * apply all actions and processes up to given time
	 * @param - the event causing state change
	 * @param - that fired the eventstate
	 * @param - time the event was fired
	 * @param List of conditional clauses matching state
	 */
	private void advanceState(oclEvent evnt, PlanState tmpState,int moment,List matches,TaskObject taskObj) {
		resultState = new PlanState(tmpState);
		ListIterator liObjs = evnt.getNecessary().listIterator();
		while (liObjs.hasNext()) {
			oclSCPlus curSC = (oclSCPlus)liObjs.next();
			resultState.makeTransition(curSC);
			taskObj.addResultToResultState(resultState.getObjectState(curSC.getName())); // Ron 24/05/05
		}
		advanceCondMatches(matches,taskObj);   // Ron  24/05/05
		resultState.refreshFluentList();
		startProcesses(moment);
		stopProcesses(moment);
	}
	
	/**
	 * checkEvents
	 * check to see if any events fire as a result of the change to
	 * fluent values updated by active processes
	 */
	private void checkEvents(int now) {
		PlanState tmpState = new PlanState(curState);
		tmpState.setTime(curState.getTime()); //Same time
		if (possibleEvents == null) {
			createPossibleEventsList(curTask);
		}
		// First advance the state for active processes
		if (activeProcesses.size() > 0) {
			for (int step = curState.getTime() + 1; step <= now; step++) {
				ListIterator liProcs = activeProcesses.listIterator();
				while (liProcs.hasNext()) {
					TaskObject procDrawable = (TaskObject)liProcs.next();
					updateState(procDrawable,tmpState);
				}
				tmpState.setTime(step);
				// Now check the Events
				ListIterator liEvents = possibleEvents.listIterator();
				while (liEvents.hasNext()) {
					oclEvent evnt = (oclEvent)liEvents.next();
					if (showInstantiatedEvent(evnt,tmpState,step)) {
						// event fired cur state will have been update
						tmpState = new PlanState(resultState);
						resultState = null;
						tmpState.setTime(step);
					}
				}
				stopProcesses(step,tmpState); //Incase process preconditions are no longer true
			}
			curState = tmpState;
		}
	}
	
	/**
	 * updateState
	 * update the state for one time increment
	 * update the results of applying given active processes
	 * updates by one time tick
	 * @param - the active process
	 * @param - the temporary state to update
	 */
	private void updateState(TaskObject drProc,PlanState tempState) {
		oclProcess proc = (oclProcess)drProc.getObject();
		ListIterator liSC = proc.getNecessary().listIterator();
		while (liSC.hasNext()) {
			oclSCPlus sc = (oclSCPlus)liSC.next();
			tempState.makeProcessTransition(sc);
			//tempState.refreshFluentList();
		}
		editCanvas.advanceProcess(drProc,tempState.getTime() + 1);
		Utility.debugPrint("STATE at " + (tempState.getTime() + 1)
		                   + "\n" + tempState.toString());
	}

	/**
	 * Invoked when the value of the Time Slider is changing.
	 * Looks for events that may be triggered as time advances
	 * @param e the source of the event.
	 */
	public void stateChanged(ChangeEvent e) {
		TimeSlider source = (TimeSlider)e.getSource();
		int time = source.getValue();
		if (!source.getValueIsAdjusting()) {
			if (resultState != null) {
				curState = resultState;
				resultState = null;
			}
			sliderTime = time;
			Utility.debugPrintln("patterns","Time = " + time);
			checkEvents(time);
		}
	}

}

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

package jplan.top;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.HashMap;
import java.util.NoSuchElementException;

import jplan.ocl.*;
import jplan.pddl.*;
import jplan.general.*;

import se.sics.jasper.*;

/**
 * PtoolsControl.java
 * This class encapsulates the top level calling of the various
 * sicstus prolog tools called using the jasper interface
 * @author Ron Simpson
 */

public class PToolsControl {
	private OclEd parent; // Top level window
	Prolog sp = null;
	String domPath;
	String toolPath;
	String oclPath; // where are we
	String strSICSPath; //where sicstus is i.e. sprt.sav
	String strOSName = null; //operating system name Ron 3/10/0
	private boolean plannerConfigured;
	private AlgRun sicRunner = null;
	private Process externalProc = null;

	public PToolsControl(OclEd top) {
		parent = top;
		oclPath = parent.strOCLPath;
		domPath =
			new String(
				parent.strOCLPath
					+ File.separator
					+ "domains"
					+ File.separator);
		toolPath =
			new String(
				parent.strCodeBase
					+ File.separator
					+ "jplan"
					+ File.separator
					+ "ptools"
					+ File.separator);
		plannerConfigured = false;
		strSICSPath = new String(System.getProperty("sicstus.path"));
		strOSName = new String(System.getProperty("os.name"));
		// Ron 3/10/04 NOTE all calls to newProlog now check the OS name property
		// TODO check how if sparc will deal with being gives the path to sprt.sav
		// Could also look at just selecting windos versions to give the path to
		// Need to know how XP responds to os.name property
	}

	/**
	 * runPlanner
	 * this routine attemps to run the currently configered planner
	 * against the current domain
	 * Considerable checking should take place befor we launch the
	 * planner. For the moment we will make do with minimal checks
	 */
	public void runPlanner() {
		String taskID;
		if (!parent.plannerConfig.dirty) { // Have we run this planner before
			// How about if the curDomain changes!! should check as well
			runAnother();
			return;
		}
		if (parent.curDomain == null) {
			JOptionPane.showMessageDialog(
				parent,
				"No current domain loaded.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (parent.getOclFile() == null) {
			JOptionPane.showMessageDialog(
				parent,
				"You must save the current domain before running the domain.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if ("none".equals(parent.plannerConfig.algName)) {
			JOptionPane.showMessageDialog(
				parent,
				"You must choose a Planning algorithm before"
					+ " you can run any planner.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (!parent.plannerConfig.condEffects
			&& parent.curDomain.hasCondEffects()) {
			JOptionPane.showMessageDialog(
				parent,
				"The planner "
					+ parent.plannerConfig.algName
					+ " does not support conditional effects.\n"
					+ "You cannot use it on this domain.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;

		}
		if (!parent.plannerConfig.hierSorts
			&& parent.curDomain.sorts.size() > 1) {
			JOptionPane.showMessageDialog(
				parent,
				"The planner "
					+ parent.plannerConfig.algName
					+ " does not support hierarchical sorts.\n"
					+ "You cannot use it on this domain.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;

		}
		if (!parent.plannerConfig.hierMethods
			&& parent.curDomain.hasHierMethods()) {
			JOptionPane.showMessageDialog(
				parent,
				"The planner "
					+ parent.plannerConfig.algName
					+ " does not support hierarchical methods.\n"
					+ "You cannot use it on this domain.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;

		}
		/* WZ 4/7/02 */
		// Ron 5/5/03
		if (parent.curDomain.isHierarchical()) {
			if (parent.curDomain.methods.size() == 0) {
				JOptionPane.showMessageDialog(
					parent,
					"You must create compound operators before"
						+ " you can run any HTN planner.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			if (parent.curDomain.htntasks.size() > 0) {
				String choice[] = new String[parent.curDomain.htntasks.size()];
				// 		Utility.debugPrintln("NO TASKS = " + parent.curDomain.htntasks.size());
				ListIterator li = parent.curDomain.htntasks.listIterator();
				int count = 0;
				while (li.hasNext()) {
					choice[count++] = ((oclHTNTask) li.next()).ID;
				}
				taskID =
					(String) JOptionPane.showInputDialog(
						parent,
						"Choose HTN Task Id",
						"GIPO Query",
						JOptionPane.QUESTION_MESSAGE,
						null,
						choice,
						choice[0]);
				if (taskID == null) {
					JOptionPane.showMessageDialog(
						parent,
						"You must choose a Task before you can run any planner.",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					return;
				}
			} else {
				JOptionPane.showMessageDialog(
					parent,
					"There are no HTN tasks defined for this domain,\n"
						+ "Please load or define.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
		} else {
			/* end 4/7/02 */
			if (parent.curDomain.tasks.size() > 0) {
				String choice[] = new String[parent.curDomain.tasks.size()];
				Utility.debugPrintln(
					"NO TASKS = " + parent.curDomain.tasks.size());
				ListIterator li = parent.curDomain.tasks.listIterator();
				int count = 0;
				while (li.hasNext()) {
					choice[count++] = ((oclTask) li.next()).ID;
				}
				taskID =
					(String) JOptionPane.showInputDialog(
						parent,
						"Choose Task Id",
						"GIPO Query",
						JOptionPane.QUESTION_MESSAGE,
						null,
						choice,
						choice[0]);
				if (taskID == null) {
					JOptionPane.showMessageDialog(
						parent,
						"You must choose a Task before you can run any planner.",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					return;
				}
			} else {
				JOptionPane.showMessageDialog(
					parent,
					"There are no tasks defined for this domain\n"
						+ "Please load or define.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
		}
		if (parent.plannerConfig.callMethod == RuntimeConfig.SHELL) {
			// Now set up the kill proces dialog box
			String haltMssg =
				parent.plannerConfig.algName + " running on task " + taskID;
			HaltControl stopDia = new HaltControl(parent, haltMssg, this);
			parent.desktop.add(stopDia);
			parent.deskManager.activateFrame(stopDia);
			ExternalRun ex =
				new ExternalRun(parent, this, oclPath, toolPath, taskID);
			ex.setKillDia(stopDia);
			Thread exThread = new Thread(ex);
			exThread.start();
		} else {
			try {
				// Can we save the current domain to tmp directory
				// First make sure all operators have full signatures
				/* WZ 4/7/02 no need to do this anymore */
				PrintWriter pwTmpDom =
					new PrintWriter(
						new FileWriter(
							new File(
								oclPath
									+ File.separator
									+ "tmp"
									+ File.separator
									+ "tmpDom.pl")));
				pwTmpDom.println(":- multifile operator/4.");
				pwTmpDom.println(":- dynamic operator/4.");
				pwTmpDom.println(":- multifile substate_classes/3.");
				pwTmpDom.println(":- dynamic substate_classes/3.");
				parent.curDomain.oclPrintComponent(new PrintWriter(pwTmpDom), 0, false);
				pwTmpDom.close();
			} catch (Exception e) {
				JOptionPane.showMessageDialog(
					parent,
					"Error in saving tempory file.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			if (sicRunner != null && sicRunner.isRunning) {
				JOptionPane.showMessageDialog(
					parent,
					"You must wait for the current planner to complete running.\n"
					+ "With a runaway task you may have to save the domain and halt GIPO!!",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			sicRunner = new AlgRun(parent, oclPath, toolPath, taskID);
			sicRunner.setTask(taskID);
			if (sp == null) {
				// OK Lets try and run the prolog
				try {
					Utility.debugPrintln("sicstus path " + strSICSPath);
					if (strOSName.equals("Linux") || strOSName.equals("SunOS"))
						sp = Jasper.newProlog(); 
					else {
						sp = Jasper.newProlog(strSICSPath);
					}
				} catch (Exception e) {
				Utility.debugPrintln("failed to initialize sicstus " + e.toString());
				}
			}
			String haltMssg =
				parent.plannerConfig.algName + " running on task " + taskID;

			HaltControl stopDia = new HaltControl(parent, haltMssg, "killsic");
			parent.desktop.add(stopDia);
			parent.deskManager.activateFrame(stopDia);
			sicRunner.setKillDia(stopDia);
			sicRunner.requireLoad();
			sicRunner.setSicstus(sp);
			sicRunner.setKillFile("killsic");
			Thread sicThread = new Thread(sicRunner);
			sicThread.start();

		}
	}

	/**
	 * runAnother
	 * this routine attempts to run the currently configered planner
	 * against the current domain
	 * But this time the planner has already been loaded into sicstus
	 * we just call solve again with the new task
	 */
	public void runAnother() {
		String taskID;

		if (parent.plannerConfig.dirty) {
			JOptionPane.showMessageDialog(
				parent,
				"Planner Has not run in this configuration.\n"
					+ "Use menu item Run Planner",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		/* WZ 4/7/02 */
		// Ron 5/5/03
		if (parent.curDomain.isHierarchical()) {
			if (parent.curDomain.methods.size() == 0) {
				JOptionPane.showMessageDialog(
					parent,
					"You must create compound operators before"
						+ " you can run any HTN planner.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			if (parent.curDomain.htntasks.size() > 0) {
				String choice[] = new String[parent.curDomain.htntasks.size()];
				// 		Utility.debugPrintln("NO TASKS = " + parent.curDomain.htntasks.size());
				ListIterator li = parent.curDomain.htntasks.listIterator();
				int count = 0;
				while (li.hasNext()) {
					choice[count++] = ((oclHTNTask) li.next()).ID;
				}
				taskID =
					(String) JOptionPane.showInputDialog(
						parent,
						"Choose HTN Task Id",
						"GIPO Query",
						JOptionPane.QUESTION_MESSAGE,
						null,
						choice,
						choice[0]);
				if (taskID == null) {
					JOptionPane.showMessageDialog(
						parent,
						"You must choose a Task before you can run any planner.",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					return;
				}
			} else {
				JOptionPane.showMessageDialog(
					parent,
					"There are no HTN tasks defined for this domain,\n"
						+ "Please load or define.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
		} else {
			/* end 4/7/02 */
			String choice[] = new String[parent.curDomain.tasks.size()];
			Utility.debugPrintln("NO TASKS = " + parent.curDomain.tasks.size());
			ListIterator li = parent.curDomain.tasks.listIterator();
			int count = 0;
			while (li.hasNext()) {
				choice[count++] = ((oclTask) li.next()).ID;
			}
			taskID =
				(String) JOptionPane.showInputDialog(
					parent,
					"Choose Task Id",
					"GIPO Query",
					JOptionPane.QUESTION_MESSAGE,
					null,
					choice,
					choice[0]);
			if (taskID == null) {
				JOptionPane.showMessageDialog(
					parent,
					"You must choose a Task before you can run any planner.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
		}
		if (parent.plannerConfig.callMethod == RuntimeConfig.SHELL) {
			// Now set up the kill proces dialog box
			String haltMssg =
				parent.plannerConfig.algName + " running on task " + taskID;
			HaltControl stopDia = new HaltControl(parent, haltMssg, this);
			parent.desktop.add(stopDia);
			parent.deskManager.activateFrame(stopDia);
			ExternalRun ex =
				new ExternalRun(parent, this, oclPath, toolPath, taskID);
			ex.setKillDia(stopDia);
			Thread exThread = new Thread(ex);
			exThread.start();
		} else {
			// If we have got here we can now run a task
			if (sicRunner.isRunning) {
				JOptionPane.showMessageDialog(
					parent,
					"You must wait for the current planner to complete running.\n"
					+ "With a runaway task you may have to save the domain and halt GIPO!!",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			String haltMssg =
				parent.plannerConfig.algName + " running on task " + taskID;
			HaltControl stopDia = new HaltControl(parent, haltMssg, "killsic");
			parent.desktop.add(stopDia);
			parent.deskManager.activateFrame(stopDia);
			sicRunner.setKillDia(stopDia);
			sicRunner.setTask(taskID);
			Thread sicThread = new Thread(sicRunner);
			sicThread.start();
		}
	}

	/**
	 * groundOpsTool 
	 * Calls Prolog tool to ground ocl Operators
	 * results are placed in the file oclFile.grnd
	 */
	public void groundOpsTool() {
		String query = null;
		
		if (parent.curDomain == null) {
			JOptionPane.showMessageDialog(
				parent,
				"No current domain loaded.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (parent.curDomain.sorts.size() > 1) {
			JOptionPane.showMessageDialog(
				parent,
				"Tool does not support hierarchial sorts.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (parent.getOclFile() == null) {
			JOptionPane.showMessageDialog(
				parent,
				"You must save the current domain before grounding the operators.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}

		try {
			if (sp == null) {
				if (strOSName.equals("Linux"))
					sp = Jasper.newProlog(); 
				else
					sp = Jasper.newProlog(strSICSPath);
			}
		} catch (Exception e) {
			Utility.debugPrintln("Failed to initialize sicstus " + e.toString());
		}
		try {
			String codeFile = new String(toolPath + "groundCE.sav");
			query = query.replace('\\','/');
			if (! sp.query("restore(" + codeFile + ").",new HashMap())) {
				JOptionPane.showMessageDialog(
								parent,
								"Failed to load groundCE.sav. Check that it exists in ptools",
								"GIPO Error",
								JOptionPane.ERROR_MESSAGE,
								null);
			}
			String from = parent.getOclFile().toString();
			from = from.replace('\\','/');
			String to =  parent.getOclFile().getParent()
				+ File.separator
				+ "grnd"
				+ parent.getOclFile().getName() ;
			to = to.replace('\\','/');
			query = "convert('" + from +"','" + to + "').";
			if (sp.query(query,new HashMap())) {
				JOptionPane.showMessageDialog(
					parent,
					"Convertion suceeded. Results in "
						+ parent.getOclFile().getParent()
						+ File.separator
						+ "grnd"
						+ parent.getOclFile().getName(),
					"GIPO Information",
					JOptionPane.INFORMATION_MESSAGE,
					null);
			} else {
				JOptionPane.showMessageDialog(
					parent,
					"Convertion failed.\nCheck that domain file ["
						+ parent.getOclFile().toString()
						+ "] contains a legal GIPO domain",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(
				parent,
				"Sicstus Has Thrown an error.\nConsult your local expert.\n",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			JOptionPane.showMessageDialog(
				parent,
				"Sicstus has thrown an exception " + e.toString(),
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
	}

	/**
	 * getproc
	 * get reference to external running process
	 * @return Process proc
	 */
	protected Process getProc() {
		return externalProc;
	}

	/**
	 * setproc
	 * save reference to external process
	 * @param p - Process proc
	 */
	protected void setProc(Process p) {
		externalProc = p;
	}

	/**
	 * ocl2pddl Call the Prolog routine to convert from 
	 * oclFile -> pddlFile
	 */
	public void ocl2pddl() {
		try {
			if (parent.curDomain == null) {
				JOptionPane.showMessageDialog(
					parent,
					"No OCL(h) Domain Currently Loaded",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			} else {
				String pddlStrName = toPDDLName(parent.getOclFile().getName());
				File pddlOut =
					new File(parent.getOclFile().getParent(), pddlStrName);
				if (pddlOut.exists()) {
					if (pddlOut.canWrite()) {
						int res =
							JOptionPane.showConfirmDialog(
								parent,
								"Overwrite Existing File",
								"GIPO Warning",
								JOptionPane.YES_NO_OPTION);
						if (res == JOptionPane.YES_OPTION) {
							ocl2pddlConvert(parent.getOclFile(), pddlOut);
							JOptionPane.showMessageDialog(
								parent,
								"Overwrite Existing - Conversion done\n Results in "
									+ toPDDLName(parent.getOclFile().toString()),
								"GIPO Information",
								JOptionPane.INFORMATION_MESSAGE,
								null);
						}
					} else {
						JOptionPane.showMessageDialog(
							parent,
							"Can not write file",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE,
							null);
					}
				} else {
					if (pddlOut.createNewFile()) {
						ocl2pddlConvert(parent.getOclFile(), pddlOut);
						JOptionPane.showMessageDialog(
							parent,
							"Conversion Done",
							"GIPO Information",
							JOptionPane.INFORMATION_MESSAGE,
							null);
					} else {
						JOptionPane.showMessageDialog(
							parent,
							"Can not write to directory",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE,
							null);
					}
				}
			}
		} catch (IOException e) {
			JOptionPane.showMessageDialog(
				parent,
				"IO Error - Cannot complete operation",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
	}

	static String toPDDLName(String oclName) {
		String pddlOut;
		int dotPos = oclName.lastIndexOf('.');
		if (dotPos == -1) { // No file extension
			pddlOut = new String(oclName + ".pddl");
		} else {
			pddlOut = new String(oclName.substring(0, dotPos) + ".pddl");
		}
		return pddlOut;
	}

	/**
	 * ocl2pddlConvert
	 * translate to standard PDDL
	 * @param inFile - File the ocl domain
	 * @param outFile - the resulting PDDL file
	 */
	public void ocl2pddlConvert(File inFile, File outFile) {
		Term spInFile, spOutFile, spPlFile;
		Term loadList;
		String query = null;
		Utility.debugPrintln("Try to convert6 to PDDL");
		try {
			if (sp == null) {
				Utility.debugPrintln("Initialise Sicstus");
				if (strOSName.equals("Linux"))
					sp = Jasper.newProlog(); 
				else
					sp = Jasper.newProlog(strSICSPath);
				if (sp == null)
				Utility.debugPrintln("Initialise Sicstus Failed sp is null");
			}
		} catch (Exception e) {
			Utility.debugPrintln("Prolog initialisation failed " + e.toString());	
			return;
		}
		try {
			// Now start up prolog and load the program code and the
			// OCL domain
			Utility.debugPrintln("Try to restore saved state");
			query = "restore('" + toolPath + "ocl2pddl.sav').";
			query = query.replace('\\','/');
			Utility.debugPrintln("Restore Query " + query);
			sp.query(query,new HashMap());
			Utility.debugPrintln("Try to consult source");
			query = "consult('" + inFile.toString() + "').";
			query = query.replace('\\','/');
			if (! sp.query(query,new HashMap())){
				Utility.debugPrintln("Prolog failed query " + query);
				return;
			}

			// Set-up the output stream
			query = "tell('" + outFile.toString() +"').";
			query = query.replace('\\','/');
			if (!sp.queryCutFail(query,new HashMap())){
				Utility.debugPrintln("Prolog failed query " + query);
				return;
			}
			// Now convert main domain elements
			query = "convert.";
			if (! sp.queryCutFail(query, new HashMap())){
				Utility.debugPrintln("Prolog failed query " + query);
				return;
			}
			if (parent.curDomain.tasks.size() > 0) {
				// We have some tasks so convert them as well
				query = "convert_p.";
				if (! sp.queryCutFail(query,new HashMap())) {
					Utility.debugPrintln("Prolog failed query " + query);
					return;
				}
			}
			if (! sp.queryCutFail("told.", new HashMap())){
				Utility.debugPrintln("Prolog failed query :: told");
				return;
			}
		} catch (Exception e) {
			Utility.debugPrintln(
				"Sicstus has thrown an exception While converting to PDDL " + e.toString());
			Utility.debugPrintln("\nLast Query " + query);	
		}
	}

	/**
	 * ocl2pddlConvert
	 * translate to standard PDDL split domain from task
	 * @param inFile the ocl domain
	 * @param outDom the resulting PDDL file for the domain definition
	 * @param outTask the PDDL version of the chosen task
	 * @param taskID the task to translate
	 */
	protected void ocl2pddlConvert(
		File inFile,
		File outDom,
		File outTask,
		String taskID) {
			
		String query = null;

		try {
			if (sp == null) {
				if (strOSName.equals("Linux"))
					sp = Jasper.newProlog(); 
				else
					sp = Jasper.newProlog(strSICSPath);
			}

			// Now start up prolog and load the program code and the
			// OCL domain
			query = "restore('" + toolPath + "ocl2pddl.sav').";
			query = query.replace('\\','/');
			sp.query(query,new HashMap());
			query = "consult('" + inFile.toString() + "').";
			query = query.replace('\\','/');
			sp.query(query,new HashMap());

			// Set-up the output stream
			query = "tell('" + outDom.toString() + "').";
			query = query.replace('\\','/');
			sp.queryCutFail(query, new HashMap());
			// Now convet main domain elements
			sp.queryCutFail("convert.", new HashMap());
			sp.queryCutFail("told.", new HashMap());
			query = "tell('" + outTask.toString() + "').";
			sp.queryCutFail(query,new HashMap());
			// We have some tasks so convert them as well
			query = "convert_p_id(" + taskID + ").";
			sp.queryCutFail(query,new HashMap());
			sp.queryCutFail("told.", new HashMap());
		} catch (Exception e) {
			Utility.debugPrintln(
				"Sicstus has thrown an exception \n" + 
				"Last query " + query + "\n" + e.toString());
		}

	}

	/**
	 * genRandomTasks
	 * run the prolog random task generato tool
	 */
	public void genRandomTasks() {
		int noTasks = 0;
		int addToTasks = 0;

		if (parent.curDomain == null) {
			JOptionPane.showMessageDialog(
				parent,
				"No current domain loaded.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		try {
			if (!parent.curDomain.allPrimSortsHaveObjects()) {
				JOptionPane.showMessageDialog(
					parent,
					"All primitive sorts must have object instances.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
		} catch (NoSuchElementException e) {
			JOptionPane.showMessageDialog(
				parent,
				"No primitive_sorts have been defined.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (parent.curDomain.classDefs.size() == 0) {
			JOptionPane.showMessageDialog(
				parent,
				"State definitions have not been made.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (parent.curDomain.operators.size() == 0) {
			JOptionPane.showMessageDialog(
				parent,
				"Operator definitions have not been made.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (parent.getOclFile() == null) {
			JOptionPane.showMessageDialog(
				parent,
				"You must save the current domain before generating tasks.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (parent.curDomain.tasks.size() > 0) {
			addToTasks =
				JOptionPane.showConfirmDialog(
					parent,
					"Add to existing tasks",
					"GIPO Query",
					JOptionPane.YES_NO_CANCEL_OPTION);
			if (addToTasks == JOptionPane.CANCEL_OPTION) {
				return;
			}
		}

		/* Weihong changed/added on 6/9/2001 */
		while (true) {
			/* Weihong changed/added on 6/9/2001 */
			String strNoTasks =
				GipoInputBox.showIntegerInputBox(
					parent,
					"Number of tasks to generate",
					"Please enter an integer between 0 and 50.");
			if (strNoTasks != null) {
				noTasks = Integer.parseInt(strNoTasks);
				if (noTasks < 1 || noTasks >= 50) {
					JOptionPane.showMessageDialog(
						parent,
						"Please enter an integer greater than 0 and less than 50",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
				} else {
					break;
				}
			} else {
				return;
			}
		}

		// Now we can try to run the tool
		// First dump the current domain definition to temp file
		try {
			PrintWriter pwTmpDom =
				new PrintWriter(
					new FileWriter(
						new File(
							oclPath
								+ File.separator
								+ "tmp"
								+ File.separator
								+ "tmpDom.pl")));
			parent.curDomain.oclPrintComponent(new PrintWriter(pwTmpDom), 0, false);
			pwTmpDom.close();
		} catch (Exception e) {
			JOptionPane.showMessageDialog(
				parent,
				"Error in saving tempory file.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}

		try {
			if (sp == null) {
				if (strOSName.equals("Linux"))
					sp = Jasper.newProlog(); 
				else
					sp = Jasper.newProlog(strSICSPath);
			}
		} catch (Exception e) {
			Utility.debugPrintln("Failed to initialise Sicstus " + e.toString());
			return;
		}
		String query = null;
		try {
			// Now start up prolog and load the program code and the
			String gen = toolPath + "gentasks.sav";
			gen = gen.replace('\\','/');
			query = "restore('" + gen + "').";
			sp.query(query, new HashMap());
			String dom = oclPath
				+ File.separator
				+ "tmp"
				+ File.separator
				+ "tmpDom.pl";
			dom = dom.replace('\\','/');
			query = "consult('" + dom + "').";
			sp.query(query, new HashMap());
			//assert dynamic predicates
			String outFile =
					oclPath
						+ File.separator
						+ "tmp"
						+ File.separator
						+ "randTasks.pl";
			outFile = outFile.replace('\\','/');
			// Set-up the output stream
			query = "asserta(solution_file('" + outFile + "')).";
			if (!sp.query(query, new HashMap())) {
				JOptionPane.showMessageDialog(
					parent,
					"Failed to assert tempory file",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				sp = null;
				return;
			}
			int high = 0;
			if (parent.curDomain.tasks.size() > 0) {
				if (addToTasks == JOptionPane.YES_OPTION) {
					high = parent.curDomain.getHighestTaskNo();
				} else {
					parent.curDomain.tasks.clear();
					high = 0;
				}
			}
			query = "asserta(task_counter( " + Integer.toString(high + 1) + ")).";
			if (!sp.query(query, new HashMap())) {
				JOptionPane.showMessageDialog(
					parent,
					"Failed to assert taskCounter",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				sp = null;
				return;
			}
			query = "gen_full(" + Integer.toString(noTasks) + ").";
			sp.queryCutFail(query, new HashMap());
			Utility.debugPrintln("Done creation");
			// force new selection of algorithm
			parent.plannerConfig.dirty = true;

		} catch (Exception e) {
			Utility.debugPrintln(
				"Sicstus has thrown an exception " + e.toString());
			Utility.debugPrintln("\nLast query " + query);
		}
		// Now try and load them
		if (addToTasks == JOptionPane.YES_OPTION) {
			parent.osDiags.loadRandomTasks(true);
			/* Weihong added on 5/10/2001 */
			parent.updateWindow("RandGenerateTask");
			//notify for the update of the task view
		} else {
			parent.osDiags.loadRandomTasks(false);
		}
	}

}

class ExternalRun implements Runnable {
	private OclEd parent; // Top level window
	private PToolsControl cont = null;
	String oclPath;
	String toolPath;
	String TaskId;
	JInternalFrame killDia = null;
	Process externalProc = null;

	public ExternalRun(
		OclEd par,
		PToolsControl cont,
		String oclPath,
		String toolPath,
		String TaskID) {
		parent = par;
		this.cont = cont;
		this.oclPath = oclPath;
		this.toolPath = toolPath;
		this.TaskId = TaskID;
	}

	public void setTask(String TaskID) {
		this.TaskId = TaskID;
	}

	public void setKillDia(JInternalFrame kf) {
		killDia = kf;
	}

	public void run() {
		runPDDLPlanner(TaskId);
	}

	/**
	 * runPDDLPlanner
	 * run in a shell external planner on pddl domain
	 * assume FF style interface to planner
	 * @param taskID - the problem to run against
	 */
	private void runPDDLPlanner(String taskID) {
		// First dump the current domain as pddl
		// One file for the domain definition and another for the
		// chosen task
		// Note preparatory checks on the domain already made
//		File domOut = null;
//		try {
//			domOut =
//				new File(
//					oclPath
//						+ File.separator
//						+ "tmp"
//						+ File.separator
//						+ "tmpDom.pl");
//			PrintWriter pwTmpDom = new PrintWriter(new FileWriter(domOut));
//			parent.curDomain.oclPrintComponent(new PrintWriter(pwTmpDom), 0, true);
//			pwTmpDom.close();
//		} catch (IOException e) {
//			JOptionPane.showMessageDialog(
//				parent,
//				"IO Error - save temporary files",
//				"GIPO Error",
//				JOptionPane.ERROR_MESSAGE,
//				null);
//			killDia.dispose();
//			return;
//		}
		File pddlOut = null;
		File pddlTaskOut = null;
		try {
			pddlOut =
				new File(
					oclPath
						+ File.separator
						+ "tmp"
						+ File.separator
						+ "tmpDom.pddl");
			pddlTaskOut =
				new File(
					oclPath
						+ File.separator
						+ "tmp"
						+ File.separator
						+ "tmpTask.pddl");
			if (pddlOut.exists()) {
				if (pddlOut.canWrite()) {
					//cont.ocl2pddlConvert(domOut, pddlOut, pddlTaskOut, taskID);
					FileWriter pddlDom = new FileWriter(pddlOut);
					parent.curDomain.pddlPrintDomainBody(new PrintWriter(pddlDom));
					pddlDom.close();
					FileWriter pddlTask = new FileWriter(pddlTaskOut);
					parent.curDomain.pddlPrintDomainTasks(new PrintWriter(pddlTask),taskID);
					pddlTask.close();
				} else {
					JOptionPane.showMessageDialog(
						parent,
						"Can not write temporary file",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					killDia.dispose();
					return;
				}
			} else {
				if (pddlOut.createNewFile()) {
					FileWriter pddlDom = new FileWriter(pddlOut);
					parent.curDomain.pddlPrintDomainBody(new PrintWriter(pddlDom));
					pddlDom.close();
					FileWriter pddlTask = new FileWriter(pddlTaskOut);
					parent.curDomain.pddlPrintDomainTasks(new PrintWriter(pddlTask),taskID);
					pddlTask.close();
				} else {
					JOptionPane.showMessageDialog(
						parent,
						"Can not write to directory",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					killDia.dispose();
					return;
				}
			}

			Utility.debugPrintln("Dumped PDDL Domain in " + pddlOut.getPath());
			Utility.debugPrintln(
				"Dumped PDDL Task in " + pddlTaskOut.getPath());
		} catch (IOException e) {
			JOptionPane.showMessageDialog(
				parent,
				"IO Error - Cannot complete operation",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			killDia.dispose();
			return;
		}
		// Done the convertion now try and run the planner
		// First build the string representing the command to run
		String cline = new String(parent.plannerConfig.command);
		int domPos = cline.indexOf("%d", 0);
		int taskPos = cline.indexOf("%t", 0);
		if (domPos == -1 || taskPos == -1) {
			JOptionPane.showMessageDialog(
				parent,
				"command specification error in ocled.properties",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			killDia.dispose();
			return;
		}
		String call = "";
		if (domPos < taskPos) {
			call = cline.substring(0, domPos);
			call = call.concat(pddlOut.getPath());
			call = call.concat(cline.substring(domPos + 2, taskPos));
			call = call.concat(pddlTaskOut.getPath());
			if (cline.length() > taskPos + 2)
				call = call.concat(cline.substring(taskPos + 2));
		} else {
			call = cline.substring(0, taskPos);
			call = call.concat(pddlTaskOut.getPath());
			call = call.concat(cline.substring(taskPos + 2, domPos));
			call = call.concat(pddlOut.getPath());
			if (cline.length() > domPos + 2)
				call = call.concat(cline.substring(domPos + 2));
		}

		// Now try and run the planner
		String strRes = new String("");
		BufferedReader res = null;
		Process p = null;

		try {
			// Now run the command
			p = Runtime.getRuntime().exec(call);
			cont.setProc(p);
			res = new BufferedReader(new InputStreamReader(p.getInputStream()));
		} catch (java.io.IOException e) {
			JOptionPane.showMessageDialog(
				parent,
				"Can't run external planner.\n" + "Check command \n" + call,
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			killDia.dispose();
			return;
		}

		try {
			p.waitFor();
			killDia.dispose();
		} catch (Exception e) {
			JOptionPane.showMessageDialog(
				parent,
				"Planner terminated.",
				"GIPO Information",
				JOptionPane.INFORMATION_MESSAGE,
				null);
			if (killDia != null)
				killDia.dispose();
			return;
		}
		try {
			String line = null;
			while ((line = res.readLine()) != null) {
				strRes = strRes.concat(line + "\n");
			}
		} catch (Exception e) {
			Utility.debugPrintln("Exception while processing planner output");
		}

		if (strRes.length() == 0) {
			JOptionPane.showMessageDialog(
				parent,
				"Planner terminated without producing any results.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		RunView rView =
			new RunView(parent, strRes, parent.plannerConfig.algName);
		parent.desktop.add(rView, BorderLayout.NORTH);
		parent.deskManager.activateFrame(rView);
		parent.plannerConfig.dirty = false;
		// Now save the results to file to be used by the animator
		File results =
			new File(
				oclPath + File.separator + "tmp" + File.separator + "run.txt");
		try {
			if (results.exists())
				results.delete();
		} catch (Exception e) {
			JOptionPane.showMessageDialog(
				parent,
				"Can not delete old results file.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		PrintWriter resOut;
		try {
			resOut = new PrintWriter(new FileWriter(results));
			resOut.print(strRes);
			resOut.close();
		} catch (IOException e) {
			JOptionPane.showMessageDialog(
				parent,
				"Problem saving results to file",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE);
			return;
		}
	}
}

class AlgRun implements Runnable {
	Prolog sp = null;
	private OclEd parent; // Top level window
	String oclPath;
	String toolPath;
	String TaskId;
	boolean doLoad;
	String killFileName = new String("none");
	JInternalFrame killDia = null;
	public boolean isRunning = false;

	public AlgRun(OclEd par, String oclPath, String toolPath, String TaskID) {
		parent = par;
		this.oclPath = oclPath;
		this.toolPath = toolPath;
		this.TaskId = TaskID;
		doLoad = true;
		killDia = null;
		isRunning = false;
	}

	public void setTask(String TaskID) {
		this.TaskId = TaskID;
	}

	public void setSicstus(Prolog sp) {
		this.sp = sp;
	}

	public void requireLoad() {
		doLoad = true;
	}

	public void setKillFile(String kfName) {
		killFileName = kfName;
	}

	public void setKillDia(JInternalFrame kf) {
		killDia = kf;
	}

	public void run() {
		String query = "none";
		isRunning = true;
		try {
			if (doLoad) {
				// Need to assert all these
				// Allow prolog to find the dynamic files
				String domFile = 
					new String(
						oclPath
							+ File.separator
							+ "tmp"
							+ File.separator
							+ "tmpDom.pl");
				domFile = domFile.replace('\\','/');
				// Now load up the prolog algorithm file

				String algFile =
					new String(
						toolPath + parent.plannerConfig.algName + ".sav");
				algFile = algFile.replace('\\','/');
				query = "restore('" + algFile + "').";
				Utility.debugPrintln("Run Algorithm " + query);
				sp.query(query, new HashMap());
				query = "consult('" + domFile + "').";
				Utility.debugPrintln(query);
				sp.query(query, new HashMap());
				String solFile = new String (
						oclPath	
							+ File.separator
							+ "tmp"
							+ File.separator
							+ "run.txt");
				solFile = solFile.replace('\\','/');
				query = new String (
						"asserta(solution_file('" + solFile + "')).");
				Utility.debugPrintln(query);
				if (!sp.query(query,new HashMap())) {
					JOptionPane.showMessageDialog(
						parent,
						"Failed to assert solution file",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					isRunning = false;
					return;
				}
				String killFileTerm =
					new String(
						oclPath
							+ File.separator
							+ "tmp"
							+ File.separator
							+ killFileName);
				killFileTerm = killFileTerm.replace('\\','/');
				query = "asserta(kill_file('" + killFileTerm +"')).";
				Utility.debugPrintln(query);
				if (!sp.query(query,new HashMap())) {
					JOptionPane.showMessageDialog(
						parent,
						"Failed to assert kill file",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					isRunning = false;
					return;
				}
			}
			isRunning = true;
			// If we have got here we can now run a task
			query = "solve(" + TaskId +").";
			Utility.debugPrintln(query);
			sp.queryCutFail(query,new HashMap());
			boolean gotResult = true;
			try {
				File killFile =
					new File(
						oclPath
							+ File.separator
							+ "tmp"
							+ File.separator
							+ killFileName);
				if (killFile.exists()) {
					JOptionPane.showMessageDialog(
						parent,
						"Stopped Planning Process.",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					gotResult = false;
					killFile.delete();
				}
			} catch (Exception e) {
				// Do nothing
			}
			if (killDia != null) {
				try {
					killDia.dispose();
				} catch (Exception e) {
					// Just ignore this
				}
			}

			// Now show the results
			if (gotResult) {
				RunView res =
					new RunView(
						parent,
						new String(
							oclPath
								+ File.separator
								+ "tmp"
								+ File.separator
								+ "run.txt"));
				parent.desktop.add(res, BorderLayout.NORTH);
				parent.deskManager.activateFrame(res);

				// If all this has worked we can enable runAnother
				parent.plannerConfig.dirty = false;
				doLoad = false;
			}

		} catch (Exception e) {
			JOptionPane.showMessageDialog(
				parent,
				"Sicstus Has Thrown an error."
					+ "\nConsult your local expert.\n"
					+ query + "\n"
					+ e.toString(),
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
		isRunning = false;
	}
}

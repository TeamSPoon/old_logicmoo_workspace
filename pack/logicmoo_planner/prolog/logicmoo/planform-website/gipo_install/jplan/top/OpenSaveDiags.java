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
 * OpenSaveDiags Encapsulates the Open and Save routines for
 * both OCL and PDDL domains
 */

package jplan.top;

import javax.swing.*;
import java.io.*;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.List;
import java_cup.runtime.Symbol;
import javax.swing.*;
import java.awt.event.*;
import java.text.SimpleDateFormat;
import java.util.Date;

import jplan.ocl.*;
import jplan.pddl.*;
import jplan.general.*;

// Chris adds 14/02/2001
//import java.awt.*;
//import jplan.dwiz.*;	// essential to create a new OCL Domain with ontology


/* Revision History
 * Ron 8/11/02 - allow cancel of new domains without forcing
 *               user to provide a domain name
 *       Added new flag for new domains and call new constructor
 * Ron 13/7/05 changes save to PDDL to use Java translator (Rather than Prolog
 */

/**
 * @author ron
 * @ version 2
 */
public class OpenSaveDiags {
	private OclEd top; // The top level controll OclEd/class    

	private JFileChooser fileChoose;
	private ExtensionFileFilter oclFilter =
		new ExtensionFileFilter("Planning Domains (*.ocl)", new String("ocl"));
	private ExtensionFileFilter pddlFilter =
		new ExtensionFileFilter("PDDL Domains (*.pddl)", new String("pddl"));
	private static final SimpleDateFormat formatter =
		new SimpleDateFormat("yyyy/MM/dd 'at' hh:mm:ss a zzz");
	private File oclFile = null;
	private File pddlFile = null;
	//private JTextArea tMssgs;
	Symbol parsRes = null;
	static final int SAVED = 0; // Return value for Save Dialog
	static final int CANCELLED = -1;
	static final int DISMISSED = -2;
	static final int OCL = 0;
	static final int PDDL = 1;
	int butSel = 0;

	public JDesktopPane desktop;
	public DesktopManager deskManager;

	/**
	 * Constructor 
	 * @args parent - requires a reference to the top level OclEd
	 */
	public OpenSaveDiags(OclEd parent) {
		top = parent;
		//tMssgs = top.getMessagePane();
		fileChoose = new JFileChooser(top.strDomainsDir);
		fileChoose.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent actionEvent) {
				JFileChooser fc = (JFileChooser) actionEvent.getSource();
				String command = actionEvent.getActionCommand();
				if (command.equals(JFileChooser.APPROVE_SELECTION)) {
					butSel = SAVED;
				} else if (command.equals(JFileChooser.CANCEL_SELECTION)) {
					butSel = CANCELLED;
				} else {
					butSel = DISMISSED;
				}
			}
		});
	}

	/** 
	 * Present a dialog box to have the user select
	 * an existing OCL Domain specification 
	 */
	public void loadOCLFile() {

		fileChoose.setDialogTitle("Open");
		fileChoose.setFileFilter(oclFilter);
		int result = fileChoose.showOpenDialog(top);
		File file = fileChoose.getSelectedFile();
		oclFile = file;
		List mssgs = new ArrayList();
		if (file != null
			&& result == JFileChooser.APPROVE_OPTION
			&& butSel == SAVED)
			try {
				oclDomain tempDomain;
				FileInputStream oclFIS = new FileInputStream(file);
				parsRes = new parser(new Yylex(oclFIS)).parse();
				tempDomain = (oclDomain) (parsRes.value);
				oclFIS.close();
				oclFIS = null;
				// Ron added routine to deal with static predicates
				tempDomain.markStaticPredicates();
				mssgs = parser.getWarnMssgs();
				String strMssgs = new String("");
				ListIterator li;
				li = mssgs.listIterator();
				if (li.hasNext()) {
					while (li.hasNext()) {
						strMssgs = strMssgs.concat((String) li.next());
						if (li.hasNext())
							strMssgs = strMssgs.concat("\n");
								    JOptionPane.showMessageDialog(top,
											      strMssgs,
											      "GIPO Error",
											      JOptionPane.ERROR_MESSAGE);   
					}
				}
				if (top.curDomain == null) {
					top.curDomain = tempDomain;
					top.curDomain.setEnvironment(top);
				} else {
					if (tempDomain.hasTasksOnly()) {
						if (tempDomain.htntasks.size() == 0) {
							if (top.curDomain.tasks.size() == 0) {
								top.curDomain.tasks = tempDomain.tasks;
								JOptionPane.showMessageDialog(
									top,
									"Domain tasks added",
									"GIPO Information",
									JOptionPane.INFORMATION_MESSAGE);
							} else {
								if (JOptionPane.YES_OPTION
									== JOptionPane.showConfirmDialog(
										top,
										"Replace existing tasks",
										"GIPO Query",
										JOptionPane.YES_NO_OPTION)) {
									top.curDomain.tasks = tempDomain.tasks;
									JOptionPane.showMessageDialog(
										top,
										"Domain tasks replaced",
										"GIPO Information",
										JOptionPane.INFORMATION_MESSAGE);
								} else {
									JOptionPane.showMessageDialog(
										top,
										"Domain tasks abandoned",
										"GIPO Information",
										JOptionPane.INFORMATION_MESSAGE);
								}
							}
						} else {
							if (top.curDomain.htntasks.size() == 0) {
								top.curDomain.htntasks = tempDomain.htntasks;
								JOptionPane.showMessageDialog(
									top,
									"Domain tasks added",
									"GIPO Information",
									JOptionPane.INFORMATION_MESSAGE);
							} else {
								if (JOptionPane.YES_OPTION
									== JOptionPane.showConfirmDialog(
										top,
										"Replace existing tasks",
										"GIPO Query",
										JOptionPane.YES_NO_OPTION)) {
									top.curDomain.htntasks =
										tempDomain.htntasks;
									JOptionPane.showMessageDialog(
										top,
										"Domain tasks replaced",
										"GIPO Information",
										JOptionPane.INFORMATION_MESSAGE);
								} else {
									JOptionPane.showMessageDialog(
										top,
										"Domain tasks abandoned",
										"GIPO Information",
										JOptionPane.INFORMATION_MESSAGE);
								}
							}
						}
					} else if (
						(top.curDomain.tasks.size() > 0)
							&& (tempDomain.operators.size() == 0)) {
						if (JOptionPane.YES_OPTION
							== JOptionPane.showConfirmDialog(
								top,
								"Keep existing tasks",
								"GIPO Query",
								JOptionPane.YES_NO_OPTION)) {
							List tempTasks = top.curDomain.tasks;
							top.curDomain = tempDomain;
							top.curDomain.setEnvironment(top);
							top.curDomain.tasks = tempTasks;
							JOptionPane.showMessageDialog(
								top,
								"Domain tasks retained",
								"GIPO Information",
								JOptionPane.INFORMATION_MESSAGE);
						} else {
							top.curDomain = tempDomain;
							top.curDomain.setEnvironment(top);
							JOptionPane.showMessageDialog(
								top,
								"No Domain defined",
								"GIPO Information",
								JOptionPane.INFORMATION_MESSAGE);
						}
					} else {
						if (JOptionPane.YES_OPTION
							== JOptionPane.showConfirmDialog(
								top,
								"Replace Existing Domain",
								"GIPO Query",
								JOptionPane.YES_NO_OPTION)) {
							top.curDomain = tempDomain;
							top.curDomain.setEnvironment(top);
							top.plannerConfig.dirty = true;
							top.plannerConfig.algName = "none";
						    //JOptionPane.showMessageDialog(
							//	top,
							//	"No Domain defined",
							//	"GIPO Information",
							//	JOptionPane.INFORMATION_MESSAGE);
                            /* commented by donghong 28/10/02 */
						} else {
							JOptionPane.showMessageDialog(
								top,
								"Load cancelled",
								"GIPO Information",
								JOptionPane.INFORMATION_MESSAGE);
							return;
						}
					}
				}
				if ("oclDefault".equals(top.curDomain.getName())) {
					String domName;
					int inx = oclFile.getParent().length();
					int inxDot = oclFile.toString().lastIndexOf(".ocl");
					if (inxDot > 0) {
						domName = oclFile.toString().substring(inx + 1, inxDot);
					} else {
						domName = oclFile.toString().substring(inx + 1);
					}
					top.curDomain.setName(domName);
				}
			    /* Donghong added 28/10/02 */
			    //close all opened windows before open a new domain
			    closeAllOpenedInternalFrames();
				top.setTitle(
					"GIPO Planning with Objects [ "
						+ top.curDomain.getName()
						+ " ]");
				// Ron 26/05/05
				if (top.curDomain.isHierarchical()) {
					if (top.hierarchicalSwitch) {
						top.updateMenuBar(OclEd.OCLHIER);
					} else {
						top.updateMenuBar(OclEd.OCLFLAT);
					}
				} else if (top.curDomain.isOclPlus()) {
					top.updateMenuBar(OclEd.OCLPLUS);
				} else {
					top.updateMenuBar(OclEd.OCLFLAT);
				}
				// Ron 20/9/01
				// Do validateion checks
				boolean carryOn = true;
				mssgs.add("Analysing domain " + top.curDomain.getName());
				mssgs.add("Doing Sorts and Objects check.");
				List temp = new ArrayList();
				try {
					temp = top.curDomain.checkSortTree();
				} catch (jplan.general.OCLException e) {
					mssgs.add("Sorts not defined for the domain.");
					mssgs.add("No further checks carried out on this domain.");
					carryOn = false;
				}
				if (carryOn) {
					mssgs.addAll(temp);
					temp.clear();
					if (top.curDomain.predicates.size() == 0) {
						mssgs.add("No predicates defined for this domain.");
						mssgs.add("No further checks carried out.");
						carryOn = false;
					} else {
						mssgs.add("Doing predicate checks.");
						temp = top.curDomain.checkPredicates();
						mssgs.addAll(temp);
						temp.clear();
					}
					if (carryOn && top.curDomain.classDefs.size() == 0) {
						mssgs.add("No states defined for this domain.");
						mssgs.add("No further checks carried out.");
						carryOn = false;
					} else if (carryOn) {
						mssgs.add("Doing state definition checks.");
						temp = top.curDomain.checkStates();
						mssgs.addAll(temp);
						temp.clear();
					}
					if (carryOn) {
						mssgs.add("Doing atomic invariant checks.");
						temp = top.curDomain.checkAtomic();
						mssgs.addAll(temp);
						temp.clear();
					}
					if (carryOn && top.curDomain.operators.size() == 0) {
						mssgs.add("No operators defined for this domain.");
					} else if (carryOn) {
						mssgs.add("Doing operator/transition checks.");
						temp = top.curDomain.checkOps();
						mssgs.addAll(temp);
						temp.clear();
					}
					if (carryOn && top.curDomain.events.size() > 0) {
						// This is oclplus
						mssgs.add("Doing Events checks.");
						temp = top.curDomain.checkEvents();
						mssgs.addAll(temp);
						temp.clear();
					}
					if (carryOn && top.curDomain.processes.size() > 0) {
						// This is oclplus
						mssgs.add("Doing Processes checks.");
						temp = top.curDomain.checkProcesses();
						mssgs.addAll(temp);
						temp.clear();
					}
					if (carryOn
						&& top.curDomain.tasks.size() == 0
						&& top.curDomain.htntasks.size() == 0) {
						mssgs.add("No tasks defined for this domain.");
					} else if (carryOn) {
						mssgs.add("Doing task checks.");
						temp = top.curDomain.checkTasks();
						mssgs.addAll(temp);
						temp.clear();
					}
				}
				// Make sure all Patterns load details from the loaded domain
				//ListIterator liPats = top.curDomain.patterns.listIterator();
				//while (liPats.hasNext()) {
				//	oclPattern pat = (oclPattern) liPats.next();
				//	pat.restoreValuesFromDomain(top.curDomain);
				//}		
				/*Donghong changed 28/10/02 */
	           if (mssgs.size() < 9) {
		           top.checkResultsDialog(mssgs,"All checks passed.");		
	           } else {
				   CheckResultFrame.showMessages(top, mssgs,"Domain Check Messages");
	           }
				// show check results in a internal frame
				// Donghong 25/10/02
				//top.checkResultsDialog(mssgs, "All checks passed.");
				// Make sure all operators have full signatures
				// Ron 4/9/01
				// 		try {
				// 		    ListIterator liOps = 
				// 			top.curDomain.operators.listIterator();
				// 		    while (liOps.hasNext()) {
				// 			oclOperator curOP = (oclOperator)liOps.next();
				// 			/* WZ 19/4/02 */
				// 			oclPredicate curSig = 
				// 			    curOP.createOperatorName(top.curDomain);
				// 			curOP.setName(curSig);
				// 		    }
				// 		    /* Weihong 19/2/02 create method signature */
				// 		    ListIterator liMDs = 
				// 			top.curDomain.methods.listIterator();
				// 		    while (liMDs.hasNext()) {
				// 			oclMethod curMD = (oclMethod)liMDs.next();
				// 			oclPredicate curSig = 
				// 			    top.curDomain.createMethodSignature(curMD);
				// 			curMD.setName(curSig);
				// 		    }
				// 		    /* end 19/2/02 */
				// 		} catch (Exception e) {
				// 		    JOptionPane.showMessageDialog(top,
				// 		        "WARNING run validation checks - the operator names could not be constructed",
				// 						  "GIPO Warning",
				// 						  JOptionPane.WARNING_MESSAGE);
				// 		}
				// Ron  add following line
				file = null;
       /* Donghong added here for IO errors 28/10/02 */
			} catch (IOException e) {
				e.printStackTrace();
				JOptionPane.showMessageDialog(
					top,
					"Domain file not exist, Use New Domain to create.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE);
			} catch (Exception e) {
				e.printStackTrace();
				mssgs = parser.getWarnMssgs();
				String strMssgs = new String("");
				ListIterator li;
				li = mssgs.listIterator();
				while (li.hasNext()) {
					strMssgs = strMssgs.concat((String) li.next() + "\n");
				}
				strMssgs =
					strMssgs.concat(
						"To load correct error in an external editor.");
				/* donghong changed 25/10/02 */
				CheckResultFrame.showMessages(top,strMssgs,"Domain Error Message");
/*				JOptionPane.showMessageDialog(
					top,
					strMssgs,
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE);*/
			}

	}

	/** 
	 * Present a dialog box to have the user select
	 * an existing PDDL Domain specification 
	 */
	public void loadPDDLFile() {
		butSel = DISMISSED;
		fileChoose.setDialogTitle("PDDL Import");
		fileChoose.setFileFilter(pddlFilter);
		int result = fileChoose.showOpenDialog(top);
		File file = fileChoose.getSelectedFile();
		List mssgs = new ArrayList();

		if (file != null
			&& result == JFileChooser.APPROVE_OPTION
			&& butSel == SAVED)
			try {
				parsRes =
					new PDDLparser(new pddlYylex(new FileInputStream(file)))
						.parse();
				top.curPDDLDomain = (pddlDomain) (parsRes.value);
				//curPDDLDomain.pddlPrint
				//    (new PrintWriter (dom = new StringWriter())
				//	,0,false);
				//text.setText(new String(dom.getBuffer()));
				mssgs = PDDLparser.getWarnMssgs();
				String strMssgs = "";
				ListIterator li;
				li = mssgs.listIterator();
				if (li.hasNext()) {
					while (li.hasNext()) {
						strMssgs = strMssgs.concat((String) li.next());
						if (li.hasNext())
							strMssgs = strMssgs.concat("\n");
					}
					JOptionPane.showMessageDialog(
						top,
						strMssgs,
						"GIPO Information",
						JOptionPane.INFORMATION_MESSAGE);
				} else {

					try {
						top.curDomain = top.curPDDLDomain.convertToOcl();
						top.setTitle(
							"GIPO Planning with Objects [ "
								+ top.curDomain.getName()
								+ " ]");
					} catch (Exception e) {
						Utility.debugPrintln(
							"Conversion Exception " + e.toString());
					}
				}
			} catch (Exception e) {
				Utility.debugPrintln(e.toString());
				mssgs = PDDLparser.getWarnMssgs();
				String strMssgs = new String("ERROR\n");
				ListIterator li;
				li = mssgs.listIterator();
				while (li.hasNext()) {
					strMssgs = strMssgs.concat((String) li.next());
					if (li.hasNext())
						strMssgs = strMssgs.concat("\n");
				}
				JOptionPane.showMessageDialog(
					top,
					strMssgs,
					"GIPO Information",
					JOptionPane.INFORMATION_MESSAGE);
			}
	}

	/** 
	 * loadRandomTasks
	 * load the generated random tasks and add to current domain
	 * @param addTasks - if true add generated tasks to existing tasks
	 *                   otherwise replace
	 */
	public void loadRandomTasks(boolean addTasks) {
		File file =
			new File(
				top.strOCLPath
					+ File.separator
					+ "tmp"
					+ File.separator
					+ "randTasks.pl");
		List mssgs = new ArrayList();
		try {
			oclDomain tempDomain;
			FileInputStream oclFIS = new FileInputStream(file);
			parsRes = new parser(new Yylex(oclFIS)).parse();
			tempDomain = (oclDomain) (parsRes.value);
			oclFIS.close();
			oclFIS = null;
			mssgs = parser.getWarnMssgs();
			String strMssgs = new String("");
			ListIterator li;
			li = mssgs.listIterator();
			if (li.hasNext()) {
				while (li.hasNext()) {
					strMssgs = strMssgs.concat((String) li.next());
					if (li.hasNext())
						strMssgs = strMssgs.concat("\n");
				}
				JOptionPane.showMessageDialog(
					top,
					strMssgs,
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE);

			}
			if (addTasks) {
				if (top.curDomain.tasks.size() == 0) {
					top.curDomain.tasks = tempDomain.tasks;
				} else {
					ListIterator liTasks = tempDomain.tasks.listIterator();
					while (liTasks.hasNext()) {
						top.curDomain.tasks.add((oclTask) liTasks.next());
					}
				}
				JOptionPane.showMessageDialog(
					top,
					"Generated tasks added to existing",
					"GIPO Information",
					JOptionPane.INFORMATION_MESSAGE);
			} else {
				top.curDomain.tasks = tempDomain.tasks;
				JOptionPane.showMessageDialog(
					top,
					"Generated tasks added.",
					"GIPO Information",
					JOptionPane.INFORMATION_MESSAGE);
				//force reloading of planner befor running tasks
				top.plannerConfig.dirty = true;
				top.plannerConfig.algName = "none";
			}
		} catch (Exception e) {
			//e.printStackTrace();
			mssgs = parser.getWarnMssgs();
			String strMssgs = new String("");
			//    new String("ERROR: " +  e.toString() + "\n");
			ListIterator li;
			li = mssgs.listIterator();
			while (li.hasNext()) {
				strMssgs = strMssgs.concat((String) li.next() + "\n");
			}
			strMssgs = strMssgs.concat("Consult your system administrator.");
			JOptionPane.showMessageDialog(
				top,
				strMssgs,
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE);
		}
	}

	/**
	 * save current OCL Domain
	 * no dialogue with user
	 * @return CANCELLED or SAVED
	 */
	public int saveFile() {
		if (top.curDomain == null) {
			JOptionPane query =
				new JOptionPane(
					"No Domain Currently Exists",
					JOptionPane.ERROR_MESSAGE,
					JOptionPane.DEFAULT_OPTION);
			JDialog dia = query.createDialog(top, "GIPO Error");
			dia.show();
			return CANCELLED;
		}
		if (oclFile == null) {
			JOptionPane query =
				new JOptionPane(
					"No Current File Open\nUse Save As",
					JOptionPane.ERROR_MESSAGE,
					JOptionPane.DEFAULT_OPTION);
			JDialog dia = query.createDialog(top, "GIPO Error");
			dia.show();
			return CANCELLED;
		}
		File file = oclFile;
		FileWriter domOut;
		if (file != null) {
			try {
				domOut = new FileWriter(file);
				// First set the last modified date

				String strDate = formatter.format(new Date());
				top.curDomain.setDateModified(strDate);
				top.curDomain.oclPrintComponent(new PrintWriter(domOut), 0, false);
				domOut.close();
				JOptionPane.showMessageDialog(
					top,
					"Wrote file " + file.getName(),
					"GIPO Information",
					JOptionPane.INFORMATION_MESSAGE);
			} catch (IOException e) {
				JOptionPane.showMessageDialog(
					top,
					"Problem writing to file",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE);
				return CANCELLED;
			}
		} else {
			return CANCELLED;
		}
		return SAVED;
	}

	/**
	 * save current OCL Domain
	 * request file name from user
	 * @param kind - OCL or PDDL
	 * @return CANCELLED or SAVED
	 */
	public int saveAsFile(int kind) {
		if (top.curDomain == null) {
			JOptionPane query =
				new JOptionPane(
					"No Domain Currently Exists",
					JOptionPane.ERROR_MESSAGE,
					JOptionPane.DEFAULT_OPTION);
			JDialog dia = query.createDialog(top, "GIPO Error");
			dia.show();
			return CANCELLED;
		}
		butSel = DISMISSED;
		
		if (kind == OCL) {
			fileChoose.setDialogTitle("Save As OCL");
			fileChoose.setFileFilter(oclFilter);
			if (oclFile != null) {
				fileChoose.setSelectedFile(oclFile);
			} else {
				fileChoose.setSelectedFile(new File(""));
			}
		} else {
			fileChoose.setDialogTitle("Save As PDDL");
			fileChoose.setFileFilter(pddlFilter);
			if (pddlFile != null){
				fileChoose.setSelectedFile(pddlFile);
			} else if (oclFile != null){
				String fPath = oclFile.toString();
				if (fPath.endsWith(".ocl")) {
					String pfpath = fPath.substring(0,(fPath.length() - 4));
					fileChoose.setSelectedFile(new File(pfpath + ".pddl"));
				}
			} else {
				fileChoose.setSelectedFile(new File(""));
			}
		}
		int result = fileChoose.showSaveDialog(top);
		File file = fileChoose.getSelectedFile();

		FileWriter domOut;
		if (file != null
			&& result == JFileChooser.APPROVE_OPTION
			&& butSel == SAVED) {
			String fName = file.getName();
			if (kind == OCL) {
				if (!fName.endsWith(".ocl")){
					file = new File(file.toString() + ".ocl");
				}
			} else {
				if (!fName.endsWith(".pddl")){
					file = new File(file.toString() + ".pddl");
				}
			}
			if (file.exists()) {
				JOptionPane query =
					new JOptionPane(
						"Overwrite Existing File",
						JOptionPane.QUESTION_MESSAGE,
						JOptionPane.YES_NO_OPTION);
				JDialog dia = query.createDialog(top, "GIPO Query");
				dia.show();
				if (((Integer) query.getValue()).intValue() == 0)
					try {
						domOut = new FileWriter(file);
						String strDate = formatter.format(new Date());
						top.curDomain.setDateModified(strDate);
						if (kind == OCL) {
							top.curDomain.oclPrintComponent(
								new PrintWriter(domOut),
								0,
								false);
						} else {
							top.curDomain.pddlPrintDomain(new PrintWriter(domOut));
						}
						domOut.close();
						JOptionPane.showMessageDialog(
							top,
							"Wrote file " + file.getName(),
							"GIPO Information",
							JOptionPane.INFORMATION_MESSAGE);
					} catch (IOException e) {
						JOptionPane.showMessageDialog(
							top,
							"Problem writing to file",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE);
						return CANCELLED;
					}
			} else {
				try {
					domOut = new FileWriter(file);
					String strDate = formatter.format(new Date());
					top.curDomain.setDateModified(strDate);
					if (kind == OCL) {
						top.curDomain.oclPrintComponent(new PrintWriter(domOut), 0, false);
					} else {
						top.curDomain.pddlPrintDomain(new PrintWriter(domOut));
					}
					domOut.close();
					JOptionPane.showMessageDialog(
						top,
						"Wrote file " + file.getName(),
						"GIPO Information",
						JOptionPane.INFORMATION_MESSAGE);
				} catch (IOException e) {
					JOptionPane.showMessageDialog(
						top,
						"Problem writing to file",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE);
					return CANCELLED;
				}
			}
		} else {
			return CANCELLED;
		}
		if (kind == OCL) {
			oclFile = file;
		} else {
			pddlFile = file;
		}
		return SAVED;
	}

//	/**
//	 * save current OCL Domain as PDDL
//	 * request file name from user
//	 * @return CANCELLED or SAVED
//	 */
//	public int saveAsPDDLFile() {
//		if (top.curDomain == null) {
//			JOptionPane query =
//				new JOptionPane(
//					"No Domain Currently Exists",
//					JOptionPane.ERROR_MESSAGE,
//					JOptionPane.DEFAULT_OPTION);
//			JDialog dia = query.createDialog(top, "GIPO Error");
//			dia.show();
//			return CANCELLED;
//		}
//		// First dump the current domain as temporary file
//		File domOut = null;
//		try {
//			domOut =
//				new File(
//					top.strOCLPath
//						+ File.separator
//						+ "tmp"
//						+ File.separator
//						+ "tmpConvDom.pl");
//			PrintWriter pwTmpDom = new PrintWriter(new FileWriter(domOut));
//			String strDate = formatter.format(new Date());
//			top.curDomain.setDateModified(strDate);
//			top.curDomain.oclPrintComponent(new PrintWriter(pwTmpDom), 0, true);
//			pwTmpDom.close();
//		} catch (IOException e) {
//			JOptionPane.showMessageDialog(
//				top,
//				"IO Error - cannot save temporary files",
//				"GIPO Error",
//				JOptionPane.ERROR_MESSAGE,
//				null);
//			return CANCELLED;
//		}
//		PToolsControl pTools = top.getPrologTools();
//		fileChoose.setFileFilter(pddlFilter);
//		fileChoose.setDialogTitle("Export as PDDL");
//		butSel = DISMISSED;
//
//		if (top.getOclFile() != null) {
//			String sugFile =
//				PToolsControl.toPDDLName(top.getOclFile().getName());
//			fileChoose.setSelectedFile(new File(sugFile));
//		}
//		int result = -1;
//		result = fileChoose.showDialog(top, "Export");
//		File file = fileChoose.getSelectedFile();
//		Utility.debugPrintln("result value is " + result + " but " + butSel);
//		if (file != null
//			&& result == JFileChooser.APPROVE_OPTION
//			&& butSel == SAVED) {
//			if (file.exists()) {
//				JOptionPane query =
//					new JOptionPane(
//						"Overwrite Existing File",
//						JOptionPane.QUESTION_MESSAGE,
//						JOptionPane.YES_NO_OPTION);
//				JDialog dia = query.createDialog(top, "GIPO Query");
//				dia.show();
//				if (((Integer) query.getValue()).intValue() == 0)
//					try {
//						pTools.ocl2pddlConvert(domOut, file);
//						JOptionPane.showMessageDialog(
//							top,
//							"Wrote file " + file.getName(),
//							"GIPO Information",
//							JOptionPane.INFORMATION_MESSAGE);
//					} catch (Exception e) {
//						JOptionPane.showMessageDialog(
//							top,
//							"Problem writing to file",
//							"GIPO Error",
//							JOptionPane.ERROR_MESSAGE);
//						return CANCELLED;
//					}
//			} else {
//				try {
//					pTools.ocl2pddlConvert(domOut, file);
//					JOptionPane.showMessageDialog(
//						top,
//						"Wrote file " + file.getName(),
//						"GIPO Information",
//						JOptionPane.INFORMATION_MESSAGE);
//				} catch (Exception e) {
//					JOptionPane.showMessageDialog(
//						top,
//						"Problem writing to file",
//						"GIPO Error",
//						JOptionPane.ERROR_MESSAGE);
//					return CANCELLED;
//				}
//			}
//		} else {
//			return CANCELLED;
//		}
//		return SAVED;
//	}

	/**
	 * newOCLDomain
	 * Create a new OCL Domain
	 */

	public void newOCLDomain() {
		if (top.curDomain != null) {
			JOptionPane query =
				new JOptionPane(
					"Save Current domain",
					JOptionPane.QUESTION_MESSAGE,
					JOptionPane.YES_NO_CANCEL_OPTION);
			JDialog dia = query.createDialog(top, "GIPO Query");
			dia.show();
			if (((Integer) query.getValue()).intValue()
				== JOptionPane.YES_OPTION) {
				if (saveFile() == CANCELLED) {
					return;
				}
			} else if (
				((Integer) query.getValue()).intValue()
					!= JOptionPane.NO_OPTION) {
				return;
			}
		}
		// 	OCLIdentifierDocument formatter = new OCLIdentifierDocument();
		// 	String domName = GipoInputBox.showInputBox("GIPO Input", "Enter a name",
		// 				      (PlainDocument)formatter);
		// 	formatter = null;

		// 	String domName =  (String)JOptionPane.showInputDialog(top,
		// 					       "Enter Domain Name","GIPO Query",JOptionPane.QUESTION_MESSAGE);
		// 	String domName = 
		// 	    GipoInputBox.showIdentifierInputBox(top,
		// 						"GIPO Input",
		// 						"Enter domain name");
		// 	if (domName != null) {	    
		// Ron
		top.curDomain = new oclDomain(top);
		top.curDomain.setName("none");
		top.curDomain.setHierarchical(false);
		oclFile = null;
        // Ron 8/11/02 added newDomain flag
		DomainProperties domainProp = new DomainProperties(top, top.curDomain, true);
		domainProp.setLocation(
			(int) (0.5 * top.getWidth() - 0.5 * domainProp.getWidth()),
			(int) (0.5 * (top.getHeight() - domainProp.getHeight())));
		domainProp.show();
		if (!top.curDomain.getName().equals("none"))  {
			top.setTitle("GIPO Editor [ " + top.curDomain.getName() + " ]");
		} else {
			// Must have cancelled new domain
			top.curDomain = null;
			top.setTitle("GIPO Editor [ ]");
		}	
		// 	    PatternManager patMan = new PatternManager(top,top.curDomain);
		// 	    top.desktop.add(patMan);
		// 	    top.deskManager.activateFrame(patMan);
		// 	    patMan.show();
		// 	} else {
		// 	    return;
		// 	}
	}

	//     Salford Version
	//     public void newOCLDomain() {
	// 	if (top.curDomain != null) {
	// 	    JOptionPane query = 
	// 		new JOptionPane("Save Current domain",
	// 				JOptionPane.QUESTION_MESSAGE,
	// 				JOptionPane.YES_NO_CANCEL_OPTION);
	// 	    JDialog dia = query.createDialog(top,"OCL Query");
	// 	    dia.show();
	// 	    if (((Integer)query.getValue()).intValue() == 
	// 		JOptionPane.YES_OPTION) {
	// 		if (saveFile() == CANCELLED) {
	// 		    tMssgs.append("New Domain not created\n");
	// 		    return;
	// 		}
	// 	    } else if (((Integer)query.getValue()).intValue() != 
	// 		       JOptionPane.NO_OPTION) {
	// 		return;
	// 	    }
	// 	}
	// 	String domName = 
	// 	    (String)JOptionPane.showInputDialog(top,
	// 						"Enter Domain Name","OCL Query",JOptionPane.QUESTION_MESSAGE);
	// 	if (domName != null) {	    
	// 	    // Ron
	// 	    if (dt == null)
	// 		dt = new dataTransfer();
	// 	    top.curDomain = new oclDomain();
	// 	    top.curDomain.setName(domName);
	// 	    oclFile = null;
	// 	    top.setTitle("OCL Editor [ " 
	// 			 + top.curDomain.getName() + " ]");
	// 	    // Chris adds 14/02/2001

	// 	    SelectDomainFrame dlg_selectDomain = new SelectDomainFrame(top,"",true,dt);
	// 	  //dlg_selectDomain.setLocation(0,0);
	//           dlg_selectDomain.setModal(true);
	//           dlg_selectDomain.show();

	// 	  TemplateFrame dlg_templateFrame = new TemplateFrame(top);
	// 	  top.desktop.add(dlg_templateFrame,BorderLayout.NORTH);

	// 	  OntologyFrame frmOntology = new OntologyFrame(top,dt);
	// 	  top.desktop.add(frmOntology,BorderLayout.NORTH);
	// 	  PropertiesFrame frmProps = new PropertiesFrame(top,dt);
	//           //dlg_templateFrame.setLocation(0,0);
	//           //dlg_templateFrame.setModal(false);
	//           //dlg_templateFrame.show();
	// 	} else {
	// 	    tMssgs.append("No Domain created\n");
	// 	}
	//     }

	/**
	 * get The file object for the current domain
	 */
	public File getOclFile() {
		return oclFile;
	}
	// Donghong added 28/10/02
	public void  closeAllOpenedInternalFrames() {
 	    int numberOfIFrames;
 	    int i;
		JInternalFrame[] jif = top.desktop.getAllFrames();
		numberOfIFrames = jif.length;
		if (numberOfIFrames > 0) {
		  for ( i =0; i < numberOfIFrames; i++) {
			jif[i].dispose();
		  }
		}
	}
}

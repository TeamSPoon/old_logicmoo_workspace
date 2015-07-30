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
 * HGoalVarEditPanel.java
 *
 *
 * Created: Tue Apr 16 10:04:08 2002
 *
 * @author W Zhao
 * @version
 */
import jplan.graphics.transition.*;
import javax.swing.*;
import jplan.graphics.gTool.Graphics.vShape;
import jplan.ocl.*;
import java.util.ListIterator;
import java.util.List;
import java.awt.*;

public class HGoalVarEditPanel extends JPanel {
	private EditPaneGroup editPaneGroup = new EditPaneGroup();
	private MethodHeadCanvas mdCanvas;
	private JPanel actionListPanel = new JPanel();
	private JPanel methodHeadPanel = new JPanel();

	private MethodVarPane mvpMDName;
	private MethodVarPane mvpMDStatics;
	JDialog parent; /* WZ 25/4/02 */
	oclMethod method; /* WZ 29/4/02 */

	public HGoalVarEditPanel(JDialog parent, MethodHeadCanvas mdc) {
		this.parent = parent; /* WZ 25/4/02 */
		setBackground(Color.white);
		mdCanvas = mdc;
		initMethodHead();
		initActionList();
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(methodHeadPanel);
		add(actionListPanel);
	}

	/** 
	 * assemble method name and statics
	 *
	 */
	private void initMethodHead() {
		methodHeadPanel.setBorder(
			new javax.swing.border.LineBorder(java.awt.Color.black));
		methodHeadPanel.setLayout(
			new BoxLayout(methodHeadPanel, BoxLayout.Y_AXIS));
		vShape vs = mdCanvas.getParentShape();

		if (vs != null) { /* WZ 3/5/02 */
			HighLevelTransitionWindow htw =
				(HighLevelTransitionWindow) mdCanvas.getParentFrame();
			method = htw.getMethod(vs); /* WZ 29/4/02 */

			//assemble method name
			mvpMDName = new MethodVarPane(mdCanvas.getWorkingDomain());
			// 	mvpMDName.setObject(method); //oclMethod
			try {
				mvpMDName.addPredicate(method.getName());
			} catch (Exception e) {
				Utility.debugPrintln(e.toString());
			}
			addPanel("METHOD NAME", mvpMDName, methodHeadPanel);
		} else {
			jplan.edexpt.HGoalStateWindow hsw =
				(jplan.edexpt.HGoalStateWindow) mdCanvas.getParentFrame();
			method = hsw.getCurMethod();
		}

		//assemble statics
		mvpMDStatics = new MethodVarPane(mdCanvas.getWorkingDomain());
		java.util.List staticsList = (java.util.List) method.getStatics();
		mvpMDStatics.setObject(staticsList);
		ListIterator li = staticsList.listIterator();
		while (li.hasNext()) {
			oclPredicate opd = (oclPredicate) li.next();
			try {
				mvpMDStatics.addPredicate(opd);
			} catch (Exception e) {
				Utility.debugPrintln(e.toString());
			}
		}
		addPanel("STATICS", mvpMDStatics, methodHeadPanel);
	}

	// Ron 29/4/03 - Changed routine to get prototypes of achieve goals to work even
	// when sorts of predicates are not set
	/** 
	 * assemble decompositions
	 *
	 */
	private void initActionList() {
		actionListPanel.setBorder(
			new javax.swing.border.LineBorder(java.awt.Color.black));
		// 	actionListPanel.setLayout(new GridLayout(0,1)); 
		actionListPanel.setLayout(
			new BoxLayout(actionListPanel, BoxLayout.Y_AXIS));

		for (int i = 1; i < mdCanvas.getShapeCount() + 1; i++) {
			vShape tempShape = mdCanvas.getVShape(String.valueOf(i));
			switch (tempShape.getShapeID()) {
				case 3 : //oclSS
					try {
						oclSS ss =
							(oclSS) ((oclSS) tempShape.getObject()).clone();

						MethodVarPane mvpSS =
							new MethodVarPane(mdCanvas.getWorkingDomain());
						mvpSS.setObject(ss);
						ListIterator li = ss.getState().listIterator();
						while (li.hasNext()) {
							oclPredicate opt = (oclPredicate) li.next();
							try {
								opt.getProtoType(mdCanvas.getWorkingDomain());
								// Side effect sets sorts of opt
								mvpSS.addPredicate(opt);
							} catch (Exception e) {
								Utility.debugPrintln(e.toString());
								break;
							}
						}
						addPanel("LIST SS", mvpSS, actionListPanel);
					} catch (CloneNotSupportedException e) {
						break;
					}
					break;
				case 5 : //oclMethod
					oclPredicate mdName = (oclPredicate) tempShape.getObject();
					MethodVarPane mvpMD =
						new MethodVarPane(mdCanvas.getWorkingDomain());

					//WZ 18/4/02
					//get the predicate with prototype
					oclMethod omd =
						(oclMethod) mdCanvas
							.getWorkingDomain()
							.checkObjectType(
							mdName);
					try {
						mvpMD.addPredicate(omd.getName());
					} catch (Exception e) {
						Utility.debugPrintln(e.toString());
					}

					mvpMD.setObject(omd); /* WZ 25/4/02 */
					addPanel("LIST MD", mvpMD, actionListPanel);
					break;
				case 0 : //oclOperator
					oclOperator op = (oclOperator) tempShape.getObject();
					oclPredicate opName = (oclPredicate) op.opName;
					MethodVarPane mvpOP =
						new MethodVarPane(mdCanvas.getWorkingDomain());
					mvpOP.setObject(op);
					try {
						mvpOP.addPredicate(opName);
					} catch (Exception e) {
						Utility.debugPrintln(e.toString());
					}
					addPanel("LIST OP", mvpOP, actionListPanel);
					break;
			}
		}
	}

	/* WZ 24/4/02 */
	/**
	 * add a set of panels to this panel
	 */
	private void addPanel(String name, MethodVarPane mp, JPanel panel) {
		panel.add(
			new PropertyButton(parent, name, mp, mdCanvas.getWorkingDomain()));
		editPaneGroup.addPane((MethodVarPane) mp);
	}

	/* WZ 29/4/02 */
	/**
	 * to update the variables
	 */
	public void updateVariable() {
		vShape vs = mdCanvas.getParentShape();
		oclMethod md = getOclMethod();
		// Ron 1/5/03 make sure that the Id's of any achieve ss(s) match the values in the 
		// predicate list
		// md.updateAchieveIDs(mdCanvas.getWorkingDomain());
		if (vs != null) { /* WZ 3/5/02 */
			HighLevelTransitionWindow htw =
				(HighLevelTransitionWindow) mdCanvas.getParentFrame();
			htw.setCurMethod(md);
			htw.showGraphics(md);
			htw.setCurMethod(md); //Ron keep this as the current method
		} else {
			jplan.edexpt.HGoalStateWindow hsw =
				(jplan.edexpt.HGoalStateWindow) mdCanvas.getParentFrame();
			hsw.refreshWindow();
			hsw.populateTask(md);
		}
	}

	/* WZ 29/4/02 */
	/**
	 * return the changed content/variables in terms of an oclMethod
	 */
	private oclMethod getOclMethod() {
		Utility.debugPrintln("getOclMethod ..");
		oclMethod retMD = new oclMethod();
		ListIterator li;

		if (mvpMDName != null) { /* WZ 3/5/02 */
			//get name
			li = mvpMDName.getFullPredicateList().listIterator();
			if (!li.hasNext()) {
				JOptionPane.showMessageDialog(
					parent,
					"Method name lost.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
			}
			retMD.setName((oclPredicate) li.next());
		}
		oclPredicate oldName = method.getName();
		oclPredicate newName = retMD.getName();
		//get statics
		retMD.setStatic(mvpMDStatics.getFullPredicateList());

		//get decomposition list
		List listDec = new java.util.ArrayList();
		for (int i = 0; i < actionListPanel.getComponentCount(); i++) {
			PropertyButton pb =
				(PropertyButton) actionListPanel.getComponent(i);
			MethodVarPane mvp = pb.getMethodVarPane();
			if (mvp
				.getObject()
				.getClass()
				.getName()
				.equals("jplan.ocl.oclSS")) {
				try {
					oclSS ss = (oclSS) ((oclSS) mvp.getObject()).clone();
					if (!ss.getName().equals(mvp.getObjectID())) {
						// Ron if the ss ID has changed we need to set it to the new name
						Utility.debugPrintln("XXXX changes ss ID to " + mvp.getObjectID());
						ss.setName(mvp.getObjectID());
					}
					ss.setState(mvp.getFullPredicateList());
					listDec.add(ss);
				} catch (Exception e) {
					Utility.debugPrintln(" Something wrong" + e.toString());
				}
			} else {
				li = mvp.getFullPredicateList().listIterator();
				if (!li.hasNext()) {
					JOptionPane.showMessageDialog(
						parent,
						"lost one decomposition.",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE,
						null);
				}
				try {
					oclPredicate opd =
						(oclPredicate) ((oclPredicate) li.next()).clone();
					listDec.add(opd);
				} catch (CloneNotSupportedException e) {
				}
			}
		}
		retMD.setDecomps(listDec);

		//keep the same temproal constraints
		retMD.setTemps(method.getTemps());

		//debug print
		ListIterator liStatics = retMD.getTemps().listIterator();
		Utility.debugPrintln("\n\n*** getOclMethod ***");
		while (liStatics.hasNext()) {
			Utility.debugPrintln(((oclPredicate) liStatics.next()).toString());
		}

		//change index and precondition

		/* WZ 7/5/02 */
		if (oldName == null)
			return retMD;
// Ron - a complete rework here
// get the method element then do all necessary replacements
	//		if (!before.equals(after)) {
				//oclSE
		li = method.getPrecondition().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			for (int i = 0; i < oldName.size(); i++) {
				String before = oldName.getNthElementName(i);
				String after = newName.getNthElementName(i);
				if (!before.equals(after)) {
					Utility.debugPrintln("replace SE .." + se.toString());
					se.replaceVariableName(before, after);
				}
			}
			try {
				retMD.addPreSE((oclSE) se.clone());
			} catch (CloneNotSupportedException e) {
			}
		}
		//oclSC 
		li = method.getIndex().listIterator();
		while (li.hasNext()) {
			oclSC sc = (oclSC) li.next();
			for (int i = 0; i < oldName.size(); i++) {
				String before = oldName.getNthElementName(i);
				String after = newName.getNthElementName(i);
				if (!before.equals(after)) {
					Utility.debugPrintln("replace SC .." + sc.toString());
					sc.replaceVariableName(before, after);
				}
			}
			try {
				retMD.addIndexSC((oclSC) sc.clone());
			} catch (CloneNotSupportedException e) {
			}
		}
	//		}
	//	}
		return retMD;
	}

} // HGoalVarEditPanel

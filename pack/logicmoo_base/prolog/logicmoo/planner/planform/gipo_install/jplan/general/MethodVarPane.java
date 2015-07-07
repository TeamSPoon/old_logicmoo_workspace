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
 * methodVarPane.java
 *
 *
 * Created: Mon Apr 15 16:08:10 2002
 *
 * @author W Zhao
 * @version
 */
/*
 * History
 * 10/11/02 Ron added code to set object id to prevent renaming when
 *          the pane object is set to an ss
 * 28/4/03 Ron added code to ensure that when a variable in a ss is changed the matching 
 *         ss id is also changed
 */
import java.awt.Point;
import java.util.List;
import jplan.ocl.oclSS;

public class MethodVarPane extends TransExpressionDocPane {
	private Object object;

	/* WZ 8/5/02 */
	private javax.swing.plaf.basic.BasicComboPopup popupMenuInstantiate;
	private int argStart = 0; //the position of the argument of a predicate
	private String oldV = null; //the argument to be replaced
	int ArgNo = 0;
	javax.swing.JMenuItem instanMI;

	java.awt.Component theInvoker;
	int locX, locY;
	/* end 8/5/02 */

	public MethodVarPane(jplan.ocl.oclDomain cur) {
		super(cur);
		setVisible(true);
	}

	/**
	   * Get the value of object.
	   * @return Value of object.
	   */
	public Object getObject() {
		return object;
	}

	/**
	 * Set the value of object.
	 * @param v  Value to assign to object.
	 */
	// 10/11/02 Ron added code to set object id to prevent renaming
	public void setObject(Object v) {
		this.object = v;
		if (object.getClass().getName().equals("jplan.ocl.oclSS")) {
			Utility.debugPrintln(
				"XXX set SS ID to " + ((oclSS) object).getName());
			setObjectID(((oclSS) object).getName());
		}
	}

	/* WZ 8/5/02 */
	/**
	 * mouseWatch
	 * watch for selections of variables and requests for popups
	 *
	 */
	public void mouseWatch(java.awt.event.MouseEvent mouseEvent) {
		Utility.debugPrintln("Method var pane.. ");
		Point viewPoint = mouseEvent.getPoint();
		clickPos = this.viewToModel(viewPoint);
		jplan.ocl.oclPredicate curPred = null;
		try {
			curPred = curExpModel.getPredicateAt(clickPos);
		} catch (ExpressionModel.ExpressionException ee) {
			return;
		}

		if (javax.swing.SwingUtilities.isRightMouseButton(mouseEvent)) {
			if (curPred.getName().equals("ne")
				|| curPred.getName().equals("is_of_sort")) {
				deletePopupMenu.show(
					mouseEvent.getComponent(),
					mouseEvent.getX(),
					mouseEvent.getY());
			} else {
				Point selPoint = getSelectedVar();
				if (selPoint.x <= clickPos && selPoint.y >= clickPos) {
					// This is the currently selected variable
					List subTypes = curDomain.getSortSubTypes(selectedType);

					try {
						String sortBranch =
							curExpModel.getSortForArgumentAt(clickPos);
						Utility.debugPrintln("sortBranch>> " + sortBranch);
						initInstantiatePopup(sortBranch);
						ExpressionModel.PredDetail cur =
							curExpModel.getPredDetailAt(clickPos);
						ArgNo = curPred.elementNoAt(clickPos - cur.startOffset);
						getCurrentPredInfo(clickPos, ArgNo);
						theInvoker = mouseEvent.getComponent();
						locX = mouseEvent.getX();
						locY = mouseEvent.getY();
					} catch (Exception e) {
						Utility.debugPrintln(e);
					}

					if (subTypes != null) {
						restrictPopupMenu.show(
							mouseEvent.getComponent(),
							mouseEvent.getX(),
							mouseEvent.getY());
					} else {
						popupMenuRename.show(
							mouseEvent.getComponent(),
							mouseEvent.getX(),
							mouseEvent.getY());
					}
				} else {
					java.util.ListIterator li =
						getMatchingVars().listIterator();
					boolean found = false;
					while (li.hasNext() && !found) {
						Point next = (Point) li.next();
						if (next.x <= clickPos && next.y >= clickPos) {
							popupTarget.x = next.x;
							popupTarget.y = next.y;
							found = true;
						}
					}
					if (found) {
						popupMenu.show(
							mouseEvent.getComponent(),
							mouseEvent.getX(),
							mouseEvent.getY());
					}
				}
			}
		} else {
			// We need to highlight variables
			int pos = clickPos;
			ExpressionModel.PredDetail cur = null;
			String arg = null;
			try {
				cur = curExpModel.getPredDetailAt(pos);
			} catch (Exception e) {
				// No predicate found
				return;
			}
			try {
				arg = cur.pred.elementAt(pos - cur.startOffset);
				int startOffset =
					cur.startOffset
						+ cur.pred.startElementAt(pos - cur.startOffset);
				int n = cur.pred.elementNoAt(pos - cur.startOffset);
				selectedType = cur.proto.getNthElementName(n);
				List unifiers = curDomain.getSortUnifiers(selectedType);
				selectedArg = arg;
				searcher.markSelected(startOffset, startOffset + arg.length());
				unifiers.add(selectedType);
				selectedUnifiers = unifiers;
				searcher.searchUnifiers(
					unifiers,
					startOffset,
					startOffset + arg.length());
				lastSelPred = cur;
				fireEvent(
					new ExpressionPaneEvent(
						this,
						ExpressionPaneEvent.SELECTION,
						arg));
				selectedArg = arg;
			} catch (Exception e) {
				if (e instanceof java.util.NoSuchElementException) {
					Utility.debugPrintln("Illegal sort name!!");
					return;
				}

				// Ignore dot outside predicate most likely at the 
				// end of the document
			}
		}
	}

	/* WZ 8/5/02 */
	/**
	 * Inheriated from its parents with added function of
	 * instantiation of variables
	 */
	protected void initRestrictPopup() {
		super.initRestrictPopup();

		instanMI = new javax.swing.JMenuItem("Instantiate variables");
		popupTarget = new Point(-1, -1);
		instanMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				popupMenuInstantiate.show(theInvoker, locX, locY);
			}
		});
		restrictPopupMenu.add(instanMI);
	}

	/* WZ 19/8/02 */
	/**
	 * init rename popup menu
	 * create the edit target variable name
	 */
	protected void initRenamePopup() {
		super.initRenamePopup();

		instanMI = new javax.swing.JMenuItem("Instantiate variables");
		popupTarget = new Point(-1, -1);
		instanMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				popupMenuInstantiate.show(theInvoker, locX, locY);
			}
		});
		popupMenuRename.add(instanMI);
	}

	/* WZ 8/5/02 */
	// Ron 28/4/03 Added code to ensure that ss ids are changed when matching
	// variable in a state predicate is changed
	/**
	 * init task popup menu to create the edit target variable name.
	 * @param sort the oclSort name
	 * 
	 */
	protected void initInstantiatePopup(String sort) {
		java.util.Vector items = new java.util.Vector();
		java.util.List objNames = curDomain.getObjectsOfSubTypes(sort);
		// Process the objects
		if (objNames != null) {
			java.util.ListIterator liObj = objNames.listIterator();
			String obj;
			while (liObj.hasNext()) {
				obj = (String) liObj.next();
				items.addElement(obj);
			}
		}
		//build the PopupMenu
		javax.swing.JComboBox combo = new javax.swing.JComboBox(items);
		combo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				try {
					String objname =
						(String) (popupMenuInstantiate
							.getList()
							.getSelectedValue());
					Point selPoint = getSelectedVar();	
					// Ron 2/5/03
					String oldID = oldV;	
					selPoint.x = replaceAllVariables(selPoint.x,oldV,objname);
					selPoint.y = selPoint.x + objname.length();
					
//					curExpModel.editVar(selPoint.x, objname);
					// Ron 28/4/03
					if (object
						.getClass()
						.getName()
						.equals("jplan.ocl.oclSS")) {
						oclSS objSS = (oclSS) object;
						if (objSS.getName().equals(oldID)) {
							objSS.setName(objname);
							setObjectID(objname); // Ron 2/5/03 added this

						}
						
					}
// Ron 2/5/03 removed following
//					doc.remove(argStart, oldV.length());
//					doc.insertString(
//						argStart,
//						objname,
//						new javax.swing.text.SimpleAttributeSet());

					removeHighlights();
					/* WZ 9/5/02 changed 'this' to 'MethodVarPane.this' */
					fireEvent(
						new ExpressionPaneEvent(
							MethodVarPane.this,
							ExpressionPaneEvent.CLEAR));
					popupMenuInstantiate.hide(); /* WZ 15/5/02 */
				} catch (Exception e) {
					Utility.debugPrintln(e);
				}
			}
		});
		popupMenuInstantiate =
			new javax.swing.plaf.basic.BasicComboPopup(combo);
	}

	/* WZ 8/5/02 */
	/**
	 * hold enough information for the current predicate which are dealt with.
	 * @param clickPoint the position of the variable which has been clicked
	 * @param argNo the order number of the variable which has been clicked
	 * 
	 */
	private void getCurrentPredInfo(int clickPoint, int argNo) {
		try {
			ExpressionModel.PredDetail curPredDetail =
				curExpModel.getPredDetailAt(clickPoint);
			int offset = curPredDetail.pred.startElementNo(argNo + 1);
			argStart = curPredDetail.startOffset + offset;
			oldV = curExpModel.getArgumentAt(clickPoint);
		} catch (Exception e) {
			Utility.debugPrintln("Unexpected failure to edit variable" + e);
		}
	}

} // methodVarPane

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
 * Created on 20-Oct-2004
 * 
 * Author ron
 *  
 */
package jplan.tools.lifeHist;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.net.URL;
import java.util.*;

import javax.swing.ImageIcon;

import org.jgraph.JGraph;
import org.jgraph.graph.*;
import org.jgraph.cellview.JGraphRoundRectView;


import org.jgraph.cellview.JGraphEllipseView;
/**
 * @author ron 
 * Derived from JGraph Example
 * Custom Graph
 *
 * Defines a Graph that uses the Shift-Button (Instead of the Right
 * Mouse Button, which is Default) to add/remove point to/from an edge.
 */

public class LHGraph extends JGraph {
	
	boolean mergesVisible = true;
	List objectSortsInVisible = null; // contains references to objects and their transitions switched off
	List objectsInVisible = null; // contains the Sort Names of objets switched off
	List objectDescriptionsOn = null;
	
	// Construct the Graph using the Model as its Data Source
	public LHGraph(GraphModel model) {
		super(model);
		objectsInVisible = new ArrayList();
		objectSortsInVisible = new ArrayList();
		objectDescriptionsOn = new ArrayList();
		// Use a Custom Marquee Handler
		//setMarqueeHandler(new HistoryWindow.LHMarqueeHandler());
		// Tell the Graph to Select new Cells upon Insertion
		// TODO do I still need an equivalent setSelectNewCells(true);
		// Make Ports Visible by Default
		setPortsVisible(true);
		// Use the Grid (but don't make it Visible)
		setGridEnabled(true);
		// Set the Grid Size to 10 Pixel
		setGridSize(6);
		// Set the Tolerance to 2 Pixel
		setTolerance(2);
		// Accept edits if click on background
		setInvokesStopCellEditing(true);
		// Allows control-drag
		setCloneable(true);
		// Jump to default port on connect
		setJumpToDefaultPort(true);
		getGraphLayoutCache().setAutoSizeOnValueChange(true);
		getGraphLayoutCache().setFactory(new DefaultCellViewFactory() {
			/**
			 * Creates and returns a default <code>GraphView</code>.
			 * 
			 * @return the default <code>GraphView</code>
			 */
			protected VertexView createVertexView(Object v) {
				if (v instanceof TransitionCell)
					return new JGraphRoundRectView(v);
//				else if (v instanceof StateCell)
//					return new JGraphMultilineView(v);
				return super.createVertexView(v);
			}
		});
	}
	public LHGraph(GraphModel model,BasicMarqueeHandler mh){
		this(model);
		setMarqueeHandler(mh);
	}
	// Override Superclass Method to Return Custom EdgeView
	// Override Superclass Method to Return Custom EdgeView
	protected EdgeView createEdgeView(Object cell) {
		// Return Custom EdgeView
		return new EdgeView(cell) {
			
			/**
			 * Returns a cell handle for the view.
			 */
			public CellHandle getHandle(GraphContext context) {
				return new LHEdgeHandle(this, context);
			}
			
		};
	}
	
	/**
	 * toggleMerges 
	 * make merge Edge visible or invisible
	 * @param on = make visible
	 */
	public void toggleMerges(boolean on){
		mergesVisible = on;
		GraphModel model = getModel();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof MergeEdge) {
				// This node refers to a merge
				toggleMerge((MergeEdge)curRoot,on);
			}
		}
	}
	public void toggleObjectDescriptions(String objSort){
		boolean on = switchDescriptions(objSort);
		GraphModel model = getModel();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell){
				// This is a state of one of our objects
				LHUserObject obj = (LHUserObject)((StateCell)curRoot).getUserObject();
				String curObjSort = (String)obj.getProperty("ObjectSort");
				if (objSort.equals(curObjSort)) {
					StateCell state = (StateCell)curRoot;
					obj.switchStateDescription(on);
					Map nested = new Hashtable();
					AttributeMap aMap = state.getAttributes();
					GraphConstants.setResize(aMap,true);
					nested.put(state,aMap);
					getModel().edit(nested,null,null,null);
				}
			} else if (curRoot instanceof TransitionCell) {
				LHUserObject obj = (LHUserObject)((TransitionCell)curRoot).getUserObject();
				String curObjSort = (String)obj.getProperty("ObjectSort");
				if (objSort.equals(curObjSort)) {
					TransitionCell trans = (TransitionCell)curRoot;
					obj.switchVTDescription(on);
					Map nested = new Hashtable();
					AttributeMap aMap = trans.getAttributes();
					GraphConstants.setResize(aMap,true);
					nested.put(trans,aMap);
					getModel().edit(nested,null,null,null);
				}
			}
		}
	}
	/**
	 * toggleObjects 
	 * make Objects and connecting Edges visible or invisible
	 * @param objSort  the name of the sort to toggle
	 */
	public void toggleObjects(String objSort){
		boolean on = switchSort(objSort);
		GraphModel model = getModel();
		URL iconURL = HistoryWindow.class.getResource("resources/icoInvisib.png");
		ImageIcon icon  = null;
		if (iconURL != null) {
			icon = new ImageIcon(iconURL);
		}
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell){
				// This is a state of one of our objects
				LHUserObject obj = (LHUserObject)((StateCell)curRoot).getUserObject();
				String curObjSort = (String)obj.getProperty("ObjectSort");
				if (objSort.equals(curObjSort)) {
					recordState((DefaultGraphCell)curRoot,on);
					Map nested = new Hashtable();
					StateCell state = (StateCell)curRoot;
					AttributeMap aMap = state.getAttributes();
					if (on) {
						GraphConstants.setBorderColor(aMap,Color.BLACK);
						GraphConstants.setForeground(aMap,Color.BLACK);
						GraphConstants.setIcon(aMap,(javax.swing.Icon)obj.getProperty("icon"));
					} else {
						obj.putProperty("icon",GraphConstants.getIcon(aMap));
						if (icon != null)
							GraphConstants.setIcon(aMap,icon);
						GraphConstants.setBorderColor(aMap,Color.WHITE);
						GraphConstants.setForeground(aMap,Color.WHITE);
					}
					nested.put(state,aMap);
					getModel().edit(nested, null, null, null);
					toggleLinks((StateCell)curRoot,on);
				}
			}
		}
	}

	/**
	 * toggleLinks
	 * follow links from state cells to turn edges and transitions on/off
	 */
	private void toggleLinks(StateCell node, boolean on){
		GraphModel model = getModel();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof TransitionEdge) {
				// This is a connecting edge
				TransitionEdge edge = (TransitionEdge)curRoot;
				DefaultGraphCell target = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
				DefaultGraphCell source = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
				if (source == node ||target == node) {	
					Map nested = new Hashtable();
					AttributeMap aMap = edge.getAttributes();
					if (on) {
						GraphConstants.setLineColor(aMap,Color.BLACK);
					} else {
						GraphConstants.setLineColor(aMap,Color.WHITE);
					}
					nested.put(edge,aMap);
					getModel().edit(nested, null, null, null);
				}
				if (source == node) {
					// Target must be a TransitionCell
					recordState(target,on);
					// Toggle it
					TransitionCell transition = (TransitionCell)target;
					LHUserObject user = (LHUserObject)transition.getUserObject();
					Map nested = new Hashtable();
					AttributeMap aMap = transition.getAttributes();
					if (on) {
						GraphConstants.setBorderColor(aMap,Color.BLACK);
						GraphConstants.setForeground(aMap,Color.BLACK);
						GraphConstants.setGradientColor(aMap,(Color)user.getProperty("Gradient Colour"));
					} else {
						user.putProperty("Gradient Colour",GraphConstants.getGradientColor(aMap));
						GraphConstants.setBorderColor(aMap,Color.WHITE);
						GraphConstants.setForeground(aMap,Color.WHITE);
						GraphConstants.setGradientColor(aMap,Color.WHITE);
					}
					nested.put(transition,aMap);
					getModel().edit(nested, null, null, null);
					toggleMergesToTransition(transition,on);
				}
			} else if (curRoot instanceof MergeEdge) {
				MergeEdge merge = (MergeEdge) curRoot;
				DefaultGraphCell target = (DefaultGraphCell)((DefaultPort)merge.getTarget()).getParent();
				DefaultGraphCell source = (DefaultGraphCell)((DefaultPort)merge.getSource()).getParent();
				if (source == node ||target == node) {	
					toggleMerge(merge,on);
				}
			}
		}
	}
	/**
	 * toggleMergesToTransition
	 * find any attached merge links and toggle on and off
	 * @param transition
	 * @param on
	 */
	private void toggleMergesToTransition(TransitionCell transition,boolean on){
		if (!mergesVisible) {
			// Nothing to do
			return;
		}
		GraphModel model = getModel();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof MergeEdge) {
				MergeEdge edge = (MergeEdge)curRoot;
				DefaultGraphCell target = (DefaultGraphCell)((DefaultPort)edge.getTarget()).getParent();
				DefaultGraphCell source = (DefaultGraphCell)((DefaultPort)edge.getSource()).getParent();
				if (source == transition ||target == transition) {	
					toggleMerge(edge,on);
				}
			}
		}
	}
	/**
	 * toggleMerge
	 * switch merges visible / invisible
	 * @param merge
	 * @param on
	 */
	private void toggleMerge(MergeEdge merge,boolean on){
		DefaultGraphCell target = (DefaultGraphCell)((DefaultPort)merge.getTarget()).getParent();
		DefaultGraphCell source = (DefaultGraphCell)((DefaultPort)merge.getSource()).getParent();
		
		Map nested = new Hashtable();
		AttributeMap aMap = merge.getAttributes();
		if (on && !objectsInVisible.contains(target) && !objectsInVisible.contains(source)) {
				GraphConstants.setLineColor(aMap,Color.RED);
		} else {
			GraphConstants.setLineColor(aMap,Color.WHITE);
		}
		nested.put(merge,aMap);
		getModel().edit(nested, null, null, null);
	}
	
	private boolean switchSort(String objSort) {
		boolean ans = false;
		if (objectSortsInVisible.contains(objSort)) {
			objectSortsInVisible.remove(objSort);
			ans = true;
		} else {
			objectSortsInVisible.add(objSort);
			ans = false;
		}
		return ans;
	}
	private boolean switchDescriptions(String objSort) {
		boolean ans = false;
		if (objectDescriptionsOn.contains(objSort)) {
			objectDescriptionsOn.remove(objSort);
			ans = false;
		} else {
			objectDescriptionsOn.add(objSort);
			ans = true;
		}
		return ans;
	}
	/**
	 * recordState
	 * manage the list of objects currently invisible
	 * @param cell - StateCell or MergeCell
	 * @param on
	 */
	private void recordState(DefaultGraphCell cell,boolean on){
		if (on) {
			objectsInVisible.remove(cell);
		} else {
			objectsInVisible.add(cell);
		}
	}

}
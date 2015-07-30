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
 * Created on 20-Oct-2004
 *
 * Author ron
 * 
 */
package jplan.tools.lifeHist;

import java.awt.event.MouseEvent;

import org.jgraph.JGraph;
import org.jgraph.event.GraphSelectionEvent;
import org.jgraph.event.GraphSelectionListener;
import org.jgraph.graph.BasicMarqueeHandler;
import org.jgraph.graph.CellHandle;
import org.jgraph.graph.CellMapper;
import org.jgraph.graph.CellView;
import org.jgraph.graph.ConnectionSet;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.Edge;
import org.jgraph.graph.EdgeView;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphContext;
import org.jgraph.graph.GraphModel;
import org.jgraph.graph.GraphUndoManager;
import org.jgraph.graph.ParentMap;
import org.jgraph.graph.Port;
import org.jgraph.graph.PortView;

import org.jgraph.cellview.JGraphEllipseView;
import org.jgraph.graph.VertexRenderer;
import org.jgraph.graph.VertexView;

/**
 * @author ron
 * Derived from JGraph Example
 * Custom Edge Handle
 *
 * Defines a EdgeHandle that uses the Shift-Button (Instead of the Right
 * Mouse Button, which is Default) to add/remove point to/from an edge.
 */
public class LHEdgeHandle extends EdgeView.EdgeHandle {

	/**
	 * @param edge
	 * @param ctx
	 */
	public LHEdgeHandle(EdgeView edge, GraphContext ctx) {
		super(edge, ctx);
	}
	// Override Superclass Method
	public boolean isAddPointEvent(MouseEvent event) {
		// Points are Added using Shift-Click
		return event.isShiftDown();
	}
	// Override Superclass Method
	public boolean isRemovePointEvent(MouseEvent event) {
		// Points are Removed using Shift-Click
		return event.isShiftDown();
	}
	
}

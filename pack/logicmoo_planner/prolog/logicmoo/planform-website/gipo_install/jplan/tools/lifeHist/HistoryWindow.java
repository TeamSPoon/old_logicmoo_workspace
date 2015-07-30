/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.TextNote
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
 * Created on 12-Oct-2004
 * 
 * Author ron
 *  
 */
package jplan.tools.lifeHist;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.Insets;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

import javax.swing.AbstractAction;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.UndoableEditEvent;
import javax.swing.JFileChooser;
import org.jgraph.JGraph;
import org.jgraph.event.GraphSelectionEvent;
import org.jgraph.event.GraphSelectionListener;
import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.BasicMarqueeHandler;
import org.jgraph.graph.CellView;
import org.jgraph.graph.ConnectionSet;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.Edge;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphModel;
import org.jgraph.graph.GraphUndoManager;
import org.jgraph.graph.ParentMap;
import org.jgraph.graph.Port;
import org.jgraph.graph.PortView;

import jplan.tools.lifeHist.PalletManager.InstantiatePanel;
import jplan.top.OclEd;
import jplan.ocl.oclDomain;
import jplan.ocl.oclPredicate;
import jplan.general.GipoInternalFrame;
import jplan.general.OCLException;
import jplan.general.PNGThumbNailViewer;
import jplan.general.Utility;
import jplan.general.EdTextComboBox;
import jplan.general.OCLIdentifierDocument;
import jplan.general.TextComboBoxEditor;
import jplan.general.GipoInputBox;
import jplan.top.ExtensionFileFilter;

/**
 * @author ron
 * 
 * This is the Window containing the life history editing canvas Used to
 * generate domain definitions from object life histories uses JGraph to draw
 * and maintain graphics
 */
public class HistoryWindow extends GipoInternalFrame implements
		GraphSelectionListener, KeyListener {
	/**
	 * parent frame
	 */
	private OclEd top;

	private oclDomain curDomain;

	// Edit state Constants
	public final static int SELECT = 0;

	public final static int INSERTSTATE = 1;

	public final static int INSERTTRANSITION = 2;

	public final static int INSERTLINK = 4;

	public final static int INSERTONEWAYMERGELINK = 5;

	public final static int INSERTTWOWAYMERGELINK = 6;
	
	public final static int INSERTLIBRARYITEM = 7; 
	
	public final static int INSERTTEXT = 8;
	
	public final static int INSERTPROCESS = 9;
	
	public final static int INSERTPROCESSLINK = 10;

	private int editMode = SELECT; // This is the variable to store the edit
								   // editMode

	// Node constants
	public final static int STATENODE = 0;

	public final static int TRANSNODE = 1;
	
	public static final int TEXTNODE = 3; // Just textual comment on diagram
	
	public static final int PROCESSNODE = 4; 
	

	// Node counters
	private int stateCount = 0;

	private int transCount = 0;
	
	private int processCount = 0;

	// JGraph instance
	protected JGraph graph;
	protected JGraph dsGraph; // Stores data structures for the domain
	
	// PalletManager - looks after library
	private PalletManager palletMgr;

	// Undo Manager
	protected GraphUndoManager undoManager;
	protected GraphUndoManager dsUndoManager;
	
	// Actions which Change State
	protected Action undo, redo, remove, group, ungroup, tofront, toback, cut,
			copy, paste, closeAction, toOCL;

	// Stores The current object sort for life history being edited
	private EdTextComboBox cmbSortName = null;

	// Store object type icons
	private File iconFile = null;
	private ImageIcon procIcon = null;
	private JLabel lblIcon = null;

	// Record if edits done
	boolean dirty = false;

	// Open Save constants
	static final int SAVED = 0; // Return value for Save Dialog

	static final int CANCELLED = -1;

	static final int DISMISSED = -2;

	private File graphFile = null;

	public static ExtensionFileFilter gfxFilter = new ExtensionFileFilter(
			"Planning Domains (*.gfx)", new String("gfx"));
	

	/**
	 * Create an instance of HistoryWindow Allows the creation of specifications
	 * from Object Life Histories
	 * 
	 * @param title
	 *            String title
	 * @param curDomain
	 *            oclDomain
	 * @param parent
	 *            parent frame
	 */
	public HistoryWindow(String title, oclDomain curDomain, OclEd parent) {
		super(parent);
		setTitle(title);
		setClosable(false);
		this.curDomain = curDomain;
		top = parent;
		if (curDomain == null) {
			JOptionPane.showMessageDialog(top,
					"No Domain currently being edited.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		palletMgr = new PalletManager(this,parent);
		initComponents();
		pack();
		updateUI();
		setVisible(true);
		if (!curDomain.lifeHistoryFileName.equals("none")) {
			String fName = top.strDomainsDir+ File.separator + curDomain.lifeHistoryFileName;
			graphFile = new File(fName);
			if (graphFile != null && graphFile.exists() && graphFile.canRead()) {
				GraphModel model = null;
				try {
					DefaultGraphModelFileFormatXML writer = new DefaultGraphModelFileFormatXML(top);
					model = writer.read(graphFile, (LHGraph) graph);
					graph.clearSelection();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}

	/**
	 * initComponents set-up the window components
	 */
	private void initComponents() {
		// Use Border Layout
		getContentPane().setLayout(new BorderLayout());
		LHMarqueeHandler mh = new LHMarqueeHandler();
		LHMarqueeHandler dsmh = new LHMarqueeHandler();
		// Construct the Graph
		graph = (JGraph) new LHGraph((GraphModel) new LifeHistoryModel(),
				((BasicMarqueeHandler) mh));
		dsGraph = (JGraph) new LHGraph((GraphModel) new LifeHistoryModel(),
				((BasicMarqueeHandler) dsmh));
		// Construct Command History
		//
		// Create a GraphUndoManager which also Updates the ToolBar
		undoManager = new GraphUndoManager() {
			// Override Superclass
			public void undoableEditHappened(UndoableEditEvent e) {
				// First Invoke Superclass
				super.undoableEditHappened(e);
				// Then Update Undo/Redo Buttons
				updateHistoryButtons();
				// Set dirty status
				dirty = true;
			}
		};
		// Add Listeners to Graph
		//
		// Register UndoManager with the Model
		graph.getModel().addUndoableEditListener(undoManager);
		// Update ToolBar based on Selection Changes
		graph.getSelectionModel().addGraphSelectionListener(this);
		dsGraph.getSelectionModel().addGraphSelectionListener(this);
		// Listen for Delete Keystroke when the Graph has Focus
		graph.addKeyListener(this);
		dsGraph.addKeyListener(this);
		// Construct Panel
		//
		// Add a ToolBar
		getContentPane().add(createToolBar(), BorderLayout.NORTH);
		JTabbedPane palletTab = palletMgr.initPalletTab();
		JTabbedPane graphTab = new JTabbedPane();
		graphTab.addTab("Life History",null,new JScrollPane(graph),"Main editor panel for life histories");
		graphTab.addTab("Data Structures",null,new JScrollPane(dsGraph),"Editor panel to edit static data structures");
		graphTab.setEnabledAt(1,false);
		JSplitPane contentArea = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,palletTab,graphTab);
		// Add the Graph as Center Component
		getContentPane().add(contentArea, BorderLayout.CENTER);
		
	}
	/**
	 * getGraph
	 * @return - the current graph being edited
	 */
	public JGraph getGraph(){
		return graph;
	}
	
	
	/**
	 * @return Returns the editMode.
	 */
	public int getEditMode() {
		return editMode;
	}
	/**
	 * @param editMode The editMode to set.
	 */
	public void setEditMode(int editMode) {
		this.editMode = editMode;
	}
	// Insert a new Edge between source and target
	public void connect(Port source, Port target) {
		if (!legalConnection((DefaultPort) source, (DefaultPort) target,
				editMode)) {
			this.getToolkit().beep();
			graph.repaint();
			return;
		}
		// Connections that will be inserted into the Model
		ConnectionSet cs = new ConnectionSet();
		// Construct Edge with no label
		DefaultEdge edge = null;
		Map map = null;
		if (editMode == INSERTONEWAYMERGELINK) {
			LHUserObject mergeInfo = new LHUserObject();
			mergeInfo.putProperty("label", "");
			edge = (DefaultEdge) new OneWayMergeEdge(mergeInfo);
			// Create Connection between source and target using edge
			cs.connect(edge, source, target);
			// Create a Map thath holds the attributes for the edge
			map = new Hashtable();
			// Add a Line End Attribute
			GraphConstants.setLineEnd(map, GraphConstants.ARROW_CLASSIC);
			// Set colour
			GraphConstants.setLineColor(map, Color.RED);
			// Add a label along edge attribute
			GraphConstants.setLabelAlongEdge(map, true);
			mergeActions((DefaultPort) source, (DefaultPort) target);
		} else if (editMode == INSERTTWOWAYMERGELINK) {
			LHUserObject mergeInfo = new LHUserObject();
			mergeInfo.putProperty("label", "");
			edge = (DefaultEdge) new TwoWayMergeEdge(mergeInfo);
			// Create Connection between source and target using edge
			cs.connect(edge, source, target);
			// Create a Map thath holds the attributes for the edge
			map = new Hashtable();
			// Add a Line End Attribute
			GraphConstants.setLineEnd(map, GraphConstants.ARROW_CLASSIC);
			GraphConstants.setLineBegin(map, GraphConstants.ARROW_CIRCLE);
			// Set colour
			GraphConstants.setLineColor(map, Color.RED);
			// Add a label along edge attribute
			GraphConstants.setLabelAlongEdge(map, true);
			mergeActions((DefaultPort) source, (DefaultPort) target);
		} else if (editMode == INSERTPROCESSLINK) {
			LHUserObject procInfo = new LHUserObject();
			procInfo.putProperty("label", "");
			edge = (DefaultEdge) new ProcessEdge(procInfo);
			// Create Connection between source and target using edge
			cs.connect(edge, source, target);
			// Create a Map thah holds the attributes for the edge
			map = new Hashtable();
			// Add a Line End Attribute
			GraphConstants.setLineEnd(map, GraphConstants.ARROW_CLASSIC);
			// Set colour
			GraphConstants.setLineColor(map, Color.BLUE);
			// Add a label along edge attribute
			GraphConstants.setLabelAlongEdge(map, true);
		} else {
			// Create Connection between source and target using edge
			edge = (DefaultEdge) new TransitionEdge();
			cs.connect(edge, source, target);
			// Create a Map thath holds the attributes for the edge
			map = new Hashtable();
			// Add a Line End Attribute
			GraphConstants.setLineEnd(map, GraphConstants.ARROW_SIMPLE);
			// Add a label along edge attribute
			GraphConstants.setLabelAlongEdge(map, false);
		}
		// Construct a Map from cells to Maps (for insert)
		Hashtable attributes = new Hashtable();
		// Associate the Edge with its Attributes
		attributes.put(edge, map);
		// Insert the Edge and its Attributes
		graph.getGraphLayoutCache().insert(new Object[] { edge }, attributes,
				cs, null, null);
	}

	/**
	 * legalConnection ensure that connections do not break ocl rules
	 * 
	 * @param source
	 * @param target
	 * @param editMode
	 * @return
	 */
	private boolean legalConnection(DefaultPort source, DefaultPort target,
			int editMode) {
		// TODO look for multiple connections between nodes and validate
		if (editMode == INSERTONEWAYMERGELINK) {
			if (target.getParent() instanceof StateCell) {
				return false;
			}
		} else if (editMode == INSERTTWOWAYMERGELINK) {
			if (source.getParent() instanceof StateCell
					|| target.getParent() instanceof StateCell) {
				return false;
			}
		} else if (editMode == INSERTPROCESSLINK) {
			if (source.getParent() instanceof ProcessCell
					&& target.getParent() instanceof ProcessCell) {
				return false;
			}
			if (source.getParent() instanceof ProcessCell
					&& target.getParent() instanceof StateCell) {
				return false;
			}
			if (!(source.getParent() instanceof ProcessCell)
					&& !(target.getParent() instanceof ProcessCell)) {
				return false;
			}
		} else if (!((source.getParent() instanceof StateCell && target
				.getParent() instanceof TransitionCell) || (target.getParent() instanceof StateCell && source
				.getParent() instanceof TransitionCell))) {
			return false;
		}
		return true;
	}

	/**
	 * mergeAction if this is a merge between two transitions ensure that they
	 * have the same label name. Propigate from source to target
	 * 
	 * @param source
	 *            port of source node
	 * @param target
	 *            port of target node
	 */
	public void mergeActions(DefaultPort source, DefaultPort target) {
		Object sourceNode = source.getParent();
		Object targetNode = target.getParent();
		if ((TransitionCell.isValueTransitionCell(sourceNode) && TransitionCell.isValueTransitionCell(targetNode))
				|| (TransitionCell.isStateTransitionCell(sourceNode) && TransitionCell.isStateTransitionCell(targetNode))
				|| (TransitionCell.isValueTransitionCell(sourceNode) && TransitionCell.isStateTransitionCell(targetNode))
				|| (TransitionCell.isStateTransitionCell(sourceNode) && TransitionCell.isValueTransitionCell(targetNode))) {
			LHUserObject sourceObj = (LHUserObject) ((DefaultGraphCell) sourceNode)
					.getUserObject();
			LHUserObject targetObj = (LHUserObject) ((DefaultGraphCell) targetNode)
					.getUserObject();
			String sourceName = (String) sourceObj.getProperty("label");
			String targetName = (String) targetObj.getProperty("label");
			if (!targetName.equals(sourceName)) {
				targetObj.putProperty("label", sourceName);
				// TODO should follow merge links to see if this impacts on
				// existing merges
			}
		}
	}

	/**
	 * insert insert a vertex - state or transition node
	 * 
	 * @param point
	 * @param NodeType
	 */
	public void insert(Point2D point, int NodeType) {
		// Construct Vertex with no Label
		DefaultGraphCell vertex = createDefaultGraphCell(NodeType);
		if (vertex == null)
			return; // No vertex created
		// Add one Floating Port
		if (NodeType != TEXTNODE) {
			vertex.add(new DefaultPort());
		}
		// Create a Map that holds the attributes for the Vertex
		Map map = createCellAttributes(point, NodeType);
		// Construct a Map from cells to Maps (for insert)
		Map attributes = new Hashtable();
		// Associate the Vertex with its Attributes
		attributes.put(vertex, map);
		// Insert the Vertex and its Attributes (can also use model)
		graph.getGraphLayoutCache().insert(new Object[] { vertex }, attributes,
				null, null, null);
	}

	/**
	 * createCellAttributes Simple vertex nodes differentiated by type shown by
	 * different colours at the moment
	 * 
	 * @param point
	 * @param NodeType
	 * @return
	 */
	protected Map createCellAttributes(Point2D point, int NodeType) {
		Map map = new Hashtable();
		// Snap the Point to the Grid
		point = graph.snap((Point2D) point.clone());
		// Add a Bounds Attribute to the Map
		GraphConstants.setBounds(map, new Rectangle2D.Double(point.getX(),
				point.getY(), 0, 0));
		// Make sure the cell is resized on insert
		GraphConstants.setResize(map, true);
		// Add a nice looking gradient background
		if (NodeType == STATENODE) {
			if (iconFile != null) {
				ImageIcon icon = null;
				try {
					icon = new ImageIcon(iconFile.toURL());
				} catch (MalformedURLException e) {}
				GraphConstants.setIcon(map, icon);
			} else {
				GraphConstants.setGradientColor(map, Color.blue);
			}
		} else if (NodeType == TEXTNODE) {
//			 Add a Border Color Attribute to the Map
			//GraphConstants.setBorderColor(map, Color.white);
			// Add a White Background
			GraphConstants.setBackground(map, Color.white);
			GraphConstants.setAutoSize(map,true);
			// Make Vertex Opaque
			GraphConstants.setOpaque(map, false);
			return map;
		} else if(NodeType == PROCESSNODE) {
			GraphConstants.setIcon(map, procIcon);
			GraphConstants.setGradientColor(map,Color.PINK);
			GraphConstants.setBorder(map,new BevelBorder(BevelBorder.RAISED));
		} else {
			GraphConstants.setGradientColor(map, Color.green);
		}
		// Add a Border Color Attribute to the Map
		GraphConstants.setBorderColor(map, Color.black);
		// Add a White Background
		GraphConstants.setBackground(map, Color.white);
		// Make Vertex Opaque
		GraphConstants.setOpaque(map, true);
		return map;
	}

	/**
	 * createDefaultGraphCell
	 * 
	 * @param NodeType
	 *            the type State or Transition
	 * @return the node
	 */
	protected DefaultGraphCell createDefaultGraphCell(int NodeType) {
		if (editMode == INSERTSTATE) {
			if (!"none".equals((String) cmbSortName.getSelectedItem())) {
				LHUserObject stateInfo = new LHUserObject("state"
						+ stateCount++);
				stateInfo.putProperty("ObjectSort", (String) cmbSortName
						.getSelectedItem());
				copyStateProperties(stateInfo,(String) cmbSortName.getSelectedItem());
				return (DefaultGraphCell) new StateCell(stateInfo);
			} else {
				JOptionPane.showMessageDialog(this,
						"You must provide an Object Type.", "GIPO ALERT",
						JOptionPane.WARNING_MESSAGE, null);
				return null;
			}
		} else if (editMode == INSERTPROCESS) {
			LHUserObject processInfo = new LHUserObject("process"
					+ processCount++);
			return (DefaultGraphCell) new ProcessCell(processInfo);
		} else if (editMode == INSERTTEXT) {
			LHUserObject info = new LHUserObject("<html><body>[Edit Me]</body></html>");
			return (DefaultGraphCell) new TextNote(info);
		} else {
			LHUserObject tInfo = new LHUserObject("trans" + transCount++);
			tInfo.putProperty("ObjectSort", (String) cmbSortName
					.getSelectedItem());
			TransitionCell tCell = new TransitionCell(tInfo,TransitionCell.STATE_TRANSITION);
			return (DefaultGraphCell) tCell;
		}
	}
	
	/**
	 * copyStateProperties
	 * used to copy properties of any new state node from exitsing node.
	 * @param userInfo - the new user object
	 * @param sort - the object sort
	 */
	private void copyStateProperties(LHUserObject userInfo,String sort) {
		boolean found = false;
		Object[] roots = graph.getRoots();
		for(int i = 0; !found && i < roots.length; i++){
			DefaultGraphCell cell = (DefaultGraphCell)roots[i];
			if (cell instanceof StateCell){
				if (((StateCell)cell).getObjectSort().equals(sort)) {
					// Copy existing state properties
					StateCell cur = (StateCell)cell;
					LHUserObject curUser = (LHUserObject)cur.getUserObject();
					Set keys = curUser.properties.keySet();
					Iterator it = keys.iterator();
					while (it.hasNext()){
						String key = (String)it.next();
						if (key.startsWith("Property")) {
							userInfo.putProperty(key,curUser.getProperty(key));
						}
					}
					found = true;
				}
			}
		}
	}
	
	//	 Create a PackageNode that Contains the Cells
	public void packageNodes(Object[] cells) {
		// Order Cells by Model Layering
		cells = graph.order(cells);
		// If Any Cells in View
		if (cells != null && cells.length > 0) {
			// Get name and icon from user
			String libraryPath =
				top.strDomainsDir
					+ File.separator
					+ "icons";
			Icon packageIcon = null;
			JFileChooser chooser = new JFileChooser(libraryPath);
			URL packageUrl = HistoryWindow.class.getResource("resources/package.png");
			packageIcon = new ImageIcon(packageUrl);
			FileChooserPanel fcPan = new FileChooserPanel(packageIcon);
			chooser.setAccessory(fcPan);
			// Ok, set up our own file view for the chooser
			chooser.setFileView(new PNGThumbNailViewer(this));
			int option =
				chooser.showDialog(this, "Package Details");
			if (option == JFileChooser.APPROVE_OPTION) {
				File iconFile = chooser.getSelectedFile();
				if (iconFile != null) {
					ImageIcon icon = null;
					try {
						packageIcon = new ImageIcon(iconFile.toURL());
					} catch (MalformedURLException e) {
					}
				}
			} else {
				// Must have done a cancel
				return;
			}
			// Create Group Cell
			PackageCell packCell = new PackageCell(new LHUserObject(fcPan.jtxtPackageName.getText()));
			Map attr = new Hashtable();
			GraphConstants.setIcon(attr,packageIcon);
			// Add a White Background
			GraphConstants.setBackground(attr, Color.white);
			// Make Vertex Opaque
			GraphConstants.setOpaque(attr, true);
			// Set Location
			Point2D point = getPackageCoordinates(cells);
			point = graph.snap((Point2D) point.clone());
			// Add a Bounds Attribute to the Map
			GraphConstants.setBounds(attr, new Rectangle2D.Double(point.getX(),
					point.getY(), 0, 0));
//			 Make sure the cell is resized on insert
			GraphConstants.setResize(attr, true);
			GraphConstants.setBorderColor(attr, Color.black);
			GraphConstants.setGradientColor(attr,Color.GRAY);
			Map attributes = new Hashtable();
			// Associate the Vertex with its Attributes
			attributes.put(packCell, attr);
			// Insert into model
			// Give the packCell node a port
			DefaultPort port = new DefaultPort("Floating");
			packCell.add(port);
			ConnectionSet cs = redirectEdges(packCell,cells,port);
			PackageWriter pw = new PackageWriter(top);
			String strPackage = pw.toString(cells,(LHGraph)graph);
			System.out.println(strPackage);
			packCell.setPrivateNodes(strPackage);
			graph.getGraphLayoutCache().insert(new Object[] { packCell }, attributes,
					cs,null, null);
			graph.getGraphLayoutCache().remove(cells);
		}
	}
	
	private Point2D getPackageCoordinates(Object[] cells){
		double x = 0.0;
		double y = 0.0;
		int count = 0;
		for (int i = 0;i < cells.length; i++){
			if (cells[i] instanceof StateCell) {
				Rectangle2D bounds = GraphConstants.getBounds(((StateCell)cells[i]).getAttributes());
				x = x + bounds.getX();
				y = y + bounds.getY();
				count++;
			} else if (cells[i] instanceof TransitionCell){
				Rectangle2D bounds = GraphConstants.getBounds(((TransitionCell)cells[i]).getAttributes());
				x = x + bounds.getX();
				y = y + bounds.getY();
				count++;
			}
		}
		x = x/count;
		y = y/count;
		return new Point2D.Double(x,y);
	}
	
	// TODO Working on this
	// Need to iterate over all graph or inspect edges comming into Cells
	private ConnectionSet redirectEdges(PackageCell packCell,Object[] cells, DefaultPort port){
		int internalNodesID = 1; // Used to count package internal nodes
		ConnectionSet cs = new ConnectionSet();
		Object[] roots = graph.getRoots();
		for(int i = 0; i < roots.length; i++){
			DefaultGraphCell cell = (DefaultGraphCell)roots[i];
			if (cell instanceof DefaultEdge){
				DefaultGraphCell target = (DefaultGraphCell)((DefaultPort)((DefaultEdge)cell).getTarget()).getParent();
				DefaultGraphCell source = (DefaultGraphCell)((DefaultPort)((DefaultEdge)cell).getSource()).getParent();
				int dir = 0;
				if ((dir = externalLink(cells,source,target)) != 0) {
					int nodeID = -1;
					if (dir == 1) {
						((DefaultEdge)cell).setSource(port);
						cs.connect(cell,port,((DefaultEdge)cell).getTarget());
						if (source instanceof StateCell) {
							nodeID = ((StateCell)source).getPackageID();
							if (nodeID == -1) {
								((StateCell)source).setPackageID(internalNodesID);
								nodeID = internalNodesID++;
							}
						} else if (source instanceof TransitionCell) {
							nodeID = ((TransitionCell)source).getPackageID();
							if (nodeID == -1) {
								((TransitionCell)source).setPackageID(internalNodesID);
								nodeID = internalNodesID++;
							}
						}
					} else {
						((DefaultEdge)cell).setTarget(port);
						cs.connect(cell,((DefaultEdge)cell).getSource(),port);
						if (target instanceof StateCell) {
							nodeID = ((StateCell)target).getPackageID();
							if (nodeID == -1) {
								((StateCell)target).setPackageID(internalNodesID);
								nodeID = internalNodesID++;
							}
						} else if (target instanceof TransitionCell) {
							nodeID = ((TransitionCell)target).getPackageID();
							if (nodeID == -1){
								((TransitionCell)target).setPackageID(internalNodesID);
								nodeID = internalNodesID++;
							}
						}
					}
					int oldEdgeID = -1;
					if (cell instanceof MergeEdge) {
						oldEdgeID = ((MergeEdge)cell).getPackageID();
						((MergeEdge)cell).setPackageID(nodeID);
					} else if (cell instanceof TransitionEdge) {
						oldEdgeID = ((TransitionEdge)cell).getPackageID();
						((TransitionEdge)cell).setPackageID(nodeID);
					}
					packCell.addRedirectIDs(nodeID,oldEdgeID);
				}
			}
		}
		return cs;
	}
	private void redirectPackageEdges(PackageCell selPackage,Object[] incells){
		DefaultPort port = (DefaultPort)selPackage.getChildAt(0);
		if (port != null){
			Iterator li = port.edges();	
			while (li.hasNext()) {
				DefaultEdge edge = (DefaultEdge)li.next();
				int countID = -1;
				if (edge instanceof TransitionEdge) {
					countID = ((TransitionEdge)edge).getPackageID();
					int oldID = selPackage.getOldEdgeValue(countID);
					((TransitionEdge)edge).setPackageID(oldID);
				} else if (edge instanceof MergeEdge) {
					countID = ((MergeEdge)edge).getPackageID();
					int oldID = selPackage.getOldEdgeValue(countID);
					((MergeEdge)edge).setPackageID(oldID);
				}
				boolean found = false;
				int i = 0;
				while (!found && i < incells.length) {
					if (incells[i] instanceof StateCell){
						DefaultPort oldPort = (DefaultPort)((DefaultGraphCell)incells[i]).getChildAt(0);
						int cellCount = ((StateCell)incells[i]).getPackageID();
						if (cellCount == countID) {
							found = true;
							if (port.equals(edge.getSource())){
								edge.setSource(oldPort);
							} else {
								edge.setTarget(oldPort);
							}
						}
					} else if (incells[i] instanceof TransitionCell) {
						DefaultPort oldPort = (DefaultPort)((DefaultGraphCell)incells[i]).getChildAt(0);
						int cellCount = ((TransitionCell)incells[i]).getPackageID();
						if (cellCount == countID) {
							found = true;
							if (port.equals(edge.getSource())){
								edge.setSource(oldPort);
							} else {
								edge.setTarget(oldPort);
							}
						}
					}
					i++;
				}
			}
		}
		// Reset internal Node counts
		for (int i = 0; i < incells.length;i++) {
			if (incells[i] instanceof StateCell){
				((StateCell)incells[i]).setPackageID(-1);
			} else if (incells[i] instanceof TransitionCell) {
				((TransitionCell)incells[i]).setPackageID(-1);
			}
		}
	}
	
	private int externalLink(Object[] cells,DefaultGraphCell source,DefaultGraphCell target){
		int ret = 0;
		boolean foundTarget = false;
		boolean foundSource = false;
		for(int i = 0; i < cells.length;i++) {
			DefaultGraphCell cell = (DefaultGraphCell)cells[i];
			if (target.equals(cell))
				foundTarget = true;
			if (source.equals(cell))
				foundSource = true;
		}
		if( foundSource && !foundTarget) {
			ret = 1;
		}
		if (!foundSource && foundTarget) {
			ret = 2;
		}
		return ret;
	}
	//	 Unpackage the Cells and Select the Children
	public void openPackage(Object[] cells) {
		// Check selected cells appropriate
		if (cells.length != 1) {
			JOptionPane.showMessageDialog(top,
					"You Must select a single Package Node to Unpack.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (!(cells[0] instanceof PackageCell)) {
			JOptionPane.showMessageDialog(top,
					"You Must select a single Package Node to Unpack.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		PackageCell selPackage = (PackageCell)cells[0];
		String xmlDesc = selPackage.getPrivateNodes();
		PackageWriter reader = new PackageWriter(top);
		LHGraph tempGraph = new LHGraph(new DefaultGraphModel());
		try {
			reader.read(xmlDesc,tempGraph);
		} catch (Exception e){
			System.out.println("Package instantiation error");
		}
		setEditMode(HistoryWindow.SELECT);
		if (tempGraph == null) {
			// must be error - just give up error message already displayed
			return;
		}
		GraphModel model = tempGraph.getModel();
		// Order Cells by Model Layering
		Object[] incells =
			DefaultGraphModel.order(
				model,
				DefaultGraphModel.getAll((GraphModel) model));
		if (incells != null && incells.length > 0) {
			// Create Group Cell
			DefaultGraphCell group =
				new DefaultGraphCell(
					new String("temp"));
			Rectangle2D bounds = tempGraph.getCellBounds(incells);
			if (bounds != null) {
				bounds =
					new Rectangle2D.Double(
						bounds.getX() + bounds.getWidth() / 4,
						bounds.getY() + bounds.getHeight() / 4,
						bounds.getWidth() / 2,
						bounds.getHeight() / 2);
				GraphConstants.setBounds(group.getAttributes(), bounds);
			}
			// Insert into model
			redirectPackageEdges(selPackage,incells);
			graph.getGraphLayoutCache().insertGroup(group, incells);
			graph.getGraphLayoutCache().remove(new Object[]{selPackage});
		}
	}

	// Create a Group that Contains the Cells
	public void group(Object[] cells) {
		// Order Cells by Model Layering
		cells = graph.order(cells);
		// If Any Cells in View
		if (cells != null && cells.length > 0) {
			// Create Group Cell
			int count = getCellCount(graph);
			DefaultGraphCell group = new DefaultGraphCell(
					new Integer(count - 1));
			// Create Change Information
			ParentMap map = new ParentMap();
			// Insert Child Parent Entries
			for (int i = 0; i < cells.length; i++)
				map.addEntry(cells[i], group);
			// Insert into model
			graph.getGraphLayoutCache().insert(new Object[] { group }, null,
					null, map, null);
		}
	}

	// Returns the total number of cells in a graph
	protected int getCellCount(JGraph graph) {
		Object[] cells = graph.getDescendants(graph.getRoots());
		return cells.length;
	}

	// Ungroup the Groups in Cells and Select the Children
	public void ungroup(Object[] cells) {
		// If any Cells
		if (cells != null && cells.length > 0) {
			// List that Holds the Groups
			ArrayList groups = new ArrayList();
			// List that Holds the Children
			ArrayList children = new ArrayList();
			// Loop Cells
			for (int i = 0; i < cells.length; i++) {
				// If Cell is a Group
				if (isGroup(cells[i])) {
					// Add to List of Groups
					groups.add(cells[i]);
					// Loop Children of Cell
					for (int j = 0; j < graph.getModel()
							.getChildCount(cells[i]); j++) {
						// Get Child from Model
						Object child = graph.getModel().getChild(cells[i], j);
						// If Not Port
						if (!(child instanceof Port))
							// Add to Children List
							children.add(child);
					}
				}
			}
			// Remove Groups from Model (Without Children)
			graph.getGraphLayoutCache().remove(groups.toArray());
			// Select Children
			graph.setSelectionCells(children.toArray());
		}
	}

	// Determines if a Cell is a Group
	public boolean isGroup(Object cell) {
		// Map the Cell to its View
		CellView view = graph.getGraphLayoutCache().getMapping(cell, false);
		if (view != null)
			return !view.isLeaf();
		return false;
	}

	// Brings the Specified Cells to Front
	public void toFront(Object[] c) {
		graph.getGraphLayoutCache().toFront(c);
	}

	// Sends the Specified Cells to Back
	public void toBack(Object[] c) {
		graph.getGraphLayoutCache().toBack(c);
	}

	// Undo the last Change to the Model or the View
	public void undo() {
		try {
			undoManager.undo(graph.getGraphLayoutCache());
		} catch (Exception ex) {
			System.err.println(ex);
		} finally {
			updateHistoryButtons();
		}
	}

	// Redo the last Change to the Model or the View
	public void redo() {
		try {
			undoManager.redo(graph.getGraphLayoutCache());
		} catch (Exception ex) {
			System.err.println(ex);
		} finally {
			updateHistoryButtons();
		}
	}

	// Update Undo/Redo Button State based on Undo Manager
	protected void updateHistoryButtons() {
		// The View Argument Defines the Context
		undo.setEnabled(undoManager.canUndo(graph.getGraphLayoutCache()));
		redo.setEnabled(undoManager.canRedo(graph.getGraphLayoutCache()));
	}

	//
	// Listeners
	//
	// From GraphSelectionListener Interface
	public void valueChanged(GraphSelectionEvent e) {
		// Group Button only Enabled if more than One Cell Selected
		group.setEnabled(graph.getSelectionCount() > 1);
		// Update Button States based on Current Selection
		boolean enabled = !graph.isSelectionEmpty();
		remove.setEnabled(enabled);
		ungroup.setEnabled(enabled);
		tofront.setEnabled(enabled);
		toback.setEnabled(enabled);
		copy.setEnabled(enabled);
		cut.setEnabled(enabled);
	}

	//
	// KeyListener for Delete KeyStroke
	//
	public void keyReleased(KeyEvent e) {
	}

	public void keyTyped(KeyEvent e) {
	}

	public void keyPressed(KeyEvent e) {
		// Listen for Delete Key Press
		if (e.getKeyCode() == KeyEvent.VK_DELETE)
			// Execute Remove Action on Delete Key Press
			remove.actionPerformed(null);
	}

	//
	// Custom Model
	//
	// A Custom Model that does not allow Self-References
	public static class LifeHistoryModel extends DefaultGraphModel {
		// Override Superclass Method
		public boolean acceptsSource(Object edge, Object port) {
			// Source only Valid if not Equal Target
			return (((Edge) edge).getTarget() != port);
		}

		// Override Superclass Method
		public boolean acceptsTarget(Object edge, Object port) {
			// Target only Valid if not Equal Source
			return (((Edge) edge).getSource() != port);
		}

		public int countMergesTo(TransitionCell node) {
			int ret = 0;
			Enumeration enum = node.children();
			while (enum.hasMoreElements()) {
				DefaultPort port = (DefaultPort) enum.nextElement();
				Iterator li = port.edges();
				while (li.hasNext()) {
					DefaultEdge edge = (DefaultEdge) li.next();
					if (edge instanceof MergeEdge) {
						DefaultGraphCell connTrans = (DefaultGraphCell) ((DefaultPort) edge
								.getTarget()).getParent();
						if (connTrans == node) {
							//This is an incomming merge edge
							ret++;
						}
					}
				}
			}
			return ret;
		}
	}

	public class LHMarqueeHandler extends BasicMarqueeHandler {
		// Holds the Start and the Current Point
		protected Point2D start, current;

		// Holds the First and the Current Port
		protected PortView port, firstPort;

		// Override to Gain Control (for PopupMenu and ConnectMode)
		public boolean isForceMarqueeEvent(MouseEvent e) {
			if (e.isShiftDown())
				return false;
			// If Right Mouse Button we want to Display the PopupMenu
			if (SwingUtilities.isRightMouseButton(e))
				// Return Immediately
				return true;
			// Find and Remember Port
			port = getSourcePortAt(e.getPoint());
			// If Port Found and in ConnectMode (=Ports Visible)
			if (port != null && graph.isPortsVisible())
				return true;
			// Else Call Superclass
			return super.isForceMarqueeEvent(e);
		}

		// Display PopupMenu or Remember Start Location and First Port
		public void mousePressed(final MouseEvent e) {
			// If Right Mouse Button
			if (SwingUtilities.isRightMouseButton(e)) {
				// Find Cell in Model Coordinates
				// Create PopupMenu for the Cell
				Object cell = graph.getFirstCellForLocation(e.getX(), e.getY());
				if (cell instanceof TransitionCell || cell instanceof StateCell ||
						cell instanceof MergeEdge || cell instanceof TextNote ||
						cell instanceof ProcessCell) {
					JPopupMenu menu = createPopupMenu(e.getPoint(), cell);
					// Display PopupMenu
					menu.show(graph, e.getX(), e.getY());
				} else if (cell instanceof ProcessEdge) {
					DefaultGraphCell source = ((ProcessEdge)cell).getEdgeSource();
					if (source instanceof TransitionCell) {
						JPopupMenu menu = createPopupMenu(e.getPoint(), cell);
						// Display PopupMenu
						menu.show(graph, e.getX(), e.getY());
					}
				}
				// Else if in ConnectMode and Remembered Port is Valid
			} else if (editMode == INSERTSTATE) {
				insert(e.getPoint(), STATENODE);
			} else if (editMode == INSERTTEXT) {
				insert(e.getPoint(),TEXTNODE);
			} else if (editMode == INSERTTRANSITION) {
				insert(e.getPoint(), TRANSNODE);
			} else if (editMode == INSERTLIBRARYITEM) {
				editMode = SELECT;
				//palletMgr.insertLibraryItem(e.getPoint());
			} else if (editMode == INSERTPROCESS) {
				insert(e.getPoint(),PROCESSNODE);
			} else if (port != null && graph.isPortsVisible()) {
				// Remember Start Location
				start = graph.toScreen(port.getLocation(null));
				// Remember First Port
				firstPort = port;
			} else {
				// Call Superclass
				super.mousePressed(e);
			}
		}

		// Find Port under Mouse and Repaint Connector
		public void mouseDragged(MouseEvent e) {
			// If remembered Start Point is Valid
			if (start != null) {
				// Fetch Graphics from Graph
				Graphics g = graph.getGraphics();
				// Reset Remembered Port
				PortView newPort = getTargetPortAt(e.getPoint());
				// Do not flicker (repaint only on real changes)
				if (newPort == null || newPort != port) {
					// Xor-Paint the old Connector (Hide old Connector)
					paintConnector(Color.black, graph.getBackground(), g);
					// If Port was found then Point to Port Location
					port = newPort;
					if (port != null)
						current = graph.toScreen(port.getLocation(null));
					// Else If no Port was found then Point to Mouse Location
					else
						current = graph.snap(e.getPoint());
					// Xor-Paint the new Connector
					paintConnector(graph.getBackground(), Color.black, g);
				}
			}
			// Call Superclass
			super.mouseDragged(e);
		}

		public PortView getSourcePortAt(Point2D point) {
			// Disable jumping
			graph.setJumpToDefaultPort(false);
			PortView result;
			try {
				// Find a Port View in Model Coordinates and Remember
				result = graph.getPortViewAt(point.getX(), point.getY());
			} finally {
				graph.setJumpToDefaultPort(true);
			}
			return result;
		}

		// Find a Cell at point and Return its first Port as a PortView
		protected PortView getTargetPortAt(Point2D point) {
			// Find a Port View in Model Coordinates and Remember
			return graph.getPortViewAt(point.getX(), point.getY());
		}

		// Show Special Cursor if Over Port
		public void mouseMoved(MouseEvent e) {
			// Check Mode and Find Port
			if (e != null && getSourcePortAt(e.getPoint()) != null
					&& graph.isPortsVisible()) {
				// Set Cusor on Graph (Automatically Reset)
				graph.setCursor(new Cursor(Cursor.HAND_CURSOR));
				// Consume Event
				// Note: This is to signal the BasicGraphUI's
				// MouseHandle to stop further event processing.
				e.consume();
			} else
				// Call Superclass
				super.mouseMoved(e);
		}

		// Connect the First Port and the Current Port in the Graph or Repaint
		public void mouseReleased(MouseEvent e) {
			Object cell = graph.getFirstCellForLocation(e.getX(), e.getY());
			if (cell instanceof StateCell) {
				LHUserObject uObj = (LHUserObject) ((StateCell) cell)
						.getUserObject();
				String objSort = (String) uObj.getProperty("ObjectSort");
				if (!((String) cmbSortName.getSelectedItem()).equals(objSort))
					cmbSortName.setSelectedItem(objSort);
			}
			// If Valid Event, Current and First Port
			if (e != null && port != null && firstPort != null
					&& firstPort != port) {
				// Then Establish Connection
				connect((Port) firstPort.getCell(), (Port) port.getCell());
				// Else Repaint the Graph
			} else
				graph.repaint();
			// Reset Global Vars
			firstPort = port = null;
			start = current = null;
			if (e != null ){
				Utility.debugPrintln("patterns","NULL mouse event on release");
				// Call Superclass
				super.mouseReleased(e);
			}
		}

		// Use Xor-Mode on Graphics to Paint Connector
		protected void paintConnector(Color fg, Color bg, Graphics g) {
			// Set Foreground
			g.setColor(fg);
			// Set Xor-Mode Color
			g.setXORMode(bg);
			// Highlight the Current Port
			paintPort(graph.getGraphics());
			// If Valid First Port, Start and Current Point
			if (firstPort != null && start != null && current != null)
				// Then Draw A Line From Start to Current Point
				g.drawLine((int) start.getX(), (int) start.getY(),
						(int) current.getX(), (int) current.getY());
		}

		// Use the Preview Flag to Draw a Highlighted Port
		protected void paintPort(Graphics g) {
			// If Current Port is Valid
			if (port != null) {
				// If Not Floating Port...
				boolean o = (GraphConstants.getOffset(port.getAttributes()) != null);
				// ...Then use Parent's Bounds
				Rectangle2D r = (o) ? port.getBounds() : port.getParentView()
						.getBounds();
				// Scale from Model to Screen
				r = graph.toScreen((Rectangle2D) r.clone());
				// Add Space For the Highlight Border
				r.setFrame(r.getX() - 3, r.getY() - 3, r.getWidth() + 6, r
						.getHeight() + 6);
				// Paint Port in Preview (=Highlight) Mode
				graph.getUI().paintCell(g, port, r, true);
			}
		}

		//
		// PopupMenu
		//
		public JPopupMenu createPopupMenu(final Point pt, final Object cell) {
			JPopupMenu menu = new JPopupMenu();
			if (cell != null) {
				if (cell instanceof StateCell) {
					menu.add(new AbstractAction("Properties") {
						public void actionPerformed(ActionEvent e) {
							LHUserObject uObj = (LHUserObject) ((StateCell) cell)
							.getUserObject();
							String objSort = (String) uObj
							.getProperty("ObjectSort");
							if (!((String) cmbSortName.getSelectedItem()).equals(objSort))
								cmbSortName.setSelectedItem(objSort);
							new PropertyDialog(PropertyDialog.STATEPROPERTY,(LHGraph) graph,uObj, (StateCell)cell);
						}
					});
					if (curDomain.isOclPlus()) {
						menu.add(new AbstractAction("Fluents") {
							public void actionPerformed(ActionEvent e) {
								LHUserObject uObj = (LHUserObject) ((StateCell) cell)
								.getUserObject();
								String objSort = (String) uObj
								.getProperty("ObjectSort");
								if (!((String) cmbSortName.getSelectedItem()).equals(objSort))
									cmbSortName.setSelectedItem(objSort);
								new PropertyDialog(PropertyDialog.STATEFLUENT,(LHGraph) graph,uObj, (StateCell)cell);
							}
						});
						menu.add(new AbstractAction("Transforming Property") {
							public void actionPerformed(ActionEvent e) {
								LHUserObject uObj = (LHUserObject) ((StateCell) cell)
								.getUserObject();
								String objSort = (String) uObj
								.getProperty("ObjectSort");
								if (!((String) cmbSortName.getSelectedItem()).equals(objSort))
									cmbSortName.setSelectedItem(objSort);
								new PropertyDialog(PropertyDialog.TRANSFORMPROPERTY,(LHGraph) graph,uObj, (StateCell)cell);
							}
						});
					}
				} else if (cell instanceof TransitionCell) {
					menu.add(new AbstractAction("Property Change") {
						public void actionPerformed(ActionEvent e) {
							LHUserObject uObj = (LHUserObject) ((TransitionCell) cell)
							.getUserObject();
							String objSort = (String) uObj
							.getProperty("ObjectSort");
							if (!((String) cmbSortName.getSelectedItem()).equals(objSort))
								cmbSortName.setSelectedItem(objSort);
							new PropertyDialog(PropertyDialog.PROPERTYCHANGE,(LHGraph) graph,uObj, (TransitionCell)cell);
						}
					});
					if (curDomain.isOclPlus()) {
						menu.add(new AbstractAction("Fluent Constraint") {
							public void actionPerformed(ActionEvent e) {
								LHUserObject uObj = (LHUserObject) ((TransitionCell) cell)
								.getUserObject();
								String objSort = (String) uObj
								.getProperty("ObjectSort");
								if (!((String) cmbSortName.getSelectedItem()).equals(objSort))
									cmbSortName.setSelectedItem(objSort);
								new FluentDialog((LHGraph) graph,uObj, (TransitionCell)cell);
							}
						});
					}
					menu.add(new AbstractAction("Number Constraint") {
						public void actionPerformed(ActionEvent e) {
							LHUserObject uObj = (LHUserObject) ((TransitionCell) cell)
							.getUserObject();
							String objSort = (String) uObj
							.getProperty("ObjectSort");
							if (!((String) cmbSortName.getSelectedItem()).equals(objSort)) {
								cmbSortName.setSelectedItem(objSort);
							}
							new PropertyDialog(PropertyDialog.NUMBERCONSTRAINT,(LHGraph) graph,uObj, (TransitionCell)cell);
						}
					});
					if (((TransitionCell)cell).isDisjunction()) {
						menu.add(new AbstractAction("Remove Disjunction") {
							public void actionPerformed(ActionEvent e) {
								LHUserObject uObj = (LHUserObject) ((TransitionCell) cell)
								.getUserObject();
								uObj.removeDisjunction((LHGraph) graph, (TransitionCell)cell);
							}
						});
					} else {
						// Check that there is at least two incomming merges
						LifeHistoryModel curModel = (LifeHistoryModel) graph.getModel();
						if (curModel.countMergesTo((TransitionCell)cell) >= 2) {
							menu.add(new AbstractAction("Make Disjunction") {
								public void actionPerformed(ActionEvent e) {
									LHUserObject uObj = (LHUserObject) ((TransitionCell) cell)
									.getUserObject();
									uObj.makeDisjunction((LHGraph) graph, (TransitionCell)cell);
								}
							});
						}
					}
					if (curDomain.isOclPlus()){
						if (((TransitionCell)cell).isEvent()) {
							menu.add(new AbstractAction("Remove Event") {
								public void actionPerformed(ActionEvent e) {
									LHUserObject uObj = (LHUserObject) ((TransitionCell) cell)
									.getUserObject();
									uObj.removeEvent((LHGraph) graph, (TransitionCell)cell);
								}
							});
						} else {
							menu.add(new AbstractAction("Make Event") {
								public void actionPerformed(ActionEvent e) {
									LHUserObject uObj = (LHUserObject) ((TransitionCell) cell)
									.getUserObject();
									uObj.makeEvent((LHGraph) graph, (TransitionCell)cell);
								}
							});
						}
						
					}
				} else if (cell instanceof MergeEdge) {
					menu.add(new AbstractAction("Start Association") {
						public void actionPerformed(ActionEvent e) {
							manageAssociation("+",(MergeEdge)cell);
						}
					});
					menu.add(new AbstractAction("End Association") {
						public void actionPerformed(ActionEvent e) {
							manageAssociation("-",(MergeEdge)cell);
						}
					});
					menu.add(new AbstractAction("Remove Association") {
						public void actionPerformed(ActionEvent e) {
							manageAssociation("",(MergeEdge)cell);
						}
					});
					menu.add(new AbstractAction("Coordination Constraint") {
						public void actionPerformed(ActionEvent e) {
							LHUserObject uObj = (LHUserObject) ((MergeEdge) cell)
							.getUserObject();
							String objSort = (String) uObj
							.getProperty("ObjectSort");
							new PropertyDialog(PropertyDialog.COORDINATIONCONSTRAINT,(LHGraph) graph,uObj, (MergeEdge)cell);
						}
					});
				} else if (cell instanceof ProcessCell) {
					menu.add(new AbstractAction("Fluent Constraint")  {
						public void actionPerformed(ActionEvent e) {
							LHUserObject uObj = (LHUserObject) ((ProcessCell) cell)
							.getUserObject();
							new FluentDialog((LHGraph) graph,uObj, (ProcessCell)cell);
						}
					});
				} else if (cell instanceof TextNote) {
					menu.add(new AbstractAction("Edit") {
						public void actionPerformed(ActionEvent e) {
							((TextNote)cell).showTextDialog((LHGraph)graph);
						}
					});
				} else if (cell instanceof ProcessEdge) {
					menu.add(new AbstractAction("Property Precondition") {
						public void actionPerformed(ActionEvent e) {
							LHUserObject uObj = (LHUserObject) ((ProcessEdge) cell)
							.getUserObject();
							new PropertyDialog(PropertyDialog.PROCESSPRECONDITION,(LHGraph) graph, uObj, (ProcessEdge)cell);
						}
					});
				}
				// Edit
				menu.addSeparator();
				menu.add(new AbstractAction("View") {
					public void actionPerformed(ActionEvent e) {
						if (cell instanceof StateCell) {
							LHUserObject uObj = (LHUserObject) ((StateCell) cell)
									.getUserObject();
							String objSort = (String) uObj
									.getProperty("ObjectSort");
							if (!((String) cmbSortName.getSelectedItem())
									.equals(objSort))
								cmbSortName.setSelectedItem(objSort);
							uObj.propertyViewer((LHGraph) graph, cell);
						} else if (cell instanceof TextNote) {
							((TextNote)cell).showTextDialog((LHGraph)graph);
						} else if (cell instanceof OneWayMergeEdge) {
							LHUserObject uObj = (LHUserObject) ((OneWayMergeEdge) cell)
									.getUserObject();
							uObj.showPropertyDialog((LHGraph) graph, cell);
						} else if (cell instanceof TwoWayMergeEdge) {
							LHUserObject uObj = (LHUserObject) ((TwoWayMergeEdge) cell)
									.getUserObject();
							uObj.showPropertyDialog((LHGraph) graph, cell);
						} else if (cell instanceof TransitionCell) {
							LHUserObject uObj = (LHUserObject) ((TransitionCell) cell)
									.getUserObject();
							uObj.propertyViewer((LHGraph) graph, cell);
						} else if (cell instanceof ProcessCell) {
							LHUserObject uObj = (LHUserObject) ((ProcessCell) cell)
							.getUserObject();
							uObj.showPropertyDialog((LHGraph) graph, cell);
						}else {
							// dont allow editing
							//graph.startEditingAtCell(cell);
						}
					}
				});
			}
			return menu;
		}
	} // End of Editor.LHMarqueeHandler

	//
	//
	//
	//
	// PopupMenu and ToolBar
	//
	//
	//
	//
	/**
	 * createToolBar create the multi line LifeHistory Toolbar
	 * 
	 * @return the compound toolbar
	 */
	public JPanel createToolBar() {
		JPanel firstBar = new JPanel(new BorderLayout());
		firstBar.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		firstBar.add(createEditBar(), BorderLayout.NORTH);
		//JPanel midBar = new JPanel(new BorderLayout());
		//midBar.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		//midBar.add(firstBar, BorderLayout.CENTER);
		//midBar.add(createConnectionTypeBar(), BorderLayout.NORTH);
		JPanel botBar = new JPanel(new BorderLayout());
		//botBar.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		botBar.add(createObjectTypeBar(), BorderLayout.NORTH);
		botBar.add(firstBar, BorderLayout.CENTER);
		return botBar;
	}

	/**
	 * createObjectTypeBar create the toolbar to control mode of editor which
	 * selects role of editor and object sort being edited
	 * 
	 * @return the tool bar
	 */
	private JToolBar createObjectTypeBar() {
		JToolBar toolbar = new JToolBar();
		toolbar.setMargin(new Insets(-3, 1, -3, 1));
		toolbar.setFloatable(false);
		// Open Graphics File
		URL openURL = HistoryWindow.class.getResource("resources/open.gif");
		ImageIcon openIcon = new ImageIcon(openURL);
		JButton cmdOpen = new JButton(openIcon);
		cmdOpen.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				GraphModel model = loadGraphFile();
			}
		});
		cmdOpen.setToolTipText("Open State Graphics");
		toolbar.add(cmdOpen);
		
		// Save Graphics File
		URL saveURL = HistoryWindow.class.getResource("resources/save.gif");
		ImageIcon saveIcon = new ImageIcon(saveURL);
		JButton cmdSave = new JButton(saveIcon);
		cmdSave.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				saveAsFile();
			}
		});
		cmdSave.setToolTipText("Save State Graphics");
		toolbar.add(cmdSave);
		URL saveLibURL = HistoryWindow.class.getResource("resources/saveLib.png");
		ImageIcon saveLibIcon = new ImageIcon(saveLibURL);
		JButton cmdSaveLib = new JButton(saveLibIcon);
		cmdSaveLib.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				palletMgr.saveLibraryAsFile();
			}
		});
		cmdSaveLib.setToolTipText("Save Graphics as Library Item");
		toolbar.add(cmdSaveLib);
		toolbar.addSeparator();
		toolbar.add(new JLabel(" Object Type "));
		cmbSortName = new EdTextComboBox();
		cmbSortName.setEditor(new TextComboBoxEditor(
				new OCLIdentifierDocument()));
		cmbSortName.addItem("none");
		ActionListener actListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String objectSort = (String) cmbSortName.getSelectedItem();
				int count = cmbSortName.getItemCount();
				boolean found = false;
				for (int i = 0; i < count && !found; i++) {
					String item = (String) cmbSortName.getItemAt(i);
					if (item.equals(objectSort))
						found = true;
				}
				if (!found)
					cmbSortName.addItem(objectSort);
			}
		};
		cmbSortName.addActionListener(actListener);
		toolbar.add(cmbSortName);
		URL replaceURL = HistoryWindow.class.getResource("resources/replace.png");
		ImageIcon replaceIcon = new ImageIcon(replaceURL);
		JButton cmdReplace = new JButton(replaceIcon);
		cmdReplace.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				replaceSort();
			}
			});
		cmdReplace.setToolTipText("Replace Name/Icon");
		toolbar.add(cmdReplace);
		toolbar.addSeparator();
		toolbar.add(new JLabel(" Object Icon "));
		String defaultPath = top.strDomainsDir +
			File.separator + "icons" + File.separator + "icoRed.png";
		File temp = new File(defaultPath);
		lblIcon = null;
		try {
			lblIcon = new JLabel(new ImageIcon(temp.toURL()));
			iconFile = temp;
		} catch (MalformedURLException e){}
		if (lblIcon == null) {
			lblIcon = new JLabel("none");
		}
		toolbar.add(lblIcon);
		toolbar.addSeparator();
		URL iconURL = HistoryWindow.class.getResource("resources/iconSel.png");
		ImageIcon iconIcon = new ImageIcon(iconURL);
		JButton cmdIconSel = new JButton(iconIcon);
		cmdIconSel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String libraryPath = top.strDomainsDir+
				 	File.separator + "icons";
				JFileChooser chooser = new JFileChooser(libraryPath);
		        // Ok, set up our own file view for the chooser
		        chooser.setFileView(new PNGThumbNailViewer(top));
		        int option = chooser.showDialog(top,"Select Icon");
		        if (option == JFileChooser.APPROVE_OPTION) {
		            iconFile = chooser.getSelectedFile();
		            try {
		            	lblIcon.setIcon(new ImageIcon(iconFile.toURL()));
		            } catch (MalformedURLException ex){}
		        }
			}
			});
		cmdIconSel.setToolTipText("Select Icon for Objects");
		toolbar.add(cmdIconSel);
		toolbar.addSeparator();
		// check Diagram Consistency
		URL checkUrl = HistoryWindow.class.getResource("resources/check16.png");
		ImageIcon checkIcon = new ImageIcon(checkUrl);
		JButton cmdCheck = new JButton(checkIcon);
		cmdCheck.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				OclModel oclModel = new OclModel(curDomain);
				try {
					oclModel.checkConsistency(graph.getModel());
					JOptionPane.showMessageDialog(top,
							"All Checks Passed","Gipo Message",
							JOptionPane.INFORMATION_MESSAGE, null);
					Utility.debugPrintln("patterns",
							"ERROR in Diagram Consistency Check");
				} catch (OCLException ex) {
					JOptionPane.showMessageDialog(top,
							ex.getMessage() + "\n Please Correct and try again.", "GIPO Error",
							JOptionPane.ERROR_MESSAGE, null);
					Utility.debugPrintln("patterns",
							"ERROR in Diagram Consistency Check");
				}
			}
		});
		cmdCheck.setToolTipText("Check Diagram Consistency");
		toolbar.add(cmdCheck);
		// translate to OCL
		URL oclUrl = HistoryWindow.class.getResource("resources/oclTrans.png");
		ImageIcon oclIcon = new ImageIcon(oclUrl);
		JButton cmdToOcl = new JButton(oclIcon);
		cmdToOcl.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if (curDomain.isOclPlus()) {
					OclPlusModel oclModel = new OclPlusModel(curDomain);
					try {
						oclModel.toOCL(graph.getModel());
						dirty = false;
					} catch (OCLException ex) {
						Utility.debugPrintln("patterns",
							"ERROR in translation to OCL");
					}
				} else {
					OclModel oclModel = new OclModel(curDomain);
					try {
						oclModel.toOCL(graph.getModel());
						dirty = false;
					} catch (OCLException ex) {
						Utility.debugPrintln("patterns",
							"ERROR in translation to OCL");
					}
				}
			}
		});
		cmdToOcl.setToolTipText("Translate to Ocl");
		toolbar.add(cmdToOcl);
		// Close
		URL closeUrl = HistoryWindow.class.getResource("resources/close.gif");
		ImageIcon closeIcon = new ImageIcon(closeUrl);
		JButton cmdClose = new JButton(closeIcon);
		cmdClose.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if (dirty) {
					int ret = JOptionPane.showConfirmDialog(top,
							"Changes may be lost. Really Close?",
							"GIPO Message", JOptionPane.YES_NO_OPTION);
					if (ret == JOptionPane.NO_OPTION)
						return;
				}
				setVisible(false);
				dispose();
			}
		});
		cmdClose.setToolTipText("Close Window");
		toolbar.add(cmdClose);
		toolbar.add(Box.createHorizontalGlue());
		return toolbar;
	}


	/**
	 * createEditBar create the main editing tool bar
	 * 
	 * @return the tool bar
	 */
	private JToolBar createEditBar() {
		JToolBar toolbar = new JToolBar();
		toolbar.setMargin(new Insets(-3, 1, -3, 1));
		toolbar.setFloatable(false);

		ButtonGroup grpEditMode = new ButtonGroup();
		// Select Mode
		URL selectUrl = HistoryWindow.class.getResource("resources/select.gif");
		ImageIcon selectIcon = new ImageIcon(selectUrl);
		JToggleButton cmdSelect = new JToggleButton(selectIcon, true);
		cmdSelect.setToolTipText("Select");
		cmdSelect.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editMode = SELECT;
				graph.setPortsVisible(false);
			}
		});
		toolbar.add(cmdSelect);
		grpEditMode.add(cmdSelect);
		// Insert State Node
		URL stateUrl = HistoryWindow.class
				.getResource("resources/rectangle.gif");
		ImageIcon stateIcon = new ImageIcon(stateUrl);
		JToggleButton cmdState = new JToggleButton(stateIcon);
		cmdState.setToolTipText("Add New State");
		toolbar.add(cmdState);
		cmdState.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editMode = INSERTSTATE;
				graph.setPortsVisible(false);
			}
		});
		grpEditMode.add(cmdState);
		// Insert Transition Node
		URL transUrl = HistoryWindow.class
				.getResource("resources/transNode.png");
		ImageIcon transIcon = new ImageIcon(transUrl);
		JToggleButton cmdTrans = new JToggleButton(transIcon);
		cmdTrans.setToolTipText("Add Transition Node");
		toolbar.add(cmdTrans);
		cmdTrans.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editMode = INSERTTRANSITION;
				graph.setPortsVisible(false);
			}
		});
		grpEditMode.add(cmdTrans);
		if (curDomain.isOclPlus()) {
		//	Insert Process
			URL procUrl = HistoryWindow.class
				.getResource("resources/clock.png");
			procIcon = new ImageIcon(procUrl);
			JToggleButton cmdProc = new JToggleButton(procIcon);
			cmdProc.setToolTipText("Add Process");
			toolbar.add(cmdProc);
			cmdProc.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					editMode = INSERTPROCESS;
					graph.setPortsVisible(false);
				}
			});
			grpEditMode.add(cmdProc);
		}
		// Insert Plain link
		URL linkUrl = HistoryWindow.class.getResource("resources/connect.png");
		ImageIcon linkIcon = new ImageIcon(linkUrl);
		JToggleButton cmdLink = new JToggleButton(linkIcon);
		cmdLink.setToolTipText("Connect State and Transition Nodes");
		toolbar.add(cmdLink);
		cmdLink.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editMode = INSERTLINK;
				graph.setPortsVisible(true);
			}
		});
		grpEditMode.add(cmdLink);
		// Insert Merge Link
		URL mergeUrl = HistoryWindow.class.getResource("resources/merge.png");
		ImageIcon mergeIcon = new ImageIcon(mergeUrl);
		JToggleButton cmdMerge = new JToggleButton(mergeIcon);
		cmdMerge.setToolTipText("Merge Requires Presence");
		toolbar.add(cmdMerge);
		cmdMerge.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editMode = INSERTONEWAYMERGELINK;
				graph.setPortsVisible(true);
			}
		});
		grpEditMode.add(cmdMerge);
		// Insert Merge Necessary Link
		URL mergeNecUrl = HistoryWindow.class
				.getResource("resources/mergeNec.png");
		ImageIcon mergeNecIcon = new ImageIcon(mergeNecUrl);
		JToggleButton cmdNecMerge = new JToggleButton(mergeNecIcon);
		cmdNecMerge.setToolTipText("Merge Coordinates Transitions");
		toolbar.add(cmdNecMerge);
		cmdNecMerge.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editMode = INSERTTWOWAYMERGELINK;
				graph.setPortsVisible(true);
			}
		});
		grpEditMode.add(cmdNecMerge);
		if (curDomain.isOclPlus()) {
			// Insert Process Link
			URL procLinkUrl = HistoryWindow.class.getResource("resources/plink.png");
			ImageIcon procLinkIcon = new ImageIcon(procLinkUrl);
			JToggleButton cmdProcLink = new JToggleButton(procLinkIcon);
			cmdProcLink.setToolTipText("Process Link");
			toolbar.add(cmdProcLink);
			cmdProcLink.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					editMode = INSERTPROCESSLINK;
					graph.setPortsVisible(true);
				}
			});
			grpEditMode.add(cmdProcLink);
		}
		
//		 Insert Text Note
		URL textUrl = HistoryWindow.class
				.getResource("resources/textNote.png");
		ImageIcon textIcon = new ImageIcon(textUrl);
		JToggleButton cmdText = new JToggleButton(textIcon);
		cmdText.setToolTipText("Add Text Note");
		toolbar.add(cmdText);
		cmdText.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editMode = INSERTTEXT;
				graph.setPortsVisible(false);
			}
		});
		grpEditMode.add(cmdText);
		toolbar.addSeparator();
		// Toggle View full descriptions one object sort at a time
		URL descUrl = HistoryWindow.class.getResource("resources/desc.png");
		ImageIcon descIcon = new ImageIcon(descUrl);
		JButton cmdDesc = new JButton(descIcon);
		cmdDesc.setToolTipText("Toggle Selected Object Descriptions");
		cmdDesc.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				toggleDescription(e);
			}
		});
		toolbar.add(cmdDesc);
		// Hide - make merges invisible
		URL hideUrl = HistoryWindow.class.getResource("resources/hide.png");
		ImageIcon hideIcon = new ImageIcon(hideUrl);
		URL hide2Url = HistoryWindow.class.getResource("resources/hide2.png");
		ImageIcon hide2Icon = new ImageIcon(hide2Url);
		JToggleButton cmdHide = new JToggleButton(hideIcon);
		cmdHide.setSelectedIcon(hide2Icon);
		toolbar.add(cmdHide);
		cmdHide.setToolTipText("Hide Merge Connections");
		cmdHide.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				hideMerges(e);
			}
		});
		// Hide - make Object and its connections invisible
		URL hideObjUrl = HistoryWindow.class
				.getResource("resources/hideObj.png");
		ImageIcon hideObjIcon = new ImageIcon(hideObjUrl);
		URL hideObj2Url = HistoryWindow.class
				.getResource("resources/hideObj2.png");
		ImageIcon hideObj2Icon = new ImageIcon(hideObj2Url);
		JButton cmdHideObj = new JButton(hideObjIcon);
		//cmdHideObj.setSelectedIcon(hideObj2Icon);
		cmdHideObj.setToolTipText("Hide All Selected Object");
		toolbar.add(cmdHideObj);
		cmdHideObj.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				hideObject(e);
			}
		});
		graph.setPortsVisible(false);
		// Package and unpackage
		URL packageUrl = HistoryWindow.class.getResource("resources/package.png");
		ImageIcon packageIcon = new ImageIcon(packageUrl);
		URL packageOpenUrl = HistoryWindow.class.getResource("resources/packageOpen.png");
		ImageIcon packageOpenIcon = new ImageIcon(packageOpenUrl);
		JButton cmdPackage = new JButton(packageIcon);
		cmdPackage.setToolTipText("Package All Selected Object");
		toolbar.add(cmdPackage);
		cmdPackage.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				packageNodes(graph.getSelectionCells());
			}
		});
		JButton cmdPackageOpen = new JButton(packageOpenIcon);
		cmdPackageOpen.setToolTipText("Unpack Package");
		toolbar.add(cmdPackageOpen);
		cmdPackageOpen.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				openPackage(graph.getSelectionCells());
			}
		});
		graph.setPortsVisible(false);
		// Undo
		toolbar.addSeparator();
		URL undoUrl = HistoryWindow.class.getResource("resources/undo.gif");
		ImageIcon undoIcon = new ImageIcon(undoUrl);
		undo = new AbstractAction("", undoIcon) {
			public void actionPerformed(ActionEvent e) {
				undo();
			}
		};
		undo.setEnabled(false);
		toolbar.add(undo);
		// Redo
		URL redoUrl = HistoryWindow.class.getResource("resources/redo.gif");
		ImageIcon redoIcon = new ImageIcon(redoUrl);
		redo = new AbstractAction("", redoIcon) {
			public void actionPerformed(ActionEvent e) {
				redo();
			}
		};
		redo.setEnabled(false);
		toolbar.add(redo);
		//
		// Edit Block
		//
		toolbar.addSeparator();
		Action action;
		URL url;
		// Copy
		action = javax.swing.TransferHandler // JAVA13:
				// org.jgraph.plaf.basic.TransferHandler
				.getCopyAction();
		url = HistoryWindow.class.getResource("resources/copy.gif");
		action.putValue(Action.SMALL_ICON, new ImageIcon(url));
		toolbar.add(copy = new EventRedirector(action));
		// Paste
		action = javax.swing.TransferHandler // JAVA13:
				// org.jgraph.plaf.basic.TransferHandler
				.getPasteAction();
		url = HistoryWindow.class.getResource("resources/paste.gif");
		action.putValue(Action.SMALL_ICON, new ImageIcon(url));
		toolbar.add(paste = new EventRedirector(action));
		// Cut
		action = javax.swing.TransferHandler // JAVA13:
				// org.jgraph.plaf.basic.TransferHandler
				.getCutAction();
		url = HistoryWindow.class.getResource("resources/cut.gif");
		action.putValue(Action.SMALL_ICON, new ImageIcon(url));
		toolbar.add(cut = new EventRedirector(action));
		// Remove
		URL removeUrl = HistoryWindow.class.getResource("resources/delete.gif");
		ImageIcon removeIcon = new ImageIcon(removeUrl);
		remove = new AbstractAction("", removeIcon) {
			public void actionPerformed(ActionEvent e) {
				if (!graph.isSelectionEmpty()) {
					Object[] cells = graph.getSelectionCells();
					cells = graph.getDescendants(cells);
					graph.getModel().remove(cells);
				}
			}
		};
		remove.setEnabled(false);
		toolbar.add(remove);
		// Zoom Std
		toolbar.addSeparator();
		URL zoomUrl = HistoryWindow.class.getResource("resources/zoom.gif");
		ImageIcon zoomIcon = new ImageIcon(zoomUrl);
		toolbar.add(new AbstractAction("", zoomIcon) {
			public void actionPerformed(ActionEvent e) {
				graph.setScale(1.0);
			}
		});
		// Zoom In
		URL zoomInUrl = HistoryWindow.class.getResource("resources/zoomin.gif");
		ImageIcon zoomInIcon = new ImageIcon(zoomInUrl);
		toolbar.add(new AbstractAction("", zoomInIcon) {
			public void actionPerformed(ActionEvent e) {
				graph.setScale(2 * graph.getScale());
			}
		});
		// Zoom Out
		URL zoomOutUrl = HistoryWindow.class
				.getResource("resources/zoomout.gif");
		ImageIcon zoomOutIcon = new ImageIcon(zoomOutUrl);
		toolbar.add(new AbstractAction("", zoomOutIcon) {
			public void actionPerformed(ActionEvent e) {
				graph.setScale(graph.getScale() / 2);
			}
		});
		// Group
		toolbar.addSeparator();
		URL groupUrl = HistoryWindow.class.getResource("resources/group.gif");
		ImageIcon groupIcon = new ImageIcon(groupUrl);
		group = new AbstractAction("", groupIcon) {
			public void actionPerformed(ActionEvent e) {
				group(graph.getSelectionCells());
			}
		};
		group.setEnabled(false);
		toolbar.add(group);
		// Ungroup
		URL ungroupUrl = HistoryWindow.class
				.getResource("resources/ungroup.gif");
		ImageIcon ungroupIcon = new ImageIcon(ungroupUrl);
		ungroup = new AbstractAction("", ungroupIcon) {
			public void actionPerformed(ActionEvent e) {
				ungroup(graph.getSelectionCells());
			}
		};
		ungroup.setEnabled(false);
		toolbar.add(ungroup);
		// To Front
		toolbar.addSeparator();
		URL toFrontUrl = HistoryWindow.class
				.getResource("resources/tofront.gif");
		ImageIcon toFrontIcon = new ImageIcon(toFrontUrl);
		tofront = new AbstractAction("", toFrontIcon) {
			public void actionPerformed(ActionEvent e) {
				if (!graph.isSelectionEmpty())
					toFront(graph.getSelectionCells());
			}
		};
		tofront.setEnabled(false);
		toolbar.add(tofront);
		// To Back
		URL toBackUrl = HistoryWindow.class.getResource("resources/toback.gif");
		ImageIcon toBackIcon = new ImageIcon(toBackUrl);
		toback = new AbstractAction("", toBackIcon) {
			public void actionPerformed(ActionEvent e) {
				if (!graph.isSelectionEmpty())
					toBack(graph.getSelectionCells());
			}
		};
		toback.setEnabled(false);
		toolbar.add(toback);
		toolbar.addSeparator();
		toolbar.add(Box.createHorizontalGlue());

		return toolbar;
	}

	// This will change the source of the actionevent to graph.
	protected class EventRedirector extends AbstractAction {
		protected Action action;

		// Construct the "Wrapper" Action
		public EventRedirector(Action a) {
			super("", (ImageIcon) a.getValue(Action.SMALL_ICON));
			this.action = a;
		}

		// Redirect the Actionevent
		public void actionPerformed(ActionEvent e) {
			e = new ActionEvent(graph, e.getID(), e.getActionCommand(), e
					.getModifiers());
			action.actionPerformed(e);
		}
	}

	

	/**
	 * hideMerges toggle merge arrows invisible - visible
	 * 
	 * @param evt -
	 *            the button event
	 */
	private void hideMerges(ActionEvent evt) {
		JToggleButton btn = (JToggleButton) evt.getSource();
		if (btn.isSelected()) {
			// Hide merges
			((LHGraph) graph).toggleMerges(false);
		} else {
			// Reveal merges
			((LHGraph) graph).toggleMerges(true);
		}
	}

	/**
	 * toggleDescription
	 * Toggle Display of full details of selected object and its transitions
	 * @param e
	 */
	private void toggleDescription(ActionEvent e){
		String objSort = (String) cmbSortName.getSelectedItem();
		if (objSort == null || "none".equals(objSort)) {
			getToolkit().beep();
			JOptionPane.showMessageDialog(top, "You Must Enter an Object Type",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		((LHGraph) graph).toggleObjectDescriptions(objSort);
	}
	/**
	 * hideObject toggle merge Objects and their connections invisible - visible
	 * 
	 * @param evt -
	 *            the button event
	 */
	private void hideObject(ActionEvent evt) {
		String objSort = (String) cmbSortName.getSelectedItem();
		if (objSort == null || "none".equals(objSort)) {
			getToolkit().beep();
			JOptionPane.showMessageDialog(top, "You Must Enter an Object Type",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		((LHGraph) graph).toggleObjects(objSort);
	}

	/**
	 * save current OCL Domain request file name from user
	 */
	public void saveAsFile() {
		JFileChooser fileChoose = new JFileChooser(top.strDomainsDir);
		fileChoose.setDialogTitle("Save As");
		fileChoose.setFileFilter(gfxFilter);
		int result = fileChoose.showSaveDialog(top);
		File file = fileChoose.getSelectedFile();

		FileWriter domOut;
		if (file != null && result == JFileChooser.APPROVE_OPTION) {
			if (file.exists()) {
				JOptionPane query = new JOptionPane("Overwrite Existing File",
						JOptionPane.QUESTION_MESSAGE, JOptionPane.YES_NO_OPTION);
				JDialog dia = query.createDialog(top, "GIPO Query");
				dia.show();
				if (((Integer) query.getValue()).intValue() == 0) {
					DefaultGraphModelFileFormatXML writer = new DefaultGraphModelFileFormatXML(top);
					try {
						writer.write(file, (LHGraph) graph);
						curDomain.lifeHistoryFileName = file.getName();
						JOptionPane.showMessageDialog(top, "Wrote file "
								+ file.getName(), "GIPO Information",
								JOptionPane.INFORMATION_MESSAGE);
					} catch (Exception e) {
						JOptionPane.showMessageDialog(top,
								"Problem writing to file", "GIPO Error",
								JOptionPane.ERROR_MESSAGE);
						return;
					}
				}
			} else {
				DefaultGraphModelFileFormatXML writer = new DefaultGraphModelFileFormatXML(top);
				try {
					writer.write(file, (LHGraph) graph);
					JOptionPane.showMessageDialog(top, "Wrote file "
							+ file.getName(), "GIPO Information",
							JOptionPane.INFORMATION_MESSAGE);
				} catch (Exception e) {
					JOptionPane.showMessageDialog(top,
							"Problem writing to file", "GIPO Error",
							JOptionPane.ERROR_MESSAGE);
					return;
				}
			}
		} else {
			return;
		}
		graphFile = file;
	}
	
	

	/**
	 * Present a dialog box to have the user select an existing Graphic Domain
	 * specification
	 */
	public GraphModel loadGraphFile() {
		JFileChooser fileChoose = new JFileChooser(top.strDomainsDir);
		fileChoose.setDialogTitle("Open");
		fileChoose.setFileFilter(gfxFilter);
		int result = fileChoose.showOpenDialog(top);
		File file = fileChoose.getSelectedFile();
		graphFile = file;
		if (file != null && result == JFileChooser.APPROVE_OPTION) {
			GraphModel model = null;
			try {
				DefaultGraphModelFileFormatXML writer = new DefaultGraphModelFileFormatXML(top);
				model = graph.getModel();
				//graph.setModel(new DefaultGraphModel());
				graph.setModel(new LifeHistoryModel());
				model = writer.read(file, (LHGraph)graph);
				int ret = JOptionPane.showConfirmDialog(top,
						"Make " + file.getName() + " default Life History for this domain?",
						"GIPO Message", JOptionPane.YES_NO_OPTION);
				if (ret == JOptionPane.YES_OPTION) {
					graphFile = file;
					curDomain.lifeHistoryFileName = file.getName();
				}
			} catch (FileNotFoundException fe) {
				JOptionPane.showMessageDialog(top,
						"Cannot find file " + file.getName(), "GIPO Error",
						JOptionPane.ERROR_MESSAGE, null);
				graph.setModel(model);
				return model;
			} catch (Exception e) {
				e.printStackTrace();
				graph.setModel(model);
				return model;
			}
			return model;
		}
		return null;
	}

//	/**
//	 * convertToDisjunction convert the merge edges entering a node to
//	 * disjunctions Show as dashed arrow lines
//	 */
//	private void convertToDisjunction() {
//		if (graph.getSelectionCount() != 1) {
//			getToolkit().beep();
//			return;
//		}
//		Object node = graph.getSelectionCell();
//		if (!(node instanceof TransitionCell)) {
//			getToolkit().beep();
//			return;
//		}
//		TransitionCell trans = (TransitionCell) node;
//		// Check that there is at least two incomming merges
//		LifeHistoryModel curModel = (LifeHistoryModel) graph.getModel();
//		if (curModel.countMergesTo(trans) < 2) {
//			getToolkit().beep();
//			return;
//		}
//		LHUserObject userObj = (LHUserObject) trans.getUserObject();
//		userObj.makeDisjunction((LHGraph) graph, trans);
//	}



	/**
	 * addPropertyToSort iterate through nodes finding state nodes matching
	 * selected state and add propery sort to each such node.
	 * 
	 * @param model -
	 *            the graph model
	 * @param sortName -
	 *            the sort of the selected object state
	 * @param propertySort -
	 *            the property sort to add
	 * @param propertyName -
	 *            the name of this property
	 */
	private void addPropertyToSort(GraphModel model, String sortName,
			String propertySort, String propertyName) {
		String strPropID = "Property";
		String strPropTypeID = "Property Type";
		int suffix = 2;
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				// This node refers to a state
				LHUserObject userObj = (LHUserObject) ((StateCell) curRoot)
						.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				if (curSortName.equals(sortName)) {
					boolean found = false;
					String propVal = (String) userObj.getProperty("Property");
					while (!found && propVal != null) {
						if (propVal.equals(propertySort)) {
							found = true;
						} else {
							propVal = (String) userObj.getProperty("Property"
									+ suffix++);
						}
					}
					if (!found) {
						if (suffix != 2) {
							strPropID = "Property" + (suffix - 1);
							strPropTypeID = "Property Type" + (suffix - 1);
						}
						userObj.putProperty(strPropID, propertyName);
						userObj.putProperty(strPropTypeID, propertySort);
					} else {
						userObj.putProperty(strPropID, propertyName);
						userObj.putProperty(strPropTypeID, propertySort);
					}
				}
			}
		}
	}

	/**
	 * addAssociation add aassociation annotation (+) to the
	 * selected merge edge
	 */
	private void manageAssociation(String prefix,MergeEdge node) {
		LHUserObject userObj = (LHUserObject) node.getUserObject();
		String oldLabel = (String)userObj.getProperty("label");
		if (oldLabel != null) {
			if (oldLabel.startsWith("+") || oldLabel.startsWith("-")) {
				oldLabel = oldLabel.substring(1);
			}
			if (oldLabel != null && oldLabel.length() > 0) {
				prefix = prefix + oldLabel;
			}
		}
		userObj.putProperty("label", prefix);
		Map nested = new Hashtable();
		AttributeMap attr = new AttributeMap();
		GraphConstants.setValue(attr, userObj);
		nested.put(node, attr);
		graph.getModel().edit(nested, null, null, null);
	}
	
	
	private void replaceSort(){
		// TODO only doing sort names Must do Icons
		// Should check if new options already used in graph.
		String sortName = (String) cmbSortName.getSelectedItem();
		if ("none".equals(sortName)){
			JOptionPane
			.showMessageDialog(
					top,
					"You Must enter a new Object Type.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (graph.getSelectionCount() != 1) {
			getToolkit().beep();
			JOptionPane
			.showMessageDialog(
					top,
					"You Must Select a state node.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		Object node = graph.getSelectionCell();
		if (!(node instanceof StateCell) && !(node instanceof TransitionCell)){
			getToolkit().beep();
			JOptionPane
			.showMessageDialog(
					top,
					"You Must Select a state or transition node.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		if (node instanceof TransitionCell) {
			String oldSortName = ((TransitionCell)node).getObjectSort();
			if (oldSortName.equals(sortName)){
				JOptionPane
				.showMessageDialog(
						top,
						"The Type name of the selected transition and the new name are identical.",
						"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
				return;
			}
			((TransitionCell)node).setObjectSort(sortName);
			return;
		}
		StateCell state = (StateCell)node;
		String oldSortName = state.getObjectSort();
		if (oldSortName.equals(sortName)){
			JOptionPane
			.showMessageDialog(
					top,
					"The Type name of the selected state and the new name are identical.",
					"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		GraphModel model = graph.getModel();
		for (int inx = 0; inx < model.getRootCount(); inx++) {
			Object curRoot = model.getRootAt(inx);
			if (curRoot instanceof StateCell) {
				LHUserObject userObj = (LHUserObject) ((StateCell) curRoot)
						.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				if (curSortName.equals(oldSortName)){
					StateCell curState = (StateCell)curRoot;
					curState.setObjectSort(sortName);
					Map map = new Hashtable();
					if (iconFile != null) {
						ImageIcon icon = null;
						try {
							icon = new ImageIcon(iconFile.toURL());
						} catch (MalformedURLException e) {}
						GraphConstants.setIcon(map, icon);
						Map nested = new Hashtable();
						nested.put(curState,map);
						model.edit(nested,null,null,null);
					} 
				}
			} else if (curRoot instanceof TransitionCell){
				LHUserObject userObj = (LHUserObject) ((TransitionCell) curRoot)
					.getUserObject();
				String curSortName = (String) userObj.getProperty("ObjectSort");
				if (curSortName.equals(oldSortName)){
					((TransitionCell)curRoot).setObjectSort(sortName);
				}
			} 
		}
	}
}
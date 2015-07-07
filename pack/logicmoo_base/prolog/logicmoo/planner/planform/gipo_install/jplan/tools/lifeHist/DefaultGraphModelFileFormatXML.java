/*
 * @(#)DefaultGraphModelFileFormatXML.java	1.0 17.02.2003
 *
 * Copyright (C) 2003 sven.luzar
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 */

package jplan.tools.lifeHist;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import javax.swing.ImageIcon;
import java.io.*;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.StringTokenizer;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.filechooser.FileFilter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.jgraph.*;
import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.ConnectionSet;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.Edge;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphModel;
//import org.jgraph.pad.resources.Translator;
//import org.jgraph.utils.ShadowBorder;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import jplan.top.OclEd;

/**
 * File format for the default graph model. The file format writes a XML file
 * with the graph as content.
 * 
 * @author luzar
 * @version 1.0
 */
public class DefaultGraphModelFileFormatXML {

	private OclEd top = null; // Reference to editor
	
	//private static DefaultGraphModelFileFormatXML _instance = new DefaultGraphModelFileFormatXML(top);

	public static final String EMPTY = new String("Empty");

	public static final String PARENT = new String("Parent");

	protected static Map cells = new Hashtable();

	protected static Map attrs = new Hashtable();

	protected static Map objs = new Hashtable();

	protected static List delayedAttributes;

	protected static List connectionSetIDs;

	protected Map cellMap = new Hashtable();

	protected AttributeCollection attrCol = new AttributeCollection();

	protected Map userObjectMap = new Hashtable();

	/**
	 * file filter for this file format
	 */
	FileFilter fileFilter;

	/**
	 * accessory component. A checkbox for the zipped output or unzipped output
	 *  
	 */
	JComponent compZipSelect;

	/**
	 * a const value for the key at the properties hashtable
	 */
	public static final String COMPRESS_WITH_ZIP = "CompressWithZip";

//	public static DefaultGraphModelFileFormatXML instance(OclEd top) {
//		this.top = top;
//		if (null == _instance) {
//			_instance = new DefaultGraphModelFileFormatXML(top);
//		}
//		return _instance;
//	}

	/**
	 * Constructor for DefaultGraphModelFileFormatXML.
	 */
	protected DefaultGraphModelFileFormatXML(OclEd top) {
		this.top = top;
		fileFilter = new FileFilter() {
			/**
			 * @see javax.swing.filechooser.FileFilter#accept(File)
			 */
			public boolean accept(File f) {
				if (f == null)
					return false;
				if (f.getName() == null)
					return false;
				if (f.getName().endsWith(".jgx"))
					return true;
				if (f.isDirectory())
					return true;

				return false;
			}

			/**
			 * @see javax.swing.filechooser.FileFilter#getDescription()
			 */
			public String getDescription() {
				return "FileDesc.JGraphpadDiagramXml"; /*
																			  * Finished:
																			  * Original="JGraphpad
																			  * Diagram
																			  * (*.pad_xml)"
																			  */
			}
		};
		compZipSelect = new JCheckBox("zipCompress");
	}

	/**
	 * returns <tt>pad_xml</tt>
	 */
	public String getFileExtension() {
		return "jgx";
	}


	
	/**
	 * write
	 * main method to write the graph to file in an xml
	 * format
	 * @param file
	 * @param lhGraph
	 * @throws Exception
	 */
	public void write(File file, LHGraph lhGraph)
			throws Exception {

		// don't try / catch this command
		// sothat we get error messages at the
		// frontend.
		// e.g. I you have not permissions to
		// write a file you should get an error message

		OutputStream out = null;
		out = new FileOutputStream(file);
		out = new BufferedOutputStream(out);
		out.write(toString(lhGraph).getBytes());
		out.flush();
		out.close();
	}



	/**
	 * Reads the File form the XML input stream. Tempts to load from a zipped
	 * input stream. If this procedure fails the method tempts to load without
	 * the zipped option.
	 * 
	 * @param file - the file to read from
	 * @param LHGraph the graph structure to populate
	 */
	public GraphModel read(File file, LHGraph LHGraph)
			throws Exception {
		read(new FileInputStream(file), LHGraph);
		return LHGraph.getModel();
	}



	//
	// Read
	//

	public void read(InputStream in, LHGraph graph) throws Exception {
		GraphModel model = graph.getModel();
		// Create a DocumentBuilderFactory
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		// Create a DocumentBuilder
		DocumentBuilder db = dbf.newDocumentBuilder();
		// Parse the input file to get a Document object
		Document doc = db.parse(in);
		// Get the first child (the jgx-element)
		Node modelNode = null;
		Node objsNode = null;
		Node attrsNode = null;
		Node viewNode = null;

		delayedAttributes = new LinkedList();
		connectionSetIDs = new LinkedList();

		for (int i = 0; i < doc.getDocumentElement().getChildNodes()
				.getLength(); i++) {
			Node node = doc.getDocumentElement().getChildNodes().item(i);
			if (node.getNodeName().toLowerCase().equals("model")) {
				modelNode = node;
			} else if (node.getNodeName().toLowerCase().equals("user")) {
				objsNode = node;
			} else if (node.getNodeName().toLowerCase().equals("attrs")) {
				attrsNode = node;
			} else if (node.getNodeName().toLowerCase().equals("view")) {
				viewNode = node;
			}
		}
		objs = decodeUserObjects(objsNode);
		attrs = parseAttrs(attrsNode);
		attrs = augmentAttrs(attrs);
		Map settings = decodeMap(viewNode, false, false);
		ConnectionSet cs = new ConnectionSet();
		Hashtable cells = new Hashtable();
		DefaultGraphCell[] insert = parseChildren(modelNode, cells, cs);

		// Create ConnectionSet
		Iterator it = connectionSetIDs.iterator();
		while (it.hasNext()) {
			ConnectionID cid = (ConnectionID) it.next();
			Object cell = cid.getCell();
			String tid = cid.getTargetID();
			if (tid != null) {
				Object port = cells.get(tid);
				if (port != null) {
					cs.connect(cell, port, cid.isSource());
				}
			}
		}

		// Create AttributeMap
		Map nested = new Hashtable();
		it = delayedAttributes.iterator();
		while (it.hasNext()) {
			DelayedAttributeID att = (DelayedAttributeID) it.next();
			Map attr = (Map) attrs.get(att.getMapID());
			if (attr != null) {
				AttributeMap attr_temp = new AttributeMap(attr);
				attr = (Map) attr_temp.clone();
				if (att.getBounds() != null)
					GraphConstants.setBounds(attr, att.getBounds());
				if (att.getPoints() != null)
					GraphConstants.setPoints(attr, att.getPoints());
				nested.put(att.getCell(), attr);
			}
		}

		// Apply settings to graph
		applySettings(settings, graph);

		// Insert the cells (View stores attributes)
		model.insert(insert, nested, cs, null, null);
	}

	public static void applySettings(Map s, LHGraph graph) {
		Object tmp;

		tmp = s.get("editable");
		if (tmp != null)
			graph.setEditable(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("bendable");
		if (tmp != null)
			graph.setBendable(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("cloneable");
		if (tmp != null)
			graph.setCloneable(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("connectable");
		if (tmp != null)
			graph.setConnectable(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("disconnectable");
		if (tmp != null)
			graph.setDisconnectable(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("disconnectOnMove");
		if (tmp != null)
			graph.setDisconnectOnMove(new Boolean(tmp.toString())
					.booleanValue());

		tmp = s.get("doubleBuffered");
		if (tmp != null)
			graph.setDoubleBuffered(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("dragEnabled");
		if (tmp != null)
			graph.setDragEnabled(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("dropEnabled");
		if (tmp != null)
			graph.setDropEnabled(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("moveable");
		if (tmp != null)
			graph.setMoveable(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("sizeable");
		if (tmp != null)
			graph.setSizeable(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("selectNewCells");
		//if (tmp != null)
			//graph.setSelectClonedCells(new Boolean(tmp.toString()).booleanValue());
			//graph.setSelectNewCells(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("gridVisible");
		if (tmp != null)
			graph.setGridVisible(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("gridEnabled");
		if (tmp != null)
			graph.setGridEnabled(new Boolean(tmp.toString()).booleanValue());

		tmp = s.get("gridSize");
		if (tmp != null)
			graph.setGridSize(Double.parseDouble(tmp.toString()));

		tmp = s.get("gridMode");
		if (tmp != null)
			graph.setGridMode(Integer.parseInt(tmp.toString()));

		tmp = s.get("scale");
		if (tmp != null)
			graph.setScale(Double.parseDouble(tmp.toString()));

		tmp = s.get("antiAlias");
		if (tmp != null)
			graph.setAntiAliased(new Boolean(tmp.toString()).booleanValue());

	}

	public static Map augmentAttrs(Map attrs) {
		Map newAttrs = new Hashtable();
		Iterator it = attrs.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry entry = (Map.Entry) it.next();
			Object key = entry.getKey();
			Map map = (Map) entry.getValue();
			Stack s = new Stack();
			s.add(map);
			Object parentID = map.get(PARENT);
			Object hook = null;
			while (parentID != null) {
				hook = attrs.get(parentID);
				s.add(hook);
				parentID = ((Map) hook).get(PARENT);
			}
			Map newMap = new Hashtable();
			while (!s.isEmpty()) {
				newMap.putAll((Map) s.pop());
			}
			newMap.remove(PARENT);
			// Remove Empty values
			Iterator it2 = newMap.entrySet().iterator();
			while (it2.hasNext()) {
				entry = (Map.Entry) it2.next();
				if (entry.getValue() == EMPTY)
					it2.remove();
			}
			newAttrs.put(key, newMap);
		}
		return newAttrs;
	}

	public DefaultGraphCell parseCell(Node node, Hashtable cells,
			ConnectionSet cs) {
		DefaultGraphCell cell = null;
		if (node.getNodeName().toLowerCase().equals("a")) {
			Node key = node.getAttributes().getNamedItem("id");
			Node type = node.getAttributes().getNamedItem("class");
			if (key != null && type != null) {
				Node value = node.getAttributes().getNamedItem("val");
				Object userObject = "";
				if (value != null)
					userObject = objs.get(value.getNodeValue());
				cell = createCell(type.getNodeValue(), userObject);
				cell.setAttributes(new AttributeMap()); // install custom
														  // attribute map

				if (cell != null) {
					cells.put(key.getNodeValue(), cell);

					DefaultGraphCell[] children = parseChildren(node, cells, cs);
					for (int i = 0; i < children.length; i++)
						cell.add(children[i]);

					Node source = node.getAttributes().getNamedItem("src");
					Node target = node.getAttributes().getNamedItem("tgt");
					if (source != null) {
						ConnectionID cid = new ConnectionID(cell, source
								.getNodeValue(), true);
						connectionSetIDs.add(cid);
					}
					if (target != null) {
						ConnectionID cid = new ConnectionID(cell, target
								.getNodeValue(), false);
						connectionSetIDs.add(cid);
					}

					Node boundsNode = node.getAttributes().getNamedItem("rect");
					Rectangle2D bounds = null;
					if (boundsNode != null) {
						Object rectangle = decodeValue(Rectangle2D.class,
								boundsNode.getNodeValue());
						if (rectangle instanceof Rectangle2D)
							bounds = (Rectangle2D) rectangle;
					}

					Node pointsNode = node.getAttributes().getNamedItem("pts");
					List points = null;
					if (pointsNode != null) {
						Object pointList = decodeValue(List.class, pointsNode
								.getNodeValue());
						if (pointList instanceof List)
							points = (List) pointList;
					}

					Node attr = node.getAttributes().getNamedItem("attr");
					String mapID = null;
					if (attr != null)
						mapID = attr.getNodeValue();

					if (mapID != null)
						delayedAttributes.add(new DelayedAttributeID(cell,
								bounds, points, mapID));
				}
			}
		}
		return cell;
	}

	public DefaultGraphCell[] parseChildren(Node node, Hashtable cells,
			ConnectionSet cs) {
		List list = new LinkedList();
		for (int i = 0; i < node.getChildNodes().getLength(); i++) {
			Node child = node.getChildNodes().item(i);
			DefaultGraphCell cell = parseCell(child, cells, cs);
			if (cell != null)
				list.add(cell);
		}
		DefaultGraphCell[] dgc = new DefaultGraphCell[list.size()];
		list.toArray(dgc);
		return dgc;
	}

	public Map parseAttrs(Node node) {
		Hashtable map = new Hashtable();
		for (int i = 0; i < node.getChildNodes().getLength(); i++) {
			Node child = node.getChildNodes().item(i);
			if (child.getNodeName().toLowerCase().equals("map")) {
				Node key = child.getAttributes().getNamedItem("id");
				Node pid = child.getAttributes().getNamedItem("pid");
				Map attrs = decodeMap(child, true, false);
				if (key != null && attrs.size() > 0) {
					if (pid != null)
						attrs.put(PARENT, pid.getNodeValue());
					map.put(key.getNodeValue(), attrs);
				}
			}
		}
		return map;
	}

	/**
	 * Returns an attributeMap for the specified position and color.
	 */
	public static Map createDefaultAttributes() {
		// Create an AttributeMap
		AttributeMap map = new AttributeMap();
		// Set a Black Line Border (the Border-Attribute must be Null!)
		GraphConstants.setBorderColor(map, Color.black);
		// Return the Map
		return map;
	}

	public static class DelayedAttributeID {

		protected Object cell;

		protected Rectangle2D bounds;

		protected List points;

		protected String mapID;

		public DelayedAttributeID(Object cell, Rectangle2D bounds, List points,
				String mapID) {
			this.cell = cell;
			this.bounds = bounds;
			this.points = points;
			this.mapID = mapID;
		}

		/**
		 * @return
		 */
		public Object getCell() {
			return cell;
		}

		/**
		 * @return
		 */
		public Rectangle2D getBounds() {
			return bounds;
		}

		/**
		 * @return
		 */
		public String getMapID() {
			return mapID;
		}

		/**
		 * @return
		 */
		public List getPoints() {
			return points;
		}

		/**
		 * @param rectangle
		 */
		public void setBounds(Rectangle2D rectangle) {
			bounds = rectangle;
		}

		/**
		 * @param object
		 */
		public void setCell(Object object) {
			cell = object;
		}

		/**
		 * @param string
		 */
		public void setMapID(String string) {
			mapID = string;
		}

		/**
		 * @param list
		 */
		public void setPoints(List list) {
			points = list;
		}

	}

	public static class ConnectionID {

		protected Object cell;

		protected String targetID;

		protected boolean source;

		public ConnectionID(Object cell, String targetID, boolean source) {
			this.cell = cell;
			this.targetID = targetID;
			this.source = source;
		}

		/**
		 * @return
		 */
		public Object getCell() {
			return cell;
		}

		/**
		 * @return
		 */
		public boolean isSource() {
			return source;
		}

		/**
		 * @return
		 */
		public String getTargetID() {
			return targetID;
		}

		/**
		 * @param object
		 */
		public void setCell(Object object) {
			cell = object;
		}

		/**
		 * @param b
		 */
		public void setSource(boolean b) {
			source = b;
		}

		/**
		 * @param string
		 */
		public void setTargetID(String string) {
			targetID = string;
		}

	}

	//
	// Write
	//

	public String toString(LHGraph graph) {
		userObjectMap.clear();
		cellMap.clear();
		attrs.clear();

		String xml = "<jgx-1.01>\n";

		GraphModel model = graph.getModel();
		xml += "<model>\n";
		xml += outputModel(model, "\t", null);
		xml += "</model>\n";

		xml += "<attrs>\n";
		xml += outputAttributes("\t");
		xml += "</attrs>\n";

		xml += "<user>\n";
		xml += encodeUserObjects("\t", userObjectMap);
		xml += "</user>\n";

		xml += "<view>\n";
		xml += outputView(graph, "\t");
		xml += "</view>\n";

		// Close main tags
		xml += "</jgx-1.01>\n";
		return xml;
	}

	public String outputView(LHGraph graph, String indent) {
		String xml = indent + "<a key=\"editable\" val=\"" + graph.isEditable()
				+ "\"/>\n" + indent + "<a key=\"bendable\" val=\""
				+ graph.isBendable() + "\"/>\n" + indent
				+ "<a key=\"cloneable\" val=\"" + graph.isCloneable()
				+ "\"/>\n" + indent + "<a key=\"connectable\" val=\""
				+ graph.isConnectable() + "\"/>\n" + indent
				+ "<a key=\"disconnectable\" val=\"" + graph.isDisconnectable()
				+ "\"/>\n" + indent + "<a key=\"disconnectOnMove\" val=\""
				+ graph.isDisconnectOnMove() + "\"/>\n" + indent
				+ "<a key=\"doubleBuffered\" val=\"" + graph.isDoubleBuffered()
				+ "\"/>\n" + indent + "<a key=\"dragEnabled\" val=\""
				+ graph.isDragEnabled() + "\"/>\n" + indent
				+ "<a key=\"dropEnabled\" val=\"" + graph.isDropEnabled()
				+ "\"/>\n" + indent + "<a key=\"moveable\" val=\""
				+ graph.isMoveable() + "\"/>\n" + indent
				+ "<a key=\"sizeable\" val=\"" + graph.isSizeable() + "\"/>\n"
				//+ indent + "<a key=\"selectNewCells\" val=\""
				//+ graph.isSelectNewCells() + "\"/>\n" 
				+ indent
				+ "<a key=\"gridVisible\" val=\"" + graph.isGridVisible()
				+ "\"/>\n" + indent + "<a key=\"gridEnabled\" val=\""
				+ graph.isGridEnabled() + "\"/>\n" + indent
				+ "<a key=\"gridSize\" val=\"" + graph.getGridSize() + "\"/>\n"
				+ indent + "<a key=\"gridMode\" val=\"" + graph.getGridMode()
				+ "\"/>\n" + indent + "<a key=\"scale\" val=\""
				+ graph.getScale() + "\"/>\n" + indent
				+ "<a key=\"antiAlias\" val=\"" + graph.isAntiAliased()
				+ "\"/>\n";
		return xml;
	}

	public String outputModel(GraphModel model, String indent, Object parent) {
		String xml = new String("");
		int max = (parent != null) ? model.getChildCount(parent) : model
				.getRootCount();
		for (int i = 0; i < max; i++) {
			Object cell = (parent != null) ? model.getChild(parent, i) : model
					.getRootAt(i);
			if (cell != null)
				xml += outputCell(indent, model, cell);
		}
		return xml;
	}

	public String outputCell(String indent, GraphModel model, Object cell) {
		Map map = new Hashtable(model.getAttributes(cell));
		Rectangle2D r = (Rectangle2D) map.remove(GraphConstants.BOUNDS);
		Object value = map.remove(GraphConstants.VALUE);
		if (GraphConstants.getFont(map).equals(GraphConstants.DEFAULTFONT))
			map.remove(GraphConstants.FONT);
		Object source = model.getSource(cell);
		Object target = model.getTarget(cell);
		if (GraphConstants.getRouting(map) != null)
			map.remove(GraphConstants.POINTS);
		String sourceID = "";
		String targetID = "";
		if (source != null)
			sourceID = " src=\"" + getID(source) + "\"";
		if (target != null)
			targetID = " tgt=\"" + getID(target) + "\"";
		String bounds = "";
		String valueS = "";
		if (r != null && !model.isEdge(cell) && !model.isPort(cell)
				&& !r.equals(DefaultGraphCell.defaultBounds))
			bounds = " rect=\"" + encodeValue(r) + "\"";
		List p = GraphConstants.getPoints(map);
		map.remove(GraphConstants.POINTS);
		String points = "";
		if (p != null) {
			String tmp = encodeValue(p);
			if (tmp.length() > 0)
				points = " pts=\"" + tmp + "\"";
		}
		if (value != null)
			valueS = " val=\"" + getUserObjectID(value) + "\"";
		String attrID = "";
		if (map.size() > 0)
			attrID = " attr=\"" + attrCol.addMap(map) + "\"";
		String xml = new String(indent + "<a class=\"" + getType(cell)
				+ "\" id=\"" + getID(cell) + "\"" + valueS + sourceID
				+ targetID + bounds + points + attrID);
		if (model.getChildCount(cell) > 0)
			xml += ">\n" + outputModel(model, indent + "\t", cell) + indent
					+ "</a>\n";
		else
			xml += "/>\n";
		return xml;
	}

	public int getUserObjectID(Object object) {
		Integer index = (Integer) userObjectMap.get(object);
		if (index != null)
			return index.intValue();
		index = new Integer(userObjectMap.size() + 1);
		userObjectMap.put(object, index);
		return index.intValue();
	}

	public int getID(Object object) {
		Integer index = (Integer) cellMap.get(object);
		if (index != null)
			return index.intValue();
		index = new Integer(cellMap.size() + 1);
		cellMap.put(object, index);
		return index.intValue();
	}

	public String outputAttributes(String indent) {
		Set set = new HashSet();
		set.add(PARENT);
		set.add(GraphConstants.BOUNDS);
		set.add(GraphConstants.POINTS);

		String xml = new String();
		for (int i = 0; i < attrCol.maps.size(); i++) {
			Map map = (Map) attrCol.maps.get(i);
			Object hook = map.get(PARENT);
			String hookS = "";
			if (hook != null)
				hookS = " pid=\"" + attrCol.maps.indexOf(hook) + "\"";
			xml += indent + "<map id=\"" + i + "\"" + hookS + ">\n";
			xml += encodeMap(indent + "\t", map, false, set, false);
			xml += indent + "</map>\n";
		}
		return xml;
	}

	public class AttributeCollection {

		public List maps = new LinkedList();

		public int addMap(Map attr) {
			Iterator it = maps.iterator();
			Map storeMap = new Hashtable(attr);
			Map hook = storeMap;
			while (it.hasNext()) {
				Map ref = (Map) it.next();
				Map diff = diffMap(ref, attr);
				if (diff.size() < storeMap.size()) {
					hook = ref;
					storeMap = diff;
				}
			}
			if (storeMap.size() == 0 && hook != storeMap)
				return maps.indexOf(hook);
			if (hook != storeMap)
				storeMap.put(PARENT, hook);
			maps.add(storeMap);
			return maps.indexOf(storeMap);
		}

		public void clear() {
			maps.clear();
		}

		/**
		 * Returns a new map that contains all (key, value)-pairs of
		 * <code>newState</code> where either key is not used or value is
		 * different for key in <code>oldState</code>. In other words, this
		 * method removes the common entries from oldState and newState, and
		 * returns the "difference" between the two.
		 * 
		 * This method never returns null.
		 */
		public Map diffMap(Map oldState, Map newState) {
			// Augment oldState
			Stack s = new Stack();
			s.add(oldState);
			Object hook = oldState.get(PARENT);
			while (hook instanceof Map) {
				s.add(hook);
				hook = ((Map) hook).get(PARENT);
			}
			oldState = new Hashtable();
			while (!s.isEmpty()) {
				oldState.putAll((Map) s.pop());
			}
			Map diff = new Hashtable();
			Iterator it = newState.entrySet().iterator();
			while (it.hasNext()) {
				Map.Entry entry = (Map.Entry) it.next();
				Object key = entry.getKey();
				Object oldValue = oldState.remove(key);
				if (key != PARENT) {
					Object newValue = entry.getValue();
					if (oldValue == null || !oldValue.equals(newValue))
						diff.put(key, newValue);
				}
			}
			it = oldState.keySet().iterator();
			while (it.hasNext()) {
				Object key = it.next();
				if (!oldState.get(key).equals(""))
					diff.put(key, "");
			}
			diff.remove(PARENT);
			return diff;
		}

	}

	//
	// Codec
	//

	public static String[] knownKeys = new String[] { GraphConstants.ABSOLUTEX,
			GraphConstants.ABSOLUTEY, GraphConstants.AUTOSIZE,
			GraphConstants.BACKGROUND, GraphConstants.GRADIENTCOLOR,
			GraphConstants.BEGINFILL, GraphConstants.BEGINSIZE,
			GraphConstants.BENDABLE, GraphConstants.BORDER,
			GraphConstants.BORDERCOLOR, GraphConstants.BOUNDS,
			GraphConstants.CONNECTABLE, GraphConstants.DASHPATTERN,
			GraphConstants.DISCONNECTABLE, GraphConstants.EDITABLE,
			GraphConstants.ENDFILL, GraphConstants.ENDSIZE,
			GraphConstants.FONT, GraphConstants.FOREGROUND,
			GraphConstants.HORIZONTAL_ALIGNMENT,
			GraphConstants.VERTICAL_ALIGNMENT, GraphConstants.ICON,
			GraphConstants.LABELALONGEDGE, GraphConstants.LABELPOSITION,
			GraphConstants.LINEBEGIN, GraphConstants.LINECOLOR,
			GraphConstants.LINEEND, GraphConstants.LINESTYLE,
			GraphConstants.LINEWIDTH, GraphConstants.MOVEABLE,
			GraphConstants.OFFSET, GraphConstants.OPAQUE,
			GraphConstants.POINTS, GraphConstants.ROUTING, GraphConstants.SIZE,
			GraphConstants.SIZEABLE, GraphConstants.VALUE };

	public static Class[] keyTypes = new Class[] { Boolean.class,
			Boolean.class, Boolean.class, Color.class, Color.class,
			Boolean.class, Integer.class, Boolean.class, Border.class,
			Color.class, Rectangle2D.class, Boolean.class, float[].class,
			Boolean.class, Boolean.class, Boolean.class, Integer.class,
			Font.class, Color.class, Integer.class, Integer.class, Icon.class,
			Boolean.class, Point.class, Integer.class, Color.class,
			Integer.class, Integer.class, Float.class, Boolean.class,
			Point.class, Boolean.class, List.class, Edge.Routing.class,
			Dimension.class, Boolean.class, Object.class };

	public static String encodeMap(String indent, Map attributes,
			boolean invert, Set excludeAttributes, boolean URLencodeValues) {
		String xml = new String("");
		Iterator it = attributes.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry entry = (Map.Entry) it.next();
			Object key = entry.getKey();
			if (excludeAttributes == null || !excludeAttributes.contains(key)) {
				Object value = entry.getValue();
				if (invert) {
					Object tmp = key;
					key = value;
					value = tmp;
				}
				if (URLencodeValues) {
					try {
						key = URLEncoder.encode(key.toString(), "UTF-8");
						value = URLEncoder.encode(encodeValue(value), "UTF-8");
					} catch (Exception e) {
						System.err.println(e.getMessage());
					}
				}
				xml += indent + "<a key=\"" + encodeKey(key.toString())
						+ "\" val=\"" + encodeValue(value) + "\"/>\n";
			}
		}
		return xml;
	}

	public static String encodeUserObjects(String indent, Map userObjects) {
		String xml = new String("");
		Iterator it = userObjects.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry entry = (Map.Entry) it.next();
			Object key = entry.getValue();
			Object value = entry.getKey();
			if (value instanceof LHUserObject) {
				xml += indent + "<a key=\"" + encodeKey(key.toString()) + "\"";
				Map map = ((LHUserObject) value).getProperties();
				xml += ">\n" + encodeMap(indent + "\t", map, false, null, true)
						+ indent + "</a>\n";
			} else {
				try {
					value = URLEncoder.encode(encodeValue(value), "UTF-8");
				} catch (Exception e) {
					System.err.println(e.getMessage());
				}
				xml += indent + "<a key=\"" + encodeKey(key.toString())
						+ "\" val=\"" + value + "\"/>\n";
			}
		}
		return xml;
	}

	public static String encodeKey(String key) {
		// 		for (int i = 0; i < knownKeys.length; i++)
		// 			if (key.equals(knownKeys[i]))
		// 				return Integer.toString(i);
		return key;
	}

	public static String encodeValue(Object value) {
		String ret = "";
		if (value instanceof Rectangle2D) {
			Rectangle2D r = (Rectangle2D) value;
			ret = r.getX() + "," + r.getY() + "," + r.getWidth() + ","
					+ r.getHeight();
		} else if (value instanceof List) { // TODO: non-points
			List list = (List) value;
			String s = "";
			for (int i = 0; i < list.size(); i++) {
				if (list.get(i) instanceof Point2D) {
					Point2D pt = (Point2D) list.get(i);
					s = s + pt.getX() + "," + pt.getY() + ",";
				}
			}
			ret = (s.length() > 0) ? s.substring(0, s.length() - 1) : s;
		} else if (value instanceof Font) {
			Font font = (Font) value;
			ret = font.getName() + "," + font.getSize() + "," + font.getStyle();
		} else if (value instanceof Color) {
			Color color = (Color) value;
			ret = Integer.toString(color.getRed()) + ","
					+ Integer.toString(color.getGreen()) + ","
					+ Integer.toString(color.getBlue());
		} else if (value instanceof Point) {
			Point point = (Point) value;
			ret = point.x + "," + point.y;
		} else if (value instanceof float[]) {
			float[] f = (float[]) value;
			String s = "";
			for (int i = 0; i < f.length; i++) {
				s = s + Float.toString(f[i]) + ",";
			}
			ret = s.substring(0, s.length() - 1);
		} else if (value instanceof Border) {
			if (value instanceof LineBorder) {
				LineBorder lb = (LineBorder) value;
				ret = "L," + lb.getLineColor().getRGB() + ","
						+ lb.getThickness();
			} else if (value instanceof BevelBorder) {
				BevelBorder bb = (BevelBorder) value;
				ret = "B," + bb.getBevelType();
			}
		} else if (value instanceof Edge.Routing) {
			if (value instanceof DefaultEdge.DefaultRouting)
				ret = "simple";
		} else if (value != null)
			ret = value.toString();
		return ret;
	}

	public Map decodeMap(Node node, boolean useKnownKeys,
			boolean URLdecodeValues) {
		Hashtable map = new Hashtable();
		for (int i = 0; i < node.getChildNodes().getLength(); i++) {
			Node child = node.getChildNodes().item(i);
			if (child.getNodeName().toLowerCase().equals("a")) {
				Node key = child.getAttributes().getNamedItem("key");
				Node value = child.getAttributes().getNamedItem("val");
				if (key != null && value != null) {
					// TODO can I trap icon values here and load them up????
					String keyVal = key.getNodeValue().toString();
					Object valueS = value.getNodeValue().toString();
					if (useKnownKeys) {
						int index = -1;
						for (int j = 0; j < knownKeys.length; j++)
							if (keyVal.equals(knownKeys[j]))
								index = j;
						if (index != -1)
							valueS = decodeValue(keyTypes[index], valueS
									.toString());
					} else if (URLdecodeValues) {

						try {
							keyVal = URLDecoder.decode(keyVal.toString(),
									"UTF-8");
							valueS = URLDecoder.decode(valueS.toString(),
									"UTF-8");
						} catch (Exception e) {
							System.err.println(e.getMessage());
						}
					}
					if (valueS != null)
						map.put(keyVal, valueS);
				}
			}
		}
		return map;
	}

	public Map decodeUserObjects(Node node) {
		Hashtable map = new Hashtable();
		for (int i = 0; i < node.getChildNodes().getLength(); i++) {
			Node child = node.getChildNodes().item(i);
			if (child.getNodeName().toLowerCase().equals("a")) {
				Node key = child.getAttributes().getNamedItem("key");
				Node value = child.getAttributes().getNamedItem("val");
				if (key != null) {
					String keyVal = key.getNodeValue().toString();
					if (value != null) {
						Object valueS = value.getNodeValue().toString();
						if (valueS != null) {
							try {
								valueS = URLDecoder.decode(valueS.toString(),
										"UTF-8");
							} catch (Exception e) {
								System.err.println(e.getMessage());
							}
							map.put(keyVal, valueS);
						}
					} else {
						Map properties = decodeMap(child, false, true);
						if (properties != null)
							map.put(keyVal, properties);
					}
				}
			}
		}
		return map;
	}

	public static String[] tokenize(String s, String token) {
		StringTokenizer tokenizer = new StringTokenizer(s, token);
		String[] tok = new String[tokenizer.countTokens()];
		int i = 0;
		while (tokenizer.hasMoreElements()) {
			tok[i++] = tokenizer.nextToken();
		}
		return tok;
	}

	public Object decodeValue(Class key, String value) {
		if (key != String.class && key != Object.class
				&& (value == null || value.equals("")))
			return EMPTY;
		if (key == Rectangle2D.class) {
			String[] tok = tokenize(value, ",");
			if (tok.length == 4) {
				double x = Double.parseDouble(tok[0]);
				double y = Double.parseDouble(tok[1]);
				double w = Double.parseDouble(tok[2]);
				double h = Double.parseDouble(tok[3]);
				return new Rectangle2D.Double(x, y, w, h);
			}
		} else if (key == List.class) { // FIX: Do not assume Points!
			List list = new LinkedList();
			String[] tok = tokenize(value, ",");
			for (int i = 0; i < tok.length; i = i + 2) {
				double x = Double.parseDouble(tok[i]);
				double y = Double.parseDouble(tok[i + 1]);
				AttributeMap dummyMap = new AttributeMap();
				Point2D point = dummyMap.createPoint(x, y);
				list.add(point);
			}
			return list;
		} else if (key == Font.class) {
			String[] tok = tokenize(value, ",");
			if (tok.length == 3) {
				String name = tok[0];
				int size = Integer.parseInt(tok[1]);
				int style = Integer.parseInt(tok[2]);
				return new Font(name, style, size);
			}
		} else if (key == Color.class) {
			String[] tok = tokenize(value, ",");
			if (tok.length == 3) {
				int r = Integer.parseInt(tok[0]);
				int g = Integer.parseInt(tok[1]);
				int b = Integer.parseInt(tok[2]);
				return new Color(r, g, b);
			}
			return new Color(Integer.parseInt(value));
		} else if (key == Point.class) {
			String[] tok = tokenize(value, ",");
			if (tok.length == 2) {
				int x = Integer.parseInt(tok[0]);
				int y = Integer.parseInt(tok[1]);
				return new Point(x, y);
			}
		} else if (key == float[].class) {
			String[] tok = tokenize(value, ",");
			float[] f = new float[tok.length];
			for (int i = 0; i < tok.length; i++)
				f[i] = Float.parseFloat(tok[i]);
			return f;
		} else if (key == Integer.class) {
			return new Integer(value);
		} else if (key == Border.class) {
			String[] tok = tokenize(value, ",");
			if (tok[0].equals("L")) { // LineBorder
				Color c = new Color(Integer.parseInt(tok[1]));
				int thickness = Integer.parseInt(tok[2]);
				return BorderFactory.createLineBorder(c, thickness);
			} else if (tok[0].equals("B")) { // BevelBorder
				int type = Integer.parseInt(tok[1]);
				return BorderFactory.createBevelBorder(type);
			}
			return BorderFactory.createLineBorder(Color.black, 1);
		} else if (key == Boolean.class) {
			return new Boolean(value);
		} else if (key == Float.class) {
			return new Float(value);
		} else if (key == Edge.Routing.class) {
			if (value.equals("simple"))
				return GraphConstants.ROUTING_SIMPLE;
		} else if (key == Icon.class) {
			//TODO make this more path independant
			// should be subdirectory of domains?
			int inx = value.indexOf("resource");
			if (inx != -1) {
				String path = value.substring(inx);
				URL keyURL = HistoryWindow.class.getResource(path);
				if (keyURL != null) {
					ImageIcon keyIcon = new ImageIcon(keyURL);
					return keyIcon;
				} else {
					return null;
				}
			} else {
				// look in domains/icons
				inx = value.indexOf("domains");
				if (inx > -1) {
					String fPath = top.strOCLPath + File.separator + value.substring(inx);
					ImageIcon keyIcon = new ImageIcon(fPath);
					return keyIcon;
				} else {
					return null;
				}
			}
		}
		return value;
	}

	//
	// Cell Factory
	//

	public static DefaultGraphCell createCell(String type, Object userObject) {
		if (userObject instanceof Map)
			userObject = new LHUserObject((Map) userObject);
		if (type.equals("rect"))
			return new DefaultGraphCell(userObject);
		else if (type.equals("transState"))
			return new TransitionCell(userObject,TransitionCell.STATE_TRANSITION);
		else if (type.equals("transValue"))
			return new TransitionCell(userObject,TransitionCell.VALUE_TRANSITION);
		else if (type.equals("transitionEdge"))
			return new TransitionEdge(userObject);
		else if (type.equals("OneWayMergeEdge"))
			return new OneWayMergeEdge(userObject);
		else if (type.equals("TwoWayMergeEdge"))
			return new TwoWayMergeEdge(userObject);
		else if (type.equals("MergeEdge"))
			return new MergeEdge(userObject);
		else if (type.equals("state"))
			return new StateCell(userObject);
		else if (type.equals("textNote"))
			return new TextNote(userObject);
		else if (type.equals("package"))
			return new PackageCell(userObject);
		else if (type.equals("port"))
			return new DefaultPort(userObject);
		else if (type.equals("edge"))
			return new DefaultEdge(userObject);
		else if (type.equals("ProcessCell"))
			return new ProcessCell(userObject);
		else if (type.equals("ProcessEdge"))
			return new ProcessEdge(userObject);
		return null;
	}

	public static String getType(Object cell) {
		if (cell instanceof DefaultPort)
			return "port";
		else if (cell instanceof StateCell)
			return "state";
		else if (cell instanceof PackageCell)
			return "package";
		else if (TransitionCell.isStateTransitionCell(cell))
			return "transState";
		else if (TransitionCell.isValueTransitionCell(cell))
			return "transValue";
		else if (cell instanceof TextNote)
			return "textNote";
		else if (cell instanceof TransitionEdge)
			return "transitionEdge";
		else if (cell instanceof OneWayMergeEdge)
			return "OneWayMergeEdge";
		else if (cell instanceof TwoWayMergeEdge)
			return "TwoWayMergeEdge";
		else if (cell instanceof MergeEdge)
			return "MergeEdge";
		else if (cell instanceof ProcessEdge)
			return "ProcessEdge";
		else if (cell instanceof ProcessCell)
			return "ProcessCell";
		else if (cell instanceof DefaultEdge)
			return "edge";
		return "rect";
	}

}

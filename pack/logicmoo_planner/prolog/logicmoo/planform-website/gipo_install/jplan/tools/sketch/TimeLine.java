/**
 * Time Line Associated with a List Component in TimeLineCanvas.
 * A TimeLine consists of a series of Node objects each of which contain one or more JComponents.
 * A TimeLine is associated with a Y-Position relative to the top of TimeLineCanvas.
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import javax.swing.*;
import java.util.ArrayList;

class TimeLine {
	
	/**
	 * List Component the TimeLine is associated with.
	 */
	private JComponent listComponent;	
	/**
	 * Y-Position of the TimeLine relative to the top of TimeLineCanvas.
	 */
	private int yPos;
	/**
	 * ArrayList containing the Node objects which exist on the TimeLine.
	 */
	private ArrayList nodes = new ArrayList();
	
	/**
	 * Creates a TimeLine object associated with a List Component at the specified Y-Position.
	 * @param listComponent the List Component the TimeLine is associated with.
	 * @param yPos the y-position of the TimeLine relative to the top of TimeLineCanvas.
	 */
	public TimeLine(JComponent listComponent, int yPos) {
		this.listComponent = listComponent;
		this.yPos = yPos;
	}
	
	/**
	 * Returns the Node at the given Time Index on the TimeLine.
	 * @param timeIndex the time index of the Node to be returned.
	 * @return the Node object at the given Time Index on the TimeLine.
	 * @throws NodeDoesNotExistException if no Node exists at the given Time Index.
	 */
	public Node getNode(int timeIndex) throws NodeDoesNotExistException {
		int i;
		/*
		 * check the time index of all nodes on the time line
		 * return the node whos' time index matches the supplied time index
		 */
		for (i=0;i<nodes.size();i++) {
			if (((Node)nodes.get(i)).getTimeIndex() == timeIndex) {
				return (Node)nodes.get(i);
			}
		}
		/*
		 * throw exception if no nodes were found at the supplied time index
		 */
		throw new NodeDoesNotExistException();
	}
	
	/**
	 * Adds a Node object to the TimeLine.
	 * @param node the Node to be added to the TimeLine.
	 */
	public void addNode(Node node) {
		nodes.add(node);
	}
	
	/**
	 * Returns the Y-Position of the TimeLine relative to the top of TimeLineCanvas.
	 * @return the Y-Position of the TimeLine relative to the top of TimeLineCanvas.
	 */
	public int getYPos() {
		return yPos;
	}
	
	/**
	 * Returns the List Component associated with the TimeLine.
	 * @return the List Component associated with the TimeLine.
	 */
	public JComponent getJComponent() {
		return listComponent;
	}
	
	/**
	 * Returns the ArrayList of Nodes on the TimeLine.
	 * @return the ArrayList of Nodes on the TimeLine.
	 */
	 public ArrayList getNodes() {
		return nodes;
	 }
}
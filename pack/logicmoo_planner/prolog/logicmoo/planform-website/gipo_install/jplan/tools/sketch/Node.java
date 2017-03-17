/**
 * Represents a Time Index on a Time Line where one or more JComponents can be placed.
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import javax.swing.*;
import java.util.ArrayList;

public class Node {
	
	/**
	 * Time index of the Node. All JComponents added to the node will have this time index associated with them.
	 */
	private int timeIndex;	
	/**
	 * ArrayList containing the JComponents placed on the Node.
	 */
	private ArrayList components = new ArrayList();
	/**
	 * Number of JComponents currently on the Node.
	 */
	private int componentCount = 0;
	/**
	 * Maximum number of JComponents that can be on a Node.
	 */
	private int maxComponents = 0;
	
	/**
	 * Creates a Node object at the given time index on a TimeLine with the maximum number of
	 * JComponents that can be on the node set to maxCount.
	 * @param timeIndex the time index of the node to be created.
	 * @param maxCount the maximum number of components that can be on the node.
	 */
	public Node(int timeIndex, int maxCount) {
		this.timeIndex = timeIndex;
		maxComponents = maxCount;
	}
	
	/**
	 * Creates a Node object at the given time index on a TimeLine with an initial JComponent
	 * with the maximum number of JComponents that can be on the Node set to maxCount.
	 * @param timeIndex the time index of the node to be created
	 * @param component the initial JComponent to be placed on the node
	 * @param maxCount the maximum number of components that can be on the node
	 */
	public Node(int timeIndex, JComponent component, int maxCount) {
		this.timeIndex = timeIndex;
		maxComponents = maxCount;
		addComponent(component);
	}
	
	/**
	 * Adds a JComponent to the node.
	 * @param component the JComponent to be added.
	 * @throws NodeIsFullException if the node is full.
	 */
	public void addComponent(JComponent component) throws NodeIsFullException {
		if (componentCount == maxComponents) {
			throw new NodeIsFullException();
		} else {
		components.add(component);
		componentCount++;              // increment component count
		}
	}
	
	/**
	 * Returns the time index of the Node on a TimeLine.
	 * @return the time index of the Node on a TimeLine.
	 */
	public int getTimeIndex() {
		return timeIndex;
	}
	
	/**
	 * Returns the number of JComponents on the Node.
	 * @return the number of JComponents on the Node.
	 */
	public int getCount() {
		return componentCount;
	}
	
	/**
	 * Returns the nth JComponent on the Node.
	 * @param componentNumber the index of the JComponent to be returned.
	 * @return the nth JComponent on the Node.
	 */
	public JComponent getComponent(int componentNumber) {
		return (JComponent)components.get(componentNumber - 1);
	}
}
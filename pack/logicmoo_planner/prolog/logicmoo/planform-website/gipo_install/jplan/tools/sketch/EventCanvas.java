/**
 * Event Space Graphical Display Area. 
 * Allows the placement of JComponents representing Actions and Events against a Time Line.
 * Draws dotted lines for Nodes from ProcessCanvas.
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;

public class EventCanvas extends TimeLineCanvas {
	
	/**
	 * ArrayList of TimeLine objects in ProcessCanvas. This is used to draw dotted lines for Nodes which exist in ProcessCanvas.
	 */
	private ArrayList processTimeLines;
	
	/**
	 * Places a component representing an Action or Event. These components are placed centered on the Time Line.
	 * Finds the TimeLine and Node where the component should be placed.
	 * Creates a new Node capable of holding 3 JComponents if no Node exists for the specified time index.
	 * Updates the current minumum time index.
	 * @param componentToBeAdded the JComponent to be placed on the canvas.
	 * @param timeIndex the time index the JComponent is to be placed at.
	 * @param timeLineComponent the List Component associated with the TimeLine where the placed component should reside.
	 */
	public void placeComponent(JComponent componentToBeAdded, int timeIndex, JComponent timeLineComponent) {
		int i;
		int xPos;
		int yPos;                   // y position of the JComponent to be placed
		TimeLine line = null;       // TimeLine object associated with the List Component
		Node node = null;           // Node associated with the supplied x position.
		JComponent tmpComponent;    // component in the node
				
		/*
		 * add the component to the canvas
		 */
		add(componentToBeAdded);
		/*
		 * get the time line for the supplied component representing a domain object
		 */
		 line = getTimeLine(timeLineComponent);
		 /*
		  * get the node at the given time index on the time line or create a new node if no node exists
		  * add the component to the node
		  */
		 try {
			node = line.getNode(timeIndex);	
		 } catch (NodeDoesNotExistException e) {
			node = new Node(timeIndex,3);               // create new node
			line.addNode(node);                  		// add node to the time line
		 } finally {
			try {
				node.addComponent(componentToBeAdded);           // add component to the node
				setCurrentMinIndex(timeIndex);          // update current minimum time index
			} catch (NodeIsFullException e) {
				// do nothing if node is full
			}
		 }
		 /*
		  * get the dimensions of the component to be added
		  */
		 Dimension dim = componentToBeAdded.getPreferredSize();
		 /*
		  * calculate the x position of the component to be added
		  */
		xPos = getIndexPosition(timeIndex) - (dim.width/2);
		 /*
		  * get the number of components in the node
		  */
		 i = node.getCount();
		 /*
		  * calculate the y position of the component to be added
		  * place directly on the time line if no other components are on the node
		  * place above the time line if there is one other component on the node
		  * place below the time line if there are two other components on the node
		  */
		if (i == 1) {
			yPos = line.getYPos() - dim.height/2;
		} else if (i == 2) {
			tmpComponent = node.getComponent(1);
			yPos = line.getYPos() - tmpComponent.getHeight()/2 - NODECOMPONENTGAP - dim.height;
		} else {
			tmpComponent = node.getComponent(1);
			yPos = line.getYPos() + tmpComponent.getHeight()/2 + NODECOMPONENTGAP;
		}
		/*
		 * set the size and position of the component on the event canvas 
		 */
		componentToBeAdded.setBounds(xPos,yPos,dim.width, dim.height);
		repaint();
	}
	
	/**
	 * Sets the reference to the ArrayList of TimeLine objects in ProcessCanvas.
	 * @param processTimeLines the ArrayList of TimeLine objects in ProcessCanvas.
	 */
	public void setProcessTimeLines(ArrayList processTimeLines) {
		this.processTimeLines = processTimeLines;
	}
	
	/**
	 * Overrides drawDottedLines in class TimeLineCanvas.
	 * Draws dotted lines for Nodes on the ProcessCanvas in addition to those on EventCanvas.
	 * @param g2 the EventCanvas graphics context.
	 */
	public void drawDottedLines(Graphics2D g2) {
		super.drawDottedLines(g2);
		int i;		// counting integer
		int j;		// counting integer
		int currentTimeIndex;
		int currentXPos;
		TimeLine currentLine;
		Node currentNode;
		
		g2.setColor(Color.GRAY);
		float dash[] = {5.0f};
		g2.setStroke(new BasicStroke(0.5f,BasicStroke.CAP_BUTT,BasicStroke.JOIN_MITER,5.0f, dash, 0.0f));
		/*
		 * draw dotted lines for all process nodes on all time lines
		 */
		for (i=0;i<processTimeLines.size();i++) {
			currentLine = (TimeLine)processTimeLines.get(i);
			for (j=0;j<currentLine.getNodes().size();j++) {
				currentNode = (Node)(currentLine.getNodes().get(j));
				currentTimeIndex = currentNode.getTimeIndex();
				currentXPos = getIndexPosition(currentTimeIndex);
				g2.drawLine(currentXPos,0,currentXPos,getHeight());
			}
		}
	}
}
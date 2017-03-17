/**
 * Process Space Graphical Display Area.
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import javax.swing.*;
import java.util.ArrayList;
import javax.swing.event.*;


public class ProcessCanvas extends TimeLineCanvas {
	
	/**
	 * ArrayList of the active processes on the canvas. A process is placed in an active state.
	 */
	private ArrayList activeProc = new ArrayList();
	
	/**
	 * Creates a canvas capable of placing Processes on different Time Lines.
	 * Active Processes are extended when the current time index is increased.
	 */
	public ProcessCanvas(EventCanvas evtCanvas) {
		super();
		setLineSpacing(30);
		evtCanvas.setProcessTimeLines(getTimeLines());
	}
	
	/**
	 * Places a Component representing a Process on the canvas at the given Time Index.
	 * The left edge of the Process component is placed at the given time index.
	 * @param componentToBeAdded the JComponent to be placed on the canvas.
	 * @param timeIndex the time index the JComponent is to be placed at.
	 * @param timeLineComponent the List Component associated with the TimeLine where the placed component should reside.
	 */
	public void placeComponent(JComponent componentToBeAdded, int timeIndex, JComponent timeLineComponent) {
		/*
		 * get the height of the Process DomainIcon to be added to the canvas
		 */
		int processJComponentHeight = componentToBeAdded.getPreferredSize().height;
		
		TimeLine line = null;       // TimeLine object associated with the supplied list icon
		Node node = null;           // the node at the supplied time index
		int xPos;                   // position of the left edge of the process icon
		int yPos;                   // position of the top edge of the process icon
		
		/*
		 * add the component to the canvas
		 */
		 add(componentToBeAdded);
		 /*
		  * set the added process as being active
		  */
		 activeProc.add(componentToBeAdded);
		 /*
		  * get the time line for the supplied list icon
		  */
		line = getTimeLine(timeLineComponent);
		/*
		 * get the node at the given time index on the time line or create a new node if no node exists
		 * add the process icon to the node
		 */
		try {
			node = line.getNode(timeIndex);	
		} catch (NodeDoesNotExistException e) {
			node = new Node(timeIndex,1);               // create new node
			line.addNode(node);                  		// add node to the time line
		} finally {
			try {
				node.addComponent(componentToBeAdded);   	    // add component to the node
				setCurrentMinIndex(timeIndex);          // update current minimum time index
				} catch (NodeIsFullException e) {
				}	
		}
		/*
		 * calculate the position of the process icon on the canvas
		 */
		 xPos = getIndexPosition(timeIndex);
		 yPos = line.getYPos() - processJComponentHeight/2;
		 /*
		  * set the size and position of the process icon on the canvas
		  */
		componentToBeAdded.setBounds(xPos,yPos,1, processJComponentHeight);
		repaint();
	}
	
	/**
	 * Sets a Process as being inactive so it is not extended when the time index is increased.
	 * @param processLine the List Component associated with the Process so set as inactive.
	 * @param timeIndex the time index at which to set the Process as inactive.
	 */
	public void stopProcess(JComponent processLine, int timeIndex) {
		TimeLine line = getTimeLine(processLine);
		ArrayList nodes = line.getNodes();
		Node currentNode = null;
		int i;
		for (i=0;i<nodes.size();i++) {
			currentNode = (Node)nodes.get(i);
			activeProc.remove(currentNode.getComponent(1));
		}
		line.addNode(new Node(timeIndex, currentNode.getComponent(1), 1));
		setCurrentMinIndex(timeIndex);
	}
	
	/**
	 * Overrides stateChanged in class TimeSlider.
	 * Invoked when the value of TimeSlider has changed.
	 * Sets the value of the current time index to the value of the slider.
	 * If the sliders value is lower than the current min index, set the slider to current min index.
	 * Extends active processes.
	 * @param e the source of the event.
	 */
	public void stateChanged(ChangeEvent e) {
		super.stateChanged(e);   // call superclass
//		int i;
//		int currentXPos;		// the x position of the current process icon
//		int currentYPos;		// the y position of the current process icon
//		JComponent currentProcIcon;
//		JSlider source = (JSlider)e.getSource();	// the JSlider
//		/*
//		 * extend active processes
//		 */
//		for (i=0;i<activeProc.size();i++) {
//			currentProcIcon = (JComponent)activeProc.get(i);
//			currentXPos = currentProcIcon.getX();
//			currentYPos = currentProcIcon.getY();
//			currentProcIcon.setBounds(currentXPos,currentYPos,
//											getIndexPosition(source.getValue()) - currentXPos,
//											currentProcIcon.getHeight());
//		}
		repaint();
	}
	
	// Ron 15/9/03
	/**
	 * advanceProcess
	 * extend an active process to a new time position
	 * @param - a DrawableObject corresponding to the process
	 */
	public void advanceProcess(DrawableObject dObj,int timePos){
		int currentXPos;		// the x position of the current process icon
		int currentYPos;		// the y position of the current process icon
		boolean found = false;
		int inx = 0;
		while (!found && inx <activeProc.size()) {
			ProcessIcon procIcon = (ProcessIcon)activeProc.get(inx);
			if (procIcon.getDrawableObj().equals(dObj)) {
				found = true;
				currentXPos = procIcon.getX();
				currentYPos = procIcon.getY();
				procIcon.setBounds(currentXPos,currentYPos,
											getIndexPosition(timePos) - currentXPos,
											procIcon.getHeight());
			}
			inx++;
		}
		repaint();
	}
	
	/**
	 * Clears the canvas as in TimeLineCanvas and also clears the list of Active Processes.
	 * Overrides clear in class TimeLineCanvas
	 */
	public void clear() {
		super.clear();
		activeProc.clear();
	}
	
	/**
	 * Returns a List of the Active Processes on the Canvas.
	 * @return a List of the Active Processes on the Canvas.
	 */
	public ArrayList getActiveProc() {
		return activeProc;
	}
}

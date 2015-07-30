/**
 * An abstract class representing a canvas allowing for the placement of JComponents against a Time Line.
 * The canvas consists of a list of JComponents (referred to as List Components) which are placed vertically down the left edge
 * of the canvas to form a list. Each of these List Components are associated with an instance of the class TimeLine.
 * A vertical line (referred to as the Margin Line) is drawn to seperate the List Components from the area where JComponents may
 * be placed on the Time Lines. Dotted lines are drawn from each Node object on each TimeLine to the top of the canvas and a vertical
 * green line indicates the current time position.
 * The canvas implements ChangeListener to listen for changes to objects of class TimeSlider. TimeSlider objects are used to set the value of the
 * current time index.
 * This class is Abstract and cannot be instantiated directly, it must be subclassed and a definition of the method placeComponent
 * must be created. This method determins how JComponents should be placed on a Time Line (for example, how the components are aligned).
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.util.ArrayList;

public abstract class TimeLineCanvas extends JPanel implements ChangeListener {
	
	/**
	 * Constant for the gap between the Margin Line and the start of the Time Lines. This should typically be set to at least
	 * half the width of the JComponents to be added to the Time Lines.
	 */
	public final static int STARTGAP = 15;
	/**
	 * Constant for the width of the list of JComponents. This should be at least the width of the widest List Component to be added
	 * to the canvas.
	 */
	public final static int MARGINWIDTH = 100;
	/**
	 * Constant for the gap between the end of the Time Lines and end of the canvas. This should typically be set to at least
	 * half the width of the JComponents to be added to the Time Lines.
	 */
	public final static int ENDGAP = 15;
	/**
	 * Constant for the vertical distance between JComponents on a Node. This is only needed if more than one JComponent can be
	 * placed at any given time on a particular Time Line.
	 */
	public final static int NODECOMPONENTGAP = 0;
	/**
	 * Number of pixels on the canvas per time index.
	 */
	private int scale = 8;
	/**
	 * Vertical distance between time lines. This value should be set by taking into account the height of the JComponents to be added
	 * to the time lines, the number which can be placed at any given time on a particular time line and the gap between JComponents on
	 * a Node.
	 */
	private int lineSpacing = 60;
	/**
	 * Minimum time index a component may currently be placed on a time line. This value should be updated when a new JComponent is added to a
	 * Time Line to ensure that the canvas will not allow the placement of JComponents at a time with a smaller time index.
	 */
	private int currentMinIndex = 0;
	/**
	 * Time index at the start of the time lines. This should usually be left as zero.
	 */
	private int initialTimeIndex = 0;
	/**
	 * Time index at the end of the time lines.
	 */
	private int maxTimeIndex = 60;
	/**
	 * Current time index.
	 */
	private int currentTimeIndex = 0;
	/**
	 * ArrayList of the List JComponents each TimeLine is associated with.
	 */ 
	private ArrayList listJComponents = new ArrayList();
	/**
	 * ArrayList of the TimeLines on the canvas. There should be one TimeLine object for each List JComponent.
	 */
	private ArrayList timelines = new ArrayList();
	
	/**
	 * Creates a canvas allowing for the placement of JComponents against a Time Line.
	 */
	public TimeLineCanvas() {
		setBackground(Color.white);
		setLayout(null);
	}
	
	/**
	 * Clears the canvas by removing all JComponents which are placed on the canvas, clearing the ArrayLists holding
	 * the List JComponents and TimeLines and setting the current time index to zero.
	 * Calling repaint() updates the canvas to reflect the changes.
	 */
	public void clear() {
		removeAll();
		listJComponents.clear();
		timelines.clear();
		currentMinIndex = 0;
		repaint();
	}
	
	/**
	 * Returns the ArrayList of List JComponents each TimeLine is associated with.
	 * @return the ArrayList of List JComponents each TimeLine is associated with.
	 */
	public ArrayList getListJComponents() {
		return listJComponents;
	}
	
	/**
	 * Sets the current time index.
	 * @param currentTimeIndex the time index to set the current time to.
	 */
	public void setCurrentIndex(int currentTimeIndex) {
		this.currentTimeIndex = currentTimeIndex;
	}
	
	/**
	 * Sets the initial and final time index.
	 * @param initialIndex the initial time index.
	 * @param finalIndex the final time index.
	 */
	public void setIndexRange(int initialIndex, int finalIndex) {
		initialTimeIndex = initialIndex;
		maxTimeIndex = finalIndex;
	}
	
	/**
	 * Returns the position of the supplied time index in pixels relative to the left edge of the canvas.
	 * @param timeIndex the time index from which the position should be calculated.
	 * @return the position of the supplied time index in pixels relative to the left edge of the canvas.
	 */
	public int getIndexPosition(int timeIndex) {
		return (timeIndex-initialTimeIndex)*scale + MARGINWIDTH + STARTGAP;
	}
	
	/**
	 * Returns the time index at the start of the time lines.
	 * @return initialTimeIndex the time index at the start of the time lines.
	 */
	public int getInitialTimeIndex() {
		return initialTimeIndex;
	}
	
	/**
	 * Overrides getPreferredSize in class JComponent. Calculates the preferred size of the canvas by considering the number of
	 * TimeLines and the gap between them and by calculating the number of pixels from the left of the canvas to the end of the TimeLines.
	 * @return the preferred size of the canvas as a Dimension object.
	 */
	public Dimension getPreferredSize() {
		/*
		 * maximum width is the position of the end of the time line plus the end gap
		 */
		 int maxX = getIndexPosition(maxTimeIndex) + ENDGAP;
		 /*
		  * maximum height is the number of time lines times the gap between lines
		  */
		 int maxY = (listJComponents.size() + 1) * lineSpacing;
		return new Dimension(maxX,maxY);
	}
	
	/**
	 * Returns the TimeLine object associated with the supplied List Component.
	 * Assumes that the supplied List Component does exist on the canvas.
	 * @param listJComponent the List Component associated with the TimeLine object to be retrieved.
	 * @return the TimeLine object associated with the supplied List Component.
	 */
	public TimeLine getTimeLine(JComponent listJComponent) {
		int i;
		TimeLine line = null;
		
		for (i=0;i<timelines.size();i++) {
			line = ((TimeLine)getTimeLines().get(i));
			if (line.getJComponent() == listJComponent) {
				break;
			}
		}
		return line;
	}
	
	/**
	 * Adds a Component as a List Component. A TimeLine object associated with the List Component is created.
	 * The List Components are aligned horizontally to the Margin Line.
	 * @param listItem the JComponent to be added as a List Component.
	 */
	public void addListItem(JComponent listItem) {
		/*
		 * y position of the time line to be created relative to the top of the canvas
		 */
		int timeLineYPos;
		/*
		 * add the component to the canvas and the list of JComponents
		 */
		add(listItem);
		listJComponents.add(listItem);
		/*
		 * calculate the y position of the new time line
		 */
		timeLineYPos = listJComponents.size()*lineSpacing;
		/*
		 * create a new TimeLine object and add it to the list of time lines
		 */
		timelines.add(new TimeLine(listItem, timeLineYPos));
		/*
		 * calculate the position of the component on the canvas
		 */
		Dimension dim = listItem.getPreferredSize();
		/*
		 * set the position and size of the component on the canvas
		 */
		listItem.setBounds(MARGINWIDTH-dim.width-10,timeLineYPos - (dim.height/2)
													,dim.width, dim.height);
	}
	
	/**
	 * Places a JComponent on a time line at the specified time index. This must be defined by a subclass of TimeLineCanvas.
	 * The implementation should create a new Node object at the specified TimeLine and Time Index in order for dotted lines to be drawn
	 * correctly. In addition the value of currentMinIndex should be updated to reflect the changes.
	 * @param timeLineComponent the List Component associated with the TimeLine where the placed component should reside.
	 * @param timeIndex the time index the JComponent is to be placed at.
	 * @param componentToBeAdded the JComponent to be placed on the canvas.
	 */
	public abstract void placeComponent(JComponent componentToBeAdded, int timeIndex, JComponent timeLineComponent);
	
	/**
	 * Returns the scale.
	 * @return scale the scale.
	 */
	public int getScale() {
		return scale;
	}
	
	/**
	 * Returns the current minimum time index a JComponent can be placed at.
	 * @return currentMinIndex the current minimum time index.
	 */
	public int getCurrentMinIndex() {
		return currentMinIndex;
	}
	
	/**
	 * Returns the ArrayList of TimeLine objects.
	 * @return timeLines the ArrayList of TimeLine objects.
	 */
	public ArrayList getTimeLines() {
		return timelines;
	}
	
	/**
	 * Sets the current minimum time index a JComponent can be placed at.
	 * @param minIndex the index to set currentMinIndex to.
	 */
	public void setCurrentMinIndex(int minIndex) {
		currentMinIndex = minIndex;
	}
	
	/**
	 * Sets the spacing between time lines. This should only be called before any JComponents have been added to the canvas.
	 * @param newSpacing the new distance between time lines.
	 */
	public void setLineSpacing(int newSpacing) {
		lineSpacing = newSpacing;
	}
	
	/**
	 * Overrides paintComponent in class JComponent.
	 * @param g the graphics context.
	 */
	public synchronized void paintComponent(Graphics g) {
		super.paintComponent(g);
		Graphics2D g2 = (Graphics2D) g;
		drawMargin(g2);
		drawTimeLines(g2);
		drawDottedLines(g2);
		drawNowLine(g2);
	}

	/**
	 * Draws the Margin Line between the List Components and the time lines.
	 * @param g2 the graphics context.
	 */
	private void drawMargin(Graphics2D g2) {
		/*
		 * draw the margin line at x=MARGINWIDTH running the height of the canvas
		 */
		g2.setColor(Color.GRAY);
		g2.setStroke(new BasicStroke(5.0f));
		g2.drawLine(MARGINWIDTH,0,MARGINWIDTH,getHeight());	
	}
	
	/**
	 * Draws the Now Line.
	 * @param g2 the graphics context.
	 */
	private void drawNowLine(Graphics2D g2) {
		/*
		 * line is red if the Now Line is before the last component on the time lines, green otherwise
		 */
		if (currentTimeIndex < currentMinIndex) {
			g2.setColor(new Color(197,16,16));
		} else {
			g2.setColor(new Color(56,163,20));
		}
		g2.setStroke(new BasicStroke(0.5f));
		g2.drawLine(getIndexPosition(currentTimeIndex),0,getIndexPosition(currentTimeIndex),getHeight());
	}
	
	/**
	 * Draws the dotted lines from the Node objects to the top of the canvas. 
	 * @param g2 the graphics context.
	 */
	public void drawDottedLines(Graphics2D g2) {
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
		 * draw dotted lines for all nodes on all time lines
		 */
		for (i=0;i<timelines.size();i++) {
			currentLine = (TimeLine)timelines.get(i);
			for (j=0;j<currentLine.getNodes().size();j++) {
				currentNode = (Node)(currentLine.getNodes().get(j));
				currentTimeIndex = currentNode.getTimeIndex();
				currentXPos = getIndexPosition(currentTimeIndex);
				g2.drawLine(currentXPos,0,currentXPos,currentLine.getYPos());
			}
		}
	}
	
	/**
	 * Draws the time lines for each List Component.
	 * @param g2 the graphics context.
	 */
	private void drawTimeLines(Graphics2D g2) {
		int i;                // for counting
		int currentPos;       // the position of the current time line
		
		g2.setColor(Color.GRAY);
		/*
		 * run through all TimeLine objects drawing time lines for each
		 */
		for (i=0;i<timelines.size();i++) {
			currentPos = ((TimeLine)timelines.get(i)).getYPos();
			g2.setStroke(new BasicStroke(5.0f));
			g2.drawLine(MARGINWIDTH-5,currentPos,MARGINWIDTH,currentPos);
			g2.setStroke(new BasicStroke(0.5f));
			g2.drawLine(MARGINWIDTH,currentPos,getWidth(),currentPos);
		}
	}
	
	/**
	 * Invoked when the value of TimeSlider has changed.
	 * Sets the value of the current time index to the value of the slider.
	 * If the sliders value is lower than the current min index, set the slider to current min index.
	 * @param e the source of the event.
	 */
	public void stateChanged(ChangeEvent e) {
		JSlider source = (JSlider)e.getSource();
		if (!source.getValueIsAdjusting()) {
			if (source.getValue() < currentMinIndex) {
				source.setValue(currentMinIndex);
			}
		}
		setCurrentIndex(source.getValue());
		repaint();
	}
}
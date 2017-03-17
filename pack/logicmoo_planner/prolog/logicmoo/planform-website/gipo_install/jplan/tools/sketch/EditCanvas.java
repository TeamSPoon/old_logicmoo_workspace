/**
 * A class allowing for the placement of Icons representing Actions, Events and Processes
 * on the Event and Process Canvases.
 * Adds ToolTip text to DomainIcons before placing them on the canvases.
 * Creates Popup Menus associated with clicks of DomainIcons.
 * @author Graeme Elliott
 * 7/08/03
 */

package jplan.tools.sketch;



import javax.swing.*;
import java.util.ArrayList;
import java.util.*;
import javax.swing.event.*;
import java.awt.event.*;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.io.*;


// Ron
import jplan.ocl.oclTask;
import jplan.ocl.oclSS;
import jplan.ocl.oclOperator;
import jplan.ocl.oclEvent;
import jplan.ocl.oclProcess;

public class EditCanvas implements ChangeListener,ActionListener {
	
	/**
	 * The list of currently known objects
	 */
	private List objects;  //Ron
	/**
	 * EventCanvas instance to be edited.
	 */
	private EventCanvas evtCanvas;
	/**
	 * ProcessCanvas instance to be edited.
	 */
	private ProcessCanvas procCanvas;
	/**
	 * TimeSlider associated with the Event and Process Canvases.
	 */
	private TimeSlider slider;
	/**
	 * List of Tasks which extend TaskDemo.
	 */
	private ArrayList tasks = new ArrayList();
	/**
	 * Colour index of the next Action or Event to be placed.
	 */
	private int nextColour = 0;
	
	
	/**
	 * Creates an instance of EditCanvas allowing for the placement of Icons representing Actions, Events and Processes
	 * on the Event and Process Canvases.
	 * @param evtCanvas the EventCanvas instance to be edited.
	 * @param procCanvas the ProcessCanvas instance to be edited.
	 * @param slider the TimeSlider associated with the Event and Process Canvases.
	 */
	public EditCanvas(EventCanvas evtCanvas, ProcessCanvas procCanvas, TimeSlider slider) {
		this.evtCanvas = evtCanvas;
		this.procCanvas = procCanvas;
		this.slider = slider;
//		tasks.add(new BathFill());         // add demo tasks to list of tasks.
//		tasks.add(new Gripper());
		slider.addChangeListener(this);
		objects = new ArrayList(); //Ron
	}
	
	/**
	 * Returns the List of Tasks.
	 * @return the List of Tasks.
	 */
	public List getTasks() {
		return tasks;
	}
	
	
	/**
	 * Adds an Action to the EventCanvas at the specified time.
	 * @param action DrawableObject representing the Action to be placed.
	 * @param affectedObjects DrawableObject representing the Domain Objects affected by the Action.
	 * @param time the time index the Action occours at.
	 */
	public void addAction(DrawableObject action, List affectedObjects, int time) {
		int i;
		for (i=0;i<affectedObjects.size();i++) {
			ActionIcon actionIcon = new ActionIcon(action, nextColour);   // create an ActionIcon for each affected object.
			action.setStartTime(time);
			action.setEndTime(time);
			actionIcon.setToolTipText(action.getTitle() + ", " + time + " " + slider.getUnit());      // set tool tip text
			evtCanvas.placeComponent(actionIcon, time, ((DrawableObject)affectedObjects.get(i)).getIcon());
			addDefaultPopup(actionIcon);
		}
		updateNextColour();
	}
	
	/**
	 * Adds an Event to the EventCanvas at the specified time.
	 * @param event DrawableObject representing the Event to be placed.
	 * @param affectedObjects DrawableObject representing the Domain Objects affected by the Event.
	 * @param time the time index the Event occours at.
	 */
	public void addEvent(DrawableObject event, List affectedObjects, int time) {
		int i;
		for (i=0;i<affectedObjects.size();i++) {
			EventIcon eventIcon = new EventIcon(event, nextColour);    // create an EventIcon for each affected object.
			event.setStartTime(time);
			event.setEndTime(time);
			eventIcon.setToolTipText(event.getTitle() + ", " + time + " " + slider.getUnit());      // set tool tip text
			evtCanvas.placeComponent(eventIcon, time, ((DrawableObject)affectedObjects.get(i)).getIcon());
			addDefaultPopup(eventIcon);
		}
		updateNextColour();
	}
	
	/**
	 * Adds an Action to the EventCanvas at the time provided by the Slider.
	 * @param action DrawableObject representing the Action to be placed.
	 * @param affectedObjects DrawableObject representing the Domain Objects affected by the Action.
	 */
	public void addAction(DrawableObject action, List affectedObjects) {
		addAction(action, affectedObjects, slider.getValue());
	}
	
	/**
	 * Adds an Event to the EventCanvas at the time provided by the Slider.
	 * @param event DrawableObject representing the Event to be placed.
	 * @param affectedObjects DrawableObject representing the Domain Objects affected by the Event.
	 */
	public void addEvent(DrawableObject event, List affectedObjects) {
		addEvent(event, affectedObjects, slider.getValue());
	}
	
	/**
	 * Starts a Process on the Process Canvas at the specified time.
	 * @param processLine Drawable Object representing the Domain Process to be started.
	 * @param time the time the Process starts.
	 */
	public void startProcess(DrawableObject processLine, int time) {
		ProcessIcon procIcon = new ProcessIcon(processLine, DomainIcon.GREEN);
		procIcon.getDrawableObj().setStartTime(time);
		procCanvas.placeComponent(procIcon, time, processLine.getIcon());
		addProcessPopup(procIcon);
	}
	
	/**
	 * Starts a Process on the Process Canvas at the time provided by the Slider.
	 * @param processLine Drawable Object representing the Domain Process to be started.
	 */
	public void startProcess(DrawableObject processLine) {
		startProcess(processLine, slider.getValue());
	}
	
	/**
	 * Stops a Process on the Process Canvas.
	 * @param processLine Drawable Object representing the Domain Process to be stopped.
	 */
	public void stopProcess(DrawableObject processLine) {
		procCanvas.stopProcess(processLine.getIcon(), slider.getValue());
	}
	
	/**
	 * Stops a Process on the Process Canvas.
	 * @param processLine Drawable Object representing the Domain Process to be stopped.
	 */
		public void stopProcess(DrawableObject processLine,int time) {
			procCanvas.stopProcess(processLine.getIcon(), time);
		}
	
	/**
	 * Stops all Processes represented in the supplied List.
	 * @param processLines List of Drawable Objects representing the Domain Process to be stopped.
	 */
	public void batchStopProcess(List processLines) {
		int i;
		for (i=0;i<processLines.size();i++) {
			stopProcess((DrawableObject)processLines.get(i));
		}
	}
	
	/**
	 * Starts all Processes represented in the supplied List.
	 * @param processLines List of Drawable Objects representing the Domain Process to be started.
	 */
	public void batchStartProcess(List processLines) {
		int i;
		for (i=0;i<processLines.size();i++) {
			startProcess((DrawableObject)processLines.get(i));
		}
	}
	
	// Ron 15/9/03
	/**
	 * advanceProcess
	 * extend an active process to a new time position
	 * @param - a DrawableObject corresponding to the process
	 */
	public void advanceProcess(DrawableObject dObj,int timePos){
		procCanvas.advanceProcess(dObj,timePos);
	}
	
	/**
	 * Adds a Domain Object to the list on the Event Canvas.
	 * @param drawableObject Drawable Object representing the Domain Object to be added to the list.
	 */
	public void addObject(DrawableObject drawableObject) {
		ObjectIcon icon = new ObjectIcon(drawableObject);
		drawableObject.setIcon(icon);
		evtCanvas.addListItem(icon);
		objects.add(drawableObject); //Ron
	}
	
	// Ron
	/**
	 * getObjects
	 * @return the known objects
	 */
	public List getObjects(){
		return objects;
	}
	
	/**
	 * Adds the Domain Objects in the supplied List to the list on the Event Canvas.
	 * @param drawableObjects List of Drawable Objects representing the Domain Object to be added to the list.
	 */
	public void batchAddObject(List drawableObjects) {
		int i;
		for (i=0;i<drawableObjects.size();i++) {
			addObject((DrawableObject)drawableObjects.get(i));
		}
	}
	
	/**
	 * Adds a Process Line to the list on the Process Canvas.
	 * @param process Drawable Object representing the Process Line to be added to the list.
	 */
	public void addProcessLine(DrawableObject process) {
		ObjectIcon icon = new ObjectIcon(process);
		process.setIcon(icon);
		procCanvas.addListItem(icon);
	}
	
	/**
	 * Adds the Process Lines in the supplied List to the list on the Process Canvas.
	 * @param process List of Drawable Objects representing the Process Lines to be added to the list.
	 */
	public void batchAddProcessLine(List processLines) {
		int i;
		for (i=0;i<processLines.size();i++) {
			addProcessLine((DrawableObject)processLines.get(i));
		}
	}
	
	/**
	 * Sets the time of the Event Canvas, Process Canvas and Slider to the supplied value.
	 * @param time the time to set the system to.
	 */
	public void setTime(int time) {
		slider.setValue(time);
	}
	
	/**
	 * Clears the Event Canvas, Process Canvas and Slider.
	 */
	public void clearAll() {
		evtCanvas.clear();
		procCanvas.clear();
		slider.resetSlider();
	}
	
	/**
	 * Loads a task.
	 * @param demo the task to be loaded.
	 */
	public void loadTaskDemo(oclTask demo) {
		List objs = new ArrayList();
		clearAll();
		ListIterator li = demo.getInits().listIterator();
		while (li.hasNext()) {
			oclSS clause = (oclSS)li.next();
			TaskObject obj = new TaskObject(clause.getName(),clause.getSort());
			objs.add(obj);
		}
		batchAddObject(objs);
//		batchAddProcessLine(demo.getProcesses());
	}
	
	/**
	 * Increments the next colour of an Action or Event Icon.
	 * If all colours have been used, wrap around to the first colour in DomainIcon.
	 */
	private void updateNextColour() {
		nextColour++;
		if (nextColour == DomainIcon.COLOURCOUNT) {
			nextColour = 0;
		}
	}
	
	// Ron add abilitly to have other listeners added to time slider
	/**
	 * addTimeListener
	 * add abilitly to have other listeners added to time slider
	 * @param - a change listener
	 */
	public void addTimeListener(ChangeListener listen) {
		slider.addChangeListener(listen);
	}
	
	/**
	 * Invoked when the value of the Slider is changing.
	 * Updates the end time of any active processes on the Process Canvas.
	 * Updates the tool tip for Process Icons.
	 * @param e the source of the event.
	 */
	public void stateChanged(ChangeEvent e) {
		TimeSlider source = (TimeSlider)e.getSource();
		ArrayList active = procCanvas.getActiveProc();
		ProcessIcon currentIcon = null;
		String unit = source.getUnit();
		int i;
		for (i=0;i<active.size();i++) {
			currentIcon = (ProcessIcon)active.get(i);
			currentIcon.getDrawableObj().setEndTime(source.getValue());
			currentIcon.setToolTipText("Start " + currentIcon.getDrawableObj().getStartTime() + " " + unit
										+ ", End " + currentIcon.getDrawableObj().getEndTime() + " " + unit
										+ ", Elapsed " + (currentIcon.getDrawableObj().getEndTime() -
											currentIcon.getDrawableObj().getStartTime()) + " " + unit);
		}
	}
	
	/**
	 * Invoked when a Menu Item that is part of a Popup is clicked.
	 * Shows the View Details dialog or Stops a process depending on which Menu Item is clicked.
	 * @param e the source of the event.
	 */
	public void actionPerformed(ActionEvent e) {
		MyMenuItem source = (MyMenuItem)e.getSource();
		if (source.getText() == "View Details...") {
			ViewDetails details = new ViewDetails(source.getDomainIcon().getDrawableObj(),ViewDetails.ACTION);
			details.show();
		} else if (source.getText() == "View State Before") {
			ViewDetails details = new ViewDetails(source.getDomainIcon().getDrawableObj(),ViewDetails.STATE);
			details.show();
		} else if (source.getText() == "View State After") {
			ViewDetails details = new ViewDetails(source.getDomainIcon().getDrawableObj(),ViewDetails.STATEAFTER);
			details.show();
		} else {
			this.stopProcess(source.getDomainIcon().getDrawableObj());
		}
	}
	
	/**
	 * Creates a basic Popup with one option, "View Details".
	 * Adds ActionListener to the Menu Item and a MouseListener to the Popup
	 * @param icon the DomainIcon which triggers the Popup.
	 * @return the basic Popup.
	 */
	private JPopupMenu addDefaultPopup(DomainIcon icon) {
		// create the default popup
		JPopupMenu popup = new JPopupMenu();
		MyMenuItem menuItem = new MyMenuItem("View Details...", icon);
		menuItem.addActionListener(this);
		popup.add(menuItem);
		MyMenuItem menuItem2 = new MyMenuItem("View State Before", icon);
		menuItem2.addActionListener(this);
		popup.add(menuItem2);
		MyMenuItem menuItem3 = new MyMenuItem("View State After", icon);
		menuItem3.addActionListener(this);
		popup.add(menuItem3);
		//Add listener to the JComponent so the popup menu can come up
		MouseListener popupListener = new PopupListener(popup);
		icon.addMouseListener(popupListener);
		return popup;
	}
	
	/**
	 * Extends addDefaultPopup to create a Popup with two options, "View Details" and "Stop Process".
	 * @param icon the DomainIcon which triggers the Popup.
	 * @return the popup.
	 */
	private JPopupMenu addProcessPopup(ProcessIcon icon) {
		JPopupMenu popup = new JPopupMenu();
		MyMenuItem menuItem = new MyMenuItem("View Details...", icon);
		menuItem.addActionListener(this);
		popup.add(menuItem);
		MyMenuItem menuItem2 = new MyMenuItem("View State Before", icon);
		menuItem2.addActionListener(this);
		popup.add(menuItem2);
		menuItem = new MyMenuItem("Stop Process...", icon);
		menuItem.addActionListener(this);
		popup.add(menuItem);
//		Add listener to the JComponent so the popup menu can come up
		MouseListener popupListener = new PopupListener(popup);
		icon.addMouseListener(popupListener);
		return popup;
	}
}

/**
 * Adds a MouseListener to a Popup.
 * Shows the Popup if the MouseEvent is found to be a Popup Trigger.
 * @author Graeme Elliott
 * 7/08/03
 */
class PopupListener extends MouseAdapter {
	/**
	 * The PopupMenu this MouseListener shows.
	 */
	JPopupMenu popup;
	
	/**
	 * Creates a PopupListener with the supplied PopupMenu.
	 * @param popupMenu
	 */
	public PopupListener(JPopupMenu popupMenu) {
		popup = popupMenu;
	}
	
	/**
	 * Invoked when the mouse button is pressed on the component.
	 * param e The MouseEvent.
	 */
	public void mousePressed(MouseEvent e) {
		maybeShowPopup(e);
	}

	/**
	 * Invoked when the mouse button is released on the component.
	 * param e The MouseEvent.
	 */
	public void mouseReleased(MouseEvent e) {
		maybeShowPopup(e);
	}
	
	/**
	 * Shows the popup if the MouseEvent is found to be a Popup Trigger.
	 * @param e The MouseEvent.
	 */
	private void maybeShowPopup(MouseEvent e) {
		if (e.isPopupTrigger()) {
			popup.show(e.getComponent(),
					   e.getX(), e.getY());
		}
	}
}

/**
 * Class which associates a JMenuItem with a DomainIcon.
 * @author Graeme Elliott
 * 7/08/03
 */
class MyMenuItem extends JMenuItem {
	
	/**
	 * The DomainIcon associated with the JMenuItem.
	 */
	DomainIcon icon;
	
	/**
	 * Creates a JMenuItem with the given text associated with a DomainIcon.
	 * @param text the text of the JMenuItem.
	 * @param icon the DomainIcon to associate the JMenuItem with.
	 */
	public MyMenuItem(String text, DomainIcon icon) {
		super(text);	
		this.icon = icon;
	}
	
	/**
	 * Returns the DomainIcon associated with the JMenuItem.
	 * @return the DomainIcon associated with the JMenuItem.
	 */
	public DomainIcon getDomainIcon() {
		return icon;
	}
}

/**
 * JDialog which can show the details of a DrawableObject.
 * @author Graeme Elliott
 * 7/08/03
 */
class ViewDetails extends JDialog implements ActionListener {
	private JTextArea detailArea = null;
	public final static int STATE = 1;
	public final static int ACTION = 2;
	public final static int STATEAFTER = 3;
	
	
	/**
	 * Creates a dialog based on the supplied DrawableObject.
	 * @param obj the DrawableObject to base the dialog on.
	 */
	public ViewDetails(DrawableObject obj,int kind) {
		super();            // call JDialog constructor
		setTitle("View Details...");
		setLocation(400,300);            // should change location calculation
		
		JPanel details = new JPanel();          // panel to hold the details of the DrawableObject
		details.setLayout(new GridLayout(2,2));
		JLabel nameLabel = new JLabel("Name: ");
		JLabel timeLabel = new JLabel("Time: ");
		JLabel name = new JLabel(obj.getTitle());
		JLabel time = new JLabel(String.valueOf(obj.getStartTime()));
		
		details.add(nameLabel);               // add labels to panel
		details.add(name);
		details.add(timeLabel);
		details.add(time);
		
		detailArea = new javax.swing.JTextArea("detailedArea");	
		detailArea.setLineWrap(true);
		detailArea.setEditable(false);
		detailArea.setSize(500, 300); /* Weihong changed on 10/10/2001 */
		detailArea.setFont(
			new java.awt.Font("Arial", java.awt.Font.PLAIN, 11));
		detailArea.setBorder(new javax.swing.border.BevelBorder(1));
		StringWriter opDetail = new StringWriter();
		if (kind == ACTION) {
			Object object = ((TaskObject)obj).getObject();
			if (object.getClass().getName().equals("jplan.ocl.oclOperator")) {
				oclOperator operator = (oclOperator)object;
				operator.oclPrintComponent(new PrintWriter(opDetail), 0, false);
				detailArea.setText(new String(opDetail.getBuffer()));
			} else if (object.getClass().getName().equals("jplan.ocl.oclEvent")) {
				oclEvent evnt = (oclEvent)object;
				evnt.oclPrintComponent(new PrintWriter(opDetail), 0, false);
				detailArea.setText(new String(opDetail.getBuffer()));
			} else if (object.getClass().getName().equals("jplan.ocl.oclProcess")) {
				oclProcess proc = (oclProcess)object;
				proc.oclPrintComponent(new PrintWriter(opDetail), 0, false);
				detailArea.setText(new String(opDetail.getBuffer()));
			}
		} else if (kind == STATE) {
			List state = ((TaskObject)obj).getStateList();
			ListIterator li = state.listIterator();
			while (li.hasNext()) {
				oclSS ss = (oclSS)li.next();
				PrintWriter pw = new PrintWriter(opDetail);
				ss.oclPrintComponent(pw,0,false);
				if (li.hasNext()) {
					pw.print("\n");
				}
			}
			detailArea.setText(new String(opDetail.getBuffer()));
		} else if (kind == STATEAFTER) {
			List state = ((TaskObject)obj).getResultList();
			ListIterator li = state.listIterator();
			while (li.hasNext()) {
				oclSS ss = (oclSS)li.next();
				PrintWriter pw = new PrintWriter(opDetail);
				ss.oclPrintComponent(pw,0,false);
				if (li.hasNext()) {
					pw.print("\n");
				}
			}
			detailArea.setText(new String(opDetail.getBuffer()));
		}
		
		JPanel command = new JPanel();          // panel to hold the OK button
		command.setBorder(BorderFactory.createRaisedBevelBorder()); 
		JButton cmdOK = new JButton("OK");
		cmdOK.addActionListener(this);
		command.add(cmdOK);
		
		getContentPane().add(details, BorderLayout.NORTH);
		getContentPane().add(detailArea,BorderLayout.CENTER);
		getContentPane().add(command, BorderLayout.SOUTH);
		pack();
	}
	
	
	/**
	 * Invoked when the OK button is clicked.
	 * Closes the Dialog.
	 * @param e the ActionEvent.
	 */
	public void actionPerformed(ActionEvent e) {
		dispose();
	}
}

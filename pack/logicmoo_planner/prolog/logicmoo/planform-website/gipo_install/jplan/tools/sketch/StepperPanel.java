/**
 * An extension of JPanel which incorporates an Event Canvas, Process Canvas and Time Slider.
 * Vertical Scroll Panes exist for the two canvas and a single horizontal Scroll Bar exists to scroll the system horizontally.
 * The size and position of the TimeSlider are calculated in such a way that the length of the track of the Slider is equal to
 * the length of the time lines on the Event and Process Canvases. In addition the start of the track is set to the same position as
 * the start of the time lines on the Event and Process Canvases.
 * An instance of EditCanvas is created and made available to other objects by the getEditCavas method.
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;


import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;

public class StepperPanel extends JPanel implements ChangeListener{
	
	/**
	 * Instance of EditCanvas associated with the Event Canvas, Process Canvas and Time Slider.
	 */
	EditCanvas editCanvas;
	/**
	 * Models for the Slider Panel horizontal scroll bar and the EventCanvas horizontal scroll bar.
	 */
	private BoundedRangeModel sliderModel,evtCanvasModel;
	
	public StepperPanel() {
		/*
		* create the event and process canvases and their respective scroll panes
		*/
		EventCanvas evtCanvas = new EventCanvas();
		JScrollPane evtCanvasScrl = new JScrollPane(evtCanvas, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
													JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		evtCanvasModel = evtCanvasScrl.getHorizontalScrollBar().getModel();
		ProcessCanvas procCanvas = new ProcessCanvas(evtCanvas);
		JScrollPane procCanvasScrl = new JScrollPane(procCanvas, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
													JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		JScrollBar  horizProcessBar = procCanvasScrl.getHorizontalScrollBar();
		horizProcessBar.getModel().addChangeListener(this);         // listen for changes to horizontal process bar
		/*
		 * create the time slider,time slider panel and scroll pane for the slider panel
		 */
		TimeSlider slider = new TimeSlider(evtCanvas,procCanvas);
		JPanel sliderPanel = new JPanel();
		sliderPanel.setLayout(null);
		sliderPanel.add(slider);
		sliderPanel.setPreferredSize(new Dimension(evtCanvas.getPreferredSize().width,45));
		JScrollPane sliderScrl = new JScrollPane(sliderPanel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
												JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		sliderModel = sliderScrl.getHorizontalScrollBar().getModel();
		/*
		 * position slider on slider panel
		 * match width of slider to width of  time lines on event canvas
		 */
		int xPos = EventCanvas.MARGINWIDTH + EventCanvas.STARTGAP - 5;
		slider.setBounds(xPos,0,evtCanvas.getPreferredSize().width - EventCanvas.MARGINWIDTH
															 - EventCanvas.STARTGAP - EventCanvas.ENDGAP + 13,45);
		/*
		 * create split pane for the event and process canvas
		 * event canvas on the top, process canvas on the bottom
		 */
		JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,true,evtCanvasScrl,procCanvasScrl);
		/*
		 * add components to the panel
		 */
		setLayout(new BorderLayout());
		add(sliderScrl, BorderLayout.NORTH);
		add(splitPane, BorderLayout.CENTER);
		/*
		 * create an instance of EditCanvas
		 */
		this.editCanvas = new EditCanvas(evtCanvas, procCanvas, slider);
	}
	
	/**
	 * Invoked when the state of the horizontal scroll bar on the ProcessCanvas changes.
	 * Sets the value of the horizontal scroll bars on the EventCanvas and Slider to the value of the
	 * horizontal scroll bar on the ProcessCanvas.
	 * @param e the ChangeEvent object
	 */
	public void stateChanged(ChangeEvent e) {
		BoundedRangeModel sourceModel = (BoundedRangeModel) e.getSource();
	  	int sourceDiff  = sourceModel.getMaximum() - sourceModel.getMinimum();
	  	int destDiff  = evtCanvasModel.getMaximum() - evtCanvasModel.getMinimum();
	  	int destValue = sourceModel.getValue();

	  	if (sourceDiff != destDiff) {
	   		destValue   = (destDiff * sourceModel.getValue())/sourceDiff;
	  	}
	  	evtCanvasModel.setValue(destValue);
	  	sliderModel.setValue(destValue);
	}
	 
	 /**
	  * Returns the EditCanvas object associated with the Event Canvas, Process Canvas and TimeSlider that exist on the JPanel.
	  * @return the EditCanvas object associated with the Event Canvas, Process Canvas and TimeSlider.
	  */
	 public EditCanvas getEditCanvas() {
	 	return editCanvas;
	 }
}

///////////////
package UserInterface; 
/*******************************************************************************
 * Copyright (c) 2003, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/


import jpl.Query;
import java.util.*;

import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.ScrollBar;


import Main.frolog;
import NLPModules.Parsing.*;
import NLPModules.ReferenceResolution.*;
import NLPModules.Actions.*;
import NLPModules.ContentDetermination.*;
import NLPModules.ReferenceGeneration.CompleteUtterances;
import NLPModules.Realization.*;
import de.tuebingen.tag.*;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.TabItem;

import Tools.*;
import org.eclipse.swt.widgets.Menu;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;

import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.events.*;


import java.io.*;
import org.eclipse.swt.widgets.MenuItem;


public class PlayerInterface {
	
	public enum Exc {ambiguous,not_relevant,racer_error,no_parse, unknown_word, no_ref_for, no_ref_for_pronoun, reference_ambig, missing_action}

	public static final String APP_TITLE = "Frolog";

	private static org.eclipse.swt.widgets.Shell sShell = null;  //  @jve:decl-index=0:visual-constraint="10,10"
	private static org.eclipse.swt.widgets.Display display = null;
	private Text locationText = null;

//	private static Label statusText = null;

	private static ProgressBar progressBar = null;

	private Button refreshButton = null;

	private StyledText styledText = null;
	
	public static TabFolder tabFolder = null;
	public static ScrolledComposite composite = null;
	public static Canvas canvas = null;
	public static Image image = null;
	
	public static Text kbmodel = null;
	public static Text parsing = null;
	public static Text reference_resolution = null;
	public static Text actions = null;
	public static Text content_determination = null;
	public static Text reference_generation = null;
	public static Text realization = null;
	public static Text racer_log = null;
	public static Text planner_log = null;
	
	public static Color blue;

	private Text text = null;

	Menu menuBar, fileMenu, accommodationMenu, hintMenu;

	MenuItem fileMenuHeader, accommodationMenuHeader, hintMenuHeader;

	MenuItem fileExitItem, fileSaveItem, helpGetHelpItem, hintItem;
	
	public static MenuItem item1, item2, item3, item4;
	
	public static Command command;
	public static List<Literal> instantiated_reading;  
	public static boolean underspecified_command = false;
	
	//  @jve:decl-index=0:

	public static void startInterface() {
		/* Before this is run, be sure to set up the following in the launch configuration 
		 * (Arguments->VM Arguments) for the correct SWT library path. 
		 * The following is a windows example:
		 * -Djava.library.path="installation_directory\plugins\org.eclipse.swt.win32_3.0.0\os\win32\x86"
		 */
		display = org.eclipse.swt.widgets.Display
				.getDefault();
		PlayerInterface thisClass = new PlayerInterface();
		thisClass.createSShell();
		thisClass.sShell.open();
		
		blue = display.getSystemColor(SWT.COLOR_BLUE);
		

		while (!thisClass.sShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	/**
	 * This method initializes sShell
	 */
	private void createSShell() {
		sShell = new org.eclipse.swt.widgets.Shell();
		refreshButton = new Button(sShell, SWT.NONE);
		locationText = new Text(sShell, SWT.BORDER);
		progressBar = new ProgressBar(sShell, SWT.BORDER);
//		statusText = new Label(sShell, SWT.NONE);
		styledText = new StyledText(sShell, SWT.MULTI | SWT.WRAP | SWT.V_SCROLL | SWT.BORDER);
		styledText.setBounds(new Rectangle(9, 29, 959, 272));
		styledText.setEditable(false);
	    progressBar.setMinimum(0);
	    progressBar.setMaximum(7);
		
		menuBar = new Menu(sShell, SWT.BAR);
	    fileMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
	    fileMenuHeader.setText("&File");

	    fileMenu = new Menu(sShell, SWT.DROP_DOWN);
	    fileMenuHeader.setMenu(fileMenu);

	    fileSaveItem = new MenuItem(fileMenu, SWT.PUSH);
	    fileSaveItem.setText("&Save game");

	    fileExitItem = new MenuItem(fileMenu, SWT.PUSH);
	    fileExitItem.setText("E&xit");

	    accommodationMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
	    accommodationMenuHeader.setText("&Accommodation");

	    accommodationMenu = new Menu(sShell, SWT.DROP_DOWN);
	    accommodationMenuHeader.setMenu(accommodationMenu);

	    // Create the radio items
	    item1 = new MenuItem(accommodationMenu, SWT.RADIO);
	    item1.setText("Do not accommodate");
//	    item2 = new MenuItem(accommodationMenu, SWT.RADIO);
//	    item2.setText("Accommodate 1 action");
//	    item3 = new MenuItem(accommodationMenu, SWT.RADIO);
//	    item3.setText("Accommodate 2 actions");
	    item4 = new MenuItem(accommodationMenu, SWT.RADIO);
	    item4.setText("Accommodate");
	    item1.setSelection(true);
	    
	    hintMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
	    hintMenuHeader.setText("&Hint");

	    hintMenu = new Menu(sShell, SWT.DROP_DOWN);
	    hintMenuHeader.setMenu(hintMenu);

	    // Create the radio items
	    hintItem = new MenuItem(hintMenu, SWT.PUSH);
	    hintItem.setText("Give me a hint");
   
	    fileExitItem.addSelectionListener(new SelectionListener() {
	    	public void widgetSelected(SelectionEvent event) {
	  	      sShell.close();
	  	      display.dispose();
	  	    }
		    public void widgetDefaultSelected(SelectionEvent event) {
			      sShell.close();
			      display.dispose();
		    }
	    });
	    fileSaveItem.addSelectionListener(new SelectionListener(){
		    public void widgetSelected(SelectionEvent event) {

				FileDialog dialog = new FileDialog(sShell, SWT.SAVE);
				String result = dialog.open();
				if (result != null) {
					File f = new File(result);
					try {
						BufferedWriter bw = new BufferedWriter(new FileWriter(f));
						String text = styledText.getText();
						bw.write(text);
						bw.close();
					} catch (FileNotFoundException e1) {
						e1.printStackTrace();
					} catch (IOException e1) {
						e1.printStackTrace();
					}
				}
		    	
//		    	statusText.setText("Saved");
		    }

		    public void widgetDefaultSelected(SelectionEvent event) {
//		    	statusText.setText("Saved");
		    }
	    });
	    hintItem.addSelectionListener(new SelectionListener(){
		    
	    	public void widgetSelected(SelectionEvent event) {
		    	
	    		try{
//	    			styledText.append("Frolog: I'm thinking ...\n");
	    			progressBar.setSelection(2);
	    			List<Literal> hint = find_hint();
			    	Utterance utterance = new Utterance(hint,"I suggest that ");
			    	List<Utterance> list_utterance = new LinkedList<Utterance>();
			    	list_utterance.add(utterance);
			    	Utterances utterances = new Utterances(list_utterance);
					
			    	progressBar.setSelection(6);
					//begin Reference Generation
					PlayerInterface.reference_generation.append("\n~~~~~~~~ (hinting) \n");
				
					CompleteUtterances complete_utterances = new CompleteUtterances(utterances);
				
					PlayerInterface.reference_generation.append(complete_utterances.toString()); 
					//end Reference Generation
					
					progressBar.setSelection(7);
					//begin Generation
					PlayerInterface.realization.append("\n~~~~~~~~ (hinting) \n");
					Generation surface_utterances = new Generation(complete_utterances);
					PlayerInterface.realization.append(complete_utterances.toStringforGeneration()); 
					//end Generation
					
					styledText.append(surface_utterances.toString());
		    	
	    		}catch (Exception e){
					//This exception will never happen
				}
		    }

		    public void widgetDefaultSelected(SelectionEvent event) {
		    }
		});
	    sShell.setMenuBar(menuBar);
		
	    tabFolder = new TabFolder(sShell, SWT.BORDER);
	    tabFolder.setBounds(new Rectangle(16, 344, 952, 338));  
	    
	    // Tab Parsing
	    TabItem tabItem1 = new TabItem(tabFolder, SWT.NULL);
	    tabItem1.setText("Parsing");
        parsing = new Text(tabFolder, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP);
        parsing.setEditable(false);
	    tabItem1.setControl(parsing);
	    
	    // Tab Reference Resolution
	    TabItem tabItem2 = new TabItem(tabFolder, SWT.NULL);
	    tabItem2.setText("Reference Resolution");
        reference_resolution = new Text(tabFolder, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP);
        reference_resolution.setEditable(false);
	    tabItem2.setControl(reference_resolution);
	    
	    // Tab Actions
	    TabItem tabItem3 = new TabItem(tabFolder, SWT.NULL);
	    tabItem3.setText("Actions");
        actions = new Text(tabFolder, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP);
        actions.setEditable(false);
	    tabItem3.setControl(actions);
	    
	    // Tab Content Determination
	    TabItem tabItem4 = new TabItem(tabFolder, SWT.NULL);
	    tabItem4.setText("Content Determination");
        content_determination = new Text(tabFolder, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP);
        content_determination.setEditable(false);
	    tabItem4.setControl(content_determination);
	    
	    // Tab Reference Generation
	    TabItem tabItem5 = new TabItem(tabFolder, SWT.NULL);
	    tabItem5.setText("Reference Generation");
        reference_generation = new Text(tabFolder, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP);
        reference_generation.setEditable(false);
	    tabItem5.setControl(reference_generation);
	    
	    // Tab Realization
	    TabItem tabItem6 = new TabItem(tabFolder, SWT.NULL);
	    tabItem6.setText("Realization");
        realization = new Text(tabFolder, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP);
        realization.setEditable(false);
	    tabItem6.setControl(realization);
	    
	    // Tab Racer Log
//	    TabItem tabItem7 = new TabItem(tabFolder, SWT.NULL);
//	    tabItem7.setText("Racer Log");
//	    racer_log = new Text(tabFolder, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP);
//        racer_log.setEditable(false);
//	    tabItem7.setControl(racer_log);
	    
	    // Planner Log
	    TabItem tabItem8 = new TabItem(tabFolder, SWT.NULL);
	    tabItem8.setText("Planner Log");
	    planner_log = new Text(tabFolder, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP);
        planner_log.setEditable(false);
	    tabItem8.setControl(planner_log);

		sShell.setText(APP_TITLE);
		sShell.setLayout(null);
		locationText.setToolTipText("Enter a command");
		locationText.setBounds(new Rectangle(100, 305, 867, 27));
//		statusText.setText("");
//		statusText.setBounds(new Rectangle(569, 5, 191, 17));
		progressBar.setEnabled(false);
		progressBar.setBounds(new Rectangle(762, 4, 206, 20));
		progressBar.setSelection(0);
		refreshButton.setText("Context");
		refreshButton.setBounds(new Rectangle(11, 305, 86, 29));
//		refreshButton.setToolTipText("You don't know what to do?");
		refreshButton.addSelectionListener(new SelectionAdapter() {
		     public void widgetSelected(SelectionEvent event) {
		    	 reloadKBModel();
		    	 open_KBmodel();
		     }
		});

//        try {   
//            GeniSocket geniSocket = new GeniSocket();
//            String answer = geniSocket.generate("A:seated(myself)[n0A1 | adjective] A:and(x1) A:red(myself)[adjective] B:and(x1) A:alive(myself)[adjective] A:you(myself)").toString();
//            System.out.println(answer);
//            } catch (Exception ex) {
//                    System.err.println(ex);
//            } 
		
		sShell.setSize(new Point(989, 744));
		
		locationText.addKeyListener(new org.eclipse.swt.events.KeyAdapter() {
			public void keyPressed(org.eclipse.swt.events.KeyEvent e) {
				// Handle the press of the Enter key in the locationText.
				// 
				if (e.character == SWT.LF || e.character == SWT.CR) {
					e.doit = false;
					String playerInput = locationText.getText();
					String playerText = "Player: " + playerInput + "\n";
					StyleRange styleRange = new StyleRange();
					styleRange.start = styledText.getText().length();
					styleRange.length = playerText.length();
					styleRange.foreground = blue;
					
					styledText.append(playerText);
					
					styledText.setStyleRange(styleRange);
					
					locationText.setText("");
					String frologAnswer = getanswer(playerInput);
					      
					if (!underspecified_command){
						if (command.decision.equals(Command.Decision.failed)  && (!item1.getSelection())){
//							if the command failed and the radio button "Do not accommodate" is not selected then find plan
							String tacit_acts = accommodate(command,instantiated_reading);
							if (!tacit_acts.isEmpty()){
								styledText.append(frologAnswer.replaceFirst("You can't do that! ", ""));
								styledText.append(tacit_acts);
							} else {
								styledText.append(frologAnswer);
							}
						} else {
							styledText.append(frologAnswer);
						}
					} else {
						underspecified_command = false;
						styledText.append(frologAnswer.replaceFirst("You can't do that! It is not the case that", "You can't do that! You don't know"));
					}
					//Scroll down the styledtext to see Frolog's answer. 
					styledText.setSelection(styledText.getText().length());

					
				}
			}
		});
	}
	
	public static String getanswer(String playerInput){
		
		progressBar.setSelection(1);
		
		try {
//			statusText.setText("Parsing");
			
			//begin Parsing
			PlayerInterface.parsing.append("\n~~~~~~~~ PLAYER INPUT: "+playerInput+"\n");
			List<List<SemLit>> tulipareadings = new LinkedList<List<SemLit>>();
			try{
			 tulipareadings = Tulipa.parseSentence(frolog.tulipaOptions, frolog.tulipaGrammar, playerInput);
			 if (tulipareadings.isEmpty()) {
				 FrologException noparse = new FrologException("no_parse","");
				 throw noparse;
			 } else { 
				 PlayerInterface.parsing.append(tulipareadings.toString());
			 }
			} catch (FrologException fe){
				PlayerInterface.parsing.append("Parsing problem with: " +  playerInput);
				throw fe;
			} catch (Exception e) {
				//TODO here I should distinguish between unknown word and no parse
				String message = e.getMessage();
				if (message.startsWith("Unknown")){
					
				}
				FrologException fe = new FrologException("unknown_word",e.getMessage());
				PlayerInterface.parsing.append("Parsing problem with: " +  playerInput);
				throw fe;
			}
			//end Parsing
			
			progressBar.setSelection(2);
//			statusText.setText("Resolving References");
			//beginReference Resolution
			PlayerInterface.reference_resolution.append("\n~~~~~~~~ PLAYER INPUT: "+playerInput+"\n");
			Readings semanticclass = new Readings(tulipareadings);
			List<Reading> clean_readings = semanticclass.cleanReadings();
			ResolvedReadings resolved_readings = new ResolvedReadings(clean_readings);
			List<List<Literal>> instantiated_readings = resolved_readings.instantiate();
			instantiated_reading = instantiated_readings.get(0);
			PlayerInterface.reference_resolution.append(instantiated_reading.toString());
			//end Reference Resolution
			
			progressBar.setSelection(3);
			//begin Action
			//TODO Add treatment of conjunction, for now I just added the following line
			PlayerInterface.actions.append("\n~~~~~~~~ PLAYER INPUT: "+playerInput+"\n\n");
			command = new Command(instantiated_readings,0); 
			command.trytoExecute();
			PlayerInterface.actions.append(command.toString() + "\n");
			
			progressBar.setSelection(4);
			
			//end Action
			
			progressBar.setSelection(5);
			//begin Content Determination
			PlayerInterface.content_determination.append("\n~~~~~~~~ PLAYER INPUT: "+playerInput+"\n");
			Utterances utterances = new Utterances(command);
//			utterances.content.addAll(tacit_acts);
			PlayerInterface.content_determination.append(utterances.toString()); 
			//end Content Determination
			
			progressBar.setSelection(6);
			//begin Reference Generation
			PlayerInterface.reference_generation.append("\n~~~~~~~~ PLAYER INPUT: "+playerInput+"\n");
			CompleteUtterances complete_utterances = new CompleteUtterances(utterances);
			if (!complete_utterances.semantics.isEmpty()){
				PlayerInterface.reference_generation.append(complete_utterances.toString()); 
			} else {
				FrologException e = new FrologException("not_relevant",utterances.toString());
				throw e;
			}
			//end Reference Generation
			
			progressBar.setSelection(7);
			//begin Generation
			PlayerInterface.realization.append("\n~~~~~~~~ PLAYER INPUT: "+playerInput+"\n");
			Generation surface_utterances = new Generation(complete_utterances);
			PlayerInterface.realization.append(complete_utterances.toStringforGeneration()); 
			//end Generation
			 
			return surface_utterances.toString();
			
		}  catch (FrologException e) {

			String reply;
			if (e instanceof AmbiguousReference){
				AmbiguousReference am = (AmbiguousReference) e;
				reply = HandleAmbiguousReference(am.message,am.referents);
			} else {
				Exc type = e.type;
				String message = e.message;
				reply = HandleException(type,message);
			}
			return reply;
		}
	}
	
	public static String accommodate(Command command,List<Literal> instantiated_reading) {
		
		progressBar.setSelection(1);
		String res = "";
		try {	
			PlayerInterface.planner_log.append("\n~~~~~~~~ FAILED COMMAND: "+command.toString()+"\n");
			FailedReading firstreading = (FailedReading) command.execution.get(0);
			PlayerInterface.planner_log.append("Failed Precondition: "+firstreading.failed_precondition.toString()+"\n");
			List<List<Literal>> plan = find_plan(firstreading.failed_precondition);
			PlayerInterface.planner_log.append("Tacit acts: "+plan+"\n");
			PlayerInterface.planner_log.append("Player command: "+instantiated_reading+"\n");
			progressBar.setSelection(4);
			
			if (!plan.isEmpty()){ 
				for (List<Literal> action:plan){
					res += fourlastmodules("I accommodate that ",action);
				}
				res += fourlastmodules("and then ", instantiated_reading);
			}
				 
		return res;
			
		}  catch (FrologException e) {
			Exc type = e.type;
			String message = e.message;
			String reply = HandleException(type,message);
			return reply;
		}
	}

	public static String fourlastmodules(String reason, List<Literal> action) throws FrologException{
		
		//begin Action
		List<List<Literal>> actionlist = new LinkedList<List<Literal>>();
		actionlist.add(action);
		PlayerInterface.actions.append("\n~~~~~~~~ (trying to accommodate) \n\n");
		command = new Command(actionlist,0); 
		command.trytoExecute();
		PlayerInterface.actions.append(command.toString() + "\n");
		//end Action
		
		progressBar.setSelection(5);
		//begin Content Determination
		PlayerInterface.content_determination.append("\n~~~~~~~~ (trying to accommodate) \n");
		Utterances utterances = new Utterances(command);
		Utterance tacit_action = new Utterance(action,reason);
		utterances.content.add(0, tacit_action);
		PlayerInterface.content_determination.append(utterances.toString()); 
		//end Content Determination
		
		progressBar.setSelection(6);
		//begin Reference Generation
		PlayerInterface.reference_generation.append("\n~~~~~~~~ (trying to accommodate) \n");
		CompleteUtterances complete_utterances = new CompleteUtterances(utterances);
		if (!complete_utterances.semantics.isEmpty()){
			PlayerInterface.reference_generation.append(complete_utterances.toString()); 
		} else {
			FrologException e = new FrologException("not_relevant",utterances.toString());
			throw e;
		}
		//end Reference Generation
		
		progressBar.setSelection(7);
		//begin Generation
		PlayerInterface.realization.append("\n~~~~~~~~ (trying to accommodate) \n");
		Generation surface_utterances = new Generation(complete_utterances);
		PlayerInterface.realization.append(complete_utterances.toStringforGeneration()); 
		//end Generation
		
		return surface_utterances.toString();
	}
	
	public static String HandleException(Exc exc, String parameter){
		
		switch (exc) {
        case no_parse: 
        	return "Frolog: Sorry? I couldn't parse that.\n"; 
        case unknown_word: 
        	return "Frolog: I don't know the word " + parameter + ".\n";
        case no_ref_for: 
        	return "Frolog: You can't see any " + parameter + ".\n";
        case no_ref_for_pronoun: 
        	return "Frolog: I don't understand what you mean by `it'.\n";
        case missing_action: 
        	return "Frolog: Sorry! I don't know how to do that.\n";
        case racer_error: 
        	return "Frolog: RACER error occurred.\n";
        case not_relevant:
        	return "Frolog: You don't see anything interesting\n";
        case ambiguous:
        	return "Frolog: There is more than one way of doing that.\n";
        	
        default: return "An error ocurred, see the tabs for details";
		}
	}
	
	public static String HandleAmbiguousReference(String referring_expression,List<String> referents){
		String res = "";
		res += "Frolog: There is more than one " + referring_expression+".\n";
		for (String referent:referents){
			List<Literal> referent_sem = CompleteUtterances.reference_generation(referent);
			String realized_re = Generation.generateNP(referent_sem);
			res+= "Frolog: There is "+ realized_re+".\n";
		}
		return res;
	}
	
	public static void reloadKBModel(){
	try{
		ModelPlayerKB kbmodel = new ModelPlayerKB("player");
		String kbmodeldot = kbmodel.toStringforDOT();
        // Create file 
        FileWriter fstream = new FileWriter("GameScenarios/FairyTaleCastle/KBModel.dot");
        BufferedWriter out = new BufferedWriter(fstream);
        out.write(kbmodeldot);
        //Close the output stream
        out.close();
        Process dot = Runtime.getRuntime().exec("dot GameScenarios/FairyTaleCastle/KBModel.dot -Tpng -o GameScenarios/FairyTaleCastle/KBModel.png");
        dot.waitFor();
		} catch (Exception e1) {
    	  e1.printStackTrace();
      }
	}
	
	public static List<List<Literal>> find_plan(Literal goal){ 
		List<List<Literal>> plan = new LinkedList<List<Literal>>();
		try{
			ModelPlayerKB kbmodel = new ModelPlayerKB("player");
			String kbmodeldot = kbmodel.toStringforPlanner(goal);
	        // Create file 
	        FileWriter fstream = new FileWriter("GameScenarios/FairyTaleCastle/PlanningProblems/problem.PDDL");
	        BufferedWriter out = new BufferedWriter(fstream);
	        out.write(kbmodeldot);
	        out.close();
	        // Execute the planner
	        String plannerstr = "./KBInterfaces/PlannerInterface/Blackbox/blackbox -M 10000";
	        String planningdomain = "GameScenarios/FairyTaleCastle/PlanningProblems/FairyTaleCastle.PDDL"; 
			String planningproblem = "GameScenarios/FairyTaleCastle/PlanningProblems/problem.PDDL";
			String planfile = "GameScenarios/FairyTaleCastle/PlanningProblems/plan.PDDL";
			Runtime.getRuntime().exec("rm -f " + planfile);
			String runplanner = plannerstr + " -o " + planningdomain + " -f " + planningproblem + " -g " + planfile;
			System.out.println(runplanner);
			Process planner = Runtime.getRuntime().exec(runplanner);
	        planner.waitFor();
			// Parse the plan file
	        FileReader input = new FileReader(planfile);
	        BufferedReader bufRead = new BufferedReader(input);
	        //Read the first line
	        String line;
	        int count = 0;
	        line = bufRead.readLine();
	        count++;
	        while (line != null){
	        	System.out.println(count+": "+line);
	        	if ((!line.equals("NO SOLUTION")) && (!line.equals("()"))){
		        	//Erase the parenthesis
		        	line = line.replace("(", "");
		        	line = line.replace(")", "");
		        	line = line.trim();
		        	//Split the line in two:
		        	//0. the action with the thematic roles
		        	//1. the arguments of the action
		        	String[] linesp = line.split(" ",2);
		        	String action = linesp[0];
		        	if (!action.contains("access")){
			        	String arg = linesp[1];
			        	//Split the thematic roles
			        	String[] roles = action.split("=");
			        	//Split the arguments
			        	String[] args = arg.split(" ");
			        	
			        	//Build the action literals
			        	List<Literal> actionsem = new LinkedList<Literal>();
			        	//The first element of the roles list is the name of the action
			        	//Put the name of the action in a literal with a reified event
			        	String actionname = roles[0];
			        	Object[] actionarg = {"event","h"};
			        	List<Object> actionargl= Arrays.asList(actionarg);
			        	Literal actionliteral = new Literal(actionname,actionargl);
			        	actionsem.add(actionliteral);
			        	for(int i = 1 ; i < roles.length ; i++){
			        		String rolename = roles[i];
			        		String argname = args[i-1];
				        	Object[] roleargs = {"h",argname};
				        	List<Object> roleargsl= Arrays.asList(roleargs);	        		
			        		Literal role = new Literal(rolename,roleargsl); 
			        		actionsem.add(role);
			        	}
			        	plan.add(actionsem);
		        	} else{
		        		//the action is discarded
		        	}	
	        	} else {
	        		//If there is not plan, then an empty plan is returned
	        	}
	        	//move to the following line
	        	line = bufRead.readLine();
	        	count++;
	        	}
	        bufRead.close();
	      } catch (Exception e1) {
	    	  e1.printStackTrace();
	      }
	      System.out.println("\n Plan: " + plan.toString());
	      return plan;
	}
	
	public static List<Literal> find_hint(){ 
		List<Literal> hint = new LinkedList<Literal>();
		Object[] argsaux = {"grisu"};
		List<Object> argsgoal = Arrays.asList(argsaux);
	    Literal goal = new Literal("happy",argsgoal);
		try{
			ModelPlayerKB kbmodel = new ModelPlayerKB("world");
			String kbmodeldot = kbmodel.toStringforPlanner(goal);
	        // Create file 
	        FileWriter fstream = new FileWriter("GameScenarios/FairyTaleCastle/PlanningProblems/hintproblem.PDDL");
	        BufferedWriter out = new BufferedWriter(fstream);
	        out.write(kbmodeldot);
	        out.close();
	        // Execute the planner
	        String plannerstr = "./KBInterfaces/PlannerInterface/Blackbox/blackbox -M 10000";
	        String planningdomain = "GameScenarios/FairyTaleCastle/PlanningProblems/FairyTaleCastle.PDDL"; 
			String planningproblem = "GameScenarios/FairyTaleCastle/PlanningProblems/hintproblem.PDDL";
			String planfile = "GameScenarios/FairyTaleCastle/PlanningProblems/hintplan.PDDL";
			Runtime.getRuntime().exec("rm -f " + planfile);
			String runplanner = plannerstr + " -o " + planningdomain + " -f " + planningproblem + " -g " + planfile;
			System.out.println(runplanner);
			Process planner = Runtime.getRuntime().exec(runplanner);
	        planner.waitFor();
			// Parse the plan file
	        FileReader input = new FileReader(planfile);
	        BufferedReader bufRead = new BufferedReader(input);
	        //Read the first line
	        String line;
	        int count = 0;
	        line = bufRead.readLine();
	        count++;
	        while ((line != null) && (hint.isEmpty())){
	        	System.out.println(count+": "+line);
	        	if ((!line.equals("NO SOLUTION")) && (!line.equals("()"))){
		        	//Erase the parenthesis
		        	line = line.replace("(", "");
		        	line = line.replace(")", "");
		        	line = line.trim();
		        	//Split the line in two:
		        	//0. the action with the thematic roles
		        	//1. the arguments of the action
		        	String[] linesp = line.split(" ",2);
		        	String action = linesp[0];
		        	if (!action.contains("access")){
			        	String arg = linesp[1];
			        	//Split the thematic roles
			        	String[] roles = action.split("=");
			        	//Split the arguments
			        	String[] args = arg.split(" ");
			        	
			        	//The first element of the roles list is the name of the action
			        	//Put the name of the action in a literal with a reified event
			        	String actionname = roles[0];
			        	Object[] actionarg = {"event","h"};
			        	List<Object> actionargl= Arrays.asList(actionarg);
			        	Literal actionliteral = new Literal(actionname,actionargl);
			        	hint.add(actionliteral);
			        	for(int i = 1 ; i < roles.length ; i++){
			        		String rolename = roles[i];
			        		String argname = args[i-1];
				        	Object[] roleargs = {"h",argname};
				        	List<Object> roleargsl= Arrays.asList(roleargs);	        		
			        		Literal role = new Literal(rolename,roleargsl); 
			        		hint.add(role);
			        	}
		        	} else{
		        		//the action is discarded
		        	}	
	        	} else {
	        		//If there is not plan, then an empty plan is returned
	        	}
	        	//move to the following line
	        	line = bufRead.readLine();
	        	count++;
	        	}
	        bufRead.close();
	      } catch (Exception e1) {
	    	  e1.printStackTrace();
	      }
	      System.out.println("\n Hint: " + hint.toString());
	      return hint;
	}
	
	public static void open_KBmodel() {
//		Display display = new Display ();
		Shell shell = new Shell (display);
		shell.setText("Model of the context");
		shell.setLayout(new FillLayout());
		
		String string = "GameScenarios/FairyTaleCastle/KBModel.png";
		image = new Image (display, string);
		
		final Point origin = new Point (0, 0);
		canvas = new Canvas (shell, SWT.NO_BACKGROUND |
				SWT.NO_REDRAW_RESIZE | SWT.V_SCROLL | SWT.H_SCROLL);
		final ScrollBar hBar = canvas.getHorizontalBar ();
		hBar.addListener (SWT.Selection, new Listener () {
			public void handleEvent (Event e) {
				int hSelection = hBar.getSelection ();
				int destX = -hSelection - origin.x;
				Rectangle rect = image.getBounds ();
				canvas.scroll (destX, 0, 0, 0, rect.width, rect.height, false);
				origin.x = -hSelection;
			}
		});
		final ScrollBar vBar = canvas.getVerticalBar ();
		vBar.addListener (SWT.Selection, new Listener () {
			public void handleEvent (Event e) {
				int vSelection = vBar.getSelection ();
				int destY = -vSelection - origin.y;
				Rectangle rect = image.getBounds ();
				canvas.scroll (0, destY, 0, 0, rect.width, rect.height, false);
				origin.y = -vSelection;
			}
		});
		canvas.addListener (SWT.Resize,  new Listener () {
			public void handleEvent (Event e) {
				Rectangle rect = image.getBounds ();
				Rectangle client = canvas.getClientArea ();
				hBar.setMaximum (rect.width);
				vBar.setMaximum (rect.height);
				hBar.setThumb (Math.min (rect.width, client.width));
				vBar.setThumb (Math.min (rect.height, client.height));
				int hPage = rect.width - client.width;
				int vPage = rect.height - client.height;
				int hSelection = hBar.getSelection ();
				int vSelection = vBar.getSelection ();
				if (hSelection >= hPage) {
					if (hPage <= 0) hSelection = 0;
					origin.x = -hSelection;
				}
				if (vSelection >= vPage) {
					if (vPage <= 0) vSelection = 0;
					origin.y = -vSelection;
				}
				canvas.redraw ();
			}
		});
		canvas.addListener (SWT.Paint, new Listener () {
			public void handleEvent (Event e) {
				GC gc = e.gc;
				gc.drawImage (image, origin.x, origin.y);
				Rectangle rect = image.getBounds ();
				Rectangle client = canvas.getClientArea ();
				int marginWidth = client.width - rect.width;
				if (marginWidth > 0) {
					gc.fillRectangle (rect.width, 0, marginWidth, client.height);
				}
				int marginHeight = client.height - rect.height;
				if (marginHeight > 0) {
					gc.fillRectangle (0, rect.height, client.width, marginHeight);
				}
			}
		});
		shell.setSize (400, 500);
		shell.open ();
		
		while (!shell.isDisposed ()) {
			if (!display.readAndDispatch ()) display.sleep ();
		}
		image.dispose();
		shell.dispose();
	}
	
}

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
import java.io.*;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

public class SimpleDebugger {

	private org.eclipse.swt.widgets.Shell sShell = null; 

	public static Text textArea = null;

	private boolean hasChanged = false;

	private boolean isClosing = false;

	private static final String title = "Into Frolog";

	private static final String NEW_LINE = System.getProperty("line.separator");  //  @jve:decl-index=0:

	public static void startDebugger() {
		/* Before this is run, be sure to set up the following in the launch configuration 
		 * (Arguments->VM Arguments) for the correct SWT library path. 
		 * The following is a windows example:
		 * -Djava.library.path="installation_directory\plugins\org.eclipse.swt.win32_3.0.0\os\win32\x86"
		 */
		org.eclipse.swt.widgets.Display display = org.eclipse.swt.widgets.Display
				.getDefault();
		SimpleDebugger thisClass = new SimpleDebugger();
		thisClass.createSShell();
		thisClass.sShell.open();

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
		textArea = new Text(sShell, SWT.MULTI | SWT.WRAP | SWT.V_SCROLL
				| SWT.BORDER);
		textArea.setBounds(new Rectangle(5, 5, 373, 206));
		sShell.setText(title);
		sShell.setLayout(null);
		sShell.setSize(new org.eclipse.swt.graphics.Point(393, 279));
		textArea.addModifyListener(new org.eclipse.swt.events.ModifyListener() {
			public void modifyText(org.eclipse.swt.events.ModifyEvent e) {
				if (!hasChanged) {
					sShell.setText(title + " *");
					hasChanged = true;
				}
			}
		});
		sShell.addShellListener(new org.eclipse.swt.events.ShellAdapter() {
			public void shellClosed(org.eclipse.swt.events.ShellEvent e) {
				if (!isClosing) {
					e.doit = doExit();
				}
			}
		});
	}

//	private void saveFile() {
//		FileDialog dialog = new FileDialog(sShell, SWT.SAVE);
//		String result = dialog.open();
//		if (result != null) {
//			File f = new File(result);
//			try {
//				BufferedWriter bw = new BufferedWriter(new FileWriter(f));
//				String text = textArea.getText();
//				bw.write(text);
//				bw.close();
//				sShell.setText(title);
//				hasChanged = false;
//			} catch (FileNotFoundException e1) {
//				e1.printStackTrace();
//			} catch (IOException e1) {
//				e1.printStackTrace();
//			}
//		}
//	}

	private boolean doExit() {
		if (hasChanged) {
			MessageBox mb = new MessageBox(sShell, SWT.ICON_QUESTION | SWT.YES
					| SWT.NO | SWT.CANCEL);
			mb.setText("Save Changes?");
			mb.setMessage("File has been changed. Save before exit?");
			int state = mb.open();

		}
		isClosing = true;
		sShell.close();
		sShell.dispose();
		return true;
	}
	
	public static void appendtextArea(String s){
		
		textArea.append(s); 
	}
	
}

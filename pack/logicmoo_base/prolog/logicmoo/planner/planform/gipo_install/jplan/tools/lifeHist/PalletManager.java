/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 * 
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both the
 * copyright notice and this permission notice and warranty disclaimer appear in
 * supporting documentation, and that the names of the authors or their
 * employers not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 * 
 * The authors and their employers disclaim all warranties with regard to this
 * software, including all implied warranties of merchantability and fitness. In
 * no event shall the authors or their employers be liable for any special,
 * indirect or consequential damages or any damages whatsoever resulting from
 * loss of use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 * 
 * 
 * 
 * Created on 11-Jan-2005
 * 
 * Author Ron
 */
package jplan.tools.lifeHist;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.Insets;
import java.io.File;
import java.net.URL;
import java.net.MalformedURLException;
import java.io.FileFilter;
import java.io.FileWriter;
import java.util.*;

import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.JLabel;
import javax.swing.Icon;

import org.jgraph.*;
import org.jgraph.graph.*;

import jplan.top.OclEd;
import jplan.general.EdTextComboBox;
import jplan.general.OCLIdentifierDocument;
import jplan.general.PNGThumbNailViewer;
import jplan.general.TextComboBoxEditor;
import jplan.general.Utility;

/**
 * Class PalletManager
 * Looks after insertung items into the library 
 * and placing library items into the current graph
 * @author ron
 *
 */
public class PalletManager {
	private HistoryWindow parent;
	private OclEd top;
	private JList libraryList = null;

	public PalletManager(HistoryWindow parent, OclEd top) {
		this.parent = parent;
		this.top = top;
	}

	/**
	 * initPalletTab
	 * set up the pallet pane
	 * @return
	 */
	public JTabbedPane initPalletTab() {
		//		 Add Library Pallet
		JTabbedPane palletTab = new JTabbedPane();
		JPanel libPanel = new JPanel(new BorderLayout());
		JToolBar toolbar = new JToolBar();
		toolbar.setMargin(new Insets(-3, 1, -3, 1));
		toolbar.setFloatable(false);
		URL viewUrl = HistoryWindow.class.getResource("resources/view.png");
		ImageIcon viewIcon = new ImageIcon(viewUrl);
		JButton viewLibBut = new JButton(viewIcon);
		viewLibBut.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				describeLibraryItemAP();
			}
		});
		viewLibBut.setToolTipText("Describe Library Item");
		toolbar.add(viewLibBut);
		URL goUrl = HistoryWindow.class.getResource("resources/go.png");
		ImageIcon goIcon = new ImageIcon(goUrl);
		JButton insLibBut = new JButton(goIcon);
		insLibBut.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				insertLibraryItemAP();
			}
		});
		insLibBut.setToolTipText("Add Selected to Design");
		toolbar.add(insLibBut);

		libPanel.add(toolbar, "South");
		DefaultListModel libraryListModel = populateLibrary();
		libraryList = new JList(libraryListModel);
		JScrollPane libScrl = new JScrollPane(libraryList);
		libPanel.add(libScrl, "Center");
		palletTab.addTab("Library", libPanel);
		return palletTab;
	}

	/**
	 * populateLibrary
	 * Create the library list
	 */
	private DefaultListModel populateLibrary() {
		DefaultListModel lm = new DefaultListModel();
		File dir = new File(top.strOCLPath + "/domains/library");

		//	    String[] children = dir.list();
		//	    if (children == null) {
		//	        // Either dir does not exist or is not a directory
		//	    } else {
		//	        for (int i=0; i<children.length; i++) {
		//	            // Get filename of file or directory
		//	            String filename = children[i];
		//	        }
		//	    }
		//	    
		//	    // It is also possible to filter the list of returned files.
		//	    // This example does not return any files that start with `.'.
		//	    FilenameFilter filter = new FilenameFilter() {
		//	        public boolean accept(File dir, String name) {
		//	            return !name.startsWith(".");
		//	        }
		//	    };
		//	    children = dir.list(filter);

		// The list of files can also be retrieved as File objects
		//File[] files = dir.listFiles();

		// This filter only returns non - directories
		FileFilter fileFilter = new FileFilter() {
			public boolean accept(File file) {
				return !file.isDirectory();
			}
		};
		File[] files = dir.listFiles(fileFilter);
		for (int i = 0; i < files.length; i++) {
			File file = files[i];
			if (file.getName().endsWith(".gfx")) {
				String itemName =
					file.getName().substring(0, file.getName().indexOf('.'));
				if (!lm.contains(itemName))
					lm.addElement(itemName);
			}
		}
		return lm;
	}

	/**
	 * save current OCL Domain and add to Library
	 * request file name from user
	 */
	public void saveLibraryAsFile() {
		String libraryPath =
			top.strOCLPath
				+ File.separator
				+ "domains"
				+ File.separator
				+ "library";
		JFileChooser fileChoose = new JFileChooser(libraryPath);
		fileChoose.setDialogTitle("Save Library Item As");
		fileChoose.setFileFilter(HistoryWindow.gfxFilter);
		int result = fileChoose.showSaveDialog(top);
		File file = fileChoose.getSelectedFile();

		FileWriter domOut;
		if (file != null && result == JFileChooser.APPROVE_OPTION) {
			if (file.exists()) {
				JOptionPane query =
					new JOptionPane(
						"Overwrite Existing File",
						JOptionPane.QUESTION_MESSAGE,
						JOptionPane.YES_NO_OPTION);
				JDialog dia = query.createDialog(top, "GIPO Query");
				dia.show();
				if (((Integer) query.getValue()).intValue() == 0) {
					DefaultGraphModelFileFormatXML writer =
						new DefaultGraphModelFileFormatXML(top);
					try {
						writer.write(file, (LHGraph) parent.getGraph());
						JOptionPane.showMessageDialog(
							top,
							"Wrote file " + file.getName(),
							"GIPO Information",
							JOptionPane.INFORMATION_MESSAGE);
					} catch (Exception e) {
						JOptionPane.showMessageDialog(
							top,
							"Problem writing to file",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE);
						return;
					}
				}
			} else {
				DefaultGraphModelFileFormatXML writer =
					new DefaultGraphModelFileFormatXML(top);
				try {
					writer.write(file, (LHGraph) parent.getGraph());
					JOptionPane.showMessageDialog(
						top,
						"Wrote file " + file.getName(),
						"GIPO Information",
						JOptionPane.INFORMATION_MESSAGE);
				} catch (Exception e) {
					JOptionPane.showMessageDialog(
						top,
						"Problem writing to file",
						"GIPO Error",
						JOptionPane.ERROR_MESSAGE);
					return;
				}
			}
		} else {
			return;
		}
		String itemName =
			file.getName().substring(0, file.getName().indexOf('.'));
		DefaultListModel lm = (DefaultListModel) libraryList.getModel();
		if (!lm.contains(itemName))
			lm.addElement(itemName);
	}

	/**
	 * insertLibraryItem
	 * The insert button has been pressed
	 *
	 */
	private void insertLibraryItemAP() {
		if (libraryList.getSelectedIndex() == -1) {
			JOptionPane.showMessageDialog(
				top,
				"You must select a Library Item to Insert",
				"GIPO Information",
				JOptionPane.INFORMATION_MESSAGE);
			return;
		}
		//parent.setEditMode(HistoryWindow.INSERTLIBRARYITEM);
		LHGraph tempGraph = new LHGraph(new DefaultGraphModel());
		DefaultGraphModel model = loadGraphFile(tempGraph);
		if (model == null)
			return;
		insertLibraryItem(parent.graph,model,tempGraph,true);
	}
	/**
	 * descriveLibraryItem
	 * give a preview of selected library item
	 *
	 */
	private void describeLibraryItemAP() {
		if (libraryList.getSelectedIndex() == -1) {
			JOptionPane.showMessageDialog(
				top,
				"You must select a Library Item to Describe",
				"GIPO Information",
				JOptionPane.INFORMATION_MESSAGE);
			return;
		}
		LHGraph tempGraph = new LHGraph(new DefaultGraphModel());
		DefaultGraphModel model = loadGraphFile(tempGraph);
		if (model == null)
			return;
		LibraryItemView view = new LibraryItemView(top,this,(String)libraryList.getSelectedValue(),model,tempGraph);
		top.desktop.add(view);
		top.deskManager.activateFrame(view);
		view.show();
	}

	/**
	 * insertLibraryItem
	 * m
	 * @param graph - the graph to insert this item into
	 * @param model - the already loaded model
	 * @param tempGraph - the Graph loaded so far
	 * @param renameSorts - do we collect sort info from user
	 */
	public void insertLibraryItem(JGraph graph,DefaultGraphModel model,LHGraph tempGraph,boolean renameSorts) {
		parent.setEditMode(HistoryWindow.SELECT);
		if (model == null) {
			// must be error - just give up error message already displayed
			return;
		}
		// Order Cells by Model Layering
		Object[] cells =
			DefaultGraphModel.order(
				model,
				DefaultGraphModel.getAll((GraphModel) model));
		// If Any Cells in View
		if (renameSorts)
			editSorts(cells);
		if (cells != null && cells.length > 0) {
			// Create Group Cell
			DefaultGraphCell group =
				new DefaultGraphCell(
					new String((String) libraryList.getSelectedValue()));
			Rectangle2D bounds = tempGraph.getCellBounds(cells);
			if (bounds != null) {
				bounds =
					new Rectangle2D.Double(
						bounds.getX() + bounds.getWidth() / 4,
						bounds.getY() + bounds.getHeight() / 4,
						bounds.getWidth() / 2,
						bounds.getHeight() / 2);
				GraphConstants.setBounds(group.getAttributes(), bounds);
			}
			// Insert into model
			graph.getGraphLayoutCache().insertGroup(group, cells);
		}

	}

	/**
	 * Present a dialog box to have the user select an existing Graphic Domain
	 * specification
	 * @param tempGraph - graph to store loaded pattern
	 */
	public DefaultGraphModel loadGraphFile(LHGraph tempGraph) {
		String fPath =
			top.strOCLPath
				+ File.separator
				+ "domains"
				+ File.separator
				+ "library"
				+ File.separator
				+ (String) libraryList.getSelectedValue()
				+ ".gfx";
		File file = new File(fPath);
		if (file != null) {
			DefaultGraphModel model = null;
			try {
				DefaultGraphModelFileFormatXML reader =
					new DefaultGraphModelFileFormatXML(top);
				model = (DefaultGraphModel) reader.read(file, tempGraph);
			} catch (Exception e) {
				JOptionPane.showMessageDialog(
					top,
					"Library file cannot be found.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE);
					return null;
			}
			return model;
		}
		return null;
	}

	/**
	 * editSorts
	 * collect sort information from user and modify state cells
	 * @param cells
	 */
	private void editSorts(Object[] cells) {
		Hashtable sorts = new Hashtable();
		List sortsList = new ArrayList();
		List transitions = new ArrayList();
		sortsList.add("none");
		for (int i = 0; i < cells.length; i++) {
			if (cells[i] instanceof StateCell) {
				StateCell state = (StateCell) cells[i];
				String sort = state.getObjectSort();
				if (sorts.containsKey(sort)) {
					state.setObjectSort((String) sorts.get(sort));
					File iconFile = (File) sorts.get(sort + "Icon");
					if (iconFile != null) {
						ImageIcon icon = null;
						try {
							icon = new ImageIcon(iconFile.toURL());
						} catch (MalformedURLException e) {
						}
						GraphConstants.setIcon(state.getAttributes(), icon);
					}
				} else {
					String libraryPath =
						top.strOCLPath
							+ File.separator
							+ "domains"
							+ File.separator
							+ "icons";
					Icon curIcon =
						GraphConstants.getIcon(state.getAttributes());
					JFileChooser chooser = new JFileChooser(libraryPath);
					InstantiatePanel instPan =
						new InstantiatePanel(sortsList, sort, curIcon);
					chooser.setAccessory(instPan);
					// Ok, set up our own file view for the chooser
					chooser.setFileView(new PNGThumbNailViewer(parent));
					int option =
						chooser.showDialog(parent, "Object Sort Details");
					if (option == JFileChooser.APPROVE_OPTION) {
						File iconFile = chooser.getSelectedFile();
						Utility.debugPrintln(
							"patterns",
							"SORT = " + instPan.objectSort);
						sorts.put(sort, instPan.objectSort);
						sorts.put(sort + "Icon", iconFile);
						sortsList.add(instPan.objectSort);
						state.setObjectSort(instPan.objectSort);
						if (iconFile != null) {
							ImageIcon icon = null;
							try {
								icon = new ImageIcon(iconFile.toURL());
							} catch (MalformedURLException e) {
							}
							GraphConstants.setIcon(state.getAttributes(), icon);
						}
					} else {
						// TODO something sensible if they cancel
					}
				}
			} else if (cells[i] instanceof TransitionCell) {
				// Delay dealing with transitions until all states done
				transitions.add(cells[i]);
			}
		}
		ListIterator li = transitions.listIterator();
		while (li.hasNext()) {
			TransitionCell trans = (TransitionCell) li.next();
			String sort = trans.getObjectSort();
			if (sorts.containsKey(sort)) {
				trans.setObjectSort((String) sorts.get(sort));
			} else {
				JOptionPane.showMessageDialog(
					top,
					"Transition "
						+ trans.toString()
						+ "has unidentified object sort\n Please edit manually.",
					"GIPO Information",
					JOptionPane.INFORMATION_MESSAGE);
			}
		}
	}

	/**
	 * Class implements query dialog to insert into FileChooser when prompting
	 * for Object Sorts and representing icons.
	 */
	class InstantiatePanel extends JPanel {
		private EdTextComboBox cmbSortName;
		public String objectSort = "none";

		public InstantiatePanel(
			List sortsList,
			String curSortName,
			Icon curIcon) {
			setLayout(new BorderLayout());
			objectSort = curSortName;
			JLabel lblCurSortName = new JLabel("Library Sort = " + curSortName);
			add(lblCurSortName, "North");
			if (curIcon != null) {
				JLabel lblIcon = new JLabel(curIcon);
				add(lblIcon, "Center");
			} else {
				JLabel lblIcon = new JLabel("none");
				add(lblIcon, "Center");
			}
			cmbSortName = new EdTextComboBox();
			cmbSortName.setEditor(
				new TextComboBoxEditor(new OCLIdentifierDocument()));
			cmbSortName.addAllList(sortsList);
			cmbSortName.addItem(curSortName);
			ActionListener actListener = new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					cmbSortAP();
				}
			};
			cmbSortName.addActionListener(actListener);
			add(cmbSortName, "South");
		}

		private void cmbSortAP() {
			objectSort = (String) cmbSortName.getSelectedItem();
			int count = cmbSortName.getItemCount();
			boolean found = false;
			for (int i = 0; i < count && !found; i++) {
				String item = (String) cmbSortName.getItemAt(i);
				if (item.equals(objectSort))
					found = true;
			}
			if (!found)
				cmbSortName.addItem(objectSort);
		}
	}
}

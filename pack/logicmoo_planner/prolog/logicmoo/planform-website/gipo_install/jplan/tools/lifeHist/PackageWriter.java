/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the names of
 * the authors or their employers not be used in advertising or publicity 
 * pertaining to distribution of the software without specific, written 
 * prior permission.
 *
 * The authors and their employers disclaim all warranties with regard to 
 * this software, including all implied warranties of merchantability and 
 * fitness.  In no event shall the authors or their employers be liable 
 * for any special, indirect or consequential damages or any damages 
 * whatsoever resulting from loss of use, data or profits, whether in an 
 * action of contract, negligence or other tortious action, arising out of 
 * or in connection with the use or performance of this software.
 *
 *
 *
 * Created on 22-Jan-2005
 *
 * Author ron
 * 
 */
package jplan.tools.lifeHist;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.GraphModel;

import jplan.top.OclEd;

/**
 * @author ron
 *
 * This package encodes/decodes Package contents
 */
public class PackageWriter extends DefaultGraphModelFileFormatXML {
	
	public PackageWriter(OclEd top){
		super(top);
	}
	
	public String toString(Object[] cells,LHGraph graph){
		userObjectMap.clear();
		cellMap.clear();
		attrs.clear();

		String xml = "<jgx-1.01>\n";

		GraphModel model = graph.getModel();
		xml += "<model>\n";
		xml += outputModel(cells, model, "\t", null);
		xml += "</model>\n";

		xml += "<attrs>\n";
		xml += outputAttributes("\t");
		xml += "</attrs>\n";

		xml += "<user>\n";
		xml += encodeUserObjects("\t", userObjectMap);
		xml += "</user>\n";

		xml += "<view>\n";
		xml += outputView(graph, "\t");
		xml += "</view>\n";

		// Close main tags
		xml += "</jgx-1.01>\n";
		return xml;
		
	}
	
	public String outputModel(Object[] cells,GraphModel model, String indent, Object parent) {
		String xml = new String("");
		for (int i = 0; i < cells.length; i++) {
			Object cell = cells[i];
			if (cell != null)
				xml += outputCell(indent, model, cell);
		}
		return xml;
	}
	
	public void read(String xmlStr,LHGraph graph) throws Exception{
		
		read((InputStream)new ByteArrayInputStream(xmlStr.getBytes()),graph);
		
	}

}

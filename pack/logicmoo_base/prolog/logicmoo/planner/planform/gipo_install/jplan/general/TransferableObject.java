package jplan.general;
/**
 * TransferableObject.java
 *
 *
 * Created: Fri Nov 30 10:57:56 2001
 *
 * @author W Zhao
 * @version
 */
import java.awt.datatransfer.*;  /* Weihong added on 30/11/2001 */
import java.io.*;

public class TransferableObject extends Object implements Transferable {
    
    public TransferableObject() {
	super();
    }

    /**
     * Returns an array of DataFlavor objects indicating
     * the flavors the data can be provided in. 
     * @return an array of data flavors in which this data can be transferred
     */
    public DataFlavor[] getTransferDataFlavors(){
	DataFlavor[] df = new DataFlavor[1];
	df[0] = new DataFlavor(this.getClass(), "TransferableObject");
	return df;
    }
    
    /**
     * Returns whether or not the specified data flavor is supported for this object.
     * @param flavor the requested flavor for the data
     * @return boolean indicating wjether or not the data flavor is supported
     */
    public boolean isDataFlavorSupported(DataFlavor flavor){
	DataFlavor flavor1 = new DataFlavor(this.getClass(), "TransferableObject");
	if (flavor.match(flavor1))
	    return true;

	return false;
    }

    /**
     * Returns an object which represents the data to be transferred.
     * The class of the object returned is defined by the
     * representation class of the flavor.
     * @param flavor the requested flavor for the data
     * @throws IOException if the data is no longer available in the requested flavor.
     * @throws UnsupportedFlavorException if the requested data flavor is not supported.
     * @return transferable object
     */
    public Object getTransferData(DataFlavor flavor)
	throws UnsupportedFlavorException, IOException {
	TransferableObject tobj = null;
	try {
	   tobj = (TransferableObject)this.clone();
	} catch (CloneNotSupportedException e){
	    System.err.println(e);
	}
	System.out.println("Return Class: "+getClass().getName());
	return tobj;
    }

} // TransferableObject

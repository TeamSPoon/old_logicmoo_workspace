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
 */

package jplan.general;
/**
 * PositionArray.java
 *
 *
 * Created: Tue May 21 09:32:04 2002
 *
 * @author W Zhao
 * @version
 */

/**
 * This is chess board (table) coordination system
 */
public class PositionArray  {
    
    private int xUnit, yUnit; //how long actually per unit stands for
    private int size; //counter of total rows
    private int[] chessBoard; //stores current relative position
    private int originalY, originalX; //offset to the (0,0) original

    public PositionArray(int xUnit, int yUnit) {
	this.xUnit = xUnit;
	this.yUnit = yUnit;
	originalY = 0;
	originalX = 0;
	size = 0;
	chessBoard = new int[size];
    }
    
    public PositionArray(int xUnit, int yUnit, int originalX, int originalY) {
	this.xUnit = xUnit;
	this.yUnit = yUnit;
	this.originalY = originalY;
	this.originalX = originalX;
	size = 0;
	chessBoard = new int[size];
    }

    /* WZ 10/6/02 */
    public PositionArray(int size, int xUnit, int yUnit, int originalX, int originalY) {
	this.xUnit = xUnit;
	this.yUnit = yUnit;
	this.originalY = originalY;
	this.originalX = originalX;
	this.size = size;
	chessBoard = new int[size];
    }

    /**
       * Set the value of column.
       * @return Value of column.
       */
    public boolean setPosition(int row, int column){
	if (row < size+1){
	    chessBoard [row-1] = column;
	    return true;
	}
	else
	    return false;
    }

    /**
       * Add one more record/row and set the value of column.
       * 
       */
    public void addPosition(int row, int column){
	addPosition();
	setPosition(row, column);
    }

    /**
       * add one more record
       * @return number of records/rows.
       */
    public int addPosition(){
	//clone old one
	int [] copy = new int[size];
	merge(size, chessBoard, copy);
	//create new array with size increase by 1
	size ++;
	chessBoard =  new int[size];
	chessBoard[size-1] = 0; //initialise
	//take existing data
	merge(size-1, copy, chessBoard);
	return size;
    }

    private void merge(int size, int[] oldArray, int[] newArray){
	for (int i = 0; i < size; i++){
	     newArray[i] = oldArray[i];
	}
    }

    /**
       * Get the value of xUnit.
       * @return Value of xUnit.
       */
    public int getXUnit() {return xUnit;}
    
    /**
       * Set the value of xUnit.
       * @param v  Value to assign to xUnit.
       */
    public void setXUnit(int  v) {this.xUnit = v;}
    
    /**
       * Get the value of x in the coordination system
       * @return Value of x.
       */
    public int getX(int row) {
	return (int)(xUnit * (chessBoard[row-1] - 1) + originalX);
    }

    /**
       * Get the value of x in the coordination system
       * @return Value of x.
       */
    public int getNextX(int row) {
	return (int)(xUnit * chessBoard[row-1] + originalX);
    }

    /**
       * Get the value of yUnit.
       * @return Value of yUnit.
       */
    public int getYUnit() {return yUnit;}
    
    /**
       * Set the value of yUnit.
       * @param v  Value to assign to yUnit.
       */
    public void setYUnit(int  v) {this.yUnit = v;}

    /**
       * Get the value of yUnit.
       * @return Value of yUnit.
       */
    public int getY(int row) {
	return (int)(yUnit * (row-1) + originalY);
    }

    /**
       * Get the value of yUnit.
       * @return Value of yUnit.
       */
    public int getNextY(int row) {
	return getY(row);
    }    

    /**
       * Get the value of size.
       * @return Value of size.
       */
    public int getSize() {
	return size;
    }  

    /* WZ 10/6/02 */
    /**
       * Set the value of size.
       * 
       */
    public void setSize(int s) {
	size = s;
    }  

    /**
       * Get the value of getColumn.
       * @return Value of getColumn.
       */
    public int getColumn(int row) {
	if (row > size || row < 0 || row == 0)
	    return -1;
	return chessBoard[row-1];
    }  

    /* WZ 10/6/02 */
    /**
       * Set the value of Column.
       * 
       */
    public void setColumn(int row, int col) {
	chessBoard[row-1] = col;
    }  

    /**
       * Increase by 1.
       * 
       */
    public void increase(int row) {
	chessBoard[row-1]++;
    }

    /**
       * Decrease by 1.
       * 
       */
    public void decrease(int row) {
	chessBoard[row-1]--;
    }

    /* WZ 10/6/02 */
    /**
     * clone a copy of this vShape
     * @return Object
     */    
    public Object clone(){
	PositionArray copy = new PositionArray(size, xUnit, yUnit, originalX, originalY);
	for (int i=1; i<size+1; i++){ //counter of total rows
	    copy.setPosition(i, getColumn(i));
	}
	return copy;
    }

    /* WZ 11/6/02 */
    /**
     * Returns a string representation of the oclOperator
     * @return String
     */
    public String toString(){
	StringBuffer sb = new StringBuffer();
	for (int i=1; i<size+1; i++){ //counter of total rows
	    sb.append("(Row"+i+": "+getColumn(i)+");  ");
	}
	return sb.toString();
    }

} // PositionArray

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
 * Created on 24-Aug-2003
 *
 * Author ron
 * 
 */
package jplan.ocl;

import java.io.*;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;

import jplan.general.Utility;
import jplan.general.OCLException;

/**
 * @author ron
 *
 * This class implements the ocl plus expressions that for part of
 * operators. Includes both LHS boolean expressions and
 * RHS update expressions
 */
public class oclExpression implements oclPrint, Serializable, Cloneable {

	public static final int TEST = 1;
	public static final int ASSIGN = 2;
	public static final int INCREASE = 3;
	public static final int DECREASE = 4;
	public static final int SCALEUP = 5;
	public static final int SCALEDOWN = 6;
	public static final int NOT = 7;
	public static final int AND = 8;
	public static final int OR = 9;
	public static final int PLUS = 10;
	public static final int MINUS = 11;
	public static final int UMINUS = 12;
	public static final int TIMES = 13;
	public static final int DIV = 14;
	public static final int EQUAL = 15;
	public static final int GREATER = 16;
	public static final int LESS = 17;
	public static final int GREATEREQ = 18;
	public static final int LESSEQ = 19;
	public static final int FLUENT_EXPR = 20;
	public static final int DOUBLE_EXPR = 21;
	public static final int TIME_EXPR = 22;
	public static final int UNARY_EXPR = 23;
	public static final int VAR_EXPR = 24;

	private int exprType; // The type of this expression
	private oclExpression rhs_expr;
	private oclExpression lhs_expr;
	private Object value;

	/**
	 * getExprType
	 * @return exprType
	 */
	public int getExprtType() {
		return exprType;
	}

	/**
	 * creates an instance of the oclPredicate
	 * @param name the name of the oclPredicate
	 */
	public oclExpression(int exprType) {
		this.exprType = exprType;
	}

	/**
	 * addRHS
	 * @param exp the expesssion forming the RHS
	 */

	public void addRHS(oclExpression exp) {
		rhs_expr = exp;
	}

	/**
	 * addLHS
	 * @param exp the expesssion forming the LHS
	 */

	public void addLHS(oclExpression exp) {
		lhs_expr = exp;
	}

	/**
	 * addDouble add a numeric argument as lhs
	 * @param dbl
	 */
	public void addDouble(Double dbl) {
		value = dbl;
	}
	/**
	 * addFluent add a fluent argument as value
	 * @param pred
	 */
	public void addFluent(oclPredicate pred) {
		value = pred;
	}
	/**
	 * addVariable add a fluent argument as lhs
	 * @param variable
	 */
	public void addVariable(String var) {
		value = var;
	}

	/**
	 * getRHS
	 * @return - the right hand side expression
	 */
	public oclExpression getRHS() {
		return rhs_expr;
	}

	/**
	 * getValue
	 * @return - the value
	 */
	public Object getValue() {
		return value;
	}

	/**
	 * getLHS
	 * @return - the left hand side expression
	 */
	public oclExpression getLHS() {
		return lhs_expr;
	}

	/**
	 * for all the expression functor elements replace with a new variable value.
	 * @param before the varible
	 * @param after new element name
	 * @return void
	 */
	public void replaceVariableName(String before, String after) {
		if (exprType == FLUENT_EXPR) {
			try {
				((oclPredicate) value).replaceVariableNameByName(before, after);
			} catch (Exception e) {
				// Do nothing variable not present
			}
		}
		if (rhs_expr != null) {
			rhs_expr.replaceVariableName(before, after);
		}
		if (lhs_expr != null) {
			lhs_expr.replaceVariableName(before, after);
		}
	}

	public oclPredicate updateFluent(List stateFluents) throws OCLException {
		oclPredicate res = null;
		switch (exprType) {
			case ASSIGN :
				try {
					res =
						(oclPredicate) ((oclPredicate) lhs_expr.getValue())
							.clone();
				} catch (CloneNotSupportedException e) {
				}
				double val = rhs_expr.eval(stateFluents);
				res.setFluentValue(val);
				break;
			case INCREASE :
				try {
					res =
						(oclPredicate) (((oclPredicate) lhs_expr.getValue())
							.clone());
				} catch (CloneNotSupportedException e) {
				}
				double oldVal = getStateValue(res, stateFluents);
				val = rhs_expr.eval(stateFluents) + oldVal;
				res.setFluentValue(val);
				break;
			case DECREASE :
				try {
					res =
						(oclPredicate) (((oclPredicate) lhs_expr.getValue())
							.clone());
				} catch (CloneNotSupportedException e) {
				}
				oldVal = getStateValue(res, stateFluents);
				val = oldVal - rhs_expr.eval(stateFluents);
				res.setFluentValue(val);
				break;
			case SCALEUP :
				try {
					res =
						(oclPredicate) (((oclPredicate) lhs_expr.getValue())
							.clone());
				} catch (CloneNotSupportedException e) {
				}
				oldVal = getStateValue(res, stateFluents);
				val = oldVal * rhs_expr.eval(stateFluents);
				res.setFluentValue(val);
				break;
			case SCALEDOWN :
				try {
					res =
						(oclPredicate) (((oclPredicate) lhs_expr.getValue())
							.clone());
				} catch (CloneNotSupportedException e) {
				}
				oldVal = getStateValue(res, stateFluents);
				val = oldVal - rhs_expr.eval(stateFluents);
				res.setFluentValue(val);
				break;
			default :
				throw new OCLException("Update type not supported.");
		}
		// TODO Shouldn't have to do this Semantic checking or parsing should deal with it
		res.setFluent(true);
		return res;
	}

	/**
	 * updateFluentList
	 * update the fluent in the list to match this expression
	 * @param stateFluents
	 * @return
	 * @throws OCLException
	 */
	public oclPredicate updateFluentList(List stateFluents)
		throws OCLException {
		oclPredicate res = null;
		oclPredicate lhs = null;
		switch (exprType) {
			case ASSIGN :
				try {
					lhs =
						(oclPredicate) ((oclPredicate) lhs_expr.getValue())
							.clone();
				} catch (CloneNotSupportedException e) {
				}
				double val = rhs_expr.eval(stateFluents);
				res = getStateFluent(lhs, stateFluents);
				res.setFluentValue(val);
				break;
			case INCREASE :
				try {
					lhs =
						(oclPredicate) (((oclPredicate) lhs_expr.getValue())
							.clone());
				} catch (CloneNotSupportedException e) {
				}
				double oldVal = getStateValue(lhs, stateFluents);
				val = rhs_expr.eval(stateFluents) + oldVal;
				res = getStateFluent(lhs, stateFluents);
				res.setFluentValue(val);
				break;
			case DECREASE :
				try {
					lhs =
						(oclPredicate) (((oclPredicate) lhs_expr.getValue())
							.clone());
				} catch (CloneNotSupportedException e) {
				}
				oldVal = getStateValue(lhs, stateFluents);
				val = oldVal - rhs_expr.eval(stateFluents);
				res = getStateFluent(lhs, stateFluents);
				res.setFluentValue(val);
				break;
			case SCALEUP :
				try {
					lhs =
						(oclPredicate) (((oclPredicate) lhs_expr.getValue())
							.clone());
				} catch (CloneNotSupportedException e) {
				}
				oldVal = getStateValue(lhs, stateFluents);
				val = oldVal * rhs_expr.eval(stateFluents);
				res = getStateFluent(lhs, stateFluents);
				res.setFluentValue(val);
				break;
			case SCALEDOWN :
				try {
					lhs =
						(oclPredicate) (((oclPredicate) lhs_expr.getValue())
							.clone());
				} catch (CloneNotSupportedException e) {
				}
				oldVal = getStateValue(lhs, stateFluents);
				val = oldVal - rhs_expr.eval(stateFluents);
				res = getStateFluent(lhs, stateFluents);
				res.setFluentValue(val);
				break;
			default :
				throw new OCLException("Update type not supported.");
		}
		// TODO Shouldn't have to do this Semantic checking or parsing should deal with it
		res.setFluent(true);
		return res;
	}

	/**
	 * evalBoolean
	 * evaluate the boolean expression against the current state fluents
	 * @param - the list of state fluents
	 * @return - true or false
	 */
	public boolean evalBoolean(List stateFluents) throws OCLException {
		boolean res, blhs, brhs;
		double rhs, lhs;
		switch (exprType) {
			case AND :
				blhs = lhs_expr.evalBoolean(stateFluents);
				brhs = rhs_expr.evalBoolean(stateFluents);
				res = brhs && blhs;
				break;
			case OR :
				blhs = lhs_expr.evalBoolean(stateFluents);
				brhs = rhs_expr.evalBoolean(stateFluents);
				res = brhs || blhs;
				break;
			case NOT :
				brhs = rhs_expr.evalBoolean(stateFluents);
				res = !brhs;
				break;
			case EQUAL :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = rhs == lhs;
				break;
			case LESS :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = lhs < rhs;
				break;
			case LESSEQ :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = lhs <= rhs;
				break;
			case GREATER :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = lhs > rhs;
				break;
			case GREATEREQ :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = lhs >= rhs;
				break;
			default :
				throw new OCLException("Unexpected ocl expression type");
		}
		return res;
	}

	/**
	 * eval
	 * calculate the numeric result of an expression
	 * @param - the state fluent list
	 * @return - the numeric value
	 */
	public double eval(List stateFluents) throws OCLException {
		double lhs, rhs, res;
		switch (exprType) {
			case PLUS :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = rhs + lhs;
				break;
			case MINUS :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = rhs - lhs;
				break;
			case TIMES :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = rhs * lhs;
				break;
			case DIV :
				lhs = lhs_expr.eval(stateFluents);
				rhs = rhs_expr.eval(stateFluents);
				res = rhs / lhs;
				break;
			case UMINUS :
				rhs = rhs_expr.eval(stateFluents);
				res = rhs * -1;
				break;
			case FLUENT_EXPR :
				res = getStateValue((oclPredicate) value, stateFluents);
				break;
			case DOUBLE_EXPR :
				res = ((Double) value).doubleValue();
				break;
			case TIME_EXPR :
				res = 1.0;
				break;
			case UNARY_EXPR :
				res = rhs_expr.eval(stateFluents);
				break;
			case VAR_EXPR :
				throw new OCLException("Dont use stand alone variables.");
			default :
				throw new OCLException(
					"Unexpected ocl expression type." + exprType);
		}
		return res;
	}

	/**
	 * getStateValue
	 * find he value for the given fluent predicate
	 * @param fluent
	 * @param stateFluents
	 */
	private double getStateValue(oclPredicate fluent, List stateFluents)
		throws OCLException {
		double res = 0.0;
		boolean found = false;
		ListIterator li = stateFluents.listIterator();
		while (!found && li.hasNext()) {
			oclPredicate clause = (oclPredicate) li.next();
			if (clause.toString().equals(fluent.toString())) {
				res = clause.getFluentValue();
				found = true;
			}
		}
		if (!found) {
			throw new OCLException(
				"Cannot find fluent value" + fluent.toString());
		}
		return res;
	}

	/**
	 * getStateFluent
	 * find he value for the given fluent predicate
	 * @param fluent
	 * @param stateFluents
	 */
	private oclPredicate getStateFluent(oclPredicate fluent, List stateFluents)
		throws OCLException {
		oclPredicate res = null;
		;
		boolean found = false;
		ListIterator li = stateFluents.listIterator();
		while (!found && li.hasNext()) {
			oclPredicate clause = (oclPredicate) li.next();
			if (clause.toString().equals(fluent.toString())) {
				res = clause;
				found = true;
			}
		}
		if (!found) {
			throw new OCLException("Cannot find fluent.");
		}
		return res;
	}

	/**
	 * eval
	 * evaluate the numeric expression
	 */
	/**
	 * clone a copy of oclPredicate
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		oclExpression res = new oclExpression(exprType);
		if (exprType == FLUENT_EXPR) {
			res.value = (oclPredicate) ((oclPredicate) value).clone();
		} else if (exprType == DOUBLE_EXPR) {
			res.value = new Double(((Double) value).doubleValue());
		} else if (exprType == VAR_EXPR) {
			res.value = (String) value;
		}
		if (rhs_expr != null) {
			res.rhs_expr = (oclExpression) rhs_expr.clone();
		}
		if (lhs_expr != null) {
			res.lhs_expr = (oclExpression) lhs_expr.clone();
		}
		return (oclExpression) res;
	}

	/**
	 * to print the current oclExpression as a String
	 * @return - the string
	 */
	public String toString() {
		String res = "";
		switch (exprType) {
			case TEST :
				res = "test(";
				res = res + rhs_expr.toString();
				res = res + ")";
				break;
			case ASSIGN :
				res = "assign(";
				res = res + lhs_expr.toString();
				res = res + ",";
				res = res + rhs_expr.toString();
				res = res + ")";
				break;
			case INCREASE :
				res = "increase(";
				res = res + lhs_expr.toString();
				res = res + ",";
				res = res + rhs_expr.toString();
				res = res + ")";
				break;
			case DECREASE :
				res = "decrease(";
				res = res + lhs_expr.toString();
				res = res + ",";
				res = res + rhs_expr.toString();
				res = res + ")";
				break;
			case EQUAL :
				res = res + lhs_expr.toString();
				res = res + " = ";
				res = res + rhs_expr.toString();
				break;
			case LESS :
				res = res + lhs_expr.toString();
				res = res + " < ";
				res = res + rhs_expr.toString();
				break;
			case LESSEQ :
				res = res + lhs_expr.toString();
				res = res + " <= ";
				res = res + rhs_expr.toString();
				break;
			case GREATER :
				res = res + lhs_expr.toString();
				res = res + " > ";
				res = res + rhs_expr.toString();
				break;
			case GREATEREQ :
				res = res + lhs_expr.toString();
				res = res + " >= ";
				res = res + rhs_expr.toString();
				break;
			case AND :
				res = res + lhs_expr.toString();
				res = res + " and ";
				res = res + rhs_expr.toString();
				break;
			case OR :
				res = res + lhs_expr.toString();
				res = res + " or ";
				res = res + rhs_expr.toString();
				break;
			case NOT :
				res = res + "not(";
				res = res + rhs_expr.toString();
				res = res + ")";
				break;
			case PLUS :
				res = res + lhs_expr.toString();
				res = res + " + ";
				res = res + rhs_expr.toString();
				break;
			case MINUS :
				res = res + lhs_expr.toString();
				res = res + " - ";
				res = res + rhs_expr.toString();
				break;
			case TIMES :
				res = res + lhs_expr.toString();
				res = res + " * ";
				res = res + rhs_expr.toString();
				break;
			case DIV :
				res = res + lhs_expr.toString();
				res = res + " / ";
				res = res + rhs_expr.toString();
				break;
			case DOUBLE_EXPR :
				Double val = (Double) value;
				res = res + val.toString();
				break;
			case FLUENT_EXPR :
				oclPredicate fluent = (oclPredicate) value;
				res = res + fluent.toString();
				break;
			case TIME_EXPR :
				res = res + "#t";
				break;
			case UNARY_EXPR :
				res = res + "(";
				res = res + rhs_expr.toString();
				res = res + ")";
				break;
			case VAR_EXPR :
				res = res + " " + ((String) value) + " ";
				break;
			default :
				res = "Expression no print routine yet.(" + exprType + ")";
		}
		return res;
	}

	/**
	 * isAssignmentNoOp
	 * check to see if expression used to set value to existing value
	 * @return true if No Op
	 */
	public boolean isAssignmentNoOp() {
		if (exprType == ASSIGN) {
			if (rhs_expr.getExprtType() == FLUENT_EXPR) {
				oclPredicate lhsPred = (oclPredicate) lhs_expr.getValue();
				oclPredicate rhsPred = (oclPredicate) rhs_expr.getValue();
				if (lhsPred.equals(rhsPred)) {
					return true;
				} else {
					return false;
				}
			} else {
				return false;
			}
		} else {
			return false;
		}

	}

	/**
	 * to print the current oclPredicate to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * @return void
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		switch (exprType) {
			case TEST :
				ps.print(pad + "test(");
				rhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(")");
				break;
			case ASSIGN :
				ps.print(pad + "assign(");
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(",");
				rhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(")");
				break;
			case INCREASE :
				ps.print(pad + "increase(");
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(",");
				rhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(")");
				break;
			case DECREASE :
				ps.print(pad + "decrease(");
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(",");
				rhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(")");
				break;
			case EQUAL :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" = ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case LESS :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" < ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case LESSEQ :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" <= ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case GREATER :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" > ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case GREATEREQ :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" >= ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case AND :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" and ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case OR :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" or ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case NOT :
				ps.print("not(");
				rhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(")");
				break;
			case PLUS :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" + ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case MINUS :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" - ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case TIMES :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" * ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case DIV :
				lhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(" / ");
				rhs_expr.oclPrintComponent(ps, 0, false);
				break;
			case DOUBLE_EXPR :
				Double val = (Double) value;
				ps.print(val.toString());
				break;
			case FLUENT_EXPR :
				oclPredicate fluent = (oclPredicate) value;
				fluent.oclPrintComponent(ps, 0, false);
				break;
			case TIME_EXPR :
				ps.print("#t");
				break;
			case UNARY_EXPR :
				ps.print("(");
				rhs_expr.oclPrintComponent(ps, 0, false);
				ps.print(")");
				break;
			case VAR_EXPR :
				ps.print(" " + (String) value + " ");
				break;
			default :
				ps.print("Expression no print routine yet.(" + exprType + ")");
		}
	}

	/**
	* to translate and print to a PrintWriter.
	* @param curDom - the current full domain
	* @param ps PrintWriter
	* @param indent value of indentation
	* @param nline if true print new line on completion
	* @return void
	*/
	public void pddlPrint(
		oclDomain curDom,
		PrintWriter ps,
		int indent,
		boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		switch (exprType) {
			case TEST :
				ps.print(pad + "(");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				if (nline)
					ps.println(")");
				else
					ps.print(")");
				break;
			case ASSIGN :
				ps.print(pad + "(assign ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				if (nline)
					ps.println(")");
				else
					ps.print(")");
				break;
			case INCREASE :
				ps.print(pad + "(increase ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				if (nline)
					ps.println(")");
				else
					ps.print(")");
				break;
			case DECREASE :
				ps.print(pad + "(decrease ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				if (nline)
					ps.println(")");
				else
					ps.print(")");
				break;
			case EQUAL :
				ps.print("= ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				break;
			case LESS :
				ps.print("< ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				break;
			case LESSEQ :
				ps.print("<= ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				break;
			case GREATER :
				ps.print("> ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				break;
			case GREATEREQ :
				ps.print(">= ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				break;
			case AND :
				ps.print("(and ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(")");
				break;
			case OR :
				ps.print(("or "));
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(")");
				break;
			case NOT :
				ps.print("(not ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(")");
				break;
			case PLUS :
				ps.print("(+ ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(")");
				break;
			case MINUS :
				ps.print("(- ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(")");
				break;
			case TIMES :
				ps.print("(* ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(")");
				break;
			case DIV :
				ps.print("(/ ");
				lhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(" ");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(")");
				break;
			case DOUBLE_EXPR :
				Double val = (Double) value;
				ps.print(val.toString());
				break;
			case FLUENT_EXPR :
				oclPredicate fluent = (oclPredicate) value;
				fluent.pddlPrint(curDom, ps, 0, false);
				break;
			case TIME_EXPR :
				ps.print("#t");
				break;
			case UNARY_EXPR :
				ps.print("(");
				rhs_expr.pddlPrint(curDom, ps, 0, false);
				ps.print(")");
				break;
			case VAR_EXPR :
				ps.print(" " + (String) value + " ");
				break;
			default :
				ps.print("Expression no print routine yet.(" + exprType + ")");
		}
	}

}

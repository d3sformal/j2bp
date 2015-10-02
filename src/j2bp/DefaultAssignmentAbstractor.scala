/*
 * Copyright (C) 2015, Charles University in Prague.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package j2bp

import scala.math._
import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Label

import common._


class DefaultAssignmentAbstractor extends AssignmentAbstractor with StatementResolver
{
	def generateAbstractStore(ctx : AbstractionContext, mv : MethodVisitor, tgtVarExpr : Expression, srcValueExpr : Expression, bcIndex : Int, tgtVarType : String) : Unit =
	{
		// update values of boolean variables that represent predicates over "tgtVarExpr" based on the "srcValueExpr" and predicates over "srcValueExpr"

		// all the assignments must be done atomically (without thread switch in the middle)

		if (Main.INFO) println("[INFO] assignment: target variable = " + tgtVarExpr + ", source expression = '" + srcValueExpr + "'")

		GenUtils.generateBeginAtomic(mv)

		
		// find all predicates that involve the target variable (and its aliases)
		
		var tgtVarPredSet : Set[AtomicPredicate] = null
		
		if (ExpressionUtils.isArrayAccess(tgtVarExpr))
		{
			val arrayAccessExpr = tgtVarExpr.asInstanceOf[FunctionExpression]
			
			var arrayExpr = arrayAccessExpr.args(1)
			
			while (ExpressionUtils.isArrayAccess(arrayExpr))
			{
				arrayExpr = arrayExpr.asInstanceOf[FunctionExpression].args(1)
			}
			
			tgtVarPredSet = findPredicatesOverVariable(ctx, arrayExpr, -1)
		}
		else
		{
			// field access, simple assignment
			
			tgtVarPredSet = findPredicatesOverVariable(ctx, tgtVarExpr, -1)
		}
				
		var tgtVarPredList = tgtVarPredSet.toList
			
		if (Main.INFO) FormulaUtils.printAtomPredSet(tgtVarPredSet, "[INFO] predicates over target variable:")
		
		
		if (srcValueExpr.toString() == Constants.STACK_ELEM_NEWOBJ)
		{
			// source value is reference to a newly created object ("[newobj]")
			
			val isNewArray : Boolean = (tgtVarType.charAt(0) == '[')
			
			// boolean variables for all predicates over fields of "tgtVarExpr" will be set to a non-deterministic value (no need for theorem prover calls)
			// special case: all predicates of the form 'v = p' (aliasing) are set to 'false' automatically
			
			for (tgtPred <- tgtVarPredList)
			{
				if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] current predicate = '" + tgtPred + "'")
				
				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtPred).get
				
				val tgtPredOperands = FormulaUtils.extractOperands(tgtPred)				
				
				
				if (tgtPred.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(tgtPred.asInstanceOf[BinaryPredicate], tgtVarExpr))
				{
					// load 'false'
					mv.visitInsn(Opcodes.ICONST_0)
					
					if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if (tgtPred.isInstanceOf[BinaryPredicate] && tgtPredOperands.contains(tgtVarExpr) && tgtPredOperands.contains(Constants.NULL_EXPR))
				{
					if (tgtPred.getOperator() == "=")
					{
						mv.visitInsn(Opcodes.ICONST_0)
						
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
					}
					else
					{
						mv.visitInsn(Opcodes.ICONST_1)
						
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = true")
					}
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if (tgtPred.isInstanceOf[BinaryPredicate] && tgtPredOperands.contains(tgtVarExpr) && (tgtPred.getOperator() == "="))
				{
					// aliasing with something else than a variable name
					
					mv.visitInsn(Opcodes.ICONST_0)
						
					if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else if (isNewArray)
				{
					var isPredOverLength = false
					
					for (funcOperand <- FormulaUtils.extractFunctionOperands(tgtPred))
					{
						if ((funcOperand.name == Constants.FUNC_FIELD_READ) && (funcOperand.args(0).toString() == "length")) isPredOverLength = true
					}
					
					// predicates over length are already set
					if ( ! isPredOverLength )
					{
						mv.visitInsn(Opcodes.ICONST_0)
						
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
					
						GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
					}
				}
				else if (FormulaUtils.containsFieldReadOverObjExpr(tgtPred, tgtVarExpr))
				{
					// field read on target expression

					mv.visitInsn(Opcodes.ICONST_0)
						
					if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else
				{
					// generate call to Verify.getBoolean
					GenUtils.generateChooseBool(mv)
						
					if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = choose bool")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
			}
		}
		else if (srcValueExpr.toString() == Constants.VALUE_NULL)
		{
			// predicate "v = null" is set to true
			// other predicates over target expression are set to false
			
			for (tgtPred <- tgtVarPredList)
			{
				if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] current predicate = '" + tgtPred + "'")
				
				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtPred).get
				
				val tgtPredOperands = FormulaUtils.extractOperands(tgtPred)
				
			
				if (tgtPred.isInstanceOf[BinaryPredicate] && tgtPredOperands.contains(tgtVarExpr) && tgtPredOperands.contains(Constants.NULL_EXPR))
				{
					if (tgtPred.getOperator() == "=")
					{
						mv.visitInsn(Opcodes.ICONST_1)
						
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = true")
					}
					else
					{
						mv.visitInsn(Opcodes.ICONST_0)
						
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
					}
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
				else
				{
					mv.visitInsn(Opcodes.ICONST_0)
						
					if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
					
					GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
				}
			}			
		}		
		else if (ExpressionUtils.isVariableName(srcValueExpr))
		{
			// given expression (srcValueExpr) is only a variable name (e.g., in case of assignment "v := p") or a field access path (e.g., "v := p.f")
			
			// find all predicates involving the source variable			
			var srcVarPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), srcValueExpr.toString(), -1)

			// add predicates over operands that are not constant values
			var newOperandPreds = Set[AtomicPredicate]()
			for (srcPred <- srcVarPredSet)
			{
				for (operand <- FormulaUtils.extractOperands(srcPred))
				{
					if ( ! ExpressionUtils.isConstantValue(operand) )
					{
						newOperandPreds = newOperandPreds ++ Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), operand.toString(), -1)
					}
				}
			}
			srcVarPredSet = srcVarPredSet ++ newOperandPreds
		
			// for each predicate over the target variable, set the value of boolean variable to the truth value of matching predicate over the source variable
				// matching predicate: "tgtVarExpr" syntactically replaced with "srcVarName"
			// for unmatched predicates over target variable, set their value to unknown (non-deterministic boolean value) 
				// this includes predicates with function expressions over the target variable, for which does not exist matching predicates over source variable 
			// special case: all predicates of the form 'v = p' (aliasing) are set to 'true' automatically
			// no need for theorem prover calls in this case
			
			// load new values on the stack
			for ( tgtPred <- tgtVarPredList )
			{
				if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] current predicate = '" + tgtPred + "'")
				
				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtPred).get
			
				if (tgtPred.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(tgtPred.asInstanceOf[BinaryPredicate], tgtVarExpr, srcValueExpr))
				{
					// load 'true'
					mv.visitInsn(Opcodes.ICONST_1)
					
					if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] target variable name = '" + tgtPredBoolVar + "', new value = true")
				}
				else
				{					
					val matcherSrcPred = FormulaUtils.copyWithReplace(tgtPred, tgtVarExpr.toString(), srcValueExpr.toString())
				
					val matchingSrcPredOpt = srcVarPredSet.find(_ == matcherSrcPred)
										
					if (matchingSrcPredOpt == None)
					{
						// generate call to Verify.getBoolean
						GenUtils.generateChooseBool(mv)
						
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = choose bool")
					}
					else
					{
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] matched predicate = '" + matcherSrcPred + "'")
						
						val srcPredBoolVar = ctx.getVariableForPredicate(matchingSrcPredOpt.get).get
						
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] source variable name = " + srcPredBoolVar)
						
						GenUtils.generateLoadInstruction(srcPredBoolVar, mv, ctx)
					}
				}
			}
			
			// save new values into variables (they are taken from stack in reversed order)
			for ( tgtPred <- tgtVarPredList.reverse )
			{
				if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] current predicate = '" + tgtPred + "'")
				
				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtPred).get
				
				GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
			}
		}
		else
		{
			// we have an expression that may contain some variable names and integer constants
			
			if (Main.INFO) 
			{
				println("[INFO] assignment '" + tgtVarExpr + " = " + srcValueExpr + "', updated predicates count = " + tgtVarPredSet.size())
				
				FormulaUtils.printAtomPredSet(tgtVarPredSet, "[INFO] updated predicates (over target expression):")
			}			

			val tgtVarPredList = tgtVarPredSet.toList
			
			for (tgtVarPred <- tgtVarPredList)
			{
				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtVarPred).get
				
				val tgtPredOperands = FormulaUtils.extractOperands(tgtVarPred)

				var resdetPredSet = Set[AtomicPredicate]()

				// collect all predicates over variables in the source expression and all predicates over variables that are referenced in the target predicate over the target variable  
			
				for (srcVarSubExpr <- ExpressionUtils.extractVariableExpressions(srcValueExpr))
				{
					if ( ! ExpressionUtils.isConstantExpression(srcVarSubExpr) )
					{
						// find all predicates involving the variable expression that does not have a constant value
				
						var exprPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), srcVarSubExpr.toString(), -1)
					
						resdetPredSet = resdetPredSet ++ exprPredSet
					}
				}

				// add predicates over operands that are not constant values
				var newOperandPreds = Set[AtomicPredicate]()
				for (resPred <- resdetPredSet)
				{
					for (operand <- FormulaUtils.extractOperands(resPred))
					{
						if ( ! ExpressionUtils.isConstantValue(operand) )
						{
							newOperandPreds = newOperandPreds ++ Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), operand.toString(), -1)
						}
					}
				}
				resdetPredSet = resdetPredSet ++ newOperandPreds
	
				// take predicates over array indexes
				var curTgtSubExpr = tgtVarExpr
				while (ExpressionUtils.isArrayAccess(curTgtSubExpr))
				{
					val arrayAccessExpr = curTgtSubExpr.asInstanceOf[FunctionExpression]
	
					var arrayExpr = arrayAccessExpr.args(1)
					var indexExpr = arrayAccessExpr.args(2)
				
					for (indexSubExpr <- ExpressionUtils.extractVariableExpressions(indexExpr))
					{
						for (isePred <- Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), indexSubExpr.toString(), -1))
						{
							if (isePred.containsOperand(indexSubExpr)) resdetPredSet = resdetPredSet + isePred
						}
					}
	
					curTgtSubExpr = arrayExpr
				}

				// take predicates over variables referred to by already available predicates over the target variable, and also predicates over their operands
				// consider only binary predicates where one operand is variable name or expression in question and the other operand is a constant value
			
				val tgtPredVarNames = FormulaUtils.extractVariableNames(tgtVarPred)
				for (varName <- tgtPredVarNames if (varName != tgtVarExpr.toString()))
				{
					// find all predicates involving the variable			
					var varPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), varName, -1)
					
					for (varPred <- varPredSet if varPred.isInstanceOf[BinaryPredicate])
					{
						val binVarPred = varPred.asInstanceOf[BinaryPredicate]
						
						if (ExpressionUtils.isVariableName(binVarPred.left) && ExpressionUtils.isConstantValue(binVarPred.right)) resdetPredSet = resdetPredSet + varPred
						else if (ExpressionUtils.isVariableName(binVarPred.right) && ExpressionUtils.isConstantValue(binVarPred.left)) resdetPredSet = resdetPredSet + varPred
					}
				}

				for (operand <- tgtPredOperands if (operand != tgtVarExpr))
				{
					var operandPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), operand.toString(), -1)
					
					for (operandPred <- operandPredSet if operandPred.isInstanceOf[BinaryPredicate])
					{
						val binOperandPred = operandPred.asInstanceOf[BinaryPredicate]
						
						if ((binOperandPred.left == operand) && ExpressionUtils.isConstantValue(binOperandPred.right)) resdetPredSet = resdetPredSet + operandPred
						else if ((binOperandPred.right == operand) && ExpressionUtils.isConstantValue(binOperandPred.left)) resdetPredSet = resdetPredSet + operandPred
					}
				}
			
				if (Main.INFO) 
				{
					println("[INFO] updated predicate = '" + tgtVarPred + "'")
					
					FormulaUtils.printAtomPredSet(resdetPredSet, "[INFO] result determining predicates (over source and target expression):")
				}
			

				if (resdetPredSet.size > 0)
				{
					// determine new values of boolean variables that correspond to predicates over target variable and update them
	
					resolveAssignment(ctx, mv, tgtVarExpr, Set[AtomicPredicate](tgtVarPred), srcValueExpr, resdetPredSet, false)
				}
				else
				{
					// there are no result-determining predicates in this case
	
					// we set the new value of some predicates based on their structure and operands
					// we use non-deterministic boolean value for other predicates

					var unknownNewValue = true
				
					if (tgtVarPred.isInstanceOf[BinaryPredicate])
					{
						val tgtVarPredOperands = FormulaUtils.extractOperands(tgtVarPred)
						
						if (tgtVarPredOperands.contains(tgtVarExpr) && tgtVarPredOperands.contains(srcValueExpr))
						{
							if (tgtVarPred.getOperator() == "=") 
							{
								mv.visitInsn(Opcodes.ICONST_1)

								if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = true")
							}
							else
							{
								mv.visitInsn(Opcodes.ICONST_0)

								if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
							}

							unknownNewValue = false
						}
						else if (tgtVarPredOperands.contains(tgtVarExpr) && ExpressionUtils.isConstantValue(srcValueExpr))
						{
							// different constant value than srcValueExpr
							
							if (tgtVarPred.getOperator() == "=") 
							{
								mv.visitInsn(Opcodes.ICONST_0)

								if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = false")
							}
							else
							{
								mv.visitInsn(Opcodes.ICONST_1)

								if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = true")
							}

							unknownNewValue = false
						}						
					}

					// use non-deterministic boolean value if we do not know a precise value
					if (unknownNewValue) 
					{
						GenUtils.generateChooseBool(mv)

						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] variable name = '" + tgtPredBoolVar + "', new value = choose bool")
					}
				}
			}
			
			// save new truth values into boolean variables (they are taken from stack in reversed order)
			for (tgtVarPred <- tgtVarPredList.reverse)
			{
				if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateAbstractStore] updated predicate = '" + tgtVarPred.toString() + "'")

				val tgtPredBoolVar = ctx.getVariableForPredicate(tgtVarPred).get

				GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
			}
		}
		
		GenUtils.generateEndAtomic(mv)
	}

	def generateAbstractPutfield(ctx : AbstractionContext, mv : MethodVisitor, tgtObj : Expression, fieldName : String, srcValueExpr : Expression, bcIndex : Int, fieldType : String) =
	{
		// update values of boolean variables that represent predicates over "varname.fieldname" based on the "valueExpr" and predicates over "valueExpr"

		generateAbstractStore(ctx, mv, ExpressionUtils.createFieldAccessExpr(tgtObj, fieldName), srcValueExpr, bcIndex, fieldType)
	}
	
	def generateVariableLoad(ctx : AbstractionContext, mv : MethodVisitor, tgtVarExpr : Expression, tgtPred : AtomicPredicate, srcValueExpr : Expression, srcPredSet : Set[AtomicPredicate]) =
	{
		// load boolean value that represent the truth value of predicate "tgtPred" based on the predicates in the "srcPredSet"

		// all loading operations must be done atomically (without thread switch in the middle)

		if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.generateVariableLoad] variable expression = " + tgtVarExpr + ", target predicate = " + tgtPred.toString())
		
		if (Main.DEBUG) FormulaUtils.printAtomPredSet(srcPredSet, "[DEBUG DefaultAssignmentAbstractor.generateVariableLoad] source predicates:")
		
		if (Main.INFO) 
		{
			println("[INFO] variable load for '" + tgtVarExpr + "', target predicate = '" + tgtPred.toString() + "', result determining predicates count = " + srcPredSet.size())
			
			FormulaUtils.printAtomPredSet(srcPredSet, "[INFO] result determining predicates:")
		}		
		
		// determine new values of boolean variables that correspond to target predicates
		
		resolveAssignment(ctx, mv, tgtVarExpr, Set[AtomicPredicate](tgtPred), srcValueExpr, srcPredSet, false)
	}

	protected def resolveAssignment(ctx : AbstractionContext, mv : MethodVisitor, tgtVarExpr : Expression, tgtVarPredSet : Set[AtomicPredicate], srcValueExpr : Expression, srcValuePredSet : Set[AtomicPredicate], writeToVariables : Boolean) =
	{
		// solve the assignment for given sets of predicates (with calls of theorem prover) and generate necessary code
		
		// weakest preconditions for predicates over target variable are computed in advance here -> we use syntactic replacement of tgtVarExpr with srcValueExpr to model assignment

		val tgtVarPred2PosWP : Map[AtomicPredicate, LogicFormula] = new HashMap
		val tgtVarPred2NegWP : Map[AtomicPredicate, LogicFormula] = new HashMap

		for (pred <- tgtVarPredSet) 
		{
			var funcOverTgtExpr = false
			
			val predFuncExprs = FormulaUtils.extractFunctionExpressionsRecursively(pred)
			
			for (funcExpr <- predFuncExprs)
			{
				for (funcArg <- funcExpr.args)
				{
					if (funcArg.contains(tgtVarExpr.toString())) funcOverTgtExpr = true
				}
			}
			
			if (funcOverTgtExpr)
			{
				// if the predicate contains function over the target expression then invalidate its value (set its value to unknown by making both preconditions false)
				
				tgtVarPred2PosWP.put(pred, Constants.FALSE_PRED)
				
				tgtVarPred2NegWP.put(pred, Constants.FALSE_PRED)
			}
			else if (ExpressionUtils.isFieldAccessPath(tgtVarExpr)) 
			{
				// create weakest precondition according to the template "fread(fwrite(f,v,e),v)"

				val fapTgtObjExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(tgtVarExpr)
				
				val fieldNames = ExpressionUtils.extractFieldNamesFromFieldAccessPath(tgtVarExpr)

				val fieldUpdateGetExpr = ExpressionUtils.createFieldUpdateGetExpr(fapTgtObjExpr, fieldNames, srcValueExpr)

				tgtVarPred2PosWP.put(pred, FormulaUtils.copyWithReplace(pred, tgtVarExpr.toString(), fieldUpdateGetExpr.toString()))				
			}
			else if (ExpressionUtils.isArrayAccess(tgtVarExpr))
			{
				// create weakest precondition according to the template "aread(awrite(arr,a,i,e),a,i)"

				val tgtArrayAccessExpr = tgtVarExpr.asInstanceOf[FunctionExpression]
			
				val tgtFullArrayExpr = tgtArrayAccessExpr.args(1)
				val tgtIndexExpr = tgtArrayAccessExpr.args(2)
				
				var tgtArrayVar : Expression = null
				
				// find the array variable
				var curArrayExpr = tgtFullArrayExpr				
				while (ExpressionUtils.isArrayAccess(curArrayExpr))
				{
					curArrayExpr = curArrayExpr.asInstanceOf[FunctionExpression].args(1)	
				}
				tgtArrayVar = curArrayExpr
				
				if (pred.isInstanceOf[BinaryPredicate])
				{
					val binPred = pred.asInstanceOf[BinaryPredicate]
					
					var predArrayOperand : Expression = null
					var predValueOperand : Expression = null
					
					if (ExpressionUtils.isArrayAccess(binPred.left)) 
					{
						predArrayOperand = binPred.left
						predValueOperand = binPred.right
					}
					else if (ExpressionUtils.isArrayAccess(binPred.right)) 
					{
						predArrayOperand = binPred.right
						predValueOperand = binPred.left
					}
					
					if (predArrayOperand != null)
					{
						val predArrayAccessExpr = predArrayOperand.asInstanceOf[FunctionExpression]
						
						val predFullArrayExpr = predArrayAccessExpr.args(1)
						val predIndexExpr = predArrayAccessExpr.args(2)
						
						// we use the generic function "update" here instead of specific function "awrite"
						val arrayUpdateGetExpr = new FunctionExpression(Constants.FUNC_ARRAY_READ, Array[Expression](new FunctionExpression(Constants.ARRAY_UPDATE_OPER, Array[Expression](new Expression(Constants.FUNC_ARRAY), tgtFullArrayExpr, tgtIndexExpr, srcValueExpr)), predFullArrayExpr, predIndexExpr))
						
						val posWP = new BinaryPredicate(binPred.op, arrayUpdateGetExpr, predValueOperand)
					
						tgtVarPred2PosWP.put(pred, posWP)
					}
				}
			}
			
			if ( ! tgtVarPred2PosWP.contains(pred) )
			{
				tgtVarPred2PosWP.put(pred, FormulaUtils.copyWithReplace(pred, tgtVarExpr.toString(), srcValueExpr.toString()))
			}
			
			if ( ! tgtVarPred2NegWP.contains(pred) )
			{
				tgtVarPred2NegWP.put(pred, FormulaUtils.negate(tgtVarPred2PosWP.getOrElse(pred, pred)))
			}
		}

		resolve(ctx, mv, tgtVarExpr, tgtVarPredSet, srcValuePredSet, tgtVarPred2PosWP, tgtVarPred2NegWP, writeToVariables)
	}

	protected def findPredicatesOverVariable(ctx : AbstractionContext, tgtVarExpr : Expression, bcIndex : Int) : Set[AtomicPredicate] =
	{	
		// find all predicates that involve the target variable
		
		var tgtVarPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtVarExpr.toString(), bcIndex)
		
	
		// when the assignment defines new value of the field 'f' of an object referred to by variable 'v', we must consider predicates over 'f' for each variable aliased with 'v'
		if (ExpressionUtils.isFieldAccessPath(tgtVarExpr))
		{
			val fapTgtObjExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(tgtVarExpr)			
			
			if (fapTgtObjExpr.toString() != "this")
			{
				val aliasingPreds = Configuration.predicatesMngr.getAliasingPredicates()
				
				for (aliasPred <- aliasingPreds)
				{
					val binAliasPred = aliasPred.asInstanceOf[BinaryPredicate]
					
					var aliasedVarExpr : Expression = null
					
					if (binAliasPred.left == fapTgtObjExpr) aliasedVarExpr = binAliasPred.right
					else if (binAliasPred.right == fapTgtObjExpr) aliasedVarExpr = binAliasPred.left

					if (aliasedVarExpr != null)
					{
						if (Main.DEBUG) println("[DEBUG DefaultAssignmentAbstractor.findPredicatesOverVariable] aliased variable = " + aliasedVarExpr)
				
						tgtVarPredSet = tgtVarPredSet ++ Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtVarExpr.replace(fapTgtObjExpr.toString(), aliasedVarExpr.toString()).toString(), bcIndex)
					}
				}
			}
		}
		
		return tgtVarPredSet
	}
}

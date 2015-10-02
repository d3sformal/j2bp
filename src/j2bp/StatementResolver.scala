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

import common.Constants
import common.Expression
import common.LogicFormula
import common.AtomicPredicate
import common.UnaryPredicate
import common.BinaryPredicate
import common.Negation
import common.FormulaUtils
import common.ExpressionUtils


trait StatementResolver
{
	/**
	 * Generates large if-else statement with one block for each combination of truth values of predicates in the result-determining list. Each block updates boolean variables corresponding to predicates in the to-be-updated list. Uses theorem prover to find new values of the boolean variables. 
	 * If the parameter "writeToVariables" is true then computed boolean values are stored into respective variables, otherwise they are only added to the stack (so that they can serve as arguments for the subsequent method call).
	 */
	def resolve(ctx : AbstractionContext, mv : MethodVisitor, targetVarExpr : Expression, updatedPredSet : Set[AtomicPredicate], resdetPredSet : Set[AtomicPredicate], updatedPred2PosWP : Map[AtomicPredicate, LogicFormula], updatedPred2NegWP : Map[AtomicPredicate, LogicFormula], writeToVariables : Boolean) =
	{
		if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] resolve start")
		
		var updatedPredList : List[AtomicPredicate] = updatedPredSet.toList
		var resdetPredList : List[AtomicPredicate] = resdetPredSet.toList
		
		
		// get equality predicates about temporary variables

		val tempvar2eqlpreds : Map[String, Set[AtomicPredicate]] = new HashMap
		
		for ( resPred <- resdetPredSet if (resPred.isInstanceOf[BinaryPredicate] && (resPred.getOperator() == "=")) )
		{
			val binResPred = resPred.asInstanceOf[BinaryPredicate]
			
			var tempVarName : String = null
			var otherOperand : Expression = null
			
			if ( ExpressionUtils.isTemporaryVariable(binResPred.left) && ( ! ExpressionUtils.isTemporaryReturnVariable(binResPred.left) ) ) 
			{
				tempVarName = binResPred.left.toString()
				otherOperand = binResPred.right
			}
			
			if ( ExpressionUtils.isTemporaryVariable(binResPred.right) && ( ! ExpressionUtils.isTemporaryReturnVariable(binResPred.right) ) ) 
			{
				tempVarName = binResPred.right.toString()
				otherOperand = binResPred.left
			}
			
			if ((tempVarName != null) && ExpressionUtils.isConstantValue(otherOperand))
			{				
				var tempVarPreds = tempvar2eqlpreds.getOrElse(tempVarName, Set[AtomicPredicate]())
				
				tempVarPreds = tempVarPreds + resPred
				
				tempvar2eqlpreds.put(tempVarName, tempVarPreds)
			}
		}
		
		if (Main.DEBUG)
		{
			for ( (tempVarName, tempVarPreds) <- tempvar2eqlpreds )
			{
				println("[DEBUG StatementResolver.resolve] temporary variable = '" + tempVarName + "'")
				FormulaUtils.printAtomPredSet(tempVarPreds, "[DEBUG StatementResolver.resolve] equality predicates for the variable:")
			}
		}
		
		
		// get aliasing predicates over target variable expression and all relevant predicates for each alias
		
		val alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]] = new HashMap
		
		for (resPred <- resdetPredSet if resPred.isInstanceOf[BinaryPredicate])
		{
			val binResPred = resPred.asInstanceOf[BinaryPredicate]
			
			if ((targetVarExpr != null) && FormulaUtils.isAliasingPredicate(binResPred, targetVarExpr))
			{
				val aliasingPred = binResPred
				
				val aliasedExpr = FormulaUtils.extractOtherOperandFromBinPred(aliasingPred, targetVarExpr)
				
				// get all result-determinig predicates where "aliasedExpr" is used instead of "tgtVarExpr"
				
				var relevantPreds = Set[AtomicPredicate]()
				
				for ( predOverAlias <- resdetPredSet if predOverAlias.containsExpression(aliasedExpr.toString()) )
				{
					if (predOverAlias != aliasingPred) 
					{
						if (Configuration.predicateSemModel.isRelevantPredicateOverAliasedExpr(predOverAlias, targetVarExpr, aliasedExpr))
						{						
							relevantPreds = relevantPreds + predOverAlias
						}
					}
				}
				
				if (Main.DEBUG)
				{
					println("[DEBUG StatementResolver.resolve] aliasing predicate = '" + aliasingPred + "'")
					FormulaUtils.printAtomPredSet(relevantPreds, "[DEBUG StatementResolver.resolve] relevant predicates for the alias:")
				}
				
				alias2relevantpreds.put(aliasingPred, relevantPreds)
			}
		}
					
		
		// load new values on the stack
		for (updatedPred <- updatedPredList)
		{
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] new updated predicate")
			
			if (Main.DEBUG) println("[DEBUG StatementResolver.resolve] updated predicate = '" + updatedPred + "'")

			
			var updatedPredBoolVar : String = ""

			// get boolean variable for the to-be-updated predicate
			// we do not need the variable if we are not going to write into it (and there may not be such variable in that case anyway)
			if (writeToVariables) updatedPredBoolVar = ctx.getVariableForPredicate(updatedPred).get

			
			// use the original to-be-updated predicate if the weakest precondition is not available
			val posWP = updatedPred2PosWP.getOrElse(updatedPred, updatedPred)
			
			// use the original to-be-updated predicate if the weakest precondition is not available
			val negWP = updatedPred2NegWP.getOrElse(updatedPred, FormulaUtils.negate(updatedPred))
			
			
			// check for missing input predicates and record them
			if (Configuration.collectMissingPredicates)
			{
				val wpAtomicPreds = FormulaUtils.extractAtomicPredicates(posWP) ++ FormulaUtils.extractAtomicPredicates(negWP)
				
				for (wpPred <- wpAtomicPreds)
				{
					var candidateMissingPreds = Set[AtomicPredicate]()
					
					var containsTempVar = false
							
					// replace temporary variables with possible constant values and record every variant
					for (vname <- FormulaUtils.extractVariableNames(wpPred))
					{
						if (ExpressionUtils.isTemporaryVariable(vname) && Configuration.predicatesMngr.isReservedName(vname))
						{
							val tmpvarPreds = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), vname, -1)
							
							// consider equality predicates over the temporary variable
							for (tvPred <- tmpvarPreds)
							{
								if (tvPred.isInstanceOf[BinaryPredicate] && tvPred.containsOperand(vname) && (tvPred.getOperator() == "="))
								{
									val eqValue = FormulaUtils.extractOtherOperandFromBinPred(tvPred.asInstanceOf[BinaryPredicate], vname)
									
									// new instance of the given predicate
									val instPred = FormulaUtils.copyWithReplace(wpPred, vname, eqValue.toString())
									
									candidateMissingPreds = candidateMissingPreds + instPred
								}										
							}
						
							containsTempVar = true
						}								
					}
					
					if ( ! containsTempVar )
					{
						candidateMissingPreds = candidateMissingPreds + wpPred
					}

					
					for (cmPred <- candidateMissingPreds)
					{
						if ( ! Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), cmPred, -1) ) 
						{
							var skipPred = false
							
							// ignore predicates with logic variables
							if (FormulaUtils.containsLogicVariables(cmPred)) skipPred = true
							
							if (cmPred.isInstanceOf[BinaryPredicate])
							{
								val cmBinPred = cmPred.asInstanceOf[BinaryPredicate]
								
								// ignore binary predicates where both operands are the same
								if (cmBinPred.left == cmBinPred.right) skipPred = true
								
								// ignore binary predicates where both operands are constant values
								if (ExpressionUtils.isConstantValue(cmBinPred.left) && ExpressionUtils.isConstantValue(cmBinPred.right)) skipPred = true
							}
							
							// ignore predicates containing the "update" function somewhere inside
							val updateExprs = FormulaUtils.extractFunctionExpressionsRecursively(cmPred, Constants.ARRAY_UPDATE_OPER)
							if (updateExprs.size > 0) skipPred = true
							
							if ( ! skipPred )
							{
								Configuration.predicatesMngr.recordMissingPredicate(cmPred)
							}
						}
					}
				}
			}
			
			
			// identify tuples of conflicting literals
			
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] started identification of conflicting literals")
			
			val resdetpred2conflicts : Map[AtomicPredicate, Set[Set[LogicFormula]]] = new HashMap

			for (rdPred <- resdetPredSet)
			{
				// get tuples of literals conflicting with "rdPred"
				val predConflictTuples = Configuration.predicateSemModel.getConflictingLiterals(rdPred, resdetPredSet, posWP, ctx.getCurClassOrigName(), ctx.getCurMethodName())
				
				if (Main.DEBUG) 
				{
					println("[DEBUG StatementResolver.resolve] conflicting literals for the predicate '" + rdPred + "':")
					
					var tupleCount = 0
					
					for (cfLitTuple <- predConflictTuples) 
					{
						tupleCount += 1
						
						FormulaUtils.printFormSet(cfLitTuple, "[DEBUG StatementResolver.resolve] tuple " + tupleCount + ":")
					}
				}
				
				if (predConflictTuples.size > 0) resdetpred2conflicts.put(rdPred, predConflictTuples)
			}
			
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] finished identification of conflicting literals")
			
			
			// create temporary supporting formulas for updated predicate, its weakest precondition, and set of result determining predicates
			val tempSupFormsResultSet = Configuration.predicateSemModel.getTemporarySupportingFormulasResultSet(updatedPred, posWP, resdetPredSet, ctx.getCurClassOrigName(), ctx.getCurMethodName())
			
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] acquired temporary supporting formulas (result set)")
			
			
			// we remember results for cubes (result determining predicates augmented with truth values) in these sets
			// the cubes that yielded positive result and cubes that yielded negative result
			var posResultCubes = Set[Set[LogicFormula]]()
			var negResultCubes = Set[Set[LogicFormula]]()

			
			// put something on the stack so that it is guaranteed in the bytecode that some value is always loaded
			// this dummy value is popped out if we compute the actual value (see the code below) and assign it to the variable
			mv.visitInsn(Opcodes.ICONST_0)
			

			// create Label marking the end of the chain of all combinations for given to-be-updated predicate
			val updatedCombsEndLabel = new Label()

	
			// generate all cubes of the maximal size from the set of result-determining predicates			
			// we use empty cube results holder (last argument) to get all cubes of the maximal size that do not contain conflicting predicates			
			var cubes = generateCubes(resdetPredList, resdetpred2conflicts, tempvar2eqlpreds, alias2relevantpreds)

			// find matching expressions for all variables (program, logic) -> it is used for constructing temporary support formulas 
			var varnames2matchexprs = Configuration.predicateSemModel.findMatchingExpressions(posWP, resdetPredSet, ctx.getCurClassOrigName(), ctx.getCurMethodName())
			
			
			// process all cubes
			
			while (cubes.size > 0)
			{
				if (Main.DEBUG) println("[DEBUG StatementResolver.resolve] remaining cubes for this predicate set = " + cubes.size)

				val resdetCube = cubes.head
				
				cubes = cubes.tail
				
				if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] started processing next cube")
				
				if (Main.DEBUG) FormulaUtils.printFormSet(resdetCube, "[DEBUG StatementResolver.resolve] cube without result:")
				
				// create temporary supporting formulas for updated predicate, its weakest precondition (positive and negative), and result determining cube				
				val tsfrcInfo = Configuration.predicateSemModel.getTemporarySupportingFormulasResultCube(updatedPred, posWP, resdetPredSet, resdetCube, varnames2matchexprs, ctx.getCurClassOrigName(), ctx.getCurMethodName())
				val tempSupFormsResultCube = tsfrcInfo._1
				val logicvar2matchexprs = tsfrcInfo._2

				if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] acquired temporary supporting formulas (result cube)")
				
				
				// compute new truth value of the updated predicate for the cube
				
				val tempSupForms = tempSupFormsResultSet ++ tempSupFormsResultCube
				
				val posResult = computePredicateTruthValue(ctx, updatedPred, resdetPredSet, resdetCube, posWP, tempSupForms)
				
				val negResult = computePredicateTruthValue(ctx, updatedPred, resdetPredSet, resdetCube, negWP, tempSupForms)
				
				if (Main.DEBUG) println("[DEBUG StatementResolver.resolve] positive result = " + posResult + ", negative result = " + negResult)
				
				
				// try to find minimal sub-cubes that give the same precise result and prune their super-cubes
				// update records about sets of predicates that yield precise result
				
				// positive case
				if ( posResult && ( ! negResult ) )
				{
					val minimalCubes = findMinimalSubCubesImplyingWP(ctx, updatedPred, posWP, negWP, resdetCube, resdetPredSet, tempSupForms, logicvar2matchexprs, tempvar2eqlpreds)
					
					if (minimalCubes != null)
					{
						if (Main.DEBUG) 
						{
							for (minCube <- minimalCubes) FormulaUtils.printFormSet(minCube, "[DEBUG StatementResolver.resolve] minimal cube with positive result:")
						}
						
						posResultCubes = posResultCubes ++ minimalCubes
						
						for (minCube <- minimalCubes)
						{
							cubes = cubes.filterNot(cb => minCube.subsetOf(cb))
						}
					}
					else
					{
						posResultCubes = posResultCubes + resdetCube
					}					
				}
				
				// negative case
				if ( negResult && ( ! posResult ) )
				{
					val minimalCubes = findMinimalSubCubesImplyingWP(ctx, updatedPred, negWP, posWP, resdetCube, resdetPredSet, tempSupForms, logicvar2matchexprs, tempvar2eqlpreds)
					
					if (minimalCubes != null)
					{
						if (Main.DEBUG) 
						{
							for (minCube <- minimalCubes) FormulaUtils.printFormSet(minCube, "[DEBUG StatementResolver.resolve] minimal cube with negative result:")
						}
						
						negResultCubes = negResultCubes ++ minimalCubes
						
						for (minCube <- minimalCubes)
						{
							cubes = cubes.filterNot(cb => minCube.subsetOf(cb))
						}
					}
					else
					{
						negResultCubes = negResultCubes + resdetCube
					}
				}
				
				if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] finished processing cube")
			}
			
			
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] started generating bytecode")
			
			// generate bytecode for cubes with precise result
			// we iterate over the cubes in the order of decreasing size
			
			var cubeSize = resdetPredSet.size
			
			while (cubeSize > 0)
			{
				for (posCube <- posResultCubes if (posCube.size == cubeSize))
				{
					if (Main.DEBUG) FormulaUtils.printFormSet(posCube, "[DEBUG StatementResolver.resolve] generating code for positive cube:")
					
					// create predicate set
					
					var posPredSet = Set[AtomicPredicate]()
					
					for (resPred <- resdetPredSet)
					{
						if (posCube.contains(resPred.asInstanceOf[LogicFormula])) posPredSet = posPredSet + resPred
						else if (posCube.contains(new Negation(resPred))) posPredSet = posPredSet + resPred
					}
					
					// generate bytecode to set new truth value of the updated predicate				
					generateCodeForUpdatingPredicateTruthValue(ctx, mv, updatedPred, updatedPredBoolVar, posPredSet, posCube, updatedCombsEndLabel, true, false)
				}
				
				for (negCube <- negResultCubes if (negCube.size == cubeSize))
				{
					if (Main.DEBUG) FormulaUtils.printFormSet(negCube, "[DEBUG StatementResolver.resolve] generating code for negative cube:")
					
					// create predicate set
					
					var negPredSet = Set[AtomicPredicate]()
					
					for (resPred <- resdetPredSet)
					{
						if (negCube.contains(resPred.asInstanceOf[LogicFormula])) negPredSet = negPredSet + resPred
						else if (negCube.contains(new Negation(resPred))) negPredSet = negPredSet + resPred
					}
					
					// generate bytecode to set new truth value of the updated predicate				
					generateCodeForUpdatingPredicateTruthValue(ctx, mv, updatedPred, updatedPredBoolVar, negPredSet, negCube, updatedCombsEndLabel, false, true)
				}
				
				cubeSize -= 1
			}

				
			// generate "else-if" branches for tuples of conflicting literals
			// they force backtracking
			for ( (rdPred, conflictTuples) <- resdetpred2conflicts)
			{
				for (cfLitTuple <- conflictTuples) generateCodeForConflictingPredicates(ctx, mv, rdPred, cfLitTuple, updatedCombsEndLabel, writeToVariables)				
			}
			
			
			// generate "else-if" branches for cubes that specify ambiguous values of temporary variables
			// they force backtracking
			generateCodeForAmbiguousCubes(ctx, mv, resdetPredSet, tempvar2eqlpreds, updatedCombsEndLabel, writeToVariables)
			
			// generate "else-if" branches for cubes that are not relevant with respect to aliasing between variables
			// they keep the current value of the updated predicate
			generateCodeForIrrelevantCubes(ctx, mv, updatedPred, resdetPredSet, alias2relevantpreds, updatedCombsEndLabel, writeToVariables)
			
			// generate the "else" branch that assigns non-deterministic boolean value ("we do not know")
			// this handles cases when the solver says yes (sat) both for original precondition and negated precondition
			mv.visitInsn(Opcodes.POP) // pop the dummy value
			GenUtils.generateChooseBool(mv)		
			
			mv.visitLabel(updatedCombsEndLabel)
			
			if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] finished generating bytecode")
		}

		if (writeToVariables)
		{		
			// save new values into variables (they are taken from stack in reversed order)
			for (updatedPred <- updatedPredList.reverse)
			{
				if (Main.DEBUG) println("[DEBUG StatementResolver.resolve] updated predicate = '" + updatedPred + "'")
				
				var updatedPredBoolVar = ctx.getVariableForPredicate(updatedPred).get
				
				GenUtils.generateStoreInstruction(updatedPredBoolVar, mv, ctx)
			}
		}

		if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] resolve finish")		
	}

	
	private def computePredicateTruthValue(ctx : AbstractionContext, updatedPred : AtomicPredicate, resdetPredSet : Set[AtomicPredicate], resdetCube : Set[LogicFormula], updatedPredWP : LogicFormula, tempSupForms : Set[LogicFormula]) : Boolean =
	{
		// compute new value of the given predicate based on weakest preconditions
			
		var result = false
					
		// add all supporting predicates
		val allInputPreds = resdetCube ++ Configuration.predicateSemModel.getSupportFormulas() ++ tempSupForms
			
		// check validity using SMT solver
		result = Configuration.smtProvider.checkValidity(ctx, updatedPredWP, allInputPreds)
		
		return result
	}
	
	
	private def generateCodeForUpdatingPredicateTruthValue(ctx : AbstractionContext, mv : MethodVisitor, updatedPred : AtomicPredicate, updatedPredBoolVar : String, resdetPredSet : Set[AtomicPredicate], resdetCube : Set[LogicFormula], updatedCombsEndLabel : Label, posResult : Boolean, negResult : Boolean) : Unit =
	{
		// generate bytecode for this cube
		
		// create Label marking the end of the chain of all if-else statements for this combination
		val chainEndLabel = new Label()
		
		var resdetPredCoefBCG = 1
		
		for (resdetCubeElem <- resdetCube)
		{
			var truthValue = 0			
			var resdetPred : AtomicPredicate = null 
			
			if ( resdetCubeElem.isInstanceOf[Negation] && resdetPredSet.contains(resdetCubeElem.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate]) ) 
			{
				// this predicate is a negated variant of some predicate in the original set of result-determining predicates

				truthValue = 0
				resdetPred = resdetCubeElem.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate]
			}
			else if ( resdetCubeElem.isInstanceOf[Negation] && resdetCubeElem.isAlwaysFalse() )
			{
				// this predicate is always false
				
				truthValue = 0
				resdetPred = resdetCubeElem.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate]
			}			
			else
			{
				truthValue = 1
				resdetPred = resdetCubeElem.asInstanceOf[AtomicPredicate]
			}
						
			if (resdetPred == Constants.TRUE_PRED)
			{
				// this predicate is always true
				mv.visitInsn(Opcodes.ICONST_1)
			}
			else
			{
				// get boolean variable for the predicate
				val resdetPredBoolVar = ctx.getVariableForPredicate(resdetPred).get
				
				// generate another element of the chain of if-else statements
				
				// load the local variable or field
				GenUtils.generateLoadInstruction(resdetPredBoolVar, mv, ctx)
			}
	
			// load integer constant 0 or 1 depending on truth value
				// why: program must jump when the condition does not hold (this is how Java programs are compiled)
			if (truthValue == 0) mv.visitInsn(Opcodes.ICONST_1)
			else /*if (truthValue > 0)*/ mv.visitInsn(Opcodes.ICONST_0)
		
			// if_icmpeq instruction with proper jump target
			// we use the chainEndLabel as target for negative case (i.e., when the comparison is evaluated to false)
			mv.visitJumpInsn(Opcodes.IF_ICMPEQ, chainEndLabel)
			
			resdetPredCoefBCG *= 2
		}
		
		// pop the dummy value
		mv.visitInsn(Opcodes.POP)

		if (posResult && ( ! negResult ) )
		{
			// solver says "yes" for original updatedPred -> assign true to the variable representing updatedPred

			// load 'true'
			mv.visitInsn(Opcodes.ICONST_1)			
		}
		else if (negResult && ( ! posResult) )
		{
			// solver says "yes" for negated updatedPred -> assign false to the variable representing updatedPred
			
			// load 'false'
			mv.visitInsn(Opcodes.ICONST_0)
		}
		
		// generate goto instruction
		mv.visitJumpInsn(Opcodes.GOTO, updatedCombsEndLabel)
		
		mv.visitLabel(chainEndLabel)
	}
	
	
	private def generateCodeForConflictingPredicates(ctx : AbstractionContext, mv : MethodVisitor, rdPred : AtomicPredicate, conflictTuple : Set[LogicFormula], updatedCombsEndLabel : Label, writeToVariables : Boolean) =
	{
		// generate proper if-else statements based on truth values of boolean variables for the predicates
		
		// create Label marking the end of the chain of the if-else statements for this tuple of conflicting literals
		val chainEndLabel = new Label()
		
		
		// test if the boolean variable for the first predicate is true
		
		// get boolean variable for the result-determining predicate
		val rdPredBoolVar = ctx.getVariableForPredicate(rdPred).get
		
		GenUtils.generateLoadInstruction(rdPredBoolVar, mv, ctx)

		// load integer constant 0 -> program must jump when the condition does not hold (this is how Java programs are compiled)
		mv.visitInsn(Opcodes.ICONST_0)
		
		// if_icmpeq instruction with proper jump target
		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, chainEndLabel)

		
		// test if the boolean variables for the other predicates are true
		
		for (cfLit <- conflictTuple)
		{
			var cfPred : AtomicPredicate = null
			var isNegated = false
			
			if (cfLit.isInstanceOf[Negation])
			{
				cfPred = cfLit.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate]
				isNegated = true
			}
			else
			{
				cfPred = cfLit.asInstanceOf[AtomicPredicate]
				isNegated = false
			}
			
			val cfPredBoolVar = ctx.getVariableForPredicate(cfPred).get
			
			GenUtils.generateLoadInstruction(cfPredBoolVar, mv, ctx)
	
			if (isNegated) mv.visitInsn(Opcodes.ICONST_1)
			else mv.visitInsn(Opcodes.ICONST_0)
			
			// if_icmpeq instruction with proper jump target
			mv.visitJumpInsn(Opcodes.IF_ICMPEQ, chainEndLabel)			
		}
				
		// we keep the dummy value on the stack (no need for removal)
		
		// force backtrack when both conflicting predicates are true
		GenUtils.generateForcedBacktrack(mv)
		
		mv.visitLabel(chainEndLabel)
	}
	
	
	private def generateCodeForAmbiguousCubes(ctx : AbstractionContext, mv : MethodVisitor, resdetPredSet : Set[AtomicPredicate], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]], updatedCombsEndLabel : Label, writeToVariables : Boolean) =
	{
		// generate proper if-else statements based on truth values of boolean variables for the predicates
		
		for ( (tempVarName, tempVarPreds) <- tempvar2eqlpreds )
		{
			var tvPredBoolVars = Set[String]()
		
			// get boolean variables for the predicates
			for (tvPred <- tempVarPreds)
			{
				tvPredBoolVars = tvPredBoolVars + ctx.getVariableForPredicate(tvPred).get
			}
				
			
			// create Label marking the end of the chain of the if-else statements for predicates about current temporary variable
			val chainEndLabel = new Label()
			
			
			// test if the boolean variable for every predicate is false (whether negated predicate holds)
			for (tvpBoolVar <- tvPredBoolVars)
			{
				GenUtils.generateLoadInstruction(tvpBoolVar, mv, ctx)
		
				// load integer constant 1 -> program must jump when the condition does not hold (this is how Java programs are compiled)
				mv.visitInsn(Opcodes.ICONST_1)
				
				// if_icmpeq instruction with proper jump target
				mv.visitJumpInsn(Opcodes.IF_ICMPEQ, chainEndLabel)
			}
	
			
			// we keep the dummy value on the stack (no need for removal)
			
			// force backtrack when all predicates about the temporary variable are false
			GenUtils.generateForcedBacktrack(mv)
		
			mv.visitLabel(chainEndLabel)
		}
	}
	
	
	private def generateCodeForIrrelevantCubes(ctx : AbstractionContext, mv : MethodVisitor, updatedPred : AtomicPredicate, resdetPredSet : Set[AtomicPredicate], alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]], updatedCombsEndLabel : Label, writeToVariables : Boolean) =
	{
		// generate proper if-else statements based on truth values of boolean variables for the predicates
		
		for ( (aliasingPred, relevantPreds) <- alias2relevantpreds )
		{
			val aliasingPredBoolVar = ctx.getVariableForPredicate(aliasingPred).get
			
			var relevantPredBoolVars = Set[String]()
			
			// get boolean variables for the relevant predicates
			for (relevPred <- relevantPreds)
			{
				relevantPredBoolVars = relevantPredBoolVars + ctx.getVariableForPredicate(relevPred).get
			}
			
			// create Label marking the end of the chain of the if-else statements for the aliasing predicate and relevant predicates 
			val chainEndLabel = new Label()
	
			// create Label marking for the code that keeps the current value of the updated predicate
			val keepValueLabel = new Label()
			
			
			// irrelevant cube: aliasing predicate is false and some relevant predicate is true

			
			// test if the boolean variable for aliasing predicate is false	(whether the negated predicate holds)
			
			GenUtils.generateLoadInstruction(aliasingPredBoolVar, mv, ctx)
		
			// load integer constant 1 -> program must jump when the condition does not hold (this is how Java programs are compiled)
			mv.visitInsn(Opcodes.ICONST_1)
				
			// if_icmpeq instruction with proper jump target
			mv.visitJumpInsn(Opcodes.IF_ICMPEQ, chainEndLabel)
			
			
			// test if the boolean variable for every relevant predicate is false
			// if some relevant predicate is true then we must load the current value of the updated predicate
			for (relevPredBoolVar <- relevantPredBoolVars)
			{
				GenUtils.generateLoadInstruction(relevPredBoolVar, mv, ctx)
		
				// load integer constant 1 -> program must jump when the condition does not hold (this is how Java programs are compiled)
				mv.visitInsn(Opcodes.ICONST_1)
				
				// if_icmpeq instruction with proper jump target
				mv.visitJumpInsn(Opcodes.IF_ICMPEQ, keepValueLabel)
			}
			
			// if we are here then all relevant predicates are false
			mv.visitJumpInsn(Opcodes.GOTO, chainEndLabel)
			

			mv.visitLabel(keepValueLabel)
			
			// load the current value of the updated predicate and jump to the end

			// pop the dummy value
			mv.visitInsn(Opcodes.POP)

			val updatedPredBoolVar = ctx.getVariableForPredicate(updatedPred).get
			GenUtils.generateLoadInstruction(updatedPredBoolVar, mv, ctx)

			mv.visitJumpInsn(Opcodes.GOTO, updatedCombsEndLabel)
			

			mv.visitLabel(chainEndLabel)
		}
	}
	
	
	private def generateCubes(resdetPredList : List[AtomicPredicate], resdetpred2conflicts : Map[AtomicPredicate, Set[Set[LogicFormula]]], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]], alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]]) : List[Set[LogicFormula]] =
	{
		if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] started generating cubes")
		
		// we use bit operations on the number that encodes a current combination (cube), and precompute some information (e.g., which powers of two correspond to conflicting predicates)
		// we generate actual cube objects (sets of logic formulas) only for the included ones -> this avoids unnecessary computation for pruned cubes
		
		var cubes = List[Set[LogicFormula]]()
		
		val maxCombinations : Long = round(pow(2, resdetPredList.size))
		
		if (Main.DEBUG) println("[DEBUG StatementResolver.generateCubes] max combinations = " + maxCombinations)
		
		
		// compute flags (patterns) that mark conflicting predicates
		
		// all tuples of conflicting predicates
		// each pattern contains positive literals and negative literals
		var patternsConflictLits = Set[(Set[Long],Set[Long])]()
		
		for ( (resPred, conflictingTuples) <- resdetpred2conflicts )
		{
			val resPredIdx = resdetPredList.indexOf(resPred)
			
			for (cfLitTuple <- conflictingTuples)
			{
				var posFlags = Set[Long]()
				var negFlags = Set[Long]()
								
				posFlags = posFlags + round(pow(2, resPredIdx))
				
				for (cfLit <- cfLitTuple)
				{
					if (cfLit.isInstanceOf[Negation])
					{
						val cfPred = cfLit.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate]						
						
						val cfPredIdx = resdetPredList.indexOf(cfPred)
						
						negFlags = negFlags + round(pow(2, cfPredIdx))
					}
					else
					{
						var cfPred = cfLit.asInstanceOf[AtomicPredicate]
						
						val cfPredIdx = resdetPredList.indexOf(cfPred)
						
						posFlags = posFlags + round(pow(2, cfPredIdx))
					}
				}
				
				patternsConflictLits = patternsConflictLits + ( (posFlags, negFlags) )
			}
		}
			
		
		// compute flags marking predicates that would make a given cube unambiguous
		
		// one set of flags for each temporary variable
		var flagsUnambigPreds = List[Set[Long]]()
		
		// get indexes of predicates that would make a given cube unambiguous
		val tmpvar2unambgindexes = Configuration.predicateSemModel.getIndexesForUnambiguousPredicates(resdetPredList, tempvar2eqlpreds)
		
		for ( (tempVarName, unambgIndexes) <- tmpvar2unambgindexes )
		{
			var tmpvarFlags = Set[Long]()
			
			for (idx <- unambgIndexes) 
			{
				tmpvarFlags = tmpvarFlags + round(pow(2, idx))	
			}
			
			flagsUnambigPreds = flagsUnambigPreds :+ tmpvarFlags
		}
		
		
		// compute flags that mark irrelevant predicates with respect to aliasing between variables
		
		// one patterns represents the aliasing predicate and relevant predicates
		var patternsAliasRelevPreds = List[(Long,Set[Long])]()
		
		// get indexes of aliasing predicate and relevant predicates
		var indexesAliasRelevPreds = Configuration.predicateSemModel.getIndexesForAliasingRelevantPredicates(resdetPredList, alias2relevantpreds)
		
		for ( (aliasingPredIdx, relevantPredIndexes) <- indexesAliasRelevPreds )		
		{
			val aliasPredFlag = round(pow(2, aliasingPredIdx))
			
			var relevPredFlags = Set[Long]()
			
			for (idx <- relevantPredIndexes) 
			{
				relevPredFlags = relevPredFlags + round(pow(2, idx))	
			}
			
			patternsAliasRelevPreds = patternsAliasRelevPreds :+ (aliasPredFlag, relevPredFlags)
		}		
		
		
		if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] all flags precomputed")
		
		
		// we create list of cubes where the first contains only negations and the last contains all positive predicates
		
		var curCmb : Long = 0
		
		while (curCmb <= (maxCombinations - 1))
		{
			var includeCube = true
			
			// decide whether the cube should be ignored for some reason (conflicts, ambiguity, irrelevance for aliasing)
			
			// look for conflicting literals
			for ( (posFlags, negFlags) <- patternsConflictLits if includeCube )
			{
				var matchingConflictPattern = true
				
				for (pf <- posFlags)
				{
					if ((curCmb & pf) != pf) matchingConflictPattern = false
				}
				
				for (nf <- negFlags)
				{
					if ((curCmb & nf) != 0) matchingConflictPattern = false
				}
				
				if (matchingConflictPattern) includeCube = false
			}			
			
			// check ambiguity
			for (tmpvarFlagSet <- flagsUnambigPreds if includeCube)
			{
				var containsUnambigPred = false
				
				for (flag <- tmpvarFlagSet)
				{
					if ((curCmb & flag) == flag) containsUnambigPred = true						
				}
				
				if ( ! containsUnambigPred ) includeCube = false
			}

			// check irrelevancy with respect to aliasing between variables
			for ( (aliasPredFlag, relevPredFlags) <- patternsAliasRelevPreds if includeCube )
			{
				if ((curCmb & aliasPredFlag) == 0)
				{
					// we have negated aliasing predicate
					
					var containsPositiveRelevantPred = false
					
					for (flag <- relevPredFlags)
					{
						if ((curCmb & flag) == flag) containsPositiveRelevantPred = true						
					}
					
					if (containsPositiveRelevantPred) includeCube = false
				}
			}
			

			// include the new cube based on previous checks
			if (includeCube)
			{			
				// generate the cube (subset of result determining predicates augmented with truth values) for the given combination		

				var newCube = Set[LogicFormula]()
	
				var coef = 1
				
				for (pred <- resdetPredList)
				{
					val truthValue = curCmb & coef
					
					if (truthValue == coef) newCube = newCube + pred
					else newCube = newCube + new Negation(pred)
					
					coef *= 2
				}
										
				cubes = cubes :+ newCube
			}
			
			
			curCmb += 1
		}
		
		if (Main.DEBUG) Main.printCurrentTimeMS("[TIME] finished generating cubes")
		
		return cubes
	}
	
	
	private def findMinimalSubCubesImplyingWP(ctx : AbstractionContext, updatedPred : AtomicPredicate, targetWP : LogicFormula, oppositeWP : LogicFormula, origCube : Set[LogicFormula], origPredSet : Set[AtomicPredicate], tempSupForms : Set[LogicFormula], logicvar2matchexprs : Map[Expression, Set[Expression]], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]]) : Set[Set[LogicFormula]] =
	{
		val cubeHolder : Map[String, Set[Set[LogicFormula]]] = new HashMap
		
		cubeHolder.put("minimal", Set[Set[LogicFormula]]())
		
		// we use a set because there can be multiple different sub-cubes 
		
		findMinimalSubCubesRecursively(ctx, updatedPred, targetWP, oppositeWP, origCube, origPredSet, tempSupForms, logicvar2matchexprs, tempvar2eqlpreds, cubeHolder)
		
		val minimalCubes = cubeHolder.get("minimal").get
		
		if (minimalCubes.size > 0) return minimalCubes
		else return null // we have not found anything
	}
	

	private def findMinimalSubCubesRecursively(ctx : AbstractionContext, updatedPred : AtomicPredicate, targetWP : LogicFormula, oppositeWP : LogicFormula, origCube : Set[LogicFormula], origPredSet : Set[AtomicPredicate], tempSupForms : Set[LogicFormula], logicvar2matchexprs : Map[Expression, Set[Expression]], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]], cubeHolder : Map[String, Set[Set[LogicFormula]]]) : Boolean =
	{
		// we do not want an empty cube
		if (origCube.size <= 1) return false
		
		var addedNewCubes = false
		
		var firstCubePart = Set[LogicFormula]()
		var secondCubePart = origCube

		val wpAtomicPreds = FormulaUtils.extractAtomicPredicates(targetWP)

		while (secondCubePart.size > 0)
		{
			val curCubeElem = secondCubePart.head
			secondCubePart = secondCubePart.tail

			var curResPred : AtomicPredicate = null
			if (curCubeElem.isInstanceOf[AtomicPredicate] && origPredSet.contains(curCubeElem.asInstanceOf[AtomicPredicate])) curResPred = curCubeElem.asInstanceOf[AtomicPredicate]
			else curResPred = curCubeElem.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate]
			

			var skipCubeElem = false

			// if the weakest precondition contains some logic variables, then drop only those elements of the full cube that do not match possible values of logic variables that are captured in temporary supporting formulas
			
			var matchesSomeLogicVar = false
			var wpContainsLogicVar = false

			for (wpPred <- wpAtomicPreds)
			{
				val wpPredVarNames = FormulaUtils.extractVariableNames(wpPred)

				for (vname <- wpPredVarNames)
				{
					if (ExpressionUtils.isLogicVariable(vname))
					{
						wpContainsLogicVar = true

						val vnameExpr = new Expression(vname)

						val samePosExprs = FormulaUtils.findExprsWithMatchingPosition(wpPred, vnameExpr, curResPred)

						if (samePosExprs.size > 0)
						{
							var usedMatchExprs = logicvar2matchexprs.getOrElse(vnameExpr, Set[Expression]())

							if (samePosExprs.intersect(usedMatchExprs).size > 0) matchesSomeLogicVar = true
						}
					}
				}
			}

			if (wpContainsLogicVar && matchesSomeLogicVar) skipCubeElem = true
			

			if ( ! skipCubeElem )
			{
				val newSubCube = firstCubePart ++ secondCubePart
				
				var newPredSet : Set[AtomicPredicate] = origPredSet - curResPred
	
				var skipCube = false
	
				// check if the new sub-cube includes some already identified minimal cube
				for (minCube <- cubeHolder.get("minimal").get if ( ! skipCube ) )
				{				
					if (minCube.subsetOf(newSubCube)) skipCube = true
				}
				
				// check whether the new sub-cube contains only negated predicates from the result-determinig set
				var onlyNegatedResPreds = true
				for (newCubeElem <- newSubCube)
				{
					if (newCubeElem.isInstanceOf[AtomicPredicate] && origPredSet.contains(newCubeElem.asInstanceOf[AtomicPredicate])) onlyNegatedResPreds = false	
				}
				if (onlyNegatedResPreds) skipCube = true
				
				// check whether the new sub-cube defines matching expressions for every logic variable in the weakest precondition				
				val newSubCubeMatches = Configuration.predicateSemModel.findMatchingExpressions(targetWP, newPredSet, ctx.getCurClassOrigName(), ctx.getCurMethodName())
				for (logicVarName <- FormulaUtils.extractVariableNames(targetWP) if ExpressionUtils.isLogicVariable(logicVarName))
				{
					// no matches for some logic variable
					if (newSubCubeMatches.getOrElse(new Expression(logicVarName), Set[Expression]()).size == 0) skipCube = true
				}
	
				
				if ( ! skipCube )
				{	
					// consider only sub-cubes that are not ambiguous
					if ( ! Configuration.predicateSemModel.isAmbiguousCube(newSubCube, tempvar2eqlpreds) )
					{
						// check validity of the subcube
					
						// we use temporary supporting formulas based on the original predicate set and cube
						
						if (Main.DEBUG) FormulaUtils.printFormSet(newSubCube, "[DEBUG StatementResolver.findMinimalSubCubesRecursively] processing sub-cube:")
						
						val tgtResult = computePredicateTruthValue(ctx, updatedPred, newPredSet, newSubCube, targetWP, tempSupForms)
					
						val oppResult = computePredicateTruthValue(ctx, updatedPred, newPredSet, newSubCube, oppositeWP, tempSupForms)
					
						if (Main.DEBUG) println("[DEBUG StatementResolver.findMinimalSubCubesRecursively] target result = " + tgtResult + ", opposite result = " + oppResult)
									
						if ( tgtResult && ( ! oppResult ) ) 
						{
							// this new subcube also implies the given weakest precondition
	
							// try to find even smaller cubes recursively
							val addedSmallerCubes = findMinimalSubCubesRecursively(ctx, updatedPred, targetWP, oppositeWP, newSubCube, newPredSet, tempSupForms, logicvar2matchexprs, tempvar2eqlpreds, cubeHolder)
							
							if ( ! addedSmallerCubes )
							{
								// we add the currently processed new sub-cube
								
								var minimalCubes = cubeHolder.get("minimal").get
								
								minimalCubes = minimalCubes + newSubCube
								
								cubeHolder.put("minimal", minimalCubes)							
							}
							
							// either the current processed new sub-cube or a smaller cube was added
							addedNewCubes = true
						}
					}
				}
			}

			// try another subcube (different subtree) or backtrack from here (if the loop ends)

			firstCubePart = firstCubePart + curCubeElem
		}
		
		return addedNewCubes
	}

}

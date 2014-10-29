package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import scala.util.matching.Regex

import common.Constants
import common.AtomicPredicate
import common.LogicFormula
import common.Expression
import common.BinaryPredicate
import common.UnaryPredicate
import common.ExpressionUtils
import common.FormulaUtils


class DefaultPredicateSemanticsModel extends PredicateSemanticsModel
{
	protected var supportFormulas = Set[LogicFormula]()
	
	
	def addSupportFormula(form : LogicFormula) =
	{
		supportFormulas = supportFormulas + form
	}
	
	def getSupportFormulas() : Set[LogicFormula] =
	{
		return supportFormulas
	}
	
	def derivePermanentSupportingFormulas() = 
	{
	}
	
	def getTemporarySupportingFormulasResultSet(updatedPred : AtomicPredicate, predWP : LogicFormula, resdetPreds : Set[AtomicPredicate], curClassOrigName : String, curMethodName : String) : Set[LogicFormula] =
	{
		return Set[LogicFormula]()
	}
	
	def getTemporarySupportingFormulasResultCube(updatedPred : AtomicPredicate, predWP : LogicFormula, resdetPreds : Set[AtomicPredicate], resdetCube : Set[LogicFormula], varnames2matchexprs : Map[Expression, Set[Expression]], curClassOrigName : String, curMethodName : String) : (Set[LogicFormula], Map[Expression,Set[Expression]]) =
	{
		return (Set[LogicFormula](), new HashMap[Expression,Set[Expression]])
	}
	
	
	def getConflictingLiterals(headPred : AtomicPredicate, resdetPredSet : Set[AtomicPredicate], weakPrecond : LogicFormula, curClassOrigName : String, curMethodName : String) : Set[Set[LogicFormula]] =
	{
		var conflictingTuples = Set[Set[LogicFormula]]()
		
		// return empty set
		if ( ! headPred.isInstanceOf[BinaryPredicate] ) return conflictingTuples 
		
		
		val binHeadPred = headPred.asInstanceOf[BinaryPredicate]
		
		if (binHeadPred.op == "=")
		{
			var varExpr1 : Expression = null
			var constExpr1 : Expression = null
	
			if (ExpressionUtils.isConstantValue(binHeadPred.right))
			{
				varExpr1 = binHeadPred.left
				constExpr1 = binHeadPred.right
			}
			else if (ExpressionUtils.isConstantValue(binHeadPred.left))
			{
				varExpr1 = binHeadPred.right
				constExpr1 = binHeadPred.left
			}
			
			for (resPred <- resdetPredSet if ((resPred != headPred) && resPred.isInstanceOf[BinaryPredicate]))
			{
				val binResPred = resPred.asInstanceOf[BinaryPredicate]
		
				// tells whether the pair (headPred, resPred) is conflicting
				var isConflictPred = false
		
				if ((varExpr1 != null) && (constExpr1 != null))
				{
					if (binResPred.op == "=")
					{
						var varExpr2 : Expression = null
						var constExpr2 : Expression = null
						
						if (ExpressionUtils.isConstantValue(binResPred.right))
						{
							varExpr2 = binResPred.left
							constExpr2 = binResPred.right
						}
						else if (ExpressionUtils.isConstantValue(binResPred.left))
						{
							varExpr2 = binResPred.right
							constExpr2 = binResPred.left
						}
						
						if ((varExpr2 != null) && (constExpr2 != null))
						{						
							// v = c1, v = c2 for c1 != c2
							if ((varExpr1 == varExpr2) && (constExpr1 != constExpr2)) isConflictPred = true
							
							// v1 = c1, v2 = c2, v1 < v2 for c1 >= c2
							if ((varExpr1 != varExpr2) && (constExpr1 != Constants.NULL_EXPR) && (constExpr2 != Constants.NULL_EXPR))
							{
								val constNum1 = java.lang.Integer.parseInt(constExpr1.toString())
								val constNum2 = java.lang.Integer.parseInt(constExpr2.toString())
								if (constNum1 >= constNum2)
								{
									val cfAtomPred = new BinaryPredicate("<", varExpr1, varExpr2)
									
									if (Configuration.predicatesMngr.existsPredicate(curClassOrigName, curMethodName, cfAtomPred, -1))
									{									
										val newCfTuple = Set[LogicFormula](resPred, cfAtomPred)
										conflictingTuples = conflictingTuples + newCfTuple
									}
								}
							}
						}
					}
				}
				
				// e1 = e2, e1 < e2
				if ((binResPred.op == ">") || (binResPred.op == "<"))
				{
					if ((binHeadPred.left == binResPred.left) && (binHeadPred.right == binResPred.right)) isConflictPred = true
										
					if ((binHeadPred.left == binResPred.right) && (binHeadPred.right == binResPred.left)) isConflictPred = true					
				}
				
				// v = null, fread(f,v) = e
				if ((constExpr1 == Constants.NULL_EXPR) && (binResPred.op == "="))
				{
					var fapExpr : Expression = null
				
					if (ExpressionUtils.isFieldAccessPath(binResPred.left)) fapExpr = binResPred.left
					else if (ExpressionUtils.isFieldAccessPath(binResPred.right)) fapExpr = binResPred.right
					
					if (fapExpr != null)
					{
						val tgtObjExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(fapExpr)
						
						if (tgtObjExpr == varExpr1) isConflictPred = true
					}
				}
				
				if (isConflictPred)
				{
					val newCfTuple = Set[LogicFormula](resPred)
					conflictingTuples = conflictingTuples + newCfTuple
				}
			}
		}
		
		return conflictingTuples			
	}
	
	def isAmbiguousCube(resdetCube : Set[LogicFormula], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]]) : Boolean =
	{
		return false	
	}
	
	def getIndexesForUnambiguousPredicates(resdetPredList : List[AtomicPredicate], tempvar2eqlpreds : Map[String, Set[AtomicPredicate]]) : List[(String,List[Int])] =
	{
		return List[(String,List[Int])]()	
	}
	
	def isIrrelevantCubeForAliasing(resdetCube : Set[LogicFormula], alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]]) : Boolean =
	{
		return false
	}
	
	def getIndexesForAliasingRelevantPredicates(resdetPredList : List[AtomicPredicate], alias2relevantpreds : Map[AtomicPredicate, Set[AtomicPredicate]]) : List[(Int,List[Int])] =
	{
		return List[(Int,List[Int])]()
	}
	
	def isRelevantPredicateOverAliasedExpr(pred : AtomicPredicate, targetVarExpr : Expression, aliasedExpr : Expression) : Boolean =
	{
		return false	
	}
	
	def findMatchingExpressions(inputForm : LogicFormula, availablePreds : Set[AtomicPredicate], curClassOrigName : String, curMethodName : String) : Map[Expression, Set[Expression]] =
	{
		var matchingExprs : Map[Expression, Set[Expression]] = new HashMap

		val inputFormAtomPreds = FormulaUtils.extractAtomicPredicates(inputForm)
		
		for (atomPred <- inputFormAtomPreds)
		{
			val atomPredVarNames = FormulaUtils.extractVariableNames(atomPred)
			
			// try to find matches for each variable name
			for (vname <- atomPredVarNames)
			{
				if (vname != Constants.FUNC_ARRAY)				
				{
					val varNameExpr = new Expression(vname)
					
					var possibleMatches = Set[Expression]()
					
					for (availPred <- availablePreds)
					{
						possibleMatches = possibleMatches ++ FormulaUtils.findExprsWithMatchingPosition(atomPred, varNameExpr, availPred)
					}

					// aliasing predicate defines a possible match for a logic variable
					if (ExpressionUtils.isLogicVariable(varNameExpr) && atomPred.isInstanceOf[BinaryPredicate])
					{
						val binAtomPred = atomPred.asInstanceOf[BinaryPredicate]

						if (FormulaUtils.isAliasingPredicate(binAtomPred, varNameExpr))
						{
							possibleMatches = possibleMatches + FormulaUtils.extractOtherOperandFromBinPred(binAtomPred, varNameExpr)
						}
					}
						
					if ( ! ExpressionUtils.isLogicVariable(varNameExpr) )
					{
						// program variable can match only constant expressions						
						possibleMatches = possibleMatches.filter(e => ExpressionUtils.isConstantExpression(e))
					}
					
					if (possibleMatches.size() > 0)
					{
						matchingExprs.put(varNameExpr, matchingExprs.getOrElse(varNameExpr, Set[Expression]()) ++ possibleMatches)
					}
				}
			}
		}
		
		
		// for each variable keep only expressions that (i) match all occurrences of the given variable in the input formula and (ii) have the same type
				
		for (tgtVarNameExpr <- matchingExprs.keys)
		{
			val possibleMatches = matchingExprs.getOrElse(tgtVarNameExpr, Set[Expression]())
			
			if (Main.DEBUG) 
			{
				println("[DEBUG DefaultPredicateSemanticsModel.findMatchingExpressions] target variable = " + tgtVarNameExpr)
				ExpressionUtils.printSet(possibleMatches, "[DEBUG DefaultPredicateSemanticsModel.findMatchingExpressions] possible matches:")
			}
			
			var dropMatches = Set[Expression]()
				
			for (posMatchExpr <- possibleMatches)
			{
				for (inputPred <- inputFormAtomPreds)
				{
					var dropMatchPred = true
						
					var possibleMatchExprPreds = Set[AtomicPredicate]()
					
					possibleMatchExprPreds = possibleMatchExprPreds + FormulaUtils.copyWithReplace(inputPred, tgtVarNameExpr.toString(), posMatchExpr.toString())
					
					// try all combinations where other variables are replaced with their matches
					
					for (otherVarExpr <- matchingExprs.keys if (tgtVarNameExpr != otherVarExpr))
					{
						val otherVarMatches = matchingExprs.getOrElse(otherVarExpr, Set[Expression]())
						
						var newMatchExprPreds = Set[AtomicPredicate]()
						
						for (matchExprPred <- possibleMatchExprPreds)
						{
							newMatchExprPreds = newMatchExprPreds + matchExprPred
							
							for (otherMatchExpr <- otherVarMatches)
							{
								newMatchExprPreds = newMatchExprPreds + FormulaUtils.copyWithReplace(matchExprPred, otherVarExpr.toString(), otherMatchExpr.toString())
							}
						}
						
						possibleMatchExprPreds = newMatchExprPreds
					}
					
					// drop matching predicates that contain logic variables
					possibleMatchExprPreds = possibleMatchExprPreds.filterNot(e => FormulaUtils.containsLogicVariables(e))
				
					// equality predicate between a logic variables and the candidate matching expression
					if (inputPred.isInstanceOf[BinaryPredicate] && (inputPred.getOperator() == "="))
					{
						if (inputPred.containsOperand(posMatchExpr.toString()))
						{
							val otherVarExpr = FormulaUtils.extractOtherOperandFromBinPred(inputPred.asInstanceOf[BinaryPredicate], posMatchExpr)
						
							if (ExpressionUtils.isLogicVariable(otherVarExpr)) dropMatchPred = false
						}
					}
					
					for (matchExprPred <- possibleMatchExprPreds)
					{
						if (availablePreds.contains(matchExprPred) || matchExprPred.isAlwaysTrue()) dropMatchPred = false
						
						// binary predicate over two integer constants
						if (matchExprPred.isInstanceOf[BinaryPredicate])
						{
							val binMEPred = matchExprPred.asInstanceOf[BinaryPredicate]

							if (ExpressionUtils.isConstantValue(binMEPred.left) && ExpressionUtils.isConstantValue(binMEPred.right)) dropMatchPred = false
						}
					}
					
					
					if (dropMatchPred)
					{
						dropMatches = dropMatches + posMatchExpr
					}
				}
			}

			// keep all matches defined by aliasing predicates in the weakest precondition
			for (inputPred <- inputFormAtomPreds)
			{
				if (inputPred.isInstanceOf[BinaryPredicate])
				{
					val binInputPred = inputPred.asInstanceOf[BinaryPredicate]
					
					dropMatches = dropMatches.filterNot(e => FormulaUtils.isAliasingPredicate(binInputPred, tgtVarNameExpr, e))
				}
			}

			val filteredPossibleMatches = possibleMatches.filterNot(e => dropMatches.contains(e))

			if (Main.DEBUG) 
			{
				ExpressionUtils.printSet(dropMatches, "[DEBUG DefaultPredicateSemanticsModel.findMatchingExpressions] dropped matches:")
				ExpressionUtils.printSet(filteredPossibleMatches, "[DEBUG DefaultPredicateSemanticsModel.findMatchingExpressions] filtered possible matches:")
			}
			
			matchingExprs.put(tgtVarNameExpr, filteredPossibleMatches)				
		}
		
		// drop empty sets of matching expressions
		matchingExprs = matchingExprs.filter( e => (e._2.size() > 0) )
		
		return matchingExprs
	}

	def clearAll() =
	{
		supportFormulas = Set[LogicFormula]()
	}
}

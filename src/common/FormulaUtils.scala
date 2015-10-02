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
package common

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import scala.util.matching.Regex

import util.StringUtils


object FormulaUtils
{
	private var cacheForm2VarExprs : Map[String, List[Expression]] = new HashMap
	private var cacheForm2VarNames : Map[String, Set[String]] = new HashMap
	private var cacheForm2FieldNames : Map[String, List[String]] = new HashMap

	
	private val binPredOpRE = new Regex("([<>=\\!]+)\\s*")

	
	def copyWithReplace(origForm : LogicFormula, oldExprStr : String, newExprStr : String) : LogicFormula =
	{
		if (origForm.isInstanceOf[BinaryPredicate])
		{
			val origBinPred = origForm.asInstanceOf[BinaryPredicate]
			
			return new BinaryPredicate(origBinPred.op, origBinPred.left.replace(oldExprStr, newExprStr), origBinPred.right.replace(oldExprStr, newExprStr))
		}
		
		if (origForm.isInstanceOf[UnaryPredicate])
		{
			val origUnPred = origForm.asInstanceOf[UnaryPredicate]
			
			return new UnaryPredicate(origUnPred.op, origUnPred.arg.replace(oldExprStr, newExprStr))
		}
		
		if (origForm.isInstanceOf[Conjunction])
		{
			val origConjForm = origForm.asInstanceOf[Conjunction]

			var copyClauses = List[LogicFormula]()

			for (origCL <- origConjForm.clauses)
			{
				copyClauses = copyClauses :+ copyWithReplace(origCL, oldExprStr, newExprStr)
			}

			return new Conjunction(copyClauses)
		}
		
		if (origForm.isInstanceOf[Disjunction])
		{
			val origDisjForm = origForm.asInstanceOf[Disjunction]

			var copyClauses = List[LogicFormula]()

			for (origCL <- origDisjForm.clauses)
			{
				copyClauses = copyClauses :+ copyWithReplace(origCL, oldExprStr, newExprStr)
			}

			return new Disjunction(copyClauses)
		}
		
		if (origForm.isInstanceOf[Negation])
		{
			val origNegForm = origForm.asInstanceOf[Negation]
			
			return new Negation(copyWithReplace(origNegForm.clause, oldExprStr, newExprStr))
		}
		
		if (origForm.isInstanceOf[Implication])
		{
			val origImplyForm = origForm.asInstanceOf[Implication]
			
			return new Implication(copyWithReplace(origImplyForm.ante, oldExprStr, newExprStr), copyWithReplace(origImplyForm.cons, oldExprStr, newExprStr))
		}
		
		if (origForm.isInstanceOf[ExistentialQuantification])
		{
			val origExistForm = origForm.asInstanceOf[ExistentialQuantification]
			
			return new ExistentialQuantification(origExistForm.quantVarName.replace(oldExprStr, newExprStr), copyWithReplace(origExistForm.clause, oldExprStr, newExprStr))
		}
		
		return origForm
	}
	
	def copyWithReplace(origPred : AtomicPredicate, oldExprStr : String, newExprStr : String) : AtomicPredicate =
	{
		return copyWithReplace(origPred.asInstanceOf[LogicFormula], oldExprStr, newExprStr).asInstanceOf[AtomicPredicate]
	}
	
	def isAliasingPredicate(pred : BinaryPredicate, varExpr1 : Expression, varExpr2 : Expression) : Boolean =
	{
		if (pred.op != "=") return false
		
		if ((pred.left == varExpr1) && (pred.right == varExpr2)) return true
		if ((pred.left == varExpr2) && (pred.right == varExpr1)) return true
		
		return false
	}
	
	def isAliasingPredicate(pred : BinaryPredicate, varExpr : Expression) : Boolean =
	{
		if (pred.op != "=") return false
		
		if ((pred.left == varExpr) && ExpressionUtils.isVariableName(pred.right)) return true
		if ((pred.right == varExpr) && ExpressionUtils.isVariableName(pred.left)) return true
		
		return false
	}
	
	def isAliasingPredicate(pred : BinaryPredicate) : Boolean =
	{
		if (pred.op != "=") return false
		
		if (ExpressionUtils.isVariableName(pred.left) && ExpressionUtils.isVariableName(pred.right)) return true
		
		return false
	}
	
	def negate(origForm : LogicFormula) : LogicFormula =
	{
		if (origForm.isInstanceOf[Negation])
		{
			return origForm.asInstanceOf[Negation].clause
		}
		else 
		{
			return new Negation(origForm)
		}
	}
	
	def extractVariableExpressions(form : LogicFormula) : List[Expression] =
	{
		var varExprsOpt = cacheForm2VarExprs.get(form.toString())
		
		if (varExprsOpt != None) return varExprsOpt.get
		
		
		var varExprs = List[Expression]()
		
		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				varExprs = varExprs ++ ExpressionUtils.extractVariableExpressions(operand)			
			}
		}
			
		cacheForm2VarExprs.put(form.toString(), varExprs)

		return varExprs
	}
	
	def extractVariableNames(form : LogicFormula) : Set[String] =
	{
		var varNamesOpt = cacheForm2VarNames.get(form.toString())
		
		if (varNamesOpt != None) return varNamesOpt.get

		
		var varNames = Set[String]()
		
		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				varNames = varNames ++ ExpressionUtils.extractVariableNames(operand)
			}
		}
		
		cacheForm2VarNames.put(form.toString(), varNames)
		
		return varNames
	}
	
	def extractFieldNames(form : LogicFormula) : List[String] =
	{
		var fieldNamesOpt = cacheForm2FieldNames.get(form.toString())
		
		if (fieldNamesOpt != None) return fieldNamesOpt.get
		
		
		var fieldNames = List[String]()

		val varExprs = extractVariableExpressions(form)
		
		for (expr <- varExprs)
		{
			fieldNames = fieldNames ++ ExpressionUtils.extractFieldNamesFromVarExpr(expr)
		}
		
		cacheForm2FieldNames.put(form.toString(), fieldNames)
		
		return fieldNames
	}
	
	def findAtomicPredicateWithExpr(form : LogicFormula, exprStr : String) : AtomicPredicate =
	{
		val atomPredicates = extractAtomicPredicates(form)
		
		for (atomPred <- atomPredicates)
		{
			if (atomPred.containsExpression(exprStr)) return atomPred
		}
		
		return null
	}
	
	def extractAtomicPredicates(form : LogicFormula) : List[AtomicPredicate] =
	{
		var atomicPredicates = List[AtomicPredicate]()
	
		val literals = extractLiterals(form)
		
		for (lit <- literals)
		{
			atomicPredicates = atomicPredicates :+ extractAtomicPredicateFromLiteral(lit)
		}
		
		return atomicPredicates
	}		
		
	def extractLiterals(form : LogicFormula) : List[LogicFormula] =
	{
		var literals = List[LogicFormula]()
		
		if (form.isInstanceOf[AtomicPredicate])
		{
			literals = literals :+ form
		}
		
		if (form.isInstanceOf[Conjunction])
		{
			val conjForm = form.asInstanceOf[Conjunction]

			for (cl <- conjForm.clauses)
			{
				literals = literals ++ extractLiterals(cl)
			}
		}
		
		if (form.isInstanceOf[Disjunction])
		{
			val disjForm = form.asInstanceOf[Disjunction]

			for (cl <- disjForm.clauses)
			{
				literals = literals ++ extractLiterals(cl)
			}
		}
		
		if (form.isInstanceOf[Negation])
		{
			val negForm = form.asInstanceOf[Negation]
			
			if (negForm.clause.isInstanceOf[AtomicPredicate]) literals = literals :+ negForm
			else literals = literals ++ extractLiterals(negForm.clause)
		}
		
		if (form.isInstanceOf[Implication])
		{
			val implyForm = form.asInstanceOf[Implication]

			literals = literals ++ extractLiterals(implyForm.ante)
			literals = literals ++ extractLiterals(implyForm.cons)
		}		
		
		if (form.isInstanceOf[ExistentialQuantification])
		{
			val existForm = form.asInstanceOf[ExistentialQuantification]

			literals = literals ++ extractLiterals(existForm.clause)
		}
		
		return literals
	}
	
	def extractAtomicPredicateFromLiteral(litForm : LogicFormula) : AtomicPredicate =
	{
		var pred : AtomicPredicate = null
		
		if (litForm.isInstanceOf[Negation]) pred = litForm.asInstanceOf[Negation].clause.asInstanceOf[AtomicPredicate]
		else pred = litForm.asInstanceOf[AtomicPredicate]
		
		return pred
	}							
	
	def extractOperands(pred : AtomicPredicate) : List[Expression] =
	{
		var operandList = List[Expression]()
		
		if (pred.isInstanceOf[BinaryPredicate])
		{
			val binPred = pred.asInstanceOf[BinaryPredicate]
			
			operandList = operandList :+ binPred.left
			operandList = operandList :+ binPred.right
		}
		
		if (pred.isInstanceOf[UnaryPredicate])
		{
			val unPred = pred.asInstanceOf[UnaryPredicate]
			
			operandList = operandList :+ unPred.arg
		}
			
		return operandList
	}
	
	def extractOperandsAsStrings(pred : AtomicPredicate) : List[String] = 
	{
		val operandList = extractOperands(pred)
		
		var operandStrList = List[String]()
		
		for (operand <- operandList) operandStrList = operandStrList :+ operand.toString()
		
		return operandStrList
	}
	
	def extractOperandWithFunc(pred : AtomicPredicate, funcName : String) : Expression =
	{
		val predOperands = extractOperands(pred)
		
		for (operand <- predOperands)
		{
			if (operand.containsFunction(funcName)) return operand
		}
		
		return null
	}
	
	def extractOtherOperandFromBinPred(binPred : BinaryPredicate, operand : Expression) : Expression =
	{
		if (binPred.left == operand) return binPred.right
		else if (binPred.right == operand) return binPred.left
		else return null
	}
	
	def extractOtherOperandFromBinPred(binPred : BinaryPredicate, operandStr : String) : Expression =
	{
		if (binPred.left.toString() == operandStr) return binPred.right
		else if (binPred.right.toString() == operandStr) return binPred.left
		else return null
	}
	
	def extractFunctionOperands(form : LogicFormula) : List[FunctionExpression] =
	{	
		var funcExprs = List[FunctionExpression]()

		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				if (operand.isInstanceOf[FunctionExpression]) funcExprs = funcExprs :+ operand.asInstanceOf[FunctionExpression]
			}
		}
		
		return funcExprs
	}
	
	def extractFunctionExpressionsTopLevel(form : LogicFormula) : List[FunctionExpression] =
	{	
		var funcExprs = List[FunctionExpression]()

		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				funcExprs = funcExprs ++ ExpressionUtils.extractFunctionExpressionsTopLevel(operand)
			}
		}
		
		return funcExprs
	}	
	
	def extractFunctionExpressionsTopLevel(form : LogicFormula, funcName : String) : List[FunctionExpression] =
	{
		var funcExprs = List[FunctionExpression]()

		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				funcExprs = funcExprs ++ ExpressionUtils.extractFunctionExpressionsTopLevel(operand, funcName)
			}
		}
		
		return funcExprs		
	}
	
	def extractFunctionExpressionsRecursively(form : LogicFormula) : List[FunctionExpression] =
	{	
		var funcExprs = List[FunctionExpression]()

		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				funcExprs = funcExprs ++ ExpressionUtils.extractFunctionExpressionsRecursively(operand)
			}
		}
		
		return funcExprs
	}
	
	def extractFunctionExpressionsRecursively(form : LogicFormula, funcName : String) : List[FunctionExpression] =
	{	
		var funcExprs = List[FunctionExpression]()

		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				funcExprs = funcExprs ++ ExpressionUtils.extractFunctionExpressionsRecursively(operand, funcName)
			}
		}
		
		return funcExprs
	}
	
	def extractFunctionExprsWithFragment(form : LogicFormula, exprStrFragment : String) : List[FunctionExpression] =
	{
		var funcExprs = List[FunctionExpression]()
		
		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))			
			{
				funcExprs = funcExprs ++ ExpressionUtils.extractFunctionExprsWithFragment(operand, exprStrFragment)
			}
		}
		
		return funcExprs
	}
	
	
	// find expressions in "otherPred" that have the same position as "tgtExpr" inside "srcPred"
	def findExprsWithMatchingPosition(srcPred : AtomicPredicate, tgtExpr : Expression, otherPred : AtomicPredicate) : Set[Expression] =
	{
		var matchingExprs = Set[Expression]()
		
		if (srcPred.isInstanceOf[BinaryPredicate] && otherPred.isInstanceOf[BinaryPredicate])
		{
			val binSrcPred = srcPred.asInstanceOf[BinaryPredicate]
			val binOtherPred = otherPred.asInstanceOf[BinaryPredicate]
			
			// we ignore operands here -> it does not matter if one predicates contains "<" and the second contains "="
			
			// we do symmetric matching with respect to left and right operands
				
			if (binSrcPred.left == binOtherPred.left)
			{
				matchingExprs = matchingExprs ++ ExpressionUtils.findExprsWithMatchingPosition(binSrcPred.right, tgtExpr, binOtherPred.right)
			}
			else if (binSrcPred.left == binOtherPred.right)
			{
				matchingExprs = matchingExprs ++ ExpressionUtils.findExprsWithMatchingPosition(binSrcPred.right, tgtExpr, binOtherPred.left)
			}
			else if (binSrcPred.right == binOtherPred.left)
			{
				matchingExprs = matchingExprs ++ ExpressionUtils.findExprsWithMatchingPosition(binSrcPred.left, tgtExpr, binOtherPred.right)
			}
			else if (binSrcPred.right == binOtherPred.right)
			{
				matchingExprs = matchingExprs ++ ExpressionUtils.findExprsWithMatchingPosition(binSrcPred.left, tgtExpr, binOtherPred.left)
			}
			else
			{
				matchingExprs = matchingExprs ++ ExpressionUtils.findExprsWithMatchingPosition(binSrcPred.left, tgtExpr, binOtherPred.left) ++ ExpressionUtils.findExprsWithMatchingPosition(binSrcPred.left, tgtExpr, binOtherPred.right) ++ ExpressionUtils.findExprsWithMatchingPosition(binSrcPred.right, tgtExpr, binOtherPred.left) ++ ExpressionUtils.findExprsWithMatchingPosition(binSrcPred.right, tgtExpr, binOtherPred.right)
			}
		}
		else if (srcPred.isInstanceOf[UnaryPredicate] && otherPred.isInstanceOf[UnaryPredicate])
		{
			val unSrcPred = srcPred.asInstanceOf[UnaryPredicate]
			val unOtherPred = otherPred.asInstanceOf[UnaryPredicate]
			
			// we ignore operands here
			
			matchingExprs = matchingExprs ++ ExpressionUtils.findExprsWithMatchingPosition(unSrcPred.arg, tgtExpr, unOtherPred.arg)
		}
		
		return matchingExprs
	}
	
	
	def printFormSet(formSet : Set[LogicFormula], prefix : String) =
	{
		printFormList(formSet.toList, prefix)
	}
	
	def printFormList(formList : List[LogicFormula], prefix : String) : Unit =
	{
		println(prefix)
		for (form <- formList) println("\t" + form.toString())			
	}
	
	def printAtomPredSet(predSet : Set[AtomicPredicate], prefix : String) =
	{
		printAtomPredList(predSet.toList, prefix)
	}
	
	def printAtomPredList(predList : List[AtomicPredicate], prefix : String) : Unit =
	{
		println(prefix)
		for (pred <- predList) println("\t" + pred.toString())			
	}
	
	
	def parseFromStr(inputStr : String) : Option[LogicFormula] =
	{
		var form : LogicFormula = null
		
		val firstSpaceIdx = inputStr.indexOf(' ')
		
		if (firstSpaceIdx == -1)
		{
			// no space found -> unary predicate
		
			form = new UnaryPredicate("", ExpressionUtils.createExprFromStr(inputStr.trim().replace("true", "1").replace("false", "0")))
			
			return new Some(form)
		}
		
		// there must be some operator (relational, logic)
		val operStr = inputStr.substring(0, firstSpaceIdx)
		
		operStr match
		{
			case binPredOpRE(op) => 
			{
				// binary predicate with some relational operator
				
				var leftStr = ""
				var rightStr = ""
				
				var leftStrPos = firstSpaceIdx + 1
				var rightStrPos = -1
				
				if (inputStr.charAt(leftStrPos) == '(')
				{
					// arithmetic expression
					
					leftStr = StringUtils.getBracketedStringFromPos(inputStr, leftStrPos)
					
					// consider brackets around "leftStr" and the next space
					rightStrPos = leftStrPos + leftStr.length() + 2 + 1
				}
				else
				{
					// just variable name or function expression
					
					val nextSpaceIdx = inputStr.indexOf(' ', leftStrPos)
					
					leftStr = inputStr.substring(leftStrPos, nextSpaceIdx)
					
					rightStrPos = leftStrPos + leftStr.length() + 1
				}
				
				if (inputStr.charAt(rightStrPos) == '(')
				{
					// arithmetic expression
					
					rightStr = StringUtils.getBracketedStringFromPos(inputStr, rightStrPos)
				}
				else
				{
					// just variable name or function expression
					
					rightStr = inputStr.substring(rightStrPos)
				}

				form = new BinaryPredicate(op, ExpressionUtils.createExprFromStr(leftStr.replace("true", "1").replace("false", "0")), ExpressionUtils.createExprFromStr(rightStr.replace("true", "1").replace("false", "0")))
				
				return new Some(form)
			}
			case _ => 
			{
				// the default case (we have some logic operator)
				form = null
			}
		}
				
		// we have a complex formula with some logic operator

		val logOperStr = operStr
		
		if (logOperStr == "not")
		{
			val argInputStr = StringUtils.getBracketedStringFromPos(inputStr, firstSpaceIdx + 1)
			
			val argFormOpt = parseFromStr(argInputStr)
			
			if (argFormOpt != None) return new Some(new Negation(argFormOpt.get))
			else return None
		}
		
		if ((logOperStr == "and") || (logOperStr == "or"))
		{
			val firstClauseStr = StringUtils.getBracketedStringFromPos(inputStr, firstSpaceIdx + 1)			
			val secondClauseStr = StringUtils.getBracketedStringFromPos(inputStr, firstSpaceIdx + 1 + firstClauseStr.length() + 2)
			
			val firstClauseOpt = parseFromStr(firstClauseStr)
			val secondClauseOpt = parseFromStr(secondClauseStr)
			
			if ((firstClauseOpt != None) && (secondClauseOpt != None)) 
			{
				if (logOperStr == "and") return new Some(new Conjunction(List[LogicFormula](firstClauseOpt.get, secondClauseOpt.get)))
				else if (logOperStr == "or") return new Some(new Disjunction(List[LogicFormula](firstClauseOpt.get, secondClauseOpt.get)))
			}
			
			return None
		}
		
		if (logOperStr == "exists")
		{
			val nextSpaceIdx = inputStr.indexOf(' ', firstSpaceIdx + 1)
			
			val quantVarName = inputStr.substring(firstSpaceIdx + 1, nextSpaceIdx)
			
			val clauseStr = StringUtils.getBracketedStringFromPos(inputStr, nextSpaceIdx + 1)
			
			val clauseOpt = parseFromStr(clauseStr)
			
			if (clauseOpt != None) return new Some(new ExistentialQuantification(quantVarName, clauseOpt.get))
			else return None
		}
		
		if (logOperStr == "implies")
		{
			val anteStr = StringUtils.getBracketedStringFromPos(inputStr, firstSpaceIdx + 1)
			val consStr = StringUtils.getBracketedStringFromPos(inputStr, firstSpaceIdx + 1 + anteStr.length() + 2 + 1)
			
			val anteOpt = parseFromStr(anteStr)
			val consOpt = parseFromStr(consStr)
			
			if ((anteOpt != None) && (consOpt != None)) return new Some(new Implication(anteOpt.get, consOpt.get))
			else return None
		}
		
		return None
	}
	
	
	def eliminateQuantifiers(inputForm : LogicFormula, logvar2values : Map[Expression, Set[Expression]]) : LogicFormula =
	{
		// replace quantified subformulas with conjunctions or disjunctions properly
		
		if (inputForm.isInstanceOf[BinaryPredicate]) return inputForm
		
		if (inputForm.isInstanceOf[UnaryPredicate]) return inputForm
		
		if (inputForm.isInstanceOf[Conjunction])
		{
			val inputConjForm = inputForm.asInstanceOf[Conjunction]

			var qfClauses = List[LogicFormula]()

			for (inputCL <- inputConjForm.clauses)
			{
				qfClauses = qfClauses :+ eliminateQuantifiers(inputCL, logvar2values)
			}

			return new Conjunction(qfClauses)
		}
		
		if (inputForm.isInstanceOf[Disjunction])
		{
			val inputDisjForm = inputForm.asInstanceOf[Disjunction]

			var qfClauses = List[LogicFormula]()

			for (inputCL <- inputDisjForm.clauses)
			{
				qfClauses = qfClauses :+ eliminateQuantifiers(inputCL, logvar2values)
			}

			return new Disjunction(qfClauses)
		}
		
		if (inputForm.isInstanceOf[Negation])
		{
			val inputNegForm = inputForm.asInstanceOf[Negation]
			
			return new Negation(eliminateQuantifiers(inputNegForm.clause, logvar2values))
		}
		
		if (inputForm.isInstanceOf[Implication])
		{
			val inputImplyForm = inputForm.asInstanceOf[Implication]
			
			return new Implication(eliminateQuantifiers(inputImplyForm.ante, logvar2values), eliminateQuantifiers(inputImplyForm.cons, logvar2values))
		}
		
		if (inputForm.isInstanceOf[ExistentialQuantification])
		{
			val inputExistForm = inputForm.asInstanceOf[ExistentialQuantification]
			
			val quantVarExpr = ExpressionUtils.createExprFromStr(inputExistForm.quantVarName)
			
			var instSubforms = List[LogicFormula]()
			
			for ( instValue <- logvar2values.getOrElse(quantVarExpr, Set[Expression]()) )
			{
				val instClause = copyWithReplace(inputExistForm.clause, quantVarExpr.toString(), instValue.toString())
				
				instSubforms = instSubforms :+ eliminateQuantifiers(instClause, logvar2values)
			}
			
			if (instSubforms.size == 0)
			{
				return Constants.FALSE_PRED
			}
			else if (instSubforms.size == 1)
			{
				return instSubforms.head
			}
			else
			{
				return new Disjunction(instSubforms)	
			}			
		}
		
		return inputForm
	}
	
	def eliminateImplicitUniversalQuantification(inputForm : LogicFormula, logvar2values : Map[Expression, Set[Expression]]) : LogicFormula =
	{
		// assume that all remaining logic variables are implicitly universally quantified 
		// create big conjunction over possible values of every logic variable

		var inputLogicVarNames = extractVariableNames(inputForm)		
		inputLogicVarNames = inputLogicVarNames.filter(e => ExpressionUtils.isLogicVariable(e))
		
		// no logic variables
		if (inputLogicVarNames.size() == 0) return inputForm
		
		// there are some logic variables -> eliminate them
		
		var newQuantFreeForm = inputForm
		
		for (logicVarName <- inputLogicVarNames)
		{
			val logicVarExpr = new Expression(logicVarName)

			var instConjClauses = List[LogicFormula]()
			
			for ( instValue <- logvar2values.getOrElse(logicVarExpr, Set[Expression]()) )
			{
				val instClause = copyWithReplace(newQuantFreeForm, logicVarName, instValue.toString())
				
				instConjClauses = instConjClauses :+ instClause
			}
			
			newQuantFreeForm = new Conjunction(instConjClauses)			
		}
		
		return newQuantFreeForm
	}
	
	def containsLogicVariables(pred : AtomicPredicate) : Boolean =
	{
		val predOperands = extractOperands(pred)
		
		for (operand <- predOperands)
		{
			if (ExpressionUtils.containsLogicVariables(operand)) return true
		}
		
		return false
	}

	def containsObjectFields(form : LogicFormula) : Boolean =
	{
		val varNames = extractVariableNames(form)
		
		if (varNames.contains("this")) return true
		
		return false
	}
	
	def containsMethodVariables(form : LogicFormula) : Boolean =
	{
		val varNames = extractVariableNames(form)
		
		for (vname <- varNames)
		{
			if (ExpressionUtils.isMethodVariable(vname)) return true
		}
		
		return false
	}
	
	def containsMethodParameters(form : LogicFormula) : Boolean =
	{
		val varNames = extractVariableNames(form)
		
		for (vname <- varNames)
		{
			if (ExpressionUtils.isMethodParameter(vname)) return true
		}
		
		return false
	}
	
	def containsMethodLocalVars(form : LogicFormula) : Boolean =
	{
		val varNames = extractVariableNames(form)
		
		for (vname <- varNames)
		{
			if (ExpressionUtils.isLocalVariable(vname)) return true
		}
		
		return false
	}
	
	def containsTemporaryVariables(form : LogicFormula) : Boolean =
	{
		val varNames = extractVariableNames(form)
		
		for (vname <- varNames)
		{
			if (ExpressionUtils.isTemporaryVariable(vname)) return true
		}
		
		return false
	}	

	def containsFieldReadOverObjExpr(form : LogicFormula, objExpr : Expression) : Boolean =
	{
		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				if (ExpressionUtils.isFieldAccessPath(operand))
				{
					val fapTgtObj = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(operand)
					
					if (fapTgtObj == objExpr) return true
				}
			}
		}

		return false
	}
	
	def containsOperandWithTopLevelFunction(form : LogicFormula, funcName : String) : Boolean =
	{
		for (atomPred <- extractAtomicPredicates(form))
		{
			for (operand <- extractOperands(atomPred))
			{
				var funcList = ExpressionUtils.extractFunctionExpressionsTopLevel(operand)
				
				for (funcExpr <- funcList)
				{
					if (funcExpr.name == funcName) return true
				}
			}
		}

		return false
	}
}


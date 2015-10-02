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
import scala.util.matching.Regex

import util.StringUtils


object ExpressionUtils
{
	val delimiters = Array[String](" ", ".", ",", "(", ")")

	private val constantRE = new Regex("^\\-?[0-9]+$")
	private val constExprRE = new Regex("^[0-9\\(\\)\\s\\+\\-\\*/]+$")
	private val varNameRE = new Regex("^[a-zA-Z][a-zA-Z0-9_]*$")
	private val fieldAccessRE = new Regex("^[a-zA-Z][a-zA-Z0-9_]*\\.[a-zA-Z][a-zA-Z0-9_\\.]*$")
	private val funcExprRE = new Regex("^[a-zA-Z][a-zA-Z0-9_]*\\(.*\\)$")

	private var cacheExpr2VarNames : Map[String, Set[String]] = new HashMap


	def isVariableName(exprStr : String) : Boolean =
	{
		exprStr match
		{
			case Constants.VALUE_NULL => return false
			case varNameRE() => return true
			case _ => return false
		}
	}

	def isVariableName(expr : Expression) : Boolean =
	{
		if (expr.isInstanceOf[FunctionExpression] || expr.isInstanceOf[ArithmeticExpression] || expr.isInstanceOf[StringExpression]) return false
		
		return isVariableName(expr.toString())
	}
	
	def isFunctionExpr(exprStr : String) : Boolean =
	{
		exprStr match
		{
			case funcExprRE() => return true
			case _ => return false
		}
	}
	
	def isFunctionExpr(expr : Expression, funcName : String) : Boolean =
	{
		return isFunctionExpr(expr.toString(), funcName)
	}
	
	def isFunctionExpr(exprStr : String, funcName : String) : Boolean =
	{
		exprStr match
		{
			case funcExprRE() => 
				{
					if (exprStr.startsWith(funcName+"(")) return true
					else return false
				}
			case _ => return false
		}
	}

	def isConstantValue(expr : Expression) : Boolean =
	{
		return isConstantValue(expr.toString())
	}
	
	def isConstantValue(exprStr : String) : Boolean =
	{
		exprStr match
		{
			case constantRE() => return true
			case Constants.VALUE_NULL => return true
			case _ => return false
		}
	}
	
	def isConstantExpression(expr : Expression) : Boolean =
	{
		if (expr.isInstanceOf[FunctionExpression] || expr.isInstanceOf[StringExpression]) return false
		
		return isConstantExpression(expr.toString())
	}
	
	def isConstantExpression(exprStr : String) : Boolean =
	{
		exprStr match
		{
			case constExprRE() => return true
			case Constants.VALUE_NULL => return true
			case _ => return false
		}
	}
	
	def isFieldAccessPath(expr : Expression) : Boolean =
	{
		if (expr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = expr.asInstanceOf[FunctionExpression]

			if (funcExpr.name == Constants.FUNC_FIELD_READ) return true
		}
		else if (expr.isInstanceOf[ArithmeticExpression])
		{
			return false
		}
		else if (expr.isInstanceOf[StringExpression])
		{
			return false
		}
		else
		{
			expr.toString() match
			{
				case Constants.VALUE_NULL => return false
				case fieldAccessRE() => return true
				case _ => return false
			}
		}
		
		return false
	}
	
	def isArrayAccess(expr : Expression) : Boolean =
	{
		if (expr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = expr.asInstanceOf[FunctionExpression]

			if (funcExpr.name == Constants.FUNC_ARRAY_READ) return true
		}
		else if (expr.isInstanceOf[StringExpression])
		{
			return false
		}
		else
		{
			return expr.toString().startsWith(Constants.FUNC_ARRAY_READ+"(")
		}
		
		return false
	}
	
	def isVariableExpression(expr : Expression) : Boolean =
	{
		if (expr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = expr.asInstanceOf[ArithmeticExpression]
			
			return isVariableExpression(arExpr.left) || isVariableExpression(arExpr.right)			
		}
		else
		{
			return isVariableName(expr) || isFieldAccessPath(expr) || isArrayAccess(expr)
		}
	}
	
	def isMethodVariable(expr : Expression) : Boolean =
	{
		return isMethodVariable(expr.toString())
	}
	
	def isMethodVariable(exprStr : String) : Boolean =
	{
		if ( ! isVariableName(exprStr) ) return false
		
		if (exprStr.startsWith(Constants.MTHPARAM_PREFIX) || exprStr.startsWith(Constants.LOCALVAR_PREFIX) || exprStr.startsWith(Constants.TEMP_VAR_PREFIX)) return true
		else return false
	}
	
	def isLocalVariable(expr : Expression) : Boolean =
	{
		return isLocalVariable(expr.toString())
	}
	
	def isLocalVariable(exprStr : String) : Boolean =
	{
		if ( ! isVariableName(exprStr) ) return false
		
		if (exprStr.startsWith(Constants.LOCALVAR_PREFIX) || exprStr.startsWith(Constants.TEMP_VAR_PREFIX)) return true
		else return false
	}
	
	def isMethodParameter(expr : Expression) : Boolean =
	{
		return isMethodParameter(expr.toString())	
	}
	
	def isMethodParameter(exprStr : String) : Boolean =
	{
		if ( ! isVariableName(exprStr) ) return false
		
		return exprStr.startsWith(Constants.MTHPARAM_PREFIX)
	}
	
	def isTemporaryVariable(expr : Expression) : Boolean =
	{
		return isTemporaryVariable(expr.toString())	
	}
		
	def isTemporaryVariable(exprStr : String) : Boolean =
	{
		if ( ! isVariableName(exprStr) ) return false

		return exprStr.startsWith(Constants.TEMP_VAR_PREFIX)
	}
	
	def isTemporaryReturnVariable(expr : Expression) : Boolean =
	{
		return isTemporaryReturnVariable(expr.toString())	
	}
	
	def isTemporaryReturnVariable(exprStr : String) : Boolean =
	{
		if ( ! isVariableName(exprStr) ) return false

		return exprStr.startsWith(Constants.TEMP_RETVAR_PREFIX)
	}
	
	def isLogicVariable(expr : Expression) : Boolean =
	{
		return isLogicVariable(expr.toString())
	}
	
	def isLogicVariable(exprStr : String) : Boolean =
	{
		if ( ! isVariableName(exprStr) ) return false

		if (exprStr.startsWith(Constants.QUANT_VAR_PREFIX)) return true
		if (exprStr.startsWith(Constants.LOGIC_VAR_PREFIX)) return true
		
		return false
	}
	
	def isStackSymbol(expr : Expression) : Boolean =
	{
		return isStackSymbol(expr.toString())
	}
	
	def isStackSymbol(exprStr : String) : Boolean =
	{
		return ((exprStr == Constants.STACK_ELEM_NEWOBJ) || (exprStr == Constants.STACK_ELEM_DUMMY) || (exprStr == Constants.STACK_ELEM_RETVAL_DET) || (exprStr == Constants.STACK_ELEM_RETVAL_NDT))
	}
	
	def isUpdateExpr(expr : Expression) : Boolean =
	{
		if (expr.isInstanceOf[FunctionExpression])
		{
			if (expr.asInstanceOf[FunctionExpression].name == Constants.ARRAY_UPDATE_OPER) return true
		}
		
		return false
	}
	
	
	def extractTargetObjExprFromFieldAccessPath(accessPath : Expression) : Expression =
	{
		if (accessPath.isInstanceOf[FunctionExpression])
		{
			var curFuncExpr = accessPath.asInstanceOf[FunctionExpression]

			// traverse chain of "fread" expressions up to the first expression of a different kind
			while (curFuncExpr.name == Constants.FUNC_FIELD_READ) 
			{
				if ( ! curFuncExpr.args(1).isInstanceOf[FunctionExpression] ) return curFuncExpr.args(1)
				
				curFuncExpr = curFuncExpr.args(1).asInstanceOf[FunctionExpression]
			}
			
			return curFuncExpr
		}
		else
		{
			val accessPathStr = accessPath.toString()
			
			val k = accessPathStr.indexOf('.')
		
			if (k == -1) return accessPath
		
			return ExpressionUtils.createExprFromStr(accessPathStr.substring(0, k))
		}
	}
	
	def extractFieldNamesFromFieldAccessPath(accessPath : Expression) : List[String] =
	{
		var fieldNames = List[String]()
		
		if (accessPath.isInstanceOf[FunctionExpression])
		{
			var curExpr = accessPath

			// traverse chain of "fread" expressions and collect field names
			while (curExpr.isInstanceOf[FunctionExpression] && (curExpr.asInstanceOf[FunctionExpression].name == Constants.FUNC_FIELD_READ)) 
			{
				val curFuncExpr = curExpr.asInstanceOf[FunctionExpression]
				
				if (curFuncExpr.args(0).isInstanceOf[FunctionExpression] && (curFuncExpr.args(0).asInstanceOf[FunctionExpression].name == Constants.ARRAY_UPDATE_OPER))
				{
					// we have "fread(update(f,o,e),o)"
					fieldNames = curFuncExpr.args(0).asInstanceOf[FunctionExpression].args(0).toString() +: fieldNames
				}
				else
				{
					// we have "fread(f,o)"
					fieldNames = curFuncExpr.args(0).toString() +: fieldNames
				}
				
				curExpr = curFuncExpr.args(1)
			}
		}
		else
		{
			val accessPathStr = accessPath.toString()
			
			var k = accessPathStr.indexOf('.')
			
			while (k != -1)
			{
				val l = accessPathStr.indexOf('.', k+1)
	
				var fieldName = ""
				if (l == -1) fieldName = accessPathStr.substring(k+1)
				else fieldName = accessPathStr.substring(k+1, l)
				
				fieldNames = fieldNames :+ fieldName 
				
				k = l
			}
		}
			
		return fieldNames
	}
	
	def extractFieldNamesFromVarExpr(varExpr : Expression) : Set[String] =
	{
		var fieldNames = Set[String]()
		
		if (ExpressionUtils.isFieldAccessPath(varExpr))
		{
			fieldNames = fieldNames ++ ExpressionUtils.extractFieldNamesFromFieldAccessPath(varExpr)
		}
		else if (varExpr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = varExpr.asInstanceOf[FunctionExpression]
			
			for (i <- 0 to (funcExpr.args.length - 1))
			{
				fieldNames = fieldNames ++ extractFieldNamesFromVarExpr(funcExpr.args(i))
			}			
		}
		else if (varExpr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = varExpr.asInstanceOf[ArithmeticExpression]
			
			fieldNames = fieldNames ++ extractFieldNamesFromVarExpr(arExpr.left)
			fieldNames = fieldNames ++ extractFieldNamesFromVarExpr(arExpr.right)
		}
		
		return fieldNames
	}
	
	def extractVariableExpressions(expr : Expression) : List[Expression] =
	{
		var varExprs = List[Expression]()

		if (expr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = expr.asInstanceOf[ArithmeticExpression]
			
			varExprs = varExprs ++ extractVariableExpressions(arExpr.left)
			varExprs = varExprs ++ extractVariableExpressions(arExpr.right)
		}
		else if ( ! expr.isInstanceOf[StringExpression] )
		{
			// variable, field access, array access, constant value, function expression
			varExprs = varExprs :+ expr
		}
		
		return varExprs
	}
	
	def extractVariableNames(expr : Expression) : Set[String] =
	{
		var varNamesOpt = cacheExpr2VarNames.get(expr.toString())
		
		if (varNamesOpt != None) return varNamesOpt.get
		
		
		val varSubExprs = extractVariableExpressions(expr)
		
		var varNames = Set[String]()
		
		for (subExpr <- varSubExprs)
		{
			if (subExpr.isInstanceOf[FunctionExpression])
			{
				val funcSubExpr = subExpr.asInstanceOf[FunctionExpression]
				
				if (funcSubExpr.name == Constants.FUNC_FIELD_READ)
				{
					// skip the field name
					varNames = varNames ++ extractVariableNames(funcSubExpr.args(1))
				}
				else if (funcSubExpr.name == Constants.FUNC_ARRAY_READ)
				{
					// skip the generic function "arr"
					for (i <- 1 to (funcSubExpr.args.length - 1))
					{
						varNames = varNames ++ extractVariableNames(funcSubExpr.args(i))
					}	
				}
				else
				{
					for (arg <- funcSubExpr.args)
					{	
						varNames = varNames ++ extractVariableNames(arg)
					}
				}
			}
			else if (ExpressionUtils.isFieldAccessPath(subExpr))
			{
				val fapTgtObjSubExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(subExpr)
			
				if (ExpressionUtils.isVariableName(fapTgtObjSubExpr))
				{
					varNames = varNames + fapTgtObjSubExpr.toString()
				}
				else
				{
					varNames = varNames ++ extractVariableNames(fapTgtObjSubExpr)	
				}
			}
			else if (ExpressionUtils.isVariableName(subExpr))
			{
				varNames = varNames + subExpr.toString()
			}
		}
		
		cacheExpr2VarNames.put(expr.toString(), varNames)

		return varNames
	}
	
	def extractFunctionExpressionsTopLevel(inputExpr : Expression) : List[FunctionExpression] =
	{
		var funcExprs = List[FunctionExpression]()

		if (inputExpr.isInstanceOf[FunctionExpression])
		{
			funcExprs = funcExprs :+ inputExpr.asInstanceOf[FunctionExpression]			
		}
		else if (inputExpr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = inputExpr.asInstanceOf[ArithmeticExpression]
			
			funcExprs = funcExprs ++ extractFunctionExpressionsTopLevel(arExpr.left)
			funcExprs = funcExprs ++ extractFunctionExpressionsTopLevel(arExpr.right)
		}

		return funcExprs
	}
	
	def extractFunctionExpressionsTopLevel(inputExpr : Expression, funcName : String) : List[FunctionExpression] =
	{
		var funcExprs = List[FunctionExpression]()

		if (inputExpr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = inputExpr.asInstanceOf[FunctionExpression]
			
			if (funcExpr.name == funcName) 
			{
				funcExprs = funcExprs :+ funcExpr
			}
			else
			{
				for (arg <- funcExpr.args)
				{
					funcExprs = funcExprs ++ extractFunctionExpressionsTopLevel(arg, funcName)
				}
			}
		}
		else if (inputExpr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = inputExpr.asInstanceOf[ArithmeticExpression]
			
			funcExprs = funcExprs ++ extractFunctionExpressionsTopLevel(arExpr.left, funcName)
			funcExprs = funcExprs ++ extractFunctionExpressionsTopLevel(arExpr.right, funcName)
		}

		return funcExprs
	}
	
	def extractFunctionExpressionsRecursively(inputExpr : Expression) : List[FunctionExpression] =
	{
		var funcExprs = List[FunctionExpression]()

		if (inputExpr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = inputExpr.asInstanceOf[FunctionExpression]
			
			funcExprs = funcExprs :+ funcExpr
			
			for (arg <- funcExpr.args)
			{
				funcExprs = funcExprs ++ extractFunctionExpressionsRecursively(arg)
			}
		}
		else if (inputExpr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = inputExpr.asInstanceOf[ArithmeticExpression]
			
			funcExprs = funcExprs ++ extractFunctionExpressionsRecursively(arExpr.left)
			funcExprs = funcExprs ++ extractFunctionExpressionsRecursively(arExpr.right)
		}

		return funcExprs
	}
	
	def extractFunctionExpressionsRecursively(inputExpr : Expression, funcName : String) : List[FunctionExpression] =
	{
		var funcExprs = List[FunctionExpression]()

		if (inputExpr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = inputExpr.asInstanceOf[FunctionExpression]
			
			if (funcExpr.name == funcName)
			{
				funcExprs = funcExprs :+ funcExpr
			}
			
			for (arg <- funcExpr.args)
			{
				funcExprs = funcExprs ++ extractFunctionExpressionsRecursively(arg)
			}
		}
		else if (inputExpr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = inputExpr.asInstanceOf[ArithmeticExpression]
			
			funcExprs = funcExprs ++ extractFunctionExpressionsRecursively(arExpr.left)
			funcExprs = funcExprs ++ extractFunctionExpressionsRecursively(arExpr.right)
		}

		return funcExprs
	}
	
	def extractFunctionExprsWithFragment(inputExpr : Expression, exprStrFragment : String) : List[FunctionExpression] =
	{
		var funcExprs = List[FunctionExpression]()

		if (inputExpr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = inputExpr.asInstanceOf[FunctionExpression]
			
			var argWithExpr = false
			
			for (arg <- funcExpr.args)
			{
				if (arg.isInstanceOf[FunctionExpression] && arg.contains(exprStrFragment))
				{
					argWithExpr = true
					
					funcExprs = funcExprs ++ extractFunctionExprsWithFragment(arg, exprStrFragment)
				}
			}
			
			if ( ( ! argWithExpr ) && funcExpr.contains(exprStrFragment) )
			{
				funcExprs = funcExprs :+ funcExpr	
			}
		}
		else if (inputExpr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = inputExpr.asInstanceOf[ArithmeticExpression]
			
			funcExprs = funcExprs ++ extractFunctionExprsWithFragment(arExpr.left, exprStrFragment)
			funcExprs = funcExprs ++ extractFunctionExprsWithFragment(arExpr.right, exprStrFragment)
		}

		return funcExprs		
	}
	
	
	// find subexpressions of "otherExpr" that have the same position as "tgtSubExpr" inside "srcExpr"
	def findExprsWithMatchingPosition(srcExpr : Expression, tgtSubExpr : Expression, otherExpr : Expression) : Set[Expression] =
	{
		var matchingExprs = Set[Expression]()
		
		if (srcExpr == tgtSubExpr)
		{
			matchingExprs = matchingExprs + otherExpr
		}
		else if (srcExpr.isInstanceOf[FunctionExpression] && otherExpr.isInstanceOf[FunctionExpression])
		{
			val funcSrcExpr = srcExpr.asInstanceOf[FunctionExpression]	
			val funcOtherExpr = otherExpr.asInstanceOf[FunctionExpression]
			
			if (funcSrcExpr.name == funcOtherExpr.name)
			{
				for (i <- 0 to (funcSrcExpr.args.length - 1))
				{
					matchingExprs = matchingExprs ++ findExprsWithMatchingPosition(funcSrcExpr.args(i), tgtSubExpr, funcOtherExpr.args(i))
				}				
			}
		}
		else if (srcExpr.isInstanceOf[ArithmeticExpression] && otherExpr.isInstanceOf[ArithmeticExpression])
		{
			val arSrcExpr = srcExpr.asInstanceOf[ArithmeticExpression]	
			val arOtherExpr = otherExpr.asInstanceOf[ArithmeticExpression]
			
			if (arSrcExpr.op == arOtherExpr.op)
			{
				// symmetric with respect to left and right operands
				
				if (arSrcExpr.left == arOtherExpr.left)
				{
					matchingExprs = matchingExprs ++ findExprsWithMatchingPosition(arSrcExpr.right, tgtSubExpr, arOtherExpr.right)
				}
				else if (arSrcExpr.left == arOtherExpr.right)
				{
					matchingExprs = matchingExprs ++ findExprsWithMatchingPosition(arSrcExpr.right, tgtSubExpr, arOtherExpr.left)
				}
				else if (arSrcExpr.right == arOtherExpr.left)
				{
					matchingExprs = matchingExprs ++ findExprsWithMatchingPosition(arSrcExpr.left, tgtSubExpr, arOtherExpr.right)
				}
				else if (arSrcExpr.right == arOtherExpr.right)
				{
					matchingExprs = matchingExprs ++ findExprsWithMatchingPosition(arSrcExpr.left, tgtSubExpr, arOtherExpr.left)
				}
				else
				{
					matchingExprs = matchingExprs ++ findExprsWithMatchingPosition(arSrcExpr.left, tgtSubExpr, arOtherExpr.left) ++ findExprsWithMatchingPosition(arSrcExpr.left, tgtSubExpr, arOtherExpr.right) ++ findExprsWithMatchingPosition(arSrcExpr.right, tgtSubExpr, arOtherExpr.left) ++ findExprsWithMatchingPosition(arSrcExpr.right, tgtSubExpr, arOtherExpr.right)
				}
			}
		}
		
		return matchingExprs
	}

	
	def createExprFromStr(exprStr : String) : Expression =
	{
		if (exprStr.contains("+") || exprStr.contains("-") || exprStr.contains("*") || exprStr.contains("/"))
		{
			val operPos = StringUtils.findArithmeticOperatorWithBrackets(exprStr)
			
			// operPos == -1 if the arithmetic operation is inside some function argument, etc
			
			if (operPos != -1)
			{
				val left = createExprFromStr(exprStr.substring(0, operPos).trim())		
				val right = createExprFromStr(exprStr.substring(operPos + 1).trim())
			
				val op = exprStr.substring(operPos, operPos + 1).trim()
			
				return new ArithmeticExpression(op, left, right)
			}
		}
		
		if (isFunctionExpr(exprStr))
		{
			val openPos = exprStr.indexOf('(') 
			val closePos = exprStr.lastIndexOf(')')
			
			val funcName = exprStr.substring(0, openPos)
			
			val funcArgsStr = exprStr.substring(openPos + 1, closePos)
			
			val funcArgStrList = StringUtils.splitWithBrackets(funcArgsStr, ',')
			
			var funcArgs = new Array[Expression](funcArgStrList.length)
			
			var i = 0
			for (funcArgStr <- funcArgStrList) 
			{
				funcArgs(i) = createExprFromStr(funcArgStr.trim())
				i += 1
			}
			
			return new FunctionExpression(funcName, funcArgs)
		}
		
		// variable name, field access, constant value
		return new Expression(exprStr)
	}
	
	
	def createExprFromClassName(clsName : String) : Expression =
	{
		return new Expression(StringUtils.getClassNameAsIndivisibleString(clsName))
	}
	
	
	def containsLogicVariables(expr : Expression) : Boolean = 
	{
		if (expr.isInstanceOf[FunctionExpression])
		{
			val funcExpr = expr.asInstanceOf[FunctionExpression]
			
			for (arg <- funcExpr.args)
			{
				if (containsLogicVariables(arg)) return true
			}
		}
		else if (expr.isInstanceOf[ArithmeticExpression])
		{
			val arExpr = expr.asInstanceOf[ArithmeticExpression]
			
			if (containsLogicVariables(arExpr.left)) return true
			if (containsLogicVariables(arExpr.right)) return true			
		}
		else
		{
			if (isLogicVariable(expr)) return true	
		}
		
		return false
	}
	
	def containsObjectFields(expr : Expression) : Boolean =
	{
		val exprVarNames = extractVariableNames(expr)
		
		if (exprVarNames.contains("this")) return true
		
		return false
	}
	
	def containsMethodVariables(expr : Expression) : Boolean =
	{
		val exprVarNames = extractVariableNames(expr)
		
		for (vname <- exprVarNames)
		{
			if (isMethodVariable(vname)) return true
		}

		return false
	}

	
	def createFieldAccessExpr(tgtObj : Expression, fieldName : String) : Expression =
	{
		return new FunctionExpression(Constants.FUNC_FIELD_READ, Array[Expression](new Expression(fieldName), tgtObj))
	}
	
	def createArrayAccessExpr(arrayExpr : Expression, index : Expression) : Expression =
	{
		return new FunctionExpression(Constants.FUNC_ARRAY_READ, Array[Expression](new Expression(Constants.FUNC_ARRAY), arrayExpr, index))
	}
	
	def createFieldUpdateGetExpr(targetObj : Expression, fieldNames : List[String], newValue : Expression) : Expression =
	{
		var curObj = targetObj
		
		var fieldNamesIt = fieldNames
		
		for (i <- 1 to (fieldNames.size - 1))
		{
			curObj = new FunctionExpression(Constants.FUNC_FIELD_READ, Array[Expression](new Expression(fieldNamesIt.head), curObj))
			
			fieldNamesIt = fieldNamesIt.tail
		}
		
		// we use the generic function "update" here instead of specific function "fwrite"
		val updateExpr = new FunctionExpression(Constants.ARRAY_UPDATE_OPER, Array[Expression](new Expression(fieldNames.last), curObj, newValue))
		
		val fullExpr = new FunctionExpression(Constants.FUNC_FIELD_READ, Array[Expression](updateExpr, targetObj))
		
		return fullExpr
	}
	
	def createArrayUpdateGetExpr(arrayExpr : Expression, index : Expression, newValue : Expression) : Expression =
	{
		// we use the generic function "update" here instead of specific function "awrite"
		val updateExpr = new FunctionExpression(Constants.ARRAY_UPDATE_OPER, Array[Expression](new Expression(Constants.FUNC_ARRAY), arrayExpr, index, newValue))
		
		val fullExpr = new FunctionExpression(Constants.FUNC_ARRAY_READ, Array[Expression](updateExpr, arrayExpr, index))
		
		return fullExpr
	}
	
	
	def printSet(exprSet : Set[Expression], prefix : String) =
	{
		printList(exprSet.toList, prefix)
	}
	
	def printList(exprList : List[Expression], prefix : String) =
	{
		println(prefix)
		for (expr <- exprList) println("\t" + expr.toString())			
	}
}

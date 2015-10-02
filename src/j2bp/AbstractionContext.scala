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

import scala.collection.immutable.List
import scala.collection.mutable.HashMap

import org.objectweb.asm.Label

import common.AtomicPredicate
import common.Constants
import common.Property
import common.Expression


class AbstractionContext
{
	private var programClasses = List[String]()
	
	private var mainClassName = ""
	
	private val classname2data = new HashMap[String, ClassData]
	
	private val methodname2data = new HashMap[String, MethodData]
	
	private var curClassData : ClassData = null
	
	private var curMethodData : MethodData = null

	// common over all program classes
	private var nextTempRetVarID = 1
	private var curTempRetVarName = ""
	
	// all predicates over the variables into which the return value can be stored 
	private val mthname2resultpreds = new HashMap[String, Set[AtomicPredicate]]
	
	// all predicates over the method output parameters
	private val mthoutparamid2preds = new HashMap[(String, Int), Set[AtomicPredicate]]

	// last processed invoke
	private var lastInvokeClassName = ""
	private var lastInvokeMethodName = ""
	
	private var absClassNameSuffix = "PA"
	
	private var currentProps : List[Property] = null
	
	private var backtrackLocations : List[(String,Int)] = null
	
	
	def setProgramClasses(progClasses : List[String]) =
	{
		this.programClasses = progClasses
	}
	
	def getProgramClasses() : List[String] =
	{
		return this.programClasses	
	}	
	
	def isProgramClass(clsName : String) : Boolean =
	{
		return this.programClasses.contains(clsName)	
	}
	
	def setMainClass(clsName : String) =
	{
		mainClassName = clsName
	}
	
	def getMainClass() : String = 
	{
		return mainClassName
	}
		
	def saveResultPredicatesForMethod(clsName : String, mthName : String, resultPreds : Set[AtomicPredicate]) =
	{
		mthname2resultpreds.put(clsName + "." + mthName, resultPreds)
	}
	
	def getResultPredicatesForMethod(clsName : String, mthName : String) : Set[AtomicPredicate] =
	{
		return mthname2resultpreds.getOrElseUpdate(clsName + "." + mthName, Set[AtomicPredicate]())
	}
	
	def clearResultPredicates() =
	{
		mthname2resultpreds.clear()
	}
	
	def saveOutputParamPredicates(clsName : String, mthName : String, paramIdx : Int, outputPreds : Set[AtomicPredicate]) =
	{
		mthoutparamid2preds.put( (clsName + "." + mthName, paramIdx), outputPreds)		
	}	
	
	def getOutputParamPredicates(clsName : String, mthName : String) : List[(Int, Set[AtomicPredicate])] =
	{
		var outparam2preds = List[(Int, Set[AtomicPredicate])]()
		
		for ( (mthParamID, predSet) <- mthoutparamid2preds )
		{
			if (mthParamID._1 == clsName + "." + mthName)
			{
				outparam2preds = outparam2preds :+ (mthParamID._2, predSet)
			}
		}

		return outparam2preds
	}
	
	def getOutputParamPredicatesCount(clsName : String, mthName : String) : Int =
	{
		var predCount = 0
		
		for ( (mthParamID, predSet) <- mthoutparamid2preds )
		{
			if (mthParamID._1 == clsName + "." + mthName)
			{
				predCount = predCount + predSet.size
			}
		}

		return predCount
	}
	
	def getOutputParamPredicates(clsName : String, mthName : String, paramIdx : Int) : Set[AtomicPredicate] =
	{
		return mthoutparamid2preds.getOrElse( (clsName + "." + mthName, paramIdx), Set[AtomicPredicate]() ) 
	}
	
	def clearOutputParamPredicates() =
	{
		mthoutparamid2preds.clear()		
	}
	
	def storeMethodOrigParamCount(clsName : String, mthName : String, paramCount : Int) =
	{
		getMethodData(clsName + "." + mthName).origParamCount = paramCount
	}
	
	def getMethodOrigParamCount(clsName : String, mthName : String) : Int =
	{
		return getMethodData(clsName + "." + mthName).origParamCount
	}
	
	def storeMethodOrigResultType(clsName : String, mthName : String, hasRetVal : Boolean)
	{
		getMethodData(clsName + "." + mthName).hasOrigReturnValue = hasRetVal
	}
	
	def storeMethodAbsResultType(clsName : String, mthName : String, hasRetVal : Boolean)
	{
		getMethodData(clsName + "." + mthName).hasAbsReturnValue = hasRetVal
	}
	
	def hasMethodOrigReturnValue(clsName : String, mthName : String) : Boolean =
	{
		return getMethodData(clsName + "." + mthName).hasOrigReturnValue
	}
	
	def hasMethodAbsReturnValue(clsName : String, mthName : String) : Boolean =
	{
		return getMethodData(clsName + "." + mthName).hasAbsReturnValue
	}
	
	def storeMethodAbsParamCount(clsName : String, mthName : String, paramCount : Int) =
	{
		getMethodData(clsName + "." + mthName).absParamCount = paramCount
	}
	
	def getMethodAbsParamCount(clsName : String, mthName : String) : Int =
	{
		return getMethodData(clsName + "." + mthName).absParamCount
	}
	
	def initCurClass(clsName : String) =
	{
		curClassData = getClassData(clsName)
		
		curClassData.originalName = clsName
		
		curClassData.pred2fieldname.clear()
	}
	
	def getCurClassOrigName() : String =
	{
		return curClassData.originalName
	}
	
	def setCurClassAbstractName(clsName : String) =
	{
		curClassData.abstractName = clsName
	}
	
	def getCurClassAbstractName() : String =
	{
		return curClassData.abstractName
	}
	
	def initCurMethod(mthName : String, isStatic : Boolean) =
	{
		curMethodData = getMethodData(curClassData.originalName + "." + mthName)
		
		curMethodData.name = mthName
		
		curMethodData.isStatic = isStatic
		
		curMethodData.insn2label.clear()
		
		curMethodData.str2label.clear()
		
		curMethodData.exprStack = List[Expression]()
		
		curMethodData.pred2varname.clear()
		
		curMethodData.maxExprStackDepth = 0
		
		curMethodData.visitedMonitorEnter = false
		curMethodData.monitorVarCount = 0
		
		curMethodData.lockVarExprs = List[Expression]()
		
		curMethodData.localRefVarTypes.clear()
		
		curMethodData.lastNewType = ""
	}
	
	def getCurMethodName() : String =
	{
		return curMethodData.name
	}
	
	def isCurMethodStatic() : Boolean =
	{
		return curMethodData.isStatic
	}

	def getCurMethodOrigParamCount() : Int =
	{
		return curMethodData.origParamCount
	}
	
	def hasCurMethodOrigReturnValue() : Boolean =
	{
		return curMethodData.hasOrigReturnValue
	}
	
	def hasCurMethodAbsReturnValue() : Boolean =
	{
		return curMethodData.hasAbsReturnValue
	}
	
	def getCurMethodParamCount() : Int =
	{
		return curMethodData.absParamCount
	}
	
	def setCurMethodParamCount(count : Int) =
	{
		curMethodData.absParamCount = count
	}

	def addLabelForInsn(insnIdx : Int) =
	{
		if ( ! curMethodData.insn2label.contains(insnIdx) ) curMethodData.insn2label.put(insnIdx, new Label())
	}
	
	def getLabelForInsn(insnIdx : Int) : Option[Label] =
	{
		return curMethodData.insn2label.get(insnIdx)
	}
	
	def addLabelWithName(lblName : String) =
	{
		if ( ! curMethodData.str2label.contains(lblName) ) curMethodData.str2label.put(lblName, new Label())
	}
	
	def getLabelWithName(lblName : String) : Label =
	{
		return curMethodData.str2label.getOrElseUpdate(lblName, new Label())
	}
	
	def addExprToStack(expr : Expression) =
	{
		curMethodData.exprStack = expr :: curMethodData.exprStack
		
		if (curMethodData.exprStack.size > curMethodData.maxExprStackDepth) curMethodData.maxExprStackDepth = curMethodData.exprStack.size
	}
	
	def insertExprToStack(expr : Expression, depth : Int) =
	{
		var newTopPart = List[Expression]()		
		var newBottomPart = curMethodData.exprStack
		
		for (i <- 1 to depth)
		{
			newTopPart = newTopPart :+ newBottomPart.head			
			newBottomPart = newBottomPart.tail
		}
		
		curMethodData.exprStack = newTopPart :+ expr
		curMethodData.exprStack = curMethodData.exprStack ++ newBottomPart
		
		if (curMethodData.exprStack.size > curMethodData.maxExprStackDepth) curMethodData.maxExprStackDepth = curMethodData.exprStack.size
	}
	
	def removeExprFromStack() : Expression =
	{
		val expr = curMethodData.exprStack.head
		
		curMethodData.exprStack = curMethodData.exprStack.tail
		
		return expr
	}
	
	def getExprFromStack() : Expression =
	{
		return curMethodData.exprStack.head
	}
	
	def getExprFromStackAtDepth(depth : Int) : Expression =
	{
		var tmpStack = curMethodData.exprStack
		
		for (i <- 1 to depth) tmpStack = tmpStack.tail			
		
		return tmpStack.head
	}
	
	def printExprStack(prefix : String) =
	{
		println(prefix)
		
		var curPos : Int = 0
		for (expr <- curMethodData.exprStack) 
		{
			curPos += 1
			
			if (curPos == 1) println("top:    " + expr)
			else if (curPos == curMethodData.exprStack.size) println("bot:    " + expr)
			else println("        " + expr)
		}
	}
	
	def getMaxExprStackSize() : Int =
	{
		return curMethodData.maxExprStackDepth
	}

	def storeFieldForPredicate(pred : AtomicPredicate, fieldName : String) =
	{
		curClassData.pred2fieldname.put(pred, fieldName)
	}

	def storeLocalVariableForPredicate(pred : AtomicPredicate, varName : String) =
	{
		curMethodData.pred2varname.put(pred, varName)
	}
	
	def getLocalVariableCount() : Int =
	{
		return curMethodData.pred2varname.size
	}

	def getVariableForPredicate(pred : AtomicPredicate) : Option[String] =
	{
		if (curMethodData.pred2varname.contains(pred)) return curMethodData.pred2varname.get(pred)
		else if (curClassData.pred2fieldname.contains(pred)) return curClassData.pred2fieldname.get(pred)
		else return None
	}
	
	def getMonitorVarCount() : Int =
	{
		return curMethodData.monitorVarCount
	}

	def incMonitorVarCount() =
	{
		curMethodData.monitorVarCount += 1
	}
	
	def setVisitedMonitorEnterFlag(b : Boolean) =
	{
		curMethodData.visitedMonitorEnter = b
	}
	
	def visitedMonitorEnter() : Boolean =
	{
		return curMethodData.visitedMonitorEnter
	}

	def pushLockVariableExpr(varExpr : Expression) =
	{
		curMethodData.lockVarExprs = varExpr :: curMethodData.lockVarExprs
	}
	
	def popLockVariableExpr() : Expression =
	{
		val varExpr = curMethodData.lockVarExprs.head
	
		curMethodData.lockVarExprs = curMethodData.lockVarExprs.tail
		
		return varExpr
	}
	
	def getOrigLocalVarNames() : String =
	{
		return curMethodData.origLocalVarNames
	}
	
	def setOrigLocalVarNames(varNamesInStr : String) =
	{
		curMethodData.origLocalVarNames = varNamesInStr
	}
	
	def getLocalRefVarType(varExpr : Expression) : String =
	{
		return curMethodData.localRefVarTypes.getOrElse(varExpr, "")
	}		

	def setLocalRefVarType(varExpr : Expression, varType : String) =
	{
		curMethodData.localRefVarTypes.put(varExpr, varType)
	}
	
	def getLastNewType() : String =
	{
		return curMethodData.lastNewType
	}		

	def setLastNewType(varType : String) =
	{
		curMethodData.lastNewType = varType
	}

	def getFreshTempReturnVariableName() : String =
	{
		curTempRetVarName = Constants.TEMP_RETVAR_PREFIX + nextTempRetVarID

		nextTempRetVarID = nextTempRetVarID + 1

		return curTempRetVarName
	}

	def getTempReturnVariableName() : String =
	{
		return curTempRetVarName
	}

	def clearTempReturnVariableName() =
	{
		curTempRetVarName = ""
	}
	
	def resetTempReturnVariables() =
	{
		nextTempRetVarID = 1
		curTempRetVarName = ""	
	}
	
	def getLocalVariableName(varIndex : Int) : String =
	{
		val paramCount = getCurMethodOrigParamCount()
		
		// note: local variables start with method parameters
		
		val startIndex = 
			if (isCurMethodStatic()) 0
			else 1
			
		if (Main.DEBUG) println("[DEBUG AbstractionContext.getLocalVariableName] var index = " + varIndex + ", param count = " + paramCount + ", start index = " + startIndex + ", monitor count = " + getMonitorVarCount())

		return JavaCodeUtils.getLocalVarName(varIndex, startIndex, paramCount, getMonitorVarCount())
	}
	
	def setLastInvokeInfo(clsName : String, mthName : String)
	{
		lastInvokeClassName = clsName
		lastInvokeMethodName = mthName
	}
	
	def getLastInvokeClass() : String =
	{
		return lastInvokeClassName
	}
	
	def getLastInvokeMethod() : String =
	{
		return lastInvokeMethodName
	}
	
	def setAbsClassNameSuffix(suffix : String) =
	{
		absClassNameSuffix = suffix
	}
	
	def getAbsClassNameSuffix() : String =
	{
		return absClassNameSuffix
	}	

	def setCurrentProperties(props : List[Property]) =
	{
		currentProps = props
	}
	
	def getCurrentProperties() : List[Property] =
	{
		return currentProps
	}

	def setForcedBacktrackLocations(locations : List[(String,Int)]) =
	{
		backtrackLocations = locations;
	}
	
	def isForcedBacktrackLocation(fullMthName : String, bcPos : Int) : Boolean =
	{
		if (backtrackLocations == null) return false
		
		return backtrackLocations.contains( (fullMthName, bcPos) )	
	}
	
	private def getClassData(clsName : String) : ClassData =
	{
		return classname2data.getOrElseUpdate(clsName, new ClassData())
	}
	
	private def getMethodData(fullMethodName : String) : MethodData =
	{
		return methodname2data.getOrElseUpdate(fullMethodName, new MethodData())
	}

	
	class ClassData
	{
		var originalName = ""
		
		var abstractName = ""
		
		// maps predicate to the name of boolean field that represents it
		val pred2fieldname = new HashMap[AtomicPredicate, String]
	}
	
	class MethodData
	{
		// short name
		var name = ""
		
		var isStatic : Boolean = false
		
		// parameter count for original method
		var origParamCount = 0
		
		// parameter count for the abstract method
		var absParamCount = 0
		
		// whether original method returns something
		var hasOrigReturnValue = false
		
		// whether abstract method returns something
		var hasAbsReturnValue = false
		
		// map from instruction indexes to associated labels	
		val insn2label = new HashMap[Int, Label]
		
		// map from generic strings to associated labels	
		val str2label = new HashMap[String, Label]
	
		// stack of expressions
		// possible expressions: local variable names, fields names, arithmetic expressions over variables, numerical constants, etc
		var exprStack = List[Expression]()
		
		// maps predicate to the name of boolean variable that represents it
		val pred2varname = new HashMap[AtomicPredicate, String]
		
		var maxExprStackDepth = 0

		var visitedMonitorEnter : Boolean = false		
		var monitorVarCount = 0
		
		// stack of lock variable names
		var lockVarExprs = List[Expression]()
		
		var origLocalVarNames = ""
		
		// map from local variable to its type (only for references)
		var localRefVarTypes = new HashMap[Expression, String]
		
		// type of the result of the last the NEW instruction that was processed 
		var lastNewType = ""
	}
	
}

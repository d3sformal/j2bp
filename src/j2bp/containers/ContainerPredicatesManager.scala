package j2bp.containers

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedList
import scala.collection.JavaConversions._
import scala.util.matching.Regex

import common._

import util.StringUtils

import j2bp.Main
import j2bp.WALAUtils
import j2bp.analysis.MethodParamFinder


class ContainerPredicatesManager extends j2bp.DefaultPredicatesManager
{
	protected val cacheCMBExpr2RelPred : Map[String, Set[AtomicPredicate]] = new HashMap

	
	/**
	 * This method returns all predicates that may be updated (predicates that contain some functions and predicates over the variable used to store the return value).
	 */
	def getPredicatesToBeUpdated(absOperName : String, absArgValues : Array[Expression], absRetVarExprs : Array[Expression], absTargetObj : Expression, curClassName : String, curMethodName : String, bcIndex : Int) : Set[AtomicPredicate] =
	{
		if (Main.DEBUG) println("[DEBUG ContainerPredicatesManager.getPredicatesToBeUpdated] class name = " + curClassName + ", method name = " + curMethodName + ", target variable = " + absTargetObj)
		
		val exprSet = BasicContainerModel.getExpressionsToBeUpdated(absOperName, absArgValues, absRetVarExprs, absTargetObj, curClassName, curMethodName, bcIndex)
		
		return getRelevantPredicates(curClassName, curMethodName, exprSet, bcIndex)
	}

	/**
	 * This method returns all relevant predicates that contain at least one of given expressions.
	 */
	def getRelevantPredicates(curClassName : String, curMethodName : String, exprSet : Set[Expression], bcIndex : Int) : Set[AtomicPredicate] =
	{
		// find predicates that contain at least one of given expressions (in its operand or nested clause)
		// we consider the following: left or right operand for binary predicate, operand for unary predicate, any nested predicate for conjunction predicate or negation predicate

		var relevantPreds = Set[AtomicPredicate]()
		
		for (expr <- exprSet)
		{
			// search for predicates in the maps defined in the parent class

			if (Main.DEBUG) println("[DEBUG ContainerPredicatesManager.getRelevantPredicates] expression = '" + expr + "'")
			
			
			var exprPredSet = Set[AtomicPredicate]()
			
			val curCMBExpr = curClassName + "." + curMethodName + ":" + String.valueOf(bcIndex) + ":" + expr
			
			var cmbExprPredsOpt = cacheCMBExpr2RelPred.get(curCMBExpr)
			
			if (cmbExprPredsOpt != None)
			{
				exprPredSet = cmbExprPredsOpt.get
			}
			else
			{
				for (pred <- static2predset.getOrElseUpdate(curClassName, Set[AtomicPredicate]()))
				{
					if (pred.containsExpression(expr.toString()))
					{
						exprPredSet = exprPredSet + pred
					}
				}
	
				for (pred <- object2predset.getOrElseUpdate(curClassName, Set[AtomicPredicate]()))
				{
					if (pred.containsExpression(expr.toString()))
					{
						exprPredSet = exprPredSet + pred
					}
				}
				
				for (pred <- method2predset.getOrElseUpdate(curClassName + "." + curMethodName, Set[AtomicPredicate]()))
				{
					if (pred.containsExpression(expr.toString()) && isValidScopeForPredicate(pred, curClassName + "." + curMethodName, bcIndex))
					{
						exprPredSet = exprPredSet + pred
					}
				}
				
				cacheCMBExpr2RelPred.put(curCMBExpr, exprPredSet)
			}
			
			relevantPreds = relevantPreds ++ exprPredSet
		}

		return relevantPreds
	}

	override def getReservedVariableNames() : List[String] =
	{
		var varNames = super.getReservedVariableNames()
		
		varNames = varNames ++ List[String](BasicContainerModel.VAR_LOGIC_ANY, BasicContainerModel.VAR_LOGIC_MAP, BasicContainerModel.VAR_LOGIC_KEY, BasicContainerModel.VAR_TMP_ITER, BasicContainerModel.VAR_TMP_KEY, BasicContainerModel.VAR_TMP_LOCAL, BasicContainerModel.VAR_TMP_INDEX, BasicContainerModel.VAR_TMP_VALUE, BasicContainerModel.VAR_TMP_BOOL)
		
		return varNames
	}
	
	override def getReservedFunctionSignatures() : List[(String,String,String)] = 
	{
		var funcNames = super.getReservedFunctionSignatures() 
		
		funcNames = funcNames ++ List[(String,String,String)]( (BasicContainerModel.FUNC_ARRAY_MAP, "int int", "int"), (BasicContainerModel.FUNC_ARRAY_SIZE, "int", "int"), (BasicContainerModel.FUNC_ARRAY_ITER, "int int int", "bool"), (BasicContainerModel.FUNC_ARRAY_KEYS, "int int", "bool"), (BasicContainerModel.FUNC_ARRAY_VALUES, "int int", "bool"), (BasicContainerModel.BOTTOM_STR+"f", "int", "int") )
		
		return funcNames
	}
	
	override def getReservedConstants() : List[(String,String)] =
	{
		var constants = super.getReservedConstants()
		
		constants = constants ++ List[(String,String)]( (BasicContainerModel.BOTTOM_STR, BasicContainerModel.BOTTOM_STR+"f 0") )
		
		return constants
	}
	
	override def isReservedName(name : String) : Boolean =
	{
		if (super.isReservedName(name)) return true
		
		if (name == BasicContainerModel.BOTTOM_STR) return true
		
		if ((name == BasicContainerModel.VAR_TMP_ITER) || (name == BasicContainerModel.VAR_TMP_KEY) || (name == BasicContainerModel.VAR_TMP_LOCAL) || (name == BasicContainerModel.VAR_TMP_INDEX) || (name == BasicContainerModel.VAR_TMP_VALUE) || (name == BasicContainerModel.VAR_TMP_BOOL)) return true
		
		if ((name == BasicContainerModel.VAR_LOGIC_ANY) || (name == BasicContainerModel.VAR_LOGIC_MAP) || (name == BasicContainerModel.VAR_LOGIC_KEY)) return true
		
		if ((name == BasicContainerModel.FUNC_ARRAY_MAP) || (name == BasicContainerModel.FUNC_ARRAY_SIZE) || (name == BasicContainerModel.FUNC_ARRAY_ITER) || (name == BasicContainerModel.FUNC_ARRAY_KEYS) || (name == BasicContainerModel.FUNC_ARRAY_VALUES)) return true
		
		return false			
	}
	
	override def dropUnnecessaryPredicates(requiredPredicatesWithMethodName : List[(AtomicPredicate, String)], programClasses : List[String]) =
	{
		// get possible callers (transitively) of methods associated with the required predicates
		
		var allPossibleCallerMethodNames = Set[String]()
		var allPossibleCallerClassNames = Set[String]()
		
		var assocMethodNames = Set[String]()
		
		// find local variables used as method call parameters
		val mthname2paramvars = MethodParamFinder.identifyParameterVariables(programClasses)
		
		
		// find possible call stacks
		for ( (reqPred, fullMthName) <- requiredPredicatesWithMethodName )
		{
			val mthClassName = StringUtils.extractClassName(fullMthName)
			val shortMthName = StringUtils.extractShortMethodName(fullMthName)
			
			var targetClassNames = WALAUtils.findSuperclasses(mthClassName, programClasses)
			
			targetClassNames = targetClassNames :+ mthClassName
			
			for (tgtClsName <- targetClassNames)
			{				
				val callerMethodNames = WALAUtils.findPossibleCallersTransitively(tgtClsName + "." + shortMthName, programClasses)
				
				for (callerMthName <- callerMethodNames)
				{ 
					allPossibleCallerMethodNames = allPossibleCallerMethodNames + callerMthName
					allPossibleCallerClassNames = allPossibleCallerClassNames + StringUtils.extractClassName(callerMthName)
				}
			}
			
			assocMethodNames = assocMethodNames + fullMthName
		}
		
		if (Main.INFO)
		{
			println("[INFO] keep selected predicates for entities:")
			for (callerClsName <- allPossibleCallerClassNames) println("\t class = " + callerClsName)
			for (callerMthName <- allPossibleCallerMethodNames) println("\t method = " + callerMthName)
		}
		
		
		// drop existing predicates with respect to required predicates and possible call stacks
		
		// 1) drop predicates over static fields in classes that are not on any possible call stack		
		for ( clsName <- static2predset.keys )
		{
			if ( ! allPossibleCallerClassNames.contains(clsName) ) 
			{
				static2predset.put(clsName, Set[AtomicPredicate]())
			}
		}
		
		// 2) drop predicates over object fields in classes that are not on any possible call stack		
		for ( clsName <- object2predset.keys )
		{
			if ( ! allPossibleCallerClassNames.contains(clsName) ) 
			{
				object2predset.put(clsName, Set[AtomicPredicate]())
			}
		}
		
		for ( mthName <- method2predset.keys )
		{
			if ( ! allPossibleCallerMethodNames.contains(mthName) ) 
			{
				// 3) drop predicates over local variables in methods that are not on any possible call stack		
				method2predset.put(mthName, Set[AtomicPredicate]())
			}
			else if ( ! assocMethodNames.contains(mthName) ) 
			{
				// 4) drop predicates over local variables in methods that are on some possible call stack but where the local variables are not used as arguments for some method call
				
				val mthParamVars = mthname2paramvars.getOrElse(mthName, Set[String]())
				
				var oldPredSet = method2predset.getOrElse(mthName, Set[AtomicPredicate]())
				
				var newPredSet = Set[AtomicPredicate]()
				
				for (pred <- oldPredSet)
				{
					var keepPred = false
					
					val predVarNames = FormulaUtils.extractVariableNames(pred)
					
					for (vname <- predVarNames)
					{
						if (mthParamVars.contains(vname)) keepPred = true
					}
					
					if (keepPred) newPredSet = newPredSet + pred
				}
				
				method2predset.put(mthName, newPredSet)
			}
			else if (assocMethodNames.contains(mthName))
			{
				// 5) drop predicates that do not refer transitively to the same variables as some property formula
				
				var oldPredSet = method2predset.getOrElse(mthName, Set[AtomicPredicate]())
				
				var reqVarNames = Set[String]()
				
				// 5a) get variable names directly referred to by the property formula
				for ( (reqPred, fullMthName) <- requiredPredicatesWithMethodName )
				{
					for (varName <- FormulaUtils.extractVariableNames(reqPred))
					{
						if ( ! BasicContainerModel.isSpecialArrayName(varName) ) reqVarNames = reqVarNames + varName
					}
				}
				
				// 5b) get variables used in predicates that refer to some already collected variables
				
				var foundNewVar = true
				
				while (foundNewVar)
				{
					foundNewVar = false
					
					for (pred <- oldPredSet)
					{
						val predVarNames = FormulaUtils.extractVariableNames(pred)
						
						var usesReqVar = false
						
						for (vname <- predVarNames)
						{
							if (reqVarNames.contains(vname)) usesReqVar = true							
						}
						
						if (usesReqVar)
						{
							for (vname <- predVarNames)
							{
								if ( ! reqVarNames.contains(vname) )
								{
									reqVarNames = reqVarNames + vname
									foundNewVar = true
								}
							}
						}
					}
				}
				
				// 5c) keep predicates that refer to selected variables
				
				var newPredSet = Set[AtomicPredicate]()
				
				for (pred <- oldPredSet)
				{
					var keepPred = false
					
					val predVarNames = FormulaUtils.extractVariableNames(pred)
					
					for (vname <- predVarNames)
					{
						if (reqVarNames.contains(vname)) keepPred = true
					}
					
					if (keepPred) newPredSet = newPredSet + pred
				}
				
				method2predset.put(mthName, newPredSet)
			}
		}
	}
	
	override protected def isValidScopeForPredicate(pred : AtomicPredicate, fullMethodName : String, bcIndex : Int) : Boolean =
	{
		if ( ! super.isValidScopeForPredicate(pred, fullMethodName, bcIndex) ) return false
		
		if (bcIndex == -1) return true
				
		// for predicates that refer to iterator variables and map variables, check whether the iterator variable is currently associated with the map variable		
		if (pred.isInstanceOf[UnaryPredicate] && pred.containsFunction(BasicContainerModel.FUNC_MAP_ORDER))
		{			
			val morderFuncExpr = FormulaUtils.extractFunctionExpressionsTopLevel(pred, BasicContainerModel.FUNC_MAP_ORDER).head
				
			val mapObj = morderFuncExpr.args(1)
			val firstPosExpr = morderFuncExpr.args(2)
			val secondPosExpr = morderFuncExpr.args(3)
			
			val clsName = StringUtils.extractClassName(fullMethodName)			
			val mthName = StringUtils.extractShortMethodName(fullMethodName)
			
			if (ContainerAbstractionData.isIteratorVariable(clsName, mthName, firstPosExpr))
			{
				if ( ! ContainerAbstractionData.isIteratorWithMap(clsName, mthName, firstPosExpr, mapObj) ) return false
			}
			
			if (ContainerAbstractionData.isIteratorVariable(clsName, mthName, secondPosExpr))
			{
				if ( ! ContainerAbstractionData.isIteratorWithMap(clsName, mthName, secondPosExpr, mapObj) ) return false
			}
		}
		
		return true		
	}

	override def invalidateCaches() =
	{
		super.invalidateCaches()

		cacheCMBExpr2RelPred.clear()
	}
	
	override def clearAll() =
	{
		super.clearAll()

		cacheCMBExpr2RelPred.clear()
	}
}

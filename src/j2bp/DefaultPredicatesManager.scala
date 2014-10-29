package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import scala.io.Source._

import common.Constants
import common.Expression
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.ExpressionUtils
import common.FormulaUtils

import util.StringUtils


class DefaultPredicatesManager extends PredicatesManager
{
	protected var static2predset : Map[String, Set[AtomicPredicate]] = new HashMap
	protected var object2predset : Map[String, Set[AtomicPredicate]] = new HashMap
	protected var method2predset : Map[String, Set[AtomicPredicate]] = new HashMap

	// map from (full method name, predicate) to tuples (start bytecode index, end bytecode index)
	protected var mthpred2bcindexes : Map[(String, AtomicPredicate), List[(Int,Int)]] = new HashMap
	
	protected val cacheCMExprStr2Pred : Map[String, Set[AtomicPredicate]] = new HashMap
	protected val cacheCMOperand2Pred : Map[String, Set[AtomicPredicate]] = new HashMap
	protected var cacheAliasingPreds : Set[AtomicPredicate] = null 
	
	protected var missingPredicates = Set[AtomicPredicate]()
	
	
	def loadGeneratedPredicates(static2preds : Map[String, Set[AtomicPredicate]], object2preds : Map[String, Set[AtomicPredicate]], method2preds : Map[String, Set[AtomicPredicate]]) =
	{
		for ( (className, predSet) <- static2preds )
		{
			for (pred <- predSet)
			{
				addEntityPredicate(static2predset, className, pred)
			}
		}
		
		for ( (className, predSet) <- object2preds )
		{
			for (pred <- predSet)
			{
				addEntityPredicate(object2predset, className, pred)
			}
		}

		for ( (fullMethodName, predSet) <- method2preds )
		{
			for (pred <- predSet)
			{
				addEntityPredicate(method2predset, fullMethodName, pred)
			}
		}
	}
	
	def loadPredicatesFromFile(fileName : String) =
	{
		// 1 -> static, 2 -> object, 4 -> method
		var section : Int = 0
		
		var curName : String = ""

		var lines = fromFile(fileName).getLines()

		for (line <- lines if line.trim().length() > 0)
		{
			if (Main.DEBUG) println("[DEBUG DefaultPredicatesManager.loadPredicatesFromFile] line = " + line)
			
			if (line.startsWith("[static:"))
			{
				curName = line.substring("[static:".length(), line.length() - 1).trim()
				section = 1
			}
			else if (line.startsWith("[object:"))
			{
				curName = line.substring("[object:".length(), line.length() - 1).trim()
				section = 2
			}
			else if (line.startsWith("[method:"))
			{
				curName = line.substring("[method:".length(), line.length() - 1).trim()
				section = 4
			}			
			else
			{
				val predOpt = FormulaUtils.parseFromStr(line)
				
				if (section == 1)
				{
					var curSet = static2predset.getOrElseUpdate(curName, Set[AtomicPredicate]())
					if (predOpt != None) curSet = curSet + predOpt.get.asInstanceOf[AtomicPredicate]
					static2predset.put(curName, curSet)
				}
				
				if (section == 2)
				{
					var curSet = object2predset.getOrElseUpdate(curName, Set[AtomicPredicate]())
					if (predOpt != None) curSet = curSet + predOpt.get.asInstanceOf[AtomicPredicate]
					object2predset.put(curName, curSet)
				}
				
				if (section == 4)
				{
					var curSet = method2predset.getOrElseUpdate(curName, Set[AtomicPredicate]())
					if (predOpt != None) curSet = curSet + predOpt.get.asInstanceOf[AtomicPredicate]
					method2predset.put(curName, curSet)
				}				
			}
		}
	}

	def printAllPredicatesToConsole(prefix : String) = 
	{
		for ( (clsName, predSet) <- static2predset )
		{
			println("\t[static] " + clsName)
			for (pred <- predSet) println(prefix + "\t" + pred.toString)	
		}
		
		for ( (clsName, predSet) <- object2predset )
		{
			println("\t[object] " + clsName)
			for (pred <- predSet) println(prefix + "\t" + pred.toString)
		}
		
		for ( (mthName, predSet) <- method2predset )
		{
			println("\t[method] " + mthName)
			for (pred <- predSet) println(prefix + "\t" + pred.toString)
		}
	}

	def getAllPredicates() : List[AtomicPredicate] =
	{
		var allPredList = List[AtomicPredicate]()
	
		for ( (clsName, predSet) <- static2predset ) allPredList = allPredList ++ predSet
		for ( (clsName, predSet) <- object2predset ) allPredList = allPredList ++ predSet
		for ( (mthName, predSet) <- method2predset ) allPredList = allPredList ++ predSet
		
		return allPredList
	}
	
	def countAllPredicates() : Int =
	{
		var count = 0

		for ( (clsName, predSet) <- static2predset ) count += predSet.size

		for ( (clsName, predSet) <- object2predset ) count += predSet.size

		for ( (mthName, predSet) <- method2predset ) count += predSet.size

		return count
	}

	def countPredicatesOverStaticFields(className : String) : Int = 
	{
		val predSet = static2predset.getOrElseUpdate(className, Set[AtomicPredicate]())
		
		return predSet.size
	}
	
	def getPredicatesOverStaticFields(className : String) : Set[AtomicPredicate] =
	{
		return static2predset.getOrElseUpdate(className, Set[AtomicPredicate]())
	}
	
	def countPredicatesOverObjectFields(className : String) : Int = 
	{
		val predSet = object2predset.getOrElseUpdate(className, Set[AtomicPredicate]())
		
		return predSet.size
	}
	
	def getPredicatesOverObjectFields(className : String) : Set[AtomicPredicate] =
	{
		return object2predset.getOrElseUpdate(className, Set[AtomicPredicate]())
	}
	
	def countPredicatesOverMethodParams(className : String, methodName : String) : Int = 
	{
		var count : Int = 0;
		
		val predSet = method2predset.getOrElseUpdate(className + "." + methodName, Set[AtomicPredicate]())
		
		for (pred <- predSet)
		{
			if (FormulaUtils.containsMethodParameters(pred)) count += 1
		}
		
		return count
	}
	
	def getPredicatesOverMethodVariables(className : String, methodName : String, bcIndex : Int) : Set[AtomicPredicate] =
	{
		var predSet = Set[AtomicPredicate]()
	
		for (pred <- method2predset.getOrElseUpdate(className + "." + methodName, Set[AtomicPredicate]())) 
		{
			if (isValidScopeForPredicate(pred, className + "." + methodName, bcIndex)) predSet = predSet + pred
		}
		
		return predSet
	}

	def getPredicatesOverMethodParams(className : String, methodName : String, bcIndex : Int) : Set[AtomicPredicate] =
	{
		var predSet = Set[AtomicPredicate]()
	
		for (pred <- method2predset.getOrElseUpdate(className + "." + methodName, Set[AtomicPredicate]())) 
		{
			if (FormulaUtils.containsMethodParameters(pred) && isValidScopeForPredicate(pred, className + "." + methodName, bcIndex)) predSet = predSet + pred
		}
		
		return predSet
	}
	
	def getPredicatesOverMethodLocalVars(className : String, methodName : String, bcIndex : Int) : Set[AtomicPredicate] =
	{
		var predSet = Set[AtomicPredicate]()
	
		for (pred <- method2predset.getOrElseUpdate(className + "." + methodName, Set[AtomicPredicate]())) 
		{
			if (FormulaUtils.containsMethodLocalVars(pred) && isValidScopeForPredicate(pred, className + "." + methodName, bcIndex)) predSet = predSet + pred
		}
		
		return predSet
	}
	
	def getAllPredicatesForCodeLocation(fullMethodName : String, bcIndex : Int) : Set[AtomicPredicate] =
	{
		val className = StringUtils.extractClassName(fullMethodName)
		val shortMethodName = StringUtils.extractShortMethodName(fullMethodName)
		
		return getPredicatesOverStaticFields(className) ++ getPredicatesOverObjectFields(className) ++ getPredicatesOverMethodVariables(className, shortMethodName, bcIndex)
	}
	
	def isPredicateOverObjectFields(pred : AtomicPredicate) : Boolean =
	{
		return pred.containsExpression("this")
	}
	
	def isPredicateOverMethodVariables(pred : AtomicPredicate) : Boolean =
	{
		return FormulaUtils.containsMethodVariables(pred)			
	}
	
	def existsPredicate(className : String, methodName : String, pred : AtomicPredicate, bcIndex : Int) : Boolean =
	{
		// look everywhere for the predicate
		
		if (static2predset.getOrElse(className, Set[AtomicPredicate]()).contains(pred)) return true
		
		if (object2predset.getOrElse(className, Set[AtomicPredicate]()).contains(pred)) return true
		
		if (method2predset.getOrElse(className + "." + methodName, Set[AtomicPredicate]()).contains(pred) && isValidScopeForPredicate(pred, className + "." + methodName, bcIndex)) return true

		return false
	}

	def getPredicatesOverExpr(className : String, methodName : String, exprStr : String, bcIndex : Int) : Set[AtomicPredicate] =
	{
		// returns all predicates with the given expression (it may refer to field of the class/object or to variable of the method) that are valid for given bytecode index
				
		if (Main.DEBUG) println("[DEBUG DefaultPredicatesManager.getPredicatesOverExpr] class name = " + className + ", method name = " + methodName + ", expr = " + exprStr + ", bytecode index = " + bcIndex)
		
		var cmbexpr = className + "." + methodName + ":" + String.valueOf(bcIndex) + ":" + exprStr
		
		var resPredSetOpt = cacheCMExprStr2Pred.get(cmbexpr)
		if (resPredSetOpt != None) return resPredSetOpt.get
		
		
		var resPredSet = Set[AtomicPredicate]()
		
		if (ExpressionUtils.isConstantValue(exprStr)) 
		{
			cacheCMExprStr2Pred.put(cmbexpr, resPredSet)
			return resPredSet
		}
		
		for (pred <- static2predset.getOrElseUpdate(className, Set[AtomicPredicate]()))
		{
			if (pred.containsExpression(exprStr)) resPredSet = resPredSet + pred	
		}
		
		for (pred <- object2predset.getOrElseUpdate(className, Set[AtomicPredicate]()))
		{
			if (pred.containsExpression(exprStr)) resPredSet = resPredSet + pred	
		}
		
		for (pred <- method2predset.getOrElseUpdate(className + "." + methodName, Set[AtomicPredicate]()))
		{
			if (pred.containsExpression(exprStr) && isValidScopeForPredicate(pred, className + "." + methodName, bcIndex)) resPredSet = resPredSet + pred	
		}
		
		cacheCMExprStr2Pred.put(cmbexpr, resPredSet)
		
		return resPredSet
	}
		
	def getPredicatesWithOperand(className : String, methodName : String, operand : Expression, bcIndex : Int) : Set[AtomicPredicate] =
	{
		// returns all predicates with the given expression as one operand that are valid for given bytecode index
				
		if (Main.DEBUG) println("[DEBUG DefaultPredicatesManager.getPredicatesWithOperand] class name = " + className + ", method name = " + methodName + ", operand = " + operand.toString() + ", bytecode index = " + bcIndex)
		
		
		var cmboper = className + "." + methodName + ":" + String.valueOf(bcIndex) + ":" + operand.toString()
		
		var resPredSetOpt = cacheCMOperand2Pred.get(cmboper)
		if (resPredSetOpt != None) return resPredSetOpt.get
		
		
		var resPredSet = Set[AtomicPredicate]()
		
		if (ExpressionUtils.isConstantValue(operand)) 
		{
			cacheCMOperand2Pred.put(cmboper, resPredSet)
			return resPredSet
		}
		
		for (pred <- static2predset.getOrElseUpdate(className, Set[AtomicPredicate]()))
		{
			if (pred.containsOperand(operand)) resPredSet = resPredSet + pred	
		}
		
		for (pred <- object2predset.getOrElseUpdate(className, Set[AtomicPredicate]()))
		{
			if (pred.containsOperand(operand)) resPredSet = resPredSet + pred	
		}
		
		for (pred <- method2predset.getOrElseUpdate(className + "." + methodName, Set[AtomicPredicate]()))
		{
			if (pred.containsOperand(operand) && isValidScopeForPredicate(pred, className + "." + methodName, bcIndex)) resPredSet = resPredSet + pred	
		}
		
		cacheCMOperand2Pred.put(cmboper, resPredSet)
		
		return resPredSet
	}
	
	def getAliasingPredicates() : Set[AtomicPredicate] =
	{
		if (cacheAliasingPreds != null) return cacheAliasingPreds
		
		
		var aliasPreds = Set[AtomicPredicate]()
		
		for ( (clsName, predSet) <- static2predset )
		{
			for (pred <- predSet)
			{
				if (pred.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(pred.asInstanceOf[BinaryPredicate])) aliasPreds = aliasPreds + pred
			}
		}
		
		for ( (clsName, predSet) <- object2predset )
		{
			for (pred <- predSet)
			{
				if (pred.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(pred.asInstanceOf[BinaryPredicate])) aliasPreds = aliasPreds + pred
			}
		}
		
		for ( (mthName, predSet) <- method2predset )
		{
			for (pred <- predSet)
			{
				if (pred.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(pred.asInstanceOf[BinaryPredicate])) aliasPreds = aliasPreds + pred
			}
		}
		
		cacheAliasingPreds = aliasPreds
		
		return aliasPreds
	}
	
	def getReservedVariableNames() : List[String] =
	{
		return List[String]()
	}
	
	def getReservedFunctionSignatures() : List[(String,String,String)] = 
	{
		var funcNames = List[(String,String,String)]( (Constants.FUNC_ARRAY, "int int", "int"), (Constants.VALUE_NULL+"f", "int", "int") )
		
		return funcNames
	}
	
	def getReservedConstants() : List[(String,String)] =
	{
		var constants = List[(String,String)]( (Constants.VALUE_NULL, Constants.VALUE_NULL+"f 0") )
		
		return constants
	}
	
	def isReservedName(name : String) : Boolean =
	{
		return ((name == Constants.FUNC_ARRAY) || (name == Constants.VALUE_NULL))	
	}
	
	def addMethodPredicate(className : String, methodName : String, pred : AtomicPredicate) =
	{
		var fullMethodName = className + "." + methodName
		addEntityPredicate(method2predset, fullMethodName, pred)
	}
	
	def addObjectPredicate(className : String, pred : AtomicPredicate)
	{
		addEntityPredicate(object2predset, className, pred)
	}
	
	def addStaticPredicate(className : String, pred : AtomicPredicate)
	{
		addEntityPredicate(static2predset, className, pred)
	}
	
	private def addEntityPredicate(entity2predset : Map[String, Set[AtomicPredicate]], entityName : String, pred : AtomicPredicate)
	{
		var entityPredSet = entity2predset.getOrElseUpdate(entityName, Set[AtomicPredicate]())
		
		entityPredSet = entityPredSet + pred
		
		entity2predset.put(entityName, entityPredSet)
	}
	
	def dropUnnecessaryPredicates(requiredPredicatesWithMethodName : List[(AtomicPredicate, String)], programClasses : List[String]) =
	{
	}
	
	def dropPredicatesWithNonlinearArithmetic() =
	{
		dropPredsWithNonlinearOps(static2predset)
		dropPredsWithNonlinearOps(object2predset)
		dropPredsWithNonlinearOps(method2predset)
	}
	
	private def dropPredsWithNonlinearOps(entity2predset : Map[String, Set[AtomicPredicate]]) =
	{
		for (entityName <- entity2predset.keys)
		{
			var newPredSet = Set[AtomicPredicate]()
			
			var oldPredSet = entity2predset.getOrElse(entityName, Set[AtomicPredicate]())
			
			for (pred <- oldPredSet)
			{
				if ( ! (pred.containsExpression("* ") || pred.containsExpression("/ ")) ) newPredSet = newPredSet + pred
			}
			
			entity2predset.put(entityName, newPredSet)
		}
	}
	
	def defineScopeForPredicatesOverMethodVars(mthvar2bcindexes : Map[(String, String), List[(Int, Int)]]) =
	{
		for ( (fullMthName, mthPredSet) <- method2predset )
		{
			for (pred <- mthPredSet)
			{
				val predVarNames = FormulaUtils.extractVariableNames(pred)
				
				// find minimal scope of local variables used in the predicate
				
				var mthpredBCIndexes = List[(Int,Int)]()
				
				for ( (mthvar, varBCIndexes) <- mthvar2bcindexes if (mthvar._1 == fullMthName) )
				{
					val localVarName = mthvar._2
					
					if (predVarNames.contains(localVarName))
					{
						if (mthpredBCIndexes.size == 0)
						{
							// initial scope
							
							mthpredBCIndexes = mthpredBCIndexes ++ varBCIndexes
						}
						else
						{
							// refine the scope
							
							var newPredBCIndexes = List[(Int,Int)]()
							
							for ( (oldStart, oldEnd) <- mthpredBCIndexes )
							{
								var newStart = oldStart
								var newEnd = oldEnd
								
								for ( (varStart, varEnd) <- varBCIndexes )
								{
									if ((varStart > newStart) && (varStart <= newEnd)) newStart = varStart
									if ((varEnd < newEnd) && (varEnd >= newStart)) newEnd = varEnd
								}
								
								newPredBCIndexes = newPredBCIndexes :+ ( (newStart, newEnd) )
							}
							
							mthpredBCIndexes = newPredBCIndexes							
						}
					}
				}
				
				mthpred2bcindexes.put( (fullMthName, pred), mthpredBCIndexes )
			}
		}
	}

	protected def isValidScopeForPredicate(pred : AtomicPredicate, fullMethodName : String, bcIndex : Int) : Boolean =
	{
		// check bytecode index (scope, locality) for predicates over method variables

		// we want all predicates in this case
		if (bcIndex == -1) return true
		
		val bcIndexes = mthpred2bcindexes.getOrElse( (fullMethodName, pred), List[(Int, Int)]() )
		
		// by default, each predicate is valid for the whole method
		if (bcIndexes.length == 0) return true
		
		// test all ranges
		for ( (start, end) <- bcIndexes )
		{
			if ((bcIndex >= start) && (bcIndex <= end)) return true
		}
		
		return false
	}
	
	def recordMissingPredicate(pred : AtomicPredicate) =
	{
		missingPredicates = missingPredicates + pred
	}
	
	def getMissingPredicates() : Set[AtomicPredicate] =
	{
		return missingPredicates	
	}
	
	def clearMissingPredicates() =
	{
		missingPredicates = Set[AtomicPredicate]()	
	}

	def invalidateCaches() =
	{
		cacheCMExprStr2Pred.clear()
		cacheCMOperand2Pred.clear()
		cacheAliasingPreds = null 
	}
	
	def clearAll() =
	{
		static2predset.clear()
		object2predset.clear()
		method2predset.clear()

		cacheCMExprStr2Pred.clear()
		cacheCMOperand2Pred.clear()
		cacheAliasingPreds = null 
	}
}

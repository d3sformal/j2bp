package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod} 
import com.ibm.wala.shrikeBT._ 
import com.ibm.wala.types.TypeReference

import common.Constants
import common.Expression
import common.FunctionExpression
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.ExpressionUtils
import common.FormulaUtils

import util.StringUtils


class DefaultPredicatesGenerator extends PredicatesGenerator with ExecutionVisitor
{
	/**
	 * This method returns tuple of maps: 
	 *  (1) from class name to predicates over static fields; 
	 *  (2) from class name to predicates over object fields;
	 *  (3) from full method name to predicates over local variables and method parameters.
	 */
	def inferPredicates(progClasses : List[String], mthvar2values : Map[(String, Expression), Set[Expression]]) : ( Map[String, Set[AtomicPredicate]], Map[String, Set[AtomicPredicate]], Map[String, Set[AtomicPredicate]] ) =
	{
		val static2predicates : Map[String, Set[AtomicPredicate]] = new HashMap
		val object2predicates : Map[String, Set[AtomicPredicate]] = new HashMap
		val method2predicates : Map[String, Set[AtomicPredicate]] = new HashMap
		
		
		val ctx = Main.initNewContext(progClasses)
		
		for (className <- progClasses)
		{
			val cls = WALAUtils.findClass(className)
				
			if (Main.INFO) println("[INFO] generating predicates for class: " + className);
			
			ctx.initCurClass(className)
			
			// generate method abstractions
			for (mth <- cls.getDeclaredMethods()) 
			{
				ctx.initCurMethod(WALAUtils.getShortMethodName(mth), mth.isStatic()) 

				inferPredicatesForMethod(mth.asInstanceOf[IBytecodeMethod], cls.isInterface() || mth.isAbstract(), ctx, static2predicates, object2predicates, method2predicates)
			}
		}
		
		return (static2predicates, object2predicates, method2predicates)
	}
	
	def inferPredicatesForMethod(mth : IBytecodeMethod, isAbstract : Boolean, ctx : AbstractionContext, static2predicates : Map[String, Set[AtomicPredicate]], object2predicates : Map[String, Set[AtomicPredicate]], method2predicates : Map[String, Set[AtomicPredicate]]) =
	{
		if (Main.INFO) println("[INFO] inferring predicates for method: " + ctx.getCurMethodName());
		
		// loop through all Shrike bytecode instructions and process each relevant one (some are ignored) to get necessary information for generating predicates
		
		var dataHolder : Map[String, java.lang.Object] = new HashMap
		
		dataHolder.put("s2p", static2predicates)
		dataHolder.put("o2p", object2predicates)
		dataHolder.put("m2p", method2predicates)
		
		ExecutionSimulator.simulateMethod(mth, isAbstract, ctx, this, dataHolder)
		
		// each tuple in "newPredsInfoList" contains a new predicate, full method name, and some important expressions
		val newPredsInfoList = dataHolder.getOrElse("newPreds", List[(AtomicPredicate, String, List[Expression])]()).asInstanceOf[List[(AtomicPredicate, String, List[Expression])]]


		// generate predicates from retrieved information 

		for ( newPredInfo <- newPredsInfoList )
		{
			if (containsMethodVariables(newPredInfo._3)) addPredicate(newPredInfo._2, method2predicates, newPredInfo._1)
			else if (containsObjectFields(newPredInfo._3)) addPredicate(StringUtils.extractClassName(newPredInfo._2), object2predicates, newPredInfo._1)
			else addPredicate(StringUtils.extractClassName(newPredInfo._2), static2predicates, newPredInfo._1)
		}
	}
	
	override def visitArrayStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, arrayExpr : Expression, index : Expression, newValue : Expression, elementType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		var newPredsInfoList = dataHolder.getOrElse("newPreds", List[(AtomicPredicate, String, List[Expression])]()).asInstanceOf[List[(AtomicPredicate, String, List[Expression])]]
		
		// we need predicates for all assignments
		if ( ! ExpressionUtils.isStackSymbol(newValue) )
		{
			val newPred = new BinaryPredicate("=", ExpressionUtils.createArrayAccessExpr(arrayExpr, index), newValue)

			newPredsInfoList = newPredsInfoList :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(arrayExpr, newValue))
		}
		
		dataHolder.put("newPreds", newPredsInfoList)
	}
	
	override def visitConditionalBranchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, posOpStr : String, negOpStr : String, value1 : Expression, value2 : Expression, target : Int, backjumpInsnPos : Int) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]

		var newPredsInfoList = dataHolder.getOrElse("newPreds", List[(AtomicPredicate, String, List[Expression])]()).asInstanceOf[List[(AtomicPredicate, String, List[Expression])]]

		if ( ( ! ExpressionUtils.isStackSymbol(value1) ) && ( ! ExpressionUtils.isStackSymbol(value2) ) )
		{
			val newPred = new BinaryPredicate(posOpStr, value1, value2)

			newPredsInfoList = newPredsInfoList :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(value1, value2))
		}

		dataHolder.put("newPreds", newPredsInfoList)
	}
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]

		// get data from holder		
		val newPredsInfoList = dataHolder.getOrElse("newPreds", List[(AtomicPredicate, String, List[Expression])]()).asInstanceOf[List[(AtomicPredicate, String, List[Expression])]]
		
		
		val piRes = Configuration.mthcallAbstr.preprocessInvoke(ctx, ownerClassName, tgtMethodName, tgtMethodParamCount, isStaticCall)
		val targetObj = piRes._1
		val actualParamArray : Array[Expression] = piRes._2
		val skip = piRes._3
		
		if ( ! skip )
		{
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
		}
		
		// put retrieved lists to the holder object
		dataHolder.put("newPreds", newPredsInfoList)
	}
	
	override def visitPutInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String, fieldType : String, newValue : Expression) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		var newPredsInfoList = dataHolder.getOrElse("newPreds", List[(AtomicPredicate, String, List[Expression])]()).asInstanceOf[List[(AtomicPredicate, String, List[Expression])]]
			
		val tgtFieldExpr = ExpressionUtils.createFieldAccessExpr(tgtObj, fieldName)
		
		// we need predicates for all assignments
		if ( ( ! ExpressionUtils.isStackSymbol(newValue) ) && ( ! newValue.contains(tgtFieldExpr.toString()) ) )
		{			
			val newPred = new BinaryPredicate("=", tgtFieldExpr, newValue)

			newPredsInfoList = newPredsInfoList :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(tgtFieldExpr, newValue))
		}
		
		dataHolder.put("newPreds", newPredsInfoList)
	}
	
	override def visitStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtVarExpr : Expression, tgtVarType : String, newValue : Expression) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		var newPredsInfoList = dataHolder.getOrElse("newPreds", List[(AtomicPredicate, String, List[Expression])]()).asInstanceOf[List[(AtomicPredicate, String, List[Expression])]]
		
		// we need predicates for all assignments
		if ( ( ! ExpressionUtils.isStackSymbol(newValue) ) && ( ! newValue.contains(tgtVarExpr.toString()) ) )
		{
			val newPred = new BinaryPredicate("=", tgtVarExpr, newValue)

			newPredsInfoList = newPredsInfoList :+ (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(tgtVarExpr, newValue))
		}

		dataHolder.put("newPreds", newPredsInfoList)
	}

	override def visitSwitchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, testedValue : Expression, caseValues : List[Int], caseTargets : List[Int], defaultTarget : Int) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		var newPredsInfoList = dataHolder.getOrElse("newPreds", List[(AtomicPredicate, String, List[Expression])]()).asInstanceOf[List[(AtomicPredicate, String, List[Expression])]]
				
		for (i <- 0 to (caseValues.size() - 1))
		{
			val caseValueStr = caseValues(i).toString()			
			val caseValueExpr = ExpressionUtils.createExprFromStr(caseValueStr)
			
			if ( ! ExpressionUtils.isStackSymbol(testedValue) )
			{
				val newPred = new BinaryPredicate("=", testedValue, caseValueExpr)

				newPredsInfoList = newPredsInfoList :+ ( (newPred, ctx.getCurClassOrigName()+"."+ctx.getCurMethodName(), List(testedValue, caseValueExpr)) )
			}
		}

		dataHolder.put("newPreds", newPredsInfoList)
	}	
	
	private def containsMethodVariables(exprList : List[Expression]) : Boolean =
	{
		for (expr <- exprList)
		{
			if (ExpressionUtils.containsMethodVariables(expr)) return true
		}
		
		return false
	}
	
	private def containsObjectFields(exprList : List[Expression]) : Boolean =
	{
		for (expr <- exprList)
		{
			if (ExpressionUtils.containsObjectFields(expr)) return true
		}
		
		return false
	}
}

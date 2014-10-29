package j2bp.containers

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import util.StringUtils

import common.Constants
import common.Expression
import common.ExpressionUtils

import j2bp.AbstractionContext
import j2bp.Configuration


// computes scope also for temporary variables used in translation from Java collections to abstract map
class ContainerVarScopeDetector extends j2bp.analysis.DefaultVarScopeDetector
{
	override def visitArrayStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, arrayExpr : Expression, index : Expression, newValue : Expression, elementType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		if (ExpressionUtils.isMethodVariable(arrayExpr)) recordMethodVariableDefUse(dataHolder, arrayExpr.toString(), insnIndex)
		
		saveTargetMapVariable(ctx, ExpressionUtils.createArrayAccessExpr(arrayExpr, index), elementType)
	}
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String,java.lang.Object]]
		
		val storedVarsInMaps = dataHolder.getOrElse("storedvars", new HashMap[String, Set[String]]).asInstanceOf[Map[String, Set[String]]]
		
		val curMethodNameFull = ctx.getCurClassOrigName() + "." + ctx.getCurMethodName()
		
		
		val piRes = Configuration.mthcallAbstr.preprocessInvoke(ctx, ownerClassName, tgtMethodName, tgtMethodParamCount, isStaticCall)
		val targetObj = piRes._1
		val actualParamArray : Array[Expression] = piRes._2
		val skip = piRes._3
		
		
		if ( ! skip )
		{
			// we must copy the code from superclass here, because the call of super method would destroy the expression stack
			
			val tgtObjVarExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(targetObj)
			
			val tgtObjVarNames = ExpressionUtils.extractVariableNames(tgtObjVarExpr)

			for (vname <- tgtObjVarNames)
			{
				if (ExpressionUtils.isMethodVariable(vname)) 
				{
					recordMethodVariableDefUse(dataHolder, vname, insnIndex)
					
					val varNameExpr = new Expression(vname)
					
					// map variable has at least the same scope as iterator variable
					if (ContainerAbstractionData.isIteratorVariable(ctx.getCurClassOrigName(), ctx.getCurMethodName(), varNameExpr))
					{
						val assocMapVars = ContainerAbstractionData.getMapsForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), varNameExpr)

						for (mapVar <- assocMapVars)
						{
							recordMethodVariableDefUse(dataHolder, mapVar.toString(), insnIndex)
						}
					}
				}
			}
				
			for (i <- 0 to (actualParamArray.length - 1))
			{
				val paramVarNames = ExpressionUtils.extractVariableNames(actualParamArray(i))
				
				for (vname <- paramVarNames)
				{
					if (ExpressionUtils.isMethodVariable(vname))
					{
						recordMethodVariableDefUse(dataHolder, vname, insnIndex)
						
						val varNameExpr = new Expression(vname)
						
						// map variable has at least the same scope as iterator variable
						if (ContainerAbstractionData.isIteratorVariable(ctx.getCurClassOrigName(), ctx.getCurMethodName(), varNameExpr))
						{
							val assocMapVars = ContainerAbstractionData.getMapsForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), varNameExpr)

							for (mapVar <- assocMapVars)
							{
								recordMethodVariableDefUse(dataHolder, mapVar.toString(), insnIndex)
							}
						}
					}					
				}
			}
			
			if (retVarExpr != null)
			{
				if (ExpressionUtils.isMethodVariable(retVarExpr)) 
				{
					recordMethodVariableDefUse(dataHolder, retVarExpr.toString(), insnIndex)
					
					// map variable has at least the same scope as iterator variable
					if (ContainerAbstractionData.isIteratorVariable(ctx.getCurClassOrigName(), ctx.getCurMethodName(), retVarExpr))
					{
						val assocMapVars = ContainerAbstractionData.getMapsForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), retVarExpr)

						for (mapVar <- assocMapVars)
						{
							recordMethodVariableDefUse(dataHolder, mapVar.toString(), insnIndex)
						}
					}					
				}
			}
			
			
			if ( BasicContainerModel.isSupportedMethod(ownerClassName, tgtMethodName) )
			{
				val absOperSeq = BasicContainerModel.getAbstractOperationSeq(ownerClassName, tgtMethodName, targetObj, actualParamArray, retVarExpr, ctx.getCurClassOrigName(), ctx.getCurMethodName(), insnIndex)
				
				for (absOper <- absOperSeq)
				{
					val absOperName = absOper._1
					val absTargetObj = absOper._2
					val absParamVals = absOper._3
					val absRetVarExprs = absOper._4
					
			
					if (absOperName == "createIterator") 
					{
						// we must save assocation between iterators and maps for the purpose of generating the correct properties
						ContainerAbstractionData.saveMapForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), absRetVarExprs(0), absTargetObj)
					}					
			

					var tempVarNames = List[String]()
					
					// find all temporary variables used in translation from Java container methods to operations upon abstract maps
					
					if (ExpressionUtils.isTemporaryVariable(absTargetObj)) tempVarNames = tempVarNames :+ absTargetObj.toString()
					
					for (i <- 0 to (absParamVals.length - 1))
					{
						if (ExpressionUtils.isTemporaryVariable(absParamVals(i))) tempVarNames = tempVarNames :+ absParamVals(i).toString()
					}
					
					for (i <- 0 to (absRetVarExprs.length - 1))
					{
						if (ExpressionUtils.isTemporaryVariable(absRetVarExprs(i))) tempVarNames = tempVarNames :+ absRetVarExprs(i).toString()
					}
					
					for (tmpVarName <- tempVarNames)
					{
						recordMethodVariableDefUse(dataHolder, tmpVarName, insnIndex)
						
						val tmpVarNameExpr = new Expression(tmpVarName)
					
						// map variable has at least the same scope as iterator variable
						if (ContainerAbstractionData.isIteratorVariable(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tmpVarNameExpr))
						{
							val assocMapVars = ContainerAbstractionData.getMapsForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tmpVarNameExpr)

							for (mapVar <- assocMapVars)
							{
								recordMethodVariableDefUse(dataHolder, mapVar.toString(), insnIndex)
							}
						}
					}
					
					
					// record local variables stored in the map 
					
					if (absOperName == "put")
					{
						var storedVars = storedVarsInMaps.getOrElse(absTargetObj.toString(), Set[String]())
						
						if (ExpressionUtils.isMethodVariable(absParamVals(0))) storedVars = storedVars + absParamVals(0).toString()
						if (ExpressionUtils.isMethodVariable(absParamVals(1))) storedVars = storedVars + absParamVals(1).toString()
						
						storedVarsInMaps.put(absTargetObj.toString(), storedVars)
					}
					
					if (absOperName == "putAhead")
					{
						var storedVars = storedVarsInMaps.getOrElse(absTargetObj.toString(), Set[String]())
						
						if (ExpressionUtils.isMethodVariable(absParamVals(0))) storedVars = storedVars + absParamVals(0).toString()
						if (ExpressionUtils.isMethodVariable(absParamVals(1))) storedVars = storedVars + absParamVals(1).toString()
						if (ExpressionUtils.isMethodVariable(absParamVals(2))) storedVars = storedVars + absParamVals(2).toString()
						
						storedVarsInMaps.put(absTargetObj.toString(), storedVars)
					}
				}
			}
			
			// all local variables stored in maps must have the scope ending at the same point as scope of the map variable			
			if (ContainerAbstractionData.isMapExpression(ctx.getCurClassOrigName(), ctx.getCurMethodName(), targetObj))
			{
				var storedVars = storedVarsInMaps.getOrElse(targetObj.toString(), Set[String]())
				
				for (vname <- storedVars) 
				{
					recordMethodVariableDefUse(dataHolder, vname, insnIndex)
				}
			}
			
					
			dataHolder.put("storedvars", storedVarsInMaps)
			
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
		}
	}
	
	override def visitPutInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String, fieldType : String, newValue : Expression) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val fapTgtObjExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(tgtObj)

		val varNames = ExpressionUtils.extractVariableNames(fapTgtObjExpr)

		for (vname <- varNames)
		{
			if (ExpressionUtils.isMethodVariable(vname)) recordMethodVariableDefUse(dataHolder, vname, insnIndex)
		}
		
		saveTargetMapVariable(ctx, fapTgtObjExpr, fieldType)
	}
	
	override def visitStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtVarExpr : Expression, tgtVarType : String, newValue : Expression) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val newValueVarNames = ExpressionUtils.extractVariableNames(newValue)

		if (ExpressionUtils.isMethodVariable(tgtVarExpr)) recordMethodVariableDefUse(dataHolder, tgtVarExpr.toString(), insnIndex)
		
		for (varName <- newValueVarNames)
		{
			if (ExpressionUtils.isMethodVariable(varName)) recordMethodVariableDefUse(dataHolder, varName, insnIndex)
		}
		
		saveTargetMapVariable(ctx, tgtVarExpr, tgtVarType)
	}

	protected def saveTargetMapVariable(ctx : AbstractionContext, tgtVarExpr : Expression, tgtVarType : String)
	{
		// we assume that value of the tgtVarType variable has the form "Ljava/util/List;"
		var tgtVarClassName = StringUtils.getPlainClassName(tgtVarType)
		
		if (BasicContainerModel.isSupportedClass(tgtVarClassName))
		{
			// remember the map variable
			ContainerAbstractionData.saveMapExpression(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtVarExpr)
		}
	}
}

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
package j2bp.containers

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod} 
import com.ibm.wala.shrikeBT._ 
import com.ibm.wala.types.TypeReference

import common.Constants
import common.Expression
import common.LogicFormula
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.Negation
import common.ExpressionUtils
import common.Property

import j2bp.Main
import j2bp.Configuration
import j2bp.AbstractionContext
import j2bp.WALAUtils
import j2bp.ExecutionSimulator


class ContainerPropertiesGenerator extends j2bp.PropertiesGenerator with j2bp.ExecutionVisitor
{
	def inferProperties(progClasses : List[String]) : List[Property] =
	{
		var properties = List[Property]()
		
		
		val ctx = Main.initNewContext(progClasses)
		

		for (className <- progClasses)
		{
			val cls = WALAUtils.findClass(className)
				
			if (Main.INFO) println("[INFO] generating properties for class: " + className);
			
			ctx.initCurClass(className)
		
			// generate method abstractions
			for (mth <- cls.getDeclaredMethods()) 
			{
				ctx.initCurMethod(WALAUtils.getShortMethodName(mth), mth.isStatic()) 

				properties = inferPropertiesForMethod(mth.asInstanceOf[IBytecodeMethod], cls.isInterface() || mth.isAbstract(), ctx, properties)
			}
		}
		
		return properties
	}
	
	def inferPropertiesForMethod(mth : IBytecodeMethod, isAbstract : Boolean, ctx : AbstractionContext, existingProperties : List[Property]) : List[Property] =
	{
		if (Main.INFO) println("[INFO] inferring properties for method: " + ctx.getCurMethodName());

		// loop through all Shrike bytecode instructions and process each relevant one (some are ignored) to get necessary information for generating predicates

		var dataHolder : Map[String, java.lang.Object] = new HashMap
		
		dataHolder.put("props", existingProperties)
		dataHolder.put("retvars", List[String]())
		
		ExecutionSimulator.simulateMethod(mth, isAbstract, ctx, this, dataHolder)
		
		return dataHolder.getOrElse("props", List[Property]()).asInstanceOf[List[Property]]
	}
	
	override def visitConditionalBranchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, posOpStr : String, negOpStr : String, value1 : Expression, value2 : Expression, target : Int, backjumpInsnPos : Int) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		if ( ( ! ExpressionUtils.isStackSymbol(value1) ) && ( ! ExpressionUtils.isStackSymbol(value2) ) )
		{
			val branchPred = new BinaryPredicate(posOpStr, value1, value2)

			dataHolder.put("lastBranchPred", branchPred)
		}
	}
	
	override def visitGetInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		var properties = dataHolder.getOrElse("props", List[Property]()).asInstanceOf[List[Property]] 
		var retvarNames = dataHolder.getOrElse("retvars", List[String]()).asInstanceOf[List[String]]

		val tgtFieldExpr = ExpressionUtils.createFieldAccessExpr(tgtObj, fieldName)
		
		properties = addNotNullPropertyForFieldAccess(ctx, insnIndex, properties, retvarNames, tgtFieldExpr)
		
		dataHolder.put("props", properties)
	}	
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		var properties = dataHolder.getOrElse("props", List[Property]()).asInstanceOf[List[Property]] 
		var retvarNames = dataHolder.getOrElse("retvars", List[String]()).asInstanceOf[List[String]]

		
		// this must be done before preprocessing invoke because Verify methods are skipped
		if ((ownerClassName == "gov.nasa.jpf.jvm.Verify") && (tgtMethodName == "assertTrue"))
		{
			val branchPredOpt = dataHolder.get("lastBranchPred")
				
			if (branchPredOpt != None)
			{				
				val newProp = new Property(branchPredOpt.get.asInstanceOf[LogicFormula], List[(String, Int)]( (ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), insnIndex - 1) ))
					
				properties = properties :+ newProp
			}
		}
		
		
		val piRes = Configuration.mthcallAbstr.preprocessInvoke(ctx, ownerClassName, tgtMethodName, tgtMethodParamCount, isStaticCall)
		val targetObj = piRes._1
		val actualParamArray : Array[Expression] = piRes._2
		val skip = piRes._3
		
		if ( ! skip )
		{
			if ( BasicContainerModel.isSupportedMethod(ownerClassName, tgtMethodName) )
			{
				var propFormulaList = List[LogicFormula]()
				
				if (ownerClassName.contains("List"))
				{
					if (tgtMethodName.equals("get") || tgtMethodName.equals("set") || tgtMethodName.equals("remove"))
					{
						if ( ! ExpressionUtils.isConstantValue(actualParamArray(0)) )
						{						
							// 0 <= i								
							propFormulaList = propFormulaList :+ new BinaryPredicate("<=", Constants.ZERO_EXPR, actualParamArray(0))
						}
						
						// i < size()								
						propFormulaList = propFormulaList :+ new BinaryPredicate(">", ContainerExpressionUtils.createMapSizeExpr(targetObj), actualParamArray(0))																
					}
					
					// add(i,o)
					if (tgtMethodName.equals("add") && (actualParamArray.length == 2))
					{	
						if ( ! ExpressionUtils.isConstantValue(actualParamArray(0)) )
						{
							// 0 <= i								
							propFormulaList = propFormulaList :+ new BinaryPredicate("<=", Constants.ZERO_EXPR, actualParamArray(0))
						}
						
						// i <= size()								
						propFormulaList = propFormulaList :+ new BinaryPredicate(">=", ContainerExpressionUtils.createMapSizeExpr(targetObj), actualParamArray(0))								
					}
				}
				
				if (tgtMethodName == "next") 
				{
					val mapObj = ContainerAbstractionData.getMapsForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), targetObj).head
						
					propFormulaList = propFormulaList :+ new Negation(new UnaryPredicate("", ContainerExpressionUtils.createMapOrderExpr(mapObj, targetObj, BasicContainerModel.BOTTOM_EXPR)))								
				}
				
				if (tgtMethodName == "get")
				{
					if ( ExpressionUtils.isMethodVariable(retVarExpr) && ( ! ExpressionUtils.isMethodParameter(retVarExpr) ) ) retvarNames = retvarNames :+ retVarExpr.toString()	
				}
				
				val absOperSeq = BasicContainerModel.getAbstractOperationSeq(ownerClassName, tgtMethodName, targetObj, actualParamArray, retVarExpr,  ctx.getCurClassOrigName(), ctx.getCurMethodName(), insnIndex)
				
				for (absOper <- absOperSeq)
				{
					val absOperName = absOper._1
					val absTargetObj = absOper._2
					val absParamVals = absOper._3
					val absRetVarExprs = absOper._4
					
					if (Main.DEBUG) println("[DEBUG ContainerPropertiesGenerator.visitInvokeInsn] abstract operation name = " + absOperName)
					if (Main.DEBUG) println("[DEBUG ContainerPropertiesGenerator.visitInvokeInsn] abstract operation target = '" + absTargetObj + "'")
					
					if (Main.DEBUG)
					{
						if (absRetVarExprs.length > 0)
						{
							println("[DEBUG ContainerPropertiesGenerator.visitInvokeInsn] abstract operation return = '" + absRetVarExprs(0) + "'")
						}
					}	
					
					if (absOperName == "createIterator") 
					{
						// we must save assocation between iterators and maps for the purpose of generating the correct properties
						ContainerAbstractionData.saveMapForIterator(ctx.getCurClassOrigName(), ctx.getCurMethodName(), absRetVarExprs(0), absTargetObj)
					}					
				}
				
				for (propFormula <- propFormulaList)
				{						
					// create the actual property based on the formula and code location
					
					val newProp = new Property(propFormula, List[(String, Int)]( (ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), insnIndex - 1) ))
					
					properties = properties :+ newProp
				}
			}
			
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
		}
		
		dataHolder.put("props", properties)
		dataHolder.put("retvars", retvarNames)
	}

	override def visitPutInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String, fieldType : String, newValue : Expression) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		var properties = dataHolder.getOrElse("props", List[Property]()).asInstanceOf[List[Property]] 
		var retvarNames = dataHolder.getOrElse("retvars", List[String]()).asInstanceOf[List[String]]
		
		val tgtFieldExpr = ExpressionUtils.createFieldAccessExpr(tgtObj, fieldName)
		
		properties = addNotNullPropertyForFieldAccess(ctx, insnIndex, properties, retvarNames, tgtFieldExpr)
		
		dataHolder.put("props", properties)
	}

	private def addNotNullPropertyForFieldAccess(ctx : AbstractionContext, insnIndex : Int, properties : List[Property], retvarNames : List[String], tgtFieldExpr : Expression) : List[Property] =
	{
		val tgtObjVarExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(tgtFieldExpr)
		
		if (retvarNames.contains(tgtObjVarExpr.toString()))
		{
			val propFormula = new Negation(new BinaryPredicate("=", tgtObjVarExpr, Constants.NULL_EXPR))
			
			val newProp = new Property(propFormula, List[(String, Int)]( (ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), insnIndex - 1) ))
			
			return properties :+ newProp
		}
		
		return properties
	}
}

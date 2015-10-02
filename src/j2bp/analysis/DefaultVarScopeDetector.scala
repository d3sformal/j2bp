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
package j2bp.analysis

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod} 

import common.Constants
import common.Expression
import common.ExpressionUtils
import common.BinaryPredicate
import common.FormulaUtils

import j2bp.AbstractionContext
import j2bp.Configuration


// computes liveness analysis for each local variable in each method of program classes 
class DefaultVarScopeDetector extends j2bp.ExecutionVisitor
{
	override def visitArrayStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, arrayExpr : Expression, index : Expression, newValue : Expression, elementType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		if (ExpressionUtils.isMethodVariable(arrayExpr)) recordMethodVariableDefUse(dataHolder, arrayExpr.toString(), insnIndex)
	}

	override def visitConditionalBranchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, posOpStr : String, negOpStr : String, value1 : Expression, value2 : Expression, target : Int, backjumpInsnPos : Int) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val varNames1 = ExpressionUtils.extractVariableNames(value1)
		val varNames2 = ExpressionUtils.extractVariableNames(value2)
		
		for (vname <- (varNames1 ++ varNames2))
		{
			if (ExpressionUtils.isMethodVariable(vname)) 
			{
				recordMethodVariableDefUse(dataHolder, vname, insnIndex)
			
				// variables used in the loop condition must be live for the whole loop body
				if (backjumpInsnPos != -1) recordMethodVariableDefUse(dataHolder, vname, backjumpInsnPos)
			}
		}
	}
	
	override def visitGetInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val fapTgtObjExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(tgtObj)
		
		val varNames = ExpressionUtils.extractVariableNames(fapTgtObjExpr)

		for (vname <- varNames)
		{
			if (ExpressionUtils.isMethodVariable(vname)) recordMethodVariableDefUse(dataHolder, vname, insnIndex)
		}
	}
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		
		val piRes = Configuration.mthcallAbstr.preprocessInvoke(ctx, ownerClassName, tgtMethodName, tgtMethodParamCount, isStaticCall)
		val targetObj = piRes._1
		val actualParamArray : Array[Expression] = piRes._2
		val skip = piRes._3
			

		if ( ! skip )
		{
			val tgtObjVarExpr = ExpressionUtils.extractTargetObjExprFromFieldAccessPath(targetObj)
			
			val tgtObjVarNames = ExpressionUtils.extractVariableNames(tgtObjVarExpr)

			for (vname <- tgtObjVarNames)
			{
				if (ExpressionUtils.isMethodVariable(vname)) recordMethodVariableDefUse(dataHolder, vname, insnIndex)
			}
				
			for (i <- 0 to (actualParamArray.length - 1))
			{
				val paramVarNames = ExpressionUtils.extractVariableNames(actualParamArray(i))
				
				for (vname <- paramVarNames)
				{
					if (ExpressionUtils.isMethodVariable(vname)) recordMethodVariableDefUse(dataHolder, vname, insnIndex)
				}
			}
			
			if (retVarExpr != null)
			{
				if (ExpressionUtils.isMethodVariable(retVarExpr)) recordMethodVariableDefUse(dataHolder, retVarExpr.toString(), insnIndex)
			}
			
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
		}
	}

	override def visitNewInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtVarExpr : Expression, tgtVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		if (ExpressionUtils.isMethodVariable(tgtVarExpr)) recordMethodVariableDefUse(dataHolder, tgtVarExpr.toString(), insnIndex)
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
	}
	
	override def visitSwitchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, testedValue : Expression, caseValues : List[Int], caseTargets : List[Int], defaultTarget : Int) =
	{
		val dataHolder = arg.asInstanceOf[Map[String, java.lang.Object]]
		
		val testedValueVarNames = ExpressionUtils.extractVariableNames(testedValue)
		
		for (varName <- testedValueVarNames)
		{
			if (ExpressionUtils.isMethodVariable(varName)) recordMethodVariableDefUse(dataHolder, varName, insnIndex)
		}
	}
	
	protected def recordMethodVariableDefUse(dataHolder : Map[String,java.lang.Object], mainLocalVarName : String, insnIndex : Int)
	{
		val localVarNamesRefs = dataHolder.get("varnamesrefs").get.asInstanceOf[Map[String, (Int, Int)]]
		
		// we consider the given local variable and all other program variables that may be aliased with it
		
		var targetLocalVarNames = Set[String]()
		targetLocalVarNames = targetLocalVarNames + mainLocalVarName
		
		for (aliasPred <- Configuration.predicatesMngr.getAliasingPredicates())
		{
			val binAliasPred = aliasPred.asInstanceOf[BinaryPredicate]
			
			if (binAliasPred.containsOperand(mainLocalVarName))
			{
				val aliasedExpr = FormulaUtils.extractOtherOperandFromBinPred(binAliasPred, mainLocalVarName)
				
				if (ExpressionUtils.isMethodVariable(aliasedExpr))
				{
					val aliasedVarName = aliasedExpr.toString()
					
					targetLocalVarNames = targetLocalVarNames + aliasedVarName
				}
			}
		}
		
		for (tgtVarName <- targetLocalVarNames)
		{		
			var tgtVarNameRefs = localVarNamesRefs.getOrElse(tgtVarName, (-1, -1))
			
			var firstRef = tgtVarNameRefs._1
			var lastRef = tgtVarNameRefs._2
			
			if ((firstRef == -1) || (insnIndex < firstRef)) firstRef = insnIndex
			if ((lastRef == -1) || (insnIndex > lastRef)) lastRef = insnIndex
			
			tgtVarNameRefs = (firstRef, lastRef)
			
			localVarNamesRefs.put(tgtVarName, tgtVarNameRefs)
		}		
		
		dataHolder.put("varnamesrefs", localVarNamesRefs)
	}
}

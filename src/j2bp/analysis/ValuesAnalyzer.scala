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

import common.Expression
import common.ExpressionUtils
import common.Constants

import util.StringUtils

import j2bp.Main
import j2bp.Configuration
import j2bp.AbstractionContext
import j2bp.WALAUtils
import j2bp.ExecutionSimulator


object ValuesAnalyzer extends j2bp.ExecutionVisitor
{
	def computePossibleValues(progClasses : List[String]) : Map[(String, Expression), Set[Expression]] =
	{
		// map from the tuple (full method name, variable name) to a set of possible values
		var mthvar2possvalues : Map[(String, Expression), Set[Expression]] = new HashMap

		var mthQueue = List[String]()

		// put all methods of all program classes into the queue at the start
		for (clsName <- progClasses)
		{
			val cls = WALAUtils.findClass(clsName)
			
			for (mth <- cls.getDeclaredMethods())
			{
				val mthName = WALAUtils.getFullMethodName(mth)
				
				mthQueue = mthQueue :+ mthName
			}
		}


		// init context
		val ctx = Main.initNewContext(progClasses)
		
		
		// main analysis loop
		
		while (mthQueue.size() > 0)
		{
			val curMthName = mthQueue.head
			
			mthQueue = mthQueue.tail
		
			val curClassName = StringUtils.extractClassName(curMthName)

			val curCls = WALAUtils.findClass(curClassName)
				
			ctx.initCurClass(curClassName)
		
			var curMth : IMethod = null
			
			// find current method
			for (mth <- curCls.getDeclaredMethods()) 
			{
				if (WALAUtils.getFullMethodName(mth) == curMthName) curMth = mth
			}
			
			ctx.initCurMethod(WALAUtils.getShortMethodName(curMth), curMth.isStatic()) 

			
			// process the current method
			
			var dataHolder : Map[String,java.lang.Object] = new HashMap
		
			dataHolder.put("invmths", List[String]())
			dataHolder.put("valsets", mthvar2possvalues)
			
			ExecutionSimulator.simulateMethod(curMth.asInstanceOf[IBytecodeMethod], curCls.isInterface() || curMth.isAbstract(), ctx, this, dataHolder)
			
			mthvar2possvalues = dataHolder.get("valsets").get.asInstanceOf[Map[(String, Expression), Set[Expression]]]
			
						
			// put into the queue all methods that must be processed again
			
			val invalidatedMethods = dataHolder.getOrElse("invmths", List[String]()).asInstanceOf[List[String]]			
			
			for (invMthName <- invalidatedMethods)
			{
				if ( ! mthQueue.contains(invMthName) ) mthQueue = mthQueue :+ invMthName				
			}
		}
		
		if (Main.INFO)
		{
			println("[INFO] possible values of local variables:")
			
			for ( (mthvar, possibleValues) <- mthvar2possvalues )
			{
				print("  variable identification = " + mthvar._1 + "." + mthvar._2 + ", possible values = ")
				ExpressionUtils.printSet(possibleValues, "")
			}
		}
		
		return mthvar2possvalues
	}
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String,java.lang.Object]]
		
		var invalidatedMethods = dataHolder.getOrElse("invmths", List[String]()).asInstanceOf[List[String]]
		val mthvar2possvalues = dataHolder.get("valsets").get.asInstanceOf[Map[(String, Expression), Set[Expression]]]
				
		
		val piRes = Configuration.mthcallAbstr.preprocessInvoke(ctx, ownerClassName, tgtMethodName, tgtMethodParamCount, isStaticCall)
		val actualParamArray : Array[Expression] = piRes._2
		val skip = piRes._3
			
		if ( ! skip )
		{
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
			
			if (ctx.isProgramClass(ownerClassName))
			{
				// propagate analysis facts over the method call
					
				var mustRecomputeTgtMethod = false
				
				var formalParamStartIndex = 0
				if ( ! isStaticCall ) formalParamStartIndex = 1
				
				// propagate sets over method parameters
				var actualParamIdx = 0
				for (formalParamIdx <- formalParamStartIndex to (formalParamStartIndex + tgtMethodParamCount - 1))
				{
					if ( ! WALAUtils.hasMethodParamReferenceType(ownerClassName, tgtMethodName, formalParamIdx) )
					{
						val actualParamValue = actualParamArray(actualParamIdx)
						
						val formalParamName = Constants.MTHPARAM_PREFIX + formalParamIdx
						
						if (propagateValueSets(mthvar2possvalues, actualParamValue, ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), new Expression(formalParamName), ownerClassName + "." + tgtMethodName))
						{
							invalidatedMethods = invalidatedMethods :+ (ownerClassName + "." + tgtMethodName) 
						}
					}
					
					actualParamIdx = actualParamIdx + 1
				}
			}
		}
		
		dataHolder.put("invmths", invalidatedMethods)
		dataHolder.put("valsets", mthvar2possvalues)
	}
	
	override def visitArrayStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, arrayExpr : Expression, index : Expression, newValue : Expression, elementType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String,java.lang.Object]]
		
		val mthvar2possvalues = dataHolder.get("valsets").get.asInstanceOf[Map[(String, Expression), Set[Expression]]]
		
		val arrayAccessExpr = ExpressionUtils.createArrayAccessExpr(arrayExpr, index)
		
		val isReferenceArray = elementType.contains("/")
		
		if ( ! isReferenceArray )
		{
			propagateValueSets(mthvar2possvalues, newValue, ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), arrayAccessExpr, ctx.getCurClassOrigName() + "." + ctx.getCurMethodName())
		}
		
		dataHolder.put("valsets", mthvar2possvalues)
	}
	
	override def visitStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtVarExpr : Expression, tgtVarType : String, newValue : Expression) =
	{
		val dataHolder = arg.asInstanceOf[Map[String,java.lang.Object]]
		
		val mthvar2possvalues = dataHolder.get("valsets").get.asInstanceOf[Map[(String, Expression), Set[Expression]]]
		
		val isReferenceVar = tgtVarType.contains("/")
		
		if ( ! isReferenceVar )
		{
			propagateValueSets(mthvar2possvalues, newValue, ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), tgtVarExpr, ctx.getCurClassOrigName() + "." + ctx.getCurMethodName())
		}
		
		dataHolder.put("valsets", mthvar2possvalues)
	}

	private def propagateValueSets(mthvar2possvalues : Map[(String, Expression), Set[Expression]], newValue : Expression, valueMethodName : String, tgtVarExpr : Expression, tgtVarMethodName : String) : Boolean =
	{
		val oldTgtVarSet = mthvar2possvalues.getOrElse( (tgtVarMethodName, tgtVarExpr), Set[Expression]() )
		
		var srcValueSet = Set[Expression]()
		
		// constants and field accesses make singleton sets
		if (ExpressionUtils.isConstantValue(newValue)) 
		{
			srcValueSet = srcValueSet + newValue
		}
		else if (ExpressionUtils.isFieldAccessPath(newValue)) 
		{
			srcValueSet = srcValueSet + newValue			
		}
		else // variable name or array read
		{
			srcValueSet = mthvar2possvalues.getOrElse( (valueMethodName, newValue), Set[Expression]() )
		}
		
		if ( ! srcValueSet.subsetOf(oldTgtVarSet) )
		{
			var newTgtVarSet = oldTgtVarSet
			
			newTgtVarSet = newTgtVarSet ++ srcValueSet
			
			mthvar2possvalues.put( (tgtVarMethodName, tgtVarExpr), newTgtVarSet )
	
			// updated the set and therefore invalidated data for target variable method 
			return true
		}
		
		return false
	}
}

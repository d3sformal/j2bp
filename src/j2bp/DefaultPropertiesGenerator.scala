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
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod} 

import common._


class DefaultPropertiesGenerator extends PropertiesGenerator with ExecutionVisitor
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

		var dataHolder : Map[String,java.lang.Object] = new HashMap
		
		dataHolder.put("props", existingProperties)
		
		ExecutionSimulator.simulateMethod(mth, isAbstract, ctx, this, dataHolder)
		
		return dataHolder.getOrElse("props", List[Property]()).asInstanceOf[List[Property]]
	}
	
	override def visitConditionalBranchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, posOpStr : String, negOpStr : String, value1 : Expression, value2 : Expression, target : Int, backjumpInsnPos : Int) =
	{
		val dataHolder = arg.asInstanceOf[Map[String,java.lang.Object]]
		
		if ( ( ! ExpressionUtils.isStackSymbol(value1) ) && ( ! ExpressionUtils.isStackSymbol(value2) ) )
		{
			val branchPred = new BinaryPredicate(posOpStr, value1, value2)

			dataHolder.put("lastBranchPred", branchPred)
		}
	}
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String,java.lang.Object]]
		
		var properties = dataHolder.getOrElse("props", List[Property]()).asInstanceOf[List[Property]]
		
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
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
		}
		
		dataHolder.put("props", properties)
	}

}

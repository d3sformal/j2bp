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

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Type
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Label

import common.Constants
import common.AtomicPredicate
import common.Expression
import common.FormulaUtils
import common.ExpressionUtils


class DefaultMethodCallAbstractor extends MethodCallAbstractor
{
	def generateInternalCall(ctx : AbstractionContext, mv : MethodVisitor, className : String, methodName : String, isStaticCall : Boolean, dynTargetClasses : List[String], bcIndex : Int) =
	{
		val tgtMethodOrigParamCount = ctx.getMethodOrigParamCount(className, methodName)
		
		var tgtMethodAbsParamCount = ctx.getMethodAbsParamCount(className, methodName)

		if (Main.DEBUG) println("[DEBUG DefaultMethodCallAbstractor.generateInternalCall] static class name = '" + className + "', method name = '" + methodName + "', static = " + isStaticCall + ", orig param count = " + tgtMethodOrigParamCount + ", abstract param count = " + tgtMethodAbsParamCount)

		
		// compute predicates over actual parameter values
		
		val actualParamIdx2Value : Map[Int, Expression] = new HashMap
		
		var actualParamInfoList = List[(Expression, Set[AtomicPredicate])]()
	
		// take all predicates over formal parameters of the original callee method 
		// replace names of the formal parameters in these predicates with the actual parameter values in the original caller
		// technically, for each formal parameter of the original called method (i.e., for "param1", ..., "paramN"), do this:
			// find all predicates over the formal parameter "paramX" that are associated with the original callee
			// for each such predicate, replace name of the formal parameter with actual parameter value in the original caller to get predicate over the actual value
			// remove the actual parameter value in original code from the stack
		
		for (i <- 1 to tgtMethodOrigParamCount)
		{
			val formalParamName : String = Constants.MTHPARAM_PREFIX + String.valueOf(tgtMethodOrigParamCount + 1 - i)
			
			val formalParamPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(className, methodName, formalParamName, -1)
			
			if (Main.DEBUG) FormulaUtils.printAtomPredSet(formalParamPredSet, "[DEBUG DefaultMethodCallAbstractor.generateInternalCall] predicates over the formal parameter '" + formalParamName + "':")
			
			val actualParamValue = ctx.removeExprFromStack()
			
			actualParamIdx2Value.put(tgtMethodOrigParamCount + 1 - i, actualParamValue)
			
			var actualParamPredSet = Set[AtomicPredicate]()
			
			for (formalParamPred <- formalParamPredSet)
			{			
				actualParamPredSet = actualParamPredSet + FormulaUtils.copyWithReplace(formalParamPred, formalParamName, actualParamValue.toString())
			}
			
			actualParamInfoList = actualParamInfoList :+ ( (actualParamValue, actualParamPredSet) )
		}

		
		var targetObj : Expression = null
		var targetObjStr = ""
		
		if ( ! isStaticCall )
		{
			// remove target object from the stack
			targetObj = ctx.removeExprFromStack()
			targetObjStr = targetObj.toString()
		}


		// generate bytecode instructions
		
		// we do not need to load receiver object (this)
		/*
		if ( ! isStaticCall ) mv.visitVarInsn(Opcodes.ALOAD, 0)
		*/
		
		if (targetObjStr != "this")
		{
			// propagate truth values of predicate over target variable and some fields to predicates over "this" associated with target object 
				// update predicates over "this" based on predicates over target variable and some fields of the object
			// 1) take all predicates over "this" associated with the target method of the call and the class where it is defined
			// 2) for each predicate find all predicates over the target variable of the method call and the same fields
			// 3) if there are some predicates over the target variable and the field, then update the respective predicate over "this" based on their truth value
			// we assume that "this" in some predicate operand is always the first element of a field access path (it cannot stay alone)
			
			val calleeThisPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(className, methodName, "this", bcIndex)
			
			for (calleeThisPred <- calleeThisPredSet)
			{
				var callerLocVarFieldPredSet = Set[AtomicPredicate]()
				
				for (vexpr <- FormulaUtils.extractVariableExpressions(calleeThisPred))
				{
					if (vexpr.contains("this"))
					{
						val callerLocVarFieldExpr = vexpr.replace("this", targetObjStr)
						
						callerLocVarFieldPredSet = callerLocVarFieldPredSet ++ Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), callerLocVarFieldExpr.toString(), bcIndex)
					}
				}
				
				if (callerLocVarFieldPredSet.size > 0)
				{
					if (Main.DEBUG) FormulaUtils.printAtomPredSet(callerLocVarFieldPredSet, "[DEBUG DefaultMethodCallAbstractor.generateInternalCall] predicates over caller's local variables:")
					
					Configuration.assignAbstr.generateVariableLoad(ctx, mv, new Expression("this"), calleeThisPred, targetObj, callerLocVarFieldPredSet)
					
					val calleeThisPredBoolVar = ctx.getVariableForPredicate(calleeThisPred).get
					
					GenUtils.generateStoreInstruction(calleeThisPredBoolVar, mv, ctx)
				}
			}
		}
		
		var usedParamCount = 0
		
		// load boolean variables that represent actual argument values
		// put actual values on the stack
		for ( (actualParamValue, generatedParamPredSet) <- actualParamInfoList )
		{
			// derive actual parameter value in the abstracted caller from all predicates about the actual parameter in the original caller using weakest preconditions and SMT solver
				// we mimic assignment from boolean variables representing predicates in the caller to temporary variables representing actual parameters
				
			if (Main.DEBUG) FormulaUtils.printAtomPredSet(generatedParamPredSet, "[DEBUG DefaultMethodCallAbstractor.generateInternalCall] generated predicates over the actual parameter '" + actualParamValue + "':")

			val availableParamPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), actualParamValue.toString(), bcIndex)
			
			if (Main.DEBUG) FormulaUtils.printAtomPredSet(availableParamPredSet, "[DEBUG DefaultMethodCallAbstractor.generateInternalCall] available predicates over the actual parameter '" + actualParamValue + "':")
			
			for (genPred <- generatedParamPredSet)
			{
				// we consider only those predicates over formal parameters in the callee that refer to constants and static fields of any class + if both methods belong to the same object, we consider also predicates that refer to instance fields of the object variable "this"
				
				if ( Configuration.predicatesMngr.isPredicateOverMethodVariables(genPred) || ( Configuration.predicatesMngr.isPredicateOverObjectFields(genPred) && ( ! ( (className == ctx.getCurClassOrigName()) && (targetObjStr == "this") ) ) ) )
				{
					// ignored predicate over formal parameters -> the corresponding actual argument will have non-deterministic boolean value
				
					// put non-deterministic value on the stack
					// generate call to Verify.getBoolean					
					GenUtils.generateChooseBool(mv)			
				}
				else
				{
					// predicate over formal parameter that satisfies our restrictions
					
					Configuration.assignAbstr.generateVariableLoad(ctx, mv, actualParamValue, genPred, actualParamValue, availableParamPredSet)					
				}
				
				usedParamCount = usedParamCount + 1
			}
		}
		
		if (Main.DEBUG) println("[DEBUG DefaultMethodCallAbstractor.generateInternalCall] used parameters count = " + usedParamCount)
		
		// fill unused parameters (for which no values exist at this call site)
		for (i <- (usedParamCount + 1) to tgtMethodAbsParamCount)
		{
			// unused predicate over formal parameters -> the corresponding actual argument will have non-deterministic boolean value
			GenUtils.generateChooseBool(mv)	
		}
		
		
		val mthDesc = GenUtils.createMethodDescriptor(tgtMethodAbsParamCount, ctx.hasMethodAbsReturnValue(className, methodName))
		
		if (Main.DEBUG) println("[DEBUG DefaultMethodCallAbstractor.generateInternalCall] abstract param count = " + tgtMethodAbsParamCount + ", method descriptor = '" + mthDesc + "'")

		
		var calledMethodName = methodName
		if (methodName == "<init>") calledMethodName = "init0"

		
		// select non-deterministically between all methods that can be invoked here
		
		val allEndLabel = new Label()
			
		GenUtils.generateChooseInt(mv, 1, dynTargetClasses.length)

		var curTgtClassIndex = 0
		
		for (curTgtClassName <- dynTargetClasses)
		{
			if (Main.DEBUG) println("[DEBUG DefaultMethodCallAbstractor.generateInternalCall] current dynamic target class name = '" + curTgtClassName + "'")
			
			// generate if-else statement that guards method call on the current target class
				
			curTgtClassIndex = curTgtClassIndex + 1
				
			val curEndLabel = new Label()
			
			mv.visitInsn(Opcodes.DUP)
			mv.visitLdcInsn(curTgtClassIndex)
				
			mv.visitJumpInsn(Opcodes.IF_ICMPNE, curEndLabel)
			
			// pop result of getInt(1,X) from the stack
			mv.visitInsn(Opcodes.POP)

			// generate the actual method call
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, (curTgtClassName + ctx.getAbsClassNameSuffix()).replace('.', '/'), calledMethodName, mthDesc)
			
			mv.visitJumpInsn(Opcodes.GOTO, allEndLabel)
		
			mv.visitLabel(curEndLabel)
			
			if (curTgtClassIndex == dynTargetClasses.length)
			{
				// this handles case where no method was called
			
				// pop result of getInt(1,X) from the stack
				mv.visitInsn(Opcodes.POP)
				
				// pop correct number of arguments from the stack				
				for (i <- 1 to tgtMethodAbsParamCount) mv.visitInsn(Opcodes.POP)

				if (ctx.hasMethodAbsReturnValue(className, methodName))
				{
					// dummy return value (to make bytecode verifier happy)
					mv.visitInsn(Opcodes.ICONST_0)
				}
			}
		}
			
		mv.visitLabel(allEndLabel)

		if (ctx.hasMethodAbsReturnValue(className, methodName))
		{
			if (Main.DEBUG) println("[DEBUG DefaultMethodCallAbstractor.generateInternalCall] adding deterministic return value to the stack for abstract method with return value")
				
			// put "[retvaldet]" symbol on the stack (indicates presence of returned value) 
			ctx.addExprToStack(new Expression(Constants.STACK_ELEM_RETVAL_DET))
		}
		else if (ctx.hasMethodOrigReturnValue(className, methodName))
		{
			// put "[dummy]" symbol on the stack (indicates presence of returned value that will not be used)
			ctx.addExprToStack(new Expression(Constants.STACK_ELEM_DUMMY))
		}

		// decode truth values of predicates over output parameters
		GenUtils.generateDecodingOfOutputParameterValues(mv, ctx, className, methodName, bcIndex, actualParamIdx2Value)
			
		// decoding procedure for truth values of predicates over result variable is called when processing an instruction that reads the result (store, putfield, array store, etc)
		
		if (targetObjStr != "this")
		{
			// propagate truth values of predicate over "this" associated with target object to predicates over the target variable and same fields
				// it is a reversed process to the one performed before the method call (see above)
				
			val callerLocVarPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), targetObjStr, bcIndex)
			
			for (callerLocVarPred <- callerLocVarPredSet)
			{
				var calleeThisFieldPredSet = Set[AtomicPredicate]()
				
				for (vexpr <- FormulaUtils.extractVariableExpressions(callerLocVarPred))
				{
					if (vexpr.contains(targetObjStr))
					{
						val callerThisFieldExpr = vexpr.replace(targetObjStr, "this")
						
						calleeThisFieldPredSet = calleeThisFieldPredSet ++ Configuration.predicatesMngr.getPredicatesOverExpr(className, methodName, callerThisFieldExpr.toString(), bcIndex)
					}
				}
				
				if (calleeThisFieldPredSet.size > 0)
				{
					Configuration.assignAbstr.generateVariableLoad(ctx, mv, targetObj, callerLocVarPred, new Expression("this"), calleeThisFieldPredSet)
					
					val callerLocVarPredBoolVar = ctx.getVariableForPredicate(callerLocVarPred).get
					
					GenUtils.generateStoreInstruction(callerLocVarPredBoolVar, mv, ctx)
				}
			}
		}
	}
	
	def generateLibraryCall(ctx : AbstractionContext, mv : MethodVisitor, className : String, methodName : String, isStaticCall : Boolean, origParamCount : Int, retVarExpr : Expression, retVarType : String, bcIndex : Int) : Unit =
	{
		// completely abstract (remove) the call by setting boolean variables that represent predicates over fields to non-deterministic values (unknown)
		// ignore methods for boxing and unboxing of primitive values (int <-> Integer) 
	
		if (Main.DEBUG) println("[DEBUG DefaultMethodCallAbstractor.generateLibraryCall] class name = '" + className + "', method name = '" + methodName + "', static = " + isStaticCall + ", orig param count = " + origParamCount + ", return variable name = " + retVarExpr)

		// boxing and unboxing methods
		if (isIgnoredLibraryMethod(className, methodName)) 
		{
			ctx.clearTempReturnVariableName()
			return ()
		}		
		
		var actualParamExprList = List[Expression]()
		
		for (i <- 1 to origParamCount)
		{
			val paramExpr = ctx.removeExprFromStack()
			actualParamExprList = paramExpr :: actualParamExprList
		}
		
		if ( ! isStaticCall )
		{
			// remove target object from the stack
			ctx.removeExprFromStack()
		}
		
		if ((className == "java.lang.Object") && (methodName == "<init>")) return ()
		
		if (methodName == "equals")
		{
			// this method returns non-deterministic boolean value
			// put "[retvalndt]" symbol on the stack 
			ctx.addExprToStack(new Expression(Constants.STACK_ELEM_RETVAL_NDT))
			return ()
		}
		
		if ((className == "gov.nasa.jpf.jvm.Verify") && (methodName == "assertTrue"))
		{
			if (actualParamExprList.tail.head == Constants.ZERO_EXPR) GenUtils.generateAssertFalse(mv, actualParamExprList.head.toString())
			else GenUtils.generateAssertTrue(mv, actualParamExprList.head.toString())
			return ()
		}

		
		// set non-deterministic value to each boolean variable that represents a predicate over static and object fields or a predicate over local variables that refers also to some fields
		
		var fieldsPredSet = Set[AtomicPredicate]()
		
		fieldsPredSet = fieldsPredSet ++ Configuration.predicatesMngr.getPredicatesOverStaticFields(ctx.getCurClassOrigName())
		fieldsPredSet = fieldsPredSet ++ Configuration.predicatesMngr.getPredicatesOverObjectFields(ctx.getCurClassOrigName())

		val predsOverParams = Configuration.predicatesMngr.getPredicatesOverMethodParams(ctx.getCurClassOrigName(), ctx.getCurMethodName(), -1)
		val predsOverLocals = Configuration.predicatesMngr.getPredicatesOverMethodLocalVars(ctx.getCurClassOrigName(), ctx.getCurMethodName(), -1)
		
		for ( pred <- ( predsOverParams ++ predsOverLocals ) )
		{
			val varNames = FormulaUtils.extractVariableNames(pred)

			var isOverFields : Boolean = false
			for (vname <- varNames)
			{
				// check whether the predicate refers to some field
				if ( ! ExpressionUtils.isMethodVariable(vname) ) isOverFields = true
			}
			
			if (isOverFields) fieldsPredSet = fieldsPredSet + pred
		}
				
		for ( pred <- fieldsPredSet )
		{
			GenUtils.generateChooseBool(mv)

			// store value into the variable
			
			val predBoolVar = ctx.getVariableForPredicate(pred).get
			
			if (Main.DEBUG) println("[DEBUG DefaultMethodCallAbstractor.generateLibraryCall] variable '" + predBoolVar + "' for predicate '" + pred.toString() + "'")
			
			GenUtils.generateStoreInstruction(predBoolVar, mv, ctx) 
		}
		
		
		// find return type of the method using WALA 
		if (WALAUtils.hasMethodReturnValue(className, methodName))
		{
			if (Main.DEBUG) println("[DEBUG DefaultMethodCallAbstractor.generateInternalCall] adding non-deterministic return value to the stack")
			
			// put "[retvalndt]" symbol on the stack (denoting presence of non-deterministic returned value) 
			ctx.addExprToStack(new Expression(Constants.STACK_ELEM_RETVAL_NDT))
		}
	}
	
	def isIgnoredLibraryMethod(className : String, methodName : String) : Boolean =
	{
		if (className == "java.lang.Integer") 
		{
			if (methodName == "valueOf") return true
			if (methodName == "intValue") return true
			
			return false
		}
		
		if (className == "java.lang.Long") 
		{
			if (methodName == "valueOf") return true
			if (methodName == "intValue") return true
			if (methodName == "longValue") return true
			
			return false
		}
		
		if (className == "java.lang.Short") 
		{
			if (methodName == "valueOf") return true
			if (methodName == "intValue") return true
			if (methodName == "shortValue") return true
			
			return false
		}
		
		return false
	}
	
	def preprocessInvoke(ctx : AbstractionContext, ownerClassName : String, tgtMethodName : String, tgtMethodParamCount : Int, isStaticCall : Boolean) : (Expression, Array[Expression], Boolean) =
	{
		var skip = false

		// target receiver object
		var targetObj : Expression = null
	
		// actual parameters
		var actualParamArray = new Array[Expression](tgtMethodParamCount)

		
		if (Configuration.mthcallAbstr.isIgnoredLibraryMethod(ownerClassName, tgtMethodName)) skip = true
		
		if (ownerClassName == "gov.nasa.jpf.jvm.Verify") 
		{
			// it is static method; remove all parameters from the stack
			for (i <- 1 to tgtMethodParamCount) ctx.removeExprFromStack()
			
			skip = true
		}
		
		if (tgtMethodName == "wait")
		{
			// remove "tgtObj" from the top of the stack
			ctx.removeExprFromStack()
			
			skip = true
		}
		
		if (tgtMethodName == "notify")
		{
			// remove "tgtObj" from the top of the stack
			ctx.removeExprFromStack()
			
			skip = true
		}
		
		if ( (tgtMethodName == "equals") && ( ! ctx.isProgramClass(ownerClassName) ) )
		{
			// remove parameter from the stack
			ctx.removeExprFromStack()
			
			// remove target object from the stack
			ctx.removeExprFromStack()
			
			// this method returns non-deterministic boolean value
			// put "[retvalndt]" symbol on the stack 
			ctx.addExprToStack(new Expression(Constants.STACK_ELEM_RETVAL_NDT))
			
			skip = true
		}
		
		if ( ! skip )
		{
			// for each method parameter, remove "paramExprStr" from the stack
			for (i <- 1 to tgtMethodParamCount)
			{
				actualParamArray(tgtMethodParamCount - i) = ctx.removeExprFromStack()
			}

			if (isStaticCall) targetObj = ExpressionUtils.createExprFromClassName(ownerClassName) 
			else targetObj = ctx.removeExprFromStack()

			if (WALAUtils.hasMethodReturnValue(ownerClassName, tgtMethodName))
			{
				if (ctx.isProgramClass(ownerClassName))
				{
					if (ctx.getTempReturnVariableName() != "")
					{
						// add implicit variable used to store the method result on stack for further processing
						// this applies only to internal method calls
						ctx.addExprToStack(new Expression(ctx.getTempReturnVariableName()))
					}
					else
					{
						ctx.addExprToStack(new Expression(Constants.STACK_ELEM_DUMMY))
					}
				}
			}
		}
		
		return (targetObj, actualParamArray, skip)
	}

	def postprocessInvoke(ctx : AbstractionContext, ownerClassName : String, tgtMethodName : String) = 
	{
	}
}

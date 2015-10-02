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

import common.Constants
import common.Expression
import common.AtomicPredicate
import common.BinaryPredicate
import common.ExpressionUtils
import common.FormulaUtils


object GenUtils
{
	def generateLoadInstruction(varName : String, mv : MethodVisitor, ctx : AbstractionContext) =
	{
		// generates instruction for loading of variable (via iload, getfield or getstatic)

		val isStaticField = varName.startsWith(Constants.BOOL_STATICFIELD_NAME_PREFIX)
		val isObjectField = varName.startsWith(Constants.BOOL_OBJECTFIELD_NAME_PREFIX)
		val isLocalVariable = varName.startsWith(Constants.BOOL_LOCALVAR_NAME_PREFIX)
		val isMethodParam = varName.startsWith(Constants.BOOL_MTHPARAM_NAME_PREFIX)

				
		if (isStaticField) 
		{
			mv.visitFieldInsn(Opcodes.GETSTATIC, ctx.getCurClassAbstractName().replace('.', '/'), varName, Type.BOOLEAN_TYPE.getDescriptor())
		}
		else if (isObjectField) 
		{
			// we do not need to load 'this' for static fields
			/*
			mv.visitVarInsn(Opcodes.ALOAD, 0)
			*/
			
			mv.visitFieldInsn(Opcodes.GETSTATIC, ctx.getCurClassAbstractName().replace('.', '/'), varName, Type.BOOLEAN_TYPE.getDescriptor())
		}
		else if (isLocalVariable) 
		{
			mv.visitVarInsn(Opcodes.ILOAD, java.lang.Integer.parseInt(varName.substring(Constants.BOOL_LOCALVAR_NAME_PREFIX.length())) + ctx.getCurMethodParamCount())
		}
		else if (isMethodParam) 
		{
			mv.visitVarInsn(Opcodes.ILOAD, java.lang.Integer.parseInt(varName.substring(Constants.BOOL_MTHPARAM_NAME_PREFIX.length())))
		}
	}

	def generateStoreInstruction(varName : String, mv : MethodVisitor, ctx : AbstractionContext) =
	{
		// generates instruction for storing into variable (via istore, putfield or putstatic)

		val isStaticField = varName.startsWith(Constants.BOOL_STATICFIELD_NAME_PREFIX)
		val isObjectField = varName.startsWith(Constants.BOOL_OBJECTFIELD_NAME_PREFIX)
		val isLocalVariable = varName.startsWith(Constants.BOOL_LOCALVAR_NAME_PREFIX)
		val isMethodParam = varName.startsWith(Constants.BOOL_MTHPARAM_NAME_PREFIX)

		
		if (isStaticField) 
		{
			mv.visitFieldInsn(Opcodes.PUTSTATIC, ctx.getCurClassAbstractName().replace('.', '/'), varName, Type.BOOLEAN_TYPE.getDescriptor())
		}
		else if (isObjectField) 
		{
			// we do not need to load "this" for static fields
			/*
			// load 'this'
			mv.visitVarInsn(Opcodes.ALOAD, 0)
			
			// swap 'this' with the value to be stored
			mv.visitInsn(Opcodes.SWAP)
			*/
			
			mv.visitFieldInsn(Opcodes.PUTSTATIC, ctx.getCurClassAbstractName().replace('.', '/'), varName, Type.BOOLEAN_TYPE.getDescriptor())
		}
		else if (isLocalVariable) 
		{
			mv.visitVarInsn(Opcodes.ISTORE, java.lang.Integer.parseInt(varName.substring(Constants.BOOL_LOCALVAR_NAME_PREFIX.length())) + ctx.getCurMethodParamCount())
		}
		else if (isMethodParam) 
		{
			mv.visitVarInsn(Opcodes.ISTORE, java.lang.Integer.parseInt(varName.substring(Constants.BOOL_MTHPARAM_NAME_PREFIX.length())))
		}
	}	
	
	def createMethodDescriptor(mthParamCount : Int, hasReturnValue : Boolean) : String =
	{
		val paramTypesArray : Array[Type] = new Array[Type](mthParamCount)
		
		for (i <- 0 to (mthParamCount - 1)) paramTypesArray(i) = Type.BOOLEAN_TYPE
		
		val returnType : Type =
			if (hasReturnValue) Type.INT_TYPE
			else Type.VOID_TYPE
		
		val mthDesc : String = Type.getMethodDescriptor(returnType, paramTypesArray)
		
		return mthDesc
	}
	
	def createMainDescriptor() : String =
	{
		val paramTypesArray : Array[Type] = new Array[Type](1)
		
		paramTypesArray(0) = Type.getType("[Ljava/lang/String;")
		
		val returnType : Type = Type.VOID_TYPE
		
		val mthDesc : String = Type.getMethodDescriptor(returnType, paramTypesArray)
		
		return mthDesc
	}

	
	def generateEncodingOfReturnExpression(mv : MethodVisitor, ctx : AbstractionContext, retExpr : Expression) =
	{
		// generates byte code for encoding predicates over expression-to-be-returned into one integer

		// encode truth value of computed predicates about expression-to-be-returned into one integer and push that integer onto the stack as a return value
		// resulting integer value is computed this way: res = 0; if (pred1) res +=1; if (pred2) res +=2; ... ; if (predN) res += 2^(N-1)
		
		generateBeginAtomic(mv)

		mv.visitInsn(Opcodes.ICONST_0)

		var coef : Int = 1
		
		
		// process all predicates about expression-to-be-returned that we must consider
		
		var generatedRetExprPredList = List[AtomicPredicate]()
		
		var resultPredSet = ctx.getResultPredicatesForMethod(ctx.getCurClassOrigName(), ctx.getCurMethodName())
		
		if (Main.DEBUG) FormulaUtils.printAtomPredSet(resultPredSet, "[DEBUG GenUtils.generateEncodingOfReturnExpression] predicates over result variables for method '" + ctx.getCurClassOrigName() + "." + ctx.getCurMethodName() + "':")
		
		for (resPred <- resultPredSet)
		{
			// replace the mark for result variable (string "[result]") with the actual returned expression in the original callee			
			generatedRetExprPredList = generatedRetExprPredList :+ FormulaUtils.copyWithReplace(resPred, Constants.RESULT_VAR_MARK, retExpr.toString())
		}

		// the variable-to-be-returned is always live
		var availableRetExprPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), retExpr.toString(), -1)
		
		// compute truth values of generated predicates over the returned expression based on existing predicates over the returned expression (mimic assignment statement)		
		for (genPred <- generatedRetExprPredList)
		{
			Configuration.assignAbstr.generateVariableLoad(ctx, mv, retExpr, genPred, retExpr, availableRetExprPredSet)
			
			mv.visitLdcInsn(coef)
			
			mv.visitInsn(Opcodes.IMUL)
			mv.visitInsn(Opcodes.IADD)

			coef *= 2
		}
			
		
		// get all predicates about output parameters that we must consider
		
		var outputParam2PredSet = ctx.getOutputParamPredicates(ctx.getCurClassOrigName(), ctx.getCurMethodName())
		
		for ( (outputParamIdx, outputPredSet) <- outputParam2PredSet )
		{
			val outputParamName = Constants.MTHPARAM_PREFIX + String.valueOf(outputParamIdx)
			
			val outputParamExpr = ExpressionUtils.createExprFromStr(outputParamName)
			
			if (Main.DEBUG) FormulaUtils.printAtomPredSet(outputPredSet, "[DEBUG GenUtils.generateEncodingOfReturnExpression] predicates over the output parameter " + outputParamIdx + " for method '" + ctx.getCurClassOrigName() + "." + ctx.getCurMethodName() + "':")

			// find predicates about this output parameter
			
			var generatedPredList = List[AtomicPredicate]()
			
			for (outPred <- outputPredSet)
			{				
				// replace the mark for output parameter (string "[output]") with the formal parameter name in the original callee			
				generatedPredList = generatedPredList :+ FormulaUtils.copyWithReplace(outPred, Constants.OUTPUT_PARAM_MARK, Constants.MTHPARAM_PREFIX + outputParamIdx)
			}
		
			// each output parameter is always live
			var availablePredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), outputParamName, -1)
		
			// compute truth values of generated predicates over the output parameter based on existing predicates over the output parameter (mimic assignment statement)		
			for (genPred <- generatedPredList)
			{
				Configuration.assignAbstr.generateVariableLoad(ctx, mv, outputParamExpr, genPred, outputParamExpr, availablePredSet)
				
				mv.visitLdcInsn(coef)
				
				mv.visitInsn(Opcodes.IMUL)
				mv.visitInsn(Opcodes.IADD)
				
				coef *= 2
			}
		}
				
		generateEndAtomic(mv)
	}
	
	def generateDecodingOfReturnedValues(mv : MethodVisitor, ctx : AbstractionContext, tgtVarExpr : Expression, calledClassName : String, calledMethodName : String, bcIndex : Int) =
	{
		// generates byte code for decoding returned integer value and setting predicates over target variable expression

		generateBeginAtomic(mv)

		var resultPredSet = ctx.getResultPredicatesForMethod(calledClassName, calledMethodName)
		
		if (Main.DEBUG) FormulaUtils.printAtomPredSet(resultPredSet, "[DEBUG GenUtils.generateDecodingOfReturnedValues] predicates over result variables for method '" + calledClassName + "." + calledMethodName + "':")
		
		// decode truth value of predicates about target variable from one integer

		var coef : Int = 1
		for (resPred <- resultPredSet)
		{
			val tgtVarPred = FormulaUtils.copyWithReplace(resPred, Constants.RESULT_VAR_MARK, tgtVarExpr.toString())
			
			mv.visitInsn(Opcodes.DUP)
			
			mv.visitLdcInsn(coef)
			
			mv.visitInsn(Opcodes.IAND)
			
			
			// if the current predicate does not exist (or if it is not live) then do not save the decoded value into any variable and just pop it
			
			if (Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtVarPred, bcIndex))
			{
				val predVarName = ctx.getVariableForPredicate(tgtVarPred).get
				
				generateStoreInstruction(predVarName, mv, ctx)
			}
			else
			{
				mv.visitInsn(Opcodes.POP)
			}
			
			coef *= 2
		}
		
		// pop that single integer from the stack at the end
		mv.visitInsn(Opcodes.POP)

		
		generateEndAtomic(mv)
	}
	
	def generateDecodingOfOutputParameterValues(mv : MethodVisitor, ctx : AbstractionContext, calledClassName : String, calledMethodName : String, bcIndex : Int, actualParamIdx2Value : Map[Int, Expression]) =
	{
		// generates byte code for decoding returned integer value and setting predicates over actual output parameters

		generateBeginAtomic(mv)

		var coef : Int = 1
		
		var resultPredSet = ctx.getResultPredicatesForMethod(calledClassName, calledMethodName)
		
		// skip predicates over returned expression
		for (resPred <- resultPredSet) coef *= 2
		
		
		var outputParam2PredSet = ctx.getOutputParamPredicates(calledClassName, calledMethodName)
		
		// decode truth value of predicates over output parameter from one integer
		
		for ( (outputParamIdx, outputPredSet) <- outputParam2PredSet )
		{
			val actualParamValue = actualParamIdx2Value.getOrElse(outputParamIdx, "")
			
			if (Main.DEBUG) FormulaUtils.printAtomPredSet(outputPredSet, "[DEBUG GenUtils.generateDecodingOfOutputParameterValues] predicates over the output parameter " + outputParamIdx + " for method '" + calledClassName + "." + calledMethodName + "':")

			for (outPred <- outputPredSet)
			{
				val actualParamPred = FormulaUtils.copyWithReplace(outPred, Constants.OUTPUT_PARAM_MARK, actualParamValue.toString())
				
				mv.visitInsn(Opcodes.DUP)
				
				mv.visitLdcInsn(coef)
				
				mv.visitInsn(Opcodes.IAND)
				
				
				// if the current predicate does not exist then do not save the decoded value into any variable and just pop it
				
				if (Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), actualParamPred, bcIndex))
				{
					val predVarName = ctx.getVariableForPredicate(actualParamPred).get
					
					generateStoreInstruction(predVarName, mv, ctx)
				}
				else
				{
					mv.visitInsn(Opcodes.POP)
				}
				
				coef *= 2
			}
		}
		
		// we keep the single integer at the stack (there is no POP instruction here)

		generateEndAtomic(mv)
	}
	
	def generateNondetValues(mv : MethodVisitor, ctx : AbstractionContext, tgtVarExpr : Expression, bcIndex : Int) : Unit =
	{
		generateNondetValues(mv, ctx, tgtVarExpr.toString(), bcIndex)
	}
	
	def generateNondetValues(mv : MethodVisitor, ctx : AbstractionContext, tgtVarExprStr : String, bcIndex : Int) : Unit =
	{
		generateBeginAtomic(mv)
		
		// set non-deterministic values to boolean variables representing predicates over the target variable 

		// find all predicates involving the target variable
		var tgtVarPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtVarExprStr, bcIndex)

		for (tgtPred <- tgtVarPredSet)
		{
			val tgtPredBoolVar = ctx.getVariableForPredicate(tgtPred).get

			// store non-deterministic value into the variable
			
			generateChooseBool(mv)
			
			GenUtils.generateStoreInstruction(tgtPredBoolVar, mv, ctx)
		}
		
		generateEndAtomic(mv)
	}

	def generateBeginAtomic(mv : MethodVisitor) =
	{
		if (Configuration.multipleThreads)
		{
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, VERIFY_CLASS.replace('.', '/'), "beginAtomic", Type.getMethodDescriptor(Type.VOID_TYPE, new Array[Type](0)))
		}
	}

	def generateEndAtomic(mv : MethodVisitor) =
	{
		if (Configuration.multipleThreads)
		{
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, VERIFY_CLASS.replace('.', '/'), "endAtomic", Type.getMethodDescriptor(Type.VOID_TYPE, new Array[Type](0)))
		}
	}

	def generateChooseBool(mv : MethodVisitor) =
	{
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, VERIFY_CLASS.replace('.', '/'), "getBoolean", Type.getMethodDescriptor(Type.BOOLEAN_TYPE, new Array[Type](0))) 
	}
	
	def generateChooseInt(mv : MethodVisitor, min : Int, max : Int)
	{
		mv.visitLdcInsn(min)
		mv.visitLdcInsn(max)
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, VERIFY_CLASS.replace('.', '/'), "getInt", Type.getMethodDescriptor(Type.INT_TYPE, Array(Type.INT_TYPE, Type.INT_TYPE)))
	}
	
	def generateForcedBacktrack(mv : MethodVisitor) =
	{
		mv.visitInsn(Opcodes.ICONST_1)
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, VERIFY_CLASS.replace('.', '/'), "ignoreIf", Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.BOOLEAN_TYPE)))
	}
	
	def generateAssertTrue(mv : MethodVisitor, msg : String) =
	{
		mv.visitLdcInsn("[ASSERT] " + msg)
		mv.visitInsn(Opcodes.ICONST_1)
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, VERIFY_CLASS.replace('.', '/'), "assertTrue", Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType("Ljava/lang/String;"), Type.BOOLEAN_TYPE)))
	}
	
	def generateAssertFalse(mv : MethodVisitor, msg : String) =
	{
		mv.visitLdcInsn("[ERROR (ASSERT VIOLATION)] " + msg)
		mv.visitInsn(Opcodes.ICONST_0)
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, VERIFY_CLASS.replace('.', '/'), "assertTrue", Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType("Ljava/lang/String;"), Type.BOOLEAN_TYPE)))
	}
	
	val VERIFY_CLASS : String = "gov.nasa.jpf.jvm.Verify"

}

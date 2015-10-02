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

import java.io.File
import java.io.FileOutputStream

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import scala.io.Source._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod}
import com.ibm.wala.types.TypeReference

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Type
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Label
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.analysis.Analyzer
import org.objectweb.asm.tree.analysis.BasicVerifier

import common.Constants
import common.LogicFormula
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.Negation
import common.Expression
import common.ExpressionUtils
import common.FormulaUtils

import util.StringUtils


object JavaCodeAbstractor extends ExecutionVisitor with StatementResolver
{
	def generateAbstractionForClass(ctx : AbstractionContext, clsName : String, outputDirName : String) =
	{
		if (Main.INFO) println("[INFO] generating abstraction for class: " + clsName);
		
		ctx.initCurClass(clsName)
		
		ctx.setCurClassAbstractName(clsName + ctx.getAbsClassNameSuffix())
		
		
		val cls = WALAUtils.findClass(clsName)
		
		
		val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)

		// write class header
		cw.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, ctx.getCurClassAbstractName().replace('.', '/'), null, "java/lang/Object", null)

		
		// generate boolean fields representing predicates over static fields and instance fields of the original class
		generateBooleanFields(cw, ctx);
		
		
		// generate method abstractions
		for (mth <- cls.getDeclaredMethods()) 
		{
			ctx.initCurMethod(WALAUtils.getShortMethodName(mth), mth.isStatic())
			
			generateAbstractionForMethod(mth.asInstanceOf[IBytecodeMethod], cls.isInterface || mth.isAbstract(), cw, ctx)
		}
		
		
		// save generated class to a file in the output directory
		
		cw.visitEnd()
		
		val clsBytes : Array[Byte] = cw.toByteArray()
		
		val clsDir = new File(outputDirName + "/" + StringUtils.extractPackageName(clsName).replace('.', '/'))
		clsDir.mkdirs()
		
		val outf : FileOutputStream = new FileOutputStream(outputDirName + "/" + ctx.getCurClassAbstractName().replace('.', '/') + ".class")
		outf.write(clsBytes)
		outf.close()
	}
	
	private def generateBooleanFields(cw : ClassWriter, ctx : AbstractionContext) =
	{
		var fieldIndex = 1

		for ( pred <- Configuration.predicatesMngr.getPredicatesOverStaticFields(ctx.getCurClassOrigName()) )
		{
			val fldName : String = Constants.BOOL_STATICFIELD_NAME_PREFIX + fieldIndex
			
			println("[J2BP] class = '" + ctx.getCurClassOrigName() + "', static field = '" + fldName + "', predicate = '" + pred + "'");
			
			ctx.storeFieldForPredicate(pred, fldName)
			
			val fv : FieldVisitor = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, fldName, Type.BOOLEAN_TYPE.getDescriptor(), null, null)
			fv.visitEnd()
			
			fieldIndex += 1
		}

		for ( pred <- Configuration.predicatesMngr.getPredicatesOverObjectFields(ctx.getCurClassOrigName()) )
		{
			val fldName : String = Constants.BOOL_OBJECTFIELD_NAME_PREFIX + fieldIndex
			
			println("[J2BP] class = '" + ctx.getCurClassOrigName() + "', object field = '" + fldName + "', predicate = '" + pred + "'");
			
			ctx.storeFieldForPredicate(pred, fldName)
			
			val fv : FieldVisitor = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, fldName, Type.BOOLEAN_TYPE.getDescriptor(), null, null)
			fv.visitEnd()
			
			fieldIndex += 1			
		}
	}
	
	private def generateAbstractionForMethod(mth : IBytecodeMethod, isAbstract : Boolean, cw : ClassWriter, ctx : AbstractionContext) =
	{
		if (Main.INFO) println("[INFO] generating abstraction for method: " + ctx.getCurMethodName());

		val mthParamCount = ctx.getCurMethodParamCount()
		
		// generate names of local variables (including parameters)
		generateLocalVariableNames(ctx)		
		
		
		// generate method header				

		if (Main.INFO) println("[INFO] method param count = " + mthParamCount + ", return value = " + ctx.hasCurMethodAbsReturnValue())

		var mthDesc = ""
		
		if (ctx.getCurMethodName() == "main") mthDesc = GenUtils.createMainDescriptor()
		else mthDesc = GenUtils.createMethodDescriptor(mthParamCount, ctx.hasCurMethodAbsReturnValue())
        
		var mthOpcodes = Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC
		if (mth.isSynchronized()) mthOpcodes += Opcodes.ACC_SYNCHRONIZED
		
		var mthName = ctx.getCurMethodName()
		if (mthName == "<init>") mthName = "init0"
		
		val mv = cw.visitMethod(mthOpcodes, mthName, mthDesc, null, null)
		
		mv.visitCode()

		
		// generate method byte code
		
		if (isAbstract)
		{
			// make sure that abstract methods or interface methods are not empty in the generated program

			mv.visitInsn(Opcodes.NOP)
			
			if (ctx.hasCurMethodAbsReturnValue()) 
			{
				mv.visitInsn(Opcodes.ICONST_0)
				mv.visitInsn(Opcodes.IRETURN)
			}
			else
			{
				mv.visitInsn(Opcodes.RETURN)
			}
		}
				
		// we do not need to call the constructor of java.lang.Object for classes that have only static fields
		/*
		if (ctx.getCurMethodName() == "<init>")
		{
			mv.visitVarInsn(Opcodes.ALOAD, 0)
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
		}
		*/
		
		// create list of possible jump targets (labels)		
		JavaCodeUtils.findAllJumpTargets(mth, ctx)
		
		
		// initialize all local variables to the value "false"
		initializeLocalVariables(mv, ctx)
		
		
		// loop through all Shrike bytecode instructions and process each relevant one (some are ignored)
		
		ctx.setOrigLocalVarNames("")
		
		ExecutionSimulator.simulateMethod(mth, isAbstract, ctx, this, mv) 

		
		var mthVarCount = mthParamCount + ctx.getLocalVariableCount()
		
		// we do not need that because all generated methods are static
		// if ( ! ctx.isCurMethodStatic() ) mthVarCount += 1
        
		if (Main.DEBUG) 
		{
			println("[DEBUG JavaCodeAbstractor.generateAbstractionForMethod] param count = " + mthParamCount + ", local variable count = " + ctx.getLocalVariableCount())
			println("[DEBUG JavaCodeAbstractor.generateAbstractionForMethod] max stack depth = " + ctx.getMaxExprStackSize() + ", variable count = " + mthVarCount)
		}

		mv.visitMaxs(6, mthVarCount + 1)
		
		mv.visitEnd()
	}
	
	override def visitPreInstruction(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		val newLocalVarNames = WALAUtils.printLocalVariableNames(ctx.getCurClassOrigName(), ctx.getCurMethodName(), insnIndex, ctx.getMonitorVarCount())
			
		if (ctx.getOrigLocalVarNames() != newLocalVarNames)
		{
			println("[J2BP] method = '" + ctx.getCurClassOrigName() + "." + ctx.getCurMethodName() + "', bytecode index = " + insnIndex + ", local variable names = '" + newLocalVarNames + "'")
				
			ctx.setOrigLocalVarNames(newLocalVarNames)
		}
		
		// this instruction is a possible jump target
		val lblOpt : Option[Label] = ctx.getLabelForInsn(insnIndex)
		if (lblOpt != None) 
		{
			mv.visitLabel(lblOpt.get)
			
			if (Main.DEBUG) println("[DEBUG JavaCodeAbstractor.visitPreInstruction] new label for instruction with index = " + insnIndex)
		}
	}
	
	override def visitPostInstruction(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object) =
	{
		val mv = arg.asInstanceOf[MethodVisitor] 
		
		// generate code for checking properties associated with the instruction
		
		val propertyFormulas = Configuration.propertiesMngr.getPropertiesForCodeLocation(ctx.getCurrentProperties(), ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), insnIndex)
		
		if (Main.DEBUG) 
		{
			FormulaUtils.printFormList(propertyFormulas, "[DEBUG JavaCodeAbstractor.visitPostInstruction] available properties for code location '" + ctx.getCurClassOrigName() + "." + ctx.getCurMethodName() + ":" + insnIndex + "'")
		}

		for (propFormula <- propertyFormulas)
		{
			if (Main.INFO) println("[INFO] generating property: formula = '" + propFormula.toString() + "', code location = " + ctx.getCurClassOrigName() + "." + ctx.getCurMethodName() + ":" + insnIndex)			

			// generate code that computes truth value of the property formula based on atomic predicates
			// we use the artificial predicate "prop = 1"
			// code assigns the correct value into the boolean field that represents given property formula
			
						
			// get predicates that will determine truth value of the property formula
			
			var propValDetPreds = Set[AtomicPredicate]()
			
			val availablePredicates = Configuration.predicatesMngr.getAllPredicatesForCodeLocation(ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), insnIndex)
	
			// get all predicates that have a common operand with some predicate in the property formula (but ignore constants)
			for (pfAtomPred <- FormulaUtils.extractAtomicPredicates(propFormula))
			{
				val pfOperands = FormulaUtils.extractOperands(pfAtomPred)
				
				for (availPred <- availablePredicates)
				{
					for (availOperand <- FormulaUtils.extractOperands(availPred))
					{
						if ( ( ! ExpressionUtils.isConstantValue(availOperand) ) && ( ! Configuration.predicatesMngr.isReservedName(availOperand.toString()) ) )
						{
							if (pfOperands.contains(availOperand)) propValDetPreds = propValDetPreds + availPred
						}
					}
				}
			}

			// get equality predicates over function arguments
			for (pfAtomPred <- FormulaUtils.extractAtomicPredicates(propFormula))
			{
				for (predFuncExpr <- FormulaUtils.extractFunctionExpressionsRecursively(pfAtomPred))
				{
					for (i <- 1 to (predFuncExpr.args.length - 1))
					{
						for ( argPred <- Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), predFuncExpr.args(i).toString(), -1) )
						{
							if (argPred.containsOperand(predFuncExpr.args(i)) && (argPred.getOperator() == "=")) propValDetPreds = propValDetPreds + argPred
						}
					}
				}
			}


			if (Main.DEBUG) FormulaUtils.printAtomPredSet(propValDetPreds, "[DEBUG JavaCodeAbstractor.visitPostInstruction] predicates that determine property value:")
			
			val propVarPred2PosWP : Map[AtomicPredicate, LogicFormula] = new HashMap
			val propVarPred2NegWP : Map[AtomicPredicate, LogicFormula] = new HashMap

			propVarPred2PosWP.put(Constants.PROP_HOLDS_PRED, propFormula)
			propVarPred2NegWP.put(Constants.PROP_HOLDS_PRED, new Negation(propFormula))

			// load truth value of the property formula on the stack			
			resolve(ctx, mv, null, Set[AtomicPredicate](Constants.PROP_HOLDS_PRED), propValDetPreds.toSet, propVarPred2PosWP, propVarPred2NegWP, false)

			val propHoldsLabel = new Label()
			
			// load integer constant 1 (program must jump when the condition holds)
			mv.visitInsn(Opcodes.ICONST_1)
					
			// if_icmpeq instruction with proper jump target
			mv.visitJumpInsn(Opcodes.IF_ICMPEQ, propHoldsLabel)
			
			GenUtils.generateAssertFalse(mv, "not (" + propFormula.toString() + ")")
			
			mv.visitLabel(propHoldsLabel)
		}
		
		// generate forced backtracking if associated with the instruction		
		if (ctx.isForcedBacktrackLocation(ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), insnIndex))
		{
			if (Main.DEBUG) println("[DEBUG JavaCodeAbstractor.visitPostInstruction] generating forced backtrack at code location '" + ctx.getCurClassOrigName() + "." + ctx.getCurMethodName() + ":" + insnIndex + "'")
		
			GenUtils.generateForcedBacktrack(mv)
		}
	}
	
	override def visitArrayStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, arrayExpr : Expression, index : Expression, newValue : Expression, elementType : String) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		val arrayAccessExpr = ExpressionUtils.createArrayAccessExpr(arrayExpr, index)
			
		// dummy value is ignored here

		if (newValue.toString() == Constants.STACK_ELEM_RETVAL_DET)
		{
			// method call result is on the stack
			
			// generate decoding integer value returned from method call into multiple boolean variables
			GenUtils.generateDecodingOfReturnedValues(mv, ctx, arrayAccessExpr, ctx.getLastInvokeClass(), ctx.getLastInvokeMethod(), insnIndex)
		}
		else if (newValue.toString() == Constants.STACK_ELEM_RETVAL_NDT)
		{
			// non-deterministic method call result is on the stack

			GenUtils.generateNondetValues(mv, ctx, arrayAccessExpr, insnIndex)
		}
		else if (newValue.toString() != Constants.STACK_ELEM_DUMMY)
		{
			// update values of boolean variables that represent predicates over "arrayname[index]" based on the "value" and predicates over "value"
			Configuration.assignAbstr.generateAbstractStore(ctx, mv, arrayAccessExpr, newValue, insnIndex, elementType)
		}	
	}
	
	override def visitConditionalBranchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, posOpStr : String, negOpStr : String, value1 : Expression, value2 : Expression, target : Int, backjumpInsnPos : Int) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		val posPred = new BinaryPredicate(posOpStr, value1, value2)
		val negPred = new BinaryPredicate(negOpStr, value1, value2)
		
		if (Main.DEBUG) println("[DEBUG JavaCodeAbstractor.visitConditionalBranchInsn] positive predicate = '" + posPred + "', negative predicate = '" + negPred)
		
		// check whether the predicate (or its negation) is defined
		val posPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), posPred, -1)
		val negPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), negPred, -1)
		
		if (Main.DEBUG) 
		{
			println("[DEBUG JavaCodeAbstractor.visitConditionalBranchInsn] positive predicate exists = '" + posPredExists + "', negative predicate exists = '" + negPredExists + "'")
		}				
		
		// find boolean variable (local or field) for the predicate (or its negation)
		var predVarName = ""
		if (posPredExists) predVarName = ctx.getVariableForPredicate(posPred).get
		if (negPredExists) predVarName = ctx.getVariableForPredicate(negPred).get
		
		if (Main.DEBUG) println("[DEBUG JavaCodeAbstractor.visitConditionalBranchInsn] variable name = " + predVarName)

		if (Main.INFO)
		{
			if (predVarName == "") println("[INFO] condition predicates do not exist: pos = '" + posPred.toString() + "', neg = '" + negPred.toString() + "'")
		}
		
		// generate byte code
		
		if (predVarName != "")
		{
			// load the local variable or field
			GenUtils.generateLoadInstruction(predVarName, mv, ctx)
		}
		else
		{
			GenUtils.generateChooseBool(mv)
		}

		// load integer constant 1 (for original predicate) or 0 (for negated predicate)
			// why: program must jump when the condition holds (this is how Java programs are compiled)
			// default value is 1 (both branches are taken anyway if the predicate does not exist
		if (posPredExists) mv.visitInsn(Opcodes.ICONST_1)
		else if (negPredExists) mv.visitInsn(Opcodes.ICONST_0)
		else mv.visitInsn(Opcodes.ICONST_1)

		// if_icmpeq instruction with proper jump target
		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, ctx.getLabelForInsn(target).get)
	}

	override def visitGotoInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, targetIndex : Int) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		// generate goto instruction
		mv.visitJumpInsn(Opcodes.GOTO, ctx.getLabelForInsn(targetIndex).get)
	}
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]

		ctx.setLastInvokeInfo(ownerClassName, tgtMethodName)
		
		if (tgtMethodName == "wait")
		{
			// remove "tgtObjExpr" from the top of the stack
			val tgtObjExpr = ctx.removeExprFromStack()
		
			// call 'BooleanLockManager.wait(tgtObjExpr)'
			mv.visitLdcInsn(tgtObjExpr.toString())
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, Constants.LOCKMNGR_CLASS.replace('.', '/'), Constants.LOCKMNGR_WAIT, Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[java.lang.String]))))
		}
		else if (tgtMethodName == "notify")
		{
			// remove "tgtObjExpr" from the top of the stack
			val tgtObjExpr = ctx.removeExprFromStack()
		
			// call 'BooleanLockManager.notify(tgtObjExpr)'
			mv.visitLdcInsn(tgtObjExpr)
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, Constants.LOCKMNGR_CLASS.replace('.', '/'), Constants.LOCKMNGR_NOTIFY, Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[java.lang.String]))))
		}
		else
		{
			if (ctx.isProgramClass(ownerClassName))
			{
				// find all methods that can be invoked here at program runtime			
				// we consider subclasses of the 'ownerClassName' retrieved from bytecode and method signatures
				
				val tgtMethodDescr = tgtMethodSig.substring(tgtMethodSig.indexOf('('))
				
				var dynTargetClasses = List[String]()
				
				val ownerClass = WALAUtils.findClass(ownerClassName)
				if ( ! ownerClass.isInterface ) dynTargetClasses = dynTargetClasses :+ ownerClassName
				
				if ( ! isStaticCall ) 
				{
					for ( subClassName <- WALAUtils.findSubclassesWithMethod(ownerClassName, tgtMethodName, tgtMethodDescr, ctx.getProgramClasses()) )
					{
						// avoid recursive calls of the current method, when the owner class is an interface that is implemented by the currently processed class
						if ( ! ((ctx.getCurMethodName() == tgtMethodName) && (subClassName == ctx.getCurClassOrigName())) ) 
						{
							dynTargetClasses = dynTargetClasses :+ subClassName
						}
					}
				}
		
				// generate abstraction for method call
				Configuration.mthcallAbstr.generateInternalCall(ctx, mv, ownerClassName, tgtMethodName, isStaticCall, dynTargetClasses, insnIndex)
			}
			else
			{
				Configuration.mthcallAbstr.generateLibraryCall(ctx, mv, ownerClassName, tgtMethodName, isStaticCall, tgtMethodParamCount, retVarExpr, retVarType, insnIndex)
			}

			if (ctx.getTempReturnVariableName() != "")
			{
				// remove method call result from the top of the stack
				val resultExpr = ctx.removeExprFromStack()
			
				if (resultExpr.toString() == Constants.STACK_ELEM_RETVAL_DET)
				{
					// deterministic method call result is on the stack
			
					// generate decoding of integer value returned from method call into multiple boolean variables
					GenUtils.generateDecodingOfReturnedValues(mv, ctx, new Expression(ctx.getTempReturnVariableName()), ctx.getLastInvokeClass(), ctx.getLastInvokeMethod(), insnIndex)
				}
				else if (resultExpr.toString() == Constants.STACK_ELEM_RETVAL_NDT)
				{
					// non-deterministic method call result is on the stack
			
					GenUtils.generateNondetValues(mv, ctx, ctx.getTempReturnVariableName(), insnIndex)
				}

				// store implicit variable used to store result on stack for further processing
				// this applies only to internal method calls
				ctx.addExprToStack(new Expression(ctx.getTempReturnVariableName()))
			}
		}		
	}
	
	override def visitMonitorInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, insnOpcode : Int) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		var tgtObjExpr : Expression = null
		
		if (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_monitorenter)
		{
			// remove "tgtObjExpr" from the top of the stack
			tgtObjExpr = ctx.removeExprFromStack()
		}
		else if (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_monitorexit)
		{
			// use previously stored variable name
			tgtObjExpr = ctx.popLockVariableExpr()	
		}


		GenUtils.generateBeginAtomic(mv)
		
		// find all predicates involving the target variable name
		var tgtVarPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), tgtObjExpr.toString(), -1)
		
		
		// create list that includes 'tgtObjExpr' and the variable name 'w' for each predicate 'tgtObjExpr = w'
		
		var lockExpressions = List[Expression]()

		lockExpressions = lockExpressions :+ tgtObjExpr				

		for (tgtVarPred <- tgtVarPredSet if (tgtVarPred.isInstanceOf[BinaryPredicate] && FormulaUtils.isAliasingPredicate(tgtVarPred.asInstanceOf[BinaryPredicate])))
		{
			val aliasTgtVarPred = tgtVarPred.asInstanceOf[BinaryPredicate]
								
			if ((aliasTgtVarPred.left == tgtObjExpr) && ExpressionUtils.isVariableName(aliasTgtVarPred.right)) 
			{
				lockExpressions = lockExpressions :+ aliasTgtVarPred.right
			}
			else if ((aliasTgtVarPred.right == tgtObjExpr) && ExpressionUtils.isVariableName(aliasTgtVarPred.left)) 
			{
				lockExpressions = lockExpressions :+ aliasTgtVarPred.left
			}
		}
		
		
		// perform actual locking or unlocking
		
		if (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_monitorenter)
		{
			ctx.pushLockVariableExpr(tgtObjExpr)
			

			val lockStatusCheckBlock = new Label()
			
			mv.visitLabel(lockStatusCheckBlock)
			
			
			// check availability of the lock
			
			mv.visitInsn(Opcodes.ICONST_0)
			
			for (lockExpr <- lockExpressions)
			{
				// call 'BooleanLockManager.isLocked(lockVarName)'
				mv.visitLdcInsn(lockExpr)
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, Constants.LOCKMNGR_CLASS.replace('.', '/'), Constants.LOCKMNGR_IS_LOCKED, Type.getMethodDescriptor(Type.BOOLEAN_TYPE, Array(Type.getType(classOf[java.lang.String]))))
				
				if (lockExpr != tgtObjExpr)
				{
					// find boolean variable for the predicate 'tgtObjExpr = lockExpr'
					
					val lockPred1 = new BinaryPredicate("=", tgtObjExpr, lockExpr)
					val lockPred2 = new BinaryPredicate("=", lockExpr, tgtObjExpr)
					
					var lockPredBoolVar = ""
					
					if (Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), lockPred1, -1)) lockPredBoolVar = ctx.getVariableForPredicate(lockPred1).get
					else lockPredBoolVar = ctx.getVariableForPredicate(lockPred2).get
				
					// change lock status to zero if the predicate does not hold
					GenUtils.generateLoadInstruction(lockPredBoolVar, mv, ctx)
					mv.visitInsn(Opcodes.IMUL)
				}
				
				// add result of the method call and previous stack top
				mv.visitInsn(Opcodes.IADD)
			}

			
			// if the overall result is zero, then the lock is available
			
			val lockPossibleBranch = new Label()
			
			mv.visitInsn(Opcodes.ICONST_0)
			mv.visitJumpInsn(Opcodes.IF_ICMPEQ, lockPossibleBranch)

			// call 'BooleanLockManager.waitForLock(tgtObjExpr)'
			mv.visitLdcInsn(tgtObjExpr)
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, Constants.LOCKMNGR_CLASS.replace('.', '/'), Constants.LOCKMNGR_WAIT_FOR_LOCK, Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[java.lang.String]))))
			
			// jump to the beginning of lock status checking block
			mv.visitJumpInsn(Opcodes.GOTO, lockStatusCheckBlock)
			
			
			mv.visitLabel(lockPossibleBranch)
			
			// call 'BooleanLockManager.lock(tgtObjExpr)'
			mv.visitLdcInsn(tgtObjExpr)
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, Constants.LOCKMNGR_CLASS.replace('.', '/'), Constants.LOCKMNGR_LOCK, Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[java.lang.String]))))					
		}
		
		if (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_monitorexit)
		{
			for (lockExpr <- lockExpressions)
			{
				if (lockExpr != tgtObjExpr)
				{
					val unlockBranchEnd = new Label()
			
					// find boolean variable for the predicate 'tgtObjExpr = lockExpr'
					
					val lockPred1 = new BinaryPredicate("=", tgtObjExpr, lockExpr)
					val lockPred2 = new BinaryPredicate("=", lockExpr, tgtObjExpr)
					
					var lockPredBoolVar = ""
					
					if (Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), lockPred1, -1)) lockPredBoolVar = ctx.getVariableForPredicate(lockPred1).get
					else lockPredBoolVar = ctx.getVariableForPredicate(lockPred2).get					

					// no unlock if the predicate does not hold
					GenUtils.generateLoadInstruction(lockPredBoolVar, mv, ctx)
					mv.visitInsn(Opcodes.ICONST_0)
					mv.visitJumpInsn(Opcodes.IF_ICMPEQ, unlockBranchEnd)

					// call 'BooleanLockManager.unlock(lockExpr)'
					mv.visitLdcInsn(lockExpr)
					mv.visitMethodInsn(Opcodes.INVOKESTATIC, Constants.LOCKMNGR_CLASS.replace('.', '/'), Constants.LOCKMNGR_UNLOCK, Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[java.lang.String]))))
					
					mv.visitLabel(unlockBranchEnd)
				}
				else
				{
					// call 'BooleanLockManager.unlock(tgtObjExpr)'
					mv.visitLdcInsn(tgtObjExpr)
					mv.visitMethodInsn(Opcodes.INVOKESTATIC, Constants.LOCKMNGR_CLASS.replace('.', '/'), Constants.LOCKMNGR_UNLOCK, Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[java.lang.String]))))
				}
			}
		}
							
		GenUtils.generateEndAtomic(mv)
	}
	
	override def visitPutInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtObj : Expression, fieldName : String, fieldType : String, newValue : Expression) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		val tgtFieldExpr = ExpressionUtils.createFieldAccessExpr(tgtObj, fieldName)
		
		// dummy value is ignored here

		if (newValue.toString() == Constants.STACK_ELEM_RETVAL_DET)
		{
			// method call result is on the stack
		
			// generate decoding integer value returned from method call into multiple boolean variables
			GenUtils.generateDecodingOfReturnedValues(mv, ctx, tgtFieldExpr, ctx.getLastInvokeClass(), ctx.getLastInvokeMethod(), insnIndex)
		}
		else if (newValue.toString() == Constants.STACK_ELEM_RETVAL_NDT)
		{
			// non-deterministic method call result is on the stack
				
			GenUtils.generateNondetValues(mv, ctx, tgtFieldExpr, insnIndex)
		}
		else if (newValue.toString() != Constants.STACK_ELEM_DUMMY)
		{
			// update values of boolean variables that represent predicates over "varname.fieldname" based on the "value" and predicates over "value"
			Configuration.assignAbstr.generateAbstractPutfield(ctx, mv, tgtObj, fieldName, newValue, insnIndex, fieldType)
		}	
	}
	
	override def visitReturnInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, retExpr : Expression) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]

		// check whether abstract method returns something
		if ( ctx.hasCurMethodAbsReturnValue() )
		{					
			// encode returned expression to single integer
			GenUtils.generateEncodingOfReturnExpression(mv, ctx, retExpr)
			
			mv.visitInsn(Opcodes.IRETURN)		
		}
		else
		{
			mv.visitInsn(Opcodes.RETURN)
		}	
	}
	
	override def visitStoreInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, tgtVarExpr : Expression, tgtVarType : String, newValue : Expression) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		// dummy value is ignored here

		if (newValue.toString() == Constants.STACK_ELEM_RETVAL_DET)
		{
			// deterministic method call result is on the stack
		
			// generate decoding of integer value returned from method call into multiple boolean variables
			GenUtils.generateDecodingOfReturnedValues(mv, ctx, tgtVarExpr, ctx.getLastInvokeClass(), ctx.getLastInvokeMethod(), insnIndex)
		}
		else if (newValue.toString() == Constants.STACK_ELEM_RETVAL_NDT)
		{
			// non-deterministic method call result is on the stack
		
			GenUtils.generateNondetValues(mv, ctx, tgtVarExpr, insnIndex)
		}
		else if (newValue.toString() != Constants.STACK_ELEM_DUMMY)
		{
			// update values of boolean variables that represent predicates over "varname" based on the "value" and predicates over "value"
			Configuration.assignAbstr.generateAbstractStore(ctx, mv, tgtVarExpr, newValue, insnIndex, tgtVarType)
		}	
	}
	
	override def visitSwitchInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, testedValue : Expression, caseValues : List[Int], caseTargets : List[Int], defaultTarget : Int) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		for (i <- 0 to (caseValues.size() - 1))
		{
			val eqPred = new BinaryPredicate("=", testedValue, ExpressionUtils.createExprFromStr(caseValues(i).toString()))

			// check whether equality predicate exists
			val eqPredExists = Configuration.predicatesMngr.existsPredicate(ctx.getCurClassOrigName(), ctx.getCurMethodName(), eqPred, -1)
		
			if (eqPredExists)
			{
				// find boolean variable (local or field) for the equality predicate
				val predVarName = ctx.getVariableForPredicate(eqPred).get

				// generate byte code instructions
		
				// load the local variable or field
				GenUtils.generateLoadInstruction(predVarName, mv, ctx)
			}
			else
			{
				if (Main.INFO) println("[INFO] condition predicates do not exist: case = '" + eqPred.toString() + "'")

				GenUtils.generateChooseBool(mv)
			}
			
			// load integer constant 1 
			mv.visitInsn(Opcodes.ICONST_1)
				
			// if_icmpeq instruction with proper jump target
			mv.visitJumpInsn(Opcodes.IF_ICMPEQ, ctx.getLabelForInsn(caseTargets(i)).get)
		}
		
		// generate goto instruction for default branch
		mv.visitJumpInsn(Opcodes.GOTO, ctx.getLabelForInsn(defaultTarget).get)
	}
	
	override def visitThrowInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, excObj : Expression) =
	{
		val mv = arg.asInstanceOf[MethodVisitor]
		
		GenUtils.generateAssertFalse(mv, "exception")	
	}
	
	private def generateLocalVariableNames(ctx : AbstractionContext) =
	{
		var varIndex = 0
		
		// all generated methods are static
		/*
		if ( ! ctx.isCurMethodStatic() ) varIndex += 1
		*/
		
		// we must ignore the "String[] args" parameter
		if (ctx.getCurMethodName() == "main") varIndex += 1
		
		val paramPreds = Configuration.predicatesMngr.getPredicatesOverMethodParams(ctx.getCurClassOrigName(), ctx.getCurMethodName(), -1)
		
		for (pred <- paramPreds)
		{
			val varName : String = Constants.BOOL_MTHPARAM_NAME_PREFIX + varIndex
			
			println("[J2BP] method = '" + ctx.getCurClassOrigName() + "." + ctx.getCurMethodName() + "', parameter = '" + varName + "', predicate = '" + pred + "'");
						
			ctx.storeLocalVariableForPredicate(pred, varName)
			
			varIndex += 1
		}
		
		val locvarPreds = Configuration.predicatesMngr.getPredicatesOverMethodLocalVars(ctx.getCurClassOrigName(), ctx.getCurMethodName(), -1)
		
		for (pred <- locvarPreds)
		{
			val varName : String = Constants.BOOL_LOCALVAR_NAME_PREFIX + varIndex
			
			println("[J2BP] method = '" + ctx.getCurClassOrigName() + "." + ctx.getCurMethodName() + "', local variable = '" + varName + "', predicate = '" + pred + "'");
			
			ctx.storeLocalVariableForPredicate(pred, varName)
			
			varIndex += 1
		}
	}
	
	private def initializeLocalVariables(mv : MethodVisitor, ctx : AbstractionContext) =
	{	
		val locvarPreds = Configuration.predicatesMngr.getPredicatesOverMethodLocalVars(ctx.getCurClassOrigName(), ctx.getCurMethodName(), -1)
		
		for (pred <- locvarPreds)
		{
			val predVarName = ctx.getVariableForPredicate(pred).get
			
			if (Main.DEBUG) println("[DEBUG JavaCodeAbstractor.initializeLocalVariables] predicate = '" + pred + "', local variable name = " + predVarName)
			
			// we initialize all local variables to false at their declaration (method beginning)
			mv.visitInsn(Opcodes.ICONST_0)
									
			GenUtils.generateStoreInstruction(predVarName, mv, ctx)			
		}
	}	
	
	def findResultPredicates(ctx : AbstractionContext, progClasses : List[String]) =
	{
		for ( className <- progClasses )
		{				
			if (Main.INFO) println("[INFO] finding result predicates for class: " + className);
	
			ctx.initCurClass(className)

			val cls = WALAUtils.findClass(className)
			
			for (mth <- cls.getDeclaredMethods()) 
			{
				val methodName = WALAUtils.getShortMethodName(mth)
				
				if (Main.INFO) println("[INFO] finding result predicates for method: " + methodName);
					
				ctx.initCurMethod(methodName, mth.isStatic())
				
				JavaCodeUtils.findAllJumpTargets(mth.asInstanceOf[IBytecodeMethod], ctx)
				
				ExecutionSimulator.simulateMethod(mth.asInstanceOf[IBytecodeMethod], cls.isInterface || mth.isAbstract(), ctx, new ResultPredicatesCollector(), null)
			}
		}		
		
		if (Main.DEBUG)
		{
			for ( className <- progClasses )
			{				
				val cls = WALAUtils.findClass(className)
				
				for (mth <- cls.getDeclaredMethods()) 
				{
					val methodName = WALAUtils.getShortMethodName(mth)
					
					FormulaUtils.printAtomPredSet(ctx.getResultPredicatesForMethod(className, methodName), "[DEBUG JavaCodeAbstractor.findResultPredicates] result predicates for method '" + className + "." + methodName + "':")
				}
			}
		}
	}
	
	class ResultPredicatesCollector extends ExecutionVisitor
	{
		override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
		{
			val piRes = Configuration.mthcallAbstr.preprocessInvoke(ctx, ownerClassName, tgtMethodName, tgtMethodParamCount, isStaticCall)
			val actualParamArray = piRes._2
			val skip = piRes._3
		
			if (ctx.isProgramClass(ownerClassName))
			{
				// we have method defined in the program that has a return value
				if (WALAUtils.hasMethodReturnValue(ownerClassName, tgtMethodName))
				{
					// get all predicates over the variable used to store result
					var retVarPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), retVarExpr.toString(), -1)
	
					var resultPredSet = Set[AtomicPredicate]()
					
					// replace name of the result variable with "[result]" (Constants.RESULT_VAR_MARK) 
					// filter out predicates that refer to something else than constants and static fields
					for (rvPred <- retVarPredSet)
					{
						val rvPredWithMark = FormulaUtils.copyWithReplace(rvPred, retVarExpr.toString(), Constants.RESULT_VAR_MARK)
	
						if ( ! ( Configuration.predicatesMngr.isPredicateOverMethodVariables(rvPredWithMark) || Configuration.predicatesMngr.isPredicateOverObjectFields(rvPredWithMark) ) )
						{								
							resultPredSet = resultPredSet + rvPredWithMark
						}
					}
					
					// append predicates to the list recorded so far (to make union over all call sites)
					ctx.saveResultPredicatesForMethod(ownerClassName, tgtMethodName, ctx.getResultPredicatesForMethod(ownerClassName, tgtMethodName) ++ resultPredSet)
				}
				
				var paramStartIndex = 0
				if ( ! isStaticCall ) paramStartIndex = 1
				
				// process output parameters of reference type and container type
				for (paramIdx <- paramStartIndex to (paramStartIndex + tgtMethodParamCount - 1))
				{
					// output parameter
					if (WALAUtils.hasMethodParamReferenceType(ownerClassName, tgtMethodName, paramIdx))
					{
						val formalParamName : String = Constants.MTHPARAM_PREFIX + String.valueOf(tgtMethodParamCount + 1 - paramIdx)
						
						val actualParamValue = actualParamArray(paramIdx - 1)
							
						var actualParamPredSet = Configuration.predicatesMngr.getPredicatesOverExpr(ctx.getCurClassOrigName(), ctx.getCurMethodName(), actualParamValue.toString(), -1)
						
						var outputPredSet = Set[AtomicPredicate]()
						
						// replace actual parameter name with mark "[output]"
						for (apPred <- actualParamPredSet)
						{
							val apPredWithMark = FormulaUtils.copyWithReplace(apPred, actualParamValue.toString(), Constants.OUTPUT_PARAM_MARK)
							
							if ( ! ( Configuration.predicatesMngr.isPredicateOverMethodVariables(apPredWithMark) || Configuration.predicatesMngr.isPredicateOverObjectFields(apPredWithMark) ) )
							{								
								outputPredSet = outputPredSet + apPredWithMark
							}
						}	
		
						ctx.saveOutputParamPredicates(ownerClassName, tgtMethodName, paramIdx, ctx.getOutputParamPredicates(ownerClassName, tgtMethodName, paramIdx) ++ outputPredSet)
					}
				}
			}
			
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
		}
	}
}

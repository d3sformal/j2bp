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
import common.ArithmeticExpression
import common.AtomicPredicate
import common.BinaryPredicate
import common.UnaryPredicate
import common.ExpressionUtils

import util.StringUtils


object ExecutionSimulator
{
	def simulateMethod(mth : IBytecodeMethod, isAbstract : Boolean, ctx : AbstractionContext, execVisitor : ExecutionVisitor, arg : java.lang.Object) =
	{
		var mthInstructions : Array[IInstruction] = mth.getInstructions()
		
		if (isAbstract) mthInstructions = new Array[IInstruction](0)

		var insnIndex = 0

		// loop through all Shrike bytecode instructions and process each relevant one (some are ignored)
		while (insnIndex < mthInstructions.length)
		{
			execVisitor.visitPreInstruction(ctx, insnIndex, arg)
			
			val insn = mthInstructions(insnIndex)
			
			val insnOpcode = insn.asInstanceOf[Instruction].getOpcode()
			
			var nextInsnIndex = insnIndex
			
			
			if (Main.DEBUG) println("[DEBUG ExecutionSimulator.simulateMethod] current instruction: " + insn.getClass().getName())

			if (Main.DEBUG) ctx.printExprStack("[DEBUG ExecutionSimulator.simulateMethod] stack before:")

			
			if (insn.isInstanceOf[ArrayLengthInstruction])
			{
				// remove "arrayExpr" from the top of the stack and put there "arrayExpr.length"
				val arrayExpr = ctx.removeExprFromStack()
				
				ctx.addExprToStack(ExpressionUtils.createFieldAccessExpr(arrayExpr, "length"))				
			}
			
			if (insn.isInstanceOf[ArrayLoadInstruction])
			{
				// remove "index" from the top of the stack
				val index = ctx.removeExprFromStack()
					
				// remove "arrayExpr" from the top of the stack
				val arrayExpr = ctx.removeExprFromStack()
				
				// add "arrayExpr[index]" to the stack
				ctx.addExprToStack(ExpressionUtils.createArrayAccessExpr(arrayExpr, index))
			}
			
			if (insn.isInstanceOf[ArrayStoreInstruction])
			{
				// remove value (or method call result) from the top of the stack
				val newValue = ctx.removeExprFromStack()
				
				// remove "index" from the top of the stack
				val index = ctx.removeExprFromStack()
					
				// remove "arrayExpr" from the top of the stack
				val arrayExpr = ctx.removeExprFromStack()
				
				// we use element type for arrays because an assignment statement can change the value of one element only
	
				var staticElementType = insn.asInstanceOf[ArrayStoreInstruction].getType()
				
				var actualElementType = JavaCodeUtils.findAssignmentTargetRefVarType(ctx, arrayExpr, staticElementType, newValue)
				
				execVisitor.visitArrayStoreInsn(ctx, insnIndex, arg, arrayExpr, index, newValue, actualElementType)
			}
				
			if (insn.isInstanceOf[BinaryOpInstruction])
			{
				// remove "value2" from the top of the stack
				val value2 = ctx.removeExprFromStack()
				
				// remove "value1" from the top of the stack
				val value1 = ctx.removeExprFromStack()
				
				// add string "(op value1 value2)" to the stack, where "op" is determined based on result of BinaryOpInstruction.getOperator()
				var opStr = ""				
				insn.asInstanceOf[BinaryOpInstruction].getOperator() match 
				{
					case IBinaryOpInstruction.Operator.ADD => opStr = "+"
					case IBinaryOpInstruction.Operator.DIV => opStr = "/"
					case IBinaryOpInstruction.Operator.MUL => opStr = "*"					
					case IBinaryOpInstruction.Operator.REM => opStr = "%"
					case IBinaryOpInstruction.Operator.SUB => opStr = "-"
				}				
				ctx.addExprToStack(new ArithmeticExpression(opStr, value1, value2))
			}
			
			if (insn.isInstanceOf[ConditionalBranchInstruction])
			{
				var value2 : Expression = null 
				if ((insnOpcode >= 153) && (insnOpcode <= 158)) value2 = Constants.ZERO_EXPR 
				else value2 = ctx.removeExprFromStack()
								
				// remove "value1" from the top of the stack
				val value1 = ctx.removeExprFromStack()

				// create conditional expression
				var posOpStr = ""
				var negOpStr = ""
				insn.asInstanceOf[ConditionalBranchInstruction].getOperator() match 
				{
					case IConditionalBranchInstruction.Operator.EQ => { posOpStr = "="; negOpStr = "!=" }
					case IConditionalBranchInstruction.Operator.GE => { posOpStr = ">="; negOpStr = "<" }
					case IConditionalBranchInstruction.Operator.GT => { posOpStr = ">"; negOpStr = "<=" }
					case IConditionalBranchInstruction.Operator.LE => { posOpStr = "<="; negOpStr = ">" }
					case IConditionalBranchInstruction.Operator.LT => { posOpStr = "<"; negOpStr = ">=" }
					case IConditionalBranchInstruction.Operator.NE => { posOpStr = "!="; negOpStr = "=" }
				}
				
				
				// try to find back jump to this instruction
				// we would have a loop in that case
				
				var backjumpInsnPos = -1
				
				var futureInsnIndex = insnIndex + 1
				
				while ((futureInsnIndex < mthInstructions.length) && (backjumpInsnPos == -1)) 
				{
					var futureInsn = mthInstructions(futureInsnIndex)
					
					if (futureInsn.isInstanceOf[GotoInstruction])
					{
						val gotoTargetIndex = futureInsn.asInstanceOf[GotoInstruction].getLabel()
						
						if (gotoTargetIndex < futureInsnIndex)
						{
							// we have back jump to this conditional branch (to the start of a loop body)
							
							if (gotoTargetIndex <= insnIndex) backjumpInsnPos = futureInsnIndex
						}
					}
					
					futureInsnIndex += 1
				}
				
				execVisitor.visitConditionalBranchInsn(ctx, insnIndex, arg, posOpStr, negOpStr, value1, value2, insn.asInstanceOf[ConditionalBranchInstruction].getTarget(), backjumpInsnPos)
			}
			
			if (insn.isInstanceOf[ConstantInstruction])
			{
				// add string representation of the constant to the stack
				if (insn.asInstanceOf[ConstantInstruction].getValue() == null) ctx.addExprToStack(Constants.NULL_EXPR)
				else ctx.addExprToStack(ExpressionUtils.createExprFromStr(insn.asInstanceOf[ConstantInstruction].getValue().toString()))
			}
			
			if (insn.isInstanceOf[DupInstruction])
			{
				var skip : Boolean = false
				
				// this takes care of the way javac compiles synchronized blocks
				if ((insnIndex + 2 < mthInstructions.length) && mthInstructions(insnIndex + 2).isInstanceOf[MonitorInstruction]) skip = true
				
				if ( ! skip )
				{
					// get "value" from the top of the stack
					val dupValue = ctx.getExprFromStack()
					
					if (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_dup_x1)
					{
						// make duplicate at proper depth
						ctx.insertExprToStack(dupValue, 2)
					}
					else
					{				
						// make duplicate
						ctx.addExprToStack(dupValue)
					}
				}
			}
			
			if (insn.isInstanceOf[GetInstruction])
			{
				val fieldName = insn.asInstanceOf[GetInstruction].getFieldName()
				
				var tgtObj : Expression = null
				
				if (insn.asInstanceOf[GetInstruction].isStatic)
				{
					tgtObj = new Expression(StringUtils.getPlainClassNameAsIndivisibleString(insn.asInstanceOf[GetInstruction].getClassType()))
				}
				else
				{
					// remove "tgtObj" (name of the local variable (including 'this'), or field access expression) from the stack
					tgtObj = ctx.removeExprFromStack()				
				}
				
				// put "tgtObj.fieldname" to the stack, where fieldname is result of GetInstruction.getFieldName()
				ctx.addExprToStack(ExpressionUtils.createFieldAccessExpr(tgtObj, fieldName))

				execVisitor.visitGetInsn(ctx, insnIndex, arg, tgtObj, fieldName)
			}
			
			if (insn.isInstanceOf[GotoInstruction])
			{
				val prevInsn = mthInstructions(insnIndex - 1)
				val prevInsnOpcode = prevInsn.asInstanceOf[Instruction].getOpcode()
				
				if (prevInsnOpcode == com.ibm.wala.shrikeBT.Constants.OP_monitorexit)
				{
					// skip monitor exit for exceptions thrown inside the synchronized block
					nextInsnIndex = insn.asInstanceOf[GotoInstruction].getLabel() - 1
				}
				else
				{
					val targetIndex = insn.asInstanceOf[GotoInstruction].getLabel()
					
					execVisitor.visitGotoInsn(ctx, insnIndex, arg, targetIndex)
				}
			}
			
			if (insn.isInstanceOf[InvokeInstruction])
			{
				val ownerClassName = StringUtils.getPlainClassName(insn.asInstanceOf[InvokeInstruction].getClassType())
				val tgtMethodName = insn.asInstanceOf[InvokeInstruction].getMethodName()
		
				val tgtMethodSig = insn.asInstanceOf[InvokeInstruction].getMethodSignature()

				val isStaticCall = insn.asInstanceOf[InvokeInstruction].getInvocationCode() == IInvokeInstruction.Dispatch.STATIC
				
				var tgtMethodParamCount = 0
				if (isStaticCall) tgtMethodParamCount = insn.asInstanceOf[InvokeInstruction].getPoppedCount()
				else tgtMethodParamCount = insn.asInstanceOf[InvokeInstruction].getPoppedCount() - 1 
				
				// variable used to store the return value
				var retVarExpr : Expression = null
				var retVarType : String = ""
				
				if (WALAUtils.hasMethodReturnValue(ownerClassName, tgtMethodName))
				{
					// determine name of the variable that will store the return value 
					if (insnIndex + 1 < mthInstructions.length)
					{
						var nextInsn = mthInstructions(insnIndex + 1)

						if (nextInsn.isInstanceOf[CheckCastInstruction]) 
						{
							// this works for return variables of object types in which we are mostly interested
							val castTypes = nextInsn.asInstanceOf[CheckCastInstruction].getTypes()
							retVarType = castTypes(0)

							// skip unboxing
							nextInsn = mthInstructions(insnIndex + 2)								
							if (nextInsn.isInstanceOf[InvokeInstruction]) nextInsn = mthInstructions(insnIndex + 3)								
						}
						
						if (nextInsn.isInstanceOf[StoreInstruction]) 
						{
							retVarExpr = new Expression(ctx.getLocalVariableName(nextInsn.asInstanceOf[StoreInstruction].getVarIndex()))
						}
						else if (nextInsn.isInstanceOf[PutInstruction])
						{
							if (nextInsn.asInstanceOf[PutInstruction].isStatic) 
							{
								retVarExpr = ExpressionUtils.createFieldAccessExpr(new Expression(StringUtils.getPlainClassNameAsIndivisibleString(nextInsn.asInstanceOf[PutInstruction].getClassType())), nextInsn.asInstanceOf[PutInstruction].getFieldName())
							}
							else
							{
								retVarExpr = ExpressionUtils.createFieldAccessExpr(ctx.getExprFromStackAtDepth(0), nextInsn.asInstanceOf[PutInstruction].getFieldName())
							}
						}
						else if (nextInsn.isInstanceOf[ArrayStoreInstruction])
						{
							retVarExpr = ExpressionUtils.createArrayAccessExpr(ctx.getExprFromStackAtDepth(1), ctx.getExprFromStackAtDepth(0))
						}
					}

					if (retVarExpr == null) retVarExpr = new Expression(ctx.getFreshTempReturnVariableName())
					else ctx.clearTempReturnVariableName()						
				}
				else
				{
					ctx.clearTempReturnVariableName()
				}
				
				if (Main.DEBUG) println("[DEBUG ExecutionSimulator.simulateMethod(Invoke)] class name = '" + ownerClassName + "', method name = '" + tgtMethodName + "', static = " + isStaticCall + ", orig param count = " + tgtMethodParamCount + ", orig return value = " + WALAUtils.hasMethodReturnValue(ownerClassName, tgtMethodName) + ", return var name = " + retVarExpr + ", return var type = " + retVarType)
				
				execVisitor.visitInvokeInsn(ctx, insnIndex, arg, ownerClassName, tgtMethodName, tgtMethodSig, isStaticCall, tgtMethodParamCount, retVarExpr, retVarType)
			}
           
			if (insn.isInstanceOf[LoadInstruction])
			{
				var skip : Boolean = false
				
				// this takes care of the way javac compiles synchronized blocks
				if ((insnIndex + 1 < mthInstructions.length) && mthInstructions(insnIndex + 1).isInstanceOf[MonitorInstruction]) skip = true
				
				if ( ! skip )
				{
					val varIndex = insn.asInstanceOf[LoadInstruction].getVarIndex()
					
					// determine the local variable name
					var varName = ctx.getLocalVariableName(varIndex)
					
					// add variable name to the stack
					ctx.addExprToStack(ExpressionUtils.createExprFromStr(varName))
				}
			}
			
			if (insn.isInstanceOf[MonitorInstruction])
			{
				if (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_monitorenter)
				{
					if ( ! ctx.visitedMonitorEnter() ) ctx.incMonitorVarCount()
					ctx.setVisitedMonitorEnterFlag(true)
				}

				if (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_monitorexit)
				{
					ctx.incMonitorVarCount()
				}

				execVisitor.visitMonitorInsn(ctx, insnIndex, arg, insnOpcode)
			}
			
			if (insn.isInstanceOf[NewInstruction])
			{
				val resultVarType = insn.asInstanceOf[NewInstruction].getType()
				
				var createdNewArray = false
				var newArraySizeExpr : Expression = null
				
				if ((resultVarType.charAt(0) == '[') || (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_anewarray) || (insnOpcode == com.ibm.wala.shrikeBT.Constants.OP_newarray)) 
				{
					// remove size of the array from the stack
					newArraySizeExpr = ctx.removeExprFromStack()
					
					createdNewArray = true
				}
				
				// add string "[newobj]" (representing newly allocated object) to the stack
				ctx.addExprToStack(new Expression(Constants.STACK_ELEM_NEWOBJ))
				
				// variable used to store the result (new object)
				var resultVarExpr : Expression = null
				
				// determine name of the variable that will store the result (new object) 
				if (insnIndex + 1 < mthInstructions.length)
				{
					var nextInsn = mthInstructions(insnIndex + 1)
					
					// skip invocation of the object constructor
					if (nextInsn.isInstanceOf[DupInstruction]) 
					{
						var initOffset = 2
						
						// we must also skip all parameters to the object constructor
						while ( ! mthInstructions(insnIndex + initOffset).isInstanceOf[InvokeInstruction] ) initOffset += 1
						
						nextInsn = mthInstructions(insnIndex + initOffset + 1)								
					}

					if (nextInsn.isInstanceOf[StoreInstruction]) 
					{
						resultVarExpr = new Expression(ctx.getLocalVariableName(nextInsn.asInstanceOf[StoreInstruction].getVarIndex()))
					}
					else if (nextInsn.isInstanceOf[PutInstruction])
					{
						if (nextInsn.asInstanceOf[PutInstruction].isStatic) 
						{
							resultVarExpr = ExpressionUtils.createFieldAccessExpr(new Expression(StringUtils.getPlainClassNameAsIndivisibleString(nextInsn.asInstanceOf[PutInstruction].getClassType())), nextInsn.asInstanceOf[PutInstruction].getFieldName())
						}
						else
						{
							resultVarExpr = ExpressionUtils.createFieldAccessExpr(ctx.getExprFromStackAtDepth(0), nextInsn.asInstanceOf[PutInstruction].getFieldName())
						}
					}
					else if (nextInsn.isInstanceOf[ArrayStoreInstruction])
					{
						// take only the array name
						resultVarExpr = ctx.getExprFromStackAtDepth(1)
					}
				}

				if (resultVarExpr == null) 
				{
					resultVarExpr = new Expression(ctx.getFreshTempReturnVariableName())
				
					// remove the string "[newobj]" from the top of the stack
					val newObjExpr = ctx.removeExprFromStack()
				
					ctx.setLocalRefVarType(resultVarExpr, resultVarType)
					
					ctx.setLastNewType("")
					
					// we abuse the visitStore method here a little bit
					execVisitor.visitStoreInsn(ctx, insnIndex, arg, resultVarExpr, resultVarType, newObjExpr)
					
					// add temporary implicit variable used to store the new object on stack for further processing
					ctx.addExprToStack(resultVarExpr)
				}
				else
				{
					ctx.clearTempReturnVariableName()
					
					ctx.setLastNewType(resultVarType)
				}
				
				if (createdNewArray)
				{
					// set array length
					execVisitor.visitPutInsn(ctx, insnIndex, arg, resultVarExpr, "length", "int", newArraySizeExpr)
				}

				execVisitor.visitNewInsn(ctx, insnIndex, arg, resultVarExpr, resultVarType)				
			}
			
			if (insn.isInstanceOf[PopInstruction])
			{
				// remove value from the top of the stack
				ctx.removeExprFromStack()
			}
			
			if (insn.isInstanceOf[PutInstruction])
			{
				// remove value (or method call result) from the top of the stack
				val newValue = ctx.removeExprFromStack()
				
				// determine target object (or class for static fields)
				var tgtObj : Expression = null
				if (insn.asInstanceOf[PutInstruction].isStatic)
				{
					tgtObj = new Expression(StringUtils.getPlainClassNameAsIndivisibleString(insn.asInstanceOf[PutInstruction].getClassType()))
				}
				else
				{
					// remove "tgtObj" (name of the local variable (including 'this') or field access expression) from the stack
					tgtObj = ctx.removeExprFromStack()
				}
				
				val fieldName = insn.asInstanceOf[PutInstruction].getFieldName()
							
				var fieldType = insn.asInstanceOf[PutInstruction].getFieldType()

				if (fieldType.length() > 1)
				{
					ctx.setLocalRefVarType(ExpressionUtils.createFieldAccessExpr(tgtObj, fieldName), fieldType)
				}

				execVisitor.visitPutInsn(ctx, insnIndex, arg, tgtObj, fieldName, fieldType, newValue)
			}
			
			if (insn.isInstanceOf[ReturnInstruction])
			{
				var retExpr : Expression = null
				
				// check whether original method returns something
				if ( ctx.hasCurMethodOrigReturnValue() )
				{					
					// remove "returned expression" from the top of the stack
					retExpr = ctx.removeExprFromStack()
				}

				execVisitor.visitReturnInsn(ctx, insnIndex, arg, retExpr)
			}
			
			if (insn.isInstanceOf[StoreInstruction])
			{
				var skip : Boolean = false
				
				// this takes case of the way javac compiles synchronized blocks
				if ((insnIndex + 1 < mthInstructions.length) && mthInstructions(insnIndex + 1).isInstanceOf[MonitorInstruction]) skip = true
				
				if ( ! skip )
				{
					// determine the local variable name 
					val tgtVarIndex = insn.asInstanceOf[StoreInstruction].getVarIndex()
					
					val tgtVarName = ctx.getLocalVariableName(tgtVarIndex)
					
					val tgtVarExpr = ExpressionUtils.createExprFromStr(tgtVarName)
					
					var staticTgtVarType = insn.asInstanceOf[StoreInstruction].getType()

					// remove value (or method call result) from the top of the stack
					val newValue = ctx.removeExprFromStack()

					var actualTgtVarType = JavaCodeUtils.findAssignmentTargetRefVarType(ctx, tgtVarExpr, staticTgtVarType, newValue)

					execVisitor.visitStoreInsn(ctx, insnIndex, arg, tgtVarExpr, actualTgtVarType, newValue)
				}
			}
			
			if (insn.isInstanceOf[SwapInstruction])
			{
				// remove "value1" from the top of the stack
				val value1 = ctx.removeExprFromStack()
				
				// remove "value2" from the top of the stack
				val value2 = ctx.removeExprFromStack()
				
				// put values back in swapped order
				ctx.addExprToStack(value1)
				ctx.addExprToStack(value2)
			}
			
			if (insn.isInstanceOf[SwitchInstruction])
			{
				// remove "tested value" from the top of the stack
				val testedValue = ctx.removeExprFromStack()
		
				var caseValues = List[Int]()
				var caseTargets = List[Int]()
				
				for (i <- 0 to ((insn.asInstanceOf[SwitchInstruction].getCasesAndLabels().length - 1) / 2))
				{
					caseValues = caseValues :+ insn.asInstanceOf[SwitchInstruction].getCasesAndLabels()(i*2)
					caseTargets = caseTargets :+ insn.asInstanceOf[SwitchInstruction].getCasesAndLabels()(i*2+1)
				}
				
				execVisitor.visitSwitchInsn(ctx, insnIndex, arg, testedValue, caseValues, caseTargets, insn.asInstanceOf[SwitchInstruction].getDefaultLabel())
			}
			
			if (insn.isInstanceOf[ThrowInstruction])
			{
				// remove the exception object from the top of the stack
				val excObj = ctx.removeExprFromStack()
				
				execVisitor.visitThrowInsn(ctx, insnIndex, arg, excObj)
			}
			
			if (insn.isInstanceOf[UnaryOpInstruction])
			{
				// remove "value" from the top of the stack
				val valueExpr = ctx.removeExprFromStack()

				// add string "(- value)" to the stack 
				ctx.addExprToStack(new ArithmeticExpression("-", Constants.ZERO_EXPR, valueExpr))
			}
			
			// instructions currently not processed (ignored): Conversion, CheckCast, Comparison (for these types: long,float,double). Instanceof, Shift

			execVisitor.visitPostInstruction(ctx, insnIndex, arg)
			
			if (Main.DEBUG) ctx.printExprStack("[DEBUG ExecutionSimulator.simulateMethod] stack after:")			
			
			nextInsnIndex = nextInsnIndex + 1
			
			insnIndex = nextInsnIndex			
		}
	}	
}

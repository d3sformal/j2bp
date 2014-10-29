package j2bp

import java.io.File
import java.io.FileOutputStream

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.JavaConversions._
import scala.io.Source._

import com.ibm.wala.classLoader.{IClass,IMethod,IBytecodeMethod}
import com.ibm.wala.types.TypeReference
import com.ibm.wala.shrikeBT._ 

import common.Constants
import common.Expression


object JavaCodeUtils
{
	def loadOrigMethodInfo(ctx : AbstractionContext, className : String, mth : IBytecodeMethod) =
	{
		// store original numbers of method parameters

		val mthShortName = WALAUtils.getShortMethodName(mth)
		
		if (Main.DEBUG) println("[DEBUG JavaCodeAbstractor.loadMethodInfo] method name = " + className + "." + mthShortName + ", orig param count = " + mth.getNumberOfParameters() + ", return value = " + (mth.getReturnType() != TypeReference.Void))
				
		val paramCount = 
			if (mth.isStatic()) mth.getNumberOfParameters();
			else mth.getNumberOfParameters() - 1; // subtract first parameter representing "this" for instance methods
	
		ctx.storeMethodOrigParamCount(className, mthShortName, paramCount)
		ctx.storeMethodOrigResultType(className, mthShortName, mth.getReturnType() != TypeReference.Void)
	}
	
	def findAllJumpTargets(mth : IBytecodeMethod, ctx : AbstractionContext) =
	{
		var insnIndex : Int = 0
		
		var mthInstructions : Array[IInstruction] = mth.getInstructions()
		
		if (mth.isAbstract()) mthInstructions = new Array[IInstruction](0)
				
		// loop through all Shrike bytecode instructions and process each that contains some jump targets
		for (insn : IInstruction <- mthInstructions)
		{
			if (insn.isInstanceOf[ConditionalBranchInstruction])
			{
				ctx.addLabelForInsn(insn.asInstanceOf[ConditionalBranchInstruction].getTarget())
			}
			
			if (insn.isInstanceOf[GotoInstruction])
			{
				ctx.addLabelForInsn(insn.asInstanceOf[GotoInstruction].getLabel())
			}
			
			if (insn.isInstanceOf[SwitchInstruction])
			{
				val targets : Array[Int] = insn.asInstanceOf[SwitchInstruction].getBranchTargets()
				
				for (tgt <- targets) ctx.addLabelForInsn(tgt)
				
				ctx.addLabelForInsn(insn.asInstanceOf[SwitchInstruction].getDefaultLabel())
			}			
		}
	}
	
	def getLocalVarName(localVarIdx : Int, startIdx : Int, mthParamCount : Int, monitorVarCount : Int) : String =
	{
		var localVarName = ""
						
		if ((localVarIdx >= startIdx) && (localVarIdx < startIdx + mthParamCount)) 
		{
			localVarName = Constants.MTHPARAM_PREFIX + localVarIdx
		}
		else if (localVarIdx >= startIdx + mthParamCount) 
		{
			if (localVarIdx - monitorVarCount >= startIdx + mthParamCount) localVarName = Constants.LOCALVAR_PREFIX + (localVarIdx - mthParamCount - monitorVarCount)
			else localVarName = Constants.LOCALVAR_PREFIX + (localVarIdx - mthParamCount)
		}
		else localVarName = "this"
	
		return localVarName
	}
	
	def findAssignmentTargetRefVarType(ctx : AbstractionContext, tgtVarExpr : Expression, staticType : String, srcValueExpr : Expression) : String =
	{
		if ( ! staticType.contains("java/lang/Object") ) return staticType
		
		var actualType = staticType
		
		val knownTgtVarType = ctx.getLocalRefVarType(tgtVarExpr)
		val lastNewType = ctx.getLastNewType()
		val knownSrcValueType = ctx.getLocalRefVarType(srcValueExpr)
			
		if (knownTgtVarType != "")
		{
			actualType = knownTgtVarType
		}
		else if (knownSrcValueType != "")
		{
			actualType = knownSrcValueType
		}
		else if (lastNewType != "")
		{
			actualType = lastNewType

			ctx.setLastNewType("")
		}
		
		ctx.setLocalRefVarType(tgtVarExpr, actualType)
		
		return actualType
	}
}


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
import j2bp.AbstractionContext
import j2bp.WALAUtils
import j2bp.Configuration
import j2bp.ExecutionSimulator


object MethodParamFinder extends j2bp.ExecutionVisitor
{
	def identifyParameterVariables(progClasses : List[String]) : Map[String, Set[String]] =
	{
		// map from method name to a set of local variables used as parameters for some calls
		var mth2paramvars : Map[String, Set[String]] = new HashMap

		val ctx = Main.initNewContext(progClasses)
		
		for (className <- progClasses)
		{
			val cls = WALAUtils.findClass(className)
			
			ctx.initCurClass(className)
				
			for (mth <- cls.getDeclaredMethods()) 
			{
				ctx.initCurMethod(WALAUtils.getShortMethodName(mth), mth.isStatic())

				// process the current method
				
				var dataHolder : Map[String,java.lang.Object] = new HashMap
			
				dataHolder.put("paramvars", mth2paramvars)
				
				ExecutionSimulator.simulateMethod(mth.asInstanceOf[IBytecodeMethod], cls.isInterface() || mth.isAbstract(), ctx, this, dataHolder)
				
				mth2paramvars = dataHolder.get("paramvars").get.asInstanceOf[Map[String, Set[String]]]	
			}
		}			
		
		return mth2paramvars
	}
	
	override def visitInvokeInsn(ctx : AbstractionContext, insnIndex : Int, arg : java.lang.Object, ownerClassName : String, tgtMethodName : String, tgtMethodSig : String, isStaticCall : Boolean, tgtMethodParamCount : Int, retVarExpr : Expression, retVarType : String) =
	{
		val dataHolder = arg.asInstanceOf[Map[String,java.lang.Object]]
		
		val mth2paramvars = dataHolder.get("paramvars").get.asInstanceOf[Map[String, Set[String]]]
				
		
		val piRes = Configuration.mthcallAbstr.preprocessInvoke(ctx, ownerClassName, tgtMethodName, tgtMethodParamCount, isStaticCall)
		val actualParamArray : Array[Expression] = piRes._2
		val skip = piRes._3
			
		if ( ! skip )
		{
			Configuration.mthcallAbstr.postprocessInvoke(ctx, ownerClassName, tgtMethodName)
			
			
			// collect actual parameters of this method call that are local variables in the current method
			
			var paramVarSet = mth2paramvars.getOrElse(ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), Set[String]())
			
			for (paramIdx <- 0 to (actualParamArray.length - 1))
			{
				val actualParamValue = actualParamArray(paramIdx)
				
				if (ExpressionUtils.isMethodVariable(actualParamValue))
				{
					paramVarSet = paramVarSet + actualParamValue.toString()
				}
			}
				
			mth2paramvars.put(ctx.getCurClassOrigName() + "." + ctx.getCurMethodName(), paramVarSet)
		}
			
		dataHolder.put("paramvars", mth2paramvars)
	}	

}

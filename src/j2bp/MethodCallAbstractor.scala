package j2bp

import org.objectweb.asm.MethodVisitor

import common.Expression


abstract class MethodCallAbstractor
{
	def generateInternalCall(ctx : AbstractionContext, mv : MethodVisitor, className : String, methodName : String, isStaticCall : Boolean, dynTargetClasses : List[String], bcIndex : Int)
	
	def generateLibraryCall(ctx : AbstractionContext, mv : MethodVisitor, className : String, methodName : String, isStaticCall : Boolean, paramCount : Int, retVarExpr : Expression, retVarType : String, bcIndex : Int) : Unit
	
	def isIgnoredLibraryMethod(className : String, methodName : String) : Boolean
	
	def preprocessInvoke(ctx : AbstractionContext, ownerClassName : String, tgtMethodName : String, tgtMethodParamCount : Int, isStaticCall : Boolean) : (Expression, Array[Expression], Boolean)
	
	def postprocessInvoke(ctx : AbstractionContext, ownerClassName : String, tgtMethodName : String)
}
